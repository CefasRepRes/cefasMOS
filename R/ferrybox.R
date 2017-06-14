#' Ferrybox query tool
#'
#' Fetches ferrybox data
#'
#' @details TODO
#' @param cruiseID optional cruise ID string, e.g. "CEND_02_14", if provided only data from this cruise will be returned.
#' @param after optional date string, if provided only data after this date will be returned, assumes UTC e.g. "2014-08-10"
#' @param before optional date string, if provided only data before this date will be returned, assumes UTC e.g. "2014-12-09"
#' @param area optional vector consisting of 4 elements, minimum Latitude, minimum Longitude, maximum Latitude, maximum Longitude e.g. c(52, -3.5, 53, -4)
#' @param parameters vector of parameter code names, defaults to c('TEMP', 'SAL', 'FTU', 'O2CONC')
#' @param min_QA_reached boolean, if True only data which has passed required QA level is returned, always used with QA0 = True.
#' @param RQ0 boolean, if True only data where result quality = 0 is returned, i.e. good data, default it TRUE
#' @param hull_temp_only boolean, if True only temperature data from the hull temperature probe will be returned
#' @param db_name character string matching ODBC data source name, defaults to 'ferrybox'
#' @return data.frame with returned data in "long" format or error string if no data returned
#' @keywords ferrybox query
#' @import data.table RODBC
#' @export
ferrybox.fetch <- function(cruiseID = NA,
                           after = NA, before = NA,
                           area = NA,
                           parameters = c('TEMP', 'SAL', 'FTU', 'O2CONC'),
                           min_QA_reached = TRUE,
                           most_recent = NA,
                           RQ0 = TRUE,
                           hull_temp_only = FALSE,
                           db_name = 'ferrybox'){
        # boilerplate query start
    query = paste("SELECT (CAST([Date/Time] AS NVARCHAR)) as dateTime,",
              "[Cruise Id] as cruise,",
              "[Latitude] as latitude,",
              "[Longitude] as longitude,",
              "[Course] as course,",
              "[Speed] as speed,",
              "[Heading] as heading,",
              "[Result - Mean] as value,",
              "[Result - Variance] as var,",
              "[Result - Count] as n,",
              "[Result - Error] as stdev_derived,",
              "[Result - Error Type] as derived_type,",
              "[Sensor Description] as sensor,",
              "[Parameter code] as par,",
              "[Parameter Unit] as unit",
              "FROM v_FerryBox_Data")

        # collapse down parameters vector and wrap with quotes to work with IN (xxx)
    parameters = paste(parameters, collapse = "', '")
    query = paste0(query, " WHERE [Parameter code] IN ('", parameters, "')")

    # filter cruise ID, is.na evaluates each element of vector, so only check first one is not NA
    if(!is.na(cruiseID[1])){
        cruises = paste(cruiseID, collapse = "', '")
        query = paste0(query, " AND [Cruise Id] IN ('", cruises, "')")
    }

        # if area is suppled build filter into query
        # area = c(minLat, minLon, maxLat, maxLon)
    if(!is.na(area)){
            # check if area contains 4 elements
        if(length(area) != 4){
            stop('area does not have 4 elements')
        }else{
            # TODO between code
            stop(' not implemented')
        }
    }

      # if before or after is suppled build filter into query
    if(!is.na(before)){
        query = paste0(query, " AND [Date/Time] <= '", before, "'")
    }
    if(!is.na(after)){
        query = paste0(query, " AND [Date/Time] >= '", after, "'")
    }

    if(!is.na(most_recent)){
        query = paste0(query, " AND [Date/Time] = ( SELECT MAX([Date/Time]) FROM ferrybox.dbo.v_FerryBox_Data)")
    }

        # if only RQ0 data is required build filter into query
    if(RQ0 == TRUE){
        query = paste0(query, " AND [Result - Quality] = 0")
    }
    if(min_QA_reached == TRUE){
        query = paste0(query, " AND [Result - Required QA Level Met] = 1")
    }

    # finally
    query = paste(query, 'ORDER BY dateTime')
    fb = odbcConnect(db_name)
    dat = sqlQuery(fb, query)
    print(query)
    odbcCloseAll()
    dat = data.table(dat)

    # check if valid data has been returned, if not quit
    if(! nrow(dat) > 1){
        stop("no data returned")
    }

    print(paste(nrow(dat), 'rows returned'))
    dat$dateTime = as.POSIXct(dat$dateTime, format="%b %d %Y %I:%M%p", tz="UTC")

    if(hull_temp_only == TRUE & 'TEMP' %in% parameters){
        print('removing non hull temperatures')
        dat = dat[!(!sensor %like% "ADAM PT100 PRT" & par == 'TEMP')]
    }

    return(dat)
}

#' ferrybox cruise id queryer
#'
#' Fetches lists of cruise id
#'
#' @details This function querys the Smartbuoy database and returns a list of valid cruise IDs matching the supplied critiera where ESM2
#' profiler data is available. The v_CtdProfile_AllData table is used, as such private data will not be available to this function.
#' @param yr integer specifing a year to limit the search, default is 'ALL'
#' @param db_name character string matching ODBC data source name, defaults to 'ferrybox'
#' @return character vector of Cruise Id's
#' @keywords ferrybox query
#' @export
ferrybox.cruiselist <- function(yr = 'ALL', db_name = 'ferrybox'){
    query = "SELECT DISTINCT [CruiseId] FROM ConfigHeader"
    if(yr != 'ALL'){
        query = paste(query, ' WHERE YEAR([StartTime]) = ', yr, sep = '')
    }
    sb = RODBC::odbcConnect(db_name)
    cruiseList = RODBC::sqlQuery(sb, query)
    RODBC::odbcCloseAll()
    return(as.vector(cruiseList))
}

#' ferrybox cruise tracks
#'
#' Fetches position of ferrybox
#'
#' @details TODO
#' @param cruiseID optional character string matching cruise ID
#' @param after optional date string, if provided only data after this date will be returned, assumes UTC e.g. "2014-08-10"
#' @param before optional date string, if provided only data before this date will be returned, assumes UTC e.g. "2014-12-09"
#' @param db_name character string matching ODBC data source name, defaults to 'ferrybox'
#' @return data.frame (data.table) containing the ferrybox cruise track
#' @import data.table RODBC
#' @keywords ferrybox query
#' @export
ferrybox.position <- function(cruiseID = NA,
                              after = NA, before = NA,
                              db_name = 'ferrybox'){
        query = paste(
            "SELECT CruiseId, (CAST(DataHeaderTime AS NVARCHAR)) as dateTime,",
            "Latitude as latitude, Longitude as longitude,",
            "Course, Heading, Speed",
            "FROM [Ferrybox].[dbo].[DataHeader]",
            "INNER JOIN [FerryBox].[dbo].[DataFile]",
            "ON [FerryBox].[dbo].[DataHeader].DataFileId = [FerryBox].[dbo].[DataFile].DataFileId",
            "INNER JOIN [FerryBox].[dbo].[ConfigHeader]",
            "ON [FerryBox].[dbo].[DataFile].ConfigHeaderId = [FerryBox].[dbo].[ConfigHeader].ConfigHeaderId",
            "WHERE PositionQualityOk = 1 AND SatellitesVisible > 0")

    if(!is.na(cruiseID[1])){
        query= paste0(query, " AND CruiseId IN ('", paste(cruiseID, collapse = "', '"), "')")
    }
      # if before or after is suppled build filter into query
    if(!is.na(before)){
        query = paste0(query, " AND DataHeaderTime <= '", before, "'")
    }
    if(!is.na(after)){
        query = paste0(query, " AND DataHeaderTime >= '", after, "'")
    }

    # finally
    query = paste(query, 'ORDER BY dateTime')

    sb = odbcConnect(db_name)
    dat = sqlQuery(sb, query)
    odbcCloseAll()
    dat$dateTime = as.POSIXct(dat$dateTime, format="%b %d %Y %I:%M%p", tz="UTC")
    return(data.table(dat))
}

#' Ferrybox error codes
#'
#' Flags are:
#' 0 = no flag, good data
#' 1 = Bad or missing data (general flag)
#' 2 = flow rate too high
#' 4 = flow rate too low
#' 8 = timeout
#' 16 = standby, fb loop not running
#' 32 = wrong channel
#' 64 = value during cleaning
#' 128 = value during standby
#' 256 = value during empty
#' 512 = value during general error
#' 1024 = undefined
#' 2048 = value is simulated
#'
#' @param x error code
#' @param collapse_vector if true single element vector will be returned with ";" seperators
#'
#' @return codes used in composite
#' @export
ferrybox.errorcode <- function(x, collapse_vector = T){
  # devtools::use_data(cds_table, internal = T)
  x = as.character(x)
  if(collapse_vector){
    out = unlist(lapply(cds_table[x], paste, collapse = "; "), use.names = F)
  }
  else{
    out = unlist(cds_table[x], use.names = F)
  }
  return(out)
}


#' calculate speed from GPS
#'
#' Make sure your data.frame or table is ordered by dateTime.
#'
#' @param dateTime in POSIXct
#' @param lat in decimal degrees
#' @param lon in decimal degrees
#' @param threshold maximum time interval between gps points (seconds)
#'
#' @return vector of speeds
#' @export
#'
ferrybox.speed <- function(dateTime, lat, lon, threshold = 65){
  dat = data.table(dateTime, lat, lon)
  if(dat != dat[order(dateTime)]){
    stop("ERROR - data.table is not ordered")
  }
  dat[, diff := c(NA, diff(as.numeric(dateTime)))]
  dat[, lats := data.table::shift(dat$lat, type = "lag")]
  dat[, lons := data.table::shift(dat$lon, type = "lag")]
  dat[, dist := geosphere::distHaversine(cbind(lons, lats), cbind(lon, lat))]
  dat[, speed := abs((dist / diff) / 0.51444)] # knots
  dat[diff > threshold | speed > 24, speed := NA]
  dat[lats == lat & lons == lon, speed := NA]
  return(dat$speed)
}


#' Calculate course from GPS
#'
#' @param dateTime in POSIXct
#' @param lat in decimal degrees
#' @param lon in decimal degrees
#' @param threshold maximum time interval between gps points (seconds)
#'
#' @return vector of courses
#' @export
#'
ferrybox.course <- function(dateTime, lat, lon, threshold = 65){
  dat = data.table(dateTime, lat, lon)
  if(dat != dat[order(dateTime)]){
    stop("ERROR - data.table is not ordered")
  }
  dat[, diff := c(NA, diff(as.numeric(dateTime)))] #ensure we're talking about seconds
  dat[, lats := data.table::shift(dat$lat, type = "lag")]
  dat[, lons := data.table::shift(dat$lon, type = "lag")]
  dat[, course := geosphere::bearing(cbind(lons, lats), cbind(lon, lat))]
  dat[diff >= threshold, course := NA]
  dat[lats == lat & lons == lon, course := NA]
  dat[course < 0, course := course + 360] # geosphere method goes 0 to 180/-180 rather than 360
  return(dat$course)
}
