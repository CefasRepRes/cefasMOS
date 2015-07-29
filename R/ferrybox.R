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
#' @param db_name character string matching ODBC data source name, defaults to 'ferrybox'
#' @return data.frame with returned data in "long" format or error string if no data returned
#' @keywords ferrybox query
#' @export
ferrybox.fetch <- function(cruiseID = NA,
                           after = NA, before = NA,
                           area = NA,
                           parameters = c('TEMP', 'SAL', 'FTU', 'O2CONC'),
                           min_QA_reached = TRUE,
                           RQ0 = TRUE,
                           db_name = 'ferrybox'){
    require(RODBC)
    require(data.table)
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
    return(dat)
}

#' Ferrybox device data reader
#'
#' reads ferrybox device data files
#'
#' @details TODO
#' @param devdata_folder character string indicating path to folder containing device data files
#' @param pivot optional boolian indicating if returned table should be recast into 'wide' format
#' @return data.frame
#' @keywords ferrybox
#' @export
ferrybox.devdata <- function(devdata_folder, pivot = F){
    require(data.table)
    require(reshape2)
    
    # sometimes \0 characters appear in data, to sanitise run sed from a bash terminal
    # for i in *; do sed -i "s/\x0//g" $i done

    dat = data.table()
    for(fn in list.files(devdata_folder)){
        f = paste(devdata_folder, fn, sep='/')
        fd = readLines(f)
        startLine = grep("DATASETS",fd)
        sensorLine = grep("Type", fd)
        sensor = unlist(strsplit(fd[sensorLine], '; '))[2]
        print(f)
        d = fread(f, skip = startLine, sep = '\t')
        param = colnames(d)[2]
        d = d[-1, c('$Timestamp', param, 'Quality', 'Longitude', 'Latitude'), with = F]
        setnames(d, c('$Timestamp', param, 'Quality'), c('datetime', 'value', 'quality'))
        d$param = paste(param, sensor, sep = '_')
        dat = rbind(dat, d)
    }
    dat$datetime = as.POSIXct(dat$datetime, format = '%Y.%m.%d %H:%M:%S', tz='UTC')
    
    export = dat
        # reclasiffy types
    export$value = as.numeric(export$value)
    export$Longitude = as.numeric(export$Longitude)
    export$Latitude = as.numeric(export$Latitude)

    if(pivot == T){
        export = dcast.data.table(export, datetime ~ param)
        pos = dat[,.(datetime, Latitude, Longitude)]
        pos = pos[,lapply(.SD, as.numeric), by = datetime]
        pos = pos[,lapply(.SD, median), by = datetime]
        export = merge(export, pos, by = 'datetime')
        return(export)
    }else{
        return(export)
    }
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
    require(RODBC)
    query = "SELECT DISTINCT [CruiseId] FROM ConfigHeader"
    if(yr != 'ALL'){
        query = paste(query, ' WHERE YEAR([StartTime]) = ', yr, sep = '')
    }
    sb = odbcConnect(db_name)
    cruiseList = sqlQuery(sb, query)
    odbcCloseAll()
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
#' @keywords ferrybox query
#' @export
ferrybox.position <- function(cruiseID = NA,
                              after = NA, before = NA,
                              db_name = 'ferrybox'){
    require(RODBC)
        query = paste(
            "SELECT CruiseId, DataHeaderTime as dateTime,",
            "Latitude as latitude, Longitude as longitude,",
            "Course, Heading, Speed",
            "FROM [Ferrybox].[dbo].[DataHeader]",
            "INNER JOIN [FerryBox].[dbo].[DataFile]",
            "ON [FerryBox].[dbo].[DataHeader].DataHeaderId = [FerryBox].[dbo].[DataFile].DataFileId",
            "INNER JOIN [FerryBox].[dbo].[ConfigHeader]",
            "ON [FerryBox].[dbo].[DataFile].ConfigHeaderId = [FerryBox].[dbo].[ConfigHeader].ConfigHeaderId",
            "WHERE PositionQualityOk = 1 AND SatellitesVisible > 0")
    
    if(!is.na(cruiseID[1])){
        query= paste0(query, " AND CruiseId IN ('", paste(cruiseID, collapse = "', '"), "')")
    }
      # if before or after is suppled build filter into query
    if(!is.na(before)){
        query = paste0(query, " AND dateTime <= '", before, "'")
    }
    if(!is.na(after)){
        query = paste0(query, " AND dateTime >= '", after, "'")
    }
    
    # finally
    query = paste(query, 'ORDER BY dateTime')
    
    sb = odbcConnect(db_name)
    dat = sqlQuery(sb, query)
    dat$dateTime = as.POSIXct(dat$dateTime, format = '%Y.%m.%d %H:%M:%S', tz='UTC')
    odbcCloseAll()
    return(data.table(dat))
}