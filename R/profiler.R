#' ESM2 profiler data query tool
#'
#' Fetches CTD data from ESM2 profiler records
#'
#' @details This function querys the Smartbuoy database and returns ESM2 profiler data matching the provided critiera.
#' the v_CtdProfile_AllData table is used, as such private data will not be available to this function.
#' @param cruiseID optional cruise ID string, e.g. "CEND_02_14", if provided only data from this cruise will be returned.
#' @param profiler optional profiler name string, e.g. "PR009", if provided only data from this profiler will be returned.
#' @param after optional date string, if provided only data after this date will be returned, assumes UTC e.g. "2014-08-10"
#' @param before optional date string, if provided only data before this date will be returned, assumes UTC e.g. "2014-12-09"
#' @param area optional area vector consisting of 4 elements, minimum Latitude, minimum Longitude, maximum Latitude, maximum Longitude e.g. c(52, -3.5, 53, -4)
#' @param parameters vector of parameter code names, defaults to c('TEMP', 'SAL', 'FTU', 'O2CONC', 'PAR')
#' @param RQ0 boolean, if True only data where result quality = 0 is returned, i.e. good data. default is True
#' @param ct_temp_only boolean, if True all non-FSI temperature data is discarded, default is False.
#' @param min_QA_reached boolean, if True only data which has passed required QA level is returned, always used with QA0 = True.
#' @param db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return data.frame with returned data in "long" format or error string if no data returned
#' @keywords profiler ctd esm2 query
#' @export
fetch.profiler <- function(cruiseID = NA, profiler = NA,
                           after = NA, before = NA,
                           area = NA,
                           parameters = c('TEMP', 'SAL', 'FTU', 'O2CONC', 'PAR'),
                           RQ0 = TRUE, ct_temp_only = TRUE,
                           min_QA_reached = TRUE,
                           db_name = 'smartbuoydblive'){
    require(RODBC)
    require(data.table)
        # boilerplate query start
    query = paste("SELECT (CAST([Date/Time] AS NVARCHAR)) as startTime,",
              "[Start Time Offset (secs)] as offset,",
              "[Depth] as depth,",
              "[QA Status] as QA_level,",
              "[Result Quality] as QA_flag,",
              "[Result Value] as value,",
              "[Latitude] as latitude, [Longitude] as longitude,",
              "[Sensor Description] as sensor,",
              "[Parameter code] as par,",
              "[Station] as station,",
              "[Cruise Id] as cruise,",
              "[Logger Id] as profiler",
              "FROM v_CtdProfile_AllData",
              "WHERE [Parameter code] IN")

        # collapse down parameters vector and wrap with quotes to work with IN (xxx)
    parameters = paste(parameters, collapse = "', '")
    query = paste0(query, " ('", parameters, "') ")
    
        # if cruise id is suppled build filter into query
    if(!is.na(cruiseID[1])){
        cruiseID = paste(cruiseID, collapse = "', '")
        query = paste0(query, "AND [Cruise Id] IN ('", cruiseID,"')")
    }
        # if profiler id is suppled build filter into query
    if(!is.na(profiler[1])){
        profiler = paste(profiler, collapse = "', '")
        query = paste0(query, "AND [Logger Id] IN ('", profiler,"')")
    }
      # if area is suppled build filter into query
      # area = c(minLat, minLon, maxLat, maxLon)
    if(!is.na(area)){
      # check if area contains 4 elements
      if(!length(area) == 4){
        stop('area does not have 4 elements')
      }else{
        # TODO between code
        stop('not yet implemented')
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
      query = paste(query, "AND [Result Quality] = 0 ")
    }
    
    if(min_QA_reached == TRUE){
        # TODO min QA level reached
        query = paste(query, "AND [QA Status] >= [QA Status Min Publish Level]")
    }
    
    # finally
    query = paste0(query, ' ORDER BY startTime')

    print(query)
    sb = odbcConnect(db_name)
    dat = data.table(sqlQuery(sb, query))
    odbcCloseAll()
    

    dat$startTime = as.POSIXct(dat$startTime, format="%b %d %Y %I:%M%p", tz="UTC") 
    dat$dateTime = dat$startTime + dat$offset
    # dat[,max_depth := max(depth), by = list(startTime, profiler)]
    
        # if only CT temp wanted remove non ct data
    if(ct_temp_only == TRUE){
        ctSensors = 'Aanderaa Conductivity Sensor|FSI CT Module|Seabird'
        dat = dat[!(!sensor %like% ctSensors & par == 'TEMP')]
    }

    # check if valid data has been returned, if not quit
    if(nrow(dat) > 1){
        print(paste(nrow(dat), 'rows returned'))
        return(dat)
    }else{
        stop("no data returned")
    }
}

#' ESM2 profiler cruise id queryer
#'
#' Fetches lists of cruise id where ESM2 profiler data exists.
#'
#' @details This function querys the Smartbuoy database and returns a list of valid cruise IDs matching the supplied critiera where ESM2 
#' profiler data is available. The v_CtdProfile_AllData table is used, as such private data will not be available to this function.
#' @param yr integer specifing a year to limit the search, default is 'ALL'
#' @param db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return character vector of Cruise Id's
#' @keywords profiler ctd esm2 query
#' @export
profiler.cruiselist <- function(yr = 'ALL', db_name = 'smartbuoydblive'){
    require(RODBC)
    query = "SELECT DISTINCT [CruiseId] FROM CtdHeader"
    if(yr != 'ALL'){
        query = paste(query, ' WHERE YEAR([StartDate]) = ', yr, sep = '')
    }
    sb = odbcConnect(db_name)
    cruiseList = sqlQuery(sb, query)
    odbcCloseAll()
    return(as.vector(cruiseList))
}


#' ESM2 profiler depth binning
#'
#' Binns profiler data into depth bins
#'
#' @details TODO
#' @param x data.frame matching output from fetch.profiler
#' @param bin_height numeric vector determining depth binning interval, default is 0.5
#' @param method function for binning, default is floor, ceil and round also work
#' @param use_cast character string matching matching cast required, options are 'UP'
#' @param return_bin numeric vector, only these bin depths will be returned
#' @return character vector of Cruise Id's
#' @keywords profiler ctd esm2 query
#' @export
profiler.binning <- function(x,
                             bin_height= 1,
                             method = floor,
                             use_cast = 'UP',
                             return_bin = 'all'){
    require(data.table)
    dat = data.table(x)     # just make sure
    if(use_cast == 'UP'){
        # subset to up cast only
        max_depth_offsets =  dat[depth == max_depth, list(max_depth_offset = max(offset)), by = startTime]
        dat = merge(dat, max_depth_offsets, by=  'startTime')
        dat = dat[offset >= max_depth_offset, !"max_depth_offset", with = F] # select all but max_depth_time
    }
    if(use_cast == 'DOWN'){
        # subset to up cast only
        max_depth_offsets =  dat[depth == max_depth, list(max_depth_offset = max(offset)), by = startTime]
        dat = merge(dat, max_depth_offsets, by=  'startTime')
        dat = dat[offset <= max_depth_offset, !"max_depth_offset", with = F] # select all but max_depth_time
    }
    dat = dat[,depth_bin := method(depth / bin_height) * bin_height]
    
    dat = dat[, list(bin_mean = mean(value), count = length(value)),
        by = list(startTime, latitude, longitude, cruise, station, profiler, depth_bin, par)]
    
    if(return_bin != 'all'){
        dat = dat[depth_bin == return_bin,]
    }
    return(dat)
}