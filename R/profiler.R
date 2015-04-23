#' ESM2 profiler data query tool
#'
#' Fetches CTD data from ESM2 profiler records
#'
#' @details TODO
#' @param cruiseID optional cruise ID string, e.g. "CEND_02_14", if provided only data from this cruise will be returned.
#' @param profiler optional profiler name string, e.g. "PR009", if provided only data from this profiler will be returned.
#' @param after optional date string, if provided only data after this date will be returned, assumes UTC e.g. "2014-08-10"
#' @param before optional date string, if provided only data before this date will be returned, assumes UTC e.g. "2014-12-09"
#' @param area optional area vector consisting of 4 elements, minimum Latitude, minimum Longitude, maximum Latitude, maximum Longitude e.g. c(52, -3.5, 53, -4)
#' @param parameters vector of parameter code names, defaults to c('TEMP', 'SAL', 'FTU', 'O2CONC', 'PAR')
#' @param QA0 boolean, if True only data where result quality = 0 is returned, i.e. good data. default is True
#' @param FSI_temp_only boolean, if True all non-FSI temperature data is discarded, default is False.
#' @param min_QA_reached boolean, if True only data which has passed required QA level is returned, always used with QA0 = True.
#' @param db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return data.frame with returned data in "long" format or error string if no data returned
#' @keywords profiler ctd esm2
#' @examples
#' @export
#' data <- fetch.profiler('CEND_01_14')
fetch.profiler <- function(cruiseID = NA, profiler = NA,
                           after = NA, before = NA,
                           area = NA,
                           parameters = c('TEMP', 'SAL', 'FTU', 'O2CONC', 'PAR'),
                           RQ0 = TRUE, FSI_temp_only = FALSE,
                           min_QA_reached = TRUE,
                           db_name = 'smartbuoydblive'){
    require(RODBC)
    require(data.table)
        # boilerplate query start
    query = c("SELECT (CAST([Date/Time] AS NVARCHAR)) as startTime,",
              "[Start Time Offset (secs)] as offset,",
              "[Depth] as depth,",
              "[QA Status] as QA_level,",
              "[Result Quality] as QA_flag,",
              "[Result Value] as value,",
              "[Sensor Description] as sensor,",
              "[Parameter code] as par,",
              "[Station] as station,",
              "[Cruise Id] as cruise,",
              "[Logger Id] as profiler",
              "FROM v_CtdProfile_AllData",
              "WHERE [Parameter code] IN")

        # collapse down parameters vector and wrap with quotes to work with IN (xxx)
    query = c(query, paste("('",paste(parameters, collapse = "', '"),"')", sep = ""))

        # if cruise id is suppled build filter into query
    if(!is.na(cruiseID)){
      query = c(query, paste("AND [Cruise Id] = '", cruiseID,"'", sep = ""))
    }
        # if profiler id is suppled build filter into query
    if(!is.na(profiler)){
      query = c(query, paste("AND [Logger Id] = '", profiler,"'", sep = ""))
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

      # if only RQ0 data is required build filter into query
    if(RQ0 == TRUE){
      query = c(query, "AND [Result Quality] = 0")
    }
    if(FSI_temp_only == T){
      stop('not yet implemented')
    }
    if(min_QA_reached == TRUE){
        # TODO min QA level reached
        query = c(query, "AND [QA Status] >= [QA Status Min Publish Level]")
    }

        # combine query elements into one query string
    queryString = paste(query, collapse = ' ')
        # fetch the data
        # format dates
        # if specified remove non-FSI temperature data
    
    sb = odbcConnect(db_name)
    dat = data.table(sqlQuery(sb, queryString))
    odbcCloseAll()
    
    print(queryString)
    dat$startTime= as.POSIXct(dat$startTime, format="%b %d %Y %I:%M%p", tz="UTC") 
    dat$dateTime = dat$startTime + dat$offset
    

    # check if valid data has been returned, if not quit
    if(nrow(dat) > 1){
        return(dat)
    }else{
        stop("no data returned")
    }
}


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