#' Ferrybox query tool
#'
#' Fetches ferrybox data
#'
#' @details TODO
#' @param optional cruise ID string, e.g. "CEND_02_14", if provided only data from this cruise will be returned.
#' @param optional date string, if provided only data after this date will be returned, assumes UTC e.g. "2014-08-10"
#' @param optional date string, if provided only data before this date will be returned, assumes UTC e.g. "2014-12-09"
#' @param optional area vector consisting of 4 elements, minimum Latitude, minimum Longitude, maximum Latitude, maximum Longitude e.g. c(52, -3.5, 53, -4)
#' @param optional vector of parameter code names, defaults to c('TEMP', 'SAL', 'FTU', 'O2CONC')
#' @param optional boolean, if True only data where result quality = 0 is returned, i.e. good data
#' @param optional character string matching ODBC data source name, defaults to 'ferrybox'
#' @return data.frame with returned data in "long" format or error string if no data returned
#' @keywords ferrybox query
#' @examples d <- fetch.ferrybox('CEND_01_14')
fetch.ferrybox<- function(cruiseID = NA,
                           after = NA, before = NA,
                           area = NA,
                           parameters = c('TEMP', 'SAL', 'FTU', 'O2CONC'),
                           RQ0 = TRUE,
                           db.name = 'ferrybox'){
    # require(RODBC)
        # boilerplate query start
    query = c("SELECT (CAST([Date/Time] AS NVARCHAR)) as startTime,",
              "[Start Time Offset (secs)] as offset,",
              "[Depth] as depth,",
              "[Result Value] as value,",
              "[Parameter code] as par,",
              "[Station] as station,",
              "[Cruise Id] as cruise",
              "[Logger Id] as profiler",
              "FROM v_CtdProfile_AllData",
              "WHERE [Parameter code] IN")

        # collapse down parameters vector and wrap with quotes to work with IN (xxx)
    query = c(query, paste("('",paste(parameters, collapse = "', '"),"')", sep = ""))

        # if area is suppled build filter into query
        # area = c(minLat, minLon, maxLat, maxLon)
    if(!is.na(area)){
            # check if area contains 4 elements
        if(!length(area) == 4){
            stop('area does not have 4 elements')
        }else{
            # TODO between code
        }
        query = c(query, paste("AND [Logger Id] = '", profiler,"'", sep = ""))
    }

        # if only RQ0 data is required build filter into query
    if(RQ0 == TRUE){
        query = c(query, "AND [Result Quality = 0]")
    }

        # TODO min QA level reached

    query = c(query, "ORDER BY startTime")
        # combine query elements into one query string
    queryString = paste(query, collapse = ' ')
        # fetch the data
        # format dates
        # if specified remove non-FSI temperature data

    # check if valid data has been returned, if not quit
    if(nrow(dat) > 1){
        return(dat)
    }else{
        stop(c("no data returned", dat))
    }
}

ferrybox.devdata <- function(x){
    
}