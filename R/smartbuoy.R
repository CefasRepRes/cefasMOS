#' SmartBuoy data query tool
#'
#' Fetches SmartBuoy data
#'
#' @details This function querys the Smartbuoy database and returns 
#' @param deployment 
#' @param deployment_group
#' @param after optional date string, if provided only data after this date will be returned, assumes UTC e.g. "2014-08-10"
#' @param before optional date string, if provided only data before this date will be returned, assumes UTC e.g. "2014-12-09"
#' @param parameters vector of parameter code names, defaults to c('TEMP', 'SAL', 'FTU', 'O2CONC', 'PAR')
#' @param min_QA_reached boolean, if True only data which has passed required QA level is returned, always used with QA0 = True.
#' @param QA0 boolean, if True only data where result quality = 0 is returned, i.e. good data. default is True
#' @param FSI_temp_only boolean, if True all non-FSI temperature data is discarded, default is False.
#' @param db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return data.table with returned data in "long" format or error string if no data returned
#' @keywords smartbuoy esm2 query
fetch.smartbuoy <- function(deployment = NA, deployment_group = NA,
                           after = NA, before = NA,
                           parameters = c('TEMP', 'SAL', 'FTU', 'O2CONC', 'PAR'),
                           min_QA_reached = TRUE,
                           RQ0 = TRUE, FSI_temp_only = FALSE,
                           db_name = 'smartbuoydblive'){
    require(RODBC)
    require(data.table)
    if(min_QA_reached == TRUE){
        # boilerplate query start
        query = paste("SELECT (CAST([Date/Time] AS NVARCHAR)) as dateTime,",
                  "[Deployment Id] as deployment,",
                  "[Deployment Group Id] as deployment_group,",
                  "[Depth Of Sensor] as depth,",
                  "[Result - mean] as value,",
                  "[Result - Std Dev] as stdev,",
                  "[Result - Count] as n,",
                  "[Sensor Description] as sensor,",
                  "[Parameter code] as par,",
                  "[Parameter Unit] as unit",
                  "FROM v_BurstMean_QaData")
    }else{
        query = paste("SELECT (CAST([Date/Time] AS NVARCHAR)) as dateTime,",
                  "[Deployment Id] as deployment,",
                  "[Deployment Group Id] as deployment_group,",
                  "[Depth Of Sensor] as depth,",
                  "[QA Level] as QA_level,",
                  "[Result Quality Flag] as QA_flag,",
                  "[Result - mean] as value,",
                  "[Result - Std Dev] as stdev,",
                  "[Result - Count] as n,",
                  "[Sensor Description] as sensor,",
                  "[Sensor Serial Number] as sensor_serial,",
                  "[Parameter Code] as par,",
                  "[Parameter Unit] as unit",
                  "FROM AdHocRetrieval_BurstMeanResults")
    }
    
        # collapse down parameters vector and wrap with quotes to work with IN (xxx)
    parameters = paste(parameters, collapse = "', '")
    query = paste0(query, " WHERE [Parameter code] IN ('", parameters, "')")

    # filter deployments
    if(!is.na(deployment)){
        deployment = paste(deployment, collapse = "', '")
        query = paste0(query, " AND [Deployment Id] IN ('", deployment, "')")
    }
    if(!is.na(deployment_group)){
        deployment_group = paste(deployment_group, collapse = "', '")
        query = paste0(query, " AND [Deployment Group Id] IN ('", deployment_group, "')")
    }
      # if only RQ0 data is required build filter into query
    if(RQ0 == TRUE & min_QA_reached != TRUE){
        query = paste(query, "AND [Result Quality] = 0")
    }
    if(FSI_temp_only == TRUE){
      stop('not yet implemented')
    }
      # if before or after is suppled build filter into query
    if(!is.na(before)){
        stop('not yet implemented')
    }
    if(!is.na(after)){
        stop('not yet implemented')
    }
    # finaly
    query = paste(query, 'ORDER BY dateTime')
    
    sb = odbcConnect(db_name)
    dat = data.table(sqlQuery(sb, query))
    odbcCloseAll()
    
    print(query)

    # check if valid data has been returned, if not quit
    if(nrow(dat) > 1){
        dat$dateTime= as.POSIXct(dat$dateTime, format="%b %d %Y %I:%M%p", tz="UTC") 
        return(dat)
    }else{
        stop("no data returned")
    }
}
