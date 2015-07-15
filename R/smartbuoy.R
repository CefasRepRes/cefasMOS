#' SmartBuoy data query tool
#'
#' Fetches SmartBuoy data
#'
#' @details This function querys the Smartbuoy database and returns 
#' @param deployment optional string matching smartbuoy deployment
#' @param deployment_group optional string matching smartbuoy deployment_group
#' @param after optional date string, if provided only data after this date will be returned, assumes UTC e.g. "2014-08-10"
#' @param before optional date string, if provided only data before this date will be returned, assumes UTC e.g. "2014-12-09"
#' @param parameters vector of parameter code names, defaults to c('TEMP', 'SAL', 'FTU', 'O2CONC', 'FLUORS')
#' @param min_QA_reached boolean, if True only data which has passed required QA level is returned, always used with QA0 = True.
#' @param RQ0 boolean, if True only data where result quality = 0 is returned, i.e. good data. default is True
#' @param ct_temp_only boolean, if True all non-FSI temperature data is discarded, default is False.
#' @param averaging_period, integer matching length in hours of averaging interval, i.e. 24 for daily means
#' @param night_flu_only optional boolean, if True (default) only quenched flu data will be removed
#' @param db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return data.table with returned data in "long" format or error string if no data returned
#' @keywords smartbuoy esm2 query
#' @export
smartbuoy.fetch <- function(deployment = NA, deployment_group = NA,
                           after = NA, before = NA,
                           parameters = c('TEMP', 'SAL', 'FTU', 'O2CONC', 'FLUORS'),
                           min_QA_reached = TRUE,
                           RQ0 = TRUE, ct_temp_only = TRUE,
                           averaging_period = NA,
                           night_flu_only = FALSE,
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
                  "[Result - Error] as stdev_derived,",
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
                  "[Sensor Descr] as sensor,",
                  "[Sensor Serial Number] as sensor_serial,",
                  "[Parameter Code] as par",
                  "FROM AdHocRetrieval_BurstMeanResults")
    }
    
        # collapse down parameters vector and wrap with quotes to work with IN (xxx)
    parameters_fetch = paste(parameters, collapse = "', '")
    
        # make sure we fetch PAR if needed
    if(night_flu_only & "FLUORS" %in% parameters & !('PAR' %in% parameters)){
        print('also fetching PAR to filter FLUORS')
        parameters_fetch = paste(parameters_fetch, "', 'PAR")
    }
        
    query = paste0(query, " WHERE [Parameter code] IN ('", parameters_fetch, "')")

    # filter deployments, is.na evaluates each element of vector, so only check first one is not NA
    if(!is.na(deployment[1])){
        deployment = paste(deployment, collapse = "', '")
        query = paste0(query, " AND [Deployment Id] IN ('", deployment, "')")
    }
    if(!is.na(deployment_group[1])){
        deployment_group = paste(deployment_group, collapse = "', '")
        query = paste0(query, " AND [Deployment Group Id] IN ('", deployment_group, "')")
    }
      # if only RQ0 data is required build filter into query
    if(RQ0 == TRUE & min_QA_reached != TRUE){
        query = paste(query, "AND [Result Quality Flag] = 0")
    }
    
      # if before or after is suppled build filter into query
    if(!is.na(before)){
        query = paste0(query, " AND [Date/Time] <= '", before, "'")
    }
    if(!is.na(after)){
        query = paste0(query, " AND [Date/Time] >= '", after, "'")
    }
    
    # finaly
    query = paste(query, 'ORDER BY dateTime')
    
    print(query)
    sb = odbcConnect(db_name)
    dat = data.table(sqlQuery(sb, query))
    odbcCloseAll()
    dat$dateTime = as.POSIXct(dat$dateTime, format="%b %d %Y %I:%M%p", tz="UTC") 
    
    # check if valid data has been returned, if not quit
    if(! nrow(dat) > 1){
        stop("no data returned")
    }
    
    if(night_flu_only & "FLUORS" %in% parameters){
        print('using PAR to subset, FLUORS threshold = 1 uE m-2 s-1')
        PAR = dat[par == 'PAR', .(dateTime, PAR = value)]
        dat = merge(dat, PAR, by = 'dateTime', all = T, allow.cartesian = T) # allow.cartesian needed for duplicate timestamps
        dat = dat[is.na(PAR), PAR := -1]
        dat = dat[PAR < 1,]
            # tidy up if we fetched PAR and don't want it anymore
        if(!('PAR' %in% parameters)){
            print('removing PAR records')
            dat = dat[par != 'PAR']
        }
    }
    
    if(ct_temp_only == TRUE & 'TEMP' %in% parameters){
        ctSensors = 'Aanderaa Conductivity Sensor|FSI CT Module|Seabird|Waverider'
        print('removing non CT temperatures')
        dat = dat[!(!sensor %like% ctSensors & par == 'TEMP')]
    }
    if(is.na(averaging_period)){
        return(dat[order(dateTime),])
    }
    else{
        averaging_period = averaging_period*60*60 # convert to seconds
        dat$dateTime = as.POSIXct(round(as.numeric(dat$dateTime) / averaging_period) *
                                      averaging_period, origin = '1970-01-01', tz = 'UTC')
        dat = dat[,.(value = mean(value), stdev = mean(stdev), n = length(value), stdev_derived = mean(stdev_derived)),
            by = list(dateTime, deployment_group, deployment, depth, sensor, par)]
        return(dat[order(dateTime),])
    }
}

#' SmartBuoy burst level data query tool
#'
#' Fetches burst level SmartBuoy data
#'
#' @details This function querys the Smartbuoy database and returns 
#' @param deployment optional string matching smartbuoy deployment
#' @param parameters vector of parameter code names, defaults to c('TEMP', 'SAL', 'FTU', 'O2CONC', 'FLUORS')
#' @param db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return data.table with returned data in "long" format or error string if no data returned
#' @keywords smartbuoy esm2 query
#' @export
smartbuoy.fetch_burst <- function(deployment = NA,
                           parameters = NA,
                           db_name = 'smartbuoydblive'){
    deployment = gsub('/', '_', deployment)
    deptable= paste0('[SmartBuoyUser].[Result_', deployment,']')
    depJoinQuery = paste0(deptable, '.DepSensorId = [dbo].[DeploymentSensor].DepSensorId')
    parameters_fetch = paste0("('", paste(parameters, collapse = "', '"),"')")
    # stuff
        query = paste(
            "SELECT ResultTime as dateTime,",
            "BurstNumber, ResultFlag, ResultQuality,",
            "SerialNumber as serial," ,
            "ResultValue as Value" ,
                  "FROM", deptable,
            "INNER JOIN [dbo].[DeploymentSensor]",
            "ON", depJoinQuery,
            "INNER JOIN [dbo].[Sensor]",
            "ON [dbo].[DeploymentSensor].SensorId = [dbo].[Sensor].SensorId",
            "WHERE [Parcode] IN", parameters_fetch,
            "ORDER BY [ResultTime]" )
    print(query)
    sb = odbcConnect(db_name)
    dat = data.table(sqlQuery(sb, query))
    odbcCloseAll()
    dat[,mean := mean(Value), by = BurstNumber]
    dat[,sd:= sd(Value), by = BurstNumber]
    return(dat)
    dat$dateTime = as.POSIXct(dat$dateTime, format="%b %d %Y %I:%M%p", tz="UTC") 
}