# Functions which interface with the Smartbuoy database for buoy data


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
#' @param night_flu_only optional boolean, if True (default) only night flu data will be returned
#' @param db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return data.table with returned data in "long" format or error string if no data returned
#' @keywords smartbuoy esm2 query
#' @import data.table RODBC
#' @export
smartbuoy.fetch <- function(deployment = NA, deployment_group = NA,
                           after = NA, before = NA,
                           parameters = c('TEMP', 'SAL', 'FTU', 'O2CONC', 'FLUORS'),
                           min_QA_reached = TRUE,
                           RQ0 = TRUE, ct_temp_only = TRUE,
                           averaging_period = NA,
                           night_flu_only = FALSE,
                           db_name = 'smartbuoydblive'){

    if(min_QA_reached == TRUE & RQ0 == TRUE){
        # boilerplate query start
        query = paste("SELECT (CAST([Date/Time] AS NVARCHAR)) as dateTime,",
                  "[Deployment Id] as deployment,",
                  "[Deployment Group Id] as deployment_group,",
                  "[Deployment Latitude] as lat,",
                  "[Deployment Longitude] as lon,",
                  "[Depth Of Sensor] as depth,",
                  "[Result - mean] as value,",
                  "[Result - Std Dev] as stdev,",
                  "[Result - Count] as n,",
                  "[Result - Error] as stdev_derived,",
                  "[Sensor Description] as sensor,",
                  "[SerialNumber] as sensor_serial,",
                  "[Parameter code] as par,",
                  "[Parameter Unit] as unit,",
                  "[ParCodeFrom] as calculated_from,",
                  "[Slope] as calibration_slope,",
                  "[Intercept] as calibration_intercept",
                  "FROM v_BurstMean_QaData",
                  "INNER JOIN DeploymentSensor ON",
                  "v_BurstMean_QaData.[Deployment/Sensor Id] = DeploymentSensor.DepSensorId",
                  "INNER JOIN Sensor ON",
                  "DeploymentSensor.SensorId = Sensor.SensorId",
                  "LEFT JOIN DeploymentAuditQa3Derived ON",
                  "v_BurstMean_QaData.[Deployment/Sensor Id] = DeploymentAuditQa3Derived.[DepSensorId] AND",
                  "v_BurstMean_QaData.[Parameter Code] = DeploymentAuditQa3Derived.[ParCodeDerived]"
                  )
    }else{
        query = paste("SELECT (CAST([Date/Time] AS NVARCHAR)) as dateTime,",
                  "[Deployment Id] as deployment,",
                  "[Deployment Group Id] as deployment_group,",
                  "[Deployment Latitude] as lat,",
                  "[Deployment Longitude] as lon,",
                  "[Depth Of Sensor] as depth,",
                  "[QA Level] as QA_level,",
                  "[Result Quality Flag] as QA_flag,",
                  "[Result - mean] as value,",
                  "[Result - Std Dev] as stdev,",
                  "[Result - Count] as n,",
                  "[Sensor Descr] as sensor,",
                  "[Sensor Serial Number] as sensor_serial,",
                  "[Parameter Code] as par,",
                  "SensorParameter.[ParUnit] as unit",
                  "FROM AdHocRetrieval_BurstMeanResults",
                  "INNER JOIN SensorParameter ON",
                  "AdHocRetrieval_BurstMeanResults.[Sensor Id] = SensorParameter.SensorId AND",
                  "AdHocRetrieval_BurstMeanResults.[Parameter Code] = SensorParameter.ParCode")
    }

        # collapse down parameters vector and wrap with quotes to work with IN (xxx)
    parameters_fetch = paste(parameters, collapse = "', '")

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

    # check if valid data has been returned, if not quit
    if(! nrow(dat) > 1){
        stop("no data returned")
    }
    dat$dateTime = as.POSIXct(dat$dateTime, format="%b %d %Y %I:%M%p", tz="UTC")

      # if we only want night fluorometery, use the insol package to work out when sunrise is and subset
    if(night_flu_only & "FLUORS" %in% parameters){
      dat[, sunrise := as.data.frame(insol::daylength(lat, lon, insol::daydoy(dateTime), 0))$sunrise]
      dat[, sunset := as.data.frame(insol::daylength(lat, lon, insol::daydoy(dateTime), 0))$sunset]
      dat[, dhour := lubridate::hour(dateTime) + (lubridate::minute(dateTime)/60)]
      dat = dat[(par == "FLUORS" & (dhour < sunrise | dhour > sunset)) | par != "FLUORS",]
      dat = dat[,!c("sunrise", "sunset", "dhour"), with = F]
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
            by = list(dateTime, deployment_group, deployment, lat, lon, depth, sensor, sensor_serial, unit, par)]
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
#' @import data.table RODBC
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
            "SELECT [ResultTime] as dateTime,",
            "ResultId,",
            "BurstNumber, ResultFlag, ResultQuality,",
            "SerialNumber as serial,",
            "Parcode as parameter,",
            "ResultValue,",
            "ResultValueQA",
                  "FROM", deptable,
            "INNER JOIN [dbo].[DeploymentSensor]",
            "ON", depJoinQuery,
            "INNER JOIN [dbo].[Sensor]",
            "ON [dbo].[DeploymentSensor].SensorId = [dbo].[Sensor].SensorId",
            "WHERE [Parcode] IN", parameters_fetch,
            "ORDER BY [ResultTime]" )
    print(query)
    sb = odbcConnect(db_name)
    dat = data.table(sqlQuery(sb, query, as.is = T))
    odbcCloseAll()
    dat$dateTime = as.POSIXct(dat$dateTime, format="%Y-%m-%d %H:%M:%S", tz="UTC")
    dat[, ResultValue := as.numeric(ResultValue)]
    dat[, ResultValueQA := as.numeric(ResultValueQA)]
    return(dat)
}

#' SmartBuoy T/S plots
#'
#' Draws a T/S plot for a SmartBuoy Deployment
#'
#' @details TODO
#' @param deployment character string matching SmartBuoy deployment (case sensitive)
#' @param db_name optional character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return ggplot object
#' @keywords esm2
#' @import ggplot2 data.table RODBC
#' @export
smartbuoy.TS <- function(deployment, db_name = 'smartbuoydblive'){

    smartbuoydb = odbcConnect(db_name)
    queryString = paste0("
                        SELECT (CAST([Date/Time] AS NVARCHAR)) as dateTime,
                        [Parameter Code] as parcode,
                        [Parameter Description] as pardesc,
                        [Sensor Descr] as sensor,
                        [Result - Mean] as result,
                        [Depth Of Sensor] as depth,
                        [Deployment Id] as deployment,
                        [Deployment Group Id] as site,
                        [QA Level] as QAlevel
                        FROM AdHocRetrieval_BurstMeanResults
                        WHERE
                        [Parameter Code] IN ('TEMP', 'SAL')
                        AND [Deployment Id] IN ('", paste(deployment, collapse = "', '"), "')
                        AND [Result Quality Flag] = 0
                        ")
    dat = sqlQuery(smartbuoydb, queryString)
    odbcCloseAll()
        # check data is valid
    if(nrow(dat) > 1){
        print(paste(nrow(dat), 'rows returned'))
    }else{
        stop('no data returned')
    }
        # convert back from the character representation of date back to POSIX
    dat$dateTime = as.POSIXct(dat$dateTime, format="%b %d %Y %I:%M%p",tz="UTC")
    dat = data.table(dat)

    ctSensors = 'Aanderaa Conductivity Sensor|FSI CT Module|Seabird'
    dat = dat[sensor %like% ctSensors,]
    dat$result = as.numeric(dat$result)

    # plots
    dat = dcast.data.table(dat, dateTime + deployment ~ parcode, value.var = 'result', fun.aggregate = mean)
    dat = na.omit(dat)
    dat$day = (as.numeric(dat$dateTime) - as.numeric(min(dat$dateTime))) / 86400
    gp = ggplot(dat) + geom_point(aes(SAL, TEMP, colour = day)) + theme_bw() + scale_color_gradientn(colours = rainbow(7))
    return(list('data' = dat, 'ggplot' = gp))
}


#' Sensor calibration query
#'
#' Tool for fetching serial numbers and calibration dates for SmartBuoy sensor
#'
#' @param deployment optional string vector denoting deployment names e.g. "DOWSING/042"
#' @param deployment_group optional string vector denoting deployment group names e.g. "DOWSING"
#' @param parameters optional parameter codes to match sensors by e.g. c("FLUORS", "FTU")
#' @param db_name optional character string matching ODBC data source name, defaults to 'smartbuoydblive'
#'
#' @return data.table containing query results
#' @import data.table RODBC
#' @export
smartbuoy.sensorCals <- function(deployment = NA, deployment_group= NA,
                                 parameters = NA,
                                 db_name = "smartbuoydblive"){
  query= paste0("
  SELECT DISTINCT
  Sensor.SensorId AS sensorid,
  SerialNumber, SensorDescr,
  SensorCalibration.StartDate AS CalibrationDate,
  SensorCalibration.EndDate, CalibrationInterval, Sensor.CurrentRecord,
  Deployment.DepId, Deployment.DepDateFrom, Deployment.DepDateTo
  FROM Sensor
  INNER JOIN SensorParameter ON Sensor.SensorId = SensorParameter.SensorId
  INNER JOIN SensorCalibration ON SensorParameter.SensorParameterId = SensorCalibration.SensorParameterId
  INNER JOIN DeploymentSensor ON Sensor.SensorId = DeploymentSensor.SensorId
  INNER JOIN Deployment ON DeploymentSensor.DepId = Deployment.DepId
  WHERE SensorCalibration.EndDate IS NULL
  ")
  if(!is.na(parameters)){
    # collapse down parameters vector and wrap with quotes to work with IN (xxx)
    parameters_fetch = paste(parameters, collapse = "', '")
    query = paste0(query, " AND ParCode IN ('", parameters_fetch, "')")
  }
  # filter deployments, is.na evaluates each element of vector, so only check first one is not NA
  if(!is.na(deployment[1])){
    deployment = paste(deployment, collapse = "', '")
    query = paste0(query, " AND DepId IN ('", deployment, "')")
  }
  if(!is.na(deployment_group[1])){
    deployment_group = paste(deployment_group, collapse = "', '")
    query = paste0(query, " AND DepGroupId IN ('", deployment_group, "')")
  }
  sb = odbcConnect(db_name)
  print(query)
  dat = setDT(sqlQuery(sb, query, as.is = T))
  odbcCloseAll()
  dat[, CalibrationDate := as.POSIXct(CalibrationDate, tz="UTC")]
  dat[, DepDateTo := as.POSIXct(DepDateTo, tz="UTC")]
  dat[, LastCalDate := min(CalibrationDate), by=sensorid]
  dat[, LastDeploymentDate := min(DepDateTo), by=sensorid]
  dat = dat[CalibrationDate == LastCalDate & DepDateTo == LastDeploymentDate]
  dat = dat[,.(SerialNumber, SensorDescr, CalibrationDate, CalibrationInterval, DepId, DepDateTo, CurrentRecord)]
  return(dat)
}



#' Query smartbuoy deployment position from telemetry messages
#'
#' @param deployment provide if you want a specific buoy deployment e.g. "DOWSING/040"
#' @param deployment_group provide if you want everything for a site e.g. "WESTGAB2"
#'
#' @return data.table of lat-long from telemetry messages
#' @export
#'
smartbuoy.telemetry_position <- function(deployment = NA, deployment_group = NA,
                                         db_name = "smartbuoydblive"){
  query= paste0("
  SELECT DeploymentInstrument.DepId as deployment
  ,[DepGroupId] as deployment_group
  ,[SequenceNumber]
  ,[TelTime]
  ,[TelLocLat] as lat
  ,[TelLocLong] as lon
  FROM [SmartBuoy].[dbo].[TelemetryHeader]
  INNER JOIN DeploymentInstrument ON
  TelemetryHeader.DepInstId = DeploymentInstrument.DepInstId
  INNER JOIN Deployment ON
  DeploymentInstrument.DepId = Deployment.DepId
  WHERE
  SequenceNumber > 1")
  if(!is.na(deployment[1])){
      deployment = paste(deployment, collapse = "', '")
      query = paste0(query, " AND DeploymentInstrument.[DepId] IN ('", deployment, "')")
  }
  if(!is.na(deployment_group[1])){
      deployment_group = paste(deployment_group, collapse = "', '")
      query = paste0(query, " AND [DepGroupId] IN ('", deployment_group, "')")
  }
  query = paste0(query, " ORDER BY [TelTime]")
  sb = odbcConnect(db_name)
  dat = data.table(sqlQuery(sb, query, as.is = T))
  odbcCloseAll()
  dep_check = deployment %in% dat$deployment | deployment_group %in% dat$deployment_group
  dat[, lat := as.numeric(lat)]
  dat[, lon := as.numeric(lon)]
  if(FALSE %in% dep_check){ # not working correctly
    warning(paste(deployment[!dep_check], "has no telemetry data"))
  }
  return(dat[order(deployment)])
}


#' SmartBuoy positions
#'
#' Fetches median positions of deployments
#'
#' @param group if True (default) aggregates position by deployment group
#' @param db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return data.table of positions
#' @keywords SmartBuoy
#' @import data.table RODBC
#' @export
smartbuoy.positions <- function(db_name = 'smartbuoydblive', group=T){
    sbdb = odbcConnect(db_name)

    pos_query = paste("SELECT
                      Deployment.[DepLocLat] as lat, Deployment.[DepLocLong] as long,
                      Deployment.[DepGroupId] as groupId, Deployment.[DepDateFrom] as dateFrom,
                      Deployment.[DepDateTo] as dateTo, Deployment.[DepDescr] as description,
                      Deployment.[DepId] as dep, Platform.[PlatformId] as platformId,
                      Platform.[PlatformTypeId] as platform
                      FROM Deployment INNER JOIN Platform ON Deployment.PlatformId=Platform.PlatformId ")
    d = data.table(sqlQuery(sbdb, pos_query))
    odbcCloseAll()

    d = d[platform %in% c(1, 4, 8)] # remove non-standard deployments
    d = d[!(groupId %in% c('LOWTEST', 'ESM2TEST'))] # remove test deployments
    if(group){
      d = d[,list(lat = median(lat), lon = median(long),
                  dateTo = max(dateTo), dateFrom = min(dateFrom),
                  platform = platform[1]), by = groupId] # group by deployment group
    }else{
      d = d[,list(lat = median(lat), lon = median(long),
                  dateTo = max(dateTo), dateFrom = min(dateFrom),
                  platform = platform[1]), by = dep] # group by deployment
    }
    d$active = "inactive"
    d$active[d$dateTo > lubridate::now()] = "active"

    d[platform == 1, platformName := 'SmartBuoy']
    d[platform == 4, platformName := 'Lander']
    d[platform == 8, platformName := 'Waverider']
    return(d[,.(deployment = groupId, lat, lon, dateFrom, dateTo, platform = platformName, active)])
}

#' SmartBuoy parameter codes
#'
#' Fetches parameter codes from SmartBuoy database
#'
#' @param db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return data.table of parcodes
#' @keywords SmartBuoy
#' @import data.table RODBC
#' @export
smartbuoy.parcodes <- function(db_name = 'smartbuoydblive'){
  sbdb = odbcConnect(db_name)
  pos_query = paste("SELECT
                    [ParCode], [ParDescr] as description, [DefaultUnit]
                    FROM Parameter ORDER BY [ParCode]")
  d = data.table(sqlQuery(sbdb, pos_query))
  odbcCloseAll()
  return(d)
}
