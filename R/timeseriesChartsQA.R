#' SmartBuoy time series plots
#'
#' Fetches time series data for a deployment group
#'
#' @details TODO
#' @param deploymentGroup character string matching SmartBuoy deployment group (case sensitive)
#' @param db_name optional character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @param parcode character string matching parameter code of interested e.g. 'TEMP' or 'O2CONC'
#' @param yr integer vector corrisponding to year or years of interest e.g. c(2014, 2015), defaults to current year.
#' @param parcode2 TODO
#' @param CT_temp_only optional boolean, if True and parcode = 'TEMP' only CT derived temperature data is returned
#' @return dygraph or ggplot object depending on style, or if return_data is True a data.table for the timeseries.
#' @keywords profiler ctd esm2
#' @examples plot.sbts('DOWSING', 'O2CONC')
#' @examples plot.sbts('TH1', 'TEMP', yr = c(2012, 2013))
#' @export
plot.sbts <- function(deploymentGroup, parcode,
                      yr = year(now()),
                      parcode2 = FALSE,
                      CT_temp_only = TRUE,
                      style = 'dygraph',
                      include_telemetry = F,
                      db_name = 'smartbuoydblive'){
    require(RODBC)
    require(data.table)
    require(lubridate)
    # Database connection, modify string to match name in windows ODBC
    smartbuoydb = odbcConnect(db_name)

    # fetch the data from smartbuoy database
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
                        ([Parameter Code] IN ('", parcode, "'))
                        AND [Deployment Group Id] IN ('", deploymentGroup, "')
                        AND YEAR([Date/Time]) IN (", paste(yr, collapse = ", "),")
                        AND [Result Quality Flag] = 0
                        ORDER BY dateTime
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

        # if only CT temp wanted remove non ct data
    ctSensors = c('Aanderaa Conductivity Sensor - Type 3919B IW','Aanderaa Conductivity Sensor - Type 4319B IW','FSI CT Module')
    if(CT_temp_only & parcode == 'TEMP'){
        dat = dat[sensor %in% ctSensors,]
    }
        # TODO split graphs by depth if depth varies
    if(length(unique(dat$depth)) > 1){
        stop('multiple depths not yet supported')
    }

    if(include_telemetry == T){
        # query telemetry table, only get data where no burstmean data
        smartbuoydb = odbcConnect(db_name)
        telequery= paste0("
            SELECT (CAST([Date/Time] AS NVARCHAR)) as dateTime,
            [Parameter Code] as parcode,
            [Parameter Description] as pardesc,
            [Sensor Descr] as sensor,
            [Result - Mean] as result,
            [Depth Of Sensor] as depth,
            [Deployment Id] as deployment,
            [Deployment Group Id] as site
            FROM AdHocRetrieval_TelemetryResults
            WHERE NOT EXISTS
            (SELECT * FROM AdHocRetrieval_BurstMeanResults
                WHERE [Deployment Id] = AdHocRetrieval_TelemetryResults.[Deployment Id])
            AND ([Parameter Code] IN ('", parcode, "'))
            AND [Deployment Group Id] IN ('", deploymentGroup, "')
            AND YEAR([Date/Time]) IN (", paste(yr, collapse = ", "),")
            AND [Result Quality Flag] = 0
            ORDER BY dateTime
            ")
        teldat= sqlQuery(smartbuoydb, telequery)
        return(teldat)
        odbcCloseAll()
        teldat$dateTime = as.POSIXct(teldat$dateTime, format="%b %d %Y %I:%M%p",tz="UTC")
        teldat = data.table(teldat)
        
        if(CT_temp_only & parcode == 'TEMP'){
            teldat = teldat[sensor %in% ctSensors,]
        }
        teldat$deployment = paste0(teldat$deployment, '_telemetry')
        # check if burstmean data is available and if so discard
        # merge tables
        dat = rbind(dat, teldat, fill = T)
    }

        # plots
    if(style == 'dygraph'){
        require(dygraphs)
        require(xts)
        parcodes = unique(dat$parcode)
        dts = dcast.data.table(dat, dateTime ~ pardesc + deployment + sensor, value.var = 'result')

        dts= xts(dts[,!"dateTime", with=F], order.by = dts$dateTime)
        print('generating dygraph..')
        return(list('data' = dat, 'dygraph' = dygraph(dts, main = paste(deploymentGroup, yr)) %>% dyRangeSelector()))
    }
    if(style == 'ggplot'){
        require(ggplot2)
        print('ggplot not yet implemented')
    }else{ print('style not implemented') }
}
