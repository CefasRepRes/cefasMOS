#' SmartBuoy time series plots
#'
#' Fetches time series data for a deployment group
#'
#' @details TODO
#' @param deploymentGroup character string matching SmartBuoy deployment group (case sensitive)
#' @param parcode character string matching parameter code of interested e.g. 'TEMP' or 'O2CONC'
#' @param yr integer vector corrisponding to year or years of interest e.g. c(2014, 2015), defaults to current year.
#' @param ct_temp_only optional boolean, if True and parcode = 'TEMP' only CT derived temperature data is returned
#' @param style character string matching graph style, either 'dygraph' (default) or 'ggplot'
#' @param include_telemetry optional boolean, if True (default) telemetry data is also used for the timeseries
#' @param night_flu_only optional boolean, if True (default) only quenched flu data will be removed
#' @param db_name optional character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return dygraph or ggplot object depending on style, or if return_data is True a data.table for the timeseries.
#' @keywords profiler ctd esm2
#' @export
smartbuoy.timeseries <- function(deploymentGroup, parcode,
                      yr = year(Sys.time()),
                      ct_temp_only = TRUE,
                      style = 'dygraph',
                      include_telemetry = TRUE,
                      night_flu_only = FALSE,
                      db_name = 'smartbuoydblive'){
    require(RODBC)
    require(data.table)

    if(length(parcode) > 1 & style == 'dygraph'){
        stop('dygraphs can only display 1 parameter')
    }

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
                        ([Parameter Code] IN ('", paste(parcode, collapse = "', '"), "'))
                        AND [Deployment Group Id] IN ('", paste(deploymentGroup, collapse = "', '"), "')
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
    ctSensors = 'Aanderaa Conductivity Sensor|FSI CT Module|Seabird|Waverider'
    if(ct_temp_only == TRUE & 'TEMP' %in% parcode){
        dat = dat[sensor %like% ctSensors,]
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
            AND ([Parameter Code] IN ('", paste(parcode, collapse = "', '"), "'))
            AND [Deployment Group Id] IN ('", paste(deploymentGroup, collapse = "', '"), "')
            AND YEAR([Date/Time]) IN (", paste(yr, collapse = ", "),")
            AND [Result Quality Flag] = 0
            ORDER BY dateTime
            ")
        teldat= sqlQuery(smartbuoydb, telequery)
        odbcCloseAll()
        teldat$dateTime = as.POSIXct(teldat$dateTime, format="%b %d %Y %I:%M%p",tz="UTC")
        teldat = data.table(teldat)

        if(ct_temp_only & 'TEMP' %in% parcode){
            teldat = teldat[sensor %like% ctSensors,]
        }
        teldat$deployment = paste0(teldat$deployment, '_telemetry')
        dat = rbind(dat, teldat, fill = T)
    }

    if(night_flu_only & "FLUORS" %in% parcode){
        # stop('night_flu_only not implemented')
        print('using PAR to subset, FLUORS threshold = 1 uE m-2 s-1')
        smartbuoydb = odbcConnect(db_name)
        PARquery = paste0("
                            SELECT (CAST([Date/Time] AS NVARCHAR)) as dateTime,
                            [Result - Mean] as par
                            FROM AdHocRetrieval_BurstMeanResults
                            WHERE [Parameter Code] = 'PAR'
                            AND [Depth Of Sensor] = 0
                            AND [Deployment Group Id] IN ('", paste(deploymentGroup, collapse = "', '"), "')
                            AND YEAR([Date/Time]) IN (", paste(yr, collapse = ", "),")
                            AND [Result Quality Flag] = 0
                            ORDER BY dateTime
                            ")
        par = sqlQuery(smartbuoydb, PARquery)
        odbcCloseAll()
        par$dateTime = as.POSIXct(par$dateTime, format="%b %d %Y %I:%M%p",tz="UTC")
        par = data.table(par)
        dat = merge(dat, par, by = 'dateTime', all = T, allow.cartesian = T) # allow.cartesian needed for duplicate timestamps
        dat = dat[is.na(par), par := -1]
        dat = dat[par < 1,]
    }

    dat$result = as.numeric(dat$result)

        # plots
    if(style == 'dygraph'){
        require(dygraphs)
        require(xts)
        dts = dcast.data.table(dat, dateTime ~ pardesc + deployment + sensor + QAlevel, value.var = 'result', fun.aggregate = median)
        dts = xts(dts[,!"dateTime", with = F], order.by = dts$dateTime)
        title = paste(paste(deploymentGroup, collapse = ', '), paste(yr, collapse = ', '))
        dg = dygraph(dts, main = title) %>% dyRangeSelector() %>% dyOptions(useDataTimezone = T)
        return(list('data' = dat, 'dygraph' = dg))
    }
    if(style == 'ggplot'){
        require(ggplot2)
        print('ggplot not yet implemented')
        return(dat)
        dep_label = dat[,list(x = median(dateTime), y = mean(result), lim = min(dateTime)), by = list(deployment, pardesc)]
        gp = ggplot(dat) + geom_line(aes(dateTime, result, colour = pardesc, group = deployment)) +
            geom_text(data = dep_label, aes(x, y, label = deployment), alpha = 0.8) +
            facet_grid(pardesc ~ ., scales = 'free_y') +
            theme_bw()
        return(list('data' = dat, 'ggplot' = gp))
    }else{ print('style not implemented') }
}

