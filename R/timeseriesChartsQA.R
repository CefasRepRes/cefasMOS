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
#' @param return_data optional boolean, if True timeseries data is returned rather than a plot
#' @return dygraph or ggplot object depending on style, or if return_data is True a data.table for the timeseries.
#' @keywords profiler ctd esm2
#' @examples plot.sbts('DOWSING', 'O2CONC')
#' @examples plot.sbts('TH1', 'TEMP', yr = c(2012, 2013))
#' @export
plot.sbts <- function(deploymentGroup, parcode,
                      yr = year(now()),
                      parcode2 = FALSE,
                      CT_temp_only = TRUE,
                      return_data = FALSE, style = 'dygraph',
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
    if(CT_temp_only & parcode == 'TEMP'){
        ctSensors = c('Aanderaa Conductivity Sensor - Type 3919B IW','Aanderaa Conductivity Sensor - Type 4319B IW','FSI CT Module')
        dat = dat[sensor %in% ctSensors,]
    }
        # TODO split graphs by depth if depth varies
    if(length(unique(dat$depth)) > 1){
        # stop('multiple depths not yet supported')
    }
    
        # plots
    if(style == 'dygraph'){
        require(dygraphs)
        require(xts)
        parcodes = unique(dat$parcode)
        dts = dcast.data.table(dat, dateTime ~ pardesc + deployment + sensor, value.var = 'result', fun.aggregate = mean)
        
        dts= xts(dts[,!"dateTime",with=F], order.by = dts$dateTime)
        print('generating dygraph..')
        return(dygraph(dts, main = paste(c(deploymentGroup, yr))) %>% dyRangeSelector())
    }
    if(style == 'ggplot'){
        require(ggplot2)
        print('ggplot not yet implemented')
    }else{ print('style not implemented') }
}