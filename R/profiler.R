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
#' @param parameters vector of parameter code names, defaults to c('TEMP', 'SAL', 'FTU', 'O2CONC', 'PAR', 'FLUORS')
#' @param RQ0 boolean, if True only data where result quality = 0 is returned, i.e. good data. default is True
#' @param ct_temp_only boolean, if True all non-FSI temperature data is discarded, default is False.
#' @param min_QA_reached boolean, if True only data which has passed required QA level is returned, always used with QA0 = True.
#' @param db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return data.frame with returned data in "long" format or error string if no data returned
#' @keywords profiler ctd esm2 query
#' @export
profiler.fetch <- function(cruiseID = NA, profiler = NA,
                           after = NA, before = NA,
                           area = NA,
                           parameters = c('TEMP', 'SAL', 'FTU', 'O2CONC', 'PAR', 'FLUORS'),
                           RQ0 = TRUE, ct_temp_only = TRUE,
                           min_QA_reached = TRUE,
                           db_name = 'smartbuoydblive'){
    require(RODBC)
    require(data.table)
        # boilerplate query start
    query = paste("SELECT (CAST([Start Time] AS NVARCHAR)) as startTime,",
              "[Start Time Offset (secs)] as offset,",
              "[Site] as site,",
              "[Depth] as depth,",
              "[QA Status] as QA_level,",
              "[Result Quality] as QA_flag,",
              "[Result Value] as value,",
              "[Latitude] as latitude, [Longitude] as longitude,",
              "[Sensor Description] as sensor,",
              "[Parameter code] as par,",
              "[Station] as station,",
              "[Cruise Id] as cruise,",
              "[Logger Id] as profiler,",
              "[Notes] as notes",
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
    # check if valid data has been returned, if not quit
    if(nrow(dat) > 1){
        print(paste(nrow(dat), 'rows returned'))
    }else{
        stop("no data returned")
    }

    dat$startTime = as.POSIXct(dat$startTime, format="%b %d %Y %I:%M%p", tz="UTC")
    dat$dateTime = dat$startTime + dat$offset
    # dat[,max_depth := max(depth), by = list(startTime, profiler)]

        # if only CT temp wanted remove non ct data
    if(ct_temp_only == TRUE){
        ctSensors = 'Aanderaa Conductivity Sensor|FSI CT Module|Seabird'
        dat = dat[!(!sensor %like% ctSensors & par == 'TEMP')]
    }
    return(dat)
}

#' ESM2 profiler cruise id queryer
#'
#' Fetches lists of cruise id where ESM2 profiler data exists.
#'
#' @details This function querys the Smartbuoy database and returns a list of valid cruise IDs matching the supplied critiera where ESM2
#' profiler data is available. The v_CtdProfile_AllData table is used, as such private data will not be available to this function.
#' @param yr integer specifing a year to limit the search, default is 'ALL'
#' @param db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return data.frame of Cruise and instrument Id's
#' @keywords profiler ctd esm2 query
#' @export
profiler.cruiselist <- function(yr = 'ALL', db_name = 'smartbuoydblive'){
    require(RODBC)
    query = paste("SELECT DISTINCT [CruiseId], [InstId] FROM [SmartBuoy].[dbo].[CtdHeader]",
                  "INNER JOIN [SmartBuoy].[dbo].[CtdConfig]",
                  "ON [SmartBuoy].[dbo].[CtdHeader].CtdConfigId = [SmartBuoy].[dbo].[CtdConfig].CtdConfigId")
    if(yr != 'ALL'){
        query = paste(query, ' WHERE YEAR([StartDate]) = ', yr, sep = '')
    }
    sb = odbcConnect(db_name)
    cruiseList = sqlQuery(sb, query)
    cruiseList = cruiseList[order(cruiseList$CruiseId),]
    odbcCloseAll()
    return(data.frame(cruiseList))
}

#' ESM2 profiler header queryer
#'
#' @param yr
#' @param db_name
#'
#' @return data.frame of headers
#' @export
profiler.header <- function(yr = 'ALL', db_name = 'smartbuoydblive'){
    require(RODBC)
    query = paste("SELECT [CruiseId], [InstId], [Latitude], [Longitude], [StartDate] FROM",
                  "[SmartBuoy].[dbo].[CtdHeader]",
                  "INNER JOIN [SmartBuoy].[dbo].[CtdConfig]",
                  "ON [SmartBuoy].[dbo].[CtdHeader].CtdConfigId = [SmartBuoy].[dbo].[CtdConfig].CtdConfigId")
    if(yr != 'ALL'){
        query = paste(query, ' WHERE YEAR([StartDate]) = ', yr, sep = '')
    }
    sb = odbcConnect(db_name)
    header = sqlQuery(sb, query)
    header = header[order(header$CruiseId),]
    odbcCloseAll()
    return(data.table(header))
}

#' ESM2 profiler depth binning
#'
#' Binns profiler data into depth bins
#'
#' @details TODO, default boxcar
#' @param x data.frame matching output from fetch.profiler
#' @param bin_height numeric vector determining depth binning interval, default is 0.5
#' @param method function for binning, default is round, ceiling and floor also work
#' @param use_cast character string matching matching cast required, options are UP, DOWN and ALL
#' @param return_bin numeric vector, only these bin depths will be returned
#' @param O2_trim if true the first measurement in each bin is dropped, this is to give the sensor time to respond
#' @return character vector of Cruise Id's
#' @keywords profiler ctd esm2 query
#' @export
profiler.binning <- function(x, bin_height= 1,
                             method = round,
                             use_cast = 'UP',
                             return_bin = 'all',
                             O2_trim = F){
    dat = data.table(x)     # just make sure
    dat[,max_depth := max(depth), by = startTime]
    if(use_cast == 'UP'){
        # subset to up cast only
        max_depth_offsets =  dat[depth == max_depth, list(max_depth_offset = max(offset)), by = startTime]
        dat = merge(dat, max_depth_offsets, by=  'startTime')
        dat = dat[offset >= max_depth_offset, !"max_depth_offset", with = F] # select all but max_depth_time
    }
    if(use_cast == 'DOWN'){
        # subset to down cast only
        max_depth_offsets =  dat[depth == max_depth, list(max_depth_offset = max(offset)), by = startTime]
        dat = merge(dat, max_depth_offsets, by=  'startTime')
        dat = dat[offset <= max_depth_offset, !"max_depth_offset", with = F] # select all but max_depth_time
    }
    dat = dat[, depth_bin := method(depth / bin_height) * bin_height]
    dat[,endTime := startTime + max(offset), by = startTime]

    if(O2_trim & "O2CONC" %in% dat$par){
      print("Trimming oxygen")
      # identify first measurement in bin to remove
      dat[, bin_start := 0]
      dat[par == "O2CONC",bin_start := min(offset, na.rm = T), by = list(par, startTime, depth_bin)]
      dat = dat[bin_start != offset]
    }
      print(nrow(dat))

    dat = dat[, list(bin_mean = mean(value, na.rm = T), count = length(value), bin_sd = sd(value, na.rm = T)),
        by = list(startTime, endTime, latitude, longitude, cruise, station, site, profiler, depth_bin, par)]

    if(return_bin != 'all'){
        dat = dat[depth_bin == return_bin,]
    }
    return(dat)
}

#' ESM2 profiler ferrybox matching
#'
#' Fetches and matches profiler and ferrybox data
#'
#' @details TODO
#' @param cruiseID cruise ID string, e.g. "CEND_02_14"
#' @param parameters vector of parameter code names, defaults to c('TEMP')
#' @param min_QA_reached boolean, if True only data which has passed required QA level is returned, always used with QA0 = True.
#' @param ferrybox_db_name character string matching ODBC data source name, defaults to 'ferrybox'
#' @param smartbuoy_db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return list containing matched data and regression ggplot
#' @keywords profiler ctd ferrybox QA
#' @export
profiler.match_ferrybox <- function(cruiseID = NA,
                                    parameters = c('TEMP'),
                                    min_QA_reached = F,
                                    ferrybox_db_name = 'ferrybox',
                                    smartbuoy_db_name = 'smartbuoydblive'){

    require(RODBC)
    require(data.table)
    # matching first wet times >3.5m < 4.5 up to 1min mean, match to ferrybox
        # fetch profiler data
    ctd = profiler.fetch(cruiseID = cruiseID, parameters = parameters,
                         db_name = smartbuoy_db_name, min_QA_reached = min_QA_reached)
        # filter by depth
    ctd = ctd[depth > 3.5 & depth < 4.5]
        # round to minute
    ctd[,dateTime := as.POSIXct(round(as.numeric(dateTime)/60)*60,origin = '1970-01-01',  tz = 'UTC')]
        # bin to minute
    ctd = ctd[,.(value = mean(value)), by = list(dateTime, site, latitude, longitude, sensor, par, cruise, profiler)]

        # fetch ferrybox
    fb = ferrybox.fetch(cruiseID = cruiseID, parameters = parameters,
                        db_name = ferrybox_db_name, min_QA_reached = min_QA_reached)
        # if temperature only use ADAM
    fb =  fb[sensor == 'ADAM PT100 PRT' | par != 'TEMP']
        # only some columns needed
    fb = fb[,.(sensor, par, dateTime, latitude, longitude, ferrybox_speed = speed, value)]
        # fast data table merge
    setkey(fb, dateTime, par)
    setkey(ctd, dateTime, par)
    matched =  merge(fb, ctd, suffixes = c('_ferrybox', '_profiler'))
    setcolorder(matched, order(colnames(matched)))

    lm_eqn = function(x, y){
        m = lm(y ~ x);
        eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                         list(a = format(coef(m)[1], digits = 2),
                              b = format(coef(m)[2], digits = 2),
                              r2 = format(summary(m)$r.squared, digits = 3)))
        as.character(as.expression(eq));
    }
    eq = matched[, .(eq = lm_eqn(value_ferrybox, value_profiler),
                     xpos = min(value_profiler) + ((max(value_profiler - min(value_profiler))/2)),
                     ypos = max(value_ferrybox)), by = list(cruise, par)]

    plt = ggplot(matched, aes(value_profiler, value_ferrybox)) +
        geom_point() + geom_smooth(method = 'lm') +
        geom_text(data = eq, aes(x = xpos, y = ypos, label = eq),
                  parse = T) +
        facet_grid(cruise ~ par, scales = 'free') +
        theme_bw()
    print(plt)

    return(list(data = matched, plot = plt))
}