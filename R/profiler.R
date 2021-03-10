#' ESM2 profiler data query tool
#'
#' Fetches CTD data from ESM2 profiler records
#'
#' @details This function querys the Smartbuoy database and returns ESM2 profiler data matching the provided critiera.
#' Note that startTime is the time at which the instrument started logging.
#' remember to do `options(digits.secs=3)` if you want to display POSIXct miliseconds.
#' @param cruiseID optional cruise ID string, e.g. "CEND_02_14", if provided only data from this cruise will be returned.
#' @param profiler optional profiler name string, e.g. "PR009", if provided only data from this profiler will be returned.
#' @param after optional date string, if provided only data after this date will be returned, assumes UTC e.g. "2014-08-10"
#' @param before optional date string, if provided only data before this date will be returned, assumes UTC e.g. "2014-12-09"
#' @param area optional area vector consisting of 4 elements, minimum Latitude, minimum Longitude, maximum Latitude, maximum Longitude e.g. c(52, -3.5, 53, -4)
#' @param parameters vector of parameter code names, defaults to c('TEMP', 'SAL', 'FTU', 'O2CONC', 'PAR', 'FLUORS')
#' @param RQ0 boolean, if True only data where result quality = 0 is returned, i.e. good data. default is True
#' @param ct_temp_only boolean, if True all non-FSI temperature data is discarded, default is False.
#' @param min_QA_reached boolean, if True only data which has passed required QA level is returned, always used with QA0 = True.
#' @param privateData boolean, if True will return non-public data (e.g. commerical contracts), default is False.
#' @param debug boolean, if True will return raw query output
#' @param db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return data.frame with returned data in "long" format or error string if no data returned
#' @keywords profiler ctd esm2 query
#' @import RODBC
#' @export
profiler.fetch <- function(cruiseID = NA, profiler = NA,
                           after = NA, before = NA,
                           area = NA,
                           parameters = c('TEMP', 'SAL', 'FTU', 'O2CONC', 'PAR', 'FLUORS'),
                           RQ0 = TRUE, ct_temp_only = TRUE,
                           min_QA_reached = TRUE,
                           privateData = FALSE,
                           debug = FALSE,
                           db_name = 'smartbuoydblive'){
        # boilerplate query start
    query = {paste(
      "SELECT",
      "(CAST(StartDate AS NVARCHAR)) AS startTime,",
      "StartTimeOffset AS offset,",
      "0 AS dateTime,",
      "Latitude AS latitude,",
      "Longitude AS longitude,",
      "Site AS site,",
      "Station AS station,",
      "CruiseId AS cruise,",
      "Notes AS notes,",
      "InAir_Pressure,",
      "InAir_PAR,",
      "CtdData.ParCode AS par,",
      "Depth AS depth,",
      "QaStatus AS QA_level,",
      "ResultQuality AS QA_flag,",
      "SensorDescr AS sensor,",
      "sensor.SerialNumber AS serial,",
      "CtdConfig.InstId as profiler,",
      "ResultValue AS value,",
      "ParUnit AS unit",
      "FROM CtdHeader",
      "INNER JOIN CtdConfig ON CtdHeader.CtdConfigId = CtdConfig.CtdConfigId",
      "INNER JOIN CtdData ON CtdHeader.CtdHeaderId = CtdData.CtdHeaderId",
      "INNER JOIN Sensor ON CtdData.SensorId = Sensor.SensorId",
      "INNER JOIN Instrument ON CtdConfig.InstId = Instrument.InstId",
      "INNER JOIN SensorParameter ON Sensor.SensorId = SensorParameter.SensorId",
      "AND CtdData.ParCode = SensorParameter.ParCode",
      "WHERE CtdData.ParCode IN"
    )}

    options(digits.secs=3)

        # collapse down parameters vector and wrap with quotes to work with IN (xxx)
    parameters = paste(parameters, collapse = "', '")
    query = paste0(query, " ('", parameters, "') ")

        # if cruise id is suppled build filter into query
    if(!is.na(cruiseID[1])){
        cruiseID = paste(cruiseID, collapse = "', '")
        query = paste0(query, "AND CruiseId IN ('", cruiseID,"')")
    }
        #
    if(privateData == FALSE){
        query = paste0(query, " AND IsPrivate = 0")
    }
        # if profiler id is suppled build filter into query
    if(!is.na(profiler[1])){
        profiler = paste(profiler, collapse = "', '")
        query = paste0(query, " AND CtdConfig.InstId IN ('", profiler,"')")
    }
      # if area is suppled build filter into query
      # area = c(minLat, minLon, maxLat, maxLon)
    if(!is.na(area[1])){
      # check if area contains 4 elements
      if(!length(area) == 4){
        stop('area does not have 4 elements')
      }else{
        query = paste0(query,
                       " AND [Latitude] >= ", area[1],
                       " AND [Latitude] <= ", area[3],
                       " AND [Longitude] >= ", area[2],
                       " AND [Longitude] <= ", area[4])
        }
    }

      # if before or after is suppled build filter into query
    if(!is.na(before)){
        query = paste0(query, " AND StartDate <= '", before, "'")
    }
    if(!is.na(after)){
        query = paste0(query, " AND StartDate >= '", after, "'")
    }

      # if only RQ0 data is required build filter into query
    if(RQ0 == TRUE){
      query = paste(query, "AND ResultQuality = 0 ")
    }

    if(min_QA_reached == TRUE){
        query = paste(query, "AND QaStatus >= QaStatusMinPublishLevel")
    }

    # finally
    query = paste(query, 'ORDER BY startTime')

    print(query)
    sb = odbcConnect(db_name)
    if(debug){
      out = sqlQuery(sb, query, stringsAsFactors=F)
      odbcCloseAll()
      return(out) # return output data
    }
    dat = data.table(sqlQuery(sb, query, stringsAsFactors=F))
    odbcCloseAll()
    # check if valid data has been returned, if not quit
    if(nrow(dat) > 1){
        print(paste(nrow(dat), 'rows returned'))
    }else{
        stop("no data returned")
    }

    dat[, startTime := as.POSIXct(startTime, format="%b %d %Y %I:%M%p", tz="UTC")]
    dat[, dateTime := startTime + offset]

        # if only CT temp wanted remove non ct data
    dat[grepl("optode", sensor) & par == "TEMP", par := "O2TEMP"]
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
#' @import RODBC
#' @export
profiler.cruiselist <- function(yr = 'ALL', db_name = 'smartbuoydblive'){
    query = paste("SELECT DISTINCT [CruiseId], [InstId] FROM [SmartBuoy].[dbo].[CtdHeader]",
                  "INNER JOIN [SmartBuoy].[dbo].[CtdConfig]",
                  "ON [SmartBuoy].[dbo].[CtdHeader].CtdConfigId = [SmartBuoy].[dbo].[CtdConfig].CtdConfigId")
    if(yr != 'ALL'){
        query = paste(query, ' WHERE YEAR([StartDate]) = ', yr, sep = '')
    }
    sb = odbcConnect(db_name)
    cruiseList = sqlQuery(sb, query, stringsAsFactors = F)
    cruiseList = cruiseList[order(cruiseList$CruiseId),]
    odbcCloseAll()
    return(data.frame(cruiseList))
}

#' ESM2 profiler header queryer
#'
#' @param yr integer year defaults to "ALL"
#' @param db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#'
#' @return data.frame of headers
#' @import RODBC
#' @export
profiler.header <- function(yr = 'ALL', db_name = 'smartbuoydblive'){
    query = paste("SELECT [CtdHeaderId], [CruiseId], [InstId], [Latitude], [Longitude], (CAST([StartDate] AS NVARCHAR)) AS startTime FROM",
                  "[SmartBuoy].[dbo].[CtdHeader]",
                  "INNER JOIN [SmartBuoy].[dbo].[CtdConfig]",
                  "ON [SmartBuoy].[dbo].[CtdHeader].CtdConfigId = [SmartBuoy].[dbo].[CtdConfig].CtdConfigId")
    if(yr != 'ALL'){
        query = paste(query, ' WHERE YEAR([StartDate]) = ', yr, sep = '')
    }
    sb = odbcConnect(db_name)
    header = data.table(sqlQuery(sb, query, stringsAsFactors = F))
    odbcCloseAll()
    header = header[order(CtdHeaderId)]
    header[, startTime := as.POSIXct(startTime, format="%b %d %Y %I:%M%p", tz="UTC")]
    return(header)
}

#' ESM2 profiler depth binning
#'
#' Bins profiler data into depth bins
#'
#' @details This function uses the standard "boxcar" approch to binning CTD profiles.
#' profiles can be split to use the average value at each depth from both up and down casts (the "ALL" option),
#' or the binning can be restricted to just the down or up casts. DOWN is usually best when the profiler has been deployed correctly,
#' UP is useful for comparison to niskin bottle firings.
#' Note that startTime and endTime corrispond to the time at the start or end of the cast.
#' e.g. if you select UP ast the use_cast option startTime will be the time at the start of the UP cast.
#' The UP cast is identified as being the time after the deepest part of the profile.
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
    dat[, endTime := startTime + max(offset), by = startTime]
    dat[, startTime := startTime + min(offset), by = startTime]

    if(O2_trim & "O2CONC" %in% dat$par){
      print("Trimming oxygen")
      # identify first measurement in bin to remove
      dat[, bin_start := 0]
      dat[par == "O2CONC",bin_start := min(offset, na.rm = T), by = list(par, startTime, depth_bin)]
      dat = dat[bin_start != offset]
    }

    dat = dat[, list(bin_mean = mean(value, na.rm = T), count = length(value), bin_sd = sd(value, na.rm = T)),
        by = list(startTime, endTime, latitude, longitude, cruise, station, site, profiler, depth_bin, par, serial)]

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

#' Estimate Kd from profile
#'
#' @description Estimates Kd PAR from downwelling PAR and depth using log par method.
#' @param depth vector of depth values
#' @param par vector of par values
#'
#' @return kd (m-1)
#' @export
profiler.kd <- function(depth, par){
  #
  lm(log(par)~depth)$coefficients[2]
}


#' Estimate optical depths from PAR profile
#'
#' @param depth vector of depth values
#' @param par vector of par values
#'
#' @return vector of optical depth zone for each depth
#' @export
profiler.odz <- function(depth, par){
  kd <- meankd(depth, par)
  odstart <- 0.1
  odstep <- 1
  # odmax <- min(depth)*kd
  ztop = odstart/kd
  inc = abs(odstep/kd)
  brks <- c(0,seq(ztop,max(abs(depth)),inc),999)
  cut(abs(depth),brks,labels = 1:(length(brks)-1)-1)
}


#' estimate euphotic depth from PAR profile
#'
#' Euphotic depth defined as 1% of surface
#'
#' @param depth vector of depth values
#' @param par vector of par values
#' @param surf_par surface par
#'
#' @return euphotic depth
#' @export
#'
profiler.eu_depth <-  function(depth, par, surf_par){
  # 1 % of surface value
  depth = depth[!is.na(par)]
  par = par[!is.na(par)]
  min(depth[par <= (surf_par[1]/100)], na.rm=T)
}


#' Split ESM2 profiler data in distinct profiles
#'
#' Split profiles for ESM2 profiler data.
#' If an ESM2 as been left sampling between station, or a station as been repeated the multiple casts will have the same "startTime"
#'
#' This function assumes that the ESM2 leaves the water between stations, uses that and a minimum time difference to mark seperate stations.
#' Specify a threshold for time-between-stations (`t_threshold`), and a threshold for when it is out of the water (`z_threshold`)
#'
#' A pure "out of water" threshold won't work because an ESM2 often bobs when near the surface
#'
#' @param x profiler data.table, such as that generated by `fetch.profiler`
#' @param t_threshold time interval in seconds, default is 180 s
#' @param z_threshold depth / pressure threshold in either meters or dbar
#' @param use_prs if true then function will use PRSADC and the `airprs` parameter to determine depth, otherwise uses depth column which needs to already be calculated
#' @param airprs in-air pressure reading, if using `use_prs` typically around 10 dbar
#'
#' @return data.table containing profiler data with startTimes updated and "profile" column added
#' @export
profiler.split <- function(x, t_threshold = 180, z_threshold = 0.25, use_prs = F, airprs = 10){
  if(use_prs == T){
    d = unique(x[par == "PRSADC",.(startTime, dateTime, offset, z = value - airprs)])
  }else{
    d = unique(x[,.(startTime, dateTime, offset, z = depth)])
  }
  d = d[z > z_threshold]
  d[, dt := c(0, diff(dateTime))]
  d[, profile := 0]
  d[dt > t_threshold, profile := 1]
  d[, profile := cumsum(profile) + 1]
  d[profile > 1, startTime := min(dateTime), by=profile]
  out = merge(x, d[,.(dateTime, profile)], by="dateTime")
  return(out)
}
