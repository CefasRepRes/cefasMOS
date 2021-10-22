
#' propagate errors for data.table
#'
#' This function serves as a wrapper for `propagate::propagate` to make it easier to calculate errors in a data.table
#'
#' Errors are calculated though a 2nd order Taylor expansion (`mode = "prop"`) or though Monte-Carlo (`mode = "MC"`).
#'
#' @param expr an expression, such as `expression(x / y)`
#' @param dat a matrix denoting the mean and standard deviation, see examples
#' @param mode either `"prop"` or `"MC`
#' @param n number of iterations for Monte-Carlo, minimum is 10000
#'
#' @return a list of mean and standard deviations
#' @export
#'
#' @examples
#' dx = data.table(
#'   index = c(500, 1000),
#'   A = c(0, 5),
#'   A_sd = c(0.1, 0.1),
#'   B = c(1, 9),
#'   B_sd = c(0.1, 0.1))
#'
#' eq = expression(A + B^2)
#'
#' dx[, c("mean", "sd") := prop_dt(eq, cbind("A" = c(A, A_sd), "B" = c(B, B_sd))), by=index]
#'
#' # Can also be applied to a standard function, if you create a wrapper
#' d2 = data.table(
#'   time = c(1, 2),
#'   temp = c(20, 5),
#'   temp_sd = c(0.2, 0.1),
#'   sal = c(35.5, 34.5),
#'   sal_sd = c(0.01, 0.05))
#'
#' f = function(temp, sal){
#'   # a wrapper is needed if you don't supply every variable, and you can't have non-numeric variables
#'   oxygen.sat(temp, sal)
#' }
#'
#' d2[, c("mean", "sd") := prop_dt(f, cbind("temp" = c(temp, temp_sd), "sal" = c(sal, sal_sd))), by=time]
prop_dt <- function(expr, dat, mode = c("prop", "MC"), n = 1E+06){
  if(mode[1] == "prop"){
    res = propagate::propagate(expr, dat, do.sim = F)$prop
    mean_sd = as.list(res)[c(1, 3)]
  }
  if(mode[1] == "MC"){
    res = propagate::propagate(expr, dat, second.order = F, nsim = n)$sim
    mean_sd = as.list(res)[1:2]
  }
  return(mean_sd)
}

#' Fast melt to long data.table for large 2d matrix
#'
#' Provides a fast method to melt a large 2D matrix into a long format data.table
#' this is particularly useful for converting gridded data into a format for using with ggplot2
#'
#' @param m a 2d matrix
#' @param value_name text string for the data column, default is "value"
#'
#' @return a long data.table with row and col columns
#' @export
#'
#' @examples
#' m = matrix(1:9, nrow = 3, ncol = 3)
#' as.melt.data.table(m) # long format data
as.melt.data.table <- function(m, value_name = "value"){
  if(is.matrix(m) & length(dim(m)) == 2){
    DT = data.table(
      row = rep(seq_len(nrow(m)), ncol(m)),
      col = rep(seq_len(ncol(m)), each = nrow(m)),
      value = c(m)
    )
    setnames(DT, "value", value_name)
    return(DT)
  }else{
    error("m is not a 2D matrix, can't melt!")
  }
}


#' Time group
#'
#' given a timeseries with gaps, this returns a vector which can serve as a label for each unbroken period
#'
#' in short, it calculates the median time between all observations,
#'  and assigns a new group number when there is a gap.
#'
#' @param x vector or timestamps
#' @param fuzz amount of extra leeway in seconds to give the function to avoid small gaps, default is 0
#'
#' @return
#' @export
time_group <- function(x, fuzz = 0){
  dt = diff(as.numeric(x[order(x)]))
  breaks = c(F, dt > (median(dt) + fuzz))
  grps = cumsum(as.numeric(breaks)) + 1
  return(grps)
}

#' Round dateTime to x minutes
#'
#' @param x vector of POSIXct datetimes
#' @param minutes integer minutes to round to, default is 30
#'
#' @importFrom lubridate is.POSIXct
#' @return vector of rounded POSIXct
#' @export
round_minute <- function(x, minutes = 30){
  rt = minutes * 60
  if(lubridate::is.POSIXct(x)){
    return(
      as.POSIXct(round(as.numeric(x)/rt)*rt,
                 origin = "1970-01-01", tz = "UTC")
    )

  }else{
    stop("x is not POSIXct!")
  }
}

#' ftu from ADC
#'
#' Calculates FTU value from ESM2 raw hex ADC counts
#'
#' @details This function querys the Smartbuoy database and returns
#' @param x vector of ESM2 hex ADC counts, each count should be 6 characters i.e. "1a13D2"
#' @param factor channel calibration factor, specific to logger, default is 1.22
#' @param offset channel calibration offset, default is 0.0
#' @return vector of calibrated FTU values
#' @keywords esm2
#' @export
esm2.FTU_from_ADC<- function(x, factor = 1.22, offset = 0.0){
    which_range <- function(range){
        switch(range,
               "0" = 500,
               "1" = 100,
               "2" = 25,
               "3" = 5)
    }
    subfunc <- function(x){
        if(nchar(x) != 6){return(NA)}
        dec = as.numeric(paste0("0x", substr(x, 4, 6)))
        range = which_range(as.character(substr(x, 3, 3)))   # calculate factor from range
        dec.mv = dec / 1000 # convert to mV
        dec.c = (dec.mv * factor) - (offset / 1000)  # apply channel calibrations
        return(dec.c * range) # apply gain factor
    }
    return(sapply(x, subfunc))
}

#' FLUORS from ADC
#'
#' Calculates FLUORS value from ESM2 raw hex ADC counts
#'
#' @details This function querys the Smartbuoy database and returns
#' @param x vector of ESM2 hex ADC counts, each count should be 6 characters i.e. "1a13D2"
#' @param factor channel calibration factor, specific to logger, default is 1.22
#' @param offset channel calibration offset, default is 0.001
#' @return vector of calibrated FLUORS values
#' @keywords esm2
#' @export
esm2.FLUORS_from_ADC <- function(x, factor = 1.22, offset = 0.001){
    which_range <- function(range){
        switch(range,
               "0" = 30,
               "1" = 10,
               "2" = 3,
               "3" = 1)
    }
    subfunc <- function(x){
        if(nchar(x) != 6){return(NA)}
        dec = as.numeric(paste0("0x", substr(x, 4, 6)))
        range = which_range(as.character(substr(x, 3, 3)))   # calculate factor from range
        dec.mv = dec / 1000 # convert to mV
        dec.c = (dec.mv * factor) - (offset / 1000)  # apply channel calibrations
        return(dec.c * range) # apply gain factor
    }
    return(sapply(x, subfunc))
}

#' volts from ADC
#'
#' Calculates raw voltage value from ESM2 hex ADC counts
#'
#' @details This function querys the Smartbuoy database and returns
#' @param x vector of ESM2 hex ADC counts, each count should be 6 characters i.e. "1a13D2"
#' @param factor channel calibration factor, specific to logger, default is 1.22
#' @param offset channel calibration offset, default is 0.0
#' @return vector of channel corrected voltage values
#' @keywords esm2
#' @export
esm2.volts_from_ADC <- function(x, factor = 1.22, offset = 0.0){
  dec = as.numeric(paste0("0x", substr(x, 4, 6)))
  dec.mv = dec / 1000 # convert to mV
  dec.c = (dec.mv * factor) - (offset / 1000)  # apply channel calibrations
  return(dec.c)
}

#' Calculate Cefas LiCor PAR from voltage
#'
#' @description Calculates PAR from Licor Li-198 sensors
#'  fitted with Cefas inline low-light amplifier.
#'  Refer to calibration documents for factor and offset.
#'
#' @param x vector of output voltage (LICOR)
#' @param factor licor factor in volts
#' @param offset licor offset in volts
#'
#' @return numeric PAR in uE m-2 s-1
#' @export
PAR_from_Voltage <- function(x, factor, offset){
    return(factor * exp(offset * x))
}

#' Calculate salinity
#'
#' @description calculate salinity from conductivity, temperature and depth.
#' Adapted from marelac package by Karline Soetaert using
#' Fofonoff NP and Millard RC Jr, 1983. Algorithms for computation of fundamental properties of
#' seawater. UNESCO technical papers in marine science, 44, 53 pp.
#' http://unesdoc.unesco.org/images/0005/000598/059832EB.pdf
#' cran.r-project.org/web/packages/marelac
#'
#' @param Cond Conductivity in Sm, (SmartBuoy = mmoh/cm = 0.1 Sm)
#' @param t temperature
#' @param p gauge pressure in bar (reference to ambient)
#' @param P true pressure in bar
#'
#' @return salinity
#' @keywords salinity conductivity CTD
#' @export
SAL_from_CT <- function (Cond, t, p = max(0, P - 1.013253), P = 1.013253) {
    #
    # Cond in Sm-, 1 Sm = 10 mmoh/cm (Smartbuoy Unit)
    # e.g.R = cond / 4.2914
    # R = 0.955819857, T = 13.3208333, P = 37.38296403. -> 34.75933
    # p = gauge pressure, i.e. reference to local (bar)
    # P = true pressure (bar)

    R = (Cond / 10) / 4.2914 # as per UNESCO Cond in mmoh/cm
    P <- p # pressure in dbar
    C_P <- (2.07e-05 + (-6.37e-10 + 3.989e-15 * P) * P) * P
    DT <- t - 15
    R_T <- 0.6766097 + (0.0200564 + (0.0001104259 + (-6.9698e-07 + 1.0031e-09 * t) * t) * t) * t
    A_T <- 0.4215 + -0.003107 * t
    B_T <- 1 + (0.03426 + 0.0004464 * t) * t
    RT <- R/(R_T * (1 + C_P/(B_T + A_T * R)))
    RT <- sqrt(abs(RT))
    DS <- (DT/(1 + 0.0162 * DT)) * (5e-04 + (-0.0056 + (-0.0066 + (-0.0375 + (0.0636 + -0.0144 * RT) * RT) * RT) * RT) * RT)
    return(0.008 + (-0.1692 + (25.3851 + (14.0941 + (-7.0261 + 2.7081 * RT) * RT) * RT) * RT) * RT + DS)
}

#' Find mixed layer depth
#'
#' @description  simple threshold technique
#'
#' For de Boyer Montegut et al, 2004 definition set threshold to 0.03 and ref_z to 10
#'
#' @details returns max z if threshold not met
#' @param z numeric vector of z (either pressure or depth)
#' @param y vector of MLD indicating variable, typically temperature, or density
#' @param threshold numeric y threshold, default is 0.03 (assuming you using density in kg m^-3)
#' @param ref_z reference `z` for threshold, default is shallowest `z` available
#' @param surface default is true, set to false for bottom mixed layer threshold (base of gradient)
#' @param band default is false, if true function returns paired vector of interval which meets threshold
#' @param mean_ref if T will take the mean of the values above `ref_z` as the reference `y`
#'
#' @return mld in units of `z`
#' @export
#' @examples
#' x = data.table(pressure = 1:100,
#'                density = approx(c(1, 40, 50, 80, 100),
#'                                 c(1.125, 1.125, 1.293, 1.308, 1.308),
#'                                 xout = 1:100)$y)
#'
#' findMLD(x$pressure, x$density)
findMLD <- function(z, y, threshold = 0.03, ref_z = NA, surface = T, band=F, mean_ref=F){
  y = y[order(z)] # make sure you sort y first
  z = z[order(z)]
  # note this means if you supply a up and down cast they are combined together
  # internal waves or hysteresis of sensors means this should be avoided

  if(anyNA(z)){ error("NA's found in z record") }
  if(anyNA(y)){ warning("NA's found in y record") }

  if(!is.na(ref_z) & mean_ref == T){
    if(surface == T){
      y[z <= ref_z] = mean(y[z <= ref_z]) # calculate mean y for everything between ref_z and min_z
    }else{
      y[z >= ref_z] = mean(y[z >= ref_z]) # calculate mean y for everything between ref_z and max_z
    }
  }

  if(!is.na(ref_z)){
    # Subset to ref_z
    if(surface == T){
      y = y[z >= ref_z]
      z = z[z >= ref_z]
    }else{
      y = y[z <= ref_z]
      z = z[z <= ref_z]
    }
  }
  if(length(na.omit(z)) < 5 | length(na.omit(y)) < 5 ){
    # need some actual data to calculate MLD
    warning("n < 5, too few points to calculate MLD, returning NA")
    return(as.numeric(NA))
  }

  bottom = y[match(max(z, na.rm = T), z)] # y at max z
  top = y[match(min(z, na.rm = T), z)] # y at min z

  if(surface == T){
    # done this way as some dips have min y @ surface
    if(abs(top - bottom) > threshold){ # is there strat?
      index = min(which(abs(y - top) > threshold))
      if(band){
        return(list("upper" = z[index-1], "lower" = z[index]))
      }else{
        return(z[index])
      }
    }else{
      return(max(z, na.rm = T)) # fully mixed
    }
  }
  if(surface == F){
    # done this way as some dips have min y @ surface
    if(abs(top - bottom) > threshold){ # is there strat?
      index = max(which(abs(y - bottom) > threshold))
      if(band){
        return(list("upper" = z[index], "lower" = z[index+1]))
      }else{
        return(z[index])
      }
    }else{
      return(max(z, na.rm = T)) # fully mixed
    }
  }
}


#' yday with decimal time
#'
#' @description converts POSIXct to decimal day of year with decimal time.
#'  or optionally, decimal day since start
#' @details The first day of the year is treated as zero, i.e. midday on Jan 1st will return 0.5.
#' This differs from the value provided by the `yday` lubridate  function
#'
#' @param x as POSIXct vector
#' @param from either "year", "start" or provide a "YYYY-mm-dd" date to start from. "year is default"
#'
#' @return numeric vector of day of the year with decimal time
#' @importFrom lubridate is.POSIXct
#' @export
ydaytime <- function(x, from = "year"){
  if(!lubridate::is.POSIXct(x)){stop("input is not valid POSIXct!")}
  n = as.numeric(x)
  if(from == "year"){
    yr = lubridate::year(x)
    origin = as.numeric(as.POSIXct(paste0(yr,"-01-01"), format = "%Y-%m-%d", tz = "UTC"))
  }
  else if(from == "start"){
    origin = as.numeric(min(x))
  }
  else{
    origin = as.numeric(as.POSIXct(from, tz="UTC", format = "%Y-%m-%d"))
  }
  r = (n - origin) / (60 * 60 * 24)
  return(r)
}


#' Fast fuzzy matcher
#'
#' @details finded closest index in `reference`.
#'
#' Matches values from a reference data.table to another. Typically used for fuzzy dateTime matching.
#' note, make sure the index column is the same data type in both dat and reference. e.g. as.numeric
#'
#' @param dat data.table of variables you want to look up, typically contain a POSIXct "dateTime" index column.
#' @param reference  data.table of variable you want to search against, must contain the same named index column.
#' @param index  column name in `reference` you want to match against (typically "dateTime")
#' @param threshold integer value for maximum permissible gap between lookup value, unit = seconds for "dateTime".
#' @param return if false (default) values with no match, or outside threshold are dropped
#' @return data.table with added variable column and index from `reference`
#'
#' @examples
#'dat = data.table(i = c(4.3, 5.9, 1.2), datval = runif(3)+10, datstuff="test")
#'reference = data.table(i = as.numeric(1:10), refjunk = "junk", refval = runif(10))
#'fuzzymatch(dat, reference, index="i")
#'
#' @export
fuzzymatch <- function(dat, reference, index="dateTime", threshold = Inf, return = F){
  reference[, (paste0("ref_", index)) := get(index)] # add duplicate reference column
  out = reference[dat, roll="nearest", on=index] # match nearest
  if(return){
    out = out[abs(as.numeric(get(paste0("ref_", index))) - as.numeric(get(index)) ) < threshold, within_threshold := T]
  }else{
    out = out[abs(as.numeric(get(paste0("ref_", index))) - as.numeric(get(index)) ) < threshold] # exclude values outside threshold
  }
  return(copy(out))
}

update_telid <- function(){
  query = "SELECT [TelemetrySensorType] ,[TelemetrySensorDescr] FROM [SmartBuoy].[dbo].[TelemetrySensorType] ORDER BY [TelemetrySensorType]"
  sb = RODBC::odbcConnect("smartbuoydblive")
  telids = data.table(RODBC::sqlQuery(sb, query))
  RODBC::odbcCloseAll()
  colnames(telids) = c("telid", "sensor_parameter")
  telids[, telid := as.character(telid)]
  save(telids, file = "data/telids.rdata")
}


#' Telemetry id lookup function
#'
#' @param x a vector of telemetry ids
#'
#' @return a vector of matching sensor parameter names
#' @export
smartbuoy.telid <- function(x){
  # if(!is.na(telids)){ }
  x = as.character(x)
  data("telids")
  return(telids[telid %in% x]$sensor_parameter)
}



#' ggplot biwavelet
#'
#' Plots a biwavelet wt object with sensible defaults
#'
#' @param wt a wavelet object created with biwavelet::wt
#' @param base log scale to use
#' @param colors either "viridis" (default), "inferno" or "jet"
#' @param isPOSIXct if True (default), assume time is in seconds
#' @param yscale if ifPOSIXct is True set this to "hours" (default), "seconds" or "days"
#' @param ylim optional, two element vector containing limits of y in units of yscale
#'
#' @return ggplot
#'
ggwavelet <- function(wt, base = 2, colors = "viridis", isPOSIXct = T, yscale = "hours", ylim = NA){

  if(colors == "viridis"){
    grad_scale = scale_fill_viridis_c()
    }
  if(colors == "inferno"){
    grad_scale = scale_fill_viridis_c(option="A")
    }
  if(colors == "jet"){
    fill.cols = c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
    grad_scale =  scale_fill_gradientn(colors = fill.cols)
    }

  reverselog_trans <- function(base = exp(1)) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    trans_new(paste0("reverselog-", format(base)), trans, inv,
              log_breaks(base = base),
              domain = c(1e-100, Inf))
  }

  time = wt$t
  if(isPOSIXct){time = as.POSIXct(time, origin = "1970-01-01", tz = "UTC")}
  period = wt$period
  power = melt(wt$power.corr, varnames = c("period", "time"), value.name = "power")
  wave = melt(wt$wave, varnames = c("period", "time"), value.name = "wave")
  signif = melt(wt$signif, varnames = c("period", "time"), value.name = "signif")
  x = merge(power, wave, by = c("time", "period"))
  x = merge(x, signif, by = c("time", "period"))
  # coi = data.frame(x = c(time, rev(time)), y = c(wt$coi, rep(max(wt$coi, na.rm = TRUE), length(wt$coi))))
  coi = data.frame(x = time, y = wt$coi)
  coi$y[coi$y < min(period)] = 0
  # coi = subset(coi, y > min(period))
  if(isPOSIXct){
    if(yscale == "hours"){
      period = period / (60*60)
      coi$y = coi$y / (60*60)
      }
    if(yscale == "days"){
      period = period / (60*60*24)
      coi$y = coi$y / (60*60*24)
      }
  }
  x$time = time[x$time]
  x$period = period[x$period]
  p1 =  ggplot(x) +
    geom_raster(aes(time, period, fill = power)) +
    scale_y_continuous(trans = reverselog_trans(base), limits = ylim) +
    grad_scale +
    geom_contour(aes(time, period, z = signif), color = "black", breaks = 1, size = 1) +
    geom_line(data = coi, aes(x = x, y = y), color = "white") +
    theme_bw()
  return(p1)
}

#' uniform random numbers from percentage
#'
#' This is a wrapper for runif that calculates the upper and lower bounds based on a percentage of a given mean
#'
#' @param n number of observations
#' @param mean mean of observations
#' @param percent percentage of mean
#'
#' @return numeric vector of random deviates
#' @export
#'
#' @examples
#' rpercentunif(10, 50, 10)
rpercentunif = function(n, mean, percent){
  lower = mean - ((mean/100) * percent)
  upper = mean + ((mean/100) * percent)
  return(runif(n, lower, upper))
}


#' quantile uniform random numbers from percentage
#'
#' This is a wrapper for qunif that calculates the upper and lower bounds based on a percentage of a given mean
#'
#' @param p a vector of probabilies
#' @param mean mean of observations
#' @param percent percentage of mean
#'
#' @return numeric vector of random deviates
#' @export
#'
#' @examples
#' rpercentunif(10, 50, 10)
qpercentunif = function(p, mean, percent){
  lower = mean - ((mean/100) * percent)
  upper = mean + ((mean/100) * percent)
  return(qunif(p, lower, upper))
}

#' lagged flag
#'
#' function designed to flag data based on specified start times
#' This is mostly used for flagging data for x minutes after an event
#'
#' @param dateTime a vector of times which are to be flagged, this does not need to be POSIXct
#' @param init POSIXct times of event
#' @param lag integer period to add to times, should be in the same units i.e. seconds for POSIXct
#'
#' @return logical vector of same length as dateTime, True denotes flagged
#' @export
#'
#' @examples
#'
#' wash_times = as.POSIXct("2017-01-01 11:05:00")
#' # NOT RUN
#' # fb[, flag := lagged_flag(dateTime, wash_times, 600)] # flag for 10 minutes after wash
lagged_flag <- function(dateTime, init, lag = 300){
  init = data.table("start" = unique(init))
  init[, end := init + lag]
  init[, flag := T]
  dateTime = data.table("dateTime" = dateTime, "dt2" = dateTime)
  setkey(dateTime, dateTime, dt2) # set keys for foverlaps
  setkey(init, start, end)
  # flag = foverlaps(dateTime, init, type="within")$flag
  flag = foverlaps(dateTime, init, type="within", mult="first")
  if(!all(dateTime$dateTime == flag$dateTime)){
    stop("")
  }
  return(flag$flag)
}

#' Parse a NMEA GGA GPS file
#'
#' Tool to parse a text log containing GGA NMEA strings
#' Note that GGA has a timestamp but not a date, so the date origin needs to be supplied
#'
#' Tool returns gpsTime (UTC), lat and lon in decimal degrees, number of satelites.
#' HDOP and altitude in meters.
#'
#' @param file
#' @param date_origin default "2000-01-01"
#'
#' @return data.table with processed positions
#' @export
#'
#' @examples
#'  gps = read.NMEA_GGA("teraterm.log", date_origin = "2021-10-02")
read.NMEA_GGA <- function(file, date_origin = "2000-01-01"){
  ln = readLines(file)
  ln = ln[grepl("GPGGA", ln)]
  GGA = stringr::str_extract_all(ln, "\\$..GGA[\\w\\d,.\\*]+")
  GGA = data.table(stringr::str_split_fixed(GGA, ",", 15))
  GGA[, gpsTime := as.POSIXct(paste(date_origin, V2), format = "%Y-%m-%d %H%M%S", origin = , tz = "UTC")]
  GGA[, lat := convert_latlong(stringr::str_sub(V3,1,2), stringr::str_sub(V3, -8, -1), polarity = V4)]
  GGA[, lon := convert_latlong(stringr::str_sub(V5,1,3), stringr::str_sub(V5, -8, -1), polarity = V6)]
  GGA[, fix := V7]
  GGA[, sat := V8]
  GGA[, HDOP := V9]
  GGA[, alt := V10]
  GGA = GGA[fix != 0 & sat != "0"]
  return(GGA[,.(gpsTime, lat, lon, sat, HDOP, alt)])
}



#' Calculate relative humidity from air temperature and dewpoint
#'
#' @param t2m air temperature
#' @param d2m dew point temperature
#' @param unit "K" or "C" (default)
#'
#' @return relative humidity (%)
#' @export
#'
RH_from_dewtemp <- function(t2m, d2m, unit="C"){
  RH = 100 * (saturation_vapour_pressure(d2m) / saturation_vapour_pressure(t2m))
  return(RH)
}

#' Calculate water vapour saturation pressure
#'
#' @param TEMP vector of temperature
#' @param unit "C" default or "K"
#' @param method options are "Wagner", "Buck" and "Weiss", default ("Weiss")
#'
#' @return saturation vapour pressure in hPa (mBar)
#' @export
#'
saturation_vapour_pressure <- function(TEMP, unit="C", method="Weiss"){
  if(unit == "C"){
    KTEMP = TEMP + 273.15
  }else{
    KTEMP = TEMP
    TEMP = KTEMP - 273.15
  }
  if(method == "Wagner"){
    # IAPWS Formulation 1995 for the Thermodynamic Properties of Ordinary Water Substance for General and Scientific Use ",
    # Journal of Physical and Chemical Reference Data, June 2002 ,Volume 31, Issue 2, pp. 387535):
    Tc = 647.096 # critical temperature [K]
    Pc = 220640 # critical pressure [hPa]
    C1 = -7.85951783
    C2 = 1.84408259
    C3 = -11.7866497
    C4 = 22.6807411
    C5 = -15.9618719
    C6 = 1.80122502
    v = 1 - (KTEMP / Tc)
    Cx = (C1 * v + C2 * v^1.5 + C3 * v^3 + C4 * v^3.5 + C5 * v^4 + C6 * v^7.5)
    return(exp((Tc / KTEMP) * Cx) * Pc) # hPa
  }
  if(method == "Buck"){
    # Buck 1996 equation
    return(6.1121 * exp((18.678 - (TEMP / 234.5)) * (TEMP / (257.14 + TEMP)))) # hPa/mbar
  }
  if(method == "Weiss")
    # weiss1980 [mbar]
    return(
      (exp(24.4543 - 67.4509 * (100/(273.15 + TEMP)) - 4.8489 * log((273.15 + TEMP)/100) - 0.000544 * 0)) * 1013.25
    )
}

#' ggplot lm
#'
#' quickly plot a lm like everyone wants from excel
#'
#' @param fit a lm fit object
#'
#' @return ggplot2 dot plot with equation at top
#'
ggplot.lm <- function (fit) {
plt = ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")

plt + labs(caption = paste0(
                           "y = ",
                           signif(fit$coef[[1]], 4),
                           " + ",
                           signif(fit$coef[[2]]),
                           "x",
                           ", ",
                           "Adj R2 = ",
                           signif(summary(fit)$adj.r.squared, 5)
                           )
           )
}

scale_colour_datetimen <- function(..., colours, values = NULL, space = "Lab",
                                   na.value = "grey50", guide = "colourbar", colors){
  colours <- if (missing(colours))
    colors
  else colours
  ggplot2:::datetime_scale("colour", "time", palette = scales::gradient_n_pal(colours, values, space),
                           na.value = na.value, guide = guide, ...)
}

#' mark up and down casts
#'
#' finds the deepest point of pressure record
#'
#' @param pressure a order vector of pressures, any unit
#'
#' @return vector of same length as pressure, with -1 for descending and 1 for ascending (after deepest point)
#' @export
dircast <- function(pressure){
  max_prs_index = min(which(pressure == max(pressure, na.rm=T)))
  out = rep(-1, length(pressure))
  out[max_prs_index:length(pressure)] = 1
  return(out)
}

#' First order differentiation
#'
#' This is an implementation of Sunke Schmidtko's method for a first order differentiation of a vector.
#' Unlike the standard R diff function this returns a vector of the same length, coping with end effects.
#' If time is supplied to the optional "t" variable then the value returned will be dx/dt.
#' Otherwise it will just be dx.
#'
#' @param x vector of values to differentiate
#' @param t optional time in seconds, or POSIXct
#'
#' @return
#' @export
fdiff <- function(x, t = NA){
  # this is Sunke's method for first order differentiation with an vector of same length
  dx = diff(x)
  Xx = cbind(c(NA, dx), c(dx, NA))
  dx = apply(Xx, 1, mean, na.rm=T)
  if(!all(is.na(t))){
    dt = diff(as.numeric(t))
    Xt = cbind(c(NA, dt), c(dt, NA))
    dt = apply(Xt, 1, mean, na.rm=T)
    return(dx / dt)
  }else{
    return(dx)
  }
}

# replacement for reshape2::melt for netcdf arrays
melt_dt_array <- function(x){
  # tested and is 3x faster than data.table(reshape2::melt(x))
  dimnames(x) <- list(NULL, 1:ncol(x))
  x = as.data.table(x)
  x[ , row := 1:.N]
  x = melt.data.table(x, id.vars = "row", variable.name = "col")
  x[ , col := as.integer(col)]
  return(x)
}

#' Tidier for Stan timeseries
#'
#' Takes a stanfit object, calculates median, and 95 % quantiles, includes Rhat and number of effective samples and includes a column for the observation number,
#' very useful for time-series data with large parameter vectors.
#'
#'
#' @param fit a stanfit object
#'
#' @return tidied data.table
#' @export
#'
tidyMCMCts <- function(fit){
  stan <- inherits(fit, "stanfit")
  ss <- if(stan){
    as.matrix(fit)
  }
  else{stop("not a stan fit")}
  m = apply(ss, 2, median)
  summ = rstan::summary(fit, probs=NULL)$summary[,c("Rhat", "n_eff"), drop = F]
  ret = data.table(term = stringr::str_extract(names(m), "[\\w\\_]+"),
                   obs = as.numeric(stringr::str_extract(names(m), "\\d+")),
                   estimate = m,
                   std.error = apply(ss, 2, sd),
                   conf.low = apply(ss, 2, quantile, 0.05),
                   conf.high = apply(ss, 2, quantile, 0.95),
                   Rhat = summ[,"Rhat"],
                   n_eff = round(summ[,"n_eff"])
  )
}

scale_colour_datetimen <- function (..., colours, values = NULL, space = "Lab", na.value = "grey50",
                                    guide = "colourbar", aesthetics = "colour"){
  # fix for broken ggplot2 datetime colour scales
  ggplot2:::datetime_scale("colour", "time",
                           palette = scales::gradient_n_pal(colours, values, space),
                           na.value = na.value, guide = guide, ...)
}

#' rescale axis for use with ggplot2 sec_axis
#'
#' rescales variables to use with ggplot2 when using secondary axis,
#' perfect for a CTD profile.
#'
#' @param x vector of variable to be scaled
#' @param y vector of variable to scale against
#'
#' @return rescaled vector
#' @export
#'
#' @examples
#'library(oce)
#'data("ctd") # example CTD profile from oce
#'
#'x = as.data.frame(ctd@data)
#'
#'ggplot(x) +
#'  geom_path(aes(salinity, pressure, color = "Salinity")) +
#'  geom_path(aes(sec_axis_rescale(temperature, salinity), pressure, color = "Temperature")) +
#'  scale_x_continuous("Salinity", sec.axis = sec_axis(bquote(Temperature~(degree*C)),
#'                                                     trans = ~ sec_axis_rescale(., x$temperature))) +
#'  scale_y_reverse("Pressure (dbar)")
sec_axis_rescale <- function(x, y){
  x_r = range(x, na.rm = T)
  y_r = range(y, na.rm = T)
  y_r[1] + (y_r[2] - y_r[1]) * (x - x_r[1]) / (x_r[2] - x_r[1])
}
