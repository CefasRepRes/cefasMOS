


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
#' @import propagate
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

#' Round dateTime to x minutes
#'
#' @param x vector of POSIXct datetimes
#' @param minutes integer minutes to round to, default is 30
#'
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


#' Calculate rate of change
#'
#' Calculates rate of change given datetime and value
#'
#' @param dateTime POSIXct vector
#' @param value  numeric vector
#'
#' @return vector of rate of change in unit/second
#' @export
rate_of_change <- function(dateTime, value){
  dt = as.numeric(dateTime) - shift(as.numeric(dateTime))
  dV = value - shift(value)
  return(dV/dt)
}

#' ftu from ADC
#'
#' Calculates FTU value from ESM2 raw hex ADC counts
#'
#' @details This function querys the Smartbuoy database and returns
#' @param x vector of ESM2 hex ADC counts, each count should be 6 characters i.e. "1a13D2"
#' @param factor channel calibration factor, specific to logger, default is 1.22
#' @param offset channel calibration offset, default is 0.001
#' @return vector of calibrated FTU values
#' @keywords esm2
#' @export
esm2.FTU_from_ADC<- function(x, factor = 1.22, offset = 0.001){
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
#' @param offset channel calibration offset, default is 0.001
#' @return vector of channel corrected voltage values
#' @keywords esm2
#' @export
esm2.volts_from_ADC <- function(x, factor = 1.22, offset = 0.001){
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

ferrybox.PAR_from_ADC <- function(y){
    #       (0.5301*exp(((y-32770)/3276.7)*3.3674))/5.008332834
    return( (0.5301*exp(((y-32770)/3276.7)*3.3674))/5.008332834 )
}

fix_par <- function(x){
    ADC = ((log((x * 5.008332834) / (0.5301 * 3.3674))) * 3276.7) + 32770
    PAR = (0.5301 * exp(((ADC - 32770)/3276.7) * 3.3674)) / 5.008332834
    return(PAR)
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
#' @details returns max depth if threshold not met
#' @param depth numeric vector of depth
#' @param density vector of density
#' @param threshold numeric density threshold, default is 0.125
#' @param ref_depth reference depth for threshold, default is shallowest depth available
#' @param surface default is true, set to false for bottom mixed layer threshold (base of gradient)
#' @param band default is false, if true function returns paired vector of interval which meets threshold
#'
#' @return mld
#' @import data.table
#' @export
findMLD <- function(depth, density, threshold = 0.125, ref_depth = NA, surface = T, band=F){
  depth = depth[order(depth)]
  density = density[order(depth)]

  if(anyNA(depth)){ warning("NA's found in depth record") }
  if(anyNA(density)){ warning("NA's found in density record") }

  if(!is.na(ref_depth)){
    # Subset to ref_depth
    density = density[depth >= ref_depth]
    depth = depth[depth >= ref_depth]
  }
  if(length(depth) < 5){
    # need some actual data to calculate MLD
    return(NA)
  }

  bottom = density[match(max(depth, na.rm = T), depth)] # density at max depth
  top = density[match(min(depth, na.rm = T), depth)] # density at min depth

  if(surface == T){
    # done this way as some dips have min density !@ surface
    if(abs(top - bottom) > threshold){ # is there strat?
      if(band){
        index = min(which(abs(density - top) > threshold))
        return(list("upper" = depth[index-1], "lower" = depth[index]))
      }else{
        return(min(depth[abs(density - top) > threshold], na.rm = T))
      }
    }else{
      return(max(depth, na.rm = T)) # fully mixed
    }
  }
  if(surface == F){
    # done this way as some dips have min density !@ surface
    if(abs(top - bottom) > threshold){ # is there strat?
      if(band){
        index = max(which(abs(density - bottom) > threshold))
        return(list("upper" = depth[index], "lower" = depth[index+1]))
      }else{
        return(max(depth[abs(density - bottom) > threshold], na.rm = T))
      }
    }else{
      return(max(depth, na.rm = T)) # fully mixed
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
#' @param from either "year" or "start" see details. "year is default"
#'
#' @return numeric vector of day of the year with decimal time
#' @export
ydaytime <- function(x, from = "year"){
  if(!lubridate::is.POSIXct(x)){stop("input is not valid POSIXct!")}
  n = as.numeric(x)
  if(from == "year"){
    yr = lubridate::year(x)
    origin = as.numeric(as.POSIXct(paste0(yr,"-01-01"), format = "%Y-%m-%d", tz = "UTC"))
  }
  if(from == "start"){
    origin = as.numeric(min(x))
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
#' @import data.table
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
    out = out[abs( get(paste0("ref_", index)) - get(index) ) < threshold, within_threshold := T]
  }else{
    out = out[abs( get(paste0("ref_", index)) - get(index) ) < threshold] # exclude values outside threshold
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
  require(scales)

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
#' TODO needs testing and documention
#'
#' @param dateTime a vector of times which are to be flagged, this does not need to be POSIXct
#' @param init POSIXct times of event
#' @param lag integer period to add to times, should be in the same units i.e. seconds for POSIXct
#' @import data.table
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
#' @param option currently just "caption"
#'
#' @return ggplot2 dotplot with equation at top
#' @export
#'
ggplot.lm <- function (fit, option="caption") {
require(ggplot2)
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

#' mark up and down casts
#'
#' finds the deepest point of pressure record
#'
#' @param pressure a order vector of pressures, any unit
#'
#' @return vector of same length as pressure, with -1 for desending and 1 for assending (after deepest point)
#' @export
dircast <- function(pressure){
  max_prs_index = min(which(pressure == max(pressure, na.rm=T)))
  out = rep(-1, length(pressure))
  out[max_prs_index:length(pressure)] = 1
  return(out)
}
