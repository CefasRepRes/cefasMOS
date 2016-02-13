# TODO vectorise me!
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
ftu_from_ADC<- function(x, factor = 1.22, offset = 0.001){
    which_range <- function(range){
        switch(range,
               "0" = 500,
               "1" = 100,
               "2" = 25,
               "3" = 5)
    }
    subfunc <- function(x){
        # TODO check valid ESM hex output
        # if(nchar(x) != 6){return(NA)}
        dec = as.numeric(paste0("0x", substr(x, 4, 6)))
        range = which_range(as.character(substr(x, 3, 3)))   # calculate factor from range
        dec.mv = dec / 1000 # convert to mV
        dec.c = (dec.mv * factor) - (offset / 1000)  # apply channel calibrations
        return(dec.c * range) # apply gain factor
    }
    ftu = subfunc(x)
    return(ftu)
}

flu_from_ADC<- function(x, factor = 1.22, offset = 0.001){
        # check valid ESM hex output
    # use sapply(x, flu_from_ADC)
    if(nchar(as.character(x)) != 6){return(NA)}
    dec = as.numeric(paste0("0x", substr(x, 4, 6)))

    which_range <- function(range){
        switch(range,
               "0" = 30,
               "1" = 10,
               "2" = 3,
               "3" = 1)
    }

    range = which_range(as.character(substr(x, 3, 3)))   # calculate factor from range
    dec.mv = dec / 1000 # convert to mV
    dec.c = (dec.mv * factor) - (offset / 1000)  # apply channel calibrations
    flu = dec.c * range # apply gain factor
    return(flu)
}

par_from_voltage <- function(x, factor, offset){
    return(factor * exp(offset * x))
}

ferrybox.ADC_to_PAR <- function(y){
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
#' @param Cond
#' @param t
#' @param p
#' @param P
#'
#' @return salinity
#' @export
calc_sal <- function (Cond, t, p = max(0, P - 1.013253), P = 1.013253) {
    # Adapted from marelac package by Karline Soetaert using
    #     Fofonoff NP and Millard RC Jr, 1983. Algorithms for computation of fundamental properties of
    #     seawater. UNESCO technical papers in marine science, 44, 53 pp.
    #     http://unesdoc.unesco.org/images/0005/000598/059832EB.pdf
    # cran.r-project.org/web/packages/marelac
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

# convert_RtoS((39.023/10/4.2914), t = 11.606, p = 10.86638775/10 ) # marelac version
# calc_sal(39.023, t = 11.606, p = 10.86638775)


#' Find mixed layer depth
#'
#' @param d
#' @param p
#' @param threshold
#'
#' @return mld
#' @import data.table
#' @export
findMLD <- function(d, p, threshold = 0.125){
  # simple threshold technique, mld where p +/- 0.125 of surface p

  bottom = p[d == max(d)][1] # density at max d
  top = p[d == min(d)][1]

  # done this way as some dips have min density !@ surface
  if(abs(top - bottom) > threshold){ # is there strat?
    return(min(d[abs(p - top) > threshold]))
  }else{
    return(0) # fully mixed
  }
}

#' yday with decimal time
#'
#' @description converts POSIXct to day of year with decimal time.
#' @details The first day of the year is treated as zero, i.e. midday on Jan 1st will return 0.5.
#' This differs from the value provided by the `yday` lubridate  function
#'
#' @param x as POSIXct vector
#'
#' @return numeric vector of day of the year with decimal time
#' @export
ydaytime <- function(x){
  if(!is.POSIXct(x)){stop("input is not valid POSIXct!")}
  n = as.numeric(x)
  yr = lubridate::year(x)
  o = as.numeric(as.POSIXct(paste0(yr,"-01-01"), format = "%Y-%m-%d", tz = "UTC"))
  r = (n - o) / (60 * 60 * 24)
  return(r)
}
