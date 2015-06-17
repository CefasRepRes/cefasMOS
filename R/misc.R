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


optode.fixer <- function(O2SAT, O2CONC, salinity = 0){
    # when optode not configured correctly O2CONC is actually o2 sat with o2conc cal applied, and o2sat is actually temp    
    TEMP = O2SAT
    O2SAT = O2CONC / 0.0319988
    
    A0 = 2.00856
    A1 = 3.224   
    A2 = 3.99063
    A3 = 4.80299
    A4 = 0.978188
    A5 = 1.71069
    B0 = -0.00624097
    B1 = -0.00693498
    B2 = -0.00690358
    B3 = -0.00429155
    C0 = -3.1168E-07
    Ts = log((298.15-TEMP)/(273.15+TEMP))
    
    Cstar = exp(A0+(A1*Ts)+(A2*Ts^2)+
                    (A3*Ts^3)+(A4*Ts^4)+(A5*Ts^5)+
                    salinity*(B0+(B1*Ts)+(B2*Ts^2)+(B3*Ts^3))+
                    (C0*salinity^2))
    
    O2CONC = ((Cstar * 44.614 * O2SAT) / 100) * 0.0319988 # mg/l
}

optode.salinity_correction <- function(Sal, Temp, O2, depth = 1, optode_salinity_setting = 0){
    Ts = log((298.15-Temp)/(273.15+Temp))
    corrected = (O2*(exp(Sal*(-0.00624097+-0.00693498*Ts+-0.00690358*Ts^2+-0.00429155*Ts^3)+-0.00000031168*Sal^2))/
                     (exp(optode_salinity_setting*(-0.00624097+-0.00693498*Ts+-0.00690358*Ts^2+-0.00429155*Ts^3)+-0.00000031168*optode_salinity_setting^2))
                 )*(1+(0.04*depth)/1000)
    return(corrected)
}