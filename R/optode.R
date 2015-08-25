
# McNeil2014 optode calibration equation

# 7 coefs C0 to c6
# t = temp
# Pr = raw phase shift (TCPhase)

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

#' Optode salinity and pressure compensation
#'
#' @param O2 oxygen concentration in mmol m-3, (umol / l) or ml/l
#' @param t temperature
#' @param S salinity
#' @param depth depth of sensor in meters
#' @param optode_salinity internal optode setting default is 0
#'
#' @return corrected oxygen concentration in units of `O2`
#' @export
#'
#' @examples
#' optode.correction(300, 7.701, 34.176, 1000)
optode.correction <- function(O2, t, S, depth = 0, optode_salinity = 0){
  # corrects optode measurements for salinity and depth
    # oxygen units returned same as input
  # Solubility and salinity comp based on Garcia and Gordon, 1992. Limno Ocean
  pCoef = 0.032 # empricial derived pressure compensation coef from Uchida et al, 2008. J Atmos Ocean Tech
  B0 = -6.24097E-03
  B1 = -6.93498E-03
  B2 = -6.90358E-03
  B3 = -4.29155E-03
  C0 = -3.11680E-07

  Ts = log((298.15-t)/(273.15+t)) # scaled temperature

  O2c = O2 * exp(S * (B0 + B1 * Ts + B2 * Ts^2 + B3 * Ts^3) + C0 * S^2) /
      exp(optode_salinity* (B0 + B1 * Ts + B2 * Ts^2 + B3 * Ts^3) + C0^2 * optode_salinity^2) *
      (1 + (pCoef * abs(depth)) / 1000)
  # sal_factor = exp((S - optode_salinity_setting) * (B0 + B1*Ts + B2*Ts^2 + B3*Ts^3) + C0 * (S^2 - optode_salinity_setting^2))
  # prs_factor = (((abs(depth))/1000)*pCoef) + 1
  return(O2c)
}


