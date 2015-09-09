
# McNeil2014 optode calibration equation

# 7 coefs C0 to c6
# t = temp
# Pr = raw phase shift (TCPhase)

#' Optode ESM2 oxygen parsing correction
#'
#' @param O2SAT miss-parsed TEMP
#' @param O2CONC miss-parsed O2
#' @param depth depth of sensor in meters
#' @param optode_salinity internal optode setting default is 0
#'
#' @return O2CONC oxygen concentration in mmol m-3, (umol / l) or ml/l
#' @export
optode.fixer <- function(O2SAT, O2CONC, salinity = 0){
    # when optode not configured correctly O2CONC is actually o2 sat with o2conc cal applied, and o2sat is actually temp
    TEMP = O2SAT
    O2SAT = O2CONC / 0.0319988 # reverse database calibration

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
    # O2CONC = (O2CONC/31.9988) * 1000 # mg/l to mmol m-3

    return(O2CONC)
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

coefs = data.frame(batch = 3606,
                         coef = c(0, 1, 2, 3),
                         C0 = c(4.602618E+03,	-1.563518E+02,	3.110023E+00,	-2.632892E-02),
                         C1 = c(-2.565486E+02,	7.841255E+00,	-1.556604E-01,	1.323442E-03),
                         C2 = c(5.797144E+00,	-1.582655E-01,	3.175702E-03,	-2.714864E-05),
                         C3 = c(-6.109157E-02,	1.486597E-03,	-3.058298E-05,	2.621733E-07),
                         C4 = c(2.464531E-04,	-5.324218E-06,	1.139455E-07,	-9.730743E-10))

optode.phaseCalc <- function(DPhase, Temp, coefs){
  # for mkl optodes 3830 & 3835
    with(coefs, {
      print(paste("using foil batch coefs", batch[1]))
      (C0[1]+C0[2]*Temp+C0[3]*Temp^2+C0[4]*Temp^3) +
      (C1[1]+C1[2]*Temp+C1[3]*Temp^2+C1[4]*Temp^3) *
      DPhase+(C2[1]+C2[2]*Temp+C2[3]*Temp^2+C2[4]*Temp^3) *
      DPhase^2+(C3[1]+C3[2]*Temp+C3[3]*Temp^2+C3[4]*Temp^3) *
      DPhase^3+(C4[1]+C4[2]*Temp+C4[3]*Temp^2+C4[4]*Temp^3) *
      DPhase^4
    })
}

coefs2 = data.frame(batch = "4807E",
                   coef = 0:6,
                   SVU = c(3.1214451E-03,	1.3333315E-04,	2.5103003E-06,	2.3471194E+02,	-2.2094030E-01,	-5.0761027E+01,	4.6214941E+00),
                   CC = c(-1.7682690E-01,	1.0473560E+00, NA, NA, NA, NA, NA)
                   )

optode.sternvolmer <- function(CalPhase, Temp, coefs){
  with(coefs, {
  print(paste("using foil batch coefs", batch[1]))
    (((SVU[4] + SVU[5] * Temp) / (SVU[6] + SVU[7] * CalPhase) -1) / (SVU[1] + SVU[2] * Temp + SVU[3] * Temp^2)) * CC[2] + CC[1]
  })
}



