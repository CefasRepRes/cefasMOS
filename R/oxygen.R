
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

#' Calculate RINKO temperature from voltage
#'
#' @param V measured RINKO output in volts
#' @param tC list of named coef, defaults to those of #0263 ARO-CAV
#'
#' @return vector of temperatures
#' @export
rinko.temp <- function(V, tC = list(A = -5.326887e+00, B = +1.663288e+01, C = -2.123968e+00, D = +4.543014e-01)){
    # RINKO III defaults to #0263 ARO-CAV

  temp = tC$A + tC$B * V + tC$C * V^2 + tC$D * V^3
  return(temp)
}


#' Calculate RINKO oxygen from voltage
#'
#' @param V output oxygen voltage
#' @param t temperature from rinko_temp
#' @param oC list of named calibration coefs A-F, defaults to #0263 ARO-CAV
#' @param p in-situ pressure in dBar, default to atmospheric (10.1325)
#' @param G alpha calibration coef
#' @param H beta calibration coef
#'
#' @return vector of RINKO oxygen in mmol m-3
#' @export
rinko.o2 <- function(V, t, S, oC = list(A = -4.234162e+01,
                                     B = +1.276475e+02,
                                     C = -3.677435e-01,
                                     D = +1.137000e-02,
                                     E = +4.600000e-03,
                                     F = +7.570000e-05),
                     p = 10.1325, G = 0, H = 1){

  # V = output voltage
  # t = tempeture from rinko_temp
  # p = in-situ pressure in decibar
  # G & H = RINKO calibration coefs (alpha and beta)

    # RINKO III #0263 ARO-CAV

  P1 = oC$A / (1 + oC$D * (t - 25) + oC$F * (t - 25)^2)
  P2 = oC$B / (V * (1 + oC$D * (t - 25) + oC$F * (t - 25)^2) + oC$C)
  P = P1 + P2 # P is DO in %

    # G and H are calibration coefs
  DO = G + H * P
    # pressure correction
  d = p * 0.01 # convert from decibar to MPa
  DO = DO * (1 + oC$E * d) # DO = %

  # from garcia and gordon
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
    Ts = log((298.15-t)/(273.15+t))

    Cstar = exp(A0+(A1*Ts)+(A2*Ts^2)+
                    (A3*Ts^3)+(A4*Ts^4)+(A5*Ts^5)+
                    S*(B0+(B1*Ts)+(B2*Ts^2)+(B3*Ts^3))+
                    (C0*S^2))

    DO = ((Cstar * 44.614 * DO) / 100) * 0.0319988 # mg/l
    DO = (DO/31.9988) * 1000 # mg/l to mmol m-3

  return(DO)
}

#' Equilibrium Oxygen saturation concentration
#'
#' Calculates oxygen saturation concentration in equilibrium with the atmosphere
#' as per Garcia & Gordon, 1992 (Benson & Kraus data)
#' @param temp numeric vector of water temperature in degrees Celsius
#' @param salinity numeric vector of salinity (PSU)
#' @return vector of saturation concentration in mmol m-3
#' @keywords oxygen
#' @examples
#' Csat(10, 35)  # saturation concentration at 10 degrees and 35 salinity
#' @export
oxygen.sat <- function(temp, salinity){
  # Coefficents
  A0 = 2.00907
  A1 = 3.22014
  A2 = 4.05010
  A3 = 4.94457
  A4 = -0.256847
  A5 = 3.88767
  B0 = -0.00624523
  B1 = -0.00737614
  B2 = -0.0103410
  B3 = -0.00817083
  C0 = -4.88682E-07

  Ts = log((298.15-temp)/(273.15+temp))

  O2.sat = A0+(A1*Ts)+(A2*Ts^2)+
    (A3*Ts^3)+(A4*Ts^4)+(A5*Ts^5)+
    salinity*(B0+(B1*Ts)+(B2*Ts^2)+(B3*Ts^3))+
    (C0*salinity^2)

  return((exp(O2.sat))* 44.6608)     # output in uMol/L
}

#' convert oxygen partial pressure to molar oxygen concentration
#' according to recommendations by SCOR WG 142 "Quality Control Procedures
#' for Oxygen and Other Biogeochemical Sensors on Floats and Gliders"
#' Henry Bittig
#'
#' @param pO2 partial pressure of oxygen in mbar
#' @param TEMP temperature
#' @param SAL salinity
#' @param PRS hydrostatic pressure in dbar (default = 0)
#'
#' @return oxygen concentration in mmol m-3
#' @export
oxygen.pp_to_conc <- function(pO2, TEMP, SAL=0, PRS = 0){
  #function O2conc=O2ptoO2c(pO2,T,S,P)
  #
  # 28.10.2015
  # 19.04.2018, v1.1, fixed typo in B2 exponent
  xO2     = 0.20946 # mole fraction of O2 in dry air (Glueckauf 1951)
  pH2Osat = 1013.25*(exp(24.4543-(67.4509*(100./(TEMP+273.15)))-(4.8489*log(((273.15+TEMP)/100)))-0.000544*SAL)) # saturated water vapor in mbar (vapour pressure, Weiss & Price, 1980)
  # pH2Osat = 6.1121 * exp((18.678 - (TEMP / 234.5)) * (TEMP / (257.14 + TEMP))) # mbar , Buck 1996 equation for over water , TEMP = air temp ?more accurate
  sca_T   = log((298.15-TEMP)/(273.15+TEMP)) # scaled temperature for use in TCorr and SCorr
  TCorr   = 44.6596*exp(2.00907+3.22014*sca_T+4.05010*sca_T^2+4.94457*sca_T^3-2.56847e-1*sca_T^4+3.88767*sca_T^5) # temperature correction part from Garcia and Gordon (1992), Benson and Krause (1984) refit mL(STP) L-1; and conversion from mL(STP) L-1 to umol L-1
  Scorr   = exp(SAL*(-6.24523e-3-7.37614e-3*sca_T-1.03410e-2*sca_T^2-8.17083e-3*sca_T^3)-4.88682e-7*SAL^2) # salinity correction part from Garcia and Gordon (1992), Benson and Krause (1984) refit ml(STP) L-1
  Vm      = 0.317 # molar volume of O2 in m3 mol-1 Pa dbar-1 (Enns et al. 1965)
  R       = 8.314 # universal gas constant in J mol-1 K-1

  pO2/(xO2*(1013.25-pH2Osat))*(TCorr*Scorr)/exp(Vm*PRS/(R*(TEMP+273.15)))
}

#' Oxygen in-air concentration
#'
#' Calculates in-air concentration of oxygen, given temperature, pressure and relative humidity.
#' If only given dewpoint will use Sargent (1980) approximation to derive relative humidity
#' Bittig2018
#'
#' @param TEMP air temperature in Celsius
#' @param SAL salinity
#' @param AIRPRS air pressure in mbar/hPa
#' @param RH relative humidity in \%
#' @param DTEMP dewpoint temperature in Celsius
#' @param return_conc if True (default) return concentration, otherwise returns partial pressure in hPa
#' @keywords oxygen
#'
#' @return oxygen concentration in mmol m-3
#' @export
oxygen.air_conc <- function(TEMP, SAL, AIRPRS, RH = NA, DTEMP = NA, return_conc=T){
  if(any(is.na(RH)) & any(is.na(DTEMP))){stop("tool requires relative humidity or dew temp")}
  if(any(is.na(RH)) & !any(is.na(DTEMP))){
    # sargent1980
    K0 = 17.9
    K1 = 0.18 # for RH > 65%
    RH = (-TEMP + K0 + DTEMP) / K1
    if(any(RH < 65)){warning("Sargent1980 only valid for RH > 65%")}
  }
  # pVap = 6.1121 * exp((18.678 - (TEMP / 234.5)) * (TEMP / (257.14 + TEMP))) # mbar , Buck 1996 equation , TEMP = air temp ?more accurate
  pVap = (exp(24.4543 - 67.4509 * (100/(273.15 + TEMP)) - 4.8489 * log((273.15 + TEMP)/100) - 0.000544 * SAL)) # weiss1970 [Atm]
  xO2     = 0.20946 # mole fraction of O2 in dry air (Glueckauf 1951)
  pO2air = xO2 * (AIRPRS - (pVap * (RH / 100))) # partial pressure of oxygen in air

  # TODO scale height of pH2O assuming log profile with roughness z0=10-4m (Subrahamanyam and Ramachandran 2003)
  # pVap_surf = (exp(24.4543 - 67.4509 * (100/(273.15 + TEMP)) - 4.8489 * log((273.15 + TEMP)/100) - 0.000544 * SAL))
  # z0 = 10E-04
  # RH_z = RH / 100
  # pVap_z = (exp(24.4543 - 67.4509 * (100/(273.15 + TEMP)) - 4.8489 * log((273.15 + TEMP)/100) - 0.000544 * SAL))
  # pVap = pVap_surf + (RH_z * pVap_z - pVap_surf) * (log(0.1 / z0) / log(10 / z0))

  if(return_conc){
    return(oxygen.pp_to_conc(pO2air, TEMP, 0, 0))
  }else{
    return(pO2air)
  }
}