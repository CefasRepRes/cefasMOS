
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
  return(O2c)
}

#' Analog optode temperature
#'
#' Calculates temperature from optode analog channel
#'
#' @param v volts
#' @param TempLimit0 optode TempLimit lower parameter (usually -5)
#' @param TempLimit1 optode TempLimit upper parameter (usually 35)
#'
#' @return vector of temperature (C)
#' @export
optode.analogtemp <- function(v, TempLimit0=-5, TempLimit1=35){
  TempLimit0 + (v / 5) * diff(c(TempLimit0, TempLimit1))
}

#' Analog optode calphase
#'
#' Calculates Calphase from optode analog channel
#'
#' @param v volts
#' @param TempLimit0 optode PhaseLimit lower parameter (usually 10)
#' @param TempLimit1 optode PhaseLimit upper parameter (usually 70)
#'
#' @return vector of temperature (C)
#' @export
optode.analogCalphase <- function(v, PhaseLimit0=10, PhaseLimit1=70){
  PhaseLimit0 + (v / 5) * diff(c(PhaseLimit0, PhaseLimit1))
}

#' Optode phase
#'
#' Uses supplied optode calibration coefficients to calculate oxygen concentration from Dphase/Calphase and temperature.
#'
#' For mk1 optodes provide Dphase
#' for mk2 without SVU multipoint calibration provide Calphase (not TCphase!)
#' remember, for optodes > #1000 two point calibrations will only affect the linear transformation coefs.
#'
#' @param DPhase vector of phase values
#' @param Temp vector of temperature (C)
#' @param coefs list (or data.frame) consisting of C0..C6 coefs, see examples
#' @param silent if True function won't print out the equation and batch numbers when called
#'
#' @return oxygen concentration in mmol m-3
#' @export
#'
#' @examples
#'
#' # for SVU multipoint calibrated optodes:
#' SVU_coef = list(batch="1517M", coef="SVU", C0=0.002757413, C1=0.000115132, C2=2.34E-06, C3=234.4436, C4=-0.3752192, C5=-45.60156, C6=4.630104)
#' optode.phaseCalc(45, 10, coefs=SVU_coef)
#'
#' # For standard 4330, 4831, 4835 optodes:
# coefs = list(batch = "4807E", coef = "mk2",
#   FoilCoefA = c(-2.988314E-06, -6.137785E-06, 1.684659E-03, -1.857173E-01, 6.784399E-04, -5.597908E-07, 1.040158E+01,
#                 -5.986907E-02, 1.360425E-04, -4.776977E-07, -3.032937E+02, 2.530496E+00, -1.267045E-02, 1.040454E-04),
#   FoilCoefB = c(-3.560390E-07, 3.816713E+03, -4.475507E+01, 4.386164E-01, -7.146342E-03, 8.906236E-05, -6.343012E-07,
#                 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00),
#   ConcCoef = c(0, 1))
# optode.phaseCalc(30, 10, coefs=coefs)
#'
#' # For standard 3835 optodes:
#' coefs = list(
#'   batch = 1707,
#'   coef = "mk1",
#'   C0 = c(5326.5, -192.117, 4.14357, -0.0378695),
#'   C1 = c(-292.068, 9.71993, -0.214295, 0.00200778),
#'   C2 = c(6.47595, -0.19808, 0.0044994, -0.0000431),
#'   C3 = c(-0.0669288, 0.00188066, -0.0000442, 0.000000428),
#'   C4 = c(0.000265042, -0.00000683, 0.000000167, -1.62E-09))
#' optode.phaseCalc(30, 10, coefs=coefs)

optode.phaseCalc <- function(phase, Temp, coefs, silent = F){
  with(coefs, {
    if(coef[1] == "SVU"){
      # For 4831 multipoint calibrated optodes
      if(!silent){ print(paste("using SVU foil batch coefs", batch)) }
      Ksv = C0 + C1*Temp + C2*Temp^2
      P0 = C3 + C4*Temp
      Pc = C5 + C6*phase # actually calphase
      DO = ((P0/Pc)-1) / Ksv
    }
    if(coef[1] == "mk1"){
      # for mkl optodes 3830 & 3835
      if(!silent){ print(paste("using mk1 foil batch coefs", batch[1])) }
      DO = (C0[1]+C0[2]*Temp+C0[3]*Temp^2+C0[4]*Temp^3) +
        (C1[1]+C1[2]*Temp+C1[3]*Temp^2+C1[4]*Temp^3) *
        phase+(C2[1]+C2[2]*Temp+C2[3]*Temp^2+C2[4]*Temp^3) *
        phase^2+(C3[1]+C3[2]*Temp+C3[3]*Temp^2+C3[4]*Temp^3) *
        phase^3+(C4[1]+C4[2]*Temp+C4[3]*Temp^2+C4[4]*Temp^3) *
        phase^4 # this is Dphase
    }
    if(coef[1] == "mk2"){
      # for mk2 optodes 4330, 4835
      if(!silent){ print(paste("using mk2 foil batch coefs", batch[1])) }
        # calculate partial pressure

      Pp =
        FoilCoefA[1]  * Temp^1 * phase^4 +
        FoilCoefA[2]  * Temp^0 * phase^5 +
        FoilCoefA[3]  * Temp^0 * phase^4 +
        FoilCoefA[4]  * Temp^0 * phase^3 +
        FoilCoefA[5]  * Temp^1 * phase^3 +
        FoilCoefA[6]  * Temp^2 * phase^3 +
        FoilCoefA[7]  * Temp^0 * phase^2 +
        FoilCoefA[8]  * Temp^1 * phase^2 +
        FoilCoefA[9]  * Temp^2 * phase^2 +
        FoilCoefA[10] * Temp^3 * phase^2 +
        FoilCoefA[11] * Temp^0 * phase^1 +
        FoilCoefA[12] * Temp^1 * phase^1 +
        FoilCoefA[13] * Temp^2 * phase^1 +
        FoilCoefA[14] * Temp^3 * phase^1 +
        FoilCoefB[1]  * Temp^4 * phase^1 +
        FoilCoefB[2]  * Temp^0 * phase^0 +
        FoilCoefB[3]  * Temp^1 * phase^0 +
        FoilCoefB[4]  * Temp^2 * phase^0 +
        FoilCoefB[5]  * Temp^3 * phase^0 +
        FoilCoefB[6]  * Temp^4 * phase^0 +
        FoilCoefB[7]  * Temp^5 * phase^0 +
        FoilCoefB[8]  * Temp^0 * phase^0 +
        FoilCoefB[9]  * Temp^0 * phase^0 +
        FoilCoefB[10] * Temp^0 * phase^0 +
        FoilCoefB[11] * Temp^0 * phase^0 +
        FoilCoefB[12] * Temp^0 * phase^0 +
        FoilCoefB[13] * Temp^0 * phase^0 +
        FoilCoefB[14] * Temp^0 * phase^0
      solub = oxygen.sat(Temp, 0) # benson kraus GG solubility
      VapP = (exp(52.57-6690.9/(Temp+273.15)-4.681*log(Temp+273.15)))
      DO = Pp * solub / (0.20946*(1013.25-VapP))
    }
  # return(list(solub, VapP, Pp, DO))
  if("ConcCoef" %in% names(coefs)){
    DO = ConcCoef[1] + DO * ConcCoef[2]
  }
  return(DO)
  })
}


#' Optode tau
#'
#' Calculates Tau as per Bittig & Korsinger 2017
#'
#' foil thickness (Im)
#' 100 µm for the Aanderaa optodes with standard foil
#' 50 µm for the Aanderaa 4330F optodes with fast response foil
#'
#' 3835, 4330 tau/s = 25 s
#' 4330F tau/s = 8 s
#' RINKO tau/s = 0.4 s (gas phase)
#'
#' Boundary layer thickness (IL) - dependent on platform
#' CTD-mounted optodes (range 20 – 50 µm), close to 40 at 1 dbar s-1
#' glider ~
#' 210 - (110 / 0.095) * vel # if vel < 0.95 dbar s-1
#' 40 + (60 / 0.905) * (1-vel) # if vel > 0.095 dbar s-1
#'
#' @param temp in-situ temperature
#' @param IL boundary layer thickness
#' @param type either "fast", "standard" or "SBE63"
#'
#' @return tau value in seconds
#' @export
#'
optode.tau <- function(temp, IL, type=c("fast", "standard", "SBE63")){
  # usethis::use_data(optode_tau)
  data("optode_tau")
  if(length(type) > 1){type = type[1]}
  selection = switch(type,
         "fast" = optode_tau$fast[J(round(temp), round(IL)), roll="nearest", on=c("temp_", "IL_")],
         "standard" = optode_tau$standard[J(round(temp), round(IL)), roll="nearest", on=c("temp_", "IL_")],
         "SBE63" = optode_tau$SBE63[J(round(temp), round(IL)), roll="nearest", on=c("temp_", "IL_")]
         )
  return(selection$tau)
}


#' Hahn optode lag correction
#'
#' Implements the Hahn optode lag correction
#'
#' @param dateTime datetime vector (seconds or POSIXct)
#' @param TCphase optode phase
#' @param temp in-situ temperature
#' @param tau default = NA
#' @param tau_DO default is c(14.8, -0.4)
#' @param k width of the median filter, default = 3
#'
#' @return lagged phase
#' @export
#'
optode.lagcorrect_hahn <- function(dateTime, TCphase, temp, tau = NA, tau_DO = c(14.8, -0.4), k = 3){
  if(is.na(tau[1])){
    tau = tau_DO[1] + tau_DO[2] * (temp - 20)
  }
    ts_hr = seq(min(dateTime), max(dateTime), by=1)
        # apply filtering/smoothing in case of spikes
    tau = runmed(tau, k)
    TCphase = runmed(TCphase, k)

        # Interpolate tau and phase to 1 Hz
    tphase_hr = approx(dateTime, runmed(TCphase, 3), xout = ts_hr)$y
    tau_hr = approx(dateTime, runmed(tau, 3), xout = ts_hr)$y
        # Pre-allocate for speed
    tphase_new = tphase_hr

        # Apply step-wise lag correction
    for(jstep in 2:length(tphase_hr)){
      tphase_new[jstep] =
                     (tphase_hr[jstep] - (tphase_hr[jstep-1] *
                     (exp(-(ts_hr[jstep]-ts_hr[jstep-1]) / tau_hr[jstep-1])))) /
                     (1-sum(exp(-(ts_hr[jstep]-ts_hr[jstep-1]) / tau_hr[jstep-1])))
    }
        # downsample back to original time-array
    tphase_new = approx(ts_hr, tphase_new, dateTime, rule = 1)$y
    return(tphase_new)
}

optode.lagcorrect_bittig <- function(dateTime, oxygen, tau){
  ts_hr = seq(min(dateTime), max(dateTime), by=1)
  C_hr = approx(dateTime, runmed(oxygen, 5), xout = ts_hr)$y
  tau_hr = rep(tau, length.out = length(C_hr))
  out_hr = rep(C_hr[1], length.out = length(C_hr))

  for(i in 2:length(C_hr)){
    b = (1 + 2 * (tau_hr[i] / 1))^-1
    a = 1 - 2*b
    out_hr[i] = (1/(2*b)) * (C_hr[i] - a * C_hr[i-1])
  }
  out = approx(ts_hr, out_hr, xout=dateTime, rule = 2)$y
  return(out)
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
#' @param p hydrostatic pressure (dbar) (default = 0)
#' @param G alpha calibration coef
#' @param H beta calibration coef
#'
#' @return vector of RINKO oxygen in mmol m-3
#' @export
rinko.o2 <- function(V, t, S, oC = list(A = -4.234162e+01, B = +1.276475e+02, C = -3.677435e-01, D = +1.137000e-02, E = +4.600000e-03, F = +7.570000e-05), p = 0, G = 0, H = 1){

  # V = output voltage
  # t = temperature from rinko_temp
  # p = in-situ pressure in decibar
  # G & H = RINKO calibration coefs (alpha and beta)

    # RINKO III #0263 ARO-CAV

  P1 = oC$A / (1 + oC$D * (t - 25) + oC$F * (t - 25)^2)
  P2 = oC$B / (V * (1 + oC$D * (t - 25) + oC$F * (t - 25)^2) + oC$C)
  P = P1 + P2 # P is DO in %

    # G and H are calibration coefs
  DO = G + H * P
    # pressure correction
  d = p * 0.01 # convert from decibar to MPa, and don't include 1 atm (Rinko manual is wrong)
  DO = DO * (1 + oC$E * d) # DO = oxygen saturation %, corrected for pressure

  # from garcia and gordon
  # Benson & Krause cm3 dm-3 coefficients (ml/l)
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
  Ts = log((298.15-t)/(273.15+t))

  Csat = exp(A0+(A1*Ts)+(A2*Ts^2)+
               (A3*Ts^3)+(A4*Ts^4)+(A5*Ts^5)+
               S*(B0+(B1*Ts)+(B2*Ts^2)+(B3*Ts^3))+
               (C0*S^2))
  Csat = Csat * 44.6596 # convert ml/l to mmol m-3 as per SCOR WG 142
  DO = Csat * (DO/100)

  return(DO)
}

#' Calculate RINKO saturation from voltage
#'
#' @param V output oxygen voltage
#' @param t temperature from rinko_temp
#' @param oC list of named calibration coefs A-F, defaults to #0263 ARO-CAV
#' @param p hydrostatic pressure in dBar, default = 0
#' @param G alpha calibration coef
#' @param H beta calibration coef
#'
#' @return vector of RINKO oxygen in \% saturation
#' @export
rinko.p <- function(V, t, S, oC = list(A = -4.234162e+01, B = +1.276475e+02, C = -3.677435e-01, D = +1.137000e-02, E = +4.600000e-03, F = +7.570000e-05, G = +0.000000e+00, H = +1.000000e+00), p = 0, G = 0, H = 1){ # G & H = RINKO calibration coefs (alpha and beta)
  P1 = oC$A / (1 + oC$D * (t - 25) + oC$F * (t - 25)^2)
  P2 = oC$B / (V * (1 + oC$D * (t - 25) + oC$F * (t - 25)^2) + oC$C)
  P = P1 + P2

  DO = G + H * P
  # pressure correction
  d = 0.101325 + (p * 0.01) # convert from decibar to MPa, and include 1 atm
  DO = DO * (1 + oC$E * d) # pressure corrected DO in %
  return(DO)
}

#' convert oxygen partial pressure to molar oxygen concentration
#'
#' @param pO2 partial pressure of oxygen in hPa
#' @param TEMP temperature
#' @param SAL salinity
#' @param PRS hydrostatic pressure in dbar (default = 0)
#'
#' @references
#' Bittig, H., Körtzinger, A., Johnson, K., Claustre, H., Emerson, S., Fennel, K., Garcia, H., Gilbert, D., Gruber, N., Kang, D.-J., Naqvi, W., Prakash, S., Riser, S., Thierry, V., Tilbrook, B., Uchida, H., Ulloa, O., Xing, X., 2018. SCOR WG 142: Quality Control Procedures for Oxygen and Other Biogeochemical Sensors on Floats and Gliders. Recommendations on the conversion between oxygen quantities for Bio-Argo floats and other autonomous sensor platforms. https://doi.org/10/ggzjj3
#'
#' @return oxygen concentration in mmol m-3
#' @examples
#' oxygen.pp_to_conc(200, 10, 35, 50) # 267.1465 mmol m-3
#' @export
oxygen.pp_to_conc <- function(pO2, TEMP, SAL=0, PRS = 0){
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

#' convert molar oxygen concentration to oxygen partial pressure
#'
#' according to recommendations by SCOR WG 142 "Quality Control Procedures
#' for Oxygen and Other Biogeochemical Sensors on Floats and Gliders"
#' Henry Bittig
#'
#' @param O2 molar concentration of oxygen in mmol m-3
#' @param TEMP temperature
#' @param SAL salinity (if correction has been applied, default = 0)
#' @param PRS hydrostatic pressure in hPa (default = 0)
#' @references Bittig, H., Körtzinger, A., Johnson, K., Claustre, H., Emerson, S., Fennel, K., Garcia, H., Gilbert, D., Gruber, N., Kang, D.-J., Naqvi, W., Prakash, S., Riser, S., Thierry, V., Tilbrook, B., Uchida, H., Ulloa, O., Xing, X., 2018. SCOR WG 142: Quality Control Procedures for Oxygen and Other Biogeochemical Sensors on Floats and Gliders. Recommendations on the conversion between oxygen quantities for Bio-Argo floats and other autonomous sensor platforms. https://doi.org/10/ggzjj3
#'
#' @return partial pressure of oxygen in hPa
#' @examples
#' oxygen.conc_to_pp(267, 10, 0, 50) # 159.7683 hPa
#' @export
oxygen.conc_to_pp <- function(O2, TEMP, SAL=0, PRS=0){

  xO2 = 0.20946 # mole fraction of O2 in dry air (Glueckauf 1951)
  pH2Osat = 1013.25 * (exp(24.4543 -(67.4509 * (100 / (TEMP + 273.15))) - (4.8489 * log(((273.15 + TEMP) / 100)))-0.000544 * SAL)) # saturated water vapor in mbar (vapour pressure, Weiss & Price, 1980)
  sca_T   = log((298.15 - TEMP)/(273.15 + TEMP)) # scaled temperature for use in TCorr and SCorr
  TCorr   = 44.6596 * exp(2.00907 + 3.22014 * sca_T + 4.05010 * sca_T^2 + 4.94457 * sca_T^3 - 2.56847e-1 * sca_T^4 + 3.88767 * sca_T^5) # temperature correction part from Garcia and Gordon (1992), Benson and Krause (1984) refit mL(STP) L-1; and conversion from mL(STP) L-1 to umol L-1
  Scorr   = exp(SAL * (-6.24523e-3-7.37614e-3 * sca_T - 1.03410e-2 * sca_T^2-8.17083e-3*sca_T^3)-4.88682e-7*SAL^2) # salinity correction part from Garcia and Gordon (1992), Benson and Krause (1984) refit ml(STP) L-1
  Vm      = 0.317 # molar volume of O2 in m3 mol-1 Pa dbar-1 (Enns et al. 1965)
  R       = 8.314 # universal gas constant in J mol-1 K-1

  pO2 = O2*(xO2*(1013.25-pH2Osat))/(TCorr*Scorr)*exp(Vm*PRS/(R*(TEMP+273.15)))
  return(pO2)
}

#' Oxygen in-air concentration
#'
#' Calculates in-air concentration of oxygen, given temperature, pressure and relative humidity.
#' If only given dewpoint will use Sargent (1980) approximation to derive relative humidity
#'
#' @param TEMP air temperature in Celsius
#' @param AIRPRS air pressure in mbar (hPa)
#' @param RH relative humidity in %
#' @param DTEMP dewpoint temperature in Celsius
#' @param return_conc if True (default) return concentration, otherwise returns partial pressure in hPa
#' @keywords oxygen
#' @references Bittig, H.C., Körtzinger, A., Neill, C., van Ooijen, E., Plant, J.N., Hahn, J., Johnson, K.S., Yang, B., Emerson, S.R., 2018. Oxygen Optode Sensors: Principle, Characterization, Calibration, and Application in the Ocean. Frontiers in Marine Science 4, 1–25. https://doi.org/10/ggc8m9
#' @examples
#' oxygen.air_conc(10, 1019.5, 80, return_con = FALSE) # partial pressure of oxygen at 10oC and 1019.5 hPa local air pressure and 80% relative humidity
#' @return oxygen concentration in mmol m-3
#' @export
oxygen.air_conc <- function(TEMP, AIRPRS, RH = NA, DTEMP = NA, return_conc=T){
  if(any(is.na(RH)) & any(is.na(DTEMP))){stop("function requires relative humidity or dew temp")}
  if(any(is.na(RH)) & !any(is.na(DTEMP))){
    RH = RH_from_dewtemp(TEMP, DTEMP)
  }
  # pVap = saturation_vapour_pressure(TEMP) # should be mbar
  SAL = 0 # right?
  pVap = 1013.25 * (exp(24.4543 -(67.4509 * (100 / (TEMP + 273.15))) - (4.8489 * log(((273.15 + TEMP) / 100)))-0.000544 * SAL)) # saturated water vapor in mbar (vapour pressure, Weiss & Price, 1980)
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

#' Equilibrium Oxygen saturation concentration
#'
#' Calculates oxygen saturation concentration (C_sat) in equilibrium with the atmosphere
#' as per Garcia & Gordon, 1992, using the Benson and Kraus data.
#'
#' This is also known as C* in some literature.
#'
#' conversions via SCOR WG 142
#'
#' check values from Garcia and Gordon
#' 10oC 35 salinity
#' 6.315 ml/l
#' 274.610 umol kg-1
#'
#' @param temp numeric vector of water temperature in degrees Celsius
#' @param salinity numeric vector of salinity (PSU)
#' @param unit "mmolm" for mmol m-3 (default), "mgl" for mg l-1 or "umolkg" for umol kg-1.
#' @param p_atm atmospheric (air) pressure in hPa (default = 1013.25)
#' @return vector of saturation concentration in mmol m-3
#' @keywords oxygen
#' @references
#' 1. Bittig, H., Körtzinger, A., Johnson, K., Claustre, H., Emerson, S., Fennel, K., Garcia, H., Gilbert, D., Gruber, N., Kang, D.-J., Naqvi, W., Prakash, S., Riser, S., Thierry, V., Tilbrook, B., Uchida, H., Ulloa, O., Xing, X., 2018. SCOR WG 142: Quality Control Procedures for Oxygen and Other Biogeochemical Sensors on Floats and Gliders. Recommendations on the conversion between oxygen quantities for Bio-Argo floats and other autonomous sensor platforms. <https://doi.org/10/ggzjj3>
#' 1. Garcia, H.E., Gordon, L.I., 1992. Oxygen solubility in seawater: Better fitting equations. Limnology and Oceanography 37, 1307–1312. <https://doi.org/10/dxf339>
#' @examples
#' oxygen.sat(10, 35)  # 282.015 mmol m-3, saturation concentration at 10 degrees and 35 salinity
#' oxygen.sat(10, 35, "mll")  # 6.314767 ml/l kg-1 saturation concentration at 10 degrees and 35 salinity
#' oxygen.sat(10, 35, "umolkg")  # 274.6095 umol kg-1 saturation concentration at 10 degrees and 35 salinity
#' oxygen.sat(20, 35, p_atm = 1020.5)  # 284.06 true saturation concentration at 20 degrees and 35 salinity in mmol m-3 when local air pressure is 1020.5 hPa
#' @export
oxygen.sat <- function(temp, salinity, unit = "mmolm", p_atm = 1013.25){

  if(unit == "umolkg"){
    # umol kg coefficents
    A0 = 5.80871;
    A1 = 3.20291;
    A2 = 4.17887;
    A3 = 5.10006;
    A4 = -9.86643e-2;
    A5 = 3.80369;
    B0 = -7.01577e-3;
    B1 = -7.70028e-3;
    B2 = -1.13864e-2;
    B3 = -9.51519e-3;
    C0 = -2.75915e-7;
  }else{
    # cm3 dm-3 coefficents (ml/l)
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
  }
    Ts = log((298.15-temp)/(273.15+temp))

    Csat = A0+(A1*Ts)+(A2*Ts^2)+
    (A3*Ts^3)+(A4*Ts^4)+(A5*Ts^5)+
    salinity*(B0+(B1*Ts)+(B2*Ts^2)+(B3*Ts^3))+
    (C0*salinity^2)

    # adjust for in-situ pressure # as per SCOR WG 142
    # using vapour pressure correction, but still always at 1 atm so ignore the exp(Vm*P...
    pH2Osat = 1013.25 * (exp(24.4543-(67.4509*(100/(temp+273.15)))-(4.8489*log(((273.15+temp)/100)))-0.000544*salinity)) # saturated water vapour in mbar
    Csat = exp(Csat) * (p_atm-pH2Osat)/(1013.25-pH2Osat)

    if(unit == "mmolm"){
      return(Csat * 44.6596)     # convert ml/l to mmol m-3  as per SCOR WG 142
    }
    if(unit == "mll"){
      return(Csat) # no conversion
    }
    if(unit == "mgl"){
      return(Csat / 0.699745)     # convert ml/l to mg/l
    }
    if(unit == "umolkg"){
      return(Csat) # no conversion
    }

    # 1 μmol O2 = .022391 ml at sea surface pressure
    # 1 mg/l = 22.391 ml/31.998 = 0.699745 ml/l

    else{
      stop("unit not recognised")
    }
}

#' Equilibrium Oxygen saturation concentration (combined fit)
#'
#' Calculates oxygen saturation concentration in equilibrium with the atmosphere
#' as per Garcia & Gordon, 1992, using the combined fit, which is not recommended.
#' This is however the formulation used on all Aanderaa optodes.
#'
#' @param temp numeric vector of water temperature in degrees Celsius
#' @param salinity numeric vector of salinity (PSU)
#' @param unit "mmolm" for mmol m-3 (default), "mgl" for mg l-1 or "umolkg" for umol kg-1.
#' @return vector of saturation concentration in mmol m-3
#' @keywords oxygen
#'
oxygen.sat.combined <- function(temp, salinity, unit = "mmolm"){

  if(unit == "umolkg"){
    # umol kg coefficents
    A0 = 5.80818
    A1 = 3.20684
    A2 = 4.11890
    A3 = 4.93845
    A4 = 1.01567
    A5 = 1.41575
    B0 = -7.01211e-3
    B1 = -7.25958e-3
    B2 = -7.93334e-3
    B3 = -5.54491e-3
    C0 = -1.32412e-7
  }else{
    # cm3 dm-3 coefficents (ml/l)
    A0 = 2.00856
    A1 = 3.22400
    A2 = 3.99063
    A3 = 4.80299
    A4 = 9.78188e-1
    A5 = 1.71069
    B0 = -6.24097e-3
    B1 = -6.93498e-3
    B2 = -6.90358e-3
    B3 = -4.29155e-3
    C0 = -3.11680e-7
  }
    Ts = log((298.15-temp)/(273.15+temp))

    Csat = A0+(A1*Ts)+(A2*Ts^2)+
    (A3*Ts^3)+(A4*Ts^4)+(A5*Ts^5)+
    salinity*(B0+(B1*Ts)+(B2*Ts^2)+(B3*Ts^3))+
    (C0*salinity^2)

    if(unit == "mmolm"){
      return(exp(Csat) * 44.6596)     # convert ml/l to mmol m-3  as per SCOR WG 142
    }
    if(unit == "mll"){
      return(exp(Csat)) # no conversion
    }
    if(unit == "mgl"){
      return(exp(Csat) / 0.699745)     # convert ml/l to mg/l
    }
    if(unit == "umolkg"){
      return(exp(Csat)) # no conversion
    }

    else{
      stop("unit not recognised")
    }
}

#' Winkler titration equation
#'
#' Calculates oxygen concentration from Winkler titation
#'
#'
#' @param Vthio volume of thiosulfate at endpoint (ml)
#' @param Vblank V1 - V2 for blank (ml)
#' @param Mthio Molarity of thiosulfate (typically 0.1)
#' @param Vbot volume of sample bottle (ml)
#' @param Tfix temperature of sample when reagents added
#' @param Tcal temperature of thiosulfate during standardisation (default = 20)
#' @param Vreag volume of reagents added to sample (default = 2ml)
#'
#' @return oxygen concentration in umol l-1 (mmol m-3)
#'
#' @examples
#' oxygen.winkler(1.2807, 0.001, 0.11801, 143.34, 11.5, 20, 2)
#'
#' @export
oxygen.winkler <- function(Vthio, Vblank, Mthio, Vbot, Tfix, Tcal = 20, Vreag = 2){
  reagent_O2 = 3.8E-5 * Vreag # mmol oxygen in 1 ml reagents
  ((Vthio - Vblank)*Mthio/4 - reagent_O2)/(Vbot*(1+0.00001*(Tfix-Tcal))-Vreag)*1000000
}
