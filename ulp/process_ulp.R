files = list.files(pattern = "*\\.000")
x = lapply(files, read.ULP000)
x = rbindlist(x)
x[, c("v1", "v2", "v3") := tstrsplit(value, ";", type.convert = T)]

  # OBS
x[channel == 0 & v1 == 3, gain := 5]
x[channel == 0 & v1 == 2, gain := 25]
x[channel == 0 & v1 == 1, gain := 100]
x[channel == 0 & v1 == 0, gain := 500]
x[channel == 0, FTU := (v2 / 1000) * 1.22 * gain]

  # FLU
x[channel == 6 & v1 == 3, gain := 1]
x[channel == 6 & v1 == 2, gain := 3]
x[channel == 6 & v1 == 1, gain := 10]
x[channel == 6 & v1 == 0, gain := 30]
x[channel == 6, FLUORS := (v2 / 1000) * 1.22 * gain]

  # FSI
CF = 69.0085
C0 = -0.122862
TF = 35.34299654
T0 = -0.826790298

x[channel == 2 & v1 == 0, vc := v2 * 0.6666666 * 0.001]
x[channel == 2 & v1 == 1, vt := v2 * 0.6666666 * 0.001]
x[channel == 2 & v1 == 2, vl := v2 * 0.6666666 * 0.001]
x[channel == 2 & v1 == 3, vh := v2 * 0.6666666 * 0.001]
x[channel == 2, cs := (CF - C0) / (na.omit(vh) - na.omit(vl)), by = index]
x[channel == 2, cz := CF - (cs * na.omit(vh)), by = index]
x[channel == 2, ts := (TF - T0) / (na.omit(vh) - na.omit(vl)), by = index]
x[channel == 2, tz := TF - (ts * na.omit(vh)), by = index]
x[channel == 2, COND := (cs * vc) + cz]
x[channel == 2, TEMP := (ts * vt) + tz]

  # PRS
prs_cal = 7.999680013 # pr006
x[channel == 3, OFFDAC := 6.6666667 * (0.001 * v1)] # convert DAC and calculate DAC offset
x[channel == 3, PRSADC := (0.001 * v2)] # convert ADC
x[channel == 3, PRS := prs_cal * (OFFDAC + PRSADC)]

  # Licor
x[channel == 5, LICOR := v1 * 0.001]
x[, PAR := PAR_from_Voltage(LICOR, 0.142651297, 3.4413)] # PAR #71

  # Optode
x[channel == 17, O2CONC := v1]
x[channel == 17, O2TEMP := v3]

dat = x[,lapply(.SD, na.omit),
        .SDcols = c("TEMP", "COND", "PRS", "FLUORS", "FTU", "O2CONC"),
        by = list(index, time, id, dip = burst)]

  # calculate salinity
dat[, SAL := SAL_from_CT(COND, TEMP, PRS)]

dat = dat[PRS > 15]
dat[, DEPTH := -1 * PRS]
m = melt(dat, id.var=c("index", "dip", "time", "id"))

ggplot(m[variable %in% c("DEPTH", "FTU", "SAL", "TEMP") & dip == 1]) +
  geom_line(aes(index, value)) +
  facet_grid(variable ~ ., scales="free_y") +
  theme_bw()

```
