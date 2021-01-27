# functions for dealing with (UEA) seaglider data

#' read seaglider logfiles, extracts GPS, altimetry, battery data, and targets
#'
#' dateTime is from GPS time
#'
#' @param glider_folder folder containing log files
#' @param echo if true print name of log file
#'
#' @return data.table containing processed logfiles
#' @import data.table
#' @export
read.seaglider_log <- function(glider_folder, echo=F){
  file_list = list.files(glider_folder, full.names = T, pattern = "\\d+\\.log")
  if(length(file_list) < 1){stop("No log files found")}
  logs = list()
  for(file in file_list){
    # file = "AE2_sg620/p6200001.log"
    if(echo){ print(file) }
    log = readLines(file)
    dive = strsplit(log[grepl("dive:", log)], ": ")[[1]][2]
    log_time = strsplit(log[grepl("start:", log)], ": ")[[1]][2]
    log_time = as.POSIXct(log_time, format="%m %d 1%y %H %M %S", tz="UTC")
    if(dive != "0" & !any(grepl("RECOV_CODE", log))){
      # extract targets
      target_line = grep("$TGT_NAME", log, fixed=T, value=F)
      if(length(target_line) == 0){
        target = NA
      }else{
        target = strsplit(log[target_line], ",")[[1]][2]
      }
      # extract pings
      ping_line = grep("ALTIM_BOTTOM_PING,", log, fixed=T, value=F)
      ping_depth = NA
      alt = NA
      if(length(ping_line) > 0){
        ping_depth = strsplit(log[max(ping_line)], ",")[[1]][2]
        alt = strsplit(log[max(ping_line)], ",")[[1]][3]
      }
      # extract gps
      gps_line = grep("$GPS2,", log, fixed=T, value=F)
      if(length(gps_line) == 0){ lat = NA;  lon = NA }
      gps = strsplit(log[max(gps_line)], ",")[[1]]
      gelement = length(gps)
      HDOP = NA
      magvar = NA
      hozerror = NA
      timetofix = NA
      if(gelement == 13){
        # better gps
        # GPS1, date, time, lat, lon, timetofix, HDOP, totaltofix, magvar, driftspd, driftdir, numsat, hozerror
        HDOP = as.numeric(gps[7])
        timetofix = as.numeric(gps[6])
        magvar = as.numeric(gps[9])
        hozerror = as.numeric(gps[13])
      }
      if(gelement == 9){
        # old gps
        # $gps1, date, time, lat, long, timetofix, HDOP, totaltime, magvar
        HDOP = as.numeric(gps[7])
        timetofix = as.numeric(gps[6])
        magvar = as.numeric(gps[9])
      }

      # end of dive position
      end_gps_line = max(grep("$GPS,", log, fixed=T, value=F))
      if(length(end_gps_line) == 0){ e_lat = NA;  e_lon = NA }
      end_gps = strsplit(log[max(end_gps_line)], ",")[[1]]

      # battery
      batt24 = strsplit(grep("\\$24V_AH", log, value=T), ",")[[1]]
      batt10 = strsplit(grep("\\$10V_AH", log, value=T), ",")[[1]]
      batt10_V = as.numeric(batt10[2])
      batt24_V = as.numeric(batt24[2])
      if(batt24_V > 16) {
          # 24v pack
        batt24_AH = 100 * (1 - as.numeric(batt24[3]) / 145);
        batt10_AH = 100 * (1 - as.numeric(batt10[3]) / 95);
      }else{
          # 15v pack
        batt24_AH = 100 * (1 - as.numeric(batt24[3]) / 310);
        batt10_AH = batt10[3];
      }

      dateTime = as.POSIXct(paste(gps[2], gps[3]), format="%d%m%y %H%M%S", tz="UTC")
      lat = as.numeric(gps[4])
      lat = sign(lat) * (as.integer(abs(lat)/100) + (abs(lat)%%100)/60)
      lon = as.numeric(gps[5])
      lon = sign(lon) * (as.integer(abs(lon)/100) + (abs(lon)%%100)/60)

      dateTime_end = as.POSIXct(paste(end_gps[2], end_gps[3]), format="%d%m%y %H%M%S", tz="UTC")
      lat_end = as.numeric(end_gps[4])
      lat_end = sign(lat_end) * (as.integer(abs(lat_end)/100) + (abs(lat_end)%%100)/60)
      lon_end = as.numeric(end_gps[5])
      lon_end = sign(lon_end) * (as.integer(abs(lon_end)/100) + (abs(lon_end)%%100)/60)

      logs[[file]] = data.frame("dive" = as.numeric(dive),
                                "target" = target,
                                "ping_depth" = as.numeric(ping_depth),
                                "alt" = as.numeric(alt),
                                HDOP, magvar, hozerror, timetofix,
                                log_time,
                                dateTime, lat, lon, lat_end, lon_end, dateTime_end, batt24_V, batt10_V, batt24_AH, batt10_AH,
                                stringsAsFactors=F)
      if(any(grepl("loiter", log))){
        logs[[file]]$loiter = T
      }
      }
    }
  logs = rbindlist(logs, fill = T)
  logs[alt == 999, alt := NA]
  logs[, h := ping_depth + alt]
  return(logs)
}

read.seaglider_toolbox_nc <- function(ncfile, variables = c("sigma0", "salinity", "oxygen", "temp")){
  # require(ncdf4)
  # require(data.table)
  variables = c("dive", "time", "direction", "pressure", "lat", "lon", variables)
  nc = nc_open(ncfile)
  if(!grepl("UEA gt_sg_", ncatt_get(nc, varid=0, attname="About")$value)){
    nc_close(nc)
    stop("wrong netCDF file")
  }
  d = list()
  for(var in variables){
    print(paste("extracting", var))
    x = ncvar_get(nc, var)
    x = melt(x)
    dims = sapply(nc$var[[var]]$dim, function(i){i$name})
    colnames(x) = c(dims, "value")
    x = as.data.table(x)
    x[, variable := var]
    d[[var]] = x
  }
  nc_close(nc)
  d = rbindlist(d)
  d = dcast.data.table(d, profile + depth ~ variable)
  d[, time := as.POSIXct((time - 719529)*86400, origin = "1970-01-01", tz = "UTC")] # convert
  return(d)
}

#' read UEA seaglider toolbox timeseries netcdf
#'
#' extracts data from UEA seaglider toolbox timeseries netcdf output
#' always includes dive, direction, pressure, lat and lon
#'
#' @param ncfile UEA glider netcdf timeseries file
#' @param variables varibles to extract as named in netcdf, e.g. c("salinity", "temp")
#'
#' @import data.table ncdf4
#' @return data.table containing extracted fields
#' @export
read.seaglider_toolbox_ts_nc <- function(ncfile, variables = c("sigma0", "salinity", "oxygen", "temp"), keep_flag = F){
  variables = c("dive", "direction", "pressure", "lat", "lon", variables)
  nc = nc_open(ncfile)
  if(!grepl("UEA gt_sg_", ncatt_get(nc, varid=0, attname="About")$value) | nc$ndims != 1){
    nc_close(nc)
    stop("wrong netCDF file")
  }
  d = list()
  time_val =  ncvar_get(nc, "time")
  time_unit = ncatt_get(nc, "time", "unit")
  for(var in variables){
    print(paste("extracting", var))
    dat = ncvar_get(nc, var)
    dat = data.table(time_val, "value" = dat)
    if(paste0(var,"_flag") %in% names(nc$var)){
      flag = ncvar_get(nc, paste0(var,"_flag"))
      if(keep_flag == T){
        d[[paste0(var, "_flag")]] = data.table(time_val, "value" = flag)
      }else{
        dat[flag != 0, value := NA]
      }
    }
    d[[var]] = dat
  }
  nc_close(nc)
  d = rbindlist(d, idcol = "variable")
  if(exists("value", time_unit)){
    if(time_unit$value == "seconds"){
      d[, time := as.POSIXct(time_val, origin = "1970-01-01", tz = "UTC")] # convert
    }
  }else{
    d[, time := as.POSIXct((time_val - 719529)*86400, origin = "1970-01-01", tz = "UTC")] # convert
  }
  d = dcast.data.table(d, time ~ variable, value.var = "value")
  return(d)
}

#' Read seaglider .eng files
#'
#' @param folder folder containing .eng
#' @param files single files if not using folder
#'
#' @return data.table containing extracted fields
#' @export
read.seaglider_eng <- function(folder=NA, files=NA){
  if(!is.na(files)){
    file_list = files
  }else{
    file_list = list.files(folder, full.names = T, pattern = "*.eng")
    if(length(file_list) < 1){stop("no .eng files found")}
  }
  d = list()
  for(file in file_list){
    lns = readLines(file)
    data_start = grep("%data", lns)
    data = fread(file, skip=data_start, header = F)
    header = readLines(file, data_start)
    header = lapply(header, function(x){ gsub("\\%.+\\:\\ ", "", x, perl = T) })
    header_columns = unlist(strsplit(header[[data_start-1]], ","))
    colnames(data) = header_columns
    data[, dive := as.numeric(header[[grep("%dive", lns)]])]
    data[, start := as.POSIXct(header[[grep("%start", lns)]], format="%m %d 1%y %H %M %S", tz="UTC")]
    d[[file]] = data
  }
  d = rbindlist(d, fill=T)
  return(d)
}

read.seaglider_basestation_binned_nc <- function(ncfile, variables = c("sigma_theta", "pressure", "salinity", "temperature")){
  # for binned basestation NC
    # require(ncdf4)
    nc = nc_open(ncfile)
    my_vars = c("dive_number", "start_time", "start_latitude", "start_longitude",
                "end_latitude", "end_longitude", variables)
    d = list()
    metadata = list()
    for(var in my_vars){
      print(paste("extracting", var))
      x = ncvar_get(nc, var)
      x = melt(x)
      dims = sapply(nc$var[[var]]$dim, function(i){i$name})
      colnames(x) = c(dims, "value")
      x = as.data.table(x)
      x[, variable := var]
      if("depth" %in% colnames(x)){
        d[[var]] = x
      }else{
        metadata[[var]] = x
      }
    }
    nc_close(nc)
    d = rbindlist(d)
    d = dcast.data.table(d, profile + depth ~ variable)
    metadata = rbindlist(metadata)
    metadata = dcast.data.table(metadata, profile ~ variable)
    d = merge(d, metadata, by="profile")
    d[, dateTime := as.POSIXct(start_time, origin="1970-01-01")]
    return(d)
}

read.seaglider_basestation_nc <- function(folder, variables = c("sigma_theta", "salinity", "temperature")){
  variables = c("sigma_theta", "salinity", "temperature")
  filelist = list.files(folder, pattern="p*.nc", full.names = T)
  out = list()
  for(f in filelist){
    d = list()
    print(f)
    nc = nc_open(f)
    time_var = ncvar_get(nc, "time")
    time_var = as.POSIXct(time_var, origin="1970-01-01", tz="UTC")
    lat_var = ncvar_get(nc, "latitude")
    lon_var = ncvar_get(nc, "longitude")
    prs_var = ncvar_get(nc, "pressure")
    dive_number = ncatt_get(nc, 0, "dive_number")$value
    for(var in variables){
      # print(paste("extracting", var))
      x = ncvar_get(nc, var)
      x = melt(x)
      colnames(x) = c("obs", "value")
      x = as.data.table(x)
      x[, c("time", "lat", "lon", "pressure") := list(time_var, lat_var, lon_var, prs_var)]
      d[[var]] = x
    }
    nc_close(nc)
    d = rbindlist(d, idcol = "var")
    d[, dive := dive_number]
    out[[f]] = d
  }
  out = rbindlist(out)
  return(out)
}

#' Read BODC ego glider files (e.g. slocum)
#'
#' Reads EGO glider time-series data
#' Currently only tested with v1.2
#'
#' @param ncfile ego netcdf file
#' @param vars character vector of variables to extract, e.g. c("DOXY", "PRES")
#'
#' @import ncdf4
#' @return data.table containing extracted fields
#' @export
read.ego <- function(ncfile, vars = c("DOXY", "PRES", "TEMP", "SAL", "CNDC", "CHLA", "BBP700")){
  nc = nc_open(ncfile)
  nc_var_names = names(nc$var)

  EGO_version = ncatt_get(nc, 0, "format_version")
  EGO_type = ncatt_get(nc, 0, "data_Type")
  if(!(EGO_version$value == 1.2) & EGO_type$value == "EGO glider time-series data"){
    stop("invalid netCDF, not EGO timeseries version 1.2")
  }

    # check variables are present
  if(!"TIME" %in% names(nc$dim)){
    stop("TIME variable missing from netcdf, unable to continue")
  }
  if(!all(c("LATITUDE", "LONGITUDE") %in% nc_var_names)){
    stop("LATITUDE or LONGITUDE variable missing from netcdf, unable to continue")
  }
  if(!all(vars %in% nc_var_names)){
    warning(paste(paste(vars[!vars %in% nc_var_names], collapse = ", "),
                  "variable(s) not found in netcdf, skipping\n"))
    vars = vars[vars %in% nc_var_names]
  }

    # phase?
  if("PHASE" %in% nc_var_names){
    vars = c(vars, "PHASE")
  }

  time_var = ncvar_get(nc, "TIME")
  time_var = as.POSIXct(time_var, origin = "1970-01-01", tz="UTC")
  lat = as.vector(ncvar_get(nc, "LATITUDE"))
  lon = as.vector(ncvar_get(nc, "LONGITUDE"))
  d = list()
  metadata = list()
  for(var in vars){
    print(paste("extracting", var))
    x = ncvar_get(nc, var, collapse_degen = T)
    unit = ncatt_get(nc, var, "units")
    x = data.table(1:length(x), x)
    dims = sapply(nc$var[[var]]$dim, function(i){i$name})
    colnames(x) = c(dims, "value")
    if(unit$hasatt){
      x[, unit := unit$value]
    }
    x[, c("lat", "lon") := list(lat, lon)]
    d[[var]] = x
  }
  out = rbindlist(d, fill=T, idcol="var")
  out[, dateTime := time_var[TIME]]
  return(out)
}

read.seaglider_calib <- function(file){
  cal = read.table(file, sep="=", comment.char="%")
  cal$V2 = gsub(";", "", cal$V2)
  nm = cal$V1
  cal = cal$V2
  names(cal) = nm
  as.list(cal)
}

#' Calculate seaglider temperature
#'
#' @param tempFreq vector of seaglider SBE temperature frequency
#' @param calib_file path to seaglider calibration.mat file
#'
#' @return vector of calibrated temperature
#' @export
seaglider.temp <- function(tempFreq, calib_file){
  cal = read.seaglider_calib(calib_file)
  tempPrelim = log(1000 / tempFreq)
  tempPrelim = (1 / (as.numeric(cal$t_g) + tempPrelim * (as.numeric(cal$t_h) + tempPrelim * (as.numeric(cal$t_i) + tempPrelim * as.numeric(cal$t_j))))) - 273.15
  return(tempPrelim)
}

#' Calculate seaglider conductivity
#'
#' Direct method, uses temperature and depth at cond sample time.
#' No correction for flow speed or thermal inertia applied
#'
#' @param condFreq vector of seaglider SBE temperature frequency
#' @param temp temperature as calculated with `seaglider.temp`
#' @param pressure in dbar as calculated with `seaglider.pressure`
#' @param calib_file path to seaglider calibration.mat file
#'
#' @return vector of calibrated conductivity in S/m
#' @export
seaglider.cond <- function(condFreq, temp, pressure, calib_file){
  cal = read.seaglider_calib(calib_file)
  cal = suppressWarnings(lapply(cal, as.numeric)) # convert all to numeric
  condPrelim = condFreq / 1000
  condPrelim = (cal$c_g + condPrelim * condPrelim * (cal$c_h + condPrelim * (cal$c_i + condPrelim * cal$c_j))) /
    ( 10 * ( 1.0 + cal$ctcor * temp + cal$cpcor * pressure / 100 ));
  if("sbe_cond_offset" %in% names(cal)){
    condPrelim = condPrelim + cal$sbe_cond_offset
  }
  return(condPrelim)
}


#' Calculate seaglider pressure
#'
#' calculates true pressure from .eng file "depth" variable
#'
#' @param depth vector of seaglider depth (cm)
#'
#' @return vector of calibrated pressure in dbar
#' @export
seaglider.pressure <- function(depth){
  # The glider converts pressure readings to PSI onboard using Z = PSI * 0.685.
  # So we backcalculate to PSI, then convert to dbar.
  sgdpth2psi = 1 / 0.685
  psi2bar = 1 / 14.5037738007
  depth / 100 * sgdpth2psi * psi2bar * 10
}


# seaglider.gt_sg_filter <- function(x, range_median = 2, range_lowpass = 0){
#   # as used by toolbox, compare with standard median filter
#   n = length(x)
#   m = matrix(NA_real_, n + range_median*2, range_median*2 + 1)
#   m[(range_median+1):(nrow(m)-range_median),] =  matlab::repmat(x, 1, range_median*2+1)
#   for(i in 1:(range_median*2+1)){
#     m[i:(i+n-1), i] = x
#   }
#   out = apply(m, 1, median, na.rm=T)
#   out = out[(range_median+1):(length(out)-range_median)]
#     # todo convolve low-pass
#
#   # out = conv(out,ones(1,range_lowpass*2 +1)/(range_lowpass*2 +1),'same');
#
#   return(out)
# }

seaglider.optode_boundary_layer <- function(speed){
  # technicall this is just for the slocum
  speed = abs(speed)
  IL = 210 - (110/0.095) * speed
  IL[speed > 0.095] = 40 + (60/0.905) * (1-speed[speed > 0.095])
  return(IL)
}


