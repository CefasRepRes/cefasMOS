#' print the variables in a netcdf (ncdump lite)
#'
#' @param ncfile a netcdf filename
#'
#' @return variable names
#' @import ncdf4
#' @export
printvars <- function(ncfile){
  nc = nc_open(ncfile)
  print(names(nc$var))
  nc_close(nc)
}

#' Read and convert ECMWF netcdf to data.table
#'
#' @param file name of ECMWF netcdf file
#' @param convert_units if True convert to more typical units (i.e. Celcius / mbar), default is False.
#'
#' @return data.table containing all netcdf variables in wide format
#' @import ncdf4
#' @export
read.ecmwf <- function(file, convert_units = F){
  nc = nc_open(file)
  print(nc)
  dims = names(nc$dim)
  vars = names(nc$var)
  lon = nc$dim$longitude$vals
  lat = nc$dim$latitude$vals

  time_origin = ncatt_get(nc, "time", "units")$value
  if(grepl("hours", time_origin)){
    time_scale = 60*60
  }else{
    time_scale = 1
  }
  time_origin = stringr::str_extract(time_origin, "[0-9\\-]+ [0-9:]+")
  dateTime = nc$dim$time$vals # hours since 1900-01-01
  dateTime = as.POSIXct(dateTime * time_scale, origin = time_origin, tz = "UTC")
  met = list()

  for(var in vars){
    print(paste("extracting", var))
    dat = ncvar_get(nc, var, collapse_degen=F)
    v_unit = ncatt_get(nc, var, "units")
    dat = as.data.table(dat)
    var_dim_index = nc$var[[var]]$dimids + 1 # get dim indexes
    colnames(dat) = c(dims[var_dim_index], "value")
    if(v_unit$hasatt == T & convert_units == T){
      if(v_unit$value == "K"){
        print("converting Kelvin to Celcius")
        dat[, value := value - 273.15]
      }
      if(v_unit$value == "Pa"){
        print("converting Pa to hPa/mbar")
        dat[, value := value * 0.01]
      }
    }
    dat$variable = var
    dat[, longitude := lon[longitude]]
    dat[, latitude := lat[latitude]]
    dat[, time := dateTime[time]]
    met[[var]] = dat
  }
  nc_close(nc)
  met = rbindlist(met)
  met = dcast.data.table(met, time + longitude + latitude ~ variable)
  if("u10" %in% vars & "v10" %in% vars){
    print("calculating wsp and dir (m s-1 & degrees)")
    met[, wsp := sqrt(u10^2 + v10^2)] # wind speed
    met[, dir := atan2(u10, v10)] # calculate direction
    met[, dir := dir * (360/(2*pi))] # convert to 180 degrees
    met[dir < 0, dir := dir + 360] # convert to 360 degrees
  }
  return(met)
}
