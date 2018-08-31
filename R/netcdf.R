#' Read and convert ECMWF netcdf to data.table
#'
#' @param file
#'
#' @return data.table containing all netcdf variables in long format
#' @import ncdf4 data.table
#' @export
read.ecmwf <- function(file){
  nc = nc_open(file)
  print(nc)
  dims = names(nc$dim)
  vars = names(nc$var)
  lon = nc$dim$longitude$vals
  lat = nc$dim$latitude$vals
  dateTime = nc$dim$time$vals # hours since 1900-01-01
  dateTime = as.POSIXct(dateTime*60*60, origin = "1900-01-01", tz = "UTC")
  met = list()
  for(var in vars){
    print(paste("extracting", var))
    dat = ncvar_get(nc, var)
    dat = data.table(melt(dat))
    colnames(dat) = c(names(nc$dim), "value")
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
    print("calculating wsp and dir")
    met[, wsp := sqrt(u10^2 + v10^2)] # wind speed
    met[, dir := atan2(u10, v10)] # calculate direction
    met[, dir := dir * (360/(2*pi))] # convert to 180 degrees
    met[dir < 0, dir := dir + 360] # convert to 360 degrees
  }
  return(met)
}
