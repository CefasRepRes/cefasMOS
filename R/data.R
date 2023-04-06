# sinew::makeOxygen(mapdata)

#' @title GEBCO 2022
#' @description GEBCO bathymetry at 0.00417 degree grid centred on the UK (45-65N -16-10E).
#' @format A data table
#' \describe{
#'   \item{\code{lon}}{WGS84 Longitude in decimal degrees}
#'   \item{\code{lat}}{WGS84 Latitude in decimal degrees}
#'   \item{\code{depth}}{depth in meters}
#'}
#' @source \url{https://www.gebco.net/}
"gebco_2022"

#' @title GEBCO 2022 Low
#' @description GEBCO bathymetry at 0.05 degree grid centred on the UK (45-65N -16-10E).
#' @format A data table
#' \describe{
#'   \item{\code{lon}}{WGS84 Longitude in decimal degrees}
#'   \item{\code{lat}}{WGS84 Latitude in decimal degrees}
#'   \item{\code{depth}}{depth in meters}
#'   \item{\code{label}}{label corisponding to depth bin}
#'}
#' @source \url{https://www.gebco.net/}
"gebco_2022_low"

#' @title cds_table
#' @description a precompiled list of all possible combinations of Cefas Endeavour ferrybox error codes
#' This is used as a lookup table
#' @format A List of 4095
#'@examples cds_table[3] # underflow and value too low
"cds_table"

#' @title telids
#' @description A table of the telemetry id's from the Cefas SmartBuoy database
#' @format A data frame with 96 rows and 3 variables:
#' \describe{
#'   \item{\code{telid}}{The Id as represented on ULEEF and the database}
#'   \item{\code{sensor_parameter}}{parameter code}
#'   \item{\code{in_fb_loop}}{Is this telid used on the Cefas Endeavour ferrybox}
#'}
"telids"

# update mapdata
# coastline = rworldmap::getMap("high")
# e = as(raster::extent(-15, 10, 45, 70), "SpatialPolygons")
# sp::proj4string(e) = sp::CRS(sp::proj4string(coastline))
# coastline = rgeos::gIntersection(coastline, e, byid=T)
# mapdata = data.table(ggplot2::fortify(coastline))[,.(id, lon = long, lat, group)]

#' @title Western European shelf coastlines
#' @description a subset from rworldmap high resolution, but just for the Western European shelf countries
#' @format data frame
#' \describe{
#'   \item{\code{lon}}{WGS84 longitude in decimal degees}
#'   \item{\code{lat}}{WGS84 latitude in decimal degees}
#'   \item{\code{id}}{country name}
#'   \item{\code{group}}{polygon group}
#'}
#' @references `rworldmap`
#' @example
# ggplot(nwe_coastline) + geom_polygon(aes(lon, lat, group = group, fill = id))
"nwe_coastline"
