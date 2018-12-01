#' Deployment Map
#'
#' Creates a map of MOS deployment sites
#'
#' @details 1 for SmartBuoy, 4 = Lander, 8 = Waverider
#' @param platforms optional integer vector, if supplied only these platforms will be displayed. see details. default is c(1, 4, 8)
#' @param deployment_group_id optional chararacter vector, if supplied only these deployments will be displayed.
#' @param labels boolean, if True labels are plotted using directlabels.
#' @param active_only boolean, if True only current active deployments will be displayed
#' @param zoom_to_group boolean, if True map is centred and zoomed to selected deployments, if False entire UK is used.
#' @param point_size numeric, size of plotted points
#' @param style character string denoting map style, default is 'gsat'
#' @param db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return ggmap object
#' @keywords SmartBuoy
#' @import ggplot2 data.table RODBC
#' @export
smartbuoy.map <- function(platforms = c(1, 4, 8),
                            deployment_group_id = 'ALL',
                            style = 'gsat',
                            point_size = 5,
                            labels = TRUE,
                            active_only = TRUE,
                            zoom_to_group = TRUE,
                            db_name = 'smartbuoydblive'){

    sbdb = odbcConnect(db_name)

    pos_query = paste("SELECT
                      Deployment.[DepLocLat] as lat, Deployment.[DepLocLong] as long,
                      Deployment.[DepGroupId] as groupId, Deployment.[DepDateFrom] as dateFrom,
                      Deployment.[DepDateTo] as dateTo, Deployment.[DepDescr] as description,
                      Deployment.[DepId] as dep, Platform.[PlatformId] as platformId,
                      Platform.[PlatformTypeId] as platform
                      FROM Deployment INNER JOIN Platform ON Deployment.PlatformId=Platform.PlatformId ")
    d = data.table(sqlQuery(sbdb, pos_query))
    odbcCloseAll()

    d = d[platform %in% c(1, 4, 8)] # remove non-standard deployments
    d = d[!(groupId %in% c('LOWTEST', 'ESM2TEST'))] # remove test deployments
    d = d[,list(lat = median(lat), lon = median(long), dateTo = max(dateTo), platform = platform[1]), by = groupId] # group by deployment
    d$active = "inactive"
    d$active[d$dateTo > lubridate::now()] = "active"

    d[platform == 1, c("shape", "platformName") := list(2, 'SmartBuoy')]
    d[platform == 4, c("shape", "platformName") := list(0, 'Lander')]
    d[platform == 8, c("shape", "platformName") := list(1, 'Waverider')]

    d = d[platform %in% platforms] # remove unwanted deployments
    if(deployment_group_id != 'ALL'){
        d = d[groupId %in% deployment_group_id]
    }

    if(active_only == T){
        d = d[active == 'active']
    }

    if(style == 'gsat'){
    mp = ggmap.fetch(d$lat, d$lon, zoom_to_group)
    mp = mp +
      geom_point(data = d, aes(lon, lat, color = active, shape = platformName), size = point_size) +
      scale_color_discrete('') +
      labs(x = 'Longitude', y = 'Latitude')
    }else{
        warning('style not implemented')
        mp = NA
    }

    return(list(map = mp, data = d))
}


#' fetch localised ggmap
#'
#' Creates a base map centred and scaled
#'
#' @param lat optional vector of decimal latitudes
#' @param lon optional vector of decimal longitudes
#' @param zoom_to_group boolean, if True map is centred and zoomed to input lat/long, if False entire UK is used.
#' @param scale_factor optional integer, increase set to 1 (or more) to pad to next zoom level
#' @param crop boolean, if True map is cropped to lat lon + crop_padding (default is False)
#' @param maptype string of either "satellite", "terrain", "terrain-background", "hybrid"
#' @return ggmap object
#' @keywords map
#' @export
ggmap.fetch <- function(lat, lon, zoom_to_group = T, scale_factor = 0, crop = F, maptype = "satellite"){
    ranges = data.frame(zoom = c(3, 4, 5, 6, 7, 8, 4, 2), range = c(120, 60, 30, 14, 8, 4, 2, 1))
    if(zoom_to_group == TRUE){
        centre.lat = mean(range(lat, na.rm = T))
        centre.lon = mean(range(lon, na.rm = T))
        max.range = max(diff(range(lon)), diff(range(lat)))
        # add 10% buffer
        max.range = max.range
        zoom = max(ranges$zoom[ranges$range > max.range]) - scale_factor
    }else{
        centre.lat = 53
        centre.lon = -2.7
        max.range = 14
        zoom = 6
    }
    centre = c(centre.lon, centre.lat)

    require(ggmap)
    mp = ggmap::ggmap(ggmap::get_map(location = centre, zoom = zoom, maptype = maptype))
    if(crop == TRUE){
        mp = mp + ylim(range(lat)) + xlim(range(lon))
    }
    return(mp)
}

#' GEBCO Bathymetry base map
#'
#' Simple basemap using GEBCO data, if you want something more technical try marmap.
#'
#' @param lat vector of latitude coordinates for calculating map extent
#' @param lon as above for longitude
#' @param margin integer (default = 8) indicating fraction of range to use for a margin.
#' @param breaks if true (default) depths are binned to <25, 25-50, 50-100, 100-200 and >200m bins
#'
#' @references GEBCO data from GEBCO 2014
#' @references Coastlines from rworldmap
#'
#' @return ggplot
#' @import ggplot2 rworldmap
#' @export
#'
bathymap <- function(lat = c(47, 60), lon = c(-14.996, 8.004), margin=8, breaks=T){
    # should build bathymap which fits all data in
  if(!exists("GBbathy2014")){
    data("GBbathy2014")
    print("loaded GEBCO2014")
    }
    # make new bathy?
    # GBbathy2014 = raster::raster(bathy_file)
    # GBbathy2014 = raster::crop(GBbathy2014, raster::extent(c(xlim, ylim)))
    # rtp = data.frame(raster::rasterToPoints(GBbathy2014))
    # colnames(rtp) = c('lon', 'lat', 'depth')
    # GBbathy2014$label = raster::cut(GBbathy2014$depth, breaks = c(Inf, -25, -50, -100, -200, -Inf), labels = rev(c('< 25','25-50','50-100','100-200','> 200')))
    # devtools::use_data(GBbathy2014, overwrite=T)

  max.lat = abs(min(lat) - max(lat))
  max.lon = abs(min(lon) - max(lon))
  xlim = c(min(lon) - max.lon / margin, max(lon) + max.lon / margin)
  ylim = c(min(lat) - max.lat / margin, max(lat) + max.lat / margin)

  bathy = GBbathy2014[lon %between% xlim & lat %between% ylim]

  GEBCOcolors5 = c("#0F7CAB", "#38A7BF", "#68CDD4", "#A0E8E4", "#E1FCF7")
  GEBCOcolors12 = c("#0F7CAB", "#1D8CB2", "#2C9CBA", "#3CABC1", "#4DB9C8", "#5FC6D0",
                    "#72D3D8", "#86DFDF", "#9BE6E3", "#B1EDE8", "#C8F5EF", "#E1FCF7")

  # classify
  if(breaks == T){
    colorNum = length(unique(bathy$label))
    bathy_scale = scale_fill_manual(values = GEBCOcolors5, name='Depth')
  }else{
    bathy$label = bathy$depth*-1
    bathy$label[bathy$label < 0] = 0
    bathy_scale = scale_fill_gradientn(name = "Depth (m)", colors=rev(GEBCOcolors12))
  }

  # mapdata = data.table(ggplot2::fortify(rworldmap::getMap("high")))
    # subset to just regions in xlim and ylim, see http://stackoverflow.com/a/16574176
  # mapdata = mapdata[mapdata[,.I[any(long %between% c(-50, 25)) & any(lat %between% c(45, 70))], by = list(group)]$V1]
  # devtools::use_data(mapdata, overwrite=T)
  data("mapdata") # saved for speed

  # make geom
  bathy_raster = geom_raster(data=bathy, aes(lon, lat, fill=label))
  coast.poly = geom_polygon(data=mapdata, aes(x=long, y=lat, group=group), colour="#999999", fill="#999999", lwd=0.2)
  coast.outline = geom_path(data=mapdata, aes(x=long, y=lat, group=group), colour="#000000", lwd=0.2)

  mp = ggplot() +
    bathy_raster + bathy_scale +
    coast.poly + coast.outline +
    labs(x = '', y = '') +
    scale_x_continuous(expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0)) +
    coord_quickmap(xlim, ylim)

  return(mp)
}

#' Convert degrees + decimal minutes to decimal degrees
#'
#' @param degrees numeric vector of whole degrees, if negative will convert to W or S.
#' @param decimal_minutes numeric vector of decimal minutes
#' @param polarity optional "E, W, N, S"
#'
#' @return decimal degrees
#' @export
convert_latlong <- function(degrees, decimal_minutes, polarity = NA){
  degrees = as.numeric(degrees)
  decimal_minutes = as.numeric(decimal_minutes)
  if(any(is.na(degrees)) | any(is.na(decimal_minutes))){return(NA)}
  if((min(degrees, na.rm = T) < 0) & !any(is.na(polarity))){
    stop("polarity supplied for negative decimal value")
  }
  decimal_minutes[degrees < 0 & !is.na(degrees)] = decimal_minutes[degrees < 0 & !is.na(degrees)] * -1
  decimal_degrees = (degrees + decimal_minutes/60)
  decimal_degrees[grepl("[sSwW]", polarity)] = decimal_degrees[grep("[sSwW]", polarity)] * -1 # apply polarity
  return(decimal_degrees)
}

#' Calculate bounding box
#'
#' used WGS84 elipsoid to calculate northern, eastern, southern and western extent from a starting lat/lon.
#'
#' @param lat vector of lat
#' @param lon vector of lon
#' @param size distance in meters
#'
#' @return vector of limits (north, east, south, west)
#' @export
#'
#' @examples
#' calc_bounding_box(51, 1.2, 500) # 1x1km square
calc_bounding_box <- function(lat, lon, size = 250){
  north = geosphere::destPoint(c(lon, lat), 0, size)
  east = geosphere::destPoint(c(lon, lat), 90, size)
  south = geosphere::destPoint(c(lon, lat), 180, size)
  west = geosphere::destPoint(c(lon, lat), 270, size)
  box = c(north[2], east[1], south[2], west[1])
  return(box)
}

#' Spatial-temporal matching
#'
#' builds distance matrix between all points in two data.tables using WGS84 elipsoid,
#' this matrix is then reduced to those within a specified distance threshold.
#' These spacial matched points are then matched against time within a time threshold
#'
#' @param x data.table with lat, lon and dateTime columns
#' @param y data.table with lat, lon and dateTime columns
#' @param distance_threshold distance in meters to match within
#' @param time_threshold time in seconds to match within
#' @param merge bool
#' @import geosphere
#'
#' @return if merge is true (default) returns single combined data.table of matching values, else returns list of the two subset data.tables
#' @export
match_spacetime <- function(x, y, distance_threshold = 5000, time_threshold = 3600, merge=T){
  if(!is.data.table(x) | !is.data.table(y)){stop("x and y must be data.tables")}
  if(!all(c("lat", "lon", "dateTime") %in% names(x)))stop("lat, lon and dateTime columns must be present in x")
  if(!all(c("lat", "lon", "dateTime") %in% names(y)))stop("lat, lon and dateTime columns must be present in y")
  dm = geosphere::distm(x[,.(lon, lat)], y[,.(lon, lat)])
  ind = data.table(which(dm < distance_threshold, arr.ind=T))
  ind[, rtime := x$dateTime[row]]
  ind[, ctime := y$dateTime[col]]
  ind = ind[abs(rtime - ctime) < 5]
  x = copy(x[ind$row])
  y = copy(y[ind$col])
  if(merge){
    setnames(y, c("dateTime", "lat", "lon"), c("dateTime_y", "lat_y", "lon_y"))
    out = cbind(x, y)
    out[, dist := geosphere::distGeo(cbind(lon, lat), cbind(lon_y, lat_y))]
    out[, t_dist := as.numeric(dateTime) - as.numeric(dateTime_y)]
    out
  }
  else{
    list("x" = x, "y" = y)
  }
}
