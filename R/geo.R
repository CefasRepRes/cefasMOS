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
    d$active[d$dateTo > now()] = "active"

    d[platform == 1, platformName := 'SmartBuoy']
    d[platform == 4, platformName := 'Lander']
    d[platform == 8, platformName := 'Waverider']

    # shapes = c(24, 22, 21)
    # shapes = c(17, 15, 16)
    shapes = c(2, 0, 1)
    names(shapes) = c('SmartBuoy', 'Lander', 'Waverider')

    d = d[platform %in% platforms] # remove unwanted deployments
    if(deployment_group_id != 'ALL'){
        d = d[groupId %in% deployment_group_id]
    }

    if(active_only == T){
        d = d[active == 'active']
    }
    warning("FIXME Shapes not allocated correctly")

    if(style == 'gsat'){
    mp = ggmap.fetch(d$lat, d$lon, zoom_to_group)
    mp = mp +
        geom_point(data = d, aes(lon, lat, color = active, position = 'jitter', shape = platformName), size = point_size) +
        scale_color_discrete('') +
        scale_shape_manual('', values = shapes, labels = names(shapes), drop = F) +
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
#' @details TODO
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

#' GEBCO Bathimity base map
#'
#' @param lat
#' @param lon
#' @param bathy_file
#'
#' @return ggplot
#' @import ggplot2 mapdata
#' @export
#'
bathymap <- function(lat, lon, bathy_file = NA){
    # stuff
  require(mapdata)
    # ? require rgdal
    # should build bathymap which fits all data in
  if(is.na(bathy_file)){
    data("GBbathy2014")
    bathy = GBbathy2014
  }else{
    # make bathy
    bathy = raster::raster(bathy_file)
    bathy = raster::crop(bathy, raster::extent(c(xlim, ylim)))
    rtp = data.frame(raster::rasterToPoints(bathy))
    colnames(rtp) = c('lon', 'lat', 'depth')
  }
  centre.lat = mean(range(lat))
  centre.lon = mean(range(lon))
  max.range = max(diff(range(lon)), diff(range(lat)))
  xlim = c(centre.lon - (max.range / 2), centre.lon + (max.range / 2))
  ylim = c(centre.lat- (max.range / 2), centre.lat+ (max.range / 2))

  # classify
  bathy$label = raster::cut(bathy$depth, breaks = c(1, -25, -50, -100, -200, -5000),
                    labels = rev(c('<25','25-50','50-100','100-200','>200')))

  # crop
  bathy = bathy[lon > min(xlim) & lon < max(xlim) &
          lat > min(ylim) & lat < max(ylim)]
  coast = ggplot2::map_data('worldHires', xlim = xlim, ylim = ylim)

  # make geom
  bathy_raster = geom_raster(data = bathy, aes(lon, lat, fill = label))

  coast.poly <- geom_polygon(data=coast, aes(x=long, y=lat, group=group), colour= "#999999", fill="#999999", lwd=0.2)
  coast.outline <- geom_path(data=coast, aes(x=long, y=lat, group=group), colour= "#999999", lwd=0.2)

  colorNum = length(unique(bathy$label))

  mp = ggplot() + bathy_raster + coast.poly + coast.outline +
      coord_quickmap(xlim, ylim) +
      scale_fill_manual(values = rev( RColorBrewer::brewer.pal(colorNum, "Blues")), name='depth') +
      labs(x = 'Longitude', y = 'Latitude')
  return(mp)
}

#' Convert degrees + decimal minutes to decimal degrees
#'
#' @param degrees if negative will convert
#' @param decimal_minutes
#' @param polarity optional "E, W, N, S"
#'
#' @return decimal degrees
#' @export
convert_latlong <- function(degrees, decimal_minutes, polarity = NA){
  degrees = as.numeric(degrees)
  decimal_minutes = as.numeric(decimal_minutes)
  # if(is.na(degrees) | is.na(decimal_minutes)){return(NA)}
  if((min(degrees, na.rm = T) < 0) & !is.na(polarity[1])){
    stop("polarity supplied for negative decimal value")
  }
  decimal_minutes[degrees < 0 & !is.na(degrees)] = decimal_minutes[degrees < 0 & !is.na(degrees)] * -1
  decimal_degrees = (degrees + decimal_minutes/60)
  decimal_degrees[grepl("[sSwW]", polarity)] = decimal_degrees[grep("[sSwW]", polarity)] * -1 # apply polarity
  return(decimal_degrees)
}
