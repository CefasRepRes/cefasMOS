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
#' @param lat vector of latitude coordinates for calculating map extent
#' @param lon as above for longitude
#' @param bathy_file optional bathymetry raster file
#' @param breaks if true (default) depths are binned to <25, 25-50, 50-100, 100-200 and >200m bins
#'
#' @references GEBCO data from GEBCO 2014
#' @references Coastlines from maps::worldHires CIA World Data Bank II (2003)
#'
#' @return ggplot
#' @import ggplot2 rworldmap
#' @export
#'
bathymap <- function(lat, lon, bathy_file = NA, breaks = T){
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
  centre.lat = median(range(lat, na.rm=T))
  centre.lon = median(range(lon, na.rm=T))
  max.range = max(diff(range(lon, na.rm=T)), diff(range(lat, na.rm=T)))
  xlim = c(centre.lon - (max.range / 2), centre.lon + (max.range / 2))
  ylim = c(centre.lat - (max.range / 2), centre.lat+ (max.range / 2))

  # classify
  if(breaks == T){
    bathy$label = raster::cut(bathy$depth, breaks = c(Inf, -25, -50, -100, -200, -Inf),
                      labels = rev(c('<25','25-50','50-100','100-200','>200')))
    colorNum = length(unique(bathy$label))
    bathy_scale = scale_fill_manual(values = rev( RColorBrewer::brewer.pal(colorNum, "Blues")), name='Depth')
  }else{
    bathy$label = bathy$depth*-1
    bathy$label[bathy$label < 0] = 0
    bathy_scale = scale_fill_gradient(name = "Depth", high = "#132B43", low = "#56B1F7")
  }

  # crop
  bathy = bathy[lon %between% xlim & lat %between% ylim]

  mapdata = data.table(ggplot2::fortify(rworldmap::getMap("high")))
  # subset to just regions in xlim and ylim, see http://stackoverflow.com/a/16574176
  mapdata = mapdata[mapdata[,.I[any(long %between% xlim) & any(lat %between% ylim)], by = list(group)]$V1]

  # make geom
  bathy_raster = geom_raster(data=bathy, aes(lon, lat, fill=label))
  coast.poly = geom_polygon(data=mapdata, aes(x=long, y=lat, group=group), colour="#999999", fill="#999999", lwd=0.2)
  coast.outline = geom_path(data=mapdata, aes(x=long, y=lat, group=group), colour="#000000", lwd=0.2)

  mp = ggplot() + bathy_raster + bathy_scale +
      coast.poly + coast.outline +
      coord_quickmap(xlim, ylim) +
      # cowplot::theme_cowplot() +
      labs(x = '', y = '') +
      scale_x_continuous(expand=c(0, 0)) +
      scale_y_continuous(expand=c(0, 0))

  return(mp)
  # mp + geom_point(data=x, aes(lon, lat), color = "red") +
  #   ggrepel::geom_label_repel(data=x, aes(lon, lat, label=deployment),size=1) +
  #   theme(legend.position="none")
  # ggsave("sb_mp.pdf", height=297, units="mm", dpi=400)
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
