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
#' Basemap using GEBCO 2019 data, if you want something more technical try the marmap package.
#'
#' @param lat vector of latitude coordinates for calculating map extent
#' @param lon as above for longitude
#' @param highres default = False, if true fetch the full half degree GEBCO 2019 data
#' @param margin integer (default = 8) indicating fraction of range to use for a margin.
#' @param breaks if true (default) depths are binned to <25, 25-50, 50-100, 100-200 and >200m bins
#'
#' @references GEBCO data from GEBCO 2019
#' @references Coastlines from rworldmap
#'
#' @return ggplot
#' @import ggplot2 rworldmap cmocean
#' @export
#'
bathymap <- function(lat = c(47, 60), lon = c(-14.996, 8.004), margin=8, breaks=T, highres=F){
    # should build bathymap which fits all data in
    # gebco_2014$label = raster::cut(GBbathy2014$depth, breaks = c(Inf, -25, -50, -100, -200, -Inf), labels = rev(c('< 25','25-50','50-100','100-200','> 200')))
    # gebco_2014[, lat := round(lat, digits=6)]
    # gebco_2014[, lon := round(lon, digits=6)]
    # gebco_2014[, depth := depth * -1]
    # ggplot(gebco_2014) + geom_raster(aes(lon, lat, fill=depth)) + scale_fill_gradientn(name = "Depth [m]", colors=cmocean("deep")(256))
    # usethis::use_data(gebco_2019, overwrite=T)

  max.lat = abs(min(lat) - max(lat))
  max.lon = abs(min(lon) - max(lon))
  xlim = c(min(lon) - max.lon / margin, max(lon) + max.lon / margin)
  ylim = c(min(lat) - max.lat / margin, max(lat) + max.lat / margin)

  if(highres){
    if(!exists("gebco_2019")){data("gebco_2019");print("loaded GEBCO 2019")}
    bathy = gebco_2019[lon %between% xlim & lat %between% ylim]
  }else{
    if(!exists("GBbathy2014")){data("gebco_2014");print("loaded GEBCO 2014")}
    bathy = gebco_2014[lon %between% xlim & lat %between% ylim]
  }

  GEBCOcolors5 = rev(c("#0F7CAB", "#38A7BF", "#68CDD4", "#A0E8E4", "#E1FCF7"))

  # classify
  if(breaks == T){
    bathy_scale = scale_fill_manual(values = GEBCOcolors5, name='Depth [m]')
    bathy_raster = geom_raster(aes(lon, lat, fill=label))
  }else{
    bathy_scale = scale_fill_gradientn(name = "Depth [m]", colors=cmocean("deep")(256))
    bathy_raster = geom_raster(aes(lon, lat, fill=depth))
  }

  # mapdata = data.table(ggplot2::fortify(rworldmap::getMap("high")))
    # subset to just regions in xlim and ylim, see http://stackoverflow.com/a/16574176
  # mapdata = mapdata[mapdata[,.I[any(long %between% c(-50, 25)) & any(lat %between% c(45, 70))], by = list(group)]$V1]
  # devtools::use_data(mapdata, overwrite=T)
  data("mapdata") # saved for speed

  # make geom
  coast.poly = geom_polygon(data=mapdata, aes(x=long, y=lat, group=group), colour="#999999", fill="#999999", lwd=0.2)
  coast.outline = geom_path(data=mapdata, aes(x=long, y=lat, group=group), colour="#000000", lwd=0.2)

  mp = ggplot(bathy) +
    bathy_raster + bathy_scale +
    coast.poly + coast.outline +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0)) +
    coord_quickmap(xlim, ylim)

  # mp +  geom_contour(aes(lon, lat, z=depth), binwidth=20, color="black")
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

#' Convert decimal degrees  to degrees + decimal minutes
#'
#' @param degrees numeric vector of decimal degrees
#' @param paste if True (default) return a pasted string, otherwise returns list of DD and MM
#'
#' @return decimal degrees
#' @export
convert_latlong_ddmmm <- function(degrees, paste=T){
  dd = degrees %/% 1
  mm = round((degrees %% 1) * 60, 3)
  if(paste){
    return(paste0(dd, "'", mm))
  }else{
    return(list(dd, mm))
  }
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

#' Improved Spatial-temporal matching
#'
#' builds distance matrix between all points in two data.tables using WGS84 elipsoid,
#' this matrix is then reduced to those within a specified distance threshold.
#' These spacial matched points are then matched against time within a time threshold
#'
#' @param A data.table with lat, lon and dateTime columns
#' @param Z data.table with lat, lon and dateTime columns
#' @param dt distance in meters to match within
#' @param tt time in seconds to match within
#' @param merge bool
#' @import geosphere scales data.table
#'
#' @return if merge is true (default) returns single combined data.table of matching values, else returns list of the two subset data.tables
#' @export
fuzzy_spacetime <- function(A, Z, tt=3600, dt=5000){

  A_name = deparse(substitute(A)) # get the names of the objects for labling later
  Z_name = deparse(substitute(Z))
  if(!is.data.table(A) | !is.data.table(Z)){stop("x and y must be data.tables")}
  if(!all(c("lat", "lon", "dateTime") %in% names(A)))stop("lat, lon and dateTime columns must be present in A")
  if(!all(c("lat", "lon", "dateTime") %in% names(Z)))stop("lat, lon and dateTime columns must be present in Z")

  d_lon = distHaversine(c(mean(Z$lon), min(Z$lat)), c(mean(Z$lon), max(Z$lat))) / (max(Z$lat) - min(Z$lat)) # meters per degree at this lat
  dt_deg = (dt / d_lon) * 1.1
  # subset the two sets of data to make sure we only calculate what we need to
  A = A[dateTime %between% c(min(Z$dateTime) - tt, max(Z$dateTime) + tt)]
  if(nrow(A) == 0){stop("Time bounds of A are not within Z, no overlaps")}
  A = A[lat %between% scales::expand_range(range(Z$lat), add=dt_deg) &
          lon %between% scales::expand_range(range(Z$lon), add=dt_deg)] # need to use scales because lon can be neg
  if(nrow(A) == 0){stop("Spatial bounds of A are not within Z, no overlaps")}
  Z = Z[dateTime %between% c(min(A$dateTime) - tt, max(A$dateTime) + tt)]
  if(nrow(Z) == 0){stop("Time bounds of Z are not within A, no overlaps")}
  Z = Z[lat %between% scales::expand_range(range(A$lat), add=dt_deg) &
          lon %between% scales::expand_range(range(A$lon), add=dt_deg)] # need to use scales because lon can be neg
  if(nrow(Z) == 0){stop("Spatial bounds of Z are not within A, no overlaps")}

  Dm = distm(Z[,.(lon, lat)], A[,.(lon, lat)]) # generate distance matrix
  Dx = which(Dm < dt, arr.ind=T) # find the indexes in the matrix where close enough
  Dx = data.table(cbind(Dx, id = 1:nrow(Dx), dist = Dm[Dx])) # convert to DT and add actual distance
  Zx = Z[Dx$row] # select those in Z which are close enough, replicate if needed
  colnames(Zx) = paste0(colnames(Zx),"_", Z_name) # rename columns to avoid duplicates
  Ax = A[Dx$col] # select those in Az which are close enough, replicate if needed
  colnames(Ax) = paste0(colnames(Ax),"_", A_name) # rename columns to avoid duplicates
  M = cbind(Zx, Ax) # stick them together
  M[, dist := Dx$dist] # add the distance
  M[, dtime := as.numeric(get(paste0("dateTime_", A_name))) - as.numeric(get(paste0("dateTime_", Z_name)))] # calculate distance in time
  M = M[abs(dtime) < tt] # subset those too far away in time.
  return(M)
}

geom_text_contour <- function (mapping = NULL, data = NULL, stat = "text_contour",
                               position = "identity", ..., min.size = 5, skip = 0, rotate = TRUE,
                               parse = FALSE, nudge_x = 0, nudge_y = 0, stroke = 0, stroke.color = "white",
                               check_overlap = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`",
           call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  layer(data = data, mapping = mapping, stat = stat, geom = GeomTextContour,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(skip = skip, min.size = min.size, rotate = rotate,
                      parse = parse, check_overlap = check_overlap, stroke = stroke,
                      stroke.color = stroke.color, na.rm = na.rm, ...))
}
