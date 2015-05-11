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
#' @param style character string denoting map style, default is 'gsat'
#' @param db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return ggmap object
#' @keywords ferrybox query
#' @examples d <- fetch.ferrybox('CEND_01_14')
#' @export
deployment.map <- function(platforms = c(1, 4, 8),
                            deployment_group_id = 'ALL',
                            style = 'gsat',
                            labels = TRUE,
                            active_only = TRUE,
                            zoom_to_group = TRUE,
                            db_name = 'smartbuoydblive'){
    
    require(reshape2)
    require(RODBC)
    require(ggplot2)
    require(data.table)
    require(lubridate)
    require(ggmap)

    sbdb= odbcConnect("SmartbuoydbLive")

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
    d$active = "Historical"
    d$active[d$dateTo > now()] = "Current"
    
    d[platform == 1, platformName := 'SmartBuoy']
    d[platform == 1, shape := 2]
    d[platform == 4, platformName := 'Lander']
    d[platform == 4, shape := 0]
    d[platform == 8, platformName := 'Waverider']
    d[platform == 8, shape := 1]
    
    d = d[platform %in% platforms] # remove unwanted deployments
    if(deployment_group_id != 'ALL'){
        d = d[groupId %in% deployment_group_id]
    }
    
    if(active_only == T){
        d = d[active == 'Current']
    }
    
    ranges = data.frame(zoom = c(3, 4, 5, 6, 7, 8), range = c(120, 60, 30, 14, 8, 4))
    
    if(zoom_to_group == FALSE){
        centre.lat = 53
        centre.lon = -2.7
        zoom = 6
    }else{
        centre.lat = mean(range(d$lat))
        centre.lon = mean(range(d$lon))
        max.range = max(diff(range(d$lon)), diff(range(d$lat)))
        zoom = max(ranges$zoom[ranges$range >= max.range])
    }
    
    centre = c(centre.lon, centre.lat)
    
    if(style == 'gsat'){
    mp = ggmap(get_map(location = centre, zoom = zoom, maptype = 'satellite'))
    mp = mp + geom_point(data = d, aes(lon, lat, color = active, position = 'jitter', shape = as.factor(platform)), size = 2.5) +
        scale_color_discrete('') + scale_shape_manual('', values = unique(d$shape), labels = unique(d$platformName), drop = F) +
        labs(x = 'Longitude', y = 'Latitude')
    }else{
        stop('style not implemented')
    }
    
    return(list(map = mp, data = d))
}