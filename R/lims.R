#' Fetch nutrients data from LIMS
#'
#' @details parameter codes:
#' Chlorophyll, TOXN, O2, SLD, SAL
#'
#' note "value" returned as character string to allow for <LOD values
#'
#' @param parameters character string of LIMS parameter names
#' @param cruise optional character string of cruise name
#' @param after optional date string, if provided only data after this date will be returned, assumes UTC e.g. "2014-08-10"
#' @param before optional date string, if provided only data before this date will be returned, assumes UTC e.g. "2014-12-09"
#' @param area optional vector consisting of 4 elements, max Latitude, max Longitude, min Latitude, min Longitude e.g. c(53, -2.5, 52, -4)
#' @param convert convert fields to numeric? default = True
#' @param db_name character string matching ODBC data source name, defaults to 'lims'
#' @return data frame (data.table) of extracted nutrients data
#' @import RODBC data.table
#' @export
lims.fetch <- function(parameters = c('SAL', 'CHLOROPHYLL', 'SPM', 'TOXN', 'SIO4', 'NH4', 'PHAEOP', 'NO2', 'PO4', 'DO2'),
                       cruise = NA,
                       after = NA, before = NA,
                       convert = T,
                       area = NA,
                       db_name = 'lims'){

    query = paste("SELECT [C_DATE_COLLECTED] as dateTime,",
                  "[C_LATITUDE] as latitude,",
                  "[C_LONGITUDE] as longitude,",
                  "[C_SURVEY] as survey,",
                  "[C_STATION] as station,",
                  "[TEXT_ID] as LSN,",
                  "[C_SAMPLING_GEAR] as gear,",
                  "[C_COMMENT] as comment,",
                  "[NAME] as variable,",
                  "[UNITS] as unit,",
                  "[ENTRY] as value,",
                  "[C_SAMPLE_DEPTH] as depth",
                  "FROM C_NUTRIENTS_SAMPLES")


        # collapse down parameters vector and wrap with quotes to work with IN (xxx)
    parameters_fetch = paste(parameters, collapse = "', '")
    query = paste0(query, " WHERE [RESULT STATUS] = 'A' AND [NAME] IN ('", parameters_fetch, "')")

    if(!is.na(cruise[1])){
      cruise = paste(cruise, collapse = "', '")
      query = paste0(query, " AND [C_CRUISE_CODE] IN ('", cruise, "')")
    }
    if(!is.na(before)){
      query = paste0(query, " AND [C_DATE_COLLECTED] <= '", before, "'")
    }
    if(!is.na(after)){
      query = paste0(query, " AND [C_DATE_COLLECTED] >= '", after, "'")
    }

    if(!is.na(area[1])){
        # check if area contains 4 elements
      if(length(area) != 4){
        stop('area does not have 4 elements')
      }else{
        query = paste(query,
        "AND CONVERT(float, (SELECT C_LATITUDE WHERE C_LATITUDE NOT LIKE '%[^0-9.-]%')) BETWEEN", area[3], "AND", area[1],
        "AND CONVERT(float, (SELECT C_LONGITUDE WHERE C_LONGITUDE NOT LIKE '%[^0-9.-]%')) BETWEEN", area[4], "AND", area[2])
      }
    }

    query = paste(query, 'ORDER BY dateTime')
    print(query)

    lims = odbcConnect(db_name)
    dat =  data.table(sqlQuery(lims, query, as.is=T))
    odbcCloseAll()
    # check if valid data has been returned, if not quit
    if(! nrow(dat) > 1){
        warning("no data returned")
    }
    if(convert == TRUE){
      dat[, dateTime := as.POSIXct(dateTime, format="%Y-%m-%d %H:%M:%S", tz="UTC")]
      dat[, latitude := as.numeric(latitude)]
      dat[, longitude := as.numeric(longitude)]
      dat[, value := as.numeric(value)]
      dat[, depth := as.numeric(depth)]
    }
    return(dat)
}


#' Fetch list of cruises from LIMS
#'
#' @param yr integer specifing a year to limit the search, default is 'ALL'
#' @param db_name character string matching ODBC data source name, defaults to 'lims'
#'
#' @return vector of cruise ids
#' @import RODBC
#' @export
lims.cruiselist <- function(yr = 'ALL', db_name = 'lims'){
    query = "SELECT DISTINCT [C_SURVEY] FROM C_NUTRIENTS_SAMPLES"
    if(yr != 'ALL'){
        query = paste(query, ' WHERE YEAR([C_DATE_COLLECTED]) = ', yr, sep = '')
    }
    sb = odbcConnect(db_name)
    cruiseList = sqlQuery(sb, query)
    odbcCloseAll()
    return(as.vector(cruiseList))
}
