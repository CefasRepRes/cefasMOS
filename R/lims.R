#' Fetch nutrients data from LIMS (post 2013)
#'
#' @details parameter codes:
#' Chlorophyll, TOXN, O2, SLD, SAL
#' 
#' @param parameters 
#' @param cruise 
#' @param db_name
#'
#' @return data frame (data.table) of extracted nutrients data
#' @export
lims.fetch <- function(parameters = 'SAL', cruise = NA, db_name = 'lims'){
    require(RODBC)
    require(data.table)
    
        query = paste("SELECT [C_DATE_COLLECTED] as dateTime,",
                  "[C_LATITUDE] as latitude,",
                  "[C_LONGITUDE] as longitude,",
                  "[C_CRUISE_CODE] as cruise,",
                  "[C_STATION] as station,",
                  "[NAME] as par,",
                  "[UNITS] as unit,",
                  "[ENTRY] as value,",
                  "[C_SAMPLE_DEPTH] as depth",
                  "FROM C_NUTRIENTS_SAMPLES")
    
        # collapse down parameters vector and wrap with quotes to work with IN (xxx)
    parameters_fetch = paste(parameters, collapse = "', '")
    query = paste0(query, " WHERE [NAME] IN ('", parameters_fetch, "')")
    query = paste(query, "AND [SAMPLE STATUS] = 'A' AND [RESULT STATUS] = 'A'")
    
    if(!is.na(cruise[1])){
        cruise = paste(cruise, collapse = "', '")
        query = paste0(query, " AND [C_CRUISE_CODE] IN ('", cruise, "')")
    }
    
    query = paste(query, 'ORDER BY dateTime')
    print(query)
    
    lims = odbcConnect(db_name)
    dat =  data.table(sqlQuery(lims, query))
    # check if valid data has been returned, if not quit
    if(! nrow(dat) > 1){
        stop("no data returned")
    }
    dat[,longitude := as.numeric(as.character(longitude))] # lat and long are stored as varchar15
    dat[,latitude := as.numeric(as.character(latitude))]
    dat[,value := as.numeric(as.character(value))]
    odbcCloseAll()
    return(dat)
}

#' Fetch nutrients data from LIMS (pre 2013)
#'
#' @param parameters 
#' @param db_name
#'
#' @return data frame (data.table) of extracted nutrients data
#' @export
lims.fetch.historic <- function(parameters = 'SAL', db_name = 'lims'){
    require(RODBC)
    require(data.table)
    
        query = paste("SELECT [date_collected] as dateTime,",
                  "[latitude] as latitude,",
                  "[longitude] as longitude,",
                  "[station] as station,",
                  "[ParamId] as par,",
                  "[EnteredUnits] as unit,",
                  "[EnteredValue] as value,",
                  "[pressure] as depth",
                  "FROM C_Historic_Nutrient_Samples")
    
        # collapse down parameters vector and wrap with quotes to work with IN (xxx)
    parameters_fetch = paste(parameters, collapse = "', '")
    query = paste0(query, " WHERE [ParamId] IN ('", parameters_fetch, "')")
    
    query = paste(query, 'ORDER BY dateTime')
    print(query)
    
    lims = odbcConnect(db_name)
    dat =  data.table(sqlQuery(lims, query))
    # check if valid data has been returned, if not quit
    if(! nrow(dat) > 1){
        stop("no data returned")
    }
#     dat[,longitude := as.numeric(as.character(longitude))] # lat and long are stored as varchar15
#     dat[,latitude := as.numeric(as.character(latitude))]
#     dat[,value := as.numeric(as.character(value))]
    odbcCloseAll()
    return(dat)
    
}

#' Fetch list of cruises from LIMS
#'
#' @param yr 
#' @param db_name 
#'
#' @return vector of cruise ids
#' @export
lims.cruiselist <- function(yr = 'ALL', db_name = 'lims'){
    require(RODBC)
    query = "SELECT DISTINCT [C_CRUISE_CODE] FROM C_NUTRIENTS_SAMPLES"
    if(yr != 'ALL'){
        query = paste(query, ' WHERE YEAR([C_DATE_COLLECTED]) = ', yr, sep = '')
    }
    sb = odbcConnect(db_name)
    cruiseList = sqlQuery(sb, query)
    odbcCloseAll()
    return(as.vector(cruiseList))
}
    