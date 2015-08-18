#' Fetch nutrients data from LIMS (post 2013)
#'
#' @param parameters 
#' @param cruise 
#'
#' @return data frame (data.table) of extracted nutrients data
#' @export
lims.fetch <- function(parameters = 'SAL', cruise = NA){
    require(RODBC)
    require(data.table)
    
        query = paste("SELECT [C_DATE_COLLECTED] as dateTime,",
                  "[C_LATITUDE] as latitude,",
                  "[C_LONGITUDE] as longitude,",
                  "[C_CRUISE_CODE] as cruise,",
                  "[NAME] as par,",
                  "[UNITS] as unit,",
                  "[ENTRY] as value",
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
    
    db_name = 'lims'
    lims = odbcConnect(db_name)
    dat =  data.table(sqlQuery(lims, query))
    dat[,longitude := as.numeric(as.character(longitude))] # lat and long are stored as varchar15
    dat[,latitude := as.numeric(as.character(latitude))]
    dat[,value := as.numeric(as.character(value))]
    odbcCloseAll()
    return(dat)
}
    