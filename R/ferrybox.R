#' Ferrybox query tool
#'
#' Fetches ferrybox data
#'
#' @details TODO
#' @param optional cruise ID string, e.g. "CEND_02_14", if provided only data from this cruise will be returned.
#' @param optional date string, if provided only data after this date will be returned, assumes UTC e.g. "2014-08-10"
#' @param optional date string, if provided only data before this date will be returned, assumes UTC e.g. "2014-12-09"
#' @param optional area vector consisting of 4 elements, minimum Latitude, minimum Longitude, maximum Latitude, maximum Longitude e.g. c(52, -3.5, 53, -4)
#' @param optional vector of parameter code names, defaults to c('TEMP', 'SAL', 'FTU', 'O2CONC')
#' @param optional boolean, if True only data where result quality = 0 is returned, i.e. good data
#' @param optional character string matching ODBC data source name, defaults to 'ferrybox'
#' @return data.frame with returned data in "long" format or error string if no data returned
#' @keywords ferrybox query
#' @examples d <- fetch.ferrybox('CEND_01_14')
fetch.ferrybox<- function(cruiseID = NA,
                           after = NA, before = NA,
                           area = NA,
                           parameters = c('TEMP', 'SAL', 'FTU', 'O2CONC'),
                           QA_data = FALSE,
                           RQ0 = TRUE,
                           db_name = 'ferrybox'){
    require(RODBC)
    require(data.table)
        # boilerplate query start
    query = paste("SELECT (CAST([Date/Time] AS NVARCHAR)) as dateTime,",
              "[Cruise Id] as cruise,",
              "[Latitude] as latitude,",
              "[Longitude] as longitude,",
              "[Course] as course,",
              "[Speed] as speed,",
              "[Heading] as heading,",
              "[Result - Mean] as value,",
              "[Result - Variance] as var,",
              "[Result - Count] as n,",
              "[Result - Error] as stdev_derived,",
              "[Result - Error Type] as derived_type,",
              "[Sensor Description] as sensor,",
              "[Parameter code] as par,",
              "[Parameter Unit] as unit",
              "FROM v_FerryBox_Data")
    
        # collapse down parameters vector and wrap with quotes to work with IN (xxx)
    parameters = paste(parameters, collapse = "', '")
    query = paste0(query, " WHERE [Parameter code] IN ('", parameters, "')")
    
    # filter cruise ID, is.na evaluates each element of vector, so only check first one is not NA
    if(!is.na(cruiseID[1])){
        cruises = paste(cruiseID, collapse = "', '")
        query = paste0(query, " AND [Cruise Id] IN ('", cruises, "')")
    }

        # if area is suppled build filter into query
        # area = c(minLat, minLon, maxLat, maxLon)
    if(!is.na(area)){
            # check if area contains 4 elements
        if(length(area) != 4){
            stop('area does not have 4 elements')
        }else{
            # TODO between code
            stop(' not implemented')
        }
    }
    
      # if before or after is suppled build filter into query
    if(!is.na(before)){
        query = paste0(query, " AND [Date/Time] <= '", before, "'")
    }
    if(!is.na(after)){
        query = paste0(query, " AND [Date/Time] >= '", after, "'")
    }

        # if only RQ0 data is required build filter into query
    if(RQ0 == TRUE){
        query = paste0(query, " AND [Result - Quality] = 0")
    }
    if(QA_data == TRUE){
        query = paste0(query, " AND [Result - Required QA Level Met] = 1")
    }

    # finally
    query = paste(query, 'ORDER BY dateTime')
    fb = odbcConnect(db_name)
    dat = sqlQuery(fb, query)
    print(query)
    odbcCloseAll()
    dat = data.table(dat)
    
    # check if valid data has been returned, if not quit
    if(! nrow(dat) > 1){
        stop("no data returned")
    }

    print(paste(nrow(dat), 'rows returned'))
    dat$dateTime = as.POSIXct(dat$dateTime, format="%b %d %Y %I:%M%p", tz="UTC") 
    return(dat)
}

#' Ferrybox device data reader
#'
#' reads ferrybox device data files
#'
#' @details TODO
#' @param devdata_folder character string indicating path to folder containing device data files
#' @param pivot optional boolian indicating if returned table should be recast into 'wide' format
#' @return data.frame
#' @keywords ferrybox query
#' @examples d <- fetch.ferrybox('CEND_01_14')
#' @export
ferrybox.devdata <- function(devdata_folder, pivot = F){
    require(data.table)
    require(reshape2)
    
    # sometimes \0 characters appear in data, to sanitise run sed from a bash terminal
    # for i in *; do sed -i "s/\x0//g" $i done

    dat = data.table()
    for(fn in list.files(devdata_folder)){
        f = paste(devdata_folder, fn, sep='/')
        fd = readLines(f)
        startLine = grep("DATASETS",fd)
        sensorLine = grep("Type", fd)
        sensor = unlist(strsplit(fd[sensorLine], '; '))[2]
        print(f)
        d = fread(f, skip = startLine, sep = '\t')
        param = colnames(d)[2]
        d = d[-1, c('$Timestamp', param, 'Quality', 'Longitude', 'Latitude'), with = F]
        setnames(d, c('$Timestamp', param, 'Quality'), c('datetime', 'value', 'quality'))
        d$param = paste(param, sensor, sep = '_')
        dat = rbind(dat, d)
    }
    dat$datetime = as.POSIXct(dat$datetime, format = '%Y.%m.%d %H:%M:%S', tz='UTC')
    
    export = dat
        # reclasiffy types
    export$value = as.numeric(export$value)
    export$Longitude = as.numeric(export$Longitude)
    export$Latitude = as.numeric(export$Latitude)

    if(pivot == T){
        export = dcast.data.table(export, datetime ~ param)
        pos = dat[,.(datetime, Latitude, Longitude)]
        pos = pos[,lapply(.SD, as.numeric), by = datetime]
        pos = pos[,lapply(.SD, median), by = datetime]
        export = merge(export, pos, by = 'datetime')
        return(export)
    }else{
        return(export)
    }
}

#' ferrybox cruise id queryer
#'
#' Fetches lists of cruise id