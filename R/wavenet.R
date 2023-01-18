#' WaveNet data query tool
#'
#' Fetches WaveNet DWR data
#'
#' @details This function queries the SmartBuoy database and returns a data.table
#'
#' @param deployment optional string matching smartbuoy deployment
#' @param deployment_group optional string matching smartbuoy deployment_group
#' @param after optional date string, if provided only data after this date will be returned, assumes UTC e.g. "2014-08-10"
#' @param before optional date string, if provided only data before this date will be returned, assumes UTC e.g. "2014-12-09"
#' @param db_name character string matching ODBC data source name, defaults to 'smartbuoydblive'
#' @return data.table with returned data in "long" format or error string if no data returned
#' @keywords wavenet query
#' @import RODBC
wavenet.fetch <- function(deployment = NA, deployment_group = NA,
                          parameters = c('Hm0', 'Tpeak', 'Tz', 'W_PDIR', 'W_SPR'),
                          after = NA, before = NA,
                          db_name = 'smartbuoydblive'){

  query = paste("SELECT (CAST([Date/Time] AS NVARCHAR)) as dateTime,",
                  "[Deployment Id] as deployment,",
                  "[Deployment Group Id] as deployment_group,",
                  "[Deployment Latitude] as lat,",
                  "[Deployment Longitude] as lon,",
                  "[Instrument Id] as instrument_id,",
                  "[Result - mean] as value,",
                  "[Parameter Code] as par,",
                  "SensorParameter.[ParUnit] as unit",
                  "FROM AdHocRetrieval_BurstMeanResults",
                  "INNER JOIN SensorParameter ON",
                  "AdHocRetrieval_BurstMeanResults.[Sensor Id] = SensorParameter.SensorId AND",
                  "AdHocRetrieval_BurstMeanResults.[Parameter Code] = SensorParameter.ParCode")

    parameters = paste(parameters, collapse = "', '")
    query = paste0(query, " WHERE [Parameter code] IN ('", parameters, "')")

    # filter deployments, is.na evaluates each element of vector, so only check first one is not NA
    if(!is.na(deployment[1])){
        deployment = paste(deployment, collapse = "', '")
        query = paste0(query, " AND [Deployment Id] IN ('", deployment, "')")
    }
    if(!is.na(deployment_group[1])){
        deployment_group = paste(deployment_group, collapse = "', '")
        query = paste0(query, " AND [Deployment Group Id] IN ('", deployment_group, "')")
    }
      # if before or after is suppled build filter into query
    if(!is.na(before)){
        query = paste0(query, " AND [Date/Time] <= '", before, "'")
    }
    if(!is.na(after)){
        query = paste0(query, " AND [Date/Time] >= '", after, "'")
    }
    print(query)
    sb = odbcConnect(db_name)
    dat = data.table(sqlQuery(sb, query))
    odbcCloseAll()

    # check if valid data has been returned, if not quit
    if(! nrow(dat) > 1){
        stop("no data returned")
    }
    dat[, dateTime := as.POSIXct(dateTime, format="%b %d %Y %I:%M%p", tz="UTC")]
    return(dat[order(dateTime)])
}

#' Decode DWR3 sbd file info
#'
#' @param sbd waverider sbd filename
#' @param debug if True, print matching messages
#'
#' @return list with named elements of lat, lon and battery (in weeks)
#'
read.wavenet_sbd_info <- function(sbd, debug = F){
  sbd = readBin(sbd, what = "raw", n = 128)

  which_msg <- function(byte){
    # checks id nibble from byte, returns 2 element vector or message ID and size of message
    # first element nibble
    HiNibble = paste0(rawToBits(byte)[5:8], collapse = " ")
    switch(HiNibble,
           "00 00 00 00" = c("0", 31), # message 0 (31 bytes) heave
           "01 01 00 00" = c("3", 11), # message 3 (11 bytes) spectral param
           "01 00 01 00" = c("5", 10), # message 5 (10 bytes) info
           "00 01 01 00" = c("6", 10), # message 6 (10 bytes) sea surface temp
           "01 00 00 01" = c("9", 31), # message 9 (31 bytes) compressed directional spectrum
           "00 01 00 01" = c("A", 10), # message A (10 bytes) air temperature
           c(NA, NA))
  }

  # starting with first byte,
  # figure out which message it is and increment using known message lengths until we find info message
  i = 1
  info_index = NA
  sst_index = NA
  airtemp_index = NA
  while(i < length(sbd)){
    res = which_msg(sbd[i])
    if(debug){
      print(c(paste0(rawToBits(sbd[i])[5:8], collapse = " "), res)) # debug
    }
    if(anyNA(res)){stop(paste("message type not recognised at index "), i)}
    if(res[1] == "5"){
      info_index = i
    }
    if(res[1] == "6"){
      sst_index = i
    }
    if(res[1] == "A"){
      airtemp_index = i
    }
    i = i + as.numeric(res[2])
  }

  raw_to_dec <- function(x)  sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))

  info = sbd[info_index:(info_index+10)]
  info = as.numeric(info)[-1]
  lat = 90 * (info[1] * 0x10000 + info[2] * 0x100 + info[3])/2^23
  lon = 180 * (info[4] * 0x10000 + info[5] * 0x100 + info[6])/2^23
  lon[lon > 180] = lon[lon > 180] - 360
  batt = info[7]
  if(is.na(sst_index)){
    sst = NA
  }else{
    sst = sbd[(sst_index+1):(sst_index+2)]
    sst = rawToBits(sst)[1:10]
    sst = raw_to_dec(sst)/20 - 5
  }

  return(list(lat = lat, lon = lon, batt = batt))
}
