#' Ferrybox device data reader
#'
#' reads ferrybox device data files
#'
#' @details TODO
#' Ferrybox quality codes:
#' 0 = good, 1 = no data, 2 = over value, 4 = under value, 8 = sensor timeout
#' 64 = clean cycle, 128 = standby, 256 = empty, 512 = error, 1024 = undefined, 2048 = simulation mode
    #' sometimes \0 characters appear in data, to sanitise run sed from a bash terminal
    # sed -i -b "s/\x0//g" *
    # filter by files by ... *_A_Optode*
    # -i = in place, -b = binary (keep windows line feeds)

#' @param devdata_folder character string indicating path to folder containing device data files
#' @param pivot optional boolian indicating if returned table should be recast into 'wide' format
#' @return data.frame
#' @keywords ferrybox
#' @export
read.ferrybox.devdata <- function(devdata_folder, pivot = F){
    require(data.table)
    require(reshape2)

    dat = data.table()
    for(fn in list.files(devdata_folder)){
        f = paste(devdata_folder, fn, sep='/')
        print(f)
        fd = readLines(f)
        startLine = grep("DATASETS",fd)
        sensorLine = grep("Type", fd)
        sensor = unlist(strsplit(fd[sensorLine], '; '))[2]
        d = fread(f, skip = startLine, sep = '\t')
        param = colnames(d)[2]
        d = d[-1, c('$Timestamp', param, 'Quality', 'Longitude', 'Latitude'), with = F]
        setnames(d, c('$Timestamp', param, 'Quality'), c('datetime', 'value', 'quality'))
        d$sensor = sensor
        d$param = param
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

#' Ferrybox conlog data reader
#'
#' reads DAQ server conlog data files
#'
#' @details TODO
#' requires intensive string operations on many rows and thus fairly slow
#' @param conlog character string indicating device data file
#' @return data.frame
#' @keywords ferrybox conlog
#' @export
read.ferrybox.conlog <- function(conlog){

    require(stringr)
    conlog = fread(conlog)
    conlog[,Time := as.POSIXct(conlog$Time, format = '%m/%d/%Y %H:%M:%S', tz = 'UTC')]
    parameters = "Time|Sounder|WeatherPack|GPS_GLL|MRU_3axis"
    conlog = subset(conlog, select = colnames(conlog) %like% parameters)
    conlog = melt(conlog, id.vars = 'Time')
    conlog = na.omit(conlog)
    conlog[, sensor := str_extract(variable, "\\w+")]
    conlog[, parameter := str_extract(variable, "\\w+(?=$)")]
    conlog$variable = NULL
    conlog[order(Time)]
    return(conlog)
}

read.profiler.ULP <- function(f){
  require(reshape2)
  require(data.table)

  lines = readLines(f)

  timeStamp = lines[max(grep("TIMESTAMP", lines))]
  timeStamp = unlist(strsplit(timeStamp," "))
  timeStamp = paste(timeStamp[4],"/",timeStamp[5]," ",timeStamp[3],sep="")
  timeStamp = as.POSIXct(timeStamp, format="%d/%m/%Y %H%M.%S",tz="UTC")

  # extract sensor lines
  sensorStart = max(grep("CHAN", lines))
  sensorEnd = length(lines)
  dat = data.table()
  for(l in (sensorStart + 1):sensorEnd){
    dataLine = lines[l]
    dataLine = unlist(strsplit(dataLine,","))
    rate = as.numeric(dataLine[3])/10
    id = dataLine[7]
    dataLine = dataLine[9:length(dataLine)]
    index = seq(0,(length(dataLine)*rate)-rate,rate)
    dat0 = data.table(channel = id, index, do.call("rbind", strsplit(dataLine, ";")))
    dat = rbind(dat, dat0, fill = T)
  }
  dip = melt.data.table(dat, id.var = c("channel", "index"))
  dip$value = as.numeric(dip$value)
  dip$startTime = timeStamp

  return(dip)
}

read.ULP000 <- function(file){
  fl = readLines(file)
  startLine = max(grep("CHAN", fl)) + 1
  # d = read.csv(file, header = F, skip = startLine)
  dat = data.table()
  for(sen in fl[startLine:length(fl)] ){
    sen = unlist(strsplit(sen, ","))
    channel = sen[2]
    XOPC = sen[4]
    rate = as.numeric(sen[3])
    d = sen[7:length(sen)]
    index = seq(0,(length(d)*rate)-rate,rate)
    dat = rbind(dat, data.frame(index = index, value = d, channel, XOPC))
  }
  dat[, c(range)]
  return(dat)
}

#' Ferrybox 10min file data reader
#'
#' reads JENA-4H 10min files
#'
#' @details TODO
#' @param folder folder containing 10min files
#' @param save if true tool will write to 10mindat.rdata file while reading
#' @param recursive if true look in subfolders
#' @return data.frame (data.table) of processed 10min files
#' @keywords ferrybox 10minfile
#' @export
read.ferrybox.10min <- function(folder, save = T, recursive = F){
  require(zoo)
  require(stringr)
  dat = data.table()
  for(f in list.files(folder, recursive = T)){
    if(grepl("10min", f, ignore.case = T)){
      print(f)
      f = paste0(folder, f)
      ln = readLines(f)
      dateLine = grep("Date Time", ln)
      d = read.table(f, sep = "\t", header = F, skip = dateLine + 1, fill = F)
      header1 = unlist(strsplit(ln[dateLine], "\t")) # split on tabs
      header1[header1 == ""] = NA # assign NA to blanks
      header1 = na.locf(header1) # pull forward into blank rows
      header2 = unlist(strsplit(ln[dateLine+1], "\t")) # split 2nd row, pad for date_time
      header = paste(header1, header2, sep = "~~") # combine headers
      colnames(d) = gsub("[^[:alnum:]~/]", "", header) # apply headers after removing bad chars
      d = data.table(d)
      d[, dateTime := as.POSIXct(d$"DateTime~~", format = "%Y.%m.%d %H:%M:%S", tz = "UTC")]
      d = d[,-c("DateTime~~"), with = F]
      d = suppressWarnings(
        melt.data.table(d, id.vars = grep("Course|Long|Lat|Satellite|Speed|Heading|dateTime", colnames(d)))
      )
      d[, c("variable", "unit", "telid", "serial", "stat") := tstrsplit(variable, "~~")]
      colnames(d) = gsub("~~[[:alnum:]]*", "", colnames(d))
      d = na.omit(d)
      d = dcast.data.table(d, ... ~ stat, value.var = "value")
      dat = rbind(dat, d, fill = T)
      if(save == T){save(dat, file = "10mindat.rdata")}
    }
  }
  return(dat)
}
