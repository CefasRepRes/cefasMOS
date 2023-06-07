#' Ferrybox device data reader
#'
#' reads ferrybox device data files
#'
#' @details
#' Ferrybox quality codes:
#' - 0 = good,
#' - 1 = no data,
#' - 2 = over value,
#' - 4 = under value,
#' - 8 = sensor timeout
#' - 64 = clean cycle,
#' - 128 = standby,
#' - 256 = empty,
#' - 512 = error,
#' - 1024 = undefined,
#' - 2048 = simulation mode
#'
#' sometimes \0 characters appear in data, to sanitise run sed from a bash terminal
#  sed -i -b "s/\x0//g" *
#  filter by files by ... *_A_Optode*
# -i = in place, -b = binary (keep windows line feeds)

#' @param devdata_folder character string indicating path to folder containing device data files
#' @param pivot optional boolean indicating if returned table should be recast into 'wide' format
#' @param print optional boolean indicating if current file should be printed, default is True.
#' @return data.table
#' @keywords ferrybox
#' @export
read.ferrybox.devdata <- function(devdata_folder, pivot = F, print = T){

    dat = list()
    for(fn in list.files(devdata_folder)){
        f = paste(devdata_folder, fn, sep='/')
        print(f)
        fd = readLines(f, 50) # all the info should be in the first 50 lines, no need to read whole file
        startLine = grep("DATASETS",fd)
        sensorLine = grep("Type", fd)
        sensor = unlist(strsplit(fd[sensorLine], '; '))[2]
        d = fread(f, skip = startLine, sep = '\t')
        param = colnames(d)[2]
        d = d[-1, c('$Timestamp', param, 'Quality', 'Minimum', 'Maximum', 'Variance', 'MeasCount', 'Longitude', 'Latitude'), with = F]
        setnames(d, c('$Timestamp', param, 'Quality','Minimum', 'Maximum', 'Variance', 'MeasCount', 'Longitude', 'Latitude'),
                 c('dateTime', 'value', 'quality', 'min', 'max', 'variance', 'count', 'lon', 'lat'))
        d[, sensor := sensor]
        d[, param := param]
        dat[[fn]] = d
    }
    dat = rbindlist(dat)
    dat$dateTime = as.POSIXct(dat$dateTime, format = '%Y.%m.%d %H:%M:%S', tz='UTC')

    export = dat
        # reclasiffy types
    export$value = as.numeric(export$value)
    export$variance = as.numeric(export$variance)
    export$count = as.numeric(export$count)
    export$lon = as.numeric(export$lon)
    export$lat = as.numeric(export$lat)

    if(pivot == T){
        export = dcast.data.table(export, dateTime ~ param)
        pos = dat[,.(dateTime, lat, lon)]
        pos = pos[,lapply(.SD, as.numeric), by = dateTime]
        pos = pos[,lapply(.SD, median), by = dateTime]
        export = merge(export, pos, by = 'dateTime')
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
#' @importFrom stringr str_extract
#' @export
read.ferrybox.conlog <- function(conlog){

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

#' Process ESM2 .000 files
#'
#' Process ESM2 .000 files
#'
#' @param file an extracted ESM2 .000 filename
#'
#' to unpack a .ULP use the `unulp.exe` with the following command: `ulunp -i *.ULP`.
#'
#' @return data.table
#' @export
read.ULP000 <- function(file){
  fl = readLines(file)
  id = fl[max(grep("ID", fl)+1)]
  id = unlist(strsplit(id, ","))
  startTime = fl[max(grep("TIMESTAMP", fl))]
  startTime = unlist(strsplit(startTime, " "))
  startTime = paste(startTime[5], startTime[4], startTime[3])
  startTime = as.POSIXct(startTime, format = "%Y %d/%m %H%M.%S", tz = "UTC")
  startLine = max(grep("CHAN", fl)) + 1
  dat = list()
  for(i in fl[startLine:length(fl)] ){
    sen = unlist(strsplit(i, ","))
    channel = as.numeric(sen[2])
    rate = as.numeric(sen[3])
    d = sen[7:length(sen)]
    index = seq(0,(length(d)*rate)-rate,rate)
    dat[[i]] = data.table(index = index, raw = d, channel, stringsAsFactors=F)
  }
  dat = rbindlist(dat)
  dat[, burst := id[6]]
  dat[, time := startTime + (index/10)]
  dat[, id := id[1]]
  fst = dat[index == 0]
  max_element = max(stringr::str_count(fst$raw, ";"))
  outs = paste0("v", 0:max_element)
  dat[, (outs) := tstrsplit(raw, ";", type.convert = T)]
  return(dat)
}

#' Ferrybox 10min file data reader
#'
#' reads JENA-4H 10min files
#'
#' @details TODO
#' @param folder folder containing 10min files
#' @param recursive if true look in subfolders
#' @param print_file if true filenames are printed as processed (faster without)
#' @param debug if true enables careful parsing
#' @return data.frame (data.table) of processed 10min files
#' @keywords ferrybox 10minfile
#' @importFrom zoo na.locf
#' @export
read.ferrybox.10min <- function(folder, recursive = T, print_file = F, debug = F){
  read_10min <- function(f, print_file = F){
    if(grepl("10minfiles", f, ignore.case = F)){
      f = paste0(folder, f)
      ln = readLines(f)
      if(print_file){print(f)}
      dateLine = grep("Date[ /\t]Time", ln, perl = T)
      d = fread(f, header = F, skip = dateLine + 1, fill = T)
      cruise = strsplit(ln[2], "\t")[[1]][2]
      SIC = strsplit(ln[3], "\t")[[1]][2]
      comment = strsplit(ln[6], "\t")[[1]][2]
      header1 = unlist(strsplit(ln[dateLine], "\t")) # split on tabs
      header1[header1 == ""] = NA # assign NA to blanks
      header1 = zoo::na.locf(header1) # pull forward into blank rows
      header2 = unlist(strsplit(ln[dateLine+1], "\t")) # split 2nd row, pad for date_time
      header = paste(header1, header2, sep = "~~") # combine headers
      header = gsub("Date/Time", "DateTime", header)
      colnames(d) = gsub("[^[:alnum:]~/]", "", header[1:length(colnames(d))]) # apply headers after removing bad chars
      if("Date~~" %in% header){d[, "DateTime~~" := paste(`Date~~`, `Time~~`)]}
      d[, dateTime := as.POSIXct(d$"DateTime~~", format = "%Y.%m.%d %H:%M:%S", tz = "UTC")]
      d = suppressWarnings(
        melt.data.table(d, id.vars = grep("Course|(^Longitude)|(^Latitude)|GPSSource|Satellite|Speed|Heading|dateTime", colnames(d)))
      )
      d[, c("variable", "unit", "telid", "serial", "stat") := tstrsplit(variable, "~~")]
      colnames(d) = gsub("~~[[:alnum:]]*", "", colnames(d))
      d = d[!is.na(value) & !variable %in% c("Date", "Time")]
      if(anyDuplicated(d) > 0){ warning(paste("duplicates found and removed", f)) }
      d = dcast.data.table(unique(d), ... ~ stat, value.var = "value", fun.aggregate=median)
      d[, Quality := as.character(Quality)]
      d[, Cruise := cruise]
      d[, SIC := SIC]
      d[, Comment := comment]
      d[, `NA` := NULL]
      if(debug){
        d[, filename := f]
      }
      return(d)
    }
  }
  if(print_file){
    recursive = T
    dat = lapply(list.files(folder, recursive = recursive), read_10min, print_file = T)
    dat = rbindlist(dat, fill = T)
  }else{
    dat = pbapply::pblapply(list.files(folder, recursive = recursive), read_10min)
    dat = rbindlist(dat, fill = T)
  }
  return(dat[order(dateTime)])
}

#' SmartBuoy live aquire export reader
#'
#' reads live aquire files exported from smartbuoydb and plots output
#'
#' @details This function reads text files generated by the SmartBuoy interface live aquire.
#' @param file location of live aquire file, leave blank to use dialog box
#' @return a list containing a data frame and ggplot
#' @keywords smartbuoy esm2
#' @export
read.SmartBuoyLiveAquireExport <- function(file = svDialogs::dlgOpen(title = 'Open LiveAquire file...')$res){
  warning("This tool can not be considered 'robust'!")
    x = fread(file)     # read in file
    x = x[,lapply(.SD, as.numeric)]     # convert all columns to numeric
    x = melt(x, id.vars = 'Seconds Elapsed')
    setnames(x, 'Seconds Elapsed', 'time')
    x[,depth := as.numeric(stringr::str_extract(variable, '\\d+.\\d(?=m)')), by = variable]
    x[,serial := stringr::str_extract(variable, '\\d+(?=,)'), by = variable]
    x[,channel := unlist(strsplit(as.character(variable), split = ':', fixed = T))[1], by = variable]
    x[,subchannel := unlist(strsplit(as.character(channel), split = '.', fixed = T))[2], by = variable]
    x[,par := unlist(strsplit(as.character(variable), split = ' ', fixed = T))[2], by = variable]
    dat = x
    plotpar = c('TEMP', 'COND', 'FLUORS', 'FTU', 'O2SAT', 'PAR')
    plot = ggplot(na.omit(x[par %in% plotpar,]), aes(time, value)) +
        geom_line(aes(color = variable)) +
        facet_grid(variable ~ .,scales='free') +
        theme_bw()
    return(list(dat, plot))
}

#' Read (and check) ESM2 LiveAquire
#'
#' Function to parse and optionally plot liveaquire data
#'
#' @param f filename
#' @param plot if True (default) plots the liveaquire data
#'
#' @return ggplot or data.table depending on value of "plot"
#' @export
#'
read.liveAquire <- function(f, plot = T){
  x = fread(f)
  m = suppressWarnings(melt(x, id.vars = "Seconds Elapsed"))
  m = m[!is.na(value)]
  m[, variable := gsub(",|#|\\(|\\)", "", variable)]
  m[, c("channel", "variable") := tstrsplit(variable, ":")]
  m[, c("serial", "variable", "depth") := tstrsplit(variable, " ")]
  m[, channel := as.numeric(channel)]
  if(plot == T){
    p1 = ggplot(m[channel %between% c(0, 5.9)]) + geom_line(aes(`Seconds Elapsed`, value, color = variable)) +
      facet_grid(channel + serial + depth + variable ~ ., scales = "free_y") +
      theme(legend.position = "none") +
      labs(title = f, y = NULL)
    p2 = ggplot(m[channel %between% c(6, 19)]) + geom_line(aes(`Seconds Elapsed`, value, color = variable)) +
      facet_grid(channel + serial + depth + variable ~ ., scales = "free_y") +
      labs(y = NULL) +
      theme(legend.position = "none")
    return(p1 + p2)
  }else{
    return(m)
  }
}


#' read analog live aquire data
#'
#' @param x one or more files captured from liveaquire
#' @param channels_table data.frame containing channel calibrations
#' @return processed data table
#' @export
#'
read.liveAquireAnalog <- function(x, channels_table = data.frame(channel = c(0, 1, 6),
                                                                    type = c("OBS", "FLU", "OBS"),
                                                                    slope = c(1.22, 1.22, 1.22),
                                                                    offset = c(0, 0, 0))){
  rlaq <- function(file, channels_table){
    ln = readLines(file, warn = F)
    if(!any(stringr::str_detect(ln, "(\\d[as]\\d\\w{3})"))){
      warning(paste("no data found in file ", file, "skipping...")); return(NULL)
      }
    ex = stringr::str_extract_all(ln, "(\\d[as]\\d\\w{3})")
    ticks = 1:length(ex[lapply(ex, length) >0]) # number of lines where values
    ex = unlist(ex)
    dat = data.table("tick" = 1:length(ex), "reading" = ex)
    dat[, c("channel", "mode", "range", "V1", "V2", "V3") := tstrsplit(reading, "", fixed = T)]
    dat[, raw_value := strtoi(paste0("0x",V1, V2, V3)) / 1000] # convert to decimal mV from hex ADC count
    dat[, channel := as.numeric(channel)]
    dat[, filename := file]

    if(!is.null(channels_table)){
      dat = merge(dat, channels_table, by = "channel")
      obs_gain = c(500, 100, 25, 5) # obs gain settings
      flu_gain = c(30, 10, 3, 1) # flu gain settings
      dat[type == "OBS", gain := obs_gain[as.numeric(range) + 1]]
      dat[type == "FLU", gain := flu_gain[as.numeric(range) + 1]]
      dat[, value := ((raw_value * slope) - (offset/1000)) * gain]
      dat = dat[order(tick)]
    }
    return(dat)
  }
 return(do.call(rbind, lapply(x, rlaq, channels_table)))
}


#' Read BODC CTD ascii (lst) files
#'
#' Reads ascii CTD files from BODC, attempts to extract metadata
#'
#' @param file string indicating BODC file location
#' @param stripAgg if True (default) aggregation is used
#'
#' @return data.frame (data.table)
#' @export
#' @importFrom stringr str_extract_all str_extract
#' @usage
#' x = lapply(list.files(folder, full.names = T, pattern = "*.lst"), read.BODC_ctd_asci)
#' d = rbindlist(x, fill = T)
read.BODC_ctd_ascii <- function(file, stripAgg = T){
  ln = readLines(file, warn = F)
  dataStart = max(grep("Cycle", ln))
  id = ln[min(grep("Id ", ln))]
  id = str_extract(id, "[\\w]+(?= Unit)")
  startTime = ln[min(grep("start:", ln))]
  startTime = str_extract(startTime, "(?<=start:)[\\d]+")
  latLon = ln[(min(grep("start:", ln)))]
  latLon = str_extract(latLon, "^\\S+")
  latLon.n =  as.numeric(unlist(str_extract_all(latLon, "[\\d\\.]+")))
  latLon.sign =  unlist(str_extract_all(latLon, "[NESW]"))
  if(latLon.sign[2] == "W"){latLon.n[3] = latLon.n[3]*(-1)}
  lat = convert_latlong(latLon.n[1], latLon.n[2])
  lon = convert_latlong(latLon.n[3], latLon.n[4])

  parmNames = unlist(str_extract_all(ln[dataStart], "\\w+"))
  if(stripAgg){
    parmNames2 = gsub("\\d{2}\\b", "", parmNames, perl = T)
    if(anyDuplicated(parmNames2) != 0){stop("aggregation needed")}
    parmNames = parmNames2
  }
  dat = data.table(read.table(file, skip = dataStart+1, stringsAsFactors = F))
  dat = suppressWarnings(dat[, lapply(.SD, as.numeric)])
  colnames(dat) = parmNames
  dat$Cycle = NULL
  dat$dateTime = as.POSIXct(startTime, format = "%Y%m%d%H%M%S", tz = "UTC")
  dat$id = id
  dat$lat = lat
  dat$lon = lon
  return(data.table(dat))
}


#' Read LIMS style nutrients excel spreadsheet
#'
#' Tool to parse the "Samples" sheet from a modern nutrients lab spreadsheet (.xlsx only)
#'
#' @param filename single xlsx filename
#'
#' @return long format data.table
#' @export
read.nutrients <- function(filename){
  r = openxlsx::read.xlsx(filename, sheet="Samples", detectDates=F)
  r_header = data.table(r[1,])
  r = data.table(r[-1,])
  r = r[,lapply(.SD, as.numeric), by=list(Survey, Site, comment, Gear)]
  r[, Date := openxlsx::convertToDateTime(r$Date)]
  dat = melt(r, id.var=c("LSN", "Survey", "Site", "Station", "depth",
                         "lat", "lon", "CR", "comment", "Gear", "replicate", "Date"))
  units = melt(r_header, id.var=c("LSN", "Survey", "Site", "Station", "depth",
                                  "lat", "lon", "CR", "comment", "Gear", "replicate", "Date"))
  dat = merge(dat, units[,.(variable, unit = value)], by="variable")
  dat = dat[!is.na(value)]
  return(dat)
}

#' Read CTDQC data
#'
#' Extracts in a nice data.table format the nested oce CTD casts from a CTDQC session
#'
#' @param session i.e. that loaded with load("CTDQC.rdata")
#' @param type  either "data" for the trimmed downcasts or "untrimmed" for everything
#'
#' @return data.table of all CTDs
#' @export
read.CTDQC <- function(session, type = c("data", "untrimmed")){
  m = list()
  for(i in session[[type[1]]]){
    stn = i@metadata$station
    m[[stn]] = as.data.table(i@data)
    m[[stn]][, startTime := i@metadata$startTime]
    fname = strsplit(i@metadata$filename, "\\\\")[[1]]
    fname = fname[[length(fname)]]
    m[[stn]][, filename := fname]
  }
  return(rbindlist(m, idcol="stn", fill=T))
}

#' read and parse NMEA GGA log file
#'
#' Tool will parse a NMEA log file, and decode each line with a "$xxGGA" string
#' Note that GGA does not contain the date, only UTC time, so a date origin must be supplied for each file.
#' If your log file spans multiple days, make sure you handle the roll-over at midnight!
#'
#' This tool also ignores any prefix to the $xxGGA string start, so preceding Teraterm timestamps are ignored.
#'
#' @param file filename
#' @param date_origin character string containing the date origin in "YYYY-MM-DD"
#' @param only_good if True (default) discard lines with bad GPS fixes
#'
#' @return data.table containing, gpsTime, lat, lon, number of satelites, HDOP (meters) and altitude (meters)
#' @export
#'
#' @examples
#' read.NMEA_GGA("teraterm.log", "2020-01-01")
read.NMEA_GGA <- function(file, date_origin = "2000-01-01", only_good = T){
  ln = readLines(file)
  ln = ln[grepl("\\w\\wGGA", ln)]
  GGA = stringr::str_extract_all(ln, "\\$..GGA[\\w\\d,.\\*]+")
  GGA = data.table(stringr::str_split_fixed(GGA, ",", 15))
  GGA[, gpsTime := as.POSIXct(paste(date_origin, V2), format = "%Y-%m-%d %H%M%S", origin = , tz = "UTC")]
  GGA[, lat := convert_latlong(stringr::str_sub(V3,1,2), stringr::str_sub(V3, -8, -1), polarity = V4)]
  GGA[, lon := convert_latlong(stringr::str_sub(V5,1,3), stringr::str_sub(V5, -8, -1), polarity = V6)]
  GGA[, fix := V7]
  GGA[, sat := as.numeric(V8)]
  GGA[, HDOP := as.numeric(V9)]
  GGA[, alt := as.numeric(V10)]
  if(only_good){
    GGA = GGA[fix != 0 & sat != "0"]
    return(GGA[,.(gpsTime, lat, lon, sat, HDOP, alt)])
  }else{
    return(GGA[,.(gpsTime, lat, lon, sat, HDOP, alt, fix)])
  }
}

#' Read ESMx Burst files
#'
#' @param file
#'
#' @return data.table
#' @importFrom stringr str_extract
#' @export
#'
#' @examples
#' flist = list.files(pattern = "Burst*") # get the files e.g. "1001_Burst 2.csv"
#' d = lapply(flist, read.ESMxburst)
#' d = rbindlist(d)[order(dateTime)]
read.ESMxburst <- function(file){
  # read ESMx converted bin (burst)
  d = list()
  ln = readLines(file)
  start_ind = which(grepl("Burst", ln))
  end_ind = c(start_ind[-1]-1, length(ln))
  burst_num = as.numeric(stringr::str_extract(ln[start_ind[1]], "(?<=Burst )(\\d+)"))
  for(i in 1:length(start_ind)){
    print(paste("processing...", varname, i,"line", start_ind[i]))
    varname = ln[start_ind[i]]
    sensor = stringr::str_extract(varname, "(?<=- )\\w+")
    if(end_ind[i] - start_ind[i] < 1){
      warning(paste("No data for "), file, varname)
    }else{
      DT = fread(text = ln[(start_ind[i]+1):end_ind[i]], header = F, tz = "UTC")
      DT = process_ESMx_sensor(DT, sensor)
      DT[, burst := burst_num]
      d[[sensor]] = DT
    }
  }
  d = rbindlist(d, fill = T)
  setcolorder(d, c("dateTime", "burst", "sensor", "variable", "value", "unit"))
  return(d)
}

process_ESMx_sensor <- function(DT, varname){
  if(varname == "Turbidity"){
    colnames(DT) = c("dateTime", "gain", "raw")
    DT[, c("raw", "gain") := list(as.numeric(raw), as.numeric(gain))]
    DT[, raw := as.numeric(raw)]
    DT = merge(DT, data.table(gain = c(0, 1, 2, 3),
                              factor = c(500, 100, 25, 5)))
    DT[, volts := (raw / (2^12)) * 5] # convert ADC to volts
    DT[, value := (volts) * factor] # apply gain
    DT = DT[,.(dateTime, variable = "FTU", value, unit = "FTU", sensor = "Seapoint_turbidity")]
    return(DT)
  }
  if(varname == "Fluorometer"){
    colnames(DT) = c("dateTime", "gain", "raw")
    DT[, c("raw", "gain") := list(as.numeric(raw), as.numeric(gain))]
    DT[, raw := as.numeric(raw)]
    DT = merge(DT, data.table(gain = c(0, 1, 2, 3),
                              factor = c(30, 10, 3, 1)))
    DT[, volts := (raw / (2^12)) * 5] # convert ADC to volts
    DT[, value := (volts) * factor] # apply gain
    DT = DT[,.(dateTime, variable = "FLUORS", value, unit = "", sensor = "Seapoint_chl")]
    return(DT)
  }
  if(varname == "Light"){
    factor = 0.1; offset = 3.45
    colnames(DT) = c("dateTime", "raw")
    DT[, raw := as.numeric(raw)]
    DT[, value := (raw / (2^12)) * 5] # convert ADC to volts
    # DT[, value := factor * exp(offset * raw)]
    DT = DT[,.(dateTime, variable = "PAR", value, unit = "uMol m^-2 s^-1", sensor = "LiCor_par")]
    return(DT)
  }
  if(varname == "Aanderaa"){
    colnames(DT) = c("dateTime", "O2CONC_raw", "O2SAT_raw", "TEMP")
    DT = melt(DT, id.var = "dateTime")
    DT[, value := as.numeric(value)]
    DT = merge(DT, data.table(variable = c("O2CONC_raw", "TEMP", "O2SAT_raw"),
                              unit = c("mmol m^-3", "oC", "%")))
    DT[, sensor := "Aanderaa_Optode"]
    return(DT)
  }
  if(varname == "Battery"){
    colnames(DT) = c("dateTime", "BATT1", "BATT2")
    DT = melt(DT, id.var = "dateTime")
    DT[, value := as.numeric(value)]
    # DT[, value := (value / (2^12)) * 5] # convert ADC to volts
    DT[, c("unit", "sensor") := list("ADC", "Battery")]
    return(DT)
  }
  if(varname == "Roll"){
    colnames(DT) = c("dateTime", "x", "y", "z")
    DT = melt(DT, id.var = "dateTime")
    DT[, value := as.numeric(value)]
    DT[, c("unit", "sensor") := list("m/s^2", "Accelerometer")]
    return(DT)
  }
  if(varname == "RBR"){
    colnames(DT) = c("dateTime", "COND", "TEMP", "PRS", "SAL")
    DT = melt(DT, id.var = "dateTime")
    DT[, value := as.numeric(value)]
    DT = merge(DT, data.table(variable = c("COND", "TEMP", "PRS", "SAL"),
                              unit = c("mS/cm", "oC", "dbar", "")))
    DT[, sensor := "RBR_CT"]
    return(DT)
  }
}

#' Read teraterm datetime log
#'
#' Teraterm has a timestamp option, however it's awkward to parse, this function takes a log file created by teraterm,
#' reads it as a table and extracts the dateTime column.
#'
#' You will need to clean the file to remove commands or badly formatted rows.
#'
#' @param f filename
#' @importFrom stringr str_extract
#'
#' @return data.table
#' @export
read.teraterm_log <- function(f){
  x = fread(f, header = F)
  x[, dateTime := stringr::str_extract(V1, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d+(.\\d)+")]
  x[, dateTime := as.POSIXct(dateTime, tz = "UTC")]
  x[, V1 := stringr::str_remove(V1, "\\[.+\\] ")]
  setcolorder(x, "dateTime")
  return(x)
}
