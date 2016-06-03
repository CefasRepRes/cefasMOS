# state table
# CONNECTING = while waiting for rxd
# MAIN:
# DIAG: = ulog.ini loaded and ready to start
# AQUIRE
# AQUIRING


esm.liveaquire <- function(com = "COM1"){
  # assumes logger is connected and ready for "l" to start liveaquire
  require(serial)
  com = "COM3"
  write.serialConnection(esm, "\r\n")
  # d
  # r ulog.ini
  # settings read ok

  start_liveaquire <- function(){
    if(esm.state == "MAIN"){
      print("mode switching to live aquire menu")
      write.serialConnection(esm, "d\r\n")
      write.serialConnection(esm, "r ulog.ini\r\n")
      a = read.serialConnection(esm)
      if(!grepl("settings read ok", a)){stop("ulog.ini not read correctly, aborting...")}
      esm.state <<- "DIAG"
    }
    if(esm.state == "DIAG"){
      print("    <starting liveaquire...")
      write.serialConnection(esm, "l\r\n")
    }else{
      stop("esm is not in main or aquire menu, please reset and establish comms.")
    }
  }

  parse_liveaquire <- function(string){
    string = a
    analog = unlist(stringr::str_extract_all(string, "\\d[ab]\\d{4}"))
    serial = unlist(stringr::str_extract_all(string, "\\d+sf[\\d\\.\\;]+"))
  }
  close(esm)
}


esm.establishcoms <- function(com = "COM1"){
  esm <<- serialConnection(name = "ESM2",
                         port = com,
                         mode = "9800,n,8,1")
  open(esm)
  esm.state <<- "CONNECTING"
  while(esm.state == "connecting"){
    print("    <waiting for esm to wake...>")
    a = read.serialConnection(esm)
    print(a)
    if(grepl("RXD is active, awaiting rxd inactive", a)){
      esm.state <<- "MAIN"
      print("    <ESM connected!>")
    }
    Sys.sleep(1)
  }
}

livePlot <- function(lag = 10){
  while(T){
    d = read.csv("livedata.csv")
    d$dateTime = as.POSIXct(d$dateTime, tz = "UTC")
    limx = max(d$dateTime) - (lag * 60)
    plot(d$dateTime, d$value)
    Sys.sleep(1)
  }
}

livedatagen <- function(){
  require(lubridate)
  while(T){
    # dat = paste0(now(tzone = "UTC"),",",rnorm(1))
    dat = rnorm(1)
    file("livedata.csv", "w")
    cat(dat)
    Sys.sleep(1)
  }
}
