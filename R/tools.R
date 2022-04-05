#' Clearwater data converter and QA1
#'
#' @param file csv file from Clearview software
#' @param parcode string matching the SmartBuoy database parameter code, currently only "TOXN"
#' @param return_excel if FALSE (default) returns data.table, if True writes an excel file.
#'
#' This tool converts the output from the Clearwater software,
#' applies a simple in-out of range check to the data (QA level 1) and returns a data.table or optionally a excel file ready for upload
#' the ranges applied are:
#'
#' 1, Long 80mm (range 0.1-20 uM)
#' 2, Medium 10 mm (range < 100 uM)
#' 3, Short 1mm (range < 1000 uM)
#'
#' The function then selects the best range.
#'
#' @return Either a data.table or a string if a excel file is written
#' @export
#'
#' @examples
#' convert.clearwater("WGAB2_CWN54/WESTGAB2_000_CWN54.csv")
convert.clearwater <- function(file, parcode = "TOXN", return_excel = FALSE){
  x = fread(file)
  x[, dateTime := as.POSIXct(`Time (sensor)`, format = "%d/%m/%Y %H:%M:%S", tz="UTC")]
  # remove unused columns
  x[, c("Depth (m)", "S (PSU)", "T (degC)", "Time (sensor)", "Time (platform)") := NULL]
  setnames(x, c("Nitrate (Medium.uM)", "Nitrate (Short.uM)", "Nitrate (Long.uM)"),
           c("medium", "short", "long"))
  m = melt(x, id.vars = "dateTime") # convert to long format
  m[variable == "long", range := 1]
  m[variable == "medium", range := 2]
  m[variable == "short", range := 3]

  # ---- Automated QC (per channel)
  # 1, Long 80mm (range 0.1-20 uM)
  # 2, Medium 10 mm (range < 100 uM)
  # 3, Short 1mm (range < 1000 uM)
  m[,flag := 0] # assume good to start
  m[value < 0.1, flag := 2] # flag under minimum
  m[range == 1 & value > 20, flag := 3] # over max for long channel
  m[range == 2 & value > 100, flag := 3] # over max for medium channel
  m[value > 1000, flag := 3] # flag over maximum
  m[flag == 0, best_range := min(range), by = dateTime]
  m[is.na(best_range), best_range := 3, by = dateTime] # if all flagged use long

  out = m[range == best_range,.(Parcode = parcode,
                                ResultTime = lubridate::round_date(dateTime, "minute"),
                                BurstNumber = 1:.N,
                                ResultMean = value,
                                ResultQuality = flag)]
  if(return_excel == TRUE){
    if (!requireNamespace("pkg", quietly = TRUE)) {
      stop("Package \"svDialogs\" and \"openxlsx\" must be installed to use this function.", call. = FALSE)
    }
    else{
      suggested_filename = strsplit(file, ".csv")[[1]]
      filename = svDialogs::dlgSave(default = paste0(suggested_filename, ".xlsx"))$res
      openxlsx::write.xlsx(out, file = filename)
      return(paste("file saved to", filename))
    }
  }
  else{
    return(out)
  }
}

