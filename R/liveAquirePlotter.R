liveAquirePlot <- function(file = dlgOpen(title = 'Open LiveAquire file...')$res, style = 'oaat', channel = 'all'){
    require(ggplot2)
    require(reshape2)
    require(data.table)
    require(svDialogs)
    require(stringr)
    
    x = fread(file)     # read in file
    x = x[,lapply(.SD, as.numeric)]     # convert all columns to numeric
    return(x)
    x = melt(x, id.vars = 'Seconds Elapsed')
    setnames(x, 'Seconds Elapsed', 'time')
    x[,depth := as.numeric(str_extract(variable, perl('\\d+.\\d(?=m)'))), by = variable]
    x[,serial := str_extract(variable, perl('\\d+(?=,)')), by = variable]
    x[,channel := unlist(strsplit(as.character(variable), split = ':', fixed = T))[1], by = variable]
    x[,subchannel := unlist(strsplit(as.character(channel), split = '.', fixed = T))[2], by = variable]
    x[,channel := unlist(strsplit(as.character(channel), split = '.', fixed = T))[1], by = variable]
    x[,variable := unlist(strsplit(as.character(variable), split = ' ', fixed = T))[2], by = variable]
    
    # p = ggplot(na.omit(x), aes(Seconds.Elapsed, value)) + geom_line(aes(color = variable)) + facet_grid(variable ~ .,scales='free')
}