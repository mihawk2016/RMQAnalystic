


reform.time <- cmpfun(function(time) {
  # ''' reform report info: time '''
  # 2016-08-16: Done
  # 2016-08-16: ToDo: if (grepl(',', time[1])) only for first element!
  if (length(time) > 1) {
    return(as.POSIXct(sapply(time, reform.time), origin = '1970-01-01', tz = 'GMT'))
  }
  if (all(class(time) == c("POSIXct", "POSIXt"))) {
    return(time)
  }
  if (is.character(time)) {
    if (grepl(',', time[1])) {
      return(reform.mt4.report.info.time(time))
    }
    time <- gsub('-', '.', time[1])
    format <- '%Y.%m.%d %H:%M:%S'
    sub_format <- substr(format, 1, nchar(time) - 2)
    return(as.POSIXct(time, format = sub_format, tz = 'GMT'))
  }
  if (is.numeric(time)) {
    return(as.POSIXct(time, origin = '1970-01-01', tz = 'GMT'))
  }
  NA
})# 2016-08-16: ToDo

reform.comments <- cmpfun(function(comments) {
  # ''' reform the comments '''
  # 2016-08-16: Done
  # 2016-08-16: ToDo: add for Credit etc.
  # 2016-08-17: Add: for type 's/l' & 's / l
  # 2016-08-17: ToDo: for 'close at stop' check
  comments <- toupper(comments)
  comments <- gsub('/| / ', '', comments)
  new.comments <- vector(mode = 'character', length = length(comments))
  new.comments[grep('SO', comments)] <- 'SO'
  new.comments[grep('SL', comments)] <- 'SL'
  new.comments[grep('TP', comments)] <- 'TP'
  new.comments
})# 2016-08-16: ToDo

reform.mt4.report.info.time <- cmpfun(function(time) {
  # ''' reform mt4 report info time '''
  # 2016-08-16: Done
  local_time <- Sys.getlocale('LC_TIME')
  Sys.setlocale('LC_TIME', 'us')
  new_time <- as.POSIXct(time, '%Y %b %d, %H:%M', tz = 'GMT')
  Sys.setlocale('LC_TIME', local_time)
  new_time
})# 2016-08-16: Done

