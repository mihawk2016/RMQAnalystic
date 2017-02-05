#### NOTE ####
# 2016-11-24: Create

require(compiler)

#### read file ####



reform.report.tickets.number <- cmpfun(function(number) {
  # ''' reform report tickets column: number '''
  # 2016-08-16: Done
  if (is.numeric(number)) {
    return(number)
  }
  if (is.character(number)) {
    match1 <- regexpr('[[:digit:]]+.[[:digit:]]+', number)
    match1.index <- which(match1 > 0)
    number[match1.index] <- substr(number[match1.index], match1[match1.index], attr(match1, 'match.length')[match1.index] + match1[match1.index] - 1)
    number[number == ''] <- '0'
    return(as.numeric(number))
  }
  NA
})# 2016-08-16: Done





db.influxdb.query <- cmpfun(function(query, host = '192.168.1.155', port = 8086, username = 'strategy', password = 'strategy', database = 'auto_history', time_precision=c("s", "m", "u")) {
  # ''' influxdb query '''
  # 2016-08-23: TESTING
  response <- GET(
    "",
    scheme = "http",
    hostname = host,
    port = port,
    path = sprintf("query"),
    query = list(
      db = database,
      u = username,
      p = password,
      q = query,
      time_precision = match.arg(time_precision),
      chunked = "false"
    )
  )
  if (response$status_code < 200 | response$status_code >= 300) {
    if (length(response$content) > 0) {
      warning(rawToChar(response$content))
    }
    stop("Influx query failed with HTTP status code ", response$status_code)
  }
  response_data <- fromJSON(rawToChar(response$content))
  response_data$results
})# 2016-08-23: TESTING

#### Utils ####

timeframe.secs <- cmpfun(function(timeframe) {
  # ''' time frame secs '''
  # 2016-08-23: Done
  timeframe.mapping <- c('M1' = 60, 'M5' = 300, 'M15' = 900, 'M30' = 1800, 'H1' = 3600, 'H4' = 14400, 'D1' = 86400)
  timeframe.mapping[timeframe]
})# 2016-08-23: Done


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

reform.mt4.report.info.time <- cmpfun(function(time) {
  # ''' reform mt4 report info time '''
  # 2016-08-16: Done
  local_time <- Sys.getlocale('LC_TIME')
  Sys.setlocale('LC_TIME', 'us')
  new_time <- as.POSIXct(time, '%Y %b %d, %H:%M', tz = 'GMT')
  Sys.setlocale('LC_TIME', local_time)
  new_time
})# 2016-08-16: Done

