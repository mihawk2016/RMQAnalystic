



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


