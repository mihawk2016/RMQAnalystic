#### NOTE ####
# 2016-11-24: Create

require(compiler)


price.data.required <- cmpfun(function(symbols, from, to, timeframe, db = c('mysql.old', 'mysql.new', 'influxdb')) {
  # ''' get need price data '''
  # 2016-08-23: TESTING
  if (length(db) > 1) {
    db <- 'mysql.old'
  }
  prices <- tryCatch(get('PRICES', envir = .GlobalEnv), error = function(e) 'No Data')
                     if(prices == 'No Data') {
    prices <- list()
  }
  timeframe.prices <- prices[[timeframe]]
  if(is.null(timeframe.prices)) {
    timeframe.prices <- list()
  }
  load.prices <- lapply(symbols, load.price.data.from.local_db, from = from, to = to, timeframe = timeframe, local.data.list = timeframe.prices, db = db)
  required.prices <- lapply(load.prices, cmpfun(function(data.list) {
    merge.data <- rbind(timeframe.prices[[data.list$Symbol]], data.list$New.Data)
    merge.data <- merge.data[!duplicated(index(merge.data))]
    timeframe.prices[[data.list$Symbol]] <<- merge.data
    data.list$Required.Data
  }))
  prices[[timeframe]] <- timeframe.prices
  assign('PRICES', prices, envir = .GlobalEnv)
  names(required.prices) <- symbols
  save('PRICES', file = './_Datas/PRICES.rdata', envir = .GlobalEnv)
  required.prices
})# 2016-08-23: TESTING

load.price.data.from.local_db <- cmpfun(function(symbol, from, to, timeframe, local.data.list, db = c('mysql.old', 'mysql.new', 'influxdb')) {
  # ''' load price data from.local & db '''
  # 2016-08-23: Vectorize
  # 2016-08-23: Working
  local.data <- load.price.data.from.local(symbol, from, to, timeframe, local.data.list)
  if (is.null(local.data)) {
    required.data <- db.data <- load.price.data.from.db(symbol, from, to, timeframe, db)
  } else {
    required.data <- local.data
    db.data <- NULL
  }
  timeframe_secs <- timeframe.secs(timeframe)
  # print('111')
  # print(from)
  # print(to)
  # if(difftime(index(required.data)[1], from, units = 'secs') >= timeframe_secs) {
  #   db.data <- rbind(load.price.data.from.db(symbol, from, index(required.data)[1], timeframe, db), db.data)
  #   required.data <- rbind(required.data, db.data)
  # }
  # print('111')
  # if(difftime(to, tail(index(required.data), 1), units = 'secs') >= timeframe_secs) {
  #   db.data <- rbind(load.price.data.from.db(symbol, tail(index(required.data), 1), to, timeframe, db), db.data)
  #   required.data <- rbind(required.data, db.data)
  # }
  list(
    Required.Data = required.data[!duplicated(index(required.data))],
    New.Data = db.data,
    Symbol = symbol
  )
})# 2016-08-23: Working

load.price.data.from.local <- cmpfun(function(symbol, from, to, timeframe, local.data.list) {
  # ''' load price data from local '''
  # 2016-08-23: TESTING
  symbol.data <- local.data.list[[symbol]]
  if(is.null(symbol.data)) {
    return(NULL)
  }
  symbol.data[paste(from, to, sep = '/')]
})# 2016-08-23: TESTING

load.price.data.from.db <- cmpfun(function(symbol, from, to, timeframe, db = c('mysql.old', 'mysql.new', 'influxdb')) {
  # ''' load price data from db '''
  # 2016-08-23: Working
  if (length(db) > 1) {
    db <- 'mysql.old'
  }
  switch (db,
          'mysql.old' = load.price.data.from.db.mysql.old(symbol, from, to, timeframe),
          'mysql.new' = load.price.data.from.db.mysql.new(symbol, from, to, timeframe),
          'influxdb' = load.price.data.from.db.influxdb(symbol, from, to, timeframe)
  )
})

price.data.open <- cmpfun(function(symbol, time, timeframe = 'M1', db = c('mysql.old', 'mysql.new', 'influxdb')) {
  # ''' price data open '''
  # 2016-08-23: TESTING
  if (length(db) > 1) {
    db <- 'mysql.old'
  }
  switch (db,
    'mysql.old' = db.mysql.old.query.price.data.open(symbol, time, timeframe),
    'mysql.new' = db.mysql.new.query.price.data.open(symbol, time, timeframe),
    'influxdb' = db.influxdb.query.price.data.open(symbol, time, timeframe)
  )
})# 2016-08-23: TESTING

#### mysql ####

db.mysql.query <- cmpfun(function(sql, host = '192.168.1.9', port = 3306, username = 'mtk_bj_db', password = 'mtk_bj_db', dbname = 'auto_history') {
  # ''' mysql query '''
  # 2016-08-23: TESTING
  sql.len <- length(sql)
  if (sql.len == 0) return(NULL)
  mysql.connect <- dbConnect(MySQL(), host = host, port = port, username = username, password = password, dbname = dbname)
  res <- lapply(sql, function(s) {
    dbGetQuery(mysql.connect, s)
  })
  dbDisconnect(mysql.connect)
  res
})# 2016-08-23: TESTING

#### mysql.old ####

db.mysql.old.query.price.data.open <- cmpfun(function(symbol, time, timeframe = 'M1') {
  # ''' old mysql query: open '''
  # 2016-08-23: TESTING
  char.time <- as.character(reform.time(time))
  nchar.time <- nchar(char.time)
  only.date.index <- which(nchar.time == 10)
  if (length(only.date.index) > 0) {
    char.time[only.date.index] <- paste(char.time[only.date.index], '00:00:00')
  }
  reformed.time <- strsplit(char.time, ' ')
  date <- gsub('-', '.', sapply(reformed.time, function(x) x[1]))
  time <- sapply(reformed.time, function(x) x[2])
  if (any(is.na(time))) return(NULL)
  sql.string <- "SELECT open_price FROM mt4_currency_data WHERE currency_name = '%s' AND period = '%s' AND currency_date >= '%s' AND currency_time >= '%s' limit 1"
  sql <- sprintf(sql.string, symbol, timeframe, date, time)
  query.result <- db.mysql.query(sql, host = '192.168.1.9', port = 3306, username = 'mtk_bj_db', password = 'mtk_bj_db', dbname = 'auto_history')
  do.call(rbind, query.result)$open_price
})# 2016-08-23: TESTING

load.price.data.from.db.mysql.old <- cmpfun(function(symbol, from, to, timeframe) {
  # ''' load price data from db: mysql.old to xts '''
  # 2016-08-23: Working
  from <- gsub('-', '.', as.character(as.Date(reform.time(from) - 86400)))
  to <- gsub('-', '.', as.character(as.Date(reform.time(to) + 86400)))
  sql.string <- "SELECT currency_date, currency_time, open_price, high_price, low_price, close_price FROM mt4_currency_data WHERE currency_name = '%s' AND period = '%s' AND currency_date BETWEEN '%s' AND '%s'"
  sql <- sprintf(sql.string, symbol, timeframe, from, to)
  query.result <- db.mysql.query(sql, host = '192.168.1.9', port = 3306, username = 'mtk_bj_db', password = 'mtk_bj_db', dbname = 'auto_history')[[1]]
  time <- as.POSIXct(strptime(with(query.result, paste(as.character(currency_date), as.character(currency_time))), '%Y.%m.%d %H:%M', tz = 'GMT'))
  price <- xts(query.result[3:6], time)
  colnames(price) <- c('Open', 'High', 'Low', 'Close')
  price
})# 2016-08-23: Working

#### mysql.new ####

db.mysql.new.query.price.data.open <- cmpfun(function(symbol, time, timeframe = 'M1') {
  # ''' new mysql query: open '''
  # 2016-08-23: TESTING
  time <- as.numeric(reform.time(time))
  if (any(is.na(time))) return(NULL)
  sql.string <- "SELECT open FROM mt4_charts WHERE symbol = '%s' AND mode = '%s' AND lasttime >= %i limit 1"
  sql <- sprintf(sql.string, symbol, timeframe, time)
  query.result <- db.mysql.query(sql, host = '192.168.1.9', port = 3306, username = 'mtk_bj_db', password = 'mtk_bj_db', dbname = 'auto_history')
  as.numeric(do.call(rbind, query.result)$open)
})# 2016-08-23: TESTING

load.price.data.from.db.mysql.new <- cmpfun(function(symbol, from, to, timeframe) {
  # ''' load price data from db: mysql.new to xts '''
  # 2016-08-23: Working
  from <- as.numeric(reform.time(from))
  to <- as.numeric(reform.time(to))
  sql.string <- "SELECT open, high, low, close, lasttime FROM mt4_charts WHERE symbol = '%s' AND mode = '%s' AND lasttime BETWEEN %i AND %i"
  sql <- sprintf(sql.string, symbol, timeframe, from, to)
  query.result <- db.mysql.query(sql, host = '192.168.1.9', port = 3306, username = 'mtk_bj_db', password = 'mtk_bj_db', dbname = 'auto_history')[[1]]
  query.result <- apply(query.result, 2, as.numeric)
  time <- reform.time(query.result[, 5])
  price <- xts(query.result[, 1:4], time)
  colnames(price) <- c('Open', 'High', 'Low', 'Close')
  price
})# 2016-08-23: Working

#### influxDB ####

db.influxdb.query.price.data.open <- cmpfun(function(symbol, time, timeframe = 'M1') {
  # ''' influxdb query: open '''
  # 2016-08-23: TESTING
  time <- reform.time(time) - 8 * 3600
  sql.string <- "SELECT open FROM %s_bar WHERE timeframe = '%s' AND time >= '%s' limit 1"
  sql <- sprintf(sql.string, tolower(symbol), tolower(timeframe), time)
  res <- db.influxdb.query(sql)
  res[[1]]$series[[1]]$values[[1]][[2]]
})# 2016-08-23: TESTING

load.price.data.from.db.influxdb <- cmpfun(function(symbol, from, to, timeframe) {
  # ''' load price data from db: influxdb to xts '''
  # 2016-08-23: Working
  from <- reform.time(from)
  to <- reform.time(to)
  sql.string <- "SELECT open, high, low, close FROM %s_bar WHERE timeframe = '%s' AND time >= '%s' AND time <= '%s'"
  sql <- sprintf(sql.string, tolower(symbol), tolower(timeframe), from, to)
  res <- db.influxdb.query(sql)
  series <- res[[1]]$series[[1]]
  serie.values <- lapply(series$values, cmpfun(function(x) {
    data.frame(
      stringsAsFactors = F,
      x[[1]],
      Open = x[[2]],
      High = x[[3]],
      Low = x[[4]],
      Close = x[[5]]
    )
  }))
  serie.values <- do.call(rbind, serie.values)
  time <- as.POSIXct(strptime(serie.values[, 1], '%Y-%m-%dT%H:%M:%SZ', tz = 'GMT')) + 8 * 3600
  xts(serie.values[, -1], time)
})# 2016-08-23: Working

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


