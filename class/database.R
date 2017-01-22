

require(R6)
require(compiler)
require(RMySQL)

#### DATA CENTER ####





#### DATA BASE MYSQL ####

DataBase.MySQL <- R6Class(
  classname = 'MySQL Data Base',
  public = list(
    initialize = function(host, port, username, password, dbname, local.file) {
      # ''' init '''
      # 2017-01-23: Version 1.0
      private$m.host <- host
      private$m.port <- port
      private$m.username <- username
      private$m.password <- password
      private$m.dbname <- dbname
    },# FINISH
    ## data base ##
    db.query = function(sql, host=private$m.host, port=private$m.port, username=private$m.username, password=private$m.password, dbname=private$m.dbname) {
      # ''' mysql query '''
      # 2017-01-22: Version 1.0
      if (length(sql) == 0) {
        return(NULL)
      }
      mysql.connect <- dbConnect(MySQL(), host, port, username, password, dbname)
      res <- lapply(sql, function(s) {
        dbGetQuery(mysql.connect, s)
      })
      dbDisconnect(mysql.connect)
      res
    },# FINISH
    db.get.open = function(symbol, time, timeframe='M1') {
      # ''' get open data from mysql database '''
      # 2017-01-22: Version 1.0
      table <- tolower(paste(symbol, timeframe, sep = '_'))
      time <- gsub('-', '.', substr(char.time, 1, 16))
      sql.string <- "SELECT open FROM %s WHERE time >= '%s' limit 1"
      sql <- sprintf(sql.string, table, time)
      query.result <- db.mysql.query(sql)
      unlist(query.result)
    },# FINISH
    db.get.ohlc = function(symbol, from, to, timeframe='H1') {
      # ''' get ohlc from mysql database '''
      # 2017-01-22: Version 1.0
      table <- tolower(paste(symbol, timeframe, sep = '_'))
      from <- gsub('-', '.', as.character(as.Date(reform.time(from))))
      to <- gsub('-', '.', as.character(as.Date(reform.time(to) + 1)))
      sql.string <- "SELECT time, open, high, low, close FROM %s WHERE time BETWEEN '%s' AND '%s'"
      sql <- sprintf(sql.string, table, from, to)
      query.result <- db.mysql.query(sql)[[1]]
      time <- as.POSIXct(strptime(with(query.result, time), '%Y.%m.%d %H:%M', tz = 'GMT'))
      price <- xts(query.result[2:5], time)
      colnames(price) <- c('Open', 'High', 'Low', 'Close')
      price
    } # FINISH
    
  ),
  private = list(
    m.host = NULL,
    m.port = NULL,
    m.username = NULL,
    m.password = NULL,
    m.dbname = NULL
  )
)

#### LOCAL DATA R ####

Local.Data.R <- R6Class(
  classname = 'Local Data RData',
  public = list(
    initialize = function(local.file) {
      private$m.local.file <- local.file
    },
    get.local.data = function() {
      if (is.null(private$m.local.data)) {
        self$set.local.data(self$load.local.file())
      }
      private$m.local.data
    },
    set.local.data = function(local.data) {
      private$m.local.data <- local.data
    },
    get.data = function(timeframe, symbol) {
      local.data <- self$get.local.data()
      ifelse(identical(local.data, NULL), NULL, local.data[[timeframe]][[symbol]])
      
    },
    set.data = function(timeframe, symbol, data) {
      if (is.null(private$m.local.data)) {
        private$m.local.data <- list()
      }
      if (!(timeframe %in% names(private$m.local.data))) {
        private$m.local.data[[timeframe]] <- list()
      }
      private$m.local.data[[timeframe]][[symbol]] <- data
    },
    load.local.file = function(file=private$m.local.file) {
      if (!file.exists(file)) {
        file.create(file)
        return(NULL)
      }
      tryCatch(
        get(load(file)),
        error = function(e) {
          message('Local Data File ERROR')
          NULL
        }
      )
    },
    save.local.file = function(file=private$m.local.file) {
      local.data <- self$get.local.data()
      tryCatch(
        save(local.data, file = file),
        error = function(e)  {
          message('Local Data File ERROR')
          NULL
        }
      )
    },
    local.get.open = function(symbol, time, timeframe='M1') {
      # ''' get open data from local data '''
      # 2017-01-22: Version 1.0
      local.data <- self$get.data(timeframe, symbol)
      if (is.null(local.data)) {
        return(NULL)
      }
      open.serie <- local.data$Open
      time.string <- paste0(gsub('[.]', '-', as.character(time)), '/')
      sapply(time.string, function(time) open.serie[time][1])
    },# FINISH
    local.get.ohlc = function(symbol, from, to, timeframe='H1') {
      # ''' get ohlc from local data '''
      # 2017-01-22: Version 1.0
      local.data <- self$get.data(timeframe, symbol)
      if (is.null(local.data)) {
        return(NULL)
      }
      from <- as.character(as.Date(from))
      to <- as.character(as.Date(to))
      local.data[paste(from, to, sep = '/')]
    },# FINISH
    import.csv.files = function(files.csv) {
      # ''' import csv files '''
      # 2017-01-22: Version 1.0
      lapply(files.csv, private$import.csv)
      self$save.local.file()
    } # FINISH
  ),
  private = list(
    m.local.file = NULL,
    m.local.data = NULL,
    
    read.data.csv = function(file.csv) {
      csv <- read.csv(file.csv, header = F)
      datetime <- strptime(paste(csv$V1, csv$V2), '%Y.%m.%d %H:%M', tz='GMT')
      xts.data <- xts(csv[, 3:7], datetime)
      colnames(xts.data) <- c('Open', 'High', 'Low', 'Close', 'Volume')
      xts.data
    },
    xts.deduplicate = function(xts) {
      xts[!duplicated(index(xts))] 
    },
    import.csv = function(file.csv) {
      local <- self$get.local.data()
      file.name <- toupper(tail(strsplit(file.csv, '/', fixed = T)[[1]], 1))
      split.file.name <- strsplit(file.name, '_')[[1]]
      symbol <- split.file.name[1]
      timeframe <- split.file.name[2]
      new.data <- private$read.data.csv(file.csv)
      old.data <- self$get.data(timeframe, symbol)
      merge.data <- private$xts.deduplicate(rbind(old.data, new.data))
      self$set.data(timeframe, symbol, merge.data)
    }
  )
)
