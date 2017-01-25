#### ToDo ####
## not inherit from datacenter, or Local.Data.R get methode: get.mysql & get.local from it
## add functions from Local.Data.R to DataCenter like import.csv.files
## add default open for symbol tickvalue and marginrequired calculation

require(R6)
require(compiler)
require(RMySQL)

#### DATA CENTER ####

DataCenter <- R6Class(
  classname = 'Data Center',
  public = list(
    initialize = function(host = '192.168.2.103', port = 3306, username = 'root', password = '', dbname = 'historical_data', local.file = './abc.rdata') {
      private$m.mysql <- DataBase.MySQL$new(host, port, username, password, dbname)
      private$m.local <- Local.Data.R$new(local.file)
    },
    get.mysql = function() {
      private$m.mysql
    },
    get.local = function() {
      private$m.local
    },
    get.open = function(symbol, time, timeframe='M1') {
      local.data <- self$get.local()$get.open(symbol, time, timeframe)
      local.open.na.index <- which(is.na(local.data))
      if(length(local.open.na.index) > 0) {
        local.na.time <- time[local.open.na.index]
        local.data[local.open.na.index] <- self$get.mysql()$get.open(symbol, local.na.time, timeframe)
        if (any(is.na(local.data))) {
          message('NA returns from DataCenter')
        }
      }
      local.data
    },
    get.ohlc = function(symbol, from, to, timeframe='H1') {
      local.data <- private$m.local$get.ohlc(symbol, from, to, timeframe)
      if (is.null(local.data)) {
        mysql.data <- private$m.mysql$get.ohlc(symbol, from, to, timeframe)
        private$m.local$import.xts(timeframe, symbol, mysql.data)
        return(mysql.data)
      }
      from <- .format.time(from)
      to <- .format.time(to)
      local.data.times <- index(local.data)
      first.local.time <- local.data.times[1]
      if (from < first.local.time) {
        mysql.data <- private$m.mysql$get.ohlc(symbol, from, first.local.time, timeframe)
        local.data <- private$xts.merge(mysql.data, local.data)
      }
      last.local.time <- tail(local.data.times, 1)
      if (to > last.local.time) {
        mysql.data <- private$m.mysql$get.ohlc(symbol, last.local.time, to, timeframe)
        local.data <- private$xts.merge(local.data, mysql.data)
      }
      new.times <- index(local.data)
      if (from < new.times[1] || to > tail(new.times, 1)) {
        message('NOT Enough Data from DataCenter')
      }
      private$m.local$import.xts(timeframe, symbol, local.data)
      local.data
    }
  ),
  private = list(
    m.mysql = NULL,
    m.local = NULL,
    
    xts.deduplicate = function(xts) {
      xts[!duplicated(index(xts))] 
    },
    xts.merge = function(xts1, xts2) {
      private$xts.deduplicate(rbind(xts1, xts2))
    }
    
  )
)



#### DATA BASE MYSQL ####

DataBase.MySQL <- R6Class(
  classname = 'MySQL Data Base',
  inherit = DataCenter,
  public = list(
    initialize = function(host = '192.168.2.103', port = 3306, username = 'root', password = '', dbname = 'historical_data') {
      # ''' init '''
      # 2017-01-23: Version 1.0
      private$m.host <- host
      private$m.port <- port
      private$m.username <- username
      private$m.password <- password
      private$m.dbname <- dbname
    },# FINISH
    ## data base ##
    get.open = function(symbol, time, timeframe='M1') {
      # ''' get open data from mysql database '''
      # 2017-01-22: Version 1.0
      table <- tolower(paste(symbol, timeframe, sep = '_'))
      time.time <- .format.time(time)
      max.time <- max(time.time)
      min.time <- min(time.time)
      from <- gsub('-', '.', as.character(as.Date(min.time)))
      to <- gsub('-', '.', as.character(as.Date(max.time) + 1))
      sql.string <- "SELECT time, open, high, low, close FROM %s WHERE time BETWEEN '%s' AND '%s'"
      sql <- sprintf(sql.string, table, from, to)
      query.result <- self$query(sql)[[1]]
      open.serie <- xts(query.result$open, as.POSIXct(strptime(with(query.result, time), '%Y.%m.%d %H:%M', tz = 'GMT')))
      time.string <- paste0('/', gsub('[.]', '-', as.character(time)))
      sapply(time.string, function(.time) {
        if (nchar(.time) == 11) {
          time <- paste(.time, '00:00')
        }
        res <- open.serie[.time]
        if (nrow(res) == 0) {
          return(NA)
        }
        res <- tail(res, 1)
        #### ToDo right now just use 7200 as threshold ####
        if (difftime(.format.time(gsub('/', '', .time)), index(res), units = 'secs') > 3600 * 2) {
          return(NA)
        }
        res
      })
    },# FINISH
    get.ohlc = function(symbol, from, to, timeframe='H1') {
      # ''' get ohlc from mysql database '''
      # 2017-01-22: Version 1.0
      table <- tolower(paste(symbol, timeframe, sep = '_'))
      from <- gsub('-', '.', as.character(as.Date(.format.time(from))))
      to <- gsub('-', '.', as.character(as.Date(.format.time(to)) + 1))
      sql.string <- "SELECT time, open, high, low, close FROM %s WHERE time BETWEEN '%s' AND '%s'"
      sql <- sprintf(sql.string, table, from, to)
      query.result <- self$query(sql)[[1]]
      time <- as.POSIXct(strptime(query.result$time, '%Y.%m.%d %H:%M', tz = 'GMT'))
      price <- xts(query.result[2:5], time)
      colnames(price) <- c('Open', 'High', 'Low', 'Close')
      price
    },# FINISH
    query = function(sql, host=private$m.host, port=private$m.port, username=private$m.username, password=private$m.password, dbname=private$m.dbname) {
      # ''' mysql query '''
      # 2017-01-22: Version 1.0
      if (length(sql) == 0) {
        return(NULL)
      }
      mysql.connect <- tryCatch(
        dbConnect(MySQL(), host = host, port = port, username = username, password = password, dbname = dbname),
        error = function(e) {
          message('MySQL Connect ERROR')
          NULL
        }
      )
      if (is.null(mysql.connect)) {
        return(NULL)
      }
      res <- lapply(sql, function(s) {
        dbGetQuery(mysql.connect, s)
      })
      dbDisconnect(mysql.connect)
      res
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
  inherit = DataCenter,
  public = list(
    initialize = function(local.file) {
      private$m.local.file <- local.file
    },
    get.open = function(symbol, time, timeframe='M1') {
      # ''' get open data from local data '''
      # 2017-01-22: Version 1.3 bug fix: check time, if too late for last data of date, return NA
      # 2017-01-22: Version 1.2 bug fix: add 00:00, get right result for Date format time
      # 2017-01-22: Version 1.1 NA value for check
      # 2017-01-22: Version 1.0
      local.data <- self$get.data(timeframe, symbol)
      if (is.null(local.data)) {
        return(NULL)
      }
      open.serie <- local.data$Open
      time.string <- paste0('/', gsub('[.]', '-', as.character(time)))
      sapply(time.string, function(.time) {
        if (nchar(.time) == 11) {
          time <- paste(.time, '00:00')
        }
        res <- open.serie[.time]
        if (nrow(res) == 0) {
          return(NA)
        }
        res <- tail(res, 1)
        #### ToDo right now just use 7200 as threshold ####
        if (difftime(.format.time(gsub('/', '', .time)), index(res), units = 'secs') > 3600 * 2) {
          return(NA)
        }
        res
      })
    },# FINISH
    get.ohlc = function(symbol, from, to, timeframe='H1') {
      # ''' get ohlc from local data '''
      # 2017-01-22: Version 1.0
      local.data <- self$get.data(timeframe, symbol)[, 1:4]
      if (is.null(local.data)) {
        return(NULL)
      }
      from <- as.character(as.Date(from))
      to <- as.character(as.Date(to))
      data <- local.data[paste(from, to, sep = '/')]
      if (nrow(data) == 0) {
        return(NULL)
      }
      data
    },# FINISH
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
      if (identical(local.data, NULL)) {
        return(NULL)
      }
      local.data[[timeframe]][[symbol]]
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
    import.csv.files = function(files.csv) {
      # ''' import csv files '''
      # 2017-01-22: Version 1.0
      lapply(files.csv, private$import.csv)
      self$save.local.file()
    },# FINISH
    import.xts = function(timeframe, symbol, xts) {
      old.data <- self$get.data(timeframe, symbol)
      merge.data <- private$xts.merge(old.data, xts)
      self$set.data(timeframe, symbol, merge.data)
      self$save.local.file()
    }
  ),
  private = list(
    m.local.file = NULL,
    m.local.data = NULL,
    
    read.data.csv = function(file.csv) {
      csv <- read.csv(file.csv, header = FALSE)
      datetime <- as.POSIXct(strptime(paste(csv$V1, csv$V2), '%Y.%m.%d %H:%M', tz = 'GMT'))
      xts.data <- xts(csv[, 3:7], datetime)
      colnames(xts.data) <- c('Open', 'High', 'Low', 'Close', 'Volume')
      xts.data
    },
    import.csv = function(file.csv) {
      local <- self$get.local.data()
      file.name <- toupper(tail(strsplit(file.csv, '/', fixed = T)[[1]], 1))
      split.file.name <- strsplit(file.name, '_')[[1]]
      symbol <- split.file.name[1]
      timeframe <- split.file.name[2]
      new.data <- private$read.data.csv(file.csv)
      old.data <- self$get.data(timeframe, symbol)
      merge.data <- private$xts.merge(old.data, new.data)
      self$set.data(timeframe, symbol, merge.data)
    }
  )
)
