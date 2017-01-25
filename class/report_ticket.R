require(R6)
require(compiler)

#### REPORT TICKETS ####

MetaQuote.ReportTickets <- R6Class(
  # ''' Report Tickets in many phase '''
  # 2017-01-21: Version 0.2: set 3 types of tickets
  classname = 'MetaQuote Report Tickets',
  #### + PUBLIC ####
  public = list(

    
    #### ++ Getter & Setter ####
    
    #### +++ tickets member ####
    get.tickets.member = function(tickets.type) {
      # ''' get tickets member '''
      # 2017-01-21: Version 0.1
      switch(
        tickets.type,
        'raw' = private$m.tickets.raw,
        'supported' = private$m.tickets.supported
      )
    },
    set.tickets.member = function(tickets.type, tickets) {
      # ''' set tickets member '''
      # 2017-01-21: Version 0.1
      switch(
        tickets.type,
        'raw' = private$m.tickets.raw <- tickets,
        'supported' = private$m.tickets.supported <- tickets
      )
    },
    
    
    
    #### ++ ACTION ####
    
    #### +++ raw tickets ####
    add.tickets = function(tickets.type='raw', tickets) {
      new.tickets <- rbind(self$get.tickets.member(tickets.type), tickets)
      self$set.tickets.member(tickets.type, new.tickets)
    },
    add.table = function(tickets.type='raw', table, group, columns, uniform.columns) {
      self$add.tickets(tickets.type, private$build.group.tickets(table, group, columns, uniform.columns))
    },
    
    #### +++ format tickets ####
    
    format.tickets = function(tickets.type='raw', overwrite=TRUE) {
      # ''' format tickets type '''
      # 2017-01-24: Version 0.1
      tickets <- self$get.tickets.member(tickets.type)
      tickets <- within(tickets, {
        TICKET <- private$format.number(TICKET)
        OTIME <- private$format.time(OTIME)
        TYPE <- private$format.string(TYPE)
        VOLUME <- private$format.number(VOLUME)
        ITEM <- private$format.string(ITEM)
        OPRICE <- private$format.number(OPRICE)
        SL <- private$format.number(SL)
        TP <- private$format.number(TP)
        CTIME <- private$format.time(CTIME)
        CPRICE <- private$format.number(CPRICE)
        COMMISSION <- private$format.money(COMMISSION)
        TAXES <- private$format.money(TAXES)
        SWAP <- private$format.money(SWAP)
        PROFIT <- private$format.money(PROFIT)
      })
      if (overwrite) {
        self$set.tickets.member(tickets.type, tickets)
      }
      tickets
    },
    sort.tickets = function(tickets.type='raw', column='OTIME', decreasing=F, overwrite=TRUE) {
      # ''' sort tickets type '''
      # 2016-08-15: Version 1.0
      tickets <- self$get.tickets.member(tickets.type)
      tickets <- tickets[order(tickets[[column]], decreasing = decreasing), ]
      if (overwrite) {
        self$set.tickets.member(tickets.type, tickets)
      }
      tickets
    },# FINISH
    
    
    get.unique.items = function(tickets=self$get.tickets.member('raw')) {
      # ''' get items of tickets '''
      # 2017-01-21: Version 1.1 use unique() for level - factor method
      unique(tickets$ITEM)
    } # FINISH
  ),
  
  #### + PRIVATE ####
  private = list(
    #### ++ MEMBER ####
    m.tickets.raw = NULL,
    m.tickets.supported = NULL,
    
    
    #### ++ build tickets ####
    build.group.tickets = function(table, group, columns, uniform.columns) {
      .build.tickets.group(table, group, columns, uniform.columns)
    },
    
    #### ++ format columns ####
    format.number = function(number) {
      # ''' format number column '''
      # 2016-08-16: Version 1.0
      if (is.numeric(number)) {
        return(number)
      }
      as.numeric(number)
    },# FINISH
    format.string = function(string) {
      # ''' format string column '''
      # 2016-08-16: Version 1.0
      toupper(string)
    },# FINISH
    format.money = function(money) {
      # ''' format money column '''
      # 2016-08-16: Version 1.0
      if (is.numeric(money)) {
        return(money)
      }
      private$format.number(gsub(' ', '', money))
    },# FINISH
    format.time = function(time) {
      # ''' format time column '''
      # 2016-08-16: Version 1.0
      if (all(class(time) == c("POSIXct", "POSIXt"))) {
        return(time)
      }
      if (is.numeric(time)) {
        return(as.POSIXct(time, origin = '1970-01-01', tz = 'GMT'))
      }
      if (is.character(time)) {
        time <- gsub('[.]', '-', time)
        only.date <- nchar(time) < 12
        if (any(only.date, na.rm = T)) {
          time[only.date] <- paste(time[only.date], '00:00')
        }
        return(as.POSIXct(time, tz = 'GMT'))
      }
      message('format time may cause error')
      NA
    },# FINISH
    cal.profit = function() {
      #### ToDo ####
      
    }
  )
)

.build.tickets.group = cmpfun(function(table, group, columns, uniform.columns) {
  # ''' build tickets group '''
  # 2017-01-21: Version 1.0 change logic expr to || short way connection
  # 2017-01-17: Version 0.2 add Comment
  # 2017-01-17: Version 0.1
  if (is.null(table) || nrow(table) == 0) {
    return(NULL) 
  }
  table.columns <- colnames(table)
  table$GROUP <- group
  ## default 0 check
  zero.columns <- columns[which(!(columns %in% table.columns))]
  table <- tryCatch(
    cbind(table, matrix(data = 0, ncol = length(zero.columns), dimnames = list(NULL, c(zero.columns)))),
    error = function(e) table
  )
  ## comment
  if (is.null(table$COMMENT)) {
    table$COMMENT <- ''
  }
  group.columns <- c(columns, c('GROUP', 'COMMENT'))
  table <- table[group.columns]
  na.columns <- uniform.columns[which(!(uniform.columns %in% group.columns))]
  ## NAs check
  tryCatch(
    cbind(table, matrix(data = NA, ncol = length(na.columns), dimnames = list(NULL, c(na.columns)))),
    error = function(e) table
  )
})# FINISH



.format.time <- cmpfun(function(time) {
  # ''' format time '''
  # 2017-01-16: Version 0.1
  if (all(class(time) == c('POSIXct', 'POSIXt'))) {
    return(time)
  }
  if (is.character(time)) {
    suppressWarnings(
      if (grepl(',', time)) {
        return(.format.html.mt4.trade.time(time))
      }
    )
    time <- gsub('-', '.', time)
    format <- '%Y.%m.%d %H:%M:%S'
    sub_format <- substr(format, 1, nchar(time) - 2)
    return(as.POSIXct(time, format = sub_format, tz = 'GMT'))
  }
  if (is.numeric(time)) {
    return(as.POSIXct(time, origin = '1970-01-01', tz = 'GMT'))
  }
  NA
})# TESTING

.format.html.mt4.trade.time <- cmpfun(function(time) {
  # ''' format html mt4 trade time '''
  # 2017-01-16: Version 0.1
  local_time <- Sys.getlocale('LC_TIME')
  Sys.setlocale('LC_TIME', 'us')
  new_time <- as.POSIXct(time, '%Y %b %d, %H:%M', tz = 'GMT')
  Sys.setlocale('LC_TIME', local_time)
  new_time
})# TESTING
