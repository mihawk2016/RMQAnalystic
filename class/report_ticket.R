require(R6)
require(compiler)

#### REPORT TICKETS ####

MetaQuote.ReportTickets <- R6Class(
  # ''' Report Tickets in many phase '''
  # 2017-01-21: Version 0.2: set 3 types of tickets
  classname = 'MetaQuote Report Tickets',
  public = list(


    get.tickets = function(type) {
      # ''' get tickets '''
      # 2017-01-21: Version 0.1
      switch(
        type,
        'raw' = private$m.tickets.raw,
        'support' = private$m.tickets.support
      )
      
    },
    set.tickets = function(type, tickets) {
      switch(
        type,
        'raw' = private$m.tickets.raw <- tickets,
        'support' = private$m.tickets.support <- tickets
      )
      
    },
    add.tickets = function(tickets) {
      new.tickets <- rbind(self$get.tickets('raw'), tickets)
      self$set.tickets('raw', new.tickets)
    },
    add.table = function(table, group, columns, uniform.columns) {
      self$add.tickets(private$build.group.tickets(table, group, columns, uniform.columns))
    },
    item.unique = function(tickets=private$m.tickets.raw) {
      # ''' get items of tickets '''
      # 2017-01-21: Version 1.1 use unique() for level - factor method
      unique(tickets$Item)
    } # FINISH
  ),

  private = list(
    m.tickets.raw = NULL,
    m.tickets.support = NULL,
    
    build.group.tickets = function(table, group, columns, uniform.columns) {
      .build.tickets.group(table, group, columns, uniform.columns)
    }
    # .sort.dataframe <- cmpfun(function(dataframe, columns, decreasing = F) {
    #   # ''' sort dataframe with columns '''
    #   # 2016-08-15: Done
    #   dataframe[order(dataframe[, columns], decreasing = decreasing), ]
    # })# FINISH
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
})# TESTING

.report.tickets.exit <- cmpfun(function(comments) {
  # ''' get report tickets column: exit from comment'''
  # 2017-01-17: Version 1.1 add support for comments type - data.frame
  # 2016-12-01: Version 1.0
  if (is.data.frame(comments)) {
    comments <- unlist(comments)
  }
  comments <- toupper(comments)
  comments <- gsub('/| / ', '', comments)
  exit <- vector(mode = 'character', length = length(comments))
  exit[grep('SO', comments)] <- 'SO'
  exit[grep('SL', comments)] <- 'SL'
  exit[grep('TP', comments)] <- 'TP'
  exit
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
