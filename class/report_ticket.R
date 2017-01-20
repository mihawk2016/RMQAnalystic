require(R6)
require(compiler)


#### DEFINES ####

TICKETS_COLUMNS = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
                    'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', 'GROUP', 'COMMENT')#, 'EXIT')

TICKETS_GROUP = c('Money', 'Closed', 'Open', 'Pending', 'Working')

TICKETS_GROUP_COLUMNS = list(
  'Money' = c('TICKET', 'OTIME', 'PROFIT'),
  'Closed' = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
               'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT'),
  'Open' = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
             'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT'),
  'Pending' = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
                'CTIME', 'CPRICE'),
  'Working' = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP', 'CPRICE')
)
names(TICKETS_GROUP_COLUMNS) <- TICKETS_GROUP

#### REPORT TICKETS ####



MetaQuote.ReportTickets <- R6Class(
  classname = 'MetaQuote Report Tickets',
  public = list(


    get.tickets = function() {
      private$m.tickets
    },
    set.tickets = function(tickets) {
      private$m.tickets <- tickets
    },
    add.tickets = function(tickets) {
      new.tickets <- rbind(self$get.tickets(), tickets)
      self$set.tickets(new.tickets)
    },
    add.table = function(table, group) {
      self$add.tickets(private$build.group.tickets(table, group))
    }
  ),

  private = list(
    m.tickets = NULL,
    m.columns.uniform = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
                          'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', 'GROUP', 'COMMENT'),
    m.columns.money = c('TICKET', 'OTIME', 'PROFIT'),
    m.columns.closed = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
                         'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT'),
    m.columns.open = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
                       'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT'),
    m.columns.pending = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
                          'CTIME', 'CPRICE'),
    m.columns.working = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP', 'CPRICE'),
    

    build.group.tickets = function(table, group) {
      .build.tickets.group(table, private$get.group.columns(group), group, private$m.columns.uniform)
    },
    get.group.columns = function(group) {
      switch(
        group,
        Money = private$m.columns.money,
        Closed = private$m.columns.closed,
        Open = private$m.columns.open,
        Pending = private$m.columns.pending,
        Working = private$m.columns.working,
        NULL
      )
    }
    # .sort.dataframe <- cmpfun(function(dataframe, columns, decreasing = F) {
    #   # ''' sort dataframe with columns '''
    #   # 2016-08-15: Done
    #   dataframe[order(dataframe[, columns], decreasing = decreasing), ]
    # })# FINISH
  )
)

.build.tickets.group = cmpfun(function(table, columns, group, uniform.columns) {
  # ''' build tickets group '''
  # 2017-01-17: Version 0.2 add Comment
  # 2017-01-17: Version 0.1
  if (is.null(table)) {
    return(NULL) 
  }
  if (nrow(table) == 0) {
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
})

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

#### + REPORT TICKETS GROUP ####

MetaQuote.ReportTickets.Group <- R6Class(
  classname = 'MetaQuote Report Tickets Group',
  public = list(
    
  ),
  private = list(
    # m.original = NULL
    
  )
)

#### ++ REPORT TICKETS GROUP - MONEY ####

MetaQuote.ReportTickets.Money <- R6Class(
  classname = 'MetaQuote Report Tickets Group - Money',
  public = list(
    
  ),
  private = list(
    m.columns = c('Tickets')
    
  )
)

# .build.report.tickets.money.from.columns <- cmpfun(function(ticket, otime, profit, comment = '') {
#   # ''' build report tickets: money, from columns '''
#   # 2016-08-16: Done
#   .build.report.tickets(
#     ticket = ticket,
#     otime = otime,
#     profit = profit,
#     group = 'MONEY',
#     comment = comment
#   )
# })# FINISH








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
})

.format.html.mt4.trade.time <- cmpfun(function(time) {
  # ''' format html mt4 trade time '''
  # 2017-01-16: Version 0.1
  local_time <- Sys.getlocale('LC_TIME')
  Sys.setlocale('LC_TIME', 'us')
  new_time <- as.POSIXct(time, '%Y %b %d, %H:%M', tz = 'GMT')
  Sys.setlocale('LC_TIME', local_time)
  new_time
})