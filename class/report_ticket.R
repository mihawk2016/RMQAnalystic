require(R6)
require(compiler)

TICKETS_COLUMNS = c(
  'TICKETS' = 'Tickets',
  'OTIME' = 'OTime',
  'TYPE' = 'Type',
  'VOLUME' = 'Volume',
  'ITEM' = 'Item',
  'OPRICE' = 'OPrice',
  'SL' = 'SL',
  'TP' = 'TP',
  'CTIME' = 'CTime',
  'CPRICE' = 'CPrice',
  'COMMISSION' = 'Commission',
  'TAXES' = 'Taxes',
  'SWAP' = 'Swap',
  'PROFIT' = 'Profit',
  'GROUP' = 'Group',
  'COMMENT' = 'Comment',
  'EXIT' = 'Exit'
)

TICKETS_GROUP = c(
  'MONEY' = 'Money',
  'CLOSED' = 'Closed',
  'OPEN' = 'Open',
  'PENDING' = 'Pending',
  'WORKING' = 'Working'
)

TICKETS_GROUP_COLUMNS = list(
  'Money' = TICKETS_COLUMNS[c('TICKETS', 'OTIME', 'PROFIT')],
  'CLOSED' = 'Closed',
  'OPEN' = 'Open',
  'PENDING' = 'Pending',
  'WORKING' = 'Working'
)
names(TICKETS_GROUP_COLUMNS) <- TICKETS_GROUP
#### REPORT TICKETS ####

MetaQuote.ReportTickets <- R6Class(
  classname = 'MetaQuote Report Tickets',
  public = list(
    
  ),
  private = list(
    m.original = NULL#,
    # build.group = function()
  )
)

.build.tickets.group = cmpfun(function(table, group, columns) {
  # ''' build tickets group '''
  # 2017-01-17: Version 0.2 add Comment & Exit check
  # 2017-01-17: Version 0.1
  table.columns <- colnames(table)
  table[TICKETS_COLUMNS['GROUP']] <- group
  ## default 0 check
  zero.columns <- columns[which(!(columns %in% table.columns))]
  table <- tryCatch(
    cbind(table, matrix(data = 0, ncol = zero.columns.length, dimnames = list(NULL, c(zero.columns)))),
    error = function(e) table
  )
  ## comment & exit
  if (TICKETS_COLUMNS['COMMENT'] %in% table.columns) {
    table[TICKETS_COLUMNS['EXIT']] <- .report.tickets.exit(table[TICKETS_COLUMNS['EXIT']])
  } else {
    table <- cbind(table, matrix(data = '', ncol = 2, dimnames = list(NULL, TICKETS_COLUMNS[c('COMMENT', 'EXIT')])))
  }
  group.tickets <- table[c(columns, TICKETS_COLUMNS[c('GROUP', 'COMMENT', 'EXIT')])]
  na.columns <- TICKETS_COLUMNS[which(!(TICKETS_COLUMNS %in% c(columns, TICKETS_COLUMNS[c('GROUP', 'COMMENT', 'EXIT')])))]
  na.columns.length <- length(na.columns)
  if (na.columns.length > 0) {
    group.tickets <- cbind(group.tickets, matrix(data = NA, ncol = na.columns.length, dimnames = list(NULL, c(na.columns))))
  }
  group.tickets
})

.report.tickets.exit <- cmpfun(function(comments) {
  # ''' get report tickets column: exit from comment'''
  # 2016-12-01: Version 1.0
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
    if (grepl(',', time)) {
      return(.format.html.mt4.trade.time(time))
    }
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