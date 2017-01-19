
## 2017-01-20 废弃

MetaQuote.Tickets <- R6Class(
  classname = 'MetaQuote Tickets',
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
    add.table = function(table) {
      new.tickets <- .build.group.tickets(table, private$m.columns)
      self$add.tickets(new.tickets)
    }
  ),
  private = list(
    m.tickets = NULL,
    # m.uniform.columns = = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
    #                         'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', 'GROUP', 'COMMENT'),
    m.columns = NULL
  )
)

.build.group.tickets = cmpfun(function(table, columns, group) {
  # ''' build group tickets '''
  # 2017-01-17: Version 0.3 for OOP
  # 2017-01-17: Version 0.2 add Comment
  # 2017-01-17: Version 0.1
  if (is.null(table)) {
    return(NULL) 
  }
  if (nrow(table) == 0) {
    return(NULL)
  }
  table.columns <- colnames(table)
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
  group.columns <- c(columns, 'COMMENT')
  table[group.columns]
  within(
    table,
    GROUP <- group
  )
})

MetaQuote.Tickets.Money <- R6Class(
  classname = 'MetaQuote Tickets Money',
  inherit = MetaQuote.Tickets,
  public = list(
    
  ),
  private = list(
    m.type = 'Money',
    m.columns = c('TICKET', 'OTIME', 'PROFIT')
  )
)

MetaQuote.Tickets.Closed <- R6Class(
  classname = 'MetaQuote Tickets Closed',
  inherit = MetaQuote.Tickets,
  public = list(
    
  ),
  private = list(
    m.type = 'Closed',
    m.columns = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
                  'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT')
  )
)

MetaQuote.Tickets.Open <- R6Class(
  classname = 'MetaQuote Tickets Open',
  inherit = MetaQuote.Tickets,
  public = list(
    
  ),
  private = list(
    m.type = 'Open',
    m.columns = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
                  'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT')
  )
)

MetaQuote.Tickets.Pending <- R6Class(
  classname = 'MetaQuote Tickets Pending',
  inherit = MetaQuote.Tickets,
  public = list(
    
  ),
  private = list(
    m.type = 'Pending',
    m.columns = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
                  'CTIME', 'CPRICE')
  )
)

MetaQuote.Tickets.Working <- R6Class(
  classname = 'MetaQuote Tickets Working',
  inherit = MetaQuote.Tickets,
  public = list(
    
  ),
  private = list(
    m.type = 'Working',
    m.columns = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP', 'CPRICE')
  )
)