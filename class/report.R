require(R6)
require(compiler)

#### DEFINES ####

REPORT_TYPE <- c(
  'MT4.EA' = 'MT4-EA',
  'MT4.TRADE' = 'MT4-Trade',
  'MT5.EA' = 'MT5-EA',
  'MT5.TRADE' = 'MT5-Trade',
  'MT4M.CLOSED' = 'MT4M-Closed',
  'MT4M.RAW' = 'MT4M-Raw'
)


#### REPORT ####

MetaQuote.Report <- R6Class(
  classname = 'MetaQuote Report',
  public = list(
    initialize = function(file.path, file.name) {
      private$m.infos <- MetaQuote.ReportInfos$new()
      self$set.infos('file.path', file.path)
      self$set.infos('file', file.name)
      self$set.infos('type', private$m.type)
    },
    get.infos.dataframe = function() {
      private$m.infos$to.dataframe()
    },
    get.infos = function(member) {
      if (missing(member)) {
        return(private$m.infos)
      }
      private$m.infos$get(member)
    },
    set.infos = function(member, value) {
      if (missing(member)) {
        return(private$m.infos <- value)
      }
      private$m.infos$set(member, value)
    }
  ),
  private = list(
    m.infos = NULL,
    m.tickets = NULL
    
  )
)

#### + HTML REPORT : REPORT ####

MetaQuote.HTML.Report <- R6Class(
  classname = 'MetaQuote HTML Report',
  inherit = MetaQuote.Report,
  public = list(
    initialize = function(file.path, file.name) {
      super$initialize(file.path, file.name)
    }
  ),
  private = list(
    get.html.table = function(file.path=self$get.infos('file.path')) {
      # ''' get html table for tickets '''
      # 2017-01-18: Version 1.1 tryCatch for 2 type of encodings
      # 2017-01-17: Version 1.0
      tryCatch(
        readHTMLTable(file.path, stringsAsFactors = FALSE, encoding = 'UTF-8'),
        error = function(e) readHTMLTable(file.path, stringsAsFactors = FALSE)
      )
    }
  )
)



#### ++ HTML MT4 EA : HTML REPORT : REPORT ####

MetaQuote.HTML.MT4EA.Report <- R6Class(
  classname = 'MetaQuote HTML MT4 EA Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.path, file.name, html.parse) {
      super$initialize(file.path, file.name)
      private$m.html.parse <- html.parse
      private$init.infos(html.parse)
    },
    init.tickets = function() {
      # ''' init tickets ''' ###
      # 2017-01-18: Version 0.1
      html.table <- private$get.html.table()
      item <- private$get.tickets.item()
      tickets.table <- html.table[[2]]
      colnames(tickets.table) <- c('deal', 'time', 'type', 'tickets', 'volume', 'price', 'sl', 'tp', 'profit', 'balance')
      tickets <- subset(tickets.table, subset = type != 'modify', select = -c(deal, balance))
      ## pending tickets ##
      pending.tickets.close.part.index <- which(tickets$type == 'delete')
      if (length(pending.tickets.close.part.index) == 0) {
        tickets.pending.temp <- NULL
      } else {
        pending.tickets.close.part <- tickets[pending.tickets.close.part.index, ]
        pending.tickets.tickets <- pending.tickets.close.part$tickets
        tickets <- tickets[-pending.tickets.close.part.index, ]
        pending.tickets.open.part.index <- which(tickets$tickets %in% pending.tickets.tickets)
        pending.tickets.open.part <- tickets[pending.tickets.open.part.index, ]
        tickets <- tickets[-pending.tickets.open.part.index, ]
        tickets.pending.temp <- merge(pending.tickets.open.part, pending.tickets.close.part, by = 'tickets')
        colnames(tickets.pending.temp) <- TICKETS_COLUMNS[c('TICKETS', 'OTIME', 'TYPE', '', 'OPRICE', '', '', '',
                                                            'CTIME', '', 'VOLUME', 'CPRICE', 'SL', 'TP', 'PROFIT')]
        tickets.pending.temp[TICKETS_COLUMNS['ITEM']] <- item
        tickets.pending.temp[TICKETS_COLUMNS['COMMENT']] <- 'cancelled'
      }
      ## closed tickets ##
      closed.tickets.pending.index <- which(grepl('(buy|sell) (limit|stop)', tickets$type))
      if (length(closed.tickets.pending.index) > 0) {
        tickets <- tickets[-closed.tickets.pending.index, ]
      }
      closed.tickets.open.part.index <- which(grepl('(buy|sell)', tickets$type))
      tickets.closed.temp <- merge(tickets[closed.tickets.open.part.index, ], tickets[-closed.tickets.open.part.index, ], by = 'tickets')
      if (with(tickets.closed.temp, any(volume.x != volume.y))) {
        tickets.closed.temp <- tickets.closed.temp[order(as.numeric(tickets.closed.temp$tickets)), ]
        part.closed.tickets.index <- with(tickets.closed.temp, which(volume.x != volume.y))
        tickets.closed.temp[part.closed.tickets.index, 'volume.x'] <- tickets.closed.temp[part.closed.tickets.index, 'volume.y']
        sapply(part.closed.tickets.index, function(x) {
          tickets.closed.temp[x + 1, 'time.x'] <<- tickets.closed.temp[x, 'time.x']
        })
      }
      colnames(tickets.closed.temp) <- TICKETS_COLUMNS[c('TICKETS', 'OTIME', 'TYPE', '', 'OPRICE', '', '', '',
                                                         'CTIME', 'COMMENT', 'VOLUME', 'CPRICE', 'SL', 'TP', 'PROFIT')]
      ## money tickets ##
      tickets.money.temp <- data.frame(
        stringsAsFactors = F,
        0,
        private$get.tickets.begin(),
        private$get.tickets.deposit(html.table[[1]])
      )
      colnames(tickets.money.temp) <- TICKETS_GROUP_COLUMNS[[TICKETS_GROUP['MONEY']]]
      print(MetaQuote.ReportTickets$new(money.table = tickets.money.temp,
                                  closed.table = tickets.closed.temp,
                                  pending.table = tickets.pending.temp)
      )
      # part.closed.tickets.index <- with(tickets.closed.temp, which())
      # if (length(part.closed.tickets.index) > 0) {
      #   
      # }
      
      # tickets[, 1] <- .format.report.tickets.time(tickets[, 1])
      # tickets[, 3] <- as.numeric(tickets[, 3])

      
      # comment <- closed.tickets[, 10]
      # close.at.stop.index <- which(grepl(' at ', comment))
      # so.index.in.close.at.stop <- which(difftime(end.time, closed.tickets[close.at.stop.index, 9], units = 'mins') >= 1)
      # if (length(so.index.in.close.at.stop) > 0) {
      #   so.index <- close.at.stop.index[so.index.in.close.at.stop]
      #   comment[so.index] <- 'so'
      #   closed.tickets[, 10] <- comment
      # }

      # tickets.closed <- .build.report.tickets.closed.from.columns(
      #   ticket = closed.tickets[, 1],
      #   otime = as.character(closed.tickets[, 2]),
      #   type = closed.tickets[, 3],
      #   volume = closed.tickets[, 4],
      #   item = item,
      #   oprice = closed.tickets[, 5],
      #   sl = closed.tickets[, 13],
      #   tp = closed.tickets[, 14],
      #   ctime = as.character(closed.tickets[, 9]),
      #   cprice = closed.tickets[, 12],
      #   profit = closed.tickets[, 15],
      #   comment = closed.tickets[, 10]
      # )
      # tickets.money <- .build.report.tickets.money.from.columns(
      #   ticket = 0,
      #   otime = .html.mt4.ea.tickets.begin(html.parse),
      #   profit = .html.mt4.ea.tickets.capital(info.table)
      # )
      # .build.report.tickets.group(
      #   closed = tickets.closed,
      #   pending = tickets.pending,
      #   money = tickets.money
      # )
    }
    
  ),
  private = list(
    m.type = REPORT_TYPE['MT4.EA'],
    m.html.parse = NULL,
    
    init.infos = function(html.parse) {
      # ''' init infos '''
      # 2017-01-16: Version 0.3
      head.lines <- getNodeSet(html.parse, '//b', fun = xmlValue)[2:3]
      time.string <- getNodeSet(html.parse, '//tr', fun = xmlValue)[2]
      nchar.time.string <- nchar(time.string)
      self$set.infos('time', substr(time.string, nchar.time.string - 10, nchar.time.string - 1))
      self$set.infos('name', head.lines[[1]])
      self$set.infos('broker', head.lines[[2]])
    },
    get.tickets.item = function(html.parse=private$m.html.parse) {
      item.string <- getNodeSet(html.parse, '//tr/td', fun = xmlValue)[[2]]
      gsub(' ([ \\(\\)[:alpha:]])*', '', item.string)
    },
    
    get.tickets.begin = function(html.parse=private$m.html.parse) {
      # ''' mt4 ea trade begin time '''
      # 2016-08-14: Done
      time.string <- getNodeSet(html.parse, '//tr', fun = xmlValue)[2]
      nchar.time.string <- nchar(time.string)
      substr(time.string, nchar.time.string - 23, nchar.time.string - 14)
    },
    get.tickets.deposit = function(info.table) {
      # ''' mt4 ea init deposit '''
      # 2016-08-14: Done
      info.table[nrow(info.table) - 11, 2]
    }
    # .html.mt4.ea.tickets.begin <- cmpfun(function(html.parse) {
    #   # ''' mt4 ea info '''
    #   # 2016-08-14: Done
    #   time.string <- getNodeSet(html.parse, '//tr', fun = xmlValue)[2]
    #   nchar.time.string <- nchar(time.string)
    #   substr(time.string, nchar.time.string - 23, nchar.time.string - 14)
    # })# FINISH
    # 
    # .html.mt4.ea.tickets.capital <- cmpfun(function(info.table) {
    #   # ''' mt4 strategy capital '''
    #   # 2016-08-14: Done
    #   info.table[nrow(info.table) - 11, 2]
    # })# FINISH
  )
)

#### ++ HTML MT4 Trade : HTML REPORT : REPORT ####

MetaQuote.HTML.MT4Trade.Report <- R6Class(
  classname = 'MetaQuote HTML MT4 Trade Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.path, file.name, html.parse) {
      super$initialize(file.path, file.name)
      private$m.html.parse <- html.parse
      private$init.infos(html.parse)
      
    }
  ),
  private = list(
    m.type = REPORT_TYPE['MT4.Trade'],
    m.html.parse = NULL,
    
    init.infos = function(html.parse) {
      # ''' init infos '''
      # 2017-01-16: Version 0.2
      infos <- sapply(getNodeSet(html.parse, '//b')[1:8], xmlValue)
      time.index <- which(grepl('Trans', infos)) - 1
      others <- infos[2:(time.index - 1)]
      self$set.infos('account', others[grep('Account', others)])
      self$set.infos('name', others[grep('Name', others)])
      self$set.infos('broker', infos[1])
      self$set.infos('currency', others[grep('Currency', others)])
      self$set.infos('leverage', others[grep('Leverage', others)])
      self$set.infos('time', infos[time.index])
    }
  )
)

#### ++ HTML MT5 EA : HTML REPORT : REPORT ####

MetaQuote.HTML.MT5EA.Report <- R6Class(
  classname = 'MetaQuote HTML MT5 EA Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.path, file.name) {
      super$initialize(file.path, file.name)
      private$init.infos(private$m.html.table <- .html.table(file.path))
    }
  ),
  private = list(
    m.type = REPORT_TYPE['MT5.EA'],
    m.html.table = NULL,
    
    init.infos = function(html.table) {
      # ''' init infos '''
      # 2017-01-16: Version 0.2
      info.table <- html.table[[1]]
      labels <- info.table[, 1]
      values <- info.table[, 2]
      time.string <- values[which(grepl('Period', labels))[1]]
      nchar.time.string <- nchar(time.string)
      self$set.infos('name', values[which(grepl('Expert', labels))[1]])
      self$set.infos('broker', values[which(grepl('Broker', labels))[1]])
      self$set.infos('currency', values[which(grepl('Currency', labels))[1]])
      self$set.infos('leverage', values[which(grepl('Leverage', labels))[1]])
      self$set.infos('time', substr(time.string, nchar.time.string - 10, nchar.time.string - 1))
    }
  )
)

#### ++ HTML MT5 Trade : HTML REPORT : REPORT ####

MetaQuote.HTML.MT5Trade.Report <- R6Class(
  classname = 'MetaQuote HTML MT5 Trade Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.path, file.name) {
      super$initialize(file.path, file.name)
      private$init.infos(private$m.html.table <- .html.table(file.path))
    }
  ),
  private = list(
    m.type = REPORT_TYPE['MT5.Trade'],
    m.html.table = NULL,
    
    init.infos = function(html.table) {
      # ''' init infos '''
      # 2017-01-16: Version 0.2
      html.table <- html.table[[1]]
      head.info <- html.table$V2[1:4]
      self$set.infos('account', head.info[2])
      self$set.infos('name', head.info[1])
      self$set.infos('broker', head.info[3])
      self$set.infos('currency', head.info[2])
      self$set.infos('leverage', head.info[2])
      self$set.infos('time', self$set.infos('time', head.info[4]) - 8 * 3600)
    }
  )
)

#### ++ HTML MT4Manager CLOSED : HTML REPORT : REPORT ####

MetaQuote.HTML.MT4M_Closed.Report <- R6Class(
  classname = 'MetaQuote HTML MT4Manager Closed Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.path, file.name) {
      super$initialize(file.path, file.name)
      # private$m.html.table <- .html.table(file.path)
      private$init.infos()
    }
  ),
  private = list(
    m.type = REPORT_TYPE['MT4M.Closed'],
    m.html.table = NULL,
    
    init.infos = function() {
      # ''' init infos '''
      # 2017-01-16: Version 0.2
    }
  )
)

#### ++ HTML MT4Manager RAW : HTML REPORT : REPORT ####

MetaQuote.HTML.MT4M_Raw.Report <- R6Class(
  classname = 'MetaQuote HTML MT4Manager Raw Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.path, file.name) {
      super$initialize(file.path, file.name)
      # private$m.html.table <- .html.table(file.path)
      private$set.infos()
    }
  ),
  private = list(
    m.type = REPORT_TYPE['MT4M.Raw'],
    m.html.table = NULL,
    
    set.infos = function() {
      # ''' set infos '''
      # 2017-01-16: Version 0.2
    }
  )
)