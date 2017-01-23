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
    initialize = function() {
      
    },
    ## Getter & Setter ##
    ## + info ##
    get.infos.dataframe = function() {
      private$m.infos$get.info()
    },
    get.infos = function(column) {
      if (missing(column)) {
        return(private$m.infos)
      }
      private$m.infos$get.info(column)
    },
    set.infos = function(column, value) {
      if (missing(column)) {
        return(private$m.infos <- value)
      }
      private$m.infos$set.info(column, value)
    },
    ## + tickets ##
    get.tickets = function() {
      if (is.null(private$m.tickets)) {
        return(private$init.tickets())
      }
      private$m.tickets
    },
    set.tickets = function(tickets) {
      private$m.tickets <- tickets
    },
    ## + raw tickets ##
    get.raw.tickets = function(tickets.columns) {
      tickets.member <- self$get.tickets()
      raw.tickets <- tickets.member$get.tickets('raw')
      if (is.null(raw.tickets)) {
        private$init.raw.tickets(tickets.columns)
      }
      tickets.member$get.tickets('raw')
    },
    set.raw.tickets = function(tickets=private$init.raw.tickets()) {
      tickets.member <- self$get.tickets()
      tickets.member$set.tickets('raw', tickets)
    },
    init.infos = function(file.path, file.name) {
      private$m.infos <- MetaQuote.ReportInfos$new()
      self$set.infos('FilePath', file.path)
      self$set.infos('File', file.name)
      self$set.infos('Type', private$m.type)
    },
    ## + currency ##
    get.currency = function(default='USD') {
      if (is.null(private$m.currency)) {
        self$set.currency(default = default)
      }
      private$m.currency
    },
    set.currency = function(currency, default='USD') {
      if (missing(currency)) {
        currency <- self$get.infos()$get.currency(default)
      }
      private$m.currency <- currency
    },
    ## + leverage ##
    get.leverage = function(default=100) {
      if (is.null(private$m.leverage)) {
        self$set.leverage(default = default)
      }
      private$m.leverage
    },
    set.leverage = function(leverage, default=100) {
      if (missing(leverage)) {
        leverage <- self$get.infos()$get.leverage(default)
      }
      private$m.leverage <- leverage
    },
    ## init tickets ##
    init.ticketss = function(tickets.columns) {
      # ''' init tickets ''' ###
      # 2017-01-21: Version 0.2 split in many functions
      # 2017-01-18: Version 0.1
      private$init.raw.tickets(tickets.columns)
      

    }
    
    
  ),
  private = list(
    m.infos = NULL,
    m.tickets = NULL,
    m.currency = NULL,
    m.leverage = NULL,
    
    
    init.tickets = function() {
      # ''' init tickets '''
      # 2017-01-21: Version 1.0
      private$m.tickets <- MetaQuote.ReportTickets$new()
    },# FINISH
    add.tickets.table = function(table, group, columns, uniform.columns) {
      # ''' add tickets table into tickets '''
      # 2017-01-21: Version 1.0
      if (is.null(self$get.tickets())) {
        private$init.tickets()
      }
      private$m.tickets$add.table(table, group, columns, uniform.columns)
    } # FINISH
    
  )
)

#### + HTML REPORT : REPORT ####

MetaQuote.HTML.Report <- R6Class(
  classname = 'MetaQuote HTML Report',
  inherit = MetaQuote.Report,
  public = list(
    init.infos = function(file.path, file.name) {
      super$init.infos(file.path, file.name)
    }
  ),
  private = list(
    m.html.table = NULL,
    
    get.html.table = function(file.path=self$get.infos('FilePath')) {
      # ''' get html table for tickets '''
      # 2017-01-21: Version 1.2 also for null - check
      # 2017-01-18: Version 1.1 tryCatch for 2 type of encodings
      # 2017-01-17: Version 1.0
      if (is.null(private$m.html.table)) {
        private$m.html.table <- tryCatch(
          readHTMLTable(file.path, stringsAsFactors = FALSE, encoding = 'UTF-8'),
          error = function(e) readHTMLTable(file.path, stringsAsFactors = FALSE)
        )
      }
      private$m.html.table
    } # FINISH
  )
)



#### ++ HTML MT4 EA : HTML REPORT : REPORT ####

MetaQuote.HTML.MT4EA.Report <- R6Class(
  classname = 'MetaQuote HTML MT4 EA Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.path, file.name, html.parse) {
      # ''' init infos '''
      # 2017-01-16: Version 1.0
      super$init.infos(file.path, file.name)
      private$m.html.parse <- html.parse
      private$init.infos(html.parse)
    }#,# FINISH
    # init.tickets = function(tickets.columns) {
    #   # ''' init tickets ''' ###
    #   # 2017-01-21: Version 0.2 split in many functions
    #   # 2017-01-18: Version 0.1
    #   private$init.raw.tickets(tickets.columns)
    #   
    #   
    #   
    # 
    #   
    #   # part.closed.tickets.index <- with(tickets.closed.temp, which())
    #   # if (length(part.closed.tickets.index) > 0) {
    #   #   
    #   # }
    # 
    #   
    #   # comment <- closed.tickets[, 10]
    #   # close.at.stop.index <- which(grepl(' at ', comment))
    #   # so.index.in.close.at.stop <- which(difftime(end.time, closed.tickets[close.at.stop.index, 9], units = 'mins') >= 1)
    #   # if (length(so.index.in.close.at.stop) > 0) {
    #   #   so.index <- close.at.stop.index[so.index.in.close.at.stop]
    #   #   comment[so.index] <- 'so'
    #   #   closed.tickets[, 10] <- comment
    #   # }
    # }
    
  ),
  private = list(
    m.type = REPORT_TYPE['MT4.EA'],
    m.html.parse = NULL,
    
    init.infos = function(html.parse) {
      # ''' init infos '''
      # 2017-01-16: Version 1.0
      head.lines <- getNodeSet(html.parse, '//b', fun = xmlValue)[2:3]
      time.string <- getNodeSet(html.parse, '//tr', fun = xmlValue)[2]
      nchar.time.string <- nchar(time.string)
      self$set.infos('Time', substr(time.string, nchar.time.string - 10, nchar.time.string - 1))
      self$set.infos('Name', head.lines[[1]])
      self$set.infos('Broker', head.lines[[2]])
    },# FINISH
    init.raw.tickets = function(tickets.columns) {
      # ''' get all tickets from html table '''
      # 2017-01-21: Version 1.0
      item <- private$get.tickets.item()
      tickets.table <- private$get.html.table()[[2]]
      colnames(tickets.table) <- c('deal', 'time', 'type', 'tickets', 'volume', 'price', 'sl', 'tp', 'profit', 'balance')
      tickets <- subset(tickets.table, subset = type != 'modify', select = -c(deal, balance))
      ## pending tickets ##
      pending.tickets.close.part.index <- which(tickets$type == 'delete')
      if (length(pending.tickets.close.part.index) > 0) {
        pending.tickets.close.part <- tickets[pending.tickets.close.part.index, ]
        pending.tickets.tickets <- pending.tickets.close.part$tickets
        tickets <- tickets[-pending.tickets.close.part.index, ]
        pending.tickets.open.part.index <- which(tickets$tickets %in% pending.tickets.tickets)
        pending.tickets.open.part <- tickets[pending.tickets.open.part.index, ]
        tickets <- tickets[-pending.tickets.open.part.index, ]
        tickets.pending.temp <- merge(pending.tickets.open.part, pending.tickets.close.part, by = 'tickets')
        colnames(tickets.pending.temp) <- c('TICKET', 'OTIME', 'TYPE', '', 'OPRICE', '', '', '',
                                            'CTIME', '', 'VOLUME', 'CPRICE', 'SL', 'TP', 'PROFIT')
        tickets.pending.temp$ITEM <- item
        tickets.pending.temp$COMMENT <- 'cancelled'
        private$add.tickets.table(tickets.pending.temp, 'Pending', tickets.columns$Pending, tickets.columns$Uniform)
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
      colnames(tickets.closed.temp) <- c('TICKET', 'OTIME', 'TYPE', '', 'OPRICE', '', '', '',
                                         'CTIME', 'COMMENT', 'VOLUME', 'CPRICE', 'SL', 'TP', 'PROFIT')
      tickets.closed.temp$ITEM <- item
      private$add.tickets.table(tickets.closed.temp, 'Closed', tickets.columns$Closed, tickets.columns$Uniform)
      ## money tickets ##
      tickets.money.temp <- data.frame(
        stringsAsFactors = F,
        0,
        private$get.tickets.begin(),
        private$get.tickets.deposit(private$get.html.table()[[1]])
      )
      colnames(tickets.money.temp) <- c('TICKET', 'OTIME', 'PROFIT')
      private$add.tickets.table(tickets.money.temp, 'Money', tickets.columns$Money, tickets.columns$Uniform)
      self$get.raw.tickets()
    },# FINISH
    get.tickets.item = function(html.parse=private$m.html.parse) {
      # ''' mt4 ea trade item '''
      # 2017-01-21: Version 1.0
      item.string <- getNodeSet(html.parse, '//tr/td', fun = xmlValue)[[2]]
      gsub(' ([ \\(\\)[:alpha:]])*', '', item.string)
    },# FINISH
    get.tickets.begin = function(html.parse=private$m.html.parse) {
      # ''' mt4 ea trade begin time '''
      # 2016-08-14: Version 1.0
      time.string <- getNodeSet(html.parse, '//tr', fun = xmlValue)[2]
      nchar.time.string <- nchar(time.string)
      substr(time.string, nchar.time.string - 23, nchar.time.string - 14)
    },# FINISH
    get.tickets.deposit = function(info.table) {
      # ''' mt4 ea init deposit '''
      # 2016-08-14: Version 1.0
      info.table[nrow(info.table) - 11, 2]
    } # FINISH
  )
)

#### ++ HTML MT4 Trade : HTML REPORT : REPORT ####

MetaQuote.HTML.MT4Trade.Report <- R6Class(
  classname = 'MetaQuote HTML MT4 Trade Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.path, file.name, html.parse) {
      # ''' init '''
      # 2017-01-21: Version 1.0
      super$init.infos(file.path, file.name)
      private$m.html.parse <- html.parse
      private$init.infos(html.parse)
      
    } # FINISH
    
  ),
  private = list(
    m.type = REPORT_TYPE['MT4.TRADE'],
    m.html.parse = NULL,
    
    init.infos = function(html.parse) {
      # ''' init infos '''
      # 2017-01-16: Version 1.0
      infos <- sapply(getNodeSet(html.parse, '//b')[1:8], xmlValue)
      time.index <- which(grepl('Trans', infos)) - 1
      others <- infos[2:(time.index - 1)]
      self$set.infos('Account', others[grep('Account', others)])
      self$set.infos('Name', others[grep('Name', others)])
      self$set.infos('Broker', infos[1])
      self$set.infos('Currency', others[grep('Currency', others)])
      self$set.infos('Leverage', others[grep('Leverage', others)])
      self$set.infos('Time', infos[time.index])
    },# FINISH
    init.raw.tickets = function(tickets.columns) {
      # ''' get all tickets from html table '''
      # 2017-01-21: Version 1.0
      tickets.table <- private$get.html.table()[[1]]
      colnames(tickets.table) <- tickets.columns$Uniform[1:14]
      tickets.table[tickets.table == ''] <- NA
      tickets.table$COMMENT <- private$get.tickets.comments()
      suppressWarnings(tickets <- tickets.table[which(!is.na(as.numeric(tickets.table[, 1]))), ])
      if (nrow(tickets) == 0) {
        return(NULL)
      }
      na.count <- as.numeric(rowSums(is.na(tickets)))
      tickets.money.temp <- tickets[which(na.count == 9), c('TICKET', 'OTIME', 'ITEM', 'COMMENT')]
      colnames(tickets.money.temp)[3] <- 'PROFIT'
      private$add.tickets.table(tickets.money.temp, 'Money', tickets.columns$Money, tickets.columns$Uniform)
      private$add.tickets.table(tickets[which(na.count == 0), ], 'Closed', tickets.columns$Closed, tickets.columns$Uniform)
      private$add.tickets.table(tickets[which(na.count == 1), ], 'Open', tickets.columns$Open, tickets.columns$Uniform)
      private$add.tickets.table(tickets[which(na.count == 3), ], 'Pending', tickets.columns$Pending, tickets.columns$Uniform)
      private$add.tickets.table(tickets[which(na.count == 5), ], 'Working', tickets.columns$Working, tickets.columns$Uniform)
      self$get.raw.tickets()
    },# FINISH
    get.tickets.comments = function(html.parse=private$m.html.parse) {
      # ''' get comments for mt4 trade html '''
      # 2017-01-19: Version 1.0
      sapply(getNodeSet(html.parse, '//tr'), function(tr) {
        comment <- xmlGetAttr(xmlChildren(tr)[[1]], 'title')
        ifelse(is.null(comment), '', comment)
      })[-1]
    } # FINISH
  )
)

#### ++ HTML MT5 EA : HTML REPORT : REPORT ####

MetaQuote.HTML.MT5EA.Report <- R6Class(
  classname = 'MetaQuote HTML MT5 EA Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.path, file.name) {
      # ''' init '''
      # 2017-01-21: Version 1.0
      super$init.infos(file.path, file.name)
      private$init.infos(private$get.html.table())
    } # FINISH
    
  ),
  private = list(
    m.type = REPORT_TYPE['MT5.EA'],
    m.html.table = NULL,
    
    init.infos = function(html.table) {
      # ''' init infos '''
      # 2017-01-16: Version 1.0
      info.table <- html.table[[1]]
      labels <- info.table[, 1]
      values <- info.table[, 2]
      time.string <- values[which(grepl('Period', labels))[1]]
      nchar.time.string <- nchar(time.string)
      self$set.infos('Name', values[which(grepl('Expert', labels))[1]])
      self$set.infos('Broker', values[which(grepl('Broker', labels))[1]])
      self$set.infos('Currency', values[which(grepl('Currency', labels))[1]])
      self$set.infos('Leverage', values[which(grepl('Leverage', labels))[1]])
      self$set.infos('Time', substr(time.string, nchar.time.string - 10, nchar.time.string - 1))
    },# FINISH
    init.raw.tickets = function(tickets.columns) {
      # ''' get all tickets from html table '''
      # 2017-01-21: Version 1.0
      tickets.table <- private$get.html.table()[[2]]
      first.col <- tickets.table[, 1]
      spaces.index <- which(first.col == '')
      deals <- .html.mt5.ea_trade.tickets.block(tickets.table, first.col, spaces.index, 'Deals')
      tickets.list <- private$get.html.mt5.money_closed_open(deals, tickets.columns)
      self$get.raw.tickets()
    },# FINISH
    get.html.mt5.money_closed_open = function(deals, tickets.columns, position=NULL) {
      # ''' get money, closed open tickets from mt5 html '''
      # 2017-01-21: Version
      if (is.null(deals) || nrow(deals) == 0) {
        return(NULL)
      }
      money <- subset(deals, subset = Type == 'balance')
      tickets.money.temp <- .html.mt5.ea_trade.tickets.money(money)
      private$add.tickets.table(tickets.money.temp, 'Money', tickets.columns$Money, tickets.columns$Uniform)
      closed_open <- subset(deals, subset = Type != 'balance')
      private$get.html.mt5.closed_open(closed_open, tickets.columns, positions)
    },
    get.html.mt5.closed_open = function(deals.closed_open, tickets.columns, positions) {
      # ''' get closed and open tickets for mt5 html file ''
      # 2016-08-16: Version 1.0
      if (is.null(deals.closed_open) || nrow(deals.closed_open) == 0) {
        return(NULL)
      }
      closed_open <- within(deals.closed_open, {
        Time <- .format.time(Time)
        Type <- Type
        Volume <- as.numeric(Volume)
        Price <- as.numeric(Price)
        Order <- as.numeric(Order)
      })
      split.item <- split.data.frame(closed_open, closed_open$Symbol)
      lapply(split.item, private$get.html.mt5.closed_open.symbol, tickets.columns, positions)
    },
    get.html.mt5.closed_open.symbol = function(symbol.trades, tickets.columns, positions) {
      # ''' get single symbol closed and open tickets for mt5 html file '''
      # 2016-08-16: Version 1.0
      in_out.index <- with(symbol.trades, {
        which(Direction == 'in/out')
      })
      if (length(in_out.index) > 0) {
        volume.cumsum <- with(symbol.trades, {
          cumsum(ifelse(Type == 'buy', Volume, -Volume))
        })
        in.volume.value <- abs(volume.cumsum[in_out.index])
        in_out.tickets <- symbol.trades[in_out.index, ]
        other.tickets <- symbol.trades[-in_out.index, ]
        in_out.out <- within(in_out.tickets, {
          Direction <- 'out'
          Volume <- Volume - in.volume.value
          Time <- Time - 1
        })
        in_out.in <- within(in_out.tickets, {
          Direction <- 'in'
          Volume <- in.volume.value
        })
        symbol.trades <- rbind(other.tickets, in_out.out, in_out.in)
        symbol.trades <- symbol.trades[order(symbol.trades['Deal']), ]
      }
      buy <- symbol.trades$Type == 'buy'
      buy.index <- which(buy)
      sell.index <- which(!buy)
      in_ <- symbol.trades$Direction == 'in'
      in.index <- which(in_)
      out.index <- which(!in_)
      buy_in.index <- intersect(buy.index, in.index)
      buy_out.index <- intersect(buy.index, out.index)
      sell_in.index <- intersect(sell.index, in.index)
      sell_out.index <- intersect(sell.index, out.index)
      private$get.html.mt5.deals.closed_open.symbol.make.tickets(symbol.trades, buy_in.index, sell_out.index, positions, 'Buy', tickets.columns)
      private$get.html.mt5.deals.closed_open.symbol.make.tickets(symbol.trades, sell_in.index, buy_out.index, positions, 'Sell', tickets.columns)
    },# FINISH
    get.html.mt5.deals.closed_open.symbol.make.tickets = function(symbol.trades, in.index, out.index, positions, type, tickets.columns) {
      # ''' mt5 trade html file tickets '''
      # 2016-08-16: Version 1.0
      if (length(in.index) == 0) {
        return(NULL)
      }
      item <- symbol.trades$Symbol[1]
      deals.in <- symbol.trades$Deal[in.index]
      volume.in <- symbol.trades$Volume[in.index]
      deals.out <- symbol.trades$Deal[out.index]
      volume.out <- symbol.trades$Volume[out.index]
      volume.cumsum.in <- cumsum(volume.in)
      volume.cumsum.out <- cumsum(volume.out)
      volume.cumsum <- sort(union(volume.cumsum.in, volume.cumsum.out))
      tickets.in <- sapply(volume.cumsum, function(x) {
        deals.in[which(volume.cumsum.in >= x)[1]]
      })
      tickets.out <- sapply(volume.cumsum, function(x) {
        deals.out[which(volume.cumsum.out >= x)[1]]
      })
      tickets.volume <- c(volume.cumsum[1], diff(volume.cumsum))
      tickets.in.index <- match(tickets.in, symbol.trades$Deal)
      tickets.out.index <- match(tickets.out, symbol.trades$Deal)
      na.check <- is.na(tickets.out.index)
      open.index <- which(na.check)
      closed.index <- which(!na.check)
      if (length(open.index) > 0) {
        open.tickets.in.index <- tickets.in.index[open.index]
        tickets.open.temp <- with(symbol.trades, {
          data.frame(
            stringsAsFactors = F,
            row.names = NULL,
            TICKET = Order[open.tickets.in.index],
            OTIME = Time[open.tickets.in.index],
            TYPE = type,
            VOLUME = tickets.volume,
            ITEM = item,
            OPRICE = Price[open.tickets.in.index],
            CPRICE = positions[item]
          )
        })
        private$add.tickets.table(tickets.open.temp, 'Open', tickets.columns$Open, tickets.columns$Uniform)
      }
      if (length(closed.index) > 0) {
        closed.tickets.in.index <- tickets.in.index[closed.index]
        closed.tickets.out.index <- tickets.out.index[closed.index]
        tickets.closed.temp <- with(symbol.trades, {
          data.frame(
            stringsAsFactors = F,
            row.names = NULL,
            TICKET = Order[closed.tickets.in.index],
            OTIME = Time[closed.tickets.in.index],
            TYPE = type,
            VOLUME = tickets.volume,
            ITEM = item,
            OPRICE = symbol.trades$Price[closed.tickets.in.index],
            CTIME = as.character(Time[closed.tickets.out.index]),
            CPRICE = Price[closed.tickets.out.index],
            commission = Commission[closed.tickets.out.index],
            SWAP = Swap[closed.tickets.out.index],
            PROFIT = NA,
            COMMENT = Comment[closed.tickets.out.index]
          )
        })
        comments <- tickets.closed.temp$COMMENT
        sl.index <- grep('sl', comments)
        tp.index <- grep('tp', comments)
        tickets.closed.temp <- within(tickets.closed.temp, {
          TP[tp.index] <- CPRICE[tp.index]
          SL[sl.index] <- CPRICE[sl.index]
        })
        private$add.tickets.table(tickets.closed.temp, 'Closed', tickets.columns$Closed, tickets.columns$Uniform)
      }
    }
  )
)

#### ++ HTML MT5 Trade : HTML REPORT : REPORT ####

MetaQuote.HTML.MT5Trade.Report <- R6Class(
  classname = 'MetaQuote HTML MT5 Trade Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.path, file.name) {
      # ''' init '''
      # 2017-01-21: Version 1.0
      super$init.infos(file.path, file.name)
      private$init.infos(private$get.html.table())
    } # FINISH
  ),
  private = list(
    m.type = REPORT_TYPE['MT5.TRADE'],
    m.html.table = NULL,
    
    init.infos = function(html.table) {
      # ''' init infos '''
      # 2017-01-16: Version 1.0
      html.table <- html.table[[1]]
      head.info <- html.table$V2[1:4]
      self$set.infos('Account', head.info[2])
      self$set.infos('Name', head.info[1])
      self$set.infos('Broker', head.info[3])
      self$set.infos('Currency', head.info[2])
      self$set.infos('Leverage', head.info[2])
      self$set.infos('Time', self$set.infos('Time', head.info[4]) - 8 * 3600)
    },# FINISH
    init.raw.tickets = function(tickets.columns) {
      # ''' get all tickets from html table '''
      # 2017-01-21: Version 1.0
      tickets.table <- private$get.html.table()[[1]]
      first.col <- tickets.table$V1
      spaces.index <- which(first.col == '')
      orders <- .html.mt5.ea_trade.tickets.block(tickets.table, first.col, spaces.index, 'Orders')
      positions <- .html.mt5.ea_trade.tickets.block(tickets.table, first.col, spaces.index, 'Trade Positions')
      workings <- .html.mt5.ea_trade.tickets.block(tickets.table, first.col, spaces.index, 'Working Orders')
      deals <- .html.mt5.ea_trade.tickets.block(tickets.table, first.col, spaces.index, 'Deals')
      positions.market.price <- .html.mt5.trade.tickets.positions.market.price(positions)
      private$get.html.mt5.money_closed_open(deals, tickets.columns, positions.market.price)
      self$get.raw.tickets()
    },# FINISH
    get.html.mt5.money_closed_open = function(deals, tickets.columns, positions=NULL) {
      # ''' get money, closed open tickets from mt5 html '''
      # 2017-01-21: Version
      if (is.null(deals) || nrow(deals) == 0) {
        return(NULL)
      }
      money <- subset(deals, subset = Type == 'balance')
      tickets.money.temp <- .html.mt5.ea_trade.tickets.money(money)
      private$add.tickets.table(tickets.money.temp, 'Money', tickets.columns$Money, tickets.columns$Uniform)
      closed_open <- subset(deals, subset = Type != 'balance')
      private$get.html.mt5.closed_open(closed_open, tickets.columns, positions)
    },# FINISH
    get.html.mt5.closed_open = function(deals.closed_open, tickets.columns, positions) {
      # ''' get closed and open tickets for mt5 html file ''
      # 2016-08-16: Version 1.0
      if (is.null(deals.closed_open) || nrow(deals.closed_open) == 0) {
        return(NULL)
      }
      closed_open <- within(deals.closed_open, {
        Time <- .format.time(Time)
        Type <- Type
        Volume <- as.numeric(Volume)
        Price <- as.numeric(Price)
        Order <- as.numeric(Order)
      })
      split.item <- split.data.frame(closed_open, closed_open$Symbol)
      lapply(split.item, private$get.html.mt5.closed_open.symbol, tickets.columns, positions)
    },# FINISH
    get.html.mt5.closed_open.symbol = function(symbol.trades, tickets.columns, positions) {
      # ''' get single symbol closed and open tickets for mt5 html file '''
      # 2016-08-16: Version 1.0
      in_out.index <- with(symbol.trades, {
        which(Direction == 'in/out')
      })
      if (length(in_out.index) > 0) {
        volume.cumsum <- with(symbol.trades, {
          cumsum(ifelse(Type == 'buy', Volume, -Volume))
        })
        in.volume.value <- abs(volume.cumsum[in_out.index])
        in_out.tickets <- symbol.trades[in_out.index, ]
        other.tickets <- symbol.trades[-in_out.index, ]
        in_out.out <- within(in_out.tickets, {
          Direction <- 'out'
          Volume <- Volume - in.volume.value
          Time <- Time - 1
        })
        in_out.in <- within(in_out.tickets, {
          Direction <- 'in'
          Volume <- in.volume.value
        })
        symbol.trades <- rbind(other.tickets, in_out.out, in_out.in)
        symbol.trades <- symbol.trades[order(symbol.trades['Deal']), ]
      }
      buy <- symbol.trades$Type == 'buy'
      buy.index <- which(buy)
      sell.index <- which(!buy)
      in_ <- symbol.trades$Direction == 'in'
      in.index <- which(in_)
      out.index <- which(!in_)
      buy_in.index <- intersect(buy.index, in.index)
      buy_out.index <- intersect(buy.index, out.index)
      sell_in.index <- intersect(sell.index, in.index)
      sell_out.index <- intersect(sell.index, out.index)
      private$get.html.mt5.deals.closed_open.symbol.make.tickets(symbol.trades, buy_in.index, sell_out.index, positions, 'Buy', tickets.columns)
      private$get.html.mt5.deals.closed_open.symbol.make.tickets(symbol.trades, sell_in.index, buy_out.index, positions, 'Sell', tickets.columns)
    },# FINISH
    get.html.mt5.deals.closed_open.symbol.make.tickets = function(symbol.trades, in.index, out.index, positions, type, tickets.columns) {
      # ''' mt5 trade html file tickets '''
      # 2016-08-16: Version 1.0
      if (length(in.index) == 0) {
        return(NULL)
      }
      item <- symbol.trades$Symbol[1]
      deals.in <- symbol.trades$Deal[in.index]
      volume.in <- symbol.trades$Volume[in.index]
      deals.out <- symbol.trades$Deal[out.index]
      volume.out <- symbol.trades$Volume[out.index]
      volume.cumsum.in <- cumsum(volume.in)
      volume.cumsum.out <- cumsum(volume.out)
      volume.cumsum <- sort(union(volume.cumsum.in, volume.cumsum.out))
      tickets.in <- sapply(volume.cumsum, function(x) {
        deals.in[which(volume.cumsum.in >= x)[1]]
      })
      tickets.out <- sapply(volume.cumsum, function(x) {
        deals.out[which(volume.cumsum.out >= x)[1]]
      })
      tickets.volume <- c(volume.cumsum[1], diff(volume.cumsum))
      tickets.in.index <- match(tickets.in, symbol.trades$Deal)
      tickets.out.index <- match(tickets.out, symbol.trades$Deal)
      na.check <- is.na(tickets.out.index)
      open.index <- which(na.check)
      closed.index <- which(!na.check)
      if (length(open.index) > 0) {
        open.tickets.in.index <- tickets.in.index[open.index]
        tickets.open.temp <- with(symbol.trades, {
          data.frame(
            stringsAsFactors = F,
            row.names = NULL,
            TICKET = Order[open.tickets.in.index],
            OTIME = Time[open.tickets.in.index],
            TYPE = type,
            VOLUME = tickets.volume,
            ITEM = item,
            OPRICE = Price[open.tickets.in.index],
            CPRICE = positions[item]
          )
        })
        private$add.tickets.table(tickets.open.temp, 'Open', tickets.columns$Open, tickets.columns$Uniform)
      }
      if (length(closed.index) > 0) {
        closed.tickets.in.index <- tickets.in.index[closed.index]
        closed.tickets.out.index <- tickets.out.index[closed.index]
        tickets.closed.temp <- with(symbol.trades, {
          data.frame(
            stringsAsFactors = F,
            row.names = NULL,
            TICKET = Order[closed.tickets.in.index],
            OTIME = Time[closed.tickets.in.index],
            TYPE = type,
            VOLUME = tickets.volume,
            ITEM = item,
            OPRICE = symbol.trades$Price[closed.tickets.in.index],
            CTIME = as.character(Time[closed.tickets.out.index]),
            CPRICE = Price[closed.tickets.out.index],
            commission = Commission[closed.tickets.out.index],
            SWAP = Swap[closed.tickets.out.index],
            PROFIT = NA,
            COMMENT = Comment[closed.tickets.out.index],
            SL = 0,
            TP = 0
          )
        })
        comments <- tickets.closed.temp$COMMENT
        sl.index <- grep('sl', comments)
        tp.index <- grep('tp', comments)
        tickets.closed.temp <- within(tickets.closed.temp, {
          TP[tp.index] <- CPRICE[tp.index]
          SL[sl.index] <- CPRICE[sl.index]
        })
        private$add.tickets.table(tickets.closed.temp, 'Closed', tickets.columns$Closed, tickets.columns$Uniform)
      }
    } # FINISH
  )
)

.html.mt5.ea_trade.tickets.block <- cmpfun(function(html.table, first.col, spaces.index, block.name) {
  # ''' get mt5 ea & trade html block tickets '''
  # 2016-08-15: Version 1.0
  block.name.index <- which(first.col == block.name)
  if (length(block.name.index) == 0) return(NULL)
  block.begin.index <- block.name.index + 2
  block.end.index <- spaces.index[which(spaces.index > block.name.index)[1]] - 1
  if (block.begin.index > block.end.index) return(NULL)
  block <- html.table[block.begin.index:block.end.index, ]
  colnames(block) <- html.table[block.name.index + 1, ]
  block
})# FINISH

.html.mt5.ea_trade.tickets.money <- cmpfun(function(money) {
  # ''' get money tickets for mt5 html file ''
  # 2017-01-21: Version 1.1 change | to ||
  # 2016-08-16: Version 1.0
  if (is.null(money) || nrow(money) == 0) {
    return(NULL)
  }
  with(money, {
    data.frame(
      stringsAsFactors = F,
      row.names = NULL,
      TICKET = Deal,
      OTIME = Time,
      PROFIT = Profit,
      COMMENT = Comment
    )
  })
})# FINISH

.html.mt5.trade.tickets.positions.market.price <- cmpfun(function(positions) {
  # ''' handle mt5 trade html positions '''
  # 2016-08-15: Version 1.0
  price <- as.numeric(positions$'Market Price')
  names(price) <- positions$Symbol
  price
})# FINISH

.html.mt5.trade.tickets.working <- cmpfun(function(workings) {
  # ''' work with workings, create working ticktes '''
  # 2016-08-16: Version 1.0
  if (is.null(workings) || nrow(workings) == 0) {
    return(NULL) 
  }
  colnames(workings) <- c('OTIME', 'TICKET', 'ITEM', 'TYPE', 'VOLUME', 'OPRICE', 'SL', 'TP', 'CPRICE', '', 'COMMENT')
  workings
})# FINISH

.html.mt5.trade.tickets.pending <- cmpfun(function(orders) {
  # ''' handle mt5 trade html orders tickets pending '''
  # 2016-08-16: Version 1.0
  pending.index <- with(orders, which(State == 'canceled'))
  if (length(pending.index) == 0) {
    return(NULL) 
  }
  pending <- orders[pending.index, ]
  colnames(pending) <- c('OTIME', 'TICKET', 'ITEM', 'TYPE', 'VOLUME', 'OPRICE', 'SL', 'TP', 'CTIME', '', 'COMMENT', 'CPRICE')
  pending
})# FINISH

#### ++ HTML MT4Manager CLOSED : HTML REPORT : REPORT ####

MetaQuote.HTML.MT4M_Closed.Report <- R6Class(
  classname = 'MetaQuote HTML MT4Manager Closed Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.path, file.name) {
      # ''' init '''
      # 2017-01-21: Version 1.0
      super$init.infos(file.path, file.name)
      # private$m.html.table <- .html.table(file.path)
      private$init.infos()
    }
  ),
  private = list(
    m.type = REPORT_TYPE['MT4M.CLOSED'],
    m.html.table = NULL,
    
    init.infos = function() {
      # ''' init infos '''
      # 2017-01-16: Version 0.2
    },# FINISH
    init.raw.tickets = function(tickets.columns) {
      # ''' get all tickets from html table '''
      # 2017-01-21: Version 1.0
      tickets.table <- private$get.html.table()[[1]]
      tickets.table <- tickets.table[2:(nrow(tickets.table) - 1), ]
      colnames(tickets.table) <- c('TICKET', 'LOGIN', '', 'OTIME', 'TYPE', 'ITEM', 'VOLUME', 'OPRICE', 'CTIME', 'CPRICE',
                                   'COMMISSION', 'TAXES', '', 'SWAP', 'PROFIT', '', 'COMMENT')
      new.comment <- with(tickets.table, paste(COMMENT, LOGIN, sep = ' | Login: '))
      tickets.table$COMMENT <- gsub('^ [|] ', '', new.comment)
      private$add.tickets.table(tickets.table, 'Closed', tickets.columns$Closed, tickets.columns$Uniform)
      self$get.raw.tickets()
    } # FINISH
  )
)

#### ++ HTML MT4Manager RAW : HTML REPORT : REPORT ####

MetaQuote.HTML.MT4M_Raw.Report <- R6Class(
  classname = 'MetaQuote HTML MT4Manager Raw Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.path, file.name) {
      # ''' init '''
      # 2017-01-21: Version 1.0
      super$init.infos(file.path, file.name)
      # private$m.html.table <- .html.table(file.path)
      private$set.infos()
    }
  ),
  private = list(
    m.type = REPORT_TYPE['MT4M.RAW'],
    m.html.table = NULL,
    
    set.infos = function() {
      # ''' set infos '''
      # 2017-01-16: Version 0.2
    },# FINISH
    init.raw.tickets = function(tickets.columns) {
      # ''' get all tickets from html table '''
      # 2017-01-21: Version 1.0
      tickets.table <- private$get.html.table()[[1]]
      tickets.table <- tickets.table[2:(nrow(tickets.table) - 7), ]
      colnames(tickets.table) <- c('TICKET', 'LOGIN', 'OTIME', 'TYPE', 'ITEM', 'VOLUME', 'OPRICE', 'SL', 'TP',
                                   'CTIME', 'CPRICE', rep('', 6), 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', '', 'COMMENT')
      new.comment <- with(tickets.table, paste(COMMENT, LOGIN, sep = ' | Login: '))
      tickets.table$COMMENT <- gsub('^ [|] ', '', new.comment)
      ## money tickets ##
      money.index <- which(tickets.table$TYPE == 'balance')
      if (length(money.index) > 0) {
        private$add.tickets.table(tickets.table[money.index, ], 'Money', tickets.columns$Money, tickets.columns$Uniform)
        tickets.table <- tickets.table[-money.index, ]
      }
      ## pending tickets ##
      pending.index <- which(grepl('(buy|sell) (limit|stop)', tickets.table$TYPE))
      if (length(pending.index) > 0) {
        private$add.tickets.table(tickets.table[pending.index, ], 'Pending', tickets.columns$Pending, tickets.columns$Uniform)
        tickets.table <- tickets.table[-pending.index, ]
      }
      if (nrow(tickets.table) > 0) {
        private$add.tickets.table(tickets.table, 'Closed', tickets.columns$Closed, tickets.columns$Uniform)
      }
      self$get.raw.tickets()
    } # FINISH
  )
)


