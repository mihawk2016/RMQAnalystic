## dont need symbol setting, usd all symbol table
## MT5/MT4 EA no need split with SYMBOL to cal profit
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
  #### + PUBLIC ####
  public = list(
    initialize = function() {
      # ''' initialize '''
      # 2017-01-23: Version 
      self$set.Infos(MetaQuote.ReportInfos$new())
      self$set.Tickets(MetaQuote.ReportTickets$new())

    },
    #### ++ Getter & Setter ####
    
    #### +++ Infos ####
    get.Infos = function() {
      # ''' get Infos '''
      # 2017-01-24: Version 1.0
      private$m.Infos
    },# FINISH
    set.Infos = function(Infos) {
      # ''' set Infos '''
      # 2017-01-24: Version 1.0
      private$m.Infos <- Infos
    },# FINISH
    get.infos.column = function(column) {
      # ''' get infos column '''
      # 2017-01-24: Version 1.0
      infos <- self$get.Infos()
      infos$get.infos.column(column)
    },# FINISH
    set.infos.column = function(column, value) {
      # ''' set infos column '''
      # 2017-01-24: Version 1.0
      infos <- self$get.Infos()
      infos$set.infos.column(column, value)
    },# FINISH
    
    #### +++ Tickets ####
    get.Tickets = function() {
      # ''' get Tickets '''
      # 2017-01-24: Version 1.0
      private$m.Tickets
    },# FINISH
    set.Tickets = function(Tickets) {
      # ''' set Tickets '''
      # 2017-01-24: Version 1.0
      private$m.Tickets <- Tickets
    },# FINISH
    get.tickets.member = function(member) {
      # ''' get Tickets member '''
      # 2017-01-24: Version 1.0
      Tickets <- self$get.Tickets()
      Tickets$get.tickets.member(member)
    },
    set.tickets.member = function(member, tickets) {
      # ''' set Tickets member '''
      # 2017-01-24: Version 1.0
      Tickets <- self$get.Tickets()
      Tickets$set.tickets.member(member, tickets)
    },
    
    #### +++  currency ####
    get.currency = function() {
      # ''' get  currency '''
      # 2017-01-23: Version 1.0
      private$m.currency
    },# FINISH
    set.currency = function(currency) {
      # ''' set  currency '''
      # 2017-01-23: Version 1.0
      private$m.currency <- currency
    },# FINISH
    
    #### +++  leverage ####
    get.leverage = function() {
      # ''' get  leverage '''
      # 2017-01-23: Version 1.0
      private$m.leverage
    },# FINISH
    set.leverage = function(leverage) {
      # ''' set  leverage '''
      # 2017-01-23: Version 1.0
      private$m.leverage <- leverage
    },# FINISH

    
    #### +++  symbol mapping ####
    get.symbol.mapping = function() {
      # ''' get symbol.mapping '''
      # 2017-01-24: Version 1.0
      private$m.symbol.mapping
    },# FINISH
    set.symbol.mapping = function(symbol.mapping) {
      # ''' set symbol.mapping '''
      # 2017-01-24: Version 1.0
      private$m.symbol.mapping <- symbol.mapping
    },# FINISH
    
    #### +++  symbol setting ####
    get.symbol.setting = function() {
      # ''' get symbol.setting '''
      # 2017-01-24: Version 1.0
      private$m.symbol.setting
    },# FINISH
    set.symbol.setting = function(symbol.setting) {
      # ''' set symbol.setting '''
      # 2017-01-24: Version 1.0
      private$m.symbol.setting <- symbol.setting
    },# FINISH

    
    #### ++ init ####
    
    #### +++ Infos ####
    init.Infos = function(file.path, file.name) {
      # ''' init Infos '''
      # 2017-01-24: Version 1.0
      # self$set.infos.column('FilePath', file.path)
      self$set.infos.column('File', file.name)
      self$set.infos.column('Type', private$m.type)
    },# FINISH
    
    #### +++ raw tickets ####
    init.raw.tickets = function(tickets.columns) {
      # ''' init raw tickets (virtual) '''
      # 2017-01-24: Version 1.0
    },# FINISH
    
    #### +++ others ####
    init.others = function(tickets.columns, default.currency, default.leverage, symbol.table, db, timeframe, format.digits, reset=FALSE) {
      # ''' init raw tickets and others '''
      # 2017-01-24: Version
      if (reset) {
        self$set.currency(NULL)
        self$set.leverage(NULL)
        self$set.symbol.mapping(NULL)
        self$set.symbol.setting(NULL)
      }
      if (is.null(self$get.tickets.member('raw'))) {
        self$init.raw.tickets(tickets.columns)
      }
      if (is.null(self$get.currency())) {
        self$init.currency(default.currency)
      }
      if (is.null(self$get.leverage())) {
        self$init.leverage(default.leverage)
      }
      symbol.mapping <- self$get.symbol.mapping()
      if (is.null(symbol.mapping)) {
        symbol.mapping <- self$init.symbol.mapping(symbol.table)
      }
      if (is.null(self$get.symbol.setting())) {
        self$init.symbol.setting(symbol.table, symbol.mapping)
      }
      self$init.else(db, timeframe, format.digits)
    },
    
    init.else = function(db, timeframe, format.digits) {
      # ''' init else (virtual) '''
      # 2017-01-25: Version 1.0
      
    },# FINISH
    
    #### +++ currency ####
    init.currency = function(default.currency) {
      # ''' init currency '''
      # 2017-01-24: Version 1.0
      currency <- unique(self$get.infos.column('Currency'))
      if (length(currency) > 1 || is.na(currency)) {
        currency <- default.currency
      }
      self$set.currency(currency)
    },# FINISH
    
    #### +++ leverage ####
    init.leverage = function(default.leverage) {
      # ''' init leverage'''
      # 2017-01-24: Version 1.0
      leverage <- unique(self$get.infos.column('Leverage'))
      if (length(leverage) > 1 || is.na(leverage)) {
        leverage <- default.leverage
      }
      self$set.leverage(leverage)
    },# FINISH
    
    #### +++ symbol setting ####
    init.symbol.mapping = function(all.symbol.table) {
      # ''' init symbol mapping '''
      # 2017-01-24: Version 1.0
      all.symbols <- rownames(all.symbol.table)
      items <- self$get.Tickets()$get.unique.items()
      self$set.symbol.mapping(sapply(items, private$item2symbol, all.symbols, USE.NAMES = T))
    },# FINISH
    
    #### +++ symbol setting ####
    init.symbol.setting = function(all.symbol.table, symbol.mapping=self$get.symbol.mapping()) {
      # ''' init symbol setting '''
      # 2017-01-24: Version 1.0
      symbols <- unique(symbol.mapping)
      symbols <- symbols[!is.na(symbols)]
      self$set.symbol.setting(all.symbol.table[symbols, ])
    },# FINISH
    
    
    
    
    #### ++ ACTION ####
    format.tickets = function(tickets.type='raw', overwrite=TRUE) {
      # ''' format tickets member '''
      # 2017-01-24: Version 0.1
      self$get.Tickets()$format.tickets(tickets.type, overwrite)
    },
    sort.tickets = function(tickets.type='raw', column='OTIME', decreasing=F, overwrite=TRUE) {
      # ''' sort tickets member '''
      # 2017-01-24: Version 0.1
      self$get.Tickets()$sort.tickets(tickets.type, column, decreasing, overwrite)
    },
    
    #### ++ TICKETS BEHAVIOR ####
    tickets.add.symbol = function() {
      # ''' tickets add symbol '''
      # 2017-01-25: Version 1.0
      self$set.tickets.member('raw', private$cal.tickets.symbol())
    },# FINISH
    tickets.add.exit = function() {
      # ''' tickets add exit '''
      # 2017-01-25: Version 1.0
      self$set.tickets.member('raw', private$cal.tickets.exit())
    },# FINISH
    
    output.csv = function(tickets=self$get.tickets.member('raw'), groups, columns, filename, file) {
      ## ToDo: filename ####
      .output.csv(tickets, groups, columns, filename, file)
    },
    
    
    ## init tickets ##
    init.ticketss = function(tickets.columns) {
      # ''' init tickets ''' ###
      # 2017-01-21: Version 0.2 split in many functions
      # 2017-01-18: Version 0.1
      private$init.raw.tickets(tickets.columns)
      
      
    }
    
    
  ),
  #### + PRIVATE ####
  private = list(
    #### ++ MEMBER ####
    m.Infos = NULL,
    m.Tickets = NULL,
    m.currency = NULL,
    m.leverage = NULL,
    m.symbol.setting = NULL,
    m.symbol.mapping = NULL,


    add.tickets.table = function(tickets.type, table, group, columns, uniform.columns) {
      # ''' add tickets table into tickets '''
      # 2017-01-24: Version 1.1 load Tickets function
      # 2017-01-21: Version 1.0
      tickets <- self$get.Tickets()
      tickets$add.table(tickets.type, table, group, columns, uniform.columns)
    },# FINISH
    
    #### +++ calculate ####
    recal.tickets.profits = function(tickets, db, timeframe, format.digits = 2) {
      # ''' calculate profit for tickets '''
      # 2016-08-15: Version 1.0
      closed.tickets.index <- which(tickets$GROUP == 'Closed')
      closed.tickets <- tickets[closed.tickets.index, ]
      new.closed.tickets <- do.call(rbind, lapply(split(closed.tickets, closed.tickets$SYMBOL), function(symbol.tickets) {
        private$cal.symbol.tickets.profits(symbol.tickets, db, timeframe, format.digits, overwrite = TRUE)
      }))
      new.tickets <- rbind(new.closed.tickets, tickets[-closed.tickets.index, ])
      new.tickets[order(new.tickets$OTIME), ]
    },
    cal.symbol.tickets.profits = function(tickets, db, timeframe, format.digits = 2, overwrite=FALSE) {
      # ''' calculate profit for one symbol tickets '''
      # 2016-08-15: Version 1.0
      symbol <- tickets$SYMBOL[1]
      digit <- private$m.symbol.setting[symbol, 'DIGITS']
      tick.value <- private$cal.tick.value(symbol, tickets$CTIME, db, timeframe)
      pips <- with(tickets, private$cal.pips(TYPE, OPRICE, CPRICE, digit))
      profits <- private$cal.profits(tickets$VOLUME, tick.value, pips, format.digits)
      if (overwrite) {
        return(within(tickets, PROFIT <- profits))
      }
      return(profits)
    },
    
    cal.tick.value = function(symbol, times, db, timeframe='M1') {
      # ''' cal tick value '''
      # 2017-01-23: Version 0.1
      currency <- self$get.currency()
      base.currency <- private$symbol.base.currency(symbol)
      tick.value.point <- with(private$m.symbol.setting[symbol, ], CON_SIZE * 10 ^ -DIGITS)
      if (base.currency == currency) {
        return(tick.value.point)
      }
      target.symbol <- private$build.symbol(base.currency, currency)
      target.open <- db$get.open(target.symbol, times, timeframe)
      if (base.currency == private$symbol.base.currency(target.symbol)) {
        return(tick.value.point / target.open)
      }
      tick.value.point * target.open
    },
    
    cal.profits = function(volume, tickvalue, pips, format.digits = 2) {
      # ''' calculate profit from: volume, tickvalue, pips '''
      # 2016-08-15: Version 1.0
      round(volume * tickvalue * pips, format.digits)
    },# FINISH
    
    cal.pips = function(type, open.price, close.price, digit) {
      # ''' calculate pips '''
      # 2017-01-22: Version 1.0
      sell.index <- which(grepl('SELL', toupper(type)))
      diff.price <- close.price - open.price
      if (length(sell.index) > 0) {
        diff.price[sell.index] <- -diff.price[sell.index]
      }
      diff.price * 10 ^ digit
    },# FINISH
    
    cal.margin.required = function(symbol, times, db, timeframe='M1') {
      # ''' cal margin required '''
      # 2017-01-23: Version 0.1
      currency <- self$get.currency()
      leverage <- self$get.leverage()
      quote.currency <- private$symbol.quote.currency(symbol)
      margin.required.point <- with(private$m.symbol.setting[symbol, ], CON_SIZE / leverage)
      if (quote.currency == currency) {
        return(margin.required.point)
      }
      target.symbol <- private$build.symbol(quote.currency, currency)
      target.open <- db$get.open(target.symbol, times, timeframe)
      if (quote.currency == private$symbol.quote.currency(target.symbol)) {
        return(margin.required.point * target.open)
      }
      margin.required.point / target.open
    },
    
    
    #### +++ symbol ####
    item2symbol = function(item, symbols) {
      # ''' item to symbol '''
      # 2016-08-12: Version 1.0
      if (is.na(item) || grepl('BX', item)) {
        return('') 
      }
      symbol <- symbols[str_detect(item, symbols)]
      if (length(symbol) != 1) {
        symbol <- ''
      }
      symbol
    },# FINISH
    build.symbol = function(currency1, currency2, support.symbols = self$get.support.symbols()) {
      # ''' build symbol from 2 currencies '''
      # 2016-08-12: Version 1.0 FIX support.symbols: self$get.support.symbols() should usd all symbol table
      support.symbols = c('AUDCAD', 'AUDCHF', 'AUDJPY', 'AUDNZD', 'AUDUSD', 'CADCHF', 'CADJPY', 'CHFJPY', 'EURAUD', 'EURCAD',
        'EURCHF', 'EURGBP', 'EURJPY', 'EURNZD', 'EURUSD', 'GBPAUD', 'GBPCAD', 'GBPCHF', 'GBPJPY', 'GBPNZD',
        'GBPUSD', 'NZDCAD', 'NZDCHF', 'NZDJPY', 'NZDUSD', 'USDCAD', 'USDCHF', 'USDJPY', 'XAGUSD', 'XAUUSD')
      match.currency1 <- support.symbols[str_detect(support.symbols, currency1)]
      symbol <- match.currency1[str_detect(match.currency1, currency2)]
      if (length(symbol) == 1) {
        return(symbol) 
      }
      NULL
    },# FINISH
    symbol.base.currency = function(symbol) {
      # ''' symbol's base currency '''
      # 2017-01-23: Version 1.0
      substr(symbol, 4, 6)
    },# FINISH
    symbol.quote.currency = function(symbol) {
      # ''' symbol's quote currency '''
      # 2017-01-23: Version 1.0
      substr(symbol, 1, 3)
    },# FINISH
    cal.tickets.symbol = function(tickets=self$get.tickets.member('raw'), overwrite=T) {
      # ''' cal tickets symbol '''
      # 2017-01-24: Version 1.0
      symbols <- self$get.symbol.mapping()[tickets$ITEM]
      if (overwrite) {
        return(within(tickets, SYMBOL <- symbols))
      }
      symbols
    },# FINISH
    cal.tickets.exit = function(tickets=self$get.tickets.member('raw'), overwrite=T) {
      # ''' cal tickets symbol '''
      # 2017-01-24: Version 1.0
      exit <- .report.tickets.exit(tickets$COMMENT)
      if (overwrite) {
        return(within(tickets, EXIT <- exit))
      }
      exit
    } # FINISH
  )
)


#### + HTML REPORT : REPORT ####

MetaQuote.HTML.Report <- R6Class(
  classname = 'MetaQuote HTML Report',
  inherit = MetaQuote.Report,
  public = list(
    initialize = function() {
      # ''' initialize '''
      # 2017-01-23: Version 1.0
      super$initialize()
    },# FINISH
    init.Infos = function(file.path, file.name) {
      super$init.Infos(file.path, file.name)
    },
    
    #### +++ others ####
    init.others = function(tickets.columns, default.currency, default.leverage, symbol.table, db, timeframe, format.digits, reset=FALSE) {
      # ''' init raw tickets and others '''
      # 2017-01-24: Version
      super$init.others(tickets.columns, default.currency, default.leverage, symbol.table, db, timeframe, format.digits, reset)
    },
    
    init.else = function(db, timeframe, format.digits) {
      # ''' init else '''
      super$init.else(db, timeframe, format.digits)
      self$tickets.add.symbol()
      self$tickets.add.exit()
    },# FINISH
    
    #### Getter & Setter ####
    get.html.parse = function() {
      # ''' get html parse '''
      # 2017-01-25: Version 1.0
      private$m.html.parse
    },# FINISH
    set.html.parse = function(html.parse) {
      # ''' set html parse '''
      # 2017-01-25: Version 1.0
      private$m.html.parse <- html.parse
    } # FINISH
  ),
  private = list(
    m.html.parse = NULL,
    m.html.table = NULL,
    
    get.html.table = function(file.path=self$get.html.parse()) {
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
      super$initialize()
      super$init.Infos(file.path, file.name)
      private$m.html.parse <- html.parse
      private$init.Infos(html.parse)
    },# FINISH
    
    #### +++ others ####
    init.others = function(tickets.columns, default.currency, default.leverage, symbol.table, db, timeframe, format.digits, reset=FALSE) {
      # ''' init raw tickets and others '''
      # 2017-01-24: Version
      super$init.others(tickets.columns, default.currency, default.leverage, symbol.table, db, timeframe, format.digits, reset)
    },
    
    init.else = function(db, timeframe, format.digits) {
      # ''' init else '''
      # 2017-01-25: Version 1.0
      super$init.else(db, timeframe, format.digits)
      new.tickets <- private$close.at.stop.is.so()
      new.tickets <- private$recal.profit.and.swap(new.tickets, db, timeframe, format.digits)
      self$set.tickets.member('raw', new.tickets)
    },# FINISH
    
    init.raw.tickets = function(tickets.columns) {
      # ''' get all tickets from html table '''
      # 2017-01-24: Version 1.1 format & sort
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
        private$add.tickets.table('raw', tickets.pending.temp, 'Pending', tickets.columns$Pending, tickets.columns$Uniform)
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
      private$add.tickets.table('raw',tickets.closed.temp, 'Closed', tickets.columns$Closed, tickets.columns$Uniform)
      ## money tickets ##
      tickets.money.temp <- data.frame(
        stringsAsFactors = F,
        0,
        private$get.tickets.begin(),
        private$get.tickets.deposit(private$get.html.table()[[1]])
      )
      colnames(tickets.money.temp) <- c('TICKET', 'OTIME', 'PROFIT')
      private$add.tickets.table('raw',tickets.money.temp, 'Money', tickets.columns$Money, tickets.columns$Uniform)
      self$format.tickets()
      self$sort.tickets()
    } # FINISH
  ),
  private = list(
    m.type = REPORT_TYPE['MT4.EA'],
    
    init.Infos = function(html.parse) {
      # ''' init infos '''
      # 2017-01-16: Version 1.0
      head.lines <- getNodeSet(html.parse, '//b', fun = xmlValue)[2:3]
      time.string <- getNodeSet(html.parse, '//tr', fun = xmlValue)[2]
      nchar.time.string <- nchar(time.string)
      self$set.infos.column('Time', substr(time.string, nchar.time.string - 10, nchar.time.string - 1))
      self$set.infos.column('Name', head.lines[[1]])
      self$set.infos.column('Broker', head.lines[[2]])
    },# FINISH
    
    #### ++ raw tickets extra ####
    recal.profit.and.swap = function(tickets, db, timeframe, format.digits = 2) {
      ### '' recal profit and swap '''
      ### 2015-01-25: Version 0.9 profit of new and old tickets may not in same order
      profit <- private$recal.tickets.profits(tickets, db, timeframe, format.digits)$PROFIT
      ## need 'profit' in same order as 'tickets'
      within(tickets, {
        SWAP <- PROFIT - profit
        PROFIT <- profit
      })
    },# ToDo
    
    close.at.stop.is.so = function() {
      tickets <- self$get.tickets.member('raw')
      close.at.stop.index <- which(grepl('close at stop', tickets$COMMENT))
      time <- self$get.infos.column('Time')
      difftimes <- time - as.numeric(tickets$CTIME[close.at.stop.index])
      tickets$EXIT[close.at.stop.index] <- ifelse(difftimes > 60, 'SO', '')
      tickets
    },
    
    #### ++ UTILS ####
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
  #### + PUBLIC ####
  public = list(
    initialize = function(file.path, file.name, html.parse) {
      # ''' init '''
      # 2017-01-21: Version 1.0
      super$initialize()
      super$init.Infos(file.path, file.name)
      private$m.html.parse <- html.parse
      private$init.Infos(html.parse)
    },# FINISH
    init.raw.tickets = function(tickets.columns) {
      # ''' get all tickets from html table '''
      # 2017-01-24: Version 1.1 format & sort
      # 2017-01-21: Version 1.0
      tickets.table <- private$get.html.table()[[1]]
      colnames(tickets.table) <- tickets.columns$Uniform[1:14]
      tickets.table[tickets.table == ''] <- NA
      tickets.table$COMMENT <- private$get.tickets.comments()
      suppressWarnings(tickets <- tickets.table[which(!is.na(as.numeric(tickets.table[, 1]))), ]) #### 留后处理 ####
      if (nrow(tickets) == 0) {
        return(NULL)
      }
      na.count <- as.numeric(rowSums(is.na(tickets)))
      tickets.money.temp <- tickets[which(na.count == 9), c('TICKET', 'OTIME', 'ITEM', 'COMMENT')]
      colnames(tickets.money.temp)[3] <- 'PROFIT'
      private$add.tickets.table('raw', tickets.money.temp, 'Money', tickets.columns$Money, tickets.columns$Uniform)
      private$add.tickets.table('raw', tickets[which(na.count == 0), ], 'Closed', tickets.columns$Closed, tickets.columns$Uniform)
      private$add.tickets.table('raw', tickets[which(na.count == 1), ], 'Open', tickets.columns$Open, tickets.columns$Uniform)
      private$add.tickets.table('raw', tickets[which(na.count == 3), ], 'Pending', tickets.columns$Pending, tickets.columns$Uniform)
      private$add.tickets.table('raw', tickets[which(na.count == 5), ], 'Working', tickets.columns$Working, tickets.columns$Uniform)
      self$format.tickets()
      self$sort.tickets()
    } # FINISH
    
  ),
  #### + PRIVATE ####
  private = list(
    m.type = REPORT_TYPE['MT4.TRADE'],
    
    init.Infos = function(html.parse) {
      # ''' init infos '''
      # 2017-01-16: Version 1.0
      infos <- sapply(getNodeSet(html.parse, '//b')[1:8], xmlValue)
      time.index <- which(grepl('Trans', infos)) - 1
      others <- infos[2:(time.index - 1)]
      self$set.infos.column('Account', others[grep('Account', others)])
      self$set.infos.column('Name', others[grep('Name', others)])
      self$set.infos.column('Broker', infos[1])
      self$set.infos.column('Currency', others[grep('Currency', others)])
      self$set.infos.column('Leverage', others[grep('Leverage', others)])
      self$set.infos.column('Time', infos[time.index])
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
    initialize = function(file.path, file.name, html.parse) {
      # ''' init '''
      # 2017-01-21: Version 1.0
      super$initialize()
      super$init.Infos(file.path, file.name)
      private$m.html.parse <- html.parse
      private$init.Infos(private$get.html.table())
    },# FINISH
    
    init.else = function(db, timeframe, format.digits) {
      # ''' init else '''
      # 2017-01-25: Version 1.0
      super$init.else(db, timeframe, format.digits)
      new.tickets <- private$end.of.test.is.so()
      new.tickets <- private$recal.tickets.profits(tickets=new.tickets, db, timeframe, format.digits)
      self$set.tickets.member('raw', new.tickets)
    },# FINISH
    
    init.raw.tickets = function(tickets.columns) {
      # ''' get all tickets from html table '''
      # 2017-01-24: Version 1.1 format & sort
      # 2017-01-21: Version 1.0
      tickets.table <- private$get.html.table()[[2]]
      first.col <- tickets.table[, 1]
      spaces.index <- which(first.col == '')
      deals <- .html.mt5.ea_trade.tickets.block(tickets.table, first.col, spaces.index, 'Deals')
      tickets.list <- private$get.html.mt5.money_closed_open(deals, tickets.columns)
      self$format.tickets()
      self$sort.tickets()
    } # FINISH
    
  ),
  private = list(
    m.type = REPORT_TYPE['MT5.EA'],
    
    init.Infos = function(html.table) {
      # ''' init infos '''
      # 2017-01-16: Version 1.0
      info.table <- html.table[[1]]
      labels <- info.table[, 1]
      values <- info.table[, 2]
      time.string <- values[which(grepl('Period', labels))[1]]
      nchar.time.string <- nchar(time.string)
      self$set.infos.column('Name', values[which(grepl('Expert', labels))[1]])
      self$set.infos.column('Broker', values[which(grepl('Broker', labels))[1]])
      self$set.infos.column('Currency', values[which(grepl('Currency', labels))[1]])
      self$set.infos.column('Leverage', values[which(grepl('Leverage', labels))[1]])
      self$set.infos.column('Time', substr(time.string, nchar.time.string - 10, nchar.time.string - 1))
    },# FINISH
    
    end.of.test.is.so = function() {
      tickets <- self$get.tickets.member('raw')
      close.at.stop.index <- which(grepl('end of test', tickets$COMMENT))
      time <- self$get.infos.column('Time')
      difftimes <- time - as.numeric(tickets$CTIME[close.at.stop.index])
      tickets$EXIT[close.at.stop.index] <- ifelse(difftimes > 60, 'SO', '')
      tickets
    },
    
    get.html.mt5.money_closed_open = function(deals, tickets.columns, position=NULL) {
      # ''' get money, closed open tickets from mt5 html '''
      # 2017-01-21: Version
      if (is.null(deals) || nrow(deals) == 0) {
        return(NULL)
      }
      money <- subset(deals, subset = Type == 'balance')
      tickets.money.temp <- .html.mt5.ea_trade.tickets.money(money)
      private$add.tickets.table('raw', tickets.money.temp, 'Money', tickets.columns$Money, tickets.columns$Uniform)
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
            OTIME = .format.time(Time[open.tickets.in.index]),
            TYPE = type,
            VOLUME = tickets.volume,
            ITEM = item,
            OPRICE = Price[open.tickets.in.index],
            CPRICE = positions[item]
          )
        })
        private$add.tickets.table('raw', tickets.open.temp, 'Open', tickets.columns$Open, tickets.columns$Uniform)
      }
      if (length(closed.index) > 0) {
        closed.tickets.in.index <- tickets.in.index[closed.index]
        closed.tickets.out.index <- tickets.out.index[closed.index]
        tickets.closed.temp <- with(symbol.trades, {
          data.frame(
            stringsAsFactors = F,
            row.names = NULL,
            TICKET = Order[closed.tickets.in.index],
            OTIME = .format.time(Time[closed.tickets.in.index]),
            TYPE = type,
            VOLUME = tickets.volume,
            ITEM = item,
            OPRICE = symbol.trades$Price[closed.tickets.in.index],
            CTIME = .format.time(Time[closed.tickets.out.index]),
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
        private$add.tickets.table('raw', tickets.closed.temp, 'Closed', tickets.columns$Closed, tickets.columns$Uniform)
      }
    } # FINISH
  )
)

#### ++ HTML MT5 Trade : HTML REPORT : REPORT ####

MetaQuote.HTML.MT5Trade.Report <- R6Class(
  classname = 'MetaQuote HTML MT5 Trade Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.path, file.name, html.parse) {
      # ''' init '''
      # 2017-01-21: Version 1.0
      super$initialize()
      super$init.Infos(file.path, file.name)
      private$m.html.parse <- html.parse
      private$init.Infos(private$get.html.table())
    },# FINISH
    
    init.else = function(db, timeframe, format.digits) {
      # ''' init else '''
      # 2017-01-25: Version 1.0
      super$init.else(db, timeframe, format.digits)
      new.tickets <- private$recal.tickets.profits(tickets=self$get.tickets.member('raw'), db, timeframe, format.digits)
      self$set.tickets.member('raw', new.tickets)
    },# FINISH
    
    init.raw.tickets = function(tickets.columns) {
      # ''' get all tickets from html table '''
      # 2017-01-24: Version 1.1 format & sort
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
      self$format.tickets()
      self$sort.tickets()
    } # FINISH
  ),
  private = list(
    m.type = REPORT_TYPE['MT5.TRADE'],
    
    init.Infos = function(html.table) {
      # ''' init infos '''
      # 2017-01-16: Version 1.0
      html.table <- html.table[[1]]
      head.info <- html.table$V2[1:4]
      self$set.infos.column('Account', head.info[2])
      self$set.infos.column('Name', head.info[1])
      self$set.infos.column('Broker', head.info[3])
      self$set.infos.column('Currency', head.info[2])
      self$set.infos.column('Leverage', head.info[2])
      self$set.infos.column('Time', self$set.infos.column('Time', head.info[4]) - 8 * 3600)
    },# FINISH

    get.html.mt5.money_closed_open = function(deals, tickets.columns, positions=NULL) {
      # ''' get money, closed open tickets from mt5 html '''
      # 2017-01-21: Version
      if (is.null(deals) || nrow(deals) == 0) {
        return(NULL)
      }
      money <- subset(deals, subset = Type == 'balance')
      tickets.money.temp <- .html.mt5.ea_trade.tickets.money(money)
      private$add.tickets.table('raw', tickets.money.temp, 'Money', tickets.columns$Money, tickets.columns$Uniform)
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
            OTIME = .format.time(Time[open.tickets.in.index]),
            TYPE = type,
            VOLUME = tickets.volume,
            ITEM = item,
            OPRICE = Price[open.tickets.in.index],
            CPRICE = positions[item]
          )
        })
        private$add.tickets.table('raw', tickets.open.temp, 'Open', tickets.columns$Open, tickets.columns$Uniform)
      }
      if (length(closed.index) > 0) {
        closed.tickets.in.index <- tickets.in.index[closed.index]
        closed.tickets.out.index <- tickets.out.index[closed.index]
        tickets.closed.temp <- with(symbol.trades, {
          data.frame(
            stringsAsFactors = F,
            row.names = NULL,
            TICKET = Order[closed.tickets.in.index],
            OTIME = .format.time(Time[closed.tickets.in.index]),
            TYPE = type,
            VOLUME = tickets.volume,
            ITEM = item,
            OPRICE = symbol.trades$Price[closed.tickets.in.index],
            CTIME = .format.time(Time[closed.tickets.out.index]),
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
        private$add.tickets.table('raw', tickets.closed.temp, 'Closed', tickets.columns$Closed, tickets.columns$Uniform)
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
      OTIME = .format.time(Time),
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
      super$initialize()
      super$init.Infos(file.path, file.name)
      private$init.Infos()
    },# FINISH
    init.raw.tickets = function(tickets.columns) {
      # ''' get all tickets from html table '''
      # 2017-01-24: Version 1.1 format & sort
      # 2017-01-21: Version 1.0
      tickets.table <- private$get.html.table()[[1]]
      tickets.table <- tickets.table[2:(nrow(tickets.table) - 1), ]
      colnames(tickets.table) <- c('TICKET', 'LOGIN', '', 'OTIME', 'TYPE', 'ITEM', 'VOLUME', 'OPRICE', 'CTIME', 'CPRICE',
                                   'COMMISSION', 'TAXES', '', 'SWAP', 'PROFIT', '', 'COMMENT')
      new.comment <- with(tickets.table, paste(COMMENT, LOGIN, sep = ' | Login: '))
      tickets.table$COMMENT <- gsub('^ [|] ', '', new.comment)
      private$add.tickets.table('raw', tickets.table, 'Closed', tickets.columns$Closed, tickets.columns$Uniform)
      self$format.tickets()
      self$sort.tickets()
    } # FINISH
  ),
  private = list(
    m.type = REPORT_TYPE['MT4M.CLOSED'],
    
    init.Infos = function() {
      # ''' init infos '''
      # 2017-01-16: Version 0.2
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
      super$initialize()
      super$init.Infos(file.path, file.name)
      private$init.infos()
    },# FINISH
    init.raw.tickets = function(tickets.columns) {
      # ''' get all tickets from html table '''
      # 2017-01-24: Version 1.1 format & sort
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
        private$add.tickets.table('raw', tickets.table[money.index, ], 'Money', tickets.columns$Money, tickets.columns$Uniform)
        tickets.table <- tickets.table[-money.index, ]
      }
      ## pending tickets ##
      pending.index <- which(grepl('(buy|sell) (limit|stop)', tickets.table$TYPE))
      if (length(pending.index) > 0) {
        private$add.tickets.table('raw', tickets.table[pending.index, ], 'Pending', tickets.columns$Pending, tickets.columns$Uniform)
        tickets.table <- tickets.table[-pending.index, ]
      }
      if (nrow(tickets.table) > 0) {
        private$add.tickets.table('raw', tickets.table, 'Closed', tickets.columns$Closed, tickets.columns$Uniform)
      }
      self$format.tickets()
      self$sort.tickets()
    } # FINISH
  ),
  private = list(
    m.type = REPORT_TYPE['MT4M.RAW'],
    
    init.infos = function() {
      # ''' set infos '''
      # 2017-01-16: Version 0.2
    } # FINISH

  )
)


.report.tickets.exit <- cmpfun(function(comments) {
  # ''' get report tickets column: exit from comment'''
  # 2017-01-17: Version 1.1 add support for comments type - data.frame
  # 2016-12-01: Version 1.0
  comments <- toupper(comments)
  comments <- gsub('/| / ', '', comments)
  exit <- vector(mode = 'character', length = length(comments))
  exit[grep('SO', comments)] <- 'SO'
  exit[grep('SL', comments)] <- 'SL'
  exit[grep('TP', comments)] <- 'TP'
  exit
})# FINISH


.output.csv <- cmpfun(function(tickets, groups, columns, filename, file) {
  if ('FILE' %in% columns) {
    tickets$FILE <- filename
  }
  selected.columns <- c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
                        'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', columns)
  sub.tickets <- subset(tickets, subset = GROUP %in% groups, select = selected.columns)
  write.csv(sub.tickets, file = file, row.names = FALSE)
})