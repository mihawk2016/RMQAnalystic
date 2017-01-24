## tick.value & margin.required both need Open from DataBase, try to merge

require(R6)
require(compiler)

Symbol.Table <- R6Class(
  classname = 'Symbol Table',
  public = list(
    initialize = function(symbol.file='./symbols_new.rdata') {
      private$m.symbol.file <- symbol.file

    },
    get.symbol.table = function() {
      if (is.null(private$m.symbol.table)) {
        self$set.symbol.table(self$load.local.file())
      }
      private$m.symbol.table
    },
    set.symbol.table = function(symbol.table) {
      private$m.symbol.table <- symbol.table
    },
    load.symbol.file = function(file=private$m.symbol.file) {
      if (!file.exists(file)) {
        file.create(file)
        self$set.symbol.table(private$set.default.table())
        self$save.symbol.table()
        return(NULL)
      }
      tryCatch(
        get(load(file)),
        error = function(e) {
          message('Symbols File ERROR')
          NULL
        }
      )
    },
    save.symbol.file = function(file=private$m.symbol.file) {
      symbol.table <- self$get.symbol.table()
      tryCatch(
        save(symbol.table, file = file),
        error = function(e)  {
          message('Local Data File ERROR')
          NULL
        }
      )
    },
    get.support.symbols = function() {
      rownames(self$get.symbol.table())
    },

    get.report.table = function() {
      private$m.report.table
    },
    set.report.table = function(currency='USD', leverage=100) {
      symbols <- unique(self$get.mapping())
      symbols <- symbols[symbols != '']
      table <- self$get.symbol.table()[symbols, ]
      table <- within(table, {
        TICKVALUE_POINT <- CON_SIZE * 10 ^ -DIGITS
        MARGINREQUIRED_POINT <- CON_SIZE / leverage
      })
      base.currency <- private$symbol.base.currency(symbols)
      quote.currency <- private$symbol.quote.currency(symbols)
      
    },
    
    #,
    # get.symbols = function(items) {
    #   
    # }
    
    get.tick.value = function(symbol, times, db, symbol.table=private$symbol.table, currency='USD', timeframe='M1') {
      # ''' get tick value '''
      # 2017-01-23: Version 0.1
      base.currency <- private$symbol.base.currency(symbol)
      tick.value.point <- with(symbol.table[symbol, ], CON_SIZE * 10 ^ -DIGITS)
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
    
    get.margin.required = function(symbol, times, db, symbol.table=private$symbol.table, currency='USD', leverage=100, timeframe='M1') {
      # ''' get margin required '''
      # 2017-01-23: Version 0.1
      quote.currency <- private$symbol.quote.currency(symbol)
      margin.required.point <- with(symbol.table[symbol, ], CON_SIZE / leverage)
      if (quote.currency == currency) {
        return(margin.required.point)
      }
      target.symbol <- private$build.symbol(quote.currency, currency)
      target.open <- db$get.open(target.symbol, times, timeframe)
      if (quote.currency == private$symbol.quote.currency(target.symbol)) {
        return(margin.required.point * target.open)
      }
      margin.required.point / target.open
    }
  ),
  private = list(
    m.symbol.file = NULL,
    m.symbol.table = NULL,
    m.mapping = NULL,
    m.report.table = NULL,
    
    set.default.table = function() {
      data.frame(
        stringsAsFactors = F,
        row.names = c('AUDCAD', 'AUDCHF', 'AUDJPY', 'AUDNZD', 'AUDUSD', 'CADCHF', 'CADJPY', 'CHFJPY', 'EURAUD', 'EURCAD',
                      'EURCHF', 'EURGBP', 'EURJPY', 'EURNZD', 'EURUSD', 'GBPAUD', 'GBPCAD', 'GBPCHF', 'GBPJPY', 'GBPNZD',
                      'GBPUSD', 'NZDCAD', 'NZDCHF', 'NZDJPY', 'NZDUSD', 'USDCAD', 'USDCHF', 'USDJPY', 'XAGUSD', 'XAUUSD'),
        SPREAD = c(40,40,40,40,20,40,40,40,40,40,40,40,40,40,20,40,20,40,40,40,20,40,40,40,20,20,20,40,50,500),
        DIGITS = c(5,5,3,5,5,5,3,3,5,5,5,5,3,5,5,5,5,5,3,5,5,5,5,3,5,5,5,3,3,2),
        CON_SIZE = c(rep(100000, 28), 1000, 100)
      )
    },
    
    build.symbol = function(currency1, currency2, symbols = self$get.support.symbols()) {
      # ''' build symbol from 2 currencies '''
      # 2016-08-12: Version 1.0
      match.currency1 <- support.symbols[str_detect(support.symbols, currency1)]
      symbol <- match.currency1[str_detect(match.currency1, currency2)]
      if (length(symbol) == 1) {
        return(symbol) 
      }
      ''
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
    } # FINISH
  )
)
