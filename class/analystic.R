##
## names(x) <- c(1,2,3) ==>> setNames(x, c(1,2,3))
## during calculate profit, if pips == 0, no need to cal tickvalue
## 
## merged report if unique(TICKET) > 1, set new serie of Ticket Number

## 2017-01-13: Create

require(R6)
require(compiler)


#### MQAnalystic ####

MQAnalystic <- R6Class(
  classname = 'MetaQuote Analystic',
  #### + PUBLIC ####
  public = list(
    initialize = function() {
      # ''' initialize '''
      # 2017-01-23: ToDo
      self$set.DataCenter()
      self$set.tickets.columns()
      self$set.default.currency()
      self$set.default.leverage()
      self$set.symbol.table()
      ## --
      
    },
    #### ++ Getter & Setter ####
    
    #### +++ DataCenter ####
    get.DataCenter = function() {
      # ''' get DataCenter '''
      # 2017-01-23: Version 1.0
      if (is.null(private$m.DataCenter)) {
        self$set.DataCenter()
      }
      private$m.DataCenter
    },# FINISH
    set.DataCenter = function(DataCenter=private$default.DataCenter()) {
      # ''' set DataCenter '''
      # 2017-01-23: Version 1.0
      private$m.DataCenter <- DataCenter
    },# FINISH
    
    #### +++ tickets columns ####
    get.tickets.columns = function() {
      # ''' get tickets columns '''
      # 2017-01-23: Version 1.0
      if (is.null(private$m.tickets.columns)) {
        self$set.tickets.columns()
      }
      private$m.tickets.columns
    },# FINISH
    set.tickets.columns = function(tickets.columns=private$default.tickets.columns()) {
      # ''' set tickets columns '''
      # 2017-01-23: Version 1.0
      private$m.tickets.columns <- tickets.columns
    },# FINISH
    
    #### +++ default currency ####
    get.default.currency = function() {
      # ''' get default currency '''
      # 2017-01-23: Version 1.0
      if (is.null(private$m.default.currency)) {
        self$set.default.currency()
      }
      private$m.default.currency
    },# FINISH
    set.default.currency = function(default.currency=private$default.currency()) {
      # ''' set default currency '''
      # 2017-01-23: Version 1.0
      private$m.default.currency <- default.currency
    },# FINISH
    
    #### +++ default leverage ####
    get.default.leverage = function() {
      # ''' get default leverage '''
      # 2017-01-23: Version 1.0
      if (is.null(private$m.default.leverage)) {
        self$set.default.leverage()
      }
      private$m.default.leverage
    },# FINISH
    set.default.leverage = function(default.leverage=private$default.leverage()) {
      # ''' set default leverage '''
      # 2017-01-23: Version 1.0
      private$m.default.leverage <- default.leverage
    },# FINISH
    
    #### +++ selected index ####
    get.selected.index = function() {
      # ''' get selected index '''
      # 2017-01-23: Version 1.0
      private$m.selected.index
    },# FINISH
    set.selected.index = function(index) {
      # ''' set selected index '''
      # 2017-01-23: Version 1.0
      if (length(index) < 1) {
        return(NULL)
      }
      private$m.selected.index <- index
    },# FINISH

    #### +++ symbol table ####
    get.symbol.table = function() {
      # ''' get selected index '''
      # 2017-01-23: Version 1.0
      if (is.null(private$m.symbol.table)) {
        self$set.symbol.table()
      }
      private$m.symbol.table
    },# FINISH
    set.symbol.table = function(symbol.table=private$default.symbol.table()) {
      # ''' set selected index '''
      # 2017-01-23: Version 1.0
      private$m.symbol.table <- symbol.table
    },# FINISH
    
    
    
    #### ++ PUBLIC ACTIONS ####
    
    #### +++ files ####
    add.files = function(file.path) {
      # ''' add files '''
      # 2017-01-13: Version 1.0
      if (is.data.frame(file.path)) {
        for (i in 1:nrow(file.path)) {
          private$input.file(file.path[i, ])
        }
      } else {
        lapply(file.path, FUN = private$input.file)
      }
    },# FINISH
    clear.files = function() {
      # ''' clear all files '''
      # 2017-01-13: Version 1.0
      private$m.unsupported.files <- NULL
      private$m.Reports <- NULL
      private$m.selected.index <- NULL
    },# FINISH
    get.unsupported.files = function(index) {
      # ''' get unsupported files '''
      # 2017-01-14: Version: 1.0
      if (missing(index)) {
        private$m.unsupported.files
      } else {
        private$m.unsupported.files[[index]]
      }
    },# FINISH
    get.all.Reports.infos = function() {
      all.reports <- self$get.Reports()
      infos <- lapply(all.reports, function(x) x$get.infos.column())
      do.call(rbind, infos)
    },
    
    #### +++ analystics ####
    
    #### +++ output ####
    
    #### ++ BEHAVIOR ####
    one.by.one.do = function(fun) {
      # ''' do fun one by one reports '''
      # 2017-01-23: Version: 0.1
      selected.reports <- private$get.selected.Reports()
      if (is.null(selected.reports)) {
        return(NULL)
      }
      lapply(selected.reports, fun)
    },# FINISH
    merge.do = function(fun) {
      # ''' do fun by merge reports '''
      # 2017-01-23: Version: 0.1
      merged.reports <- private$get.selected.merge.Reports()
      if (is.null(merged.reports)) {
        return(NULL)
      }
      fun(merged.reports)
    },# FINISH
    
    
    
    
    
    set.analyzing.report = function() {
      index <- self$get.selected.index()
      if (is.null(index)) {
        return(NULL)
      }
      analyzing.Report <- private$get.selected.merge.Reports()
      
      #### ToDo ####
    },
    get.Reports = function(index) {
      # ''' get supported reports '''
      # 2017-01-14: Version: 1.0
      if (missing(index)) {
        private$m.Reports
      } else {
        private$m.Reports[index]
      }
    },# FINISH
    
    #### Output ####
    output.tickets = function(index, groups, columns, filename, file) {
      # ''' output tickets as .csv '''
      # 2017-01-30: Version 
      self$set.selected.index(index)
      if (length(index) > 1) {
        Report <- private$get.selected.merge.Reports()
      } else {
        Report <- self$get.Reports(index)[[1]]
      }
      tickets <- Report$get.tickets.member('raw')
      if (is.null(tickets)) {
        tickets <- Report$init.raw.tickets(self$get.tickets.columns())
      }
      Report$output.tickets(tickets, groups, columns, filename, file)
    },
    init.others = function(Report, tickets.columns = self$get.tickets.columns(),
                           default.currency = self$get.default.currency(),
                           default.leverage = self$get.default.leverage(),
                           symbol.table = self$get.symbol.table(),
                           db = self$get.DataCenter(),
                           timeframe = 'H1',
                           format.digits = 2, 
                           reset = FALSE) {
      Report$init.others(tickets.columns = tickets.columns,
                         default.currency = default.currency,
                         default.leverage = default.leverage,
                         symbol.table = symbol.table,
                         db = db,
                         timeframe = timeframe,
                         format.digits = format.digits, 
                         reset = reset)
      Report
    },
    
    #### TESTING ####
    TESTING = function() {
      # ''' RIGHT NOW JUST FOR TESTING '''
      # 2017-01-21: Version
      
      # self$merge.do(function(x) {
      #   print(x$get.infos.column())
      # })
      t.time <- system.time(
        res <- self$merge.do(function(x) {
          # x$init.raw.tickets(self$get.tickets.columns())
          # print(x$get.tickets.member('raw'))
          
          x$init.others(tickets.columns = self$get.tickets.columns(),
                        default.currency = self$get.default.currency(),
                        default.leverage = self$get.default.leverage(),
                        symbol.table = self$get.symbol.table(),
                        db = self$get.DataCenter(),
                        timeframe = 'H1',
                        format.digits = 2, 
                        reset = FALSE)
          x
        })
      )
      print(paste('The TESTING Time in Fun is:', paste(format(t.time[1:3], digits = 2), collapse = ' ')))
      res
    }
  ),
  #### + PRIVATE ####
  private = list(
    #### ++ CONFIG ####
    m.DataCenter = NULL,
    m.tickets.columns = NULL,
    m.default.currency = NULL,
    m.default.leverage = NULL,
    m.symbol.table = NULL,
    
    #### ++ MEMBER ####
    m.unsupported.files = NULL,
    m.Reports = NULL,
    m.selected.index = NULL,
    m.analyzing.Report = NULL,
    
    #### ++ DEFAULT FUNCTIONS ####
    
    #### +++ DataCenter ####
    default.DataCenter = function() {
      # ''' default DataCenter '''
      # 2017-01-23: Version 1.0
      DataCenter$new()
    },# FINISH
    
    #### +++ tickets columns ####
    default.tickets.columns = function() {
      # ''' default tickets columns '''
      # 2017-01-23: Version 1.0
      list(
        Uniform = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
                    'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', 'GROUP', 'COMMENT'),
        Money = c('TICKET', 'OTIME', 'PROFIT'),
        Closed = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
                   'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT'),
        Open = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
                 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT'),
        Pending = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
                    'CTIME', 'CPRICE'),
        Working = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP', 'CPRICE')
      )
    },# FINISH
    
    #### +++ default currency ####
    default.currency = function() {
      # ''' default currency '''
      # 2017-01-23: Version 1.0
      'USD'
    },# FINISH
    
    #### +++ default leverage ####
    default.leverage = function() {
      # ''' default leverage '''
      # 2017-01-23: Version 1.0
      100
    },# FINISH
    
    #### +++ default symbol table ####
    default.symbol.table = function() {
      # ''' default symbol table '''
      # 2017-01-23: Version 1.0
      data.frame(
        stringsAsFactors = F,
        row.names = c('AUDCAD', 'AUDCHF', 'AUDJPY', 'AUDNZD', 'AUDUSD', 'CADCHF', 'CADJPY', 'CHFJPY', 'EURAUD', 'EURCAD',
                      'EURCHF', 'EURGBP', 'EURJPY', 'EURNZD', 'EURUSD', 'GBPAUD', 'GBPCAD', 'GBPCHF', 'GBPJPY', 'GBPNZD',
                      'GBPUSD', 'NZDCAD', 'NZDCHF', 'NZDJPY', 'NZDUSD', 'USDCAD', 'USDCHF', 'USDJPY', 'XAGUSD', 'XAUUSD'),
        SPREAD = c(40,40,40,40,20,40,40,40,40,40,40,40,40,40,20,40,20,40,40,40,20,40,40,40,20,20,20,40,50,500),
        DIGITS = c(5,5,3,5,5,5,3,3,5,5,5,5,3,5,5,5,5,5,3,5,5,5,5,3,5,5,5,3,3,2),
        CON_SIZE = c(rep(100000, 28), 1000, 100)
      )
    },# FINISH
    
    
    #### ++ PRIVATE ACTIONS ####
    
    #### +++ files #####
    input.file = function(file.path) {
      # ''' input one file '''
      # 2017-01-30: Version 1.1 for fileInput
      # 2017-01-14: Version 1.0
      if (is.data.frame(file.path)) {
        file.name <- file.path$name
        file.path <- file.path$datapath
      } else {
        file.name <- .file.name(file.path)
      }
      file.extension <- .file.extension(file.name)
      report <- .read.file(file.path, file.name, file.extension)
      ifelse(is.null(report), private$add.unsupported.file(file.name), private$add.Reports(report))
    },# FINISH
    add.unsupported.file = function(file) {
      # ''' add unsupported files '''
      # 2017-01-13: Version 1.0
      private$m.unsupported.files <- c(private$m.unsupported.files, file)
    },# FINISH
    add.Reports = function(file) {
      # ''' add supported files '''
      # 2017-01-13: Version 1.0
      private$m.Reports <- c(private$m.Reports, file)
    },# FINISH

    #### +++ Reports ####
    get.selected.Reports = function() {
      # ''' get selected index for Reports '''
      # 2017-01-13: Version 1.0
      index <- self$get.selected.index()
      if (is.null(index) || length(index) == 0) {
        return(NULL)
      }
      self$get.Reports(index)[index]
    },# FINISH
    get.selected.merge.Reports = function() {
      # ''' get merged reports '''
      # 2017-01-21: Version
      reports <- private$get.selected.Reports()
      if (length(reports) == 1) {
        return(reports[[1]])
      }
      merged.Report <- MetaQuote.Report$new()
      infos <- do.call(rbind, lapply(reports, function(report) {
        report$get.infos.column()
      }))
      merged.Report$set.infos.column(value = infos)
      raw <- do.call(rbind, lapply(reports, function(report) {
        raw.tickets <- report$get.tickets.member('raw')
        if (is.null(raw.tickets)) {
          raw.tickets <- report$init.raw.tickets(self$get.tickets.columns())
        }
        raw.tickets
      }))
      merged.Report$set.tickets.member('raw', raw)
      merged.Report$sort.tickets()
      merged.Report
    },
    
    
    
    
    
    
    
    
    
    ## CONFIG ##
    
    ## MEMBER ##
    m.analyzing.report = NULL
    

    # report.init.tickets = function(report) {
    #   # ''' merge reports '''
    #   # 2017-01-21: Version 1.0
    #   report$init.tickets(private$m.tickets.columns)
    # } # FINISH
  )
)

#### FILE READER - Utils ####

.file.name <- cmpfun(function(file.path) {
  # ''' get file name '''
  # 2017-01-30: Version 1.1 for fileInput
  # 2016-08-11: Version 1.0
  if (is.data.frame(file.path)) {
    return(file.path$name)
  }
  basename(file.path)
})# FINISH

.file.extension <- cmpfun(function(file.path) {
  # ''' get file extension '''
  # 2017-01-13: Version 1.1 add support for none extension file
  # 2016-08-11: Version 1.0
  file.path.split <- strsplit(file.path, '.', fixed = T)[[1]]
  ifelse(length(file.path.split) > 1, tolower(tail(file.path.split, 1)), '')
})# FINISH

.read.file <- cmpfun(function(file.path, file.name, file.extension) {
  # ''' read file '''
  # 2017-01-13: ToDo
  if (grepl('htm|html', file.extension)) {
    html.parse <- htmlParse(file.path, encoding = 'UTF-8')
    html.title <- xmlValue(getNodeSet(html.parse,'//title')[[1]])
    if (grepl('Strategy Tester:', html.title)) {
      return(MetaQuote.HTML.MT4EA.Report$new(file.path, file.name, html.parse))
    }
    if (grepl('Statement:', html.title)) {
      return(MetaQuote.HTML.MT4Trade.Report$new(file.path, file.name, html.parse))
    }
    if (grepl('Strategy Tester Report', html.title)) {
      return(MetaQuote.HTML.MT5EA.Report$new(file.path, file.name, html.parse))
    }
    if (grepl('Trade History Report', html.title)) {
      return(MetaQuote.HTML.MT5Trade.Report$new(file.path, file.name, html.parse))
    }
    if (grepl('Closed Trades Report', html.title, file.name)) {
      return(MetaQuote.HTML.MT4M_Closed.Report$new(file.path, file.name))
    }
    if (grepl('Raw Report', html.title, file.name)) {
      return(MetaQuote.HTML.MT4M_Raw.Report$new(file.path, file.name))
    }
    return(NULL)
    # MetaQuote.HTML.Report$new(file.path)
  } else if (grepl('xlsx|xls', file.extension)) {
    return(NULL)
    #### ToDo ####
    # xlsx_table <- read.xlsx(file.path)
    # print(xlsx_table)
    # file.csv_xlsx(file.path, xlsx_table)
  } else if (grepl('csv', file.extension)) {
    return(NULL)
    #### ToDo ####
    # csv_table <- read.csv(file.path, encoding = 'UTF-8')
    # file.csv_xlsx(file.path, csv_table)
  } else {
    NULL
  }
})

