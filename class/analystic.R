## 2017-01-13: Create

require(R6)
require(compiler)


#### MQAnalystic ####

MQAnalystic <- R6Class(
  classname = 'MetaQuote Analystic',
  public = list(
    initialize = function() {

    },# FINISH
    add.files = function(file.path) {
      # ''' add files '''
      # 2017-01-13: Version 0.1
      lapply(file.path, FUN = private$input.file)
    },# TESTING
    clear.files = function() {
      # ''' clear all files '''
      # 2017-01-13: Version 0.1
      private$m.unsupported.files <- NULL
      private$m.reports <- NULL
    },# TESTING
    get.unsupported.file = function(index) {
      # ''' get unsupported files '''
      # 2017-01-14: Version: 0.1
      if (missing(index)) {
        private$m.unsupported.files
      } else {
        private$m.unsupported.files[[index]]
      }
    },# TESTING
    get.select.index = function() {
      private$m.select.index
    },
    set.select.index = function(index) {
      if (length(index) < 1) {
        return(NULL)
      }
      private$m.select.index <- index
    },
    set.analyzing.report = function() {
      index <- self$get.select.index()
      if (is.null(index)) {
        return(NULL)
      }
      private$merge.reports(self$get.reports(index))
      #### ToDo ####
    },
    get.reports = function(index) {
      # ''' get supported reports '''
      # 2017-01-14: Version: 1.0
      if (missing(index)) {
        private$m.reports
      } else {
        private$m.reports[index]
      }
    },# FINISH
    TESTING = function() {
      # ''' RIGHT NOW JUST FOR TESTING '''
      # 2017-01-21: Version
      reports = self$get.reports(c(1, 10))
      lapply(reports, function(report) {
        r <- private$report.init.tickets(report)
        print(r$get.tickets())
      })
      
    }
  ),
  private = list(
    ## CONFIG ##
    m.tickets.columns = list(
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
    ),
    
    ## MEMBER ##
    m.unsupported.files = NULL,
    m.reports = NULL,
    m.select.index = NULL,
    m.analyzing.report = NULL,
    
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
    },
    
    
    
    input.file = function(file.path) {
      # ''' input one file '''
      # 2017-01-14: Version 0.1
      file.name <- .file.name(file.path)
      file.extension <- .file.extension(file.name)
      report <- .read.file(file.path, file.name, file.extension)
      ifelse(is.null(report), private$add.unsupported.file(file.name), private$add.reports(report))
    },
    add.unsupported.file = function(file) {
      # ''' add unsupported files '''
      # 2017-01-13: Version 0.1
      private$m.unsupported.files <- c(private$m.unsupported.files, file)
    },# TESTING
    add.reports = function(file) {
      # ''' add supported files '''
      # 2017-01-13: Version 0.1
      private$m.reports <- c(private$m.reports, file)
    },# TESTING
    merge.reports = function(reports) {
      # ''' merge reports '''
      # 2017-01-21: Version 0.1
      if (length(reports) == 1) {
        return(reports[[1]])
      }
      report <- MetaQuote.Report$new()
      infos <- do.call(rbind, lapply(reports, FUN = function(report) report$get.infos.dataframe()))
    },
    report.init.tickets = function(report) {
      # ''' merge reports '''
      # 2017-01-21: Version 1.0
      report$init.tickets(private$m.tickets.columns)
    } # FINISH
  )
)

#### FILE READER - Utils ####

.file.name <- cmpfun(function(file.path) {
  # ''' get file name '''
  # 2016-08-11: Version 1.0
  tail(strsplit(file.path, '/', fixed = T)[[1]], 1)
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
  # 2017-01-13:
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
      return(MetaQuote.HTML.MT5EA.Report$new(file.path, file.name))
    }
    if (grepl('Trade History Report', html.title)) {
      return(MetaQuote.HTML.MT5Trade.Report$new(file.path, file.name))
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

