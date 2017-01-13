## 2017-01-13: Create

require(R6)
require(compiler)

#### FILE ####

File <- R6Class(
  classname = 'File',
  public = list(
    initialize = function(file.link) {
      file.extension <- .file.extension(private$m.file.name <- .file.name(file.link))
    },
    is.supprted = function() {
      private$m.is.supported
    },
    get.file.name = function(file.link) {
      private$m.file.name
    }
  ),
  private = list(
    m.is.supported = FALSE,
    m.file.name = NULL
  )
)

#### + FILE - Read File ####

.read.file <- cmpfun(function(file.link) {
  # ''' read file '''
  # 2017-01-13:
  if (grepl('htm|html', file.extension)) {
    #### ToDo ####
    html.parse <- .html.parse(file.link)
    html.title <- xmlValue(getNodeSet(html.parse,'//title')[[1]])
    if (grepl('Strategy Tester:', html.title)) {
      #### ToDo ####
      return(.read.html.mt4.ea(file.link, html.parse))
    }
    if (grepl('Statement:', html.title)) {
      #### ToDo ####
      return(.read.html.mt4.trade(file.link, html.parse))
    }
    if (grepl('Strategy Tester Report', html.title)) {
      #### ToDo ####
      return(.read.html.mt5.ea(file.link))
    }
    if (grepl('Trade History Report', html.title)) {
      #### ToDo ####
      return(.read.html.mt5.trade(file.link))
    }
    if (grepl('Closed Trades Report', html.title)) {
      #### ToDo ####
      return(.read.html.mt4m.closed(file.link))
    }
    if (grepl('Raw Report', html.title)) {
      #### ToDo ####
      return(.read.html.mt4m.raw(file.link))
    }
    
    # MetaQuote.HTML.Report$new(file.link)
  } else if (grepl('xlsx|xls', file.extension)) {
    #### ToDo ####
    xlsx_table <- read.xlsx(file.link)
    print(xlsx_table)
    file.csv_xlsx(file.link, xlsx_table)
  } else if (grepl('csv', file_extension)) {
    #### ToDo ####
    csv_table <- read.csv(file.link, encoding = 'UTF-8')
    file.csv_xlsx(file.link, csv_table)
  } else {
    NULL
  }
})

#### + FILE - Utils ####

.file.name <- cmpfun(function(file.link) {
  # ''' get file name '''
  # 2016-08-11: version 1.0
  tail(strsplit(file.link, '/', fixed = T)[[1]], 1)
})# FINISH

.file.extension <- cmpfun(function(file.link) {
  # ''' get file extension '''
  # 2017-01-13: version 1.1 add support for none extension file
  # 2016-08-11: version 1.0
  file.link.split <- strsplit(file.link, '.', fixed = T)[[1]]
  ifelse(length(file.link.split) > 1, tolower(tail(file.link.split, 1)), '')
})# FINISH

.html.parse <- cmpfun(function(file.link) {
  htmlParse(file.link, encoding = 'UTF-8')
})

#### REPORT ####

MetaQuote.Report <- R6Class(
  classname = 'MetaQuote Report',
  public = list(
    initialize = function(file.link) {
      
    }
  ),
  private = list(
    m.file = NA,
    m.account = NA,
    m.group = NA,
    m.broker = NA,
    m.currency = NA,
    m.leverage = NA,
    m.time = NA
    
  )
)

#### + HTML REPORT : REPORT ####

MetaQuote.HTML.Report <- R6Class(
  classname = 'MetaQuote HTML Report',
  inherit = MetaQuote.File,
  public = list(
    initialize = function(file.link) {
      html.parse <- private$m.html.parse <- .html.parse(file.link)
      html.title <- xmlValue(
        getNodeSet(html.parse,'//title')[[1]]
      )
      if (grepl('Strategy Tester:', html.title)) {
        return(.read.html.mt4.ea(file.link, html.parse))
      }
      if (grepl('Statement:', html.title)) {
        return(.read.html.mt4.trade(file.link, html.parse))
      }
      if (grepl('Strategy Tester Report', html.title)) {
        return(.read.html.mt5.ea(file.link))
      }
      if (grepl('Trade History Report', html.title)) {
        return(.read.html.mt5.trade(file.link))
      }
      if (grepl('Closed Trades Report', html.title)) {
        return(.read.html.mt4m.closed(file.link))
      }
      if (grepl('Raw Report', html.title)) {
        return(.read.html.mt4m.raw(file.link))
      }
      return(NULL)
    }
  ),
  private = list(
    m.html.parse = NULL
  )
)

#### ++ HTML MT4 EA : HTML REPORT : REPORT ####

MetaQuote.HTML.MT4EA.Report <- R6Class(
  classname = 'MetaQuote HTML MT4 EA Report',
  public = list(
    initialize = function(file.link) {
      
    }
  ),
  private = list(
    
  )
)


#### FILE READER ####

File.Reader <- R6Class(
  classname = 'File Reader',
  public = list(
    initialize = function(file.link) {
      # ''' init: input files '''
      # 2017-01-13: version 0.1
      add.files(file.link)
    },# FINISH
    add.files = function(file.link) {
      # ''' add files '''
      # 2017-01-13: version 0.1
      lapply(file.link, FUN = function(file) {
        file <- File$new(file)
        ifelse(
          file$is.supprted(),
          private$add.supported.file(file),
          private$add.unsupported.file(file)
        )
      })
    },# TESTING
    clear.files = function() {
      # ''' clear all files '''
      # 2017-01-13: version 0.1
      private$m.unsupported.files <- list()
      private$m.supported.files <- list()
    } # TESTING
  ),
  private = list(
    m.unsupported.files = list(),
    m.supported.files = list(),
    add.supported.file = function(file) {
      # ''' add supported files '''
      # 2017-01-13: version 0.1
      private$m.supported.files <- c(private$m.supported.files, file)
    },# TESTING
    add.unsupported.file = function(file) {
      # ''' add unsupported files '''
      # 2017-01-13: version 0.1
      private$m.unsupported.files <- c(private$m.unsupported.files, file)
    } # TESTING
  )
)

#### FILE READER Functions ####





# file.extension <- .file.extension(file.name)
# report <- if (grepl('htm|html', file.extension)) {
#   .read.html(file.link)
# } else if (grepl('xlsx|xls', file.extension)) {
#   #### ToDo ####
#   xlsx_table <- read.xlsx(file.link)
#   print(xlsx_table)
#   file.csv_xlsx(file.link, xlsx_table)
# } else if (grepl('csv', file_extension)) {
#   #### ToDo ####
#   csv_table <- read.csv(file.link, encoding = 'UTF-8')
#   file.csv_xlsx(file.link, csv_table)
# } else {
#   NULL
# }
# if (is.null(report)) return(NULL)
# within(report, Source <- file.link)
# })