## 2017-01-13: Create

require(R6)
require(compiler)

#### FILE READER ####

File.Reader <- R6Class(
  classname = 'File Reader',
  public = list(
    initialize = function(file.link) {
      # ''' init: input files '''
      # 2017-01-13: Version 0.1
      self$add.files(file.link)
    },# FINISH
    add.files = function(file.link) {
      # ''' add files '''
      # 2017-01-13: Version 0.1
      lapply(file.link, FUN = private$input.file)
    },# TESTING
    clear.files = function() {
      # ''' clear all files '''
      # 2017-01-13: Version 0.1
      private$m.unsupported.files <- NULL
      private$m.supported.files <- NULL
    },# TESTING
    get.reports = function(index) {
      # ''' get supported reports '''
      # 2017-01-14: Version: 1.0
      if (missing(index)) {
        private$m.supported.files
      } else {
        private$m.supported.files[[index]]
      }
    },# FINISH
    get.unsupported.file = function(index) {
      # ''' get unsupported files '''
      # 2017-01-14: Version: 0.1
      if (missing(index)) {
        private$m.unsupported.files
      } else {
        private$m.unsupported.files[[index]]
      }
    } # TESTING
  ),
  private = list(
    m.unsupported.files = NULL,
    m.supported.files = NULL,
    
    input.file = function(file.link) {
      # ''' input one file '''
      # 2017-01-14: Version 0.1
      file.name <- .file.name(file.link)
      file.extension <- .file.extension(file.name)
      report <- .read.file(file.link, file.name, file.extension)
      ifelse(is.null(report), private$add.unsupported.file(file.name), private$add.supported.file(report))
    },
    add.supported.file = function(file) {
      # ''' add supported files '''
      # 2017-01-13: Version 0.1
      private$m.supported.files <- c(private$m.supported.files, file)
    },# TESTING
    add.unsupported.file = function(file) {
      # ''' add unsupported files '''
      # 2017-01-13: Version 0.1
      private$m.unsupported.files <- c(private$m.unsupported.files, file)
    } # TESTING
  )
)

#### FILE READER - Utils ####

.file.name <- cmpfun(function(file.link) {
  # ''' get file name '''
  # 2016-08-11: Version 1.0
  tail(strsplit(file.link, '/', fixed = T)[[1]], 1)
})# FINISH

.file.extension <- cmpfun(function(file.link) {
  # ''' get file extension '''
  # 2017-01-13: Version 1.1 add support for none extension file
  # 2016-08-11: Version 1.0
  file.link.split <- strsplit(file.link, '.', fixed = T)[[1]]
  ifelse(length(file.link.split) > 1, tolower(tail(file.link.split, 1)), '')
})# FINISH

.read.file <- cmpfun(function(file.link, file.name, file.extension) {
  # ''' read file '''
  # 2017-01-13:
  if (grepl('htm|html', file.extension)) {
    #### ToDo ####
    html.parse <- .html.parse(file.link)
    html.title <- xmlValue(getNodeSet(html.parse,'//title')[[1]])
    if (grepl('Strategy Tester:', html.title)) {
      return(MetaQuote.HTML.MT4EA.Report$new(file.link, file.name, html.parse))
    }
    if (grepl('Statement:', html.title)) {
      return(MetaQuote.HTML.MT4Trade.Report$new(file.link, file.name, html.parse))
    }
    if (grepl('Strategy Tester Report', html.title)) {
      return(MetaQuote.HTML.MT5EA.Report$new(file.link, file.name))
    }
    if (grepl('Trade History Report', html.title)) {
      return(MetaQuote.HTML.MT5Trade.Report$new(file.link, file.name))
    }
    if (grepl('Closed Trades Report', html.title, file.name)) {
      #### ToDo ####
      return(.read.html.mt4m.closed(file.link, file.name))
    }
    if (grepl('Raw Report', html.title, file.name)) {
      #### ToDo ####
      return(.read.html.mt4m.raw(file.link, file.name))
    }
    return(NULL)
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

.html.parse <- cmpfun(function(file.link) {
  # ''' parse html '''
  # 2017-01-13: Version 1.0
  htmlParse(file.link, encoding = 'UTF-8')
})# FINISH


#### REPORT ####

MetaQuote.Report <- R6Class(
  classname = 'MetaQuote Report',
  public = list(
    # initialize = function(file.name) {
    #   private$m.info.name <- file.name
    # }
  ),
  private = list(
    ## Info ##
    m.info.type = NA,
    m.info.file = NA,
    m.info.account = NA,
    m.info.group = NA,
    m.info.name = NA,
    m.info.broker = NA,
    m.info.currency = NA,
    m.info.leverage = NA,
    m.info.time = NA
    
  )
)

#### + HTML REPORT : REPORT ####

MetaQuote.HTML.Report <- R6Class(
  classname = 'MetaQuote HTML Report',
  inherit = MetaQuote.Report,
  public = list(
    initialize = function(file.link) {
      
    }
  ),
  private = list(
    
    # get.html.table = function(file.link) {
    #   .html.table(file.link)
    # }
  )
)

#### + HTML REPORT - Utils ####

.html.table <- cmpfun(function(file.link) {
  # ''' get html table '''
  # 2017-01-13: Version 0.1
  readHTMLTable(file.link, stringsAsFactors = FALSE, encoding = 'UTF-8')
})

#### ++ HTML MT4 EA : HTML REPORT : REPORT ####

MetaQuote.HTML.MT4EA.Report <- R6Class(
  classname = 'MetaQuote HTML MT4 EA Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.link, file.name, html.parse) {
      private$m.info.file <- file.name
      private$m.html.parse <- html.parse
      private$set.infos(html.parse)
    }
  ),
  private = list(
    m.info.type = 'MT4-EA',
    m.html.parse = NULL,
    
    set.infos = function(html.parse) {
      # ''' set infos '''
      # 2017-01-13: Version 0.1
      head.lines <- getNodeSet(html.parse, '//b', fun = xmlValue)[2:3]
      time.string <- getNodeSet(html.parse, '//tr', fun = xmlValue)[2]
      nchar.time.string <- nchar(time.string)
      private$m.info.time <- substr(time.string, nchar.time.string - 10, nchar.time.string - 1)
      private$m.info.name <- head.lines[[1]]
      private$m.info.broker <- head.lines[[2]]
    }
  )
)

#### ++ HTML MT4 Trade : HTML REPORT : REPORT ####

MetaQuote.HTML.MT4Trade.Report <- R6Class(
  classname = 'MetaQuote HTML MT4 Trade Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.link, file.name, html.parse) {
      private$m.info.file <- file.name
      private$m.html.parse <- html.parse
      private$set.infos(html.parse)
      
    }
  ),
  private = list(
    m.info.type = 'MT4-Trade',
    m.html.parse = NULL,
    
    set.infos = function(html.parse) {
      # ''' set infos '''
      # 2017-01-13: Version 0.1
      infos <- sapply(getNodeSet(html.parse, '//b')[1:8], xmlValue)
      time.index <- which(grepl('Trans', infos)) - 1
      others <- infos[2:(time.index - 1)]
      private$m.info.account <- others[grep('Account', others)]
      private$m.info.name <- others[grep('Name', others)]
      private$m.info.broker <- infos[1]
      private$m.info.currency <- others[grep('Currency', others)]
      private$m.info.leverage <- others[grep('Leverage', others)]
      private$m.info.time <- infos[time.index]
    }
  )
)

#### ++ HTML MT5 EA : HTML REPORT : REPORT ####

MetaQuote.HTML.MT5EA.Report <- R6Class(
  classname = 'MetaQuote HTML MT5 EA Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.link, file.name) {
      private$m.info.file <- file.name
      private$set.infos(private$m.html.table <- .html.table(file.link))
    }
  ),
  private = list(
    m.info.type = 'MT5-EA',
    m.html.table = NULL,
    
    set.infos = function(html.table) {
      # ''' set infos '''
      # 2017-01-14: Version 0.1
      info.table <- html.table[[1]]
      labels <- info.table[, 1]
      values <- info.table[, 2]
      time.string <- values[which(grepl('Period', labels))[1]]
      nchar.time.string <- nchar(time.string)
      private$m.info.broker <- values[which(grepl('Broker', labels))[1]]
      private$m.info.name <- values[which(grepl('Expert', labels))[1]]
      private$m.info.time <- substr(time.string, nchar.time.string - 10, nchar.time.string - 1)
      private$m.info.currency <- values[which(grepl('Currency', labels))[1]]
      private$m.info.leverage <- values[which(grepl('Leverage', labels))[1]]
    }
  )
)

#### ++ HTML MT5 Trade : HTML REPORT : REPORT ####

MetaQuote.HTML.MT5Trade.Report <- R6Class(
  classname = 'MetaQuote HTML MT5 Trade Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.link, file.name) {
      private$m.info.file <- file.name
      private$set.infos(private$m.html.table <- .html.table(file.link))
    }
  ),
  private = list(
    m.info.type = 'MT5-Trade',
    m.html.table = NULL,
    
    set.infos = function(html.table) {
      # ''' set infos '''
      # 2017-01-14: Version 0.1
      html.table <- html.table[[1]]
      head.info <- html.table$V2[1:4]
      private$m.info.account <- head.info[2]
      private$m.info.name <- head.info[1]
      private$m.info.broker <- head.info[3]
      private$m.info.currency <- head.info[2]
      private$m.info.leverage <- head.info[2]
      private$m.info.time <- head.info[4]
      # ), Time <- Time - 8 * 3600)

    }
  )
)


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