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
    html.parse <- htmlParse(file.link, encoding = 'UTF-8')
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
      return(MetaQuote.HTML.MT4M_Closed.Report$new(file.link, file.name))
    }
    if (grepl('Raw Report', html.title, file.name)) {
      return(MetaQuote.HTML.MT4M_Raw.Report$new(file.link, file.name))
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