require(R6)
require(compiler)

#### REPORT ####

MetaQuote.Report <- R6Class(
  classname = 'MetaQuote Report',
  public = list(
    initialize = function() {
      # private$m.info.name <- file.name
      private$m.infos <- MetaQuote.ReportInfos$new()
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
    initialize = function() {
      super$initialize()
    }
  ),
  private = list(
    get.html.table = function(file.link) {
      # ''' get html table for tickets '''
      # 2017-01-17: Version 1.0
      readHTMLTable(file.link, stringsAsFactors = FALSE, encoding = 'UTF-8')
    }
  )
)



#### ++ HTML MT4 EA : HTML REPORT : REPORT ####

MetaQuote.HTML.MT4EA.Report <- R6Class(
  classname = 'MetaQuote HTML MT4 EA Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.link, file.name, html.parse) {
      super$initialize()
      private$m.html.parse <- html.parse
      private$init.infos(file.name, html.parse)
    },
    init.tickets = function() {
      # ''' init tickets ''' ###
      # 2017-01-18: Version 0.1
      html.table <- private$get.html.table
    }
    
  ),
  private = list(
    m.html.parse = NULL,
    
    init.infos = function(file.name, html.parse) {
      # ''' init infos '''
      # 2017-01-16: Version 0.2
      self$set.infos('file', file.name)
      self$set.infos('type', 'MT4-EA')
      head.lines <- getNodeSet(html.parse, '//b', fun = xmlValue)[2:3]
      time.string <- getNodeSet(html.parse, '//tr', fun = xmlValue)[2]
      nchar.time.string <- nchar(time.string)
      self$set.infos('time', substr(time.string, nchar.time.string - 10, nchar.time.string - 1))
      self$set.infos('name', head.lines[[1]])
      self$set.infos('broker', head.lines[[2]])
    }
  )
)

#### ++ HTML MT4 Trade : HTML REPORT : REPORT ####

MetaQuote.HTML.MT4Trade.Report <- R6Class(
  classname = 'MetaQuote HTML MT4 Trade Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.link, file.name, html.parse) {
      super$initialize()
      private$m.html.parse <- html.parse
      private$init.infos(file.name, html.parse)
      
    }
  ),
  private = list(
    m.html.parse = NULL,
    
    init.infos = function(file.name, html.parse) {
      # ''' init infos '''
      # 2017-01-16: Version 0.2
      self$set.infos('file', file.name)
      self$set.infos('type', 'MT4-Trade')
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
    initialize = function(file.link, file.name) {
      super$initialize()
      private$init.infos(file.name, private$m.html.table <- .html.table(file.link))
    }
  ),
  private = list(
    m.html.table = NULL,
    
    init.infos = function(file.name, html.table) {
      # ''' init infos '''
      # 2017-01-16: Version 0.2
      self$set.infos('file', file.name)
      self$set.infos('type', 'MT5-EA')
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
    initialize = function(file.link, file.name) {
      super$initialize()
      private$init.infos(file.name, private$m.html.table <- .html.table(file.link))
    }
  ),
  private = list(
    m.html.table = NULL,
    
    init.infos = function(file.name, html.table) {
      # ''' init infos '''
      # 2017-01-16: Version 0.2
      self$set.infos('file', file.name)
      self$set.infos('type', 'MT5-Trade')
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
    initialize = function(file.link, file.name) {
      super$initialize()
      # private$m.html.table <- .html.table(file.link)
      private$init.infos(file.name)
    }
  ),
  private = list(
    m.html.table = NULL,
    
    init.infos = function(file.name) {
      # ''' init infos '''
      # 2017-01-16: Version 0.2
      self$set.infos('file', file.name)
      self$set.infos('type', 'MT4M-Closed')
    }
  )
)

#### ++ HTML MT4Manager RAW : HTML REPORT : REPORT ####

MetaQuote.HTML.MT4M_Raw.Report <- R6Class(
  classname = 'MetaQuote HTML MT4Manager Raw Report',
  inherit = MetaQuote.HTML.Report,
  public = list(
    initialize = function(file.link, file.name) {
      private$m.info.file <- file.name
      # private$m.html.table <- .html.table(file.link)
      private$set.infos(file.name)
    }
  ),
  private = list(
    m.html.table = NULL,
    
    set.infos = function(file.name) {
      # ''' set infos '''
      # 2017-01-16: Version 0.2
      self$set.infos('file', file.name)
      self$set.infos('type', 'MT4M-Raw')
    }
  )
)