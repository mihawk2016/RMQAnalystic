require(R6)
require(compiler)

#### REPORT INFOS ####

MetaQuote.ReportInfos <- R6Class(
  # ''' Report Member: Infos '''
  # 2017-01-21: Version 1.1 data.frame mode
  classname = 'MetaQuote Report Infos',
  public = list(
    #### + PUBLIC ####
    initialize = function() {
      # ''' report infos class '''
      # 2017-01-24: Version 1.0
      private$m.infos <- data.frame(
        stringsAsFactors = F,
        row.names = NULL,
        FilePath = NA,
        File = NA,
        Type = NA,
        Account = NA,
        # Group = NA,
        Name = NA,
        Broker = NA,
        Currency = NA,
        Leverage = NA,
        Time = NA
      )
    },# FINISH
    get.infos.column = function(column) {
      # ''' get member '''
      # 2017-01-21: Version 1.0 dataframe mode
      # 2017-01-16: Version 0.1
      if (missing(column)) {
        return(private$m.infos)
      }
      info <- subset(private$m.infos, select = column)
      if (length(column) == 1) {
        info <- unlist(info)
      }
      info
    },# FINISH
    set.infos.column = function(column, value) {
      # ''' set member '''
      # 2017-01-21: Version 1.0 dataframe mode
      # 2017-01-16: Version 0.1
      if (missing(column)) {
        return(private$m.infos <- value)
      }
      formated.value <- switch(
        column,
        Account = sapply(value, .format.report.infos.account, USE.NAMES = T),
        Name = sapply(value, .format.report.infos.name, USE.NAMES = T),
        Broker = sapply(value, .format.report.infos.broker, USE.NAMES = T),
        Currency = sapply(value, .format.report.infos.currency, USE.NAMES = T),
        Leverage = sapply(value, .format.report.infos.leverage, USE.NAMES = T),
        Time = private$format.time(value),
        value
      )
      private$m.infos[column] <- formated.value
    } # FINISH
  ),
  private = list(
    #### + PRIVATE ####
    
    #### ++ Member ####
    m.infos = NULL,
    format.time = function(time) {
      # ToDo: need not so many check
      formated.time <- sapply(time, .format.report.infos.time, USE.NAMES = T)
      if (is.numeric(formated.time)) {
        formated.time <- as.POSIXct(formated.time, origin = '1970-01-01', tz = 'GMT')
      }
      formated.time
    } #FINISH
  )
)

#### OUTTER FUNCTION ####

.format.report.infos.account <- cmpfun(function(account) {
  # ''' format report info: account '''
  # 2017-01-16: Version 0.1
  if (is.na(account) | is.numeric(account)) {
    return(account)
  }
  if (account == '') {
    return(NA)
  }
  if (is.character(account)) {
    account <- gsub('Account: ', '', account)
    match1 <- regexpr('[[:digit:]]*', account)
    if (match1 > 0) {
      account <- substr(account, match1, attr(match1, 'match.length') + match1 - 1)
    }
    return(as.numeric(account))
  }
  NA
})# FINISH

.format.report.infos.name <- cmpfun(function(name) {
  # ''' format report info: name '''
  # 2017-01-16: Version 0.1
  if (is.na(name)) {
    return(name)
  }
  name <- gsub('Name: ', '', name)
  ifelse(name == '', NA, name)
})# FINISH

.format.report.infos.broker <- cmpfun(function(broker) {
  # ''' format report info: broker '''
  # 2017-01-16: Version 0.1
  if (is.na(broker)) {
    return(broker)
  }
  gsub(' .*', '', broker)
})# FINISH

.format.report.infos.currency <- cmpfun(function(currency) {
  # ''' format report info: currency '''
  # 2017-01-16: Version 0.1
  if (is.na(currency)) {
    return(currency)
  }
  currency <- gsub('Currency: ', '', currency)
  match1 <- regexpr('[[:upper:]]+', currency)
  if (match1 > 0) {
    currency <- substr(currency, match1, attr(match1, 'match.length') + match1 - 1)
  }
  ifelse(currency == '', NA, currency)
})# FINISH

.format.report.infos.leverage <- cmpfun(function(leverage) {
  # ''' format report info: leverage '''
  # 2017-01-16: Version 0.1
  if (is.na(leverage) | is.numeric(leverage)) {
    return(leverage)
  }
  if (leverage == '') {
    return(NA)
  }
  if (is.character(leverage)) {
    match1 <- regexpr('1:[[:digit:]]+', leverage)
    if (match1 > 0) {
      leverage <- substr(leverage, match1 + 2, attr(match1, 'match.length') + match1 - 1)
    }
    return(as.numeric(leverage))
  }
  NA
})# FINISH

.format.report.infos.time <- cmpfun(function(time) {
  # ''' format report info: time '''
  # 2017-01-16: Version 0.1
  .format.time(time)
})# FINISH
