require(R6)
require(compiler)

#### REPORT INFOS ####

MetaQuote.ReportInfos <- R6Class(
  classname = 'MetaQuote Report Infos',
  public = list(
    #### PUBLIC ####
    to.dataframe = function() {
      # ''' infos in data.frame mode '''
      # 2017-01-16: Version 0.1
      cbind.data.frame(
        stringsAsFactors = FALSE,
        row.names = NULL,
        File = private$m.file.name,
        Type = private$m.type,
        Account = private$m.account,
        Group = private$m.group,
        Name = private$m.name,
        Broker = private$m.broker,
        Currency = private$m.currency,
        Leverage = private$m.leverage,
        Time = private$m.time
      )
    },
    get = function(member) {
      # ''' get member '''
      # 2017-01-16: Version 0.1
      switch(
        member,
        file.path = private$m.file.path,
        file.name = private$m.file.name,
        type = private$m.type,
        account = private$m.account,
        group = private$m.group,
        name = private$m.name,
        broker = private$m.broker,
        currency = private$m.currency,
        leverage = private$m.leverage,
        time = private$m.time,
        NA
      )
    },
    set = function(member, value) {
      # ''' set member '''
      # 2017-01-16: Version 0.1
      switch(
        member,
        file.path = private$set.file.path(value),
        file.name = private$set.file.name(value),
        type = private$set.type(value),
        account = private$set.account(value),
        group = private$set.group(value),
        name = private$set.name(value),
        broker = private$set.broker(value),
        currency = private$set.currency(value),
        leverage = private$set.leverage(value),
        time = private$set.time(value),
        value
      )
    }
  ),
  private = list(
    #### PRIVATE ####
    #### + Member ####
    m.file.path = NULL,
    m.file.name = NA,
    m.type = NA,
    m.account = NA,
    m.group = NA,
    m.name = NA,
    m.broker = NA,
    m.currency = NA,
    m.leverage = NA,
    m.time = NA,
    #### + FUNCTION ####
    #### ++ Getter ####
    get.file.path = function() {
      private$m.file.path
    },#FINISH
    get.file.name = function() {
      private$m.file.name
    },#FINISH
    get.type = function() {
      private$m.type
    },#FINISH
    get.account = function() {
      private$m.account
    },#FINISH
    get.group = function() {
      private$m.group
    },#FINISH
    get.name = function() {
      private$m.name
    },#FINISH
    get.broker = function() {
      private$m.broker
    },#FINISH
    get.currency = function() {
      private$m.currency
    },#FINISH
    get.leverage = function() {
      private$m.leverage
    },#FINISH
    get.time = function() {
      private$m.time
    },#FINISH
    #### ++ Setter ####
    set.file.path = function(file.path) {
      private$m.file.path <- file.path
    },#FINISH
    set.file.name = function(file.name) {
      private$m.file.name <- file.name
    },#FINISH
    set.type = function(type) {
      private$m.type <- type
    },#FINISH
    set.account = function(account) {
      private$m.account <- sapply(account, .format.report.infos.account, USE.NAMES = T)
    },#FINISH
    set.group = function(group) {
      private$m.group <- group
    },#FINISH
    set.name = function(name) {
      private$m.name <- sapply(name, .format.report.infos.name, USE.NAMES = T)
    },#FINISH
    set.broker = function(broker) {
      private$m.broker <- sapply(broker, .format.report.infos.broker, USE.NAMES = T)
    },#FINISH
    set.currency = function(currency) {
      private$m.currency <- sapply(currency, .format.report.infos.currency, USE.NAMES = T)
    },#FINISH
    set.leverage = function(leverage) {
      private$m.leverage <- sapply(leverage, .format.report.infos.leverage, USE.NAMES = T)
    },#FINISH
    set.time = function(time) {
      formated.time <- sapply(time, .format.report.infos.time, USE.NAMES = T)
      if (is.numeric(formated.time)) {
        formated.time <- as.POSIXct(formated.time, origin = '1970-01-01', tz = 'GMT')
      }
      private$m.time <- formated.time
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
})

.format.report.infos.name <- cmpfun(function(name) {
  # ''' format report info: name '''
  # 2017-01-16: Version 0.1
  if (is.na(name)) {
    return(name)
  }
  name <- gsub('Name: ', '', name)
  ifelse(name == '', NA, name)
})

.format.report.infos.broker <- cmpfun(function(broker) {
  # ''' format report info: broker '''
  # 2017-01-16: Version 0.1
  if (is.na(broker)) {
    return(broker)
  }
  gsub(' .*', '', broker)
})

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
})

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
})

.format.report.infos.time <- cmpfun(function(time) {
  # ''' format report info: time '''
  # 2017-01-16: Version 0.1
  .format.time(time)
})
