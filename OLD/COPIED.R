#### NOTE ####
# 2016-11-24: Create

require(compiler)

#### read file ####



reform.report.tickets.number <- cmpfun(function(number) {
  # ''' reform report tickets column: number '''
  # 2016-08-16: Done
  if (is.numeric(number)) {
    return(number)
  }
  if (is.character(number)) {
    match1 <- regexpr('[[:digit:]]+.[[:digit:]]+', number)
    match1.index <- which(match1 > 0)
    number[match1.index] <- substr(number[match1.index], match1[match1.index], attr(match1, 'match.length')[match1.index] + match1[match1.index] - 1)
    number[number == ''] <- '0'
    return(as.numeric(number))
  }
  NA
})# 2016-08-16: Done

reform.report.tickets.money <- cmpfun(function(money) {
  # ''' reform report tickets column: money '''
  # 2016-08-16: Done
  if (is.numeric(money)) {
    return(money)
  }
  if (is.character(money)) {
    money[money == ''] <- '0'
    return(as.numeric(gsub(' ', '', money)))
  }
  NA
})# 2016-08-16: Done

reform.report.tickets.time <- cmpfun(function(time) {
  # ''' reform report tickets column: time '''
  # 2016-08-16: TESTING
  reform.time(time)
})# 2016-08-16: TESTING

reform.report.tickets.string <- cmpfun(function(string) {
  # ''' reform report tickets column: string '''
  # 2016-08-16: Done
  toupper(string)
})# 2016-08-16: Done



sort.dataframe <- function(dataframe, columns, decreasing = F) {
  # ''' sort dataframe with columns '''
  # 2016-08-15: Done
  dataframe[order(dataframe[, columns], decreasing = decreasing), ]
} # 2016-08-15: Done



