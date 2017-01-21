#### NOTE ####
# 2016-11-28: Create

require(compiler)

#### READ INPUT FILES ####


#### BUILD REPORT ####



#### BUILD TICKETS ####


.build.report.tickets.group <- cmpfun(function(closed = NULL, open = NULL, money = NULL, pending = NULL, working = NULL) {
  # ''' build all report tickets groups'''
  # 2016-08-14: Done
  # 2016-08-16: Change to rbind.data.frame type, it's easy to sort or merge
  #@2016-08-14
  #list(
  #Closed = closed,
  #Open = open,
  #Money = money,
  #Pending = pending,
  #Working = working
  #)
  #@2016-08-16
  all.tickets <- rbind(money, closed, open, pending, working, make.row.names = F)
  .sort.dataframe(within(all.tickets, CTime <- .format.report.tickets.time(CTime)), 'OTime')
})# FINISH

#### + FORMAT REPORT-TICKETS ####

.format.report.tickets.number <- cmpfun(function(number) {
  # ''' reform report tickets column: number '''
  # 2016-08-16: FINISH
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
})# FINISH

.format.report.tickets.string <- cmpfun(function(string) {
  # ''' reform report tickets column: string '''
  # 2016-08-16: FINISH
  toupper(string)
})# FINISH

.format.report.tickets.money <- cmpfun(function(money) {
  # ''' reform report tickets column: money '''
  # 2016-08-16: FINISH
  if (is.numeric(money)) {
    return(money)
  }
  if (is.character(money)) {
    money[money == ''] <- '0'
    return(as.numeric(gsub(' ', '', money)))
  }
  NA
})# FINISH
