#### NOTE ####
# 2016-11-24: Create

require(compiler)



report.tickets.add.mode <- cmpfun(function(tickets, type) {
  # ''' tickets add mode for recalculate '''
  # 2016-08-18: ToDo: for xlsx or database
  mode <- character(nrow(tickets))
  if (type == 'MT4 - EA') {
    closed.index <- which(tickets$Group == 'CLOSED')
    mode[closed.index] <- 'sp'
  } else if (grepl('MT5', type)) {
    closed_open.index <- which(tickets$Group %in% c('CLOSED', 'OPEN'))
    mode[closed_open.index] <- 'p'
  }
  within(tickets, Mode <- mode)
})# 2016-08-18: ToDo
