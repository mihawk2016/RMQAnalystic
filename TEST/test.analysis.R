rm(list = ls())

load(file = './TEST/Raw.Tickets.rdata')
load(file = './TEST/Supported.Tickets.rdata')
load(file = './TEST/Editing.Tickets.rdata')
load(file = './TEST/Edited.Tickets.rdata')
load(file = './TEST/Price.rdata')

package.list <- search()
if ('package:RMitekeLab' %in% package.list) {
  detach('package:RMitekeLab', unload = TRUE)
}
library(RMitekeLab)

# SUPPORTED.TICKETS <- tickets.supported(tickets.raw = RAW.TICKETS)
# EDITING.TICKETS <- tickets.editing(SUPPORTED.TICKETS)
# EDITED.TICKETS <- tickets.edited(EDITING.TICKETS)

# print(EDITING.TICKETS)
# print(EDITED.TICKETS)


# print(tickets.supported(tickets.raw = RAW.TICKETS))
# print(not.supported.items(tickets.raw = RAW.TICKETS))
# print(system.time({
  # print(A <- tickets.period(EDITED.TICKETS))
  # print(B <- price.data(EDITED.TICKETS, A))
#   
# }))


# print(A <- tickets.statistics.by.result(EDITED.TICKETS))
# print(B <- tickets.statistics.summary(A))

# print(cal.continuous(c(1,1,2,3)))
#
# print(maxdrawdown(c(1,5,8,-1,-5,5,4,3,8,2,1)))

# print(A <- tickets.statistics.by.exit(EDITED.TICKETS))

# print(A <- tickets.statistics.continuous(EDITED.TICKETS))

# print(A <- EDITED.TICKETS[, tickets.statistics.by.result(.SD) ,by = SYMBOL])

# print(A <- timeseries(EDITED.TICKETS, PRICE))

print(system.time({
  A <- timeseries(EDITED.TICKETS, PRICE)
}))


# melt(A, c('SYMBOL', 'RESULT')) -> B
# dcast(B, SYMBOL ~ variable + RESULT)
