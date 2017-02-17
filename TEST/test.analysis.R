rm(list = ls())

load(file = './TEST/Raw.Tickets.rdata')
load(file = './TEST/Supported.Tickets.rdata')
load(file = './TEST/Editing.Tickets.rdata')
load(file = './TEST/Edited.Tickets.rdata')

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
# print(tickets.period(TEST.TICKETS))

print(A <- tickets.statistics.by.result(EDITED.TICKETS[RESULT == 'PROFIT']))
