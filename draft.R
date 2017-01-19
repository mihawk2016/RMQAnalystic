


# A <- R6Class(
#   public = list(
#     initialize = function(x) {
#       print(private$m.a)
#     }
#   ),
#   private = list(
#     m.a = 'aaa'
#   )
# )
# 
# B <- R6Class(
#   inherit = A,
#   public = list(
#     # initialize = function(x) {
#     #   super$initialize()
#     #   print('B')
#     # }
#   ),
#   private = list(
#     m.a = 'bbb'
#   )
# )

.read.html.mt4m.raw <- function(file.link) {
  html_table <- readHTMLTable(file.link, stringsAsFactors = FALSE, encoding = 'UTF-8')[[1]]
  html_table <- html_table[2:(nrow(html_table) - 7), ]
  money.index <- which(html_table[, 4] == 'balance')
  money.table <- html_table[money.index, ]
  pending.index <- which(grepl('(buy|sell) (limit|stop)', html_table[, 4]))
  pending.table <- html_table[pending.index, ]
  closed.table <- html_table[-c(money.index, pending.index), ]
  .build.report(
    type = 'MT4M-Raw',
    tickets = .build.report.tickets.group(
      closed = .build.report.tickets.closed.from.columns(
        ticket = closed.table[, 1],
        otime = closed.table[, 3],
        type = closed.table[, 4],
        volume = closed.table[, 6],
        item = closed.table[, 5],
        oprice = closed.table[, 7],
        sl = closed.table[, 8],
        tp = closed.table[, 9],
        ctime = closed.table[, 10],
        cprice = closed.table[, 11],
        commission = closed.table[, 18],
        taxes = closed.table[, 19],
        swap = closed.table[, 20],
        profit = closed.table[, 21],
        comment = paste(closed.table[, 23], closed.table[, 2], sep = ' | Login: ')
      ),
      pending = .build.report.tickets.pending.from.columns(
        ticket = pending.table[, 1],
        otime = pending.table[, 3],
        type = pending.table[, 4],
        volume = pending.table[, 6],
        item = pending.table[, 5],
        oprice = pending.table[, 7],
        sl = pending.table[, 8],
        tp = pending.table[, 9],
        ctime = pending.table[, 10],
        cprice = pending.table[, 11],
        comment = paste(pending.table[, 23], pending.table[, 2], sep = ' | Login: ')
      ),
      money = .build.report.tickets.money.from.columns(
        ticket = money.table[, 1],
        otime = money.table[, 3],
        profit = money.table[, 21],
        comment = paste(money.table[, 23], money.table[, 2], sep = ' | Login: ')
      )
    ),
    info = .build.report.info()
  )
}

.read.html.mt4m.raw.cmp <- cmpfun(.read.html.mt4m.raw)

system.time(.read.html.mt4m.raw(file))

system.time(.read.html.mt4m.raw.cmp(file))
