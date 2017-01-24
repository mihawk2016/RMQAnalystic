item2symbol <- function(item, symbols) {
  # ''' item to symbol '''
  # 2016-08-12: Version 1.0
  if (grepl('BX', item) || is.na(item)) {
    return('') 
  }
  symbol <- symbols[str_detect(item, symbols)]
  if (length(symbol) != 1) {
    symbol <- ''
  }
  # names(symbol) <- item
  symbol
}
SYMBOLS <- c('AUDCAD', 'AUDCHF', 'AUDJPY', 'AUDNZD', 'AUDUSD', 'CADCHF', 'CADJPY', 'CHFJPY', 'EURAUD', 'EURCAD',
             'EURCHF', 'EURGBP', 'EURJPY', 'EURNZD', 'EURUSD', 'GBPAUD', 'GBPCAD', 'GBPCHF', 'GBPJPY', 'GBPNZD',
             'GBPUSD', 'NZDCAD', 'NZDCHF', 'NZDJPY', 'NZDUSD', 'USDCAD', 'USDCHF', 'USDJPY', 'XAGUSD', 'XAUUSD')
test <- c('EURUSDx', '.AUDUSD', 'XAUUSDfx', 'BXUSDCAD', '', NA)
EEE <- sapply(test, item2symbol, SYMBOLS, USE.NAMES = T)
print(EEE)
# 
# format.time = function(time) {
#   # ''' format time column '''
#   # 2016-08-16: Version 1.0
#   if (all(class(time) == c("POSIXct", "POSIXt"))) {
#     return(time)
#   }
#   if (is.numeric(time)) {
#     return(as.POSIXct(time, origin = '1970-01-01', tz = 'GMT'))
#   }
#   if (is.character(time)) {
#     time <- gsub('[.]', '-', time)
#     only.date <- nchar(time) < 12
#     time[only.date] <- paste(time[only.date], '00:00')
#     return(as.POSIXct(time, tz = 'GMT'))
#   }
#   message('format time may cause error')
#   NA
# }
# 
# format.time(c('2015.01.02', '2015.01.02 01:00'))



# A <- data.frame(stringsAsFactors = F,
#                 E = c('2015-01-01', '2015-02-02 00:00'))
# # print(nchar(A))
# 
# 
# # BBB <- function(time.str) {
# #   paste(time.str, '00:00')
# # }
# # 
# # A[nchar(A) <= 11] <- BBB(A[nchar(A) <= 11])
# 
# A[nchar(A$E) < 12, ]
# A <- with(A[nchar(A$E) < 12, ], {
#   paste(E, '00:00')
# })
# 
# print(A)



# as.POSIXlt(1, origin = '1970-01-01', tz = 'GMT') -> A
# 
# data.frame(A = as.POSIXct(c('2015-01-01 01:00', '2015-02-02 00:00'), tz = 'GMT'))

# library(R6)
# 
# A <- R6Class(
#   public = list(
#     a = 1,
#     initialize = function(x) {
#       print(private$m.a)
#     },
#     TEST = function(a, b) {
#       self$a <- a - b
#       print(self$a)
#     }
#   ),
#   private = list(
#     m.a = 'aaa'
#   )
# )
# 
# x1 <- A$new()
# x2 <- x1
# x2$a <- 3
# print(x1$a)
# print(x1[['a']])
# 
# with.do = function(class, fun, ...) {
#   (class[[fun]])(...)
# }
# 
# print(with.do(x1, 'TEST', a = 6, 7))
# print(x2$a)

# A <- function(a) {
#   B(a)
# }
# 
# B <- function(a) {
#   if (missing(a)) {
#     print('missing')
#   } else {
#     print('exists')
#   }
# }
# A()

# a <- 'c'
# 
# print(switch(
#   a,
#   c(b, d) = 2,
#   c = 3,
#   e = 5
# ))

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



# lapply(split(TTT, TTT$ITEM), print)


# 
# recalculate.tickets.profit <- cmpfun(function(tickets) {
#   group.index <- tickets$GROUP
#   closed.index <- which(group.index == 'Closed')
#   if (length(closed.index) == 0) {
#     return(tickets)
#   }
#   closed.tickets <- tickets[closed.index, ]
#   other.tickets <- tickets[-closed.index, ]
#   do.call(rbind, lapply(split(closed.tickets, closed.tickets$ITEM), print))
# })
# 
# recalculate.tickets.profit.symbol <- cmpfun(function(tickets) {
# 
# })
# 
# 
# 
# 




# build.path = function(timeframe, symbol, local) {
#   if (is.null(local)) {
#     local <- list()
#   }
#   if (!(timeframe %in% names(local))) {
#     local[[timeframe]] <- list()
#   }
#   if (!(symbol %in% names(local[[timeframe]]))) {
#     local[[timeframe]][[symbol]] <- NA
#     return(NULL)
#   }
#   local[[timeframe]][[symbol]]
# }
# 
# A <- NULL
# B <- build.path('abc', 'DEF', A)


# A <- Local.Data.R$new('./abc.rdata')

# A$import.csv.files(c('USDCAD_H1_2014.csv', 'USDCAD_H1_2015.csv', 'USDCAD_H1_2016M01-10.csv'))
# A$get.ohlc('USDCAD', '2014-01-01', '2014-01-02', 'H1') -> E
# # A$get.open('USDCAD', c('2014-01-02 05:01', '2014-01-15', '2019-02-15'), 'H1') -> E
# print(E)
# 
# # A <- DataBase.MySQL$new()
# # A$get.open('USDCAD', c('2014-01-02 05:01', '2014-01-15 00:00', '2019-02-15 00:00'), 'H1') -> B
# A$get.ohlc('USDCAD', '2014-01-01', '2014-01-02', 'H1') -> B
# print(B)



# FFF <- R6Class(
#   public = list(
#     ss = list(),
#     bb = 1,
#     test = function(f='abc', e='ABC') {
#       self$ss[[f]] <- list()
#       self$ss[[f]][[e]] <- NULL
#     },
#     set.bb = function() {
#       self$bb <- 2
#     },
#     test2 = function() {
#       print(self$bb)
#       self$set.bb()
#       print(self$bb)
#     }
#   ),
#   private = list(
#     
#   )
# )
# 
# ZZ <- FFF$new()
# ZZ$test()
# ZZ$test2()


# A <- DataCenter$new()
# A$get.ohlc('EURUSD', '2014-01-01', '2014-01-02', 'H1')
# A$get.local() -> B
# B$get.local.data() -> C
# C$H1$USDCAD
