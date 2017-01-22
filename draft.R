


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



recalculate.tickets.profit <- cmpfun(function(tickets) {
  group.index <- tickets$GROUP
  closed.index <- which(group.index == 'Closed')
  if (length(closed.index) == 0) {
    return(tickets)
  }
  closed.tickets <- tickets[closed.index, ]
  other.tickets <- tickets[-closed.index, ]
  do.call(rbind, lapply(split(closed.tickets, closed.tickets$ITEM), print))
})

recalculate.tickets.profit.symbol <- cmpfun(function(tickets) {

})




calculate.profits <- cmpfun(function(volumn, tickvalue, pips, format.digits = 2) {
  # ''' calculate profit from: volume, tickvalue, pips '''
  # 2016-08-15: Version 1.0
  round(volume * tickvalue * pips, format.digits)
})# FINISH

calculate.pips <- cmpfun(function(type, open.price, close.price, digit) {
  # ''' calculate pips '''
  # 2017-01-22: Version 1.0
  sell.index <- which(grepl('SELL', toupper(type)))
  diff.price <- close.price - open.price
  if (length(sell.index) > 0) {
    diff.price[sell.index] <- -diff.price[sell.index]
  }
  diff.price * 10 ^ digit
})# FINISH

# calculate.pips <- cmpfun(function(symbol, type, open.price, close.price, support.symbols.table) {
#   # ''' calculate pips '''
#   # 2016-08-15: TESTING
#   diffprice <- ifelse(type == 'BUY', close.price - open.price, open.price - close.price)
#   diffprice * 10 ^ support.symbols.table[symbol, 'Digit']
# })# 2016-08-15: TESTING

recalculate.tickets.profit(TTT)