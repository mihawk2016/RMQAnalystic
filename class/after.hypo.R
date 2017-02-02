
library(compiler)

.with.edited.trades.add.columns <- cmpfun(function(edited.trades, symbol.setting) {
  edited.trades <- within(edited.trades, {
    DiffPrice <- ifelse(TYPE == 'BUY', CPRICE - OPRICE, OPRICE - CPRICE)
    DiffTime <- difftime(CTIME, OTIME, units = 'secs')
    Pips <- DiffPrice * 10 ^ symbol.setting[SYMBOL, 'DIGITS']
    PROFIT <- 0 ## ToDo ####
    NetProfit <- sum(COMMISSION, TAXES, SWAP, PROFIT)
    ProfitPerLot <- PROFIT / VOLUME
    Result <- ifelse(NetProfit >= 0, 'PROFIT', 'LOSS')
  })
  ## ToDo ####
  
})