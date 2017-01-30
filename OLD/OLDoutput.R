#### NOTE ####
# 2016-11-24: Create

require(compiler)

report.output <- cmpfun(function(report, timeframe = 'H1', dir = './Output', comment = F) {
  # ''' report output '''
  # 2016-08-18: Coding
  
  # windowsFonts(CON = windowsFont("Consolas"))
  
  statistics.portfolio <- report$Statistics$Portfolio
  trade.period <- report$Period$Period
  trade.days <- report$Period$TradeDays
  timeseries <- report$TimeSeries
  parameter <- report$Parameter
  statistics.symbols <- report$Statistics$Symbol
  report.capital <- report.capital(parameter)
  trades.after.hypothesis <- report$Trades$After.Hypothesis
  money.trades <- report$Trades$Money
  
  output.info <- report.output.info(report$Info)
  output.portfolio.result <- report.output.portfolio.result(statistics.portfolio)
  output.portfolio.return <- report.output.portfolio.returns(statistics.portfolio, trade.days)
  output.portfolio.trades <- report.output.portfolio.trades(statistics.portfolio, trade.days)
  output.portfolio.summary <- report.output.portfolio.summary(statistics.portfolio, timeseries, report.capital)
  output.portfolio.symbols.profit_loss <- report.output.symbols.profit_loss(statistics.symbols)
  output.portfolio.symbols.summary <- report.output.symbols.summary(statistics.symbols, timeseries, report.capital)
  trades.output <- trades.output(trades.after.hypothesis, timeseries)
  equity.with.deposit_withdraw <- report.output.balance.serie(timeseries, money.trades, report$Period)
  return.calendar <- report.calendar.return(timeseries, report.capital, digit = 2, percent = T)
  mdd.calendar <- report.calendar.mdd(timeseries, report.capital, digit = 2, percent = F)
  scores <- cmpfun(function() {
    report.output.score(statistics.portfolio, trade.period, timeseries, report.capital,
                        parameter$Score.Return.Table, parameter$Score.MaxDD.Table, parameter$Score.Weight, parameter$Score.Level.Table)
  })
  equity_volume <- cmpfun(function() {
    report.output.equity_volume(timeseries, report.capital)
  })
  margin <- cmpfun(function() {
    report.output.margin(timeseries, report.capital)
  })
  evalution <- report.evaluation(portfolio = statistics.portfolio, trade.days, timeseries = timeseries, capital = report.capital)
  
  report.output.file(report$Info, timeframe, dir, comment, environment())
})# 2016-08-18: Coding




report.output.portfolio.result <- cmpfun(function(portfolio) {
  # ''' portfolio result for output '''
  # 2016-08-24: Done
  result <- portfolio$Result
  result <- format.data.frame(result, digits = 0, nsmall = 2, scientific = F)
  colnames(result) <- c('Trades', 'Sum', 'Mean', 'Max', 'Sum', 'Mean', 'Max', 'Sum', 'Mean', 'Max',
                                              'Min', 'MaxTrades', 'MeanTrades', 'MaxSum')
  htmlTable(result,
            caption = 'Profit & Loss',
            css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
            cgroup = c('', 'Profit', 'Pips', 'Volume', 'Continuous'),
            n.cgroup = c(1, 3, 3, 4, 3),
            col.rgroup = c("none", "#F9FAF0"),
            col.columns = c("none", "#F1F0FA")
            )
  
})# 2016-08-24: Done

report.output.portfolio.returns <- cmpfun(function(portfolio, trade.days) {
  # ''' portfolio profits & returns for output '''
  # 2016-08-24: Done
  result <- portfolio$Result
  return <- portfolio$Return_Sharpe$ReturnR
  total.profit <- sum(result[, 2])
  daily <- c(total.profit, return * 100) / trade.days
  result_return <- data.frame(
    stringsAsFactors = F,
    row.names = c('PROFIT($)', 'RETURN(%)'),
    Daily = daily,
    Weekly = daily * 7,
    Monthly = daily * 30.42,
    Yearly = as.character(round(daily * 365, 2))
  )
  result_return <- format.data.frame(result_return, digits = 0, nsmall = 2, scientific = F)
  htmlTable(result_return,
            caption = 'Profit & Return',
            css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
            col.rgroup = c("none", "#F9FAF0"),
            col.columns = c("none", "#F1F0FA")
  )
})# 2016-08-24: Done

report.output.portfolio.trades <- cmpfun(function(portfolio, trade.days) {
  # ''' portfolio trades for output '''
  # 2016-08-25: Done
  result <- portfolio$Result
  total.trades <- sum(result[, 1])
  total.profit <- sum(result[, 2])
  total <- c(total.trades, total.profit)
  exit <- portfolio$Exit
  exit.trades <- exit[, 1]
  exit.percent <- sprintf('%s (%.2f%%)', exit.trades, exit.trades / total.trades * 100)
  exit[, 1] <- exit.percent
  exit.table <- t(exit)
  trades.table <- cbind(Total = round(total, 2), Daily = round(total / trade.days, 2), exit.table)
  rownames(trades.table) <- c('TRADES', 'PROFIT')
  htmlTable(trades.table,
            caption = 'Trades & Profit',
            css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
            col.rgroup = c("none", "#F9FAF0"),
            col.columns = c("none", "#F1F0FA")
  )
})# 2016-08-25: Done

report.output.portfolio.summary <- cmpfun(function(portfolio, timeseries, capital) {
  # ''' portfolio summary for output '''
  # 2016-08-25: Done
  ts.portfolio <- timeseries$Portfolio
  equity.portfolio <- ts.portfolio$Balance.Delta + ts.portfolio$Floating + capital
  times <- index(equity.portfolio)
  mdd.portfolio <- maxdrawdown(coredata(equity.portfolio))
  mdd.portfolio.period <- format.difftime(as.numeric(difftime(times[mdd.portfolio$MinIndex], times[mdd.portfolio$MaxIndex], units = 'secs')))
  result <- cbind(portfolio$Summary, mdd.portfolio[ , 1:2], Period = mdd.portfolio.period)
  result <- format.data.frame(result, digits = 0, nsmall = 2, scientific = F)
  colnames(result)[10:11] <- c('Money', 'Percent')
  htmlTable(result,
            caption = 'Summary',
            css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
            rnames = F,
            cgroup = c('Summary', 'MaxDrawDown'),
            n.cgroup = c(9, 3),
            col.rgroup = c("none", "#F9FAF0"),
            col.columns = c("none", "#F1F0FA")
  )
})# 2016-08-25: Done

report.output.symbols.profit_loss <- cmpfun(function(symbols) {
  # ''' symbol for output '''
  # 2016-08-25: Coding
  symbol.table <- as.data.frame(do.call(rbind, lapply(symbols, symbol.statistics.profit_loss.in.one.row)))
  colnames(symbol.table) <- c('Profit', 'Loss', 'Total', 'Win(%)', 'Profit', 'Loss', 'Total', rep(c('Profit', 'Loss'), 4))
  symbol.table[, 12] <- as.character(round(symbol.table[, 12], 0))
  symbol.table[, 13] <- as.character(round(symbol.table[, 13], 0))
  symbol.table <- format(symbol.table, digits = 0, nsmall = 2, scientific = F)
  htmlTable(symbol.table,
            caption = 'Profit & Loss',
            css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
            cgroup = c('Trades', 'Gross', 'Max', 'Mean', 'ProfitPips','Volume'),
            n.cgroup = c(4, 3, 2, 2, 2, 2),
            col.rgroup = c("none", "#F9FAF0"),
            col.columns = c("none", "#F1F0FA")
  )
})# 2016-08-25: Coding

symbol.statistics.profit_loss.in.one.row <- cmpfun(function(symbol) {
  # ''' all symbol profit & loss statistics in one row '''
  # 2016-08-25: TESTING
  result <- symbol$Result
  summary <- symbol$Summary
  c(result$Profit.TradesTotal, summary$TradesTotal, summary$WinPercent,
    result$Profit.Total, summary$ProfitTotal,
    result$Profit.Max, result$Profit.Mean,
    result$ProfitP.Total,
    result$Volume.Total)
})# 2016-08-25: TESTING

report.output.symbols.summary <- cmpfun(function(symbols, timeseries, report.capital) {
  # ''' symbol for output '''
  # 2016-08-25: Coding
  symbol.table <- as.data.frame(do.call(rbind, lapply(symbols, symbol.statistics.summary.in.one.row, report.capital)))
  colnames(symbol.table) <- c('ProfitFactor', 'Expect', 'VolumePerTrade', 'ProfitPerLot', rep(c('Buy', 'Sell'), 2))#, 'Money', 'Percent', 'Period')
  symbol.table[, 5] <- as.character(round(symbol.table[, 5], 0))
  symbol.table[, 6] <- as.character(round(symbol.table[, 6], 0))
  timeseries <- timeseries$Symbol
  mdd.table <- as.data.frame(do.call(rbind, lapply(timeseries, symbol.statistics.mdd.in.one.row, report.capital)))
  symbol.table <- cbind(symbol.table, mdd.table)
  symbol.table <- format(symbol.table, digits = 0, nsmall = 2, scientific = F)
  htmlTable(symbol.table,
            caption = 'Summary',
            css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
            cgroup = c('Summary', 'Trades', 'Volume', 'MaxDrawDown'),
            n.cgroup = c(4, 2, 2, 3),
            col.rgroup = c("none", "#F9FAF0"),
            col.columns = c("none", "#F1F0FA")
  )
})# 2016-08-25: Coding

symbol.statistics.summary.in.one.row <- cmpfun(function(symbol, report.capital) {
  # ''' all symbol summary statistics in one row '''
  # 2016-08-29: TESTING
  summary <- symbol$Summary
  type <- symbol$Type
  c(summary$ProfitFactor, summary$Expect, summary$VolumePerTrade, summary$ProfitPerLot, type$TradesTotal, type$VolumeTotal)
})# 2016-08-29: TESTING

symbol.statistics.mdd.in.one.row <- cmpfun(function(timeseries, report.capital) {
  # ''' all symbol mdds in one row '''
  # 2016-08-29: TESTING
  timeserie <- timeseries$Symbol
  equitys <- timeserie$Balance.Delta + timeserie$Floating  + report.capital
  times <- index(equitys)
  mdd.symbol <- maxdrawdown(coredata(equitys))# , mdd.symbol[ , 1:2], Period = mdd.symbol.period
  mdd.symbol.period <- format.difftime(as.numeric(difftime(times[mdd.symbol$MinIndex], times[mdd.symbol$MaxIndex], units = 'secs')))
  c(mdd.symbol[ , 1:2], Period = mdd.symbol.period)
})# 2016-08-29: TESTING

trades.output <- cmpfun(function(trades.after.hypothesis, timeseries) {
  # '''
  # 2016-08-29: Coding
  symbol.timeseries <- timeseries$Symbol
  trades.floatings <- do.call(rbind, lapply(symbol.timeseries, cmpfun(function(symbol.timeserie) {
    symbol.trades <- symbol.timeserie$Trades
    do.call(rbind, lapply(symbol.trades, cmpfun(function(symbol.trade) {
      index <- index(symbol.trade)
      max.floating <- max(symbol.trade$Max.Floating)
      if (max.floating < 0) {
        max.floating.time <- 0
      } else {
        max.floating.time <- index[which.max(symbol.trade$Max.Floating)]
      }
      min.floating <- min(symbol.trade$Min.Floating)
      if (min.floating >= 0) {
        min.floating.time <- 0
      } else {
        min.floating.time <- index[which.min(symbol.trade$Min.Floating)]
      }
      c(MFP = max.floating, MFPP = max.floating.time, MFL = min.floating, MFLP = min.floating.time)
    })))
  })))
  trades.floatings <- as.data.frame(trades.floatings)
  trades.floatings$Ticket <- as.numeric(gsub('T', '', rownames(trades.floatings)))
  # trades.floatings
  merge.data <- merge.data.frame(trades.after.hypothesis, trades.floatings, by = 'Ticket')
  # mfpp.not.zero <- which(merge.data$MFPP != 0)
  new.mfpp <- merge.data$MFPP - as.numeric(merge.data$OTime)
  new.mfpp[new.mfpp < 0] <- 0
  new.mflp <- merge.data$MFLP - as.numeric(merge.data$OTime)
  new.mflp[new.mflp < 0] <- 0
  merge.data$MFPP <- format.difftime(new.mfpp)
  merge.data$MFLP <- format.difftime(new.mflp)
  merge.data
  trades <- subset(merge.data, select = c(Ticket, Item, Type, Volume, OTime, OPrice, CTime, CPrice, SL, TP, NetProfit, ProfitP, MFP, MFPP, MFL, MFLP, DiffTime, Comment))
  trades$Volume <- as.character(round(trades$Volume, 2))
  trades$DiffTime <- format.difftime(as.numeric(trades$DiffTime))
  trades$OTime <- as.character(reform.time(trades$OTime))
  trades$CTime <- as.character(reform.time(trades$CTime))
  trades$ProfitP <- as.character(round(trades$ProfitP, 0))
  trades$MFP <- as.character(round(trades$MFP, 2))
  trades$MFL <- as.character(round(trades$MFL, 2))
  colnames(trades) <- c('Ticket', 'Item', 'Type', 'Volume', 'Time', 'Price', 'Time', 'Price',' SL', 'TP', 'NetProfit', 'ProfitPips', 'Profit', 'P.Period', 'Loss', 'L.Period', 'Period', 'Comment')
  # trades <- format.data.frame(trades, digits = 0, nsmall = 2, scientific = F)
  
  htmlTable(
    trades,
    caption = 'Tickets',
    css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
    rnames = FALSE,
    cgroup = c('', 'Request', 'Open', 'Close', 'Setting', 'Result', 'Max Floating', ''),
    n.cgroup = c(1, 3, 2, 2, 2, 2, 4, 2),
    col.rgroup = c("none", "#F9FAF0"),
    col.columns = c("none", "#F1F0FA")
  )
})

report.output.balance.serie <- cmpfun(function(timeseries, money.trades, period) {
  # ''' plot blance serie '''
  # 2016-08-29: Coding
  portfolio.timeserie <- timeseries$Portfolio
  equity <- with(portfolio.timeserie, Balance.Delta + Floating)
  n.money <- nrow(money.trades)
  for (i in 1:n.money) {
    equity[paste0(as.character(money.trades[i, 'OTime']), '/')] <- equity[paste0(as.character(money.trades[i, 'OTime']), '/')] + money.trades[i, 'Profit']
  }
  equity <- data.frame(
    row.names = NULL,
    Time = index(equity),
    Equity = round(equity$Balance.Delta, 2)
  )
  colnames(equity) <- c('Time', 'Equity')
  rows <- nrow(equity)
  middle <- rows %/% 2
  highest <- equity[which.max(equity$Equity), ]
  begin <- equity[1, ]
  end <- equity[rows, ]
  period.m <- equity[middle, ]
  period.text <- paste('Trade Days: ', period$TradeDays, '\n',
                       ifelse(period$Weeks > 0, paste('Weeks: ', period$Weeks, '\n'), ''),
                       ifelse(period$Months > 0, paste('Months: ', period$Months), ''))
  end$Equity <- period.m$Equity <- begin$Equity
  ggplot(equity) +
    geom_line(aes(x = Time, y = Equity)) +
    ggtitle('equity with deposit / withdraw') +
    xlab(NULL) +
    ylab(NULL) +
    theme(legend.position = 'none', panel.background = element_blank(), axis.text.y = element_text()) +
    scale_y_continuous(labels = scales::dollar) +
    geom_point(data = highest, aes(x = Time, y = Equity), colour = 'blue') +
    geom_text(hjust = 'inward', vjust = 'inward', data = highest, aes(x = Time, y = Equity * 1.05, label = paste(Time, '\n', '$', Equity)), colour = 'blue') +
    geom_text(hjust = 'inward', vjust = 'inward', data = begin, aes(x = Time, y = Equity, label = paste('Begin', '\n', as.Date(Time)))) + 
    geom_text(hjust = 'inward', vjust = 'inward', data = end, aes(x = Time, y = Equity, label = paste('End', '\n', as.Date(Time)))) + 
    geom_text(hjust = 'inward', vjust = 'inward', data = period.m, aes(x = Time, y = Equity, label = period.text))
})# 2016-08-29: Coding

report.output.score <- cmpfun(function(portfolio, trade.period, timeseries, capital, return.table, mdd.table, weight, level.table) {
  return <- portfolio$Return_Sharpe$ReturnR
  yearly.return <- return * 86400 / trade.period * 365
  ts.portfolio <- timeseries$Portfolio
  equity.portfolio <- ts.portfolio$Balance.Delta + ts.portfolio$Floating + capital
  mdd.portfolio <- maxdrawdown(coredata(equity.portfolio))
  portfolio.scores <- cal.scores(yearly.return, mdd.portfolio$MDDP, return.table, mdd.table, weight, level.table)
  return.table$Value <- return.table$Value / 100
  yearly.return <- portfolio.scores$Value['Y.Return']
  v0 <- with(return.table, {
    min(2 * Value[1] - Value[2], yearly.return)
  })
  vn <- with(return.table, {
    max.index <- length(Value) - 1
    max(2 * Value[max.index] - Value[max.index - 1], yearly.return)
  })
  yearly.return.serie <- c(v0, return.table$Value[1:(nrow(return.table) - 1)], vn)
  plot.return <- ggplot() +
    geom_segment(aes(x = yearly.return.serie[1], y = return.table$Score[1], xend = yearly.return.serie[2], yend = return.table$Score[1])) +
    geom_segment(aes(x = yearly.return.serie[2], y = return.table$Score[2], xend = yearly.return.serie[3], yend = return.table$Score[2])) +
    geom_segment(aes(x = yearly.return.serie[3], y = return.table$Score[3], xend = yearly.return.serie[4], yend = return.table$Score[3])) +
    geom_segment(aes(x = yearly.return.serie[4], y = return.table$Score[4], xend = yearly.return.serie[5], yend = return.table$Score[4])) +
    geom_segment(aes(x = yearly.return.serie[5], y = return.table$Score[5], xend = yearly.return.serie[6], yend = return.table$Score[5])) +
    geom_segment(aes(x = yearly.return, y = -10, xend = yearly.return, yend = portfolio.scores$Score['Y.Return'])) +
    geom_point(aes(x = yearly.return, y = portfolio.scores$Score['Y.Return'])) +
    geom_text(hjust = 'inward', vjust = 'inward', aes(x = yearly.return, y = portfolio.scores$Score['Y.Return'], label = paste('Yearly Return', as.character(round(yearly.return, 2)), '%', '\n', 'Score', portfolio.scores$Score['Y.Return']))) +
    xlab('Yearly Return') +
    ylab('Score') +
    ggtitle('Yearly Return - Score') + 
    scale_x_continuous(labels = scales::percent) +
    ylim(-20, 120)
  ## mdd
  mdd.table$Value <- mdd.table$Value / 100
  mdd <- portfolio.scores$Value['Max.DD'] / 100
  v0 <- with(mdd.table, {
    min(2 * Value[1] - Value[2], mdd)
  })
  vn <- with(mdd.table, {
    max.index <- length(Value) - 1
    max(2 * Value[max.index] - Value[max.index - 1], mdd)
  })
  mdd.serie <- c(v0, mdd.table$Value[1:(nrow(mdd.table) - 1)], vn)
  plot.mdd <- ggplot() +
    geom_segment(aes(x = mdd.serie[1], y = mdd.table$Score[1], xend = mdd.serie[2], yend = mdd.table$Score[1])) +
    geom_segment(aes(x = mdd.serie[2], y = mdd.table$Score[2], xend = mdd.serie[3], yend = mdd.table$Score[2])) +
    geom_segment(aes(x = mdd.serie[3], y = mdd.table$Score[3], xend = mdd.serie[4], yend = mdd.table$Score[3])) +
    geom_segment(aes(x = mdd.serie[4], y = mdd.table$Score[4], xend = mdd.serie[5], yend = mdd.table$Score[4])) +
    geom_segment(aes(x = mdd.serie[5], y = mdd.table$Score[5], xend = mdd.serie[6], yend = mdd.table$Score[5])) +
    geom_segment(aes(x = mdd, y = -10, xend = mdd, yend = portfolio.scores$Score['Max.DD'])) +
    geom_point(aes(x = mdd, y = portfolio.scores$Score['Max.DD'])) +
    geom_text(hjust = 'inward', vjust = 'inward', aes(x = mdd, y = portfolio.scores$Score['Max.DD'], label = paste('Max.DD', as.character(round(mdd * 100, 2)), '%', '\n', 'Score', portfolio.scores$Score['Max.DD']))) +
    xlab('Max.DD') +
    ylab('Score') +
    ggtitle('Max.DD - Score') + 
    scale_x_continuous(labels = scales::percent) +
    ylim(-20, 120)
  ## total
  total.score <- portfolio.scores$TotalScore
  level <- match(portfolio.scores$Level, level.table$Level)
  total.serie <- c(0, level.table$Value[1:(nrow(level.table) - 1)], 100)
  plot.total <- ggplot() +
    geom_segment(aes(x = total.serie[1], y = 1, xend = total.serie[2], yend = 1)) +
    geom_segment(aes(x = total.serie[2], y = 2, xend = total.serie[3], yend = 2)) +
    geom_segment(aes(x = total.serie[3], y = 3, xend = total.serie[4], yend = 3)) +
    geom_segment(aes(x = total.serie[4], y = 4, xend = total.serie[5], yend = 4)) +
    geom_segment(aes(x = total.serie[5], y = 5, xend = total.serie[6], yend = 5)) +
    geom_segment(aes(x = total.score, y = 0, xend = total.score, yend = level)) +
    geom_point(aes(x = total.score, y = level)) +
    geom_text(hjust = 'inward', vjust = 'inward', aes(x = total.score, y = level, label = paste('Total Score', as.character(round(total.score, 2)), '\n', 'Level', portfolio.scores$Level))) +
    xlab('Total Score') +
    ylab('Level') +
    ggtitle('Total Score - Level') +
    scale_y_continuous(breaks = 1:5, labels = level.table$Level)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 2)))
  print(plot.return, vp = vplayout(1, 1))
  print(plot.mdd, vp = vplayout(1, 2))
  print(plot.total, vp = vplayout(2, 1:2))
})

report.output.equity_volume <- cmpfun(function(timeseries, capital) {
  portfolio.timeserie <- timeseries$Portfolio
  symbol.timeserie <- timeseries$Symbol
  symbol.names <- names(symbol.timeserie)
  equity.table <- do.call(cbind, lapply(symbol.timeserie, cmpfun(function(symbol) {
    symbol.table <- symbol$Symbol
    with(symbol.table, Balance.Delta + Floating)
  })))
  equity.table <- na.omit(cbind(equity.table, with(portfolio.timeserie, Balance.Delta + Floating))) + capital
  colnames(equity.table) <- c(symbol.names, 'Portfolio')
  equity.data.frame <- data.frame(
    Time = index(equity.table),
    coredata(equity.table)
  )
  melt.equity.data.frame <- melt(equity.data.frame, var.ids=c('Time'), measure.vars = -1, variable.name = 'Item', value.name = 'Equity')
  
  volume.table <- do.call(cbind, lapply(symbol.timeserie, cmpfun(function(symbol) {
    symbol.table <- symbol$Symbol
    with(symbol.table, NetVolume)
  })))
  volume.table <- na.omit(cbind(volume.table, with(portfolio.timeserie, NetVolume)))
  colnames(volume.table) <- c(symbol.names, 'Portfolio')
  volume.data.frame <- data.frame(
    Time = index(volume.table),
    coredata(volume.table)
  )
  melt.volume.data.frame <- melt(volume.data.frame, var.ids=c('Time'), measure.vars = -1, variable.name = 'Item', value.name = 'Volume')
  equity <- ggplot(melt.equity.data.frame) +
    geom_line(aes(x = Time, y = Equity, color = Item)) +
    geom_text(data = last, aes(label = Item, x = Time, y = Equity), size = 3, check_overlap = T) +
    scale_y_continuous(labels = scales::dollar) +
    ggtitle('Floating Profit & Volume') +
    xlab(NULL) +
    ylab(NULL) +
    theme(legend.position = 'none', panel.background = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  volume <- ggplot(melt.volume.data.frame) +
    geom_bar(stat='identity', aes(x = Time, y = Volume, fill = Item), position = position_dodge()) +
    ggtitle(NULL) +
    xlab(NULL) +
    ylab(NULL) +
    theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.key.size = unit(8, 'pt'), legend.title = element_blank(), panel.background = element_blank())
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 1)))
  print(equity, vp = vplayout(1, 1))
  print(volume, vp = vplayout(2, 1))
})

report.output.margin <- cmpfun(function(timeseries, capital) {
  portfolio.timeserie <- timeseries$Portfolio
  volume <- abs(portfolio.timeserie$NetVolume)
  equity <- with(portfolio.timeserie, Balance.Delta + Floating) + capital
  margin.used.account <- volume * 1500
  margin.free.account <- equity - margin.used.account
  margin.used <- data.frame(
    Time = time(margin.used.account),
    coredata(margin.used.account)
  )
  margin.used <-  melt(margin.used, var.ids=c('Time'), measure.vars = -1, variable.name = 'Item', value.name = 'MarginUsed')
  margin.free <- data.frame(
    Time = time(margin.free.account),
    coredata(margin.free.account)
  )
  margin.free <-  melt(margin.free, var.ids=c('Time'), measure.vars = -1, variable.name = 'Item', value.name = 'MarginFree')
  
  free <- ggplot(margin.free) +
    scale_y_continuous(labels = scales::dollar) +
    geom_line(aes(x = Time, y = MarginFree, color = Item)) +
    geom_text(data = last, aes(label = 'Margin Free', x = Time, y = MarginFree), size = 3, check_overlap = T) +
    ggtitle('Margin Free & Used') +
    xlab(NULL) +
    ylab(NULL) +
    theme(legend.position = 'none', panel.background = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  used <- ggplot(margin.used) +
    scale_y_continuous(labels = scales::dollar) +
    geom_bar(stat='identity', aes(x = Time, y = MarginUsed, fill = Item), position = position_dodge()) +
    ggtitle(NULL) +
    xlab(NULL) +
    ylab(NULL) +
    theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.key.size = unit(8, 'pt'), legend.title = element_blank(), panel.background = element_blank())
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 1)))
  print(free, vp = vplayout(1, 1))
  print(used, vp = vplayout(2, 1))
})

report.capital <- cmpfun(function(parameter) {
  # ''' get capital '''
  # 2016-08-25: TESTING
  capital <- parameter$Capital.Initialize.Setting
  if (capital < 0) {
    capital <- parameter$Capital.Initialize
    if (capital == 0) {
      capital <- 10000
    }
  }
  capital
})# 2016-08-25: TESTING

report.calendar.return <- cmpfun(function(timeseries, capital, digit = 2, percent = F) {
  timeserie.portfolio <- timeseries$Portfolio
  equity.portfolio <- timeserie.portfolio$Balance.Delta + timeserie.portfolio$Floating + capital
  returns.portfolio <- Return.calculate(equity.portfolio)
  return.table <- calendar.table(returns.portfolio, sum, digit = digit, percent = percent, name = 'Return')
  htmlTable(return.table,
            caption = 'Return (in percent)',
            css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
            col.rgroup = c("none", "#F9FAF0"),
            col.columns = c("none", "#F1F0FA")
  )
})

report.calendar.mdd <- cmpfun(function(timeseries, capital, digit = 2, percent = F) {
  timeserie.portfolio <- timeseries$Portfolio
  equity.portfolio <- timeserie.portfolio$Balance.Delta + timeserie.portfolio$Floating + capital
  mdd.table <- calendar.table(equity.portfolio, fun = cmpfun(function(x) {
    maxdrawdown(coredata(x))[[2]]
  }), digit = digit, percent = percent, name = 'Max DD')
  htmlTable(mdd.table,
            caption = 'Max DrawDown (in percent)',
            css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
            col.rgroup = c("none", "#F9FAF0"),
            col.columns = c("none", "#F1F0FA")
  )
})

report.evaluation <- cmpfun(function(portfolio, trade.days, timeseries, capital) {
  timeserie.portfolio <- timeseries$Portfolio
  equity.portfolio <- timeserie.portfolio$Balance.Delta + timeserie.portfolio$Floating + capital
  returns.portfolio <- Return.calculate(equity.portfolio)
  return.table <- calendar.table(returns.portfolio, sum, digit = 2, percent = T, name = 'Return')
  
  condition.string <- c('Net Profit', 'Monthly Return', 'Trades per Day', 'Max DrawDown', 'Win Percent')
  threshold <- c(0, 2, 1, 30, 50)
  result <- c('Refuse', 'Accept')
  net.profit <- sum(portfolio$Result$Profit.Total)
  returns <- as.vector(t(return.table[, -13]))
  monthly.returns <- returns[!is.na(returns)]
  con.monthly.returns <- continuous.profit_loss(monthly.returns)
  con.monthly.loss <- max(con.monthly.returns$DownCon)
  trades.per.day <- sum(portfolio$Result$Profit.TradesTotal) / trade.days
  
  mdd.portfolio <- maxdrawdown(coredata(equity.portfolio))$MDDP
  win.percent <- portfolio$Summary$WinPercent
  evaluation.table <- data.frame(
    stringsAsFactors = F,
    row.names = NULL,
    Check = condition.string,
    Description = c(sprintf(' greater than %.2f $', threshold[1]),
                    sprintf(' no more than %i Loss in a row', threshold[2]),
                    sprintf(' greater than %.2f trades per day', threshold[3]),
                    sprintf(' less than %.2f %%', threshold[4]),
                    sprintf(' greater than %.2f %%', threshold[5])),
    Value = c(sprintf('%.2f $', net.profit),
              sprintf('max %i', con.monthly.loss),
              sprintf('%.2f', trades.per.day),
              sprintf('%.2f %%', mdd.portfolio),
              sprintf('%.2f %%', win.percent)),
    Evalution = result[c(net.profit > threshold[1], con.monthly.loss <= threshold[2], trades.per.day > threshold[3], mdd.portfolio < threshold[4],
               win.percent > threshold[5]) + 1]
    
  )
  htmlTable(evaluation.table,
            caption = 'Evalution',
            rnames = F,
            css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
            col.rgroup = c("none", "#F9FAF0"),
            col.columns = c("none", "#F1F0FA")
  )
})


cal.scores <- cmpfun(function(yearly.return, max.dd, return.table, mdd.table, weight, level.table) {
  score.return <- return.table$Score[which(yearly.return <= return.table$Value)[1]]
  score.mdd <- mdd.table$Score[which(max.dd <= mdd.table$Value)[1]]
  score.total <- score.return * weight[1] + score.mdd * weight[2]
  level <- level.table$Level[which(score.total <= level.table$Value)[1]]
  list(
    Value = c(Y.Return = yearly.return, Max.DD = max.dd),
    Score = c(Y.Return = score.return, Max.DD = score.mdd),
    TotalScore = score.total,
    Level = level
  )
})

vplayout <- cmpfun(function(x, y) {
  viewport(layout.pos.row = x, layout.pos.col = y)
})

calendar.table <- cmpfun(function(xts, fun, digit = 2, percent = F, name = '') {
  xts <- na.omit(xts)
  year.first <- as.POSIXlt(index(xts[1]))$year + 1900
  year.last <- as.POSIXlt(index(tail(xts, 1)))$year + 1900
  year.labels <- year.first:year.last
  month.labels <- strftime(seq.Date(as.Date("2000-01-01"), length.out = 12, by = "months"), format = "%b")
  monthes <- levels(factor(format(time(xts), "%Y-%m")))
  table <- matrix(data = NA, nrow = length(year.labels), ncol = 13, dimnames = list(year.labels, c(month.labels, name)))
  sapply(monthes, cmpfun(function(month) {
    year <- substr(month, 1, 4)
    month.index <- as.numeric(substr(month, 6, 7))
    value <- fun(xts[month])
    if (percent) {
      value <- value * 100
    }
    table[year, month.index] <<- round(value, digit)
  }))
  sapply(as.character(year.labels), cmpfun(function(year) {
    value <- fun(xts[year])
    if (percent) {
      value <- value * 100
    }
    table[year, 13] <<- round(value, digit)
  }))
  as.data.frame(table)
})

