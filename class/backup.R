#### NOTE ####
# 2016-11-28: Create

require(compiler)

#### READ INPUT FILES ####


#### + READ HTML ####


#### ++ READ HTML MT4 EA ####

.read.html.mt4.ea <- cmpfun(function(file.link, html.parse) {
  # ''' work with mt4 ea from html file '''
  # 2016-08-17: TESTING
  info = .html.mt4.ea.info(file.link, html.parse)
  .build.report(
    type = 'MT4-EA',
    tickets = .html.mt4.ea.tickets(file.link, html.parse, info$Time),
    info = info
  )
})# FINISH



.html.mt4.ea.tickets <- cmpfun(function(file.link, html.parse, end.time) {
  # ''' get mt4 ea tickets '''
  # 2016-08-17: TESTING
  tables <- readHTMLTable(file.link, stringsAsFactors = FALSE)
  info.table <- tables[[1]]
  tickets.table <- tables[[2]]
  item <- .html.mt4.ea.tickets.item(html.parse)
  # print(info.table)
  # write.csv(info.table, 'abc.csv')
  tickets <- subset(tickets.table, subset = tickets.table[, 3] != 'modify', select = -c(1, 10))
  tickets[, 1] <- .format.report.tickets.time(tickets[, 1])
  # print(tickets)
  # print(class(tickets[,1]))
  tickets[, 3] <- as.numeric(tickets[, 3])
  pending.tickets.close.index <- which(tickets[, 2] == 'delete')
  if (length(pending.tickets.close.index) > 0) {
    pending.tickets.close <- tickets[pending.tickets.close.index,]
    pending.tickets.tickets <- pending.tickets.close[, 3]
    tickets._pending.tickets.close <- tickets[ - pending.tickets.close.index,]
    pending.tickets.open.index <- which(tickets._pending.tickets.close[, 3] %in% pending.tickets.tickets)
    pending.tickets.open <- tickets._pending.tickets.close[pending.tickets.open.index,]
    pending.tickets <- merge(pending.tickets.open, pending.tickets.close, by = 3)
    # pending.tickets$Item <- file.html.mt4.strategy.tickets.item(html.parse)
    # pending.tickets$Comment <- ''
    # pending.tickets <- build.report.tickets.pending.from.table(pending.tickets, c(1:4, 15, 5, 13, 14, 9, 12, 16))
    tickets.pending <- .build.report.tickets.pending.from.columns(
      ticket = pending.tickets[, 1],
      otime = as.character(pending.tickets[, 2]),
      type = pending.tickets[, 3],
      volume = pending.tickets[, 4],
      item = item,
      oprice = pending.tickets[, 5],
      sl = pending.tickets[, 13],
      tp = pending.tickets[, 14],
      ctime = as.character(pending.tickets[, 9]),
      cprice = pending.tickets[, 12]
    )
    closed.tickets <- tickets._pending.tickets.close[-pending.tickets.open.index, ]
  } else {
    tickets.pending <- NULL
    closed.tickets <- tickets
  }
  closed.pending.index <- which(grepl('(buy|sell) (limit|stop)', closed.tickets[, 2]))
  if (length(closed.pending.index) > 0) {
    closed.tickets <- closed.tickets[ - closed.pending.index,]
  }
  closed.tickets.open.index <- which(grepl('(buy|sell)', closed.tickets[, 2]))
  closed.tickets <- merge(closed.tickets[closed.tickets.open.index,], closed.tickets[ - closed.tickets.open.index,], by = 3)
  comment <- closed.tickets[, 10]
  close.at.stop.index <- which(grepl(' at ', comment))
  so.index.in.close.at.stop <- which(difftime(end.time, closed.tickets[close.at.stop.index, 9], units = 'mins') >= 1)
  if (length(so.index.in.close.at.stop) > 0) {
    so.index <- close.at.stop.index[so.index.in.close.at.stop]
    comment[so.index] <- 'so'
    closed.tickets[, 10] <- comment
  }
  part.closed.index <- which(closed.tickets[, 4] != closed.tickets[, 11])
  closed.tickets[part.closed.index, 4] <- closed.tickets[part.closed.index, 11]
  if (length(part.closed.index) > 0) {
    sapply(part.closed.index + 1, function(x) {
      closed.tickets[x, 2] <<- closed.tickets[x - 1, 2]
      # closed.tickets[x, 4] <- closed.tickets[x, 11]
    })
  }
  tickets.closed <- .build.report.tickets.closed.from.columns(
    ticket = closed.tickets[, 1],
    otime = as.character(closed.tickets[, 2]),
    type = closed.tickets[, 3],
    volume = closed.tickets[, 4],
    item = item,
    oprice = closed.tickets[, 5],
    sl = closed.tickets[, 13],
    tp = closed.tickets[, 14],
    ctime = as.character(closed.tickets[, 9]),
    cprice = closed.tickets[, 12],
    profit = closed.tickets[, 15],
    comment = closed.tickets[, 10]
  )
  tickets.money <- .build.report.tickets.money.from.columns(
    ticket = 0,
    otime = .html.mt4.ea.tickets.begin(html.parse),
    profit = .html.mt4.ea.tickets.capital(info.table)
  )
  .build.report.tickets.group(
    closed = tickets.closed,
    pending = tickets.pending,
    money = tickets.money
  )
})# FINISH

.html.mt4.ea.tickets.item <- cmpfun(function(html.parse) {
  # ''' mt4 ea item '''
  # 2016-08-14: Done
  item.string <- getNodeSet(html.parse, '//tr/td', fun = xmlValue)[[2]]
  gsub(' ([ \\(\\)[:alpha:]])*', '', item.string)
})# FINISH

.html.mt4.ea.tickets.begin <- cmpfun(function(html.parse) {
  # ''' mt4 ea info '''
  # 2016-08-14: Done
  time.string <- getNodeSet(html.parse, '//tr', fun = xmlValue)[2]
  nchar.time.string <- nchar(time.string)
  substr(time.string, nchar.time.string - 23, nchar.time.string - 14)
})# FINISH

.html.mt4.ea.tickets.capital <- cmpfun(function(info.table) {
  # ''' mt4 strategy capital '''
  # 2016-08-14: Done
  info.table[nrow(info.table) - 11, 2]
})# FINISH

#### ++ READ HTML MT4 TRADE ####

.read.html.mt4.trade <- cmpfun(function(file.link, html.parse) {
  # ''' read mt4 statement html file '''
  # 2016-08-16: CODING
  html.table <- readHTMLTable(file.link, stringsAsFactors = FALSE, encoding = 'UTF-8')[[1]]
  .build.report(
    type = 'MT4-Trade',
    tickets = .html.mt4.trade.tickets(html.table, html.parse),
    info = .html.mt4.trade.info(html.parse)
  )
})# FINISH

.html.mt4.trade.tickets <- cmpfun(function(html.table, html.parse) {
  # ''' mt4 html trade tickets '''
  # 2016-08-16: CODING
  html.table[html.table == ''] <- NA
  html.table$Comment <- .html.mt4.trade.tickets.comment(html.parse)
  suppressWarnings(tickets <- html.table[which(!is.na(as.numeric(html.table[, 1]))),])
  if (nrow(tickets) == 0) return(NULL)
  na.count <- as.numeric(rowSums(is.na(tickets)))
  .build.report.tickets.group(
    money = .build.report.tickets.money.from.table(tickets[which(na.count == 9), ], c(1, 2, 5, 15)),
    closed = .build.report.tickets.closed.from.table(tickets[which(na.count == 0), ], 1:15),
    open = .build.report.tickets.open.from.table(tickets[which(na.count == 1), ], c(1:8, 10:15)),
    pending = .build.report.tickets.pending.from.table(tickets[which(na.count == 3), ], c(1:10, 15)),
    working = .build.report.tickets.working.from.table(tickets[which(na.count == 5), ], c(1:8, 10, 15))
  )
})# FINISH

.html.mt4.trade.tickets.comment <- cmpfun(function(html.parse) {
  # ''' get comment for mt4 trade html '''
  # 2016-08-12: FINISH & Optimize: getNodeSet() arg:fun= xmlChildren
  tag_tr_children <- sapply(getNodeSet(html.parse, '//tr'), xmlChildren)
  sapply(tag_tr_children, function(title) {
    comment <- xmlGetAttr(title[[1]], 'title')
    return(ifelse(is.null(comment), '', comment))
  })[-1]
})# FINISH & OPTIMIZE



#### ++ READ HTML MT5 EA ####

.read.html.mt5.ea <- cmpfun(function(file.link) {
  # ''' mt5 html ea file '''
  # 2016-08-16: Working
  html_table <- readHTMLTable(file.link, stringsAsFactors = FALSE, encoding = 'UTF-8')
  .build.report(
    type = 'MT5-EA',
    tickets = .html.mt5.ea.tickets(html_table[[2]]),
    info = .html.mt5.ea.info(html_table[[1]])
  )
})# FINISH



.html.mt5.ea.tickets <- cmpfun(function(tickets.table) {
  # ''' mt5 html ea file: tickets '''
  # 2016-08-17: ToDo: 'end of test'
  first_col <- tickets.table[, 1]
  spaces_index <- which(first_col == '')
  deals <- .html.mt5.trade_ea.tickets.group(tickets.table, first_col, spaces_index, 'Deals')
  # .html.mt5.trade_ea.tickets.money_closed_open(deals)
  .build.report.tickets.group(
    closed = .html.mt5.trade_ea.tickets.money_closed_open(deals)
  )
})# FINISH

.html.mt5.trade_ea.tickets.group <- cmpfun(function(html.table, first.col, spaces.index, group.name) {
  # ''' get mt5 trade html group tickets '''
  # 2016-08-15: Done
  group.name.index <- which(first.col == group.name)
  if (length(group.name.index) == 0) return(NULL)
  group.begin.index <- group.name.index + 2
  group.end.index <- spaces.index[which(spaces.index > group.name.index)[1]] - 1
  if (group.begin.index > group.end.index) return(NULL)
  group <- html.table[group.begin.index:group.end.index,]
  colnames(group) <- html.table[group.name.index + 1,]
  group
})# FINISH

.html.mt5.trade_ea.tickets.money_closed_open <- cmpfun(function(deals, positions = NULL) {
  # ''' get money closed and open tickets for mt5 html file ''
  # 2016-08-17: TESTING
  if (is.null(deals)) return(NULL)
  money.index <- with(deals, which(Type == 'balance'))
  if (length(money.index) == 0) {
    money <- NULL
    closed_open <- deals
  } else {
    money <- deals[money.index, ]
    closed_open <- deals[ - money.index,]
    if (nrow(closed_open) == 0) {
      closed_open <- NULL
    }
  }
  tickets.money <- .html.mt5.trade.tickets.money(money)
  tickets.closed_open <- .html.mt5.trade.tickets.closed_open(closed_open, positions)
  rbind(tickets.money, tickets.closed_open)
})# FINISH

#### ++ READ HTML MT5 TRADE ####

.read.html.mt5.trade <- cmpfun(function(file.link) {
  # ''' mt5 html trade file '''
  # 2016-08-16: TESTING
  html_table <- readHTMLTable(file.link, stringsAsFactors = FALSE, encoding = 'UTF-8')[[1]]
  .build.report(
    type = 'MT5-Trade',
    tickets = .html.mt5.trade.tickets(html_table),
    info = .html.mt5.trade.info(html_table)
  )
})# FINISH



.html.mt5.trade.tickets <- cmpfun(function(html.table) {
  # ''' get mt5 strategy tickets '''
  # 2016-08-17: Working
  first_col <- html.table$V1
  spaces_index <- which(first_col == '')
  orders <- .html.mt5.trade_ea.tickets.group(html.table, first_col, spaces_index, 'Orders')
  positions <- .html.mt5.trade_ea.tickets.group(html.table, first_col, spaces_index, 'Trade Positions')
  workings <- .html.mt5.trade_ea.tickets.group(html.table, first_col, spaces_index, 'Working Orders')
  deals <- .html.mt5.trade_ea.tickets.group(html.table, first_col, spaces_index, 'Deals')
  positions.market.price <- .html.mt5.trade.tickets.positions.market.price(positions)
  .build.report.tickets.group(
    pending = .html.mt5.trade.tickets.pending(orders),
    working = .html.mt5.trade.tickets.working(workings),
    closed = .html.mt5.trade_ea.tickets.money_closed_open(deals, positions.market.price)
  )
})# FINISH

.html.mt5.trade.tickets.working <- cmpfun(function(workings) {
  # ''' work with workings, create working ticktes '''
  # 2016-08-16: Done
  if (is.null(workings)) return(NULL)
  with(workings, {
    .build.report.tickets.working.from.columns(
      ticket = Order,
      otime = as.character(`Open Time`),
      type = Type,
      volume = Volume,
      item = Symbol,
      oprice = Price,
      sl = `S / L`,
      tp = `T / P`,
      cprice = `Market Price`,
      comment = Comment
    )
  })
})# FINISH

.html.mt5.trade.tickets.pending <- cmpfun(function(orders) {
  # ''' handle mt5 trade html orders tickets pending '''
  # 2016-08-16: Done
  pending.index <- with(orders, which(State == 'canceled'))
  if (length(pending.index) == 0) return(NULL)
  pending <- orders[pending.index, ]
  with(pending, {
    .build.report.tickets.pending.from.columns(
      ticket = Order,
      otime = as.character(`Open Time`),
      type = Type,
      volume = Volume,
      item = Symbol,
      oprice = Price,
      sl = `S / L`,
      tp = `T / P`,
      ctime = as.character(Time),
      cprice = NA,
      comment = Comment
    )
  })
})# FINISH

.html.mt5.trade.tickets.money <- cmpfun(function(money) {
  # ''' get money tickets for mt5 html file ''
  # 2016-08-16: Done
  if (is.null(money)) return(NULL)
  with(money, {
    .build.report.tickets.money.from.columns(
      ticket = Deal,
      otime = as.character(Time),
      profit = Profit,
      comment = Comment
    )
  })
})# FINISH

.html.mt5.trade.tickets.closed_open <- cmpfun(function(deals.closed_open, positions) {
  # ''' get closed and open tickets for mt5 html file ''
  # 2016-08-16: TESTING
  if (is.null(deals.closed_open)) return(NULL)
  closed_open <- within(deals.closed_open, {
    Time <- .format.report.tickets.time(Time)
    Type <- toupper(Type)
    Volume <- as.numeric(Volume)
    Price <- as.numeric(Price)
    Order <- as.numeric(Order)
  })
  split.item <- split.data.frame(closed_open, closed_open$Symbol)
  do.call(rbind, lapply(split.item, .html.mt5.trade.tickets.closed_open.symbol, positions))
})# FINISH

.html.mt5.trade.tickets.closed_open.symbol <- cmpfun(function(symbol.trades, positions) {
  # ''' get single symbol closed and open tickets for mt5 html file '''
  # 2016-08-16: TESTING
  in_out.index <- with(symbol.trades, {
    which(Direction == 'in/out')
  })
  if (length(in_out.index) > 0) {
    volume.cumsum <- with(symbol.trades, {
      cumsum(ifelse(Type == 'BUY', Volume, -Volume))
    })
    in.volume.value <- abs(volume.cumsum[in_out.index])
    in_out.tickets <- symbol.trades[in_out.index,]
    other.tickets <- symbol.trades[ - in_out.index,]
    in_out.out <- within(in_out.tickets, {
      Direction <- 'out'
      Volume <- Volume - in.volume.value
      Time <- Time - 1
    })
    in_out.in <- within(in_out.tickets, {
      Direction <- 'in'
      Volume <- in.volume.value
    })
    symbol.trades <- sort.dataframe(rbind(other.tickets, in_out.out, in_out.in), 'Deal')
  }
  buy <- symbol.trades$Type == 'BUY'
  buy.index <- which(buy)
  sell.index <- which(!buy)
  in_ <- symbol.trades$Direction == 'in'
  in.index <- which(in_)
  out.index <- which(!in_)
  buy_in.index <- intersect(buy.index, in.index)
  buy_out.index <- intersect(buy.index, out.index)
  sell_in.index <- intersect(sell.index, in.index)
  sell_out.index <- intersect(sell.index, out.index)
  buy.tickets <- .html.mt5.trade.deals.closed_open.symbol.make.tickets(symbol.trades, buy_in.index, sell_out.index, positions, 'BUY')
  sell.tickets <- .html.mt5.trade.deals.closed_open.symbol.make.tickets(symbol.trades, sell_in.index, buy_out.index, positions, 'SELL')
  rbind(buy.tickets, sell.tickets)
})# FINISH

.html.mt5.trade.deals.closed_open.symbol.make.tickets <- cmpfun(function(symbol.trades, in.index, out.index, positions, type) {
  # ''' mt5 trade html file tickets '''
  # 2016-08-16: Done
  if (length(in.index) == 0) return(NULL)
  item <- symbol.trades$Symbol[1]
  deals.in <- symbol.trades$Deal[in.index]
  volume.in <- symbol.trades$Volume[in.index]
  deals.out <- symbol.trades$Deal[out.index]
  volume.out <- symbol.trades$Volume[out.index]
  volume.cumsum.in <- cumsum(volume.in)
  volume.cumsum.out <- cumsum(volume.out)
  volume.cumsum <- sort(union(volume.cumsum.in, volume.cumsum.out))
  tickets.in <- sapply(volume.cumsum, function(x) {
    deals.in[which(volume.cumsum.in >= x)[1]]
  })
  tickets.out <- sapply(volume.cumsum, function(x) {
    deals.out[which(volume.cumsum.out >= x)[1]]
  })
  tickets.volume <- c(volume.cumsum[1], diff(volume.cumsum))
  tickets.in.index <- match(tickets.in, symbol.trades$Deal)
  tickets.out.index <- match(tickets.out, symbol.trades$Deal)
  na.check <- is.na(tickets.out.index)
  open.index <- which(na.check)
  closed.index <- which(!na.check)
  if (length(open.index) == 0) {
    tickets.open <- NULL
  } else {
    open.tickets.in.index <- tickets.in.index[open.index]
    tickets.open <- with(symbol.trades, {
      .build.report.tickets.open.from.columns(
        ticket = Order[open.tickets.in.index],
        otime = as.character(Time[open.tickets.in.index]),
        type = type,
        volume = tickets.volume,
        item = item,
        oprice = Price[open.tickets.in.index],
        cprice = positions[item]
      )
    })
  }
  if (length(closed.index) == 0) {
    tickets.closed <- NULL
  } else {
    closed.tickets.in.index <- tickets.in.index[closed.index]
    closed.tickets.out.index <- tickets.out.index[closed.index]
    tickets.closed <- with(symbol.trades, {
      .build.report.tickets.closed.from.columns(
        ticket = Order[closed.tickets.in.index],
        otime = as.character(Time[closed.tickets.in.index]),
        type = type,
        volume = tickets.volume,
        item = item,
        oprice = symbol.trades$Price[closed.tickets.in.index],
        ctime = as.character(Time[closed.tickets.out.index]),
        cprice = Price[closed.tickets.out.index],
        commission = Commission[closed.tickets.out.index],
        swap = Swap[closed.tickets.out.index],
        profit = NA,
        comment = Comment[closed.tickets.out.index]
      )
    })
    tp.index <- which(tickets.closed$Comment == 'TP')
    sl.index <- which(tickets.closed$Comment == 'SL')
    tickets.closed <- within(tickets.closed, {
      TP[tp.index] <- CPrice[tp.index]
      SL[sl.index] <- CPrice[sl.index]
    })
  }
  rbind(tickets.open, tickets.closed)
})# FINISH

.html.mt5.trade.tickets.positions.market.price <- cmpfun(function(positions) {
  # ''' handle mt5 trade html positions '''
  # 2016-08-15: Done
  price <- as.numeric(positions$'Market Price')
  names(price) <- positions$Symbol
  price
})# FINISH



#### ++ READ HTML MT4M CLOSED ####

.read.html.mt4m.closed <- cmpfun(function(file.link) {
  html_table <- readHTMLTable(file.link, stringsAsFactors = FALSE, encoding = 'UTF-8')[[1]]
  html_table <- html_table[2:(nrow(html_table) - 1), ]
  .build.report(
    type = 'MT4M-Closed',
    tickets = .build.report.tickets.group(closed = .build.report.tickets.closed.from.columns(
      ticket = html_table[[1]],
      otime = html_table[[4]],
      type = html_table[[5]],
      volume = html_table[[7]],
      item = html_table[[6]],
      oprice = html_table[[8]],
      sl = 0,
      tp = 0,
      ctime = html_table[[9]],
      cprice = html_table[[10]],
      commission = html_table[[11]],
      taxes = html_table[[12]],
      swap = html_table[[14]],
      profit = html_table[[15]],
      comment = paste(html_table[[17]], html_table[[2]], sep = ' | Login: ')
    )),
    info = .build.report.info()
  )
  
  # tickets <- subset.data.frame(html_table, )
})

#### ++ READ HTML MT4M RAW ####

.read.html.mt4m.raw <- cmpfun(function(file.link) {
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
})

#### BUILD REPORT ####

.build.report <- cmpfun(function(tickets, info, type = NULL) {
  # ''' build report '''
  # 2016-08-14: Done
  list(
    # Tickets = report.tickets.add.mode(tickets, type),
    Tickets = tickets,
    Info = info,
    Type = type
  )
})# FINISH

.sort.dataframe <- cmpfun(function(dataframe, columns, decreasing = F) {
  # ''' sort dataframe with columns '''
  # 2016-08-15: Done
  dataframe[order(dataframe[, columns], decreasing = decreasing), ]
})# FINISH

#### BUILD TICKETS ####

.build.report.tickets <- cmpfun(function(ticket=NA, otime=NA, type=NA, volume=NA, item=NA, oprice=NA, sl=NA, tp=NA, ctime=NA,
                                         cprice=NA, commission=NA, taxes=NA, swap=NA, profit=NA, group=NA, comment=NA, exit=NA) {
  # ''' build report tickets '''
  # 2016-08-16: FINISH
  if (length(ticket) == 0) return(NULL)
  data.frame(
    stringsAsFactors = F,
    row.names = NULL,
    Ticket = .format.report.tickets.number(ticket),
    OTime = .format.report.tickets.time(otime),
    Type = .format.report.tickets.string(type),
    Volume = .format.report.tickets.number(volume),
    Item = .format.report.tickets.string(item),
    OPrice = .format.report.tickets.number(oprice),
    SL = .format.report.tickets.number(sl),
    TP = .format.report.tickets.number(tp),
    CTime = .format.report.tickets.time(ctime),
    CPrice = .format.report.tickets.number(cprice),
    Commission = .format.report.tickets.money(commission),
    Taxes = .format.report.tickets.money(taxes),
    Swap = .format.report.tickets.money(swap),
    Profit = .format.report.tickets.money(profit),
    Group = group,
    Comment = comment,
    Exit = .report.tickets.exit(comment)
  )
})# FINISH

.build.report.tickets.money.from.columns <- cmpfun(function(ticket, otime, profit, comment = '') {
  # ''' build report tickets: money, from columns '''
  # 2016-08-16: Done
  .build.report.tickets(
    ticket = ticket,
    otime = otime,
    profit = profit,
    group = 'MONEY',
    comment = comment
  )
})# FINISH

.build.report.tickets.money.from.table <- cmpfun(function(money.table, columns) {
  # ''' build report tickets: money, from data.frame '''
  # 2016-08-16: Done
  .build.report.tickets.money.from.columns(
    ticket = money.table[[columns[1]]],
    otime = money.table[[columns[2]]],
    profit = money.table[[columns[3]]],
    comment = money.table[[columns[4]]]
  )
})# FINISH

.build.report.tickets.closed.from.columns <- cmpfun(function(ticket, otime, type, volume, item, oprice, sl = 0, tp = 0, ctime, cprice, commission = 0, taxes = 0, swap = 0, profit = 0, comment = '') {
  # ''' build report tickets: closed, from columns '''
  # 2016-08-16: Done
  .build.report.tickets(
    ticket = ticket,
    otime = otime,
    type = type,
    volume = volume,
    item = item,
    oprice = oprice,
    sl = sl,
    tp = tp,
    ctime = ctime,
    cprice = cprice,
    commission = commission,
    taxes = taxes,
    swap = swap,
    profit = profit,
    group = 'CLOSED',
    comment = comment
  )
})# FINISH

.build.report.tickets.closed.from.table <- cmpfun(function(closed.table, columns) {
  # ''' build report tickets: closed, from data.frame '''
  # 2016-08-16: Done
  .build.report.tickets.closed.from.columns(
    ticket = closed.table[[columns[1]]],
    otime = closed.table[[columns[2]]],
    type = closed.table[[columns[3]]],
    volume = closed.table[[columns[4]]],
    item = closed.table[[columns[5]]],
    oprice = closed.table[[columns[6]]],
    sl = closed.table[[columns[7]]],
    tp = closed.table[[columns[8]]],
    ctime = closed.table[[columns[9]]],
    cprice = closed.table[[columns[10]]],
    commission = closed.table[[columns[11]]],
    taxes = closed.table[[columns[12]]],
    swap = closed.table[[columns[13]]],
    profit = closed.table[[columns[14]]],
    comment = closed.table[[columns[15]]]
  )
})# FINISH

.build.report.tickets.open.from.columns <- cmpfun(function(ticket, otime, type, volume, item, oprice, sl = 0, tp = 0, cprice, commission = 0, taxes = 0, swap = 0, profit = NA, comment = '') {
  # ''' build report tickets: open-, from columns '''
  # 2016-08-16: Done
  .build.report.tickets(
    ticket = ticket,
    otime = otime,
    type = type,
    volume = volume,
    item = item,
    oprice = oprice,
    sl = sl,
    tp = tp,
    cprice = cprice,
    commission = commission,
    taxes = taxes,
    swap = swap,
    profit = profit,
    group = 'OPEN',
    comment = comment
  )
})# FINISH

.build.report.tickets.open.from.table <- cmpfun(function(open.table, columns) {
  # ''' build report tickets: open-, from data.frame '''
  # 2016-08-17: Done
  .build.report.tickets.open.from.columns(
    ticket = open.table[[columns[1]]],
    otime = open.table[[columns[2]]],
    type = open.table[[columns[3]]],
    volume = open.table[[columns[4]]],
    item = open.table[[columns[5]]],
    oprice = open.table[[columns[6]]],
    sl = open.table[[columns[7]]],
    tp = open.table[[columns[8]]],
    cprice = open.table[[columns[9]]],
    commission = open.table[[columns[10]]],
    taxes = open.table[[columns[11]]],
    swap = open.table[[columns[12]]],
    profit = open.table[[columns[13]]],
    comment = open.table[[columns[14]]]
  )
})# FINISH

.build.report.tickets.pending.from.columns <- cmpfun(function(ticket, otime, type, volume, item, oprice, sl = 0, tp = 0, ctime, cprice, comment = '') {
  # ''' build report tickets: pending, from columns '''
  # 2016-08-16: Done
  .build.report.tickets(
    ticket = ticket,
    otime = otime,
    type = type,
    volume = volume,
    item = item,
    oprice = oprice,
    sl = sl,
    tp = tp,
    ctime = ctime,
    cprice = cprice,
    group = 'PENDING',
    comment = comment
  )
})# FINISH

.build.report.tickets.pending.from.table <- cmpfun(function(pending.table, columns) {
  # ''' build report tickets: pending, from data.frame '''
  # 2016-08-16: Done
  .build.report.tickets.pending.from.columns(
    ticket = pending.table[[columns[1]]],
    otime = pending.table[[columns[2]]],
    type = pending.table[[columns[3]]],
    volume = pending.table[[columns[4]]],
    item = pending.table[[columns[5]]],
    oprice = pending.table[[columns[6]]],
    sl = pending.table[[columns[7]]],
    tp = pending.table[[columns[8]]],
    ctime = pending.table[[columns[9]]],
    cprice = pending.table[[columns[10]]],
    comment = pending.table[[columns[11]]]
  )
})# FINISH

.build.report.tickets.working.from.columns <- cmpfun(function(ticket, otime, type, volume, item, oprice, sl = 0, tp = 0, cprice, comment = '') {
  # ''' build report tickets: working, from columns '''
  # 2016-08-17: Done
  .build.report.tickets(
    ticket = ticket,
    otime = otime,
    type = type,
    volume = volume,
    item = item,
    oprice = oprice,
    sl = sl,
    tp = tp,
    cprice = cprice,
    group = 'Working',
    comment = comment
  )
})# FINISH

.build.report.tickets.working.from.table <- cmpfun(function(working.table, columns) {
  # ''' build report tickets: working, from data.frame '''
  # 2016-08-17: Done
  .build.report.tickets.working.from.columns(
    ticket = working.table[[columns[1]]],
    otime = working.table[[columns[2]]],
    type = working.table[[columns[3]]],
    volume = working.table[[columns[4]]],
    item = working.table[[columns[5]]],
    oprice = working.table[[columns[6]]],
    sl = working.table[[columns[7]]],
    tp = working.table[[columns[8]]],
    cprice = working.table[[columns[9]]],
    comment = working.table[[columns[10]]]
  )
})# FINISH

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

<<<<<<< HEAD

=======
.report.tickets.exit <- cmpfun(function(comments) {
  # ''' get report tickets column: exit from comment'''
  # 2016-12-01: CODING
  comments <- toupper(comments)
  comments <- gsub('/| / ', '', comments)
  exit <- vector(mode = 'character', length = length(comments))
  exit[grep('SO', comments)] <- 'SO'
  exit[grep('SL', comments)] <- 'SL'
  exit[grep('TP', comments)] <- 'TP'
  exit
})# FINISH
>>>>>>> 0ff17fdc68e57f8ef76f8ba25ded1c096efb86e2


