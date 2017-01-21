#### NOTE ####
# 2016-11-24: Create

require(compiler)

#### read file ####


build.report.tickets.group <- cmpfun(function(closed = NULL, open = NULL, money = NULL, pending = NULL, working = NULL) {
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
  sort.dataframe(within(all.tickets, CTime <- reform.report.tickets.time(CTime)), 'OTime')
})# 2016-08-16: Done

reform.report.tickets.number <- cmpfun(function(number) {
  # ''' reform report tickets column: number '''
  # 2016-08-16: Done
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
})# 2016-08-16: Done

reform.report.tickets.money <- cmpfun(function(money) {
  # ''' reform report tickets column: money '''
  # 2016-08-16: Done
  if (is.numeric(money)) {
    return(money)
  }
  if (is.character(money)) {
    money[money == ''] <- '0'
    return(as.numeric(gsub(' ', '', money)))
  }
  NA
})# 2016-08-16: Done

reform.report.tickets.time <- cmpfun(function(time) {
  # ''' reform report tickets column: time '''
  # 2016-08-16: TESTING
  reform.time(time)
})# 2016-08-16: TESTING

reform.report.tickets.string <- cmpfun(function(string) {
  # ''' reform report tickets column: string '''
  # 2016-08-16: Done
  toupper(string)
})# 2016-08-16: Done



sort.dataframe <- function(dataframe, columns, decreasing = F) {
  # ''' sort dataframe with columns '''
  # 2016-08-15: Done
  dataframe[order(dataframe[, columns], decreasing = decreasing), ]
} # 2016-08-15: Done



#-----------------------------------

reform.time <- function(time) {
  # ''' reform report info: time '''
  # 2016-08-16: Done
  # 2016-08-16: ToDo: if (grepl(',', time[1])) only for first element!
  if (length(time) > 1) {
    return(as.POSIXct(sapply(time, reform.time), origin = '1970-01-01', tz = 'GMT'))
  }
  if (all(class(time) == c("POSIXct", "POSIXt"))) {
    return(time)
  }
  if (is.character(time)) {
    if (grepl(',', time[1])) {
      return(reform.mt4.report.info.time(time))
    }
    time <- gsub('-', '.', time[1])
    format <- '%Y.%m.%d %H:%M:%S'
    sub_format <- substr(format, 1, nchar(time) - 2)
    return(as.POSIXct(time, format = sub_format, tz = 'GMT'))
  }
  if (is.numeric(time)) {
    return(as.POSIXct(time, origin = '1970-01-01', tz = 'GMT'))
  }
  NA
} # 2016-08-16: ToDo

file.html.mt4.trade <- cmpfun(function(file.link, html.parse) {
  # ''' work with mt4 statement html file '''
  # 2016-08-16: Done
  html_table <- readHTMLTable(file.link, stringsAsFactors = FALSE, encoding = 'UTF-8')[[1]]
  build.report(
    type = 'MT4 - Trade',
    tickets = file.html.mt4.trade.tickets(html_table, html.parse),
    info = file.html.mt4.trade.info(html.parse)
  )
})# 2016-08-16: Done

file.html.mt4.trade.info <- cmpfun(function(html.parse) {
  # ''' get mt4 html trade file: info '''
  # 2016-08-16: Done
  infos <- sapply(getNodeSet(html.parse, '//b')[1:8], xmlValue)
  time.index <- which(grepl('Trans', infos)) - 1
  others <- infos[2:(time.index - 1)]
  build.report.info(
    account = others[grep('Account', others)],
    name = others[grep('Name', others)],
    broker = infos[1],
    currency = others[grep('Currency', others)],
    leverage = others[grep('Leverage', others)],
    time = infos[time.index]
  )
})# 2016-08-16: Done

file.html.mt4.trade.tickets <- cmpfun(function(html.table, html.parse) {
  # ''' mt4 html trade tickets '''
  # 2016-08-16: Done
  # 2016-08-16: Optimate
  html.table[html.table == ''] <- NA
  html.table$Comment <- file.html.mt4.trade.tickets.comment(html.parse)
  suppressWarnings(tickets <- html.table[which(!is.na(as.numeric(html.table[, 1]))),])
  if (nrow(tickets) == 0) return(NULL)
  na.count <- as.numeric(rowSums(is.na(tickets)))
  build.report.tickets.group(
    money = build.report.tickets.money.from.table(tickets[which(na.count == 9), ], c(1, 2, 5, 15)),
    closed = build.report.tickets.closed.from.table(tickets[which(na.count == 0), ], 1:15),
    open = build.report.tickets.open.from.table(tickets[which(na.count == 1), ], c(1:8, 10:15)),
    pending = build.report.tickets.pending.from.table(tickets[which(na.count == 3), ], c(1:10, 15)),
    working = build.report.tickets.working.from.table(tickets[which(na.count == 5), ], c(1:8, 10, 15))
  )
})# 2016-08-16: Optimate

file.html.mt4.trade.tickets.comment <- cmpfun(function(html.parse) {
  # ''' get comment for mt4 statement html file '''
  # 2016-08-12: ToDo: getNodeSet() arg:fun= xmlChildren
  tag_tr_children <- sapply(getNodeSet(html.parse, '//tr'), xmlChildren)
  comments <- sapply(tag_tr_children, function(title) {
    comment <- xmlGetAttr(title[[1]], 'title')
    return(ifelse(is.null(comment), '', comment))
  })[-1]
})# 2016-08-12: ToDo


file.html.mt4.strategy <- cmpfun(function(file.link, html.parse) {
  # ''' work with mt4 strategy from html file '''
  # 2016-08-17: TESTING
  info <- file.html.mt4.strategy.info(file.link, html.parse)
  build.report(
    type = 'MT4 - EA',
    tickets = file.html.mt4.strategy.tickets(file.link, html.parse, info$Time),
    info = info
  )
})# 2016-08-17: TESTING

file.html.mt4.strategy.info <- cmpfun(function(file.link, html.parse) {
  # ''' mt4 strategy info '''
  # 2016-08-17: Done
  head.lines <- getNodeSet(html.parse, '//b', fun = xmlValue)[2:3]
  time.string <- getNodeSet(html.parse, '//tr', fun = xmlValue)[2]
  nchar.time.string <- nchar(time.string)
  time <- substr(time.string, nchar.time.string - 10, nchar.time.string - 1)
  build.report.info(
    name = head.lines[[1]],
    broker = head.lines[[2]],
    time = time
  )
})# 2016-08-17: Done

file.html.mt4.strategy.tickets <- cmpfun(function(file.link, html.parse, end.time) { #}, item.symbol.mapping, support.symbols.table) {
  # ''' get mt4 strategy tickets '''
  # 2016-08-17: TESTING
  item <- file.html.mt4.strategy.tickets.item(html.parse)
  tickets.table <- readHTMLTable(file.link, stringsAsFactors = FALSE)[[2]]
  
  tickets <- subset(tickets.table, subset = tickets.table[, 3] != 'modify', select = -c(1, 10))
  tickets[, 1] <- reform.time(tickets[, 1])
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
    tickets.pending <- build.report.tickets.pending.from.columns(
      ticket = pending.tickets[, 1],
      otime = pending.tickets[, 2],
      type = pending.tickets[, 3],
      volume = pending.tickets[, 4],
      item = item,
      oprice = pending.tickets[, 5],
      sl = pending.tickets[, 13],
      tp = pending.tickets[, 14],
      ctime = pending.tickets[, 9],
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
  tickets.closed <- build.report.tickets.closed.from.columns(
    ticket = closed.tickets[, 1],
    otime = closed.tickets[, 2],
    type = closed.tickets[, 3],
    volume = closed.tickets[, 4],
    item = item,
    oprice = closed.tickets[, 5],
    sl = closed.tickets[, 13],
    tp = closed.tickets[, 14],
    ctime = closed.tickets[, 9],
    cprice = closed.tickets[, 12],
    profit = closed.tickets[, 15],
    comment = closed.tickets[, 10]
  )
  tickets.money <- build.report.tickets.money.from.columns(
    ticket = 0,
    otime = file.html.mt4.strategy.tickets.begin(html.parse),
    profit = file.html.mt4.strategy.tickets.capital(html.parse)
  )
  build.report.tickets.group(
    closed = tickets.closed,
    pending = tickets.pending,
    money = tickets.money
  )
})# 2016-08-17: TESTING

file.html.mt4.strategy.tickets.item <- cmpfun(function(html.parse) {
  # ''' mt4 strategy item '''
  # 2016-08-14: Done
  item.string <- getNodeSet(html.parse, '//tr/td', fun = xmlValue)[[2]]
  gsub(' ([ \\(\\)[:alpha:]])*', '', item.string)
})# 2016-08-14: Done

file.html.mt4.strategy.tickets.begin <- cmpfun(function(html.parse) {
  # ''' mt4 strategy info '''
  # 2016-08-14: Done
  time.string <- getNodeSet(html.parse, '//tr', fun = xmlValue)[2]
  nchar.time.string <- nchar(time.string)
  substr(time.string, nchar.time.string - 23, nchar.time.string - 14)
})# 2016-08-14: Done

file.html.mt4.strategy.tickets.capital <- cmpfun(function(html.parse) {
  # ''' mt4 strategy capital '''
  # 2016-08-14: Done
  xmlValue(xmlChildren(getNodeSet(html.parse, '//tr')[[7]])[[2]])
})# 2016-08-14: Done

#### strategy ####

file.html.mt5.strategy <- cmpfun(function(file.link) {
  # ''' mt5 html strategy file '''
  # 2016-08-16: Working
  html_table <- readHTMLTable(file.link, stringsAsFactors = FALSE, encoding = 'UTF-8')
  build.report(
    type = 'MT5 - EA',
    tickets = file.html.mt5.strategy.tickets(html_table[[2]]),
    info = file.html.mt5.strategy.info(html_table[[1]])
  )
})

file.html.mt5.strategy.info <- cmpfun(function(info.table) {
  # ''' mt5 html strategy file: info '''
  # 2016-08-16: Done
  labels <- info.table[, 1]
  values <- info.table[, 2]
  time.string <- values[which(grepl('Period', labels))[1]]
  nchar.time.string <- nchar(time.string)
  build.report.info(
    broker = values[which(grepl('Broker', labels))[1]],
    name = values[which(grepl('Expert', labels))[1]],
    time = substr(time.string, nchar.time.string - 10, nchar.time.string - 1),
    currency = values[which(grepl('Currency', labels))[1]],
    leverage = values[which(grepl('Leverage', labels))[1]]
  )
})# 2016-08-16: Done

file.html.mt5.strategy.tickets <- cmpfun(function(tickets.table) {
  # ''' mt5 html strategy file: tickets '''
  # 2016-08-17: ToDo: 'end of test'
  first_col <- tickets.table[, 1]
  spaces_index <- which(first_col == '')
  deals <- file.html.mt5.trade_strategy.tickets.group(tickets.table, first_col, spaces_index, 'Deals')
  file.html.mt5.trade_strategy.tickets.money_closed_open(deals)
  build.report.tickets.group(
    closed = file.html.mt5.trade_strategy.tickets.money_closed_open(deals)
  )
})# 2016-08-17: ToDo

file.html.mt5.trade_strategy.tickets.group <- cmpfun(function(html.table, first.col, spaces.index, group.name) {
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
})# 2016-08-15: Done

file.html.mt5.trade_strategy.tickets.money_closed_open <- cmpfun(function(deals, positions = NULL) {
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
  tickets.money <- file.html.mt5.trade.tickets.money(money)
  tickets.closed_open <- file.html.mt5.trade.tickets.closed_open(closed_open, positions)
  rbind(tickets.money, tickets.closed_open)
})# 2016-08-17: TESTING

#### Trade ####

file.html.mt5.trade <- cmpfun(function(file.link) {
  # ''' mt5 html trade file '''
  # 2016-08-16: TESTING
  html_table <- readHTMLTable(file.link, stringsAsFactors = FALSE, encoding = 'UTF-8')[[1]]
  build.report(
    type = 'MT5 - Trade',
    tickets = file.html.mt5.trade.tickets(html_table),
    info = file.html.mt5.trade.info(html_table)
  )
})# 2016-08-16: TESTING

file.html.mt5.trade.info <- cmpfun(function(html.table) {
  # ''' mt5 html trade info '''
  # 2016-08-17: Done
  head.info <- html.table$V2[1:4]
  within(build.report.info(
    account = head.info[2],
    name = head.info[1],
    broker = head.info[3],
    currency = head.info[2],
    leverage = head.info[2],
    time = head.info[4]
  ), Time <- Time - 8 * 3600)
})# 2016-08-17: Done

file.html.mt5.trade.tickets <- cmpfun(function(html.table) {
  # ''' get mt5 strategy tickets '''
  # 2016-08-17: Working
  first_col <- html.table$V1
  spaces_index <- which(first_col == '')
  orders <- file.html.mt5.trade_strategy.tickets.group(html.table, first_col, spaces_index, 'Orders')
  positions <- file.html.mt5.trade_strategy.tickets.group(html.table, first_col, spaces_index, 'Trade Positions')
  workings <- file.html.mt5.trade_strategy.tickets.group(html.table, first_col, spaces_index, 'Working Orders')
  deals <- file.html.mt5.trade_strategy.tickets.group(html.table, first_col, spaces_index, 'Deals')
  positions.market.price <- file.html.mt5.trade.tickets.positions.market.price(positions)
  build.report.tickets.group(
    pending = file.html.mt5.trade.tickets.pending(orders),
    working = file.html.mt5.trade.tickets.working(workings),
    closed = file.html.mt5.trade_strategy.tickets.money_closed_open(deals, positions.market.price)
  )
})# 2016-08-17: Working



file.html.mt5.trade.tickets.working <- cmpfun(function(workings) {
  # ''' work with workings, create working ticktes '''
  # 2016-08-16: Done
  if (is.null(workings)) return(NULL)
  with(workings, {
    build.report.tickets.working.from.columns(
      ticket = Order,
      otime = `Open Time`,
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
})# 2016-08-16: Done

file.html.mt5.trade.tickets.pending <- cmpfun(function(orders) {
  # ''' handle mt5 trade html orders tickets pending '''
  # 2016-08-16: Done
  pending.index <- with(orders, which(State == 'canceled'))
  if (length(pending.index) == 0) return(NULL)
  pending <- orders[pending.index, ]
  with(pending, {
    build.report.tickets.pending.from.columns(
      ticket = Order,
      otime = `Open Time`,
      type = Type,
      volume = Volume,
      item = Symbol,
      oprice = Price,
      sl = `S / L`,
      tp = `T / P`,
      ctime = Time,
      cprice = NA,
      comment = Comment
    )
  })
})# 2016-08-16: Done

file.html.mt5.trade.tickets.money <- cmpfun(function(money) {
  # ''' get money tickets for mt5 html file ''
  # 2016-08-16: Done
  if (is.null(money)) return(NULL)
  with(money, {
    build.report.tickets.money.from.columns(
      ticket = Deal,
      otime = Time,
      profit = Profit,
      comment = Comment
    )
  })
})# 2016-08-16: Done

file.html.mt5.trade.tickets.closed_open <- cmpfun(function(deals.closed_open, positions) {
  # ''' get closed and open tickets for mt5 html file ''
  # 2016-08-16: TESTING
  if (is.null(deals.closed_open)) return(NULL)
  closed_open <- within(deals.closed_open, {
    Time <- reform.time(Time)
    Type <- toupper(Type)
    Volume <- as.numeric(Volume)
    Price <- as.numeric(Price)
    Order <- as.numeric(Order)
  })
  split.item <- split.data.frame(closed_open, closed_open$Symbol)
  do.call(rbind, lapply(split.item, file.html.mt5.trade.tickets.closed_open.symbol, positions))
})# 2016-08-16: TESTING



file.html.mt5.trade.tickets.closed_open.symbol <- cmpfun(function(symbol.trades, positions) {
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
  buy.tickets <- file.html.mt5.trade.deals.closed_open.symbol.make.tickets(symbol.trades, buy_in.index, sell_out.index, positions, 'BUY')
  sell.tickets <- file.html.mt5.trade.deals.closed_open.symbol.make.tickets(symbol.trades, sell_in.index, buy_out.index, positions, 'SELL')
  rbind(buy.tickets, sell.tickets)
})# 2016-08-16: TESTING

file.html.mt5.trade.deals.closed_open.symbol.make.tickets <- cmpfun(function(symbol.trades, in.index, out.index, positions, type) {
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
      build.report.tickets.open.from.columns(
        ticket = Order[open.tickets.in.index],
        otime = Time[open.tickets.in.index],
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
      build.report.tickets.closed.from.columns(
        ticket = Order[closed.tickets.in.index],
        otime = Time[closed.tickets.in.index],
        type = type,
        volume = tickets.volume,
        item = item,
        oprice = symbol.trades$Price[closed.tickets.in.index],
        ctime = Time[closed.tickets.out.index],
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
})# 2016-08-16: Done

file.html.mt5.trade.tickets.positions.market.price <- cmpfun(function(positions) {
  # ''' handle mt5 trade html positions '''
  # 2016-08-15: Done
  price <- as.numeric(positions$'Market Price')
  names(price) <- positions$Symbol
  price
})# 2016-08-15: Done

