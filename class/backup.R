#### NOTE ####
# 2016-11-28: Create

require(compiler)

#### READ INPUT FILES ####


#### ++ READ HTML MT5 EA ####




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

