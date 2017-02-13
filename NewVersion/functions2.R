## 2017-02-05: Version 0.1

library(compiler)
compilePKGS(T)




#### FETCH TICKETS ####
fetch.html.data.tickets.mt4ea <- function(mq.file, mq.file.parse,
                                          symbols.setting=SYMBOLS.SETTING) {
  
  table <- readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'GBK', which = 2,
                         colClasses = c('character', format.time.all.to.numeric, 'character', rep('numeric', 7))) %>%
    as.data.table %>%
    set_colnames(c('deal', 'time', 'type', 'ticket', 'volume', 'price', 'sl', 'tp', 'profit', 'balance')) %>%
    extract(type != 'modify', -c('deal', 'balance'))
  xml.text <-
    mq.file.parse %>%
    xml_find_first('.//table') %>%
    xml_find_all('.//td') %>%
    xml_text
  deposit <-
    xml.text %>%
    extract(24) %>%
    as.numeric
  time.string <- xml.text[4]
  len.time.string <- nchar(time.string)
  deposit.time <-
    time.string %>%
    substr(len.time.string - 23, len.time.string - 14) %>%
    format.time.all.to.numeric
  end.time <-
    time.string %>%
    substr(len.time.string - 10, len.time.string - 1) %>%
    format.time.all.to.numeric
  money.tickets <-
    data.table(
      TICKET = 0,
      OTIME = deposit.time,
      PROFIT = deposit
    ) %>%
    build.tickets('MONEY')
  rows <- nrow(table)
  if (!rows) {
    return(money.tickets)
  }
  item <-
    xml.text %>%
    extract(2) %>%
    gsub(' ([ \\(\\)[:alpha:]])*', '', .)
  table.index <- 1:rows
  table.types <- table[, type]
  table.tickets <- table[, ticket]
  pending.close.part.index <- which(table.types == 'delete')
  pending.tickets <-
    if (length(pending.close.part.index) > 0) {
      pending.tickets.ticket <- table.tickets[pending.close.part.index]
      table.index %<>% setdiff(pending.close.part.index)
      pending.open.part.index <- table.index[table.tickets[table.index] %in% pending.tickets.ticket]
      table.index %<>% setdiff(pending.open.part.index)
      merge(table[pending.open.part.index], table[pending.close.part.index], by = 'ticket') %>%
        set_colnames(c('TICKET', 'OTIME', 'TYPE', '', 'OPRICE', '', '', '',
                   'CTIME', '', 'VOLUME', 'CPRICE', 'SL', 'TP', 'PROFIT')) %>%
        extract(j = (c('ITEM', 'COMMENT')) := list(item, 'cancelled')) %>%
        build.tickets('PENDING')
    } else {
      NULL
    }
  pending.of.closed.ticktets.index <- table.index[grepl('(buy|sell) (limit|stop)', table.types[table.index])]
  if (length(pending.of.closed.ticktets.index) > 0) {
    table.index %<>% setdiff(pending.of.closed.ticktets.index)
  }
  closed.tickets <-
    if (length(table.index) > 0) {
      closed.tickets.open.part.index <- table.index[grepl('(buy|sell)', table.types[table.index])]
      closed.tickets.close.part.index <- table.index %<>% setdiff(closed.tickets.open.part.index)
      closed.tickets <- merge(table[closed.tickets.open.part.index], table[closed.tickets.close.part.index], by = 'ticket')
      part.closed.index <- which(closed.tickets[, volume.x != volume.y])
      if (length(part.closed.index) > 0) {
        closed.tickets[part.closed.index, volume.x := volume.y]
        closed.tickets[part.closed.index + 1, time.x := NA]
        closed.tickets[, time.x := na.locf(time.x)]
      }
      closed.tickets %<>%
        set_colnames(c('TICKET', 'OTIME', 'TYPE', '', 'OPRICE', '', '', '',
                   'CTIME', 'COMMENT', 'VOLUME', 'CPRICE', 'SL', 'TP', 'PROFIT')) %>%
        extract(j = ITEM := item) %>%
        build.tickets('CLOSED') %>%
        extract(CTIME >= end.time - 60 & COMMENT == 'close at stop', EXIT := 'SO')
      symbol <- item.to.symbol(item)
      if (symbol != '') {

        closed.tickets[, {
          pips <- cal.pips(TYPE, OPRICE, CPRICE, symbols.setting[symbol, DIGITS])
          tickvalue <- cal.tick.value(symbol, CTIME)## ToDo ####
          ## ToDo ####
        }]
      }
    } else {
      NULL
    }

  # pending.tickets
}



fetch.html.data.tickets.mt5ea <- function(mq.file) {
  
  table <- readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 2,
                         colClasses = c('character', 'numeric', rep('character', 3), rep('numeric', 7), 'character')) #%>%
  # table.values <- xml_text(xml_find_all(xml_find_first(mq.file.parse, '//table'), './/td'))
  # time.string <- table.values[grep('Period:', table.values) + 1]
  # nchar.time.string <- nchar(time.string)
  # build.infos(
  #   type = 'MT5-EA',
  #   name = table.values[grep('Expert:', table.values) + 1],
  #   broker = table.values[grep('Broker:', table.values) + 1],
  #   currency = table.values[grep('Currency:', table.values) + 1],
  #   leverage = table.values[grep('Leverage:', table.values) + 1],
  #   time = substr(time.string, nchar.time.string - 10, nchar.time.string - 1)
  # )
}

fetch.html.data.tickets.mt5trade <- function(mq.file) {
  
  table <- readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1)
  # table.values <- xml_text(xml_find_all(xml_find_first(mq.file.parse, '//table'), './/th'))
  # account.currency.leverage <- table.values[grep('Account:', table.values) + 1]
  # build.infos(
  #   type = 'MT5-Trade',
  #   account = account.currency.leverage,
  #   name = table.values[grep('Name:', table.values) + 1],
  #   broker = table.values[grep('Broker:', table.values) + 1],
  #   currency = account.currency.leverage,
  #   leverage = account.currency.leverage,
  #   time = format.infos.time(table.values[grep('Date:', table.values) + 1]) - 8 * 3600
  # )
}

fetch.html.data.tickets.mt4m_closed <- function(mq.file) {
  table <- readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1)
  
  # td.nodes <- xml_find_all(xml_find_all(mq.file.parse, '//table'), './/td')
  # table.values <- xml_text(td.nodes)
  # colspan <- as.numeric(xml_attr(td.nodes, 'colspan'))
  # build.html.table(table.values, colspan, 17)
}

fetch.html.data.tickets.mt4m_raw <- function(mq.file) {
  table <- readHTMLTable(mq.file, colClasses = c(rep('character', 23)), stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1)
  # build.infos(
  #   type = 'MT4M-Raw'
  # )
}











