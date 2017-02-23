
rm(list = ls())

package.list <- search()
if ('package:RMitekeLab' %in% package.list) {
  detach('package:RMitekeLab', unload = TRUE)
}
# if ('package:parallel' %in% package.list) {
#   detach('package:parallel', unload = TRUE)
# }
library(RMitekeLab)
library(parallel)
library(XML)
library(xml2)
library(data.table)
library(magrittr)
library(stringr)
# source('./TEST/test.class.R')

# BUG MT4-EA//Trade 无法并行

files <- file.path('.', 'TEST_FILE', dir('TEST_FILE'))[1:10]


# cl <- makeCluster(detectCores())
# clusterExport(cl, envir = METAQUOTE.ANALYSTIC)
# clusterEvalQ(cl, {library(RMitekeLab); NULL})
TEST.CLASS <- MQ_ANALYSTIC$new()

time1 <- system.time({

  TEST.CLASS$add.files(files)
  # cl <- makeCluster(3)
  # stopCluster()
})
print(time1)
## 8.56 Secs

time2 <- system.time({

  TEST.CLASS$get.report('TICKETS.RAW')

})
print(time2) ##  no parallel 62.42 Secs ## 35.5 Secs for parallel


# time.new <- system.time({
#   # table.new <- read.mq.file(files)
#   # table.new <- readHTMLTable(files, colClasses = c(rep('character', 23)), stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1)
#   parse <- read_html(files)
#   table <- html_table(parse, fill = TRUE)
# })
# print(time.new)
## 2.85 Secs
# %>% 



# time.old <- system.time({
#   # parse <- htmlParse(files)
#   # parse <- read_html(files, encoding = 'GBK')
#   # table.old <- lapply(files, readHTMLTable)
#   # table.old <- readHTMLTable(files, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1)
#   
#   # xml2parse <- read_html(files)
#   # readHTMLTable(xml2parse)
#   # htmlParse(files)
#   
#   old.data <- read.mq.file(files)
#   raw.tickets <- get.tickets.raw()
#   # print(get.tickets.raw())
#   # old.data <- fetch.html.data.tickets.mt4trade()
#   # old.data <- fetch.html.data.tickets.mt4m_raw(files)
#   # old.data <- fetch.html.data.tickets.mt4m_closed(files)
#   # old.data <- fetch.html.data.tickets.mt5ea(files)
#   # old.data <- fetch.html.data.tickets.mt5trade(files)
#   
#   # old.data <- mysql.price.open('EURUSD', c(1451606400, 1451106400))
# })
# print(time.old)
# print(old.data)
# print(raw.tickets)
# print(str(raw.tickets))
# print(get.infos())
# print(T <- old.data[[1]])
# print(str(get.tickets.raw()[[1]]))


## 31.58 Secs for parLapply(cl, xx, read.mq.file)
## 87.06 Secs for lapply(xxx, read.mq.file)


## after optimization 59.46 Secs for all 10 files fetched raw tickets

#### 7.97 Secs for old infos fetch method ####
#### 2.05 Secs for new infos fetch method ####