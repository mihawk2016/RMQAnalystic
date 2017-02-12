
rm(list = ls())

package.list <- search()
if ('package:RMitekeLab' %in% package.list) {
  detach('package:RMitekeLab', unload = TRUE)
}
if ('package:parallel' %in% package.list) {
  detach('package:parallel', unload = TRUE)
}
library(RMitekeLab)
library(parallel)
library(XML)
library(xml2)
library(data.table)
library(magrittr)
library(stringr)
# source('./NewVersion/functions2.R')

files <- file.path('.', 'TEST_FILE', dir('TEST_FILE'))[1:10]

# cl <- makeCluster(detectCores())
# # clusterExport(cl, files)
# 
# time <- system.time({
#   infos <- read.mq.file(files, cl)
# })
# print(time)
# ## 8.56 Secs



# time.new <- system.time({
#   # table.new <- read.mq.file(files)
#   # table.new <- readHTMLTable(files, colClasses = c(rep('character', 23)), stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1)
#   parse <- read_html(files)
#   table <- html_table(parse, fill = TRUE)
# })
# print(time.new)
# ## 2.85 Secs




time.old <- system.time({
  # parse <- htmlParse(files)
  # parse <- read_html(files, encoding = 'GBK')
  # table.old <- lapply(files, readHTMLTable)
  # table.old <- readHTMLTable(files, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1)
  
  # xml2parse <- read_html(files)
  # readHTMLTable(xml2parse)
  # htmlParse(files)
  old.data <- read.mq.file(files)
})
print(time.old)
print(T <- old.data[[1]])
## 31.58 Secs for parLapply(cl, xx, read.mq.file)
## 87.06 Secs for lapply(xxx, read.mq.file)


#### 7.97 Secs for old infos fetch method ####
#### 2.05 Secs for new infos fetch method ####