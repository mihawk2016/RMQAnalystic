
rm(list = ls())
library(XML)

sapply(file.path('./class', dir('./class')), source)
files <- file.path('.', 'TEST_FILE', dir('TEST_FILE'))[1:10]

A <- MQAnalystic$new()
time <- system.time({
  A$add.files(files)
})
print(time)
## 8.37 Secs


# TIME <- system.time({
#   # rm(list = ls())
#   
#   
# 
#   
#   # Rprof('./prof.out', memory.profiling=T)
#   library(quantmod)
#   library(XML)
#   library(stringr)
#   
#   # sapply(file.path('./class', dir('./class')), source)
#   
#   # files <- file.path('.', 'TEST_FILE', dir('TEST_FILE'))
#   # files
#   
#   test.number <- 1:10
#   
#   file <- files[test.number]
#   
#   # .read.file(file, .file.extension(file))
#   
#   # ## parallel
#   # library(parallel)
#   # cl <- makeCluster(detectCores())
#   # clusterSetRNGStream(cl)
#   # clusterEvalQ(cl, {
#   #   library(XML)
#   # })
#   # ##
#   A <- MQAnalystic$new()
#   
#   A$add.files(file)
#   
#   # A$get.unsupported.file()
#   
#   # A$set.selected.index(1:10)
#   
#   # TEST <- A$TESTING()
# })
# 
# print(TIME)

## merge 10 files in 60secs

## 纯读取infos，10稳健大约6秒 ##5.86 Secs
# Rprof()

# REPORT <- A$get.reports(1)




# ANALYZER <- A$set.analyzing.report()




# A$get.reports() -> B

# B[[1]] -> B
# print(B$get.infos.dataframe())

# print(E <- B$init.tickets())
