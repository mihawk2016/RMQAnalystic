library(XML)
library(xml2)
library(parallel)
# sapply(file.path('./class', dir('./class')), source)

files <- file.path('.', 'TEST_FILE', dir('TEST_FILE'))[1:10]
# 
# time.old <- system.time({
#   old <- lapply(files, fetch.html.data.old)
# })
# print(time.old)
# 
time.new <- system.time({
  new <- lapply(files, read.mq.file)
})
print(time.new)
# 
cl <- makeCluster(detectCores())
# # clusterExport(cl, c('files', 'mq.file.name', 'fetch.file.data', 'fetch.html.data', 'fetch.html.data.infos.mt4m_closed',
# #                     'build.infos', ls()))
# clusterCall(cl, function() {
#   library(xml2)
#   library(TEST)
#   NULL
# })
# 
time.new.parallel <- system.time({

  # environment(read.mq.file) <- .GlobalEnv
  new.parallel <- p.read.mq.file(cl, files)
})
# stopCluster(cl)
print(time.new.parallel)




# print(old)
# print(new)
# print(new.parallel)
# print(read.mq.file(files))
