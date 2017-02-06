
rm(list = ls())

# detach('package:RMitekeLab', unload = TRUE)
# detach('package:parallel', unload = TRUE)
# library(RMitekeLab)
# library(parallel)

source('./NewVersion/functions2.R')

files <- file.path('.', 'TEST_FILE', dir('TEST_FILE'))[2]

# cl <- makeCluster(detectCores())
# 
# time <- system.time({
#   infos <- p.read.mq.file(cl, files)
# })
# print(time) 
# ## 8.56 Secs

print(TT <- read.mq.file(files))
