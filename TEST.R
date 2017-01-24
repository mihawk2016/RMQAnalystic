


rm(list = ls())
library(XML)
lapply(file.path('./class', dir('./class')), source)

files <- file.path('.', 'TEST_FILE', dir('TEST_FILE'))
files

test.number <- 1:10

file <- files[test.number]

# .read.file(file, .file.extension(file))

A <- MQAnalystic$new()

A$add.files(file)

# A$get.unsupported.file()

A$set.selected.index(8:9)


TEST <- A$TESTING()



# REPORT <- A$get.reports(1)




# ANALYZER <- A$set.analyzing.report()




# A$get.reports() -> B

# B[[1]] -> B
# print(B$get.infos.dataframe())

# print(E <- B$init.tickets())
