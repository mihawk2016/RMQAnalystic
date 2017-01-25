TIME <- system.time({
  rm(list = ls())
  
  # Rprof('./prof.out', memory.profiling=T)
  library(XML)
  library(stringr)
  lapply(file.path('./class', dir('./class')), source)
  
  files <- file.path('.', 'TEST_FILE', dir('TEST_FILE'))
  files
  
  test.number <- 1:10
  
  file <- files[test.number]
  
  # .read.file(file, .file.extension(file))
  
  A <- MQAnalystic$new()
  
  A$add.files(file)
  
  # A$get.unsupported.file()
  
  A$set.selected.index(1:10)
  
  
  TEST <- A$TESTING()
})

print(TIME)

## merge 10 files in 60secs


# Rprof()

# REPORT <- A$get.reports(1)




# ANALYZER <- A$set.analyzing.report()




# A$get.reports() -> B

# B[[1]] -> B
# print(B$get.infos.dataframe())

# print(E <- B$init.tickets())
