

# rm(list = ls())

files <- file.path('.', 'TEST_FILE', dir('TEST_FILE'))
files
# test.number <- c(2:9, 11)
<<<<<<< HEAD
test.number <- 3
=======
test.number <- 9
>>>>>>> 6997a7b4216c8e11aff0aaa65d8b6471c68732dc
file <- files[test.number]

# .read.file(file, .file.extension(file))

A <- MQAnalystic$new()

A$add.files(file)

A$get.unsupported.file()

A$get.reports() -> B

B[[1]] -> B
# print(B$get.infos.dataframe())

print(E <- B$init.tickets())
