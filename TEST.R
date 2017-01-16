

# rm(list = ls())

files <- file.path('.', 'TEST_FILE', dir('TEST_FILE'))
files
# test.number <- c(2:9, 11)
test.number <- 12
file <- files[test.number]

# .read.file(file, .file.extension(file))

A <- File.Reader$new(file)

A$get.unsupported.file()

A$get.reports() -> B

B[[1]] -> B
print(B$get.infos.dataframe())
