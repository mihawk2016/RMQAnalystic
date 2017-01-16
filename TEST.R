

# rm(list = ls())

files <- file.path('.', 'TEST_FILE', dir('TEST_FILE'))
files
test.number <- c(2:9, 11)
file <- files[test.number]

# .read.file(file, .file.extension(file))

A <- File.Reader$new(file)

A$get.unsupported.file()

A$get.reports() -> B

print(B)
