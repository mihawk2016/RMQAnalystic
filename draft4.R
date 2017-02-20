# 
# 
# file <- './USDCAD_H1_2014.csv'
# 
# 
# # print(system.time({
# #   read.csv(file)
# # }))
# # 
# # print(system.time({
# #   fread(file)
# # }))
# 
# read.data.csv <- function(file.csv) {
#   csv <- fread(file.csv)
#   new.csv <-
#     data.table(
#       ymd_hm(paste(csv$V1, csv$V2), tz = 'GMT'),
#       csv[j = c(3:7)]
#     ) %>%
#     as.xts.data.table %>%
#     set_colnames(c('Open', 'High', 'Low', 'Close', 'Volume'))
# }
# 
# print(aaa <- read.data.csv(file))

# A <- data.table(X = 1:10, Y = LETTERS[1:10])
# A[, Z := NA]
# A[, Z := {print(is.na(Z))}]

# TEST.fun <- function() {
#   a <- environment()
#   print(a)
#   test.inner.fun <- function() {
#     assign('x', 1, envir = a)
#   }
#   print(a$x)
#   test.inner.fun()
#   x
# }
# 
# TEST.fun()


XX <- PRICE

symbols <- names(XX)

serie.columns <- c('PROFIT', 'FLOATING', 'PL.VOLUME', 'VOLUME', 'MAX.FLOATING', 'MIN.FLOATING')
YY <- lapply(symbols, function(symbol) {
  XX[[symbol]] %>%
    extract(j = (serie.columns) := 0)
}) %>%
  set_names(symbols)
