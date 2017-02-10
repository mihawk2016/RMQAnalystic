# 
# 
# library(R6)
# A <- R6Class(
#   public = list(
#     d = NULL,
#     TEST = function(x) {
#       cat(x + 1)
#     }
#   ),
#   private = list(
#     TEST.Private = function(x) {
#       cat(x + 2)
#     }
#   )
# )
# 
# `%::%` <- function(env, fun) {
#   aaa <<- x <- env$public_methods
#   print(x)
#   print(x[[fun]])
#   x[[fun]]
# }
# 
# a <- (A %::% 'TEST')(5)
# a <- A$public_methods$TEST(5)
# # d <- A$clone_method()
# b <- A$private_methods$TEST.Private(5)


# A <- data.frame(matrix(nrow = 200, ncol = 200))
# 
# B <- A
# rbind.time <- system.time(
#   for (i in 1: 50) {
#     B <- rbind(B, A)
#   }
# )
# print(rbind.time) ## 9.31 Secs
# 
# B <- A
# lapply.time <- system.time(
#   lapply(1:50, function(x) B <<- rbind(B, A))
# )
# print(lapply.time) ## 7.48 Secs
# 
# B <- vector('list', 50) ## 0.84 Secs
# # B <- list() ## 1.40 Secs
# list.time <- system.time(
#   D <- do.call(rbind, lapply(1:50, function(x) B[x] <<- A))
# )
# print(list.time) ## 0.84 Secs
# 
# 
# B <- data.frame(matrix(nrow = 200 * 50, ncol = 200))
# df.time <- system.time(
#   lapply(1:50, function(x) B[(1+(x-1)*200):(200*x), ] <- A)
# )
# print(df.time) ## 1.09 Secs
rm(list = ls())

n <- 100000000
xx <- c(A = 'x', B = 'y')

A1 <- data.table(X = c(rep('A', n), rep('B', n)))
time1 <- system.time({
  res1 <- A1[, X := xx[X], by = X]
})
print(time1)

A2 <- data.table(X = c(rep('A', n), rep('B', n)))
time2 <- system.time({
  res2 <- A2[, X := xx[X[1]], by = X]
})
print(time2)

A3 <- data.table(X = c(rep('A', n), rep('B', n)))
time3 <- system.time({
  res3 <- A3[, X := xx[X]]
})
print(time3)

cat(identical(res1, res2), identical(res1, res3))
