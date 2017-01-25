mean1 <- function(x) mean(x)
mean2 <- function(x) sum(x) / length(x)

i <- runif(100000000)
# print(i)

print(system.time(
  a <- mean1(i)
))
print(system.time(
  b <- mean2(i)
))