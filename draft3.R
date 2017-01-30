

library(R6)
A <- R6Class(
  public = list(
    d = NULL,
    TEST = function(x) {
      cat(x + 1)
    }
  ),
  private = list(
    TEST.Private = function(x) {
      cat(x + 2)
    }
  )
)

`%::%` <- function(env, fun) {
  aaa <<- x <- env$public_methods
  print(x)
  print(x[[fun]])
  x[[fun]]
}

a <- (A %::% 'TEST')(5)
a <- A$public_methods$TEST(5)
# d <- A$clone_method()
b <- A$private_methods$TEST.Private(5)
