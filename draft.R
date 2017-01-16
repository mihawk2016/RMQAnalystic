


A <- R6Class(
  public = list(
    initialize = function(x) {
      print('A')
    }
  )
)

B <- R6Class(
  inherit = A,
  public = list(
    initialize = function(x) {
      super$initialize()
      print('B')
    }
  )
)