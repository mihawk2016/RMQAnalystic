


A <- R6Class(
  public = list(
    initialize = function(x) {
      print(private$m.a)
    }
  ),
  private = list(
    m.a = 'aaa'
  )
)

B <- R6Class(
  inherit = A,
  public = list(
    # initialize = function(x) {
    #   super$initialize()
    #   print('B')
    # }
  ),
  private = list(
    m.a = 'bbb'
  )
)