
library(R6)
library(RMitekeLab)

MQ_ANALYSTIC <- R6Class(
  classname = 'MetaQuote Analystic',
  public = list(
    initialize = function() {
      # private$cluster <- makeCluster(detectCores() - 1)
    },
    read.files = function(files, cluster=FALSE) {
      # cluster <- makeCluster(detectCores() - 1)
      read.mq.file(files, cluster)
      # stopCluster(cluster)
      # x
    }
  ),
  private = list(
    # cluster = NULL
  )
)