# mean1 <- function(x) mean(x)
# mean2 <- function(x) sum(x) / length(x)
# 
# i <- runif(100000000)
# # print(i)
# 
# print(system.time(
#   a <- mean1(i)
# ))
# print(system.time(
#   b <- mean2(i)
# ))
library(compiler)
library(XML)

.file.name <- cmpfun(function(file.path) {
  # ''' get file name '''
  # 2016-08-11: Version 1.0
  tail(strsplit(file.path, '/', fixed = T)[[1]], 1)
})# FINISH

.file.extension <- cmpfun(function(file.path) {
  # ''' get file extension '''
  # 2017-01-13: Version 1.1 add support for none extension file
  # 2016-08-11: Version 1.0
  file.path.split <- strsplit(file.path, '.', fixed = T)[[1]]
  ifelse(length(file.path.split) > 1, tolower(tail(file.path.split, 1)), '')
})# FINISH

.read.file <- cmpfun(function(file.path, file.name, file.extension) {
  # ''' read file '''
  # 2017-01-13: ToDo
  if (grepl('htm|html', file.extension)) {
    html.parse <- htmlParse(file.path, encoding = 'UTF-8')
    html.title <- xmlValue(getNodeSet(html.parse,'//title')[[1]])
    if (grepl('Strategy Tester:', html.title)) {
      return(MetaQuote.HTML.MT4EA.Report$new(file.path, file.name, html.parse))
    }
    if (grepl('Statement:', html.title)) {
      return(MetaQuote.HTML.MT4Trade.Report$new(file.path, file.name, html.parse))
    }
    if (grepl('Strategy Tester Report', html.title)) {
      return(MetaQuote.HTML.MT5EA.Report$new(file.path, file.name))
    }
    if (grepl('Trade History Report', html.title)) {
      return(MetaQuote.HTML.MT5Trade.Report$new(file.path, file.name))
    }
    if (grepl('Closed Trades Report', html.title, file.name)) {
      return(MetaQuote.HTML.MT4M_Closed.Report$new(file.path, file.name))
    }
    if (grepl('Raw Report', html.title, file.name)) {
      return(MetaQuote.HTML.MT4M_Raw.Report$new(file.path, file.name))
    }
    return(NULL)
    # MetaQuote.HTML.Report$new(file.path)
  } else if (grepl('xlsx|xls', file.extension)) {
    return(NULL)
    #### ToDo ####
    # xlsx_table <- read.xlsx(file.path)
    # print(xlsx_table)
    # file.csv_xlsx(file.path, xlsx_table)
  } else if (grepl('csv', file.extension)) {
    return(NULL)
    #### ToDo ####
    # csv_table <- read.csv(file.path, encoding = 'UTF-8')
    # file.csv_xlsx(file.path, csv_table)
  } else {
    NULL
  }
})

files <- file.path('.', 'TEST_FILE', dir('TEST_FILE'))[1:10]
files <- rep(files, 10)
names <- .file.name(files)
extensions <- .file.extension(files)

T1 <- system.time(
  A1 <- lapply(files, htmlParse)
)
print(T1) ## 20.38 Secs

T2 <- system.time({
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, library(XML))
  A2 <- parLapply(cl, files, htmlParse)
  stopCluster(cl)
})
print(T2) ## 22.19 Secs