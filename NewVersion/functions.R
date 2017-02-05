## 2017-02-05: Version 0.1

library(compiler)
compilePKGS(T)

read.mq.file <- function(mq.files) {
  # ''' read mq files '''
  # @param mq.files: MetaQuote files.
  # @return:
  # 2017-02-05: Version 0.1
  mq.names <- mq.file.name(mq.files)
  
}

mq.file.name <- function(mq.files) {
  # ''' get mq file names (V) '''
  # @param mq.files: MetaQuote files.
  # @return: names of MetaQuote files.
  # 2017-02-05: Version 0.1
  ifelse(is.data.frame(mq.files), mq.files$name, basename(mq.files))
}

# mq.file.extension <- function(mq.names) {
#   # ''' get mq file extension '''
#   # @param mq.names: MetaQuote file-names.
#   # @return: the extensions of MetaQuote files or file-names.
#   # 2017-02-05: Version 0.1
#   file.path.split <- strsplit(file.path, '.', fixed = T)[[1]]
#   ifelse(length(file.path.split) > 1, tolower(tail(file.path.split, 1)), '')
# } # FINISH

fetch.file.data <- function(mq.file, mq.file.name) {
  # ''' fetch mq file's data (S) '''
  # @param mq.files: MetaQuote files.
  # @param mq.names: MetaQuote file-names.
  # @return: data of MetaQuote files.
  # 2017-02-05: Version 0.1
  if (grepl('.(html|htm)$', mq.file.name)) {
    return(fetch.html.data(mq.file, mq.file.name))
  }
  if (grepl('.(xlsx|xls)$', mq.file.name)) {
    ## ToDo ####
    return(fetch.excel.data(mq.file, mq.file.name))
  }
  if (grepl('.(csv)$', mq.file.name)) {
    ## ToDo ####
    return(fetch.csv.data(mq.file, mq.file.name))
  }
  NULL
}







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
      return(MetaQuote.HTML.MT5EA.Report$new(file.path, file.name, html.parse))
    }
    if (grepl('Trade History Report', html.title)) {
      return(MetaQuote.HTML.MT5Trade.Report$new(file.path, file.name, html.parse))
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


read.files <- 