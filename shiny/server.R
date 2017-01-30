library(shiny)
library(quantmod)
library(XML)
library(stringr)
options(shiny.maxRequestSize=30*1024^2)
# sapply(file.path('./class', dir('./class')), source)
analystic <- MQAnalystic$new()

#### SHINY-SERVER { ####

shiny.file.upload <- function(input, output) {
  analystic$add.files(input$file.upload)
  un.files <- analystic$get.unsupported.files()
  if (!is.null(un.files)) {
    output$file.list.unsupport <- renderDataTable({
      datatable(as.matrix(analystic$get.unsupported.files()), selection = 'none', colnames = NULL)
    })
  }
  all.infos <- analystic$get.all.Reports.infos()
  if (!is.null(all.infos)) {
    output$file.list.support <- renderDataTable({
      datatable(all.infos, selection = 'multiple')
    })
  }
}

#### SHINY-SERVER } ####

shinyServer(function(input, output) {
  observeEvent(input$file.upload, shiny.file.upload(input, output))
  observeEvent(input$file.clear, {
    analystic$clear.files()
    output$file.list.unsupport <- renderDataTable({
      NULL
    })
    output$file.list.support <- renderDataTable({
      NULL
    })
  })
  # output$file.list.unsupport <- renderTable({
  #   analystic$get.unsupported.files()
  # })
  
  # ntext <- eventReactive(input$file.clear, {
  #   restoreInput('file.upload', NULL)
  # })
  # output$file.upload <- ntext
  
})
