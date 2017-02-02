library(shiny)
library(quantmod)
library(XML)
library(stringr)
options(shiny.maxRequestSize=15*1024^2)

sapply(file.path('./class', dir('./class')), source)
analystic <- MQAnalystic$new()

#### SHINY-SERVER >> ####

#### + SHINY-SERVER >> INPUT ####
shiny.input <- function(input, output) {
  analystic$add.files(input$input.upload)
  un.files <- analystic$get.unsupported.files()
  if (!is.null(un.files)) {
    output$input.unsupport.table <- renderDataTable({
      datatable(as.matrix(analystic$get.unsupported.files()), selection = 'none', colnames = NULL)
    })
  }
  all.infos <- analystic$get.all.Reports.infos()
  if (!is.null(all.infos)) {
    output$input.support.table <- renderDataTable({
      datatable(all.infos, selection = 'multiple')
    })
  }
}

#### + SHINY-SERVER >> CLEAR ####
shiny.clear <- function(input, output) {
  analystic$clear.files()
  output$input.unsupport.table <- renderDataTable({
    NULL
  })
  output$input.support.table <- renderDataTable({
    NULL
  })
}

#### SHINY-SERVER << ####
shinyServer(function(input, output) {
  observeEvent(input$input.upload, shiny.input(input, output))
  observeEvent(input$input.clear, shiny.clear)
  
})
