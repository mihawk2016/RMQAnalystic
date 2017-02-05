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
  all.infos <- subset(analystic$get.all.Reports.infos(), select = -FilePath)
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
  # hide('input.upload')
}

#### + SHINY-SERVER >> OUTPUT.CSV ####
# output.csv <- function(input, output) {
#   
# }

#### SHINY-SERVER << ####
shinyServer(function(input, output) {
  selected.reports <- reactive({
    input$input.support.table_rows_selected
  })
  
  observeEvent(input$input.upload, shiny.input(input, output))
  observeEvent(input$input.clear, shiny.clear(input, output))

  
  output$output.csv.button <- downloadHandler(
    filename = function() {
      paste0('Tickets', '_xx', '.csv')
    },
    content = function(file) {
      analystic$output.tickets(
        index = input$input.support.table_rows_selected,
        groups = input$output.csv.groups,
        columns = input$output.csv.columns,
        filename,
        file = file
      ) ## ToDo: filename ####
    }
  )
  
  output$analystic.tickets.table <- renderDataTable({
    ## ToDo ####
    old.selected <- analystic$get.selected.index()
    selected <- selected.reports()
    if (!identical(old.selected, selected)) {
      if (is.null(selected)) {
        render <- NULL
      } else {
        analystic$set.selected.index(selected)
        report <- analystic$set.analyzing.report()
        supported.Report <- analystic$init.others(report)
        tickets <- supported.Report$get.tickets.member('supported')
        tickets <- format(tickets, nsmall = 2)
        tickets[is.na(tickets)] <- ''
        render <- datatable(tickets, selection = 'single')
      }
    } else {
      render <- datatable(as.matrix(c(1,2,3)), selection = 'single', colnames = NULL)
    }
    render
  })
  
  
})
