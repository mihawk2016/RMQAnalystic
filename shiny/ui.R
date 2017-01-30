library(shiny)
library(shinydashboard)
library(DT)

#### PAGE { ####

#### + HEADER ####
dashboard.header <- dashboardHeader(title = 'MetaQuote Tools')

#### + SIDEBAR ####
dashboard.sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Analystic', tabName = 'Analystic', icon = icon('institution'))
  )
)

#### + BODY { ####

#### ++ FILE UPLOADER { ####

#### +++ UPLOAD ####
file.uploader.upload <- fileInput(
  inputId = 'file.upload',
  label = NULL,
  multiple = T,
  width = '100%'
)

#### +++ CLEAR ####
file.uploader.clear <- actionButton(
  inputId = 'file.clear',
  label = 'Clear',
  icon = shiny::icon('refresh'),
  width = '100%'
)

## +++ TABLE ####
file.list.support.list <- DT::dataTableOutput(
  outputId = 'file.list.support',
  width = '100%'
)

file.list.unsupport.list <- DT::dataTableOutput(
  outputId = 'file.list.unsupport',
  width = '100%'
)
file.list.support <- tabPanel(
  title = 'SUPPORT',
  file.list.support.list
)

file.list.unsupport <- tabPanel(
  title = 'UNSUPPORT',
  file.list.unsupport.list
)

file.list <- tabBox(
  width = 12,
  file.list.support,
  file.list.unsupport
)
#### ++ FILE UPLOADER } ####

file.uploader <- box(
  collapsible = T,
  status = 'danger',
  solidHeader = TRUE,
  title = 'INPUT',
  width = 12,
  column(
    width = 12,
    file.uploader.upload,
    file.uploader.clear,
    hr()
  ),
  file.list
)


#### + BODY: TABBOX: ANALYSIS ####

tabbox.report.account <- tabPanel(title = 'ACCOUNT')
tabbox.report.symbol <- tabPanel(title = 'SYMBOL')
tabbox.report.tickets <- tabPanel(title = 'TICKETS')

tabbox.report <- box(
  collapsible = T,
  status = 'warning',
  solidHeader = TRUE,
  title = 'ANALYSTIC',
  width = 12,
  tabBox(
    width = 12,
    tabbox.report.account,
    tabbox.report.symbol,
    tabbox.report.tickets
))

#### + BODY } ####

dashboard.body <- dashboardBody(
  # Boxes need to be put in a row (or column)
  file.uploader,
  # file.uploader,
  hr(),
  tabbox.report,
  tabItems(
    tabItem(tabName = "Upload",
            h2("Dashboard tab content")
    ),
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  ),
  fluidRow(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    box(
      title = "Controls",
      sliderInput("slider", "Number of observations:", 1, 100, 50)
    ),
    tabBox(
      side = "right", height = "250px",
      selected = "Tab3",shiny::icon("gear"),
      tabPanel("Tab1", "Tab content 1"),
      tabPanel("Tab2", "Tab content 2"),
      tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
    )
  )
)

#### PAGE } ####

dashboardPage(
  dashboard.header,
  dashboard.sidebar,
  dashboard.body
)



