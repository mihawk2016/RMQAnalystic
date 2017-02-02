library(shiny)
library(shinydashboard)
library(DT)

#### PAGE >> ####

#### + HEADER ####
dashboard.header <- dashboardHeader(title = 'MetaQuote Tools')

#### + SIDEBAR ####
dashboard.sidebar <- dashboardSidebar(
  # disable = T,
  sidebarMenu(
    menuItem('Analystic', tabName = 'Analystic', icon = icon('institution'))
  )
)

#### + BODY >> ####

#### ++ BODY >> INPUT >> ####

#### +++ BODY >> INPUT >> UPLOAD ####
input.upload <- fileInput(
  inputId = 'input.upload',
  label = NULL,
  multiple = T,
  width = '100%'
)

#### +++ BODY >> INPUT >> CLEAR ####
input.clear <- actionButton(
  inputId = 'input.clear',
  label = 'CLEAR',
  icon = shiny::icon('refresh'),
  width = '100%'
)

## +++ BODY >> INPUT >> TABLE ####
input.support.table <- DT::dataTableOutput(
  outputId = 'input.support.table',
  width = '100%'
)

input.unsupport.table <- DT::dataTableOutput(
  outputId = 'input.unsupport.table',
  width = '100%'
)
input.support <- tabPanel(
  title = 'SUPPORT',
  input.support.table
)

input.unsupport <- tabPanel(
  title = 'UNSUPPORT',
  input.unsupport.table
)

input.list <- tabBox(
  width = 12,
  input.support,
  input.unsupport
)
#### ++ BODY >> INPUT << ####

input <- box(
  collapsible = T,
  status = 'danger',
  solidHeader = TRUE,
  title = 'INPUT',
  width = 12,
  column(
    width = 12,
    input.upload,
    input.clear
  ),
  input.list
)


#### ++ BODY >> ANALYSIS >> ####

#### +++ BODY >> ANALYSIS >> ACCOUNT ####
analystic.account <- tabPanel(title = 'ACCOUNT')

#### +++ BODY >> ANALYSIS >> SYMBOL ####
analystic.symbol <- tabPanel(title = 'SYMBOL')

#### +++ BODY >> ANALYSIS >> TICKETS ####
analystic.tickets <- tabPanel(title = 'TICKETS')

#### +++ BODY >> ANALYSIS << ####
analystic <- box(
  collapsible = T,
  status = 'warning',
  solidHeader = TRUE,
  title = 'ANALYSTIC',
  width = 12,
  tabBox(
    width = 12,
    analystic.account,
    analystic.symbol,
    analystic.tickets
))

#### ++ BODY >> OUTPUT >> ####

#### +++ BODY >> OUTPUT >> CSV ####
output.csv <- downloadButton(
  outputId = 'output.csv',
  label = 'CSV'
)

output.csv.groups <- checkboxGroupInput(
  inputId = 'output.csv.groups',
  label = 'Choose Ticket Types: ',
  choices = list(
    'MONEY' = 'Money',
    'CLOSED' = 'Closed',
    'OPEN' = 'Open',
    'PENDING' = 'Pending',
    'WORKING' = 'Working'
  ),
  selected = c('Money', 'Closed', 'Open'),
  inline = TRUE
)

output.csv.columns <- checkboxGroupInput(
  inputId = 'output.csv.columns',
  label = 'Choose Extra Columns: ',
  choices = list(
    'COMMENT' = 'COMMENT',
    'GROUP' = 'GROUP'#,
    # 'FILE NAME' = 'FILE'
  ),
  selected =NULL,
  inline = TRUE
)

#### +++ BODY >> OUTPUT >> CSV ####
output.report <- downloadButton(
  outputId = 'output.report',
  label = 'REPORT'
)
#### ++ BODY >> OUTPUT >> ####
output <- box(
  collapsible = T,
  status = 'success',
  solidHeader = TRUE,
  title = 'OUTPUT',
  width = 12,
  
  box(
    width = 6,
    title = 'TICKETS',
    # solidHeader = TRUE,
    background = NULL,
    output.csv.groups,
    output.csv.columns,
    output.csv
  ),
  box(
    width = 6,
    title = 'REPORT',
    # solidHeader = TRUE,
    background = NULL,
    output.report
  )
)

#### + BODY << ####

dashboard.body <- dashboardBody(
  input,
  analystic,
  output
)

#### PAGE << ####

dashboardPage(
  title = 'Miteke Lab',
  # skin = 'yellow',
  dashboard.header,
  dashboard.sidebar,
  dashboard.body
)



