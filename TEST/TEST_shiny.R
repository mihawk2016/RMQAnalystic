library(shiny)

sapply(file.path('./class', dir('./class')), source)
runApp('./shiny')