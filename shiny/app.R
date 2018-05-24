options(shiny.maxRequestSize=2000*1024^2)
library(RSQLite)
library(DT)
library(plotly)
library(xcms)
library(XML)
library(tools)
library(shinyjs)
library(shinyBS)
library(pracma)
library(stringr)
library(shinyWidgets)
library(shinyFiles)
library(openxlsx)
library(shinydashboard)
library(htmlwidgets)

ui <- source('ui.R', local=TRUE)$value

server <- source('server.R', local=TRUE)$value

shinyApp(ui=ui, server=server)