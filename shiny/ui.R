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

header <- dashboardHeader(
	title = "Target ROI"
)

sidebar <- dashboardSidebar(
	sidebarMenu(id='app',
		menuItem('Files', icon=icon('cog'), tabName='Files'),
		menuItem('TargetFull', icon=icon('list-alt'), tabName = 'TargetFull'),
		menuItem('Target', icon=icon('list-alt'), tabName='Target'),
		menuItem('Correlation', icon=icon('bar-chart-o'), tabName='Correlation'),
		uiOutput('uiSelectProject')
	)
)

body <- dashboardBody(
	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "shinyCSS.css"),
		useShinyjs(),
		tags$script(src = "shinyJS.js"),
		tags$script(src = "addItemsPicker.js"),
		tags$script(src = "shinyWidgets/sweetAlert/js/sweetalert.min.js")
	),
	div(id = "loader"),
	hidden(div(id='app-content',
		tabItems(
			source(file.path("ui", "files.R"), local=TRUE)$value,
			source(file.path("ui", "targetFull.R"), local=TRUE)$value,
			source(file.path("ui", "target.R"), local=TRUE)$value
		)
	)
))
	

shinyUI(
	fillPage(
		dashboardPage(
			header,
			sidebar,
			body
		)
	)
)
