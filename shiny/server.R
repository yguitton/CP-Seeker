options(shiny.maxRequestSize=2000*1024^2)
library(RSQLite)
library(DT)
library(plotly)
library(xcms)
library(XML)
library(tools)
library(shinyjs)
library(pracma)
library(stringr)
library(shinyWidgets)
library(shinyFiles)
library(openxlsx)
normalizePath('.')
sqlitePath <- "database.db"
converter <- file_path_as_absolute('../pwiz/msconvert.exe')
thermo <- file_path_as_absolute('../pwiz/ThermoRawMetaDump.exe')
dirOutput <- 'mzXMLFiles' 	#where sample files are stored
icon_encoded <- 'm518 386q0 8-5 13t-13 5q-37 0-63-27t-26-63q0-8 5-13t13-5 12 5 5 13q0 23 16 38t38 16q8 0 13 5t5 13z m125-73q0-59-42-101t-101-42-101 42-42 101 42 101 101 42 101-42 42-101z m-572-320h858v71h-858v-71z m643 320q0 89-62 152t-152 62-151-62-63-152 63-151 151-63 152 63 62 151z m-571 358h214v72h-214v-72z m-72-107h858v143h-462l-36-71h-360v-72z m929 143v-714q0-30-21-51t-50-21h-858q-29 0-50 21t-21 51v714q0 30 21 51t50 21h858q29 0 50-21t21-51z'


shinyServer(function(input, output, session) {
#to close the connection
session$onSessionEnded(function() {
stopApp()
})

source(file.path("server", "init.R"), local=TRUE)$value
source(file.path("server", "reactiveValues.R"), local=TRUE)$value

hide(id='loader', anim=TRUE, animType='fade')
shinyjs::show("app-content")
		
source(file.path('server', 'importFile.R'), local=TRUE)$value
source(file.path('server', 'importMzXML.R'), local=TRUE)$value
source(file.path('server', 'importRaw.R'), local=TRUE)$value
source(file.path('server', 'info.R'), local=TRUE)$value
source(file.path('server', 'targetFull.R'), local=TRUE)$value
source(file.path('server', 'recordTarget.R'), local=TRUE)$value
source(file.path('server', 'target.R'), local=TRUE)$value
source(file.path('server', 'ppmTable.R'), local=TRUE)$value
source(file.path('server', 'scoreTable.R'), local=TRUE)$value
source(file.path('server', 'tableInto.R'), local=TRUE)$value

})