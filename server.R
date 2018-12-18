options(show.error.messages = TRUE)
shinyServer(function(input, output, session) {

#to close the connection
session$onSessionEnded(function() {
	stopApp()
	# comment line above because it's still in developement
	# q('no')
})

values <- reactiveValues()

source('server/func.R', local=TRUE)$value

source('server/loadXlsx.R', local=TRUE)$value

source('server/profils.R', local=TRUE)$value

source('server/tetrahedrization.R', local=TRUE)$value

})	