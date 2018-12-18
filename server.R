options(show.error.messages = TRUE)
shinyServer(function(input, output, session) {

#to close the connection
session$onSessionEnded(function() {
	stopApp()
	# comment line above because it's still in developement
	# q('no')
})

source('server/func.R', local=TRUE)$value

})	