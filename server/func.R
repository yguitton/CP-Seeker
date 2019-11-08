`%!in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

#conversion factor to numeric directly
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

# read in sql-statements and preformat them                                        
dbDisconnect <- function(db){
	suppressWarnings(RSQLite::dbDisconnect(db))
}
dbGetQuery <- function(db, query){
	if(length(query) > 1) stop('multiple request not authorized')
	suppressWarnings(RSQLite::dbGetQuery(db, query))
}

dbExecute <- function(db, query, ...){
	if(length(query) > 1) stop('multiple request not authorized')
	msg <- "database is locked"
	while(msg == "database is locked"){
		msg <- tryCatch({
			suppressWarnings(RSQLite::dbExecute(db, query, ...))
			"success"
		}, error = function(e) e$message)
	}
	if(msg != "success") stop(msg)
}

toastr_error <- function(title = "", msg = ""){
	shinytoastr::toastr_error(msg, title, closeButton=TRUE, newestOnTop=TRUE, position='top-center', preventDuplicates=TRUE)
}

toastr_success <- function(title = "", msg = ""){
	shinytoastr::toastr_success(msg, title, closeButton=TRUE, newestOnTop=TRUE, position='top-center', preventDuplicates=TRUE)
}

toastr_warning <- function(title="", msg=""){
	shinytoastr::toastr_warning(msg, title, closeButton=TRUE, newestOnTop=TRUE, position='top-center', preventDuplicates=TRUE)
}

sendSweetAlert <- function(title="", msg=""){
	shinyWidgets::sendSweetAlert(session, html = TRUE, title = title, 
		text = tags$div(msg, tags$br(), 
			tags$a(href = sprintf(
				'mailto:sebastien.hutinet@oniris-nantes.fr?subject=Describe header of error&body=don\'t forget to attach error log (in "%s") 
				and/or database (in "%s") if needed', normalizePath('~/../.CPSeeker/error.log'), normalizePath('database.sqlite')), 
				"Send a mail to the developper")), type='error')
}

# check if the inputs respect the conditions (TRUE it not respect, FALSE it respect)
# if not it use shinyFeedback to hightlight the input and send a message
inputsTest <- function(inputs = c(), conditions = c(), titles = c(), messages = c()){
	nbInputs <- length(inputs)
	if(any(length(conditions) != length(inputs) | 
		length(titles) != length(inputs) | 
		length(messages) != length(inputs))) return(TRUE)
	for(i in 1:length(inputs)){
		feedbackDanger(inputs[i], conditions[i], messages[i])
		if(conditions[i]){
			print(paste('INVALID: ', titles[i], ':', messages[i]))
			toastr_error(titles[i], messages[i])
		}
	}
	return(!any(conditions))
}

condition <- function(subclass, message, call=sys.call(-1), ...){
	structure(
		class=c(subclass, "condition"),
		list(message=message, call=call),
		...
	)
}

custom_stop <- function(subclass, message, call=sys.call(-1), ...){
	c <- condition(c(subclass, "error"), message, call=call, ...)
	stop(c)
}