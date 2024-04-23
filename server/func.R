#' @title Disconnect db
#'
#' @description
#' Suppress warnings when disconnecting database with RSQLite pkg
#'  so annoying when reading log files....
#'
#' @param db sqlite connection
#'
#' @examples
#' \dontrun{db_disconnect(db)}
db_disconnect <- function(db) {
    suppressWarnings(RSQLite::dbDisconnect(db))
}

#' @title Query database
#'
#' @description
#' Suppress warnings when querying database with RSQLite pkg
#'  so annoying when reading log files....
#'
#' @param db sqlite connection
#' @param query string
#'
#' @examples
#' \dontrun{db_get_query(db, "select * from mtcars;")}
db_get_query <- function(db, query) {
    if (length(query) > 1) stop('multiple request not authorized')
	#print(query)
    suppressWarnings(RSQLite::dbGetQuery(db, query))
}

#' @title Send query to database
#'
#' @description
#' Suppress warnings when sending query to database with RSQLite pkg
#' also repeat process while the database is locked because it so dumb it stopped directly after first try...
#' replace NA entries by null for more convenience
#'
#' @param db sqlite connection
#' @param db query string
#'
#' @examples
#' \dontrun{db_execute(db, "insert into mtcars (id) values (1);")}
db_execute <- function(db, query, ...){
	if (length(query) == 0) stop('error when inserting in database')
	else if (length(query) > 1) stop('multiple request not authorized')
	query <- gsub("\"NA\"", "null", query)
	query <- gsub("NA", "null", query)
	query <- gsub("\"\"", "null", query)
	#print(query)
	msg <- "database is locked"
	while (msg == "database is locked") {
		msg <- tryCatch ({
			suppressWarnings(RSQLite::dbExecute(db, query, ...))
			"success"
		}, error = function(e) e$message)
	}
	if (msg != "success") stop(msg)
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

#' @title Send toastr error msg
#'
#' @description
#' Send predefined toastr error msg
#'
#' @param title string, title of the msg
#' @param msg string, content of the msg
#'
#' @return shinytoastr object to show
#' 
#' @examples
#' \dontrun{toastr_error("Aaaaaaargh", "A zombie bite me...")}
toastr_error <- function(msg = "") shinyFeedback::showToast(
	type = "error", msg, .options = list(
		closeButton = TRUE, newestOnTop = TRUE, progressBar = FALSE, 
		preventDuplicates = TRUE, positionClass = "toast-top-center"))

#' @title Send toastr success msg
#'
#' @description
#' Send predefined toastr success msg
#'
#' @param title string, title of the msg
#' @param msg string, content of the msg
#'
#' @return shinytoastr object to show
#' 
#' @examples
#' \dontrun{toastr_success("yeeeeees", "Just dodged a zombie...")}
toastr_success <- function(msg = "") shinyFeedback::showToast(
	type = "success", msg, .options = list(
		closeButton = TRUE, newestOnTop = TRUE, progressBar = FALSE, 
		preventDuplicates = TRUE, positionClass = "toast-top-center"))

#' @title Send toastr warning msg
#'
#' @description
#' Send predefined toastr warning msg
#'
#' @param title string, title of the msg
#' @param msg string, content of the msg
#'
#' @return shinytoastr object to show
#' 
#' @examples
#' \dontrun{toastr_error("Ohoh", "Is it a zombie ?...")}
toastr_warning <- function(msg = "") shinyFeedback::showToast(
	type = "warning", msg, .options = list(
		closeButton = TRUE, newestOnTop = TRUE, progressBar = FALSE, 
		preventDuplicates = TRUE, positionClass = "toast-top-center"))

#' @title Send sweet alert error msg
#'
#' @description
#' Send predefined sweet alert error msg
#'
#' @param title string, title of the msg
#' @param msg string, content of the msg
#'
#' @return sweet alert object to show
#' 
#' @examples
#' \dontrun{sweet_alert_error("Garrrrrrrr", "Braiiiinn...")}
sweet_alert_error <- function(title = "", msg = "") shinyWidgets::sendSweetAlert(
	session, html = TRUE, title = title, 
	text = shiny::tags$div(msg, shiny::tags$br(), 
		shiny::tags$a(href = sprintf(
			'mailto:sebastien.hutinet@oniris-nantes.fr?subject=Describe header of error&body=don\'t forget to attach error log (in "%s") and database if needed (in "%s")', 
			normalizePath(error_log_path), normalizePath("")), 
			"Contact me"
		)
	), type='error')

#' @title Check inputs
#'
#' @description
#' Check conditon for an input
#' If false call shinyFeedback on input, send a toastr & call an invalid error
#'
#' @param inputs vector of strings, inputs name
#' @param condition vector of booleans, condition to respect for each input
#' @param msg vector of strings, message to display in toastr & shinyFeedback for each error
check_inputs <- function(inputs, conditions, msgs) {
	for (i in 1:length(inputs)) {
		if (!conditions[i]) {
			print(msgs[i])
			show_feedback(inputs[i], msgs[i])
			toastr_error(msgs[i])
		} else hide_feedback(inputs[i])
	}
	if (any(!conditions)) custom_stop("invalid", "invalid inputs")
}

show_feedback <- function(input, msg) if (input != "") shinyFeedback::showFeedbackDanger(input, msg)
hide_feedback <- function(input) if (input != "") shinyFeedback::hideFeedback(input)

# check if the inputs respect the conditions
# if not it use shinyFeedback to hightlight the input and send a message
inputsTest <- function(inputs, conditions, messages){
	if(length(inputs) == 0) return(TRUE)
	for(i in 1:length(inputs)){
		feedbackDanger(inputs[i], conditions[i], messages[i])
		if(conditions[i]){
			print(paste('ERR: ', messages[i]))
			toastr_error(messages[i])
		}
	}
	return(!any(conditions))
}