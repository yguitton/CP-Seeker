#' @title Event for creating user
#'
#' @description 
#' Event observer for creating users
#'
#' @param db sqlite connection
#' @param input$login string, login to record
#' @param users reactive value containing vector of strings (users table)
shiny::observeEvent(input$user_create, {
	print('############################################################')
	print('######################### USER_CREATE ######################')
	print('############################################################')
	params <- list(
		login = gsub('"', "'", input$login)
	)
	print(params)
	
	tryCatch({
	inputs <- c("login", "login")
	conditions <- c(params$login != "", !params$login %in% users())
	msgs <- c("you must select a login", "A user with this login already exists")
	check_inputs(inputs, conditions, msgs)
	
	record_user(db, params$login)
	toastr_success("User created")
	}, invalid = function(i) NULL
	, error = function(e) {
		print(e)
		sweet_alert_error("Cannot create user", e$message)
	})
	print('############################################################')
	print('######################### END USER_CREATE ##################')
	print('############################################################')
})