#' @title Event for creating project
#'
#' @description
#' Event when creating a project
#'
#' @params db sqlite connection
#' @params input$project_name string, project name
#' @params input$project_comment string, project comment, can be NA
#' @param specialChars string, contains a sample of special characters to avoid
shiny::observeEvent(input$project_create, {
	print('############################################################')
	print('######################### SEQUENCE_CREATE ###################')
	print('############################################################')
	params <- list(
		name = gsub('"', "'", input$project_name), 
		comments = gsub('"', "'", input$project_comment))
	print(params)
	
	tryCatch({
	inputs <- c("project_name")
	conditions <- c(params$name != "")
	msgs <- c("A sequence name is required")
	check_inputs(inputs, conditions, msgs)
	
	record_project(db, params$name, params$comments)
  	toastr_success(paste('Sequence', params$name, 'created!'))
	}, invalid = function(i) NULL
	, error = function(e){
		print(e)
		sweet_alert_error("Cannot create sequence", e$message)
	})
	print('############################################################')
	print('######################### END SEQUENCE_CREATE ###############')
	print('############################################################')
})