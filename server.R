shiny::shinyServer(function(input, output, session) {

#' @title Clean the RAM
#' 
#' @description
#' Call garbage collector every 10 sec
shiny::observe({
	shiny::invalidateLater(10000, session)
	gc()
})

#' @title Event when session is ended
#'
#' @description
#' At session end it remove all object in environnement & call garbage collector
session$onSessionEnded(function() {
        print('clean db')
		clean_samples(db)
		clean_params(db)
		export_lighted_database(db, sqlite_lighted_path)
		db_execute(db, 'pragma wal_checkpoint(truncate);')
	shiny::stopApp()
})

#' @title Print in console in which user swicth to tab
#'
#' @description
#' Print each time the tab which the user switch
#'
#' @params input$tabs string, name of the tab associated (see file ui.R)
shiny::observeEvent(input$tabs, {
	gc()
	print('                                                            ')
	print('############################################################')
	print(
		stringr::str_trunc(
			paste0('TAB ######################### ', input$tabs, ' ###########################'),
			60
		)
	)
	print('############################################################')
})

#' @title Update config file
#'
#' @description
#' write in config file each time the user or selected project change
#'
#' @param input$user string, user ID
#' @param input$project integer, project ID selected
#' @param configFile string, path to the config file
shiny::observeEvent(c(input$user, input$project), {
	last_user <<- input$user
	last_project <<- input$project
	writeLines(paste(last_user, last_project, sep = "\n"), 
		con = config_file)
})

# Open the documentation file when the 'open_doc' input is triggered
shiny::observeEvent(input$open_doc, {
	system(sprintf("\"%s\" \"%s\"", chromium, documentation_file), wait = FALSE)
})

source(file.path('server', 'func.R'), local = TRUE)$value
source(file.path('server', 'reactiveValues.R'), local = TRUE)$value
source(file.path('server', 'db_func.R'), local = TRUE)$value
source(file.path('server', 'db_delete.R'), local = TRUE)$value
source(file.path('server', 'db_record.R'), local = TRUE)$value
source(file.path('server', 'db_get.R'), local = TRUE)$value
source(file.path('server', 'chem_func.R'), local = TRUE)$value
source(file.path('server', 'plot.R'), local = TRUE)$value
source(file.path('server', 'user.R'), local = TRUE)$value	
source(file.path('server', 'project.R'), local = TRUE)$value
source(file.path('server', 'file.R'), local = TRUE)$value
source(file.path('server', 'manage.R'), local = TRUE)$value
source(file.path('server', 'process.R'), local = TRUE)$value
source(file.path('server', 'deconvolution.R'), local = TRUE)$value
source(file.path('server', 'EIC.R'), local=TRUE)$value
source(file.path('server', 'process_results.R'), local = TRUE)$value
source(file.path('server', 'graphics.R'), local = TRUE)$value
source(file.path('server', 'regression.R'), local = TRUE)$value
source(file.path('server', 'export_function.R'), local = TRUE)$value

# hide loader & show app div
shinyjs::hide(id='loader', anim=TRUE, animType='fade')
shinyjs::show("app-content")

})
