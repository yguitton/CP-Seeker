#' @title Profile matix table
#'
#' @description
#' Display the profile matrix of a sample 
#' Each cell contains the score of the deconvolution according the adduct selected
#' A selection of a cell will update the input `process_results_profile_selected` 
#' 		with the number of Carbon & Chlore retrieve in the rownames & colnames of the table
#'
#' @param db sqlite connection
#' @param input$process_results_file integer project_sample ID
#' @param input$process_results_adduct string adduct name
#' 
#' DataTable instance with the profile matrix
output$process_results_profile <- DT::renderDataTable({
	actualize$deconvolution_params # only to force it reloading after deconvolution
	params <- list(
		project_sample = input$process_results_file, 
		adduct = input$process_results_adduct
	)
	
	tryCatch({
		if (length(params$project_sample) == 0) custom_stop("invalid", "no 
			file selected")
		else if (params$project_sample == "") custom_stop("invalid", "no 
			file selected")
		else if (length(params$adduct) == 0) custom_stop("invalid", "no 
			adduct selected")
		else if (params$adduct == "") custom_stop("invalid", "no 
			adduct selected")
		get_profile_matrix(db, params$project_sample, params$adduct)
	}, invalid = function(i) get_profile_matrix(db)
	, error = function(e) {
		print("ERR process_results_table")
		print(e)
		sweet_alert_error(e$message)
		get_profile_matrix(db)
	})
}, selection = "none", server = FALSE, extensions = 'Scroller', 
class = 'display cell-border compact nowrap', 
options = list(info = FALSE, paging = FALSE, dom = 'Bfrtip', scoller = TRUE, 
scrollX = TRUE, bFilter = FALSE, ordering = FALSE, columnDefs = list(list(
	className = 'dt-body-center', targets = "_all")), 
initComplete = htmlwidgets::JS("
	Shiny.onInputChange('process_results_profile_selected', null);
")), callback = htmlwidgets::JS("
	table.on('click', 'tbody td', function() {
		if ($(this).hasClass('selected')) return(null);
		table.$('td.selected').removeClass('selected');
		$(this).addClass('selected');
		var cell_index = table.cell(this).index(),
			C = $(table.row(cell_index.row).node()).
				children().get(0).textContent.match(/\\d+/g)[0], 
			Cl = $(table.column(cell_index.column).header()).
				get(0).textContent.match(/\\d+/g)[0];
		Shiny.onInputChange('process_results_profile_selected', 
			{C: C, Cl: Cl});
	});
"))

#' @title Event when a cell is selected
#' 
#' @description
#' Event when a cell is selected, it only serve to print what happened in the trace log
#'
#' @param input$process_results_file integer project_sample ID
#' @param input$process_results_adduct string adduct name
#' @param input$process_results_profile_selected vector(integer)[2] contains number of Carbon & Chlore, 
#' 		correspond to the rowname and colname of the cell selected
observeEvent(input$process_results_profile_selected, {
	tryCatch({
	if (is.null(input$process_results_profile_selected)) custom_stop(
		"invalid", "no cell selected")
	print('######################### PROFILE CELL SELECTED ##########################')
	params <- list(
		project_sample = input$process_results_file, 
		adduct = input$process_results_adduct, 
		C = as.numeric(input$process_results_profile_selected$C), 
		Cl = as.numeric(input$process_results_profile_selected$Cl)
	)
	print(params)
	}, invalid = function(i) NULL
	, error  = function(e) {
		print("ERR process_results_profile_selected")
		print(e)
		sweet_alert_error(e$message)
	})
})

#' @title EIC plot for a chloroparaffin
#'
#' @description
#' Plot all isotopologue traces for a chloroparaffin according the cell selected
#' It trace the raw data with area colored where the deconvolution process integrate something
#'
#' @param input$process_results_file integer project_sample ID
#' @param input$process_results_adduct string adduct name
#' @param input$process_results_profile_selected vector(integer)[2] contains number of Carbon & Chlore, 
#' 		correspond to the rowname and colname of the cell selected
#' 
#' @return plotly object
output$process_results_eic <- plotly::renderPlotly({
	tryCatch({
	if (is.null(input$process_results_profile_selected)) custom_stop(
		"invalid", "no cell selected")
	params <- list(
		project_sample = input$process_results_file, 
		adduct = input$process_results_adduct, 
		C = as.numeric(input$process_results_profile_selected$C), 
		Cl = as.numeric(input$process_results_profile_selected$Cl)
	)
	# retrieve the parameters used for the deconvolution to trace EICs with same parameters
	# same reasoning for the resolution parameter to simulate isotopic pattern
	deconvolution_param <- as.list(deconvolution_params()[which(
		deconvolution_params()$project == params$project & 
		deconvolution_params()$adduct == params$adduct), ])
	plot_chloroparaffin_EIC(db, params$project_sample, params$adduct, 
		params$C, params$Cl, deconvolution_param$ppm, 
		deconvolution_param$mda, resolution = list(
			resolution = deconvolution_param$resolution, 
			mz = deconvolution_param$resolution_mz, 
			index = deconvolution_param$resolution_index))
	}, invalid = function(i) plot_empty_chromato()
	, error = function(e) {
		print("ERR process_results_eic")
		print(e)
		sweet_alert_error(e$message)
		plot_empty_chromato()
	})
})

#' @title MS plot for a chloroparaffin
#'
#' @description
#' Plot a MS in mirror mode: above the observed corresponding of the chloroparaffin integrated
#' 		below the theoretical isotopic pattern
#'
#' @param input$process_results_file integer project_sample ID
#' @param input$process_results_adduct string adduct name
#' @param input$process_results_profile_selected vector(integer)[2] contains number of Carbon & Chlore, 
#' 		correspond to the rowname and colname of the cell selected
#' 
#' @return plotly object
output$process_results_ms <- plotly::renderPlotly({
	tryCatch({
	if (is.null(input$process_results_profile_selected)) custom_stop(
		"invalid", "no cell selected")
	params <- list(
		project_sample = input$process_results_file, 
		adduct = input$process_results_adduct, 
		C = as.numeric(input$process_results_profile_selected$C), 
		Cl = as.numeric(input$process_results_profile_selected$Cl)
	)
	# retrieve the resolution parameter to simulate isotopic pattern
	deconvolution_param <- as.list(deconvolution_params()[which(
		deconvolution_params()$project == params$project & 
		deconvolution_params()$adduct == params$adduct), ])
	plot_chloroparaffin_MS(db, params$project_sample, params$adduct, 
		params$C, params$Cl, resolution = list(
			resolution = deconvolution_param$resolution, 
			mz = deconvolution_param$resolution_mz, 
			index = deconvolution_param$resolution_index))
	}, invalid = function(i) plot_empty_MS()
	, error = function(e) {
		print("ERR process_results_eic")
		print(e)
		sweet_alert_error(e$message)
		plot_empty_MS()
	})
})