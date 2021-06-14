#' @title Event when switch between chemical and standard
#' 
#' @description 
#' If chemical is choose, will display the choice of the chemical type.
#' If standard is choose, will display the choice of the formula.
#' 
#' @param input$process_results_study string, choice
shiny::observeEvent(input$process_results_study, {
  params <- list(choice = input$process_results_study)
  if (params$choice == "chemical") {
    shinyjs::show("process_results_chemical")
    shinyjs::hide("process_results_standard")
    shinyjs::show("process_results_download")
  }
  else if (params$choice == "standard") {
    shinyjs::hide("process_results_chemical")
    shinyjs::show("process_results_standard")
    shinyjs::hide("process_results_download")
  }
})

#' @title Profile matix table
#'
#' @description
#' Display the profile, intensities or deviation matrix of a sample 
#' Each cell contains the score of the deconvolution according the adduct selected 
#'    and the type of chemical studied
#' A selection of a cell will update the input `process_results_profile_selected` 
#' 		with the number of Carbon & Chlore retrieve in the rownames & colnames of the table
#'
#' @param db sqlite connection
#' @param input$process_results_file integer project_sample ID
#' @param input$process_results_adduct string adduct name
#' @param input$process_results_study string type of study
#' @param input$process_results_chemical_type string type of chemical studied
#' @param input$process_results_selected_matrix string type of matrix selected, 
#'   can be "Scores", "Standardized intensities", "Deviations"
#' @param input$process_results_standard_formula string standard formula
#' 
#' DataTable instance with the profile matrix
output$process_results_profile <- DT::renderDataTable({
	actualize$deconvolution_params # only to force it reloading after deconvolution
  params <- list(
		project_sample = input$process_results_file, 
		adduct = input$process_results_adduct,
		study = input$process_results_study,
		chemical_type = input$process_results_chemical_type,
		selected_matrix = isolate(input$process_results_selected_matrix),
		standard_formula = input$process_results_standard_formula
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
	}, invalid = function(i) get_profile_matrix(db)
	, error = function(e) {
		print("ERR process_results_table")
		print(e)
		sweet_alert_error(e$message)
		get_profile_matrix(db)
	})
	if(params$study == "chemical"){
	  shinyjs::show("process_results_selected_matrix")
  	mat <- get_profile_matrix(db, params$project_sample, params$adduct, params$chemical_type)
  	session$sendCustomMessage("matrix", jsonlite::toJSON(mat))
  	mat
	}
	else if(params$study == "standard"){
	  shinyjs::hide("process_results_selected_matrix")
	  get_standard_table(db, params$project_sample, params$adduct, params$standard_formula)
	}
    
}, selection = "none", server = FALSE, extensions = 'Scroller', 
class = 'display cell-border compact nowrap', 
options = list(info = FALSE, paging = FALSE, dom = 'Bfrtip', scoller = TRUE, 
scrollX = TRUE, bFilter = FALSE, ordering = FALSE, columnDefs = list(list(
	className = 'dt-body-center', targets = "_all")), 
initComplete = htmlwidgets::JS("
	function (settings, json) {
	  Shiny.onInputChange('process_results_profile_selected', null);
    var table = settings.oInstance.api();
    var button = $('#process_results_selected_matrix .active').text(); 
    var selected_button = button.includes('Scores') ? 0 : button.includes('Standardized intensities') ? 1 : 2;
    table.cells().every(function() {
      if(this.index().column == 0) {
        this.data(this.data());
      }
      else if(this.data() == 'NA/NA/NA'){
        this.data('')
      }
      else if (this.data() != null){
        var splitted_cell = this.data().split('/');
        this.data(splitted_cell[selected_button]);
      }
    });
    table.columns.adjust()
  }
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
	$('#process_results_selected_matrix').on('click', 'div button', function(){
	  if ($(this).hasClass('active')) return(null);
	  $('#process_results_selected_matrix button.active').removeClass('active');
	  $(this).addClass('active');
    var selected_button = $(this).text().includes('Scores') ? 0 : $(this).text().includes('Standardized intensities') ? 1 : 2;
    var table = $('#process_results_profile').data('datatable');
    table.cells().every(function() {
      var row = this.index().row
      var col = this.index().column - 1
      if(this.index().column == 0) {
        this.data(this.data());
      }
      else if(old_matrix[row][col] == 'NA/NA/NA'){
        this.data('')
      }
      else if (old_matrix[row][col] != null){
        var splitted_cell = old_matrix[row][col].split('/');
        this.data(splitted_cell[selected_button]);
      }
    });
    table.columns.adjust()
    Shiny.setInputValue('process_results_profile', table.data());
	});
"))

#' @title Event when a cell is selected
#' 
#' @description
#' Event when a cell is selected, it only serve to print what happened in the trace log
#'
#' @param input$process_results_file integer project_sample ID
#' @param input$process_results_adduct string adduct name
#' @param input$process_chemical_type string type of chemical studied
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
		chemical_type = input$process_chemical_type,
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

#' @title EIC plot for a chemical
#'
#' @description
#' Plot all isotopologue traces for a chemical according the cell selected
#' It trace the raw data with area colored where the deconvolution process integrate something
#'
#' @param input$process_results_file integer project_sample ID
#' @param input$process_results_adduct string adduct name
#' @param input$process_results_chemical_type string type of chemical studied
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
		chemical_type = input$process_results_chemical_type, 
		C = as.numeric(input$process_results_profile_selected$C), 
		Cl = as.numeric(input$process_results_profile_selected$Cl)
	)
	# retrieve the parameters used for the deconvolution to trace EICs with same parameters
	# same reasoning for the resolution parameter to simulate isotopic pattern
	deconvolution_param <- as.list(deconvolution_params()[which(
		deconvolution_params()$project == params$project & 
		deconvolution_params()$adduct == params$adduct &
		deconvolution_params()$chemical_type == params$chemical_type), ])
	plot_chemical_EIC(db, params$project_sample, params$adduct, 
		params$chemical_type, params$C, params$Cl, deconvolution_param$ppm, 
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

#' @title MS plot for a chemical
#'
#' @description
#' Plot a MS in mirror mode: above the observed corresponding of the chemical integrated
#' 		below the theoretical isotopic pattern
#'
#' @param input$process_results_file integer project_sample ID
#' @param input$process_results_adduct string adduct name
#' @param input$process_results_chemical_type string type of chemical studied
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
		chemical_type = input$process_results_chemical_type,
		C = as.numeric(input$process_results_profile_selected$C), 
		Cl = as.numeric(input$process_results_profile_selected$Cl)
	)
	# retrieve the resolution parameter to simulate isotopic pattern
	deconvolution_param <- as.list(deconvolution_params()[which(
		deconvolution_params()$project == params$project & 
		deconvolution_params()$adduct == params$adduct & 
		deconvolution_params()$chemical_type == params$chemical_type), ])
	plot_chemical_MS(db, params$project_sample, params$adduct, 
		params$chemical_type, params$C, params$Cl, resolution = list(
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

#' @title Download matrix
#' 
#' @description 
#' Download the selected matrix at the xlsx format
#' 
#' @param input$process_results_file integer project_sample ID
#' @param input$process_results_adduct string adduct name
#' @param input$process_results_chemical_type string type of chemical studied
#' @param input$process_result_selected_matrix string type of matrix selected, 
#'   can be "Scores", "Standardized intensities", "Deviations"
#' 
#' @return xlsx file
output$process_results_download <- shiny::downloadHandler(
  filename = function() { paste("CPSeeker0.1_", input$process_results_selected_matrix, ".xlsx", sep = "") },
  content = function(file) {
    params <- list(
      file = input$process_results_file,
      adduct = input$process_results_adduct,
      chemical_type = input$process_results_chemical_type,
      selected_matrix = input$process_results_selected_matrix)
    matr <- get_profile_matrix(db, params$file, params$adduct, 
      params$chemical_type)
    selected <- if(params$selected_matrix == "Scores") 1 
      else if (params$selected_matrix == "Standardized intensities") 2
      else 3
    for(rows in 1:nrow(matr)){
      for(cols in 1:ncol(matr)){
        cell = matr[rows,cols]
        if(is.na(cell)) next
        splitted_cell = unlist(stringr::str_split(cell, "/"))[selected]
        if(splitted_cell == "NA"){
          matr[rows,cols] = ""
        }
        else{
          matr[rows,cols] = splitted_cell
        }
      }
    }
    first_col <- matrix(dimnames(matr)[[1]])
    matr <- cbind(first_col, matr)
    write.xlsx(matr, file)
})