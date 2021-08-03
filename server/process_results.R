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
    shinyjs::show("process_results_adduct")
    shinyjs::hide("process_results_adduct2")
    shinyjs::show("process_results_download")
  }
  else if (params$choice == "standard") {
    shinyjs::hide("process_results_chemical")
    shinyjs::show("process_results_standard")
    shinyjs::hide("process_results_adduct")
    shinyjs::show("process_results_adduct2")
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
#' @param input$process_results_chemical_adduct string adduct name for chemical
#' @param input$process_results_standard_adduct string adduct name for standard
#' @param input$process_results_study string type of study
#' @param input$process_results_chemical_type string type of chemical studied
#' @param input$process_results_selected_matrix string type of matrix selected, 
#'   can be "Scores", "Normalized intensities", "Deviations"
#' @param input$process_results_standard_formula string standard formula
#' 
#' DataTable instance with the profile matrix
output$process_results_profile <- DT::renderDataTable({
	actualize$deconvolution_params # only to force it reloading after deconvolution
  actualize$results_matrix
  params <- list(
    project = input$project,
		project_sample = isolate(input$process_results_file), 
		chemical_adduct = isolate(input$process_results_chemical_adduct),
		standard_adduct = isolate(input$process_results_standard_adduct),
		study = isolate(input$process_results_study),
		chemical_type = isolate(input$process_results_chemical_type),
		selected_matrix = isolate(input$process_results_selected_matrix),
		standard_formula = isolate(input$process_results_standard_formula)
	)
	
	tryCatch({
		if (length(params$project_sample) == 0) custom_stop("invalid", "no 
			file selected")
		else if (params$project_sample == "") custom_stop("invalid", "no 
			file selected")
		else if (length(params$chemical_adduct) == 0) custom_stop("invalid", "no 
			adduct selected")
		else if (params$chemical_adduct == "") custom_stop("invalid", "no 
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
	  samples <- get_samples(db, params$project)
	  mat_params <- list(
	    sample_id = samples$sample_id,
	    project_sample = samples$project_sample,
	    chemicals = c('CPs', 'COs', 'CdiOs'),
	    adducts = c('M-H', 'M+Cl', 'M+Hac-H')
	  )
	  mat <- list()
	  for(i in 1:length(mat_params$sample_id)){
	    mat2 <- sapply(mat_params$sample_id[i], function(project){
  	    sapply(mat_params$chemicals, function(chemical){
  	      sapply(mat_params$adducts, function(adduct){
  	        get_profile_matrix(db, mat_params$project_sample[i], adduct, chemical)
  	      }, simplify = FALSE, USE.NAMES = TRUE)
  	    }, simplify = FALSE, USE.NAMES = TRUE)
  	  }, simplify = FALSE, USE.NAMES = TRUE)
	    mat <- append(mat, mat2)
	  }
	  file <- mat_params$sample_id[which(mat_params$project_sample == params$project_sample)]
	  if(length(file) == 0) file <- mat_params$sample_id[1]
	  session$sendCustomMessage("matrix", jsonlite::toJSON(mat))
	  mat[[file]][[params$chemical_type]][[params$chemical_adduct]]
	}
	else if(params$study == "standard"){
	  shinyjs::hide("process_results_selected_matrix")
	  get_standard_table(db, params$project_sample, params$standard_adduct, params$standard_formula)
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
    var selected_button = button.includes('Scores') ? 0 : button.includes('Normalized intensities') ? 1 : 2;
    table.cells().every(function() {
      if(this.index().column == 0) {
        this.data(this.data());
      }
      else if (this.data() == null){
        $(this.node()).addClass('outside');
      }
      else if (this.data() != null){
        var splitted_cell = this.data().split('/');
        if(splitted_cell[selected_button] == 'NA'){
          this.data('')
        }
        else{
          if(splitted_cell[0] < parseInt(process_results_score_min.value) | splitted_cell[0] > parseInt(process_results_score_max.value)){
            this.data('')
          }
          else{
            this.data(splitted_cell[selected_button]);
          }
        }
        if(splitted_cell[3] == 'outside'){
          $(this.node()).addClass('outside');
        }
        else if(splitted_cell[3] == 'half'){
          $(this.node()).addClass('half');
        }
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
	$('#process_results_matrix').on('click', function(){
	  var study = $('#process_results_study').text();
    if(study == 'standard'){
      debugger;
      var table = standard_table
      Shiny.setInputValue('process_results_profile', table.data());
    }
    else{
  	  var project = $('#process_results_file').text();
    	var chemical = $('#process_results_chemical_type').text();
    	var adduct = $('#process_results_chemical_adduct').text();
    	var table = $('#process_results_profile').data('datatable');
    	var old_table = old_matrix[project][chemical][adduct];
    	var button = $('#process_results_selected_matrix .active').text(); 
      var selected_button = button.includes('Scores') ? 0 : button.includes('Normalized intensities') ? 1 : 2;
      table.cells().every(function() {
        var row = this.index().row
        var col = this.index().column - 1
        if(this.index().column == 0) {
          this.data(this.data());
        }
        else if (old_table[row][col] != null){
          var splitted_cell = old_table[row][col].split('/');
          if(splitted_cell[selected_button] == 'NA'){
            this.data('')
          }
          else{
           if(splitted_cell[0] < parseInt(process_results_score_min.value) | splitted_cell[0] > parseInt(process_results_score_max.value)){
              this.data('');
            }
            else{
              this.data(splitted_cell[selected_button]);
            }
          }
          if(splitted_cell[3] == 'outside'){
            $(this.node()).addClass('outside');
          }
          else if(splitted_cell[3] == 'half'){
            $(this.node()).addClass('half');
          }
        }
          
      });
      Shiny.setInputValue('process_results_profile', table.data());
    }
	});
	$('#process_results_selected_matrix').on('click', 'div button', function(){
	  if ($(this).hasClass('active')) return(null);
	  $('#process_results_selected_matrix button.active').removeClass('active');
	  $(this).addClass('active');
	  var project = $('#process_results_file').text();
  	var chemical = $('#process_results_chemical_type').text();
  	var adduct = $('#process_results_chemical_adduct').text();
  	var old_table = old_matrix[project][chemical][adduct]; 
    var selected_button = $(this).text().includes('Scores') ? 0 : $(this).text().includes('Normalized intensities') ? 1 : 2;
    var table = $('#process_results_profile').data('datatable');
    table.cells().every(function() {
      var row = this.index().row
      var col = this.index().column - 1
      if(this.index().column == 0) {
        this.data(this.data());
      }
      else if (old_table[row][col] != null){
        var splitted_cell = old_table[row][col].split('/');
        if(splitted_cell[selected_button] == 'NA'){
          this.data('')
        }
        else{
          if(splitted_cell[0] < parseInt(process_results_score_min.value) | splitted_cell[0] > parseInt(process_results_score_max.value)){
            this.data('');
          }
          else{
            this.data(splitted_cell[selected_button]);
          }
        }
      }
    });
    table.columns.adjust()
    Shiny.setInputValue('process_results_profile', table.data());
	});
	$('#process_results_apply').on('click', function(){
		var project = $('#process_results_file').text();
  	var chemical = $('#process_results_chemical_type').text();
  	var adduct = $('#process_results_chemical_adduct').text();
  	var old_table = old_matrix[project][chemical][adduct]; 
  	var table = $('#process_results_profile').data('datatable');
  	var mat = $('#process_results_selected_matrix button.active').text()
  	var selected_button = mat.includes('Scores') ? 0 : mat.includes('Normalized intensities') ? 1 : 2;
	  table.cells().every(function() {
	    var row = this.index().row
      var col = this.index().column - 1
      if(this.index().column == 0) {
        this.data(this.data());
      }
      if(old_table[row][col] != null){
        var splitted_cell = old_table[row][col].split('/');
        if(splitted_cell[0] < parseInt(process_results_score_min.value) | splitted_cell[0] > parseInt(process_results_score_max.value)){
          this.data('');
        }
        else if(splitted_cell[selected_button] != 'NA'){
          this.data(splitted_cell[selected_button])
        }
      }
    });
    table.columns.adjust()
    Shiny.setInputValue('process_results_profile', table.data());
	});
"))

#' @title Update profile matrix
#' 
#' @description 
#' When process_results_matrix is clicked, will update profile matrix according to parameters
observeEvent(input$process_results_matrix, {
  if(input$process_results_study == "standard") actualize$results_matrix <<- runif(1)
})

#' @title Event when a cell is selected
#' 
#' @description
#' Event when a cell is selected, it only serve to print what happened in the trace log
#'
#' @param input$process_results_file integer project_sample ID
#' @param input$process_results_chemical_adduct string adduct name
#' @param input$process_results_chemical_type string type of chemical studied
#' @param input$process_results_profile_selected vector(integer)[2] contains number of Carbon & Chlore, 
#' 		correspond to the rowname and colname of the cell selected
observeEvent(input$process_results_profile_selected, {
	tryCatch({
	if (is.null(input$process_results_profile_selected)) custom_stop(
		"invalid", "no cell selected")
	print('######################### PROFILE CELL SELECTED ##########################')
	params <- list(
		project_sample = input$process_results_file, 
		adduct = input$process_results_chemical_adduct, 
		chemical_type = input$process_results_chemical_type,
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
#' @param input$process_results_chemical_adduct string adduct name
#' @param input$process_results_chemical_type string type of chemical studied
#' @param input$process_results_profile_selected vector(integer)[2] contains number of Carbon & Chlore, 
#' 		correspond to the rowname and colname of the cell selected
#' 
#' @return plotly object
output$process_results_eic <- plotly::renderPlotly({
  actualize$results_eic
	tryCatch({
	if (is.null(input$process_results_profile_selected)) custom_stop(
		"invalid", "no cell selected")
	params <- list(
		project_sample = isolate(input$process_results_file),
		adduct = isolate(input$process_results_chemical_adduct), 
		chemical_type = isolate(input$process_results_chemical_type), 
		C = as.numeric(input$process_results_profile_selected$C), 
		Cl = as.numeric(input$process_results_profile_selected$Cl)
	)
	# retrieve the parameters used for the deconvolution to trace EICs with same parameters
	# same reasoning for the resolution parameter to simulate isotopic pattern
	deconvolution_param <- as.list(deconvolution_params()[which(
		deconvolution_params()$project == params$project & 
		deconvolution_params()$adduct == params$adduct &
		deconvolution_params()$chemical_type == params$chemical_type), ])
	p <- plot_chemical_EIC(db, params$project_sample, params$adduct, 
		params$chemical_type, params$C, params$Cl, deconvolution_param$ppm, 
		deconvolution_param$mda, resolution = list(
			resolution = deconvolution_param$resolution, 
			mz = deconvolution_param$resolution_mz, 
			index = deconvolution_param$resolution_index), 
	  retention_time = c(deconvolution_param$retention_time_min, 
	    deconvolution_param$retention_time_max))
	htmlwidgets::onRender(p, 
  	"function(el, x) {
      el.on('plotly_selected', function(d) {
        var min = d.range.x[0]
        var max = d.range.x[1]
        Shiny.setInputValue('rtmin', min);
        Shiny.setInputValue('rtmax', max);
      })
    }"
  )
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
#' @param input$process_results_chemical_adduct string adduct name
#' @param input$process_results_chemical_type string type of chemical studied
#' @param input$process_results_profile_selected vector(integer)[2] contains number of Carbon & Chlore, 
#' 		correspond to the rowname and colname of the cell selected
#' 
#' @return plotly object
output$process_results_ms <- plotly::renderPlotly({
  actualize$results_ms
	tryCatch({
	if (is.null(input$process_results_profile_selected)) custom_stop(
		"invalid", "no cell selected")
	params <- list(
		project_sample = isolate(input$process_results_file),
		adduct = isolate(input$process_results_chemical_adduct), 
		chemical_type = isolate(input$process_results_chemical_type),
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
#' @param input$process_results_chemical_adduct string adduct name
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
      adduct = input$process_results_chemical_adduct,
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
    colnames(matr)[1] <- " "
    write.xlsx(matr, file)
})

#' @title Launch reintegration
#' 
#' @decription 
#' Launch reintegration of a chloroparaffin
#' 
#' @param db sqlite connection
#' @param input$project integer, project ID
#' @param input$process_results_file string, file studied
#' @param input$process_results_chemical_type string, type of chemical studied
#' @param input$process_results_chemical_adduct string, adduct name
#' @param input$process_results_profile_selected vector(integer)[2] contains number of Carbon & Chlore, 
#' 		correspond to the rowname and colname of the cell selected
shiny::observeEvent(input$process_results_reintegration, {
  print('############################################################')
  print('###################### REINTEGRATION #######################')
  print('############################################################')
  
  params <- list(
    project = input$project,
    project_sample = input$process_results_file,
    chemical_type = input$process_results_chemical_type,
    adduct = input$process_results_chemical_adduct,
    retention_time = c(input$rtmin, input$rtmax),
    C = input$process_results_profile_selected$C,
    Cl = input$process_results_profile_selected$Cl
  )
  deconvolution_params <- get_deconvolution_params(db, params$project, 
    params$chemical_type, params$adduct)
  params2 <- list(
    resolution = list(
      instrument = deconvolution_params$instrument, 
      index = as.numeric(deconvolution_params$resolution_index), 
      resolution = deconvolution_params$resolution, 
      mz = deconvolution_params$resolution_mz
    ), 
    ppm = deconvolution_params$ppm, mda = deconvolution_params$mda, 
    peakwidth = c(deconvolution_params$peakwidth_min, deconvolution_params$peakwidth_max),
    missing_scans = deconvolution_params$missing_scans
  )
  params <- append(params, params2)
  params$sample <- project_samples()[which(
    project_samples()$project == params$project), "sample"]
  
  tryCatch({
    ion_form <- get_chemical_ion(db, params$adduct, params$chemical_type, 
      params$C, params$Cl)
    if (nrow(ion_form) == 0) custom_stop("minor_error", "no chemical founded 
      with this adduct")
    theoric_pattern <- get_theoric(ion_form$ion_formula, 
      ion_form$charge, params$resolution)
    theoric_pattern <- lapply(theoric_pattern, function(x) 
      cbind(x, get_mass_range(x[, "mz"], deconvolution_params$ppm, deconvolution_params$mda)))
    ms_file <- load_ms_file(db, params$sample)
    scalerange <- round((params$peakwidth  / mean(diff(ms_file@scantime))) /2)
    peak <- deconvolution(ms_file, theoric_pattern, ion_form$chemical_ion, c(1, scalerange[2]), 
      params$retention_time, params$missing_scans, pb = "pb2", reintegration = TRUE)
    if(is.null(peak)) custom_stop("minor_error", "no chemical founded 
      with this adduct")
    if(peak$score[1] < 0) custom_stop("minor_error", "no chemical founded 
      with this adduct")
    peak$project_sample <- params$project
    query <- sprintf("delete from feature where chemical_ion == %s", peak$chemical_ion[1])
    db_execute(db, query)
    record_features(db, peak)
    val <- paste(round(peak$score[1]), 
      round(peak$intensities[1]/10**6, digits = 1), 
      round(peak$weighted_deviations[1]*10**4, digits = 1), sep = "/")
    session$sendCustomMessage("values", jsonlite::toJSON(val))
    shinyjs::runjs("
      var values = new_values[0];
      var table = $('#process_results_profile').data('datatable');
      var cell_index = table.cell(table.$('td.selected')).index();
      var C = cell_index.row;
      var Cl = cell_index.column;
      old_matrix[C][Cl-1] = values + '/' + old_matrix[C][Cl-1].split('/')[3]; 
      var mat = $('#process_results_selected_matrix button.active').text();
    	var selected_button = mat.includes('Scores') ? 0 : mat.includes('Normalized intensities') ? 1 : 2;
    	var splitted_cell = values.split('/');
    	table.cell(C, Cl).data(splitted_cell[selected_button]);
    ")
    actualize$results_eic <<- runif(1)
    actualize$results_ms <<- runif(1)
  }, invalid = function(i) NULL
  , minor_error = function(e) {
    print(e)
    shinyWidgets::closeSweetAlert(session)
    toastr_error(e$message)
  }, error = function(e) {
    print(e)
    shinyWidgets::closeSweetAlert(session)
    sweet_alert_error(e$message)
  })
  
  print('############################################################')
  print('#################### END REINTEGRATION #####################')
  print('############################################################')
})