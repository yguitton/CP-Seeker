#' @title Event when switch between chemical and standard
#' 
#' @description 
#' If chemical is choose, will display the choice of the chemical type.
#' If standard is choose, will display the choice of the formula.
#' 
#' @param input$process_results_study string, choice
shiny::observeEvent(input$process_results_study, {
  params <- list(choice = input$process_results_study)
  if (params$choice == "Chemical") {
    shinyjs::show("process_results_chemical")
    shinyjs::hide("process_results_standard")
    shinyjs::show("process_results_adduct")
    shinyjs::hide("process_results_adduct2")
	  shinyjs::show("process_results_selected_matrix")
    shinyjs::show("process_results_download")
    shinyjs::show("process_results_score_min")
    shinyjs::show("process_results_score_max")
    shinyjs::show("process_results_apply")
    shinyjs::show("process_results_profile")
    shinyjs::hide("process_results_standard_table")
  }
  else if (params$choice == "Standard") {
    shinyjs::hide("process_results_chemical")
    shinyjs::show("process_results_standard")
    shinyjs::hide("process_results_adduct")
    shinyjs::show("process_results_adduct2")
    shinyjs::hide("process_results_selected_matrix")
    shinyjs::hide("process_results_download")
    shinyjs::hide("process_results_score_min")
    shinyjs::hide("process_results_score_max")
    shinyjs::hide("process_results_apply")
    shinyjs::hide("process_results_profile")
    shinyjs::show("process_results_standard_table")
  }
})

#' @title Profile matrix table
#'
#' @description
#' Display the profile, intensities or deviation matrix of a sample 
#' Each cell contains the score of the deconvolution according the adduct selected 
#'    and the type of chemical studied
#' A selection of a cell will update the input `process_results_profile_selected` 
#' 		with the number of Carbon & Chlore retrieve in the rownames & colnames of the table
#'
#' @param db sqlite connection
#' @param input$project integer project ID
#' @param input$process_results_file integer project_sample ID
#' @param input$process_results_chemical_adduct string adduct name for chemical
#' @param input$process_results_chemical_type string type of chemical studied
#' @param input$process_results_selected_matrix string type of matrix selected, 
#'   can be "Scores", "Normalized intensities", "Deviations"
#' 
#' DataTable instance with the profile matrix
output$process_results_profile <- DT::renderDataTable({
	actualize$deconvolution_params # only to force it reloading after deconvolution
  actualize$results_matrix
  params <- list(
    project = input$project,
		project_sample = isolate(input$process_results_file), 
		chemical_adduct = isolate(input$process_results_chemical_adduct),
		chemical_type = isolate(input$process_results_chemical_type),
		selected_matrix = isolate(input$process_results_selected_matrix)
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
	  else if (length(params$chemical_type) == 0) custom_stop("invalid", "no
	    chemical selected")
	  else if (params$chemical_type == "") custom_stop("invalid", "no
	    chemical selected")
	}, invalid = function(i) get_profile_matrix(db)
	, error = function(e) {
		print("ERR process_results_table")
		print(e)
		sweet_alert_error(e$message)
		get_profile_matrix(db)
	})
  samples <- get_samples(db, params$project)
  
  query <- sprintf('select chemical_type, adduct from deconvolution_param where project == %s and
    chemical_type in (select chemical_type from chemical where chemical_type != "Standard");',
    params$project)
  chemicals <- db_get_query(db, query)

  mat_params <- list(
    sample_id = samples$sample_id,
    project_sample = samples$project_sample
  )
  mat <- list()
  for(i in 1:length(mat_params$sample_id)){
    mat2 <- sapply(mat_params$sample_id[i], function(project){
	    sapply(unique(chemicals$chemical_type), function(chemical){
	      sapply(unique(chemicals$adduct[which(chemicals$chemical_type == chemical)]), function(adduct){
	        get_profile_matrix(db, mat_params$project_sample[i], adduct, chemical)
	      }, simplify = FALSE, USE.NAMES = TRUE)
	    }, simplify = FALSE, USE.NAMES = TRUE)
	  }, simplify = FALSE, USE.NAMES = TRUE)
    mat <- append(mat, mat2)
  }
  session$sendCustomMessage("matrix", jsonlite::toJSON(mat))
  file <- mat_params$sample_id[which(mat_params$project_sample == params$project_sample)]
  if(length(file) == 0) file <- mat_params$sample_id[1]
  mat[[file]][[params$chemical_type]][[params$chemical_adduct]]
}, selection = "none", server = FALSE, extensions = 'Scroller', 
class = 'display cell-border compact nowrap', 
options = list(info = FALSE, paging = FALSE, dom = 'Bfrtip', scoller = TRUE, 
scrollX = TRUE, bFilter = FALSE, ordering = FALSE, columnDefs = list(list(
	className = 'dt-body-center', targets = "_all")), 
initComplete = htmlwidgets::JS("
	function (settings, json) {
	  Shiny.onInputChange('process_results_profile_selected', null);
	  Shiny.onInputChange('process_results_standard_selected', null);
    var table = settings.oInstance.api();
    var button = $('#process_results_selected_matrix .active').text(); 
    var selected_button = button.includes('Score(%)') ? 0 : button.includes('Normalized intensity (xE6)') ? 1 : 2;
    table.cells().every(function() {
      if(this.index().column == 0) {
        this.data(this.data());
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
	  var project = $('#process_results_file').text();
  	var chemical = $('#process_results_chemical_type').text();
  	var adduct = $('#process_results_chemical_adduct').text();
  	var table = $('#process_results_profile').data('datatable');
  	var old_table = old_matrix[project][chemical][adduct];
  	var button = $('#process_results_selected_matrix .active').text(); 
    var selected_button = button.includes('Score(%)') ? 0 : button.includes('Normalized intensity (xE6)') ? 1 : 2;
    table.cells().every(function() {
      var row = this.index().row
      var col = this.index().column - 1
      if(this.index().column == 0) {
        this.data(this.data());
      }
      else if (old_table[row][col] == null){
        this.data('');
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
    Shiny.setInputValue('process_results_profile', old_matrix);
	});
	$('#process_results_selected_matrix').on('click', 'div button', function(){
	  if ($(this).hasClass('active')) return(null);
	  $('#process_results_selected_matrix button.active').removeClass('active');
	  $(this).addClass('active');
	  var project = $('#process_results_file').text();
  	var chemical = $('#process_results_chemical_type').text();
  	var adduct = $('#process_results_chemical_adduct').text();
  	var old_table = old_matrix[project][chemical][adduct]; 
    var selected_button = $(this).text().includes('Score (%)') ? 0 : $(this).text().includes('Normalized intensity (xE6)') ? 1 : 2;
    var table = $('#process_results_profile').data('datatable');
    table.cells().every(function() {
      var row = this.index().row
      var col = this.index().column - 1
      if(this.index().column == 0) {
        this.data(this.data());
      }
      else if (old_table[row][col] == null){
        this.data('');
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
  	var selected_button = mat.includes('Score') ? 0 : mat.includes('Normalized intensity (xE6)') ? 1 : 2;
	  table.cells().every(function() {
	    var row = this.index().row
      var col = this.index().column - 1
      if(this.index().column == 0) {
        this.data(this.data());
      }
      else if (old_table[row][col] == null){
        this.data('');
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

#' @title Standard table
#'
#' @description
#' Display the standard table
#'
#' @param db sqlite connection
#' @param input$project integer project ID
#' 
#' DataTable instance with the standard table
output$process_results_standard_table <- DT::renderDataTable({
  actualize$deconvolution_params # only to force it reloading after deconvolution
  actualize$results_matrix
  params <- list(
    project = input$project
  )
  #print (params$project)
  #error <- print(params$project_sample)
  #sweet_alert_error(error)
  tryCatch({
   if (length(params$project_sample) == 0)  custom_stop ("invalid", "no 
			file selected")
    else 
	if(params$project_sample == "") custom_stop("invalid", "no
      file selected")
  }, invalid = function(i) get_profile_matrix(db)
  , error = function(e) {
	print("ERR process_results_table")
	print(e)
	sweet_alert_error(e$message)
	get_profile_matrix(db)
  })

  samples <- get_samples(db, params$project)
  query <- sprintf('select chemical_type, adduct from deconvolution_param where project == %s and
    chemical_type in (select formula from chemical where chemical_type == "Standard");',
      params$project)
  standard <- db_get_query(db, query)
  table_params <- list(
    standard = unique(standard$chemical_type),
    adduct = unique(standard$adduct)
  )
  table <- get_standard_table(db, params$project, table_params$adduct, table_params$standard)
  session$sendCustomMessage("Standard", jsonlite::toJSON(as.matrix(table)))
  as.matrix(table)
  
}, selection = "none", server = FALSE, extensions = 'Scroller', 
class = 'display cell-border compact nowrap', 
options = list(info = FALSE, paging = FALSE, dom = 'Bfrtip', scoller = TRUE, 
  scrollX = TRUE, bFilter = FALSE, ordering = FALSE, columnDefs = list(list(
    className = 'dt-body-justify', targets = "_all")),
initComplete = htmlwidgets::JS("
  Shiny.onInputChange('process_results_profile_selected', null);
	Shiny.onInputChange('process_results_standard_selected', null);
")), callback = htmlwidgets::JS("
	table.on('click', 'tbody tr', function() {
		if ($(this).hasClass('selected')) return(null);
		table.$('tr.selected').removeClass('selected');
		$(this).addClass('selected');
		var row = table.row(this).index();
		var formula = table.cell(row, 1).data();
		var adduct = table.cell(row, 2).data();
		Shiny.onInputChange('process_results_standard_selected', formula);
		Shiny.onInputChange('process_results_adduct_selected', adduct);
	});
"))

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

#' @title Event when a standard is selected
#' 
#' @description
#' Event when a standard is selected, it only serve to print what happened in the trace log
#'
#' @param input$process_results_file integer project_sample ID
#' @param input$process_results_chemical_adduct string adduct name
#' @param input$process_results_chemical_type string type of chemical studied
#' @param input$process_results_standard_selected vector(integer)[2] contains number of Carbon & Chlore, 
#' 		correspond to the rowname and colname of the cell selected
observeEvent(input$process_results_standard_selected, {
  tryCatch({
  if (is.null(input$process_results_standard_selected)) custom_stop(
    "invalid", "no standard selected")
  print('####################### PROFILE STANDARD SELECTED ########################')
  params <- list(
    project_sample = input$process_results_file, 
    adduct = input$process_results_adduct_selected, 
    chemical_type = input$process_results_chemical_type,
    formula = input$process_results_standard_selected
  )
  print(params)
  }, invalid = function(i) NULL
  , error  = function(e) {
    print("ERR process_results_standard_selected")
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
#' @param input$project integer project ID
#' @param input$process_results_file integer project_sample ID
#' @param input$process_results_study string type of study, chemical or standard
#' @param input$process_results_chemical_adduct string adduct name
#' @param input$process_results_chemical_type string type of chemical studied
#' @param input$process_results_profile_selected vector(integer)[2] contains number of Carbon & Chlore, 
#' 		correspond to the rowname and colname of the cell selected
#' @param input$process_results_standard_seleceted string, formula of standard selected
#' @param input$process_results_adduct_selected string, adduct selected
#' 
#' @return plotly object
output$process_results_eic <- plotly::renderPlotly({
  actualize$results_eic
	tryCatch({
	if (is.null(input$process_results_profile_selected) & 
	  is.null(input$process_results_standard_selected)) custom_stop(
		"invalid", "no cell selected")
	study <- isolate(input$process_results_study)
	params <- list(
	  project = input$project,
		project_sample = isolate(input$process_results_file),
		adduct = if(study == "Chemical") isolate(input$process_results_chemical_adduct) 
	    else input$process_results_adduct_selected, 
		chemical_type = if(study == "Chemical") isolate(input$process_results_chemical_type) 
		  else study, 
		C = as.numeric(input$process_results_profile_selected$C), 
		Cl = as.numeric(input$process_results_profile_selected$Cl),
		formula = input$process_results_standard_selected
	)
	# retrieve the parameters used for the deconvolution to trace EICs with same parameters
	# same reasoning for the resolution parameter to simulate isotopic pattern
	deconvolution_param <- if(study == "Chemical") as.list(deconvolution_params()[which(
		deconvolution_params()$project == params$project & 
		deconvolution_params()$adduct == params$adduct &
		deconvolution_params()$chemical_type == params$chemical_type), ])
	else as.list(deconvolution_params()[which(
	  deconvolution_params()$project == params$project & 
	    deconvolution_params()$adduct == params$adduct &
	    deconvolution_params()$chemical_type == params$formula), ])
	p <- plot_chemical_EIC(db, params$project_sample, params$adduct, 
		params$chemical_type, params$C, params$Cl, params$formula, deconvolution_param$ppm, 
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
        Shiny.setInputValue('process_results_reintegration_rt_min', min);
        Shiny.setInputValue('process_results_reintegration_rt_max', max);
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
#' @param input$project integer project ID
#' @param input$process_results_file integer project_sample ID
#' @param input$process_results_study string type of study, chemical or standard
#' @param input$process_results_chemical_adduct string adduct name
#' @param input$process_results_chemical_type string type of chemical studied
#' @param input$process_results_profile_selected vector(integer)[2] contains number of Carbon & Chlore, 
#' 		correspond to the rowname and colname of the cell selected
#' @param input$process_results_standard_seleceted string, formula of standard selected
#' @param input$process_results_adduct_selected string, adduct selected
#' 
#' @return plotly object
output$process_results_ms <- plotly::renderPlotly({
  actualize$results_ms
	tryCatch({
	if (is.null(input$process_results_profile_selected) & 
	  is.null(input$process_results_standard_selected)) custom_stop(
		"invalid", "no cell selected")
  study <- isolate(input$process_results_study)
  params <- list(
    project = input$project,
    project_sample = isolate(input$process_results_file),
    adduct = if(study == "Chemical") isolate(input$process_results_chemical_adduct) 
    else input$process_results_adduct_selected, 
    chemical_type = if(study == "Chemical") input$process_results_chemical_type 
    else study, 
    C = as.numeric(input$process_results_profile_selected$C), 
    Cl = as.numeric(input$process_results_profile_selected$Cl),
    formula = input$process_results_standard_selected
  )
	# retrieve the resolution parameter to simulate isotopic pattern
  deconvolution_param <- if(study == "Chemical") as.list(deconvolution_params()[which(
    deconvolution_params()$project == params$project & 
    deconvolution_params()$adduct == params$adduct &
    deconvolution_params()$chemical_type == params$chemical_type), ])
  else as.list(deconvolution_params()[which(
    deconvolution_params()$project == params$project & 
    deconvolution_params()$adduct == params$adduct &
    deconvolution_params()$chemical_type == params$formula), ])
	plot_chemical_MS(db, params$project_sample, params$adduct, 
		params$chemical_type, params$C, params$Cl, params$formula, resolution = list(
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

#' @title File selection for matrix downloading
#' 
#' @description 
#' Will display a modal dialog for the selection of files to download
#' 
#' @param input$project integer, project id
shiny::observeEvent(input$process_results_download, {
  choices <- project_samples()[which(project_samples()$project == input$project), 
     c("sample_id", "project_sample")]
  shiny::showModal(modalDialog(
    shiny::checkboxGroupInput('process_results_download_file', 'Which file(s) ?', 
      choices = c(setNames(choices$project_sample, choices$sample_id)), 
      selected = c(setNames(choices$project_sample, choices$sample_id))
	),
    shiny::downloadButton('process_results_export', 'Export matrix'),
    easyClose = TRUE
  ))
})

#' @title Download matrix
#' 
#' @description 
#' Download the selected matrix at the xlsx format
#' 
#' @param input$project integer project ID
#' @param input$process_results_download_file integer project_sample ID
#' 
#' @return xlsx file
output$process_results_export <- shiny::downloadHandler(
  filename = function() { 
    params <- list(
      project = input$project
    )
    name <- get_project_name(db, params$project)
    paste("CPSeeker0.1_", name, ".xlsx", sep = "") },#".xlsx"
  content = function(file) {
    params <- list(
      project = input$project,
      file = input$process_results_download_file,
      matrix_type = c('Score', 'Intensity', 'Deviation')
    )
    samples <- get_samples(db, params$project)
    samples <- samples[which(samples$project_sample == params$file),]
    query <- sprintf('select chemical_type, adduct from deconvolution_param where project == %s and
      chemical_type in (select chemical_type from chemical where chemical_type != "Standard");',
      params$project)
    chemicals <- db_get_query(db, query)
    
    pb_max <- length(samples$project_sample)
    shinyWidgets::progressSweetAlert(session, 'pb', title = 'Initialisation',
     value = 0, display_pct = TRUE)
    
    wb <- openxlsx::createWorkbook()
		for(i in 1:length(samples$sample_id)){
		  msg <- sprintf("%s", samples$sample_id[i])
		  print(msg)
		 shinyWidgets::updateProgressBar(session, id = 'pb', 
			title = msg, value = (i - 1) * 100 / pb_max)
		  
		  #l <- 1
		  #addWorksheet(wb, "Sequence")#samples$sample_id[i])
		  addWorksheet(wb=wb, sheetName='Sequence', gridLines=FALSE)
		  addWorksheet(wb=wb, sheetName='Parameters', gridLines=FALSE)
		  addWorksheet(wb=wb, sheetName='Standard', gridLines=FALSE)
		  addWorksheet(wb=wb, sheetName='Label', gridLines=FALSE)
		  for(chemical in unique(chemicals$chemical_type)){
			adducts <- unique(chemicals$adduct[which(chemicals$chemical_type == chemical)])
			for(adduct in adducts){
			 #first_col <- params$project
			  #mat <- get_profile_matrix(db, samples$project_sample[i], adduct, chemical, simplify = FALSE)
			  #mat2 <- sapply(1:3, function(selected){ #changement de sapply (1:3) en sapply (1:1) pour afficher uniquement une seule table dans le csv
				#mat3 <- reduce_matrix(mat, selected, na_empty = TRUE)
				#first_col <- matrix(dimnames(mat3)[[1]])
				#mat3 <- cbind(first_col, mat3)
				#first_row <- t(matrix(dimnames(mat3)[[2]]))
				#mat3 <- rbind(first_row, mat3)
				#mat_title = params$matrix_type[selected]
				#mat3[1,1] <- mat_title
				#mat3
			  #}, simplify = FALSE, USE.NAMES = TRUE)
			  name <- get_project_name(db, params$project)
			  openxlsx::writeData(wb, 1, paste("CP-Seeker"), startRow = 1)#, chemical, adduct, sep = " - "),samples$sample_id[i]
			  openxlsx::writeData(wb,1 , paste(name),startRow = 2)#, chemical, adduct, sep = " - "),samples$sample_id[i]
			  openxlsx::writeData(wb,1 , paste("User"),startRow = 4)
			  openxlsx::writeData(wb, 1, paste("Sequence"),startRow = 5)
			  openxlsx::writeData(wb, 1, paste("Comments"),startRow = 6)
			  openxlsx::writeData(wb, 1, paste("Creation date"),startRow = 7)
			  openxlsx::writeData(wb, 1, paste("Last modified"),startRow = 8)
			  
			  openxlsx::writeData(wb, 1, paste("Sequence"),startRow = 5, startCol = 3)
			  openxlsx::writeData(wb, 1, paste("Comments"),startRow = 6, startCol = 3)
			  openxlsx::writeData(wb, 1, paste("Creation date"),startRow = 7, startCol = 3)
			  openxlsx::writeData(wb, 1, paste("Last modified"),startRow = 8, startCol = 3)
			  #openxlsx::writeData(wb, 1 , paste("User"),startRow = 9)
			  #openxlsx::writeData(wb, samples$sample_id[i], mat2, startRow = l + 1)
			  
			  #name <- get_project_name(db, params$project)
			 openxlsx::writeData(wb, 2, paste("CP-Seeker"), startRow = 1)
			 openxlsx::writeData(wb, 2, paste(name),startRow = 2)
			 openxlsx::writeData(wb, 2, paste("User"),startRow = 4)
			 openxlsx::writeData(wb, 2, paste("Sequence"),startRow = 5)
			 openxlsx::writeData(wb, 2, paste("Comments"),startRow = 6)
			 openxlsx::writeData(wb, 2, paste("Creation date"),startRow = 7)
			 openxlsx::writeData(wb, 2, paste("Last modified"),startRow = 8)
			 openxlsx::writeData(wb, 2, paste("Sequence"),startRow = 5, startCol = 3)
			 openxlsx::writeData(wb, 2, paste("Comments"),startRow = 6, startCol = 3)
			 openxlsx::writeData(wb, 2, paste("Creation date"),startRow = 7, startCol = 3)
			 openxlsx::writeData(wb, 2, paste("Last modified"),startRow = 8, startCol = 3)   
			#first_col <- params$project
			  l <- 1
			  mat <- get_profile_matrix(db, samples$project_sample[i], adduct, chemical, simplify = FALSE)
			  mat2 <- sapply(c(2,1,3), function(selected){ 
				mat3 <- reduce_matrix(mat, selected, na_empty = TRUE)
				first_col <- matrix(dimnames(mat3)[[1]])
				mat3 <- cbind(first_col, mat3)
				first_row <- t(matrix(dimnames(mat3)[[2]]))
				mat3 <- rbind(first_row, mat3)
				mat_title = params$matrix_type[selected]
				mat3[1,1] <- mat_title
				mat3
			  }, simplify = FALSE, USE.NAMES = TRUE)
			  openxlsx::writeData(wb, 4 , paste("CP-Seeker"),startRow = 1)
			  openxlsx::writeData(wb, 4, paste(samples$sample_id),startRow = 2)
			  openxlsx::writeData(wb, 4, paste(chemical, adduct, sep = " - "),startRow = 3)
			 openxlsx::writeData(wb, 4, mat2, startRow = l + 4)
			 l <- l + nrow(mat) + 3
			}
		  }
		}
    shinyWidgets::updateProgressBar(session, id = 'pb', value = 100)
    print('done')
    shiny::updateTabsetPanel(session, "tabs", "process_results")
    shinyWidgets::closeSweetAlert(session)
    openxlsx::saveWorkbook(wb, file) 
	
	

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
#' @param input$process_results_reintegration_rt_min float retention time min
#' @param input$process_results_reintegration_rt_max float retention time max
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
    retention_time = c(input$process_results_reintegration_rt_min,
      input$process_results_reintegration_rt_max),
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
    project_samples()$project == params$project & 
      project_samples()$project_sample == params$project_sample), "sample"]
  
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
    peak$project_sample <- params$project_sample
    query <- sprintf("delete from feature where chemical_ion == %s and 
      project_sample == %s", peak$chemical_ion[1], params$project_sample)
    db_execute(db, query)
    record_features(db, peak)
    val <- paste(round(peak$score[1]), 
      round(peak$intensities[1]/10**6, digits = 0), 
      round(peak$weighted_deviations[1]*10**3, digits = 1), sep = "/")
    session$sendCustomMessage("values", jsonlite::toJSON(val))
    shinyjs::runjs("
      var values = new_values[0];
      var table = $('#process_results_profile').data('datatable');
      var cell_index = table.cell(table.$('td.selected')).index();
      var C = cell_index.row;
      var Cl = cell_index.column;
      var project = $('#process_results_file').text();
    	var chemical = $('#process_results_chemical_type').text();
    	var adduct = $('#process_results_chemical_adduct').text();
      old_matrix[project][chemical][adduct][C][Cl-1] = values + '/' + old_matrix[project][chemical][adduct][C][Cl-1].split('/')[3]; 
      var mat = $('#process_results_selected_matrix button.active').text();
    	var selected_button = mat.includes('Score(%)') ? 0 : mat.includes('Normalized intensity (xE6)') ? 1 : 2;
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