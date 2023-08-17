#' @title Event when switch between chemical and standard
#' 
#' @description 
#' If standard is choose, will display the choice of the formula and adduct.
#' If chemical type is choose, will display the choice of the sample and adduct.
#' 
#' @param input$process_results_study string, choice
shiny::observeEvent(input$process_results_study, {
  params <- list(choice = input$process_results_study)
  if (params$choice == "Standard") {
    shinyjs::hide("process_result_sample")
    shinyjs::show("process_results_standard")
    shinyjs::hide("process_results_adduct")
    shinyjs::show("process_results_adduct2")
    shinyjs::hide("process_results_selected_matrix")
    shinyjs::hide("process_results_download")
    shinyjs::hide("process_results_deviation_max")
    shinyjs::hide("process_results_score_min")
    shinyjs::hide("process_results_apply")
    shinyjs::hide("process_results_profile_div")
    shinyjs::show("process_results_standard_table")
  }else{
    shinyjs::show("process_result_sample")
    shinyjs::hide("process_results_standard")
    shinyjs::show("process_results_adduct")
    shinyjs::hide("process_results_adduct2")
    shinyjs::show("process_results_selected_matrix")
    shinyjs::show("process_results_download")
    shinyjs::show("process_results_deviation_max")
    shinyjs::show("process_results_score_min")
    shinyjs::show("process_results_apply")
    shinyjs::show("process_results_profile_div")
    shinyjs::hide("process_results_standard_table")
  }
})

# Filter variable to keep filters matrix when one filter was applied
filters_apply <- reactiveValues(f=FALSE)

# Reactive final matrix to show resultats according to project, file, chemical type, adduct and results selections
final_mat <- reactive({
  samples <- get_samples(db, input$project)
  file <- samples$sample_id[which(samples$project_sample == input$process_results_file)]
  if(length(file) == 0) file <- samples$sample_id[1]
  if(input$process_results_selected_matrix == "Normalized intensity (xE6)"){
    select_choice <- 2
  }else if(input$process_results_selected_matrix == "Score (%)"){
    select_choice <- 1
  }else if(input$process_results_selected_matrix == "Deviation (mDa)"){
    select_choice <- 3
  }else{
    print("ERROR !!")
  }
  if(input$process_results_chemical_adduct %in% names(mat()[[file]][[input$process_results_study]])){
    table <- reduce_matrix(mat()[[file]][[input$process_results_study]][[input$process_results_chemical_adduct]], select_choice, greycells = TRUE)
    if("Error" %in% colnames(table)){
      data.frame(Error = paste("This adduct doesn't exist for this chemical type sorry !",3,sep="/"))
    }else{
      table
    }
  }else{
    data.frame(Error = paste("This adduct doesn't exist for this chemical type sorry !",3,sep="/"))
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
#' @param input$process_results_study string type of chemical studied
#' @param input$process_results_selected_matrix string type of matrix selected, 
#'   can be "Scores", "Normalized intensities", "Deviations"
#' 
#' DataTable instance with the profile matrix
output$process_results_profile <- DT::renderDataTable({
  actualize$results_matrix
  actualize$deconvolution_params
  if(filters_apply$f == FALSE){
    final_mat()
  }else{
    final_filter_mat()
  }
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
    table.cells().every(function() {
      if(this.index().column == 0) {
        this.data(this.data());
      }else if (this.data() == null){
        $(this.node()).addClass('outside');
      }else if (this.data() != null){
        var splitted_cell = this.data().split('/');
        if(splitted_cell[0] == 'NA'){
          this.data('')
        }else{
          this.data(splitted_cell[0]);
        }
        if(splitted_cell[1] == 'outside'){
          $(this.node()).addClass('outside');
        }else if(splitted_cell[1] == 'half'){
          $(this.node()).addClass('half');
        }else if(splitted_cell[1] == 'inside'){
          $(this.node()).removeClass('outside');
          $(this.node()).removeClass('half');
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
"))

# Observe event to change the filter checked variable to TRUE when "apply" button clicked
observeEvent(input$process_results_apply, {
  print(paste0("Apply filter on score : min = ", input$process_results_score_min," and max = 100"))
  print(paste0("Apply filter on deviation : min = -", input$process_results_deviation_max, " - max = ", input$process_results_deviation_max))
  filters_apply$f <- TRUE
})

# Reactive final matrix to show resultats according to project, file, chemical type, adduct and results selections
# When a filter has been applied
final_filter_mat <- reactive({
  if(filters_apply$f == TRUE){
    samples <- get_samples(db, input$project)
    file <- samples$sample_id[which(samples$project_sample == input$process_results_file)]
    if(length(file) == 0) file <- samples$sample_id[1]
    if(input$process_results_selected_matrix == "Normalized intensity (xE6)"){
      select_choice <- 2
    }else if(input$process_results_selected_matrix == "Score (%)"){
      select_choice <- 1
    }else if(input$process_results_selected_matrix == "Deviation (mDa)"){
      select_choice <- 3
    }else{
      print("ERROR !!")
    }
  }
  reduce_matrix(filter_mat()[[file]][[input$process_results_study]][[input$process_results_chemical_adduct]], select_choice, greycells = TRUE)
})


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
  samples <- get_samples(db, params$project)
  query <- sprintf('select chemical_type, adduct from deconvolution_param where project == %s and
    chemical_type in (select chemical_type from chemical where chemical_familly == "Standard");',
      params$project)
  standard <- db_get_query(db, query)
  table_params <- list(
    standard = unique(standard$chemical_type)[which(unique(standard$chemical_type) == input$process_results_standard_formula)],
    adduct = unique(standard$adduct)[which(unique(standard$adduct) == input$process_results_standard_adduct)]
  )
  if(nrow(standard) > 0){
    table <- get_standard_table(db, params$project, table_params$adduct, table_params$standard)
    session$sendCustomMessage("Standard", jsonlite::toJSON(as.matrix(table)))
    as.matrix(table)
  }else{
    table <- as.data.frame("No results")
    as.data.frame(table)
  }
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
#' @param input$process_results_study string type of chemical studied
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
		chemical_type = input$process_results_study,
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
#' @param input$process_results_study string type of chemical studied
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
    chemical_familly = input$process_results_study,
    chemical_type = input$process_results_standard_selected,
    formula = "formula to complete"
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
#' @param input$process_results_study string type of chemical studied
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
		adduct = if(study != "Standard") isolate(input$process_results_chemical_adduct) 
	    else input$process_results_adduct_selected, 
		chemical_type = if(study != "Standard") isolate(input$process_results_study) 
		  else study, 
		C = as.numeric(input$process_results_profile_selected$C), 
		Cl = as.numeric(input$process_results_profile_selected$Cl),
		formula = input$process_results_standard_selected
	)
	# retrieve the parameters used for the deconvolution to trace EICs with same parameters
	# same reasoning for the resolution parameter to simulate isotopic pattern
  deconvolution_param <- if(study != "Standard") as.list(deconvolution_params()[which(
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
#' @param input$process_results_study string type of chemical studied
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
    adduct = if(study != "Standard") isolate(input$process_results_chemical_adduct) 
    else input$process_results_adduct_selected, 
    chemical_type = if(study != "Standard") input$process_results_study 
    else study, 
    C = as.numeric(input$process_results_profile_selected$C), 
    Cl = as.numeric(input$process_results_profile_selected$Cl),
    formula = input$process_results_standard_selected
  )
	# retrieve the resolution parameter to simulate isotopic pattern
  deconvolution_param <- if(study != "Standard") as.list(deconvolution_params()[which(
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
  print('############################################################')
  print('###################### EXPORTATION #########################')
  print('############################################################')
  print(Sys.time())
  files <- project_samples()[which(
    project_samples()$project == input$project), "sample_id"]
  allDeconv <- deconvolution_params()[which(
    deconvolution_params()$project == input$project),]
  chem_type <- unique(deconvolution_params()[which(
    deconvolution_params()$project == input$project), "chemical_type"])
  # Search for all families we have
  family <- unique(db_get_query(db, "select chemical_type, chemical_familly from chemical"))
  # Merge our type and their family
  chem_type <- family[which(family$chemical_type %in% chem_type),]
  chem_type <- chem_type[-which(chem_type$chemical_familly == "Standard"),]
  actual_user <- input$user
  actual_project_informations <- projects()[which(
    projects()$project == input$project),]
  pbValue <- 0 # When add unique is when we considered all chem type in one family
  shinyWidgets::progressSweetAlert(session, 'exportBar', value = pbValue, title = "Exportation...", striped = TRUE, display_pct = TRUE)
  if(length(c(grep("Chlorinated paraffins",chem_type$chemical_familly), grep("Brominated paraffins",chem_type$chemical_familly))) > 0){
    adducts <- deconvolution_params()[which(
      deconvolution_params()$chemical_type %in% chem_type$chemical_type[c(grep("Chlorinated paraffins",chem_type$chemical_familly), grep("Brominated paraffins",chem_type$chemical_familly))]), ]
    adducts <- unique(adducts[which(adducts$project == input$project), "adduct"])
    export_PCA(actual_user, maxBar = length(unique(chem_type$chemical_familly))*length(adducts), chem_type = chem_type[c(grep("Chlorinated paraffins",chem_type$chemical_familly), grep("Brominated paraffins",chem_type$chemical_familly)), "chemical_type"], 
      adducts = adducts, actual_project_informations, pbValue)
    pbValue <- pbValue + length(allDeconv[c(grep("^PCAs",allDeconv$chemical_type), grep("^PBAs",allDeconv$chemical_type)),"adduct"])
  }
  if(length(grep("Chlorinated olefins", chem_type$chemical_familly)) > 0){
    adducts <- deconvolution_params()[which(
      deconvolution_params()$chemical_type %in% chem_type$chemical_type[grep("Chlorinated olefins",chem_type$chemical_familly)]), ]
    adducts <- unique(adducts[which(adducts$project == input$project), "adduct"])
    export_PCO(actual_user, maxBar = length(unique(chem_type$chemical_familly))*length(adducts), chem_type = chem_type[grep("Chlorinated olefins",chem_type$chemical_familly), "chemical_type"], 
      adducts = adducts, actual_project_informations, pbValue)
    pbValue <- pbValue + length(unique(allDeconv[grep("P.*Os",allDeconv$chemical_type),"adduct"]))
  }
  if(length(grep("Mixed paraffins",chem_type$chemical_familly)) > 0){
    adducts <- deconvolution_params()[which(
      deconvolution_params()$chemical_type %in% chem_type$chemical_type[grep("Mixed paraffins",chem_type$chemical_familly)]), ]
    adducts <- unique(adducts[which(adducts$project == input$project), "adduct"])
    export_PXA(actual_user, maxBar = length(unique(chem_type$chemical_familly))*length(adducts), chem_type = chem_type[grep("Mixed paraffins",chem_type$chemical_familly), "chemical_type"], 
      adducts = adducts, actual_project_informations, pbValue)
    pbValue <- pbValue + length(unique(allDeconv[grep("PXAs",allDeconv$chemical_type),"adduct"]))
  }
  if(length(grep("Phase I metabolites", chem_type$chemical_familly)) > 0){
    adducts <- deconvolution_params()[which(
      deconvolution_params()$chemical_type %in% chem_type$chemical_type[grep("Phase I metabolites",chem_type$chemical_familly)]), ]
    adducts <- unique(adducts[which(adducts$project == input$project), "adduct"])
    export_phase1(actual_user, maxBar = length(unique(chem_type$chemical_familly))*length(adducts), chem_type = chem_type[grep("Phase I metabolites",chem_type$chemical_familly), "chemical_type"], 
      adducts = adducts, actual_project_informations, pbValue)
    pbValue <- pbValue + length(unique(allDeconv[grep("-PCAs",allDeconv$chemical_type),"adduct"]))
  }
  if(length(grep("Phase II metabolites", chem_type$chemical_familly)) > 0){
    adducts <- deconvolution_params()[which(
      deconvolution_params()$chemical_type %in% chem_type$chemical_type[grep("Phase I metabolites",chem_type$chemical_familly)]), ]
    adducts <- unique(adducts[which(adducts$project == input$project), "adduct"])
    export_phase2(actual_user, maxBar = length(unique(chem_type$chemical_familly))*length(adducts), chem_type = chem_type[grep("Phase II metabolites",chem_type$chemical_familly), "chemical_type"], 
      adducts = adducts, actual_project_informations, pbValue)
    pbValue <- pbValue + length(unique(allDeconv[grep("-OH-PCAs",allDeconv$chemical_type),"adduct"]))
  }

  toastr_success("Exportation success !")
  print(Sys.time())
  print('############################################################')
  print('################### END OF EXPORTATION #####################')
  print('############################################################')
  shinyWidgets::closeSweetAlert(session)
})


#' @title Launch reintegration
#' 
#' @decription 
#' Launch reintegration of a chloroparaffin
#' 
#' @param db sqlite connection
#' @param input$project integer, project ID
#' @param input$process_results_file string, file studied
#' @param input$process_results_study string, type of chemical studied
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
    chemical_type = input$process_results_study,
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
    	var chemical = $('#process_results_study').text();
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