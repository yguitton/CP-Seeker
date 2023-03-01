#' @title standard reactive value
#' 
#' @description 
#' 
#' @param input$process_standard_formula string, standard formula
standard_number <- reactive({
  length(input$process_standard_type)
})

#' @title Force reactualization of reactive values
#'
#' @description
#' reactive value update each time a table of the database is modified
#' contains a list with float for each table
actualize <- shiny::reactiveValues(
	users = 0,
	projects = 0, 
	samples = 0, 
	project_samples = 0, 
	deconvolution_params = 0,
	results_eic = 0,
	results_ms = 0,
	results_matrix = 0,
	graphics_bubble = 0,
	graphics_surface_plot = 0,
	graphics_contours = 0,
	graphics_histogram = 0
)

#' @title Matrix reactive value
#'
#' @description
#' matrix reactive value to show results
#' update when user changes the actual project
#'
#' @return list of matrix
mat <- reactive({  
  query <- sprintf('select chemical_type, adduct from deconvolution_param where project == %s and
    chemical_type in (select chemical_type from chemical where chemical_type != "Standard");',
    input$project)
  chemicals <- db_get_query(db, query)
  samples <- get_samples(db, input$project)
  mat <- list()
  for(i in 1:length(samples$sample_id)){
    mat2 <- sapply(samples$sample_id[i], function(project){
      sapply(unique(chemicals$chemical_type), function(chemical){
        sapply(unique(chemicals$adduct[which(chemicals$chemical_type == chemical)]), function(adduct){
          get_profile_matrix(db, samples$project_sample[i], adduct, chemical)
        }, simplify = FALSE, USE.NAMES = TRUE)
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, simplify = FALSE, USE.NAMES = TRUE)
    mat <- append(mat, mat2)
  }
  return(mat)
})

#' @title Matrix with filters reactive value
#'
#' @description
#' matrix with filters reactive value update each time
#' "apply" button is actualize
#'
#' @return list of matrix
filter_mat <- reactive({
	input$process_results_apply # To reload when new value
	chemicals <- data.frame()
	for(chem in 1:length(names(mat()[[names(mat())[1]]]))){
		add <- data.frame(chemical_type = rep(names(mat()[[names(mat())[1]]])[chem], length(names(mat()[[names(mat())[1]]][[chem]]))), adduct = names(mat()[[names(mat())[1]]][[chem]]))
		chemicals <- rbind(chemicals, add)
	}
	temp <- mat()
	filter_mat <- list()
	nameForRename <- list()
	for(i in 1:length(names(temp))){
		temp2 <- sapply(names(temp)[i], function(project){
      sapply(unique(chemicals$chemical_type), function(chemical){
        sapply(unique(chemicals$adduct[which(chemicals$chemical_type == chemical)]), function(adduct){
          cellToFilter <- which(reduce_matrix(temp[[project]][[chemical]][[adduct]],1) < isolate(input$process_results_score_min), arr.ind=TRUE)
          cellToFilter <- rbind(cellToFilter, which(reduce_matrix(temp[[project]][[chemical]][[adduct]],3) > isolate(input$process_results_deviation_max), arr.ind=TRUE))
          cellToFilter <- rbind(cellToFilter, which(reduce_matrix(temp[[project]][[chemical]][[adduct]],3) < -(isolate(input$process_results_deviation_max)), arr.ind=TRUE))
        	if(nrow(cellToFilter) > 0){
        		for(c in 1:nrow(cellToFilter)){
        			keep <- strsplit(temp[[project]][[chemical]][[adduct]][cellToFilter[c,1], cellToFilter[c,2]], "/")[[1]][4]
        			temp[[project]][[chemical]][[adduct]][cellToFilter[c,1], cellToFilter[c,2]] <- paste0("NA/NA/NA/",keep)
        		}
        	}
        	return(temp[[project]][[chemical]][[adduct]])
        }, simplify = FALSE, USE.NAMES = TRUE) 
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, simplify = FALSE, USE.NAMES = TRUE)
    filter_mat[[i]] <- temp2[[names(temp)[i]]]
    nameForRename <- c(nameForRename,names(temp)[i])
	}
	names(filter_mat) <- nameForRename
	return(filter_mat)
})

share_vars <- shiny::reactiveValues()

#' @title Users reactive value
#'
#' @description
#' users reactive value update each time actualize is modified
#' represent the users table in database
#'
#' @return vector of strings
users <- shiny::eventReactive(actualize$users, db_get_query(db, 
	"select user from user;")$user)

#' @title Users reactive value event
#'
#' @description
#' update picker list of users when users reactive value change
#'
#' @param users reactive value, users table database
#' @param last_user string, last login used
shiny::observeEvent(users(), shinyWidgets::updatePickerInput(session, 'user', 
	label = NULL, choices = users(), selected = last_user))
	
#' @title Samples reactive value
#'
#' @description
#' samples reactive value update each time actualize is modified
#' represent the samples table in database
#'
#' @return dataframe with columns:
#' \itemize{
#'      \item sample string, sample ID
#'      \item size string size of the blob stored
#'      \item raw_path string, old path of the file
#'      \item polarity string, "negative" or "positive"
#'      \item path string, path is file was stored as mzXML in app
#'      \item instrument_model string, instrument model
#'      \item instrument_manufacturer string, instrument manufacturer
#'      \item software_name string, name of software
#'      \item software_version string, version of software
#'      \item ion_source string, ion source
#'      \item analyzer string, analyzer
#'      \item detector_type string, type of detector
#'      \item resolution string, resolution of instrument
#'      \item agc_target string, AGC target
#'      \item maximum_it string, maximum IT
#'      \item number_of_scan_range string, number of scans
#'      \item scan_range string, scan range
#'}
samples <- shiny::eventReactive(actualize$samples, db_get_query(db, 
	"select sample, round(length(raw_data) / 1000000) as size, raw_path, polarity, 
		path, instrument_model, instrument_manufacturer, software_name, 
		software_version, ion_source, analyzer, detector_type, resolution, 
		agc_target, maximum_it, number_of_scan_range, scan_range from sample;"))

#' @title Projects reactive value
#'
#' @description
#' projects reactive value update each time actualize is modified
#' represent the projects table in database
#'
#' @return datafram with columns:
#' \itemize{
#'      \item project integer, project id
#'      \item name string, project name
#'      \item comments string, project comments
#'      \item creation date, date of creation
#'      \item modified date, last date of project modification
#'      \item param_xcms integer, param_xcms ID
#'      \item param_pairing integer, param_pairing ID
#'      \item param_alignment integer, param_alignment ID
#'      \item param_camera integer, param_camera ID 
#'}
projects <- shiny::eventReactive(actualize$projects, db_get_query(db, 
	"select * from project;"))

#' @title Projects reactive value event
#'
#' @description
#' update picker list of projects when projects reactive value change
#'
#' @param projects reactive value, projects table database
#' @param last_project integer, last project id used
shiny::observeEvent(projects(), shinyWidgets::updatePickerInput(session, 'project', 
	label = NULL, choices = isolate(setNames(projects()$project, projects()$name)), 
	selected = last_project))

#' @title Project_samples reactive value
#'
#' @description
#' Project_samples reactive value update each time actualize is modified
#' represent the project_samples table in database
#'
#' @return dataframe with columns
#' \itemize{
#'      \item project_sample integer, project_sample ID
#'      \item project integer, project ID of table project
#'      \item sample string, sample ID of table sample
#'      \item sample_id string, other names used for sample in project
#'}
project_samples <- shiny::eventReactive(actualize$project_samples, db_get_query(db, 
	"select * from project_sample order by sample;"))

#' @title Project_samples reactive value event
#'
#' @description
#' update picker list of project_samples when project_samples reactive value change
#' and also when user change project
#'
#' @param project_samples reactive value, project_samples table database
#' @param input$project integer, project id
shiny::observeEvent(c(project_samples(), input$project), {
	choices <- split(project_samples(), project_samples()$project)
	choices <- lapply(choices, function(x) 
		setNames(x$project_sample, x$sample_id))
	selected <- project_samples()[which(
		project_samples()$project == input$project), "project_sample"]
	names(choices) <- projects()[which(projects()$project %in% 
		unique(project_samples()$project)), "name"]
	shinyWidgets::updatePickerInput(session, "files_in_db", label = NULL, 
		 choices = choices, selected = selected)
	shiny::updateSelectInput(session, "files_from_db", label = NULL, 
		choices = choices, selected = selected)
	shiny::updateSelectInput(session,"regression_theoretic_files", label = "Samples", 
	  choices = choices)
	choices <- project_samples()[which(project_samples()$project == input$project), 
		c("sample_id", "project_sample")]
	shiny::updateSelectizeInput(session, "eic_files", 
		label = "Sample(s)", choices = setNames(
			choices$project_sample, choices$sample_id))
	shinyjs::runjs("Shiny.onInputChange('process_TIC_rt', 0);")
	shinyjs::runjs("Shiny.onInputChange('eic_rt', 0);")
	shiny::updateSelectInput(session, "process_results_file", 
		label = "Sample", choices = setNames(
			choices$project_sample, choices$sample_id))
	shiny::updateSelectInput(session, "graphics_file",
	  label = "Sample", choices = setNames(
	    choices$project_sample, choices$sample_id))
	shiny::updateSelectInput(session, "regression_observed_file", 
	  label = "Sample", choices = setNames(
	    choices$project_sample, choices$sample_id))
})

#' @title deconvolution_params reactive value
#'
#' @description
#' deconvolution_params reactive value update each time actualize is modified
#' represent the deconvolution_param table in database
#'
#' @return dataframe with columns
#' \itemize{
#'      \item deconvolution_param integer deconvolution_param ID
#' 	  \item chemical_type string type of chemical
#' 		\item adduct string adduct name
#' 		\item instrument string name of the instrument
#' 		\item resolution float resolution of the instrument
#' 		\item mz float m/z where resolution of the instrument where measured
#' 		\item resolution_index integer ID in the list resolution_list of enviPat
#' 		\item ppm float ppm tolerance used
#' 		\item mda float mda tolerance used
#' 		\item peakwidth_min float peakwidth min
#' 		\item peakwidth_max float peakwidth max
#' 		\item retention_time_min float retention time min
#' 		\item retention_time_max float retention time max
#' 		\item missing_scans integer missing scan parameter
#'}
deconvolution_params <- shiny::eventReactive(actualize$deconvolution_params, 
	db_get_query(db, "select * from deconvolution_param;"))

#' @title deconvolution_params reactive value event
#' 
#' @description update molecule type list when a deconvolution was made
#' 
#' @param deconvolution_params reactive value deconvolution_param table database
#' @param input$project integrer, project id
shiny::observeEvent(c(deconvolution_params(), input$project), {
	std <- db_get_query(db, "select chemical_type from chemical where chemical_familly == 'Standard'")$chemical_type
	choices <- deconvolution_params()[which(
    deconvolution_params()$project == input$project), "chemical_type"]
	table <- unique(db_get_query(db, "select chemical_type, chemical_familly from chemical"))
	table <- table[which(table$chemical_type %in% choices),]
  splitTable <- split(table$chemical_type, table$chemical_familly)
	if("Standard" %in% names(splitTable)){
		splitTable$Standard <- "Standard" # Delete types from Standard and add 'Standard'
  	# Correction to have a family and the name of the chemical even if it is alone in its family
  	for(x in names(splitTable)){
  		if(length(splitTable[[x]]) < 2){
  			names(splitTable[[x]]) <- splitTable[[x]]
  		}
  	}
		shiny::updateSelectInput(session, "process_results_study", 
		"Type", choices = splitTable, selected = "Standard")
	}else{
		# Just keep choices it like it is (without any standards)
		# Correction to have a family and the name of the chemical even if it is alone in its family
  	for(x in names(splitTable)){
  		if(length(splitTable[[x]]) < 2){
  			names(splitTable[[x]]) <- splitTable[[x]]
  		}
  	}
		shiny::updateSelectInput(session, "process_results_study", 
		"Type", choices = splitTable)
	}	
})

#' @title deconvolution_params reactive value event
#' 
#' @description update type list when a deconvolution was made
#' 
#' @param deconvolution_params reactive value deconvolution_param table database
#' @param input$project integrer, project id
shiny::observeEvent(c(deconvolution_params(), input$project), {
  choices <- deconvolution_params()[which(
    deconvolution_params()$project == input$project), "chemical_type"]
  choices <- c(choices[grep("PXA", choices)], choices[which(choices %in% c("PCAs", "PCOs", "PCdiOs"))])
  shiny::updateSelectInput(session, "graphics_chemical",
    "Family", choices = choices)
  shiny::updateSelectInput(session, "regression_observed_family", 
    "Family", choices = choices)
  shiny::updateSelectInput(session, "regression_theoretic_family",
    "Family", choices = choices)
})

#' @title deconvolution_params reactive value event
#' 
#' @description update chemical adduct list when a deconvolution was made
#' 
#' @param deconvolution_params reactive value deconvolution_param table database
#' @param input$project integrer, project id
#' @param input$process_results_study string, chemical type
shiny::observeEvent(c(deconvolution_params(), input$project, input$process_results_study), {
  choices <- deconvolution_params()[which(
    deconvolution_params()$project == input$project & 
      deconvolution_params()$chemical_type == input$process_results_study), 
    "adduct"]
  # That if loop to be able to keep the selected adduct when travel between chemical type
  if(input$process_results_chemical_adduct != ""){
  	shiny::updateSelectInput(session, "process_results_chemical_adduct", 
    	"Adduct", choices = as.factor(choices), selected = input$process_results_chemical_adduct)
  }else{
  	shiny::updateSelectInput(session, "process_results_chemical_adduct", 
    	"Adduct", choices = as.factor(choices))
  }
  shiny::updateSelectInput(session, "graphics_adduct",
    "Adduct", choices = choices)
  shiny::updateSelectInput(session, "regression_observed_adduct",
    "Adduct", choices = choices)
  shiny::updateSelectInput(session, "regression_theoretic_adduct",
    "Adduct", choices = choices)
})

#' @title deconvolution_params reactive value event
#' 
#' @description 
#' update standard formula list when a deconvolution was made
#' 
#' @param deconvolution_params reactive value standard_deconvolution_param table database
#' @param input$project integer, project id
shiny::observeEvent(c(deconvolution_params(), input$project), {
  choices <- deconvolution_params()[which(
    deconvolution_params()$project == input$project), "chemical_type"]
  choices <- choices[-which(choices %in% c("PCAs", "PBAs", "PCOs", "PCdiOs"))]
  choices <- choices[-grep("PXAs", choices)]
  shiny::updateSelectInput(session, "process_results_standard_formula", 
    "Standard formula", choices = choices)
})

#' @title deconvolution_params reactive value event
#' 
#' @description 
#' update standard adduct list when a deconvolution was made
#' 
#' @param deconvolution_params reactive value standard_deconvolution_param table database
#' @param input$project integrer, project id
#' @param input$process_results_standard_formula string, standard formula
shiny::observeEvent(c(deconvolution_params(), input$project, input$process_results_standard_formula), {
  choices <- deconvolution_params()[which(
    deconvolution_params()$project == input$project & 
      deconvolution_params()$chemical_type == input$process_results_standard_formula), 
    "adduct"]
  shiny::updateSelectInput(session, "process_results_standard_adduct", 
    "Adduct", choices = choices)
})



#' @title adduct reactive value
#'
#' @description
#' adducts reactive value update each time actualize is modified
#' represent the adduct table in database
#'
#' @return datafram with columns:
#' \itemize{
#'      \item adduct string, adduct name
#'      \item chemical_ion_family string, adduct familly name

#'}
#adducts <- shiny::eventReactive(actualize$adduct, db_get_query(db, 
	#"select distinct adduct from chemical_ion;")
#ecni_adduct <- shiny::eventReactive(actualize$adduct, db_get_query(db,'select distinct adduct as ecni from chemical_ion where chemical_ion_family = "ECNI";')

#esiapci_adducts <- shiny::eventReactive(actualize$adduct, db_get_query(db,'select distinct adduct as esiapci from chemical_ion where chemical_ion_family = "ESI/APCI";')
#' @title adducts reactive value event
#'
#' @description
#' update picker list of adducts when adducts reactive value change
#'
#' @param adducts reactive value, adducts in database
#' @param adduct_selected string, adduct selected
#shiny::observeEvent(adducts(), shinyWidgets::updatePickerInput(session, 'process_chemical', 
	#label = NULL, choices = isolate(setNames(adducts()$adduct, adducts()$chemical_ion_family))
	#)
	#shiny::updateSelectInput(session, "process_chemical", 
    #Adduct", choices = choices)
	#)

#' @title adduct reactive value event
#'
#' @description
#' update picker list of project_samples when project_samples reactive value change
#' and also when user change project
#'
#' @param project_samples reactive value, project_samples table database
#' @param input$project integer, project id

	