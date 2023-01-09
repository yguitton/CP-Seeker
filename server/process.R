#' @title Event when switch between parameters
#'
#' @description
#' If general is choose, will display generals parameters
#' Else if deconvolution is choose, will display the choice of the type of chemical
#' Else if standard is choose, will display the choice of the standard formula
#'
#' @param input$process_chemical_standard string, choice
shiny::observeEvent(input$process_chemical_standard, {
  params <- list(choice = input$process_chemical_standard)
  if (params$choice == "General"){
    shinyjs::show("process_general")
    shinyjs::hide("process_chemical")
    shinyjs::hide("process_standard")
  }
  else if (params$choice == "Target analyte") {
    shinyjs::hide("process_general")
    shinyjs::show("process_chemical")
    shinyjs::hide("process_standard")
  }
  else if (params$choice == "Standard") {
    shinyjs::hide("process_general")
    shinyjs::hide("process_chemical")
    shinyjs::show("process_standard")
  }
})

#javais souhaité faire de la programation dynamique qui permet de recupérer directement les vaiables adduct et chemical type avec leurs familles respectives dans la base de données
#malheureusement je n'ai pas pule faire. néanmoins jai créer deux fonction get_ecni_adduct et get_esi_adduct qui permettent de recuppérer les adduits de chaque famille dans la base de donées.
# J'ai également  créer les variables ecni_adduct et esi_apci_adduct dans le fichier manager.r  afin les  inclure dans la liste deroulante qui categorise les adduits de faço,n dynamique. 
#Cependant cela n'a pas fonctionner car le serveur ne prend pas en comte ces variables  en compte.
output$ui_process_chemical_type <- shiny::renderUI({
    table <- unique(db_get_query(db, "select chemical_type, chemical_familly from chemical"))
    splitTable <- split(table$chemical_type, table$chemical_familly)
    splitTable <- splitTable[splitTable != "Standard"]
    # Correction to have a family and the name of the chemical even if it is alone in its family
    for(x in names(splitTable)){
    	if(length(splitTable[[x]]) < 2){
    		names(splitTable[[x]]) <- splitTable[[x]]
    	}
    }
    bsplus::shinyInput_label_embed(
        shinyWidgets::pickerInput(
            "process_chemical_type",
            "Family",
            choices = splitTable,
            multiple = TRUE,
            options = list(`live-search` = TRUE)
        ),
        bsplus::bs_embed_tooltip(
            bsplus::shiny_iconlink(),
            placement = 'top',
            title = 'Type of chemical to study'
        )
    )
})

output$ui_result <- shiny::renderUI({
      paste("You chose",input$process_adduct)
    })
#' @title Event when choosing instrument
#'
#' @description
#' If Orbitrap is choose will display resolution & mz numeric box
#' Else will display picker lists with resolution & instrument names from enviPat
#'
#' @param input$process_instrument string, instrument name
shiny::observeEvent(input$process_instrument, {
	params <- list(instrument = input$process_instrument)
	if (params$instrument == "Orbitrap") {
		shiny::updateSelectInput(session, "process_resolution_index", choices = NA)
		shiny::updateNumericInput(session, "input$process_resolution", value = 140)
		shiny::updateNumericInput(session, "input$process_resolution_mz", value = 200)
		shinyjs::hide("process_resolution_index")
		shinyjs::show("process_orbitrap")
	} else {
		choices <- if (params$instrument == "QTOF_XevoG2-S") setNames(25, "25k@200")
			else if (params$instrument == "Sciex_TripleTOF5600") setNames(26, "25k@200")
			else if (params$instrument == "Sciex_TripleTOF6600") setNames(27, "25k@200")
			else if (params$instrument == "Sciex_QTOFX500R") setNames(28, "25k@200")
			else setNames(29:31, c("low_extended_highSens_R25000@200",
				"low_extended2GHz_highRes", "low_highRes4GHz_highRes"))
		shiny::updateSelectInput(session, "process_resolution_index",
			choices = choices)
		shiny::updateNumericInput(session, "input$process_resolution", value = NA)
		shiny::updateNumericInput(session, "input$process_resolution_mz", value = NA)
		shinyjs::hide("process_orbitrap")
		shinyjs::show("process_resolution_index")
	}
})

#' @title Event when choose if standard is studied
#'
#' @description
#' If yes is choose, will display standard deconvolution parameters
#' Else, will hide this parameters
#'
#' @param input$process_standard_study string, user choice
shiny::observeEvent(input$process_standard_study, {
  params <- list(standard_study = input$process_standard_study)
  if (params$standard_study == TRUE){
    shinyjs::show("process_standard_params")
  }
  else if(params$standard_study == FALSE){
    shinyjs::hide("process_standard_params")
  }
})

# List of all possible standards for deconvolution
output$ui_process_standard_formula <- shiny::renderUI({
    table <- unique(db_get_query(db, "select chemical_type, chemical_familly from chemical"))
    table <- table[which(table$chemical_familly == "Standard"),]
    std_list <- table$chemical_type
    bsplus::shinyInput_label_embed(
        shinyWidgets::pickerInput(
            "process_standard_formula",
            "Standard formula",
            choices = std_list,
            multiple = TRUE
        ),
        bsplus::bs_embed_tooltip(
          bsplus::shiny_iconlink(),
          placement = 'top',
          title = "Formula of the standard"
        )
    )
})


#' @title Event when multiple standards are selected
#'
#' @description
#' When more than one standard are chosen, will display a retention time input
#' for each standard
#'
#' @param standard_number reactive value number of standard chosen
output$process_standard_rt <- renderUI({
  standard <- standard_number()
  print(standard)
  ids <- sapply(1:standard, function(i){
    paste("process_standard_retention_time_", i, sep = "")
  })
  print(ids)
  output <- tagList()
  if(standard > 1){
    for(i in 2:standard){
      output[[i]] <- tagList()
      output[[i]][[1]] <- numericInput(ids[i], "Retention time", value = NA)
    }
  }
  output
})

#' @title Display TICs
#'
#' @description
#' Display TIC of all files in project
#' add two JS events:
#' \itemize{
#'      \item click: send the x coordinate at click to input$process_TIC_rt
#'      \item restyle: hide or show traces on ouptut$process_MS according
#'                     which traces are hidden or shown on output$process_TIC
#'}
#'
#' @param db sqlite connection
#' @param input$project integer, project ID
output$process_TIC <- plotly::renderPlotly({
	params <- list(
		project = input$project
	)
	tryCatch({
		if (is.null(params$project)) custom_stop("invalid", "no project")
		else if (params$project == "") custom_stop("invalid", "no project")
		htmlwidgets::onRender(plot_TIC(db, project = params$project),
			"function(el, x){
				el.on('plotly_click', function(eventData){
					Shiny.onInputChange('process_TIC_rt', eventData.points[0].x);
				});
				el.on('plotly_restyle', function(data){
					var gd = $('#process_MS').get(0),
						traces_hidden = el._fullData
							.filter(x => x.visible == 'legendonly')
							.map(x => x.name),
						to_hide = [],
						to_show = [],
						traces_ms = gd._fullData;

					if (traces_ms.length <= 1) return 0;
					for (var i = 1; i < traces_ms.length; i++) {
						if (traces_hidden.includes(traces_ms[i].name)) {
							to_hide.push(i);
						} else {
							to_show.push(i);
						}
					}
					if(to_hide.length > 0){
						Plotly.restyle(gd,
							{visible: 'legendonly'}, to_hide);
					}
					if(to_show.length > 0){
						Plotly.restyle(gd,
							{visible: true}, to_show);
					}
				});
			}"
		)
	}, invalid = function(i) {
		print("ERR process_TIC")
		print(i)
		plot_empty_chromato()
	}, error = function(e) {
		print("ERR process_TIC")
		print(e)
		sweet_alert_error(e$message)
		plot_empty_chromato()
	})
})

#' @title Display mass sprectras at rt
#'
#' @description
#' Display mass sprectras of all files in project  at a specific rt
#'      according the time retention selected on output$process_TIC
#'
#' @param db sqlite connection
#' @param input$project integer, project ID
#' @param input$process_TIC_rt float, time retention
output$process_MS <- plotly::renderPlotly({
	params <- list(
		project = input$project,
		rt = input$process_TIC_rt
	)
	tryCatch({
		if (is.null(params$project)) custom_stop("invalid", "no project")
		if (is.null(params$rt)) custom_stop("invalid", "no rt selected")
		else if (params$project == "") custom_stop("invalid", "no project")
		else if (params$rt == 0) custom_stop("invalid", "no rt selected")

		plot_MS(db, project = params$project, rt = params$rt)
	}, invalid = function(i) {
		print("ERR process_MS")
		print(i)
		plot_empty_MS()
	}, error = function(e) {
		print("ERR process_MS")
		print(e)
		sweet_alert_error(e$message)
		plot_empty_MS()
	})
})

#' @title Launch process event
#'
#' @description
#' Launch deconvolution process
#' use blob files stored in the database
#'
#' @param db sqlite connection
#' @param project_samples reactive value, project_samples table
#' @param input$project integer, project ID
#' @param input$process_chemical_type string, type of chemical studied
#' @param input$process_adduct vector, Adduct name
#' @param input$process_instrument string, name of the instrument
#' @param input$process_resolution_index integer, index of the instrument in the resolution list of enviPat if it is not an Orbitrap
#' @param input$process_resolution integer, resolution of instrument if it is an Orbitrap
#' @param input$process_resolution_mz float, resolution@mz if the instrument is an Orbitrap
#' @param input$process_mz_tol float, m/z tolerance to use
#' @param input$process_mz_tol_unit boolean, if TRUE m/z tolerance is in ppm, else it is in mDa
#' @param input$process_peakwidth_min float, minimum in peakwidth (in sec)
#' @param input$process_peakwidth_max float, maximum in peakwidth (in sec)
#' @param input$process_retention_time_min float, minimum in retention time (in min)
#' @param input$process_retention_time_max float, minimum in retention time (in min)
#' @param input$process_missing_scans integer, maximim number of scans to consider them consecutive
#' @param input$process_standard_formula string, standard formula
#' @param input$process_standard_adduct string adduct name for standard
#' @param input$process_standard_retention_time float, standard retention time
#'
shiny::observeEvent(input$process_launch, {
	print('############################################################')
	print('######################### PROCESS ##########################')
	print('############################################################')

	param <- list(standard_study = input$process_standard_study)
	params <- list(
		project = input$project,
		chemical_type = input$process_chemical_type,
		adduct = input$process_adduct,
		resolution = list(
			instrument = input$process_instrument,
			index = as.numeric(input$process_resolution_index),
			resolution = input$process_resolution * 10**3,
			mz = input$process_resolution_mz
		),
		mz_tol = input$process_mz_tol,
		mz_tol_unit = input$process_mz_tol_unit,
		ppm = 0, mda = 0,
		peakwidth = c(input$process_peakwidth_min, input$process_peakwidth_max),
		retention_time = c(input$process_retention_time_min, input$process_retention_time_max),
		missing_scans = input$process_missing_scans
	)
	print(params)
	if(param$standard_study) {
	  params_standard <- list(
      project = input$project,
      chemical_type = "Standard",
      standard_formula = input$process_standard_formula,
      adduct = input$process_standard_adduct,
      resolution = list(
        instrument = input$process_instrument,
        index = as.numeric(input$process_resolution_index),
        resolution = input$process_resolution * 10**3,
        mz = input$process_resolution_mz
      ),
      mz_tol = input$process_mz_tol,
      mz_tol_unit = input$process_mz_tol_unit,
      ppm = 0, mda = 0,
      peakwidth = c(input$process_peakwidth_min, input$process_peakwidth_max),
      retention_time = lapply(1:length(input$process_standard_formula), function(i){
        rt <- eval(parse(text = paste("input$process_standard_retention_time_", i, sep = "")))
      }),
      missing_scans = input$process_missing_scans
    )
	  params_standard$adduct[which(params_standard$adduct == "M-H (or M-D)")] = "M-H"
	  print(params_standard)
	}

	tryCatch({
		if (is.null(params$project)) custom_stop("minor_error",
			"A project with files is needed for processing")
		else if (params$project == "") custom_stop("minor_error",
			"A project with files is needed for processing")
		params$samples <- project_samples()[which(
			project_samples()$project == params$project), "sample"]
		if (length(params$samples) == 0) custom_stop("minor_error",
			"you need to import files in project to process them")
		params$project_samples <- project_samples()[which(
			project_samples()$project == params$project), "project_sample"]
		params$sample_ids <- project_samples()[which(
			project_samples()$project == params$project), "sample_id"]
		params$polarity <- get_project_polarity(db, params$project)
		params$mz_range <- get_project_mz_range(db, params$project_sample)
		print(params[c("samples", "project_samples", "sample_ids", "polarity")])

		inputs <- c("process_resolution", "process_resolution_mz",
			"process_mz_tol", "process_peakwidth_min", "process_peakwidth_max",
			"process_retention_time_min", "process_retention_time_max",
			"missing_scans")
		conditions <- c(
			(is.na(params$resolution$index) &
				!is.na(params$resolution$resolution)) |
				!is.na(params$resolution$index),
			(is.na(params$resolution$index) &
				!is.na(params$resolution$mz)) |
				!is.na(params$resolution$index),
			!is.na(params$mz_tol), !is.na(params$peakwidth[1]),
			!is.na(params$peakwidth[2]), !is.na(params$retention_time[1]),
			!is.na(params$retention_time[2]),!is.na(params$missing_scans))
		msgs <- c("the resolution of instrument is required",
			"the m/z reference for the resolution of instrument is required",
			"m/z tolerance is required", "Peakwidth min (s) is required",
			"Peakwidth max (s) is required", "Retention time min (min) is required",
			"Retention time max (min) is required", "missing scans is required")
		check_inputs(inputs, conditions, msgs)

		conditions <- c(
			(is.na(params$resolution$index) &
				params$resolution$resolution > 0) |
				!is.na(params$resolution$index),
			(is.na(params$resolution$index) &
				params$resolution$mz > 0) |
				!is.na(params$resolution$index),
			params$mz_tol >= 0, params$peakwidth[1] > 0,
			params$peakwidth[2] > 0, params$retention_time[1] >= 0,
			params$retention_time[2] > 0, params$missing_scans >= 0)
		messages <- c("the resolution of instrument must be over 0",
			"the m/z reference for the resolution of instrument must be over 0",
			"m/z tolerance need to be positive or 0",
			"Peakwidth min (s) must be over 0", "Peakwidth max (s) must be over 0",
			"Retention time min (min) must be a positive number or 0", "Retention time max (min) must be over 0",
			"missing scans must be a positive number or 0")
		check_inputs(inputs, conditions, messages)

		inputs <- c("process_peakwidth_min", "process_peakwidth_max",
		  "process_retention_time_min", "process_retention_time_max", "missing_scans")
		conditions <- c(params$peakwidth[1] <= params$peakwidth[2],
			params$peakwidth[1] <= params$peakwidth[2],
			params$retention_time[1] <= params$retention_time[2],
			params$retention_time[1] <= params$retention_time[2],
			params$missing_scans %% 1 == 0)
		messages <- c("Peakwidth min (s) must be lower than Peakwidth max (s)",
			"Peakwidth min (s) must be lower than Peakwidth max (s)",
			"Retention time min (min) must be lower than Retention time max (min)",
			"Retention time min (min) must be lower than Retention time max (min)",
			"Missing scan must be an integer")
		check_inputs(inputs, conditions, messages)

		inputs <- c("process_chemical_type", "process_adduct")
		conditions <- c(length(params$chemical_type) > 0, length(params$adduct) > 0)
		messages <- c("A chemical must be chosen", "An adduct for chemical must be chosen")
		check_inputs(inputs, conditions, messages)

		if(param$standard_study){
		  inputs <- c("process_standard_formula", "process_standard_adduct")
		  conditions <- c(length(params_standard$standard_formula) > 0,
		    length(params_standard$adduct) > 0)
		  messages <- c("A standard must be chosen", "An adduct for standard must be chosen")
		  check_inputs(inputs, conditions, messages)

		  inputs <- sapply(1:length(params_standard$standard_formula), function(i){
		    paste("process_standard_retention_time_", i, sep = "")
		  })
		  conditions <- sapply(1:length(inputs), function(i){
		    !is.na(params_standard$retention_time[i]) & params_standard$retention_time[i] >= 0
		  })
		  messages <- c(sapply(1:length(inputs), function(i){
		    "retention time must be a positive number or 0"
		  }))
		  check_inputs(inputs, conditions, messages)
		}
		if(param$standard_study) params_standard$retention_time <- lapply(
		  params_standard$retention_time, function(rt){
	      if(rt - 2 > 0) c(rt - 2, rt + 2) else c(0, rt + 2)
	    })

		pb_max <- length(params$samples)
		shinyWidgets::progressSweetAlert(session, 'pb', title = 'Initialisation',
			value = 0, display_pct = TRUE)
		shiny::insertUI(selector = "#sweet-alert-progress-sw",
			ui = shinyWidgets::progressBar("pb2", title = "", value = 0, display_pct = TRUE),
			immediate = TRUE, session = session)

		if (params$mz_tol_unit) {
		  params$ppm <- params$mz_tol
		  if(param$standard_study) params_standard$ppm <- params_standard$mz_tol
		}
		else {
		  params$mda <- params$mz_tol
		  if(param$standard_study) params_standard$mda <- params_standard$mz_tol
		}
		ion_forms <- get_chemical_ions(db, params$adduct, params$chemical_type)
		if(param$standard_study) ion_forms_standard <- lapply(
		  params_standard$standard_formula, function(x){
		    do.call(rbind, lapply(params_standard$adduct, function(adduct){
		        get_chemical_ions(db, adduct, params_standard$chemical_type,
		          formula = x)
		    }))
		})

  	if (nrow(ion_forms) == 0) custom_stop("minor_error", "no chemical founded
  		with this adduct")

		theoric_patterns <- get_theoric(ion_forms$ion_formula,
		  ion_forms$charge[1], params$resolution)
		if(param$standard_study) theoric_patterns_standard <-
		  lapply(1:length(ion_forms_standard), function(i){
		    get_theoric(ion_forms_standard[[i]]$ion_formula,
		      ion_forms_standard[[i]]$charge[1], params$resolution)
		  })

  	# for each theoric pattern compute m/z borns
		theoric_patterns <- lapply(theoric_patterns,
		    function(x) cbind(x, get_mass_range(x[, "mz"], params$ppm, params$mda)))
		if(param$standard_study) theoric_patterns_standard <- lapply(
		  theoric_patterns_standard, function(x){
		    lapply(x, function(y){
		        cbind(y, get_mass_range(y[, "mz"], params$ppm, params$mda))
		      })
		  })

		peaks <- NULL
  	peaks_standard <- NULL
  	for (i in 1:length(params$samples)) {
  		shinyWidgets::updateProgressBar(session, id = "pb2",
  			value = 0, title = "")
  		msg <- sprintf("load data of %s", params$sample_ids[i])
  		print(msg)
  		shinyWidgets::updateProgressBar(session, id = 'pb',
  			title = msg, value = (i - 1) * 100 / pb_max)
  		ms_file <- load_ms_file(db, sampleID = params$samples[i])

  		msg <- sprintf("target on %s", params$sample_ids[i])
  		print(msg)
  		shinyWidgets::updateProgressBar(session, id = 'pb',
  			title = msg, value = (i - 1) * 100 / pb_max)

  		status <- get_patterns_status(theoric_patterns, params$mz_range[i,])
  		deleted <- which(status == "outside")
  		scalerange <- round((params$peakwidth / mean(diff(ms_file@scantime))) /2)
  		peaks2 <- if(length(deleted) != 0) deconvolution(ms_file, theoric_patterns[-deleted],
  			ion_forms[-deleted,]$chemical_ion, scalerange, params$retention_time,
  		  params$missing_scans, pb = "pb2")
  		  else deconvolution(ms_file, theoric_patterns,
  		    ion_forms$chemical_ion, scalerange, params$retention_time,
  		    params$missing_scans, pb = "pb2")
  		if (length(peaks2) > 0) peaks <- rbind(peaks, cbind(
  			project_sample = params$project_samples[i],
  			peaks2))
  		else toastr_error(sprintf("no chemical detected
  			in %s", params$samples[i]))

  		if(param$standard_study){
  		  peaks2_standard <- do.call(rbind, lapply(1:length(theoric_patterns_standard), function(i){
  		    deconvolution(ms_file, theoric_patterns_standard[[i]],
  		      ion_forms_standard[[i]]$chemical_ion, scalerange, params_standard$retention_time[[i]],
  		      params$missing_scans, pb = "pb2")
  		  }))
  		  if (length(peaks2_standard) > 0) peaks_standard <- rbind(peaks_standard, cbind(
  		    project_sample = params$project_samples[i],
  		    peaks2_standard))
  		}
 	  }
  	msg <- "record peaks"
  	print(msg)
  	shinyWidgets::updateProgressBar(session, id = 'pb',
  		title = msg, value = 100)

  	delete_features(db, params$project_samples, params$adduct, params$chemical_type)
    delete_deconvolution_params(db, params$project, params$adduct, params$chemical_type)
    if (length(peaks) > 0) {
  	  record_deconvolution_params(db, params)
  	  record_features(db, peaks)
    }

    if(param$standard_study){
      delete_features(db, params$project_samples, params_standard$adduct, params_standard$chemical_type)
  	  delete_deconvolution_params(db, params$project, params_standard$adduct, params_standard$standard_formula)
      if (length(peaks_standard) > 0) {
    	  record_deconvolution_params(db, params_standard)
    	  record_features(db, peaks_standard)
    	}
  	}

		print('done')
		shiny::updateTabsetPanel(session, "tabs", "process_results")
		shinyWidgets::closeSweetAlert(session)
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
	print('######################### END PROCESS ######################')
	print('############################################################')
})
