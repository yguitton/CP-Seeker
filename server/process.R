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
#' @param input$process_adduct string, Adduct name
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
shiny::observeEvent(input$process_launch, {
	print('############################################################')
	print('######################### PROCESS ##########################')
	print('############################################################')
	
	params <- list(
		project = input$project, 
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
		check_inputs(inputs, conditions, msgs)
			
		inputs <- c("process_peakwidth_min", "process_peakwidth_max", "process_retention_time_min", 
		            "process_retention_time_max", "missing_scans")
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
		check_inputs(inputs, conditions, msgs)
		
		pb_max <- length(params$samples)
		shinyWidgets::progressSweetAlert(session, 'pb', title = 'Initialisation',
			value = 0, display_pct = TRUE)
		shiny::insertUI(selector = "#sweet-alert-progress-sw", 
			ui = shinyWidgets::progressBar("pb2", title = "", value = 0, display_pct = TRUE), 
			immediate = TRUE, session = session)
		
		if (params$mz_tol_unit) params$ppm <- params$mz_tol
		else params$mda <- params$mz_tol
		ion_forms <- get_chloroparaffin_ions(db, params$adduct)
		if (nrow(ion_forms) == 0) custom_stop("minor_error", "no chloroparaffin founded 
			with this adduct")
		theoric_patterns <- get_theoric(ion_forms$ion_formula, 
			ion_forms[1, "charge"], params$resolution)
		# for each theoric pattern compute m/z borns
		theoric_patterns <- lapply(theoric_patterns, function(x) 
			cbind(x, get_mass_range(x[, "mz"], params$ppm, params$mda)))
		
		peaks <- NULL
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
			scalerange <- round((params$peakwidth / mean(diff(ms_file@scantime))) /2)
			peaks2 <- deconvolution(ms_file, theoric_patterns, 
				ion_forms$chloroparaffin_ion, scalerange, 
				params$missing_scans, pb = "pb2")
			if (length(peaks2) > 0) peaks <- rbind(peaks, cbind(
				project_sample = params$project_samples[i], 
				peaks2))
			else toastr_error(sprintf("no chloroparaffin detected 
				in %s", params$samples[i]))
		}
		
		msg <- "record peaks"
		print(msg)
		shinyWidgets::updateProgressBar(session, id = 'pb', 
			title = msg, value = 100)					
		delete_features(db, params$project_samples)
		delete_deconvolution_params(db, params$project, params$adduct)
		if (length(peaks) > 0) {
			record_deconvolution_params(db, params)
			record_features(db, peaks)
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