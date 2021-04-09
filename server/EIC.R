#' @title Display TIC or EICs
#'
#' @description
#' Display a TIC or EIC according which button is clicked
#' add some JS for when a trace is hidden or shown to do the same to its corresponding MS trace
#' add also JS to return the rT coordinate when clicked on the graph
#'
#' @param db sqlite connection
#' @param input$eic_draw_value string, `TIC` or `EIC`
#' @param input$eic_files vector of integers, project_sample IDs
#' @param input$eic_mz float, m/z
#' @param input$eic_mz_tol float, m/z tolerance in mDa
output$eic_chromato <- plotly::renderPlotly({
	input$eic_draw_random
	params <- list(
		draw_value = input$eic_draw_value,
		files = isolate(input$eic_files), 
		mz = isolate(input$eic_mz),
		mz_tol = isolate(input$eic_mz_tol)
	)
	print('############################################################')
	print(sprintf('######################### %s ##############################', 
		params$draw_value))
	print('############################################################')
	print(params)
	
	p <- tryCatch({
	if (length(params$draw_value) == 0) custom_stop('invalid', 'no button clicked')
		
	inputs <- "eic_files"
	conditions <- length(params$files) >= 1
	msgs <- "At least a sample is required"
	check_inputs(inputs, conditions, msgs)
		
	if (params$draw_value == "TIC") plot_TIC(db, 
		project_samples = params$files)
	else {
		inputs <- c('eic_mz', 'eic_mz_tol')
		conditions <- c(!is.na(params$mz), !is.na(params$mz_tol))
		msgs <- c('no m/z provided', 'no m/z tolerance provided')
		check_inputs(inputs, conditions, msgs)
		
		conditions <- c(params$mz > 0, params$mz_tol >= 0)
		msgs <- c("m/z must be upper than 0", "m/z tolerance must be a positive number")
		check_inputs(inputs, conditions, msgs)
		
		plot_EIC(db, project_samples = params$files, 
			mz = params$mz, mda = params$mz_tol)
	}
	}, invalid = function(i) plot_empty_chromato()
	, error = function(e){
		print(e)
		sweet_alert_error(paste(e$message))
		plot_empty_chromato()
	})
	print('############################################################')
	print(sprintf('######################### END %s ##########################', 
		params$draw_value))
	print('############################################################')
		
	htmlwidgets::onRender(p, 
		"function(el, x){
			el.on('plotly_click', function(eventData){
				Shiny.onInputChange('eic_rt', eventData.points[0].x);
			});
			el.on('plotly_restyle', function(data){
				var gd = $('#eic_ms').get(0),
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
})

#' @title Draw MS
#'
#' @description
#' Draw a mass spectrum at a given rT
#'
#' @param db sqlite connection
#' @param input$eic_files vector of integers, project_sample IDs
#' @param input$eic_rt float, rT
output$eic_ms <- plotly::renderPlotly({
	params <- list(
		files = isolate(input$eic_files), 
		rt = input$eic_rt
	)
	
	tryCatch({
	if (length(params$rt) == 0) custom_stop('invalid', 'no button clicked')
	else if (params$rt == 0) custom_stop('invalid', 'no button clicked')
	else if (length(params$files) == 0) custom_stop('invalid', 'no files')
		
	plot_MS(db, project_samples = params$files, rt = params$rt)
	}, invalid = function(i) plot_empty_MS()
	, error = function(e){
		print("ERR eic_ms")
		print(e)
		sweet_alert_error(paste(e$message))
		plot_empty_MS()
	})
})