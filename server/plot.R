#' @title
#'
#' @description
#' Override plotly constructor by adding a JS event:
#'     hover on legend items will highligh its associated trace
#'
#' @param ... parameters to pass to plotly constructor
#'
#' @return plotly object
plot_ly <- function(...) {
	p <- plotly::plot_ly(...)
	# htmlwidgets::onRender(p, 
		# "function(el, x){
			# $(el).find('.legend .traces').on('mouseover', function(event) {
				# var el = this.closest('.js-plotly-plot'),
					# legend_traces = $(el).find('.legend .traces').toArray(),
					# to_hide = Array.from(Array(legend_traces.length + 1).keys()).splice(1),
					# to_show = legend_traces.indexOf(this);
				# to_hide.splice(to_show, 1);
				# Plotly.restyle(el, {opacity: 0.1}, to_hide);
			# });
			# $(el).find('.legend .traces').on('mouseout', function(event) {
				# var el = this.closest('.js-plotly-plot');
				# Plotly.restyle(el, {opacity: 1});
			# });
		# }"
	# )
}

#' @title Construct empty chromatogram
#'
#' @description
#' Construct an empty chromatogram
#'
#' @return plotly object
plot_empty_chromato <- function(title = "Extracted Ion Chromatogram(s)") {
	p <- plot_ly(
		type='scatter', 
		mode='markers'
	)
	p <- plotly::layout(p, 
		title = list(
			text = sprintf('<b>%s</b>', title),
			y = .95, 
			x = .5, 
			font = list(
				family = '"Open Sans",verdana,arial,sans-serif', 
				size = 18
			), 
			xanchor = "center", 
			yanchor = "bottom"
		), 
		margin = list(t = 50), 
		spikedistance = -1, 
		hovermode = "x unified",
		xaxis = list(
			title = 'Retention time', 
			titlefont = list(
				family = '"Open Sans",verdana,arial,sans-serif', 
				size = 18
			), 
			showspikes = TRUE, 
			spikemode = "across", 
			spikedash = "dash", 
			spikecolor = "#000000", 
			spikethickness = 1,
			ticksuffix = " min",
			showticksuffix = "all",
			hoverformat = '.2f'
		), 
		yaxis = list(
			exponentformat = 'e', 
			title = '',
			hoverformat = '.2e'
		), 
		hoverlabel = list(
			namelength = -1
		),
		selectdirection = "h", 
		annotations = list(list(
			xref = 'paper', 
			yref = 'paper', 
			x = 0, 
			y = 1, 
			xanchor = 'left', 
			yanchor = 'bottom', 
			text = 'Intensity', 
			showarrow = FALSE, 
			font = list(
				family = '"Open Sans",verdana,arial,sans-serif', 
				size = 18
			)
		))
	)
	p <- plotly::config(p, 
		responsive = TRUE, 
		displaylogo = FALSE, 
		scrollZoom = FALSE, 
		modeBarButtons = list(
			list(list(
					name = 'toImage', 
					title = 'Download plot as a png',
					icon = htmlwidgets::JS('Plotly.Icons.camera'),
					click = htmlwidgets::JS(sprintf("function(gd){Plotly.downloadImage(gd, {format:'png', width:1200, height:400, filename:'Chromatogram'})}"))
			)),
			list('zoom2d', 'autoScale2d'), 
			list(list(
				name = 'resetView', 
				title = 'Reset legend', 
				icon = htmlwidgets::JS("Plotly.Icons.undo"),
				click = htmlwidgets::JS(sprintf("function(gd){Plotly.restyle(gd, 'visible', true);}")))
			)
		)
	)
	p
}

#' @title Plot TIC
#'
#' @description
#' Plot TIC of multiple files with plotly pkg
#' If no project or no files available return an empty plot with 
#'       function `plot_empty_chromato`
#' Will plot all TICs of files in a project if given or only files according the `project_samples` parameter
#'
#' @param db sqlite connection
#' @param project integer, project ID
#' @param project_samples vector of integers, project_sample IDs
#' @param title string, title of the plot
#'
#' @return plotly object
plot_TIC <- function(db, project = NULL, project_samples = NULL, 
		title = "Total Ion Chromatogram(s)"){
	datas <- get_tics(db, project, project_samples)
	p <- plot_empty_chromato(
		title = if (length(unique(datas$sample_name)) > 1) "Total Ion Chromatograms"
			 else "Total Ion Chromatogram")
	if (nrow(datas) == 0) return(p)
	
	plotly::toWebGL(plotly::add_trace(p, 
		mode = "lines+markers", 
		data = datas 	, 
		x = ~rt, 
		y = ~int, 
		name = ~sample_name, 
		color = ~sample_name, 
		marker = list(
			opacity = 1, 
			size = 1*10**-9
		)
	))
}

#' @title Plot EIC
#'
#' @description
#' Plot EIC of multiple files with plotly pkg
#' If no project or no files available return an empty plot with 
#'       function `plot_empty_chromato`
#' Draw a dashed line for non integrated data & continuous for integrated
#' Will plot all EICs of files in a project if given or only files according the `project_samples` parameter
#'
#' @param db sqlite connection
#' @param project_samples vector of integers, project_sample IDs
#' @param mz float, m/z
#' @param mda float, m/z tolerance in mDa
#'
#' @return plotly object
plot_EIC <- function(db, project = NULL, project_samples = NULL, 
		mz = NULL, ppm = 0, mda = 0) {
	datas <- get_eics(db, project, project_samples, mz, ppm, mda)
	p <- plot_empty_chromato("EIC")
	if (nrow(datas) == 0) return(p)
	
	p <- plotly::add_trace(p, 
		mode = "lines+markers", 
		data = datas, 
		x = ~rt, 
		y = ~int, 
		legendgroup = ~sample_name, 
		name = ~sample_name, 
		marker = list(
			opacity = 1, 
			size = 1*10**-9
		),
		line = list(
			color = 'rgb(0,0,0)', 
			width = 1, 
			dash = 'dash'
		),
		showlegend = FALSE
	)
	plotly::toWebGL(p)
}

#' @title Plot EIC for chemical
#'
#' @description
#' Plot EIC for each isotopologue of a chemical
#' If some chemical were integrated it will color the area integrated
#'
#' @param db sqlite connection
#' @param project_sample integer project_sample IDs
#' @param adduct string adduct name
#' @param chemical_type string type of chemical studied
#' @param C integer number of carbon of chemical
#' @param Cl integer number of chlore of chemical
#' @param ppm float m/z tolerance in ppm
#' @param mda float m/z tolerance in mDa
#' @param resolution list with items:
#' \itemize{
#' 		\item resolution float, resolution of instrument if Orbitrap
#' 		\item mz float, resolution@mz if Orbitrap
#'		\item index integer, index of the instrument in the enviPat resolution_list
#' }
#' 
#' @return plotly object
plot_chemical_EIC <- function(db, project_sample = NULL, 
		adduct = NULL, chemical_type = NULL, C = 0, Cl = 0, ppm = 0, mda = 0, resolution = NULL, retention_time = NULL) {
	p <- plot_empty_chromato("EIC")
	
	chemical_ion <- get_chemical_ion(db, adduct, chemical_type, C, Cl)
	if (nrow(chemical_ion) == 0) return(p)
	
	theoric_pattern <- get_theoric(chemical_ion$ion_formula, 
		chemical_ion$charge, resolution)[[1]]
	if (nrow(theoric_pattern) == 0) return(p)
	# now get eic data
	datas <- get_eics(db, project = NULL, project_sample,  
		theoric_pattern[, "mz"], ppm, mda)
	if (nrow(datas) == 0) return(p)
	# add iso column on datas
	datas <- merge(datas, theoric_pattern[, c("mz", "iso")], 
		by = "mz", all.x = TRUE)
	p <- plotly::add_trace(p, 
		mode = "lines", 
		data = datas, 
		x = ~rt, 
		y = ~int, 
		legendgroup = ~iso, 
		name = ~iso, 
		line = list(
			color = 'rgb(0,0,0)', 
			width = 1, 
			dash = 'dash'
		),
		showlegend = FALSE
	)
	
	# get integrated data 
	features <- get_chemical_features(db, project_sample, 
		chemical_ion$chemical_ion)
	if (nrow(features) == 0) return(plotly::toWebGL(p))
	# now color only between the rt range where integrated
	for (i in seq(nrow(features))) p <- plotly::add_trace(p, 
		mode = "lines", 
		data = datas[which(
			datas$rt >= features[i, "rtmin"] & 
			datas$rt <= features[i, "rtmax"] & 
			datas$iso == features[i, "iso"]), ], 
		x = ~rt, 
		y = ~int, 
		legendgroup = ~iso, 
		name = ~iso, 
		fill = "tozeroy", 
		showlegend = FALSE
	)
	p <- plotly::layout(p,
	  xaxis = list(range = retention_time))
	plotly::toWebGL(p)
}

#' @title Construct empty MS
#'
#' @description
#' Construct an empty MS
#'
#' @return plotly object
plot_empty_MS <- function(title = "Mass Spectra", yTitle = 'Intensity') {
	p <- plot_ly(
		type = 'scatter', 
		mode='markers'
	) 
	p <- plotly::layout(p,  
		title = list(
			text = sprintf('<b>%s</b>', title), 
			y = .95, 
			x = .5, 
			font = list(
				family = '"Open Sans",verdana,arial,sans-serif', 
				size = 18
			), 
			xanchor = "center", 
			yanchor = "bottom"
		), 
        margin = list(t = 50),
		hoverdistance = 10, 
		hovermode = "x",
		xaxis = list(
			title = 'm/z', 
			titlefont = list(
				family = '"Open Sans",verdana,arial,sans-serif', 
				size = 18
			), 
			showspikes = FALSE, 
			showticksuffix = "all",
			hoverformat = ".5f"
		), 
		yaxis = list(
			exponentformat = 'e', 
			title = '',
			hoverformat = '.2e'
		), 
		hoverlabel = list(
			namelength = -1
		), 
		annotations=list(list(
			xref = 'paper', 
			yref = 'paper', 
			x = 0, 
			y = 1, 
			xanchor = 'left', 
			yanchor = 'bottom', 
			text = yTitle, 
			showarrow = FALSE, 
			font = list(
				family = '"Open Sans",verdana,arial,sans-serif', 
				size = 18
			)
		))
	)
	p <- plotly::config(p,  
		responsive = TRUE, 
		scrollZoom = FALSE, 
		displaylogo = FALSE, 
		edits = list(
			annotationTail = TRUE
		), 
		modeBarButtons = list(
			list(list(
				name = 'toImage', 
				title = 'Download plot as a png',
				icon = htmlwidgets::JS('Plotly.Icons.camera'),
				click = htmlwidgets::JS("function(gd){Plotly.downloadImage(gd, {format:'png', width:1200, height:400, filename:'MS'})}")
			)), 
			list('zoom2d', 'autoScale2d')
		)
	)
	p
}

#' @title Plot MS
#'
#' @description
#' Plot MS of multiple files with plotly pkg at a specific time retention
#' If no project or no files available return an empty plot with 
#'       function `plot_empty_ms`
#'
#' @param db sqlite connection
#' @param project integer, project ID
#' @param project_samples vector of integers, project_sample IDs
#' @param rt float, time retention in minutes
#'
#' @return plotly object
plot_MS <- function(db, project = NULL, project_samples = NULL, rt) {
	datas <- get_mss(db, project, project_samples, rt)
	p <- plot_empty_MS(
		title = if (length(unique(datas$sample_name)) > 1) sprintf(
				"Mass spectra (%s min)", round(rt, 2))
		else sprintf("Mass spectrum (%s min)", round(rt, 2)))
	if (nrow(datas) == 0) return(p)
	
	p <- plotly::add_segments(p, 
		data = datas, 
		x = ~mz, 
		xend = ~mz, 
		y = 0, 
		yend = ~int, 
		legendgroup = ~sample_name, 
		name = ~sample_name, 
		color = I("black"),
		showlegend = FALSE
	)
	plotly::layout(p, 
		xaxis = list(
			range = c(
				min(datas$mz) - 1, 
				max(datas$mz) + 1
			)
		)
	)
}

#' @title Plot MS for chemical
#'
#' @description
#' Plot MS of a chemical in mirror mode:
#' above the observed (features integrated) & below the theoretical
#'
#' @param db sqlite connection
#' @param project_sample integer project_sample IDs
#' @param adduct string adduct name
#' @param chemical_type string type of chemical studied
#' @param C integer number of carbon of chemical
#' @param Cl integer number of chlore of chemical
#' @param resolution list with items:
#' \itemize{
#' 		\item resolution float, resolution of instrument if Orbitrap
#' 		\item mz float, resolution@mz if Orbitrap
#'		\item index integer, index of the instrument in the enviPat resolution_list
#' }
#' 
#' @return plotly object
plot_chemical_MS <- function(db, project_sample = NULL, 
		adduct = NULL, chemical_type = NULL, C = 0, Cl = 0, resolution = NULL) {
	p <- plot_empty_MS(yTitle = "Abundance")
	chemical_ion <- get_chemical_ion(db, adduct, chemical_type, C, Cl)
	if (nrow(chemical_ion) == 0) return(p)
	
	theoric_pattern <- get_theoric(chemical_ion$ion_formula, 
		chemical_ion$charge, resolution)[[1]]
	if (nrow(theoric_pattern) == 0) return(p)
	theoric_pattern$abundance <- -theoric_pattern$abundance
	p <- plotly::add_segments(p, 
		data = theoric_pattern, 
		x = ~mz, 
		xend = ~mz, 
		y = 0, 
		yend = ~abundance, 
		name = "theoric", 
		color = I("red"),
		showlegend = FALSE
	)
	p <- plotly::layout(p, 
		xaxis = list(
			range = c(
				min(theoric_pattern$mz) - 1, 
				max(theoric_pattern$mz) + 1
			)
		)
	)
	
	# now get the features integrated
	data <- get_chemical_features(db, project_sample, 
		chemical_ion$chemical_ion)
	if (nrow(data) == 0) return(p)
	p <- plotly::add_segments(p, 
		data = data, 
		x = ~mz, 
		xend = ~mz, 
		y = 0, 
		yend = ~abundance, 
		name = "observed", 
		color = I("green"),
		showlegend = FALSE
	)
}