plotEmptyChromato <- function(title = "TIC"){
	plot_ly(type='scatter', mode='markers') %>% 
		layout(title=list(text = sprintf('<b>%s</b>', title), y = 1, x = 1, 
			font = list(family='"Open Sans",verdana,arial,sans-serif', size=18)), 
			xaxis=list(title='Time', titlefont=list(family='"Open Sans",verdana,arial,sans-serif', size=18)), 
			yaxis=list(exponentformat='e', title=''), selectdirection="h", annotations=list(list(
				xref='paper', yref='paper', x=-0.05, y=1, xanchor='left', 
				yanchor='bottom', text='Intensity', showarrow=FALSE, 
				font=list(family='"Open Sans",verdana,arial,sans-serif', size=18)))) %>% 
		config(scrollZoom=TRUE, displaylogo=FALSE, 
			modeBarButtons=list(list(
				list(
					name='toImage', 
					title='Download plot as a png',
					icon=htmlwidgets::JS('Plotly.Icons.camera'),
					click=htmlwidgets::JS(sprintf("function(gd){Plotly.downloadImage(gd, {format:'png', width:1920, height:1080, filename:'%s'})}", title)))), 
				list('zoom2d', 'select2d', 'pan2d', 'autoScale2d', 'resetScale2d')))
}

plotChromato <- function(db, pj, title = "TIC", msFile = NULL){
	chromato <- plotEmptyChromato(title)
	
	if(is.null(msFile))	msFile <- loadMSFile(db, pj)
	if(!any(class(msFile) == "xcmsRaw")) stop(paste(msFile))
	
	points <- if(all(msFile@tic == 0)) data.frame(x = msFile@scantime/60, 
			y = rawEIC(msFile, mzrange = range(msFile@env$mz)))
		else data.frame(x=msFile@scantime/60, y=msFile@tic)
	 points %>% 
		dplyr::mutate(x = round(x,2)) %>% group_by(x) %>% 
		dplyr::summarise(y = median(y))
	rm(msFile)
	gc()
	
	#add the raw data
	chromato <- add_trace(chromato, mode = "lines+markers", data = points, 
			x = ~x, y = ~y, line=list(color='red'), name=title, hoverinfo='text', 
			text = ~paste('Intensity: ', formatC(y, format="e"), '<br />Retention Time: ', 
				round(x, digits=2)), marker=list(opacity=1, size=1*10**-9))
}

plotClusterEIC <- function(clusters, theoric, msFile = NULL){
	eic <- plotEmptyChromato("EIC")

	if(is.null(msFile)) return(eic)
	if(nrow(clusters) > 0) clusters <- clusters %>% mutate(rtmin = rtmin / 60, rtmax = rtmax / 60)
	
	data <- lapply(1:nrow(theoric), function(i) 
		rawEIC(msFile, mzrange = as.double(theoric[i, c('mzmin', 'mzmax')])) %>% 
			as.data.frame() %>% cbind(rt = msFile@scantime / 60))
	rm(msFile)
	gc()
	
	for(i in 1:length(data)){
		integratedScans <- if(nrow(clusters) == 0) c()
			else do.call(c, lapply(which(clusters$iso == theoric[i, 'theoricIso']), 
				function(j) clusters[j, 'lmin']:clusters[j, 'lmax'])) %>% unique %>% sort
		nonIntegrated <- data[[i]]
		if(length(integratedScans) > 0){
			integrated <- data[[i]]
			integrated[-integratedScans, 'intensity'] <- NA
			nonIntegrated[integratedScans, 'intensity'] <- NA
			
			eic <- eic %>% add_lines(data = integrated, x = ~rt, 
				y = ~intensity, legendgroup=toString(theoric[i, 'theoricIso']), 
				line = list(color=colors[i+1]),
				showlegend=FALSE, hoverinfo = "text", name = theoric[i, 'theoricIso'], 
				text = paste0("iso: ", theoric[i, 'theoricIso'], 
					"<br />mz: ", round(theoric[i, 'mz'], 5) , 
					"<br />rt: ", round(integrated$rt, 2), 
					"<br />intensity: ", prettyNum(round(integrated$intensity), 
						big.mark = " ")))
		}
		
		eic <- eic %>% add_lines(data = nonIntegrated, x = ~rt, 
				y = ~intensity, legendgroup=toString(theoric[i, 'theoricIso']), 
				line = list(color='rgb(0,0, 0)', width=1, dash = 'dash'), 
				showlegend=TRUE, hoverinfo = "text", name = theoric[i, 'theoricIso'], 
				text = paste0("iso: ", theoric[i, 'theoricIso'], 
					"<br />mz: ", round(theoric[i, 'mz'], 5) , 
					"<br />rt: ", round(nonIntegrated$rt, 2), 
					"<br />intensity: ", prettyNum(round(nonIntegrated$intensity), 
						big.mark = " ")))
	}
	eic
}

plotEmptyMS <- function(){
	plot_ly(type='scatter', mode='markers') %>% 
		layout(title=list(text = '<b>Mass Spectrum</b>', y = 1, x = 1, 
				font = list(family='"Open Sans",verdana,arial,sans-serif', size=18)), 
			xaxis=list(title='m/z', titlefont=list(family='"Open Sans",verdana,arial,sans-serif', size=18)), 
			yaxis=list(exponentformat='e', title=''), annotations=list(list(
				xref='paper', yref='paper', x=-0.05, y=1, xanchor='left', 
				yanchor='bottom', text='Intensity', showarrow=FALSE, 
				font=list(family='"Open Sans",verdana,arial,sans-serif', size=18)))) %>% 
		config(scrollZoom=TRUE, displaylogo=FALSE, 
			modeBarButtons=list(list(
				list(
					name='toImage', 
					title='Download plot as a png',
					icon=htmlwidgets::JS('Plotly.Icons.camera'),
					click=htmlwidgets::JS(sprintf("function(gd){Plotly.downloadImage(gd, {format:'png', width:1920, height:1080, filename:'MS'})}"))),
				list(
					name='resetView', 
					title='Reset legend', 
					icon=htmlwidgets::JS("Plotly.Icons.undo"),
					click=htmlwidgets::JS(sprintf("function(gd){ Plotly.restyle(gd, 'visible', true);}")))
				), 
				list('zoom2d', 'pan2d', 'autoScale2d', 'resetScale2d')))
}

plotMS <- function(clusters, theoric){
	massSpectrum <- plotEmptyMS()
	
	minMz <- min(c(clusters$mz, theoric$mz))
	maxMz <- max(c(clusters$mz, theoric$mz))
	
	for(clusterID in unique(clusters$cluster)) massSpectrum <- massSpectrum %>% add_segments(
		data = clusters %>% filter(cluster == clusterID), x = ~mz, xend = ~mz, 
		y = 0, yend = ~abundance, name = paste('Cluster', clusterID), hoverinfo = "text", 
		text = ~paste0('Cluster :', clusterID, '<br />mz: ', round(mz, 5), 
			'<br />into : ', prettyNum(round(into), big.mark=' '), 
			'<br />rt :', round(rt / 60, 2)))
	massSpectrum <- massSpectrum %>% add_segments(data = theoric, 
		x = ~mz, xend = ~mz, y = 0, yend = ~-abundance, name = 'theoretic', hoverinfo = 'text', 
		text = ~paste0('Theoretic', '<br />mz: ', round(mz, 5), 
			'<br />abundance: ', round(abundance), '%'))
	massSpectrum %>% layout(xaxis = list(range = c(minMz - 1, maxMz + 1)))
}

draw2D <- function(zones = NULL, xrange, yrange, zCut){
	p <- plot_ly(type="scatter", mode="lines")
	if(!is.null(zones)){
		# begin at 2 because first is the zone under zCut
		for(i in 2:length(zones)) p <- p %>% 
			add_lines(data = getEdges(zones[[i]], zCut), x=~x, y=~y, hoverinfo = "text", showlegend = FALSE, 
				text = ~paste("C:", round(x, precision), "<br />Cl:", round(y, precision)))
	}
	p %>%
		layout(xaxis=list(title="Number of Carbon", range=xrange), 
			yaxis=list(title="Number of Chlorine", range=yrange)) %>% 
		plotly::config(scrollZoom=TRUE, displaylogo=FALSE, modeBarButtons=list(
			list('zoom2d', 'pan2d', 'autoScale2d', 'resetScale2d')))
}


draw3D <- function(triangles=NULL, xrange, yrange, zCut = 0){
	p <- plot_ly()
	if(!is.null(triangles)){
		faces <- do.call(rbind, triangles)
		edges <- purrr::reduce(triangles, function(a, b) 
			a %>% rbind(data.frame(x = NA, y = NA, z = NA)) %>% 
				rbind(b[c(1:3, 1), ]), .init=data.frame())
		p <- p %>% add_trace(data=faces, x=~x, y=~y, z=~z, i=seq(1, nrow(faces), by=3)-1,
				j=seq(2, nrow(faces), by=3)-1, k=seq(3, nrow(faces), by=3)-1, hoverinfo="text", 
				text=~paste('C:', round(x, precision), '<br />Cl:', round(y, precision), 
					'<br />intensity:', formatC(z))) %>% 
			add_trace(mode='lines', color=I('black'), line=list(width=2), data=edges, x=~x, y=~y, z=~z, hoverinfo='none')
		if(zCut > 0) p <- p %>% add_trace(data = data.frame(
				x = rep(xrange, each = 2), y = rep(yrange, times = 2), z=rep(zCut, times=4)),
				x = ~x, y = ~y, z = ~z, i = c(0,3), j = c(1,1), k = c(2,2), 
				opacity = .5, hoverinfo = "none")
	}
	p %>% layout(showlegend=FALSE, scene=list(camera=list(eye=list(x=1.25, y=-1.25, z=1.25)), 
			zaxis=list(title="Intensity", rangemode='tozero'), 
			xaxis=list(title="Number of Carbon", range=xrange), 
			yaxis=list(title="Number of Chlorine", range=yrange))) %>% 
		plotly::config(modeBarButtons=list(list('toImage', 'zoom3d', 'pan3d', 'orbitRotation', 'tableRotation',
			'resetCameraDefault3d', 'resetCameraLastSave3d')), displaylogo=FALSE)
}
