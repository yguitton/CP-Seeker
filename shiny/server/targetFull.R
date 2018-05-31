#the files
output$uiTargetFullSelectFiles <- renderUI({
	pickerInput("targetFullFiles", "Select file(s)", choices=samples()[which(samples()$project == input$selectProject), 'sample'], multiple=TRUE,
		options=list(`actions-box`=TRUE, `live-search`=TRUE))
})

#the Target event
observeEvent(input$targetFullSubmit, {
	if(length(input$targetFullFiles) == 0) return(sendSweetAlert(session, title='You have to choose at least one file!', type='error'))
	else if(input$targetFullPrefilterL == '') return(sendSweetAlert(session, title='You need to specified the prefilter level', type="error"))
	else if(input$targetFullTolAbd == '') return(sendSweetAlert(session, title='You need to specified the abundance tolerance', type='error'))
	tryCatch({
		hide('app-content')
		show('loader')
		recordParametersTargetFull(input$targetFullFiles, input$targetFullAdduct, input$targetFullPpm, input$targetFullTolAbd,
			input$targetFullPrefilterS, input$targetFullPrefilterL)
		db <- dbConnect(SQLite(), sqlitePath)
		query <- sprintf('select * from theoric inner join molecule on theoric.molecule = molecule.id where adduct == "%s";',
			input$targetFullAdduct)
		data <- dbGetQuery(db, query)
		dbDisconnect(db)
		files <- readMSData(samples()[which(samples()$sample %in% input$targetFullFiles), 'path'], 
			centroided=TRUE, msLevel=1, mode='onDisk')
		targetROI(files, data, input$targetFullPpm, input$targetFullPrefilterS, input$targetFullPrefilterL, input$targetFullTolAbd)
		actualize$graph <- if(actualize$graph) FALSE else TRUE
		hide('loader')
		shinyjs::show('app-content')
	}, error=function(e){
		print(e)
		hide('loader')
		show('app-content')
		sendSweetAlert(session, title='Error targeting', text=paste(e), type='error')
	})
})

recordParametersTargetFull <- function(files, adduct, ppm, tolAbd, prefilterS, prefilterL){
	db <- dbConnect(SQLite(), sqlitePath)
	# create the param for each file or update it
	query <- sprintf('select sample from param where sample in (%s) and adduct == "%s";', 
		paste('"', files, '"', collapse=', ', sep=''), adduct)
	toUpdate <- dbGetQuery(db, query)$sample
	sapply(toUpdate, function(x) dbSendQuery(db, sprintf(
		'update param set tolPpm = %s, tolAbd = %s, prefilterS = %s, prefilterL = %s where sample == "%s" and adduct == "%s";',
		ppm, tolAbd, prefilterS, prefilterL, x, adduct)))
	sapply(input$targetFullFiles[which(!input$targetFullFiles %in% toUpdate)], function(x) dbSendQuery(db, sprintf(
		'insert into param (tolPpm, tolAbd, sample, adduct, prefilterS, prefilterL) values (%s, %s, "%s", "%s", %s, %s);',
		ppm, tolAbd, x, adduct, prefilterS, prefilterL)))
	queries <- c(
		sprintf('delete from measured where observed in (select id from observed where sample in (%s) 
			and molecule in (select id from molecule where adduct == "%s"));',
			paste('"', files, '"', sep='', collapse=', '), adduct),
		sprintf('delete from observed where sample in (%s) and molecule in (select id from molecule where adduct == "%s");',
			paste('"', files, '"', sep='', collapse=', '), adduct))
	dbSendQueries(db, queries)
	dbDisconnect(db)
}

output$targetFullGraph <- renderPlotly({
	actualize$graph
	graph <- plot_ly(type='scatter3d', mode='markers') %>% 
		layout(scene=list(xaxis=list(title='Cl', autorange='reversed', rangemode='tozero'), yaxis=list(title='C', rangemode='tozero'), zaxis=list(title='Intensity')))%>% 
		config(editable=TRUE, scrollZoom=TRUE, showLink=TRUE, displaylogo=FALSE, 
			modeBarButtons=list(
				list(
					list(
						name='toImage2', 
						title='Download plot as png', 
						icon=list(width = 1000, ascent = 850, descent = -150, path = icon_encoded), 
						click=htmlwidgets::JS(sprintf("function(gd){Plotly.downloadImage(gd, {format:'png', width:1920, height:1080, filename:'chromato'})}"))
					)
				), list('zoom3d', 'pan3d', 'resetCameraDefault3d', 'orbitRotation', 'tableRotation')
			)
		)
	if(is.null(input$targetFullFiles) | is.null(input$targetFullAdduct)) return(graph)
	else if(length(input$targetFullFiles) == 0 | input$targetFullAdduct == '') return(graph)
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select sum(auc), formula from measured inner join observed on observed.id = measured.observed 
		inner join molecule on molecule.id = observed.molecule 
		where sample in (%s) and adduct == "%s" group by formula;',
		paste('"', input$targetFullFiles, '"', sep='', collapse=', '), input$targetFullAdduct)
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	if(nrow(data) == 0) return(graph)
	data$C <- sapply(strsplit(data$formula, 'H'), function(x) as.numeric(strsplit(x[1], 'C')[[1]][2]))
	data$Cl <- sapply(strsplit(data$formula, 'Cl'), function(x) as.numeric(x[2]))
	z <- matrix(0, nrow=36, ncol=30)
	for(row in 1:nrow(data)){
		z[data[row, 'C'], data[row, 'Cl']] <- data[row, 'sum(auc)']
	}
	graph <- plot_ly(x=0:36, y=0:30, z=z) %>% add_surface() %>%
		layout(scene=list(xaxis=list(title='Cl', autorange='reversed', rangemode='tozero'), yaxis=list(title='C', rangemode='tozero'), zaxis=list(title='Intensity')))
})
