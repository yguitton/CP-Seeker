#the files
output$uiTargetFullSelectFiles <- renderUI({
	selectInput("targetFullFiles", "Select file(s)", choices=samples()[which(samples()$project == input$selectProject), 'sample'], multiple=TRUE)
})

#the Target event
observeEvent(input$targetFullSubmit, {
	if(length(input$targetFullFiles) == 0) return(sendSweetAlert(session, title='You have to choose at least one file!', type='error'))
	hide('app-content')
	shinyjs::show('loader')
	db <- dbConnect(SQLite(), sqlitePath)
	# create the param for each file or update it
	query <- sprintf('select sample from param where sample in (%s) and adduct == "%s";', 
		paste('"', input$targetFullFiles, '"', collapse=', ', sep=''), input$targetFullAdduct)
	toUpdate <- dbGetQuery(db, query)$sample
	sapply(toUpdate, function(x) dbSendQuery(db, sprintf(
		'update param set ppmTol = %s, rtMin = %s, rtMax = %s, threshold = %s, abdTol = %s where sample == "%s" and adduct == "%s";',
		input$targetFullPpm, input$targetFullRt[1], input$targetFullRt[2], input$targetFullThreshold, input$targetFullTolAbd, x, input$targetFullAdduct)))
	sapply(input$targetFullFiles[which(!input$targetFullFiles %in% toUpdate)], function(x) dbSendQuery(db, sprintf(
		'insert into param (ppmTol, rtMin, rtMax, threshold, abdTol, sample, adduct) values (%s, %s, %s, %s, %s, "%s", "%s");',
		input$targetFullPpm, input$targetFullRt[1], input$targetFullRt[2], input$targetFullThreshold, input$targetFullTolAbd, x, input$targetFullAdduct)))
	queries <- c(
		sprintf('delete from measured where observed in (select id from observed where sample in (%s) and molecule in (select id from molecule where adduct == "%s"));',
			paste('"', input$targetFullFiles, '"', sep='', collapse=', '), input$targetFullAdduct),
		sprintf('delete from observed where sample in (%s) and molecule in (select id from molecule where adduct == "%s");',
			paste('"', input$targetFullFiles, '"', sep='', collapse=', '), input$targetFullAdduct))
	dbSendQueries(db, queries)	
	query <- sprintf('select id from molecule where adduct == "%s";',
		input$targetFullAdduct)
	molecules <- dbGetQuery(db, query)$id
	query <- sprintf('select mz, abd, molecule from theoric where molecule in (select molecule from molecule where adduct == "%s");',
		input$targetFullAdduct)
	theoric <- dbGetQuery(db, query)
	xraws <- sapply(samples()[which(samples()$sample %in% input$targetFullFiles), 'path'], function(file) 
		readMSData(file, centroided=TRUE, msLevel=1, mode='onDisk'))
	withProgress(message='target chloroparaffins', value=0, max=length(molecules), {
	for(molecule in molecules){
		data <- theoric[which(theoric$molecule == molecule), c('mz', 'abd')]
		sapply(1:length(xraws), function(x) 
			recordTarget(db=db, sample=input$targetFullFiles[x], xraw=xraws[[x]], 
				molecule=molecule, data=data[order(data$abd, decreasing=TRUE), ], rt=input$targetFullRt*60, 
				tolPpm=input$targetFullPpm, tolAbd=input$targetFullTolAbd, 
				threshold=input$targetFullThreshold))
		incProgress(amount=1)
	}
	})
	dbDisconnect(db)
	actualize$graph <- if(actualize$graph) FALSE else TRUE
	hide('loader')
	shinyjs::show('app-content')
})

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
	if(is.null(input$targetFullFiles)) return(graph)
	if(length(input$targetFullFiles) == 0) return(graph)
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select formula, sum(auc), adduct from measured inner join observed on observed.id = measured.observed inner join molecule on molecule.id = observed.molecule group by observed having sample in (%s);',
		paste('"', input$targetFullFiles, '"', sep='', collapse=', '))
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	if(nrow(data) == 0) return(graph)
	data$C <- as.numeric(str_extract_all(str_extract_all(data$formula, 'C[[:digit:]]+'), '[[:digit:]]+'))
	data$Cl <- as.numeric(str_extract_all(str_extract_all(data$formula, 'Cl[[:digit:]]+'), '[[:digit:]]+'))
	for(adduct in unique(data$adduct)) graph <- graph %>% 
		add_trace(x=data[which(data$adduct == adduct), 'Cl'], y=data[which(data$adduct == adduct), 'C'], 
			z=data[which(data$adduct == adduct), 'sum(auc)']*0.5, size=I(1), name=adduct,
			error_z=list(thickness=0, symmetric=TRUE, type='data', array=data[which(data$adduct == adduct), 'sum(auc)']*0.5), 
				hoverinfo='text', text=paste0('Adduct: ', adduct, '<br />C: ', data[which(data$adduct == adduct), 'C'], 
				'<br />Cl: ', data[which(data$adduct == adduct), 'Cl'], 
				'<br />Intensity :', formatC(data[which(data$adduct == adduct), 'sum(auc)'], format='e', digits=2)))
	return(graph)
})
