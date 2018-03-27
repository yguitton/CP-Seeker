output$uiTargetFile <- renderUI({
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select distinct(sample) from observed where sample in (select sample from sample where project == "%s");',
		input$selectProject)
	files <- dbGetQuery(db, query)[, 1]
	dbDisconnect(db)
	pickerInput("targetFile", "Select file", choices=files, multiple=FALSE)
})

output$uiTargetAdduct <- renderUI({
	db <- dbConnect(SQLite(), sqlitePath)
	choices <- if(is.null(input$targetFile)) c()
		else if(input$targetFile == '') c()
		else dbGetQuery(db, 
			sprintf('select distinct(adduct) from molecule where id in ( select molecule from observed where sample == "%s");',
				input$targetFile))$adduct
	dbDisconnect(db)
	pickerInput('targetAdduct', 'Select adduct', choices=choices)
})

computeSumAUC <- function(file, adduct, C, Cl){
	if(is.null(C) | is.null(Cl) | is.null(file) | is.null(adduct)) return(0)
	if(file == '' | adduct == '') return(0)
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select formula, sum(auc) from measured inner join observed on observed.id = measured.observed inner join molecule on molecule.id = observed.molecule where sample == "%s" and adduct == "%s" group by observed;',
		file, adduct)
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	if(nrow(data) == 0) return(0)
	data$C <- as.numeric(str_extract_all(str_extract_all(data$formula, 'C[[:digit:]]+'), '[[:digit:]]+'))
	data$Cl <- as.numeric(str_extract_all(str_extract_all(data$formula, 'Cl[[:digit:]]+'), '[[:digit:]]+'))
	res <- sum(data[which(data$C >= C[1] & data$C <= C[2] & data$Cl >= Cl[1] & data$Cl <= Cl[2]), 'sum(auc)'])
	return(formatC(res, format='e', digits=2))
}

output$targetSumAUC <- renderText({
	return(paste('Sum of AUC:', computeSumAUC(input$targetFile, input$targetAdduct, input$targetC, input$targetCl)))	
})

observeEvent(input$targetTable_cell_selected, {
	session$sendCustomMessage('targetTableSelectPpm', list(row=input$targetTable_cell_selected$C-8, column=input$targetTable_cell_selected$Cl-3))
	session$sendCustomMessage('targetTableSelectScore', list(row=input$targetTable_cell_selected$C-8, column=input$targetTable_cell_selected$Cl-3))
	session$sendCustomMessage('targetTableSelectAUC', list(row=input$targetTable_cell_selected$C-8, column=input$targetTable_cell_selected$Cl-3))
})

output$targetDownload <- downloadHandler(
	filename = 'targetROI.xlsx',
	content = function(file){
		wb <- createWorkbook()
		addWorksheet(wb=wb, sheetName='ppmDeviation', gridLines=FALSE)
		addWorksheet(wb=wb, sheetName='Score', gridLines=FALSE)
		addWorksheet(wb=wb, sheetName='AUC', gridLines=FALSE)
		data <- targetTableFunctionPpmDeviation(input$targetFile, input$targetAdduct)
		writeDataTable(wb, sheet=1, x=data, rowNames=TRUE)
		data <- targetTableFunctionScore(input$targetFile, input$targetAdduct)
		writeDataTable(wb, sheet=2, x=data, rowNames=TRUE)
		writeData(wb, sheet=3, x=paste('Sum of AUC:', computeSumAUC(input$targetFile, input$targetAdduct, input$targetC, input$targetCl)))
		writeData(wb, sheet=3, x=paste('   by the filters C:', input$targetC[1], '-', input$targetC[2], ',', 'Cl:', input$targetCl[1], '-', input$targetCl[2]), startRow=2)
		data <- targetTableFunctionInto(input$targetFile, input$targetAdduct)
		writeDataTable(wb, sheet=3, x=data, rowNames=TRUE, startRow=3)
		saveWorkbook(wb, file, overwrite=TRUE)
	}
)

output$targetEIC <- renderPlotly({
	actualize$graph
	eic <- plot_ly(source='targetEIC', type='scatter', mode='markers') %>% 
		layout(xaxis=list(title='retentionTime'), yaxis=list(title='Intensity', rangemode='tozero'), showlegend=TRUE) %>% 
		config(scrollZoom=TRUE, showLink=TRUE, displaylogo=FALSE, 
			modeBarButtons=list(list(list(
				name='toImage2', 
				title='Download plot as png', 
				icon=list(width = 1000, ascent = 850, descent = -150, path = icon_encoded), 
				click=htmlwidgets::JS(sprintf("function(gd){Plotly.downloadImage(gd, {format:'png', width:1920, height:1080, filename:'eic'})}"))
				)), list('zoom2d', 'select2d', 'pan2d', 'autoScale2d', 'resetScale2d')))
	cell <- input$targetTable_cell_selected
	if(is.null(cell)) return(eic)
	if(cell$C == 0) return(eic)
	db <- dbConnect(SQLite(), sqlitePath)
	xraw <- readMSData(samples()[which(samples()$sample == input$targetFile), 'path'], centroided=TRUE, msLevel=1, mode='onDisk')
	query <- sprintf('select mz from measured where observed == (select id from observed where sample == "%s" and molecule == (
		select id from molecule where adduct == "%s" and formula like "%s"));',
		input$targetFile, input$targetAdduct, paste('%C', cell$C, '%Cl', cell$Cl, '%', sep='', collapse=''))
	mzs <- dbGetQuery(db, query)$mz
	query <- sprintf('select * from observed where sample == "%s" and molecule == (select id from molecule where adduct == "%s" and formula like "%s");',
		input$targetFile, input$targetAdduct, paste('%C', cell$C, '%Cl', cell$Cl, '%', sep='', collapse=''))
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	mzRange <- matrix(sapply(mzs, function(mz) c(mz-(mz*data$tolPpm)/10^6, mz+(mz*data$tolPpm)/10^6)), ncol=2, byrow=TRUE)
	chrom <- chromatogram(xraw, mz=mzRange, rt=c(min(data$rtMin)*60-60, max(data$rtMax)*60+60), aggregationFun='sum', missing=0, BPPARAM=SnowParam())@.Data
	# get scan where the intensity is at the maximum
	i <- which(max(sapply(chrom, function(x) max(x@intensity))) == sapply(chrom, function(x) max(x@intensity)))
	points <- data.frame(rtime=chrom[[i]]@rtime, intensity=chrom[[i]]@intensity)
	points <- points[which(points$rtime >= data$rtMin*60 & points$rtime <= data$rtMax*60), ]
	eic <- eic %>% add_trace(x=points$rtime/60, y=points$intensity, size=I(1), fill='tozeroy', 
		showlegend=FALSE, hoverinfo='text', text=paste('Intensity: ', round(points$intensity, digits=0), 
			'<br />Retention Time: ', round(points$rtime/60, digits=2)))
	# traces
	for(i in 1:length(chrom)) eic <- eic %>% 
		add_trace(mode='lines+markers', size=I(1), x=chrom[[i]]@rtime/60, y=chrom[[i]]@intensity, size=I(1), 
			showlegend=FALSE, hoverinfo='text', text=paste('Intensity: ', round(chrom[[i]]@intensity, digits=0), 
				'<br />Retention Time: ', round(chrom[[i]]@rtime/60, digits=2)))
	# plotlyProxy('targetEIC', session) %>% 
		# plotlyProxyInvoke('restyle', list('marker.color', 'black', list(1:length(chrom)), list(1:60, (length(chrom[[1]]@rtime)-60):length(chrom[[1]]@rtime))))
	updateNumericInput(session, 'targetPpm', 'tol mz (ppm)', value=data$tolPpm, min=0, max=50)
	updateNumericInput(session, 'targetRtMin', 'rt min (min)', value=data$rtMin, min=0, max=25)
	updateNumericInput(session, 'targetRtMax', 'rt max (min)', value=data$rtMax, min=0, max=25)
	updateNumericInput(session, 'targetThreshold', 'threshold', value=data$threshold, min=0, max=25)
	updateNumericInput(session, 'targetTolAbd', 'tol abd (%)', value=data$tolAbd, min=0, max=25)
	return(eic)
})

observeEvent(event_data(event='plotly_selected', source='targetEIC'), {
	points <- event_data(event='plotly_selected', source='targetEIC')
	if(is.null(points)) return()
	if(nrow(points) == 0) return()
	rtmin <- min(points$x)
	rtmax <- max(points$x)
	updateNumericInput(session, 'targetRtMin', value=round(rtmin, digits=2), min=0, max=25)
	updateNumericInput(session, 'targetRtMax', value=round(rtmax, digits=2), min=0, max=25)
})

observeEvent(input$targetSubmit, {
	if(input$targetTable_cell_selected$C == 0) return(sendSweetAlert(session, title='You have to select a cell!', type='error'))
	hide('app-content')
	shinyjs::show('loader')
	cell <- input$targetTable_cell_selected
	deleteTargetROI(input$targetFile, input$targetAdduct, cell$C, cell$Cl)
	db <- dbConnect(SQLite(), sqlitePath)
	xraw <- readMSData(samples()[which(samples()$sample == input$targetFile), 'path'], centroided=TRUE, msLevel=1, mode='onDisk')
	query <- sprintf('select id from molecule where adduct == "%s" and formula like "%s";',
		input$targetAdduct, paste('%C', cell$C, '%Cl', cell$Cl, '%', sep='', collapse=''))
	molecule <- dbGetQuery(db, query)$id	
	query <- sprintf('select mz, abd from theoric where molecule == %s order by abd desc;',
		molecule)
	data <- dbGetQuery(db, query)	
	recordTarget(db, input$targetFile, xraw, molecule, data, c(input$targetRtMin*60, input$targetRtMax*60), input$targetPpm, input$targetTolAbd, input$targetThreshold)
	res <- dbGetQuery(db, 
		sprintf('select ppmDeviation, score, sum(auc) as sumOfAUC from measured inner join observed on observed.id = measured.observed 
		where molecule == %s and sample == "%s" group by observed;',
		molecule, input$targetFile))
	dbDisconnect(db)
	actualize$graph <- if(actualize$graph) FALSE else TRUE
	hide('loader')
	shinyjs::show('app-content')
	print(list(row=cell$C-8, column=cell$Cl-3, ppm=res$ppmDeviation, score=res$score, auc=res$sumOfAuc))
	session$sendCustomMessage('targetTableSelect', list(row=cell$C-8, column=cell$Cl-3, ppm=res$ppmDeviation, score=res$score, auc=res$sumOfAuc))
})

deleteTargetROI <- function(file, adduct, C, Cl){
	db <- dbConnect(SQLite(), sqlitePath)
	queries <- c(
		sprintf('delete from measured where observed == (select id from observed where sample == "%s" and molecule == (select id from molecule where adduct == "%s" and formula like "%s"));',
		file, adduct, paste('%C', C, '%Cl', Cl, '%', sep='', collapse='')),
		sprintf('delete from observed where sample == "%s" and molecule == (select id from molecule where adduct == "%s" and formula like "%s");',
		file, adduct, paste('%C', C, '%Cl', Cl, '%', sep='', collapse='')))
	dbSendQueries(db, queries)
	dbDisconnect(db)
}

observeEvent(input$targetDelete, {
	if(input$targetTable_cell_selected$C == 0) return(sendSweetAlert(session, title='You have to select a cell!', type='error'))
	cell <- input$targetTable_cell_selected
	deleteTargetROI(input$targetFile, input$targetAdduct, cell$C, cell$Cl)
	actualize$graph <- if(actualize$graph) FALSE else TRUE
})