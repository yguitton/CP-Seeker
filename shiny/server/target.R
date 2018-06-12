output$uiTargetFile <- renderUI({
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select distinct(sample) from observed where sample in (select sample from sample where project == "%s");',
		input$selectProject)
	files <- dbGetQuery(db, query)[, 1]
	dbDisconnect(db)
	pickerInput("targetFile", "Select file", choices=files, multiple=FALSE, options=list(`live-search`=TRUE))
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

observeEvent(c(input$targetFile, input$targetAdduct), {
	if(is.null(input$targetFile) | is.null(input$targetAdduct)) return()
	else if(input$targetFile == '' | input$targetAdduct == '') return()
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select * from param where sample == "%s" and adduct == "%s";', input$targetFile, input$targetAdduct)
	param <- dbGetQuery(db, query)
	dbDisconnect(db)
	updateNumericInput(session, 'targetPpm', 'Tol mz (ppm)', value=param$tolPpm, min=0, max=50)
	updateNumericInput(session, 'targetPrefilterL', 'Prefilter level', value=param$prefilterL)
	updateNumericInput(session, 'targetPrefilterS', 'Prefilter step', value=param$prefilterS)
	updateNumericInput(session, 'targetTolAbd', 'tol abd (%)', value=param$tolAbd, min=0, max=100)
	runjs("Shiny.onInputChange('targetTable_cell_selected', {C:0, Cl:0});")
})


observeEvent(input$targetTable_cell_selected, {
	if(is.null(input$targetTable_cell_selected)) return()
	else if(input$targetTable_cell_selected$C == 0) return()
	print(input$targetTable_cell_selected)
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select * from observed where sample == "%s" and molecule == 
				(select id from molecule where adduct == "%s" and formula like "%s");', 
			input$targetFile, input$targetAdduct, paste('C', input$targetTable_cell_selected$C, '%Cl',input$targetTable_cell_selected$Cl, sep='', collapse=''))
	param <- dbGetQuery(db, query)
	dbDisconnect(db)
	print(param)
	updateNumericInput(session, 'targetPpm', 'Tol mz (ppm)', value=param$tolPpm, min=0, max=50)
	updateNumericInput(session, 'targetPrefilterL', 'Prefilter level', value=param$prefilterL)
	updateNumericInput(session, 'targetPrefilterS', 'Prefilter step', value=param$prefilterS)
	updateNumericInput(session, 'targetTolAbd', 'tol abd (%)', value=param$tolAbd, min=0, max=100)
	if(input$targetTable_cell_selected$C != 0){
		session$sendCustomMessage('targetTableSelectPpm', list(row=input$targetTable_cell_selected$C-8, column=input$targetTable_cell_selected$Cl-3))
		session$sendCustomMessage('targetTableSelectScore', list(row=input$targetTable_cell_selected$C-8, column=input$targetTable_cell_selected$Cl-3))
		session$sendCustomMessage('targetTableSelectAUC', list(row=input$targetTable_cell_selected$C-8, column=input$targetTable_cell_selected$Cl-3))
	}
})

output$targetDownload <- downloadHandler(
	filename = 'targetROI.xlsx',
	content = function(file){
		wb <- createWorkbook()
		addWorksheet(wb=wb, sheetName='ppmDeviation', gridLines=FALSE)
		addWorksheet(wb=wb, sheetName='Score', gridLines=FALSE)
		addWorksheet(wb=wb, sheetName='AUC', gridLines=FALSE)
		addWorksheet(wb, sheetName='param', gridLines=FALSE)
		data <- targetTableFunctionPpmDeviation(input$targetFile, input$targetAdduct)
		writeDataTable(wb, sheet=1, x=data, rowNames=TRUE)
		addStyle(wb, sheet=1, rows=which(data <= input$targetPpm, arr.ind=TRUE)[, 'row']+1, cols=which(data <= input$targetPpm, arr.ind=TRUE)[, 'col']+1, style=createStyle(fgFill='green'))
		addStyle(wb, sheet=1, rows=which(data > input$targetPpm, arr.ind=TRUE)[, 'row']+1, cols=which(data > input$targetPpm, arr.ind=TRUE)[, 'col']+1, style=createStyle(fgFill='red'))
		data <- targetTableFunctionScore(input$targetFile, input$targetAdduct)
		writeDataTable(wb, sheet=2, x=data, rowNames=TRUE)
		addStyle(wb, sheet=2, rows=which(data >= 100-input$targetTolAbd, arr.ind=TRUE)[, 'row']+1, cols=which(data >= 100-input$targetTolAbd, arr.ind=TRUE)[, 'col']+1, style=createStyle(fgFill='green'))
		addStyle(wb, sheet=2, rows=which(data < 100-input$targetTolAbd, arr.ind=TRUE)[, 'row']+1, cols=which(data < 100-input$targetTolAbd, arr.ind=TRUE)[, 'col']+1, style=createStyle(fgFill='red'))
		writeData(wb, sheet=3, x=paste('Sum of AUC:', computeSumAUC(input$targetFile, input$targetAdduct, input$targetC, input$targetCl)))
		writeData(wb, sheet=3, x=paste('   by the filters C:', input$targetC[1], '-', input$targetC[2], ',', 'Cl:', input$targetCl[1], '-', input$targetCl[2]), startRow=2)
		data <- targetTableFunctionInto(input$targetFile, input$targetAdduct)
		writeDataTable(wb, sheet=3, x=data, rowNames=TRUE, startRow=3)
		db <- dbConnect(SQLite(), sqlitePath)
		data <- dbGetQuery(db, sprintf('select * from param where sample == "%s" and adduct == "%s";', input$targetFile, input$targetAdduct))
		writeDataTable(wb, sheet=4, x=data, rowNames=FALSE)
		saveWorkbook(wb, file, overwrite=TRUE)
	}
)

output$targetEIC <- renderPlotly({
	actualize$graph
	eic <- plot_ly(source='targetEIC', type='scatter', mode='lines') %>% 
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
	else if(cell$C == 0) return(eic)
	db <- dbConnect(SQLite(), sqlitePath)
	xraw <- readMSData(samples()[which(samples()$sample == input$targetFile), 'path'], centroided=TRUE, msLevel=1, mode='onDisk')
	query <- sprintf('select mz, rtmin, rtmax from measured where observed == (select id from observed where sample == "%s" and molecule == (
		select id from molecule where adduct == "%s" and formula like "%s"));',
		input$targetFile, input$targetAdduct, paste('%C', cell$C, '%Cl', cell$Cl, '%', sep='', collapse=''))
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	mzRange <- matrix(sapply(data$mz, function(mz) c(mz-(mz*input$targetPpm)/10**6, mz+(mz*input$targetPpm)/10**6)), ncol=2, byrow=TRUE)
	chrom <- chromatogram(xraw, mz=rbind(mzRange[1, ], mzRange), 
		rt=rbind(c(rtmin=0, rtmax=max(rtime(xraw))), data[, c('rtmin', 'rtmax')]), 
		aggregationFun='sum', missing=0, BPPARAM=SnowParam())@.Data
	eic <- eic %>%
		add_trace(x=chrom[[1]]@rtime/60, y=chrom[[1]]@intensity, 
			showlegend=FALSE, hoverinfo='text', text=paste('Intensity: ', round(chrom[[1]]@intensity, digits=0), 
				'<br />Retention Time: ', round(chrom[[1]]@rtime/60, digits=2)), line=list(dash='dash', width=4, color='rgb(0,0,0)'))
	for(i in 2:length(chrom)) eic <- eic %>% 
		add_trace(x=chrom[[i]]@rtime/60, y=chrom[[i]]@intensity, 
			showlegend=FALSE, hoverinfo='text', text=paste('Intensity: ', round(chrom[[i]]@intensity, digits=0), 
				'<br />Retention Time: ', round(chrom[[i]]@rtime/60, digits=2)), line=list(width=3))
	eic <- eic %>% add_trace(mode='markers', x=chrom[[1]]@rtime/60, y=chrom[[1]]@intensity, showlegend=FALSE, marker=list(size=0.001)) %>%
		layout(xaxis=list(range=c(min(data$rtmin)/60-1, max(data$rtmax)/60+1))) 
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
	if(is.null(input$targetFile) | is.null(input$targetAdduct)) return(sendSweetAlert(session, title='Error', type='error'))
	else if(input$targetFile == '') return(sendSweetAlert(session, title='You have to select a file', type='error'))
	else if(input$targetAdduct == '') return(sendSweetAlert(session, title='You have to select an adduct!', type='error'))
	else if(input$targetTable_cell_selected$C == 0) return(sendSweetAlert(session, title='You have to select a cell!', type='error'))
	else if(input$targetRtMin == '' | input$targetRtMax == '' | input$targetRtMax <= input$targetRtMin) return(
		sendSweetAlert(session, title='You have to select a rt range!', type='error'))
	else if(input$targetPpm == '') return(sendSweetAlert(session, title='Ppm is incorrect!', type='error'))
	else if(input$targetPrefilterL == '') return(sendSweetAlert(session, title='Prefilter level is incorrect!', type='error'))
	else if(input$targetPrefilterS == '') return(sendSweetAlert(session, title='Prefilter step is incorrect!', type='error'))
	else if(input$targetTolAbd == '') return(sendSweetAlert(session, title='Tol abd is incorrect!', type='error'))
	hide('app-content')
	shinyjs::show('loader')
	tryCatch({
	cell <- input$targetTable_cell_selected
	deleteTargetROI(input$targetFile, input$targetAdduct, cell$C, cell$Cl)
	file <- readMSData(samples()[which(samples()$sample == input$targetFile), 'path'], centroided=TRUE, msLevel=1, mode='onDisk')
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select * from theoric inner join molecule on theoric.molecule = molecule.id 
		where adduct == "%s" and formula like "%s";',
			input$targetAdduct, paste('C', cell$C, '%Cl', cell$Cl, sep='', collapse=''))
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	tol <- data$mz * input$targetPpm /10**6
	mzRange <- data.frame(mzmin=data$mz-tol, mzmax=data$mz+tol)
	rtRange <- data.frame(rtmin=rep(input$targetRtMin*60, each=5), rtmax=rep(input$targetRtMax*60, each=5))
	chrom <- chromatogram(file, mz=mzRange, rt=rtRange, missing=0)
	res <- integrateROI(file, chrom, data$mz, input$targetPrefilterS, input$targetPrefilterL)
	if(nrow(res) < 2){
		session$sendCustomMessage('targetTablePpmDelete', list(row=cell$C-8, column=cell$Cl-3))
		session$sendCustomMessage('targetTableScoreDelete', list(row=cell$C-8, column=cell$Cl-3))
		session$sendCustomMessage('targetTableIntoDelete', list(row=cell$C-8, column=cell$Cl-3))
	} else{
		res$abd <- res$AUC*100/max(res$AUC)
		ppmDeviation <- mean(res$ppmDeviation)
		score <- computeScore(data$abd, res$abd, input$targetTolAbd)
		sumAUC <- sum(res$AUC)
		recordTarget(res, data[1, 'molecule'], input$targetPpm, 
			input$targetTolAbd, score, ppmDeviation, input$targetFile, input$targetPrefilterS, input$targetPrefilterL)
		print(list(row=cell$C-8, column=cell$Cl-3, ppm=ppmDeviation, score=score, auc=sumAUC))
		session$sendCustomMessage('targetTablePpmUpdate', list(row=cell$C-8, column=cell$Cl-3, ppm=round(ppmDeviation, digits=2)))
		session$sendCustomMessage('targetTableScoreUpdate', list(row=cell$C-8, column=cell$Cl-3, score=round(score, digits=2)))
		session$sendCustomMessage('targetTableIntoUpdate', list(row=cell$C-8, column=cell$Cl-3, into=formatC(sumAUC, format='e', digits=2)))
	}
	hide('loader')
	shinyjs::show('app-content')
	}, error=function(e){
		print(e)
		hide('loader')
		shinyjs::show('app-content')
		return(sendSweetAlert(session, title='Error on targeting', text=paste(e), type='error'))
	})
})

deleteTargetROI <- function(file, adduct, C, Cl){
	db <- dbConnect(SQLite(), sqlitePath)
	queries <- c(
		sprintf('delete from measured where observed == (
			select id from observed where sample == "%s" and molecule == (
				select id from molecule where adduct == "%s" and formula like "%s"));',
			file, adduct, paste('%C', C, '%Cl', Cl, '%', sep='', collapse='')),
		sprintf('delete from observed where sample == "%s" and molecule == (
			select id from molecule where adduct == "%s" and formula like "%s");',
			file, adduct, paste('%C', C, '%Cl', Cl, '%', sep='', collapse='')))
	dbSendQueries(db, queries)
	dbDisconnect(db)
}

observeEvent(input$targetDelete, {
	if(input$targetTable_cell_selected$C == 0) return(sendSweetAlert(session, title='You have to select a cell!', type='error'))
	cell <- input$targetTable_cell_selected
	deleteTargetROI(input$targetFile, input$targetAdduct, cell$C, cell$Cl)
	session$sendCustomMessage('targetTablePpmDelete', list(row=cell$C-8, column=cell$Cl-3))
	session$sendCustomMessage('targetTableScoreDelete', list(row=cell$C-8, column=cell$Cl-3))
	session$sendCustomMessage('targetTableIntoDelete', list(row=cell$C-8, column=cell$Cl-3))
})