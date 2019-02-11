updateOutput$details <- FALSE
updateOutput$detailsEic <- FALSE

output$uiDetailsSample <- renderUI({
	updateOutput$details
	if(is.null(input$project)) samples <- c()
	else if(input$project ==  "") samples <- c()
	else {
		db <- dbConnect(SQLite(), sqlitePath)
		query <- sprintf('select sample from project_sample where project == "%s";', 
			input$project)
		print(query)
		samples <- dbGetQuery(db, query)$sample
		dbDisconnect(db)
	}
	pickerInput('detailsSample', 'sample', choices=samples, multiple=FALSE, options=list(`live-search`=TRUE))
})

output$uiDetailsAdduct <- renderUI({
	updateOutput$details
	if(is.null(input$detailsSample)) choices <- c()
	else if(input$detailsSample == "") choices <- c()
	else {
		db <- dbConnect(SQLite(), sqlitePath)
		query <- sprintf('select adduct from project_sample where sample ==  "%s" and
			project == "%s";', input$detailsSample, input$project)
		print(query)
		choices <- dbGetQuery(db, query)$adduct
		dbDisconnect(db)		
	}
	pickerInput('detailsAdduct', 'adduct', choices=choices, multiple=FALSE)
})

output$detailsTable <- renderDataTable({
	print('------------------- DETAILS TABLE ------------------')
	print(list(project=input$project, sample=input$detailsSample, 
		adduct=input$detailsAdduct))
	updateOutput$details
	data <- tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project not initialized')
		else if(input$project == '') custom_stop('invalid', 'no project created')
		else if(is.null(input$detailsSample)) custom_stop('invalid', 'sample not initialized')
		else if(input$detailsSample == "") custom_stop('invalid', 'no sample processed in project')
		else if(is.null(input$detailsAdduct)) custom_stop('invalid', 'adduct not initialized')
		else if(input$detailsAdduct == "") custom_stop('invalid', 'no adduct in project_sample???')
		
		db <- dbConnect(SQLite(), sqlitePath)
		query <- sprintf('select * from observed where project_sample == (select project_sample
			from project_sample where project == "%s" and sample == "%s" and adduct == "%s");',
			input$project, input$detailsSample, input$detailsAdduct)
		print(query)
		data <- dbGetQuery(db, query)
		dbDisconnect(db)
		data$abd_deviation <- round(data$abd_deviation)
		res <- matrix(NA, nrow=maxC-minC+1, ncol=maxCl-minCl+1)
		for(row in 1:nrow(data)) res[data[row, 'C']-minC+1, data[row, 'Cl']-minCl+1] <- data[row, 'abd_deviation']
		res
	}, invalid = function(i){
		print(i)
		matrix(NA, nrow=maxC-minC+1, ncol=maxCl-minCl+1)
	}, error = function(e){
		print(e$message)
		sendSweetAlert(e$message)
		matrix(NA, nrow=maxC-minC+1, ncol=maxCl-minCl+1)
	})
	colnames(data) <- paste0('Cl', minCl:maxCl)
	rownames(data) <- paste0('C', minC:maxC)
	print('------------------- END DETAILS TABLE ------------------')
	data
}, selection="none", extensions='Scroller', class='display cell-border compact nowrap', options=list(
	info=FALSE, paging=FALSE, dom='Bfrtip', scoller=TRUE, scrollX=TRUE, scrollY=input$dimension[2]/1.6, bFilter=FALSE, ordering=FALSE,
	columnDefs=list(list(className='dt-body-center', targets="_all"))), callback = htmlwidgets::JS("
		Shiny.onInputChange('detailsTable_selected', {C: 0, Cl: 0});
		table.on('click', 'tbody td', function(){
			if(table.cell(this).data() != null){
				if($(this).hasClass('selected')){
					$(table.cells('.selected').nodes()).toggleClass('selected');
					Shiny.onInputChange('detailsTable_selected', {C: 0, Cl: 0});
				} else {
					$(table.cells('.selected').nodes()).toggleClass('selected');
					$(this).toggleClass('selected');
					Shiny.onInputChange('detailsTable_selected', {
						C: table.cell(this).index().row+4, 
						Cl: table.cell(this).index().column+1});
				}
			}
		});
		Shiny.addCustomMessageHandler('updateDetailsTable', function(value){
			table.cell('.selected').data(value);
		})
		Shiny.addCustomMessageHandler('detailsTableErase', function(message){
			table.cell('.selected').data(null);
			$(table.cell('.selected').node()).toggleClass('selected');
			Shiny.onInputChange('detailsTable_selected', {C:0, Cl:0});
		})
	"))
	
output$detailsEic <- renderPlotly({
	print('------------------- DETAILS EIC -------------------')
	print(list(project=input$project, sample=input$detailsSample, 
		adduct=input$detailsAdduct, table_rows=input$detailsTable_selected))

	updateOutput$details
	updateOutput$detailsEic
	eicPlot <- tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project is not yet initialized')
		else if(is.null(input$detailsSample)) custom_stop('invalid', 'sample is not yet initialized')
		else if(is.null(input$detailsAdduct)) custom_stop('invalid', 'adduct is not yet initialized')
		else if(is.null(input$detailsTable_selected)) custom_stop('invalid', 'no cell clicked')	
		else if(input$project == "") custom_stop('invalid', 'no project selected')
		else if(input$detailsSample == "") custom_stop('invalid', 'no sample selected')
		else if(input$detailsAdduct == "") custom_stop('invalid', 'no adduct selected')
		else if(length(input$detailsTable_selected) == 0) custom_stop('invalid', 'no cell clicked')
		feedbackDanger('detailsTolPpm', !is.numeric(input$detailsTolPpm), 'tol ppm is not numeric')
		if(!is.numeric(input$detailsTolPpm)) custom_stop('invalid', 'tol ppm is not numeric')
		C <- input$detailsTable_selected$C
		Cl <- input$detailsTable_selected$Cl
		if(C == 0 | Cl == 0) custom_stop('invalid', 'no cell clicked')
		
		db <- dbConnect(SQLite(), sqlitePath)
		query <- sprintf('select path from sample where sample == "%s";', 
			input$detailsSample)
		print(query)
		path <- dbGetQuery(db, query)$path
		raw <- read.raw(path)
		rts <- raw$StartTime
		
		query <- sprintf('select * from observed where C == %s and Cl == %s and 
			project_sample == (
				select project_sample from project_sample where sample == "%s" and
					project == "%s" and adduct == "%s");',
			input$detailsTable_selected[1], input$detailsTable_selected[2], 
				input$detailsSample, input$project, input$detailsAdduct)
		print(query)
		data <- dbGetQuery(db, query)
		dbDisconnect(db)
		
		eics <- readXICs(path, masses=data$mz, tol=input$detailsTolPpm)
		
		eicPlot <- plot_ly(type="scatter", mode="lines")
		# for the first mass
		A <- data[which(round(data$mz) == round(eics[[1]]$mass)), ]
		data1 <- data.frame(x=rts, y=0)
		data1[which(data1$x %in% eics[[1]]$times), 'y'] <- eics[[1]]$intensities[which(eics[[1]]$times %in% data1$x)]
		eicPlot1 <- eicPlot %>% add_trace(mode="lines+markers", name=min(data$mz), legendgroup="group1", 
			data=data1, x=~x, y=~y, color=I('black'), showlegend=FALSE, marker=list(opacity=1, size=1*10**-9))
		scans1 <- lapply(1:nrow(A), function(x) 
			which(data1$x >= A[x, 'rtmin'] & data1$x <= A[x, 'rtmax']))
		for(i in 1:length(scans1)) eicPlot1 <- eicPlot1 %>% add_trace(mode='none', 
			data=data1[scans1[[i]], ], x=~x, y=~y, fill='tozeroy', showlegend=FALSE, legendgroup="group1")
		baseline1 <- runmed(data1$y, 1+2*(min(c((nrow(data1)-1)%/%2, ceiling(.15*nrow(data1))))), 
			endrule="median", algorithm="Turlach")
		eicPlot1 <- eicPlot1 %>% add_lines(x=data1$x, y=baseline1, color=I('red'), showlegend=FALSE, legendgroup="group1")
		
		# for the second mass
		A2 <- data[which(round(data$mz) == round(eics[[2]]$mass)), ]
		data2 <- data.frame(x=rts, y=0)
		data2[which(data2$x %in% eics[[2]]$times), 'y'] <- eics[[2]]$intensities[which(eics[[2]]$times %in% data2$x)]
		eicPlot2 <- eicPlot %>% add_trace(mode="lines+markers", name=max(data$mz), 
			data=data2, x=~x, y=~y, color=I('black'), legendgroup="group2", showlegend=FALSE, marker=list(opacity=1, size=1*10**-9))
		scans2 <- lapply(1:nrow(A2), function(x) 
			which(data2$x >= A2[x, 'rtmin'] & data2$x <= A2[x, 'rtmax']))
		for(i in 1:length(scans2)) eicPlot2 <- eicPlot2 %>% add_trace(mode='none', 
			data=data2[scans2[[i]], ], x=~x, y=~y, fill='tozeroy', showlegend=FALSE, legendgroup="group2")
		baseline2 <- runmed(data2$y, 1+2*(min(c((nrow(data2)-1)%/%2, ceiling(.15*nrow(data2))))), 
			endrule="median", algorithm="Turlach")
		eicPlot2 <- eicPlot2 %>% add_lines(x=data2$x, y=baseline2, color=I('red'), showlegend=FALSE, legendgroup="group2")
		
		subplot(eicPlot1, eicPlot2, nrows=2, shareX=TRUE)
	}, invalid = function(i){
		print(i$message)
		plot_ly(type="scatter", mode="lines")
	}, error = function(e){
		print(e$message)
		sendSweetAlert(e$message)
		plot_ly(type="scatter", mode="lines")
	})	
	updateOutput$detailsEic <- FALSE
	print('------------------- END DETAILS EIC -------------------')
	eicPlot %>% 
		layout(xaxis=list(title="retention time"), yaxis=list(title="intensity"), selectdirection="h")	 %>%
		plotly::config(scrollZoom=TRUE, displaylogo=FALSE, modeBarButtons=list(list('zoom2d', 'select2d', 'pan2d', 'autoScale2d', 'resetScale2d')))
})

observeEvent(event_data(event='plotly_selected'), {
	print('------------------------------ RE-INTEGRATE ------------------------')
	pts <- event_data(event='plotly_selected')
	print(list(project=input$project, sample=input$detailsSample,
		adduct=input$detailsAdduct, tolPpm=input$detailsTolPpm, 
		rtmin=min(pts$x), rtmax=max(pts$x),	table_rows=input$detailsTable_selected))
	tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project is not yet initialized')
		else if(is.null(input$detailsSample)) custom_stop('invalid', 'sample is not yet initialized')
		else if(is.null(input$detailsAdduct)) custom_stop('invalid', 'adduct is not yet initialized')
		else if(is.null(input$detailsTable_selected)) custom_stop('invalid', 'no cell clicked')
		else if(input$project == "") custom_stop('invalid', 'no project')
		else if(input$detailsSample == "") custom_stop('invalid', 'no sample')
		else if(input$detailsAdduct == "") custom_stop('invalid', 'no adducts')
		else if(all(input$detailsTable_selected == 0)) custom_stop('invalid', 'no cell selected')
		else if(any(!is.numeric(c(min(pts$x), max(pts$x))))) custom_stop('invalid', 'no pts selected')
		
		updateOutput$detailsEic <- TRUE
		
		chloropara <- getChloroPara(input$detailsTable_selected$C, input$detailsTable_selected$Cl, 
			input$detailsAdduct)
		abd_deviation <- reintegrate(input$project, input$detailsSample, input$detailsAdduct, input$detailsTolPpm, 
			min(pts$x), max(pts$x), input$detailsTable_selected$C, input$detailsTable_selected$Cl, chloropara$abundance)
			
		session$sendCustomMessage("updateDetailsTable", abd_deviation)
		
		toastr_success('re-integration success')
	}, invalid=function(i){
		print(i$message)
	}, minor_error = function(e){
		print(e$message)
		toastr_error(e$message)
	}, error=function(e){
		print(e$message)
		sendSweetAlert(e$message)
	})
	print('------------------------------ END RE-INTEGRATE ------------------------')
})

getChloroPara <- function(C, Cl, adduct){
	adduct <- adducts[which(adducts$Name == adduct), ]
	formula <- paste('C', C, 'H', 2*C+2-Cl, 'Cl', Cl, sep='')
	# check formulas in case
	test <- check_chemform(isotopes, formula)
	if(any(test$warning)) custom_stop('minor_error', paste('number of carbon or chlorine not valid'))
	formula <- test[which(!test$warning), 'new_formula']
	# then add adduct
	brute_formula <- formula
	if(adduct$Formula_add != FALSE) brute_formula <- mergeform(brute_formula, adduct$Formula_add)
	if(adduct$Formula_ded != FALSE){
		test <- check_ded(brute_formula, adduct$Formula_ded)
		if(test == "TRUE") custom_stop('minor_error', paste('cannot substract adduct', adduct$Name, 'to formula', formula))
		brute_formula <- subform(brute_formula, adduct$Formula_ded)
	}
	# remove those who have 0 in one element
	brute_formula <- str_replace_all(brute_formula, '[[:upper:]][[:lower:]]?0', '')
	if(brute_formula == "") custom_stop('minor_error', 'an error occur when substracting elements')
	data <- isopattern(isotopes, brute_formula, charge=adduct$Charge, verbose=FALSE)[[1]]
	data %>% data.frame %>% top_n(2, abundance) %>% 
		select(`m.z`, abundance) %>% cbind(formula=rep(formula, each=2))
}

reintegrate <- function(project, sample, adduct, tolPpm, rtmin, rtmax, C, Cl, abdTheo){
	db <- dbConnect(SQLite(), sqlitePath)
	path <- dbGetQuery(db, sprintf('select path from sample where sample == "%s";',
		sample))$path
	query <- sprintf('select * from observed where C == %s and Cl == %s and 
			project_sample == (
				select project_sample from project_sample where sample == "%s" and
					project == "%s" and adduct == "%s");',
			C, Cl, sample, project, adduct)
	print(query)
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	
	rts <- read.raw(path)$StartTime
	eics <- readXICs(path, masses=data$mz, tol=tolPpm)
	
	data1 <- data.frame(x=rts, y=0)
	data2 <- data.frame(x=rts, y=0)
	data1[which(data1$x %in% eics[[1]]$times), 'y'] <- eics[[1]]$intensities[which(eics[[1]]$times %in% data1$x)]
	data2[which(data2$x %in% eics[[2]]$times), 'y'] <- eics[[2]]$intensities[which(eics[[2]]$times %in% data2$x)]
	
	scans1 <- which(data1$x >= rtmin & data1$x <= rtmax)
	scans2 <- which(data2$x >= rtmin & data2$x <= rtmax)
	
	baseline1 <- runmed(data1$y, 1+2*(min(c((nrow(data1)-1)%/%2, ceiling(.15*nrow(data1))))), 
		endrule="median", algorithm="Turlach")
	baseline2 <- runmed(data2$y, 1+2*(min(c((nrow(data2)-1)%/%2, ceiling(.15	*nrow(data2))))),
		endrule="median", algorithm="Turlach")
		
	auc1 <- trapz(data1[scans1, 'y'])
	auc2 <- trapz(data2[scans2, 'y'])
	
	scanMaxI1 <- scans1[which(data1[scans1, "y"] == max(data1[scans1, "y"]))]
	scanMaxI2 <- scans2[which(data2[scans2, "y"] == max(data2[scans2, "y"]))]
	snthresh1 <- if(sd(baseline1) != 0) (data1[scanMaxI1, 'y'] - baseline1[scanMaxI1]) / sd(baseline1) else (data1[scanMaxI1, 'y'] - baseline1[scanMaxI1])
	snthresh2 <- if(sd(baseline2) != 0) (data2[scanMaxI2, 'y'] - baseline2[scanMaxI2]) / sd(baseline2) else (data2[scanMaxI2, 'y'] - baseline2[scanMaxI2])
	
	abdObs <- auc2 / auc1 * 100
			
	db <- dbConnect(SQLite(), sqlitePath)
	queries <- sprintf('update observed set rtmin = %s, rtmax = %s, auc = %s, ppm = %s, peakwidth = null, 
		snthresh = %s, abd_deviation = %s where C == %s and Cl == %s and project_sample == (
			select project_sample from project_sample where project == "%s" and sample == "%s" and adduct == "%s");',
		rtmin, rtmax, c(auc1, auc2), tolPpm, c(snthresh1, snthresh2), abs(abdTheo - c(100, abdObs)),
		C, Cl, project, sample, adduct)
	print(queries)
	map(queries, function(query) dbSendQuery(db, query))
	dbDisconnect(db)
	
	round(abs(abdTheo[2] - abdObs))
}

observeEvent(input$detailsErase, {
	print('----------------------------- DETAILS ERASE --------------------')
	print(list(projet=input$project, sample=input$detailsSample,
		adduct=input$detailsAdduct, table_rows=input$detailsTable_selected))
	tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project is not yet initialized')
		else if(is.null(input$detailsSample)) custom_stop('invalid', 'sample is not yet initialized')
		else if(is.null(input$detailsAdduct)) custom_stop('invalid', 'adduct is not yet initialized')
		else if(is.null(input$detailsTable_selected)) custom_stop('invalid', 'no cell clicked')
		else if(input$project == "") custom_stop('invalid', 'no project')
		else if(input$detailsSample == "") custom_stop('invalid', 'no sample')
		else if(input$detailsAdduct == "") custom_stop('invalid', 'no adducts')
		else if(all(input$detailsTable_selected == 0)) custom_stop('invalid', 'no cell selected')
		
		db <- dbConnect(SQLite(), sqlitePath)
		query <- sprintf('delete from observed where C == %s and Cl == %s and project_sample == (
			select project_sample from project_sample where project == "%s" and sample == "%s" and adduct == "%s");',
			input$detailsTable_selected$C, input$detailsTable_selected$Cl, input$project,
			input$detailsSample, input$detailsAdduct)
		print(query)
		dbSendQuery(db, query)
		dbDisconnect(db)
		
		session$sendCustomMessage("detailsTableErase", NA)
		toastr_success('record erased')
	}, invalid = function(i){
		print(i$message)
		toastr_error(i$message)
	}, error = function(e){
		print(e$message)
		sendSweetAlert(e$message)
	})
	print('----------------------------- END DETAILS ERASE --------------------')
})


