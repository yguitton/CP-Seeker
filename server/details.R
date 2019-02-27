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
		data$score <- round(data$score)
		res <- matrix(NA, nrow=maxC-minC+1, ncol=maxCl-minCl+1)
		for(row in 1:nrow(data)) res[data[row, 'C']-minC+1, data[row, 'Cl']-minCl+1] <- data[row, 'score']
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
			//if(table.cell(this).data() != null){
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
			//}
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
	
observeEvent(input$detailsTable_selected, {
	if(is.null(input$project)) return()
	else if(is.null(input$detailsSample)) return()
	else if(is.null(input$detailsAdduct)) return()
	else if(is.null(input$detailsTable_selected)) return()
	else if(input$project == "") return()
	else if(input$detailsSample == "") return()
	else if(input$detailsAdduct == "") return()
	else if(length(input$detailsTable_selected) == 0) return()
	db <- dbConnect(SQLite(), sqlitePath)
	data <- dbGetQuery(db, sprintf('select ppm, machine from observed where C == %s and Cl == %s and 
		project_sample == (select project_sample from project_sample where project == "%s" and 
			sample == "%s" and adduct == "%s");', input$detailsTable_selected$C, 
			input$detailsTable_selected$Cl, input$project, input$detailsSample, 
			input$detailsAdduct))[1, ]
	dbDisconnect(db)
	if(!is.na(data$ppm)) updateNumericInput(session, 'detailsTolPpm', 'tol ppm', value=data$ppm, min=0, step=1)
	if(!is.na(data$machine)) updatePickerInput(session, 'detailsMachine', 'machine', choices=setNames(1:length(resolution_list),
			names(resolution_list)), selected=data$machine)
})	

plotEIC <- function(data, mz, rois=data.frame()){
	eicPlot <- plot_ly(type="scatter", mode="lines") %>% 
		add_trace(mode="lines+markers", name=mz, legendgroup="group1", 
			data=data, x=~x, y=~y, color=I('black'), showlegend=FALSE, 
			marker=list(opacity=1, size=1*10**-9),
			hoverinfo="text", text=~paste('rt:', round(data$x, digits=2), 
			'<br />intensity:', formatC(data$y)))
	if(nrow(rois) > 0){
		scans <- lapply(1:nrow(rois), function(x) 
			which(data$x >= rois[x, 'rtmin'] & data$x <= rois[x, 'rtmax']))
		for(i in 1:length(scans)) eicPlot <- eicPlot %>% add_trace(mode='none', 
			data=data[scans[[i]], ], x=~x, y=~y, fill='tozeroy', showlegend=FALSE, 
			legendgroup="group1")
		baseline <- runmed(data$y, 9*(length(unlist(scans))%/%2), 
			endrule="median", algorithm="Turlach")
		eicPlot <- eicPlot %>% 
			add_lines(name="baseline", x=data$x, y=baseline, color=I('red'), 
				showlegend=FALSE, legendgroup="group1", hoverinfo="text", text=~paste(
				'rt:', round(data$x, digits=2), '<br />intensity:', formatC(baseline)))
	}
	eicPlot
}

output$detailsEic <- renderPlotly({
	print('------------------- DETAILS EIC -------------------')
	print(list(project=input$project, sample=input$detailsSample, 
		adduct=input$detailsAdduct, machine = names(resolution_list)[[input$detailsMachine %>% as.numeric]], 
		table_rows=input$detailsTable_selected))

	updateOutput$details
	updateOutput$detailsEic
	eicPlot <- tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project is not yet initialized')
		else if(is.null(input$detailsSample)) custom_stop('invalid', 'sample is not yet initialized')
		else if(is.null(input$detailsAdduct)) custom_stop('invalid', 'adduct is not yet initialized')
		else if(is.null(input$detailsMachine)) custom_stop('invalid', 'machine is not yet initialized')
		else if(is.null(input$detailsTable_selected)) custom_stop('invalid', 'no cell clicked')	
		else if(input$project == "") custom_stop('invalid', 'no project selected')
		else if(input$detailsSample == "") custom_stop('invalid', 'no sample selected')
		else if(input$detailsAdduct == "") custom_stop('invalid', 'no adduct selected')
		else if(input$detailsMachine == '') custom_stop("invalid", "no machine selected")
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
		
		query <- sprintf('select * from observed where C == %s and Cl == %s and 
			project_sample == (
				select project_sample from project_sample where sample == "%s" and
					project == "%s" and adduct == "%s");',
			input$detailsTable_selected[1], input$detailsTable_selected[2], 
				input$detailsSample, input$project, input$detailsAdduct)
		print(query)
		data <- dbGetQuery(db, query)
		dbDisconnect(db)
		
		chloropara <- getChloroPara(input$detailsTable_selected$C, input$detailsTable_selected$Cl, 
			input$detailsAdduct, input$detailsMachine %>% as.numeric)
		eics <- readXICs(path, masses=chloropara$mz, tol=input$detailsTolPpm)
		# rearrange eic from rawDiag to have a dataframe for each eic & not a list
		raw <- read.raw(path)
		rts <- raw$StartTime
		eics <- map(eics, function(eic) arrangeEICRawDiag(eic, rts))
		
		eicPlot1 <- plotEIC(eics[[1]], chloropara[1, 'mz'], 
			data[which(data$mz == chloropara[1, 'mz']), c('rtmin', 'rtmax')])
		eicPlot2 <- plotEIC(eics[[2]], chloropara[2, 'mz'], 
			data[which(data$mz == chloropara[2, 'mz']), c('rtmin', 'rtmax')])
	
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
		machine = names(resolution_list)[[input$detailsMachine %>% as.numeric]],
		rtmin=min(pts$x), rtmax=max(pts$x),	table_rows=input$detailsTable_selected))
	tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project is not yet initialized')
		else if(is.null(input$detailsSample)) custom_stop('invalid', 'sample is not yet initialized')
		else if(is.null(input$detailsAdduct)) custom_stop('invalid', 'adduct is not yet initialized')
		else if(is.null(input$detailsMachine)) custom_stop('invalid', 'machine is not yet initialized')
		else if(is.null(input$detailsTable_selected)) custom_stop('invalid', 'no cell clicked')
		else if(input$project == "") custom_stop('invalid', 'no project')
		else if(input$detailsSample == "") custom_stop('invalid', 'no sample')
		else if(input$detailsAdduct == "") custom_stop('invalid', 'no adducts')
		else if(input$detailsMachine == '') custom_stop('invalid', 'no machine selected')
		else if(all(input$detailsTable_selected == 0)) custom_stop('invalid', 'no cell selected')
		else if(any(!is.numeric(c(min(pts$x), max(pts$x))))) custom_stop('invalid', 'no pts selected')
		
		updateOutput$detailsEic <- TRUE
		
		chloropara <- getChloroPara(input$detailsTable_selected$C, input$detailsTable_selected$Cl, 
			input$detailsAdduct, input$detailsMachine %>% as.numeric)
		score <- reintegrate(input$project, input$detailsSample, input$detailsAdduct, input$detailsTolPpm, 
			min(pts$x), max(pts$x), input$detailsTable_selected$C, input$detailsTable_selected$Cl, chloropara,
			input$detailsMachine %>% as.numeric)
			
		session$sendCustomMessage("updateDetailsTable", score)
		
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

getChloroPara <- function(C, Cl, adduct, machine=NULL){
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
	data <- isopattern(isotopes, brute_formula, charge=adduct$Charge, verbose=FALSE)
	if(!is.null(machine)){
		checked <- check_chemform(isotopes, brute_formula)
		resolution <- getR(checked, plotit=FALSE, 
			resmass=resolution_list[[machine]])
		data <- envelope(data, resolution=resolution, verbose=FALSE)
		data <- vdetect(data, detect='centroid', plotit=FALSE, verbose=FALSE)
	}
	data[[1]] %>% data.frame %>% top_n(2, abundance) %>% arrange(desc(abundance)) %>% 
		mutate(mz = round(`m.z`, digits=5)) %>% select(mz, abundance) %>% 
		cbind(formula = formula)
}

reintegrate <- function(project, sample, adduct, tolPpm, rtmin, rtmax, C, Cl, theo, machine){
	db <- dbConnect(SQLite(), sqlitePath)
	path <- dbGetQuery(db, sprintf('select path from sample where sample == "%s";',
		sample))$path
	project_sample <- dbGetQuery(db, sprintf('select project_sample from project_sample 
		where project == "%s" and sample == "%s" and adduct == "%s";', 
		project, sample, adduct))$project_sample
	query <- sprintf('delete from observed where C == %s and Cl == %s and 
			project_sample == %s;',
			C, Cl, project_sample)
	print(query)
	dbSendQuery(db, query)
	dbDisconnect(db)
	
	eics <- readXICs(path, masses=theo[, 'mz'], tol=tolPpm)
	# rearrange eic from rawDiag to have a dataframe for each eic & not a list
	raw <- read.raw(path)
	rts <- raw$StartTime
	eics <- map(eics, function(eic) arrangeEICRawDiag(eic, rts))
	
	roi <- which(eics[[1]]$x >= rtmin & eics[[1]]$x <= rtmax)
	windowRTMed <- 9*(length(roi)%/%2)
	
	aucs <- reduce(eics, function(a, b) 
		c(a, getAUCs(b, roi, windowRTMed)), .init = c())
	if(aucs[2] == 0) stop('auc of A2 is O')
	theo <- theo[which(aucs > 0), ]
	aucs <- aucs[which(aucs > 0)]
	abdObs <- sapply(aucs, function(x) x * 100 / aucs[1])
	score <- (abdObs[2] - theo[2, 'abundance']) / theo[2, 'abundance'] * 100
	
	res <- data.frame(mz=theo$mz, rtmin=eics[[1]][min(roi), 'x'], rtmax=eics[[1]][max(roi), 'x'], 
		auc=aucs, abd=abdObs, score=score)
				
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('insert into observed (mz, formula, rtmin, rtmax, auc, project_sample, ppm,
				C, Cl, abundance, score, machine) values %s;', paste('(', theo$mz, ', "', theo[1, 'formula'],
					'", ', rtmin, ', ', rtmax, ', ', aucs, ', ', project_sample, ', ', 
					tolPpm, ', ', C, ', ', Cl, ', ', abdObs, ', ', score, ', ', machine, ')', collapse=', ', sep=''))
	print(query)
	dbSendQuery(db, query)
	dbDisconnect(db)
	
	round(score)
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


