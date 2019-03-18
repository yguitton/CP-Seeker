output$uiDetailsSample <- renderUI({
	pickerInput('detailsSample', 'sample', choices=project_samples() %>%
		filter(project == input$project) %>% pull(sample), multiple=FALSE, options=list(`live-search`=TRUE))
})

output$uiDetailsAdduct <- renderUI({
	pickerInput('detailsAdduct', 'adduct', choices=project_samples() %>%
		filter(project == input$project & sample == input$detailsSample) %>%
		pull(adduct), multiple=FALSE)
})

output$detailsTable <- renderDataTable({
	print('------------------- DETAILS TABLE ------------------')
	print(list(project=input$project, sample=input$detailsSample, 
		adduct=input$detailsAdduct, switch=input$detailsSwitch))
	actualize$project_samples
	data <- tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project not initialized')
		else if(input$project == '') custom_stop('invalid', 'no project created')
		else if(is.null(input$detailsSample)) custom_stop('invalid', 'sample not initialized')
		else if(input$detailsSample == "") custom_stop('invalid', 'no sample processed in project')
		else if(is.null(input$detailsAdduct)) custom_stop('invalid', 'adduct not initialized')
		else if(input$detailsAdduct == "") custom_stop('invalid', 'no adduct in project_sample???')
		
		query <- sprintf('select * from observed where project_sample == (select project_sample
			from project_sample where project == "%s" and sample == "%s" and adduct == "%s");',
			input$project, input$detailsSample, input$detailsAdduct)
		print(query)
		data <- dbGetQuery(db, query)
		data <- if(input$detailsSwitch) data %>% group_by(formula) %>% 
				summarise(C=C[1], Cl=Cl[1], score=mean(score) %>% round, rois=n()/2) %>% 
				data.frame
			else data %>% group_by(formula) %>% 
				summarise(C=C[1], Cl=Cl[1], rt=mean(rt) %>% round, rois=n()/2) %>% 
				data.frame
		res <- matrix(NA, nrow=maxC-minC+1, ncol=maxCl-minCl+1)
		for(row in 1:nrow(data)) res[data[row, 'C']-minC+1, data[row, 'Cl']-minCl+1] <- paste(data[row, 4:5], collapse=" ")
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
	columnDefs=list(list(className='dt-body-center', targets="_all")), initComplete = htmlwidgets::JS("
		function(settings, json){
			var table = settings.oInstance.api();
			var switchVal = $('#detailsSwitch').val();
			table.cells(null, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29]).every(function(){
				if(this.data() != null){
					var value = this.data().split(' ');
					this.data(Number(value[0]));
					if(switchVal == 'on' && (this.data() < -20 || this.data() > 20)) $(this.node()).css('background-color', 'rgb(255,36,0)');
					if(Number(value[1]) > 1) $(this.node()).css('border', '5px solid orange');
				}
			})
		}
	")), callback = htmlwidgets::JS("
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
			if(value < -20 || value > 20) var backColor = 'rgb(255,36,0)'
			else var backColor = 'rgba(0,0,0,0)'
			$(table.cell('.selected').node()).css('background-color', backColor);
			$(table.cell('.selected').node()).css('border', '');
		})
		Shiny.addCustomMessageHandler('detailsTableErase', function(message){
			table.cell('.selected').data(null);
			$(table.cell('.selected').node()).css('background-color', 'rgba(0,0,0,0)');
			$(table.cell('.selected').node()).css('border', '');
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
	data <- dbGetQuery(db, sprintf('select ppm, machine from observed where C == %s and Cl == %s and 
		project_sample == (select project_sample from project_sample where project == "%s" and 
			sample == "%s" and adduct == "%s");', input$detailsTable_selected$C, 
			input$detailsTable_selected$Cl, input$project, input$detailsSample, 
			input$detailsAdduct))[1, ]
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
	
	actualize$detailsEic
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
		
		query <- sprintf('select * from observed where C == %s and Cl == %s and 
			project_sample == (
				select project_sample from project_sample where sample == "%s" and
					project == "%s" and adduct == "%s");',
			input$detailsTable_selected[1], input$detailsTable_selected[2], 
				input$detailsSample, input$project, input$detailsAdduct)
		print(query)
		data <- dbGetQuery(db, query)
		
		chloropara <- getChloroPara(input$detailsTable_selected$C, input$detailsTable_selected$Cl, 
			input$detailsAdduct, input$detailsMachine %>% as.numeric)
		path <- samples() %>% filter(sample == input$detailsSample) %>% pull(path)
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
	actualize$detailsEic <- FALSE
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
		rtmin=min(pts$x), rtmax=max(pts$x),	table_rows=input$detailsTable_selected,
		switch=input$detailsSwitch))
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
		
		chloropara <- getChloroPara(input$detailsTable_selected$C, input$detailsTable_selected$Cl, 
			input$detailsAdduct, input$detailsMachine %>% as.numeric)
		vals <- reintegrate(input$project, input$detailsSample, input$detailsAdduct, input$detailsTolPpm, 
			min(pts$x), max(pts$x), input$detailsTable_selected$C, input$detailsTable_selected$Cl, chloropara,
			input$detailsMachine %>% as.numeric)
		val <- if(input$detailsSwitch) vals$score else vals$rt
			
		session$sendCustomMessage("updateDetailsTable", round(val))
		actualize$detailsEic <- TRUE
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
	
	eics <- readXICs(path, masses=theo[, 'mz'], tol=tolPpm)
	# rearrange eic from rawDiag to have a dataframe for each eic & not a list
	raw <- read.raw(path)
	rts <- raw$StartTime
	eics <- map(eics, function(eic) arrangeEICRawDiag(eic, rts))
	
	roi <- which(eics[[1]]$x >= rtmin & eics[[1]]$x <= rtmax)
	windowRTMed <- 9*(length(roi)%/%2)
	
	rts <- sapply(eics, function(i) apply(i[roi, ], 1, function(j) 
		j[1] * (j[2] / sum(i[roi, 2]))) %>% sum)
	aucs <- reduce(eics, function(a, b) 
		c(a, getAUC(b, roi, windowRTMed)), .init = c())
	if(aucs[2] == 0) stop('auc of A2 is O')
	theo <- theo[which(aucs > 0), ]
	aucs <- aucs[which(aucs > 0)]
	abdObs <- sapply(aucs, function(x) x * 100 / aucs[1])
	score <- (abdObs[2] - theo[2, 'abundance']) / theo[2, 'abundance'] * 100
	
	res <- data.frame(mz=theo$mz, rtmin=eics[[1]][min(roi), 'x'], rtmax=eics[[1]][max(roi), 'x'], 
		auc=aucs, abd=abdObs, score=score)
				
	query <- sprintf('insert into observed (mz, formula, rtmin, rtmax, rt, auc, project_sample, ppm,
				C, Cl, abundance, score, machine) values %s;', paste('(', theo$mz, ', "', theo[1, 'formula'],
					'", ', rtmin, ', ', rtmax, ', ', rts, ', ', aucs, ', ', project_sample, ', ', 
					tolPpm, ', ', C, ', ', Cl, ', ', abdObs, ', ', score, ', ', machine, ')', collapse=', ', sep=''))
	print(query)
	dbSendQuery(db, query)
	
	list(score=score, rt=mean(rts))
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
		
		query <- sprintf('delete from observed where C == %s and Cl == %s and project_sample == (
			select project_sample from project_sample where project == "%s" and sample == "%s" and adduct == "%s");',
			input$detailsTable_selected$C, input$detailsTable_selected$Cl, input$project,
			input$detailsSample, input$detailsAdduct)
		print(query)
		dbSendQuery(db, query)
		
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


