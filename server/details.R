output$uiDetailsSample <- renderUI({
	choices <- tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project picker is not yet initialized')
		else if(input$project == '') custom_stop('invalid', 'no project')
		else project_samples() %>% filter(project == input$project & 
			project_sample %in% params()$project_sample) %>% 
			select(sampleID, project_sample)
	}, invalid = function(i){
		print(paste(i))
		data.frame(sampleID = c(), project_sample = c())
	}, error = function(e){
		print(paste(e))
		sendSweetAlert(paste(e$message))
		data.frame(sampleID = c(), project_sample = c())
	})
	pickerInput('detailsSample', 'sample', choices=setNames(choices$project_sample, choices$sampleID), 
		multiple=FALSE, options=list(`live-search`=TRUE))
})

output$uiDetailsParam <- renderUI({
	choices <- tryCatch({
		if(is.null(input$detailsSample)) custom_stop('invalid', 'detailsSample picker is not yet initialized')
		else if(input$detailsSample == '') custom_stop('invalid', 'no details sample selected')
		else params() %>% filter(project_sample == input$detailsSample) %>% 
			select(param, adduct)
	}, invalid = function(i){
		print(paste(i))
		c()
	}, error = function(e){
		print(paste(e))
		sendSweetAlert(paste(e$message))
		c()
	})
	pickerInput('detailsParam', 'adduct', choices=setNames(
		choices$param, choices$adduct), multiple=FALSE)
})

output$detailsTable <- DT::renderDataTable({
	actualize$project_samples
	data <- tryCatch({
		if(is.null(input$detailsParam)) custom_stop('invalid', 'param picker not initialized')
		else if(input$detailsParam == "") custom_stop('invalid', 'no param for the file')
		
		query <- sprintf('select * from cluster where project_sample == %s 
			and param == %s;', input$detailsSample, input$detailsParam)
		print(query)
		data <- dbGetQuery(db, query)
		data <- if(input$detailsSwitch) data %>% dplyr::group_by(formula) %>% 
				dplyr::summarise(C=C[1], Cl=Cl[1], score=mean(score) %>% round, rois=dplyr::n()) %>% 
				data.frame
			else data %>% dplyr::group_by(formula) %>% arrange(desc(score)) %>% 
				dplyr::summarise(C=C[1], Cl=Cl[1], rt=rtMean[1] %>% round, rois=dplyr::n()) %>% 
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
	data
}, selection="none", extensions='Scroller', class='display cell-border compact nowrap', options=list(
	info=FALSE, paging=FALSE, dom='Bfrtip', scoller=TRUE, scrollX=TRUE, bFilter=FALSE, ordering=FALSE,
	columnDefs=list(list(className='dt-body-center', targets="_all")), initComplete = htmlwidgets::JS("
		function(settings, json){
			var table = settings.oInstance.api();
			var switchVal = document.getElementById('detailsSwitch').checked;
			table.cells(null, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29]).every(function(){
				if(this.data() != null){
					var value = this.data().split(' ');
					this.data(Number(value[0]));
					if(switchVal && this.data() < 50) $(this.node()).css('background-color', 'rgb(255,36,0)');
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
						C: table.cell(this).index().row+8, 
						Cl: table.cell(this).index().column+1});
				}
			//}
		});
		Shiny.addCustomMessageHandler('updateDetailsTable', function(value){
			table.cell('.selected').data(value);
			if(value < 50 ) var backColor = 'rgb(255,36,0)'
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
	if(is.null(input$detailsParam)) return()
	else if(input$detailsParam == "") return()
	updateNumericInput(session, 'detailsTolPpm', 'tol ppm', value=params() %>% 
		filter(param == input$detailsParam) %>% pull(ppm), min=0, step=1)
})	

plotEIC <- function(data, mz, rois=data.frame(), windowSc){
	baseline <- runmed(data$y, windowSc, endrule="median", algorithm="Turlach")
	eicPlot <- plot_ly(type="scatter", mode="lines") %>% 
		add_trace(mode="lines+markers", 
			data=data, x=~x, y=~y, color=I('black'), showlegend=FALSE, 
			marker=list(opacity=1, size=1*10**-9),
			hoverinfo="text", text=~paste('mz:', round(mz, 5), 
				'<br />rt:', round(data$x, digits=2), 
				'<br />intensity:', formatC(data$y))) %>% 
		add_lines(x=data$x, y=baseline, showlegend=FALSE, color=I('red'), 
			hoverinfo="text", text=~paste('rt:', 
				round(data$x, digits=2), '<br />intensity:', formatC(baseline)))
	if(nrow(rois) > 0){
		scans <- lapply(1:nrow(rois), function(x) 
			which(data$x >= rois[x, 'rtmin'] & data$x <= rois[x, 'rtmax']))
	
		# trace a line under the roi for filling it 
		for(i in 1:nrow(rois)) eicPlot <- eicPlot %>% add_trace(mode='none', 
			x=rois[i, c('rtmin', 'rtmax')] %>% unlist, y=max(data$y), hoverinfo='none', 
				fill='tozeroy', showlegend=FALSE)
		eicPlot <- eicPlot %>% 
			add_annotations(x=.5, y=1, text=rois[1, 'iso'], showarrow=FALSE, 
				xref='paper', yref='paper', showlegend=FALSE, font=list(size=30)) %>% 
		layout(xaxis=list(title="retention time"), yaxis=list(title="intensity"), selectdirection="h")
	}
	eicPlot
}

output$detailsEic <- renderPlotly({
	actualize$detailsEic
	eicPlot <- tryCatch({
		if(is.null(input$detailsParam)) custom_stop('invalid', 'param picker is not yet initialized')
		else if(is.null(input$detailsTable_selected)) custom_stop('invalid', 'no cell clicked')	
		else if(input$detailsParam == "") custom_stop('invalid', 'no param selected')
		else if(length(input$detailsTable_selected) == 0) custom_stop('invalid', 'no cell clicked')
		if(!is.numeric(input$detailsTolPpm)) custom_stop('invalid', 'tol ppm is not numeric')
		
		C <- input$detailsTable_selected$C
		Cl <- input$detailsTable_selected$Cl
		if(C == 0 | Cl == 0) custom_stop('invalid', 'no cell clicked')
		
		query <- sprintf('select data from sample where sample == (
			select sample from project_sample where project_sample == %s);', 
			input$detailsSample)
		msFile <- dbGetQuery(db, query)$data[[1]] %>% unserialize
		rtScan <- mean(diff(msFile@scantime))
		param <- params() %>% filter(param == input$detailsParam)
		
		query <- sprintf('select * from feature where cluster in (select cluster 
			from cluster where C == %s and Cl == %s and project_sample == %s and 
			param == %s);', input$detailsTable_selected[1], input$detailsTable_selected[2], 
				input$detailsSample, input$detailsParam)
		print(query)
		data <- dbGetQuery(db, query)
		
		theoric <- getChloroPara(input$detailsTable_selected$C, input$detailsTable_selected$Cl, 
				param$adduct, param$machine %>% as.numeric) %>% 
			dplyr::mutate(tolMDa = mz * input$detailsTolPpm * 10**-6, 
				mzmin = mz - tolMDa, mzmax = mz + tolMDa)
		theoric <- if(nrow(data) == 0) theoric[1:2, ] else theoric[1:length(unique(data$iso)), ]
		eics <- lapply(1:nrow(theoric), function(row)
			rawEIC(msFile, mzrange=theoric[row, c('mzmin', 'mzmax')] %>% as.double) %>% 
			arrangeEic2(msFile))
		
		
		subplot(lapply(1:length(eics), function(i) 
			plotEIC(eics[[i]], theoric[i, "mz"], data %>% filter(iso == 
				theoric[i, 'theoricIso']) %>% 
				select(rtmin, rtmax, iso), round((param$pw2 / rtScan) * 2))), 
			shareX=TRUE, nrows=length(eics))
	}, invalid = function(i){
		print(paste(i))
		plot_ly(type="scatter", mode="lines")
	}, error = function(e){
		print(paste(e))
		sendSweetAlert(paste(e$message))
		plot_ly(type="scatter", mode="lines")
	})	
	actualize$detailsEic <- FALSE
	eicPlot %>%
		plotly::config(scrollZoom=TRUE, displaylogo=FALSE, modeBarButtons=list(list('zoom2d', 'select2d', 'pan2d', 'autoScale2d', 'resetScale2d')))
})

observeEvent(event_data(event='plotly_selected'), {
	print('------------------------------ RE-INTEGRATE ------------------------')
	pts <- event_data(event='plotly_selected')
	print(list(project_sample=input$detailsSample, tolPpm=input$detailsTolPpm, 
		rtmin=min(pts$x), rtmax=max(pts$x),	table_rows=input$detailsTable_selected,
		switch=input$detailsSwitch))
	tryCatch({
		if(is.null(input$detailsSample)) custom_stop('invalid', 'sample is not yet initialized')
		else if(is.null(input$detailsTable_selected)) custom_stop('invalid', 'no cell clicked')
		else if(input$detailsSample == "") custom_stop('invalid', 'no sample')
		else if(all(input$detailsTable_selected == 0)) custom_stop('invalid', 'no cell selected')
		else if(any(!is.numeric(c(min(pts$x), max(pts$x))))) custom_stop('invalid', 'no pts selected')
		
		path <- samples() %>% filter(sample == project_samples() %>% 
			filter(project_sample == input$detailsSample) %>% 
			pull(sample)) %>% pull(path)
		msFile <- xcmsRaw(path, mslevel=1)
		pts$x <- pts$x * 60
		roi <- which(msFile@scantime >= min(pts$x) & 
			msFile@scantime <= max(pts$x))
		
		theo <- getChloroPara(input$detailsTable_selected$C, input$detailsTable_selected$Cl, 
			input$detailsAdduct, input$detailsMachine %>% as.numeric) %>% 
			dplyr::mutate(tolMDa = mz * input$detailsTolPpm * 10**-6) %>% 
			dplyr::mutate(mzmin = mz - tolMDa, mzmax = mz + tolMDa) %>% select(-tolMDa) %>% 
			filter(mzmin >= msFile@mzrange[1] & 
			mzmax <= msFile@mzrange[2])
		if(nrow(theo) == 0) custom_stop('minor_error', 
			"the chloroparafin m/z cannot be detected in the sample")
		eic <- rawEIC(msFile, mzrange = theo[1, c('mzmin', 'mzmax')] %>% as.matrix) %>% 
			arrangeEics(msFile)
			
		windowRTMed <- 1 + 2 * min(
			(length(roi)*12-1)%/% 2, 
			ceiling(0.1*length(roi)*12))
		
		mzs <- theo[, c('mz', 'mzmin', 'mzmax')]
		tmpRes <- targetChloroPara2(msFile, mzs[1, ], roi, 
			windowRTMed, 2, 0, Inf)
		res <- data.frame(mz = c(), rt = c(), rtmin = c(), rtmax = c(), 
			auc = c(), score = c())
		while(nrow(tmpRes) > 0){
			res <- rbind(res, tmpRes)
			mzs <- mzs[-1, ]
			tmpRes <- targetChloroPara2(msFile, mzs[1, ], roi, 
				windowRTMed, 0, 0, tmpRes$auc)
		}
		if(nrow(res) < 2) custom_stop('minor_error', 'detect none chloroparafin')
		
		abdObs <- sapply(res$auc, function(x) x * 100 / res[1, 'auc'])
		score <- computeScore(abdObs[-1], theo$abundance[-1])
		annotations <- getAnnotations(res$mz)
		res <- res %>% dplyr::mutate(abd = abdObs, score = score, formula = theo[1, 'formula'],
			roiNb = 1, annotation = annotations, C=input$detailsTable_selected$C, 
			Cl = input$detailsTable_selected$Cl)
			
		query <- sprintf('delete from observed where C == %s and Cl == %s and project_sample == %s;',
			input$detailsTable_selected$C, input$detailsTable_selected$Cl, input$detailsSample)
		print(query)
		dbSendQuery(db, query)
		query <- sprintf('insert into observed (mz, formula, rt, rtmin, rtmax, auc, project_sample, ppm,
			C, Cl, abundance, score, machine, roiNb, annotation) values %s;', 
				paste('(', res$mz, ', "', res$formula,
				'", ', res$rt, ', ', res$rtmin, ', ', res$rtmax, ', ', res$auc, 
				', ', input$detailsSample, ', ', input$detailsTolPpm, ', ', res$C, 
				', ', res$Cl, ', ', res$abd, ', ', res$score, ', ', input$detailsMachine %>% as.numeric, 
				', ', res$roiNb, ', "', res$annotation, '")', collapse=', ', sep=''))
		print(query)
		dbSendQuery(db, query)
	
		val <- if(input$detailsSwitch) res$score[1] else res$rt[1] / 60
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
	data[[1]] %>% data.frame %>% 
		arrange(desc(abundance)) %>% dplyr::mutate(mz = round(`m.z`, digits=5), 
		theoricIso = ceiling(mz), theoricIso = theoricIso - theoricIso[1], 
		theoricIso = case_when(theoricIso < 0 ~ paste0('A', theoricIso), 
			theoricIso > 0 ~ paste0('A+', theoricIso), TRUE ~ 'A')) %>% 
			distinct(theoricIso, .keep_all=TRUE) %>% 
		select(mz, abundance, theoricIso) %>% cbind(formula = formula)
}

observeEvent(input$detailsErase, {
	print('----------------------------- DETAILS ERASE --------------------')
	print(list(projet_sample=input$detailsSample,
		table_rows=input$detailsTable_selected))
	tryCatch({
		if(is.null(input$detailsSample)) custom_stop('invalid', 'sample is not yet initialized')
		else if(is.null(input$detailsTable_selected)) custom_stop('invalid', 'no cell clicked')
		else if(input$detailsSample == "") custom_stop('invalid', 'no sample')
		else if(all(input$detailsTable_selected == 0)) custom_stop('invalid', 'no cell selected')
		
		query <- sprintf('delete from observed where C == %s and Cl == %s and project_sample == %s;',
			input$detailsTable_selected$C, input$detailsTable_selected$Cl, input$detailsSample)
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


