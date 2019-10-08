output$uiDetailsSample <- renderUI({
	choices <- tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project picker is not yet initialized')
		else if(input$project == '') custom_stop('invalid', 'no project')
		else dbGetQuery(db, sprintf('select sampleID, project_sample 
			from project_sample where project == %s and project_sample in (
				select distinct(project_sample) from cluster);', input$project))
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

output$uiDetailsAdduct <- renderUI({
	choices <- tryCatch({
		if(is.null(input$detailsSample)) custom_stop('invalid', 'detailsSample picker is not yet initialized')
		else if(input$detailsSample == '') custom_stop('invalid', 'no details sample selected')
		else dbGetQuery(db, sprintf('select adduct from cluster where project_sample == %s;',  
			input$detailsSample))$adduct %>% unique
	}, invalid = function(i){
		print(paste(i))
		c()
	}, error = function(e){
		print(paste(e))
		sendSweetAlert(paste(e$message))
		c()
	})
	pickerInput('detailsAdduct', 'adduct', choices=choices, multiple=FALSE)
})

output$detailsTable <- DT::renderDataTable({
	actualize$project_samples
	data <- tryCatch({
		if(is.null(input$detailsAdduct)) custom_stop('invalid', 'adduct picker not initialized')
		else if(input$detailsAdduct == "") custom_stop('invalid', 'no adduct for the file??')
		
		query <- sprintf('select * from cluster where project_sample == %s 
			and adduct == \"%s\";', input$detailsSample, input$detailsAdduct)
		print(query)
		data <- dbGetQuery(db, query)
		data <- if(input$detailsSwitch) data %>% dplyr::group_by(formula) %>% 
				dplyr::summarise(C=C[1], Cl=Cl[1], score=mean(score) %>% round, rois=dplyr::n()) %>% 
				data.frame
			else data %>% dplyr::group_by(formula) %>% arrange(desc(score)) %>% 
				dplyr::summarise(C=C[1], Cl=Cl[1], rt=round(rtMean[1] / 60), rois=dplyr::n()) %>% 
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
			maxCols = table.row(0).data().length - 1;
			maxRows = table.column(0).data().length - 1;
			for(var i = 0; i <= maxRows; i++){
				for(var j = 1; j <= maxCols; j++){
					var value = table.cell(i, j).data();
					if(value != null){
						value = value.split(' ');
						table.cell(i, j).data(Number(value[0]));
						if(switchVal && value[0] < 70) $(table.cell(i, j).node()).css('background-color', 'rgb(255,36,0)');
						if(Number(value[1]) > 1) $(table.cell(i, j).node()).css('border', '5px solid orange');
					}
				}
			}
		}
	")), callback = htmlwidgets::JS("
		Shiny.onInputChange('detailsTable_selected', {C: 0, Cl: 0});
		function getFormula(table, obj){
			var id = obj.index();
			var C = Number(table.cell(id.row, 0).data().replace('C', ''));
			var Cl = Number(table.column(id.column).header().textContent.replace('Cl', ''));
			return {
				C : C, 
				Cl : Cl
			}
		}
		table.on('click', 'tbody td', function(){
			if($(this).hasClass('selected')){
				$(table.cells('.selected').nodes()).toggleClass('selected');
				Shiny.onInputChange('detailsTable_selected', {C: 0, Cl: 0});
			} else {
				$(table.cells('.selected').nodes()).toggleClass('selected');
				$(this).toggleClass('selected');
				var formula = getFormula(table, table.cell(this));
				if(formula.C == NaN || formula.Cl == NaN){
					Shiny.onInputChange('detailsTable_selected', {C: 0, Cl: 0});
				} else {
					Shiny.onInputChange('detailsTable_selected', 
						{C: formula.C, Cl: formula.Cl});
				}
			}
		});
		Shiny.addCustomMessageHandler('updateDetailsTable', function(value){
			Shiny.onInputChange('detailsTable_selected', {C:0, Cl:0});
			table.cell('.selected').data(value);
			if(value < 50 ) var backColor = 'rgb(255,36,0)'
			else var backColor = 'rgba(0,0,0,0)'
			$(table.cell('.selected').node()).css('background-color', backColor);
			$(table.cell('.selected').node()).css('border', '');
			var formula = getFormula(table, table.cell('.selected'));
			Shiny.onInputChange('detailsTable_selected', 
						{C: formula.C, Cl: formula.Cl});
		})
		Shiny.addCustomMessageHandler('detailsTableErase', function(message){
			table.cell('.selected').data(null);
			$(table.cell('.selected').node()).css('background-color', 'rgba(0,0,0,0)');
			$(table.cell('.selected').node()).css('border', '');
			$(table.cell('.selected').node()).toggleClass('selected');
			Shiny.onInputChange('detailsTable_selected', {C:0, Cl:0});
		})
	"))
	
values$table_xr <- NULL
observeEvent(input$detailsSample, {
	values$table_xr <- tryCatch(loadRawFile(db, input$detailsSample), 
		error = function(e){
		print("file not found")
		toastr_error(sprintf('File "%s" is not found in database', 
			project_samples() %>% filter(project_sample == input$detailsSample) %>% 
				pull(sampleID)), paste(e$message))
		NULL
	})
})

values$table_clusterID <- list(C = 0, Cl = 0, ids = c())
observeEvent(input$detailsTable_selected, {
	tryCatch({
		if(is.null(input$detailsSample)) custom_stop('invalid', 'no sample selected')
		else if(input$detailsSample == '') custom_stop('invalid', 'no sample selected')
		else if(is.null(input$detailsAdduct)) custom_stop('invalid', 'no adduct selected')
		else if(input$detailsAdduct == "") custom_stop('invalid', 'no adduct selected')
		else if(is.null(input$detailsTable_selected)) custom_stop('invalid', 'no cell selected')
		
		values$table_clusterID$C <- input$detailsTable_selected$C
		values$table_clusterID$Cl <- input$detailsTable_selected$Cl
		
		values$table_clusterID$ids <- if(any(input$detailsTable_selected == 0)) c()
		else dbGetQuery(db, sprintf('select cluster from cluster 
			where project_sample == %s and 
			adduct == \"%s\" and C == %s and Cl == %s;', input$detailsSample, 
				input$detailsAdduct, input$detailsTable_selected$C, 
				input$detailsTable_selected$Cl))$cluster
	}, invalid = function(i) values$table_clusterID <- list(C = 0, Cl = 0, ids = c())
	, error = function(e){
		print('ERR detailsTable_selected')
		print(input$detailsTable_selected)
		print(e)
		toastr_error("Cannot retrieve the chloroparaffin in database ?!", 
			paste(e$message))
		values$table_clusterID <- list(C = 0, Cl = 0, ids = c())
	})
})

observeEvent(values$table_clusterID, {
	tryCatch({
		if(is.null(values$table_clusterID)) custom_stop('invalid', 'not initialized ?????')
		else if(length(values$table_clusterID$ids) == 0) custom_stop('invalid', 'no cluster retrieve by cell selected')
		
		vals <- dbGetQuery(db, sprintf('select ppm, machine from cluster where cluster in (%s);', 
			paste(values$table_clusterID$ids, collapse=', ')))
			
		updateNumericInput(session, 'detailsTolPpm', 'tol ppm', value=vals$ppm)
		updatePickerInput(session, 'detailsMachine', 'machine', choices=
					names(resolution_list), selected = vals$machine)
	}, invalid = function(i) NULL
	, error = function(e){
		print('ERR values$table_clusterID')
		print(e)
		toastr_error()
	})
})

output$detailsMS <- renderPlotly({
	tryCatch({
		if(is.null(values$table_clusterID)) custom_stop('invalid', 'no cell selected')
		else if(values$table_clusterID$C == 0) custom_stop('invalid', 'no cell selected')
		else if(is.null(input$detailsAdduct)) custom_stop('invalid', 'adduct picker not initialized')
		else if(input$detailsAdduct == "") custom_stop('invalid', 'no adduct for the file??')
		else if(is.null(input$detailsMachine)) custom_stop('invalid', 'machine picker not initialized')
		else if(input$detailsMachine == "") custom_stop('invalid', 'no machine')
		
		plotMS(db, values$table_clusterID$C, values$table_clusterID$Cl, 
			values$table_clusterID$ids, isolate(input$detailsAdduct), input$detailsMachine)
	}, invalid = function(i) plotEmptyMS()
	, error = function(e){
		print('ERR detailsMS')
		print(e)
		toastr_error("Cannot draw chloroparaffin mass spectrum")
		plotEmptyMS()
	})
		
})

output$detailsEIC <- renderPlotly({
	tryCatch({
		if(is.null(values$table_clusterID)) custom_stop('invalid', 'no cell selected')
		else if(values$table_clusterID$C == 0) custom_stop('invalid', 'no cell selected')
		else if(!is.numeric(input$detailsTolPpm)) custom_stop('invalid', 'ppm is not numeric')
		else if(is.null(isolate(input$detailsAdduct))) custom_stop('invalid', 'adduct picker not initialized')
		else if(isolate(input$detailsAdduct) == "") custom_stop('invalid', 'no adduct for the file??')
		else if(is.null(input$detailsMachine)) custom_stop('invalid', 'machine picker not initialized')
		else if(input$detailsMachine == "") custom_stop('invalid', 'no machine')
		
		plotEIC(db, values$table_clusterID$C, values$table_clusterID$Cl, 
			values$table_clusterID$ids, isolate(input$detailsAdduct), input$detailsMachine,  
			ppm = input$detailsTolPpm, xr = isolate(values$table_xr))
	}, invalid = function(i) plotEmptyChromato(title="EIC")
	, error = function(e){
		print('ERR detailsEIC')
		print(e)
		toastr_error("Cannot draw chloroparaffin eic")
		plotEmptyChromato(title="EIC")
	}) %>% onRender("function(el, x){
		el.on('plotly_selected', function(eventData){
			Shiny.onInputChange('detailsEIC_selected', eventData.range.x);
		})
	}")
})

observeEvent(input$detailsErase, {
	print('############################################################')
	print('######################### DETAILS ERASE ####################')
	print('############################################################')
	print(list(projet_sample=input$detailsSample,
		table_rows=input$detailsTable_selected))
	tryCatch({
		if(is.null(values$table_clusterID)) custom_stop('invalid', 'not initialized ?????')
		else if(length(values$table_clusterID$ids) == 0) custom_stop('invalid', 'no cluster retrieve by cell selected')
		
		removeTarget(values$table_clusterID$ids)
		
		session$sendCustomMessage("detailsTableErase", NA)
		toastr_success('record erased')
	}, invalid = function(i){
		print(i$message)
		toastr_error(i$message)
	}, error = function(e){
		print(e$message)
		sendSweetAlert(e$message)
	})
	print('############################################################')
	print('######################### END DETAILS ERASE ################')
	print('############################################################')
})

removeTarget <- function(ids){
	query <- sprintf('delete from feature where cluster in (%s);', 
		paste(ids, collapse=','))
	print(query)
	dbExecute(db, query)
	query <- sprintf('delete from cluster where cluster in (%s);', 
		paste(ids, collapse=', '))
	print(query)
	dbExecute(db, query)
}

observeEvent(input$detailsEIC_selected, {
	print('############################################################')
	print('######################### DETAILS RE-INTEGRATE #############')
	print('############################################################')
	print(list(projet_sample=input$detailsSample,
		table_rows=input$detailsTable_selected))
	tryCatch({
		if(is.null(values$table_xr)) custom_stop('invalid', 'cannot load file')
		if(is.null(values$table_clusterID)) custom_stop('invalid', 'not initialized ?????')
		else if(values$table_clusterID$C == 0) custom_stop('invalid', 'no cell selected')
		else if(!is.numeric(input$detailsTolPpm)) custom_stop('invalid', 'ppm is not numeric')
		else if(is.null(input$detailsAdduct)) custom_stop('invalid', 'adduct picker not initialized')
		else if(input$detailsAdduct == "") custom_stop('invalid', 'no adduct for the file??')
		else if(is.null(input$detailsMachine)) custom_stop('invalid', 'machine picker not initialized')
		else if(input$detailsMachine == "") custom_stop('invalid', 'no machine')
		
		data <- forceTargetChloroPara(values$table_xr, input$detailsEIC_selected * 60, 
			values$table_clusterID$C, values$table_clusterID$Cl, 
			input$detailsSample, input$detailsTolPpm, adducts() %>% filter(
				adduct == input$detailsAdduct), input$detailsMachine, minPts)
		
		if(class(data) != "list") custom_stop('invalid', toString(data))
		if(length(values$table_clusterID$ids) > 0) removeTarget(values$table_clusterID$ids)
		recordOneTarget(data$features, data$clusters)		
		
		session$sendCustomMessage("updateDetailsTable", 
			if(input$detailsSwitch) round(min(data$clusters$score)) 
			else round(mean(data$clusters$rtMean)))
		toastr_success('success')
	}, invalid = function(i){
		print(i$message)
		toastr_error(i$message)
	}, error = function(e){
		print(e$message)
		sendSweetAlert(e$message)
	})
	print('############################################################')
	print('######################### END DETAILS RE-INTEGRATE #########')
	print('############################################################')
})

recordOneTarget <- function(features, clusters){
	query <- sprintf('insert into cluster (formula, C, Cl, score, rtMean, deviation, 
			ppm, peakwidth1, peakwidth2, adduct, machine, project_sample) values %s;', 
		paste(sprintf("(\"%s\", %s, %s, %s, %s, %s, %s, %s, %s, \"%s\", \"%s\", %s)", 
			clusters$formula, clusters$C, clusters$Cl, clusters$score, 
			clusters$rtMean, clusters$deviation, clusters$ppm, clusters$peakwidth1, 
			clusters$peakwidth2, clusters$adduct, clusters$machine, clusters$project_sample), collapse=', '))
	dbExecute(db, query)
	
	clusterIDs <- dbGetQuery(db, sprintf('select cluster from cluster where 
		project_sample == %s and formula == "%s";', unique(clusters$project_sample), 
		unique(clusters$formula)))$cluster
	features <- do.call(rbind, lapply(1:length(features), function(i) 
		features[[i]] %>% mutate(cluster = clusterIDs[i])))
	query <- sprintf('insert into feature (mz, mzmin, mzmax, rt, rtmin, rtmax, 
		\"into\", maxo, scale, scpos, scmin, scmax, lmin, lmax, sn, iso, cluster, abundance) 
		values %s;', paste(sprintf(
			"(%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, \"%s\", 
				%s, %s)", features$mz, features$mzmin, features$mzmax, features$rt, 
				features$rtmin, features$rtmax, features$into, features$maxo, 
				features$scale, features$scpos, features$scmin, features$scmax, features$lmin, 
				features$lmax, features$sn, features$iso, features$cluster, 
				features$abundance), collapse=', '))
	dbExecute(db, query)
}


forceIntegrate <- function(xr, mzRange, scRange, minPts){
	eic <- rawEIC(xr, mzrange = mzRange) %>% data.frame
	omz <- rawMat(xr, mzrange = mzRange) %>% data.frame %>% 
		mutate(scan = sapply(time, function(x) which(xr@scantime == x)))
	if(all(eic$intensity == 0)) return("flat eic")
	
	baseline <- runmed(eic$intensity, scRange[2]*3, 
			endrule="constant", algorithm="Turlach")
	noise <- eic %>% pull(intensity) %>% sd
	
	lm <- narrow_rt_boundaries(scRange, sum(scRange) / 2, 
		eic$intensity - baseline, minPts)
	if(length(lm) < 2) return("not enough consecutive points")

	mz.value <- omz %>% filter(scan >= lm[1] & scan <= lm[2] & 
		intensity > 0) %>% pull(mz)
	mz.int <- omz %>% filter(scan >= lm[1] & scan <= lm[2] & 
		intensity > 0) %>% pull(intensity)
	maxo <- max(mz.int)
	maxo.pos <- which.max(mz.int)
	if(length(mz.value) == 0) return("cannot get m/z values ???")
	mzrange <- range(mz.value)
	mz <- do.call(xcms:::mzCenter.wMean, list(mz = mz.value,
		intensity = mz.int))
	sn <- trapz(eic[lm[1]:lm[2], 'intensity'] - baseline[lm[1]:lm[2]]) / 
		trapz(rep(noise, diff(lm) + 1))
	if(sn < 1) return("signal under noise, too weak")
	
	data.frame(
		mz = mz, mzmin = mzrange[1], mzmax = mzrange[2], 
		rt = xr@scantime[sum(lm) / 2], 
		rtmin = xr@scantime[lm[1]],
		rtmax = xr@scantime[lm[2]],
		into = 	trapz(eic[lm[1]:lm[2], 'intensity']),
		maxo = maxo, scale = 0, scpos = 0, 
		scmin = 0, scmax = 0, lmin = lm[1],
		lmax = lm[2], sn = sn)
}

forceTargetChloroPara <- function(xr, rtRange, C, Cl, pj, ppm, adduct, app, minPts){
	H <- 2*C+2-Cl
	if(H < 0) return("impossible to compute formula (number of H negative)")
	formula <- paste('C', C, 'Cl', Cl, 'H', H, sep='')
	ionFormula <- getIonFormula(formula, adduct, 0)
	if(nrow(ionFormula) == 0) return("impossible to compute ion formula with this adduct")
	charge <- ionFormula$charge
	ionFormula <- ionFormula$ion_formula

	theoric <- theoricClustersFunction(ionFormula, 
		charge, app)[[1]] %>% arrange(desc(abundance)) %>% 
			dplyr::mutate(theoricIso = ceiling(mz), 
				theoricIso = theoricIso - theoricIso[1], 
				theoricIso = case_when(
					theoricIso < 0 ~ paste0('A', theoricIso), 
					theoricIso > 0 ~ paste0('A+', theoricIso), TRUE ~ 'A')) %>% 
		distinct(theoricIso, .keep_all=TRUE)
	mzRanges <- data.frame(mz = theoric$mz) %>% 
		mutate(tolMDa = mz * ppm * 10**-6, 
			mzmin = mz - tolMDa,
			mzmax = mz + tolMDa) %>% 
		select(mzmin, mzmax) %>% as.matrix
		
	scRange <- c(which.min(abs(xr@scantime - rtRange[1])),
		which.min(abs(xr@scantime - rtRange[2])))

	peaks <- data.frame()
	for(i in 1:nrow(mzRanges)){
		tmp <- forceIntegrate(xr, mzRanges[i, ], scRange, minPts)
		if(class(tmp) == "data.frame") peaks <- peaks %>% rbind(tmp %>% 
			mutate(iso = theoric[i, 'theoricIso']))
		else break
	}
	
	if("A" %in% peaks$iso & "A+2" %in% peaks$iso){
		# clusterize along rtmin & rtmax
		clusters <- data.frame(
			rtmin = peaks[1, 'rtmin'], 
			rtmax = peaks[1, 'rtmax'])
		peaks$cluster <- 0
		peaks[1, 'cluster'] <- 1
		for(i in 2:nrow(peaks)){
			ids <- which(sapply(1:nrow(clusters), function(j) 
				between(peaks[i, 'rt'], clusters[j, 'rtmin'], clusters[j, 'rtmax'])))
			if(length(ids) == 0){ 
				peaks[i, 'cluster'] <- max(peaks$cluster) + 1
				clusters <- clusters %>% rbind(peaks[i, c('rtmin', 'rtmax')])
			} else {
				peaks[i, 'cluster'] <- ids[1]
				clusters[ids[1], 'rtmin'] <- min(c(clusters[ids[1], 'rtmin'], peaks[i, 'rtmin']))
				clusters[ids[1], 'rtmax'] <- max(c(clusters[ids[1], 'rtmax'], peaks[i, 'rtmax']))
			}
		}
		clusters <- split(peaks, peaks$cluster) %>% 
			keep(function(x) "A" %in% x$iso & "A+2" %in% x$iso) %>% 
			map(function(x) x %>% mutate(abundance = into / max(into) * 100))
		if(length(clusters) == 0) return("cannot get A & A+2 in same cluster")
		
		list(features = clusters, clusters = 
			do.call(rbind, lapply(clusters, function(cluster) 
				data.frame(
					score = calculateScore(cluster, theoric, ppm, 100),
					rtMean = sum(cluster$rt * (cluster$into / sum(cluster$into))),
					deviation = calculateDeviation(cluster, theoric)))) %>% 
				mutate(formula = formula, C = C, Cl = Cl, adduct = adduct$adduct,
					ppm = ppm, peakwidth1 = 0, peakwidth2 = 0, 
					machine = app, project_sample = pj))
	} else "cannot get A & A+2"
}