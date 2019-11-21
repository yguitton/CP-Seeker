param$details <- list(
	xr = NULL, 
	clusters = data.frame(), 
	theoric = list(
		pattern = data.frame(), 
		formula = "", 
		ion_formula = "",
		charge = 0,
		machine = "Elite_R240000@400", 
		ppm = 5
	)
)

output$uiDetailsSample <- renderUI({
	choices <- tryCatch({
		if(length(input$project) == 0) custom_stop('invalid', 'project picker is not yet initialized')
		else project_samples_adducts() %>% left_join(project_samples()) %>% 
			filter(project == input$project) %>% select(sampleID, project_sample)
	}, invalid = function(i) data.frame(sampleID = c(), project_sample = c())
	, error = function(e){
		print('ERR detailsSample')
		print(paste(e))
		data.frame(sampleID = c(), project_sample = c())
	})
	pickerInput('detailsSample', 'sample', choices=setNames(choices$project_sample, 
		choices$sampleID), multiple=FALSE, options=list(`live-search`=TRUE))
})

observeEvent(input$detailsSample, {
	param$details <- list(
		xr = loadMSFile(db, input$detailsSample), 
		clusters = data.frame(), 
		theoric = list(
			pattern = data.frame(), 
			formula = "", 
			ion_formula = "",
			charge = 0,
			machine = "Elite_R240000@400", 
			ppm = 5
		)
	)
})

output$uiDetailsAdduct <- renderUI({
	choices <- tryCatch({
		if(length(input$detailsSample) == 0) custom_stop('invalid', 'detailsSample picker is not yet initialized')
		else project_samples_adducts() %>% filter(project_sample == input$detailsSample) %>% 
			select(adduct, project_sample_adduct)
	}, invalid = function(i) data.frame(adduct = c(), project_sample_adduct = c())
	, error = function(e){
		print(paste(e))
		data.frame(adduct = c(), project_sample_adduct = c())
	})
	pickerInput('detailsAdduct', 'adduct', choices = setNames(
		choices$project_sample_adduct, choices$adduct), multiple=FALSE)
})

observeEvent(input$detailsAdduct, {
	param$details <- list(
		xr = param$details$xr,
		clusters = data.frame(), 
		theoric = list(
			pattern = data.frame(), 
			formula = "", 
			ion_formula = "",
			charge = 0,
			machine = "Elite_R240000@400", 
			ppm = 5
		)
	)
})

output$detailsTable <- DT::renderDataTable({
	actualize$project_samples_adducts
	data <- tryCatch({
		if(length(input$detailsAdduct) == 0) custom_stop('invalid', 'adduct picker not initialized')
		
		query <- sprintf('select * from cluster where project_sample_adduct == %s;', 
			input$detailsAdduct)
		print(query)
		data <- dbGetQuery(db, query) %>% dplyr::group_by(formula) %>% 
				dplyr::summarise(C=C[1], Cl=Cl[1], 
					rois=dplyr::n(),
					score = round(mean(score)), 
					rt = round(rtMean[1] / 60, 1),
					CCI = round(mean(CCI)),
					deviation = round(mean(deviation), 1)) %>% 
				data.frame
	
		res <- matrix(NA, nrow=maxC-minC+1, ncol=maxCl-minCl+1)
		for(row in 1:nrow(data)) res[data[row, 'C']-minC+1, data[row, 'Cl']-minCl+1] <- paste(data[row, 4:8], collapse="/")
		session$sendCustomMessage("dfUpdate", toJSON(res))
		res
	}, invalid = function(i) matrix(NA, nrow=maxC-minC+1, ncol=maxCl-minCl+1)
	, error = function(e){
		print(e$message)
		sendSweetAlert("Cannot display table of chloroparaffins", e$message)
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
			var switchVal = $('#detailsSwitch input').toArray().filter(x => x.checked)[0].value;
			
			var maxCols = table.row(0).data().length - 1;
			var maxRows = table.column(0).data().length - 1;
			for(var i = 0; i <= maxRows; i++){
				for(var j = 1; j <= maxCols; j++){
					var values = table.cell(i, j).data();
					if(values != null && values != undefined){
						values = values.split('/');
						var value = switchVal == 'score' ? values[1]
							: switchVal == 'rT' ? values[2]
							: switchVal == 'CCI' ? values[3]
							: values[4];
						table.cell(i, j).data(Number(value));
						if(values[1] < 70) $(table.cell(i, j).node()).css('background-color', 'rgb(255,36,0)');
						if(Number(values[0]) > 1) $(table.cell(i, j).node()).css('border', '5px solid orange');
					}
				}
			}
			table.columns.adjust();
		}
	")), callback = htmlwidgets::JS("
		function getFormula(table, obj){
			var id = obj.index();
			var C = Number(table.cell(id.row, 0).data().replace('C', ''));
			var Cl = Number(table.column(id.column).header().textContent.replace('Cl', ''));
			return {
				C : C, 
				Cl : Cl
			}
		}
		$(document).on('change', '#detailsSwitch input', function(){
			var switchVal = $('#detailsSwitch input').toArray().filter(x => x.checked)[0].value;
			var maxCols = df[0].length - 1;
			var maxRows = df.length - 1;
			for(var i = 0; i <= maxRows; i++){
				for(var j = 0; j <= maxCols; j++){
					var values = df[i][j];
					if(values != null && values != undefined){
						values = values.split('/');
						var value = switchVal == 'score' ? values[1]
							: switchVal == 'rT' ? values[2]
							: switchVal == 'CCI' ? values[3]
							: values[4];
						table.cell(i, j+1).data(Number(value));
						if(values[1] < 70) $(table.cell(i, j+1).node()).css('background-color', 'rgb(255,36,0)');
						if(Number(values[0]) > 1) $(table.cell(i, j+1).node()).css('border', '5px solid orange');
					}
				}
			}
			table.columns.adjust();
		});
		table.on('click', 'tbody td', function(){
			if($(this).hasClass('selected')){
				$(table.cells('.selected').nodes()).toggleClass('selected');
				Shiny.onInputChange('detailsTable_selected', {C: 0, Cl: 0, ignore: Math.random()});
			} else {
				$(table.cells('.selected').nodes()).toggleClass('selected');
				$(this).toggleClass('selected');
				var formula = getFormula(table, table.cell(this));
				if(formula.C == NaN || formula.Cl == NaN){
					Shiny.onInputChange('detailsTable_selected', {C: 0, Cl: 0, ignore: Math.random()});
				} else {
					Shiny.onInputChange('detailsTable_selected', 
						{C: formula.C, Cl: formula.Cl, ignore: Math.random()});
				}
			}
		});
		Shiny.addCustomMessageHandler('updateDetailsTable', function(message){
			var switchVal = $('#detailsSwitch input').toArray().filter(x => x.checked)[0].value;
			Shiny.onInputChange('detailsTable_selected', {C:0, Cl:0});
			values = message.split('/');
			if(values[1] < 50 ) var backColor = 'rgb(255,36,0)'
			else var backColor = 'rgba(0,0,0,0)'
			$(table.cell('.selected').node()).css('background-color', backColor);
			$(table.cell('.selected').node()).css('border', '');
			var value = switchVal == 'score' ? values[1]
				: switchVal == 'rT' ? values[2]
				: switchVal == 'CCI' ? values[3]
				: values[4];
			table.cell('.selected').data(value);
			var formula = getFormula(table, table.cell('.selected'));
			Shiny.onInputChange('detailsTable_selected', 
						{C: formula.C, Cl: formula.Cl});
			var cellInfo = table.cell('.selected').index();
			df[cellInfo.row][cellInfo.column - 1] = message;
		})
		Shiny.addCustomMessageHandler('detailsTableErase', function(message){
			table.cell('.selected').data(null);
			$(table.cell('.selected').node()).css('background-color', 'rgba(0,0,0,0)');
			$(table.cell('.selected').node()).css('border', '');
			var cellInfo = table.cell('.selected').index();
			df[cellInfo.row][cellInfo.column - 1] = null;
			
		})
	"))
	
observeEvent(input$detailsTable_selected, {
	tryCatch({
		if(length(input$detailsTable_selected) == 0) custom_stop('invalid', 
			'no cell selected')
		else if(input$detailsTable_selected$C == 0) custom_stop('invalid', 
			'no chloroparaffin selected')
		
		C <- input$detailsTable_selected$C
		Cl <- input$detailsTable_selected$Cl
		pja <- input$detailsAdduct
		param$details$clusters <- dbGetQuery(db, sprintf('select * from feature where cluster in (
			select cluster from cluster where project_sample_adduct == %s and 
				C == %s and Cl == %s);', pja, C, Cl))
		
		info <- dbGetQuery(db, sprintf('select * from cluster where 
			project_sample_adduct == %s and C == %s and Cl == %s;', 
			pja, C, Cl))
		if(nrow(info) > 0){
			param$details$theoric <- list(
				pattern = data.frame(),
				formula = info$formula, 
				ion_formula = info$ion_formula, 
				charge = info$charge, 
				machine = info$machine,
				ppm = info$ppm
			)
			updateNumericInput(session, 'detailsTolPpm', 'tol ppm', value=info$ppm)
			updatePickerInput(session, 'detailsMachine', 'machine', choices=
					names(resolution_list), selected = info$machine)
		} else {
			formula <- paste0("C", C, "Cl", Cl, "H", 2 * C + 2 - Cl)
			test <- check_chemform(isotopes, formula)
			if(test$warning) custom_stop('minor_error', 'incorrect chloroparaffin formula')
			adductName <- project_samples_adducts() %>% filter(
				project_sample_adduct == input$detailsAdduct) %>% pull(adduct)
			adduct <- adducts() %>% filter(adduct == adductName)
			ion_formula = getIonFormula(formula, adduct)
			if(nrow(ion_formula) > 0){
				param$details$theoric$formula <- ion_formula$formula
				param$details$theoric$ion_formula <- ion_formula$ion_formula
				param$details$theoric$charge <- ion_formula$charge
			} else custom_stop('minor_error', 'incorrect chloroparaffin formula with this adduct')
		}
	}, invalid = function(i) param$details <- list(
		xr = param$details$xr, 
		clusters = data.frame(), 
		theoric = list(
			pattern = data.frame(), 
			formula = "", 
			ion_formula = "", 
			charge = 0, 
			machine = "Elite_R240000@400", 
			ppm = 5
		))
	, minor_error = function(e){
		print(e)
		toastr_error(paste(e$message))
	}, error = function(e){
		print(e)
		sendSweetAlert("Cannot retrieve information about chloroparaffin selected", 
			paste(e$message))
		param$details <- list(
			xr = param$details$xr, 
			clusters = data.frame(), 
			theoric = list(
				pattern = data.frame(), 
				formula = "", 
				ion_formula = "", 
				charge = 0, 
				machine = "Elite_R240000@400", 
				ppm = 5
			))
	})
})

observeEvent(input$detailsMachine, param$details$theoric$machine <- input$detailsMachine)
observeEvent(input$detailsTolPpm, {
	tryCatch({
		inputs <- 'detailsTolPpm'
		titles <- 'ppm'
		conditions <- is.na(input$detailsTolPpm)
		messages <- 'ppm need to be a numeric'
		if(!inputsTest(inputs,  conditions, titles, messages)) custom_stop('invalid', 'ppm is not numeric')
		conditions <- input$detailsTolPpm < 0
		messages <- 'ppm need to be positive'
		if(!inputsTest(inputs, conditions, titles, messages)) custom_stop('invalid', 'ppm is negative')
		
		param$details$theoric$ppm <- input$detailsTolPpm
	}, invalid = function(i) param$details$theoric$ppm <- 0
	, error = function(e){
		print(e)
		sendSweetAlert('value of ppm is understandable', paste(e$message))
		param$details$theoric$ppm <- 0
	})
})

observeEvent(param$details$theoric, {
	tryCatch({
		ion_formula <- param$details$theoric$ion_formula
		charge <- param$details$theoric$charge
		machine <- param$details$theoric$machine
		ppm <- param$details$theoric$ppm
		
		if(ion_formula == "" | charge == 0 | machine == "" | ppm == 0) custom_stop('invalid', 
			'invalid args')
		param$details$theoric$pattern <- theoricClustersFunction(ion_formula, charge, machine)[[1]] %>% 
			arrange(desc(abundance)) %>% mutate(
				tolMDa =  mz * ppm * 10**-6, 
				mzmin = mz - tolMDa, 
				mzmax = mz + tolMDa,
				theoricIso = round(mz - mz[1]),
				theoricIso = case_when(
					theoricIso < 0 ~ paste0("A", theoricIso),
					theoricIso > 0 ~ paste0("A+", theoricIso),
					TRUE ~ "A")) %>% distinct(theoricIso, .keep_all = TRUE)
	}, invalid = function(i) param$details$theoric$pattern <- data.frame()
	, error = function(e){
		print(e)
		sendSweetAlert("Something weird happen", paste(e$message))
		param$details$theoric$pattern <- data.frame()
	})
})

output$detailsMS <- renderPlotly({
	tryCatch({
		if(nrow(param$details$theoric$pattern) == 0) custom_stop('invalid', 
			'theoric pattern not initialized yet')
		
		plotMS(param$details$clusters, param$details$theoric$pattern)
	}, invalid = function(i) plotEmptyMS()
	, error = function(e){
		print('ERR detailsMS')
		print(e)
		toastr_error("Cannot draw chloroparaffin mass spectrum", paste(e$message))
		plotEmptyMS()
	})
})

output$detailsEIC <- renderPlotly({
	tryCatch({
		if(nrow(param$details$theoric$pattern) == 0) custom_stop('invalid', 
			'theoric pattern not initialized yet')
		
		plotClusterEIC(param$details$clusters, param$details$theoric$pattern, 
			param$details$xr)
	}, invalid = function(i) plotEmptyChromato()
	, error = function(e){
		print('ERR detailsEIC')
		print(e)
		toastr_error("Cannot draw chloroparaffin EIC", paste(e$message))
		plotEmptyChromato()
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
	print(list(table_rows=input$detailsTable_selected))
	tryCatch({
		if(nrow(param$details$clusters) == 0) custom_stop('invalid', 'no clusters')
		
		removeTarget(param$details$clusters$cluster %>% unique)
		
		param$details$clusters <- data.frame()
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
		if(is.null(param$details$xr)) custom_stop('invalid', 'cannot load file')
		else if(nrow(param$details$theoric$pattern) == 0) custom_stop('invalid', 
			'cannot compute theoretical pattern of chlorparaffin')
		else if(is.null(input$detailsEIC_selected)) custom_stop('invalid', 'no rt range selected')
		else if(length(input$detailsEIC_selected) != 2) custom_stop('invalid', 'no rt range selected')
		
		theoricPattern <- param$details$theoric$pattern 
		rtRange <- input$detailsEIC_selected * 60
		scRange <- c(which.min(abs(param$details$xr@scantime - rtRange[1])),
			which.min(abs(param$details$xr@scantime - rtRange[2])))
		peaks <- data.frame()
		for(row in 1:nrow(theoricPattern)){
			tmp <- forceTargetChloroPara(param$details$xr, 
				as.double(theoricPattern[row, c('mzmin', 'mzmax')]), 
				scRange, missingScans)
			if(nrow(tmp) > 0) peaks <- peaks %>% rbind(
				tmp %>% mutate(iso = theoricPattern[row, 'theoricIso']))
			else break
		}
		
		if(nrow(peaks) > 1){
			peaks <- peaks %>% mutate(abundance = into / max(into) * 100)
			info <- data.frame(
					score = calculateScore(peaks, theoricPattern, param$details$theoric$ppm, 100),
					cci = calculateCCI(peaks, theoricPattern, param$details$theoric$ppm),
					rtMean = sum(peaks$rt * (peaks$into / sum(peaks$into))),
					deviation = calculateDeviation(peaks, theoricPattern, param$details$theoric$ppm)) %>% 
				mutate(formula = param$details$theoric$formula, 
					ion_formula = param$details$theoric$ion_formula, 
					charge = param$details$theoric$charge, 
					C = input$detailsTable_selected$C, 
					Cl = input$detailsTable_selected$Cl, 
					adduct = project_samples_adducts() %>% 
						filter(project_sample_adduct == input$detailsAdduct) %>% 
						pull(adduct),
					ppm = param$details$theoric$ppm, peakwidth1 = 0, 
					peakwidth2 = 0, machine = param$details$theoric$machine, 
					project_sample_adduct = input$detailsAdduct)
			recordOneTarget(peaks, info)
			toastr_success('success')
			session$sendCustomMessage("updateDetailsTable", 
				paste("1", round(info$score), 
					round(info$rtMean / 60, 1), 
					round(info$cci), 
					round(info$deviation, 1), sep='/'))
		} else {
			toastr_error("Cannot integrate chloroparaffin selected")
			removeTarget(unique(param$details$clusters$cluster))
			session$sendCustomMessage("detailsTableErase", NA)
		}	
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

recordOneTarget <- function(features, cluster){
	query <- sprintf('insert into cluster (formula, ion_formula, charge, 
			C, Cl, score, rtMean, deviation, CCI, 
			ppm, peakwidth1, peakwidth2, machine, project_sample_adduct) values 
			(\"%s\", \"%s\", %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, \"%s\", %s);', 
			cluster$formula, cluster$ion_formula, cluster$charge, cluster$C, cluster$Cl, cluster$score, 
			cluster$rtMean, cluster$deviation, cluster$cci, cluster$ppm, cluster$peakwidth1, 
			cluster$peakwidth2, cluster$machine, cluster$project_sample_adduct)
	print(query)
	dbExecute(db, query)
	
	features$cluster <- dbGetQuery(db, sprintf('select cluster from cluster where 
		project_sample_adduct == %s and formula == "%s";', 
		cluster$project_sample_adduct, cluster$formula))$cluster
	query <- sprintf('insert into feature (mz, mzmin, mzmax, rt, rtmin, rtmax, 
		\"into\", maxo, scale, scpos, scmin, scmax, lmin, lmax, sn, iso, cluster, abundance) 
		values %s;', paste(sprintf(
			"(%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, \"%s\", 
				%s, %s)", features$mz, features$mzmin, features$mzmax, features$rt, 
				features$rtmin, features$rtmax, features$into, features$maxo, 
				features$scale, features$scpos, features$scmin, features$scmax, features$lmin, 
				features$lmax, features$sn, features$iso, features$cluster, 
				features$abundance), collapse=', '))
	print(query)
	dbExecute(db, query)
}
