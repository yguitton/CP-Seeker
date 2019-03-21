output$uiTargetSamples <- renderUI({
	pickerInput('targetSamples', 'samples', choices=samples()$sample, multiple=TRUE, options=list(
		`actions-box`=TRUE, `live-search`=TRUE))
})

output$uiTargetSampleTic <- renderUI({
	pickerInput('targetSampleTic', 'sample', choices=input$targetSamples, multiple=FALSE, width='50%')
})

output$targetTic <- renderPlotly({
	print('------------------- TARGET TIC -----------------')
	print(list(sample=input$targetSampleTic))
	tic <- tryCatch({
		if(is.null(input$targetSampleTic)) custom_stop('invalid', 'picker not initialized')
		else if(input$targetSampleTic == "") custom_stop('invalid', 'no file selected')
		raw <- read.raw(file.path(rawPath, paste(input$targetSampleTic, '.raw', sep='')))
		x <- raw$StartTime
		y <- raw$TIC
		plot_ly(type="scatter", mode="lines", x=x, y=y)
	}, invalid = function(i){
		print(paste(i$message))
		plot_ly(type="scatter", mode="lines")
	}, error = function(e){
		print(paste(e$message))
		sendSweetAlert(e$message)
		plot_ly(type="scatter", mode="lines")
	})
	print('------------------- END TARGET TIC -----------------')
	tic %>% layout(xaxis=list(title="Retention time(min)"),
			yaxis=list(title="Intensity"), showlegend=FALSE) %>%
		plotly::config(scrollZoom=TRUE, displaylogo=FALSE, 
			modeBarButtons=list(list('zoom2d', 'pan2d', 'autoScale2d', 'resetScale2d')))
})

observeEvent(input$target, {
	print('------------------- TARGET --------------------')
	print(list(project=input$project, samples=input$targetSamples, 
		adduct=input$targetAdduct, rtRange = input$targetRT, ppm=input$targetTolPpm, 
		peakwidth=input$targetPeakwidth, threshold=input$targetThreshold, 
		machine=names(resolution_list)[input$targetMachine %>% as.numeric]))
	tryCatch({
		inputs <- c('targetProject', 'targetSamples', 
			'targetAdduct', 'targetRT', 'targetTolPpm', 'targetPeakwidth',
			'targetMachine')
		conditions <- c(is.null(input$project), is.null(input$targetSamples),
			is.null(input$targetAdduct), length(input$targetRT) < 2, !is.numeric(input$targetTolPpm), 
			!is.numeric(input$targetPeakwidth), is.null(input$targetMachine))
		messages <- c('You need to select a project', 'You need to select at least one sample',
			'You need to select an adduct', 'You need to select a range in rT', 'tol ppm has to be numeric', 'min peakwidth has to be numeric',
			'You need to select a machine')
		test <- inputsTest(inputs, conditions, messages)
		if(!test) custom_stop('invalid', 'invalid args')
		inputs <- c('targetProject', 'targetSamples', 'targetAdduct', 'targetMachine')
		conditions <- c(input$project == "", length(input$targetSamples) == 0,
			input$targetAdduct == "", input$targetMachine == '', input$targetRT[2] < input$targetRT[1])
		messages <- c('You need to select a project', 'You need to select at least one sample',
			'You need to select an adduct', 'the range in rT is incorrect')
		test <- inputsTest(inputs, conditions, messages)
		if(!test) custom_stop('invalid', 'invalid args')
		
		progressSweetAlert(session, id='pb', title='Target', value=0, display_pct=TRUE)
		# load mzs of each chloroparaffin
		chloropara <- loadChloroParaMzs(input$targetAdduct, input$targetMachine %>% as.numeric)
		# then load eics for each file
		for(i in 1:length(input$targetSamples)){
			updateProgressBar(session, id="pb", title=paste("load eic", input$targetSamples[i]),
				value=0)
				
			# record the project_sample & delete precedent records
			project_sample <- project_samples() %>% 
				filter(sample == input$targetSamples[i] & project == input$project & 
					adduct == input$targetAdduct)
			if(nrow(project_sample) == 0){
				query <- sprintf('insert into project_sample (project, sample, adduct) values ("%s", "%s", "%s");',
					input$project, input$targetSamples[i], input$targetAdduct)
				print(query)
				dbSendQuery(db, query)
				query <- sprintf('select project_sample from project_sample where project == "%s" and
				sample == "%s" and adduct == "%s";', input$project, input$targetSamples[i], input$targetAdduct)
				project_sample <- dbGetQuery(db, query)$project_sample		
			} else {
				project_sample <- project_sample$project_sample
				query <- sprintf('delete from observed where project_sample == %s;
					delete from triangle where project_sample == %s; update project_sample 
					set zVal = null where project_sample == %s;',
					project_sample, project_sample, project_sample)
				print(query)
				dbSendQuery(db, query)			
			}
			
			path <- samples() %>% filter(sample == input$targetSamples[i]) %>% pull(path)
			
			eics <- readXICs(path, masses=chloropara$mz, tol=input$targetTolPpm)
			
			# rearrange eic from rawDiag to have a dataframe for each eic & not a list
			raw <- read.raw(path)
			rts <- raw$StartTime
			if(input$targetRT[1] > max(rts) | input$targetRT[2] < min(rts)){
				message <- sprintf('the rT range is out of the rt range of the file %s', input$targetSamples[i])
				toastr_warning(message)
				print(message)
				next
			}
			eics <- map(eics, function(eic) arrangeEICRawDiag(eic, rts))
			
			# split to analyze by chloroparaffin
			eics <- split(eics, chloropara$formula %>% as.factor)
			chloropara2 <- split(chloropara, chloropara$formula %>% as.factor)
			minScans <- floor((input$targetPeakwidth/60) / mean(diff(rts)))
			scRange <- which(rts >= input$targetRT[1] & rts <= input$targetRT[2])
			scRange <- c(min(scRange), max(scRange))
	
			rois <- data.frame(mz=c(), formula=c(), rt=c(), rtmin=c(), rtmax=c(), auc=c(), abd=c(), score=c())
			for(j in 1:length(eics)){
				updateProgressBar(session, id="pb", title=paste("targeting in", input$targetSamples[i]),
					value=j*100/length(eics))
				roisTmp <- targetChloroPara(eics[[j]], minScans, chloropara2[[j]], scRange, input$targetThreshold)
				if(nrow(roisTmp) > 0) rois <- rois %>% bind_rows(roisTmp %>% 
						cbind(formula=unique(chloropara2[[j]]$formula)))
			}
			rois$C <- str_extract(rois$formula, 'C[[:digit:]]+') %>% str_extract('[[:digit:]]+') %>% as.numeric
			rois$Cl <- str_extract(rois$formula, 'Cl[[:digit:]]+') %>% str_extract('[[:digit:]]+') %>% as.numeric
			rois$Cl[which(is.na(rois$Cl))] <- 0
			
			rois <- rois[which(rois$rtmin >= input$targetRT[1] & rois$rtmax <= input$targetRT[2]), ]
			
			if(nrow(rois) > 0){
				query <- sprintf('insert into observed (mz, formula, rt, rtmin, rtmax, auc, project_sample, ppm,
					peakwidth, C, Cl, abundance, score, machine, rangeRT_1, rangeRT_2, threshold) values %s;', 
						paste('(', rois$mz, ', "', rois$formula,
						'", ', rois$rt, ', ', rois$rtmin, ', ', rois$rtmax, ', ', rois$auc, 
						', ', project_sample, ', ', input$targetTolPpm, ', ', input$targetPeakwidth, 
						', ', rois$C, ', ', rois$Cl, ', ', rois$abd, ', ', rois$score, ', ', input$targetMachine %>% as.numeric, 
						', ', input$targetRT[1], ', ', input$targetRT[2], ', ', input$targetThreshold, ')', collapse=', ', sep=''))
				print(query)
				dbSendQuery(db, query)
			} else {
				message <- sprintf('no chloroparaffin detected in %s', input$targetSamples[i])
				print(message)
				toastr_warning(message)
			}
			actualize$project_samples <- TRUE
		}
		closeSweetAlert(session)
		}, invalid = function(i){
			print(i$message)
		}, error = function(e){
			print(e$message)
			sendSweetAlert(e$message)
		})
	print('------------------- END TARGET --------------------')
})

# load for each chloroparaffine with adduct two mz
# the formula for a chloroparaffine is C(n)H(2n+2-x)Cl(x)
loadChloroParaMzs <- function(adduct, machine=NULL, it=2){
	adduct <- adducts[which(adducts$Name == adduct), ]
	formulas <- c()
	for(C in minC:maxC) for(Cl in minCl:maxCl) formulas <- c(formulas, paste('C', C, 'H', 2*C+2-Cl, 'Cl', Cl, sep=''))
	# check formulas in case
	test <- check_chemform(isotopes, formulas)
	formulas <- test[which(!test$warning), 'new_formula']
	# then add adduct
	brute_formulas <- formulas
	if(adduct$Formula_add != FALSE) brute_formulas <- mergeform(brute_formulas, adduct$Formula_add)
	if(adduct$Formula_ded != FALSE){
		formulas <- formulas[which(check_ded(brute_formulas, adduct$Formula_ded) == "FALSE")]
		brute_formulas <- brute_formulas[which(check_ded(brute_formulas, adduct$Formula_ded) == "FALSE")]
		brute_formulas <- subform(brute_formulas, adduct$Formula_ded)
	}
	# remove those who have 0 in one element
	brute_formulas <- str_replace_all(brute_formulas, '[[:upper:]][[:lower:]]?0', '')
	formulas <- formulas[which(brute_formulas != '')]
	brute_formulas <- brute_formulas[which(brute_formulas != '')]
	data <- isopattern(isotopes, brute_formulas, charge=adduct$Charge, verbose=FALSE)
	if(!is.null(machine)){
		checked <- check_chemform(isotopes, brute_formulas)
		resolution <- getR(checked, plotit=FALSE, 
			resmass=resolution_list[[machine]])
		data <- envelope(data, resolution=resolution, verbose=FALSE)
		data <- vdetect(data, detect='centroid', plotit=FALSE, verbose=FALSE)
	}
	reduce(1:length(data), function(a, b) a %>% bind_rows(
		data[[b]] %>% data.frame %>% top_n(it, abundance) %>% arrange(desc(abundance)) %>% 
			mutate(mz = round(`m.z`, digits=5)) %>% select(mz, abundance) %>% 
			cbind(formula = formulas[b])), .init=data.frame())
}

findClosestMzs <- function(mzs, file, ppm, scan){
	mzO <- c()
	spectra <- readScans(file, scan)[[1]]
	spectra <- data.frame(mz=spectra$mZ, i=spectra$intensity)
	for(mz in mzs){
		tol <- mz * ppm * 10**-6
		mzO <- c(mzO, spectra[which(spectra$mz >= mz-tol & spectra$mz <= mz+tol), ] %>% 
			arrange(desc(i)) %>% top_n(1, i) %>% pull(mz))
	}
	mzO
}

arrangeEICRawDiag <- function(eic, rts){
	data <- data.frame(x=rts, y=0)
	data[which(data$x %in% eic$times), 'y'] <- eic$intensities[which(
		eic$times %in% data$x)]
	data
}

getAUC <- function(eic, roi, windowRTMed){
	baseline <- runmed(eic$y, windowRTMed, endrule="median", algorithm="Turlach")
	trapz(eic[roi, 'x'], eic[roi, 'y']) - 
		trapz(eic[roi, 'x'], baseline[roi])
}

targetChloroPara2 <- function(eic, roi, windowRTMed, minScans, scRange, threshold){
	baseline <- runmed(eic$y, windowRTMed, endrule="median", algorithm="Turlach")
	
	roi <- roi[which(sapply(roi, function(x) eic[x, 'y'] >= baseline[x] & eic[x, 'y'] > threshold))]
	if(length(roi) < minScans) return(data.frame(rtmin=NA, rtmax=NA, auc=0))
	
	# enlarge the ROI until the baseline
	minScan <- min(roi)
	while((baseline[minScan-1] < eic[minScan-1, 'y'] | 
		baseline[minScan-2] < eic[minScan-2, 'y']) & 
		minScan >= scRange[1]) minScan <- minScan-1
	minScan <- minScan-1
	maxScan <- max(roi)
	while((baseline[maxScan+1] < eic[maxScan+1, 'y'] | 
		baseline[maxScan+2] < eic[maxScan+2, 'y']) & 
		maxScan <= scRange[2]) maxScan <- maxScan+1
	maxScan <- maxScan+1
	roi <- minScan:maxScan
	
	data.frame(rtmin=eic[min(roi), 'x'], rtmax=eic[max(roi), 'x'], 
		auc=getAUC(eic, roi, windowRTMed))	
}

targetChloroPara <- function(eics, minScans, theo, scRange, threshold){
	tryCatch({
		res <- data.frame(mz=c(), rtmin=c(), rtmax=c(), auc=c(), score=c())
		
		eic <- eics[[1]]
		# search the ROI
		rois <- which(eic$y > mean(eic$y) + (sd(eic$y)) & eic$y > threshold)
		rois <- rois[which(rois >= scRange[1] & rois <= scRange[2])]
		rois <- split(rois, cumsum(c(TRUE, diff(rois) > 2)))
		rois <- rois[which(lengths(rois) > 1)]
		if(length(rois) == 0) custom_stop('fail', 'no chloroparaffin detected')
		rois <- map(rois, function(x) min(x):max(x))
		rois <- rois[which(lengths(rois) > minScans)]
		if(length(rois) == 0) custom_stop('fail', 'no chloroparaffin detected')
		
		windowRTMed <- 4*length(unlist(rois))
		if(windowRTMed %% 2 == 0) windowRTMed <- windowRTMed + 1
		
		for(roi in rois){
			tmpRes <- reduce(eics, function(a, b) 
				a %>% rbind(targetChloroPara2(b, roi, windowRTMed, minScans, scRange, threshold)), .init = data.frame())
			if(tmpRes[1, 'auc'] <= 0) custom_stop('fail', 'auc of A is O')
			if(tmpRes[2, 'auc'] <= 0) custom_stop('fail', 'auc of A2 is O')
			theo <- theo[which(tmpRes$auc > 0), ]
			tmpRes <- tmpRes[which(tmpRes$auc > 0), ]
			rts <- sapply(eics, function(i) apply(i[roi, ], 1, function(j) 
				j[1] * (j[2] / sum(i[roi, 2]))) %>% sum)
			abdObs <- sapply(tmpRes$auc, function(x) x * 100 / tmpRes[1, 'auc'])
			score <- (abdObs[2] - theo[2, 'abundance']) / theo[2, 'abundance'] * 100
			
			res <- res %>% bind_rows(data.frame(mz=theo$mz,
				rt=rts, rtmin=tmpRes$rtmin, rtmax=tmpRes$rtmax, 
				auc=tmpRes$auc, abd=abdObs, score=score))
		}
		res
	}, fail = function(f) data.frame()
	, error = function(e){
		print(e$message)
		sendSweetAlert(e$message)
		data.frame()
	})	
}