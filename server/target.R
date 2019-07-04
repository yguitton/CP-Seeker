output$uiTargetSamples <- renderUI({
	choices <- tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project is not yet initialized')
		else if(input$project == '') custom_stop('invalid', 'no project')
		else project_samples() %>% filter(project == input$project) %>% 
			select(sampleID, project_sample)
	}, invalid = function(i){
		print(paste(i))
		data.frame(project_sample=c(), sampleID=c())
	}, error = function(e){
		print(paste(e))
		sendSweetAlert(paste(e$message))
		data.frame(project_sample=c(), sampleID=c())
	})	
	pickerInput('targetSamples', 'samples', choices=setNames(choices$project_sample, choices$sampleID), 
		multiple=TRUE, options=list(`actions-box`=TRUE, `live-search`=TRUE))
})

output$uiTargetSampleTic <- renderUI({
	choices <- tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project is not yet initialized')
		else if(input$project == '') custom_stop('invalid', 'no project')
		else project_samples() %>% filter(project == input$project) %>% 
			select(sampleID, sample)
	}, invalid = function(i){
		print(paste(i))
		data.frame(sample=c(), sampleID=c())
	}, error = function(e){
		print(paste(e))
		sendSweetAlert(paste(e$message))
		data.frame(sample=c(), sampleID=c())
	})	
	pickerInput('targetSampleTic', 'sample', choices=setNames(choices$sample, choices$sampleID), 
		multiple=FALSE, width='50%')
})

output$targetTic <- renderPlotly({
	print('------------------- TARGET TIC -----------------')
	print(list(sample=input$targetSampleTic))
	tic <- tryCatch({
		if(is.null(input$targetSampleTic)) custom_stop('invalid', 'picker not initialized')
		else if(input$targetSampleTic == "") custom_stop('invalid', 'no file selected')
		path <- samples() %>% filter(sample == input$targetSampleTic) %>% pull(path)
		msFile <- xcmsRaw(path, mslevel=1)
		x <- msFile@scantime / 60
		y <- msFile@tic
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

actualize$targetSuccess <- data.frame(samples=c(), success=c())
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
		
		# record the project_sample & delete precedent records
		updateProgressBar(session, id="pb", title="delete precedent records", value=0)
		print('delete precedent records')
		query <- sprintf('delete from observed where project_sample in (%s);
			delete from triangle where project_sample in (%s); update project_sample 
			set zVal = null where project_sample in (%s);',
			paste(input$targetSamples, collapse=', '), 
			paste(input$targetSamples, collapse=', '),
			paste(input$targetSamples, collapse=', '))
		print(query)
		dbSendQuery(db, query)
		# add new record
		query <- sprintf('update project_sample set adduct = "%s" where project_sample in (%s);',
			input$targetAdduct, paste(input$targetSamples, collapse=', '))
		print(query)
		dbSendQuery(db, query)
		
		# load mzs of each chloroparaffin
		print('load mzs')
		updateProgressBar(session, id="pb", title="compute cholorpara mzs", value=0)
		chloropara <- loadChloroParaMzs(input$targetAdduct, input$targetMachine %>% as.numeric)
		chloropara <- lapply(chloropara, function(x) x %>% 
			dplyr::mutate(tolMDa = mz * input$targetTolPpm * 10**-6) %>%
			dplyr::mutate(mzmin = mz - tolMDa, mzmax = mz + tolMDa) %>% select(-tolMDa))
		
		success <- c()
		for(i in 1:length(input$targetSamples)){
			print(sprintf('init %s', input$targetSamples[i]))
			path <- samples() %>% filter(sample == project_samples() %>% 
				filter(project_sample == input$targetSamples[i]) %>% pull(sample)) %>% 
				pull(path)
			msFile <- xcmsRaw(path, mslevel=1)
			
			# restrict according limits on m/z
			print('restrict')
			chloroparaLimited <- keep(chloropara, function(x)
				x[1, 'mzmin'] >= msFile@mzrange[1] &
					x[1, 'mzmax'] <= msFile@mzrange[2])
				
			print('draw eics')
			updateProgressBar(session, id="pb", title=
				sprintf("draw eics in %s", input$targetSamples[i]), 
				value=i*100/length(input$targetSamples))
			eics <- lapply(1:length(chloroparaLimited), function(j)
				rawEIC(msFile, mzrange=chloroparaLimited[[j]][1, c('mzmin', 'mzmax')] %>% 
					as.matrix, rtrange=input$targetRT * 60))
			eics <- lapply(eics, function(eic) arrangeEics(eic, msFile))
					
			minScans <- floor((input$targetPeakwidth) / mean(diff(msFile@scantime)))
			
			print('detect rois')
			updateProgressBar(session, id="pb", title=
				sprintf("detect chloroparaffin in %s", input$targetSamples[i]), 
				value=i*100/length(input$targetSamples))
			rois <- purrr::reduce(1:length(eics), function(a,b) a %>% rbind(
				targetChloroPara(msFile, eics[[b]], minScans, input$targetThreshold, chloroparaLimited[[b]])),
				.init=data.frame())
			
			if(nrow(rois) > 0){
				print('record rois')
				rois$C <- str_extract(rois$formula, 'C[[:digit:]]+') %>% str_extract('[[:digit:]]+') %>% as.numeric
				rois$Cl <- str_extract(rois$formula, 'Cl[[:digit:]]+') %>% str_extract('[[:digit:]]+') %>% as.numeric
				rois$Cl[which(is.na(rois$Cl))] <- 0
			
				query <- sprintf('insert into observed (mz, formula, rt, rtmin, rtmax, auc, project_sample, ppm,
					peakwidth, C, Cl, abundance, score, machine, rangeRT_1, rangeRT_2, threshold, roiNb, annotation) values %s;', 
						paste('(', rois$mz, ', "', rois$formula,
						'", ', rois$rt, ', ', rois$rtmin, ', ', rois$rtmax, ', ', rois$auc, 
						', ', input$targetSamples[i], ', ', input$targetTolPpm, ', ', input$targetPeakwidth, 
						', ', rois$C, ', ', rois$Cl, ', ', rois$abd, ', ', rois$score, ', ', input$targetMachine %>% as.numeric, 
						', ', input$targetRT[1], ', ', input$targetRT[2], ', ', input$targetThreshold, ', ', rois$roiNb, 
						', "', rois$annotation, '")', collapse=', ', sep=''))
				print(query)
				dbSendQuery(db, query)
				success[i] <- sprintf("detect %s chloroparaffins", length(unique(rois$formula)))
			} else {
				success[i] <- "no chloroparaffin detected"
				message <- sprintf('no chloroparaffin detected in %s', input$targetSamples[i])
				print(message)
				toastr_warning(message)
			}
		}
		print(success)
		actualize$project_samples <- TRUE
		actualize$targetSuccess <- data.frame(samples=project_samples() %>% 
			filter(project_sample %in% input$targetSamples) %>% pull(sampleID), success=success)
		closeSweetAlert(session)
		showModal(modalDialog(title="Result of targeting", easyClose=TRUE, 
			dataTableOutput('targetSuccessTable'), footer=modalButton('Close')))
		}, invalid = function(i){
			print(i$message)
			actualize$targetSuccess <- data.frame(samples=c(), success=c())
		}, error = function(e){
			print(e$message)
			sendSweetAlert(e$message)
			actualize$targetSuccess <- data.frame(samples=c(), success=c())
		})
	print('------------------- END TARGET --------------------')
})

output$targetSuccessTable <- DT::renderDataTable({
	tryCatch({
		data <- actualize$targetSuccess
		if(nrow(data) == 0) return(data)
		data$success <- sapply(data$success, function(x) 
			if(x == "no chloroparaffin detected") paste("<div style=\"background-color: #FDCDAC;\">",
				x, "<div/>") else paste("<div style=\"background-color: #B3E2CD;\">", x))
		data
	}, error = function(e){
		print(paste(e))
		data.frame(samples=c(), success=c())
	})
}, escape=FALSE, rownames=FALSE, selection='none', class='compact nowrap', 
options=list(dom='frtip', bFilter=FALSE, ordering=FALSE))

# load for each chloroparaffine with adduct two mz
# the formula for a chloroparaffine is C(n)H(2n+2-x)Cl(x)
loadChloroParaMzs <- function(adduct, machine=NULL){
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
	data <- lapply(1:length(data), function(i) data[[i]] %>% data.frame %>% 
		arrange(desc(abundance)) %>% dplyr::mutate(mz = round(`m.z`, digits=5)) %>% 
		select(mz, abundance) %>% cbind(formula = formulas[i]))
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

arrangeEics <- function(eic, msFile){
	eic <- eic %>% as.data.frame
	eic$scan <- msFile@scantime[eic$scan]
	colnames(eic) <- c('x', 'y')
	eic
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

getAnnotations <- function(mzs){
	annotations <- sapply(mzs[-1], function(x) round(x - mzs[1]))
	c('A', sapply(annotations, function(x) 
		if(x < 0) paste0('A', x) else paste0('A+', x)))
}

computeScore <- function(obs, theo){
	theo <- theo[1:length(obs)]
	weights <- sapply(theo, function(x)
		x / sum(theo))
	sapply(1:length(obs), function(i) 
		(obs[i] - theo[i]) / theo[i] * weights[i]) %>% sum * 100
}

targetChloroPara2 <- function(msFile, mzs, roi, windowRTMed, minScans, threshold, aucCheck){
	eic <- rawEIC(msFile, mzrange=mzs[, c('mzmin', 'mzmax')] %>% as.matrix)
	eic <- arrangeEics(eic, msFile)
	baseline <- runmed(eic$y, windowRTMed, endrule="median", algorithm="Turlach")
	
	roi <- roi[which(sapply(roi, function(x) eic[x, 'y'] >= baseline[x] & eic[x, 'y'] > threshold))]
	rois <- split(roi, cumsum(c(TRUE, diff(roi) > 3)))
	rois <- rois[which(lengths(rois) > 1)]
	if(length(rois) == 0) return(data.frame())
	roi <- rois[which(lengths(rois) == max(lengths(rois)))][[1]]
	roi <- min(roi):max(roi)
	if(length(roi) < minScans) return(data.frame())
		
	# enlarge the ROI until the baseline
	minScan <- tryCatch({
		minScan <- min(roi)
		while(baseline[minScan-1] < eic[minScan-1, 'y'] | 
			baseline[minScan-2] < eic[minScan-2, 'y']) minScan <- minScan-1
		minScan-1
	}, error = function(e) 1)
	maxScan <- tryCatch({
		maxScan <- max(roi)
		while(baseline[maxScan+1] < eic[maxScan+1, 'y'] | 
		baseline[maxScan+2] < eic[maxScan+2, 'y']) maxScan <- maxScan+1
		maxScan+1
	}, error = function(e) length(baseline))
	roi <- minScan:maxScan
	
	auc <- getAUC(eic, roi, windowRTMed)
	if(auc == 0 | auc > aucCheck * 1.2) return(data.frame())
	data.frame(mz=mzs$mz, rtmin=eic[min(roi), 'x'], rtmax=eic[max(roi), 'x'], 
		auc=auc, rt=apply(eic[roi, ], 1, function(i)
			i[1] * (i[2] / sum(eic[roi, 2]))) %>% sum)	
}

targetChloroPara <- function(msFile, eic, minScans, threshold, theo){
	tryCatch({
		res <- data.frame(mz=c(), rtmin=c(), rtmax=c(), auc=c(), score=c())
		
		# search the ROI
		rois <- which(eic$y > mean(eic$y, trim=.05) & eic$y > threshold)
		rois <- split(rois, cumsum(c(TRUE, diff(rois) > 3)))
		rois <- rois[which(lengths(rois) > 1)]
		if(length(rois) == 0) custom_stop('fail', 'no chloroparaffin detected')
		rois <- purrr::map(rois, function(x) min(x):max(x))
		rois <- rois[which(lengths(rois) > minScans)]
		if(length(rois) == 0) custom_stop('fail', 'no chloroparaffin detected')
		
		windowRTMed <- 1 + 2 * min(
			(length(unlist(rois))*12-1)%/% 2, 
			ceiling(0.1*length(unlist(rois))*12))
		
		# for(roi in rois){
		for(i in 1:length(rois)){
			roi <- rois[[i]]
			mzs <- theo[, c('mz', 'mzmin', 'mzmax')]
			tmpRes2 <- targetChloroPara2(msFile, mzs[1, ], roi, 
				windowRTMed, minScans, threshold, Inf)
			tmpRes <- data.frame(mz=c(), rtmin=c(), rtmax=c(), auc=c(), score=c())
			while(nrow(tmpRes2) > 0){
				tmpRes <- rbind(tmpRes, tmpRes2)
				mzs <- mzs[-1, ]
				tmpRes2 <- targetChloroPara2(msFile, mzs[1, ], roi, 
					windowRTMed, minScans, threshold, tmpRes2$auc)
			}
			if(nrow(tmpRes) < 2) next
			
			abdObs <- sapply(tmpRes$auc, function(x) x * 100 / tmpRes[1, 'auc'])
			score <- computeScore(abdObs[-1], theo$abundance[-1])
			annotations <- getAnnotations(tmpRes$mz)
			
			res <- res %>% bind_rows(data.frame(mz=tmpRes$mz,
				rt=tmpRes$rt, rtmin=tmpRes$rtmin, rtmax=tmpRes$rtmax, 
				auc=tmpRes$auc, abd=abdObs, score=score, formula=theo[1, 'formula'], 
				roiNb = i, annotation = annotations))
		}
		res
	}, fail = function(f) data.frame()
	, error = function(e){
		print(e$message)
		toastr_error(e$message)
		data.frame()
	})	
}