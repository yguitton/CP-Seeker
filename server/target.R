output$uiTargetSamples <- renderUI({
	choices <- tryCatch({
		if(length(input$project) == 0) custom_stop('invalid', 'input$project not initialized')
		else project_samples() %>% filter(project == input$project) %>%
			select(sampleID, project_sample)
	}, invalid = function(i) data.frame(sampleID=c(), project_sample=c())
	, error = function(e){
		print('ERR targetSamples')
		print(e)
		data.frame(sampleID=c(), project_sample=c())
	})
	pickerInput('targetSamples', 'samples', choices=setNames(
		choices$project_sample, choices$sampleID), 
		multiple=TRUE, options=list(`actions-box`=TRUE, `live-search`=TRUE))
})

output$uiTargetSampleTic <- renderUI({
	choices <- tryCatch({
		if(length(input$project) == 0) custom_stop('invalid', 'input$project not initialized')
		else project_samples() %>% filter(project == input$project) %>%
			select(sampleID, project_sample)
	}, invalid = function(i) data.frame(sampleID=c(), project_sample=c())
	, error = function(e){
		print('ERR targetSamples')
		print(e)
		data.frame(sampleID=c(), project_sample=c())
	})
	pickerInput('targetSampleTic', 'sample', choices=setNames(choices$project_sample, choices$sampleID), 
		multiple=FALSE, width='50%')
})

output$targetTic <- renderPlotly({
	tryCatch({
	if(length(input$targetSampleTic) == 0) custom_stop('invalid', 'targetSampleTic is empty')
	
	#to force the re-actualization after peak picking
	actualize$project_samples
	plotChromato(db, input$targetSampleTic)
	}, invalid = function(i) plotEmptyChromato()
	, error = function(e){
		print('ERR targetTIC')
		print(e)
		toastr_error('Cannot display TIC of the file', paste(e$message))
		plotEmptyChromato()
	})
})

actualize$targetSuccess <- data.frame(samples=c(), success=c())
observeEvent(input$target, {
	print('############################################################')
	print('######################### TARGET ###########################')
	print('############################################################')
	print(list(project_samples=input$targetSamples, 
		adducts=input$targetAdducts, rtRange = input$targetRT, ppm=input$targetTolPpm, 
		peakwidth=input$targetPeakwidth, 
		machine=input$targetMachine))
	tryCatch({
		inputs <- c('targetSamples', 'targetAdducts')
		titles <- c('samples', 'adducts')
		conditions <- c(length(input$targetSamples) == 0, length(input$targetAdducts) == 0)
		messages <- c('You need to select at least one sample',	'You need to select at least one adduct')
		test <- inputsTest(inputs, conditions, titles, messages)
		if(!test) custom_stop('invalid', 'invalid args')
		
		progressSweetAlert(session, id='pb', title='Target', value=0, display_pct=TRUE)
		# record the project_sample & delete precedent records
		updateProgressBar(session, id="pb", title="delete precedent records", value=0)
		print('delete precedent records')
		deleteProject_samples_adducts(input$targetSamples, input$targetAdducts)
		
		# load all theoricPatterns
		updateProgressBar(session, id="pb", title="Load theoretic chloroparaffin patterns", value=0)
		theorics <- expand.grid(minC:maxC, minCl:maxCl)
		colnames(theorics) <- c('C', 'Cl')
		theorics <- do.call(rbind, lapply(input$targetAdducts, function(x) 
			theorics %>% mutate(H = 2 * C + 2 - Cl) %>% 
				filter(H >= 0) %>% 
				mutate(formula = paste0("C", C, "Cl", Cl, "H", H),
					charge = 0,
					adduct = x,
					ion_formula = getIonFormula(formula, adducts() %>% filter(adduct == x), 
						charge)$ion_formula, 
					charge = adducts() %>% filter(adduct == x) %>% pull(charge))))
		theoricPatterns <- theoricClustersFunction(theorics$ion_formula, 
			theorics$charge, input$targetMachine) %>% lapply(function(x) 
				x %>% arrange(desc(abundance)) %>% 
					mutate(tolMDa = mz * input$targetTolPpm * 10**-6,
						mzmin = mz - tolMDa,
						mzmax = mz + tolMDa,
						theoricIso = round(mz - mz[1]),
						theoricIso = case_when(
							theoricIso < 0 ~ paste0("A", theoricIso),
							theoricIso > 0 ~ paste0("A+", theoricIso),
							TRUE ~ "A")) %>% 
						distinct(theoricIso, .keep_all = TRUE))

		pbValMax <- length(input$targetSamples) * nrow(theorics)
		pbVal <- 0
		
		success <- c()
		res <- list()
		resInfo <- data.frame()
		for(pj in input$targetSamples){
			print(paste('--------------', pj, '------------'))
			updateProgressBar(session, id='pb', title=sprintf("Targeting in %s", project_samples() %>% 
						filter(project_sample == pj) %>% pull(sampleID)),
						value=pbVal * 100 / pbValMax)
			
			xr <- loadMSFile(db, pj)
			if(!any(class(xr) == "xcmsRaw")){
				success <- c(success, 0)
				pbVal <- pbVal + nrow(theorics)
				next
			}
			
			nbChloroParaf <- 0
			scalerange <- round((input$targetPeakwidth / mean(diff(xr@scantime))) / 2)
			scRange <- round(input$targetRT * 60 / mean(diff(xr@scantime)))
			for(row in 1:nrow(theorics)){
				print(row)
				theoric <- theorics[row, ]
				theoricPattern <- theoricPatterns[[row]]
				
				peaksA <- suppressWarnings(targetChloroPara(xr, scalerange, 
					as.double(theoricPattern[1, c('mzmin', 'mzmax')]), scRange, 
						FALSE, missingScans))
				if(nrow(peaksA) > 0){
					peaksA$iso <- 'A'
					for(row2 in 1:nrow(peaksA)){
						peaks <- peaksA[row2, ]
						for(row3 in 2:nrow(theoricPattern)){
							tmp <- suppressWarnings(forceTargetChloroPara(xr, 
								as.double(theoricPattern[row3, c('mzmin', 'mzmax')]), 
								as.double(peaksA[row2, c('lmin', 'lmax')]), 
								missingScans))
							if(nrow(tmp) > 0) peaks <- peaks %>% rbind(tmp %>% 
								mutate(iso = theoricPattern[row3, 'theoricIso']))
							else break
						}				
						if(nrow(peaks) > 1){
							peaks <- peaks %>% mutate(abundance = into / max(into) * 100)
							res <- res %>% append(list(peaks))
							resInfo <- resInfo %>% rbind(
								data.frame(
									score = calculateScore(peaks, theoricPattern, input$targetTolPpm, 100),
									cci = calculateCCI(peaks, theoricPattern, input$targetTolPpm),
									rtMean = sum(peaks$rt * (peaks$into / sum(peaks$into))),
									deviation = calculateDeviation(peaks, theoricPattern, input$targetTolPpm)) %>% 
								cbind(theoric[, c('formula', 'ion_formula', 'charge', 'C', 'Cl', 'adduct')]) %>% 
								mutate(ppm = input$targetTolPpm, peakwidth1 = input$targetPeakwidth[1], 
									peakwidth2 = input$targetPeakwidth[2], machine = input$targetMachine, 
									project_sample = as.numeric(pj)))
						}
					}
				}
				pbVal <- pbVal + 1
				updateProgressBar(session, id='pb', title=sprintf("Targeting in %s", project_samples() %>% 
						filter(project_sample == pj) %>% pull(sampleID)),
						value=pbVal * 100 / pbValMax)
			}
			rm(xr)
			gc()
			success <- c(success, nrow(resInfo %>% filter(project_sample == pj)))
		}
		print(success)
		if(length(res) > 1) recordTarget(res, resInfo)
		actualize$project_samples <- TRUE
		actualize$targetSuccess <- data.frame(samples=project_samples() %>% 
			filter(project_sample %in% input$targetSamples) %>% pull(sampleID), 
			success=success)
		closeSweetAlert(session)
		showModal(modalDialog(title="Result of targeting", easyClose=TRUE, 
			DT::dataTableOutput('targetSuccessTable') %>% withSpinner(), 
			footer=modalButton('Close')))
		}, invalid = function(i){
			print(i)
			actualize$targetSuccess <- data.frame(samples=c(), success=c())
		}, error = function(e){
			print(e)
			sendSweetAlert("Cannot launch targeting process", e$message)
			actualize$targetSuccess <- data.frame(samples=c(), success=c())
		})
	print('############################################################')
	print('######################### END TARGET #######################')
	print('############################################################')
})

integrate <- function(eic, omz, scalerange, baseline, sdnoise, missingScans = 1){
	scales <- seq(from = scalerange[1], to = scalerange[2], by = 2)
	wCoefs <- xcms:::MSW.cwt(eic$intensity, 
		scales = scales, wavelet = 'mexh')
	if(is.null(dim(wCoefs))) return(data.frame()) 
	wCoefs <- apply(wCoefs, 2, function(x) smooth.spline(x, spar = 0)$y)
	
	if(all(sapply(wCoefs, function(x) 
		all(x - baseline < sdnoise)))) return(data.frame())
	localMax <- xcms:::MSW.getLocalMaximumCWT(wCoefs)
	rL <- xcms:::MSW.getRidge(localMax) %>% keep(
		function(x) {
				w <- min(1:length(x),ncol(wCoefs))
			any(wCoefs[x,w]- baseline[x] >= sdnoise) & 
				length(unique(x)) > 1
	})
	if(length(rL) == 0) return(data.frame())
	
	peaks <- data.frame()
	irange <- ceiling(scales[1]/2)
	for(opp in rL){	
		inti <- sapply(opp, function(x){
			left <- ifelse(x - irange > 1, x - irange, 1)
			right <- ifelse(x + irange < nrow(eic), x + irange, nrow(eic))
			sum(eic[left:right, 'intensity'])
		})
		maxpi <- which(inti == max(inti))
		if (length(maxpi) > 1){
			m <- wCoefs[opp[maxpi], maxpi]
			bestcol <- which(m == max(m), arr.ind = TRUE)[2]
			best.scale <- maxpi[bestcol]
		} else best.scale <- maxpi
		best.scale.pos <- opp[best.scale]
		
		lwpos <- max(1, best.scale.pos - best.scale)
		rwpos <- min(best.scale.pos + best.scale, nrow(eic))
		# smooth because it can be jagged
		lm <- descendMin(wCoefs[, best.scale], best.scale.pos, missingScans)
		if(length(lm) < 2) next
		else if(lm[2] - lm[1] < scalerange[1]) next
		peaks <- peaks %>% rbind(integrate2(eic, omz, baseline, sdnoise, lm, missingScans, best.scale.pos))
	}
	peaks %>% distinct
}


integrate2 <- function(eic, omz, baseline, sdnoise, lm, missingScans, center = NULL){
	if(is.null(center)) center <- sum(lm) / 2
	lm <- narrow_rt_boundaries_extend(lm, center, eic$intensity - baseline, missingScans)
	lm <- narrow_rt_boundaries_reduce(lm, center, eic$intensity - baseline, missingScans)
	if(length(lm) < 2) return(data.frame())
	else if(diff(lm) <= 0) return(data.frame())
	
	mz.value <- omz %>% filter(scan >= eic[lm[1] , 'scan'] 
		& scan <= eic[lm[2], 'scan'] & 
		intensity > 0) %>% pull(mz)
	mz.int <- omz %>% filter(scan >= eic[lm[1] , 'scan'] 
		& scan <= eic[lm[2], 'scan'] & 
		intensity > 0) %>% pull(intensity)
	maxo <- max(mz.int)
	maxo.pos <- which.max(mz.int)
	if(length(mz.value) == 0) return(data.frame())
	mzrange <- range(mz.value)
	mz <- do.call(xcms:::mzCenter.wMean, list(mz = mz.value,
		intensity = mz.int))
	sn <- trapz(eic[lm[1]:lm[2], 'intensity'] - baseline[lm[1]:lm[2]]) / 
		trapz(rep(sdnoise, diff(lm) + 1))
	if(is.na(sn)) return(data.frame())
	else if(sn < 1) return(data.frame())
	data.frame(
		mz = mz, mzmin = mzrange[1], mzmax = mzrange[2], 
		rt = eic[center, 'rt'], 
		rtmin = eic[lm[1], 'rt'], 
		rtmax = eic[lm[2], 'rt'], 
		into = 	trapz(eic[lm[1]:lm[2], 'intensity']),
		maxo = maxo, scale = 0,
		scpos = eic[center, 'scan'], 
		scmin = eic[lm[1], 'scan'], 
		scmax = eic[lm[2], 'scan'], 
		lmin = eic[lm[1], 'scan'], 
		lmax = eic[lm[2], 'scan'], 
		sn = sn)
}
descendMin <- function(int, center, minPts = 1){
	left <- if(center != 1){
		lefts <- split((center - 1):1, 
			cumsum(diff(int[center:1]) < 0))
		limit <- which(lengths(lefts) > minPts)[1]
		if(is.na(limit)) 1 else lefts[[limit]][1]
	} else center
	
	right <- if(center != length(int)){
		rights <- split((center + 1):length(int), 
			cumsum(diff(int[center:length(int)]) < 0))
		limit <- which(lengths(rights) > minPts)[1]
		if(is.na(limit)) length(int) else rights[[limit]][1]
	} else center
	
	c(left, right)
}
narrow_rt_boundaries_extend <- function(lm, center, int, minPts = 1){
	left <- if(lm[1] != 1){
		lefts <- (lm[1]:1)[which(int[lm[1]:1] <= 0)]
		limits <- split(lefts, cumsum(c(TRUE, diff(lefts) > 1)))
		limit <- which(lengths(limits) > minPts)[1]
		if(is.na(limit)) 1 else limits[[limit]][1]
	} else lm[1]
	
	right <- if(lm[2] != length(int)){
		rights <- (lm[2]:length(int))[which(int[lm[2]:length(int)] <= 0)]
		limits <- split(rights, cumsum(c(TRUE, diff(rights) > 1)))
		limit <- which(lengths(limits) > minPts)[1]
		if(is.na(limit)) length(int) else limits[[limit]][1]
	} else lm[2]
	
	c(left, right)
}
narrow_rt_boundaries_reduce <- function(lm, center, int, minPts = 1){
	left <- if(lm[1] != center){
		lefts <- (lm[1]:center)[which(int[lm[1]:center] > 0)]
		limits <- split(lefts, cumsum(c(TRUE, diff(lefts) > minPts)))
		limit <- which(lengths(limits) > minPts)[1]
		if(is.na(limit)) lm[1] else limits[[limit]][1] - 1
	} else lm[1]
	
	right <- if(lm[2] != center){
		rights <- (lm[2]:center)[which(int[lm[2]:center] > 0)]
		limits <- split(rights, cumsum(c(TRUE, diff(rights) > minPts)))
		limit <- which(lengths(limits) > minPts)[1]
		if(is.na(limit)) lm[2] else limits[[limit]][1] + 1
	} else lm[2]
	
	c(if(left < 1) 1 else left, 
		if(right > length(int)) length(int) else right)
}

overlap <- function(peaks, eic){
	if(nrow(peaks) < 2) return(peaks)
	peaks <- peaks %>% arrange(desc(into))
	peaks2 <- peaks[1, ]
	peaks <- peaks[-1, ]
	while(nrow(peaks) > 0){
		# search any peaks in peaks2 that overlap the first peaks in peaksMat
		ids <- which(sapply(1:nrow(peaks2), function(i)
			# peaks2[i, 'mzmax'] >= peaks[1, 'mzmin'] & 
			# peaks2[i, 'mzmin'] <= peaks[1, 'mzmax'] & 
			peaks2[i, 'rtmax'] >= peaks[1, 'rtmin'] & 
			peaks2[i, 'rtmin'] <= peaks[1, 'rtmax']))
		if(length(ids) == 0) peaks2 <- peaks2 %>% rbind(peaks[1, ])
		else peaks2[ids[1], ] <- fusionPeaks(
			rbind(peaks2[ids[1], ], peaks[1, ]), eic)
		peaks <- peaks[-1, ]
	}
	peaks2
}

# a little false for the "into" calculation
fusionPeaks <- function(peaks, eic){
	data.frame(
		mz = peaks[1, 'mz'],
		mzmin = min(peaks$mzmin), 
		mzmax = max(peaks$mzmax),
		rt = peaks[1, 'rt'], 
		rtmin = min(peaks$rtmin),
		rtmax = max(peaks$rtmax),
		into = pracma::trapz(eic[min(peaks$lmin):max(peaks$lmax), 'intensity']),
		maxo = peaks[1, 'maxo'],
		scale = peaks[1, 'scale'],
		scpos = peaks[1, 'scpos'],
		scmin = min(peaks$scmin),
		scmax = max(peaks$scmax),
		lmin = min(peaks$lmin), 
		lmax = max(peaks$lmax),
		sn = peaks[1, 'sn']
	)
}

targetChloroPara <- function(xr, scalerange, mzRange, scRange, 
		printPlot = FALSE, missingScans = 1){
	eic <- rawEIC(xr, mzrange = mzRange) %>% as.data.frame %>% 
		cbind(rt = xr@scantime)
	mzMat <- rawMat(xr, mzrange = mzRange) %>% data.frame %>% 
		left_join(data.frame(scan = 1:length(xr@scantime), 
			time = xr@scantime), by = 'time') %>% filter(!is.na(scan))
	if(all(eic$intensity == 0)) return(data.frame())
	
	baseline <- runmed(eic$intensity, length(xr@scantime) / 3, 
			endrule="constant", algorithm="Turlach")
	noise <- eic %>% pull(intensity) %>% sd
	
	rois <- which(eic$intensity - baseline > noise)
	if(length(rois) == 0) return(data.frame())
	else if(length(rois) <= scalerange[1]) return(data.frame())
	
	if(printPlot) print(plot_ly(type="scatter", mode = "lines", data = eic, 
		x = ~scan, y = ~intensity) %>% add_lines(y = baseline) %>% 
		add_lines(y = noise))
			
	
	rois <- split(rois, cumsum(c(TRUE, diff(rois) > missingScans))) %>% 
		map(function(x) eic[x, ]) %>% 
		keep(function(x) x %>% filter(between(scan, scRange[1], scRange[2]) & 
			intensity - baseline[scan] >= noise) %>% 
				nrow >= ceiling(scalerange[1])) %>% 
		map(function(x) c(
			max(1, min(x$scan) - ((max(x$scan) - min(x$scan)) * 2)),
			min(nrow(eic), max(x$scan) + ((max(x$scan) - min(x$scan)) * 2)))) %>% 
		overlapROI
	if(length(rois) == 0) return(data.frame())
	do.call(rbind, 
		lapply(rois, function(roi)
			integrate(eic %>% filter(between(scan, roi[1], roi[2])), 
				mzMat %>% filter(between(scan, roi[1], roi[2])), 
				scalerange, 
				baseline[roi[1]:roi[2]], 
				noise, missingScans))) %>% 
					overlap(eic)
}

forceTargetChloroPara <- function(xr, mzRange, scRange, missingScans = 1){
	eic <- rawEIC(xr, mzrange = mzRange) %>% 
		as.data.frame %>% cbind(rt = xr@scantime)
	mzMat <- rawMat(xr, mzrange = mzRange, scanrange = scRange) %>% 
		data.frame %>% 
		left_join(data.frame(scan = 1:length(xr@scantime), 
			time = xr@scantime), by = 'time') %>% filter(!is.na(scan))
	if(all(eic$intensity == 0)) return(data.frame())

	baseline <- runmed(eic$intensity, length(xr@scantime) / 3, 
			endrule="constant", algorithm="Turlach")
	noise <- eic %>% pull(intensity) %>% sd
	
	integrate2(eic %>% filter(between(scan, scRange[1], scRange[2])), mzMat, 
		baseline[scRange[1]:scRange[2]], noise, c(1, diff(scRange)), missingScans)
}

overlapROI <- function(rois){
	if(length(rois) < 2) return(rois)
	rois1 <- rois[1]
	rois2 <- rois[-1]
	while(length(rois2) > 0){
		id <- which(sapply(rois1, function(x) 
			x[1] <= rois2[[1]][2] & 
			x[2] >= rois2[[1]][1]))
		if(length(id) == 0) rois1 <- rois1 %>% append(rois2[1])
		else {
			rois1[[id]][1] <- min(rois1[[id]][1], rois2[[1]][1])
			rois1[[id]][2] <- max(rois1[[id]][2], rois2[[1]][2])
		}
		rois2 <- rois2[-1]
	}
	rois1
}

recordTarget <- function(features, clusters){
	project_samples_adducts <- clusters %>% group_by(project_sample, adduct, machine) %>% 
		summarise()
	query <- sprintf('insert into project_sample_adduct (project_sample, adduct, machine) 
		values %s;', paste('(', project_samples_adducts$project_sample, ', \"', 
		project_samples_adducts$adduct, '\", \"', project_samples_adducts$machine, 
		'\")', sep='', collapse=', '))
	print(query)
	dbExecute(db, query)
	
	query <- sprintf('select project_sample_adduct, project_sample, adduct 
		from project_sample_adduct where project_sample in (%s) and adduct in (%s);', 
		paste(project_samples_adducts$project_sample, collapse=', ', sep=''), 
		paste('\"', project_samples_adducts$adduct, '\"', sep='', collapse=', '))
	print(query)
	project_samples_adducts <- dbGetQuery(db, query)
	clusters <- clusters %>% left_join(project_samples_adducts, 
		by = c('project_sample', 'adduct'))
	
	query <- sprintf('insert into cluster (formula, ion_formula, charge, C, Cl, score, 
			rtMean, deviation, CCI, 
			ppm, peakwidth1, peakwidth2, machine, project_sample_adduct) values %s;', 
		paste(sprintf("(\"%s\", \"%s\", %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, \"%s\", %s)", 
			clusters$formula, clusters$ion_formula, clusters$charge, clusters$C, clusters$Cl, 
			clusters$score, 
			clusters$rtMean, clusters$deviation, clusters$cci, clusters$ppm, clusters$peakwidth1, 
			clusters$peakwidth2, clusters$machine, clusters$project_sample_adduct), 
				collapse=', '))
	print(query)
	dbExecute(db, query)
	
	clusterIDs <- dbGetQuery(db, sprintf('select cluster from cluster where 
		project_sample_adduct in (%s);', paste(clusters$project_sample_adduct %>% unique, 
			collapse = ', ')))$cluster
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
	print(query)
	dbExecute(db, query)
}


output$targetSuccessTable <- DT::renderDataTable({
	tryCatch({
		data <- actualize$targetSuccess
		if(nrow(data) == 0) return(data)
		data$success <- sapply(data$success, function(x) 
			if(x == 0) "<div style=\"background-color: #FDCDAC;\">no chloroparaffin detected<div/>" 
			else paste("<div style=\"background-color: #B3E2CD;\">", x, "</div>"))
		data
	}, error = function(e){
		print(paste(e))
		data.frame(samples=c(), success=c())
	})
}, escape=FALSE, rownames=FALSE, selection='none', class='compact nowrap', 
options=list(dom='frtip', bFilter=FALSE, ordering=FALSE))
