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
		print('ERR targetSamples')
		print(paste(e))
		sendSweetAlert(paste(e$message))
		data.frame(project_sample=c(), sampleID=c())
	})
	pickerInput('targetSamples', 'samples', choices=setNames(
		choices$project_sample, choices$sampleID), 
		multiple=TRUE, options=list(`actions-box`=TRUE, `live-search`=TRUE))
})

output$uiTargetSampleTic <- renderUI({
	choices <- tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project is not yet initialized')
		else if(input$project == '') custom_stop('invalid', 'no project')
		else project_samples() %>% filter(project == input$project) %>% 
			select(sampleID, project_sample)
	}, invalid = function(i){
		print(paste(i))
		data.frame(project_sample=c(), sampleID=c())
	}, error = function(e){
		print('ERR targetSampleTIC')
		print(paste(e))
		sendSweetAlert(paste(e$message))
		data.frame(project_sample=c(), sampleID=c())
	})	
	pickerInput('targetSampleTic', 'sample', choices=setNames(choices$project_sample, choices$sampleID), 
		multiple=FALSE, width='50%')
})

output$targetTic <- renderPlotly({
	tryCatch({
	
	if(is.null(input$targetSampleTic)) custom_stop("invalid", 'targetSampleTic is null')
 	else if(input$targetSampleTic == '') custom_stop('invalid', 'targetSampleTic is empty')
	else print(sprintf('targetSampleTic: %s', input$targetSampleTic))
	
	#to force the re-actualization after peak picking
	actualize$project_samples
	plotChromato(db, input$targetSampleTic)
	}, invalid = function(i){
		print(i)
		plotEmptyChromato()
	}, error = function(e){
		print('ERR targetTIC')
		print(e)
		toastr_error('Draw TIC', paste(e$message))
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
		conditions <- c(is.null(input$targetSamples), is.null(input$targetAdducts))
		messages <- c('You need to select at least one sample',	'You need to select at least one adduct')
		test <- inputsTest(inputs, conditions, messages)
		if(!test) custom_stop('invalid', 'invalid args')
		inputs <- c('targetSamples', 'targetAdducts')
		conditions <- c(length(input$targetSamples) == 0, input$targetAdducts == "")
		messages <- c('You need to select at least one sample',	'You need to select an adduct')
		test <- inputsTest(inputs, conditions, messages)
		if(!test) custom_stop('invalid', 'invalid args')
		
		progressSweetAlert(session, id='pb', title='Target', value=0, display_pct=TRUE)
		# record the project_sample & delete precedent records
		updateProgressBar(session, id="pb", title="delete precedent records", value=0)
		print('delete precedent records')
		deleteDatas(input$targetSamples)
		
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
						theoricIso = ceiling(mz),
						theoricIso = theoricIso - theoricIso[1],
						theoricIso = case_when(
							theoricIso < 0 ~ paste0("A", theoricIso),
							theoricIso > 0 ~ paste0("A+", theoricIso),
							TRUE ~ "A")) %>% 
						distinct(theoricIso, .keep_all = TRUE))

		pbValMax <- length(input$targetSamples) * nrow(theorics)
		pbVal <- 0
		
		success <- c()
		res <- list()
		resInfo <- list()
		for(pj in input$targetSamples){
			updateProgressBar(session, id='pb', title=sprintf("Targeting in %s", project_samples() %>% 
						filter(project_sample == pj) %>% pull(sampleID)),
						value=pbVal * 100 / pbValMax)
			
			xr <- tryCatch(loadRawFile(db, pj), 
				error = function(e){
				print("file not found")
				toastr_error(sprintf('File "%s" is not found in database', 
					project_samples() %>% filter(project_sample == pj) %>% pull(sampleID)), 
						paste(e$message))
				NULL
			})
			if(is.null(xr)){
				success <- c(success, 0)
				pbVal <- pbVal + nrow(theorics)
				next
			}
			
			nbChloroParaf <- 0
			scalerange <- round((input$targetPeakwidth / mean(diff(xr@scantime))) / 2)
			scRange <- round(input$targetRT * 60 / mean(diff(xr@scantime)))
			for(row in 1:nrow(theorics)){
				theoric <- theorics[row, ]
				theoricPattern <- theoricPatterns[[row]]
				
				peaks <- data.frame()
				for(row2 in 1:nrow(theoricPattern)){
					tmp <- suppressWarnings(targetChloroPara(xr, scalerange, 
						as.double(theoricPattern[row2, c('mzmin', 'mzmax')]), 
						scRange, minPts))
					if(nrow(tmp) > 0) peaks <- peaks %>% rbind(tmp %>% 
						mutate(iso = theoricPattern[row2, 'theoricIso']))
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
					nbChloroParaf <- nbChloroParaf + length(clusters)
					if(length(clusters) == 0) next
					res <- res %>% append(clusters)
					resInfo <- resInfo %>% rbind(
						do.call(rbind, lapply(clusters, function(cluster) 
							data.frame(
								score = calculateScore(cluster, theoricPattern, input$targetTolPpm, 100),
								rtMean = sum(cluster$rt * (cluster$into / sum(cluster$into))),
								deviation = calculateDeviation(cluster, theoricPattern)))) %>% 
							cbind(theoric[, c('formula', 'C', 'Cl', 'adduct')]) %>% 
							mutate(ppm = input$targetTolPpm, peakwidth1 = input$targetPeakwidth[1], 
								peakwidth2 = input$targetPeakwidth[2], machine = input$targetMachine, 
								project_sample = pj))
				}
				pbVal <- pbVal + 1
				updateProgressBar(session, id='pb', title=sprintf("Targeting in %s", project_samples() %>% 
						filter(project_sample == pj) %>% pull(sampleID)),
						value=pbVal * 100 / pbValMax)
			}
			rm(xr)
			gc()
			success <- c(success, nbChloroParaf)
		}
		print(success)
		if(length(res) > 1) recordTarget(res, resInfo)
		actualize$project_samples <- TRUE
		actualize$targetSuccess <- data.frame(samples=project_samples() %>% 
			filter(project_sample %in% input$targetSamples) %>% pull(sampleID), 
			success=success)
		closeSweetAlert(session)
		showModal(modalDialog(title="Result of targeting", easyClose=TRUE, 
			DT::dataTableOutput('targetSuccessTable'), footer=modalButton('Close')))
		}, invalid = function(i){
			print(i)
			actualize$targetSuccess <- data.frame(samples=c(), success=c())
		}, error = function(e){
			print(e)
			sendSweetAlert(e$message)
			actualize$targetSuccess <- data.frame(samples=c(), success=c())
		})
	print('############################################################')
	print('######################### END TARGET #######################')
	print('############################################################')
})

integrate <- function(xr, roi, scalerange, baseline, sdnoise, minPts){
	if(length(roi) == 0) return(data.frame())
	scantime <- xr@scantime
	scales <- seq(from = scalerange[1], to = scalerange[2], by = 2)
	
	sr <- c(max(1, roi$scmin - scalerange[2] * 1.5),
		min(length(scantime), roi$scmax + scalerange[2] * 1.5))
	eic <- rawEIC(xr, mzrange = as.double(roi[c('mzmin', 'mzmax')]), 
		scanrange = sr) %>% as.data.frame
	omz <- rawMat(xr, mzrange=as.double(roi[c('mzmin', 'mzmax')]), 
		scanrange = as.double(roi[c('scmin', 'scmax')])) %>% data.frame %>% 
		mutate(scan = sapply(time, function(x) which(scantime == x)))
	baseline <- baseline[sr[1]:sr[2]]
	
	wCoefs <- xcms:::MSW.cwt(eic$intensity, 
		scales = scales, wavelet = 'mexh') %>% 
			as.data.frame
	if(is.null(dim(wCoefs))) return(data.frame()) 
	else if(all(sapply(wCoefs, function(x) 
		all(x - baseline < sdnoise)))) return(data.frame())
	if(sr[2] == length(scantime)) wCoefs[nrow(wCoefs),] <- wCoefs[nrow(wCoefs) - 1, ] * 0.99
	localMax <- xcms:::MSW.getLocalMaximumCWT(wCoefs)
	rL <- xcms:::MSW.getRidge(localMax) %>% keep(
		function(x) {
				w <- min(1:length(x),ncol(wCoefs))
			any(wCoefs[x,w]- baseline[x] >= sdnoise) & 
				length(x) >= 1
	})
	if(length(rL) == 0) return(data.frame())
	
	peaks <- data.frame()
	for(opp in rL){
		inti <- sapply(1:length(opp), function(i)
			wCoefs[opp[i], i])
		maxpi <- which(inti == max(inti))
		if (length(maxpi) > 1){
			m <- wCoefs[opp[maxpi], maxpi]
			bestcol <- which(m == max(m), arr.ind = TRUE)[2]
			best.scale <- maxpi[bestcol]
		} else  best.scale <- maxpi
		best.scale.pos <- opp[best.scale]
		
		lwpos <- max(1, best.scale.pos - best.scale)
		rwpos <- min(best.scale.pos + best.scale, nrow(eic))
		lm <- descendMin(wCoefs[, best.scale], 
			best.scale.pos, minPts)
		if(length(lm) < 2) next
		else if(lm[2] - lm[1] < scalerange[1]) next
		lm <- narrow_rt_boundaries(lm, best.scale.pos, 
			eic$intensity - baseline, minPts)
		if(length(lm) < 2) next
		else if(lm[2] - lm[1] < scalerange[1]) next
				
		mz.value <- omz %>% filter(scan >= lwpos  + sr[1] - 1 
			& scan <= rwpos  + sr[1] - 1 & 
			intensity > 0) %>% pull(mz)
		mz.int <- omz %>% filter(scan >= lwpos + sr[1] - 1 
			& scan <= rwpos + sr[1] - 1 & 
			intensity > 0) %>% pull(intensity)
		maxo <- max(mz.int)
		maxo.pos <- which.max(mz.int)
		if(length(mz.value) == 0) next
		mzrange <- range(mz.value)
		mz <- do.call(xcms:::mzCenter.wMean, list(mz = mz.value,
			intensity = mz.int))
		sn <- trapz(eic[lm[1]:lm[2], 'intensity'] - baseline[lm[1]:lm[2]]) / 
			trapz(rep(sdnoise, diff(lm) + 1))
		if(sn < 1) next
		peaks <- peaks %>% rbind(data.frame(
			mz = mz, mzmin = mzrange[1], mzmax = mzrange[2], 
			rt = scantime[best.scale.pos + sr[1] - 1], 
			rtmin = scantime[lm[1] + sr[1] - 1],
			rtmax = scantime[lm[2] + sr[1] - 1],
			into = 	trapz(eic[lm[1]:lm[2], 'intensity']),
			maxo = maxo, scale = scales[best.scale],
			scpos = best.scale.pos + sr[1] - 1, 
			scmin = lwpos + sr[1] - 1,
			scmax = rwpos + sr[1] - 1,
			lmin = lm[1] + sr[1] - 1,
			lmax = lm[2] + sr[1] - 1, 
			sn = sn))
	}
	peaks
}

descendMin <- function(int, center, minPts){
	lefts <- split(center:1, 
		cumsum(c(TRUE, diff(int[center:1]) < 0)))
	left <- center - which(lengths(lefts) > minPts)[1]
	
	rights <- split(center:length(int), 
		cumsum(c(TRUE, diff(int[center:length(int)]) < 0)))
	right <- center + which(lengths(rights) > minPts)[1]
	if(is.na(left)) left <- center
	if(is.na(right)) right <- center
	c(left, right)
}
narrow_rt_boundaries <- function(lm, center, int, minPts){
	lefts <- split(lm[1]:center, 
		cumsum(c(TRUE, diff(int[lm[1]:center]) > 0)))
	left <- lefts[[which(lengths(lefts) < minPts)[1]]][1] - 1
	rights <- split(lm[2]:center, 
		cumsum(c(TRUE, diff(int[lm[2]:center]) > 0)))
	right <- rights[[which(lengths(rights) < minPts)[1]]][1] + 1
	if(length(left) == 0) left <- lm[1]
	if(length(right) == 0) right <- lm[2]
	c(left, right)
}
overlap <- function(peaks, eic){
	if(nrow(peaks) < 2) return(peaks)
	peaks <- peaks %>% arrange(desc(into))
	peaks2 <- peaks[1, ]
	peaks <- peaks[-1, ]
	while(nrow(peaks) > 0){
		# search any peaks in peaks2 that overlap the first peaks in peaksMat
		ids <- which(sapply(1:nrow(peaks2), function(i)
			peaks2[i, 'mzmax'] >= peaks[1, 'mzmin'] & 
			peaks2[i, 'mzmin'] <= peaks[1, 'mzmax'] & 
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

targetChloroPara <- function(xr, scalerange, mzRange, scRange, minPts = 3, printPlot = FALSE){
	eic <- rawEIC(xr, mzrange = mzRange) %>% data.frame
	mzMat <- rawMat(xr, mzrange = mzRange) %>% data.frame %>% 
		mutate(scan = sapply(time, function(x) which(xr@scantime == x)))
	if(all(eic$intensity == 0)) return(data.frame())
	
	baseline <- runmed(eic$intensity, scalerange[2]*3, 
			endrule="constant", algorithm="Turlach")
	noise <- eic %>% pull(intensity) %>% sd
			
	rois <- which(eic$intensity - baseline > noise)
	if(length(rois) == 0) return(data.frame())
	rois <- split(rois, cumsum(c(TRUE, diff(rois) > minPts))) %>% 
		map(function(x) mzMat %>% filter(scan %in% 
			eic[x, 'scan'])) %>% 
		keep(function(x) x %>% filter(between(scan, scRange[1], scRange[2]) & 
			intensity - baseline[scan] >= noise) %>% 
				nrow >= ceiling(scalerange[1] / 2)) %>% 
		map(function(x) list(
			mz = mean(x$mz),
			mzmin = min(x$mz),
			mzmax = max(x$mz), 
			scmin = min(x$scan), 
			scmax = max(x$scan)))
	peaks <- if(length(rois) == 0) data.frame() else do.call(rbind, 
		lapply(rois, function(roi) 
			integrate(xr, roi, scalerange, baseline, noise, minPts))) %>% 
				overlap(eic)
	if(printPlot){
		p <- plot_ly(type="scatter", mode="lines", data=eic, x=~scan, y=~intensity) %>% 
			add_lines(y=baseline) %>% add_lines(y=noise) %>% layout(title=paste(mzRange[1], '-', mzRange[2]))
		if(nrow(peaks) > 0) for(i in 1:nrow(peaks)) p <- p %>% 
			add_lines(x = rep(peaks[i, 'lmin'],2), y = c(0, max(eic$intensity)), 
				line=list(dash = "dash", width = 1), color=I('black')) %>% 
			add_lines(x = rep(peaks[i, 'lmax'],2), y = c(0, max(eic$intensity)), 
				line=list(dash = "dash", width = 1), color=I('black'))
		print(p)
	}
	peaks
}

recordTarget <- function(features, clusters){
	query <- sprintf('insert into cluster (formula, C, Cl, score, rtMean, deviation, 
			ppm, peakwidth1, peakwidth2, adduct, machine, project_sample) values %s;', 
		paste(sprintf("(\"%s\", %s, %s, %s, %s, %s, %s, %s, %s, \"%s\", \"%s\", %s)", 
			clusters$formula, clusters$C, clusters$Cl, clusters$score, 
			clusters$rtMean, clusters$deviation, clusters$ppm, clusters$peakwidth1, 
			clusters$peakwidth2, clusters$adduct, clusters$machine, clusters$project_sample), collapse=', '))
	dbExecute(db, query)
	
	clusterIDs <- dbGetQuery(db, sprintf('select cluster from cluster where 
		project_sample in (%s) and adduct in (%s);', clusters$project_sample %>% unique, 
		paste0("\"", unique(clusters$adduct), "\"", collapse=", ")))$cluster
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
