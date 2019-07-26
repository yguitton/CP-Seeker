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
	pickerInput('targetSamples', 'samples', choices=setNames(
		choices$project_sample, choices$sampleID), 
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
		query <- sprintf('select data from sample where sample == "%s";', 
			input$targetSampleTic)
		print(query)
		msFile <- dbGetQuery(db, query)$data[[1]] %>% unserialize
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
		adducts=input$targetAdducts, rtRange = input$targetRT, ppm=input$targetTolPpm, 
		peakwidth=input$targetPeakwidth, 
		machine=names(resolution_list)[input$targetMachine %>% as.numeric]))
	tryCatch({
		inputs <- c('targetProject', 'targetSamples', 'targetAdducts')
		conditions <- c(is.null(input$project), is.null(input$targetSamples),
			is.null(input$targetAdducts))
		messages <- c('You need to select a project', 'You need to select at least one sample',
			'You need to select at least one adduct')
		test <- inputsTest(inputs, conditions, messages)
		if(!test) custom_stop('invalid', 'invalid args')
		inputs <- c('targetProject', 'targetSamples', 'targetAdducts')
		conditions <- c(input$project == "", length(input$targetSamples) == 0,
			input$targetAdducts == "")
		messages <- c('You need to select a project', 'You need to select at least one sample',
			'You need to select an adduct')
		test <- inputsTest(inputs, conditions, messages)
		if(!test) custom_stop('invalid', 'invalid args')
		
		progressSweetAlert(session, id='pb', title='Target', value=0, display_pct=TRUE)
		pbVal <- 0
		# record the project_sample & delete precedent records
		updateProgressBar(session, id="pb", title="delete precedent records", value=0)
		print('delete precedent records')
		deleteDatas(input$targetSamples)
		
		success <- c()
		for(i in 1:length(input$targetAdducts)){
			# load mzs of each chloroparaffin
			print('load mzs')
			updateProgressBar(session, id="pb", title="compute cholorpara m/z", value=pbVal)
			theorics <- loadChloroParaMzs(input$targetAdducts[i], 
				input$targetMachine %>% as.numeric)
			theorics <- lapply(theorics, function(x) x %>% 
				dplyr::mutate(tolMDa = mz * input$targetTolPpm * 10**-6) %>%
				dplyr::mutate(mzmin = mz - tolMDa, mzmax = mz + tolMDa) %>% 
				select(-tolMDa))
			
			for(j in 1:length(input$targetSamples)){
				updateProgressBar(session, id='pb', title=paste(
					"Processing", project_samples() %>% filter(
						project_sample == input$targetSamples[j]) %>% 
						pull(sampleID), sep=' '), value=pbVal)
				pbI <- 100 / (length(input$targetAdducts) * 
					length(input$targetSamples) * length(theorics))
				
				param <- recordParamTarget(ppm = input$targetTolPpm, 
					pw = input$targetPeakwidth, adduct = input$targetAdducts[i], 
					rangeRT = input$targetRT, machine = input$targetMachine %>% 
						as.numeric, project_sample = input$targetSamples[j])
					
				query <- sprintf('select data from sample where sample == (
					select sample from project_sample where project_sample == %s);', 
					param$project_sample)
				msFile <- dbGetQuery(db, query)$data[[1]] %>% unserialize
				data <- list()
				for(k in 1:length(theorics)){
					data <- data %>% append(list(targetChloroPara(
						msFile, theorics[[k]], param)))
					pbVal <- pbVal + pbI
					updateProgressBar(session, id='pb', title=paste(
					"Processing", project_samples() %>% filter(
						project_sample == input$targetSamples[j]) %>% 
						pull(sampleID), sep=' '), value=pbVal)
				}
				data <- unlist(data, recursive=FALSE) %>% 
					keep(function(x) length(x) > 0)
				if(length(data) > 0){
					success[i] <- sprintf("detect %s chloroparaffins", 
						length(data))
					recordDataTarget(data, param)
				} else {
					success[i] <- "no chloroparaffin detected"
					message <- sprintf('no chloroparaffin detected in %s', project_samples() %>% 
						filter(project_sample == input$targetSamples[i]) %>% pull(sampleID))
					print(message)
					toastr_warning(message)
				}
				pbVal <- pbVal + 1
				rm(msFile)
				gc()
			}
		}
		print(success)
		actualize$project_samples <- TRUE
		actualize$params <- TRUE
		actualize$targetSuccess <- data.frame(samples=project_samples() %>% 
			filter(project_sample %in% input$targetSamples) %>% pull(sampleID), success=success)
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
	gc()
	print('------------------- END TARGET --------------------')
})

recordParamTarget <- function(ppm, pw, adduct, rangeRT, machine, project_sample){
	print('record param')
	query <- sprintf('insert into param (ppm, pw1, pw2, adduct, 
		rangeRt1, rangeRt2, machine, date, project_sample) values (%s, %s, %s, 
		"%s", %s, %s, %s, "%s", %s);', ppm, pw[1], pw[2], adduct, 
		rangeRT[1], rangeRT[2], machine, Sys.Date() %>% as.character, project_sample)
	print(query)
	dbSendQuery(db, query)
	paramID <- dbGetQuery(db, 'select last_insert_rowid();')$last_insert_rowid[1]
	list(param = paramID, ppm = ppm, pw = pw, adduct = adduct, 
		rangeRT = rangeRT, machine = machine, project_sample=project_sample)
}

recordDataTarget <- function(data, param){
	clusters <- reduce(data, function(a, b) a %>% rbind(
		b %>% group_by(cluster) %>% dplyr::summarise(
			formula = formula[1], 
			C = str_extract(formula[1], "C[[:digit:]]+") %>% 
				str_extract('[[:digit:]]+') %>% as.numeric,
			Cl = str_extract(formula[1], "Cl[[:digit:]]+") %>% 
				str_extract('[[:digit:]]+') %>% as.numeric,
			score = score[1], rtMean = rtMean[1], project_sample = 
				param$project_sample, param = param$param)), 
		.init = data.frame())
	query <- sprintf('insert into cluster (formula, C, Cl, 
		score, rtMean, param, project_sample) values %s;', paste(apply(
			clusters, 1, function(x) paste0('("', 
				x['formula'], '", ', x['C'], ', ', x['Cl'], 
				', ', x['score'], ', ', x['rtMean'], ', ', 
				x['param'], ', ', x['project_sample'], ')')), 
			collapse=', '))
	print(query)
	dbSendQuery(db, query)
	query <- sprintf('select cluster from cluster where 
		project_sample == %s and param == %s;', param$project_sample, 
			param$param)
	clusterIDs <- dbGetQuery(db, query)$cluster
	features <- reduce(1:length(data), function(a, b) a %>% rbind(
		data[[b]] %>% mutate(cluster = clusterIDs[b])), 
		.init=data.frame())
	query <- sprintf('insert into feature (mz, mzmin, mzmax, 
		maxo, "into", abundance, rt, rtmin, rtmax, lm, lmin, lmax, 
		iso, sn, scale, cluster, pw, dppm, dmDa) values %s;', paste(apply(
			features, 1, function(x) paste0('(', x['mz'], ', ', 
			x['mzmin'], ', ', x['mzmax'], ', ', x['maxo'], ', ', 
			x['into'], ', ', x['abundance'], ', ', x['rt'], ', ', 
			x['rtmin'], ', ', x['rtmax'], ', ', x['lm'], ', ', 
			x['lmin'], ', ', x['lmax'], ', "', x['iso'], '", ', 
			x['sn'], ', ', x['scale'], ', ', x['cluster'], ', ', 
			x['pw'], ', ', x['dppm'], ', ', x['dmDa'], ')')), 
		collapse=', '))
	print(query)
	dbSendQuery(db, query)
}

# load for each chloroparaffine with adduct two mz
# the formula for a chloroparaffine is C(n)H(2n+2-x)Cl(x)
loadChloroParaMzs <- function(adduct, machine=NULL, minC=4, maxC=36, 
		minCl=2, maxCl=30){
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
	data <- isopattern(isotopes, brute_formulas, charge=adduct$Charge, 
		verbose=FALSE, threshold=.1)
	if(!is.null(machine)){
		checked <- check_chemform(isotopes, brute_formulas)
		resolution <- getR(checked, plotit=FALSE, 
			resmass=resolution_list[[machine]])
		data <- envelope(data, resolution=resolution, verbose=FALSE)
		data <- vdetect(data, detect='centroid', plotit=FALSE, verbose=FALSE)
	}
	lapply(1:length(data), function(i) data[[i]] %>% data.frame %>% 
		arrange(desc(abundance)) %>% dplyr::mutate(mz = round(`m.z`, digits=5), 
		theoricIso = ceiling(mz), theoricIso = theoricIso - theoricIso[1], 
		theoricIso = case_when(theoricIso < 0 ~ paste0('A', theoricIso), 
			theoricIso > 0 ~ paste0('A+', theoricIso), TRUE ~ 'A')) %>% 
			distinct(theoricIso, .keep_all=TRUE) %>% 
		select(mz, abundance, theoricIso) %>% cbind(formula = formulas[i]))
}
		
targetChloroPara <- function(msFile, theoric, param){
	eics <- list(getEIC(msFile, theoric[1, ], param))
	
	# first get ROIs
	rois <- findROIs(msFile, eics[[1]], param)
	if(length(rois) == 0) return(list())
	
	#get all basepeaks
	basepeaks <- reduce(rois, function(a, b) a %>% rbind(
		targetROI(msFile, eics[[1]], theoric[1, ], param, 
			scmin = b$scmin, scmax = b$scmax)), 
		.init=data.frame())
	# if(grepl('C10H[[:digit:]]+Cl5', theoric[1, 'formula'])) browser()
	if(nrow(basepeaks) == 0) return(list())
	else basepeaks <- overlap(basepeaks, max(basepeaks$dmDa))
	
	res <- list()
	l <- 0
	for(i in 1:nrow(basepeaks)){
		basepeak <- basepeaks[i, ]
		cluster <- basepeak %>% 
			mutate(abundance = 100)
		j <- 1
		continue <- TRUE
		while(continue & j <= nrow(theoric)){
			j <- j + 1
			if(length(eics) < j) eics <- eics %>% append(list(
				getEIC(msFile, theoric[j, ], param)))
			tmp <- targetROI(msFile, eics[[j]], theoric[j, ], param, 
				scmin = basepeak$lwpos, scmax = basepeak$rwpos)
			if(nrow(tmp) > 0) {
				tmp <- overlap(tmp, max(tmp$dmDa)) %>% mutate(
						diffRt = abs(rt - basepeak$rt),
						abundance = into * 100 / basepeak$into, 
						diffAbd = abs(abundance - theoric[j, 'abundance'])) %>% 
					filter(rt >= basepeak$rtmin & 
						rt <= basepeak$rtmax & diffAbd <= 100) %>% 
					top_n(1, desc(diffRt)) %>% slice(1) %>% 
					select(-diffRt, -diffAbd)
				if(nrow(tmp) > 0) cluster <- cluster %>% rbind(tmp)
				else continue <- FALSE
			} else continue <- FALSE
		}
		score <- calculateScore(cluster, theoric, 
			max(cluster$dmDa), 100)
		# if(grepl("C10H[[:digit:]]+Cl5", theoric[1, 'formula'])) browser()
		if(score > 0){
			l <- l + 1
			res <- res %>% append(list(
				cluster %>% mutate(
					cluster = l, 
					score = score, 
					formula = theoric[1, 'formula'],
					rtMean = mean(rt)
				)
			))
		}
	}
	res
}

getEIC <- function(msFile, theoric, param){
	rtScan <- mean(diff(msFile@scantime))
	rtrange <- param$rangeRT * 60
	eic <- rawEIC(msFile, mzrange=theoric[, c('mzmin', 'mzmax')] %>% 
				as.double(), rtrange = rtrange) %>% arrangeEic(msFile)
	
	windowSc <- round((param$pw[2] / rtScan) / 2)
	baseline <- suppressWarnings(
		runmed(eic$y, windowSc, endrule="constant", 
			algorithm="Turlach"))
	
	eic %>% mutate(y = y - baseline) %>% 
		mutate(y = dplyr::case_when(y < 0 ~ 0, TRUE ~ y))
}

findROIs <- function(msFile, eic, param){
	if(all(eic$y == 0)) return(list())
	
	rtScan <- mean(diff(msFile@scantime))
	scalerange <- round((param$pw / rtScan) / 4)
	
	noise <- if(all(eic$y == 0)) 0 else eic %>% pull(y) %>% xcms:::trimm(c(.05, .95)) %>% mean
	plot_ly(type="scatter", mode="lines", data=eic, x=~x, y=~y) %>% 
		add_lines(x=eic$x, y=noise)
	
	rois <- which(eic$y > noise)
	if(length(rois) == 0) return(list())
	split(rois, cumsum(c(TRUE, diff(rois) > scalerange[1]))) %>% 
		keep(function(roi) between(length(roi), scalerange[1], scalerange[2])) %>% 
		purrr::map(function(roi) list(scmin = eic[min(roi), 'x'], 
			scmax = eic[max(roi), 'x']))
}


targetROI <- function(msFile, eic, theoric, param, scmin, scmax){
	if(all(eic$y == 0)) return(data.frame())
	
	rtScan <- mean(diff(msFile@scantime))
	scalerange <- round((param$pw / rtScan) / 2)
	minPts <- scalerange[1] / 2
	scrange <- c(scmin, scmax)
	scrange[1] <- if(scrange[1] - scalerange[2] < 1) 1 else scrange[1] - scalerange[2]
	scrange[2] <- if(scrange[2] + scalerange[2] > nrow(eic)) nrow(eic) else scrange[2] + scalerange[2]
	
	eic <- eic %>% filter(x >= scrange[1] & x <= scrange[2])
	noise <- eic %>% filter(!between(x, scmin, scmax) & y > 0) %>% pull(y)
	noise <- if(all(noise == 0)) 0 else mean(noise)
	p <- plot_ly(type="scatter", mode="lines", data=eic, x=~x, y=~y) %>% add_lines(x=eic$x, y = noise)
		
	if(all(eic$y == 0)) return(data.frame())
	# if(grepl('C10H[[:digit:]]+Cl5', theoric$formula)) browser()
	# if(grepl("C25H[[:digit:]]+Cl19", theoric$formula) & theoric$theoricIso == "A+1") browser()
	
	scales <- seq(from = scalerange[1], to = scalerange[2], by = 2)
	wCoefs <- xcms:::MSW.cwt(eic$y, scales = scales, wavelet = 'mexh') %>% 
		as.data.frame
	if(is.na(wCoefs[1, 1])) return(data.frame())
	localMax <- do.call(cbind, lapply(wCoefs, function(x)
		localMaxFunc(x, minPts, noise)))
	rL <- which(localMax == 1, arr.ind=TRUE)[, 1] %>% unique %>% sort
	if(length(rL) == 0) return(data.frame())
	rL <- purrr::keep(rL, function(x) 
		between(eic[x, 'x'], scmin, scmax))
	if(length(rL) == 0) return(data.frame())
	rL <- split(rL, cumsum(c(TRUE, diff(rL) > minPts))) %>% 
		keep(function(roi) any(eic[roi, 'y'] > noise))
	
	# browser()
	# if(grepl("C15H[[:digit:]]+Cl5", theoric$formula) & theoric$theoricIso == "A+1") browser()
	# if(grepl("C27H[[:digit:]]+Cl8", theoric$formula)) browser()
	
	peaks <- data.frame()
	for(opp in rL){
		opp <- unique(opp)
		waves <- which(localMax[opp, ] == 1, arr.ind=TRUE) %>% 
			matrix(ncol=2) %>% data.frame
		if(nrow(waves) == 1) waves[1, 1] <- 1
		
		counts <- c()
		for(row in 1:nrow(waves)){
			lm <- descendMin2(wCoefs[, waves[row, 2]], opp[waves[row, 1]], minPts)
			gap <- all(eic[lm[1]:lm[2], 'y'] == 0) ## looks like we got stuck in a gap right in the middle of the peak
			if((lm[1] == lm[2]) || gap ) counts <- c(counts, FALSE)
			else {
				lm <- narrow_rt_boundaries(lm, eic$y, minPts)
				if(lm[1] == 0) counts <- c(counts, FALSE)
				counts <- c(counts, which(
					eic[lm[1]:lm[2], 'y'] > noise) %>% 
						length >= scales[waves[row, 2]])
			}
		}
		if(all(!counts)) next
		bestRows <- waves[which(counts), ]
		bestRow <- which.max(bestRows[, 2])
		bestScan <- opp[waves[bestRow, 1]]
		bestWave <- waves[bestRow, 2]
	
		# bestScan <- unique(opp)[which(table(opp) == max(table(opp)))]
		# if(length(bestScan) > 1){
			# int <- sapply(bestScan, function(x) 
				# max(wCoefs[x, which(localMax[x, ]==1)]))
			# bestScan <- bestScan[which(int == max(int))]
			# if(length(bestScan) > 1) bestScan <- bestScan[which.max(
				# eic[bestScan, 'y'])]
		# }
		# bestWave <- which(localMax[bestScan, ] == 1)
		# if(length(bestWave) > 1){
			# counts <- c()
			# for(wave in bestWave){
				# lm <- descendMin2(wCoefs[, wave], bestScan, minPts)
				# gap <- all(eic[lm[1]:lm[2], 'y'] == 0) ## looks like we got stuck in a gap right in the middle of the peak
				# if((lm[1] == lm[2]) || gap ) counts <- c(counts, FALSE)
				# else {
					# lm <- narrow_rt_boundaries(lm, eic$y, minPts)
					# if(lm[1] == 0) counts <- c(counts, FALSE)
					# counts <- c(counts, which(
						# eic[lm[1]:lm[2], 'y'] > noise) %>% 
							# length >= scales[wave])
				# }
			# }
			# bestWave <- bestWave[counts]
			# if(length(bestWave) > 1) bestWave <- bestWave[length(bestWave)]
		# }
		# if(length(bestWave) == 0) next
		
		lm <- descendMin2(wCoefs[, bestWave], bestScan, minPts)
		gap <- all(eic[lm[1]:lm[2], 'y'] == 0) ## looks like we got stuck in a gap right in the middle of the peak
		if((lm[1] == lm[2]) || gap ) next
		lm <- narrow_rt_boundaries(lm, eic$y, minPts)
		if(lm[1] == 0) next
			
		# if lm too high or too less
		if(!between(lm[2] - lm[1], scalerange[1]*2, scalerange[2]*2)) next
		# if not enough points above noise
		if(which(eic[lm[1]:lm[2], 'y'] > noise) %>% 
			length < scales[bestWave]) next
			
		lwpos <- bestScan - scales[bestWave]
		if(lwpos < 1) lwpos <- 1
		rwpos <- bestScan + scales[bestWave]
		if(rwpos > nrow(eic)) rwpos <- nrow(eic)
		
		into <- trapz(eic[lm[1]:lm[2], 'x'], eic[lm[1]:lm[2], 'y'])
		intn <- trapz(eic[lm[1]:lm[2], 'x'], rep(noise, diff(lm)+1))
		sn <- if(intn > 0) into / intn else into
		if(sn < 1) next
			
		submsFile <- suppressWarnings(msFile[eic[lwpos:rwpos, 'x']])
		mzDensity <- data.frame(mz = submsFile@env$mz, 
			intensity = submsFile@env$intensity) %>% 
			filter(mz >= theoric$mzmin & mz <= theoric$mzmax & 
				intensity > 0)
		mz <- do.call(xcms:::mzCenter.wMean, list(
			mz = mzDensity$mz, intensity = mzDensity$intensity))
		mzrange <- range(mzDensity$mz)
		maxo <- max(mzDensity$intensity)
		dppm <- if(nrow(mzDensity) >= (scalerange[1] + 1)) min(
			xcms:::running(abs(diff(mzDensity$mz)) / (mzrange[2] *  1e-6), 
				fun = max, width = scalerange[1])) else (mzrange[2] - 
					mzrange[1]) / (mzrange[2] * 1e-6)
		rtrange <- range(msFile@scantime[eic[lm[1]:lm[2], 'x']]) / 60
		pw <- scales[bestWave] * mean(diff(msFile@scantime)) * 2
		rt <- msFile@scantime[eic[bestScan, 'x']] / 60
		
		peaks <- peaks %>% rbind(data.frame(
			mz = mz, mzmin = mzrange[1], mzmax = mzrange[2], 
			rt = rt, rtmin = rtrange[1], rtmax = rtrange[2], 
			into = into, maxo = maxo, lm = eic[bestScan, 'x'], 
			lmin = eic[lm[1], 'x'], lmax = eic[lm[2], 'x'], 
			lwpos = eic[lwpos, 'x'], rwpos = eic[rwpos, 'x'], 
			scale = scales[bestWave], pw = pw, iso = theoric$theoricIso, 
			dppm = dppm, dmDa = abs(mz - theoric$mz), sn = sn))
	}
	peaks
}

# re-adapt from xcms
descendMin <- function(d, startpos){
    l <- if(startpos > 1) startpos - 1 else startpos
    r <- if(startpos < length(d)) startpos + 1 else startpos 
    N <- length(d)
    while ((l > 1) && d[l] != 0) l <- l - 1
    while ((r < N) && d[r] != 0) r <- r + 1
    c(l-1, r+1)
}

# re-adapt from xcms
descendMin2 <- function(d, startpos, maxDescOutlier){
    l <- if(startpos > 1) startpos - 1 else startpos
    r <- if(startpos < length(d)) startpos + 1 else startpos 
    outl <- 0
    N <- length(d)
    while ((l > 1) && outl <= maxDescOutlier) {
        if (outl > 0) vpos <- opos
        else vpos <- l
        if (d[l - 1] > d[vpos]) outl <- outl + 1
        else outl <- 0
        if (outl == 1) opos <- l
        l <- l - 1
    }
    if (outl > 0) l <- l + outl
    outl <- 0
    while ((r < N) && outl <= maxDescOutlier) {
        if (outl > 0) vpos <- opos
        else vpos <- r
        if (d[r + 1] > d[vpos]) outl <- outl + 1
        else outl <- 0
        if (outl == 1) opos <- r
        r <- r + 1
    }
    if (outl > 0) r <- r - outl
    c(l, r)
}

# local Max
localMaxFunc <- function(pts, minPts, noise){
	pts2 <- c()
	out <- 0
	pos <- "desc"
	j <- 1
	for(i in 2:length(pts)){
		if(pts[i] > pts[j]){
			j <- i
			out <- 0
			pos <- "asc"
		} else if(pts[i] < pts[j] & 
				out < minPts & 
				pos == "desc"){
			j <- i
		} else if(pts[i] < pts[j] & 
				out < minPts & 
				pos == "asc"){
			out <- out + 1
		} else if(pts[i] < pts[j] & 
				out >= minPts & 
				pos == "asc"){
			pts2 <- c(pts2, j)
			j <- i
			out <- 0
			pos <- "desc"
		} else next
	}
	if(length(pts2) > 0) pts2 <- pts2[which(
		pts[pts2] > noise)]
	localMax <- rep(0, length(pts))
	localMax[pts2] <- 1
	localMax
}

narrow_rt_boundaries <- function(lm, d, minPts, thresh = 0){
    lm_seq <- lm[1]:lm[2]
    above_thresh <- lm_seq[which(d[lm_seq] > 0)]
	if(length(above_thresh) == 0) return(c(0, 0))
	above_thresh <- split(above_thresh, cumsum(c(TRUE, 
		diff(above_thresh) >= minPts)))
	lm <- above_thresh[[which.max(lengths(above_thresh))]] %>% 
		range
	l <- if(lm[1] - 1 < 1) 1 else lm[1] - 1
	r <- if(lm[2] + 1 > length(d)) length(d) else lm[2] + 1
	c(l, r)
}

overlap <- function(peaks, mzdiff){
	uorder <- order(peaks$into, decreasing = TRUE)
	pm <- as.matrix(peaks[,c("mzmin", "mzmax", "rtmin", "rtmax"), drop = FALSE])
	uindex <- xcms:::rectUnique(pm, uorder, mzdiff, ydiff = -0.00001) ## allow adjacent peaks
	peaks[uindex, , drop = FALSE]
}

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
