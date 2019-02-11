output$uiTargetProject <- renderUI({
	db <- dbConnect(SQLite(), sqlitePath)
	projects <- dbGetQuery(db, 'select name as project from project;')
	dbDisconnect(db)
	projects <- if(nrow(projects) == 0) c() else projects$project
	pickerInput('targetProject', 'project', choices=projects, multiple=FALSE, width='75%')
})

output$uiTargetSamples <- renderUI({
	db <- dbConnect(SQLite(), sqlitePath)
	samples <- dbGetQuery(db, 'select sample from sample;')
	dbDisconnect(db)
	samples <- if(nrow(samples) == 0) c() else samples$sample
	pickerInput('targetSamples', 'samples', choices=samples, multiple=TRUE, options=list(
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


# load for each chloroparaffine with adduct two mz
# the formula for a chloroparaffine is C(n)H(2n+2-x)Cl(x)
loadChloroParaMzs <- function(adduct){
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
	reduce(data, function(a, b) a %>% bind_rows(
		b %>% data.frame %>% top_n(2, abundance) %>% 
			select(`m.z`, abundance)), .init=data.frame()) %>% 
		cbind(formula=rep(formulas, each=2))
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

targetChloroPara <- function(eics, rts, minScans, ppm, snthresh, abdTheo, file=NULL){
	tryCatch({
		rois <- data.frame(mzO=c(), rtmin=c(), rtmax=c(), auc=c(), snthresh=c())
		data1 <- data.frame(x=rts, y=0)
		data2 <- data.frame(x=rts, y=0)
		data1[which(data1$x %in% eics[[1]]$times), 'y'] <- eics[[1]]$intensities[which(eics[[1]]$times %in% data1$x)]
		data2[which(data2$x %in% eics[[2]]$times), 'y'] <- eics[[2]]$intensities[which(eics[[2]]$times %in% data2$x)]
		
		scans <- which(data1$y > mean(data1$y) + (sd(data1$y)))
		scans <- split(scans, cumsum(c(TRUE, diff(scans) > 2)))
		scans <- scans[which(lengths(scans) > minScans)]
		if(length(scans) == 0) custom_stop('fail', 'no chloroparaffin detected')
		
		baseline1 <- runmed(data1$y, 1+2*(min(c((nrow(data1)-1)%/%2, ceiling(.15*nrow(data1))))), 
			endrule="median", algorithm="Turlach")
		baseline2 <- runmed(data2$y, 1+2*(min(c((nrow(data2)-1)%/%2, ceiling(.15	*nrow(data2))))),
			endrule="median", algorithm="Turlach")
				
		for(i in 1:length(scans)){
			scans1 <- scans[[i]]
			scans2 <- scans[[i]][which(data2[scans[[i]], 'y'] >= baseline2[scans[[i]]])]
			# enlarge the roi until it meet the baseline
			# first left
			while(baseline1[min(scans1)-1] < data1[min(scans1)-1, 'y']) scans1 <- scans1 %>% prepend(min(scans1)-1)
			while(baseline2[min(scans2)-1] < data2[min(scans2)-1, 'y']) scans2 <- scans2 %>% prepend(min(scans2)-1)
			while(baseline1[max(scans1)+1] < data1[max(scans1)+1, 'y']) scans1 <- scans1 %>% append(max(scans1)+1)
			while(baseline2[max(scans2)+1] < data2[max(scans2)+1, 'y']) scans2 <- scans2 %>% append(max(scans2)+1)
			
			auc1 <- trapz(data1[scans1, 'x'], data1[scans1, 'y']) - 
					trapz(data1[scans1, 'x'], baseline1[scans1])
			auc2 <- trapz(data2[scans2, 'x'], data2[scans2, 'y']) - 
					trapz(data2[scans2, 'x'], baseline2[scans2])
			# check if the A-2 or A+2 not > A
			if(auc2 > auc1 + auc1 * .2) next
			# search the scan where A is the most intense
			scanMaxI1 <- scans1[which(data1[scans1, "y"] == max(data1[scans1, "y"]))][1]
			scanMaxI2 <- scans2[which(data2[scans2, "y"] == max(data2[scans2, "y"]))][1]
			# loading spectra takes too long
			# mzO <- findClosestMzs(sapply(eics, function(x) x$masses), file, ppm, scanMaxI)
			# if(length(mzO[1]) == 0 | length(mzO[2]) == 0) next
			# check the S/N
			snthresh1 <- if(sd(baseline1) != 0) (data1[scanMaxI1, 'y'] - baseline1[scanMaxI1]) / sd(baseline1) else (data1[scanMaxI1, 'y'] - baseline1[scanMaxI1])
			snthresh2 <- if(sd(baseline2) != 0) (data2[scanMaxI2, 'y'] - baseline2[scanMaxI2]) / sd(baseline2) else (data2[scanMaxI2, 'y'] - baseline2[scanMaxI2])
			if(any(c(snthresh1, snthresh2) < snthresh)) next
			
			abdObs <- auc2 / auc1 * 100
			
			rois <- rois %>% bind_rows(data.frame(mzO=c(eics[[1]]$mass, eics[[2]]$mass),
				rtmin=c(data1[min(scans1), 'x'], data2[min(scans2), 'x']),
				rtmax=c(data1[max(scans1), 'x'], data2[max(scans2), 'x']),
				auc=c(auc1, auc2), abd_deviation=abs(abdTheo - c(100, abdObs)), snthresh=c(snthresh1, snthresh2)))
		}
		# for printing the eics
		# print(
		# plot_ly(type="scatter", mode="lines", dragmode="select") %>% layout(selectdirection="h")
			# add_lines(data=data1, x=~x, y=~y, color=I('black')) %>%
			# add_trace(mode='none', data=data1[scans1, ], x=~x, y=~y, fill='tozeroy') %>% 
			# add_lines(x=data1$x, y=baseline1, color=I('red')) %>%
			# add_lines(data=data2, x=~x, y=~y, color=I('black')) %>%
			# add_trace(mode='none', data=data2[scans2, ], x=~x, y=~y, fill='tozeroy') %>% 
			# add_lines(x=data2$x, y=baseline2, color=I('red'))
		# )
		rois
	}, fail = function(f) data.frame()
	, error = function(e){
		print(e$message)
		sendSweetAlert(e$message)
		data.frame()
	})	
}

observeEvent(input$target, {
	print('------------------- TARGET --------------------')
	print(list(project=input$project, samples=input$targetSamples, 
		adduct=input$targetAdduct, ppm=input$targetTolPpm, 
		peakwidth=input$targetPeakwidth,
		snthresh=input$targetSnthresh))
	tryCatch({
		inputs <- c('targetProject', 'targetSamples', 
			'targetAdduct', 'targetTolPpm', 'targetPeakwidth',
			'targetSnthresh')
		conditions <- c(is.null(input$project), is.null(input$targetSamples),
			is.null(input$targetAdduct), !is.numeric(input$targetTolPpm), 
			!is.numeric(input$targetPeakwidth), !is.numeric(input$targetSnthresh))
		messages <- c('You need to select a project', 'You need to select at least one sample',
			'You need to select an adduct', 'tol ppm has to be numeric', 'min peakwidth has to be numeric',
			'snthresh has to be numeric')
		test <- inputsTest(inputs, conditions, messages)
		if(!test) custom_stop('invalid', 'invalid args')
		inputs <- c('targetProject', 'targetSamples', 'targetAdduct')
		conditions <- c(input$project == "", length(input$targetSamples) == 0,
			input$targetAdduct == "")
		messages <- c('You need to select a project', 'You need to select at least one sample',
			'You need to select an adduct')
		test <- inputsTest(inputs, conditions, messages)
		if(!test) custom_stop('invalid', 'invalid args')
		
		progressSweetAlert(session, id='pb', title='Target', value=0, display_pct=TRUE)
		db <- dbConnect(SQLite(), sqlitePath)
		# load mzs of each chloroparaffin
		chloropara <- loadChloroParaMzs(input$targetAdduct)
		# then load eics for each file
		eics <- list()
		pbVal <- 100 / length(input$targetSamples)
		for(i in 1:length(input$targetSamples)){
			updateProgressBar(session, id="pb", title=paste("load eic", input$targetSamples[i]),
				value=0)
				
			# record the project_sample & delete precedent records
			query <- sprintf('select project_sample from project_sample where project == "%s" and
				sample == "%s" and adduct == "%s";', input$project, input$targetSamples[i], input$targetAdduct)
			print(query)
			project_sample <- dbGetQuery(db, query)
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
				query <- sprintf('delete from observed where project_sample == %s;',
					project_sample, input$targetAdduct)
				print(query)
				dbSendQuery(db, query)			
			}
			
			path <- dbGetQuery(db, sprintf('select path from sample where sample == "%s";', 
				input$targetSamples[i]))$path
			
			eics <- readXICs(path, masses=chloropara[, 'm.z'], tol=input$targetTolPpm)
			# split to analyze two by two (one chloroparaffin : two mz)
			eics <- split(eics, rep(1:(nrow(chloropara)/2), each=2) %>% as.factor)
			raw <- read.raw(path)
			rts <- raw$StartTime
			minScans <- floor((input$targetPeakwidth/60) / mean(diff(rts)))
			
			rois <- data.frame(mzO=c(), formula=c(), rtmin=c(), rtmax=c(), auc=c(), abd_deviation=c(), snthresh=c())
			for(j in 1:length(eics)){
			# for(j in 100:200){
				updateProgressBar(session, id="pb", title=paste("target in", input$targetSamples[i]),
					value=(i-1)*100/length(input$targetSamples) + j*100/length(eics))
				roisTmp <- targetChloroPara(eics[[j]], rts, minScans, 
					input$targetTolPpm, input$targetSnthresh, chloropara[j*2, 'abundance'])
				if(nrow(roisTmp) > 0) rois <- rois %>% bind_rows(roisTmp %>% 
						cbind(formula=unique(chloropara$formula)[j]))
			}
			rois$C <- str_extract(rois$formula, 'C[[:digit:]]+') %>% str_extract('[[:digit:]]+') %>% as.numeric
			rois$Cl <- str_extract(rois$formula, 'Cl[[:digit:]]+') %>% str_extract('[[:digit:]]+') %>% as.numeric
			rois$Cl[which(is.na(rois$Cl))] <- 0
			
			query <- sprintf('insert into observed (mz, formula, rtmin, rtmax, auc, project_sample, ppm,
				peakwidth, snthresh, abd_deviation, C, Cl) values %s;', paste('(', rois$mzO, ', "', rois$formula,
					'", ', rois$rtmin, ', ', rois$rtmax, ', ', rois$auc, 
					', ', project_sample, ', ', input$targetTolPpm, ', ', input$targetPeakwidth, 
					', ', input$targetSnthresh, ', ', rois$abd_deviation, 
					', ', rois$C, ', ', rois$Cl, ')', collapse=', ', sep=''))
			print(query)
			
			dbSendQuery(db, query)
			updateOutput$details <- if(updateOutput$details) FALSE else TRUE
		}
		closeSweetAlert(session)
		dbDisconnect(db)
		}, invalid = function(i){
			print(i$message)
		}, error = function(e){
			print(e$message)
			sendSweetAlert(e$message)
		})
})
