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
		plot_ly(type="scatter", mode="lines", x=x, y=y) %>% layout(xaxis=list(title="Retention time(min)"),
			yaxis=list(title="Intensity"), showlegend=FALSE)
	}, invalid = function(i){
		print(paste(i$message))
		plot_ly(type="scatter", mode="lines") %>% layout(xaxis=list(title="Retention time(min)"),
			yaxis=list(title="Intensity"), showlegend=FALSE)
	}, error = function(e){
		print(paste(e$message))
		sendSweetAlert(e$message)
		plot_ly(type="scatter", mode="lines", x=x, y=y) %>% layout(xaxis=list(title="Retention time(min)"),
			yaxis=list(title="Intensity"), showlegend=FALSE)
	})
	print('------------------- END TARGET TIC -----------------')
	tic
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
	brute_formulas <- str_replace_all(formulas, '[[:upper:]][[:lower:]]?0', '')
	formulas <- formulas[which(brute_formulas != '')]
	brute_formulas <- brute_formulas[which(formulas != '')]
	data <- isopattern(isotopes, brute_formulas, charge=adduct$Charge, verbose=FALSE)
	mzs <- reduce(data, function(a, b) c(a, b %>% data.frame %>% 
		top_n(2, abundance) %>% pull(m.z)), .init=c())
	data.frame(formulas=rep(formulas, each=2), mzs=mzs)
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

targetChloroPara <- function(eics, rts, minScans, collapseScans, ppm, snthresh, file=NULL){
	tryCatch({
		rois <- data.frame(mzO=c(), rtmin=c(), rtmax=c(), auc=c(), snthresh=c())
		data1 <- data.frame(x=rts, y=0)
		data2 <- data.frame(x=rts, y=0)
		data1[which(data1$x %in% eics[[1]]$times), 'y'] <- eics[[1]]$intensities[which(eics[[1]]$times %in% data1$x)]
		data2[which(data2$x %in% eics[[2]]$times), 'y'] <- eics[[2]]$intensities[which(eics[[2]]$times %in% data2$x)]
		
		scans <- which(data1$y > mean(data1$y) + (sd(data1$y)))
		scans <- split(scans, cumsum(c(TRUE, diff(scans) > collapseScans)))
		scans <- scans[which(lengths(scans) > minScans)]
		if(length(scans) == 0) custom_stop('fail', 'no chloroparaffin detected')
		scans <- map(scans, function(x) (min(x)-1):(max(x)+1))
		# k need to be odd
		baseline1 <- runmed(data1$y, nrow(data1[which(data1$y == 0), ]) + 
			(nrow(data1[which(data1$y == 0), ]) %% 2) - 1)
		baseline2 <- runmed(data2$y, nrow(data2[which(data2$y == 0), ]) + 
			(nrow(data2[which(data2$y == 0), ]) %% 2) - 1)
				
		for(i in 1:length(scans)){
			auc1 <- trapz(data1[scans[[i]], 'x'], data1[scans[[i]], 'y']) - 
					trapz(data1[scans[[i]], 'x'], baseline1[scans[[i]]])
			auc2 <- trapz(data2[scans[[i]], 'x'], data2[scans[[i]], 'y']) - 
					trapz(data2[scans[[i]], 'x'], baseline2[scans[[i]]])
			# check if the A-2 or A+2 not > A
			if(auc2 > auc1 + auc1 * .2) next
			# search the scan where A is the most intense
			scanMaxI <- scans[[i]][which(data1[scans[[i]], "y"] == max(data1[scans[[i]], "y"]))]
			# loading spectra takes too long
			# mzO <- findClosestMzs(sapply(eics, function(x) x$masses), file, ppm, scanMaxI)
			# if(length(mzO[1]) == 0 | length(mzO[2]) == 0) next
			# check the S/N
			snthresh1 <- if(sd(baseline1) != 0) (data1[scanMaxI, 'y'] - baseline1[scanMaxI]) / sd(baseline1)
					else (data1[scanMaxI, 'y'] - baseline1[scanMaxI])
			snthresh2 <- if(sd(baseline2) != 0) (data2[scanMaxI, 'y'] - baseline2[scanMaxI]) / sd(baseline2)
					else (data2[scanMaxI, 'y'] - baseline2[scanMaxI])
			if(any(c(snthresh1, snthresh2) < snthresh)) next
			
			rois <- rois %>% bind_rows(data.frame(mzO=c(eics[[1]]$mass, eics[[2]]$mass),
				rtmin=c(data1[min(scans[[i]]), 'x'], data2[min(scans[[i]]), 'x']),
				rtmax=c(data1[max(scans[[i]]), 'x'], data2[max(scans[[i]]), 'x']),
				auc=c(auc1, auc2), snthresh=c(snthresh1, snthresh2)))
		}
		# for printing the eics
		# if(length(rois) > 0) print(plot_ly(type="scatter", mode="lines") %>% 
			# add_lines(data=data1, x=~x, y=~y, color=I('black')) %>%
			# add_trace(mode='none', data=data1[unlist(scans), ], x=~x, y=~y, fill='tozeroy') %>% 
			# add_lines(x=data1$x, y=baseline1, color=I('red')) %>%
			# add_lines(data=data2, x=~x, y=~y, color=I('black')) %>%
			# add_trace(mode='none', data=data2[unlist(scans), ], x=~x, y=~y, fill='tozeroy') %>% 
			# add_lines(x=data2$x, y=baseline2, color=I('red')))
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
		prefilterStep=input$targetPrefilterStep,
		prefilterLevel=input$targetPrefilterLevel,
		snthresh=input$targetSnthresh))
	tryCatch({
		inputs <- c('targetProject', 'targetSamples', 
			'targetAdduct', 'targetTolPpm', 'targetPeakwidth',
			'targetDiffRt', 'targetSnthresh')
		conditions <- c(is.null(input$project), is.null(input$targetSamples),
			is.null(input$targetAdduct), !is.numeric(input$targetTolPpm), 
			!is.numeric(input$targetPeakwidth), !is.numeric(input$targetDiffRt),
			!is.numeric(input$targetSnthresh))
		messages <- c('You need to select a project', 'You need to select at least one sample',
			'You need to select an adduct', 'tol ppm has to be numeric', 'min peakwidth has to be numeric',
			'min collapse peaks has to be numeric', 'snthresh has to be numeric')
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
		mzs <- loadChloroParaMzs(input$targetAdduct)
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
			
			eics <- readXICs(file.path(rawPath, paste0(input$targetSamples[i], '.raw')), 
				masses=mzs$mzs, tol=input$targetTolPpm)
			# split to analyze two by two (one chloroparaffin : two mz)
			eics <- split(eics, rep(1:(nrow(mzs)/2), each=2) %>% as.factor)
			raw <- read.raw(file.path(rawPath, paste0(input$targetSamples[i], '.raw')))
			rts <- raw$StartTime
			minScans <- floor((input$targetPeakwidth/60) / mean(diff(rts)))
			collapseScans <- ceiling((input$targetDiffRt/60) / mean(diff(rts)))
			
			rois <- data.frame(mzO=c(), formula=c(), rtmin=c(), rtmax=c(), auc=c(), snthresh=c())
			for(j in 1:length(eics)){
				updateProgressBar(session, id="pb", title=paste("target in", input$targetSamples[i]),
					value=(i-1)*100/length(input$targetSamples) + j*100/length(eics))
				roisTmp <- targetChloroPara(eics[[j]], rts, minScans, 
					collapseScans, input$targetTolPpm, input$targetSnthresh)
				if(nrow(roisTmp) > 0) rois <- rois %>% bind_rows(roisTmp %>% 
						cbind(formula=unique(mzs$formulas)[j]))
			}
						
			query <- sprintf('insert into observed (mz, formula, rtmin, rtmax, auc, project_sample, ppm,
				peakwidth, diffRt, snthresh) values %s;', paste('(', rois$mzO, ', "', rois$formula,
					'", ', rois$rtmin, ', ', rois$rtmax, ', ', rois$auc, 
					', ', project_sample, ', ', input$targetTolPpm, ', ', input$targetPeakwidth, 
					', ', input$targetDiffRt, ', ', input$targetSnthresh, ')', collapse=', ', sep=''))
			print(query)
			dbSendQuery(db, query)
			dbDisconnect(db)
		}
		})
	closeSweetAlert(session)
})
