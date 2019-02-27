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

observeEvent(input$target, {
	print('------------------- TARGET --------------------')
	print(list(project=input$project, samples=input$targetSamples, 
		adduct=input$targetAdduct, ppm=input$targetTolPpm, 
		peakwidth=input$targetPeakwidth,
		machine=names(resolution_list)[input$targetMachine %>% as.numeric]))
	tryCatch({
		inputs <- c('targetProject', 'targetSamples', 
			'targetAdduct', 'targetTolPpm', 'targetPeakwidth',
			'targetMachine')
		conditions <- c(is.null(input$project), is.null(input$targetSamples),
			is.null(input$targetAdduct), !is.numeric(input$targetTolPpm), 
			!is.numeric(input$targetPeakwidth), is.null(input$targetMachine))
		messages <- c('You need to select a project', 'You need to select at least one sample',
			'You need to select an adduct', 'tol ppm has to be numeric', 'min peakwidth has to be numeric',
			'You need to select a machine')
		test <- inputsTest(inputs, conditions, messages)
		if(!test) custom_stop('invalid', 'invalid args')
		inputs <- c('targetProject', 'targetSamples', 'targetAdduct', 'targetMachine')
		conditions <- c(input$project == "", length(input$targetSamples) == 0,
			input$targetAdduct == "", input$targetMachine == '')
		messages <- c('You need to select a project', 'You need to select at least one sample',
			'You need to select an adduct')
		test <- inputsTest(inputs, conditions, messages)
		if(!test) custom_stop('invalid', 'invalid args')
		
		progressSweetAlert(session, id='pb', title='Target', value=0, display_pct=TRUE)
		db <- dbConnect(SQLite(), sqlitePath)
		# load mzs of each chloroparaffin
		chloropara <- loadChloroParaMzs(input$targetAdduct, input$targetMachine %>% as.numeric)
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
			
			eics <- readXICs(path, masses=chloropara$mz, tol=input$targetTolPpm)
			
			# rearrange eic from rawDiag to have a dataframe for each eic & not a list
			raw <- read.raw(path)
			rts <- raw$StartTime
			eics <- map(eics, function(eic) arrangeEICRawDiag(eic, rts))
			
			# split to analyze by chloroparaffin
			eics <- split(eics, chloropara$formula %>% as.factor)
			chloropara <- split(chloropara, chloropara$formula %>% as.factor)
			minScans <- floor((15/60) / mean(diff(rts)))
	
			rois <- data.frame(mz=c(), formula=c(), rtmin=c(), rtmax=c(), auc=c(), abd=c(), score=c())
			for(j in 1:length(eics)){
			# for(j in 100:200){
				updateProgressBar(session, id="pb", title=paste("target in", input$targetSamples[i]),
					value=(i-1)*100/length(input$targetSamples) + j*100/length(eics))
				roisTmp <- targetChloroPara(eics[[j]], minScans, chloropara[[j]])
				if(nrow(roisTmp) > 0) rois <- rois %>% bind_rows(roisTmp %>% 
						cbind(formula=unique(chloropara[[j]]$formula)))
			}
			rois$C <- str_extract(rois$formula, 'C[[:digit:]]+') %>% str_extract('[[:digit:]]+') %>% as.numeric
			rois$Cl <- str_extract(rois$formula, 'Cl[[:digit:]]+') %>% str_extract('[[:digit:]]+') %>% as.numeric
			rois$Cl[which(is.na(rois$Cl))] <- 0
			
			query <- sprintf('insert into observed (mz, formula, rtmin, rtmax, auc, project_sample, ppm,
				peakwidth, C, Cl, abundance, score, machine) values %s;', paste('(', rois$mz, ', "', rois$formula,
					'", ', rois$rtmin, ', ', rois$rtmax, ', ', rois$auc, 
					', ', project_sample, ', ', input$targetTolPpm, ', ', input$targetPeakwidth, 
					', ', rois$C, ', ', rois$Cl, ', ', rois$abd, ', ', rois$score, ', ', input$targetMachine %>% as.numeric, ')', collapse=', ', sep=''))
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

getAUCs <- function(eic, roi, windowRTMed){
	baseline <- runmed(eic$y, windowRTMed, endrule="median", algorithm="Turlach")
	trapz(eic[roi, 'x'], eic[roi, 'y']) - 
		trapz(eic[roi, 'x'], baseline[roi])
}

targetChloroPara <- function(eics, minScans, theo){
	tryCatch({
		res <- data.frame(mz=c(), rtmin=c(), rtmax=c(), auc=c(), score=c())
		
		eic <- eics[[1]]
		# search the ROI
		rois <- which(eic$y > mean(eic$y) + (sd(eic$y)))
		rois <- split(rois, cumsum(c(TRUE, diff(rois) > 2)))
		rois <- rois[which(lengths(rois) > minScans)]
		if(length(rois) == 0) custom_stop('fail', 'no chloroparaffin detected')
		
		windowRTMed <- 1+9*(length(unlist(rois))%/%2)
		baseline <- runmed(eic$y, windowRTMed, endrule="median", algorithm="Turlach")
		
		rois2 <- c()
		# for each ROI
		for(roi in rois){
			# enlarge the ROI until the baseline
			minScan <- min(roi)
			while(baseline[minScan-1] < eic[minScan-1, 'y'] & 
				baseline[minScan-2] < eic[minScan-2, 'y']) minScan <- minScan-1
			minScan <- minScan-1
			maxScan <- max(roi)
			while(baseline[maxScan+1] < eic[maxScan+1, 'y'] & 
				baseline[maxScan+2] < eic[maxScan+2, 'y']) maxScan <- maxScan+1
			maxScan <- maxScan+1
			rois2 <- c(rois2, minScan:maxScan)
		}
		
		# merge ROI if they cross each other
		rois <- rois2 %>% unique %>% sort
		rois <- split(rois, cumsum(c(TRUE, diff(rois) > 2)))
		
		for(roi in rois){
			aucs <- reduce(eics, function(a, b) 
				c(a, getAUCs(b, roi, windowRTMed)), .init = c())
			if(aucs[2] <= 0) custom_stop('fail', 'auc of A2 is O')
			theo <- theo[which(aucs > 0), ]
			aucs <- aucs[which(aucs > 0)]
			abdObs <- sapply(aucs, function(x) x * 100 / aucs[1])
			score <- (abdObs[2] - theo[2, 'abundance']) / theo[2, 'abundance'] * 100
			
			res <- res %>% bind_rows(data.frame(mz=theo$mz,
				rtmin=eic[min(roi), 'x'], rtmax=eic[max(roi), 'x'], 
				auc=aucs, abd=abdObs, score=score))
		}
		res
	}, fail = function(f) data.frame()
	, error = function(e){
		print(e$message)
		sendSweetAlert(e$message)
		data.frame()
	})	
}