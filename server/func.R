`%!in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

#conversion factor to numeric directly
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

# read in sql-statements and preformat them                                        
dbDisconnect <- function(db){
	suppressWarnings(RSQLite::dbDisconnect(db))
}
dbGetQuery <- function(db, query){
	suppressWarnings(RSQLite::dbGetQuery(db, query))
}

dbExecute <- function(db, query, ...){
	msg <- "database is locked"
	while(msg == "database is locked"){
		msg <- tryCatch({
			suppressWarnings(RSQLite::dbExecute(db, query, ...))
			"success"
		}, error = function(e){
			e$message
		})
	}
	if(msg != "success") stop(msg)
}


# read from a file with SQL statements
sqlFromFile <- function(file){
    require(stringr)
	sql <- readLines(file)
    sql <- unlist(str_split(paste(sql,collapse=" "),";"))
    sql <- sql[grep("^ *$", sql, invert=T)]
}

# apply query function to each element
dbSendQueries <- function(con,sql, ...){
    dummyfunction <- function(sql,con){
        dbSendQuery(con,sql, ...)
    }
    lapply(sql, dummyfunction, con)
}

toastr_error <- function(title="", msg=""){
	shinytoastr::toastr_error(msg, title, closeButton=TRUE, newestOnTop=TRUE, position='top-center', preventDuplicates=TRUE)
}

toastr_success <- function(title="", msg=""){
	shinytoastr::toastr_success(msg, title, closeButton=TRUE, newestOnTop=TRUE, position='top-center', preventDuplicates=TRUE)
}

toastr_warning <- function(title="", msg=""){
	shinytoastr::toastr_warning(msg, title, closeButton=TRUE, newestOnTop=TRUE, position='top-center', preventDuplicates=TRUE)
}

sendSweetAlert <- function(title = "Unexpected error", text = "something weird occur"){
	shinyWidgets::sendSweetAlert(session, title = title, text = text, type='error')
}

# check if the inputs respect the conditions
# if not it use shinyFeedback to hightlight the input and send a message
# the condition has to be FALSE
inputsTest <- function(inputs, conditions, messages){
	if(length(inputs) == 0) return(TRUE)
	for(i in 1:length(inputs)){
		feedbackDanger(inputs[i], conditions[i], messages[i])
		if(conditions[i]){
			print(paste('ERR: ', messages[i]))
			toastr_error(messages[i])
		}
	}
	return(!any(conditions))
}

condition <- function(subclass, message, call=sys.call(-1), ...){
	structure(
		class=c(subclass, "condition"),
		list(message=message, call=call),
		...
	)
}

custom_stop <- function(subclass, message, call=sys.call(-1), ...){
	c <- condition(c(subclass, "error"), message, call=call, ...)
	stop(c)
}

# function provided by ProtGenerics
fileIsCentroided <- function(msFile){
	sapply(1:length(msFile@scanindex), function(i) 
		getScan(msFile, i) %>% as.data.frame %>% 
		isCentroided)
}

isCentroided <- function(spectra, k = .025, qtl = .9){
	spectra %>% filter(intensity > quantile(spectra$intensity, qtl)) %>% 
		pull(mz) %>% diff %>% quantile(.25) > k
}

deleteProjects <- function(projects=NULL){
	print('delete projects')
	print(list(projects=projects))
	if(is.null(projects)) return()
	
	deleteProject_sample(project_samples() %>% 
		filter(project %in% projects) %>% 
		pull(project_sample))
		
	query <- sprintf("delete from project where project in (%s);",
		paste(projects, collapse=', ', sep=''))
	print(query)
	dbExecute(db, query)
	actualize$projects <- TRUE
}

deleteProject_sample <- function(project_samples = NULL){
	print('delete project_samples')
	if(is.null(project_samples)) return()
	deleteDatas(project_samples)
	query <- sprintf('delete from project_sample where project_sample in (%s);',
		paste(project_samples, collapse=', '))
	print(query)
	dbExecute(db, query)
	actualize$project_samples <- TRUE
}

deleteDatas <- function(project_samples = NULL){
	print('delete datas')
	if(is.null(project_samples)) return()
	query <- sprintf('delete from feature where cluster in (
		select cluster from cluster where project_sample in (%s));', 
		paste(project_samples, collapse=', '))
	print(query)
	dbExecute(db, query)
	query <- sprintf('delete from cluster where project_sample in (%s);', 
		paste(project_samples, collapse=', '))
	print(query)
	dbExecute(db, query)
}

deleteSamples <- function(samples=NULL){
	print('delete samples')
	print(list(samples=samples))
	if(is.null(samples)) return()
	
	deleteProject_sample(project_samples() %>% 
		filter(sample %in% samples) %>% 
		pull(project_sample))
		
	query <- sprintf("delete from sample where sample in (%s);",
		paste('\"', samples, '\"', collapse=', ', sep=''))
	print(query)
	dbExecute(db, query)
	actualize$samples <- TRUE
}

# compute the score 
# tolMz need to be in Da
calculateScore <- function(cluster, theoric, ppm, tolI){
	if(nrow(cluster) == 0 | nrow(theoric) == 0) return(0)
	cluster <- cluster %>% mutate(abundance = into / max(into) * 100) %>% 
		arrange(desc(abundance)) %>% 
		mutate(weight = abundance / sum(abundance), id = 1:n())
	theoric <- theoric %>% arrange(desc(abundance)) %>% 
			mutate(weight = abundance / sum(abundance), id = 1:n())
	50 * (2 - calculateScore2(cluster, theoric, ppm, tolI))
}
calculateScore2 <- function(cluster, theoric, ppm, tolI){
	if(nrow(cluster) == 0 | nrow(theoric) == 0) return(
		sum(theoric$weight) + sum(cluster$weight))
	res <- calculateScore3(cluster[1, ], theoric, ppm, tolI)
	theoric <- res$theoric
	res$score + calculateScore2(cluster[-1, ], theoric, ppm, tolI) # cumulative sum
}
calculateScore3 <- function(feature, theoric, ppm, tolI){
	# first search the matched observed feature
	tolMz <- feature$mz * ppm * 10**-6
	theoricFeature <- theoric %>% filter(between(mz, feature$mz - tolMz, 
		feature$mz + tolMz)) %>% 
		mutate(deviation = ((feature$abundance - abundance) %>% abs) / tolI) %>% 
		filter(deviation < 1) %>% top_n(1, abundance)
	# return the minimal score (0: perfect match, 1: wrong match)
	# remove the matched feature for avoid an other assignation with a feature
	if(nrow(theoricFeature) == 0) list(
		score = feature$weight, 
		theoric = theoric
	) else list(
		score = theoricFeature$deviation * (theoricFeature$weight + 
			feature$weight),
		theoric = theoric %>% filter(id != theoricFeature$id)
	)
}

# compute the deviation (return in mDa)
calculateDeviation <- function(cluster, theoric){
	if(nrow(cluster) == 0 | nrow(theoric) == 0) return(0)
	cluster <- cluster %>% arrange(desc(abundance))
	theoric <- theoric %>% arrange(desc(abundance))
	
	round(calculateDeviation2(cluster, theoric) * 10**3, digits=2)
}
calculateDeviation2 <- function(cluster, theoric){
	if(nrow(theoric) == 0 | nrow(cluster) == 0) return(0)
	res <- calculateDeviation3(cluster[1, ], theoric)
	theoric <- res$theoric
	res$score + calculateDeviation2(cluster[-1, ], theoric) # cumulative sum
}
calculateDeviation3 <- function(feature, theoric){
	theoricFeatures <- theoric %>% mutate(deviation = feature$mz - mz)
	theoricFeatureID <- which.min(abs(theoricFeatures$deviation))
	# remove the matched feature for avoid an other assignation with a feature
	list(
		score = theoricFeatures[theoricFeatureID, 'deviation'], 
		theoric = theoric[-theoricFeatureID, ]
	)
}

getIonMz <- function(formulas, adduct, charges){
	data <- getIonFormula(formulas, adduct, charges) %>% 
		mutate(adduct = adduct$adduct)
	# cannot pass all formulas because if there is one charge at 0 -> bug
	data$mz <- sapply(1:nrow(data), function(i) 
		isopattern(isotopes, data[i, 'ion_formula'], charge = data[i, 'charge'], 
			verbose=FALSE)[[1]] %>% data.frame %>% 
			top_n(1, abundance) %>% pull(m.z))
	data
}

getIonFormula <- function(formulas, adduct, charges = NULL){
	if(is.null(charges)) charges <- rep(0, length(formulas))
	else if(length(charges) != length(formulas)) return(data.frame(
		row = c(), formula = c(), ion_formula = c()))
	check_chemform(isotopes, formulas) %>% mutate(
			row = 1:n(), 
			formula = new_formula, 
			charge = charges + as.numeric(adduct$charge)) %>% 
		filter(!warning) %>% mutate(
			ion_formula = multiform(formula, as.numeric(adduct$multi)) %>% 
				mergeform(adduct$formula_add) %>% 
				mergeform("H0") %>% 
				subform(adduct$formula_ded)) %>% 
		filter(!grepl('not', ion_formula)) %>% 
		select(row, formula, ion_formula, charge)
}


getNeutralFormula <- function(ion_formulas, adduct){
	check_chemform(isotopes, ion_formulas) %>% dplyr::mutate(
			row = 1:n(), ion_formula = new_formula) %>% 
		filter(!warning) %>% mutate(
			formula = multiform(new_formula, 1/adduct$multi) %>% 
				mergeform(adduct$formula_ded) %>% 
				subform(adduct$formula_add)) %>% 
		filter(!grepl('not', formula)) %>% 
		select(row, formula, ion_formula)
}

# return the theoretic pattern of formulas
# it need a vector with formulas, a charge and a resolution (can be the instrument or an integer)
# the data for instrument is give by the package enviPat
theoricClustersFunction <- function(formulas, charges, resolution){
	if(any(!is.numeric(charges)) | is.null(resolution)){
		print('charge or resolution is null')
		return(list(data.frame()))
	}
	else if(!resolution %in% names(resolution_list) & (
			!is.numeric(resolution) | resolution == "0")){
		print(paste('resolution incorrect:', resolution))
		return(list(data.frame()))
	}

	res <- check_chemform(isotopes, formulas)

	data <- list()
	if(resolution %in% names(resolution_list)){
		resmass <- resolution_list[[which(names(resolution_list) == resolution)]]
		massLimit <- range(resmass[, 'm/z'])
		
		# check if any mass is upper or below
		test <- res[, 'monoisotopic_mass'] < massLimit[1] | 
			res[, 'monoisotopic_mass'] > massLimit[2]
		data[which(test)] <- data.frame()
		data[which(!test)] <- isowrap(isotopes, checked=res[which(!test), ], 
				resmass=resmass, threshold=1, charge=charges[which(!test)]) %>% 
			map(data.frame) %>% map(setNames, c('mz', 'abundance'))
		data
	} else isowrap(isotopes, checked=res, resmass=FALSE, resolution=as.numeric(resolution),
			threshold=1, charge=charges) %>% map(data.frame %>% 
				rename(mz = `m.z`))
}

# function from the package enviPat
isowrap <- function (isotopes, checked, resmass, resolution = FALSE, nknots = 6, 
	spar = 0.2, threshold = 0.1, charge = 1, emass = 0.00054858, 
	algo = 2, ppm = FALSE, dmz = "get", frac = 1/4, env = "Gaussian", 
	detect = "centroid", plotit = FALSE, verbose = FALSE){
	if (any(checked[, 1])){
		stop("WARNING: checked with incorrect chemical detected!")
	}
	if (length(resmass) > 1){
		resolution <- getR(checked, resmass = resmass, nknots = nknots, 
			spar = spar, plotit = plotit)
	}
	
	pattern <- isopattern(isotopes, checked[, 2], threshold = threshold, 
		charge = charge, emass = emass, plotit = plotit, algo = algo, verbose = verbose)
	profiles <- envelope(pattern, ppm = ppm, dmz = dmz, frac = frac, 
		env = env, resolution = resolution, plotit = plotit, verbose = verbose)
	centro <- vdetect(profiles, detect = detect, plotit = plotit, verbose = verbose)
	return(centro)
}

loadRawFile <- function(db, pj){
	msFile <- tryCatch(dbGetQuery(db, sprintf('select raw from sample where sample == (
		select sample from project_sample where project_sample == %s);', 
		pj))$raw %>% unlist %>% decompress_fst %>% unserialize, 
		error = function(e) NULL)
	gc()
	if(is.null(msFile)){
		# try to found it in mzXMLFiles repertory
		paths <- dbGetQuery(db, sprintf('select path, rawPath from sample 
			where sample == (select sample from project_sample 
				where project_sample == %s);', pj)) %>% 
			c %>% keep(file.exists)
		if(length(paths) == 0) stop('file is not found in database')
		else {
			msFile <- tryCatch(xcmsRaw(paths[1], mslevel=1, profstep=0), 
				error = function(e) NULL)
			gc()
			if(is.null(msFile) & length(paths) == 2){
				msFile <- tryCatch(xcmsRaw(paths[2], mslevel=1, profstep=0), 
					error = function(e) NULL)
				gc()
				if(is.null(msFile)) stop('file is not found in database')
			} else if(is.null(msFile)) stop('file is not found in database')
		}
	}
	msFile
}

plotEmptyChromato <- function(title = "TIC"){
	plot_ly(type='scatter', mode='markers') %>% 
		layout(title=sprintf('<b>%s</b>', title), 
			title=list(font = list(family='"Open Sans",verdana,arial,sans-serif', size=18)), 
			xaxis=list(title='Time', titlefont=list(family='"Open Sans",verdana,arial,sans-serif', size=18)), 
			yaxis=list(exponentformat='e', title=''), selectdirection="h", annotations=list(list(
				xref='paper', yref='paper', x=-0.05, y=1, xanchor='left', 
				yanchor='bottom', text='Intensity', showarrow=FALSE, 
				font=list(family='"Open Sans",verdana,arial,sans-serif', size=18)))) %>% 
		config(scrollZoom=TRUE, displaylogo=FALSE, 
			modeBarButtons=list(list(
				list(
					name='toImage', 
					title='Download plot as a png',
					icon=htmlwidgets::JS('Plotly.Icons.camera'),
					click=htmlwidgets::JS(sprintf("function(gd){Plotly.downloadImage(gd, {format:'png', width:1920, height:1080, filename:'%s'})}", title)))), 
				list('zoom2d', 'select2d', 'pan2d', 'autoScale2d', 'resetScale2d')))
}

plotChromato <- function(db, pj, title = "TIC", msFile = NULL){
	chromato <- plotEmptyChromato(title)
	
	if(is.null(msFile)) msFile <- loadRawFile(db, pj)
	
	points <- if(all(msFile@tic == 0)) data.frame(x = msFile@scantime/60, 
			y = rawEIC(msFile, mzrange = range(msFile@env$mz)))
		else data.frame(x=msFile@scantime/60, y=msFile@tic)
	 points %>% 
		dplyr::mutate(x = round(x,2)) %>% group_by(x) %>% 
		dplyr::summarise(y = median(y))
	rm(msFile)
	gc()
	
	#add the raw data
	add_trace(chromato, mode = "lines+markers", data = points, 
			x = ~x, y = ~y, line=list(color='red'), name=title, hoverinfo='text', 
			text = ~paste('Intensity: ', formatC(y, format="e"), '<br />Retention Time: ', 
				round(x, digits=2)), marker=list(opacity=1, size=1*10**-9))
}

plotEIC <- function(db, C, Cl, clusterIDs, adductName, machine, ppm = 0, xr = NULL){
	eic <- plotEmptyChromato(title = "EIC")
	
	if(is.null(xr)) return(eic)

	H <- 2*C+2-Cl
	formula <- paste('C', C, 'Cl', Cl, 'H', H, sep='')
	adduct <- adducts() %>% filter(adduct == adductName)
	ion_formula <- getIonFormula(formula, adduct, 0)$ion_formula
	theoric <- theoricClustersFunction(ion_formula, adduct$charge, 
		machine)[[1]] %>% arrange(desc(abundance)) %>% 
			mutate(tolMDa = mz * ppm * 10**-6,
				mzmin = mz - tolMDa,
				mzmax = mz + tolMDa, 
				theoricIso = ceiling(mz),
				theoricIso = theoricIso - theoricIso[1],
				theoricIso = case_when(
					theoricIso < 0 ~ paste0("A", theoricIso),
					theoricIso > 0 ~ paste0("A+", theoricIso),
					TRUE ~ "A")) %>% 
			distinct(theoricIso, .keep_all = TRUE)
	
	# get all theoretical m/z
	info <- dbGetQuery(db, sprintf('select * from cluster where cluster in (%s);', 
		paste(clusterIDs, collapse=', ')))
	
	features <- dbGetQuery(db, sprintf('select * from feature where cluster in (%s)', 
			paste(clusterIDs, collapse=', '))) %>% 
		mutate(rtmin = rtmin / 60, rtmax = rtmax / 60)
	
	data <- lapply(1:nrow(theoric), function(i) 
		rawEIC(xr, mzrange = as.double(theoric[i, c('mzmin', 'mzmax')])) %>% 
			as.data.frame %>% mutate(rt = xr@scantime / 60))
	rm(xr)
	gc()
	
	for(i in 1:length(data)){
		integratedScans <- do.call(c, lapply(which(features$iso == theoric[i, 'theoricIso']), 
			function(j) features[j, 'lmin']:features[j, 'lmax'])) %>% sort
		integrated <- data[[i]]
		nonIntegrated <- data[[i]]
		if(length(integratedScans) > 0){
			integrated[-integratedScans, 'intensity'] <- NA
			nonIntegrated[integratedScans, 'intensity'] <- NA
			
			eic <- eic %>% add_lines(data = integrated, x = ~rt, 
				y = ~intensity, legendgroup=toString(theoric[i, 'theoricIso']), 
				line = list(color=colors[i+1]),
				showlegend=FALSE, hoverinfo = "text", name = theoric[i, 'theoricIso'], 
				text = paste0("iso: ", theoric[i, 'theoricIso'], 
					"<br />mz: ", round(theoric[i, 'mz'], 5) , 
					"<br />rt: ", round(integrated$rt, 2), 
					"<br />intensity: ", prettyNum(round(integrated$intensity), 
						big.mark = " ")))
		} else integrated[, 'intensity'] <- NA
		
		eic <- eic %>% add_lines(data = nonIntegrated, x = ~rt, 
				y = ~intensity, legendgroup=toString(theoric[i, 'theoricIso']), 
				line = list(color='rgb(0,0, 0)', width=1, dash = 'dash'), 
				showlegend=TRUE, hoverinfo = "text", name = theoric[i, 'theoricIso'], 
				text = paste0("iso: ", theoric[i, 'theoricIso'], 
					"<br />mz: ", round(theoric[i, 'mz'], 5) , 
					"<br />rt: ", round(nonIntegrated$rt, 2), 
					"<br />intensity: ", prettyNum(round(nonIntegrated$intensity), 
						big.mark = " ")))
	}
	eic
}


plotEmptyMS <- function(){
	plot_ly(type='scatter', mode='markers') %>% 
		layout(title='<b>Mass Spectrum</b>', 
			title=list(font = list(family='"Open Sans",verdana,arial,sans-serif', size=18)), 
			xaxis=list(title='m/z', titlefont=list(family='"Open Sans",verdana,arial,sans-serif', size=18)), 
			yaxis=list(exponentformat='e', title=''), annotations=list(list(
				xref='paper', yref='paper', x=-0.05, y=1, xanchor='left', 
				yanchor='bottom', text='Intensity', showarrow=FALSE, 
				font=list(family='"Open Sans",verdana,arial,sans-serif', size=18)))) %>% 
		config(scrollZoom=TRUE, displaylogo=FALSE, 
			modeBarButtons=list(list(
				list(
					name='toImage', 
					title='Download plot as a png',
					icon=htmlwidgets::JS('Plotly.Icons.camera'),
					click=htmlwidgets::JS(sprintf("function(gd){Plotly.downloadImage(gd, {format:'png', width:1920, height:1080, filename:'MS'})}"))),
				list(
					name='resetView', 
					title='Reset legend', 
					icon=htmlwidgets::JS("Plotly.Icons.undo"),
					click=htmlwidgets::JS(sprintf("function(gd){ Plotly.restyle(gd, 'visible', true);}")))
				), 
				list('zoom2d', 'pan2d', 'autoScale2d', 'resetScale2d')))
}

plotMS <- function(db, C, Cl, clusterIDs, adductName, machine){
	massSpectrum <- plotEmptyMS()
	
	H <- 2*C+2-Cl
	formula <- paste('C', C, 'Cl', Cl, 'H', H, sep='')
	adduct <- adducts() %>% filter(adduct == adductName)
	ion_formula <- getIonFormula(formula, adduct, 0)$ion_formula
	theoric <- theoricClustersFunction(ion_formula, adduct$charge, 
		machine)[[1]] %>% arrange(desc(abundance)) %>% 
			mutate(mzR = ceiling(mz)) %>% distinct(mzR, .keep_all = TRUE) %>% 
			select(-mzR)
	
	info <- dbGetQuery(db, sprintf('select * from cluster where cluster in (%s);', 
		paste(clusterIDs, collapse=', ')))
	
	features <- dbGetQuery(db, sprintf('select * from feature where cluster in (%s)', 
		paste(clusterIDs, collapse=', ')))
	
	minMz <- min(c(features$mz, theoric$mz))
	maxMz <- max(c(features$mz, theoric$mz))
	
	for(clusterID in clusterIDs) massSpectrum <- massSpectrum %>% add_segments(
		data = features %>% filter(cluster == clusterID), x = ~mz, xend = ~mz, 
		y = 0, yend = ~abundance, name = paste('Cluster', clusterID), hoverinfo = "text", 
		text = ~paste0('Cluster :', clusterID, '<br />mz: ', round(mz, 5), 
			'<br />into : ', prettyNum(round(into), big.mark=' '), 
			'<br />rt :', round(rt / 60, 2)))
	massSpectrum <- massSpectrum %>% add_segments(data = theoric, 
		x = ~mz, xend = ~mz, y = 0, yend = ~-abundance, name = 'theoretic', hoverinfo = 'text', 
		text = ~paste0('Theoretic', '<br />mz: ', round(mz, 5), 
			'<br />abundance: ', round(abundance), '%'))
	massSpectrum %>% layout(xaxis = list(range = c(minMz - 1, maxMz + 1)))
}

