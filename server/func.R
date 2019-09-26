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
	
	# get params IDs
	query <- sprintf('select param from param 
		where project_sample in (%s);', paste(project_samples, collapse=', '))
	print(query)
	params <- dbGetQuery(db, query)$param
	if(length(params) == 0) print('no data to delete')
	else {
		query <- sprintf('delete from param where param in (%s);', 
			paste(params, collapse=', '))
		print(query)
		dbExecute(db, query)
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
calculateScore <- function(cluster, theoric, tolMz, tolI){
	if(nrow(cluster) == 0 | nrow(theoric) == 0 | 
		!is.numeric(tolMz) | !is.numeric(tolI)) return(0)
	else if(tolMz < 0 | tolI < 0) return(0)
	cluster <- cluster %>% arrange(desc(abundance)) %>% 
		mutate(weight = abundance / sum(abundance), id = 1:n())
	theoric <- theoric %>% arrange(desc(abundance)) %>% 
		mutate(weight = abundance / sum(abundance), id = 1:n())
	round(50 * (2 - calculateScore2(cluster, theoric, tolMz, tolI)), digits=2)
}
calculateScore2 <- function(cluster, theoric, tolMz, tolI){
	if(nrow(cluster) == 0 | nrow(theoric) == 0) return(
		sum(theoric$weight) + sum(cluster$weight))
	res <- calculateScore3(cluster, theoric[1, ], tolMz, tolI)
	cluster <- res$cluster
	res$score + calculateScore2(cluster, theoric[-1, ], tolMz, tolI) # cumulative sum
}
calculateScore3 <- function(cluster, theoricFeature, tolMz, tolI){
	# first search the matched observed feature
	observedFeature <- cluster %>% filter(between(mz, 
		theoricFeature$mz - tolMz, theoricFeature$mz + tolMz)) %>% 
		mutate(deviation = ((theoricFeature$abundance - abundance) %>% abs) / tolI) %>% 
		filter(deviation < 1) %>% top_n(1, abundance)
	# return the minimal score (0: perfect match, 1: wrong match)
	# remove the matched feature for avoid an other assignation with a feature
	if(nrow(observedFeature) == 0) list(
		score = theoricFeature$weight, 
		cluster = cluster
	) else list(
		score = observedFeature$deviation * (theoricFeature$weight + 
			observedFeature$weight),
		cluster = cluster %>% filter(id != observedFeature$id)
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

rawEIC <- function(msFile, mzrange = c(), rtrange = c(), scanrange = c()){
	xcms::rawEIC(msFile, mzrange = mzrange, rtrange = rtrange, 
		scanrange = scanrange) %>% data.frame %>% cbind(
			scan = 1:n())
}

arrangeEics <- function(eic, msFile){
	eic <- eic %>% as.data.frame
	eic$scan <- msFile@scantime[eic$scan] / 60
	colnames(eic) <- c('x', 'y')
	eic %>% dplyr::mutate(x = round(x,2)) %>% group_by(x) %>% 
		dplyr::summarise(y = median(y))
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
			row = 1:n(), formula = new_formula) %>% 
		cbind(charge = charges + adduct$charge) %>% 
		filter(!warning) %>% mutate(
			ion_formula = multiform(formula, adduct$multi) %>% 
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
