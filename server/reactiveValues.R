# to force the actualization of reactive values   
actualize <- reactiveValues()
# keep some parameters on memory
param <- reactiveValues()


# create the adducts [M-Cl]- & [M-H-Cl]-
adducts <- reactive({
	actualize$adducts
	# select all except data column which contains blobs
	adducts <- dbGetQuery(db, 'select * from adduct;')
	actualize$adducts <- FALSE
	adducts
})
	
samples <- reactive({
	actualize$samples
	# select all except data column which contains blobs
	samples <- dbGetQuery(db, 'select sample, rawPath, instrumentModel, 
		instrumentManufacturer, softwareName, softwareVersion, ionSource, 
		analyzer, detectorType, resolution, agcTarget, maximumIT, 
		numberOfScanRange, scanRange, polarity from sample;')
	actualize$samples <- FALSE
	samples
})

projects <- reactive({
	actualize$projects
	projects <- dbGetQuery(db, 'select * from project;')
	actualize$projects <- FALSE
	projects
})

project_samples <- reactive({
	actualize$project_samples
	project_samples <- dbGetQuery(db, 'select * from project_sample;')
	actualize$project_samples <- FALSE
	project_samples
})

project_samples_adducts <- reactive({
	actualize$project_samples_adducts
	project_samples_adducts <- dbGetQuery(db, 'select * from project_sample_adduct;')
	actualize$project_samples_adducts <- FALSE
	project_samples_adducts
})

profiles <- reactive({
	actualize$profiles
	profiles <- dbGetQuery(db, 'select * from profile;')
	actualize$profiles <- FALSE
	profiles
})

deleteSamples <- function(samples = NULL){
	print('delete samples')
	if(length(samples) == 0) return()
	query <- sprintf('select project_sample from project_sample 
		where sample in (%s);', paste("\"", samples, "\"", collapse=', '))
	print(query)
	project_samples <- dbGetQuery(db, query)$project_sample
	deleteProjectSamples(project_samples)
	query <- sprintf('select path from sample where sample in (%s);', 
		paste("\"", samples, "\"", collapse=', '))
	print(query)
	paths <- dbGetQuery(db, query)$path 
	paths <- paths[which(str_detect(paths, "^mzXMLFiles") & 
		file.exists(paths))]
	file.remove(paths)
	query <- sprintf('delete from sample where sample in (%s);', 
		paste("\"", samples, "\"", collapse=', '))
	print(query)
	dbExecute(db, query)	
}

deleteProjects <- function(projects = NULL){
	print('delete projects')
	if(length(projects) == 0) return()
	query <- sprintf('select project_sample from project_sample where 
		project in (%s);', paste(projects, collapse=', '))
	print(query)
	project_samples <- dbGetQuery(db, query)$project_sample
	deleteProjectSamples(project_samples)
	query <- sprintf('delete from project where project in (%s);', 
		paste(projects, collapse=', '))
	print(query)
	dbExecute(db, query)
}

deleteProjectSamples <- function(project_samples = NULL){
	print('delete project samples')
	if(length(project_samples) == 0) return()
	deleteProject_samples_adducts(project_samples)
	query <- sprintf('delete from project_sample where 
		project_sample in (%s);', paste(project_samples, collapse = ', '))
	print(query)
	dbExecute(db, query)
}

deleteProject_samples_adducts <- function(project_samples = NULL, adducts = NULL){
	if(length(project_samples) == 0) return()
	print('delete project_sample_adduct')
	query <- if(length(adducts) == 0) sprintf('select project_sample_adduct 
			from project_sample_adduct where project_sample in (%s);', 
			paste(project_samples, collapse=', '))
		else sprintf('select project_sample_adduct from project_sample_adduct 
			where project_sample in (%s) and adduct in (%s);', 
			paste(project_samples, collapse=', ', sep=''), 
			paste('\"', adducts, '\"', collapse=', ', sep=''))
	print(query)
	ids <- dbGetQuery(db, query)$project_sample_adduct
	deleteClusters(ids)
	deleteProfiles(ids)
	query <- if(length(adducts) == 0) sprintf('delete from project_sample_adduct 
			where project_sample in (%s);', paste(project_samples, collapse=', '))
		else sprintf('delete from project_sample_adduct 
			where project_sample in (%s) and adduct in (%s);', 
			paste(project_samples, collapse=', ', sep=''), 
			paste('\"', adducts, '\"', collapse=', ', sep=''))
	print(query)
	dbExecute(db, query)
}

deleteClusters <- function(project_samples_adducts = NULL){
	if(length(project_samples_adducts) == 0) return()
	print('delete clusters')
	deleteFeatures(project_samples_adducts)
	query <- sprintf('delete from cluster where 
		project_sample_adduct in (%s);', paste(
			project_samples_adducts, collapse=', ', sep=''))
	print(query)
	dbExecute(db, query)
}

deleteFeatures <- function(project_samples_adducts = NULL){
	print('delete features')
	if(length(project_samples_adducts) == 0) return()
	query <- sprintf('delete from feature where cluster in (
		select cluster from cluster where project_sample_adduct in (%s));', 
		paste(project_samples_adducts, collapse=', '))
	print(query)
	dbExecute(db, query)
}

deleteProfiles <- function(project_samples_adducts = NULL){
	if(length(project_samples_adducts) == 0) return()
	print('deleteProfiles')
	deletePoints(project_samples_adducts)
	query <- sprintf('delete from profile where project_sample_adduct 
		in (%s);', paste(project_samples_adducts, collapse=', ', sep=''))
	print(query)
	dbExecute(db, query)
}

deletePoints <- function(project_samples_adducts = NULL){
	if(length(project_samples_adducts) == 0) return()
	print('delete points')
	query <- sprintf('delete from point where profile in (
		select profile from profile where project_sample_adduct in (%s));', 
		paste(project_samples_adducts, collapse=', ', sep=''))
	print(query)
	dbExecute(db, query)
}

recordProject <- function(name = NULL, comment = NULL){
	if(any(length(c(name, comment)) == 0)) stop('invalid arguments')
	query <- sprintf("insert into project (name, comment) 
		values(\"%s\", \"%s\");", name, comment)
	print(query)
	dbExecute(db, query)
}

recordProjectSample <- function(sample, project, sampleID){
	print('record project sample')
	query <- sprintf("insert into project_sample (project, sample, sampleID) values (%s, \"%s\", \"%s\" );", 
		project, sample, sampleID)
	print(query)
	dbExecute(db, query)
	'success'
}

recordProject_sample_adduct <- function(project_sample, adduct, machine){
	print('record project_sample_adduct')
	query <- sprintf("insert into project_sample_adduct (project_sample, adduct, machine) 
		values (%s, \"%s\", \"%s\");", project_sample, adduct, machine)
	dbExecute(db, query)
	"success"
}

output$downloadProject <- downloadHandler(
	filename = function(){
		if(!is.null(input$project)) paste(projects() %>% 
			filter(project == input$project) %>% pull(name), '.xlsx', sep='')
		else '*.xlsx'
	}, content = function(con){
	print('############################################################')
	print('######################### DOWNLOAD PROJECT #################')
	print('############################################################')
	print(list(project = input$project))
	wb <- createWorkbook()
	headerStyle <- createStyle(fgFill = "#4F81BD", halign = "CENTER", textDecoration = "Bold", border = "Bottom", fontColour = "white", wrapText=TRUE)
	cellStyle <- createStyle(halign="center", valign="center", wrapText=TRUE)
	tryCatch({
		if(!is.null(input$project)){
			data <- dbGetQuery(db, sprintf('select formula, C, Cl, adduct, sampleID, score from cluster 
				left join project_sample on project_sample.project_sample = cluster.project_sample 
				where project == %s;', input$project))
			# construct the final dataframe (formula, C, Cl, adduct, sample1, sample2, ...)
			tmp <- unique(data[, c('formula', 'C', 'Cl', 'adduct')])
			res <- tmp %>% cbind(do.call(cbind, 
				lapply(unique(data$sampleID), function(sampleid) 
					sapply(1:nrow(tmp), function(i) data %>% 
						filter(formula == tmp[i, 'formula'] & 
							adduct == tmp[i, 'adduct'] & 
							sampleID == sampleid) %>% 
						pull(score) %>% round))))
			colnames(res)[5:ncol(res)] <- unique(data$sampleID)
			addWorksheet(wb, "Resume")
			writeDataTable(wb, 1, res, headerStyle=headerStyle)
			addStyle(wb, sheet=1, cellStyle, rows=1:(nrow(res)+1), 
				cols=1:ncol(res), gridExpand=TRUE)
			setColWidths(wb, sheet=1, cols=1:ncol(res), widths=20)
			
			for(i in which(project_samples()$project == input$project)){
				addWorksheet(wb, project_samples()[i, 'sampleID'])
				data <- dbGetQuery(db, sprintf('select mz, rt, \"into\", maxo, abundance, 
					iso, sn, feature.cluster as cluster, formula, C, Cl, 
					score, rtMean, deviation, adduct, ppm, peakwidth1, peakwidth2, machine from feature 
					left join cluster on feature.cluster = cluster.cluster 
					left join project_sample on project_sample.project_sample = cluster.project_sample 
					where cluster.project_sample == %s;', project_samples()[i, 'project_sample'])) %>% 
					mutate(peakwidth = paste(peakwidth1, '-', peakwidth2), 
						`m/z` = round(mz, 5), rT = round(rt / 60, 2), area = round(into), 
						`max Int` = round(maxo), `relative abundance` = round(abundance), 
						`pattern score` = round(score), `rT mean` = round(rtMean / 60, 2), 
						`deviation (mDa)` = round(deviation, 1), isotopologue = iso, `s/n` = round(sn)) %>% 
					select(`m/z`, rT, area, `max Int`, `relative abundance`, 
						isotopologue, `s/n`, cluster, formula, C, Cl, 	
						`pattern score`, `rT mean`, `deviation (mDa)`, adduct, 
						ppm, peakwidth, machine)
				writeDataTable(wb, project_samples()[i, 'sampleID'], data, headerStyle=headerStyle)
				addStyle(wb, sheet=project_samples()[i, 'sampleID'], 
					cellStyle, rows=1:(nrow(data)+1), cols=1:ncol(data), gridExpand=TRUE)
				setColWidths(wb, sheet=project_samples()[i, 'sampleID'], 
					cols=1:(ncol(data) - 1), widths=20)
				setColWidths(wb, sheet=project_samples()[i, 'sampleID'], 
					cols=ncol(data), widths=36)
			}
		}
		saveWorkbook(wb, con, overwrite=TRUE)
	}, error = function(e){
		print(e)
		sendSweetAlert(e$message)
	})
	print('############################################################')
	print('######################### END DOWNLOAD PROJECT #############')
	print('############################################################')
	con
})
