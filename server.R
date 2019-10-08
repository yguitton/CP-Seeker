options(show.error.messages = TRUE)
shinyServer(function(input, output, session) {

#to close the connection
session$onSessionEnded(function() {
	tryCatch({
		# dbExecute(db, 'vacuum;')
		dbExecute(db, 'pragma wal_checkpoint(truncate);')
	}, error = function(e){
		dbExecute(db, 'pragma wal_checkpoint(passive);')
	})
	dbDisconnect(db)
	rm(list = ls())
	gc()
	stopApp()
	# comment line above because it's still in developement
	# q('no')
})

observeEvent(input$tabs, {
	gc()
	print('                                                            ')
	print('############################################################')
	paste0('TAB ######################### ', input$tabs, ' ############################') %>% 
		str_trunc(60) %>% print
	print('############################################################')
})

values <- reactiveValues()
actualize <- reactiveValues()

colors <- c(brewer.pal(9, "Set1"), brewer.pal(8, 'Set2'))

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

source('server/func.R', local=TRUE)$value

source('server/loadProfile.R', local=TRUE)$value

source('server/profils.R', local=TRUE)$value

source('server/tetrahedrization.R', local=TRUE)$value

source('server/projectFiles.R', local=TRUE)$value

source('server/target.R', local=TRUE)$value

source('server/details.R', local=TRUE)$value

})