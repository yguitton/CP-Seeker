options(show.error.messages = TRUE)
specialChars <- "[\\:*?\"<>|]"
shinyServer(function(input, output, session) {
colors <- c(brewer.pal(9, "Set1"), brewer.pal(8, 'Set2'))

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

source(file.path("server", "initialize.R"), local=TRUE)$value

source(file.path("server", "reactiveValues.R"), local=TRUE)$value

output$uiProject <- renderUI({
	choices <- if(nrow(projects()) == 0) c() else projects()
	pickerInput('project', label = NULL, inline = TRUE, choices=setNames(
		choices$project, choices$name))
})

output$download <- downloadHandler(
	filename = function() paste(projects() %>% 
		filter(project == input$project) %>% pull(name), 
		'.xlsx', sep=''),
	content = function(xlsxFile){
		print('############################################################')
		print('######################### DOWNLOAD #########################')
		print('############################################################')
		wb <- createWorkbook()
		
		progressSweetAlert(session, 'pb', title='Generation of xlsx file',
			value = 0, display_pct=TRUE)
	
		tryCatch({
			headerStyle <- createStyle(fgFill = "#4F81BD", halign = "CENTER", textDecoration = "Bold", border = "Bottom", fontColour = "white", wrapText=TRUE)
			cellStyle <- createStyle(halign="center", valign="center", wrapText=TRUE)
			
			sheets <- project_samples_adducts() %>% left_join(project_samples(), 
				by = 'project_sample') %>% select(sampleID, adduct, project_sample_adduct)
			for(i in 1:nrow(sheets)){
				sheetName <- paste(sheets[i, 'sampleID'], sheets[i, 'adduct'])
				updateProgressBar(session, id='pb', title=paste('Generate xlsx data of', sheetName),
					value=(i-1) * 100 / nrow(sheets))
				addWorksheet(wb, sheetName = sheetName, gridLines=TRUE)
				
				data <- dbGetQuery(db, sprintf('select cluster.cluster as cluster, formula, ion_formula, C, Cl, 
					score, rtMean, deviation, mz, rt, \"into\", maxo, sn as \"s/n\", iso as isotopologue, 
					ppm, peakwidth1, peakwidth2 
					from cluster left join feature on cluster.cluster = feature.cluster where 
					project_sample_adduct == %s;', sheets[i, 'project_sample_adduct'])) %>% 
					mutate(peakwidth = paste(peakwidth1, peakwidth2, sep='-'), 
						rtMean = round(rtMean / 60, 2), mz = round(mz, 5), 
						rt = round(rt / 60, 2), into = round(into), 
						maxo = round(maxo), `s/n` = round(`s/n`)) %>% 
					select(-c(peakwidth1, peakwidth2))
									
					writeDataTable(wb = wb, sheet = sheetName, x = data, headerStyle=headerStyle)
					addStyle(wb, sheet = sheetName, cellStyle, rows=1:(nrow(data)+1), 
						cols=1:ncol(data), stack=TRUE, gridExpand=TRUE)
					setColWidths(wb, sheet = sheetName, cols=1:ncol(data), widths=10)
					setColWidths(wb, sheet = sheetName, cols=c(2, 3), widths=15)
			}
		}, error = function(e){
			print(e)
			toastr_error('Cannot make excel file', paste(e$message))
		})
		updateProgressBar(session, id='pb', title='Save xlsx file', value=100)
		saveWorkbook(wb, xlsxFile, overwrite=TRUE)
		closeSweetAlert(session)
		print('############################################################')
		print('######################### END DOWNLOAD #####################')
		print('############################################################')
		return(xlsxFile)
	}
)

source('server/func.R', local=TRUE)$value

source("server/project.R", local=TRUE)$value

source("server/file.R", local=TRUE)$value

source('server/delete.R', local = TRUE)$value

source('server/plots.R', local=TRUE)$value

source('server/chemFunc.R', local=TRUE)$value

source('server/target.R', local=TRUE)$value

source('server/details.R', local=TRUE)$value

source('server/tetras.R', local = TRUE)$value

source('server/tetrahedrization.R', local = TRUE)$value

})