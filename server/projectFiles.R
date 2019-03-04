output$uiProject <- renderUI({
	pickerInput('project', 'project', choices=projects())
})

output$uiRawFiles <- renderUI({
	pickerInput('sample', 'sample', choices=samples()$sample, options=list(`live-search` = TRUE))
})

output$sampleTable <- renderDataTable({	
	print('--------------- SAMPLE TABLE ----------------')
	data <- tryCatch({
		if(is.null(input$sample)) custom_stop('invalid', 'picker input not initialized yet')
		else if(input$sample ==  "") custom_stop('invalid', 'no file selected')
		samples()[which(samples()$sample == input$sample), ] %>% t
	}, invalid = function(i){
		print(paste(i$message))
		matrix(, nrow=7, ncol=1) %>% data.frame
	}, error = function(e){
		print(paste(e$message))
		sendSweetAlert(e$message)
		matrix(, nrow=7, ncol=1) %>% data.frame
	})
	colnames(data) <- c('Details')
	rownames(data) <- c('sample', 'path', 'raw path', 'ionization mode', 
		'mass analyzer', 'FT resolution', 'AGC target')
	print('--------------- END SAMPLE TABLE ----------------')
	data
}, selection='none', filter='none', options=list(paging=FALSE, bFilter=FALSE, ordering=FALSE, info=FALSE))

observeEvent(input$projectCreate, {
	print('------------------ CREATE PROJECT ---------------')
	print(list(name=input$projectName, comment=input$projectComment))
	tryCatch({
		inputs <- c('projectName')
		conditions <- c(length(input$projectName) == 0)
		messages <- c('You have to specify the name of the project')
		test <- inputsTest(inputs, conditions, messages)
		if(!test) custom_stop('invalid', 'invalid params')
		query <- sprintf('insert into project (name, comment) values ("%s", "%s");',
			input$projectName, input$projectComment)
		print(query)
		dbSendQuery(db, query)
		actualize$projects <- TRUE
	}, invalid = function(i){
	}, error = function(e){
		print(paste(e$message))
		sendSweetAlert(paste(e$message))
	})
	print('------------------ END CREATE PROJECT ---------------')
})

importRaw <- function(path){
	print(path)
	tryCatch({
		if(nrow(dbGetQuery(db, sprintf('select * from sample where sample == "%s";',
			basename(path) %>% str_replace('\\.raw', '')))) > 0) custom_stop('invalid', 'file already imported')
		raw <- read.raw(path, rawDiag=FALSE)
		ionizationMode <- unique(raw$IonizationMode)
		if(length(ionizationMode) > 1){
			print(paste('find multiple ionization mode', ionizationMode))
			ionizationMode <- ionizationMode[1]
		}
		massAnalyzer <- unique(raw$MassAnalyzer)
		if(length(massAnalyzer) > 1){
			print(paste('find multiple mass analyzer', massAnalyzer))
			massAnalyzer <- massAnalyzer[1]
		}
		ftResolution <- unique(raw$FTResolution)
		if(length(ftResolution) > 1){
			print(paste('find multiple ft resolution', ftResolution))
			ftResolution <- ftResolution[1]
		}
		AGCTarget <- unique(raw$AGCTarget)
		if(length(AGCTarget) > 1){
			print(paste('find agc target', AGCTarget))
			AGCTarget <- AGCTarget[1]
		}
		file.copy(path, file.path(rawPath, basename(path)), overwrite=TRUE)
		query <- sprintf('insert into sample (sample, path, rawPath, ionization_mode, massAnalyzer, 
			FTResolution, AGCTarget) values ("%s", "%s", "%s", "%s", "%s", %s, %s);',
			basename(path) %>% str_replace('\\.raw', ''), file.path(rawPath, basename(path)),
			path, ionizationMode, massAnalyzer, ftResolution, AGCTarget)
		print(query)
		dbSendQuery(db, query)
		toastr_success(paste(basename(path), 'import'))
	}, invalid = function(i){
		print(paste(i$message))
		toastr_warning(paste(i$message))
	}, error=function(e){
		print(paste(e$message))
		toastr_error(paste(basename(file), ' : ', e$message))
	})
}

observeEvent(input$rawFilesImport, {
	print('-------------- RAW FILES IMPORT --------------------')
	tryCatch({
		rawPaths <- choose.files(caption='Select raw files (thermo)', multi=TRUE,
			filter=matrix(c('.raw', '*.raw'), 1, 1, byrow=TRUE))
		print(paste('raw path:', rawPaths))
		if(length(rawPaths) == 0) custom_stop('invalid', 'no files selected')
		rawPaths <- gsub('\\\\', '/', rawPaths)
		progressSweetAlert(session, 'pb', title="Raw file importation", value=0, display_pct=TRUE)
		for(i in 1:length(rawPaths)){
			updateProgressBar(session, id='pb', title=paste("Raw file importation: ", 
				basename(rawPaths[i])), value=round((i-1)*100/length(rawPaths)))
			importRaw(rawPaths[i])
		}
		updateProgressBar(session, id='pb', title=paste("Raw file importation: ", 
				basename(rawPaths[i])), value=round((i)*100/length(rawPaths)))
		actualize$samples <- TRUE
		closeSweetAlert(session)
		
	}, invalid = function(i){
		print(paste(i$message))
	}, error = function(e){
		print(paste(e$message))
		sendSweetAlert(paste(e$message))
	})
	print('-------------- END RAW FILES IMPORT --------------------')
})





