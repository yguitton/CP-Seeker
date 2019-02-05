output$uiProject <- renderUI({
	db <- dbConnect(SQLite(), sqlitePath)
	projects <- dbGetQuery(db, 'select name as project from project;')
	choices <- if(nrow(projects) == 0) c() else projects$project
	dbDisconnect(db)
	pickerInput('project', 'project', choices=choices)
})

output$uiRawFiles <- renderUI({
	db <- dbConnect(SQLite(), sqlitePath)
	samples <- dbGetQuery(db, 'select sample from sample;')
	dbDisconnect(db)
	choices <- if(nrow(samples) > 0) samples$sample else c()
	pickerInput('sample', 'sample', choices=choices, options=list(`live-search` = TRUE))
})

updateOutput$sampleTable <- FALSE
output$sampleTable <- renderDataTable({	
	print('--------------- SAMPLE TABLE ----------------')
	db <- dbConnect(SQLite(), sqlitePath)
	data <- tryCatch({
		if(is.null(input$sample)) custom_stop('invalid', 'picker input not initialized yet')
		else if(input$sample ==  "") custom_stop('invalid', 'no file selected')
		query <- sprintf('select * from sample where sample == "%s";', input$sample)
		print(query)
		dbGetQuery(db, query) %>% t
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
	dbDisconnect(db)
	updateOutput$sampleTable <- FALSE
	print('--------------- END SAMPLE TABLE ----------------')
	data
}, selection='none', filter='none', options=list(paging=FALSE, bFilter=FALSE, ordering=FALSE, info=FALSE))

observeEvent(input$projectCreate, {
	print('------------------ CREATE PROJECT ---------------')
	print(list(name=input$projectName, comment=input$projectComment))
	db <- dbConnect(SQLite(), sqlitePath)
	dbSendQuery(db, 'begin transaction;')
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
		dbSendQuery(db, 'commit;')
		updatePickerInput(session, 'project', 'project', 
			choices=dbGetQuery(db, 'select name as project from project;')$project, 
			selected=input$projectName)
	}, invalid = function(i){
		dbSendQuery(db, 'rollback;')
	}, error = function(e){
		print(paste(e$message))
		sendSweetAlert(paste(e$message))
		dbSendQuery(db, 'rollback;')
	})
	dbDisconnect(db)
	print('------------------ END CREATE PROJECT ---------------')
})

importRaw <- function(path){
	print(path)
	db <- dbConnect(SQLite(), sqlitePath)
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
	dbDisconnect(db)
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
		updatePickerInput(session, 'sample', choices=
			dbGetQuery(db, 'select sample from sample;')$sample, 
			selected=basename(rawPaths[1]) %>% str_replace('\\.raw', ''))
		updateOutput$sampleTable <- TRUE
		closeSweetAlert(session)
		
	}, invalid = function(i){
		print(paste(i$message))
	}, error = function(e){
		print(paste(e$message))
		sendSweetAlert(paste(e$message))
	})
	print('-------------- END RAW FILES IMPORT --------------------')
})





