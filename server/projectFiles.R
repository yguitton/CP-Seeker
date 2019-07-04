output$uiProject <- renderUI({
	pickerInput('project', 'project', choices=projects())
})

output$uiRawFiles <- renderUI({
	choices <- tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project is not yet initialized')
		else if(input$project == '') custom_stop('invalid', 'no project')
		else project_samples() %>% filter(project == input$project) %>% 
			select(sampleID, sample)
	}, invalid = function(i){
		print(paste(i))
		data.frame(sample=c(), sampleID=c())
	}, error = function(e){
		print(paste(e))
		sendSweetAlert(paste(e$message))
		data.frame(sample=c(), sampleID=c())
	})
	pickerInput('sample', 'sample', choices=
		setNames(choices$sample, choices$sampleID), 
		options=list(`live-search` = TRUE))
})

output$sampleTable <- DT::renderDataTable({	
	print('--------------- SAMPLE TABLE ----------------')
	data <- tryCatch({
		if(is.null(input$sample)) custom_stop('invalid', 'picker input not initialized yet')
		else if(input$sample ==  "") custom_stop('invalid', 'no file selected')
		samples() %>% filter(sample == input$sample) %>% select(
			-c(sample, path)) %>% t
	}, invalid = function(i){
		print(paste(i))
		matrix(, nrow=15, ncol=1) %>% data.frame
	}, error = function(e){
		print(paste(e))
		sendSweetAlert(e$message)
		matrix(, nrow=15, ncol=1) %>% data.frame
	})
	colnames(data) <- c('Details')
	rownames(data) <- c('raw path', 'instrument model', 'instrument manufacturer',
		'software name', 'software version', 'ion source', 'analyzer', 
		'detector type', 'method', 'resolution', 'agc target', 'maximum IT', 
		'number of scan range', 'scan range', 'polarity')
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
		if(input$projectName %in% projects()) custom_stop('minor_error', 'a project with this name already exists')
		query <- sprintf('insert into project (name, comment) values ("%s", "%s");',
			input$projectName, input$projectComment)
		print(query)
		dbSendQuery(db, query)
		actualize$projects <- TRUE
	}, invalid = function(i){
	}, minor_error = function(e){
		print(paste(e))
		toastr_error(paste(e$message))
	}, error = function(e){
		print(paste(e))
		sendSweetAlert(paste(e$message))
	})
	print('------------------ END CREATE PROJECT ---------------')
})

shinyFileChoose(input, 'filesImport', roots=getVolumes(), 
	filetypes=c('mzML', 'mzXML', 'cdf', 'CDF', 'RAW', 'd', 'YEP', 'BAF', 'FID', 'WIFF', 'mzXML', 'mzML', 'MGF'), 
	defaultRoot=names(getVolumes()()[1]))

observeEvent(parseFilePaths(getVolumes()(), input$filesImport), {
	if(nrow(parseFilePaths(getVolumes()(), input$filesImport)) == 0) return()
	showModal(modalDialog(title='',
		column(width=3, pickerInput('fileProject', 'Add in project', 
			choices=projects())),
		column(style="padding-top: 2%;", width=5, offset=1, prettyRadioButtons("filePolarity", label = ("Choose polarity of file(s):"), 
			choices = list("negative" = "negative", "positive" = "positive"), 
			selected = "negative", icon=icon('check'), bigger=TRUE, status='primary', animation="jelly",
			inline=TRUE, fill=TRUE)),
		column(style="padding-top: 2%;", width=2, offset=1, actionBttn('searchSampleIDs', 'guess sample IDs', style='bordered', 
					color='primary', size='sm', icon=icon('sync'))),
		# tags$h4('Rename the file with the name of your choice'),
		dataTableOutput('sampleIDTable'),
		footer=div(
			actionBttn('fileCancel', 'Cancel', style='stretch', color='warning'),
			actionBttn('fileAdd', 'Valid', style='stretch', color='primary')
		)
	))
})


output$sampleIDTable <- DT::renderDataTable({
	tryCatch({
	files <- parseFilePaths(getVolumes()(), input$filesImport)
	print(list(files=files, polarity = input$filePolarity))
	data.frame(file=files$name, 
		label = paste(input$filePolarity %>% str_trunc(3, ellipsis=""), 
			file_path_sans_ext(files$name)) %>% str_trunc(30, ellipsis=""))	
	}, error = function(e){
		print(paste(e))
		data.frame(matrix(, ncol=2, dimnames=list(c(), c('file', 'label'))))
	})
}, selection='none', rownames=FALSE, options=list(dom='frtip', bFilter=FALSE, ordering=FALSE, paging=FALSE), 	
callback=htmlwidgets::JS("
	table.on('dblclick', 'tbody td', function(){
		var colIndex = table.cell(this).index().column;
		if(colIndex == 1){
			var $input = $('<input type=\"text\">');
			var $this = $(this), value = table.cell(this).data(), html = $this.html();
			$input.val(value);
			$this.empty().append($input);
			$input.css('width', '100%').focus().on('change', function(){
				var valueNew = $input.val().replace('\"', ' ');
				if(valueNew != ''){
					if(valueNew.split('').length < 31){
						table.cell($this).data(valueNew);
					} else {
						table.cell($this).data(value);
						toastr.error('sampleID cannot contain more than 30 characters')
					}
				}
				$input.remove();
			})
		}
	});
	$('#fileAdd').on('click', function(){
		Shiny.onInputChange('sampleIDsValues', table.data().toArray());
		Shiny.onInputChange('sampleIDsValidBttn', Math.random());
	});
"))

observeEvent(input$searchSampleIDs, {
	print('-------------------- SEARCH SAMPLE IDs ---------------')
	tryCatch({
		progressSweetAlert(session, id='pb', title='Search in metadata ...', 
			value=0, display_pct=TRUE)
		for(i in 1:length(actualize$rawPaths)){
			updateProgressBar(session, id='pb', value=i*100/length(actualize$rawPaths) %>% round)
			if(grepl('raw$', actualize$rawPaths[i]) & 
					!file.info(actualize$rawPaths[i])$isdir){
				tmpFile <- file.path(tempdir(), actualize$rawPaths[i] %>% 
					basename %>% file_path_sans_ext %>% paste0('.txt'))
				query <- sprintf("\"\"%s\" --scanTrailers \"%s\" > \"%s\"\"",
					thermo, actualize$rawPaths[i], tmpFile)
				shell(query)
				if(file.exists(tmpFile)){
					txt <- readLines(tmpFile)
					if(any(grepl('^SeqRowSampleID', txt, ignore.case=T))) actualize$sampleIDs[i] <- (txt[
						which(grepl('^SeqRowSampleID', txt, ignore.case=T))] %>% 
							str_split(':[[:space:]]') %>% unlist)[2]
				}
			}
		}
		closeSweetAlert(session)
	}, error = function(e){
		print(paste(e))
		closeSweetAlert(session)
		sendSweetAlert(paste(e$message))
		actualize$sampleIDs <- actualize$rawPaths %>% 
			basename %>% file_path_sans_ext
	})
	print('-------------------- END SEARCH SAMPLE IDs ---------------')
})

actualize$importationSuccess <- data.frame(file=c(), success=c())

observeEvent(input$fileCancel, {
	removeModal()
})

observeEvent(input$sampleIDsValidBttn, {
	print('-------------------- IMPORT FILES -----------------------')
	files <- parseFilePaths(getVolumes()(), input$filesImport)
	sampleIDs <- input$sampleIDsValues[seq(2, length(
		input$sampleIDsValues), 2)] %>% str_replace_all("\"", "'")
	print(list(project = input$fileProject, paths = files$datapath, sampleIDs = sampleIDs, 
		polarity = input$filePolarity))
	tryCatch({
		if(is.null(input$fileProject)) custom_stop('invalid', 'you must select a project')
		else if(input$fileProject == '') custom_stop('invalid', 'you must select a project')
		
		actualize$importationSuccess <- data.frame(file=c(), success=c())
		success <- c()
		progressSweetAlert(session, 'pb', title='file importation / conversion',
			value = 0, display_pct=TRUE)
		for(i in 1:length(files$datapath)){
			updateProgressBar(session, id='pb', title=
				paste('Importation of', files$name[i], 'as', sampleIDs[i]),
				value = round((i-1)*100/length(files$datapath)))
			print(paste('importation of', files$datapath[i]))
			
			# check if file already exists in database
			sampleName <- paste(input$filePolarity, file_path_sans_ext(files$name[i]))
			if(project_samples() %>% filter(project == input$fileProject & sample == sampleName) %>% 
				nrow > 0) success[i] <- 'file already in project'
			else if(sampleName %in% samples()$sample) success[i] <- addProject_sample(
				input$fileProject, sampleName, sampleIDs[i])
			else success[i] <- conversion(input$fileProject, files$datapath[i], 
				sampleName, sampleIDs[i], input$filePolarity)
			print('---')
		}
		actualize$samples <- TRUE
		actualize$project_samples <- TRUE
		actualize$importationSuccess <- data.frame(
			file = files$name, success = success)
		print(success)
		closeSweetAlert(session)
		showModal(modalDialog(title = 'Result of importation', easyClose=TRUE,
			dataTableOutput('importationSuccessTable'), footer=modalButton('Close')
		))
	}, invalid = function(i){
		print(i)
		toastr_error(paste(i$message))
	}, error = function(e){
		closeSweetAlert(session)
		print(e)
		sendSweetAlert(paste(e$message))
	})
	print('-------------------- END IMPORT FILES -----------------------')
})

output$importationSuccessTable <- DT::renderDataTable({
	if(nrow(actualize$importationSuccess) == 0) stop('none file(s) imported')
	data <- actualize$importationSuccess
	data$success <- sapply(data$success, function(x) 
		if(x == "success") paste("<div style=\"background-color: #B3E2CD;\">", 
			icon("check-circle"), x, "</div>")
		else paste("<div style=\"background-color: #FDCDAC;\">", 
			icon("exclamation-circle"), x, "</div>")
	)
	data
}, escape=FALSE, rownames=FALSE, selection='none', class='compact nowrap', 
options=list(dom='frtip', bFilter=FALSE, ordering=FALSE))

conversion <- function(project, rawPath, sample, sampleID, polarity){
	path <- file.path(dirOutput, polarity, rawPath %>% basename %>% 
		file_path_sans_ext %>% paste0('.mzXML'))
	
	# if it is a WIFF file it needs its corresponding WIFF.SCAN
	if(grepl('WIFF$', rawPath, ignore.case=TRUE)) if(!file.exists(paste0(
		rawPath, '.scan'))) return('missing corresponding wiff.scan file in same directory')
	
	# if it is CDF file cannot centroid it
	if(grepl('CDF$', rawPath, ignore.case=TRUE)){
		file.copy(rawPath, file.path(dirOutput, polarity, rawPath %>% basename))
		return(importation(file.path(dirOutput, polarity, rawPath %>% basename), 
			rawPath, project, sample, sampleID, if(polarity == 'negative') 0 else 1))
	}
	
	print(paste('conversion of', basename(rawPath)))
	# if it is a Water repertory, don't use the vendor algorithm
	algorithm <- if((grepl('raw$', rawPath, ignore.case=TRUE) & 
		file.info(rawPath)$isdir) | 
		grepl('mzXML$', rawPath, ignore.case=TRUE) | 
		grepl('mzML$', rawPath, ignore.case=TRUE)) 'cwt' else 'vendor'
	
	# call msConvert
	query <- sprintf("\"\"%s\" \"%s\" -o \"%s\" --outfile \"%s\" --mzXML --32 --zlib --filter \"peakPicking %s msLevel=1\" --filter \"polarity %s\"\"", 
		converter, rawPath, file.path(dirOutput, polarity), rawPath %>% basename %>% file_path_sans_ext, algorithm, polarity) 
	shell(query)
	
	if(!file.exists(path)){
		print('conversion failed')
		if(grepl('mzXML$', rawPath, ignore.case=TRUE) | 
			grepl('mzML$', rawPath, ignore.case=TRUE)){
				file.copy(rawPath, file.path(dirOutput, polarity, rawPath %>% basename))
				return(importation(file.path(dirOutput, polarity, rawPath %>% basename), 
					rawPath, project, sample, sampleID, if(polarity == 'negative') 0 else 1))
		}
		else return('missing windows update, cannot convert')
	}
	
	thermoFile <- if(grepl('raw$', rawPath, ignore.case=TRUE) & 
		!file.info(rawPath)$isdir){
			tmpFile <- file.path(tempdir(), rawPath %>% basename %>% 
				file_path_sans_ext %>% paste0('.txt'))
			query <- sprintf("\"\"%s\" --scanTrailers \"%s\" > \"%s\"\"",
				thermo, rawPath, tmpFile)
			shell(query)
			if(file.exists(tmpFile)) tmpFile else NULL
		} else NULL
		
	importation(path, rawPath, project, sample, sampleID, 
		if(polarity == 'negative') 0 else 1, thermoFile)
}

importation <- function(path, rawPath, project, sample, sampleID, polarity, thermoFile=NULL){
	print(paste('importation of', path))
	
	# check if it can be read
	msFile <- tryCatch({
		readMSData(path, msLevel=1, mode='onDisk')
	}, error = function(e){
		print(e)
		list()
	})
	if(length(msFile) == 0) return('no scans detected or file unreadable')
	
	# check if it is centroided
	centroided <- tryCatch({
		which(isCentroided(msFile))
	}, error = function(e){
		print(e)
		list()
	})
	if(length(centroided) == 0) return('file is not centroided, it will slower the application')
	
	# chek if polarity is the good one
	filePolarity <- if(grepl('cdf$', path, ignore.case=TRUE)) polarity
		else unique(polarity(msFile))
	if(length(polarity) == 2) cutMsFile(msFile, path, polarity)
	else if(is.na(polarity)) return(recordFile(project, msFile, path, rawPath, 
		sample, sampleID, polarity, thermoFile))
	else if(filePolarity != polarity) return('not the same polarity in file than selected')

	recordFile(project, msFile, path, rawPath, sample, sampleID, polarity, thermoFile)
}

cutMsFile <- function(msFile, path, polarity){
	print(paste('cut', msFile))
	spectras <- which(polarity(msFile) == polarity)
	writeMSData(msFile[spectras], path, copy=TRUE)
}

recordFile <- function(project, msFile, path, rawPath, sample, sampleID, polarity, thermoFile=NULL){
	print(paste('record', sample))
	infos <- msFile@experimentData
	instrumentModel <- if(length(infos@instrumentModel) == 0) '' else infos@instrumentModel
	manufacturer <- if(length(infos@instrumentManufacturer) == 0) '' else infos@instrumentManufacturer
	softwareName <- if(length(infos@softwareName) == 0) '' else infos@softwareName
	softwareVersion <- if(length(infos@softwareVersion) == 0) '' else infos@softwareVersion
	ionSource <- if(length(infos@ionSource) == 0) '' else infos@ionSource
	analyzer <- if(length(infos@analyser) == 0) '' else infos@analyser
	detectorType <- if(length(infos@detectorType) == 0) '' else infos@detectorType
	if(!is.null(thermoFile)){
		txt <- readLines(thermoFile)
		resolution <- if(any(grepl('^Resolution', txt))) txt[which(
				grepl('^Resolution', txt))] %>% 
				str_extract('[[:digit:]]+[[[:punct:]]{1}[[:alpha:]]{1}][[:digit:]]*')
			else ''
		agcTarget <- if(any(grepl('^AGC target', txt))) txt[which(
				grepl('^AGC target', txt))] %>% 
				str_extract('[[:digit:]]+[[[:punct:]]{1}[[:alpha:]]{1}][[:digit:]]')
			else ''
		maximumIT <- if(any(grepl('^Maximum IT', txt))) txt[which(
				grepl('^Maximum IT', txt))] %>% 
				str_extract('[[:digit:]]+\\s[[:alpha:]]+')
			else ''
		numberOfScanRange <- if(any(grepl('^Number of scan ranges', txt))) txt[which(
				grepl('^Number of scan ranges', txt))] %>% 
				str_extract('[[:digit:]]+')
			else ''
		scanRange <- if(any(grepl('^Scan range', txt))) txt[which(
				grepl('^Scan range', txt))] %>% 
				str_extract('[[:digit:]]+\\sto\\s[[:digit:]]+\\sm/z')
			else ''
		query <- sprintf('insert into sample (sample, path, rawPath, polarity, instrumentModel,
			instrumentManufacturer, softwareName, softwareVersion, ionSource,
			analyzer, detectorType, resolution, agcTarget, maximumIT, 
			numberOfScanRange, scanRange) values ("%s", "%s", "%s", "%s", "%s", "%s", 
			"%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s");', 
			sample, path, rawPath, if(polarity == 0) "negative" else "positive", 
			instrumentModel, manufacturer, softwareName,
			softwareVersion, ionSource, analyzer, detectorType, resolution, 
			agcTarget, maximumIT, numberOfScanRange, scanRange)
		query <- str_replace_all(query, "\"", "'")
		print(query)
		if(length(query) == 1){
			dbSendQuery(db, query)
			return(recordProjectSample(sample, project, sampleID))
		}
	}
	query <- sprintf('insert into sample (sample, path, rawPath, polarity, instrumentModel,
		instrumentManufacturer, softwareName, softwareVersion, ionSource,
		analyzer, detectorType) values ("%s", "%s", "%s", "%s", "%s", "%s", 
		"%s", "%s", "%s", "%s", "%s");', sample, path, rawPath, if(polarity == 0) "negative" else "positive",
		instrumentModel, manufacturer, softwareName,	softwareVersion, ionSource, analyzer, detectorType)
	query <- str_replace_all(query, "\"", "'")
	print(query)
	if(length(query) == 1){
		dbSendQuery(db, query)
		return(recordProjectSample(sample, project, sampleID))
	} else paste('cannot insert in database')
}

recordProjectSample <- function(sample, project, sampleID){
	print('record project sample')
	query <- sprintf("insert into project_sample (project, sample, sampleID) values (\"%s\", \"%s\", \"%s\" );", 
		project, sample, sampleID)
	print(query)
	dbSendQuery(db, query)
	'success'
}

# output$uiStandardTable <- renderDataTable({
	# tryCatch({
	# db <- dbConnect(SQLite(), sqlitePath)
	# data <- dbGetQuery(db, 'select * from standards')
	# dbDisonnect(db)
	# data
	# })
# }, rownames=FALSE, selection=list(mode='multiple', target='row'), extensions=c('Buttons'), options=list(dom='Bfrtip',
# buttons=JS("
	# [
		# {
			# text: 'new', 
			# action: function(e, dt, node, config){
				# Shiny.onInputChange('standardTable_add', Math.random());
			# }
		# },
		# {
			# text: 'remove', 
			# action: function(e, dt, node, config){
				# Shiny.onInputChange('standardTable_remove', {rand: Math.random(),
					# ids: table.rows('.selected').data().map(x => x[0])});
				# table.rows('.selected').remove();
			# }
		# }
	# ]
# "), callback=JS("table.column(':contains(standard)').visible(false);"))

# observeEvent(input$standardTable_add, {
	# showModal(addStandardModal())
# })

# addStandardModal <- function(){
	# modalDialog(title='Add new standard in database', 
		# textInput('SDName', 'name', '', placeholder='enter the name'),
		# textInput('SDFormula', 'formula', '', placeholder='enter the formula'),
		# footer=actionBttn('standardAdd', 'add', style='stretch', color='primary')
	# )
# }

# observeEvent(input$standardAdd, {
	# tryCatch({
		
	# })
# })

# observeEvent(input$standardTable_remove, {
	# tryCatch({
	# ids <- input$standardTable_remove$ids
	# if(length(ids) == 0) custom_stop('minor_error', 'You need to select at least one row')
	# db <- dbConnect(SQLite(), sqlitePath)
	# dbGetQuery(db, sprintf('delete from standard where standard in (%s);', 
		# paste(ids, collapse=', ')))
	# dbDisconnect(db)
	# }, minor_error = function(e){
		# print(e$message)
		# toastr_error(e$message)
	# })
# })


