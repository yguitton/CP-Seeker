output$uiProject <- renderUI({
	choices <- if(nrow(projects()) == 0) c() else projects()
	pickerInput('project', 'project', choices=setNames(
		choices$project, choices$name))
})

observeEvent(input$projectCreate, {
	print('############################################################')
	print('######################### CREATE PROJECT ###################')
	print('############################################################')
	tryCatch({
	print(list(name=input$projectName, comment=input$projectComment, users=input$projectUsers))
	
	args <- c('projectName', 'projectComment')
	conditions <- c(!is.character(input$projectName) | input$projectName == '', !is.character(input$projectComment))
	messages <- c('Invalid project name', 'Invalid project comment')
	if(!inputsTest(args, conditions, messages)) custom_stop('invalid', 'Invalid args')
	
	projectName <- str_replace_all(input$projectName, "\"", "'")
	comment <- str_replace_all(input$projectComment, "\"", "'")
	
	query <- sprintf("insert into project (name, comment) values(\"%s\", \"%s\");", projectName, comment)
	print(query)
	dbSendQuery(db, query)
	
	actualize$projects <- TRUE
	toastr_success(paste('Project', projectName, 'created!'))
	}, invalid = function(i) print(i)
	, error = function(e){
		print('ERR projectCreate')
		print(e)
		sendSweetAlert(paste(e$message))
	})
	print('############################################################')
	print('######################### END CREATE PROJECT ###############')
	print('############################################################')
})

output$uiFileDBProject <- renderUI({
	choices <- if(nrow(projects()) == 0) data.frame(project = c(), name = c()) 
		else projects()
	div(style='text-align:center;', 
		pickerInput('fileDBProject', 'In project', choices=setNames(
			choices$project, choices$name), multiple=FALSE))
})

output$uiFilesDB <- renderUI({
	selected <- tryCatch({
		if(is.null(input$fileDBProject)) c()
		else if(input$fileDBProject == '') c()
		else project_samples() %>% filter(project == input$fileDBProject) %>% pull(sample)
	}, error = function(e){
		print(e)
		c()
	})
	pickerInput('filesDB', 'Add sample(s)', choices=samples()$sample, selected=selected, multiple=TRUE, 
		options=list(`live-search` = TRUE, `actions-box` = TRUE))
})

actualize$fileDBAdd <- c()
# add a sample in project where the sample is already provide in the database
observeEvent(input$fileDBAdd, {
	print('############################################################')
	print('######################### FILE ADD FROM DB #################')
	print('############################################################')
	print(list(project = input$fileDBProject, files = input$filesDB))
	
	tryCatch({
	args <- c("fileDBProject")
	conditions <- c(is.null(input$fileDBProject))
	messages <- c('No project selected', 'no files (de)selected')
	if(!inputsTest(args, conditions, messages)) custom_stop('invalid', 'Invalid args')
	conditions <- c(input$fileDBProject == "")
	if(!inputsTest(args, conditions, messages)) custom_stop('invalid', 'Invalid args')
	
	actualize$fileDBAdd <- c()
	fileAlreadyAdd <- project_samples() %>% filter(project == input$fileDBProject) %>% pull(sample) 
	toAdd <- input$filesDB[!input$filesDB %in% fileAlreadyAdd]
	toDelete <- fileAlreadyAdd[!fileAlreadyAdd %in% input$filesDB]
	
	print(list(fileAlreadyAdd=fileAlreadyAdd, toAdd=toAdd, toDelete=toDelete))
	if(length(toDelete) > 0){
		sapply(toDelete, function(x) deleteProject_sample(x))
		toastr_success(sprintf("Delete %s in project", paste(toDelete, collapse=', ')))
	} else print('no file to remove')
		
	if(length(toAdd) > 0){
		actualize$fileDBAdd <- toAdd
		showModal(modalDialog(title='', DT::dataTableOutput('sampleIDDBTable'), 
			footer=div(
				actionBttn('fileDBCancel', 'Cancel', style='stretch', color='warning'),
				actionBttn('fileDBAdd2', 'Valid', style='stretch', color='primary')
			)
		))
	}
	else {
		print('############################################################')
		print('######################### END FILE ADD FROM DB #############')
		print('############################################################')
	}
	actualize$project_samples <- TRUE
	}, invalid = function(i) print(i)
	, error = function(e){
		print(e)
		sendSweetAlert(paste(e$message))
	})
})

observeEvent(input$fileDBCancel, {
	removeModal()
})

output$sampleIDDBTable <- DT::renderDataTable({
	tryCatch({
	files <- actualize$fileDBAdd
	print(list(files=files))
	if(length(files) == 0) stop('no files to import')
	data.frame(file=files, label= files)
	}, error = function(e){
		print('ERR sampleIDDBTable')
		print(paste(e))
		toastr_error("Cannot display table with files", paste(e$message))
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
	$('#fileDBAdd2').on('click', function(){
		Shiny.onInputChange('sampleIDsDBValues', table.data().toArray());
		Shiny.onInputChange('sampleIDsDBValidBttn', Math.random());
	});
"))

observeEvent(input$sampleIDsDBValidBttn, {
	files <- input$sampleIDsDBValues[seq(1, length(
		input$sampleIDsDBValues), 2)]
	sampleIDs <- input$sampleIDsDBValues[seq(2, length(
		input$sampleIDsDBValues), 2)] %>% str_replace_all("\"", "'")
	print(list(project = input$fileDBProject, files = files, sampleIDs = sampleIDs, 
		polarity = input$filePolarity))
	sapply(1:length(files), function(i) 
		recordProjectSample(files[i], input$fileDBProject, sampleIDs[i]))
	removeModal()
	actualize$project_samples <- TRUE
	toastr_success(sprintf("Add %s in project", paste(files, collapse=', ')))
	
	print('############################################################')
	print('######################### END FILE ADD FROM DB #############')
	print('############################################################')
})

   
shinyFileChoose(input, 'filesImport', roots=getVolumes(), 
	filetypes=c('mzML', 'mzXML', 'cdf', 'CDF', 'RAW', 'd', 'YEP', 'BAF', 'FID', 'WIFF', 'MGF'), 
	defaultRoot=names(getVolumes()()[1]))
 
observeEvent(parseFilePaths(getVolumes()(), input$filesImport), {
	if(nrow(parseFilePaths(getVolumes()(), input$filesImport)) == 0) return()
	showModal(modalDialog(title='',
		column(width=3, pickerInput('fileProject', 'Add in project', 
			choices=if(nrow(projects()) == 0) c() else setNames(
				projects()$project, projects()$name))),
		column(style="padding-top: 2%;", width=8, offset=1, prettyRadioButtons("filePolarity", label = ("Choose polarity of file(s):"), 
			choices = list("negative" = "negative", "positive" = "positive"), 
			selected = "negative", icon=icon('check'), bigger=TRUE, status='primary', animation="jelly",
			inline=TRUE, fill=TRUE)),
		# tags$h4('Rename the file with the name of your choice'),
		DT::dataTableOutput('sampleIDTable'),
		footer=div(
			actionBttn('fileCancel', 'Cancel', style='stretch', color='warning'),
			actionBttn('fileAdd', 'Valid', style='stretch', color='primary')
		)
	))
})

observeEvent(input$fileCancel, {
	removeModal()
})

output$sampleIDTable <- DT::renderDataTable({
	tryCatch({
	files <- parseFilePaths(getVolumes()(), input$filesImport)
	print(list(files=files, polarity = input$filePolarity))
	data.frame(file=files$name, 
		label = paste(input$filePolarity %>% str_trunc(3, ellipsis=""),
			file_path_sans_ext(files$name)) %>% str_trunc(30, ellipsis=""))	
	}, error = function(e){
		print('ERR sampleIDTable')
		print(paste(e))
		toastr_error("Cannot display table with files", paste(e$message))
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

actualize$importationSuccess <- data.frame(file=c(), success=c())

observeEvent(input$fileCancel, {
	removeModal()
})

observeEvent(input$sampleIDsValidBttn, {
	print('############################################################')
	print('######################### IMPORT FILES #####################')
	print('############################################################')
	
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
			if(project_samples() %>% filter(
				project == input$fileProject & sample == sampleName) %>% 
					nrow > 0) success[i] <- 'file already in project'
			else if(sampleName %in% samples()$sample) success[i] <- recordProjectSample(
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
			DT::dataTableOutput('importationSuccessTable'), footer=modalButton('Close')
		))
	}, invalid = function(i){
		print(i)
		toastr_error(paste(i$message))
	}, error = function(e){
		closeSweetAlert(session)
		print(e)
		sendSweetAlert('Cannot import files', paste(e$message))
	})
	
	print('############################################################')
	print('######################### END IMPORT FILES #################')
	print('############################################################')
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
	fileName <- rawPath %>% basename %>% file_path_sans_ext
	path <- tempdir()
	
	# if it is a WIFF file it needs its corresponding WIFF.SCAN
	if(grepl('WIFF$', rawPath, ignore.case=TRUE)) if(!file.exists(paste0(
		rawPath, '.scan'))) return('missing corresponding wiff.scan file in same directory')
	
	# if it is CDF file cannot centroid it
	if(grepl('CDF$', rawPath, ignore.case=TRUE)) return(importation(
		rawPath, rawPath, project, sample, sampleID, if(polarity == 'negative') 0 else 1))
	
	print(paste('conversion of', fileName))
	# if it is a Water repertory, don't use the vendor algorithm
	algorithm <- if((grepl('raw$', rawPath, ignore.case=TRUE) & 
		file.info(rawPath)$isdir) | 
		grepl('mzXML$', rawPath, ignore.case=TRUE) | 
		grepl('mzML$', rawPath, ignore.case=TRUE)) 'cwt' else 'vendor'
	
	# call msConvert
	query <- sprintf("\"\"%s\" \"%s\" -o \"%s\" --outfile \"%s\" --mzXML --32 --zlib --filter \"peakPicking %s msLevel=1\" --filter \"polarity %s\"\"", 
		converter, rawPath, path, fileName, algorithm, polarity)
	print(query)
	shell(query)
	path <- file.path(path, paste(fileName, '.mzXML', sep=''))
	
	if(!file.exists(path)){
		print('conversion failed')
		if(grepl('mzXML$', rawPath, ignore.case=TRUE) | 
			grepl('mzML$', rawPath, ignore.case=TRUE)) return(importation(
				rawPath, rawPath, project, sample, sampleID, if(polarity == 'negative') 0 else 1))
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
	gc()
	if(length(msFile) == 0) return('no scans detected or file unreadable')
	
	# comment because it will consume too much RAM
	# check if it is centroided
	# centroided <- tryCatch({
		# which(isCentroided(msFile))
	# }, error = function(e){
		# print(e)
		# list()
	# })
	# gc()
	# if(length(centroided) == 0) return('file is not centroided, it will slower the application')
	
	# chek if polarity is the good one
	filePolarity <- if(grepl('cdf$', path, ignore.case=TRUE)) polarity
		else unique(polarity(msFile))
	if(is.na(filePolarity)) filePolarity <- polarity
	if(length(filePolarity) == 2) importation(
		cutMsFile(msFile, polarity), rawPath, project, sample, 
			sampleID, polarity, thermoFile)
	else if(polarity == 1 & filePolarity < 1) 'no positive scans detected'
	else if(polarity == 0 & filePolarity > 0) 'no negative scans detected'
	else recordFile(project, msFile, path, rawPath, sample, sampleID, polarity, thermoFile)
}

cutMsFile <- function(msFile, polarity){
	print(paste('cut', msFile))
	spectras <- if(polarity == 1) which(polarity(msFile) >= polarity)
		else which(polarity(msFile) <= polarity)
	path <- file.path(tempfile(), fileNames(msFile) %>% basename)
	writeMSData(msFile[spectras], path, copy=TRUE)
	rm(msFile)
	gc()
	path
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
	rm(msFile)
	gc()
	
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
			dbExecute(db, query)
			bl <- xcmsRaw(path, mslevel=1, profstep=0)
			gc()
			bl <- bl %>% serialize(NULL) %>% compress_fst(compression=100) %>% blob
			print(bl)
			if(length(bl[[1]]) >= 2*10**9){
				print('file too big for database')
				print('recording into mzXML repertory')
				new_path <- file.path('mzXMLFiles', 
					if(polarity == 0) "negative" else "positive", 
					basename(path))
				file.copy(path, new_path)
				dbExecute(db, sprintf('update sample set path = \"%s\" where 
					sample == \"%s\";', new_path, sample))
			} else dbExecute(db, sprintf('update sample set raw = :a where 
				sample == \"%s\";', sample), params = list(a = bl))
			rm(bl)
			gc()
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
		dbExecute(db, query)
		bl <- xcmsRaw(path, mslevel=1, profstep=0)
		gc()
		bl <- bl %>% serialize(NULL) %>% compress_fst(compression = 100) %>% blob
		print(bl)
		if(length(bl[[1]]) >= 2 * 10**9){
			print('file too big for database')
			print('recording into mzXML repertory')
				new_path <- file.path('mzXMLFiles', 
					if(polarity == 0) "negative" else "positive", 
					basename(path))
				file.copy(path, new_path)
				dbExecute(db, sprintf('update sample set path = \"%s\" where 
					sample == \"%s\";', new_path, sample))
		} else dbExecute(db, sprintf('update sample set raw = :a where 
			sample == \"%s\";', sample), params = list(a = bl))
		rm(bl)
		gc()
		return(recordProjectSample(sample, project, sampleID))
	} else paste('cannot insert in database')
}

recordProjectSample <- function(sample, project, sampleID){
	print('record project sample')
	query <- sprintf("insert into project_sample (project, sample, sampleID) values (%s, \"%s\", \"%s\" );", 
		project, sample, sampleID)
	print(query)
	dbExecute(db, query)
	'success'
}