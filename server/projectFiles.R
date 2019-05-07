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

output$sampleTable <- renderDataTable({	
	print('--------------- SAMPLE TABLE ----------------')
	data <- tryCatch({
		if(is.null(input$sample)) custom_stop('invalid', 'picker input not initialized yet')
		else if(input$sample ==  "") custom_stop('invalid', 'no file selected')
		samples() %>% filter(sample == input$sample) %>% select(
			-c(sample, path)) %>% t
	}, invalid = function(i){
		print(paste(i))
		matrix(, nrow=14, ncol=1) %>% data.frame
	}, error = function(e){
		print(paste(e))
		sendSweetAlert(e$message)
		matrix(, nrow=14, ncol=1) %>% data.frame
	})
	colnames(data) <- c('Details')
	rownames(data) <- c('raw path', 'instrument model', 'instrument manufacturer',
		'software name', 'software version', 'ion source', 'analyzer', 
		'detector type', 'method', 'resolution', 'agc target', 'maximum IT', 
		'number of scan range', 'scan range')
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

shinyFileChoose(input, 'fileImportRaw', roots=getVolumes(), 
	filetypes=c('T2D', 'd', 'YEP', 'BAF', 'FID',
		'TDF', 'LCD', 'RAW'), defaultRoot=names(getVolumes()()[1]))
actualize$rawPaths <- c()
actualize$sampleIDs <- c()
observeEvent(parseFilePaths(getVolumes()(), input$fileImportRaw), {
	tryCatch({
		rawPaths <- parseFilePaths(getVolumes()(), input$fileImportRaw)$datapath
		print(paste('raw path:', rawPaths, collapse=", "))
		if(length(rawPaths) == 0) custom_stop('invalid', 'no files selected')
		actualize$rawPaths <- gsub('\\\\', '/', rawPaths)
		actualize$sampleIDs <- actualize$rawPaths %>% basename %>% file_path_sans_ext
		
		showModal(modalDialog(easyClose=FALSE, title="Indicate sample IDs", 
			column(width=4, radioGroupButtons('sampleIDPolarity', label='Polarity',
				choices=c('positive', 'negative'), justified=TRUE)),
			column(width=4, pickerInput('sampleIDProject', 'add in project', projects())),
			column(width=4, style="padding-top: 5%;", 
				actionBttn('searchSampleIDs', 'guess sample IDs', style='bordered', 
					color='primary', size='sm', icon=icon('sync'))),
			dataTableOutput('sampleIDsTable'),
			footer=div(
				actionBttn('sampleIDCancel', 'Cancel', style='stretch', color='warning'),
				actionBttn('sampleIDsValid', 'Valid', style='stretch', color='primary')
			)
		))
	}, invalid = function(i){
		print(paste(i))
		actualize$rawPaths <- c()
		actualize$sampleIDs <- c()
	}, error = function(e){
		print(paste(e))
		sendSweetAlert(paste(e$message))
		actualize$rawPaths <- c()
		actualize$sampleIDs <- c()
	})
})

output$sampleIDsTable <- renderDataTable({
	tryCatch({
		if(length(actualize$rawPaths) == 0) custom_stop('invalid', 'no raw file selected')
		else data.frame(`filenames` = basename(actualize$rawPaths), 
			`sample_IDs` = actualize$sampleIDs)
	}, invalid = function(i){
		print(paste(i))
		data.frame(matrix(, ncol=2, dimnames=list(c(), c('file names', 'sample IDs'))))
	}, error = function(e){
		print(paste(e))
		sendSweetAlert(paste(e$message))
		data.frame(matrix(, ncol=2, dimnames=list(c(), c('file names', 'sample IDs'))))
	})
}, server=FALSE, escape=FALSE, rownames=FALSE, selection='none', options = list(dom='frtip', bFilter=FALSE, ordering=FALSE), callback=htmlwidgets::JS("
	table.on('dblclick', 'tbody td', function(){
		var colIndex = table.cell(this).index().column;
		if(colIndex == 1){
			var $input = $('<input type=\"text\">');
			var $this = $(this), value = table.cell(this).data(), html = $this.html();
			$input.val(value);
			$this.empty().append($input);
			$input.css('width', '100%').focus().on('change', function(){
				var valueNew = $input.val().replace('\"', ' ');
				if(valueNew != '') table.cell($this).data(valueNew);
				$input.remove();
			})
		}
	});
	$('#sampleIDsValid').on('click', function(){
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

observeEvent(input$sampleIDCancel, {
	actualize$rawPaths <- c()
	removeModal()
})

actualize$conversionSuccess <- data.frame(file=c(), success=c())		
observeEvent(input$sampleIDsValidBttn, {
	print('-------------------- IMPORTATION ---------------')
	tryCatch({
		sampleIDs <- input$sampleIDsValues[
			seq(2, length(input$sampleIDsValues) ,2)]
		print(list(rawPaths=actualize$rawPaths, sampleIDs=sampleIDs, 
			polarity=input$sampleIDPolarity, project=input$sampleIDProject))
		if(is.null(input$project)) custom_stop('invalid', 'no project selected')
		else if(input$project == '') custom_stop('invalid', 'no project selected')
		removeModal()
		rawPaths <- actualize$rawPaths
		res <- c()
		progressSweetAlert(session, 'pb', title="file importation/conversion", value=0, display_pct=TRUE)
		for(i in 1:length(rawPaths)){
			updateProgressBar(session, id='pb', title=paste("Raw file importation/conversion: ", 
				basename(rawPaths[i])), value=round((i-1)*100/length(rawPaths)))
			res[i] <- importRaw(rawPaths[i], sampleIDs[i], input$sampleIDPolarity, input$sampleIDProject)
		}
		print(res)
		actualize$samples <- TRUE
		actualize$project_samples <- TRUE
		actualize$conversionSuccess <- data.frame(file=sampleIDs, success=res)
		closeSweetAlert(session)
		showModal(modalDialog(title="Result of importation", easyClose=TRUE, 
			dataTableOutput('importationSucessTable'), footer=modalButton('Close')
		))
	}, invalid = function(i){
		print(paste(i))
		toastr_error(paste(i$message))
	}, error = function(e){
		closeSweetAlert(session)
		print(paste(e))
		sendSweetAlert(paste(e$message))
		actualize$rawPaths <- c()
		actualize$sampleIDs <- c()
		actualize$conversionSuccess <- data.frame(file=c(), success=c())
	})
	print('-------------------- END IMPORTATION ---------------')
})	

output$importationSucessTable <- renderDataTable({
	tryCatch({
		if(nrow(actualize$conversionSuccess) == 0) stop('none file import')
		res <- actualize$conversionSuccess
		res$success <- sapply(res$success, function(x) 
			if(x == "success") paste('<div style="background-color: #B3E2CD;">', 
				icon('check-circle'), x, '</div>') 
			else paste('<div style="background-color: #FDCDAC;">', 
				icon('exclamation-circle'), x, '</div>'))
		res
	}, error = function(e){
		print(paste(e))
		data.frame(matrix(, ncol=2, dimnames=list(c(), c('file', 'success'))))
	})
}, escape=FALSE ,rownames=FALSE, selection='none', class='compact nowrap',
options = list(dom='frtip', bFilter=FALSE, ordering=FALSE))

importRaw <- function(path, sampleID, polarity, project){
	tryCatch({
		outfile <- file.path(dirOutput, polarity, paste0(file_path_sans_ext(basename(path)), '.mzXML'))
		sample <- paste(polarity, path %>% basename %>% file_path_sans_ext)
		if(file.exists(outfile) & sample %in% samples()$sample) return(recordProjectSample(sample, project, sampleID))
		
		print(paste('conversion of', basename(path)))
		# if it is a WIFF file, search his corresponding WIFF.SCAN
		if(grepl('WIFF$', path, ignore.case=TRUE)) if(!file.exists(paste0(
			path, '.scan'))) stop(paste(
				paste0(basename(path), '.scan'), 
				'not find in the same directory as', path))
		# if it is a Water file, don't use the vendor algorithm
		algorithm <- if(grepl('raw$', path, ignore.case=TRUE) & 
			file.info(path)$isdir) 'cwt' else 'vendor'
		print(paste('algorithm used:', algorithm))
		
		# call msConvert
		query <- sprintf("\"\"%s\" \"%s\" -o \"%s\" --outfile \"%s\" --mzXML --32 --zlib --filter \"peakPicking %s msLevel=1\" --filter \"polarity %s\"\"", 
			converter, path, file.path(dirOutput, polarity), file_path_sans_ext(basename(outfile)), algorithm, polarity) 
		shell(query)
		
		if(!file.exists(outfile)) stop('conversion failed')
		msFile <- tryCatch({
			readMSData(outfile, msLevel=1, centroided=TRUE, mode='onDisk')
		}, error = function(e){
			print(paste(e))
			paste(basename(outfile), 'not contain', polarity, 'spectras')
		})
		if(class(msFile) != "OnDiskMSnExp") stop(msFile)
		else if(length(msFile) == 0) stop(paste(basename(outfile), 'has no scans??!!'))
		
		thermoFile <- if(grepl('raw$', path) & !file.info(path)$isdir){
			tmpFile <- file.path(tempdir(), path %>% basename %>% 
				file_path_sans_ext %>% paste0('.txt'))
			query <- sprintf("\"\"%s\" --scanTrailers \"%s\" > \"%s\"\"",
				thermo, path, tmpFile)
			shell(query)
			if(file.exists(tmpFile)) tmpFile else NULL
		} else NULL
	
		recordRaw(outfile, msFile, sample, sampleID, path, project, thermoFile)
	}, error=function(e){
		if(file.exists(outfile)){
			test <- FALSE
			while(test == FALSE) test <- unlink(outfile, force=TRUE)
		}
		print(paste(e))
		toastr_error(paste(e$message))
		paste(e$message)
	})
}

recordRaw <- function(outfile, msFile, sample, sampleID, rawPath, project, thermoFile=NULL){
	tryCatch({
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
		query <- sprintf('insert into sample (sample, path, rawPath, instrumentModel,
			instrumentManufacturer, softwareName, softwareVersion, ionSource,
			analyzer, detectorType, resolution, agcTarget, maximumIT, 
			numberOfScanRange, scanRange) values ("%s", "%s", "%s", "%s", "%s", 
			"%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s");', 
			sample, outfile, rawPath, instrumentModel, manufacturer, softwareName,
			softwareVersion, ionSource, analyzer, detectorType, resolution, 
			agcTarget, maximumIT, numberOfScanRange, scanRange)
		if(length(query) == 1){
			dbSendQuery(db, query)
			return(recordProjectSample(sample, project, sampleID))
		}
	}
	query <- sprintf('insert into sample (sample, path, rawPath, instrumentModel,
		instrumentManufacturer, sofwtareName, softwareVersion, ionSource,
		analyzer, detectorType) values ("%s", "%s", "%s", "%s", "%s", 
		"%s", "%s", "%s", "%s", "%s");', sample, outfile, rawPath, instrumentModel, 
		manufacturer, softwareName,	softwareVersion, ionSource, analyzer, detectorType)
	if(length(query) == 1){
		dbSendQuery(db, query)
		return(recordProjectSample(sample, project, sampleID))
	} else stop('minor_error', paste('cannot insert', sample, 'in database'))
	}, error = function(e){
		print(paste(e))
		toastr_error(paste(e$message))
		return(paste(e$message))
	})
}

recordProjectSample <- function(sample, project, sampleID){
	print('record project sample')
	tryCatch({
		query <- sprintf('insert into project_sample (sample, project, sampleID) 
			values ("%s", "%s", "%s");', sample, project, sampleID)
		if(length(query) == 1){
			dbSendQuery(db, query)
			return('success')
		} else stop(paste('cannot insert', sample, 'in project', project))
	}, error = function(e){
		print(paste(e))
		toastr_error(paste(e$message))
		return(paste(e$message))
	})
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


