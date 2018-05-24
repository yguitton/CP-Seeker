output$uiFileProjectmzXML <- renderUI({
	tagList(
		tags$script(HTML("
			var content = \"<input type='text' class='bss-input' onKeyDown='event.stopPropagation();' onKeyPress='addSelectInpKeyPress(this,event)' onClick='event.stopPropagation()' placeholder='Add item'> <span class='glyphicon glyphicon-plus addnewicon' onClick='addSelectItem(this,event,1);'></span>\";
				var divider = $('<option/>')
					.addClass('divider')
					.data('divider', true);
				var addoption = $('<option/>', {class: 'addItem'})
					.data('content', content);
				$('#fileProject')
					.append(divider)
					.append(addoption)
					.selectpicker('refresh');
		")),
	pickerInput('fileProject', 'Select a project', choices=unique(samples()$project)))
})

output$uiFileProjectRaw <- renderUI({
	tagList(
		tags$script(HTML("
			var content = \"<input type='text' class='bss-input' onKeyDown='event.stopPropagation();' onKeyPress='addSelectInpKeyPress(this,event)' onClick='event.stopPropagation()' placeholder='Add item'> <span class='glyphicon glyphicon-plus addnewicon' onClick='addSelectItem(this,event,1);'></span>\";
				var divider = $('<option/>')
					.addClass('divider')
					.data('divider', true);
				var addoption = $('<option/>', {class: 'addItem'})
					.data('content', content);
				$('#fileProject')
					.append(divider)
					.append(addoption)
					.selectpicker('refresh');
		")),
	pickerInput('fileProject', 'Select a project', choices=unique(samples()$project)))
})

#record file in db
addFile <- function(sample, path, project, txtFile=NULL){
	file <- readMSData(path, msLevel=1, mode='onDisk')
	#test the converted file
	polarity <- unique(polarity(file))
	if(length(polarity) == 1 & polarity == 1){
		file.remove(path)
		if(!is.null(txtFile)) file.remove(txtFile)
		return('is in positive polarity!')
	}
	else if(length(polarity) > 1){
		# cut the file
		spectras <- which(polarity(file) == 0)
		file <- file[spectras]
		file.remove(path)
		writeMSData(file, path, outformat='mzxml', copy=TRUE)
	}
	if(FALSE %in% isCentroided(file)){
		file.remove(path)
		if(!is.null(txtFile)) file.remove(txtFile)
		return('is not centroided!')
	}
	# record in DB
	db <- dbConnect(SQLite(), sqlitePath)
	xml_data <- xmlToList(path)
	rawPath <- if(!is.null(xml_data$msRun))	xml_data$msRun$parentFile[1] 
		else xml_data$mzML$fileDescription$sourceFileList[1]$sourceFile$.attrs['location']
	if(!is.null(xml_data$msRun)){
		msManufacturer <- xml_data$msRun$msInstrument$msManufacturer[2]
		msIonisation <- xml_data$msRun$msInstrument$msIonisation[2]
		acquisition <- paste(xml_data$msRun$msInstrument$software[2], " version ", xml_data$msRun$msInstrument$software[3])
		conversion <- paste(xml_data$msRun$dataProcessing$software[2], " version ", xml_data$msRun$dataProcessing$software[3])
		centroided <- xml_data$msRun$scan$.attrs[3]
		msLevel <- xml_data$msRun$scan$.attrs[4]
		if(!is.null(txtFile)){
			lines <- readLines(paste0(file_path_sans_ext(path), ".txt"))
			method <- substr(lines[2], 1, nchar(lines[2])-1)
			line <- strsplit(lines[31], "  ")
			resolution <- line[[1]][length(line[[1]])]
			line <- strsplit(lines[32], "  ")
			agcTarget <- line[[1]][length(line[[1]])]
			line <- strsplit(lines[33], "  ")
			maximumIT <- line[[1]][length(line[[1]])]
			line <- strsplit(lines[34], "  ")
			numberOfScanRange <- line[[1]][length(line[[1]])]
			line <- strsplit(lines[35], "  ")
			scanRange <- line[[1]][length(line[[1]])]    
			query <- sprintf('insert into sample (sample, project, path, rawPath, msManufacturer, msIonisation, acquisition, conversion, msLevel, polarity, method, resolution, agcTarget, maximumIT, numberOfScanRange, scanRange) values ("%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s");',
				sample, project, path, rawPath, msManufacturer, msIonisation, acquisition, conversion, msLevel, 'negative', method, resolution, agcTarget, maximumIT, numberOfScanRange, scanRange)
			if(length(query) == 1){
				dbSendQuery(db, query)
				dbDisconnect(db)
				return('imported')
			}
		}
		query <- sprintf('insert into sample (sample, project, path, rawPath, msManufacturer, msIonisation, acquisition, conversion, msLevel, polarity) values ("%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s");', 
			sample, project, path, rawPath, msManufacturer, msIonisation, acquisition, conversion, msLevel, 'negative')
		if(length(query) == 1){
			dbSendQuery(db, query)
			dbDisconnect(db)
			return('imported')
		}
	}
	query <- sprintf('insert into sample (sample, project, path, rawPath, polarity) values ("%s", "%s", "%s", "%s", "%s");', 
		sample, project, path, rawPath, 'negative')
	dbSendQuery(db, query)
	dbDisconnect(db)
	return('imported')
}


