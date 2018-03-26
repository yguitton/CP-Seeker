#record file in db
addFile <- function(sample, path, project, polarity, txtFile=NULL){
	db <- dbConnect(SQLite(), sqlitePath)
	xml_data <- xmlToList(path)
	rawPath <- if(!is.null(xml_data$msRun))	xml_data$msRun$parentFile[1] else xml_data$mzML$fileDescription$sourceFileList[1]$sourceFile$.attrs['location']
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
			queries <- c(sprintf('insert into sample (sample, path, rawPath, msManufacturer, msIonisation, acquisition, conversion, msLevel, polarity, method, resolution, agcTarget, maximumIT, numberOfScanRange, scanRange) values ("%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s");',	sample, path, rawPath, msManufacturer, msIonisation, acquisition, conversion, msLevel, polarity, method, resolution, agcTarget, maximumIT, numberOfScanRange, scanRange), sprintf('insert into project_sample (project, sample) values (%s, "%s");', project, sample))
			if(length(queries) == 2){
				dbSendQueries(db, queries)
				dbDisconnect(db)
				return()
			}
		}
		queries <- c(sprintf('insert into sample (sample, path, rawPath, msManufacturer, msIonisation, acquisition, conversion, msLevel, polarity) values ("%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s");', sample, path, rawPath, msManufacturer, msIonisation, acquisition, conversion, msLevel, polarity), sprintf('insert into project_sample (project, sample) values (%s, "%s");', project, sample))
		if(length(queries) == 2){
			dbSendQueries(db, queries)
			dbDisconnect(db)
			return()
		}
	}
	queries <- c(sprintf('insert into sample (sample, path, rawPath, polarity) values ("%s", "%s", "%s", "%s");', sample, path, rawPath, polarity), sprintf('insert into project_sample (project, sample) values (%s, "%s");', project, sample))
	dbSendQueries(db, queries)
	dbDisconnect(db)
}

#for choosen the project where to add file
output$selectProjectAddFile <- renderUI({
	selectizeInput("projectAddFile", "Select a project", 
		choices=unique(samples()$project), options=list(create=TRUE))
})

