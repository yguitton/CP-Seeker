options(shiny.maxRequestSize=2000*1024^2)
library(RSQLite)
library(DT)
library(plotly)
library(xcms)
library(XML)
library(tools)
library(shinyjs)
library(pracma)
library(stringr)
library(shinyWidgets)
library(shinyFiles)
library(openxlsx)
normalizePath('.')
sqlitePath <- "database.db"
converter <- file_path_as_absolute('../pwiz/msconvert.exe')
thermo <- file_path_as_absolute('../pwiz/ThermoRawMetaDump.exe')
dirOutput <- 'mzXMLFiles' 	#where sample files are stored
sharingPath <- 'm518 386q0 8-5 13t-13 5q-37 0-63-27t-26-63q0-8 5-13t13-5 12 5 5 13q0 23 16 38t38 16q8 0 13 5t5 13z m125-73q0-59-42-101t-101-42-101 42-42 101 42 101 101 42 101-42 42-101z m-572-320h858v71h-858v-71z m643 320q0 89-62 152t-152 62-151-62-63-152 63-151 151-63 152 63 62 151z m-571 358h214v72h-214v-72z m-72-107h858v143h-462l-36-71h-360v-72z m929 143v-714q0-30-21-51t-50-21h-858q-29 0-50 21t-21 51v714q0 30 21 51t50 21h858q29 0 50-21t21-51z'


shinyServer(function(input, output, session) {
#to close the connection
session$onSessionEnded(function() {
stopApp()
})

actualize <- reactiveValues() #scope values

# read in sql-statements and preformat them                                        
sqlFromFile <- function(file){
    require(stringr)
    sql <- readLines(file)
    sql <- unlist(str_split(paste(sql,collapse=" "),";"))
    sql <- sql[grep("^ *$", sql, invert=T)]
}

# apply query function to each element
dbSendQueries <- function(con,sql){
    dummyfunction <- function(sql,con){
        dbSendQuery(con,sql)
    }
    lapply(sql, dummyfunction, con)
}

#----------------------------------------------------------------------------------------------
# shinyFiles is incompatible with R v3.4
# Two functions are overridden file.info() (base package) and fileGetter() (shinyFiles package) 
#-----------------------------------------------------------------------------------------------
fileGetter <- function (roots, restrictions, filetypes, hidden = FALSE){
	if (missing(filetypes)) filetypes <- NULL
	if (missing(restrictions)) restrictions <- NULL
	function(dir, root){
		currentRoots <- if (class(roots) == "function") roots()
						else roots
		if (is.null(names(currentRoots))) stop("Roots must be a named vector or a function returning one")
		if (missing(root)) root <- names(currentRoots)[1]
		fulldir <- file.path(currentRoots[root], dir)
		writable <- as.logical(file.access(fulldir, 2) == 0)
		files <- list.files(fulldir, all.files = hidden, full.names = TRUE, no.. = TRUE)
		files <- gsub(pattern = "//*", "/", files, perl = TRUE)
		if(!is.null(restrictions) && length(files) != 0){
			if(length(files) == 1) keep <- !any(sapply(restrictions, function(x) { grepl(x, files, fixed = T) }))
			else keep <- !apply(sapply(restrictions, function(x) { grepl(x, files, fixed = T) }), 1, any)
			files <- files[keep]
		}
		fileInfo <- (file.info(files))
		fileInfo$filename <- basename(files)
		fileInfo$extension <- tolower(tools::file_ext(files))
		validIndex <- which(!is.na(fileInfo$mtime))
		fileInfo <- fileInfo[validIndex,]
		fileInfo$mtime <- format(fileInfo$mtime, format = "%Y-%m-%d-%H-%M")
		fileInfo$ctime <- format(fileInfo$ctime, format = "%Y-%m-%d-%H-%M")
		fileInfo$atime <- format(fileInfo$atime, format = "%Y-%m-%d-%H-%M")
		if(!is.null(filetypes)){
			matchedFiles <- tolower(fileInfo$extension) %in%
				tolower(filetypes) & fileInfo$extension != ""
			fileInfo$isdir[matchedFiles] <- FALSE
			fileInfo <- fileInfo[matchedFiles | fileInfo$isdir, ]
		}
		rownames(fileInfo) <- NULL
		breadcrumps <- strsplit(dir, .Platform$file.sep)[[1]]
		list(files = fileInfo[, c("filename", "extension", "isdir", "size", "mtime", "ctime", "atime")], writable = writable,
					exist = file.exists(fulldir), breadcrumps = I(c("", breadcrumps[breadcrumps != ""])), roots = I(names(currentRoots)), root = root)
	}
}
unlockBinding("fileGetter", getNamespace("shinyFiles"))
assign("fileGetter",fileGetter,getNamespace("shinyFiles"))

file.info <- function (..., extra_cols = TRUE){
	suppressWarnings(res <- .Internal(file.info(fn <- c(...), extra_cols)))
	res$mtime <- .POSIXct(res$mtime)
	res$ctime <- .POSIXct(res$ctime)
	res$atime <- .POSIXct(res$atime)
	class(res) <- "data.frame"
	attr(res, "row.names") <- fn
	res
}
unlockBinding("file.info", getNamespace("base"))
assign("file.info",file.info,getNamespace("base"))	

actualize$graph <- FALSE
actualize$samples <- FALSE

samples <- reactive({
	actualize$samples
	db <- dbConnect(SQLite(), sqlitePath)
	samples <- dbGetQuery(db, 'select * from sample;')
	dbDisconnect(db)
	actualize$samples <- FALSE
	return(samples)
})

output$uiSelectProject <- renderUI({ selectInput('selectProject', 'Select a project', choices=unique(samples()$project), multiple=FALSE) })

hide(id='loader', anim=TRUE, animType='fade')
shinyjs::show("app-content")
		
#----------------------------------------------------------------
#---------------------FILES---------------------------------------
#-----------------------------------------------------------------
#-----------------------------------------------------------------------
#record file in db
addFile <- function(sample, path, project, txtFile=NULL){
	db <- dbConnect(SQLite(), sqlitePath)
	xml_data <- xmlToList(path)
	rawPath <- if(!is.null(xml_data$msRun))	xml_data$msRun$parentFile[1] 
		else xml_data$mzML$fileDescription$sourceFileList[1]$sourceFile$.attrs['location']
	if(!is.null(xml_data$msRun)){
		msManufacturer <- xml_data$msRun$msInstrument$msManufacturer[2]
		msIonisation <- xml_data$msRun$msInstrument$msIonisation[2]
		acquisition <- paste(xml_data$msRun$msInstrument$software[2], " version ",  
			xml_data$msRun$msInstrument$software[3])
		conversion <- paste(xml_data$msRun$dataProcessing$software[2], " version ",  
			xml_data$msRun$dataProcessing$software[3])
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
			query <- sprintf('insert into sample (sample, path, rawPath, msManufacturer, msIonisation, acquisition, conversion, msLevel, polarity, method, resolution, agcTarget, maximumIT, numberOfScanRange, scanRange, project) values ("%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s");',
					sample, path, rawPath, msManufacturer, msIonisation, acquisition, conversion, msLevel, 
					'negative', method, resolution, agcTarget, maximumIT, numberOfScanRange, 
					scanRange, project)
			if(length(query) != 0){
				dbSendQuery(db, query)
				dbDisconnect(db)
				return()
			}
		}
		query <- sprintf('insert into sample (sample, path, rawPath, msManufacturer, msIonisation, acquisition, conversion, msLevel, polarity, project) values ("%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s");',
				sample, path, rawPath, msManufacturer, msIonisation, acquisition, conversion,
				msLevel, 'negative', project)
		if(length(query) != 0){
			dbSendQuery(db, query)
			dbDisconnect(db)
			return()
		}
	}
	query <- sprintf('insert into sample (sample, path, rawPath, polarity, project) values ("%s", "%s", "%s", "%s", "%s");',
			sample, path, rawPath, 'negative', project)
	dbSendQuery(db, query)
	dbDisconnect(db)
}

#for choosen the project where to add file
output$selectProjectAddFile <- renderUI({
	selectizeInput("projectAddFile", "Select a project", 
		choices=unique(samples()$project), options=list(create=TRUE))
})

#---------------------import mzXML-------------------------------
shinyFileChoose(input, 'fileImportmzXML', roots=getVolumes(), filetypes=c('mzML', 'mzXML'))
observeEvent(input$fileImportmzXML, {
	showModal(modalDialog(easyClose=TRUE, title='Choose a project',
		uiOutput('selectProjectAddFile'),
		footer=actionButton('filemzXMLAdd', 'Valid')
	))
})

observeEvent(input$filemzXMLAdd, {
	removeModal()
	hide('app-content')
	shinyjs::show('loader')
	files <- parseFilePaths(getVolumes(), input$fileImportmzXML)
	success <- c()
	withProgress(message='importation', value=0, max=nrow(files), {
	for(i in 1:nrow(files)){
		name <- as.character(files[i, 'name'])
		path <- as.character(files[i, 'datapath'])
		incProgress(amount=0, detail=name)
		sample <- file_path_sans_ext(name)
		if(sample %in% samples()$sample) success[i] <- paste(name, 'already imported in project', samples()[which(samples()$sample == name), 'project'])
		else{
			file.copy(path, file.path(dirOutput, name))
			addFile(sample=sample, path=file.path(dirOutput, name), project=input$projectAddFile)
			success[i] <- paste(name, 'Success!')
		}
		incProgress(1)
	}})
	actualize$samples <- TRUE
	hide('loader')
	shinyjs::show('app-content')
	return(showModal(modalDialog(paste(success, collapse='\n'))))
})


#----------------import raw---------------------------------------
shinyFileChoose(input, 'fileImportRaw', roots=getVolumes(), filetypes=c('raw'))
#the conversion event
observeEvent(input$fileImportRaw, {
	showModal(modalDialog(easyClose=TRUE, title='Choose a project',
		uiOutput('selectProjectAddFile'),
		footer=actionButton('fileRawAdd', 'Valid')
	))
})
observeEvent(input$fileRawAdd, {
	removeModal()
	hide('app-content')
	shinyjs::show('loader')
	files <- parseFilePaths(getVolumes()(), input$fileImportRaw)
	success <- c()
	withProgress(message='conversion', value=0, max=nrow(files), {
	for(i in 1:nrow(files)){
		name <- as.character(files[i, 'name'])
		path <- as.character(files[i, 'datapath'])
		incProgress(amount=0, detail=name)
		success <- msConvert(path, polarity='negative', name=name, 
			success=success, project=input$projectAddFile)
		incProgress(amount=1)
	}
	})
	actualize$samples <- TRUE
	hide('loader')
	shinyjs::show('app-content')
	return(showModal(modalDialog(div(paste(success, collapse='\n')))))
})

#the function for the conversion
msConvert <- function(path, polarity, name, success, project){
	tryCatch({
		query <- sprintf('""%s" "%s" -o "%s" --mzXML --32 --zlib --filter "peakPicking true 1" --filter "polarity %s""', 
			converter, path, file.path(getwd(), dirOutput), polarity) 
		shell(query)
		if(!file.info(path)$isdir){
			query <- sprintf('""%s" --scanTrailers "%s" > "%s""',
				thermo, path, file.path(getwd(), dirOutput, 
					paste(file_path_sans_ext(name), '.txt', sep='')))
			shell(query)
			addFile(sample=file_path_sans_ext(name), 
				path=file.path(getwd(), dirOutput, 
					paste(file_path_sans_ext(name), '.mzXML', sep='')), 
				project=project, txtFile=file.path(getwd(), dirOutput, 
					paste(file_path_sans_ext(name), '.txt', sep='')))	
		} else {
			addFile(sample=file_path_sans_ext(name), 
				path=file.path(getwd(), dirOutput, 
					paste(file_path_sans_ext(name), '.mzXML', sep='')), 
				project=project)
			return(c(success, paste(name, 'Sucess!')))
		}
	}, error=function(e){
		success <- c(success, paste(name, e, sep=' : '))
		return(success)
	})
}

#--------------------------------------------------------------
#-----------------------------INFORMATION----------------------
#-------------------------------------------------------------- 
output$uiFileInfo <- renderUI({
	if(is.null(input$selectProject)) return(selectInput('fileDTInfo', 'Choose a file', choices=samples()[which(samples()$project == unique(samples()$project)[1]), 'sample']))
	else choices <- samples()[which(samples()$project == input$selectProject), 'sample']
	selectInput('fileDTInfo', 'Choose a file', choices=choices, multiple=FALSE)
})

#about conversion
output$tableInfoConversion <- renderDataTable(t(samples()[which(samples()$sample == input$fileDTInfo), ]), selection='none', colnames='Info', options=list(bFilter=FALSE, paging=FALSE, dom = 'frtip'))

#---------------------------------------------------------------------
#-------------------TARGET FULL----------------------------------
#---------------------------------------------------------------------
#the files
output$uiTargetFullSelectFiles <- renderUI({
	selectInput("targetFullFiles", "Select file(s)", choices=samples()[which(samples()$project == input$selectProject), 'sample'], multiple=TRUE)
})

shinyjs::disable('targetFullSubmit')
observeEvent(input$targetFullFiles, {
	if(length(input$targetFullFiles) > 0) shinyjs::enable('targetFullSubmit') else shinyjs::disable('targetFullSubmit')
})

targetMasses <- function(xraw, mzs, tolPpm, rt, threshold){
	AUC <- c()
	mzList <- c()
	deviationPpm <- c()
	for(i in 1:length(mzs)){
		mz <- mzs[i]
		mzRange <- c(mz-(mz*tolPpm)/10**6, mz+(mz*tolPpm)/10**6)
		points <- extractMsData(xraw, mz=mzRange, rt=rt)[[1]]
		points <- points[which(points$i >= threshold), ]
		#compute the area
		tmpAUC <- trapz(points$rt, points$i)
		#test if the intensity decrease
		if(tmpAUC == 0) return(list(AUC=AUC, mzObserved=mzList, deviationPpm=deviationPpm))
		else if(tmpAUC > 0 & i == 1) mzObserved <- points[which(points$i == max(points$i)), 'mz']
		else if(tmpAUC >= AUC[i - 1]*1.2) return(list(AUC=AUC, mzObserved=mzList, deviationPpm=deviationPpm))
		else mzObserved <- points[which(points$i == max(points$i)), 'mz']
		if(mzObserved == -Inf) return(list(AUC=AUC, mzObserved=mzList, deviationPpm=deviationPpm))
		else{
			AUC <- c(AUC, tmpAUC)
			mzList <- c(mzList, mzObserved)
			deviationPpm <- c(deviationPpm, round(abs((mzObserved - mz) / mz * 10**6), digits=2))
		}
	}
	return(list(AUC=AUC, mzObserved=mzList, deviationPpm=deviationPpm))	
}

recordTarget <- function(db, sample, xraw, molecule, data, rt, tolPpm, tolAbd, threshold){
	res <- targetMasses(xraw, data$mz, tolPpm, rt, threshold)
	AUC <- res$AUC
	if(length(AUC) < 2) return()
	else print('FIND')
	deviationPpm <- res$deviationPpm
	mzObserved <- res$mzObserved
	observed <- sapply(AUC, function(x) round((x*100)/AUC[1], digits=2))
	score <- computeScore(data$abd, observed, tolAbd)
	query <- sprintf('insert into observed (molecule, tolPpm, rtMin, rtMax, threshold, tolAbd, score, ppmDeviation, sample) values (%s, %s, %s, %s, %s, %s, %s, %s, "%s");',
		molecule, tolPpm, round(rt[1]/60, digits=2), round(rt[2]/60, digits=2), threshold, tolAbd, score, round(mean(deviationPpm), digits=2), sample)
	dbSendQuery(db, query)
	observedId <- dbGetQuery(db, 'select last_insert_rowid() from observed;')$last_insert_rowid[1]
	query <- sprintf('insert into measured (observed, mz, auc, abd) values %s;',
		paste(sapply(1:length(observed), function(x)
			sprintf('(%s, %s, %s, %s)',
				observedId, mzObserved[[x]], AUC[[x]], observed[[x]])),
		sep='', collapse=', '))
	dbSendQuery(db, query)
}

computeScore <- function(theoric, observed, tolAbd){
	weight <- sapply(1:length(observed), function(x) theoric[x] / sum(theoric))
	observed <- sapply(1:length(observed), function(x) if(x < 2) observed[x] else (observed[x] * 100) / observed[x-1])
	theoric <- sapply(1:length(observed), function(x) if(x < 2) theoric[x] else (theoric[x] * 100) / theoric[x-1])
	# deltaMz <- sapply(deviationPpm, function(x) x/tolPpm)
	deltaAbundance <- sapply(1:length(observed), function(x) abs((observed[x] - theoric[x]) / tolAbd))
	# deviation <- sqrt(deltaMz**2 + deltaAbundance**2)
	deviation <- deltaAbundance
	deviation[which(deviation > 1)] <- 1
	score <- 100 * (1 - sum(deviation * weight))
	return(round(score, digits=0))
}

#the Target event
observeEvent(input$targetFullSubmit, {
	hide('app-content')
	shinyjs::show('loader')
	db <- dbConnect(SQLite(), sqlitePath)
	# create the param for each file or update it
	query <- sprintf('select sample from param where sample in (%s) and adduct == "%s";', 
		paste('"', input$targetFullFiles, '"', collapse=', ', sep=''), input$targetFullAdduct)
	toUpdate <- dbGetQuery(db, query)$sample
	sapply(toUpdate, function(x) dbSendQuery(db, sprintf(
		'update param set ppmTol = %s, rtMin = %s, rtMax = %s, threshold = %s, abdTol = %s where sample == "%s" and adduct == "%s";',
		input$targetFullPpm, input$targetFullRt[1], input$targetFullRt[2], input$targetFullThreshold, input$targetFullTolAbd, x, input$targetFullAdduct)))
	sapply(input$targetFullFiles[which(!input$targetFullFiles %in% toUpdate)], function(x) dbSendQuery(db, sprintf(
		'insert into param (ppmTol, rtMin, rtMax, threshold, abdTol, sample, adduct) values (%s, %s, %s, %s, %s, "%s", "%s");',
		input$targetFullPpm, input$targetFullRt[1], input$targetFullRt[2], input$targetFullThreshold, input$targetFullTolAbd, x, input$targetFullAdduct)))
	queries <- c(
		sprintf('delete from measured where observed in (select id from observed where sample in (%s) and molecule in (select id from molecule where adduct == "%s"));',
			paste('"', input$targetFullFiles, '"', sep='', collapse=', '), input$targetFullAdduct),
		sprintf('delete from observed where sample in (%s) and molecule in (select id from molecule where adduct == "%s");',
			paste('"', input$targetFullFiles, '"', sep='', collapse=', '), input$targetFullAdduct))
	dbSendQueries(db, queries)	
	query <- sprintf('select id from molecule where adduct == "%s";',
		input$targetFullAdduct)
	molecules <- dbGetQuery(db, query)$id
	query <- sprintf('select mz, abd, molecule from theoric where molecule in (select molecule from molecule where adduct == "%s");',
		input$targetFullAdduct)
	theoric <- dbGetQuery(db, query)
	xraws <- sapply(samples()[which(samples()$sample %in% input$targetFullFiles), 'path'], function(file) 
		readMSData(file, centroided=TRUE, msLevel=1, mode='onDisk'))
	withProgress(message='target chloroparaffins', value=0, max=length(molecules), {
	for(molecule in molecules){
		data <- theoric[which(theoric$molecule == molecule), c('mz', 'abd')]
		sapply(1:length(xraws), function(x) 
			recordTarget(db=db, sample=input$targetFullFiles[x], xraw=xraws[[x]], 
				molecule=molecule, data=data[order(data$abd, decreasing=TRUE), ], rt=input$targetFullRt*60, 
				tolPpm=input$targetFullPpm, tolAbd=input$targetFullTolAbd, 
				threshold=input$targetFullThreshold))
		incProgress(amount=1)
	}
	})
	dbDisconnect(db)
	actualize$graph <- if(actualize$graph) FALSE else TRUE
	hide('loader')
	shinyjs::show('app-content')
})

output$targetFullGraph <- renderPlotly({
	actualize$graph
	graph <- plot_ly(type='scatter3d', mode='markers') %>% 
		layout(scene=list(xaxis=list(title='Cl', autorange='reversed', rangemode='tozero'), yaxis=list(title='C', rangemode='tozero'), zaxis=list(title='Intensity')))%>% 
		config(editable=TRUE, scrollZoom=TRUE, showLink=TRUE, displaylogo=FALSE, 
			modeBarButtons=list(
				list(
					list(
						name='toImage2', 
						title='Download plot as png', 
						icon=list(width = 1000, ascent = 850, descent = -150, path = sharingPath), 
						click=htmlwidgets::JS(sprintf("function(gd){Plotly.downloadImage(gd, {format:'png', width:1920, height:1080, filename:'chromato'})}"))
					)
				), list('zoom3d', 'pan3d', 'resetCameraDefault3d', 'orbitRotation', 'tableRotation')
			)
		)
	if(is.null(input$targetFullFiles)) return(graph)
	if(length(input$targetFullFiles) == 0) return(graph)
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select formula, sum(auc), adduct from measured inner join observed on observed.id = measured.observed inner join molecule on molecule.id = observed.molecule group by observed having sample in (%s);',
		paste('"', input$targetFullFiles, '"', sep='', collapse=', '))
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	if(nrow(data) == 0) return(graph)
	data$C <- as.numeric(str_extract_all(str_extract_all(data$formula, 'C[[:digit:]]+'), '[[:digit:]]+'))
	data$Cl <- as.numeric(str_extract_all(str_extract_all(data$formula, 'Cl[[:digit:]]+'), '[[:digit:]]+'))
	for(adduct in unique(data$adduct)) graph <- graph %>% 
		add_trace(x=data[which(data$adduct == adduct), 'Cl'], y=data[which(data$adduct == adduct), 'C'], 
			z=data[which(data$adduct == adduct), 'sum(auc)']*0.5, size=I(1), name=adduct,
			error_z=list(thickness=0, symmetric=TRUE, type='data', array=data[which(data$adduct == adduct), 'sum(auc)']*0.5), 
				hoverinfo='text', text=paste0('Adduct: ', adduct, '<br />C: ', data[which(data$adduct == adduct), 'C'], 
				'<br />Cl: ', data[which(data$adduct == adduct), 'Cl'], 
				'<br />Intensity :', formatC(data[which(data$adduct == adduct), 'sum(auc)'], format='e', digits=2)))
	return(graph)
})

#-------------------------------------------------------------------------------------
#--------------------TARGET---------------------------------------------------------
#------------------------------------------------------------------------------------
shinyjs::disable('targetSubmit')
shinyjs::disable('targetDelete')

output$uiTargetFile <- renderUI({
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select distinct(sample) from observed where sample in (select sample from sample where project == "%s");',
		input$selectProject)
	files <- dbGetQuery(db, query)[, 1]
	dbDisconnect(db)
	pickerInput("targetFile", "Select file", choices=files, multiple=FALSE)
})

output$uiTargetAdduct <- renderUI({
	db <- dbConnect(SQLite(), sqlitePath)
	choices <- if(is.null(input$targetFile)) c()
		else if(input$targetFile == '') c()
		else dbGetQuery(db, 
			sprintf('select distinct(adduct) from molecule where id in ( select molecule from observed where sample == "%s");',
				input$targetFile))$adduct
	dbDisconnect(db)
	pickerInput('targetAdduct', 'Select adduct', choices=choices)
})

targetTableFunctionPpmDeviation <- function(file, adduct){
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select formula, ppmDeviation, adduct from observed inner join molecule on molecule.id = observed.molecule where sample == "%s" and adduct == "%s";',
		file, adduct)
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	data$C <- as.numeric(str_extract_all(str_extract_all(data$formula, 'C[[:digit:]]+'), '[[:digit:]]+'))
	data$Cl <- as.numeric(str_extract_all(str_extract_all(data$formula, 'Cl[[:digit:]]+'), '[[:digit:]]+'))
	res <- as.data.frame(matrix(NA, nrow=29, ncol=27))
	colnames(res) <- paste0('Cl', 4:30)
	rownames(res) <- paste0('C', 8:36)
	for(row in 1:nrow(data)) res[data[row, 'C']-7, data[row, 'Cl']-3] <- data[row, 'ppmDeviation']
	return(res)
}

targetTableFunctionScore <- function(file, adduct){
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select formula, score, adduct from observed inner join molecule on molecule.id = observed.molecule where sample == "%s" and adduct == "%s";',
		file, adduct)
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	data$C <- as.numeric(str_extract_all(str_extract_all(data$formula, 'C[[:digit:]]+'), '[[:digit:]]+'))
	data$Cl <- as.numeric(str_extract_all(str_extract_all(data$formula, 'Cl[[:digit:]]+'), '[[:digit:]]+'))
	res <- as.data.frame(matrix(NA, nrow=29, ncol=27))
	colnames(res) <- paste0('Cl', 4:30)
	rownames(res) <- paste0('C', 8:36)
	for(row in 1:nrow(data)) res[data[row, 'C']-7, data[row, 'Cl']-3] <- data[row, 'score']
	return(res)
}

targetTableFunctionInto <- function(file, adduct){
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select formula, sum(auc), adduct from measured inner join observed on observed.id = measured.observed inner join molecule on molecule.id = observed.molecule group by observed having sample == "%s" and adduct == "%s";',
		file, adduct)
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	data$C <- as.numeric(str_extract_all(str_extract_all(data$formula, 'C[[:digit:]]+'), '[[:digit:]]+'))
	data$Cl <- as.numeric(str_extract_all(str_extract_all(data$formula, 'Cl[[:digit:]]+'), '[[:digit:]]+'))
	res <- as.data.frame(matrix(NA, nrow=29, ncol=27))
	colnames(res) <- paste0('Cl', 4:30)
	rownames(res) <- paste0('C', 8:36)
#	for(row in 1:nrow(data)) res[data[row, 'C']-7, data[row, 'Cl']-3] <- format(data[row, 'sum(auc)'], format='e', digits=2)
	for(row in 1:nrow(data)) res[data[row, 'C']-7, data[row, 'Cl']-3] <- data[row, 'sum(auc)']
	return(res)
}

output$targetTablePpmDeviation <- renderDataTable({
	if(is.null(input$targetFile) | is.null(input$targetAdduct)) return(c())
	if(input$targetFile == '' | input$targetAdduct == '') return(c())
	input$targetPpm
	res <- targetTableFunctionPpmDeviation(input$targetFile, input$targetAdduct)
}, selection='none', extensions=c('Scroller', 'Buttons'), options=list(dom='Bfrtip', scrollX=TRUE, scrollY=550, scroller=TRUE, deferRender=TRUE, bFilter=FALSE, 
ordering=FALSE, buttons=htmlwidgets::JS('
	[
		{
			text: "Export to excel",
			action:function(e, table, node, config){
				document.getElementById("targetDownload").click();
			}
		}
	]
'), initComplete=htmlwidgets::JS(paste("
	function(){
		var api = this.api();
		var $cell = api.cell(", 
		if(is.null(isolate(input$targetTable_cell_selected))) "-1, -1" else paste(isolate(input$targetTable_cell_selected$C)-8, isolate(input$targetTable_cell_selected$Cl)-3, sep=', '), 
		").nodes().to$();
		$cell.addClass('selected');
		var ppm = Number(document.getElementById('targetPpm').value);
		api.cells().every(function(){
			if(this.data() != null & this.index().column != 0 & this.data() <= ppm){
				this.nodes().to$().css('background-color', 'green');
			}
			else if(this.data() != null & this.index().column != 0){
				this.nodes().to$().css('background-color', 'red');
			}
		});
	}
"))), callback = htmlwidgets::JS("
	Shiny.onInputChange('targetTable_cell_selected', {C:0, Cl:0});
	table.on('click', 'tbody td', function(){
		if(table.cell(this).data() != null){
			if ( $(this).hasClass('selected') ) {
				$(this).removeClass('selected');
				Shiny.onInputChange('targetTable_cell_selected', {C: 0, Cl: 0});
			}
			else {
				table.$('td.selected').removeClass('selected');
				$(this).addClass('selected');
				Shiny.onInputChange('targetTable_cell_selected', {C:table.cell(this).index().row+8, Cl:table.cell(this).index().column+3});
			}
		}
		
	});
	Shiny.addCustomMessageHandler('targetTableSelectPpm', reselect);
	function reselect(message){
		console.log('ppm');
		table.$('td.selected').removeClass('selected');
		var $cell = table.cell(message.row, message.column).nodes().to$();
		$cell.toggleClass('selected');
		//table.cell(message.row, message.column).data(message.ppm);
	};
"))

output$targetTableScore <- renderDataTable({
	if(is.null(input$targetFile) | is.null(input$targetAdduct)) return(c())
	if(input$targetFile == '' | input$targetAdduct == '') return(c())	
	res <- targetTableFunctionScore(input$targetFile, input$targetAdduct)
}, selection='none', extensions=c('Scroller', 'Buttons'), options=list(dom='Bfrtip', scrollX=TRUE, scrollY=550, scroller=TRUE, deferRender=TRUE, bFilter=FALSE, ordering=FALSE, buttons=htmlwidgets::JS('
	[
		{
			text: "Export to excel",
			action:function(e, table, node, config){
				document.getElementById("targetDownload").click();
			}
		}
	]
'), initComplete=htmlwidgets::JS(paste("
	function(){
		var api = this.api();
		var $cell = api.cell(", 
		if(is.null(isolate(input$targetTable_cell_selected))) "-1, -1" else paste(isolate(input$targetTable_cell_selected$C)-8, isolate(input$targetTable_cell_selected$Cl)-3, sep=', '), 
		").nodes().to$();
		$cell.addClass('selected');
	}
"))), callback = htmlwidgets::JS("
	table.on('click', 'tbody td', function(){
		if(table.cell(this).data() != null){
			if ( $(this).hasClass('selected') ) {
				$(this).removeClass('selected');
				Shiny.onInputChange('targetTable_cell_selected', {C: 0, Cl: 0});
			}
			else {
				table.$('td.selected').removeClass('selected');
				$(this).addClass('selected');
				Shiny.onInputChange('targetTable_cell_selected', {C:table.cell(this).index().row+8, Cl:table.cell(this).index().column+3});
			}
		}
		
	});
	Shiny.addCustomMessageHandler('targetTableSelectScore', reselect);
	function reselect(message){
		console.log('score');
		table.$('td.selected').removeClass('selected');
		var $cell = table.cell(message.row, message.column).nodes().to$();
		$cell.toggleClass('selected');
		//table.cell(message.row, message.column).data(message.score);
	};
"))


computeSumAUC <- function(file, adduct, C, Cl){
	if(is.null(C) | is.null(Cl) | is.null(file) | is.null(adduct)) return(0)
	if(file == '' | adduct == '') return(0)
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select formula, sum(auc) from measured inner join observed on observed.id = measured.observed inner join molecule on molecule.id = observed.molecule where sample == "%s" and adduct == "%s" group by observed;',
		file, adduct)
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	if(nrow(data) == 0) return(0)
	data$C <- as.numeric(str_extract_all(str_extract_all(data$formula, 'C[[:digit:]]+'), '[[:digit:]]+'))
	data$Cl <- as.numeric(str_extract_all(str_extract_all(data$formula, 'Cl[[:digit:]]+'), '[[:digit:]]+'))
	res <- sum(data[which(data$C >= C[1] & data$C <= C[2] & data$Cl >= Cl[1] & data$Cl <= Cl[2]), 'sum(auc)'])
	return(formatC(res, format='e', digits=2))
}

output$targetSumAUC <- renderText({
	return(paste('Sum of AUC:', computeSumAUC(input$targetFile, input$targetAdduct, input$targetC, input$targetCl)))	
})

output$targetTableInto <- renderDataTable({
	if(is.null(input$targetFile) | is.null(input$targetAdduct)) return(c())
	if(input$targetFile == '' | input$targetAdduct == '') return(c())	
	res <- targetTableFunctionInto(input$targetFile, input$targetAdduct)
	res <- apply(res, c(1, 2), function(x) if(!is.na(x)) formatC(x, format='e', digits=2) else NA)
}, selection='none', extensions=c('Scroller', 'Buttons'), options=list(dom='Bfrtip', scrollX=TRUE, scrollY=450, scroller=TRUE, deferRender=TRUE, bFilter=FALSE, ordering=FALSE, buttons=htmlwidgets::JS('
	[
		{
			text: "Export to excel",
			action:function(e, table, node, config){
				document.getElementById("targetDownload").click();
			}
		}
	]
'), initComplete=htmlwidgets::JS(paste("
	function(){
		var api = this.api();
		var $cell = api.cell(", 
		if(is.null(isolate(input$targetTable_cell_selected))) "-1, -1" else paste(isolate(input$targetTable_cell_selected$C)-8, isolate(input$targetTable_cell_selected$Cl)-3, sep=', '), 
		").nodes().to$();
		$cell.addClass('selected');
	}
"))), callback = htmlwidgets::JS("
	table.on('change', '#targetTable_cell_selected', function(){
		var $cell = table.cell(this.value[1]-8, this.value[2])
	})
	table.on('click', 'tbody td', function(){
		if(table.cell(this).data() != null){
			if ( $(this).hasClass('selected') ) {
				$(this).removeClass('selected');
				Shiny.onInputChange('targetTable_cell_selected', {C: 0, Cl: 0});
			}
			else {
				table.$('td.selected').removeClass('selected');
				$(this).addClass('selected');
				Shiny.onInputChange('targetTable_cell_selected', {C:table.cell(this).index().row+8, Cl:table.cell(this).index().column+3});
			}
		}
		
	});
	Shiny.addCustomMessageHandler('targetTableSelectAUC', reselect);
	function reselect(message){
		console.log('auc');
		table.$('td.selected').removeClass('selected');
		var $cell = table.cell(message.row, message.column).nodes().to$();
		$cell.toggleClass('selected');
		//table.cell(message.row, message.column).data(message.sumOfAuc);
	};
"))

observeEvent(input$targetTable_cell_selected, {
	session$sendCustomMessage('targetTableSelectPpm', list(row=input$targetTable_cell_selected$C-8, column=input$targetTable_cell_selected$Cl-3))
	session$sendCustomMessage('targetTableSelectScore', list(row=input$targetTable_cell_selected$C-8, column=input$targetTable_cell_selected$Cl-3))
	session$sendCustomMessage('targetTableSelectAUC', list(row=input$targetTable_cell_selected$C-8, column=input$targetTable_cell_selected$Cl-3))
})

output$targetDownload <- downloadHandler(
	filename = 'targetROI.xlsx',
	content = function(file){
		wb <- createWorkbook()
		addWorksheet(wb=wb, sheetName='ppmDeviation', gridLines=FALSE)
		addWorksheet(wb=wb, sheetName='Score', gridLines=FALSE)
		addWorksheet(wb=wb, sheetName='AUC', gridLines=FALSE)
		data <- targetTableFunctionPpmDeviation(input$targetFile, input$targetAdduct)
		writeDataTable(wb, sheet=1, x=data, rowNames=TRUE)
		data <- targetTableFunctionScore(input$targetFile, input$targetAdduct)
		writeDataTable(wb, sheet=2, x=data, rowNames=TRUE)
		writeData(wb, sheet=3, x=paste('Sum of AUC:', computeSumAUC(input$targetFile, input$targetAdduct, input$targetC, input$targetCl)))
		writeData(wb, sheet=3, x=paste('   by the filters C:', input$targetC[1], '-', input$targetC[2], ',', 'Cl:', input$targetCl[1], '-', input$targetCl[2]), startRow=2)
		data <- targetTableFunctionInto(input$targetFile, input$targetAdduct)
		writeDataTable(wb, sheet=3, x=data, rowNames=TRUE, startRow=3)
		saveWorkbook(wb, file, overwrite=TRUE)
	}
)

output$targetEIC <- renderPlotly({
	actualize$graph
	eic <- plot_ly(source='targetEIC', type='scatter', mode='markers') %>% 
		layout(xaxis=list(title='retentionTime'), yaxis=list(title='Intensity', rangemode='tozero'), showlegend=TRUE) %>% 
		config(scrollZoom=TRUE, showLink=TRUE, displaylogo=FALSE, 
			modeBarButtons=list(list(list(
				name='toImage2', 
				title='Download plot as png', 
				icon=list(width = 1000, ascent = 850, descent = -150, path = sharingPath), 
				click=htmlwidgets::JS(sprintf("function(gd){Plotly.downloadImage(gd, {format:'png', width:1920, height:1080, filename:'eic'})}"))
				)), list('zoom2d', 'select2d', 'pan2d', 'autoScale2d', 'resetScale2d')))
	cell <- input$targetTable_cell_selected
	if(is.null(cell)) return(eic)
	if(cell$C == 0) return(eic)
	db <- dbConnect(SQLite(), sqlitePath)
	xraw <- readMSData(samples()[which(samples()$sample == input$targetFile), 'path'], centroided=TRUE, msLevel=1, mode='onDisk')
	query <- sprintf('select mz from measured where observed == (select id from observed where sample == "%s" and molecule == (
		select id from molecule where adduct == "%s" and formula like "%s"));',
		input$targetFile, input$targetAdduct, paste('%C', cell$C, '%Cl', cell$Cl, '%', sep='', collapse=''))
	mzs <- dbGetQuery(db, query)$mz
	query <- sprintf('select * from observed where sample == "%s" and molecule == (select id from molecule where adduct == "%s" and formula like "%s");',
		input$targetFile, input$targetAdduct, paste('%C', cell$C, '%Cl', cell$Cl, '%', sep='', collapse=''))
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	mzRange <- matrix(sapply(mzs, function(mz) c(mz-(mz*data$tolPpm)/10^6, mz+(mz*data$tolPpm)/10^6)), ncol=2, byrow=TRUE)
	chrom <- chromatogram(xraw, mz=mzRange, rt=c(min(data$rtMin)*60-60, max(data$rtMax)*60+60), aggregationFun='sum', missing=0, BPPARAM=SnowParam())@.Data
	# get scan where the intensity is at the maximum
	# i <- which(max(sapply(chrom, function(x) max(x@intensity))) == sapply(chrom, function(x) max(x@intensity)))
	# points <- data.frame(rtime=chrom[[i]]@rtime, intensity=chrom[[i]]@intensity)
	# points <- points[which(points$rtime >= data$rtMin*60 & points$rtime <= data$rtMax*60), ]
	# eic <- eic %>% add_trace(x=points$rtime/60, y=points$intensity, size=I(1), fill='tozeroy', 
		# showlegend=FALSE, hoverinfo='text', text=paste('Intensity: ', round(points$intensity, digits=0), 
			# '<br />Retention Time: ', round(points$rtime/60, digits=2)))
	for(i in 1:length(chrom)) eic <- eic %>% 
		add_trace(mode='lines+markers', size=I(1), x=chrom[[i]]@rtime/60, y=chrom[[i]]@intensity, size=I(1), 
			showlegend=FALSE, hoverinfo='text', text=paste('Intensity: ', round(chrom[[i]]@intensity, digits=0), 
				'<br />Retention Time: ', round(chrom[[i]]@rtime/60, digits=2)))
	updateNumericInput(session, 'targetPpm', 'tol mz (ppm)', value=data$tolPpm, min=0, max=50)
	updateNumericInput(session, 'targetRtMin', 'rt min (min)', value=data$rtMin, min=0, max=25)
	updateNumericInput(session, 'targetRtMax', 'rt max (min)', value=data$rtMax, min=0, max=25)
	updateNumericInput(session, 'targetThreshold', 'threshold', value=data$threshold, min=0, max=25)
	updateNumericInput(session, 'targetTolAbd', 'tol abd (%)', value=data$tolAbd, min=0, max=25)
	return(eic)
})

observeEvent(event_data(event='plotly_selected', source='targetEIC'), {
	points <- event_data(event='plotly_selected', source='targetEIC')
	if(is.null(points)) return()
	if(nrow(points) == 0) return()
	rtmin <- min(points$x)
	rtmax <- max(points$x)
	updateNumericInput(session, 'targetRtMin', value=round(rtmin, digits=2), min=0, max=25)
	updateNumericInput(session, 'targetRtMax', value=round(rtmax, digits=2), min=0, max=25)
})

shinyjs::disable('targetSubmit')
shinyjs::disable('targetDelete')
observeEvent(input$targetTable_cell_selected, {
	if(input$targetTable_cell_selected$C == 0){
		shinyjs::disable('targetSubmit')
		shinyjs::disable('targetDelete')
	}
	else{
		shinyjs::enable('targetSubmit')
		shinyjs::enable('targetDelete')
	}
})

observeEvent(input$targetSubmit, {
	hide('app-content')
	shinyjs::show('loader')
	cell <- input$targetTable_cell_selected
	deleteTargetROI(input$targetFile, input$targetAdduct, cell$C, cell$Cl)
	db <- dbConnect(SQLite(), sqlitePath)
	xraw <- readMSData(samples()[which(samples()$sample == input$targetFile), 'path'], centroided=TRUE, msLevel=1, mode='onDisk')
	query <- sprintf('select id from molecule where adduct == "%s" and formula like "%s";',
		input$targetAdduct, paste('%C', cell$C, '%Cl', cell$Cl, '%', sep='', collapse=''))
	molecule <- dbGetQuery(db, query)$id	
	query <- sprintf('select mz, abd from theoric where molecule == %s order by abd desc;',
		molecule)
	data <- dbGetQuery(db, query)	
	recordTarget(db, input$targetFile, xraw, molecule, data, c(input$targetRtMin*60, input$targetRtMax*60), input$targetPpm, input$targetTolAbd, input$targetThreshold)
	res <- dbGetQuery(db, 
		sprintf('select ppmDeviation, score, sum(auc) as sumOfAUC from measured inner join observed on observed.id = measured.observed 
		where molecule == %s and sample == "%s" group by observed;',
		molecule, input$targetFile))
	dbDisconnect(db)
	actualize$graph <- if(actualize$graph) FALSE else TRUE
	hide('loader')
	shinyjs::show('app-content')
	print(list(row=cell$C-8, column=cell$Cl-3, ppm=res$ppmDeviation, score=res$score, auc=res$sumOfAuc))
	session$sendCustomMessage('targetTableSelect', list(row=cell$C-8, column=cell$Cl-3, ppm=res$ppmDeviation, score=res$score, auc=res$sumOfAuc))
})

deleteTargetROI <- function(file, adduct, C, Cl){
	db <- dbConnect(SQLite(), sqlitePath)
	queries <- c(
		sprintf('delete from measured where observed == (select id from observed where sample == "%s" and molecule == (select id from molecule where adduct == "%s" and formula like "%s"));',
		file, adduct, paste('%C', C, '%Cl', Cl, '%', sep='', collapse='')),
		sprintf('delete from observed where sample == "%s" and molecule == (select id from molecule where adduct == "%s" and formula like "%s");',
		file, adduct, paste('%C', C, '%Cl', Cl, '%', sep='', collapse='')))
	dbSendQueries(db, queries)
	dbDisconnect(db)
}

observeEvent(input$targetDelete, {
	cell <- input$targetTable_cell_selected
	deleteTargetROI(input$targetFile, input$targetAdduct, cell$C, cell$Cl)
	actualize$graph <- if(actualize$graph) FALSE else TRUE
})

# ------------------------------------------------------------------------------
# --------------------CORRELATION-----------------------------------------------
# ------------------------------------------------------------------------------
output$uiCorrelationStandardFile <- renderUI({
	files <- allFiles()
	files <- files[which(files$standard == 1), ]
	selectInput('correlationStandardFile', 'Select standard(s)', choices=files$sample, multiple=TRUE)
})

output$uiCorrelationFile <- renderUI({
	files <- allFiles()
	files <- files[which(files$standard == 0), ]
	selectInput('correlationFile', 'Select sample', choices=files$sample, multiple=FALSE)
})

output$correlationGraph <- renderPlotly({
	graph <- plot_ly(type='scatter', mode='markers')
	standardFiles <- input$correlationStandardFile
	file <- input$correlationFile
	if(is.null(standardFiles) | is.null(file)) return(graph)
	if(length(standardFiles) == 0 | file == "") return(graph)
	graph <- graph %>% layout(xaxis=list(title=file), yaxis=list(title='Standard(s)'), showlegend=FALSE, title='Correlation')
	db <- dbConnect(SQLite(), sqlitePath)
#	dataStandard <- dbGetQuery(db, sprintf('select formula, auc as aucStandard from observed where sample in (%s);', paste(sapply(standardFiles, function(x) paste0("'", x, "'")), collapse=', ')))
	dataFile <- dbGetQuery(db, sprintf('select formula, auc as aucFile from observed where sample == "%s";', file))
	dataStandard <- dbGetQuery(db, sprintf('select formula, auc as aucStandard from observed where sample in (%s) and formula in (select formula from observed where sample == "%s");', paste(sapply(standardFiles, function(x) paste0("'", x, "'")), collapse=', '), file))
	data <- merge(dataStandard, dataFile, all=TRUE)
	data[is.na(data)] <- 0
	graph <- graph %>% add_trace(x=data$aucFile, y=data$aucStandard)	
	fit <- lm(aucStandard ~ aucFile, data)
	updateTextInput(session, 'correlationText', '', paste(coef(fit)))
	print(summary(fit))
	graph <- graph %>% add_lines(x=data$aucFile, y=fitted(fit))
})


# output$correlationResidues <- renderPlotly({
# graph <- plot_ly(type='scatter', mode='markers')
	# standardFiles <- input$correlationStandardFile
	# file <- input$correlationFile
	# if(is.null(standardFiles) | is.null(file)) return(graph)
	# if(length(standardFiles) == 0 | file == "") return(graph)
	# graph <- graph %>% layout(xaxis=list(title=file), yaxis=list(title='Residuals'), showlegend=FALSE, title='Residual')
	# db <- dbConnect(SQLite(), sqlitePath)
	# dataStandard <- dbGetQuery(db, sprintf('select formula, auc as aucStandard from observed where sample in (%s);', paste(sapply(standardFiles, function(x) paste0("'", x, "'")), collapse=', ')))
	# dataFile <- dbGetQuery(db, sprintf('select formula, auc as aucFile from observed where sample == "%s";', file))
	# data <- merge(dataStandard, dataFile, all=TRUE)
	# data[is.na(data)] <- 0
	# data <- data[which(data$aucStandard != 0 & data$aucFile != 0), ]
	# fit <- lm(aucStandard ~ aucFile, data)
	# graph <- graph %>% add_trace(x=data$aucFile, y=resid(fit))
# })

})