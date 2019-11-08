#  This file is part of the HaloSeeker software for analyzing LC-MS data
#  
#  Copyright (C) 2018  Sébastien HUTINET, Ronan CARIOU, Alexis LÉON, Julie HUREL, Yann GUITTON, Céline TIXIER, Catherine MUNSCHY, Jean-Philippe ANTIGNAC, Gaud DERVILLY-PINEL, Bruno LE BIZEC
#  
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the

#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <https://www.gnu.org/licenses/>.

output$uiFilesDB <- renderUI({
	selected <- tryCatch({
		if(length(input$project) == 0) custom_stop('invalid', 'input$project not initialized')
		project_samples() %>% filter(project == input$project) %>% pull(sample)
	}, invalid = function(i) c()
	, error = function(e){
		print('ERR uiFilesDB')
		print(e)
		toastr_error("Cannot display list of samples", paste(e$message))
		c()
	})
	pickerInput('filesDB', label = NULL, choices=samples()$sample, 
		selected=selected, multiple=TRUE, inline = FALSE, 
		options=list(`live-search` = TRUE, `actions-box` = TRUE))
})

actualize$fileDBAdd <- c()
# add a sample in project where the sample is already provide in the database
observeEvent(input$fileDBAdd, {
	print('############################################################')
	print('######################### FILE ADD FROM DB #################')
	print('############################################################')
	
	print(list(project = input$project, files = input$filesDB))
	tryCatch({
	args <- c("project")
	titles <- c('project selector')
	conditions <- c(length(input$project) == 0)
	messages <- c('No project selected')
	if(!inputsTest(args, conditions, titles, messages)) custom_stop('invalid', 'Invalid args')
	
	actualize$fileDBAdd <- c()
	fileAlreadyAdd <- project_samples() %>% 
		filter(project == input$project) %>% pull(sample) 
	toAdd <- input$filesDB[!input$filesDB %in% fileAlreadyAdd]
	toDelete <- fileAlreadyAdd[!fileAlreadyAdd %in% input$filesDB]
	
	print(list(fileAlreadyAdd=fileAlreadyAdd, toAdd=toAdd, toDelete=toDelete))
	deleteProjectSamples(project_samples() %>% 
		filter(sample %in% toDelete & project == input$project) %>% 
		pull(project_sample))
	if(length(toDelete) > 0) toastr_success(
		sprintf("Delete %s in project", paste(toDelete, collapse=', ')))
			
	if(length(toAdd) > 0){
		actualize$fileDBAdd <- toAdd
		showModal(modalDialog(title='', 
			DT::dataTableOutput('sampleIDDBTable') %>% withSpinner(), 
			footer=div(
				actionBttn('fileDBCancel', 'Cancel', style='stretch', color='warning'),
				actionBttn('fileDBAdd2', 'Valid', style='stretch', color='primary')
			)
		))
	} else {
		print('############################################################')
		print('######################### END FILE ADD FROM DB #############')
		print('############################################################')
	}
	actualize$project_samples <- TRUE
	}, invalid = function(i) NULL
	, error = function(e){
		print(e)
		sendSweetAlert("Cannot add sample to project", paste(e$message))
	})
})

observeEvent(input$fileDBCancel, {
	print('############################################################')
	print('######################### END FILE ADD FROM DB #############')
	print('############################################################')
	removeModal()
})

output$sampleIDDBTable <- DT::renderDataTable({
	tryCatch({
	files <- actualize$fileDBAdd
	if(length(files) == 0) stop('no files to import')
	data.frame(file=files, label = paste('<input type=\"text\" value=\"', files, 
		'\" maxlength=30 width=\"100%\" required>', sep=''))
	}, error = function(e){
		print('ERR sampleIDDBTable')
		print(list(files = actualize$fileDBAdd))
		print(paste(e))
		toastr_error("Cannot display table with all samples to import", paste(e$message))
		data.frame(matrix(, ncol=2, dimnames=list(c(), c('file', 'label'))))
	})
}, escape = FALSE, selection='none', rownames=FALSE, options=list(dom='frtip', bFilter=FALSE, ordering=FALSE, paging=FALSE))

observeEvent(input$sampleIDsDBValidBttn, {
	tryCatch({
	files <- actualize$fileDBAdd
	sampleIDs <- input$sampleIDsDBValues
	print(list(project = input$project, files = files, sampleIDs = sampleIDs, 
		polarity = input$filePolarity))
	if(any(str_detect(sampleIDs, specialChars))) custom_stop('invalid', 
		'Cannot contain any special character ([\\:*?\"<>|])')
	sapply(1:length(files), function(i) 
		recordProjectSample(files[i], input$project, sampleIDs[i]))
	removeModal()
	
	actualize$project_samples <- TRUE
	toastr_success(sprintf("Add %s in project", paste(files, collapse=', ')))
	
	}, invalid = function(i) toastr_error(paste(i$message))
	, error = function(e){
		print(e)
		sendSweetAlert("Cannot import samples in project", paste(e$message))
	})
	
	print('############################################################')
	print('######################### END FILE ADD FROM DB #############')
	print('############################################################')
})

   
shinyFileChoose(input, 'filesImport', roots=getVolumes(), 
	filetypes=c('mzML', 'mzXML', 'cdf', 'CDF', 'RAW', 'd', 'YEP', 'BAF', 'FID', 'WIFF', 'MGF'), 
	defaultRoot=names(getVolumes()()[1]))
 
observeEvent(parseFilePaths(getVolumes()(), input$filesImport), {
	tryCatch({
		files <- parseFilePaths(getVolumes()(), input$filesImport)
		if(nrow(files) == 0) custom_stop('invalid', 'no files selected')
		else if(files$name %>% str_replace_all('\\\\', '/') %>% 
			str_detect(specialChars) %>% any) custom_stop('minor_error', 
				'Filepaths cannot contain any special characters ([\\:*?\"<>|])')
		showModal(modalDialog(title='',
			column(style="padding-top: 2%;", width=8, offset=1, 
				prettyRadioButtons("filePolarity", label = ("Choose polarity of file(s):"), 
					choices = list("negative" = "negative", "positive" = "positive"), 
					selected = "negative", icon=icon('check'), bigger=TRUE, 
					status='primary', animation="jelly", inline=TRUE, fill=TRUE)
			),
			DT::dataTableOutput('sampleIDTable'),
			footer=div(
				actionBttn('fileCancel', 'Cancel', style='stretch', color='warning'),
				actionBttn('fileAdd', 'Valid', style='stretch', color='primary')
			)
		))
	}, invalid = function(i) NULL, 
	minor_error = function(e) toastr_error(paste(e$message)), 
	error = function(e){
		print('ERR parseFilePaths(getVolumes()(), input$filesImport)')
		print(e)
		sendSweetAlert("Cannot import files to database", paste(e$message))
	})
})

observeEvent(input$fileCancel, {
	removeModal()
})

output$sampleIDTable <- DT::renderDataTable({
	tryCatch({
	files <- parseFilePaths(getVolumes()(), input$filesImport)
	data.frame(file=files$name, 
		label = paste('<input type=\"text\" value=\"', 
			paste(input$filePolarity %>% str_trunc(3, ellipsis=""),
				file_path_sans_ext(files$name)) %>% str_trunc(30, ellipsis=""), 
			'\" maxlength=30 width=\"100%\" required>', sep=''))
	}, error = function(e){
		print('ERR sampleIDTable')
		print(paste(e))
		toastr_error("Cannot display table with all files to import", paste(e$message))
		data.frame(matrix(, ncol=2, dimnames=list(c(), c('file', 'label'))))
	})
}, escape = FALSE, selection='none', rownames=FALSE, options=list(dom='frtip', bFilter=FALSE, ordering=FALSE, paging=FALSE))

actualize$importationSuccess <- data.frame(file=c(), success=c())
observeEvent(input$fileCancel, {
	removeModal()
})

observeEvent(input$sampleIDsValidBttn, {
	print('############################################################')
	print('######################### IMPORT FILES #####################')
	print('############################################################')
	
	files <- parseFilePaths(getVolumes()(), input$filesImport)
	sampleIDs <- input$sampleIDsValues
	print(list(project = input$project, paths = files$datapath, sampleIDs = sampleIDs, 
		polarity = input$filePolarity))
	
	tryCatch({
		if(length(input$project) == 0) custom_stop('invalid', 'you must select a project')
		else if(any(str_detect(sampleIDs, specialChars))) custom_stop('invalid', 'label 
			cannot contain any special character ([\\:*?\"<>|])')
		
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
				project == input$project & sample == sampleName) %>% 
					nrow > 0) success[i] <- 'file already in project'
			else if(sampleName %in% samples()$sample) success[i] <- recordProjectSample(
				sampleName, input$project, sampleIDs[i])
			else success[i] <- conversion(input$project, files$datapath[i], 
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
			DT::dataTableOutput('importationSuccessTable') %>% withSpinner(), 
			footer=modalButton('Close')
		))
	}, invalid = function(i){
		print(i)
		toastr_error(paste(i$message))
	}, error = function(e){
		closeSweetAlert(session)
		print(e)
		sendSweetAlert("Cannot import files to database", paste(e$message))
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
	instrumentModel <- if(length(infos@instrumentModel) == 0) '' else infos@instrumentModel %>% str_replace_all("\\:*?\"<>|", " ")
	manufacturer <- if(length(infos@instrumentManufacturer) == 0) '' else infos@instrumentManufacturer %>% str_replace_all("\\:*?\"<>|", " ")
	softwareName <- if(length(infos@softwareName) == 0) '' else infos@softwareName %>% str_replace_all("\\:*?\"<>|", " ")
	softwareVersion <- if(length(infos@softwareVersion) == 0) '' else infos@softwareVersion %>% str_replace_all("\\:*?\"<>|", " ")
	ionSource <- if(length(infos@ionSource) == 0) '' else infos@ionSource %>% str_replace_all("\\:*?\"<>|", " ")
	analyzer <- if(length(infos@analyser) == 0) '' else infos@analyser %>% str_replace_all("\\:*?\"<>|", " ")
	detectorType <- if(length(infos@detectorType) == 0) '' else infos@detectorType %>% str_replace_all("\\:*?\"<>|", " ")
	rm(msFile)
	gc()
	
	if(!is.null(thermoFile)){
		txt <- readLines(thermoFile)
		resolution <- if(any(grepl('^Resolution', txt))) txt[which(
				grepl('^Resolution', txt))] %>% 
				str_extract('[[:digit:]]+[[[:punct:]]{1}[[:alpha:]]{1}][[:digit:]]*') %>% str_replace_all("\\:*?\"<>|", " ")
			else ''
		agcTarget <- if(any(grepl('^AGC target', txt))) txt[which(
				grepl('^AGC target', txt))] %>% 
				str_extract('[[:digit:]]+[[[:punct:]]{1}[[:alpha:]]{1}][[:digit:]]') %>% str_replace_all("\\:*?\"<>|", " ")
			else ''
		maximumIT <- if(any(grepl('^Maximum IT', txt))) txt[which(
				grepl('^Maximum IT', txt))] %>% 
				str_extract('[[:digit:]]+\\s[[:alpha:]]+') %>% str_replace_all("\\:*?\"<>|", " ")
			else ''
		numberOfScanRange <- if(any(grepl('^Number of scan ranges', txt))) txt[which(
				grepl('^Number of scan ranges', txt))] %>% 
				str_extract('[[:digit:]]+') %>% str_replace_all("\\:*?\"<>|", " ")
			else ''
		scanRange <- if(any(grepl('^Scan range', txt))) txt[which(
				grepl('^Scan range', txt))] %>% 
				str_extract('[[:digit:]]+\\sto\\s[[:digit:]]+\\sm/z') %>% str_replace_all("\\:*?\"<>|", " ")
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

