shinyFileChoose(input, 'fileImportmzXML', roots=getVolumes(), filetypes=c('mzML', 'mzXML'))

observeEvent(input$fileImportmzXML, {
	showModal(modalDialog(easyClose=FALSE, title='Choose a project',
		uiOutput('uiFileProject'),
		footer=actionBttn('filemzXMLAdd', 'Valid', style='stretch', color='primary')
	))
})

observeEvent(input$filemzXMLAdd, {
	if(input$fileProject == '') return(sendSweetAlert(session, title='You have to choose a project!', type='error'))
	removeModal()
	hide('app-content')
	show('loader')
	tryCatch({
	files <- parseFilePaths(getVolumes(), input$fileImportmzXML)
	fileAlreadyAdd <- samples()[which(samples()$project == input$fileProject), 'sample']
	success <- c()
	withProgress(message='importation', value=0, max=nrow(files), {
	for(i in 1:nrow(files)){
		name <- as.character(files[i, 'name'])
		path <- as.character(files[i, 'datapath'])
		incProgress(amount=0, detail=name)
		success[i] <- recordmzXML(name, path, fileAlreadyAdd, samples()$sample, input$fileProject)
		incProgress(1)
	}})
	actualize$samples <- TRUE
	type <- if(TRUE %in% sapply(success, function(x) !grepl('imported', x))) 'warning'
		else 'success'
	hide('loader')
	shinyjs::show('app-content')
	print(success)
	sendSweetAlert(session, title='Importation', text=HTML(paste(success, collapse='\n'), type=type))
	}, error=function(e){
		actualize$samples <- TRUE
		hide('loader')
		shinyjs::show('app-content')
		print(e)
		return(sendSweetAlert(session, title='Importation failed', text=paste(e), type='error'))
	})
})

recordmzXML <- function(name, path, inProject, inDB, project){
	# get info about file
	file <- readMSData(path, msLevel=1, mode='onDisk')
	if(FALSE %in% isCentroided(file)) return(paste(name, 'is not centroided'))
	# get the polarity
	polarity <- unique(polarity(file))
	print(polarity)
	if(length(polarity) > 1){
		# cut the file
		print('cut file')
		negative_spectras <- which(polarity(file) == 0)
		tmpdir <- tempdir(check=TRUE)
		writeMSData(file[negative_spectras], file.path(tmpdir, name), outformat='mzxml', copy=TRUE)
		success <- recordmzXML2(name, file.path(tmpdir, name), inProject, inDB, project)
		file.remove(file.path(tmpdir, name))
	}
	else if(polarity == 1) return(paste(name, 'is in positive'))
	else success <- recordmzXML2(name, path, inProject, inDB, project)
	return(paste(name, success))
}

recordmzXML2 <- function(name, path, inProject, inDB, project){
	polarity <- 'negative'
	sample <- file_path_sans_ext(name)
	if(sample %in% inProject) return('imported')
	else if(sample %in% inDB) return('already in an another project')
	success <- addFile(sample, path, project)
	if(success == 'imported') file.copy(path, file.path(dirOutput, name))
	return(success)
}