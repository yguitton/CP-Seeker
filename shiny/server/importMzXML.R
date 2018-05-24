shinyFileChoose(input, 'fileChoosemzXMLModal', roots=getVolumes(), filetypes=c('mzML', 'mzXML'))

observeEvent(input$fileChoosemzXML, {
	if(input$fileProject == '') return(sendSweetAlert(session, title='You have to choose a project!', type='error'))
	toggleModal(session, 'fileProjectmzXMLModal')
	hide('app-content')
	show('loader')
	files <- parseFilePaths(getVolumes(), input$fileChoosemzXMLModal)
	fileAlreadyAdd <- samples()[which(samples()$project == input$fileProject), 'sample']
	success <- c()
	withProgress(message='importation', value=0, max=nrow(files), {
	for(i in 1:nrow(files)){
		name <- as.character(files[i, 'name'])
		path <- as.character(files[i, 'datapath'])
		incProgress(amount=0, detail=name)
		xml_data <- xmlToList(path)
		sample <- paste(file_path_sans_ext(name))
		if(sample %in% fileAlreadyAdd) success[i] <- paste(name, 'already imported in project')
		else if(sample %in% samples()$sample){
			success[i]<- paste(name, 'already in the database in the project', samples()[which(samples()$sample == name), 'project'])
		}
		else{
			file.copy(path, file.path(dirOutput, name))
			success[i] <- paste(name, addFile(sample=sample, path=file.path(dirOutput, name), project=input$fileProject))
		}
		incProgress(1)
	}})
	actualize$samples <- TRUE
	print(success)
	type <- if(TRUE %in% sapply(success, function(x) !grepl('imported', x))) 'warning'
		else 'success'
	hide('loader')
	shinyjs::show('app-content')
	sendSweetAlert(session, title='Importation', text=paste(success, collapse='; '), type=type)
})