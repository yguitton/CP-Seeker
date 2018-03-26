shinyFileChoose(input, 'fileImportmzXML', roots=getVolumes(), filetypes=c('mzML', 'mzXML'))
observeEvent(input$fileImportmzXML, {
	showModal(modalDialog(easyClose=TRUE, title='Choose a project',
		uiOutput('selectProjectAddFile'),
		footer=actionButton('filemzXMLAdd', 'Valid')
	))
})

observeEvent(input$filemzXMLAdd, {
	removeModal()
	if(input$projectAddFile == '') return(sendSweetAlert(session, title='You have to choose a project!', type='error'))
	hide('app-content')
	shinyjs::show('loader')
	print('IMPORT MZXML')
	print(paste('input$projectAddFile:', input$projectAddFile))
	files <- parseFilePaths(getVolumes(), input$fileImportmzXML)
	print('files:')
	print(files)
	fileAlreadyAdd <- samples()[which(samples()$project == input$projectAddFile), 'sample']
	success <- c()
	withProgress(message='importation', value=0, max=nrow(files), {
	for(i in 1:nrow(files)){
		name <- as.character(files[i, 'name'])
		path <- as.character(files[i, 'datapath'])
		incProgress(amount=0, detail=name)
		xml_data <- xmlToList(path)
		print('polarity in file:')
		print(xml_data$msRun)
		polarity <- if(is.null(xml_data$msRun)) 'positive' else if(xml_data$msRun$scan$.attrs['polarity'] == '-') 'negative' else 'positive'
		sample <- paste(polarity, file_path_sans_ext(name))
		if(sample %in% fileAlreadyAdd) success[i] <- paste(name, 'already imported in project')
		else if(sample %in% samples()$sample){
			success[i]<- paste(name, 'already in the database in the project', samples()[which(samples()$sample == name), 'project'])
		}
		else{
			file.copy(path, file.path(dirOutput, polarity, name))
			addFile(sample=sample, path=file.path(dirOutput, polarity, name), project=input$projectAddFile, polarity=polarity)
			success[i] <- paste(name, 'Success!')
		}
		incProgress(1)
	}})
	actualize$samples <- TRUE
	hide('loader')
	shinyjs::show('app-content')
	sendSweetAlert(session, title=paste(success, collapse='; '), type='success')
})