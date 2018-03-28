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
	if(input$projectAddFile == '') return(sendSweetAlert(session, title='You have to choose a project!', type='error'))
	hide('app-content')
	shinyjs::show('loader')
	print('IMPORT RAW')
	print(paste('input$projectAddFile:', input$projectAddFile))
	files <- parseFilePaths(getVolumes()(), input$fileImportRaw)
	print('files:')
	print(files)
	polarity <- 'negative'
	success <- c()
	fileAlreadyAdd <- samples()[which(samples()$project == input$projectAddFile), 'sample']
	withProgress(message='conversion', value=0, max=nrow(files), {
	for(i in 1:nrow(files)){
		name <- as.character(files[i, 'name'])
		path <- as.character(files[i, 'datapath'])
		incProgress(amount=0, detail=name)
		sample <- paste(file_path_sans_ext(name))
		if(sample %in% fileAlreadyAdd) success[i] <- paste(name, 'already imported in project')
		else if(sample %in% samples()$sample) success[i]<- paste(name, 'already in the database in the project', samples()[which(samples()$sample == name), 'project'])
		else{
			success <- msConvert(path, polarity, name=name, 
				success=success, project=input$projectAddFile)
			incProgress(amount=1)
		}
	}
	})
	actualize$samples <- TRUE
	hide('loader')
	shinyjs::show('app-content')
	sendSweetAlert(session, title=paste(success, collapse='; '), type='success')
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
			addFile(sample=paste(file_path_sans_ext(name)), 
				path=file.path(getwd(), dirOutput,  
					paste(file_path_sans_ext(name), '.mzXML', sep='')), 
				project=project, 
				txtFile=file.path(getwd(), dirOutput, 
					paste(file_path_sans_ext(name), '.txt', sep='')))
				return(c(success, paste(name, 'Sucess!')))
		}
		else{
			addFile(sample=paste(file_path_sans_ext(name)), 
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