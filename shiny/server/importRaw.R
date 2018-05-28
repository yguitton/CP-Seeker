shinyFileChoose(input, 'fileChooseRawModal', roots=getVolumes(), filetypes=c('raw'))

observeEvent(input$fileChooseRaw, {
	if(input$fileProject == '') return(sendSweetAlert(session, title='You have to choose a project!', type='error'))
	toggleModal(session, 'fileProjectRawModal')
	hide('app-content')
	shinyjs::show('loader')
	files <- parseFilePaths(getVolumes()(), input$fileChooseRawModal)
	polarity <- 'negative'
	success <- c()
	fileAlreadyAdd <- samples()[which(samples()$project == input$fileProject), 'sample']
	withProgress(message='conversion', value=0, max=nrow(files), {
	for(i in 1:nrow(files)){
		name <- as.character(files[i, 'name'])
		path <- as.character(files[i, 'datapath'])
		incProgress(amount=0, detail=name)
		sample <- paste(file_path_sans_ext(name))
		if(sample %in% fileAlreadyAdd) success[i] <- paste(name, 'already imported in project')
		else if(sample %in% samples()$sample) success[i]<- paste(name, 'already in the database in the project', samples()[which(samples()$sample == name), 'project'])
		else{
			success[i] <- paste(name, msConvert(path, polarity, name=name, 
				project=input$fileProject))
			incProgress(amount=1)
		}
	}
	})
	type <- if(TRUE %in% sapply(success, function(x) !grepl('imported', x))) 'warning'
		else 'success'
	actualize$samples <- TRUE
	print(success)
	hide('loader')
	shinyjs::show('app-content')
	sendSweetAlert(session, title='Conversion', text=paste(success, collapse='; '), type=type)
})

#the function for the conversion
msConvert <- function(path, polarity, name, project){
	tryCatch({
		query <- sprintf('""%s" "%s" -o "%s" --mzXML --32 --zlib --filter "peakPicking true 1" --filter "polarity %s""', 
			converter, path, file.path(getwd(), dirOutput), polarity) 
		shell(query)
		if(!file.exists(file.path(getwd(), dirOutput, paste(file_path_sans_ext(name), '.mzXML', sep='')))) return('failed (missing .dll?)')
		if(!file.info(path)$isdir){
			query <- sprintf('""%s" --scanTrailers "%s" > "%s""',
				thermo, path, file.path(getwd(), dirOutput, 
					paste(file_path_sans_ext(name), '.txt', sep='')))
			shell(query)
			txtFile <- file.path(getwd(), dirOutput, paste(file_path_sans_ext(name), '.txt', sep=''))
			if(!file.exists(txtFile)) txtFile <- NULL
			success <- addFile(sample=paste(file_path_sans_ext(name)), 
				path=file.path(getwd(), dirOutput,  
					paste(file_path_sans_ext(name), '.mzXML', sep='')), 
				project=project, txtFile=txtFile)
		}
		else{
			success <- addFile(sample=paste(file_path_sans_ext(name)), 
				path=file.path(getwd(), dirOutput, 
					paste(file_path_sans_ext(name), '.mzXML', sep='')), 
				project=project)
		}
		return(success)
	}, error=function(e){
		return(paste(e))
	})
}