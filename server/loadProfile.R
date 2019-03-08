output$uiDbProfileSampleAdduct <- renderUI({
	data <- project_samples() %>% filter(project == input$project)
	if(nrow(data) > 0){
		choices <- paste(data$sample, data$adduct)
		choices <- setNames(data$project_sample, choices)
	} else choices <- c()
	pickerInput('dbProfileSample', 'sample', choices=choices, multiple=TRUE, options=list(`live-search`=TRUE))
})

getDbProfileContent <- function(project_samples=NULL){
	if(is.null(project_samples)) return(list())
	datas <- map(project_samples, function(project_sample) 
		dbGetQuery(db, sprintf('select C as x, Cl as y, sum(auc) as z from observed where project_sample == %s group by(formula);', 
		project_sample)))
	datas
}

getXlsxContent <- function(xlsxPaths=NULL){
	if(is.null(xlsxPaths)) return(list())
	datas <- list()
	for(xlsxPath in xlsxPaths){
		data <- read.xlsx(xlsxPath)
		data <- apply(data, c(1, 2), as.numeric) %>% data.frame
		colnames(data) <- c('x', 'y', 'z')
		datas <- datas %>% append(list(data))
	}
	datas
}

observeEvent(input$xlsxChoose,{
	print('------------- XLSX CHOOSE --------------')
	tryCatch({
	values$xlsxPath <- choose.files(caption="Select xlsx file", multi=TRUE, 
		filter=matrix(c("xlsx", "*.xls*"), 1, 2, byrow=TRUE))
	# test if a file was correctly give
	print(paste('xlsx paths choosen:', values$xlsxPath, collapse='\n'))
	if(length(values$xlsxPath) == 0) custom_stop('minor_error', 'incorrect path')
	values$xlsxPath <- gsub('\\\\', '/', values$xlsxPath)
	
	# test each file
	for(xlsxFile in values$xlsxPath){
		data <- read.xlsx(xlsxFile)
		if(nrow(data) == 0 | class(data) != 'data.frame') custom_stop('minor_error', 
			paste('invalid data in', xlsxFile))
		else if(ncol(data) > 3) custom_stop('minor_error', 
			paste('more than three column in', xlsxFile))
		else if(ncol(data) < 3) custom_stop('minor_error', 
			paste('less than three column in', xlsxFile))
		notNum <- which(!apply(data, c(1,2), is.numeric), arr.ind=TRUE) %>% data.frame
		if(nrow(notNum) > 0) custom_stop('minor_error', 
			paste('data on file', xlsxFile, 'is not numeric on `row/column`:', paste(apply(notNum, 1, function(x) 
				paste(x, collapse='/')), collapse=', ')))
		negative <- which(apply(data, c(1, 2), function(x) x < 0), arr.ind=TRUE) %>% data.frame
		if(nrow(negative) > 0) custom_stop('minor_error', 
			paste('data on file', xlsxFile, 'is negative on `row/column`:', paste(apply(negative, 1, function(x) 
				paste(x, collapse='/')), collapse=', ')))
	}	
	}, minor_error = function(e){
		print(paste(e$message))
		toastr_error(paste(e$message))
		values$xlsxPath <- NULL
	} , error = function(e){
		print(paste(e$message))
		sendSweetAlert(paste(e$message))
		values$xlsxPath <- NULL
	})
	print('------------- END XLSX CHOOSE --------------')
})

output$xlsxPath <- renderText({
	if(is.null(values$xlsxPath)) "No xlsx file(s) selected"
	else paste(basename(values$xlsxPath), collapse=', ')
})