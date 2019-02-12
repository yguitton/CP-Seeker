output$uiDbProfileSample <- renderUI({
	if(is.null(input$project)) choices <- c()
	else if(input$project == "") choices <- c()
	else {
		db <- dbConnect(SQLite(), sqlitePath)
		choices <- dbGetQuery(db, sprintf('select sample from project_sample
			where project == "%s";', input$project))$sample
		dbDisconnect(db)
	}
	pickerInput('dbProfileSample', 'sample', choices=choices, options=list(`live-search`=TRUE))
})

output$uiDbProfileAdduct <- renderUI({
	if(is.null(input$project)) choices <- c()
	else if(is.null(input$dbProfileSample)) choices <- c()
	else if(input$project == '') choices <- c()
	else if(input$dbProfileSample == '') choices <- c()
	else {
		db <- dbConnect(SQLite(), sqlitePath)
		choices <- dbGetQuery(db, sprintf('select adduct from project_sample where 
			project == "%s" and sample == "%s";', input$project,
			input$dbProfileSample))$adduct
		dbDisconnect(db)
	}
	pickerInput('dbProfileAdduct', 'adduct', choices=choices)
})

getDbProfileContent <- function(project, sample, adduct){
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select * from observed where project_sample == (
		select project_sample from project_sample where project == "%s" and 
			sample == "%s" and adduct ==  "%s");', project,	sample, adduct)
	print(query)
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	
	data <- data %>% mutate(groupVal = paste(C, Cl)) %>%
		group_by(groupVal) %>% summarise(C=unique(C), Cl=unique(Cl), profile=sum(auc))
	data[, -1] %>% data.frame
}

output$dbProfileContent <- renderDataTable({
	print('----------------- DB PROFILE CONTENT ---------------------')
	print(list(project=input$project, sample=input$dbProfileSample,
		adduct=input$dbProfileAdduct))
	data <- tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project is not yet initialized')
		else if(is.null(input$dbProfileSample)) custom_stop('invalid', 'sample is not yet initialized')
		else if(is.null(input$dbProfileAdduct)) custom_stop('invalid', 'adduct is not yet initialized')
		else if(input$project == "") custom_stop('invalid', 'no project selected')
		else if(input$dbProfileSample == '') custom_stop('invalid', 'no sample selected')
		else if(input$dbProfileAdduct == '') custom_stop('invalid', 'no adduct selected')
		getDbProfileContent(input$project, input$dbProfileSample, input$dbProfileAdduct)
	}, invalid = function(i){
		print(i$message)
		data.frame(matrix(, nrow=0, ncol=3, dimnames=list(c(), c('C', 'Cl', 'Profile'))))
	}, error = function(e){
		print(e$message)
		sendSweetAlert(e$message)
		data.frame(matrix(, nrow=0, ncol=3, dimnames=list(c(), c('C', 'Cl', 'Profile'))))
	})
	print('----------------- END DB PROFILE CONTENT ---------------------')
	data
}, rownames=FALSE, selection='none', filter='top', extensions=c('Scroller'), options=list(
	dom='Bfrtip', scrollX=TRUE, scroller=TRUE, scrollY="400px", bFilter=FALSE))


observeEvent(input$xlsxChoose,{
	print('------------- XLSX CHOOSE --------------')
	tryCatch({
	values$xlsxPath <- choose.files(caption="Select xlsx file", multi=FALSE, 
		filter=matrix(c("xlsx", "*.xls*"), 1, 2, byrow=TRUE))
	# test if a file was correctly give
	print(paste('xlsx path choosen:', values$xlsxPath))
	if(length(values$xlsxPath) == 0) custom_stop('minor_error', 'incorrect path')
	else {
		# replace all "\\" by "/" (Windows...)
		values$xlsxPath <- gsub('\\\\', '/', values$xlsxPath)
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
	if(is.null(values$xlsxPath)) "No xlsx file selected"
	else basename(values$xlsxPath)
})

output$xlsxContent <- renderDataTable({
	print('----------------- XLSX CONTENT ---------------------')
	print(list(xlsxPath = values$xlsxPath))
	data <- tryCatch({
		if(is.null(values$xlsxPath)) custom_stop('invalid', 'no file selected')
		data <- read.xlsx(values$xlsxPath)
		if(nrow(data) == 0 | class(data) != "data.frame") custom_stop('minor_error', 'invalid data in this sheet')
		else if(ncol(data) > 3) custom_stop('minor_error', 'more than 3 column in the excel sheet')
		notNum <- which(!apply(data, c(1, 2), is.numeric), arr.ind=TRUE) %>% data.frame
		if(nrow(notNum) > 0) custom_stop('minor_error', paste('data is not numeric on `row/column`:', 
			paste(apply(notNum, 1, function(x) paste(x, collapse='/')), collapse=', ')))
		negative <- which(!apply(data, c(1, 2), function(x) x < 0), arr.ind=TRUE) %>% data.frame
		if(nrow(notNum) > 0) custom_stop('minor_error', paste('data is negative on `row/column`:', 
			paste(apply(notNum, 1, function(x) paste(x, collapse='/')), collapse=', ')))
		colnames(data) <- c('C', 'Cl', 'profile')
		data
	}, invalid = function(i){
		print(i$message)
		data.frame(matrix(, nrow=0, ncol=3, dimnames=list(c(), c('C', 'Cl', 'Profile'))))
	}, minor_error = function(e){
		print(e$message)
		toastr_error(e$message)
		data.frame(matrix(, nrow=0, ncol=3, dimnames=list(c(), c('C', 'Cl', 'Profile'))))
	}, error = function(e){
		print(e$message)
		sendSweetAlert(e$message)
		data.frame(matrix(, nrow=0, ncol=3, dimnames=list(c(), c('C', 'Cl', 'Profile'))))
	})
	print('----------------- END XLSX CONTENT ---------------------')
	data
}, rownames=FALSE, selection='none', filter='top', extensions=c('Scroller'), options=list(
	dom='Bfrtip', scrollX=TRUE, scroller=TRUE, scrollY="400px", bFilter=FALSE))