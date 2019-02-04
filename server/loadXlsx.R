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
		values$C <- data[, 1]
		values$Cl <- data[, 2]
		values$profile <- data[, 3]
		values$zVal <- NULL
		values$triangles <- NULL
		notNum <- which(!apply(data, c(1, 2), is.numeric), arr.ind=TRUE) %>% data.frame
		if(nrow(notNum) > 0) custom_stop('minor_error', paste('data is not numeric on `row/column`:', 
			paste(apply(notNum, 1, function(x) paste(x, collapse='/')), collapse=', ')))
		negative <- which(!apply(data, c(1, 2), function(x) x < 0), arr.ind=TRUE) %>% data.frame
		if(nrow(notNum) > 0) custom_stop('minor_error', paste('data is negative on `row/column`:', 
			paste(apply(notNum, 1, function(x) paste(x, collapse='/')), collapse=', ')))
		data
	}, invalid = function(i){
		values$C <- NULL
		values$Cl <- NULL
		values$profile <- NULL
		print(i$message)
		data.frame(matrix(, nrow=0, ncol=3, dimnames=list(c(), c('C', 'Cl', 'Profile'))))
	}, minor_error = function(e){
		values$C <- NULL
		values$Cl <- NULL
		values$profile <- NULL
		print(e$message)
		toastr_error(e$message)
		data.frame(matrix(, nrow=0, ncol=3, dimnames=list(c(), c('C', 'Cl', 'Profile'))))
	}, error = function(e){
		values$C <- NULL
		values$Cl <- NULL
		values$profile <- NULL
		print(e$message)
		sendSweetAlert(e$message)
		data.frame(matrix(, nrow=0, ncol=3, dimnames=list(c(), c('C', 'Cl', 'Profile'))))
	})
	print('----------------- END XLSX CONTENT ---------------------')
	data
}, rownames=FALSE, selection='none', filter='top', extensions=c('Scroller'), options=list(
	dom='Bfrtip', scrollX=TRUE, scroller=TRUE, scrollY="400px"))