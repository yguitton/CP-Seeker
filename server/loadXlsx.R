observeEvent(input$xlsxChoose,{
	print('------------- XLSX CHOOSE --------------')
	tryCatch({
	values$xlsxPath <- choose.files(caption="Select xlsx file", multi=FALSE, 
		filter=matrix(c("Excel file", "*.xlsx", "Excel file", "*.xlsm"), 2, 2, byrow=TRUE))
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

output$uiXlsxTab <- renderUI({
	print('------------------ XLSX TAB -------------------')
	print(list(xlsxPath = values$xlsxPath))
	sheets <- tryCatch({
	if(is.null(values$xlsxPath)) custom_stop('invalid', 'no file selected')
	sheets <- getSheetNames(values$xlsxPath)
	}, invalid = function(i){
		print(i$message)
		NULL
	}, error = function(e){
		print(e$message)
		sendSweetAlert(e$message)
		NULL
	})
	print('------------------ END XLSX TAB -------------------')
	pickerInput('xlsxTab', label=NULL, choices=sheets)
})

xlsxContent <- reactive({
	print('----------------- XLSX CONTENT ---------------------')
	print(list(xlsxPath = values$xlsxPath, sheet = input$xlsxTab))
	data <- tryCatch({
		if(is.null(values$xlsxPath)) custom_stop('invalid', 'no file selected')
		else if(is.null(input$xlsxTab)) custom_stop('invalid', 'no tab selected')
		sheets <- getSheetNames(values$xlsxPath)
		if(input$xlsxTab %!in% sheets) custom_stop('invalid', "sheet selected not in sheet's file")
		data <- read.xlsx(values$xlsxPath, input$xlsxTab)
		if(nrow(data) == 0 | class(data) != "data.frame") custom_stop('minor_error', 'invalid data in this sheet')
		data
	}, invalid = function(i){
		print(i$message)
		data.frame(matrix(, nrow=0, ncol=0))
	}, minor_error = function(e){
		print(e$message)
		toastr_error(e$message)
		data.frame(matrix(, nrow=0, ncol=0))
	}, error = function(e){
		print(e$message)
		sendSweetAlert(e$message)
		data.frame(matrix(, nrow=0, ncol=0))
	})
	print('----------------- END XLSX CONTENT ---------------------')
	data
})

output$xlsxContent <- renderDataTable(xlsxContent(), selection=list(target="column", mode="multiple"), 
filter='top', extensions=c('Scroller'), options=list(
	dom='Bfrtip', scrollX=TRUE, scroller=TRUE, scrollY="400px"), callback=htmlwidgets::JS("
	table.on('click', 'tbody td', function(){
		var cell = table.cell(this).index();
		// zzz is just to force the shiny observer
		if($(table.cell(this).node()).hasClass('selected')) Shiny.onInputChange('xlsxColumn_remove', 
			{zzz: Math.random(), id: cell.column});
		else{
			// check if the number of columns >= 3
			var nbCol = table.$('td.selected').length / table.rows().data().length;
			if(nbCol >= 3){
				toastr['error']('cannot select more than 4 column!');
				e.stopImmediatePropagation();
			} else Shiny.onInputChange('xlsxColumn_add', {zzz: Math.random(), id: cell.column});
		}
	});
"))

observeEvent(input$xlsxColumn_add, {
	showModal(chooseXlsxTag())
})

chooseXlsxTag <- function(){
	modalDialog(title='Choose the tag in the list',
		uiOutput('uiXlsxTag'),
		footer = actionBttn('xlsxTagValid', label="Valid")
	)
}

values$C <- list(id=0, data=NULL)
values$Cl <- list(id=0, data=NULL)
values$profile <- list(id=0, data=NULL)

output$uiXlsxTag <- renderUI({
	choices <- c('C', 'Cl', 'profile')
	toKeep <- which(sapply(list(values$C, values$Cl, values$profile), 
		function(x) x$id == 0))
	pickerInput('xlsxTag', '', choices=choices[toKeep])
})

observeEvent(c(values$xlsxPath, input$xlsxTab), {
	values$C <- list(id=0, data=NULL)
	values$Cl <- list(id=0, data=NULL)
	values$profile <- list(id=0, data=NULL)
})

observeEvent(input$xlsxColumn_remove, {
	print('------------------ XLSX TAG REMOVE ---------------')
	print(list(file=values$xlsxPath, sheet=input$xlsxTab, 
		column=input$xlsxColumn_remove$id))
	tryCatch({
		column <- input$xlsxColumn_remove$id
		id <- which(sapply(list(values$C, values$Cl, values$profile), 
			function(x) x$id == column))
		if(length(id) > 1) stop('the column is referred multiple times ???')
		# delete the tag referring to the column
		if(id == 1) values$C <- list(id=0, data=NULL)
		else if(id == 2) values$Cl <- list(id=0, data=NULL)
		if(id == 3) values$profile <- list(id=0, data=NULL)
	}, error = function(e){
		print(e$message)
		sendSweetAlert(e$message)
	})
	print('------------------ END XLSX TAG REMOVE ---------------')
})

observeEvent(input$xlsxTagValid, {
	print('-------------- XLSX TAG VALID -----------------')
	print(list(file=values$xlsxPath, sheet=input$xlsxTab, 
		column=input$xlsxColumn_add$id, tag=input$xlsxTag))
	removeModal()
	tryCatch({
		column <- input$xlsxColumn_add$id
		data <- xlsxContent()[, column]
		if(column == 0) stop("column doesn't exist !!!!")
		else if(length(data) == 0) stop('no data on column !!!')
		else if(!input$xlsxTag %in% c('C', 'Cl', 'profile')) stop("tag doesn't exist !!!")
		
		data <- data %>% as.numeric
		if(any(is.na(data))) custom_stop('minor_error', 
			paste('values are not numeric in rows:', paste(which(is.na(data)), collapse=', ')))
		else if(any(any(data < 0))) custom_stop('minor_error', paste(
			'values cannot be negative in rows:', paste(which(data < 0), collapse=', ')))
		
		if(input$xlsxTag == "C") values$C <- list(id=column, data=data)
		else if(input$xlsxTag == "Cl") values$Cl <- list(id=column, data=data)
		else if(input$xlsxTag == "profile") values$profile <- list(id=column, data=data)
		
		toastr_success(paste(input$xlsxTag, "loaded"))
	}, minor_error = function(e){
		print(e$message)
		toastr_error(e$message)
		if(input$xlsxTag == "C") values$C <- list(id=0, data=NULL)
		else if(input$xlsxTag == "Cl") values$Cl <- list(id=0, data=NULL)
		else if(input$xlsxTag == "profile") values$profile <- list(id=0, data=NULL)
		else{
			mes <- "don't recognize the tag!!!"
			print(mes)
			toastr_error(mes)
			sendSweetAlert(mes)
		}
	}, error = function(e){
		print(e$message)
		sendSweetAlert(e$emessage)
		if(input$xlsxTag == "C") values$C <- list(id=0, data=NULL)
		else if(input$xlsxTag == "Cl") values$Cl <- list(id=0, data=NULL)
		else if(input$xlsxTag == "profile") values$profile <- list(id=0, data=NULL)
		else{
			mes <- "don't recognize the tag!!!"
			print(mes)
			toastr_error(mes)
			sendSweetAlert(mes)
		}
	})
	print('-------------- END XLSX TAG VALID -----------------')
})
