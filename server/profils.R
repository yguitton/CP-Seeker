values$triangles <- NULL
values$zVal <- NULL
values$centroid <- NULL

observeEvent(input$dbProfileLaunch, {
	print('------------------------- DB PROFILE TETRAHEDRIZATION ------------------')
	print(list(project=input$project, sample=input$dbProfileSample,
		adduct=input$dbProfileAdduct))
	progressSweetAlert(session, 'pb', title="Initialization", 100, striped=TRUE)
	tryCatch({
		if(is.null(input$project)) custom_stop('minor_error', 'you must select a project')
		else if(input$project == "") custom_stop('minor_error', 'you must select a project')
		else if(is.null(input$dbProfileSample)) custom_stop('minor_error', 'you must select a sample')
		else if(input$dbProfileSample == "") custom_stop('minor_error', 'you must select a sample')
		else if(is.null(input$dbProfileAdduct)) custom_stop('minor_error', 'you must select an adduct')
		else if(input$dbProfileAdduct == '') custom_stop('minor_error', 'you must select an adduct')
		data <- getDbProfileContent(input$project, input$dbProfileSample, input$dbProfileAdduct)
		
		updateProgressBar(session, id="pb", title="Compute distance matrix", value=0)
		values$centroid <- data.frame(x=data$C, y=data$Cl, z=data$profile) %>% 
			summarise(x=sum(x*z), y=sum(y*z))
		data <- distMatrix(data)
		updateProgressBar(session, id="pb", title="Tetrahedrization", value=100)
		res <- tetrahedrization(data)
		tetras <- res$tetras
		triangles <- res$triangles
		print(paste('Volume :', computeVolumePolyhedra(tetras)))
		zVal <- getZCut(tetras, input$vTarget, input$vDigits)
		print(paste('Zcut :', zVal))
		
		values$triangles <- triangles
		values$zVal <- zVal
		
		closeSweetAlert(session)
	}, minor_error = function(e){
		closeSweetAlert(session)
		print(e$message)
		toastr_error(e$message)
		values$triangles <- NULL
		values$zVal <- NULL
	}, error = function(e){
		closeSweetAlert(session)
		print(e$message)
		sendSweetAlert(e$message)
		values$triangles <- NULL
		values$zVal <- NULL
	})
	print('------------------------- END DB PROFILE TETRAHEDRIZATION ------------------')
})

observeEvent(input$xlsxLaunch, {
	print('--------------------- XLSX TETRAHEDRIZATION ----------------------')
	print(list(C=values$C, Cl=values$Cl, profile=values$profile))
	progressSweetAlert(session, 'pb', title="Initialization", 100, striped=TRUE)
	tryCatch({
		if(is.null(values$xlsxPath)) custom_stop('²', 'no file selected')
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
		
		updateProgressBar(session, id="pb", title="Compute distance matrix", value=0)
		values$centroid <- data.frame(x=data$C, y=data$Cl, z=data$profile) %>% 
			summarise(x=sum(x*z), y=sum(y*z))
		data <- distMatrix(data)
		updateProgressBar(session, id="pb", title="Tetrahedrization", value=100)
		res <- tetrahedrization(data)
		tetras <- res$tetras
		triangles <- res$triangles
		print(paste('Volume :', computeVolumePolyhedra(tetras)))
		zVal <- getZCut(tetras, input$vTarget, input$vDigits)
		print(paste('Zcut :', zVal))
		
		values$triangles <- if(input$tetraSmooth) triangles %>% 
				modify(function(a) a %>% mutate_at(c('x', 'y'), function(.) ./2))
			else triangles
		values$zVal <- zVal
		
		closeSweetAlert(session)
	}, minor_error = function(e){
		closeSweetAlert(session)
		print(e$message)
		toastr_error(e$message)
		values$triangles <- NULL
		values$zVal <- NULL
	}, error = function(e){
		closeSweetAlert(session)
		print(e$message)
		sendSweetAlert(e$message)
		values$triangles <- NULL
		values$zVal <- NULL
	})
	print('--------------------- END TETRAHEDRIZATION ----------------------')
})

output$tetrahedras <- renderPlotly(drawTriCut(values$triangles, values$zVal))

output$map <- renderPlotly(contourPolyhedras(values$triangles, values$zVal, 
	values$centroid))

