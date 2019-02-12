values$triangles <- NULL
values$zVal <- NULL
values$centroid <- NULL

observeEvent(input$dbProfileLaunch, {
	print('------------------------- DB PROFILE TETRAHEDRIZATION ------------------')
	print(list(project=input$project, sample=input$dbProfileSample,
		adduct=input$dbProfileAdduct))
	progressSweetAlert(session, 'pb', title="Initialization", 100, striped=TRUE)
	tryCatch({
		inputs <- c('project', 'dbProfileSample', 'dbProfileAdduct')
		conditions <- c(is.null(input$project), is.null(input$dbProfileSample), 
			is.null(input$dbProfileAdduct))
		messages <- c('You must select a project', 'You must select a sample',
			'You must select an adduct')
		test <- inputsTest(inputs, conditions, messages)
		if(!test) custom_stop('minor_error', 'invalid params')
		inputs <- c('project', 'dbprofileSample', 'dbProfileAdduct', 'vTarget', 'vDigits')
		conditions <- c(input$project == "", input$dbProfileSample == "", input$dbProfileAdduct == "",
			!is.numeric(input$vTarget), !is.numeric(input$vDigits))
		messages <- c(messages, 'target volume must be a number', 'precision must be a number')
		test <- inputsTest(inputs, conditions, messages)
		if(!test) custom_stop('invalid', 'invalid args')
		
		data <- getDbProfileContent(input$project, input$dbProfileSample, input$dbProfileAdduct)
		
		updateProgressBar(session, id="pb", title="Compute distance matrix", value=0)
		values$centroid <- data.frame(x=data$C, y=data$Cl, z=data$profile) %>% 
			summarise(x=sum(x*z/sum(z)), y=sum(y*z/sum(z)))
		data <- distMatrix(data)
		updateProgressBar(session, id="pb", title="Tetrahedrization", value=100)
		res <- tetrahedrization(data)
		tetras <- res$tetras
		triangles <- res$triangles
		print(paste('Volume :', computeVolumePolyhedra(tetras)))
		zVal <- getZCut(tetras, input$vTarget, input$vDigits)
		print(paste('Zcut :', zVal))
		
		values$triangles <- reduce(triangles, function(a, b) a %>% 
			append(splitTri(b)), .init=list())
		values$zVal <- zVal
		
		closeSweetAlert(session)
	}, minor_error = function(e){
		closeSweetAlert(session)
		print(e$message)
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
		inputs <- c('xlsxPath', 'vTarget', 'vDigits')
		conditions <- c(is.null(values$xlsxPath), !is.numeric(input$vTarget), !is.numeric(input$vDigits))
		messages <- c('no excel file selected', 'target volume must be a number',
			'precision must be a number')
		test <- inputsTest(inputs, conditions, messages)
		if(!test) custom_stop('invalid', 'invalid params')
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
			summarise(x=sum(x*z/sum(z)), y=sum(y*z/sum(z)))
		data <- distMatrix(data)
		updateProgressBar(session, id="pb", title="Tetrahedrization", value=100)
		res <- tetrahedrization(data)
		tetras <- res$tetras
		triangles <- res$triangles
		print(paste('Volume :', computeVolumePolyhedra(tetras)))
		zVal <- getZCut(tetras, input$vTarget, input$vDigits)
		print(paste('Zcut :', zVal))
		
		values$triangles <- reduce(triangles, function(a, b) a %>% 
			append(splitTri(b)), .init=list())
		values$zVal <- zVal
		
		closeSweetAlert(session)
	}, invalid = function(i){
		closeSweetAlert(session)
		print(i$message)
		values$triangles <- NULL
		values$zVal <- NULL
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

