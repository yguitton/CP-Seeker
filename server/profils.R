values$triangles <- NULL
values$zVals <- NULL

observeEvent(input$profileLaunch, {
	print('---------------------------- PROFILE LAUNCH -------------------')
	print(list(from_db = input$dbProfileSample,
		from_xlsx = values$xlsxPath, vTarget=input$vTarget,
		precision=input$vDigits))
	progressSweetAlert(session, 'pb', title='Initialization', 0, striped=TRUE, display_pct=TRUE)
	tryCatch({
		if(!is.numeric(input$vTarget)) custom_stop('invalid', 'target volume must be a number')
		else if(!is.numeric(input$vDigits)) custom_stop('invalid', 'precision must be a number')
		
		samplesComputed <- if(length(input$dbProfileSample) > 0) project_samples() %>% filter(project_sample %in% input$dbProfileSample) %>%
			select(sample, adduct) %>% apply(1, function(x) paste(x, collapse=' ')) else c()
		if(!is.null(values$xlsxPath)) samplesComputed <- c(samplesComputed, basename(values$xlsxPath))

		datas <- getDbProfileContent(input$dbProfileSample)
		datas <- append(datas, getXlsxContent(values$xlsxPath))
		samplesComputed <- samplesComputed[which(sapply(datas, function(x) nrow(x) > 0))]
		datas <- keep(datas, function(x) nrow(x) > 0)
		if(length(datas) == 0) custom_stop('invalid', 'no profile to compute')
		
		updateProgressBar(session, id='pb', title='Compute distance matrix', value=0)
		datas <- map(datas, distMatrix)
		updateProgressBar(session, id='pb', title='Tetrahedrization', value=0)
		triangles <- map(datas, triangulization)
		values$zVals <- c()
		for(i in 1:length(triangles)){
			pbVal <- i*100/length(triangles)
			updateProgressBar(session, id='pb', title='Tetrahedrization', value=pbVal)
			values$zVals <- c(values$zVals, getZCut(triangles[[i]], vTarget=input$vTarget, digits=input$vDigits, pbVal))
		}
		values$triangles <- triangles
		names(values$triangles) <- samplesComputed
		
		closeSweetAlert(session)
	}, invalid = function(i){
		closeSweetAlert(session)
		print(i$message)
		sendSweetAlert(i$message)
		values$triangles <- NULL
		values$zVal <- NULL
	}, error = function(e){
		closeSweetAlert(session)
		print(e$message)
		sendSweetAlert(e$message)
		values$triangles <- NULL
		values$zVal <- NULL
	})
	print('---------------------------- END PROFILE LAUNCH -------------------')
})

output$uiTetrasSample <- renderUI({
	choices <- if(!is.null(values$triangles)) setNames(1:length(values$triangles), names(values$triangles)) else c()
	pickerInput('tetrasSample', 'sample', choices=choices, multiple=FALSE, width='50%')
})

output$tetrahedras <- renderPlotly({
	if(is.null(input$tetrasSample)) drawTriCut()
	else if(input$tetrasSample == '') drawTriCut()
	else drawTriCut(values$triangles[[input$tetrasSample %>% as.numeric]], 
		values$zVals[[input$tetrasSample %>% as.numeric]])
})

output$map <- renderPlotly({
	contourPolyhedras(values$triangles, values$zVals)
})

