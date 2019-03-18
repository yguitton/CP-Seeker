values$triangles <- NULL
values$zVals <- NULL
values$zones <- NULL

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
		datas <- map(datas, function(x) distMatrix(x, maxC, maxCl))
		updateProgressBar(session, id='pb', title='Tetrahedrization', value=0)
		triangles <- map(datas, triangulization)
		zVals <- c()
		for(i in 1:length(triangles)){
			pbVal <- i*100/length(triangles)
			updateProgressBar(session, id='pb', title='Tetrahedrization', value=pbVal)
			zVals <- c(zVals, getZCut(triangles[[i]], vTarget=input$vTarget, digits=input$vDigits, pbVal))
		}
		names(triangles) <- samplesComputed
		zones <- reduce(1:length(triangles), function(a, b) a %>% append(
			list(splitToZones(triangles[[b]], zVals[b]))), .init=list())
		zoneNames <- sapply(1:length(zones), function(i) 
			paste(samplesComputed[i], '- zone', 1:length(zones[[i]]))) %>% unlist
		zones <- unlist(zones, recursive=FALSE)
		names(zones) <- zoneNames
		
		values$zVals <- zVals
		values$triangles <- triangles
		values$zones <- zones	
		
		closeSweetAlert(session)
	}, invalid = function(i){
		closeSweetAlert(session)
		print(i$message)
		sendSweetAlert(i$message)
		values$triangles <- NULL
		values$zVals <- NULL
		values$zones <- NULL
	}, error = function(e){
		closeSweetAlert(session)
		print(e$message)
		sendSweetAlert(e$message)
		values$triangles <- NULL
		values$zVals <- NULL
		values$zones <- NULL
	})
	print('---------------------------- END PROFILE LAUNCH -------------------')
})

output$uiTetrasSample <- renderUI({
	choices <- if(!is.null(values$triangles)) setNames(1:length(values$triangles), names(values$triangles)) else c()
	pickerInput('tetrasSample', 'sample', choices=choices, multiple=FALSE, width='50%')
})

output$tetrahedras <- renderPlotly({
	if(is.null(input$tetrasSample)) drawTriCut(maxC=maxC, maxCl=maxCl)
	else if(input$tetrasSample == '') drawTriCut(maxC=maxC, maxCl=maxCl)
	else drawTriCut(values$triangles[[input$tetrasSample %>% as.numeric]], 
		values$zVals[[input$tetrasSample %>% as.numeric]], maxC, maxCl)
})

output$map <- renderPlotly({
	contourPolyhedras(values$zones, maxC=maxC, maxCl=maxCl)
})

output$profilesScores <- renderDataTable({
	if(is.null(values$zones)) return(data.frame())
	scores <- proxy::dist(values$zones, method=function(x, y) scoreZones(x, y))
	scores <- scores %>% as.matrix
	rownames(scores) <- names(values$zones)
	colnames(scores) <- names(values$zones)
	apply(scores, c(1, 2), function(x)
		if(x == 0) NA else paste(round(x), '%'))
}, selection='none', extension='Scroller', options=list(
	info=FALSE, pagin=FALSE, dom='frtip', scroller=TRUE, scrollX=TRUE, 
	bFilter=FALSE, ordering=FALSE))

