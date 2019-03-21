actualize$tetrahedrization <- FALSE

observeEvent(input$profileLaunch, {
	print('---------------------------- PROFILE LAUNCH -------------------')
	print(list(from_db = input$dbProfileSample,
		from_xlsx = values$xlsxPath, vTarget=input$vTarget,
		precision=input$vDigits))
	progressSweetAlert(session, 'pb', title='Initialization', 0, striped=TRUE, display_pct=TRUE)
	tryCatch({
		if(!is.numeric(input$vTarget)) custom_stop('invalid', 'target volume must be a number')
		else if(!is.numeric(input$vDigits)) custom_stop('invalid', 'precision must be a number')
		else if(length(input$dbProfileSample) == 0 & length(values$xlsxPath) == 0) custom_stop('invalid', 
			'no profile to compute')
			
		query <- sprintf('delete from triangle where project_sample in (%s); 
			update project_sample set zVal = null where project_sample in (%s);',
			paste(input$dbProfileSample, collapse=','),
			paste(input$dbProfileSample, collapse=','))
		print(query)
		dbSendQuery(db, query)
		
		datas <- getDbProfileContent(input$dbProfileSample)
		# datas <- append(datas, getXlsxContent(values$xlsxPath))
		
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
		updateProgressBar(session, id='pb', title='Split into zone(s)', value=100)
		print(zVals)
		
		lapply(1:length(zVals), function(i) dbSendQuery(db, sprintf(
			'update project_sample set zVal = %s where project_sample == %s;',
			zVals[i], input$dbProfileSample[i])))
		
		project_samples <- input$dbProfileSample[which(
			lengths(triangles) > 0)]
		triangles <- triangles[which(lengths(triangles) > 0)]
		if(length(triangles) > 0){
			zones <- map(1:length(triangles), function(i) 
				splitToZones(triangles[[i]], zVals[[i]]))
			edges <- reduce(1:length(zones), function(i, j) i %>% 
				rbind(reduce(1:length(zones[[j]]), function(k, l) k %>% 
					rbind(reduce(zones[[j]][[l]], rbind) %>% 
						cbind(zone=l)), .init=data.frame()) %>% 
				cbind(project_sample=project_samples[j])), .init=data.frame())
			edges$triangle <- rep(1:(nrow(edges)/3), each=3)
			
			query <- sprintf('insert into triangle (x, y, z, triangle, project_sample, zone) values (%s);', 
				paste(edges$x, edges$y, edges$z, edges$triangle, edges$project_sample, edges$zone, sep=', ', collapse='), ('))
			dbSendQuery(db, query)
		}
	
		closeSweetAlert(session)
	}, invalid = function(i){
		closeSweetAlert(session)
		print(i$message)
		toastr_error(i$message)
	}, error = function(e){
		closeSweetAlert(session)
		print(e$message)
		dbSendQuery(db, paste(sprintf('update project_sample set zVal = null where
				project_sample in (%s);', paste(input$dbProfileSample, collapse=', ')),
			sprintf('delete from triangle where project_sample in (%s);', 
				paste(input$dbProfileSample, collapse=', ')), sep=' '))
		sendSweetAlert(e$message)	
	})
	actualize$tetrahedrization <- !actualize$tetrahedrization
	print('---------------------------- END PROFILE LAUNCH -------------------')
})

output$uiTetrasSample <- renderUI({
	actualize$tetrahedrization
	choices <- dbGetQuery(db, sprintf('select project_sample, sample from project_sample where 
		zVal is not null and project == "%s";', input$project))
	pickerInput('tetrasSample', 'sample', 
		choices=setNames(choices$project_sample, choices$sample), 
		multiple=FALSE, width='50%', options=list(`live-search`=TRUE))
})

output$tetrahedras <- renderPlotly({
	actualize$tetrahedrization
	if(is.null(input$tetrasSample)) drawTriCut(maxC=maxC, maxCl=maxCl)
	else if(input$tetrasSample == '') drawTriCut(maxC=maxC, maxCl=maxCl)
	else{
		triangles <- dbGetQuery(db, sprintf(
			'select x, y, z, triangle from triangle where project_sample == %s;',
				input$tetrasSample))
		drawTriCut(split(triangles[, c('x', 'y', 'z')], triangles$triangle),
			dbGetQuery(db, sprintf('select zVal from project_sample where project_sample == %s;',
			input$tetrasSample))$zVal, maxC, maxCl)
	}
})

output$uiZoneSamples <- renderUI({
	actualize$tetrahedrization
	choices <- dbGetQuery(db, sprintf('select project_sample, sample from project_sample where 
		zVal is not null and project == "%s";', input$project))
	pickerInput('zoneSamples', 'samples', 
		choices=setNames(choices$project_sample, choices$sample), 
		multiple=TRUE, width='50%', options=list(`live-search`=TRUE, `actions-box`=TRUE))
})

output$map <- renderPlotly({
	actualize$tetrahedrization
	if(is.null(input$zoneSamples)) contourPolyhedras(maxC=maxC, maxCl=maxCl)
	else if(length(input$zoneSamples) == 0) contourPolyhedras(maxC=maxC, maxCl=maxCl)
	else{
		zVals <- dbGetQuery(db, sprintf('select zVal from project_sample where 
			project_sample in (%s);', paste(input$zoneSamples, collapse=', ')))$zVal
		project_samples <- input$zoneSamples[which(zVals > 0)]
		zVals <- zVals[which(zVals > 0)]
		triangles <- dbGetQuery(db, sprintf(
			'select x, y, z, zone, project_sample, triangle from triangle 
				where project_sample in (%s);',
				paste(project_samples, collapse=', ')))
		triangles <- split(triangles, triangles$project_sample)
		triangles <- map(1:length(triangles), function(i) triangles[[i]] %>% 
			mutate(z = z - zVals[i]) %>% split(triangles[[i]]$zone) %>%
				map(function(x) split(x, x$triangle)))
		contourPolyhedras(triangles, dbGetQuery(db, sprintf('select sample from 
			project_sample where project_sample in (%s);', 
				paste(project_samples, collapse=', ')))$sample, maxC, maxCl)
	}
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

