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
		else if(input$vTarget > 100 | input$vTarget <= 0) custom_stop('invalid', 'target volume must be between 0 and 100')
		else if(length(input$dbProfileSample) == 0 & length(values$xlsxPath) == 0) custom_stop('invalid', 
			'no profile to compute')
			
		query <- sprintf('delete from triangle where project_sample in (%s); 
			update project_sample set zVal = null where project_sample in (%s);',
			paste(input$dbProfileSample, collapse=','),
			paste(input$dbProfileSample, collapse=','))
		print(query)
		dbSendQuery(db, query)
		
		updateProgressBar(session, id='pb', title='Get data', value=0)
		datas <- getDbProfileContent(input$dbProfileSample)
		samplesComputed <- project_samples() %>% 
			filter(project_sample %in% input$dbProfileSample) %>% pull(sample)
		# datas <- append(datas, getXlsxContent(values$xlsxPath))
		
		for(i in 1:length(datas)){
			updateProgressBar(session, id='pb', title=sprintf('Compute distance matrix in %s',
				samplesComputed[i]), value=0)
			datas[[i]] <- distMatrix(datas[[i]], maxC, maxCl)
		}
		
		triangles <- list()
		for(i in 1:length(datas)){
			updateProgressBar(session, id='pb', title=sprintf('triangulization %s',
				samplesComputed[i]), value=0)
			triangles[[i]] <- triangulization(datas[[i]])
		}
		zVals <- c()
		for(i in 1:length(triangles)){
			pbVal <- i*100/length(triangles)
			zVals <- c(zVals, getZCut(triangles[[i]], vTarget=input$vTarget, digits=input$vDigits))
			updateProgressBar(session, id="pb", title=sprintf('target volume in %s', 
				samplesComputed[i]), value=pbVal)
		}
		lapply(1:length(zVals), function(i) dbSendQuery(db, sprintf(
			'update project_sample set zVal = %s where project_sample == %s;',
			zVals[i], input$dbProfileSample[i])))
		
		project_samples <- input$dbProfileSample[which(
			lengths(triangles) > 0)]
		samplesComputed <- samplesComputed[which(
			lengths(triangles) > 0)]
		zVals <- zVals[which(lengths(triangles) > 0)]
		triangles <- triangles[which(lengths(triangles) > 0)]
		
		if(length(triangles) > 0){
			zones <- list()
			edges <- data.frame()
			for(i in 1:length(triangles)){
				updateProgressBar(session, id="pb", title=sprintf('split in zones %s', 
					samplesComputed[i]), value=100)
				zones[[i]] <- splitToZones(triangles[[i]], zVals[[i]])
				edges <- edges %>% rbind(reduce(1:length(zones[[i]]), function(k, l)
					k %>% rbind(reduce(zones[[i]][[l]], rbind) %>%
						cbind(zone=l)), .init=data.frame()) %>%
					cbind(project_sample=project_samples[i]) %>% 
					mutate(z=z - zVals[i]))
			}
			# edges <- reduce(1:length(zones), function(i, j) i %>% 
				# rbind(reduce(1:length(zones[[j]]), function(k, l) k %>% 
					# rbind(reduce(zones[[j]][[l]], rbind) %>% 
						# cbind(zone=l)), .init=data.frame()) %>% 
				# cbind(project_sample=project_samples[j]) %>% mutate(z=z-zVals[j])), .init=data.frame())
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
		dbSendQuery(db, sprintf('update project_sample set zVal = null where
			project_sample in (%s);', paste(input$dbProfileSample, collapse=', ')))
		dbSendQuery(db, sprintf('delete from triangle where project_sample in (%s);', 
			paste(input$dbProfileSample, collapse=', ')))
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
	if(is.null(input$tetrasSample)) drawTri(maxC=maxC, maxCl=maxCl)
	else if(input$tetrasSample == '') drawTri(maxC=maxC, maxCl=maxCl)
	else{
		triangles <- dbGetQuery(db, sprintf(
			'select x, y, z, triangle from triangle where project_sample == %s;',
				input$tetrasSample))
		drawTri(split(triangles[, c('x', 'y', 'z')], triangles$triangle), 
			maxC, maxCl)
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
	print('----------------------------- MAP ------------------------------')
	actualize$tetrahedrization
	p <- if(is.null(input$zoneSamples)) contourPolyhedras(maxC=maxC, maxCl=maxCl)
		else if(length(input$zoneSamples) == 0) contourPolyhedras(maxC=maxC, maxCl=maxCl)
		else{
			triangles <- dbGetQuery(db, sprintf(
				'select x, y, z, zone, project_sample, triangle from triangle 
					where project_sample in (%s);',
					paste(input$zoneSamples, collapse=', ')))
			project_samples <- triangles$project_sample %>% unique
			triangles <- split(triangles, triangles$project_sample)
			triangles <- map(1:length(triangles), function(i) triangles[[i]] %>% 
				split(triangles[[i]]$zone) %>%	map(function(x) split(x, x$triangle)))
				
			# compute score for each zone
			print('compute scores')
			zones <- unlist(triangles, recursive=FALSE)
			scores <- proxy::dist(zones, method=function(x, y) scoreZones(x, y)) %>% 
				as.matrix %>% apply(c(1, 2), function(x) round(x, digits=2))
			
			print('construct map')
			contourPolyhedras(triangles, dbGetQuery(db, sprintf('select sample from 
				project_sample where project_sample in (%s);', 
					paste(project_samples, collapse=', ')))$sample, maxC, maxCl) %>% onRender("
						function(el, x, scores){
							el.on('plotly_hover', function(data){
								var tn = 0,
									pn = 0,
									x = 0,
									y = 0;
								for(var i=0; i < data.points.length; i++){
									tn = data.points[i].curveNumber;
									pn = data.points[i].pointNumber
									x = data.points[i].x;
									y = data.points[i].y;
								};
								el._fullData[tn].hovertext = el.data[tn].name + 
									'<br />Carbons: ' + Math.round(x * 100) / 100 + 
									'<br />Chlorines: ' + Math.round(y * 100) / 100 
								
								var annotations = [{
									curveNumber: tn,
									pointNumber: pn
								}];
								for(var i=1; i < (scores[tn-1].length+1); i++){
									var score = scores[tn-1][i-1];
									if(score == 0) continue;
								
									var id = 0,
										dist = 0,
										tmpDist = 0;
									for(var j=0; j < el.data[i].x.length; j++){
										if(el.data[i].x[j] == null) continue;
										tmpDist = Math.sqrt(
											Math.pow(el.data[i].x[j] - x, 2) + 
											Math.pow(el.data[i].y[j] - y, 2));
										if(tmpDist > dist){
											id = j;
											dist = tmpDist;
										}
									}
									
									annotations.push({
										curveNumber: i,
										pointNumber: id
									});	
									el._fullData[i].hovertext = el.data[i].name + ' - ' + score + '%';
								}
								Plotly.Fx.hover(el, annotations);
							});
						}
					", data=scores)
		}
	print('----------------------------- END MAP ------------------------------')
	p
})
