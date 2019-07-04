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
		
		trianglesPS <- list()
		for(i in 1:length(datas)){
			updateProgressBar(session, id='pb', title=sprintf('triangulization %s',
				samplesComputed[i]), value=0)
			trianglesPS[[i]] <- triangulization(datas[[i]])
		}
		zVals <- c()
		for(i in 1:length(trianglesPS)){
			pbVal <- i*100/length(trianglesPS)
			zVals <- c(zVals, getZCut(trianglesPS[[i]], vTarget=input$vTarget, digits=input$vDigits))
			updateProgressBar(session, id="pb", title=sprintf('target volume in %s', 
				samplesComputed[i]), value=pbVal)
		}
		lapply(1:length(zVals), function(i) dbSendQuery(db, sprintf(
			'update project_sample set zVal = %s where project_sample == %s;',
			zVals[i], input$dbProfileSample[i])))
		
		project_samples <- input$dbProfileSample[which(
			lengths(trianglesPS) > 0)]
		zVals <- zVals[which(lengths(trianglesPS) > 0)]
		trianglesPS <- trianglesPS[which(lengths(trianglesPS) > 0)]
		zonesPS <- list()
		
		saveRDS(list(trianglesPS=trianglesPS, zVals=zVals, project_sample=project_samples),
			file='test.RDS')
		
		for(i in 1:length(trianglesPS)){
			updateProgressBar(session, id='pb', title=sprintf('split into zones %s',
				samplesComputed[i]), value=100)
			trianglesPS[[i]] <- purrr::reduce(trianglesPS[[i]], function(a, b)
				a %>% append(splitTriangle(b, zVals[i])), .init=list())
			# get triangle under zVal
			trianglesUnder <- keep(trianglesPS[[i]], function(triangle)
				any(triangle$z < zVals[i]))
			# get triangle above zVal
			trianglesAbove <- keep(trianglesPS[[i]], function(triangle)
				any(triangle$z > zVals[i]))
			zonesPS[[i]] <- list(trianglesUnder) %>% 
				append(splitToZones(trianglesAbove))
		}
		edges <- purrr::reduce(1:length(zonesPS), function(a, b) 
			a %>% rbind(purrr::reduce(1:length(zonesPS[[b]]), function(c, d) 
				c %>% rbind(purrr::reduce(zonesPS[[b]][[d]], rbind, .init=data.frame()) %>% 
					cbind(zone = d-1)), .init=data.frame()) %>% 
				cbind(triangle = rep(1:(sum(lengths(zonesPS[[b]]))), each=3)) %>% 
				cbind(project_sample = project_samples[b])), 
				.init=data.frame())
		query <- sprintf('insert into triangle (x, y, z, triangle, project_sample, zone) values (%s);', 
			paste(edges$x, edges$y, edges$z, edges$triangle, edges$project_sample, edges$zone, sep=', ', collapse='), ('))
		dbSendQuery(db, query)
			
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
	# purge the zones map
	plotlyProxy("map", session) %>% 
		plotlyProxyInvoke("purge")
	print('---------------------------- END PROFILE LAUNCH -------------------')
})

output$uiTetrasSample <- renderUI({
	actualize$tetrahedrization
	choices <- tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project picker is not yet initialized')
		else if(input$project == '') custom_stop('invalid', 'no project')
		else project_samples() %>% filter(project == input$project & 
			!is.na(zVal)) %>% select(sampleID, project_sample)
	}, invalid = function(i){
		print(paste(i))
		data.frame(sampleID = c(), project_sample = c())
	}, error = function(e){
		print(paste(e))
		sendSweetAlert(paste(e$message))
		data.frame(sampleID = c(), project_sample = c())
	})
	pickerInput('tetrasSample', 'sample', 
		choices=setNames(choices$project_sample, choices$sampleID), 
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
		zVal <- dbGetQuery(db, sprintf(
			'select zVal from project_sample where project_sample == %s;',
				input$tetrasSample))$zVal
		drawTriCut(split(triangles[, c('x', 'y', 'z')], triangles$triangle), 
			zVal, maxC, maxCl)
	}
})

output$uiZoneSamples <- renderUI({
	actualize$tetrahedrization
	choices <- tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project picker is not yet initialized')
		else if(input$project == '') custom_stop('invalid', 'no project')
		else project_samples() %>% filter(project == input$project & 
			!is.na(zVal)) %>% select(sampleID, project_sample)
	}, invalid = function(i){
		print(paste(i))
		data.frame(sampleID = c(), project_sample = c())
	}, error = function(e){
		print(paste(e))
		sendSweetAlert(paste(e$message))
		data.frame(sampleID = c(), project_sample = c())
	})
	pickerInput('zoneSamples', 'samples', 
		choices=setNames(choices$project_sample, choices$sampleID), 
		multiple=TRUE, options=list(`live-search`=TRUE, `actions-box`=TRUE))
})

output$map <- renderPlotly(zonesMap())

zonesMap <- eventReactive(input$zoneDraw, {
	print('----------------------------- MAP ------------------------------')
	p <- if(is.null(input$zoneSamples)) contourPolyhedras(maxC=maxC, maxCl=maxCl)
		else if(length(input$zoneSamples) == 0) contourPolyhedras(maxC=maxC, maxCl=maxCl)
		else{
			project_samples <- input$zoneSamples
			edges <- dbGetQuery(db, sprintf(
				'select x, y, z, project_sample, triangle, zone from triangle 
					where project_sample in (%s) and zone != 0;',
					paste(project_samples, collapse=', ')))
			if(nrow(edges) == 0) return(contourPolyhedras(maxC=maxC, maxCl=maxCl))
			zonesPS <- split(edges %>% select(-project_sample), edges$project_sample) %>%
				purrr::map(function(triangles) split(triangles %>% select(-zone), triangles$zone)) %>%
				purrr::map(function(zones) zones %>% purrr::map(function(triangles) split(
					triangles %>% select(-triangle), triangles$triangle)))
			
			project_samples <- unique(edges$project_sample)
			zVals <- dbGetQuery(db, sprintf('select zVal from project_sample
				where project_sample in (%s);',
				paste(project_samples, collapse=', ')))$zVal
			
			# compute score for each zone
			print('compute scores')
			zones <- unlist(zonesPS, recursive=FALSE)
			scores <- proxy::dist(zones, method=function(x, y) scoreZones(x, y)) %>% 
				as.matrix %>% apply(c(1, 2), function(x) round(x, digits=2))
			
			print('construct map')
			sampleNames <- project_samples() %>% filter(project_sample %in% project_samples) %>% 
				pull(sampleID)
			contourPolyhedras(zonesPS, zVals, sampleNames, maxC, maxCl) %>% onRender("
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
