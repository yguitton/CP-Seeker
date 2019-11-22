output$uiTetrasSample <- renderUI({
	choices <- tryCatch({
		if(length(input$project) == 0) custom_stop('invalid', 'project picker is not yet initialized')
		else project_samples_adducts() %>% left_join(project_samples(), by="project_sample") %>% 
			filter(project == input$project) %>% mutate(name = paste(sampleID, adduct)) %>% 
				select(name, project_sample_adduct)
	}, invalid = function(i) data.frame(name = c(), project_sample_adduct = c())
	, error = function(e){
		print('ERR uiTetrasSamples')
		print(paste(e))
		sendSweetAlert("Cannot retrieve analyzed samples", paste(e$message))
		data.frame(name = c(), project_sample_adduct = c())
	})
	pickerInput('tetrasSample', 'sample', choices=setNames(
		choices$project_sample_adduct, choices$name), 
		multiple=FALSE, options=list(`live-search`=TRUE))
})

shinyFileChoose(input, 'tetrasXlsxImport', roots=getVolumes(), 
	filetypes=c('xlsx'), defaultRoot=names(getVolumes()()[1]))
	
observeEvent(parseFilePaths(getVolumes()(), input$tetrasXlsxImport), {
	runjs(sprintf("$('#tetrasXlsxFileName')[0].placeholder = '%s';", 
		basename(parseFilePaths(getVolumes()(), input$tetrasXlsxImport)$datapath)))
})

param$tetrahedras <- list(
	pja = NULL,
	xlsxFile = NULL,
	vTarget = NULL, 
	points = NULL,
	zCut = NULL
)

observeEvent(input$tetrasCompute, {
	print('############################################################')
	print('######################### TETRAS ###########################')
	print('############################################################')

	print(list(tabSelected = input$tetrasTab, project_sample_adduct = input$tetrasSample, 
		xlsxFile = parseFilePaths(getVolumes()(), input$tetrasXlsxImport),
		vTarget = input$tetrasVTarget, precision = precision))
	tryCatch({
		if(input$tetrasTab == "tetrasDBTab"){
			args <- 'tetrasSample'
			titles <- 'sample picker'
			conditions <- length(input$tetrasSample) == 0
			messages <- c('You need to select a sample')
			test <- inputsTest(args, conditions, titles, messages)
			if(!test) custom_stop('invalid', 'invalid args')
			data <- initializeTetras(input$tetrasSample, minScore)
		} else {
			file <- parseFilePaths(getVolumes()(), input$tetrasXlsxImport)
			if(nrow(file) == 0) custom_stop('minor_error', 'no file selected')
			else if(file$name %>% str_replace_all('\\\\', '/') %>% 
				str_detect(specialChars) %>% any) custom_stop('minor_error', 
					'Filepaths cannot contain any special characters ([\\:*?\"<>|])')
			
			# check if there is the required column (C, Cl and Int)
			data <- read.xlsx(file$datapath)
			if(colnames(data)[1] != 'C') custom_stop('minor_error', 'the first column need to be C')
			else if(colnames(data)[2] != 'Cl') custom_stop('minor_error', 'the first column need to be Cl')
			else if(colnames(data)[3] != 'Int') custom_stop('minor_error', 'the first column need to be Int')
			else if(any(!is.numeric(data[, 1]))) custom_stop('minor_error', 'The column C need to contain only numerical values')
			else if(any(!is.numeric(data[, 2]))) custom_stop('minor_error', 'The column Cl need to contain only numerical values')
			else if(any(!is.numeric(data[, 3]))) custom_stop('minor_error', 'The column Int need to contain only numerical values')
			else if(any(data[, 1] <= 0)) custom_stop('minor_error', 'The column C cannot contain any value under or equal to 0')
			else if(any(data[, 2] <= 0)) custom_stop('minor_error', 'The column Cl cannot contain any value under or equal to 0')
			else if(any(data[, 3] < 0)) custom_stop('minor_error', 'The column Int cannot contain any negative values')
			colnames(data) <- c('x' ,'y', 'z')
		}
				
		pb <- progressSweetAlert(session, 'pb', value = 0, striped = TRUE, title = "Triangulization")
		triangles <-  data %>% 
			profileMat(maxC, maxCl) %>% 
			triangulization
		if(length(triangles) == 0) custom_stop('minor_error', 'no data')
		zCut <- getZCut(triangles, input$tetrasVTarget, precision, "pb")	
		triangles <- do.call(c, lapply(triangles, function(triangle) 
			splitTriangle(triangle, zCut)))
		updateProgressBar(session, id = "pb", title="Split into zone(s)", value = 100)
		zoneUnderZ <- keep(triangles, function(triangle) 
			all(triangle$z <= zCut))
		zoneAboveZ <- keep(triangles, function(triangle) 
			all(triangle$z >= zCut)) %>% splitToZones
		points <- do.call(rbind, lapply(1:length(zoneUnderZ), function(i) 
			zoneUnderZ[[i]] %>% mutate(triangle = i, zone = 0)))
		points <- points %>% rbind(do.call(rbind, lapply(1:length(zoneAboveZ), function(i) 
			do.call(rbind, lapply(1:length(zoneAboveZ[[i]]), function(j) 
				zoneAboveZ[[i]][[j]] %>% mutate(zone = i, triangle = j))))))
		
		param$tetrahedras <- list(
			pja = if(input$tetrasTab == "tetrasDBTab") input$tetrasSample else NULL,
			xlsxFile = if(input$tetrasTab == "tetrasXlsxTab") file$datapath else NULL,
			vTarget = input$tetrasVTarget, 
			points = points, 
			zCut = zCut
		)
		closeSweetAlert(session)
			
	}, invalid = function(i) param$tetrahedras <- list(
			pja = NULL,
			xlsxFile = NULL,
			vTarget = NULL, 
			points = NULL,
			zCut = NULL
		)
	, minor_error = function(e){
		print(e)
		toastr_error("no data for this file")
		param$tetrahedras <- list(
			pja = NULL,
			xlsxFile = NULL,
			vTarget = NULL, 
			points = NULL,
			zCut = NULL
		)
	}, error = function(e){
		param$tetrahedras <- list(
			pja = NULL,
			xlsxFile = NULL,
			vTarget = NULL, 
			points = NULL,
			zCut = NULL
		)
		print(e)
		sendSweetAlert('Cannot compute 3D profile', paste(e$message))
	})
	
	print('############################################################')
	print('######################### END TETRAS #######################')
	print('############################################################')
})

output$tetras2D <- renderPlotly({
	tryCatch({
	data <- list()
	if(!is.null(param$tetrahedras$points)){
		data <- data %>% append(
			list(getEdges(param$tetrahedras$points, param$tetrahedras$zCut)))
		names(data) <- project_samples_adducts() %>% 
			left_join(project_samples(), by="project_sample") %>% 
			filter(project_sample_adduct == param$tetrahedras$pja) %>% 
			mutate(name = paste(sampleID, adduct)) %>% 
			pull(name)
	}
	if(length(input$tetrasCompare) > 0){
		data2 <- lapply(input$tetrasCompare, function(profile){
				points <- dbGetQuery(db, sprintf('select * from point where profile == "%s";', profile))
				zCut <- dbGetQuery(db, sprintf('select zCut from profile where profile == "%s";', profile))$zCut
				getEdges(points, zCut)
			})
		names(data2) <- input$tetrasCompare
		data <- data %>% append(data2)
	}
	draw2D(data, xrange = c(minC, maxC), yrange = c(minCl, maxCl))
	}, error = function(e){
		print('ERR tetras2D')
		print(e)
		toastr_error('Cannot draw 2D profile', paste(e$message))
		draw2D(xrange = c(minC, maxC), yrange = c(minCl, maxCl))
	})
})

output$tetras3D <- renderPlotly({
	tryCatch(
	draw3D(param$tetrahedras$points, c(minC, maxC), c(minCl, maxCl), param$tetrahedras$zCut)
	, error = function(e){
		print('ERR tetras3D')
		print(e)
		toastr_error('Cannot draw 3D profile', paste(e$message))
		draw3D(xrange=c(minC, maxC), yrange=c(minCl, maxCl))
	})
})

output$uiTetrasCompare <- renderUI({
	choices <- tryCatch({
		profiles()$profile
	}, error = function(e){
		print('ERR uiTetrasCompare')
		print(e)
		sendSweetAlert("Cannot retrieve profiles in database")
		c()
	})
	pickerInput('tetrasCompare', 'profiles', choices = choices, 
		multiple = TRUE, options=list(`actions-box`=TRUE, `live-search`=TRUE))
})

output$tetrasTableScores <- renderDataTable({
	tryCatch({
		if(is.null(param$tetrahedras$points)) custom_stop('invalid', 'no profile computed')
		else if(length(input$tetrasCompare) == 0) custom_stop('invalid', 'no profile selected for comparison')
		
		profiles <- list(param$tetrahedras$points) %>% 
			append(lapply(input$tetrasCompare, getPoints))
		scoreMat <- matrix(, nrow = length(profiles), ncol = length(profiles))
		for(i in 1:(length(profiles) - 1)) for(j in (i + 1):length(profiles)) scoreMat[i, j] <- scoreProfiles(profiles[[i]], profiles[[j]])
		scoreMat <- data.frame(scoreMat)
		profileNameComputed <- project_samples_adducts() %>% 
			left_join(project_samples(), by="project_sample") %>% 
			filter(project_sample_adduct == input$tetrasSample) %>% 
			mutate(name = paste(sampleID, adduct)) %>% 
			pull(name)
			
		rownames(scoreMat) <- c(profileNameComputed, input$tetrasCompare)
		colnames(scoreMat) <- c(profileNameComputed, input$tetrasCompare)
		scoreMat
	}, invalid = function(i) data.frame()
	, error = function(e){
		print('ERR tetrasTableScores')
		print(e)
		sendSweetAlert('Cannot compare profiles between them', paste(e$message))
		data.frame()
	})
}, selection = "none", extensions = "Scroller", class='display cell-border compact nowrap', options = list(
dom = "frtip", info = FALSE, ordering = FALSE, scroller = TRUE, bFilter = FALSE, 
scrollX = TRUE))


observeEvent(input$tetrasRecord, {
	print('############################################################')
	print('######################### RECORD TETRAS ####################')
	print('############################################################')
	
	tryCatch({
		print(list(param$tetrahedras[c('pja', 'xlsxFile', 'vTarget', 'zCut')], 
			input$tetrasName))
		
		inputs <- c('', 'tetrasName')
		titles <- c('missing profile', 'profile name')
		conditions <- c(is.null(param$tetrahedras[['pja']]) & is.null(param$tetrahedras[['xlsxFile']]), 
			length(input$tetrasName) == 0)
		messages <- c('you need to compute first the profile before recording it', 
			'you need to provide a name for this profile')
		if(!inputsTest(inputs, conditions, titles, messages)) custom_stop('invalid', 'invalid args')
		inputs <- inputs[-1]
		titles <- titles[-1]
		conditons <- any(str_detect(input$tetrasName, specialChars))
		messages <- 'Profile name cannot contain any special characters ([\\:*?\"<>|])'
		if(!inputsTest(inputs, conditions, titles, messages)) custom_stop('invalid', 'invalid args')
			
		deleteProfiles(input$tetrasName)
		recordProfile(input$tetrasName, param$tetrahedras$zCut, param$tetrahedras$vTarget, 
			param$tetrahedras$pja, param$tetrahedras$xlsxFile)
		recordPoints(param$tetrahedras$points, input$tetrasName)
		toastr_success(paste(input$tetrasName, "recorded"))
	}, invalid = function(i) NULL
	, error = function(e){
		print(e)
		sendSweetAlert('Cannot record this profile', paste(e$message))
	})
	actualize$profiles <- TRUE
	print('############################################################')
	print('######################### END RECORD TETRAS ################')
	print('############################################################')
})



