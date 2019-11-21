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
	pickerInput('tetrasSample', 'sample(s)', choices=setNames(
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
	zones = NULL,
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
			titles <- 'sample(s) picker'
			conditions <- length(input$tetrasSample) == 0
			messages <- c('You need to select a sample')
			test <- inputsTest(args, conditions, messages)
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
			all(triangle$z >= zCut))
		zones <- c(list(zoneUnderZ), splitToZones(zoneAboveZ))
		
		param$tetrahedras <- list(
			pja = if(input$tetrasTab == "tetrasDBTab") input$tetrasSample else NULL,
			xlsxFile = if(input$tetrasTab == "tetrasXlsxTab") file$datapath else NULL,
			vTarget = input$tetrasVTarget, 
			zones = zones,
			zCut = zCut
		)
		closeSweetAlert(session)
			
	}, invalid = function(i) param$tetrahedras <- list(
			pja = NULL,
			xlsxFile = NULL,
			vTarget = NULL, 
			zones = NULL,
			zCut = NULL
		)
	, minor_error = function(e){
		print(e)
		toastr_error("no data for this file")
		param$tetrahedras <- list(
			pja = NULL,
			xlsxFile = NULL,
			vTarget = NULL, 
			zones = NULL,
			zCut = NULL
		)
	}, error = function(e){
		param$tetrahedras <- list(
			pja = NULL,
			xlsxFile = NULL,
			vTarget = NULL, 
			zones = NULL,
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
	tryCatch(
	draw2D(param$tetrahedras$zones, c(minC, maxC), c(minCl, maxCl), param$tetrahedras$zCut)
	, error = function(e){
		print('ERR tetras3D')
		print(e)
		toastr_error('Cannot draw 3D profile', paste(e$message))
		draw2D(c(minC, maxC), c(minCl, maxCl))
	})
})

output$tetras3D <- renderPlotly({
	tryCatch(
	draw3D(unlist(param$tetrahedras$zones, recursive = FALSE), c(minC, maxC), c(minCl, maxCl), param$tetrahedras$zCut)
	, error = function(e){
		print('ERR tetras3D')
		print(e)
		toastr_error('Cannot draw 3D profile', paste(e$message))
		draw3D(c(minC, maxC), c(minCl, maxCl))
	})
})


# observeEvent(input$tetrasRecord, {
	# print('############################################################')
	# print('######################### RECORD TETRAS ####################')
	# print('############################################################')
	
	# tryCatch({
		# print(list(name = input$tetrasName, nbTriangles = length(param$tetras$triangles)))
		# profileNames <- dbGetQuery(db, 'select name from profile;')$name
		# args <- c("tetrasName", "tetrasName", "")
		# conditions <- c(input$tetrasName == "", input$tetrasName %in% profileNames, 
			# is.null(param$tetras$triangles))
		# messages <- c("You have to specify a name for the record", 
			# "A profile with this name already exists", "no profile computed")
		# if(!inputsTest(args, conditions, messages)) custom_stop('invalid', 'invalid args')
		
		# recordProfile(input$tetrasName, param$tetras$vTarget, param$tetras$triangles, 
			# param$tetras$zCut, pja = param$tetras$pja, xlsx = param$tetras$xlsxFile)
	# }, invalid = function(i) NULL
	# , error = function(e){
		# print(e)
		# deleteProfile(input$tetrasName)
		# sendSweetAlert('Cannot record this profile', paste(e$message))
	# })
	# actualize$profiles <- TRUE
	# print('############################################################')
	# print('######################### END RECORD TETRAS ################')
	# print('############################################################')
# })



