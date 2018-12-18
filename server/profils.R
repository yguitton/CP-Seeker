values$triangles <- NULL
values$zVal <- NULL

maxC <- 36
maxCl <- 30

observeEvent(input$launch, {
	print('--------------------- TETRAHEDRIZATION ----------------------')
	print(list(C=values$C$id, Cl=values$Cl$id, profile=values$profile$id))
	progressSweetAlert(session, 'pb', title="Initialization", value=0, display_pct=TRUE)
	tryCatch({
		if(values$C$id == 0 | is.null(values$C$data) | length(values$C$data) == 0) custom_stop('minor_error', 'wrong initialization of the column "C"')
		else if(values$Cl$id == 0 | is.null(values$Cl$data) | length(values$Cl$data) == 0) custom_stop('minor_error', 'wrong initialization of the column "Cl"')
		else if(values$profile$id == 0 | is.null(values$profile$data) | length(values$profile$data) == 0) custom_stop('minor_error', 'wrong initialization of the column "profile"')
		
		data <- data.frame(x=values$C$data, y=values$Cl$data, z=values$profile$data) %>% arrange(desc(z))
		updateProgressBar(session, id="pb", title="Compute distance matrix", value=0)
		data <- distMatrix(data)
		updateProgressBar(session, id="pb", title="Tetrahedrization", value=0)
		res <- tetrahedrization(data)
		tetras <- res$tetras
		triangles <- res$triangles
		print(paste('Volume :', computeVolumePolyhedra(tetras)))
		zVal <- getZCut(tetras, input$vTarget, input$vDigits)
		print(paste('Zcut :', zVal))
		
		values$triangles <- triangles
		values$zVal <- zVal
		
	}, minor_error = function(e){
		print(e$message)
		toastr_error(e$message)
		values$triangles <- NULL
		values$zVal <- NULL
	}, error = function(e){
		print(e$message)
		sendSweetAlert(e$message)
		values$triangles <- NULL
		values$zVal <- NULL
	})
	closeSweetAlert(session)
	print('--------------------- END TETRAHEDRIZATION ----------------------')
})

output$tetrahedras <- renderPlotly(drawTriCut(values$triangles, values$zVal))

output$map <- renderPlotly(contourPolyhedras(values$triangles, values$zVal))

