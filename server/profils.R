values$triangles <- NULL
values$zVal <- NULL

observeEvent(input$launch, {
	print('--------------------- TETRAHEDRIZATION ----------------------')
	print(list(C=values$C, Cl=values$Cl, profile=values$profile))
	progressSweetAlert(session, 'pb', title="Initialization", 100, striped=TRUE)
	tryCatch({
		if(is.null(values$C) | is.null(values$Cl) | is.null(values$profile)) custom_stop('minor_error', 'You need to load a file with the profile data')
		data <- data.frame(x=values$C, y=values$Cl, z=values$profile) %>% arrange(desc(z))
		updateProgressBar(session, id="pb", title="Compute distance matrix", value=0)
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

output$map <- renderPlotly(contourPolyhedras(values$triangles, values$zVal, 
	data.frame(x=values$C, y=values$Cl, z=values$profile) %>% summarise(x=sum(x*z), y=sum(y*z))))

