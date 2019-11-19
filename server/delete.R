output$deleteProjectsTable <- DT::renderDataTable(
projects(), rownames = NULL, selection = "none", options = list(dom = 'frtip', 
columnDefs = list(list(visible = FALSE, targets = 0)), bFilter = FALSE), 
callback = htmlwidgets::JS("
	table.on('click', 'tbody tr', function(){
		$(this).toggleClass('selected');
		var ids = table.rows('.selected').data().toArray().map(x => x[0])
		Shiny.onInputChange('deleteProjectsTable_ids', {id: ids, ignore: Math.random()});
	})
"))

output$deleteSamplesTable <- DT::renderDataTable(
samples(), rownames = NULL, selection = "none", extensions = 'Scroller', filter = 'top', 
options = list(dom = 'frtip', bFilter = FALSE, scrollY = input$dimension[2] / 2, scrollX = TRUE, scroller = TRUE, paging = TRUE, pageLength = 10), 
callback = htmlwidgets::JS("
	table.on('click', 'tbody tr', function(){
		$(this).toggleClass('selected');
		var ids = table.rows('.selected').data().toArray().map(x => x[0])
		Shiny.onInputChange('deleteSamplesTable_ids', {id: ids, ignore: Math.random()});
	})
"))

observeEvent(input$deleteProjects, {
	print('############################################################')
	print('######################### DELETE PROJECT ###################')
	print('############################################################')
	tryCatch({
		print(list(project = paste(
			input$deleteProjectsTable_ids$id, collapse=', ')))
		inputs <- ''
		titles <- 'delete project'
		conditions <- length(input$deleteProjectsTable_ids$id) == 0
		messages <- 'You must select a project first to delete it'
		if(!inputsTest(inputs, conditions, titles, messages)) custom_stop('invalid', 'no row selected')
		
		deleteProjects(input$deleteProjectsTable_ids$id)
		actualize$projects <- TRUE
	}, invalid = function(i) print(i)
	, error = function(e){
		print(e)
		sendSweetAlert('Cannot delete project(s)', paste(e$message))
	})
	print('############################################################')
	print('######################### DELETE PROJECT ###################')
	print('############################################################')
})

observeEvent(input$deleteSamples, {
	print('############################################################')
	print('######################### DELETE SAMPLES ###################')
	print('############################################################')
	tryCatch({
		print(list(project = paste(
			input$deleteSamplesTable_ids$id, collapse=', ')))
		inputs <- ''
		titles <- 'delete sample'
		conditions <- length(input$deleteSamplesTable_ids$id) == 0
		messages <- 'You must select a sample first to delete it'
		if(!inputsTest(inputs, conditions, titles, messages)) custom_stop('invalid', 'no row selected')
		
		deleteSamples(input$deleteSamplesTable_ids$id)
		actualize$samples <- TRUE
	}, invalid = function(i) print(i)
	, error = function(e){
		print(e)
		sendSweetAlert('Cannot delete sample(s)', paste(e$message))
	})
	print('############################################################')
	print('######################### DELETE SAMPLES ###################')
	print('############################################################')
})
