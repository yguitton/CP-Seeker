output$uiFileInfo <- renderUI({
	choices <- if(is.null(input$selectProject)) c()
		else samples()[which(samples()$project == input$selectProject), 'sample']
	pickerInput('fileDTInfo', 'Choose a file', choices=choices, multiple=FALSE)
})

#about conversion
output$tableInfoConversion <- renderDataTable(t(samples()[which(samples()$sample == input$fileDTInfo), ]), 
	selection='none', colnames='Info', options=list(bFilter=FALSE, paging=FALSE, dom = 'frtip'))
