output$uiFileInfo <- renderUI({
	if(is.null(input$selectProject)) return(selectInput('fileDTInfo', 'Choose a file', choices=samples()[which(samples()$project == unique(samples()$project)[1]), 'sample']))
	else choices <- samples()[which(samples()$project == input$selectProject), 'sample']
	selectInput('fileDTInfo', 'Choose a file', choices=choices, multiple=FALSE)
})

#about conversion
output$tableInfoConversion <- renderDataTable(t(samples()[which(samples()$sample == input$fileDTInfo), ]), selection='none', colnames='Info', options=list(bFilter=FALSE, paging=FALSE, dom = 'frtip'))
