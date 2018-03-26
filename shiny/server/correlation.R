output$uiCorrelationStandardFile <- renderUI({
	files <- allFiles()
	files <- files[which(files$standard == 1), ]
	selectInput('correlationStandardFile', 'Select standard(s)', choices=files$sample, multiple=TRUE)
})

output$uiCorrelationFile <- renderUI({
	files <- allFiles()
	files <- files[which(files$standard == 0), ]
	selectInput('correlationFile', 'Select sample', choices=files$sample, multiple=FALSE)
})

output$correlationGraph <- renderPlotly({
	graph <- plot_ly(type='scatter', mode='markers')
	standardFiles <- input$correlationStandardFile
	file <- input$correlationFile
	if(is.null(standardFiles) | is.null(file)) return(graph)
	if(length(standardFiles) == 0 | file == "") return(graph)
	graph <- graph %>% layout(xaxis=list(title=file), yaxis=list(title='Standard(s)'), showlegend=FALSE, title='Correlation')
	db <- dbConnect(SQLite(), sqlitePath)
#	dataStandard <- dbGetQuery(db, sprintf('select formula, auc as aucStandard from observed where sample in (%s);', paste(sapply(standardFiles, function(x) paste0("'", x, "'")), collapse=', ')))
	dataFile <- dbGetQuery(db, sprintf('select formula, auc as aucFile from observed where sample == "%s";', file))
	dataStandard <- dbGetQuery(db, sprintf('select formula, auc as aucStandard from observed where sample in (%s) and formula in (select formula from observed where sample == "%s");', paste(sapply(standardFiles, function(x) paste0("'", x, "'")), collapse=', '), file))
	data <- merge(dataStandard, dataFile, all=TRUE)
	data[is.na(data)] <- 0
	graph <- graph %>% add_trace(x=data$aucFile, y=data$aucStandard)	
	fit <- lm(aucStandard ~ aucFile, data)
	updateTextInput(session, 'correlationText', '', paste(coef(fit)))
	print(summary(fit))
	graph <- graph %>% add_lines(x=data$aucFile, y=fitted(fit))
})


# output$correlationResidues <- renderPlotly({
# graph <- plot_ly(type='scatter', mode='markers')
	# standardFiles <- input$correlationStandardFile
	# file <- input$correlationFile
	# if(is.null(standardFiles) | is.null(file)) return(graph)
	# if(length(standardFiles) == 0 | file == "") return(graph)
	# graph <- graph %>% layout(xaxis=list(title=file), yaxis=list(title='Residuals'), showlegend=FALSE, title='Residual')
	# db <- dbConnect(SQLite(), sqlitePath)
	# dataStandard <- dbGetQuery(db, sprintf('select formula, auc as aucStandard from observed where sample in (%s);', paste(sapply(standardFiles, function(x) paste0("'", x, "'")), collapse=', ')))
	# dataFile <- dbGetQuery(db, sprintf('select formula, auc as aucFile from observed where sample == "%s";', file))
	# data <- merge(dataStandard, dataFile, all=TRUE)
	# data[is.na(data)] <- 0
	# data <- data[which(data$aucStandard != 0 & data$aucFile != 0), ]
	# fit <- lm(aucStandard ~ aucFile, data)
	# graph <- graph %>% add_trace(x=data$aucFile, y=resid(fit))
# })
