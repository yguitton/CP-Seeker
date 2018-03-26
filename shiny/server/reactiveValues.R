actualize <- reactiveValues() #scope values
actualize$graph <- FALSE
actualize$samples <- FALSE

samples <- reactive({
	actualize$samples
	db <- dbConnect(SQLite(), sqlitePath)
	samples <- dbGetQuery(db, 'select * from sample;')
	dbDisconnect(db)
	actualize$samples <- FALSE
	return(samples)
})
