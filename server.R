options(show.error.messages = TRUE)
shinyServer(function(input, output, session) {

#to close the connection
session$onSessionEnded(function() {
	stopApp()
	# comment line above because it's still in developement
	# q('no')
})

values <- reactiveValues()
actualize <- reactiveValues()

minC <- 4
maxC <- 36
minCl <- 2
maxCl <- 30

# create the adducts [M-Cl]- & [M-H-Cl]-
adducts <- adducts %>% rbind(data.frame(
	Name = c("M-Cl", "M-H-Cl"), calc = c("M-34.9683", "M-35.97613"),
	Charge = c(-1, -1), Mult = c(1, 1), Mass = c(-34.9683, -35.97613),
	Ion_mode = c('negative', 'negative'), Formula_add = c("FALSE", "FALSE"),
	Formula_ded = c('Cl1', 'Cl1H1'), Multi = c(1, 1)))
	
samples <- reactive({
	actualize$samples
	samples <- dbGetQuery(db, 'select * from sample;')
	actualize$samples <- FALSE
	samples
})

projects <- reactive({
	actualize$projects
	projects <- dbGetQuery(db, 'select name from project;')$name
	actualize$projects <- FALSE
	projects
})

project_samples <- reactive({
	actualize$project_samples
	project_samples <- dbGetQuery(db, 'select * from project_sample;')
	actualize$project_samples <- FALSE
	project_samples
})

output$downloadProject <- downloadHandler(
	filename = function(){
		if(!is.null(input$project)) paste(input$project, '.xlsx', sep='')
		else '*.xlsx'
	}, content = function(con){
	print('-------------- DOWNLOAD PROJECT -----------')
	print(list(project = input$project))
	wb <- createWorkbook()
	tryCatch({
		if(!is.null(input$project)){
			db <- dbConnect(SQLite(), sqlitePath)
			datas <- dbGetQuery(db, sprintf('select sample, adduct, rangeRT_1, 
				rangeRT_2, formula, C, Cl, round(auc) as auc, round(score, 2) as score, 
				round(rt, 2) as rt, round(rtmin, 2) as rtmin, round(rtmax, 2) as rtmax, 
				ppm, peakwidth, machine from observed inner join project_sample on 
				project_sample.project_sample = observed.project_sample where
				project == "%s";', input$project))
			dbDisconnect(db)
			sample_adducts <- paste(datas$sample, datas$adduct)
			datas$machine <- names(resolution_list)[datas$machine]
			datas <- split(datas[, -c(1:4)] %>% 
				cbind(range_rT_param = paste(datas$rangeRT_1, '-', datas$rangeRT_2)) %>% 
				cbind(range_rT = paste(datas$rtmin, '-', datas$rtmax)) %>% 
				mutate(range_rT_param = replace(range_rT_param, range_rT_param == "NA - NA", NA)),
				sample_adducts)
			for(i in 1:length(datas)){
				addWorksheet(wb, sample_adducts[i])
				writeDataTable(wb, i, datas[[i]][, c('formula', 'C', 'Cl', 'auc', 'score',	
					'rt', 'range_rT', 'ppm', 'peakwidth', 'machine', 'range_rT_param')])
			}
		}
		saveWorkbook(wb, con, overwrite=TRUE)
	}, error = function(e){
		print(e$message)
		sendSweetAlert(e$message)
	})
	con
})

source('server/func.R', local=TRUE)$value

source('server/loadProfile.R', local=TRUE)$value

source('server/profils.R', local=TRUE)$value

source('server/tetrahedrization.R', local=TRUE)$value

source('server/projectFiles.R', local=TRUE)$value

source('server/target.R', local=TRUE)$value

source('server/details.R', local=TRUE)$value

})