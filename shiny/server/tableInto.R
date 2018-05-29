computeSumAUC <- function(file, adduct, C, Cl){
	if(is.null(C) | is.null(Cl) | is.null(file) | is.null(adduct)) return(0)
	if(file == '' | adduct == '') return(0)
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select formula, sum(auc) from measured inner join observed 
		on observed.id = measured.observed inner join molecule on molecule.id = observed.molecule 
			where sample == "%s" and adduct == "%s" group by observed;',
		file, adduct)
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	if(nrow(data) == 0) return(0)
	data$C <- as.numeric(str_extract_all(str_extract_all(data$formula, 'C[[:digit:]]+'), '[[:digit:]]+'))
	data$Cl <- as.numeric(str_extract_all(str_extract_all(data$formula, 'Cl[[:digit:]]+'), '[[:digit:]]+'))
	res <- sum(data[which(data$C >= C[1] & data$C <= C[2] & data$Cl >= Cl[1] & data$Cl <= Cl[2]), 'sum(auc)'])
	return(formatC(res, format='e', digits=2))
}

output$targetSumAUC <- renderText({
	return(paste('Sum of AUC:', computeSumAUC(input$targetFile, input$targetAdduct, input$targetC, input$targetCl)))	
})

targetTableFunctionInto <- function(file, adduct){
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select formula, sum(auc) from measured inner join observed 
		on observed.id = measured.observed inner join molecule on molecule.id = observed.molecule 
			group by observed having sample == "%s" and adduct == "%s";',
		file, adduct)
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	data$C <- as.numeric(str_extract_all(str_extract_all(data$formula, 'C[[:digit:]]+'), '[[:digit:]]+'))
	data$Cl <- as.numeric(str_extract_all(str_extract_all(data$formula, 'Cl[[:digit:]]+'), '[[:digit:]]+'))
	res <- as.data.frame(matrix(NA, nrow=29, ncol=27))
	colnames(res) <- paste0('Cl', 4:30)
	rownames(res) <- paste0('C', 8:36)
	for(row in 1:nrow(data)) res[data[row, 'C']-7, data[row, 'Cl']-3] <- data[row, 'sum(auc)']
	return(res)
}

output$targetTableInto <- renderDataTable({
	if(is.null(input$targetFile) | is.null(input$targetAdduct)) return(c())
	if(input$targetFile == '' | input$targetAdduct == '') return(c())	
	res <- targetTableFunctionInto(input$targetFile, input$targetAdduct)
	res <- apply(res, c(1, 2), function(x) if(!is.na(x)) formatC(x, format='e', digits=2) else NA)
}, selection='none', extensions='Scroller', options=list(dom='frtip', scrollX=TRUE, scrollY=input$dimension[2]/1.5, scroller=TRUE, 
	deferRender=TRUE, bFilter=FALSE, ordering=FALSE, class='display cell-bordered compact', initComplete=htmlwidgets::JS(paste("
	function(){
		var api = this.api();
		if(", input$targetTable_cell_selected$C, " != 0){
			var $cell = api.cell(", 
				paste(isolate(input$targetTable_cell_selected$C)-8, isolate(input$targetTable_cell_selected$Cl)-3, sep=', '), 
				").nodes().to$();
			$cell.addClass('selected');
		}
	}
"))), callback = htmlwidgets::JS("
	table.on('click', 'tbody td', function(){
		if(table.cell(this).data() != null){
			if ( $(this).hasClass('selected') ) {
				$(this).removeClass('selected');
				Shiny.onInputChange('targetTable_cell_selected', {C: 0, Cl: 0});
			}
			else {
				table.$('td.selected').removeClass('selected');
				$(this).addClass('selected');
				Shiny.onInputChange('targetTable_cell_selected', {C:table.cell(this).index().row+8, Cl:table.cell(this).index().column+3});
			}
		}
		
	});
	Shiny.addCustomMessageHandler('targetTableSelectAUC', reselect);
	function reselect(message){
		console.log('auc');
		table.$('td.selected').removeClass('selected');
		var $cell = table.cell(message.row, message.column).nodes().to$();
		$cell.toggleClass('selected');
		//table.cell(message.row, message.column).data(message.sumOfAuc);
	};
	Shiny.addCustomMessageHandler('targetTableIntoUpdate', update);
	function update(message){
		console.log('update into table');
		table.cell(message.row, message.column).data(message.into);
	};
	Shiny.addCustomMessageHandler('targetTableIntoDelete', deleteCell);
	function deleteCell(message){
		console.log('delete into table');
		table.cell(message.row, message.column).data(null);
		table.$('td:selected').removeClass('selected');
	};
"))