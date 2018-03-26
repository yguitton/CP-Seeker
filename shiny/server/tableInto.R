targetTableFunctionInto <- function(file, adduct){
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select formula, sum(auc), adduct from measured inner join observed on observed.id = measured.observed inner join molecule on molecule.id = observed.molecule group by observed having sample == "%s" and adduct == "%s";',
		file, adduct)
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	data$C <- as.numeric(str_extract_all(str_extract_all(data$formula, 'C[[:digit:]]+'), '[[:digit:]]+'))
	data$Cl <- as.numeric(str_extract_all(str_extract_all(data$formula, 'Cl[[:digit:]]+'), '[[:digit:]]+'))
	res <- as.data.frame(matrix(NA, nrow=29, ncol=27))
	colnames(res) <- paste0('Cl', 4:30)
	rownames(res) <- paste0('C', 8:36)
#	for(row in 1:nrow(data)) res[data[row, 'C']-7, data[row, 'Cl']-3] <- format(data[row, 'sum(auc)'], format='e', digits=2)
	for(row in 1:nrow(data)) res[data[row, 'C']-7, data[row, 'Cl']-3] <- data[row, 'sum(auc)']
	return(res)
}

output$targetTableInto <- renderDataTable({
	if(is.null(input$targetFile) | is.null(input$targetAdduct)) return(c())
	if(input$targetFile == '' | input$targetAdduct == '') return(c())	
	res <- targetTableFunctionInto(input$targetFile, input$targetAdduct)
	res <- apply(res, c(1, 2), function(x) if(!is.na(x)) formatC(x, format='e', digits=2) else NA)
}, selection='none', extensions=c('Scroller', 'Buttons'), options=list(dom='Bfrtip', scrollX=TRUE, scrollY=450, scroller=TRUE, deferRender=TRUE, bFilter=FALSE, ordering=FALSE, buttons=htmlwidgets::JS('
	[
		{
			text: "Export to excel",
			action:function(e, table, node, config){
				document.getElementById("targetDownload").click();
			}
		}
	]
'), initComplete=htmlwidgets::JS(paste("
	function(){
		var api = this.api();
		var $cell = api.cell(", 
		if(is.null(isolate(input$targetTable_cell_selected))) "-1, -1" else paste(isolate(input$targetTable_cell_selected$C)-8, isolate(input$targetTable_cell_selected$Cl)-3, sep=', '), 
		").nodes().to$();
		$cell.addClass('selected');
	}
"))), callback = htmlwidgets::JS("
	table.on('change', '#targetTable_cell_selected', function(){
		var $cell = table.cell(this.value[1]-8, this.value[2])
	})
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
"))