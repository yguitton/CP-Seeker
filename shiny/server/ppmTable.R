targetTableFunctionPpmDeviation <- function(file, adduct){
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('select formula, ppmDeviation from observed inner join molecule on molecule.id = observed.molecule where sample == "%s" and adduct == "%s";',
		file, adduct)
	data <- dbGetQuery(db, query)
	dbDisconnect(db)
	data$C <- as.numeric(str_extract_all(str_extract_all(data$formula, 'C[[:digit:]]+'), '[[:digit:]]+'))
	data$Cl <- as.numeric(str_extract_all(str_extract_all(data$formula, 'Cl[[:digit:]]+'), '[[:digit:]]+'))
	res <- as.data.frame(matrix(NA, nrow=29, ncol=27))
	colnames(res) <- paste0('Cl', 4:30)
	rownames(res) <- paste0('C', 8:36)
	for(row in 1:nrow(data)) res[data[row, 'C']-7, data[row, 'Cl']-3] <- data[row, 'ppmDeviation']
	return(res)
}

output$targetTablePpmDeviation <- renderDataTable({
	if(is.null(input$targetFile) | is.null(input$targetAdduct)) return(data.frame())
	if(input$targetFile == '' | input$targetAdduct == '' | input$targetPpm == '') return(data.frame())
	res <- targetTableFunctionPpmDeviation(input$targetFile, input$targetAdduct)
	res <- apply(res, c(1, 2), function(x) round(x, digits=2))
}, selection='none', extensions='Scroller', class="display cell-border compact", options=list(dom='frtip', 
scrollX=TRUE, scrollY=input$dimension[2]/1.5, scroller=TRUE, deferRender=TRUE, bFilter=FALSE, ordering=FALSE, initComplete=htmlwidgets::JS(paste("
	function(){
		var api = this.api();
		if(", input$targetTable_cell_selected$C, " != 0){
			var $cell = api.cell(", 
				paste(isolate(input$targetTable_cell_selected$C)-8, isolate(input$targetTable_cell_selected$Cl)-3, sep=', '), 
				").nodes().to$();
			$cell.addClass('selected');
		}
		var ppm = Number(document.getElementById('targetPpm').value);
		api.cells().every(function(){
			if(this.data() != null & this.index().column != 0 & this.data() <= ppm){
				this.nodes().to$().css('background-color', 'green');
			}
			else if(this.data() != null & this.index().column != 0){
				this.nodes().to$().css('background-color', 'red');
			}
		});
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
	Shiny.addCustomMessageHandler('targetTableSelectPpm', reselect);
	function reselect(message){
		console.log('ppm');
		table.$('td.selected').removeClass('selected');
		var $cell = table.cell(message.row, message.column).nodes().to$();
		$cell.addClass('selected');
		//table.cell(message.row, message.column).data(message.ppm);
	};
	Shiny.addCustomMessageHandler('targetTablePpmUpdate', update);
	function update(message){
		console.log('update ppm table');
		table.cell(message.row, message.column).data(message.ppm);
	};
	Shiny.addCustomMessageHandler('targetTablePpmDelete', deleteCell);
	function deleteCell(message){
		console.log('delete ppm table');
		table.cell(message.row, message.column).data(null);
		table.cell(message.row, message.column).nodes().to$().css('background-color', '');
		table.$('td:selected').removeClass('selected');
		Shiny.onInputChange('targetTable_cell_selected', {C:0, Cl:0});
	};
"))
