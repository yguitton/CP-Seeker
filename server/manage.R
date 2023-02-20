#' @title Table in manage tab 
#'
#' @description
#' table in manage tab to display some table of databases : "Project", "Sample"
#' For the table sample it merges with the table project & project_samples so every line will be a project_sample
#' possibility to change sample id or name of projects by dbl click on the cell
#'
#' @param input$manage_select string, can be "Sample", "Project"
#' @param projects reactive value, projects table with at least columns:
#' \itemize{
#'      \item project integer, project id
#'      \item name string, project name
#'      \item comments string, project comments
#'      \item creation date, date of creation
#'      \item modified date, last date of project modification
#'}
#' @param samples reactive value, samples table with at least columns:
#' \itemize{
#'      \item sample string, sample ID
#'      \item size string size of the blob stored
#'      \item raw_path string, old path of the file
#'      \item polarity string, "negative" or "positive"
#'      \item path string, path is file was stored as mzXML in app
#'      \item instrument_model string, instrument model
#'      \item instrument_manufacturer string, instrument manufacturer
#'      \item software_name string, name of software
#'      \item software_version string, version of software
#'      \item ion_source string, ion source
#'      \item analyzer string, analyzer
#'      \item detector_type string, type of detector
#'      \item resolution string, resolution of instrument
#'      \item agc_target string, AGC target
#'      \item maximum_it string, maximum IT
#'      \item number_of_scan_range string, number of scans
#'      \item scan_range string, scan range
#'}
#' @param project_samples reactive value, project_samples table with at least columns:
#' \itemize{
#'      \item project_sample integer, project_sample ID
#'      \item project integer, project ID of table project
#'      \item sample string, sample ID of table sample
#'      \item sample_id string, other names used for sample in project
#'}
#'
#' @return datatable with column according the choice of table:
#' \itemize{
#'    \item Project: \itemize{
#'         \item Name string, name of the project
#'         \item Comments string, project comments
#'         \item Creation date, date of creation
#'         \item Last modified date, last date of project modification
#'     }
#'    \item Sample: \itemize{
#'         \item project_sample integer, project_sample ID
#'         \item Sample string, sample_id
#'         \item Project string, project name
#'         \item Polarity string, "negative" or "positive"
#'         \item Size (Mo) string size of the blob stored
#'         \item Instrument model string, instrument model
#'         \item Instrument manufacturer string, instrument manufacturer
#'         \item Ion source string, ion source
#'         \item Analyzer string, analyzer
#'         \item Detector type string, type of detector
#'         \item Resolution string, resolution of instrument
#'         \item AGC target string, AGC target
#'         \item Maximum IT string, maximum IT
#'         \item Number of scan range string, number of scans
#'         \item Scan range string, scan range
#'         \item Original path string, old path of the file
#'     }
#' } 
output$manage_table <- DT::renderDataTable({
	params <- list(
		table_selected = input$manage_select
	)

	tryCatch(
	if (params$table_selected == "Sequence") {
		data <- projects()
		data <- data[order(data$project, decreasing = TRUE), 
			c("project", "name", "comments", "creation", "modified")]
		colnames(data) <- c("Sequence", "Name", "Comments", "Creation", "Last modified")
		data
	} else if (params$table_selected == "Sample") {
		data <- samples()
		data <- merge(data, project_samples(), by = "sample")
		data <- merge(data, projects(), by = "project")
		data <- data[, c("project_sample", "sample_id", "name", "polarity", 
			"size", "instrument_model", "instrument_manufacturer", "ion_source", 
			"analyzer", "detector_type", "resolution", "agc_target", "maximum_it", 
			"number_of_scan_range", "scan_range", "raw_path")]
		data[, c("name", "polarity", "instrument_model", "instrument_manufacturer", 
				"ion_source", "analyzer", "detector_type", "resolution", "agc_target", 
				"maximum_it", "number_of_scan_range", "scan_range")] <- lapply(data[, 
			c("name", "polarity", "instrument_model", "instrument_manufacturer", 
			"ion_source", "analyzer", "detector_type", "resolution", "agc_target", 
			"maximum_it", "number_of_scan_range", "scan_range")], as.factor)
		colnames(data) <- c("project_sample", "Sample", "Project", "Polarity", 
			"Size (Mo)", "Instrument model", "Instrument manufacturer", "Ion source", 
			"Analyzer", "Detector type", "Resolution", "AGC target", "Maximum IT", 
			"Number of scan range", "Scan range", "Original path")
		data
	} else data.frame()
	, error = function(e){
		print("ERR manage_table")
		print(e)
		sweet_alert_error('Cannot display the table', e$message)
		data.frame()
	})
}, selection = 'none', rownames = FALSE, filter = 'top', 
extensions = c("Scroller", "Buttons"), 
options = list(dom = 'frtip', fixedColumns = TRUE, scrollX = TRUE, 
scrollCollapse = TRUE, buttons = list(list(text = "Column display", extend = 'colvis')), 
columnDefs = list(list(className = "dt-head-center dt-center", targets = "_all")), 
initComplete = htmlwidgets::JS("
	function(settings, json){
		var select = $('#manage_select .active').get(0).innerText,
			table = settings.oInstance.api();
		if (select == 'Sample' || select == 'Project') {
			table.column(0).visible(false);
		}
	}
"), language = list(emptyTable = "No entries in database")), 
callback = htmlwidgets::JS("
	table.on('click', 'tbody tr', function(){
		$(this).toggleClass('selected');
		var ids = table.rows('.selected').data().toArray().map(x => x[0]),
			ids = ids.length == 0 ? 0 : ids
		Shiny.onInputChange('manage_table_selected', ids);
	});
	table.on('dblclick', 'tbody td', function(){
		var select = $('#manage_select .active').get(0).innerText,
			index = table.cell(this).index();
		if ((select == 'Project' || select == 'Sample') && 
				index.column == 1){
			var $input = $('<input type = \"text\">'),
				value = table.cell(this).data(),
				$this = $(this),
				html = $this.html();
			$input.val(value);
			$this.empty().append($input);
			$input.css('width', '100%').focus().on('change', function(){
				var valueNew = $input.val();
				if(valueNew != value){
					Shiny.onInputChange('manage_table_rename', {
						id : table.cell(index.row, 0).data(),
						val : valueNew});
				}
				$input.remove();
				table.cell(index.row, index.column).data(valueNew);
			})
		}
	});
"))

#' @title Rename database entry event
#' 
#' @description
#' Event when user dbl click on a cell of columns "Project" or "Sample"
#' allow to rename entry in database
#'
#' @param input$manage_select string, can be "Sample", "Project"
#' @param input$manage_table_rename$id integer or string, project_sample ID or project ID
#' @param input$manage_table_rename$val string, new sample_id or new project name
shiny::observeEvent(input$manage_table_rename, {
	print('############################################################')
	print('####################### MANAGE TABLE RENAME ################')
	print('############################################################')
	params <- list(
		table = input$manage_select,
		id = input$manage_table_rename$id,
		name = input$manage_table_rename$val
	)
	print(params)
	
	tryCatch({
		if (params$table == "Sequence") rename_project(db, params$id, params$name)
		else if (params$table == "Sample") rename_project_sample(db, 
			params$id, params$name)
		toastr_success(sprintf('renamed to %s', params$name))
	}, error = function(e){
		print(e)
		sweet_alert_error('Cannot rename', e$message)
	})
	print('############################################################')
	print('####################### END MANAGE TABLE RENAME ############')
	print('############################################################')
})

#' @title Delete database entry event
#' 
#' @description
#' Delete database entry event
#' if delete project entries, will also delete all project_samples associated
#' 
#' @param input$manage_select string, can be "Sample", "Sequence"
#' @param input$manage_table_selected vector of integers or strings, project_sample IDs or project IDs
shiny::observeEvent(input$manage_delete, {
	print('############################################################')
	print('######################### MANAGE DELETE ####################')
	print('############################################################')
	params <- list(
		table = input$manage_select, 
		selected = input$manage_table_selected
	)
	print(params)
	
	tryCatch({
	if (length(params$selected) == 0) custom_stop('invalid', 
		"Please select at least a row")
	else if (params$selected[1] == 0) custom_stop('invalid', 
		"Please select at least a row")
	
	if (params$table == "Sequence") delete_projects(db, params$selected)
	else if (params$table == "Sample") delete_project_samples(db, params$selected)
	
	toastr_success("Entry(ies) deleted")
	}, invalid = function(i) {
		print(i)
		toastr_error(i$message)
	}, error = function(e) {
		print(e)
		sweet_alert_error('Cannot delete entry(ies)', e$message)
	})
	print('############################################################')
	print('######################### END MANAGE DELETE ################')
	print('############################################################')
})
