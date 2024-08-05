# to keep trace between two reactive environnement 
# (between the observe event & the renderDataTable)
# will contain three columns: project_sample, sample & sample_id
share_vars$file_associate_add <- data.frame()

# to keep trace between two reactive environnement 
# (between the observe event & the renderDataTable)
# will contain two columns: File & Label
share_vars$file_import_success <- data.frame()

#' @title File association event
#'
#' @description
#' File association event when user try to import files from others projects
#' users can also delete files from here
#' check that the polarity of the files to associate are the same than the other files in project
#' open a modal if there is files to asssociate with project to let users give new sample ids (or to keep the same)
#' 
#' @param input$project integer, project id
#' @param input$files_from_db vector of integers, project sample ids of sample to import
#' @param db sqlite connection
#' @param project_samples reactive value, project_samples table with at least columns:
#' \itemize{
#'      \item project_sample integer, project_sample ID
#'      \item project integer, project ID of table project
#'      \item sample string, sample ID of table sample
#'      \item sample_id string, other names used for sample in project
#'}
#' @param projects reactive value, projects table with at least columns:
#' \itemize{
#'      \item project integer, project id
#'      \item name string, project name
#'}
#' @param samples reactive value, samples table with at least columns:
#' \itemize{
#'      \item sample string, sample ID
#'      \item polarity string, "negative" or "positive"
#'}
shiny::observeEvent(input$file_associate, {
	print('############################################################')
	print('######################### FILE_ASSOCIATE ###################')
	print('############################################################')
		
	params <- list(
		project = input$project, 
		project_samples = input$files_from_db
	)
	print(params)
	
	tryCatch({
	if (is.null(params$project)) custom_stop("invalid", 
		"You must create a sequence before importing files")
	else if (params$project == "") custom_stop("invalid", 
		"You must create a sequence before importing files")
	
	files_in_project <- project_samples()[which(
		project_samples()$project == params$project), 
			c("project_sample", "sample", "sample_id")]
	params$files <- project_samples()[which(
		project_samples()$project_sample %in% params$project_samples), 
			c("project_sample", "sample", "sample_id")]
	to_add <- params$files[which(!params$files$sample %in% files_in_project$sample), ]
	to_delete <- files_in_project[which(!files_in_project$sample %in% params$files$sample), ]
	print(list(
		to_add = to_add,
		to_delete = to_delete
	))
	
	if (nrow(to_delete) > 0) {
		delete_project_samples(db, to_delete$project_sample)
		toastr_success(sprintf("%s removed from sequence %s", 
			paste(to_delete$sample_id, collapse = ", "), 
			projects()[which(projects()$project == params$project), "name"]))
	}
	if (nrow(to_add) > 0) {
		# check if polarity of file to add respect the same polarity than other files already present in project
		if (nrow(files_in_project) > nrow(to_delete)) {
			project_polarity <- samples()[which(
				samples()$sample %in% project_samples()[which(
					project_samples()$project == input$Sequence), 
				"sample"]), "polarity"][1]
			to_add_polarities <- samples()[which(
				samples()$sample %in% to_add$sample), "polarity"]
			test_polarities <- which(to_add_polarities != project_polarity)
			if (any(test_polarities)) custom_stop("invalid", sprintf(
				"%s have a %s polarity instead of %s polarity 
				of files in sequence %s", 
				paste(to_add[which(test_polarities), "sample_id"], collapse = ", "), 
				projects()[which(projects()$project == params$project), "name"]))
		}
		
		share_vars$file_associate_add <- to_add
		shiny::showModal(shiny::modalDialog(
			title = '',
			DT::dataTableOutput('file_associate_table'),
			footer = shiny::div(
				shinyWidgets::actionBttn('file_associate_cancel', 'Cancel', style = 'minimal', color = 'primary'),
				shinyWidgets::actionBttn('file_associate_valid', 'Valid', style = 'minimal', color = 'primary')
			),
			size = "l"
		))
	} else {
		print('############################################################')
		print('######################### END FILE_ASSOCIATE ###############')
		print('############################################################')
	}
	}, invalid = function(i) {
		print(i) 
		toastr_error(i$message)
	}, error = function(e) {
		print(e)
		sweet_alert_error(e$message)
	})
})

#' @title Table for sample IDs
#'
#' @description
#' Table to display for the user to give sample IDs
#' sample IDs are limited to 30 characters (limited by excel for sheet names)
#' the click on the valid button will parse all the column "sample id" to send it to shiny
#'
#' @param share_vars$file_from_db_add reactive value 
#'       contain three columns: project_sample, sample & sample_id
#'
#' @return datatable with columns:
#' \itemize{
#'    \item File string, file names
#'    \item Label string, sample IDs 
#'}
output$file_associate_table <- DT::renderDataTable({
	tryCatch({
	if (nrow(share_vars$file_associate_add) == 0) custom_stop("invalid", "")
	params <- list(
		sample_names = share_vars$file_associate_add$sample, 
		sample_ids = share_vars$file_associate_add$sample_id
	)
	
	data.frame(
		File = params$sample_names, #c'est ici que je dois changer la taille pour le nom usuel du fichier ) 
		Label = paste('<input type=\"text\" value=\"', 
				params$sample_ids, 
			'\" maxlength=30 width=\"100%\" required>', sep=''
		)
	)
	}, invalid = function(i) data.frame(matrix(, nrow = 0, ncol = 2, 
		dimnames = list(c(), c('File', 'Label'))), check.names = FALSE)
	, error = function(e){
		print('ERR file_from_db_table')
		print(e)
		sweet_alert_error("Cannot display table with all files to import", e$message)
		data.frame(matrix(, nrow = 0, ncol = 2, dimnames = list(c(), c('File', 'Label'))), 
			check.names = FALSE)
	})
}, escape = FALSE, selection = 'none', rownames = FALSE, extensions = "Scroller", 
options = list(dom = 'frtip', fixedColumns = TRUE, bFilter = FALSE, paging = FALSE, 
ordering = FALSE, info = FALSE, scrollX = TRUE, scrollY = "70%", scrollCollapse = TRUE, 
columnDefs = list(list(className = "dt-head-center dt-center", 
	targets = "_all"))), 
callback = DT::JS('
	$(document).on("click", "#file_associate_valid", function() {
		var sample_ids = $("#file_associate_table input").toArray().map(x => x.value);
		if (sample_ids.some(x => x.length > 30)) {
			toastr.error("sample_ids cannot contain more than 30 characters", "", 
				{positionClass: "toast-top-center",
				closeButton: true, 
				newestOnTop: true, 
				preventDuplicates: true
			})
		} else {
			Shiny.onInputChange("file_associate_sample_id", sample_ids);
			Shiny.onInputChange("file_associate_valid2", Math.random());
		}
	});
'))

#' @title Cancel asssociation file button event
#'
#' @description 
#' remove modal dialog when user click on cancel button
#'
#' @param input$file_associate_cancel integer, shiny button
shiny::observeEvent(input$file_associate_cancel, {
	shiny::removeModal()
	print('############################################################')
	print('######################### END FILE_ASSOCIATE ###############')
	print('############################################################')
})

#' @title Association of file(s) to project event
#'
#' @description
#' Associate file(s) to project with sample ids give by the user
#'
#' @param input$project integer, project id
#' @param share_vars$file_associate_add reactive value, 
#'      contain three columns: project_sample, sample & sample_id
#' @param input$file_associate_sample_id vector of strings, sample idss
#' @param db sqlite connection
#' @param projects reactive value, projects table with at least columns:
#' \itemize{
#'      \item project integer, project id
#'      \item name string, project name
#'}
shiny::observeEvent(input$file_associate_valid2, {
	shiny::removeModal()
	params <- list(
		project = input$project, 
		sample_names = share_vars$file_associate_add$sample, 
		sample_ids = input$file_associate_sample_id
	)
	
	tryCatch({
		sapply(1:length(params$sample_names), function(i) 
			record_project_sample(db, params$project, 
				params$sample_names[i], params$sample_ids[i])) 
		toastr_success(sprintf("%s imported in sequence %s", 
			paste(params$sample_ids, collapse = ", "), 
			projects()[which(projects()$project == params$project), "name"]))
	}, error = function(e) {
		print(e)
		sweet_alert_error(e$message)
	})
	print('############################################################')
	print('######################### END FILE_ASSOCIATE ###############')
	print('############################################################')
})

# Declaration of shinyFilesChoose button
shinyFiles::shinyFileChoose(input, 'file_import', roots = volumes, 
	filetypes = c('mzML', 'mzXML', 'CDF', 'RAW', 'd', 'YEP', 'BAF', 'FID', 'WIFF', 'MGF'))

#' @title File import event
#'
#' @description 
#' open a modal for user to give sample ids for each file imported in a datatable
#' if files were already imported in current project the polarity is locked
#' 
#' @param input$file_import shinyFiles value, contain name & path of file(s) selected by user
#' @param volumes vector of strings, path of directories & disks to display
#' @param input$project integer, id of the current project
#' @param samples reactive value, sample table of database with at least columns:
#' \itemize{
#'      \item sample string, sample ID
#'      \item polarity string, "negative" or "positive"
#'}
#' @param project_samples reactive value, project_sample table of database with at least columns:
#' \itemize{
#'      \item project_sample integer, project_sample ID
#'      \item project integer, project ID of table project
#'      \item sample string, sample ID of table sample
#'      \item sample_id string, other names used for sample in project
#'}
shiny::observeEvent(input$file_import, {
	# if no file(s) was choose
	if (is.integer(input$file_import)) return()
	
	print('############################################################')
	print('######################### FILE_IMPORT ######################')
	print('############################################################')
	params <- list(
		filepaths = gsub('"', "'", 
			shinyFiles::parseFilePaths(volumes, input$file_import)$datapath)
	)
	print(params)
	
	tryCatch({
		if (is.null(input$project)) custom_stop("invalid", "You must create a sequence 
			before importing files")
		else if (input$project == "") custom_stop("invalid", "You must create a sequence 
			before importing files")
		
		project_polarity <- samples()[which(
			samples()$sample %in% project_samples()[which(
				project_samples()$project == input$project), 
			"sample"]), "polarity"][1]
		
		shiny::showModal(shiny::modalDialog(
			title = 'Only negatives scans can be imported !!',
			#shiny::div("Only negatives scans can be imported !!"),
			DT::dataTableOutput('file_import_table'),
			footer = shiny::div(
				shiny::column(width=4,
					HTML("You can't add \"<b>:</b>\", \"<b>?</b>\", \"<b>/</b>\", \"<b>\\</b>\", \"<b>*</b>\", \"<b>[</b>\", \"<b>]</b>\", \"<b>'</b>\" in your sample names !!")
				),
				shiny::column(width=4),
				shiny::column(width=4,
					shinyWidgets::actionBttn('file_import_cancel', 'Cancel', style = 'minimal', color = 'primary'),
					shinyWidgets::actionBttn('file_import_valid', 'Valid', style = 'minimal', color = 'primary')
				)
			),
			size = 'l'
		))
		if (!is.na(project_polarity)) shinyjs::disable("file_polarity")
	}, invalid = function(i) {
		print(i)
		toastr_error(i$message)
	} , error = function(e){
		print(e)
		sweet_alert_error("Cannot create sequence", e$message)
	})
	print('############################################################')
	print('######################### END FILE_IMPORT ##################')
	print('############################################################')
})

#' @title Table for sample IDs
#'
#' @description
#' Table to display for the user to give sample IDs
#' sample IDs are limited to 30 characters (limited by excel for sheet names)
#' default sample ID begin with `pos ` or `neg ` according polarity choosen, 
#'	   followed with the name of the file without its extension
#' the click on the valid button will parse all the column "sample id" to send it to shiny
#'
#' @param input$file_import shinyFiles value, contain name & path of file(s) selected by user
#' @param input$file_polarity string, can only be "negative" or "positive"
#'
#' @return datatable with columns:
#' \itemize{
#'    \item File string, file names
#'    \item Label string, sample IDs 
#'}
output$file_import_table <- DT::renderDataTable({
	tryCatch({
		if (is.integer(input$file_import)) custom_stop("invalid", "")
		params <- list(
			filenames = gsub('"', "'", 
				shinyFiles::parseFilePaths(volumes, input$file_import)$name)
		)
	
		data.frame(
			File = params$filenames,  
			Label = paste('<input type=\"text\" value=\"', 
				stringr::str_trunc(
					paste(
						#stringr::str_trunc(input$file_polarity, 3, ellipsis=""),
						tools::file_path_sans_ext(params$filenames)
					), 30, ellipsis=""), 
				'\" maxlength=30 width=\"100%\" pattern="[^:\'[\\]?\\/\\\\*]+" required/>', sep=''
			)
		)
		}, invalid = function(i) data.frame(matrix(, nrow = 0, ncol = 2, dimnames = list(c(), 
			c('File', 'Label'))), check.names = FALSE)
		, error = function(e){
			print('ERR file_import_table')
			print(e)
			toastr_error(e$message)
			data.frame(matrix(, nrow = 0, ncol = 2, dimnames = list(c(), c('File', 'Label'))), 
				check.names = FALSE)
	})
}, escape = FALSE, selection = 'none', rownames = FALSE, extensions = "Scroller", 
options = list(dom = 'frtip', fixedColumns = TRUE, bFilter = FALSE, paging = FALSE, 
	ordering = FALSE, info = FALSE, scrollX = TRUE, scrollY = "70%", scrollCollapse = TRUE, 
	columnDefs = list(list(className = "dt-head-center dt-center", targets = "_all"))),
callback = DT::JS("
	$(document).on('click', '#file_import_valid', function() {
		var sample_ids = $('#file_import_table input').toArray().map(x => x.value);
		Shiny.onInputChange('file_import_sample_id', sample_ids);
		Shiny.onInputChange('file_import_valid2', Math.random());
	});
")
)

#' @title Cancel importation file button event
#'
#' @description 
#' remove modal dialog when user click on cancel button
#'
#' @param input$file_import_cancel integer, shiny button
shiny::observeEvent(input$file_import_cancel, {
	shiny::removeModal()
	print('############################################################')
	print('######################### END IMPORT FILES #################')
	print('############################################################')
})

#' @title Importation file event
#'
#' @description
#' importation event
#' launch a suite of functions
#' \itemize{
#'    \item conversion() convert files to mzXML format & call thermoReader
#'    \item importation() check if file can be read & polarity is good
#'    \item cut_ms_file() only if multiple scans with different polarities are founded, 
#'       it will cut the file according the polarity choosen
#'    \item get_ms_file_infos() get infos from the ms file with MSnbase & thermoReader
#'    \item record_sample() record sample in database
#'    \item record_project_sample() associate sample & project in database
#' }
#' Display at the end a modal with a datatable to resume for each file imported if it was success or not
#'
#' @param input$file_import_valid2 float, only to launch event
#' @param db sqlite connection
#' @param input$project integer, project id
#' @param input$file_import shinyFiles value, contain name & path of file(s) selected by user
#' @param input$file_import_sample_id vector of strings, sample IDs given by user
#' @param input$file_polarity string, can only be "negative" or "positive"
#' @param project_samples reactive value, project_samples table with at least columns:
#' \itemize{
#'      \item project_sample integer, project_sample ID
#'      \item project integer, project ID of table project
#'      \item sample string, sample ID of table sample
#'}
shiny::observeEvent(input$file_import_valid2, {
	#shiny::removeModal()
	params <- list(
		project = input$project, 
		filepaths = gsub('"', "'", 
			shinyFiles::parseFilePaths(volumes, input$file_import)$datapath), 
		filenames = gsub('"', "'", 
			shinyFiles::parseFilePaths(volumes, input$file_import)$name), 
		sample_ids = gsub('"', "'", input$file_import_sample_id), 
		polarity = "negative" # set it to negative here because only negatives are done
	)
	print(params)

	tryCatch({
		if(length(params$project) == 0) custom_stop('error', 'You must select a sequence')
		inputs <- c('sample_ids', 'sample_ids')
		conditions <- c(!(is.null(params$sample_ids)), length(grep("[][:*?/\\']", params$sample_ids, perl = TRUE)) == 0)
		messages <- c("You have to write a sample name", "You can't add special characters listed in your sample names")
		check_inputs(inputs, conditions, messages) 

		# Hide the modal if tests are good
		shiny::removeModal()

		params$sample_names <- stringr::str_trunc(
			paste(
				stringr::str_trunc(params$polarity, 3, ellipsis=""),
				tools::file_path_sans_ext(params$filenames)
			), 30, ellipsis="")
		success <- rep("Unknown error", length(params$filenames))
		shinyWidgets::progressSweetAlert(session, 'pb', title = 'file import / convert',
			value = 0, display_pct = TRUE)
		for(i in 1:length(params$filepaths)){
			msg <- paste('Import of', params$filenames[i], 
				'as', params$sample_ids[i])
			print(msg)
			shinyWidgets::updateProgressBar(session, id = 'pb', title = msg, 
				value = round((i - 1) * 100 / length(params$filepaths)))
			
			# Have to add the checking of the label given by user to not have multiple file labels with the same name => error in Excel export
			
			# check if file already exists in database
			success[i] <- tryCatch({
				if (params$sample_names[i] %in% project_samples()[which(
					project_samples()$project == params$project), "sample"]) custom_stop("invalid", 
						"File already in sequence")
				else if (params$sample_names[i] %in% project_samples()$sample) record_project_sample(
					db, params$project, params$sample_names[i], params$sample_ids[i])
				else {
					conversion(db, params$project, params$sample_names[i], 
						params$sample_ids[i], params$filepaths[i], params$filenames[i], 
						params$polarity)
					record_project_sample(db, params$project, params$sample_names[i], 
						params$sample_ids[i])
				}
				"success"
			}, error = function(e) {
				print(e)
				e$message
			})
			print('---')
		}
		share_vars$file_import_success <- data.frame(
			File = params$sample_ids, 
			Import = sapply(success, function(x) 
				if(x == "success") paste("<div style=\"background-color: #B3E2CD;\">", 
					shiny::icon("check-circle"), x, "</div>")
				else paste("<div style=\"background-color: #FDCDAC;\">", 
					shiny::icon("exclamation-circle"), x, "</div>")
			)
		)
		print(success)
		shinyWidgets::closeSweetAlert(session)
		shiny::showModal(shiny::modalDialog(title = 'Result of import', easyClose = TRUE,
			DT::dataTableOutput("file_import_success"),
			footer = modalButton('Close'),
			size = "l"
		))
	}, invalid = function(i){ NULL
	}, error = function(e){
		#shinyWidgets::closeSweetAlert(session)
		print(e)
		sweet_alert_error("Cannot import files to database", e$message)
	})
	print('############################################################')
	print('######################### END IMPORT FILES #################')
	print('############################################################')
})

#' @title Datatable of importation results
#'
#' @description
#' display a datatable with results of importation & colorize line if it a success or not
#'
#' @param share_vars$file_import_success reactive value, contain dataframe with the columns:
#' \itemize{
#'    \item file string, file name
#'    \item success string, "success" if it, else display error message
#' }
#'
#' @return datatable with two columns
#' \itemize{
#'    \item file string, file name
#'    \item success string, "success" if it, else display error message
#' }
output$file_import_success <- DT::renderDataTable(share_vars$file_import_success, 
	escape = FALSE, selection = 'none', rownames = FALSE, extensions = "Scroller", 
	options = list(dom = 'frtip', fixedColumns = TRUE, bFilter = FALSE, paging = FALSE, 
	info = FALSE, scrollX = TRUE, scrollY = "70%", scrollCollapse = TRUE, 
	columnDefs = list(list(
		className = "dt-head-center dt-center", targets = "_all"))))

#' @title Conversion
#'
#' @description
#' Call msconvert & thermoreader from proteowizard to convert file to mzXML
#' in case of failure, if file is mzML, mzXML or CDF it continue, otherwise reject an error
#' use proteowizard algotithm for centroidization for Waters directories
#' call thermoReader only if file came from Thermo
#' launch a suite of functions
#' \itemize{
#'    \item importation() check if file can be read & polarity is good
#'    \item cut_ms_file() only if multiple scans with different polarities are founded, 
#'       it will cut the file according the polarity choosen
#'    \item get_ms_file_infos() get infos from the ms file with MSnbase & thermoReader
#'    \item record_sample() record sample in database
#' }
#'
#' @param db sqlite connection
#' @param project integer, project id
#' @param sample_name string, sample name
#' @param sample_id string, sample id
#' @param filepath string, path of the file to convert
#' @param filename string, name of the file to convert
#' @param polarity string, can only be "negative" or "positive"
conversion <- function(db, project, sample_name, sample_id, filepath, filename, polarity) {
	out_dir <- tempdir()
	out_filename <- paste0(tools::file_path_sans_ext(filename), ".mzXML")
	out_filepath <- file.path(out_dir, out_filename)
	
	# if it is a WIFF file it needs its corresponding WIFF.SCAN
	if (grepl('WIFF$', filepath, ignore.case=TRUE) & 
		!file.exists(paste0(filepath, '.scan'))) stop(
			'missing corresponding wiff.scan file in same directory')
	
	# if it is CDF file cannot centroid it
	if (grepl('CDF$', filepath, ignore.case=TRUE)) return(importation(
		db, project, sample_name, sample_id, filepath, filepath, 
			if(polarity == 'negative') 0 else 1))
	
	print(paste('conversion of', filename))
	# if it is a Water repertory, don't use the vendor algorithm
	# And if it is already converted (mz(X)ML) also
	algorithm <- if (
		(grepl('raw$', filepath, ignore.case=TRUE) & 
			!dir.exists(filepath)) | 
		grepl('mzXML$', filepath, ignore.case=TRUE) | 
		grepl('mzML$', filepath, ignore.case=TRUE)) 'vendor' #'cwt' else 'vendor' cwt only for waters but raw is also thermo
	
	# call msConvert
	query <- sprintf("\"%s\" \"%s\" -o \"%s\" --outfile \"%s\" --mzXML --64 --zlib --filter \"peakPicking %s msLevel=1\" --filter \"polarity %s\"", 
		converter, filepath, out_dir, out_filename, algorithm, polarity)
	print(query)
	msconvert_blabla <- system(query, intern = TRUE, wait = TRUE)
	print(msconvert_blabla)

	if (!file.exists(out_filepath)) {
		print('conversion failed')
		if (grepl('mzXML$', filepath, ignore.case=TRUE) | 
			grepl('mzML$', filepath, ignore.case=TRUE)) return(importation(
				db, project, sample_name, sample_id, filepath, filepath, 
				if(polarity == 'negative') 0 else 1))
		else stop(msconvert_blabla[length(msconvert_blabla)])
	}
	
	thermo_file <- if (grepl('raw$', filepath, ignore.case=TRUE) & 
				!dir.exists(filepath)){
			tmp_file <- file.path(out_dir, 
				paste0(tools::file_path_sans_ext(filename), ".txt"))
			query <- sprintf("\"\"%s\" --scanTrailers \"%s\" > \"%s\"\"",
				thermo, filepath, tmp_file)
			print(query)
			shell(query)
			if (file.exists(tmp_file)) tmp_file 
			else NULL
		} else NULL
		
	importation(db, project, sample_name, sample_id, out_filepath, filepath, 
		if(polarity == 'negative') 0 else 1, thermo_file)
}

#' @title Importation
#'
#' @description
#' try to read the file & check if polarity is good
#' if it can detect scans with polarity desired reject an error
#' if it detect multiple scans with different polarities it will call the function cut_ms_file()
#' launch a suite of functions
#' \itemize{
#'    \item cut_ms_file() only if multiple scans with different polarities are founded, 
#'       it will cut the file according the polarity choosen
#'    \item get_ms_file_infos() get infos from the ms file with MSnbase & thermoReader
#'    \item record_sample() record sample in database
#' }
#'
#' @param db sqlite connection
#' @param project integer, project id
#' @param sample_name string, sample name
#' @param sample_id string, sample id
#' @param filepath string, path of the ms file
#' @param old_filepath string, path of the old file converted
#' @param polarity integer, can only be 0 (negative) or 1(positive)
#' @param thermo_file string, path to the file generated by thermoReader (only if file was a raw Thermo)
importation <- function(db, project, sample_name, sample_id, filepath, old_filepath, 
		polarity, thermo_file = NULL) {
	print(paste('import of', sample_name))
	
	# check if it can be read
	ms_file <- MSnbase::readMSData(filepath, msLevel = 1, mode = 'onDisk')
	if (length(ms_file) == 0) stop("no readable scans in file")
	
	# commented because it will consume too much RAM
	# check if it is centroided
	# centroided <- which(MSnbase::isCentroided(ms_file))
	# gc()
	# if(length(centroided) == 0) return('file is not centroided, it will slower the application')
	
	# chek if polarity is good
	real_polarity <- if (grepl('cdf$', filepath, ignore.case=TRUE)) polarity
		else unique(MSnbase::polarity(ms_file))
	print(sprintf("detect polarity %s", paste(real_polarity, collapse = ", ")))
	
	if (is.na(real_polarity)) real_polarity <- polarity
	if (length(real_polarity) > 1) importation(db, project, sample_name, sample_id, 
		cut_ms_file(ms_file, polarity), old_filepath, polarity, thermo_file)
	else if(polarity == 1 & real_polarity < 1) stop('no positive scans detected')
	else if(polarity == 0 & real_polarity > 0) stop('no negative scans detected')
	else record_sample(db, project, sample_name, sample_id, filepath, old_filepath, polarity, 
		ms_file, thermo_file)
}

#' @title Split Scans in MS file
#'
#' @description
#' split ms file according polarity of scans & the polarity desired
#' record a new mzXML file with scans of the polarity desired
#'
#' @param ms_file OnDiskMSnExp, ms file object read by MSnbase pkg
#' @param polarity integer, can only be 0 (negative) or 1(positive)
#'
#' @return string path of the file created
cut_ms_file <- function(ms_file, polarity) {
	print('Cut ms file')
	spectras <- if(polarity == 1) which(MSnbase::polarity(ms_file) >= polarity)
		else which(MSnbase::polarity(ms_file) <= polarity)
	out_filepath <- file.path(tempdir(), basename(MSnbase::fileNames(ms_file)))
	MSnbase::writeMSData(ms_file[spectras], out_filepath, copy=TRUE)
	rm(msFile)
	gc()
	out_filepath
}

#' @title Get various informations of ms file
#' 
#' @description
#' Get various informations of ms file by reading it with MSnExp & thermoReader
#'
#' @param ms_file OnDiskMSnExp, ms file object read by MSnbase pkg
#' @param thermo_file string, path to the file generated by thermoReader (only if file was a raw Thermo)
#'
#' @return list
#' \itemize{
#'	   \item instrument_model
#'	   \item instrument_manufacturer
#'	   \item software_name
#'	   \item software_version
#'	   \item ion_source
#'	   \item analyzer
#'	   \item detector_type
#'	   \item resolution
#'	   \item agc_target
#'	   \item maximum_it
#'	   \item number_of_scan_range
#'	   \item scan_range
#'	   \item mz_min
#'	   \item mz_max
#'}
get_ms_file_infos <- function(ms_file, thermo_file = NULL) {
	ms_file_infos <- ms_file@experimentData
	thermo_file_infos <- if (is.null(thermo_file)) "" 
		else readLines(thermo_file)
	
	infos <- list(
		instrument_model = ms_file_infos@instrumentModel, 
		instrument_manufacturer  = ms_file_infos@instrumentManufacturer, 
		software_name = ms_file_infos@softwareName, 
		software_version = ms_file_infos@softwareVersion, 
		ion_source = ms_file_infos@ionSource, 
		analyzer = ms_file_infos@analyser, 
		detector_type = ms_file_infos@detectorType,
		resolution = thermo_file[which(grepl("Resolution", thermo_file))][1], 
		agc_target = thermo_file[which(grepl("AGC [Tt]arget", thermo_file))][1], 
		maximum_it = thermo_file[which(grepl(
			"Maximum I(njection)?[[:space:]]?T(ime)?", thermo_file))][1], 
		number_of_scan_range = thermo_file[which(grepl(
			"Number of scan ranges", thermo_file))][1], 
		scan_range = thermo_file[which(grepl("Scan [Rr]ange", thermo_file))][1],
		mz_min =  range(MSnbase::mz(ms_file))[1],
		mz_max =  range(MSnbase::mz(ms_file))[2]
	)
	
	if (!is.null(infos$resolution)) infos$resolution <- stringr::str_extract(
		infos$resolution, "[[:digit:]]+([.,EeKk][[:digit:]]*)?")
	if (!is.null(infos$agc_target)) infos$agc_target <- stringr::str_extract(
		infos$agc_target, "[[:digit:]]+([.,EeKk][[:digit:]]*)?")
	if (!is.null(infos$maximum_it)) {
		unit_maximum_it <- stringr::str_extract(infos$maximum_it, 
			"(([[:alpha:]]+$)|((?<=\\()[[:alpha:]]+(?=\\))))")
		infos$maximum_it <- paste(stringr::str_extract(infos$maximum_it, "[[:digit:]]+"), 
			unit_maximum_it)
	}
	if (!is.null(infos$number_of_scan_range)) infos$number_of_scan_range <- stringr::str_extract(
		infos$number_of_scan_range, "[[:digit:]]+([.,EeKk][[:digit:]]*)?")
	if (!is.null(infos$scan_range)) infos$scan_range <- paste(paste(
		stringr::str_extract_all(infos$scan_range, "[[:digit:]]+")[[1]], 
		collapse = "-"), "m/z")
	
	infos <- lapply(infos, function(x) if (length(x) == 0) NA else x)
	lapply(infos, function(x) gsub('"', "'", x))
}
