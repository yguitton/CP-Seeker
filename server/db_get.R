#' @title Load MS file
#'
#' @description
#' Load MS file from database
#' first try to decompress the blob in the database
#' if doesn't work try to retrieve & read old files
#' 
#' @param db sqlite connection
#' @param sampleID string, id of the sample
#' 
#' @return xcmsRaw object
load_ms_file <- function(db, sampleID) {
	query <- sprintf("select raw_data from sample where sample == \"%s\";", sampleID)
	print(query)
	ms_file <- tryCatch(unserialize(fst::decompress_fst(unlist(
		db_get_query(db, query)$raw_data))), error = function(e) NULL)
	if (is.null(ms_file)) {
		# try to found it in the mzXML directory
		query <- sprintf("select path, raw_path from sample 
			where sample == \"%s\";", sampleID)
		print(query)
		paths <- unlist(db_get_query(db, query))
		paths <- paths[which(file.exist(paths))]
		if (length(paths) == 0) stop(sprintf("\"%s\" is not found in database", sampleID))
		else {
			ms_file <- tryCatch(xcms::xcmsRaw(paths[1], mslevel = 1, profstep = 0), 
				error = function(e) NULL)
			if (is.null(ms_file) & length(path) > 1) {
				ms_file <- tryCatch(xcms::xcmsRaw(paths[1], mslevel = 1, profstep = 0), 
					error = function(e) NULL)
				if (is.null(ms_file)) stop(sprintf("\"%s\" is not found in database", sampleID))
			} else if (is.null(ms_file)) stop(sprintf("\"%s\" is not found in database", sampleID))
		}
	}
	ms_file
}

#' @title Get polarity in project
#' 
#' @description
#' Get polarity of files in project (only one is possible)
#' 
#' @param db sqlite connection
#' @param project integer, project id
#'
#' @return string, "positive" or "negative"
get_project_polarity <- function(db, project) {
	query <- sprintf("select polarity from sample where sample == (
		select sample from project_sample where project == %s limit 1);", 
		project)
	print(query)
	db_get_query(db, query)$polarity
}

#' @title Get project name
#'
#' @description 
#' Get project name
#'
#' @param db sqlite connection
#' @param project integer, project id
#' 
#' @return string, project name
get_project_name <- function(db, project = NULL) {
	if (is.null(project)) return("")
	query <- sprintf("select name from project where project == %s;", project)
	print(query)
	project_name <- db_get_query(db, query)$name
	if (is.na(project_name)) "" else project_name
}

#' @title Get sample names
#'
#' @description
#' Get sample ids & sample labels for a project
#'
#' @param db sqlite connection
#' @param project integer, project id
#' @param project_samples vector of integers, project_sample IDs
#'
#' @return dataframe with columns:
#' \itemize{
#'      \item project_sample integer, project_sample ID
#'      \item sample string, sample ID
#'      \item sample_id string, sample id given by user
#' }
get_samples <- function(db, project = NULL, project_samples = NULL) {
	if (!is.null(project)) query <- sprintf(
		"select project_sample, sample_id, sample from project_sample where project == %s", 
			project)
	else if (!is.null(project_samples)) query <- sprintf(
		"select project_sample, sample_id, sample from project_sample 
			where project_sample in (%s)", 
			paste(project_samples, collapse = ", "))
	else return(data.frame())
	print(query)
	db_get_query(db, query)
}

#' @title Get all chloroparaffin ions
#' 
#' @description
#' Get all chloroparaffin ions given an adduct name
#' 
#' @param db sqlite connection
#' @param adduct_name string adduct name
#'
#' @return dataframe with columns:
#' \itemize{
#' 		\item chloroparaffin_ion integer chloroparaffin_ion ID
#' 		\item ion_formula string ion formula
#' 		\item charge integer charge of ion
#' }
get_chloroparaffin_ions <- function(db, adduct_name = NULL) {
	if (is.null(adduct_name)) return(data.frame())
	query <- sprintf("select chloroparaffin_ion, ion_formula, charge 
		from chloroparaffin_ion where adduct == \"%s\";", adduct_name)
	print(query)
	db_get_query(db, query)
}