#' @title Export lighten database
#'
#' @description
#' Export a version of the actual database but lighted (without sample blobs)
#'
#' @param db sqlite connection
#' @param db_lighted_path string path to the lighted database
export_lighted_database <- function(db, db_lighted_path) {
	db2 <- RSQLite::dbConnect(RSQLite::SQLite(), db_lighted_path)
	tmp <- db_get_query(db, "select sample, raw_path, polarity, path, 
		instrument_model, instrument_manufacturer, software_name, software_version, 
		ion_source, analyzer, detector_type, resolution, agc_target, maximum_it, 
		number_of_scan_range, scan_range from sample;")
    RSQLite::dbWriteTable(db2, "sample", tmp, overwrite = TRUE)
	tmp <- RSQLite::dbReadTable(db, "project")
    RSQLite::dbWriteTable(db2, "project", tmp, overwrite = TRUE)
    tmp <- RSQLite::dbReadTable(db, "project_sample")
    RSQLite::dbWriteTable(db2, "project_sample", tmp, overwrite = TRUE)
    tmp <- RSQLite::dbReadTable(db, "user")
    RSQLite::dbWriteTable(db2, "user", tmp, overwrite = TRUE)
    tmp <- RSQLite::dbReadTable(db, "chemical")
    RSQLite::dbWriteTable(db2, "chemical", tmp, overwrite = TRUE)
    tmp <- RSQLite::dbReadTable(db, "chemical_ion")
    RSQLite::dbWriteTable(db2, "chemical_ion", tmp, overwrite = TRUE)
    tmp <- RSQLite::dbReadTable(db, "standard_ion")
    RSQLite::dbWriteTable(db2, "standard_ion", tmp, overwrite = TRUE)
    tmp <- RSQLite::dbReadTable(db, "feature")
    RSQLite::dbWriteTable(db2, "feature", tmp, overwrite = TRUE)
    RSQLite::dbDisconnect(db2)
}

#' @title Rename project
#'
#' @description
#' Rename project
#' 
#' @param db sqlite connection
#' @param project integer, project id to modify
#' @param name string, future project name
rename_project <- function(db, project, name) {
	query <- sprintf("update project set name = \"%s\", modified = Date('now') 
		where project == %s", name, project)
	db_execute(db, query)
	actualize$projects <<- runif(1)
}

#' @title Rename sample id
#'
#' @description
#' Rename sample id of a project_sample
#' 
#' @param db sqlite connection
#' @param project_sample integer, project_sample to modify
#' @param sample_id string, new sample_id
rename_project_sample <- function(db, project_sample, sample_id) {
	query <- sprintf("update project_sample set sample_id = \"%s\" 
		where project_sample == %s;", sample_id, project_sample)
	db_execute(db, query)
	update_project(db, project_sample = project_sample)
	actualize$project_samples <<- runif(1)
}

#' @title Update project
#' 
#' @description
#' Update field `modified` of a project in database
#'
#' @param db sqlite connection
#' @param project_sample integer, project_sample id to find related project
update_project <- function(db, project_sample) {
	query <- sprintf("update project set modified = Date('now') 
		where project == (select project from project_sample 
			where project_sample == %s);", project_sample)
	db_execute(db, query)
	actualize$projects <<- runif(1)
}

#' @title Delete unused samples in db
#'
#' @description
#' Delete unused samples in db
#'
#' @param db sqlite connection
clean_samples <- function(db) {
	samples <- db_get_query(db, 'select sample from sample 
		where sample not in (select distinct(sample) as sample 
			from project_sample);')$sample
	delete_samples(db, samples)
}

#' @title Delete unused process parameters in database
#' 
#' @description
#' Delete alone parameters in database (include params_xcms, param_camera & param_alignment)
#'
#' @param db sqlite connection
clean_params <- function(db) {
	db_execute(db, "delete from deconvolution_param where deconvolution_param not in (
		select deconvolution_param from project_sample);")
}