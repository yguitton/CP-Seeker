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
	ms_file <- tryCatch(
		unserialize(
			fst::decompress_fst(
				unlist(
					db_get_query(db, query)$raw_data
				)
			)
		), error = function(e) {
		print(e)
		NULL
	})
	if (is.null(ms_file)) {
		# try to found it in the mzXML directory
		query <- sprintf("select path, raw_path from sample 
			where sample == \"%s\";", sampleID)
		paths <- unlist(db_get_query(db, query))
		paths <- paths[which(file.exists(paths))]
		if (length(paths) == 0) stop(sprintf("\"%s\" is not found in database", sampleID))
		else {
			ms_file <- tryCatch(xcms::xcmsRaw(paths[1], mslevel = 1, profstep = 0), 
				error = function(e) NULL)
			if (is.null(ms_file) & length(paths) > 1) {
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
	db_get_query(db, query)
}

#' @title Get chloroparaffin ion
#' 
#' @description
#' Get chloroparaffin ion given an adduct name, a number of C & Cl
#' 
#' @param db sqlite connection
#' @param adduct_name string adduct name
#' @param C integer number of carbon
#' @param Cl integer number of chlore
#'
#' @return dataframe with columns:
#' \itemize{
#' 		\item chloroparaffin_ion integer chloroparaffin_ion ID
#' 		\item ion_formula string ion formula
#' 		\item charge integer charge of ion
#' }
get_chloroparaffin_ion <- function(db, adduct_name = NULL, C = 0, Cl = 0) {
	if (is.null(adduct_name)) return(data.frame())
	query <- sprintf("select chloroparaffin_ion, ion_formula, charge 
		from chloroparaffin_ion where adduct == \"%s\" and 
		chloroparaffin == (select chloroparaffin from chloroparaffin 
		where C == %s and Cl == %s);", adduct_name, C, Cl)
	db_get_query(db, query)
}

#' @title Get all features
#'
#' @descriptition
#' Get all features given a project_sample and an adduct
#'
#' @param db sqlite connection
#' @param project_samples vector(integer) project_sample ID
#' @param adducts vector(string) adduct names
#'
#' @return dataframe with columns:
#' \itemize{
#' 		\item feature integer feature ID
#' 		\item mz float m/z
#' 		\item mzmin float born m/z min
#' 		\item mzmax float born m/z max
#' 		\item rt float rT
#' 		\item rtmin float born rT min
#' 		\item rtmax float born rT max
#' 		\item into float area integrated
#' 		\item intb float area above baseline integrated
#' 		\item maxo float max intensity
#' 		\item sn float signal/noise
#' 		\item scale integer wave used for integration
#' 		\item scpos integer center scan position
#' 		\item scmin integer born scan min
#' 		\item scmax integer born scan max
#' 		\item lmin integer to ignore
#' 		\item lmax integer to ignore
#' 		\item iso string isotopologue annotation
#' 		\item abundance float abundance
#' 		\item score float isotopic pattern score
#' 		\item deviation float m/z deviation
#' 		\item chloroparaffin_ion integer id of the chloroparaffin ion
#' 		\item project_sample id integer project_sample ID
#' }
get_features <- function(db, project_samples = NULL, adducts = NULL) {
	if (is.null(project_samples) | is.null(adducts)) return(data.frame())
	query <- sprintf("select * from feature where project_sample in (%s) 
		and chloroparaffin_ion in (select chloroparaffin_ion from 
			chloroparaffin_ion where adduct in (%s));", 
		paste(project_samples, collapse = ", "), 
		paste('"', adducts, '"', sep = "", collapse = ", "))
	db_get_query(db, query)
}

#' @title Get all chloroparaffin features
#'
#' @description
#' Get all features for a chloroparaffin integrated in a project_sample
#'
#' @param db sqlite connection
#' @param project_sample integer project_sample ID
#' @param chloroparaffin_ion integer chloroparaffin ion ID
#'
#' @return dataframe with columns:
#' \itemize{
#' 		\item feature integer feature ID
#' 		\item mz float m/z
#' 		\item mzmin float born m/z min
#' 		\item mzmax float born m/z max
#' 		\item rt float rT
#' 		\item rtmin float born rT min
#' 		\item rtmax float born rT max
#' 		\item into float area integrated
#' 		\item intb float area above baseline integrated
#' 		\item maxo float max intensity
#' 		\item sn float signal/noise
#' 		\item scale integer wave used for integration
#' 		\item scpos integer center scan position
#' 		\item scmin integer born scan min
#' 		\item scmax integer born scan max
#' 		\item lmin integer to ignore
#' 		\item lmax integer to ignore
#' 		\item iso string isotopologue annotation
#' 		\item abundance float abundance
#' 		\item score float isotopic pattern score
#' 		\item deviation float m/z deviation
#' 		\item chloroparaffin_ion integer id of the chloroparaffin ion
#' 		\item project_sample id integer project_sample ID
#' }
get_chloroparaffin_features <- function(db, project_sample = NULL, 
		chloroparaffin_ion = NULL) {
	if (is.null(project_sample) | is.null(chloroparaffin_ion)) return(data.frame())
	query <- sprintf("select * from feature where project_sample == %s 
		and chloroparaffin_ion == %s;", project_sample, 
		chloroparaffin_ion)
	db_get_query(db, query)
}

#' @title Get profile matrix
#' 
#' @description
#' Construct profile matrix consisting of rows representing carbons & cols chlore
#' It cells will contain the isotopic score of the corresponding ion integrated
#'
#' @param db sqlite connection
#' @param project_sample integer project_sample ID
#' @param adduct string adduct name use for deconvolution
#' 
#' @return matrix with isotopic scores of chloroparaffin ions integrated
#' 		each column represent a level of chlore & 
#'		each row represent a level of carbon
get_profile_matrix <- function(db, project_sample = NULL, adduct = NULL) {
	query <- if (is.null(adduct)) "select C, Cl from chloroparaffin;" 
		else sprintf("select chloroparaffin_ion, C, Cl from chloroparaffin 
			left join chloroparaffin_ion on 
			chloroparaffin.chloroparaffin = chloroparaffin_ion.chloroparaffin 
			where adduct == \"%s\";", adduct)
	chloroparaffins <- db_get_query(db, query)
	C <- range(chloroparaffins$C)
	Cl <- range(chloroparaffins$Cl)
	profile_mat <- matrix(NA, nrow = C[2] - C[1] + 1, ncol = Cl[2] - Cl[1] + 1, 
		dimnames = list(paste0("C", C[1]:C[2]), paste0("Cl", Cl[1]:Cl[2])))
	
	if (is.null(project_sample) | is.null(adduct)) return(profile_mat)
	query <- sprintf("select chloroparaffin_ion, round(score,0) as score 
		from feature where 
		iso == \"A\" and project_sample == %s and chloroparaffin_ion in (
			select chloroparaffin_ion from chloroparaffin_ion
			where adduct == \"%s\");", project_sample, adduct)
	data <- db_get_query(db, query)
	if (nrow(data) == 0) return(profile_mat)
	data <- merge(chloroparaffins, data, 
		by = "chloroparaffin_ion", all.x = TRUE)
	for (row in seq(nrow(data))) profile_mat[
		data[row, "C"] - C[1] + 1, 
		data[row, "Cl"] - Cl[1] + 1] <- 
		data[row, "score"]
	profile_mat
}