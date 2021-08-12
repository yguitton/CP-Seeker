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

#' @title Get m/z range in project
#' 
#' @description 
#' Get m/z range of files in project
#' 
#' @param db sqlite connection
#' @param project_sample vector, project_sample id
#' 
#' @return vector, m/z range
get_project_mz_range <- function(db, project_sample) {
  query <- sprintf("select mz_min, mz_max from sample where sample in (
    select sample from project_sample where project_sample in (%s))",
    paste(sprintf("\"%s\"", project_sample), collapse = ",")
    )
  db_get_query(db, query)
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

#' @title Get all chemical ions
#' 
#' @description
#' Get all chemical ions given an adduct name and a chemical type
#' 
#' @param db sqlite connection
#' @param adduct_name string adduct name
#' @param chemical_type string type of chemical studied
#'
#' @return dataframe with columns:
#' \itemize{
#' 		\item chemical_ion integer chemical_ion ID
#' 		\item ion_formula string ion formula
#' 		\item charge integer charge of ion
#' }
get_chemical_ions <- function(db, adduct_name = NULL, chemical_type = NULL, formula = NULL) {
	if (is.null(adduct_name)) return(data.frame())
  if(chemical_type == "standard") query <- sprintf(
    "select chemical_ion, ion_formula, charge 
		from chemical_ion where adduct in (%s)
		and chemical in (select chemical from chemical where formula in (%s));",
    paste(sprintf("\"%s\"", adduct_name), collapse = ","), 
    paste(sprintf("\"%s\"", formula), collapse = ","))
	else query <- sprintf("select chemical_ion, ion_formula, charge 
		from chemical_ion where adduct in (%s)
		and chemical_type in (%s);",
	  paste(sprintf("\"%s\"", adduct_name), collapse = ","), 
	  paste(sprintf("\"%s\"", chemical_type), collapse = ","))
	db_get_query(db, query)
}

#' @title Get chemical ion
#' 
#' @description
#' Get chemical ion given an adduct name, a number of C & Cl and the chemical type
#' 
#' @param db sqlite connection
#' @param adduct_name string adduct name
#' @param chemical_type string type of chemical studied
#' @param C integer number of carbon
#' @param Cl integer number of chlore
#'
#' @return dataframe with columns:
#' \itemize{
#' 		\item chemical_ion integer chemical_ion ID
#' 		\item ion_formula string ion formula
#' 		\item charge integer charge of ion
#' }
get_chemical_ion <- function(db, adduct_name = NULL, chemical_type = NULL, C = 0, Cl = 0, formula = NULL) {
	if (is.null(adduct_name)) return(data.frame())
  if (chemical_type == "standard"){
    query <- sprintf("select chemical_ion, ion_formula, charge 
		from chemical_ion where adduct == \"%s\" and 
		chemical == (select chemical from chemical
    where formula == \"%s\") ;", adduct_name, formula)
  }
  else{
   query <- sprintf("select chemical_ion, ion_formula, charge 
		from chemical_ion where adduct == \"%s\" and 
		chemical == (select chemical from chemical 
		where C == %s and Cl == %s and chemical_type == \"%s\");", adduct_name, C, Cl, chemical_type) 
  }
	db_get_query(db, query)
}

#' @title Get all features
#'
#' @descriptition
#' Get all features given a project_sample, an adduct and a chemical type
#'
#' @param db sqlite connection
#' @param project_samples vector(integer) project_sample ID
#' @param adducts vector(string) adduct names
#' @param chemical_type string type of chemical studied
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
#' 		\item chemical_ion integer id of the chemical ion
#' 		\item project_sample id integer project_sample ID
#' }
get_features <- function(db, project_samples = NULL, adducts = NULL, chemical_type = NULL) {
	if (is.null(project_samples) | is.null(adducts)) return(data.frame())
	query <- sprintf("select * from feature where project_sample in (%s) 
		and chemical_ion in (select chemical_ion from 
			chemical_ion where adduct in (%s) and chemical_type == \"%s\");", 
		paste(project_samples, collapse = ", "), 
		paste('"', adducts, '"', sep = "", collapse = ", "),
		chemical_type)
	db_get_query(db, query)
}

#' @title Get all chemical features
#'
#' @description
#' Get all features for a chemical integrated in a project_sample
#'
#' @param db sqlite connection
#' @param project_sample integer project_sample ID
#' @param chemical_ion integer chemical ion ID
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
#' 		\item chemical_ion integer id of the chemical ion
#' 		\item project_sample id integer project_sample ID
#' }
get_chemical_features <- function(db, project_sample = NULL, 
		chemical_ion = NULL) {
	if (is.null(project_sample) | is.null(chemical_ion)) return(data.frame())
	query <- sprintf("select * from feature where project_sample == %s 
		and chemical_ion == %s;", project_sample, 
		chemical_ion)
	db_get_query(db, query)
}

#' @title Get deconvolution parameters
#' 
#' @description 
#' Get deconvolution parameters whith a given project, chemical end adduct
#' 
#' @param db sqlite connection
#' @param project integrer project id
#' @param chemical_type string type of chemical
#' @param adduct string adduct name
get_deconvolution_params <- function(db, project, chemical_type, adduct){
  query <- sprintf("select * from deconvolution_param 
    where project == %s and chemical_type == \"%s\" and adduct == \"%s\"", 
    project, chemical_type, adduct)
  db_get_query(db, query)
}

#' @title Get profile matrix
#' 
#' @description
#' Construct profile matrix consisting of rows representing carbons & cols chlore
#' It cells will contain the isotopic score, the intensity and the deviation of the corresponding ion integrated
#'
#' @param db sqlite connection
#' @param project_sample integer project_sample ID
#' @param adduct string adduct name use for deconvolution
#' @param chemical_type string type of chemical studied
#' 
#' @return matrix with isotopic scores of chemical ions integrated
#' 		each column represent a level of chlore & 
#'		each row represent a level of carbon
get_profile_matrix <- function(db, project_sample = NULL, adduct = NULL,
  chemical_type = NULL, simplify = TRUE) {
  query <- if (is.null(adduct)) "select C, Cl from chemical;" 
  else sprintf("select chemical_ion, C, Cl from chemical 
		left join chemical_ion on 
		chemical.chemical = chemical_ion.chemical 
		where adduct == \"%s\" and chemical.chemical_type == \"%s\";", adduct, chemical_type)
  chemicals <- db_get_query(db, query)
  C <- range(chemicals$C)
  Cl <- range(chemicals$Cl)
  profile_mat <- matrix(NA, nrow = C[2] - C[1] + 1, ncol = Cl[2] - Cl[1] + 1, 
    dimnames = list(paste0("C", C[1]:C[2]), paste0("Cl", Cl[1]:Cl[2])))
  
  if (is.null(project_sample) | is.null(adduct)) return(profile_mat)
  query <- sprintf("select chemical_ion, 
    round(score,0) as score, intensities, weighted_deviation from feature where 
		iso == \"A\" and project_sample == %s and chemical_ion in (
			select chemical_ion from chemical_ion
			where adduct == \"%s\" and chemical_type == \"%s\");", 
    project_sample, adduct, chemical_type)
  data <- db_get_query(db, query)
  if (nrow(data) == 0) return(profile_mat)
  data <- merge(chemicals, data, 
    by = "chemical_ion", all.x = TRUE)
  ion_forms <- get_chemical_ions(db, adduct, chemical_type)
  theoric_patterns <- get_theoric(ion_forms$ion_formula, 
    ion_forms$charge[1])
  mz_range <- get_project_mz_range(db, project_sample)
  status <- get_patterns_status(theoric_patterns, mz_range)
  
  if(simplify){
    for (row in seq(nrow(data))) profile_mat[
      data[row, "C"] - C[1] + 1, 
      data[row, "Cl"] - Cl[1] + 1] <- paste(data[row, "score"], 
        round(data[row, "intensities"]/10**6, digits = 0),
        round(data[row, "weighted_deviation"]*10**3, digits = 1),
        status[row], sep = "/")
  }
  else {
    for (row in seq(nrow(data))) profile_mat[
      data[row, "C"] - C[1] + 1, 
      data[row, "Cl"] - Cl[1] + 1] <- paste(data[row, "score"], 
        data[row, "intensities"],
        data[row, "weighted_deviation"],
        status[row], sep = "/")
  }
  profile_mat
}

#' @title Get standard table
#' 
#' @description
#' Construct standard table with standard formula, adduct, area, score and deviation
#' 
#' @param db sqlite connection
#' @param project_sample
#' @param adduct
#' @param standard_formula
#' 
#' @return table with standard formula, adduct, area, score and deviation of the standard studied
get_standard_table <- function(db, project = NULL, adduct = NULL, standard_formula = NULL){
  sample <- get_samples(db, project)
  table <- NULL
  for(i in 1:length(sample$project_sample)){
    data <- do.call(rbind,
      lapply(standard_formula, function(y){
        do.call(rbind, 
          lapply(adduct, function(x){
            query <- sprintf('select `into`, intb, score, weighted_deviation from feature where
              iso == \"A\" and project_sample in (select project_sample from project_sample where 
              project == %s and sample_id == \"%s\") and chemical_ion in (
                select chemical_ion from chemical_ion where adduct == \"%s\"
                and chemical == (select chemical from chemical where formula == \"%s\"));', 
              project, sample$sample_id[i], x, y)
            data2 <- db_get_query(db, query)
            if(nrow(data2) == 0){
              data2 <- data.frame(into = NA, intb = NA, score = NA, weighted_deviation = NA)
            }
            data2 <- cbind(sample_id = sample$sample_id[i], formula = y, adduct = x, data2)
          })
        )
      })
    )
    table <- rbind(table, data)
  }
  table$into[which(!is.na(table$into))] <- formatC(
    as.numeric(table$into[which(!is.na(table$into))]), format = 'f', big.mark = " ", digits = 0)
  table$intb[which(!is.na(table$intb))] <- formatC(
    as.numeric(table$intb[which(!is.na(table$intb))]), format = 'f', big.mark = " ", digits = 0)
  table$score[which(!is.na(table$score))] <- round(
    table$score[which(!is.na(table$score))], digits = 0)
  table$weighted_deviation[which(!is.na(table$weighted_deviation))] <- round(
    table$weighted_deviation[which(!is.na(table$weighted_deviation))]*10**3, digits = 2)
  data.table::setnames(table, c("into", "intb", "weighted_deviation"), 
    c("total area", "area above baseline", "deviation (mDa, xE-3)"))
  table
}