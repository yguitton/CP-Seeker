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
#' @param formula string standard formula
#'
#' @return dataframe with columns:
#' \itemize{
#' 		\item chemical_ion integer chemical_ion ID
#' 		\item ion_formula string ion formula
#' 		\item charge integer charge of ion
#' }
get_chemical_ions <- function(db, adduct_name = NULL, chemical_type = NULL, formula = NULL) {
	if (is.null(adduct_name)) return(data.frame())
  if(chemical_type == "Standard") query <- sprintf(
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
#' @param formula string standard formula
#'
#' @return dataframe with columns:
#' \itemize{
#' 		\item chemical_ion integer chemical_ion ID
#' 		\item ion_formula string ion formula
#' 		\item charge integer charge of ion
#' }
get_chemical_ion <- function(db, adduct_name = NULL, chemical_type = NULL, C = 0, Cl = 0, formula = NULL) {
	if (is.null(adduct_name)) return(data.frame())
  if (chemical_type == "Standard"){
    query <- sprintf("select adduct, chemical_ion, ion_formula, charge
		from chemical_ion where adduct == \"%s\" and
		chemical == (select chemical from chemical
    where chemical_type == \"%s\");", adduct_name, formula)
  }else if(length(grep("PXA",chemical_type)) > 0){
  	query <- sprintf("select adduct, chemical_ion, ion_formula, charge
		from chemical_ion where adduct == \"%s\" and
		chemical == (select chemical from chemical
		where Br == %s and Cl == %s and chemical_type == \"%s\");", adduct_name, C, Cl, chemical_type)
  }else if(length(grep("PBA",chemical_type)) > 0){
  	query <- sprintf("select adduct, chemical_ion, ion_formula, charge
		from chemical_ion where adduct == \"%s\" and
		chemical == (select chemical from chemical
		where C == %s and Br == %s and chemical_type == \"%s\");", adduct_name, C, Cl, chemical_type)
  }else{
   query <- sprintf("select adduct, chemical_ion, ion_formula, charge
		from chemical_ion where adduct == \"%s\" and
		chemical == (select chemical from chemical
		where C == %s and Cl == %s and chemical_type == \"%s\");", adduct_name, C, Cl, chemical_type)
  }
	db_get_query(db, query)
}

get_ion_without_adduct <- function(db, adduct_name = NULL, ion_formula = NULL){
	if(is.null(adduct_name)) return(ion_formula)
	if(is.null(ion_formula)) return("")
	adduct <- strsplit(adduct_name, "")[[1]]
	sign <- adduct[2]
	compound <- paste(adduct[3:length(adduct)], collapse = "")
	data(isotopes)
	compound_checked <- check_chemform(isotopes, compound)
	if(compound_checked$warning == FALSE) compound <- compound_checked$new_formula
	if(check_ded(compound, ion_formula)){ # check if compound in ion formula
		if(sign == "+"){
			print("Retirer le compound")
			result_compound <- subform(ion_formula, compound)
		}else if(sign == "-"){
			print("Ajouter le compound")
			result_compound <- mergeform(ion_formula, compound)
		}else{
			print("Error compound")
		}
	}else{
		print("Compound not in ion_formula")
	}
	return(result_compound)
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
#' Its cells will contain the isotopic score, the intensity and the deviation of the corresponding ion integrated
#'
#' @param db sqlite connection
#' @param project_sample integer project_sample ID
#' @param adduct string adduct name use for deconvolution
#' @param chemical_type string type of chemical studied
#' @param simplify boolean if TRUE will round deviation and intensities
#' @param table boolean if TRUE will return the data table, else the matrix
#' @param export boolean to know if the function is in export and needs different digits
#'
#' @return matrix with isotopic scores of chemical ions integrated
#' 		each column represent a level of chlore &
#'		each row represent a level of carbon
get_profile_matrix <- function(db, project_sample = NULL, adduct = NULL,
  chemical_type = NULL, simplify = TRUE, table = FALSE, export = FALSE) {

  noChem <- FALSE

  digits_int <- if (export) 6 else 0
  digits_score <- 0
  digits_dev <- if (export) 2 else 1

  if (is.null(adduct)) {
    print("adduct null")
    query <- "SELECT C, Cl FROM chemical;"
    chemicals <- db_get_query(db, query)
    colY <- range(chemicals[,1])
    colX <- range(chemicals[,2])
    profile_mat <- matrix(NA, nrow = colY[2] - colY[1] + 1, ncol = colX[2] - colX[1] + 1,
      dimnames = list(paste0(colnames(chemicals)[1], colY[1]:colY[2]), paste0(colnames(chemicals)[2], colX[1]:colX[2])))
    if (is.null(project_sample) | is.null(adduct)) return(profile_mat)
    query <- sprintf("SELECT chemical_ion,
      ROUND(score, %s) AS score, intensities, weighted_deviation FROM feature WHERE
      abundance = 100 AND project_sample = %s AND chemical_ion IN (
        SELECT chemical_ion FROM chemical_ion
        WHERE adduct = \"%s\" AND chemical_type = \"%s\");",
      digits_score, project_sample, adduct, chemical_type)
    data <- db_get_query(db, query)
    if (nrow(data) == 0) return(profile_mat)
    print("passe")
    data <- merge(chemicals, data, by = "chemical_ion", all.x = TRUE)
    if (table) return(data)
    ion_forms <- get_chemical_ions(db, adduct, chemical_type)
    theoric_patterns <- get_theoric(ion_forms$ion_formula, ion_forms$charge[1])
    mz_range <- get_project_mz_range(db, project_sample)
    status <- get_patterns_status(theoric_patterns, mz_range)
    # Calculate row and column indices
    row_indices <- data[, colnames(chemicals)[1]] - colY[1] + 1
    col_indices <- data[, colnames(chemicals)[2]] - colX[1] + 1
    if (simplify) {
      # Create values to insert
      values <- paste(
        data[,"score"],
        round(data[,"intensities"] / 10**6, digits = digits_int),
        round(data[,"weighted_deviation"] * 10**3, digits = digits_dev),
        status,
        sep = "/"
      )
    } else {
      # Create values to insert
      values <- paste(
        data[,"score"],
        data[,"intensities"],
        data[,"weighted_deviation"],
        status,
        sep = "/"
      )
    }
    # Fill the matrix
    profile_mat[cbind(row_indices, col_indices)] <- values
    return(profile_mat)
  } else {
    # Handle different chemical types with progress
    withProgress(message = 'get_profile_matrix() - Processing profile matrix...', value = 0, {
      # Set up progress bar
      query <- switch(chemical_type,
        "PXAs" = sprintf("SELECT chemical_ion, Br, Cl FROM chemical
                          LEFT JOIN chemical_ion ON
                          chemical.chemical = chemical_ion.chemical
                          WHERE adduct = \"%s\" AND chemical.chemical_type = \"%s\";", adduct, chemical_type),
        "PBAs" = sprintf("SELECT chemical_ion, C, Br FROM chemical
                          LEFT JOIN chemical_ion ON
                          chemical.chemical = chemical_ion.chemical
                          WHERE adduct = \"%s\" AND chemical.chemical_type = \"%s\";", adduct, chemical_type),
        {
          sprintf("SELECT chemical_ion, C, Cl FROM chemical
                   LEFT JOIN chemical_ion ON
                   chemical.chemical = chemical_ion.chemical
                   WHERE adduct = \"%s\" AND chemical.chemical_type = \"%s\";", adduct, chemical_type)
        }
      )
      chemicals <- db_get_query(db, query)
      if (nrow(chemicals) == 0) {
        query <- switch(chemical_type,
          "PXAs" = sprintf("SELECT chemical_ion, Br, Cl FROM chemical
                            LEFT JOIN chemical_ion ON
                            chemical.chemical = chemical_ion.chemical
                            WHERE chemical.chemical_type = \"%s\";", chemical_type),
          "PBAs" = sprintf("SELECT chemical_ion, C, Br FROM chemical
                            LEFT JOIN chemical_ion ON
                            chemical.chemical = chemical_ion.chemical
                            WHERE chemical.chemical_type = \"%s\";", chemical_type),
          {
            sprintf("SELECT chemical_ion, C, Cl FROM chemical
                     LEFT JOIN chemical_ion ON
                     chemical.chemical = chemical_ion.chemical
                     WHERE chemical.chemical_type = \"%s\";", chemical_type)
          }
        )
        chemicals <- db_get_query(db, query)
        noChem <- TRUE
      }
      colY <- range(chemicals[,2])
      colX <- range(chemicals[,3])
      
      # Setup the profile matrix
      profile_mat <- matrix(NA, nrow = colY[2] - colY[1] + 1, ncol = colX[2] - colX[1] + 1,
        dimnames = list(paste0(colnames(chemicals)[2], colY[1]:colY[2]), paste0(colnames(chemicals)[3], colX[1]:colX[2])))
      
      if (is.null(project_sample) | is.null(adduct)) return(profile_mat)
      
      query <- sprintf("SELECT chemical_ion,
        ROUND(score, %s) AS score, intensities, weighted_deviation FROM feature WHERE
        abundance = 100 AND project_sample = %s AND chemical_ion IN (
          SELECT chemical_ion FROM chemical_ion
          WHERE adduct = \"%s\" AND chemical_type = \"%s\");",
        digits_score, project_sample, adduct, chemical_type)
      data <- db_get_query(db, query)
      
      if (nrow(data) > 0) {
        if (table) return(merge(chemicals, data, by = "chemical_ion", all.x = TRUE))
        ion_forms <- get_chemical_ions(db, adduct, chemical_type)
        theoric_patterns <- get_theoric(ion_forms$ion_formula, ion_forms$charge[1])
        mz_range <- get_project_mz_range(db, project_sample)
        status <- get_patterns_status(theoric_patterns, mz_range)
        status <- merge(ion_forms, status, by = "ion_formula", all.x = TRUE)
        data <- merge(chemicals, data, by = "chemical_ion", all.x = TRUE)
        data <- merge(status[,c("chemical_ion","status")], data, by = "chemical_ion", all.x = TRUE)
        row_indices <- data[, colnames(chemicals)[2]] - colY[1] + 1
        col_indices <- data[, colnames(chemicals)[3]] - colX[1] + 1
        
        if (simplify) {
          values <- paste(
            data[,"score"],
            round(data[,"intensities"] / 10**6, digits = digits_int),
            round(data[,"weighted_deviation"] * 10**3, digits = digits_dev),
            data[,"status"],
            sep = "/"
          )
        } else {
          values <- paste(
            data[,"score"],
            data[,"intensities"],
            data[,"weighted_deviation"],
            data[,"status"],
            sep = "/"
          )
        }
        profile_mat[cbind(row_indices, col_indices)] <- values
      } 
      # Fill NA cells
      profile_mat[which(is.na(profile_mat))] <- "NA/NA/NA/inside"
      
      # Adjust for adduct-specific conditions
      frag <- strsplit(adduct, "")[[1]]
      sign <- frag[2]
      compound <- paste(frag[3:length(frag)], collapse = "")
      if (sign == "-") {
        print("Chemical needs this compound in formula")
        if (length(grep(paste(compound,"0",sep=""), colnames(profile_mat))) > 0) {
          profile_mat[,1] <- "NA/NA/NA/outside"
        } else if (length(grep(paste(compound,"0",sep=""), rownames(profile_mat))) > 0) {
          profile_mat[1,] <- "NA/NA/NA/outside"
        }
      }
      profile_mat <- get_type_pattern(colX, colY, chemical_type, chemicals, profile_mat)
      
      # Update progress bar
      incProgress(1, detail = "Processing complete")
    })
  }
  return(profile_mat)
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
    # query of normal standard with iso = A
    query <- sprintf('select intensities, intensities_b, score, weighted_deviation from feature where
         			iso == \"A\" and project_sample in (select project_sample from project_sample where
              project == %s and sample_id == \"%s\") and chemical_ion in (
                select chemical_ion from chemical_ion where adduct == \"%s\" and chemical == (
                	select chemical from chemical where chemical_type == \"%s\"));',
              project, sample$sample_id[i], adduct, standard_formula)
    data <- db_get_query(db, query)
    if(nrow(data) > 0) data <- cbind(sample_id = sample$sample_id[i], formula = standard_formula, adduct = adduct, data) 
    # Second try the standard had no results but has a theoric pattern (iso = "no ROIs" or something like that)
    if(nrow(data) == 0){
      # query of standard where nothing found and nexted (iso = "no ROIs" or something)
    	query <- sprintf('select intensities, intensities_b, score, weighted_deviation from feature where
         			iso == \"no ROIs\" and project_sample in (select project_sample from project_sample where
              project == %s and sample_id == \"%s\") and chemical_ion in (
                select chemical_ion from chemical_ion where adduct == \"%s\" and chemical == (
                	select chemical from chemical where chemical_type == \"%s\"));',
              project, sample$sample_id[i], adduct, standard_formula)
    	dataOut <- db_get_query(db, query)
    	if(nrow(dataOut) > 0) data <- cbind(sample_id = sample$sample_id[i], formula = standard_formula, adduct = adduct, dataOut) 
    }
    # Last try, the standard has been asked but the adduct is not possible with it
    if(nrow(data) == 0){
    	data <- cbind(sample_id = sample$sample_id[i], formula = standard_formula, adduct = adduct, intensities = "not possible", 
    		intensities_b = "not possible", score = "not possible", weighted_deviation = "not possible")
    }
    table <- as.data.frame(rbind(table, data))
  }
  if(class(table$intensities) != "character") table$intensities[which(!is.na(table$intensities))] <- formatC(
    as.numeric(table$intensities[which(!is.na(table$intensities))]), format = 'f', big.mark = " ", digits = 0)
  if(class(table$intensities_b) != "character") table$intensities_b[which(!is.na(table$intensities_b))] <- formatC(
    as.numeric(table$intensities_b[which(!is.na(table$intensities_b))]), format = 'f', big.mark = " ", digits = 0)
  if(class(table$score) != "character") table$score[which(!is.na(table$score))] <- round(
    table$score[which(!is.na(table$score))], digits = 0)
  if(class(table$weighted_deviation) != "character") table$weighted_deviation[which(!is.na(table$weighted_deviation))] <- round(
    table$weighted_deviation[which(!is.na(table$weighted_deviation))]*10**3, digits = 2)
  data.table::setnames(table, c("intensities", "intensities_b", "weighted_deviation"),
    c("total area", "area above baseline", "deviation(mDa)"))
  table
}

get_chemical_families <- function(db) {
    db_get_query(db, "SELECT DISTINCT(chemical_type) FROM chemical;")[, 1]
}
get_ecni_adduct <- function(db) {
    db_get_query(db, 'SELECT DISTINCT adduct FROM chemical_ion where chemical_ion_family = "ECNI";')
}
get_esi_adduct <- function(db) {
   db_get_query(db, 'SELECT DISTINCT adduct FROM chemical_ion where chemical_ion_family = "ESI/APCI";')
}

get_formula <- function(db, chemical_type, C = 0, Cl = 0, Br = 0){
	if(is.null(chemical_type)) return("No chemical_type")
	query <- sprintf("select * from chemical where 
		chemical_type == \"%s\" and C == %s and Cl == %s and Br == %s", 
		chemical_type, C, Cl, Br)
	db_get_query(db, query)
}

#' @title Get deconvolution_infos table
#'
#' @description
#' Recover information corresponding to execution times during deconvolution and information 
#' on the specifications of the device that performed the calculation. 
#'
#' @param db sqlite connection
#' @param project integrer project id
#'
#' @return complete info table
get_infos <- function(db, project = NULL){
  query <- "SELECT * FROM deconvolution_infos"
  if (!is.null(project)) {
    query <- paste(query, sprintf("WHERE project = '%s'", project), sep = " ")
  }
  db_get_query(db, query)
}