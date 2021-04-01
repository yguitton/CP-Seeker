#' @title Delete samples in db
#'
#' @description
#' Delete samples in db
#' delete also all project_samples associated
#'
#' @param db sqlite connection
#' @param samples vector of strings, sample to delete
delete_samples <- function(db, samples = NULL) {
	if (length(samples) == 0) return()
	query <- sprintf('select project_sample from project_sample 
		where sample in (%s);', paste("\"", samples, "\"", sep="", collapse=', '))
	print(query)
	project_samples <- db_get_query(db, query)$project_sample
	delete_project_samples(db, project_samples)
	query <- sprintf('select path from sample where sample in (%s);', 
		paste("\"", samples, "\"", sep="", collapse=', '))
	print(query)
	paths <- db_get_query(db, query)$path 
	paths <- paths[which(grepl("^mzXMLFiles", paths) & 
		file.exists(paths))]
	file.remove(paths)
	query <- sprintf('delete from sample where sample in (%s);', 
		paste("\"", samples, "\"", sep="", collapse=', '))
	print(query)
	db_execute(db, query)
	actualize$samples <<- runif(1)
	actualize$project_samples <<- runif(1)
}

#' @title Delete projects in db
#'
#' @description
#' Delete projects in db
#' delete also all project_samples associated
#'
#' @param db sqlite connection
#' @param projects vector of integers, project ids to delete
delete_projects <- function(db, projects = NULL){
	if (length(projects) == 0) return()
	query <- sprintf('select project_sample from project_sample where 
		project in (%s);', paste(projects, collapse=', '))
	print(query)
	project_samples <- db_get_query(db, query)$project_sample
	delete_project_samples(db, project_samples)
	query <- sprintf('delete from project where project in (%s);', 
		paste(projects, collapse=', '))
	print(query)
	db_execute(db, query)
	actualize$projects <<- runif(1)
	actualize$project_samples <<- runif(1)
}

#' @title Delete project_samples in db
#'
#' @description
#' Delete project_samples in db
#' will also delete all features associated
#'
#' @param db sqlite connection
#' @param project_samples vector of integers, project_sample ids to delete
delete_project_samples <- function(db, project_samples = NULL){
	if (length(project_samples) == 0) return()
	delete_features(db, project_samples)
	query <- sprintf('delete from project_sample where project_sample in (%s);', 
		paste(project_samples, collapse=', '))
	print(query)
	db_execute(db, query)
	actualize$project_samples <<- runif(1)
}