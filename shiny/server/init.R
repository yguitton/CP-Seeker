# read in sql-statements and preformat them                                        
sqlFromFile <- function(file){
    require(stringr)
    sql <- readLines(file)
    sql <- unlist(str_split(paste(sql,collapse=" "),";"))
    sql <- sql[grep("^ *$", sql, invert=T)]
}

# apply query function to each element
dbSendQueries <- function(con,sql){
    dummyfunction <- function(sql,con){
        dbSendQuery(con,sql)
    }
    lapply(sql, dummyfunction, con)
}

if(!file.exists(sqlitePath)){
	print('create database')
	db <- dbConnect(SQLite(), sqlitePath)
	dbSendQueries(db, sqlFromFile('../createDatabase.sql'))
	dbDisconnect(db)
} else{
	hide('loader')
	show('app-content')
}

#----------------------------------------------------------------------------------------------
# shinyFiles is incompatible with R v3.4
# Two functions are overridden file.info() (base package) and fileGetter() (shinyFiles package) 
#-----------------------------------------------------------------------------------------------
fileGetter <- function (roots, restrictions, filetypes, hidden = FALSE){
	if (missing(filetypes)) filetypes <- NULL
	if (missing(restrictions)) restrictions <- NULL
	function(dir, root){
		currentRoots <- if (class(roots) == "function") roots()
						else roots
		if (is.null(names(currentRoots))) stop("Roots must be a named vector or a function returning one")
		if (missing(root)) root <- names(currentRoots)[1]
		fulldir <- file.path(currentRoots[root], dir)
		writable <- as.logical(file.access(fulldir, 2) == 0)
		files <- list.files(fulldir, all.files = hidden, full.names = TRUE, no.. = TRUE)
		files <- gsub(pattern = "//*", "/", files, perl = TRUE)
		if(!is.null(restrictions) && length(files) != 0){
			if(length(files) == 1) keep <- !any(sapply(restrictions, function(x) { grepl(x, files, fixed = T) }))
			else keep <- !apply(sapply(restrictions, function(x) { grepl(x, files, fixed = T) }), 1, any)
			files <- files[keep]
		}
		fileInfo <- (file.info(files))
		fileInfo$filename <- basename(files)
		fileInfo$extension <- tolower(tools::file_ext(files))
		validIndex <- which(!is.na(fileInfo$mtime))
		fileInfo <- fileInfo[validIndex,]
		fileInfo$mtime <- format(fileInfo$mtime, format = "%Y-%m-%d-%H-%M")
		fileInfo$ctime <- format(fileInfo$ctime, format = "%Y-%m-%d-%H-%M")
		fileInfo$atime <- format(fileInfo$atime, format = "%Y-%m-%d-%H-%M")
		if(!is.null(filetypes)){
			matchedFiles <- tolower(fileInfo$extension) %in%
				tolower(filetypes) & fileInfo$extension != ""
			fileInfo$isdir[matchedFiles] <- FALSE
			fileInfo <- fileInfo[matchedFiles | fileInfo$isdir, ]
		}
		rownames(fileInfo) <- NULL
		breadcrumps <- strsplit(dir, .Platform$file.sep)[[1]]
		list(files = fileInfo[, c("filename", "extension", "isdir", "size", "mtime", "ctime", "atime")], writable = writable,
					exist = file.exists(fulldir), breadcrumps = I(c("", breadcrumps[breadcrumps != ""])), roots = I(names(currentRoots)), root = root)
	}
}
unlockBinding("fileGetter", getNamespace("shinyFiles"))
assign("fileGetter",fileGetter,getNamespace("shinyFiles"))

file.info <- function (..., extra_cols = TRUE){
	suppressWarnings(res <- .Internal(file.info(fn <- c(...), extra_cols)))
	res$mtime <- .POSIXct(res$mtime)
	res$ctime <- .POSIXct(res$ctime)
	res$atime <- .POSIXct(res$atime)
	class(res) <- "data.frame"
	attr(res, "row.names") <- fn
	res
}
unlockBinding("file.info", getNamespace("base"))
assign("file.info",file.info,getNamespace("base"))	


output$uiSelectProject <- renderUI({ selectInput('selectProject', 'Select a project', choices=unique(samples()$project), multiple=FALSE) })
