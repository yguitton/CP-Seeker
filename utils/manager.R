Sys.setenv(LANG = "en")

appwd <- getwd()

# Read the application config
library("jsonlite", character.only = TRUE)
config <- jsonlite::fromJSON("utils/config.cfg")
reg_paths <- jsonlite::fromJSON("utils/regpaths.json")

pb <- winProgressBar(
  title = sprintf("Starting %s ...", config$appname),
  label = "Initializing ...")
  
  
tryCatch({

message('check dir & files...')
  
if(!file.exists("utils/checks.R")) stop('Missing ckecks.R file')

library(tools)

source("utils/checks.R")

# ensure all package dependencies are installed
applibpath <- file.path(appwd, file.path(reg_paths$r, "library", sep='\\'))

# Load functions to ensure software dependencies
source("utils/ensure.R")

.libPaths(applibpath)

message("library paths:\n", paste0("... ", .libPaths(), collapse = "\n"))
message("working path:\n", paste("...", appwd))

message("ensuring packages dependencies ...")
setWinProgressBar(pb, 0, label = "Ensuring package dependencies ...")
  
for (i in seq_along(pkgs)) {
	setWinProgressBar(pb,
		value = i / (length(pkgs) + 1),
		label = sprintf("Loading package - %s", pkgs[i]))
	if(ensure(pkgs[i], names(pkgs)[i], pb) == FALSE) stop(sprintf(
		"%s is not founded or version is lower than required", names(pkgs)[i]))
	else{
		message(sprintf("%s loaded", names(pkgs)[i]))
		library(names(pkgs)[i], character.only = TRUE)
	}
}

db <- dbConnect(SQLite(), sqlitePath)

# add CCI in cluster table
tableCluster <- dbGetQuery(db, 'pragma table_info(cluster);')
if(!('CCI' %in% tableCluster$name)) dbExecute(db, 'alter table cluster add column CCI float default 0;')

dbExecute(db, 'pragma journal_mode=wal;')
dbExecute(db, 'pragma auto_vacuum = incremental;')

source(file.path(appwd, "utils/app.R"))
},
error = function(e) {
	msg <- sprintf("Startup failed with error(s):\n\n%s", e$message)
	message(msg)
	tcltk::tk_messageBox(
		type = "ok",
		message = msg)
	close(pb)
})

message("application terminated normally")
