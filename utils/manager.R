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

.libPaths(c(applibpath, .libPaths()))

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

# add the column thresold if it doesn't exist
db <- dbConnect(SQLite(), sqlitePath)
cols <- dbGetQuery(db, 'pragma table_info("observed");')
if(!any(cols$name == "threshold")) dbSendQuery(db, 'alter table observed add column threshold integer default 0 not null;')
cols <- dbGetQuery(db, 'pragma table_info("project_sample");')
if(!any(cols$name == "zVal")) dbSendQuery(db, 'alter table project_sample add column 
	zVal float default null;')
cols <- dbGetQuery(db, 'pragma table_info("sample");')
if(!any(cols$name == "polarity")) dbSendQuery(db, 'alter table sample add column 
	polarity varchar (45) default "negative";')
tableNames <- dbGetQuery(db, 'select name from sqlite_master where type == "table";')$name
if(!any(tableNames == "triangle")) dbSendQuery(db, 'create table triangle (id integer primary key autoincrement,
	project_sample integer not null, x float not null, y float not null, 
	z float not null, triangle integer not null, 
	foreign key(project_sample) references project_sample(project_sample));')

dbSendQuery(db, 'pragma journal_mode=wal;')

source(file.path(appwd, "utils/app.R"))
},
error = function(e) {
msg <- sprintf("Startup failed with error(s):\n\n%s", e$message)
message(msg)
tcltk::tk_messageBox(
type = "ok",
message = msg,
icon = "error")
close(pb)
})

message("application terminated normally")
