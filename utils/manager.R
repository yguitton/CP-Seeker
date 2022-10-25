# change default language to english
Sys.setenv(LANG = "en") 
appwd <- getwd()

# Read the application config
config <- jsonlite::fromJSON("utils/config.cfg")
reg_paths <- jsonlite::fromJSON("utils/regpaths.json")

pb <- utils::winProgressBar(
	title = sprintf("Starting %s ...", config$appname),
	label = "Initializing ...")  
  
tryCatch({

print('check dir & files...')
if(!file.exists("utils/checks.R")) stop('Missing ckecks.R file')
source("utils/checks.R")

# change local PATH for the use of rtools
print('add path...')
Sys.setenv(PATH = paste(rtools, Sys.getenv("PATH"), sep=";"))

# ensure all package dependencies are installed
applibpath <- file.path(appwd, reg_paths$r, "library")
.libPaths(applibpath)

# Load functions to ensure software dependencies
source("utils/ensure.R")

message("library paths:\n", paste0("... ", .libPaths(), collapse = "\n"))
message("working path:\n", paste("...", appwd))

print("ensuring packages dependencies ...")
utils::setWinProgressBar(pb, 0, label = "Ensuring package dependencies ...")  
for (i in seq_along(pkgs)) {
	utils::setWinProgressBar(pb,
		value = i / (length(pkgs) + 1),
		label = sprintf("Loading package - %s", pkgs[i]))
	if(ensure(pkgs[i], names(pkgs)[i], pb) == FALSE) stop(sprintf(
		"%s is not founded or version is incorrect", names(pkgs)[i]))
	else {
		library(names(pkgs)[i], character.only = TRUE)
		print(sprintf("%s loaded", names(pkgs)[i]))
	}
}

db <- RSQLite::dbConnect(RSQLite::SQLite(), sqlite_path)
RSQLite::dbExecute(db, 'pragma temp_store = memory;')
RSQLite::dbExecute(db, 'pragma synchronous = normal;')
RSQLite::dbExecute(db, 'pragma locking_mode = normal;')
RSQLite::dbExecute(db, 'pragma cache_size = 1000000;')
RSQLite::dbExecute(db, 'pragma journal_mode = wal;')
RSQLite::dbExecute(db, 'pragma auto_vacuum = FULL;')

available_adducts <- RSQLite::dbGetQuery(db, "select distinct adduct from chemical_ion ;")
ecni_adduct <- RSQLite::dbGetQuery(db, 'select distinct adduct as ecni from chemical_ion where chemical_ion_family = "ECNI";')

#esiapci_adducts <- RSQLite::dbGetQuery(db, 'select distinct adduct as esiapci from chemical_ion where chemical_ion_family = "ESI/APCI";')
#available_adducts <- RSQLite::dbGetQuery(db, "Select DISTINCT  ' [ '  || adduct ||' ] '  || '-' as adduct from chemical_ion;")
utils::setWinProgressBar(pb, 1.00, label = "Starting app")  
close(pb)

source(file.path(appwd, "utils/app.R"))

print("application terminated normally")
}, error = function(e) {
	close(pb)
	msg <- sprintf("Startup failed with error(s):\n\n%s", e$message)
	print(msg)
	tcltk::tk_messageBox(
		type = "ok",
		message = msg)
})
if (!is.null(db)) DBI::dbDisconnect(db)
rm(list = ls())
gc()
source( "data/update.R")
# q("no")