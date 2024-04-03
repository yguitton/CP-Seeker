if (is.null(config$appname)) stop('Missing appname in file config.cfg')
if (is.null(config$pkgs)) stop('Missing pkgs in file config.cfg')

if (is.null(reg_paths$rtools)) stop('Missing rtools in file reg_paths.json')
if (is.null(reg_paths$chromium)) stop('Missing chromium in file reg_paths.json')
if (is.null(reg_paths$converter)) stop('Missing converter in file reg_paths.json')
if (is.null(reg_paths$thermo)) stop('Missing thermo in file reg_paths.json')
if (is.null(reg_paths$create_database)) stop('Missing create_database in file reg_paths.json')
if (is.null(reg_paths$chemical)) stop('Missing chemical in file reg_paths.json')
if (is.null(reg_paths$chemical_ion)) stop('Missing chemical_ion in file reg_paths.json')
if (is.null(reg_paths$create_database)) stop('Missing create_database in file reg_paths.json')
if (is.null(reg_paths$sqlite_path)) stop('Missing sqlitePath in file reg_paths.json')
if (is.null(reg_paths$sqlite_lighted_path)) stop('Missing sqlite lighted path in file reg_paths.json')
if (is.null(reg_paths$documentation_file)) stop('Missing documentation file in file reg_paths.json')

for (i in 1:length(reg_paths)){
	name <- names(reg_paths[i])
	path <- reg_paths[[i]]
	path <- gsub('\\\\', '/', path)
    test <- file.exists(path)
    # keep dirOutput as a relative path for recording files
	if (!test & (name == "chemical")) {
        utils::setWinProgressBar(pb, 0, label = "Generate chloroparaffin...")
        source("data/generate_chloroparaffin.R")
    } else if (!test & (name == "chemical_ion")) {
        utils::setWinProgressBar(pb, 0, label = "Generate chloroparaffin ions...")
        source("data/generate_chloroparaffin_ions.R")
    } else if(!test & (name == "sqlite_path")) {
		utils::setWinProgressBar(pb, 0, label = "Creating database file...")  
        db <- RSQLite::dbConnect(RSQLite::SQLite(), path)
        queries <- readLines(reg_paths$create_database)
        lapply(queries, function(x) RSQLite::dbExecute(db, x))
		RSQLite::dbWriteTable(db, "chemical", read.csv(
			reg_paths$chemical, stringsAsFactors = FALSE))
		RSQLite::dbWriteTable(db, "chemical_ion", read.csv(
			reg_paths$chemical_ion, stringsAsFactors = FALSE))
		RSQLite::dbDisconnect(db)
    } else if(name == "r" | name == "sqlite_lighted_path") {
        next
    } else if (!test) {
        stop(sprintf('%s not found at %s', name, tools::file_path_as_absolute(path)))
    }
	reg_paths[[i]] <- tools::file_path_as_absolute(reg_paths[[i]])
}

appname <- config$appname
pkgs <- config$pkgs
rtools <- reg_paths$rtools
chromium <- reg_paths$chromium
converter <- reg_paths$converter
thermo <- reg_paths$thermo
sqlite_path <- reg_paths$sqlite_path
sqlite_lighted_path <- reg_paths$sqlite_lighted_path
documentation_file <- reg_paths$documentation_file

# Root PATH
app_root <- normalizePath(".", mustWork = FALSE)

# create a config file if not exists already
config_dir <- file.path(app_root, "export")

# Create the "export" directory if it doesn't exist
config_file <- file.path(config_dir, "user.cfg")
if (!dir.exists(config_dir)) dir.create(config_dir)
if (file.exists(config_file)) {
	txt <- readLines(config_file)
	last_user <- txt[1]
	last_project <- txt[2]
} else {
	last_user <- NULL
	last_project <- NULL
}