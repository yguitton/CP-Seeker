# check the two files

if(is.null(config$appname)) stop('Missing appname in file config.cfg')
if(is.null(reg_paths$chrome)) stop('Missing chrome in file reg_paths.json')
if(is.null(config$pkgs)) stop('Missing pkgs in file config.cfg')
if(is.null(config$pkgs$pkgs)) stop('Missing pkgs in file config.cfg')
if(!is.numeric(config$minC)) stop('minC is not numeric in file config.cfg')
if(!is.numeric(config$maxC)) stop('maxC is not numeric in file config.cfg')
if(!is.numeric(config$minCl)) stop('minCl is not numeric in file config.cfg')
if(!is.numeric(config$maxCl)) stop('maxCl is not numeric in file config.cfg')
if(!is.numeric(config$missingScans)) stop('missingScans is not numeric in file config.cfg')
if(!is.numeric(config$minScore)) stop('minPts is not numeric in file config.cfg')
if(!is.numeric(config$precision)) stop('precision is not numeric in file config.cfg')

if(is.null(reg_paths$sqlitePath)) stop('Missing sqlitePath in file reg_paths.json')
if(is.null(reg_paths$converter)) stop('Missing converter in file reg_paths.json')
if(is.null(reg_paths$thermo)) stop('Missing thermo in file reg_paths.json')

for(i in 1:length(reg_paths)){
	name <- names(reg_paths[i])
	if(name %in% c('r', 'chrome', 'ie', 'ff')) next
	path <- reg_paths[[i]]
	path <- gsub('\\\\', '/', path)
	if(!file.exists(path)) stop(sprintf('%s not found at %s', name, file_path_as_absolute(path)))
	else reg_paths[[i]] <- file_path_as_absolute(reg_paths[[i]])
}

appname <- config$appname
chrome <- reg_paths$chrome
pkgs <- config$pkgs$pkgs
minC <- config$minC
maxC <- config$maxC
minCl <- config$minCl
maxCl <- config$maxCl
missingScans <- config$missingScans
minScore <- config$minScore
precision <- config$precision
sqlitePath <- reg_paths$sqlitePath
converter <- reg_paths$converter
thermo <- reg_paths$thermo