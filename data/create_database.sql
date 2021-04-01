CREATE TABLE `user`( 
	`user` TEXT PRIMARY KEY
);
CREATE TABLE `sample` ( 
	`sample` TEXT PRIMARY KEY, 
	`raw_data` BLOB NOT NULL, 
	`raw_path` TEXT NOT NULL, 
	`polarity` TEXT NOT NULL, 
	`path` TEXT, 
	`instrument_model` TEXT, 
	`instrument_manufacturer` TEXT, 
	`software_name` TEXT, 
	`software_version` TEXT, 
	`ion_source` TEXT, 
	`analyzer` TEXT, 
	`detector_type` TEXT, 
	`resolution` TEXT, 
	`agc_target` TEXT, 
	`maximum_it` TEXT, 
	`number_of_scan_range` TEXT, 
	`scan_range` TEXT
);
CREATE TABLE `project_sample` ( 
	`project_sample` INTEGER PRIMARY KEY AUTOINCREMENT, 
	`project` INTEGER NOT NULL, 
	`sample` TEXT NOT NULL, 
	`sample_id` TEXT NOT NULL 
);
CREATE TABLE `project` ( 
	`project` INTEGER PRIMARY KEY AUTOINCREMENT, 
	`name` TEXT NOT NULL, 
	`comments` TEXT, 
	`creation` DATE, 
	`modified` DATE, 
	`deconvolution_param` INTEGER 
);
CREATE TABLE `deconvolution_param` ( 
	`deconvolution_param` INTEGER PRIMARY KEY AUTOINCREMENT, 
	`adduct` TEXT NOT NULL, 
	`resolution` = REAL, 
	`resolution_mz` = REAL, 
	`resolution_index` = INTEGER, 
	`ppm` REAL NOT NULL, 
	`peakwidth_min` REAL NOT NULL,  
	`peakwidth_max` REAL NOT NULL, 
	`missing_scans` INTEGER NOT NULL 
);
CREATE TABLE `chloroparaffin` (
	`chloroparaffin` INTEGER PRIMARY KEY, 
	`C` INTEGER NOT NULL, 
	`Cl` INTEGER NOT NULL, 
	`H` INTEGER NOT NULL, 
	`formula` TEXT NOT NULL 
);
CREATE TABLE `chloroparaffin_ion` (
	`chloroparaffin_ion` INTEGER PRIMARY KEY, 
	`ion_formula` TEXT NOT NULL, 
	`adduct` TEXT NOT NULL, 
	`charge` INTEGER NOT NULL, 
	`chloroparaffin` INTEGER NOT NULL
);
CREATE TABLE `feature` (
	`feature` INTEGER PRIMARY KEY AUTOINCREMENT, 
	`mz` REAL NOT NULL, 
	`mzmin` REAL NOT NULL,
	`mzmax` REAL NOT NULL,
	`rt` REAL NOT NULL,
	`rtmin` REAL NOT NULL,
	`rtmax` REAL NOT NULL,
	`into` REAL NOT NULL,
	`intb` REAL NOT NULL,
	`maxo` REAL NOT NULL,
	`sn` REAL NOT NULL,
	`scale` INTEGER NOT NULL, 
	`scpos` INTEGER NOT NULL, 
	`scmin` INTEGER NOT NULL, 
	`scmax` INTEGER NOT NULL, 
	`theoric_mz` REAL NOT NULL,
	`theoric_abundance` REAL NOT NULL,
	`iso` TEXT NOT NULL, 
	`abundance` REAL NOT NULL,
	`chloroparaffin_ion` INTEGER NOT NULL, 
	`project_sample` INTEGER NOT NULL
);