BEGIN TRANSACTION;
DROP TABLE IF EXISTS `sample`;
CREATE TABLE IF NOT EXISTS `sample` (
	`sample`	VARCHAR NOT NULL,
	`rawPath`	VARCHAR NOT NULL,
	`instrumentModel`	VARCHAR NOT NULL,
	`instrumentManufacturer`	VARCHAR NOT NULL,
	`softwareName`	VARCHAR NOT NULL,
	`softwareVersion`	VARCHAR NOT NULL,
	`ionSource`	VARCHAR NOT NULL,
	`analyzer`	VARCHAR NOT NULL,
	`detectorType`	VARCHAR NOT NULL,
	`resolution`	VARCHAR,
	`agcTarget`	VARCHAR,
	`maximumIT`	VARCHAR,
	`numberOfScanRange`	VARCHAR,
	`scanRange`	VARCHAR,
	`polarity`	varchar ( 45 ) NOT NULL,
	`raw`	BLOB,
	`path`	VARCHAR ( 45 ),
	PRIMARY KEY(`sample`)
);
DROP TABLE IF EXISTS `project_sample_adduct`;
CREATE TABLE IF NOT EXISTS `project_sample_adduct` (
	`project_sample_adduct`	INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
	`project_sample`	INTEGER NOT NULL,
	`adduct`	varchar ( 45 ) NOT NULL,
	`machine`	varchar ( 45 ) NOT NULL
);
DROP TABLE IF EXISTS `project_sample`;
CREATE TABLE IF NOT EXISTS `project_sample` (
	`project_sample`	INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
	`sample`	TEXT NOT NULL,
	`project`	TEXT NOT NULL,
	`sampleID`	TEXT NOT NULL
);
DROP TABLE IF EXISTS `project`;
CREATE TABLE IF NOT EXISTS `project` (
	`project`	INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
	`name`	TEXT,
	`comment`	TEXT
);
DROP TABLE IF EXISTS `profile`;
CREATE TABLE IF NOT EXISTS `profile` (
	`profile`	varchar ( 45 ) NOT NULL,
	`vTarget`	INTEGER NOT NULL,
	`zCut`	float NOT NULL,
	`project_sample_adduct`	INTEGER,
	`xlsxFile`	TEXT,
	PRIMARY KEY(`profile`)
);
DROP TABLE IF EXISTS `point`;
CREATE TABLE IF NOT EXISTS `point` (
	`point`	INTEGER NOT NULL,
	`x`	float NOT NULL,
	`y`	float NOT NULL,
	`z`	float NOT NULL,
	`zone`	INTEGER,
	`triangle`	INTEGER,
	`profile`	varchar ( 45 ) NOT NULL,
	PRIMARY KEY(`point`)
);
DROP TABLE IF EXISTS `feature`;
CREATE TABLE IF NOT EXISTS `feature` (
	`feature`	INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
	`mz`	REAL NOT NULL,
	`mzmin`	REAL NOT NULL,
	`mzmax`	REAL NOT NULL,
	`maxo`	REAL NOT NULL,
	`into`	REAL NOT NULL,
	`abundance`	REAL NOT NULL,
	`rt`	REAL NOT NULL,
	`rtmin`	REAL NOT NULL,
	`rtmax`	REAL NOT NULL,
	`iso`	TEXT NOT NULL,
	`sn`	REAL NOT NULL,
	`scale`	INTEGER NOT NULL,
	`cluster`	INTEGER NOT NULL,
	`lmin`	INTEGER NOT NULL,
	`lmax`	INTEGER NOT NULL,
	`scpos`	INTEGER NOT NULL,
	`scmin`	INTEGER NOT NULL,
	`scmax`	INTEGER NOT NULL
);
DROP TABLE IF EXISTS `cluster`;
CREATE TABLE IF NOT EXISTS `cluster` (
	`cluster`	INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
	`formula`	TEXT NOT NULL,
	`C`	INTEGER NOT NULL,
	`Cl`	INTEGER NOT NULL,
	`score`	REAL NOT NULL,
	`project_sample_adduct`	INTEGER NOT NULL,
	`rtMean`	REAL NOT NULL,
	`deviation`	REAL NOT NULL,
	`ppm`	REAL NOT NULL,
	`peakwidth1`	REAL NOT NULL,
	`peakwidth2`	NUMERIC NOT NULL,
	`machine`	INTEGER,
	`ion_formula`	TEXT,
	`charge`	INTEGER
);
DROP TABLE IF EXISTS `adduct`;
CREATE TABLE IF NOT EXISTS `adduct` (
	`adduct`	varchar ( 45 ),
	`charge`	integer,
	`formula_add`	varchar ( 45 ),
	`formula_ded`	varchar ( 45 ),
	`multi`	integer,
	PRIMARY KEY(`adduct`)
);
COMMIT;
