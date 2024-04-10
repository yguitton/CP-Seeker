// JSON object provided by json2.js since WSH JScript interpreter doesn't have/expose it
// https://github.com/douglascrockford/JSON-js/blob/master/json2.js
//
// JSON.minify provided by https://github.com/getify/JSON.minify
// to strip out comments from JSON so that it can be parsed without error.
//
// This script and dependencies are is loaded into the interpreter using a .wsf file:
// http://stackoverflow.com/questions/14319592/jscript-dynamically-load-javascript-libraries

//' Instantiate required objects
var oFSO = WScript.CreateObject("Scripting.FileSystemObject");
var oShell = WScript.CreateObject("WScript.Shell");

// Load Configuration File
var fConfig = oFSO.OpenTextFile('utils\\config.cfg', 1); // 1 = for reading
var sConfig = (fConfig.AtEndOfStream) ? "" : fConfig.ReadAll();
if (this.JSON) {
	var oConfig = (sConfig !== "") ? JSON.parse(JSON.minify(sConfig)) : undefined;
} else {
	oShell.Popup('Error: JSON object not found, cannot process configuration');
	WScript.Quit(1);
}

// Load Registry Paths
var fRegPaths = oFSO.OpenTextFile('utils\\regpaths.json', 1); // 1 = for reading
var sRegPaths = (fRegPaths.AtEndOfStream) ? "" : fRegPaths.ReadAll();
if (this.JSON) {
	var oRegPaths = (sRegPaths !== "") ? JSON.parse(JSON.minify(sRegPaths)) : undefined;
} else {
	oShell.Popup('Error: JSON object not found, cannot process registry paths');
	WScript.Quit(1);
}

// Determine where to keep the error log
// If deployed to users individually, keep with the deployment (default)
// If deployed to a central location (e.g. a network share) use a directory in
// each user's %userprofile%

// Old code 
// sLogPath = 'log';
// //' Determine User Home directory
// var sUPath = oShell.ExpandEnvironmentStrings("%USERPROFILE%");
// var sLogPath = sUPath + "\\." + oConfig.appname;

// New version 
// Define the log directory path to store the error log file.
// It is obtained by going three levels up from the current script's directory.
var sLogPath = oFSO.GetParentFolderName(oFSO.GetParentFolderName(oFSO.GetParentFolderName(WScript.ScriptFullName))) + "\\Error_log";

//' Create the error log directory if it does not exist
if (!oFSO.FolderExists(sLogPath)) {
    try {
        oFSO.CreateFolder(sLogPath);
        WScript.Echo("Directory created: " + sLogPath);
    } catch(e) {
        WScript.Echo("Error creating directory: " + sLogPath);
        WScript.Quit(1); // Quit the script if directory creation fails
    }
}

//' Create an application log directory as needed
if (!oFSO.FolderExists(sLogPath)) {
	oFSO.CreateFolder(sLogPath);
}

// Ajoutez la date dans le nom du fichier error.log
var currentDate = new Date();
// Format de la date : YYYY-MM-DD_HH-MM
var formattedDate = currentDate.getFullYear() + '-' + ('0' + (currentDate.getMonth() + 1)).slice(-2) + '-' + ('0' + currentDate.getDate()).slice(-2) + '-' + ('0' + currentDate.getHours()).slice(-2) + 'h-' + ('0' + currentDate.getMinutes()).slice(-2) + 'min';
sLogFile = 'error_' + formattedDate + '.log';

//' Define the R interpreter
var Rbindir = "";
if (oRegPaths.r) {
	var Rbindir = oRegPaths.r;
}

//' Rscript.exe is much more efficient than R.exe CMD BATCH
var Rexe           = Rbindir + "\\bin\\Rscript.exe";
var Ropts          = "--vanilla";

//' --vanilla implies the following flags:
//' --no-save --no-environ --no-site-file --no-restore --no-Rconsole --no-init-file

if (!oFSO.FileExists(Rexe)) {
	oShell.Popup('Error: R executable not found:\n' + Rexe);
	WScript.Quit(1);
}

var RScriptFile    = "utils\\manager.R";
if (!oFSO.FileExists(RScriptFile)) {
	oShell.Popup('Error: manager.R not found:\n' + RScriptFile);
	WScript.Quit(1);
}

var Outfile        = sLogPath + "\\" + sLogFile;

// Charger le contenu actuel de regpaths.json
var regPathsContent = oFSO.OpenTextFile('utils\\regpaths.json', 1).ReadAll();

// Convertir le contenu JSON en objet JavaScript
var regPathsObject = JSON.parse(JSON.minify(regPathsContent));

// Mettre à jour le chemin du fichier journal d'erreurs
regPathsObject.error_log_path = Outfile;

// Convertir l'objet JavaScript mis à jour en chaîne JSON
var updatedRegPathsContent = JSON.stringify(regPathsObject, null, 2);

// Écrire la chaîne JSON dans le fichier regpaths.json
var regPathsFile = oFSO.OpenTextFile('utils\\regpaths.json', 2); // 2 = for writing
regPathsFile.Write(updatedRegPathsContent);
regPathsFile.Close();


var strCommand     = ['"' + Rexe + '"', Ropts, '"' + RScriptFile + '"', "1>", '"' + Outfile + '"', "2>&1"].join(" ");
// var strCommand     = ['"' + Rexe + '"', RScriptFile].join(" ");
var intWindowStyle = 0;
/*
' other values:
' 0 Hide the window and activate another window.
' 1 Activate and display the window. (restore size and position) Specify this flag when displaying a window for the first time.
' 2 Activate & minimize.
' 3 Activate & maximize.
' 4 Restore. The active window remains active.
' 5 Activate & Restore.
' 6 Minimize & activate the next top-level window in the Z order.
' 7 Minimize. The active window remains active.
' 8 Display the window in its current state. The active window remains active.
' 9 Restore & Activate. Specify this flag when restoring a minimized window.
' 10 Sets the show-state based on the state of the program that started the application.
*/

//' continue running script after launching R
var bWaitOnReturn  = false;

oShell.Run(strCommand, intWindowStyle, bWaitOnReturn);
