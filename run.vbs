Set objShell = WScript.CreateObject("WScript.Shell")
strFolder = objShell.ExpandEnvironmentStrings("%userprofile%") & "\shiny"
Dim filesys
Set filesys = CreateObject("Scripting.FileSystemObject")
if Not filesys.FolderExists(strFolder) Then filesys.CreateFolder strFolder End If
filesys.CopyFile "runShinyApp.R", strFolder & "\", true  ' true - overwrite existing

'Run
Rexe = "R-Portable\App\R-Portable\bin\Rscript.exe"
Ropts = "--no-save --no-environ --no-init-file --no-restore --no-Rconsole"
RScriptFile = strFolder & "\runShinyApp.R"
Outfile = strFolder & "\ShinyAppError.log" 
strCommand = Rexe & " " & Ropts & " " & RScriptFile & " 1> " & Outfile & " 2>&1"
bWaitOnReturn  = False ' continue running script after launching R   '' the following is a Sub call, so no parentheses around arguments'
objShell.Run strCommand, intWindowStyle, bWaitOnReturn
 
 
'Finalise
set filesys = Nothing
set objShell = Nothing