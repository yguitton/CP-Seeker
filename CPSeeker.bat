@ECHO OFF
SET registreChemin=HKEY_CURRENT_USER\Control Panel\International
SET registreCle=sDecimal
SET registreType=REG_SZ
SET registreValeurVirgule=,
SET registreValeurPoint=.
REG QUERY "%registreChemin%" /v "%registreCle%" | FIND "%registreValeurVirgule%"
CLS
IF %ERRORLEVEL%==0 (GOTO :ERROR) ELSE (GOTO :LAUNCH)

:LAUNCH
WSCRIPT utils\wsf\run.wsf
:END
GOTO :eof

:ERROR
SET msgboxTitle=Error starting CPSeeker
SET msboxBody=The application need a point as decimal separator to work
SET tmpmsgbox=%temp%\~tmpmsgbox.vbs
IF EXIST "%tmpmsgbox%" DEL /F /Q "%tmpmsgbox%"
ECHO msgbox "%msboxBody%",0,"%msgboxTitle%">"%tmpmsgbox%"
WSCRIPT "%tmpmsgbox%"
:END
GOTO :eof