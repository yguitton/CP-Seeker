@ECHO OFF
SET registreChemin=HKEY_CURRENT_USER\Control Panel\International
SET registreCle=sDecimal
SET registreType=REG_SZ
SET registreValeurVirgule=,
SET registreValeurPoint=.
REG QUERY "%registreChemin%" /v "%registreCle%" | FIND "%registreValeurPoint%"
CLS
IF %ERRORLEVEL%==1 GOTO ADD_POINT
:ADD_POINT
REG ADD "HKEY_CURRENT_USER\Control Panel\International" /f /v "sDecimal" /t %registreType% /d "%registreValeurPoint%"
CLS
ECHO Change Separateur decimal "," to "."
ECHO Launch app
wscript utils\wsf\run.wsf
REG ADD "HKEY_CURRENT_USER\Control Panel\International" /f /v "sDecimal" /t %registreType% /d "%registreValeurVirgule%"
GOTO END
:END

ECHO Launch R
wscript utils\wsf\run.wsf
