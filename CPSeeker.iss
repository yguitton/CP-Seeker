#define MyAppName "targetROI"
#define MyAppVersion "0.1"
#define MyAppExeName "targetROI.bat"

[Setup]
AppName = {#MyAppName}
DefaultDirName = {pf}\{#MyAppName}
DefaultGroupName = {#MyAppName}
OutputDir = C:/Users/shu/Documents/Inno_installer
OutputBaseFilename = setup_{#MyAppName}
SetupIconFile = setup.ico
AppVersion = {#MyAppVersion}
PrivilegesRequired = lowest
Compression = lzma2/ultra
SolidCompression = yes
ArchitecturesAllowed = x64 ia64
ArchitecturesInstallIn64BitMode = x64
LicenseFile = LICENSE
CloseApplications = yes
RestartApplications = yes
Uninstallable = yes
DisableProgramGroupPage=yes
DisableWelcomePage=false



[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"

[Files]
Source: "AppInfo/*"; DestDir: "{app}\AppInfo"; Flags: ignoreversion createallsubdirs recursesubdirs;
Source: "R-Portable/*"; DestDir: "{app}\R-Portable"; Flags: ignoreversion createallsubdirs recursesubdirs;
Source: "server/*"; DestDir: "{app}\server"; Flags: ignoreversion;
Source: "ui/*"; DestDir: "{app}\ui"; Flags: ignoreversion;
Source: "utils/*"; DestDir: "{app}\utils"; Flags: ignoreversion createallsubdirs recursesubdirs;
Source: "www/*"; DestDir: "{app}\www"; Flags: ignoreversion;
Source: "default.ico"; DestDir: "{app}"; Flags: ignoreversion;
Source: "{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion;
Source: "LICENSE"; DestDir: "{app}"; Flags: ignoreversion;
Source: "server.R"; DestDir: "{app}"; Flags: ignoreversion;
Source: "setup.ico"; DestDir: "{app}"; Flags: ignoreversion;
Source: "ui.R"; DestDir: "{app}"; Flags: ignoreversion;

[Dirs]


[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Comment: "{#MyAppName} description"; IconFilename: "{app}\default.ico"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commonprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Comment: "{#MyAppName} description"; IconFilename: "{app}\default.ico"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon; Comment: "{#MyAppName} description"; IconFilename: "{app}\default.ico"

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: runhidden shellexec postinstall skipifsilent


[UninstallDelete]
Type: filesandordirs; Name: "{app}";
