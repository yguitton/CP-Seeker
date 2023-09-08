#define MyAppName "CPSeeker"
#define MyAppVersion "2.1.1"
#define MyAppExeName "CPSeeker.bat"

[Setup]
AppName = {#MyAppName}
DefaultDirName = {sd}\{#MyAppName}
DefaultGroupName = {#MyAppName}
OutputDir = .
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
DisableProgramGroupPage=false
DisableWelcomePage=false
UsePreviousAppDir=true



[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"

[Files]
Source: "chromium/*"; DestDir: "{app}\chromium"; Flags: ignoreversion createallsubdirs recursesubdirs; Permissions: everyone-full;
Source: "data/*"; DestDir: "{app}\data"; Flags: ignoreversion createallsubdirs recursesubdirs; Permissions: everyone-full;
Source: "pwiz/*"; DestDir: "{app}\pwiz"; Flags: ignoreversion createallsubdirs recursesubdirs; Permissions: everyone-full;
Source: "R-Portable/*"; DestDir: "{app}\R-Portable"; Flags: ignoreversion createallsubdirs recursesubdirs; Permissions: everyone-full;
Source: "Rtools/*"; DestDir: "{app}\Rtools"; Flags: ignoreversion createallsubdirs recursesubdirs; Permissions: everyone-full;
Source: "src/*"; DestDir: "{app}\src"; Flags: ignoreversion createallsubdirs recursesubdirs; Permissions: everyone-full;
Source: "server/*"; DestDir: "{app}\server"; Flags: ignoreversion; Permissions: everyone-full;
Source: "ui/*"; DestDir: "{app}\ui"; Flags: ignoreversion; Permissions: everyone-full;
Source: "utils/*"; DestDir: "{app}\utils"; Flags: ignoreversion createallsubdirs recursesubdirs; Permissions: everyone-full;
Source: "www/*"; DestDir: "{app}\www"; Flags: ignoreversion; Permissions: everyone-full;
Source: "default.ico"; DestDir: "{app}"; Flags: ignoreversion; Permissions: everyone-full;
Source: "{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion; Permissions: everyone-full;
Source: "LICENSE"; DestDir: "{app}"; Flags: ignoreversion; Permissions: everyone-full;
Source: "server.R"; DestDir: "{app}"; Flags: ignoreversion; Permissions: everyone-full;
Source: "setup.ico"; DestDir: "{app}"; Flags: ignoreversion; Permissions: everyone-full;
Source: "ui.R"; DestDir: "{app}"; Flags: ignoreversion; Permissions: everyone-full;

[Dirs]
Name: "{app}\mzXMLFiles\negative"; Permissions: everyone-full;
Name: "{app}\mzXMLFiles\positive"; Permissions: everyone-full;

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Comment: "HaloSeeker description"; IconFilename: "{app}\default.ico"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commonprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Comment: "HaloSeeker description"; IconFilename: "{app}\default.ico"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon; Comment: "HaloSeeker description"; IconFilename: "{app}\default.ico"

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: runhidden shellexec postinstall skipifsilent

[UninstallDelete]
Type: filesandordirs; Name: "{app}";