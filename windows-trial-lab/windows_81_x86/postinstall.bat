set POWERSHELL=%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe
set POSTINSTALLSCRIPT=C:\PackerTemp\postinstall\postinstall.ps1
dir C:\
dir C:\PackerTemp
dir C:\PackerTemp\postinstall
%POWERSHELL% -NoLogo -NoProfile -NonInteractive -File %POSTINSTALLSCRIPT%
