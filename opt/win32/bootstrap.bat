@echo off

:: Bootstrap a new Windows box
:: Assumes .dhd folder
:: %~dp0 means the folder that this batch script is in lololol
:: run scripts through 'cmd /c' so that an error in the script won't cause
:: this script to stop too (what). 

echo Bootstrapping a new Windows box.
echo Important functions will require admin access.

set dhd=%USERPROFILE%\.dhd

echo Changing capslock to control...
reg import "%~dp0caps_lock_to_control.reg"

echo Setting home and path related variables (might require logout for full effect)...
cmd /c "%~dp0home-and-path-vars.bat"

echo Setting up filetypes...
cmd /c "%~dp0filetypes.bat"

echo Setting powershell execution policy bullshit...
reg import "%~dp0powershell-executionpolicy-unrestricted.reg"

echo Copying Powershell profile (this will not work on Windows XP)...
md "%home%\Documents\WindowsPowerShell"
copy "%dhd%\hbase\Microsoft.PowerShell_profile.win32.ps1" "%home%\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1"

echo Copying .emacs...
copy "%dhd%\hbase\.emacs.w32.el" "%home%\.emacs"

pause