@echo off

set EMACSBINPATH=%USERPROFILE%\opt\emacs-23.4\bin
"%EMACSBINPATH%\emacsclientw.exe" -na "%EMACSBINPATH%\runemacs.exe" "%*"


rem runemacs.exe if there's no server, emacsclientw.exe if there is. 
rem No bullshit with two separate Win7 taskbar icons, no DOS window
rem (except at the beginning).

rem You could also change this file to contain this instead of the
rem absolute path to emacsclientw and runemacs: 
rem     "%~dp0emacsclientw.exe" -na "%~dp0runemacs.exe" "%1"

rem If you do that, you can place it in the emacs\bin\ directory
rem and call it directly. (The %~dp0 junk means "location of this
rem batch file" to Windows.)