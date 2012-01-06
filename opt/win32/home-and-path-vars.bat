@echo off

rem Setting the %HOME% variable to %USERPROFILE%
rem Setting the %PATH% variable 


:s_home
rem Note that this will copy the VALUE of %USERPROFILE% to %HOME%
rem so if %USERPROFILE% changes, %HOME% will not unless you do it manually
setx HOME %USERPROFILE%
:s_home_end


:s_path
rem If you want to set the system %PATH%, use the -m flag like: 
rem     setx PATH "C:\opt\bin;C:\opt\ntemacs\bin;" 

rem Note: Windows does not require double-quotes around entries in %PATH% --
rem   even entries with spaces. In fact, when I call the setx argument, it
rem   appears to strip them. However. I couldn't figure out a way to 
rem   make this script work without adding them. 
rem   (cmd syntax is bullshit.)

set MYPATH=

call :s_check_exists %HOME%\.dhd\opt\win32bin
call :s_check_exists %HOME%\opt\win32bin
call :s_check_exists C:\strawberry\perl\bin
call :s_check_exists C:\Python32
call :s_check_exists C:\opt\local\bin
call :s_check_exists C:\opt\ntemacs24\bin
call :s_check_exists C:\opt\git\bin
call :s_check_exists C:\opt\svn\bin
call :s_check_exists C:\opt\SysinternalsSuite
call :s_check_exists C:\opt\nirsoft64
call :s_check_exists C:\opt\nirsoft_package
call :s_check_exists C:\opt\Console2
call :s_check_exists C:\opt\UnxUtils\bin
call :s_check_exists C:\opt\UnxUtils\usr\local\wbin
call :s_check_exists C:\opt\sqlite
call :s_check_exists C:\Program Files\PuTTY
call :s_check_exists C:\Program Files\7-Zip
call :s_check_exists C:\Program Files (x86)\7-Zip
call :s_check_exists C:\Program Files (x86)\PuTTY
call :s_check_exists C:\Program Files\Windows SDKs\Windows\v7.0\Bin
call :s_check_exists C:\Program Files\NSIS

rem These are no longer used: 
rem call :s_check_exists C:\Program Files\Emacs\emacs
rem call :s_check_exists C:\Program Files (x86)\Emacs\emacs

setx PATH %MYPATH%
set MYPATH=

rem This is helpful for Python scripts
set MYPATHEXT=%PATHEXT%;".PY"
setx PATHEXT %MYPATHEXT%
set MYPATHEXT=

goto :s_path_end

rem Note that I couoldn't do a nested if statement without all these 
rem    `call` statements. 
rem   (cmd syntax is bullshit.)
:s_check_exists
if exist "%*" call :s_exists_yes %*
goto :eof

:s_exists_yes
rem echo Adding: %*
if defined MYPATH (
    set MYPATH=%MYPATH%;"%*"
) else (
    set MYPATH="%*"
)
goto :eof

:s_path_end
