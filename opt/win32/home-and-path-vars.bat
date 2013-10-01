@echo off

rem Setting the %HOME% variable to %USERPROFILE%
rem Setting the %PATH% variable 


rem I have to put some subroutines at the top; this skips over them
goto :STARTSCRIPT


rem Note that I couoldn't do a nested if statement without all these 
rem    `call` statements. 
rem   (cmd syntax is bullshit.)
:s_check_exists
echo Checking to see if path exists: %*
if exist "%*" call :s_exists_yes %*
goto :eof

:s_exists_yes
echo Adding: %*
rem Note: Windows does not require double-quotes around entries in %PATH% --
rem   even entries with spaces. In fact, when I call the setx argument, it
rem   appears to strip them. However. I couldn't figure out a way to 
rem   make this script work without adding them. 
rem   (cmd syntax is bullshit.)
if defined MYPATH (
    set MYPATH=%MYPATH%;"%*"
) else (
    set MYPATH="%*"
)
goto :eof


:STARTSCRIPT

:s_environment
rem Note that this will copy the VALUE of %USERPROFILE% to %HOME%
rem so if %USERPROFILE% changes, %HOME% will not unless you do it manually
setx HOME %USERPROFILE%
set HOME=%USERPROFILE%
setx PYTHONSTARTUP %HOME%\.dhd\hbase\python.profile
set PYTHONSTARTUP=%HOME%\.dhd\hbase\python.profile

:: Something is setting TERM=dumb and that means that git displays everything with
:: no color, and git's less warns that my term is dumb before displaying in e.g.
:: git log. 
:: Changing this to msys seems to work for me even in cmd.exe so...
set TERM=msys
setx TERM msys

:: This sets up for Emacs (to go in opt\) and other shit (to go in opt\win32bin)
md %HOME%\opt\win32bin
:s_environment_end


:s_path
echo You're starting from the top of s_path...

set MYPATH=

echo 0: START
call :s_check_exists C:\Chocolatey\bin
echo 1
call :s_check_exists %HOME%\.dhd\opt\win32bin
echo 11
call :s_check_exists %HOME%\opt\win32bin
echo 2
call :s_check_exists %HOME%\opt\Console2
echo 3
call :s_check_exists %HOME%\opt\SysinternalsSuite
echo 4
call :s_check_exists %HOME%\opt\mupdf
echo 5
call :s_check_exists C:\opt\strawberry\perl\bin
echo 6
call :s_check_exists C:\opt\Python32
echo 7
call :s_check_exists C:\opt\Python32\scripts
echo 8
rem call :s_check_exists C:\opt\Python27
echo 9
call :s_check_exists C:\opt\Python27\scripts
echo a
call :s_check_exists C:\opt\GnuWin32\bin
echo b
call :s_check_exists C:\opt\GnuWin32\sbin
echo c
rem call :s_check_exists C:\opt\local\bin
echo d
rem call :s_check_exists C:\opt\svn\bin
echo e
rem call :s_check_exists C:\opt\SysinternalsSuite
echo f
rem call :s_check_exists C:\opt\nirsoft64
echo g
rem call :s_check_exists C:\opt\nirsoft_package
echo h
rem call :s_check_exists C:\opt\Console2
echo i
rem call :s_check_exists C:\opt\UnxUtils\bin
echo j
rem call :s_check_exists C:\opt\UnxUtils\usr\local\wbin
echo k
call :s_check_exists C:\opt\sqlite
echo l
call :s_check_exists C:\Program Files (x86)\Git\cmd
echo m
call :s_check_exists C:\Program Files\PuTTY
echo n
call :s_check_exists C:\Program Files\7-Zip
echo o
call :s_check_exists C:\Program Files (x86)\7-Zip
echo p
call :s_check_exists C:\Program Files (x86)\PuTTY
echo q
rem call :s_check_exists C:\Program Files\Windows SDKs\Windows\v7.0\Bin
echo r
call :s_check_exists C:\Program Files\NSIS
echo s
call :s_check_exists C:\Program Files (x86)\NSIS
echo t
call :s_check_exists C:\Program Files\Nmap
echo u
call :s_check_exists C:\Program Files (x86)\Nmap
echo v
call :s_check_exists C:\Program Files (x86)\VMware\VMware Virtual Disk Development Kit\bin
echo w
call :s_check_exists C:\Program Files\VMware\VMware Virtual Disk Development Kit\bin
echo x: END

:s_path_end

rem If you want to set the system %PATH%, use the /m flag like: 
rem     setx PATH "C:\opt\bin;C:\opt\ntemacs\bin;" /m
rem Note that the %PATH% variable is a concatenation of user's path and system path environment variables
rem     which you can define in System Properties -> Advanced -> Environment Variables
rem     and by default, user path is empty but the system path contains system settings...
rem     we're just overwriting the user path here.
setx PATH %MYPATH%
