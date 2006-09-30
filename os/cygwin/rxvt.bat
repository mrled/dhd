@echo off
C:
chdir \cygwin\bin

REM # Cygwin behavior: dump core + no title in windows (only on NT)
set CYGWIN="error_start:C:\cygwin\bin\dumper.exe notitle"
\cygwin\bin\rxvt.exe -g 80x32+100+100 -bg black -fg white -cr #00ff00 -pr #ff0000 -fn "Lucida Console-11" -sl 4096 -vb -e /usr/bin/tcsh -l
