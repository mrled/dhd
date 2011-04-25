@echo off

rem assoc tells you which file extensions are associated w/ which filetypes
rem ftype tells you which filetypes are associated w/ which programs

set emacsdir=C:\opt\ntemacs24\bin
ftype EmacsFile="%emacsdir%\emacsclientw.exe" -n -a "%emacsdir%\runemacs.exe" "%%1"
ftype txtfile="%emacsdir%\emacsclientw.exe" -n -a "%emacsdir%\runemacs.exe" "%%1"
ftype inifile="%emacsdir%\emacsclientw.exe" -n -a "%emacsdir%\runemacs.exe" "%%1"

assoc .el=EmacsFile
assoc .mdwn=txtfile
assoc .nfo=txtfile


