@echo off

rem assoc tells you which file extensions are associated w/ which filetypes
rem ftype tells you which filetypes are associated w/ which programs

set emacsdir=C:\opt\ntemacs24\bin
assoc .el=EmacsFile
assoc .mdwn=txtfile
assoc .nfo=txtfile
ftype EmacsFile="%emacsdir%\emacsclientw.exe" -n -a "%emacsdir%\runemacs.exe" "%%1"
ftype txtfile="%emacsdir%\emacsclientw.exe" -n -a "%emacsdir%\runemacs.exe" "%%1"
ftype inifile="%emacsdir%\emacsclientw.exe" -n -a "%emacsdir%\runemacs.exe" "%%1"

set pythondir=C:\Python32
assoc .py=Python.File
ftype Python.File=%pythondir%\python.exe "%1" %*

pause
