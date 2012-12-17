@echo off

rem assoc tells you which file extensions are associated w/ which filetypes
rem ftype tells you which filetypes are associated w/ which programs

assoc .el=txtfile
assoc .nfo=txtfile

assoc .mdwn=txtfile
assoc .markdown=txtfile
assoc .md=txtfile
assoc .text=txtfile

rem I'm changing my mind about this. use notepad2 for dumb quick editing, and only use Emacs when I open it myself
::ftype EmacsFile="%emacsdir%\emacsclientw.exe" -n -a "%emacsdir%\runemacs.exe" "%%1"
::ftype txtfile="%emacsdir%\emacsclientw.exe" -n -a "%emacsdir%\runemacs.exe" "%%1"
::ftype inifile="%emacsdir%\emacsclientw.exe" -n -a "%emacsdir%\runemacs.exe" "%%1"

rem This is a dumb bitch to maintain since it changes with every python version, but
rem it lets you run python scripts like 'script.py' without calling it through python
rem like 'pythonw.exe script.py'. 
::set pythondir=C:\Python32
::assoc .py=Python.File
::ftype Python.File=%pythondir%\python.exe "%1" %*
::setx PATHEXT %PATHEXT%;".PY"

::pause
