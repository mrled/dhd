@echo off

rem - To use this you must set the AutoRun registry key:
rem - reg add "HKCU\Software\Microsoft\Command Processor" /v AutoRun /d "%HOMEDRIVE%%HOMEPATH%\.dhd\hbase\autorun.bat"

doskey omg=echo wtf $*
