@echo off

rem - To use this you must set the AutoRun registry key:
rem - reg add "HKCU\Software\Microsoft\Command Processor" /v AutoRun /d "%USERPROFILE%\.dhd\hbase\autorun.bat"

rem doskey omg=echo wtf $*
rem doskey wh=which $* /a
rem doskey sourceautorun=%USERPROFILE%\.dhd\hbase\autorun.bat
rem doskey sourceautoruns=%USERPROFILE%\.dhd\hbase\autorun.bat
rem doskey markdown=C:\strawberry\perl\bin\perl.exe %USERPROFILE%\.dhd\opt\bin\Markdown.pl