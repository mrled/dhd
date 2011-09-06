@echo off

rem - To use this you must set the AutoRun registry key:
rem - reg add "HKCU\Software\Microsoft\Command Processor" /v AutoRun /d "%USERPROFILE%\.dhd\hbase\autorun.bat"

doskey omg=echo wtf $*
doskey wh=which $* /a
doskey sourceautorun=%USERPROFILE%\.dhd\hbase\autorun.bat
doskey sourceautoruns=%USERPROFILE%\.dhd\hbase\autorun.bat
doskey markdown=C:\strawberry\perl\bin\perl.exe %USERPROFILE%\.dhd\opt\bin\Markdown.pl