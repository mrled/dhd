reg Query "HKLM\Hardware\Description\System\CentralProcessor\0" | find /i "x86" > NUL && set OSARCHITECTURE=32BIT || set OSARCHITECTURE=64BIT
set SZFILENAME=7z920.msi
if %OSARCHITECTURE%==64BIT set SZFILENAME=7z920-x64.msi
set SZURL=http://7-zip.org/a/%SZFILENAME%
set SZDLPATH=C:\Windows\Temp\%SZFILENAME%
powershell -Command "(New-Object System.Net.WebClient).DownloadFile('%SZURL%', '%SZDLPATH%')" <NUL
echo msiexec /qb /i %SZDLPATH%
