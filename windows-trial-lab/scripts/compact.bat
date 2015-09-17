set TEMPTEMP=C:\PackerTemp
mkdir %TEMPTEMP%

reg Query "HKLM\Hardware\Description\System\CentralProcessor\0" | find /i "x86" > NUL && set OSARCHITECTURE=32BIT || set OSARCHITECTURE=64BIT
echo OSARCHITECTURE == %OSARCHITECTURE%

set UDFARCH=i386
if %OSARCHITECTURE%==64BIT set UDFARCH=amd64
echo UDFARCH == %UDFARCH%
    
set UDFFILENAME=ultradefrag-portable-6.1.0.bin.%UDFARCH%.zip
set UDFURL=http://downloads.sourceforge.net/project/ultradefrag/stable-release/6.1.0/%UDFFILENAME%
set UDFZIP=%TEMPTEMP%\%UDFFILENAME%
set UDFEXE=%TEMPTEMP%\ultradefrag-portable-6.1.0.%UDFARCH%\udefrag.exe

if not exist "%UDFZIP%" (
    powershell -Command "(New-Object System.Net.WebClient).DownloadFile('%UDFURL%', '%UDFZIP%')" <NUL
)
if not exist "%UDFEXE%" (
    cmd /c ""C:\Program Files\7-Zip\7z.exe" x %UDFZIP% -o%TEMPTEMP%"
)

if not exist "%TEMPTEMP%\SDelete.zip" (
  powershell -Command "(New-Object System.Net.WebClient).DownloadFile('http://download.sysinternals.com/files/SDelete.zip', '%TEMPTEMP%\SDelete.zip')" <NUL
)
if not exist "%TEMPTEMP%\sdelete.exe" (
	cmd /c ""C:\Program Files\7-Zip\7z.exe" x %TEMPTEMP%\SDelete.zip -o%TEMPTEMP%"
)

@echo ========================================
@echo ALL THE FILES IN MY TEMP DIRECTORY OF "%TEMPTEMP%"
@dir %TEMPTEMP%
@echo ========================================

net stop wuauserv
rmdir /S /Q C:\Windows\SoftwareDistribution\Download
mkdir C:\Windows\SoftwareDistribution\Download
net start wuauserv

cmd /c %UDFEXE% --optimize --repeat C:

cmd /c %SystemRoot%\System32\reg.exe ADD HKCU\Software\Sysinternals\SDelete /v EulaAccepted /t REG_DWORD /d 1 /f
cmd /c %TEMPTEMP%\sdelete.exe -q -z C:

rmdir /s /q %TEMPTEMP%
