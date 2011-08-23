@ECHO OFF

rem http://www.robvanderwoude.com/unixports.php

:: Check Windows version
IF NOT "%OS%"=="Windows_NT" GOTO Syntax

:: Localize variables
SETLOCAL ENABLEDELAYEDEXPANSION

:: Check NUMBER of command line arguments
IF     "%~1"=="" GOTO Syntax
IF NOT "%~4"=="" GOTO Syntax

:: Initialize variables
SET Param.C=
SET Param.D=
SET Param.F=
SET Param.L=
SET Error=0

:: Parse command line arguments
FOR %%A IN (%*) DO (
	ECHO.%%A | FINDSTR /R /B /I /C:"-[CDFL]:.*$" >NUL
	IF NOT ERRORLEVEL 1 (
		FOR /F "tokens=1* delims=-:" %%B IN ("%%~A") DO (
			SET Param.%%B=%%~C
			IF /I "%%~B"=="D" (
				IF NOT "!Param.D:~0,1!"=="!Param.D!" SET Error=1
			) ELSE (
				SET /A ParamTest = Param.%%B
				IF !Paramtest! EQU   0 SET Error=1
				IF !ParamTest! NEQ %%C SET Error=1
			)
		)
	)
)
IF %Error% EQU 1 GOTO Syntax

:: Either -C or -F must be used, one and only one
IF NOT DEFINED Param.C IF NOT DEFINED Param.F GOTO Syntax
IF     DEFINED Param.C IF     DEFINED Param.F GOTO Syntax

:: If -C was specified
IF DEFINED Param.C (
	SET /A StartPos = Param.C - 1
	IF DEFINED Param.L (SET Length=,%Param.L%) ELSE (SET Length=)
	FOR /F "tokens=* delims=" %%A IN ('MORE') DO (
		SET Line=%%A
		rem CALL ECHO !Line:~%StartPos%%Length%!
		CALL :DisplaySubstr "!StartPos!" "!Length!"
	)
	ENDLOCAL
	GOTO:EOF
)

:: If not -C then -F must have been specified
IF DEFINED Param.L (
	SET /A LastPos  = %Param.F% + %Param.L% - 1
) ELSE (
	SET /A StartPos = %Param.F% - 1
)
IF DEFINED Param.D (
	SET Delims= delims=%Param.D%
) ELSE (
	SET Delims=
)
IF DEFINED Param.L (
	FOR /F "tokens=* delims=" %%A IN ('MORE') DO (
		SET Line=
		FOR /L %%B IN (%Param.F%,1,%LastPos%) DO (
			CALL :AddWord "%%~B" "%Delims%" "%%~A"
		)
		IF NOT "!Line!"=="" ECHO !Line:~1!
	)
) ELSE (
	FOR /F "tokens=%StartPos%*%Delims%" %%A IN ('MORE') DO (
		ECHO.%%~B
	)
)

ENDLOCAL
GOTO:EOF


:AddWord
FOR /F "tokens=%~1%~2" %%C IN ("%~3") DO (SET Line=!Line! %%C)
GOTO:EOF


:DisplaySubstr
CALL ECHO.!Line:^~%~1%~2!
GOTO:EOF


:Syntax
ECHO.
ECHO Cut.bat,  Version 1.00 for Windows 2000 and later
ECHO Attempt to "port" the Unix CUT command to batch
ECHO.
:: Separate help for DOS and NT because in DOS pipe symbols cannot be
:: escaped and hence are emulated using extended ASCII character 179
IF NOT "%OS%"=="Windows_NT" ECHO Usage:  command  ³  CUT  -C:n   [ -L:n ]
IF NOT "%OS%"=="Windows_NT" ECHO    or:  command  ³  CUT  -F:n   [ -L:n ]  [ -D:"chr" ]
IF NOT "%OS%"=="Windows_NT" GOTO SkipPipes
ECHO Usage:  command  ^|  CUT  -C:n   [ -L:n ]
ECHO    or:  command  ^|  CUT  -F:n   [ -L:n ]  [ -D:"chr" ]
:SkipPipes
ECHO.
ECHO Where:  command  is a command whose standard output is piped to this script
ECHO         -C:n     cuts the standard input at the Nth "column" (character)
ECHO         -F:n     cuts the standard input at the Nth "field"  (word)
ECHO         -D:"chr" specifies delimiter for -F  (default: space)
ECHO         -L:n     maximum N columns or fields (default: all till end of line)
ECHO.
ECHO Notes:  Empty lines in the standard input as well as in the output are skipped.
ECHO         With -F and -L specified, multiple consecutive delimiters are treated
ECHO         as a single delimiter in the output.
ECHO         Delimiter specified with -D is case insensitive.
ECHO         When nothing is piped to this script's standard input, the keyboard is
ECHO         read until the stream is closed by pressing either Ctrl+Z and Enter or
ECHO         F6 and Enter.
ECHO.
ECHO Written by Rob van der Woude
ECHO http://www.robvanderwoude.com

IF "%OS%"=="Windows_NT" ENDLOCAL

:: Exit with ErrorLevel 1, even in "true" DOS
ECHO A | FIND "B"