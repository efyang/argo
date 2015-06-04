@ECHO OFF
CD "%~dp0"

SET racketdir=%~dp0Racket

@REM Try to delete the file only if it exists
IF EXIST "%~dp0dist\windows" rmdir /s /q "%~dp0dist\windows"
@REM If the file wasn't deleted for some reason, stop and error
IF EXIST "%~dp0dist\windows" exit 1
REM ECHO "Running Tests..."
REM "%racketdir%\Racket.exe" "%~dp0src\tests.rkt"
REM IF NOT ERRORLEVEL 0 GOTO testfail
ECHO "Generating Files..."
CD src
python "parse.py"
CD ..
ECHO "Compiling P1..."
"%racketdir%\raco.exe" exe "%~dp0src\jeopardyp1.rkt"
ECHO "Compiling P2..."
"%racketdir%\raco.exe" exe "%~dp0src\jeopardyp2.rkt"
ECHO "Compiling Server..."
"%racketdir%\raco.exe" exe "%~dp0src\server.rkt"
ECHO "Creating distro..."
mkdir "%~dp0dist\windows"
"%racketdir%\raco.exe" distribute "%~dp0dist\windows" "%~dp0src\jeopardyp1.exe" "%~dp0src\jeopardyp2.exe" "%~dp0src\server.exe"
del /F "%~dp0src\jeopardyp1.exe" 
del /F "%~dp0src\jeopardyp2.exe" 
del /F "%~dp0src\server.exe"
ECHO "Zipping package..."
"%~dp0Racket\Racket.exe" "compress.rkt"
REM echo "Done. Running program."
REM cd "%~dp0dist\windows"
REM "%~dp0dist\windows\main.exe"
REM cd ../..
REM :testfail
REM ECHO "Tests Failed."
REM exit /b 255
ECHO "Finished."
