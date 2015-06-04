@ECHO OFF
CD "%~dp0"
SET racketdir=%~dp0Racket

@REM Try to delete the file only if it exists
IF EXIST "%~dp0dist\windows" rmdir /s /q "%~dp0dist\windows"
@REM If the file wasn't deleted for some reason, stop and error
IF EXIST "%~dp0dist\windows" exit 1
ECHO "Compiling Client..."
"%racketdir%\raco.exe" exe "%~dp0src\argo.rkt"
ECHO "Compiling Server..."
"%racketdir%\raco.exe" exe "%~dp0src\argoserver.rkt"
ECHO "Creating distro..."
mkdir "%~dp0dist\windows"
"%racketdir%\raco.exe" distribute "%~dp0dist\windows" "%~dp0src\argo.exe" "%~dp0src\argoserver.exe" 
del /F "%~dp0src\argo.exe" 
del /F "%~dp0src\argoserver.exe"
ECHO "Zipping package..."
"%~dp0Racket\Racket.exe" "compress.rkt"
ECHO "Finished."
