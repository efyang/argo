@ECHO OFF
CD "%~dp0"
SET racketdir=%~dp0Racket

IF EXIST "%~dp0dist\windows" rmdir /s /q "%~dp0dist\windows"
IF EXIST "%~dp0dist\windows" exit 1
IF EXIST "%~dp0release\Argo.zip" del "%~dp0release\Argo.zip"
IF EXIST "%~dp0release\Argo.zip" exit 1
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
