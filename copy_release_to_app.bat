@echo off
cd /D "%~d0%~p0"
IF %ERRORLEVEL%==0 GOTO PATH_IS_OK
exit
:PATH_IS_OK

set TARGET_DIR=D:\heXor\App\INet_File\syncthing\
copy SyncthingLazurite.exe %TARGET_DIR%
xcopy languages %TARGET_DIR%languages /E /I /Y

