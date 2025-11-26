@echo off
cd /D "%~d0%~p0"
IF %ERRORLEVEL%==0 GOTO PATH_IS_OK
exit
:PATH_IS_OK

copy SyncthingLazuriteConfig.ini %temp%\tmp\SyncthingLazurite\
