@echo off

for %%F in ("%0") do set SOURCE_DIR=%%~dpF

copy "%SOURCE_DIR%.emacs" "%APPDATA%\.emacs"

if not exist "%APPDATA%\.emacs.rc" ( mkdir "%APPDATA%\.emacs.rc" )
copy "%SOURCE_DIR%.emacs.rc\*.*" "%APPDATA%\.emacs.rc"