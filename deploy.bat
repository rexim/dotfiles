@echo off
set SCRIPT_DIR=%~dp0

:COPY_FILES

copy "%SCRIPT_DIR%.emacs" "%APPDATA%\.emacs"

if not exist "%APPDATA%\.emacs.rc" ( mkdir "%APPDATA%\.emacs.rc" )
copy "%SCRIPT_DIR%.emacs.rc\*.*" "%APPDATA%\.emacs.rc"

rem FIXME(rexim): deploy .emacs.snippets

:EOF