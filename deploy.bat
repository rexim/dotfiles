@echo off
set SCRIPT_DIR=%~dp0

mklink %APPDATA%\.emacs %SCRIPT_DIR%emacs.init
mklink /D %APPDATA%\.emacs.rc %SCRIPT_DIR%emacs.rc
mklink /D %APPDATA%\.emacs.snippets %SCRIPT_DIR%emacs.snippets
