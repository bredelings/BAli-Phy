@echo off
rem Windows does not interpret Unix shebangs, so run the adjacent Python script explicitly.
where py >nul 2>nul
if errorlevel 1 goto use_python
py -3 "%~dp0bali-phy-pkg.py" %*
exit /b %errorlevel%

:use_python
python "%~dp0bali-phy-pkg.py" %*
exit /b %errorlevel%
