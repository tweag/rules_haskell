REM This Windows script dumps the short path of its first argument
REM into the file specified by its second argument.
REM
REM Example:
REM .\short_path.bat "C:\Program Files" foo
REM cat foo => C:\PROGRA~1

@ECHO OFF
echo | set /p dummy="%~s1" > %2

