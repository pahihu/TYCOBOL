set Path=C:\Windows\System32;C:\Windows;C:\Windows\System32\Wbem
set GC33DIR=C:\Apps\OpenCobolIDE\gc33bdbCH
set Path=%GC33DIR%\bin;C:\Apps\Git\usr\bin;%Path%
call %GC33DIR%\set_env.cmd
set MYDIR=%USERPROFILE%\Documents\github\TYCOBOL
set COB_LIBRARY_PATH=%MYDIR%\LOADLIB;%COB_LIBRARY_PATH%
set SO=.dll
start "GnuCOBOL V3R3CH + Berkeley DB" /d "%MYDIR%"