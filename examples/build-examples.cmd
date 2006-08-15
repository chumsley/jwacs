@echo off

if not exist ..\bin\jwacs.exe echo Can't find jwacs.exe!
if not exist ..\bin\jwacs.exe echo You can build the examples by evaluating
if not exist ..\bin\jwacs.exe echo     (asdf:oos 'asdf:load-op :jwacs-tests)
if not exist ..\bin\jwacs.exe echo     (jw-tests::compile-examples)         
if not exist ..\bin\jwacs.exe echo from the REPL.
if not exist ..\bin\jwacs.exe goto end

..\bin\jwacs.exe --noinform -p /lib/=..\lib CalendarMark2.jw
..\bin\jwacs.exe --noinform Counter.jw
..\bin\jwacs.exe --noinform TrivialHttpRequest.jw

:end