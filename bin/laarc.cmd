@echo off
setlocal

set home=%~dp0

cd "%home%"
cd ..

set DEV=1
racket -t ln.scm %*
