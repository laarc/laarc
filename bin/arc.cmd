@echo off
setlocal

set home=%~dp0

cd "%home%"
cd ..

racket -t as.scm %*

