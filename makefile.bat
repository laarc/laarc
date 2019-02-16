rd /s /q src\bcrypt\build
mkdir src\bcrypt\build
pushd src\bcrypt\build
cmake ..
devenv bcrypt.sln /rebuild Release 
copy Release\bcrypt.dll .\bcrypt.dll
popd
