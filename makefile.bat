rd /s /q src\bcrypt\build
mkdir src\bcrypt\build
pushd src\bcrypt\build
cmake .. -DCMAKE_GENERATOR_PLATFORM=x64
devenv bcrypt.sln /rebuild Release 
copy Release\bcrypt.dll .\bcrypt.dll
popd
