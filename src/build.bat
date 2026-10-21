@echo off

mkdir ..\build
pushd ..\build
cl -Zi -Od \handmade\code\handmade.cpp user32.lib
popd


