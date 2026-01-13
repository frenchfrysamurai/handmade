#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

int CALLBACK WinMain(
    HINSTANCE hInstance, 
    HINSTANCE hPrevInstance, 
    LPSTR lpCmdLine, 
    int nCmdshow)
{
  AllocConsole();

  FILE* fp;
  freopen_s(&fp, "CONOUT$", "w", stdout);

  char unsigned Test;
  char unsigned *TestPointer;

  TestPointer = &Test;

  Test = 255;
  Test = Test + 1;


  MessageBoxA(0, "Hello Handmade World!", "Handmade", MB_ICONINFORMATION | MB_OK); 



  return(0);
}
