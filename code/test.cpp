#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

struct projectile
{
  // These are the members, or fields, of this structure

  char unsigned IsThisOnFire;
  int Damage;
  int ParticlesPerSecond;
  short HowManyCooks;
};


int CALLBACK WinMain(
    HINSTANCE hInstance, 
    HINSTANCE hPrevInstance, 
    LPSTR lpCmdLine, 
    int nCmdshow)
{
  AllocConsole();

  FILE* fp;
  freopen_s(&fp, "CONOUT$", "w", stdout);

  projectile Test;

  int SizeOfChar = sizeof(char unsigned);
  int SizeofInt = sizeof(int);
  int SizeOfProjectile = sizeof(projectile);
  int SizeOfTest = sizeof(Test);

  Test.IsThisOnFire = 1;
  Test.Damage = 3249839 + Test.IsThisOnFire;
  Test.ParticlesPerSecond = 23490830;
  Test.HowManyCooks = 50;

  return(0);
}
