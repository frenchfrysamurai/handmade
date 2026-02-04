#include <windows.h>

LRESULT CALLBACK
MainWindowCallback(HWND Window,
                   UINT Message,
                   WPARAM WParam,
                   LPARAM LParam)
{
    LRESULT Result = 0;

#if 0
    switch(Message)
    {
        case WM_SIZE:
        {
            OutputDebugStringA("WM_SIZE\n");
        } break;

        case WM_DESTROY:
        {
            OutputDebugStringA("WM_DESTROY\n");
        }break;

        case WM_CLOSE:
        {
            OutputDebugStringA("WM_CLOSE\n");
        }break;

        case WM_ACTIVATEAPP:
        {
            OutputDebugStringA("WM_ACTIVATEAPP\n");
        }break;

        default:
        {
            //        OutputDebugStringA("default\n");
            Result = ;
        }break;
    }
#endif

    return(Result);
}

        

int CALLBACK WinMain(
    HINSTANCE Instance, 
    HINSTANCE PrevInstance, 
    LPSTR CommandLine, 
    int Cmdshow)
{
// Stopped at 35:20, Day 2
    WNDCLASS WindowClass = {};

    WindowClass.style = CS_OWNDC|CS_HREDRAW|CS_VREDRAW;
    WindowClass.lpfnWndProc = MainWindowCallback;
    WindowClass.hInstance = Instance;
    //WindowClass.hIcon;
    WindowClass.lpszClassName = "HandmadeHeroWindowClass";


  return(0);
}
