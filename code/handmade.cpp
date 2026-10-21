#include <windows.h>

LRESULT CALLBACK
MainWindowCallback(HWND Window,
                   UINT Message,
                   WPARAM WParam,
                   LPARAM LParam)
{
    LRESULT Result = 0;

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

	// Day 2 56:10 .... Very useful info on how to solve linking errors. Stopping here
	case WM_PAINT:
	{
	  PAINTSTRUCT Paint;
	  HDC_DeviceContext = BeginPaint(Window, &Paint);
	  int x = 

	}
	

        default:
        {
            //        OutputDebugStringA("default\n");
	  Result = DefWindowProc(Window, Message, WParam, LParam);
        }break;
    }
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

    if(RegisterClass(&WindowClass))
    {
      HWND Window = CreateWindowExA(0,
                                      WindowClass.lpszClassName,
                                      "Handmade Hero",
                                      WS_OVERLAPPEDWINDOW | WS_VISIBLE,
                                      CW_USEDEFAULT,
                                      CW_USEDEFAULT,
                                      CW_USEDEFAULT,
                                      CW_USEDEFAULT,
                                      0,
                                      0,
                                      Instance,
                                      0);
      if(Window)
       {
	   for(;;)
	     {
	       MSG Message;
	       BOOL MessageResult = GetMessage(&Message, 0, 0, 0);
	       if(MessageResult > 0)
		 {
		   TranslateMessage(&Message);
		   DispatchMessageA(&Message);
		 }
	       else
		 {
		   break;
		 }
	     }
       }
    }
    else
    {
    }
    ;


  return(0);
}
