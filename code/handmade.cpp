#include <windows.h>

#define internal static
#define local_persist static
#define global_variable static

global_variable bool Running;

internal void
Win32ResizeDIBSection(int Width, int Height)
{
      HBITMAP CreateDIBSection(
	  [in]  HDC              hdc,
	  [in]  const BITMAPINFO *pbmi,
	  [in]  UINT             usage,
	  [out] VOID             **ppvBits,
	  [in]  HANDLE           hSection,
	  [in]  DWORD            offset
	);
}

internal void
Wind32UpdateWindow(HDC DeviceContext, int X, int Y, int Width, int Height)
{
      int StretchDIBits(
	  [in] HDC              hdc,
	  [in] int              xDest,
	  [in] int              yDest,
	  [in] int              DestWidth,
	  [in] int              DestHeight,
	  [in] int              xSrc,
	  [in] int              ySrc,
	  [in] int              SrcWidth,
	  [in] int              SrcHeight,
	  [in] const VOID       *lpBits,
	  [in] const BITMAPINFO *lpbmi,
	  [in] UINT             iUsage,
	  [in] DWORD            rop
	);
}

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
	  RECT ClientRect(Window, &ClientRect);
	  GetClientRect(Window, &ClientRect);
	  int Width = ClientRect.right -ClientRect.left;
	  int Height = ClientRect.bottom - ClientRect.top;
	  ResizeDIBSection(Width, Height);
	  OutputDebugStringA("WM_SIZE\N");
        } break;

        case WM_DESTROY:
        {
	  Running = false;
        }break;

        case WM_CLOSE:
        {
	  Running = false;
        }break;

        case WM_ACTIVATEAPP:
        {
            OutputDebugStringA("WM_ACTIVATEAPP\n");
        }break;

	// Day 2 56:10 .... Very useful info on how to solve linking errors. Stopping here
	case WM_PAINT:
	{
	  PAINTSTRUCT Paint;
	  HDC DeviceContext = BeginPaint(Window, &Paint);
	  int X = Paint.rcPaint.left;
	  int Y = Paint.rcPaint.top;
	  int Width = Paint.rcPaint.right - Paint.rcPaint.left;
	  int Height = Paint.rcPaint.bottom - Paint.rcPaint.top;
	  Win32UpdateWindow(Window, X, Y, Width, Height);
	  EndPaint(Window, &Paint);
	}break;
	

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
	 Running = true;
         while(Running)
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
