(* written by Joe C. Hecht *)
(* tweaked for DVAssist by Chrismo *)
library DVAHook;

uses
  Windows,
  Messages,
  SysUtils;

{Define a record for recording and passing information process wide}  
type
  PHookRec = ^THookRec;
  THookRec = packed record
    TheHookHandle : HHOOK;
    TheAppWinHandle : HWND;
    TheCtrlWinHandle : HWND;
    TheKeyCount : DWORD;
  end;

var
  hObjHandle : THandle; {Variable for the file mapping object}
  lpHookRec : PHookRec; {Pointer to our hook record}


procedure MapFileMemory(dwAllocSize : DWORD);
begin
 {Create a process wide memory mapped variable}
  hObjHandle := CreateFileMapping($FFFFFFFF,
                                  NIL,
                                  PAGE_READWRITE,
                                  0,
                                  dwAllocSize,
                                  'HookRecMemBlock');
   if (hObjHandle = 0) then begin
     MessageBox(0,
                'Hook DLL',
                'Could not create file map object',
                MB_OK);
     exit;
   end;
 {Get a pointer to our process wide memory mapped variable}
  lpHookRec := MapViewOfFile(hObjHandle,
                             FILE_MAP_WRITE,
                             0,
                             0,
                             dwAllocSize);
  if (lpHookRec = NIL) then begin
    CloseHandle(hObjHandle);
    MessageBox(0,
               'Hook DLL',
               'Could not map file',
               MB_OK);
    exit;
  end;
end;


procedure UnMapFileMemory;
begin
 {Delete our process wide memory mapped variable}
  if (lpHookRec <> NIL) then begin
    UnMapViewOfFile(lpHookRec);
    lpHookRec := NIL;
  end;
  if (hObjHandle > 0) then begin
    CloseHandle(hObjHandle);
    hObjHandle := 0;
  end;
end;


function GetHookRecPointer : pointer stdcall;
begin
 {Return a pointer to our process wide memory mapped variable}
  result := lpHookRec;
end;


{The function that actually processes the keystrokes for our hook}
function KeyBoardProc(Code : integer;
                      wParam : integer;
                      lParam : integer): integer; stdcall;
var
  KeyUp : bool;
 {Remove comments for additional functionability
  IsAltPressed : bool;
  IsCtrlPressed : bool;
  IsShiftPressed : bool;
 }
begin
  result := 0;

  case Code of
    HC_ACTION : begin
     {We trap the keystrokes here}

     {Is this a key up message?}
      KeyUp := ((lParam AND (1 shl 31)) <> 0);

    (*Remove comments for additional functionability
     {Is the Alt key pressed}
      if ((lParam AND (1 shl 29)) <> 0) then begin
        IsAltPressed := TRUE;
      end else begin
        IsAltPressed := FALSE;
      end;

     {Is the Control key pressed}
      if ((GetKeyState(VK_CONTROL) AND (1 shl 15)) <> 0) then begin
        IsCtrlPressed := TRUE;
      end else begin
        IsCtrlPressed := FALSE;
      end;

     {if the Shift key pressed}
      if ((GetKeyState(VK_SHIFT) AND (1 shl 15)) <> 0) then begin
        IsShiftPressed := TRUE;
      end else begin
        IsShiftPressed := FALSE;
      end;
     *)

     {If KeyUp then increment the key count}
      if (KeyUp <> FALSE) then begin
        Inc(lpHookRec^.TheKeyCount);
      end;

      case wParam of

       {Was the enter key pressed?}
        VK_RETURN : begin
          {if KeyUp}
           if (KeyUp <> FALSE) then begin
            {Post a bogus message to the window control in our app}
             PostMessage(lpHookRec^.TheCtrlWinHandle,
                         WM_KEYDOWN,
                         0,
                         0);
             PostMessage(lpHookRec^.TheCtrlWinHandle,
                         WM_KEYUP,
                         0,
                         0);
           end;
          {If you wanted to swallow the keystroke then return -1}
          {else if you want to allow the keystroke then return 0}
           result := 0;
           exit;
         end; {VK_RETURN}

       {If the left arrow key is pressed then lets play a joke!}
        VK_LEFT : begin
          {if KeyUp}
           if (KeyUp <> FALSE) then begin
            {Create a UpArrow keyboard event}
             keybd_event(VK_RIGHT, 0, 0, 0);
             keybd_event(VK_RIGHT, 0, KEYEVENTF_KEYUP, 0);
           end;
          {Swallow the keystroke}
           result := -1;
           exit;
         end; {VK_LEFT}

      end; {case wParam}
     {Allow the keystroke}
      result := 0;
    end; {HC_ACTION}
    HC_NOREMOVE : begin
      {This is a keystroke message, but the keystroke message}
      {has not been removed from the message queue, since an}
      {application has called PeekMessage() specifying PM_NOREMOVE}
      result := 0;
      exit;
    end;
  end; {case code}
  if (Code < 0) then
   {Call the next hook in the hook chain}
    result :=
      CallNextHookEx(lpHookRec^.TheHookHandle,
                     Code,
                     wParam,
                     lParam);
end;

procedure StartKeyBoardHook stdcall;
begin
 {If we have a process wide memory variable}
 {and the hook has not already been set...}
  if ((lpHookRec <> NIL) AND
      (lpHookRec^.TheHookHandle = 0)) then begin
   {Set the hook and remember our hook handle}
    lpHookRec^.TheHookHandle := SetWindowsHookEx(WH_KEYBOARD,
                                                 @KeyBoardProc,
                                                 hInstance,
                                                 0);
  end;
end;


procedure StopKeyBoardHook stdcall;
begin
 {If we have a process wide memory variable}
 {and the hook has already been set...}
  if ((lpHookRec <> NIL) AND
      (lpHookRec^.TheHookHandle <> 0)) then begin
   {Remove our hook and clear our hook handle}
    if (UnHookWindowsHookEx(lpHookRec^.TheHookHandle) <> FALSE) then
begin
      lpHookRec^.TheHookHandle := 0;
    end;
  end;
end;


procedure DllEntryPoint(dwReason : DWORD);
begin
  case dwReason of
    Dll_Process_Attach : begin
     {If we are getting mapped into a process, then get}
     {a pointer to our process wide memory mapped variable}
      hObjHandle := 0;
      lpHookRec := NIL;
      MapFileMemory(sizeof(lpHookRec^));
    end;
    Dll_Process_Detach : begin
     {If we are getting unmapped from a process then, remove}
     {the pointer to our process wide memory mapped variable}
      UnMapFileMemory;
    end;
  end;
end;


exports
  KeyBoardProc name 'KEYBOARDPROC',
  GetHookRecPointer name 'GETHOOKRECPOINTER',
  StartKeyBoardHook name 'STARTKEYBOARDHOOK',
  StopKeyBoardHook name 'STOPKEYBOARDHOOK';



begin
 {Set our Dll's main entry point}
  DLLProc := @DllEntryPoint;
 {Call our Dll's main entry point}
  DllEntryPoint(Dll_Process_Attach);
end.

