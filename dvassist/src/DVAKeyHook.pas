(* written by Joe C. Hecht *)
(* tweaked for DVAssist by Chrismo *)
unit DVAKeyHook;
{
--------------------------------------------------------------------------
Copyright (c) 2000, Chris Morris 
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this 
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, 
this list of conditions and the following disclaimer in the documentation and/or 
other materials provided with the distribution.

3. Neither the names Chris Morris, cLabs nor the names of contributors to this 
software may be used to endorse or promote products derived from this software 
without specific prior written permission. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' 
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE 
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR 
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
-------------------------------------------------------------------------------- 
(based on BSD Open Source License) 
}
interface

uses Windows;

{Functions prototypes for the hook dll}
type TGetHookRecPointer = function : pointer stdcall;

type TStartKeyBoardHook = procedure stdcall;

type TStopKeyBoardHook = procedure stdcall;

{The record type filled in by the hook dll}
type THookRec = packed record
  TheHookHandle : HHOOK;
  TheAppWinHandle : HWND;
  TheCtrlWinHandle : HWND;
  TheKeyCount : DWORD;
end;

{A pointer type to the hook record}
type PHookRec = ^THookRec;

var
  hHookLib : THANDLE; {A handle to the hook dll}
  GetHookRecPointer : TGetHookRecPointer; {Function pointer}
  StartKeyBoardHook : TStartKeyBoardHook; {Function pointer}
  StopKeyBoardHook : TStopKeyBoardHook; {Function pointer}
  LibLoadSuccess : bool; {If the hook lib was successfully loaded}
  lpHookRec : PHookRec; {A pointer to the hook record}

procedure EnableKeyboardHook;
procedure DisableKeyboardHook;

implementation

procedure EnableKeyboardHook;
begin
 {Set our initial variables}
  lpHookRec := NIL;
  LibLoadSuccess := FALSE;
  @GetHookRecPointer := NIL;
  @StartKeyBoardHook := NIL;
  @StopKeyBoardHook := NIL;
 {Try to load the hook dll}
  hHookLib := LoadLibrary('DVAHOOK.DLL');
 {If the hook dll was loaded successfully}
  if hHookLib <> 0 then begin
   {Get the function addresses}
    @GetHookRecPointer :=
      GetProcAddress(hHookLib, 'GETHOOKRECPOINTER');
    @StartKeyBoardHook :=
      GetProcAddress(hHookLib, 'STARTKEYBOARDHOOK');
    @StopKeyBoardHook :=
      GetProcAddress(hHookLib, 'STOPKEYBOARDHOOK');
   {Did we find all the functions we need?}
    if ((@GetHookRecPointer <> NIL) AND
        (@StartKeyBoardHook <> NIL) AND
        (@StopKeyBoardHook <> NIL)) then begin
       LibLoadSuccess := TRUE;
      {Get a pointer to the hook record}
       lpHookRec := GetHookRecPointer;
      {Were we successfull in getting a ponter to the hook record}
       if (lpHookRec <> nil) then begin
        {Fill in our portion of the hook record}
         lpHookRec^.TheHookHandle := 0;
         //lpHookRec^.TheCtrlWinHandle := Button1.Handle;
         lpHookRec^.TheKeyCount := 0;
        {Start the keyboard hook}
         StartKeyBoardHook;
        {Start the timer if the hook was successfully set
         if (lpHookRec^.TheHookHandle <> 0) then begin
           Timer1.Enabled := TRUE;                       }
         end;
       end;
    end else begin
     {We failed to find all the functions we need}
      FreeLibrary(hHookLib);
      hHookLib := 0;
      @GetHookRecPointer := NIL;
      @StartKeyBoardHook := NIL;
      @StopKeyBoardHook := NIL;
    end;
  end;

procedure DisableKeyboardHook;
begin
 {Did we load the dll successfully?}
  if (LibLoadSuccess = TRUE) then begin
   {Did we sucessfully get a pointer to the hook record?}
    if (lpHookRec <> nil) then begin
     {Did the hook get set?}
      if (lpHookRec^.TheHookHandle <> 0) then begin
        //Timer1.Enabled := FALSE;
        StopKeyBoardHook;
      end;
    end;
   {Free the hook dll}
    FreeLibrary(hHookLib);
  end;
end;


end.
