unit clHotKey;
{
--------------------------------------------------------------------------
Copyright (c) 2000-2007, Chris Morris
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

  Based on:
    Global Hotkey class by Martin Pelletier
    You have the rights to use & modify this class for your
    own needs. All I ask is, if you make a modification to this
    class, please send me a copy by e-mail at pemartin@videotron.ca

  cLabs Modifications
  ===================
  - Refactored hkRegisterHotKey and hkUnregisterHotKey out of SetHotkey
  - Changed Hotkey from string to TShortCut, rewrote hkRegisterHotKey
    to accommodate.
  - FIX: MsgWndProc not handing unhandled messages off to DefWndProc. Without
    this code, any app using this component would not shutdown with OS shutdown.
  - Added VirtualKey & Shift properties to allow Windows hot key combinations.
    Ideal for use with clHotKeyEdit control
}
interface

uses Windows, Classes, Messages, Forms, Sysutils, Menus;

type
  EclHotKeyException = class(Exception);

  TclHotKey = class(TComponent)
  private
    FRegistered : Boolean;
    FID : Integer;
    FHotkey: TShortCut;
    FOnHotkey: TNotifyEvent;
    FMsgWindow: HWnd;
    FVirtualKey: Word;
    FShift: TShiftState;
    FActive: boolean;
    procedure SetHotkey(const Value: TShortCut);
    procedure SetOnHotkey(const Value: TNotifyEvent);
    procedure SetShift(const Value: TShiftState);
    procedure SetVirtualKey(const Value: Word);
    procedure SetActive(const Value: boolean);
  protected
    procedure hkRegisterHotKey; virtual;
    procedure hkUnregisterHotKey;
    procedure MsgWndProc( var Msg: TMessage );
    procedure RegisterHotKeyIfActive;
  public
    constructor Create(AOwner : TComponent); overload; override;
    constructor Create(AOwner : TComponent; AHotkey : TShortCut); reintroduce; overload;
    destructor Destroy; override;

    property Shift: TShiftState read FShift write SetShift;
    property VirtualKey: Word read FVirtualKey write SetVirtualKey;
  published
    property Active: boolean read FActive write SetActive;
    property Hotkey: TShortCut read FHotkey write SetHotkey;
    property OnHotkey: TNotifyEvent read FOnHotkey write SetOnHotkey;
  end;

procedure Register;

implementation

uses Dialogs;

procedure Register;
begin
  RegisterComponents('cLabs', [TclHotKey]);
end;

{ TclHotKey }

constructor TclHotKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRegistered := False;
  FID := 100; // Hotkey ID see RegisterHotKey API for reference
  FMsgWindow := AllocateHWnd(MsgWndProc);
end;

constructor TclHotKey.Create(AOwner: TComponent; AHotkey: TShortCut);
begin
  inherited Create(AOwner);
  FRegistered := False;
  FID := 100; // Hotkey ID see RegisterHotKey API for reference
  FMsgWindow := AllocateHWnd(MsgWndProc);
  SetHotKey(AHotkey);
end;

destructor TclHotKey.Destroy;
begin
  hkUnregisterHotKey;
  DeallocateHWnd(FMsgWindow);

  inherited Destroy;
end;

procedure TclHotKey.MsgWndProc(var Msg: TMessage);
  procedure Default;
  begin
    with Msg do
      Result := DefWindowProc(FMsgWindow, Msg, WParam, LParam);
  end;
begin
  with Msg do
  begin
    if (Msg = WM_HOTKEY) and (WParam = FID) then
    begin
      if Assigned(FOnHotKey) then
        FOnHotKey(Self);
    end
    else
      Default;
  end;
  Dispatch(Msg);
end;

procedure TclHotKey.hkRegisterHotKey;
var
  Modifiers: Cardinal;
begin
  hkUnregisterHotKey;

  Modifiers := 0;

  { MOD_ALT = 1;
    MOD_CONTROL = 2;
    MOD_SHIFT = 4; }

  if ssShift in FShift then
    Modifiers := Modifiers or MOD_SHIFT;
  if ssCtrl in FShift then
    Modifiers := Modifiers or MOD_CONTROL;
  if ssAlt in FShift then
    Modifiers := Modifiers or MOD_ALT;

  if not (csDesigning in ComponentState) then
  begin
    if RegisterHotKey(FMsgWindow, FID, Modifiers, FVirtualKey) then
      FRegistered := True
    else
      raise EclHotKeyException.Create('Internal error: cannot create hotkey ' +
        '[TclHotKey.hkRegisterHotKey]');
  end;
end;

procedure TclHotKey.hkUnregisterHotKey;
begin
  if FRegistered then
  begin
    UnregisterHotKey(FMsgWindow, FID);
    FRegistered := False;
  end;
end;

procedure TclHotKey.RegisterHotKeyIfActive;
begin
  if FActive then
    hkRegisterHotKey;
end;

procedure TclHotKey.SetActive(const Value: boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
      hkRegisterHotKey
    else
      hkUnregisterHotKey;
  end;
end;

procedure TclHotKey.SetHotkey(const Value: TShortCut);
begin
  FHotkey := Value;
  ShortCutToKey(FHotkey, FVirtualKey, FShift);

  RegisterHotKeyIfActive;
end;

procedure TclHotKey.SetOnHotkey(const Value: TNotifyEvent);
begin
  FOnHotkey := Value;
end;

procedure TclHotKey.SetShift(const Value: TShiftState);
begin
  FShift := Value;
  FHotkey := scNone;
  RegisterHotKeyIfActive;
end;

procedure TclHotKey.SetVirtualKey(const Value: Word);
begin
  FVirtualKey := Value;
  FHotkey := scNone;
  RegisterHotKeyIfActive;
end;

end.

