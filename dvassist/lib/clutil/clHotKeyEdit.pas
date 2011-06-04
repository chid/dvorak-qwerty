unit clHotKeyEdit;
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
}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TclHotKeyEdit = class(TCustomEdit)
  private
    FVirtualKey: Word;
    FShift: TShiftState;
    procedure SetShift(const Value: TShiftState);
    procedure SetVirtualKey(const Value: Word);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function ShiftCount(Shift: TShiftState): integer;
    procedure UpdateText;
  public
    property Shift: TShiftState read FShift write SetShift;
    property VirtualKey: Word read FVirtualKey write SetVirtualKey;
  published
    property Text;
  end;

procedure Register;

implementation

uses Menus;

procedure Register;
begin
  RegisterComponents('cLabs', [TclHotKeyEdit]);
end;

{ TclHotKeyEdit }

procedure TclHotKeyEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FVirtualKey := Key;
  FShift := Shift;

  if (ShiftCount(Shift) <= 1) and ((Key < VK_F1) or (Key > VK_F12)) then
    FShift := [ssCtrl, ssAlt];
    
  Key := 0;
  UpdateText;
end;

procedure TclHotKeyEdit.KeyPress(var Key: Char);
begin
  { this is necessary to keep the keystroke from being processed. }
  Key := #0;
end;

procedure TclHotKeyEdit.SetShift(const Value: TShiftState);
begin
  FShift := Value;
  UpdateText;
end;

procedure TclHotKeyEdit.SetVirtualKey(const Value: Word);
begin
  FVirtualKey := Value;
  UpdateText;
end;

function TclHotKeyEdit.ShiftCount(Shift: TShiftState): integer;
begin
  Result := 0;

  if ssCtrl in Shift then
    Inc(Result);

  if ssShift in Shift then
    Inc(Result);

  if ssAlt in Shift then
    Inc(Result);
end;

procedure TclHotKeyEdit.UpdateText;
var
  NewText: string;
begin
  NewText := '';

  if ssCtrl in Shift then
    NewText := 'Ctrl + ';

  if ssShift in Shift then
    NewText := NewText + 'Shift + ';

  if ssAlt in Shift then
    NewText := NewText + 'Alt + ';

  NewText := NewText + Menus.ShortCutToText(TShortCut(FVirtualKey));
  Text := NewText;
end;

end.
