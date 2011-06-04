unit clKeyboardPanel;
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
  ExtCtrls;

type
  TclKeyboardKey = class(TObject)
  private
    FColor  :     TColor;
    FKeyChar:     string;
    FRow:         integer;
    FSuperChar:   string;
    FWidthRatio:  single;
  public
    property Color:       TColor  read FColor       write FColor;
    property KeyChar:     string  read FKeyChar     write FKeyChar;
    property Row:         integer read FRow         write FRow;
    property SuperChar:   string  read FSuperChar   write FSuperChar;
    property WidthRatio:  single  read FWidthRatio  write FWidthRatio;
  end;

  TclKeyboardLayout = class(TObject)
  private
    FCursor: integer;
    FList: TList;
  protected
    procedure CreateKey(Key, SuperKey: string; Row: integer; WidthRatio: single;
      Color: TColor);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetFirstKey: TclKeyboardKey;
    function GetKeyCount(ARow: integer): integer;
    function GetNextKey: TclKeyboardKey;
    function GetRowCount: integer;
  end;

  TclDvorakLayout = class(TclKeyboardLayout)
  public
    constructor Create; override;
  end;

  TclQwertyLayout = class(TclKeyboardLayout)
  public
    constructor Create; override;
  end;

  TclKeyboardLayoutName = (klQwerty, klDvorak);

  TclKeyboardPanel = class(TCustomPanel)
  private
    FCurrentLayout: TclKeyboardLayout;
    FCurrentLayoutName: TclKeyboardLayoutName;
  protected
    procedure Paint; override;
    procedure PaintKey(AKey: TclKeyboardKey; ABitmap: TBitmap);
    procedure SetCurrentLayoutName(ACurrentLayoutName: TclKeyboardLayoutName);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Color;
    property CurrentLayoutName: TclKeyboardLayoutName read FCurrentLayoutName
                                                      write SetCurrentLayoutName;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Mo', [TclKeyboardPanel]);
end;

// ===== TclKeyboardKey ========================================================
// ===== TclKeyboardKey ========================================================
// ===== TclKeyboardKey ========================================================
// ===== TclKeyboardKey ========================================================

constructor TclKeyboardLayout.Create;
begin
  FList := TList.Create;
end; { Create }

procedure TclKeyboardLayout.CreateKey(Key, SuperKey: string; Row: integer;
  WidthRatio: single; Color: TColor);
var
  AKey: TclKeyboardKey;
begin
  AKey := TclKeyboardKey.Create;
  AKey.KeyChar := Key;
  AKey.SuperChar := SuperKey;
  AKey.WidthRatio := WidthRatio;
  AKey.Row := Row;
  AKey.Color := Color;
  FList.Add(AKey);
end; { CreateKey }

destructor TclKeyboardLayout.Destroy;
begin
  while FList.Count > 0 do
  begin
    TclKeyboardKey(FList[0]).Free;
    FList.Delete(0);
  end;
  FList.Free;
end; { Destroy }

function TclKeyboardLayout.GetFirstKey: TclKeyboardKey;
begin
  Result := TclKeyboardKey(FList[0]);
  FCursor := 1;
end; { GetFirstKey }

function TclKeyboardLayout.GetNextKey: TclKeyboardKey;
begin
  Result := nil;
  if FCursor < FList.Count then
  begin
    Result := TclKeyboardKey(FList[FCursor]);
    Inc(FCursor);
  end;
end; { GetNextKey }

function TclKeyboardLayout.GetKeyCount(ARow: integer): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do
    if TclKeyboardKey(FList[i]).Row = ARow then
      Inc(Result);
end; { GetKeyCount }

function TclKeyboardLayout.GetRowCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do
    if TclKeyboardKey(FList[i]).Row > Result then
      Result := TclKeyboardKey(FList[i]).Row;
end; { GetKeyCount }

// TclDvorakLayout =============================================================

constructor TclDvorakLayout.Create;
begin
  inherited;
  CreateKey('`', #0, 1, 0.93, clWhite);
  CreateKey('1', #0, 1, 0.93, clWhite);
  CreateKey('2', #0, 1, 0.93, clWhite);
  CreateKey('3', #0, 1, 0.93, clWhite);
  CreateKey('4', #0, 1, 0.93, clWhite);
  CreateKey('5', #0, 1, 0.93, clWhite);
  CreateKey('6', #0, 1, 0.93, clWhite);
  CreateKey('7', #0, 1, 0.93, clWhite);
  CreateKey('8', #0, 1, 0.93, clWhite);
  CreateKey('9', #0, 1, 0.93, clWhite);
  CreateKey('0', #0, 1, 0.93, clWhite);
  CreateKey('[', #0, 1, 0.93, clWhite);
  CreateKey(']', #0, 1, 0.93, clWhite);
  CreateKey('Bkspc', #0, 1, 1.91, clWhite);
  CreateKey('Tab', #0, 2, 1.42, clWhite);
  CreateKey('''', #0, 2, 0.93, clWhite);
  CreateKey(',', #0, 2, 0.93, clWhite);
  CreateKey('.', #0, 2, 0.93, clWhite);
  CreateKey('P', #0, 2, 0.93, clYellow);
  CreateKey('Y', #0, 2, 0.93, clYellow);
  CreateKey('F', #0, 2, 0.93, clAqua);
  CreateKey('G', #0, 2, 0.93, clAqua);
  CreateKey('C', #0, 2, 0.93, clWhite);
  CreateKey('R', #0, 2, 0.93, clWhite);
  CreateKey('L', #0, 2, 0.93, clWhite);
  CreateKey('/', #0, 2, 0.93, clWhite);
  CreateKey('=', #0, 2, 0.93, clWhite);
  CreateKey('', #0, 2, 1.42, clWhite);
  CreateKey('CAPS', #0, 3, 1.55, clWhite);
  CreateKey('A', #0, 3, 0.86, clSilver);
  CreateKey('O', #0, 3, 0.86, clSilver);
  CreateKey('E', #0, 3, 0.86, clSilver);
  CreateKey('U', #0, 3, 0.86, clSilver);
  CreateKey('I', #0, 3, 0.86, clYellow);
  CreateKey('D', #0, 3, 0.86, clAqua);
  CreateKey('H', #0, 3, 0.86, clSilver);
  CreateKey('T', #0, 3, 0.86, clSilver);
  CreateKey('N', #0, 3, 0.86, clSilver);
  CreateKey('S', #0, 3, 0.86, clSilver);
  CreateKey('-', #0, 3, 0.86, clWhite);
  CreateKey('ENTER', #0, 3, 1.99, clWhite);
  CreateKey('SHIFT', #0, 4, 1.80, clWhite);
  CreateKey(';', #0, 4, 0.8, clWhite);
  CreateKey('Q', #0, 4, 0.8, clWhite);
  CreateKey('J', #0, 4, 0.8, clWhite);
  CreateKey('K', #0, 4, 0.8, clYellow);
  CreateKey('X', #0, 4, 0.8, clYellow);
  CreateKey('B', #0, 4, 0.8, clAqua);
  CreateKey('M', #0, 4, 0.8, clAqua);
  CreateKey('W', #0, 4, 0.8, clWhite);
  CreateKey('V', #0, 4, 0.8, clWhite);
  CreateKey('Z', #0, 4, 0.8, clWhite);
  CreateKey('SHIFT', #0, 4, 2.20, clWhite);
end; { Create }

constructor TclQwertyLayout.Create;
begin
  inherited;
  CreateKey('`', #0, 1, 0.93, clWhite);
  CreateKey('1', #0, 1, 0.93, clWhite);
  CreateKey('2', #0, 1, 0.93, clWhite);
  CreateKey('3', #0, 1, 0.93, clWhite);
  CreateKey('4', #0, 1, 0.93, clWhite);
  CreateKey('5', #0, 1, 0.93, clWhite);
  CreateKey('6', #0, 1, 0.93, clWhite);
  CreateKey('7', #0, 1, 0.93, clWhite);
  CreateKey('8', #0, 1, 0.93, clWhite);
  CreateKey('9', #0, 1, 0.93, clWhite);
  CreateKey('0', #0, 1, 0.93, clWhite);
  CreateKey('-', #0, 1, 0.93, clWhite);
  CreateKey('=', #0, 1, 0.93, clWhite);
  CreateKey('Bkspc', #0, 1, 1.91, clWhite);
  CreateKey('Tab', #0, 2, 1.42, clWhite);
  CreateKey('Q', #0, 2, 0.93, clWhite);
  CreateKey('W', #0, 2, 0.93, clWhite);
  CreateKey('E', #0, 2, 0.93, clWhite);
  CreateKey('R', #0, 2, 0.93, clWhite);
  CreateKey('T', #0, 2, 0.93, clWhite);
  CreateKey('Y', #0, 2, 0.93, clWhite);
  CreateKey('U', #0, 2, 0.93, clWhite);
  CreateKey('I', #0, 2, 0.93, clWhite);
  CreateKey('O', #0, 2, 0.93, clWhite);
  CreateKey('P', #0, 2, 0.93, clWhite);
  CreateKey('[', #0, 2, 0.93, clWhite);
  CreateKey(']', #0, 2, 0.93, clWhite);
  CreateKey('', #0, 2, 1.42, clWhite);
  CreateKey('CAPS', #0, 3, 1.55, clWhite);
  CreateKey('A', #0, 3, 0.86, clSilver);
  CreateKey('S', #0, 3, 0.86, clSilver);
  CreateKey('D', #0, 3, 0.86, clSilver);
  CreateKey('F', #0, 3, 0.86, clSilver);
  CreateKey('G', #0, 3, 0.86, clWhite);
  CreateKey('H', #0, 3, 0.86, clWhite);
  CreateKey('J', #0, 3, 0.86, clSilver);
  CreateKey('K', #0, 3, 0.86, clSilver);
  CreateKey('L', #0, 3, 0.86, clSilver);
  CreateKey(';', #0, 3, 0.86, clSilver);
  CreateKey('''', #0, 3, 0.86, clWhite);
  CreateKey('ENTER', #0, 3, 1.99, clWhite);
  CreateKey('SHIFT', #0, 4, 1.80, clWhite);
  CreateKey('Z', #0, 4, 0.8, clWhite);
  CreateKey('X', #0, 4, 0.8, clWhite);
  CreateKey('C', #0, 4, 0.8, clWhite);
  CreateKey('V', #0, 4, 0.8, clWhite);
  CreateKey('B', #0, 4, 0.8, clWhite);
  CreateKey('N', #0, 4, 0.8, clWhite);
  CreateKey('M', #0, 4, 0.8, clWhite);
  CreateKey(',', #0, 4, 0.8, clWhite);
  CreateKey('.', #0, 4, 0.8, clWhite);
  CreateKey('/', #0, 4, 0.8, clWhite);
  CreateKey('SHIFT', #0, 4, 2.20, clWhite);
end; { Create }


// TclKeyboardPanel ============================================================

constructor TclKeyboardPanel.Create;
begin
  inherited;
end; { Create }

destructor TclKeyboardPanel.Destroy;
begin
  FCurrentLayout.Free;
  inherited;
end; { Destroy }

procedure TclKeyboardPanel.Paint;
var
  workBmp: TBitmap;
  CurKeyRect: TRect;
  AKey: TclKeyboardKey;
  CurRow: integer;
  //x: integer;
  //Size: integer;
  BaseKeyWidth, AKeyWidth, AKeyHeight: integer;
begin
  if FCurrentLayout = nil then exit;
  
  (* reset *)
  Canvas.Brush.Color := clGray;
  Canvas.Rectangle(0, 0, Width, Height);

  //Size := 40;
  workBmp := TBitmap.Create;
  try
    AKey := FCurrentLayout.GetFirstKey;
    //x := 1;
    CurKeyRect.Top := 2;
    CurKeyRect.Left := 2;
    while AKey <> nil do
    begin
      CurRow := AKey.Row;
      BaseKeyWidth := Self.Width div FCurrentLayout.GetKeyCount(CurRow);
      AKeyHeight := (Self.Height - 2) div FCurrentLayout.GetRowCount;
      workBmp.Height := AKeyHeight;
      AKeyWidth := Round(BaseKeyWidth * AKey.WidthRatio);
      workBmp.Width := AKeyWidth;
      CurKeyRect.Bottom := CurKeyRect.Top + AKeyHeight;
      CurKeyRect.Right := CurKeyRect.Left + AKeyWidth;
      PaintKey(AKey, workBmp);
      Canvas.CopyRect(CurKeyRect, workBmp.Canvas,
        Rect(0, 0, workBmp.Width, workBmp.Height));
      //if x = 3 then break;
      AKey := FCurrentLayout.GetNextKey;
      //INc(x);
      if AKey <> nil then
      begin
        if AKey.Row <> CurRow then
        begin
          CurKeyRect.Top := CurKeyRect.Top + AKeyHeight;
          CurKeyRect.Left := 2;

          //CurRow := AKey.Row;
        end
        else begin
          CurKeyRect.Left := CurKeyRect.Left + AKeyWidth;
        end;
      end;
    end;
  finally
    workBmp.Free;
  end;
end; { Paint }

(*:This procedure will paint a keyboard key image into the entire TBitmap area. *)
procedure TclKeyboardPanel.PaintKey(AKey: TclKeyboardKey; ABitmap: TBitmap);
var
  w, h, s, TextWidth: integer;
  ARect: TRect;
  AFont: TFont;
  AFontStyle: TFontStyles;
begin
  w := ABitmap.Width;
  h := ABitmap.Height;

  (* draw generic key *)
  ARect := Rect(0, 0, w, h);
  ABitmap.Canvas.Brush.Color := clGray;
  ABitmap.Canvas.FillRect(ARect);

  ARect.Left   := w div 8;
  ARect.Right  := w - ARect.Left;
  ARect.Top    := h div 16;
  ARect.Bottom := h - (ARect.Top * 3);

  if w < h then s := w else s := h;
  ABitmap.Canvas.Brush.Color := AKey.Color;

  ABitmap.Canvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
    s div 4, s div 4);

  (* draw character(s) *)
  AFont := TFont.Create;
  try
    if AKey.SuperChar = #0 then
    begin
      //TextHeight := h - (h div 4);
      TextWidth  := ABitmap.Canvas.TextWidth(AKey.KeyChar);

      (* from TFont.Height in Delphi help *)
      AFont.Size := 8;//-TextHeight * AFont.PixelsPerInch div 72;
      AFontStyle := AFont.Style;
      {if AKey.Color then
        Include(AFontStyle, fsBold);}
      AFont.Style := AFontStyle;
      ABitmap.Canvas.Font := AFont;

      ABitmap.Canvas.TextOut((w div 2) - (TextWidth div 2), h div 4, AKey.KeyChar);
    end;
  finally
    AFont.Free;
  end;
end; { PaintKey }

procedure TclKeyboardPanel.SetCurrentLayoutName(ACurrentLayoutName: TclKeyboardLayoutName);
begin
  if (FCurrentLayoutName <> ACurrentLayoutName)
     or
     (FCurrentLayout = nil) then
  begin
    FCurrentLayoutName := ACurrentLayoutName;
    FCurrentLayout.Free;
    case FCurrentLayoutName of
    klQwerty: FCurrentLayout := TclQwertyLayout.Create;
    klDvorak: FCurrentLayout := TclDvorakLayout.Create;
    end;
    Refresh;
  end;
end; { SetCurrentLayoutName }


end.
