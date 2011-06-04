unit clCreditPanel;
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
  TclCreditPanelStyle = (psScrollUp, psSlideShow, psScrollLeft);
  TclCreditPanelFontStyle = (fsNormal, fsHighlight);

  TclCreditClickEvent = procedure (CreditIndex: integer) of object;
  TclCredit = class (TObject)
  public
    Rect: TRect;
    Selected: Boolean;
    Text: string;
  end;

  TclCreditPanel = class (TCustomPanel)
  private
    FBackgroundColor: TColor;
    FBitmap: TBitmap;
    FCenterOffset: TPoint;
    FCreditList: TList;
    FCredits: TStrings;
    FFontHeight: Integer;
    FForegroundColor: TColor;
    FHasDragged: boolean;
    FHighlightSelect: Boolean;
    FHitPoint: TPoint;
    FMouseDown: Boolean;
    FMouseOverTimerInterval: Integer;
    FNeedDrawMaster: Boolean;
    FOnClickCredit: TclCreditClickEvent;
    FPrevMousePt: TPoint;
    FScrollAmount: Integer;
    FScrollAmountToUse: integer;
    FScrollBottom: Integer;
    FScrollLeft: Integer;
    FScrollTop: Integer;
    FSelectedIndex: Integer;
    FStyle: TclCreditPanelStyle;
    FTimer: TTimer;
    FTimerInterval: Integer;
    FAfterCredit: TNotifyEvent;
    FBeforeCredit: TNotifyEvent;
    FAfterScroll: TNotifyEvent;
    FBeforeTimer: TNotifyEvent;
    FAfterTimer: TNotifyEvent;
    FMouseOverScrollAmount: integer;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetCredits(Value: TStrings);
    procedure SetForegroundColor(const Value: TColor);
    procedure SetHighlightSelect(Value: Boolean);
    procedure SetMouseOverTimerInterval(Value: Integer);
    procedure SetScrollAmount(Value: Integer);
    procedure SetStyle(const Value: TclCreditPanelStyle);
    procedure SetTimerInterval(Value: Integer);
    procedure SetAfterCredit(const Value: TNotifyEvent);
    procedure SetBeforeCredit(const Value: TNotifyEvent);
    procedure SetAfterScroll(const Value: TNotifyEvent);
    procedure SetAfterTimer(const Value: TNotifyEvent);
    procedure SetBeforeTimer(const Value: TNotifyEvent);
    procedure SetMouseOverScrollAmount(const Value: integer);
  protected
    procedure BlankCanvas;
    procedure CalcMasterHitPoint(X, Y: integer);
    procedure CalculateScrollBottom;
    procedure CheckCursorPos(p: TPoint);
    procedure ClearCanvas;
    procedure Click; override;
    function CountCredits: Integer;
    procedure DoAfterScroll;
    procedure DoAfterTimer;
    procedure DoBeforeTimer;
    procedure DragDisplay(X, Y: integer);
    procedure DrawBitmapToCanvas(Dest, Source: TRect);
    procedure DrawMasterBitmap;
    procedure FillCreditList;
    procedure FreeCreditList;
    function HitTest(APoint: TPoint; ARect: TRect): Boolean;
    procedure Init;
    procedure InitScrollValues;
    procedure InitTimer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    function SelectCredit(Index: integer): Boolean;
    procedure SetFontStyle(AFont: TFont; AStyle: TclCreditPanelFontStyle);
    procedure TimerEvent(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DrawPanel(Step: boolean);
    function GetPanelCanvas: TCanvas;
    procedure ResetCredits;
    {:Resets credits to the first credit}
    procedure ResetToTop;

    property Canvas;
  published
    property Align;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property Credits: TStrings read FCredits write SetCredits;
    property ForegroundColor: TColor read FForegroundColor write SetForegroundColor;
    property HighlightSelect: Boolean read FHighlightSelect write SetHighlightSelect;
    property MouseOverScrollAmount: integer read FMouseOverScrollAmount write SetMouseOverScrollAmount;
    property MouseOverTimerInterval: Integer read FMouseOverTimerInterval write
        SetMouseOverTimerInterval;
    property PopupMenu;
    property ScrollAmount: Integer read FScrollAmount write SetScrollAmount;
    property Style: TclCreditPanelStyle read FStyle write SetStyle;
    property TimerInterval: Integer read FTimerInterval write SetTimerInterval;
    property Visible;

    property AfterCredit: TNotifyEvent read FAfterCredit write SetAfterCredit;
    property AfterScroll: TNotifyEvent read FAfterScroll write SetAfterScroll;
    property AfterTimer: TNotifyEvent read FAfterTimer write SetAfterTimer;
    property BeforeCredit: TNotifyEvent read FBeforeCredit write SetBeforeCredit;
    property BeforeTimer: TNotifyEvent read FBeforeTimer write SetBeforeTimer;
    property OnClickCredit: TclCreditClickEvent read FOnClickCredit write FOnClickCredit;
  end;

{$IFDEF CLLOG}
var
  LogFile: TextFile;
{$ENDIF}

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CLabs', [TclCreditPanel]);
end;

{ TclCreditPanel }

{
************************************ TclCreditPanel *************************************
}
procedure TclCreditPanel.BlankCanvas;
begin
  Canvas.Brush.Color := FBackgroundColor;
  Canvas.FillRect(GetClientRect);
  Canvas.Font.Color := FForegroundColor;
end; { BlankCanvas }

{:Method calculates what credit the mouse is currently over, marks it and
  if it's changed, forces a redraw.    }
procedure TclCreditPanel.CalcMasterHitPoint(X, Y: integer);
var
  i: Integer;
  SelectChange: Boolean;
  Rect: TRect;
  AnyHit: Boolean;
begin
  { first step is to translate the input point to the master bitmap point. }
  case FStyle of
  psScrollUp:
    { X does not require a translation - only Y. }
    begin
      Y := Y + FScrollTop;
    end;
  psSlideShow:
    { this one is a bit more difficult - not supported currently }
    begin
      //X := -1;
      Y := Y + FScrollTop - FCenterOffset.y;
    end;
  psScrollLeft:
    { Y does not require translation - only X. }
    begin
      X := X + FScrollLeft;
    end;
  end;

  FHitPoint.X := X;
  FHitPoint.Y := Y;

  AnyHit := false;
  SelectChange := false;
  for i := 0 to FCreditList.Count - 1 do
  begin
    Rect := TclCredit(FCreditList[i]).Rect;
    if HitTest(FHitPoint, Rect) then
    begin
      AnyHit := true;
      SelectChange := SelectCredit(i);
    end;
  end;

  if not AnyHit then
    SelectChange := SelectCredit(-1);

  if SelectChange then
    DrawMasterBitmap;
end; { CalcMasterHitPoint }

procedure TclCreditPanel.CalculateScrollBottom;
var
  i: Integer;
begin
  { find the top credit }
  i := FScrollTop div FFontHeight;
  if i < 0 then i := 0;
  while i <= FCredits.Count - 1 do
  begin
    if FCredits[i] = '' then break;
    Inc(i);
  end;
  FScrollBottom := i * FFontHeight;
end; { CalculateScrollBottom }

procedure TclCreditPanel.CheckCursorPos(p: TPoint);
begin
  if HitTest(p, Self.ClientRect) then
  begin
    FTimer.Interval := FMouseOverTimerInterval;
    FScrollAmountToUse := FMouseOverScrollAmount;
  end
  else begin
    FTimer.Interval := FTimerInterval;
    FScrollAmountToUse := FScrollAmount;
  end;
end; { CheckCursorPos }

procedure TclCreditPanel.ClearCanvas;
begin
  Canvas.Brush.Color := FBackgroundColor;
  Canvas.FillRect(GetClientRect);
end; { ClearCanvas }

procedure TclCreditPanel.Click;
begin
  inherited;
  if Assigned(FOnClickCredit) and (not FHasDragged) then
    if FSelectedIndex <> -1 then
      FOnClickCredit(FSelectedIndex);
end; { Click }

{:Returns number of credits. Credits are delimited by a blank line.   }
function TclCreditPanel.CountCredits: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FCredits.Count - 1 do
    if FCredits[i] = '' then Inc(Result);

  { the last credit probably won't be followed by a blank line, so up the Count
    one more. This will break (have an additional count) if the last line
    *is* a blank line. }
  if FCredits.Count > 0 then Inc(Result);
end; { CountCredits }

constructor TclCreditPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitmap := TBitmap.Create;
  FCreditList := TList.Create;
  FCredits := TStringList.Create;
  FTimer := TTimer.Create(Self);
  FBackgroundColor := clBtnFace;
  FForegroundColor := clWindowText;
  FScrollTop := 0;
  FScrollAmount := 2;
  FScrollTop := -Height;
  FScrollBottom := 0;
  FNeedDrawMaster := true;
  FTimerInterval := 20;
  FMouseOverTimerInterval := 40;
end; { Create }

destructor TclCreditPanel.Destroy;
begin
  FTimer.Free;
  FreeCreditList;
  FCredits.Free;
  FCreditList.Free;
  FBitmap.Free;
  inherited;
end; { Destroy }

procedure TclCreditPanel.DoAfterScroll;
begin
  if Assigned(FAfterScroll) then FAfterScroll(Self);
end;

procedure TclCreditPanel.DoAfterTimer;
begin
  if Assigned(FAfterTimer) then FAfterTimer(Self);
end;

procedure TclCreditPanel.DoBeforeTimer;
begin
  if Assigned(FBeforeTimer) then FBeforeTimer(Self);
end;

procedure TclCreditPanel.DragDisplay(X, Y: integer);
begin
  case FStyle of
  psScrollUp:
    begin
      FScrollTop := FScrollTop - (Y - FPrevMousePt.y);
    end;
  psSlideShow:
    begin
      // not supported
    end;
  psScrollLeft:
    begin
      FScrollLeft := FScrollLeft - (X - FPrevMousePt.x);
    end;
  end;
  FHasDragged := true;
end; { DragDisplay }

{:The whole reason for this method is to eliminate any trailing garbage
  when copying to just a portion of the master bitmap to the panel canvas.
  For example, drag the credits (when scrolling left) to the right so
  the left most edge of the credit bitmap falls in view. To the left of
  this, garbage will be left over. }
procedure TclCreditPanel.DrawBitmapToCanvas(Dest, Source: TRect);
var
  ABitmap: TBitmap;
begin
  ABitmap := TBitmap.Create;
  try
    { first blank out transition bitmap }
    ABitmap.Width := Width;
    ABitmap.Height := Height;
    ABitmap.Canvas.Brush.Color := FBackgroundColor;
    ABitmap.Canvas.FillRect(GetClientRect);

    { then copy portion from master bitmap to work bitmap }
    ABitmap.Canvas.CopyRect(Dest, FBitmap.Canvas, Source);

    { copy clean work bitmap to panel canvas }
    Canvas.CopyRect(GetClientRect, ABitmap.Canvas, GetClientRect);
  finally
    ABitmap.Free;
  end;
end;

procedure TclCreditPanel.DrawMasterBitmap;
var
  i, j: Integer;
  x, curTop, curLeft: Integer;
  TextWidth: Integer;
  BlockWidth: Integer;
  StartCredit: Integer;
  ACredit: TclCredit;

  procedure ClearBitmap;
  begin
    FBitmap.Canvas.Brush.Color := FBackgroundColor;
    FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
    FBitmap.Canvas.Font.Color := FForegroundColor;
  end;
begin
  if FCreditList <> nil then
  begin
    FillCreditList;
    FNeedDrawMaster := false;

    FFontHeight := FBitmap.Canvas.TextHeight('ANYTEXTHEREg');
    case FStyle of
    psScrollUp..psSlideShow:
      begin
        FBitmap.Width := Width;
        FBitmap.Height := FCredits.Count * FFontHeight;
        ClearBitmap;
        curTop := 0;

        for i := 0 to FCreditList.Count - 1 do
        begin
          ACredit := TclCredit(FCreditList[i]);
          TextWidth := FBitmap.Canvas.TextWidth(ACredit.Text);
          x := (FBitmap.Width div 2) - (TextWidth div 2);
          if FHighlightSelect then
            if ACredit.Selected then
              SetFontStyle(FBitmap.Canvas.Font, fsHighlight);
          FBitmap.Canvas.TextOut(x, curTop, ACredit.Text);

          { store rect where credit is drawn for hit testing }
          ACredit.Rect := Rect(x, curTop, x + TextWidth, curTop + FFontHeight);
          SetFontStyle(FBitmap.Canvas.Font, fsNormal);
          curTop := curTop + FFontHeight;
        end;
      end;
    psScrollLeft:
      begin
        FBitmap.Width := 0;
        FBitmap.Height := Height;
        curLeft := 0;
        StartCredit := 0; BlockWidth := 0;
        ClearBitmap;

        for i := 0 to FCreditList.Count - 1 do
        begin
          if TclCredit(FCreditList[i]).Text <> '' then
          begin
            { cycle through group, find widest line }
            if FBitmap.Canvas.TextWidth(TclCredit(FCreditList[i]).Text) > BlockWidth then
              BlockWidth :=
                FBitmap.Canvas.TextWidth(TclCredit(FCreditList[i]).Text);
          end
          else begin
            { add width to FBitmap: widest line plus border }
            FBitmap.Width := FBitmap.Width + BlockWidth + 10;

            { set curTop to the total height of this group }
            curTop := ((i - StartCredit) * FFontHeight);

            { set curTop to the pixel setting to center group }
            curTop := (FBitmap.Height div 2) - (curTop div 2);

            for j := StartCredit to (i - 1) do
            begin
              ACredit := TclCredit(FCreditList[j]);
              { text out all lines in group, centered in current 'frame' }
              TextWidth := FBitmap.Canvas.TextWidth(ACredit.Text);
              x := curLeft + (BlockWidth div 2) - (TextWidth div 2);
              if FHighlightSelect then
                if ACredit.Selected then
                  SetFontStyle(FBitmap.Canvas.Font, fsHighlight);
              (*HitRect := Rect(x, curTop, x + TextWidth, curTop + FFontHeight);
              if HitTest(FHitPoint, HitRect) then
              begin
                Selected := i;
              end;*)
              FBitmap.Canvas.TextOut(x, curTop, ACredit.Text);
              { store rect where credit is drawn for hit testing }
              ACredit.Rect :=
                Rect(x, curTop, x + TextWidth, curTop + FFontHeight);
              SetFontStyle(FBitmap.Canvas.Font, fsNormal);
              curTop := curTop + FFontHeight;
            end;
            curLeft := curLeft + BlockWidth + 10;
            BlockWidth := 0;
            StartCredit := i + 1;
          end;
        end;
      end;
    end;
  end;
end; { DrawMasterBitmap }

procedure TclCreditPanel.DrawPanel(Step: boolean);
var
  SrcR: TRect;
  SrcW: Integer;
  SrcH: Integer;
  DestR: TRect;
  p: TPoint;
begin
  {$IFDEF CLLOG}
  WriteLn(LogFile, FTimer.Interval);
  {$ENDIF}
  if FNeedDrawMaster then DrawMasterBitmap;
  p := ScreenToClient(Mouse.CursorPos);
  CheckCursorPos(p);
  CalcMasterHitPoint(p.x, p.y);
  try
    if FCredits.Count = 0 then
      FTimer.Enabled := false
    else begin
      case FStyle of
      psScrollUp:
        begin
          if Step and (not FMouseDown) then
          begin
            Inc(FScrollTop, FScrollAmountToUse);
            if FScrollTop > FBitmap.Height then
              FScrollTop := -Height;
          end;
          DrawBitmapToCanvas(GetClientRect,
            Rect(0, FScrollTop, FBitmap.Width, FScrollTop + Height));
          DoAfterScroll;
        end;
      psSlideShow:
        begin
          if Assigned(FBeforeCredit) then FBeforeCredit(Self);
          ClearCanvas;
          if Step and (not FMouseDown) then
          begin
            FScrollTop := FScrollBottom + FFontHeight;
            if (FScrollTop > FBitmap.Height) or (FScrollTop < 0) then
              FScrollTop := 0;
            CalculateScrollBottom;
          end;
          SrcR := Rect(0, FScrollTop, FBitmap.Width, FScrollBottom);
          SrcW := (SrcR.Right - SrcR.Left);
          SrcH := (SrcR.Bottom - SrcR.Top);
          FCenterOffset.x := (Width div 2) - (SrcW div 2);
          FCenterOffset.y := (Height div 2) - (SrcH div 2);
          DestR := Rect((Width div 2) - (SrcW div 2),
            (Height div 2) - (SrcH div 2), (Width div 2) + (SrcW div 2),
            (Height div 2) + (SrcH div 2) + 1);
          DrawBitmapToCanvas(DestR, SrcR);
          if Assigned(FAfterCredit) then FAfterCredit(Self);
        end;
      psScrollLeft:
        begin
          if Step and (not FMouseDown) then
          begin
            Inc(FScrollLeft, FScrollAmountToUse);
            if FScrollLeft > FBitmap.Width then
              FScrollLeft := -Width;
          end;
          DrawBitmapToCanvas(GetClientRect,
            Rect(FScrollLeft, 0, FScrollLeft + Width, Height));
          DoAfterScroll;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      FTimer.Enabled := false;
      raise;
    end;
  end;
end; { DrawPanel }

procedure TclCreditPanel.FillCreditList;
var
  i: Integer;
  ACredit: TclCredit;
begin
  if FCreditList.Count <> FCredits.Count then
  begin
    FreeCreditList;
    for i := 0 to FCredits.Count - 1 do
    begin
      ACredit := TclCredit.Create;
      ACredit.Text := FCredits[i];
      FCreditList.Add(ACredit);
    end;
  end;
end; { FillCreditList }

procedure TclCreditPanel.FreeCreditList;
var
  i: integer;
begin
  for i := 0 to FCreditList.Count - 1 do
    TObject(FCreditList[i]).Free;
  FCreditList.Clear;
end; { FreeCreditList }

function TclCreditPanel.GetPanelCanvas: TCanvas;
begin
  Result := Canvas;
end;

{:Checks to see if APoint is in the ARect. Returns true if APoint is in
  or on the border, false if not.    }
function TclCreditPanel.HitTest(APoint: TPoint; ARect: TRect): Boolean;
begin
  Result := (
             (ARect.Left <= APoint.X) and (APoint.X <= ARect.Right)
             and
             (ARect.Top <= APoint.Y) and (APoint.Y <= ARect.Bottom)
            );
end; { HitTest }

procedure TclCreditPanel.Init;
begin
        {if not (csDestroying in ComponentState) then
        begin
          InitTimer;
          DrawMasterBitmap;
          ClearCanvas;
        end;}
end; { Init }

procedure TclCreditPanel.InitScrollValues;
begin
  case FStyle of
  psScrollUp:
    begin
      FScrollTop := -Height;
      FScrollBottom := 0; // not used
    end;
  psSlideShow:
    begin
      FScrollTop := 0;
      { this value forces the first FScrollTop in DrawPanel to end up at
        0 }
      FScrollBottom := -100; // will be calculated
    end;
  psScrollLeft:
    begin
      FScrollLeft := -Width;
    end;
  end;
end; { InitScrollValues }

procedure TclCreditPanel.InitTimer;
begin
  if FTimer <> nil then
  begin
    FTimer.Enabled := false;
    FTimer.Interval := FTimerInterval;
    FTimer.OnTimer := TimerEvent;
    FTimer.Enabled := (FTimerInterval <> 0);
  end;
end; { InitTimer }

procedure TclCreditPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
    Integer);
begin
  inherited;
  FMouseDown := true;
  FPrevMousePt.X := X;
  FPrevMousePt.Y := Y;
end; { MouseDown }

procedure TclCreditPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FMouseDown then
  begin
    { Drag Display }
    DragDisplay(X, Y);
    FPrevMousePt.X := X;
    FPrevMousePt.Y := Y;
  end;
  DrawPanel(false);
end; { MouseMove }

procedure TclCreditPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:
    Integer);
begin
  inherited;
  FMouseDown := false;
  FHasDragged := false;
end; { MouseUp }

procedure TclCreditPanel.Paint;
begin
  ClearCanvas;
end; { Paint }

procedure TclCreditPanel.ResetCredits;
begin
  FreeCreditList;
  FillCreditList;
  DrawMasterBitmap;
  InitTimer;
end;

{:Resets credits to the first credit }
procedure TclCreditPanel.ResetToTop;
begin
  InitScrollValues;
  BlankCanvas;
end; { ResetToTop }

procedure TclCreditPanel.Resize;
begin
  inherited;
  ClearCanvas;
  DrawMasterBitmap;
  if not FTimer.Enabled then
    FScrollTop := -Height;
end; { Resize }

function TclCreditPanel.SelectCredit(Index: integer): Boolean;
var
  i: Integer;
  ACredit: TclCredit;
  PrevSelect: Integer;
begin
  PrevSelect := -1;
  FSelectedIndex := -1;
  for i := 0 to FCreditList.Count - 1 do
  begin
    ACredit := TclCredit(FCreditList[i]);
    if ACredit.Selected then
      PrevSelect := i;

    ACredit.Selected := (i = Index);
    if ACredit.Selected then
      FSelectedIndex := i;
  end;

  Result := (PrevSelect <> Index);
end; { SelectCredit }

procedure TclCreditPanel.SetAfterCredit(const Value: TNotifyEvent);
begin
  FAfterCredit := Value;
end;

procedure TclCreditPanel.SetAfterScroll(const Value: TNotifyEvent);
begin
  FAfterScroll := Value;
end;

procedure TclCreditPanel.SetAfterTimer(const Value: TNotifyEvent);
begin
  FAfterTimer := Value;
end;

procedure TclCreditPanel.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
end; { SetBackgroundColor }

procedure TclCreditPanel.SetBeforeCredit(const Value: TNotifyEvent);
begin
  FBeforeCredit := Value;
end;

procedure TclCreditPanel.SetBeforeTimer(const Value: TNotifyEvent);
begin
  FBeforeTimer := Value;
end;

procedure TclCreditPanel.SetCredits(Value: TStrings);
begin
  FCredits.Assign(Value);
  ResetCredits;
end; { SetCredits }

procedure TclCreditPanel.SetFontStyle(AFont: TFont; AStyle: TclCreditPanelFontStyle);
var
  AFontStyles: TFontStyles;
begin
  case AStyle of
  fsNormal:
    begin
      AFont.Color := clWindowText;
      AFontStyles := [];
    end;
  fsHighlight:
    begin
      AFont.Color := clBlue;
      AFontStyles := [fsUnderline];
    end;
  end;
  AFont.Style := AFontStyles;
end; { SetFontStyle }

procedure TclCreditPanel.SetForegroundColor(const Value: TColor);
begin
  FForegroundColor := Value;
end; { SetForegroundColor }

procedure TclCreditPanel.SetHighlightSelect(Value: Boolean);
begin
  FHighlightSelect := Value;
end; { SetHighlightSelect }

procedure TclCreditPanel.SetMouseOverScrollAmount(const Value: integer);
begin
  FMouseOverScrollAmount := Value;
end;

procedure TclCreditPanel.SetMouseOverTimerInterval(Value: Integer);
begin
  FMouseOverTimerInterval := Value;
end; { SetMouseOverTimerInterval }

procedure TclCreditPanel.SetScrollAmount(Value: Integer);
begin
  FScrollAmount := Value;
end; { SetScrollAmount }

procedure TclCreditPanel.SetStyle(const Value: TclCreditPanelStyle);
begin
  FStyle := Value;
  ClearCanvas;
  InitScrollValues;
  DrawMasterBitmap;
end; { SetStyle }

procedure TclCreditPanel.SetTimerInterval(Value: Integer);
begin
  FTimerInterval := Value;
  FTimer.Interval := Value;
  InitTimer;
end; { SetTimerInterval }

procedure TclCreditPanel.TimerEvent(Sender: TObject);
begin
  DoBeforeTimer;
  DrawPanel(true);
  DoAfterTimer;
end; { TimerEvent }

initialization
{$IFDEF CLLOG}
  AssignFile(LogFile, 'clCreditPanel.log');
  Rewrite(LogFile);
{$ENDIF}

finalization
{$IFDEF CLLOG}
  CloseFile(LogFile);
{$ENDIF}

end.


