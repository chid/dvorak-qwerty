unit TestDVAFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    btnDvorak: TButton;
    btnQwerty: TButton;
    Memo1: TMemo;
    Button3: TButton;
    procedure btnDvorakClick(Sender: TObject);
    procedure btnQwertyClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    procedure Toggle(OrigLayoutString, LayoutString: string);
    procedure SendMessage(DvHKL: HKL);
    { Private declarations }
  public
    { Public declarations }
  end;

(* I think chasing LoadKeyboardLayoutEx is a rabbit trail. My guess is
  maybe it was a function that never panned out - it's just never been
  removed. Why can't I UnloadKeyboardLayout?

{ 1st param cannot be nil - if it is, function does nothing at all.
  A handle to somethin'?

  2nd param is definitely a pointer (passed flags in before and AVed).

Function seems to want a string for the 2nd param - it copies it
into a local variable (A). Then it passes 2 unused local vars (both
4 bytes long - B & C), the 3rd parameter of the call, and the local
var (A) into a subroutine.

Maybe the 3rd param is an OUT parameter

The register containing the 3rd param going into this call is
checked after the call returns. If that register is nil, the
function returns - doing nothing.

Current goal - get EAX to not be nil when that call returns.

Okay - I ran regmon on my test call - it very clearly seems to be
missing a language ID.

I'm guessing that maybe the Ex call is a special call after
LKL to do the global switch. I'm guessing it takes a handle as the
first param - then maybe the next 2 params are pointers to the
old and new keyboard layouts.

AHA - requires Unicode string - I watched the string concatenation process
in the CPU window and saw the Unicode string - and the ANSI being appended.
Bad.

The reg output now that I've got it - looks just like a portion of output
from LKLayout - right in the middle - I wonder if this is a rabbit trail.

 }
(*function LoadKeyboardLayoutEx(AnHKL: HKL;
  pwszOldKLID: PChar; ANewHKL: HKL): boolean; stdcall;  *)
function LoadKeyboardLayoutEx(pwszKLID: PWideChar;
  pwszNewKLID: PWideChar; ANewHKL: HKL): boolean; stdcall;

var
  Form1: TForm1;

implementation

uses Registry;

{$R *.DFM}

function LoadKeyboardLayoutEx; external user32 name 'LoadKeyboardLayoutEx';

procedure TForm1.Button3Click(Sender: TObject);
var
  DvHKL: HKL;
begin
  (*DvHKL := LoadKeyboardLayoutEx(PChar('00010409'), PChar('00000409'),
    KLF_ACTIVATE or KLF_SUBSTITUTE_OK or KLF_UNLOADPREVIOUS);  *)
end;

procedure TForm1.btnDvorakClick(Sender: TObject);
begin
  Toggle('00000409', '00010409');
end;

procedure TForm1.btnQwertyClick(Sender: TObject);
begin
  Toggle('00000409', '00000409');
end;

procedure TForm1.SendMessage(DvHKL: HKL);
begin
  PostMessage(HWND_BROADCAST, WM_INPUTLANGCHANGEREQUEST, 0, DvHKL);

  //PostMessage(HWND_BROADCAST, WM_INPUTLANGCHANGE, 0, DvHKL); doesn't seem to affect anything
end;

procedure TForm1.Toggle(OrigLayoutString, LayoutString: string);
var
  DvHKL: HKL;
  OrigHandle: HKL;
  NewHandle: HKL;
  r: TRegistry;
  UNILayoutString: PWideChar;
begin
  OrigHandle := GetKeyboardLayout(0);

  (*GetMem(UNILayoutString, Length(LayoutString) * 2);
  try
    StringToWideChar(LayoutString, UNILayoutString, Length(LayoutString) * 2);

    if not LoadKeyboardLayoutEx(UNILayoutString, UNILayoutString, NewHandle) then
      MessageDlg('LKLEx returned false', mtError, [mbOK], 0)
    else begin                                                                *)

    { Change registry to change default }
    r := TRegistry.Create;
    try
      r.RootKey := HKEY_CURRENT_USER;
      r.DeleteKey('Keyboard Layout');
      r.OpenKey('Keyboard Layout\Preload', true);
      r.WriteString('1', '00000409');
      r.CloseKey;
      r.OpenKey('Keyboard Layout\Substitutes', true);
      if LayoutString = '00010409' then
        r.WriteString('00000409', '00010409');
      r.CloseKey;
      r.OpenKey('Keyboard Layout\Toggle', true);
      r.CloseKey;

      { New docs on MSDN say the UnloadPrevious flag is unsupported }

      DvHKL := LoadKeyboardLayout(PChar(LayoutString), KLF_ACTIVATE or
        KLF_SUBSTITUTE_OK or KLF_REPLACELANG);

      if DvHKL = 0 then
        raise Exception.Create('LoadKeyboardLayout failed.');

(*    end;
  finally
    FreeMem(UNILayoutString);
  end; *)
    r.OpenKey('Keyboard Layout\Toggle', true);
    r.WriteString('HotKey', '1');
    r.CloseKey;
  finally
    r.Free;
  end;

  SendMessage(DvHKL);

  if not SystemParametersInfo(SPI_SETDEFAULTINPUTLANG, 0, Addr(DvHKL),
    SPIF_SENDCHANGE) then
    RaiseLastWin32Error;

  if not UnloadKeyboardLayout(OrigHandle) then
    raise Exception.Create('Unable to unload keyboard layout.');

  Memo1.SetFocus;
end;


end.

