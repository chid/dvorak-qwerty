unit TestHk1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

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
  EnterKeyCount : DWORD; {An internal count of the Enter Key}

procedure TForm1.FormCreate(Sender: TObject);
begin
 {Set our initial variables}
  Timer1.Enabled := FALSE;
  Timer1.Interval := 1000;
  Label1.Caption := '0 Keys Logged';
  Label2.Caption := '0 Enter Keys Logged';
  EnterKeyCount := 0;
  lpHookRec := NIL;
  LibLoadSuccess := FALSE;
  @GetHookRecPointer := NIL;
  @StartKeyBoardHook := NIL;
  @StopKeyBoardHook := NIL;
 {Try to load the hook dll}
  hHookLib := LoadLibrary('THEHOOK.DLL');
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
         lpHookRec^.TheCtrlWinHandle := Button1.Handle;
         lpHookRec^.TheKeyCount := 0;
        {Start the keyboard hook}
         StartKeyBoardHook;
        {Start the timer if the hook was successfully set}
         if (lpHookRec^.TheHookHandle <> 0) then begin
           Timer1.Enabled := TRUE;
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
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 {Did we load the dll successfully?}
  if (LibLoadSuccess = TRUE) then begin
   {Did we sucessfully get a pointer to the hook record?}
    if (lpHookRec <> nil) then begin
     {Did the hook get set?}
      if (lpHookRec^.TheHookHandle <> 0) then begin
        Timer1.Enabled := FALSE;
        StopKeyBoardHook;
      end;
    end;
   {Free the hook dll}
    FreeLibrary(hHookLib);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 {Display the number of keystrokes logged}
  Label1.Caption := IntToStr(lpHookRec^.TheKeyCount) + ' Keys Logged';
end;

procedure TForm1.Button1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 {Process message sent from hook dll and display}
 {number of time the enter key was pressed}
  if (Key = 0) then begin
    Inc(EnterKeyCount);
    Label2.Caption := IntToStr(EnterKeyCount) + ' Enter Keys Logged';
  end;
end;

end.

