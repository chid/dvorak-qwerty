unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, HotKeyUnit;

type
  TMain = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    HotkeyHandler : THgeHotkey; // Hotkey handler

    procedure HotKeyPressed(Sender: TObject); // Event
  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

{$R *.DFM}

procedure TMain.FormCreate(Sender: TObject);
begin
  HotkeyHandler := THgeHotkey.Create(Self, 'A');
  HotkeyHandler.OnHotkey := HotKeyPressed;  // Assign event handler
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  HotKeyHandler.Free;
end;

procedure TMain.HotKeyPressed(Sender: TObject);
begin
  MessageDlg('You pressed the hotkey ALT_' + HotkeyHandler.Hotkey,
    mtConfirmation, [mbOK],0);
end;

procedure TMain.Button1Click(Sender: TObject);
begin
  HotkeyHandler.Hotkey := Edit1.Text;
end;

end.
