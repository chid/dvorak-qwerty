unit TestLoadKeyboardForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  DvHKL: HKL;
begin
  if SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT then
    (* NT *)
    {DvHKL := LoadKeyboardLayout(PChar(LangString), KLF_ACTIVATE or KLF_REORDER
      or KLF_SUBSTITUTE_OK)}
    {DvHKL := LoadKeyboardLayout(PChar(LangString), KLF_ACTIVATE or
      KLF_UNLOADPREVIOUS)}
    DvHKL := LoadKeyboardLayout(PChar(LangString), KLF_ACTIVATE or
      KLF_SUBSTITUTE_OK or KLF_UNLOADPREVIOUS)
  else
    (* 95 *)
    DvHKL := LoadKeyboardLayout(PChar(LangString), KLF_ACTIVATE or KLF_REORDER
      or KLF_REPLACELANG or KLF_SUBSTITUTE_OK);

  if DvHKL = 0 then
    raise Exception.Create('LoadKeyboardLayout failed.');
end; { LoadLayout }

end.
