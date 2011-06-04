program HotKey;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Main},
  HotKeyUnit in 'HotKeyUnit.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
