program dunit;

uses
  Windows,
  SysUtils,
  Forms,
  Dialogs,
  TestFramework in 'TestFramework.pas',
  GUITestRunner in 'GUITestRunner.pas' {GUITestRunner},
  TextTestRunner in 'TextTestRunner.pas',
  DUnitMainForm in 'DUnitMainForm.pas',
  DunitAbout in 'DunitAbout.pas' {DunitAboutBox},
  TestModules in 'TestModules.pas';

{$R *.RES}
{$R versioninfo.res }

const
  rcs_id :string = '#(@)$Id: dunit.dpr,v 1.1 2001/07/05 13:02:47 chrismo Exp $';
  SwitchChars = ['-','/'];

procedure RunInConsoleMode;
var
  i :Integer;
begin
  try
    Windows.AllocConsole;
    for i := 1 to ParamCount do
    begin
      if not (ParamStr(i)[1] in SwitchChars) then
         RegisterModuleTests(ParamStr(i));
    end;
    TextTestRunner.RunRegisteredTests(rxbHaltOnFailures);
  except
    on e:Exception do
      Writeln(Format('%s: %s', [e.ClassName, e.Message]));
  end;
end;

begin
  if FindCmdLineSwitch('c', SwitchChars, false) then
    RunInConsoleMode
  else
  begin
    Application.Initialize;
    Application.Title := 'DUnit - An Extreme Testing Framework';
    DUnitAbout.Splash;
    Application.CreateForm(TDUnitForm, DUnitForm);
    try
      Application.Run;
    except
      on e:Exception do
        ShowMessage(e.Message);
    end;
  end;
end.
