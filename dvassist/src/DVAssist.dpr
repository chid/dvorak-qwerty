program DVAssist;

{%TogetherDiagram 'ModelSupport_DVAssist\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_DVAssist\DVAssist\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_DVAssist\DVAssistForm\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_DVAssist\DVAKeyHook\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_DVAssist\default.txvpck'}
{%TogetherDiagram 'ModelSupport_DVAssist\DVAssistForm\default.txvpck'}
{%TogetherDiagram 'ModelSupport_DVAssist\DVAssist\default.txvpck'}

uses
  Forms,
  DVAssistForm in 'DVAssistForm.pas' {DVAssistFrm},
  DVAKeyHook in 'DVAKeyHook.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.ShowMainForm := false;
  Application.Title := 'Dvorak Assistant';
  Application.CreateForm(TDVAssistFrm, DVAssistFrm);
  Application.Run;
end.
