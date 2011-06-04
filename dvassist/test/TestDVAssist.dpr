program TestDVAssist;

{%TogetherDiagram 'ModelSupport_TestDVAssist\default.txaPackage'}

uses
  TestFramework,
  GUITestRunner,
  DVAssistTest in 'DVAssistTest.pas';

{$R *.RES}

function Suite: ITestSuite;
begin
  Result := DVAssistTest.suite;
end;

begin
  GUITestRunner.RunTest(Suite);
end.

