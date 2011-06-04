program TestHk1;

uses
  Forms,
  TestHk1Form in 'TestHk1Form.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
