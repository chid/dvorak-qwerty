{ rev 1.1 - fix in version string parsing routine }
unit cLabsAboutFrm;
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
  StdCtrls, ExtCtrls, clCreditPanel, ComCtrls;

type
  TclAboutForm = class(TForm)
    Panel1: TPanel;
    btnOK: TButton;
    PageControl1: TPageControl;
    tbsCredits: TTabSheet;
    Panel2: TPanel;
    lblVersion: TLabel;
    lblURL: TLabel;
    lblEmail: TLabel;
    lblAppNameFull: TLabel;
    clCreditPanel: TclCreditPanel;
    tbsLicense: TTabSheet;
    Memo1: TMemo;
    procedure lblURLClick(Sender: TObject);
    procedure lblEmailClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lblAppNameFullClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDate: string;
    FBuild: string;
    FVersion: string;
    FAppName: string;
    FAppURL: string;
    procedure SetBuild(const Value: string);
    procedure SetDate(const Value: string);
    procedure SetVersion(const Value: string);
    procedure SetAppURL(const Value: string);
    procedure SetAppName(const Value: string);
    { Private declarations }
  public
    procedure ReadVersionInfo;
    procedure ShowForm;

    property AppName: string read FAppName write SetAppName;
    property AppURL: string read FAppURL write SetAppURL;
    property Build: string read FBuild write SetBuild;
    property Date: string read FDate write SetDate;
    { Maj.Min.Release }
    property Version: string read FVersion write SetVersion;
  end;

var
  clAboutForm: TclAboutForm;

implementation

uses ShellApi, VersInfo;

{$R *.DFM}

procedure TclAboutForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage := tbsCredits;
end;

procedure TclAboutForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then Close;
end;

procedure TclAboutForm.lblAppNameFullClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open',
    PChar(FAppURL), Nil, Nil,0);
end;

procedure TclAboutForm.lblEmailClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar('mailto:' + lblEmail.Caption), Nil, Nil,0);
end;

procedure TclAboutForm.lblURLClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(lblURL.Caption), Nil, Nil,0);
end;

{ refactor to use clUtilFile }
procedure TclAboutForm.ReadVersionInfo;
var
  v: TVersionInfo;
  i: integer;
  Version: string;
  Work: string;
  iPos: integer;
begin
  v := TVersionInfo.Create(nil);
  try
    v.FileName := ''; // defaults to App.ExeName
    Version := v.FileVersion;
    Work := Version;
    iPos := 0;
    for i := 1 to 3 do
    begin
      iPos := Pos('.', Work) + iPos;
      Work := Copy(Version, iPos + 1, Length(Version));
    end;
    FVersion := Copy(Version, 1, iPos - 1);
    FBuild := Copy(Version, iPos + 1, Length(Version));
  finally
    v.Free;
  end;
end;

procedure TclAboutForm.SetAppName(const Value: string);
begin
  FAppName := Value;
end;

procedure TclAboutForm.SetAppURL(const Value: string);
begin
  FAppURL := Value;
end;

procedure TclAboutForm.SetBuild(const Value: string);
begin
  FBuild := Value;
end;

procedure TclAboutForm.SetDate(const Value: string);
begin
  FDate := Value;
end;

procedure TclAboutForm.SetVersion(const Value: string);
begin
  FVersion := Value;
end;

procedure TclAboutForm.ShowForm;
begin
  ReadVersionInfo;
  lblAppNameFull.Caption := FAppName;
  lblVersion.Caption := StringReplace(lblVersion.Caption, 'vvv', FVersion, []);
  lblVersion.Caption := StringReplace(lblVersion.Caption, 'bbb', FBuild, []);
  lblVersion.Caption := StringReplace(lblVersion.Caption, 'ddd', FDate, []);
  ShowModal;
end;

end.

