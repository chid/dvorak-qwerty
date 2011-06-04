unit DVAssistConfigFrm;
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
  ExtCtrls, StdCtrls, ActnList, clHotKeyEdit;

type
  TDVAssistConfigForm = class(TForm)
    pnlClient: TPanel;
    pnlBottom: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    ActionList1: TActionList;
    actOK: TAction;
    actCancel: TAction;
    Label1: TLabel;
    lblBeware: TLabel;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure actOKExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure InitCustomComponents;
  public
    clHotKeyEdit: TclHotKeyEdit;
  end;

var
  DVAssistConfigForm: TDVAssistConfigForm;

implementation

{$R *.DFM}

procedure TDVAssistConfigForm.actCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TDVAssistConfigForm.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TDVAssistConfigForm.FormCreate(Sender: TObject);
begin
  InitCustomComponents;
end;

procedure TDVAssistConfigForm.InitCustomComponents;
begin
  clHotKeyEdit := TclHotKeyEdit.Create(Self);
  clHotKeyEdit.Parent := pnlClient;
  clHotKeyEdit.Left := 120;
  clHotKeyEdit.Top := 16;
  clHotKeyEdit.Width := 137;
  clHotKeyEdit.Height := 21;
  Label1.FocusControl := clHotKeyEdit;
end;

procedure TDVAssistConfigForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  { This enter key also gets trapped by the hot key edit box - 
  if Key = #13 then
    actOK.Execute;}

  if Key = #27 then
    actCancel.Execute;
end;

end.
