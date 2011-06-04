{ #(@)$Id: UnitTestGUITesting.pas,v 1.1 2001/07/05 13:02:47 chrismo Exp $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 1.1 $ uberto 08/03/2001
}
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Serge Beaumont
 * and Juancarlo Añez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2001.
 * All rights reserved.
 *
 * Contributor(s):
 * Serge Beaumont <beaumose@iquip.nl>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Uberto Barbini <uberto@usa.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)
unit UnitTestGUITesting;

interface
uses
  TestFramework,
  GUITesting,
  GUITestRunner,

  SysUtils,
  Classes,
  Windows,
  Graphics;

const
  rcs_id: string = '#(@)$Id: UnitTestGUITesting.pas,v 1.1 2001/07/05 13:02:47 chrismo Exp $';

type
  TDunitDialogCracker = class(TGUITestRunner);

  TGUITestRunnerTests = class(TGUITestCase)
    FRunner :TGUITestRunner;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTabOrder;
    procedure TestViewResult;
    procedure TestElapsedTime;
    procedure RunEmptySuite;
    procedure RunSuccessSuite;
    procedure RunFailureSuite;
  end;

implementation

type
  TSuccessTestCase = class(TTestCase)
  published
    procedure OneSuccess;
    procedure SecondSuccess;
  end;

  TFailuresTestCase = class(TTestCase)
  private
    procedure DoNothing;
  published
    procedure OneSuccess;
    procedure OneFailure;
    procedure SecondFailure;
    procedure OneError;
  end;

  TTimeTestCase = class(TTestCase)
  published
    procedure TestTime;
  end;

{ TGUITestRunnerTests }

procedure TGUITestRunnerTests.SetUp;
begin
  inherited;
  FRunner := TGUITestRunner.Create(nil);
  FRunner.Color   := clWhite;
  FRunner.Caption := 'This Form is being tested';
  FRunner.Left    :=  FRunner.Left + 200;
  FRunner.Top     := FRunner.Top + 100;
  FRunner.Width   := 300;
  FRunner.Height  := 480;
  FRunner.AutoSaveAction.Checked := False;
  GUI := FRunner;
end;

procedure TGUITestRunnerTests.TearDown;
begin
  GUI := nil;
  FRunner.Free;
  inherited;
end;

procedure TGUITestRunnerTests.TestTabOrder;
begin
  // need to set a test suite, or buttons will be disabled
  FRunner.Suite := TFailuresTestCase.Suite;
  FRunner.AutoSaveAction.Checked := False;
  FRunner.BreakOnFailuresAction.Checked := False;
  Show;
  (*!! Actions are now in Toolbar
  CheckFocused(FRunner.RunButton);
  Tab;
  CheckFocused(FRunner.CloseButton);
  Tab;
  *)
  CheckFocused(FRunner.TestTree);
  Tab;
  CheckFocused(FRunner.ResultsView);
  Tab;
  CheckFocused(FRunner.FailureListView);
  Tab;
  CheckFocused(FRunner.ErrorMessageRTF);
  Tab;
  (*
  CheckFocused(FRunner.RunButton);
  Tab;
  CheckTabTo('RunButton');
  *)
end;

procedure TGUITestRunnerTests.RunEmptySuite;
begin
  // no suite
  Show;

  CheckEquals( 0,  FRunner.ProgressBar.Position,               'progress bar');
  CheckEquals( '', FRunner.LbProgress.Caption,               'progress label');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[0],   'tests');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[1],   'run count');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[2],   'failure count');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[3],   'error count');
  CheckEquals( 0,  FRunner.FailureListView.Items.Count,        'failure list');

  EnterKey(vk_F9);
  // nothing happens
  CheckEquals( 0,  FRunner.ProgressBar.Position,               'progress bar');
  CheckEquals( '', FRunner.LbProgress.Caption,               'progress label');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[0],   'tests');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[1],   'run count');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[2],   'failure count');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[3],   'error count');
  CheckEquals( 0,  FRunner.FailureListView.Items.Count,        'failure list');

  EnterKey('X', [ssAlt]);

  Check(not FRunner.Visible, 'form closed?');
end;

procedure TGUITestRunnerTests.RunSuccessSuite;
begin
  FRunner.Suite := TSuccessTestCase.Suite;
  FRunner.AutoSaveAction.Checked := False;
  FRunner.BreakOnFailuresAction.Checked := False;
  Show;

  CheckEquals( 0,  FRunner.ProgressBar.Position,               'progress bar');
  CheckEquals( '', FRunner.LbProgress.Caption,               'progress label');
  CheckEquals('2', FRunner.ResultsView.Items[0].SubItems[0],   'tests');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[1],   'run count');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[2],   'failure count');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[3],   'error count');
  CheckEquals( 0,  FRunner.FailureListView.Items.Count,        'failure list');

  EnterKey('R', [ssAlt]);

  CheckEquals( 2,  FRunner.ProgressBar.Position,               'progress bar');
  CheckEquals( '100%', FRunner.LbProgress.Caption,             'progress label');
  CheckEquals('2', FRunner.ResultsView.Items[0].SubItems[0],   'tests');
  CheckEquals('2', FRunner.ResultsView.Items[0].SubItems[1],   'run count');
  CheckEquals('0', FRunner.ResultsView.Items[0].SubItems[2],   'failure count');
  CheckEquals('0', FRunner.ResultsView.Items[0].SubItems[3],   'error count');
  CheckEquals( 0,  FRunner.FailureListView.Items.Count,        'failure list');

  EnterKey('X', [ssAlt]);

  Check(not FRunner.Visible, 'form closed?');
end;

procedure TGUITestRunnerTests.RunFailureSuite;
begin
  FRunner.Suite := TFailuresTestCase.Suite;
  FRunner.AutoSaveAction.Checked := False;
  FRunner.BreakOnFailuresAction.Checked := False;
  Show;

  CheckEquals( 0,  FRunner.ProgressBar.Position,               'progress bar');
  CheckEquals( '', FRunner.LbProgress.Caption,               'progress label');
  CheckEquals('4', FRunner.ResultsView.Items[0].SubItems[0],   'tests');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[1],   'run count');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[2],   'failure count');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[3],   'error count');
  CheckEquals( 0,  FRunner.FailureListView.Items.Count,        'failure list');

  EnterKey('R', [ssAlt]);

  CheckEquals( 4,  FRunner.ProgressBar.Position,               'progress bar');
  CheckEquals( '25%', FRunner.LbProgress.Caption,             'progress label');
  CheckEquals('4', FRunner.ResultsView.Items[0].SubItems[0],   'tests');
  CheckEquals('4', FRunner.ResultsView.Items[0].SubItems[1],   'run count');
  CheckEquals('2', FRunner.ResultsView.Items[0].SubItems[2],   'failure count');
  CheckEquals('1', FRunner.ResultsView.Items[0].SubItems[3],   'error count');
  CheckEquals( 3,  FRunner.FailureListView.Items.Count,        'failure list');


  EnterKey('X', [ssAlt]);

  Check(not FRunner.Visible, 'form closed?');
end;

procedure TGUITestRunnerTests.TestViewResult;
begin
  FRunner.Suite := TFailuresTestCase.Suite;
  FRunner.AutoSaveAction.Checked := False;
  FRunner.BreakOnFailuresAction.Checked := False;
  Show;
  FRunner.TestTree.SetFocus;
  FRunner.TestTree.Items[0].Item[0].Selected := true;
  FRunner.TestTree.Items[0].Item[0].Selected := true;
  EnterKey('D',[ssAlt]); //to uncheck node
  CheckEquals( 0,  FRunner.ProgressBar.Position,               'progress bar');
  CheckEquals( '', FRunner.LbProgress.Caption,                 'progress label');
  CheckEquals('4', FRunner.ResultsView.Items[0].SubItems[0],   'tests');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[1],   'run count');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[2],   'failure count');
  CheckEquals('',  FRunner.ResultsView.Items[0].SubItems[3],   'error count');
  CheckEquals( 0,  FRunner.FailureListView.Items.Count,        'failure list');
end;

procedure TGUITestRunnerTests.TestElapsedTime;
var
  ElapTime, MinTime, MaxTime: string;
begin
  FRunner.Suite := TTimeTestCase.Suite;
  FRunner.AutoSaveAction.Checked := False;
  FRunner.BreakOnFailuresAction.Checked := False;
  Show;
  EnterKey(vk_F9);
  ElapTime := FRunner.ResultsView.Items[0].SubItems[4];
  MinTime := '0:00:00.090';
  MaxTime := '0:00:00.110';
  Check(ElapTime > MinTime, 'elapsed time ('+ElapTime+') should be bigger than ' + MinTime);
  Check(ElapTime < MaxTime, 'elapsed time ('+ElapTime+') should be lesser than ' + MaxTime);
end;

{ TSuccessTestCase }

procedure TSuccessTestCase.OneSuccess;
begin
  check(true);
end;

procedure TSuccessTestCase.SecondSuccess;
begin
  check(true);
end;

{ TFailuresTestCase }

procedure TFailuresTestCase.OneSuccess;
begin
  DoNothing;
end;

procedure TFailuresTestCase.OneError;
begin
  raise EAbort.Create('One Error');
end;

procedure TFailuresTestCase.OneFailure;
begin
  fail('One failure');
end;

procedure TFailuresTestCase.SecondFailure;
begin
  fail('Second failure');
end;

procedure TFailuresTestCase.DoNothing;
begin
// Do Nothing
end;

{ TTimeTestCase }


procedure TTimeTestCase.TestTime;
const
  DELAY = 100;
begin
  Sleep(DELAY);
  Check( True );
end;

initialization
  RegisterTestSuites('GUI Tests', [TGUITestRunnerTests]);
end.

