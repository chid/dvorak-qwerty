{ $Id: UnitTestExtensions.pas,v 1.1 2001/07/05 13:02:47 chrismo Exp $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 1.1 $
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
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo Añez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2001.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)
unit UnitTestExtensions;

interface

uses
  TestFramework,
  TestExtensions;

const
  COUNT_MAX = 5;

type
  ITestStub = interface(ITest)
    function GetCounter: integer;
  end;

  TTestStub = class(TTestCase, ITestStub)
  protected
    FCounter: integer;
  public
    function GetCounter: integer;
  published
    procedure test;
  end;

  TTestSetupStub = class(TTestSetup)
  public
    SetupCalled: boolean;
    TearDownCalled: boolean;

    procedure Setup; override;
    procedure TearDown; override;
  end;

  TTestStubTest = class(TTestCase)
  private
    FTestResult: TTestResult;
    FTestStub: ITestStub;
  public
    procedure Setup; override;
    procedure TearDown; override;
  end;

  TTestSetupTest = class(TTestStubTest)
  private
    FSetupTest: TTestSetupStub;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestSetupTest;
    procedure TestDecoratedEnabling;
  end;

  TTestRepeatedTest = class(TTestStubTest)
  private
    FIterations: integer;
    FRepTest: ITest;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure testRepeatedTest;
    procedure testWithCounting;
  end;

  TCountCase = class(TTestCase)
  private
    FCounter : Integer;
    FTotal   : Integer;
    FLast    : Integer;
  public
    procedure Setup; override;
  published
    procedure CountTest; virtual;
  end;

implementation
uses
  SysUtils;

{ TTestStub }

function TTestStub.GetCounter: integer;
begin
  Result := FCounter;
end;

procedure TTestStub.test;
begin
  check(true);
  Inc(FCounter);
end;

{ TTestSetupStub }

procedure TTestSetupStub.Setup;
begin
  SetupCalled := true;
end;

procedure TTestSetupStub.TearDown;
begin
  TearDownCalled := true;
end;

{ TTestStubTest }

procedure TTestStubTest.Setup;
begin
  inherited;
  FTestStub := TTestStub.Create('test');
  FTestResult := TTestResult.Create;
end;

procedure TTestStubTest.TearDown;
begin
  FTestResult.Free;
  FTestStub := nil;
  inherited;
end;
            
{ TTestSetupTest }

procedure TTestSetupTest.Setup;
begin
  inherited;
  FSetupTest := TTestSetupStub.Create(FTestStub);
end;

procedure TTestSetupTest.TearDown;
begin
  FSetupTest.Free;
  inherited;
end;

procedure TTestSetupTest.TestDecoratedEnabling;
var
  childEnabled :boolean;
begin
  childEnabled := FSetupTest.Test.Enabled;
  FSetupTest.Enabled := true;
  check(FSetupTest.Enabled);
  check(childEnabled = FSetupTest.Test.Enabled);
  FSetupTest.Enabled := false;
  check(not FSetupTest.Enabled);
  check(childEnabled = FSetupTest.Test.Enabled);
end;

procedure TTestSetupTest.TestSetupTest;
begin
  { call the interface to ensure proper inheritence in TTestSetup.
    To make this test fail, remove the override directive from
    TTestSetup.Run. }
  ITestDecorator(FSetupTest).Run(FTestResult);
  check(FTestResult.wasSuccessful);
  check(FSetupTest.SetupCalled);
  check(FSetupTest.TearDownCalled);
end;

{ TTestRepeatedTest }

procedure TTestRepeatedTest.Setup;
begin
  inherited;
  FIterations := COUNT_MAX;
  FRepTest := TRepeatedTest.Create(FTestStub, FIterations);
end;

procedure TTestRepeatedTest.TearDown;
begin
  FRepTest := nil;
  FRepTest := nil;
  inherited;
end;

procedure TTestRepeatedTest.testRepeatedTest;
begin
  check(FRepTest.CountTestCases = COUNT_MAX);
  check(FTestStub.getEnabled);
  FRepTest.Run(FTestResult);
  check(FTestResult.wasSuccessful);
  check(FTestStub.GetCounter = COUNT_MAX);
end;

procedure TTestRepeatedTest.testWithCounting;
var
  CountCase :ITest;
  AREsult   :TTestResult;
begin
  CountCase := TRepeatedTest.Create(TCountCase.Create('CountTest'), COUNT_MAX);
  AResult := CountCase.Run;
  try
    check(AResult.runCount     = COUNT_MAX, 'wrong runCount, was ' + IntToStr(AResult.runCount) );
    check(AResult.failureCount = 0, 'wrong failureCount, was ' + IntToStr(AResult.failureCount) );
    check(AResult.errorCount   = 0, 'wrong errorCount, was ' + IntToStr(AResult.errorCount) );
  finally
    AResult.Free
  end
end;

{ TCountCase }

procedure TCountCase.CountTest;
begin
   Inc(FCounter);
   check(FCounter = 1,  'must be one, or setup was not called');
   Inc(FTotal);
   check(FTotal   >= 1, 'total should be at least one');
   check(FTotal   = (FLast+1),  'total should be increment');
   FLast := FTotal;
end;

procedure TCountCase.Setup;
begin
  FCounter := 0;
end;

initialization
  RegisterTests('TestExtensions Suite',
                     [ TTestRepeatedTest.Suite,
                       TTestSetupTest.Suite,
                       TRepeatedTest.Create(TCountCase.Create('CountTest'), COUNT_MAX)
                      ]);
end.

