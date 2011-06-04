{ $Id: TestExtensions.pas,v 1.1 2001/07/05 13:02:47 chrismo Exp $ }
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
// Turning long strings off enables the use of the framework in a DLL without
// the need for the Borland memory management DLL (BORLDNDMM.DLL).
{$LONGSTRINGS OFF}
unit TestExtensions;

interface

uses
  Classes,
  IniFiles,
  TestFramework;

type
  {: General interface for test decorators}
  ITestDecorator = interface(ITest)
    ['{85DE7EC1-686E-11D4-B323-00C04F5E43B6}']
    {: Get the decorated test
    @return The decorated test }
    function GetTest: ITest;
    property Test: ITest read GetTest;
  end;

  {:A Decorator for Tests. Use TTestDecorator as the base class
    for defining new test decorators. Test decorator subclasses
    can be introduced to add behaviour before or after a test
    is run. }
  TTestDecorator = class(TAbstractTest, ITestDecorator, ITest)
  protected
    FName:  string;
    FTest:  ITest;
    FTests: IInterfaceList;

    function GetTest: ITest;
  public
    {: Decorate a test. If no name parameter is given, the decorator
       will be named as the decorated test, with some extra information
       prepended.
       @param ATest The test to decorate.
       @param AName  Optional name to give to the decorator. }
    constructor Create(ATest: ITest; AName: string = '');

    function  CountTestCases: integer;          override;
    function  CountEnabledTestCases: integer;   override;

    function  GetName: string;                  override;
    function  Tests: IInterfaceList;            override;

    {: Overrides the inherited behavior and executes the
       decorated test's RunBare instead }
    procedure RunBare(ATestResult: TTestResult); override;

    procedure LoadConfiguration(const iniFile :TCustomIniFile; const section :string);  override;
    procedure SaveConfiguration(const iniFile :TCustomIniFile; const section :string);  override;

    property Test: ITest read GetTest;
  end;

  {:A Decorator to set up and tear down additional fixture state.
    Subclass TestSetup and insert it into your tests when you want
    to set up additional state once before the tests are run.
    @example <br>
    <code>
    function UnitTests: ITest;
    begin
      Result := TSetubDBDecorator.Create(TDatabaseTests.Suite, 10);
    end; </code> }
  TTestSetup = class(TTestDecorator)
  protected
    {:Sets up the fixture. Override to set up additional fixture state.}
    procedure Setup; virtual; abstract;
    {:Tears down the fixture. Override to tear down the additional
     fixture state.}
    procedure TearDown; virtual; abstract;
  public
    constructor Create(ATest: ITest; AName: string = '');
    procedure RunBare(ATestResult: TTestResult); override;
  end;

 {:A test decorator that runs a test repeatedly.
   Use TRepeatedTest to run a given test or suite a specific number
   of times.
    @example <br>
    <code>
    function UnitTests: ITestSuite;
    begin
      Result := TRepeatedTest.Create(ATestArithmetic.Suite, 10);
    end;
    </code> }
  TRepeatedTest = class(TTestDecorator)
  private
    FTimesRepeat: integer;
  public
    {: Construct decorator that repeats the decorated test.
       The ITest parameter can hold a single test or a suite. The Name parameter
       is optional.
       @param ATest The test to repeat.
       @param Itrations The number of times to repeat the test.
       @param AName An optional name to give to the decorator instance }
    constructor Create(ATest: ITest; Iterations: integer; AName: string = '');
    function  GetName: string;                    override;

    {: Overrides the inherited behavior to included the number of repetitions.
       @return Iterations * inherited CountTestCases }
    function  CountTestCases: integer;            override;

    {: Overrides the inherited behavior to included the number of repetitions.
       @return Iterations * inherited CountEnabledTestCases }
    function  CountEnabledTestCases: integer;     override;

    {: Overrides the behavior of the base class as to execute
       the test repeatedly. }
    procedure RunBare(ATestResult: TTestResult);  override;
  end;

  {: A test decorator for running tests in a separate thread
     @todo Implement this class }
  TActiveTest = class(TTestDecorator)
  end;

  {: A test decorator for running tests expecting a specific exceptions
     to be thrown.
     @todo Implement this class }
  TExceptionTestCase = class(TTestDecorator)
  end;

implementation

uses SysUtils;

{ TTestDecorator }

procedure TTestDecorator.RunBare(ATestResult: TTestResult);
begin
  FTest.RunBare(ATestResult);
end;

function TTestDecorator.CountEnabledTestCases: integer;
begin
  if Enabled then
    Result := FTest.countEnabledTestCases
  else
    Result := 0;
end;

function TTestDecorator.CountTestCases: integer;
begin
  if Enabled then
    Result := FTest.countTestCases
  else
    Result := 0;
end;

constructor TTestDecorator.Create(ATest: ITest; AName: string);
begin
  if AName <> '' then
    inherited Create(AName)
  else
    inherited Create(ATest.name);
  FTest := ATest;
  FTests:= TInterfaceList.Create;
  FTests.Add(FTest);
end;

function TTestDecorator.GetTest: ITest;
begin
  Result := FTest;
end;

procedure TTestDecorator.LoadConfiguration(const iniFile: TCustomIniFile; const section: string);
begin
  FTest.LoadConfiguration(iniFile, section)
end;

procedure TTestDecorator.SaveConfiguration(const iniFile: TCustomIniFile; const section: string);
begin
  FTest.SaveConfiguration(iniFile, section)
end;

function TTestDecorator.tests: IInterfaceList;
begin
   Result := FTests;
end;


function TTestDecorator.GetName: string;
begin
  Result := Format('[d] %s', [getTest.Name]);
end;

{ TTestSetup }

constructor TTestSetup.Create(ATest: ITest; AName: string);
begin
  inherited Create(ATest, AName);
end;

procedure TTestSetup.RunBare(ATestResult: TTestResult);
begin
  try
    Setup;
  except
    on E: Exception do
      assert(false, 'Setup failed: ' + e.message);
  end;
  try
    inherited RunBare(ATestResult);
  finally
    try
      TearDown;
    except
      on E: Exception do
        assert(false, 'Teardown failed: ' + e.message);
    end;
  end;
end;

{ TRepeatedTest }

function TRepeatedTest.CountEnabledTestCases: integer;
begin
  Result := inherited CountTestCases * FTimesRepeat;
end;

function TRepeatedTest.CountTestCases: integer;
begin
  Result := inherited CountTestCases * FTimesRepeat;
end;

constructor TRepeatedTest.Create(ATest: ITest; Iterations: integer;
  AName: string);
begin
  inherited Create(ATest, AName);
  FTimesRepeat := Iterations;
end;

function TRepeatedTest.GetName: string;
begin
  Result := Format('%d x %s', [FTimesRepeat, getTest.Name]);
end;

procedure TRepeatedTest.RunBare(ATestResult: TTestResult);
var
  i: integer;
begin
  assert(assigned(ATestResult));

  for i := 0 to FTimesRepeat - 1 do
  begin
    if ATestResult.shouldStop then
      Break;
    inherited RunBare(ATestResult);
  end;
end;

end.

