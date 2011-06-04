{#(@)$Id: TestFramework.pas,v 1.1 2001/07/05 13:02:47 chrismo Exp $ }
{  DUnit: An XTreme testing framework for Delphi programs. }
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
 * Uberto Barbini <uberto@usa.net>
 * Brett Shearer <BrettShearer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)
{: The testing framework.
   The TestFramework unit contains all the core definitions in
   the library, such as:<br>
   <ul>
     <li>The definitions of ITest, TTestCase, and TTestSuite.</li>
     <li>The definitions of TTestResult and ITestListener.</li>
     <li>The test registry.</li>
  </ul>
  The <a href="IDH_Class_TTestCase.htm">TTestCase</a> class should be
  ther first stop for all new users.
    @author  The DUnit Group.
    @version $Revision: 1.1 $
}


// Turning long strings off enables the use of the framework in a DLL without
// the need for the Borland memory management DLL (BORLDNDMM.DLL).
{$LONGSTRINGS OFF}
unit TestFramework;

interface
uses
  SysUtils,
  Classes,
  IniFiles;

const
  {:
  This is the version of the testframework which this documentation describes.
  }
  rcs_id: string = '#(@)$Id: TestFramework.pas,v 1.1 2001/07/05 13:02:47 chrismo Exp $';

type
  {: A method on a TTestCase descendent which implements a unit test. }
  TTestMethod  = procedure of object;
  TTestProc    = procedure;

  {: Used to pass a class reference to a descendant of TTestCase to TTestSuite.Create. }
  TTestCaseClass  = class of TTestCase;

  ITestListener = interface;

  TTestResult   = class;
  TAbstractTest = class;
  TTestCase     = class;
  TTestSuite    = class;
  TTestFailure  = class;

  {: Failed tests throw this exception.
     Exceptions of this type will be reported as test failures
     @seeAlso <See Class="EBreakingTestFailure"> }
  ETestFailure = class(EAbort)
     constructor Create;               overload;
     constructor Create(msg :string);  overload;
  end;

  {: Failed tests throw this exception when the framework is set to
     cause the Delphi IDE debugger to break on test failures.
     Exceptions of this type will be reported as test failures }
  EBreakingTestFailure = class(Exception)
     constructor Create;               overload;
     constructor Create(msg :string);  overload;
  end;

  {: The framework will throw this type of exception when
     it encounters an error in a test case }
  ETestError = class(Exception)
  end;

  {: Tests can throw this exception to stop any further testing }
  EStopTestsFailure = class(ETestFailure)
  end;

  EDUnitException = class(Exception);

  {:
  <em>ITest</em> represents a test that can be run.
  }
  ITest = interface(IUnknown)
    ['{715DF8E1-9925-11D3-992E-00207813339E}']
    {:
    Returns the name of this test.
    }
    function GetName: string;

    {:
    Counts the number of test cases that can be run by this test.
    }
    function CountTestCases: integer;

    {:
    Counts the number of test cases that will be run by this test.
    }
    function CountEnabledTestCases: integer;

    {:
    A convenience method to run this test, collecting the results with a
    default TestResult object.
    Run will Create a TTestResult, run this test case and collect any TTestFailure
    into the TTestResult.

    The caller is responsible for freeing the returned TTestResult.

    @returns A TTestResult with the results of running this test.
    }
    function Run : TTestResult;  overload;

    {:
    Runs a test and collects its result in a TTestResult instance.
    @param testResult the results of running this test are collected into this
      TTestResult object.
    see TTestResult
    }
    procedure Run(testResult: TTestResult); overload;

    {:
    Runs a test and collects its result in a TTestResult instance.
    @param testResult the results of running this test are collected into this
      TTestResult object.
    see TTestResult
    }
    procedure RunBare(testResult: TTestResult);

    {:
    returns true if the test is enabled to be run.
    }
    function GetEnabled: boolean;

    {:
    Determine if this test will be run.
    @param value  when true, the test will be enabled, when false, the test will be disabled.
    }
    procedure SetEnabled(Value: boolean);

    {: Load a previously saved configuration.
       The enabled/disabled state for each contained test will be read
       from the given TIniFile under the given section name.
       @param iniFile The TIniFile to read the configuration from.
       @param section The name of the section the configuration will be read from. }
    procedure LoadConfiguration(const iniFile :TCustomIniFile; const section :string);  overload;

    {: Load a previously saved configuration.
       The enabled/disabled state of each contained test will be loaded
       from a .INI file by the given name.
       @param fileName The name of the .INI file the configuration will be read from. }
    procedure LoadConfiguration(const fileName: string; const UseRegistry: boolean); overload;

    {: Save the current test configuration.
       The enabled/disabled state of each contained test will be saved
       to the given TIniFile under the given section name.
       @param iniFile The TIniFile to save the configuration to.
       @param section The name of the section the configuration will be saved to. }
    procedure SaveConfiguration(const iniFile :TCustomIniFile; const section :string);  overload;

    {: Save the current test configuration.
       The enabled/disabled state of each contained test will be saved
       to an .INI file by the given name.
       @param fileName The name of the .INI file the configuration will be saved to. }
    procedure SaveConfiguration(const fileName: string; const UseRegistry: boolean); overload;
    {:
    The name of this test.
    }
    property Name: string read GetName;

    {:
    Indicates if a test will be performed.  This test will only be run if enabled is
    true for this test.
    }
    property Enabled: boolean read GetEnabled write SetEnabled;

    function Tests: IInterfaceList;
  end;

  {: A Listener for test progress.
     TTestResult calls the methods of registered ITestListeners as
     testing progress. The standard TestRunners implement this interface
     to receive notifications about testing events.
     @seeAlso <See Class="TTestResult">
              <See Class="TGUITestRunner">
              <See Class="TTextTestRunner"> }
  ITestListener = interface
    ['{8E99FC5B-0B22-4FCF-B6BD-1F9B19E23591}']
    {:
    Successful test
    }
    procedure AddSuccess(test: ITest);

    {:
    An error occurred.
    }
    procedure AddError(error: TTestFailure);

    {:
    A failure occurred.
    }
    procedure AddFailure(Failure: TTestFailure);

    {:
    A test started.
    }
    procedure StartTest(test: ITest);

    {:
    A test ended.
    }
    procedure EndTest(test: ITest);

    {:
    Called when testing has started.
    }
    procedure TestingStarts;

    {:
    Called when testing has ended.
    }
    procedure TestingEnds(testResult :TTestResult);
  end;

  {:
  An ITestSuite is a Composite of Tests.
  It runs a collection of test cases as if they were a single test.
  see ITest
  }
  ITestSuite = interface(ITest)
    ['{715DF8E6-9925-11D3-992E-00207813339E}']
    {:
    Adds a test to the suite.
    }
    procedure AddTest(test: ITest);
    procedure AddSuite(suite : ITestSuite);
  end;


  {:
  A TTestResult collects the results of executing
  a test case. It is an instance of the Collecting Parameter pattern.
  The test framework distinguishes between <i>failures</i> and <i>errors</i>.
  A failure is anticipated and checked for with checks. Errors are
  unanticipated problems that raise an exception.

  see Check
  see ITest
  }
  TTestResult = class(TObject)
  private
    FTotalTime: Int64;
  protected
    fFailures: TList;
    fErrors: TList;
    fListeners: IInterfaceList;
    fRunTests: integer;
    fStop: boolean;
    FBreakOnFailures :boolean;

    {:
    Runs a TestCase.
    Run executes a test cases and broadcasts status information to any listeners
    registered with this TTestResult.  Run also records any failures or errors
    that occur while running the testcase.
       @param test The TTEstCase.
    }
    procedure Run(test: TTestCase); virtual;

    {: Calls TTestCase.SetUp, recording errors.
       Helper for TTestResult.Run;
       @param test The TTEstCase.
     }
    function RunTestSetup(test: TTestCase):boolean; virtual;

    {: Calls TTestCase.TearDown, recording errors.
       Helper for TTestResult.Run;
       @param test The TTEstCase.
     }
    procedure RunTestTearDown(test: TTestCase); virtual;

    {: Calls a TTestCase.RunTest, recording errors.
       Helper for TTestResult.Run;
       @param test The TTEstCase.
     }
    procedure RunTestRun(test: TTestCase); virtual;

    {: Notify listeners when testing starts }
    procedure TestingStarts;        virtual;
    {: Notify listeners when testing ends }
    procedure TestingEnds;          virtual;
  public
    {:
    construct a TTestCase.
    }
    constructor Create;

    destructor destroy; override;

    {:
    Notifies all listeners of a successful test
    }
    procedure AddSuccess(test: ITest);

    {:
    Adds an error to the list of errors.
    Creates a TTestFailure recording that an error occurred in test.
    Notifies any registered ITestListeners that an error occurred.

    The TTestFailure returned should not be freed.

    @param test The test with an error.
    @param e The exception that caused the error.
    @returns the TTestFailure object representing this error.
    }
    function AddError(test: ITest; e: Exception; addr :Pointer; msg :string = ''): TTestFailure; virtual;

    {:
    Adds a failure to the list of failures.
    Creates a TTestFailure recording that a failure occurred in test.
    Notifies any registered ITestListeners that a failure occurred.

    The TTestFailure returned should not be freed.

    @param test The test that failed.
    @param e The exception that caused the failure.
    @returns The TTestFailure object representing this failure.
    }
    function AddFailure(test: ITest; e: Exception; addr :Pointer): TTestFailure; virtual;

    {:
    Registers an ITestListener to listen to the status of the testing.
    }
    procedure addListener(listener: ITestListener); virtual;

    {:
    Informs the result that a test was completed.
    Notify any registered ITestListener that a test was completed.
    }
    procedure EndTest(test: TTestCase); virtual;

    {:
    Returns a list of the errors.
    }
    function errors: TList; virtual;

    {:
    Returns a list of the failures.
    }
    function Failures: TList; virtual;

    {:
    Gets the number of run tests.
    }
    function RunCount: integer; virtual;

    {:
    Checks whether the test run should stop
    }
    function ShouldStop: boolean; virtual;

    {:
    Informs the result that a test will be started.
    Notify any registered ITestListener that a test was started.
    }
    procedure StartTest(test: TTestCase); virtual;

    {:
    Marks that the test run should stop.
    }
    procedure Stop; virtual;

    {:
    Was testing stopped?.
    }
    function WasStopped :boolean; virtual;

    {:
    Gets the number of detected errors.
    }
    function ErrorCount: integer; virtual;

    {:
    Gets the number of detected failures.
    }
    function FailureCount: integer; virtual;

    {:
    Returns whether the entire test was successful or not.
    }
    function WasSuccessful: boolean; virtual;

    {:
    Runs a test.
    Run executes a test cases and broadcasts status informatio to any listeners
    registered with this TTestResult.  Run also records any failures or errors
    that occur while running the testcase. Listeners are also notified when
    TestingStarts and ends.
    @param test The test case to run.
    }
    procedure RunSuite(test: TAbstractTest);  overload;
    {: Allow the IDE to break on failure exceptions.
       When set to True, the framework raises EEBreakingTestFailure when a test fails.
       The Delphi IDE will break on this type of exception by default.
       When set to False, the framework will raise ETestFailure instead, which is derived
       from EAbort, and on which the IDE will not brak by default.
       The default value is False.
       @seeAlso <See Class="EBreakingTestFailure">
       @seeAlso <See Class="ETestFailure"> }
    property BreakOnFailures :boolean
      read  FBreakOnFailures
      write FBreakOnFailures;

    property TotalTime: Int64 read FTotalTime;
  end;


  {: Provides the basic functionality tha all implementors of ITest must
     provide. Basically: having a name, and having an enabled, disabled state
    @seeAslo <See Interface="ITest">
    @seeAslo <See Class="TTestCase">
    @seeAslo <See Class="TTestSuite">
    @seeAslo <See Class="TTestDecorator">
    }
  TAbstractTest = class(TInterfacedObject, ITest)
  protected
    FTestName: string;
    fEnabled: boolean;

  public
    {: Create an instance of a test fixture to run the named test.
    Create a test fixture capabile of running a single testcase.  The test case
    that will be run is the method called <I>MethodName</I>, which must be
    declared in the published section of a sub-class of TTestCase.
    @see <See Interface="ITest"> }
    constructor Create(Name: string);

    {: Get the name of the test.
    @see <See Interface="ITest"> }
    function GetName: string; virtual;

    {: Get the enabled state of the test.
    @see <See Interface="ITest"> }
    function getEnabled: boolean; virtual;

    {: Set the enabled state of the test.
    @see <See Interface="ITest"> }
    procedure setEnabled(value: boolean); virtual;

    {: Count contained tests.
    @see <See Interface="ITest"> }
    function CountTestCases: integer; virtual;

    {: Count the number of contained enabled tests.
    @see <See Interface="ITest"> }
    function CountEnabledTestCases: integer; virtual;

    {: A convenience method to run this test, collecting the results with a
    default TestResult object.
    Run will Create a TTestResult, run this test case and collect any TTestFailure
    into the TTestResult.

    The caller is responsible for freeing the returned TTestResult.

    @returns A TTestResult with the results of running this test. }
    function Run: TTestResult; overload;

    {: Runs a test and collects its result in a TTestResult instance.
       @param testResult the results of running this test are collected into this
       TTestResult object. }
    procedure Run(testResult: TTestResult); overload;

    {: Runs a test and collects its result in a TTestResult instance.
       @param testResult the results of running this test are collected into this
       TTestResult object. }
    procedure RunBare(testResult: TTestResult); virtual; abstract;

    {: Load a previously saved state.
       Load the enabled/disabled state of this and contained tests.
       @param fileName The name of the .INI file to read the configuration from.
       @seeAlso <See Interface="ITest" Method="LoadConfiguration"> }
    procedure LoadConfiguration(const fileName: string; const UseRegistry: boolean); overload;
    {: Load a previously saved state.

       @seeAlso <See Interface="ITest" Method="LoadConfiguration"> }
    procedure LoadConfiguration(const iniFile :TCustomIniFile; const section :string);  overload; virtual;

    {: Save the current state.
       @seeAlso <See Interface="ITest" Method="SaveConfiguration"> }
    procedure SaveConfiguration(const fileName: string; const UseRegistry: boolean); overload;
    {: Save the current state.
       @seeAlso <See Interface="ITest" Method="SaveConfiguration"> }
    procedure SaveConfiguration(const iniFile :TCustomIniFile; const section :string);  overload; virtual;

    {: The list of contained tests.
       @seeAlso <See Interface="ITest" Method="Tests"> }
    function Tests: IInterfaceList; virtual;

    {: The test's name.
       @seeAlso <See Interface="ITest" Method="GetName"> }
    property Name: string read GetName;

    {: The enabled state of the test.
       @seeAlso <See Interface="ITest" Method="GetEnabled">
       @seeAlso <See Interface="ITest" Method="SetEnabled"> }
    property Enabled: boolean read GetEnabled write SetEnabled;
  end;


(***
A test case defines the fixture to run multiple tests. To define a test case<br>
1) implement a subclass of TestCase<br>
2) define instance variables that store the state of the fixture<br>
3) initialize the fixture state by overriding <em>setUp</em><br>
4) clean-up after a test by overriding <em>tearDown</em>.<br>
Each test runs in its own fixture so there
can be no side effects among test runs.
Here is an example:
  <code>
  interface

  type
    TMathTest = class(TTestCase)
    private
      fValue1,
      fValue2: Float;
      procedure testAdd;
    public
      procedure setUp; override;
    end;

  implementation

  procedure TMathTest.setUp;
  begin
    fValue1 := 2.0;
    fValue2 := 3.0;
  end;
  </code>

For each test implement a method which interacts
with the fixture. Verify the expected results with tests specified
by calling <em>check</em> with a boolean and an optional message.
  <code>
  procedure TMathTest.testAdd;
  var
    testResult: Float;
  begin
    testResult := fValue1 + fValue2;
    check(testResult = 5.0);
  end;
  </code>

see TTestResult
see TTestSuite
*)
  ExceptionClass = class of Exception;

  TTestCase = class(TAbstractTest, ITest)
  protected
    fMethod:    TTestMethod;
    fStartTime: Int64;
    fStopTime:  Int64;
    fExpectedException: ExceptionClass;

    {:
    Sets up the fixture, for example, open a network connection.
    This method is used to ensure that state of the application is in a known state
    before running any test cases.
    This method is called before each test is executed in this TTestCase.
    Override this method to add code to setup the preconditions for the testcases
    declared in descendants of TTestCase.
    @seeAlso <See Method="TearDown">
    }
    procedure SetUp; virtual;

    {:
    Tears down the fixture, for example, close a network connection.
    This method is called after each test in this TTestCase is executed.
    Override this method to de-allocate any resources allocated by SetUp.
    @seeAlso <See Method="SetUp">
    }
    procedure TearDown; virtual;

    function BoolToStr(ABool: boolean): string;

    {:
    Checks that a condition is met. Logs a failure using the optional
    message string if not.
    @param condition The condition to check.
    @param msg       An optional message use if the check fails.
    }
    procedure Check(condition: boolean; msg: string = ''); virtual;
    procedure CheckEquals(expected, actual: double; delta: double = 0; msg: string = ''); overload; virtual;
    procedure CheckEquals(expected, actual: integer; msg: string = ''); overload; virtual;
    procedure CheckEquals(expected, actual: string; msg: string = ''); overload; virtual;
    procedure CheckEquals(expected, actual: boolean; msg: string = ''); overload; virtual;

    procedure CheckNotEquals(expected, actual: integer; msg: string = ''); overload; virtual;
    procedure CheckNotEquals(expected: double; actual: double; delta: double = 0; msg: string = ''); overload; virtual;
    procedure CheckNotEquals(expected, actual: string; msg: string = ''); overload; virtual;
    procedure CheckNotEquals(expected, actual: boolean; msg: string = ''); overload; virtual;

    procedure CheckNotNull(obj :IUnknown; msg :string = ''); overload; virtual;
    procedure CheckNull(obj: IUnknown; msg: string = ''); overload; virtual;
    procedure CheckSame(expected, actual: IUnknown; msg: string = ''); overload; virtual;
    procedure CheckSame(expected, actual: TObject; msg: string = ''); overload; virtual;

    procedure CheckNotNull(obj: TObject; msg: string = ''); overload; virtual;
    procedure CheckNull(obj: TObject; msg: string = ''); overload; virtual;

    procedure CheckException(AMethod: TTestMethod; AExceptionClass: TClass; msg :string = '');
    procedure CheckEquals(  expected, actual: TClass; msg: string = ''); overload; virtual;
    procedure CheckInherits(expected, actual: TClass; msg: string = ''); overload; virtual;
    procedure CheckIs(obj :TObject; klass: TClass; msg: string = ''); overload; virtual;

    {: Report a test failure.
    Reports a failure to all TestListeners using the provided message.
    @param msg The optional message.  }
    procedure Fail(msg: string; errorAddr: Pointer = nil); overload; virtual;
    procedure FailEquals(expected, actual: string; msg: string = ''; errorAddr: Pointer = nil); virtual;
    procedure FailNotEquals(expected, actual: string; msg: string = ''; errorAddr: Pointer = nil); virtual;
    procedure FailNotSame(expected, actual: string; msg: string = ''; errorAddr: Pointer = nil); virtual;

    function EqualsErrorMessage(expected, actual, msg: string): string;
    function NotEqualsErrorMessage(expected, actual, msg: string): string;
    function NotSameErrorMessage(expected, actual, msg: string): string;

    {: Stops testing.
    Throws EStopTestsFailure to Stop further testing.
    @param  msg An optional message to attach to the thrown exception.
    @throws EStopTestsFailure
    @seeAlso <See Class="TTestResult">
    }
    procedure StopTests(msg: String = ''); virtual;

    {: Elapsed test time.
       Returns the time elapsed since the test was started. If the
       test already ended, return the total test time.
       @returns The elapsed test time in milliseconds.
    }
    function ElapsedTestTime: Cardinal; virtual;

    {: Create a test suite.
    Creates a test suite composed of the published methods in the current
    TTestCase class.
    @seeAlso <See Routine="TestSuiteOf">
    }
    class function Suite: ITestSuite; virtual;

    {: Checks method is not empty.
    Empty tests should fail as allowing them to pass
    would confuse testing, and allow untested methods.}
    procedure CheckMethodIsNotEmpty(MethodPointer: pointer);
  public

    {:
    Create an instance of a test fixture to run the named test.
    Create a test fixture capabile of running a single testcase.  The test case
    that will be run is the method called <I>MethodName</I>, which must be
    declared in the published section of a sub-class of TTestCase.

    see @TTestSuite
    see @ITestSuite
    }
    constructor Create(MethodName: string); virtual;

    {:
    Makes the given testResult run this test.
    @param testResult Where test results will be accumulated.
    }
    procedure Run(testResult: TTestResult); overload;

    {:
    Runs the test case and collects the results in TestResult.
    This is the template method that defines the control flow
    for running a test case.
    }
    procedure RunBare(testResult: TTestResult); override;

    {:
    Perform the test.
    Performs the test.  The pre-conditions for the test must already have been met.
    runTest may leave the system in an unknown state.  Do not override this function
    to Create testcases.

    @raises ETestFailure The test failed.
    @raises Exception An error occcurred while running this test.
    }
    procedure RunTest; virtual;

    procedure StartExpectingException(e: ExceptionClass);
    procedure StopExpectingException(Msg :string = '');

  published
    // last private/protected/public/published section is the
    // default for descendant classes
    property ExpectedException :ExceptionClass
      read  fExpectedException
      write StartExpectingException;
  end;


  {: A TTestFailure collects a failed test together with information about the exception
  which caused the test to fail.

  TTestCase descendants do not need to manipulate TTestFailures directly.
  TTestFailures are used by TTestResult to keep track of the results of running
  a set of tests.  TTestFailure is used by the test runners to report test failures.

  see TTestResult
  }
  TTestFailure = class(TObject)
  protected
    fFailedTest: ITest;
    fThrownExceptionClass: TClass;
    fThrownExceptionMessage: string;
    FThrownExceptionAddress: Pointer;
  public
    {: Constructs a record of a failed test.
    Constructs a TTestFailure with the given test and the exception which occurred
    while running that test.

    see TTestResult.AddError
    see TTestResult.AddFailure
    }
    constructor Create(failedTest: ITest; thrownException: Exception; Addr: Pointer; msg: string = '');

    {: Returns the test which failed.

    see ITest
    }
    function FailedTest: ITest; virtual;

    {: Returns the class of the exception which triggered the failure of this test.
    }
    function ThrownExceptionClass: TClass; virtual;

    {: Returns the name of the exception which triggered the failure of this test.
    }
    function ThrownExceptionName: string; virtual;

    {: Returns the message from the exception which triggered the failure of this test.
    }
    function ThrownExceptionMessage: string; virtual;

    {: Returns address of the exception which triggered the failure of this test.
    }
    function ThrownExceptionAddress: pointer; virtual;

    {: Returns the location info of the exception which triggered the failure of this test.
    }
    function LocationInfo: string; virtual;
    function AddressInfo:  string; virtual;
  end;

  {:
  A TTestSuite is a <em>Composite</em> of Tests, implementing the ITestSuite interface.
  It runs a collection of test cases.

  It should not be necessary for test writers to extend TTestSuite.  Decorators
  should be used instead.

  see TTestDecorator.
  see RegisterTest
  see ITest
  }
  TTestSuite = class(TAbstractTest, ITestSuite, ITest)
  protected
    fTests: IInterfaceList;
  public
    {:
    Constructs an empty TestSuite.
    Constructs an empty TestSuite with the same name as the class.
    }
    constructor Create; overload;

    {: Constructs an empty TestSuite.
    Constructs an empty Testsuite called <em>name</em>.
    @param Name The name of the test suite.
    }
    constructor Create(Name: string); overload;

    {: Creates and populates a test suite.
       Creates a TestSuite that contains a test for each published method
       of the give TestCase class.
    @param  TestClass The TTestCase class descendant from which to extract the test methods.
    @seeAlso <See Routine="RegisterTest">
    }
    constructor Create(TestClass: TTestCaseClass); overload;

    {: Creates and populates a test suite.
       Creates a tests suite containing the tests passed in the array parameter.
       @param Name The name of the test suite.
       @param Tests The tests to add to the suite. }
    constructor Create(Name: string; const Tests: array of ITest); overload;

    {: Counts the number of test cases in the suite and sub-suites.
	   @return The number of test cases. }
    function CountTestCases: integer;         override;

    {: Counts the number of enabled test cases in the suite and sub-suites.
	   @return The number of enabled test cases. }
    function CountEnabledTestCases: integer;  override;

    {: Runs the tests in the suite.
       @param TestResult Where to accumulate the test results.
       @seeAlso <See Method="Itest.RunBare"> }
    procedure RunBare(testResult: TTestResult); override;

    {: Add a test to the suite.
       @param ATest The test to add. }
    procedure AddTest(ATest: ITest); virtual;

    {: Adds a test suite to the suite.
       @param suite The suite to add. }
    procedure AddSuite(suite:  ITestSuite); virtual;

    {: Makes a test instance out of each published method of the given testClass
       and adds them to the suite.
       @param testClass The testCase class with published test methods. }
    procedure AddTests(testClass: TTestCaseClass); virtual;

    {: Returns the list of tests contained in this suite
       @returns The contained list of tests, each of type ITest.
       @seeAlso <See Method="Itest.Tests">}
    function Tests: IInterfaceList; override;

    {: Load a previously saved configuration.
       The enabled/disabled state for each contained test will be read
       from the given TIniFile under the given section name.
       @param iniFile The TIniFile to read the configuration from.
       @param section The name of the section the configuration will be read from. }
    procedure LoadConfiguration(const iniFile: TCustomIniFile; const section: string);  override;

    {: Save the current test configuration.
       The enabled/disabled state of each contained test will be saved
       to the given TIniFile under the given section name.
       @param iniFile The TIniFile to save the configuration to.
       @param section The name of the section the configuration will be saved to. }
    procedure SaveConfiguration(const iniFile: TCustomIniFile; const section: string);  override;
  end;

  {: Enumerates the published methods of a class.
     This class is used by the framework to make the creation of test
     suites easy. Users of the framework should never have to use this class
     directly
     @SeeAlso <See Class="TTestCase">
     @SeeAlso <See Class="TTestSuite" Method="Create">
  }
  TMethodEnumerator = class
  protected
    FMethodNameList:  array of string;
    function GetNameOfMethod(Index: integer):  string;
    function GetMethodCount: Integer;
  public
    constructor Create(AClass: TClass);
    property MethodCount: integer read GetMethodCount;
    property NameOfMethod[index:  integer]: string read GetNameOfMethod;
  end;

{: Create a test suite out of the published methods in a TTestCase descendant
   @param aclass The TTestCase class to get the test methods from.
   @deprecated Use TTestCase.Suite instead.
   @seeAlso <See Class="TTestCase" Method="Suite"> }
function  TestSuiteOf(AClass: TTestCaseClass): ITestSuite;

{: Create a named test suite out a series of tests
   @param name  The name the ITestSuite will have.
   @param Tests The series of tests.
   @deprecated Use TTestSuite.Create(string, array of ITest) instead
   @seeAlso <See Class="TTestSuite" Method="Create"> }
function  MakeTestSuite(name: string; const Tests: array of ITest): ITestSuite;

{: Create a test suite hierarchy out of a series of TTestCase descendants
   @param name  The name the ITestSuite will have.
   @param classes The series of TTestCase classes that will compose the suite.
   @deprecated Use TTestCase.Create(string, array of ITest) instead
   @seeAlso <See Class="TTestSuite" Method="Create">
   @seeAlso <See Class="TTestCase" Method="Suite"> }
function  MakeTestSuites(name: string; const classes: array of TTestCaseClass): ITestSuite;

{:
Register a test.
Add a test to an arbitraty test suite in the test hierarchy. The SuitePath is a dot-separated
list of suite names. The first name is taken to be a top level suite, the second a child of the
first, and so on. Test suites are created if they don't exist. The test is added to the suite
indicated by the last names.
@param SuitePath Dot separated list of test suite names, indicating a path from the root of the
                 test hierarchy. Tests are added to the suite indicated by the last name.
@param test The test to register.
}
procedure RegisterTest(SuitePath: string; test: ITest); overload;

{:
Create and register a test suite consisting of each published method of a TTestcase
descendant.
@param path Indicates a path of tests suites to register the test under.
@param suiteTemplate The TTestCase descendant that forms a template for a test suite.
@deprecated Use RegisterTest(string, ITest) instead in combination with TTestCase.Suite.
@seeAlso <See Class="TTestCase" Method="Suite"> }
procedure RegisterTest(SuitePath: string; suiteTemplate: TTestCaseClass); overload;

{:
Register a test.
Add a test to to the root of the test hierarchy.
@param test The test to register.
}
procedure RegisterTest(test: ITest); overload;
{:
Create and register a test suite consisting of each published method of a TTestcase
descendant.
@param suiteTemplate The TTestCase descendant that forms a template for a test suite.
@deprecated Use RegisterTest(string, ITest) instead in combination with TTestCase.Suite.
@seeAlso <Ssee Class="TTestCase" Method="Suite"> }
procedure RegisterTest(suiteTemplate: TTestCaseClass); overload;

{: Register a series of tests under the given path.
@param path Indicates a path of tests suites to register the test under.
@param Tests The series of tests to register. }
procedure RegisterTests(SuitePath: string; const Tests: array of ITest);

{: Register a series of test suites consisting of the published methods of the
   TTestCase descendants passed in the classes array.
   @param path Indicates a path of tests suites to register the test under.
   @param Tests The series of tests to register.
   @deprecated Use RegisterTest(string, array of ITest) instead, in combination
   with TTestCase.Suite, and TTestSuite.Create.
   @seeAlso <See Class="TTestCase" Method="Suite">
   @seeAlso <See Class="TTestSuite" Method="Create"> }
procedure RegisterTestSuites(SuitePath: string; const classes: array of TTestCaseClass);

{: Return a TestSuite containing all registered tests.
   @returns A TestSuite with all registered tests. }
function RegisteredTests: ITestSuite;

{: Run a test using default features and notifying the given set of
   listeners of testing events.
   @param suite The test to run.
   @listeners An array of listeners that will receive testing events.
}
function RunTest(suite: ITest; listeners: array of ITestListener): TTestResult; overload;

{: Run the test methods in a TTEstCaseClass using default features
   and notifying the given set of listeners of testing events.
   @deprecated Use RunTestt(ITest, array of IListener) instead, in combination
   with TTestCase.Suite.
   @seeAlso <See Class="TTestCase" Method="Suite"> }
function RunTest(aclass: TTestCaseClass; listeners: array of ITestListener): TTestResult; overload;

{: Run registered tests using default features
   and notifying the given set of listeners of testing events.
}
function RunRegisteredTests(listeners: array of ITestListener): TTestResult;

procedure ClearRegistry;

{: Returns the address that the calling procedure will return to.
   Used internally by the framework. Don't use directly. }

function CallerAddr: Pointer; assembler;

{: Convert a pointer into its string representation }
function PtrToStr(p: Pointer): string;

function PointerToLocationInfo(Addr: Pointer): string;
function PointerToAddressInfo(Addr: Pointer):  string;

///////////////////////////////////////////////////////////////////////////
implementation
uses
	Windows, Registry, TypInfo{$IFDEF USE_JEDI_JCL}, JclDebug{$ENDIF};


{$STACKFRAMES ON} //required retreive caller's address

{: Convert a pointer into its string representation }
function PtrToStr(p: Pointer): string;
begin
   Result := Format('%p', [p])
end;

function IsBadPointer(P: Pointer):boolean; register;
begin
  try
    Result  := (p = nil)
              or ((Pointer(P^) <> P) and (Pointer(P^) = P));
  except
    Result := false
  end
end;


function CallerAddr: Pointer; assembler;
const
  CallerIP = $4;
asm
   mov   eax, ebp
   call  IsBadPointer
   test  eax,eax
   jne   @@Error

   mov   eax, [ebp].CallerIP
   sub   eax, 5   // 5 bytes for call

   push  eax
   call  IsBadPointer
   test  eax,eax
   pop   eax
   je    @@Finish

@@Error:
   xor eax, eax
@@Finish:
end;

{$IFNDEF USE_JEDI_JCL}
function PointerToLocationInfo(Addr: Pointer): string;
begin
 Result := ''
end;

function PointerToAddressInfo(Addr: Pointer): string;
begin
 Result := '$'+PtrToStr(Addr);
end;

{$ELSE}
function PointerToLocationInfo(Addr: Pointer): string;
var
  _file,
  _module,
  _proc: AnsiString;
  _line: integer;
begin
  JclDebug.MapOfAddr(Addr, _file, _module, _proc, _line);

  Result   := Format('%s:%d', [_file, _line]);
end;

function PointerToAddressInfo(Addr: Pointer): string;
var
  _file,
  _module,
  _proc: AnsiString;
  _line: integer;
begin
  JclDebug.MapOfAddr(Addr, _file, _module, _proc, _line);
  Result := Format('%s$%p', [_proc, Addr]);
end;
{$ENDIF}


{ TTestResult }

constructor TTestResult.Create;
begin
  inherited Create;
  fFailures := TList.Create;
  fErrors := TList.Create;
  fListeners := TInterfaceList.Create;
  fRunTests := 0;
  fStop := false;
end;

destructor TTestResult.destroy;
var
  i: Integer;
begin
  fListeners := nil;
  for i := 0 to fErrors.Count - 1 do
  begin
    TTestFailure(fErrors[i]).Free;
  end;
  fErrors.Free;
  for i := 0 to fFailures.Count - 1 do
  begin
    TTestFailure(fFailures[i]).Free;
  end;
  fFailures.Free;
  inherited Destroy;
end;

procedure TTestResult.AddSuccess(test: ITest);
var
  i: integer;
begin
  assert(assigned(test));
  for i := 0 to fListeners.count - 1 do
  begin
    (fListeners[i] as ITestListener).AddSuccess(test);
  end;
end;

function TTestResult.AddError(test: ITest; e: Exception; addr: Pointer; msg: string): TTestFailure;
var
  i: integer;
  error:  TTestFailure;
begin
  assert(assigned(test));
  assert(assigned(e));
  assert(assigned(fErrors));

  error := TTestFailure.Create(test, e, addr, msg);
  fErrors.add(error);
  for i := 0 to fListeners.count - 1 do
  begin
    (fListeners[i] as ITestListener).AddError(error);
  end;

  assert(assigned(error));
  Result := error;
end;

function TTestResult.AddFailure(test: ITest; e: Exception; addr: Pointer): TTestFailure;
var
  i: integer;
  Failure:  TTestFailure;
begin
  assert(assigned(test));
  assert(assigned(e));
  assert(assigned(fFailures));

  Failure := TTestFailure.Create(test, e, addr);
  fFailures.add(Failure);
  for i := 0 to fListeners.count - 1 do
  begin
    (fListeners[i] as ITestListener).AddFailure(Failure);
  end;

  assert(assigned(Failure));
  Result := Failure;
end;

procedure TTestResult.addListener(listener: ITestListener);
begin
  assert(assigned(listener), 'listener is nil');
  fListeners.add(listener);
end;

procedure TTestResult.EndTest(test: TTestCase);
var
  i: integer;
begin
  assert(assigned(fListeners));

  for i := 0 to fListeners.count - 1 do
  begin
    (fListeners[i] as ITestListener).EndTest(test);
  end;
end;

function TTestResult.errors: TList;
begin
  result := fErrors;
end;

function TTestResult.Failures: TList;
begin
  result := fFailures;
end;

function TTestResult.RunTestSetup(test: TTestCase):boolean;
begin
  try
    test.fStopTime  := 0;
    QueryPerformanceCounter(test.fStartTime);
    test.SetUp;
    Result := true;
  except
    on e: Exception do
    begin
      AddError(test, e, ExceptAddr, 'SetUp FAILED: ');
      Result := false;
    end
  end;
end;

procedure TTestResult.RunTestTearDown(test: TTestCase);
begin
  try
    test.TearDown;
  except
    on e: Exception do
      AddError(test, e, ExceptAddr, 'TearDown FAILED: ');
  end;
  QueryPerformanceCounter(test.fStopTime);
end;

procedure TTestResult.RunTestRun(test: TTestCase);
var
  failure: TTestFailure;
begin
  failure := nil;
  try
    test.RunTest;
    fTotalTime := test.ElapsedTestTime + fTotalTime;
    AddSuccess(test);
  except
    on e: EStopTestsFailure do
    begin
      failure := AddFailure(test, e, ExceptAddr);
      FStop := True;
    end;
    on e: ETestFailure do
    begin
      failure := AddFailure(test, e, ExceptAddr);
    end;
    on e: EBreakingTestFailure do
    begin
      failure := AddFailure(test, e, ExceptAddr);
    end;
    on e: Exception do
    begin
      failure := AddError(test, e, ExceptAddr);
    end;
  end;
  if BreakOnFailures
  and (failure <> nil)
  and (failure.FThrownExceptionClass.InheritsFrom(ETestFailure))
  then
  begin
    try
       raise EBreakingTestFailure.Create(failure.ThrownExceptionMessage)
          at failure.ThrownExceptionAddress;
    except
    end;
  end;
end;

procedure TTestResult.Run(test: TTestCase);
begin
  assert(assigned(test));
  if not ShouldStop then
  begin
    StartTest(test);
    try
      if RunTestSetUp(test) then
      begin
        RunTestRun(test);
      end;
      RunTestTearDown(test);
    finally
      EndTest(test);
    end;
  end;
end;

function TTestResult.RunCount: integer;
begin
  result := fRunTests;
end;

function TTestResult.ShouldStop: boolean;
begin
  result := fStop;
end;

procedure TTestResult.StartTest(test: TTestCase);
var
  i: integer;
begin
  assert(assigned(test));
  assert(assigned(fListeners));

  for i := 0 to fListeners.count - 1 do
  begin
    (fListeners[i] as ITestListener).StartTest(test);
  end;
  inc(fRunTests);
end;

procedure TTestResult.Stop;
begin
  fStop := true;
end;

function TTestResult.ErrorCount: integer;
begin
  assert(assigned(fErrors));

  result := fErrors.count;
end;

function TTestResult.FailureCount: integer;
begin
  assert(assigned(fFailures));

  result := fFailures.count;
end;

function TTestResult.WasSuccessful: boolean;
begin
  result := (FailureCount = 0) and (ErrorCount() = 0) and not WasStopped;
end;

procedure TTestResult.TestingStarts;
var
  i: Integer;
begin
  for i := 0 to fListeners.count - 1 do
  begin
    (fListeners[i] as ITestListener).TestingStarts;
  end;
end;

procedure TTestResult.TestingEnds;
var
  i: Integer;
begin
  for i := 0 to fListeners.count - 1 do
  begin
    (fListeners[i] as ITestListener).TestingEnds(self);
  end;
end;

function TTestResult.WasStopped: boolean;
begin
  result := fStop;
end;

procedure TTestResult.RunSuite(test: TAbstractTest);
begin
  TestingStarts;
  try
    test.RunBare(self);
  finally
    TestingEnds
  end
end;

{ TAbstractTest }

constructor TAbstractTest.Create(Name: string);
begin
  inherited Create;
  FTestName := Name;
  FEnabled  := true;
end;

procedure TAbstractTest.Run(testResult: TTestResult);
begin
  testResult.RunSuite(self);
end;

function TAbstractTest.CountEnabledTestCases: integer;
begin
  if GetEnabled then
    Result := 1
  else
    Result := 0
end;

function TAbstractTest.CountTestCases: integer;
begin
  Result := 1;
end;

function TAbstractTest.getEnabled: boolean;
begin
  Result := fEnabled
end;

function TAbstractTest.GetName: string;
begin
  Result := fTestName
end;

procedure TAbstractTest.LoadConfiguration(const fileName: string; const UseRegistry: boolean);
var
  f: TCustomIniFile;
begin
  if UseRegistry then
    f := TRegistryIniFile.Create(fileName)
  else
    f := TIniFile.Create(fileName);

  try
    LoadConfiguration(f, 'Tests')
  finally
    f.free
  end
end;

procedure TAbstractTest.LoadConfiguration(const iniFile: TCustomIniFile; const section: string);
begin
  self.setEnabled(iniFile.readBool(section, self.GetName, True));
end;

procedure TAbstractTest.SaveConfiguration(const fileName: string; const UseRegistry: boolean);
var
  f: TCustomIniFile;
begin
  if UseRegistry then
    f := TRegistryIniFile.Create(fileName)
  else
    f := TIniFile.Create(fileName);

  try
    SaveConfiguration(f, 'Tests')
  finally
    f.free
  end
end;

procedure TAbstractTest.SaveConfiguration(const iniFile: TCustomIniFile; const section: string);
begin
  iniFile.writeBool(section, self.GetName, self.getEnabled);
end;

function TAbstractTest.Run: TTestResult;
var
  testResult:  TTestResult;
begin
  testResult := TTestResult.Create;
  try
    testResult.RunSuite(self);
  except
    testResult.Free;
    raise;
  end;
  Result := testResult;
end;

procedure TAbstractTest.setEnabled(value: boolean);
begin
  fEnabled := value;
end;

var
  EmptyTestList: IInterfaceList = nil;

function TAbstractTest.Tests: IInterfaceList;
begin
   if EmptyTestList = nil then
     EmptyTestList := TInterfaceList.Create;
   Result := EmptyTestList;
end;


{ TTestCase }

constructor TTestCase.Create(MethodName: string);
var
  RunMethod: TMethod;
begin
  assert(length(MethodName) >0);
  assert(assigned(MethodAddress(MethodName)));

  inherited Create(MethodName);
  RunMethod.code := MethodAddress(MethodName);
  RunMethod.Data := self;
  fMethod := TTestMethod(RunMethod);

  assert(assigned(fMethod));
end;

procedure TTestCase.RunBare(testResult: TTestResult);
begin
  assert(assigned(testResult));

  if getEnabled then
  begin
    testResult.Run(self);
  end;
end;

procedure TTestCase.RunTest;
begin
  assert(assigned(fMethod), 'Method "' + FTestName + '" not found');
  fExpectedException := nil;
  try
    CheckMethodIsNotEmpty(tMethod(fMethod).Code);
    fMethod;
  except
    on E: Exception  do
    begin
      if  not Assigned(fExpectedException) then
        raise
      else if not E.ClassType.InheritsFrom(fExpectedException) then
         FailNotEquals(fExpectedException.ClassName, E.ClassName, 'unexpected exception', ExceptAddr)
      else
        fExpectedException := nil;
    end
  end;
  StopExpectingException;
end;

procedure TTestCase.SetUp;
begin
end;

procedure TTestCase.TearDown;
begin
end;

procedure TTestCase.Check(condition: boolean; msg: string);
begin
    if (not condition) then
        Fail(msg, CallerAddr);
end;

procedure TTestCase.Fail(msg: string; errorAddr: Pointer = nil);
begin
  if errorAddr = nil then
    raise ETestFailure.Create(msg) at CallerAddr
  else
    raise ETestFailure.Create(msg) at errorAddr;
end;

procedure TTestCase.StopTests(msg: String);
begin
  raise EStopTestsFailure.Create(msg);
end;

procedure TTestCase.Run(testResult: TTestResult);
begin
  testResult.RunSuite(self);
end;

procedure TTestCase.FailNotEquals( expected,
                                   actual   : string;
                                   msg      : string = '';
                                   errorAddr: Pointer = nil);
begin
    Fail(notEqualsErrorMessage(expected, actual, msg), errorAddr);
end;

procedure TTestCase.FailEquals( expected,
                                actual   : string;
                                msg      : string = '';
                                errorAddr: Pointer = nil);
begin
    Fail(EqualsErrorMessage(expected, actual, msg), errorAddr);
end;

procedure TTestCase.FailNotSame( expected,
                                 actual   : string;
                                 msg      : string = '';
                                 errorAddr: Pointer = nil);
begin
    Fail(NotSameErrorMessage(expected, actual, msg), errorAddr);
end;

procedure TTestCase.CheckEquals( expected,
                                 actual   : double;
                                 delta    : double = 0;
                                 msg      : string = '');
begin
    if (abs(expected-actual) > delta) then
        FailNotEquals(FloatToStr(expected), FloatToStr(actual), msg, CallerAddr);
end;

procedure TTestCase.CheckNotNull(obj: IUnknown; msg: string);
begin
    if obj = nil then
      Fail(msg, CallerAddr);
end;

procedure TTestCase.CheckNull(obj: IUnknown; msg: string);
begin
    if obj <>  nil then
      Fail(msg, CallerAddr);
end;

procedure TTestCase.CheckSame(expected, actual: IUnknown; msg: string = '');
begin
    if (expected <> actual) then
      FailNotSame(PtrToStr(Pointer(expected)), PtrToStr(Pointer(actual)), msg, CallerAddr);
end;

procedure TTestCase.CheckEquals(expected, actual: string; msg: string = '');
begin
   if expected <> actual then begin
      FailNotEquals(expected, actual, msg, CallerAddr);
   end
end;

procedure TTestCase.CheckNotEquals(expected, actual: string; msg: string = '');
begin
   if expected = actual then begin
      FailEquals(expected, actual, msg, CallerAddr);
   end
end;

procedure TTestCase.CheckEquals(expected, actual: integer; msg: string);
begin
  if (expected <> actual) then
    FailNotEquals(IntToStr(expected), IntToStr(actual), msg, CallerAddr);
end;

procedure TTestCase.CheckNotEquals(expected, actual: integer; msg: string = '');
begin
  if expected = actual then
    FailEquals(IntToStr(expected), IntToStr(actual), msg, CallerAddr);
end;

procedure TTestCase.CheckNotEquals(expected: double; actual: double; delta: double = 0; msg: string = '');
begin
    if (abs(expected-actual) <= delta) then
        FailNotEquals(FloatToStr(expected), FloatToStr(actual), msg, CallerAddr);
end;

procedure TTestCase.CheckEquals(expected, actual: boolean; msg: string);
begin
  if (expected <> actual) then
    FailNotEquals(BoolToStr(expected), BoolToStr(actual), msg, CallerAddr);
end;

procedure TTestCase.CheckNotEquals(expected, actual: boolean; msg: string);
begin
  if (expected = actual) then
    FailEquals(BoolToStr(expected), BoolToStr(actual), msg, CallerAddr);
end;

procedure TTestCase.CheckSame(expected, actual: TObject; msg: string);
begin
    if (expected <> actual) then
      FailNotSame(PtrToStr(Pointer(expected)), PtrToStr(Pointer(actual)), msg, CallerAddr);
end;

procedure TTestCase.CheckNotNull(obj: TObject; msg: string);
begin
    if obj = nil then
       FailNotSame('object', PtrToStr(Pointer(obj)), msg, CallerAddr);
end;

procedure TTestCase.CheckNull(obj: TObject; msg: string);
begin
    if obj <> nil then
       FailNotSame('nil', PtrToStr(Pointer(obj)), msg, CallerAddr);
end;

function TTestCase.NotEqualsErrorMessage(expected, actual: string; msg: string): string;
begin
    if (msg <> '') then
        msg := msg + ', ';
    Result := Format('%sexpected: <%s> but was: <%s>', [msg, expected, actual])
end;

function TTestCase.EqualsErrorMessage(expected, actual: string; msg: string): string;
begin
    if (msg <> '') then
        msg := msg + ', ';
    Result := Format('%sexpected and actual were: <%s>', [msg, expected])
end;

function TTestCase.NotSameErrorMessage(expected, actual, msg: string): string;
begin
    if (msg <> '') then
        msg := msg + ', ';
    Result := Format('%sexpected: <%s> but was: <%s>', [msg, expected, actual])
end;

class function TTestCase.Suite: ITestSuite;
begin
  Result := TestSuiteOf(Self);
end;

function TTestCase.ElapsedTestTime: Cardinal;
var
  Freq, Time: Int64;
begin
// returns TestTime in millisecs
  if fStopTime > 0 then
    Time := fStopTime
  else
    QueryPerformanceCounter(Time);
  Time := Time - fStartTime;
  if QueryPerformanceFrequency(Freq) then
    Result := (1000*Time) div Freq
  else
    Result := 0;
end;

function TTestCase.BoolToStr(ABool: boolean): string;
begin
	Result := BooleanIdents[aBool];
end;

procedure TTestCase.StartExpectingException(e: ExceptionClass);
begin
  StopExpectingException;
  fExpectedException := e;
end;

procedure TTestCase.StopExpectingException(Msg :string);
begin
  if fExpectedException <> nil then
    raise ETestFailure.Create(Format('Expected exception <%s> but there was none. %s', [fExpectedException.ClassName, Msg]));
end;

procedure TTestCase.CheckMethodIsNotEmpty(MethodPointer: pointer);
const
  AssemblerRet = $C3;
begin
  if byte(MethodPointer^) = AssemblerRet then
    fail('Empty test', MethodPointer);
end;

procedure TTestCase.CheckException(AMethod: TTestMethod; AExceptionClass: TClass; msg :string);
begin
  try
    AMethod;
  except
    on e :Exception do
    begin
      if  not Assigned(AExceptionClass) then
        raise
      else if not e.ClassType.InheritsFrom(AExceptionClass) then
        FailNotEquals(AExceptionClass.ClassName, e.ClassName, msg, CallerAddr)
      else
        AExceptionClass := nil;
    end;
  end;
  if Assigned(AExceptionClass) then
    FailNotEquals(AExceptionClass.ClassName, 'nothing', msg, CallerAddr)
end;

procedure TTestCase.CheckEquals(expected, actual: TClass; msg: string);
begin
 if expected <> actual then
 begin
   if expected = nil then
     FailNotEquals('nil', actual.ClassName, msg, CallerAddr)
   else if actual = nil then
     FailNotEquals(expected.ClassName, 'nil', msg, CallerAddr)
   else
     FailNotEquals(expected.ClassName, actual.ClassName, msg, CallerAddr)
 end;
end;

procedure TTestCase.CheckInherits(expected, actual: TClass; msg: string);
begin
 if expected = nil then
   FailNotEquals('nil', actual.ClassName, msg, CallerAddr)
 else if actual = nil then
   FailNotEquals(expected.ClassName, 'nil', msg, CallerAddr)
 else if not actual.InheritsFrom(expected) then
   FailNotEquals(expected.ClassName, actual.ClassName, msg, CallerAddr)
end;

procedure TTestCase.CheckIs(obj: TObject; klass: TClass; msg: string);
begin
 Assert(klass <> nil);
 if obj = nil then
   FailNotEquals('nil', klass.ClassName, msg, CallerAddr)
 else if obj.ClassType.InheritsFrom(klass) then
   FailNotEquals(obj.ClassName, klass.ClassName, msg, CallerAddr)
end;

{ TTestFailure }

constructor TTestFailure.Create(FailedTest: ITest; thrownException: Exception; Addr: Pointer; msg: string);
begin
  assert(assigned(thrownException));

  inherited Create;
  fFailedTest := FailedTest;
  fThrownExceptionClass := thrownException.ClassType;
  fThrownExceptionMessage := msg + thrownException.message;
  FThrownExceptionAddress := Addr;
end;

function TTestFailure.FailedTest: ITest;
begin
  result := fFailedTest;
end;

function TTestFailure.ThrownExceptionName: string;
begin
  result := fThrownExceptionClass.ClassName;
end;

function TTestFailure.ThrownExceptionMessage: string;
begin
  result := fThrownExceptionMessage;
end;

function TTestFailure.ThrownExceptionAddress: pointer;
begin
  Result := FThrownExceptionAddress;
end;

function TTestFailure.ThrownExceptionClass: TClass;
begin
  Result := FThrownExceptionClass;
end;

function TTestFailure.LocationInfo: string;
begin
  Result := PointerToLocationInfo(ThrownExceptionAddress);
end;

function TTestFailure.AddressInfo: string;
begin
  Result := PointerToAddressInfo(ThrownExceptionAddress);
end;

{ TTestSuite }

constructor TTestSuite.Create;
begin
  self.Create(self.ClassName);
end;

constructor TTestSuite.Create(name: string);
begin
  assert(length(name) > 0);

  inherited Create(name);

  fTests := TInterfaceList.Create;
end;

constructor TTestSuite.Create( testClass: TTestCaseClass);
begin
  self.Create(testClass.ClassName);
  AddTests(testClass);
end;

constructor TTestSuite.Create(Name: string; const Tests: array of ITest);
var
  i: Integer;
begin
  self.Create(Name);
  for i := Low(Tests) to High(Tests) do begin
    Self.addTest(Tests[i])
  end;
end;

procedure TTestSuite.AddTest(ATest: ITest);
begin
  Assert(Assigned(ATest));

  fTests.Add(ATest);
end;

procedure TTestSuite.AddSuite(suite: ITestSuite);
begin
  AddTest(suite);
end;


procedure TTestSuite.AddTests(testClass: TTestCaseClass);
var
  MethodIter     :  Integer;
  NameOfMethod   :  string;
  MethodEnumerator:  TMethodEnumerator;
begin
  { call on the method enumerator to get the names of the test
    cases in the testClass }
  MethodEnumerator := nil;
  try
    MethodEnumerator := TMethodEnumerator.Create(testClass);
    { make sure we add each test case  to the list of tests }
    for MethodIter := 0 to MethodEnumerator.Methodcount-1 do
      begin
        NameOfMethod := MethodEnumerator.nameOfMethod[MethodIter];
        self.addTest(testClass.Create(NameOfMethod) as ITest);
      end;
  finally
    MethodEnumerator.free;
  end;
end;

function TTestSuite.CountTestCases: integer;
var
  test: ITest;
  i: Integer;
  Total:  integer;
begin
  assert(assigned(fTests));

  Total := 0;
  for i := 0 to fTests.Count - 1 do
  begin
    test := fTests[i] as ITest;
    Total := Total + test.CountTestCases;
  end;
  Result := Total;
end;

function TTestSuite.CountEnabledTestCases: integer;
var
  i: Integer;
  test: ITest;
  Total:  Integer;
begin
  assert(assigned(fTests));

  Total := 0;
  if getEnabled then
  begin
    for i := 0 to fTests.Count - 1 do
    begin
      test := fTests[i] as ITest;
      Total := Total + test.CountEnabledTestCases;
    end;
  end;
  Result := Total;
end;

procedure TTestSuite.RunBare(testResult: TTestResult);
var
  i: Integer;
  test: ITest;
begin
  assert(assigned(testResult));
  assert(assigned(fTests));

  if getEnabled then
  begin
    for i := 0 to fTests.Count - 1 do
    begin
      if testResult.ShouldStop then
        BREAK;
      test := fTests[i] as ITest;
      test.RunBare(testResult);
    end;
  end;
end;

function TTestSuite.Tests: IInterfaceList;
begin
  result := fTests;
end;

procedure TTestSuite.LoadConfiguration(const iniFile: TCustomIniFile; const section: string);
var
  i    : integer;
  Tests: IInterfaceList;
begin
  inherited LoadConfiguration(iniFile, section);
  Tests := self.Tests;
  for i := 0 to Tests.count-1 do
    (Tests[i] as ITest).LoadConfiguration(iniFile, section + '.' + self.GetName);
end;

procedure TTestSuite.SaveConfiguration(const iniFile: TCustomIniFile; const section: string);
var
  i    : integer;
  Tests: IInterfaceList;
begin
  inherited SaveConfiguration(iniFile, section);
  Tests := self.Tests;
  for i := 0 to Tests.count-1 do
    (Tests[i] as ITest).SaveConfiguration(iniFile, section + '.' + self.GetName);
end;


{ ETestFailure }

constructor ETestFailure.Create;
begin
   inherited Create('')
end;

constructor ETestFailure.Create(msg: string);
begin
   inherited Create(msg)
end;

{ EBreakingTestFailure }

constructor EBreakingTestFailure.Create;
begin
   inherited Create('')
end;

constructor EBreakingTestFailure.Create(msg: string);
begin
   inherited Create(msg)
end;

{ TMethodEnumerator }

constructor TMethodEnumerator.Create(AClass: TClass);
type
  TMethodTable = packed record
    count: SmallInt;
  //[...methods...]
  end;
var
  table: ^TMethodTable;
  name:  ^ShortString;
  i, j:  Integer;
begin
  inherited Create;
  while aclass <> nil do
  begin
    // *** HACK ALERT *** !!!
    // Review System.MethodName to grok how this method works
    asm
      mov  EAX, [aclass]
      mov  EAX,[EAX].vmtMethodTable { fetch pointer to method table }
      mov  [table], EAX
    end;
    if table <> nil then
    begin
      name := Pointer(PChar(table) + 8);
      for i := 1 to table.count do
      begin
        // check if we've seen the method name
        j := Low(FMethodNameList);
        while (j <= High(FMethodNameList)) 
        and (name^ <> FMethodNameList[j]) do
          inc(j);
        // if we've seen the name, then the method has probably been overridden
        if j <= High(FMethodNameList) then
          CONTINUE;
        SetLength(FMethodNameList,length(FMethodNameList)+1);
        FMethodNameList[j] := name^;
        name := Pointer(PChar(name) + length(name^) + 7)
      end;
    end;
    aclass := aclass.ClassParent;
  end;
end;

function TMethodEnumerator.GetMethodCount: Integer;
begin
  Result := Length(FMethodNameList);
end;

function TMethodEnumerator.GetNameOfMethod(Index: integer): string;
begin
  Result := FMethodNameList[Index];
end;

{ Convenience routines }

var
  __TestRegistry: ITestSuite = nil;

function TestSuiteOf(AClass: TTestCaseClass): ITestSuite;
begin
  Result := TTestSuite.Create(AClass)
end;

function  MakeTestSuite(name: string; const Tests: array of ITest): ITestSuite;
begin
   result := TTestSuite.Create(name, Tests);
end;

function  MakeTestSuites(name: string; const classes: array of TTestCaseClass): ITestSuite;
var
  i: Integer;
begin
   result := TTestSuite.Create(name);
   for i := Low(classes) to High(classes) do begin
      result.addTest(testSuiteOf(classes[i]));
   end;
end;

procedure RegisterTestInSuite(rootSuite: ITestSuite; path: string; test: ITest);
var
  pathRemainder:  String;
  suiteName:  String;
  targetSuite:  ITestSuite;
  suite:  ITestSuite;
  currentTest:  ITest;
  Tests:  IInterfaceList;
  dotPos:  Integer;
  i: Integer;
begin
  if (path = '') then
  begin
    // End any recursion
    rootSuite.addTest(test);
  end
  else
  begin
    // Split the path on the dot (.)
    dotPos := Pos('.', Path);
    if (dotPos <= 0) then dotPos := Pos('\', Path);
    if (dotPos <= 0) then dotPos := Pos('/', Path);
    if (dotPos > 0) then
    begin
      suiteName := Copy(path, 1, dotPos - 1);
      pathRemainder := Copy(path, dotPos + 1, length(path) - dotPos);
    end
    else
    begin
      suiteName := path;
      pathRemainder := '';
    end;
    Tests := rootSuite.Tests;

    // Check to see if the path already exists
    targetSuite := nil;
    Tests := rootSuite.Tests;
    for i := 0 to Tests.count -1 do
    begin
      currentTest := Tests[i] as ITest;
      currentTest.queryInterface(ITestSuite, suite);
      if suite <> nil then
      begin
        if (currentTest.GetName = suiteName) then
        begin
          targetSuite := suite;
          break;
        end;
      end;
    end;

    if not assigned(targetSuite) then
    begin
      targetSuite := TTestSuite.Create(suiteName);
      rootSuite.addTest(targetSuite);
    end;

    RegisterTestInSuite(targetSuite, pathRemainder, test);
  end;
end;

procedure RegisterTest(SuitePath: string; suiteTemplate: TTestCaseClass);
var
  test:  ITest;
begin
  assert(assigned(suiteTemplate));

  test := TTestSuite.Create(suiteTemplate);
  RegisterTest(SuitePath, test);
end;

procedure RegisterTest(SuitePath: string; test: ITest);
begin
  assert(assigned(test));

  RegisterTestInSuite(__TestRegistry, SuitePath, test);
end;

procedure RegisterTest(test: ITest);
begin
  RegisterTest('', test);
end;

procedure RegisterTest(suiteTemplate: TTestCaseClass);
begin
  RegisterTest('', suiteTemplate);
end;

procedure RegisterTests(SuitePath: string; const Tests: array of ITest);
var
  i: Integer;
begin
  for i := Low(Tests) to High(Tests) do begin
    TestFramework.RegisterTest(SuitePath, Tests[i])
  end
end;

procedure RegisterTestSuites(SuitePath: string; const classes: array of TTestCaseClass);
var
  i: Integer;
begin
  for i := Low(classes) to High(classes) do begin
    TestFramework.RegisterTest(SuitePath, classes[i])
  end
end;

function RegisteredTests: ITestSuite;
begin
  result := __TestRegistry;
end;

function RunTest(suite: ITest; listeners: array of ITestListener): TTestResult; overload;
var
  i        : Integer;
begin
  result := TTestResult.Create;
  for i := low(listeners) to high(listeners) do
      result.addListener(listeners[i]);
  if suite <> nil then
    suite.Run(result);
end;

function RunTest(aclass: TTestCaseClass; listeners: array of ITestListener): TTestResult;
begin
   result := RunTest(TestFramework.TTestSuite.Create(aclass), listeners)
end;

function RunRegisteredTests(listeners: array of ITestListener): TTestResult;
begin
   result := RunTest(RegisteredTests, listeners)
end;

procedure CreateRegistry;
var
  MyName :AnsiString;
begin
  SetLength(MyName, 1024);
  GetModuleFileName(hInstance, PChar(MyName), Length(MyName));
  MyName := Trim(PChar(MyName));
  MyName := ExtractFileName(MyName);
  __TestRegistry := TTestSuite.Create(MyName);
end;

procedure ClearRegistry;
begin
  __TestRegistry := nil;
end;

initialization
  CreateRegistry;
finalization
  ClearRegistry;
end.



