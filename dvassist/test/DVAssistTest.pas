unit DVAssistTest;

interface

uses
  Classes,
  TestFramework,
  GUITestRunner,
  DVAssistForm;

type

  TTestShiftStateAdapter = class(TTestCase)
  protected
  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure testShiftStateConversions;
  end;

  function Suite: ITestSuite;


implementation

{ TTestDVAForm }

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create(TTestShiftStateAdapter);
end;

procedure TTestShiftStateAdapter.SetUp;
begin
end;

procedure TTestShiftStateAdapter.TearDown;
begin
end;

procedure TTestShiftStateAdapter.testShiftStateConversions;
begin
  CheckEquals(0, TShiftStateAdapter.Create([]).ToInt);
  CheckEquals(1, TShiftStateAdapter.Create([ssShift]).ToInt);
  CheckEquals(2, TShiftStateAdapter.Create([ssCtrl]).ToInt);
  CheckEquals(3, TShiftStateAdapter.Create([ssShift, ssCtrl]).ToInt);
  CheckEquals(4, TShiftStateAdapter.Create([ssAlt]).ToInt);
  CheckEquals(5, TShiftStateAdapter.Create([ssShift, ssAlt]).ToInt);
  CheckEquals(6, TShiftStateAdapter.Create([ssCtrl, ssAlt]).ToInt);
  CheckEquals(7, TShiftStateAdapter.Create([ssShift, ssCtrl, ssAlt]).ToInt);

  CheckEquals(0, TShiftStateAdapter.Create(TShiftStateAdapter.Create(0).ShiftState).ToInt);
  CheckEquals(1, TShiftStateAdapter.Create(TShiftStateAdapter.Create(1).ShiftState).ToInt);
  CheckEquals(2, TShiftStateAdapter.Create(TShiftStateAdapter.Create(2).ShiftState).ToInt);
  CheckEquals(3, TShiftStateAdapter.Create(TShiftStateAdapter.Create(3).ShiftState).ToInt);
  CheckEquals(4, TShiftStateAdapter.Create(TShiftStateAdapter.Create(4).ShiftState).ToInt);
  CheckEquals(5, TShiftStateAdapter.Create(TShiftStateAdapter.Create(5).ShiftState).ToInt);
  CheckEquals(6, TShiftStateAdapter.Create(TShiftStateAdapter.Create(6).ShiftState).ToInt);
  CheckEquals(7, TShiftStateAdapter.Create(TShiftStateAdapter.Create(7).ShiftState).ToInt);
end;


end.
