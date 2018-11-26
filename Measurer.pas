unit Measurer;
//HOW TO USE
//1. Call Init Measurement
//2. Call Measure('something');
//3. Call Measure('something_else') (automatically stops and tags previous measurement
//4. Call ConcludeMeasurement - does the final roll up
//5. Use MeasureGUI.MeasureToChart() to put it all in a pie


interface

uses
  tickcount, typex, betterobject, sharedobject, chart, classes, windows, systemx, stringx, graphics, controls, advancedgraphics, colorconversion;

const
    MAX_MEASURE = 64;

type
  TMEasurement = record
    StartTime: cardinal;
    Name: string;
    Time: cardinal;
    CumTime: int64;
    HitCount: int64;

    procedure Init;
    procedure OpenMeasurement;
    procedure CloseMeasurement;
    function AvgTime: nativefloat;
  end;

  PMeasureMent = ^TMeasurement;

  TMeasurer = class(TBetterObject)
  private
  protected
    FAddPOinter: ni;
    FMeasureIndex: integer;
  public
    Measurements: array[0..MAX_MEASURE-1] of TMeasurement;
    procedure DefineMeasurement(sName: string);

    procedure SetCurrentMeasurement(sName: string);
    procedure InitMeasurement;
    procedure Measure(sName: string);
    procedure EndCurrentMeasurement;
    procedure ConcludeMeasurement;

    function Count: nativeint;


  end;

implementation

{ TMeasurer }


function TMeasurer.Count: nativeint;
begin
  result := FAddPOinter;
end;

procedure TMeasurer.DefineMeasurement(sName: string);
begin
  Measurements[FaddPointer].init;
  Measurements[FAddPOinter].Name := sNAme;
  inc(FAddPOinter);

end;

procedure TMeasurer.EndCurrentMeasurement;
begin
  if FMeasureIndex >=0 then begin
    Measurements[FMeasureIndex].CloseMeasurement;

  end;
end;

procedure TMeasurer.ConcludeMeasurement;
begin
  EndCurrentMeasurement;
end;

procedure TMeasurer.InitMeasurement;
begin
  FMeasureIndex := -1;
end;

procedure TMeasurer.Measure(sName: string);
begin
  EndCurrentMeasurement;

  SetCurrentMeasurement(sNAme);
  if FMeasureIndex < 0 then begin
    DefineMeasurement(sName);
    SetCurrentMeasurement(sNAme);
  end;
  if FMeasureIndex >=0 then
    Measurements[FMeasureIndex].OpenMeasurement;
end;

procedure TMeasurer.SetCurrentMeasurement(sName: string);
var
  t: integer;
begin
  FMeasureIndex := -1;
  for t:= 0 to FAddPOinter-1 do begin
    if Measurements[t].Name = sNAme then begin
      FMeasureIndex := t;
      exit;
    end;
  end;
end;

{ TMEasurement }

function TMEasurement.AvgTime: nativefloat;
begin
  result := cumtime/hitcount;
end;

procedure TMEasurement.CloseMeasurement;
begin
  Time := GetTimeSince(StartTime);
end;

procedure TMEasurement.Init;
begin
  CUmtime := 0;
  HitCount := 0;
  Name := '';
end;

procedure TMEasurement.OpenMeasurement;
begin
  StartTime := GetTicker;
end;

end.
