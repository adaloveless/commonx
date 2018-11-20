unit AnalogTrackBar;

interface

uses
  SysUtils, Classes, Controls, ComCtrls, geometry;

type
  TAnalogTrackBar = class(TTrackBar)
  private
    FRMin: real;
    FRMax: real;
    function GetRPosition: real;
    procedure SetRPosition(const Value: real);
    procedure SetRMax(const Value: real);
    procedure SetRMin(const Value: real);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property RMax: real read FRMax write SetRMax;
    property RMin: real read FRMin write SetRMin;
    property RPosition: real read GetRPosition write SetRPosition;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Digital Tundra', [TAnalogTrackBar]);
end;

{ TAnalogTrackBar }

function TAnalogTrackBar.GetRPosition: real;
begin
  result := interpolate(position, RMin, RMax, Min, Max);
end;

procedure TAnalogTrackBar.SetRMax(const Value: real);
begin
  FRMax := Value;

  SetRPosition(position);
end;

procedure TAnalogTrackBar.SetRMin(const Value: real);
begin
  FRMin := Value;
  SetRPosition(position);
end;

procedure TAnalogTrackBar.SetRPosition(const Value: real);
begin
  position := round(interpolate(Value, Min, Max, RMin, RMax));

end;

end.
