unit MeasureGUI;

interface

uses
  chart, measurer, typex;

procedure MeasurerToChart(chart: TChart; measurer: TMeasurer);

implementation

procedure MeasurerToChart(chart: TChart; measurer: TMeasurer);
var
  t: ni;
begin
  chart.Series[0].Clear;

  for t:= 0 to measurer.count-1 do begin
    chart.Series[0].Add(measurer.Measurements[t].Time, measurer.Measurements[t].Name);
  end;

end;

end.
