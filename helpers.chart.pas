unit helpers.chart;

interface

uses
  stringx, typex, classes, sysutils, debug,
  VclTee.TeeGDIPlus, VCLTee.TeEngine, jsonhelpers,
  VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart;

procedure ExportChart(c: TChart; sFile: string);
function FindSeriesYValueByX(c: TChart; xvalue: double; iSeries: ni): double;overload;
function FindSeriesYValueByX(cs: TChartSeries; xvalue: double): double;overload;
function HasXValue(c: TChart; xvalue: double; iSeries: ni): boolean;
function GetPointAverageByX(c: TChart; xvalue: double; iSeries: ni): double;
procedure GenerateAverageSeries(c: TChart; iTargetSeries, iSourceSeries: ni);
procedure chart_ClearData(c: TChart);
procedure LimitValues(c: TChart; iSeriesIndex: ni; limit: int64);

procedure JSONToSeries(j: TJSON; s: TChartSeries; xField,yField: string);
procedure JSONToSeriesXTime(j: TJSON; s: TChartSeries; xField,yField: string);



implementation


procedure chart_ClearData(c: TChart);
var
  t: ni;
begin
  for t:= 0 to c.seriescount-1 do begin
     c.series[t].clear;
  end;

end;

function FindSeriesYValueByX(c: TChart; xvalue: double; iSeries: ni): double;
var
  v: ni;
begin
  result := 0;
  for v := 0 to c.Series[iSeries].Count-1 do begin
    if c.Series[iSeries].XValue[v] = xvalue then begin
      result := c.Series[iSeries].YValue[v];
    end;
  end;
end;

function FindSeriesYValueByX(cs: TChartSeries; xvalue: double): double;
var
  v: ni;
begin
  result := 0;
  for v := 0 to cs.Count-1 do begin
    if cs.XValue[v] = xvalue then begin
      result := cs.YValue[v];
    end;
  end;
end;


function GetCSVHeaderForChart(c: TChart): string;
var
  t: ni;
begin

  result := '';
  for t:= 0 to c.SeriesList.count-1 do begin
    if result <> '' then
      result := result +',';
    result := result + Quote(c.SeriesList[t].Title);
  end;
end;

procedure ExportChart(c: TChart; sFile: string);
var
  t,u: ni;
  sl: TStringlist;
  y: double;
  sLine:string;
begin
  sl := TStringlist.create;
  try


    //EXPORTS CHART -- LIMITATIONS, Series 0 is master
    if c.SeriesList.count = 0 then
      exit;

    sLIne := GetCSVHEaderForChart(c);
    sl.add('"X",'+sLIne);

    for u := 0 to c.Series[0].Count do
    begin
      sLine := floattostr(c.Series[0].XValue[u])+',';
      for t:= 0 to c.SeriesList.Count-1 do
      begin
        if t >0 then
          sLine := sLIne + ',';

        sLine := sLine + floattostr(FindSeriesYValueByX(c, c.Series[0].XValue[u], t));


      end;
     sl.add(sLIne);
    end;
    sl.SaveToFile(sFile);
  finally
    sl.free;

  end;


end;

function GetPointAverageByX(c: TChart; xvalue: double; iSeries: ni): double;
var
  t: ni;
  v: double;
  cnt: ni;
begin
  v := 0;
  cnt := 0;
  for t := 0 to c.Series[iSeries].Count-1 do
  begin
    if c.Series[iSeries].XValue[t] = xvalue then
    begin
      v := v + c.Series[iSeries].YValue[t];
      inc(cnt);
    end;
  end;
  result := v / cnt;
end;

procedure GenerateAverageSeries(c: TChart; iTargetSeries, iSourceSeries: ni);
var
  t: ni;
  d,xv: double;

begin
  c.series[iTargetSeries].clear;
  for t:= 0 to c.series[iSourceSeries].Count-1 do begin
    xv := c.Series[iSourceSeries].XValue[t];
    if not HasXvalue(c, xv, iTargetSeries) then begin
      d := GetPointAverageByX(c, xv, iSourceSeries);
      c.Series[iTargetSeries].AddXY(xv, d);
    end;
  end;

end;


function HasXValue(c: TChart; xvalue: double; iSeries: ni): boolean;
var
  t: ni;
begin
  result := false;
  for t:= 0 to c.series[iSeries].Count-1 do begin
    if c.Series[iSeries].XValue[t] = xvalue then begin
      result := true;
      exit;
    end;
  end;
end;

procedure LimitValues(c: TChart; iSeriesIndex: ni; limit: int64);
begin
  while(c.Series[iSeriesIndex].ValuesList.Count> limit) do
    c.Series[iSeriesIndex].ValuesList.Delete(0);
  while(c.Series[iSeriesIndex].YValues.Count > limit) do
    c.Series[iSeriesIndex].yValues.Delete(0);

  while(c.Series[iSeriesIndex].XValues.Count > limit) do
    c.Series[iSeriesIndex].XValues.Delete(0);


end;

procedure JSONToSeries(j: TJSON; s: TChartSeries; xField,yField: string);
var
  t: ni;
  x,y: variant;
begin
  s.Clear;
  for t:= 0 to j.IndexedCount-1 do begin
//    var ss := j[t].ToJson;
    x := j[t][xField].value;
    y := j[t][yField].value;
//    Debug.Log(vartostrex(x)+','+vartostrex(y));
    s.AddXY(y{+(random(1000)/1000000)},x);
  end;
end;


procedure JSONToSeriesXTime(j: TJSON; s: TChartSeries; xField,yField: string);
var
  t: ni;
  x,y: variant;
begin
  s.Clear;
  for t:= 0 to j.IndexedCount-1 do begin
//    var ss := j[t].ToJson;
    x := j[t][xField].value;
    y := j[t][yField].value;
//    Debug.Log(vartostrex(x)+','+vartostrex(y));
    s.AddXY(y{+(random(1000)/1000000)},TDateTime(x));
  end;
end;



end.
