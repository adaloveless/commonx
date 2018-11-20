unit TundraCharts;

interface
{$inline auto}
uses
  debug, advancedGraphics, sysutils, graphics, classes, easyimage, math, colorblending;


type
  TrealPoint = packed record
    x,y:variant;
    NAME: ansistring;
  end;

  TSeries = class
  private
    FName: ansistring;
    FColor: TColor;
    FValues: array of TrealPoint;
    FLineWidth: real;
    FShowTicks: boolean;
    function GetValues(idx: integer): TrealPoint;
    procedure SetColor(const Value: Tcolor);
    procedure SetName(const Value: ansistring);
    procedure SetValues(idx: integer; const Value: TrealPoint);
    function GetValueCount: integer;
  public
    constructor create; reintroduce; virtual;
    property Values[idx: integer]: Trealpoint read GetValues write SetValues;default;
    property ValueCount: integer read GetValueCount;
    procedure AddValue(r: TrealPoint);overload;
    procedure AddValue(x: variant; y: variant; sName: ansistring = '');overload;
    procedure AddValue(x: variant; sName: ansistring = '');overload;
    procedure Clear;

    property Name: ansistring read FName write SetName;
    property Color: Tcolor read FColor write SetColor;
    property LineWidth: real read FLineWidth write FLineWidth;

    function MaxX: real;
    function MaxY: real;
    function MinX: real;
    function MinY: real;
    property ShowTicks: boolean read FShowTicks write FShowTicks;



    function HasLine(x1,y1,x2,y2: variant): boolean;


    procedure SortPoints;
  end;

  TTundraChart = class(TDrawingBoard)
  private
    FList: TList;
    FTitle: ansistring;
    FMinXtick: real;
    FShowLegend: boolean;
    function GetSeries(idx: integer): TSeries;
  public
    constructor create(aowner: TComponent); override;
    destructor destroy; override;
    property Series[idx: integer]: TSeries read GetSeries;
    function AddSeries: TSeries;
    function SeriesCount: integer;
    procedure Clear;
    procedure SortPoints;
    function MaxX: real;
    function MaxY: real;
    function MinX: real;
    function MinY: real;
    property Title: ansistring read FTitle write FTitle;


    property MinXTick: real read FMinXtick write FMinXTick;
    property ShowLegend: boolean read FShowLegend write FShowLegend;
    function SumSeriesX: real;
  end;

  TLinechart = class(TTundraChart)
  private
    FInvertY: boolean;
    FYAxisName: ansistring;
    FXAxisName: ansistring;
  public
    procedure DoDraw; override;

    property XAxisName: ansistring read FXAxisName write FXAxisName;
    property YAxisName: ansistring read FYAxisName write FYAxisName;
    property InvertY: boolean read FInvertY write FInvertY;

  end;

  TBarChart = class(TTundraChart)
  private
    FYAxisName: ansistring;
    FXAxisName: ansistring;
    FBarSpacing: integer;
  public
    procedure DODraw; override;
    property XAxisName: ansistring read FXAxisName write FXAxisName;
    property YAxisName: ansistring read FYAxisName write FYAxisName;
    property BarSpacing: integer read FBarSpacing write FBarSpacing;

  end;





  TTinyPieChart = class(TTundraChart)
  private
  public
    procedure DoDraw;override;

  end;


function GreaterOf(r1,r2: variant): variant;
function LesserOf(r1,r2: variant): variant;

function SnapTo(rValue, rPrecision: real): real;
function FindPrecision(rPrecision: real): real;
function GetChartColor(i: integer): TColor;


implementation

uses variants;

{ TTundraChart }
function GetChartColor(i: integer): TColor;
begin
  result := 0;
  case i mod 8 of
    0: result := $ff0000;
    1: result := $00ff00;
    2: result := $0000ff;
    3: result := $7f7f00;
    4: result := $007f7f;
    5: result := $7f007f;
    6: result := $7f7f7f;
    7: result := $000000;
  end;






end;
function GreaterOf(r1,r2: variant): variant;
begin
  result := r1;
  if r2 > result then
    result := r2;
end;

function LesserOf(r1,r2: variant): variant;
begin
  result := r1;
  if r2 < result then
    result := r2;
end;


function TTundraChart.AddSeries: TSeries;
var
  s: TSeries;
begin
  s := TSeries.create;
  result := s;
  FList.add(s);


end;

procedure TTundraChart.Clear;
var
  s: tSeries;
begin
  while FList.count > 0 do begin
    s := TSeries(FList[0]);

    s.free;
    Flist.delete(0);


  end;

end;

constructor TTundraChart.create(aowner: TComponent);
begin
  inherited;
  FList := TList.create;
  FShowLegend := true;
end;

destructor TTundraChart.destroy;
begin
  Clear;
  FList.free;
  inherited;
end;

function TTundraChart.GetSeries(idx: integer): TSeries;
begin
  result := TSeries(FList[idx]);

end;

function TSeries.MaxX: real;
var
  t: integer;
begin
  result := 0;

  for t:= 0 to ValueCount-1 do begin
    if values[t].x > result then
      result := values[t].x;
  end;

end;

function TSeries.MaxY: real;
var
  t: integer;
begin
  result := 0;

  for t:= 0 to ValueCount-1 do begin
    if values[t].y > result then
      result := values[t].y;
  end;

end;

function TTundraChart.MaxX: real;
var
  t: integer;
begin
  if Seriescount = 0 then begin
    result := 0;
    exit;
  end;

  result :=  self.Series[0].MaxX;
  for t:= 0 to Seriescount-1 do begin
    result := GreaterOf(self.Series[t].MaxX, result);
  end;

end;

function TTundraChart.MaxY: real;
var
  t: integer;
begin
  if Seriescount = 0 then begin
    result := 0;
    exit;
  end;

  result :=     self.Series[0].MaxY;
  for t:= 0 to Seriescount-1 do begin
    result := GreaterOf(self.Series[t].MaxY, result);
  end;

end;

function TTundraChart.MinX: real;
var
  t: integer;
begin
  if Seriescount = 0 then begin
    result := 0;
    exit;
  end;

  result :=     self.Series[0].MinX;
  for t:= 0 to Seriescount-1 do begin
    result := LesserOf(self.Series[t].MinX, result);
  end;
end;

function TTundraChart.MinY: real;
var
  t: integer;
begin
  if Seriescount = 0 then begin
    result := 0;
    exit;
  end;

  result :=     self.Series[0].MinY;
  for t:= 0 to Seriescount-1 do begin
    if Series[t].ValueCount = 0 then
      continue;

    result := LesserOf(self.Series[t].MinY, result);
  end;
end;

function TTundraChart.SeriesCount: integer;
begin
  result := Flist.count;
end;

procedure TTundraChart.SortPoints;
var
  t: integer;
begin
  for t:= 0 to self.FList.Count-1 do begin
    series[t].SortPoints;
  end;


end;

function TTundraChart.SumSeriesX: real;
var
  t,u: integer;
begin
  result := 0.0;
  for t:= 0 to self.SeriesCount-1 do begin
    for u:= 0 to self.Series[t].ValueCount-1 do begin
      result := result+series[t].Values[u].x;
    end;
  end;

end;

{ TSeries }

procedure TSeries.AddValue(r: TrealPoint);
begin
  SetLength(FValues, length(FValues)+1);

  FValues[length(FValues)-1] := r;

end;

function TSeries.GetValueCount: integer;
begin
  result := Length(FValues);

end;

function TSeries.GetValues(idx: integer): TrealPoint;
begin
  result := FValues[idx];


end;
procedure TSeries.SetColor(const Value: Tcolor);
begin
  FColor := Value;
end;

procedure TSeries.SetName(const Value: ansistring);
begin
  FName := Value;
end;

procedure TSeries.SetValues(idx: integer; const Value: TRealPOint);
begin
  if idx > length(FValues) then
    raise Exception.create('Value index out of range');

  FValues[idx] := Value;


end;

procedure TSeries.SortPoints;
var
  bSorted: boolean;
  t, m: integer;
  p: tRealPoint;
begin
  repeat
    bSorted := true;
    for m := length(FValues) div 2 downto 1 do begin
      for t:= 0 to length(FValues)-1 do begin
        if t+m > length(FValues)-1 then
          continue;

        if FValues[t].x > fValues[t+m].x then begin
          p := FValues[t];
          fValues[t] := FValues[t+m];
          FValues[t+m] := p;
          bSorted := false;
        end;
      end;
    end;
  until bSorted;
end;

function TSeries.MinX: real;
var
  t: integer;
begin
  if valuecount = 0 then begin
    result := 2100000000;
    exit;
  end;

  result := values[0].x;

  for t:= 0 to ValueCount-1 do begin
    if values[t].x < result then
      result := values[t].x;
  end;
end;

function TSeries.MinY: real;
var
  t: integer;
begin
  if valuecount = 0 then begin
    result := 0;
    exit;
  end;

  result := values[0].y;

  for t:= 0 to ValueCount-1 do begin
    if values[t].y < result then
      result := values[t].y;
  end;
end;

procedure TSeries.AddValue(x: variant; sName: ansistring);
begin
  self.AddValue(x,0,'');
end;

procedure TSeries.Clear;
begin
  setlength(FValues, 0);

end;

procedure TSeries.AddValue(x, y: variant; sName: ansistring = '');
var
  p: TRealPoint;
begin
  if varType(x) = varnull then
    exit;

  if varType(y) = varnull then
    exit;

  p.x := x;
  p.y := y;

  if sName = '' then
    p.Name := vartostr(x)
  else
    p.name := sName;

  AddValue(p);


end;

constructor TSeries.create;
begin
  inherited;
  FLineWidth := 1.5;

end;

function TSeries.HasLine(x1, y1, x2, y2: variant): boolean;
var
  t: integer;
begin
  result := false;
  for t:= 1 to valuecount-1 do begin
    if (values[t].x = x2) and (values[t].y = y2)
    and (values[t-1].x = x1) and (values[t-1].y = y1)
    then begin
      result := true;
      exit;
    end;
  end;

end;

{ TLinechart }

procedure TLinechart.DoDraw;
var
  t,u,i: integer;
  lw,llw: real;
  th,tw: real;
  x1,y1,x2,y2: variant;
  polarity: integer;
  tick,ii: real;
  iPrecision: integer;
  slGraphed: TStringList;
  rKeyLeft, rKeyRight, rkeyTop, rKeyBottom: real;
  xxx: real;
begin
  inherited;

  //clear up everything
  self.new;
  self.ConstrainProportions := false;
  UseScreenCoordinates := true;
  self.Rectangle(0,0,width-1,height-1, clWhite, true);
  UseScreenCoordinates := False;


  //sort the data that will go into the graph
  SortPoints;

  //set bounds to minimums and maximums

  BoundX2 := MaxX;
  BoundX1 := MinX;
  if InvertY then begin
    BoundY1 := MinY;
    BoundY2 := MaxY;
  end else begin
    BoundY2 := MinY;
    BoundY1 := MaxY;
  end;


  //define margins for main canvas of graph
  LeftMargin := width * 0.10;
  TopMargin := 30;
  RightMargin := 120;
  BottomMargin := 30;


  //draw vertical ticks -- needs improvement
  for t:= Trunc(MinX) to Trunc(MaxX) do begin
//    i := round((MaxX-MinX)/10);
//    if i = 0 then break;
//
//    if t mod i <> 0 then
//      continue;

    if MinXTick > 0 then begin
      if (trunc(t-MinX) mod trunc(MinXTick))<> 0 then
        continue;
    end;
    fatLine(t, boundy1, t, boundY2, 0.1, clSilver, true);
  end;


  //determine tick text width
  tw := ScaleScreenXToGlobal(leftmargin);

(*  Debug.Log('10 to global is:'+floattostr(th));
  Debug.Log(floattostr(th)+' to screen is:'+floattostr(ScaleGlobalYtoScreen(th)));

  Debug.Log('global margin is:'+floattostr(tw));
  Debug.Log(floattostr(tw)+' to screen is:'+floattostr(ScaleGlobalXtoScreen(tw)));*)



  if BoundY2 < BoundY1 then
    polarity := -1
  else
    polarity := 1;


  tick := MinY;
  ii := (MaxY-MinY)/5;
  ii := FindPrecision(ii);

  tick := SnapTo(tick, ii);


  //determine tick text height
  th := ii /2;


  xxx := ScaleScreenYToGlobal(12);
//  Debug.Log('ScreenY-> GlobalY 12 = '+floattostr(xxx));
//  Debug.Log('ScreenY-> GlobalY 12 = '+floattostr(ScaleGlobalYToScreen(xxx)));


  if th > ScaleScreenYToGlobal(12) then
    th := ScaleScreenYToGlobal(12);
//  th := ;

  repeat
    if tick >= MinY then begin
      fatLine(boundX1, tick, boundX2, tick, 0.1, clGray, true);

      font.color := clBlack;
      //FatPoint(ToGlobalX(0), tick, clYellow, clBlack, 3);

      Text(BoundX1-ScaleScreenXtoGlobal(leftmargin*0.95), tick-((th/2) * polarity), tw, th, floattostr(tick), false);
//      DebugFlip();
    end;

    tick := tick + ii;


  until (tick >= MaxY) or (ii = 0);

  //------------------MAIN BOX
  self.FatBox(BoundX1,BoundY1,BoundX2,Boundy2,2,clBlack,true);

//  Flip();

  lw := 30;
  //-------------COMPLEX LINE DRAWING
  while lw > 0 do begin
    for u:= 0 to SeriesCount-1 do begin
      llw := series[u].LineWidth;
      if (llw >= lw) and (llw < (lw+0.2)) then
      for t:= 1 to series[u].Valuecount-1 do begin
        if (u = 0) and (t = 1) then begin
          UseScreenCoordinates := true;
          Text(leftmargin, height-bottommargin+4,width, 12, self.Series[u].Values[t-1].NAME, false, false);
          UseScreenCoordinates := false;
        end;
        if (u = 0) and (t = series[u].valuecount-1) then begin
          UseScreenCoordinates := true;
          Text(width-rightmargin-100, height-bottommargin+4,200, 12, self.Series[u].Values[t].NAME, true);
          UseScreenCoordinates := false;
        end;

        llw := series[u].LineWidth;
        x1 := self.Series[u].values[t-1].x;
        y1 := self.Series[u].values[t-1].y;
        x2 := self.Series[u].values[t].x;
        y2 := self.Series[u].values[t].y;


        for i := 0 to seriescount-1 do begin
          if series[i].HasLine(x1,y1,x2,y2) then begin
            if (series[i].LineWidth > (series[u].linewidth /2)) and (series[i].linewidth <= series[u].linewidth) then begin
              if (series[i].LineWidth <> series[u].LineWidth) or (i > u) then
                llw := llw + 4;
            end;
          end;
        end;
        self.FatLine(x1,y1,x2,y2,llw, series[u].color, true);

      end;
    end;
    lw := lw-0.2;
  end;


  //------------------------FAT POINTS
  for u:= 0 to Seriescount-1 do begin
    if series[u].showticks then
    for t:= 0 to series[u].Valuecount-1 do begin
      self.FatPoint(self.Series[u].values[t].x, self.series[u].Values[t].y,  ColorBlend(series[u].color, clWhite, 0.5), series[u].color, 3);
    end;
  end;

  //-------------------------TITLE
  UseScreenCoordinates := true;
  self.font.color := clMaroon;
  self.Text(0,3,width,16, Title, true);
  self.Font.color := clBlack;

  //-------------------Y AXIS NAME
  self.Text(leftmargin-15, height-1, height, 12, YAxisName, true, true);

  //-------------------X AXIS NAME
  self.Text(leftmargin, height-15, width-(rightmargin+leftmargin), 12, XAxisName, true);

  //---------------------------------------KEY
  //-keybox
  self.UseScreenCoordinates := true;
  rKeyLeft := width-(rightmargin*0.95);
  rkeyright := width-(rightmargin*0.05);
  rKeyTop := topmargin;
  rKeybottom := 3+topmargin+(12*(seriescount+2));
  Fatbox(rKeyLeft, rkeyTop, rKeyRight, rKeyBottom, 1, clBlack,true);

  rKeyLeft := width-(rightmargin*0.90);
  rkeyright := width-(rightmargin*0.10);
  rKeyTop := topmargin+3;
  self.font.color := clBlack;
  Text(rKeyLeft, rKeyTop, rKeyRight-rKeyLeft, 12, 'Key', false, false);

  for t:= 0 to self.SeriesCount -1 do begin
    self.font.color := self.series[t].color;
    Text(rKeyLeft, rKeyTop+((14)*(t+1)), rKeyRight-rKeyLeft, 12, '--'+self.Series[t].Name,false, false);
//    self.series.Name;
  end;






end;

function SnapTo(rValue, rPrecision: real): real;
begin
  result := 0;
  if rPrecision = 0 then begin
    rValue := 1;
    exit;
  end;
  result := rValue * (1/rPrecision);
  result := round(result);
  result := result / (1/rPrecision);


end;


function FindPrecision(rPrecision: real): real;
var
  t: integer;
begin
  result := rPrecision;

  for t:= -4 to 10 do begin
    if rPrecision > power(10, t) then begin
      result := SnapTo(result, power(10, t));
    end;
  end;

end;


{ TBarChart }

procedure TBarChart.DoDraw;
var
  t,u,i: integer;
  lw,llw: real;
  th,tw: real;
  x1,y1,x2,y2: variant;
  polarity: integer;
  tick,ii: real;
  iPrecision: integer;
  slGraphed: TStringList;
  rKeyLeft, rKeyRight, rkeyTop, rKeyBottom: real;
  xxx: real;
begin
  inherited;

  //clear up everything
  self.new;
  self.ConstrainProportions := false;
  UseScreenCoordinates := true;
  self.Rectangle(0,0,width-1,height-1, clWhite, true);
  UseScreenCoordinates := False;


  //sort the data that will go into the graph
  SortPoints;

  //set bounds to minimums and maximums
  BoundX2 := MaxX+1;
  BoundX1 := MinX;
  BoundY2 := MinY;
  BoundY1 := MaxY;


  //define margins for main canvas of graph
  LeftMargin := width * 0.10;
  TopMargin := 30;
  RightMargin := 120;
  BottomMargin := 30;


  //draw vertical ticks -- needs improvement
  for t:= Trunc(MinX) to Trunc(MaxX) do begin
    if MinXTick > 0 then begin
      if (trunc(t-MinX) mod trunc(MinXTick))<> 0 then
        continue;
    end;
    fatLine(t, boundy1, t, boundY2, 0.1, clSilver, true);
  end;


  //determine tick text width
  tw := ScaleScreenXToGlobal(leftmargin);

  if BoundY2 < BoundY1 then
    polarity := -1
  else
    polarity := 1;


  tick := MinY;
  ii := (MaxY-MinY)/5;
  ii := FindPrecision(ii);

  tick := SnapTo(tick, ii);


  //determine tick text height
  th := ii /2;

  xxx := ScaleScreenYToGlobal(12);


  if th > ScaleScreenYToGlobal(12) then
    th := ScaleScreenYToGlobal(12);
//  th := ;

  repeat
    if tick >= MinY then begin
      fatLine(boundX1, tick, boundX2, tick, 0.1, clGray, true);

      font.color := clBlack;
      //FatPoint(ToGlobalX(0), tick, clYellow, clBlack, 3);

      Text(BoundX1-ScaleScreenXtoGlobal(leftmargin*0.95), tick-((th/2) * polarity), tw, th, floattostr(tick), false);
//      DebugFlip();
    end;

    tick := tick + ii;


  until (tick >= MaxY) or (ii = 0);

  //------------------MAIN BOX
  self.FatBox(BoundX1,BoundY1,BoundX2,Boundy2,2,clBlack,true);

//  Flip();

  lw := 30;
  //draw boxes
  for u := 0 to SeriesCount-1 do begin
    for t:= 0 to series[u].ValueCount-1 do begin
      self.FancyRectangle(series[u].Values[t].x+(u/(seriescount+BarSpacing)),0, series[u].Values[t].x+((u+1)/(seriescount+BarSpacing)), series[u].Values[t].y, series[u].color);
    end;
  end;


  //------------------------FAT POINTS
//  for u:= 0 to Seriescount-1 do begin
//    if series[u].showticks then
//    for t:= 0 to series[u].Valuecount-1 do begin
//      self.FatPoint(self.Series[u].values[t].x, self.series[u].Values[t].y,  ColorBlend(series[u].color, clWhite, 0.5), series[u].color, 3);
//    end;
//  end;

  //-------------------------TITLE
  UseScreenCoordinates := true;
  self.font.color := clMaroon;
  self.Text(0,3,width,16, Title, true);
  self.Font.color := clBlack;

  //-------------------Y AXIS NAME
  self.Text(leftmargin-15, height-1, height, 12, YAxisName, true, true);

  //-------------------X AXIS NAME
  self.Text(leftmargin, height-15, width-(rightmargin+leftmargin), 12, XAxisName, true);

  //---------------------------------------KEY
  //-keybox
  self.UseScreenCoordinates := true;
  rKeyLeft := width-(rightmargin*0.95);
  rkeyright := width-(rightmargin*0.05);
  rKeyTop := topmargin;
  rKeybottom := 3+topmargin+(12*(seriescount+2));
  Fatbox(rKeyLeft, rkeyTop, rKeyRight, rKeyBottom, 1, clBlack,true);

  rKeyLeft := width-(rightmargin*0.90);
  rkeyright := width-(rightmargin*0.10);
  rKeyTop := topmargin+3;
  self.font.color := clBlack;
  Text(rKeyLeft, rKeyTop, rKeyRight-rKeyLeft, 12, 'Key',false, false);

  for t:= 0 to self.SeriesCount -1 do begin
    self.font.color := self.series[t].color;
    Text(rKeyLeft, rKeyTop+((14)*(t+1)), rKeyRight-rKeyLeft, 12, '--'+self.Series[t].Name,false, false);
//    self.series.Name;
  end;

end;





{ TPieChart }

procedure TTinyPiechart.DoDraw;
var
  t,u,i: integer;
  lw,llw: real;
  th,tw: real;
  x1,y1,x2,y2: variant;
  polarity: integer;
  tick,ii: real;
  iPrecision: integer;
  slGraphed: TStringList;
  rKeyLeft, rKeyRight, rkeyTop, rKeyBottom: real;
  xxx: real;
  sum: real;
  start,next: real;
begin
  //clear up everything
  self.new;
  self.ConstrainProportions := false;
  UseScreenCoordinates := true;
  self.Rectangle(0,0,width-1,height-1, clBlack, true);
  UseScreenCoordinates := False;

  //set bounds to minimums and maximums
  BoundX2 := 1;
  BoundX1 := 0;
  BoundY2 := 0.5;
  BoundY1 := 0;



  //define margins for main canvas of graph
  LeftMargin := width * 0.10;
  TopMargin := 15;
  if ShowLegend then begin
    RightMargin := 120;
  end else begin
    RightMargin := width * 0.10;
  end;
  BottomMargin := 15;

  xxx := ScaleScreenYToGlobal(12);

//  if th > ScaleScreenYToGlobal(12) then
  th := ScaleScreenYToGlobal(12);

  //------------------MAIN BOX
//  self.FatBox(BoundX1,BoundY1,BoundX2,Boundy2,2,clBlack,true);


  lw := 30;

  //-------------------------TITLE
  UseScreenCoordinates := true;
  self.font.color := clWhite;
  self.Text(0,3,width,16, Title, true);
  self.Font.color := clBlack;


  //-------------------Y AXIS NAME

  //-------------------X AXIS NAME
  self.UseScreenCoordinates := false;



  sum := SumSeriesX;
  start := 0.0;

  for t:= 0 to self.seriescount-1 do begin
    next := start+series[t].values[0].x/Sum;
    self.FatPartialCircle(self.CenterX, self.CenterY, 0.2, start,next , 0.1, series[t].color, series[t].color,true);
    start := next;
  end;


  //---------------------------------------KEY
  //-keybox
  if ShowLegend then begin
    self.UseScreenCoordinates := true;
    rKeyLeft := width-(rightmargin*0.95);
    rkeyright := width-(rightmargin*0.05);
    rKeyTop := topmargin;
    rKeybottom := 3+topmargin+(12*(seriescount+2));
    Fatbox(rKeyLeft, rkeyTop, rKeyRight, rKeyBottom, 1, clBlack,true);

    rKeyLeft := width-(rightmargin*0.90);
    rkeyright := width-(rightmargin*0.10);
    rKeyTop := topmargin+3;
    self.font.color := clBlack;
    Text(rKeyLeft, rKeyTop, rKeyRight-rKeyLeft, 12, 'Key',false, false);

    for t:= 0 to self.SeriesCount -1 do begin
      self.font.color := self.series[t].color;
      Text(rKeyLeft, rKeyTop+((14)*(t+1)), rKeyRight-rKeyLeft, 12, '--'+self.Series[t].Name,false, false);
  //    self.series.Name;
    end;
  end;

end;

end.
