unit GanttDX;

interface


uses
  system.UITypes, debug, advancedgraphics_dx, typex, systemx, graphics, betterobject, maths, geometry, System.SysConst, types, classes, colorblending, colorconversion, tickcount, numbers, sysutils, better_colors, math, guihelpers, speech, d3dx9, windows, stringx, dxtypes, direct3d9_jedi, rtti_helpers, ganttdata;

//  windows, System.SysConst, system.UITypes, AdvancedGraphics_Dx, ganttdata, colorconversion, graphics, numbers, sysutils, messages;

type
  TRegion = record
    startpoint: single;
    endpoint: single;
    function Dif: single;
    function Center: single;
  end;


  TDXGantt = class(TDX2d)
  protected
    FMouseScrollStartXX,
    CurrentHistoryx1,
    CurrenthistoryX2,
    TargetHistoryX1,
    TargetHistoryX2: single;
    scrolldrag: TRegion;
    procedure DoMouseDown; override;
    procedure DoMouseUp;override;
    procedure DoMouseOver;override;
  public
    data: PGanttData;

    procedure DoDraw; override;
    procedure ResetView;

    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;override;
    procedure UpdatePhysics(rDeltaTime: Cardinal); override;
    procedure LoadTextures; override;
    function GetHourFromScreen(screenx: ni; portion: double = 1.0): double;





  end;

implementation

{ TDXGantt }

procedure TDXGantt.DoDraw;
var
  rec: TGanttRecord;
begin
  inherited;



  ClearScreen(clBlack);
  BoundX1 := CurrentHistoryX1;
  BoundX2 := CurrentHistoryX2;
  Boundy1 := 0;
  Boundy2 := lesserof(data.maxtrack,600);

  AlphaOp := aoAdd;


//  BeginVertexBatch(D3DPT_LINELIST);
  try
    var lasthourplotted :double := -9999999;
    for var t := 0 to width-1 do begin
      var hour := GetHourFromScreen(t, 1/60);
      if hour <> lasthourplotted then begin
        Self.LineColor := clYellow;
        var xx := screenToGlobalX(t);
        self.BeginLine;
        self.DrawLine(xx, boundy1,$FF1F1F1F);
        self.DrawLine(xx, boundy2,$FF1F1F1F);
        self.EndLIne;
        lasthourplotted := hour;

      end;
    end;
    FlushLine;
  finally
//    EndVertexBatch;
  end;

//  BeginVertexBatch(D3DPT_LINELIST);
  try
    var lasthourplotted :double := -9999999;
    for var t := 0 to width-1 do begin
      var hour := GetHourFromScreen(t, 1/4);
      if hour <> lasthourplotted then begin
        Self.LineColor := clYellow;
        var xx := screenToGlobalX(t);
        self.BeginLine;
        self.DrawLine(xx, boundy1,aclGrey);
        self.DrawLine(xx, boundy2,aclGrey);
        self.EndLIne;
        lasthourplotted := hour;

      end;
    end;
    FlushLine;
  finally
//    EndVertexBatch;
  end;



  BeginVertexBatch;
  try
    var recs := data.recs;
    for var t := 0 to high(recs) do begin
      rec := recs[t];
      var ts := recs[t].date-data.startday;
      var te := recs[t].runtimef;
//      if te < 0.0001 then
//        te := 0.0001;
      te := ts+te;
      if ts > boundx2+(1/24) then
        break;
      if te < boundx1-(1/24) then
        continue;

      //rec.track := 0;
      var sts: single := ts;
      var ste: single := te;

      Rectangle_Fill(sts, rec.track, ste, rec.track+1, clWhite,clGreen,clGreen,clBlack, 0.9);

    end;

    if data.selected <> nil then begin
      var a := (sin(getticker/500)+1.0)/2;
      rec := data.selected^;
      Rectangle_Fill(rec.date-data.startday, rec.track, rec.endtime-data.startday, rec.track+1, clWhite,clBlue,clBlue,clRed, a);
    end;

  finally

    endVertexBAtch;
  end;




//  BeginVertexBatch(D3DPT_LINELIST);
  try
    var lasthourplotted :double := -9999999;
    for var t := 0 to width-1 do begin
      var hour := GetHourFromScreen(t, 1);
      if hour <> lasthourplotted then begin
        Self.LineColor := clYellow;
        var xx := screenToGlobalX(t);
        self.BeginLine;
        self.DrawLine(xx, boundy1,aclWhite);
        self.DrawLine(xx, boundy2,aclWhite);
        self.EndLIne;
        lasthourplotted := hour;

      end;
    end;
    FlushLine;
  finally
//    EndVertexBatch;
  end;

//  BeginVertexBatch(D3DPT_LINELIST);
  SetFont(0);
  try
    var lasthourplotted :double := -9999999;
    for var t := 0 to width-1 do begin
      var hour := GetHourFromScreen(t, 1);

      if t = 0 then begin
        lasthourplotted := hour;
        continue;
      end;

      if hour <> lasthourplotted then begin
        var xx := screenToGlobalX(t);
        var sDate := DateTimeToStr(data.startday+(hour/24));
        TextOut(sDate, xx, (BoundY1+BoundY2)/2, [tfShadow, tfStroke]);
        lasthourplotted := hour;

      end;
    end;
    FlushLine;
  finally
//    EndVertexBatch;
  end;

  begin
    if data.selected <> nil then begin
      TextPosition.x := 0;
      TextPOsition.y := 0;
      ResetText;
      SetFont(0);

      canvas_Text(CRLF+CRLF+CRLF+'Selected:'+CRLF,[tfStroke]);
      canvas_Text('Start: '+datetimetostr(rec.date)+CRLF);
      canvas_Text('Runtime: '+floatprecision(rec.runtime/1000,2)+' seconds.'+CRLF);
      canvas_Text('URL: '+rec.head+CRLF);
    end;
  end;


end;

procedure TDXGantt.DoMouseDown;
begin
  inherited;

  if mouse_buttons_down[1] then begin
//    data.hotstring := trunc(FLastMouseYY);
//    reg.startpoint := FLastMouseXX;
//    reg.endpoint := FLastMouseXX;
//    data.stringHistorySelection := reg;
    var searchAt  := mouse_last_pos_for_wheel;


    var x := ScreenToGlobalX(searchAt.x)+data.startday;
    var y := ScreenToGlobalY(searchAt.y);
    data.searchpoint(x,y);




    Mousehandled := true;
  end;
  if mouse_buttons_down[0] then begin
    scrolldrag.startpoint := (mouse_last_pos_for_wheel.x);
    ScrollDrag.endpoint := (mouse_last_pos_for_wheel.x);
    FMouseScrollStartXX := TargetHistoryX1;
    Debug.Log('Start = '+floatprecision(    scrolldrag.startpoint,8));
  end;
  if mouse_buttons_down[2] then begin
//    data.hotstring := trunc(FLastMouseYY);
//    if StringsToShow <=1 then begin
//      StringsToShow := NUM_STRINGS;
//      FirstShowingString := 0;
//      TargetStringStart := 0;
//      TargetStringEnd := 6;
//      w := CurrentHistoryX2-CurrentHistoryX1;
//      c := FLastMouseXX;
//      TargetHistoryX1 := c-((w/2)*6);
//      TargetHistoryX2 := c+((w/2)*6);
//
//
//    end else begin
//      StringsToShow := 1;
//      FirstShowingString := data.HotString;
//      TargetStringStart := data.HotString;
//      TargetStringEnd := data.HotString+1;
//      w := CurrentHistoryX2-CurrentHistoryX1;
//      c := FLastMouseXX;
//      TargetHistoryX1 := c-((w/2)/6);
//      TargetHistoryX2 := c+((w/2)/6);
//    end;
  end;

end;

procedure TDXGantt.DoMouseUp;
begin
  inherited;
  if not mouse_buttons_down[0] then begin
//    scrolldrag.startpoint := (mouse_last_pos_for_wheel.x);
    ScrollDrag.endpoint := (mouse_last_pos_for_wheel.x);

  end;

end;

function TDXGantt.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
var
  w: nativefloat;
  x1,x2: nativefloat;
  c,cPercent: nativefloat;
begin
  inherited;
//  if data.mode = dmPickEngine then begin
    w := (TargetHistoryX2-TargetHistoryX1);
    if w = 0 then
      w := high(data.recs);
    cPercent := (ScreenToGlobalX(mouse_last_pos_for_wheel.x) - TargetHistoryX1) / w;
    c := ScreenToGlobalX(mouse_last_pos_for_wheel.x);

    w := w * 1.21;
    x1 := c - (w*cPercent);
    x2 := c + (w*(1-cPercent));
    TargetHistoryX1 := x1;
    TargetHistoryX2 := x2;
//  end;




  result := true;

end;


function TDXGantt.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  w: nativefloat;
  x1,x2: nativefloat;
  c,cPercent: nativefloat;
begin
  inherited;
//  if data.mode = dmPickEngine then begin
    w := (TargetHistoryX2-TargetHistoryX1);
    cPercent := (ScreenToGlobalX(mouse_last_pos_for_wheel.x) - TargetHistoryX1) / w;
    c := ScreenToGlobalX(mouse_last_pos_for_wheel.x);

    w := w / 1.15;
    x1 := c - (w*cPercent);
    x2 := c + (w*(1-cPercent));

    TargetHistoryX1 := x1;
    TargetHistoryX2 := x2;
//  end;

  result := true;

end;


function TDXGantt.GetHourFromScreen(screenx: ni; portion: double = 1.0): double;
begin
  var start := CurrentHistoryx1;
  result := ScreenToGlobalX(screenx);
  result := result * 24 * 1/portion;
  result := trunc(result);
  result := result / portion;

end;

procedure TDXGantt.LoadTextures;
begin
  inherited;
  LoadFont   ('graphics\font.png',2,2);//0

end;

procedure TDXGantt.ResetView;
begin
  BoundX1 := 0;
  BoundX2 := 0.001;
  CurrentHistoryx1 := 0.0;
  CurrentHistoryx2 := 0.001;
  TargetHistoryX1 := 0.0;
  TargetHistoryX2 := 0.001;
end;


procedure TDXGantt.UpdatePhysics(rDeltaTime: Cardinal);
const
  aa = 0.1;
var
  a,b: double;
begin
  inherited;
  if rDeltaTime > 1000 then
    rDeltaTime := 1000;

  a := aa;
  b := 1.0-a;

  //btnPause.ColorWhenLit.FromColor(colorblend(clRed,clBlack,(getticker mod 333)));
//  HandleCalibrationState;

  CurrentHistoryX1 := {CLAMP}((CurrentHistoryX1 * (b)) + (TargetHistoryX1 * (a)){,0,HISTORY_SIZE});
  CurrentHistoryX2 := {CLAMP}((CurrentHistoryX2 * (b)) + (TargetHistoryX2 * (a)){,0,HISTORY_SIZE});

//  CurrentStringStart := (CurrentStringStart * (b)) + (TargetStringStart * (a));
//  CurrentStringEnd := (CurrentStringEnd * (b)) + (TargetStringEnd * (a));

end;

procedure TDXGantt.DoMouseOver;
begin
  inherited;
  if mouse_buttons_down[0] then begin
    scrolldrag.endpoint := (mouse_last_pos_for_wheel.x);
    var wid := TargetHistoryX2-TargetHistoryX1;
    var dif := scrolldrag.dif;
    dif := ScaleScreenXtoGlobal(dif,true);


    TargetHistoryX1 := FMouseScrollStartXX - dif;
    TargetHistoryX2 := TargetHistoryX1+wid;
//    Debug.Log('dif='+floatprecision(dif,8));

  end;

end;

{ TRegion }

function TRegion.Center: single;
begin
  result := ((endpoint-startpoint)/2)+startpoint;
end;

function TRegion.Dif: single;
begin
  result := endpoint-startpoint;

end;

end.
