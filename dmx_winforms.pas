unit dmx_winforms;

//implement draw for simple pantilt
//put a pantilt on the main form
//synchronize the pantilt's movements to whatever light is selected
// in
// out

// allow for manual entry of pan-tilts
// allow for reverse verification/adjustment of calibration points
// make the controls easier to remember
// give some better feedback for when we're in entry mode
// put the 5 standard calibration points in automatically.
// allow for drawing of the stage to be used
//



interface

uses
  applicationparams, beeper, sysutils, tickcount, debug, comctrls, graphics, stdctrls, extctrls, controls, typex, classes, dmx_Objects, advancedgraphics, geometry, guihelpers, systemx, numbers, stringx, winapi.windows, winapi.messages, math, colorblending;

type
  TDMXFixtureControl = class(TDrawingBoard)
  public
    lights: TDMXChannelClusterList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
  end;

  TPanTiltEvent = procedure (sender: TDMXFixtureControl; x,y: nativefloat) of object;


  TSimplePanTiltControl = class(TDMXFixtureControl)
  private
    FOnChange: TPanTiltEvent;
    FX: nativefloat;
    FY: nativefloat;
    procedure SetX(const Value: nativefloat);
    procedure SEtY(const Value: nativefloat);
  protected
    procedure Change;virtual;
    procedure DoChange;virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);override;

    procedure MouseMoveBetter(Shift: TShiftState; X, Y: Integer);override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word;
        Shift: TShiftState);override;
    procedure NudgeJOy(xby, yby: nativefloat);
    procedure NudgePan(xby, yby: nativefloat);
    procedure UpdateJoyFromScreen(x,y: integer);
  public

    procedure Paint;override;
    procedure DoDraw;override;



  published
    property X: nativefloat read FX write SetX;
    property Y: nativefloat read FY write SEtY;
    property OnChange: TPanTiltEvent read FOnChange write FonChange;

  end;

  TPanTiltControl = class(TDMXFixtureControl)
  private
    Fx: nativefloat;
    Fy: nativefloat;
    FLIghtList: TListBox;
    FPOintList: TListBox;
    resultpoint: TNativeFloatPOint;
    FCalIdx: ni;
    FGroup: string;
    FControlIdx: nativeint;
    procedure SetX(const Value: nativefloat);
    procedure SetY(const Value: nativefloat);

    procedure PaintLights;
    procedure SetLight(const Value: TDMXTiltPan);
    procedure SetLightList(const Value: TListBox);
    procedure SetPOintList(const Value: TListBox);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetCalidx(const Value: ni);
    procedure SetGroup(const Value: string);
    procedure SetControlIdx(const Value: nativeint);
    procedure ControlIDxChanged;

  public
    plane: TDMXPlane;
    Flight: TDMXTiltPan;
    lights: TUniverseList<TDMXTiltPan>;
    bCalLock: boolean;
    cals: array[0..7] of TnativefloatPoint;
    calhistidx: ni;
    selectedcalpointindex: ni;
    property light: TDMXTiltPan read FLight write SetLight;
    function PLaneLIght: PDMxPlaneLIght;
    procedure UpdateDMXPositions;
    procedure UpdateDMXPosition(l: TDMXTiltPan);
    procedure UpdateJoyFromSCreen(x,y: ni);
    procedure UpdateJoyFromFloatCoords(x,y: nativefloat);
    PROCEDURE DeletePoint(idx: nativeint);
    procedure DLGCode(var message: TMessage);message WM_GETDLGCODE;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);override;
    procedure MouseMoveBetter(Shift: TShiftState; X, Y: Integer);override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word;
        Shift: TShiftState);override;
    procedure KeyUp(var Key: Word;
        Shift: TShiftState);override;

    procedure NudgeJOy(xby, yby: nativefloat);
    procedure NudgePan(xby, yby: nativefloat);

    procedure Paint;override;
    procedure DoDraw;override;
    constructor Create(AOwner: TComponent); override;
    property X: nativefloat read Fx write SetX;
    property Y: nativefloat read Fy write SetY;
    procedure SaveData;
    procedure LoadData;
    procedure onselectlight(sender: TObject);
    procedure onselectpoint(sender: TObject);
    procedure refreshpoints;
    procedure RefreshSPanTilt;
    function CurrentCal: PDMXPLaneCalibrationPoint;
    property CalIdx: ni read FCalIdx write SetCalidx;
    procedure DefaultPoints;
    procedure RefreshLights;
  published
    spantilt: TSimplePanTiltControl;
    property LightLIst: TListBox read FLIghtList write SetLightList;
    property POintLIst: TListBox read FPOintList write SetPOintList;
    property Group: string read FGroup write SetGroup;
    property ControlIdx: nativeint read FControlIdx write SetControlIdx;
  end;

implementation

{ TPanTiltControl }


procedure TPanTiltControl.ControlIDxChanged;
begin
  if controlidx >= 0 then begin
    self.X := Multiverse.Joys[controlidx].Pan;
    self.y := Multiverse.Joys[controlidx].tilt;
  end;
end;

constructor TPanTiltControl.Create;
begin
  inherited;
  x := 0.5;
  y := 0.5;
  cursor := crCross;
  group := 'movers';
end;

function TPanTiltControl.CurrentCal: PDMXPLaneCalibrationPoint;
var
  ld: PDMXPLaneLight;
begin
  result := nil;
  if self.light = nil then exit;

  ld := plane.GetLightData(self.light);

  if ld = nil then exit;

  if calidx < 0 then exit;
  if calidx > high(ld.calibrationpoints) then exit;

  result := @ld.calibrationpoints[CalIdx];

end;

procedure TPanTiltControl.DefaultPoints;
var
  pl: PDMXPLaneLight;
begin
  pl := plane.GetLightData(light);
  pl.DefaultPOints;
  refreshpoints;

end;

procedure TPanTiltControl.DeletePoint(idx: nativeint);
var
  pl: PDMXPLaneLight;
begin
  pl := plane.GetLightData(light);
  pl.DeleteCalPOint(idx);
  refreshpoints;




end;

procedure TPanTiltControl.DLGCode(var message: TMessage);
begin
  message.Result := Message.Result or DLGC_WANTCHARS or DLGC_WANTARROWS or DLGC_WANTTAB or DLGC_WANTALLKEYS;
end;

procedure TPanTiltControl.DoDraw;
var
  c: TColor;
  xx,yy: nativefloat;
  pt: TNativefloatPOint;
  t,u: ni;
  pl: PDMXPLaneLight;
begin
  inherited;
  refreshpoints;

  self.Clear(clBlack);
//  DrawLineTest;
  self.BoundX1 := 0;
  self.BoundX2 := 1;
  self.boundy1 := 0;
  self.BoundY2 := 1;




//  xx := round(self.X * (self.width-1));
//  yy := round(self.Y * (self.Height-1));
//  self.Canvas.Brush.Color := clBlack;
//  self.Canvas.Rectangle(0,0,width, height);
  xx := self.X;
  yy := self.Y;
  c := clRed;
  if Self.Focused then
    c := clLime;
  self.LineColor := c;
  self.crosshair(xx,yy,0.05, c);

  if self.light <> nil then begin
    pl := Self.plane.GetLightData(self.light);
    if pl <> nil then begin
      if (pl.debug_primarypoint <> nil)
      and (pl.debug_influence1 <> nil)
      and (pl.debug_influence2 <> nil) then begin
        xx := pl.debug_primarypoint.joy.x;
        yy := pl.debug_primarypoint.joy.y;
        c := clYellow;
        self.LineColor := c;
        self.crosshair(xx,yy,0.05, c);

        xx := pl.debug_influence1.joy.x;
        yy := pl.debug_influence1.joy.y;
        c := clYellow;
        self.LineColor := c;
        self.crosshair(xx,yy,0.05, c);

        xx := pl.debug_influence2.joy.x;
        yy := pl.debug_influence2.joy.y;
        c := clYellow;
        self.LineColor := c;
        self.crosshair(xx,yy,0.05, c);

        self.Line(pl.debug_primarypoint.joy.x, pl.debug_primarypoint.joy.y, pl.debug_influence1.joy.x,pl.debug_influence1.joy.y, $FF007F);
        self.Line(pl.debug_primarypoint.joy.x, pl.debug_primarypoint.joy.y, pl.debug_influence2.joy.x,pl.debug_influence2.joy.y, $FF0000);
        self.Line(pl.debug_primarypoint.joy.x, pl.debug_primarypoint.joy.y, self.x, self.y, $7F7F00);


      end;
    end;
  end;


  xx := self.resultpoint.x;
  yy := self.resultpoint.y;
  c := clRed;
  if Self.Focused then
    c := clRed;
  self.LineColor := c;
  self.crosshair(xx,yy,0.05, c);





  if bCalLock then
  for t:= 1 to High(cals) do begin
    xx := cals[(calhistidx+t) mod length(cals)].x;
    yy := cals[(calhistidx+t) mod length(cals)].y;
    self.Crosshair(xx,yy,0.02, colorblend(clBlack, clYellow, t/length(cals)));
  end;


//  for t := 0 to high(plane.lights) do begin
    for u := 0 to high(planelight.calibrationpoints) do begin
      pt := planelight.calibrationpoints[u].pan;
      if u <> selectedcalpointindex then begin
        self.FatPoint(pt.x, pt.y, clGreen, clBlack, 3);
      end else begin
        self.FatPoint(pt.x, pt.y, clLime, clGray,  4);
        self.Crosshair(pt.x,pt.y, 0.03, clLime);
      end;
      //self.Rectangle(pt.x-1, pt.y-1, pt.x+1, pt.y+1, clLime, true);
    end;
    for u := 0 to high(planelight.calibrationpoints) do begin
      pt := planelight.calibrationpoints[u].joy;
      if u <> selectedcalpointindex then begin
        self.FatPoint(pt.x, pt.y, clRed, $7f, 3);
      end else begin
        self.FatPoint(pt.x, pt.y, clWhite, clGray,  4);
        self.Crosshair(pt.x,pt.y, 0.03, clWhite);
      end;
      //self.Rectangle(pt.x-1, pt.y-1, pt.x+1, pt.y+1, clLime, true);
    end;

//  end;

//
//
//  self.Canvas.Pen.color := clRed;
//  self.Canvas.MoveTo(xx, yy-5);
//  self.canvas.LineTo(xx, yy+6);
//  self.canvas.MoveTo(xx-5, yy);
//  self.canvas.LIneTo(xx+6, yy);



end;

procedure TPanTiltControl.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TPanTiltControl.KeyDown(var Key: Word;
  Shift: TShiftState);
var
  multiplier: nativefloat;
begin
  multiplier := 100/65536;

  inherited;
  if ssShift in shift then
    multiplier := multiplier / 100;
  if ssAlt in shift then
    multiplier := multiplier * 10;
  if key = VK_LEFT then begin
    if ssCtrl in shift then
    self.NudgeJoy(-1*multiplier, 0*multiplier)
    else
    self.NudgePan(-1*multiplier, 0*multiplier);
  end else
  if key = VK_RIGHT then begin
    if ssCtrl in shift then
    self.NudgeJoy(1*multiplier, 0*multiplier)
    else
    self.NudgePan(1*multiplier, 0*multiplier);
  end else
  if key = VK_UP then begin
    if ssCtrl in shift then
    self.NudgeJoy(0*multiplier, -1*multiplier)
    else
    self.NudgePan(0*multiplier, -1*multiplier);
  end else
  if key = VK_DOWN then begin
    if ssCtrl in shift then
    self.NudgeJoy(0*multiplier, 1*multiplier)
    else
    self.NudgePan(0*multiplier, 1*multiplier);
  end;

  draw;
  invalidate;

end;

procedure TPanTiltControl.KeyPress(var Key: Char);
var
  ld: PDMXPLaneLight;
begin
  inherited;
  if key = #13 then begin
    ld := plane.getlightdata(light);
    if ld <> nil then begin
      ld.AddCalPoint(self.x, self.Y, ld.GetInterpolatedPoint(x,y).x, ld.GetInterpolatedPoint(x,y).y, ld.GetInterpolatedPoint(x,y).z);
      refreshpoints;
      Self.POintLIst.ItemIndex := self.pointlist.Items.count-1;
      self.CalIdx := Self.POintLIst.ItemIndex;
      draw;
      self.UpdateJoyFromFloatCoords(self.x, self.y);
    end;
    invalidate;
    SaveData;
//      beeper.beep(400,50);
  end;

end;

procedure TPanTiltControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  //
end;

procedure TPanTiltControl.LoadData;
begin
  Multiverse.Stage.LoadData;

end;

procedure TPanTiltControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  p: TNativefloatPoint;
begin
  if not focused then begin
    if canfocus then
      SetFocus;

    draw;
    exit;
  end;

  inherited;

  UpdateJoyFromScreen(x,y);
  p.x := x-(width / 2);
  p.y := y-(height /2);
//    consolelog('quad:'+INTTOSTR(p.quadrant(aoRaster))+' angle:'+floattostr(p.AngleInUCs(aoRaster)));


  invalidate;

end;


procedure TPanTiltControl.MouseMoveBetter(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssleft in shift then
    UpdateJoyFromScreen(x,y);

  invalidate;





end;

procedure TPanTiltControl.NudgeJOy(xby, yby: nativefloat);
var
  pt: TNativeFloatPOint;
begin
  pt := self.plane.GetLightData(light).calibrationpoints[self.selectedcalpointindex].joy;
  pt.x := pt.x + xby;
  pt.y := pt.y + yby;
  self.plane.GetLightData(light).calibrationpoints[self.selectedcalpointindex].joy := pt;
  UpdateDMXPositions;

end;

procedure TPanTiltControl.NudgePan(xby, yby: nativefloat);
var
  pt: TNativeFloatPOint;
begin
  pt := self.plane.GetLightData(light).calibrationpoints[self.selectedcalpointindex].pan;
  pt.x := pt.x + xby;
  pt.y := pt.y + yby;
  self.plane.GetLightData(light).calibrationpoints[self.selectedcalpointindex].pan := pt;
  self.X := self.plane.GetLightData(light).calibrationpoints[self.selectedcalpointindex].joy.x;
  self.Y := self.plane.GetLightData(light).calibrationpoints[self.selectedcalpointindex].joy.y;
  UpdateDMXPositions;
  self.SaveData;
end;

procedure TPanTiltControl.onselectlight(sender: TObject);
var
  str: IDMXColor;
  ul: TUniverseList<IDMXColor>;
  ul2: TUniverseList<IDMXIntensity>;
begin
  if TListbox(sender).ItemIndex < 0 then
    exit;



  light := lights[TListbox(sender).ItemIndex];
  FirstUniverse.Operate<IDMXColor>(IDMXColor, group,
          procedure(intf: IDMXColor; idx: ni)
          begin
            intf.color := clRed;
          end
  );

  ul := light.parent.GetAspectInterfaceList<IDMXColor>(IDMXColor);
  if ul.count > 0 then begin
    ul[0].Color := clWhite;
  end;

  ul2 := light.parent.GetAspectInterfaceList<IDMXIntensity>(IDMXIntensity);
  if ul2.count > 0 then begin
    ul2[0].Intensity := 1.0;
  end;

  ul.free;
  ul2.free;

  refreshpoints;
  CalIdx := TListBox(sender).itemindex;
  draw;


end;

procedure TPanTiltControl.onselectpoint(sender: TObject);

var
  ld: PDMXPLaneLight;
  pt: TnativefloatPoint;
begin
  self.selectedcalpointindex := TListBox(sender).ItemIndex;
  ld := self.plane.GetLightData(light);
  pt := ld.calibrationpoints[self.selectedcalpointindex].joy;
  Self.UpdateJoyFromFloatCoords(pt.x,pt.y);

  refreshpoints;
  CalIdx := TListBox(sender).itemindex;
  draw;
  invalidate;

end;

procedure TPanTiltControl.Paint;
begin
  inherited;
//  consolelog('draw'+inttostr(getticker));
  Draw;
end;

procedure TPanTiltControl.PaintLights;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TPanTiltControl.PLaneLIght: PDMxPlaneLIght;
begin
  result := plane.GetLightData(light);
end;

procedure TPanTiltControl.RefreshLights;
var
  t,u: ni;
  gl: TDMXLIghtGroup;
  al: TUniverseList<TDMXTiltPan>;

begin
  gl := Multiverse.Groups.ByName[self.Group];

  lights.clear;
  lightlist.Items.Clear;
  for t:= 0 to gl.count-1 do begin
    al := gl[t].GetAspectList<TDMXTiltPan>;
    for u := 0 to al.count-1 do begin
      lights.add(al[u]);
    end;
    al.free;

    lightlist.items.Add(inttostr(t)+':'+gl[t].ClassName+'@'+inttostr(gl[t].BaseChannel));
  end;
end;

procedure TPanTiltControl.refreshpoints;
var
  t: ni;
  pl: PDMXPLaneLight;
begin
  if not assigned(pointlist) then
    exit;

  pl := plane.GetLightData(light);
  synclistbox(pointlist, length(pl.calibrationpoints));


  for t:= 0 to high(pl.calibrationpoints) do begin
    pointlist.Items[t] := inttostr(t)+': '+
      floatprecision(pl.calibrationpoints[t].joy.x, 2)+', '+floatprecision(pl.calibrationpoints[t].joy.y, 2)+'='+
      floatprecision(pl.calibrationpoints[t].pan.x, 2)+', '+floatprecision(pl.calibrationpoints[t].pan.y, 2);
  end;

  refreshSPanTilt;


end;

procedure TPanTiltControl.RefreshSPanTilt;
var
  cal: PDMXPLaneCalibrationPoint;
begin
  if spantilt = nil then
    exit;

  cal := Self.CurrentCal;
  if cal = nil then exit;

  spantilt.x := cal.pan.x;
  spantilt.Y := cal.pan.y;

end;

procedure TPanTiltControl.SaveData;
begin
  Multiverse.stage.savedata;

end;

procedure TPanTiltControl.SetCalidx(const Value: ni);
begin
  FCalIdx := Value;
  refreshspantilt;
end;

procedure TPanTiltControl.SetControlIdx(const Value: nativeint);
begin
  FControlIdx := Value;
  ControlIdxchanged;
end;

procedure TPanTiltControl.SetGroup(const Value: string);
begin
  FGroup := Value;

  refreshpoints;

end;

procedure TPanTiltControl.SetLight(const Value: TDMXTiltPan);
begin
  FLight := Value;
  invalidate;
end;

procedure TPanTiltControl.SetLightList(const Value: TListBox);
begin
  FLIghtList := Value;
  FLightlist.OnClick := self.onselectlight;
end;

procedure TPanTiltControl.SetPOintList(const Value: TListBox);
begin
  FPOintList := Value;
  FPOintList.OnClick := self.onselectpoint;
end;

procedure TPanTiltControl.SetX(const Value: nativefloat);
begin
  Fx := Value;
  invalidate;
end;

procedure TPanTiltControl.SetY(const Value: nativefloat);
begin
  Fy := Value;
  invalidate;
end;

procedure TPanTiltControl.UpdateDMXPosition(l: TDMXTiltPan);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TPanTiltControl.UpdateDMXPositions;
var
  t: ni;
  l: TDMXTiltPan;
  ld: PDMXPLaneLight;
  nfp: TNativeVector4;
begin
  for t:= 0 to lights.count-1 do begin
    l := TDMXTiltPan(lights[t]);
//    l.PanF := x;
//    l.TiltF := y;
    if plane <> nil then begin
      ld := plane.GetLightData(l);
      if ld = nil then
        exit;
      nfp := ld.GetInterpolatedPOint(x,y);
      l.PanF := nfp.x;
      l.TiltF := nfp.y;
      if l = light then begin
        if assigned(spantilt) then begin
          spantilt.x := l.Panf;
          spantilt.y := l.tiltf;
          spantilt.draw;
          resultpoint.x := l.PanF;
          resultpoint.y := l.TiltF;
        end;
      end;
    end;


  end;
end;

procedure TPanTiltControl.UpdateJoyFromFloatCoords(x, y: nativefloat);
begin
  self.X := x;
  self.y := y;

  //update if in calibration mode
  if ControlIDx < 0 then begin
    UpdateDMXPositions;
  end else begin
    Multiverse.joys[controlidx].pan := self.x;
    Multiverse.joys[controlidx].tilt := self.y;
  end;


  invalidate;



end;

procedure TPanTiltControl.UpdateJoyFromSCreen(x, y: ni);
begin
  updatejoyfromfloatcoords(x/(width-1), y/(height-1));


end;

{ TDMXFixtureControl }

constructor TDMXFixtureControl.Create(AOwner: TComponent);
begin
  inherited;
  lights := TDMXChannelClusterList.Create;

end;

destructor TDMXFixtureControl.Destroy;
begin
  lights.free;
  lights := nil;

  inherited;
end;

{ TSimplePanTiltControl }

procedure TSimplePanTiltControl.Change;
begin
  DoChange;
end;

procedure TSimplePanTiltControl.DoChange;
begin
  if assigned(FOnChange) then
    FOnChange(self, x,y);

end;

procedure TSimplePanTiltControl.DoDraw;
var
  c: TColor;
  xx,yy: nativefloat;
  pt: TNativefloatPOint;
  t,u: ni;
begin
  inherited;
  self.BoundX1 := 0;
  self.BoundX2 := 1;
  self.boundy1 := 0;
  self.BoundY2 := 1;

  self.Clear(clBlack);

  xx := self.X;
  yy := self.Y;
  c := clRed;
  if Self.Focused then
    c := clLime;
  self.LineColor := c;

  if not ((IsNan(xx) or (IsNan(yy)))) then
    self.crosshair(xx,yy,0.05, c);


end;

procedure TSimplePanTiltControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

end;

procedure TSimplePanTiltControl.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TSimplePanTiltControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p: TNativefloatPoint;
begin
  if not focused then begin
    if canfocus then
      SetFocus;

    draw;
    exit;
  end;

  inherited;

  UpdateJoyFromScreen(x,y);
  p.x := x-(width / 2);
  p.y := y-(height /2);
//  consolelog('quad:'+INTTOSTR(p.quadrant(aoRaster))+' angle:'+floattostr(p.AngleInUCs(aoRaster)));


end;

procedure TSimplePanTiltControl.MouseMoveBetter(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

end;

procedure TSimplePanTiltControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TSimplePanTiltControl.NudgeJOy(xby, yby: nativefloat);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TSimplePanTiltControl.NudgePan(xby, yby: nativefloat);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TSimplePanTiltControl.Paint;
begin
  inherited;
  Draw;
end;

procedure TSimplePanTiltControl.SetX(const Value: nativefloat);
begin
  FX := Value;
  invalidate;
end;

procedure TSimplePanTiltControl.SEtY(const Value: nativefloat);
begin
  FY := Value;
  invalidate;
end;

procedure TSimplePanTiltControl.UpdateJoyFromScreen(x, y: integer);
var
  xx,yy: nativefloat;
begin
  if width = 0 then exit;
  if height = 0 then exit;


  xx := x / (width-1);
  yy := y / (height-1);

  self.x := xx;
  self.Y := yy;

  invalidate;
  draw;


end;

end.
