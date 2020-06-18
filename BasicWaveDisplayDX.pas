unit BasicWaveDisplayDX;
//2020

interface

uses
  advancedgraphics_dx, typex, graphics, SoundSample, soundwavesampler, SoundInterfaces, soundtools, ColorConversion, ColorBlending, graphicsx, direct3d9_jedi, numbers, geometry, controls, classes, types, debug, stringx;

type
  TBasicWaveDisplayDX = class(TDX2D)
  private
    FStream: TSoundStream;
    procedure SetStream(const Value: TSoundStream);

  protected
    FSelectStart, FSelectEnd: double;
    FMouseScrollDragStartXX: double;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;override;
    procedure DoMouseDown; override;
    procedure DoMouseUp;override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CommitSelection;
  public
    ViewRangeScrollStart: TRegionD;
    ViewRange: TRegionD;
    TargetRange: TRegionD;
    procedure DoDraw; override;
    constructor Create(AOwner: TComponent); override;
    procedure UpdatePhysics(rDeltaTime: Cardinal); override;
    property Stream: TSoundStream read FStream write SetStream;

    function GetSampleNumberForScreenX(x: int64): int64;

    //stuff I want to accomplish
    //1. Display the wave
    //2. Select part of the wave when shift is held
    //3. Sync to playback
    //4. Draw some curves or something



  published
  end;

implementation

{ TBasicWaveDisplayDX }

procedure TBasicWaveDisplayDX.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  inherited;

end;

procedure TBasicWaveDisplayDX.CommitSelection;
begin
  order(FSelectStart, FSelectEnd);
end;

constructor TBasicWaveDisplayDX.Create(AOwner: TComponent);
begin
  inherited;
  EnableInternalDrawTimer := true;

end;

procedure TBasicWaveDisplayDX.DoDraw;
var
  ss: TStereoSoundSample;
begin
  inherited;
  if FStream = nil then
    exit;

  BoundX1 := ViewRange.startpoint;
  BoundX2 := ViewRange.endpoint;
  BoundY1 := 1;
  BoundY2 := -1;
  ClearScreen(clBlack);
  AlphaOp := aoAdd;
  BeginVertexBatch(D3DPT_LINELIST);

  BeginLine;
  try
    for var x := 0 to width-1 do begin
      var xs := ScreenToGlobalX(x);
      FStream.GetResample(xs,ss);
      DrawLine(xs, ss.Left, $7FFFFF00);
    end;
  finally
    ENdLine;
  end;

  BeginLine;
  try
    for var x := 0 to width-1 do begin
      var xs := ScreenToGlobalX(x);
      FStream.GetResample(xs,ss);
      DrawLine(xs, ss.Right, $7F00FFFF);
    end;
  finally
    ENdLine;
  end;
  CommitBatch;

  Rectangle_Fill(FSelectStart, 1, FSelectEnd, -1, clWhite, 0.2);




end;

procedure TBasicWaveDisplayDX.DoMouseDown;
begin
  inherited;
  var xx := FLastMouseXX;
  var yy := FLAstMouseYY;
  if false then begin
    //this is for selecting an area
  end else begin
    //if the mouse buttong was just clicked
    if not mouse_buttons_were_down[0] then begin
      if ssShift in FLastMouseShift then begin
        FSelectStart := fLastMouseXX;
      end else begin
        FMouseScrollDragStartXX := fLastMouseXX;
        ViewRangeScrollStart := ViewRange;
      end;
    end;





  end;
end;

procedure TBasicWaveDisplayDX.DoMouseUp;
begin
  inherited;
  CommitSelection;//orders selection if out of order
end;

function TBasicWaveDisplayDX.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
var
  w: nativefloat;
  x1,x2: nativefloat;
  c,cPercent: nativefloat;
begin
  inherited;
//  if data.mode = dmPickEngine then begin
    w := (TargetRange.Dif);
    if w = 0 then
      w := FStream.SampleCount;
    cPercent := (ScreenToGlobalX(mouse_last_pos_for_wheel.x) - TargetRange.startpoint) / w;
    c := ScreenToGlobalX(mouse_last_pos_for_wheel.x);

    w := w * 1.21;
    x1 := c - (w*cPercent);
    x2 := c + (w*(1-cPercent));
    TargetRange.startpoint := x1;
    TargetRange.endpoint := x2;
//  end;



//  Draw;
  result := true;

end;


function TBasicWaveDisplayDX.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  w: nativefloat;
  x1,x2: nativefloat;
  c,cPercent: nativefloat;
begin
  inherited;
//  if data.mode = dmPickEngine then begin
    w := (TargetRange.dif);
    cPercent := (ScreenToGlobalX(mouse_last_pos_for_wheel.x) - TargetRange.startpoint) / w;
    c := ScreenToGlobalX(mouse_last_pos_for_wheel.x);

    w := w / 1.15;
    x1 := c - (w*cPercent);
    x2 := c + (w*(1-cPercent));

    TargetRange.startpoint := x1;
    TargetRange.endpoint := x2;
//  end;

//  Draw;
  result := true;

end;
function TBasicWaveDisplayDX.GetSampleNumberForScreenX(x: int64): int64;
begin
  result := trunc(geometry.Interpolate(x, ViewRange.startpoint, ViewRange.endpoint, 0, width));
end;



procedure TBasicWaveDisplayDX.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if mouse_buttons_down[0] then begin
    if ssShift in shift then begin
      FSelectEnd := FLastMouseXX;

    end else begin
      var dif :=  (fLastMouseXX - FMouseScrollDragStartXX);
      //Debug.Log(floatprecision(dif,8));
      TargetRange := ViewRangeScrollStart - dif;
    end;
  end;

end;

procedure TBasicWaveDisplayDX.SetStream(const Value: TSoundStream);
begin
  FStream := Value;
  ViewRange.startpoint := 0;
  ViewRange.endpoint := FStream.SampleCount-1;
  TargetRange := ViewRange;

end;

procedure TBasicWaveDisplayDX.UpdatePhysics(rDeltaTime: Cardinal);
const
  AR = 0.95;
  BR = 1.0-AR;
begin
  inherited;
  ViewRange.startpoint := (viewrange.startpoint*AR)+(targetrange.startpoint*BR);
  viewrange.endpoint := (viewrange.endpoint*AR)+(targetrange.endpoint*BR);
//  viewrange.startpoint := viewrange.startpoint /2;
//  viewrange.endpoint :=viewrange.endpoint/2;

end;

end.
