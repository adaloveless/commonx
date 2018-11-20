unit Kinect;

{$INLINE AUTO}
{$DEFINE DISABLE_KINECT}

interface

uses typex, betterobject, kinectDLL, systemx, geometry,easyimage;

type
  TKinect = class(TBetterObject)
  private

  protected
    k: HKINECT;
  public
    depth: array[0..479] of array [0..639] of smallint;
    depth_base: array[0..479] of array [0..639] of smallint;
    depth_compare_first: array[0..479] of array [0..639] of integer;
    depth_compare: array[0..479] of array [0..639] of integer;
    processing: array[0..479] of array [0..639] of NativeFloat;
    center: TNativeFloatPoint;
    constructor Create;override;
    destructor Destroy;override;
    procedure FetchDepthBuffer;
    procedure SaveBaseDepthBuffer;
    procedure ApplyAsMaximumBuffer;
    procedure CreateDepthCompareBuffer;

    procedure GobbleDifferenceNoise;
    procedure CalculateWeightedCenter;
  end;



implementation


{ TKinect }

procedure TKinect.ApplyAsMaximumBuffer;
var
  x,y: integer;
begin
  for y := 0 to 479 do begin
    for x := 0 to 639 do begin
     if depth[y][x] > 2000 then
        continue;
     if depth[y][x] > self.depth_base[y][x] then
        self.depth_base[y][x] := depth[y][x];

    end;
  end;
end;

procedure TKinect.CalculateWeightedCenter;
var
  x,y: integer;
  xx,yy,w,ww: double;
begin
  fillmem(@processing[0][0], 640*480*sizeof(NativeFloat), 0);

  xx := 0;
  yy := 0;
  ww := 0;
  for y := 0 to 479 do begin
    for x := 0 to 639 do begin
      w := (depth_compare[y][x]);
//      w := w * w;
      if w < 8 then continue;

      xx := xx + (x * w);
      yy := yy + (y * w);
      ww := ww + w;
    end;
  end;

  if ww = 0 then begin
    center.x := 320;
    center.y := 240;
  end else begin
    xx := xx / ww;
    yy := yy / ww;

    center.x := xx;
    center.y := yy;
  end;

end;

constructor TKinect.Create;
begin
  inherited;
  k := Kinect_Init();
  if k >=0 then
    Kinect_SetTilt(k,0);
end;

procedure TKinect.CreateDepthCompareBuffer;
var
  x,y: integer;
begin
  for y := 0 to 479 do begin
    for x := 0 to 639 do begin
      depth_compare_first[y][x] := (0-(depth[y][x] - self.depth_base[y][x]));
    end;
  end;

  GobbleDifferenceNoise;

end;

destructor TKinect.Destroy;
begin

  inherited;
end;

procedure TKinect.FetchDepthBuffer;
begin
  Kinect_GetDepthBuffer(k, @depth[0]);
end;



procedure TKinect.GobbleDifferenceNoise;
var
  xx,yy,x,y: integer;
  d: integer;
  bKill: boolean;
begin
  for y := 2 to 477 do begin
    for x := 2 to 637 do begin
      d := 0;
      bkill := false;
      for yy := -2 to 2 do begin
        for xx := -2 to 2 do begin
          if self.depth_compare_first[y+yy][x+xx] < 32 then begin
            depth_compare[y][x] := 0;
            bKill := true;
          end;

          if bKill then break;



        end;
        if bKill then break;
      end;
      if not bKill then
        depth_compare[y][x] := depth_compare_first[y][x];
    end;
  end;

end;

procedure TKinect.SaveBaseDepthBuffer;
var
  x,y: integer;
begin
  for y := 0 to 479 do begin
    for x := 0 to 639 do begin
      if depth[y][x] > 2000 then
        depth[y][x] := 0;
      self.depth_base[y][x] := depth[y][x];

    end;
  end;
end;

initialization

Kinect_DLL_Init();


end.
