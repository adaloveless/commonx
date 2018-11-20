unit worldvis;
dont use me
interface

uses Vcl.Graphics, advancedgraphics, easyimage, systemx, typex, gameterrain, numbers, tickcount, geometry, debug, colorblending, colorconversion, fastbitmap;

type
  TWorldVisPoint = record
    x, y: double;
    px, py: ni;
    undercolor: TNativeFloatColor;
    function Distancefrom(wvp: TWorldVisPoint): double;
  end;

  TWorldRing = record
    ridx: ni;
    ridx_last_rendered: ni;
    ring: array [0 .. 2550] of TWorldVisPoint;
    sect: TCLXCriticalSection;
    fbm, fbmBase: TFastBitmap;
    lastbreak: ni;
    slsat: ni;
    procedure Init;
    procedure Finalize;
    procedure Lock;
    procedure Unlock;
    procedure Inject(wvp: TWorldVisPoint);
    procedure Renderto(bm: TPicture);
    function Hist(rBack: ni): TWorldVisPoint;
  public
    procedure generateBaseWorldVisualizer;
  end;

var
  wring: TWorldRing;


implementation

function TWorldVisPoint.Distancefrom(wvp: TWorldVisPoint): double;
begin
  RESULT := ((wvp.x - x) * (wvp.x - x)) + ((wvp.y - y) + (wvp.y - y));
  if RESULT > 0.0001 then
    RESULT := sqrt(RESULT);
end;

procedure TWorldRing.generateBaseWorldVisualizer;
var
  t, u, w, h: ni;
  xx, yy: double;
begin
  w := 600;
  h := 300;
  for u := 0 to h - 1 do
  begin
    for t := 0 to w - 1 do
    begin
      xx := ((t / w) * 360) - 180;
      yy := ((u / h) * 180) - 90;
      fbmBase.canvas.pixels[t, u] := GTER.GetTopoColor(xx, yy, 7);
    end;
  end;
end;

procedure TWorldRing.Finalize;
begin
  DCS(sect);
  fbm.free;
  fbmBase.free;
end;

function TWorldRing.Hist(rBack: ni): TWorldVisPoint;
begin
  RESULT := ring[((ridx - (rBack + 1)) + length(ring)) mod length(ring)];
end;

procedure TWorldRing.Init;
begin
  ICS(sect);
  ridx := 0;
  fbm := TFastBitmap.create;
  fbm.width := 600;
  fbm.Height := 300;
  fbm.new;
  fbmBase := TFastBitmap.copycreate(fbm);
end;

procedure TWorldRing.Inject(wvp: TWorldVisPoint);
begin
  if (not InRange(wvp.x, -0.00001, 0.00001)) and
    (not InRange(wvp.x, -0.00001, 0.00001)) then
  begin
    Lock;
    wvp.px := round(((wvp.x + 180) / 360) * (fbm.width));
    wvp.py := round((1 - (wvp.y + 90) / 180) * (fbm.Height));
    ring[ridx] := wvp;
    ridx := (ridx + 1) mod length(ring);
    Unlock;
  end;
end;

procedure TWorldRing.Lock;
begin
  // ECS(sect);
end;

procedure TWorldRing.Renderto(bm: TPicture);
var
  t: ni;
  tt, uu: ni;
  wvp: TWorldVisPoint;
  rgb, rgb2: TNativeFloatColor;
  r: single;
  xxx, yyy, zzz, latmin, latmax, lonmin, lonmax, lat, lon: double;
  tm: ticker;
  bZoomOut: boolean;
  wvpCurrent, wvpRoot: TWorldVisPoint;
  span: ni;
  n: double;
begin
  if (ridx_last_rendered = ridx) then
  begin
    if (slsat = 300) then
      exit;
  end
  else
    slsat := 0;
  try
    ridx_last_rendered := ridx;
    Lock;
    lonmin := 179.999;
    lonmax := -179.999;
    latmin := 89.999;
    latmax := -89.999;
    span := 0;
    wvpRoot := wring.Hist(1);
    for t := 0 to length(ring) - 1 do
    begin
      wvpCurrent := wring.Hist(1 + t);
      if wvpCurrent.Distancefrom(wvpRoot) > 3 then
      begin
        break;
      end;
      wvpRoot := wvpCurrent;
      inc(span);
    end;
    for t := 0 to span - 1 do
    begin
      wvpCurrent := wring.Hist(1 + t);
      if (wvpCurrent.x < lonmin) and
        (not InRange(wvpCurrent.x, -0.000001, 0.000001)) then
        lonmin := wvpCurrent.x;
      if (wvpCurrent.x > lonmax) and
        (not InRange(wvpCurrent.x, -0.000001, 0.000001)) then
        lonmax := wvpCurrent.x;
      if (wvpCurrent.y < latmin) and
        (not InRange(wvpCurrent.y, -0.000001, 0.000001)) then
        latmin := wvpCurrent.y;
      if (wvpCurrent.y > latmax) and
        (not InRange(wvpCurrent.y, -0.000001, 0.000001)) then
        latmax := wvpCurrent.y;
    end;
    xxx := (lonmin + lonmax) / 2;
    yyy := (latmin + latmax) / 2;
    zzz := greaterof((latmax - latmin) * 2, (lonmax - lonmin));
    if zzz < 0.1 then
      zzz := 0.1;
    bZoomOut := (zzz > 45) or (zzz = 0) or (span < 10);
    if bZoomOut then
      span := 1000;
    lonmin := xxx - (zzz / 2);
    lonmax := xxx + (zzz / 2);
    latmin := yyy - (zzz / 4);
    latmax := yyy + (zzz / 4);
    // if lonmin < -179.999 then lonmin := -179.999;
    // if latmin < -89.999 then latmin := -89.999;
    // if lonmax > 179.999 then lonmax := 179.999;
    // if latmax > 89.999 then latmax := 89.999;
    if bZoomOut then
    begin
      lonmin := -179.999;
      lonmax := 179.999;
      latmin := -89.999;
      latmax := 89.999;
    end;
    tm := GetTicker;
    if bZoomOut then
    begin
      for uu := lastbreak to fbm.Height - 1 do
      begin
        lastbreak := 0;
        inc(slsat);
        if gettimesince(tm) > 75 then
        begin
          lastbreak := uu;
          break;
        end;
        for tt := 0 to fbm.width - 1 do
        begin
          fbm.canvas.pixels[tt, uu] := fbmBase.canvas.pixels[tt, uu]
        end;
      end;
    end
    else
    begin
      for uu := lastbreak to fbm.Height - 1 do
      begin
        lastbreak := 0;
        inc(slsat);
        lat := interpolate(uu, latmax, latmin, 0, fbm.Height - 1);
        if gettimesince(tm) > 75 then
        begin
          lastbreak := uu;
          break;
        end;
        for tt := 0 to fbm.width - 1 do
        begin
          lon := interpolate(tt, lonmin, lonmax, 0, fbm.width - 1);
          fbm.canvas.pixels[tt, uu] := GTER.GetTopoColor(lon, 0 - lat, 7);
        end;
      end;
    end;
    for t := 0 to span do
    begin
      wvp := Hist(t + 1);
      wvp.px := round(NanFix(interpolate(wvp.x, 0, 599, lonmin, lonmax),0));
      wvp.py := round(NanFix(interpolate(wvp.y, 299, 0, latmin, latmax),0));
      for uu := -3 to +3 do
      begin
        for tt := -3 to +3 do
        begin
          rgb.FromColor(fbm.canvas.pixels[wvp.px + tt, wvp.py + uu]);
          r := (sqrt((tt * tt) + (uu * uu))) / 3;
          if r > 1 then
            r := 1;
          rgb2.FromColor(colorblend(clblack, $1010FF, (1 - r)));
          rgb := rgb + rgb2;
          fbm.canvas.pixels[wvp.px + tt, wvp.py + uu] := rgb.ToColor;
        end;
      end;
    end;
    Unlock;
    fbm.AssignToPicture(bm);
  finally
    // fbm2.free;
  end;
end;

procedure TWorldRing.Unlock;
begin
  // LCS(sect);
end;

initialization
  Debug.log('nil',nil,'whatever');


end.
