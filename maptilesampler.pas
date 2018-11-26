unit maptilesampler;

interface

uses
  numbers, sharedobject, typex, easyimage, stringx, systemx, sysutils, math, httpclient, commands_http, graphics, fastbitmap, colorconversion, colorblending;
const
  ZOOM_DEGREES : array[0..19] of nativefloat =
//    (360.0, 180.0,90.0, 45.0, 22.5, 11.25, 5.625,   2.813,   1.406,   0.703, 0.352, 0.176, 0.088, 0.044, 0.022, 0.011, 0.005, 0.003, 0.001, 0.0005);
  (360.0/1,360.0/2,360.0/4,360.0/8,360.0/16,360.0/32,360.0/64,360.0/128,360.0/256,360.0/512,360.0/1024,360.0/2048,360.0/4096,360.0/8192,360.0/16384,360.0/32768,360.0/65536,360.0/(65536*2),360.0/(65536*4),360.0/(65536*8));


type
  TZoomCacheEntry = record
    cx,cy: double;
    zoom: ni;
    bm: TFastBitmap;
    function WorldSample(lon, lat: double): TColor;
    procedure Clear;
  end;

  TMapTileSampler = class(TSharedObject)
  private
    zoomcache: array[0..19] of TZoomCacheEntry;
    procedure DestroyZoomCache;
    function GetZoomTileCenterX(long: double; zoomlevel: ni): double;
    function GetZoomTileCenterY(lat: double; zoomlevel: ni): double;

    procedure DownloadFile(long, lat: double; zoomlevel: ni);
    function GetCacheFileName(long, lat: double; zoomlevel: ni; width, height: ni): string;
    procedure LoadCacheTile(long, lat: double; zoomleve: ni);
  public
    CAcheDir: string;
    procedure Init;override;
    function GetMapTile(long, lat: double; zoomlevel: ni): TFastBitmap;
    function GetMapPixel(long, lat: double; zoomlevel: ni): TColor;
    function GetConvergedMapPixel(long, lat: double; lowzoom, highzoom: ni): TColor;
  end;


implementation

{ TMapTileSampler }

procedure TMapTileSampler.DestroyZoomCache;
var
  t: ni;
begin
  for t:= low(zoomcache) to high(zoomcache) do begin
    zoomcache[t].Clear;
  end;

end;

procedure TMapTileSampler.DownloadFile(long, lat: double; zoomlevel: ni);
var
  c: Tcmd_HTTPDownLoadToFile;
begin
  c := Tcmd_HTTPDownLoadToFile.create;
  try
    c.Resources.SetResourceUsage('http',1.0);
    c.waitforresources;
    c.URL := 'http://api.tiles.mapbox.com/v3/lazorjason.i502ml0f/'+
              floattostr(long)+','+floattostr(lat)+','+inttostr(zoomlevel)+'/256x256.png';
    c.LocalFile := GetCacheFileName(long, lat, zoomlevel, 256, 256);
    forcedirectories(extractfilepath(c.LocalFile));
    c.start;
    c.WaitFor;
  finally
    c.free;
  end;


end;

function TMapTileSampler.GetCacheFileName(long, lat: double;
  zoomlevel: ni; width, height: ni): string;
var
  sFolder, sFile: string;
begin
  sFolder := slash(cachedir)+inttostr(zoomlevel)+'\'+inttostr(round(floor(long)))+'\'+inttostr(round(floor(lat)))+'\';
  sFile := floattostr(long)+'_'+floattostr(lat)+'_'+inttostr(zoomlevel)+'_'+inttostr(width)+'x'+inttostr(height)+'.png';
  result :=sFolder+sFile;
end;

function TMapTileSampler.GetConvergedMapPixel(long, lat: double; lowzoom,
  highzoom: ni): TColor;
var
  t: ni;
  r,nfc: TnativefloatColor;
begin
  r.FromColor(clBlack);
  for t := lowzoom to highzoom do begin
    nfc.FromColor(GEtMapPixel(long, lat, t));
    r := r + nfc;
  end;
  r := r/(1+(highzoom-lowzoom));
  result := r.ToColor;

end;

function TMapTileSampler.GetMapPixel(long, lat: double; zoomlevel: ni): TColor;
begin
  Lock;
  try
    LoadCacheTile(long, lat, zoomlevel);
    result := zoomcache[zoomlevel].WorldSample(long, lat);
  finally
    Unlock;
  end;

end;

function TMapTileSampler.GetMapTile(long, lat: double;
  zoomlevel: ni): TFastBitmap;
var
  s: string;
begin
  Lock;
  try
    s := Getcachefilename(long, lat, zoomlevel, 256,256);
    while not fileexists(s) do begin
      Downloadfile(long,lat,zoomlevel);
      if not fileexists(s) then
        sleep(10000);
    end;

    result := TFastBitmap.create;
    result.loadfromfile(s);


  finally
    Unlock;
  end;

end;

function TMapTileSampler.GetZoomTileCenterX(long: double; zoomlevel: ni): double;
begin
  result := snapto(long, zoom_degrees[zoomlevel]);
end;

function TMapTileSampler.GetZoomTileCenterY(lat: double; zoomlevel: ni): double;
begin
  result := snapto(lat, zoom_degrees[zoomlevel]/2);
end;

procedure TMapTileSampler.Init;
begin
  inherited;
  CacheDir:= 'h:\terraindata\tiles\';
end;

procedure TMapTileSampler.LoadCacheTile(long, lat: double; zoomleve: ni);
begin
  long := GetZoomTileCenterX(long, zoomleve);
  lat := getzoomtilecentery(lat, zoomleve);
  if (long = zoomcache[zoomleve].cx) and (lat = zoomcache[zoomleve].cy) then
    exit;

  zoomcache[zoomleve].clear;

  zoomcache[zoomleve].cx := long;
  zoomcache[zoomleve].cy := lat;
  zoomcache[zoomleve].bm := GetMapTile(long, lat, zoomleve);
  zoomcache[zoomleve].zoom := zoomleve;

end;

{ TZoomCacheEntry }

procedure TZoomCacheEntry.Clear;
begin
  bm.free;
  bm := nil;
end;

function TZoomCacheEntry.WorldSample(lon, lat: double): TColor;
var
  lx,ly: double;
  s: double;
begin
  S:=cos(degtorad(cy));
  lx := lon-cx;
  lx := 127.5 * (lx/(ZOOM_DEGREES[zoom]/2));
  ly := (lat-cy);
  ly := 127.5 * (ly/(s*(ZOOM_DEGREES[zoom]/2)));
  ly := 0-ly;


  //ly := ly * s;

  if assigned(bm) then
    result := CanvasResample(bm.Canvas, lx+128.0, ly+128.0)
  else
    result := 0;



end;

initialization

end.
