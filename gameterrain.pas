unit gameterrain;
dont use me
{$DEFINE USE_COMMANDS}
{xDEFINE DELETE_VHD}

interface

uses
  graphics, tickcount, debug, math, numbers, stringx, sysutils, typex,systemx, classes, terraindata, osm, hgt_sampler, sharedobject, commandprocessor, virtualdisk, easyimage, types,  colorconversion, multibuffermemoryfilestream, helpers.stream, advancedgraphics, maptilesampler, osm_renderer,
  bboxlock, osm_xml_parser, rdtpdb, osmdb, fastbitmap, colorblending;
const
  SHADOW_METER_MAX = 100;
  ELEVATION_MAX = 8000;
  CURRENT_VERSION = 5;
  OSM_SEARCH_RADIUS = 6;
  TILE_WIDTH = 0.001;
  //TILE_WIDTH = 0.00025;
  TILE_RADIUS = TILE_WIDTH /2;
  MULT = 0.0005 / TILE_RADIUS;
  TILE_ZOOM_START = 11;
  TILE_ZOOM_END = 10;
type
  Tcmd_RenderArea = class(TCommand)
  strict private
    debugmap: TFastBitmap;
    bDebugDirty: boolean;
  protected
    randoms: array[0..255] of array[0..255] of TPoint;
    procedure GenerateRandoms;
  public
    fbmx,fbmy: double;
    destructor Destroy;override;

    function GetDebugMap: TFastBitmap;
    procedure DoExecute;override;
  end;


  TGameTileSampler = class(TSharedObject)
  private
    FTopos: array [0..((8*12)-1)] of Tmultibuffermemoryfilestream;
    function GetColorMatchIndex(c: TColor): TColor;

    function SelectTopoFile(x,y, month: nativeint): Tmultibuffermemoryfilestream;
  public
    vhd: TVirtualDisk_SimpleNonLinear;
    topo: TFastBitmap;
    constructor Create;override;
    destructor Destroy;override;
    procedure LoadNasaImages;
    procedure UnloadNasaImages;

    function ManufactureVErsion(version: ni; long, lat: double;  td: TTileData; c: TCommand = nil): TTileData;
    function ManufactureRoads(long, lat: double; gridsize: ni;  td: TTileData; c: Tcommand = nil): TFastBitmap;
    function ManufactureData(long, lat: double;c: TCommand = nil): TTileData;
    function ManufactureData_v2(long, lat: double; td: TTileData; c: TCommand = nil): TTileData;
    function ManufactureData_v3(long, lat: double;  radius: nativeint;  td: TTileData; c: TCommand = nil): TTileData;
    procedure WriteTileDataToVHD(long, lat: double; td: TTileDAta);
    function GetTileDataFromVHD(long, lat: double; c: TCommand = nil): TTileData;
    function GEtTileData(long, lat: double; version: ni; c: TCommand = nil): TTileData;
    function GetTileDataVHDLocation(long, lat: double): int64;
    function GetTopoColor(long, lat: double; m: ni): Tcolor;
    function GetAverageTopoColor(long, lat: double): TColor;
    function IsWater(long, lat: double): boolean;
    function IsWaterColor(c: TColor): boolean;
    function IsLand(long, lat: double): boolean;
  end;




  Tcmd_GetGameTile = class(TCommand)
  private
    Flat: double;
    FLocalx: nativeint;
    FLocaly: nativeint;
    Flong: double;
    Fresult: TTileData;
    Ftoversion: ni;
  public
    property toversion: ni read Ftoversion write FToVErsion;
    property long: double read Flong write FLong;
    property lat: double read Flat write FLat;
    property localx: nativeint read FLocalx write FLocalx;
    property localy: nativeint read FLocaly write FLocaly;
    property result: TTileData read Fresult;
    procedure InitExpense;override;
//    procedure InitOptimizationGroup;override;
    procedure DoExecute;override;
  end;

function TerTypeToColor(td: TTileData): tColor;

var
  GTER: TGameTileSAmpler;
  GBIN: TBinSampler;
  GHGT: THGTSAmpler;
  GMTS: TMapTileSampler;
  GBBOX: TBBOxLocker;

implementation

{ TGameTileSampler }

constructor TGameTileSampler.Create;
var
  s: string;
begin
  inherited;
  s := 'h:\terraindata\world.vhd';
{$IFDEF DELETE_VHD}
  if fileexists(s) then
    deletefile(s);
{$ENDIF}
  vhd := TVirtualDisk_SimpleNonLinear.Create;
  vhd.FileName := s;
  vhd.Size := sizeof(TtileData)*(int64(360000*5)*int64(180000*5));
  LoadNasaImages;


//  FTopos[1] := TMultibuffermemoryfilestream.create('h:\terraindata\topo\B1.png', fmOpenRead+fmShareDenyWrite);
//  FTopos[2] := TMultibuffermemoryfilestream.create('h:\terraindata\topo\C1.png', fmOpenRead+fmShareDenyWrite);
//  FTopos[3] := TMultibuffermemoryfilestream.create('h:\terraindata\topo\D1.png', fmOpenRead+fmShareDenyWrite);
//  FTopos[4] := TMultibuffermemoryfilestream.create('h:\terraindata\topo\A2.png', fmOpenRead+fmShareDenyWrite);
//  FTopos[5] := TMultibuffermemoryfilestream.create('h:\terraindata\topo\B2.png', fmOpenRead+fmShareDenyWrite);
//  FTopos[6] := TMultibuffermemoryfilestream.create('h:\terraindata\topo\C2.png', fmOpenRead+fmShareDenyWrite);
//  FTopos[7] := TMultibuffermemoryfilestream.create('h:\terraindata\topo\D2.png', fmOpenRead+fmShareDenyWrite);








end;

destructor TGameTileSampler.Destroy;
begin
  topo.free;
  vhd.Free;
  inherited;
end;

function TGameTileSampler.GEtTileData(long, lat: double; version: ni;
  c: TCommand): TTileData;
var
  tver: ni;
begin
  result :=GetTileDataFromVHD(long, lat, c);
  if (result.TerrainType = 0) or (not (result.IsChecksumValid)) or (result.Version < version) then begin
    if not result.ischecksumvalid then
      result.version := 0;
    tver := result.version+1;
    while tver <= version do begin
      result := manufactureversion(version, long, lat, result, c);
      result.Version := tver;
      inc(tver);
    end;
    result.SetChecksum;
    WriteTileDataToVHD(long, lat, result);
  end;

end;

function TGameTileSampler.GEtTileDataFromVHD(long, lat: double;
  c: TCommand): TTileData;
var
  iLoc: int64;
  td: TTileData;
begin
  lock;
  try
    iLoc := self.GetTileDataVHDLocation(long, lat);
    vhd.flexRead(iLoc, sizeof(td), @td);
//    if not td.IsChecksumValid then
//      debug.consolelog('wtf read');
    result := td;
  finally
    unlock;
  end;
end;

function TGameTileSampler.GetTileDataVHDLocation(long, lat: double): int64;
begin
  result := round((lat+180) * (1000*(0.001/TILE_WIDTH)) * 360000);
  result := result + round((long+90)*(1000/TILE_WIDTH));
  result := result * sizeof(TTileData);


end;

function TGameTileSampler.GetTopoColor(long, lat: double; m: ni): Tcolor;
var
  xx,yy: nativeint;
  xo,yo: nativeint;
  fs: TMultibuffermemoryfilestream;
  c,c1,c2,c3,c4: TColor;
  cs: array[0..11] of TColor;
  lrblend, udblend: double;
  fxx, fyy: double;
begin
  fxx := long;
  while fxx >= 180 do fxx := fxx - 360;
  while fxx < -180 do fxx := fxx + 360;

  fxx := (21600*4) * ((fxx+180)/360);
  fyy := (21600*2) * ((lat+90)/180);
  xx := round(floor(fxx));
  yy := round(floor(fyy));

  xo := xx mod 21600;
  yo := yy mod 21600;

  if yo >= 21600 then yy := 21599;
  if xo >= 21600 then xx := 21599;


  result := clBlack;
  Lock;
  try
    fs := SelectTopoFile(xx,yy,m);
    if fs = nil then begin
      result := clBlack;
      exit;
    end;
    fs.Seek(((yo*21600)*sizeof(TColor)) + (xo * sizeof(TColor)), soBeginning);
    //get colors
    Stream_guaranteeread(fs, @c1, sizeof(c));
    if (xo=21599) then
      c2 := c1
    else
      Stream_guaranteeread(fs, @c2, sizeof(c));

    fs.Seek((((lesserof(yo+1, 21599)*21600)*sizeof(TColor)) + (xo * sizeof(TColor))), soBeginning);
    Stream_guaranteeread(fs, @c3, sizeof(c));
    if (xo=21599) then
      c4 := c3
    else
      Stream_guaranteeread(fs, @c4, sizeof(c));

    //blend colors
    lrblend := fxx-negfloor(fxx);
    udblend := fyy-negfloor(fyy);

    c1 := colorblend(c1,c2, lrblend);
    c3 := colorblend(c3,c4, lrblend);
    result := colorblend(c1,c3, udblend);
//    result := (result and $FFFF) + (lesserof($FF, (result shr 16) * 2) shl 16)

  finally
    Unlock;
  end;




//  result := easyimage.CanvasResample(topo.Canvas, xx mod 21600,yy mod 21600);
//
end;

function TGameTileSampler.IsLand(long, lat: double): boolean;
var
  c1,c2,c3: TColor;
  t: ni;
begin
  for t := TILE_ZOOM_START downto TILE_ZOOM_END do begin
    c1 := GMTS.GetMapPixel(long, 0-lat, t);
    if GetColorMatchIndex(c1) = clGreen then begin
      result := true;
      exit;
    end;
    if GetColorMatchIndex(c1) = clBlue then begin
      result := false;
      exit;
    end;
  end;

  result := true;






end;

function TGameTileSampler.IsWater(long, lat: double): boolean;
var
  t,tt: ni;
  c: TColor;
begin
  result := false;
  for t:= 5 to (5+11) do begin
    tt := t mod 12;//<---doing this so that we can search from summer to summer
    c := GetColorMatchIndex(NormalizeColor(GetTopoColor(long, lat, tt)));
    if IsWaterColor(c) then begin
      result := true;
      break;
    end;
  end;

end;

function TGameTileSampler.IsWaterColor(c: TColor): boolean;
begin
  result := (c=clBlue) or (c=clTeal) or (c=clBlack);
end;

procedure TGameTileSampler.LoadNasaImages;
{
http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73938/world.200401.3x21600x21600.A1.png
http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73967/world.200402.3x21600x21600.A1.png
http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73992/world.200403.3x21600x21600.A1.png
http://eoimages.gsfc.nasa.gov/images/imagerecords/74000/74017/world.200404.3x21600x21600.A1.png
http://eoimages.gsfc.nasa.gov/images/imagerecords/74000/74042/world.200405.3x21600x21600.A1.png
http://eoimages.gsfc.nasa.gov/images/imagerecords/76000/76487/world.200406.3x21600x21600.A1.png
http://eoimages.gsfc.nasa.gov/images/imagerecords/74000/74092/world.200407.3x21600x21600.A1.png
http://eoimages.gsfc.nasa.gov/images/imagerecords/74000/74117/world.200408.3x21600x21600.A1.png
http://eoimages.gsfc.nasa.gov/images/imagerecords/74000/74142/world.200409.3x21600x21600.A1.png
http://eoimages.gsfc.nasa.gov/images/imagerecords/74000/74167/world.200410.3x21600x21600.A1.png
http://eoimages.gsfc.nasa.gov/images/imagerecords/74000/74192/world.200411.3x21600x21600.A1.png
http://eoimages.gsfc.nasa.gov/images/imagerecords/74000/74218/world.200412.3x21600x21600.A1.png
}
const
  NASA_DOC_IDS_FOR_MONTH:array [0..11] of integer = (73938, 73967, 73992, 74017, 74042, 76487,74092,74117,74142,74167,74192,74218);
var
  sFile: string;
  month, tile: ni;
  c1,c2: char;
  idx: ni;
begin
  idx := 0;
  for tile := 0 to 7 do begin
//    if tile > 0 then continue;
    //this should make A1,B1,C1,D1,A2,B2,C2,D2
    c1 := char(ord('A')+(tile mod 4));
    c2 := char(ord('1')+(tile div 4));
    for month := 0 to 11 do begin
//      if month <> 7 then continue;
      sFile := 'h:\terraindata\topo\world.2004'+padstring(inttostr(month+1), '0', 2)+'.3x21600x21600.'+c1+c2+'.png.raw';
      FTopos[idx] := TMultibufferMemoryFileStream.create(sFile, fmOpenRead+fmShareDenyWRite);
      Ftopos[idx].BufferSize := 100000;
      inc(idx);
    end;
  end;

//  FTopos[0] := TMultibufferMemoryFileStream.create('h:\terraindata\topo\A1x.raw', fmOpenRead+fmShareDenyWRite);
end;

function TGameTileSampler.GetAverageTopoColor(long, lat: double): TColor;
var
  res: TNativeFloatColor;
  m: ni;
begin
  res.FromColor(clBlack);

  for m := 5 to 9 do begin
    res := res + GetTopoColor(long, lat, m);
  end;

  res := res / 5;
  result := res.ToColor;


end;

function TGameTileSampler.GetColorMatchIndex(c: TColor): TColor;
const
//  primary_hues: array[0..8] of TColor = ($FF, $7fFF, $FFFF, $FF00, $FFFF00, $FF0000, $FF009F, $FF00FF,$FFFFFF);
  primary_hues: array [0..9] of TColor = (COLOR_MOTORWAY, COLOR_PRIMARy, COLOR_SECONDARY, COLOR_URBAN, COLOR_WATER, clBlack, clGreen, $00FF7f, clPurple, clMagenta);
//  priamry_hues: array[0..5] of TColor = (COLOR_MOTORWAY, COLOR_PRIMARy, COLOR_SECONDARY, COLOR_URBAN, COLOR_WATER, clBlack);
var
  hsl_img, hsl_primary: THSLnativefloatColor;
  rr,x,y,t: integer;
  iMax1, iMax2: integer;
  hsl_primarys: array[0..9] of THSLnativefloatColor;
  a: array[0..9] of TColorFloat;
begin
  for t:= low(primary_hues) to high(primary_hues) do begin
    hsl_primarys[t] := RGBtoHSL(ColorTonativefloatRGB(primary_hues[t]));
  end;

  //c := NormalizeColor(c);
  hsl_img.FromColor(c);


  //go through colors and determine error (distance from target hue)
  for t := low(primary_hues) to high(primary_hues) do begin
    hsl_primary := hsl_primarys[t];
    if hsl_img.s < 0.01 then
      a[t] := 99999999
    else
      a[t] := abs(hsl_primary.h-hsl_img.h);///(hsl_img.s);
  end;

  iMax1 := 0;
  for t := 1 to high(primary_hues) do begin
    if a[t] < a[iMax1] then
      iMax1 := t;

  end;
  if (hsl_img.s < 0.01) or (hsl_img.l < 0.01) then
    iMax1 := 0;

  result := primary_hues[iMax1];
end;


function TGameTileSampler.ManufactureData(long, lat: double;c: TCommand = nil): TTileData;
var
  bin: TBinSAmpler;
  o: TOSM_Getter;
  //xml: string;
  ccc,cc: TColor;
  rad_multiplier: ni;
  bLand: boolean;
  roads: TFastBitmap;
  i: ni;
begin

  result.TerrainType := 0;
  gbin.Lock;
  try
    result.elevation := gbin.GetSample(long, lat);
  finally
    gbin.unlock;
  end;
  if result.elevation < 0 then
    result.elevation := 0;


  bLand := Self.IsLand(long, lat);
  if bLand then
    result.TerrainType := TER_GRASS
  else
    result.TerrainType := TER_WATER;

  result.TerrainBase := result.TerrainType;
  result.topocolor := clBlack;
  ccc := GetAverageTopoColor(long, lat);
  result.TopoColor := ccc;


  if assigned(c) then begin
   c.Resources.SetResourceUsage('bin', 0.0);
  end;
end;

function TGameTileSampler.ManufactureData_v2(long, lat: double; td: TTileData;
  c: TCommand): TTileData;
var
  n: single;
begin
  result := td;
  ghgt.lock;
  try
    n := ghgt.GetReSample(long, lat);
    if n >= 0 then begin
      result.Elevation := n;
    end;

  finally
    ghgt.Unlock;
  end;



end;

function TGameTileSampler.ManufactureData_v3(long, lat: double; radius: nativeint;  td: TTileData;
  c: TCommand): TTileData;
var
  bin: TBinSAmpler;
  o: TOSM_Getter;
  //xml: string;
  ccc,cc: TColor;
  rad_multiplier: ni;
  bLand: boolean;
  roads: TFastBitmap;
  i: ni;
begin
  result := td;
  o := nil;
  try
    if assigned(c) then begin
      c.Resources.SetResourceUsage('bin', 0.0);
    end;
    o := TOSM_Getter.create;
    o.CacheDir := 'h:\terraindata\osm\';
    try


      if result.terraintype = 0 then
        raise Exception.create('wtf');


      roads := nil;
      if td.DeepSearch or (radius = 0) then
        roads := ManufactureRoads(long, lat, radius,td,c);
//      if localx = 60 then
//        debug.ConsoleLog('weeeee');
      if assigned(roads) then begin
        ccc := roads.Canvas.pixels[roads.Width div 2, roads.Height div 2];
        cc := GetColorMatchIndex(ccc);

        case cc of
          COLOR_MOTORWAY:  result.TerrainType := TER_ROAD_MOTORWAY;
          COLOR_PRIMARY: result.TerrainType := TER_ROAD_PRIMARY;
          COLOR_SECONDARY: result.TerrainType := TER_ROAD_SECONDARY;
          COLOR_URBAN: result.TerrainType := TER_URBAN;
          COLOR_WATER: result.TerrainType := TER_RIVER;
        else
          result.terrainType := result.terrainbase;
        end;

        roads.free;
      end;
      if result.terraintype = 0 then
        raise Exception.create('wtf');

    finally
      o.free;
    end;
  finally
    //bin.free;
  end;


      if result.terraintype = 0 then
        raise Exception.create('wtf');


end;

function TGameTileSampler.ManufactureRoads(long, lat: double;
  gridsize: ni; td: TTileData; c: Tcommand = nil): TFastBitmap;
const
  grid_span = 3;
var
  xp: osm_renderer.TOSMRenderer;
  getr: TOSM_Getter;
  t,u: ni;
  tx,ty,e,w,n,s: double;
  sFile: string;
  mb: TMapBox;
  xmld: TXMLDrawWorkingData;
  db: TRDTPDB;
begin
//  if assigned(c) then begin
//    c.CPUExpense := 1.0;
//    c.waitforresources;
//  end;
//  Debug.ConsoleLog('Start Road Generation');
  xp := TOSMRenderer.create;
  try
    xp.constrainproportions := false;
    xp.Width := (GRIDSIZE * 2)+1;
    xp.HEight := (GRIDSIZE * 2)+1;

    xp.BoundX1 := long-(tile_radius*GRIDSIZE);
    xp.BoundX2 := long+(tile_radius*GRIDSIZE);
    xp.BoundY1 := (lat-(tile_radius*GRIDSIZE));
    xp.BoundY2 := (lat+(tile_radius*GRIDSIZE));
//    Debug.ConsoleLog('Center:'+floattostr(xp.centerx)+','+floattostr(xp.centery));

    getr := TOSM_Getter.create;
    getr.CacheDir :='h:\terraindata\osm\';
    try

      for u := 0 to xp.width-1 do begin
        for t := 0 to xp.height-1 do begin
          tx := (u - (xp.width div 2)) * FETCH_TILE_SIZE;
          ty := (t - (xp.height div 2)) * FETCH_TILE_SIZE;
          tx := tx + xp.centerX;
          ty := ty + xp.centery;
//          Debug.ConsoleLog('Tile '+inttostr(t)+','+inttostr(u)+' center is:'+floattostr(tx)+','+floattostr(ty));

          w := tx-(FETCH_TILE_RADIUS*GRID_SPAN);
          e := tx+(FETCH_TILE_RADIUS*GRID_SPAN);
          s := 0-(ty-(FETCH_TILE_RADIUS*GRID_SPAN));
          n := 0-(ty+(FETCH_TILE_RADIUS*GRID_SPAN));
//          Debug.ConsoleLog('wnes: '+floattostr(w)+','+floattostr(n)+','+floattostr(e)+','+floattostr(s));

//          if assigned(c) then begin
//            c.Resources.SetResourceUsage('osm_'+floattostr(w)+','+floattostr(n)+','+floattostr(e)+','+floattostr(s), 1.0);
//          end;

//          getr.GetXMLData(w,n,e,s);
//          if assigned(c) then begin
//            c.Resources.SetResourceUsage('osm'+sFile, 0.0);
//          end;
          xmld := getr.GetSnapBinaryDAta(w,s,e,n, c);

          xp.watch_command := c;
          xp.addfile(w,n,e,s);

        end;
      end;
    finally
      getr.Free;
    end;

//    c.WaitForResources;

    xp.Draw;
    //xp.flip;
    result := xp; //TFastBitmap.CopyCreate(xp);
//    xp.savetofile('h:\debug\'+inttostr(getticker)+'_'+inttostr(getcurrentthreadid)+'.bmp');

  finally
//    xp.free;
  end;
  if assigned(c) then begin
    c.CPUExpense := 0;
//    c.waitforresources;
  end;


end;

function TGameTileSampler.ManufactureVErsion(version: ni; long, lat: double;  td: TTileData; c: TCommand): TTileDAta;
begin
  result := td;
  case version of
    1: result := ManufactureData(long, lat, c);
    2: result := ManufactureData_v2(long, lat, td, c);
    3: result := ManufactureData_v3(long, lat, 0, td, c);
    4: result := ManufactureData_v3(long, lat, 3, td, c);
    5: result := ManufactureData_v3(long, lat, 12, td, c);

  end;


end;

function TGameTileSampler.SelectTopoFile(x,y, month: nativeint): Tmultibuffermemoryfilestream;
var
  xfile, yfile: nativeint;
  idx: nativeint;
  char1, char2: char;
begin
  xfile := x div 21600;
  yfile := y div 21600;
  idx := (yfile * 4)+xfile;

  idx := (idx * 12)+month;
  result := FTopos[idx];




end;


procedure TGameTileSampler.UnloadNasaImages;
var
  t: ni;
  mbfs: TMultiBufferMemoryFileStream;
begin
  for t:= low(FTopos) to high(FTopos) do begin
    mbfs := FTopos[t];
    if mbfs <> nil then
      mbfs.Free;
    FTopos[t] := nil;
  end;

end;

procedure TGameTileSampler.WriteTileDataToVHD(long, lat: double; td: TTileDAta);
var
  iLoc: int64;
  td2: TTileDAta;
begin
  lock;
  try
    iLoc := self.GetTileDataVHDLocation(long, lat);
    vhd.FlexWrite(iLoc, sizeof(td), @td);
    vhd.FlexRead(iLoc, sizeof(td2), @td2);

//    if not td2.IsChecksumValid then
//      debug.consolelog('wtf');
  finally
    unlock;
  end;
end;

{ Tcmd_GetGameTile }

procedure Tcmd_GetGameTile.DoExecute;
var
  gt: TGameTileSampler;
begin
  inherited;
//  gt := TGameTIleSampler.create;
  gt := GTER;
  try
//    if localx = 60 then begin
//      debug.consolelog('poop');
//    end;

    Fresult := gt.GetTileData(long,lat, toversion, self);
  finally
//    gt.free;
  end;

end;

procedure Tcmd_GetGameTile.InitExpense;
begin
  inherited;
  CPUExpense := 0.33;
  MemoryExpense := 1/12;
end;

//procedure Tcmd_GetGameTile.InitOptimizationGRoup;
//begin
//  inherited;
//  case toversion of
//    1: FOptimizationGRoup := inttostr(GBIN.GetFileIndexForCoordinate(self.long, self.lat));
//    2: FOptimizationGRoup := GHGT.GetFileForCoordinate(self.long, self.lat);
//  else
//    FOptimizationGRoup := inttostr(localy);
//  end;
//
//end;

{ Tcmd_RenderArea }

destructor Tcmd_RenderArea.Destroy;
begin
  if assigned(debugmap) then
    debugmap.free;
  debugmap := nil;
  inherited;
end;

procedure Tcmd_RenderArea.DoExecute;
var
  fbm: TFastBitmap;
  rt,ru, t,u,v,tt,uu: nativeint;
  xx,yy: double;
//  ts: TGameTileSampler;
  td,td2: TTileData;
  c: TColor;
  cmd,c2: Tcmd_GetGameTile;
  cp: TCommandProcessor;
  mx, my:ni;
  s: double;
  pt: TPOint;
begin
  inherited;
  generaterandoms;
  cp := TCommandProcessor.create(nil,'gametile');
  try
//    ts := TGameTileSampler.create;
    try

      fbm := TFastBitmap.create;
      try
        fbm.width := 256;
        fbm.height := 256;
        fbm.New;
        stepcount := fbm.height;
//        fbm.AssignToPicture(image1.Picture);
        for v := 1 to CURRENT_VERSION do begin

          //shadows
          if v = 3 then begin
            for u := 1 to fbm.height-1 do begin
              for t:= 1 to fbm.Width-1 do begin
                xx := fbmx + ((t - (fbm.width div 2)) * TILE_WIDTH);
                yy := fbmy + ((u - (fbm.height div 2)) * TILE_WIDTH);


                s := 0.0;
                td := GTER.GetTileDataFromVHD(xx,yy);
                td2 := GTER.GEtTileDataFromVHD(xx-TILE_WIDTH,yy);
                s := s + td.elevation - td2.elevation;
                td2 := GTER.GEtTileDataFromVHD(xx-TILE_WIDTH,yy-TILE_WIDTH);
                s := s + td.elevation - td2.elevation;
                td2 := GTER.GEtTileDataFromVHD(xx,yy-TILE_WIDTH);
                s := s + td.elevation - td2.elevation;
                s := s / shadow_meter_Max;
                s := lesserof(greaterof(s,-1.0),1.0);
                s := (s * 127) + 128;
                td.Shadow := round(s);
                td.SetChecksum;
                GTER.WriteTileDataToVHD(xx,yy,td);
                fbm.Canvas.Pixels[tt,uu] := TerTypeToColor(td);
              end;
            end;
          end else
          //deep flags
          if v = 4 then begin
            for u := 0 to fbm.height-1 do begin
              for t:= 0 to fbm.Width-1 do begin
                xx := fbmx + ((t - (fbm.width div 2)) * TILE_WIDTH);
                yy := fbmy + ((u - (fbm.height div 2)) * TILE_WIDTH);

                td := GTER.GetTileDataFromVHD(xx,yy);
                if td.IsChecksumValid
                and(td.TerrainType <> TER_WATER)
                and (td.TerrainType <> TER_GRASS) then begin
                  for uu := greaterof(0,u-4) to lesserof(fbm.height-1, u+4) do begin
                    for tt := greaterof(0,t-4) to lesserof(fbm.Width-1, t+4) do begin
                      xx := fbmx + ((tt - (fbm.width div 2)) * TILE_WIDTH);
                      yy := fbmy + ((uu - (fbm.height div 2)) * TILE_WIDTH);
                      td := GTER.GetTileDataFromVHD(xx,yy);
                      td.DeepSearch := true;
                      td.SetChecksum;
                      GTER.WriteTileDataToVHD(xx,yy,td);
                      fbm.Canvas.Pixels[tt,uu] := colorblend(fbm.Canvas.Pixels[tt,uu], clRed, 0.01);


                    end;
                  end;

                end;
              end;
            end;
          end else
          //deep flags
          if v = 5 then begin
            for u := 0 to fbm.height-1 do begin
              for t:= 0 to fbm.Width-1 do begin
                xx := fbmx + ((t - (fbm.width div 2)) * TILE_WIDTH);
                yy := fbmy + ((u - (fbm.height div 2)) * TILE_WIDTH);

                td := GTER.GetTileDataFromVHD(xx,yy);
                if td.IsChecksumValid
                and((td.TerrainType = TER_RIVER) or(td.TerrainType = TER_ROAD_MOTORWAY) or (td.TerrainType = TER_ROAD_PRIMARY))
                then begin
                  for uu := greaterof(0,u-4) to lesserof(fbm.height-1, u+4) do begin
                    for tt := greaterof(0,t-4) to lesserof(fbm.Width-1, t+4) do begin
                      xx := fbmx + ((tt - (fbm.width div 2)) * TILE_WIDTH);
                      yy := fbmy + ((uu - (fbm.height div 2)) * TILE_WIDTH);
                      td := GTER.GetTileDataFromVHD(xx,yy);
                      td.DeepSearch := true;
                      td.SetChecksum;
                      GTER.WriteTileDataToVHD(xx,yy,td);
                      fbm.Canvas.Pixels[tt,uu] := colorblend(fbm.Canvas.Pixels[tt,uu], clRed, 0.01);
                    end;
                  end;
                end;
              end;
            end;
          end;

          //---------------

          LOCK;
          try
            debugMap.free;
            debugmap := nil;
            debugmap := Tfastbitmap.CopyCreate(fbm);
          finally
            unlock;
          end;
          status := 'pass #'+inttostr(v);
          for ru := 0 to fbm.height-1 do begin
            step := u;
            for mx:=0 to 0 do
            for rt:= 0 to fbm.Width-1 do begin
              pt := randoms[ru][rt];
              t := pt.x;
              u := pt.y;
              if ((t+mx) mod 1) <> 0 then continue;
              xx := fbmx + ((t - (fbm.width div 2)) * TILE_WIDTH);
              yy := fbmy + ((u - (fbm.height div 2)) * TILE_WIDTH);




              td := gter.GetTileDataFromVHD(xx,yy);
              if td.Version < v then begin
                c2 := nil;
                if true then begin
                  cmd := Tcmd_GetGameTile.create;
                  cmd.long := xx;
                  cmd.lat := yy;
                  cmd.localx := t;
                  cmd.localy := u;
                  CMD.toversion := v;
                  if c2 <> nil then
                    cmd.AddDependency(c2);
                  cmd.start(cp);
                  c2 := cmd;
                end;
              end
              else begin
                c := TerTypeToColor(td);
                fbm.Canvas.Pixels[t,u] := c;
                LOCK;
                try
                  debugMap.free;
                  debugmap := nil;
                  debugmap := Tfastbitmap.CopyCreate(fbm);
                finally
                  unlock;
                end;
              end;
            end;

            //finish commands
            if ((u mod 16) = 0 ) or (u=fbm.height-1) then
            while not cp.IsComplete do begin
              if cp.CompleteCount = 0 then
                sleep(100)
              else
              for t:= cp.completecount-1 downto 0 do begin
//                raise Exception.create('fixme');
                cmd := Tcmd_GetGameTile(cp.CompleteCommands[t]);


                if cmd.localx=60 then
                  debug.ConsoleLog('weeee');
                cmd.memoryexpense := 1.0;

                td := cmd.result;
//                if cmd.result.TerrainType = 0 then
//                  raise exception.create('wtf');
                c := TerTypeToColor(td);
                fbm.Canvas.Pixels[cmd.localx,cmd.localy] := c;
                LOCK;
                try
                  debugMap.free;
                  debugmap := nil;
                  debugmap := Tfastbitmap.CopyCreate(fbm);
                finally
                  unlock;
                end;



                while cmd.thread <> nil do
                  sleep(0);
                cmd.Free;
              end;




            end;


          end;
        end;
      finally
        fbm.free;
      end;
    finally
//      ts.free;
    end;
  finally
    cp.free;
  end;
end;

procedure Tcmd_RenderArea.GenerateRandoms;
var
  t,u,tt,uu,n: ni;
  pt: TPoint;
begin
  for u := 0 to 255 do begin
    for t := 0 to 255 do begin
      randoms[u][t].x := t;
      randoms[u][t].y := u;
    end;
  end;
  exit;
  for n:= 0 to 256*8 do begin
    t := random(255);
    u := random(255);
    tt := random(255);
    uu := random(255);
    pt := randoms[uu][tt];
    randoms[uu][tt] := randoms[u][t];
    randoms[u][t] := pt;
  end;

end;

function Tcmd_RenderArea.GetDebugMap: TFastBitmap;
begin
  Lock;
  try
    result := TFastBitmap.copycreate(debugMap);
  finally
    Unlock;
  end;

end;

function TerTypeToColor(td: TTileData): tColor;
var
  c: Tcolor;
  d: double;
begin
  c := clBlack;
  case td.TerrainType of
    TER_UNDEFINED: c := clMagenta;
    TER_WATER : c := clNavy;
    TER_RIVER : c := colorblend(clNavy, clCyan, 0.5);
    TER_GRASS : c := clBlack;
    TER_ROAD_MOTORWAY : c := clWhite;
    TER_ROAD_TRUNK : c := clCyan;
    TER_ROAD_PRIMARY : c := $dddddd;
    TER_ROAD_SECONDARY : c:= $cccccc;
    TER_ROAD_TERTIARY : c:= $aaaaaa;
    TER_URBAN : c := $aaaaaa;
  else
    c := random($FFFFFF);
  end;
  if td.Shadow = 0 then begin
    if td.TerrainType = TER_GRASS then
      result := colorblend(c, colorblend(td.TopoColor, clWhite, lesserof(td.Elevation,ELEVATION_MAX)/ELEVATION_MAX), 0.8)
    else
      result := colorblend(c, colorblend(td.TopoColor, clWhite, lesserof(td.Elevation,ELEVATION_MAX)/ELEVATION_MAX), 0.5);
  end else begin
    if c = clBlack then
      c := td.TopoColor;

    d := (td.Shadow - 128)/127;
    if d < 0 then
      c:= colorblend(c, clBlack, (0-d)*0.3)
    else
      c := colorblend(c, clWhite, d*0.8);

    result := c;


  end;
//  if not ((result =clGreen) or (result= clBlue)) then
//    raise Exception.create('wtf');
end;


initialization
{$IFNDEF DISABLE_GAMETERRAIN}
GTER := TGameTileSampler.create;
GBIN := TBinSampler.create;
GBIN.LoadFiles('h:\terraindata\ascii\');
GMTS := TMapTileSampler.create;
GHGT := THGTSAmpler.create;
GOSM := TOSM_Getter.create;
GBBOX := TBBoxLocker.create;
{$ENDIF}

finalization

GTER.free;
GBIN.free;
GMTS.free;
GHGT.free;
GOSM.free;
GBBOX.free;


end.
