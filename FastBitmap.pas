unit FastBitmap;

interface
{$I DELPHIDEFS.inc}

uses
{$IFNDEF FMX}
  graphics,helpers.stream,
  //pngimage_fixed,
{$ELSE}
  helpers.stream,
  uitypes,
{$ENDIF}
  debug,endian, vcl.imaging.pngimage, vcl.imaging.jpeg,
  betterobject, sharedobject, graphicsx, classes, geometry, types, colorconversion, typex, numbers, sysutils, stringx.ansi, systemx, colorblending;

const
  FBM_FMT_RAW_ARGB = 1;
  FMX_FMT_COMPRESSED_ARGB = 2;
type
  TlocalFileStream = TFileStream;

  TFBMHeader = packed record
    a: int64;
    width, height: int64;
    format: cardinal;
    format_size: cardinal;
    stream_size: cardinal;
    procedure Init;
  end;
  TFastBitmap = class;//forward

  TFakeBrush = record
    color: TColor;
  end;

  TFontCache = class(TSharedObject)
  protected
    FFonts: TStringlist;
    procedure CacheFont(sFile: string);
  public
    constructor Create;override;
    destructor Destroy;override;

    function LoadFont(sfile: string): TFastBitmap;
  end;

  TFontinfo = record
    charwidth, charheight: nativeint;
    charsacross, charsvertical: nativeint;
    FFont: TFastBitmap;
    fileName: string;
    function GetSourceRect(c: char): TRect;
    procedure LoadFont(sFile: string);
    procedure Cleanup;
  end;

  TFastCanvas = class(TSharedobject)
  private
    [weak] FOwnerHold: TFastBitmap;
    [unsafe] FOwner: TFastBitmap;
    FFontInfo: TFontInfo;
{$IFDEF FMX}
    function GetAlphaPixels(x, y: integer): TAlphaColor;
    procedure SetAlphaPixels(x, y: integer; const Value: TAlphaColor);
    function GetAlphaPixelsWrap(x, y: integer): TAlphaColor;inline;
    procedure SetAlphaPixelsWrap(x, y: integer; const Value: TAlphaColor);inline;

{$ENDIF}
    function GetPixels(x, y: integer): TColor;
    procedure SetPixels(x, y: integer; const Value: TColor);
    function GetHSLPixels(x, y: integer): THSLNativefloatColor;
    procedure SetHSLPixels(x, y: integer; const Value: THSLNativefloatColor);
    procedure SetOwner(const Value: TFastBitmap);

  public
    brush: TFakeBrush;
    pen: TfakeBrush;
    penpos: TPoint;
    textoffset: TPOint;
    textpos: TPoint;
{$IFDEF USE_TRANSLATION}
    Translation: TPoint;
{$ENDIF}
    destructor Destroy;override;
    property Owner: TFastBitmap read FOwner write SetOwner;
    property Pixels[x,y: integer]: TColor read GetPixels write SetPixels;
{$IFDEF FMX}
    property AlphaPixels[x,y: integer]: TAlphaColor read GetAlphaPixels write SetAlphaPixels;
    property AlphaPixelsWrap[x,y: integer]: TAlphaColor read GetAlphaPixelsWrap write SetAlphaPixelsWrap;
{$ENDIF}
    property HSLPixels[x,y: integer]: THSLNativefloatColor read GetHSLPixels write SetHSLPixels;
    procedure Rectangle(x1,y1,x2,y2: integer);
    procedure EmptyRectangle(x1, y1, x2, y2: integer);
    procedure Arc(x1,y1,x2,y2,x3,y3,x4,y4: nativeint);
    procedure LineTo(x1,y1: nativeint; c: TColor);
    procedure Clear(color: TColor);
    procedure Paste(fb: TFastBitmap;x,y: nativeint; bDebug: boolean = false);
    procedure Paste_AutoExpand(fb: TFastBitmap; x,y: nativeint);
    procedure ClearTranslation;
    procedure LoadFont(sFont: string);
    procedure DrawChar_NativeRes(c: char; x,y: nativeint);
    procedure DrawChar_2X(c: char; x,y: nativeint);
    procedure DrawText(s: string);
    procedure ResetTextPos;

  end;



  TFastBitmap = class(TBetterObject)
  private
    FW: integer;
    FH: integer;
    FScanLines: PPOinterArray;
    [unsafe] Fcanvas: TFastCanvas;
    FCanvasHold: TFastCanvas;

    FPixelFormat: TPixelFormat;
    FAlign: integer;
    FCurrentScanLine: integer;
    FCurrentScanLinePtr: PByte;
    FAverageLuminence: nativefloat;
    FMaxLuminence: nativefloat;
    FEnableAlpha: boolean;
    FAllocatedHeight: nativeint;
    FFileNAme: string;
    function GetScanLine(y: integer): PByte;inline;
    procedure SetHeight(const Value: integer);
    procedure Setwidth(const Value: integer);

    procedure FreeAllocation;
    procedure SetPixelFormat(const Value: TPixelFormat);


  public
    centerx,centery: nativefloat;
    procedure Allocate(w,h: integer);
    constructor CopyCreate(fb: TFastBitmap);
    constructor Create;override;
    destructor Destroy;override;
    property Width: integer read FW write Setwidth;
    property Height: integer read FH write SetHeight;
    procedure New;
    property ScanLine[y: integer]: PByte read GetScanLine;
{$IFNDEF FMX}
    procedure FromBitmap(bm: TBitmap);
    procedure FromPNG(bm: TPNGImage);
{$ENDIF}
    procedure FromFastBitmap(bm: TFastBitmap);
{$IFNDEF FMX}
    function ToBitmap: TBitmap;
    function ToPNG: TPNGImage;
{$ENDIF}
    procedure From24BitRGB(p: PByte; w,h: integer);
    property Canvas: TFastCanvas read Fcanvas;
    property PixelFormat: TPixelFormat read FPixelFormat write SetPixelFormat;
    property FAST_BITMAP_PIXEL_ALIGN: integer read FAlign;
    procedure Assign(bm: TFastBitmap);overload;
    procedure LoadFromFile(sFile: string);
    procedure SaveToProprietaryFile(sFile: string);
    procedure LoadFromProprietaryFile(sFile: string);
{$IFNDEF FMX}
    procedure LoadFromFile_JPG(sFile: string);
    procedure LoadFromFile_PNG(sFile: string);
    procedure LoadFromMemory_JPG(p: pbyte; psize: nativeint);
{$ELSE}


{$ENDIF}


{$IFNDEF FMX}
    procedure AssignToPicture(p: TPicture);
    procedure AssignToControl(gi: TPersistent);
    procedure FromFAstBitmapRect(fbm: TFastBitmap; ul,br: TPoint);
{$ELSE}
    procedure FromFAstBitmapRect(fbm: TFastBitmap; ul, br: TPoint);
{$ENDIF}
    procedure SavetoStream(s: TStream);
    procedure LoadFromStream(s: TStream);
    function IsEqualTo(fb: TFastBitmap): boolean;
    procedure SaveToFile(sFile: string);
    procedure SaveToFile_JPG(sFile: string);
{$IFNDEF FMX}
    procedure SaveToFile_PNG(sFile: string);
    procedure SaveToFile_BMP(sFile: string);
{$ENDIF}
    procedure ResizeCanvas(wid,hit, translateX, translateY: nativeint;clBackGround: TColor);
    procedure Effect_Difference(op: TFastBitmap; bAverageCompensate: boolean; bCalcWeightedCenter: boolean = true);
    procedure Effect_LBloom;
    procedure Effect_LGate(rThres: nativefloat);
    procedure Effect_Invert(x1: nativeint = -1; y1: nativeint = -1; x2: nativeint = -1; y2: nativeint = -1);
    procedure Effect_Saturation(sLevel: nativefloat);
    procedure Effect_SaturationFloor(sLevel: nativefloat);
    procedure Effect_LuminanceFloor(sLevel: nativefloat);
    procedure Effect_BlackAndWhite;


    function GetAverageLuminence: nativefloat;
    function GetMaxLuminence: nativefloat;
    procedure Effect_Normalize;
    property EnableAlpha: boolean read FEnableAlpha write FEnableAlpha;
    procedure Effect_MotionDetect(op: TFastBitmap; bAverageCompensate: boolean; bCalcWeightedCenter: boolean = true);
    property FileName: string read FFileNAme;

  end;

var
  FC: TFOntCache;

implementation

{$IFNDEF FMX}
uses
  EasyImage;
{$ENDIF}


{ TFontCache }

procedure TFontCache.CacheFont(sFile: string);
var
  fbm: TFastBitmap;
begin
  if FFonts.indexof(lowercase(sFile)) >=0 then
    exit;

  fbm := TFastBitmap.Create;
  fbm.LoadFromFile(sFile);
  FFonts.AddObject(lowercase(sFile), fbm);


end;

constructor TFontCache.Create;
begin
  inherited;
  FFonts := TStringlist.Create;
end;

destructor TFontCache.Destroy;
begin
  while FFonts.Count > 0 do begin
    FFonts.Objects[0].Free;
    FFonts.Delete(0);
  end;
  FFonts.Free;
  inherited;
end;

function TFontCache.LoadFont(sfile: string): TFastBitmap;
var
  i: integer;
begin
  Lock;
  try
    CacheFont(sFile);

    i := FFonts.IndexOf(lowercase(sfile));
    if i < 0 then
      result := nil
    else begin
      result := TFastBitmap.Create;
      result.FromFastBitmap(FFonts.Objects[i] as TFastBitmap);
    end;
  finally
    Unlock;
  end;

end;



{ TFastBitmap }

procedure TFastBitmap.Allocate(w, h: integer);
var
  t: integer;
  p: pointer;
begin
//  if (Fw = w) and (FH = h) then
//    exit;
  FreeAllocation;
    FW := w;
  FH := h;
  if assigned(FScanLines) then begin
    FreeMemory(FscanLines);
    FScanLines := nil;
  end;
  FScanLines := GetMemory(sizeof(pointer)*h);
  for t := 0 to h-1 do begin
    GetMem(p,w*FAST_BITMAP_PIXEL_ALIGN);
    if (p = nil ) then
      raise Exception.Create('mem alloc error');
    FScanlines[t] := p;

  end;

  FAllocatedHeight := h;

end;

procedure TFastBitmap.Assign(bm: TFastBitmap);
var
  t,u: integer;
begin
  Allocate(bm.width, bm.height);

  enablealpha := bm.enablealpha;
  for u := 0 to bm.Height-1 do begin
    for t := 0 to bm.Width-1 do begin
{$IFDEF FMX}
      canvas.AlphaPixels[t,u] := bm.canvas.AlphaPixels[t,u];
{$ELSE}
      canvas.Pixels[t,u] := bm.Canvas.Pixels[t,u];
{$ENDIF}
    end;
  end;

end;

{$IFNDEF FMX}
procedure TFastBitmap.AssignToControl(gi: TPersistent);
{$IFDEF ASSIGN_AS_BITMAP}
var
  bmTemp: Vcl.Graphics.TBitmap;
begin
  if self = nil then exit;

  bmTemp := Tobitmap;
  try
    with gi as TPersistent do
      assign(bmTemp);
  finally
    bmTemp.free;
  end;
end;
{$ELSE}
var
  bmTemp: TPNGObject;
begin
  if self = nil then exit;

  bmTemp := ToPNG;
  try
    with gi as TPersistent do
      assign(bmTemp);

  finally
    bmTemp.free;
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFNDEF FMX}
procedure TFastBitmap.AssignToPicture(p: TPicture);
{$IFDEF ASSIGN_AS_BITMAP}
var
  bmTemp: Vcl.Graphics.TBitmap;
begin
  if self = nil then exit;

  bmTemp := Tobitmap;
  try
    p.assign(bmTemp);
  finally
    bmTemp.free;
  end;
end;
{$ELSE}
var
  bmTemp: TPNGObject;
begin
  if self = nil then exit;

  bmTemp := ToPNG;
  try
    p.assign(bmTemp);

  finally
    bmTemp.free;
  end;
end;
{$ENDIF}
{$ENDIF}

constructor TFastBitmap.CopyCreate(fb: TFastBitmap);
begin
  Create;
  FromFastBitmap(fb);
end;

constructor TFastBitmap.Create;
begin
  inherited;
  FScanlines := nil;
  FCanvasHold := TFastCanvas.create;
  FCanvas := FCanvasHold;
  FCanvas.owner := self;
  FPixelFormat := pf32Bit;
  FAlign := 4;
  FCurrentscanline := -1;
end;

destructor TFastBitmap.Destroy;
begin
  FCanvas.free;
  FreeAllocation;
  inherited;
end;

procedure TFastBitmap.Effect_BlackAndWhite;
begin
  Effect_Saturation(0);
end;

procedure TFastBitmap.Effect_Difference(op: TFastBitmap; bAverageCompensate: boolean; bCalcWeightedCenter: boolean = true);
var
  avgx, avgy,totx,toty: nativefloat;
  x,y: nativeint;
  hsl1, hsl2: THSLNativefloatColor;
  Favg2: nativefloat;
begin
  FMAxLuminence := 0;
  for y:= 0 to height-1 do begin
    for x := 0 to width-1 do begin
      hsl1 := canvas.hslpixels[x,y];
      hsl2 := op.Canvas.HSLPixels[x,y];
      hsl1.l := system.abs(hsl1.l-hsl2.l)+(system.abs(hsl1.s-hsl2.s)*(system.abs(hsl1.h-hsl2.h)));
      hsl1.s := 0;
      hsl1.h := 0;
      FAverageLuminence := FAverageLuminence + hsl1.l;
      if hsl1.l > FMaxLuminence then
        FMaxLuminence := hsl1.l;
      canvas.HSLPixels[x,y] := hsl1;
    end;
  end;
  FAverageLuminence := FAverageLuminence / (width*height);

  if bAverageCompensate then begin
    FAvg2 := 0;
    for y:= 0 to height-1 do begin
      for x := 0 to width-1 do begin
        hsl1 := canvas.hslpixels[x,y];
        hsl1.l := hsl1.l - (FAverageLuminence/2);
        FAvg2 := FAvg2 +hsl1.l;
        hsl1 := canvas.hslpixels[x,y];
      end;
    end;
    FAverageLuminence := FAvg2 / (width*height);
  end;

  if bCAlcWeightedCenter then begin
    totx := 0;
    toty := 0;
    avgx := 0;
    avgy := 0;
    for y:= 0 to height-1 do begin
      for x := 0 to width-1 do begin
        hsl1 := canvas.hslpixels[x,y];
        totx := totx + hsl1.l;
        toty := toty + hsl1.l;
        avgx := avgx + ((x) * hsl1.l);
        avgy := avgy + ((y) * hsl1.l);
      end;
    end;
//    centerx := (0.5 + (avgx / totx)) * FAverageLuminence;
//    centery := (0.5 + (avgy / toty));
    if totx = 0 then begin
      centerx := 0.5;
    end else begin
      centerx := (avgx/totx) / width;
    end;
    if toty = 0 then begin
      centery := 0.5;
    end else begin
      centery := (avgy/toty) / height;
    end;
//    debug('cx,cy:'+floattostr(centerx)+','+floattostr(centery));
  end;

end;


procedure TFastBitmap.Effect_Invert(x1, y1, x2, y2: nativeint);
begin
  if x1 < 0 then
    x1 := 0;
  if y1 < 0 then
    y1 := 0;

  if x2 < 0 then
    x2 := width-1;
  if y2 < 0 then
    y2 := height-1;

{$IFNDEF FMX}
  //Todo 2 implement for fmx
  EasyImage.Invert(self, x1,y1,x2,y2);
{$ENDIF}


end;


procedure TFastBitmap.Effect_MotionDetect(op: TFastBitmap; bAverageCompensate: boolean; bCalcWeightedCenter: boolean = true);
var
  avgx, avgy,totx,toty: nativefloat;
  x,y: nativeint;
  hsl1, hsl2: THSLNativefloatColor;
  Favg2: nativefloat;
begin
  totx := 0;
  toty := 0;
  avgx := 0;
  avgy := 0;
  FMAxLuminence := 0;
  FAverageLuminence := 0;
  for y:= 0 to height-1 do begin
    for x := 0 to width-1 do begin
      hsl1 := canvas.hslpixels[x,y];
      hsl2 := op.Canvas.HSLPixels[x,y];
      hsl1.l := system.abs(hsl1.l-hsl2.l)+(system.abs(hsl1.l-hsl2.l)*(system.abs(hsl1.s-hsl2.s)*(system.abs(hsl1.h-hsl2.h))));
      hsl1.s := 0;
      hsl1.h := 0;

      if hsl1.l > FMaxLuminence then
        FMaxLuminence := hsl1.l;


      canvas.HSLPixels[x,y] := hsl1;

      if hsl1.l < 0.15 then
        hsl1.l := 0;

      avgx := avgx + ((x) * hsl1.l);
      avgy := avgy + ((y) * hsl1.l);
      totx := totx + hsl1.l;
      toty := toty + hsl1.l;


      FAverageLuminence := FAverageLuminence + hsl1.l;

    end;
  end;
  FAverageLuminence := FAverageLuminence / (width*height);

  if totx = 0 then begin
    centerx := 0.5;
  end else begin
    centerx := (avgx/totx) / width;
  end;
  if toty = 0 then begin
    centery := 0.5;
  end else begin
    centery := (avgy/toty) / height;
  end;
  exit;
//  if bAverageCompensate then begin
//    FAvg2 := 0;
//    for y:= 0 to height-1 do begin
//      for x := 0 to width-1 do begin
//        hsl1 := canvas.hslpixels[x,y];
//        hsl1.l := hsl1.l - (FAverageLuminence/2);
//        FAvg2 := FAvg2 +hsl1.l;
//        hsl1 := canvas.hslpixels[x,y];
//      end;
//    end;
//    FAverageLuminence := FAvg2 / (width*height);
//  end;

  if bCAlcWeightedCenter then begin
    Effect_LGate(0.20);
//    Effect_LBloom();
    totx := 0;
    toty := 0;
    avgx := 0;
    avgy := 0;
    FAverageLuminence := 0;
    for y:= 0 to height-1 do begin
      for x := 0 to width-1 do begin
        hsl1 := canvas.hslpixels[x,y];
        totx := totx + hsl1.l;
        toty := toty + hsl1.l;
        FAverageLuminence := FAverageLuminence + hsl1.l;
        avgx := avgx + ((x) * hsl1.l);
        avgy := avgy + ((y) * hsl1.l);
      end;
    end;
//    centerx := (0.5 + (avgx / totx)) * FAverageLuminence;
//    centery := (0.5 + (avgy / toty));
    if totx = 0 then begin
      centerx := 0.5;
    end else begin
      centerx := (avgx/totx) / width;
    end;
    if toty = 0 then begin
      centery := 0.5;
    end else begin
      centery := (avgy/toty) / height;
    end;
//    debug('cx,cy:'+floattostr(centerx)+','+floattostr(centery));
    FAverageLuminence := FAverageLuminence / (width*height);
  end;

end;


procedure TFastBitmap.Effect_LBloom;
const
  RADIUS = 1;
var
  op: TFastBitmap;
  x,y,xx,yy: nativeint;
  hsl: THSLnativefloatColor;
begin
  op := TFastBitmap.create;
  op.FromFastBitmap(self);
  try
    for y := 0 to self.width -1 do begin
      for x:= 0 to height-1 do begin
        hsl := op.canvas.hslpixels[x,y];
        for yy := 0-RADIUS to 0+RADIUS do begin
          for xx := 0-RADIUS to 0+RADIUS do begin
            if (xx <> 0) or (yy <> 0) then begin
              hsl.l := hsl.l + op.canvas.hslpixels[x+xx,y+yy].l;
            end;
          end;
        end;
        canvas.hslpixels[x,y] := hsl;
      end;
    end;

  finally
    op.free;
  end;

end;

procedure TFastBitmap.Effect_LGate(rThres: nativefloat);
var
  x,y: nativeint;
  hsl: THSLNativefloatcolor;
begin
  for y:= 0 to height-1 do begin
    for x:= 0 to width-1 do begin
      hsl := canvas.hslPixels[x,y];
      if hsl.l < rThres then begin
        hsl.l := 0;
        canvas.hslPixels[x,y] := hsl;
      end;

    end;
  end;

end;

procedure TFastBitmap.Effect_LuminanceFloor(sLevel: nativefloat);
var
  x,y: nativeint;
  a: cardinal;
  c: cardinal;
  hsl: THSLnativefloatColor;
begin

  for y:= 0 to height-1 do begin
    for x := 0 to width-1 do begin
      c := Canvas.Pixels[x,y];
      a := c shr 24;

      hsl.FromColor(c);
      if hsl.l < sLevel then
        hsl.l := 0;
      c := hsl.ToColor;
      if enablealpha then begin
        c := c or (a shl 24);
      end;

      canvas.Pixels[x,y] := c;
    end;
  end;

end;

procedure TFastBitmap.Effect_Normalize;
var
  hsl: THSLNativefloatColor;
  x,y: nativeint;
begin
  FMaxLuminence := 0;
  for y:= 0 to height-1 do begin
    for x := 0 to width-1 do begin
      hsl := canvas.hslpixels[x,y];
      if hsl.l > FMaxLuminence then begin
        FMaxLuminence := hsl.l;
      end;
    end;
  end;

  if FMaxLuminence > 0 then begin
    for y:= 0 to height-1 do begin
      for x := 0 to width-1 do begin
        hsl := canvas.hslpixels[x,y];
        hsl.l := hsl.l / FMaxLuminence;
        canvas.hslpixels[x,y] := hsl;
      end;
    end;
  end;


end;

procedure TFastBitmap.Effect_Saturation(sLevel: nativefloat);
var
  x,y: nativeint;
  a: cardinal;
  c: cardinal;
  hsl: THSLnativefloatColor;
begin

  for y:= 0 to height-1 do begin
    for x := 0 to width-1 do begin
      c := Canvas.Pixels[x,y];
      a := c shr 24;

      hsl.FromColor(c);
      hsl.s := hsl.s * sLevel;
      c := hsl.ToColor;
      if enablealpha then begin
        c := c or (a shl 24);
      end;

      canvas.Pixels[x,y] := c;
    end;
  end;

end;

procedure TFastBitmap.Effect_SaturationFloor(sLevel: nativefloat);
var
  x,y: nativeint;
  a: cardinal;
  c: cardinal;
  hsl: THSLnativefloatColor;
begin

  for y:= 0 to height-1 do begin
    for x := 0 to width-1 do begin
      c := Canvas.Pixels[x,y];
      a := c shr 24;

      hsl.FromColor(c);
      if hsl.s < sLevel then
        hsl.s := 0;
      c := hsl.ToColor;
      if enablealpha then begin
        c := c or (a shl 24);
      end;

      canvas.Pixels[x,y] := c;
    end;
  end;

end;

procedure TFastBitmap.FreeAllocation;
var
  t: integer;
  p: pointer;
begin
  if assigned(FScanLines) then begin
    for t:= 0 to FAllocatedHeight-1 do begin
      if assigned(FScanLines[t]) then
        FreeMemory(FScanLines[t]);
    end;
    FreeMemory(FScanLines);
    FAllocatedHeight := 0;
  end;

  FScanLines := nil;
  FCurrentScanLinePtr := nil;
end;

procedure TFastBitmap.From24BitRGB(p: PByte; w,h: integer);
var
  pp: PByte;
  t,u: integer;
begin
  Allocate(w,h);

  pp := p;
  for u := h-1 downto 0 do begin
    for t:= 0 to w-1 do begin
      self.Canvas.Pixels[t,u] := pp[2]+(pp[1] shl 8)+(pp[0] shl 16);
      inc(pp,3);
    end;
  end;



end;

{$IFNDEF FMX}
procedure TFastBitmap.FromBitmap(bm: TBitmap);
var
  t: integer;
  w,h: integer;
begin
  w := bm.width;
  h := bm.height;
  bm.canvas.lock;
  try
    Allocate(w, bm.height);
    bm.pixelFormat := pf32bit;
    pixelformat := pf32bit;

    for t:= 0 to h-1 do begin
      MoveMem32(FScanlines[t], bm.scanline[t], w * FAST_BITMAP_PIXEL_ALIGN);
    end;
  finally
    bm.canvas.unlock;
  end;


end;
{$ENDIF}

procedure TFastBitmap.FromFastBitmap(bm: TFastBitmap);
var
  t: integer;
  w,h: integer;
begin
  if bm = nil then
    exit;

  w := bm.width;
  h := bm.height;
  Allocate(w, h);
  bm.pixelFormat := pf32bit;

//  if FScanlines.count < h then exit;
//  canvas.paste(bm,0,0);
  EnableAlpha := bm.EnableAlpha;
  for t:= 0 to h-1 do begin
    MoveMem32(FScanlines[t], bm.scanline[t], w * FAST_BITMAP_PIXEL_ALIGN);
  end;




end;

{$IFNDEF FMX}
procedure TFastBitmap.FromPNG(bm: TPNGImage);
var
  t,u: integer;
  w,h: integer;
  c: TColor;
  pb: vcl.imaging.pngimage.PByteArray;
begin
  w := bm.width;
  h := bm.height;
  //Debug.ConsoleLog(inttostr(ord(bm.TransparencyMode)));// := ptmPartial;
  bm.canvas.lock;
  try
    Allocate(w, bm.height);

    for u:= 0 to h-1 do begin
      pb := bm.AlphaScanline[u];
      if pb <> nil then
        EnableAlpha := true;

      for t:= 0 to w-1 do begin


        c := bm.Pixels[t,u];
        if EnableAlpha and (pb<>nil) then
          canvas.Pixels[t,u] := c or (pb[t] shl 24)
        else
          canvas.Pixels[t,u] := c;

      end;
    end;


  finally
    bm.canvas.unlock;
  end;

end;
{$ENDIF}


{ TFontinfo }

procedure TFontinfo.Cleanup;
begin
  FFont.free;
  FFont := nil;
end;

function TFontinfo.GetSourceRect(c: char): TRect;
begin
  c := char(ord(c) and 255);
  result.Left := (ord(c) mod charsacross) * charwidth;
  result.Top := (ord(c) div charsacross) * charheight;
  result.Right := result.Left + charwidth - 1;
  result.Bottom := result.Top + charheight -1;

end;

procedure TFontinfo.LoadFont(sFile: string);
var
  ff: TfastBitmap;
begin
  if lowercase(filename) = lowercase(sFile) then
    exit;


  if not assigned(FFont) then begin
//    ff := TFastBitmap.Create;
//    ff.LoadFromFile(sFile);
//    ff := fFont;
//    FFont := TFastBitmap.Create;
//    FFont.fromfastbitmap(ff);
    FFont := FC.LoadFont(sfile);
  end;



  charsacross := 16;
  charsvertical := 16;
  charwidth := FFont.Width div charsacross;
  charheight := fFont.Height div charsvertical;


end;



{ TFastCanvas }


procedure TFastCanvas.Arc(x1, y1, x2, y2, x3, y3, x4, y4: nativeint);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TFastCanvas.Clear(color: TColor);
var
  t,u: integer;
begin
  for u := 0 to Self.Owner.height -1 do begin
    for t:= 0 to self.Owner.Width -1 do begin
      pixels[t,u] := color;
    end;
  end;
end;

procedure TFastCanvas.ClearTranslation;
begin
{$IFDEF USE_TRANSLATION}
  Translation.X := 0;
  Translation.Y := 0;
{$ENDIF}
end;

destructor TFastCanvas.Destroy;
begin
  FFontInfo.Cleanup;
  inherited;
end;

procedure TFastCanvas.DrawChar_2X(c: char; x, y: nativeint);
var
  xx,yy,xxx,yyy,xxxx,yyyy: nativeint;
  r: TRect;
begin
  r := FFontInfo.GetSourceRect(c);
  for yy := 0 to Ffontinfo.charheight-1 do begin
    for xx:= 0 to FFontinfo.charwidth-1 do begin
      for yyy := 0 to 1 do begin
        for xxx := 0 to 1 do begin
          xxxx := x+(xx*2)+xxx;
          yyyy := y+(yy*2)+yyy;
          pixels[xxxx,yyyy] := ColorBlend_ForegroundSourceAlpha(pixels[xxxx,yyyy],FFontinfo.FFont.Canvas.Pixels[xx+r.left,yy+r.top], 1.0);
        end;
      end;
    end;
  end;
end;

procedure TFastCanvas.DrawChar_NativeRes(c: char; x,y: nativeint);
var
  xx,yy: nativeint;
  r: TRect;
  strokex,strokey: ni;
  cc: TColor;
begin
  r := FFontInfo.GetSourceRect(c);
  for strokex := -1 to 1 do begin
    for strokey := -1 to 1 do begin
      for yy := 0 to Ffontinfo.charheight-1 do begin
        for xx:= 0 to FFontinfo.charwidth-1 do begin
          cc := ColorBlend_ForegroundSourceAlpha(pixels[x+xx+strokex, y+yy+strokey],FFontinfo.FFont.Canvas.Pixels[xx+r.left,yy+r.top] and (cardinal(self.brush.color) or $ff000000), 0.5);
          pixels[x+xx+strokex, y+yy+strokey] := cc;
        end;
      end;
    end;
  end;
  for strokex := 0 to 0 do begin
    for strokey := 0 to 0 do begin
      for yy := 0 to Ffontinfo.charheight-1 do begin
        for xx:= 0 to FFontinfo.charwidth-1 do begin
            cc := ColorBlend_ForegroundSourceAlpha(pixels[x+xx+strokex, y+yy+strokey],FFontinfo.FFont.Canvas.Pixels[xx+r.left,yy+r.top] and (cardinal(self.pen.color) or $ff000000), 1.0);
            pixels[x+xx+strokex, y+yy+strokey] := cc;
        end;
      end;
    end;
  end;



end;

procedure TFastCanvas.DrawText(s: string);
var
  t: integer;
  c: char;
begin
  for t:= 1 to length(s) do begin
    c := s[t];
    case c of
      #13: textpos.X := 0;
{$IFDEF NATIVE_RES_TEXT}
      #10: textpos.Y := textpos.Y + FFontInfo.charheight;
{$ELSE}
      #10: textpos.Y := textpos.Y + (FFontInfo.charheight*2);
{$ENDIF}
    else
{$IFDEF NATIVE_RES_TEXT}
      DrawChar_NativeRes(c, textpos.X+textoffset.x, textpos.Y+textoffset.y);
      textpos.X := textpos.X + FFontinfo.charwidth;
{$ELSE}
      DrawChar_2X(c, textpos.X+textoffset.x, textpos.Y+textoffset.y);
      textpos.X := textpos.X + (FFontinfo.charwidth*2);
{$ENDIF}

    end;
  end;
end;

{$IFDEF FMX}
function TFastCanvas.GetAlphaPixels(x, y: integer): TAlphaColor;
var
  sl : PCardinal;
begin
  if x<0 then begin
    result := 0;
    exit;
  end;

  if y<0 then begin
    result := 0;
    exit;
  end;

  if x>=owner.width then begin
    result := 0;
    exit;
  end;

  if y>=owner.height then begin
    result := 0;
    exit;
  end;


  sl := owner.FScanLines[y];

  sl := pcardinal(pbyte(sl) + (x*sizeof(x)));
  result := TAlphaColor(sl^);
end;
function TFastCanvas.GetAlphaPixelsWrap(x, y: integer): TAlphaColor;
begin
  result := GetAlphaPixels(x mod Owner.Width, y mod owner.Height);
end;

{$ENDIF}

function TFastCanvas.GetHSLPixels(x, y: integer): THSLNativefloatColor;
begin
  result.FromColor(pixels[x,y]);
end;

function TFastCanvas.GetPixels(x, y: integer): TColor;
var
  p,pc: PByte;
begin


{$IFDEF USE_TRANSLATION}
  x := x + Translation.x;
  y := y + Translation.y;
{$ENDIF}
  if (y< 0) or (y >= owner.height) then begin
    result := clBlack;
    exit;
  end else begin
    if (x< 0) or (x >=  owner.FW)  then begin
      result := clBlack;
      exit;
    end;
    pc := Pbyte(@result);
    p := Owner.Scanline[y];
    if p = nil then
      exit;
    p := @p[x*owner.FAST_BITMAP_PIXEL_ALIGN];
    if p = nil then
      exit;

    if owner.enablealpha then begin
      pc[0] := p[3];
      pc[1] := p[2];
      pc[2] := p[1];
      pc[3] := p[0];

//      result := p[3]+(p[2] shl 8)+(p[1] shl 16)+(p[0] shl 24)
    end else begin
//      result := (p[2] shl 0)+(p[1] shl 8)+(p[0] shl 16);
      pc[0] := p[2];
      pc[1] := p[1];
      pc[2] := p[0];
    end;

    //MoveMem32(@result, p, 4);
  end;


end;

procedure TFastCanvas.LineTo(x1, y1: nativeint; c: TColor);
var
  xx,yy,t1,t2: nativeint;
  l,h: integer;
begin
  //use integer space
  if system.abs(x1-penpos.x) > system.abs(y1-penpos.y) then begin
    l := x1;
    h := penpos.x;

    if h < l then begin
      t1 := penpos.y;
      t2 := y1;
      order(l,h);
    end else begin
      t2 := penpos.y;
      t1 := y1;
    end;

    for xx := l to h do begin
      yy := round(interpolate(xx, t1, t2, l,h));
      self.Pixels[xx,yy] := c;
    end;

  end else begin
    l := y1;
    h := penpos.y;
    if h < l then begin
      t1 := penpos.x;
      t2 := x1;
      order(l,h);
    end else begin
      t2 := penpos.x;
      t1 := x1;
    end;


    for yy := l to h do begin
      xx := round(interpolate(yy, t1, t2, l,h));
      self.Pixels[xx,yy] := c;
    end;


  end;
end;

procedure TFastCanvas.LoadFont(sFont: string);
begin
  FFontINfo.LoadFont(sFont);
end;

procedure TFastCanvas.Paste(fb: TFastBitmap; x, y: nativeint; bDebug: boolean);
var
  t,u: nativeint;
  c: TColor;
begin
  for u := 0 to fb.Height-1 do begin
    for t:= 0 to fb.Width -1 do begin
      c := fb.canvas.pixels[t,u];
      if bDebug then c := colorblend(c,pixels[x+t,y+u],0.5);
      pixels[x+t,y+u] := c;
    end;
//    break;
  end;

end;

procedure TFastCanvas.Paste_AutoExpand(fb: TFastBitmap; x, y: nativeint);
var
  xLeft, xRight, xTop, xBottom: nativeint;
  pt: TPoint;
begin

{$IFDEF USE_TRANSLATION}
  xLeft := 0-Translation.X;
  xTop := 0-Translation.Y;
{$ELSE}
  xLeft := 0;
  xTop := 0;
{$ENDIF}
  xRight := owner.Width-1;
  xBottom := owner.Height-1;

  if x< xLeft then begin
{$IFDEF USE_TRANSLATION}
    Translation.x := translation.x + (xLeft-x);
{$ENDIF}
    xLeft := x;
  end;
  if y< xTop then begin
{$IFDEF USE_TRANSLATION}
    Translation.y := translation.y + (xTop-y);
{$ENDIF}
    xTop := y;
  end;

{$IFDEF USE_TRANSLATION}
  if x+fb.Width > ((xRight+1)-Translation.X) then xRight := ((x+fb.Width)-1)-Translation.x;
  if y+fb.height > ((xBottom+1)-Translation.Y) then xBottom := ((y+fb.height)-1)-Translation.y;
{$ELSE}
  if x+fb.Width > ((xRight+1)) then xRight := ((x+fb.Width)-1);
  if y+fb.height > ((xBottom+1)) then xBottom := ((y+fb.height)-1);

{$ENDIF}

  self.Owner.ResizeCanvas((xRight-xLeft)+1, (xBottom-xTop)+1, 0-xLeft, 0-xTop, clBlack);
{$IFDEF USE_TRANSLATION}
  pt := self.translation;
  fb.canvas.ClearTranslation;
  self.translation := pt;
{$ENDIF}

{$IFDEF USE_TRANSLATION}
  self.Paste(fb, x,y);
{$ELSE}
  if x< 0 then x := 0;
  if y < 0 then y := 0;
  self.Paste(fb, x,y,true);
{$ENDIF}

end;

procedure TFastCanvas.Rectangle(x1, y1, x2, y2: integer);
var
  t,u: integer;
begin
  Order(x1,x2);
//  Order(y1,y2);
  for u := y1 to y2-1 do begin
    for t := x1 to x2-1 do begin
{$IFDEF FMX}
      alphapixels[t,u] := brush.color;
{$ELSE}
      pixels[t,u] := brush.color;
{$ENDIF}
    end;
  end;

end;

procedure TFastCanvas.ResetTextPos;
begin
  textpos.x := 0;
  textpos.y := 0;
end;

procedure TFastCanvas.EmptyRectangle(x1, y1, x2, y2: integer);
var
  t,u: integer;
begin
  Order(x1,x2);
//  Order(y1,y2);
  for u := y1 to y1 do begin
    for t := x1 to x2-1 do begin
{$IFDEF FMX}
      alphapixels[t,u] := brush.color;
{$ELSE}
      pixels[t,u] := brush.color;
{$ENDIF}
    end;
  end;

  for u := y2 to y2 do begin
    for t := x1 to x2-1 do begin
{$IFDEF FMX}
      alphapixels[t,u] := brush.color;
{$ELSE}
      pixels[t,u] := brush.color;
{$ENDIF}
    end;
  end;

  for u := y1 to y2-1 do begin
    for t := x1 to x1 do begin
{$IFDEF FMX}
      alphapixels[t,u] := brush.color;
{$ELSE}
      pixels[t,u] := brush.color;
{$ENDIF}
    end;
  end;

  for u := y1 to y2-1 do begin
    for t := x2 to x2 do begin
{$IFDEF FMX}
      alphapixels[t,u] := brush.color;
{$ELSE}
      pixels[t,u] := brush.color;
{$ENDIF}
    end;
  end;

end;


{$IFDEF FMX}
procedure TFastCanvas.SetAlphaPixels(x, y: integer; const Value: TAlphaColor);
var
  sl : PCardinal;
begin
  sl := owner.FScanLines[y];

  sl := pcardinal(pbyte(sl) + (x*sizeof(x)));
  sl^ := cardinal(value);

end;
procedure TFastCanvas.SetAlphaPixelsWrap(x, y: integer;
  const Value: TAlphaColor);
begin
  SetAlphaPixels(x mod Owner.Width, y mod owner.Height, value);
end;

{$ENDIF}

procedure TFastCanvas.SetHSLPixels(x, y: integer;
  const Value: THSLNativefloatColor);
begin
  pixels[x,y] := value.ToColor;
end;

procedure TFastCanvas.SetOwner(const Value: TFastBitmap);
begin
  FOwnerHold := value;
  FOwner := Value;

end;

procedure TFastCanvas.SetPixels(x, y: integer; const Value: TColor);
var
  rev1,rev2: TsmallColor;
  p: Pbyte;
  xx: integer;
begin
{$IFDEF USE_TRANSLATION}
  x := x + translation.x;
  y := y + translation.y;
{$ENDIF}
  if x >= owner.width then exit;
  if y >= owner.Height then exit;
  if x< 0 then exit;
  if y < 0 then exit;

{$IFDEF SETPIXELS_USES_SMALLCOLOR}
  rev2.SetColor(value);
  rev1.r := rev2.b;
  rev1.g := rev2.g;
  rev1.b := rev2.r;

  movemem32(PColor(PByte(Owner.Scanline[y])+(x*owner.FAST_BITMAP_PIXEL_ALIGN)), @rev1, sizeof(rev1));
{$ELSE}
    p := owner.ScanLine[y];
    if p = nil then
      exit;
    xx := x*owner.FAST_BITMAP_PIXEL_ALIGN;

    p := @p[xx];
    if owner.EnableAlpha then begin
      p^ := byte((value shr 24) and 255);
      inc(p);
    end;
    p^ := byte((value shr 16) and 255);
    inc(p);
    p^ := byte((value shr 8) and 255);
    inc(p);
    p^ := byte((value shr 0) and 255);



{$ENDIF}
//  PColor(PByte(Owner.Scanline[y])+(x*owner.FAST_BITMAP_PIXEL_ALIGN)) := value;
end;

function TFastBitmap.GetAverageLuminence: nativefloat;
//note! this function does nothing currently unless you call the _difference function... it is used for motion detection
begin
  result := FAverageluminence;
end;

function TFastBitmap.GetMaxLuminence: nativefloat;
begin
  result := FMaxluminence;
end;

function TFastBitmap.GetScanLine(y: integer): PByte;
begin
  if y <> FcurrentScanLine then begin
    FCurrentScanLine := y;
    FCurrentScanLinePtr := FScanlines[y];
  end;
  result := FcurrentscanLinePtr;
//  result := FScanlines[y];

end;

function TFastBitmap.IsEqualTo(fb: TFastBitmap): boolean;
var
  x,y: nativeint;
begin
  result := true;
  if fb.Width <> Width then begin
    result := false;
    exit;
  end;

  if fb.Height <> height then begin
    result := false;
    exit;
  end;

  for y := 0 to height-1 do begin
    for x := 0 to width-1 do begin
      if canvas.Pixels[x,y] <> fb.Canvas.Pixels[x,y] then begin
        result := false;
        exit;
      end;
    end;
  end;
end;


procedure TFastBitmap.LoadFromFile(sFile: string);
{$IFNDEF FMX}
var
  bm: TBitmap;
begin
  FFileNAme := sFile;

  if lowercase(extractfileext(sFile)) = '.jpg' then begin
    LoadFromFile_JPG(sFile);
    exit;
  end;

  if lowercase(extractfileext(sFile)) = '.png' then begin
    LoadFromFile_PNG(sFile);
    exit;
  end;


  bm := TBitmap.create;
  try
    bm.LoadFromfile(sfile);
    Frombitmap(bm);
  finally
    bm.free;
  end;
end;
{$ELSE}
begin
  raise ECritical.create('LoadFromFile is unimplemented');
end;

{$ENDIF}

procedure TFastBitmap.LoadFromProprietaryFile(sFile: string);
var
  fs: TLocalFileStream;
  h: TFBMHeader;
  y,x: ni;
  pc: PCardinal;
  c: cardinal;
begin

  fs := TLocalFileStream.create(sfile, fmOpenRead+fmShareDenyNone);
  try
    if fs.Size = 0 then begin
      allocate(0,0);
      exit;
    end;

    stream_guaranteeRead(fs, @h, sizeof(h));
    if (h.width < 0)
    or (h.height < 0)
    or (h.width > 4096)
    or (h.height > 4096)
    then begin
      allocate(0,0);
      exit;
    end;

    self.Allocate(h.width, h.height);
    self.EnableAlpha := true;
    for y := 0 to height-1 do begin
      stream_GuaranteeRead(fs, self.ScanLine[y], width*4);
{$IFNDEF FMX}
      for x:= 0 to width-1 do begin
        pc := PCArdinal(self.scanline[y]+(4*x));
        c := pc^;

        c := (c and $FF00FF00) or ((c and $ff0000) shr 16) or ((c and $ff) shl 16);
        EndianSwap(@c, 4);


        pc^ := c;
      end;
{$ENDIF}

    end;
  finally
    fs.Free;
    fs := nil;
  end;
end;


{$IFNDEF FMX}
procedure TFastBitmap.LoadFromFile_JPG(sFile: string);
var
  jp: TJpegImage;
  bm : TBitmap;
begin
  jp := TJpegImage.Create;
  try
    jp.LoadFromFile(sFile);
    bm := JpegToBitmap(jp);
    try
      self.FromBitmap(bm);
    finally
      bm.free;
    end;
  finally
    //jp.Free;
  end;


end;
{$ENDIF}

{$IFNDEF FMX}
procedure TFastBitmap.LoadFromFile_PNG(sFile: string);
var
  jp: TPngImage;
begin
  jp := TPngImage.Create;
  try
    jp.LoadFromFile(sFile);
    self.FromPNG(jp);
  finally
    jp.Free;
  end;

end;
{$ENDIF}

{$IFNDEF FMX}
procedure TFastBitmap.LoadFromMemory_JPG(p: pbyte; psize: nativeint);
var
  s: TMemoryStream;
  jp: TJPegImage;
  bm: TBitmap;
begin
  s := TMemorystream.create;
  try
    stream_guaranteewrite(s, p, psize);
    s.Seek(0,0);
    jp := TJpegImage.Create;
    try
      jp.LoadFromStream(s);
      bm := JpegToBitmap(jp);
      try
        self.FromBitmap(bm);
      finally
        bm.free;
      end;
    finally
      //jp.Free;
    end;
  finally
    s.Free;
  end;

end;
{$ENDIF}

procedure TFastBitmap.LoadFromStream(s: TStream);
var
  t,u: integer;
  c: TColor;
begin
  for u := 0 to height do begin
    for t:= 0 to width do begin
      s.Read(c, sizeof(TColor));
      self.Canvas.Pixels[t,u] := c;
    end;
  end;
end;

procedure TFastBitmap.New;
begin
  if (width > 0) and (height > 0) then begin
    allocate(width,height);
    pixelformat := pf32bit;
  end;
end;

procedure TFastBitmap.ResizeCanvas(wid, hit, translateX,
  translateY: nativeint; clBackGround: TColor);
var
  temp: TFastBitmap;
  ox,oy: nativeint;
begin
  temp := TFastBitmap.CopyCreate(self);
  try
{$IFDEF USE_TRANSLATION}
    ox := self.Canvas.Translation.X;
    oy := self.Canvas.Translation.Y;
{$ENDIF}
    self.Canvas.ClearTranslation;

    self.Width := wid;
    self.Height := hit;
    self.New;
    self.Canvas.Clear(clBackGround);
    temp.canvas.ClearTranslation;
//    self.canvas.translation.X := translateX;
//    self.canvas.translation.Y := translateY;
    self.Canvas.Paste(temp,translateX, translateY);
{$IFDEF USE_TRANSLATION}
    self.Canvas.translation.X := translateX + ox;
    self.Canvas.translation.Y := translateY + oY;
{$ENDIF}


  finally
    temp.Free;
  end;

end;

procedure TFastBitmap.SaveToFile(sFile: string);
begin
  if lowercase(extractfileext(sFile))= '.jpg' then begin
    SaveToFile_JPG(sFile);
  end else
{$IFNDEF FMX}
  if lowercase(extractfileext(sFile))= '.bmp' then begin
    SaveToFile_BMP(sFile);
  end else
  if lowercase(extractfileext(sFile))= '.png' then begin
    SaveToFile_PNG(sFile);
  end else
{$ENDIF}
  if lowercase(extractfileext(sFile))= '.pgf' then begin
    SaveToProprietaryFile(sFile);
  end
  else
    raise ECritical.create('unsupported save file format '+sFile);


end;

{$IFNDEF FMX}
procedure TFastBitmap.SaveToFile_BMP(sFile: string);
var
  bm: TBitmap;
begin
  FFileNAme := sFile;
  bm := nil;
  try
    bm := self.ToBitmap;
    bm.canvas.Lock;
    try
      bm.SavetoFile(sfile);
    finally
      bm.canvas.Unlock;
    end;
  finally
    bm.free;
  end;
end;
{$ENDIF}

procedure TFastBitmap.SaveToFile_JPG(sFile: string);
{$IFNDEF FMX}
var
  bm: TBitmap;
  jpg: TJpegImage;
begin
  FFileNAme := sFile;
  bm := nil;
  try
    bm := self.ToBitmap;
    bm.canvas.Lock;
    try
      if lowercase(extractfileext(sFile))= '.jpg' then begin
        jpg := TJpegImage.Create;
        jpg.Assign(bm);
        jpg.SaveToFile(sFile);
        jpg.Free;
      end else begin
        bm.SavetoFile(sfile);
      end;
    finally
      bm.canvas.Unlock;
    end;
  finally
    bm.free;
  end;
end;
procedure TFastBitmap.SaveToFile_PNG(sFile: string);
var
  bm: TPNGImage;
begin
  FFileNAme := sFile;
  bm := nil;
  try
    bm := self.ToPng;
    bm.canvas.Lock;
    try
      bm.SavetoFile(sfile);
    finally
      bm.canvas.Unlock;
    end;
  finally
    bm.free;
  end;
end;

{$ELSE}
begin
  raise ECritical.create('SaveToFile is unimplemented');
end;
{$ENDIF}

procedure TFastBitmap.SaveToProprietaryFile(sFile: string);
var
  fs: TLocalFileStream;
  h: TFBMHeader;
  x,y: ni;
  c: TColor;
{$IFNDEF FMX}
  pb: array of cardinal;
{$ENDIF}
begin

  fs := TLocalFileStream.create(sfile, fmCreate);
  try
    h.init;
    h.width := width;
    h.height := height;
    h.format_size := 4;
    h.stream_size := 0;
    stream_guaranteewrite(fs, @h, sizeof(h));

{$IFNDEF FMX}
    setlength(pb, width);
{$ENDIF}
    for y := 0 to height-1 do begin
{$IFNDEF FMX}
      movemem32(@pb[0], self.ScanLine[y], width*4);
      for x:= 0 to width-1 do begin
        c := pb[x];

        EndianSwap(@c, 4);
        c := (cardinal(c) and $FF00FF00) or ((cardinal(c) and $ff0000) shr 16) or ((cardinal(c) and $ff) shl 16);
        //c := c and $FFFFFFFF;

        pb[x] := c;
      end;
     stream_guaranteewrite(fs, @pb[0], width*4);
{$ELSE}
      stream_guaranteewrite(fs, self.ScanLine[y], width*4);
{$ENDIF}
    end;
  finally
    fs.Free;
    fs := nil;
  end;
end;


procedure TFastBitmap.SavetoStream(s: TStream);
var
  t,u: integer;
  c: TColor;
begin
  for u := 0 to height do begin
    for t:= 0 to width do begin
      c := self.Canvas.Pixels[t,u];
      s.Write(c, sizeof(TColor));
    end;
  end;
end;

procedure TFastBitmap.SetHeight(const Value: integer);
begin
  FH := Value;
end;

procedure TFastBitmap.SetPixelFormat(const Value: TPixelFormat);
begin
  if FPIxelFormat = value then begin
    exit;
  end;

  FPixelFormat := Value;

  case FPixelFormat of
    pf8bit: FAlign := 1;
    pf16bit: FAlign := 2;
    pf24bit: FAlign := 3;
    pf32bit: FAlign := 4;
  end;

  Allocate(Width,height);
end;

procedure TFastBitmap.Setwidth(const Value: integer);
begin
  FW := Value;
end;

{$IFNDEF FMX}
function TFastBitmap.ToBitmap: TBitmap;
var
  t: integer;
begin
  result := CreateBlankBitmap(width,height, pf32bit);

  result.canvas.lock;
  try
    for t:= 0 to height-1 do begin
      MoveMem32(result.scanline[t], FScanlines[t], width * FAST_BITMAP_PIXEL_ALIGN);
    end;
  finally
    result.canvas.unlock;
  end;

end;
{$ENDIF}


{$IFNDEF FMX}
function TFastBitmap.ToPNG: TPNGImage;
var
  t,x,y: integer;
  c: cardinal;
begin
  result := TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, width,height);
  result.canvas.lock;
  try
    result.Canvas.Rectangle(0,0,width,height);
    if EnableAlpha then
      result.CreateAlpha;

{$IFDEF FAST_FAST_TO_PNG}
    for t:= 0 to height-1 do begin
      MoveMem32(result.scanline[t], FScanlines[t], width * FAST_BITMAP_PIXEL_ALIGN);
    end;
{$ELSE}
    for y:= 0 to height-1 do begin
      for x := 0 to width-1 do begin
        if EnableAlpha then begin
          c := self.Canvas.Pixels[x,y];
          result.Canvas.Pixels[x,y] := c and $FFFFFF;
          result.AlphaScanline[y][x] := (c shr 24);
        end else
          result.Canvas.Pixels[x,y] := self.Canvas.Pixels[x,y];

      end;
    end;

    if not EnableAlpha then
      result.RemoveTransparency;

{$ENDIF}
  finally
    result.canvas.unlock;
  end;

end;
{$ENDIF}


{ TFBMHeader }

procedure TFBMHeader.Init;
begin
  a := $1010011010;
  format := FBM_FMT_RAW_ARGB;

end;


procedure TFastBitmap.FromFAstBitmapRect(fbm: TFastBitmap; ul,br: TPoint);
var
  xy: TPoint;
  x,y: ni;
begin
  Order(ul,br);
  xy := br-ul;

  allocate(xy.x+1,xy.y+1);

  for y:= 0 to xy.y do begin
    for x := 0 to xy.x do begin
{$IFDEF FMX}
      canvas.alphapixels[x,y] := fbm.canvas.alphapixels[x+ul.x, y+ul.y];
{$ELSE}
      canvas.pixels[x,y] := fbm.canvas.pixels[x+ul.x, y+ul.y];
{$ENDIF}
    end;
  end;



end;

end.
