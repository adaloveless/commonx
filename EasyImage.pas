unit EasyImage;
{$MESSAGE '*******************COMPILING EasyImage.pas'}
//TODO 2: Run Queue in Service Mode
{x$INLINE AUTO}
{$DEFINE SSE2}
{$DEFINE ASSIGN_AS_BITMAP|
{x$DEFINE USE_TRANSLATION}
{$DEFINE NATIVE_RES_TEXT}
interface

uses system.uitypes, system.types, system.rtlconsts, gdipapi, betterobject, ColorConversion, beeper, ExtCtrls, generics.collections.fixed,tickcount,
    graphics, typex, dialogs, sysutils, classes, GDIPOBJ, winapi.windows, math, controls,orderlyinit, numbers, colorblending, vcl.imaging.pngimage,
    Vcl.Imaging.GIFImg, sharedobject, debug, geometry, commandprocessor, dir, dirfile, systemx, helpers.stream, fastbitmap, vcl.imaging.jpeg;


const primary_hues: array[0..8] of TColor = ($FF, $7fFF, $FFFF, $FF00, $FFFF00, $FF0000, $FF009F, $FF00FF,$FFFFFF);
const Chart_colors: array[0..8] of Tcolor = ($0000FF,$00ff00,$ff0000, $7f00ff, $ff00ff, $FFFF00, $ffffff,$007fFF,$00ffff);

const clOrange = $007fFF;
const clCyan = $FFFF00;
const clMagenta = $FF00FF;




const PATTERN1: array[0..((12*12)-1)] of integer =
(
1,0,0,1,0,0,1,0,0,1,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,
1,0,0,1,0,0,1,0,0,1,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,
1,0,0,1,0,0,1,0,0,1,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,
1,0,0,1,0,0,1,0,0,1,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0
);
const PATTERN2: array[0..((12*12)-1)] of integer =
(
1,0,1,0,1,0,1,0,1,0,1,0,
0,0,0,0,0,0,0,0,0,0,0,0,
1,0,1,0,1,0,1,0,1,0,1,0,
0,0,0,0,0,0,0,0,0,0,0,0,
1,0,1,0,1,0,1,0,1,0,1,0,
0,0,0,0,0,0,0,0,0,0,0,0,
1,0,1,0,1,0,1,0,1,0,1,0,
0,0,0,0,0,0,0,0,0,0,0,0,
1,0,1,0,1,0,1,0,1,0,1,0,
0,0,0,0,0,0,0,0,0,0,0,0,
1,0,1,0,1,0,1,0,1,0,1,0,
0,0,0,0,0,0,0,0,0,0,0,0
);
const PATTERN3: array[0..((12*12)-1)] of integer =
(
1,0,1,0,1,0,1,0,1,0,1,0,
0,1,0,1,0,1,0,1,0,1,0,1,
1,0,0,0,1,0,0,0,1,0,0,0,
0,1,0,1,0,1,0,1,0,1,0,1,

1,0,1,0,1,0,1,0,1,0,1,0,
0,1,0,1,0,1,0,1,0,1,0,1,
1,0,0,0,1,0,0,0,1,0,0,0,
0,1,0,1,0,1,0,1,0,1,0,1,

1,0,1,0,1,0,1,0,1,0,1,0,
0,1,0,1,0,1,0,1,0,1,0,1,
1,0,0,0,1,0,0,0,1,0,0,0,
0,1,0,1,0,1,0,1,0,1,0,1
);

const PATTERN4: array[0..((12*12)-1)] of integer =
(
1,0,1,0,1,0,1,0,1,0,1,0,
0,1,0,1,0,1,0,1,0,1,0,1,
1,0,1,0,1,0,1,0,1,0,1,0,
0,1,0,1,0,1,0,1,0,1,0,1,
1,0,1,0,1,0,1,0,1,0,1,0,
0,1,0,1,0,1,0,1,0,1,0,1,
1,0,1,0,1,0,1,0,1,0,1,0,
0,1,0,1,0,1,0,1,0,1,0,1,
1,0,1,0,1,0,1,0,1,0,1,0,
0,1,0,1,0,1,0,1,0,1,0,1,
1,0,1,0,1,0,1,0,1,0,1,0,
0,1,0,1,0,1,0,1,0,1,0,1
);


type
  TColorArray = Array[Word] of TColor;
  PColorArray = ^TColorArray;
  TPointerArray = array[0..0] of PByte;
  PPointerArray = ^TPointerArray;

  Tcmd_PngToRAw = class(TCommand)
  public
    png: string;
    raw: string;
    constructor create;override;
    procedure InitExpense;override;
    procedure DoExecute;override;

  end;

  Tcmd_JpegToBitmap = class(TCommand)
  private
    FJpg: TJpegImage;
    FBmp: vcl.graphics.Tbitmap;
    FDontDestroy: boolean;
  public
    constructor Create;override;
    procedure Init;override;
    procedure DoExecute;override;
    procedure SyncEx;
    property Jpg: TJpegImage read FJpg write FJpg;
    property Bmp: vcl.graphics.Tbitmap read FBmp write FBmp;
    property DontDestroy: boolean read FDontDestroy write FDontDestroy;

  end;

  TGIFConversionQueue = class(TLockQueuedObject)
  protected
    FQueue: TStringList;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure AddToQueue(sFile: ansistring);
    function IsStillInQueue(sFile: ansistring): boolean;
    function processSingle: boolean;
    procedure ProcessAll;
    function GetFileFromQueue: ansistring;
    procedure RemoveFromQueue(sFile: ansistring);
  end;





  TFilterInfo = record
    color: TGiantColor;
    pixcount: integer;
    function Avg: TGiantColor;
  end;


  TGiantColorMatrix = record
  private
    FH: integer;
    FW: integer;
    function GetPixels(x, y: integer): TGiantColor;
    procedure SetPixels(x, y: integer; const Value: TGiantColor);
  public
    canvas: array of array of TGiantColor;
    procedure init(w,h: integer);
    procedure Clear(color: TGiantColor);
    property Pixels[x,y: integer]: TGiantColor read GetPixels write SetPixels;
    property W: integer read FW write FW;
    property H: integer read FH write FH;
    procedure RetrieveBitmap(b: Vcl.Graphics.TBitmap);overload;
    procedure RetrieveBitmap(b: TPicture);overload;
    procedure AddPoint(x,y: nativefloat; c: TGiantColor);

  end;


function GEtIMageFileDimensions(sFile: string; out w, h: nativeint): boolean;


function PNGtoGPBitmap(self: TPNGImage): TGPBitmap;


function CanvasResample(canvas: TCanvas; x,y: nativefloat): TColor;overload;
function CanvasResample(canvas: TfastCanvas; x,y: nativefloat): TColor;overload;
procedure REsizeImages(sInputDir, sFileSpec: string; w,h: nativeint);
function RBSwap(c: TColor): TColor;
function CreateBlankBitmap(w,h: integer; pixel_format: TPixelformat = pf24bit): Vcl.Graphics.TBitmap;//ok
function CopyCreateBitmap(bmsource: Tgraphic): Vcl.Graphics.TBitmap;//ok
procedure GobbleBlack(bm: Vcl.Graphics.TBitmap);//ok
procedure Darken(bm: Vcl.Graphics.TBitmap; x1,y1,x2,y2: integer; rPercent: nativefloat);//ok
function NewCrop(bm: Vcl.Graphics.TBitmap; x1,y1,x2,y2: integer): Vcl.Graphics.TBitmap;overload;//bm
function NewCrop(bm: TFastBitMap; x1,y1,x2,y2: integer): TFastBitMap;overload;//bm
procedure RemoveColor(bm: Vcl.Graphics.TBitmap);overload;//ok
procedure RemoveColorTotalWhite(bm: Vcl.Graphics.TBitmap; grayBound: integer = 0; lowIntenseRange: nativefloat = 0.6; highIntenseRange: nativefloat = 1.0);overload;//ok
procedure RemoveColorTotalWhite(bm: TFastBitmap; grayBound: integer = 0; lowIntenseRange: nativefloat = 0.6; highIntenseRange: nativefloat = 1.0);overload;//ok
procedure Invert(bm: Vcl.Graphics.TBitmap; x1,y1,x2,y2: integer);overload;//ok
procedure Invert(bm: TfastBitmap; x1,y1,x2,y2: integer; bPreserveOriginalAlpha: boolean = true);overload;//ok
procedure ResizeImage(png: TPNGImage; newwidth: integer; newheight: integer);overload;//ok
procedure ResizeImage(bm: TFastBitmap; newwidth: integer; newheight: integer);overload;//ok

procedure ResizeImage(bm: Vcl.Graphics.TBitmap; newwidth: integer; newheight: integer);overload;//ok
procedure ResizeImage(bm: TJpegimage; newwidth: integer; newheight: integer);overload;//ok
procedure FasterResizeImage(bm: Vcl.Graphics.TBitmap; newwidth: integer; newheight: integer);overload;//ok
procedure FasterResizeImage(bm: TFastBitmap; newwidth: integer; newheight: integer);overload;//ok

function GetJpegFromBitMap(bm: Vcl.Graphics.TBitmap): Tjpegimage;
procedure SaveAsJpeg(bm: Vcl.Graphics.TBitmap; sfile: ansistring);
procedure NegativeImage(bm: Vcl.Graphics.TBitmap);//ok
function DuplicateBitmap(bm: Vcl.Graphics.TBitmap): Vcl.Graphics.TBitmap;//ok
procedure TotalWhiteAndTotalBlack(bm: Vcl.Graphics.TBitmap);//ok
function JpegToBitmap(bm: TJpegImage; bDontDestroy: boolean = false): Vcl.Graphics.TBitmap;
function JpegToBitmap_TS(bm: TJpegImage; bDontDestroy: boolean = false): Vcl.Graphics.TBitmap;
function JpegToBitmap_MT(bm: TJpegImage; bDontDestroy: boolean = false): Vcl.Graphics.TBitmap;
function SmallColorToGiantColor(var sc: TSmallColor): TGiantColor;
function ColorToGiantColor(sc: TColor): TGiantColor;
function GiantColorToColor(gc: TGiantColor): TColor;
procedure SetGiantColorToPointer(gc: TGiantColor; pColor: pointer; pAlpha: pointer=nil);

function GetColorFromPointer(p: pointer): Tcolor;inline;
procedure SetColorToPointer(p: pointer; c: TColor);inline;
function GetAlphaFromPointer(p: pointer): integer;inline;
procedure SetAlphaToPointer(p: pointer; a: integer);inline;
function CountPNGColors(png: TPNGImage; iStopAt: integer = 256): integer;overload;

procedure AutoQuantizePNG(png: TPNGImage; iColorTarget: integer = 256; bDither: boolean=true; bDitherAlpha: boolean = false; debugger: TImage= nil);overload;

procedure QuantizePNG(png: TPNGImage; rLevels, gLevels, bLevels, aLevels: integer; bDither: boolean=true; bDitherAlpha: boolean = false);overload;
procedure QuantizeFB(fb: TFastBitmap; rLevels, gLevels, bLevels, aLevels: integer; bDither: boolean=true; bDitherAlpha: boolean = false);overload;
function DitherQuantize(x,y: integer; rColorLevel: nativefloat; iLevels: integer): nativefloat;
function ConvertPNGToGIF(pngNotOwned: TPNGImage): TGIFImage;overload;
function ConvertPNGToGIF(sPNGFile: ansistring; sGIFFile: ansistring = ''): boolean;overload;
function QueueConvertPNGToGIF(sPNGFile: ansistring): boolean;overload;
function GetPNGColors(png: TPNGImage; var colors: array of TColor): integer;overload;

function FocusPNG(png: TPNGImage; xFocus, yFocus: nativefloat): boolean;overload;
function PointerToColor(x_fromcorner,y_fromcorner,w,h: nativeint; p_baseofbitmap: PByte; iBitsPerPixel: nativeint): TColor;

function Minimum(ary: array of TFastBitmap): TFastBitmap;



var
  GifQueue : TGifConversionQueue;
  MainThreadID: DWORD;

implementation

uses AppLock, advancedgraphics, multibuffermemoryfilestream, CommandIcons;

function QueueConvertPNGToGIF(sPNGFile: ansistring): boolean;overload;
begin
  result := true;

  GifQueue.AddToQueue(sPNGFile);
  While GifQueue.IsStillInQueue(sPNGFile) do
    sleep(500);


end;

function SmallColorToGiantColor(var sc: TSmallColor): TGiantColor;
begin
  result.a := sc.a /255;
  result.r := sc.r /255;
  result.g := sc.g /255;
  result.b := sc.b /255;

end;

function ColorToGiantColor(sc: TColor): TGiantColor;
begin
  result.r := ((sc shr 0) and 255)/255;
  result.g := ((sc shr 8) and 255)/255;
  result.b := ((sc shr 16) and 255)/255;
  result.a := 1;

end;
function GiantColorToColor(gc: TGiantColor): TColor;
begin
  result:=(round(gc.r *255) shl 0) +
          (round(gc.g *255) shl 8)+
          (round(gc.b *255) shl 16);


end;

procedure SetColorToPointer(p: pointer; c: TColor);
var
  pp: PByte;
begin
  pp := PByte(p);
  pp[0] := byte((c shr 0) and 255);
  pp[1] := byte((c shr 8) and 255);
  pp[2] := byte((c shr 16) and 255);


end;
function GetColorFromPointer(p: pointer): Tcolor;inline;
var
  pp: PByte;
begin
  pp := PByte(p);
  result := ord(pp[0])+(ord(pp[1]) shl 8)+(ord(pp[2]) shl 16);

end;


function NewCrop(bm: TFastBitMap; x1,y1,x2,y2: integer): TFastBitMap;
var
  x,y: integer;
  c1,c2,c3,c4: integer;
  c: TColor;
  p,pd: PByte;
begin
  result := TFastBitMap.create;
  result.canvas.lock;
  bm.PixelFormat := pf32Bit;
  bm.canvas.lock;
  try
    result.Allocate(x2-x1+1,y2-y1+1);
    result.PixelFormat := pf32bit;

    for y:= y1 to y2 do begin
      if y > bm.height-1 then
        continue;
      p := bm.ScanLine[y];
      pd := result.ScanLine[y-y1];
      for x := x1 to x2 do begin
        if x > bm.width-1 then
          continue;

        c1 := ord(p[(x*4)+0]);
        c2 := ord(p[(x*4)+1]);
        c3 := ord(p[(x*4)+2]);
        c4 := ord(p[(x*4)+3]);
        pd[((x-x1)*4)+0] := byte(c1);
        pd[((x-x1)*4)+1] := byte(c2);
        pd[((x-x1)*4)+2] := byte(c3);
        pd[((x-x1)*4)+3] := byte(c4);

        //bm.Canvas.Pixels[x,y] := c;
      end;
    end;
  finally
    result.canvas.unlock;
    bm.canvas.unlock;
  end;

end;

function NewCrop(bm: Vcl.Graphics.TBitmap; x1,y1,x2,y2: integer): Vcl.Graphics.TBitmap;
var
  x,y: integer;
  c1,c2,c3,c4: integer;
  c: TColor;
  p,pd: PByte;
begin
  result := Vcl.Graphics.TBitmap.create;
  result.canvas.lock;
  bm.PixelFormat := pf32Bit;
  bm.canvas.lock;
  try
    result.Width := x2-x1+1;
    result.Height := y2-y1+1;
  //  result.canvas.Draw(0,0,bm);
    result.Canvas.FillRect(Rect(0,0,result.width,result.height));
    result.PixelFormat := pf32bit;

    for y:= y1 to y2 do begin
      p := bm.ScanLine[y];
      pd := result.ScanLine[y-y1];
      for x := x1 to x2 do begin

        c1 := ord(p[(x*4)+0]);
        c2 := ord(p[(x*4)+1]);
        c3 := ord(p[(x*4)+2]);
        c4 := ord(p[(x*4)+3]);
        pd[((x-x1)*4)+0] := byte(c1);
        pd[((x-x1)*4)+1] := byte(c2);
        pd[((x-x1)*4)+2] := byte(c3);
        pd[((x-x1)*4)+3] := byte(c4);

        //bm.Canvas.Pixels[x,y] := c;
      end;
    end;
  finally
    result.canvas.unlock;
    bm.canvas.unlock;
  end;

end;

procedure Darken(bm: Vcl.Graphics.TBitmap; x1,y1,x2,y2: integer; rPercent: nativefloat);
var
  x,y: integer;
  c1,c2,c3,c4: integer;
  p: PByte;
begin
  bm.canvas.lock;
  try
    bm.PixelFormat := pf32Bit;
    for y:= y1 to y2 do begin
      for x := x1 to x2 do begin

        p := bm.ScanLine[y];
        c1 := ord(p[(x*4)+0]);
        c2 := ord(p[(x*4)+1]);
        c3 := ord(p[(x*4)+2]);
        c4 := ord(p[(x*4)+3]);
        c1 := round(c1 * rPercent);
        c2 := round(c2 * rPercent);
        c3 := round(c3 * rPercent);
        c4 := round(c4 * rPercent);
        p[(x*4)+0] := byte(c1);
        p[(x*4)+1] := byte(c2);
        p[(x*4)+2] := byte(c3);
        p[(x*4)+3] := byte(c4);


        //bm.Canvas.Pixels[x,y] := c;
      end;
    end;
  finally
    bm.canvas.unlock;
  end;

end;

procedure RemoveColor(bm: Vcl.Graphics.TBitmap);
var

  x,y: integer;
  c1,c2,c3,c4: integer;
  c: TColor;
  p: PByte;
begin
  bm.canvas.lock;
  try
    bm.PixelFormat := pf32Bit;
    for y:= 0 to bm.height-1 do begin
      for x := 0 to bm.Width-1 do begin

        p := bm.ScanLine[y];
        c1 := ord(p[(x*4)+0]);
        c2 := ord(p[(x*4)+1]);
        c3 := ord(p[(x*4)+2]);
        c4 := ord(p[(x*4)+3]);

        if (c1 <> c2) or (c1 <> c3) or (c2 <> c3) then begin
          c1 := 0;
          c2 := 0;
          c3 := 0;
          c4 := 0;

          p[(x*4)+0] := Byte(c1) ;
          p[(x*4)+1] := Byte(c2);
          p[(x*4)+2] := Byte(c3);
          p[(x*4)+3] := Byte(c4);
        end;

        //bm.Canvas.Pixels[x,y] := c;
      end;
    end;
  finally
    bm.canvas.unlock;
  end;
end;



procedure NegativeImage(bm: Vcl.Graphics.TBitmap);
var
  x,y: integer;
  c1,c2,c3,c4: integer;
  c: TColor;
  p: PByte;
begin
    bm.canvas.lock;
    bm.PixelFormat := pf32Bit;
    try
      for y:= 0 to bm.height-1 do begin
        for x := 0 to bm.Width-1 do begin

          p := bm.ScanLine[y];
          c1 := ord(p[(x*4)+0]);
          c2 := ord(p[(x*4)+1]);
          c3 := ord(p[(x*4)+2]);
          c4 := ord(p[(x*4)+3]);

          c1 := 255-c1;
          c2 := 255-c2;
          c3 := 255-c3;


          p[(x*4)+0] := Byte(c1);
          p[(x*4)+1] := Byte(c2);
          p[(x*4)+2] := Byte(c3);
          p[(x*4)+3] := Byte(c4);



          //bm.Canvas.Pixels[x,y] := c;
        end;
      end;
    finally
      bm.Canvas.Unlock;
    end;

end;

procedure AddBlend(bmTarget: Vcl.Graphics.TBitmap; bmSource: Vcl.Graphics.TBitmap);
var
  x,y: integer;
  c1,c2,c3,c4,cc1,cc2,cc3,cc4: integer;
  c: TColor;
  p: PByte;
  p2: PByte;
  bm: Vcl.Graphics.TBitmap;
begin
  bm := bmTarget;

    bm.PixelFormat := pf32Bit;
    bm.canvas.lock;
    try
      for y:= 0 to bm.height-1 do begin
        p := bm.ScanLine[y];
        p2 := bmSource.Scanline[y];

        for x := 0 to bm.Width-1 do begin


          c1 := ord(p[(x*4)+0]);
          c2 := ord(p[(x*4)+1]);
          c3 := ord(p[(x*4)+2]);
          c4 := ord(p[(x*4)+3]);

          cc1 := ord(p2[(x*4)+0]);
          cc2 := ord(p2[(x*4)+1]);
          cc3 := ord(p2[(x*4)+2]);
          cc4 := ord(p2[(x*4)+3]);



          c1 := c1+cc1;
          c2 := c2+cc2;
          c3 := c3+cc3;
          if c1 > 255 then c1 := 255;
          if c2 > 255 then c2 := 255;
          if c3 > 255 then c3 := 255;


          p[(x*4)+0] := Byte(c1);
          p[(x*4)+1] := Byte(c2);
          p[(x*4)+2] := Byte(c3);
          p[(x*4)+3] := Byte(c4);



          //bm.Canvas.Pixels[x,y] := c;
        end;
      end;
    finally
      bm.Canvas.Unlock;
    end;

end;




procedure RemoveColorTotalWhite(bm: TFastBitmap; grayBound: integer = 0; lowIntenseRange: nativefloat = 0.6; highIntenseRange: nativefloat = 1.0);overload;//ok
var
  x,y: integer;
  c1,c2,c3,c4: integer;
  c: TColor;
  p: PByte;
begin
    bm.canvas.lock;
    bm.PixelFormat := pf32Bit;
    try
      for y:= 0 to bm.height-1 do begin
        for x := 0 to bm.Width-1 do begin

          p := bm.ScanLine[y];
          c1 := ord(p[(x*4)+0]);
          c2 := ord(p[(x*4)+1]);
          c3 := ord(p[(x*4)+2]);
          c4 := ord(p[(x*4)+3]);

          if (abs(c1 - c2)> grayBound) or (abs(c1 - c3)> grayBound) or (abs(c2 - c3)> grayBound) then begin
            c1 := 0;
            c2 := 0;
            c3 := 0;
            c4 := 0;

          end else begin
            if (c2 >= round(lowIntenseRange*255)) and (c2 <= round(highIntenseRange*255))then begin
              c1 := 255;
              c2 := 255;
              c3 := 255;
              c4 := 0;
            end else begin
              c1 := 0;
              c2 := 0;
              c3 := 0;
              c4 := 0;
            end;
          end;

          p[(x*4)+0] := Byte(c1);
          p[(x*4)+1] := Byte(c2);
          p[(x*4)+2] := Byte(c3);
          p[(x*4)+3] := Byte(c4);



          //bm.Canvas.Pixels[x,y] := c;
        end;
      end;
    finally
      bm.Canvas.Unlock;
    end;

end;

procedure RemoveColorTotalWhite(bm: Vcl.Graphics.TBitmap; grayBound: integer; lowIntenseRange: nativefloat; highIntenseRange: nativefloat);
var
  x,y: integer;
  c1,c2,c3,c4: integer;
  c: TColor;
  p: PByte;
begin
    bm.canvas.lock;
    bm.PixelFormat := pf32Bit;
    try
      for y:= 0 to bm.height-1 do begin
        for x := 0 to bm.Width-1 do begin

          p := bm.ScanLine[y];
          c1 := ord(p[(x*4)+0]);
          c2 := ord(p[(x*4)+1]);
          c3 := ord(p[(x*4)+2]);
          c4 := ord(p[(x*4)+3]);

          if (abs(c1 - c2)> grayBound) or (abs(c1 - c3)> grayBound) or (abs(c2 - c3)> grayBound) then begin
            c1 := 0;
            c2 := 0;
            c3 := 0;
            c4 := 0;

          end else begin
            if (c2 >= round(lowIntenseRange*255)) and (c2 <= round(highIntenseRange*255))then begin
              c1 := 255;
              c2 := 255;
              c3 := 255;
              c4 := 0;
            end else begin
              c1 := 0;
              c2 := 0;
              c3 := 0;
              c4 := 0;
            end;
          end;

          p[(x*4)+0] := Byte(c1);
          p[(x*4)+1] := Byte(c2);
          p[(x*4)+2] := Byte(c3);
          p[(x*4)+3] := Byte(c4);



          //bm.Canvas.Pixels[x,y] := c;
        end;
      end;
    finally
      bm.Canvas.Unlock;
    end;

end;

procedure Invert(bm: Vcl.Graphics.TBitmap; x1,y1,x2,y2: integer);
var
  x,y: integer;
  c1,c2,c3,c4: integer;
  c: TColor;
  p: PByte;
begin
  bm.canvas.lock;
  try
    bm.PixelFormat := pf32Bit;
    for y:= y1 to y2 do begin
      for x := x1 to x2 do begin

        p := bm.ScanLine[y];
        c1 := ord(p[(x*4)+0]);
        c2 := ord(p[(x*4)+1]);
        c3 := ord(p[(x*4)+2]);
        c4 := ord(p[(x*4)+3]);
        p[(x*4)+0] := Byte(255-c1);
        p[(x*4)+1] := Byte(255-c2);
        p[(x*4)+2] := Byte(255-c3);
        p[(x*4)+3] := Byte(255-c4);

        //bm.Canvas.Pixels[x,y] := c;
      end;
    end;
  finally
    bm.canvas.unlock;
  end;

end;

procedure Invert(bm: TFastBitmap; x1,y1,x2,y2: integer; bPreserveOriginalAlpha: boolean = true);
var
  x,y: integer;
  c1,c2,c3,c4: integer;
  c: TColor;
  p: PByte;
begin
  bm.canvas.lock;
  try
    //bm.PixelFormat := pf32Bit;
    for y:= y1 to y2 do begin
      for x := x1 to x2 do begin

        p := bm.ScanLine[y];
        c1 := ord(p[(x*4)+0]);
        c2 := ord(p[(x*4)+1]);
        c3 := ord(p[(x*4)+2]);
        c4 := ord(p[(x*4)+3]);
        p[(x*4)+0] := Byte(0+c1);
        p[(x*4)+1] := Byte(255-c2);
        p[(x*4)+2] := Byte(255-c3);
        p[(x*4)+3] := Byte(255-c4);


        //bm.Canvas.Pixels[x,y] := c;
      end;
    end;
  finally
    bm.canvas.unlock;
  end;

end;


procedure ResizeImage(png: TPNGImage; newwidth: integer; newheight: integer);overload;
//Implmements a simple web page request that performs a high-quailty resize of
//a larger jpeg image.  Not used or dispatched, too CPU intensive currently.
//An experiment for use in the future potentially.
var
  imgOriginal, imgNew: TPNGImage;
  x,y,xx,yy: integer;
  r64,g64,b64: integer;
  pix: array of array of TFilterInfo;
  newx, newy: integer;
  sl: PByte;
  slu: PByte;
  sll: PByte;
  slau: vcl.imaging.pngimage.pbytearray;
  slal: vcl.imaging.pngimage.pbytearray;
  iOHeight: integer;
  ox,oy: integer;
  ixiy: integer;
  sc: TSmallColor;
  byt: byte;
  ulc, urc, llc,lrc, lc, rc,ccc: TGiantColor;
  sx1,sy1,sx2,sy2: integer;
  sla: vcl.imaging.pngimage.pByteArray;
const
  PNG_REC_SIZE = 3;

begin
  imgOriginal := nil;

  png.canvas.lock;
  try
    if (png.height = newheight) and (png.width = newwidth) then exit;

    //Todo 2: evaluate whether this should be here for PNG images.
    //if (bm.Height >= newHeight) and (bm.Width >= newWidth) then begin
    //  FasterResizeImage(bm,newwidth, newheight);
    //  exit;
    //end;

    imgOriginal := TPNGImage.create;
    png.canvas.lock;
    try
      imgOriginal.Assign(png);
    finally
      png.canvas.unlock;
    end;
    imgoriginal.canvas.lock;
    imgNew := TPNGImage.create;
  //  image1 := Timage.create(nil);
    try
      imgNew.createblank(COLOR_RGBALPHA,8,newwidth, newheight);
      iOHeight := trunc((imgNew.height / imgNew.width) * imgOriginal.width);

      //setup the length of the arrays
      setlength(pix, imgNEw.height);
      for y := 0 to imgNew.height-1 do
        setlength(pix[y], imgNew.width);

      //for every row
      newx := 0;
      newy := 0;
      for y := 0 to iOHeight-1 do begin
        if y> imgOriginal.height -1 then
          break;

        oy := newy;
        newy := Trunc((y/iOHeight)*imgNew.height);
  (*      if newy>imgOriginal.picture.graphic.height-1 then
          newy := imgOriginal.picture.graphic.height-1;*)
        //for every pixel across


        sl := imgOriginal.ScanLine[y];
        newx := 0;
        for x:=0 to imgOriginal.width-1 do begin
            ox := newx;
            newx := Trunc((x/imgOriginal.width)*imgNew.Width);

            sx1 := x-1; sy1 := y-1;
            sx2 := x; sy2 := y;
            if sx1 < 0 then sx1 := 0;
            if sy1 < 0 then sy1 := 0;
            slu := imgOriginal.scanline[sy1];
            sll := imgOriginal.scanline[sy2];
            slau := imgOriginal.alphascanline[sy1];
            slal := imgOriginal.alphascanline[sy2];


            //determine four colors for interpolation
            ulc := ColorToGiantColor((ord(slu[(sx1*PNG_REC_SIZE+0)]) shl 0)+(ord(slu[(sx1*PNG_REC_SIZE+1)]) shl 8)+(ord(slu[(sx1*PNG_REC_SIZE+2)]) shl 16));
            urc := ColorToGiantColor((ord(slu[(sx2*PNG_REC_SIZE+0)]) shl 0)+(ord(slu[(sx2*PNG_REC_SIZE+1)]) shl 8)+(ord(slu[(sx2*PNG_REC_SIZE+2)]) shl 16));
            llc := ColorToGiantColor((ord(sll[(sx1*PNG_REC_SIZE+0)]) shl 0)+(ord(sll[(sx1*PNG_REC_SIZE+1)]) shl 8)+(ord(sll[(sx1*PNG_REC_SIZE+2)]) shl 16));
            lrc := ColorToGiantColor((ord(sll[(sx2*PNG_REC_SIZE+0)]) shl 0)+(ord(sll[(sx2*PNG_REC_SIZE+1)]) shl 8)+(ord(sll[(sx2*PNG_REC_SIZE+2)]) shl 16));
            if (slau<>nil) and (slal<>nil) then begin
              ulc.SetAlpha(slau[sx1+0]);
              urc.SetAlpha(slau[sx2+0]);
              llc.SetAlpha(slal[sx1+0]);
              lrc.SetAlpha(slal[sx2+0]);
            end else begin
              ulc.SetAlpha(255);
              urc.SetAlpha(255);
              llc.SetAlpha(255);
              lrc.SetAlpha(255);
            end;

            for yy := oy to newy do begin
              //color blend from top to bottom
              if oy <> newy then begin
                lc := colorblend(ulc, llc, ((yy-oy)/(newy-oy)));
                rc := colorblend(urc, lrc, ((yy-oy)/(newy-oy)));
              end else begin
                lc := ulc;
                rc := urc;
              end;

              for xx := ox to newx do begin
                if ox<> newx then begin
                  ccc := colorblend(lc, rc, (xx-ox)/(newx-ox));
                end else begin
                  ccc := lc;
                end;

                pix[yy,xx].color := pix[yy,xx].color + ccc;

                inc(pix[yy,xx].pixcount);
              end;
            end;

  (*          if newx>imgOriginal.picture.graphic.width-1 then
              newx := imgOriginal.picture.graphic.width-1;*)
  (*          inc(pix[newy,newx].color.r, ord(sl[(x*3+0)]));
            inc(pix[newy,newx].color.g, ord(sl[(x*3+1)]));
            inc(pix[newy,newx].color.b, ord(sl[(x*3+2)]));
            inc(pix[newy,newx].pixcount);*)
        end;
      end;



      //do averaging
      for y:=0 to imgNew.height-1 do begin
        for x:=0 to imgNew.width-1 do begin
          if pix[y,x].pixcount=0 then begin
            pix[y,x].color.r := 0;
            pix[y,x].color.g := 0;
            pix[y,x].color.b := 0;
            pix[y,x].color.a := 0;
          end else begin
            pix[y,x].color := pix[y,x].color / pix[y,x].pixcount;
          end;
        end;
      end;

      //copy result
      for y:=0 to imgNew.height-1 do begin
        sl := imgNew.ScanLine[y];
        for x:=0 to imgNew.width-1 do begin
          sc := pix[y,x].color;
          sl[(x*PNG_REC_SIZE+0)] := sc.r;
          sl[(x*PNG_REC_SIZE+1)] := sc.g;
          sl[(x*PNG_REC_SIZE+2)] := sc.b;
        end;

        sla := imgNew.AlphaScanLine[y];
        for x:=0 to imgNew.width-1 do begin
          byt := round(pix[y,x].color.a * 255);
          sla[x] := byt;//assignes alpha value
        end;


      end;

      png.Assign(imgNew);

    finally
      imgNew.canvas.unlock;
      imgOriginal.canvas.unlock;
      imgNew.free;
      imgOriginal.free;

    end;
  finally
    if assigned(imgOriginal) then
      imgoriginal.canvas.unlock
  end;
end;


procedure ResizeImage(bm: Vcl.Graphics.TBitmap; newwidth: integer; newheight: integer);
//Implmements a simple web page request that performs a high-quailty resize of
//a larger jpeg image.  Not used or dispatched, too CPU intensive currently.
//An experiment for use in the future potentially.
type
  TGiantColor = record
    r,g,b: integer;
  { TSmallColor }
  end;
  TFilterInfo = record
    color: TGiantColor;
    pixcount: integer;
  end;
var
  image1: Vcl.Graphics.TBitmap;
  imgOriginal: Vcl.Graphics.TBitmap;
  x,y,xx,yy: integer;
  r64,g64,b64: integer;
  pix: array of array of TFilterInfo;
  newx, newy: integer;
  sl: PByte;
  slu: PByte;
  sll: PByte;
  iOHeight: integer;
  ox,oy: integer;
  ixiy: integer;
  ulc, urc, llc,lrc, lc, rc,ccc: TColor;
  sx1,sy1,sx2,sy2: integer;
begin
  bm.canvas.lock;
  try
    if newheight < 1 then begin
      newheight := trunc((bm.height/bm.width)*newwidth);
    end;
    if (bm.height = newheight) and (bm.width = newwidth) then exit;

    if (bm.Height >= newHeight) and (bm.Width >= newWidth) then begin
      FasterResizeImage(bm,newwidth, newheight);
      exit;
    end;




    imgOriginal := Vcl.Graphics.TBitmap.create;
    bm.canvas.lock;
    try
      imgOriginal.Assign(bm);
    finally
      bm.canvas.unlock;
    end;
    imgoriginal.canvas.lock;

    image1 := Vcl.Graphics.TBitmap.create;
    try
      image1.canvas.lock;
      image1.width := newwidth;
      image1.height := newheight;

      iOHeight := trunc((image1.height / image1.width) * imgOriginal.width);

      image1.Width := image1.width;
      image1.height := image1.height;

      image1.Canvas.Rectangle(0,0,image1.width, image1.height);

  //    imgOriginal.canvas.lock;
      imgOriginal.PixelFormat := pf24bit;
  //    imgOriginal.canvas.unlock;

      image1.PixelFormat := pf24bit;
      //setup the length of the arrays
      setlength(pix, image1.height);
      for y := 0 to image1.height-1 do
        setlength(pix[y], image1.width);

      //for every row
      newx := 0;
      newy := 0;
      for y := 0 to iOHeight-1 do begin
        if y> imgOriginal.height -1 then
          break;

        oy := newy;
        newy := Trunc((y/iOHeight)*image1.height);
  (*      if newy>imgOriginal.picture.graphic.height-1 then
          newy := imgOriginal.picture.graphic.height-1;*)
        //for every pixel across


        sl := imgOriginal.ScanLine[y];
        newx := 0;
        for x:=0 to imgOriginal.width-1 do begin
            ox := newx;
            newx := Trunc((x/imgOriginal.width)*image1.Width);

            sx1 := x-1; sy1 := y-1;
            sx2 := x; sy2 := y;
            if sx1 < 0 then sx1 := 0;
            if sy1 < 0 then sy1 := 0;
            slu := imgOriginal.scanline[sy1];
            sll := imgOriginal.scanline[sy2];


            //determine four colors for interpolation
            ulc := (ord(slu[(sx1*3+0)]) shl 0)+(ord(slu[(sx1*3+1)]) shl 8)+(ord(slu[(sx1*3+2)]) shl 16);
            urc := (ord(slu[(sx2*3+0)]) shl 0)+(ord(slu[(sx2*3+1)]) shl 8)+(ord(slu[(sx2*3+2)]) shl 16);
            llc := (ord(sll[(sx1*3+0)]) shl 0)+(ord(sll[(sx1*3+1)]) shl 8)+(ord(sll[(sx1*3+2)]) shl 16);
            lrc := (ord(sll[(sx2*3+0)]) shl 0)+(ord(sll[(sx2*3+1)]) shl 8)+(ord(sll[(sx2*3+2)]) shl 16);

            for yy := oy to newy do begin
              //color blend from top to bottom
              if oy <> newy then begin
                lc := colorblend(ulc, llc, (yy-oy)/(newy-oy));
                rc := colorblend(urc, lrc, (yy-oy)/(newy-oy));
              end else begin
                lc := ulc;
                rc := urc;
              end;

              for xx := ox to newx do begin
                if ox<> newx then begin
                  ccc := colorblend(lc, rc, (xx-ox)/(newx-ox));
                end else begin
                  ccc := lc;
                end;


                inc(pix[yy,xx].color.r, (ccc shr 0) and 255);
                inc(pix[yy,xx].color.g, (ccc shr 8) and 255);
                inc(pix[yy,xx].color.b, (ccc shr 16) and 255);
                inc(pix[yy,xx].pixcount);
              end;
            end;

  (*          if newx>imgOriginal.picture.graphic.width-1 then
              newx := imgOriginal.picture.graphic.width-1;*)
  (*          inc(pix[newy,newx].color.r, ord(sl[(x*3+0)]));
            inc(pix[newy,newx].color.g, ord(sl[(x*3+1)]));
            inc(pix[newy,newx].color.b, ord(sl[(x*3+2)]));
            inc(pix[newy,newx].pixcount);*)
        end;
      end;

      //do averaging
      for y:=0 to image1.height-1 do begin
        for x:=0 to image1.width-1 do begin
          if pix[y,x].pixcount=0 then begin
            pix[y,x].color.r := 0;
            pix[y,x].color.g := 0;
            pix[y,x].color.b := 0;
          end else begin
            pix[y,x].color.r := pix[y,x].color.r div pix[y,x].pixcount;
            pix[y,x].color.g := pix[y,x].color.g div pix[y,x].pixcount;
            pix[y,x].color.b := pix[y,x].color.b div pix[y,x].pixcount;
          end;
        end;
      end;

      //copy result
      for y:=0 to image1.height-1 do begin
        sl := image1.ScanLine[y];
        for x:=0 to image1.width-1 do begin
          sl[(x*3+0)] := byte(pix[y,x].color.r);
          sl[(x*3+1)] := byte(pix[y,x].color.g);
          sl[(x*3+2)] := byte(pix[y,x].color.b);

        end;
      end;

      bm.Assign(image1);

    finally
      image1.canvas.unlock;
      imgOriginal.canvas.unlock;
      image1.free;
      imgOriginal.free;

    end;
  finally
    bm.canvas.unlock;
  end;
end;


procedure ResizeImage(bm: TFastBitmap; newwidth: integer; newheight: integer);
//Implmements a simple web page request that performs a high-quailty resize of
//a larger jpeg image.  Not used or dispatched, too CPU intensive currently.
//An experiment for use in the future potentially.
type
//  TGiantColor = record
//    r,g,b: integer;
//  { TSmallColor }
//  end;
  TFilterInfo = record
    color: TGiantColor;
    pixcount: integer;
  end;
var
  image1: TFastBitmap;
  imgOriginal: TFAstbitmap;
  x,y,xx,yy: integer;
  r64,g64,b64: integer;
  pix: array of array of TFilterInfo;
  newx, newy: integer;
  iOHeight: integer;
  ox,oy: integer;
  ixiy: integer;
  ulc, urc, llc,lrc, lc, rc,ccc: TColor;
  sx1,sy1,sx2,sy2: integer;
begin
  bm.canvas.lock;
  try
    if newheight < 1 then begin
      newheight := trunc((bm.height/bm.width)*newwidth);
    end;
    if (bm.height = newheight) and (bm.width = newwidth) then exit;

//    if (bm.Height >= newHeight) and (bm.Width >= newWidth) then begin
//      FasterResizeImage(bm,newwidth, newheight);
//      exit;
//    end;




    imgOriginal := TFastBitmap.create;
    bm.canvas.lock;
    try
      imgOriginal.Assign(bm);
    finally
      bm.canvas.unlock;
    end;
    imgoriginal.canvas.lock;

    image1 := TFastBitmap.create;
    try
      image1.canvas.lock;
      image1.width := newwidth;
      image1.height := newheight;
      image1.new;
      image1.EnableAlpha := imgOriginal.EnableAlpha;

      iOHeight := trunc((image1.height / image1.width) * imgOriginal.width);

      image1.Width := image1.width;
      image1.height := image1.height;

      image1.Canvas.Rectangle(0,0,image1.width, image1.height);

  //    imgOriginal.canvas.lock;
      imgOriginal.PixelFormat := pf32bit;
  //    imgOriginal.canvas.unlock;

      image1.PixelFormat := pf32bit;
      //setup the length of the arrays
      setlength(pix, image1.height);
      for y := 0 to image1.height-1 do
        setlength(pix[y], image1.width);

      //for every row
      newx := 0;
      newy := 0;
      for y := 0 to iOHeight-1 do begin
        if y> imgOriginal.height -1 then
          break;

        oy := newy;
        newy := Trunc((y/iOHeight)*image1.height);
  (*      if newy>imgOriginal.picture.graphic.height-1 then
          newy := imgOriginal.picture.graphic.height-1;*)
        //for every pixel across

        newx := 0;
        for x:=0 to imgOriginal.width-1 do begin
            ox := newx;
            newx := Trunc((x/imgOriginal.width)*image1.Width);

            sx1 := x-1; sy1 := y-1;
            sx2 := x; sy2 := y;
            if sx1 < 0 then sx1 := 0;
            if sy1 < 0 then sy1 := 0;


            //determine four colors for interpolation
            ulc := imgOriginal.canvas.pixels[sx1,sy1];
            urc := imgOriginal.canvas.pixels[sx2,sy1];
            llc := imgOriginal.canvas.pixels[sx1,sy2];
            lrc := imgOriginal.canvas.pixels[sx2,sy2];

            for yy := oy to newy do begin
              //color blend from top to bottom
              if oy <> newy then begin
                lc := colorblendRGBA(ulc, llc, (yy-oy)/(newy-oy));
                rc := colorblendRGBA(urc, lrc, (yy-oy)/(newy-oy));
              end else begin
                lc := ulc;
                rc := urc;
              end;

              for xx := ox to newx do begin
                if ox<> newx then begin
                  ccc := colorblendRGBA(lc, rc, (xx-ox)/(newx-ox));
                end else begin
                  ccc := lc;
                end;


                pix[yy,xx].color.r := pix[yy,xx].color.r + (ccc shr 0) and 255;
                pix[yy,xx].color.g := pix[yy,xx].color.g + (ccc shr 8) and 255;
                pix[yy,xx].color.b := pix[yy,xx].color.b + (ccc shr 16) and 255;
                pix[yy,xx].color.a := pix[yy,xx].color.a + (ccc shr 24) and 255;

//                inc(pix[yy,xx].color.r, (ccc shr 0) and 255);
//                inc(pix[yy,xx].color.g, (ccc shr 8) and 255);
//                inc(pix[yy,xx].color.b, (ccc shr 16) and 255);
                inc(pix[yy,xx].pixcount);
              end;
            end;

  (*          if newx>imgOriginal.picture.graphic.width-1 then
              newx := imgOriginal.picture.graphic.width-1;*)
  (*          inc(pix[newy,newx].color.r, ord(sl[(x*3+0)]));
            inc(pix[newy,newx].color.g, ord(sl[(x*3+1)]));
            inc(pix[newy,newx].color.b, ord(sl[(x*3+2)]));
            inc(pix[newy,newx].pixcount);*)
        end;
      end;

      //do averaging
      for y:=0 to image1.height-1 do begin
        for x:=0 to image1.width-1 do begin
          if pix[y,x].pixcount=0 then begin
            pix[y,x].color.r := 0;
            pix[y,x].color.g := 0;
            pix[y,x].color.b := 0;
            pix[y,x].color.a := 0;
          end else begin
            pix[y,x].color.r := pix[y,x].color.r / pix[y,x].pixcount;
            pix[y,x].color.g := pix[y,x].color.g / pix[y,x].pixcount;
            pix[y,x].color.b := pix[y,x].color.b / pix[y,x].pixcount;
{$DEFINE FORCE_1_ALPHA}
{$IFDEF FORCE_1_ALPHA}
            pix[y,x].color.a := 255;
{$ELSE}
            pix[y,x].color.a := pix[y,x].color.a / pix[y,x].pixcount;
{$ENDIF}
          end;
        end;
      end;

      //copy result
      for y:=0 to image1.height-1 do begin
        for x:=0 to image1.width-1 do begin
          image1.Canvas.pixels[x,y] := pix[y,x].color;
          if (x=0) and (y=0) then
            Debug.Log('UpperLeft: '+inttohex(image1.Canvas.pixels[x,y],8));
        end;
      end;

      bm.Assign(image1);

    finally
      image1.canvas.unlock;
      imgOriginal.canvas.unlock;
      image1.free;
      imgOriginal.free;

    end;
  finally
    bm.canvas.unlock;
  end;
end;




procedure FasterResizeImage(bm: Vcl.Graphics.TBitmap; newwidth: integer; newheight: integer);
//Implmements a simple web page request that performs a high-quailty resize of
//a larger jpeg image.  Not used or dispatched, too CPU intensive currently.
//An experiment for use in the future potentially.

var
  image1: Vcl.Graphics.TBitmap;
  imgOriginal: Vcl.Graphics.TBitmap;
  x,y,xx,yy: integer;
  r64,g64,b64: integer;
  pix: array of array of TFilterInfo;
  newx, newy: integer;
  newxs: array of integer;
  sl: PByte;
  iOHeight: integer;
  ox,oy: integer;
  ixiy: integer;
  c,ulc, urc, llc,lrc, lc, rc,ccc: TColor;
  sx,sy: integer;
  tm1,tm2: cardinal;
  sc: TSmallColor;
begin
  bm.canvas.lock;
  try
    //use different algorythm if the target image will be larger
    if (bm.Height < newHeight) or (bm.Width < newWidth) then begin
      ResizeImage(bm,newwidth, newheight);
      exit;
    end;


    imgOriginal := Vcl.Graphics.TBitmap.create;
    bm.canvas.lock;
    try
      imgOriginal.Assign(bm);
    finally
      bm.canvas.unlock;
    end;
    imgoriginal.canvas.lock;

    image1 := Vcl.Graphics.TBitmap.create;
    try
      image1.canvas.lock;
      image1.width := newwidth;
      image1.height := newheight;

      iOHeight := trunc((image1.height / image1.width) * imgOriginal.width);

      image1.Width := image1.width;
      image1.height := image1.height;

      image1.Canvas.Rectangle(0,0,image1.width, image1.height);

  //    imgOriginal.canvas.lock;
      imgOriginal.PixelFormat := pf24bit;
  //    imgOriginal.canvas.unlock;

      image1.PixelFormat := pf24bit;
      //setup the length of the arrays
      setlength(pix, image1.height);
      for y := 0 to image1.height-1 do
        setlength(pix[y], image1.width);

      //for every row
      newx := 0;
      newy := 0;
      tm1 := GetTicker;
      //build newx result matrix (for optimimum speed)
      SetLength(newxs, imgOriginal.width);
      for x:=0 to imgOriginal.width-1 do begin
        newxs[x] := Trunc((x/imgOriginal.width)*image1.Width);

      end;

      for y := 0 to iOHeight-1 do begin
        if y> imgOriginal.height -1 then
          break;

        oy := newy;
        newy := Trunc((y/iOHeight)*image1.height);
  (*      if newy>imgOriginal.picture.graphic.height-1 then
          newy := imgOriginal.picture.graphic.height-1;*)
        //for every pixel across


        sl := imgOriginal.ScanLine[y];




        //add up pixels

        for x:=0 to imgOriginal.width-1 do begin
            newx := newxs[x];
  //          newx := Trunc((x/imgOriginal.width)*image1.Picture.Graphic.Width);

            sx := x; sy := y;

            //determine four color

            c := (ord(sl[(sx*3+0)]) shl 0)+(ord(sl[(sx*3+1)]) shl 8)+(ord(sl[(sx*3+2)]) shl 16);

            pix[newy,newx].color := pix[newy,newx].color+c;
            inc(pix[newy,newx].pixcount);

        end;


      end;
      tm2 := GetTicker;
  //    showmessage(inttostr(tm2-tm1));

      //do averaging
      for y:=0 to image1.height-1 do begin
        for x:=0 to image1.width-1 do begin
          if pix[y,x].pixcount=0 then begin
            pix[y,x].color.r := 0;
            pix[y,x].color.g := 0;
            pix[y,x].color.b := 0;
          end else begin
            pix[y,x].color := pix[y,x].color / pix[y,x].pixcount;
          end;
        end;
      end;

      //copy result
      for y:=0 to image1.height-1 do begin
        sl := image1.ScanLine[y];
        for x:=0 to image1.width-1 do begin
          sc := pix[y,x].color;
          sl[(x*3+0)] := byte(sc.r);
          sl[(x*3+1)] := byte(sc.g);
          sl[(x*3+2)] := byte(sc.b);
        end;
      end;

      bm.Assign(image1);

    finally
      image1.canvas.unlock;
      imgOriginal.canvas.unlock;
      image1.free;
      imgOriginal.free;

    end;
  finally
    bm.canvas.unlock;
  end;
end;

procedure FasterResizeImage(bm: TFastBitmap; newwidth: integer; newheight: integer);
//Implmements a simple web page request that performs a high-quailty resize of
//a larger jpeg image.  Not used or dispatched, too CPU intensive currently.
//An experiment for use in the future potentially.

var
  image1: TFastbitmap;
  imgOriginal: TFastbitmap;
  x,y,xx,yy: integer;
  r64,g64,b64: integer;
  pix: array of array of TFilterInfo;
  newx, newy: integer;
  newxs: array of integer;
  iOHeight: integer;
  ox,oy: integer;
  ixiy: integer;
  c,ulc, urc, llc,lrc, lc, rc,ccc: TColor;
  sx,sy: integer;
  tm1,tm2: cardinal;
  sc: TSmallColor;
begin
  bm.canvas.lock;
  try
    //use different algorythm if the target image will be larger
    if (bm.Height < newHeight) or (bm.Width < newWidth) then begin
      ResizeImage(bm,newwidth, newheight);
      exit;
    end;


    imgOriginal := TFastBitmap.create;
    bm.canvas.lock;
    try
      imgOriginal.Assign(bm);
    finally
      bm.canvas.unlock;
    end;
    imgoriginal.canvas.lock;

    image1 := TFastBitmap.create;
    try
      image1.canvas.lock;
      image1.width := newwidth;
      image1.height := newheight;

      iOHeight := trunc((image1.height / image1.width) * imgOriginal.width);

      image1.Width := image1.width;
      image1.height := image1.height;

      //image1.Canvas.Rectangle(0,0,image1.width, image1.height);
      image1.new;

  //    imgOriginal.canvas.lock;
      imgOriginal.PixelFormat := pf32bit;
  //    imgOriginal.canvas.unlock;

      image1.PixelFormat := pf32bit;
      //setup the length of the arrays
      setlength(pix, image1.height);
      for y := 0 to image1.height-1 do
        setlength(pix[y], image1.width);

      //for every row
      newx := 0;
      newy := 0;
      tm1 := GetTicker;
      //build newx result matrix (for optimimum speed)
      SetLength(newxs, imgOriginal.width);
      for x:=0 to imgOriginal.width-1 do begin
        newxs[x] := Trunc((x/imgOriginal.width)*image1.Width);

      end;

      for y := 0 to iOHeight-1 do begin
        if y> imgOriginal.height -1 then
          break;

        oy := newy;
        newy := Trunc((y/iOHeight)*image1.height);
  (*      if newy>imgOriginal.picture.graphic.height-1 then
          newy := imgOriginal.picture.graphic.height-1;*)
        //for every pixel across






        //add up pixels

        for x:=0 to imgOriginal.width-1 do begin
            newx := newxs[x];
  //          newx := Trunc((x/imgOriginal.width)*image1.Picture.Graphic.Width);

            sx := x; sy := y;

            //determine four color

            c := imgOriginal.Canvas.Pixels[sx,sy];

            pix[newy,newx].color := pix[newy,newx].color+c;
            inc(pix[newy,newx].pixcount);

        end;


      end;
      tm2 := GetTicker;
  //    showmessage(inttostr(tm2-tm1));

      //do averaging
      for y:=0 to image1.height-1 do begin
        for x:=0 to image1.width-1 do begin
          if pix[y][x].pixcount=0 then begin
            pix[y][x].color.r := 0;
            pix[y][x].color.g := 0;
            pix[y][x].color.b := 0;
          end else begin
            pix[y][x].color := pix[y][x].color / pix[y][x].pixcount;
          end;
        end;
      end;

      //copy result
      for y:=0 to image1.height-1 do begin
        for x:=0 to image1.width-1 do begin
          image1.Canvas.Pixels[x,y] := pix[y][x].color;
        end;
      end;

      bm.Assign(image1);

    finally
      image1.canvas.unlock;
      imgOriginal.canvas.unlock;
      image1.free;
      imgOriginal.free;

    end;
  finally
    bm.canvas.unlock;
  end;
end;






function GetJpegFromBitMap(bm: Vcl.Graphics.TBitmap): Tjpegimage;
var
  jp: tJpegImage;
begin
  jp := TJpegImage.create;
//  jp.canvas.lock;
  try
    result := jp;
    try
      jp.Assign(bm);
      jp.JPEGNeeded;
    finally
  //    jp.Free;
    end;
  finally
//    jp.canvas.unlock;
  end;
end;


procedure SaveAsJpeg(bm: Vcl.Graphics.TBitmap; sfile: ansistring);
var
  jp: TJpegImage;
begin
  jp := nil;
  try
    jp := GetJpegFrombitmap(bm);
    jp.SaveToFile(sFile);
  finally
    jp.Free;
  end;
end;

procedure ResizeImage(bm: TJpegimage; newwidth: integer; newheight: integer);overload;
var
  bm1: Vcl.Graphics.TBitmap;
  jp:TJpegimage;
begin
//  bm.canvas.lock;
  try
    bm.DIBNeeded;
    bm1 := Vcl.Graphics.TBitmap.create;
    bm1.canvas.lock;
    try
      bm1.Assign(bm);
      ResizeImage(bm1, newwidth, newheight);
      jp := GetJpegFromBitmap(bm1);
      bm.Assign(jp);
      jp.free;
    finally
      bm1.canvas.unlock;
      bm1.free;
    end;
  finally
//    bm.canvas.unlock;
  end;
end;



{ TSmallColor }


{ TFilterInfo }

function TFilterInfo.Avg: TGiantColor;
begin
  result := self.color / self.pixcount;
end;

function DuplicateBitmap(bm: Vcl.Graphics.TBitmap): Vcl.Graphics.TBitmap;
begin
  result := Vcl.Graphics.TBitmap.create;
  bm.Canvas.Lock;
  try
    result.Canvas.Lock;
    try
      result.Assign(bm);
      bm.Canvas.Pixels[0,0] := bm.canvas.Pixels[0,0];
    finally
      result.Canvas.Unlock;
    end;
  finally
    bm.Canvas.Unlock;
  end;

end;

procedure TotalWhiteAndTotalBlack(bm: Vcl.Graphics.TBitmap);
var
  bm2: Vcl.Graphics.TBitmap;
begin
  bm.canvas.lock;
  try


    bm2 := DuplicateBitmap(bm);
    bm2.canvas.lock;
    try
      RemoveColorTotalWhite(bm);
      NegativeImage(bm2);
      RemoveColorTotalWhite(bm2, 1, 0.85, 0.95);
      AddBlend(bm,bm2);

    finally
      bm2.canvas.unlock;
      bm2.free;
    end;
  finally
    bm.canvas.unlock;
  end;
end;

procedure GobbleBlack(bm: Vcl.Graphics.TBitmap);
var
  y,x: integer;
  c, c1,c2,c3,c4: TColor;
begin
  bm.canvas.lock;
  try
    for y := 1 to bm.height - 2 do begin
      for x := 1 to bm.width - 2 do begin
        c := bm.canvas.pixels[x,y];
        c1 := bm.canvas.pixels[x,y+1];
        c2 := bm.canvas.pixels[x+1,y+0];
        c3 := bm.canvas.pixels[x+1,y+1];

        if (c1<>c) or (c2<>c) or (c3<>c) then begin
          bm.canvas.pixels[x,y] := c;
        end;
      end;
    end;
  finally
    bm.canvas.unlock;
  end;

end;

function JpegToBitmap(bm: TJpegImage; bDontDestroy: boolean = false): Vcl.Graphics.TBitmap;
begin
  result := JpegToBitmap_MT(bm, bDontDestroy);
end;
function JpegToBitmap_TS(bm: TJpegImage; bDontDestroy: boolean = false): Vcl.Graphics.TBitmap;
var
  c: Tcmd_JpegToBitmap;
begin
  c := Tcmd_JpegToBitmap.create;
  try
    c.Jpg := bm;
    c.DontDestroy := bDontDestroy;
    c.Start;
    c.WaitFor;
    result := c.bmp;
  finally
    c.free;
  end;

end;

function JpegToBitmap_MT(bm: TJpegImage; bDontDestroy: boolean = false): Vcl.Graphics.TBitmap;
var
  bm1: Vcl.Graphics.TBitmap;
  jp:TJpegimage;
begin

//  bm.Canvas.lock;
  try
    bm.DIBNeeded;
    result := Vcl.Graphics.TBitmap.create;
    result.canvas.lock;
    try
      result.Assign(bm);
    finally
      result.canvas.unlock;
//      bm.Unlock;
      if not bDontdestroy then begin
        bm.free;
      end;
    end;
  finally

  end;
end;

function CountPNGColors(png: TPNGImage; iStopAt: integer = 256): integer;
var
  i: integer;
  a: array of cardinal;
  c: TColor;
  cc: cardinal;
  y,x: integer;
  sl: pointer;
  sla: vcl.imaging.pngimage.PByteArray;
  function HasColor(cc: cardinal): boolean;
  var
    t: integer;
  begin
    result := false;
    for t:= low(a) to high(a) do begin
      if a[t] = cc then begin
        result := true;
        exit;
      end;
    end;
  end;
begin
  png.canvas.lock;
  try
    SetLength(a, 0);

    for y := 0 to png.Height-1 do begin
      sl := png.Scanline[y];
      sla := png.AlphaScanline[y];
      for x := 0 to png.Width-1 do begin
        c := GetColorFromPointer(PByte(sl)+(x*3));
        if sla <> nil then begin
          if sla[x] = 0 then
            cc := sla[x]
          else
            cc := (c shl 8) or sla[x];
        end else begin
          cc := c shl 8 or 255;
        end;

        if not HasColor(cc) then begin
          i := Length(a);
          inc(i);
          SetLength(a, i);
          if i >=iStopAt then begin
            result := iStopat + 1;
            exit;
          end;
          a[High(a)] := cc;
        end;

      end;
    end;

    result := length(a);
  finally
    png.canvas.unlock;
  end;

end;

function GetPNGColors(png: TPNGImage; var colors: array of TColor): integer;
var
  c: TColor;
  cc: Tcolor;
  y,x: integer;
  sl: pointer;
  sla: vcl.imaging.pngimage.PByteArray;
  iCount: integer;
  function HasColor(cc: Tcolor): boolean;
  var
    t: integer;
  begin
    result := false;
    for t:= low(colors) to (low(colors)+iCount)-1 do begin
      if colors[t] = cc then begin
        result := true;
        exit;
      end;
    end;
  end;
begin
//  SetLength(a, 0);
  iCount := 0;
  result := 0;
  try

    for y := 0 to png.Height-1 do begin
      sl := png.Scanline[y];
      sla := png.AlphaScanline[y];
      for x := 0 to png.Width-1 do begin
        c := GetColorFromPointer(PByte(sl)+(x*3));
        if sla <> nil then begin
          if sla[x] = 0 then
            cc := sla[x]
          else
            cc := (c shl 8) or sla[x];
        end else begin
            cc := (c shl 8) or 255;
        end;

        if not HasColor(cc) then begin
  //        SetLength(a, Length(a)+1);
          colors[low(colors)+iCount] := cc;
          inc(iCount);
          if iCount >= 254 then exit;

        end;

      end;
    end;
  finally
    result := iCount;
  end;

end;



procedure AutoQuantizePNG(png: TPNGImage; iColorTarget: integer = 256; bDither: boolean=true; bDitherAlpha: boolean = false; debugger: TImage = nil);
var
  iLevels: integer;
  png2: TPNGImage;
  iLastColors: integer;
  iThisColors: integer;
  iStep: integer;
  bFirst: boolean;
begin
  iLevels := 256;
  png2 := TPNGImage.create;
  bFirst := true;
  try
    png2.Assign(png);

    iLastColors := CountPNGColors(png2);
    iThisColors := iLastColors;
    iStep := iLevels div 2;
    try
      repeat
        repeat
          if not bFirst then
            iLevels := iLevels - iStep;


//          beeper.beep((iLEvels*5)+200, 500);
          png2.Assign(png);
          if iLevels = 0 then break;
          QuantizePNG(png2, iLevels, iLevels, iLevels, 2, bDither, bDitherAlpha);
          if assigned(Debugger) then begin
            Debugger.picture.assign(png2);
            debugger.refresh;
          end;

          iThisColors := CountPNGColors(png2);
          if bFirst and (iThisColors < iColorTarget) then
            exit;
          bFirst := false;

          iLAstColors := iThisColors;
        until  iThisColors < iColorTarget;

        iStep := iStep div 2;

        if iStep < 1 then begin
          iLevels := iLevels -1;
          png2.Assign(png);
          QuantizePNG(png2, iLevels, iLevels, iLevels, 2, bDither, bDitherAlpha);
          if assigned(Debugger) then begin
            Debugger.picture.assign(png2);
            debugger.refresh;
          end;

        end
        else repeat
          iLevels := iLevels + iStep;
//          beeper.beep((iLEvels*5)+200, 500);
          png2.Assign(png);
          QuantizePNG(png2, iLevels, iLevels, iLevels, 2, bDither, bDitherAlpha);
          if assigned(Debugger) then begin
            Debugger.picture.assign(png2);
            debugger.refresh;
          end;

          iThisColors := CountPNGColors(png2)
        until  iThisColors > iColorTarget;

        iStep := iStep div 2;


//        if iThisColors = iLastColors then
//          break;


      until iStep = 0;
    finally
    png.assign(png2);
    end;





  finally
    png2.free;
  end;


end;

procedure QuantizeFB(fb: TFastBitmap; rLevels, gLevels, bLevels, aLevels: integer; bDither: boolean=true; bDitherAlpha: boolean = false);overload;
var
  x,y: integer;
  c: TColor;
  cc: TGiantColor;
  ccRound, ccTrunc: TGiantColor;
  rmod,gmod,bmod: integer;
const
  PNG_REC_SIZE = 3;

begin

  for y:= 0 to fb.Height-1 do begin
    for x := 0 to fb.Width-1 do begin
      c := fb.Canvas.Pixels[x,y];
      cc := easyimage.ColorToGiantColor(c);


      //check pixel
      ccRound := cc;
      //multiply
      ccRound.r := DitherQuantize(x,y, ccRound.r, rLevels);
      ccRound.g := DitherQuantize(x,y, ccRound.g, gLevels);
      ccRound.b := DitherQuantize(x,y, ccRound.b, bLevels);
      if bDitherAlpha then begin
        ccRound.a := DitherQuantize(x,y, ccRound.a, aLevels);
      end else begin
        ccRound.a := round(ccRound.a);
      end;


      fb.Canvas.Pixels[x,y] := ccRound;


    end;
  end;
end;

procedure QuantizePNG(png: TPNGImage; rLevels, gLevels, bLevels, aLevels: integer; bDither: boolean=true; bDitherAlpha: boolean = false);
var
  x,y: integer;
  c: TColor;
  cc: TGiantColor;
  ccRound, ccTrunc: TGiantColor;
  sl: pointer;
  sla: vcl.imaging.pngimage.PByteArray;
  rmod,gmod,bmod: integer;
const
  PNG_REC_SIZE = 3;

begin

  for y:= 0 to png.Height-1 do begin
    sl := png.Scanline[y];
    sla := png.AlphaScanLine[y];
    for x := 0 to png.Width-1 do begin
      c := GetColorFromPOinter(PByte(sl)+(x*PNG_REC_SIZE));
      cc := easyimage.ColorToGiantColor(c);
      if sla = nil then
        cc.A := 1
      else
        cc.A := sla[x] / 255;

      //check pixel
      ccRound := cc;
      //multiply
      ccRound.r := DitherQuantize(x,y, ccRound.r, rLevels);
      ccRound.g := DitherQuantize(x,y, ccRound.g, gLevels);
      ccRound.b := DitherQuantize(x,y, ccRound.b, bLevels);
      if bDitherAlpha then begin
        ccRound.a := DitherQuantize(x,y, ccRound.a, aLevels);
      end else begin
        ccRound.a := round(ccRound.a);
      end;


      SetGiantColorToPOinter(ccRound, PByte(sl)+(x*PNG_REC_SIZE));
      if sla <> nil  then
        sla[x] := round(ccRound.a * 255);



    end;
  end;

end;

function DitherQuantizeHAlftone(x,y: integer; rColorLevel: nativefloat; iLevels: integer): nativefloat;
var
  iGridPos: integer;
  iSubLevel: integer;
  iInflatedColorValue: integer;
const
  SUBLEVELS = 8;
  GRIDWID = 12;
begin
  iInflatedColorValue := round(rColorLevel * ((iLevels-1)*SUBLEVELS));

  iGridPos := (x mod GRIDWID)+((y mod GRIDWID) * GRIDWID);

  iSubLEvel := iInflatedColorValue mod SUBLEVELS;

(*  if (iSubLevel mod 2) = 1 then begin
    if iGridPos in [0,1,4,5,14,15,10,11] then
      iSubLevel := iSubLevel -1
    else
      iSubLevel := iSubLevel +1;
  end;*)


  result := 0;
  case iSubLevel of
    0: begin
      result := (iInflatedColorValue div SUBLEVELS)+0;
    end;
    1: begin
      if Pattern1[iGridPos] = 1 then begin
        result := (iInflatedColorValue div SUBLEVELS)+1
      end else begin
        result := (iInflatedColorValue div SUBLEVELS)+0;
      end;
    end;
    2: begin
      if Pattern2[iGridPos] = 1 then begin
        result := (iInflatedColorValue div SUBLEVELS)+1
      end else begin
        result := (iInflatedColorValue div SUBLEVELS)+0;
      end;

    end;
    3: begin
      if Pattern3[iGridPos] = 1 then begin
        result := (iInflatedColorValue div SUBLEVELS)+1
      end else begin
        result := (iInflatedColorValue div SUBLEVELS)+0;
      end;

    end;
    4: begin
      if ((x+y) mod 2) = 0 then
        result := (iInflatedColorValue div SUBLEVELS)+0
      else
        result := (iInflatedColorValue div SUBLEVELS)+1;
    end;
    5: begin
      if Pattern3[iGridPos] = 0 then begin
        result := (iInflatedColorValue div SUBLEVELS)+1
      end else begin
        result := (iInflatedColorValue div SUBLEVELS)+0;
      end;

    end;
    6: begin
      if Pattern2[iGridPos] = 0 then begin
        result := (iInflatedColorValue div SUBLEVELS)+1
      end else begin
        result := (iInflatedColorValue div SUBLEVELS)+0;
      end;
    end;
    7: begin
      if Pattern1[iGridPos] = 0 then begin
        result := (iInflatedColorValue div SUBLEVELS)+1
      end else begin
        result := (iInflatedColorValue div SUBLEVELS)+0;
      end;
    end;
    8: begin
      result := (iInflatedColorValue div SUBLEVELS)+1;
    end;
  end;


  result := result / (iLevels-1);

end;

function DitherQuantize(x,y: integer; rColorLevel: nativefloat; iLevels: integer): nativefloat;
var
  iGridPos: integer;
  iSubLevel: integer;
  rInflatedColorValue: nativefloat;
  r,f: nativefloat;
begin
//  if iLevels <= 2 then begin
    result := DitherQuantizeHalftone(x,y,rColorLevel, iLevels);
    exit;
//  end;
  rInflatedColorValue := rColorLevel * (iLevels-1);

//  iGridPos := (x mod 4)+((y mod 4) * 4);


  r := random(1000)/1000;
  f := rInflatedColorValue-trunc(rInflatedColorValue);

  result := trunc(rInflatedColorValue);
  if r < f then
    result := result +1;


  result := result / (iLevels-1);

end;

function GetAlphaFromPointer(p: pointer): integer;inline;
begin
  result := ord(PByte(p)[0]) div 255;
end;
procedure SetAlphaToPointer(p: pointer; a: integer);inline;
begin
  PByte(p)[0] := byte(trunc(a * 255));
end;

procedure SetGiantColorToPointer(gc: TGiantColor; pColor: pointer; pAlpha: pointer=nil);
var
  sc: TColor;
begin
  sc := GiantColorToColor(gc); //implicit type cast
  SetColorToPOinter(pColor, sc);

  if pAlpha<> nil then
    SetAlphaToPointer(pAlpha, round(gc.a*255));





end;

function ConvertPNGToGIF(pngNotOwned: TPNGImage): TGIFImage;
var
  png2: TPNGImage;
begin
  applock.al.lock;
  try

    png2 := TPNGImage.create;
    try
      result := TGIfImage.create;
      try

        result.Transparent := true;
        result.ColorReduction := rmWindows256;

        png2.Assign(pngNotowned);
        EasyImage.AutoQuantizePNG(png2, 220, true, true);
        result.Assign(png2);

      except

        result.Free;
        result := nil;
        raise;
      end;
    finally
      png2.free;
    end;
  finally
    al.unlock;
  end;
end;

function ConvertPNGToGIF(sPNGFile: ansistring; sGIFFile: ansistring = ''): boolean;overload;
var
  png: TPNGImage;
  gif: TGIFImage;
begin
  result := true;
  applock.al.lock;
  try

    gif := nil;
    png := TPNGImage.create;
    png.Canvas.Lock;
    try
      png.LoadFromFile(sPNGFile);
      if sGIFFile = '' then begin
        sGIFFile := ChangeFileExt(sPNGFile, '.gif');
      end;
      gif := ConvertPNGToGif(png);
      gif.SaveToFile(sGIFFile);


    finally
      png.Canvas.Unlock;
      png.free;
      gif.free;
    end;
  finally
    applock.AL.Unlock;
  end;
end;



{ TGIFConversionQueue }

procedure TGIFConversionQueue.AddToQueue(sFile: ansistring);
begin
  LockWrite;
  try
    Debug.Log(self,'GIFQUEUE:'+sFile+' added to queue', 'GIFQueue');
    FQueue.Add(sFile);
  finally
    UnlockWrite;
  end;
end;

constructor TGIFConversionQueue.create;
begin
  inherited;
  FQueue := TStringlist.create;
  FQueue.CaseSensitive := false;
  FQueue.Duplicates := dupIgnore;
end;

destructor TGIFConversionQueue.Destroy;
begin
  FQueue.free;
  inherited;
end;

function TGIFConversionQueue.GetFileFromQueue: ansistring;
begin
  LockRead;
  try
    if Fqueue.Count > 0 then
      result := FQueue[0];
  finally
    UnlockRead;
  end;
end;

function TGIFConversionQueue.IsStillInQueue(sFile: ansistring): boolean;
begin
  LockRead;
  try
    result := FQueue.IndexOf(sFile) >=0;
  finally
    UnlockRead;
  end;

end;

procedure TGIFConversionQueue.ProcessAll;
begin
  while processSingle do ;
end;

function TGIFConversionQueue.processSingle: boolean;
var
  sFile: ansistring;
begin
  sFile := self.GetFileFromQueue;

  if sFile = '' then begin
    result := false;
    exit;
  end;

  try
    ConvertPNGToGIF(sFile);
  finally
    RemoveFromQueue(sFile);
  end;

  result := true;


end;

procedure TGIFConversionQueue.RemoveFromQueue(sFile: ansistring);
begin
  LockWrite;
  try
    FQueue.Delete(0);
  finally
    UnlockWrite;
  end;
end;




function FocusPNG(png: TPNGImage; xFocus, yFocus: nativefloat): boolean;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;


function CopyCreateBitmap(bmsource: TGraphic): Vcl.Graphics.TBitmap;
begin
  result := Vcl.Graphics.TBitmap.create;
//  bmsource.canvas.lock;
  try
    result.canvas.lock;
    try
      result.width := bmSource.width;
      result.height := bmSource.height;
      result.canvas.draw(0,0,bmSource);
    finally
      result.canvas.unlock;
    end;
  finally
//    bmSource.canvas.unlock;
  end;

end;

{ TGiantColorMatrix }

procedure TGiantColorMatrix.AddPoint(x, y: nativefloat; c: TGiantColor);
var
  c1,c2,c3,c4: TGiantColor;
begin
{$IFDEF AAPOINTS}
  c1 := colorblend(c, colortogiantcolor(clBlack), (x-trunc(x)) * (y-trunc(y)));
  c2 := colorblend(c, colortogiantcolor(clBlack), (1-(x-trunc(x))) * (y-trunc(y)));
  c3 := colorblend(c, colortogiantcolor(clBlack), ((x-trunc(x))) * (1-(y-trunc(y))));
  c4 := colorblend(c, colortogiantcolor(clBlack), (1-(x-trunc(x))) * (1-(y-trunc(y))));

  pixels[trunc(x), trunc(y)] := pixels[trunc(x), trunc(y)] + c1;
  pixels[trunc(x)+1, trunc(y)] := pixels[trunc(x), trunc(y)] + c2;
  pixels[trunc(x), trunc(y)+1] := pixels[trunc(x), trunc(y)] + c3;
  pixels[trunc(x)+1, trunc(y)+1] := pixels[trunc(x), trunc(y)] + c4;
{$ELSE}
  pixels[round(x), round(y)] := pixels[round(x), round(y)] + c;
{$ENDIF}


end;

procedure TGiantColorMatrix.Clear(color: TGiantColor);
var
  x,y: integer;
begin
  for y := 0 to h-1 do begin
    for x := 0 to w-1 do begin
      pixels[x,y] := color;
    end;
  end;

end;


function TGiantColorMatrix.GetPixels(x, y: integer): TGiantColor;
begin
  if (y > (H-1)) or (x > (W-1)) then begin
    result := ColorToGiantColor(clBlack);
  end else
    result := canvas[y][x];
end;

procedure TGiantColorMatrix.init(w,h: integer);
var
  t: integer;
begin
  setlength(canvas, h);
  for t:= 0 to h-1 do begin
    setlength(canvas[t],w);
  end;
  self.w := w;
  self.h := h;
end;

procedure TGiantColorMatrix.RetrieveBitmap(b: TPicture);
var
  bb: Vcl.Graphics.TBitmap;
  x,y: integer;
begin
  bb := CreateBlankBitmap(w,h);
  bb.canvas.lock;
  try
    for y := 0 to h-1 do begin
      for x := 0 to w-1 do begin
        bb.canvas.pixels[x,y] := pixels[x,y].ToColor;
      end;
    end;
    b.assign(bb);
  finally
    bb.canvas.unlock;
//    bb.free;
  end;

end;

procedure TGiantColorMatrix.RetrieveBitmap(b: Vcl.Graphics.TBitmap);
var
  bb: Vcl.Graphics.TBitmap;
  x,y: integer;
begin
  bb := CreateBlankBitmap(w,h);
  bb.canvas.lock;
  try
    for y := 0 to h-1 do begin
      for x := 0 to w-1 do begin
        bb.canvas.pixels[x,y] := pixels[x,y].ToColor;
      end;
    end;
    b.assign(bb);
  finally
    bb.canvas.unlock;
    bb.free;
  end;
end;

procedure TGiantColorMatrix.SetPixels(x, y: integer; const Value: TGiantColor);
begin
  if (y > (H-1)) or (x > (W-1)) then begin
    exit;
  end;
  canvas[y][x] := value;
end;

function CreateBlankBitmap(w,h: integer; pixel_format: TPixelformat = pf24bit): Vcl.Graphics.TBitmap;
begin
  result := Vcl.Graphics.TBitmap.create;
  result.canvas.lock;
  try
    result.width := w;
    result.height := h;
    result.pixelformat := pixel_format;
    result.canvas.rectangle(0,0,result.width,result.height);
  finally
    result.canvas.unlock;
  end;
end;

procedure LockCanvasIfNeeded(canvas: TCanvas);
begin
  canvas.lock;
end;
procedure UnlockCanvasIfNeeded(canvas: Tcanvas);
begin
  canvas.unlock;
end;








function PNGtoGPBitmap(self: TPNGImage): TGPBitmap;
var
  rr: TGPRect;
  pt: TGPPointF;
  bi: tagBitMapInfo;
  s: TMemoryStream;
  png: TPNGIMage;
  t,u: integer;
  hbm: HBITMAP;
  sl1: pbyte;
  sl2 : vcl.imaging.pngimage.PbyteArray;
  r,g,b,a: byte;
  p: pbyte;
  graphics: TGPGraphics;
  gpib: TGPBitmap;
begin
  png := self;
  gpib := GDIPOBJ.TGPBitmap.Create(png.Width, png.height, PixelFormat32bppARGB);
  result := gpib;
  for u := 0 to png.height-1 do begin
    sl1 := png.Scanline[u];
    sl2 := png.alphascanline[u];
    for t:= 0 to png.width-1 do begin
      p := sl1+(t*3);
      r := ord(p[0]);
      g := ord(p[1]);
      b := ord(p[2]);
      if sl2 = nil then
        a := 255
      else
        a := ord(sl2[t]);
      gpib.SetPixel(t,u, (a shl 24)+(b shl 16)+(g shl 8)+r);

     end;
  end;

end;

function PointerToColor(x_fromcorner,y_fromcorner,w,h: nativeint; p_baseofbitmap: PByte; iBitsPerPixel: nativeint): TColor;
var
  p: PByte;
  x,y: nativeint;
  i: smallint;
begin
  x := x_fromcorner;
  y := y_fromcorner;
  if p_baseofbitmap = nil then begin
    result := round(((x / w) * 255))+round(((y / h) * 255)) shl 16;
    exit;
  end;

  case iBitsPerPixel of
    32: begin
        p := p_baseofbitmap+((y*(w* 4))+(x*4));
        result := p[2]+(p[1] shl 8)+(p[0] shl 16);
    end;
    24: begin
        p := p_baseofbitmap+((y*(w*3))+(x*3));
        result := p[2]+(p[1] shl 8)+(p[0] shl 16);
    end;
    16: begin
        p := p_baseofbitmap+((y*(w*2))+(x*2));
        i := p[0] + (p[1] shl 8);
        result := (i and 31)+ ((i shr 5) and 63)+ ((i shl 11) and 31);
    end;
    8: begin
        p := p_baseofbitmap+((y*(w*1))+(x*1));
        i := p[0];
        result := (i and 7)+ ((i shr 3) and 7)+ ((i shl 6) and 3);
    end;
    1: begin
        p := p_baseofbitmap+((y*(w div 8))+(x div 8));
        i := (p[0] shr (x mod 8)) and 1;
        if i > 0 then
          result := clWhite
        else

          result := clBlack;

    end;





  else
    raise Exception.Create('unimplemented bits depth - '+inttostr(iBitsPerPixel));
  end;


end;





{ Tcmd_GetPuzzleError }


function Minimum(ary: array of TFastBitmap): TFastBitmap;
//Todo 5: This function only supports bitmaps of the same size and sets the w/h of the result to index [0] ... could be smarter.
var
  x,y,z: nativeint;
  hMin,h: THSLNativefloatColor;
begin
  result := TFastBitmap.Create;
  result.width := ary[0].width;
  result.height := ary[0].height;
  result.new;
  result.canvas.Clear(clBlack);


  for y := 0 to result.Height-1 do begin
    for x := 0 to result.Width-1 do begin
      hMin.FromColor(ary[0].Canvas.pixels[x,y]);
      for z:= 1 to length(ary)-1 do begin
        h.FromColor(ary[z].Canvas.pixels[x,y]);
        if h.l < hMin.l then begin
          hMin := h;
        end;
      end;
      result.Canvas.Pixels[x,y] := hMin.ToColor;
    end;
  end;
end;



{ Tcmd_JpegToBitmap }

constructor Tcmd_JpegToBitmap.Create;
begin
  inherited;
  Icon := @CMD_ICON_CRUNCH;
end;

procedure Tcmd_JpegToBitmap.DoExecute;
begin
  inherited;
  self.Thread.Synchronize(nil, SyncEx);
end;

procedure Tcmd_JpegToBitmap.Init;
begin


  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure Tcmd_JpegToBitmap.SyncEx;
begin
  bmp := JpegToBitmap_MT(jpg, DontDestroy);
end;

function RBSwap(c: TColor): TColor;
var
  b: byte;
begin
  result := c;
  b := pbyte(@result)[0];
  pbyte(@result)[0] := pbyte(@result)[2];
  pbyte(@result)[2] := b;

end;


procedure REsizeImages(sInputDir, sFileSpec: string; w,h: nativeint);
var
  t: nativeint;
  dir: TDirectory;
  fil: TFileInformation;
  j: TJPegImage;
begin
  dir := TDirectory.Create(sINputDir, sFileSpec, 0,0, true, false);
  try
    while dir.GetNextFile(fil) do begin
      j := TJpegImage.Create;
      try
        j.LoadFromFile(fil.fullname);
        ResizeImage(j, w,h);
        j.SaveToFile(fil.fullname);
      finally
        j.Free;
      end;
      end;
  finally
    dir.Free;
  end;

end;



procedure oinit;
begin
  GifQueue := TGifConversionQueue.create;
  MainThreadID := GetCurrentThreadID();
  FC := TFontCache.Create;

end;

procedure ofinal;
begin
  GifQueue.free;
  FC.free;


end;

function CanvasResample(canvas: TCanvas; x,y: nativefloat): TColor;
var
  c1,c2,c3,c4: TColor;
begin
  c1 := canvas.Pixels[trunc(x)+0,trunc(y)+0];
  c2 := canvas.Pixels[trunc(x)+1,trunc(y)+0];
  c3 := canvas.Pixels[trunc(x)+0,trunc(y)+1];
  c4 := canvas.Pixels[trunc(x)+1,trunc(y)+1];

  c1 := colorblend(c1,c2, (x-trunc(x)/1));
  c3 := colorblend(c3,c4, (x-trunc(x)/1));
  result := colorblend(c1,c3, (y-trunc(y)/1));



end;

function CanvasResample(canvas: TFastCanvas; x,y: nativefloat): TColor;
var
  c1,c2,c3,c4: TColor;
begin
  c1 := canvas.Pixels[trunc(x)+0,trunc(y)+0];
  c2 := canvas.Pixels[trunc(x)+1,trunc(y)+0];
  c3 := canvas.Pixels[trunc(x)+0,trunc(y)+1];
  c4 := canvas.Pixels[trunc(x)+1,trunc(y)+1];

  c1 := colorblend(c1,c2, (x-trunc(x)/1));
  c3 := colorblend(c3,c4, (x-trunc(x)/1));
  result := colorblend(c1,c3, (y-trunc(y)/1));



end;


procedure PNGtoRaw(sPNG, sRaw: string);
begin
  Tcmd_PNgToRaw.create;

end;
{ Tcmd_PngToRAw }

constructor Tcmd_PngToRAw.create;
begin
  inherited;
  Icon := @CMD_ICON_CRUNCH;
end;

procedure Tcmd_PngToRAw.DoExecute;
var
  p: TPNGImage;
  r: TMultibufferMemoryFileStream;
  t,u: nativeint;
  c: TColor;
  pline: array of cardinal;
begin
  inherited;
  p := nil;
  try
    p := TPNGImage.create;
    p.LoadFromFile(Png);
    r := nil;
    try
      r := TMultibuffermemoryfilestream.create(Raw, fmCReate);
      stepcount := p.Height-1;
      setlength(pline, p.width);
      for u := 0 to p.Height-1 do begin
        step := u;
        for t:= 0 to p.Width-1 do begin
          c := p.Canvas.pixels[t,u];
          pline[t] := c;
        end;
        Stream_GuaranteeWrite(r, @pline[0], length(pline)*sizeof(c));
      end;
    finally
      r.free;
    end;



  finally
    p.free;
  end;


end;






procedure Tcmd_PngToRAw.InitExpense;
begin
  inherited;
  memoryexpense := 9/10;

end;

function GEtIMageFileDimensions_JPG(sFile: string; out w, h: nativeint): boolean;
var
  jpg: TJPEGIMage;
begin
  result := true;
  jpg := TJpegImage.create;
  try
    jpg.LoadFromFile(sFile);
    w := jpg.width;
    h := jpg.height;
  finally
    jpg.free;
  end;

end;
function GEtIMageFileDimensions(sFile: string; out w, h: nativeint): boolean;
begin
  result := false;
  if lowercase(extractfileext(sFile)) = '.jpg' then begin
    result := GetImageFileDImensions_JPG(sFile, w,h);
  end;
end;


initialization
  init.RegisterProcs('EasyImage', oinit, ofinal);

finalization



end.
