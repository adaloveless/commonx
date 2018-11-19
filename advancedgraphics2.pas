unit advancedgraphics2;
{$MESSAGE '*******************COMPILING advancedgraphics.pas'}
{$INLINE auto}
{x$DEFINE USE_SSE3}

interface

uses
  system.rtlconsts,system.types, system.uitypes, geometry,
  winapi.windows, Vcl.Graphics, winapi.messages, controls, classes, dialogs, sysutils,
  extctrls, math, easyimage, colorconversion, fastbitmap, colorblending,
  glasscontrols, systemx, stringx, numbers, typex;

const
  TEXT_WIDTH_INFINITE = -1;

type
  advancedfloat = double;
  TAlphaOp = (aoNone, aoSubtract, aoAdd, aoStandard, aoStandardVariable);

  TadvancedFloatMouseEvent = procedure(x, y: advancedFloat; iButton: integer;
    bDown: boolean) of object;
  TTextJustify = (tjLeft, tjRight, tjCenter);
  TTextWeight = (tsnormal, tsbold);
  TTextKnockout = (tkoNone, tkoBackGround, tkoReverseTransparent,
    tkoReverseBackGround);
  TTextRotate = (trHorizontal, trVertical);

  TLineDecor = set of (ldOpenDiamond, ldClosedDiamond, ldOpenArrow,
    ldClosedArrow, ldDot);

// ------------------------------------------------------------------------------
  TDrawingBoard = class(TFastBitmap)
  private
    FBoundY2: advancedFloat;
    FBoundX1: advancedFloat;
    FBoundY1: advancedFloat;
    FBoundX2: advancedFloat;
    FOnPaint: TNotifyEvent;
    FScale: advancedFloat;
    FConstrainProportions: boolean;
    FLineColor, FFillColor: TColor;
    FUseScreenCoordinates: boolean;
    FLeftMargin: advancedFloat;
    FrightMargin: advancedFloat;
    FBottomMargin: advancedFloat;
    FTopMargin: advancedFloat;
    FDEbug: boolean;
    FCachedScanLine: array of byte;
    FCachedScanLineY: integer;
    FTextRotate: TTextRotate;
    FTextJustify: TTextJustify;
    FTextHeight: advancedFloat;
    FTextColor: TColor;
    FNewTextAlginRules: boolean;
    FNewTextAlignRules: boolean;
    FTextBackgroundColor: TColor;
    FTextHelperBitmap: TBitmap;
    FTextKnockout: TTextKnockout;
    FAutoTruncateText: boolean;
    FScreenMaskX2: advancedFloat;
    FScreenMaskY2: advancedFloat;
    FScreenMaskX1: advancedFloat;
    FScreenMaskY1: advancedFloat;
    FMouseX: integer;
    FMouseY: integer;
    FOnadvancedFloatMouse: TadvancedFloatMouseEvent;
    FAlphaOp: TAlphaOp;
    FAntialiasing: integer;
    FWidth: ni;
    FHeight: ni;
    function GetCenterX: advancedFloat;
    function GetCenterY: advancedFloat;

    function GetFillColor: TColor;
    function GetLineColor: TColor;
    procedure SetFillColor(const Value: TColor);
    procedure SetLineColor(const Value: TColor);
    procedure SetBoundX1(const Value: advancedFloat);
    procedure SetBoundX2(const Value: advancedFloat);
    procedure SetBoundY1(const Value: advancedFloat);
    procedure SetBoundY2(const Value: advancedFloat);
    procedure SetDimensionX(const Value: advancedFloat);
    procedure SetDimensionY(const Value: advancedFloat);
    procedure SetScale(const Value: advancedFloat);
    procedure RecalcScale;
    procedure SetConstrainProportions(const Value: boolean);
    function GetDimensionX: advancedFloat;
    function GetDimensiONY: advancedFloat;
    procedure OldFatLine(gx1, gy1, gx2, gy2, width: advancedFloat; color: TColor;
      bfirstSegment: boolean);
    function GetBottomMargin: advancedFloat;
    function GetLeftMargin: advancedFloat;
    function GetrightMargin: advancedFloat;
    function GetTopMargin: advancedFloat;
    procedure SetBottomMargin(const Value: advancedFloat);
    procedure SetLeftMargin(const Value: advancedFloat);
    procedure SetrightMargin(const Value: advancedFloat);
    procedure SetTopMargin(const Value: advancedFloat);

    function CanvasToScreenY(canvasY: advancedFloat): advancedFloat;
    function GetTextColor: TColor;
    procedure SetTextcolor(const Value: TColor);
    procedure SetTextBackgroundcolor(const Value: TColor);
    function GetDrawTo_pixels(x, y: integer): TColor;
    procedure SetDrawTo_Pixels(x, y: integer; const Value: TColor);
    procedure SetScreenMask(x1, y1, x2, y2: advancedFloat);
    function GetTextHelper: TCanvas;

  protected
    procedure DesignDraw;
    function ScaleGlobalXtoScreen(Xdistance: advancedFloat): advancedFloat;
    function ScaleGlobalYtoScreen(Ydistance: advancedFloat): advancedFloat;
    function ScaleScreenXtoGlobal(Xdistance: advancedFloat; bKeepSign: boolean = false): advancedFloat;
    function ScaleScreenYtoGlobal(Ydistance: advancedFloat; bKeepSign: boolean = false): advancedFloat;
    function ScaleScreenToCanvas(x: advancedFloat): integer;
    function ScaleCanvasToScreen(x: integer): advancedFloat;
    procedure FindFontSize(width, height: integer; sText: ansistring);

  public
    procedure Crosshair(gx1, gy1, sz: advancedFloat; color: TColor);
    property NewTextAlignRules
      : boolean read FNewTextAlignRules write FNewTextAlignRules;
    procedure InvertRect(canvasX1, canvasY1, canvasX2, canvasY2: integer);
    function CanvasToScreenX(canvasX: advancedFloat): advancedFloat;
    function ScreenToCanvasX(screenX: advancedFloat): advancedFloat;
    function ScreenToCanvasY(screenY: advancedFloat): advancedFloat;

    function GlobalToCanvasX(globalX: advancedFloat): advancedFloat;
    function GlobalToCanvasY(globalY: advancedFloat): advancedFloat;

    function CanvasToGlobalX(canvasX: advancedFloat): advancedFloat;
    function CanvasToGlobalY(canvasY: advancedFloat): advancedFloat;

    function GlobalToScreenX(globalX: advancedFloat): advancedFloat;
    function GlobalToScreenY(globalY: advancedFloat): advancedFloat;
    function ScreenToGlobalX(screenX: advancedFloat): advancedFloat;
    function ScreenToGlobalY(screenY: advancedFloat): advancedFloat;
    function GlobalToCanvasXi(globalX: advancedFloat): integer;
    function GlobalToCanvasYi(globalY: advancedFloat): integer;

    constructor Create; override;
    destructor Destroy; override;
    property LineColor: TColor read GetLineColor write SetLineColor;
    property FillColor: TColor read GetFillColor write SetFillColor;


    procedure FatPoint(x, y: advancedFloat; cInner, cOuter: TColor; size: integer = 3);
    procedure FancyRectangle(x1, y1, x2, y2: advancedFloat; color: TColor);

    procedure SetPoint(x1, y1: advancedFloat; color: TColor); virtual;
    function GetFastPoint(x, y: integer): TColor;
    procedure SetFastPoint(x, y: integer; color: TColor);
    procedure SetLowResPoint(gx, gy: advancedFloat; col: TColor);
    procedure LowResLine(gx1, gy1, gx2, gy2: advancedFloat; color: TColor;
      bDotted: boolean);
    procedure FatLine(gx1, gy1, gx2, gy2: advancedFloat; width: advancedFloat; color: TColor;
      bScreenWidth: boolean); virtual;
    procedure FatBox(gx1, gy1, gx2, gy2, width: advancedFloat; color: TColor;
      bScreenWidth: boolean);
    procedure Line(gx1, gy1, gx2, gy2: advancedFloat; color: TColor); virtual;
    procedure Tri(gx1, gy1, gx2, gy2, gx3, gy3: advancedFloat; color: TColor); virtual;
    procedure Slice(centerX, centerY, gx1, gy1, gx2, gy2: advancedFloat; color: TColor);
      virtual;
    procedure Quad(gx1, gy1, gx2, gy2, gx3, gy3, gx4, gy4: advancedFloat;
      color: TColor); virtual;
    procedure Rectangle(x1, y1, x2, y2: advancedFloat; color: TColor;
      Fill: boolean = false; bGradient: boolean = false;
      bgcolor: TColor = clBlack); overload;
      virtual;
// procedure Rectangle(x1,y1,x2,y2: integer; color: TColor; Fill: boolean = false); overload; virtual;
    procedure Circle(x1, y1, r: advancedFloat; color: TColor;
      Fill: boolean = false); virtual;
    procedure FatCircle(x1, y1: advancedFloat; radius: advancedFloat; width: advancedFloat; color: TColor;
      FillColor: TColor = clBlack; Fill: boolean = false); virtual;
    procedure FatPartialCircle(globalX, globalY, global_radius: advancedFloat;
      rStart, rEnd: advancedFloat; width: advancedFloat; color, FillColor: TColor;
      Fill: boolean);
    procedure Triangle(x1, y1, x2, y2, x3, y3: advancedFloat; color: TColor;
      FillColor: TColor = clBlack; bFill: boolean = false);

// property TextAlign: TTextAlign read FTextAlign write FTextAlign;
    property TextJustify: TTextJustify read FTextJustify write FTextJustify;
    property TextRotate: TTextRotate read FTextRotate write FTextRotate;
    property TextHeight: advancedFloat read FTextHeight write FTextHeight;
    procedure Text(x, y: advancedFloat; sText: ansistring); overload;
    procedure Text(globalX, globalY, width, height: advancedFloat; sText: ansistring;
      bCenter: boolean = false; bVertical: boolean = false); overload;
    procedure Text(globalX, globalY, width, height: advancedFloat; sText: ansistring;
      just: TTextJustify = tjLeft; rot: TTextRotate = trHorizontal); overload;
    property TextColor: TColor read GetTextColor write SetTextcolor;
    property TextKnockOut: TTextKnockout read FTextKnockout write FTextKnockout;
    property TextBackgroundColor: TColor read FTextBackgroundColor write
      SetTextBackgroundcolor;
    property AutoTruncateText
      : boolean read FAutoTruncateText write FAutoTruncateText;
    function TruncateText(width: integer; sText: ansistring): ansistring;

    procedure DoDraw; virtual;
    property UseScreenCoordinates
      : boolean read FUseScreenCoordinates write FUseScreenCoordinates;
    procedure Clear(color: TColor = clBlack);
    property DrawTo_pixels[x: integer;
    y: integer]
      : TColor read GetDrawTo_pixels write SetDrawTo_Pixels;
    procedure DrawTo_SetPixelEx(x, y: integer; color: TColor;
      AlphaOp: TAlphaOp);
  published
    // ------
    property BoundX1: advancedFloat read FBoundX1 write SetBoundX1;
    property BoundX2: advancedFloat read FBoundX2 write SetBoundX2;
    property BoundY1: advancedFloat read FBoundY1 write SetBoundY1;
    property BoundY2: advancedFloat read FBoundY2 write SetBoundY2;
    property centerX: advancedFloat read GetCenterX;
    property centerY: advancedFloat read GetCenterY;
    property DimensionX: advancedFloat read GetDimensionX write SetDimensionX;
    property DimensionY: advancedFloat read GetDimensiONY write SetDimensionY;
    property Scale: advancedFloat read FScale write SetScale;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property ConstrainProportions: boolean read FConstrainProportions write
      SetConstrainProportions;

    property TopMargin: advancedFloat read GetTopMargin write SetTopMargin;
    property LeftMargin: advancedFloat read GetLeftMargin write SetLeftMargin;
    property RightMargin: advancedFloat read GetrightMargin write SetrightMargin;
    property BottomMargin: advancedFloat read GetBottomMargin write SetBottomMargin;
    property Debug: boolean read FDEbug write FDEbug;

    property screenmaskX1: advancedFloat read FScreenMaskX1 write FScreenMaskX1;
    property screenmaskX2: advancedFloat read FScreenMaskX2 write FScreenMaskX2;
    property screenmaskY1: advancedFloat read FScreenMaskY1 write FScreenMaskY1;
    property screenmaskY2: advancedFloat read FScreenMaskY2 write FScreenMaskY2;
    procedure ClearBoundsRect;
    procedure MSG_advancedFloatMouseLDOWN(var msg: TMessage); message WM_LBUTTONDOWN;
    procedure MSG_advancedFloatMouseRDOWN(var msg: TMessage); message WM_RBUTTONDOWN;
    procedure MSG_advancedFloatMouseLUP(var msg: TMessage); message WM_LBUTTONUP;
    procedure MSG_advancedFloatMouseRUP(var msg: TMessage); message WM_RBUTTONUP;
    procedure MSG_advancedFloatMouseMove(var msg: TMessage); message WM_MOUSEMOVE;
    procedure SetLastMouseposition(x, y: integer);
    property OnadvancedFloatMouse: TadvancedFloatMouseEvent read FOnadvancedFloatMouse write FOnadvancedFloatMouse;
    property AlphaOp: TAlphaOp read FAlphaOp write FAlphaOp;
    procedure DrawLineTest;
    property Antialiasing: integer read FAntialiasing;
    property FloatWidth: ni read FWidth write FWidth;
    property FloatHeight: ni read FHeight write Fheight;
    property TextHelper: TCanvas read GetTextHelper;
    procedure Draw;
  end;

// ------------------------------------------------------------------------------

  TSlopeResult = record
    slope: advancedFloat;
    angle: advancedFloat;
    undefined: boolean;
  end;

function GetPointDistanceSq(x1, y1, x2, y2: advancedFloat): advancedFloat;
function GetPointDistance(x1, y1, x2, y2: advancedFloat): advancedFloat;
function GetLineAngle(x1, y1, x2, y2: advancedFloat): advancedFloat;
function GetAnglePoint(ox, oy, distance, angleInRad: advancedFloat): TnativeFloatPOint;
function ColorBlend(cBackGround, cForeGround: TGiantcolor;
  alpha: advancedFloat): TGiantcolor; overload;
function ColorAdd(cBackGround, cForeGround: TColor; alpha: advancedFloat): TColor;
  overload;
function ColorSubtract(cBackGround, cForeGround: TColor;
  alpha: advancedFloat): TColor; overload;
function ColorBlend(cBackGround, cForeGround: TColor; alpha: advancedFloat): TColor;overload;
function ColorBlend_ForegroundSourceAlpha(cBackGround, cForeGround: TColor; alpha: advancedFloat): TColor;
function ColorBlendRGBA(cBackGround, cForeGround: TColor; alpha: advancedFloat): TColor;

  overload;
function ColorOp(cBackGround, cForeGround: TColor; alpha: advancedFloat;
  AlphaOp: TAlphaOp): TColor;

function ColorReverse(c: TColor): TColor;
function ColorAlpha(c: TColor; rAlpha: advancedFloat): TColor;
function ColorMultiply(c: TColor; c2: TColor): TColor;

procedure Register;

implementation


{ TDrawingBoard }

// ------------------------------------------------------------------------------


function ColorReverse(c: TColor): TColor;
var
  p: pbyte;
  r: byte;
begin
  result := c;
  p := @result;
  r :=   p[0];
  p[0] := p[2];
  p[2] := r;


end;

function ColorAlpha(c: TColor; rAlpha: advancedFloat): TColor;
var
  p: pbyte;
begin
  result := c;
  p := @result;
  p[3] := round(rAlpha * 255);

end;
procedure TDrawingBoard.Circle(x1, y1, r: advancedFloat; color: TColor;
  Fill: boolean);
var
  t,u: nativeint;
  startx,endx,starty,endy: nativeint;
  b: boolean;
  l,w,rr: advancedFloat;
  sxc,syc: nativeint;
begin
  canvas.Brush.color := color;
  canvas.Pen.color := color;


  rr := ScaleGlobalXtoScreen(r);
  startx := round(GlobalToCanvasXi(x1)-rr);
  endx := round(globaltocanvasxi(x1)+rr);
  starty := round(globaltocanvasyi(y1)-rr);
  endy := round(globaltocanvasyi(y1)+rr);


  sxc := (startx+endx) div 2;
  syc := (starty+endy) div 2;

  for u := starty to endy do begin
    for t:= startx to endx do begin
      l := t-sxc;
      w := u-syc;
      if sqrt((l*l)+(w*w)) < rr then
        self.DrawTo_pixels[t,u] := color;

    end;
  end;








//  Arc(Round(GlobalToCanvasX(x1)), Round(GlobalToCanvasY(y1)),
//    Round(GlobalToCanvasX(x2)), Round(GlobalToCanvasY(y2)),
//    Round(GlobalToCanvasX(x2)), Round(GlobalToCanvasY(y1)),
//    Round(GlobalToCanvasX(x1)), Round(GlobalToCanvasY(y1)));

// DebugFlip();
end;

procedure TDrawingBoard.Clear(color: TColor);
begin
  self.Rectangle(BoundX1, BoundY1, BoundX2, BoundY2, color, true);

end;

procedure TDrawingBoard.ClearBoundsRect;
begin
  screenmaskX1 := GlobalToCanvasX(BoundX1);
  screenmaskX2 := GlobalToCanvasX(BoundX2);
  screenmaskY1 := GlobalToCanvasY(BoundY1);
  screenmaskY2 := GlobalToCanvasY(BoundY2);
end;

// ------------------------------------------------------------------------------
function TDrawingBoard.GlobalToScreenX(globalX: advancedFloat): advancedFloat;
begin
  if FUseScreenCoordinates then
    result := globalX * FAntiAliasing
  else
    result := Interpolate(globalX, (LeftMargin * AntiAliasing),
      ((width - RightMargin) * AntiAliasing), BoundX1, BoundX2);

end;

// ------------------------------------------------------------------------------
function TDrawingBoard.GlobalToCanvasX(globalX: advancedFloat): advancedFloat;
begin
  if FUseScreenCoordinates then
    result := globalX * FAntiAliasing
  else
    result := Interpolate(globalX, (LeftMargin * AntiAliasing),
      ((width - RightMargin) * AntiAliasing), BoundX1, BoundX2);

end;

function TDrawingBoard.GlobalToCanvasY(globalY: advancedFloat): advancedFloat;
begin
  if FUseScreenCoordinates then
    result := globalY * FAntiAliasing
  else
    result := Interpolate(globalY, (TopMargin * AntiAliasing),
      ((height - BottomMargin) * AntiAliasing), BoundY1, BoundY2);

end;

// ------------------------------------------------------------------------------
function TDrawingBoard.GlobalToScreenY(globalY: advancedFloat): advancedFloat;
begin
  if FUseScreenCoordinates then
    result := globalY
  else
    result := Interpolate(globalY, (TopMargin), ((height - BottomMargin)),
      BoundY1, BoundY2);
end;

procedure TDrawingBoard.InvertRect(canvasX1, canvasY1, canvasX2,
  canvasY2: integer);
var
  x, y: integer;
  x1, x2, y1, y2: integer;
  sl: PAnsiChar;
  c: TColor;
  p: pointer;
begin
  exit;
  canvas.lock;
// exit;
  try
    x1 := Round(LesserOf(canvasX1, canvasX2));
    x2 := Round(GreaterOf(canvasX1, canvasX2));
    y1 := Round(LesserOf(canvasY1, canvasY2));
    y2 := Round(GreaterOf(canvasY1, canvasY2));

    for y := y1 to y2 do begin
// sl := self.backbitmap.ScanLine[y];
      for x := x1 to x2 do begin
// p := @sl[x*3];
// c := GetFastPOint(x,y);
// SetFastPoint(x,y, (not c) and $FFFFFF);
// SetColorTopointer(p, (not c) and $FFFFFF);
        DrawTo_pixels[x, y] := (not self.DrawTo_pixels[x, y]) and
          (((255 shl 16) + 255 shl 8) + 255);
      end;
    end;
  finally
    canvas.unlock;
  end;

end;

// ------------------------------------------------------------------------------
function TDrawingBoard.CanvasToScreenY(canvasY: advancedFloat): advancedFloat;
begin
  result := canvasY / AntiAliasing;

end;

// ------------------------------------------------------------------------------
function TDrawingBoard.ScreenToGlobalX(screenX: advancedFloat): advancedFloat;
begin
  if FUseScreenCoordinates then
    result := screenX
  else
    result := Interpolate(screenX, BoundX1, BoundX2, LeftMargin,
      width - (RightMargin + LeftMargin));
end;

// ------------------------------------------------------------------------------

function TDrawingBoard.CanvasToGlobalX(canvasX: advancedFloat): advancedFloat;
begin
  if FUseScreenCoordinates then
    result := canvasX / FAntiAliasing
  else
    result := Interpolate(canvasX, BoundX1, BoundX2, LeftMargin * AntiAliasing,
      (width - (RightMargin + LeftMargin)) * AntiAliasing);
// result := Interpolate(globalX, (LeftMargin*AntiAliasing), ((Width-RightMargin)*AntiAliasing), BoundX1, BoundX2);

end;

// ------------------------------------------------------------------------------
function TDrawingBoard.ScreenToGlobalY(screenY: advancedFloat): advancedFloat;
begin
  if FUseScreenCoordinates then
    result := screenY
  else
    result := Interpolate(screenY, BoundY1, BoundY2, TopMargin,
      height - (BottomMargin + TopMargin));
end;

// ------------------------------------------------------------------------------
function TDrawingBoard.CanvasToGlobalY(canvasY: advancedFloat): advancedFloat;
begin
  if FUseScreenCoordinates then
    result := canvasY
  else
    result := Interpolate(canvasY, BoundY1, BoundY2, TopMargin,
      height - (BottomMargin + TopMargin));

  result := CanvasToScreenX(result);

end;

constructor TDrawingBoard.Create;
begin
  inherited Create;

  FAntiAliasing := 1;
  FConstrainProportions := true;
  FOnPaint := nil;
  FBoundY2 := 100;
  FBoundX1 := 0;
  FBoundY1 := 0;
  FBoundX2 := 100;
  ClearBoundsRect;
// FDrawto := self.Canvas;

  // RoundEdges := true;
  FTextHelperBitmap := TBitmap.create;
  FTextHelperBitmap.PixelFormat := pf32bit;

end;


procedure TDrawingBoard.DesignDraw;
var
  x, y: integer;
begin
  inherited;
  with self do begin
    for x := 0 to (Round(BoundX2) div 10) do begin
      for y := 0 to (Round(BoundY2) div 10) do begin
        self.Rectangle(x * 10, y * 10, (x * 10) + 5, (y * 10) + 5, clWhite);
      end;
    end;
  end;
end;

destructor TDrawingBoard.Destroy;
begin
  FTextHelperBitmap.free;
  FTextHelperBitmap := nil;
  inherited;
end;

function TDrawingBoard.GetFastPoint(x, y: integer): TColor;
var
  r, g, b: byte;
begin

  result := self.DrawTo_pixels[x, y];

end;

function TDrawingBoard.GetFillColor: TColor;
begin
  result := FFillColor;
end;

// ------------------------------------------------------------------------------
function TDrawingBoard.GetLineColor: TColor;
begin
  result := FLineColor;
end;

// ------------------------------------------------------------------------------
procedure TDrawingBoard.Crosshair(gx1, gy1, sz: advancedFloat; color: TColor);
begin
  self.Line(gx1,gy1-sz, gx1,gy1+sz, color);
  self.Line(gx1-sz, gy1, gx1+sz, gy1, color);


end;
procedure TDrawingBoard.Line(gx1, gy1, gx2, gy2: advancedFloat; color: TColor);
begin
  self.FatLine(gx1,gy1,gx2,gy2,1,color, true);
  exit;
//  gx1 := GlobalToCanvasX(gx1);
//  gy1 := GlobalToCanvasY(gy1);
//  gx2 := GlobalToCanvasX(gx2);
//  gy2 := GlobalToCanvasY(gy2);
//  canvas.Pen.color := color;
//  canvas.PenPos := point(Round(gx1), Round(gy1));
//  DrawTo_pixels[Round(gx1), Round(gy1)] := canvas.Pen.color;
//  self.Line(Round(gx1), Round(gy1),Round(gx2), Round(gy2),clWhite);
//  LineTo(Round(gx2), Round(gy2), canvas.pen.color);
//  DrawTo_pixels[Round(gx2), Round(gy2)] := canvas.Pen.color;
end;

// ------------------------------------------------------------------------------
procedure TDrawingBoard.DoDraw;
begin
  inherited;

  if Assigned(FOnPaint) then
    FOnPaint(self);

end;

procedure TDrawingBoard.Draw;
begin
  DoDraw;
end;

procedure TDrawingBoard.DrawLineTest;
var
  t,u: nativeint;
const
  steps = 50;
begin
//  pushbounds;
  try
    boundx1 := 0;
    boundy1 := 0;
    boundx2 := 50;
    boundy2 := 50;
    for t:= 0 to steps do begin
      Line(50-t,0, 0,t,$ff007f);
      Line(t, 50, 50, 50-t,$ff00ff);
      Line(t,  0, 50,  t,$ff7f00);
      Line(50-t,  50, 0,  50-t,$00ff00);
    end;


  finally
//    popbounds;
  end;


end;

procedure TDrawingBoard.DrawTo_SetPixelEx(x, y: integer; color: TColor;
  AlphaOp: TAlphaOp);
var
  c: TColor;
begin
  if x< 0 then exit;
  if y < 0 then exit;
  c := DrawTo_pixels[x, y];
  c := ColorOp(c, color, 1.0, AlphaOp);
  Drawto_pixels[x,y] := c;

end;

procedure TDrawingBoard.SetPoint(x1, y1: advancedFloat; color: TColor);
begin
  DrawTo_pixels[Round(GlobalToCanvasX(x1)), Round(GlobalToCanvasY(y1))] :=
    canvas.Pen.color;
end;

// ------------------------------------------------------------------------------
procedure TDrawingBoard.RecalcScale;
var
  iDiv: advancedFloat;
  OldScale: advancedFloat;
begin
// if DimensionX < Width then
// DimensionX := Width;

// if DimensionY < Height then
// DimensionY := height;
  OldScale := FScale;

  iDiv := FBoundX2 - FBoundX1;
  if iDiv = 0 then begin
    FScale := 1;

  end
  else begin

    if width <> 0 then
      FScale := (FBoundX2 - FBoundX1) / width;

    if ConstrainProportions then
      FBoundY2 := trunc((height * (FScale))) + FBoundY1;
  end;

// if OldScale<>FScale then
// Invalidate;

end;

(* procedure TDrawingBoard.Rectangle(x1, y1, x2, y2: advancedFloat; color: TColor; Fill: boolean);
begin
  lock;
  try
    x1 := GlobalToCanvasX(x1);
    y1 := GlobalToCanvasY(y1);
    x2 := GlobalToCanvasX(x2)+1;
    y2 := GlobalToCanvasY(y2)+1;

    self.FillColor := color;
    self.LineColor := color;

    if Fill then begin
      Brush.style := bsSolid;
      FillRect(Rect(Round(x1),Round(y1),Round(x2),Round(y2)))
    end
    else begin
      Brush.Style := bsClear;
      Rectangle(Round(x1),Round(y1),Round(x2),Round(y2));
    end;
  finally
    unlock;
  end;

end; *)

procedure TDrawingBoard.FancyRectangle(x1, y1, x2, y2: advancedFloat; color: TColor);
var
  color1, color2, color3: TColor;
  t, u: integer;
  xp, yp: advancedFloat;
  r: advancedFloat;
begin
  color1 := colorblending.ColorBlend(color, clWhite, 0.5);
  color2 := color;
  color3 := colorblending.ColorBlend(color, clBlack, 0.5);

  canvas.lock;
  try
    x1 := GlobalToCanvasX(x1);
    y1 := GlobalToCanvasY(y1);
    x2 := GlobalToCanvasX(x2);
    y2 := GlobalToCanvasY(y2);

    if y1 > y2 then begin
      r := y1;
      y1 := y2;
      y2 := r;
    end;

    if x1 > x2 then begin
      r := x1;
      x1 := x2;
      x2 := r;
    end;

    for u := Round(y1) to Round(y2) do begin
      for t := Round(x1) to Round(x2) do begin
        xp := abs(t - x1) / (abs(x2 - x1) + 1);
        yp := abs(u - y1) / (abs(y2 - y1) + 1);
        DrawTo_pixels[t, u] := colorblending.ColorBlend(colorblending.ColorBlend(color3, color2, 1 - xp),
          colorblending.ColorBlend(color2, color1, 1.0 - xp), 1.0 - yp);
      end;
    end;

  finally
    canvas.unlock;
  end;

end;

procedure TDrawingBoard.Rectangle(x1, y1, x2, y2: advancedFloat; color: TColor;
  Fill: boolean = false; bGradient: boolean = false;
  bgcolor: TColor = clBlack);
var
  color1, color2, color3: TColor;
  t, u: integer;
  xp, yp: advancedFloat;
  r: advancedFloat;
begin
  color2 := color;

  canvas.lock;
  try
    x1 := GlobalToCanvasX(x1);
    y1 := GlobalToCanvasY(y1);
    x2 := GlobalToCanvasX(x2);
    y2 := GlobalToCanvasY(y2);

    if y1 > y2 then begin
      r := y1;
      y1 := y2;
      y2 := r;
    end;

    if x1 > x2 then begin
      r := x1;
      x1 := x2;
      x2 := r;
    end;

    x2 := x2 + 1;
    y2 := y2 + 1;

    if Fill then begin
      if not bGradient then begin
        for u := Round(y1) to Round(y2) - 1 do begin
          for t := Round(x1) to Round(x2) - 1 do begin
            xp := abs(t - x1) / (abs(x2 - x1) + 1);
            yp := abs(u - y1) / (abs(y2 - y1) + 1);
            DrawTo_SetPixelEx(t, u, color, AlphaOp);

          end;
        end;
      end
      else begin
        for u := Round(y1) to Round(y2) - 1 do begin
          for t := Round(x1) to Round(x2) - 1 do begin
            xp := abs(t - x1) / (abs(x2 - x1) + 1);
            yp := abs(u - y1) / (abs(y2 - y1) + 1);
            DrawTo_SetPixelEx(t, u,
              colorblending.ColorBlend(bgcolor, color, (u - y1) / (y2 - y1)), AlphaOp);
          end;
        end;
      end;
    end
    else begin
      FatLine(x1, y1, x2, y1, 1, color, false);
      FatLine(x2, y1, x1, y2, 1, color, false);
      FatLine(x1, y2, x2, y2, 1, color, false);
      FatLine(x1, y1, x1, y2, 1, color, false);
    end;

  finally
    canvas.unlock;
  end;

end;

procedure TDrawingBoard.SetFastPoint(x, y: integer; color: TColor);
begin
  self.DrawTo_pixels[x, y];

end;

// ------------------------------------------------------------------------------
(* procedure TDrawingBoard.Rectangle(x1, y1, x2, y2: integer; color: TColor;
  Fill: boolean);
begin
  rectangle(x1*0.0, y1*0.0, x2*0.0, y2*0.0, color);
end; *)


procedure TDrawingBoard.SetScreenMask(x1, y1, x2, y2: advancedFloat);
begin
  screenmaskX1 := x1;
  screenmaskX2 := x2;
  screenmaskY1 := y1;
  screenmaskY2 := y2;
end;

procedure TDrawingBoard.SetBoundX1(const Value: advancedFloat);
begin
  FBoundX1 := Value;
  ClearBoundsRect;
  RecalcScale;
end;

// ------------------------------------------------------------------------------
procedure TDrawingBoard.SetBoundX2(const Value: advancedFloat);
begin
  FBoundX2 := Value;
  ClearBoundsRect;
  RecalcScale;
end;

// ------------------------------------------------------------------------------
procedure TDrawingBoard.SetBoundY1(const Value: advancedFloat);
begin
  FBoundY1 := Value;
  ClearBoundsRect;
  RecalcScale;
end;

// ------------------------------------------------------------------------------
procedure TDrawingBoard.SetBoundY2(const Value: advancedFloat);
begin
  FBoundY2 := Value;
  ClearBoundsRect;
  RecalcScale;
end;

// ------------------------------------------------------------------------------
procedure TDrawingBoard.SetConstrainProportions(const Value: boolean);
begin
  FConstrainProportions := Value;
  RecalcScale;
end;

procedure TDrawingBoard.SetDimensionX(const Value: advancedFloat);
begin
  BoundX2 := BoundX1 + Value;
end;

procedure TDrawingBoard.SetDimensionY(const Value: advancedFloat);
begin
  BoundY2 := BoundY1 + Value;
end;

procedure TDrawingBoard.SetDrawTo_Pixels(x, y: integer; const Value: TColor);
begin
  if x < 0 then
    exit;
  if y < 0 then
    exit;
  if x >= width then
    exit;
  if y >= height then
    exit;

  inc(y, Round(self.screenmaskY1));
  inc(x, Round(self.screenmaskX1));

  if (x <= screenmaskX2) and (y <= screenmaskY2) then
    canvas.pixels[x, y] := Value;
end;

procedure TDrawingBoard.SetFillColor(const Value: TColor);
begin
  FFillColor := Value;
  canvas.Brush.color := Value;
end;

// ------------------------------------------------------------------------------
procedure TDrawingBoard.SetLineColor(const Value: TColor);
begin
  FLineColor := Value;
  canvas.lock;
  try
    canvas.Pen.color := Value;
  finally
    canvas.unlock;
  end;
end;

procedure TDrawingBoard.SetLowResPoint(gx, gy: advancedFloat; col: TColor);
begin
  Rectangle(gx, gy, gx, gy, col, true);
end;

procedure Register;
begin
  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TDrawingBoard.SetScale(const Value: advancedFloat);
begin
  FScale := Value;
end;

{ TAASample }

function TDrawingBoard.ScaleScreenXtoGlobal(Xdistance: advancedFloat; bKeepSign: boolean = false): advancedFloat;
begin
  if FUseScreenCoordinates then
    result := Xdistance
  else
    result := Interpolate(Xdistance, 0, DimensionX, 0,
      (width - (RightMargin + LeftMargin)));

  if not bKeepSign then
    if result < 0 then
      result := 0 - result;

end;



function TDrawingBoard.ScaleCanvasToScreen(x: integer): advancedFloat;
begin
  result := (x / AntiAliasing);

end;

function TDrawingBoard.ScaleGlobalXtoScreen(Xdistance: advancedFloat): advancedFloat;
begin
  if FUseScreenCoordinates then
    result := (Xdistance)
  else
    result := Interpolate(Xdistance, 0, (width) - ((RightMargin + LeftMargin)),
      0, DimensionX);

  if result < 0 then
    result := 0 - result;

end;

function TDrawingBoard.ScaleScreenYtoGlobal(Ydistance: advancedFloat; bKeepSign: boolean = false): advancedFloat;
begin
  if FUseScreenCoordinates then
    result := (Ydistance)
  else
    result := Interpolate(Ydistance, 0, DimensionY, 0,
      (height) - (BottomMargin + TopMargin));

  if not bKeepSign then
    if result < 0 then
      result := 0 - result;

end;

function TDrawingBoard.ScaleGlobalYtoScreen(Ydistance: advancedFloat): advancedFloat;
begin
  if FUseScreenCoordinates then
    result := (Ydistance)
  else
    result := Interpolate(Ydistance, 0,
      (height) - ((BottomMargin + TopMargin)), 0, DimensionY);
end;

function TDrawingBoard.GetDimensionX: advancedFloat;
begin
  result := BoundX2 - BoundX1;
end;

function TDrawingBoard.GetDimensiONY: advancedFloat;
begin
  result := BoundY2 - BoundY1;

  if result < 0 then
    result := 0 - result;

end;

function TDrawingBoard.GetDrawTo_pixels(x, y: integer): TColor;
begin
  inc(y, Round(self.screenmaskY1));
  inc(x, Round(self.screenmaskX1));

  result := canvas.pixels[x, y];
end;




// ------------------------------------------------------------------------------
procedure TDrawingBoard.Text(globalX, globalY, width, height: advancedFloat;
  sText: ansistring; bCenter: boolean = false; bVertical: boolean = false);
var
  tj: TTextJustify;
  tr: TTextRotate;
begin
  if bCenter then
    tj := tjCenter
  else
    tj := tjLeft;
  if bVertical then
    tr := trVertical
  else
    tr := trHorizontal;
  Text(globalX, globalY, width, height, sText, tj, tr);

end;

// ------------------------------------------------------------------------------
procedure TDrawingBoard.FindFontSize(width, height: integer; sText: ansistring);
var
  itargetWidth, i, x, t: integer;
  iOldSize: integer;
begin
  // increment fontsize until height > desired height;
  iOldSize := TextHelper.font.size;
  TextHelper.font.size := 500;
  i := TextHelper.TextWidth(sText);
  itargetWidth := trunc((height / 500) * i);

  TextHelper.font.size := iOldSize;

  repeat
    TextHelper.font.size := TextHelper.font.size + 1;
    x := TextHelper.TextHeight(sText);
    // if x = 0 then font.size := 8;
  until (x > height) or (x = 0);

  // decrement fontsize until height < desired height;
  repeat
    TextHelper.font.size := TextHelper.font.size - 1;
    x := TextHelper.TextHeight(sText);
  until (x <= height) or (TextHelper.font.size < 1);

  // decrement fontsize until width < targetwidth;
  repeat
    TextHelper.font.size := TextHelper.font.size - 1;
    x := TextHelper.TextWidth(sText);
  until (x <= itargetWidth) or (TextHelper.font.size < 1);

end;


function TDrawingBoard.TruncateText(width: integer;
  sText: ansistring): ansistring;
begin
  result := sText;

  if TextHelper.TextWidth(sText) <= width then
    exit;

  while (TextHelper.TextWidth(sText + '..') > width) and (sText <> '') do
    sText := copy(sText, 1, length(sText) - 1);

  result := sText + '..';

end;



// ------------------------------------------------------------------------------

// ------------------------------------------------------------------------------
function GetPointDistance(x1, y1, x2, y2: advancedFloat): advancedFloat;
// uses pythagrean theorem to calc distance between two points
var
  dx, dy: advancedFloat;
begin
  dx := x2 - x1;
  dy := y2 - y1;

  if dy = 0 then
    result := dx
  else if dx = 0 then
    result := dy
  else
    result := Sqrt(Sqr(dx) + Sqr(dy));

  result := abs(result);

end;

// ------------------------------------------------------------------------------
function GetPointDistanceSq(x1, y1, x2, y2: advancedFloat): advancedFloat;
// uses pythagrean theorem to calc distance between two points
var
  dx, dy: advancedFloat;
begin
  dx := x2 - x1;
  dy := y2 - y1;

  if dy = 0 then
    result := dx
  else if dx = 0 then
    result := dy
  else
    result := Sqr(dx) + Sqr(dy);

  result := abs(result);

end;

procedure TDrawingBoard.Quad(gx1, gy1, gx2, gy2, gx3, gy3, gx4, gy4: advancedFloat;
  color: TColor);
var
  cx, cy, steps, t, tx1, ty1, tx2, ty2: advancedFloat;
  xi, yi: advancedFloat; // x and y increments
begin
  // find increment amounts based on the true width of one screen pixel
  xi := self.ScaleScreenXtoGlobal(1) / 2;
  yi := self.ScaleScreenYtoGlobal(1) / 2;

  // draw outer shell
  Line(gx1, gy1, gx2, gy2, color);
  Line(gx2, gy2, gx3, gy3, color);
  Line(gx3, gy3, gx4, gy4, color);
  Line(gx4, gy4, gx1, gy1, color);

// determine number of steps based on the longest travel distance
  cx := abs(gx4 - gx1);
  if abs(gx3 - gx2) > cx then
    cx := abs(gx3 - gx2);

  cy := abs(gy4 - gy1);
  if abs(gy4 - gy2) > cy then
    cy := abs(gx3 - gx2);

  if cx > cy then
    steps := cx
  else
    steps := cy;

  steps := steps;
  t := 0;
  while t <= steps do begin
    tx1 := Interpolate(t, steps, gx1, gx4);
    ty1 := Interpolate(t, steps, gy1, gy4);
    tx2 := Interpolate(t, steps, gx2, gx3);
    ty2 := Interpolate(t, steps, gy2, gy3);

// Line(tx1,ty1,tx2,ty2,color);
    Line(gx1, gy1, tx2, ty2, color);
    Line(gx2, gy2, tx2, ty2, color);
    Line(gx3, gy3, tx2, ty2, color);
    Line(gx4, gy4, tx2, ty2, color);
    Line(gx1, gy1, tx1, ty1, color);
    Line(gx2, gy2, tx1, ty1, color);
    Line(gx3, gy3, tx1, ty1, color);
    Line(gx4, gy4, tx1, ty1, color);

    t := t + xi;
  end;

  // determine number of steps based on the longest travel distance
  cx := abs(gx2 - gx1);
  if abs(gx3 - gx4) > cx then
    cx := abs(gx3 - gx4);

  cy := abs(gy2 - gy1);
  if abs(gy3 - gy4) > cy then
    cy := abs(gx3 - gx4);

  if cx > cy then
    steps := cx
  else
    steps := cy;

  steps := steps;
  t := 0;
  while t <= steps do begin
    tx1 := Interpolate(t, steps, gx1, gx2);
    ty1 := Interpolate(t, steps, gy1, gy2);
    tx2 := Interpolate(t, steps, gx4, gx3);
    ty2 := Interpolate(t, steps, gy4, gy3);

// Line(tx1,ty1,tx2,ty2,color);
    Line(gx1, gy1, tx2, ty2, color);
    Line(gx2, gy2, tx2, ty2, color);
    Line(gx3, gy3, tx2, ty2, color);
    Line(gx4, gy4, tx2, ty2, color);
    Line(gx1, gy1, tx1, ty1, color);
    Line(gx2, gy2, tx1, ty1, color);
    Line(gx3, gy3, tx1, ty1, color);
    Line(gx4, gy4, tx1, ty1, color);
    t := t + xi;
  end;

end;

procedure TDrawingBoard.Slice(centerX, centerY, gx1, gy1, gx2, gy2: advancedFloat;
  color: TColor);
begin

// TODO -cunimplemented: unimplemented block
end;

procedure TDrawingBoard.Text(x, y: advancedFloat; sText: ansistring);
begin
  Text(x, y, TEXT_WIDTH_INFINITE, TextHeight, sText, TextJustify, TextRotate);
end;

procedure TDrawingBoard.Text(globalX, globalY, width, height: advancedFloat;
  sText: ansistring; just: TTextJustify = tjLeft;
  rot: TTextRotate = trHorizontal);
// procedure Text(globalx, globaly, width, height: advancedFloat;
// sText: ansistring; just: TTextJustify; rot: TTextRotation);
var
  screenX: integer;
  textdraw: TImage;
  t, u, t1, u1: integer;
  sx, sy: integer;
  a, c, r, g, b, s: advancedFloat;
  bg: TColor;
  w: integer;
  h: THSLnativeFloatColor;
  shadowalpha: advancedFloat;
  c1, c2: TColor;

  procedure CopyH;
  var
    t, u: integer;
  begin
    if just = tjCenter then
      w := textdraw.picture.bitmap.width
    else
      w := textdraw.canvas.TextWidth(sText);

    for u := 0 to Round(textdraw.picture.bitmap.height) - 1 do begin

      for t := 0 to Round(w) - 1 do begin
        h := RGBtoHSL(ColorToNativeFloatRGB(textdraw.canvas.pixels[t, u]));
        a := h.l;

        if TextKnockOut in [tkoBackGround, tkoReverseBackGround] then
          bg := TextBackgroundColor
        else
          bg := DrawTo_pixels[sx + t, sy + u];

          // determine amount of shadow
        t1 := t - (1 * AntiAliasing);
        u1 := u - (1 * AntiAliasing);
        if (t1 < 0) or (u1 < 0) then
          s := 0
        else begin
          s := ((textdraw.canvas.pixels[t1, u1] and 255) / 255);
          bg := colorblending.ColorBlend(bg, clBlack, s * shadowalpha);
        end;

        if TextKnockOut in [tkoReverseTransparent, tkoReverseBackGround] then
          begin
          c1 := bg;
          c2 := TextColor;
        end
        else begin
          c1 := TextColor;
          c2 := bg;
        end;
        DrawTo_pixels[sx + t, sy + u] := colorblending.ColorBlend(c2, c1, a);

// Pixels[sx+t,sy+u] := textdraw.canvas.pixels[t,u];

      end;
    end;
  end;
  procedure CopyV;
  var
    t, u: integer;
  begin
    if just = tjCenter then
      w := textdraw.picture.bitmap.width
    else
      w := textdraw.canvas.TextWidth(sText);

    for u := 0 to Round(textdraw.picture.bitmap.height) - 1 do begin
      for t := 0 to Round(w) - 1 do begin
        h := RGBtoHSL(ColorToNativeFloatRGB(textdraw.canvas.pixels[t, u]));
        a := h.l;

        if TextKnockOut in [tkoBackGround, tkoReverseBackGround] then
          bg := TextBackgroundColor
        else
          bg := DrawTo_pixels[sx + u, sy + t];

          // determine amount of shadow
        t1 := t - (1 * AntiAliasing);
        u1 := u - (1 * AntiAliasing);
        if (t1 < 0) or (u1 < 0) then
          s := 0
        else begin
          s := ((textdraw.canvas.pixels[t1, u1] and 255) / 255);
          bg := colorblending.ColorBlend(bg, clBlack, s * shadowalpha);
        end;

        if TextKnockOut in [tkoReverseTransparent, tkoReverseBackGround] then
          begin
          c1 := bg;
          c2 := TextColor;
        end
        else begin
          c1 := TextColor;
          c2 := bg;
        end;
        DrawTo_pixels[sx + u, sy - t] := colorblending.ColorBlend(c2, c1, a);

// drawto_Pixels[sx+t,sy+u] := textdraw.canvas.pixels[t,u];

      end;
    end;
  end;

begin
  if sText = '' then
    exit;
  canvas.lock;
  try
    shadowalpha := ((RGBtoHSL(ColorToNativeFloatRGB(texthelper.font.color)).l - 0.5) / 0.5);
    if shadowalpha < 0 then
      shadowalpha := 0;

    height := ScaleScreenToCanvas(abs(ScaleGlobalYtoScreen(height)));
    if width = TEXT_WIDTH_INFINITE then begin
      FindFontSize(self.width, trunc(height), sText);
      width := TextHelper.TextWidth(sText);
    end
    else begin
      width := ScaleScreenToCanvas(ScaleGlobalXtoScreen(width));
      FindFontSize(trunc(width), trunc(height), sText);
    end;

// font size
    TextHelper.font.Name := 'Arial';

    // create a drawing board
{$IFNDEF simpletext}
    textdraw := TImage.Create(nil);
    textdraw.height := Round(height);
    textdraw.width := Round(width);

    textdraw.picture := nil;
    textdraw.canvas.lock;
    try
      textdraw.canvas.Rectangle(0, 0, textdraw.width, textdraw.height);
      textdraw.canvas.Pen.color := clBlack;
      textdraw.canvas.Brush.color := clBlack;
      textdraw.picture.bitmap.PixelFormat := pf32bit;
      textdraw.canvas.Rectangle(0, 0, textdraw.width, textdraw.height);

// textdraw.Canvas.rectangle(0,0,textdraw.width, textdraw.height);
      textdraw.picture.bitmap.canvas.font.color := clWhite;

      textdraw.canvas.font.size := TextHelper.font.size;
      textdraw.canvas.font.Name := TextHelper.font.name;
      if AutoTruncateText then
        sText := self.TruncateText(Round(textdraw.width), sText);
      if not(just = tjCenter) then
        textdraw.canvas.TextOut(0, 0, sText)
      else begin
        screenX := Round(((textdraw.width / 2)) - (textdraw.canvas.TextWidth
              (sText) / 2));
        textdraw.canvas.TextOut(screenX, 0, sText)
      end;

      if not self.NewTextAlignRules then begin
        sx := GlobalToCanvasXi(globalX);
        sy := GlobalToCanvasYi(globalY);
      end
      else begin
        case TextJustify of
          tjLeft: begin
              sx := GlobalToCanvasXi(globalX);
              sy := GlobalToCanvasYi(globalY);
            end;
          tjRight: begin
              sx := Round(GlobalToCanvasXi(globalX) - width);
              sy := GlobalToCanvasYi(globalY);
            end;
          tjCenter: begin
              sx := Round(GlobalToCanvasXi(globalX) - (width / 2));
              sy := GlobalToCanvasYi(globalY);
            end;
        end;
      end;

// for best performance determine width of text to copy
      if not(rot = trVertical) then
        CopyH
      else
        CopyV;

    finally
      textdraw.canvas.unlock;
      textdraw.free;
    end;
{$ELSE}
    FindFontSize(trunc(width), trunc(height), sText);
    sText := self.TruncateText(Round(width), sText);

    if not bCenter then
      TextOut(Round(ToScreenX(globalX)), Round(ToScreenY(globalY)),
        sText)
    else begin
      screenX := Round((ToScreenX(globalX) + (width / 2)) -
          (TextWidth(sText) / 2));
      TextOut(screenX, Round(ToScreenY(globalY)), sText)
    end;
{$ENDIF}
  finally
    canvas.unlock;
  end;

// DebugFlip();

end;

procedure TDrawingBoard.Tri(gx1, gy1, gx2, gy2, gx3, gy3: advancedFloat; color: TColor);
begin

// TODO -cunimplemented: unimplemented block
end;

procedure TDrawingBoard.OldFatLine(gx1, gy1, gx2, gy2, width: advancedFloat;
  color: TColor; bfirstSegment: boolean);
var
  ang: advancedFloat;
  bx1, bx2, bx3, bx4: advancedFloat;
  by1, by2, by3, by4: advancedFloat;
  pt: TNativeFloatPOint;
begin
  ang := GetLineAngle(gx1, gy1, gx2, gy2);

  // corner1 of box;
  pt := GetAnglePoint(gx1, gy1, width / 2, ang + DegToRad(90));
  bx1 := pt.x;
  by1 := pt.y;

  // corner2 of box;
  pt := GetAnglePoint(gx1, gy1, width / 2, ang - DegToRad(90));
  bx2 := pt.x;
  by2 := pt.y;

  // corner3 of box;
  pt := GetAnglePoint(gx2, gy2, width / 2, ang - DegToRad(90));
  bx3 := pt.x;
  by3 := pt.y;

  // corner4 of box;
  pt := GetAnglePoint(gx2, gy2, width / 2, ang + DegToRad(90));
  bx4 := pt.x;
  by4 := pt.y;

  self.Quad(bx1, by1, bx2, by2, bx3, by3, bx4, by4, color);

end;

// ------------------------------------------------------------------------------
function GetAnglePoint(ox, oy, distance, angleInRad: advancedFloat): TNativeFloatPOint;
begin
  while angleInRad < 0 do
    angleInRad := angleInRad + DegToRad(360);

  while angleInRad > 360 do
    angleInRad := angleInRad - DegToRad(360);

  if (angleInRad >= DegToRad(0)) and (angleInRad < DegToRad(90)) then begin
    result.x := ox + abs(Sin(angleInRad) * distance);
    result.y := oy - abs(Cos(angleInRad) * distance);

  end;

  if (angleInRad >= DegToRad(90)) and (angleInRad < DegToRad(180)) then begin
    angleInRad := angleInRad - DegToRad(90);
    result.x := ox + abs(Cos(angleInRad) * distance);
    result.y := oy + abs(Sin(angleInRad) * distance);

  end;

  if (angleInRad >= DegToRad(180)) and (angleInRad < DegToRad(270)) then begin
    angleInRad := angleInRad - DegToRad(180);
    result.x := ox - abs(Sin(angleInRad) * distance);
    result.y := oy + abs(Cos(angleInRad) * distance);

  end;

  if (angleInRad >= DegToRad(270)) and (angleInRad < DegToRad(360)) then begin
    angleInRad := angleInRad - DegToRad(360);
    result.x := ox - abs(Sin(angleInRad) * distance);
    result.y := oy - abs(Cos(angleInRad) * distance);

  end;

end;

// ------------------------------------------------------------------------------
function GetSlopePoint(ox, oy, distance: advancedFloat; slope: TSlopeResult): TNativeFloatPOint;
begin
  raise Exception.Create('unimplemented');

end;

// ------------------------------------------------------------------------------
function GetLineAngle(x1, y1, x2, y2: advancedFloat): advancedFloat;
var
  lx, ly: advancedFloat;
begin
  lx := x2 - x1;
  ly := y2 - y1;

  if ly = 0 then
    result := 90
  else
    result := abs(arctan(lx / ly));

  // NEGATIVE Y
  if ly < 0 then begin
    if lx < 0 then
      result := DegToRad(360) - result;
  end
  // positive Y
  else begin
    if lx < 0 then
      result := DegToRad(180) + result
    else
      result := DegToRad(180) - result;
  end;

end;

procedure TDrawingBoard.FatPoint(x, y: advancedFloat; cInner, cOuter: TColor;
  size: integer = 3);
var
  rx, ry: integer;
begin
  rx := GlobalToCanvasXi(x);
  ry := GlobalToCanvasYi(y);

  LineColor := cOuter;
  FillColor := cInner;
  canvas.Brush.color := cOuter;
  canvas.Pen.color := cOuter;
  canvas.Rectangle(rx - ((size - 0) * AntiAliasing),
    ry - ((size - 0) * AntiAliasing), rx + ((size - 0) * AntiAliasing),
    ry + ((size - 0) * AntiAliasing));
  canvas.Brush.color := cInner;
  canvas.Pen.color := cInner;
  canvas.Rectangle(rx - ((size - 1) * AntiAliasing),
    ry - ((size - 1) * AntiAliasing), rx + ((size - 1) * AntiAliasing),
    ry + ((size - 1) * AntiAliasing));

// DebugFlip();
end;

function TDrawingBoard.GlobalToCanvasXi(globalX: advancedFloat): integer;
begin
  result := Round(GlobalToCanvasX(globalX));
end;

function TDrawingBoard.GlobalToCanvasYi(globalY: advancedFloat): integer;
begin
  result := Round(GlobalToCanvasY(globalY));
end;

procedure TDrawingBoard.FatBox(gx1, gy1, gx2, gy2, width: advancedFloat; color: TColor;
  bScreenWidth: boolean);
begin
  FatLine(gx1, gy1, gx2, gy1, width, color, bScreenWidth);
  FatLine(gx2, gy1, gx2, gy2, width, color, bScreenWidth);
  FatLine(gx2, gy2, gx1, gy2, width, color, bScreenWidth);
  FatLine(gx1, gy2, gx1, gy1, width, color, bScreenWidth);
// DebugFlip();

end;

procedure TDrawingBoard.FatLine(gx1, gy1, gx2, gy2, width: advancedFloat; color: TColor;
  bScreenWidth: boolean);
var
  rx1, ry1, rx2, ry2: integer;
  xs, ys: integer;
  t, i: integer;
  rpwx, rpwy: integer;
  rPercent: advancedFloat;
  steps: integer;
  tx, ty: integer;
begin
  // get advancedFloat coords for the line
  rx1 := GlobalToCanvasXi(gx1);
  ry1 := GlobalToCanvasYi(gy1);
  rx2 := GlobalToCanvasXi(gx2);
  ry2 := GlobalToCanvasYi(gy2);

  // determine advancedFloat point width
  if not bScreenWidth then begin
    rpwx := Round((GlobalToCanvasX(1) - GlobalToCanvasX(0)) * (width / 2));
    rpwy := Round((GlobalToCanvasY(1) - GlobalToCanvasY(0)) * (width / 2));
  end
  else begin
    rpwx := Round((width / 2) * AntiAliasing);
    rpwy := Round((width / 2) * AntiAliasing);
  end;

  // determine number of steps in the line

  // determine number of X steps
  xs := rx2 - rx1;
  ys := ry2 - ry1;

  // determine max steps
  steps := abs(xs);
  if abs(ys) > steps then
    steps := abs(ys);

  rPercent := 0;
  canvas.lock;
  try
    repeat
      canvas.Pen.color := color;
      canvas.Brush.color := color;
      tx := Round(rx1 + (rPercent * xs));
      ty := Round(ry1 + (rPercent * ys));

      self.DrawTo_pixels[tx, ty] := color;
      self.FatPoint(ScreenToGlobalX(tx), ScreenToGlobalY(ty), color, color,
        Round(width));
// self.Rectangle(ScreenToGlobalX(CanvasToScreenX(tx)),ScreenToGlobalX(CanvasToScreenX(ty)),ScreenToGlobalX(CanvasToScreenX(tx)),ScreenToGlobalX(CanvasToScreenX(ty)), color, true);
      // DEbugFlip();

      if steps = 0 then begin
        rPercent := 1;
      end
      else begin
        rPercent := rPercent + (1 / steps);
      end;

    until (rPercent >= 1);
  finally
    canvas.unlock;
  end;

end;

procedure TDrawingBoard.LowResLine(gx1, gy1, gx2, gy2: advancedFloat; color: TColor;
  bDotted: boolean);
var
  rx1, ry1, rx2, ry2: integer;
  xs, ys: integer;
  t, i: integer;
  rpwx, rpwy: integer;
  rPercent: advancedFloat;
  steps: integer;
  tx, ty: integer;
begin
  // get advancedFloat coords for the line
  rx1 := GlobalToCanvasXi(gx1);
  ry1 := GlobalToCanvasYi(gy1);
  rx2 := GlobalToCanvasXi(gx2);
  ry2 := GlobalToCanvasYi(gy2);

  // determine advancedFloat point width
  rpwx := Round((width / 2) * AntiAliasing);
  rpwy := Round((width / 2) * AntiAliasing);

  // determine number of steps in the line

  // determine number of X steps
  xs := rx2 - rx1;
  ys := ry2 - ry1;

  // determine max steps
  steps := abs(xs);
  if abs(ys) > steps then
    steps := abs(ys);

  rPercent := 0;
  canvas.lock;
  try
    repeat
      canvas.Pen.color := color;
      canvas.Brush.color := color;
      tx := Round(rx1 + (rPercent * xs));
      ty := Round(ry1 + (rPercent * ys));

// self.DrawTo_Pixels[tx,ty] := color;
      self.Rectangle(Round(ScreenToGlobalX(CanvasToScreenX(tx))),
        Round(ScreenToGlobalX(CanvasToScreenX(ty))),
        Round(ScreenToGlobalX(CanvasToScreenX(tx))),
        Round(ScreenToGlobalX(CanvasToScreenX(ty))), color, true);

      if steps = 0 then begin
        rPercent := 1;
      end
      else begin
        rPercent := rPercent + (1 / steps);
      end;

    until (rPercent >= 1);
  finally
    canvas.unlock;
  end;

  // DEbugFlip();

end;



procedure TDrawingBoard.MSG_advancedFloatMouseLDOWN(var msg: TMessage);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TDrawingBoard.MSG_advancedFloatMouseLUP(var msg: TMessage);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TDrawingBoard.MSG_advancedFloatMouseMove(var msg: TMessage);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TDrawingBoard.MSG_advancedFloatMouseRDOWN(var msg: TMessage);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TDrawingBoard.MSG_advancedFloatMouseRUP(var msg: TMessage);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;


function TDrawingBoard.GetBottomMargin: advancedFloat;
begin
  result := FBottomMargin;
end;

function TDrawingBoard.GetCenterX: advancedFloat;
begin
  result := ((BoundX2 - BoundX1) / 2) + BoundX1;

end;

function TDrawingBoard.GetCenterY: advancedFloat;
begin
  result := ((BoundY2 - BoundY1) / 2) + BoundY1;
end;

function TDrawingBoard.GetLeftMargin: advancedFloat;
begin
  result := FLeftMargin;
end;

function TDrawingBoard.GetrightMargin: advancedFloat;
begin
  result := FrightMargin;
end;

function TDrawingBoard.GetTextColor: TColor;
begin
  result := FTextColor;
end;

function TDrawingBoard.GetTextHelper: TCanvas;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TDrawingBoard.GetTopMargin: advancedFloat;
begin
  result := FTopMargin;
end;

procedure TDrawingBoard.SetBottomMargin(const Value: advancedFloat);
begin
  FBottomMargin := Value;
end;

procedure TDrawingBoard.SetLastMouseposition(x, y: integer);
begin
  FMouseX := x;
  FMouseY := y;
  if Assigned(OnadvancedFloatMouse) then begin
    OnadvancedFloatMouse(ScreenToGlobalX(FMouseX), ScreenToGlobalY(FMouseY), 0, false);
  end;
end;

procedure TDrawingBoard.SetLeftMargin(const Value: advancedFloat);
begin
  FLeftMargin := Value;
end;

procedure TDrawingBoard.SetrightMargin(const Value: advancedFloat);
begin
  FrightMargin := Value;
end;

procedure TDrawingBoard.SetTextBackgroundcolor(const Value: TColor);
begin
  FTextBackgroundColor := Value;

end;

procedure TDrawingBoard.SetTextcolor(const Value: TColor);
begin
  FTextColor := Value;
  TextHelper.font.color := Value;
end;

procedure TDrawingBoard.SetTopMargin(const Value: advancedFloat);
begin
  FTopMargin := Value;
end;



procedure TDrawingBoard.FatCircle(x1, y1, radius: advancedFloat; width: advancedFloat;
  color, FillColor: TColor; Fill: boolean);
begin

  FatPartialCircle(x1, y1, radius, 0, 1, width, color, FillColor, Fill);

end;

procedure TDrawingBoard.FatPartialCircle(globalX, globalY, global_radius: advancedFloat;
  rStart, rEnd: advancedFloat; width: advancedFloat; color, FillColor: TColor; Fill: boolean);
var
  t: integer;
  x1, y1, radius, xx1, xx2, yy1, yy2: advancedFloat;
CONST
  PI = 3.141597;
  SEGMENTS = 90;
begin
// x1 := GlobalToScreenX(globalx);
// y1 := GlobalToScreenY(globaly);
// radius := ScaleGlobalXToScreen(global_radius);
  x1 := globalX;
  y1 := globalY;
  radius := global_radius;

  if Fill then
    for t := Round(rStart * SEGMENTS) to Round(rEnd * SEGMENTS) do begin
      xx1 := x1 + (radius * (Sin((2 * PI) * (t / SEGMENTS))));
      yy1 := y1 - (radius * (Cos((2 * PI) * (t / SEGMENTS))));
      xx2 := x1 + (radius * (Sin((2 * PI) * ((t + 1) / SEGMENTS))));
      yy2 := y1 - (radius * (Cos((2 * PI) * ((t + 1) / SEGMENTS))));
// FatLine(x1,y1,xx1,yy1,GreaterOf(xx2-xx1,yy2-yy1), fillcolor, false);
      Triangle(x1, y1, xx1, yy1, xx2, yy2, FillColor, FillColor, true);
// Flip;
    end;

  if FillColor <> color then
    for t := Round(rStart * SEGMENTS) to Round(rEnd * SEGMENTS) do begin
      xx1 := x1 + (radius * (Sin((2 * PI) * (t / SEGMENTS))));
      yy1 := y1 - (radius * (Cos((2 * PI) * (t / SEGMENTS))));
      xx2 := x1 + (radius * (Sin((2 * PI) * ((t + 1) / SEGMENTS))));
      yy2 := y1 - (radius * (Cos((2 * PI) * ((t + 1) / SEGMENTS))));
      FatLine(xx1, yy1, xx2, yy2, width, color, false);
    end;

end;

procedure TDrawingBoard.Triangle(x1, y1, x2, y2, x3, y3: advancedFloat;
  color, FillColor: TColor; bFill: boolean);
var
  xx1, xx2, xx3, yy1, yy2, yy3: advancedFloat;
  b1, b2, b3: boolean;
  bb1, bb2, bb3: boolean;
  bx1, by1, bx2, by2: integer;
  highy, lowy: advancedFloat;
  x, y: integer;
  c, cCenter: TColor;
  cxx, cyy: advancedFloat;
  flops, centerflops: integer;
const
  c1 = $FF0000;
  c2 = $00FF00;
  c3 = $0000FF;
begin
  // determine extents

  bx1 := GlobalToCanvasXi(LesserOf(x1, LesserOf(x2, x3)));
  by1 := GlobalToCanvasYi(LesserOf(y1, LesserOf(y2, y3)));
  bx2 := GlobalToCanvasXi(GreaterOf(x1, GreaterOf(x2, x3)));
  by2 := GlobalToCanvasYi(GreaterOf(y1, GreaterOf(y2, y3)));

  xx1 := x1;
  xx2 := x2;
  xx3 := x3;
  yy1 := y1;
  yy2 := y2;
  yy3 := y3;

  x1 := GlobalToCanvasXi(x1);
  y1 := GlobalToCanvasYi(y1);
  x2 := GlobalToCanvasXi(x2);
  y2 := GlobalToCanvasYi(y2);
  x3 := GlobalToCanvasXi(x3);
  y3 := GlobalToCanvasYi(y3);

  GetCenterOfTriangle(x1, y1, x2, y2, x3, y3, cxx, cyy);
  centerflops := GetTriangleFlops(cxx, cyy, x1, y1, x2, y2, x3, y3);

  cCenter := -1;

// bx1 := 0;
// by1 := 0;
// bx2 := (width*antialiasing)-1;
// by2 := (height*antialiasing)-1;
  // 2d for loop
  if bFill then
    for y := by1 to by2 do begin
      for x := bx1 to bx2 do begin
        if x > (self.width * AntiAliasing) - 1 then
          continue;
        if y > (self.height * AntiAliasing) - 1 then
          continue;

        flops := GetTriangleFlops(x, y, x1, y1, x2, y2, x3, y3);

        if flops = centerflops then
          DrawTo_pixels[x, y] := FillColor;

// if GetPointToPointDistance(x,y,cxx,cyy) < 10 then
// pixels[x,y] := clLime;

      end;
    end;

  if bFill and (FillColor <> color) then begin
    FatLine(xx1, yy1, xx2, yy2, 1, color, true);
    FatLine(xx1, yy1, xx3, yy3, 1, color, true);
    FatLine(xx2, yy2, xx3, yy3, 1, color, true);
  end;

end;


function TDrawingBoard.CanvasToScreenX(canvasX: advancedFloat): advancedFloat;
begin
  result := canvasX / AntiAliasing;
end;

function TDrawingBoard.ScreenToCanvasX(screenX: advancedFloat): advancedFloat;
begin
  result := screenX * AntiAliasing;
end;

function TDrawingBoard.ScreenToCanvasY(screenY: advancedFloat): advancedFloat;
begin
  result := screenY * AntiAliasing;
end;

function TDrawingBoard.ScaleScreenToCanvas(x: advancedFloat): integer;
begin
  result := Round(x * AntiAliasing);

end;

{ TFastBackBufferedControl }

function ColorBlend(cBackGround, cForeGround: TGiantcolor;
  alpha: advancedFloat): TGiantcolor;
begin
  if alpha > 1 then
    alpha := 1;

  if alpha < 0 then
    alpha := 0;

  cBackGround := cBackGround * (1 - alpha);
  cForeGround := cForeGround * (alpha);

  result := cBackGround + cForeGround;

end;

function ColorBlendRGBA(cBackGround, cForeGround: TGiantcolor;
  alpha: advancedFloat): TGiantcolor; overload;
begin
  if alpha > 1 then
    alpha := 1;

  if alpha < 0 then
    alpha := 0;


  cBackGround := cBackGround * (1 - alpha);
  cForeGround := cForeGround * (alpha);

  result := cBackGround + cForeGround;
end;

function ColorAdd(cBackGround, cForeGround: TColor; alpha: advancedFloat): TColor;
  overload;
var
  r1, g1, b1, r2, g2, b2: integer;
  r, g, b: integer;
begin
  r1 := (cBackGround shr 0) and 255;
  g1 := (cBackGround shr 8) and 255;
  b1 := (cBackGround shr 16) and 255;

  r2 := (cForeGround shr 0) and 255;
  g2 := (cForeGround shr 8) and 255;
  b2 := (cForeGround shr 16) and 255;

  r := Round((r2 * alpha) + r1);
  g := Round((g2 * alpha) + g1);
  b := Round((b2 * alpha) + b1);
  if r > 255 then
    r := 255;
  if g > 255 then
    g := 255;
  if b > 255 then
    b := 255;

  result := (b shl 16) + (g shl 8) + r;

end;

function Colorsubtract(cBackGround, cForeGround: TColor; alpha: advancedFloat): TColor;
  overload;
var
  r1, g1, b1, r2, g2, b2: integer;
  r, g, b: integer;
begin
  r1 := (cBackGround shr 0) and 255;
  g1 := (cBackGround shr 8) and 255;
  b1 := (cBackGround shr 16) and 255;

  r2 := (cForeGround shr 0) and 255;
  g2 := (cForeGround shr 8) and 255;
  b2 := (cForeGround shr 16) and 255;

  r := Round(r1-(r2 * alpha));
  g := Round(g1-(g2 * alpha));
  b := Round(b1-(b2 * alpha));
  if r < 0 then
    r := 0;
  if g < 0 then
    g := 0;
  if b < 0 then
    b := 0;

  result := (b shl 16) + (g shl 8) + r;

end;


function ColorBlend(cBackGround, cForeGround: TColor; alpha: advancedFloat): TColor;
{$IFDEF USE_SSE3}
asm
   err
end;
{$ELSE}
var
  r1, g1, b1, r2, g2, b2: nativeint;
  r, g, b: nativeint;
begin
  r1 := (cBackGround shr 0) and 255;
  g1 := (cBackGround shr 8) and 255;
  b1 := (cBackGround shr 16) and 255;

  r2 := (cForeGround shr 0) and 255;
  g2 := (cForeGround shr 8) and 255;
  b2 := (cForeGround shr 16) and 255;

  r := Round(((r2 - r1) * alpha) + r1);
  g := Round(((g2 - g1) * alpha) + g1);
  b := Round(((b2 - b1) * alpha) + b1);

  result := (b shl 16) + (g shl 8) + r;

end;
{$ENDIF USE_SSE2}

function ColorBlendRGBA(cBackGround, cForeGround: TColor; alpha: advancedFloat): TColor;
var
  r1, g1, b1, r2, g2, b2,a1,a2: nativeint;
  r, g, b,a: nativeint;
begin
  r1 := (cBackGround shr 0) and 255;
  g1 := (cBackGround shr 8) and 255;
  b1 := (cBackGround shr 16) and 255;
  a1 := (cBackground shr 24) and 255;

  r2 := (cForeGround shr 0) and 255;
  g2 := (cForeGround shr 8) and 255;
  b2 := (cForeGround shr 16) and 255;
  a2 := (cForeGround shr 24) and 255;


  r := Round(((r2 - r1) * alpha) + r1);
  g := Round(((g2 - g1) * alpha) + g1);
  b := Round(((b2 - b1) * alpha) + b1);
  a := Round(((a2 - a1) * alpha) + a1);

  result := (a shl 24)+(b shl 16) + (g shl 8) + r;
end;
function ColorBlend_ForegroundSourceAlpha(cBackGround, cForeGround: TColor; alpha: advancedFloat): TColor;
{$IFDEF USE_SSE3}
asm
   err
end;
{$ELSE}
var
  r1, g1, b1, r2, g2, b2,a1,a2: nativeint;
  r, g, b,a: nativeint;
begin
  r1 := (cBackGround shr 0) and 255;
  g1 := (cBackGround shr 8) and 255;
  b1 := (cBackGround shr 16) and 255;

  r2 := (cForeGround shr 0) and 255;
  g2 := (cForeGround shr 8) and 255;
  b2 := (cForeGround shr 16) and 255;
  a2 := (cForeGround shr 24) and 255;
  alpha := (a2/255) * alpha;

  r := Round(((r2 - r1) * alpha) + r1);
  g := Round(((g2 - g1) * alpha) + g1);
  b := Round(((b2 - b1) * alpha) + b1);

  result := (b shl 16) + (g shl 8) + r;

end;
{$ENDIF USE_SSE2}

function ColorOp(cBackGround, cForeGround: TColor; alpha: advancedFloat;
  AlphaOp: TAlphaOp): TColor;
begin

  case AlphaOp of
    aoAdd: begin
        result := colorblending.ColorAdd(cBackGround, cForeGround, alpha);
      end;
    aoSubtract: begin
        result := colorblending.ColorSubtract(cBackGround, cForeGround, alpha);
      end;
    aoNone: begin
        result := cForeGround;
      end;
  else
    result := clBlack;
  end;
end;

function ColorMultiply(c: TColor; c2: TColor): TColor;
var
  cc1,cc2,cc3: TRGBNativeFloatColor;
begin
  cc1.FromColor(c);
  cc2.FromColor(c2);
  cc3 := cc1*cc2;
  result := cc3.ToColor;


end;


end.


