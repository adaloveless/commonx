unit ColorBlending;


interface

uses
{$IFNDEF FMX}
  graphics,
{$ENDIF}
  colorconversion, graphicsx;

function ColorBlend(cBackGround, cForeGround: TGiantcolor;  alpha: advancedFloat): TGiantcolor; overload;
function ColorAdd(cBackGround, cForeGround: TColor; alpha: advancedFloat): TColor;
  overload;
function ColorSubtract(cBackGround, cForeGround: TColor;
  alpha: advancedFloat): TColor; overload;
function ColorBlend(cBackGround, cForeGround: TColor; alpha: advancedFloat): TColor;overload;
function ColorBlend_ForegroundSourceAlpha(cBackGround, cForeGround: TColor; alpha: advancedFloat): TColor;
function ColorBlend_PreserveSourceAlpha(cBackGround, cForeGround: TColor; alpha: advancedFloat): TColor;
function ColorBlendRGBA(cBackGround, cForeGround: TColor; alpha: advancedFloat): TColor;inline;overload;
function ColorOp(cBackGround, cForeGround: TColor; alpha: advancedFloat;
  AlphaOp: TAlphaOp): TColor;inline;

function ColorReverse(c: TColor): TColor;inline;
function ColorAlpha(c: TColor; rAlpha: advancedFloat): TColor;inline;
function ColorMultiply(c: TColor; c2: TColor): TColor;inline;

implementation

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

function ColorBlend_PreserveSourceAlpha(cBackGround, cForeGround: TColor; alpha: advancedFloat): TColor;
var
  r1, g1, b1, r2, g2, b2,a1: nativeint;
  r, g, b,a: nativeint;
begin
  r1 := (cBackGround shr 0) and 255;
  g1 := (cBackGround shr 8) and 255;
  b1 := (cBackGround shr 16) and 255;
  a1 := (cBackGround shr 24) and 255;

  r2 := (cForeGround shr 0) and 255;
  g2 := (cForeGround shr 8) and 255;
  b2 := (cForeGround shr 16) and 255;
//  a2 := (cForeGround shr 24) and 255;
//  alpha := (a1/255) * alpha;

  r := Round(((r2 - r1) * alpha) + r1);
  g := Round(((g2 - g1) * alpha) + g1);
  b := Round(((b2 - b1) * alpha) + b1);
  a := a1;

  result := (a shl 24)+(b shl 16) + (g shl 8) + r;

end;

function ColorBlend_ForegroundSourceAlpha(cBackGround, cForeGround: TColor; alpha: advancedFloat): TColor;
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


function ColorOp(cBackGround, cForeGround: TColor; alpha: advancedFloat;
  AlphaOp: TAlphaOp): TColor;
begin

  case AlphaOp of
    aoAdd: begin
        result := ColorAdd(cBackGround, cForeGround, alpha);
      end;
    aoSubtract: begin
        result := ColorSubtract(cBackGround, cForeGround, alpha);
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


function ColorReverse(c: TColor): TColor;
var
  p: pbyte;
  r: byte;
begin
  result := c;
  p := Pbyte(@result);
  r :=   p[0];
  p[0] := p[2];
  p[2] := r;


end;

function ColorAlpha(c: TColor; rAlpha: advancedFloat): TColor;
var
  p: pbyte;
begin
  result := c;
  p := Pbyte(@result);
  p[3] := round(rAlpha * 255);

end;

end.
