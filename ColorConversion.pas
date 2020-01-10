unit ColorConversion;
{$IFDEF DELPHI2005}{$INLINE AUTO}{$ENDIF}

interface

uses
  graphicsx,
{$IFDEF FMX}
  types,
  uitypes,
{$ELSE}
  vcl.Graphics,
{$ENDIF}
typex, validation, sysutils, numbers, debug;



type
  advancedfloat = double;
  TAlphaOp = (aoNone, aoSubtract, aoAdd, aoStandard, aoStandardVariable);

{$IFDEF FMX}
  TAlphaColorREcHelper = record helper for TAlphaColorRec
    procedure FromAlphaColor(ac: TAlphaColor);
    function ToAlphaColor: TAlphaColor;
  end;


  TColorREcHelper = record helper for TColorRec
    procedure FromAlphaColor(ac: TAlphaColor);
    function ToAlphaColor: TAlphaColor;
  end;
{$ENDIF}

  TSmallColor = packed record
    r,g,b,a: byte;
    procedure SetColor(c: TColor);

//    class operator Implicit(var a: TSmallColor): TColor;inline;
//    class operator Explicit(var a: TSmallColor): TColor;inline;
//    class operator Implicit(var a: TColor): TSmallColor;inline;
//    class operator Explicit(var a: TColor): TSmallColor;inline;
  end;

  TSmallRGB = packed record
    r,g,b: byte;
  end;

  PSmallRGB = ^TSmallRGB;

  TGiantColor = record
    r,g,b,a: nativefloat;

    class operator Implicit(var a: integer): TGiantColor;inline;
    class operator Explicit(var a: integer): TGiantColor;inline;
    class operator Implicit(var a: TGiantColor): integer;inline;
    class operator Explicit(var a: TGiantColor): integer;inline;
    class operator Implicit(var a: TGiantColor): TColor;inline;
    class operator Explicit(var a: TGiantColor): TColor;inline;
    class operator Implicit(var a: TGiantColor): TSmallColor;inline;
    class operator Explicit(var a: TGiantColor): TSmallColor;inline;
    class operator Implicit(var a: TGiantColor): byte;inline;
    class operator Explicit(var a: TGiantColor): byte;inline;

    class operator Add(a,b: TGiantColor): TGiantColor;inline;
    class operator Add(var a: TGiantColor; b: TColor): TGiantColor;inline;
    class operator Add(var a: TGiantColor; b: byte): TGiantColor;inline;
    class operator Divide(var a: TGiantColor; b: integer): TGiantColor;inline;
    class operator Multiply(var a: TGiantColor; b: nativefloat): TGiantColor;inline;

    function ToColor: TColor;
    function ToColorWithAlpha: TColor;
    function ToColorAdditiveMultiplyAlpha: TColor;
    procedure FromColor(c: TColor);

{$IFDEF FMX}
    function ToAlphaColor: TAlphaColor;
    procedure FromAlphaColor(c: TAlphaColor);
{$ENDIF}


    procedure SetAlpha(aa: byte);
    procedure Init;
  end;


  TnativefloatColor = TGiantColor;
  PnativefloatColor = ^TGiantColor;





  TColorFloat = nativefloat; //double, nativefloat also acceptable

  TRGBnativefloatColor = record
    r,g,b: TColorFloat;
    class operator Multiply(c1,c2: TRGBNativeFloatColor): TRGBNativeFloatColor;
    class operator Add(c1, c2: TRGBNativeFloatColor): TRGBNativeFloatColor;
    class operator Divide(c1: TRGBNativefloatColor; nf: nativefloat): TRGBNAtivefloatColor;
    class operator implicit(c: TColor): TRGBNativefloatColor;
    procedure FromColor(c: TColor);
    function ToColor: TColor;
    procedure Normalize;

  end;

  THSLnativefloatColor = record
    h,s,l: TColorFloat;
    procedure FromRGB(rgb: TRGBNativeFloatColor);
    procedure FromColor(c: TColor);
    function ToRGB: TRGBNativeFloatColor;
    function ToColor: TColor;
    function Compare(hsl: THSLNativeFloatColor; bAbs: boolean = true): THSLNativefloatCOlor;
  end;

  TPackedColor24 = packed record
    a,r,g,b: byte;
  end;

  TPackedColorRGBA = packed record
    r,g,b,a: byte;
    class operator Subtract(a,b: TPackedColorRGBA): TPackedColorRGBA;
  end;

  TPackedColorRGBX = packed record
    r,g,b,x: byte;
    class operator Subtract(a,b: TPackedColorRGBX): TPackedColorRGBX;
    function AbsoluteDifference(b: TPackedColorRGBX): integer;
  end;

  PPackedColorRGBX = ^TPackedColorRGBX;


function ColorToNativeFloatRGB(color: TColor): TRGBnativefloatColor;inline;
function NativeFloatRGBToColor(rgb: TRGBnativefloatColor): TColor;inline;

function RGBtoHSL(rgb: TRGBnativefloatColor): THSLnativefloatColor;inline;
function HSLtoRGB(hsl: THSLnativefloatColor): TRGBnativefloatColor;

function Min(a,b,c: TColorFloat): TColorFloat;overload;inline;
function Max(a,b,c: TColorFloat): TColorFloat;overload;inline;

function BGRA_2222_ToColor(bIn: byte): TColor;

function HexStringToColor(sHex: string): TColor;
function normalizeColor(c: TColor): TColor;
function GiantColor_New(r,g,b,a: nativefloat): TGiantColor;

implementation


function Min(a,b,c: TColorFloat): TColorFloat;
begin
  result := a;

  if b<result  then result := b;
  if c<result then result := c;

end;


function Max(a,b,c: TColorFloat): TColorFloat;
begin
  result := a;

  if b>result  then result := b;
  if c>result then result := c;

end;

function BGRA_2222_ToColor(bIn: byte): TColor;
var
  a,r,g,b: nativeint;
begin
  a := (bIn and 3) ;
  r := (bIn and (3 shl 2)) shr 2;
  g := (bIn and (3 shl 4)) shr 4;
  b := (bIn and (3 shl 8)) shr 8;
  a := a * 255;
  r := r * 255;
  g := g * 255;
  b := b * 255;

  result := (a shl 24) or (b shl 16) or (g shl 8) or (r shl 0);



end;


function RGBtoHSL(rgb: TRGBnativefloatColor): THSLnativefloatColor;
var
  rmin, rmax, rdel: nativefloat;
  delr, delg, delb: nativefloat;
begin
  rmin := min(rgb.r, rgb.g, rgb.b);
  rmax := max(rgb.r, rgb.g, rgb.b);
  rdel := rmax-rmin;

  result.l := (rmax+rmin)/2;
  if rdel = 0 then begin
    result.h := 0;
    result.s := 0;
  end else begin
    if result.l < 1.0 then
      result.s := rdel / (rMax + rMin)
    else
      result.s := rdel / (2 - rdel);

    delr := ( ( (rmax - rgb.r) / 6 ) + ( rdel / 2 ) ) / rdel;
    delg := ( ( (rmax - rgb.g) / 6 ) + ( rdel / 2 ) ) / rdel;
    delb := ( ( (rmax - rgb.b) / 6 ) + ( rdel / 2 ) ) / rdel;

    if rgb.r = rmax then
      result.h := delb-delg
    else
    if rgb.g = rmax then
      result.h := (1/3)+delr-delb
    else
    if rgb.b = rmax then
      result.h := (2/3) + delg-delr;

    if result.h < 0 then
      result.h := result.h+1;

    if result.h > 1 then
      result.h := result.h-1;


    if result.s > 1 then
      result.s := 1;

    if result.l > 1 then
      result.l := 1;






  end;

end;


function HSLtoRGB(hsl: THSLnativefloatColor): TRGBnativefloatColor;
  function HueToRGB(v1, v2, vh: nativefloat): nativefloat;
  begin
    result := 0;

    if vh < 0 then
      vh := vh + 1;
    if vh > 1 then
      vh := vh -1;

    if ((6*vh)<1) then
      result := v1+(v2-v1)*6*vh
    else
    if ((2*vh)<1) then
      result := v2
    else
    if ((3 *vh) < 2) then
      result := v1+(v2-v1)*((2/3)-vh)*6
    else
      result := v1;


  end;
var
  var1, var2, var3: nativefloat;
begin
  if hsl.s = 0 then begin
    result.r := hsl.l;
    result.g := result.r;
    result.b := result.r;
  end else begin
    if hsl.l < 1.0 then
      var2 := hsl.l * (1+hsl.s)
    else
      var2 := (hsl.l+hsl.s)-(hsl.s*hsl.l);

    var1 := 2 * hsl.l-var2;

    result.r := HueToRGB(var1, var2, hsl.h+(1/3));
    result.g := HueToRGB(var1, var2, hsl.h);
    result.b := HueToRGB(var1, var2, hsl.h - (1/3));


  end;

end;
function ColorTonativefloatRGB(color: TColor): TRGBnativefloatColor;
begin
  result.r := (color and 255) / 255;
  result.g := ((color shr 8) and 255) / 255;
  result.b := ((color shr 16) and 255) / 255;

end;

function nativefloatRGBToColor(rgb: TRGBnativefloatColor): TColor;
var
  r,g,b: byte;
begin
  r:= round(rgb.r *255);
  g:= round(rgb.g *255);
  b:= round(rgb.b *255);

  result := r+(g shl 8)+(b shl 16);

end;





{ TPackedColorRGBA }

class operator TPackedColorRGBA.Subtract(a,
  b: TPackedColorRGBA): TPackedColorRGBA;
var
  aa,rr,gg,bb: integer;
begin
  aa := a.a-b.a;
  rr := a.r-b.r;
  gg := a.g-b.g;
  bb := a.b-b.b;

  if aa < 0 then aa := 0;
  if rr < 0 then rr := 0;
  if gg < 0 then gg := 0;
  if bb < 0 then bb := 0;

  result.a := aa;
  result.r := rr;
  result.g := gg;
  result.b := bb;




end;

{ TPackedColorRGBX }

function TPackedColorRGBX.AbsoluteDifference(b: TPackedColorRGBX): integer;
//Returns sum of RGB value differences
//b: is the color to compare against
var
  aa,rr,gg,bb: integer;
begin
  rr := self.r-b.r;
  gg := self.g-b.g;
  bb := self.b-b.b;

  if rr < 0 then rr := 0-rr;
  if gg < 0 then gg := 0-gg;
  if bb < 0 then bb := 0-bb;

  result := rr+gg+bb;

end;

class operator TPackedColorRGBX.Subtract(a,
  b: TPackedColorRGBX): TPackedColorRGBX;
var
  aa,rr,gg,bb: integer;
begin
  rr := a.r-b.r;
  gg := a.g-b.g;
  bb := a.b-b.b;

  if rr < 0 then rr := 0;
  if gg < 0 then gg := 0;
  if bb < 0 then bb := 0;

  result.x := 0;
  result.r := rr;
  result.g := gg;
  result.b := bb;

end;

{ TRGBnativefloatColor }

class operator TRGBnativefloatColor.Add(c1,
  c2: TRGBNativeFloatColor): TRGBNativeFloatColor;
begin
  result.r := c1.r+c2.r;
  result.g := c1.g+c2.g;
  result.b := c1.b+c2.b;

end;

class operator TRGBnativefloatColor.Divide(c1: TRGBNativefloatColor;
  nf: nativefloat): TRGBNAtivefloatColor;
begin
  result.r := c1.r/nf;
  result.g := c1.g/nf;
  result.b := c1.b/nf;
end;

procedure TRGBnativefloatColor.FromColor(c: TColor);
begin
  r := (c and 255) / 255;
  g := ((c shr 8) and 255)/ 255;
  b := ((c shr 16) and 255)/ 255;
end;

class operator TRGBnativefloatColor.implicit(c: TColor): TRGBNativefloatColor;
begin
  result.FromColor(c);
end;

class operator TRGBnativefloatColor.Multiply(c1,
  c2: TRGBNativeFloatColor): TRGBNativeFloatColor;
begin
  result.R := c1.R * c2.R;
  result.G := c1.G * c2.G;
  result.B := c1.B * c2.B;
end;

procedure TRGBnativefloatColor.Normalize;
var
  rdiv: nativefloat;
begin
  rdiv := greaterof(r,greaterof(g,b));
  if rdiv > 0 then begin
    r := r / rdiv;
    g := g / rdiv;
    b := b / rdiv;
  end;


end;

function TRGBnativefloatColor.ToColor: TColor;
begin
  if r > 1 then r := 1;
  if g > 1 then g := 1;
  if b > 1 then b := 1;

  result := round(r * 255) + (round(g * 255) shl 8) + (round(b * 255) shl 16);

end;

{ THSLnativefloatColor }


{ THSLnativefloatColor }


function THSLnativefloatColor.Compare(hsl: THSLNativeFloatColor;
  bAbs: boolean): THSLNativefloatCOlor;
begin
  result.h := self.h - hsl.h;
  result.s := self.s - hsl.s;
  result.l := self.l - hsl.l;
  if bAbs then begin
    result.h := abs(result.h);
    result.s := abs(result.s);
    result.l := abs(result.l);

  end;
end;

procedure THSLnativefloatColor.FromColor(c: TColor);
var
  rgb: TRGBnativefloatColor;
begin
  rgb.FromColor(c);
  self.FromRGB(rgb);

end;

procedure THSLnativefloatColor.FromRGB(rgb: TRGBNativeFloatColor);
var
  h: THSLNativeFloatColor;
begin
  h := RGBtoHSL(rgb);

  self.h := h.h;
  self.s := h.s;
  self.l := h.l;


end;

function THSLnativefloatColor.ToColor: TColor;
begin
  result := HSLtoRGB(self).ToColor;
end;

function THSLnativefloatColor.ToRGB: TRGBNativeFloatColor;
begin
  result := HSLtoRGB(self);
end;

function HexStringToColor(sHex: string): TColor;
begin
  sHex := RestricttoHex(sHex);
  if sHex = '' then
    result := 0
  else
    result := strtoint('$'+sHex);

end;

function normalizeColor(c: TColor): TColor;
var
  cc: TRGBnativefloatColor;
begin
  cc.FromColor(c);
  cc.Normalize;
  result := cc.ToColor;


end;


procedure TSmallColor.SetColor(c: TColor);
begin
  r := (c shr 0) and 255;
  g := (c shr 8) and 255;
  b := (c shr 16) and 255;

end;


class operator TGiantColor.Add(a, b: TGiantColor): TGiantColor;
begin
  result.r := a.r+b.r;
  result.g := a.g+b.g;
  result.b := a.b+b.b;
  result.a := a.a+b.a;

end;

class operator TGiantColor.Add(var a: TGiantColor; b: TColor): TGiantColor;
begin
  result.r := a.r+((b and 255)/255);
  result.g := a.g+(((b shr 8) and 255)/255);
  result.b := a.b+(((b shr 16) and 255)/255);
end;

class operator TGiantColor.Add(var a: TGiantColor; b: byte): TGiantColor;
begin
  inherited;
  a.a := a.a+(b/255);

end;

class operator TGiantColor.Divide(var a: TGiantColor; b: integer): TGiantColor;
begin
  result.r := a.r / b;
  result.g := a.g / b;
  result.b := a.b / b;
  result.a := a.a / b;
end;

class operator TGiantColor.Multiply(var a: TGiantColor; b: nativefloat): TGiantColor;
begin
  result.r := a.r * b;
  result.g := a.g * b;
  result.b := a.b * b;
  result.a := a.a * b;

end;

function GiantColor_New(r, g, b, a: nativefloat): TGiantColor;
begin
  result.r := r;
  result.g := g;
  result.b := b;
  result.a := a;

end;

class operator TGiantColor.Explicit(var a: TGiantColor): integer;
begin
  result := round(a.b*255)+(round(a.b*255) shl 8)+(round(a.b*255) shl 16);
end;

class operator TGiantColor.Implicit(var a: TGiantColor): TSmallColor;
begin
  result.r := round(a.r*255);
  result.g := round(a.g*255);
  result.b := round(a.b*255);
end;
class operator TGiantColor.Explicit(var a: TGiantColor): TSmallColor;
begin
  result.r := round(a.r*255);
  result.g := round(a.g*255);
  result.b := round(a.b*255);
end;


class operator TGiantColor.Implicit(var a: TGiantColor): integer;
begin
  result := round(a.r*255)+(round(a.g*255) shl 8)+(round(a.g*255) shl 16);
end;

class operator TGiantColor.Implicit(var a: TGiantColor): byte;
begin
  result := round(a.a*255);
end;

class operator TGiantColor.Implicit(var a: TGiantColor): TColor;
begin
  result := round(a.r*255)+(round(a.g * 255) shl 8)+(round(a.b * 255) shl 16);
end;

procedure TGiantColor.SetAlpha(aa: byte);
begin
  a := aa/255;
end;

{$IFDEF FMX}
function TGiantColor.ToAlphaColor: TAlphaColor;
var
  rr,gg,bb,aa: integer;
begin
  rr := round(r * 255);
  gg := round(g * 255);
  bb := round(b * 255);
  aa := round(a * 255);
  if rr > 255 then rr := 255;
  if gg > 255 then gg := 255;
  if bb > 255 then bb := 255;
  if aa > 255 then aa := 255;

  result := (bb+(gg shl 8)+(rr shl 16)+(aa shl 24));
end;
{$ENDIF}

function TGiantColor.ToColor: TColor;
var
  rr,gg,bb: integer;
begin
  rr := clamp(round(r * 255),0,255);
  gg := clamp(round(g * 255),0,255);
  bb := clamp(round(b * 255),0,255);

  result := (rr)+((gg) shl 8)+(((bb) shl 16));
end;

function TGiantColor.ToColorAdditiveMultiplyAlpha: TColor;
var
  rr,gg,bb: integer;
begin

  rr := clamp(round((r*a) * 255),0,255);
  gg := clamp(round((g*a) * 255),0,255);
  bb := clamp(round((b*a) * 255),0,255);

  result := (rr)+((gg) shl 8)+(((bb) shl 16));
end;

function TGiantColor.ToColorWithAlpha: TColor;
var
  rr,gg,bb,aa: integer;
begin
  aa := clamp(round(a * 255),0,255);
  rr := clamp(round(r * 255),0,255);
  gg := clamp(round(g * 255),0,255);
  bb := clamp(round(b * 255),0,255);
  result := (rr)+((gg) shl 8)+(((bb) shl 16))+(aa shl 24);
end;

class operator TGiantColor.Explicit(var a: TGiantColor): byte;
begin
  result := round(a.a*255);
end;

{$IFDEF FMX}
procedure TGiantColor.FromAlphaColor(c: TAlphaColor);
begin
  b := ((c shr 0) and 255)/255;
  g := ((c shr 8) and 255)/255;
  r := ((c shr 16) and 255)/255;
  a := ((c shr 24) and 255)/255;

end;
{$ENDIF}

procedure TGiantColor.FromColor(c: TColor);
begin
  r := ((c shr 0) and 255)/255;
  g := ((c shr 8) and 255)/255;
  b := ((c shr 16) and 255)/255;
end;

class operator TGiantColor.Explicit(var a: TGiantColor): TColor;
begin
  result := round(a.r*255)+(round(a.g * 256))+(round(a.b * 65536));
end;

class operator TGiantColor.Implicit(var a: integer): TGiantColor;
begin
  result.r := (a shr 0) and 255;
  result.g := (a shr 8) and 255;
  result.b := (a shr 16) and 255;

end;
class operator TGiantColor.Explicit(var a: integer): TGiantColor;
begin
  result.r := (a shr 0) and 255;
  result.g := (a shr 8) and 255;
  result.b := (a shr 16) and 255;

end;


procedure TGiantColor.Init;
begin
  Fillchar(self, sizeof(self), 0);
  a := 1.0;
end;








{ TColorRec }



{ TColorREcHelper }

{$IFDEF FMX}
procedure TAlphaColorREcHelper.FromAlphaColor(ac: TAlphaColor);
begin
  R := ac and $ff;
  g := (ac and $ff00) shr 8;
  B := (ac and $ff0000) shr 16;
  a := (ac and $ff000000) shr 24;
end;

function TAlphaColorREcHelper.ToAlphaColor: TAlphaColor;
begin
//  if a <> 255 then
//    Debug.Log('trap');
  result := r + (g shl 8) + (b shl 16) + (a shl 24);
end;

procedure TColorREcHelper.FromAlphaColor(ac: TAlphaColor);
begin
  R := ac and $ff;
  g := (ac and $ff00) shr 8;
  B := (ac and $ff0000) shr 16;
  a := (ac and $ff000000) shr 24;
end;

function TColorREcHelper.ToAlphaColor: TAlphaColor;
begin
  result := r + (g shl 8) + (b shl 16) + (a shl 24);
end;
{$ENDIF COLORREC}

end.
