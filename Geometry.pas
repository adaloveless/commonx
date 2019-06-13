unit Geometry;
{$INCLUDE DelphiDefs.inc}
interface

uses
  math,
  typex,
  numbers,
  sysutils,
  systemx,
  stringx,
  types, debug,
  classes;

type
  TAngleOrientation = (aoTraditional, aoRaster);

  Tpointhelper = record helper for Tpoint
    function Divide(scalar: ni): TPoint;
    function ToString: string;
    procedure FromString(s: string);
  end;

  TpointFhelper = record helper for TpointF
    function Divide(scalar: ni): TPoint;
    function ToString: string;
    procedure FromString(s: string);
  end;

  TRectHelper = record helper for TRect
    function ToString: string;
    procedure FromString(s: string);
  end;

  TRectFHelper = record helper for TRectF
    function ToString: string;
    procedure FromString(s: string);
  end;

  TnativefloatPoint = record
    x,y: nativefloat;
    class operator Equal(a,b: TNativefloatPoint): boolean;
    function AngleInUCs(ao: TAngleOrientation): nativefloat;
    function AngleInRads(ao: TAngleOrientation): nativefloat;
    function Magnitude: nativefloat;
    function Quadrant(ao: TAngleOrientation): nativeint;
    class operator Subtract(a,b: TNativefloatPoint): TNativefloatPoint;
    class function GetAngularInterpolated_Balance(a0, a1, aPos: TNativeFLoatPOint; out bal0, bal1: nativefloat): boolean;static;

    procedure FromFloats(x,y: nativefloat);
    function ToPOint: TPoint;
  end;

  TNativeVector4 = packed record
    x,y,z,w: single;

    class operator Equal(a,b: TNativeVector4): boolean;
    class operator Add(a,b: TNativeVector4): TNativeVector4;overload;
    class operator Subtract(a,b: TNativeVector4): TNativeVector4;
    class operator Multiply(a: TNativeVEctor4; b: nativefloat): TNativeVector4;
    class operator Add(a: TNativeVEctor4; b: nativefloat): TNativeVector4;overload;
    class operator Divide(a: TNativeVEctor4; b: nativefloat): TNativeVector4;
    function ToFloatPOint: TNativeFloatPOint;
    procedure FromFloatPOint(p: TNativeFloatPOint);
    procedure Init;
    function Magnitude: nativefloat;
  end;

  TNativeFloatRect = packed record
    x1,y1,x2,y2: nativefloat;
  end;

  TSingleRect = packed record
    x1,y1,x2,y2: nativefloat;
    function ToString: string;
  end;

function SharkToothCurve(rPercent0to1: nativefloat; rSharpnessExponent: nativefloat = 1.0): nativefloat;
function HerbivoreCurve(rPercent0to1: nativefloat; rSharpnessExponent: nativefloat = 1.0): nativefloat;

procedure GetCenterOfTriangle(x1,y1,x2,y2,x3,y3: single; out rx,ry: single);overload;
procedure GetCenterOfTriangle(x1,y1,x2,y2,x3,y3: double; out rx,ry: double);overload;
function GetTriangleFlops(x,y: nativefloat; x1,y1,x2,y2,x3,y3: nativefloat): integer;
function Interpolate_SIN(position,
                          iTargetStart,
                          iTargetEnd,
                          iSourceStart,
                          iSourceEnd: nativefloat): nativefloat;

function Interpolate_SIN_Mag(mag, position,
                          iTargetStart,
                          iTargetEnd,
                          iSourceStart,
                          iSourceEnd: nativefloat): nativefloat;

function Interpolate(position, iTargetStart, iTargetEnd, iSourceStart,
      iSourceEnd: single): single; overload;
function Interpolate(position, iTargetStart, iTargetEnd, iSourceStart,
      iSourceEnd: double): double; overload;
function Interpolate(position, steps, iSource, iTarget: nativefloat): nativefloat;overload;
function Interpolate(position, iSource, iTarget: single): single;overload;
function LinearToCos(position: single): single;

function InterpolateLog(position, steps, iSource, iTarget: nativefloat): nativefloat;overload;

function GetPointToPointDistance(x1,y1,x2,y2: nativefloat): nativefloat;
function GetIntersectedArea(ax1,ay1,ax2,ay2,bx1,by1,bx2,by2: nativefloat): nativefloat;
function GetIntersectedAreaDimensions(ax1,ay1,ax2,ay2,bx1,by1,bx2,by2: nativefloat): TNativefloatPoint;
function LinesIntersect(const x1,y1,x2,y2: nativefloat; { first line}
                         const x3,y3,x4,y4: nativefloat; { second line }
                         out x,y : nativefloat): boolean; { intersection point }

function IsIn(p: TPOint; r: TRect): boolean;
function Abs(p: TPOintF): TPOintF;inline;overload;
function Abs(i: int64): int64;inline;overload;
function Abs(i: integer): integer;inline;overload;
function Abs(s: single): single;inline;overload;
function Abs(d: double): double;inline;overload;

function RectMinusRectEdge(rectOuter, rectEdge: TRect): TRect;


implementation

procedure GetCenterOfTriangle(x1,y1,x2,y2,x3,y3: single; out rx,ry: single);
begin
  rx := ((((x1+x2)/2)+((x1+x3)/2)+((x2+x3)/2))/3);
  ry := ((((y1+y2)/2)+((y1+y3)/2)+((y2+y3)/2))/3);
end;

procedure GetCenterOfTriangle(x1,y1,x2,y2,x3,y3: double; out rx,ry: double);
begin
  rx := ((((x1+x2)/2)+((x1+x3)/2)+((x2+x3)/2))/3);
  ry := ((((y1+y2)/2)+((y1+y3)/2)+((y2+y3)/2))/3);
end;

function GetTriangleFlops(x,y: nativefloat; x1,y1,x2,y2,x3,y3: nativefloat): integer;
var
  b1,b2,b3,bb1,bb2,bb3: boolean;
  highy: nativefloat;

begin
  //for given point X,Y determine if it is within the triangle

  //interpolate along slope
  if x2 <> x1 then begin
     highy := Interpolate(x, y1,y2,x1,x2);
     b1 := y<highy;
     bb1 := y=highy;
  end else begin
     b1 := x<=x1;
     bb1 := x = round(x1);
  end;

  if x3 <> x1 then begin
    highy := Interpolate(x, y1,y3,x1,x3);
    b2 := y<highy;
    bb2 := y=highy;
  end else begin
    b2 := x>x3;
    bb2 := x = round(x2);
  end;

  if x3 <> x2 then begin
    highy := Interpolate(x, y2,y3,x2,x3);
    b3 := y<highy;
    bb3 := y=highy;
  end else begin
    b3 := x>x3;
    bb3 := x = round(x3);
  end;

  result := 0;
  if b1 then inc(result);
  if b2 then inc(result);
  if b3 then inc(result);
end;


//------------------------------------------------------------------------------
function Interpolate(position, iTargetStart,
  iTargetEnd, iSourceStart, iSourceEnd: double): double;

var
  iTargetWidth, iSourceWidth: double;
  rPercent: double;
  rMoveMent: double;
begin
  iSourceWidth := iSourceEnd-(iSourceStart);
  iTargetWidth := iTargetEnd-(iTargetStart);

  if iSourceWidth = 0 then
    result := 0
  else begin
    rPercent := (position - iSourceStart)/(iSourceEnd-iSourceStart);
    rMovement := rPercent*iTargetWidth;
    result := rMoveMent + iTargetStart;

//    result := (((position-iSourceStart))/iSourceWidth)*((iTargetWidth))+iTargetStart;
  end;

end;
//------------------------------------------------------------------------------
function Interpolate(position, iTargetStart,
  iTargetEnd, iSourceStart, iSourceEnd: single): single;

var
  iTargetWidth, iSourceWidth: single;
  rPercent: single;
  rMoveMent: single;
begin
  iSourceWidth := iSourceEnd-(iSourceStart);
  iTargetWidth := iTargetEnd-(iTargetStart);

  if iSourceWidth = 0 then
    result := 0
  else begin
    rPercent := (position - iSourceStart)/(iSourceEnd-iSourceStart);
    rMovement := rPercent*iTargetWidth;
    result := rMoveMent + iTargetStart;

//    result := (((position-iSourceStart))/iSourceWidth)*((iTargetWidth))+iTargetStart;
  end;

end;

function Interpolate_SIN(position, iTargetStart,
  iTargetEnd, iSourceStart, iSourceEnd: nativefloat): nativefloat;

var
  iTargetWidth, iSourceWidth: nativefloat;
  rPercent: nativefloat;
  rMoveMent: nativefloat;
begin
  iSourceWidth := iSourceEnd-(iSourceStart);
  iTargetWidth := iTargetEnd-(iTargetStart);

  if iSourceWidth = 0 then
    result := 0
  else begin
    rPercent := (position - iSourceStart)/(iSourceEnd-iSourceStart);
    if rPercent < 0.0 then rPercent := 0.0;
    if rPercent > 1.0 then rPercent := 1.0;

    rPercent := 1.0 - (Cos(rPercent * 3.1415)+1) / 2;
    rMovement := rPercent*iTargetWidth;
    result := rMoveMent + iTargetStart;

//    result := (((position-iSourceStart))/iSourceWidth)*((iTargetWidth))+iTargetStart;
  end;

end;


//------------------------------------------------------------------------------
function Interpolate(position, steps, iSource, iTarget : nativefloat): nativefloat;
var
  width: nativefloat;
begin
  if steps = 0 then
    width := 0
  else
    width := (iTarget-iSource)/steps;

  result := iSource+(position*width);

end;


function Interpolate(position, iSource, iTarget : single): single;
var
  width: single;
begin
  width := (iTarget-iSource);

  result := iSource+(position*width);

end;
function InterpolateLog(position, steps, iSource, iTarget: nativefloat): nativefloat;overload;
var
  rpos: nativefloat;
begin
  rpos := Interpolate(position, steps, 0, 10);
  rpos := (rpos*rpos)/100;
  result := Interpolate(rpos, 1, iSource, iTarget);



end;

function GetPointToPointDistance(x1,y1,x2,y2: nativefloat): nativefloat;
begin
  result := Sqrt(Power(system.abs(x2-x1),2)+Power(system.abs(y2-y1),2));

end;


{ TNativeVector4 }

class operator TNativeVector4.Add(a, b: TNativeVector4): TNativeVector4;
begin
  result.x := a.x + b.x;
  result.y := a.y + b.y;
  result.z := a.z + b.z;
  result.w := a.w + b.w;
end;

class operator TNativeVector4.Add(a: TNativeVEctor4;
  b: nativefloat): TNativeVector4;
begin
  result.x := a.x + b;
  result.y := a.y + b;
  result.z := a.z + b;
  result.w := a.z + b;
end;

class operator TNativeVector4.Divide(a: TNativeVEctor4;
  b: nativefloat): TNativeVector4;
begin
  result.x := a.x / b;
  result.y := a.y / b;
  result.z := a.z / b;
  result.w := a.w / b;

end;

class operator TNativeVector4.Equal(a, b: TNativeVector4): boolean;
begin
  result := CompareMem(@a, @b, sizeof(TNativeVector4));

end;

procedure TNativeVector4.FromFloatPOint(p: TNativeFloatPOint);
begin
  x:= p.x;
  y := p.y;
  z := 0;
  w := 0;
end;

procedure TNativeVector4.Init;
begin
  fillmem(pointer(@self), sizeof(self), 0);
end;

function TNativeVector4.Magnitude: nativefloat;
begin
  result := sqrt((x*x)+(y*y)+(z*z));
end;

class operator TNativeVector4.Multiply(a: TNativeVEctor4;
  b: nativefloat): TNativeVector4;
begin
  result.x := a.x * b;
  result.y := a.y * b;
  result.z := a.z * b;
  result.w := a.w * b;
end;

class operator TNativeVector4.Subtract(a, b: TNativeVector4): TNativeVector4;
begin
  result.x := a.x - b.x;
  result.y := a.y - b.y;
  result.z := a.z - b.z;
  result.w := a.w - b.w;
end;

function TNativeVector4.ToFloatPOint: TNativeFloatPOint;
begin
  result.x := x;
  result.y := y;
end;

function SharkToothCurve(rPercent0to1: nativefloat; rSharpnessExponent: nativefloat = 1.0): nativefloat;
var
  rPercentAsPi: nativefloat;
  rBaseSharkTooth: nativefloat;
  rPowered: nativefloat;
begin
  rPercentAsPi := rPercent0to1 * 3.1415;
  rBaseSharkTooth := 1-sin(system.abs(rPercentAsPi-(3.1415/2)));//=1-SIN(ABS(C1-($A$1/2)))
  rPowered := power(rBaseSharkTooth, rSharpnessExponent);
  result := rPowered;
end;

function HerbivoreCurve(rPercent0to1: nativefloat; rSharpnessExponent: nativefloat = 1.0): nativefloat;

var
  rPercentAsPi: nativefloat;
  rBaseSharkTooth: nativefloat;
  rPowered: nativefloat;
begin
  rPercentAsPi := rPercent0to1 * 3.1415;
  rBaseSharkTooth := sin(rPercentAsPi);//=1-SIN(ABS(C1-($A$1/2)))
  rPowered := power(rBaseSharkTooth, rSharpnessExponent);
  result := rPowered;
end;

{ TnativefloatPoint }

function TnativefloatPoint.AngleInRads(ao: TAngleOrientation): nativefloat;
var
  q: nativeint;
begin
  result := -1;
  q := quadrant(ao);
  if q = 0 then begin
    if y = 0 then begin
      if x > 0 then result := Pi/2;
      if x < 0 then result := PI*(3/2);
    end else begin
      if ao = aoTraditional then begin
        if y > 0 then result := 0;
        if y < 0 then result := pi;
      end else begin
        if y < 0 then result := 0;
        if y > 0 then result := pi;
      end;
    end;
  end else
  if ao = aoRAster then begin
    if q = 1 then
      result := arctan(system.abs(x)/system.abs(y))
    else
    if q = 2 then
      result := PI-arctan(system.abs(x)/system.abs(y))
    else
    if q = 3 then
      result := PI+arctan(system.abs(x)/system.abs(y))
    else
    if q = 4 then
      result := (2*PI)-arctan(system.abs(x)/system.abs(y));
  end else begin
    raise ECritical.create('traditional values are probably not accurate.');
    if q = 3 then
      result := arctan(system.abs(y)/system.abs(x))
    else
    if q = 4 then
      result := PI-arctan(system.abs(y)/system.abs(x))
    else
    if q = 1 then
      result := PI+arctan(system.abs(y)/system.abs(x))
    else
    if q = 2 then
      result := (2*PI)-arctan(system.abs(y)/system.abs(x));
  end;
end;


function TnativefloatPoint.AngleInUCs(ao: TAngleOrientation): nativefloat;
begin
  result := AngleInRads(ao) / (2*PI);
end;

class operator TnativefloatPoint.Equal(a, b: TNativefloatPoint): boolean;
begin
  result := (a.x=b.x) and (a.y=b.y);
end;

procedure TnativefloatPoint.FromFloats(x, y: nativefloat);
begin
  self.x := x;
  self.y := y;
end;

class function TnativefloatPoint.GetAngularInterpolated_Balance(a0, a1,
  aPos: TNativeFLoatPOint; out bal0, bal1: nativefloat): boolean;
var
  aGap,bGap, aa0,aa1, aaPos: nativefloat;

begin
  aa0 := a0.AngleInUCs(aoRaster);
  aa1 := a1.AngleInUCs(aoRaster);
  aaPos := aPos.AngleInUCs(aoRAster);

  aGap := aa1-aa0;
  if aGAp < 0 then
    aGap := (aa1+1)-aa0;

  bGap := aaPos-aa0;
  if bGap < 0 then
    bGap := (aaPos+1)-aa0;

  if bGap > aGap then begin
    result := false;
    exit;
  end;

  if bGap < 0 then begin
    result := false;
    exit;
  end;

  if aGap = 0 then begin
    if a1.Magnitude = 0 then begin
      bal1 := 0.5;
    end else
    if a0.Magnitude = 0 then begin
      bal0 := 0.5;
    end else begin
      bal0 := aPos.Magnitude / a0.Magnitude;
      bal1 := aPos.Magnitude / a1.Magnitude;
    end;
    result := true;

  end else begin
    if a1.Magnitude = 0 then
      bal1 := 0.5
    else
      bal1 := aPos.Magnitude / a1.magnitude;

    if a0.Magnitude = 0 then
      bal0 := 0.5
    else
      bal0 := aPos.Magnitude/ a0.magnitude;

    result := true;


  end;









end;

function TnativefloatPoint.Magnitude: nativefloat;
begin
  result := sqrt((x*x) + (y*y));
end;

function TnativefloatPoint.Quadrant(ao: TAngleOrientation): nativeint;
begin
  if (ao=aoTraditional) then begin
    result := 0;
    if (x>0) then begin
      if (y>0) then begin
        result := 1;
      end else
      if (y<0) then begin
        result := 2;
      end;
    end else begin
      if (y<0) then begin
        result := 3;
      end else
      if (y>0) then begin
        result := 4;
      end;
    end;
  end else begin
    result := 0;
    if (x>0) then begin
      if (y<0) then begin
        result := 1;
      end else
      if (y>0) then begin
        result := 2;
      end;
    end else begin
      if (y>0) then begin
        result := 3;
      end else
      if (y<0) then begin
        result := 4;
      end;
    end;

  end;
end;

class operator TnativefloatPoint.Subtract(a,
  b: TNativefloatPoint): TNativefloatPoint;
begin
  result.x := a.x - b.x;
  result.y := a.y - b.y;
end;


function TnativefloatPoint.ToPOint: TPOint;
begin
  result.x := round(x);
  result.y := round(y);
end;

function GetIntersectedArea(ax1,ay1,ax2,ay2,bx1,by1,bx2,by2: nativefloat): nativefloat;
var
  p: TNativeFloatPoint;
begin
  p := GetIntersectedAreaDimensions(ax1,ay1,ax2,ay2,bx1,by1,bx2,by2);
  result := p.x*p.y;
end;

function GetIntersectedAreaDimensions(ax1,ay1,ax2,ay2,bx1,by1,bx2,by2: nativefloat): TNativefloatPoint;
var
  w,h: nativefloat;
begin
  result.x := 0;
  result.y := 0;
  if ax2 < bx1 then begin
    exit;
  end;

  if (bx2 < ax1) then begin
    exit;
  end;

  if ay2 < by1 then begin
    exit;
  end;

  if ax2 < bx1 then begin
    exit;
  end;

  order(ax1,ax2);
  order(bx1,bx2);
  order(ay1,ay2);
  order(by1,by2);

  w := lesserof(ax2,bx2)-greaterof(ax1,ax2);
  h := lesserof(ay2,by2)-greaterof(ay1,ay2);

  result.x := w;
  result.y := h;

end;


//------------------------------------------------------------------------------
function LinesIntersect(const x1,y1,x2,y2: nativefloat; { first line}
                         const x3,y3,x4,y4: nativefloat; { second line }
                         out x,y : nativefloat): boolean; { intersection point }

VAR
    a1, a2, b1, b2, c1, c2 : nativefloat; { Coefficients of line eqns.}
    denom : nativefloat;
BEGIN
  a1:= y2-y1;
  b1:= x1-x2;
  c1:= x2*y1 - x1*y2;  { a1*x + b1*y + c1 = 0 is line 1 }

  a2:= y4-y3;
  b2:= x3-x4;
  c2:= x4*y3 - x3*y4;  { a2*x + b2*y + c2 = 0 is line 2 }

  denom:= a1*b2 - a2*b1;
  IF denom = 0 THEN BEGIN
    result:=false;
    EXIT;
  END;

  x:=(b1*c2 - b2*c1)/denom;
  y:=(a2*c1 - a1*c2)/denom;
  result := true;
END;

{ TSingleRect }

function TSingleRect.ToString: string;
begin
  result := '{'+
              floattostr(x1)+','+
              floattostr(y1)+','+
              floattostr(x2)+','+
              floattostr(y2)+'}';

end;

function LinearToCos(position: single): single;
begin
  result := position*3.1415;
  result := cos(result);
  result := (result + 1) / 2;
  result := 1-result;

end;
function IsIn(p: TPOint; r: TRect): boolean;
begin
  result := true;
//  Debug.ConsoleLog('IsIn '+p.ToString+' '+r.ToString);
  Order(r);
//  Debug.ConsoleLog('Reordered IsIn '+p.ToString+' '+r.ToString);
  if p.x< r.Left then begin
    result := false;
    exit;
  end;

  if p.x>r.Right then begin
    result := false;
    exit;
  end;

  if p.y <r.Top then begin
    result := false;
    exit;
  end;

  if p.y>r.Bottom then begin
    result := false;
    exit;
  end;


end;

{ Tpointhelper }

function Tpointhelper.Divide(scalar: ni): TPoint;
begin
  result.x := x div scalar;
  result.y := y div scalar;
end;

procedure Tpointhelper.FromString(s: string);
var
  s1,s2: string;
begin
  s2 := s;
  SplitString(s2, '(',s1,s2);
  SplitString(s2, ',',s1,s2);
  x := strtoni(s1);
  SplitString(s2, ')',s1,s2);
  y := strtoni(s1);
end;

function Tpointhelper.ToString: string;
begin
  result := '('+inttostr(x)+','+inttostr(y)+')';
end;

{ TRectHelper }

procedure TRectHelper.FromString(s: string);
var
  s1,s2: string;
begin
  s2 := s;

  SplitString(s2, '~', s1,s2);
  self.TopLeft.FromString(s1);
  self.BottomRight.FromString(s2);
end;

function TRectHelper.ToString: string;
begin
  result := TopLeft.ToString+'~'+BottomRight.ToString;
end;

{ TpointFhelper }

function TpointFhelper.Divide(scalar: ni): TPoint;
begin
  x := x / scalar;
  y := y / scalar;
end;

procedure TpointFhelper.FromString(s: string);
var
  s1,s2: string;
begin
  s2 := s;
  SplitString(s2, '(',s1,s2);
  SplitString(s2, ',',s1,s2);
  x := strtofloat(s1);
  SplitString(s2, ')',s1,s2);
  y := strtofloat(s1);
end;

function TpointFhelper.ToString: string;
begin
  result := '('+floattostr(x)+','+floattostr(y)+')';
end;

{ TRectFHelper }

procedure TRectFHelper.FromString(s: string);
var
  s1,s2: string;
begin
  if zpos('~', s) < 0 then
    s := stringreplace(s, ')-(', ')~(', [rfReplaceAll]);

  s2 := s;
  SplitString(s2, '~', s1,s2);
  self.TopLeft.FromString(s1);
  self.BottomRight.FromString(s2);
end;

function TRectFHelper.ToString: string;
begin
  result := TopLeft.ToString+'~'+BottomRight.ToString;
end;

function Abs(p: TPOintF): TPOintF;
begin
  result := p;
  if result.X < 0 then
    result.x := 0-result.x;
  if result.y < 0 then
    result.y := 0-result.y;
end;


function Abs(i: int64): int64;
begin
  result := system.Abs(i);
end;


function Abs(i: integer): integer;
begin
  result := system.Abs(i);
end;

function Abs(s: single): single;
begin
  result := system.Abs(s);
end;

function Abs(d: double): double;
begin
  result := system.Abs(d);
end;

function Interpolate_SIN_Mag(mag, position,
                          iTargetStart,
                          iTargetEnd,
                          iSourceStart,
                          iSourceEnd: nativefloat): nativefloat;
var
  iTargetWidth, iSourceWidth: nativefloat;
  rPercent: nativefloat;
  rMoveMent: nativefloat;
begin
  iSourceWidth := iSourceEnd-(iSourceStart);
  iTargetWidth := iTargetEnd-(iTargetStart);

  if iSourceWidth = 0 then
    result := 0
  else begin
    rPercent := (position - iSourceStart)/(iSourceEnd-iSourceStart);
    if mag < 1 then mag := 1;

    rPercent := ((rPercent - 0.5)*mag) + 0.5;
    if rPercent < 0.0 then rPercent := 0.0;
    if rPercent > 1.0 then rPercent := 1.0;




    rPercent := 1.0 - (Cos(rPercent * 3.1415)+1) / 2;
    rMovement := rPercent*iTargetWidth;
    result := rMoveMent + iTargetStart;

//    result := (((position-iSourceStart))/iSourceWidth)*((iTargetWidth))+iTargetStart;
  end;

end;


function RectMinusRectEdge(rectOuter, rectEdge: TRect): TRect;
begin
  result := rectOuter;

  if rectEdge.Left = rectOuter.Left then begin
    if rectEdge.Right-1 = rectOuter.Right then begin
      if rectEdge.Top = rectOuter.Top then
        result.Top := rectEdge.Bottom+1;
    end;
  end;

  if rectEdge.Left = rectOuter.Left then begin
    if rectEdge.Right = rectOuter.Right then begin
      if rectEdge.Bottom = rectOuter.Bottom then
        result.Bottom := rectEdge.Top-1;
    end;
  end;

  if rectEdge.Left = rectOuter.Left then begin
    if rectEdge.Bottom = rectOuter.Bottom then begin
      if rectEdge.Top = rectOuter.Top then
        result.Left := rectEdge.Right-1;
    end;
  end;

  if rectEdge.Right = rectOuter.Right then begin
    if rectEdge.Bottom = rectOuter.Bottom then begin
      if rectEdge.Top = rectOuter.Top then
        result.Right := rectEdge.LEft+1;
    end;
  end;



end;


end.

