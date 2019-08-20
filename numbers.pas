unit numbers;

interface

uses
  typex, sysutils, math, types, variants;

function LOWORD(c: cardinal): ni;
function HIWORD(c: cardinal): ni;

function VarToDoubleNull(v: variant; ifNull: double): double;
function IntToDec32(i: integer; digits: integer): string;
function GetDec32Digit(b: byte): string;
function Dec32ToInt(s: string): integer;
function GEtDigitFromDec32(c: char): integer;
function NanFix(n: double; fixed: double): double;
function InRange(n, lo,hi: double):boolean;overload;
function InRange(n, lo,hi: int64):boolean;overload;
function SnapTo(d: double; snap: double): double;overload;
function SnapTo(d: single; snap: single): single;overload;
function SnapToLess(d: double; snap: double): double;overload;
function SnapToMore(d: double; snap: double): double;overload;

function LesserOf(i1, i2: int64): int64;overload;inline;
function LesserOf(i1, i2: integer): integer;overload;inline;
function LesserOf(i1, i2: nativefloat): nativefloat;overload;inline;
function LesserOf(i1, i2, i3: int64): int64;overload;inline;
function LesserOf(i1, i2, i3: integer): integer;overload;inline;
function LesserOf(i1, i2, i3: nativefloat): nativefloat;overload;inline;
function LesserOf(p1,p2: TPoint): TPoint;overload;inline;

function RoundPoint(p: TPointF): TPoint;
function TruncPOint(p: TpointF): TPoint;

function GreaterOf(i1, i2: int64): int64;overload;inline;
function GreaterOf(i1, i2: integer): integer;overload;inline;
function GreaterOf(r1,r2: nativefloat): nativefloat;overload;inline;
function GreaterOf(i1, i2, i3: int64): int64;overload;inline;
function GreaterOf(i1, i2, i3: integer): integer;overload;inline;
function GreaterOf(i1, i2, i3: nativefloat): nativefloat;overload;inline;


function Clamp(r1, min,max: double): double;overload;inline;
function Clamp(r1, min,max: single): single;overload;inline;
function Clamp(r1, min,max: int64): int64;overload;inline;
function Clamp(r1, min,max: integer): integer;overload;inline;



function Order(var i1,i2: int64):boolean;overload;
function Order(var i1,i2: integer): boolean;overload;
function Order(var i1,i2: byte): boolean;overload;
function Order(var i1,i2: nativefloat): boolean;overload;
function Order(var ul,br: TPoint): boolean;overload;
function Order(var ul,br: TPointF): boolean;overload;
function Order(var r: TRect): boolean;overload;
function Order(var r: TRectF): boolean;overload;
function Order(var r: TPixelRect): boolean;overload;
function HextoInt(s: string): integer;
function HextoInt64(s: string): int64;
function truncate(r: single; bSubOneNegative: boolean): nativeint;overload;
function truncate(r: double; bSubOneNegative: boolean): nativeint;overload;
function TruncateVerify(e: extended): nativeint;

function Clip(r: nativefloat; iLowerbound, iHigherbound: integer):integer;
function negfloor(r: double): double;inline;overload;
function negfloor(r: single): single;inline;overload;
function fraction(r: single): single;inline;overload;
function fraction(r: double): double;inline;overload;
function floorfraction(r: single): single;inline;overload;
function floorfraction(r: double): double;inline;overload;

procedure Swap(var x: integer; var y: integer);overload;
procedure Swap(var x: int64; var y: int64);overload;

function MinArray(a: array of nativeint): nativeint;
function IsNumber(s: string): boolean;


type
  TGiantInt = record
    i1,i2,i3,i4: Uint64;
    procedure ShiftRight;
    procedure ShiftLeft;
    procedure Init;
    function ToInt64: int64;
    procedure FRomINt64(i: int64);
    function ToUInt64: UInt64;
    procedure FRomUInt64(i: UInt64);
    class operator implicit(g: TGiantInt): int64;
    class operator explicit(g: TGiantInt): int64;
    class operator rightshift(g: TGiantInt; i: ni): TGiantInt;
    class operator LeftShift(a: TGiantInt; i:ni): TGiantInt;
    class operator negative(const g: TGiantInt): TGiantInt;
    class operator Inc(a: TGiantInt) : TGiantInt;
    class operator Dec(a: TGiantInt): TGiantInt;
    class operator Trunc(a: TGiantInt): TGiantInt;
    class operator In(a: TGiantInt; const b: array of TGiantInt) : Boolean;
    class operator Equal(a: TGiantInt; b: TGiantInt) : Boolean;
    class operator NotEqual(a: TGiantInt; b: TGiantInt): Boolean;
    class operator GreaterThan(a: TGiantInt; b: TGiantInt): Boolean;
    class operator GreaterThanOrEqual(a: TGiantInt; b: TGiantInt): Boolean;
    class operator LessThan(a: TGiantInt; b: TGiantInt): Boolean;
    class operator LessThanOrEqual(a: TGiantInt; b: TGiantInt): Boolean;
    class operator Add(a: TGiantInt; b: TGiantInt): TGiantInt;
    class operator Add(a: TGiantInt; b: int64): TGiantInt;
    class operator Add(a: TGiantInt; b: Uint64): TGiantInt;
    class operator Subtract(a: TGiantInt; b: TGiantInt) : TGiantInt;
    class operator Subtract(a: TGiantInt; b: uint64) : TGiantInt;
    class operator Subtract(a: TGiantInt; b: int64) : TGiantInt;
    class operator Multiply(a: TGiantInt; b: TGiantInt) : TGiantInt;
    class operator Divide(a: TGiantInt; b: TGiantInt) : TGiantInt;
    class operator IntDivide(a: TGiantInt; b: TGiantInt): TGiantInt;
    class operator Modulus(a: TGiantInt; b: TGiantInt): TGiantInt;
    class operator BitwiseAnd(a: TGiantInt; b: TGiantInt): TGiantInt;
    class operator BitwiseOr(a: TGiantInt; b: TGiantInt): TGiantInt;
    class operator BitwiseXor(a: TGiantInt; b: TGiantInt): TGiantInt;
    function ToHex: string;
  end;

//  TGiantInt64Helper = record helper for int64
//    class operator Explicit(a: int64): TGiantINt;
//  end;

const
  GFI_ONE : TGiantInt = (i1:1; i2:0; i3:0; i4:0);
  GFI_ZERO : TGiantInt = (i1:0; i2:0; i3:0; i4:0);
  GFI_NEGONE: TGiantInt = (i1:$FFFFFFFFFFFFFFFF;i2:$FFFFFFFFFFFFFFFF;i3:$FFFFFFFFFFFFFFFF;i4:$FFFFFFFFFFFFFFFF);


implementation

uses
  debug;

function MinArray(a: array of nativeint): nativeint;
var
  i: ni;
begin
  result := a[low(a)];
  for i := low(a)+1 to high(a) do begin
    result := min(a[i], result);
  end;


end;

procedure Swap(var x: integer; var y: integer);
var
  z: integer;
begin
  z := x;
  x := y;
  y := z;

end;

procedure Swap(var x: int64; var y: int64);overload;
var
  z: int64;
begin
  z := x;
  x := y;
  y := z;

end;


function LesserOf(i1, i2: int64): int64;overload;
begin
  if i1 < i2 then
    result := i1
  else
    result := i2;
end;

function LesserOf(i1, i2: integer): integer;overload;
begin
  if i1 < i2 then
    result := i1
  else
    result := i2;
end;




function LesserOf(i1, i2: nativefloat): nativefloat;overload;
begin
  if i1 < i2 then
    result := i1
  else
    result := i2;
end;



function GreaterOf(i1, i2: integer): integer;
begin
  if i1  > i2 then
    result := i1
  else
    result := i2;
end;

function GreaterOf(i1, i2: int64): int64;
begin
  if i1  > i2 then
    result := i1
  else
    result := i2;
end;


function GreaterOf(r1,r2: nativefloat): nativefloat;overload;
begin
  if r1  > r2 then
    result := r1
  else
    result := r2;

end;

function Order(var i1,i2: integer):boolean;
var
  t: integer;
begin
  result := false;
  if (i1>i2) then begin
    t := i1;
    i1 := i2;
    i2 := t;
    result := true;
  end;
end;

function Order(var i1,i2: int64):boolean;
var
  t: nativeint;
begin
  result := false;
  if (i1>i2) then begin
    t := i1;
    i1 := i2;
    i2 := t;
    result := true;
  end;
end;


function Order(var i1,i2: byte):boolean;
var
  t: byte;
begin
  result := false;
  if (i1>i2) then begin
    t := i1;
    i1 := i2;
    i2 := t;
    result := true;
  end;
end;

function  Order(var i1,i2: nativefloat):boolean;
var
  t: nativefloat;
begin
  result := false;
  if (i1>i2) then begin
    t := i1;
    i1 := i2;
    i2 := t;
    result := true;
  end;
end;

function HextoInt(s: string): integer;
begin
  result := strtoint('$'+s);
end;

function HextoInt64(s: string): int64;
begin
  result := strtoint('$'+s);
end;


function truncate(r: single; bSubOneNegative: boolean): nativeint;
begin
  if (r < 0) and (bSubOneNegative) then begin
    result := trunc(r)-1;
  end else begin
    result := trunc(r);
  end;
end;

function truncate(r: double; bSubOneNegative: boolean): nativeint;
begin
  if (r < 0) and (bSubOneNegative) then begin
    result := trunc(r)-1;
  end else begin
    result := trunc(r);
  end;
end;

function TruncateVerify(e: extended): nativeint;
begin
  result := trunc(e);
  if e-result = 1 then
    result := 0;
end;


function Clip(r: nativefloat; iLowerbound, iHigherbound: integer):integer;
begin
  result := system.round(r);

  if result < iLowerbound then
    result := iLowerbound;

  if result > iHigherBound then
    result := iHigherBound;

end;


function negfloor(r: double): double;
begin
  if r< 0 then
    result := floor(r)
  else
    result := floor(r);
end;

function negfloor(r: single): single;
begin
  if r< 0 then
    result := floor(r)
  else
    result := floor(r);
end;


function fraction(r: single): single;
begin
  result := r-trunc(r);
end;

function fraction(r: double): double;
begin
  result := r-trunc(r);
end;

function floorfraction(r: single): single;inline;overload;
begin
  result := r-floor(r);
end;
function floorfraction(r: double): double;inline;overload;
begin
  result := r-floor(r);
end;


function SnapTo(d: double; snap: double): double;overload;
begin
  result := system.round(d * (1/snap))*snap;
end;

function SnapTo(d: single; snap: single): single;overload;
begin
  result := system.round(d * (1/snap))*snap;
end;

function SnapToLess(d: double; snap: double): double;overload;
begin
  result := snapto(d,snap);
  if result > d then
    result := result - snap;
end;
function SnapToMore(d: double; snap: double): double;overload;
begin
  result := snapto(d,snap);
  if result > d then
    result := result + snap;

end;

function InRange(n, lo,hi: double):boolean;
begin
  result := (n >= lo) and (n <=hi);
end;

function InRange(n, lo,hi: int64):boolean;
begin
  result := (n >= lo) and (n <=hi);
end;


function NanFix(n: double; fixed: double): double;
begin
  if isnan(n) then
    result := fixed
  else
    result := n;
end;

function IntToDec32(i: integer; digits: integer): string;
var
  t: integer;
begin
  result := '';
  for t:= 1 to digits do begin
    result := GetDec32Digit(i and 31)+result;
    i := i shr 5;
  end;
end;

function GetDec32Digit(b: byte): string;
begin
  case b of
    0..9: result := inttostr(b);
    10: result := 'a';
    11: result := 'b';
    12: result := 'c';
    13: result := 'd';
    14: result := 'e';
    15: result := 'f';
    16: result := 'g';
    17: result := 'h';
    18: result := 'i';
    19: result := 'j';
    20: result := 'k';
    21: result := 'l';
    22: result := 'm';
    23: result := 'n';
    24: result := 'o';
    25: result := 'p';
    26: result := 'q';
    27: result := 'r';
    28: result := 's';
    29: result := 't';
    30: result := 'u';
    31: result := 'v';
  end;
end;


function Dec32ToInt(s: string): integer;
var
  t: integer;
begin
  result := 0;
  for t:= 1 to length(s) do begin
    result := (result shl 5)+GetDigitFromDec32(s[t]);
  end;

end;


function GEtDigitFromDec32(c: char): integer;
var
  cc: string;
begin
   cc := lowercase(c);
  if cc[1]>='a' then
    result := ord(cc[1])-ord('a')+10
  else
    result := strtoint(c);


end;

function Clamp(r1, min,max: double): double;overload;inline;
begin
  result := GreaterOf(LesserOf(r1,max), min);
end;

function Clamp(r1, min,max: single): single;overload;inline;
begin
  result := GreaterOf(LesserOf(r1,max), min);
end;

function Clamp(r1, min,max: int64): int64;overload;inline;
begin
  result := GreaterOf(LesserOf(r1,max), min);
end;

function Clamp(r1, min,max: integer): integer;overload;inline;
begin
  result := GreaterOf(LesserOf(r1,max), min);
end;



function LOWORD(c: cardinal): ni;
begin
  result := c and $ffff;
end;

function HIWORD(c: cardinal): ni;
begin
  result := (c shr 16) and $ffff;
end;

function LesserOf(p1,p2: TPoint): TPoint;overload;inline;
begin
  result.x := lesserof(p1.x,p2.x);
  result.y := lesserof(p1.y,p2.y);
end;

function Order(var ul,br: TPoint): boolean;
var
  a,b: TPoint;
begin
  result := false;

  a := ul;
  b := br;
  a.X := lesserof(ul.x, br.x);
  a.y := lesserof(ul.y, br.y);
  b.x := greaterof(ul.X, br.X);
  b.y := greaterof(ul.y, br.y);
  ul := a;  br := b;

end;

function Order(var ul,br: TPointF): boolean;overload;
var
  a,b: TPointF;
begin
  result := false;

  a := ul;
  b := br;
  a.X := lesserof(ul.x, br.x);
  a.y := lesserof(ul.y, br.y);
  b.x := greaterof(ul.X, br.X);
  b.y := greaterof(ul.y, br.y);
  ul := a;  br := b;
end;
function Order(var r: TRect): boolean;overload;
begin
  result := Order(r.TopLeft, r.BottomRight);
end;

function Order(var r: TPixelRect): boolean;overload;
begin
  r := r;
  result := true;
end;

function Order(var r: TRectF): boolean;overload;
begin
  result := Order(r.TopLeft, r.BottomRight);
end;


function RoundPoint(p: TPointF): TPoint;
begin
  result.X := system.round(p.x);
  result.y := system.round(p.y);
end;

function TruncPOint(p: TpointF): TPoint;
begin
  result.X := system.trunc(p.x);
  result.y := system.trunc(p.y);
end;


{ TGiantInt }

class operator TGiantInt.Add(a, b: TGiantInt): TGiantInt;
var
  c1,c2,c3,c4: UInt64;
  r1,r2,r3,r4: UInt64;
  i1,i2,i3,i4: UInt64;
  o1,o2,o3,o4: UInt64;
begin
  i1 := a.i1;
  i2 := a.i2;
  i3 := a.i3;
  i4 := a.i4;
  o1 := b.i1;
  o2 := b.i2;
  o3 := b.i3;
  o4 := b.i4;
  r1 := i1+o1;
//  Debug.Consolelog(inttohex(r1,16));
  c1 := byte(r1<i1);
//  Debug.Consolelog(inttohex(c1,16));
  r2 := i2+o2+c1;
//  Debug.Consolelog(inttohex(r2,16));
  c2 := byte((r2<i2) or ((r2=i2) and (c1<>0)));
//  Debug.Consolelog(inttohex(c2,16));
  r3 := i3+o3+c2;
//  Debug.Consolelog(inttohex(r3,16));
  c3 := byte((r3<i3)  or ((r3=i3) and (c2<>0)));
//  Debug.Consolelog(inttohex(c3,16));
  r4 := i4+o4+c3;
//  Debug.Consolelog(inttohex(r4,16));
  c4 := byte((r4<i4)  or ((r4=i4) and (c3<>0)));
//  Debug.Consolelog(inttohex(c4,16));
//  r1 := r1 + c4;//<<roll over carry
//  Debug.Consolelog(inttohex(r1,16));

  result.i1 := r1;
  result.i2 := r2;
  result.i3 := r3;
  result.i4 := r4;



end;

class operator TGiantInt.Add(a: TGiantInt;
  b: int64): TGiantInt;
var
  bb: TGiantInt;
begin
  bb.FRomINt64(b);
  result := a+bb;



end;

class operator TGiantInt.Add(a: TGiantInt;
  b: Uint64): TGiantInt;
var
  bb: TGiantInt;
begin
  bb.FRomUINt64(b);
  result := a+bb;
end;

class operator TGiantInt.BitwiseAnd(a,
  b: TGiantInt): TGiantInt;
var
  p1,p2,p3: ^UInt64;
begin
  p1 := @a.i1;
  p2 := @b.i1;
  p3 := @result.i1;
  p3^ := p1^ and p2^;
  inc(p3);
  inc(p1);
  inc(p2);
  p3^ := p1^ and p2^;
  inc(p3);
  inc(p1);
  inc(p2);
  p3^ := p1^ and p2^;
  inc(p3);
  inc(p1);
  inc(p2);
  p3^ := p1^ and p2^;
  inc(p3);
  inc(p1);
  inc(p2);
end;

class operator TGiantInt.BitwiseOr(a,
  b: TGiantInt): TGiantInt;
var
  p1,p2,p3: ^UInt64;
begin
  p1 := @a.i1;
  p2 := @b.i1;
  p3 := @result.i1;
  p3^ := p1^ or p2^;
  inc(p3);
  inc(p1);
  inc(p2);
  p3^ := p1^ or p2^;
  inc(p3);
  inc(p1);
  inc(p2);
  p3^ := p1^ or p2^;
  inc(p3);
  inc(p1);
  inc(p2);
  p3^ := p1^ or p2^;
  inc(p3);
  inc(p1);
  inc(p2);
end;

class operator TGiantInt.BitwiseXor(a,
  b: TGiantInt): TGiantInt;
var
  p1,p2,p3: ^UInt64;
begin
  p1 := @a.i1;
  p2 := @b.i1;
  p3 := @result.i1;
  p3^ := p1^ xor p2^;
  inc(p3);
  inc(p1);
  inc(p2);
  p3^ := p1^ xor p2^;
  inc(p3);
  inc(p1);
  inc(p2);
  p3^ := p1^ xor p2^;
  inc(p3);
  inc(p1);
  inc(p2);
  p3^ := p1^ xor p2^;
  inc(p3);
  inc(p1);
  inc(p2);
end;

class operator TGiantInt.Dec(a: TGiantInt): TGiantInt;
begin
  result := a - int64(1);
end;

class operator TGiantInt.Divide(a,
  b: TGiantInt): TGiantInt;
begin
  raise ECritical.Create('unimplemented');
end;

class operator TGiantInt.Equal(a, b: TGiantInt): Boolean;
begin
  result := true;
  if a.i1<>b.i1 then
    exit(false);
  if a.i2<>b.i2 then
    exit(false);
  if a.i3<>b.i3 then
    exit(false);
  if a.i4<>b.i4 then
    exit(false);
end;

class operator TGiantInt.explicit(g: TGiantInt): int64;
begin
  result := g.ToInt64;

end;

procedure TGiantInt.FRomINt64(i: int64);
begin
  i1 := i and $7fffffffffffffff;
  i2 := 0;
  i3 := 0;
  i4 := i and $8000000000000000;
end;

procedure TGiantInt.FRomUInt64(i: UInt64);
begin
  i1 := i;
  i2 := 0;
  i3 := 0;
  i4 := 0;
end;

class operator TGiantInt.GreaterThan(a, b: TGiantInt): Boolean;
var
  aa,bb: uint64;
begin
  aa := a.i1; bb := b.i1;
  result := aa>bb;
  if result then exit;
  aa := a.i2; bb := b.i2;
  result := aa>bb;
  if result then exit;
  aa := a.i3; bb := b.i3;
  result := aa>bb;
  if result then exit;
  aa := a.i4; bb := b.i4;
  result := aa>bb;
  if result then exit;




end;

class operator TGiantInt.GreaterThanOrEqual(a,
  b: TGiantInt): Boolean;
begin
  result := (a>b) or (a=b);


end;

class operator TGiantInt.implicit(g: TGiantInt): int64;
begin
  result := g.ToInt64;

end;


class operator TGiantInt.In(a: TGiantInt;
  const b: array of TGiantInt): Boolean;
var
  t: ni;
  bb: TGiantInt;
begin
  for t:= low(b) to high(b) do begin
    bb := b[t];
    if a = bb then
      exit(true);
  end;
  result := false;
end;

class operator TGiantInt.Inc(a: TGiantInt): TGiantInt;
begin
  result := a + int64(1);
end;

procedure TGiantInt.Init;
begin
  i1 := 0;
  i2 := 0;
  i3 := 0;
  i4 := 0;
end;

class operator TGiantInt.IntDivide(a,
  b: TGiantInt): TGiantInt;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;


class operator TGiantInt.LeftShift(a: TGiantInt;
  i: ni): TGiantInt;

begin
  result := a;
  while i > 0 do begin
    result.ShiftLeft;
    dec(i);
  end;
end;

class operator TGiantInt.LessThan(a, b: TGiantInt): Boolean;
var
  aa,bb: uint64;
begin
  aa := a.i1; bb := b.i1;
  result := aa<bb;
  if result then exit;
  aa := a.i2; bb := b.i2;
  result := aa<bb;
  if result then exit;
  aa := a.i3; bb := b.i3;
  result := aa<bb;
  if result then exit;
  aa := a.i4; bb := b.i4;
  result := aa<bb;
  if result then exit;


end;

class operator TGiantInt.LessThanOrEqual(a,
  b: TGiantInt): Boolean;
begin
  result := (a<b) or (a=b);
end;

class operator TGiantInt.Modulus(a,
  b: TGiantInt): TGiantInt;
begin
  raise ECritical.create('not implemented');
end;

class operator TGiantInt.Multiply(a,
  b: TGiantInt): TGiantInt;
begin
  raise ECritical.create('not implemented');
end;

class operator TGiantInt.negative(const g: TGiantInt): TGiantInt;
var
  b: TGiantInt;
begin
  result.i1 := g.i1 xor $FFFFFFFFFFFFFFFF;
  result.i2 := g.i2 xor $FFFFFFFFFFFFFFFF;
  result.i3 := g.i3 xor $FFFFFFFFFFFFFFFF;
  result.i4 := g.i4 xor $FFFFFFFFFFFFFFFF;
  result := result + GFI_ONE;

end;

class operator TGiantInt.NotEqual(a, b: TGiantInt): Boolean;
begin
  result := not (a=b);
end;

class operator TGiantInt.rightshift(g: TGiantInt;
  i: ni): TGiantInt;
var
  cx: ni;
begin
  result := g;
  //slow method
  cx := i;
  while cx > 0 do begin
    result.shiftright;
    dec(cx);
  end;
end;



procedure TGiantInt.ShiftLeft;
var
  c1,c2,c3: int64;
begin
  c1 := i1 and $8000000000000000;
  c2 := i2 and $8000000000000000;
  c3 := i3 and $8000000000000000;
  i1 := i1 shl int64(1);
  i2 := i2 shl int64(1);
  i3 := i3 shl int64(1);
  i4 := i4 shl int64(1);
  i2 := i2 or (c1 shr int64(63));
  i3 := i3 or (c2 shr int64(63));
  i4 := i4 or (c3 shr int64(63));





end;

procedure TGiantInt.ShiftRight;
var
  c2,c3,c4: int64;
begin
  c2 := i2 and 1;
  c3 := i3 and 1;
  c4 := i4 and 1;

  i1 := i1 shr int64(1);
  i2 := i2 shr int64(1);
  i3 := i3 shr int64(1);
  i4 := i4 shr int64(1);
  i1 := i1 or (c2 shl int64(63));
  i2 := i2 or (c3 shl int64(63));
  i3 := i3 or (c4 shl int64(63));




end;

class operator TGiantInt.Subtract(a: TGiantInt;
  b: uint64): TGiantInt;
var
  bb: TGiantInt;
begin
  bb.FRomUINt64(b);
  result := a - bb;
end;

class operator TGiantInt.Subtract(a: TGiantInt;
  b: int64): TGiantInt;
var
  bb: TGiantInt;
begin
  bb.FRomINt64(b);
  result := a - bb;
end;

class operator TGiantInt.Subtract(a,
  b: TGiantInt): TGiantInt;
var
  bb: TGiantInt;
begin
  bb := -b;
  result := a+bb;


end;

function TGiantInt.ToHex: string;
begin
  result := inttohex(i4,16)+inttohex(i3,16)+inttohex(i2,16)+inttohex(i1,16);
end;

function TGiantInt.ToInt64: int64;
begin
  result := i1 and      $7fffffffffffffff;
  if ((i4 and           $8000000000000000) <> 0) then
    result := result or $8000000000000000;

end;

function TGiantInt.ToUInt64: UInt64;
begin
  result := i1;
end;

class operator TGiantInt.Trunc(a: TGiantInt): TGiantInt;
begin
  result := a;
end;

function LesserOf(i1, i2, i3: int64): int64;overload;inline;
begin
  result := i1;
  if i2 < result then result := i2;
  if i3 < result then result := i3;
end;

function LesserOf(i1, i2, i3: integer): integer;overload;inline;
begin
  result := i1;
  if i2 < result then result := i2;
  if i3 < result then result := i3;
end;

function LesserOf(i1, i2, i3: nativefloat): nativefloat;overload;inline;
begin
  result := i1;
  if i2 < result then result := i2;
  if i3 < result then result := i3;
end;

function GreaterOf(i1, i2, i3: int64): int64;overload;inline;
begin
  result := i1;
  if i2 > result then result := i2;
  if i3 > result then result := i3;
end;
function GreaterOf(i1, i2, i3: integer): integer;overload;inline;
begin
  result := i1;
  if i2 > result then result := i2;
  if i3 > result then result := i3;
end;

function GreaterOf(i1, i2, i3: nativefloat): nativefloat;overload;inline;
begin
  result := i1;
  if i2 > result then result := i2;
  if i3 > result then result := i3;
end;

function VarToDoubleNull(v: variant; ifNull: double): double;
begin
  if vartype(v) = varNull then
    exit(ifnull);

  exit(v);
end;

function IsNumber(s: string): boolean;
var
  iValue, iCode: integer;
begin
  val(s, iValue, iCode);
  result := iCode = 0;

end;


end.
