unit typex;
{$I DelphiDefs.inc}
interface

uses
{$IFDEF NEED_FAKE_ANSISTRING}
  ios.stringx.iosansi,
{$ENDIF}
  sysutils, variants;



const

{$IFDEF WINDOWS}
  NEWLINE = #13#10;
{$ELSE}
  NEWLINE = #10;
{$ENDIF}
  CR = #13;
  LF = #10;
  THOUSAND:int64 = 1000;
  MILLION:int64 = 1000000;
  BILLION:int64 = 1000000000;
  TRILLION:int64 =    1000000000000;
  QUADRILLION:int64 = 1000000000000000;
  PENTILLION:int64 = 1000000000000000000;
  BIT_THOUSAND:int64 = 1024;
  BIT_MILLION:int64 = 1024*1024;
  BIT_BILLION:int64 = 1024*1024*1024;
  BIT_TRILLION:int64 = 1099511627776;
  KILO:int64 = 1024;
  MEGA:int64 = 1024*1024;
  GIGA:int64 = 1024*1024*1024;
  TERA:int64 = 1099511627776;
{$IF sizeof(pointer)=8}
  POINTER_SHIFT = 3;
{$ELSE}
  POINTER_SHIFT_ = 2;
{$ENDIF}








type
{$IFDEF CPUx64}
  TSize = uint64;
{$ELSE}
  TSize = cardinal;
{$ENDIF}
  TriBool = (tbNull, tbFalse, tbTrue);
  EClassException = class(Exception);
  EBetterException = class(Exception);
  ENotImplemented = class(Exception);
  ECritical = class(Exception);
  ENetworkError = class(Exception);
  EUserError = class(Exception);
  EScriptVarError = class(Exception);
{$IFDEF IS_64BIT}
  nativeQfloat = double;
{$ELSE}
  nativefloat = single;
{$ENDIF}
  DWORD = cardinal;
  PDWORD = ^DWORD;
  BOOL = wordbool;
  ni = nativeint;
  fi = integer;
  TDynByteArray = array of byte;
  TDynInt64Array = array of Int64;
  PInt16 = ^smallint;
  PInt32 = ^integer;
  tinyint = shortint;
  signedchar = shortint;
  signedbyte = shortint;


{$IFDEF ZEROBASEDSTRINGS}
const STRZERO = 0;
{$ELSE}
const STRZERO = 1;
{$ENDIF}



{$IFDEF GT_XE3}
type
  TVolatileProgression = record
    StepsCompleted: nativeint;
    TotalSteps: nativeint;
    Complete: boolean;
    procedure Reset;
  end;
  PVolatileProgression = ^TVolatileProgression;

  TStringHelperEx = record {$IFDEF SUPPORTS_RECORD_HELPERS}helper for string{$ENDIF}
    function ZeroBased: boolean;
    function FirstIndex: nativeint;
  end;
{$ELSE}
  {$Message Error 'we don''t support this compiler anymore'}
{$ENDIF}


{$IFDEF NEED_FAKE_ANSISTRING}
type
  ansistring = ios.stringx.iosansi.ansistring;
{$ENDIF}
  nf = nativefloat;

  ASingleArray = array[0..0] of system.Single;
  PSingleArray = ^ASingleArray;
  ADoubleArray = array[0..0] of system.Double;
  PDoubleArray = ^ADoubleArray;
  ASmallintArray = array[0..0] of smallint;
  PSmallintArray = ^ASmallintArray;

  ByteArray = array[0..0] of byte;
  PByteArray = ^ByteArray;


  complex = packed record
    r: double;
    i: double;
  end;
  complexSingle = packed record
    r: single;
    i: single;
  end;

  TNativeFloatRect = record
    x1,y1,x2,y2: nativefloat;
  end;

  PComplex = ^complex;

  TProgress = record
    step, stepcount: int64;
    function PercentComplete: single;
  end;

  PProgress = ^TProgress;






  AComplexArray = array[0..0] of complex;
  PComplexArray = ^AComplexArray;
  AComplexSingleArray = array[0..0] of complexSingle;
  PComplexSingleArray = ^AComplexSingleArray;
  fftw_complex = complex;
  Pfftw_complex = PComplex;
  PAfftw_complex = PComplexArray;
  fftw_float = system.double;
  Pfftw_float = system.Pdouble;
  PAfftw_float = PDoubleArray;

function STRZ(): nativeint;
function BoolToTriBool(b: boolean): TriBool;inline;
function TriBoolToBool(tb: TriBool): boolean;inline;
function BoolToint(b: boolean): integer;
function InttoBool(i: integer): boolean;
function DynByteArrayToInt64Array(a: TDynByteArray): TDynInt64Array;
function DynInt64ArrayToByteArray(a: TDynInt64Array): TDynByteArray;
function StringToTypedVariant(s: string): variant;
function JavaScriptStringToTypedVariant(s: string): variant;
function VartoStrEx(v: variant): string;
function IsVarString(v: variant): boolean;

implementation

uses
  systemx;

function STRZ(): nativeint;
//Returns the index of the first element of a string based on current configuration
begin
{$IFDEF MSWINDOWS}
  exit(1);
{$ELSE}
  exit(0);
{$ENDIF}
end;

{ TStringHelperEx }

{$IFDEF GT_XE3}
function TStringHelperEx.FirstIndex: nativeint;
begin
  result := STRZ;
end;
{$ENDIF}

{$IFDEF GT_XE3}
function TStringHelperEx.ZeroBased: boolean;
begin
  result := STRZ=0;
end;
{$ENDIF}

function BoolToTriBool(b: boolean): TriBool;inline;
begin
  if b then
    result := tbTrue
  else
    result := tbFalse;
end;
function TriBoolToBool(tb: TriBool): boolean;inline;
begin
  result := tb = tbTrue;
end;

function BoolToint(b: boolean): integer;
begin
  if b then
    result := 1
  else
    result := 0;

end;

function InttoBool(i: integer): boolean;
begin
  result := i <> 0;
end;


function DynInt64ArrayToByteArray(a: TDynInt64Array): TDynByteArray;
begin
  SetLength(result, length(a) * 8);
  movemem32(@result[0], @a[0], length(result));
end;

function DynByteArrayToInt64Array(a: TDynByteArray): TDynInt64Array;
begin
  SEtLength(result, length(a) shr 3);
  movemem32(@result[0], @a[0], length(a));
end;

function JavaScriptStringToTypedVariant(s: string): variant;
begin
  result := StringToTypedVariant(s);
  if varType(s) = varString then
    result := StringReplace(result, '\\','\', [rfReplaceall]);
end;

function StringToTypedVariant(s: string): variant;
var
  c: char;
  bCanInt: boolean;
  bCanFloat: boolean;
begin
  s := lowercase(s);
  if s = '' then
    exit('');
  if s = 'null' then
    exit(null);
  if s = 'true' then
    exit(true);
  if s = 'false' then
    exit(false);

  bcanInt := true;
  bCanFloat := true;
  for c in s do begin
    if not charinset(c, ['-','0','1','2','3','4','5','6','7','8','9']) then
      bCanInt := false;
    if not charinset(c, ['-','.','E','0','1','2','3','4','5','6','7','8','9']) then
      bCanFloat := false;
    if not (bCanInt or bCanFloat) then
      break;
  end;

  if bCanInt then begin
    try
      exit(strtoint64(s));
    except
      exit(s);
    end;
  end;

  if bCanFloat then begin
    try
      exit(strtofloat(s));
    except
      exit(s);
    end;
  end;


  exit(s);





end;

function IsVarString(v: variant): boolean;
begin
  result := (vartype(v) = varString) or (vartype(v) = varUString) or (vartype(v) = varOleStr) or (varType(v) = 0 (*null string*));// or (varType(v) = v);
end;

function VartoStrEx(v: variant): string;
begin
  if vartype(v) = varNull then
    exit('null');
  exit(vartostr(v));
end;

function TProgress.PercentComplete: single;
begin
  if StepCount = 0 then
    result := 0
  else
    result := Step/StepCount;
end;


procedure TVolatileProgression.Reset;
begin
  StepsCompleted := 0;
  Complete := false;
end;



end.

