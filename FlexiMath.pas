unit FlexiMath;

{$IFNDEF ALLOW_FLEXIMATH}
//{$ERROR FlexiMath is deprecated.  Use SpreadTree instead or force with ALLOW_FLEXIMATH}
{$ENDIF}

interface


uses
  numbers, classes, jsonhelpers, systemx, stringx, betterobject, typex, Generics.Collections.Fixed, variants, sysutils;

type
  TJSONScalar = TJSON;
  TJSONArray = TJSON;



  TFlexiMath = class(TDictionary<string, IHolder<TJSON>>)
  public
    LastScriptError: string;
    function HasNode(sAddr: string): boolean;
    function GetNode(sAddr: string): TJSON;
    function AddNew<TJ: TJSON, constructor>(sName: string): TJ;
    function ToJSON: string;
    procedure FromJSON(s: string);
    function ToQuickSave: string;
    procedure FromQuickSave(s: string);
    function CalcArrayTimeStampRate(iArrayStart, iArrayEnd: ni; sAddr: string): double;
    function Calc(sScript: string): IHolder<TJSON>;

  end;


  TJSONOp = class(TJSON)
  protected
    procedure DoCalculate;virtual;abstract;
  public
    fm: TFlexiMath;
    procedure Calculate;
  end;

  TJSONBinaryOp = class(TJSONOp)
  protected
  public
    LeftOpAddr: string;
    RightOpAddr: string;
  end;

  TJSONUnaryOp = class(TJSONOp)
  protected
  public
    SourceAddr: string;
  end;

  TJSONOp_Abs = class(TJSONUnaryOp)
  public
    procedure DoCalculate; override;
  end;

  TJSONOp_Add = class(TJSONBinaryOp)
  public
    procedure DoCalculate; override;
  end;







function JSONAdd(j1,j2: TJSONScalar): IHolder<TJSON>;
function JSONSub(j1,j2: TJSONScalar): IHolder<TJSON>;
function JSONMul(j1,j2: TJSONScalar): IHolder<TJSON>;
function JSONDiv(j1,j2: TJSONScalar): IHolder<TJSON>;
function JSONMin(j1,j2: TJSONScalar): IHolder<TJSON>;overload;
function JSONMax(j1,j2: TJSONScalar): IHolder<TJSON>;overload;
function JSONLT(j1,j2: TJSONScalar): IHolder<TJSON>;
function JSONGT(j1,j2: TJSONScalar): IHolder<TJSON>;
function JSONLTE(j1,j2: TJSONScalar): IHolder<TJSON>;
function JSONGTE(j1,j2: TJSONScalar): IHolder<TJSON>;
function JSONNE(j1,j2: TJSONScalar): IHolder<TJSON>;



function JSONToScalar(Item: TJSON): IHolder<TJSON>;
function JSONToArray(arraybase: TJSON; subItem: string = ''): IHolder<TJSON>;
//function JSONMin(j1: TJSON): IHolder<TJSON>;overload;
//function JSONMax(j1: TJSON): IHolder<TJSON>;overload;

function StrToFM(s: string): TFlexiMath;
function StrToFMh(s: string): IHolder<TFlexiMath>;
function JStrToFMh(s: string): IHolder<TFlexiMath>;
function JStrToFM(s: string): TFlexiMath;


implementation

function JSONAdd(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value + j2.value;
end;

function JSONToScalar(Item: TJSON): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := item.value;
end;

function JSONSub(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value - j2.value;
end;
function JSONMul(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value * j2.value;
end;
function JSONDiv(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value * j2.value;
end;

function JSONMin(j1,j2: TJSONScalar): IHolder<TJSON>;overload;
begin
  result.o := TJSON.create;
  if j1.value <= j2.value then
    result.o.value := j1.value
  else
    result.o.value := j2.value;
end;

function JSONLT(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value < j2.value;
end;

function JSONGT(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value > j2.value;
end;

function JSONLTE(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value <= j2.value;
end;

function JSONGTE(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value >= j2.value;
end;

function JSONNE(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value <> j2.value;
end;




function JSONMax(j1,j2: TJSONScalar): IHolder<TJSON>;overload;
begin
  result.o := TJSON.create;
  if j1.value >= j2.value then
    result.o.value := j1.value
  else
    result.o.value := j2.value;
end;


function JSONToArray(arraybase: TJSON; subItem: string = ''): IHolder<TJSON>;
var
  t: ni;
begin
  result.o := TJSON.create;
  for t:= 0 to arraybase.iCount-1 do begin
    if subitem = '' then
      result.o.AddIndexed(arraybase[t])
    else
      result.o.AddIndexed(arraybase[t][subItem]);
  end;

end;

{ TFlexiMath }


function TFlexiMath.AddNew<TJ>(sName: string): TJ;
var
  h: IHolder<TJSON>;
begin
  result := TJ.create;
  h := THolder<TJSON>.create;
  h.o := result;
  if result is TJSONOp then
    TJSONOp(result).fm := self;

  self.Add(sName, h);
end;

function TFlexiMath.Calc(sScript: string): IHolder<TJSON>;
begin
  result := nil;
  if length(sScript) < 1 then begin
    LastScriptError := 'Blank Script';
    exit;
  end;
  if length(sScript) < 2 then begin
    LastScriptError := 'A valid script must have at least 2 characters';
    exit;
  end;

  if sScript[STRZ] <> '=' then begin
    LAstScriptError := 'A Script must start with =';
    exit;
  end;






end;

function TFlexiMath.CalcArrayTimeStampRate(iArrayStart, iArrayEnd: ni;
  sAddr: string): double;
var
  s1, s2: string;
  jn,jnSub: TJSOn;
  t: ni;
  dtEnd, dtStart: TDateTime;
  collapsedIdx: ni;
  function IsUnique(idx: ni): boolean;
  var
    dx: ni;
    jn2, jn3: TJSON;
  begin
    jn2 := jn[t];
    for dx := idx-1 downto 0 do begin
      jn3 := jn[dx];
      if jn2['Quantity'].Value = jn3['Quantity'].Value then
        exit(false);
      if jn2['Price'].Value = jn3['Price'].Value then
        exit(false);

    end;
    exit(true);
  end;

begin
  if not SplitString(sAddr, '.[].', s1,s2) then
    raise ECritical.create('Excpecting .[]. to mark iterator in address: '+sAddr)
  else begin
    jn := GetNode(s1);

    if jn.iCount = 0 then
      exit(0.0);
    //----
    t := 0;//greaterof(0, jn.iCount-(iArrayEnd+1));
    if s2 <> '' then
      jnSub := jn[t].GetNode(s2)
    else
      jnSub := jn[t];

    dtEnd := btxDateToDateTime(jnSub.value);

    t := iArrayStart;
    collapsedIdx := 0;//TO FILTER OUT bot traffic (repeated order chains)
    while (t < iArrayEnd) and (t<jn.iCount) do begin
      if IsUnique(t) then
        inc(collapsedIdx);
      inc(t);
    end;
    //----
    t := lesserof(jn.icount-1, iArrayEnd);
    if s2 <> '' then
      jnSub := jn[t].GetNode(s2)
    else
      jnSub := jn[t];

    dtStart := btxDateToDateTime(jnSub.value);

    result := greaterof(0.0,LocalTimeToGMT(now)-dtStart);



  end;

end;

procedure TFlexiMath.FromJSON(s: string);
var
  h: IHolder<TJSON>;
  t: ni;
  n: string;
begin
  self.Clear;
  h := THOLDER<TJSON>.create;
  h.o := StrtoJSON(s);
  for t:= 0 to h.o.nCount-1 do begin
    n := h.o.named.Keys[t];
    self.AddNew<TJSON>(n).FromString(h.o[n].ToJson);
  end;


end;

procedure TFlexiMath.FromQuickSave(s: string);
var
  t: ni;
  n: string;
  ns: TArray<string>;
  sl: TStringlist;
  s1, s2: string;
  bForceFM: boolean;
begin
  self.Clear;

  sl := nil;
  try
    bforceFM := zcopy(s, 0,2) = 'FM';
    if bForceFM then
      s := zcopy(s,2,length(s));

    if not bForceFM then
      bForceFM := (zcopy(s, 0, 1) = '"');

    if bForceFM then begin
      sl := stringToStringList(s);
      for t:= 0 to sl.count-1 do begin
        SplitString(sl[t], ':', s1,s2);
        s1 := unquote(s1);
        self.AddNew<TJSON>(s1).FromString(s2);
      end;
    end else begin
      FromJSON(s);
    end;
  finally
    sl.free;
  end;

end;

function TFlexiMath.GetNode(sAddr: string): TJSON;
var
  s1,s2: string;
begin
  try
    SplitString(sAddr, '.', s1,s2);
    exit(self[s1].o.GetNode(s2));

  except
    raise EJSONNodeNotFound.create(sAddr+' was not found.');
  end;

end;




function TFlexiMath.HasNode(sAddr: string): boolean;
var
  s1,s2: string;
  k: TArray<string>;
  t: ni;
begin
  SplitString(sAddr, '.', s1,s2);
  k := self.keys.ToArray;
  for t:= 0 to high(k) do
    if k[t]=s1 then
      exit(self[s1].o.Hasnode(s2));

  exit(false);


end;

function TFlexiMath.ToJSON: string;
var
  t: ni;
  k: string;
  v: IHolder<TJSON>;
  ks: TArray<string>;
  vs: Tarray<IHolder<TJSON>>;
begin
  result := '{';
  ks := self.Keys.ToArray;
  vs := self.Values.ToArray;
  for t:= 0 to high(ks) do begin
    if t > 0 then
      result := result + ',';
    k := ks[t];
    v := vs[t];
    result := result + quote(k)+': '+v.o.ToJson;
  end;

  result := result + '}';

end;

function TFlexiMath.ToQuickSave: string;
var
  t,tt: ni;
  k: string;
  v: IHolder<TJSON>;
  ks: TArray<string>;
  vs: Tarray<IHolder<TJSON>>;
  s,ss: string;
begin
  ks := self.Keys.ToArray;
  vs := self.Values.ToArray;


  ss := '';

  if high(vs) <> high(ks) then
    raise ECritical.create('wtf');

  result := '';
  for t:= 0 to high(ks) do begin
    k := ks[t];
    v := vs[t];

    s := v.o.ToJson;
    s := stringreplace(s, CRLF, '', [rfReplaceAll]);
    s := (quote(k)+': ')+s+CRLF;

    result := result + s;
    v := nil;
  end;
  result := 'FM'+result;
  setlength(ks, 0);
  setlength(vs, 0);

end;

{ TJSONOp_Abs }

procedure TJSONOp_Abs.DoCalculate;
begin
  self.fromstring(vartostr(abs(fm.GetNode(SourceAddr).value)));
end;

{ TJSONOp_Add }

procedure TJSONOp_Add.DoCalculate;
var
  v1,v2: variant;
begin
  v1 := fm.GetNode(LeftOpAddr).value;
  v2 := fm.GetNode(RightOpAddr).value;
  self.FromString(vartostr(v1+v2));

end;

{ TJSONOp }

procedure TJSONOp.Calculate;
begin
  if fm = nil then
    raise ECritical.create('set fm in '+self.classname+' before calling calculate.');
  DoCalculate;
end;

function StrToFM(s: string): TFlexiMath;
begin
  result := TFlexiMath.create;
  result.FromQuickSave(s);
end;

function JStrToFM(s: string): TFlexiMath;
begin
  result := TFlexiMath.create;
  result.FromJSOn(s);
end;

function JStrToFMh(s: string): IHolder<TFlexiMath>;
begin
  result := THolder<TFlexiMath>.create;
  result.o := JStrtoFM(s);
end;

function StrToFMh(s: string): IHolder<TFlexiMath>;
begin
  result := THolder<TFlexiMath>.create;
  result.o := StrtoFM(s);
end;






end.
