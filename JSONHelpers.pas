unit JSONHelpers;

interface

uses
  generics.collections.fixed, classes, typex, stringx, sysutils, https, json, betterobject, debug, variants, systemx,numbers, tickcount, sharedobject, better_collections;

type
  EJSONNodeNotFound = class(Exception);
  TJSON = class;//forward

  TIterateCalculateNewFieldProc = reference to procedure (recordNode: TJSON; newNode: TJSON);
  TFilterProcRef = reference to procedure (recordNode: TJSON; var bAccept: boolean);

  TFindNodeRes = record
    success: boolean;
    FoundAddr: string;
    node: TJSON;
    procedure Init;
  end;
  TJSON = class(TStringObjectList<TJSON>)
  private
    FjsonInput: string;
    FAddr: string;
    FParent: TJSON;
    procedure Parse(s: string; var iPosition: nativeint);
    procedure ParseObject(s: string; var iPosition: nativeint);
    procedure ParseArray(s: string; var iPosition: nativeint);
    procedure ParseConstant(s: string; var iPosition: nativeint);
    procedure ClearAndFree;
    function GetAsString: string;
    procedure SetAsString(const Val: string);
    function GetiCount: ni;
    function GEtnCount: ni;
    function GetnValuesByIndex(idx: ni): TJSON;
    function GetnNamesByIndex(idx: ni): string;
    function valsByString(s: string): TJSON;
    function valsByIndex(ii: ni): TJSON;
    function ValsByVariant(v: variant): TJSON;
    function GetJSON: string;
    procedure SetJSONInput(const Value: string);
    function GetAddr: string;
    function GetEnableDebug: boolean;
    procedure SetEnableDebug(const Value: boolean);

    property nValuesByIndex[idx: ni]: TJSON read GetnValuesByIndex;
    property nNamesByIndex[idx: ni]: string read GetnNamesByIndex;
  public

    name: string;
    value: variant;
    named: TStringObjectList<TJSON>;
    indexed: array of TJSON;
    parent: TJSON;

    expires: Tdatetime;
    constructor create;override;
    destructor destroy;override;
    procedure FromString(s: string);
    property AsString: string read GetAsString write SetAsString;
    property iCount: ni read GetiCount;
    property nCount: ni read GEtnCount;
    procedure Sorti(sSubValue: string; bDesc: boolean = false);
    procedure Deletei(index: ni);
    property s[s: string]: TJSON read valsByString;
    property v[v: variant]: TJSON read ValsByVariant;default;
    property a[ii: ni]: TJSON read valsByIndex;
    function IndexOfSubValue(subkey: string; value: string): ni;
    function FindSubKey(subkey: string; value: string): TJSON;
    function IsExpired: boolean;
    function AddIndexed(v_will_copy: TJSON): ni;overload;
    procedure ReAddr(newaddr: string);
    function AddIndexed(stringPrimitive: string): ni;overload;
    function AddIndexed(): TJSON;overload;
    function GetNode(sAddr: string): TJSON;
    property ToJson: string read GetJSON write FJSonInput;
    function ToHolder: IHolder<TJSON>;
    property OriginalJSON: string read FjsonInput write SetJSONInput;
    procedure AddMemberJSON(const Key: string; value: string);overload;
    procedure AddMemberPrimitive(const Key: string; value: string);overload;
    procedure AddMemberPrimitive(const Key: string; value: double);overload;
    procedure AddMemberPrimitiveVariant(const Key: string; v: variant);overload;
    function AddMember(const Key: string): TJSON;overload;
    function LookUpBySubObjectField(fld: string; sSubValue: variant): TJSON;
    procedure MergeFieldsFrom(o: TJSON);
    procedure Iterate_CalculateField(sNewFieldName: string; procref: TIterateCalculateNewFieldProc);
    procedure Iterate_Filter(procref: TFilterProcRef);
    procedure AppendFieldsFrom(o: TJSON);
    function HasNode(sAddr: string): boolean;
    function FindNodeEx(sAddr: string; nParent: TJSON = nil): TFindNodeRes;

    function TryGetValue(sKey: string; out sValue: string): boolean;
    property CalcAddr: string read GetAddr;
    property Addr: string read FAddr write FAddr;
    property json: string read getJSON write FromString;
    property enable_debug: boolean read GetEnableDebug write SetEnableDebug;
  end;

  IJSONHolder = IHolder<TJSON>;

  TJSONCacheRec = record
    j: IJSONHolder;
    created: ticker;
    doctype: string;
  end;
  TJSONCache = class(TSharedObject)
  private
    FList: TList<TJSONCacheRec>;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Clear;
    function IndexOf(docType: string): ni;
    function Find(docType: string): TJSONCacheRec;
    function Count: ni;
    procedure Add(docType: string; jh: IJSONHolder);
    procedure Expire(iOlderThan: ticker);overload;
    procedure Expire(docType: string);overload;
  end;


function JSONtoDictionary(sJSON: string): TJSON;
function StrtoJSON(sString: string): TJSON;

function StrToJSONh(sString: string): IHolder<TJSON>;
function CreateJSONValue(v: variant): TJSON;
function JSONValueString(v: variant): string;
function JSONEsc(s: string): string;
function JSONArrayToCCode(n: TJSON): string;
function JSONUnionOf(o1,o2: TJSON; s1FieldMatch, s2FieldMatch: string): IHolder<TJSON>;

//function StringToJSON(sJSON: string): TJSONObject;

//function JSONGetTableFields(json: TJSONObject): TArray<string>;

var
  cunt: ni;
implementation

function StrtoJSON(sString: string): TJSON;
begin
  result := TJSON.create;
  result.FromString(sString);
end;

function StrToJSONh(sString: string): IHolder<TJSON>;
begin
  result := THolder<TJSON>.create;
  result.o := StrToJSON(sString);
end;


function JSONArrayToCCode(n: TJSON): string;
var
  t: ni;
begin
  result := '{';
  for t:= 0 to n.iCount-1 do begin
    result := result + n[t].ToJson+',';
  end;
  result := result + '}';

end;

function JSONtoDictionary(sJSON: string): TJSON;
begin
  result := strtojson(sJSON);
end;
(*var
  s1,s2: string;
  sl: TStringlist;
  t: ni;
begin
  result := TJSON.create;
  SplitString(sJSON, '{', s1,s2);
  SplitString(s2, '}', s1,s2);
  sl := SplitStringIntoStringList(s1, ',', '"');
  try
    for t:= 0 to sl.Count-1 do begin
      SplitString(sl[t], ':', s1,s2);
      s1 := Unquote(trim(s1));
      SplitString(s2,',' ,s2,sJSON);
      s2 := unquote(trim(s2));
      result.Add(s1, s2);
    end;
  finally
    sl.free;
  end;
end;*)

{ TJSON }



function TJSON.AddIndexed(v_will_copy: TJSON): ni;
begin
  result := length(indexed);
  setlength(self.indexed, result+1);
//  self.i[high(indexed)] := TJSON.create;
  self.indexed[high(indexed)] := StrToJson(v_will_copy.ToJSON);
  indexed[high(indexed)].ReAddr(self.Addr+'['+inttostr(high(indexed))+']');


end;

procedure TJSON.AddMemberPrimitive(const Key: string; value: double);
var
  j: TJSON;
begin
  j := TJSON.create;
  try
    j.fromString(floatprecision(value, 12));
    self.named.Add(key, j);
    j.ReAddr(AOR(self.Addr,'.',key));
  except
    j.free;
    raise;
  end;

end;

procedure TJSON.AddMemberPrimitiveVariant(const Key: string;
  v: variant);
var
  j: TJSON;
begin
  j := TJSON.create;
  j.FromString(VArtoJSONStorage(v));
  self.named.Add(key, j);
  j.ReAddr(AOR(self.Addr,'.',key));
end;

procedure TJSON.AppendFieldsFrom(o: TJSON);
var
  f: ni;
  sKey: string;
  val: variant;
begin
  for f:= 0 to o.nCount-1 do begin
    sKey := o.named.Keys[f];
    val := o.Named.ItemsByIndex[f].value;
    self.AddMemberPrimitiveVariant(sKey,val);
  end;

end;

procedure TJSON.ClearAndFree;
var
//  a: TArray<TPair<string,TJSON>>;
  d: TJSON;
  p: TPair<string,TJSON>;
begin
  Fjsoninput := '';
  for d in indexed do begin
    d.Free;
  end;
  setlength(indexed,0);

//  a := named.ToArray;
  named.Clear;
//  for p in a do begin
//    p.Value.Free;
//  end;

end;

constructor TJSON.create;
begin
  inherited create;
  setlength(indexed,0);
  named := TStringObjectList<TJSON>.create;
  named.takeownership := true;
//  inc(cunt);
//  Debug.Log(inttostr(cunt));
end;

procedure TJSON.Deletei(index: ni);
var
  t: ni;
begin
  for t:= index to high(indexed)-1 do begin
    indexed[t] := indexed[t+1];
    indexed[t].ReAddr(self.Addr+'['+inttostr(t)+']');
  end;
  SetLength(indexed, length(indexed)-1);

end;

destructor TJSON.destroy;
BEGIN
  ClearAndFree;
  named.free;
  named := nil;
//  dec(cunt);
//  Debug.Log(inttostr(cunt));

  inherited;
end;


function TJSON.FindNodeEx(sAddr: string; nParent: TJSON): TFindNodeRes;
var
  t: ni;
  n: TJSON;
begin
  result.init;
  //check self as node first
  if self.HasNode(sAddr) then begin
    result.success := true;
    result.node := self.GetNode(sAddr);
    result.FoundAddr := result.node.Addr;
  end;
  if not result.success then begin

    for t:= 0 to named.Count-1 do
    begin
      n := named.ItemsByIndex[t];
      if n.HasNode(sAddr) then begin
        result.success := true;
        result.node := n.GetNode(sAddr);
        result.FoundAddr := result.node.addr;
        exit;
      end;
    end;
  end;

  if not result.success then begin
    for t:= 0 to high(indexed) do
    begin
      n := indexed[t];
      if n.HasNode(sAddr) then begin
        result.success := true;
        result.node := n.GetNode(sAddr);
        result.FoundAddr := result.node.addr;
        exit;
      end;
    end;
  end;
end;

function TJSON.FindSubKey(subkey, value: string): TJSON;
var
  ii: ni;
begin
  result := nil;
  ii := IndexOfSubValue(subkey, value);
  if ii >= 0 then
    exit(indexed[ii]);

end;

procedure TJSON.FromString(s: string);
var
  i: nativeint;
begin
  i := STRZ;
  ClearAndFree;
  FjsonInput := s;
  parse(s, i);
end;


function TJSON.GetAddr: string;
begin
  result := name;
  if parent <> nil then
    result := AOR(parent.calcaddr,'.',result);
end;

function TJSON.GetAsString: string;
begin
  result := vartostrex(value);
end;

function TJSON.GetEnableDebug: boolean;
begin
  result := named.enable_debug;
end;

function TJSON.GetiCount: ni;
begin
  result := length(indexed);
end;

function TJSON.GetJSON: string;
var
  t: ni;
  a: TArray<String>;
  v: TArray<variant>;
  s: string;
begin
  result := '';

  if iCount > 0 then begin
    s := '[';

    for t:= 0 to iCount-1 do begin
      if t > 0 then
        s := s + ',';
      s := s + indexed[t].GetJSON;
      if (t and 15)=0 then
        s := s + CRLF;
    end;
    s := s + ']'+CRLF;
    result := result + s;

  end else begin
    if GetNCount > 0 then begin
      s := '{';
      for t:= 0 to GEtnCount-1 do begin
        if t > 0 then
          s := s + ',';
        s := s + quote(self.nNamesByIndex[t])+':'+nValuesByIndex[t].GetJSON;
      end;
      s := s+'}';
      result := s;
    end else begin
//      debug.Log(inttostr(vartype(value))+'='+vartostr(value));

      result := JSONValueString(value);
    end;

  end;






end;

function TJSON.GEtnCount: ni;
begin
  result := named.Count;
end;




function TJSON.GetnNamesByIndex(idx: ni): string;
begin
  result := named.Keys[idx];
end;

function TJSON.GetNode(sAddr: string): TJSON;
var
  s1,s2,s3,s4: string;
  i: nativeint;
  s: string;
begin
  try
    if sAddr = '' then
      exit(self);

    splitString(sAddr, '.', s1,s2);
    splitstring(sAddr, '[', s3,s4);
    if (length(s3) < length(s1)) and (length(s3) > 0) then begin
      s1 := s3;
      s2 := '['+s4;

    end;
    if s1 = '' then
      exit(self);

    if zcopy(s1,0,1) = '[' then begin
      if zcopy(s1,length(s1)-1, 1) <> ']' then
        raise ECritical.create('Expecting '']''');
      s1 := zcopy(s1, 1,length(s1)-2);
    end;
    if IsInteger(s1) then begin
      i := strtoint64(s1);
      exit(self[i].GetNode(s2));
    end else begin
      exit(self[s1].GetNode(s2));
    end;
  except
    raise EJSONNodeNotFound.create(sAddr+' was not found');

  end;


end;

function TJSON.GetnValuesByIndex(idx: ni): TJSON;
begin
  result := named.ItemsByIndex[idx];
end;


function TJSON.HasNode(sAddr: string): boolean;
var
  s1,s2: string;
//  k: TArray<string>;
  t: ni;
begin
  if sAddr = '' then
    exit(true);
  SplitString(sAddr, '.', s1,s2);

  if IsInteger(s1) then begin
    exit(strtoint(s1) < iCount);
  end else begin
    for t:= 0 to self.named.Count-1 do
      if self.named.Keys[t]=s1 then
        exit(named[s1].Hasnode(s2));


  end;

  exit(false);


end;

function TJSON.IndexOfSubValue(subkey, value: string): ni;
var
  t: ni;
begin
  for t:= 0 to icount-1 do begin
    if comparetext(indexed[t][subkey].AsString, value)=0 then
      exit(t);
  end;
  exit(-1);
end;

function TJSON.IsExpired: boolean;
begin
  result := (expires <> 0.0) and (now > expires);
end;

procedure TJSON.Iterate_CalculateField(sNewFieldName: string;
  procref: TIterateCalculateNewFieldProc);
var
  t: ni;
  newnode: TJSON;
begin
  for t:= 0 to Self.iCount-1 do begin
    if not self[t].HasNode(sNewFieldName) then begin
      newNode := self[t].AddMember(sNewFieldName);
    end else
      newNode := Self[t].GetNode(sNewFieldName);
    procref(self[t], newNode);
  end;

end;

procedure TJSON.Iterate_Filter(procref: TFilterProcRef);
var
  t: ni;
  newnode: TJSON;
  accept: boolean;
begin
  t := 0;
  while t < self.iCount do begin
    accept := true;
    procref(self[t], accept);
    if accept then
      inc(t)
    else
      Deletei(t);
  end;
end;

function TJSON.LookUpBySubObjectField(fld: string;
  sSubValue: variant): TJSON;
var
  t: ni;
begin
  result := nil;
  for t:= 0 to High(indexed) do begin
    if self.indexed[t].HasNode(fld) then begin
      if self.indexed[t][fld].value = sSubValue then
        exit(indexed[t]);
    end;
  end;


end;

procedure TJSON.MergeFieldsFrom(o: TJSON);
var
  t: ni;
begin
  for t:= 0 to o.nCount do begin
    self.AddMemberPrimitiveVariant(o.nNamesByIndex[t],o.valsByIndex(t).value);
  end;
end;

procedure TJSON.Parse(s: string; var iPosition: nativeint);
type
  TParseState = (psStart, psValue, psName);
var
  c: char;
  start: nativeint;
begin
  start := iPosition;
  while iPosition <= high(s) do begin
    c := s[iPosition];
    if c = '{' then begin
      inc(iPosition);
      ParseObject(s, iPosition);
      exit;
    end else
    if c = '[' then begin
      inc(iPosition);
      ParseArray(s, iPosition);
      exit;
    end else
    if not charinset(c, [' ',',',#9, CR, LF]) then begin
      ParseConstant(s, iPosition);
      exit;
    end;

    inc(iPosition);
    FjsonInput := copy(s, start, iposition-start);

  end;
end;

function DemonstrateErrorPosition(s: string; iPosition: nativeint): string;
var
  iStart: ni;
  iError: ni;
  iEnd: ni;
begin
  iStart := greaterof(strz, iPosition - 20);
  iError := iPosition;
  iEnd := iError + 20;
  result := zcopy(s, iStart, iError-iStart)+'*ERR*'+zcopy(s,iPosition, 20);


end;
procedure TJSON.ParseArray(s: string; var iPosition: nativeint);
var
  sValue: string;
  jsn: TJSON;
  c: char;
  bWaitForNext: boolean;
begin
  sValue := '';
  bWaitforNext := false;
  setlength(indexed, 0);
//  Debug.Log('Parsing Array');
  while iPosition <= high(s) do begin
    c := s[iPosition];

    if not bWaitForNext then begin
      if c = ']' then begin
        exit;
      end;
      jsn := TJSON.create;
      jsn.Addr := self.addr+'['+inttostr(length(indexed))+']';
      jsn.Parent := self;
      jsn.Parse(s, iPosition);
      setlength(indexed, length(indexed)+1);
      indexed[high(indexed)] := jsn;
      bWaitForNext := true;
    end else begin
      if c = ']' then begin
//        Debug.Log('Found '+inttostr(length(i))+' elements');
        exit;
      end else
      if c = ',' then begin
        //next index do something?
        bWaitForNext := false;
      end;
    end;

    inc(iPosition);
  end;
  raise Exception.create('array not terminated @'+ demonstrateErrorPosition(s, iPosition));
end;

procedure TJSON.ParseConstant(s: string; var iPosition: nativeint);
var
  bInQuotes: boolean;
  sValue: string;
  bEscape : boolean;
  c: char;
  bQuoted: boolean;
begin
  sValue := '';
  bEscape := false;
  bInQuotes := false;
  bQuoted := false;
  while iPosition <= high(s) do begin
    c := s[iPosition];
    if not bInQuotes then begin
      if c = '"' then begin
        bInQuotes := not bInQuotes;
        bQuoted := true;
      end else
      if not charinset(c, [' ',',',']','[','{','}',':',#9, CR, LF]) then begin
        sValue := sValue + c;
      end else begin
        Value := JavaScriptStringtoTypedVariant(sValue);

        dec(iPosition);
        exit;//<<----end
      end;

    end else begin
      if c = '\' then begin
        if bEscape then begin
          sValue := sValue + c;
          bEscape := false;
        end else
          bEscape := true;
      end else
      if c = '"' then begin
        if bEscape then begin
          sValue := sValue + c;
          bEscape := false;
        end else begin
          bInQuotes := false;
          if bQuoted then
            Value :=sValue
          else
            Value :=stringtoTypedVariant(sValue);
          exit;//<<-----end
        end;
      end else
        sValue := sValue + c;
    end;
    inc(iPosition);
  end;

  Value := stringtoTypedVariant(sValue);//DONT USE FINALLY, if we get here, then we didn't exit some toher way, need to commit

end;

procedure TJSON.ParseObject(s: string; var iPosition: nativeint);
type
  TParseState = (psName, psValue, psLimbo);
var
  bInQuotes: boolean;
  sName: string;
  bEscape : boolean;
  c: char;
  ps : TParseState;
  jsn: TJSON;
begin
  sName := '';
  bEscape := false;
  bInQuotes := false;
  ps := psName;
  while iPosition <= high(s) do begin
    c := s[iPosition];
    case ps of
      psName: begin
        if not bInQuotes then begin
          if c = '}' then
            exit;
          if c = '"' then
            bInQuotes := not bInQuotes;

          if c = ':' then begin
            ps := psValue;
          end;
          if c = ',' then begin
            raise Exception.create('expecting a colon got a comma');
          end;
        end else begin
          if c = '\' then begin
            if bEscape then begin
              sName := sName + c;
              bEscape := false;
            end else
              bEscape := true;
          end else
          if c = '"' then begin
            if bEscape then begin
              sName := sName + c;
              bEscape := false;
            end else begin
              bInQuotes := false;
            end;
          end else
            sName := sName + c;
        end;
      end;
      psValue: begin
        jsn := TJSON.create;
        jsn.Addr := AOR(self.addr,'.',sName);
        jsn.parent := self;
        jsn.Parse(s, iPosition);
        self.named.Add(sName, jsn);
        ps := psLImbo;
      end;
      psLimbo: begin
        if c = '}' then
          exit;
        if c = ',' then begin
          ps := psName;
          sName := '';
        end;
      end;
    end;
    inc(iPosition);
  end;

end;

procedure TJSON.ReAddr(newaddr: string);
var
  t: ni;
begin
  Addr := newaddr;
  for t := 0 to high(indexed) do begin
    indexed[t].ReAddr(newaddr+'['+inttostr(t)+']');
  end;
  for t := 0 to named.Count-1 do begin
    if newAddr = '' then
      named.ItemsByIndex[t].ReAddr(Self.nNamesByIndex[t])
    else
      named.ItemsByIndex[t].ReAddr(AOR(newaddr,'.',Self.nNamesByIndex[t]));
  end;
end;

procedure TJSON.SetAsString(const Val : string);
begin
  value := stringtotypedvariant(val);
end;

procedure TJSON.SetEnableDebug(const Value: boolean);
begin
  named.enable_debug := true;
end;

procedure TJSON.SetJSONInput(const Value: string);
begin
  Self.FromString(value);
end;

procedure TJSON.Sorti(sSubValue: string; bDesc: boolean = false);
var
  bDone: boolean;
  t: ni;
  procedure Swap(i1,i2: ni);
  var
    js: TJSON;
  begin
    js := indexed[i1];
    indexed[i1] := indexed[i2];
    indexed[i2] := js;
  end;
begin
  bDone := false;
  while not bDone do begin
    bDone := true;
    for t:= 0 to icount-2 do begin
      if bDesc then begin
        if self.indexed[t].named[sSubValue].Value > self.indexed[t+1].named[sSubValue].Value then begin
          Swap(t,t+1);
          bDone := false;
        end;
      end else begin
        if self.indexed[t].named[sSubValue].Value < self.indexed[t+1].named[sSubValue].Value then begin
          Swap(t,t+1);
          bDone := false;
        end;
      end;
    end;
  end;

end;

function TJSON.ToHolder: IHolder<TJSON>;
begin
  result := THOLDER<TJSON>.create;
  result.o := StrtoJSON(self.GetJSON);

end;

function TJSON.TryGetValue(sKey: string; out sValue: string): boolean;
begin
  if not self.HasNode(sKey) then
    exit(false);
  sValue := vartostrex(self[sKey].value);
  result := true;
end;

function TJSON.valsByIndex(ii: ni): TJSON;
begin
  if ii >= iCount then
    raise ECritical.create('Trying to chase index '+ii.tostring+' in JSON array of '+iCount.tostring+' elements.');
  result := indexed[ii];
end;

function TJSON.valsByString(s: string): TJSON;
begin
  result := named[s];
end;

function TJSON.ValsByVariant(v: variant): TJSON;
begin
  if varType(v) in [varInteger, varInt64, varSmallint, varByte, varUInt64] then
    exit(a[v])
  else
    exit(s[v]);

end;

function CreateJSONValue(v: variant): TJSON;
begin
  result := TJSON.create;
  result.value := v;
end;

function JSONValueString(v: variant): string;
begin
  if IsVarString(v) then begin
    result := '"'+StringReplace(vartostr(v),'\','\\',[rfReplaceAll])+'"';
  end
  else
  if VarType(v)=varBoolean then
    result := uppercase(vartostr(v))
  else
    result := vartostr(v);

end;

procedure TJSON.AddMemberJSON(const Key: string;
  value: string);
var
  j: TJSON;
begin
  j := TJSON.create;
  j.fromString(value);
  self.named.Add(key, j);
  j.ReAddr(AOR(self.Addr,'.',key));

end;

procedure TJSON.AddMemberPrimitive(const Key: string;
  value: string);
var
  j: TJSON;
begin
  j := TJSON.create;
  j.value := value;
  self.named.Add(key, j);
  j.ReAddr(AOR(self.Addr,'.',key));
  if self.named[key].tojson <> j.tojson then
    raise ECritical.create('WTF!');

  if self.named[key].value <> j.value then
    raise ECritical.create('WTF!');
end;


function JSONEsc(s: string): string;
begin
  result := s;
  result := stringreplace(result, '"', '\"', [rfReplaceAll]);
end;

function TJSON.AddIndexed: TJSON;
var
  len: ni;
begin
  len := length(indexed);
  setlength(self.indexed, len+1);
  result := TJSON.create;
  self.indexed[high(indexed)] := result;
  result.ReAddr(self.Addr+'['+inttostr(high(indexed))+']');

end;

function TJSON.AddIndexed(stringPrimitive: string): ni;
begin

  AddIndexed().value := stringPrimitive;
  result:= icount-1;
end;

function TJSON.AddMember(const Key: string): TJSON;
begin
  result := TJSON.create;
  self.named.Add(key, result);
  result.ReAddr(AOR(self.Addr,'.',mkey));
end;

{ TJSONCache }

procedure TJSONCache.Add(docType: string; jh: IJSONHolder);
var
  l: ILock;
  rec: TJSONCacheRec;
begin
  l :=  locki;

  Expire(DocType);
  rec.j := jh;
  rec.created := getticker;
  rec.doctype := docType;
  FList.add(rec);

end;

procedure TJSONCache.Clear;
var
  l: ILock;
begin
  l :=  locki;
  FList.clear;



end;

function TJSONCache.Count: ni;
var
  l: ILock;
begin
  l :=  locki;
  result := FList.count;

end;

constructor TJSONCache.Create;
begin
  inherited;
  FList := TLIst<TJSONCacheRec>.create;

end;

destructor TJSONCache.Destroy;
begin
  Clear;
  fList.free;
  FList := nil;
  inherited;
end;

procedure TJSONCache.Expire(docType: string);
var
  l: ILock;
  i: ni;
begin
  l :=  locki;

  i := IndexOf(docType);
  if i>=0 then begin
    FList.Delete(i);
  end;

end;

procedure TJSONCache.Expire(iOlderThan: ticker);
var
  l: ILock;
  t: ni;
begin
  l :=  locki;

  for t:= 0 to count-1 do begin
    if gettimesince(FList[t].created) > iOlderThan then begin
      FList.Delete(t);
    end;
  end;

end;

function TJSONCache.Find(docType: string): TJSONCacheRec;
var
  l: ILock;
  i: ni;
begin
  result.j := nil;
  result.created := 0;
  result.doctype := '';

  l :=  locki;

  i := IndexOf(docType);
  if i < 0 then
    exit();

  result := FList[i];


end;

function TJSONCache.IndexOf(docType: string): ni;
var
  l: ILock;
  t: ni;
begin
  l :=  locki;

  for t:= 0 to count-1 do begin
    if FList[t].doctype = docType then begin
      Exit(t);
    end;
  end;

  exit(-1);

end;

function JSONUnionOf(o1,o2: TJSON; s1FieldMatch, s2FieldMatch: string): IHolder<TJSON>;
var
  t: ni;
  oFound: TJSON;
  newRec: TJSON;
  v: variant;
begin
  result := THolder<TJSON>.create;
  result.o := TJSON.create;
  for t:= 0 to o1.iCount-1 do begin
//    Debug.Log('Looking for '+s1FieldMatch+' in '+o1[t].ToJson);
    v := o1[t][s1FieldMatch].value;
//    Debug.Log('Lookup value '+s2FieldMatch+' in '+o2.ToJson);
    oFound := o2.LookUpBySubObjectField(s2FieldMatch,v);
    if oFound <> nil then begin
        newRec := TJSON.create;
        try
          newREc.AppendFieldsFrom(o1[t]);
          newRec.AppendFieldsFrom(oFound);
          result.o.AddIndexed(newRec);
        finally
          newRec.free;
        end;
    end;
  end;



end;

{ TFindNodeRes }

procedure TFindNodeRes.Init;
begin
  success := false;
end;

initialization
  cunt := 0;



end.
