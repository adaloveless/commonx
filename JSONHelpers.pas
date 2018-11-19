unit JSONHelpers;

interface

uses
  generics.collections.fixed, classes, typex, stringx, sysutils, https, json, betterobject, debug, variants, systemx,numbers, tickcount, sharedobject, better_collections;

type
  EJSONNodeNotFound = class(Exception);
  TJSONDictionary = class;//forward

  TIterateCalculateNewFieldProc = reference to procedure (recordNode: TJSONDictionary; newNode: TJSONDictionary);
  TFilterProcRef = reference to procedure (recordNode: TJSONDictionary; var bAccept: boolean);

  TJSONDictionary = class(TStringObjectList<TJSONDictionary>)
  private
    FjsonInput: string;
    procedure Parse(s: string; var iPosition: nativeint);
    procedure ParseObject(s: string; var iPosition: nativeint);
    procedure ParseArray(s: string; var iPosition: nativeint);
    procedure ParseConstant(s: string; var iPosition: nativeint);
    procedure ClearAndFree;
    function GetAsString: string;
    procedure SetAsString(const Val: string);
    function GetiCount: ni;
    function GEtnCount: ni;
    function GetnValuesByIndex(idx: ni): TJSONDictionary;
    function GetnNamesByIndex(idx: ni): string;
    function valsByString(s: string): TJSONDictionary;
    function valsByIndex(ii: ni): TJSONDictionary;
    function ValsByVariant(v: variant): TJSONDictionary;
    function GetJSON: string;
    procedure SetJSONInput(const Value: string);

    property nValuesByIndex[idx: ni]: TJSONDictionary read GetnValuesByIndex;
    property nNamesByIndex[idx: ni]: string read GetnNamesByIndex;
  public

    value: variant;
    named: TStringObjectList<TJSONDictionary>;
    indexed: array of TJSONDictionary;

    expires: Tdatetime;
    constructor create;override;
    destructor destroy;override;
    procedure FromString(s: string);
    property AsString: string read GetAsString write SetAsString;
    property iCount: ni read GetiCount;
    property nCount: ni read GEtnCount;
    procedure Sorti(sSubValue: string; bDesc: boolean = false);
    procedure Deletei(index: ni);
    property s[s: string]: TJSONDictionary read valsByString;
    property v[v: variant]: TJSONDictionary read ValsByVariant;default;
    property a[ii: ni]: TJSONDictionary read valsByIndex;
    function IndexOfSubValue(subkey: string; value: string): ni;
    function FindSubKey(subkey: string; value: string): TJSONDictionary;
    function IsExpired: boolean;
    function AddIndexed(v_will_copy: TJSONDictionary): ni;overload;
    function AddIndexed(stringPrimitive: string): ni;overload;
    function AddIndexed(): TJSONDictionary;overload;
    function GetNode(sAddr: string): TJSONDictionary;
    property ToJson: string read GetJSON write FJSonInput;
    function ToHolder: IHolder<TJSONdictionary>;
    property OriginalJSON: string read FjsonInput write SetJSONInput;
    procedure AddMemberJSON(const Key: string; value: string);overload;
    procedure AddMemberPrimitive(const Key: string; value: string);overload;
    procedure AddMemberPrimitive(const Key: string; value: double);overload;
    procedure AddMemberPrimitiveVariant(const Key: string; v: variant);overload;
    function AddMember(const Key: string): TJSONDictionary;overload;
    function LookUpBySubObjectField(fld: string; sSubValue: variant): TJSONDictionary;
    procedure MergeFieldsFrom(o: TJSONDictionary);
    procedure Iterate_CalculateField(sNewFieldName: string; procref: TIterateCalculateNewFieldProc);
    procedure Iterate_Filter(procref: TFilterProcRef);
    procedure AppendFieldsFrom(o: TJSONDictionary);

    function HasNode(sAddr: string): boolean;
    function TryGetValue(sKey: string; out sValue: string): boolean;
  end;

  TJSON = TJSONDictionary;
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


function JSONtoDictionary(sJSON: string): TJSONDictionary;
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
  result := TJSONDictionary.create;
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

function JSONtoDictionary(sJSON: string): TJSONDictionary;
begin
  result := strtojson(sJSON);
end;
(*var
  s1,s2: string;
  sl: TStringlist;
  t: ni;
begin
  result := TJSONDictionary.create;
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

{ TJSONDictionary }



function TJSONDictionary.AddIndexed(v_will_copy: TJSONDictionary): ni;
begin
  result := length(indexed);
  setlength(self.indexed, result+1);
//  self.i[high(indexed)] := TJSONDictionary.create;
  self.indexed[high(indexed)] := StrToJson(v_will_copy.ToJSON);

end;

procedure TJSONDictionary.AddMemberPrimitive(const Key: string; value: double);
var
  j: TJSONDictionary;
begin
  j := TJSONDictionary.create;
  try
    j.fromString(floatprecision(value, 12));
  except
    j.free;
    raise;
  end;
  self.named.Add(key, j);
end;

procedure TJSONDictionary.AddMemberPrimitiveVariant(const Key: string;
  v: variant);
var
  j: TJSONDictionary;
begin
  j := TJSONDictionary.create;
  j.FromString(VArtoJSONStorage(v));
  self.named.Add(key, j);
end;

procedure TJSONDictionary.AppendFieldsFrom(o: TJSONDictionary);
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

procedure TJSONDictionary.ClearAndFree;
var
//  a: TArray<TPair<string,TJSONDictionary>>;
  d: TJSONDictionary;
  p: TPair<string,TJSONDictionary>;
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

constructor TJSONDictionary.create;
begin
  inherited create;
  setlength(indexed,0);
  named := TStringObjectList<TJSONdictionary>.create;
  named.takeownership := true;
//  inc(cunt);
//  Debug.Log(inttostr(cunt));
end;

procedure TJSONDictionary.Deletei(index: ni);
var
  t: ni;
begin
  for t:= index to high(indexed)-1 do begin
    indexed[t] := indexed[t+1];
  end;
  SetLength(indexed, length(indexed)-1);

end;

destructor TJSONDictionary.destroy;
BEGIN
  ClearAndFree;
  named.free;
  named := nil;
//  dec(cunt);
//  Debug.Log(inttostr(cunt));

  inherited;
end;

function TJSONDictionary.FindSubKey(subkey, value: string): TJSONDictionary;
var
  ii: ni;
begin
  result := nil;
  ii := IndexOfSubValue(subkey, value);
  if ii >= 0 then
    exit(indexed[ii]);

end;

procedure TJSONDictionary.FromString(s: string);
var
  i: nativeint;
begin
  i := STRZ;
  ClearAndFree;
  FjsonInput := s;
  parse(s, i);
end;


function TJSONDictionary.GetAsString: string;
begin
  result := vartostrex(value);
end;

function TJSONDictionary.GetiCount: ni;
begin
  result := length(indexed);
end;

function TJSONDictionary.GetJSON: string;
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

function TJSONDictionary.GEtnCount: ni;
begin
  result := named.Count;
end;




function TJSONDictionary.GetnNamesByIndex(idx: ni): string;
begin
  result := named.Keys[idx];
end;

function TJSONDictionary.GetNode(sAddr: string): TJSONDictionary;
var
  s1,s2: string;
  i: nativeint;
  s: string;
begin
  try
    if sAddr = '' then
      exit(self);

    splitString(sAddr, '.', s1,s2);
    if s1 = '' then
      exit(self);

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

function TJSONDictionary.GetnValuesByIndex(idx: ni): TJSONDictionary;
begin
  result := named.ItemsByIndex[idx];
end;


function TJSONDictionary.HasNode(sAddr: string): boolean;
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

function TJSONDictionary.IndexOfSubValue(subkey, value: string): ni;
var
  t: ni;
begin
  for t:= 0 to icount-1 do begin
    if comparetext(indexed[t][subkey].AsString, value)=0 then
      exit(t);
  end;
  exit(-1);
end;

function TJSONDictionary.IsExpired: boolean;
begin
  result := (expires <> 0.0) and (now > expires);
end;

procedure TJSONDictionary.Iterate_CalculateField(sNewFieldName: string;
  procref: TIterateCalculateNewFieldProc);
var
  t: ni;
  newnode: TJSONDictionary;
begin
  for t:= 0 to Self.iCount-1 do begin
    if not self[t].HasNode(sNewFieldName) then begin
      newNode := self[t].AddMember(sNewFieldName);
    end else
      newNode := Self[t].GetNode(sNewFieldName);
    procref(self[t], newNode);
  end;

end;

procedure TJSONDictionary.Iterate_Filter(procref: TFilterProcRef);
var
  t: ni;
  newnode: TJSONDictionary;
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

function TJSONDictionary.LookUpBySubObjectField(fld: string;
  sSubValue: variant): TJSONDictionary;
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

procedure TJSONDictionary.MergeFieldsFrom(o: TJSONDictionary);
var
  t: ni;
begin
  for t:= 0 to o.nCount do begin
    self.AddMemberPrimitiveVariant(o.nNamesByIndex[t],o.valsByIndex(t).value);
  end;
end;

procedure TJSONDictionary.Parse(s: string; var iPosition: nativeint);
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
procedure TJSONDictionary.ParseArray(s: string; var iPosition: nativeint);
var
  sValue: string;
  jsn: TJSONDictionary;
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
      jsn := TJSONDictionary.create;
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

procedure TJSONDictionary.ParseConstant(s: string; var iPosition: nativeint);
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

procedure TJSONDictionary.ParseObject(s: string; var iPosition: nativeint);
type
  TParseState = (psName, psValue, psLimbo);
var
  bInQuotes: boolean;
  sName: string;
  bEscape : boolean;
  c: char;
  ps : TParseState;
  jsn: TJSONDictionary;
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
        jsn := TJSONDictionary.create;
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

procedure TJSONDictionary.SetAsString(const Val : string);
begin
  value := stringtotypedvariant(val);
end;

procedure TJSONDictionary.SetJSONInput(const Value: string);
begin
  Self.FromString(value);
end;

procedure TJSONDictionary.Sorti(sSubValue: string; bDesc: boolean = false);
var
  bDone: boolean;
  t: ni;
  procedure Swap(i1,i2: ni);
  var
    js: TJSONDictionary;
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

function TJSONDictionary.ToHolder: IHolder<TJSONdictionary>;
begin
  result := THOLDER<TJSON>.create;
  result.o := StrtoJSON(self.GetJSON);

end;

function TJSONDictionary.TryGetValue(sKey: string; out sValue: string): boolean;
begin
  if not self.HasNode(sKey) then
    exit(false);
  sValue := vartostrex(self[sKey].value);
  result := true;
end;

function TJSONDictionary.valsByIndex(ii: ni): TJSONDictionary;
begin
  if ii >= iCount then
    raise ECritical.create('Trying to chase index '+ii.tostring+' in JSON array of '+iCount.tostring+' elements.');
  result := indexed[ii];
end;

function TJSONDictionary.valsByString(s: string): TJSONDictionary;
begin
  result := named[s];
end;

function TJSONDictionary.ValsByVariant(v: variant): TJSONDictionary;
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

procedure TJSONDictionary.AddMemberJSON(const Key: string;
  value: string);
var
  j: TJSONDictionary;
begin
  j := TJSONDictionary.create;
  j.fromString(value);
  self.named.Add(key, j);

end;

procedure TJSONDictionary.AddMemberPrimitive(const Key: string;
  value: string);
var
  j: TJSONDictionary;
begin
  j := TJSONDictionary.create;
  j.value := value;
  self.named.Add(key, j);

end;


function JSONEsc(s: string): string;
begin
  result := s;
  result := stringreplace(result, '"', '\"', [rfReplaceAll]);
end;

function TJSONDictionary.AddIndexed: TJSONDictionary;
var
  len: ni;
begin
  len := length(indexed);
  setlength(self.indexed, len+1);
  result := TJSONDictionary.create;
  self.indexed[high(indexed)] := result;

end;

function TJSONDictionary.AddIndexed(stringPrimitive: string): ni;
begin

  AddIndexed().value := stringPrimitive;
  result:= icount-1;
end;

function TJSONDictionary.AddMember(const Key: string): TJSONDictionary;
begin
  result := TJSONDictionary.create;
  self.named.Add(key, result);
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
  fList.add(rec);

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

initialization
  cunt := 0;



end.
