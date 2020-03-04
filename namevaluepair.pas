unit namevaluepair;
// At some point, we just had to have a generic list of names and values.  So
// we made one.  This is used by the TXMLAttributes, among other classes.

interface

uses sysutils, classes, stringx, generics.collections.fixed, systemx, betterobject, typex, sharedobject;

type
  TNameValuePairList = class;//forward

  TNameValuePair = class(TBetterObject)
    // r: It holds a name.
    // r: It holds a value.
  private
    FName: string;
    FValue: string;
    owner: TNameValuePairList;
    function GetAsInteger: int64;
    procedure SetAsInteger(const Value: int64);
    function GetAsIntegerOrZero: integer;
    function GetAsReal: double;
    procedure SetAsReal(const Value: double);
    procedure SEtValue(const Value: string);
    function GetAsBoolean: boolean;
    procedure SetAsBoolean(const Value: boolean);
  public
    property Name: string read FName write FName;
    // A name
    property Value: string read FValue write SEtValue;
    property AsInteger: int64 read GetAsInteger write SetAsInteger;
    property AsFloat: double read GetAsReal write SetAsReal;
    property AsIntegerOrZero: integer read GetAsIntegerOrZero;
    property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;
    // A Value
  end;

  TNameValuePairList = class(TSharedObject)
  // r: Maintains a list of TNameValuePair instances
  private
    FList: TStringList;
    FAutoAdd: boolean;
    FFileName: string;
    FSaveOnFree: boolean;
    modified: boolean;
    FDelimiter: string;
    function GetItemByIndex(Index: integer): TNameValuePair;
    function GetItemCount: integer;
    function GetItem(sName: string): TNameValuePair;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    function Add(sName: string; sValue: string): integer;
    function IndexOf(sName: string): integer;
    function Remove(sName: string): boolean;

    property ItemsByIndex[Index: integer]: TNameValuePair read GetItemByIndex;
    function GetItemEx(sName: string; sDefaultVAlue: string): string; overload;
    function GetItemEx(sName: string; iDefaultVAlue: int64): int64;overload;
    function GetItemEx(sName: string; bDefaultVAlue: boolean): boolean;overload;
    function GetItemEx(sName: string; rDefaultVAlue: real): real; overload;
    function GetIntegerParameterArray(sPrefix: string): TArray<int64>;
    procedure SetIntegerParameterArray(sPrefix: string; a: TArray<int64>);

    // Looks up a NameValuePair by index.
    property Items[sName: string]: TNameValuePair read GetItem; default;
    // looks up a nameValuePail by Name.
    function Has(sName: string): boolean;
    property Count: integer read GetItemCount;
    // Returns the number of name value pairs defined.
    procedure Clear;
    property AutoAdd: boolean read FAutoAdd write FAutoAdd;
    property FileName: string read FFileName write FFilename;
    procedure LoadFromFile(sFile: string);
    procedure LoadFromStream(s: TStream; len: ni);
    procedure SaveToFile();overload;
    procedure SaveTofile(sFile: string);overload;
    function ToString: string; override;

    procedure loadFromString(sString: string; bAppend: boolean = false);
    property Changed: boolean read modified write modified;
    property SaveOnFree: boolean read FSaveOnFree write FSaveOnFree;
    property Delimiter: string read FDelimiter write FDelimiter;

  end;

function QuickNVPSearch(sFile: string; sfield, sValue: string): boolean;
function nvpstr(s: string): Iholder<TNameValuePairList>;


implementation

uses
  helpers_stream;

{ TNameValuePairList }
function nvpstr(s: string): Iholder<TNameValuePairList>;
begin
  result := THolder<TNameValuePairlist>.create;
  result.o := TNamevaluePairList.create;
  result.o.autoadd := true;
  result.o.LoadFromString(s);
end;

{ TNameValuePairList }

function QuickNVPSearch(sFile: string; sfield, sValue: string): boolean;
var
  nvpl: TNameValuePairList;
  s1, s2: string;
begin
  nvpl := TNameValuePairList.Create;
  try
    nvpl.LoadFromFile(sFile);
    s1 := nvpl.GetItemEx(sfield, '');
    s2 := lowercase(trim(sValue));
    s1 := copy(s1, 1, length(s2));
    s1 := lowercase(s1);
    result := s1 = s2;

  finally
    nvpl.Free;
  end;

end;

function TNameValuePairList.Add(sName, sValue: string): integer;
// Adds a name value pair to the list. Returns the Index added.
var
  nv: TNameValuePair;
begin
  nv := TNameValuePair.Create;
  nv.Name := sName;
  nv.Value := sValue;
  nv.owner := self;


  result := FList.AddObject(lowercase(trim(sNAme)), nv);
  modified := true;
end;

procedure TNameValuePairList.Clear;
VAR
  nvp: TNameValuePair;
begin
  while Count > 0 do begin
    nvp := TNameValuePair(FList.objects[0]);
    nvp.Free;
    nvp := nil;
    FList.delete(0);
    modified := true;
  end;
end;

constructor TNameValuePairList.Create;
// Standard.
begin
  inherited Create;
  Delimiter := '=';
  FList := TSTringList.Create;
  FList.sorted := true;
  FList.Duplicates := dupIgnore;

end;

destructor TNameValuePairList.Destroy;
// Standard.
begin
  if SAveOnFree then
    SaveToFile;
  Clear;
  FList.Clear;
  FList.Free;

  inherited;

end;

function TNameValuePairList.GetIntegerParameterArray(
  sPrefix: string): TArray<int64>;
var
  t: ni;
begin
  setlength(result, GetItemEx(sPrefix+'Count', 0));
  for t:= 0 to high(result) do begin
    result[t] := GetItemEx(sPrefix+inttostr(t), 0);
  end;

end;

function TNameValuePairList.GetItem(sName: string): TNameValuePair;
// Getter for the Items property.
var
  i: integer;
begin
  i := IndexOf(sName);

  if i > -1 then
    result := ItemsByIndex[i]
  else begin
    if AutoAdd then begin
      result := self.GetItemByIndex(Add(sName, ''));
    end
    else
      raise Exception.Create('Item not found in name-value pair: ' + sName);
  end;

end;

function TNameValuePairList.GetItemByIndex(Index: integer): TNameValuePair;
// Getter for the ItemsByIndex property
begin
  result := TNameValuePair(FList.objects[index]);

end;

function TNameValuePairList.GetItemCount: integer;
// Getter for the ItemCount
begin
  result := FList.Count;
end;


function TNameValuePairList.GetItemEx(sName: string;
  bDefaultVAlue: boolean): boolean;
begin
  result := strtobool(GetItemEx(sName, booltostr(bDefaultValue)));

end;


function TNameValuePairList.GetItemEx(sName, sDefaultVAlue: string)
  : string;
var
  i: integer;
begin
  i := IndexOf(sName);

  if i > -1 then
    result := ItemsByIndex[i].Value
  else
    result := sDefaultVAlue;

end;


function TNameValuePairList.Has(sName: string): boolean;
// Returns whether or not Name-value-pair with given name is in the list.
begin
  result := IndexOf(sName) > -1;

end;

function TNameValuePairList.IndexOf(sName: string): integer;
// Returns index of Name-value-pair with given name is in the list.
//var
//  t: integer;
begin
  result := FList.IndexOf(lowercase(trim(sName)));

{  result := -1;
  for t := 0 to Count - 1 do begin

    if lowercase(ItemsByIndex[t].Name) = lowercase(sName) then begin
      result := t;
      break;
    end;
  end;}

end;

procedure TNameValuePairList.LoadFromFile(sFile: string);
var
  sl: TStringlist;
  s1, s2: string;
  t: integer;
begin
  FFileName := sFile;
  Clear();
  Changed := false;
  sl := TStringlist.Create;
  try

    if fileexists(sFile) then
      sl.LoadFromFile(sFile);
    for t := 0 to sl.Count - 1 do begin
      if SplitString(sl[t], delimiter, s1, s2) then begin
        Add(s1, s2);
      end;
    end;
    modified := false;
  finally
    sl.Free;
  end;
end;

procedure TNameValuePairList.LoadFromStream(s: TStream; len: ni);
var
  ms: TMemoryStream;
  sl: TStringlist;
begin
  sl := TStringlist.create;
  try
    ms := TMemoryStream.create;
    try
      stream_GuaranteeCopy(s, ms, len);
      ms.Seek(0,soBeginning);
      sl.LoadFromStream(ms,TEncoding.UTF8);
      self.loadFromString(sl.text);
   finally
      ms.free;
    end;

  finally
    sl.free;
  end;

end;

procedure TNameValuePairList.loadFromString(sString: string; bAppend: boolean = false);
var
  sl: TStringlist;
  s1, s2: string;
  t: integer;
  bAuto: boolean;
begin
  bAuto := self.AutoAdd;
  if not bAppend then
    Clear()
  else
    AutoAdd := true;
  sl := TStringlist.Create;
  try
    sl.Text := sString;
    for t := 0 to sl.Count - 1 do begin
      if SplitString(sl[t], delimiter, s1, s2) then begin
        //if we're not appending
        if not bAppend then
          //add items (quicker than alternative)
          Add(s1, s2)
        //otherwise
        else begin
          //items that are appended could be duplicates of existing items, so we need to treat them differently
          self[s1].Value := s2;
        end;

      end;
    end;
    Changed := true;
  finally
    sl.Free;
    AutoAdd := bAuto;//restore previous autoadd state
  end;
end;

function TNameValuePairList.Remove(sName: string): boolean;
var
  nvp: TNameValuePair;
  t: integer;
begin
  sName := lowercase(trim(sName));
  result := false;
  for t := FList.Count - 1 downto 0 do begin
    nvp := TNameValuePair(FList.objects[t]);
    if lowercase(nvp.Name) = sName then begin
      FList.delete(t);
      result := true;
      modified := true;
      break;
    end;
  end;

end;

procedure TNameValuePairList.SaveToFile;
begin
  if changed then
    SaveToFile(FFileName);

end;

procedure TNameValuePairList.SaveTofile(sFile: string);
var
  t: integer;
  sl: TStringlist;
begin
  if not modified then exit;

  sl := TStringlist.Create;
  try
    for t := 0 to FList.Count - 1 do begin
      sl.Add(TNameValuePair(FList.objects[t]).Name + delimiter + TNameValuePair(FList.objects[t])
          .Value);
    end;

    sl.SaveTofile(sFile);
    FFileName := sFile;
    Changed := false;
  finally
    sl.Free;
  end;
end;

procedure TNameValuePairList.SetIntegerParameterArray(sPrefix: string;
  a: TArray<int64>);
var
  t: ni;
begin
  AutoAdd := true;
  Items[sPrefix+'Count'].AsInteger := length(a);
  for t := 0 to FList.count-1 do begin
    Items[sPrefix+inttostr(t)].AsInteger := a[t];
  end;

end;

function TNameValuePairList.ToString: string;
var
  t: integer;
  sl: TStringlist;
begin
  sl := TStringlist.Create;
  try
    for t := 0 to FList.Count - 1 do begin
      sl.Add(TNameValuePair(FList.objects[t]).Name + delimiter + TNameValuePair(FList.objects[t])
          .Value);
    end;

    result := sl.Text;

  finally
    sl.Free;
  end;
end;

function TNameValuePairList.GetItemEx(sName: string; rDefaultVAlue: real): real;
begin
  result := strtofloat(GetItemEx(sName, floattostr(rDefaultValue)));
end;

function TNameValuePairList.GetItemEx(sName: string;
  iDefaultVAlue: int64): int64;
var
  sValue: string;
begin
  sValue := inttostr(iDefaultVAlue);
  sValue := GetItemEx(sName, sValue);
  result := strtoint64(sValue);

end;

{ TNameValuePair }

function TNameValuePair.GetAsBoolean: boolean;
begin
  Result := strtobool(value);
end;

function TNameValuePair.GetAsInteger: int64;
begin
  result := strtoint64(FValue);

end;

function TNameValuePair.GetAsIntegerOrZero: integer;
begin
  if IsInteger(Value) then
    result := strtoint(Value)
  else
    result := 0;

end;

function TNameValuePair.GetAsReal: double;
begin
  result := strtofloat(Value);
end;

procedure TNameValuePair.SetAsBoolean(const Value: boolean);
begin
  self.Value := booltostr(value);
  if assigned(owner) then
     owner.modified := true;
end;

procedure TNameValuePair.SetAsInteger(const Value: int64);
begin
  self.Value := inttostr(Value);
  if assigned(owner) then
     owner.modified := true;
end;

procedure TNameValuePair.SetAsReal(const Value: double);
begin
  self.Value := floattostr(Value);
  if assigned(owner) then
     owner.modified := true;
end;

procedure TNameValuePair.SEtValue(const Value: string);
begin
  FValue := Value;
  if assigned(owner) then
     owner.modified := true;
end;

end.

