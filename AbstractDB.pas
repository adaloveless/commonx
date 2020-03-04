unit AbstractDB;
{x$DEFINE NO_DB}
interface

uses
  tickcount, consolelock, betterobject, classes, stringx, typex, systemx, sysutils, storageenginetypes, commandprocessor, commandicons, namevaluepair;

type
  TAbstractDBCursor = class;//forward

  TAbstractdb = class(TBetterObject)
  private
    function GetAge: ticker;
  protected
    FMWhost, FMWendPoint: string;
    function GetMWEndPoint: string;
    function GetMWHost: string;
    procedure SetMWEndpoint(Value: string);
    procedure SetMWHost(Value: string);
  public
{$IFNDEF CONTEXT_ONLY}
    database, DBHost, DBuser, DBpassword, DBPort: string;
{$ENDIF}
    Created: ticker;
    LastUsed: ticker;
    accumulator: TStringBuilder;
    procedure Init;override;
    destructor Destroy;override;
    procedure Connect;overload;virtual;
{$IFNDEF CONTEXT_ONLY}
    procedure Connect(sHost: string; sDatabase: string; sUser: string; sPAssword: string; sPort: string = '')overload;
{$ENDIF}
    function ReadQueryDBC(sQuery: string): TAbstractDBCursor;overload;virtual;
    function ReadQueryNVPL(sQuery: string): TNameValuePairList;
    function ReadQuery(sQuery: string): TSERowSet;overload;virtual;abstract;
    function ReadQueryH(sQuery: string): IHolder<TSERowSet>;overload;
    procedure WriteQuery(sQuery: string);virtual;
    procedure Writebehind(sQuery: string; bDontLog: boolean = false);virtual;
//    property conn: TSQLConnection read Fconn;
    function FunctionQuery(sQuery: string; sDefault: int64): int64;overload;virtual;abstract;
    function FunctionQuery(sQuery: string; rDefault: double): double;overload;virtual;abstract;
    function FunctionQuery(sQuery: string; sDefault: string): string;overload;virtual;abstract;
    function FunctionQuery_Cached(sQuery: string; iDefault: int64): int64;overload;
    function FunctionQuery_Cached(sQuery: string; rDefault: double): double;overload;
    function FunctionQuery_Cached(sQuery: string; sDefault: string): string;overload;
    procedure ClearQueryCache;
    function Connected: boolean;virtual;abstract;
    property MWHost: string read GetMWHost write SetMWHost;
    property MWEndPoint: string read GetMWEndPoint write SetMWEndpoint;
    property Age: ticker read GetAge;
    function GetNextID(sType: string): int64;virtual;abstract;
    function SetNextID(sType: string; id: int64): boolean;virtual;abstract;
  end;

  TAbstractFieldDef =   TSERowsetFieldDef;

  TAbstractDBCursor = class(TBetterObject)
  private

  protected
    function GetFieldByindex(idx: ni): variant;virtual;abstract;
    function GetField(sName: string): variant;virtual;abstract;
    function GetFieldCount: ni;virtual;abstract;
  public
    procedure BeforeDestruction;override;
    procedure Open(db: TAbstractDb; sQuery: string);virtual;
    procedure Close;virtual;
    procedure First;virtual;abstract;
    function EOF: boolean;virtual;abstract;
    procedure Next;virtual;abstract;
    function CountRecords: ni;virtual;ABSTRACT;
    property Fields[sName: string]: variant read GetField;default;
    property FieldsByIndex[idx: ni]: variant read GetFieldByindex;
    property FieldCount: ni read GetFieldCount;
    property RecordCount: ni read CountRecords;
  end;

  TSERowSetCursor = class (TAbstractDBCursor)
  protected
    function GetField(sName: string): variant;override;
    function GetFieldCount: ni;override;
    function GetFieldByIndex(idx: ni): variant;override;
  public
    RS: TSERowSet;
    RecordIndex: ni;
    destructor Destroy;override;
    procedure First;override;
    function EOF: boolean;override;
    procedure Next;override;
    function CountRecords: ni;override;

  end;



  Tcmd_RunQueries = class(TCommand)
  public
    sdb: TAbstractDB;
    queries: string;
    procedure Initexpense;override;
    procedure DoExecute;override;

  end;

var
  queryCache: TNameValuePairList;

implementation

{ TAbstractdb }

procedure TAbstractdb.ClearQueryCache;
begin
  lockconsole;
  try
    queryCache.Clear;
  finally
    UnlockConsole;
  end;
end;

{$IFNDEF CONTEXT_ONLY}
procedure TAbstractdb.Connect(sHost, sDatabase, sUser, sPAssword,
  sPort: string);
begin
  DBhost := shost;
  database := sDatabase;
  DBuser := sUser;
  DBpassword := sPassword;
  DbPort := sPort;
  if sPort = '' then
    DBPort := '3306';

  Connect;


end;
{$ENDIF}


procedure TAbstractdb.Connect;
begin
  //
end;

destructor TAbstractdb.Destroy;
begin
  accumulator.Free;
  inherited;
end;



function TAbstractdb.FunctionQuery_Cached(sQuery: string;
  iDefault: int64): int64;
var
  sKey, sValue: string;
  ix: ni;
begin
  lockconsole;
  try
    sKey := StringReplace(sQuery, #13,'', [rfReplaceAll]);
    sKey := StringReplace(sKey, #10,'', [rfReplaceAll]);
    sKey := StringReplace(sKey, '=',#2, [rfReplaceAll]);
    ix := queryCache.IndexOf(sKey);
    if ix > 0 then begin
      Result := strtoint64(queryCache.ItemsByIndex[ix].Value);
    end else begin
      result := FunctionQuery(sQuery, iDefault);
      queryCache.Add(sKey, inttostr(result));
      if random(100) = 50 then
        queryCache.SaveToFile;
    end;
  finally
    UnlockConsole;
  end;
end;

function TAbstractdb.FunctionQuery_Cached(sQuery: string;
  rDefault: double): double;
var
  sKey, sValue: string;
  ix: ni;
begin
  lockconsole;
  try
    sKey := StringReplace(sQuery, #13,'', [rfReplaceAll]);
    sKey := StringReplace(sKey, #10,'', [rfReplaceAll]);
    sKey := StringReplace(sKey, '=',#2, [rfReplaceAll]);
    ix := queryCache.IndexOf(sKey);
    if ix > 0 then begin
      Result := strtofloat(queryCache.ItemsByIndex[ix].Value);
    end else begin
      result := FunctionQuery(sQuery, rDefault);
      queryCache.Add(sKey, floattostr(result));
      if random(100) = 50 then
        queryCache.SaveToFile;
    end;
  finally
    UnlockConsole;
  end;
end;

function TAbstractdb.FunctionQuery_Cached(sQuery, sDefault: string): string;
var
  sKey, sValue: string;
  ix: ni;
begin
  lockconsole;
  try
    sKey := StringReplace(sQuery, #13,'', [rfReplaceAll]);
    sKey := StringReplace(sKey, #10,'', [rfReplaceAll]);
    sKey := StringReplace(sKey, '=',#2, [rfReplaceAll]);
    ix := queryCache.IndexOf(sKey);
    if ix > 0 then begin
      Result := queryCache.ItemsByIndex[ix].Value;
    end else begin
      result := FunctionQuery(sQuery, sDefault);
      queryCache.Add(sKey, result);
      if random(100) = 50 then
        queryCache.SaveToFile;
    end;
  finally
    UnlockConsole;
  end;
end;

function TAbstractdb.GetAge: ticker;
begin
  result := getTimeSince(Created);
end;

function TAbstractdb.GetMWEndPoint: string;
begin
  result := FMWEndPoint;
end;

function TAbstractdb.GetMWHost: string;
begin
  result := FMWhost;
end;


procedure TAbstractdb.Init;
begin
  inherited;
  accumulator := TStringBuilder.create;
  DBPort := '3306';
end;


function TAbstractdb.ReadQueryDBC(sQuery: string): TAbstractDBCursor;
begin
  //
  result := nil;
end;

function TAbstractdb.ReadQueryH(sQuery: string): IHolder<TSERowSet>;
begin
  result := THolder<TSERowSet>.create;
  result.o := ReadQuery(sQuery);
end;

function TAbstractdb.ReadQueryNVPL(sQuery: string): TNameValuePairList;
var
  dbc: TAbstractDBCursor;
begin
  result := nil;

  dbc := nil;
  try
    dbc := ReadQueryDBC(sQuery);


    result := TNameValuePairList.create;
    dbc.first;
    while not dbc.eof do begin
      result.Add(dbc.fieldsbyindex[0], dbc.fieldsbyindex[1]);
      dbc.next;
    end;

  finally
    dbc.free;
  end;



end;

procedure TAbstractdb.SetMWEndpoint(Value: string);
begin
  FMWendpoint := value;
end;

procedure TAbstractdb.SetMWHost(Value: string);
begin
  FMWhost := value;
end;


procedure TAbstractdb.Writebehind(sQuery: string; bDontLog: boolean = false);
begin

//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TAbstractdb.WriteQuery(sQuery: string);
begin
   //
end;

{ Tcmd_RunQueries }

procedure Tcmd_RunQueries.DoExecute;
var
  sLeft, sRight: string;
  slParsed: TStringlist;
  t: ni;
  u: ni;
  s: string;
  iStartAt: ni;
  sAccum: string;
begin

  slParsed := nil;
  try
    sRight := queries;
    StepCount := length(Queries);
{$IFDEF SL_PARSE}
    queries := '';//<-- do this to conserve memory
{$IFDEF SLOW_PARSE}
    while SplitString(sRight, '--execute--', sLeft, sRight) do begin
      Step := StepCount-length(sRight);
      {$IFNDEF NO_DB}
      sdb.WriteBehind(sLeft);
      {$ENDIF}
    end;
  {$IFNDEF NO_DB}
    sdb.WriteBehind(sLeft);
  {$ENDIF}
{$ELSE}
    slPArsed := parseString_quick(sright, '--execute--');
    stepcount := slParsed.count;
    for t:= 0 to ((slPArsed.count div 16)*16)-1 do begin
      IF (t mod 16) <> 0 then continue;
      step := t;
      s := '';
      for u := 0 to 15 do begin
        s := s + slparsed[t+u]+'--execute--';
      end;
      sdb.WriteBehind(s);
    end;
    for t:= ((slPArsed.count div 16)*16) to slPArsed.count-1 do begin
      step := t;
      s := '';
      s := s + slparsed[t]+'--execute--';
    end;
    sdb.WriteBehind(s);
{$ENDIF}
{$ELSE}
    iStartAt := 0;
    StepCount := length(queries);
    sAccum := '';
    while zExtractDelimitedString(queries, '--execute--', iStartAt, s) do begin
      step := iStartAt;
      sAccum := sAccum + s+ '--execute--';
      if length(sAccum) > 10000 then begin
        {$IFNDEF NO_DB}
        sdb.Writebehind(sAccum);
        {$ENDIF}
        sAccum := '';
      end;
    end;
    sAccum := sAccum + s;
    if sAccum <> '' then begin
      {$IFNDEF NO_DB}
      sdb.Writebehind(sAccum);
      {$ENDIF}
    end;

{$ENDIF}

  finally
    slPArsed.free;
  end;

end;


procedure Tcmd_RunQueries.Initexpense;
begin
  inherited;
  CPUExpense := 0.0;
  Icon := @CMD_ICON_DATABASE;
end;

{ TAbstractDBCursor }

procedure TAbstractDBCursor.BeforeDestruction;
begin
  inherited;
  //
end;

procedure TAbstractDBCursor.Close;
begin
  //
end;





procedure TAbstractDBCursor.Open(db: TAbstractDb; sQuery: string);
begin
  //
end;

{ TSERowSetCursor }

function TSERowSetCursor.CountRecords: ni;
begin
  result := RS.RowCount;
end;

destructor TSERowSetCursor.Destroy;
begin
  RS.Free;
  rs := nil;
  inherited;
end;

function TSERowSetCursor.EOF: boolean;
begin
  result := RecordIndex >= RS.RowCount;

end;

procedure TSERowSetCursor.First;
begin
  RecordIndex := 0;
end;

function TSERowSetCursor.GetField(sName: string): variant;
begin
  result := rs.FieldValues[recordindex, sName];
end;

function TSERowSetCursor.GetFieldByIndex(idx: ni): variant;
begin
  result := RS.Values[idx, RecordIndex];
end;

function TSERowSetCursor.GetFieldCount: ni;
begin
  result := RS.FieldCount;
end;

procedure TSERowSetCursor.Next;
begin
  inc(RecordIndex);
end;

initialization
  queryCache := TNameValuePairList.create;
  queryCache.LoadFromFile(GetTempPath+extractfilename(dllname)+'.querycache');


finalization
  querycache.savetofile(GetTempPath+extractfilename(dllname)+'.querycache');
  querycache.free;
  querycache := nil;


end.
