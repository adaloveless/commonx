unit rdtpdb;
{$DEFINE DO_WRITE_BEHIND}

interface

uses
  consolelock, abstractdb, sysutils, stringx, typex, systemx, tickcount, RDTPSQlConnection, storageenginetypes, variants, debug, namevaluepair, rdtpkeybotclient, betterobject;
type
  Trdtpdb = class(TAbstractDB)
  protected
    FUseTCP: boolean;
    FUseTor: boolean;
    FContext: string;
    function Getcontext: string;
    procedure Setcontext(Value: string);
  public

    cli: TRDTPSQLConnectionClient;
    kb: TKeybotClient;
    tmLAstReadyCheck: ticker;
    procedure Init;override;
    destructor Destroy;override;
    constructor CopyCreate(source: TRDTPDB);
    procedure Connect;override;
//    function ReadQuery(sQuery: string): TdbCursor;
    procedure WriteQuery(sQuery: string);override;
    procedure Writebehind(sQuery: string; bDontLog: boolean = false);override;
    procedure ReadQuery_Begin(sQuery: string);
    function ReadQuery_End: TSERowSet;
    function ReadQueryH_End: IHolder<TSERowSet>;

    function ReadQuery(sQuery: string): TSERowSet;override;
    function ReadQueryMultiTable(sQueryDotDotDot: string; postOrder: string): IHolder<TSERowSet>;
    procedure WriteQueryMultiTable(sQueryDotDotDot: string);

    function ReadQueryDBC(sQuery: string): TAbstractDBCursor;override;
//    property conn: TSQLConnection read Fconn;
    function ArrayQuery(sQuery: string): TArray<string>;
    procedure FunctionQueryInt_Begin(sQuery: string);overload;
    procedure FunctionQueryDouble_Begin(sQuery: string);overload;
    procedure FunctionQueryString_Begin(sQuery: string);overload;
    function FunctionQuery_End(iDefault: int64): int64;overload;
    function FunctionQuery_End(rDefault: double): double;overload;
    function  FunctionQuery_End(sDefault: string): string;overload;

    function FunctionQuery(sQuery: string; iDefault: int64): int64;overload;override;
    function FunctionQuery(sQuery: string; rDefault: double): double;overload;override;
    function FunctionQuery(sQuery: string; sDefault: string): string;overload;override;
    procedure CleanupClient;
    property UseTCP: boolean read FUseTCP write FuseTCP;
    property UseTor: boolean read FUseTor write FuseTor;
    function Connected: boolean;override;
    property Context: string read Getcontext write Setcontext;
    procedure CheckConnectedOrConnect;
  end;






implementation

{ Trdtpdb }

function Trdtpdb.ArrayQuery(sQuery: string): TArray<string>;
var
  t: ni;
  rs: TSERowSet;
begin
  rs := TSERowSet.Create;
  try
    rs := ReadQuery(sQuery);
    setlength(result, rs.RowCount);
    for t:= 0 to rs.rowcount-1 do begin
      result[t] := rs.values[0,t];
    end;
  finally
    rs.free;
  end;


end;

procedure Trdtpdb.CheckConnectedOrConnect;
begin
  if Connected then
    exit;
  connect;

end;

procedure Trdtpdb.CleanupClient;
begin
  if assigned(cli) then
    cli.free;
  cli := nil;

  if assigned(kb) then
    kb.free;
  kb := nil;
end;

procedure Trdtpdb.Connect;
var
  sl,sr: string;
begin
  if connected then exit;

  cleanupclient;
  cli := TRDTPSQLConnectionClient.create(MWHost, MWEndpoint);
  cli.Host := self.MWHost;
  cli.endPoint := self.MWEndpoint;
  cli.UseTCP := self.UseTCP;
  cli.UseTor := self.UseTor;

  kb := TKeybotClient.create(MWHost, MWEndpoint);
  kb.UseTCP := self.UseTCP;
  kb.UseTor := self.UseTor;

  if context <> '' then begin
    splitstringnocase(context, ';db=', sl,sr);
    splitstringnocase(sr, ';', sl,sr);
    kb.Context := sl;
    cli.context := context;
  end else begin
{$IFNDEF CONTEXT_ONLY}
    kb.Context := database;
    cli.Context :='simple;db='+Database+';host='+DBHost+';user='+DBUSER+';pass='+DBPassword+';port='+DBPort+';';
{$ENDIF}
  end;

  cli.timeout := 3000000;
  kb.Timeout := 300000;




end;

function Trdtpdb.Connected: boolean;
begin
  if self.cli = nil then
    exit(false);

  result := self.cli.Connected;
end;

constructor Trdtpdb.CopyCreate(source: TRDTPDB);
begin
  inherited Create;
  FMWHost := source.MWHost;
  FMWEndpoint := source.MWEndpoint;
  if source.Context <> '' then begin
    context := source.context;
    Connect;
  end else begin
    {$IFNDEF CONTEXT_ONLY}
    Connect(source.DBhost, source.database, source.DBuser, source.DBpassword, source.DBport);
    {$ENDIF}
  end;
end;

destructor Trdtpdb.Destroy;
begin
  cleanupclient;
  inherited;
end;

function Trdtpdb.FunctionQuery(sQuery: string; iDefault: int64): int64;
begin
  FunctionQueryDouble_Begin(sQuery);
  result := FunctionQuery_End(iDefault);

end;



function Trdtpdb.FunctionQuery(sQuery, sDefault: string): string;
begin
  FunctionQueryDouble_Begin(sQuery);
  result := FunctionQuery_End(sDefault);
end;

procedure Trdtpdb.FunctionQueryDouble_Begin(sQuery: string);
begin
  ReadQuery_Begin(sQuery);



end;

procedure Trdtpdb.FunctionQueryInt_Begin(sQuery: string);
begin
  ReadQuery_Begin(sQuery);
end;

procedure Trdtpdb.FunctionQueryString_Begin(sQuery: string);
begin
  ReadQuery_Begin(sQuery);
end;

function Trdtpdb.FunctionQuery_End(iDefault: int64): int64;
var
  rs: TSERowset;
begin
  rs := nil;
  try
  rs := Readquery_End;
  if rs = nil then begin
    result := iDefault;
    exit;
  end;

  if rs.RowCount = 0 then begin
    result := iDefault;
    exit;
  end;

  if rs.fieldcount = 0 then begin
    result := iDefault;
    exit;
  end;

  if vartype(rs.values[0,0]) = varNull then
    result := iDefault
  else
    result := rs.Values[0,0];
  finally
    rs.free;
    rs := nil;
  end;


end;

function Trdtpdb.FunctionQuery_End(rDefault: double): double;
var
  rs: TSERowset;
begin
  rs := nil;
  try
  rs := Readquery_End;
  if rs = nil then begin
    result := rDefault;
    exit;
  end;

  if rs.RowCount = 0 then begin
    result := rDefault;
    exit;
  end;

  if rs.fieldcount = 0 then begin
    result := rDefault;
    exit;
  end;

  if vartype(rs.values[0,0]) = varNull then
    result := rDefault
  else
    result := rs.Values[0,0];

  finally
    rs.free;
    rs := nil;
  end;


end;


function Trdtpdb.FunctionQuery_End(sDefault: string): string;
var
  rs: TSERowset;
begin
  rs := Readquery_End;
  try
    if rs = nil then begin
      result := sDefault;
      exit;
    end;

    if rs.RowCount = 0 then begin
      result := sDefault;
      exit;
    end;

    if rs.fieldcount = 0 then begin
      result := sDefault;
      exit;
    end;

    if vartype(rs.values[0,0]) = varNull then
      result := sDefault
    else
      result := rs.Values[0,0];
  finally
    rs.free;
  end;

end;

function Trdtpdb.Getcontext: string;
begin
  result := FContext;
end;

function Trdtpdb.FunctionQuery(sQuery: string; rDefault: double): double;
begin
  FunctionQueryDouble_Begin(sQuery);
  result := FunctionQuery_End(rDefault);
end;

procedure Trdtpdb.Init;
begin
  inherited;
  FMWHost := 'localhost';
  FMWEndpoint := '235';
end;


function Trdtpdb.ReadQuery(sQuery: string): TSERowSet;
var
  tm: ticker;
begin
  result := nil;
  tm := GetTicker;
  Debug.Log('Read Query: '+sQuery);
  result := cli.ReadQuery(sQuery);
//  Debug.Log('Query Took: '+commaize(gettimesince(tm))+'ms.');
end;

function Trdtpdb.ReadQueryDBC(sQuery: string): TAbstractDBCursor;
begin
  result := TSERowSetCursor.create;
  TSERowSetCursor(result).RS := ReadQuery(sQuery);

end;

function Trdtpdb.ReadQueryH_End: IHolder<TSERowSEt>;
begin
  result := THolder<TSERowSet>.create;
  result.o := ReadQuery_end;
end;

function Trdtpdb.ReadQueryMultiTable(sQueryDotDotDot: string; postOrder: string): IHolder<TSERowset>;
var
  rsTables: TSERowsEt;
  t: ni;
  s, sl, sTable, sTablePrefix, sTableSuffix, sr, sJunk, sQuery: string;
  unsorted: TSERowSet;
  e: IHolder<TSERowSet>;
begin


  s := sQueryDotDotDot;
  if not SplitString(s, '...', sl, sr) then
    raise ECritical.create('no ... found');
  if not SplitString(sl, ' ', sl, sTablePRefix, true) then
    raise ECritical.create('could not find table prefix in '+sQueryDotDotDot);

  if (not SplitString(sr, ' ', sTableSuffix, sr)) and (sr <> '') then
    raise ECritical.create('could not find table suffix in '+sQueryDotDotDot);


  rsTables := ReadQuery('show tables');
  try
    for t:= 0 to rsTables.RowCount -1 do begin
      sTable := rstables.Values[0, t];
      if comparetext(Zcopy(sTable, 0, length(sTablePrefix)), sTablePrefix)=0 then begin
        if comparetext(Zcopy(sTable, length(sTable)-length(sTableSuffix), length(sTableSuffix)), sTableSuffix)=0 then begin
          sQuery := sl +' '+ sTable +' '+ sr;
          ReadQuery_Begin(sQuery);
        end;
      end;
    end;

    result := THolder<TSERowset>.create;
    result.o := TSERowSet.create;
    unsorted := TSERowset.create;
    try
      for t:= 0 to rsTables.RowCount -1 do begin
        sTable := rstables.Values[0, t];
        if comparetext(Zcopy(sTable, 0, length(sTablePrefix)), sTablePrefix)=0 then begin
          if comparetext(Zcopy(sTable, length(sTable)-length(sTableSuffix), length(sTableSuffix)), sTableSuffix)=0 then begin
            sQuery := sl +' '+ sTable +' '+ sr;
            e := ReadQueryH_End;
            unsorted.Append(e);//<<--- MERGES STUFF
          end;
        end;
      end;

      unsorted.BuildIndex('idxRes', postOrder);//<---- THIS ALSO SETS THE INDEX
      unsorted.CopyFieldDefsTo(result.o);
      for t:= 0 to unsorted.rowcount-1 do begin
        unsorted.cursor := t;
        result.o.AppendRowFrom(unsorted, t);
//        Debug.Log('Multi-Sort #'+inttostr(t)+': '+unsorted.RowToString);
      end;

    finally
      unsorted.free;
    end;
  finally
    rsTables.free;
  end;

end;


procedure Trdtpdb.ReadQuery_Begin(sQuery: string);
begin
  Debug.Log('BEGIN Read Query: '+sQuery);
  cli.ReadQuery_Async(sQuery);
end;

function Trdtpdb.ReadQuery_End: TSERowSet;
var
  tm: ticker;
begin
  tm := GetTicker;
  result := cli.ReadQuery_Response;
//  Debug.Log('END Read Query took '+commaize(gettimesince(tm))+'ms.');
end;

procedure Trdtpdb.Setcontext(Value: string);
begin
  Fcontext := value;
end;

procedure Trdtpdb.Writebehind(sQuery: string; bDontLog: boolean = false);
begin
  inherited;
  if not bDontLog then
    Debug.Log('WriteBehind: '+sQuery);
{$IFDEF DO_WRITE_BEHIND}
  if gettimesince(tmLastReadyCheck) > 30000 then begin
    cli.ReadyToWriteBehind;
    tmLastReadyCheck := getticker;
  end;
  cli.WriteBehind(sQuery);
{$ELSE}
  cli.WriteQuery(sQuery);
{$ENDIF}

end;

procedure Trdtpdb.WriteQuery(sQuery: string);
begin
  Debug.Log('Write: '+sQuery);
  cli.WriteQuery(sQuery);
end;


procedure Trdtpdb.WriteQueryMultiTable(sQueryDotDotDot: string);
var
  rsTables: TSERowsEt;
  t: ni;
  s, sl, sTable, sTablePrefix, sTableSuffix, sr, sJunk, sQuery: string;
  unsorted: TSERowSet;
  e: IHolder<TSERowSet>;
begin


  s := sQueryDotDotDot;
  if not SplitString(s, '...', sl, sr) then
    raise ECritical.create('no ... found');
  if not SplitString(sl, ' ', sl, sTablePRefix, true) then
    raise ECritical.create('could not find table prefix in '+sQueryDotDotDot);

  if not SplitString(sr, ' ', sTableSuffix, sr) then
    raise ECritical.create('could not find table suffix in '+sQueryDotDotDot);


  rsTables := ReadQuery('show tables');
  try
    for t:= 0 to rsTables.RowCount -1 do begin
      sTable := rstables.Values[0, t];
      if comparetext(Zcopy(sTable, 0, length(sTablePrefix)), sTablePrefix)=0 then begin
        if comparetext(Zcopy(sTable, length(sTable)-length(sTableSuffix), length(sTableSuffix)), sTableSuffix)=0 then begin
          sQuery := sl +' '+ sTable +' '+ sr;
          Writebehind(sQuery);
        end;
      end;
    end;
  finally
    rsTables.free;
  end;
end;

initialization


finalization


end.
