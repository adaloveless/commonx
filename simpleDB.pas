unit simpleDB;


interface

uses
  classes, betterobject, data.db, sqlexpr, typex, systemx, stringx, sysutils, variants, abstractdb, storageenginetypes;


const
  MYSQL_PARAMS = 'DriverUnit=Data.DBXMySQL'+NEWLINE+
                  'DriverPackageLoader=TDBXDynalinkDriverLoader,DbxCommonDriver180.bpl'+NEWLINE+
                  'DriverAssemblyLoader=Borland.Data.TDBXDynalinkDriverLoader,Borland.Data.DbxCommonDriver,Version=18.0.0.0,Culture=neutral,PublicKeyToken=91d62ebb5b0d1b1b'+NEWLINE+
                  'MetaDataPackageLoader=TDBXMySqlMetaDataCommandFactory,DbxMySQLDriver180.bpl'+NEWLINE+
                  'MetaDataAssemblyLoader=Borland.Data.TDBXMySqlMetaDataCommandFactory,Borland.Data.DbxMySQLDriver,Version=18.0.0.0,Culture=neutral,PublicKeyToken=91d62ebb5b0d1b1b'+NEWLINE+
                  'GetDriverFunc=getSQLDriverMYSQL'+NEWLINE+
                  'LibraryName=dbxmys.dll'+NEWLINE+
                  'LibraryNameOsx=libsqlmys.dylib'+NEWLINE+
                  'VendorLib=LIBMYSQL.dll'+NEWLINE+
                  'VendorLibWin64=libmysql.dll'+NEWLINE+
                  'VendorLibOsx=libmysqlclient.dylib'+NEWLINE+
                  'HostName=%hostname%'+NEWLINE+
                  'Database=%db%'+NEWLINE+
                  'User_Name=%username%'+NEWLINE+
                  'Password=%password%'+NEWLINE+
                  'MaxBlobSize=-1'+NEWLINE+
                  'LocaleCode=0000'+NEWLINE+
                  'Compressed=False'+NEWLINE+
                  'Encrypted=False'+NEWLINE+
                  'BlobSize=-1'+NEWLINE+
                  'ErrorResourceFile='+NEWLINE;

type
  TSimpledbCursor = class;//forward

  Tdb = class(TAbstractdb)
  private
    Fconn: TSQLConnection;
  public
    constructor CopyCreate(db: TDB);
    procedure Init;override;
    destructor Destroy;override;
    procedure Connect(sHost: string; sDatabase: string; sUser: string; sPAssword: string; sPort: string = '');override;
    function ReadQueryDBC(sQuery: string): TAbstractdbCursor;overload;override;
    function ReadQuery(sQuery: string): TSERowSet;override;

    procedure WriteQuery(sQuery: string);override;
    property conn: TSQLConnection read Fconn;
    function FunctionQuery(sQuery: string; sDefault: int64): int64;overload;override;
    function FunctionQuery(sQuery: string; sDefault: string): string;overload;override;

  end;

  TSimpledbCursor = class(TabstractDBCursor)
  private
    FDS: TCustomSQLDataSet;
  protected
    function GetField(sName: string): variant;override;
    function GetFieldCount: ni;override;
    function GetFieldByIndex(idx: ni): variant;override;
  public
    procedure First;override;
    procedure Next;override;
    function EOF: boolean;override;
    procedure BeforeDestruction;override;
    procedure Open(db: TAbstractDb; sQuery: string);override;
    procedure Close;override;
    property DS: TCustomSQLDataSet read FDS;
    function CountRecords: ni;override;
  end;




implementation

{ TdbCursor }

procedure TSimpledbCursor.BeforeDestruction;
begin
  inherited;
  if assigned(ds) then begin
    Fds.free;
    Fds := nil;
  end;
end;

procedure TSimpleDbCursor.Close;
begin

//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TSimpledbCursor.CountRecords: ni;
begin
  result := 0;
  if self.ds = nil then
    exit;
  Self.DS.First;
  while not ds.Eof do begin
    inc(result);
    ds.Next;
  end;
  ds.first;
end;

function TSimpledbCursor.EOF: boolean;
begin
  result := DS.Eof;
end;

procedure TSimpledbCursor.First;
begin
  inherited;
  DS.first;
end;

function TSimpledbCursor.GetField(sName: string): variant;
begin
  result := ds.FieldByName(sName).Value;
end;

function TSimpledbCursor.GetFieldByIndex(idx: ni): variant;
begin
  result := DS.Fields[idx].Value;
end;

function TSimpledbCursor.GetFieldCount: ni;
begin
  result := DS.FieldCount;
end;

procedure TSimpledbCursor.Next;
begin
  inherited;
  ds.Next;
end;

procedure TSimpledbCursor.Open(db: TAbstractDb; sQuery: string);
var
  ds: TDataSet;
begin
  TDB(db).conn.Execute(sQuery, nil, ds);
  self.FDS := TCustomSQLDataset(ds);
  self.FDS.BlockReadSize := 500;
end;

{ Tdb }

procedure Tdb.Connect(sHost, sDatabase, sUser, sPAssword, sPort: string);
begin
  inherited;
  Fconn := TSQLConnection.create(nil);
  FConn.DriverName := 'MYSQL';
  FConn.LoginPrompt := false;
  Fconn.params.text := MYSQL_PARAMS;
  Fconn.Params.Text := StringReplace(FConn.params.text, '%username%', sUser, [rfReplaceAll]);
  Fconn.Params.Text := StringReplace(FConn.params.text, '%db%', sDatabase, [rfReplaceAll]);
  Fconn.Params.Text := StringReplace(FConn.params.text, '%hostname%', sHost, [rfReplaceAll]);
  Fconn.Params.Text := StringReplace(FConn.params.text, '%password%', sPAssword, [rfReplaceAll]);
  Fconn.Params.Text := StringReplace(FConn.params.text, '%port%', sPort, [rfReplaceAll]);






end;

function Tdb.FunctionQuery(sQuery: string; sDefault: int64): int64;
var
  res: TSimpledbCursor;
begin
  res := TSimpleDbCursor.create;
  res.Open(self, sQuery);
  try
    res.DS.First;
    if res.CountRecords = 0 then
      result := sDefault
    else begin
      if res.DS.Fields[0].IsNull then
        result := sDefault
      else
        result := res.DS.Fields[0].Value;
    end;
  finally
    res.free;
  end;
end;

constructor Tdb.CopyCreate(db: TDB);
begin
  inherited CReate;
  Self.Connect(db.host, db.database, db.user, db.password, db.port);
end;

destructor Tdb.Destroy;
begin
  inherited;
end;

function Tdb.FunctionQuery(sQuery, sDefault: string): string;
var
  res: TSimpleDbCursor;
begin
  res := TSimpleDbCursor.create;
  res.Open(self, sQuery);
  try
    res.DS.First;
    if res.CountRecords = 0 then
      result := sDefault
    else begin
      if res.DS.Fields[0].IsNull then
        result := sDefault
      else
        result := vartostr(res.DS.Fields[0].Value);
    end;
  finally
    res.free;
  end;
end;

procedure Tdb.Init;
begin
  inherited;
end;

function Tdb.ReadQuery(sQuery: string): TSERowSet;
var
  t: ni;
  c: TSimpleDbCursor;
begin
  result := TSERowset.create;
  c := nil;
  try
    c := TSimpleDbCursor.create;
    c.Open(self, sQuery);
    result.CopyFromDAtaSet(c.DS, false);
  finally
    c.free;
  end;

end;

function Tdb.ReadQueryDBC(sQuery: string): TAbstractdbCursor;
begin
  result := TSimpleDbCursor.create;
  result.Open(self, sQuery);


end;

procedure Tdb.WriteQuery(sQuery: string);
begin

  FConn.Execute(sQuery, nil, nil);


end;

end.
