unit MYSQLRDTPDataModule;
{$IFNDEF VER160}{$INLINE AUTO}{$ENDIF}
{$DEFINE STD_DRV}
{x$DEFINE ALT_DRV}
{x$DEFINE SAVE_MEMORY}
interface
//TODO 1:Should not export TCustomSQLDataset

uses
  DBXDynalink, DBXMySQL,SqlExpr, SysUtils, Classes, DB, better_Sockets, typex,inifiles, replaylog, exceptions, DBXCommon,
  sharedobject, abstractrdtpdatamodule, storageenginetypes, managedthread, rdtpprocessor, beeper, inifile, systemx, namevaluepair, consolelock, betterobject;

const
//  DLL='dbexpmysql.2006.dll';
  DRIVERNAME='MySQL';//<---WORKING 32-bit
{$IFDEF STD_DRV}
  DLL='dbxmys.dll';
  LIB='libmysql.dll';
  FUNC='getSQLDriverMYSQL';
{$ELSE}
{$IFNDEF ALT_DRV}
  DLL='dbxopenmysql50.dll';//<---WORKING 32-bit
  LIB='libmysql.50.dll';//<---WORKING 32-bit
  FUNC='getSQLDriverMYSQL';//<---WORKING 32-bit
{$ENDIF}
//OLAP -- <--- DELPHI 2006 GOOD
//*****************************
{$IFDEF ALT_DRV}
  DLL='dbx4mysql.dll';
  LIB='libmysql.50.dll';
  FUNC='getSQLDriverDBXL4Mysql';
{$ENDIF}
{$ENDIF}


//*****************************

//  LIB='libmysql.2006.dll';
//  LIB='libmysql.native.dll';

type
  TMYSQLRDTPDataModule = class;





  TMYSQLRDTPDataModule = class(TAbstractRDTPDataModule)
  private

    { Private declarations }
    td: TTransactionDesc;
    FWriteQueries: integer;
    FContextVerified: boolean;
    FHost: string;
    FID: string;

    procedure DataModuleCreate(Sender: TObject);
    procedure Execute(sQuery:string; connection: TSQLConnection; out ds: TCustomSQLDataset);
    function ExecuteDirect(sQuery: string; connection: TSQLConnection): integer;
    procedure SetContext(const Value: string);




  public
    writes: TSQLConnection;
    sessiondb: TSQLConnection;
    reads: TSQLConnection;
    keybot_link: TBetterTcpClient;

    constructor create;override;
    destructor destroy;override;



    { Public declarations }


    function TryGetNextID(iKey: integer; out res: int64): boolean;
    function TrySetNextID(iKey: integer; value: int64): boolean;
    procedure ReadConfigOld;
    procedure ConfigureFromContext;override;
    procedure ConfigureFromContext_SE;
    procedure ConfigureFromContext_Simple;
    procedure IncWrite;
    procedure ChangeSQLConnectionParam(conn: TSQLConnection; sParamName: string; sVAlue: string);



    //RETURNS CUSTOMSQLDATASET
    function ExecuteSystem_Platform(sQuery: string; out dataset: TCustomSQLDataset ): integer;
    function ExecuteWrite_Platform(sQuery: string; out dataset: TCustomSQLDataset ): integer;
    procedure ExecuteRead_Platform(sQuery: string; out dataset: TCustomSQLDataset );

    //returns TSEROWSET
    function ExecuteSystem(sQuery: string; out dataset: TSERowSet): integer;override;
    function ExecuteWrite(sQuery: string; out dataset: TSERowSet): integer;override;
    procedure ExecuteRead(sQuery: string; out dataset: TSERowSet);override;
    function ExecuteSystem(sQuery: string): integer;override;
    function ExecuteWriteRaw(sQuery: string): integer;overload;

    procedure ConnectRead;override;
    procedure ConnectWrite;override;
    procedure ConnectSystem;override;

    function ContextVerified: boolean;inline;
    procedure VerifyContext;
    function TableExists(sTable: string): boolean;
    function CopyTable(sSource, sTarget: string): IHolder<TStringList>;
    function GetNextID(sKey: string): Int64; override;
    function SetNextID(sKey: string; iValue: Int64): Int64; override;

  end;



implementation

uses AppLock, debug,  stringx;

{ TMYSQLRDTPDataModule }



procedure TMYSQLRDTPDataModule.ChangeSQLConnectionParam(conn: TSQLConnection;
  sParamName, sVAlue: string);
begin
  RemovePrefixFromStringList(sPAramName,conn.Params);
  conn.params.Add(sParamName+'='+sValue);
end;

procedure TMYSQLRDTPDataModule.ConfigureFromContext_SE;
var
  sFile: string;
  ini: TMothershipINIfile;
  sSessionHost: string;
  sSessionUserName: string;
  sSessionPassword: string;
  sMainHost: string;
  sMainUserName: string;
  sMainPassword: string;
  sKeybotHost: string;
  sMainDatabase: string;
  sSessionDatabase: string;
  i: integer;
begin
  AL.lock;
  try
    ini := TMothershipInifile.create();
    ini.FileContents := GetConfigFile;
    try
      sSessionHost := ini.ReadString('engine','host', 'localhost');
      FID := ini.ReadString('engine','ID', '0');
      sSessionUserName := ini.ReadString('engine','user', 'root');
      sSessionPassword := ini.ReadString('engine','pass', '');
      sSessionDatabase := ini.ReadString('engine', 'db', 'undefined');

      sMainHost := ini.ReadString('engine','host', 'localhost');
      sMainUserName := ini.ReadString('engine','user', 'root');
      sMainPassword := ini.ReadString('engine','pass', '');
      sMainDatabase := ini.ReadString('engine', 'db', 'undefined');

      Fhost := sMainHost;

      writes.Connected := false;
      reads.Connected := false;
      sessiondb.Connected := false;

      sKeyBotHost := ini.ReadString('keybot','host', 'localhost');
      keybot_link.RemoteHost := sKeyBotHost;
      keybot_link.RemotePort := '400';

//---------------------------1
//      writes.connectionName := 'writes'

     writes.drivername := DRIVERNAME;
      writes.connectionname := 'MYSQLConnection';
      writes.libraryname := DLL;
      writes.vendorlib := LIB;
      writes.GetDriverFunc := FUNC ;
//      i := writes.Params.IndexOfName('HostName');
//      writes.Params[i] := 'HostName='+sMainHost;
      writes.Params.add('HostName='+sMainHost);
//      i := writes.Params.IndexOfName('User_Name');
//      writes.Params[i] := 'User_Name='+sMainUserName;
      writes.Params.add('User_Name='+sMainUserName);
//      i := writes.Params.IndexOfName('Password');
//      writes.Params[i] := 'Password='+sMainPassword;
      writes.Params.add('Password='+sMainPassword);
      writes.params.add('Database='+sMainDatabase);



//----------------------------------2
//      reads.connectionName := 'reads';
      reads.drivername := DRIVERNAME;
      reads.connectionname := 'MYSQLConnection';
      reads.libraryname := DLL;
      reads.vendorlib := LIB;
      reads.GetDriverFunc := FUNC;
//      i := reads.Params.IndexOfName('HostName');
//      reads.Params[i] := 'HostName='+sMainHost;
      reads.Params.add('HostName='+sMainHost);
//      i := writes.Params.IndexOfName('User_Name');
//      reads.Params[i] := 'User_Name='+sMainUserName;
      reads.Params.add('User_Name='+sMainUserName);
//      i := writes.Params.IndexOfName('Password');
//      reads.Params[i] := 'Password='+sMainPassword;
      reads.Params.add('Password='+sMainPassword);
      reads.params.add('Database='+sMAinDatabase);

//---------------------------------3
//      sessiondb.connectionName := 'sessiondb';
      sessiondb.drivername := DRIVERNAME;
      sessiondb.connectionname := 'MYSQLConnection';
      sessiondb.libraryname := DLL;
      sessiondb.vendorlib := LIB;
      sessiondb.GetDriverFunc := FUNC;
//      i := sessiondb.Params.IndexOfName('HostName');
//      sessiondb.Params[i] := 'HostName='+sSessionHost;
      sessiondb.Params.add('HostName='+sSessionHost);
//      i := writes.Params.IndexOfName('User_Name');
//      sessiondb.Params[i] := 'User_Name='+sSessionUserName;
      sessiondb.Params.add('User_Name='+sSessionUserName);
//      i := writes.Params.IndexOfName('Password');
//      sessiondb.Params[i] := 'Password='+sSessionPassword;
      sessiondb.Params.add('Password='+sSessionPassword);
      sessiondb.params.add('Database='+sSessionDatabase);

    finally
      ini.free;
    end;
  finally
    al.unlock;
  end;

end;

procedure TMYSQLRDTPDataModule.ConfigureFromContext;
begin
  inherited;
  if lowercase(zcopy(context, 0, length('simple;'))) = 'simple;' then begin
    ConfigureFromContext_Simple;
  end else begin
    ConfigureFromContext_SE;
  end;



end;

procedure TMYSQLRDTPDataModule.ConfigureFromContext_Simple;
var
  sFile: string;
  sSessionHost: string;
  sSessionUserName: string;
  sSessionPassword: string;
  sMainHost: string;
  sMainUserName: string;
  sMainPassword: string;
  sKeybotHost: string;
  sMainDatabase: string;
  sSessionDatabase: string;
  sMainPort: string;
  sSessionPort: string;
  i: integer;
  nvpl: TNameValuepairlist;
begin
  nvpl := TNameValuePairList.Create;
  try
    nvpl.loadFromString(stringreplace(context,';',CRLF, [rfreplaceall]));

      sSessionHost := nvpl.GetItemEx('host', 'localhost');
      FID := nvpl.GetItemEx('ID', '0');
      sSessionUserName := nvpl.GetItemEx('user', 'root');
      sSessionPassword := nvpl.GetItemEx('pass', '');
      sSessionDatabase := nvpl.GetItemEx('db', 'undefined');
      sSessionPort := nvpl.GetItemEx('port', '3306');

      sMainHost := nvpl.GetItemEx('host', 'localhost');
      sMainUserName := nvpl.GetItemEx('user', 'root');
      sMainPassword := nvpl.GetItemEx('pass', '');
      sMainDatabase := nvpl.GetItemEx('db', 'undefined');
      sMainPort := nvpl.GetItemEx('port', '3306');

      Fhost := sMainHost;

      writes.Connected := false;
      reads.Connected := false;
      sessiondb.Connected := false;

      sKeyBotHost := nvpl.GetItemEx('keybot_host', 'localhost');
      keybot_link.RemoteHost := sKeyBotHost;
      keybot_link.RemotePort := '400';

//---------------------------1
//      writes.connectionName := 'writes'

     writes.drivername := DRIVERNAME;
      writes.connectionname := 'MYSQLConnection';
      writes.libraryname := DLL;
      writes.vendorlib := LIB;
      writes.GetDriverFunc := FUNC ;
//      i := writes.Params.IndexOfName('HostName');
//      writes.Params[i] := 'HostName='+sMainHost;
      ChangeSQLConnectionParam(writes,'HostName',sMainHost);
      CHangeSQLConnectionParam(writes,'HostName',sMainHost);
//      writes.Params.add('HostName='+sMainHost);
      CHangeSQLConnectionParam(writes,'User_Name',sMainUserName);
//      writes.Params.add('User_Name='+sMainUserName);
      CHangeSQLConnectionParam(writes,'Password',sMainPassword);
//      writes.Params.add('Password='+sMainPassword);
      CHangeSQLConnectionParam(writes,'Database',sMainDatabase);
      CHangeSQLConnectionParam(writes,'Port',sMainPort);
//      writes.params.add('Database='+sMainDatabase);



//----------------------------------2
//      reads.connectionName := 'reads';
      reads.drivername := DRIVERNAME;
      reads.connectionname := 'MYSQLConnection';
      reads.libraryname := DLL;
      reads.vendorlib := LIB;
      reads.GetDriverFunc := FUNC;
      CHangeSQLConnectionParam(reads,'HostName',sMainHost);
//      reads.Params.add('HostName='+sMainHost);
      CHangeSQLConnectionParam(reads,'User_Name',sMainUserName);
//      reads.Params.add('User_Name='+sMainUserName);
      CHangeSQLConnectionParam(reads,'Password',sMainPassword);
//      reads.Params.add('Password='+sMainPassword);
      CHangeSQLConnectionParam(reads,'Database',sMAinDatabase);
      CHangeSQLConnectionParam(reads,'Port',sMainPort);
//      reads.params.add('Database='+sMAinDatabase);

//---------------------------------3
//      sessiondb.connectionName := 'sessiondb';
      sessiondb.drivername := DRIVERNAME;
      sessiondb.connectionname := 'MYSQLConnection';
      sessiondb.libraryname := DLL;
      sessiondb.vendorlib := LIB;
      sessiondb.GetDriverFunc := FUNC;
      CHangeSQLConnectionParam(sessiondb,'HostName',sSessionHost);
//      sessiondb.Params.add('HostName='+sSessionHost);
      CHangeSQLConnectionParam(sessiondb,'User_Name',sSessionUserName);
//      sessiondb.Params.add('User_Name='+sSessionUserName);
      CHangeSQLConnectionParam(sessiondb,'Password',sSessionPassword);
//      sessiondb.Params.add('Password='+sSessionPassword);
      CHangeSQLConnectionParam(sessiondb,'Database',sSessionDatabase);
      CHangeSQLConnectionParam(sessiondb,'Port',sSessionPort);
//      sessiondb.params.add('Database='+sSessionDatabase);
  finally
    nvpl.free;
  end;
end;

procedure TMYSQLRDTPDataModule.ConnectRead;
var
  bRetry: boolean;
begin
  repeat
    bRetry := false;
    try
      if not reads.connected then begin
        Debug.Log(self,'opening read connection '+reads.libraryname);
        reads.connected := true;
        Debug.Log(self,'read connection opened '+reads.libraryname);
      end;
    except
      on E: Exception do begin
        Debug.Log(self,'EXCEPTION on connection: '+e.message );
        if pos('failed to connect', lowercase(e.message))> 0 then begin
          Debug.Log(self,'auto-retry on EXCEPTION');
          reads.connected := false;

          bRetry := true;
          sleep(random(700));
        end
        else begin
          beeper.beep(700,300);
          Debug.Log(self,reads.Params.Text);
          Debug.Log(self,reads.DriverName);
          Debug.Log(self,reads.LIbraryName);
          Debug.Log(self,reads.VendorLIb);
          Debug.Log(self,'auto-retry on EXCEPTION');
          raise;

        end;
      end;
    end;
  until bRetry = false;


//  writes.connected := true;
//  sessiondb.connected := true;
end;







procedure TMYSQLRDTPDataModule.SetContext(const Value: string);
begin
  FContext := value;
  ConfigureFromContext;

end;




function TMYSQLRDTPDataModule.SetNextID(sKey: string; iValue: Int64): Int64;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TMYSQLRDTPDataModule.ReadConfigOld;
var
  sFile: string;
  ini: Tinifile;
  sSessionHost: string;
  sSessionUserName: string;
  sSessionPassword: string;
  sMainHost: string;
  sMainUserName: string;
  sMainPassword: string;
  sKeybotHost: string;
  sMainDatabase: string;
  sSessionDatabase: string;
  i: integer;
begin
  AL.lock;
  try
    sFile := changefileext(DLLNAme,'.ini');
    if ParamStr(1)<> '' then begin
      if copy(ParamStr(1),1,1) <> '-' then
        sFile := ParamStr(1);
    end;
    ini := TInifile.create(sfile);
    try
      sSessionHost := ini.ReadString('sessiondb','host', 'localhost');
      sSessionUserName := ini.ReadString('sessiondb','user', 'root');
      sSessionPassword := ini.ReadString('sessiondb','pass', '');
      sSessionDatabase := ini.ReadString('sessiondb', 'db', 'AGAME');

      sMainHost := ini.ReadString('maindb','host', 'localhost');
      sMainUserName := ini.ReadString('maindb','user', 'root');
      sMainPassword := ini.ReadString('maindb','pass', '');
      sMainDatabase := ini.ReadString('sessiondb', 'db', 'AGAME');

      writes.Connected := false;
      reads.Connected := false;
      sessiondb.Connected := false;

      sKeyBotHost := ini.ReadString('keybot','host', 'localhost');
      keybot_link.RemoteHost := sKeyBotHost;
      keybot_link.RemotePort := '400';

//---------------------------1
//      writes.connectionName := 'writes';
//      writes.drivername := DRIVERNAME;
      writes.libraryname := DLL;
      writes.vendorlib := LIB;
      writes.GetDriverFunc := FUNC ;
//      i := writes.Params.IndexOfName('HostName');
//      writes.Params[i] := 'HostName='+sMainHost;
      writes.Params.add('HostName='+sMainHost);
//      i := writes.Params.IndexOfName('User_Name');
//      writes.Params[i] := 'User_Name='+sMainUserName;
      writes.Params.add('User_Name='+sMainUserName);
//      i := writes.Params.IndexOfName('Password');
//      writes.Params[i] := 'Password='+sMainPassword;
      writes.Params.add('Password='+sMainPassword);
      writes.params.add('Database='+sMainDatabase);



//----------------------------------2
//      reads.connectionName := 'reads';
//      reads.drivername := DRIVERNAME;
      reads.libraryname := DLL;
      reads.vendorlib := LIB;
      reads.GetDriverFunc := FUNC;
//      i := reads.Params.IndexOfName('HostName');
//      reads.Params[i] := 'HostName='+sMainHost;
      reads.Params.add('HostName='+sMainHost);
//      i := writes.Params.IndexOfName('User_Name');
//      reads.Params[i] := 'User_Name='+sMainUserName;
      reads.Params.add('User_Name='+sMainUserName);
//      i := writes.Params.IndexOfName('Password');
//      reads.Params[i] := 'Password='+sMainPassword;
      reads.Params.add('Password='+sMainPassword);
      reads.params.add('Database='+sMAinDatabase);

//---------------------------------3
//      sessiondb.connectionName := 'sessiondb';
//      sessiondb.drivername := DRIVERNAME;
      sessiondb.libraryname := DLL;
      sessiondb.vendorlib := LIB;
      sessiondb.GetDriverFunc := FUNC;
//      i := sessiondb.Params.IndexOfName('HostName');
//      sessiondb.Params[i] := 'HostName='+sSessionHost;
      sessiondb.Params.add('HostName='+sSessionHost);
//      i := writes.Params.IndexOfName('User_Name');
//      sessiondb.Params[i] := 'User_Name='+sSessionUserName;
      sessiondb.Params.add('User_Name='+sSessionUserName);
//      i := writes.Params.IndexOfName('Password');
//      sessiondb.Params[i] := 'Password='+sSessionPassword;
      sessiondb.Params.add('Password='+sSessionPassword);
      sessiondb.params.add('Database='+sSessionDatabase);

    finally
      ini.free;
    end;
  finally
    al.unlock;
  end;

end;


procedure TMYSQLRDTPDataModule.DataModuleCreate(Sender: TObject);
begin

  //NOP
//TODO -cunimplemented: unimplemented block
end;

procedure TMYSQLRDTPDataModule.ConnectWrite;
var
  bRetry: boolean;
begin

  repeat
    bRetry := false;
    try
      if not writes.connected then begin
        FWriteQueries := 0;
        writes.connected := true;
        Debug.Log(self,'write connection opened');
        //self.BeginTransaction;

      end;
    except
      on E: Exception do begin
        if (pos('failed to connect', lowercase(e.message))> 0) or (pos('lost connection', lowercase(e.message))> 0) then begin
          bRetry := true;
          reads.connected := false;
        end
        else begin
//          beeper.beep(700,300);
          Debug.Log(self,reads.Params.Text);
          Debug.Log(self,reads.DriverName);
          Debug.Log(self,reads.LIbraryName);
          Debug.Log(self,reads.VendorLIb);
          Debug.Log(self,'auto-retry on EXCEPTION '+e.message);
          raise;
        end;
      end;
    end;
  until bRetry = false;

  //self.writes.ExecuteDirect('ROLLBACK;');

//  if not writes.connected then begin
//    FWriteQueries := 0;
//    writes.connected := true;
//    Debug.Log('write connection opened');
//    self.BeginTransaction;
//  end;

end;

function TMYSQLRDTPDataModule.ContextVerified: boolean;
begin
  result := FContextVErified;
end;

function TMYSQLRDTPDataModule.CopyTable(sSource, sTarget: string): IHolder<TStringLIst>;
var
  sQuery: string;
begin
  result := THolder<TStringList>.create;
  result.o := TStringlist.create;

  sQuery := 'create table '+sTarget+' like '+sSource;
  ExecuteWrite(sQuery);
  result.o.add(sQuery);

  sQuery := 'insert into '+sTarget+' select * from '+sSource;
  ExecuteWrite(sQuery);
  result.o.add(sQuery);



end;

procedure TMYSQLRDTPDataModule.connectsystem;
var
  bRetry: boolean;
begin
  repeat
    bRetry := false;
    try
      if not sessiondb.connected then begin
//        Debug.Log(GetCurrentDir);
        sessiondb.connected := true;
        VerifyContext;
        Debug.Log(self,'session connection opened');
      end;

    except
      on E: Exception do begin
        if (pos('failed to connect', lowercase(e.message))> 0) or (pos('lost connection', lowercase(e.message))> 0) then begin
          bRetry := true;
          reads.connected := false;
        end
        else begin
          beeper.beep(700,300);
          Debug.Log(self,reads.Params.Text);
          Debug.Log(self,reads.DriverName);
          Debug.Log(self,reads.LIbraryName);
          Debug.Log(self,reads.VendorLIb);
          Debug.Log(self,'auto-retry on EXCEPTION');
          raise;
        end;

      end;
    end;
  until bRetry = false;



end;

procedure TMYSQLRDTPDataModule.IncWrite;
begin
  inc(FWriteQueries);

end;

destructor TMYSQLRDTPDataModule.destroy;
begin
  writes.free;
  sessiondb.free;
  reads.free;
  keybot_link.free;

  inherited;
end;

constructor TMYSQLRDTPDataModule.create;
begin
  inherited;
  raise ENotImplemented.Create('Deprecated in favor of unidac.. else implement BeginTransactionOn...etc');
  writes := TSQLConnection.create(nil);
  sessiondb := TSQLConnection.create(nil);
  reads := TSQLConnection.create(nil);
  keybot_link := TBetterTcpClient.create(nil);

  with writes do begin
//    ConnectionName := 'Wms'+inttohex(integer(pointer(self)), 16);
//    DriverName := DRIVERNAME;
    GetDriverFunc := FUNC;
    LibraryName := DLL;
    LoginPrompt := False;
    Params.Clear;
    params.add('DriverUnit=DBXDynalink');
    params.add('DriverPackageLoader=TDBXDynalinkDriverLoader');
    params.add('DriverPackage=DBXCommonDriver110.bpl');
    params.add('DriverAssemblyLoader=Borland.Data.TDBXDynalinkDriverLoader');
    params.add('DriverAssembly=Borland.Data.DbxCommonDriver,Version=11.0.5000.0,Culture=neutral,PublicKeyToken=a91a7c5705831a4f');
//    params.add('HostName=ibm.ds2network.com');
    params.add('BlobSize=-1');
    params.add('ErrorResourceFile=');
    params.add('LocaleCode=0000');
    params.add('Compressed=False');
    params.add('Encrypted=False');
//    params.add('ErrorResourceFile=');
//    Params.Add('DriverName=MySQL');
    KeepConnection := true;
    VendorLib := LIB;
  end;
  with sessiondb do begin
//    ConnectionName := 'Sms'+inttohex(integer(pointer(self)),16);
//    DriverName := DRIVERNAME;
    GetDriverFunc := FUNC;
    LibraryName := DLL;
    KeepConnection := true;
    LoginPrompt := False;
    Params.Clear;
    params.add('DriverUnit=DBXDynalink');
    params.add('DriverPackageLoader=TDBXDynalinkDriverLoader');
    params.add('DriverPackage=DBXCommonDriver110.bpl');
    params.add('DriverAssemblyLoader=Borland.Data.TDBXDynalinkDriverLoader');
    params.add('DriverAssembly=Borland.Data.DbxCommonDriver,Version=11.0.5000.0,Culture=neutral,PublicKeyToken=a91a7c5705831a4f');
//    params.add('HostName=ibm.ds2network.com');
    params.add('BlobSize=-1');
    params.add('ErrorResourceFile=');
    params.add('LocaleCode=0000');
    params.add('Compressed=False');
    params.add('Encrypted=False');
//    params.add('ErrorResourceFile=');
//    Params.Add('DriverName=MySQL');
    VendorLib := LIB;
  end;
  with keybot_link do begin
    RemoteHost := '127.0.0.1';
    RemotePort := '667';
  end;
  with reads do begin
//    ConnectionName := 'Rms'+inttohex(integer(pointer(self)),16);
//    DriverName := DRIVERNAME;
    GetDriverFunc := FUNC;
    LibraryName := DLL;
    LoginPrompt := False;
    PArams.Clear;
    params.add('DriverUnit=DBXDynalink');
    params.add('DriverPackageLoader=TDBXDynalinkDriverLoader');
    params.add('DriverPackage=DBXCommonDriver110.bpl');
    params.add('DriverAssemblyLoader=Borland.Data.TDBXDynalinkDriverLoader');
    params.add('DriverAssembly=Borland.Data.DbxCommonDriver,Version=11.0.5000.0,Culture=neutral,PublicKeyToken=a91a7c5705831a4f');
//    params.add('HostName=ibm.ds2network.com');
    params.add('BlobSize=-1');
    params.add('ErrorResourceFile=');
    params.add('LocaleCode=0000');
    params.add('Compressed=False');
    params.add('Encrypted=False');
//    params.add('ErrorResourceFile=');
//    Params.Add('DriverName=MySQL');
    KeepConnection := true;
    VendorLib := LIB;
  end;

  DataModuleCreate(nil);
//  ConfigureFromContext;

end;

procedure TMYSQLRDTPDataModule.Execute(sQuery: string; connection: TSQLConnection;
  out ds: TCustomSQLDataset);
var
  bREtry: boolean;
  rc: integer;
begin
//  Al.Lock;
//  try

  ds := nil;
  rc := 0;
  repeat
    bREtry := false;
    try
      Debug.Log(self,'Execute:'+sQuery);
//      Debug.Log(self,'Params:'+#13#10+connection.params.text);
      Connection.AutoClone := false;

      connection.Execute(sQuery, nil, @ds);

    except
      on E: Exception do begin
        if zpos('gone away', lowercase(e.message))>= 0 then begin
          Connection.Connected := false;
          Connection.Connected := true;
          inc(rc);
          bRetry := rc < 30;
          Debug.Log(self,'RETRYING after '+e.Message+' #'+inttostr(rc));

        end else
        if zpos('can''t connect to ', lowercase(e.message))>= 0 then begin
          inc(rc);
          bRetry := rc < 30;
          Debug.Log(self,'RETRYING after '+e.Message+' #'+inttostr(rc));

        end

        else begin
          Debug.Log(self,'RETRYING after '+e.Message+' #'+inttostr(rc));
          raise;
        end;
      end;
    end;
  until bretry = false;
//  finally
//    Al.Unlock;
//  end;

end;

function TMYSQLRDTPDataModule.ExecuteDirect(sQuery: string; connection: TSQLConnection): integer;
var
  bREtry: boolean;
  rc: integer;
begin
//  Al.Lock;
//  try
  result := 0;
  rc := 0;
  repeat
    bREtry := false;
    try
      sQuery := Trimstr(sQuery);
      if sQuery='' then
        exit;
      Debug.Log(self,'Execute Direct:'+sQuery);
      result := connection.ExecuteDirect(sQuery);
      inc(rc);
    except
      on E: Exception do begin
        if pos('failed to connect', lowercase(e.message))> 0 then
          bRetry := rc < 15
        else
          raise;

      end;
    end;
  until bretry = false;
//  finally
//    Al.Unlock;
//  end;
end;


procedure TMYSQLRDTPDataModule.ExecuteRead(sQuery: string;
  out dataset: TSERowSet);
var
  ds: TCUSTOMSQLDataset;
begin
  ds := nil;
  WaitForCommands;
  ConnectWrite;
{$IFDEF SAVE_MEMORY}
  LockConsole;
{$ENDIF}
  try
    ExecuteRead_PLatform(sQuery, ds);
    dataset := TSERowSet.create;
    dataset.CopyFromDAtaSet(ds);


  finally
{$IFDEF SAVE_MEMORY}
    UnlockConsole;
{$ENDIF}
  end;
end;

procedure TMYSQLRDTPDataModule.ExecuteRead_Platform(sQuery: string; out dataset: TCustomSQLDataset);
begin
  inherited;

  WaitForCommands;
  ConnectRead;
{$IFDEF SAVE_MEMORY}
  LockConsole;
{$ENDIF}
  try
    Execute(sQuery, reads, dataset);

  finally
{$IFDEF SAVE_MEMORY}
    UnlockConsole;
{$ENDIF}
  end;
end;



function TMYSQLRDTPDataModule.ExecuteSystem(sQuery: string): integer;
var
  sLeft, sRight: string;
begin
  WaitForCommands;
  ConnectSystem;
  sRight := sQuery;    
  while SplitString(sRight, '--execute--', sLeft, sRight) do begin
    ExecuteDirect(sLeft, sessiondb);
  end;
  result := ExecuteDirect(sLeft, sessiondb);
end;


function TMYSQLRDTPDataModule.ExecuteSystem_Platform(sQuery: string;
  out dataset: TCustomSQLDataset): integer;
begin
  WaitForCommands;
  ConnectSystem;
  Execute(sQuery, sessiondb, dataset);
  result := 0;//todo 1: what is this for? who useds the result?
end;


function TMYSQLRDTPDataModule.TableExists(sTable: string): boolean;
var
  t: ni;
  rs: TSERowSet;
begin
  rs := nil;
  try
    result := false;
    ExecuteRead('show tables', rs);
    if rs.RowCount = 0 then
      exit;

    for t:= 0 to rs.RowCount-1 do begin
      if comparetext(rs.Values[0,t], sTable) = 0 then begin
        exit(true);
      end;
    end;

  finally
    rs.Free;
    rs := nil;
  end;

end;

function TMYSQLRDTPDataModule.TryGetNextID(iKey: integer; out res: int64): boolean;
var
  i64, iChecksum: int64;
  cmd: byte;
begin
  result := true;
  try
    try
      keybot_link.Connect;
      cmd := 1;
      keybot_link.SendBuf(cmd, 1);
      keybot_link.SendBuf(iKey, 4);
      keybot_link.WaitForData(8000);
      if keybot_link.ReceiveBuf(i64, 8, 0)=0 then begin
        result := false;
        exit;
      end;

      if keybot_link.ReceiveBuf(iChecksum, 8, 0)=0 then begin
        result := false;
        exit;
      end;


      //checksum
      if (not iCheckSum) <> i64 then begin
         result := false;
         exit;
      end;




    finally
      keybot_link.Disconnect;
    end;
    res := i64;
  except
    result := false;
  end;
end;

function TMYSQLRDTPDataModule.TrySetNextID(iKey: integer; value: int64): boolean;
var
  i64, iCheckSum: int64;
  cmd: byte;
begin
  result := false;
  try
    try

      keybot_link.Connect;
      cmd := 2;
      keybot_link.SendBuf(cmd, 1);
      keybot_link.SendBuf(iKey, 4);
      keybot_link.SendBuf(value, 8);
      if keybot_link.WaitForData(8000) then begin
        if keybot_link.ReceiveBuf(i64, 8, 0)=0 then begin
          result := false;
          exit;
        end;

        if keybot_link.ReceiveBuf(iCheckSum, 8, 0)=0 then begin
          result := false;
          exit;
        end;

        //checksum
        if (not iCheckSum) <> i64 then begin
           result := false;
           exit;
        end;

        result := true;

      end;
    finally
      keybot_link.Disconnect;
    end;
  except
    result := false;
  end;
end;

procedure TMYSQLRDTPDataModule.VerifyContext;
var
  ds: TSERowSet;
  sContext: string;
begin
  exit;

  if ContextVerified then
    exit;

  ds := nil;
  try
    //query the hosts
    ExecuteSystem('select * from se_host where ID="'+self.FID+'"', ds);
    if ds.RowCount = 0 then
      raise EClassException.create('Host Record not found in database for ID:'+FID);
    //TODO 3: Auto add hosts?

    sContext := ds.CurRecordFields['Context'];
    if sContext <> FContext then begin
      ExecuteSystem('update se_host set context="'+StringToHex(FContext)+'" where ID="'+self.FID+'"');
      
    end else begin
      FContextVerified := true;
    end;
    

    
  finally
    ds.free;
  end;

end;

{ TDataPool }





function TMYSQLRDTPDataModule.ExecuteWriteRaw(sQuery: string): integer;
var
  sLeft, sRight: string;
  ds: TCustomSQLDataset;
//  slParsed: TStringlist;
  t: ni;
begin
  result := 0;//todo 1: who uses this? what is it for?
  ds := nil;
  try
    ConnectWrite;
    sRight := sQuery;
    sQuery := '';//<-- do this to conserve memory
//    slParsed := ParseString(sRight, '--execute--');
    while SplitString(sRight, '--execute--', sLeft, sRight) do begin
      ExecuteDirect(sLeft,writes);
    end;
    ExecuteDirect(sLeft, writes);


//    ConnectWrite;
//    for t:= 0 to slParsed.count-1 do begin
//      ExecuteDirect(slParsed[t], writes);
//    end;

//    slParsed.free;

  finally
    ds.free;
  end;

end;

function TMYSQLRDTPDataModule.ExecuteWrite_Platform(sQuery: string;
  out dataset: TCustomSQLDataset): integer;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TMYSQLRDTPDataModule.GetNextID(sKey: string): Int64;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TMYSQLRDTPDataModule.ExecuteSystem(sQuery: string;
  out dataset: TSERowSet): integer;
var
  ds: TCustomSQLDataSet;
begin
  ds := nil;
  try
    result := ExecuteSystem_Platform(sQuery, ds);
    dataset := TSERowSet.create;
    dataset.CopyFromDAtaSet(ds);
  finally
    ds.free;
  end;
end;

function TMYSQLRDTPDataModule.ExecuteWrite(sQuery: string; out dataset: TSERowSet): integer;
var
  ds: TCustomSQLDataSet;
begin
  ds := nil;
  try
    result := ExecuteWrite_Platform(sQuery, ds);
    dataset := TSERowSet.create;
    dataset.CopyFromDAtaSet(ds);
  finally
    ds.free;
  end;
end;



initialization
//TDBXConnectionFActory.GetConnectionFactory.GetDriverNames(itms);

Debug.ConsoleLog('LIB='+lib);
Debug.ConsoleLog('DBX='+DLL);

end.
