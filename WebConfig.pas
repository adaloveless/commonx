unit WebConfig;
//Contains the TWebConfig class
//This class is responsible for reading and updating configuration values
//(that pertain to the workstation operation) stored in the workstation's
//registry and in the workstation's Pathways.INI file.
xxx


interface
uses {registry, }sysutils,
{$IFDEF MSWINDOWS}
  windows,
  SimpleWinsock,
{$ENDIF}
  classes, SharedObject, orderlyinit, betterobject;
const
  DT_INDEX_PWLN = 0;
  DT_INDEX_ISRN = 1;
  INVALID_DATATIER = 255;


type
//------------------------------------------------------------------------------
  TNetConnection= class(TFakeLockQueuedObject)
  private
    FMiddleWare: boolean;
    FHost: string;
    FEndPoint: string;
    FName: string;
    FURL: string;
    FFarm: string;
    FLoad: integer;
    FCitrix: boolean;
    FNoTest: boolean;
    FTestURL: string;
    FParadox: boolean;
    FContext: string;



    function GetLoad: integer;
    function GetTestURL: string;
    procedure SetTestURL(const Value: string);
    function GetURL: string;
    procedure SetURL(const Value: string);
    function GetFarm: string;
    function GetHost: string;
    function GetName: string;
    procedure SetFarm(const Value: string);
    procedure SetHost(const Value: string);
    procedure SetName(const Value: string);
    function GetParadox: boolean;
    procedure SetParadox(const Value: boolean);
    function GetContext: string;
    procedure SetContext(const Value: string);
  protected
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    property Name: string read GetName write SetName;
    property MiddleWare: boolean read FMiddleWare write FMiddleWare;
    property Citrix: boolean read FCitrix write FCitrix;
    property Host: string read GetHost write SetHost;
    property Endpoint: string read FEndPoint write FEndPoint;
    property URL: string read GetURL write SetURL;
    property Farm: string read GetFarm write SetFarm;
    property Load: integer read GetLoad;
    property NoTest: boolean read FNoTest write FNoTest;
    property TestURL: string read GetTestURL write SetTestURL;
    property Paradox: boolean read GetParadox write SetParadox;
    property Context: string read GetContext write SetContext;
  end;

//------------------------------------------------------------------------------
  TWebConfig = class(TFakeLockQueuedObject)
  private
    FConfigFile: string;
    FWebLog: boolean;
    FEnableAuditLog: boolean;
    FExcessiveAuditLog: boolean;
    FDataCenterID: integer;
    FHTMLDocPath: string;

    FLogAlertLocation: string;
    FLogFileKeyNames: TStrings;
    FLogFileLocations: TStrings;
    FLogFileCount: integer;

    FConnectionLimit: integer;

    FReportPDFTempFileLocation: string;

    function GetConn(sDestName: string): TNetConnection;
    function GetFarmRouter: string;
    function GetInitialized: boolean;
    procedure SetEchoPageParameters(const Value: boolean);
    procedure SetFarmRouter(const Value: string);
    function GetEchoPageParameters: boolean;
    function GetDestByIndex(iIndex: integer): TNetConnection;
    function GetLogfileKeyNames(idx: integer): string;
    function GetLogFileLocation(idx: integer): string;
    procedure SetConnectionLImit(limit: integer);
    function GetConnectionLimit: integer;
    function GetExternalREsourcePath: string;
    procedure SetExternalResourcePAth(const Value: string);
    function GetConfigFile: string;
    procedure SetConfigFile(const Value: string);
    function GetReportPDFTempFileLocation: string;
    function GetLogFileName: string;
    function GetLogAlertLocation: string;
    function GetHTMLDocPath: string;
    //    reg: TRegistry;
  protected
    FFarmRouter: string;
    FExternalResourcePath: string;
    FEchoPageParameters: boolean;
    lstConnections: TList;
    FLogFileName: string;


  public
    constructor create(sConfigfile: string); reintroduce;
    destructor Destroy; override;



    function ReadConfigString(sSection, sKeyName, sDefault: string): string;
    procedure WriteConfigString(sKeyName, sKeyValue: string);
    property ReportPDFTempFileLocation: string read GetReportPDFTempFileLocation;
    function HasConnection(sName: string): boolean;
    property ConfigFile: string read GetConfigFile write SetConfigFile;
    property FarmRouter: string read GetFarmRouter write SetFarmRouter;
    property Root: string read GetFarmRouter write SetFarmRouter;
    property ExternalResourcePath: string read GetExternalREsourcePath write SetExternalResourcePAth;
    property TemplateRoot: string read GetExternalResourcePAth write SetExternalResourcePath;
    property conn[sDestName: string]: TNetConnection read GetConn;
    property DestByIndex[iIndex: integer]: TNetConnection read GetDestByIndex;
    function IsConnectionDefined(sDestName: string): boolean;
    function IndexOfConnection(sDestName: string): integer;
    property LogFileName: string read GetLogFileName;
    property LogFileLocation[idx: integer]: string read GetLogFileLocation;
    property LogFileKeyNames[idx: integer]: string read GetLogfileKeyNames;
    property LogAlertLocation: string read GetLogAlertLocation;
    property HTMLDocPath: string read GetHTMLDocPath;



    property ConnectionLimit: integer read GetConnectionLimit write SetConnectionLimit;


    property EchoPageParameters: boolean read GetEchoPageParameters write SetEchoPageParameters;

    property WebLog: boolean read FWebLog write FWebLog;

    property Initialized: boolean read GetInitialized;


    property EnableAuditLog: boolean read FEnableAuditLog;
    property ExcessiveAuditLog: boolean read FExcessiveAuditLog;
    property DataCenterID: integer read FDataCenterID;

    function AddConnection: TNetConnection;
    procedure ClearConnections;
    function ConnectionCount: integer;

    function SaveConfig: boolean;
    function LoadConfig: boolean;
  end;

  function GetWindowsDir: string;

function WSC: TWebconfig;inline;

var
  WebServerConfig: TWebConfig;



{#############################################################################}
implementation

uses
  {dialogs, }systemx, IniFiles;

//-----------------------------------------------------------------------------
procedure TWebConfig.WriteConfigString(sKeyName, sKeyValue: string);
//Description: Updates the registry for the sKeyName key with
// the sKeyValue value.
begin
  raise Exception.create('WriteConfigString is unimplemented');
end;
//-----------------------------------------------------------------------------
function TWebConfig.ReadConfigString(sSection, sKeyName, sDefault: string): string;
//Description: Returns the registry setting for the sKeyName key.
var
  fIni : TIniFile;
  sDir : Array[0..255] of WideChar;
begin
  fIni := nil;
  //Lock the critical section (to ensure thread safe operation);
  //LockRead;
  try
  	if GetWindowsDirectory(@sDir[0], 255) > 255 then begin
	    exit;
  	end;

	  fIni := TIniFile.Create(slash(sDir) + 'webconfig.INI');
  	if fIni = nil then begin
//    if not reg.OpenKey(FBaseKey, false) then begin
      result := '';
    end else begin
//	    Result := fIni.ReadString(FBaseKey, sKeyName, '');
//      result := reg.ReadString(sKeyName);
//      reg.CloseKey;
    end;
  finally
    //UnLock the critical section (to ensure thread safe operation);
    //UnLockRead;
    fIni.Free;
  end;
end;
//-----------------------------------------------------------------------------
constructor TWebConfig.create(sConfigFile: string);
begin
  inherited Create;
  FConfigFile := sConfigFile;

  lstConnections := Tlist.create;
//  if sConfigFile <> '' then
    LoadConfig;

end;

//-----------------------------------------------------------------------------
destructor TWebConfig.Destroy;
begin
  lstConnections.free;
  FLogFileKeyNames.free;
  FLogFileLocations.free;
//  reg.free;
//  DeleteCriticalSection(sect);
  inherited;
end;
//------------------------------------------------------------------------------
function TWebConfig.AddConnection: TNetConnection;
var
  dest: TNetConnection;
begin
  dest := TNetConnection.create;

  result := dest;
  lstConnections.add(dest);

end;
//------------------------------------------------------------------------------
function TWebConfig.GetConn(sDestName: string): TNetConnection;
var
  idx: integer;
begin
  LockString;
  try
    UniqueSTring(sDestName);
  finally
    UnlockString;
  end;

  result := nil;
  LockRead;
  try

    idx := IndexOfConnection(sDestName);

    if idx <0 then
      raise exception.create(sDestName+' is not defined in server configuration');

    result := TNetConnection(lstConnections[idx]);
  finally
    UnlockRead;
  end;


end;

function TWebConfig.GetFarmRouter: string;
begin
  LockString;
  try
    result := FFarmRouter;
    UniqueString(result);
  finally
    UnlockString;
  end;
end;

function TWebConfig.GetInitialized: boolean;
begin
  raise exception.create('Initialized property is unimplemented');

end;

function TWebConfig.LoadConfig: boolean;
var
  ini: TInifile;
  iDestCount: integer;
  t: integer;
  dest: TNetConnection;
  sDestKey: string;
begin

  ini := TIniFile.create(FConfigFile);
  FLogFileKeyNames := TStringList.create;
  FLogFileLocations := TStringList.create;
  //LockWrite;
  try
    //Read MAIN STUFF
    FEchoPageParameters := lowercase(ini.ReadString('main', 'EchoPageParameters', 'false'))='true';
    WebLog := lowercase(ini.ReadString('main', 'WebLog', 'false'))='true';

    FExternalResourcePath := ini.ReadString('main', 'ExternalResourcePath', '');
    FExternalResourcePath := slash(FExternalResourcePath);
    FFarmRouter := ini.ReadString('main', 'FarmRouter', '');
    FFarmRouter := slash(FFarmRouter,'/');
    FEnableAuditLog := lowercase(ini.ReadString('main', 'EnableAuditLog', 'false'))='true';
    FExcessiveAuditLog := lowercase(ini.ReadString('main', 'ExcessiveAuditLog', 'false'))='true';
    FDataCenterID := ini.ReadInteger('main', 'DataCenterID', 0);
    FExcessiveAuditLog := lowercase(ini.ReadString('main', 'ExcessiveAuditLog', 'false'))='true';
    FDataCenterID := ini.ReadInteger('main', 'DataCenterID', 0);

    FConnectionLimit := strtoint(ini.ReadString('main', 'ConnectionLimit', '100'));


    FLogAlertLocation := lowercase(ini.ReadString('main', 'LogAlertLocation', ''));

    FHTMLDocPath := lowercase(ini.ReadString('main', 'HTMLDocPath', slash(ExtractFilePath(DLLName))+'HTMLDOC\'));

//    SetDefaultWinsockTimeout(strtoint(ini.ReadString('main', 'WinsockTimeoutMS', '300000')));


    // Read all the keys and locations under the [logfiles] section
    ini.ReadSection('logfiles', FLogFileKeyNames);
    FLogFileCount := FLogFileKeyNames.Count;
    for t := 0 to FLogFileCount-1 do begin
      FLogFileLocations.Add(ini.ReadString('logfiles',FLogFileKeyNames[t],''));
    end;

    FLogFileName := ini.ReadString('main', 'LogFileName', '');
    if FLogFileName = '' then
      FLogFileName := 'log';
    if extractfilepath(FLogFileName)='' then
      FLogFileName := slash(extractfilepath(DLLName))+'logs\';


    //Get the Number of Connections from INI file
    iDestCount := 0;
    try
      iDestCount := strtoint(ini.ReadString('main', 'ConnectionCount', '0'));
    except
      result := false;
    end;

    //Read Destrinations
    for t:= 0 to iDestCount-1 do begin
      sDestKey := 'Connection'+inttostr(t);
      dest := AddConnection;
      dest.MiddleWare := lowercase(ini.ReadString(sDestKey, 'Middleware', 'false'))='true';
      dest.Citrix := lowercase(ini.ReadString(sDestKey, 'Citrix', 'false'))='true';
      dest.Context := (ini.ReadString(sDestKey, 'Context', ''));
      dest.Name := ini.ReadString(sDestKey, 'Name', sDestKey);
      dest.NoTest := ini.ReadString(sDestKey, 'NoTest', 'false')='true';
      dest.TestURL := ini.ReadString(sDestKey, 'TestURL', '');

      if dest.Middleware or dest.Citrix then begin
        dest.Host := ini.ReadString(sDestKey, 'Host', '');
        dest.EndPoint := ini.ReadString(sDestKey, 'EndPoint', '3284');
      end else begin
        dest.Host := ini.ReadString(sDestKey, 'URL', '');
        dest.URL := ini.ReadString(sDestKey, 'URL', '');
      end;
    end;
    result := true;
  finally
    ini.free;
    //UnLockWrite;
  end;
end;

function TWebConfig.SaveConfig: boolean;
begin
  raise exception.create('Save Config is unimplemented');
  end;

procedure TWebConfig.SetEchoPageParameters(const Value: boolean);
begin
  FEchoPageParameters := value;

end;

procedure TWebConfig.SetFarmRouter(const Value: string);
begin
  LockString;
  try
    FFarmRouter := Value;
    UniqueString(FFarmRouter);
  finally
    UnlockString;
  end;


end;

{ TNetConnection }

constructor TNetConnection.Create;
begin
  inherited create;

  LockString;
  try
    FHost := '';
    FURL := '';
    FEndPoint := '';
    FName := '';
  finally
    UnlockString;
  end;  


end;

destructor TNetConnection.Destroy;
begin
  inherited;
end;


function TNetConnection.GetContext: string;
begin
  LockString;
  try
     result := FContext;
  finally
    Unlockstring;
  end;
end;

function TNetConnection.GetFarm: string;
begin
  LockString;
  try
    result := FFarm;
    UniqueString(result);
  finally
    UnlockString;
  end;
end;

function TNetConnection.GetHost: string;
begin
  LockString;
  try
    result := FHost;
    UniqueString(result);
  finally
    UnlockString;
  end;
end;

function TNetConnection.GetLoad: integer;
begin
  result := FLoad;
end;


procedure TWebConfig.ClearConnections;
begin
  //Destroy all Connections
  while lstConnections.count > 0 do begin
    TNetConnection(lstConnections[0]).free;
    lstConnections.delete(0);
  end;
end;

function TWebConfig.ConnectionCount: integer;
begin
  result := lstConnections.count;
end;

function TWebConfig.GetEchoPageParameters: boolean;
begin
  result := FEchoPageParameters;
end;






function TWebConfig.GetDestByIndex(iIndex: integer): TNetConnection;
begin
  LockRead;
  try
    result := TNetConnection(lstConnections[iIndex]);
  finally
    UnlockRead;
  end;

end;

function TWebConfig.IsConnectionDefined(sDestName: string): boolean;
begin
  LockString;
  try
    UniqueSTring(sDestName);
  finally
    UnlockString;
  end;

  LockRead;
  try
    result := IndexOfConnection(sDestName)>-1;
  finally
    UnlockRead;
  end;

end;

function TWebConfig.IndexOfConnection(sDestName: string): integer;
var
  t: integer;
begin
  LockString;
  try
    uniqueString(sDestName);
  finally
    UnlockString;
  end;


  LockRead;
  try
    result := -1;
    for t:= 0 to lstConnections.count-1 do begin
      if lowercase(TNetConnection(lstConnections[t]).Name) = lowercase(sDestName) then begin
        result := t;
        break;
      end;
    end;
  finally
    UnlockRead;
  end;
end;

function GetWindowsDir: string;
var
  sTemp: array[0..255] of AnsiChar;
begin
  if (GetWindowsDirectory(@sTemp[0], 255) > 255) then begin
    raise Exception.create('Windows directory too long')
  end else
    result := sTemp;

end;


function TWebConfig.HasConnection(sName: string): boolean;
var
  t: integer;
begin
  LockString;
  try
    UniqueString(sName);
  finally
    UnlockString;
  end;

  LockRead;
  try
    result := false;
    for t:= 0 to ConnectionCount -1 do begin
      if lowercase(DestByIndex[t].Name) = lowercase(sName) then begin
        result := true;
      end;
    end;
  finally
    UnlockRead;
  end;

end;

function TWebConfig.GetLogfileKeyNames(idx: integer): string;
begin
  LockString;
  try
    result := '';
    if idx <= (FLogFileCount-1) then
      result := FLogFileKeyNames[idx];

    UniqueString(result);
  finally
    UnlockString;
  end;
end;

function TWebConfig.GetLogFileLocation(idx: integer): string;
begin
  LockString;
  try

    result := '';
    if idx <= (FLogFileCount-1) then
      result := FLogFileLocations[idx];

    UniqueString(result);

  finally
    UnlockString;
  end;
end;

function TWebConfig.GetConnectionLimit: integer;
begin
  //Lockread;
  try
    result := FConnectionLimit;
  finally
    //UNlockRead;
  end;

end;

procedure TWebConfig.SetConnectionLImit(limit: integer);
begin
  //LockWrite;
  try
    self.FConnectionLimit := limit;
  finally
    //UnLockWrite;
  end;
end;

function TNetConnection.GetName: string;
begin
  LockString;
  try
    result := FName;
    UniqueString(result);
  finally
    UnlockString;
  end;
end;

function TNetConnection.GetParadox: boolean;
begin
  Lock;
  try
    result := FParadox;
  finally
    Unlock;
  end;
end;

function TNetConnection.GetTestURL: string;
begin
  LockString;
  try
    result := FTestURL;
    UniqueString(Result);
  finally
    UnlockString;
  end;
end;

function TNetConnection.GetURL: string;
begin
  LockString;
  try
    result := FURL;
    UniqueString(result);
  finally
    UnlockString;
  end;
end;


procedure TNetConnection.SetContext(const Value: string);
begin
  LockString;
  try
    FContext := Value;
  finally
    UnlockString;
  end;
end;

procedure TNetConnection.SetFarm(const Value: string);
begin
  LockString;
  try
    FFarm := value;
    UniqueString(FFarm);
  finally
    UnlockString;
  end;
end;

procedure TNetConnection.SetHost(const Value: string);
begin
  LockString;
  try
    FHost := value;
    UniqueString(FHost);
  finally
    UnlockString;
  end;
end;

procedure TNetConnection.SetName(const Value: string);
begin
  LockString;
  try
    FName := Value;
    UniqueString(FName);
  finally
    UnlockString;
  end;
end;

procedure TNetConnection.SetParadox(const Value: boolean);
begin
  Lock;
  try
    FParadox := value;
  finally
    Unlock;
  end;
end;

procedure TNetConnection.SetTestURL(const Value: string);
begin
  Lock;
  try
    FTestURL := value;
    UniqueString(FTestURL);
  finally
    Unlock;
  end;
end;

procedure TNetConnection.SetURL(const Value: string);
begin
  Lock;
  try
    FURL := value;
    UniqueString(FURL);
  finally
    Unlock;
  end;
end;

function TWebConfig.GetExternalREsourcePath: string;
begin
  LockString;
  try
    result := dllpath+'html\';
//    Result := FExternalResourcePath;
    UniqueString(result);
  finally
    UnlockString;
  end;
end;

procedure TWebConfig.SetExternalResourcePAth(const Value: string);
begin
  LockString;
  try
    FExternalREsourcePath := Value;
    uniqueString(FExternalResourcePath);
  finally
    UnlockString;
  end;
end;

function TWebConfig.GetConfigFile: string;
begin
  LockString;
  try
    result := FConfigFile
  finally
    UnlockString;
  end;

end;

procedure TWebConfig.SetConfigFile(const Value: string);
begin
  LockString;
  try
    FConfigFile := value;
    UniqueSTring(FConfigFile);
  finally
    UnlockString;
  end;
end;


function TWebConfig.GetReportPDFTempFileLocation: string;
begin
  LockString;
  try
    result := FReportPDFTempFileLocation;
    uniqueString(result);
  finally
    UnlockString;
  end;

end;


function TWebConfig.GetLogFileName: string;
begin
  LockString;
  try
    result := FlogFileName;
    UniqueString(result);
  finally
    UnlockString;
  end;
end;


function TWebConfig.GetLogAlertLocation: string;
begin
  LockString;
  try
    result := FlogAlertLocation;
    UniqueString(result);
  finally
    UnlockString;
  end;
end;

function TWebConfig.GetHTMLDocPath: string;
begin
  LockString;
  try
    result := FHTMLDocPath;
    UniqueString(result);
  finally
    UnlockString;
  end;
end;


procedure oinit;
begin
  WebServerConfig := nil;
  //  WebServerConfig := TWebConfig.create('\SOFTWARE\Digital Tundra\');

end;

procedure ofinal;
begin
  //  WebServerConfig.free;

end;


function WSC: TWebconfig;inline;
begin
  result := WebServerConfig;
end;

initialization
  init.RegisterProcs('WebConfig', oinit, ofinal);

//-----------------------------------------------------------------------------
finalization

//-----------------------------------------------------------------------------
end.
