unit DataObjectServices;

interface
uses
  PersistentInterfacedObject, DataObjectCache, typex,
  SysUtils, ThreadManager, classes,
  DataObjectCacheManager, DataObjectFactory, betterobject, SharedObject, ManagedThread, backgroundthreads,
  Variants, stringx,  debug, orderlyinit,



  {$IFDEF COMPILE_DOSV_IN_EXE}
  {$ENDIF}
  DataObjectCommonDefinitions, ExceptionsX, ServerInterfaceInterface;
type

  TDataObjectServices = class(TLockQueuedObject)
  private
    FDataCenter: integer;
    FID: integer;
    FActiveUsers: integer;
    FPAradox: boolean;
//    FSimpleServer: TSimpleServerInterface;

//    function GetSimpleServer: TSimpleServerInterface;
    function GetRefreshingAccounts: boolean;
    procedure SetRefreshingAccounts(const Value: boolean);
    function GetACtiveUsers: integer;
    procedure SetActiveUsers(const Value: integer);
    function GetParadox: boolean;
    procedure SetParadox(const Value: boolean);
  protected
    FMainModule: TObject;
    FAccounts: TList;
    //Standard Objects
    FThreadManager: TThreadManager;
    FEnableDebugMessages: boolean;
    bInitialized: boolean;
    iProgressRefcount: integer;
    FName: string;
    FRefreshingAccounts: boolean;
    FHostName, FEndPoint, FContext: string;

    function GetAccounts(idx: integer): integer;
    procedure CloseServer;

    function GetClass(idx: integer): string;
    function GetClassCount: integer;
    function GetEnableDebugMessages: boolean;
    procedure SetEnableDebugMessages(Value: boolean);
  public
    constructor Create(sName: string); reintroduce; virtual;
    destructor Destroy; override;

    function InitializeServer(sHostName, sEndPoint, sContext: string): boolean;
    procedure BeforeDestruction; override;

//    function NewServerNew: TSimpleServerInterface;
    function NewServer: IServerInterface;
    procedure NoNeedServer(si: IServerInterface);


    //1to1 links to other objects
    property MainModule: TObject read FMainModule write FMainModule;
    property ThreadManager: TThreadManager read FThreadManager;

    //data-tier load balancing

    property RefreshingAccounts: boolean read GetRefreshingAccounts write SetRefreshingAccounts;

    //Packet Debug Messages
    property EnableDebugMessages: boolean read GetEnableDebugMessages
                                          write SetEnableDebugMessages;

    property Name: string read FName write FName;
    property DataCenter: integer read FDataCenter write FDataCenter;
    property ID: integer read FID write FID;

    property ActiveUsers: integer read GetACtiveUsers write SetActiveUsers;

    property Paradox: boolean read GetParadox write SetParadox;

  end;
//------------------------------------------------------------------------------
  TDOSVPool = class (TFakeLockQueuedObject)
  private
    FServers: TList;
    function GetServer(idx: integer): TDataObjectServices;
    function GetCount: integer;
    function GEtServersbyName(sName: string): TDataObjectServices;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Add(sName: string; sHostName: string; sEndPoint, sContext:string; iDataCenterID: integer): TDataObjectServices;
    property Servers[idx: integer]: TDataObjectServices read GetServer; default;
    property ServersByName[sName: string]: TDataObjectServices read GEtServersbyName;
    property Count: integer read GetCount;
    procedure Clear;
    procedure Delete(idx: integer);

    function GetHighestAccountID: integer;

  end;
var
  GLastValidationMessage: string; //!Ditch this prototype global
  DOSVPool : TDOSVPool;
(*threadvar
  DOSV: TDataObjectServices;*)

function GetLastValidationMessageG: string;
procedure SetLastValidationMessageG(str: string);

procedure LogStartupMessage(sMessage: string);

function ObjectCache(d: TObject): TDataObjectCache;

implementation



uses
  {$IFDEF DEBUG}
  {$ENDIF}
  Dataobject, ServerInterfaceFactory;



//####################################################################
constructor TDataObjectServices.Create(sName: string);
begin
  inherited Create;
  bInitialized := false;

  FName := sName;
  //Set objects to nil;
  FAccounts := TList.create;


end;
//####################################################################
destructor TDataObjectServices.Destroy;
begin
  FAccounts.free;
  inherited;
end;
//------------------------------------------------------------------------------
procedure TDataObjectServices.BeforeDestruction;
//happens before destroy is called... shuts down the server
begin

  if binitialized then begin
    CloseServer;
    bInitialized := false;
  end;
  inherited;

end;
//------------------------------------------------------------------------------
function TDataObjectServices.InitializeServer(sHostName, sEndPoint, sContext: string): boolean;
//initializes all objects required to connect to data-tier

begin
  result := true;
  try

    FHostName := sHostName;
    FEndPoint := sEndPoint;
    FContext := sContext;
    //Configure the global transport Layer Engine

    //Initialize the Server Interface

        //Connect the Server Interface to the Transport Engine

    LogStartupMessage('Creating Data Object Factory');
    //Create the Data Object Class Factory


    //Define the Data objects supported by the DLL
    //Important note: This code should be supplied in a globally available
    //function in a unit called "DataObjectDefinitions" which must be
    //supplied by the project using the framework.
    DefineDataObjects(self);

    bInitialized := true;
  except
    result := false;
  end;
end;

//####################################################################
procedure TDataObjectServices.CloseServer;
begin

  //Free standard objects

  FThreadManager.free;

  bInitialized := false;

  //Free reference to IPathways LAST so that exceptions can be propagated for
  //during the majority of the shutdown.
  FMainModule := nil;


end;

//####################################################################
//--------------------------------------------------------------------

//####################################################################



//--------------------------------------------------------------------
function GetLAstValidationMessageG: string;
begin
  result := GLastValidationMessage;
end;
//--------------------------------------------------------------------
procedure SetLastValidationMessageG(str: string);
begin
  GLastValidationMessage := str;
end;


//####################################################################

function TDataObjectServices.GetClass(idx: integer): string;
begin
  result := DOCF.ClassNames[idx];
end;
//--------------------------------------------------------------------
function TDataObjectServices.GetClassCount: integer;
begin
  result := DOCF.ClassCount;
end;
//--------------------------------------------------------------------
function TDataObjectServices.GetEnableDebugMessages: boolean;
begin
  result := FEnableDebugMessages;
end;
//--------------------------------------------------------------------
procedure TDataObjectServices.SetEnableDebugMessages(Value: boolean);
begin
  FEnableDebugMessages := Value;
end;
//--------------------------------------------------------------------

//--------------------------------------------------------------------
procedure LogStartupMessage(sMessage: string);
begin
  {$IFDEF COMPILE_DOSV_IN_EXE}
  {$ENDIF}
end;
//--------------------------------------------------------------------
function TDataObjectServices.GetAccounts(idx: integer): integer;
//returns the account ID at specified index in the account list
begin
  result := integer(FAccounts[idx]);
end;

{ TDOSVPool }

function TDOSVPool.Add(sName: string; sHostName: string; sEndPoint, sContext:string; iDataCenterID: integer): TDataObjectServices;
begin
  LockWrite;
  try
    UniqueString(sName);
    result := TDataObjectServices.create(sName);
    result.DataCenter := iDataCenterID;
    //set ID based on index in server list
    result.ID := FServers.count;
    FServers.add(result);
    result.InitializeServer(MakeThreadSafe(sHostName), MakeThreadSafe(sEndPoint), MakeThreadSafe(sContext));
//    speech.SayNatural('Found server configuration for context '+sContext, true);
  finally
    UnLockWrite;
  end;
end;

procedure TDOSVPool.Clear;
begin
  while count > 0 do begin
    delete(0);
  end;
end;

constructor TDOSVPool.Create;
begin
  inherited;
  FServers := TList.create;
end;

procedure TDOSVPool.Delete(idx: integer);
begin
  TObject(self.FServers[idx]).free;
  FServers.delete(idx);
end;

destructor TDOSVPool.Destroy;
var
  t: integer;
begin
  LockWrite; //remain locked permanently
  for t:= 0 to FServers.count-1 do begin
    TDataObjectServices(FServers[t]).CloseServer;
    TDataObjectServices(FServers[t]).free;
  end;

  FServers.free;
  UnLockWrite;
  inherited;
end;

function TDOSVPool.GetCount: integer;
//private -- Getter returns number of data tiers
begin
  result := FServers.count;
end;

function TDOSVPool.GetServer(idx: integer): TDataObjectServices;
//returns a data tier at specified index in data-tier list
var
  s: string;
begin
  try
    LockRead;
    try
      if idx > FServers.count-1 then
        raise ECritical.create('idx ('+idx.tostring+') >= FServers.count ('+FServers.count.tostring+')');
      result := TDataObjectServices(FServers[idx]);
    finally
      UnLockRead;
    end;
  except
    on e: Exception do begin
      s := 'Data-tier not found ('+inttostr(idx)+') '+ e.message;
      Debug.Log(s, 'error');
      raise ENewException.create(925, '', s);
    end;
  end;


end;




//------------------------------------------------------------------------------
function TDOSVPool.GetHighestAccountID: integer;
begin
  result := 0;

end;

//------------------------------------------------------------------------------
function TDOSVPool.GetServersbyName(sName: string): TDataObjectServices;
var
  t: integer;
begin
  sName := lowercase(sName);
  result := nil;
  for t:= 0 to FServers.count-1 do begin
    if lowercase(Servers[t].Name) = sName then
      result := Servers[t];
  end;

  if result = nil then
    raise Exception.create('Server ' +sName+' not found.');


end;

{ TDOSVFetchAccountThread }



function TDataObjectServices.GetRefreshingAccounts: boolean;
begin
  LockRead;
  try
    result := FRefreshingAccounts
  finally
    UnlockRead;
  end;
end;

(*function TDataObjectServices.GetSimpleServer: TSimpleServerInterface;
begin
  LockRead;
  try
    result := FSimpleServer;
  finally
    UnlockRead;
  end;
end;*)

procedure TDataObjectServices.SetRefreshingAccounts(
  const Value: boolean);
begin
  LockWrite;
  try
    FRefreshingAccounts := value;
  finally
    UnlockWrite;
  end;

end;

function TDataObjectServices.GetActiveUsers: integer;
begin
  LockRead;
  try
    result := FActiveUsers;
  finally
    UnlockRead;
  end;
end;

procedure TDataObjectServices.SetActiveUsers(const Value: integer);
begin
  LockWrite;
  try
    FActiveUsers := value;
  finally
    UnlockWrite;
  end;
end;

function TDataObjectServices.GetParadox: boolean;
begin
  LockRead;
  try
    result := FParadox;
  finally
    UnlockRead;
  end;
end;

procedure TDataObjectServices.SetParadox(const Value: boolean);
begin
  LockWrite;
  try
    FParadox := value;
  finally
    UnlockWrite;
  end;
end;

(*function TDataObjectServices.NewServerNew: TSimpleServerInterface;
begin

  result := TSimpleServerInterface.create(self,FHostName,FEndPoint, nil);

end;*)

function TDataObjectServices.NewServer: IServerInterface;
begin

  result := SIF.NeedServer(FhostName, FEndPoint, FContext);//TServerInterface.create(self);
//  result.HostName := FHostName;
//  result.EndPoint := FEndPoint;


end;




procedure TDataObjectServices.NoNeedServer(si: IServerInterface);
begin
  if si = nil then
    exit;
  sif.NoNeedServer(si);
end;

function ObjectCache(d: TObject): TDataObjectCache;
var
  dd: TDataObject;
begin
  dd := d as TDataObject;

  result := TDataObjectCache(dd.Cache);

end;


procedure oinit;
begin
  DOSVPool := TDOSVPool.create;
end;

procedure ofinal;
begin
//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;


initialization
(*  TComObjectFactory.Create(ComServer,TDataObjectServices, GUID_PwayComServer,
    'TDataObjectServices','Pathways Data Com Server', ciInternal,
    tmSingle);*)
  //DOSV := nil;
  orderlyinit.init.RegisterProcs('DataObjectServices', oinit, ofinal);





finalization
//  DOSVPool.free;



end.
