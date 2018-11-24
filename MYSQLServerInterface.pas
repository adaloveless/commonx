unit MYSQLServerInterface;
//Defines functions that interface with the data tier.
//DONE 1: For Some reason errors are being suppressed... no errors raises when can't connect

{$D+}
interface

uses SysUtils, DataObject, tickcount,  xml.xmlintf, xml.xmldoc,
  DataObjectFactory, exceptions, ServerInterfaceInterface, debug,
  DataObjectCache, classes, sharedobject, variants, typex;

const
  RQ_RECORD_QUERY = $0990;
  RQ_QUERY = $0999;
  RQ_UPDATE_QUERY = $0997;


type

  TUnmarhsallDebugEvent = procedure (sText: string) of object;
  TMYSQLServerInterface = class(TFakeLockQueuedObject, IServerInterface)
  //r: Implements functions that interface with the data-tier.
  private
    FEndpoint: string;
    FHost: string;
    FTimeOut: integer;
    FDisconnectImmediately: boolean;
    FContext: string;
    FUseStorageEngineCoordinator: boolean;
    function GetTimeOut: integer;
    procedure SetTimeout(const Value: integer);
    function GetEndPoint: string;
    function GetHostName: string;
    function GetDataCenter: integer;
    function GetDataTier: integer;
    function GetAbortedTransactions: integer;
    function GetActiveTransactions: integer;
    function GetRetryingTransactions: integer;
    function GetTotalTransactions: integer;
    procedure SetAbortedTransactions(const Value: integer);
    procedure SetActiveTransactions(const Value: integer);
    procedure SetRetryingTransactions(const Value: integer);
    procedure SetTotalTransactions(const Value: integer);
    function GetContext: string;
    procedure SetContext(value: string);
    procedure SetEndpoint(value: string);
    procedure SetHostName(value: string);
    procedure CheckConnected;

    //Defines functions that interface with the data tier
    protected
      FDebugger: TUnmarhsallDebugEvent;
      FTotalTransactions: integer;
      FActiveTransactions: integer;
      FRetryingTransactions: integer;
      FAbortedTransactions: integer;
      FTransactionLog: TStringList;
      FEnableTransactionLog: boolean;
      FLastError: string;
      FLastErrorCode: integer;
      function Connect: boolean;
    public
      constructor Create; override;
      destructor Destroy; override;

      //DEFINE INTERFACE FUNCTIONS HERE
      //****************************************************************7
      function Login(cache: TDataObjectCache; out doSession: TDataObject; sUserName, sGroupName, sPassword: string; iAccountID: integer; iSystemType: integer; sIPAddress: string; bSystemAccount: boolean = false): boolean;
      procedure Logout(iSessionID: integer);

      function Post(obj: TDataObject; out objOutput: TDataObject; cache: TDataObjectCache; iSessionID: integer; bIncludeSubFields:boolean =false):boolean; overload;
      function Post(obj: TDataObject; iSessionID: integer; bIncludeSubFields:boolean =false):boolean; overload;

      function PreFetchGroup(cache: TDataObjectCache; out obj: TDataObject; sType: string; params: variant; iSessionID: integer; iTimeoutMS: integer = 300000): boolean; overload;
      function PreFetchGroup(cache: TDataObjectCache; out obj: TDataObject; iType: integer; params: variant; iSessionID: integer; iTimeoutMS: integer = 300000): boolean; overload;

      function GetStructureFromEngine(id: integer): string;
      function LazyQuery(cache: TDataObjectCache; out obj: TDataObject; sQuery: string; iSessionID: integer; bExpectMany: boolean; slDebug: TStringList = nil; iTimeoutMS: integer = 300000): boolean;
      function Query(cache: TDataObjectCache; out obj: TDataObject; sQuery: string; iSessionID: integer; bExpectMany: boolean; slDebug: TStringList = nil; iTimeoutMS: integer = 300000): boolean;
      function UpdateQuery(cache: TDataObjectCache; sQuery: string; iSessionID: integer; bExpectMany: boolean=false; slDebug: TStringList = nil; iTimeoutMS: integer = 30000): boolean;
      function NoTransUpdateQuery(cache: TDataObjectCache; sQuery: string; iSessionID: integer): boolean;
      function FireForgetQuery(cache: TDataObjectCache; sQuery: string; iSessionID: integer; bSerialize: boolean= false): boolean;
      function RecordQuery(cache: TDataObjectCache; out obj: TDataObject; sQuery: string; iSessionID: integer; bExpectMany: boolean; iTimeoutMS: integer = 300000): boolean;

      function LazyQueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
        iIgnoreKeys: integer; sBaseType: string; vBaseKeys: variant;
        slDebug: TStringList; sSubType: string; iSubKeys: integer): boolean;overload;

      function LazyQueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
        sBaseType: string; vBaseKeys: variant;
        slDebug: TStringList; sSubType: string; iSubKeys: integer): boolean;overload;

      function GhostQueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
        sBaseType: string; vBaseKeys: variant;
        slDebug: TStringList; sSubType: string; iSubKeys: integer): boolean;

//      function QueryMap(cache: TDataObjectCache; out obj: TDataObject; sQuery: string; iSessionID,
//        iTimeoutMS: integer; bLazy: boolean; sBaseType: string; iBaseKeys: integer; slDebug: TStringList; sSubType: string = ''; iSubKeys: integer = 0): boolean;overload;
      function QueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: string; iSessionID,
        iTimeoutMS: integer; bLazy: boolean; iIgnoreKeys: integer; iBaseType: integer; vBaseKeys: variant; slDebug: TStringList; iSubType:integer = 0; iSubKeys: integer = 0): boolean;overload;
      function QueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: string; iSessionID,
        iTimeoutMS: integer; bLazy: boolean; iBaseType: integer; vBaseKeys: variant; slDebug: TStringList; iSubType:integer = 0; iSubKeys: integer = 0): boolean;overload;

      function QueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: string; iSessionID,
        iTimeoutMS: integer; bLazy: boolean; iIgnoreKeys: integer; sBaseType: string; vBaseKeys: variant;
        slDebug: TStringList;
        sSubType:string = ''; iSubKeys: integer = 0): boolean;overload;

      function QueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: string; iSessionID,
        iTimeoutMS: integer; bLazy: boolean; sBaseType: string; vBaseKeys: variant;
        slDebug: TStringList;
        sSubType:string = ''; iSubKeys: integer = 0): boolean;overload;


      function Fetch(cache: TDataObjectCache; var obj: TDataObject; sType: string; params: variant; iSessionID: integer; iTimeoutMS: integer = 300000): boolean; overload;
      function Fetch(cache: TDataObjectCache; out obj: TDataObject; iConst: integer; params: variant; iSessionID: integer; iTimeoutMS: integer = 300000): boolean; overload;
      function Fetch(cache: TDataObjectCache; out obj: TDataObject; token: TDataObjectToken; iSessionID: integer; iTimeoutMS: integer = 300000): boolean; overload;
      function LazyFetch(cache: TDataObjectCache; out obj: TDataObject; iType: integer; params: variant; iSessionID: integer; iTimeoutMS: integer = 300000): boolean; overload;
      function LazyFetch(cache: TDataObjectCache; out obj: TDataObject; sType: string; params: variant; iSessionID: integer; iTimeoutMS: integer = 300000): boolean; overload;
      function GhostFetch(cache: TDataObjectCache; out obj: TDataObject; sType: string; params: variant; iSessionID: integer; bLazy: boolean = true; iTimeoutMS: integer = 300000; bcheckCacheOnly: boolean = false): boolean; overload;
      function GetNextID(iType: integer; iSessionID: integer=0): integer;
      function SetNextID(iType: integer; iID: int64): boolean;
      procedure SetRemoteContext(sContext: string);
      procedure Time;
      function Ghost(cache: TDataObjectCache; out obj: TDataObject; sType: string; params: variant; iSessionID: integer): boolean; overload;
      function New(cache: TDataObjectCache; out obj: TDataObject; sType: string; params: variant; iSessionID: integer): boolean; overload;

      function GetReplayLogs(since: TDatetime): string;

      property DataCenter: integer read GetDataCenter;
      property DataTier: integer read GetDataTier;

      property Debugger: TUnmarhsallDebugEvent read FDebugger write FDebugger;

      property TotalTransactions: integer read GetTotalTransactions write SetTotalTransactions;
      property ActiveTransactions: integer read GetActiveTransactions write SetActiveTransactions;
      property RetryingTransactions: integer read GetRetryingTransactions write SetRetryingTransactions;
      property AbortedTransactions: integer read GetAbortedTransactions write SetAbortedTransactions;

      procedure IncTotalTransactions;
      procedure DecTotalTransactions;
      procedure IncActiveTransactions;
      procedure DecActiveTransactions;
      procedure IncRetryingTransactions;
      procedure DecRetryingTransactions;
      procedure IncAbortedTransactions;
      procedure DecAbortedTransactions;
      procedure LogTransaction(sString: string);
      function DrainTransactionLog: string;
      property EnableTransactionLog: boolean read FEnableTransactionLog write FEnableTransactionLog;

      procedure ContinueConnection;
      procedure Rollback;
      procedure Commit;
      procedure SetLastError(s: string);overload;
      procedure SetLastError(iCode: integer; s: string);overload;
      function GetLastErrorMessage: string;
      function GetLastErrorCode: integer;
      property TimeOut: integer read GetTimeOut write SetTimeout;
      property HostName: string read GetHostName write SetHostName;
      property EndPoint: string read GetEndPoint write SetEndPoint;
      property Context: string read GetContext write SetContext;

      function DispatchCallback: boolean;
      property UseStorageEngineCoordinator: boolean read FUseStorageEngineCoordinator write FUseStorageEngineCoordinator;
      procedure Delete(cache: TdataObjectCache; obj: TDataObject);
    end;

implementation

uses DataObjectServices,  SpecialDataObjects;

const ERC_SERVER_BUSY = 932;

//------------------------------------------------------------------------------
destructor TMYSQLServerInterface.Destroy;
begin
  FTransactionLog.free;
  inherited;
end;


function TMYSQLServerInterface.DispatchCallback: boolean;
begin
  raise ECritical.create('not implemented');
end;

//------------------------------------------------------------------------------
constructor TMYSQLServerInterface.Create;
begin
  inherited Create;
  FDebugger := nil;
  FTotalTransactions := 0;
  FActiveTransactions := 0;
  FRetryingTransactions := 0;
  FAbortedTransactions := 0;
  FTransactionLog := TStringlist.create;
  FTimeOut := 999999999;
  self.FEnableTransactionLog := true;
  FreeWithReferences := true;



//  FXRefPool := nil;
end;

//------------------------------------------------------------------------------
function TMYSQLServerInterface.Post(obj: TDataObject; out objOutput: TDataObject; cache: TDataObjectCache; iSessionID: integer; bIncludeSubFields:boolean =false):boolean;
//Description: Saves a Data object to the database be marshalling it into
//a packet and transmitting it to the server.
var
  lid: string;
  sSql: string;
  objSub: TDataObject;
  t: ni;
  bWasNew: boolean;
  vOldParams: variant;
begin
  objOutput := nil;
  result := true;
  if not obj.IsList then begin
    bWasNew := obj.IsNew;
    if bWasNew then
      sSql := obj.InsertQuery
    else
      sSql := obj.UpdateQuery;

    Debug.Log('Posting '+obj.name+' with query '+sSQL);
    ComparatioNetwork.ExecSQL(sSQL, lid);
    obj.IsChanged := false;
    if bWasNew then begin
      vOldParams := obj.Token.VariantParams;
      if (lid = '') and (obj.KeyupdateQuery <> '') then
        lid := FunctionQuery(obj.KeyupdateQuery);

      if lid <> '' then begin
        if obj.IdentityKeyCount > 0 then
          obj.Token.Params[0] := strtoint64(lid);
        cache.NotifyParentKeyUpdate(obj, vOldParams);
      end;
    end;


  end else begin
    for t:= 0 to obj.objectcount-1 do begin
      objsub := obj.obj[t];
      if objSub.Ischanged then
        Post(objSub, objOutput, cache, 0);
    end;
  end;


end;
//------------------------------------------------------------------------------
function TMYSQLServerInterface.Fetch(cache: TDataObjectCache;
                              out obj: TDataObject;
                               iConst: integer;
                               params: variant;
                           iSessionID: integer;
                           iTimeoutMS: integer = 300000): boolean;
begin
  raise ECritical.create('not implemented');

end;



//------------------------------------------------------------------------------
function TMYSQLServerInterface.GetNextID(iType: integer; iSessionID: integer): integer;
begin
  raise ECritical.create('not implemented');

end;

//------------------------------------------------------------------------------
function TMYSQLServerInterface.New(cache: TDataObjectCache; out obj: TDataObject; sType: string;
  params: variant; iSessionID: integer): boolean;
begin

  try
    DOCF.LowLevelDOCreate(obj, cache, sType, params, DataCenter, DataTier, isessionid);
    obj.New(iSessionID);
    obj.Reference();
    result := true;
  except
        On E: Exception do begin
      SetLastError(499, E.Message);
      result := false;
    end;
  end;

(*  SetThreadSessionID(iSessionID);

  //Setup the statistics

  //pass a TDataObject to the overloaded NEW function
  result := New(TDataObjectCache(cache.RealObject), doTemp, sType, params, iSessionID);

  //assign the TDataObject to the TDataObject interface OUT
  obj := doTemp;*)

end;


//------------------------------------------------------------------------------
function TMYSQLServerInterface.Ghost(cache: TDataObjectCache; out obj: TDataObject; sType: string;
  params: variant; iSessionID: integer): boolean;
begin
  try
    DOCF.LowLevelDOCreate(obj, cache, sType, params, DataCenter, Datatier, isessionid);
    obj.Ghost;
    result := true;
  except
    result := false;
  end;

end;



function TMYSQLServerInterface.Fetch(cache: TDataObjectCache;
  out obj: TDataObject; token: TDataObjectToken; iSessionID,
  iTimeoutMS: integer): boolean;
begin
  raise ECritical.create('not implemented');
end;


function TMYSQLServerInterface.Fetch(cache: TDataObjectCache;
  var obj: TDataObject; sType: string; params: variant; iSessionID,
  iTimeoutMS: integer): boolean;
var
  //o: TDataObject;
  c: TDataObjectClass;
  sFetchQuery: string;
  sListOf: string;
begin
  if not DOCF.LowLevelDOCreate(obj, cache, sType, params, 0,0,0) then
    raise ECritical.create('Failed to create '+sType);
  if obj = nil then
    raise ECritical.create('object is nil!');
  sfetchQuery := obj.FetchQuery;

  if obj.IsList then begin
    sListOF := obj.ListOf;
    result := QueryMap(cache, obj, sFetchQuery, iSessionid, 0, false, sType, params, nil,sListOf, DOCF.GetKeycountForClass(DOCF.GetClassTypeForClassName(sListOf)))
  end else
    result := QueryMap(cache, obj, sFetchQuery, iSessionid, 0, false, sType, params, nil);



end;

function TMYSQLServerInterface.FireForgetQuery(cache: TDataObjectCache;
  sQuery: string; iSessionID: integer; bSerialize: boolean = false): boolean;
begin
  raise ECritical.create('not implemented');


end;





//------------------------------------------------------------------------------
function TMYSQLServerInterface.GhostFetch(cache: TDataObjectCache;
  out obj: TDataObject; sType: string; params: variant;
  iSessionID: integer; bLazy: boolean = true; iTimeoutMS: integer = 300000;bCheckCacheOnly: boolean = false): boolean;
begin
  if bCheckCacheOnly then begin
    obj := cache.GetExistingObject(sType, params, self.DataCenter, self.DataTier);
    result := not ((obj = nil) or obj.expired);
    if not result then
      result := Ghost(cache, obj, sType, params, iSessionID);
  end else begin
    if bLazy then
      result := LazyFetch(cache, obj, sType, params, iSessionID, iTimeoutMS)
    else
      result := Fetch(cache, obj, sType, params, iSessionID, iTimeoutMS);

    if not result then
      result := Ghost(cache, obj, sType, params, iSessionID);
  end;

end;

function TMYSQLServerInterface.GetLastErrorCode: integer;
begin
  LockRead;
  try
    result := FLastErrorCode;
  finally
    UnlockRead;
  end;

end;

function TMYSQLServerInterface.GetLastErrorMessage: string;
begin
  LockString;
  try
    result := FLastError;
  finally
    UnlockString;
  end;
end;


function TMYSQLServerInterface.GetDataCenter: integer;
begin
  result := 0

end;

function TMYSQLServerInterface.GetDataTier: integer;
begin
  result := 0

end;

function TMYSQLServerInterface.GetEndPoint: string;
begin
  LockREAd;
  try
    result := FEndpoint;
  finally
    UnlockREad;
  end;

end;

function TMYSQLServerInterface.GetHostName: string;
begin
  LockRead;
  try
    result := FHost;
  finally
    UnlockRead;
  end;
end;

function TMYSQLServerInterface.Post(obj: TDataObject; iSessionID: integer;
  bIncludeSubFields: boolean): boolean;
var
  objOut: TDataobject;
begin
  result := self.post(obj, objOut, obj.cache as TDataObjectCache, iSessionId, bIncludeSubFields);
end;

function TMYSQLServerInterface.PreFetchGroup(cache: TDataObjectCache; out obj: TDataObject;
  iType: integer; params: variant; iSessionID,
  iTimeoutMS: integer): boolean;
begin
  raise ECritical.create('not implemented');

end;

function TMYSQLServerInterface.PreFetchGroup(cache: TDataObjectCache;
  out obj: TDataObject; sType: string; params: variant; iSessionID,
  iTimeoutMS: integer): boolean;
begin
  raise ECritical.create('not implemented');
end;

function TMYSQLServerInterface.GetAbortedTransactions: integer;
begin
  LockRead;
  try
    result := FAbortedTransactions;
  finally
    UnlockRead;
  end;
end;

function TMYSQLServerInterface.GetActiveTransactions: integer;
begin
  LockRead;
  try
    result := FActiveTransactions;
  finally
    UnlockRead;
  end;

end;

function TMYSQLServerInterface.GetContext: string;
begin
  result := FContext;
end;

function TMYSQLServerInterface.GetRetryingTransactions: integer;
begin
  LockRead;
  try
    result := FRetryingTransactions;
  finally
    UnlockRead;
  end;

end;

function TMYSQLServerInterface.GetTimeOut: integer;
begin
  result := FTimeout;
end;

function TMYSQLServerInterface.GetTotalTransactions: integer;
begin
  LockRead;
  try
    result := FTotalTransactions;
  finally
    UnlockRead;
  end;
end;


procedure TMYSQLServerInterface.SetAbortedTransactions(const Value: integer);
begin
  LockWrite;
  try
    FAbortedTransactions := value;
  finally
    UnlockWrite;
  end;
end;

procedure TMYSQLServerInterface.SetActiveTransactions(const Value: integer);
begin
  LockWrite;
  try
    FActiveTransactions := value;
  finally
    UnlockWrite;
  end;
end;

procedure TMYSQLServerInterface.SetContext(value: string);
begin
  FContext := value;
end;

procedure TMYSQLServerInterface.SetRemoteContext(sContext: string);
begin
  raise ECritical.create('not implemented');

end;

procedure TMYSQLServerInterface.SetEndPoint(Value: string);
begin
  LockWrite;
  try
    FEndpoint := value;
  finally
    UnlockWrite;
  end;

end;

procedure TMYSQLServerInterface.SetHostName(Value: string);
begin
  LockWrite;
  try
    FHost := value;
  finally
    Unlockwrite;
  end;

end;

procedure TMYSQLServerInterface.SetLastError(iCode: integer; s: string);
begin
  SetLastError(s);
end;

function TMYSQLServerInterface.SetNextID(iType: integer; iID: int64): boolean;
begin
  raise ECritical.create('not implemented');
end;

procedure TMYSQLServerInterface.SetLastError(s: string);
begin
  LockString;
  try
    FLastError := s;
  finally
    UnlockString;
  end;
end;

procedure TMYSQLServerInterface.SetRetryingTransactions(const Value: integer);
begin
  LockWrite;
  try
    FRetryingTransactions := value;
  finally
    UnlockWrite;
  end;

end;

procedure TMYSQLServerInterface.SetTimeout(const Value: integer);
begin
  Ftimeout := value;
end;

procedure TMYSQLServerInterface.SetTotalTransactions(const Value: integer);
begin
  LockWrite;
  try
    FTotalTransactions := value;
  finally
    UnlockWrite;
  end;

end;

procedure TMYSQLServerInterface.Time;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TMYSQLServerInterface.DecAbortedTransactions;
begin
  LockWrite;
  try
    dec(FAbortedTransactions);
  finally
    UnlockWRite;
  end;
end;

procedure TMYSQLServerInterface.DecActiveTransactions;
begin
  LockWrite;
  try
    dec(FActiveTransactions);
  finally
    UnlockWrite;
  end;
end;

procedure TMYSQLServerInterface.DecRetryingTransactions;
begin
  LockWrite;
  try
    dec(FRetryingTransactions);
  finally
    UnlockWrite;
  end;
end;

procedure TMYSQLServerInterface.DecTotalTransactions;
begin
  LockWrite;
  try
    dec(FTotalTransactions);
  finally
    UnlockWrite;
  end;
end;


procedure TMYSQLServerInterface.IncAbortedTransactions;
begin
  LockWrite;
  try
    Inc(FAbortedTransactions);
  finally
    UnlockWrite;
  end;
end;

procedure TMYSQLServerInterface.IncActiveTransactions;
begin
  LockWrite;
  try
    Inc(FActiveTransactions);
  finally
    UnlockWrite;
  end;
end;

procedure TMYSQLServerInterface.IncRetryingTransactions;
begin
  LockWrite;
  try
    Inc(FRetryingTransactions);
  finally
    UnlockWrite;
  end;
end;

procedure TMYSQLServerInterface.IncTotalTransactions;
begin
  LockWrite;
  try
    Inc(FTotalTransactions);
    if FTotalTransactions > 2100000000 then
      FTotalTransactions := 0;

  finally
    UnlockWrite;
  end;
end;


function TMYSQLServerInterface.Query(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: string; iSessionID: integer;
  bExpectMany: boolean; slDebug: TStringList = nil; iTimeoutMS: integer =300000): boolean;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TMYSQLServerInterface.QueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: string; iSessionID,
  iTimeoutMS: integer;
  bLazy: boolean;
  iIgnoreKeys: integer;
  iBaseType: integer;
  vBaseKeys: variant;
  slDebug: TStringList;
  iSubType:integer = 0;
  iSubKeys: integer = 0): boolean;
begin
  raise Ecritical.create('this form is not implemented');



end;



(*function TMYSQLServerInterface.QueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
  bLazy: boolean; sBaseType: string; iBaseKeys: TErroneous; slDebug: TStringList; sSubType: string;
  iSubKeys: integer): boolean;
var
  iBaseType: integer;
  iSubType: integer;
begin
  if sSubType = '' then
    iSubType := 0
  else
    iSubType := DOCF.GetConstantForClassname(sSubType);

  iBaseType := DOCF.GetConstantForClassname(sBaseType);

  if iBaseType = -1 then begin
    raise Exception.create('No type registered for '+sBaseType);

  end;

  result := self.QueryMap(cache, obj, sQuery, iSessionID, iTimeoutMS, bLazy, iBaseType, iBaseKeys, slDebug, iSubType, iSubKeys);



end;*)



function TMYSQLServerInterface.RecordQuery(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: string; iSessionID: integer;
  bExpectMany: boolean; iTimeoutMS: integer): boolean;
begin
  raise ECritical.create('not implemented');
end;

function TMYSQLServerInterface.UpdateQuery(cache: TDataObjectCache;
  sQuery: string; iSessionID: integer;
  bExpectMany: boolean=false; slDebug: TStringList = nil;
  iTimeoutMS: integer = 30000): boolean;
begin
  raise ECritical.create('not implemented');
end;

function TMYSQLServerInterface.NoTransUpdateQuery(cache: TDataObjectCache;
  sQuery: string; iSessionID: integer): boolean;
begin
  raise ECritical.create('not implemented');

end;


function TMYSQLServerInterface.Login(cache: TDataObjectCache;
  out doSession: TDataObject; sUserName, sGroupName, sPassword: string;
  iAccountID, iSystemType: integer; sIPAddress: string;
  bSystemAccount: boolean): boolean;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TMYSQLServerInterface.Logout(iSessionID: integer);
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TMYSQLServerInterface.LogTransaction(sString: string);
begin
  if not FEnableTransactionLog then
    exit;
  Lock;
  try

      FTransactionLog.add(sString);
  finally
    Unlock;
  end;
end;

function TMYSQLServerInterface.DrainTransactionLog: string;
begin
  Lock;
  try
    result := Trim(FTransactionLog.text);

    FTransactionLog.Clear;
  finally
    Unlock;
  end;
end;

function TMYSQLServerInterface.Connect: boolean;
begin
  raise ECritical.create('not implemented');
end;

procedure TMYSQLServerInterface.ContinueConnection;
begin
//TODO -cunimplemented: unimplemented block
end;


function TMYSQLServerInterface.LazyFetch(cache: TDataObjectCache;
  out obj: TDataObject; sType: string; params: variant; iSessionID,
  iTimeoutMS: integer): boolean;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TMYSQLServerInterface.LazyFetch(cache: TDataObjectCache;
  out obj: TDataObject; iType: integer; params: variant; iSessionID,
  iTimeoutMS: integer): boolean;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TMYSQLServerInterface.LazyQuery(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: string; iSessionID: integer;
  bExpectMany: boolean; slDebug: TStringList;
  iTimeoutMS: integer): boolean;
begin

  obj := cache.GetExistingObject('TdoQuery', sQuery,0,0);
//        cache.GetExistingObject('TdoQuery', sQuery, Datacenter, DataTier)

  if (obj = nil) or (obj.expired) or ((obj.linkto <> 'nil') and (obj.linkto <> ''))then begin
    result := self.Query(cache, obj, squery, iSessionid, bExpectMany, slDebug, iTimeOUTMS);
  end else begin
    result := true;
    if obj.linkto = 'nil' then begin
      obj := nil;
      result := false;
    end;


  end;

end;


function TMYSQLServerInterface.LazyQueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS,
  iIgnoreKeys: integer; sBaseType: string; vBaseKeys: variant;
  slDebug: TStringList; sSubType: string; iSubKeys: integer): boolean;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TMYSQLServerInterface.GhostQueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
  sBaseType: string; vBaseKeys: variant; slDebug: TStringList;
  sSubType: string; iSubKeys: integer): boolean;
begin
  result := LazyQueryMap(cache, obj, sQuery, iSessionID, iTimeoutMS, sBaseType, vBaseKeys, SLDebug, sSUbType, iSubkeys);
  if not result then begin
    result := Ghost(cache, obj, sBaseType, vBaseKeys, iSessionID);
  end;

end;



function TMYSQLServerInterface.QueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
  bLazy: boolean; iBaseType: integer; vBaseKeys: variant;
  slDebug: TStringList; iSubType, iSubKeys: integer): boolean;
begin
  result := self.QueryMap(cache, obj, sQuery, iSessionID, iTimeoutMS, bLazy, 0, iBaseType, vBasekeys, slDebug, iSubType, iSubKeys)

end;


function TMYSQLServerInterface.QueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
  bLazy: boolean; sBaseType: string; vBaseKeys: variant;
  slDebug: TStringList; sSubType: string; iSubKeys: integer): boolean;
begin
  result := self.QueryMap(cache, obj, sQuery, iSessionID, iTimeoutMS, bLazy, 0, sBaseType, vBaseKeys, slDebug, sSubType, iSubKeys);
end;


function UnwrapObjectfromXMLNode(cache: TDataobjectCache; n: IXMLNode; cType: TDataObjectClass): TDataObject;
var
  t: ni;
  nodeField: IXMLNode;
  iKeyCount: ni;
  v: variant;
  s: string;
begin
  //get the keys (this is the touch part)
  iKeyCount := DOCF.GetKeycountForclass(cType);
  v := VarArrayCreate([0,iKeyCount-1], varVariant);
  for t:= 0 to iKeyCount-1 do begin
    s := n.ChildNodes[t].Text;
    //IF ANY KEYS COME BACK BLANK, there there really aren't any records in the result set
    if s = '' then
      exit(nil);
     v[t] := strtoint64(s);
  end;

  //instatiate the object with the keyss
  DOCF.LowLowLevelDOCreate(result, cache, cType, v, 0,0,0);
  //unwrap the fields
  for t:= 0 to result.FieldCount-1 do begin
    nodefield := n.ChildNodes[result.FieldByIndex[t].Name];
    s := nodefield.Text;

    result.FieldByIndex[t].AsString := s;
  end;

  result.IsChanged := false;

end;

function TMYSQLServerInterface.QueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
  bLazy: boolean; iIgnoreKeys: integer; sBaseType: string;
  vBaseKeys: variant; slDebug: TStringList; sSubType: string;
  iSubKeys: integer): boolean;
var
  outdata: string;
  doc: IXMLDocument;
  node, nodefield: IXMLNode;
  rec: IXMLNode;
  iType: ni;
  t: ni;
  objSub: TDataObject;
  c, cSub: TDataObjectClass;
  s: string;
begin
  //converts a Comparatio XML query into dataobjects
  //Comparatio queries typically look like:
  //<ListData ID="12345">
  //<Record>
  //<FieldName1>FieldValue</FieldName1>
  //<FieldName2>FieldValue</FieldName2>
  //....
  //</Record>
  //<Record>
  //....
  //</Record>
  //......
  //</ListData>


  //build keys
  if (vartype(vBaseKeys) and varArray) > 0 then begin
    for t:= varArrayLowBound(vBaseKeys,1) to varArrayHighBound(vBaseKeys,1) do begin
      squery := stringreplace(sQuery, '~~~'+inttostr(t-varArrayLowBound(vBaseKeys,1))+'~~~', vartostr(vBaseKeys[t]), [rfReplaceAll]);
    end;
  end else begin
    squery := stringreplace(sQuery, '~~~0~~~', vartostr(vBaseKeys), [rfReplaceAll]);
  end;

  c := DOCF.GetClassTypeForClassName(sBaseType);
  cSub := nil;
  if sSubType <> '' then
    cSub := DOCF.GetClassTypeForClassName(sSubType);

  ComparatioNetwork.ForceGetSQL(sQuery, outdata);
  doc := TXMLDocument.Create(nil);
  try
    Debug.Log(outdata);
    doc.LoadFromXML(outdata);
    doc.Active := true;
    node:=doc.DocumentElement;
    if sSubType = '' then begin
      //Single Object
      rec := node.childnodes['Record'];
      obj := UnwrapObjectfromXMLNode(cache, rec, c);
      result := obj <> nil;
    end else begin
      DOCF.LowLevelDOCreate(obj, cache, c, vBaseKeys, 0,0,0);
      rec := node.childnodes['Record'];
      while rec <> nil do begin
        objSub := UnwrapObjectfromXMLNode(cache, rec, csub);
        if objSub = nil then
          break
        else begin
          obj.AddObject(objSub);
          rec := rec.NextSibling;
        end;
      end;
      result := obj <> nil;
    end;


  finally
    doc.active := false;
  end;

end;

function TMYSQLServerInterface.LazyQueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
        sBaseType: string; vBaseKeys: variant;
        slDebug: TStringList; sSubType: string; iSubKeys: integer): boolean;
begin
  result := self.LazyQueryMap(cache, obj, sQuery, iSessionId, iTimeOutMS, 0, sBaseType, vBaseKeys, slDebug, sSubType, iSubKeys);
end;

procedure TMYSQLServerInterface.Rollback;
begin
  raise ECritical.create('not implemented');
end;

procedure TMYSQLServerInterface.Commit;
begin
  raise ECritical.create('not implemented');
end;

function TMYSQLServerInterface.GetReplayLogs(since: TDateTime): string;
begin
  raise ECritical.create('not implemented');
end;


function TMYSQLServerInterface.GetStructureFromEngine(id:integer):string;
begin
  raise ECritical.create('not implemented');
end;


procedure TMYSQLServerInterface.CheckConnected;
begin
  raise ECritical.create('not implemented');
end;

procedure TMYSQLServerInterface.Delete(cache: TdataObjectCache; obj: TDataObject);
var
  sSQL : string;
  lid: string;
begin
  sSQL := obj.DeleteQuery;
  ComparatioNetwork.ExecSQL(sSQL, lid);
end;

end.


