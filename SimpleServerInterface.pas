unit SimpleServerInterface;

//Defines functions that interface with the data tier.
{$D+}
interface

(*uses PersistentInterfacedObject,Packet, Transport, SysUtils, DataObject,
  DataObjectFactory, Windows, applock, ipclientwrapper, better_sockets,sdtppacket,
  ThreadSessionVar, DataObjectCache, classes, sharedobject, simplewinsock, variants;

const
  RQ_RECORD_QUERY = $0990;
  RQ_QUERY = $0999;
  RQ_UPDATE_QUERY = $0997;


type
  TUnmarhsallDebugEvent = procedure (sText: widestring) of object;
  TSimpleServerInterface = class(TFakeLockQueuedObject)
  //r: Implements functions that interface with the data-tier.
  private
    function Transact(var Packet: TRDTPPacket; iTimeOutMS: integer;
      slDebug: TStringList; bForget: boolean): boolean;
    //Defines functions that interface with the data tier
    protected
      FDebugger: TUnmarhsallDebugEvent;
      FDOSV: TObject;
      FTotalTransactions: integer;
      FActiveTransactions: integer;
      FRetryingTransactions: integer;
      FAbortedTransactions: integer;
      FTransactionLog: TStringList;
      FEnableTransactionLog: boolean;
      FLastError: ansistring;
      FLastErrorCode: integer;
      FHost, FEndPoint: ansistring;
      ipw: TMyIPClient;
      FSocket: TBetterCustomIPClient;
      FPacket: TSDTPPacket;
      FCache: TDAtaObjectCache;
      FSessionID: integer;

//      function Transact(var packet: TRDTPPacket; iTimeoutMS: integer = 300000; slDebug: TStringList = nil): boolean;
      function BuildVarArrayFromKeys(iNumKeys: integer): variant;
      function UnMarshallObject(sessionid: integer; cache: TDataObjectCache; packet: TRDTPPacket;slFlyFields: TSTringList = nil): TDataObject;
      procedure UnMarshallObjectContent(sessionid: integer; cache: TDataObjectCache; obj: TDataObject; packet: TRDTPPacket; iPacketFields, iPacketAssociates, iPacketObjects: cardinal; viewProgress: TObject; slFlyFields: TSTringList = nil);
      function UnMarshallObjectInObject(sessionid: integer; cache: TDataObjectCache; doParent: TDataObject; packet: TRDTPPacket; viewProgress: TObject; slFlyFields: TSTringList = nil): TDataObject;
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
      procedure SendPacketHeader;
      procedure REadPacketHeader;
    public

      constructor Create(DOSV: TObject; sHost: ansistring; sEndpoint: ansistring; cache: TDataObjectCache); reintroduce; virtual;
      destructor Destroy; override;

      function Query(cache: TDataObjectCache; out obj: TDataObject; sQuery: ansistring; iSessionID: integer; bExpectMany: boolean; slDebug: TStringList = nil; iTimeoutMS: integer = 300000): boolean;
      function LazyQuery(cache: TDataObjectCache; out obj: TDataObject; sQuery: ansistring; iSessionID: integer; bExpectMany: boolean; slDebug: TStringList = nil; iTimeoutMS: integer = 300000): boolean;
      function GetNextID(iType: integer; iSessionID: integer=0): integer;

      function Login(cache: TDataObjectCache; out doSession: TDataObject; sUserName, sGroupName, sPassword: ansistring; iAccountID: integer; iSystemType: integer; sIPAddress: ansistring; bSystemAccount: boolean = false): boolean;

      procedure Logout(iSessionID: integer);



      function UpdateQuery(cache: TDataObjectCache; sQuery: ansistring; iSessionID: integer; bExpectMany: boolean=false; slDebug: TStringList = nil; iTimeoutMS: integer = 30000): boolean;
      function NoTransUpdateQuery(cache: TDataObjectCache; sQuery: ansistring; iSessionID: integer): boolean;
      function FireForgetQuery(cache: TDataObjectCache; sQuery: ansistring; iSessionID: integer): boolean;
      function RecordQuery(cache: TDataObjectCache; out obj: TDataObject; sQuery: ansistring; iSessionID: integer; bExpectMany: boolean; iTimeoutMS: integer = 300000): boolean;
      function LazyQueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: ansistring; iSessionID, iTimeoutMS: integer;
        iIgnoreKeys: integer; sBaseType: ansistring; vBaseKeys: variant;
        slDebug: TStringList; sSubType: ansistring; iSubKeys: integer): boolean;overload;

      function LazyQueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: ansistring; iSessionID, iTimeoutMS: integer;
        sBaseType: ansistring; vBaseKeys: variant;
        slDebug: TStringList; sSubType: ansistring; iSubKeys: integer): boolean;overload;

      function GhostQueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: ansistring; iSessionID, iTimeoutMS: integer;
        sBaseType: ansistring; vBaseKeys: variant;
        slDebug: TStringList; sSubType: ansistring; iSubKeys: integer): boolean;

      function QueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: ansistring; iSessionID,
        iTimeoutMS: integer; bLazy: boolean; iIgnoreKeys: integer; iBaseType: integer; vBaseKeys: variant; slDebug: TStringList; iSubType:integer = 0; iSubKeys: integer = 0): boolean;overload;
      function QueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: ansistring; iSessionID,
        iTimeoutMS: integer; bLazy: boolean; iBaseType: integer; vBaseKeys: variant; slDebug: TStringList; iSubType:integer = 0; iSubKeys: integer = 0): boolean;overload;

      function QueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: ansistring; iSessionID,
        iTimeoutMS: integer; bLazy: boolean; iIgnoreKeys: integer; sBaseType: ansistring; vBaseKeys: variant;
        slDebug: TStringList;
        sSubType:ansistring = ''; iSubKeys: integer = 0): boolean;overload;

      function QueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: ansistring; iSessionID,
        iTimeoutMS: integer; bLazy: boolean; sBaseType: ansistring; vBaseKeys: variant;
        slDebug: TStringList;
        sSubType:ansistring = ''; iSubKeys: integer = 0): boolean;overload;


      function Ghost(cache: TDataObjectCache; out obj: TDataObject; sType: ansistring; params: variant; iSessionID: integer): boolean; overload;
      function New(cache: TDataObjectCache; out obj: TDataObject; sType: ansistring; params: variant; iSessionID: integer): boolean; overload;


      function GetReplayLogs(since: TDatetime): ansistring;

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
      procedure LogTransaction(sString: ansistring);
      function DrainTransactionLog: ansistring;
      property EnableTransactionLog: boolean read FEnableTransactionLog write FEnableTransactionLog;
      procedure ContinueConnection;
      procedure Rollback;
      procedure Commit;
      procedure SetLastError(s: ansistring);overload;
      procedure SetLastError(iCode: integer; s: ansistring);overload;
      function GetLastErrorMessage: ansistring;
      function GetLastErrorCode: integer;

      property packet: TSDTPPAcket read FPAcket write FPacket;
      function ReadObject(var obj: TDataObject; parent: TDataObject = nil): boolean;
    end;
*)
implementation


{ TSimpleServerInterface }

(*
procedure TSimpleServerInterface.Commit;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.ContinueConnection;
begin

//TODO -cunimplemented: unimplemented block
end;


constructor TSimpleServerInterface.Create(DOSV: TObject; sHost,
  sEndpoint: ansistring; cache: TDataObjectCache);
begin
  inherited create;
  ipw := TMYIPClient.create;
  FSocket := better_sockets.TBetterTCPClient.Create(nil);
  ipw.socket := FSocket;
  FSocket.RemoteHost := sHost;
  FSocket.RemotePort := inttostr(strtoint(sEndpoint)+1);
  FSocket.Active := true;
  FPacket := TSDTPPAcket.create(ipw);
  FCache := cache;


end;

procedure TSimpleServerInterface.DecAbortedTransactions;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.DecActiveTransactions;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.DecRetryingTransactions;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.DecTotalTransactions;
begin

//TODO -cunimplemented: unimplemented block
end;

destructor TSimpleServerInterface.Destroy;
begin
  ipw.Free;
  FSocket.free;
  self.FPacket.free;

  inherited;
end;

function TSimpleServerInterface.DrainTransactionLog: ansistring;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.FireForgetQuery(cache: TDataObjectCache;
  sQuery: ansistring; iSessionID: integer): boolean;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.GetAbortedTransactions: integer;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.GetActiveTransactions: integer;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.GetDataCenter: integer;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.GetDataTier: integer;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.GetLastErrorCode: integer;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.GetLastErrorMessage: ansistring;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.GetNextID(iType, iSessionID: integer): integer;
begin
  packet.FunctionID := $1004;
  SendPacketHeader;
  packet.AddLong(iType);
  //-------------------------
  ReadPacketHeader;
  result := packet.ReadVariant;


end;

function TSimpleServerInterface.GetReplayLogs(since: TDatetime): ansistring;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.GetRetryingTransactions: integer;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.GetTotalTransactions: integer;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.Ghost(cache: TDataObjectCache;
  out obj: TDataObject; sType: ansistring; params: variant;
  iSessionID: integer): boolean;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.GhostQueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: ansistring; iSessionID, iTimeoutMS: integer;
  sBaseType: ansistring; vBaseKeys: variant; slDebug: TStringList; sSubType: ansistring;
  iSubKeys: integer): boolean;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.IncAbortedTransactions;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.IncActiveTransactions;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.IncRetryingTransactions;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.IncTotalTransactions;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.LazyQuery(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: ansistring; iSessionID: integer;
  bExpectMany: boolean; slDebug: TStringList; iTimeoutMS: integer): boolean;
begin
  packet.FunctionID := $0999;
  SendPacketHeader;
  packet.AddString(sQuery);
  packet.AddBoolean(bExpectMany);
  //-------------------------
  ReadPacketHeader;
  result := ReadObject(obj);


end;

function TSimpleServerInterface.LazyQueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: ansistring; iSessionID, iTimeoutMS,
  iIgnoreKeys: integer; sBaseType: ansistring; vBaseKeys: variant;
  slDebug: TStringList; sSubType: ansistring; iSubKeys: integer): boolean;
begin


//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.LazyQueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: ansistring; iSessionID, iTimeoutMS: integer;
  sBaseType: ansistring; vBaseKeys: variant; slDebug: TStringList; sSubType: ansistring;
  iSubKeys: integer): boolean;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.Login(cache: TDataObjectCache;
  out doSession: TDataObject; sUserName, sGroupName, sPassword: ansistring;
  iAccountID, iSystemType: integer; sIPAddress: ansistring;
  bSystemAccount: boolean): boolean;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.Logout(iSessionID: integer);
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.LogTransaction(sString: ansistring);
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.New(cache: TDataObjectCache;
  out obj: TDataObject; sType: ansistring; params: variant;
  iSessionID: integer): boolean;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.NoTransUpdateQuery(cache: TDataObjectCache;
  sQuery: ansistring; iSessionID: integer): boolean;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.Query(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: ansistring; iSessionID: integer;
  bExpectMany: boolean; slDebug: TStringList; iTimeoutMS: integer): boolean;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.QueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: ansistring; iSessionID, iTimeoutMS: integer;
  bLazy: boolean; iIgnoreKeys: integer; sBaseType: ansistring; vBaseKeys: variant;
  slDebug: TStringList; sSubType: ansistring; iSubKeys: integer): boolean;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.QueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: ansistring; iSessionID, iTimeoutMS: integer;
  bLazy: boolean; sBaseType: ansistring; vBaseKeys: variant; slDebug: TStringList;
  sSubType: ansistring; iSubKeys: integer): boolean;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.QueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: ansistring; iSessionID, iTimeoutMS: integer;
  bLazy: boolean; iIgnoreKeys, iBaseType: integer; vBaseKeys: variant;
  slDebug: TStringList; iSubType, iSubKeys: integer): boolean;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.QueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: ansistring; iSessionID, iTimeoutMS: integer;
  bLazy: boolean; iBaseType: integer; vBaseKeys: variant; slDebug: TStringList;
  iSubType, iSubKeys: integer): boolean;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.ReadObject(var obj: TDataObject; parent: TDataObject = nil): boolean;
var
  tt: integer;
  vv: variant;
  vOBjectTYpe: variant;
  vKeys: variant;
  iObjectType: integer;
begin
  //read object header
  packet.ExtendedRead(tt,vObjectType);
  if not (tt in [PDT_SHORT_OBJECT, PDT_LONG_OBJECT]) then
    raise exception.create('object expected');

  //read number of keys
  vv := packet.ReadVariant;
  vKeys := self.BuildVarArrayFromKeys(vv);

  iObjectType := vObjectType;
  obj := FCache.GetExistingObject(iObjectType, vKeys, 0,0);
  if obj = nil then
    DOCF.LowLevelDOCreate(obj, FCache, iObjectType, vKeys, 0,0,Fsessionid);


end;

function TSimpleServerInterface.BuildVarArrayFromKeys(iNumKeys: integer): variant;
//Builds a variant array of the keys in the packet for easy passing to
//the Data Object Factory.
var
  t: integer;
begin
  result := VarArrayCreate([1, iNumKeys], varVariant);
  for t:= 1 to iNumKeys do begin
    result[t] := packet.ReadVariant;

    if assigned(debugger) then begin
      Debugger('-----key:'+vartostr(result[t]));
    end;

  end;

end;


procedure TSimpleServerInterface.REadPacketHeader;
var
  s: ansistring;
begin
  s := ipw.ReadLine;
  s := copy (s, 5,4);
  packet.FunctionID := strtoint('$'+s);

end;

function TSimpleServerInterface.RecordQuery(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: ansistring; iSessionID: integer;
  bExpectMany: boolean; iTimeoutMS: integer): boolean;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.Rollback;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.SendPacketHeader;
begin
  ipw.SendLine('SDTP'+inttohex(packet.FunctionID,4));

end;

procedure TSimpleServerInterface.SetAbortedTransactions(const Value: integer);
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.SetActiveTransactions(const Value: integer);
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.SetLastError(s: ansistring);
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.SetLastError(iCode: integer; s: ansistring);
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.SetRetryingTransactions(const Value: integer);
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.SetTotalTransactions(const Value: integer);
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.Transact(var Packet: TRDTPPacket;
  iTimeOutMS: integer; slDebug: TStringList; bForget: boolean): boolean;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.UnMarshallObject(sessionid: integer;
  cache: TDataObjectCache; packet: TRDTPPacket;
  slFlyFields: TSTringList): TDataObject;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleServerInterface.UnMarshallObjectContent(sessionid: integer;
  cache: TDataObjectCache; obj: TDataObject; packet: TRDTPPacket; iPacketFields,
  iPacketAssociates, iPacketObjects: cardinal; viewProgress: TObject;
  slFlyFields: TSTringList);
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.UnMarshallObjectInObject(sessionid: integer;
  cache: TDataObjectCache; doParent: TDataObject; packet: TRDTPPacket;
  viewProgress: TObject; slFlyFields: TSTringList): TDataObject;
begin

//TODO -cunimplemented: unimplemented block
end;

function TSimpleServerInterface.UpdateQuery(cache: TDataObjectCache;
  sQuery: ansistring; iSessionID: integer; bExpectMany: boolean;
  slDebug: TStringList; iTimeoutMS: integer): boolean;
begin

end;*)

end.


