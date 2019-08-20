unit ServerInterface;
//Defines functions that interface with the data tier.
//DONE 1: For Some reason errors are being suppressed... no errors raises when can't connect

{$D+}
interface

uses Packet, SysUtils, DataObject, simpleabstractconnection, GenericRDTPClient, tickcount, typex, data.db,
  DataObjectFactory,
{$IFDEF MSWINDOWS}
  Windows,
  simplewinsock,
{$ENDIF}
  exceptions, ServerInterfaceInterface, debug, SimpleReliableUDP, storageenginetypes,
  DataObjectCache, classes, sharedobject, variants, packethelpers, systemx, rdtpdb, RDTPKeyBotClient;

const
  RQ_RECORD_QUERY = $0990;
  RQ_QUERY = $0999;
  RQ_UPDATE_QUERY = $0997;


type
  ERDTPExeception = class(Exception);

  TUnmarhsallDebugEvent = procedure (sText: string) of object;
  TServerInterface = class(TRDTPDB, IServerInterface)
  //r: Implements functions that interface with the data-tier.
  protected
//    FKB: TKeybotclient;  NOT NEEDED BECAUSE IT IS PART OF RDTPDB
    FTimeOut: integer;
    FDisconnectImmediately: boolean;
    FOnProgress: TRDTPProgressEvent;
    FCallback: TRDTPCallback;
    FUseStorageEngineCoordinator: boolean;
    FPooltime: ticker;
    procedure DoProgressClose;
    function GetTimeOut: integer;
    procedure SetTimeout(const Value: integer);

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
  private
    function GetPoolTime: ticker;
    procedure SetPoolTime(const Value: ticker);


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

      function BuildVarArrayFromKeys(packet: TRDTPPacket; iNumKeys: integer): variant;
      function UnMarshallObject(sessionid: integer; cache: TDataObjectCache; packet: TRDTPPacket;slFlyFields: TSTringList = nil): TDataObject;
      procedure UnMarshallObjectContent(sessionid: integer; cache: TDataObjectCache; obj: TDataObject; packet: TRDTPPacket; iPacketFields, iPacketAssociates, iPacketObjects: cardinal; viewProgress: TObject; slFlyFields: TSTringList = nil);
      function UnMarshallObjectInObject(sessionid: integer; cache: TDataObjectCache; doParent: TDataObject; packet: TRDTPPacket; viewProgress: TObject; slFlyFields: TSTringList = nil): TDataObject;
      function InitConnectionClass: TSimpleAbstractConnection;
      procedure AddCommonStuff(packet: TRDTPPacket);
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

      function QueryMap(cache: TDataObjectCache;
                        out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
                        bLazy: boolean; iIgnoreKeys: integer; sBaseType: string;
                        vBaseKeys: variant; slDebug: TStringList; sSubType: string = '';
                        iSubKeys: integer = 0): boolean;overload;

      function QueryMap(cache: TDataObjectCache; out obj: TDataObject;
                        sQuery: string; iSessionID, iTimeoutMS: integer; bLazy: boolean;
                        sBaseType: string; vBaseKeys: variant; slDebug: TStringList;
                        sSubType: string = ''; iSubKeys: integer = 0): boolean;overload;


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


      function Fetch(cache: TDataObjectCache; out obj: TDataObject; sType: string; params: variant; iSessionID: integer; iTimeoutMS: integer = 300000): boolean; overload;
      function Fetch(cache: TDataObjectCache; out obj: TDataObject; token: TDataObjectToken; iSessionID: integer; iTimeoutMS: integer = 300000): boolean; overload;
      function LazyFetch(cache: TDataObjectCache; out obj: TDataObject; iType: integer; params: variant; iSessionID: integer; iTimeoutMS: integer = 300000): boolean; overload;
      function LazyFetch(cache: TDataObjectCache; out obj: TDataObject; sType: string; params: variant; iSessionID: integer; iTimeoutMS: integer = 300000): boolean; overload;
      function GhostFetch(cache: TDataObjectCache; out obj: TDataObject; sType: string; params: variant; iSessionID: integer; bLazy: boolean = true; iTimeoutMS: integer = 300000; bcheckCacheOnly: boolean = false): boolean; overload;
      procedure Delete(cache: TdataObjectCache; obj: TDataObject);overload;

      function GetNextID(sType: string): int64;
      function SetNextID(sType: string; iID: int64): boolean;
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
      property OnProgress: TRDTPProgressEvent read FOnProgress write FOnProgress;
      procedure DoProgress(prog: TRDTPPacket);

      function DispatchCallback: boolean;
      property Callback: TRDTPCallback read FCallback write FCallback;
      property UseStorageEngineCoordinator: boolean read FUseStorageEngineCoordinator write FUseStorageEngineCoordinator;


      property PoolTime: ticker read GetPoolTime write SetPoolTime;
      function PingRDTP: boolean;
      function PingDB: boolean;

    end;

function RSTypeToDOType(rs: data.db.TFieldType): TDataFieldClass;
function UnwrapObjectfromRow(cache: TDataobjectCache; rs: TSEROWSet; row: ni; cType: TDataObjectClass): TDataObject;

implementation

uses DtNetConst, DataObjectServices,
  ErrorHandler, specialobjects;

const ERC_SERVER_BUSY = 932;

//------------------------------------------------------------------------------
destructor TServerInterface.Destroy;
begin
  FTransactionLog.free;
  inherited;
end;
function TServerInterface.DispatchCallback: boolean;
var
  iRQ: integer;
begin
  result := false;

  iRQ := callback.request.data[0];
  callback.request.seqseek(3);
  case iRQ of
    //GetContext
    $9000:
      begin
        Debug.Log('Begin Inherited Client Callback handling of GetContext');
        callback.response.addString(Context);          
        Debug.Log('End Inherited Client Callback handling of GetContext');
        result := true;
      end;

  end;
end;

procedure TServerInterface.DoProgressClose;
begin
  if assigned(OnProgress) then
    OnProgress('','',0,0);
end;
procedure TServerInterface.DoProgress(prog: TRDTPPacket);
var
  sMessage: string;
  sDebugLog: string;
  iPos, iMax: integer;
begin
  if assigned(OnProgress) then begin
    prog.SeqSeek(0);
    sMessage := prog.SeqRead;
    sDebugLog := prog.SeqRead;
    ipos := prog.SeqRead;
    iMax := prog.SeqRead;
    OnProgress(sMessage, sDebugLog, iPos, iMax);
  end;

end;

//------------------------------------------------------------------------------
constructor TServerInterface.Create;
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


//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
procedure TServerInterface.Logout(iSessionID: integer);
begin


  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;
//----------------------------------------------------------------------------
function TServerInterface.Login(cache: TDataObjectCache; out doSession: TDataObject; sUserName, sGroupName, sPassword: string; iAccountID: integer; iSystemType: integer; sIPAddress: string; bSystemAccount: boolean = false): boolean;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;
//------------------------------------------------------------------------------
function TServerInterface.Post(obj: TDataObject; out objOutput: TDataObject; cache: TDataObjectCache; iSessionID: integer; bIncludeSubFields:boolean =false):boolean;
//Description: Saves a Data object to the database be marshalling it into
//a packet and transmitting it to the server.
var
  s: string;
  t: ni;
begin
  if obj.IsList then begin
    for t := 0 to obj.objectcount-1 do begin
      if obj.obj[t].IsChanged then
        post(obj.obj[t], objOutput, cache, iSessionID);
    end;
  end else begin
    s := obj.SaveQuery;
    Debug.Log('POST '+obj.classname+' with query: '+s);
  //  if not IsMYSQL then begin
  //    if obj.tablelink <> '' then
  //      cli.WriteQuery('SET IDENTITY_INSERT '+obj.TableLink+' ON;');
  //  end;
    cli.WriteQuery(s);
  end;
end;

//------------------------------------------------------------------------------
function TServerInterface.UnMarshallObject(sessionid: integer; cache: TDataObjectCache; packet: TRDTPPacket; slFlyFields: TStringList = nil): TDataObject;
begin
  result := UnmarshallObjectInObject(sessionid, cache, nil, packet, nil,slFlyFields);
end;

//------------------------------------------------------------------------------
procedure TServerInterface.UnMarshallObjectContent(sessionid: integer; cache: TDataObjectCache; obj: TDataObject;
    packet: TRDTPPacket; iPacketFields, iPacketAssociates, iPacketObjects: cardinal; viewProgress: TObject; slFlyFields: TSTringList = nil);
var
  fIdx: integer;
  t: integer;
  bFirstObjectInObject: boolean;
  iMinProgressUpdate: integer;
    //stores the numbder of objects that must be unmarshalled before
    //reupdating the progress bar (to improve performance)
  iTemp: integer;
  iProgressCheckpoint: integer;
    //the numbder of progress increments currently set, when integer division
    //causes this number to increase... the progress bar is updated;
begin
  bFirstObjectInObject := true;
  fIdx := 0;

  try
    //Check to make sure the PACKET's field count agrees with the
      //Data object definition
    if (slFlyFields <> nil) and (iPacketFields > 0) then begin
      obj.ClearFieldDefs;
      for t := 0 to slFlyFields.count-1 do begin
        obj.AddFieldDef(slFlyFields[t], TVariantDataField, '');
      end;
    end;

    if not (ni(obj.Fieldcount) = ni(iPacketFields)) then
      raise Exception.Create('Packet field count does not match Data Object '+
        'definition.  Object: '+obj.name+' Packet Fields: ' +
        inttostr(iPacketFields)+' DO Fields: ' + inttostr(obj.FieldCount));

    //Unmarshall FIELDS for the object embedded in the packet (optional)
    for t:= 1 to iPacketFields do begin
      obj.FastValues[fidx] := packet.SeqRead;
//      obj.FieldByIndex[fidx].AsVariant := packet.SeqRead;
      inc(fIdx);
    end;


    //Unmashall ASSOCIATE objects embedded in the packet
    for t:= 1 to iPacketAssociates do begin
      //if an object
      if (packet.NextDataType = PDT_SHORT_OBJECT)
      or (packet.NextDataType = PDT_LONG_OBJECT) then begin
        //if unmarshalling the first the object in the object...
        //then clear the objects so that the list doesn't grow in cases
        //where the object was already in the cache.

        //Umarshall the object recursively (the object could have other objects in it)
        UnMarshallObjectInObject(sessionid, cache, obj, packet, viewProgress, slFlyFields)
      end else begin
        if assigned(debugger) then
          Debugger('Packet Format Error while unmarshalling Packet Associates. Expecting object received type: '+inttostr(packet.NextDataType));

        raise Exception.create('Packet Format Error while unmarshalling Packet Associates. Expecting OBJECT received type: '+inttostr(packet.NextDataType));
      end;
    end;

    iProgressCheckpoint := 0;
    iMinProgressUpdate := (iPacketObjects div 20); //1/20th of the total packet
    if iMinProgressUpdate = 0 then
      iMinProgressUpdate := 1;

    //Unmarshall DETAIL objects embedded in the packet
    for t:=1 to iPacketObjects do begin
      if assigned(debugger) then
        Debugger(' object '+inttostr(t)+' of '+inttostr(iPacketObjects));

      if (packet.NextDataType = PDT_SHORT_OBJECT)
      or (packet.NextDataType = PDT_LONG_OBJECT) then begin

        //if unmarshalling the first the object in the object...
        //then clear the objects so that the list doesn't grow in cases
        //where the object was already in the cache.
        if bFirstObjectInObject then begin
          obj.ClearObjects;
          bFirstObjectInObject := false;
        end;

        //Umarshall the object recursively (the object could have other objects in it)
        UnMarshallObjectInObject(sessionid, cache, obj, packet, viewProgress, slFlyFields)
      end else begin
        if assigned(debugger) then
          Debugger('Packet Format Error while unmarshalling Packet Associates. Expecting object received type: '+inttostr(packet.NextDataType));

        raise Exception.create('Packet Format Error while unmarshalling sub objects. Expecting object received type: '+inttostr(packet.NextDataType)+' ... Possible row/field mismatch or subkey count mismatch');

      end;
    end;
  finally
  end;

end;

//------------------------------------------------------------------------------
procedure TServerInterface.AddCommonStuff(packet: TRDTPPacket);
begin
  packet.AddString('RDTPSystem');

end;

function TServerInterface.BuildVarArrayFromKeys(packet: TRDTPPacket; iNumKeys: integer): variant;
//Builds a variant array of the keys in the packet for easy passing to
//the Data Object Factory.
var
  t: integer;
begin
  result := VarArrayCreate([1, iNumKeys], varVariant);
  for t:= 1 to iNumKeys do begin
    result[t] := packet.SeqRead;

    if assigned(debugger) then begin
      Debugger('-----key:'+vartostr(result[t]));
    end;

  end;

end;

//------------------------------------------------------------------------------
function TServerInterface.UnMarshallObjectInObject(sessionid: integer; cache: TDataObjectCache; doParent: TDataObject;
  packet: TRDTPPacket; viewProgress: TObject; slFlyFields: TSTringList = nil): TDataObject;
begin


  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TServerInterface.UpdateQuery(cache: TDataObjectCache; sQuery: string;
  iSessionID: integer; bExpectMany: boolean; slDebug: TStringList;
  iTimeoutMS: integer): boolean;
begin
  raise ENotImplemented.create('not implemented');
end;

//------------------------------------------------------------------------------
function TServerInterface.Fetch(cache: TDataObjectCache; out obj: TDataObject; sType: string; params: variant; iSessionID: integer; iTimeoutMS: integer = 300000): boolean;
//NOT INTERFACED
var
  //o: TDataObject;
  c: TDataObjectClass;
  sFetchQuery: string;
  sListOf: string;
begin

  CheckConnectedOrConnect;
  if not DOCF.LowLevelDOCreate(obj, cache, sType, params, 0,0,0) then
    raise ECritical.create('Failed to create '+sType);
  if obj = nil then
    raise ECritical.create('object is nil!');
  sfetchQuery := obj.FetchQuery;

  debug.log('Fetch '+obj.Name+' with query '+sFetchQuery);

  if obj.IsList then begin
    sListOF := obj.ListOf;
    result := QueryMap(cache, obj, sFetchQuery, iSessionid, 0, false, sType, params, nil,sListOf, DOCF.GetKeycountForClass(DOCF.GetClassTypeForClassName(sListOf)));
  end else
    result := QueryMap(cache, obj, sFetchQuery, iSessionid, 0, false, sType, params, nil);



end;
//------------------------------------------------------------------------------
function TServerInterface.GetNextID(sType: string): int64;
begin
  result := cli.GetNextID(sType)
end;

function TServerInterface.GetPoolTime: ticker;
begin
  result := FPoolTime;
end;

//------------------------------------------------------------------------------
function TServerInterface.New(cache: TDataObjectCache; out obj: TDataObject; sType: string;
  params: variant; iSessionID: integer): boolean;
begin
  CheckConnectedOrConnect;

  result := true;
  docf.LowLevelDOCreate(obj, cache, sType, params, 0,0,0);
  obj.New(0);
  if obj.IdentityKeyCount > 0 then begin
    obj.Token.Params[0] := GetNextID(obj.AutoKeyNames[0]);
  end;




end;
//------------------------------------------------------------------------------
function TServerInterface.LazyFetch(cache: TDataObjectCache; out obj: TDataObject;
  iType:integer; params: variant; iSessionID: integer; iTimeoutMS: integer = 300000): boolean;
begin


  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;
//------------------------------------------------------------------------------
function TServerInterface.LazyFetch(cache: TDataObjectCache; out obj: TDataObject; sType: string;
  params: variant; iSessionID: integer; iTimeoutMS: integer = 300000): boolean;
begin
  obj := cache.GetExistingObject(sType, params ,0,0);
  if obj <> nil then
    exit(true);

  result := Fetch(cache, obj, sType, params, iSessionid, iTimeoutMS);
end;

//------------------------------------------------------------------------------
//Overloaded: Ghost#2->Ghost#1
function TServerInterface.Ghost(cache: TDataObjectCache; out obj: TDataObject; sType: string;
  params: variant; iSessionID: integer): boolean;
begin

  try
    DOCF.LowLevelDOCreate(obj, cache, sType, params, DataCenter, Datatier, isessionid);
    obj.Ghost;
    result := true;
  except
    result := false;
  end;

(*  SetThreadSessionID(iSessionID);

  //Setup the statistics

  //pass a TDataObject to the overloaded NEW function
  result := Ghost(TDataObjectCache(cache.RealObject), doTemp, sType, params, iSessionID);

  //assign the TDataObject to the TDataObject interface OUT
  obj := doTemp;*)

end;


//------------------------------------------------------------------------------
function TServerInterface.Fetch(cache: TDataObjectCache;
  out obj: TDataObject; token: TDataObjectToken;
  iSessionID: integer; iTimeoutMS: integer = 300000): boolean;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TServerInterface.FireForgetQuery(cache: TDataObjectCache;
  sQuery: string; iSessionID: integer; bSerialize: boolean = false): boolean;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;





//------------------------------------------------------------------------------
function TServerInterface.GhostFetch(cache: TDataObjectCache;
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

function TServerInterface.GetLastErrorCode: integer;
begin
  result := FLastErrorCode;

end;

function TServerInterface.GetLastErrorMessage: string;
begin
  result := FLastError;
end;


function TServerInterface.GetDataCenter: integer;
begin
  result := 0

end;

function TServerInterface.GetDataTier: integer;
begin
  result := 0

end;



function TServerInterface.PingDB: boolean;
begin
  result := true;
end;

function TServerInterface.PingRDTP: boolean;
begin
  result := FunctionQuery('select 1', 0) = 1;


end;

function TServerInterface.Post(obj: TDataObject; iSessionID: integer;
  bIncludeSubFields: boolean): boolean;
var
  objOut: TDataobject;
begin
  result := self.post(obj, objOut, nil, iSessionId, bIncludeSubFields);
end;

function TServerInterface.PreFetchGroup(cache: TDataObjectCache; out obj: TDataObject;
  iType: integer; params: variant; iSessionID,
  iTimeoutMS: integer): boolean;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TServerInterface.PreFetchGroup(cache: TDataObjectCache;
  out obj: TDataObject; sType: string; params: variant; iSessionID,
  iTimeoutMS: integer): boolean;
begin



  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TServerInterface.GetAbortedTransactions: integer;
begin
    result := FAbortedTransactions;
end;

function TServerInterface.GetActiveTransactions: integer;
begin
  result := FActiveTransactions;

end;


function TServerInterface.GetRetryingTransactions: integer;
begin
  result := FRetryingTransactions;

end;

function TServerInterface.GetTimeOut: integer;
begin
  result := FTimeout;
end;

function TServerInterface.GetTotalTransactions: integer;
begin
  result := FTotalTransactions;
end;


procedure TServerInterface.SetAbortedTransactions(const Value: integer);
begin
  FAbortedTransactions := value;
end;

procedure TServerInterface.SetActiveTransactions(const Value: integer);
begin
  FActiveTransactions := value;
end;




procedure TServerInterface.SetLastError(iCode: integer; s: string);
begin
  SetLastError(s);
end;



function TServerInterface.SetNextID(sType: string; iID: int64): boolean;
begin
  cli.SetNextID(sType, iid);
  result := true;
end;

procedure TServerInterface.SetPoolTime(const Value: ticker);
begin
  FPoolTime := value;
end;

procedure TServerInterface.SetLastError(s: string);
begin
  FLastError := s;
end;


procedure TServerInterface.SetRetryingTransactions(const Value: integer);
begin
  FRetryingTransactions := value;

end;

procedure TServerInterface.SetTimeout(const Value: integer);
begin
  Ftimeout := value;
end;

procedure TServerInterface.SetTotalTransactions(const Value: integer);
begin
  FTotalTransactions := value;

end;

procedure TServerInterface.Time;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TServerInterface.DecAbortedTransactions;
begin
  dec(FAbortedTransactions);
end;

procedure TServerInterface.DecActiveTransactions;
begin
  dec(FActiveTransactions);
end;

procedure TServerInterface.DecRetryingTransactions;
begin
  dec(FRetryingTransactions);
end;

procedure TServerInterface.DecTotalTransactions;
begin
  dec(FTotalTransactions);
end;

procedure TServerInterface.Delete(cache: TdataObjectCache; obj: TDataObject);
begin
  if obj.filterphrase = '' then
    raise ECritical.create('trying to delete without filterphrase in '+obj.name);
  WriteQuery('delete from '+obj.TableLink+obj.FilterPhrase);

end;

procedure TServerInterface.IncAbortedTransactions;
begin
  Inc(FAbortedTransactions);
end;

procedure TServerInterface.IncActiveTransactions;
begin
  Inc(FActiveTransactions);
end;

procedure TServerInterface.IncRetryingTransactions;
begin
  Inc(FRetryingTransactions);
end;

procedure TServerInterface.IncTotalTransactions;
begin
  Inc(FTotalTransactions);
  if FTotalTransactions > 2100000000 then
    FTotalTransactions := 0;

end;



function TServerInterface.InitConnectionClass: TSimpleAbstractConnection;
begin
  result := TSimpleReliableUDPClient.create;
  result.HostName := FMWHost;
  result.EndPoint := FMWEndpoint;

end;

function TServerInterface.Query(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: string; iSessionID: integer;
  bExpectMany: boolean; slDebug: TStringList = nil; iTimeoutMS: integer =300000): boolean;
var
  rs: TSERowSet;
  osub: TDataObject;
  t: ni;
  fd: ni;
  f: PSERowsetFieldDef;
begin
  Connect;


  ReadQuery_Begin(sQuery);
  rs := ReadQuery_End();
  DOCF.LowLevelDOCreate(obj, cache, 'TdoQuery', sQuery, 0,0,0);
  for t:= 0 to rs.RowCount-1 do begin
    DOCF.LowLevelDOCreate(osub, cache, 'TdoQuery', VarArrayOf([sQuery, t]), 0,0,0);
    obj.AddObject(osub);
    for fd := 0 to rs.FieldCount-1 do begin

      f := rs.FieldDefs[fd];
      osub.AddFieldDef(f.sName, RSTypeToDOType(f.vType), '');

      osub.FieldByIndex[fd].AsVariant := rs.Values[fd, t];
    end;
  end;

  result := true;

end;




(*function TServerInterface.QueryMap(cache: TDataObjectCache;
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



function TServerInterface.RecordQuery(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: string; iSessionID: integer;
  bExpectMany: boolean; iTimeoutMS: integer): boolean;
begin
  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;



function TServerInterface.NoTransUpdateQuery(cache: TDataObjectCache;
  sQuery: string; iSessionID: integer): boolean;
begin
  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;


procedure TServerInterface.LogTransaction(sString: string);
begin
  if not FEnableTransactionLog then
    exit;

  FTransactionLog.add(sString);
end;

function TServerInterface.DrainTransactionLog: string;
begin
  result := Trim(FTransactionLog.text);

  FTransactionLog.Clear;
end;

procedure TServerInterface.ContinueConnection;
begin
//TODO -cunimplemented: unimplemented block
end;


function TServerInterface.LazyQuery(cache: TDataObjectCache;
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

function TServerInterface.LazyQueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
  iIgnoreKeys: integer; sBaseType: string; vBaseKeys: variant;
  slDebug: TStringList; sSubType: string; iSubKeys: integer): boolean;
var
  ikeyCount: integer;
begin
  obj := cache.GetExistingObject(sBaseType,vBaseKeys,0,0);

  result := not ((obj = nil) or obj.expired);

  if not result then begin
    result := QueryMap(cache, obj, sQuery, iSessionID, itimeOutMS, true, 'TdoQuery', vBaseKeys, nil, 'TdoQuery', iSubKeys);
  end else begin
    if obj.linkto = 'nil' then begin
      obj := nil;
      result := false;
    end;
  end;

end;

function TServerInterface.GhostQueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
  sBaseType: string; vBaseKeys: variant; slDebug: TStringList;
  sSubType: string; iSubKeys: integer): boolean;
begin
  result := LazyQueryMap(cache, obj, sQuery, iSessionID, iTimeoutMS, sBaseType, vBaseKeys, SLDebug, sSUbType, iSubkeys);
  if not result then begin
    result := Ghost(cache, obj, sBaseType, vBaseKeys, iSessionID);
  end;

end;






function TServerInterface.LazyQueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
        sBaseType: string; vBaseKeys: variant;
        slDebug: TStringList; sSubType: string; iSubKeys: integer): boolean;
begin
  result := self.LazyQueryMap(cache, obj, sQuery, iSessionId, iTimeOutMS, 0, sBaseType, vBaseKeys, slDebug, sSubType, iSubKeys);
end;

procedure TServerInterface.Rollback;
begin
//
end;

procedure TServerInterface.Commit;
begin
//
end;

function TServerInterface.GetReplayLogs(since: TDateTime): string;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;






function TServerInterface.GetStructureFromEngine(id:integer):string;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;



function RSTypeToDOType(rs: data.db.TFieldType): TDataFieldClass;
begin
  case rs of
    ftString : result := TstringDataField;

    ftSmallint,
    ftLongWord,
    ftWord,
    ftByte,
    ftInteger,
    ftLargeint
      : result := TLongintDataField;

    ftBoolean: result := TBooleanDataField;
    ftFloat: result := TFloatingPointDataField;





  end;

end;

function TServerInterface.QueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
  bLazy: boolean; iIgnoreKeys: integer; sBaseType: string;
  vBaseKeys: variant; slDebug: TStringList; sSubType: string;
  iSubKeys: integer): boolean;
var
  rs: TSERowSet;
  outdata: string;
  iType: ni;
  t: ni;
  objSub: TDataObject;
  c, cSub: TDataObjectClass;
  s: string;
begin

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

  //-------------------------
  rs := ReadQuery(sQuery);
  //-------------------------

  //if single record
  if sSubType = '' then begin
    //Single Object
    obj := UnwrapObjectfromRow(cache, rs, 0, c);
    result := obj <> nil;
  end else begin
    DOCF.LowLevelDOCreate(obj, cache, c, vBaseKeys, 0,0,0);
    for t:= 0 to rs.RowCount-1 do begin
      objSub := UnwrapObjectfromROW(cache, rs, t, csub);
      obj.AddObject(objSub);
    end;

    result := obj <> nil;
  end;


end;




function TServerInterface.QueryMap(cache: TDataObjectCache;
  out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
  bLazy: boolean; sBaseType: string; vBaseKeys: variant;
  slDebug: TStringList; sSubType: string; iSubKeys: integer): boolean;
begin
  result := self.QueryMap(cache, obj, sQuery, iSessionID, iTimeoutMS, bLazy, 0, sBaseType, vBaseKeys, slDebug, sSubType, iSubKeys);
end;


function UnwrapObjectfromRow(cache: TDataobjectCache; rs: TSEROWSet; row: ni; cType: TDataObjectClass): TDataObject;
var
  t: ni;
  iKeyCount: ni;
  v: variant;
begin
  //get the keys (this is the tough part)
  iKeyCount := DOCF.GetKeycountForclass(cType);
  v := VarArrayCreate([0,iKeyCount-1], varVariant);
  for t:= 0 to iKeyCount-1 do begin
     v[t] := rs.values[t,row];
  end;

  //instatiate the object with the keyss
  DOCF.LowLowLevelDOCreate(result, cache, cType, v, 0,0,0);
  //unwrap the fields
  for t:= 0 to result.FieldCount-1 do begin
    result.FieldByIndex[t].AsVariant := rs.Values[t+iKeyCount,row];
  end;



  result.IsChanged := false;

end;

end.


