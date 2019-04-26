unit RDTPProcessor;

interface
{$INLINE AUTO}
{x$DEFINE HIT_SESSION}
{xLIMIT_PROGRESS} //puts a limit on how often progress packets are sent
uses
  helpers_winsock,
  numbers, simpleabstractconnection, packet, systemx, typex, sysutils, windows, classes, sharedobject, SQLExpr, stringx, AbstractRDTPDataModule, DtNetConst, simplewinsock, exceptions, commandprocessor,orderlyinit, helpers.list, storageenginetypes;
{x$DEFINE ALLOW_WRITEBEHIND}//<<---currently has problems when connection is terminated

const
  SUPPORTED_OPTIONS = PACKET_PLATFORM_OPTION_SUPPORTS_COMPRESSION;
type
  TRDTPProcessor = class;//forward
  ENonFatal = class(Exception);


  Tcmd_WriteBehind = class;//forward

  TRDTPFunction = procedure(proc: TRDTPProcessor);
  TRDTPIdleEvent = procedure of object;

  TRDTPFunctionLink = class
  public
    RQID: integer;
    Func: TRDTPFunction;
    ServiceName: string;
  end;

  TRDTPFunctions = class(TSharedObject)
  private
    FFuncs: TList;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    procedure DefineFunc(sService: string; iRequest: integer;  func: TRDTPFunction);
    function GetFunction(sServiceName: string; iRequest: integer): TRDTPFunction;
  end;



  TRDTPProcessor = class(TmonitorableObject)
  private
    FCallbackTimeout: integer;
    FResourceList: integer;
    FLocalResources,FResources: TList;
    FServiceName: string;
    FParentContext: string;
    Fstatus: string;
    FAllowBlankContexts: boolean;
    cmdWrite: Tcmd_WriteBehind;
    FOnidle: TRDTPIdleEvent;
    FplatformOptions: cardinal;
    function GetCommandQueue(idx: integer): TRDTPPacket;
    function GetCommandsInQueue: integer;

    procedure SetContext(const Value: string);
    procedure SetResources(const Value: TList);
    procedure SetParentContext(const Value: string);
    function GetParentContext: string;
    function GetClientParentContext: string;
    function GetClientConfig: string;
    function GetClientParentConfig: string;
    procedure WriteBehind(packet: TRDTPPacket);
    procedure WaitForPendingWrite;
    function GetSessionID: int64;
    procedure SetSessionID(const Value: int64);
    function GetStatusX: string;
    function GetSupportedPlatformOptions: cardinal;

  protected
    FContext: string;
    FSocket: TSimpleAbstractConnection;
    FData: TAbstractRDTPDataModule;
    FForgetResult: boolean;
    FCommandQueue: TList;
    FLastProgress: cardinal;
    FSessionID: int64;
    bParentContextFetched: boolean;

    function GetData: TAbstractRDTPDataModule;virtual;
    function callback(var Packet: TRDTPPacket; iTimeOutMS: integer = 300000; slDebug: TStringList = nil; bForget: boolean = false): boolean;
    procedure DoIdle;virtual;
    procedure SetPlatformOptions(const Value: cardinal);virtual;
  public
    Request: TRDTPPAcket;
    Response: TRDTPPacket;

    constructor Create; override;
    destructor Destroy; override;
    property AllowBlankContexts: boolean read FAllowBlankContexts write FAllowBlankContexts;
    procedure ProcessMultiple;
    procedure ProcessSingle;


    property Socket: TSimpleAbstractConnection read FSocket write FSocket;
    function Read(bNotFromQueue: boolean = false): TRDTPPAcket;

    function ReadMore: boolean;
    procedure Write(packet: TRDTPPacket);
    property Resources: TList read FResources write SetResources;



    property Data: TAbstractRDTPDataModule read GetData;

    procedure FreeAndNilPacket(var packet: TRDTPPacket);
    procedure ExpireSessions;

    function MasterDispatch: boolean;virtual; //dispatches using multiple methods including FunctionFactory
    function Dispatch: boolean;reintroduce;virtual;       //dispatches using simple case statement
    property ForgetResult: boolean read FForgetResult write FForgetResult;
    procedure SendHeartBeat();
    procedure SendProgress(sMessage: string; iPos, iMax: integer; sDebugLog: string = '');
    procedure OnConnectionProgress(sMessage, sDebug: string; iPos, iMax: integer);
    property CallbackTimeout: integer read FCallbackTimeout write FCallbackTimeout;

    property CommandQueue[idx: integer]: TRDTPPacket read GetCommandQueue;
    property CommandsInQueue: integer read GetCommandsInQueue;
    function GetFromCommandQueue: TRDTPPacket;
    procedure AddToCommandQueue(p: TRDTPPacket);
    property Context: string read FContext write SetContext;
    property ParentContext: string read GetParentContext write SetParentContext;
    procedure CheckContextSet;
    procedure CheckGetContext;
    function GetClientContext: string;
    property ServiceName: string read FServiceName write FServiceName;
    property SessionID: int64 read GetSessionID write SetSessionID;
    property OnIdle: TRDTPIdleEvent read FOnidle write FOnIdle;
    property StatusX: string read GetStatusX;
    property SupportedPLatformOptions: cardinal read GetSupportedPlatformOptions;
    property PlatformOptions: cardinal read FplatformOptions write SetPlatformOptions;
    function NeedPacket: TRDTPPacket;
  end;

  TRDTPProcessorClass = class of TRDTPProcessor;


  Tcmd_WriteBehind = class(TCommand)
  private
    FProcessor: TRDTPProcessor;
    FPacket: TRDTPPacket;
    procedure SetPacket(const Value: TRDTPPacket);
    procedure SetProcessor(const Value: TRDTPProcessor);
  public
    property Processor: TRDTPProcessor read FProcessor write SetProcessor;
    property Packet: TRDTPPacket read FPacket write SetPacket;
    procedure DoExecute;override;
  end;





var
  RDTPFF: TRDTPFunctions;

implementation

uses AppLock, debug;

{ TRDTPProcessor }

procedure TRDTPProcessor.AddToCommandQueue(p: TRDTPPacket);
begin
  self.FCommandQueue.add(p);
end;

procedure TRDTPProcessor.CheckContextSet;
begin
  if FContext = '' then begin

    FContext := self.GetClientContext;
    if fcontext = '' then begin
      raise EClassException.create('Context not set, could not be read... in '+self.classname);
    end;
  end;
end;

procedure TRDTPProcessor.CheckGetContext;
begin
  if FContext = '' then
    Fcontext := GetClientContext;
end;

function TRDTPProcessor.GetClientContext: string;
var
  packet: TRDTPPacket;
begin
  packet := TRDTPPacket.create;
  try
    packet.AddVariant($9000);
    packet.AddVariant(0);
    packet.AddString(ServiceName);

    if not callback(packet) then raise exception.create('callback failure');
    if not packet.result then raise exception.create('server error:'+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    result := packet.seqread;
  finally
    packet.free;
  end;
end;

function TRDTPProcessor.GetClientParentContext: string;
var
  packet: TRDTPPacket;
begin
  packet := TRDTPPacket.create;
  try
    packet.AddVariant($9001);
    packet.AddVariant(0);
    packet.AddString(ServiceName);

    if not callback(packet) then raise exception.create('callback failure');
    if not packet.result then raise exception.create('server error:'+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    result := packet.seqread;
  finally
    packet.free;
  end;
end;

function TRDTPProcessor.GetClientConfig: string;
var
  packet: TRDTPPacket;
begin
  packet := TRDTPPacket.create;
  try
    packet.AddVariant($9002);
    packet.AddVariant(0);
    packet.AddString(ServiceName);

    if not callback(packet) then raise exception.create('transaction failure');
    if not packet.result then raise exception.create('server error:'+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    result := packet.seqread;
  finally
    packet.free;
  end;
end;

function TRDTPProcessor.GetClientParentConfig: string;
var
  packet: TRDTPPacket;
begin
  packet := TRDTPPacket.create;
  try
    packet.AddVariant($9003);
    packet.AddVariant(0);
    packet.AddString(ServiceName);

    if not callback(packet) then raise exception.create('transaction failure');
    if not packet.result then raise exception.create('server error:'+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    result := packet.seqread;
  finally
    packet.free;
  end;
end;




constructor TRDTPProcessor.Create;
begin
  inherited;
//  SayNatural('create', true);
  FCallbackTimeout := 300000;
  FCommandQueue := Tlist.create;
  FLocalResources := TList.create;
  FResources := FLocalResources;
  RegisterTo(MasterMonitor);
end;

constructor TRDTPFunctions.Create;
begin
  inherited;
  FFuncs := TList.create;

end;

procedure TRDTPFunctions.DefineFunc(sService: string; iRequest: integer;
  func: TRDTPFunction);
var
  FL: TRDTPFunctionLink;
begin
  FL := TRDTPFunctionLink.create;
  FL.RQID := iRequest;
  FL.Func := func;
  FL.ServiceName := sService;
  FFuncs.add(FL);

end;

destructor TRDTPProcessor.Destroy;
begin
  WaitforPendingWrite;
//  Applock.AL.Lock;
//  try
//    SayNatural('destroy',true);

//  if assigned(self.FSocket) then begin
//    FSocket.free;
//    FSocket := nil;
//  end;
  FreeListContents(FCommandQueue);
  FCommandQueue.free;
  FreeAndNilPacket(request);
  FreeAndNilPacket(response);
  datapool.NoNeedData(Fdata);
  FreeListContents(FLocalResources);
  FLocalResources.free;
//    FData.free;
//  finally
//    Applock.AL.Unlock;
//  end;
  inherited;
end;


function TRDTPProcessor.Dispatch: boolean;
var
  iRQ: integer;
begin
//  status := 'In Dispatch';
  iRQ := request.data[0];
  if iRQ = $9000 then  result := true;
//  status := 'In Dispatch:$'+inttohex(iRQ,4);
  //default handling of SetContext
  if iRQ = $9000 then begin
    request.SeqSeek(3);
    Context := request.seqread;

    ForgetResult := true;
    result := true;
  end else
  if iRQ = $9999 then begin
    request.SeqSeek(3);
    if request.DataCount > 4 then begin
      self.PlatformOptions := request.data[4];
    end;
    Context := request.seqread;
    Response.AddLong(self.PlatformOptions);

    //ForgetResult := true;
    result := true;
  end else
  begin
    result := false;
  end;
end;

procedure TRDTPProcessor.DoIdle;
begin
  SendHeartBeat();
  if assigned(FOnIdle) then
    FOnIdle();

end;

procedure TRDTPProcessor.ExpireSessions;
var
  temp: TAbstractRDTPDataModule;
  sl: TstringList;
  ds: TSERowSet;
  sTable, sLeft, sRight: string;
  iAffected: integer;
begin
  sl := TStringlist.create;
  try
    temp := self.Data;
    iAffected := self.Data.ExecuteSystem('delete from session where lasthit<date_sub(now(), interval 1 hour)');
    if iAffected > 0 then begin
      self.Data.ExecuteSystem('select sessionid from session', ds);
      if ds <> nil then
      try
        ds.first;
        while not ds.eof do begin
          sl.Add(ds.CurRecordFieldsByIdx[0]);
          ds.next;
        end;
      finally
        ds.free;
      end;

      self.Data.ExecuteSystem('show tables', ds);
      try
        if ds <> nil then
          ds.first;

        if ds <> nil then
        while not ds.eof do begin
          sTable := ds.CurRecordFieldsByIdx[0];
          if SplitStringNoCase(sTable, 'Temp', sLeft, sRight) then begin
            if SplitString(sRight, '_', sLeft, sRight) then begin
              if sl.IndexOf(sLeft) < 0 then begin
                try
                  self.Data.ExecuteSystem('drop table '+sTable);
                except
                end;
              end;
            end;
          end;


          ds.next;
        end;
      finally
        ds.free;
      end;
    end;

  finally
    sl.Free;
  end;
end;

procedure TRDTPProcessor.FreeAndNilPacket(var packet: TRDTPPacket);
begin
  if packet <> nil then
    packet.free;

  packet := nil;
end;


function TRDTPProcessor.GetCommandQueue(idx: integer): TRDTPPacket;
begin
  result := self.FCommandQueue[idx];
end;

function TRDTPProcessor.GetCommandsInQueue: integer;
begin
  result := FCommandQueue.count;
end;

function TRDTPProcessor.GetData: TAbstractRDTPDataModule;
begin
  if FData = nil then begin
//    Applock.AL.Lock;
    try
      FDATA := datapool.NeedData(self.classtype, FContext);
      FDATA.OnConnectionProgress := self.OnConnectionProgress;
//      FDATA.Connect;
      //FDATA.BeginTransaction;
    finally
//      AppLock.AL.Unlock;
    end;
  end;
  result := FData;

end;

function TRDTPProcessor.GetFromCommandQueue: TRDTPPacket;
begin
  result := nil;
  if CommandsInQueue = 0 then
    exit;

  result := self.CommandQueue[0];
  self.FcommandQueue.delete(0);

end;

function TRDTPProcessor.GetParentContext: string;
begin
  if not bParentContextFetched then begin
      FParentContext := GetClientParentContext;
      bParentContextFetched := true;
  end;
  result := FParentContext;
end;

function TRDTPProcessor.GetSessionID: int64;
begin
  Lock;
  try
    result := FSessionID;
  finally
    Unlock;
  end;
end;

function TRDTPProcessor.GetStatusX: string;
begin
  lock;
  try
    result := Fstatus;
  finally
    unlock;
  end;
end;

function TRDTPProcessor.GetSupportedPlatformOptions: cardinal;
begin
  result := SUPPORTED_OPTIONS;
end;

function TRDTPProcessor.MasterDispatch: boolean;
var
  F: TRDTPFunction;
  iRequest: integer;
begin
  result := false;
    ForgetResult := false;
    request.SeqSeek(0);
    iRequest := Request.SeqRead;
    ServiceName := request.data[2];
    request.SeqSeek(1);
//    status := 'In MAsterDispatch:$'+inttohex(iREquest,4);
//    Debug.Log(status);
    try
      result := Dispatch;
      if not result then begin
        result := true;
        ForgetResult := iRequest = $0992;
        F:= RDTPFF.GetFunction(ServiceName, iRequest);


        if not assigned(F) then
          raise Exception.create('F is nil');

        F(self);
        result := true;

      end else begin
        result := true;
      end;
    except
      on E: Exception do begin
        Debug.Log(self,'Exception caught in '+self.ClassName+'.MasterDispatch: '+e.Message);
        Debug.ConsoleLog('Exception caught in '+self.ClassName+'.MasterDispatch: '+e.Message);
        response.ReturnError(e.Message);
        result := true; //gotta set TRUE here because even if Dispatch returns true, if an
                        // exception is thrown, it will not be assigned to result...
                        //the assignment of the result happens only after return... but the exception prevents
                        // the ASM instructions from being called after return.
      end;
    end;

end;

function TRDTPProcessor.NeedPacket: TRDTPPacket;
begin
  result := TRDTPPacket.create;
  result.PlatformOptions := self.PlatformOptions;
//  Debug.ConsoleLog(self.classname+' creates packet with options '+inttostr(result.PlatformOptions));
end;

procedure TRDTPProcessor.OnConnectionProgress(sMessage, sDebug: string; iPos,
  iMax: integer);
begin
  self.SendProgress(sMessage,iPos, iMax, sDebug);
end;



procedure TRDTPProcessor.ProcessMultiple;
var
  iRequest: integer;
  iSessionID: integer;
  bfirst: boolean;
  temp: TAbstractRDTPDataModule;

begin
  bFirst := true;
  //AL.Lock;
  try
    try
      Debug.Log(self,'beginning RDTP processing','RDTPPLUMBING');
      repeat
        ProcessSingle;
      until not ReadMore;

    except
  //    if FData <> nil then
  //      Data.Rollback;
      on E: Exception do begin
        Debug.Log(self,'********'+e.classname+':'+E.Message);
      end;
    end;
  finally
//    AL.Unlock;
    Debug.Log(self,'ending RDTP processing','RDTPPLUMBING');
  end;

end;

procedure TRDTPProcessor.ProcessSingle;
var
  iRequest: integer;
  iSessionID: integer;
  temp: TAbstractRDTPDataModule;

begin
{$IFDEF RDTP_LOGGING}  Debug.Log(self,'beginning RDTP processing - Process Single','RDTPPLUMBING');{$ENDIF}
  ForgetResult := false;
  //read request
  request := read;

  if assigned(request) and ((request.bufferlength = 0) or (request.PackedLength = 0)) then begin
    request.free;
    exit;
  end;

  if request = nil then begin
    if FData <> nil then begin
      try
//            Data.Commit;
      except
      end;
    end;
    exit;

  end;

  request.SeqSeek(0);
  iRequest := Request.SeqRead;
  request.SeqSeek(1);
  //setup response
  response := NeedPacket;
  response.Origin := poServer;
  response.AddShort(request.data[0]);//echo requestid
  iSessionID := request.data[1];
  response.AddLong(iSessionID);//echo sessionid

  response.AddBoolean(true);//result
  response.AddShort(0);//error code
  response.AddString('');//error message

{$IFDEF HIT_SESSION}
  if bFirst and (iSessionID > 0) then begin
    self.data.ExecuteSystem('UPDATE session set lasthit=now() where sessionid='+inttostr(isessionid));
    ExpireSessions;
    bFirst := false;
  end;
{$ENDIF}



  //find and call function
  try

{$IFDEF RDTP_LOGGING}    Debug.Log(self,'Dispatching request:'+inttohex(iRequest,4),'RDTPPLUMBING');{$ENDIF}
    MasterDispatch;

{$IFDEF RDTP_LOGGING}    Debug.Log(self,'Request dispatched:'+inttohex(iRequest,4),'RDTPPLUMBING');{$ENDIF}
//        AL.Lock; try


//        finally AL.Unlock; end;

  except
    on E: ENonFatal do begin
      response.Clear;
      response.AddShort(request.data[0]);//echo requestid
      response.AddLong(request.data[1]);//sessionid
      response.AddBoolean(false);//result
      response.AddShort(2);//result
      response.AddString(E.Message);//error message
      Debug.Log(self,'Non Fatal Exception: '+E.Message);
//          Write(response);
    end;
    on E: Exception do begin
      response.Clear;
      response.AddShort(request.data[0]);//echo requestid
      response.AddLong(request.data[1]);//sessionid
      response.AddBoolean(false);//result
      response.AddShort(2);//result
      response.AddString(E.Message);//error message
      Debug.Log(self,'Main Exception: '+E.Message);

      if (FData <> nil) then begin
//            try
          Data.Rollback;
          Fdata.free;
          FDAta := nil;
//            except
//            end;
      end;
    end;
  end;

  if not ForgetResult then begin
//      Debug.Log('Writing resposne');
{$IFDEF ALLOW_WRITEBEHIND}
    WriteBehind(response);
{$ELSE}
    write(Response);
{$ENDIF}
  end;
  FreeAndNilPacket(request);

{$IFNDEF ALLOW_WRITEBEHIND}
    FreeAndNilPacket(response);
{$ENDIF}





end;


function TRDTPProcessor.Read(bNotFromQueue: boolean = false): TRDTPPAcket;
var
  header: TRDTPHeader;
  pc: PByte;
  iRead: integer;
  iToRead: integer;
  iJustRead: integer;
  len: ni;
begin
  //if there's stuff in the command queue then take it
  lock;
  try
    if not bNotFromQueue then begin
      result := GetFromCommandQueue;

      if result <> nil then begin
{$IFDEF DEBUG_RDTP}        Debug.Log('Read packet from Queue: '+memorytohex(result.DecryptedBuffer.FRealBuffer, result.DecryptedBuffer.Length), 'RDTPPLUMBING');{$ENDIF}
        exit;
      end;
    end;


    result := nil;

    //idle loop
    while not FSocket.WaitForData(10000) do begin
      DoIDle;
    end;


    //read 18 bytes
    iRead := 0;
    while iRead < 18 do begin
      iToRead := 18-iRead;
      if not FSocket.WaitForData(10000) then begin
        result := nil;
        exit;
      end;



      iJustRead := FSocket.GuaranteeReadData(@(pbyte(@header)[iRead]), iToRead,8000);

{$IFDEF DEBUG_RDTP}      Debug.Log('justRead:'+inttostr(iToRead)+' '+ memorytohex(@pbyte(@header)[iRead], iJustRead));{$ENDIF}

      if iJustRead >0 then
        inc(iread, iJustREad)
      else begin
        result := nil;
        exit;
      end;

      if (not FSocket.connected) (*or (iJustRead = 0)*) then
        exit;
    end;

{$IFDEF DEBUG_RDTP}    Debug.Log('Got a header, it is:'+memorytohex(@header, 18),'RDTPPLUMBING');{$ENDIF}

    len := header.Packedlength+sizeof(header);
    if len < 18 then begin
      FSocket.Disconnect;
      raise ETransportError.create('packet was not at least 18 bytes! ('+inttostr(len)+')');
    end;
{$IFNDEF ENABLE_HUGE_MESSAGES}
    if len > 2*BILLION then begin
      raise ETransportError.create('packet length is unusually large! '+len.tostring+' bytes '+len.tohexstring);
    end;
{$ENDIF}
    PC := GetMemory(len);
    MoveMem32(@pc[0], @header, 18);

    iRead := 18;
{$IFDEF DEBUG_RDTP}    Debug.Log('Decoded header length:'+inttostr(header.length),'RDTPPLUMBING');{$ENDIF}

    while iRead < header.Packedlength do begin

      iToRead := LesserOf(header.Packedlength-iread, 8000);
      if not FSocket.WaitForData(25000) then
        exit;
      iJustRead := FSocket.ReadData(@pc[iRead], iToRead);
{$IFDEF DEBUG_RDTP}      Debug.Log('read: '+inttostr(iREad)+' justRead:'+inttostr(iJustRead)+' '+memorytohex(@pc[iRead], iJustRead),'RDTPPLUMBING');{$ENDIF}
      if iJustRead <= 0  then
        exit(nil);
      inc(iread, iJustREad);
//      Debug.Log('read: '+inttostr(iREad)+' justRead:'+inttostr(iToRead));


      if (not FSocket.connected) or (iJustRead = 0) then begin
        Debug.Log(self,'aborting read, socket not connected','RDTPPLUMBING');
        raise ETransportError.create('aborting socket read, socket not connected');
        FSocket.Disconnect;
        exit;
      end;
    end;


{$IFDEF DEBUG_RDTP}    Debug.Log('read complete, assigning buffer to packet: '+memorytohex(pc, header.length),'RDTPPLUMBING');{$ENDIF}
    result := TRDTPPAcket.create;
    result.EncryptedBuffer.AssignBuffer(pc, header.Packedlength);

{$IFDEF DEBUG_RDTP}    Debug.Log(self,'Read a packet that is '+inttostr(header.length)+' bytes','RDTPPLUMBING');{$ENDIF}
{$IFDEF DEBUG_RDTP}    Debug.Log(self,MemoryDebugString(pc, 18),'RDTPPLUMBING');{$ENDIF}
{$IFDEF DEBUG_RDTP}    Debug.Log(self,MemoryDebugString(pc+18, header.length-18),'RDTPPLUMBING');{$ENDIF}
{$IFDEF DEBUG_RDTP}    Debug.Log(self,'Data Count in packet:'+inttostr(result.datacount),'RDTPPLUMBING');{$ENDIF}

    if result.IsMarkerValid then begin
{$IFDEF DEBUG_RDTP}          Debug.Log(self,'MArker is valid','RDTPPLUMBING');{$ENDIF}
{$IFDEF DEBUG_RDTP}          Debug.Log(self,result.GetDebugMessage);{$ENDIF}
    end else
      Debug.Log(self,'MArker is **INVALID**','RDTPPLUMBING');
  finally
    Unlock;
  end;



//  Debug.Log(result.GetDebugMessage);


end;


function TRDTPProcessor.ReadMore: boolean;
var
  i, iBuf: integer;
  b: boolean;
begin
  //return TRUE if more data
  //return false if disconnected
  //wait if no information
//  status := 'In ReadMore';

  repeat
//    status := 'In ReadMore:waitfordata(20000)';
    b := FSocket.WaitForData(20000);
    if (not Fsocket.connected) then begin
      result := false;
      exit;
    end;

    if not b then begin
      SendHeartBeat();
      exit(false);
    end;
  until b;


{$IFDEF DO_PEEK}
//  status := 'In ReadMore:{DO_PEEK}';
  i := Fsocket.PeekBuf(iBuf, sizeof(integer));
  Debug.Log('Peekbuf shows '+inttostr(i)+' bytes in ReadMore','RDTPPLUMBING');
  Debug.Log('Peekbuf data is '+inttohex(iBuf, 8),'RDTPPLUMBING');

  if (i = 0) then begin
    Debug.Log('Socket was elegantly disconnected');
  end;

  result := (i>0);// or FSocket.connected;
{$ENDIF}
  result := true;

end;


procedure TRDTPProcessor.SendHeartBeat;
begin
  //SendProgress('',-1,-1,'');
end;

procedure TRDTPProcessor.SendProgress(sMessage: string; iPos, iMax: integer;
  sDebugLog: string);
var
  prog: TRDTPPacket;
  tmNow: cardinal;
begin
{$IFDEF LIMIT_PROGRESS}
  tmNow := GetTicker;
  if (FLastProgress > 0) and (iPOs > -1) then begin
    if GetTimeSince(tmNow, FLastProgress) < 100 then
      exit;

  end;
  FLastProgress := tmNow;
{$ENDIF}

  prog := TRDTPPacket.create;
  try
    prog.Origin := poServer;
    prog.PacketType := PACKET_TYPE_PROGRESS;
    prog.AddVariant(sMessage);
    prog.AddVariant(sDebugLog);
    prog.AddVariant(iPos);
    prog.AddVariant(iMax);
    Write(prog);
  finally
    prog.free;
  end;

end;



procedure TRDTPProcessor.SetContext(const Value: string);
begin
(*  if not AllowBlankContexts then begin
    if value = '' then
      raise exception.create('cannot set a blank context in '+self.ClassName);
  end;*)
  debug.consolelog('rdtpprocessor.setcontext='+value);
  FContext := Value;
  //self.Data;
end;

procedure TRDTPProcessor.SetParentContext(const Value: string);
begin
  FParentContext := Value;
end;

procedure TRDTPProcessor.SetPlatformOptions(const Value: cardinal);
begin
  FplatformOptions := Value and SupportedPLatformOptions;

end;

procedure TRDTPProcessor.SetResources(const Value: TList);
begin
  FResources := Value;
end;

procedure TRDTPProcessor.SetSessionID(const Value: int64);
begin
  Lock;
  try
    FSessionID := value;
  finally
    Unlock;
  end;
end;

function TRDTPProcessor.callback(var Packet: TRDTPPacket; iTimeOutMS: integer;
  slDebug: TStringList; bForget: boolean): boolean;
begin
  WaitForPendingWrite;

  //send the packet
  packet.PacketType := PACKET_TYPE_CALLBACK;
  self.Write(packet);
  packet.Free;
  packet := nil;
  if bForget then begin
    result := true;
    exit;
  end;
  //packet := TRDTPPacket.create;
  repeat
    packet := self.Read(true);
    //if another command comes in while we're waiting for a callback response
    //queue the commands until we get our callback response
    if not packet.IsMarkerValid then begin
      raise EPacketError.create('Callback packet marker is invalid');
    end;
    if not packet.IsCRCValid then begin
      raise EPacketError.create('Callback Response CRC is invalid Data:'+packet.BufferAsString);
    end;


    if not (packet.PacketType = PACKET_TYPE_CALLBACK_RESPONSE) then
      self.AddToCommandQueue(packet);

  until packet.PacketType = PACKET_TYPE_CALLBACK_RESPONSE;

  debug.consolelog('Found callback response:'+packet.GetDebugMessage);



  packet.Origin := poServer;
  result := true;

end;

procedure TRDTPProcessor.WriteBehind(packet: TRDTPPacket);
begin
  waitforpendingwrite;


  cmdWrite := Tcmd_writeBehind.create;
  cmdWrite.Processor := self;
  cmdwrite.packet := packet;
  cmdWrite.Start;
end;

procedure TRDTPProcessor.WaitForPendingWrite;
begin
  if cmdWrite <> nil then begin
    cmdWrite.WaitFor;
    cmdWrite.free;
    cmdWrite := nil;
  end;

end;

procedure TRDTPProcessor.Write(packet: TRDTPPacket);
var
  iFail, iTotal,iSent: integer;
  pc: PByte;
begin
//  status := 'In Write(packet)';
{$IFDEF DEBUG_RDTP_PLUMBING}  Debug.Log(self,'About to WRITE a packet that is '+inttostr(packet.length)+' bytes','RDTPPLUMBING');{$ENDIF}
  pc := packet.decryptedbuffer.FRealBuffer;
{$IFDEF DEBUG_RDTP_PLUMBING}   Debug.Log(self,MemoryToHex(pc, 18), 'RDTPPLUMBING');{$ENDIF}
{$IFDEF DEBUG_RDTP_PLUMBING}   Debug.Log(self,MemoryToHex(pc+18, packet.length-18), 'RDTPPLUMBING');{$ENDIF}

  if packet.IsCRCValid then begin
    if packet.IsMarkerValid then begin
{$IFDEF RDTP_LOGGING}      Debug.Log(self,packet.GetDebugMessage,'RDTPPLUMBING');{$ENDIF}
    end else
      Debug.Log(self,'MArker is **INVALID**','RDTPPLUMBING');
  end else begin
      Debug.Log(self,'PACKET CRC IS INVALID','RDTPPLUMBING');
  end;

  iTotal := 0;
  iFail := 0;
  packet.Encrypt;
  while iTotal < packet.PAckedLength  do begin
    lock;
    try
      iSent := FSocket.SendData(@packet.EncryptedBuffer.RawBuffer[iTotal], packet.PackedLength-iTotal);
    finally
      Unlock;
    end;
    if iSent <= 0 then begin
      inc(iFail);
      if iFail > 500 then
        raise ETransportError.create('exception on send');
    end else begin
      inc(itotal, iSent);
    end;
  end;

end;

destructor TRDTPFunctions.Destroy;
begin
  FFuncs.free;
  inherited;
end;



function TRDTPFunctions.GetFunction(sServiceName: string; iRequest: integer): TRDTPFunction;
var
  func: TRDTPFunctionLInk;
  t: integer;
begin
  Lock;
  try
    result := nil;

    for t:= 0 to FFuncs.count-1 do begin
      func := TRDTPFunctionLink(FFuncs[t]);
      if (func.RQID = iRequest) and (lowercase(sServiceName)=lowercase(func.ServiceName)) then begin
        result := func.Func;
        exit;
      end;
    end;

    if not Assigned(result) then
      raise Exception.create(inttohex(iRequest,4)+' not implemented for '+sServiceName);
  finally
    Unlock;
  end;

end;

{ Tcmd_WriteBehind }

procedure Tcmd_WriteBehind.DoExecute;
begin
  inherited;
  processor.Write(packet);
  packet.free;
  packet := nil;
end;

procedure Tcmd_WriteBehind.SetPacket(const Value: TRDTPPacket);
begin
  FPacket := Value;
end;

procedure Tcmd_WriteBehind.SetProcessor(const Value: TRDTPProcessor);
begin
  FProcessor := Value;
end;

procedure oinit;
begin
  RDTPFF := TRDTPFunctions.create;
end;

procedure ofinal;
begin
  RDTPFF.free;

end;



initialization
  init.RegisterProcs('RDTPProcessor', oinit, ofinal);


finalization


end.

