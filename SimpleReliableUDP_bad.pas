unit SimpleReliableUDP;
//{$Message Error 'I forgot... definition of LOOP changed.  I still need to be able to handle "one shot" threads.... the definition of LOOP is going to be all squirley... fuck!'}
//todo 1: Since ChangeConnID is sent without requiring ACK, who is to say if it isn't lost?  Special case, retrans?
//todo 1: OnData... separate out immediate queue from regular queue.

//PROVE
//[ ] No memory leaked
//[ ] no memory double frees
//[ ] Threadsafe [ ] Called From Single Thread [ ] Locks employed
{x$DEFINE SHORT_KEEP_ALIVE}



interface

uses
  windows,
  signals, commands_system, system.syncobjs, numbers, systemx, debug, classes, typex, persistentinterfacedobject, standardlist, netbytes, sysutils, tickcount, betterobject, generics.collections.fixed, SimpleAbstractConnection, managedthread, sharedobject, idglobal, idudpserver, idsockethandle, orderlyinit, stringx, better_indy, idwinsock2;


const
  MAX_OUTSTANDING_TX_PACKETS = 0;
  PAYLOAD_LIMIT = 4000;
  RUDP_SYS_CMD_CREATE = 1;
  RUDP_SYS_CMD_CONFIRM_BIDIR = 2;
  CMD_ACK = 3;
  CMD_KEEP_ALIVE = 5;
  RUDP_CMD_CLOSE = 4;
  RUDP_CMD_DEBUGINFO = 6;
  RUDP_CMD_REQUEST_RETRANS = 7;
  ERR_CHANGE_CONNID = $81;
{$IFDEF SHORT_KEEP_ALIVE}
  CONNECTION_TIMEOUT = 8000;
{$ELSE}
  CONNECTION_TIMEOUT = 80000;
{$ENDIF}


type
  TCloseOrigin = (coNotClosed, coClient, coServer, coDestructorClient, cdDestructorServer);
  TCloseState = (csNotClosed, csWaitingForPackets, csTimedOut, csClosed);
  TConnectionType = (ctClient, ctServer);
  TRUDPDebugInfo = packed record
    threadid: int64;
    module: array[0..511] of ansichar;
    procedure Init;
    function ToString: string;
  end;

  EReliableUDPProtocolError = class (Exception);
  ECritical = class(Exception);

  TReliableUDPEndpoint = class;//forward;

  TRUDPEndpointThread = class(TManagedThread)
  private
    FEndpoint: TReliableUDPEndpoint;
    procedure SetEndpoint(const Value: TReliableUDPEndpoint);
  public
    procedure Detach;override;
    procedure PrepareForPool;override;
    property Endpoint: TReliableUDPEndpoint read FEndpoint write SetEndpoint;
  end;

  TRUDPSystemThread = class(TRUDPEndpointThread)
  public
    procedure DoExecute;override;
  end;

  TRUDPUserThread = class(TRUDPEndpointThread)
  public
    procedure DoExecute;override;
  end;

  TAckType = (ackNone, ackOutofOrder, ackOnTimeout, ackImmediate);

  TMultiplexedUDPEndpoint = class;//forward




  //--------------------------------------------------------------------------
  TReliableUDPHeader = packed record
  private
    function GetFlagCreate: boolean;
    procedure SetFlagCReate(const Value: boolean);
    function GetFlagAck: TAckType;
    function GetFlagCommand: boolean;
    function GetFlagDestroy: boolean;
    procedure SetFlagACk(const Value: TAckType);
    procedure SetFlagCommand(const Value: boolean);
    procedure SetFlagDestroy(const Value: boolean);
    function GetFlagSystem: boolean;
    procedure SetFlagSystem(const Value: boolean);
    function GetFlagCommandResponse: boolean;
    function GetFlagRetrans: boolean;
    procedure SetFlagCommandResponse(const Value: boolean);
    procedure SetFlagRetrans(const Value: boolean);
    function GetFlagCommandError: boolean;
    procedure SetFlagCommandError(const Value: boolean);
  public
    tag: array[0..1] of byte;
    flags: byte;
    command: byte;
    connectionid: int64;
    sequencenumber: int64;
    expected_sequencenumber: int64;
    procedure Init(ep: TReliableUDPEndpoint);
    property Flag_Create: boolean read GetFlagCreate write SetFlagCReate;//
    property Flag_CLOSE: boolean read GetFlagDestroy write SetFlagDestroy;//
    property Flag_AckType: TAckType read GetFlagAck write SetFlagACk;//
    property Flag_Command: boolean read GetFlagCommand write SetFlagCommand;//
    property Flag_CommandResponse: boolean read GetFlagCommandResponse write SetFlagCommandResponse;//
    property Flag_Retrans: boolean read GetFlagRetrans write SetFlagRetrans;//
    property Flag_System: boolean read GetFlagSystem write SetFlagSystem;//
    function IncomingData: pbyte;//
    procedure PrepareToSend(ep: TReliableUDPEndpoint);
    property Flag_Command_Error:boolean read GetFlagCommandError write SetFlagCommandError;
    function DebugString: string;
    function IsDataPacket: boolean;
  end;

  //--------------------------------------------------------------------------


  //--------------------------------------------------------------------------
  TReliableUDPPacketLogRecord = class(TSharedObject, IIndexable<int64>)
  strict private
    owningEndPoint: TreliableUDPEndpoint;
  public
    RemoteOrigin: boolean;
    OriginatingHost: string;
    OriginatingPort: nativeint;
    sendtime: ticker;
    acked: boolean;
    queue_handled: boolean;
    h: TReliableUDPHeader;//takes ownership of
    randomNagleTimer: ticker;
    payload: PByte;
    payload_length: nativeint;
    payload_read_index: nativeint;
    constructor Create(owningEndPOint: TReliableUDPEndpoint);reintroduce;overload;virtual;
    constructor Create(owningEndPOint: TReliableUDPEndpoint; h: TReliableUDPHeader; p: pbyte; len: nativeint);reintroduce;overload;virtual;
    destructor Destroy;override;
    procedure Detach;override;
    function _AddRef: Integer; virtual;stdcall;
    function _Release: Integer;virtual; stdcall;


    function GetIndexValue: int64;
    property IndexValue: int64 read GetIndexValue;
    function NeedsAck: boolean;
    function DebugString: string;
    procedure DeregisterFromEndpoint;

  end;


  //--------------------------------------------------------------------------
  TReliableUDPPacketLog = class(TStandardList<int64, TReliableUDPPacketLogRecord>)
  //keeps track of packets
  public
    constructor Create;override;
    destructor Destroy;override;
  end;
  //--------------------------------------------------------------------------
  TReliableUDPEndpoint = class(TPersistentInterfacedObject,IIndexable<int64>)
  private
    FCloseorigin: TCloseOrigin;
    FCloseState: TCloseState;
    FRemotePOrt: nativeint;
    FRemoteHost: string;
    FLocalPort: nativeint;
    FMultiplexer: TMultiplexedUDPEndpoint;
    FUserThreadSignal: boolean;
    FSYstemThreadSignal: boolean;
    FKeepAliveInterval: nativeint;
    FKeepAliveTimeout: nativeint;
    FKeepAliveStagger: nativeint;
    FDestroyOnClose: boolean;
    bSaidHello: boolean;
    SynchronousDestroy: boolean;
    FevUserDataIncoming: TSignal;
    function GetIndexValue: int64;
    procedure QueueIncomingPacket(ifo: TReliableUDPPacketLogRecord);
    procedure SendAck;
    procedure ChangeConnectionIDInLog(log: TReliableUDPPacketLog; iOldID,
      iNewID: int64);
    procedure SendKeepAlive;
    procedure AddToRXQueue(ifo: TReliableUDPPacketLogRecord);
    procedure RxLogPacketIfRequired(r: TReliableUDPPacketLogRecord);
    procedure TxLogPacketIfRequired(r: TReliableUDPPacketLogRecord);
    procedure RaiseDroppedError;
    function CanSystemThreadIdle: boolean;
    function CanUserThreadIdle: boolean;
    procedure EvalSystemThreadSignal;
    procedure EvalUserThreadSignal;
    procedure SayHello;
    procedure SetCloseMe(const Value: boolean);
    procedure InitiateCloseDueToSystemExeception;
    procedure InitiateCloseDueToUserExeception;
    procedure InitiateCloseFromClient;
    procedure InitiateCloseFromDestructor;
    procedure InitiateCloseFromServerUserthread;
    procedure PrePareForDestruction(co: TCloseOrigin);
    procedure REquestImmediateRetransmission;
  strict protected
    lastsendtime, jamtime: ticker;
    WaitForNothing: boolean;
    procedure HandleIncomingCommandPacket(ifo: TReliableUDPPacketLogRecord);
    procedure HandleIncomingDataPacket(ifo: TReliableUDPPacketLogRecord);
    procedure HandleIncomingSystemPacket(ifo: TReliableUDPPacketLogRecord);
    procedure SEtUserThreadSignal(b: boolean);
    procedure SEtSystemThreadSignal(b: boolean);
    function GetUserThreadSignal: boolean;
    function GetSystemThreadSignal: boolean;
    function CanDestroyBySystemThread: boolean;
    procedure RetransImmediate(iExpecftedSequenceNumber: int64);
  public
    LocalDebugInfo,RemoteDebugInfo: TRUDPDebugInfo;
    tmLAstSeen: ticker;
    tmLAstKeepAliveSent: ticker;
    connected: boolean;
    connectionid: int64;
    seed: int64;
    eet: TRUDPSystemThread;
    threadpointerlock: TCLxCriticalSection;
    user_thread: TRUDPUserThread;
    _nextexpectedSequenceNumber: int64;
    _nextsequencenumber: int64;
    _nextQueuedSequenceNumber: int64;
    txLog: TReliableUDPPacketLog;
    rxLog: TReliableUDPPacketLog;
    rxQueue: TReliableUDPPacketLog;
    rxDataLog: TReliableUDPPacketLog;
    retrans_timeout: ticker;
    finalsequencenumber: int64;
    FAvailableData: nativeint;
    fLastDebugMessage: string;
    FCloseMe: boolean;
    SentToGarbage: boolean;
    Connecting: boolean;
    TimedOut: boolean;
    WasCreatedWithUserThread: boolean;

    Error: string;
    ConnectionType: TConnectionType;
    procedure ChangeConnectionIDInLogs(iOldID,
      iNewID: int64);

    procedure ThreadExecute(thr: TRUDPSystemThread);
    procedure UserThreadExecute(thr: TRUDPUserThread);

    function CanIdleSystemThreaD: boolean;

    constructor Create(ct: TConnectionType; mult: TMultiplexedUDPEndpoint);reintroduce;virtual;
    destructor Destroy;override;
    procedure BeforeDestruction;override;
    procedure ClearLogs;
    procedure Detach;override;
    property IndexValue: int64 read GetIndexValue;

  //--------------------------------------------------------------------------
    procedure ResetKeepAliveTimer;
    procedure HandleIncomingPacket(ifo: TReliableUDPPacketLogRecord);
  //--------------------------------------------------------------------------


    property Remotehost: string read FRemoteHost write FRemoteHost;
    property RemotePort: nativeint read FRemotePOrt write FRemotePort;
    property Multiplexer: TMultiplexedUDPEndpoint read FMultiplexer write FMultiplexer;
    function Connect(remote_host: string; remote_port: nativeint): boolean;overload;
    function Connect: boolean;overload;
    function SendDAta(p: pbyte; len: nativeint): nativeint;
    function ReadDAta(p: pbyte; len: nativeint): nativeint;
    function AvailableData: nativeint;
    procedure SendPacketAndCreateHeader(p: pbyte; len: nativeint);overload;
    procedure SendPAcket(h: TReliableUDPHeader; p: pbyte; len: nativeint);overload;
    procedure SendPAcketCopyPayLoad(h: TReliableUDPHeader; p: pbyte; len: nativeint);overload;
    function NextSequenceNumber: int64;
    procedure DoAvailableData;
    procedure ProcessKeepAliveTimer;
    procedure ProcessElegantClosure;
    procedure AckIncoming;
    procedure RetransOutgoing;
    function WaitForAck(h: TReliableUDPHeader): boolean;
    function WaitForConnected(iTimeOut: nativeint): boolean;
    procedure ClearTXLogsUpTo(seq: int64);
    procedure ProcessPacketQUEUE;
    procedure GuaranteeRead(p: pbyte; len: nativeint);
    function WaitForData(iTimeOutMs: nativeint): boolean;
    function Disconnect: boolean;


    function WaitForIncomingDataSignal(iTimeOutMS: nativeint; bRAiseIfDropped: boolean): boolean;
    property KeepAliveInterval: nativeint read FKeepAliveInterval write FKeepAliveInterval;
    property KeepAliveStagger: nativeint read FKeepAliveStagger write FKeepAliveStagger;
    property KeepAliveTimeout: nativeint read FKeepAliveTimeout write FKeepAliveTimeout;
    procedure StartDisconnect;
    function DisconnectFinished: boolean;
    function DebugMessageForLog(l: TReliableUDPPacketLog): string;
    function DebugMessage: string;
    property DestroyOnClose: boolean read FDestroyOnClose write FDestroyOnClose;

    property UserThreadSignal: boolean read GetUserThreadSignal write SetUserThreadSignal;
    property SystemTHreadSignal: boolean read GetSystemThreadSignal write SetSystemThreadSignal;
    function DebugSummaryMessage: string;
    property CloseMe: boolean read FCloseme write SetCloseMe;
    property CloseState: TCloseState read FCloseState write FCloseState;
    property CloseOrigin: TCloseOrigin read FCloseOrigin write FCloseOrigin;
    property evUserDataIncoming: TSignal read FevUserDataIncoming;
  end;





  //--------------------------------------------------------------------------
  TReliableUDPEndpointList = class(TStandardList<int64, TReliableUDPEndpoint>)
  //keeps track of all connected clients
  public
    function FindByClientSeed(sRemotehost: string; remoteport: ni; iSeed: int64): TReliableUDPEndpoint;
  end;



  TUDPDataAvailable = procedure (endpoint: TReliableUDPEndpoint) of object;

  //--------------------------------------------------------------------------
  TSimpleReliableUDPClient = class (TSimpleAbstractConnection)
  protected
    cli: TReliableUDPEndpoint;//todo 1:translate
  public
    constructor Create;override;
    function GetConnected: boolean;override;//todo 1:translate
    function DoReadData(buffer: pbyte; length: integer): integer;override;//todo 1:translate
    function DoSendData(buffer: pbyte; length: integer): integer;override;//todo 1:translate

    function Connect: boolean; override;//todo 1:translate
    destructor Destroy;override;//todo 1:translate
    procedure Disconnect; override;//todo 1:translate
    function WaitForData(timeout: cardinal): boolean;override;//todo 1:translate
  end;

  TSimpleReliablePrivateServerEndpoint = class(TSimpleAbstractConnection)
  public
    cli: TReliableUDPEndpoint;
    procedure Detach;override;
    destructor Destroy;override;

    function GetConnected: boolean;override;
    function DoReadData(buffer: pbyte; length: integer): integer;override;
    function DoSendData(buffer: pbyte; length: integer): integer;override;//todo 1:translate

    function Connect: boolean; override;
    procedure Disconnect; override;
    function WaitForData(timeout: cardinal): boolean;override;
  end;


  //--------------------------------------------------------------------------
  TMultiplexedUDPEndpoint = class(TIdUDPServer)
  private
  protected
    FOnDataAvailable: TUDPDataAvailable;
    _nextconnectionid: int64;
    FEndpoints: TReliableUDPEndpointList;
    FLocalPort: nativeint;
    sect: TCLXCriticalSection;
    shuttingdown: boolean;
    procedure DoDataAvailable(ep: TReliableUDPEndpoint);//todo 1:translate
    function HandleSystemChangeConnID(ifo: TReliableUDPPacketLogRecord):TReliableUDPEndpoint;//todo 1:translate
  public
    procedure Lock;
    procedure Unlock;

    constructor CReate(AOwner: TComponent);reintroduce;virtual;//todo 1:translate
    destructor Destroy;override;//todo 1:translate
    procedure Detach;virtual;//todo 1:translate
    procedure DetachAndFree;virtual;//todo 1:translate
    //--------------------------------------------------------------------------
    //--------------------------------------------------------------------------
    procedure DoUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle); override;//todo 1:translate
    //--------------------------------------------------------------------------
    //--------------------------------------------------------------------------
    //--------------------------------------------------------------------------

    function HandleSystemPacket(ifo: TReliableUDPPacketLogRecord): TReliableUDPEndpoint;//todo 1:translate
    function HandleSystemCommandPacket(ifo: TReliableUDPPacketLogRecord): TReliableUDPEndpoint;//todo 1:translate
    function FindAvailableConnectionID: int64;//todo 1:translate
    function  HandleSystemCreatePacket(ifo: TReliableUDPPacketLogRecord):TReliableUDPEndpoint;//todo 1:translate
    function HandleSystemCommandResponsePacket(ifo: TReliableUDPPacketLogRecord): TReliableUDPEndpoint;//todo 1:translate

    procedure DispatchPacketToEndpoint(ifo: TReliableUDPPacketLogRecord);overload;//todo 1:translate
    procedure DispatchPackettoEndpoint(ep: TReliableUDPEndpoint; ifo: TReliableUDPPacketLogRecord);overload;//todo 1:translate
    procedure SendDataFromEndPoint(ep: TReliableUDPEndPOint; h: TReliableUDPHEader; mem: PByte; memlen: nativeint);//todo 1:translate
    procedure BindToport(iPort: nativeint);
    procedure BindToAnyPort();//todo 1:translate
    function CreateEndPoint(connid: int64 = -1): TReliableUDPEndpoint;//todo 1:translate
    procedure DeregisterEndPOint(ep: TReliableUDPEndpoint);//todo 1:translate
    function NextConnectionId: int64;//todo 1:translate
    property LocalPort: nativeint read FLocalport;//todo 1:translate
    procedure SendError(ep: TReliableUDPEndpoint; bSystem: boolean; b: byte; data1, data2: int64;  diagnostic: string);//todo 1:translate
    property OnDataAvailable: TUDPDataAvailable read FOnDataAvailable write FonDataAvailable;//todo 1:translate
    //todo 1: call ondatavailable when Data is added to rxDataLog
    //todo 1: call ondataavailable after OnDataAvailable is serviced, if data is STILL available
    function DebugConnections: string;
    procedure DisconnectAll;
  end;

  //--------------------------------------------------------------------------
  TMultiplexedUDPClient = class(TMultiplexedUDPEndpoint)
  protected
    class var class_sect: TCLXCriticalSection;//todo 1:translate
    class var CFMasterClient: TMultiplexedUDPClient;//todo 1:translate
  public
    class procedure InitClass;//todo 1:translate
    class procedure FinalizeClass;//todo 1:translate
    class procedure LockClass;//todo 1:translate
    class procedure UnlockClass;//todo 1:translate
    class function MasterClient: TMultiplexedUDPClient;//todo 1:translate

  end;

  //--------------------------------------------------------------------------
  TMultiplexedUDPServer = class(TMultiplexedUDPEndpoint)//todo 1:translate
  public
  end;



procedure UDPDebug(s: string);inline;


implementation

{ TReliableUDPHeader }


procedure UDPDebug(s: string);
begin
  Debug.Log(s);
end;


function TReliableUDPHeader.DebugString: string;
begin
  result := '[srcc__dn]';
  if Flag_System          then result[1+STRZ] := uppercase(result[1+STRZ])[STRZ];
  if Flag_Retrans         then result[2+STRZ] := uppercase(result[2+STRZ])[STRZ];
  if Flag_CommandResponse then result[3+STRZ] := uppercase(result[3+STRZ])[STRZ];
  if Flag_Command         then result[4+STRZ] := uppercase(result[4+STRZ])[STRZ];
  if Flag_CLOSE         then result[7+STRZ] := uppercase(result[7+STRZ])[STRZ];
  if Flag_Create          then result[8+STRZ] := uppercase(result[8+STRZ])[STRZ];
  result := STringReplace(result, '__', inttohex(ord(Flag_AckType), 2), [rfReplaceAll]);
  result := result + '#'+inttostr(sequencenumber);
  result := result + '_EXP_'+inttostr(expected_sequencenumber);
  result := result + '_CMD_'+inttostr(command);
  result := result + '_CONN_'+inttostr(connectionid);




end;

function TReliableUDPHeader.GetFlagAck: TAckType;
begin
  result := TAckType((self.flags shr 2) and 3);
end;

function TReliableUDPHeader.GetFlagCommand: boolean;
begin
  result := BitGet(@flags, 4);

end;

function TReliableUDPHeader.GetFlagCommandError: boolean;
begin
  result := BitGet(@command, 7);
end;

function TReliableUDPHeader.GetFlagCommandResponse: boolean;
begin
  result := BitGet(@flags, 5);
end;

function TReliableUDPHeader.GetFlagCreate: boolean;
begin
  result := BitGet(@flags, 0);
end;

function TReliableUDPHeader.GetFlagDestroy: boolean;
begin
  result := BitGet(@flags, 1);
end;

function TReliableUDPHeader.GetFlagRetrans: boolean;
begin
  result := BitGet(@flags, 6);
end;

function TReliableUDPHeader.GetFlagSystem: boolean;
begin
  result := BitGet(@flags, 7);
end;

function TReliableUDPHeader.IncomingData: pbyte;
begin
  result := pbyte(@self) + sizeof(self);
end;

procedure TReliableUDPHeader.Init(ep: TReliableUDPEndpoint);
begin
  flags := 0;
  tag[0] := byte(ansichar('A'));
  tag[1] := byte(ansichar('D'));
  if ep = nil then
    connectionid := 0
  else
    connectionid := ep.connectionid;
  expected_sequencenumber := 0;


end;

function TReliableUDPHeader.IsDataPacket: boolean;
begin
  result := not (Flag_System or Flag_Command or Flag_CommandResponse or Flag_Create or Flag_CLOSE);
end;

procedure TReliableUDPHeader.PrepareToSend(ep: TReliableUDPEndpoint);
begin
  connectionid := ep.connectionid;
  expected_sequencenumber := ep._nextexpectedSequenceNumber;//<--- this should always be included
  if Flag_AckType <> ackNone then begin
    sequencenumber := ep.NextSequenceNumber;
  end else
    sequencenumber := -1;//should make the logs easier to read

end;

procedure TReliableUDPHeader.SetFlagACk(const Value: TAckType);
var
  v: nativeint;
begin
  v := ord(value);
  Flags := (Flags and not (3 shl 2))+(v shl 2);

end;

procedure TReliableUDPHeader.SetFlagCommand(const Value: boolean);
begin
  BitSet(@flags, 4, value);

end;

procedure TReliableUDPHeader.SetFlagCommandError(const Value: boolean);
begin
  BitSet(@command, 7,value);
end;

procedure TReliableUDPHeader.SetFlagCommandResponse(const Value: boolean);
begin
  BitSet(@flags, 5, value);
end;

procedure TReliableUDPHeader.SetFlagCReate(const Value: boolean);
begin
  BitSet(@flags, 0, value);
end;

procedure TReliableUDPHeader.SetFlagDestroy(const Value: boolean);
begin
  BitSet(@flags, 1, value);
end;

procedure TReliableUDPHeader.SetFlagRetrans(const Value: boolean);
begin
  BitSet(@flags, 6, value);
end;

procedure TReliableUDPHeader.SetFlagSystem(const Value: boolean);
begin
  BitSet(@flags, 7, value);
end;

{ TReliableUDPEndpoint }

procedure TReliableUDPEndpoint.SendAck();
var
  h: TReliableUDPHeader;
begin
  Lock;
  try
    h.Init(self);
    h.connectionid := self.Connectionid;
    h.Flag_AckType := ackNone;
    h.command := CMD_ACK;
    h.Flag_Command := true;
    h.sequencenumber := -1;//0 because there's no ack coming back
    h.expected_sequencenumber := _nextexpectedSequenceNumber;
    UDPDebug('Sending ack up to and including #'+inttostr(h.expected_sequencenumber-1));
    //h.PrepareToSend(self);
    SendPacket(h, nil, 0);
  finally
    Unlock;
  end;
end;

procedure TReliableUDPEndpoint.REquestImmediateRetransmission;
var
  h: TReliableUDPHeader;
begin
  exit;
  Lock;
  try
    h.Init(self);
    h.connectionid := self.Connectionid;
    h.Flag_AckType := ackNone;
    h.command := RUDP_CMD_REQUEST_RETRANS;
    h.Flag_Command := true;
    h.sequencenumber := -1;//-1 because there's no ack coming back
    h.expected_sequencenumber := _nextexpectedSequenceNumber;
    UDPDebug('Sending retransmission request '+inttostr(h.expected_sequencenumber-1));
    //h.PrepareToSend(self);
    SendPacket(h, nil, 0);
  finally
    Unlock;
  end;
end;

procedure TReliableUDPEndpoint.AckIncoming;
var
  bSend: boolean;
  r: TReliableUDPPacketLogRecord;
begin
  //try to increment the expected sequence number until a break in the sequence is found
  bSend := false;
  repeat
    Lock;
    try
      r := rxLog.Find(_nextexpectedSequenceNumber);
      if r <> nil then begin
        inc(_nextexpectedsequencenumber);
        if r.h.Flag_AckType = ackImmediate then begin
          bSend := true;
        end;
        r.Acked := true;
        UDPDebug('ACK++: '+r.DebugString);
        if rxLog.Has(r) then begin
          rxLog.Remove(r);
          r._Release;
          EvalSYstemThreadSignal;
          EvalUserThreadSignal;
        end;
      end;
    finally
      unlock;
    end;
  until r = nil;



  //acknowledge current sequence level
  if bSend then
    SendAck();



end;

function TReliableUDPEndpoint.AvailableData: nativeint;
begin
  //This is threadsafe because:
  //-----------------------------
  //Available Data should only be decreased by user thread
  //there should only be one user thread
  //if this value changes on another thread it should only increment
  //the value is inconsequential if returned invalid for a single cycle
  result := FAvailableData;

end;

procedure TReliableUDPEndpoint.InitiateCloseFromDestructor;
begin
  lock;
  try
    //is there a user thread?  if so, then there was a client connected
    //THIS MEANS
    //- the user thread can be terminated once the
    //- data has been fed to it (rxDataLog.count = 0)
    //- if for some reason the user thread doesn't take all the data
    //- the connection may hang forever because
    //-   1.  as far as we know, the user thread is just busy and will eventualyl get around to the data
    //-   2.  we can't assume that because the user thread is busy that it doesn't want the data
    //-   3.  HOWEVER, we could:
    //-       the user thread's reading operations should have some kind of timeout built into them
    //-       a. Check that RDTP Read() function allows for timeouts during MID read.
    //            - I think this mechanism should work as follows
    //              1. The RDTP Listener shoujld see that there are bytes
    //              2. It should read those bytes... maybe they're only 17 of 18 bytes needed
    //              3. Once the bytes are read, then the RDTP listener should be able to see that
    //                    the socket is closed (which only happens once the data has been drained).
    //    4.  when the user thread sees that the socket is closed, it should return from itself and
    //        give its control back up to the ExternalEventThread Loop
    //        it may expect us to throw an exception when it attempts to read from a disconnected socket.
    //        this should have the same effect... bubbling control up to the top
    //    5.  The user thread will then continue looping until it sees the evUser signal.
    //        It should eventually see the evJam signal.... lets make sure that evJam comes in as soon as possible
    //        ---- like when Read() is called ... in fact... EvalUserThreadSignal could also eval the Jam status.
                                                                //er no it couldn't... it would have to be sent to the threadpool
    //        maybe I add a "PreJam" state.   That will prevent the thread from looping and will be reset when added to the pool.
    //         When PreJam is Detected, on the next loop, The thread will simply wait for Jam before running IdleStateTasks and will NOT call OnExecute.

    //so Off on a tangent.
    //---- this means that in the destructor, if I have a user thread, we need to wait on the user thread to do its shit

    //---- if there is no user thread, We assume that the client wanted us to
    //---- disconnect, but the client didn't necessarily call disconnect explicitly.  So we'll first call disconnect and wait for proper
    //---- disconnect.




    //- about the packet inter
    StartDisconnect;

  finally
    unlock;
  end;
end;

procedure TReliableUDPEndpoint.InitiateCloseFromClient;
begin
  self.CloseOrigin := coClient;
  CloseState := csWaitingForPackets;
  eet.HasWork := true;

end;

procedure TReliableUDPEndpoint.InitiateCloseFromServerUserthread;
begin
  self.CloseOrigin := coServer;
end;

procedure TReliableUDPEndpoint.InitiateCloseDueToUserExeception;
begin
  self.CloseOrigin := coClient;
  CloseState := csTimedOut;
end;


procedure TReliableUDPEndpoint.InitiateCloseDueToSystemExeception;
begin
  self.CloseOrigin := coServer;
  CloseState := csTimedOut;
end;






procedure TReliableUDPEndpoint.BeforeDestruction;
begin
  if not CloseMe then
    raise ECritical.Create('You cannot destroy a '+classname+' that is connected.   Disconnect it first.');

  if assigned(eet) then begin
    eet.EndPOint := nil;
    TPM.NoNeedthread(eet);
  end;
  if assigned(user_thread) then begin
    user_thread.endpoint := nil;
    TPM.NoNeedthread(user_thread);
  end;



  inherited;

end;

procedure TReliableUDPEndpoint.ClearLogs;
var
  p: TReliableUDPPacketLogRecord;
begin
  if assigned(txLog) then
  while txLog.Count > 0 do begin
    p := txLog.Items[txLog.count-1];
    txLog.Delete(txLog.Count-1);
    p._Release;

  end;

  if assigned(rxLog) then
  while rxLog.Count > 0 do begin
    p := rxLog.Items[rxLog.count-1];
    rxLog.Delete(rxLog.Count-1);
    p._Release;
  end;

  if assigned(rxDataLog) then
  while rxdataLog.Count > 0 do begin
    p := rxdataLog.Items[rxdataLog.count-1];
    rxdataLog.Delete(rxdataLog.Count-1);
    p._Release;
  end;

  if assigned(rxQueue) then
  while rxQueue.Count > 0 do begin
    p := rxQueue.Items[rxQueue.count-1];
    rxQueue.Delete(rxQueue.Count-1);
    p._Release;
  end;

  inherited;

end;




procedure TReliableUDPEndpoint.ClearTXLogsUpTo(seq: int64);
var
  t: nativeint;
  r: TReliableUDPPacketLogRecord;
begin
  Lock;
  try
    txLog.Lock;
    try
      for t:= txLog.Count-1 downto 0 do begin
        r := txLog.items[t];
        if r.h.sequencenumber < seq then begin
          if txLog.Has(r) then begin
            txLog.remove(r);
            r._Release();
          end;
        end;
      end;
    finally
      txLog.Unlock;
    end;
  finally
    Unlock;
  end;

end;

function TReliableUDPEndpoint.Connect: boolean;
var
  h: TReliableUDPHeader;
begin
  if connected then begin
    result := true;
    exit;
  end;

  Lock;
  try
    h.Init(nil);
    h.connectionid := self.connectionid;
    h.Flag_Create := true;
    h.Flag_System := true;
    h.command := RUDP_SYS_CMD_CREATE;
    h.sequencenumber := 0;
    h.Flag_AckType := ackImmediate;
    h.Flag_Command := true;
    h.PrePareToSend(self);
    Connecting := true;

    SendPacket(h, nil, 0);
  //  multiplexer.Active := true;
  finally
    Unlock;
  end;
  result := WaitForAck(h);
  result := Error = '';
  if result then
    result := WaitForConnected(10000);

  SayHello;
  result := Connected;

end;

function TReliableUDPEndpoint.Connect(remote_host: string;
  remote_port: nativeint): boolean;
begin
  self.remotehost := remote_host;
  RemotePort := remote_port;
  result := connect;

end;



constructor TReliableUDPEndpoint.CReate(ct: TConnectionType; mult: TMultiplexedUDPEndpoint);
begin
  inherited CReate;

  UDPDebug('Creating RUDP connection @'+inttohex(nativeint(pointer(self)),16));
  ConnectionType := ct;
  ICS(threadpointerlock);
  ResetKeepAliveTimer;
  tmLAstKeepAliveSent := GetTicker;


  localdebuginfo.Init;
  WasCreatedWithUserThread := ct = ctServer;

{$IFDEF SHORT_KEEP_ALIVE}
  KeepAliveInterval := 3000;
  KeepAliveTimeout := 15000;
  KeepAliveStagger := 4000;
  retrans_timeout := 500;
{$ELSE}
  KeepAliveInterval := 20000;
  KeepAliveTimeout := 55000;
  KeepAliveStagger := 4000;
  retrans_timeout := 100;
{$ENDIF}

  _nextexpectedsequencenumber := 0;
  _nextsequencenumber := 0;

  tmLastSeen := tickcount.GetTicker;
  CloseMe := false;

  txLog := TReliableUDPPacketLog.Create;
  rxLog := TReliableUDPPacketLog.Create;
  rxDataLog := TReliableUDPPacketLog.create;
  rxQueue := TReliableUDPPacketLog.create;


  Connecting := true;
  Multiplexer := mult;

  if ct = ctServer then begin
    //we need to create a "user_thread" is this is a server... this is the thread that will implement the server requests
    //independent of all the packet clearing nonsense.
    ECS(threadpointerlock);
    try
      user_thread := TPM.NeedThread<TRUDPUserThread>(self);
      user_thread.Loop := true;
      user_thread.endpoint := self;
      //user_thread.loop := true;
      //user_thread.OnExecute := self.UserThreadExecute;
      if user_thread.suspended then
        raise Exception.create('WTF! THREAD IS SUSPENDED COMING OUT OF POOL!  This is no longer valid.');

      FevUserDataIncoming := user_thread.evHasWork;
      user_thread.Start;

    finally
      LCS(threadpointerlock);
    end;

  end else begin
    FevUserDataIncoming := TSignal.create;
  end;


  ECS(threadpointerlock);
  try
    eet := TPM.NeedThread<TRUDPSystemThread>(self);
    eet.Loop := true;
    eet.endpoint := self;
    //eet.OnExecute := self.ThreadExecute;
    if eet.suspended then
      raise Exception.create('WTF! THREAD IS SUSPENDED COMING OUT OF POOL! This is no longer valid.');
    eet.Start;
  finally
    LCS(threadpointerlock);
  end;
end;

function TReliableUDPEndpoint.DebugMessage: string;
begin
  Lock;
  try
    if not assigned(rxLog) then exit;
    if not assigned(rxQueue) then exit;
    if not assigned(rxDataLog) then exit;
    if not assigned(txLog) then exit;



    result := {self.DebugSummaryMEssage+NEWLINE+}'seq:'+inttostr(_nextsequencenumber)+NEWLINE
             +'exp:'+inttostr(_nextexpectedSequenceNumber)+NEWLINE;
    result := result + 'rxLog'+NEWLINE+'-----'+NEWLINE+DebugMessageForLog(rxLog)+NEWLINE
                     + 'txLog'+NEWLINE+'-----'+NEWLINE+DebugMessageForLog(txLog)+NEWLINE
                     + 'rxQueue'+NEWLINE+'-----'+NEWLINE+DebugMessageForLog(rxQueue)+NEWLINE
                     + 'rxDataLog'+NEWLINE+'-----'+NEWLINE+DebugMessageForLog(rxDataLog)+NEWLINE;


    if result = fLastDebugMessage then begin
      result := '';
    end else
      fLAstDebugMessage := result;



  finally
    Unlock;
  end;
end;

function TReliableUDPEndpoint.DebugMessageForLog(
  l: TReliableUDPPacketLog): string;
var
  sl: TStringlist;
  t: nativeint;
begin
  Lock;
  try
    sl := TStringlist.create;
    try
      for t:= 0 to l.Count-1 do begin
        sl.Add(l.Items[t].DebugString);
      end;
      result := sl.text+NEWLINE+inttostr(sl.count)+' total.'+NEWLINE;
    finally

      sl.free;//ok
    end;
  finally
    Unlock;
  end;
end;

function TReliableUDPEndpoint.DebugSummaryMessage: string;
var
  s: string;
begin
  lock;
  try
    if TECS(threadpointerlock) then
    try

      if assigned(eet) then
        s := 'ST='+inttostr(eet.threadid)+':'
      else
        s := 'ST=nil:';

      if assigned(user_thread) then
        s := s + 'UT='+inttostr(user_thread.threadid)+':'
      else
        s := s + 'UT=nil:';

      result := 'From/To:'+Remotehost+':'+inttostr(RemotePort)+':cid='+inttostr(connectionid)+': ___LocalDebug('+LocalDebugInfo.ToString()+');RemoteDebug('+RemoteDebugInfo.ToString()+'):'+stringx.booltostr(Closeme, 'CloseMe;','')+stringx.booltostr(TimedOUT, 'TimedOut;','')+
                ':rx='+inttostr(rxLog.Count)+
                ':tx='+inttostr(txLog.Count)+
                ':rxQ='+inttostr(rxQueue.Count)+
                ':rxD='+inttostr(rxDataLog.Count)+';'+s+inttostr(KeepaliveTimeout-tickcount.GetTimeSince(tmLastSeen));
    finally
      LCS(threadpointerlock);
    end;
  finally
    Unlock;
  end;

end;

destructor TReliableUDPEndpoint.Destroy;
begin
  UDPDebug('Destroying RUDP connection @'+inttohex(nativeint(pointer(self)),16));
  UDPDebug(classname+' is being destroyed.');

  if connectiontype=ctClient then
    FEvUserDataIncoming.free;


  inherited;
  DCS(threadpointerlock);
  UDPDebug('Destroyed RUDP connection @'+inttohex(nativeint(pointer(self)),16));


end;


procedure TReliableUDPEndpoint.PrePareForDestruction(co: TCloseOrigin);
begin
    if assigned(user_thread) then begin
      //in ALL cases we need to wait for the user thread to
      //complete its actions because we don't have a clue what's
      //going on in there...
      user_thread.Stop;
      user_thread.WaitForFinish;

      user_thread.endpoint := nil;
      TPM.NoNeedThread(user_thread);
    end;
    user_thread := nil;


    if assigned(eet) then begin
      eet.endpoint := nil;
      TPM.NoNeedThread(eet);
    end;
    eet := nil;

  CloseState := csClosed;



end;

procedure TReliableUDPEndpoint.Detach;
begin
  if Detached then exit;
  self._AddRef;

  if assigned(user_thread) then
    PrePareForDestruction(coServer)
  else
    PrepareForDestruction(coClient);

  UDPDebug('Deregistering endpoint');
  if assigned(Multiplexer) then begin
    Multiplexer.Lock;
    try
      self.Multiplexer.DeregisterEndPOint(self);
    finally
      MultiPlexer.unlock;
    end;
  end;


  clearLogs;

  txLog.Free;//ok
  rxLog.Free;//ok
  rxDataLog.Free;//ok
  rxQueue.Free;//ok


  UDPDebug('Done Destroying.');
  inherited;
  self._release;

end;

function TReliableUDPEndpoint.Disconnect: boolean;
begin
  if closeme then begin
    result := false;
    exit;
  end;

  result := true;
  StartDisconnect;
  while not DisconnectFinished do begin
    sleep(10);
  end;
  CloseMe := true;
  connected := false;
end;

function TReliableUDPEndpoint.DisconnectFinished: boolean;
begin
  Lock;
  try
    result := txLog.Count = 0;
    if result then begin
      connected := false;
    end;

  finally
    Unlock;
  end;
end;

procedure TReliableUDPEndpoint.DoAvailableData;
begin
  if assigned(Multiplexer) then
    Multiplexer.DoDataAvailable(self);






end;

function TReliableUDPEndpoint.GetIndexValue: int64;
begin
  result := connectionid;
end;


function TReliableUDPEndpoint.GetSystemThreadSignal: boolean;
begin
  result := eet.HasWork;
end;

function TReliableUDPEndpoint.GetUserThreadSignal: boolean;
begin
  result := user_thread.HasWork;
end;

procedure TReliableUDPEndpoint.GuaranteeRead(p: pbyte; len: nativeint);
VAR
  iTotalRead: nativeint;
  iJustRead: nativeint;
begin
  iTotalREad := 0;
  while iTotalRead < len do begin
    if WaitForData(1000) then begin
      iJustRead := self.ReadData(@p[iTotalRead], len-iTotalREad);
      inc(iTotalREad, iJustRead);
    end;
  end;
end;

procedure TReliableUDPEndpoint.HandleIncomingCommandPacket(ifo: TReliableUDPPacketLogRecord);
begin
  //look at command id and do whatever it says to do
  case ifo.h.command of
    CMD_ACK: begin
      UDPDebug('Got ACK <'+inttostr(ifo.h.expected_sequencenumber));
      if ifo.h.expected_sequencenumber > (self._nextexpectedSequenceNumber + 40000) then begin
        CloseMe := true;
        Error := 'Packet expected sequence number is out of range tolerace.';
      end;
    end;
    CMD_KEEP_ALIVE: begin
      ResetKeepAliveTimer;
      //CloseRequested := true;
    end;
    RUDP_CMD_REQUEST_RETRANS:
    begin
      //look at expected sequence number and retransmit packet matching
      RetransImmediate(ifo.h.expected_sequencenumber);

    end;
    RUDP_CMD_DEBUGINFO: begin
      movemem32(@self.remoteDebugInfo, ifo.payload, lesserof(ifo.payload_length, sizeof(self.remoteDebugInfo)));
    end;

    RUDP_CMD_CLOSE: begin
      UDPDebug('******* DESTROY PACKET RECEIVED ******');
      Lock;//c#
      try
        InitiateCloseFromClient;
      finally
        Unlock;
      end;

//    StartDisconnect;

    end;
  else begin
    Error := 'I don''t know this command: '+inttostr(ifo.h.command);
    CloseMe := true;
  end;
  end;
end;

procedure TReliableUDPEndpoint.HandleIncomingDataPacket(ifo: TReliableUDPPacketLogRecord);
begin
  //put the packet in the data queue
  Lock;
  try
    UDPDebug('+DATA: '+ifo.DebugString);
//    UDPDebug('Adding '+inttostr(ifo.h.sequencenumber)+' to data log.');
    if not rxDataLog.Has(ifo) then begin
      rxDataLog.Add(ifo);
      ifo._AddRef;
    end;

    UDPDebug('There are now '+inttostr(rxDAtaLog.count)+' packets in the data log.');
    FAVailableData := FAvailableData + ifo.payload_length;
    UDPDebug('Available data: '+inttostr(FAvailableData));

  finally
    Unlock;

  end;

end;


procedure TReliableUDPEndpoint.QueueIncomingPacket(ifo: TReliableUDPPacketLogRecord);
begin
  try

    //if the packet's sequence number is less than
    //our expected sequence number, then drop this packet. (it was an unnessary retrans)
    if (ifo.h.sequencenumber < _nextexpectedSequenceNumber) then begin
      UDPDebug('xxxx NOT Added (old) xxxx '+inttostr(ifo.h.sequencenumber)+' < '+inttostr(_nextexpectedSequenceNumber));
      ifo._Release;
      exit;
    end;


    //if the packet requires acknowledgement, then
    //put it in the log
    if ifo.h.Flag_AckType <> ackNone then begin
      //if the packet is NOT our next expected sequence number,
      //aggressively ping the sender for the next expected sequence
      if self._nextexpectedSequenceNumber < (ifo.h.sequencenumber) then
        REquestImmediateRetransmission;//accepts no parameters because it uses current state only

      //if we don't have this packet
      if not rxLog.Has(ifo.h.sequencenumber) then begin
        RxLogPacketIfRequired(ifo);

        //when queuing packets we'll always take the entire queue into
        //account when deciding what order to process the information
        //ProcessPacketQueue();
        SystemThreadSignal := true;
      end else begin
      end;
    end else begin
      //for packets that don't ask for ACK, they can be queued in any order
      //this is sorta a requirement because packets that don't require
      //ACK also do not have sequence numbers.
      //Therefore we should handle this packet immediately
      Lock;
      try
        HandleIncomingPacket(ifo);
      finally
        Unlock;
      end;
    end;
  finally
    EvalSYstemThreadSignal;
  end;




end;


procedure TReliableUDPEndpoint.HandleIncomingPacket(ifo: TReliableUDPPacketLogRecord);
begin

  ResetKeepaliveTimer;
  ClearTxLogsUpTo(ifo.h.expected_sequencenumber);
  //
  if ifo.h.Flag_System then begin
    HandleIncomingSystemPacket(ifo);
  end else
  if ifo.h.Flag_Command then begin
    HandleIncomingCommandPacket(ifo);
  end else
  begin
    HandleIncomingDataPacket(ifo);
  end;


end;

procedure TReliableUDPEndpoint.HandleIncomingSystemPacket(ifo: TReliableUDPPacketLogRecord);
begin
  //the multiplexer handled me,
  //but I'll queue the original packet so it can be ACKed like all the others
  if ifo.h.Flag_AckType <> ackNone then begin

    Lock;
    try
      if not (rxLog.Has(ifo)) then begin
        rxLog.Add(ifo);
        ifo._AddRef;
      end;
      if ifo.RemoteOrigin then begin
        AddToRXQueue(ifo);
      end;
    finally
      Unlock;
    end;


  end;
end;






function TReliableUDPEndpoint.CanUserThreadIdle: boolean;
begin
  //can idle if
  //1. all outgoing packets are Acked
  //2. all outgoing packets are transmitted
  //3. There's no data on the pipe
  self.Lock;
  try
    result := (self.rxDataLog.count = 0);

    result := true;

  finally
    self.Unlock;
  end;


end;


function TReliableUDPEndpoint.CanDestroyBySystemThread: boolean;
begin
  result := ConnectionType = ctServer;
end;

function TReliableUDPEndpoint.CanIdleSystemThread: boolean;
begin
  lock;
  try
    result := (txLog.Count = 0) and (rxLog.Count = 0) and (rxQueue.count = 0);
  finally
    unlock;
  end;
end;


procedure TREliableUDPEndpoint.EvalSystemThreadSignal;
begin
  lock;
  try
    SystemThreadSIgnal := (not CanIdleSystemThread) or (WaitForNothing) or (CloseOrigin  <> coNotClosed);
  finally
    unlock;
  end;
end;

procedure TREliableUDPEndpoint.EvalUserThreadSignal;
begin
  lock;
  try
    UserThreadSIgnal := rxDataLog.Count > 0;
  finally
    unlock;
  end;
end;

function TReliableUDPEndpoint.CanSystemThreadIdle: boolean;
begin
  //can idle if
  //1. all outgoing packets are Acked
  //2. all outgoing packets are transmitted
  //3. There's no data on the pipe
  self.Lock;
  try
    result := (self.txLog.count = 0) and (self.rxLog.count = 0) and (GetSystemThreadSignal = false);
//    if result then
    //ClearSignalIncomingData;

    result := true;

  finally
    self.Unlock;
  end;


end;



procedure TReliableUDPEndpoint.TxLogPacketIfRequired(r: TReliableUDPPacketLogRecord);
begin
  if r.h.Flag_AckType = ackNone then
    exit;

  if not txLog.Has(r) then begin
    txLog.Add(r);
    r._AddRef;
  end;
end;

procedure TReliableUDPEndpoint.UserThreadExecute(thr: TRUDPUserThread);
begin
//  Debug.Log('User Thread Execute');
  if CloseMe then begin
    self.Detach;
    thr.EndPOint := nil;
    TPM.NoNeedthread(thr);//<--- IMPORTANT, must EXIT... no further operations permitted in this thread
    exit;
  end;


  Debug.Log('"Doing" Available Data');
  DoAvailableData;

  Lock;
  try
    FevUserDataIncoming.Signal(self.AvailableData > 0);
  finally
    Unlock;
  end;



end;

procedure TReliableUDPEndpoint.RxLogPacketIfRequired(r: TReliableUDPPacketLogRecord);
var
  searched: TReliableUDPPacketLogRecord;
begin
  if r.RemoteOrigin = false then
    raise Exception.create('You cannot add a local packet to the RXLog');
  if r.h.Flag_AckType = ackNone then
    exit;

  if not rxLog.Has(r) then begin
    rxLog.Add(r);
    r._AddRef;
  end;

  if not rxQueue.Has(r) then begin
    rxQueue.Add(r);
    r._AddRef;
  end;





end;

function TReliableUDPEndpoint.NextSequenceNumber: int64;
begin
  result := _nextsequencenumber;
  inc(_nextsequencenumber);
end;

procedure TReliableUDPEndpoint.ProcessElegantClosure;
begin
  if CloseState = csNotClosed then
    exit;


  if CloseMe then begin
//    if assigned(user_thread) then begin
//      TPM.NoNeedThread(user_thread);
//      user_thread := nil;
//    end;

    Lock;
    try
      if DestroyOnClose then begin
        if not SynchronousDestroy then begin
          if not SentToGarbage then begin
            GarbageCollect(self);
            SentToGArbage := true;//this thread will eventually get killed by garbage collection process
          end;
        END;
      end else begin
        ClearLogs;
      end;
    finally
      Unlock;
    end;

  end else begin
    //if the close was requested from THIS END or the OTHER END
    if CloseOrigin = coClient then begin
      Lock;
      try
        if (rxLog.count = 0) and (txLog.count = 0) then begin
          CloseMe := true;
        end;
      finally
        Unlock;
      end;
    end else begin
      Lock;
      try
        if (rxLog.count = 0) and (txLog.count = 0) and (rxQueue.count = 0) and (rxDatalog.count = 0)then begin
          CloseMe := true;
        end;
      finally
        Unlock;
      end;
    end;

  end;
end;

procedure TReliableUDPEndpoint.ProcessKeepAliveTimer;
var
  iTimeout: nativeint;
begin
  if connecting then
    iTimeout := CONNECTION_TIMEOUT
  else
    iTimeout := KeepAliveTimeout;

  if tickcount.GetTimeSInce(tmLastSeen) > iTimeout then begin
    //set all the disconnect flags, we don't need to attempt and elegant
    //disconnect... we've determined this connection is dead.
    CloseState := csTimedOut;
    if CanDestroyBySystemThread then
      CloseOrigin := coServer
    else
      CloseOrigin := coClient;

    Self.CloseMe := true;

    connecting := false;
    connected := false;
    timedout := true;
//    if assigned(user_thread) then begin
//      TPM.NoNeedthread(user_thread);
//      user_thread := nil;
//    end else begin
//      if not SentToGarbage then begin
//        GarbageCollect(self);
//        SentToGarbage := true;
//      end;
//    end;
  end
{$IFDEF ACK_KEEP_ALIVES}
  {$Message Error 'you also need to actually change the keep alives to be set flag_acktype, which i didn't do'}
  else if lesserof(tickcount.GetTimeSInce(tmLAstSeen), tickcount.GetTimeSInce(tmLAstKeepAliveSent)) > KeepAliveInterval then begin
{$ELSE}
  else if tickcount.GetTimeSInce(tmLAstKeepAliveSent) > KeepAliveInterval then begin
{$ENDIF}
    SendKeepAlive;
  end;


end;

procedure TReliableUDPEndPoint.SendKeepAlive;
var
  h: TReliableUDPHeader;
begin
  if multiplexer.shuttingdown then
    exit;

  if not connected then
    exit;

  Lock;
  try
    h.Init(self);
    h.Flag_Command := true;
    h.command := CMD_KEEP_ALIVE;
    h.Flag_AckType := ackNone;
    h.PrepareToSend(self);
    self.SendPAcket(h, nil, 0);
    tmLAstKeepAliveSent := tickcount.GetTicker;

  finally
    Unlock;
  end;

end;

procedure TReliableUDPEndpoint.AddToRXQueue(ifo: TReliableUDPPacketLogRecord);
begin
  Lock;
  try
    if not ifo.RemoteOrigin then
      raise Exception.create('Queued packets must be of remote origin');


  UDPDebug('CurrentThread:'+inttostr(TThread.CurrentThread.ThreadID));
  UDPDebug('Queueing:'+ifo.DebugString);

  if not rxQueue.Has(ifo) then begin
      rxQueue.Add(ifo);
      ifo._AddRef;
    end;
  finally
    Unlock;
  end;

end;

procedure TReliableUDPEndpoint.ProcessPacketQUEUE;
var
//  r: TReliableUDPPacketLogRecord;
  ifo: TReliableUDPPacketLogRecord;
begin
  //look at next expected sequence number
  //interpret what to do with this packet
  while true do begin
    Lock;
    try
      if rxQueue.count = 0 then exit;
//      rxQueue.Lock;
      try
        UDPDebug('Looking for '+inttostr(_nextQueuedSequencenumber)+' in queue.');
        ifo := rxQueue.Find(self._nextQueuedSequenceNumber);
        if ifo= nil then break;


        UDPDebug('HANDLING: '+ifo.DebugString);
        if not ifo.h.Flag_CReate then
          HandleIncomingPacket(ifo);

        ifo.queue_handled := true;
        inc(_nextQueuedSequenceNumber);
        if rxQueue.Has(ifo) then begin
          rxQueue.Remove(ifo);
          ifo._Release;
        end;

        EvalUserThreadSignal;
        EvalSystemThreadSignal;



      finally
//        rxQueue.Unlock;
      end;
    finally
      Unlock;
    end;


  end;

end;


procedure TReliableUDPEndpoint.RetransImmediate(
  iExpecftedSequenceNumber: int64);
var
  t: nativeint;
  l: TReliableUDPPacketLogRecord;
begin
  Lock;
  try
    txLog.Lock;
    try
      for t:= 0 to txLog.Count-1 do begin
        l := txLog.Items[t];
        if l.h.sequencenumber = iExpecftedSequenceNumber then begin
          l.SendTime := 0;
{
          l.sendtime := GetTicker;
          l.h.Flag_Retrans := true;
          l.h.expected_sequencenumber := self._nextexpectedSequenceNumber;

          UDPDebug('RETRANSING: '+l.DebugString);
          SendPacket(l.h, l.payload, l.payload_length);}

        end;
      end;
    finally
      txLog.Unlock;
    end;
  finally
    Unlock;
  end;
end;

procedure TReliableUDPEndpoint.RetransOutgoing;
var
  t: nativeint;
  l: TReliableUDPPacketLogRecord;
begin
  Lock;
  try
    txLog.Lock;
    try
      for t:= 0 to txLog.Count-1 do begin
        l := txLog.Items[t];
        if tickcount.GEtTimeSince(l.sendtime) > (retrans_timeout+l.randomNagleTimer) then begin
          l.sendtime := GetTicker;
          l.h.Flag_Retrans := true;
          l.h.expected_sequencenumber := self._nextexpectedSequenceNumber;

          UDPDebug('RETRANSING: '+l.DebugString);
          SendPacket(l.h, l.payload, l.payload_length);
          exit;
        end;
      end;
    finally
      txLog.Unlock;
    end;
  finally
    Unlock;
  end;

end;

procedure TReliableUDPEndpoint.SendPacket(h: TReliableUDPHeader; p: pbyte;
  len: nativeint);
var
  r: TReliableUDPPacketLogRecord;

begin
  UDPDebug('SendPacket seq:'+inttostr(h.sequencenumber));
  Lock;
  try
    txLog.Lock;
    try
      r := txLog.Find(h.sequencenumber);

      if r <> nil then begin
        r.h.Flag_Retrans := true;
        Multiplexer.SendDataFromEndPoint(self, r.h, r.payload, r.payload_length);
        lastsendtime := getticker;
        tmLAstKeepAliveSent := tickcount.GetTicker;
        UDPDebug('RETRANSMIT:'+r.DebugString);

      end else begin
        UDPDebug('SendPacket seq:'+inttostr(h.sequencenumber)+' Logging...');
        r := TReliableUDPPacketLogRecord.create(self, h, p, len);
        r._AddRef;
        try
          TxLogPacketIfRequired(r);
          Multiplexer.SendDataFromEndPoint(self, r.h, r.payload, r.payload_length);
          lastsendtime := getticker;
          tmLAstKeepAliveSent := tickcount.GetTicker;
          UDPDebug('Sent:'+r.DebugString);
        finally
          r._Release;

        end;
      end;
    finally
      txlog.Unlock;
    end;
  finally
    Unlock;
  end;




end;

procedure TReliableUDPEndpoint.SendPacketAndCreateHeader(p: pbyte; len: nativeint);
var
  h: TReliableUDPHeader;
begin
  h.Init(self);
  h.connectionid := self.connectionid;
  h.Flag_AckType := ackImmediate;
  if h.Flag_AckType <> ackNone then begin
    h.sequencenumber := NextSequencenumber;
  end;

  SendPacket(h, p, len);


end;

procedure TReliableUDPEndpoint.SendPAcketCopyPayLoad(h: TReliableUDPHeader;
  p: pbyte; len: nativeint);
var
  pp: pbyte;
begin
  if len = 0 then
    SendPacket(h, nil, 0)
  else begin
    pp := GEtMemory(len);
    movemem32(pp, p, len);
    SendPacket(h, pp, len);
  end;


end;

procedure TReliableUDPEndpoint.SetCloseMe(const Value: boolean);
begin
  FCloseme := Value;
  if value then begin
    connected := false;
    connecting := false;
    if CloseSTate = csNotClosed then
      CloseState := csTimedOut;
  end;

end;

procedure TReliableUDPEndpoint.SEtSystemThreadSignal(b: boolean);
begin
  eet.RunHot := b;

end;

procedure TReliableUDPEndpoint.SEtUserThreadSignal(b: boolean);
begin
  FEvUserDataIncoming.Signal(b)
//  user_thread.HasWork := b;

end;

procedure TReliableUDPEndpoint.SayHello;
var
  h: TReliableUDPHeader;
begin
  UDPDebug('*************SAYING HELLO');
  if connected then begin
    Lock;
    try
      h.Init(self);
      h.connectionid := self.connectionid;
      h.Flag_System := false;
      h.command := RUDP_CMD_DEBUGINFO;;
      h.Flag_AckType := ackImmediate;
      h.Flag_Command := true;
      h.PrepareToSend(self);
      SendPacketCopyPayLoad(h, @localdebugInfo, sizeof(localdebuginfo));
    finally
      Unlock;
    end;
  end;

end;

procedure TReliableUDPEndpoint.StartDisconnect;
var
  h: TReliableUDPHeader;
begin
  if not (closestate = csNotClosed) then
    exit;

  UDPDebug('*************DISCONNECT INITIATED');
  if connected then begin
    Lock;
    try
      h.Init(self);
      h.connectionid := self.connectionid;
      h.Flag_CLOSE := true;
      h.Flag_System := false;
      h.command := RUDP_CMD_CLOSE;
      h.Flag_AckType := ackImmediate;
      h.Flag_Command := true;
      h.PrepareToSend(self);
      finalsequencenumber := h.sequencenumber;
      SendPacket(h, nil, 0);
      CloseState := csWaitingForPackets;
      CloseOrigin := coClient;
    finally
      Unlock;
    end;
  //  multiplexer.Active := true;
  end;

  connected := false;  //TODO 1 : I don't think we want to set this to false until later.
end;

procedure TReliableUDPEndpoint.ThreadExecute(thr: TRUDPSystemThread);
var
  s: string;
begin
  try
    //thr.IdleStateTasks;
    if closeorigin <> coNotClosed then begin
      UDPDebug('System thread running while waiting for elegant closure.');
      thr.RunHot := false;
    end;

    if CloseMe then begin
      if IsSignaled(thr.evStop)
        then exit;
      //UDPDebug('Self@'+inttohex(nativeint(pointer(self)), 16)+' CloseMe found at start of SystemThread, OrderlySuspendStatus = '+booltostr(thr.OrderlySuspend)+' WasCreatedWithUserThread='+booltostr(WasCReatedWithUserThread)+' cid='+inttostr(connectionid));
      UDPDebug('Self@'+inttohex(nativeint(pointer(self)), 16)+' CloseMe found at start of SystemThread');//, Jam='+booltostr(IsSignaled(thr.evStop))+'  Jammed='+booltostr(IsSignaled(thr.evJammed))+'  UnJammed='+booltostr(IsSignaled(thr.evStopped))+' WasCreatedWithUserThread='+booltostr(WasCReatedWithUserThread)+' cid='+inttostr(connectionid));
      //evUserThread.ResetEvent;
      //evUserthread.SetEvent;

      //I DONT THINK WE SHOULD GET THIS FAR
      //if closeme is set, then that means that we're REALLY dead... we should just exit
      //the managedthread code was tightened up, so calls to Stop, should also
      //set all appropriate signals to make sure we never make it here.
      thr.RunHot := false;
//      self.Multiplexer.DeregisterEndPOint(self);
//      self.eet := nil;
      self.PrepareForDestruction(coclient);
      if CanDestroyBySystemThread then
      begin
//        GarbageCollect(self);
        self.Detach;
        thr.Endpoint := nil;

      end;
      //TPM.NoNeedthread(thr);
      exit;
//      raise ECritical.Create('We should never make it here');
    end;
    //UDPDebug('Running Idle Tasks');
    s := '';
    if TryLock then
    try
      s := DebugMEssage;
    finally
      Unlock;
    end;
    if s <> '' then
      UDPDebug(s);


    ProcessElegantClosure;
    ProcessPAcketQueue;
    AckIncoming;
    RetransOutgoing;

    ProcessKeepAliveTimer;
    EvalSystemThreadSignal; //???? Should this be allowed in this context... maybe the multiplexer should be solely responsible for evaluation
    EvalUserThreadSignal;//c# -- check readdata and write data as well

//    if AvailableData > 0 then begin
//      DoAvailableData;
//    end;

  except
    on E: Exception do begin
      UDPDebug('Exception in RUDP System Thread: '+e.Message);
      Connecting := false;
      CloseMe := true;
    end;
  end;



end;

function TReliableUDPEndpoint.SendData(p: pbyte; len: nativeint): nativeint;
//CONTEXT UserThread - ALWAYS
var
  h: TReliableUDPHeader;
  pp: pbyte;
  tm1, tm2, diff: ticker;
begin
  //create a data packet
  h.Init(self);

  result := lesserof(PAYLOAD_LIMIT, len);
  pp := GetMemory(result+sizeof(h));
  movemem32(pp, @h, sizeof(h));
  movemem32(@pp[sizeof(h)], p, result);

  if result >= len then begin
    h.Flag_AckType := ackImmediate;
  end else begin

    h.Flag_AckType := ackImmediate;
    //h.Flag_AckType := ackOutofOrder;
  end;


{$IFDEF DO_JAMMING}
  //lets figure out how long the typical pipe is
  //congested and use it
  tm1 := GetTicker;
  tm2 := tm1;
  diff := GetTimeSince(tm1, lastsendtime);
  if diff < jamtime then begin
    sleep(jamtime-diff)
  end;
  while self.txLog.Count > MAX_OUTSTANDING_TX_PACKETS do begin
    sleep(jamtime+1);
    tm2 := GetTicker;
  end;
  tm2 := GEtTimeSince(tm2,tm1);
  if (tm2 > 10)  then begin
    jamtime := tm2;
  end;
  //accelerate jamtime... else it will continue to slow forever.
  jamtime := jamtime * 60;
  jamtime := jamtime div 100;
{$ENDIF}


  Lock;
  try
    if not CloseMe then begin
      h.PrepareToSend(self);

      SendPacket(h, p, result);
      //windows.beep(1000,30);
      SystemThreadSignal := true;
      EvalSYstemThreadSignal;//<----wake up system thread to send data
    end;
  finally
    Unlock;
  end;


end;

function TReliableUDPEndpoint.WaitForAck(h: TReliableUDPHeader): boolean;

begin
  if TThread.CurrentThread = eet then
    raise Exception.create('This is not designed to be called from EET.');
  result := false;
  repeat
    result := not txLog.Has(h.sequencenumber);
    if not result then begin
      self.WaitForIncomingDataSignal(10, true);
//      Debug.Log('Note that WaitForAck is currently slow because UserAPCs are not implemented to wake thread out of sleepex()');
    end;
  until result;

end;

function TReliableUDPEndpoint.WaitForConnected(iTimeOut: nativeint): boolean;
var
  tmStart: ticker;
begin
  result := false;
  tmStart := tickcount.GetTicker;
  repeat
    if not result then begin
      self.WaitForIncomingDataSignal(1000, true);
//      Debug.Log('Note that WaitForAck is currently slow because UserAPCs are not implemented to wake thread out of sleepex()');
      if connected then begin
        result := true;
        exit;
      end;
      if tickcount.gettimesince(tmStart) > iTimeout then begin
        result := false;
        exit;
      end;

    end;
  until result;
end;

function TReliableUDPEndpoint.WaitForData(iTimeOutMs: nativeint): boolean;
var
  tmStart, tmNow: ticker;
begin
  result := true;
  tmStart := tickcount.GetTicker;
  while FAvailableData = 0 do begin
    waitforincomingdatasignal(lesserof(10000, iTimeoutMS), true);
    tmNow := tickCount.GetTicker;
    if closeme then begin
      result := false;
      exit;
    end;
    if GetTimeSince(tmNow, tmSTart) > iTimeOutMS then begin
      result := false;
      exit;
    end;
  end;
end;

procedure TReliableUDPEndpoint.RaiseDroppedError;
begin
  //if closeme then exit;

  connected := false;
  connecting := false;
  closeme := true;

  raise EReliableUDPProtocolError.Create('Connection dropped or failed.');
end;

function TReliableUDPEndpoint.WaitForIncomingDataSignal(
  iTimeOutMS: nativeint; bRAiseIfDropped: boolean): boolean;
begin



  if (not (connected or Connecting)) and (rxDataLog.count=0) then
  begin
    if bRAiseIfDropped then begin
      RaiseDroppedError;
    end else begin
      result := false;
    end;
  end;

  result := WaitForSignal(evUserDataIncoming, iTimeOUtMS);
//  result := evUserThread.WaitFor(iTimeoutMS) = wrSignaled;


end;

{ TReliableUDPDispatcher }


{ TSimpleReliableUDP }

function TSimpleReliableUDPClient.Connect: boolean;
begin
  if assigned(cli) then begin
    //cli.free;//ok
    cli.Disconnect();
    cli.Detach;
    cli._Release;
    cli := nil;
  end;
  cli := TMultiplexedUDPClient.MasterClient.CreateEndPoint;
  cli._AddRef;
  cli.Remotehost := self.HostName;
  cli.RemotePort := strtoint(self.EndPoint);
  result := cli.Connect;
end;

constructor TSimpleReliableUDPClient.Create;
begin
  inherited;

end;

destructor TSimpleReliableUDPClient.Destroy;
begin
  if assigned(cli) then begin
    cli.Disconnect();
    //cli.FreeWithReferences;
    cli.Detach;
    cli._Release;
    cli := nil;
  end;
  inherited;
end;

procedure TSimpleReliableUDPClient.Disconnect;
begin
  inherited;
  if assigned(cli) then begin
    cli.Disconnect();
    //cli.free;//ok
    cli.Detach;
    cli._Release;
    cli := nil;
  end;
end;

function TSimpleReliableUDPClient.DoReadData(buffer: pbyte; length: integer): integer;
begin
  if cli = nil then
    raise Exception.create('must be connected to to data');
  result := cli.ReadDAta(buffer, length);

end;

function TSimpleReliableUDPClient.DoSendData(buffer: pbyte; length: integer): integer;
begin
  if cli = nil then
    raise Exception.create('must be connected to send data');
  result := cli.SendDAta(buffer, length);
end;

function TSimpleReliableUDPClient.GetConnected: boolean;
begin
  result := cli <> nil;
  if result then
    result := cli.connected;

end;


function TSimpleReliableUDPClient.WaitForData(timeout: cardinal): boolean;
begin
  result := cli.WaitForData(timeout);

end;

{ TReliableUDPServer }

procedure TMultiplexedUDPEndpoint.BindToAnyPort;
begin
  self.Port := 0;
  Active := true;
end;

procedure TMultiplexedUDPEndpoint.BindToport(iPort: nativeint);
begin
  UDPDebug('About to bind to port '+inttostr(iPort));
  self.DefaultPort := iPort;
  FLocalPort := self.DefaultPort;
  Active := true;
end;

procedure TreliableUDPEndpoint.ChangeConnectionIDInLog(
  log: TReliableUDPPacketLog; iOldID, iNewID: int64);
var
  t: integer;
  r: TReliableUDPPacketLogRecord;
begin
  Lock;
  try
    for t:= 0 to log.count-1 do begin
      r := log.Items[t];
      if r.h.connectionid = iOLDID then
        r.h.connectionid := iNewID;
    end;
  finally
    Unlock;
  end;
end;


procedure TReliableUDPEndpoint.ChangeConnectionIDInLogs(iOldID, iNewID: int64);
begin
  Lock;
  try
    ChangeConnectionIDInLog(txLog, iOldID, iNewID);
    ChangeConnectionIDInLog(rxLog, iOldID, iNewID);
  finally
    Unlock;
  end;
end;

constructor TMultiplexedUDPEndpoint.CReate(AOwner: TComponent);
begin
  inherited CReate(AOwner);
  InitializeCriticalSection(sect);
  ThreadedEvent := true;
  FEndpoints := TReliableUDPEndpointList.Create;
  FEndpoints.OwnsObjects := false;



end;

function TMultiplexedUDPEndpoint.CreateEndPoint(connid: int64 = -1): TReliableUDPEndpoint;
begin
  //LOCK if you add any more shit to this
  UDPDebug('Creating Endpoint connid:'+inttostr(connid));
  if connid = -1 then begin
    result := TReliableUDPEndpoint.Create(ctClient, self);
  end else begin
    result := TReliableUDPEndpoint.Create(ctServer, self);
  end;
  //result.Multiplexer := self;     this is now set in constructor
  if connid = -1 then
    result.connectionid := NextConnectionID
  else
    result.connectionid := connid;

  result._AddRef;
  self.FEndpoints.Add(result);
  result.ReleaseInternalRef;
end;

procedure TMultiplexedUDPEndpoint.DispatchPacketToEndpoint(ifo: TReliableUDPPacketLogRecord);
var
  ep: TReliableUDPEndpoint;
begin
  Lock;
  try
    ep := FEndpoints.Find(ifo.h.connectionid);
    if assigned(ep) then begin
      ep.Lock;
      try
        if not ep.CloseMe then begin//<<--once we hit the final stages of connection closure... ALL packets are banned.
          if ifo.h.Flag_AckType = ackNone then begin
            ep.HandleIncomingPacket(ifo);
          end else begin
            ep.QueueIncomingPacket(ifo);
          end;
        end;
      finally
        ep.Unlock;
      end;
    end else begin
      Debug.Log('Could not find endpoint with connection id'+inttostr(ifo.h.connectionid));
      ifo._Release;
//      ifo.Free;
      ifo := nil;
    end;
  finally
    Unlock;
  end;
end;

function TMultiplexedUDPEndpoint.DebugConnections: string;
var
  t: integer;
  sl: TStringlist;
  ep: TReliableUDPEndpoint;
begin
  result := '';
  Lock;
  try
    FEndpoints.lock;
    try

      sl := TStringList.create;
      try

        for t:= 0 to  Self.FEndpoints.count-1 do begin
          ep := FEndpoints.items[t];

          sl.add(ep.DebugSummaryMessage);
        end;
        result := sl.Text;
      finally
        sl.free;//ok
      end;
    finally
      FEndpoints.unlock;
    end;

  finally
    Unlock;
  end;
end;

procedure TMultiplexedUDPEndpoint.DeregisterEndPOint(ep: TReliableUDPEndpoint);
var
  bHad: boolean;
begin
  Lock;
  try
    ep.Lock;
    try
      bHad := FEndpoints.Has(ep);
      FEndpoints.Remove(ep);
    finally
      ep.Unlock;
      if bHad then
        ep._Release;//<---careful not to do this under lock

    end;
  finally
    Unlock;
  end;
end;

destructor TMultiplexedUDPEndpoint.Destroy;
begin
  Detach;
  systemx.DeleteCriticalSection(sect);
  inherited;
end;

procedure TMultiplexedUDPEndpoint.Detach;
begin
  shuttingdown := true;
  while FEndpoints.Count > 0 do begin
    sleep(1000);//endpoints should clear themselves and detach properly when they see the shutting down flag
    Debug.Log('There are still '+inttostr(FEndpoints.count)+' endpoints waiting to be destroyed.');
  end;

  if assigned(FEndpoints) then begin
    FEndpoints.SafeFree;//ok
    FEndpoints := nil;
  end;

  inherited;

end;

procedure TMultiplexedUDPEndpoint.DetachAndFree;
begin
  Detach;
{$IFNDEF IOS}
  Free;
{$ENDIF}
end;

procedure TMultiplexedUDPEndpoint.DisconnectAll;
var
  t: nativeint;
begin
  Lock;
  try
    for t:= 0 to self.FEndpoints.Count-1 do begin
      Fendpoints.items[t].CloseMe := true;

    end;
  finally
    Unlock;
  end;

end;

procedure TMultiplexedUDPEndpoint.DispatchPacketToEndpoint(
  ep: TReliableUDPEndpoint; ifo: TReliableUDPPacketLogRecord);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TMultiplexedUDPEndpoint.DoDataAvailable(ep: TReliableUDPEndpoint);
begin
  if assigned(OnDataAvailable) then
    FonDataAvailable(ep);
end;

procedure TMultiplexedUDPEndpoint.DoUDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  ifo: TReliableUDPPacketLogRecord;
begin
  ifo := TReliableUDPPacketLogRecord.Create;
  ifo._AddRef;
  try
    ifo.RemoteOrigin := true;
    ifo.OriginatingHost := abinding.PeerIP;
    ifo.OriginatingPort := abinding.PeerPort;

    if assigned(ONUDPREad) then
      raise Exception.create('You probably don''t want to have OnUDPRead assigned in a '+self.ClassName);
    //get the header
    systemx.MoveMem32(@ifo.h, @Adata[0], sizeof(ifo.h));
    ifo.payload_length := length(Adata)-sizeof(ifo.h);
    if ifo.payload_length > 0 then begin
      ifo.payload := GetMemory(ifo.payload_length);
      movemem32(ifo.payload, @adata[sizeof(ifo.h)], ifo.payload_length);
    end else
      ifo.payload := nil;

    UDPDebug('Read:'+ifo.debugstring);
    try
      if ifo.h.Flag_Create and (not ifo.h.Flag_System) then
        raise EReliableUDPProtocolError.create('Cannot use the Create flag unless System flag is also set');

      //if it doesn't belong to an endpoint, service it here
      if ifo.h.Flag_System then begin
        HandleSystemPacket(ifo);
      end else begin
       //else pass the event to an endpoint
        DispatchPacketToEndpoint(ifo);
      end;

    finally
      //DO NOT FREE MEMORY    //DO NOT FREE MEMORY    //DO NOT FREE MEMORY    //DO NOT FREE MEMORY
      //FreeMemory(mem);
      //DO NOT FREE MEMORY    //DO NOT FREE MEMORY    //DO NOT FREE MEMORY    //DO NOT FREE MEMORY
    end;
  finally
    ifo._Release;
//    ifo.FreeWithReferences;
  end;


end;

function TMultiplexedUDPEndpoint.FindAvailableConnectionID: int64;
begin
  repeat
    result := (random($7FFFFFFF) shl 32)+random($7FFFFFFF);
  until not FEndpoints.Has(result);

end;

function TMultiplexedUDPEndpoint.HandleSystemCommandPacket(ifo: TReliableUDPPacketLogRecord): TReliableUDPEndpoint;
begin
  result := nil;
  case ifo.h.command of
    RUDP_SYS_CMD_CREATE: begin
      result := HandleSYstemCReatePacket(ifo);
    end;
    ERR_CHANGE_CONNID: begin
      result := HandleSystemChangeConnID(ifo);

    end;
  else
    raise Exception.Create('Unknown System Command:'+inttohex(ifo.h.command,2));
  end;
end;

function TMultiplexedUDPEndpoint.HandleSystemCommandResponsePacket(ifo: TReliableUDPPacketLogRecord): TReliableUDPEndpoint;
begin
  result := nil;
  case ifo.h.command of
    0: begin
    end;
  else
    raise Exception.Create('Unknown System Command Response:'+inttohex(ifo.h.command,2));
  end;
end;

function TMultiplexedUDPEndpoint.HandleSystemChangeConnID(ifo: TReliableUDPPacketLogRecord): TReliableUDPEndpoint;
var
  ep: TReliableUDPEndpoint;
  i1, i2: int64;
begin
  result := nil;
  lock;
  try
    i1 := 0;
    i2 := 0;
    movemem32(@i1, @ifo.payload[0], sizeof(int64));
    movemem32(@i2, @ifo.payload[8], sizeof(int64));

    FEndpoints.lock;
    try
      ep := FEndpoints.find(i1);
      if assigned(ep) then begin
        if not FEndpoints.has(i2) then begin
          UDPDEbug('Changing connection #'+inttostr(i1)+' to '+inttostr(i2));
          ep.Connected := true;
          ep.Connecting := false;
          ep.connectionid := i2;
          //ep._nextexpectedSequenceNumber := ifo.h.expected_sequencenumber;
          //ep.txLog.FreeAndClear;
          result := ep;
        end;
      end;

    finally
      FEndpoints.Unlock;
    end;
  finally
    unlock;
  end;
end;

function TMultiplexedUDPEndpoint.HandleSystemCreatePacket(ifo: TReliableUDPPacketLogRecord):TReliableUDPEndpoint;
var
  ep: TReliableUDPEndpoint;
  iold, inew: nativeint;
begin
  result := nil;
  //create the connection
  //g
//  exit;
  //if shutting down the drop all creates
  if shuttingdown then begin
    ifo._release;
    exit;
  end;



  lock;
  try
    ep := FEndpoints.FindByClientSeed(ifo.OriginatingHost, ifo.OriginatingPort, ifo.h.connectionid);
    //if we have the connection, we still should change the connection id to something else
    if ep <> nil then begin
      ep.seed := ifo.h.connectionid;
      iold := ifo.h.connectionid;
      inew := FindAvailableConnectionid;
      UDPDebug('Sending request to change connectionID from:'+inttostr(iold)+' to '+inttostr(inew));
      SendError(ep, true, ERR_CHANGE_CONNID, iold, inew, 'Connection id invalid, change please.');
      ep.connectionid := iNew;
      ep.ChangeConnectionIDInLogs(iOld, iNew);
      ep.Connected := true;
      ep.Connecting := false;
      result := ep;
      ep.SayHello;
    end else
    //ELSE we will create a new connection, change the connection id and then send the new connectionid
    begin
      ep := self.CreateEndPoint(ifo.h.connectionid);
      EP.seed := ifo.h.connectionid;
      ep.DestroyOnClose := true;
      ep.Remotehost := ifo.OriginatingHost;
      ep.RemotePort := ifo.Originatingport;
      iold := ifo.h.connectionid;
      inew := FindAvailableConnectionid;

      UDPDebug('Sending request to change connectionID from:'+inttostr(iold)+' to '+inttostr(inew));
      SendError(ep, true, ERR_CHANGE_CONNID, iold, inew, 'Connection id invalid, change please.');
      ep.connectionid := iNew;
      ep.ChangeConnectionIDInLogs(iOld, iNew);
      ep.Connected := true;
      ep.Connecting := false;
      result := ep;
      ep.SayHello;


//      FEndpoints.Add(ep);
    end;
  finally
    unlock;
  end;

end;

function TMultiplexedUDPEndpoint.HandleSystemPacket(ifo: TReliableUDPPacketLogRecord): TReliableUDPEndpoint;
begin
  result := nil;
  if ifo.h.Flag_Command then begin
    result := HandleSYstemCommandPacket(ifo);
  end else
  if ifo.h.Flag_CommandResponse then begin
    result := HandleSYstemCommandResponsePacket(ifo);
  end;


  //even though the "system" level handled it,
  //we still pass it to the endpoint so that it can be put through the
  //standard ACK process
  Lock;
  try
    if ifo.h.Flag_AckType <> ackNone then begin
      //if the earlier dispatchers didn't return an endpoint that handled the packet
      if result = nil then
        //search for one the traditional way (not typical in this instance)
        result := FEndpoints.Find(ifo.h.connectionid);

      if assigned(result) then begin
        result.QueueIncomingPacket(ifo);
      end else begin
        ifo._Release;
        ifo := nil;
      end;
    end else begin
      ifo._Release;
      ifo := nil;
    end;
  finally
    Unlock;
  end;
end;

procedure TMultiplexedUDPEndpoint.Lock;
begin
  systemx.EnterCriticalSection(sect);
end;

function TMultiplexedUDPEndpoint.NextConnectionId: int64;
begin
  Lock;
  try
    //keep looping until we find a connectionid we don't have
    repeat
      result := _nextconnectionid;
      inc(_nextconnectionid);
    until not FEndpoints.Has(result);
  finally
    Unlock;
  end;
end;

function TReliableUDPEndpoint.ReadData(p: pbyte; len: nativeint): nativeint;
//CONTEXT: USER THREAD - ALWAYS
var
  ifo: TReliableUDPPacketLogRecord;
  iCanRead: nativeint;
begin
  result :=0;
  if rxDAtaLog.Count = 0 then exit;

  Lock;
  try
    if rxDAtaLog.Count = 0 then exit;//check again under lock

    ifo := rxDataLog.items[0];
    iCanRead := ifo.payload_length - ifo.payload_read_index;
    iCanRead := lesserof(iCanRead, len);
    result := iCanRead;

    movemem32(p, @ifo.payload[ifo.payload_read_index], iCanRead);
    FAvailableData := FAvailableData - iCanRead;
    ifo.payload_read_index := ifo.payload_read_index + iCanRead;

    if (ifo.payload_read_index=ifo.payload_length) then begin
      rxDataLog.Delete(0);
      if (ifo.needsack and ifo.acked) then begin
//        rxLog.Remove(ifo);
        ifo._Release;
//        ifo.Free;
        ifo := nil;
      end;
    end;
    EvalUserThreadSignal;

  finally
    Unlock;
  end;


end;



procedure TReliableUDPEndpoint.ResetKeepAliveTimer;
begin

  if assigned(multiplexer) and multiplexer.shuttingdown then exit;

  tmLastSeen := tickcount.GetTicker;

end;

procedure TMultiplexedUDPEndpoint.SendDataFromEndPoint(ep: TReliableUDPEndPOint; h: TReliableUDPHEader; mem: PByte; memlen: nativeint);
var
  buf: TIdBytes;
begin

  if ep=nil then
    raise Exception.Create('ep can''t be nil');
  setlength(buf, memlen + sizeof(h));
  movemem32(@buf[0], @h, sizeof(h));
  if memlen > 0 then begin
    movemem32(@buf[sizeof(h)], mem, memlen);
  end;

  UDPDebug('Send Data to '+ep.Remotehost+':'+inttostr(ep.RemotePort)+' from '+inttostr(  ep.Multiplexer.FBindings[0].Port));
  try
    self.SendBuffer(ep.RemoteHost, ep.RemotePort, buf);
  except
    on E: Exception do begin
      ep.Error := E.message;
//      ep.CloseMe := true;
//      ep.DisconnectStarted := true;
//      ep.connected := false;
//      ep.connecting := false;
//      ep.timedout := true;
    end;
  end;

end;

procedure TMultiplexedUDPEndpoint.SendError(ep: TReliableUDPEndpoint; bSystem: boolean; b: byte; data1, data2: int64;
  diagnostic: string);
var
  h: TReliableUDPHeader;
  a: ansistring;
  p: pbyte;
  l: nativeint;
begin
  Lock;
  try
    h.Init(ep);
    if ep<> nil then
      h.connectionid := ep.connectionid;

    h.Flag_System := bSystem;
    h.Flag_AckType := ackNone;
    h.FLAG_Command := true;
    h.command := ERR_CHANGE_CONNID;       //todo 2: this should not be here... should be outside function

    a := diagnostic;
    l := sizeof(data1)+sizeof(data2)+length(diagnostic);

    p := GetMemory(l);
    movemem32(@p[0], @data1, sizeof(data1));
    movemem32(@p[sizeof(data1)], @data2, sizeof(data2));
    movemem32(@p[sizeof(data1)+sizeof(data2)], @a[STRZ], length(a));


    h.PrepareToSend(ep);
    ep.SendPAcket(h, p, l);//<---takes ownership
  finally
    Unlock;
  end;
//  SendDataFromEndPoint(ep, h, p, l);//<---takes ownership

end;

procedure TMultiplexedUDPEndpoint.Unlock;
begin
  systemx.LeaveCriticalSection(sect);
end;

{ TReliableUDPPacketLogRecord }


constructor TReliableUDPPacketLogRecord.Create(owningEndPOint: TReliableUDPEndpoint; h: TReliableUDPHeader; p: pbyte;
  len: nativeint);
begin
  Create(owningendpoint);
  self.h := h;
  payload := p;
  payload_length := len;
  sendtime := tickcount.GetTicker;
  randomNagleTimer := random(500);
  self._Release;


end;

constructor TReliableUDPPacketLogRecord.Create(
  owningEndPOint: TReliableUDPEndpoint);
begin
  inherited Create;
  self.owningEndPoint := owningendpoint;
  randomNagleTimer := random(500);
end;

function TReliableUDPPacketLogRecord.DebugString: string;
begin
  result := h.DebugString + '_PAY_'+inttostr(payload_length);
  result := '[ref:'+inttostr(_RefCount)+']'+result;
  if RemoteOrigin then
    result := '>.<'+result
  else
    result := '<.>'+result;
end;

procedure TReliableUDPPacketLogRecord.DeregisterFromEndpoint;
begin
  if owningendpoint <> nil then begin
    if owningendpoint.rxLog.Has(self) then begin
      owningendpoint.rxLog.Remove(self);
      _Release;
    end;
    owningendpoint.txLog.Lock;
    try
      if owningendpoint.txLog.Has(self) then begin
        owningendpoint.txLog.Remove(self);
        _Release;
      end;
    finally
      owningendpoint.txLog.Unlock;
    end;
    if owningendpoint.rxDataLog.Has(self) then begin
      owningendpoint.rxDataLog.Remove(self);
      _Release;
    end;
    if owningendpoint.rxQueue.Has(self) then begin
      owningendpoint.rxQueue.Remove(self);
      _Release;
    end;
  end;

end;

destructor TReliableUDPPacketLogRecord.Destroy;
begin
  DeregisterFromEndpoint;
  UDPDebug('+FREEING: '+DebugString);
  if payload <> nil then begin
    freememory(payload);
    payload := nil;
  end;
  inherited;
end;

procedure TReliableUDPPacketLogRecord.Detach;
begin

  UDPDebug(DebugString+' wants to detach.');
  if assigned(owningEndPoint) then begin
    UDPDebug(self.owningEndPoint.DebugMessage);
  end;
  DeregisterFromEndPOint;

  inherited;

end;

function TReliableUDPPacketLogRecord.GetIndexValue: int64;
begin
  result := h.sequencenumber;
end;

function TReliableUDPPacketLogRecord.NeedsAck: boolean;
begin
  result := h.Flag_AckType <> ackNone;
end;

function TReliableUDPPacketLogRecord._AddRef: Integer;
begin
  UDPDebug('Adding Ref to:'+self.DebugString);
  result := inherited;
  UDPDebug('Referenced:'+self.DebugString);
end;

function TReliableUDPPacketLogRecord._Release: Integer;
begin
  UDPDebug('Releasing:'+self.DebugString);
  result := inherited;

end;

{ TMultiplexedUDPClient }



class procedure TMultiplexedUDPClient.FinalizeClass;
begin
  LockClass;
  try
    if CFMasterClient <> nil then begin
      try
        CFMasterClient.DisconnectAll;
        CFMasterCLient.Free;//ok
        CFMasterClient := nil;
      except
      end;
    end;
  finally
    UnlockClass;
  end;
  DeleteCriticalSection(class_sect);
end;

class procedure TMultiplexedUDPClient.InitClass;
begin
  InitializeCriticalSection(class_sect);
end;

class procedure TMultiplexedUDPClient.LockClass;
begin
  EnterCriticalSection(class_sect);

end;

class function TMultiplexedUDPClient.MasterClient: TMultiplexedUDPClient;
begin
  result := nil;
  if CFMasterclient <> nil then begin
    result := CFMasterClient;
    exit;
  end;

  LockClass;
  try
    if CFMasterClient = nil then begin
      CFMasterCLient := TMultiplexedUDPClient.Create(nil);
    end;
    CFMasterClient.BindToport(0);
    CFMasterClient.Active := true;
    RESULT := CFMasteRClient;

  finally
    UnlockClass;
  end;
end;

class procedure TMultiplexedUDPClient.UnlockClass;
begin
  LeaveCriticalSection(class_sect);
end;

{ TSimpleReliablePrivateServerEndpoint }

function TSimpleReliablePrivateServerEndpoint.Connect: boolean;
begin
  result := false;
  raise ETransportError.create('Connect is irrelevant for this connection');
end;

destructor TSimpleReliablePrivateServerEndpoint.Destroy;
begin
  //
  inherited;
end;

procedure TSimpleReliablePrivateServerEndpoint.Detach;
begin
  inherited;
  //
end;

procedure TSimpleReliablePrivateServerEndpoint.Disconnect;
begin
  cli.Disconnect;

  inherited;

end;

function TSimpleReliablePrivateServerEndpoint.DoReadData(buffer: pbyte;
  length: integer): integer;
begin
  result := cli.ReadDAta(buffer, length);
end;

function TSimpleReliablePrivateServerEndpoint.DoSendData(buffer: pbyte;
  length: integer): integer;
begin
  result := cli.SendDAta(buffer, length);
end;

function TSimpleReliablePrivateServerEndpoint.GetConnected: boolean;
begin
  result := cli.connected;

end;

function TSimpleReliablePrivateServerEndpoint.WaitForData(
  timeout: cardinal): boolean;
begin
  result := cli.WaitForData(timeout);
end;


procedure oinit;
begin
  TMultiplexedUDPClient.InitClass;
end;

procedure ofinal;
begin
  TMultiplexedUDPClient.FinalizeClass;
end;


{ TRUDPDebugInfo }

procedure TRUDPDebugInfo.Init;
var
  s: ansistring;
begin
  threadid := TThread.CurrentThread.ThreadID;
  s := systemx.DLLName;
  s := extractfilename(s);
  movemem32(@self.module[0], @s[STRZ], length(s)+1);



end;

function TRUDPDebugInfo.ToString: string;
begin
  result := 'threadid='+inttostr(threadid)+':'+module;
end;

{ TRUDPSystemThread }

procedure TRUDPSystemThread.DoExecute;
begin
  inherited;
  if Endpoint = nil then
    raise ECritical.create('wtf, yo... nil');
  endpoint.ThreadExecute(self);
end;

{ TRUDPUserThread }

procedure TRUDPUserThread.DoExecute;
begin
  inherited;
  if Endpoint = nil then
    raise ECritical.create('wtf, yo... nil');
  Endpoint.UserThreadExecute(self);
end;

{ TRUDPEndpointThread }

procedure TRUDPEndpointThread.Detach;
begin
  EndPoint := nil;
  inherited;
end;

procedure TRUDPEndpointThread.PrepareForPool;
begin
  inherited;
  Endpoint := nil;
end;

procedure TRUDPEndpointThread.SetEndpoint(const Value: TReliableUDPEndpoint);
begin
  if value = FEndpoint then
    exit;

  if assigned(FEndpoint) then
    FEndpoint._Release;

  FEndpoint := Value;

  if assigned(FEndpoint) then
    FEndpoint._AddRef;


end;

{ TReliableUDPPacketLog }

constructor TReliableUDPPacketLog.Create;
begin
  inherited;
//  Debug.Log('log Created '+inttohex(int64(pointer(self)), 16));
end;

destructor TReliableUDPPacketLog.Destroy;
begin
//  Debug.Log('log Destroyed '+inttohex(int64(pointer(self)), 16));
  inherited;
end;

{ TReliableUDPEndpointList }

function TReliableUDPEndpointList.FindByClientSeed(sRemotehost: string;
  remoteport: ni; iSeed: int64): TReliableUDPEndpoint;
var
  i: nativeint;
  rep: TReliableUDPEndpoint;
begin
  result := nil;
  Lock;
  try
    for i := 0 to count-1 do begin
      rep := FList[i];
      if (rep.Seed = iSeed) and (rep.FRemotePOrt = remoteport) and (rep.FRemoteHost = sremotehost) then begin
        result := rep;
        break;
      end;
    end;


  finally
    Unlock;
  end;
end;

initialization

orderlyinit.init.RegisterProcs('SimpleReliableUDP', oinit, ofinal, 'ManagedThread,better_indy');


finalization


end.
