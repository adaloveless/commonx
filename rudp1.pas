unit rudp1;
{xDEFINE USE_FIBERS}
{$INLINE AUTO}
{$DEFINE USE_SHARED_QUEUES}
{$DEFINE SHARED_MULTIQUEUES}
{$DEFINE ENDPOINT_REFERENCES}
{$DEFINE FLOODCHECK}
{x$DEFINE EXTRA_DEAD_CHECKS}
{$DEFINE SEPARATE_SEND_LOCK}
{x$DEFINE DO_INTERVALS}
{x$DEFINE VERBOSE_REFERENCES}
{x$DEFINE USE_REGISTERED_MEMORY}

//{$Message Error 'I forgot... definition of LOOP changed.  I still need to be able to handle "one shot" threads.... the definition of LOOP is going to be all squirley... fuck!'}
//todo 1: Since ChangeConnID is sent without requiring ACK, who is to say if it isn't lost?  Special case, retrans?
//todo 1: OnData... separate out immediate queue from regular queue.

//IMPLEMENT
//SplitSend
//SplitReceive
// - SplitPrepare


//PROVE
//[x] No memory leaked
//[ ] no memory double frees
//[ ] Threadsafe [ ] Called From Single Thread [ ] Locks employed
{x$DEFINE SHORT_KEEP_ALIVE}


//To Achieve C# Parity
//[ ] Add Seed and Secret to Packet Header
//[ ] Add FindByClientSeed and FindBySeed to the endpoint list.
//[x] Add NextSeed to Multiplexer
//[x] Get Next Seed when connection is created
//[ ] Save Secret when Change ConnectionID request comes in
//[ ] Save Secret when Connection is creaeted at server end.
//[ ] Change keepalive to be more aggresssive if keep alive was not received from other end.


{x$DEFINE BEEPS}
{x$DEFINE ALLOW_IMMEDIATE_RETRANS}
{x$DEFINE SLOW_ACK}
{x$DEFINE SEND_RECEIVE_DEBUG}
{x$DEFINE PERIODIC}
{x$DEFINE STALL_EMPHASIS}
{x$DEFINE ALWAYS_USE_SENDTXLOGS}
{x$DEFINE INTERVAL_SPIN}
{x$DEFINE ALLOW_PACKET_MERGE}
{$DEFINE QUEUE_INCOMING_UDP}
{$DEFINE STRUCTURED_INCOMING_UDP}
{x$DEFINE DEBUG_RETRANS}
{x$DEFINE UDP_DEBUG}
{$DEFINE MTU_TESTS}
{x$DEFINE MULTI_QUEUE_IN}
{x$DEFINE FORCE_SMALL_MTU}

{x$DEFINE REF_DEBUG}
{x$DEFINE MORE_REFS}
{x$DEFINE MORE_REFS2}
{x$DEFINE USE_FEC}
{$DEFINE FUTURE_ACKS} //causes Internal error C2359 in 32-bit!
{x$DEFINE NEVER_WALK_DOWN_MTU}
{$DEFINE DUMB}
{x$DEFINE CLEAR_DEBUG}
{$DEFINE OUT_QUEUE}
{$DEFINE HIGH_PRIORITY_QUEUES}
{x$DEFINE ALWAYS_SPLIT_ASS}
{x$DEFINE PACKET_COMBINATION}
{x$INLINE AUTO}
{x$DEFINE SEND_RESETS}
{x$DEFINE RESPOND_TO_RESETS}


//[I have 16 packets for you][here's the first]
//[got the first packet][my buffer is underrrun]
//[have two more]
//[got 2-3][my buffer is still underrun][time between 2-3 was ... ]
//[here's 4-8] at the interval you specified]
//[got 4-8][my buffer is still underrun][time between 4-8 was...]
//[heres 8-16 at your interval]
//[interval expired after 12][resend 12, 14]







//A Neuro Approach---
//----------------------
//Controls:
//- Time Between Bursts - [0][1][10][100]
//- Packet Burst Size
//- Write-Ahead distance
//Sensors:
//- Clear-Rate
//- Ack-Time
//- Retrans-Ratio

//IF ack-time increases from ping, then there's congestion
// - Disable Write Ahead
// - Increase time between retransmissions
//If clear-rate falls below maximum
// - Decrease time between retransmissions
// - Increase write-ahead
// -


interface

{$include DelphiDefs.inc}
uses
  ringstats,
{$IFDEF WINDOWS}
  idwinsock2,
{$ENDIF}
  btree,
{$IFDEF USE_FEC}
  fec,
{$ENDIF}
  signals, fiber, commands_system, system.syncobjs, numbers, systemx, debug, classes, typex, persistentinterfacedobject, standardlist, netbytes, sysutils, tickcount, betterobject, generics.collections.fixed, SimpleAbstractConnection, managedthread, sharedobject, idglobal, betteridudpserver, betteridsockethandle, orderlyinit, stringx, better_indy, simplequeue, idtcpserver, idcontext, helpers.indy;



const
  MAX_SPLIT_PARTS = 256;
  MTU_TEST_SIZES : array of ni =   [390,1250,2000, 3000, 4000,8500, 9500, 10500, 11500, 12000,16000,24000,32000,48000,64000];
//W  MTU_TEST_PING_REQ: array of ni=[99990,9999,  99,   99,   99,  99,   99,    99,    99,    99,    5,    4,    3,    2,    1];
  MTU_TEST_PING_REQ: array of ni=[99990,9999,  15,   14,   13,  9,    8,     7,     7,     6,    5,    4,    3,    2,    1];


  FLOODED_AT_MIN = 4;
  FLOODED_AT_MAX = 40;
  MTU_TEST_COUNT = 15;//<-----------^
  MTU_TEST_EXPIRATION_TIME = 90000;
  MTU_RETEST_TIME = 10000;
  STALE_RETRANS_TIME = 200;
  MAX_STALE_RETRANS_TIME = 500;
  INITIAL_PACKET_WRITE_AHEAD = 0;
  STALL_WALK_INC = 0.5;
  STALL_WALK_DEC = 0.5;
  WA_SHIFT = 5;
  MIN_PACKET_WRITE_AHEAD = 4; //
  MAX_PACKET_WRITE_AHEAD = 20; //
  MAX_OUTSTANDING_PACKETS = MAX_PACKET_WRITE_AHEAD+1;//this should probably be low because, setting it too high will make changes to the MTU size less responsive
  DEFAULT_RETRANS_TIMEOUT = 200;
  MIN_MTU = 1400;//<<--only used for FEC stuff, cutting into parts
  SINGLE_RETRANS_TIMEOUT = 500;
  WRITE_AHEAD_PING_DIVISOR = 1;
  WRITE_AHEAD_MULTIPLIER = 30;
  RETRIES_BEFORE_SPLIT = 8;
{$IFDEF USE_FEC}
  PAYLOAD_LIMIT = 400*8;
  CHAIN_SIZE_LIMIT = 400*8;
{$ELSE}
  PAYLOAD_LIMIT = 8400;
  MAX_PARTS = 1;
  CHAIN_SIZE_LIMIT = 8400;
{$ENDIF}
{$IFDEF SMALL_MTU}
  DEFAULT_MTU = 390;
{$ELSE}
  DEFAULT_MTU = 16000;
{$ENDIF}
  MAX_MTU = 9000;
//  DEFAULT_MTU = 1350;
//  DEFAULT_MTU = 3400;

  RUDP_SYS_CMD_CREATE = 1;
  RUDP_SYS_CMD_CONFIRM_BIDIR = 2;
  RUDP_SYS_CMD_MTU_TEST = 3;

  CMD_FUTURE_ACK = 8;
  CMD_ACK = 3;
  CMD_RESET = 13;
  CMD_NOTHING_TO_SEND = 9;
  CMD_MTU_SUCCESS = 11;
  CMD_MTU_TEST = 10;
  CMD_SPLIT = 12;
  CMD_KEEP_ALIVE = 5;
  RUDP_CMD_CLOSE = 4;
  RUDP_CMD_DEBUGINFO = 6;
  RUDP_CMD_REQUEST_RETRANS = 7;
  ERR_CHANGE_CONNID = $81;
{$IFDEF SHORT_KEEP_ALIVE}
  CONNECTION_TIMEOUT = 8000;
{$ELSE}
  CONNECTION_TIMEOUT = 60000;
  INITIAL_CONNECTION_TIMEOUT = 12000;
{$ENDIF}


type
{$IFNDEF USE_FEC}
  TFECReport = record
  end;
  PFECReport = PByte;
{$ENDIF}
  TMTUTestResult = record
    sz: ni;
    lasttest: ticker;
    lastsuccess: ticker;
    procedure MarkSuccessful;
    function CanUse: boolean;
    function TimeToRetest: boolean;
    procedure REset;//<--call this if route changes (future feature)

  end;
  PMTUTestResult = ^TMTUTestResult;

  TCloseOrigin = (coNotClosed, coClient, coServer, coDestructorClient, cdDestructorServer);
  TCloseState = (csNotClosed, csWaitingForPackets, csTimedOut, csClosed, csError, csReset);
  TConnectionType = (ctClient, ctServer);
  TRUDPDebugInfo = packed record
    threadid: int64;
    module: array[0..200] of byte;
    procedure Init;
    function ToString: string;
  end;

  TRUDPSplitHeader = packed record
    packet: byte;
    packetof: byte;
    totalsize: word;
    startbyte: word;
    bytecount: word;
    sequencenumber:int64;
  end;
  PRUDPSplitHEader = ^TRUDPSplitHEader;

  TSplitAssembly = record
    seq: int64;
    uniquePartsIn: ni;
    totalparts: ni;
    partsavailable: array[0..MAX_SPLIT_PARTS-1] of boolean;
    paytemp: array[0..65535] of byte;
    procedure Init;
    function FlagPArtCompleteAndReportCompletion(partid: ni): boolean;
  end;

  EReliableUDPProtocolError = class (Exception);
  ECritical = class(Exception);

  TReliableUDPEndpoint = class;//forward;
  TEndPOintInputQueueItem = class;//forward

  TRUDPEndpointThread = class(TManagedThread)
  private
    FEndpoint: TReliableUDPEndpoint;
    procedure SetEndpoint(const Value: TReliableUDPEndpoint);
  public
    procedure Init;override;
    procedure Detach;override;
    procedure PrepareForPool;override;
    property Endpoint: TReliableUDPEndpoint read FEndpoint write SetEndpoint;
  end;

  TRUDPEndPointFiber = class(TFiber)
  private
    FEndpoint: TReliableUDPEndpoint;
    procedure SetEndpoint(const Value: TReliableUDPEndpoint);
  public
    procedure Detach;override;
    property Endpoint: TReliableUDPEndpoint read FEndpoint write SetEndpoint;
  end;



{$IFDEF USE_FIBERS}
  TRUDPSystemThread = class(TRUDPEndPointFiber)
{$ELSE}
  TRUDPSystemThread = class(TRUDPEndPointThread)
{$ENDIF}
  public
    procedure DoExecute;override;
  end;

  TRUDPUserThread = class(TRUDPEndpointThread)
  public
    procedure DoExecute;override;
  end;

  TAckType = (ackNone, ackOutofOrder, ackOnTimeout, ackImmediate);

  TMultiplexedUDPEndpoint = class;//forward



  TALHeader = packed record
    sz: smallint;
  end;

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
    function GetIsRepack: boolean;
    procedure SetIsRepack(const Value: boolean);
  public
    tag: array[0..1] of byte;
    flags: byte;
    command: byte;
    tcid: cardinal;
    fcid: cardinal;
    sequencenumber: int64;
    seed: int64;
    expected_sequencenumber: int64;
    argument: int64;
    parts: byte;
    {$IFDEF DUMB}SendersBackLog: byte;{$ENDIF}
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
    property IsRepack: boolean read GetIsRepack write SetIsRepack;

  end;

  //--------------------------------------------------------------------------


  //--------------------------------------------------------------------------
  TReliableUDPPacketLogRecord = class(TSharedObject, IIndexable<int64>)
  strict private
    owningEndPoint: TreliableUDPEndpoint;
  protected
    destroying: boolean;
  public
    RemoteOrigin: boolean;
    OriginatingHost: string;
    OriginatingPort: nativeint;
    sendtime: ticker;
    sendtimetag: ni;
    acked: boolean;
    acktime: ticker;
    queue_handled: boolean;
    h: TReliableUDPHeader;//takes ownership of
    payload: PByte;
    sent: boolean;
    resent: boolean;
    retranscount: ni;
    LOOPtoken_send, looptoken_resend: ticker;
    payload_length: nativeint;
    payload_read_index: nativeint;
    retranstime: ticker;
    watchme: boolean;
    waschopped: boolean;
{$IFDEF USE_FEC}
    health: TFECReport;
{$ENDIF}
    constructor Create;overload;override;
    constructor Create(owningEndPOint: TReliableUDPEndpoint);reintroduce;overload;virtual;
    constructor Create(owningEndPOint: TReliableUDPEndpoint; h: TReliableUDPHeader; p: pbyte; len: nativeint);reintroduce;overload;virtual;
    destructor Destroy;override;
    procedure Detach;override;

    function GetIndexValue: int64;
    property IndexValue: int64 read GetIndexValue;
    function NeedsAck: boolean;
    function DebugString: string;
    procedure DeregisterFromEndpoint;
    function CompleteSize: ni;
{$IFDEF REF_DEBUG}
    function _AddRef: integer;override;
    function _Release: integer;override;
{$ENDIF}

  end;

  TPacketQueueItem =  class(TQueueItem)
  private
    procedure SetEP(const Value: TMultiplexedUDPEndpoint);
  protected
    Fep: TMultiplexedUDPEndpoint;
    procedure DoExecute; override;
  public

    Data: PByte;
    DataSize: ni;
    PeerIP: string;
    PeerPort: TIdPort;
{$IFDEF USE_FEC}
    health: TFECReport;
{$ENDIF}
    procedure Init;override;
    procedure Detach; override;
    property ep: TMultiplexedUDPEndpoint read FEp write SetEP;

  end;

  TRUDPPipeInQueue = class(TMultiQueue)
  public

  end;



  //--------------------------------------------------------------------------
  TReliableUDPPacketLog1 = class(TStandardList<int64, TReliableUDPPacketLogRecord>)
  //keeps track of packets
  public
    constructor Create;override;
    destructor Destroy;override;
  end;

  TtreeItem_ReliableUDPPacketLogRecord = class(TBTreeItem)
  public
    packet: TReliableUDPPacketLogRecord;
    function Compare(const [unsafe] ACompareTo:TBTreeItem):ni; override;
  end;

  TReliableUDPPacketLog2 = class(TBTree)
  public
    function Find(iSeqNumber: int64): TtreeItem_ReliableUDPPacketLogRecord;
    procedure Add(ifo: TReliableUDPPacketLogRecord);reintroduce;
    procedure Remove(ifo: TReliableUDPPacketLogRecord);
    function Has(iSeqNumber: int64): boolean;overload;
    function Has(ti: TtreeItem_ReliableUDPPacketLogRecord): boolean;overload;
    function Has(ifo: TReliableUDPPacketLogRecord): boolean;overload;


  end;

  TReliableUDPPacketLog = TReliableUDPPacketLog2;



  TRUDPOutQueueItem =  class(TQueueItem)
  private
    procedure SetEP(const Value: TReliableUDPEndpoint);
  protected
    Fep: TReliableUDPEndpoint;
    procedure DoExecute; override;
  public

    data: PByte;
    datalen: ni;
    PeerIP: string;
    PeerPort: ni;
    seq: ni;
    fec: ni;
    qt: ticker;
    procedure Init;override;
    procedure Detach;override;
    property ep: TReliableUDPEndpoint read Fep write SetEP;
  end;

  TRUDPOutQueue = class(TMultiQueue)
  public
{$IFNDEF USE_SHARED_QUEUES}
    ep: TReliableUDPEndPOint;
{$ENDIF}
    ticks_per_byte: double;
    lasttxtime: ticker;
    lasttxbytes: ni;
    function QuotaAVailable: boolean;
{$IFNDEF USE_SHARED_QUEUES}
    function GetNextItem: TQueueItem; override;
{$ENDIF}
    procedure SpeedUp;
    procedure SlowDown;
    procedure Init;override;
    procedure QueueSendData(ep: TReliableUDPEndpoint; basetm: ticker;AHost: string; Aport: ni; data: TIDBytes; seq, fec: ni);
    procedure QueueSendData2(ep: TReliableUDPEndpoint; basetm: ticker;AHost: string; Aport: ni; data: PByte;  sz: ni);
  end;


  //--------------------------------------------------------------------------
  TReliableUDPEndpoint = class(TPersistentInterfacedObject,IIndexable<int64>)
  private
    FdebugTag: string;
    FCloseorigin: TCloseOrigin;
    FCloseState: TCloseState;
    FRemotePOrt: nativeint;
    FRemoteHost: string;
    FLocalPort: nativeint;
    [weak] FMultiplexer: TMultiplexedUDPEndpoint;
    FUserThreadSignal: boolean;
    FSYstemThreadSignal: boolean;
    FKeepAliveInterval: nativeint;
    FKeepAliveTimeout: nativeint;
    FKeepAliveStagger: nativeint;
    FDestroyOnClose: boolean;
    bSaidHello: boolean;
    SynchronousDestroy: boolean;
    FevUserDataIncoming: TSignal;
    LastACkTime: ticker;
    FOnDataAvailable: TNotifyEvent;
    FIsServer: boolean;
    txAckLevel: int64;
    txAckLevel_AtLastTX: int64;
    lastTXLoopTime: ticker;
    rxAckedLast: int64;
    lastSwingAroundTime: int64;
    lastInComingAckTime: ticker;
    lastIncomingPeriodicAckTime: ticker;
    txTemp: array[0..MAX_MTU-1] of byte;
    connect_acked: boolean;
    FMTU: ni;
    FWalkingMTU: ni;
    function GetIndexValue: int64;
    procedure QueueIncomingPacket(ifo: TReliableUDPPacketLogRecord);
    procedure SendAck(specific_packet: int64; health: PFECReport);
    procedure SendFutureAck(packets: TDynInt64Array);
    procedure ChangeConnectionIDInLog(log: TReliableUDPPacketLog; iOldID,
      iNewID: int64; from: boolean);
    procedure SendKeepAlive(bRequestAcknowledgement: boolean);
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
    function GEtDebugTag: string;
    procedure SetDebugTag(const Value: string);
    procedure REquestImmediateRetransmission;
    procedure RetransImmediate(iExpectedSequenceNumber: int64);
    procedure AckFutureGaps;
    procedure RequestRetransmissionOfFutureGaps;
    procedure CalcNewWalkingAverage;
    procedure SendTxLogs(bOnlyUnsent: boolean; bCombine: boolean);
    function GetConnidDebug: string;
    function CheckClosed: boolean;
    function GEtFlexMTU: ni;
    function GEtFecSize: ni;
    procedure SendTxLogs_Complex(bOnlyUnsent, bCombine: boolean);
    procedure SendTxLogs_Simple(bOnlyUnsent, bCombine: boolean);
    function AnyTXPacketsTimedOut: boolean;
    procedure WalkFlexMTU(bPacketLoss: boolean);
    function GetWalkingAverage: ni;inline;
    function GetFlexPayloadLImit: ni;
    function GetQueueTime: ticker;
    procedure SetQueueTime(const Value: ticker);
    procedure SendNothingToSend();
    procedure SendMTUTEst(sz: ni);
    procedure SendMTUTEstREsult(sz: ni);
    procedure SplitPrepareReceive(seq: int64; totalParts: ni);
    procedure SetCloseState(const Value: TCloseState);
    procedure SetRemoteHost(const Value: string);
  strict protected
    lastReorderRequest: ticker;
    lastsendtime, lastSendTimeHR, jamtime: ticker;
    WaitForNothing: boolean;
    stallwalk: single;
    splitass: TSplitAssembly;
    mtu_test_results: array[0..MTU_TEST_COUNT-1] of TMTUTestResult;
    procedure UpdateMTUTestSuccesses(sz: ni);
    function GetMTUTest(mtu: ni): PMTUTestResult;
    function INdexOfMTUTest(mtu: ni): ni;
    procedure CheckAndSendMTUTest;
    procedure HandleIncomingCommandPacket(ifo: TReliableUDPPacketLogRecord);
    procedure HAndleFutureAck(ifo: TReliableUDPPacketLogRecord);
    procedure HandleSplitReceive(ifo: TReliableUdPPacketLogRecord);
    procedure HandleMTUTestResult(ifo: TReliableUDPPacketLogRecord);
    procedure HandleMTUTest(ifo: TReliableUDPPacketLogRecord);
    procedure HandleIncomingDataPacket(ifo: TReliableUDPPacketLogRecord);
    procedure HandleIncomingSystemPacket(ifo: TReliableUDPPacketLogRecord);
    procedure SEtUserThreadSignal(b: boolean);
    procedure SEtSystemThreadSignal(b: boolean);
    function GetUserThreadSignal: boolean;
    function GetSystemThreadSignal: boolean;
    function CanDestroyBySystemThread: boolean;
  public
    Fqueuetime: ticker;
    queue_out: TRUDPOutQueue;
    queue_pipe_in: TRUDPPipeInQueue;
    WAlkingAverageFine: ni;
    LocalDebugInfo,RemoteDebugInfo: TRUDPDebugInfo;
    tmLAstSeen: ticker;
    tmLAstKeepAliveSent: ticker;
    connected: boolean;
    TOconnectionid: cardinal;
    FROMConnectionId: cardinal;
    DisableAcks: boolean;
    totalTXData: int64;
    totalRXData: int64;
    totalRX: int64;
    totalTX: int64;
    spins: int64;

    eet: TRUDPSystemThread;
    threadpointerlock: TCLxCriticalSection;
    user_thread: TRUDPUserThread;
    _nextexpectedSequenceNumber: int64;
    _nextsequencenumber: int64;
    _nextdataSequence: int64;
    _nextQueuedSequenceNumber: int64;
    txLog: TReliableUDPPacketLog;
    rxLog: TReliableUDPPacketLog;
    rxQueue: TReliableUDPPacketLog;
    rxDataLog: TReliableUDPPacketLog;
    retrans_timeout: ticker;
    retransRequestTime: ticker;
    random_retrans: ticker;
    finalsequencenumber: int64;
    FAvailableData: nativeint;
    fLastDebugMessage: string;
    FCloseMe: boolean;
    retransmissions: int64;
    transmissions: int64;
    outoforder: int64;
    SentToGarbage: boolean;
    Connecting: boolean;
    TimedOut: boolean;
    WasCreatedWithUserThread: boolean;
    seed: int64;
    secret: int64;
    thr: TRUDPSystemThread;
    rsIntervalStability, rsAckTime,rsRetransRatio: TRingStats;
    systemthreadtime: ticker;
    packetsOut: int64;
    packetsOutCheckTime: ticker;
    retransLoopCheckTime: ticker;
    retransloops: int64;
    rsRetransLoopRate: TRingStats;
    sendinterval: ticker;
    lastsend: ticker;
    systm_RetransLoop: ticker;
    systm_KeepAlive: ticker;
    systm_Queue: ticker;

    lastStatItems: ni;
    bestAckTime: ni;
    {$IFDEF DUMB}SendersBackLog: byte;{$ENDIF}
    _unclearedsequencenumber: int64;

    Error: string;
    intervalStabilizer: double;
    ConnectionType: TConnectionType;
    evnotFlooded: TSignal;
    lastAckedSequence: int64;
    latent_ack: boolean;
    lastperiodicack: ticker;
    processpacketqueuetime: ticker;
    noMorePacketsAllowed: boolean;
    procedure DispatchInput_Sync(itm: TEndpointInputQueueItem);
    procedure ChangeConnectionIDInLogs(iOldID,
      iNewID: int64; from: boolean);

    procedure SystemThreadExecute(thr: TRUDPSystemThread);
    procedure UserThreadExecute(thr: TRUDPUserThread);

    function CanIdleSystemThreaD: boolean;
    procedure EvalIntervalStability;

    constructor Create(ct: TConnectionType; mult: TMultiplexedUDPEndpoint);reintroduce;virtual;
    destructor Destroy;override;
    procedure BeforeDestruction;override;
    procedure ClearLogs;
    procedure ClearAckedTX;
    procedure Detach;override;
    property IndexValue: int64 read GetIndexValue;
    function BigPacketDivisor: ni;

  //--------------------------------------------------------------------------
    procedure ResetKeepAliveTimer;
    procedure HandleIncomingPacket(ifo: TReliableUDPPacketLogRecord);
  //--------------------------------------------------------------------------



    property Remotehost: string read FRemoteHost write SetRemoteHost;
    property RemotePort: nativeint read FRemotePOrt write FRemotePort;
    property Multiplexer: TMultiplexedUDPEndpoint read FMultiplexer write FMultiplexer;
    function Connect(remote_host: string; remote_port: nativeint): boolean;overload;
    function Connect: boolean;overload;
    function SendDAta(p: pbyte; len: nativeint): nativeint;
    function ReadDAta(p: pbyte; len: nativeint): nativeint;
    function AvailableData: nativeint;
    procedure SendPacketAndCreateHeader(p: pbyte; len: nativeint);overload;
    function SendPAcket(h: TReliableUDPHeader; p: pbyte; len: nativeint): boolean;overload;
    procedure SendPAcketCopyPayLoad(h: TReliableUDPHeader; p: pbyte; len: nativeint);overload;
    function NextSequenceNumber: int64;
    procedure DoAvailableData;
    procedure ProcessKeepAliveTimer;
    procedure ProcessElegantClosure;
    procedure AckIncoming(bForce: boolean = false);
    procedure RetransOutgoing(bimmediate: boolean);
    function WaitForAck(h: TReliableUDPHeader; ack_timeout: nativeint; bThrowExceptions: boolean = true): boolean;
    function WaitForConnected(iTimeOut: nativeint): boolean;
    procedure ClearTXLogsUpTo(seq: int64);
    procedure ClearSpecificPacket(ifo: TReliableUDPPacketLogRecord);overload;
    procedure ClearSpecificPacket(seq: int64; ifo: TReliableUDPPacketLogRecord = nil);overload;
    procedure ProcessPacketQUEUE;
    function GuaranteeRead(p: pbyte; len: nativeint): ni;
    procedure GuaranteeSendData(p: pbyte; len: nativeint);
    function WaitForData(iTimeOutMs: nativeint): boolean;
    function Disconnect: boolean;
    procedure EvalFloodSignal;
    procedure CheckDebugPacketRate;
    procedure ReactToHealthReport(seq: int64; health: TFECREport);
    procedure ControlInterval(bPacketLoss: boolean);

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
    property CloseState: TCloseState read FCloseState write SetCloseState;
    property CloseOrigin: TCloseOrigin read FCloseOrigin write FCloseOrigin;
    property evUserDataIncoming: TSignal read FevUserDataIncoming;
    property DebugTag: string read GEtDebugTag write SetDebugTag;
    property OnDataAvailable: TNotifyEvent read FOnDataAvailable write FOnDataAvailable;
    property IsServer: boolean read FIsServer write FIsServer;
    property ConnidDebug: string read GetConnidDebug;
    property MTU: ni read FMTU;
    property FlexMTU: ni read GEtFlexMTU;
    property FecSize: ni read GEtFecSize;
    property FlexPayloadLimit: ni read GetFlexPayloadLImit;
    property WalkingAverage: ni read GetWalkingAverage;
    function FlexREtransTime: ni;
    procedure PeriodicAck;
    property QueueTime: ticker read GetQueueTime write SetQueueTime;
//    property connectionid: cardinal read Getconnectionid;
    function SmartLock: boolean;inline;
  end;

  TReliableUDPClientEndpoint = class(TReliableUDPEndpoint)
  end;





  //--------------------------------------------------------------------------
  TReliableUDPEndpointList = class(TStandardList<int64, TReliableUDPEndpoint>)
  private
  public
    function FindBySeed(iSeed: int64): TReliableUDPEndpoint;
    function FindByClientSeed(sRemotehost: string; remoteport: ni; iSeed: int64): TReliableUDPEndpoint;
  end;



  TUDPDataAvailable = procedure (endpoint: TReliableUDPEndpoint) of object;

  //--------------------------------------------------------------------------
  TSimpleReliableUDPClient = class (TSimpleAbstractConnection)
  private
    procedure SetCli(const Value: TReliableUDPClientEndpoint);
  protected
    sectCli: TCLXCriticalSection;
  strict protected
    Fcli: TReliableUDPClientEndpoint;//todo 1:translate
    function DoWaitForData(timeout: cardinal): boolean;override;//todo 1:translate
    procedure cli_OnDataAvailable(sender: TObject);
    function DoCheckForData: Boolean; override;

  public


    constructor Create;override;
    procedure Detach;override;
    procedure OnDataAvailable(const sender: TObject);
    function GetConnected: boolean;override;//todo 1:translate
    function DoReadData(buffer: pbyte; length: integer): integer;override;//todo 1:translate
    function DoSendData(buffer: pbyte; length: integer): integer;override;//todo 1:translate

    procedure DebugTagUpdated(s: string);override;
    function DoConnect: boolean; override;//todo 1:translate
    destructor Destroy;override;//todo 1:translate
    procedure DoDisconnect; override;//todo 1:translate
    property cli: TReliableUDPClientEndpoint read FCli write SetCli;

  end;

  TSimpleReliablePrivateServerEndpoint = class(TSimpleAbstractConnection)
  strict protected
    function DoCheckForData: boolean;override;
    function DoWaitForData(timeout: cardinal): boolean;override;
  public
    cli: TReliableUDPEndpoint;
    procedure Detach;override;
    destructor Destroy;override;

    function GetConnected: boolean;override;
    function DoReadData(buffer: pbyte; length: integer): integer;override;
    function DoSendData(buffer: pbyte; length: integer): integer;override;//todo 1:translate

    procedure DebugTagUpdated(s: string);override;
    function DoConnect: boolean; override;
    procedure DoDisconnect; override;

  end;

  TWorthless = class(TIdUDPListenerThreadList)
  public
    function GetList: TIdUDPListenerThreadList;
  end;

  TEndPOintInputQueueItem = class(TQueueItem)
  strict private

    Fifo: TReliableUDPPacketLogRecord;
    procedure SetIfo(const Value: TReliableUDPPacketLogRecord);
  private
    procedure SetEp(const Value: TReliableUDPendpoint);
  protected
    Fep: TReliableUdpEndpoint;
    procedure DoExecute;override;
  public

    bDisableAcks: boolean;
    property ep: TReliableUDPendpoint read Fep write SetEp;

    property ifo: TReliableUDPPacketLogRecord read FIfo write SetIfo;

    procedure Detach;override;




  end;


  //--------------------------------------------------------------------------
{$IFDEF USE_FEC}
  TRUDPBase = TFECServer;
{$ELSE}
  TRUDPBase = TIdUDPServer;
{$ENDIF}


  TMultiplexedTCPCompanion = class(TIdTCPServer)
  protected
    mpx: TmultiplexedUDPEndPoint;
    function DoExecute(AContext: TIdContext): Boolean; override;
  public
  end;

  TMultiplexedUDPEndpoint = class(TRUDPBase)
  private
    FIsServer: boolean;
{$IFDEF STRUCTURED_INCOMING_UDP}
    procedure ProcessUDPRead(itm: TPacketQueueItem; bDisableAcks: boolean);
    procedure ProcessUDPReadAD(itm: TPacketQueueItem; bDisableAcks: boolean);
    procedure ProcessUDPReadAL(itm: TPacketQueueItem);
    procedure ReSendDataFromEndPoint(ep: TReliableUDPEndPOint;
      h: TReliableUDPHEader; mem: PByte; memlen: nativeint; pid: cardinal);
    procedure SetHintstuffToDo(const Value: boolean);inline;
    procedure SplitSend(ep: TReliableUDPEndPOint; h: TReliableUDPHEader;
      mem: PByte; memlen: nativeint);
    function SendBackToSender(ifoReferencingPacket: TReliableUDPPacketLogRecord;
      h: TReliableUDPHEader; mem: PByte; memlen: nativeint): cardinal;
{$ENDIF}

  protected
    FOnDataAvailable: TUDPDataAvailable;
    _nextconnectionid: int64;
    _nextseed: int64;
    FEndpoints: TReliableUDPEndpointList;
    FLocalPort: nativeint;
    sect, sect_send, sectUDPSend: TCLXCriticalSection;
    shuttingdown: boolean;
    detached: boolean;
    procedure DoDataAvailable(ep: TReliableUDPEndpoint);//todo 1:translate
    function HandleSystemChangeConnID(ifo: TReliableUDPPacketLogRecord):TReliableUDPEndpoint;//todo 1:translate
    procedure Init;virtual;
  public
{$IFDEF QUEUE_INCOMING_UDP}
{$IFDEF MULTI_QUEUE_IN}
    queue_in: TMultiQUeue;
{$ELSE}
    queue_in: TSimpleQueue;
{$ENDIF}
{$ENDIF}
    Fhint_stuff_to_do: boolean;
    hint_had_stuff_to_do: boolean;
    estimated_endpoint_count: ni;
    nextfecid: cardinal;
    function StuffTodo: boolean;
    procedure Lock;
    procedure Unlock;
    procedure SendLock;
    procedure SendUnlock;
    property hint_stuff_to_do: boolean read FHint_stuff_to_do write SetHintstuffToDo;


    property IsServer: boolean read FIsServer;
    constructor Create(AOwner: TComponent);reintroduce;virtual;//todo 1:translate
    destructor Destroy;override;//todo 1:translate
    procedure Detach;{$IFDEF USE_FEC}override;{$ENDIF}//todo 1:translate
    procedure DetachAndFree;{$IFDEF USE_FEC}override;{$ENDIF}//todo 1:translate
    //--------------------------------------------------------------------------
    //--------------------------------------------------------------------------
    procedure DoUDPRead(AThread: TIdUDPListenerThread; const XData: TIdBytes; ABinding: TIdSocketHandle); override;//todo 1:translate
{$IFDEF USE_FEC}
    procedure DoUDPRead_BETTER(const health: TFECReport; const XData: TIdBytes; PeerIP: string; PEerPort: ni); override;//todo 1:translate
{$ENDIF}
    //--------------------------------------------------------------------------
    //--------------------------------------------------------------------------
    //--------------------------------------------------------------------------

    function HandleSystemPacket(ifo: TReliableUDPPacketLogRecord): TReliableUDPEndpoint;//todo 1:translate
    function HandleSystemCommandPacket(ifo: TReliableUDPPacketLogRecord): TReliableUDPEndpoint;//todo 1:translate
    function FindAvailableConnectionID: int64;//todo 1:translate
    function  HandleSystemCreatePacket(ifo: TReliableUDPPacketLogRecord):TReliableUDPEndpoint;//todo 1:translate
    function HandleSystemCommandResponsePacket(ifo: TReliableUDPPacketLogRecord): TReliableUDPEndpoint;//todo 1:translate

    procedure DispatchPacketToEndpoint(ifo: TReliableUDPPacketLogRecord; bDisableAcks: boolean);overload;//todo 1:translate
    procedure DispatchPackettoEndpoint(ep: TReliableUDPEndpoint; ifo: TReliableUDPPacketLogRecord);overload;//todo 1:translate
    function SendDataFromEndPoint(ep: TReliableUDPEndPOint; h: TReliableUDPHEader; mem: PByte; memlen: nativeint): cardinal;overload;//todo 1:translate
    function  SendDataFromEndPoint(ep: TReliableUDPEndPOint; mem: PByte; memlen: nativeint): cardinal;overload;//todo 1:translate
    procedure BindToport(iPort: nativeint);
    procedure BindToAnyPort();//todo 1:translate
    function CreateEndPoint(tcid, fcid: cardinal; bServer: boolean): TReliableUDPEndpoint;
    procedure DeregisterEndPOint(ep: TReliableUDPEndpoint);//todo 1:translate
    function NextConnectionId: cardinal;//todo 1:translate
    property LocalPort: nativeint read FLocalport;//todo 1:translate
    procedure SendError(ep: TReliableUDPEndpoint; bSystem: boolean; b: byte; data1, data2: int64;  diagnostic: string; bAck: boolean; fcidzero: boolean);//todo 1:translate
    property OnDataAvailable: TUDPDataAvailable read FOnDataAvailable write FonDataAvailable;//todo 1:translate
    //todo 1: call ondatavailable when Data is added to rxDataLog
    //todo 1: call ondataavailable after OnDataAvailable is serviced, if data is STILL available
    function DebugConnections: string;
    procedure DisconnectAll;
    function NextSeed: int64;
    procedure DoOnUDPException(AThread: TIdUDPListenerThread; ABinding: TIdSocketHandle; const AMessage : String; const AExceptionClass : TClass);  override;
    procedure SendDataToAll(p: Pbyte; len: ni);
    function SendEx(const basetm, interval: ticker; const AHost: string; const APort: TIdPort; const ABuffer: TIdBytes; const FECSize: cardinal): cardinal;
    function ReSendEx(const basetm, interval: ticker; const AHost: string; const APort: TIdPort; const ABuffer: TIdBytes; fecpid: cardinal; const FECSize: cardinal): cardinal;
    function GetNextFecID: cardinal;
    procedure ResetConnection(tcid: int64; fcid: int64);
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
    procedure Init;override;
  end;

  //--------------------------------------------------------------------------
  TMultiplexedUDPServer = class(TMultiplexedUDPEndpoint)//todo 1:translate
  public
    procedure Init;override;
  end;

  TRUDPStats = record
    outoforder: int64;
    retries: int64;
    tx: int64;
    rx: int64;
    procedure Init;
  end;



procedure UDPDebug(s: string);inline;
procedure MEmoryDebug(s: string);inline;

var
  net_stats_clear_rate_tx: TRingStats;
  net_stats_clear_rate_rx: TRingStats;
  net_stats_queue_rate_tx: TRingStats;
  net_stats_packets_tx: TRingStats;
  net_stats_packets_rx: TRingStats;
  net_stats_packets_retx: TRingStats;
  net_stats_packets_retx_rq: TRingStats;
  rudpstats: TRUDPStats;

function WalkToParts(wmtu: ni): ni;
function WalkToMTU(wmtu: ni): ni;

function NeedRUDPOutQueue: TRUDPOutQueue;
function NeedRUDPPIPEInQueue: TRUDPPipeInQueue;
procedure NoNeedRUDPOutQueue(var q: TRUDPOutQueue);
procedure  NoNeedRUDPPIPEInQueue(var q: TRUDPPipeInQueue);





implementation

{ TReliableUDPHeader }


uses
  RUDPMonitor, registeredmemory;

{$IFDEF USE_SHARED_QUEUES}
var
  shared_rudpoutqueue: TRUDPOutQueue;
  shared_RUDPPIPEInQueue: TRUDPPipeInQueue;
{$ENDIF}


function NeedRUDPOutQueue: TRUDPOutQueue;
begin
{$IFDEF USE_SHARED_QUEUES}
  result := shared_rudpoutqueue;
{$ELSE}
  result := TPM.Needthread<TRUDPOUtQueue>(nil);
  result.MAxItemsINQueue := 1024;
  result.start;
  {$IFDEF HIGH_PRIORITY_QUEUES}
    result.Priority := tpHighest;
  {$ENDIF}
{$ENDIF}
end;

function NeedRUDPPIPEInQueue: TRUDPPipeInQueue;
begin
{$IFDEF USE_SHARED_QUEUES}
  result := shared_RUDPPIPEInQueue;
{$ELSE}
  result := TPM.Needthread<TRUDPPipeInQueue>(nil);
  result.MaxItemsInQueue := 1024;
  result.start;
  {$IFDEF HIGH_PRIORITY_QUEUES}
    result.Priority := tpHighest;
  {$ENDIF}
{$ENDIF}
end;

procedure NoNeedRUDPOutQueue(var q: TRUDPOutQueue);
begin
{$IFDEF USE_SHARED_QUEUES}
  q := nil;
{$ELSE}
  q.Stop;
  q.SafeWaitFor;
  TPM.NoNeedthread(q);
{$ENDIF}
end;

procedure  NoNeedRUDPPIPEInQueue(var q: TRUDPPipeInQueue);
begin
{$IFDEF USE_SHARED_QUEUES}
  q := nil;
{$ELSE}
  q.Stop;
  q.SafeWaitFor;
  TPM.NoNeedthread(q);
{$ENDIF}
end;






function WalkToParts(wmtu: ni): ni;
begin
  exit(greaterof(MIN_MTU, lesserof(greaterof(wmtu,1),MAX_PARTS)));

end;

function WalkToMTU(wmtu: ni): ni;
begin
  result := lesserof(WalkToParts(wmtu)*400, CHAIN_SIZE_LIMIT);
end;


procedure UDPDebug(s: string);
begin
  Debug.Log(nil,s,'udp');
//  Debug.log(s);
end;

procedure MEmoryDebug(s: string);
begin
  Debug.log(s);
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
  result := result + '_EXP='+inttostr(expected_sequencenumber);
  result := result + '_CMD='+inttostr(command);
  result := result + '_TCID='+inttostr(tcid);
  result := result + '_FCID='+inttostr(fcid);
  result := result + '_SEED='+inttostr(seed);
//  result := result + '_AGE='+inttostr(gettimesince(sendtime));





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
  tag[0] := byte(ord(char('A')) and 255);
  tag[1] := byte(ord(char('D'))and 255);
  if ep = nil then begin
    tcid := 0;
    fcid := 0;
  end else begin
    tcid := ep.TOConnectionID;
    fcid := ep.FROMConnectionID;
  end;

  if ep <> nil then
    seed := ep.seed;

  expected_sequencenumber := 0;
  parts := 1;

end;

function TReliableUDPHeader.IsDataPacket: boolean;
begin
  result := not (Flag_System or Flag_Command or Flag_CommandResponse or Flag_Create or Flag_CLOSE);
end;

function TReliableUDPHeader.GetIsRepack: boolean;
begin
  result := tag[1] = 76;
end;

procedure TReliableUDPHeader.PrepareToSend(ep: TReliableUDPEndpoint);
begin
  tcid := ep.TOconnectionid;
  fcid := ep.FROMConnectionId;
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

procedure TReliableUDPHeader.SetIsRepack(const Value: boolean);
begin
  if value then
    tag[1] := 76
  else
    tag[1] := 68;
end;

{ TReliableUDPEndpoint }

procedure TReliableUDPEndpoint.SendAck(specific_packet: int64; health: PFECReport);
var
  h: TReliableUDPHeader;
  mm: Pbyte;
begin
//  if GetTimeSince(LastAckTime) < 5 then exit;

  if DisableAcks then
    exit;

  if SmartLock then
  try
    h.Init(self);
    h.tcid := self.TOconnectionid;
    h.fcid := self.FROMConnectionId;
    h.seed := self.seed;
    h.Flag_AckType := ackNone;
    h.command := CMD_ACK;
    h.Flag_Command := true;
    h.sequencenumber := -1;//0 because there's no ack coming back
    h.expected_sequencenumber := _nextexpectedSequenceNumber;
    {$IFDEF DUMB}h.SendersBackLog := txLog.count;{lesserof(_nextsequencenumber - _unclearedsequencenumber, 255);}{$ENDIF}
    h.argument := specific_packet;
    if h.argument < (rxAckedLast-1) then
      exit;
    rxAckedLast := _nextexpectedsequencenumber;
//{$IFDEF UDP_DEBUG}    UDPDebug('Sending ack up to and including #'+inttostr(h.expected_sequencenumber-1));{$ENDIF}
    LastAckTime := GetTicker;
    //h.PrepareToSend(self);
    mm := nil;
    if health <> nil then begin
{$IFDEF USE_REGISTERED_MEMORY}
      mm := GetRegisteredMemory(sizeof(TFecReport), self.classname+'.TReliableUDPEndpoint.SendAck');
{$ELSE}
      mm := GetMemory(sizeof(TFecReport));
{$ENDIF}
      movemem32(mm, PByte(health), sizeof(TFecReport));
      SendPacket(h, mm, sizeof(TFecREport));
    end else
      SendPacket(h, nil, 0);

    net_stats_clear_rate_rx.Accumulate(1);
  finally
    Unlock;
  end;
end;

procedure TReliableUDPEndpoint.SendNothingToSend();
//this message doesn't really need to do anythign on the other end
//but it is necessary because the other end might send back rapid ACKs
//after the transmission has ended
//the main purpose here is to update the SendersBackLog on the other
//end, which should stop any latent ACKS
//AS of Feb29,2016, the ExpectedSequenceNumbers and SendersBackLog parameters
//MUST NOT be updated on retransmissions, because they will cause invalid
//checksums on the FEC side.

//Send this message whenever an ACK is received and the TX buffer is
//empty

var
  h: TReliableUDPHeader;
begin
//  if GetTimeSince(LastAckTime) < 5 then exit;

  if DisableAcks then
    exit;

  if SmartLock then
  try
    h.Init(self);
    h.tcid := self.TOconnectionid;
    h.fcid := self.FROMConnectionId;
    h.seed := self.seed;
    h.Flag_AckType := ackNone;
    h.command := CMD_NOTHING_TO_SEND;
    h.Flag_Command := true;
    h.sequencenumber := -1;//0 because there's no ack coming back
    h.expected_sequencenumber := _nextexpectedSequenceNumber;
    {$IFDEF DUMB}h.SendersBackLog := txLog.count;{lesserof(_nextsequencenumber - _unclearedsequencenumber, 255);}{$ENDIF}
    if h.argument < (rxAckedLast-1) then
      exit;
    rxAckedLast := _nextexpectedsequencenumber;
//{$IFDEF UDP_DEBUG}    UDPDebug('Sending ack up to and including #'+inttostr(h.expected_sequencenumber-1));{$ENDIF}
    LastAckTime := GetTicker;
    //h.PrepareToSend(self);

    net_stats_clear_rate_rx.Accumulate(1);
  finally
    Unlock;
  end;
end;


procedure TReliableUDPEndpoint.AckFutureGaps;
{$IFDEF FUTURE_ACKS}
var
  iHighest: int64;
  ii: int64;
  t: int64;
  solid: TtreeItem_ReliableUDPPacketLogRecord;
  dax: ni;
  da: TDynInt64Array;
{$ENDIF}
begin
{$IFDEF CPUX64}
  {$IFDEF FUTURE_ACKS}
  if SmartLock then
  try

    //determine highest sequence
    iHighest := -1;
    rxLog.Iterate(
      procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
      begin
        ii := TtreeItem_ReliableUDPPacketLogRecord(ABTreeItem).packet.h.sequencenumber;
        if ii > iHighest then
          iHighest := ii;
      end
    );


    if iHighest >= 0 then begin
      setlength(da,255);
      dax := 0;
      for t := iHighest downto _nextexpectedSequenceNumber+1 do begin
        solid := rxLog.Find(t);
        If solid <> nil then begin
          if (not solid.packet.acked) or (gettimesince(solid.packet.acktime) > (1000*greaterof(0.1, queue_out.ticks_per_byte)))  then begin
            solid.packet.acktime := getticker;
            solid.packet.acked := true;
            da[dax] := solid.packet.h.sequencenumber;
            INC(DAX);
            if (dax >=255) then
              break;
          end;
        end;
      end;
      setlength(da, dax);
      if dax > 0 then
        SendFutureAck(da);

    end;

  finally
    Unlock;
  end;
  {$ENDIF}
{$ENDIF}
end;

procedure TReliableUDPEndpoint.AckIncoming(bForce: boolean = false);
var
  bSend: boolean;
  ti: Ttreeitem_ReliableUDPPacketLogRecord;
  r: TReliableUDPPacketLogRecord;
  acksince: ticker;
  acklevel: int64;
begin
  lastIncomingPEriodicAckTime := getticker;

{$IFDEF QUEUE_INCOMING_UDP}
  if not bForce then
    if (multiplexer.queue_in.EstimatedSize > 0) then
      exit;
{$ENDIF}

  //try to increment the expected sequence number until a break in the sequence is found
  acklevel := -1;
  acksince := GEtTimeSince(lastAckTime);
  bSend := false;//SendersBackLog > 1;
  if SmartLock then
  try
    repeat
      ti := rxLog.Find(_nextexpectedSequenceNumber);
      if ti <> nil then begin
        r := ti.packet;
//        windows.Beep(2000,50`);
        lastIncomingAckTime := getticker;
        inc(_nextexpectedsequencenumber);
        if r.h.Flag_AckType <> ackNone then begin
//          if gettimesince(r.acktime) > STALE_RETRANS_TIME then begin
          r.acktime := r.acktime;
          acksince := greaterof(acksince, gettimesince(r.acktime));
          bSend := true;
//          end;

        end;
        r.Acked := true;

{$IFDEF UDP_DEBUG}        Debug.Log(self, 'ACK++: '+r.DebugString);{$ENDIF}
        if rxLog.Has(r.h.sequencenumber) then begin
          rxLog.Remove(r);
          {$IFDEF REF_DEBUG}Debug.log('Release from AckIncoming');{$ENDIF}
{$IFDEF USE_FEC}
          SendAck(r.h.sequencenumber, @r.health);
{$ELSE}
          SendAck(r.h.sequencenumber, nil);
{$ENDIF}
          r._Release;
          EvalSYstemThreadSignal;
          EvalUserThreadSignal;
        end;
      end;
    until ti = nil;

//{$IFDEF SLOW_ACK}
//    if bSend or bForce then
//      SendAck(_NextExpectedSequenceNumber);
//{$ENDIF}
  finally
    unlock;
  end;

  //acknowledge current sequence level
  if ((bSend or bForce)// and ({(not Multiplexer.hint_stuff_to_do) or}
  and ((acksince > 200) or (acksince<0))) then
  begin
//    if lastAckedSequence <> _nextexpectedsequencenumber then
//    if not Multiplexer.hint_stuff_to_do then
      latent_ack := false;
      SendAck(_nextexpectedsequencenumber-1, nil);

  end;// else
//    Debug.Log(self, 'nothing to ack');

  if not multiplexer.stufftodo then
    AckFutureGAps;



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
  if SmartLock then
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


function TReliableUDPEndpoint.INdexOfMTUTest(mtu: ni): ni;
var
  t: ni;
begin
  result := -1;
  for t:= high(MTU_TEST_SIZES) to low(MTU_TEST_SIZES) do begin
    if MTU_TEST_SIZES[t] = mtu then begin
      exit(t);
    end;
  end;
end;

procedure TReliableUDPEndpoint.InitiateCloseDueToSystemExeception;
begin
  self.CloseOrigin := coServer;
  CloseState := csTimedOut;
end;






procedure TReliableUDPEndpoint.BeforeDestruction;
begin
//  if not CloseMe then
//    raise ECritical.Create('You cannot destroy a '+classname+' that is connected.   Disconnect it first.');

  if assigned(eet) then begin
    eet.EndPOint := nil;
{$IFDEF USE_FIBERS}
    FIB.NoNeedFiber(eet);
{$ELSE}
    eet.stop;
    eet.WaitFor;
    TPM.NoNeedthread(eet);
{$ENDIF}
  end;
  if assigned(user_thread) then begin
    user_thread.Stop;
    user_thread.WaitFor;
    user_thread.endpoint := nil;
    TPM.NoNeedthread(user_thread);
  end;



  inherited;

end;

function TReliableUDPEndpoint.BigPacketDivisor: ni;
begin
  result := greaterof(1, FLEXmtu div 1250);
end;

procedure TReliableUDPEndpoint.ClearAckedTX;
begin
  ClearTXLogsUpTo(txAckLevel);
end;

procedure TReliableUDPEndpoint.ClearLogs;
//!!IFOREF
//under lock
var
  p: TReliableUDPPacketLogRecord;
  ti: Ttreeitem_ReliableUDPPacketLogRecord;
begin
  Lock;
  try
  if assigned(txLog) then
  while (txLog.Count > 0) and (txLog.root <> nil) do begin
    ti := TTreeItem_ReliableUDPpacketLogRecord(txLog.Root);
    p  := ti.packet;
    txLog.Remove(p);
    {$IFDEF REF_DEBUG}Debug.log('RElease from ClearAckedTX');{$ENDIF}
    p._Release;
  end;

  if assigned(rxLog) then
  while rxLog.Count > 0 do begin
    ti := TTreeItem_ReliableUDPpacketLogRecord(rxLog.Root);
    if ti = nil then begin
      Debug.Log('WARNING! rxLog has count>0 with nil root!');
      break;
    end;
    rxLog.Remove(ti.packet);
    p  := ti.packet;
    {$IFDEF REF_DEBUG}Debug.log('RElease from ClearAckedTX1');{$ENDIF}
    p._Release;
  end;

  if assigned(rxDataLog) then
  while rxdataLog.Count > 0 do begin
    ti := TTreeItem_ReliableUDPpacketLogRecord(rxDataLog.Root);
    if ti = nil then begin
      Debug.Log('WARNING! rxDataLog has count>0 with nil root!');
      break;
    end;
    rxDataLog.Remove(ti.packet);
    p  := ti.packet;
    {$IFDEF REF_DEBUG}Debug.log('RElease from ClearAckedTX2');{$ENDIF}
    p._Release;
  end;

  if assigned(rxQueue) then
  while rxQueue.Count > 0 do begin
    ti := TTreeItem_ReliableUDPpacketLogRecord(rxQUeue.Root);
    if ti = nil then begin
      Debug.Log('WARNING! rxQueue has count>0 with nil root!');
      break;
    end;
    rxQUeue.Remove(ti.packet);
    p  := ti.packet;
    {$IFDEF REF_DEBUG}Debug.log('RElease from ClearAckedTX3');{$ENDIF}
    p._Release;
  end;

  inherited;
  finally
    unlock;
  end;

end;




procedure TReliableUDPEndpoint.ClearSpecificPacket(ifo: TReliableUDPPacketLogRecord);
//!!IFOREF
//under lcok
var
  health: TFECReport;
begin
  Lock;
  try
    if ifo.payload_length >= sizeof(health) then begin
      movemem32(@health, ifo.payload, sizeof(health));
      ReactToHealthREport(ifo.h.argument, health);
    end;
    ClearSpecificPacket(ifo.h.argument, ifo);
  finally
    Unlock;
  end;
end;

procedure TReliableUDPEndpoint.ClearSpecificPacket(seq: int64; ifo: TReliableUDPPacketLogRecord = nil);
//!!IFOREF
var
  i: int64;
  r: Ttreeitem_ReliableUDPPacketLogRecord;
  p: TReliableUDPPacketLogRecord;
  tm: ticker;
begin
  p := nil;
  lock;
  try
    r := txLog.Find(seq);
    if r <> nil then begin
      {$IFDEF CLEAR_DEBUG}
          Debug.Log(self,'Clearing '+r.packet.DebugString);
      {$ENDIF}
      lastInComingAckTime := GetTicker;
      if not r.packet.waschopped then begin
        UpdateMTUTestSuccesses(r.packet.payload_length);
      end;
      if r.packet.payload_length > 200 then begin
        WalkFlexMTU(false);
      end;
      if not r.packet.acked then begin
        tm := gettimesince(r.packet.sendtime);
//        if tm < 10 then
//          Debug.Consolelog('WTF!'+inttostr(tm));
        if not r.packet.resent then
          rsAckTime.AddStat(tm);//dont' get stats for retransmitted packets because it will slow averages
{$IFDEF BEEPS}
        if r.packet.watchme then
           SysBeep(500,50);
{$ENDIF}
//        if rsAckTime.NewBAtch then
//          bestAckTime := round(rsAckTime.Minimum);
        if (bestAckTime > tm) or (bestAckTime=0) then
          bestAckTime := tm;
      end;

      p := r.packet;
{$DEFINE STUPID}
{$IFDEF STUPID}
      txLog.remove(p);
      EvalFloodSignal;
      net_stats_clear_rate_tx.Accumulate(1);
//      p.deadcheck;
      {$IFDEF REF_DEBUG}Debug.log('RElease from ClearSpecificPacket');{$ENDIF}
//      p._Release();
{$ELSE}
      p.acked := true;
{$ENDIF}
    end else begin
      {$IFDEF CLEAR_DEBUG}
          Debug.Log(self,'ALREADY Cleared! '+inttostr(seq));
      {$ENDIF}
    end;
  finally
    unlock;
  end;

  if p <> nil then
    p._Release();
end;

procedure TReliableUDPEndpoint.ClearTXLogsUpTo(seq: int64);
//!!IFOREF
//under lock
var
//  t: nativeint;
  ti: TtreeItem_ReliableUDPPacketLogRecord;
  r: TReliableUDPPacketLogRecord;
  tm: ticker;
begin
//{$IFDEF SEND_RECEIVE_DEBUG}
//  Debug.log(self,'Clear to '+inttostr(seq));
//{$ENDIF}
//  Debug.ConsoleLog('ClearTXLogsUpTo '+inttostr(seq));
  Lock;
  try
    ti := TtreeItem_ReliableUDPPacketLogRecord(txLog.FirstItem);
    while ti <> nil do begin
      r := ti.packet;
      if seq >= _unclearedsequencenumber then
        _unclearedsequencenumber := seq+1;
      if r.h.sequencenumber < seq then begin
        if txLog.Has(ti) then begin
{$IFDEF SEND_RECEIVE_DEBUG}
          Debug.Log(self,'Clearing '+r.DebugString);
{$ENDIF}
{$IFDEF CLEAR_DEBUG}
          Debug.Log(self,'Clearing '+r.DebugString);
{$ENDIF}

          TM := gettimesince(r.sendtime);
          if not r.resent then
            WalkFlexMTU(false);
//          if tm < 10 then
//            Debug.Consolelog('WTF!'+inttostr(tm)+' ('+inttostr(r.sendtimetag)+')');
          if not r.resent then
            rsAckTime.AddStat(tm);//dont' get stats for retransmitted packets because it will slow averages
          rsAckTime.AddStat(TM);
{$IFDEF BEEPS}
          if r.watchme then
             SysBeep(500,50);
{$ENDIF}
          if (bestAckTime > tm) or (bestAckTime=0) then
            bestAckTime := tm;

          txLog.remove(r);
          ti := TtreeItem_ReliableUDPPacketLogRecord(txLog.FirstItem);
          EvalFloodSignal;
          net_stats_clear_rate_tx.Accumulate(1);
          r.deadcheck;
          {$IFDEF REF_DEBUG}Debug.log('Release from ClearTXLogsUpTo');{$ENDIF}
          r._Release();
        end;

      end else
        break;
    end;
  finally
    Unlock;
  end;

end;

function TReliableUDPEndpoint.Connect: boolean;
var
  h: TReliableUDPHeader;
  bRetry: boolean;
  tmStarT: ticker;
begin
  tmStart := getticker;
  Connecting := true;
  if Remotehost = '' then
    raise ETRansportError.create('cannot connect to a nil host');
  if RemotePort = 0 then
    raise ETRansportError.create('cannot connect to a nil endpoint');

  if connected then begin
    result := true;
    exit;
  end;


  if SmartLock then
  try
    h.Init(nil);
    h.tcid := self.TOconnectionid;
    h.fcid := self.FROMConnectionId;
    h.Flag_Create := true;
    h.Flag_System := true;
    h.command := RUDP_SYS_CMD_CREATE;
    h.sequencenumber := 0;
    h.Flag_AckType := ackImmediate;
    h.Flag_Command := true;
    h.PrePareToSend(self);
    h.seed := self.Multiplexer.NextSeed;
    self.seed := h.seed;
    Debug.Log('Created Endpoint with Seed#'+h.seed.tostring);
  finally
    Unlock;
  end;

  repeat
    bRetry := false;
    if SmartLock then
    try
      Debug.Log('Waiting for connection ACKnowledgement');

      SendPacket(h, nil, 0);
    //  multiplexer.Active := true;
    finally
      Unlock;
    end;
    try
      result := WaitForAck(h,2000, false);
      bRetry := not result;
    except
      bRetry := true;
    end;
    if gettimesince(tmStart) > INITIAL_CONNECTION_TIMEOUT then
      exit(false);
  until bRetry = false;
  result := Error = '';
  if result then
    result := WaitForConnected(10000);

  Debug.Log('Connected!');
  SayHello;
  Debug.Log('Hello!');
  result := Connected;
  Connecting := false;

end;

function TReliableUDPEndpoint.Connect(remote_host: string;
  remote_port: nativeint): boolean;
begin
  self.remotehost := remote_host;
  RemotePort := remote_port;
  result := connect;


end;



constructor TReliableUDPEndpoint.CReate(ct: TConnectionType; mult: TMultiplexedUDPEndpoint);
var
  t: ni;
begin
  inherited CReate;
  splitass.seq := -1;
  for t := low(MTU_TEST_SIZES) to high(MTU_TEST_SIZES) do begin
    mtu_test_results[t].sz := MTU_TEST_SIZES[t];
    mtu_test_results[t].lasttest := 0;
    mtu_test_results[t].lastsuccess := 0;
  end;
  mtu_test_results[0].lasttest := getticker;
  mtu_test_results[0].lastsuccess := getticker;

  FMTU := DEFAULT_MTU;
  evNotFlooded := TSignal.create;
  rsRetransLoopRate := TRingStats.create;
  rsAckTime := TRingStats.create;
  rsAckTime.SetSize(16);
  intervalStabilizer := 1;
  rsIntervalStability := Tringstats.create;
  rsIntervalStability.SetSize(100);
  for t:= 0 to 512 do begin
    rsIntervalStability.addStat(8.0);
  end;
  rsRetransRatio := TRingStats.create;
  rsRetransRatio.SetSize(64);
  for t:= 0 to 512 do begin
    rsAckTime.AddStat(200);
    rsRetransRatio.AddStat(1.0);
  end;

  random_retrans := random(20);
{$IFDEF UDP_DEBUG}  Debug.Log(self, 'Creating RUDP connection @'+inttohex(nativeint(pointer(self)),16));{$ENDIF}
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
  retrans_timeout := DEFAULT_RETRANS_TIMEOUT;
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



  walkingaverageFINE := INITIAL_PACKET_WRITE_AHEAD shl WA_SHIFT;

  queue_out := NeedRUDPOutQueue;
  queue_pipe_in := NeedRUDPPIPEInQueue;


  if ct = ctServer then begin
    //we need to create a "user_thread" is this is a server... this is the thread that will implement the server requests
    //independent of all the packet clearing nonsense.
    ECS(threadpointerlock);
    try
      user_thread := TPM.NeedThread<TRUDPUserThread>(self);
      user_thread.haswork := false;
      user_thread.Loop := true;
      user_thread.endpoint := self;
      //user_thread.loop := true;
      //user_thread.OnExecute := self.UserThreadExecute;


      FevUserDataIncoming := user_thread.evHasWork;


    finally
      LCS(threadpointerlock);
    end;

  end else begin
    FevUserDataIncoming := TSignal.create;
  end;


  ECS(threadpointerlock);
  try
{$IFDEF USE_FIBERS}
    eet := FIB.NeedFiber<TRUDPSystemThread>(self);
{$ELSE}
    eet := TPM.NeedThread<TRUDPSystemThread>(self);
    eet.Loop := true;
  {$IFDEF MSWINDOWS}
  {$IFDEF HIGH_PRIORITY_QUEUES}
      eet.BetterPriority := bpHigher;
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

    eet.endpoint := self;

    //eet.OnExecute := self.ThreadExecute;
{$IFNDEF USE_FIBERS}
//    if eet.suspended then
//      raise Exception.create('WTF! THREAD IS SUSPENDED COMING OUT OF POOL! This is no longer valid.');
{$ENDIF}
  finally
    LCS(threadpointerlock);
  end;



  Multiplexer := mult;//<---ONCE ADDED TO THE MULTIPLEXER...
                      //    PACKETS MAY FLOW IMMEDIATELY
                      //    MAKE SURE IT IS FULLY CONSTRUCTED!


  RUDPMon.RegisterEndpoint(self);//REGISTER ONLY WHEN FULLY CONSTRUCTED

  if assigned(user_thread) then
    user_thread.Start;//START ONLY WHEN FULLY CONSTRUCTED

  if assigned(eet) then
    eet.Start;//START ONLY WHEN FULLY CONSTRUCTED


  Signal(evNotFlooded, true);

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
//!!IFOREF
//under lock
var
  sl: TStringlist;
  t: nativeint;
begin
  Lock;
  try
    sl := TStringlist.create;
    try
      l.Iterate(
        procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
        begin
          sl.Add(TtreeItem_ReliableUDPPacketLogRecord(abtreeitem).packet.DebugString);
        end
      );
      result := sl.text+NEWLINE+inttostr(sl.count)+' total.'+NEWLINE;
    finally

      sl.free;//ok
      sl := nil;
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

{$IFDEF USE_FIBERS}
    s := 'fib@'+inttohex(nativeint(pointer(eet)), sizeof(ni));
{$ELSE}
      if assigned(eet) then
        s := 'ST='+inttostr(eet.threadid)+':'
      else
        s := 'ST=nil:';
{$ENDIF}

      if assigned(user_thread) then
        s := s + 'UT='+inttostr(user_thread.threadid)+':'
      else
        s := s + 'UT=nil:';

      result := 'From/To:'+Remotehost+':'+inttostr(RemotePort)+':exp='+inttostr(self._NextExpectedSequenceNumber)+':nxt'+inttostr(_nextsequencenumber)+':tcid='+inttohex(TOconnectionid,8)+':fcid='+inttohex(FROMconnectionid,8)+': ___LocalDebug('+LocalDebugInfo.ToString()+');RemoteDebug('+RemoteDebugInfo.ToString()+'):'+stringx.booltostr(Closeme, 'CloseMe;','')+stringx.booltostr(TimedOUT, 'TimedOut;','')+
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
{$IFDEF UDP_DEBUG}  Debug.Log(self, 'Destroying RUDP connection @'+inttohex(nativeint(pointer(self)),16));{$ENDIF}
{$IFDEF UDP_DEBUG}  Debug.Log(self, classname+' is being destroyed.');{$ENDIF}

  if connectiontype=ctClient then begin
    FEvUserDataIncoming.free;
    FEvUserDataIncoming := nil;
  end;





  txLog.Free;//ok
  rxLog.Free;//ok
  rxDataLog.Free;//ok
  rxQueue.Free;//ok
  rxLog := nil;
  rxLog := nil;
  rxDataLog := nil;
  rxQueue := nil;

  inherited;
  DCS(threadpointerlock);
{$IFDEF UDP_DEBUG}  Debug.Log(self,'Destroyed RUDP connection @'+inttohex(nativeint(pointer(self)),16));{$ENDIF}
  evNotFlooded.free;
  evNotFlooded := nil;
  rsRetransRAtio.free;
  rsRetransRatio := nil;
  rsAckTime.free;
  rsAckTime := nil;
  rsIntervalSTability.free;
  rsIntervalStability := nil;
  rsReTransLoopRate.free;
  rsRetransLoopRate := nil;

end;


procedure TReliableUDPEndpoint.PeriodicAck;
var
  tm: ticker;
begin
  tm := GetTicker;
  if gettimesince(tm, lastPeriodicAck) > 100 then begin
    AckIncoming(true);
    AckFutureGaps;
    lastperiodicack := GEtTicker;
  end;
end;

procedure TReliableUDPEndpoint.PrePareForDestruction(co: TCloseOrigin);
var
  st: TRUDPSystemThread;
  ut: TRUDPUserThread;
begin
  lock;
  try
    ut := user_thread;
//    user_thread := nil;
    st := eet;
//    eet := nil;
  finally
    unlock;
  end;

  if assigned(ut) then begin
    //in ALL cases we need to wait for the user thread to
    //complete its actions because we don't have a clue what's
    //going on in there...
    if co <> coServer then begin
      ut.Stop;
      ut.WaitForFinish;
    end;

    ut.endpoint := nil;
  end;


  if assigned(st) then begin

//    if not InSystemThreadStop then begin
{$IFNDEF USE_FIBERS}
      st.stop(getcurrentthreadid=thr.threadid);
      st.WaitForFinish;
      st.endpoint := nil;
      TPM.NoNeedThread(st);
{$ELSE}
      st.stop;
      st.Endpoint := nil;
      FIB.NoNeedFiber(st);
{$ENDIF}

      st := nil;
//    end;
  end;


  if assigned(ut) then begin
    if ut.threadid <> GetcurrentThreadID then
      TPM.NoNeedThread(ut);
  end;

  user_thread := nil;
  eet := nil;

  CloseState := csClosed;



end;

procedure TReliableUDPEndpoint.Detach;
var
  brefLocal: boolean;
begin
  noMorePacketsAllowed := true;
  if Detached then exit;
{$DEFINE DEREGISTER_EARLIER}
{$IFDEF DEREGISTER_EARLIER}
{$IFDEF UDP_DEBUG}  Debug.Log(self,'Deregistering endpoint');{$ENDIF}
  if assigned(Multiplexer) then begin
//    Multiplexer.Lock;<<--DEREGISTRATION uses a more specific lock and is thread-safe
    try
      self.Multiplexer.DeregisterEndPOint(self);
    finally
//      MultiPlexer.unlock;
    end;
  end;
{$ENDIF}
  queue_out.WaitForEmptyQueue;
  queue_pipe_in.WaitForEmptyQueue;
  Detached := true;

  if (self.lockowner = getcurrentthreadid)
  and (self.IsLocked) then begin
    Debug.Log(self, 'Calling detach from locked object WILL cause deadlocks.');
  end;

{$IFDEF ENDPOINT_REFERENCES}
  //if someone else has references to this object
  ECS(sectCom);
  try
    bRefLocal := _Refcount > 0;//if another object is referencing this one, then we risk potentially reentering the descrutction sequence as a result of disappearing references
    if brefLocal then          //however, if there are no other references, we don't want our own local reference to cause the same effect
{$IFDEF VERBOSE_REFERENCES}self.AddRefBy('TReliableUDPEndpoint.Detach');{$ELSE}self._AddRef;{$ENDIF}
  finally
    LCS(sectCom);
  end;


{$ENDIF}
  RUDPMon.UnRegisterEndpoint(self);

  //this kills the threads
  if assigned(user_thread) then
    PrePareForDestruction(coServer)
  else
    PrepareForDestruction(coClient);


  queue_out.WaitForEmptyQueue;
  queue_pipe_in.WaitForEmptyQueue;



{$IFNDEF DEREGISTER_EARLIER}
{$IFDEF UDP_DEBUG}  Debug.Log(self,'Deregistering endpoint');{$ENDIF}
  if assigned(Multiplexer) then begin
//    Multiplexer.Lock;<<--DEREGISTRATION uses a more specific lock and is thread-safe
    try
      self.Multiplexer.DeregisterEndPOint(self);
    finally
//      MultiPlexer.unlock;
    end;
  end;
{$ENDIF}

  //^^^^^^^^ ONCE MULTIPLEXER is DETACHED, the MULTIPLEXER
  //         will NO LONGER send us packets!!!

  //HOWEVER THERE MIGHT BE STUFF IN THE QUEUE!

  queue_out.WaitForEmptyQueue;
  queue_pipe_in.WaitForEmptyQueue;
  NoNeedRUDPOutQueue(queue_out);
  NoNeedRUDPPIPEInQueue(queue_pipe_in);


  clearLogs;



{$IFDEF UDP_DEBUG}  Debug.Log(self,'Done Destroying.');{$ENDIF}
  inherited;

{$IFDEF ENDPOINT_REFERENCES}
  if brefLocal then
{$IFDEF VERBOSE_REFERENCES}ReleaseBy('TReliableUDPEndpoint.Detach');{$ELSE}_Release;{$ENDIF}
{$ENDIF}

end;

function TReliableUDPEndpoint.Disconnect: boolean;
var
  dis_time: ticker;
begin
  if closeme then begin
    result := false;
    exit;
  end;

  result := true;
  StartDisconnect;

  if GetCurrentThreadID = eet.ThreadID then begin
    CloseMe := true;
    Connected := false;
    exit;
  end;

  dis_time := getticker;
  while not DisconnectFinished do begin
    sleep(10);
    if gettimesince(dis_time) > 10000 then begin
      break;
    end;
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
  if assigned(Multiplexer) then begin
    if self.RemoteHost = '' then begin
      Debug.Log(self, 'Data available on an undefined host?!');
      Debug.Log(self, 'STOP!');

    end;
    Multiplexer.DoDataAvailable(self);
  end;






end;

function TReliableUDPEndpoint.GetFlexPayloadLImit: ni;
begin
  result := GEtFlexMTU-50;
end;

function TReliableUDPEndpoint.GetIndexValue: int64;
begin
  result := fromconnectionid;
end;


function TReliableUDPEndpoint.GetMTUTest(mtu: ni): PMTUTestResult;
var
  t: ni;
begin
  result := nil;
  t := INdexOfMTUTest(mtu);
  if t>=0 then
    result := @Self.mtu_test_results[t];
end;

function TReliableUDPEndpoint.GetQueueTime: ticker;
begin
  result := greaterof(FQueueTime, GetHighResTicker);
end;

function TReliableUDPEndpoint.GetSystemThreadSignal: boolean;
begin
  result := eet.HasWork;
end;

function TReliableUDPEndpoint.GetUserThreadSignal: boolean;
begin
  result := user_thread.HasWork;
end;

function TReliableUDPEndpoint.GetWalkingAverage: ni;
begin
  result := lesserof(MAX_PACKET_WRITE_AHEAD, greaterof(walkingaveragefine shr WA_SHIFT, MIN_PACKET_WRITE_AHEAD));
end;

function TReliableUDPEndpoint.GuaranteeRead(p: pbyte; len: nativeint): ni;
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
  exit(iTotalRead);

end;



procedure TReliableUDPEndpoint.GuaranteeSendData(p: pbyte; len: nativeint);
var
  iJust: ni;
begin
  while len > 0 do begin
    iJust := self.SendDAta(p, len);
    dec(len, iJust);
    inc(p, iJust);
  end;

end;

procedure TReliableUDPEndpoint.RetransImmediate(
  iExpectedSequenceNumber: int64);
//!!IFOREF  OK - under lock
//!!transition
//under lock
var
  t: nativeint;
  l: TReliableUDPPacketLogRecord;
  ti: Ttreeitem_ReliableUDPPacketLogRecord;
begin
{$IFNDEF ALLOW_IMMEDIATE_RETRANS}
  exit;
{$ENDIF}
  if SmartLock then
  try
    WalkFlexMTU(true);
    txLog.Lock;
    try
      ti := TxLog.Find(iExpectedSequenceNumber);
      if ti <> nil then begin
        l := ti.packet;
        if l.h.sequencenumber = iExpectedSequenceNumber then begin
          if true or (GetTimeSInce(l.retranstime) > lesserof(greaterof(rsAckTime.PEriodicAverage, STALE_RETRANS_TIME), MAX_STALE_RETRANS_TIME)) then begin //don't retrans stuff that was already retransmitted recently
            if l.sent then begin
              l.resent := true;
              l.RetransTime := GetTicker
            end
            else begin
              l.SendTime := GetTicker;
              l.SendTimeTag := 0;
            end;


            l.h.Flag_Retrans := true;
{$IFNDEF RETRY_PRESERVE_CHECKSUMS}
            l.h.expected_sequencenumber := self._nextexpectedSequenceNumber;
{$ENDIF}
{$IFDEF SEND_RECEIVE_DEBUG}
            UDPDebug('RETRANSING: '+l.DebugString);
{$ENDIF}
            //l.h.parts := FWalkingMTU;
            SendPacket(l.h, l.payload, l.payload_length);
            inc(OutOfOrder);
            l.retranstime := GEtTicker;
            l.sent := true;
          end;
        end;
      end
    finally
      txLog.Unlock;
    end;
  finally
    Unlock;
  end;
end;


procedure TReliableUDPEndpoint.HAndleFutureAck(ifo: TReliableUDPPacketLogRecord);
//!!IFOREF - ok? should be okay as long as call to fucntion is under lock
var
  basepack: ni;
  pack: ni;
  t: ni;
  p: pbyte;
  cx: ni;
begin
  p := ifo.payload;
  cx := ifo.payload_length;
  basepack := ifo.h.argument;
  while cx > 0 do begin
    pack := basepack + int64(p^);
    ClearSpecificPacket(pack);
    dec(cx);
    inc(p);
  end;
end;

procedure TReliableUDPEndpoint.HandleIncomingCommandPacket(ifo: TReliableUDPPacketLogRecord);
//!!IFOREF - ok? should be okay as long as call to fucntion is under lock
var
  sz: ni;
begin
  //look at command id and do whatever it says to do
  case ifo.h.command of
    CMD_FUTURE_ACK: begin
      HAndleFutureAck(ifo);
    end;
    CMD_SPLIT: begin
      HandleSplitReceive(ifo);
    end;
    CMD_MTU_TEST: begin
      HandleMTUTest(ifo);
    end;
    CMD_MTU_SUCCESS: begin
      HandleMTUTestResult(ifo);
    end;
    CMD_NOTHING_TO_SEND:
    begin
      //doesn't need to do anything, just updates the SendersBackLog parameter (which is done outside of here)
    end;
    CMD_ACK: begin
{$IFDEF UDP_DEBUG}      Debug.Log(self,'Got ACK <'+inttostr(ifo.h.expected_sequencenumber));{$ENDIF}
//      UDPDebug('Got ACK <'+inttostr(ifo.h.expected_sequencenumber)+' + '+inttostr(ifo.h.argument));

      if ifo.h.expected_sequencenumber > (self._nextsequencenumber + 40000) then begin//wow
        CloseMe := true;
        Error := 'Packet expected sequence number is out of range tolerace.';
      end;
//      ConsoleLog('CLEAR!='+ifo.h.DebugString);
      ClearSpecificPacket(ifo);
      MultiPlexer.hint_stuff_to_do := false;
      //RetransOutgoing(false);
      IF txLog.count = 0 then
        SendNothingToSend();
    end;
    CMD_KEEP_ALIVE: begin
      ResetKeepAliveTimer;
      //CloseRequested := true;
    end;
    RUDP_CMD_REQUEST_RETRANS:
    begin
      //look at expected sequence number and retransmit packet matching
      rsRetransRatio.AddStat(0.0);
      RetransImmediate(ifo.h.expected_sequencenumber);
    end;

    RUDP_CMD_DEBUGINFO: begin
      sz := lesserof(ifo.payload_length, sizeof(self.remoteDebugInfo));
      if (sz < 0) or (sz > sizeof(self.remoteDebugInfo)) then
        raise ECritical.create('WTF');

      if ifo.payload = nil then
        raise ECritical.create('WTF');

      movemem32(@self.remoteDebugInfo, ifo.payload, sz);
    end;

    RUDP_CMD_CLOSE: begin
{$IFDEF UDP_DEBUG}      Debug.Log(self,'******* DESTROY PACKET RECEIVED ******');{$ENDIF}
      if SmartLock then
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
//!!IFOREF - ok? should be okay as long as call to fucntion is under lock
begin
  //put the packet in the data queue
  if SmartLock then
  try
{$IFDEF UDP_DEBUG}    Debug.Log(self,'+DATA: '+ifo.DebugString);{$ENDIF}
//    UDPDebug('Adding '+inttostr(ifo.h.sequencenumber)+' to data log.');
    if not rxDataLog.Has(ifo.h.sequencenumber) then begin
      {$IFDEF REF_DEBUG}Debug.log('AddRef from HandleIncomingDataPacket');{$ENDIF}
      ifo._AddRef;
      IF rxDataLog.count > 10000 then begin
        self.CloseMe := true;
      end;
      rxDataLog.Add(ifo);
      if not multiplexer.stufftodo then
        AckFutureGAps;
      if assigned(OnDataAvailable) then begin
        OnDataAvailable(self);
      end;
    end;
//
{$IFDEF UDP_DEBUG}    Debug.Log(self,'There are now '+inttostr(rxDAtaLog.count)+' packets in the data log.');{$ENDIF}
    FAVailableData := FAvailableData + ifo.payload_length;
{$IFDEF UDP_DEBUG}    Debug.Log(self,'Available data: '+inttostr(FAvailableData));{$ENDIF}

  finally
    Unlock;
  end;

end;


procedure TReliableUDPEndpoint.QueueIncomingPacket(ifo: TReliableUDPPacketLogRecord);
//!!IFOREF - changed - transitions packets to other threads
begin
  ifo._AddRef;
  try


    //if the packet's sequence number is less than
    //our expected sequence number, then drop this packet. (it was an unnessary retrans)
    if (ifo.h.sequencenumber < _nextexpectedSequenceNumber) then begin
{$IFDEF UDP_DEBUG}      Debug.Log(self,'xxxx NOT Added (old) xxxx '+inttostr(ifo.h.sequencenumber)+' < '+inttostr(_nextexpectedSequenceNumber));{$ENDIF}
//{$IFDEF UDP_DEBUG}      //NOT TRUE UDPDebug('packet out of order, so send immediate ack '+inttostr(ifo.h.sequencenumber)+' < '+inttostr(_nextexpectedSequenceNumber));{$ENDIF}
{$IFDEF SLOW_ACK}
      if txLog.count = 0 then
{$ENDIF}
//      ACkIncoming(false);
      latent_ack := true;//force a ack to be send when receiver becomes idle

//      ifo._Release; //should get released in upper laters
      exit;
    end;

    //if the packet requires acknowledgement, then
    //put it in the log
    if ifo.h.Flag_AckType <> ackNone then begin
      if not rxLog.Has(ifo.h.sequencenumber) then begin

        RxLogPacketIfRequired(ifo);

        //when queuing packets we'll always take the entire queue into
        //account when deciding what order to process the information
        //ProcessPacketQueue();
        SystemThreadSignal := true;
{$IFDEF SLOW_ACK}
      if txLog.count = 0 then
{$ENDIF}
        latent_ack := true;
        ACkIncoming(false);


        //IF OUT OF ORDER
        {$IFDEF ALLOW_IMMEDIATE_RETRANS}
        if ifo.h.sequencenumber > self._nextexpectedSequenceNumber then begin
          //immediately request retransmission of missing packet
          REquestImmediateRetransmission;
        end else begin
        end;
        RequestRetransmissionOfFutureGaps;
        {$ENDIF}
      end else begin
        latent_ack := true;
//        ACkincoming(true);
        //RequestRetransmissionOfFutureGaps;

      end;
    end else begin
      //for packets that don't ask for ACK, they can be queued in any order
      //this is sorta a requirement because packets that don't require
      //ACK also do not have sequence numbers.
      //Therefore we should handle this packet immediately
      if SmartLock then
      try
        HandleIncomingPacket(ifo);
      finally
        Unlock;
      end;
    end;
  finally
    EvalSYstemThreadSignal;
    ifo._Release;
  end;




end;


procedure TReliableUDPEndpoint.HandleIncomingPacket(ifo: TReliableUDPPacketLogRecord);
begin

  ResetKeepaliveTimer;
  if (ifo.h.command <> RUDP_CMD_REQUEST_RETRANS) then begin
//    Debug.ConsoleLog('Clearing in reference to: ' + ifo.DebugString);
    ClearTxLogsUpTo(ifo.h.expected_sequencenumber-1);
  end;
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

    if SmartLock then
    try
      if (ifo.h.sequencenumber >= _nextexpectedSequenceNumber)
      and (not (rxLog.Has(ifo.h.sequencenumber))) then begin
        {$IFDEF REF_DEBUG}Debug.log('AddRef from HandleIncomingSystemPacket');{$ENDIF}
        ifo._AddRef;
        rxLog.Add(ifo);

        if ifo.h.Flag_AckType = ackImmediate then begin
          if ifo.h.sequencenumber >= _nextexpectedSequenceNumber then begin
{$IFDEF USE_FEC}
            SendAck(ifo.h.sequencenumber, @ifo.health);
{$ELSE}
            SendAck(ifo.h.sequencenumber, nil);
{$ENDIF}

          end;
          ifo.acked := true;

        end;


      end;
      if ifo.RemoteOrigin then begin
        AddToRXQueue(ifo);
      end;
    finally
      Unlock;
    end;


  end;
end;






procedure TReliableUDPEndpoint.HandleMTUTest(ifo: TReliableUDPPacketLogRecord);
begin
  if SmartLock then
  try
//    Debug.ConsoleLog('Sending MTU test success arg='+inttostr(ifo.h.argument)+' sz='+inttostr(ifo.payload_length));
    SendMTUTEstREsult(ifo.h.argument);//report back to the other end that the MTU test
                                      //worked
  finally
    Unlock;
  end;
end;

procedure TReliableUDPEndpoint.HandleMTUTestResult(
  ifo: TReliableUDPPacketLogRecord);
begin
  if SmartLock then
  try
//    Debug.ConsoleLog('MTU test successful arg='+inttostr(ifo.h.argument)+' sz='+inttostr(ifo.payload_length));
    UpdateMTUTestSuccesses(ifo.h.argument);
  finally
    unlock;
  end;

end;

procedure TReliableUDPEndpoint.SplitPrepareReceive(seq: int64; totalParts: ni);
begin
  if splitass.seq = seq then
    exit;
  splitass.Init;
  splitass.seq := seq;
  splitass.totalParts := totalPArts;


end;

procedure TReliableUDPEndpoint.HandleSplitReceive(
  ifo: TReliableUdPPacketLogRecord);
//!!IFOREF - changed to hold local reference
var
  sh: PRUDPSplitHeader;
  paypay: PByte;
  bComplete: boolean;
  itm: TPacketQueueItem;
begin
  ifo._AddRef;
  try
    //split header will be at the start of the payload
    sh := PRUDPSplitHEader(ifo.payload);

    //partial payload will be at sh+sizeof(sh);
    paypay := ifo.payload + sizeof(TRUDPSplitHEader);

    //prepare the receive buffer to accept the current sequence number
    //--this resets any receive operation that may have been incomplete
    SplitPrepareReceive(sh.sequencenumber, sh.packetof);

    //copy this payload part into final assembly
    movemem32(@splitass.paytemp[sh.startbyte], paypay, sh.bytecount);

    //flags
    bComplete := splitass.FlagPArtCompleteAndReportCompletion(sh.packet);

    if bComplete then begin
      //dispatch
      itm := TPacketQueueItem.Create;
      itm.PeerIP := ifo.OriginatingHost;
      itm.PeerPort := ifo.OriginatingPort;
  {$IFDEF USE_REGISTERED_MEMORY}
      itm.Data := GetRegisteredMemory(sh.totalsize, self.classname+'.TReliableUDPEndpoint.HAndleSplitReceive');
  {$ELSE}
      itm.Data := GetMemory(sh.totalsize);
  {$ENDIF}
      itm.DataSize := sh.totalsize;
      movemem32(itm.data, @splitass.paytemp[0], sh.totalsize);
      itm.ep := self.multiplexer;
      itm.AutoDestroy := true;
      self.Multiplexer.queue_in.AddItem(itm);
      splitass.Init;
    end;
  finally
    ifo._Release;
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


procedure TReliableUDPEndpoint.EvalFloodSignal;
var
  bYes: boolean;
begin
  //bYes := (txLog.count < greaterof(lesserof(bestAckTime,FLOODED_AT_MAX),FLOODED_AT_MIN)) or (CloseState <> csnotClosed);
  bYes := (txLog.count < greaterof(lesserof((bestAckTime*10)/(FlexMTU/1250),FLOODED_AT_MAX),FLOODED_AT_MIN)) or (CloseState <> csnotClosed);
//  if not byes then
//    debug.consolelog('flood');
  Signal(evNotFlooded, bYes);
end;

procedure TReliableUDPEndpoint.EvalIntervalStability;
var
  min,max,dif: double;
begin
  if rsIntervalStability.NewBAtch then begin
    min := rsIntervalStability.Minimum;
    max := rsIntervalStability.MAximum;

    dif := max-min;
    if dif > 0.0004 then begin
      intervalStabilizer := intervalStabilizer * 0.5;
      if intervalSTabilizer < 0.0001 then begin
        intervalStabilizer := 0.0001;
      end;
    end;

    if dif < 0.0002 then begin
      intervalStabilizer := intervalStabilizer * 2;
      if intervalSTabilizer > 2 then begin
        intervalStabilizer := 2;
      end;
    end;





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

function TReliableUDPEndpoint.FlexREtransTime: ni;
var
  workingset, rt, at: ni;
begin
  at := round(lesserof(rsAckTime.PEriodicAverage,1000));
  workingset := txLog.count+queue_out.estimated_queue_size+WAlkingAverage+1;
//  rt := greaterof(bestacktime*workingset, 10);
{$DEFINE FLEX}
{$IFDEF FLEX}
  result := lesserof((at)+((bestAckTime*workingset)), 500);
{$ELSE}
  result := 2000;
{$ENDIF}

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
//!!IFOREF - ok - simple maintenance of lists
begin
  if r.h.Flag_AckType = ackNone then
    exit;

  if not txLog.Has(r.h.sequencenumber) then begin
    {$IFDEF REF_DEBUG}Debug.log('AddRef from TxLogPacketIfRequired');{$ENDIF}
    r._AddRef;
//    r.sendtime := getticker;

    txLog.Add(r);
    EvalFloodSignal;
  end;
end;

procedure TReliableUDPEndpoint.UpdateMTUTestSuccesses(sz: ni);
var
  t: ni;
  hi: ni;
  p: PMTUTestResult;
begin
  hi := 0;
  for t:= 0 to High(mtu_test_results) do begin
    hi := t;
    p := @mtu_test_results[t];
    if p.sz <= sz then
      p.MarkSuccessful
    else
      break;//<--stop, sizes should always ascend
  end;

  //if not the last in the array
  if (hi < high(mtu_test_results)) then begin
    //if we're even 1-byte beyond the limit for the previous MTU zone, then
    //mark that this zone is okay
    p := @mtu_test_results[hi];
    if p.sz < sz then begin
      mtu_test_results[hi+1].MarkSuccessful;
    end;

  end;



end;

procedure TReliableUDPEndpoint.UserThreadExecute(thr: TRUDPUserThread);
begin
//  thr.Endpoint._AddRef;
  try
  //  Debug.Log('User Thread Execute');
    if CloseMe then begin
      self.Detach;
      thr.EndPOint := nil;

      TPM.NoNeedthread(thr);//<--- IMPORTANT, must EXIT... no further operations permitted in this thread
      exit;
    end;


    //if WaitForSignal(self.evUserDAtaIncoming, 1000) then
    DoAvailableData;

    Lock;
    try
      FevUserDataIncoming.Signal(self.AvailableData > 0);
    finally
      Unlock;
    end;
  finally
//    thr.Endpoint._Release;
  end;



end;

procedure TReliableUDPEndpoint.RxLogPacketIfRequired(r: TReliableUDPPacketLogRecord);
//!!IFOREF - ok - simple maintenance of lists
var
  searched: TReliableUDPPacketLogRecord;
begin
  if r.RemoteOrigin = false then
    raise Exception.create('You cannot add a local packet to the RXLog');
  if r.h.Flag_AckType = ackNone then
    exit;

  if not rxLog.Has(r.h.sequencenumber) then begin
    {$IFDEF REF_DEBUG}Debug.log('AddRef from RxLogPacketIfRequired');{$ENDIF}
    r._AddRef;
    rxLog.Add(r);
//    if r.h.Flag_AckType = ackImmediate then begin
//      if r.h.sequencenumber >= _nextexpectedSequenceNumber then
//        SendAck(r.h.sequencenumber);
//      r.acked := true;
//    end;
  end;

  if not rxQueue.Has(r.h.sequencenumber) then begin
    {$IFDEF REF_DEBUG}Debug.log('AddRef from RxLogPacketIfRequired1');{$ENDIF}
    r._AddRef;
    rxQueue.Add(r);

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

procedure TReliableUDPEndpoint.CheckAndSendMTUTest;
//checks if it is time to send an MTU test and
//sends a TEST if it is
var
  p: PMTUTestResult;
  t: ni;
begin
{$IFDEF MTU_TESTS}
  for t:= 0 to high(mtu_test_results) do begin
    p := @mtu_test_results[t];
    if MTU_TEST_PING_REQ[t] > Self.bestAckTime then begin
      if p.TimeToRetest then begin
        SendMTUTEst(p.sz);
        p.lasttest := getticker;
        break;
      end;
    end;
  end;
{$ENDIF}
end;

function TReliableUDPEndpoint.CheckClosed: boolean;
var
  iTimeOut: ticker;
begin
  result := false;

  if connecting then
    iTimeout := CONNECTION_TIMEOUT
  else
    iTimeout := KeepAliveTimeout;

  if tickcount.GetTimeSInce(tmLastSeen) > iTimeout then begin
    if connecting then//<<---- DONT ALLOW TIMEOUT WHILE WAITING ON CONNECTION (allow client to timeout its own wait process)
      exit;
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
    result := true;
  end;

end;

procedure TReliableUDPEndpoint.CheckDebugPacketRate;
var
  tmNow: ticker;
begin
  tmNow := getticker;
  if GetTimeSince(tmNow, self.packetsOutCheckTime) > 1000 then begin
//    Debug.Log(self, 'Packets Out/sec='+inttostr(packetsOut));
    packetsOutCheckTime := tmNow;
    packetsOut := 0;
  end;
end;

procedure TReliableUDPEndpoint.ProcessKeepAliveTimer;
var
  iLocalKeepAlive: nativeint;
begin

//  if gettimesince(lastInComingAckTime) > 60000 then
//    CloseMe := true;

  if CheckClosed then begin
    //nothing
  end
{$IFDEF ACK_KEEP_ALIVES}
  {$Message Error 'you also need to actually change the keep alives to be set flag_acktype, which i didn't do'}
  else if lesserof(tickcount.GetTimeSInce(tmLAstSeen), tickcount.GetTimeSInce(tmLAstKeepAliveSent)) > KeepAliveInterval then begin
{$ELSE}
  else if tickcount.GetTimeSInce(tmLAstKeepAliveSent) > KeepAliveInterval then begin
{$ENDIF}
    SendKeepAlive(GetTimeSince(tmLastSeen) > (KeepAliveInterval shr 1));;
    //it is now safe to reset the keep alive timer because we've requested
    //acknowledgement of our last packet, which should kick keep alives into high gear
//    ResetKeepAliveTimer;
//    windows.beep(400,50);
  end;


end;

procedure TReliableUDPEndPoint.SendKeepAlive(bRequestAcknowledgement: boolean);
var
  h: TReliableUDPHeader;
begin
  if multiplexer.shuttingdown then
    exit;

  if not connected then
    exit;

  if SmartLock then
  try
    h.Init(self);
    h.Flag_Command := true;
    h.command := CMD_KEEP_ALIVE;
    if bRequestAcknowledgement then begin
      h.Flag_AckType := ackImmediate;
    end else begin
      h.Flag_AckType := ackNone;
    end;

    h.PrepareToSend(self);
//    Debug.log('SENDING KEEP ALIVE');
    self.SendPAcket(h, nil, 0);
//    Debug.log('KEEP ALIVE SENT');
    tmLAstKeepAliveSent := tickcount.GetTicker;

  finally
    Unlock;
  end;

end;

procedure TReliableUDPEndpoint.AddToRXQueue(ifo: TReliableUDPPacketLogRecord);
//!!IFOREF - ok - simple maintenance of lists
begin
  if SmartLock then
  try
    if not ifo.RemoteOrigin then
      raise Exception.create('Queued packets must be of remote origin');


{$IFDEF UDP_DEBUG}  Debug.Log(self,'CurrentThread:'+inttostr(TThread.CurrentThread.ThreadID));{$ENDIF}
{$IFDEF UDP_DEBUG}  Debug.Log(self,'Queueing:'+ifo.DebugString);{$ENDIF}

  if not rxQueue.Has(ifo.h.sequencenumber) then begin
      {$IFDEF REF_DEBUG}Debug.log('AddRef from AddToRXQueue');{$ENDIF}
      ifo._AddRef;
      rxQueue.Add(ifo);

    end;
  finally
    Unlock;
  end;

end;

function TReliableUDPEndpoint.AnyTXPacketsTimedOut: boolean;
//!!IFOREF - external lock?
var
  at: ticker;
  res: boolean;
begin
  at := FlexREtransTime;
  res := false;
  txLog.Iterate(
    procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
    begin
      if gettimesince(TtreeItem_ReliableUDPPacketLogRecord(ABtreeItem).packet.retranstime) > at then begin
        ANeedStop := true;
        res := true;
      end;
    end
  );

  result := res;
end;

procedure TReliableUDPEndpoint.ProcessPacketQUEUE;
var
//  r: TReliableUDPPacketLogRecord;
  ti: Ttreeitem_ReliableUDPPacketLogRecord;
  ifo: TReliableUDPPacketLogRecord;
begin
  //look at next expected sequence number
  //interpret what to do with this packet
  while true do begin
    if thr.stoprequested or thr.terminated then
      exit;
    if SmartLock then
    try
      if rxQueue.count = 0 then exit;
//      rxQueue.Lock;
      try
{$IFDEF UDP_DEBUG}        Debug.Log(self,'Looking for '+inttostr(_nextQueuedSequencenumber)+' in queue.');{$ENDIF}
        ti := rxQueue.Find(self._nextQueuedSequenceNumber);
        if ti= nil then break;
{$IFDEF EXTRA_DEAD_CHECKS}
        ti.DeadCheck;
{$ENDIF}
        ifo := ti.packet;
{$IFDEF EXTRA_DEAD_CHECKS}
        ifo.DeadCheck;
{$ENDIF}



{$IFDEF UDP_DEBUG}        Debug.Log(self,'HANDLING: '+ifo.DebugString);{$ENDIF}
        if not ifo.h.Flag_CReate then
          HandleIncomingPacket(ifo);

        ifo.queue_handled := true;
        inc(_nextQueuedSequenceNumber);
        if rxQueue.Has(ifo.h.sequencenumber) then begin
          rxQueue.Remove(ifo);
          {$IFDEF REF_DEBUG}Debug.log('RElease from ProcessPacketQUEUE');{$ENDIF}
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

procedure TReliableUDPEndpoint.CalcNewWalkingAverage;
begin

{$IFDEF OLDSHIT}
//      if rsRetransRatio.NewBAtch then
//      begin
//        wa := rsRetransRatio.GetAverage;
//        debug.log(self,'wa ='+floatprecision(wa,2)+' walking average='+inttostr(walkingaverage));
//        if wa < (0.99) then begin
//          if walkingaverage > 1 then begin
//            dec(walkingaverage);
//            debug.log(self,'--wa = '+inttostr(walkingaverage));
//
//          end;
//        end;
//        if wa > 0.99 then begin
//          if walkingaverage < MAX_PACKET_WRITE_AHEAD then begin
//            inc(walkingaverage);
//            debug.log(self,'++wa = '+inttostr(walkingaverage));
//
//          end;
//        end;
//
//        if walkingaverage < MIN_PACKET_WRITE_AHEAD then walkingaverage := MIN_PACKET_WRITE_AHEAD;
//
//        if walkingaverage > MAX_PACKET_WRITE_AHEAD then
//          walkingaverage := MAX_PACKET_WRITE_AHEAD;
//      end;
//

{$ENDIF}
end;

procedure TReliableUDPEndpoint.SendTxLogs(bOnlyUnsent: boolean; bCombine: boolean);
begin
{$IFDEF PACKET_COMBINATION}
  SendTxLogs_Complex(bOnlyUnsent, bCombine);
{$ELSE}
  SendTxLogs_Simple(bOnlyUnsent, bCombine);
{$ENDIF}
end;

procedure TReliableUDPEndpoint.SendTxLogs_Simple(bOnlyUnsent: boolean; bCombine: boolean);
//!!IFOREF - changed - interacts with other threads/queues, needs to hold local references during that
var
  ti: Ttreeitem_ReliableUDPPacketLogRecord;
  l: TReliableUDPPacketLogRecord;
  rt,at: ticker;
  tmNow: ticker;
{$IFDEF INTERVAL_SPIN}
  intv: ticker;
{$ENDIF}
  workingset,cx: ni;
  bigloops: ni;
  curtoken: ticker;
  buffertimedout: boolean;
  idx: ni;
  tmSince: ticker;
  fmtu: ni;
begin
  tmSince := gettimesince(lastTXLoopTime);
{x$DEFINE TX1}
{$IFDEF TX1}
  if bestAckTime > tmSince then begin
//    sleep(greaterof(0, round(0.1*(tmSince-bestAckTime))));
    exit;
  end;
{$ENDIF}

//{$IFNDEF USE_FIBERS}
  if queue_out.estimated_queue_size > 0 then begin
    exit;
  end;
//{$ENDIF}

  CheckAndSendMTUTest;//if we need to try some new tests, try them

//  if gettimesince(lastInComingPeriodicAckTime) > 5 then begin
//    if random(100) > 98 then
//      exit;
//  end;


  if SmartLock then
  try

    buffertimedout := false;
    curtoken := lastTXLoopTime;
//    sent := 0;
    bigloops := 1;
    workingset := walkingaverage;
    //if bimmediate then wa := wa shr 1;
    cx := workingset+2;
//    if not bOnlyUnsent then
//      cx := 1;


    rt := FlexRetransTime;

{$IFDEF STALLWALK}
    if (txAckLevel_AtLastTX = txAckLevel) {and (gettimesince(lastsendtime) > 50)} then begin
//      Debug.Consolelog('STALLED');
      stallwalk := stallwalk - STALL_WALK_DEC;
      if stallwalk < 0 then
        stallwalk := 0;
{$IFDEF STALL_EMPHASIS}
      if queue_out.estimated_queue_size = 0 then
        rt := lesserof(((bestAckTime+1))+(200*round(queue_out.ticks_per_byte)),100);

{$ENDIF}
    end else begin

//      Debug.Consolelog('RUNNING');
      if stallwalk < MAX_PACKET_WRITE_AHEAD then
        stallwalk := stallwalk + STALL_WALK_INC;
      cx := round(stallwalk + 1);

    end;
{$ENDIF}

    //walkingaverageFine := round(stallwalk) shl WA_SHIFT;

//    if (rt<>0) and (not AnyTXPacketsTimedOut) then
//      exit;


    fmtu := FlexMTU;
    while bigloops > 0 do begin
      dec(bigloops);

      {$IFDEF INTERVAL_SPIN}
      intv := lesserof(round(rsAckTime.Interval * 0.95), 100);
      {$endif}
      idx := 0;
      txLog.Iterate(
        procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
        var
          tt: ni;
        begin
          if idx > workingset then begin
            ANEedStop := true;
            exit;
          end;
          ti := TtreeItem_ReliableUDPPacketLogRecord(ABTreeItem);
          ti._AddRef;
          try
            l := ti.packet;

            if (not l.sent)
            or (gettimesince(l.retranstime) > lesserof(500,((rt+(rt*idx))*(l.retranscount+1))))
            or ((l.looptoken_send=curtoken) or (l.looptoken_resend=curtoken))
            then begin
              {$IFDEF INTERVAL_SPIN}
              while GetTimeSInceHR(lastSendTimeHR) < (intv * 100000) do begin
                sleeptm := GetTimeSInceHR(lastSendTimeHR) div 100000;
                if sleeptm > 0 then
                  sleep(sleeptm)
                else
                  inc(spins);
              end;
              {$ENDIF}
              //Debug.ConsoleLog(inttostr(GetTimeSInceHR(lastSendTimeHR)));
              tmNow := GetTicker;
              //Debug.Log('bigloops='+inttostr(Bigloops)+ ' idx='+inttostr(idx)+' cx='+inttostr(cx));
              //Debug.Log('txAckLevel='+inttostr(txAckLevel));
              if not l.sent then begin
                {$IFDEF DUMB}l.h.SendersBackLog := lesserof(_nextsequencenumber - _unclearedsequencenumber, 255);{$ENDIF}
              end;
              if (l.retranscount >= RETRIES_BEFORE_SPLIT) or (l.payload_length > fmtu) then begin
                {$ifndef  ALWAYS_SPLIT_ASS}
                for tt := high(mtu_test_results) downto 1 do begin
                  mtu_test_results[tt].REset;
                  if mtu_test_results[tt].sz < (greaterof(l.payload_length, fmtu)) then
                    break;
                end;
                {$ENDIF}

                Multiplexer.SplitSend(self, l.h, l.payload, l.payload_length);
                ANeedStop := true;
              end else begin
                {$ifdef  ALWAYS_SPLIT_ASS}
                  if l.payload_length > 450 then
                    Multiplexer.SplitSend(self, l.h, l.payload, l.payload_length)
                  else
                    Multiplexer.SendDataFromEndPoint(self, l.h, l.payload, l.payload_length);
                {$ELSE}
                  Multiplexer.SendDataFromEndPoint(self, l.h, l.payload, l.payload_length);
                {$ENDIF}
              end;
  //            if l.h.sequencenumber > 30 then
  //              ClearSpecificPacket(l.h.sequencenumber);
              txAckLevel_AtLastTX := txAckLevel;
  //            if gettimesince(tmNow) > 1 then begin
  //              exit because buffering took too long
  //              buffertimedout := true;
  //              ANeedStop := true;
  //            end;

              if (not l.sent) then begin
                if (l.LOOPtoken_send <> curtoken) then begin
                  l.sent := true;
                  l.sendtime := tmNow;
                  l.SendTimeTag := 1;
  //                inc(sent);
                  inc(transmissions);
                  l.looptoken_send := lastTXLoopTime;
                  l.watchme := random(100) > 90;
      {$IFDEF BEEPS}
                  if l.watchme then
                    SysBeep(1000,50);
      {$ENDIF}
  //                Debug.ConsoleLog('  trans '+inttostr(l.h.sequencenumber));
                  if queue_out.estimated_queue_size > 0 then
                    dec(cx);
                end;
              end else begin
  {$IFDEF DEBUG_RETRANS}
                Debug.ConsoleLog('Retrans '+inttostr(l.h.sequencenumber)+' age:'+inttostr(gettimesince(l.retranstime))+' > '+inttostr(rt));
  {$ENDIF}
                l.resent := true;
                l.retranscount := l.retranscount + 1;

                if not (l.LOOPtoken_resend = curtoken) then begin
                  WalkFlexMTU(true);
                  inc(Retransmissions);
                  l.looptoken_resend := curtoken;//<----wierd experiment, record the time at the beginning of this loop so we can re-tx packets in ths same loop
                end;
              end;
              l.retranstime := getticker;
            end;

            if self.Multiplexer.stufftodo then begin

    //          walkingaveragefine := sent shl 4;
              ANeedStop := true;
              //exit; //do not push packets out if there are packets waiting to be processed
            end;


  {$DEFINE TX3}
  {$IFDEF Tx3}
  //          if queue_out.estimated_queue_size > 0 then
  //            dec(cx);
  {$ENDIF}
            inc(idx);
            ANeedStop := cx = 0;
          finally
            ti._Release();
          end;
        end
      );

{x$DEFINE TX2}
{$IFDEF Tx2}
      if multiplexer.stufftodo then
        exit;
{$ENDIF}
      if buffertimedout then begin
        //Debug.Consolelog('buftm!');
        exit;
      end;
    end;

    lastTXLoopTime := getticker;
  finally
    Unlock;
    //sleep(0);
  end;

  CheckDebugPacketRate;
end;


procedure TReliableUDPEndpoint.SendTxLogs_Complex(bOnlyUnsent: boolean; bCombine: boolean);
var
  t: nativeint;
  ti: Ttreeitem_ReliableUDPPacketLogRecord;
  l: TReliableUDPPacketLogRecord;
  rt,at: ticker;
//  wa: nativefloat;
  writecount: ni;
  tmNow: ticker;
  tmWait: ticker;
  intv: ticker;
  bUpdate: boolean;
  workingset,cx: ni;
  rtemp: nativefloat;
  sz: ni;
  al: TALHEader;
  a: array[0..31] of TReliableUDPPacketLogRecord;
  ax: ni;
  procedure NewPackage;
  begin
    self.txTemp[0] := 65;
    self.txTemp[1] := 76;
    sz := 2;
  end;
  procedure CommitPackage;
  begin
    if sz > 2 then begin
//      Multiplexer.SendDataFromEndpoint(self, @txTemp[0], sz);
      queue_out.QueueSendData2(self, gethighresticker, Self.FRemoteHost, FRemoteport, @txTemp[0], sz);
    end;
    NEwPackage;
  end;
  procedure COmmitArray;
  begin
    while ax > 0 do begin
      dec(ax);
      l := a[ax];
      l.sendtime := getticker;
      l.SendTimeTag := 2;
      l.retranstime := getticker;
      l.sent := true;

    end;
  end;

begin
  ax := 0;
  if SmartLock then
  try
    workingset := MAX_PACKET_WRITE_AHEAD;
    NewPackage;

    //if bimmediate then wa := wa shr 1;
    cx := workingset+1;
    txLog.Iterate(
      procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
      begin

        ti := TtreeItem_ReliableUDPPacketLogRecord(ABTreeItem);
        l := ti.packet;


        if (ax<length(a)) and ((l.CompleteSize+sizeof(TALHEader)) < (FlexMTU-sz)) then begin
          if (not bOnlyUnsent) or (not l.sent) then begin
            if not l.acked then begin//NEVER send acked packets, that's just stupid
              al.sz := l.CompleteSize;
              movemem32(@txTemp[sz], @al, sizeof(al));
              inc(sz, sizeof(al));
              movemem32(@txTemp[sz], @l.h, sizeof(l.h));
              inc(sz, sizeof(l.h));
              //Debug.log('Repacking LOGS:'+l.DebugString);
              movemem32(@txTemp[sz], l.payload, l.payload_length);
              inc(sz, l.payload_length);
              a[ax] := l;
              inc(ax);
            end;
          end;
        end else begin
          if (sz > 2) then begin
            //Debug.Log('SEND Resend '+inttostr(ti.packet.h.sequencenumber));
            //Multiplexer.SendDataFromEndpoint(self, @txTemp[0], FlexMTU);
//            queue_out.QueueSendData(gethighresticker, Self.FRemoteHost, FRemoteport, 0,0);
            queue_out.QueueSendData2(self,gethighresticker, Self.FRemoteHost, FRemoteport, @txTemp[0], sz);
            rsRetransRatio.AddStat(0);
//            Debug.log('Commit Logs2');
          end;
          sz := 2;
          //copied from commitarray because it can't be called from anonymous method
          while ax > 0 do begin
            dec(ax);
            l := a[ax];
            l.sendtime := getticker;
            l.SendTimeTag := 3;
            l.retranstime := getticker;
            l.sent := true;
          end;
        end;

        if self.Multiplexer.stufftodo then begin
           ANeedStop := true;
           //debug.log('stuff to do');
            exit; //do not push packets out if there are packets waiting to be processed
        end;


        //////////////////////////////////////////
        dec(cx);
        ANeedStop := cx = 0;


      end
    );
//    if txLog.count = 1 then begin
    if sz > 2 then begin
      CommitPackage;
      CommitArray;
      rsRetransRatio.AddStat(1.0);
      ax := 0;
    end;
//    end;
  finally
    Unlock;
    //sleep(0);
  end;
end;

procedure TReliableUDPEndpoint.RetransOutgoing(bimmediate: boolean);
var
  t: nativeint;
  ti: Ttreeitem_ReliableUDPPacketLogRecord;
  l: TReliableUDPPacketLogRecord;
  rt,at: ticker;
//  wa: nativefloat;
  writecount: ni;
  tmNow: ticker;
  tmWait: ticker;
  intv: ticker;
  bUpdate: boolean;
  workingset,cx: ni;
  rtemp: nativefloat;
  bShouldSend: boolean;
  sent: ni;
begin
  raise ECritical.create('Deprecated');
  if SmartLock then
  try
    sent := 0;
    tmNow := GetTicker;
    bUpdate := false;
    inc(retransloops);
    if GetTimeSInce(tmNow, retransLoopCheckTime) > 1000 then begin
{$IFDEF PERIODIC}
      Debug.log(self, 'Retrans Loop rate='+inttostr(retransloops));
{$ENDIF}
      retransloops := 0;
      retransLoopCheckTime := tmNow;
      bUpdate := true;
    end;
//    txLog.Lock;
    try
      at := round(rsAckTime.PEriodicAverage) * 2;
{$IFDEF PERIODIC}
      if bUpdate then
        debug.log(self,'at ='+inttostr(at));
{$ENDIF}
      sendinterval := at shr 1;




//      if ia > at then
//        ia := at;
      //wa := rsRetransRatio.PeriodicAverage;

      rt := FlexRetransTime;
      writecount := 0;
      rTemp := (at * workingset);
      if rTemp = 0 then
        rTemp := 1;
//      workingset := round(greaterof(1,lesserof(workingset, 1/rTemp)));
      workingset := walkingaverage;
      workingset := lesserof(txLog.Count-1, workingset);


//      if bUpdate then begin
//        debug.log(self, RUDPMon.GetDebugInfoString);
//      end;
{$IFDEF PERIODIC}
      if bUpdate then begin
        //debug.log(self, RUDPMon.GetDebugInfoString);
        debug.log(self,'txLog.count ='+inttostr(txLog.count));
        debug.log(self,'rt ='+inttostr(rt));
        //debug.log(self,'wa ='+floatprecision(wa,2));
      end;
{$ENDIF}



      //if bimmediate then wa := wa shr 1;
      //if not bImmediate then
      workingset := greaterof(1,workingset);
      cx := workingset;
      if cx > 0 then
      txLog.Iterate(
        procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
        begin
          ti := TtreeItem_ReliableUDPPacketLogRecord(ABTreeItem);
          ti._AddRef;
          try
            l := ti.packet;
            {$DEFINE AGGRESSIVE_BACKOFF}
            {$IFDEF AGGRESSIVE_BACKOFF}
            if self.Multiplexer.stufftodo then begin
              //walkingaveragefine := sent shl 4;
              //debug.log('stuff to do');
              ANeedStop := true;
              exit; //do not push packets out if there are packets waiting to be processed
            end;
            {$ENDIF}

            tmNow := GetTicker;
            tmWait := gettimesince(GetHighResTicker, lastSendTimeHR);

            if true then begin
              if l.acked then begin
                debug.log(self,'acked already');
                exit;
              end;
              bShouldSend := true;
              //if already sent then....
              if l.sent then begin
                bShouldSend := GetTimeSince(l.retranstime) > (1+(bestAckTime * WalkingAverage));
                IF (bSHouldSend) then begin
                  {$IFDEF UDP_DEBUG}
                    Debug.Log(self,'Check RETRANSING X: '+l.DebugString);
                  {$ENDIF}

                  //retrans if time
                  l.h.Flag_Retrans := true;
                  if (l.h.Flag_Retrans) then begin
                    if (gettimesince(tmNow, l.retranstime) < rt) then begin
                      ANeedStop := true;
                      exit;
                    end;
                  end
                  else begin
                    if (gettimesince(tmNow, l.sendtime) < rt) then begin
                      ANeedStop := true;
                      exit;
                    end;
                  end;
                  if not l.h.Flag_Retrans then begin
                    rsRetransRatio.AddStat(-1.0);
                    CalcNewWalkingAverage;
                  end else begin
                    rsRetransRatio.AddStat(0.0);
                    CalcNewWalkingAverage;
                  end;
                  //l.retranstime := tmNow;//<---DONT DO THIS. SendPacket checks it then sets it
                  l.h.Flag_Retrans := true;
                  inc(retransmissions);
                end;
              end else begin
                l.sendtime := tmNow;
                l.SendTimeTag := 4;
                rsRetransRatio.addstat(1.0);
                CalcNewWalkingAverage;
              end;

              //BEFORE re-TX
              //tell the other end what our current ack level is
              l.sent := true;
              l.h.expected_sequencenumber := self._nextexpectedSequenceNumber;
              {$IFDEF DUMB}l.h.SendersBackLog := lesserof(_nextsequencenumber - _unclearedsequencenumber, 255);{$ENDIF}

              if not bImmediate then
              begin
                intv := round(sendInterval /10000);

    {$IFDEF DO_INTERVALS}
                while gettimesince(GetHighResTicker, lastSendTimeHR) < sendInterval do begin
      //            if intv > 0 then
      //              exit;

                end;
    {$ENDIF}
              end;
    //          ConsoleLog('TX='+l.DebugString);
              //-------------------------------------------------------------
              CheckDebugPacketRate;

              if SendPacket(l.h, l.payload, l.payload_length) then begin
                inc(packetsOut);

                //windows.beep(1000,50);
                random_retrans := random(5);
                inc(writecount);
                inc(sent);

                if writecount >= walkingaverage then begin
                  ANeedStop := true;
                  exit;
                end;
                //^^^this line is irrelevant, I KNow.
              end;


              if self.Multiplexer.stufftodo then begin
                //debug.log('stuff to do');
                //walkingaveragefine := sent shl 4;
                ANeedStop := true;
                exit; //do not push packets out if there are packets waiting to be processed

              end;
            end;

            //////////////////////////////////////////
            dec(cx);
            ANeedStop := cx = 0;
          finally
            l._Release;
          end;
        end


      );
    finally
//      txLog.Unlock;
    end;
  finally
    Unlock;
    //sleep(0);
  end;

end;

function TReliableUDPEndpoint.SendPacket(h: TReliableUDPHeader; p: pbyte;
  len: nativeint): boolean;
var
  ti: Ttreeitem_ReliableUDPPacketLogRecord;
  r: TReliableUDPPacketLogRecord;
  tm: ticker;
begin
  result := false;
{$IFDEF SEND_RECEIVE_DEBUG}
  if h.Flag_Retrans then
    Debug.Log(self, '[ReSend] '+h.DebugString)
  else
    Debug.Log(self, '[Send] '+h.DebugString);
{$ENDIF}

  if SmartLock then
  try
    if h.Flag_AckType = ackNone then
      ti := nil
    else
      ti := txLog.Find(h.sequencenumber);
    if ti <> nil then begin
      r := ti.packet;
      if GetTimeSince(r.retranstime) > STALE_RETRANS_TIME then begin
        {$IFDEF UDP_DEBUG}  Debug.Log(self,'Retrans SendPacket seq:'+inttostr(h.sequencenumber));{$ENDIF}

        Multiplexer.SendDataFromEndPoint(self, r.h, r.payload, r.payload_length);//tagged
        result := true;
        lastsendtime := getticker;
        lastsendtimeHR := GetHighResTicker;
        tmLAstKeepAliveSent := tickcount.GetTicker;
        r.retranstime := getticker;
        {$IFDEF UDP_DEBUG}
          Debug.Log(self,'RETRANSMIT:'+r.DebugString);
        {$ENDIF}
      end;
    end else begin
{$IFDEF UDP_DEBUG}      Debug.Log(self,'SendPacket seq X:'+inttostr(h.sequencenumber)+' Logging...');{$ENDIF}
      r := TReliableUDPPacketLogRecord.create(self, h, p{}, len);
      if h.parts = 1 then
        h.parts := WAlkToParts(FWalkingMTU);
//      Debug.log('refs:'+inttostr(r.refcount));
{$IFDEF MORE_REFS2}
      {$IFDEF REF_DEBUG}Debug.log('AddRef from SendPacket');{$ENDIF}
      r._AddRef;
{$ENDIF}
      try
//        Debug.log('refs:'+inttostr(r.refcount));
        TxLogPacketIfRequired(r);
//        Debug.log('refs:'+inttostr(r.refcount));
{$IFNDEF ALWAYS_USE_SendTXLogs}
        if (true)//(bestAckTime > 30)
        or  (r.h.GetFlagAck = ackNone)
        //or (r.payload_length = FlexPayloadLimit)
        then begin
          {$IFDEF DUMB}r.h.SendersBackLog := lesserof(_nextsequencenumber - _unclearedsequencenumber, 255);{$ENDIF}
          Multiplexer.SendDataFromEndPoint(self, r.h, r.payload, r.payload_length);
          result := true;
          tm := GEtTicker;
          if not r.sent then begin
            r.sent := true;
            r.sendtime := tm;
            r.SendTimeTag := 5;
          end;
          lastsendtime := tm;
          lastsendtimeHR := GetHighResTicker;
          r.retranstime := tm;

          tmLAstKeepAliveSent := tickcount.GetTicker;
{$IFDEF UDP_DEBUG}          Debug.Log(self,'Sent:'+r.DebugString);{$ENDIF}
        end else begin
          SendTxLogs(true, true);
          //lastsendtime := 0;
        end;
{$ELSE}
        if (r.h.GetFlagAck = ackNone) then
          Multiplexer.SendDataFromEndPoint(self, r.h, r.payload, r.payload_length)
        else begin
          {$IFDEF SEND_TX_IMMEDIATELY}
          SendTxLogs(true, true);
          {$ENDIF}
        end;
{$ENDIF}


      finally
//        Debug.log('refs:'+inttostr(r.refcount));
        {$IFDEF REF_DEBUG}Debug.log('Release from SendPacket2');{$ENDIF}
        r._Release;

      end;
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
  h.tcid := self.TOconnectionid;
  h.fcid := self.FROMConnectionId;
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
{$IFDEF USE_REGISTERED_MEMORY}
    pp := GetRegisteredMemory(len, self.classname+'.TReliableUDPEndpoint.SendPacketCopyPayload');
{$ELSE}
    pp := GetMemory(len);
{$ENDIF}
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


procedure TReliableUDPEndpoint.SetCloseState(const Value: TCloseState);
begin
//  UDPDEbug('Setting Close State '+ord(value).tostring+' in thread id '+getcurrentthreadid.tostring);
  FCloseState := Value;
end;

procedure TReliableUDPEndpoint.SEtSystemThreadSignal(b: boolean);
begin
  IF eet<> nil then begin
    if b then begin
      eet.RunHot := false;
      eet.ColdRunInterval := 1;
    end else begin
      eet.RunHot := false;
      eet.ColdRunInterval := 500;
    end;
  end;

end;

procedure TReliableUDPEndpoint.SEtUserThreadSignal(b: boolean);
begin
  FEvUserDataIncoming.Signal(b)
//  user_thread.HasWork := b;

end;

function TReliableUDPEndpoint.SmartLock: boolean;
begin
  if noMorePacketsAllowed then begin
    sleep(100);
    exit(false);
  end;
  Lock;
  exit(true);
//
//  while true do begin
//    if noMorePacketsAllowed then exit(false);
//    if TryLock then exit(true) else sleep(1);
//
//  end;
end;

procedure TReliableUDPEndpoint.SayHello;
var
  h: TReliableUDPHeader;
begin
{$IFDEF UDP_DEBUG}  Debug.Log(self,'*************SAYING HELLO');{$ENDIF}
  if connected then begin
    if SmartLock then
    try
      h.Init(self);
      h.tcid := self.TOconnectionid;
      h.fcid := self.FROMConnectionId;
      h.Flag_System := false;
      h.command := RUDP_CMD_DEBUGINFO;;
      h.Flag_AckType := ackImmediate;
      h.Flag_Command := true;
      h.PrepareToSend(self);
      SendPacketCopyPayLoad(h, Pbyte(@localdebugInfo), sizeof(localdebuginfo));
    finally
      Unlock;
    end;
  end;

end;

procedure TReliableUDPEndpoint.StartDisconnect;
var
  h: TReliableUDPHeader;
begin
  Signal(evNotFlooded, true);
  if not (closestate = csNotClosed) then
    exit;

{$IFDEF UDP_DEBUG}  Debug.Log(self,'*************DISCONNECT INITIATED');{$ENDIF}
  if connected then begin
    if SmartLock then
    try
      h.Init(self);
      h.tcid := self.TOconnectionid;
      h.fcid := self.FROMConnectionId;

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

function TMultiplexedUDPEndpoint.StuffTodo: boolean;
begin
//  result := hint_stuff_to_do;
  result := false;
//  if not result then
//    result := queue_in.estimated_queue_size > 0;
end;

procedure TReliableUDPEndpoint.SystemThreadExecute(thr: TRUDPSystemThread);
var
  tmNow: ticker;
  st: ni;
begin
  st := round(rsAckTime.PeriodicAverage) shr 4;

  if st > 0 then begin
{$IFDEF USE_FIBERS}
    if not thr.slept then begin
      thr.sleepfor(st);
      exit;
    end;
    thr.slept := false;
{$else}
    sleep(st);
{$ENDIF}
  end;



//  UDPDebug('SYSTEM THREAD:'+thr.ThreadID.ToString);
  self.thr := thr;
  tmNow := GEtTicker;
  try
    //thr.IdleStateTasks;
    if closeorigin <> coNotClosed then begin
{$IFDEF UDP_DEBUG}      Debug.Log(self,'System thread running while waiting for elegant closure.');{$ENDIF}
      thr.RunHot := false;
    end;

    if CloseMe then begin
{$IFNDEF USE_FIBERS}
      if IsSignaled(thr.evStop)
        then exit;
{$ENDIF}
      //UDPDebug('Self@'+inttohex(nativeint(pointer(self)), 16)+' CloseMe found at start of SystemThread, OrderlySuspendStatus = '+booltostr(thr.OrderlySuspend)+' WasCreatedWithUserThread='+booltostr(WasCReatedWithUserThread)+' cid='+inttostr(connectionid));
{$IFDEF UDP_DEBUG}      Debug.Log(self,'Self@'+inttohex(nativeint(pointer(self)), 16)+' CloseMe found at start of SystemThread');//, Jam='+booltostr(IsSignaled(thr.evStop))+'  Jammed='+booltostr(IsSignaled(thr.evJammed))+'  UnJammed='+booltostr(IsSignaled(thr.evStopped))+' WasCreatedWithUserThread='+booltostr(WasCReatedWithUserThread)+' cid='+inttostr(connectionid));
{$ENDIF}
      //evUserThread.ResetEvent;
      //evUserthread.SetEvent;

      //I DONT THINK WE SHOULD GET THIS FAR
      //if closeme is set, then that means that we're REALLY dead... we should just exit
      //the managedthread code was tightened up, so calls to Stop, should also
      //set all appropriate signals to make sure we never make it here.
      thr.RunHot := false;
//      self.Multiplexer.DeregisterEndPOint(self);
//      self.eet := nil;
      if CanDestroyBySystemThread then
      begin
//        GarbageCollect(self);
        self.Detach;
        //thr.Endpoint := nil;//!!! thr is invalid at this point

      end else
        self.PrepareForDestruction(coclient);
      //TPM.NoNeedthread(thr);
      exit;
//      raise ECritical.Create('We should never make it here');
    end;
    //UDPDebug('Running Idle Tasks');
//    if TryLock then
//    try
//      s := DebugMEssage;
//      if s <> '' then
//        thr.Status := stringreplace(s, NEWLINE, ' ', [rfREplaceAll]);
//      finally
//      Unlock;
//    end;


    ProcessElegantClosure;
{$IFDEF SLOW_ACK}
    if (not assigned(multiplexer) or (not multiplexer.hint_stuff_to_do)) then begin
{$ENDIF}
//    if (GetTimeSince(LastACkTime) > 500) and (txLog.count >0) then begin
//      AckIncoming(true);
//    end else
      if (queue_pipe_in.estimated_queue_size = 0) or (gettimesince(systm_REtransLoop) > 500) then
        AckIncoming(latent_ack);
{$IFDEF SLOW_ACK}
    end;
{$ENDIF}
    if gettimesince(processpacketqueuetime) > 100 then begin
      ProcessPAcketQueue;
      processpacketqueuetime := getticker;
    end;
    if gettimesince(systm_RetransLoop) > 0 then begin
{$IFNDEF USE_SHARED_QUEUES}
      repeat
        if (queue_pipe_in.estimated_queue_size = 0) or (gettimesince(systm_REtransLoop) > 500) then
          SendTXLogs(false, true);

      until queue_pipe_in.estimated_queue_size = 0;
{$ELSE}
      if (queue_pipe_in.estimated_queue_size = 0) or (gettimesince(systm_REtransLoop) > 500) then
        SendTXLogs(false, true);
{$ENDIF}
//      RetransOutgoing(false);
      systm_REtransLoop := getticker;
    end;
    if gettimesince(systm_KeepAlive) > 4000 then begin
      ProcessKeepAliveTimer;
      systm_KeepAlive := getticker;
    end;

{$IFDEF DuMB}
    if SendersBackLog > 1 then
      PeriodicAck;
{$ENDIF}

    EvalSystemThreadSignal; //???? Should this be allowed in this context... maybe the multiplexer should be solely responsible for evaluation
    EvalUserThreadSignal;//c# -- check readdata and write data as well

//    if AvailableData > 0 then begin
//      DoAvailableData;
//    end;

  except
    on E: Exception do begin
      UDPDebug('Exception in RUDP System Thread: '+e.Message);
      Connecting := false;
      CloseState := csError;
      CloseMe := true;
    end;
  end;
  systemthreadtime := gettimesince(tmNow);

end;

function TReliableUDPEndpoint.SendData(p: pbyte; len: nativeint): nativeint;
//!!iforef
//CONTEXT UserThread - ALWAYS
var
  h: TReliableUDPHeader;
  pp,m1,m2: pbyte;
  tm1, tm2, diff: ticker;
  merge: TReliableUDPPacketLogRecord;
  bDoMerge: boolean;
begin
  result := 0;
  bdoMerge := false;
{$IFDEF FLOODCHECK}
   while not WaitForSignal(evNotflooded, 10) do begin
    if CheckClosed then
      exit;
  end;
{$ENDIF}


  if SmartLock then
  try
    if not CloseMe then begin
      merge := nil;
{$IFDEF ALLOW_PACKET_MERGE}
      if self.txLog.count > 0 then begin
        merge := TtreeItem_ReliableUDPPacketLogRecord(txLog.LastItem).packet;
        if (not merge.sent) and (merge.payload_length < FlexPayloadLimit) then
          bDoMerge := true;
      end;
{$ENDIF}

      if bDoMerge then begin
        m1 := p;
        m2 := merge.payload;
        result := lesserof(FlexPayloadLimit-merge.payload_length, len);
{$IFDEF USE_REGISTERED_MEMORY}
        merge.payload := GetRegisteredMemory(merge.payload_length+result, self.classname+'.TReliableUDPEndpoint.SendData');
{$ELSE}
        merge.payload := GetMemory(merge.payload_length+result);
{$ENDIF}
        movemem32(@merge.payload[0], m2, merge.payload_length);
        movemem32(@merge.payload[merge.payload_length], @p[0], result);
        merge.payload_length := merge.payload_length+result;
        //FreeRegisteredMemory(m1);
{$IFDEF USE_REGISTERED_MEMORY}
        FreeRegisteredMemory(m2);
{$ELSE}
        FreeMemory(m2);
{$ENDIF}
      end else begin

        //create a data packet
        h.Init(self);

        result := lesserof(FlexPayloadLimit, len);
        inc(totalTXData, result);
        //allocate payload @pp
{$IFDEF USE_REGISTERED_MEMORY}
      //  pp := GetRegisteredMemory(result+sizeof(h));      //size of data plus header
{$ELSE}
      //  pp := GetMemory(result+sizeof(h));      //size of data plus header
{$ENDIF}
      //  movemem32(pp, @h, sizeof(h));           //move header data
      //  movemem32(@pp[sizeof(h)], p, result);   //move payload data
        if result < 0 then
          raise ECRitical.create('illegal memory size '+inttostr(result)+' FlexPlayloadLImit ='+inttostr(FlexpayloadLimit)+' len='+inttostr(len));
{$IFDEF USE_REGISTERED_MEMORY}
        pp := GetRegisteredMemory(result, self.classname+'.TReliableUDPEndpoint.SendAck(2)');      //size of data plus header
        {$ELSE}
        pp := GetMemory(result);
        {$ENDIF}
        movemem32(@pp[0], p, result);   //move payload data


        if result >= len then begin
          h.Flag_AckType := ackImmediate;
        end else begin

          h.Flag_AckType := ackImmediate;
          //h.Flag_AckType := ackOutofOrder;
        end;

        h.PrepareToSend(self);
        SendPacket(h, pp, result);

        //windows.beep(1000,30);
        SystemThreadSignal := true;
        EvalSYstemThreadSignal;//<----wake up system thread to send data
      end;
    end else
      Connected := false;
  finally
    Unlock;
  end;


end;

procedure TReliableUDPEndpoint.SendMTUTEst(sz: ni);
var
  h: TReliableUDPHeader;
  p,pp: Pbyte;
  t,cx: ni;
  pack: ni;
begin
{$IFNDEF MTU_TESTS}
  exit;
{$ENDIF}
  if not connected then
    exit;

  //fill output with random stuff (to defeat compression schemes)
{$IFDEF USE_REGISTERED_MEMORY}
  p := GetRegisteredMemory(sz, self.classname+'.TReliableUDPEndpoint.SendMTUTest');
  {$ELSE}
  p := GetMemory(sz);
  {$ENDIF}
  pp := p;
  cx := sz;
  while cx > 0 do begin
    pp^ := random(255);
    inc(pp);
    dec(cx);
  end;

//  Debug.Log('Sending MTU test sz='+inttostr(sz));

  if SmartLock then
  try
    h.Init(self);
    h.tcid := self.TOconnectionid;
    h.fcid := self.FROMConnectionId;
    h.seed := self.seed;
    h.Flag_AckType := ackNone;
    h.command := CMD_MTU_TEST;
    h.Flag_Command := true;
    h.sequencenumber := -1;//0 because there's no ack coming back
    h.expected_sequencenumber := _nextexpectedSequenceNumber;
    {$IFDEF DUMB}h.SendersBackLog := txLog.count;{lesserof(_nextsequencenumber - _unclearedsequencenumber, 255);}{$ENDIF}
    h.argument := sz;
    SendPacket(h, p,sz);

  finally
    Unlock;
//    if p <> nil then
{$IFDEF USE_REGISTERED_MEMORY}
//      FreeRegisteredMemory(p);
{$ELSE}
//      FreeMemory(p);
{$ENDIF}
  end;

end;

procedure TReliableUDPEndpoint.SendMTUTEstREsult(sz: ni);
var
  h: TReliableUDPHeader;
  t,cx: ni;
  pack: ni;
begin
{$IFNDEF MTU_TESTS}
  exit;
{$ENDIF}
  if SmartLock then
  try
    h.Init(self);
    h.tcid := self.TOconnectionid;
    h.fcid := self.FROMConnectionId;
    h.seed := self.seed;
    h.Flag_AckType := ackNone;
    h.command := CMD_MTU_SUCCESS;
    h.Flag_Command := true;
    h.sequencenumber := -1;//0 because there's no ack coming back
    h.expected_sequencenumber := _nextexpectedSequenceNumber;
    {$IFDEF DUMB}h.SendersBackLog := txLog.count;{lesserof(_nextsequencenumber - _unclearedsequencenumber, 255);}{$ENDIF}
    h.argument := sz;
    SendPacket(h, nil, 0);

  finally
    Unlock;
//    if p <> nil then
//      FreeRegisteredMemory(p);
  end;

end;

procedure TReliableUDPEndpoint.SendFutureAck(packets: TDynInt64Array);
var
  h: TReliableUDPHeader;
  p,pp: Pbyte;
  t,cx,l: ni;
  basepack,pack: ni;

begin
{$IFNDEF FUTURE_ACKS}
  exit;
{$ENDIF}
//  if GetTimeSince(LastAckTime) < 5 then exit;
  if length(packets) = 0 then
    exit;

  if DisableAcks then
    exit;

  l := length(packets);
{$IFDEF USE_REGISTERED_MEMORY}
  p := GetRegisteredMemory(l, self.classname+'.TReliableUDPEndpoint.SendFutureAck');
  {$ELSE}
  p := GetMemory(l);
  {$ENDIF}
  if SmartLock then
  try
    h.Init(self);
    h.tcid := self.TOconnectionid;
    h.fcid := self.FROMConnectionId;
    h.seed := self.seed;
    h.Flag_AckType := ackNone;
    h.command := CMD_FUTURE_ACK;
    h.Flag_Command := true;
    h.sequencenumber := -1;//0 because there's no ack coming back
    h.expected_sequencenumber := _nextexpectedSequenceNumber;
    {$IFDEF DUMB}h.SendersBackLog := txLog.count;{lesserof(_nextsequencenumber - _unclearedsequencenumber, 255);}{$ENDIF}
    basepack := packets[high(packets)];
    h.argument := basepack;
//{$IFDEF UDP_DEBUG}    UDPDebug('Sending ack up to and including #'+inttostr(h.expected_sequencenumber-1));{$ENDIF}
    //h.PrepareToSend(self);


    pp := p;
    cx := l;
    t := 0;
    while cx> 0 do begin
      pack := packets[t];
      pack := pack - basepack;
      if pack > 255 then
        pack := 0;
      if pack < 0 then
        pack := 0;
      pp^ := byte(pack);
      dec(cx);
      inc(t);
      inc(pp);
    end;
    SendPacket(h, p,l);
    net_stats_clear_rate_rx.Accumulate(1);
  finally
    Unlock;
//    if p <> nil then
//      FreeRegisteredMemory(p);
  end;
end;

function TReliableUDPEndpoint.WaitForAck(h: TReliableUDPHeader; ack_timeout: nativeint; bThrowExceptions: boolean = true): boolean;
const
  DEFAULT_ACK_TIMEOUT = 15000;
var
  tmStart: ticker;
begin
{$IFNDEF USE_FIBERS}
//  Debug.Log('Waiting for ACK '+TThread.CurrentThread.threadid.tostring);
//  Debug.Log('               --'+eet.threadid.tostring);
  if eet = nil then
    raise ECritical.create('eet is fuxed');

  if TThread.CurrentThread.threadid = eet.threadid then
    raise Exception.create('This is not designed to be called from EET.');
{$ENDIF}
  result := false;
  tmStart := GEtTicker;
  repeat
    result := not txLog.Has(h.sequencenumber);
    if not result then begin
      self.WaitForIncomingDataSignal(1000, true);
//      Debug.Log('Note that WaitForAck is currently slow because UserAPCs are not implemented to wake thread out of sleepex()');

    end;

    if GetTimeSince(tmStart) > ACK_TIMEOUT then begin
      if bThrowExceptions then
        raise EReliableUDPProtocolError.Create('packet was not acked in time')
      else
        exit(false);
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
    waitforincomingdatasignal(lesserof(10000, greaterof(1,iTimeoutMS-GetTimeSince(tmStart))), true);
//    if FAvailableData = 0 then
//      Debug.log(self,self.conniddebug+' has no data. tm='+inttostr(gettimesince(tmStart)));

    tmNow := tickCount.GetTicker;
    if closeme or (not connected) then begin
//      Debug.log(self,self.conniddebug+' waitfordata is returning false because CloseMe was set. tm='+inttostr(gettimesince(tmStart)));
      result := false;
      exit;
    end;
    if GetTimeSince(tmNow, tmSTart) > iTimeOutMS then begin
//      Debug.log(self,self.conniddebug+' waitfordata is returning false after. tm='+inttostr(gettimesince(tmStart)));
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

{$IFDEF REF_DEBUG}
function TReliableUDPPacketLogRecord._AddRef: integer;
begin

  inherited;
  Debug.log('@'+inttohex(int64(pointer(self)),8)+'++('+inttostr(_RefCount)+')');

end;

function TReliableUDPPacketLogRecord._Release: integer;
begin
  Debug.log('@'+inttohex(int64(pointer(self)),8)+'--('+inttostr(_RefCount-1)+')');
  inherited;


end;
{$ENDIF}

{ TReliableUDPDispatcher }


{ TSimpleReliableUDP }

procedure TSimpleReliableUDPClient.cli_OnDataAvailable(sender: TObject);
begin
  signal(evDAta, true);
end;

function TSimpleReliableUDPClient.DoConnect: boolean;
var
  id: int64;
begin
  id := TMultiplexedUDPClient.MasterClient.NextConnectionId;
  result := false;
  ecs(sectcli);
  try
    if assigned(cli) then begin
      result := false;
      exit;
    end;

    cli := TMultiplexedUDPClient.MasterClient.CreateEndPoint(0,id, false) as TReliableUDPClientEndpoint;
    if cli = nil then
      raise ECritical.create('CreateEndPoint returned nil.');

{$IFDEF ENDPOINT_REFERENCES}
//    cli.AddRefBy('Simple Client Connect');
{$ENDIF}
    cli.Remotehost := self.HostName;
    cli.RemotePort := strtoint(self.EndPoint);
    cli.OnDataAvailable := self.cli_OnDataAvailable;
    try
      result := cli.Connect;
    except
      result := false;
    end;
  finally
    lcs(sectcli);
  end;

end;

constructor TSimpleReliableUDPClient.Create;
begin
  ICS(sectCli);
  inherited;

end;

procedure TSimpleReliableUDPClient.DebugTagUpdated(s: string);
begin
  inherited;
  if assigned(cli) then begin
    DebugTag := s;
  end;
end;

destructor TSimpleReliableUDPClient.Destroy;
begin




  inherited;
  if cli <> nil then begin
    //cli.free;//cli is detached but not freed in detach
    cli := nil;
  end;

  DCS(sectCli);
end;

procedure TSimpleReliableUDPClient.Detach;
begin
  if detached then exit;

  if cli<>nil then begin
//    cli.Disconnect;
    cli.Detach;
  end;

  inherited;




end;

procedure TSimpleReliableUDPClient.DoDisconnect;
begin
  inherited;
  ECS(sectCli);
  try
    if assigned(cli) then begin
      cli.Disconnect();
      //cli.free;//ok
      //cli._Release;
      cli.Detach;//AndFree;
      cli := nil;
    end;
  finally
    LCS(sectCLI);
  end;
end;

function TSimpleReliableUDPClient.DoCheckForData: Boolean;
begin
  raise ECritical.create(self.ClassName+' is not intended for polled operation.');
end;

function TSimpleReliableUDPClient.DoReadData(buffer: pbyte; length: integer): integer;
begin
  result := 0;
  ECS(sectCli);
  try
    if cli = nil then
      raise ETransportError.create('must be connected to to data');
    result := cli.ReadDAta(buffer, length);
  finally
    LCS(sectCli);
  end;

end;

function TSimpleReliableUDPClient.DoSendData(buffer: pbyte; length: integer): integer;
begin
  result := 0;
  ECS(sectCli);
  try
    if cli = nil then
      raise Exception.create('must be connected to send data');
    result := cli.SendDAta(buffer, length);
  finally
    LCS(sectCli);
  end;
end;

function TSimpleReliableUDPClient.GetConnected: boolean;
begin
  ecs(sectCli);
  try
    result := cli <> nil;
    if result then
      result := cli.connected;
  finally
    lcs(sectcli);
  end;

end;


procedure TSimpleReliableUDPClient.OnDataAvailable(const sender: TObject);
begin
  Signal(evData, true);
end;

procedure TSimpleReliableUDPClient.SetCli(
  const Value: TReliableUDPClientEndpoint);
begin
  if assigned(Fcli) then begin
//    Debug.Log('Setting CLI to nil');
    FCli._Release;
    FCli := nil;
  end;



  FCli := Value;

  if assigned(Fcli) then begin
//    Debug.Log('Setting CLI to ASSIGNED');
    FCli._Addref;
  end;


end;

function TSimpleReliableUDPClient.DoWaitForData(timeout: cardinal): boolean;
begin
  ECS(sectCli);
  try
    result := HAsLeftOvers or (assigned(cli) and cli.WaitForData(timeout));
  finally
    LCS(sectCli);
  end;

end;

{ TReliableUDPServer }

procedure TMultiplexedUDPEndpoint.BindToAnyPort;
begin
  self.DefaultPort := 0;
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
  log: TReliableUDPPacketLog; iOldID, iNewID: int64; from: boolean);
var
  t: integer;
  r: TReliableUDPPacketLogRecord;
begin
  if SmartLock then
  try
    log.Iterate(
      procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
      begin
        r := TtreeItem_ReliableUDPPacketLogRecord(ABTreeItem).packet;
        if from then begin
          if r.h.fcid = iOLDID then
            r.h.fcid := iNewID;
        end else begin
//          if r.h.tcid = iOLDID then  {!there can only be one tcid in the log, so why not just change all of them?}
            r.h.tcid := iNewID;
        end;

      end
    );

  finally
    Unlock;
  end;
end;


procedure TReliableUDPEndpoint.ChangeConnectionIDInLogs(iOldID, iNewID: int64; from: boolean);
begin
  if SmartLock then
  try
    ChangeConnectionIDInLog(txLog, iOldID, iNewID, from);
    ChangeConnectionIDInLog(rxLog, iOldID, iNewID, from);
  finally
    Unlock;
  end;
end;

constructor TMultiplexedUDPEndpoint.CReate(AOwner: TComponent);
begin
  inherited CReate(AOwner);
  InitializeCriticalSection(sect);
  ICS(sect_send);
  ICS(sectUDPsend);
  ThreadedEvent := true;
  FEndpoints := TReliableUDPEndpointList.Create;
  FEndpoints.OwnsObjects := false;
  Init;

{$IFDEF QUEUE_INCOMING_UDP}
{$IFDEF MULTI_QUEUE_IN}
  queue_in := TMultiQueue.create;
{$ELSE}
  queue_in := TPM.Needthread<TSimpleQueue>(nil);
  queue_in.name := 'TSimpleQueue for incoming UDP';
  {$IFDEF MSWINDOWS}
  {$IFDEF HIGH_PRIORITY_QUEUES}
  queue_in.BetterPriority := bpHigher;
  {$ENDIF}
  {$ENDIF}
  queue_in.start;
  {$ENDIF}
{$ENDIF}
{$IFNDEF MULTI_QUEUE_IN}
  queue_in.maxItemsInQueue := 1024;
{$ENDIF}

  TWorthless(self.FListenerThreads).GetList;//.priority := tpHighest;
//  TIdUDPServer(self).FList[0].Priority := tpHighest;

end;

function TMultiplexedUDPEndpoint.CreateEndPoint(tcid, fcid: cardinal; bServer: boolean): TReliableUDPEndpoint;
begin
  //LOCK if you add any more shit to this

  if not bServer then begin
    UDPDebug('Creating Endpoint for client tcid:'+tcid.tostring+' fcid:'+fcid.tostring);
    result := TReliableUDPClientEndpoint.Create(ctClient, self);
  end else begin
    UDPDebug('Creating Endpoint for server tcid:'+tcid.tostring+' fcid:'+fcid.tostring);
    result := TReliableUDPEndpoint.Create(ctServer, self);
  end;
  result.toConnectionId := tcid;
  result.fromconnectionid := fcid;


{$IFDEF ENDPOINT_REFERENCES}
{$IFDEF VERBOSE_REFERENCES}result.AddRefBy('Create Endpoint');{$ELSE}result._AddRef;{$ENDIF}
{$ENDIF}
  Fendpoints.lock;
  try
    self.FEndpoints.Add(result);
    estimated_endpoint_count := FEndpoints.count;
  finally
    Fendpoints.unlock;
  end;


{$IFDEF ENDPOINT_REFERENCES}
  result.ReleaseInternalRef;
{$ENDIF}
end;

procedure TMultiplexedUDPEndpoint.DispatchPacketToEndpoint(ifo: TReliableUDPPacketLogRecord; bDisableAcks: boolean);
var
  ep: TReliableUDPEndpoint;
  h: TReliableUDPHeader;
  itm: TEndPOintInputQueueItem;
begin
  hint_stuff_to_do := true;
  Lock;
  try
    ifo._AddRef;    //<<---local hold
    try
      FEndpoints.Lock;
      try
      ep := FEndpoints.Find(ifo.h.tcid);
      if ep=nil then
        exit;
      if ep.noMorePacketsAllowed then
        exit;

      if assigned(ep) then begin
        itm := TEndPOintInputQueueItem.Create;
        itm.ep := ep;
        itm.ifo := ifo;
        itm.AutoDestroy := true;
        itm.bDisableAcks := bDisableAcks;
        if ep.queue_pipe_in = nil then
          raise Ecritical.create('found an endpoint but the queue was nil');
        ep.queue_pipe_in.addItem(itm);
      end else begin
  {$IFDEF SEND_RESETS}
        if (not h.Flag_System) then begin
          Debug.Log(self,'Could not find endpoint with fcid '+inttostr(ifo.h.tcid));
          h.Init(nil);
          h.tcid := ifo.h.fcid;
          h.fcid := ifo.h.tcid;
          h.command := CMD_RESET;
          h.Flag_Command := true;
          h.Flag_System := true;
          h.sequencenumber := 0;
          SendBackToSender(ifo, h, nil,0);
        end;
  {$ENDIF}

      end;
    finally
      ifo._Release;//<<---local hold
      ifo := nil;
    end;
    finally
      FEndpoints.unlock;
    end;

  finally
    Unlock;
  end;
end;

procedure TReliableUDPEndpoint.DispatchInput_Sync(itm: TEndpointInputQueueItem);
var
  h: TReliableUDPHeader;
  ifo:  TReliableUDPPAcketLogRecord;
  bDisableAcks: boolean;
  ep: TReliableUDPEndpoint;
begin
  //Lock;
  try
    ifo := itm.ifo;
    totalRX := totalRX + ifo.payload_length+sizeof(ifo.h);
    bDisableAcks := itm.bDisableAcks;
    ep := itm.ep;
    begin

      ep.DisableAcks := bDisableAcks;
      if ep.SmartLock then
      try
//{$IFDEF ACK_WITHOUT_ACK}
        if ifo.h.command <> CMD_ACK then begin  //allow the handling of CMD_ACK to handle health stuff
          ep.txAckLevel := ifo.h.expected_sequencenumber;
          ep.ClearAckedTX;
        end;
//{$ENDIF}
        if not ep.CloseMe then begin//<<--once we hit the final stages of connection closure... ALL packets are banned.
          if ifo.h.Flag_AckType = ackNone then begin
            ep.HandleIncomingPacket(ifo);
            ep.RequestRetransmissionOfFutureGaps
          end else begin
            ep.QueueIncomingPacket(ifo);
          end;
        end;
        IF ASSIGNED(EP) THEN
          ep.DisableAcks := false;
      finally
        ep.Unlock;
      end;
    end;
  finally
  //  Unlock;
  end;
  ep.Multiplexer.hint_stuff_to_do := false;

end;

function TMultiplexedUDPEndpoint.DebugConnections: string;
var
  t: integer;
  sl: TStringlist;
  ep: TReliableUDPEndpoint;
begin
  result := '';
//  Lock;
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
//    Unlock;
  end;
end;

procedure TMultiplexedUDPEndpoint.DeregisterEndPOint(ep: TReliableUDPEndpoint);
var
  bHad: boolean;
begin
  bHad := false;
  FEndpoints.lock;
  try
    ep.Lock;
    try
      FEndpoints.Lock;
      try
        bHad := FEndpoints.Has(ep);
        FEndpoints.Remove(ep);
        estimated_endpoint_count := FEndpoints.count;
      finally
        Fendpoints.unlock;
      end;
    finally
      ep.Unlock;
    end;
  finally
    FEndPoints.unlock;
  end;

  try
{$IFDEF ENDPOINT_REFERENCES}
    if bHad then
{$IFDEF VERBOSE_REFERENCES}ep.ReleaseBy('Deregister Endpoint (had)');//<---careful not to do this under lock
{$ELSE}ep._Release;{$ENDIF}
{$ENDIF}
  except
    on e:exception do begin
      e.message := 'Exception during ep._release : '+e.message;
      raise;
    end;
  end;

end;

destructor TMultiplexedUDPEndpoint.Destroy;
begin
  Detach;
  systemx.DeleteCriticalSection(sect);
  DCS(sect_send);
  DCS(sectUDPsend);
  inherited;
end;

procedure TMultiplexedUDPEndpoint.Detach;
begin
  if detached then
    exit;

  try
    self.Active := false;
  except
  end;
  shuttingdown := true;

  try
    disconnectall;
  except
  end;

  while FEndpoints.Count > 0 do begin
    sleep(1000);//endpoints should clear themselves and detach properly when they see the shutting down flag
    Debug.Log(self,'There are still '+inttostr(FEndpoints.count)+' endpoints waiting to be destroyed.');
  end;


{$IFDEF QUEUE_INCOMING_UDP}
{$IFDEF MULTI_QUEUE_IN}
  queue_in.Free;
  queue_in := nil;
{$ELSE}
  if queue_in <> nil then begin
    queue_in.waitforall;
    queue_in.stop;
    queue_in.waitforfinish;
    TPM.NoNeedthread(queue_in);
    queue_in := nil;
  end;
{$ENDIF}
{$ENDIF}

  if assigned(FEndpoints) then begin
    FEndpoints.SafeFree;//ok
    FEndpoints := nil;
  end;

  inherited;
  detached := true;

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

procedure TMultiplexedUDPEndpoint.DoOnUDPException(
  AThread: TIdUDPListenerThread; ABinding: TIdSocketHandle;
  const AMessage: String; const AExceptionClass: TClass);
begin
  inherited;
//  debug.log(self,aMEssage);
//  Error := true;


end;

{$IFDEF STRUCTURED_INCOMING_UDP}
procedure TMultiplexedUDPEndpoint.DoUDPRead(AThread: TIdUDPListenerThread;
  const XData: TIdBytes; ABinding: TIdSocketHandle);
var
  itm: TPacketQueueItem;
  sz: ni;
begin
  itm := TPacketQueueItem.Create;
  itm.ep := self;
  sz := length(XData);
  itm.datasize := sz;
{$IFDEF USE_REGISTERED_MEMORY}
  itm.data := GetRegisteredMemory(sz, self.classname+'.DoUDPRead');
  {$ELSE}
  itm.data := GetMemory(sz);
  {$ENDIF}
  movemem32(itm.data, @XData[0], sz);
{$IFDEF UDP_DEBUG}    Debug.Log(self,MemoryToHex(@XData[0], sz));{$ENDIF}
  itm.PeerIP := ABinding.PeerIP;
  itm.PeerPort := ABinding.PeerPort;
  {$IFDEF QUEUE_INCOMING_UDP}
  queue_in.AddItem(itm);
  {$ELSE}
  itm.AutoDestroy := false;
  ProcessUDPRead(itm, false);
  itm.DetachAndFree;
  itm := nil;

  //itm.free;
  {$ENDIF}
end;
{$IFDEF USE_FEC}
procedure TMultiplexedUDPEndpoint.DoUDPRead_BETTER(const Health: TFECREport; const XData: TIdBytes;
  PeerIP: string; PEerPort: ni);
var
  itm: TPacketQueueItem;
  sz: ni;
begin
  itm := TPacketQueueItem.Create;
  itm.health := health;
  itm.ep := self;
  sz := length(XData);
  itm.datasize := sz;
{$IFDEF USE_REGISTERED_MEMORY}
  itm.data := GetRegisteredMemory(sz){};
  {$ELSE}
  itm.data := GetMemory(sz){};
  {$ENDIF}
  movemem32(itm.data, @XData[0], sz);
  itm.PeerIP := PeerIP;
  itm.PeerPort := PeerPort;
  {$IFDEF QUEUE_INCOMING_UDP}
  queue_in.AddItem(itm);
  {$ELSE}
  itm.AutoDestroy := false;
  ProcessUDPRead(itm, false);
  itm.DetachFree;
  itm := nil;

  //itm.free;
  {$ENDIF}
end;
{$ENDIF}


{$ELSE}
procedure TMultiplexedUDPEndpoint.DoUDPRead(AThread: TIdUDPListenerThread;
  const XData: TIdBytes; ABinding: TIdSocketHandle);
var
  Adata: PByte;
  ADataSize: ni;
  ifo: TReliableUDPPacketLogRecord;
begin
  ADAta :=@Xdata[0];
  ADataSize := length(Xdata);
  ifo := TReliableUDPPacketLogRecord.Create;
{$IFDEF MORE_REFS}
  {$IFDEF REF_DEBUG}Debug.log('AddRef from DoUDPRead');{$ENDIF}
  ifo._AddRef;//<<---Local hold
{$ENDIF}
  try
    ifo.RemoteOrigin := true;
    ifo.OriginatingHost := abinding.PeerIP;
    ifo.OriginatingPort := abinding.PeerPort;

    if assigned(ONUDPREad) then
      raise Exception.create('You probably don''t want to have OnUDPRead assigned in a '+self.ClassName);
    //get the header
    systemx.MoveMem32(@ifo.h, @Adata[0], sizeof(ifo.h));
    ifo.payload_length := adatasize-sizeof(ifo.h);
    if ifo.payload_length > 0 then begin
{$IFDEF USE_REGISTERED_MEMORY}
      ifo.payload := GetRegisteredMemory(ifo.payload_length){};
{$ELSE}
      ifo.payload := GetMemory(ifo.payload_length){};
{$ENDIF}
//      MemoryDebug('assigning memory @'+inttohex(ni(ifo.payload),sizeof(ni)*2)+' associated with object @'+inttohex(ni(pointer(ifo)),sizeof(ni)*2)+' packet '+ifo.debugstring);
      movemem32(ifo.payload, adata+sizeof(ifo.h), ifo.payload_length);
    end else
      ifo.payload := nil;

{$IFDEF UDP_DEBUG}    Debug.Log(self,'Read:'+ifo.debugstring);{$ENDIF}
{$IFDEF SEND_RECEIVE_DEBUG}
    Debug.Log(self,'[Receive] '+ifo.debugstring);
{$ENDIF}
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
      //FreeRegisteredMemory(mem);
      //DO NOT FREE MEMORY    //DO NOT FREE MEMORY    //DO NOT FREE MEMORY    //DO NOT FREE MEMORY
    end;
  finally
    ifo.FreeWithReferences := true;
    ifo.DeadCheck;
    {$IFDEF REF_DEBUG}Debug.log('RElease from DoUDPRead');{$ENDIF}
    ifo._Release;//<<--Local hold
    ifo := nil;
  end;
end;
{$ENDIF}

{$IFDEF STRUCTURED_INCOMING_UDP}
procedure TMultiplexedUDPEndpoint.ProcessUDPRead(itm: TPacketQueueItem; bDisableAcks: boolean);
var
  dat: Pbyte;
begin
  dat := @itm.data[0];
  if dat^ = 65 {A} then begin
    inc(dat);
    case dat^ of
      68://D
      begin
//        UDPDebug('Item:'+ni(pointer(itm)).tohexstring);
        ProcessUDPReadAD(itm, bDisableAcks);
      end;
      76://L
      begin
        ProcessUDPReadAL(itm);
      end;
    end;
  end;
end;

procedure TMultiplexedUDPEndpoint.ProcessUDPReadAL(itm: TPacketQueueItem);
var
  itm2: TPacketQUeueItem;
  p: Pbyte;
  al: TALHEader;
  cnt: ni;
  d: PByte;
begin

  //Process Multiple Payloads combined into a single MTU
  cnt := itm.DataSize-2;
  p := itm.Data;
  inc(p,2); //skip over AL
  while cnt > 0 do begin
    movemem32(@al, p, sizeof(al)); //get chunk size
{$IFDEF USE_REGISTERED_MEMORY}
    d := GetRegisteredMemory(al.sz, self.classname+'.ProcessUDPReadAL');//get memory of size
{$ELSE}
    d := GetMemory(al.sz);
{$ENDIF}
    inc(p, sizeof(al));//skip over size header
    movemem32(d,p, al.sz);//move sz bytes do new data destination

    itm2 := TPacketQUeueItem.create;//create new packet queueitem
    itm2.Data := d;//use data
    itm2.DataSize := al.sz;
    itm2.ep := itm.ep;
    itm2.PeerIP := itm.PeerIP;
    itm2.PeerPort := itm.PeerPort;
    inc(p, al.sz);
    dec(cnt, al.sz+sizeof(al));
//    DisableAcks := cnt>0;//Disable acks for all but last packet in package
//                          TODO 1: This may cause a performance hitch if the last packet doesn't use ACKS in the package
    ProcessUDPRead(itm2, cnt>0);//process the queueitem immediately
    itm2.DetachAndFree;
    itm2 := nil;


  end;

end;
function TMultiplexedUDPEndpoint.ReSendEx(const basetm, interval: ticker; const AHost: string;
  const APort: TIdPort; const ABuffer: TIdBytes; fecpid: cardinal; const FECSize: cardinal): cardinal;
begin
{$IFDEF USE_FEC}
  ReSendBufferWithFec(basetm, interval, Ahost, Aport, Abuffer, fecpid, fecsize);
{$ELSE}
  SendBuffer(Ahost, Aport, Abuffer);
{$ENDIF}
  exit(0);
end;

procedure TMultiplexedUDPEndpoint.ResetConnection(tcid: int64; fcid: int64);
var
  t: ni;
begin
{$IFDEF RESPOND_TO_RESETS}
  lock;
  try
    UDPDebug('local connnection '+tcid.tohexstring +' is being reset because '+fcid.tohexstring+' was not found on the other end, so we''re resetting it');
{    for t:= 0 to FEndpoints.count-1 do begin
      if FEndpoints.Items[t].FROMConnectionId = tcid then begin
        FEndpoints.Items[t].CloseState := csReset;
        FEndpoints.Items[t].CloseMe := true;
      end;
    end;}
  finally
    unlock;
  end;
{$ENDIF}
end;

procedure TMultiplexedUDPEndpoint.ProcessUDPReadAD(itm: TPacketQueueItem; bDisableAcks: boolean);
var
  Adata: PByte;
  ADataSize: ni;
  ABinding: TIdSocketHandle;
  ifo: TReliableUDPPacketLogRecord;
begin
  ifo := nil;
//  Lock;
  try
  ADAta :=itm.data;
  ifo := TReliableUDPPacketLogRecord.Create;
  ifo._AddRef;//<<--Local hold
{$IFDEF MORE_REFS}
  {$IFDEF REF_DEBUG}Debug.log('AddRef from ProcessUDPReadAD');{$ENDIF}
  ifo._AddRef;
{$ENDIF}

{$IFDEF USE_FEC}
  ifo.health := itm.health;
{$ENDIF}

  ifo.FreeWithReferences := true;
  try
    ifo.RemoteOrigin := true;
    ifo.OriginatingHost := itm.PeerIP;
    ifo.OriginatingPort := itm.PeerPort;

    if assigned(ONUDPREad) then
      raise Exception.create('You probably don''t want to have OnUDPRead assigned in a '+self.ClassName);
    //get the header
    systemx.MoveMem32(@ifo.h, @Adata[0], sizeof(ifo.h));
    ifo.payload_length := itm.datasize-sizeof(ifo.h);
    if ifo.payload_length > 0 then begin
{$IFDEF USE_REGISTERED_MEMORY}
      ifo.payload := GetRegisteredMemory(ifo.payload_length, self.classname+'.ProcessUDPReadAD'){};
{$ELSE}
      ifo.payload := GetMemory(ifo.payload_length);
{$ENDIF}
//      MemoryDebug('assigning memory @'+inttohex(ni(ifo.payload),sizeof(ni)*2)+' associated with object @'+inttohex(ni(pointer(ifo)),sizeof(ni)*2)+' packet '+ifo.debugstring);
      movemem32(ifo.payload, itm.data+sizeof(ifo.h), ifo.payload_length);
    end else
      ifo.payload := nil;

{$IFDEF UDP_DEBUG}    Debug.Log(self,'Read:'+ifo.debugstring);{$ENDIF}
{$IFDEF SEND_RECEIVE_DEBUG}
    Debug.Log(self,'[Receive] '+ifo.debugstring);
{$ENDIF}
    try
      if ifo.h.Flag_Create and (not ifo.h.Flag_System) then
        raise EReliableUDPProtocolError.create('Cannot use the Create flag unless System flag is also set');

      //if it doesn't belong to an endpoint, service it here
      if ifo.h.Flag_System then begin
        HandleSystemPacket(ifo);
      end else begin
       //else pass the event to an endpoint
        DispatchPacketToEndpoint(ifo, bDisableAcks);
      end;

    finally
      //DO NOT FREE MEMORY    //DO NOT FREE MEMORY    //DO NOT FREE MEMORY    //DO NOT FREE MEMORY
      //FreeRegisteredMemory(mem);
      //DO NOT FREE MEMORY    //DO NOT FREE MEMORY    //DO NOT FREE MEMORY    //DO NOT FREE MEMORY
    end;
  finally
    ifo.FreeWithReferences := true;
    ifo.DeadCheck;
    {$IFDEF REF_DEBUG}Debug.log('RElease from ProcessUDPReadAD');{$ENDIF}
    ifo._Release;//<<--- not sure why, but we release here ( I think to drop ref count of 1 that is implicit on creation)
  end;
  finally
    if ifo <> nil then
      ifo._Release;//<--Local hold
//    Unlock;
  end;
end;
{$ENDIF}

function TMultiplexedUDPEndpoint.FindAvailableConnectionID: int64;
begin
  repeat
    result := random($7FFFFFFF);
  until not FEndpoints.Has(result);

end;


function TMultiplexedUDPEndpoint.GetNextFecID: cardinal;
begin
  result := nextfecid;
  inc(nextfecid);
end;

function TReliableUDPEndpoint.GetConnidDebug: string;
begin
  result := inttohex(pint64(@toConnectionID)^, 16);
end;

function TReliableUDPEndpoint.GEtDebugTag: string;
begin
  Lock;
  try
    result := FDebugTag;
  finally
    Unlock;
  end;
end;

function TReliableUDPEndpoint.GEtFecSize: ni;
begin
  result := 400;
end;

function TReliableUDPEndpoint.GEtFlexMTU: ni;
var
  t: ni;
  p:  PMTUTestResult;
begin
{$IFDEF FORCE_SMALL_MTU}
  exit(mtu_test_results[0].sz);
{$ENDIF}

{$IFDEF MTU_TESTS}
  result := mtu_test_results[0].sz;
  for t:= high(mtu_test_results) downto 0 do begin
    if MTU_TEST_PING_REQ[t] > Self.bestAckTime then begin
      p := @mtu_test_results[t];
      if p.CanUse then
        exit(p.sz);
    end;
  end;
{$ELSE}
  exit(WAlkToMTU(FWalkingMTU));
{$ENDIF}



end;

procedure TReliableUDPEndpoint.WalkFlexMTU(bPacketLoss: boolean);
var
  at: ni;
begin
{$IFNDEF USE_FEC}
  ControlInterval(bPacketLoss);
  if bPacketLoss then begin
    WalkingAverageFine := WAlkingAverageFine -1;
    if WAlkingAverageFine <= 0 then
      WAlkingAverageFine := 1;

  end else begin
    WalkingAverageFine := WalkingAverageFine +4;
  end;


{$ELSE}
  exit;
{$ENDIF}
  if bPacketLoss then begin
//    queue_out.SlowDown;
{$IFNDEF NEVER_WALK_DOWN_MTU}
    if FWalkingMTU > 40 then begin
        FWalkingMTU := FWalkingMTU - 1;


//      Debug.Consolelog('--mtu '+inttostr(FWalkingMTU));

    end;
{$ENDIF}


  end
  else begin
//    queue_out.SpeedUp;
//    inc(FWalkingMTU,1);
    if FWalkingMTU = 0 then
      FWalkingMTU := 1;
    if FWAlkingMTU < MAX_PARTS-8 then
      FWalkingMTU := FWalkingMTU + 1;

//      Debug.Consolelog('++mtu '+inttostr(FWalkingMTU));
//      if WAlkingAverageFine > (1 shl WA_SHIFT) then
//        dec(WalkingAverageFine, 1 shl WA_SHIFT);

  end;

//  WAlkingAverageFine := 4 shl WA_SHIFT;
  WalkingAverageFine := lesserof(MAX_PACKET_WRITE_AHEAD div BigPacketDivisor,((bestAckTime * WRITE_AHEAD_MULTIPLIER) div GREATEROF(1,(FlexMTU div mtu_test_sizes[0]))) shl WA_SHIFT);
//  WAlkingAverageFine := WalkingAverageFine div (((round(rsAckTime.PeriodicAverage)+1) div (bestAckTime+1))+1);


end;


function TMultiplexedUDPEndpoint.HandleSystemCommandPacket(ifo: TReliableUDPPacketLogRecord): TReliableUDPEndpoint;
begin
  result := nil;
  case ifo.h.command of
    RUDP_SYS_CMD_CREATE: begin
      result := HandleSYstemCReatePacket(ifo);
    end;
    CMD_RESET: begin
      ResetConnection(ifo.h.tcid, ifo.h.fcid);
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
//  lock;
  try
    i1 := 0;
    i2 := 0;
    movemem32(@i1, @ifo.payload[0], sizeof(int64));
    movemem32(@i2, @ifo.payload[8], sizeof(int64));

    FEndpoints.lock;
    try
{$IFDEF UDP_DEBUG}
      Debug.Log(self,'Changing connection seed#'+inttohex(i1,0)+' to '+inttohex(i2,0)+' from serverid '+ifo.h.tcid.tostring);
      Debug.Log(self,DebugConnections);
{$ENDIF}
      ep := FEndpoints.FindBySeed(ifo.h.seed);//we must find the private endpoint by its server-id
      if assigned(ep) then begin
//        if not FEndpoints.has(i2) then begin
          ep.Lock;
          try
            ep.secret := i1;
            ep.Connected := true;
            ep.Connecting := false;
            ep.toConnectionID := i2;
            ep.ChangeConnectionIDInLogs(i1, i2, false);
            ep.AckIncoming(true);
            ep.ClearTxLogsUpTo(ep._nextexpectedSequenceNumber);


            //ep._nextexpectedSequenceNumber := ifo.h.expected_sequencenumber;
            //ep.txLog.FreeAndClear;
            result := ep;
          finally
            ep.Unlock;
          end;
//        end;
      end;

    finally
      FEndpoints.Unlock;
    end;
  finally
//    unlock;
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
    {$IFDEF REF_DEBUG}Debug.log('RElease from HandleSystemCreatePacket');{$ENDIF}
    ifo._release;
    exit;
  end;

//  lock;  <<-touches state of endpoint list only, which has its own locks
  try
    ep := FEndpoints.FindByClientSeed(ifo.OriginatingHost, ifo.OriginatingPort, ifo.h.seed);
    //if we have the connection, then this is a duplicate, forget it
    //todo 1: possibly force retransmission of pending TX packets
    if ep <> nil then begin
      Debug.log(self,'Duplicate Client connection, throwing out');
      if not ep.Connect_acked then begin
        iold := ifo.h.seed;
        inew := ep.fromconnectionid;
        UDPDebug('Sending request to change connectionID from:'+inttohex(iold,0)+' to '+inttohex(inew,0));
        ep.connect_acked := true;
        SendError(ep, true, ERR_CHANGE_CONNID, iold, inew, 'Connection id invalid, change please.',true, true);
      end;
//      ep.connectionid := iNew;
//      ep.ChangeConnectionIDInLogs(iOld, iNew);
//      ep.Connected := true;
//      ep.Connecting := false;
//      result := ep;
//      ep.SayHello;
    end else
    //ELSE we will create a new connection, change the connection id and then send the new connectionid
    begin
      Debug.log(self,'New connection!');
      iold := ifo.h.seed;
      inew := FindAvailableConnectionid;
      ep := self.CreateEndPoint(ifo.h.fcid,iNew, true);
      ep.DestroyOnClose := true;
      ep.Remotehost := ifo.OriginatingHost;
      ep.RemotePort := ifo.Originatingport;
      ep.seed := ifo.h.seed;//save the seed to help with discarding duplicates
      ep.secret := ifo.h.fcid;//the oritinal connectionid is the "secret" that can be used for multi-home switching later on.

      Debug.Log(self,'Sending request to change connectionID from:'+inttostr(iold)+' to '+inttostr(inew));
      ep._nextexpectedSequenceNumber := ifo.h.sequencenumber+1;
      ep._nextQueuedSequenceNumber := ep._nextexpectedSequenceNumber;
      SendError(ep, true, ERR_CHANGE_CONNID, iold, inew, 'Connection id invalid, change please.',true, true);
//      ep.ChangeConnectionIDInLogs(iOld, iNew, true);  //<--- dont do this or retransmissions of this will try to change connid again
      ep.Connected := true;
      ep.Connecting := false;
      result := ep;
      ep.SayHello;


//      FEndpoints.Add(ep);
    end;
  finally
//    unlock;
  end;

end;

function TMultiplexedUDPEndpoint.HandleSystemPacket(ifo: TReliableUDPPacketLogRecord): TReliableUDPEndpoint;
var
  bHandled: boolean;
begin
  result := nil;
  if ifo.h.Flag_Command then begin
    result := HandleSYstemCommandPacket(ifo);

  end else
  if ifo.h.Flag_CommandResponse then begin
    result := HandleSYstemCommandResponsePacket(ifo);
  end;

  bHandled := result <> nil;



  //even though the "system" level handled it,
  //we still pass it to the endpoint so that it can be put through the
  //standard ACK process
  Lock;
  try
    if ifo.h.Flag_AckType <> ackNone then begin
      //if the earlier dispatchers didn't return an endpoint that handled the packet
      if result = nil then
        //search for one the traditional way (not typical in this instance)
        result := FEndpoints.Find(ifo.h.tcid);

      if assigned(result) then begin
        result.QueueIncomingPacket(ifo);
      end else begin
//        ifo._Release;   //reference is held in outer function
        ifo := nil;
      end;
    end else begin
//      ifo._Release;     //reference is held in outer function
      ifo := nil;
    end;
  finally
    Unlock;
  end;
end;

procedure TMultiplexedUDPEndpoint.Init;
begin
  //
end;

procedure TMultiplexedUDPEndpoint.Lock;
begin
  systemx.EnterCriticalSection(sect);
end;

function TMultiplexedUDPEndpoint.NextConnectionId: cardinal;
begin
  FendPoints.lock;
  try
    //keep looping until we find a connectionid we don't have
    repeat
      result := random($7fFF0000 or (GetTicker() and $ffff));
    until not FEndpoints.Has(result);
  finally
    Fendpoints.Unlock;
  end;
end;


function TMultiplexedUDPEndpoint.NextSeed: int64;
begin
//  Lock;  <<--findbyseed is threadsafe
  try
    //keep looping until we find a connectionid we don't have
    repeat
      result := random($7fFF0000 or (GetTicker() and $ffff));
    until FEndpoints.FindBySeed(result) = nil;
  finally
//    Unlock;
  end;
end;

procedure TReliableUDPEndpoint.ControlInterval(bPacketLoss: boolean);
begin
  //if losing too many parity packets, then slow down
  if bPacketLoss then begin
    WAlkingAverageFine := WalkingAverageFine shr 1;
    if WalkingAverageFine < 1 then WAlkingAverageFine := 1;

    queue_out.ticks_per_byte := queue_out.ticks_per_byte * (1+(intervalStabilizer/32));
    if queue_out.ticks_per_byte > 32 then
      queue_out.ticks_per_byte := 32;
  end else
  begin
    WalkingAverageFine := walkingAverageFine + (1 shl WA_SHIFT);
    if WAlkingAverageFine > (MAX_PACKET_WRITE_AHEAD shl WA_SHIFT) then
      WalkingAverageFine := (MAX_PACKET_WRITE_AHEAD shl WA_SHIFT);
    queue_out.ticks_per_byte := queue_out.ticks_per_byte * (1-(intervalStabilizer/32)){should be <1};
    if queue_out.ticks_per_byte < 0.0001 then
      queue_out.ticks_per_byte := 0.0001;
  end;

  rsIntervalStability.AddStat(queue_out.ticks_per_byte);
  EvalIntervalStability;
end;
procedure TReliableUDPEndpoint.ReactToHealthReport(seq: int64; health: TFECREport);
{$IFNDEF USE_FEC}
begin
  //no implementation required unless using FEC
end;
{$ELSE}
var
  q: single;
  bar: single;
begin
  //it is desireable for us to lose maybe 50% of parity parts
  //50% is a good number because it puts us in the center of the
  //lane, reducing the chance of requiring a retry
  //and reducing the chance that things will be slowed to the point where there
  //are performance gaps
  if health.parts <=8 then
    exit;


  q := health.Quality;

  bar := 1-((1/health.par_freq)*1.5);
//  Debug.consolelog('Seq:'+inttostr(seq)+' *Quality='+floatprecision(q,2)+' bar='+floatprecision(bar,4)+' Interval='+floatprecision(queue_out.ticks_per_byte, 4)+' Stability='+floatprecision(intervalStabilizer,4));



  //if losing too many parity packets, then slow down
  if q < bar then begin
    WAlkingAverageFine := WalkingAverageFine shr 1;
    if WalkingAverageFine < 1 then WAlkingAverageFine := 1;

    queue_out.ticks_per_byte := queue_out.ticks_per_byte * (1+(intervalStabilizer/32));
    if queue_out.ticks_per_byte > 32 then
      queue_out.ticks_per_byte := 32;
  end else
  if q > bar then begin
    WalkingAverageFine := walkingAverageFine + (1 shl WA_SHIFT);
    if WAlkingAverageFine > (MAX_PACKET_WRITE_AHEAD shl WA_SHIFT) then
      WalkingAverageFine := (MAX_PACKET_WRITE_AHEAD shl WA_SHIFT);
    queue_out.ticks_per_byte := queue_out.ticks_per_byte * (1-(intervalStabilizer/32)){should be <1};
    if queue_out.ticks_per_byte < 0.0001 then
      queue_out.ticks_per_byte := 0.0001;
  end;

  rsIntervalStability.AddStat(queue_out.ticks_per_byte);
  EvalIntervalStability;

  Debug.consolelog('Seq:'+inttostr(seq)+'  Quality='+floatprecision(q,2)+' bar='+floatprecision(bar,4)+' Interval='+floatprecision(queue_out.ticks_per_byte, 4)+' Stability='+floatprecision(intervalStabilizer,4));



end;
{$ENDIF}

function TReliableUDPEndpoint.ReadData(p: pbyte; len: nativeint): nativeint;
//CONTEXT: USER THREAD - ALWAYS
var
  ti: Ttreeitem_ReliableUDPPacketLogRecord;
  ifo: TReliableUDPPacketLogRecord;
  iCanRead: nativeint;
begin
  result := 0;

  if SmartLock then
  try

    if rxDAtaLog.Count = 0 then exit;//check again under lock
    ti := TtreeItem_ReliableUDPPacketLogRecord(rxDataLog.FirstItem);
    if ti = nil then
      raise ECritical.create('rxDataLog.count > 0 but rxDataLog.FirstItem is nil!');
    ifo := ti.packet;
    if ifo = nil then
      raise ECritical.create('ti.packet is nil8!');
    iCanRead := ifo.payload_length - ifo.payload_read_index;
    iCanRead := lesserof(iCanRead, len);
    result := iCanRead;
    inc(totalRXData, result);

    movemem32(p, @ifo.payload[ifo.payload_read_index], iCanRead);
    result := iCanRead;
    FAvailableData := FAvailableData - iCanRead;
    ifo.payload_read_index := ifo.payload_read_index + iCanRead;


    if (ifo.payload_read_index=ifo.payload_length) then begin
      rxDataLog.Remove(ifo);
      if (ifo.needsack and ifo.acked) then begin
//        rxLog.Remove(ifo);
//        {$IFDEF REF_DEBUG}Debug.log('RElease from ReadData');{$ENDIF}
//        ifo._Release;
//        ifo.Free;
//        ifo := nil;
      end;
      {$IFDEF REF_DEBUG}Debug.log('RElease from ReadData2');{$ENDIF}
      ifo._Release;
      ifo := nil;

    end;


    EvalUserThreadSignal;

  finally
    Unlock;
  end;

  if result = 0 then begin
    raise EReliableUDPProtocolError.Create('Connection dropped (probably unexpectedly) as reported by read 0.');
  end;


end;



procedure TReliableUDPEndpoint.ResetKeepAliveTimer;
begin

  if assigned(multiplexer) and multiplexer.shuttingdown then exit;

  tmLastSeen := tickcount.GetTicker;

end;

procedure TMultiplexedUDPEndpoint.SplitSend(ep: TReliableUDPEndPOint; h: TReliableUDPHEader; mem: PByte; memlen: nativeint);
var
  new_h: TReliableUDPHeader;
  sh: TRUDPSplitHeader;
  t: ni;
  p: pbyte;
  packs: ni;
  iToWrite: ni;
  iWriteIdx: ni;
  new_mem: pointer;
  part_size: ni;
  reformedMem: pbyte;
  reformedSize: ni;
  chunkSizeWithHeaders: ni;
begin
  //todo 1: implement
  //create multiple CMD_SPLIT packets with partial payloads


  reformedSize := sizeof(h)+memlen;
{$IFDEF USE_REGISTERED_MEMORY}
  reformedMem := GetRegisteredMemory(reformedSize, self.classname+'.SplitSend');
{$ELSE}
  reformedMem := GetMemory(reformedSize);
{$ENDIF}

  packs := (reformedSize div 350)+1;
  part_size := lesserof(reformedSize, 350);
  iWriteIdx := 0;
//  Lock; <<-- if you do this, it will be an inner-outer deadlock
  try
    movemem32(reformedmem, @h, sizeof(h));
    movemem32(reformedmem+sizeof(h), mem, memlen);

    t := 0;
    for t:= 0 to packs-1 do begin
      new_h.Init(ep);
      new_h.SetFlagACk(ackNone);
      new_h.SetFlagCommand(true);
      new_h.command := CMD_SPLIT;
      sh.sequencenumber := h.sequencenumber;
      sh.startbyte := iWriteIdx;
      sh.packetof := packs;
      sh.packet := t;
      sh.totalsize := reformedSize;
      iToWrite := lesserof(reformedSize - iWriteIdx, part_size);
      sh.bytecount := iToWrite;
      chunkSizeWithHeaders := iToWrite+sizeof(sh);
{$IFDEF USE_REGISTERED_MEMORY}
      new_mem := GetRegisteredMemory(chunkSizeWithHeaders, self.classname+'.SplitSend(2)');
{$ELSE}
      new_mem := GetMemory(chunkSizeWithHeaders);
{$ENDIF}
      movemem32(new_mem, @sh, sizeof(sh));///write 1
      movemem32(pbyte(new_mem)+sizeof(sh), (reformedMem+iWriteIdx), iToWrite);//write2
      SendDataFromEndPoint(ep, new_h, new_mem, chunkSizeWithHeaders);
{$IFDEF USE_REGISTERED_MEMORY}
      FreeRegisteredMemory(new_mem);
{$ELSE}
      FreeMemory(new_mem);
{$ENDIF}
      inc(iWriteIDX, iToWrite);
    end;
  finally
{$IFDEF USE_REGISTERED_MEMORY}
    FreeRegisteredMemory(reformedMem);
{$ELSE}
    FreeMemory(reformedMem);
{$ENDIF}
//    Unlock;
  end;




end;

function TMultiplexedUDPEndpoint.SendDataFromEndPoint(ep: TReliableUDPEndPOint; h: TReliableUDPHEader; mem: PByte; memlen: nativeint): cardinal;
var
  buf: TIdBytes;
  seq: cardinal;
  sNilCheck: string;
begin
  result := 0;
//  Lock;
  try
    if ep=nil then
      raise Exception.Create('ep can''t be nil');
    setlength(buf, memlen + sizeof(h));
    movemem32(@buf[0], @h, sizeof(h));
    if (mem <> nil) and (memlen > 0) then begin
      movemem32(@buf[sizeof(h)], mem, memlen);
    end;

  {$IFDEF UDP_DEBUG}  Debug.Log(self,'Send Data to '+ep.Remotehost+':'+inttostr(ep.RemotePort)+' from '+inttostr(  ep.Multiplexer.FBindings[0].Port));{$ENDIF}
    try
      SendLock;
      try
        if ep = nil then
          exit;
        if ep.queue_out = nil then
          exit;
        //make a special fecid if this is untagged data
        if h.Flag_AckType = ackNone then
          seq := getnextfecid shl 20
        else
          seq := h.sequencenumber and $FFFFF;


        {$IFDEF OUT_QUEUE}
//          sNilCheck := ep.RemoteHost;
//          if sNilCheck = '' then begin
//            Log(ep, 'nilcheck failure');
//            Log(ep, 'STOP!');
//          end;
          ep.queue_out.QUeueSendData(ep, ep.queuetime, ep.RemoteHost, ep.RemotePort, buf, seq, WalkToMTU(h.parts));
          ep.queuetime := ep.queuetime + ((round((length(buf) * ep.queue_out.ticks_per_byte))));
        {$ELSE}
          result := self.ReSendEx(ep.RemoteHost, ep.RemotePort, buf, seq, WalkToMTU(h.parts));
        {$ENDIF}

      finally
        SendUnlock;
      end;

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
  finally
//    Unlock;
  end;

end;

function TMultiplexedUDPEndpoint.SendBackToSender(ifoReferencingPacket: TReliableUDPPacketLogRecord; h: TReliableUDPHEader; mem: PByte; memlen: nativeint): cardinal;
var
  buf: TIdBytes;
  seq: cardinal;
  idb: TIdBytes;
begin
  result := 0;
//  Lock; <<-- if you do this, it will be an inner-outer deadlock
  try
    if ifoReferencingPacket=nil then
      raise Exception.Create('ifoReferencingPacket can''t be nil');
    setlength(buf, memlen + sizeof(h));
    movemem32(@buf[0], @h, sizeof(h));
    if (mem <> nil) and (memlen > 0) then begin
      movemem32(@buf[sizeof(h)], mem, memlen);
    end;

//  {$IFDEF UDP_DEBUG}  UDPDebug('Send Data to '+ifoReferencingPacket..Remotehost+':'+inttostr(ep.RemotePort)+' from '+inttostr(  ep.Multiplexer.FBindings[0].Port));{$ENDIF}
    try
      SendLock;
      try
        //make a special fecid if this is untagged data
        if h.Flag_AckType = ackNone then
          seq := getnextfecid shl 20
        else
          seq := h.sequencenumber and $FFFFF;


        setlength(idb, sizeof(h)+memlen);
        movemem32(pbyte(@idb[0]), pbyte(@h), sizeof(h));
        if memlen > 0 then begin
          movemem32(pbyte(@idb[sizeof(h)]), mem, memlen);
        end;
        self.SendBuffer(iforeferencingpacket.OriginatingHost, ifoReferencingPacket.OriginatingPort, idb);

      finally
        SendUnlock;
      end;

    except
      on E: Exception do begin
//        ep.Error := E.message;
  //      ep.CloseMe := true;
  //      ep.DisconnectStarted := true;
  //      ep.connected := false;
  //      ep.connecting := false;
  //      ep.timedout := true;
      end;
    end;
  finally
//    Unlock;
  end;

end;







procedure TMultiplexedUDPEndpoint.ReSendDataFromEndPoint(ep: TReliableUDPEndPOint; h: TReliableUDPHEader; mem: PByte; memlen: nativeint; pid: cardinal);
var
  buf: TIdBytes;
  qt: ticker;
  span: ticker;
  fs: ni;
begin
//  Lock;       <<-- if you do this, it will be an inner-outer deadlock
  try
    if ep=nil then
      raise Exception.Create('ep can''t be nil');
    setlength(buf, memlen + sizeof(h));
    movemem32(@buf[0], @h, sizeof(h));
    if memlen > 0 then begin
      movemem32(@buf[sizeof(h)], mem, memlen);
    end;

  {$IFDEF UDP_DEBUG}  Debug.Log(self,'Send Data to '+ep.Remotehost+':'+inttostr(ep.RemotePort)+' from '+inttostr(  ep.Multiplexer.FBindings[0].Port));{$ENDIF}
    try
      SendLock;
      try
        qt := ep.QueueTime;
        span := round(ep.queue_out.ticks_per_byte * ep.FECSIZE);
        fs := ep.FecSize;
        self.ReSendEx(qt, span, ep.RemoteHost, ep.RemotePort, buf, pid, fs);
        span := span * round(length(buf) / fs);
        ep.QueueTime := qt+span;

      finally
        SendUnlock;
      end;

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
  finally
//    Unlock;
  end;

end;


function TMultiplexedUDPEndpoint.SendDataFromEndPoint(ep: TReliableUDPEndPOint;
  mem: PByte; memlen: nativeint): cardinal;
var
  buf: TIdBytes;
begin
  result := 0;
//  Lock;
  try
    if ep=nil then
      raise Exception.Create('ep can''t be nil');
    setlength(buf, memlen);
    if memlen > 0 then begin
      movemem32(@buf[0], mem, memlen);
    end;

  {$IFDEF UDP_DEBUG}  Debug.Log(self,'Send Data to '+ep.Remotehost+':'+inttostr(ep.RemotePort)+' from '+inttostr(  ep.Multiplexer.FBindings[0].Port));{$ENDIF}
    try
      SendLock;
      try
        raise ECritical.create('don''t use');
        ep.queue_out.QueueSendData(ep, gethighresticker, ep.RemoteHost, ep.RemotePort, buf,0,0);


      finally
        SendUnlock;
      end;

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
  finally
//    Unlock;
  end;
end;

procedure TMultiplexedUDPEndpoint.SendDataToAll(p: Pbyte; len: ni);
var
  t: ni;
begin
  Lock;
  try
    for t := 0 to FEndPoints.Count-1 do begin

      FEndpoints.Items[t].senddata(p, len);
    end;
  finally
    Unlock;
  end;


end;

procedure TMultiplexedUDPEndpoint.SendError(ep: TReliableUDPEndpoint; bSystem: boolean; b: byte; data1, data2: int64;
  diagnostic: string; bAck: boolean; fcidzero: boolean);
var
  h: TReliableUDPHeader;
  a: TDynbyteArray;
  p: pbyte;
  l: nativeint;
begin
  Lock;
  try
    h.Init(ep);
    if ep<> nil then begin
      h.tcid := ep.TOconnectionid;
      h.fcid := ep.FromConnectionid;
    end;



    h.Flag_System := bSystem;
    h.Flag_AckType := ackImmediate;
    h.FLAG_Command := true;
    h.command := ERR_CHANGE_CONNID;       //todo 2: this should not be here... should be outside function

    a := StringToAnsiByteArray(diagnostic);

    l := sizeof(data1)+sizeof(data2)+length(diagnostic);

{$IFDEF USE_REGISTERED_MEMORY}
    p := GetRegisteredMemory(l, self.classname+'.SendError');
{$ELSE}
    p := GetMemory(l);
{$ENDIF}
    if fcidzero then
      h.fcid := 0;
    movemem32(@p[0], @data1, sizeof(data1));
    movemem32(@p[sizeof(data1)], @data2, sizeof(data2));
    movemem32(@p[sizeof(data1)+sizeof(data2)], @a[0], length(a));


    h.PrepareToSend(ep);
    ep.SendPAcket(h, p, l);//<---takes ownership
  finally
    Unlock;
  end;
//  SendDataFromEndPoint(ep, h, p, l);//<---takes ownership

end;

function TMultiplexedUDPEndpoint.SendEx(const basetm, interval: ticker; const AHost: string;
  const APort: TIdPort; const ABuffer: TIdBytes; const FECSize: cardinal): cardinal;
begin
{$IFDEF USE_FEC}
  SendBufferWithFec(basetm, interval, Ahost, Aport, Abuffer, fecsize);
{$ELSE}
  SendBuffer(Ahost, Aport, Abuffer);
{$ENDIF}
  exit(0);
end;

procedure TMultiplexedUDPEndpoint.SendLock;
begin
{$IFDEF SEPARATE_SEND_LOCK}
  ECS(sect_send);
{$ELSE}
  Lock;
{$ENDIF}
end;

procedure TMultiplexedUDPEndpoint.SendUnlock;
begin
{$IFDEF SEPARATE_SEND_LOCK}
  LCS(sect_send);
{$ELSE}
  UnLock;
{$ENDIF}
end;

procedure TMultiplexedUDPEndpoint.SetHintstuffToDo(const Value: boolean);
begin
  FHint_stuff_to_do := Value;
  hint_had_stuff_to_do := true;
end;

procedure TReliableUDPEndpoint.SetDebugTag(const Value: string);
begin
  Lock;
  try
    FDebugTag := value;
  finally
    Unlock;
  end;
end;

procedure TReliableUDPEndpoint.SetQueueTime(const Value: ticker);
begin
  FQueueTime := value;
end;

procedure TReliableUDPEndpoint.SetRemoteHost(const Value: string);
begin
  FRemoteHost := Value;
  Debug.Log(self, 'Remote host was set to '+value);
end;

procedure TMultiplexedUDPEndpoint.Unlock;
begin
  systemx.LeaveCriticalSection(sect);
end;

{ TReliableUDPPacketLogRecord }


function TReliableUDPPacketLogRecord.CompleteSize: ni;
begin
  result := payload_length + sizeof(h);
end;

constructor TReliableUDPPacketLogRecord.Create(owningEndPOint: TReliableUDPEndpoint; h: TReliableUDPHeader; p: pbyte;
  len: nativeint);
begin
  Create(owningendpoint);
{$IFDEF WINDOWS}
  //Frefcount := 0;//<--ref count will be already initalized to 0 and potentially changed by now on mobile!!!!
{$ENDIF}
  self.h := h;
//  if h.sequencenumber = 2 then
//    Debug.log(self,'2 created with a payload @'+inttohex(ni(p), 8));
  payload := p;
  payload_length := len;
//   MemoryDebug('assigning memory @'+inttohex(ni(payload),sizeof(ni)*2)+' associated with object @'+inttohex(ni(pointer(self)),sizeof(ni)*2)+' packet '+debugstring);
  sendtime := -1;
  FreeWithReferences := true;
end;

constructor TReliableUDPPacketLogRecord.Create(
  owningEndPOint: TReliableUDPEndpoint);
begin
  inherited Create;
  //Frefcount := 0;<!!---- don't do this, messed up mobile!
  self.owningEndPoint := owningendpoint;
  FreeWithReferences := true;
end;

constructor TReliableUDPPacketLogRecord.Create;
begin
  inherited Create;
  //Frefcount := 0;<!!---- don't do this, messed up mobile!
  FreeWithReferences := true;
  watchme := random(100) > 90;
end;

function TReliableUDPPacketLogRecord.DebugString: string;
begin
  result := h.DebugString + '_PAY_'+inttostr(payload_length);
  if RemoteOrigin then
    result := '>.<'+result
  else
    result := '<.>'+result;
end;

procedure TReliableUDPPacketLogRecord.DeregisterFromEndpoint;
begin

//  ORIGINALLY BUILT as a SAFTETY MEASURE
//  THIS CODE IS INVALID BECAUSE IT WAS EVENTUALLY CONVERTED
//  TO USE SEQUENCE NUMBERS INSTEAD OF PACKET POINTERS
//  IT SHOULDN'T REALLY BE NEEDED

  if owningendpoint <> nil then begin
    if owningendpoint.rxLog.Has(self) then begin
      owningendpoint.rxLog.Remove(self);
      {$IFDEF REF_DEBUG}Debug.log('RElease from DeregisterFromEndpoint1');{$ENDIF}
      _Release;
    end;
    if owningendpoint.txLog.Has(self) then begin
      owningendpoint.txLog.Remove(self);
      {$IFDEF REF_DEBUG}Debug.log('RElease from DeregisterFromEndpoint2');{$ENDIF}
      _Release;
    end;
    if owningendpoint.rxDataLog.Has(self) then begin
      owningendpoint.rxDataLog.Remove(self);
      {$IFDEF REF_DEBUG}Debug.log('RElease from DeregisterFromEndpoint3');{$ENDIF}
      _Release;
    end;
    if owningendpoint.rxQueue.Has(self) then begin
      owningendpoint.rxQueue.Remove(self);
      {$IFDEF REF_DEBUG}Debug.log('RElease from DeregisterFromEndpoint4');{$ENDIF}
      _Release;
    end;
  end;


end;

destructor TReliableUDPPacketLogRecord.Destroy;
begin
{$IFDEF SEND_RECEIVE_DEBUG}
  Debug.log(self,'destroying log '+self.DebugString);
{$ENDIF}
  DeadCheck;
  if not destroying then begin
    {$IFDEF REF_DEBUG}Debug.log('AddRef from Destroy');{$ENDIF}
    _addref;
  end;
  destroying := true;
//  DeregisterFromEndpoint;
  DeadCheck;
{$IFDEF UDP_DEBUG}  Debug.Log(self,'+FREEING: '+DebugString);{$ENDIF}
  if payload <> nil then begin
//    MemoryDebug('freeing memory @'+inttohex(ni(payload),sizeof(ni)*2)+' associated with object @'+inttohex(ni(pointer(self)),sizeof(ni)*2)+' packet '+debugstring);
{$IFDEF USE_REGISTERED_MEMORY}
    FreeRegisteredMemory(payload);
{$ELSE}
    FreeMemory(payload);
{$ENDIF}
    payload := nil;
  end;
  inherited;
end;

procedure TReliableUDPPacketLogRecord.Detach;
begin

{$IFDEF UDP_DEBUG}  Debug.Log(self,DebugString+' wants to detach.');{$ENDIF}
  if assigned(owningEndPoint) then begin
{$IFDEF UDP_DEBUG}    Debug.Log(self,self.owningEndPoint.DebugMessage);{$ENDIF}
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

{ TMultiplexedUDPClient }



class procedure TMultiplexedUDPClient.FinalizeClass;
begin
  LockClass;
  try
    if CFMasterClient <> nil then begin
      try
//        CFMasterClient.DisconnectAll;
        CFMasterCLient.DetachAndFree;//ok
        CFMasterClient := nil;
      except
      end;
    end;
  finally
    UnlockClass;
  end;
  DeleteCriticalSection(class_sect);
end;

procedure TMultiplexedUDPClient.Init;
begin
  inherited;
  self.FIsServer := false;
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

function TSimpleReliablePrivateServerEndpoint.DoConnect: boolean;
begin
  result := false;
  raise ETransportError.create('Connect is irrelevant for this connection');
end;

procedure TSimpleReliablePrivateServerEndpoint.DebugTagUpdated(s: string);
begin
  inherited;
  if assigned(cli) then begin
    cli.DebugTag := s;
  end;
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

procedure TSimpleReliablePrivateServerEndpoint.DoDisconnect;
begin
  cli.Disconnect;

  inherited;

end;

function TSimpleReliablePrivateServerEndpoint.DoCheckForData: boolean;
begin
  raise Ecritical.create('not implemented');
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

function TSimpleReliablePrivateServerEndpoint.DoWaitForData(
  timeout: cardinal): boolean;
begin
  result := cli.WaitForData(timeout);
end;


procedure oinit;
begin
  net_stats_clear_rate_tx := TRingStats.create;
  net_stats_clear_rate_rx := TRingStats.create;
  net_stats_queue_rate_tx := TRingStats.create;
  net_stats_packets_tx := TRingStats.create;
  net_stats_packets_rx := TRingStats.create;
  net_stats_packets_retx := TRingStats.create;
  net_stats_packets_retx_rq := TRingStats.create;

  net_stats_clear_rate_tx.Name := 'RUDP TX Clear';
  net_stats_clear_rate_rx.Name := 'RUDP RX Clear';
  net_stats_queue_rate_tx.Name := 'RUDP TX Queue Rate';
  net_stats_packets_tx.Name := 'RUDP Packets TX';
  net_stats_packets_rx.Name := 'RUDP Packets RX';
  net_stats_packets_retx.Name := 'RUDP Packets ReTX';
  net_stats_packets_retx_rq.Name := 'RUDP Packets ReTX Requested';


  rsMon.RegisterRingStat(net_stats_clear_rate_tx);
  rsMon.RegisterRingStat(net_stats_clear_rate_rx);
  rsMon.RegisterRingStat(net_stats_queue_rate_tx);
  rsMon.RegisterRingStat(net_stats_packets_tx);
  rsMon.RegisterRingStat(net_stats_packets_rx);
  rsMon.RegisterRingStat(net_stats_packets_retx);
  rsMon.RegisterRingStat(net_stats_packets_retx_rq);

  TMultiplexedUDPClient.InitClass;

  rudpstats.Init;

{$IFDEF USE_SHARED_QUEUES}
  {$IFDEF SHARED_MULTIQUEUES}
    shared_rudpoutqueue := TRUDPOUtQueue.create;
    shared_RUDPPIPEInQueue := TRUDPPipeInQueue.create;
    shared_rudpoutqueue.Start;
    shared_RUDPPIPEInQueue.Start;

  {$ELSE}
    shared_rudpoutqueue := TPM.Needthread<TRUDPOUtQueue>(nil);
  //  shared_rudpoutqueue.MAxItemsINQueue := 1024;
    shared_rudpoutqueue.start;

    shared_RUDPPIPEInQueue:= TPM.Needthread<TRUDPPipeInQueue>(nil);
  //  shared_RUDPPIPEInQueue.MAxItemsINQueue := 1024;
    shared_RUDPPIPEInQueue.start;
  {$ENDIF}
{$ENDIF}





end;

procedure ofinal;
begin
  rsMon.UnregisterRingStat(net_stats_clear_rate_tx);
  net_stats_clear_rate_tx.DetachAndfree;
  rsMon.UnregisterRingStat(net_stats_clear_rate_rx);
  net_stats_clear_rate_rx.DetachAndfree;
  rsMon.UnregisterRingStat(net_stats_queue_rate_tx);
  net_stats_queue_rate_tx.DetachAndfree;
  rsMon.UnregisterRingStat(net_stats_packets_tx);
  net_stats_packets_tx.DetachAndfree;
  rsMon.UnregisterRingStat(net_stats_packets_rx);
  net_stats_packets_rx.DetachAndfree;
  rsMon.UnregisterRingStat(net_stats_packets_retx);
  net_stats_packets_retx.DetachAndfree;
  rsMon.UnregisterRingStat(net_stats_packets_retx_rq);
  net_stats_packets_retx_rq.DetachAndfree;
{$IFDEF USE_SHARED_QUEUES}

  {$IFDEF SHARED_MULTIQUEUES}
    Debug.Log('Freeing shared_rudpoutqueue');
    shared_rudpoutqueue.Free;
    Debug.Log('Freeing shared_RUDPPIPEInQueue');
    shared_RUDPPIPEInQueue.free;
    shared_rudpoutqueue := nil;
    shared_RUDPPIPEInQueue := nil;
  {$ELSE}
    TPM.NoNeedthread(shared_rudpoutqueue);
    TPM.NoNeedthread(shared_RUDPPIPEInQueue);
  {$ENDIF}
{$ENDIF}

  TMultiplexedUDPClient.FinalizeClass;
end;

procedure oLATEfinal;
begin
  //
end;


{ TRUDPDebugInfo }

procedure TRUDPDebugInfo.Init;
var
  s: string;
  a: TDynByteArray;
begin
  threadid := TThread.CurrentThread.ThreadID;
  s := systemx.DLLName;
  s := extractfilename(s);
  a := stringtoansibytearray_NULL_TERMINATED(s);

  movemem32(@self.module[0], @a[0], length(a));



end;

function TRUDPDebugInfo.ToString: string;
begin
  result := 'threadid='+inttostr(threadid)+':'+AnsiPOinterToString(@module[0]);
end;

{ TRUDPSystemThread }

procedure TRUDPSystemThread.DoExecute;
begin
  inherited;
  endpoint.SystemThreadExecute(self);
end;

{ TRUDPUserThread }

procedure TRUDPUserThread.DoExecute;
begin
  inherited;


  if Endpoint = nil then
    raise ECritical.create('User thread''s endpoint was removed.  The User thread should keep its endpoint until it releases it, even if the endpoint is removed from the multiplexer.');
  if assigned(EndPoint) then
    Endpoint.UserThreadExecute(self);
end;

{ TRUDPEndpointThread }

procedure TRUDPEndpointThread.Detach;
begin
  EndPoint := nil;
  inherited;
end;

procedure TRUDPEndpointThread.Init;
begin
  inherited;
  WAitBeforeAbortTime := 12000;
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

{$IFDEF ENDPOINT_REFERENCES}
  if assigned(FEndpoint) then
{$IFDEF VERBOSE_REFERENCES}FEndpoint.ReleaseBy('Set Endpoint');{$ELSE}FEndpoint._Release;{$ENDIF}
{$ENDIF}

  FEndpoint := Value;

{$IFDEF ENDPOINT_REFERENCES}
  if assigned(FEndpoint) then
{$IFDEF VERBOSE_REFERENCES}FEndpoint.AddRefBy('Set Endpoint');{$ELSE}FEndpoint._AddRef;{$ENDIF}
{$ENDIF}


end;

{ TReliableUDPPacketLog }

constructor TReliableUDPPacketLog1.Create;
begin
  inherited;
//  Debug.Log('log Created '+inttohex(int64(pointer(self)), 16));
end;

destructor TReliableUDPPacketLog1.Destroy;
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

function TReliableUDPEndpointList.FindBySeed(iSeed: int64): TReliableUDPEndpoint;
var
  i: nativeint;
  rep: TReliableUDPEndpoint;
begin
  result := nil;
  Lock;
  try
    for i := 0 to count-1 do begin
      rep := FList[i];
      if (rep.Seed = iSeed) then begin
        result := rep;
        break;
      end;
    end;


  finally
    Unlock;
  end;
end;


procedure TReliableUDPEndpoint.RequestRetransmissionOfFutureGaps;
var
  h: TReliableUDPHeader;
  t: ni;
  iHighest: int64;
  ii: int64;
  iLastSolid: int64;
  solid: TtreeItem_ReliableUDPPacketLogRecord;
begin
  if DIsableAcks then
    exit;

  iLastSolid := -1;
{$IFDEF QUEUE_INCOMING_UDP}
  if (multiplexer.queue_in.EstimatedSize > 0) then
    exit;
{$ENDIF}

//  AckIncoming(true);
{$IFNDEF ALLOW_IMMEDIATE_RETRANS}
  exit;
{$ENDIF}
  if GetTimeSince(lastReorderRequest) < 100 then
    exit;
  if SmartLock then
  try

    //determine highest sequence
    iHighest := -1;
    rxLog.Iterate(
      procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
      begin
        ii := TtreeItem_ReliableUDPPacketLogRecord(ABTreeItem).packet.h.sequencenumber;
        if ii > iHighest then
          iHighest := ii;
      end
    );

   for t := _nextexpectedSequenceNumber to iHighest do begin
      if not rxLog.Has(t) then begin

        h.Init(self);
        h.tcid := self.TOConnectionid;
        h.fcid := self.FROMConnectionid;
        h.Flag_AckType := ackNone;
        h.command := RUDP_CMD_REQUEST_RETRANS;
        h.Flag_Command := true;
        h.sequencenumber := -1;//-1 because there's no ack coming back
        h.expected_sequencenumber := t;
{$IFDEF UDP_DEBUG}        Debug.Log(self,'Sending future retrans request '+inttostr(h.expected_sequencenumber-1));{$ENDIF}
        //h.PrepareToSend(self);
        SendPacket(h, nil, 0);
      end;
    end;
    lastReorderRequest := GetTicker;
  finally
    Unlock;
  end;

end;

procedure TReliableUDPEndpoint.REquestImmediateRetransmission;
var
  h: TReliableUDPHeader;
begin
//  AckIncoming(true);
{$IFNDEF ALLOW_IMMEDIATE_RETRANS}
  exit;
{$ENDIF}
  if SmartLock then
  try
    if gettimesince(ReTransRequesttime) < 200 then
      exit;
    ReTransRequestTime := GetTicker;
    h.Init(self);
    h.tcid := self.TOConnectionid;
    h.fcid := self.FROMConnectionid;

    h.Flag_AckType := ackNone;
    h.command := RUDP_CMD_REQUEST_RETRANS;
    h.Flag_Command := true;
    h.sequencenumber := -1;//-1 because there's no ack coming back
    h.expected_sequencenumber := _nextexpectedSequenceNumber;
{$IFDEF UDP_DEBUG}    Debug.Log(self,'Sending retransmission request '+inttostr(h.expected_sequencenumber-1));{$ENDIF}
    //h.PrepareToSend(self);
    SendPacket(h, nil, 0);
  finally
    Unlock;
  end;
end;




{ TMultiplexedUDPServer }


procedure TMultiplexedUDPServer.Init;
begin
  inherited;
  self.FIsServer := true;
end;

{ TPacketQueueItem }

procedure TPacketQueueItem.Detach;
begin
  if detached then exit;
  if data <> nil then
{$IFDEF USE_REGISTERED_MEMORY}
    FreeRegisteredMemory(data);
{$ELSE}
    FreeMemory(data);
{$ENDIF}
  data := nil;

  inherited;

end;

procedure TPacketQueueItem.DoExecute;
begin
  inherited;
{$IFDEF QUEUE_INCOMING_UDP}
  ep.ProcessUDPRead(self, false);
{$ENDIF}
end;

procedure TPacketQueueItem.Init;
begin
  inherited;
  autodestroy := true;
end;

procedure TPacketQueueItem.SetEP(const Value: TMultiplexedUDPEndpoint);
begin
  if Fep <> nil then
    FEp._release;

  FEp := Value;

  if Fep <> nil then
    Fep._addref;
end;

{ TReliableUDPPacketDataLog }


{ TRUDPStats }

procedure TRUDPStats.Init;
begin
  FillMem(@self, sizeof(self), 0);
end;

{ TtreeItem_ReliableUDPPacketLogRecord }

function TtreeItem_ReliableUDPPacketLogRecord.Compare(
  const [unsafe] ACompareTo: TBTreeItem): ni;
//!!IFOREF
var
  itm: TtreeItem_ReliableUDPPacketLogRecord;
begin
  itm := TtreeItem_ReliableUDPPacketLogRecord(ACompareTo);
  result := itm.packet.h.sequencenumber - self.packet.h.sequencenumber;
//  result := self.packet.h.sequencenumber - itm.packet.h.sequencenumber;

end;

{ TReliableUDPPacketLog2 }

procedure TReliableUDPPacketLog2.Add(ifo: TReliableUDPPacketLogRecord);
var
  ti: TtreeItem_ReliableUDPPacketLogRecord;
begin
  ti := TtreeItem_ReliableUDPPacketLogRecord.create;
  ti.packet := ifo;
  inherited add(ti);

end;

function TReliableUDPPacketLog2.Find(
  iSeqNumber: int64): TtreeItem_ReliableUDPPacketLogRecord;
var
  ti: TtreeItem_ReliableUDPPacketLogRecord;
  tin: int64;
begin
  ti := TtreeItem_ReliableUDPPacketLogRecord(self.Root);

  while ti <> nil do begin
    tin := ti.packet.h.sequencenumber;
    if iSeqNumber = tin then
      exit(ti);

    if iSeqNumber < tin then
      ti := TtreeItem_ReliableUDPPacketLogRecord(ti.FLeftNode)
    else
      ti := TtreeItem_ReliableUDPPacketLogRecord(ti.FRightNode);

  end;

  result := ti;
end;

function TReliableUDPPacketLog2.Has(ifo: TReliableUDPPacketLogRecord): boolean;
var
  i: TtreeItem_ReliableUDPPacketLogRecord;
begin
  i := Find(ifo.h.sequencenumber);

  result := i <> nil;
  if result then
    result := i.packet = ifo;

end;

function TReliableUDPPacketLog2.Has(iSeqNumber: int64): boolean;
begin
  result := Find(iSeqNumber) <> nil;
end;

function TReliableUDPPacketLog2.Has(
  ti: TtreeItem_ReliableUDPPacketLogRecord): boolean;
begin
  result := Find(ti.packet.h.sequencenumber) <> nil;
end;

procedure TReliableUDPPacketLog2.Remove(ifo: TReliableUDPPacketLogRecord);
var
  ti: TBTreeItem;
begin
  ti := Find(ifo.h.sequencenumber);
  if ti <> nil then begin
    RemoveTI(ti);
  end;
end;

{ TWorthless }

function TWorthless.GetList: TIdUDPListenerThreadList;
begin
//  result := TIdUDPListenerThreadList(self).FList;
  result := nil;
end;

{ TRUDPOutQueue }

{$IFNDEF USE_SHARED_QUEUES}
function TRUDPOutQueue.GetNextItem: TQueueItem;
begin
{$IFDEF OUT_QUOTA}
  while not quotaavailable do
    {sleep(0)};
{$ENDIF}

  result := FWorkingItems[0];
  lasttxtime := GetHighResTicker;
  lasttxbytes := TRUDPOutQueueItem(result).datalen;

end;
{$ENDIF}

procedure TRUDPOutQueue.Init;
begin
//  ticks_per_byte := (10*MILLION)/((10*MILLION)/8);
  ticks_per_byte := 8;

  inherited;

end;

procedure TRUDPOutQueue.QueueSendData(ep: TReliableUDPEndpoint; basetm: ticker; AHost: string; Aport: ni; data: TIDBytes; seq, fec: ni);
var
  itm: TRUDPOutQueueItem;
  p: pbyte;
  l: ni;
begin
  if ep.noMorePacketsAllowed then
    exit;

  itm := TRUDPOutQueueItem.Create;
  itm.ep := ep;
  l := length(DATA);
  p := nil;
  if l > 0 then begin
{$IFDEF USE_REGISTERED_MEMORY}
    p := GetRegisteredMemory(l, self.classname+'.QueueSendData');
{$ELSE}
    p := GetMemory(l);
{$ENDIF}
    movemem32(p, @data[0], l);
  end;
  itm.data := p;
  itm.datalen := l;
  itm.fec := fec;
  itm.seq := seq;
  UniqueString(AHost);
  if ahost = '' then
    raise ECritical.create('you can''t send data to nil host!');
  itm.peerip :=  Ahost;
  itm.peerport := aport;
  itm.qt := basetm;
  addItem(itm);

end;

procedure TRUDPOutQueue.QueueSendData2(ep: TReliableUDPEndpoint; basetm: ticker; AHost: string;
  Aport: ni; data: PByte; sz: ni);
var
  itm: TRUDPOutQueueItem;
  p: pbyte;
  l: ni;
begin
  if ep.noMorePacketsAllowed then
    exit;

  itm := TRUDPOutQueueItem.Create;
  itm.ep := ep;
  l := sz;
  p := nil;
  if l > 0 then begin
{$IFDEF USE_REGISTERED_MEMORY}
    p := GetRegisteredMemory(l, self.classname+'.QueueSendData2');
{$ELSE}
    p := GetMemory(l);
{$ENDIF}
    movemem32(p, data, l);
  end;
  itm.data := p;
  itm.datalen := l;
  itm.fec := 0;
  itm.seq := 0;
  itm.peerip := Ahost;
  itm.peerport := aport;
  itm.qt := basetm;
  addItem(itm);
end;

function TRUDPOutQueue.QuotaAVailable: boolean;
var
  tmSince: ticker;
begin
  tmSince := gettimesincehr(lasttxtime);
  result := tmSince > (ticks_per_byte * (lasttxbytes));
end;


procedure TRUDPOutQueue.SlowDown;
begin
  if random(100) > 98 then
    Debug.Consolelog(FloatPrecision(ticks_per_byte, 2));
  if ticks_per_byte < 32 then begin
    ticks_per_byte := ticks_per_byte * (1.008);
//    ticks_per_byte := ticks_per_byte * (2);
  end;
end;

procedure TRUDPOutQueue.SpeedUp;
begin
//ticks per byte     bandwidth
          //10                  1Mbyte / 8Mbit
          //1                  10Mbyte / 16mbit
          //0.1               100Mbyte / 160Mbit

  if random(100) > 98 then
    Debug.Consolelog(FloatPrecision(ticks_per_byte, 2));

  if ticks_per_byte > 0.01 then
    ticks_per_byte := ticks_per_byte * (0.90);
//    ticks_per_byte := ticks_per_byte * (0.992);
end;

{ TRUDPOutQueueItem }

procedure TRUDPOutQueueItem.Detach;
begin
  if data <> nil then
{$IFDEF USE_REGISTERED_MEMORY}
    FreeRegisteredMemory(data);
{$ELSE}
    FreeMemory(data);
{$ENDIF}

  data := nil;

  inherited;

end;

procedure TRUDPOutQueueItem.DoExecute;
var
  buf: TIdbytes;
  hrt: ticker;
  wt: ticker;
begin
  inherited;
  try
    setlength(buf, datalen);
    if datalen > 0 then
      movemem32(@buf[0], data, datalen);

  {$DEFINE OUT_QUEUE_TIMING}
  {$IFDEF OUT_QUEUE_TIMING}
    hrt := gethighresticker;
    while (hrt < qt) do begin
  {$DEFINE SLEEP_WAIT_OUT}
  {$IFDEF SLEEP_WAIT_OUT}
      wt := round((qt-hrt) / 10000);
      if wt > 0 then
        sleep(lesserof(wt,100));
  {$ENDIF}
      hrt := gethighresticker;
    end;
  {$ENDIF}
    ep.totalTX := ep.totalTX + length(buf);
  {$IFDEF USE_FEC}
    ep.Multiplexer.ReSendBufferWithFec(qt, round(ep.queue_out.ticks_per_byte*fec),  peerip, peerport, buf, fec, seq);
  {$ELSE}
    ecs(ep.Multiplexer.sectUDPsend);
    try
      ep.Multiplexer.SendBuffer(Peerip, peerport, buf);
    finally
      lcs(ep.MultiPlexer.sectUDPsend);
    end;
  {$ENDIF}
  except
  end;

end;

procedure TRUDPOutQueueItem.Init;
begin
  inherited;
  autodestroy := true;

end;

procedure TRUDPOutQueueItem.SetEP(const Value: TReliableUDPEndpoint);
begin
  if Fep <> nil then
    Fep._Release;

  Fep := Value;

  if Fep <> nil then
    Fep._AddRef;
end;

{ TMTUTestResult }

function TMTUTestResult.CanUse: boolean;
begin
  result := gettimesince(lastsuccess) < MTU_TEST_EXPIRATION_TIME;
end;

procedure TMTUTestResult.MarkSuccessful;
begin
  lastsuccess := getticker;
  lasttest := lastsuccess;
end;

procedure TMTUTestResult.REset;
begin
//  lasttest := 0;
  lastsuccess := 0;
end;

function TMTUTestResult.TimeToRetest: boolean;
begin
  result := (lasttest=0) or (lesserof(GEtTimeSince(lasttest),GEtTimeSince(lastsuccess)) > (MTU_RETEST_TIME+sz)) ;
end;

{ TSplitAssembly }

function  TSplitAssembly.FlagPArtCompleteAndReportCompletion(partid: ni): boolean;
var
  t: ni;
begin
  result := false;
  if partsavailable[partid] = false then begin
    inc (uniquePartsIn);
    partsavailable[partid] := true;
    result := (uniquePartsIn = totalparts);
  end;
end;

procedure TSplitAssembly.Init;
begin
  fillmem(@partsavailable[0], sizeof(partsavailable), 0);
  seq := -1;
  uniquePartsIn := 0;
end;

{ TEndPOintInputQueueItem }

procedure TEndPOintInputQueueItem.Detach;
begin
  if detached then
    exit;

  ifo := nil;

  ep := nil;
  inherited;


end;

procedure TEndPOintInputQueueItem.DoExecute;
begin
  inherited;
  ep.DispatchInput_Sync(self);

end;

procedure TEndPOintInputQueueItem.SetEp(const Value: TReliableUDPendpoint);
var
  loc: TReliableUDPendpoint;
begin
  loc := Fep;

  if loc <> nil then
    loc._Release;

  Fep := Value;

  if value <> nil then
    value._AddRef;

end;

procedure TEndPOintInputQueueItem.SetIfo(
  const Value: TReliableUDPPacketLogRecord);
begin
  if value <> FIfo then begin
    if Fifo <> nil then
      FIfo._release;
    if value <> nil then
      value._addref;

    FIfo := Value;
  end;




end;

{ TRUDPEndPointFiber }

procedure TRUDPEndPointFiber.Detach;
begin
  Endpoint := nil;
  inherited;

end;

procedure TRUDPEndPointFiber.SetEndpoint(const Value: TReliableUDPEndpoint);
begin
  Lock;
  try
  {$IFDEF ENDPOINT_REFERENCES}
    if assigned(FEndpoint) then
{$IFDEF VERBOSE_REFERENCES}FEndpoint.ReleaseBy('Fiber.SetEndpoint');{$ELSE}FEndpoint._Release;{$ENDIF}
  {$ENDIF}

    FEndpoint := Value;

  {$IFDEF ENDPOINT_REFERENCES}
    if assigned(FEndpoint) then
{$IFDEF VERBOSE_REFERENCES}FEndpoint.AddRefBy('Fiber.SetEndpoint');{$ELSE}FEndpoint._AddRef;{$ENDIF}
  {$ENDIF}
  finally
    Unlock;
  end;
end;

{ TRUDPPipeInQueue }


{ TMultiplexedTCPCompanion }

function TMultiplexedTCPCompanion.DoExecute(AContext: TIdContext): Boolean;
var
  head: TIdBytes;
  body: TIDBytes;
  sz: ni;
begin
  head := idsocket_GuaranteeRead(acontext.connection.Socket, 4);
  if (head[0] <> ord(ansichar('W')))
  or (head[1] <> ord(ansichar('R'))) then begin
    exit(false);
  end;

  sz := (head[2] shl 8) + (head[3]);
  body := idsocket_GuaranteeRead(acontext.connection.Socket, sz);


  result := true;


end;

initialization

orderlyinit.init.RegisterProcs('SimpleReliableUDP', oinit, nil, ofinal, oLATEfinal,  'ManagedThread,better_indy,ringstats');


finalization


end.
