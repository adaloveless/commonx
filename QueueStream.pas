unit QueueStream;
interface
{x$DEFINE HOLD}
{$IFDEF MSWINDOWS}
{$DEFINE PACKED_MASK}
//!!!!WINDOWS ONLY FOR NOW!!!!

{x$DEFINE DOUBLE_READ}
{x$DEFINE DISABLE_WRITES}
//2018 What I Remember
//1. Unbuffered file stream implements buffers against buffered storage
//2. It can only read in multiples of 512
//3. It can only write in multiple of 512
//4. Therefore it must maintain an independent file length which it
//   does using footers on the last 512-byte page of the file.
//5. It also implements a SIDE FETCH THREAD
//6. The side fetch thread attempts to READ AHEAD adjacent to the
//     current buffer
//7. There is also some kind of system that tries to optimize the read-ahead
//     length based on some sort of statistics.
//8. The side fetch thread might also maintain the buffer-write-through
//     system which attempts to shove data to the disc in an efficient
//     manner.
//9. The whole system should run most optimally when the slowest
//     bottle-neck is saturated.  So a goal should be to saturate the
//     bottle-neck with useful operations

//Structure of the TUnbufferedFileStream:
//locks of interest
//  lckState: usage lock (maintained by external users)
//  understream lock (to be touched only by page-fetching routines and EOF writer
//  buffer lock (locked whenerver it is important that the buffers not be reordered)

//Refector Process

//From a users's standpoint, there's basically the following ops:
//1. Read()
//2. Write()
//3. WriteZeros() - write a bunch of zeroes without consuming a bunch of RAM
//4. SetSize()
//---
//5. CReate()
//6. Destroy();

//From an internal Standpoint, there's the following ops:
//1. FetchPage for reading
//2. FetchPage for writing
//3. WriteEOF
//4. FlushPage
//5. Sort buffers
//6. Read from buffer
//7. Write To Buffer
//8. Sidefetch

//to get where I'm going:
//1. Add a queue/thread to the stream
//2. Ensure that Read() can potentially get A buffer without locking the understream (brief lock on buffer list is okay)
//3. Ensure that SideFetch() can potentially fetch a buffer without blocking Read() or Write()
//4. Ensure that Write() can potentially write without locking the understream (brief lock on buffer list is okay)
//5. SideFetch after every Read()



{x$DEFINE ALERT_WRITE}
{x$DEFINE LOCK_UNDER_INSTEAD_OF_BACK}

{$DEFINE CHECK_SIZE_BEFORE_READ}
{$DEFINE ALLOW_UB_SIDE_FETCH}//yes, tries prefetches n stuff
{$DEFINE MINIMAL_UB_PREFETCH}
{$DEFINE ALLOW_UB_WRITE_BEHIND}//yes, flush-behind keeps the pipes clean

{x$DEFINE ALLOW_SIDEFETCH_REQUEUE}
{$DEFINE USE_PERIODIC_QUEUE}
{$DEFINE READ_BEFORE_WRITE}
{$DEFINE DO_NOT_COMBINE_WRITES}//????crash if combination is enabled
{$DEFINE USE_UNBUFFERED}
{$DEFINE ALLOW_UNBUFFERED_FLAG}
{$DEFINE USE_LINKED_BUFFERS}
{x$DEFINE ALLOW_SYNCHRONOUS_READS}//if enabled, reads can potentially be handled in calling thread, so you're relying more of prefetches to bring things into memory
{x$DEFINE USE_SPOT_FLUSH}
{$DEFINE USE_OPTIMAL_FLUSH}//doesn't work
{x$define COMPLEX_STREAM}//too slow
{x$DEFINE IDLE_FETCH}//deprecated for a real side-fetch thread
{$DEFINE TIME_CRITICAL}
{x$DEFINE LOW_PRIORITY_SIDE_FETCH}
{x$DEFINE CONTEXT_SWITCH_ON_QUEUE_ADD}

{x$DEFINE BAD_USES}
uses
  tickcount, writebuilder, ringstats, globaltrap, fileproxy,AnonCommand, globalmultiqueue,
{$IF Defined(MSWINDOWS)}
  windows,
{$ENDIF}
  betterobject, multibufferstream, SimpleQueue, sharedobject, typex, classes, sysutils, managedthread, numbers,debug, betterfilestream, collision, systemx, periodicevents, commandprocessor, commands_system, linked_list;

const
  //ALLOCATION_UNIT_SIZE = 4096;
//  L0_CACHE_SIZE:int64 = int64(262144);
//  L0_CACHES:int64 = 256;
  STREAM_SPIN = 0;
  L1_CACHE_SEGMENTS = 2048;
  L1_TOTAL_CACHE:int64 = int64(262144*L1_CACHE_SEGMENTS);

  MAX_QUEUE_COMBINE_SIZE = 262144*4;

{x$DEFINE TINY}
{x$DEFINE NOT_SO_TINY}
{x$DEFINE HUGE}
{$IFDEF TINY}
  {$IFDEF NOT_SO_TINY}
//    UNBUFFERSIZE = 65536;
//    UNBUFFERSHIFT = 16;
//    UNBUFFERMASK: Uint64 = int64(UNBUFFERSIZE)-int64(1);
//    UNBUFFERED_BUFFERED_PARTS = 256*4;
  {$ELSE}
//    UNBUFFERSIZE = 2048;
//    UNBUFFERSHIFT = 11;
//    UNBUFFERMASK: Uint64 = UNBUFFERSIZE-1;
//    UNBUFFERED_BUFFERED_PARTS = 32;
  {$ENDIF}
{$ELSE}
  {$IFDEF HUGE}
//  UNBUFFERSIZE = int64(262144*2*2);
//  UNBUFFERSHIFT = int64(18+2);
//  UNBUFFERMASK: Uint64 = int64(UNBUFFERSIZE)-int64(1);
//  UNBUFFERED_BUFFERED_PARTS = 128;
  {$ELSE}
//  UNBUFFERSIZE = int64(262144);//<<<<--CHANGE TOGETHER!
//  UNBUFFERSHIFT = int64(18);//<<<<<<--------^

  UNBUFFERSIZE = int64(262144);//<<<<--CHANGE TOGETHER!  MUST BE MULTIPLE OF 64... 8 for bits in byte mask and 8 for 8byte int64-optimized array checks
  UNBUFFERSHIFT = int64(18);//<<<<<<--------^

  UNBUFFERMASK: Uint64 = int64(UNBUFFERSIZE)-int64(1);
  UNBUFFERED_BUFFERED_PARTS = 514;//514;//514;//!! SHoULD EQUAL BIG BLOCK SIZE
  {$ENDIF}
{$ENDIF}

  //PAYLOAD_PREFETCH_ALLOWANCE = UNBUFFERED_BUFFERED_PARTS shr 3;

type
  TPerformanceHints = record
    pendingOps: nativeint;
  end;
  PPerformancehints = ^TPerformanceHints;

  TFetchBufferStage = (fbInitial, fbComparative, fbRestorative);
  TDirtiness = (dirtClean, dirtSome, dirtAll);
  TUnbufferedFileStream = class;//forward

  TReadCommand = class;//forward
  TWriteCommand = class;//forward

  TStreamQueue = class(TSimpleQUeue)
  protected
    potentiallyHasReads: boolean;

    scratch_queue: array[0..15] of TWriteCommand;

    function GetRead: TReadCommand;inline;

{$IFDEF READ_BEFORE_WRITE}
    function GetNextItem: TQueueItem;override;
{$ENDIF}
    procedure OptimizeIncoming(incomingitem: TQueueItem);override;
    function CombineWrites(var a: TWriteCommand; b: TWriteCommand): boolean;
    procedure OptimizeSingle(var qi: TWriteCommand);
    procedure AfterUrgentCopy;override;
  public
    procedure Init; override;

  end;
  TQueueStream = class;//forward

  TQueueStreamItem = class(TQueueItem)
  private
    FStream: TQueueStream;

  public
    ExecutionTime: ticker;
    property Stream: TQueueStream read FStream write FStream;
    function DebugString: string;override;
    procedure Execute;override;//OUTER EXECUTE!!!
  end;


  TWriteCommand = class(TQueueStreamItem)
  private
    FCount: int64;
    FOriginalPOinter: pbyte;
    FPOinter: pbyte;
    Faddr: int64;
    procedure SetPointer(const Value: pbyte);inline;
    procedure SetCount(const Value: int64);inline;
    procedure MovePOinterData;inline;
    procedure SetAddr(const Value: int64);inline;
  public
    combined: boolean;
    destructor Destroy;override;
    procedure DoExecute;override;
    property Addr: int64 read FAddr write SetAddr;
    property Ptr: pbyte read FPOinter write SetPointer;
    procedure GiveExternalPointer(p: pbyte; cnt: ni);
    property Count: int64 read FCount write SetCount;
    function DebugString: string;override;
  end;

  TWriteZeroesCommand = class(TQueueStreamItem)
  private
    FCount: int64;
    FOriginalPOinter: pbyte;
    Faddr: int64;
  public
    destructor Destroy;override;
    procedure DoExecute;override;
    property Addr: int64 read FAddr write FAddr;
    property Count: int64 read FCount write FCount;
    function DebugString: string;override;
  end;

  TSideFetchCommand = class(TQueueStreamItem)
  public
    procedure Init;override;
    procedure DoExecute;override;
  end;
  TReadCommand = class(TQueueStreamItem)
  private
    FCount: int64;
    FPOinter: pbyte;
    FAddr: int64;
    FResult: int64;
    procedure SEtAddr(const Value: int64);inline;
  public
    procedure Init;override;
    procedure DoExecute;override;
    property Addr: int64 read FAddr write SEtAddr;
    property Ptr: pbyte read FPOinter write FPOinter;
    property Count: int64 read FCount write FCount;
    property Result: int64 read FResult write Fresult;
    function DebugString: string;override;
  end;


  TSetSize = class(TQueueStreamItem)
  private
    FNewSize: int64;
  public
    procedure Init;override;

    procedure DoExecute;override;
    property NewSize: int64 read FNewSize write FNewSize;
    function DebugString: string;override;

  end;


  TQueueStream = class(TSharedObject)
  strict
  private
    FTrackedSize: int64;
  strict protected
    FUnderStream: TStream;
    FQueue: TStreamQueue;
  private
    FEnableQueue: boolean;
    procedure SetUnderStream(const Value: TStream);
  protected
    property UnderStream: TStream read FUnderStream write SetUnderStream;

    procedure CleanupUnderStream;
    function GEtSize: int64;virtual;
    procedure SetSize(const Value: int64);virtual;
    procedure ReadInitialSize;virtual;

  public
    OwnsStream: boolean;
    ops, spikes: int64;
    xatemp: array[0..65535] of byte;
    function estimated_queue_size: ni;inline;

    procedure Init;override;
    procedure OnQueueEmpty(sender: TObject);
    procedure OnQueueNotEmpty(sender: TObject);
    procedure Detach;override;
    destructor Destroy;override;
    procedure BeforeDestruction;override;

    procedure BeginWrite(const addr: int64; const pointer: pbyte; const size: int64);
    procedure BeginWriteZeros(const addr: int64; const size: int64);
    function BeginRead(const addr: int64; const pointer: pbyte; const size: int64; bDisallowSynchronous: boolean; bForget: boolean = false): TReadCommand;inline;
    function EndRead(qi: TREadCommand): int64;inline;
    property Size: int64 read GEtSize write SetSize;

    function TryEnd(qi: TQueueItem): boolean;
    procedure EndCommand(qi: TQueueItem);
    function IsReady: boolean;
    procedure FinalizeBuffers;
    property EnableQueue: boolean read FEnableQueue write FEnableQueue;
    property Queue: TStreamQueue read FQueue;
  end;

  TFileStreamClass = class of TFileSTreamWithVirtualConstructors;

  TStreamClass = class of TSTream;

  TQueuedFileSTream = class(TQueueStream)
  private
    FFileName: string;
    FOpenMode: cardinal;
    FDisableLookAhead: boolean;
    FDisableMinimumPrefetch: boolean;
    FFLags: cardinal;
    procedure SetFileName(const Value: string);
  protected
    function CreateAndConfigureUnderclasses(sFile: string): TStream;virtual;
  public
    function GetUnderClass: TStreamClass;virtual;
    procedure ConfigureUnderclass;virtual;
    procedure INit;override;
    property OpenMode: cardinal read FOpenMode write FOpenMode;
    property Flags: cardinal read FFLags write FFLags;
    property FileNAme: string read FFileName write SetFileName;
    property DisableLookAhead: boolean read FDisableLookAhead write FDisablelookAhead;
    property DisableMinimumPrefetch: boolean read FDisableMinimumPrefetch write FDisableMinimumPrefetch;
    procedure GrowFile(iSize: int64);virtual;
    procedure Flush;
  end;

  TAdaptiveQueuedFileStream = class(TQueuedFileStream)
  private
    FPosition: int64;
    PerformanceHints: TPerformanceHints;
    procedure AdaptiveRead(p: pbyte; iSize: int64);inline;
  public
    constructor Create(const AFileName: string; Mode: cardinal; Rights:cardinal; Flags: cardinal);reintroduce;overload;virtual;
    constructor Create(const AFileName: string; Mode: cardinal);reintroduce;overload;virtual;

    property Position: int64 read FPosition write FPosition;
    function Seek(iPos: int64; origin: TseekOrigin): int64;overload;inline;
    function Seek(iPOs: int64; origin: int64): int64;overload;inline;
    procedure AdaptiveWrite(p: Pbyte; iSize: int64);inline;
    function BeginAdaptiveRead(p: pbyte; iSize: int64;bDisallowSynchronous: boolean; bForget: boolean = false): TReadCommand;inline;
    function EndAdaptiveRead(qi: TReadCommand): int64;inline;
    procedure AdaptiveWriteZeroes(addr: int64; iCount: int64);inline;

    function IsAfterEOF(iPOs: int64): boolean;inline;
    procedure GRowFile(iSize: int64);override;
  end;

  TAdaptiveQueuedStream = class(TQueueStream)
  private
    FPosition: int64;

  public


    destructor Destroy;override;
    procedure AdaptiveRead(p: pbyte; iSize: int64);inline;
    procedure Detach; override;
    property Position: int64 read FPosition write FPosition;
    function Seek(iPos: int64; origin: TseekOrigin): int64;overload;inline;
    function Seek(iPOs: int64; origin: int64): int64;overload;inline;
    procedure AdaptiveWrite(p: Pbyte; iSize: int64);inline;
    function BeginAdaptiveRead(p: pbyte; iSize: int64; bForget: boolean = false): TReadCommand;inline;
    function EndAdaptiveRead(iOriginalsize: int64; qi: TReadCommand): int64;inline;
    procedure AdaptiveWriteZeroes(addr: int64; iCount: int64);inline;

    function IsAfterEOF(iPOs: int64): boolean;inline;
    procedure GRowFile(iSize: int64);
    constructor Create(const AStream: TStream; bTakeOwnership: boolean = true);reintroduce;virtual;
    procedure queue_onidle(sender: TObject);


  end;





  TAdvancedAdaptiveQueuedFileStream = class(TAdaptiveQueuedFileStream)
  public
    function CreateAndConfigureUnderclasses(sFile: string): TStream;override;
    function GetUnderClass: TStreamClass;override;
    procedure ConfigureUnderclass;override;

    destructor Destroy;override;
    procedure queue_onidle(sender: TObject);
    procedure Detach; override;
    procedure SetPRefetchHints(addr: int64; allowed: ni = -1);inline;
    function Warnings: ni;

  end;

  Tunbuffer = record
{$IFDEF PACKED_MASK}////
    FDirtyMask: array[0..(UNBUFFERSIZE-1) div 8 ] of byte;//ok
{$ELSE}
    FDirtyMask: array[0..UNBUFFERSIZE-1] of byte;//ok
{$ENDIF}
{$IFDEF DYN_BYTES}
    data: TDynByteArray;
{$ELSE}
    data: array[0..UNBUFFERSIZE-1] of byte;
{$ENDIF}
{$IFDEF DOUBLE_READ}
    data2, data3: array[0..UNBUFFERSIZE-1] of byte;
{$ENDIF}
    Fpagenumber: int64;
    wasfetched: boolean;
  strict private
  private
    FDirtyTime: ticker;
    lck: TCLXCriticalSection;
    DirtyCount: ni;
    ubs: TUnbufferedFileStream;
    ContainsSomeZeroes: boolean;
    procedure FirstInit;
    procedure Init;
    procedure Finalize;
    procedure ClearDirtyMask;
    function AllDirty: boolean;inline;
    function AnyDirty: boolean;inline;
    function ClusterIsDirty(iClusterOff: ni): TDirtiness;
    function ByteDirtyMask(iOFF: ni): byte;
    function ByteIsDirty(iOFF: ni): boolean;
    procedure SetDirty(offset, cnt: ni);overload;inline;
    procedure SetAllDirty;
    function SetDirty(offset: ni): boolean;overload;inline;


    procedure SetPageNumber(const Value: int64);
    procedure Needread;
    procedure DebugWAtch(bWriting: boolean; iPosition: int64; iByteCount:int64; bAlarm: boolean);inline;
    function PageStart: int64;inline;
    property Dirtytime: ticker read FDirtyTime;
    property PageNumber: int64 read FPageNumber write SetPageNumber;
    procedure Lock;inline;
    procedure Unlock;inline;
  end;
  PUnbuffer = ^TunBuffer;

  TUnbufferObj = class(TBetterObject)
  public
    buf: PUnbuffer;
  end;

  TUnBufferList = TDirectlyLinkedList<TUnbufferObj>;

  Tunbufferthread = class(TExternalEventThread);

  Tevent_UBFSCheckFLush = class(TPeriodicEvent)
  public
    procedure DoExecute;override;
  end;

  Tcommand_FlushStale = class(TCommand)
  private
    Fstr: TUnbufferedFileStream;
  public
    property UnbufferedStream: TUnbufferedFileStream read Fstr write Fstr;
    procedure DoExecute;override;
  end;

  TUnbufferedSideFetchThread = class(TManagedThread)
  protected
    procedure DoExecute; override;
  public
    ubs: TUnbufferedFileStream;
  end;

  TUBOp = class(TBetterObject)
  public
    ubs: TUnbufferedFileStream;
    autokill: boolean;
    procedure Doop;virtual;abstract;
  end;

  TUBWriteBehind = class(TUBOp)
  protected
    Fpb: pbyte;
    sz: ni;
    seekpos: int64;
  public
    procedure Assign(seekpos: int64; pb: Pbyte; sz: ni);
    destructor Destroy;override;
    procedure Doop;override;
  end;

  TUBWriteBehindZeros = class(TUBOp)
  strict
  private
    procedure SetSz(const Value: ni); protected
    Fsz: ni;
    seekpos: int64;
  public
    property sz: ni read Fsz write SetSz;
    destructor Destroy;override;
    procedure Doop;override;
  end;

  TUnbufferedFileStream = class(TFileStreamWithVirtualConstructors)
  const LASTPAGE = UNBUFFERED_BUFFERED_PARTS-1;
  strict private
    function ReadEOF(var iLength: int64): boolean;inline;
    procedure GuaranteeWriteUnder(pb: Pbyte; sz: ni);
  protected
    fetchqueue: TSimpleQueue;
    cmd: Tcommand_FlushStale;
    FBuffers: array[0..LASTPAGE] of TUnbuffer;
{$IFDEF USE_LINKED_BUFFERS}
    FBufferOrders: TUnBufferLIst;
    //todo 1: create buffers
    //todo 1: destroy buffers
    //todo 1: create buffer list
    //todo 1: destroy buffer list
    //todo 1: update the method for reordering buffers upon use
{$ELSE}
    FBufferOrders: array[0..LASTPAGE] of PUnbuffer;
{$ENDIF}
    aligned_temp: TAlignedTempSpace;
{$IFDEF DYN_BYTES}
    FTemp: TDynByteArray;
{$ELSE}
    FTemp: array[0..unbuffersize-1] of byte;
{$ENDIF}
    FFront_SeekPosition: int64;
    FBack_SeekPosition: int64;

    FSeekPage: int64;
    FReportedSize: int64;
    FSizeCommittedToBuffers: int64;

    rsFetchPage, rsFetchApplyMask, rsFlushPageBruteForce, rsIndividualRead: TRingStats;
    lckOp, lckTemp, lckUnder, lckBack, lckBuffers, lckState: TCLXCriticalSection;
{$IFDEF USE_LINKED_BUFFERS}
    procedure bringtofront(buf: TUnbufferObj);inline;
{$ELSE}
    procedure bringtofront(iBufNum: ni);inline;
{$ENDIF}
    function FindPage(iPage: int64): PUnbuffer;
    procedure FlushPageBruteForce(buf: PUnBuffer; iRecursions: ni=0);
    procedure FlushPage(buf: PUnBuffer; iRecursions: ni=0);inline;
    procedure FetchPage(const iPage: int64; const buf: PUnbuffer; const stage: TFetchBufferStage = fbInitial; alt_buf: PByte = nil);//
    procedure SeekPage(const iPage: int64; bForEof: boolean = false);//inline;//
    procedure WriteEOF();inline;
    procedure FetchPageAndApplyMask(buf: PUnbuffer);inline;//
    procedure OnUnbufferThreadxExecute(thr: Tmanagedthread);//
    function IndexOfPage(iPage: int64): ni;//
    procedure DebugPAges;
{$IFDEF USE_LINKED_BUFFERS}
    function FindPageObj(iPage: int64): TUnbufferObj;//
{$ENDIF}

    procedure CheckOrStartFlushCommand;
    procedure CReateUnBuffers;
    procedure DestroyUnbuffers;
  strict private
    allowed_prefetches: ni;
    function GEtPrefetchbytepos: int64;//
    procedure SetPrefetchBytePos(const Value: int64);
  private
    function GetPosition: int64;
    procedure SetPosition(const Value: int64);
    function GetFlexSeek: int64;
    procedure SetFlexSeek(const Value: int64);//
  protected
    scratch1, scratch2: array[0..511] of byte;
    writebuilder: TWriteBuilder;
    procedure SEtSize(const iLen: int64);override;//
    procedure SyncSetSize(const iLen: int64);
    function GetSize: int64;override;//
    function FindLowestWriteThrough: PUnbuffer;//
    function FindWriteThroughPage(iPageNumber: int64): PUnbuffer;//
    procedure OptimalFlush;//
    procedure FlushWithoutRead(buf: PUnbuffer);//
  public
    warnings: ni;
    opsIn, OpsPerformed: ni;
    sfthread: TUnbufferedSideFetchThread;
    upperqueue: TSimpleQueue;
    writefailures: int64;
    periodic: Tevent_UBFSCheckFLush;
//    rsWrite0, rsWrite1, rsWrite2, rsEOF, rsFlush: TRingStats;
    prefetchposition: int64;
    prefetchwritemode: boolean;
    hits, misses: int64;
    estimated_queue_size: ni;
    oplink: TUBOp;
    in_perform_op: boolean;
    opcount: ni;
    debugme: boolean;
    lastuse: ticker;
    rsPrefetchInterruption: TRingSTats;
    UseBackSeek: boolean;
    FREcommendedPrefetchAllowance: ni;
    UpStreamHints: PPerformancehints;
//    EnableDirtyDebug: boolean;
    //property FlexSeek: int64 read GetFlexSeek write SetFlexSeek;
    procedure CheckFlush;
    constructor Create(const AFileName: string; Mode: cardinal; Rights: Cardinal; Flags: cardinal);override;
    constructor Create(const AFileName: string; Mode: cardinal);override;
    procedure BeforeDestruction;override;
    function Read(var Buffer; Count: Longint): Longint; override;//
    function Write(const Buffer; Count: Longint): Longint; override;//
    function Seek(const offset: int64; Origin: TSeekOrigin): int64; override;//

    procedure GuaranteeSyncWrite(pb: Pbyte; count: ni; bZeroFlag: boolean = false);//
    procedure GuaranteeSyncread(pb: Pbyte; count: ni);//

    procedure SyncWriteZeros(sz: ni);//
    function SyncRead(var Buffer; Count: Longint): Longint;inline;//
    procedure EnterIdleState;
    procedure EnterActiveState;
    function SyncWrite(const Buffer; Count: Longint; bZeroFlag: boolean = false): Longint;inline;//
    function SyncSeek(const offset: int64; Origin: TSeekOrigin): int64;inline;//
    function FrontSeek(const offset: int64; Origin: TSeekOrigin): int64;inline;//

    procedure GrowFile(iNewSize: int64);//
    function IsAfterEOF(iPos: int64): boolean;inline;
    function GetBufferStatusString: string;//
    function front_eof: boolean;inline;//
    function back_eof: boolean;inline;//


    procedure FlushAll;//
    function SmartSideFetch(EXTQueue: TAbstractSimpleQueue): boolean;//
    property PrefetchBytePos: int64 read GEtPrefetchbytepos write SetPrefetchBytePos;//
    function PrePareAndLockPAge(iPAge: int64; bNeedREad: boolean; bForEof: boolean = false): TUnbufferobj;//
    function PerformOp: boolean;//
    procedure PerformAllOps; inline;//
    procedure PerformOps_FallingBehind; inline;//

    procedure WriteBehind(seekpos: int64; pb:PByte; sz: int64);//
    procedure WriteBehindZeros(seekpos: int64; sz: int64);//
    procedure AddOp(ol: TUbop);//
    procedure DebugOps;inline;//
    function RecommendedPrefetchAllowance: ni;//
    procedure CalculateNewRecommendedPrefetchAllowance;//
    procedure CancelPrefetches;//
    procedure StartPrefetches;//
    function PrefetchStep: ni; inline;//
    procedure SeekLock;inline;//
    procedure SeekUnlock;inline;//
    property POsition: int64 read GetPosition write SetPosition;//
    procedure LockBack(const sReason: string);inline;
    procedure UnlockBack;inline;
    function PercentBuffersFlushed: single;
    procedure Lock;
    procedure Unlock;
    function TryLock: boolean;
  end;

//var
//  g_alarm: boolean;
var
  G_ub_QUEUE_DEPTH :ni = 3000;
  g_FORCE_UB_PREFETCH : ni = 3;
  g_USE_OPTIMAL_FLUSH: boolean = false;
  FLUSH_TIME_LIMIT : nativeint = 23;
  balancer: TCLXCriticalSection;


{$ENDIF}

procedure Balance;


implementation

{$IFDEF MSWINDOWS}

uses
{$IFDEF BAD_USES}
  virtualdisk_advanced,
{$ENDIF}
  multibufferqueuestream, helpers_stream, stringx, multibuffermemoryfilestream;

{ TQueueStream }

procedure TQueueStream.BeforeDestruction;
begin
  if assigned(FQueue) then begin
    FQueue.onidle := nil;
    FQueue.WaitForAll;
    FQueue.Stop;
    FQueue.WaitForFinish;
    Fqueue.ProcessAllSynchronously;
  end;
  inherited;

end;

function TQueueStream.BeginRead(const addr: int64; const pointer: pbyte;
  const size: int64; bDisallowSynchronous: boolean; bForget: boolean): TReadCommand;
var
  rq: TReadCommand;
begin
//  if not EnableQueue then begin
//    FUnderStream.Seek(addr,soBeginning);
//    stream_guaranteeRead(FunderStream, @pointer[0], size);
//    result := nil;
//  end else begin

    rq := TReadCommand.create;
    rq.Stream := self;
    rq.Addr := addr;
    rq.Count := size;
    rq.Ptr := pointer;
{$IFDEF ALLOW_SYNCHRONOUS_READS}
    rq.allowSynchronous := not bDisallowSynchronous;
{$ENDIF}
    result := rq;
    result.AutoDestroy := bForget;
    FQueue.AddItem(rq);

//  end;
  FQueue.urgent := true;
{$IFDEF HOLD}  FQueue.Hold := false;{$ENDIF}

{$IFDEF CONTEXT_SWITCH_ON_QUEUE_ADD}
  sleep(0);
{$ENDIF}

end;


procedure TQueueStream.BeginWrite(const addr: int64; const pointer: pbyte;
  const size: int64);
var
  wq: TWriteCommand;
begin
  queue.maxitemsinqueue := G_UB_queUE_DEPTH;
//  if addr > FtrackedSize then
//    raise ECritical.create('cannot write to point beyond end of file without expanding');
//  if not EnableQueue then begin
//    FUnderStream.Seek(addr,soBeginning);
//    stream_guaranteeWrite(FunderStream, @pointer[0], size);
//    FTrackedSize := greaterof(addr +size, FTrackedSize);
//  end else begin
    wq := TWriteCommand.create;
    wq.Stream := self;
    wq.Addr := addr;
    wq.Count := size;
    wq.Ptr := pointer;
    wq.AutoDestroy := true;
    FQueue.AddItem(wq);
    FTrackedSize := GreaterOf(wq.Addr+wq.count, FTrackedSize);

//  end;
//  wq.WAitFor;
//  wq.Free;
{$IFDEF HOLD}  FQueue.Hold := false;{$ENDIF}
end;

procedure TQueueStream.BeginWriteZeros(const addr, size: int64);
var
  wzq: TWriteZeroesCommand;
  p: pbyte;
begin

  if size < 0 then
    exit;
//  if not EnableQueue then begin
//    FUnderStream.Seek(addr,soBeginning);
//    Stream_WriteZeros(FUnderstream, size);
//    FTrackedSize := greaterof(addr +size, FTrackedSize);
//  end else begin
    if size > 100000000 then begin
      Debug.Log(self, 'Large Zero write starts at ' +inttohex(addr,1)+' and goes for '+inttohex(size,1)+' bytes');
      wzq := TWriteZeroesCommand.create;
      wzq.Stream := self;
      wzq.Addr := addr;
      wzq.Count := size;
      wzq.AutoDestroy := true;
      FQueue.AddItem(wzq);
      FTrackedSize := GreaterOf(wzq.Addr+wzq.count, FTrackedSize);
{$IFDEF HOLD}      FQueue.Hold := false;{$ENDIF}
    end else begin
      //FQueue.Hold := true;//hold when writing zeros, because we (generally) assume that there will be data coming into the zero space.
      p := getmemory(size);
      fillmem(p,size,0);
      self.BeginWrite(addr,p,size);
      freememory(p);
    end;
//  end;

end;

procedure TQueueStream.CleanupUnderStream;
begin
  if assigned(FUnderStream) then begin
    if OwnsStream then begin
      Debug.Log(self, 'Cleaning up understream:'+FUnderstream.classname);
      FUnderSTream.free;
    end;
    FUnderStream := nil;
  end;



end;

destructor TQueueStream.Destroy;
begin
  Debug.Log(self, 'Destroying '+self.classname);
  if FQueue <> nil then begin
    FQueue.Stop;
    FQueue.WaitForFinish;

    TPM.NoNeedthread(FQueue);
    FQueue := nil;
  end;
  CleanupUnderstream;


  inherited;
end;

procedure TQueueStream.Detach;
begin

  inherited;

end;

procedure TQueueStream.EndCommand(qi: TQueueItem);
begin
  qi.WAitFor;
end;

function TQueueStream.EndRead(qi: TREadCommand): int64;
begin
  result := 0;
  if qi = nil then exit;
  qi.WAitFor;
  result := qi.result;
  qi.free;
end;

function TQueueStream.estimated_queue_size: ni;
begin
  result := FQueue.estimatedsize;
end;

procedure TQueueStream.FinalizeBuffers;
begin
  //
end;

function TQueueStream.GEtSize: int64;
begin
  result := FTrackedSize

end;

procedure TQueueStream.Init;
begin
  inherited;
  FQueue := TPM.NeedThread<TStreamQueue>(self);
  FQueue.MaxItemsInQueue := G_UB_QUEUE_DEPTH;
  FQueue.Name := FQueue.classname +' for '+classname;
  Fqueue.loop := true;
  FQueue.Start;
  FQueue.OnEmpty := self.OnQueueEmpty;
  FQueue.OnNotEmpty := self.OnQueueNotEmpty;

  EnableQueue := true;
  OwnsStream := true;

end;

function TQueueStream.IsReady: boolean;
begin
  result := true;
end;

procedure TQueueStream.OnQueueEmpty(sender: TObject);
begin
  if understream is TUnbufferedFileStream then
    TUnbufferedFileStream(understream).EnterIdleState;
end;

procedure TQueueStream.OnQueueNotEmpty(sender: TObject);
begin
  if understream is TUnbufferedFileStream then
    TUnbufferedFileStream(understream).EnterActiveState;
end;

procedure TQueueStream.ReadInitialSize;
begin
  FTrackedSize := FUnderStream.size;
end;

procedure TQueueStream.SetSize(const Value: int64);
var
  qi: TSetSize;
begin
  if value < FTrackedSize then
    Debug.ConsoleLog('stream size is decreasing from '+commaize(FTrackedSize)+' to '+commaize(value));
  if value > int64(18)*TRILLION then
    Debug.ConsoleLog('whoooaa bad');
  qi := TSetSize.create;
  qi.Stream := self;
  qi.NewSize := value;
  qi.AutoDestroy := true;
  FQueue.addItem(qi);

  FTrackedSize := value;


end;


procedure TQueueStream.SetUnderStream(const Value: TStream);
begin
  FUnderStream := Value;
  if assigned(value) then begin
    ReadInitialSize;
    if UnderStream is TFileStream then begin
      FQueue.Name := classname+' for '+TFilestream(Understream).FileName;
    end;
  end;
end;

function TQueueStream.TryEnd(qi: TQueueItem): boolean;
begin
  result := qi.Wait;
end;

{ TWriteCommand }

function TWriteCommand.DebugString: string;
begin
  result := inherited + ' '+inttohex(addr,2)+' cnt='+inttohex(count,0)+' starts='+memorytohex(self.Ptr, lesserof(64,self.Count));
end;

destructor TWriteCommand.Destroy;
begin
//  if Combined then
//    Debug.consolelog('Destroying COMBINED write command @'+inttohex(ni(pointer(self)), 8)+' ptr='+inttohex(ni(FPointer), 8));
  FreeMemory(FPOinter);
  FPOinter := nil;
  inherited;

end;

procedure TWriteCommand.DoExecute;
begin
  inherited;
  try
    if Addr < 0 then
      exit;

    if self.Count > 140000000 then begin
      Debug.Log(self,'huge write is '+inttostr(count)+' bytes');
    end;

{$IFDEF DETAILED_DEBUGGING}
  Debug.Log(self, 'write @'+inttohex(Addr,1));
{$ENDIF}

//    Debug.Log(self, debugstring);
    FStream.UnderStream.Seek(Addr, TSeekOrigin.soBeginning);
    Stream_GuaranteeWrite(FStream.UnderStream, self.Ptr, self.Count{, addr, 65536});
//    Debug.Log(self,'After write size is now '+inttohex(fStream.size,0));
  except
    on E: Exception do begin
      Debug.Log(self,E.Message+' when writing to addr 0x'+inttohex(addr, 0)+' size 0x'+inttohex(self.count,0));
      //raise;
    end;
  end;
end;

procedure TWriteCommand.GiveExternalPointer(p: pbyte; cnt: ni);
begin
  FOriginalPointer := p;
  FPointer := p;
  FCount := cnt;
end;

procedure TWriteCommand.MovePOinterData;
begin
  if (FOriginalPOinter <> nil) and (FCount > 0) then begin
    FPointer := GetMemory(FCount);
    movemem32(FPointer, FOriginalPOinter, FCount);
//    AlertMemoryPattern(@BAD_PATTERN[0], sizeof(BAD_PATTERN), pbyte(FPOinter), Fcount);
  end;
end;

procedure TWriteCommand.SetAddr(const Value: int64);
begin
  FAddr := Value;
//  if value > (int64(1) * 1000000000000) then
//    raise ECritical.create('Insane Value');

end;

procedure TWriteCommand.SetCount(const Value: int64);
begin
  FCount := Value;
  MovePOinterData;
end;

procedure TWriteCommand.SetPointer(const Value: pbyte);
begin
  FOriginalPOinter := Value;
  MovePOinterData;

end;

{ TQueuedFileSTream }


procedure TQueuedFileSTream.ConfigureUnderclass;
begin
  //
{$IFDEF TIME_CRITICAL}
  FQueue.betterpriority := bpHighest;
{$ENDIF}
end;

function TQueuedFileSTream.CreateAndConfigureUnderclasses(
  sFile: string): TStream;
begin
  result := nil;
  if not fileexists(FFileName) then begin
    UnderStream := TFileStreamClass(getunderclass).create(sfile, fmCReate, 0, Flags);
    result := understream;
    UnderStream.free;
    UnderStream := nil;
  end;


  //Create(const AFileName: string; Mode: cardinal; Rights: Cardinal; Flags: cardinal)
  underStream := TFileStreamClass(getunderclass).Create(sFile, self.OpenMode, cardinal(0), cardinal(Flags));
  Self.FQueue.Name := FQueue.classname+' for '+self.ClassNAme+' '+sFile;
  configureUnderclass;

end;

procedure TQueuedFileSTream.Flush;
begin
  //
end;

function TQueuedFileSTream.GetUnderClass: TStreamClass;
begin
{$IFDEF USE_UNBUFFERED}
  result := TUnbufferedFileStream;
{$ELSE}
  result := TFileStreamWithVirtualConstructors;
{$ENDIF}
end;

procedure TQueuedFileSTream.GrowFile(iSize: int64);
var
  wz: TWriteZeroesCommand;
  iFill: int64;
begin
  iFill := iSize - size;

  if iFill > 0 then begin
    wz := TWriteZeroesCommand.create;
    wz.Stream := self;
    wz.Addr := size;
    wz.Count := iFill;
    wz.AutoDestroy := true;
    FQueue.AddItem(wz);
  end;



end;

procedure TQueuedFileSTream.INit;
begin
  inherited;
  FOpenMode := fmOpenReadWrite
end;

procedure TQueuedFileSTream.SetFileName(const Value: string);
begin
  if value <> FFileName then begin
    CleanupUnderStream;

    FFileName := Value;
    if FFileName <> '' then begin
      CreateAndConfigureUnderclasses(value);

    end;
  end;


end;

{ TGetSize }


{ TReadCommand }

function TReadCommand.DebugString: string;
begin
  result := inherited + ' '+inttohex(addr,2)+' cnt='+inttohex(count,0);

end;

procedure TReadCommand.DoExecute;
var
  sf: TSideFetchCommand;
begin
  inherited;
//  debug.consolelog('read start');
  FStream.UnderStream.Seek(Addr, soBeginning);

  result := STream_GuaranteeRead(FStream.UnderStream, self.Ptr, self.Count);

{$IFDEF DETAILED_DEBUGGING}
//  Debug.Log(self, 'read '+memorytohex(self.Ptr, lesserof(64,self.Count)));
  Debug.Log(self, 'read @'+inttohex(Addr,1));
{$ENDIF}

//  debug.consolelog('read end');
  if FStream.UnderStream is TUnbufferedFileStream then begin
{$IFDEF ALLOW_SIDEFETCH_REQUEUE}
    sf := TSideFetchCommand.Create;
    sf.FStream := self.FStream;
    queue.SelfAdd(sf);
{$ENDIF}
    FStream.Queue.hits := TUnbufferedFileStream(FStream.UnderStream).hits;
    FStream.Queue.misses :=  TUnbufferedFileStream(FStream.UnderStream).misses;
  end;



end;

procedure TReadCommand.Init;
begin
  inherited;
{$IFDEF ALLOW_SYNCHRONOUS_READS}
  AllowSynchronous := true;
{$ENDIF}
end;

procedure TReadCommand.SEtAddr(const Value: int64);
begin
  FAddr := Value;
//  if value > (int64(1) * 1000000000000) then
//    raise ECritical.create('Insane Value');
end;

{ TAdaptiveQueuedFileStream }

procedure TAdaptiveQueuedFileStream.AdaptiveRead(p: pbyte; iSize: int64);
begin
  EndAdaptiveRead(BeginAdaptiveRead(p, iSize, false));
end;

procedure TAdaptiveQueuedFileStream.AdaptiveWrite(p: Pbyte; iSize: int64);
begin
  BeginWrite(POsition, p, iSize);
  Position := Position + iSize;
  PerformanceHints.pendingOps := Self.Queue.EstimatedSize;
end;

procedure TAdaptiveQueuedFileStream.AdaptiveWriteZeroes(addr, iCount: int64);
begin
  BeginWriteZeros(addr,icount);
  position := position + icount;
  PerformanceHints.pendingOps := Self.Queue.EstimatedSize;

end;

function TAdaptiveQueuedFileStream.BeginAdaptiveRead(p: pbyte;
  iSize: int64;bDisallowSynchronous: boolean; bForget: boolean = false): TReadCommand;
begin
  iSize := lesserof(iSize, size-Position);
  result := BeginRead(position, p, iSize, bDisallowSynchronous, bForget);
  Position := Position + iSize;
  PerformanceHints.pendingOps := Self.Queue.EstimatedSize;
end;

constructor TAdaptiveQueuedFileStream.Create(const AFileName: string;
  Mode: cardinal; Rights: cardinal; Flags: cardinal);
begin
  self.Flags := flags;
  Create(AfileName, Mode);

end;

constructor TAdaptiveQueuedFileStream.Create(const AFileName: string;
  Mode: cardinal);
begin
  inherited Create;
  self.OpenMode := mode;
  self.FileNAme := AFileName;

end;



function TAdaptiveQueuedFileStream.EndAdaptiveRead(qi: TReadCommand): int64;
begin
  qi.WaitFor;
  result := qi.result;
  qi.Free;
  PerformanceHints.pendingOps := Self.Queue.EstimatedSize;

end;

procedure TAdaptiveQueuedFileStream.GRowFile(iSize: int64);
begin
  BeginWriteZeros(size, iSize - size);

end;

function TAdaptiveQueuedFileStream.IsAfterEOF(iPOs: int64): boolean;
begin
  result := iPOs > Size;
end;


function TAdaptiveQueuedFileStream.Seek(iPos: int64;
  origin: TseekOrigin): int64;
begin
  case origin of
    soBeginning: Position := iPOs;
    soCurrent: POsition := Position + iPOs;
    soEnd: POsition := Size - iPos;
  end;

  result := Position;

end;

function TAdaptiveQueuedFileStream.Seek(iPOs, origin: int64): int64;
begin
  position := iPos + origin;
  result := position;
end;

{ TSetSize }

function TSetSize.DebugString: string;
begin
  result := inherited + ' '+inttohex(newsize, 0);
end;

procedure TSetSize.DoExecute;
var
  n,o: int64;
begin
  inherited;
  o := FStream.UnderStream.Size;
  n := NewSize;
  if n > o then
    Debug.ConsoleLog(('Growing to : '+commaize(n)));
  FStream.UnderStream.Size := NewSize;
end;

procedure TSetSize.Init;
begin
  inherited;
//  Debug.ConsoleLog('Creating TSetSize');
end;

{ TAdvancedAdaptiveQueuedFileStream }




{ TAdvancedAdaptiveQueuedFileStream }

procedure TAdvancedAdaptiveQueuedFileStream.ConfigureUnderclass;
begin
  inherited;
  if FUnderSTream is TMultiBufferQueueStream then begin
//    (Self.FUnderStream as TMultiBufferQueueStream).BufferSegments := 128*8;
//    (Self.FUnderStream as TMultiBufferQueueStream).BufferSize := 65536 * 1024*4;
  //TMultiBufferStream(Self.FUnderStream).DisableLookAhead := true;
//    (Self.FUnderStream as TMultiBufferQueueStream).DisableMinimumPrefetch := true;
  end;
{$IFDEF TIME_CRITICAL}
{$IFDEF MSWINDOWS}
  FQueue.betterpriority := bpHighest;
{$ENDIF}
{$ENDIF}
{$IFDEF IDLE_FETCH}
  FQueue.OnIdle := self.queue_OnIdle;
  FQueue.NoWorkRunInterval := 666;
//  FQueue.AutoMaintainIdleInterval := true;
{$ENDIF}

end;

function TAdvancedAdaptiveQueuedFileStream.CreateAndConfigureUnderclasses(
  sFile: string): TStream;
var
  ubs: TUnbufferedFileStream;
  mbs: TmultibufferQueueStream;
  aqs: TAdaptiveQueuedStream;
  mode: cardinal;
begin
{$IFDEF COMPLEX_STREAM}
  //TAdaptiveQueuedFileStream->TMultibufferQueueStream->TAdaptiveQueuedStream->TUnbufferedFileStream
  //we need to create two underclasses for thsi configuration;
  if (openmode=fmCReate) or (not fileexists(sFile)) then
    mode := fmCreate
  else
    mode := fmOpenReadWrite+fmShareExclusive;

  ubs:=Tunbufferedfilestream.Create(sfile, mode, 0,0);
  aqs := TAdaptiveQueuedStream.create(ubs, true);
  aqs.Queue.MaxItemsInQueue := G_UB_QUEUE_LENGTH;
  mbs := TmultibufferQueueStream.create(aqs,true);
//  mbs.AllowReadPastEOF := true;
  mbs.BufferSEgments := L1_CACHE_SEGMENTS;
  mbs.BufferSize := L1_TOTAL_CACHE;

  understream := mbs;
  result := mbs;
  configureunderclass;
{$ELSE}
  //TAdaptiveQueuedFileStream->TMultibufferStream->TAdaptiveQueuedStream->TUnbufferedFileStream
  //we need to create two underclasses for thsi configuration;
  if fileexists(sFile) then
    mode := fmOpenReadWrite+fmShareExclusive
  else
    mode := fmCreate;

  ubs:=Tunbufferedfilestream.Create(sfile, mode, 0,0);
  ubs.upperqueue := self.queue;
  ubs.upstreamHints := @self.PerformanceHints;
  understream := ubs;
  result := ubs;
  configureunderclass;

{$ENDIF}
end;

destructor TAdvancedAdaptiveQueuedFileStream.Destroy;
begin
  Debug.Log(self,'Destroying with understream '+FUnderstream.classname);
  inherited;
//  FUnderStream.free;
//  FUnderStream := nil;

end;

procedure TAdvancedAdaptiveQueuedFileStream.Detach;
begin
  if detached then exit;

  fqUEUE.OnIdle := nil;
//  FQueue.Stop;
//  FQueue.WaitForAll;
//  FQueue.WaitForFinish;

  inherited;


end;

function TAdvancedAdaptiveQueuedFileStream.GetUnderClass: TStreamClass;
begin
  result := Tmultibufferstream;

end;


procedure TAdvancedAdaptiveQueuedFileStream.queue_onidle(sender: TObject);
begin
  if FQueue.TryLock then
  try
    {$IFDEF COMPLEX_STREAM}
    if (FUnderStream as TMultiBufferQUeueStream).SmartSideFetch then
      inc(FQueue.sidefetches);
    {$ELSE}
    if (FUnderStream as TUnbufferedFileStream).SmartSideFetch(FQueue) then
      inc(FQueue.sidefetches);
    {$ENDIF}
  finally
    FQueue.Unlock;
  end;
end;

procedure TAdvancedAdaptiveQueuedFileStream.SetPRefetchHints(addr: int64;
  allowed: ni);
begin
  if not (FUnderstream is TMultiBufferQueueStream) then
    exit;

  TMultiBufferQueueStream(FUnderSTream).SetPRefetchHints(addr, allowed);
end;

function TAdvancedAdaptiveQueuedFileStream.Warnings: ni;
begin
  result := 0;
  if understream is TUnbufferedfileStream then
    result := TUnbufferedFileStream(understream).warnings;
end;

{ TWriteZeroesCommand }

function TWriteZeroesCommand.DebugString: string;
begin
  result := inherited + ' '+inttohex(addr,2)+' cnt='+inttohex(count,0);
end;

destructor TWriteZeroesCommand.Destroy;
begin

  inherited;

end;

procedure TWriteZeroesCommand.DoExecute;
var
  tm1, tm2, tmDif: ticker;
begin
  inherited;
  FStream.UnderStream.Seek(Addr, TSeekOrigin.soBeginning);

  tm1 := GetHighResTicker;
  Stream_WriteZeros(FStream.UnderStream, self.Count);
  tm2 := GetHighResTicker;
  tmDif := GetTimeSince(tm2, tm1);
  debug.log('wrote '+self.count.tostring+' zeroes in '+floatprecision(gettimesince(tm2,tm1)/10000,2)+'ms at a rate of '+commaize(round((self.count/(tmDif/10000000))))+'bytes/sec');
//  Debug.Log(self,'After write size is now '+inttohex(fStream.size,0));
end;


{ TStreamQueue }


procedure TStreamQueue.AfterUrgentCopy;
begin
  inherited;
  potentiallyHasReads := true;
end;

function TStreamQueue.CombineWrites(var a: TWriteCommand; b: TWriteCommand): boolean;
VAR
  c: TWriteCommand;
  e: int64;
  ca,cb: ni;
  aa,ab,ac: int64;
  oa,ob: int64;
  pa, pb,pc: pbyte;
//  idx: ni;
begin
  result := false;
//  if bidx >= FincomingItems.count then
//    raise ECritical.create('bidx is out of range');



{$IFDEF DO_NOT_COMBINE_WRITES}
  EXIT;
{$ENDIF}
  FIncomingItems.lock;
  try
  //  IF a.count > 150000 then
  //    exit;
//    Debug.ConsoleLog('---CombineWrites---');
//    Debug.ConsoleLog('a=@'+inttohex(ni(pointer(a)),8)+' addr='+inttohex(ni(pointer(a.addr)),8)+' ptr='+inttohex(ni(pointer(a.ptr)),8)+' count='+inttohex(ni(pointer(a.count)),8));
//    Debug.ConsoleLog('b=@'+inttohex(ni(pointer(b)),8)+' addr='+inttohex(ni(pointer(b.addr)),8)+' ptr='+inttohex(ni(pointer(b.ptr)),8)+' count='+inttohex(ni(pointer(b.count)),8));

//    idx := FIncomingItems.indexof(a);

    aa := a.addr;
    ab := b.addr;
    ca := a.Count;
    cb := b.count;
    //if a comes first then
    if aa < ab then begin
      //starting position is a
      ac := aa;
      //a offset is 0
      oa := 0;
      //b offset is whatever the difference is
      ob := ab-aa;
    end else begin
      //starting position is b
      ac := ab;
      //a offset is difference
      oa := aa-ab;
      //b offset is 0
      ob := 0;
    end;

    e := greaterof(aa+ca, ab+cb)-ac;

    if (e > MAX_QUEUE_COMBINE_SIZE) and (e > ca) and (e > cb) then begin
      exit;
    end;


    c := TWriteCommand.Create;
    c.Stream := a.stream;
    c.AutoDestroy := a.autodestroy;
    c.Combined := true;
    c.Queue := a.Queue;
    c.addr :=ac;
    //ending count is is the whatever the greater end is minuz the starting offset
    pc := system.getMemory(e);
//    Debug.ConsoleLog('pc='+inttohex(ni(pc),8));
    c.GiveExternalPointer(pc,e);

//    Debug.ConsoleLog('c=@'+inttohex(ni(pointer(c)),8)+' addr='+inttohex(ni(pointer(c.addr)),8)+' ptr='+inttohex(ni(c.ptr),8)+' count='+inttohex(ni(pointer(c.count)),8));

    pa := a.ptr;
    pb := b.ptr;

//    Debug.ConsoleLog('move1 off='+inttohex(oa,1)+' ca='+inttohex(ca,1));
//    Debug.ConsoleLog('move2 off='+inttohex(ob,1)+' cb='+inttohex(cb,1));

    movemem32(@pc[oa], pa, ca);
    movemem32(@pc[ob], pb, cb);

    c.Addr := lesserof(aa,ab);

  //  FincomingItems[idx] := c;//switch a with c
    FIncomingItems.Replace(a,c);
    //FincomingItems[aidx] := c;

//    if bidx >= FincomingItems.count then
//      raise ECritical.create('bidx is out of range');
//
//    if FIncomingItems.Items[bidx] <> b then
//      raise ECritical.create('b is not at expected index');

    FIncomingItems.Remove(b);
//    FincomingItems.Insert(idx,c);

    a.free;
    b.free;
    result := true;
  finally
    FIncomingItems.unlock;
  end;

end;

{$IFDEF READ_BEFORE_WRITE}
function TStreamQueue.GetNextItem: TQueueItem;
var
  t: ni;
  itm: TReadCommand;
  wi: TQueueItem;
  u: ni;
  bGood, bThisGood: boolean;
  a,b,c,d: int64;
begin
  result := nil;
  //inherited;/// <<--- do not call

  //stream queue order processing rules

  //read operations get priority,
  //...make a list of read operations
  bGood := false;
  itm := nil;
  if potentiallyHasReads then begin
  itm := GetRead;

    if itm = nil then begin
      potentiallyHasReads := false;
    end else begin
    //for each read operation, check that it does not overlap any write operations
    bGood := true;
    for u := 0 to FWorkingItems.count-1 do begin
      wi := fWorkingItems[u];
      if wi = itm then break;

      if wi is TWriteCommand then begin
        a := TWriteCommand(wi).Addr;
        b := TWriteCommand(wi).Count;
        c := itm.Addr;
        d := itm.Count;
        bThisGood := ((a+b) <= c) or (a >= (c+d));
        if not bThisGood then begin
          bgood := false;
          break;
        end;

      end else
      if wi is TWriteZeroesCommand then begin
          a := TWriteZeroesCommand(wi).Addr;
          b := TWriteZeroesCommand(wi).Count;
          c := itm.Addr;
          d := itm.Count;
          bThisGood := ((a+b) <= c) or (a >= (c+d));
          if not bThisGood then begin
            bgood := false;
            break;
          end;
      end;
    end;
  end;
  end;

  if not bGood then
    result := inherited
  else
    result := itm;

{$IFDEF LOG_ITEMS}
  Debug.Log(self,'got '+result.debugstring);
{$ENDIF}
end;
{$ENDIF}

function TStreamQueue.GetRead: TreadCommand;
//this function fills the read_array with a list of reads
var
  t: ni;
begin
  result := nil;
  for t:= 0 to FWorkingItems.count-1 do begin
    if FWorkingItems[t] is TReadCommand then begin
      exit(TReadCommand(FWorkingItems[t]));
    end;
  end;

end;

procedure TStreamQueue.Init;
begin
  inherited;

end;

procedure TStreamQueue.OptimizeIncoming(incomingitem: TQueueItem);
var
  t: ni;
  itm: TQueueItem;
  w: TWriteCommand;

begin
  inherited;
{$IFDEF DO_NOT_COMBINE_WRITES}
  exit;
{$ENDIF}


  itm := incomingitem;
  if itm is TWriteCommand then begin
    w := itm as TWriteCommand;
    OptimizeSingle(w);
  end;

end;

procedure TStreamQueue.OptimizeSingle(var qi: TWriteCommand);
var
  t: fi;
  itm: TQueueItem;
  w: TWriteCommand;
  a,b,c,d: int64;
  col: TCollision1D;
  bRetry, bDid: boolean;

begin
{$IFDEF DO_NOT_COMBINE_WRITES}
  exit;
{$ENDIF}
//  FIncomingItems.Lock;
//  try
    bRetry := false;
    repeat
      bRetry := false;
      a := qi.Addr;
      b := qi.Count;

      for t := FIncomingItems.count-1 downto 0 do begin
        if t >= FIncomingItems.count then
          continue;

        itm := FIncomingItems[t];
        if itm = qi then continue;
        if itm is TWriteCommand then begin
          w := itm as TWriteCommand;
          c := w.Addr;
          d := w.Count;
          col := collision.TestCollision1D(a,b,c,d);
          if  col <> cNoCollision then begin
            bDid := CombineWrites(qi,w);
            if not bDid then begin
              break;
              bRetry := false;
            end;
            //check if the size of the incoming changed and decrement qidx if it is higher
            //than t
//            if qidx > t then begin
//              dec(qidx);
//            end;
            itm := FIncomingItems[FincomingItems.count-1];
            if itm is TWriteCommand then begin
              qi := itm as TWriteCommand;
              bRetry := bDid;
            end;
          end;
        end else begin
          break;
          bRetry := false;
        end;
      end;
    until bRetry = false;
//  finally
//    FincomingItems.Unlock;
//  end;

end;

{ TUnbufferedFileStream }

function Tunbuffer.AnyDirty: boolean;
begin
  result := DirtyCount > 0;
end;

function Tunbuffer.ByteDirtyMask(iOFF: ni): byte;
{$IFDEF PACKED_MASK}////
begin
  result := (FDirtyMask[iOff shr 3] shr (iOff and 7)) and 1;//ok
  if result <> 0 then
    result := $ff;
end;
{$ELSE}
begin
  result := FDirtyMask[iOff];
end;
{$ENDIF}

function Tunbuffer.ByteIsDirty(iOFF: ni): boolean;
{$IFDEF PACKED_MASK}////
var
  bo,br: ni;
begin
  bo := iOff shr 3;
  br := iOff and 7;
  result := 0 = ((FDirtyMask[br] shr br) and 1);//ok
end;
{$ELSE}
begin
  result := FDirtyMask[iOff] = 0;
end;
{$ENDIF}

procedure TUnbuffer.ClearDirtyMask;
begin
  if DirtyCount = 0 then
    exit;
  fillmem(@Fdirtymask[0],sizeof(Fdirtymask),$ff);//ok
  DirtyCount := 0;

end;

function Tunbuffer.ClusterIsDirty(iClusterOff: ni): TDirtiness;
var
  bAny, bAll: boolean;
  cx: ni;
  cnt: ni;
begin
  cx := 512;
  cnt := 0;
  while cx > 0 do begin
    if not byteisdirty(iClusterOff) then
      inc(cnt);
    inc(iClusterOff);
    dec(cx);
  end;

  case cnt of
    0: exit(dirtAll);
    512: exit(dirtClean);
  else
    exit(dirtSome);
  end;

end;

procedure Tunbuffer.DebugWAtch(bWriting: boolean; iPosition: int64; iByteCount:int64; bAlarm: boolean);
var
  s: string;
//const
  //watchpoint = sizeof(TVirtualDiskPayloadFileHeader)+sizeof(TVirtualDiskBigBlockHeader)+(1758*512)+(16*(1758 div 128));
begin
{
  if self.pagenumber <> (WATCHPOINT shr unbuffershift) then exit;
  if bAlarm or ((iPOsition <= watchpoint) and ((iPosition+iByteCount) >= watchpoint)) then begin
    s := 'Watch: '+inttohex(watchpoint,0)+' On '+booltostrex(bWriting, 'writing', 'reading')+' watch is '+memorydebugstring(@data[(watchpoint) and UNBUFFERMASK], 512);
    if data[((watchpoint) and UNBUFFERMASK)+16] = $ff then
      g_alarm := true;

//    Debug.log(booltostrex(bAlarm, '***********','')+s);
  end;}


end;

procedure Tunbuffer.Finalize;
begin
  pagenumber := -1;
  DCS(lck);
end;

procedure Tunbuffer.FirstInit;
begin
  ICSSC(lcK, STREAM_SPIN);
end;


procedure TUnbufferedFileStream.CalculateNewRecommendedPrefetchAllowance;
var
  rec, mx: ni;
  r: single;
begin
{$IFDEF MINIMAL_UB_PREFETCH}
  FRecommendedPrefetchAllowance := 2;//2 = 250MB
  exit;
{$ENDIF}

  mx := FREcommendedPrefetchAllowance;
  r := rsPrefetchInterruption.PEriodicAverage/greaterof(mx,1);
  if (r) > 0.50 then
    inc(mx)
  else
    dec(mx);


  mx := greaterof(1, lesserof(UNBUFFERED_BUFFERED_PARTS shr 2, mx));

  FREcommendedPrefetchAllowance := mx;


end;

procedure TUnbufferedFileStream.CancelPrefetches;
var
  was: ni;
begin
  was := greaterof(0,allowed_prefetches);
  allowed_prefetches := 0;
  //sfthread.waitforidle;//protected by lckOp so.... probably don't need to wait for this
  rsPrefetchInterruption.AddStat(greaterof(0,RecommendedPrefetchAllowance-was));
  if rsPrefetchInterruption.NewBatch then
    CalculateNewRecommendedPrefetchAllowance;
end;

procedure TUnbufferedFileStream.CheckFlush;
var
  t: ni;
  buf: PUnbuffer;
  tm: ticker;
  bRepeat: boolean;
//  pct: single;
  workingMaxdirtyTime: ni;
begin
  if GEtTimeSince(lastuse) < FLUSH_TIME_LIMIT then
    exit;
  bRepeat := true;
  tm := 0;
  tm := GetTicker;
  while bRepeat do begin
    bRepeat := false;
    if TECS(self.lckBuffers) then
    try
//      pct := PercentBuffersFlushed;
//      pct := 1.0-greaterof((pct * 2) -1, 0.0);
//      workingMaxDirtyTime := greaterof(100,round(10000 * pct));
      for t:= 0 to high(Fbuffers) do begin
        buf := @FBuffers[t];//todo 1: use Fbufferorders
        if TECS(buf.lck) then
        try
          if (buf.AnyDirty and (gettimesince(buf.Dirtytime) > 100)) or buf.AllDirty then begin
            if not buf.ContainsSomeZeroes then
              FlushPage(buf);
            if GetTimeSince(tm)>FLUSH_TIME_LIMIT then begin
              break;
            end;

          end;
        finally
          LCS(buf.lck);
        end;

      end;
    finally
      LCS(self.lckBuffers);
    end;
  end;
//  Debug.Log(self, 'check flush took '+inttostr(gettimesince(tm)));

end;

procedure TUnbufferedFileStream.CheckOrStartFlushCommand;
var
  c: Tcommand_FlushStale;
begin
  if TECS(lckState) then
  try
    c := cmd;
    if assigned(c) then begin
//      Debug.Log(self,'Checking ic c.IsComplete');
      if c.IsComplete then begin
//        Debug.Log(self,'c.IsComplete, waiting for');
        c.WaitFor;
//        Debug.Log(self,'c is done');
        c.free;
//        Debug.Log(self,'c is free');
        c := nil;
        cmd := nil;
      end;
    end else begin
//      Debug.Log(self,'creating new command');
      c := Tcommand_FlushStale.create;
      c.UnbufferedStream := self;
      cmd := c;
      c.Start;
    end;
  finally
    LCS(lckState);
  end;

end;

constructor TUnbufferedFileStream.Create(const AFileName: string;
  Mode: cardinal);
begin
  CReate(AFilename, Mode, 0, 0);
end;


procedure TUnbufferedFileStream.CReateUnBuffers;
var
  t: ni;
  obj: TUnbufferobj;
  FProcs: array of TAnonymousIteratorQI;
begin
  {$IFDEF USE_LINKED_BUFFERS}
  FBufferORders := TDirectlyLinkedList<TUnbufferObj>.create;
  for t:= 0 to high(FBuffers) do begin
    FBuffers[t].FirstInit;
  end;
  SetLength(FProcs, length(FBuffers));

  for t:= 0 to high(FBuffers) do begin
    FProcs[t] := InlineIteratorProcQI(t, procedure (idx: ni) begin
//        Debug.Log('init buffer '+inttostr(idx));
        FBuffers[idx].Init;
        end);
  end;
  for t:= 0 to high(FBuffers) do begin
    FProcs[t].WaitFor;
    FProcs[t].Free;
  end;
  for t:= 0 to high(FBuffers) do begin
    FBuffers[t].ubs := self;
    obj := TUnbufferObj.create;
    obj.buf := @Fbuffers[t];
    obj.buf.dirtycount := -1;
    //obj.buf.ClearDirtyMask;
    FBufferOrders.Add(obj);
  end;
  {$ELSE}
  for t:= 0 to high(FBuffers) do begin
    FBuffers[t].FirstInit;
    FBuffers[t].Init;
    FBufferOrders[t] := @FBuffers[t];
  end;

  {$ENDIF}


end;

procedure TUnbufferedFileStream.DebugOps;
begin
  //  Debug.Log('OpsIn: '+opsin.ToString+' OpsPerformed:'+opsPerformed.ToSTring);
end;

procedure TUnbufferedFileStream.DebugPAges;
var
  t: ni;
  s: string;
begin
  s := '';
  for t:= 0 to high(FBuffers) do begin
    s := s + '['+FBufferOrders[t].buf.Fpagenumber.tostring+']';
  end;

  Debug.Log(s);
end;

procedure TUnbufferedFileStream.DestroyUnbuffers;
var
  t: ni;
  obj: TUnbufferObj;
begin
  {$IFDEF USE_LINKED_BUFFERS}
  for t:= low(FBuffers) to high(FBuffers) do begin
    FlushPage(@FBuffers[t]);
    FBuffers[t].Finalize;
  end;
  while FBufferOrders.First <> nil do begin
    obj := FBufferOrders.First;
    if obj <> nil then begin
      FBufferOrders.Remove(obj);
      obj.Free;
      obj := nil;
    end;
  end;
  {$ELSE}
  //flush and finalize all the buffers
  for t:= low(FBuffers) to high(FBuffers) do begin
    FlushPage(@FBuffers[t]);
    FBuffers[t].Finalize;
  end;
  {$ENDIF}

  FBufferORders.free;
end;

procedure TUnbufferedFileStream.EnterActiveState;
begin
  CancelPrefetches;
end;

procedure TUnbufferedFileStream.EnterIdleState;
begin
    allowed_prefetches := g_FORCE_UB_PREFETCH;
    FRecommendedPrefetchAllowance := g_FORCE_UB_PREFETCH;
    sfthread.stepcount := allowed_prefetches;
    sfthread.runhot := true;
    sfthread.haswork := true;
end;

constructor TUnbufferedFileStream.Create(const AFileName: string; Mode: cardinal; Rights: Cardinal; Flags: cardinal);
begin
  inherited Create(AfileName, Mode, Rights, Flags{$IFDEF ALLOW_UNBUFFERED_FLAG} or FILE_FLAG_NO_BUFFERING{$ENDIF});
//  rsWrite0 := TRingStats.create;
//  rsWrite1 := TRingStats.create;
//  rsWrite2 := TRingStats.create;
//  rsEOF := TRingSTats.create;
//  rsFlush := TRingStats.create;
{$IFDEF DYN_BYTES}
  setlength(FTemp, UNBuFFERSIZE);
{$ENDIF}

  aligned_temp.MEMSIZE := UNBUFFERSIZE;
  aligned_temp.Allocate;
  rsIndividualRead := TRingStats.create;
  rsFetchPage := TRingSTats.create;
  rsFetchApplyMask := TRingStats.create;
  rsFlushPageBruteForce := TRingStats.create;
  rsPrefetchInterruption := TRingStats.create;
  rsPrefetchInterruption.Size := 64;
  writeBuilder := TWriteBuilder.create;
  ICS(lckTemp);
  ICS(lckUnder);
  ICS(lckOp);
  ICS(lckBack);
  ICS(lckState);
  ICS(lckBuffers);
  FSeekPage := -1;
  FFront_SeekPosition := 0;
  FBack_SeekPOsition := 0;
  FReportedSize := -1;
  FSizeCommittedToBuffers := -1;

  var tmStart := GetTicker;
  CReateUnBuffers;
  Debug.Log('Created unbuffers in '+gettimesince(tmStart).tostring+'ms.');

  if (Mode and fmCreate) = 0 then
    GetSize;

  //create a periodic event for checking the flush state
  periodic := Tevent_UBFSCheckFLush.create;
  periodic.owner := self;
  periodic.Frequency := FLUSH_TIME_LIMIT;
  periodic.enabled := true;
  //add it to the global periodic event aggregator
  PEA.Add(periodic);

  sfthread := TPM.NeedThread<TUnbufferedSideFetchThread>(nil);
{$IFDEF GWATCH}
  if lowercase(Afilename)='h:\2017.vdpayload' then
    GWatchThread := sfthread.threadid;
{$ENDIF}
  sfthread.name := 'ubs sfthread '+self.filename;
  sfthread.ubs := self;

  sfthread.HasWork := false;
  sfthread.RunHot := true;
  sfthread.ColdRunInterval := 1;
{$IFDEF TIME_CRITICAL}
  sfthread.betterPriority := bpHighest;
{$ENDIF}
{$IFDEF LOW_PRIORITY_SIDE_FETCH}
  sfthread.Priority := bpLowest;
{$ENDIF}
//  sfthread.loop := true;
{$IFDEF GWATCH}
  if lowercase(sfthread.ubs.filename)='h:\2017.vdpayload' then
    Debug.Log('trap!');
{$ENDIF}
  sfthread.start;
{$IFDEF GWATCH}
  if lowercase(sfthread.ubs.filename)='h:\2017.vdpayload' then
    Debug.Log('trap!');
{$ENDIF}

end;


function TUnbufferedFileStream.front_eof: boolean;
begin
  ecs(lckSTate);
  result := FFront_SeekPosition >= FReportedSize;
  lcs(lckState);
end;

function TUnbufferedFileStream.PercentBuffersFlushed: single;
var
  t: ni;
  bad: ni;
  hi: ni;
begin
  ecs(lckBuffers);
  try
    bad := 0;
    hi := high(FBuffers);
    for t:= 0 to hi do begin
//      ecs(FBuffers[t].lck);
      if FBuffers[t].AnyDirty then
        inc(bad);
//      lcs(FBuffers[t].lck);
    end;
    result := 1.0-(bad / hi);
  finally
    lcs(lckBuffers);
  end;
end;

function TUnbufferedFileStream.back_eof: boolean;
begin
  result := FBack_SeekPosition >= FSizeCommittedToBuffers;
end;


function TUnbuffer.AllDirty: boolean;
begin
  result := DirtyCount = UNBUFFERSIZE;
end;


procedure TUnbufferedFileStream.AddOp(ol: TUbop);
var
  oo: Tubop;
begin
  ecs(lckState);
  PerformOps_FallingBehind;

  ecs(lckOp);
  if oplink = nil then
    oplink := ol
  else begin
    oo := oplink;
    while oo.Next <> nil do begin
      oo := TUBOp(oo.next);
    end;
    oo.Next := ol;
  end;
  inc(opsIn);
  DebugOps;
  sfthread.runhot := true;
  sfthread.HasWork := true;
  inc(opcount);
  lcs(lckOp);
  lcs(lckState);
end;



procedure TUnbufferedFileStream.BeforeDestruction;
begin
  sfthread.Stop;
  SFTHREAD.HasWork := true;
  sfthread.waitfor;

  PErformAllOps;

  TPM.NoNeedthread(sfthread);
  sfthread := nil;
  //remove the periodic event from the periodic event aggregator
  if periodic <> nil then begin
    PEA.Remove(periodic);
    periodic.Free;
    periodic := nil;
  end;

//  if FReportedSize > 0  then
//    Debug.Consolelog('Kill me '+FileName);
  //make sure any running command is finished
  if assigned(cmd) then begin
    cmd.WaitFor;
    cmd.free;
    cmd := nil;
  end;
  PErformAllOps;
  DestroyUnbuffers;



  DCS(lckUnder);
  DCS(lckstate);
  DCS(lckOp);
  DCS(lckBuffers);
  DCS(lckTemp);
  DCS(lckBack);

  rsIndividualRead.Free;
  rsIndividualRead := nil;
  rsFetchPage.free;
  rsFlushPageBruteForce.free;
  rsFetchApplyMask.free;
  rsPrefetchInterruption.free;
  rsFetchPage := nil;
  rsFEtchApplyMask := nil;
  rsFlushPageBruteForce := nil;
  rsPrefetchInterruption := nil;
  writebuilder.free;
  WriteBuilder := nil;
//  rsWrite0.free;
//  rsWrite1.free;
//  rsWrite2.free;
//  rsEof.Free;
//  rsFlush.free;

  aligned_temp.Unallocate;
  inherited;

end;


{$IFDEF USE_LINKED_BUFFERS}
procedure TUnbufferedFileStream.bringtofront(buf: TUnbufferObj);
var
  tmp: TUnbufferObj;
begin
  FBufferOrders.remove(buf);
  FBufferOrders.AddFirst(buf);


end;
{$ELSE}
procedure TUnbufferedFileStream.bringtofront(iBufNum: ni);
var
  tmp: PUnbuffer;
  t: ni;
begin

  tmp := FbufferOrders[iBufNum];
  for t:= iBufNum downto 1 do begin
    FBufferorders[t] := FBufferOrders[t-1];
  end;
  FBufferOrders[0] := tmp;

end;
{$ENDIF}

procedure TUnbufferedFileStream.FetchPage(const iPage: int64; const buf: PUnbuffer; const stage: TFetchBufferStage = fbInitial; alt_buf: PByte = nil);
var
  iJustRead, iTotalToRead, iTogo: int64;
  p: Pbyte;
  iCnt: ni;
  iUnderPos: int64;
  iUnderPos2: int64;
  iUnderSize: int64;
  iBeyond: int64;
  iDif: ni;
  iSz: ni;
begin
{$IFDEF BALANCE}
  Balance;
{$ENDIF}
//  rsFetchPage.BeginTIme;
//  if g_traphit then
//    debug.Log('here');
  ECS(buf.lck);
  ecs(lckUnder);
  try
//    if iPage > HighestREportedPage then begin
//      fillmem(@buf.data[0], sizeof(buf.data), 0);
//      exit;
//    end;

{$IFDEF CHECK_SIZE_BEFORE_READ}
    iUnderSize := FileSeekPx(FHandle, int64(0), int64(ord(soEnd)));
    if iUndersize = -1 then begin
      Debug.Log(filename+' unable to determine file size error '+inttostr(GetlastError));
    end;
{$ENDIF}

    iUnderPos := iPage*UNBUFFERSIZE;
    iUnderPos2 := FileSeekPx(FHandle, iUnderPos, int64(soBeginning));//ok
//    iUnderPos :=  FileSeekPx(FHandle, 0, int64(soCurrent));//ok

    if alt_buf = nil then begin
      case stage of
        fbInitial: p := @buf.data[0];
  {$IFDEF DOUBLE_READ}
        fbComparative: p := @buf.data2[0];
        fbRestorative: p := @buf.data3[0];
  {$ENDIF}
      else
        p := @buf.data[0];
      end;
    end else
      p := alt_buf;


    iTotalToRead := sizeof(buf.data);

{$IFDEF CHECK_SIZE_BEFORE_READ}
    if iUnderPos2 >= iUnderSize then begin
{$ELSE}
    if iUnderPos2 <> iUnderPos then begin
{$ENDIF}
      fillmem(p, iTotalToRead, 0);
      buf.wasfetched := true;
      exit;
    end;


    //iRead := 0;
    iTogo := iTotalToRead;

    iCnt := 0;
    while iTogo > 0 do begin
//      rsIndividualRead.BeginTime;
      iJustRead := FileReadPx(@self.aligned_temp,FHandle, p^, iTogo);//ok
//      rsIndividualRead.EndTime;
//      if rsIndividualRead.NewBAtch then
//        rsIndividualRead.OptionDebug('IRTime for '+self.filename);
      if iJustREad <= 0 then begin
{$IFDEF CHECK_SIZE_BEFORE_READ}
        Debug.Log(self.filename+' FileRead returned '+inttostr(iJustRead)+' beyond end:'+inttostr(iUnderPos-iUnderSize)+' after count:'+inttostr(iCnt)+' error:'+inttostr(getlasterror));
{$ELSE}
        Debug.Log(self.filename+' FileRead returned '+inttostr(iJustRead));
{$ENDIF}
        break;
      end;
//      inc(iRead, iJustRead);
      inc(p, iJustRead);
      dec(iTogo, iJustREad);
      inc(iCnt);
      inc(iUnderPos, iJustRead);
{$IFDEF CHECK_SIZE_BEFORE_READ}
      if(iUnderPos = iUnderSize) then
        break;
{$ENDIF}
    end;
//        if iPAge = 0 then
//          if (p[81+600] <> 1) then
//            Debug.Log('AHA!');
    buf.wasfetched := true;
  finally
    LCS(buf.lck);
    lcs(lckUnder)
  end;

{$IFDEF DOUBLE_READ}
  xxx
  if stage = fbInitial then begin
//    sleep(random(10));
    FetchPage(iPage, buf, fbComparative);

    //CompareMemEx(const p1,p2: Pbyte; iCompareSize: ni; out pDifferent: ni; out iDifferentSize: ni): boolean;
    if not CompareMemEx(@buf.data[0], @buf.data2[0], sizeof(buf.data), iDif, iSz) then begin
      Debug.Log(self, 'MEMORIES IS DIFFERENT! @'+inttohex(iDif,0));
      FetchPage(iPage, buf, fbRestorative);

      if CompareMemEx(@buf.data3[0], @buf.data2[0], sizeof(buf.data), iDif, iSz) then begin
        Debug.Log('Reconcile with Data3!');
        movemem32(@buf.data2[0], @buf.data[0], sizeof(buf.data));
      end else
      if CompareMemEx(@buf.data3[0], @buf.data[0], sizeof(buf.data), iDif, iSz) then begin
        Debug.Log('Reconcile with Data0!');
  //      movemem32(@buf.data2[0], @buf.data[0], sizeof(buf.data));
      end else begin
        Debug.Log('UNREAL! No data reconciliation possible!');
      end;
    end;





  end;
{$ENDIF}
//  rsFetchPage.EndTime;
//  if rsFetchPage.NewBAtch then begin
//    rsFetchPage.OptionDebug('FPTime for '+self.FileName);
//    if assigned(upperqueue) then
//      upperqueue.NoWorkRunInterval := greaterof(1,round(rsFetchPage.PeriodicAverage/10000));
//  end;

end;

procedure TUnbufferedFileStream.FetchPageAndApplyMask(buf: PUnbuffer);
var
  dyn: TDynByteArray;
begin
  rsFetchApplyMask.BeginTime;
  with buf^ do begin
    ECS(lck);
    try
      if Anydirty then begin
        ecs(lckTEmp);
        try
          //movemem32(@ftemp[0],@data[0],sizeof(data));
          fetchpage(buf.pagenumber, buf, fbInitial, @FTemp[0]);

{$IFDEF PACKED_MASK}////
          MoveMem32WithMaskPacked_NOT(@data[0], @Ftemp[0], @fdirtymask[0], 0 , sizeof(data));//ok
{$ELSE}
          MoveMem32WithMask(@data[0], @Ftemp[0], @fdirtymask[0], sizeof(data));
{$ENDIF}
        finally
          lcs(lckTEmp);
        end;
      end else begin
        fetchpage(buf.pagenumber, buf);
      end;
    finally
      LCS(lck);
    end;
  end;
  rsFetchApplyMask.EndTime;
  rsFetchApplyMask.OptionDebug('FPMAskTime for '+self.FileName);
//  Cleardirtymask;
end;

function TUnbufferedFileStream.FindLowestWriteThrough: PUnbuffer;
var
  t: ni;
  cur, min: PUnbuffer;
begin
  ecs(lckBuffers);
  try
    min := nil;
    for t:= 1 to high(FBuffers) do begin
      cur := @FBuffers[t];
      if ((not cur.ContainsSomeZeroes) and cur.AnyDirty and cur.wasfetched) or cur.AllDirty then begin
        if (min = nil) or (cur.pagenumber < min.pagenumber) then
          min := cur;
      end;
    end;
    exit(min);
  finally
    lcs(lckBuffers);
  end;

end;

function TUnbufferedFileStream.FindPage(iPage: int64): PUnbuffer;
var
  t: ni;
  b: PUnbuffer;
  bo: TUnbufferObj;
begin
  result := nil;
  if iPage < 0 then exit;
{$IFDEF USE_LINKED_BUFFERS}
  bo := FBufferOrders.last;
  while bo <> nil do begin
    b := bo.buf;
    if b.pagenumber = iPage then
      exit(b);
    bo := TUnbufferObj(bo.Prev);
  end;
{$ELSE}
  for t:= high(Fbuffers) downto 0 do begin
    b := FBufferOrders[t];
    if b.pagenumber = iPage then begin
      result := FBufferorders[t];
      exit;
    end;
  end;

{$ENDIF}
end;

{$IFDEF USE_LINKED_BUFFERS}
function TUnbufferedFileStream.FindPageObj(iPage: int64): TUnbufferObj;
var
  t: ni;
  b: PUnbuffer;
  bo: TUnbufferObj;
begin
  ecs(LckBuffers);
  try
    result := nil;
    if iPage < 0 then exit;
    bo := FBufferOrders.Last;
    while bo <> nil do begin
      b := bo.buf;
      if b.pagenumber = iPage then begin
        result := bo;
        exit;
      end;
      bo := TUnbufferObj(bo.prev);
      if bo = nil then
        exit(nil);
    end;
  finally
    lcs(lckBuffers);
  end;
end;
{$ENDIF}

procedure TUnbufferedFileStream.FlushAll;
var
  t: ni;
begin
  ecs(lckBuffers);
  try
    for t:= 0 to high(FBuffers) do begin
      ecs(FBuffers[t].lck);
      FlushPage(@FBuffers[t]);
      lcs(FBuffers[t].lck);
    end;
  finally
    lcs(lckBuffers);
  end;


end;


procedure TUnbufferedFileStream.FlushPage(buf: PUnBuffer; iRecursions: ni);
begin
{$IFDEF BALANCE}
  Balance;
{$ENDIF}
//  if buf.PageNumber = 6 then
//      Debug.Log('page 6');
{$ifdef USE_SPOT_FLUSH}
  if buf.AnyDirty then
    FlushWithoutRead(buf);
{$ELSE}
  FlushPageBruteForce(buf, 0);
{$ENDIF}
end;

procedure TUnbufferedFileStream.FlushPageBruteForce(buf: PUnBuffer; iRecursions: ni=0);
{x$DEFINE SUPER}
{$IFDEF SUPER}

{$ELSE}
var
  iSeek, iTogo, iAddr, iTotalToWrite, iJustWrote, iWritten: int64;
  b2,b3: PUnbuffer;
  t: ni;
  iPage: int64;
  writesize: ni;
begin
  if buf^.pagenumber < 0 then exit;
//  if buf.pagenumber = ((1664 *512) shr unbuffershift) then begin
//    debug.log('trap flush page  '+inttostr(((1664 *512) shr unbuffershift)));
//      Debug.Log('X[0]: '+memorydebugstring(pbyte(@buf.data[0]), 2048));
//      Debug.Log('X[512]: '+memorydebugstring(pbyte(@buf.data[512]), 2048));
//      Debug.Log('X[1024]: '+memorydebugstring(pbyte(@buf.data[1024]), 2048));
//
//  end;
//  if EnableDirtyDebug then begin
//    Debug.Log('DirtyDebug Page:'+buf.PageNumber.tostring);
//  end;
  if iRecursions > 9 then
    exit;
  iPAge := -1;
  with buf^ do begin
    ECS(lck);
    try
//      rsFlush.BeginTime;
      if not AnyDirty then begin
  //      Debug.Log('page '+inttostr(buf.pagenumber)+' is not dirty, no flushing.');
        exit;
      end;
      if not wasfetched then begin
        if not AllDirty then begin
          FetchPageAndApplyMask(buf);
        end;
      end;

  //  if buf.pagenumber = ((1664 *512) shr unbuffershift) then begin
  //      Debug.Log('DAta[0]: '+memorydebugstring(pbyte(@buf.data[0]), 2048));
  //      Debug.Log('DAta[512]: '+memorydebugstring(pbyte(@buf.data[512]), 2048));
  //      Debug.Log('DAta[1024]: '+memorydebugstring(pbyte(@buf.data[1024]), 2048));
  //    end;


      //Debug.ConsoleLog('Flushing page: '+inttostr(buf.pagenumber));
  //    Debug.ConsoleLog('Flush page '+inttostr(buf.pagenumber));
      iSeek := buf.pagenumber*UNBUFFERSIZE;

      ECS(lckUnder);
      try
        FileSeekPx(FHandle, iSeek, 0);
//        if iSeek = 0 then
//          if (buf.data[81+600] <> 1) then
//            Debug.Log('AHA!');

        GuaranteeWriteUnder(@buf.data[0], sizeof(buf.data));
        iPAge := buf.pagenumber;



      finally
        LCS(lckUnder);
      end;



  //    Debug.ConsoleLog('Done page'+inttostr(buf.pagenumber));
      Cleardirtymask;
//      rsFlush.EndTime; if rsFlush.NewBAtch then Debug.log('flush='+rsFlush.debugtiming);
    finally
      LCS(lck);
    end;
  end;

  //Flush forward for optimization
{  if iPage >= 0 then begin
    b3 := FindPage(iPage+1);
    if b3 <> nil then begin
      if b3.AllDirty then begin
        if TECS(b3.lck) then
        try
          FlushPage(b3, iRecursions+1);
        finally
          LCS(b3.lck);
        end;
      end;
    end;
  end;}

end;
procedure TUnbufferedFileStream.FlushWithoutRead(buf: PUnbuffer);
var
  iOFFByte, iOFF, iStart: int64;
  iCnt: int64;
  iSeekPos: int64;
  iEnd: int64;
  iRead: int64;
  d: TDirtiness;
{$IFDEF ZERO_CHECK}
  procedure CheckForDoubleZeroes;
  var
    xxx: int64;
    cntz: ni;
  begin
    if writebuilder.datasize > 100 then begin
      cntz := 0;
      for xxx := 0 to writebuilder.datasize-2 do begin
        if writebuilder.buffer[xxx] = writebuilder.buffer[xxx+1] then begin
          inc(cntz);
        end;
      end;
      if cntz > 100000 then begin
        Debug.Log('ARGH! ' +writebuilder.buffer[xxx].tohexstring+' '+writebuilder.buffer[xxx+1].tohexstring);
      end;
    end;
  end;
{$ENDIF}
  procedure Commit;
  begin
    if iCnt <= 0 then
      exit;
    ecs(lckUnder);
    try
{$IFDEF ZERO_CHECK}
      CheckForDoubleZeroes;
{$ENDIF}
      if buf.pagenumber = 1479 then
        debug.Log('trap 1479');
{$IFDEF DETAILED_DEBUGGING}
      Debug.Log(extractfilename(filename)+' commit page '+buf.pagenumber.tostring);
{$ENDIF}
      iSeekPos := (buf.pagenumber shl unbuffershift) + (iOFFbyte);
      iEnd := FileSeekPx(FHandle, 0, 2);
      if iSeekPos >= iEnd then
        FlushPageBruteForce(buf)
      else begin
//        if (writebuilder.startpoint shr 9).tohexstring = '1F' then
//          Debug.Log('trap');
//        Debug.Log('COMMIT** FlushBlock='+(writebuilder.startpoint shr 9).tohexstring+' '+'Flush='+(writebuilder.startpoint).tohexstring+' size='+(writebuilder.datasize).tohexstring);
        FileSeekPx(FHandle, writebuilder.startpoint, 0);
        GuaranteeWriteUnder(writebuilder.buffer, writebuilder.datasize);
      end;
    finally
      lcs(lckUnder);
    end;
    iCnt := 0;
    iStart := -1;
    writebuilder.new;
  end;
begin
  with buf^ do begin

    ECS(lck);
    try
//      Debug.Log('FlushWithoutRead '+buf.pagenumber.tostring);
      if buf.pagenumber < 0 then
        exit;
      if not buf.AnyDirty then
        exit;

      //start a new write builder
      writebuilder.New;
      iStart := -1;
      iOFF := 0;
      iCnt := 0;
      while iOFF < (UNBUFFERSIZE shr 9) do begin
        iOffByte := iOff shl 9;
        if not buf.anydirty then exit;//the buffer might be brute-force flushed at any time so we can potentially exit early
        if buf.wasfetched then
          d := dirtAll
        else
          d := buf.ClusterIsDirty(iOFF);
        case d of
          dirtSome: begin
//            Debug.Log('dirtSome '+iOff.tohexstring);
            iSeekPos := (buf.pagenumber shl unbuffershift) + (iOFFbyte);
//            if (iSeekPOs shr 9) = $1f then
//              Debug.Log('trap');
            if iSeekPOs < 0 then
              raise ECritical.create('wtf');
            ecs(lckUnder);
            try

              iEnd := FileSeekPx(FHandle, 0, 2);
              if iSeekPos < iEnd then begin
                FileSeekPx(FHandle, iSeekPOs {yes this is 512 byte aligned},0);
                //read the data into scratch1
                iRead := FileReadPx(@self.aligned_temp,FHandle, scratch1[0], 512);
                if iRead <> 512 then begin
                  Debug.Log('Did not read 512, got '+iRead.toString+' from file. Seek was '+iSeekPos.ToHexString+' when size is '+iEnd.ToHexString);
                end;
              end;
            finally
              lcs(lckUnder);
            end;

            //move into scratch2, data read from disk scratch1, but keep only bytes
            //bits/bytes that are  dirty 0x00
{$IFDEF PACKED_MASK}
            MoveMem32WithMaskPacked_NOT(@scratch2[0], @scratch1[0], @buf.FDirtyMAsk[iOFFbyte shr 3], iOffByte, 512);//ok
{$ELSE}
            MoveMem32WithMask_NOT(@scratch2[0], @scratch1[0], @buf.FDirtyMAsk[iOFFbyte], 512);
{$ENDIF}
            //move the loaded data to the final buffer output, merging with dirty data
{$IFDEF PACKED_MASK}
            MoveMem32WithMaskPacked(@scratch2[0], @buf.data[iOFFByte], @buf.FDirtyMAsk[iOFFbyte shr 3], iOffByte, 512);//ok
{$ELSE}
            MoveMem32WithMask(@scratch2[0], @buf.data[iOFFByte], @buf.FDirtyMAsk[iOFFbyte], 512);
{$ENDIF}
            //if nothing started then begin
            if iStart <0 then begin
              writebuilder.startpoint := iSeekPos;
              iStart := iOFF;
            end;
            //append the data with the writebuilder
            writebuilder.AppendData(@scratch2[0], 512);
            //count the number of clusters pending
            inc(iCnt);
          end;
          dirtAll: begin
//            Debug.Log('dirtAll '+iOff.tohexstring);
            //if nothing started then begin
            if iStart <0 then begin
              writebuilder.startpoint := (buf.pagenumber shl UNBUFFERSHIFT) + (iOffByte);
              iStart := iOFF;
            end;
            //append the data with the writebuilder
            writebuilder.AppendData(@buf.data[iOFFByte], 512);
            //count the number of clusters pending
            inc(iCnt);
          end;
          dirtClean: begin
            //Debug.Log('clean '+iOff.tohexstring);
            //flush the stuff, ignore clean parts
            if iCnt > 0 then begin
              Commit;
            end;
          end;
        end;
        //move forward
        inc(iOff);
      end;
      //final commit
      commit;

      buf.ClearDirtyMask;//since everythign is flushed, we're clean now
    finally
      LCS(lck);
    end;
  end;
end;
function TUnbufferedFileStream.FrontSeek(const offset: int64;
  Origin: TSeekOrigin): int64;
begin
  ecs(lckSTate);
  case origin of
    soBeginning: FFront_SeekPosition := offset;
    soEnd: FFront_SeekPosition := Size - offset;
    soCurrent: FFront_SeekPosition := FFront_SeekPosition + offset;
  end;

  result := FFront_SeekPosition;
  lcs(lckSTate);

//  FSeekPage := FSeekPosition mod UNBUFFERSIZE;
//  SwapPage(FSeekPage);
end;

{$ENDIF}

function TUnbufferedFileStream.GetBufferStatusString: string;
var
  t: ni;
  p: PUnbuffer;
begin
  ecs(lckBuffers);
  try
  result := '';
  for t:= 0 to LASTPAGE do begin
{$IFDEF USE_LINKED_BUFFERS}
    p := FBufferOrders[t].buf;
{$ELSE}
    p := FBufferOrders[t];
{$ENDIF}

    result := result + '['+inttostr(p.pagenumber)+']';
  end;
  finally
    lcs(lckBuffers);
  end;
end;

function TUnbufferedFileStream.GetFlexSeek: int64;
begin
  if UseBackSeek then
    result := FBack_SeekPOsition
  else
    result := FFront_SeekPosition;
end;

function TUnbufferedFileStream.GetPosition: int64;
begin
  ecs(lckSTate);
  result := FFront_SeekPosition;
  lcs(lckState);
end;

function TUnbufferedFileStream.GEtPrefetchbytepos: int64;
begin

  result := prefetchposition shl UNBUFFERSHIFT;
end;

function TUnbufferedFileStream.GetSize: int64;
begin
  result := 0;
  //if the size was not determined (yet)
  if FReportedSize < 0 then begin
    ECS(lckSTate);

    //perform background operations
    ecs(lckOp);
    try
//      if not in_perform_op then
        PerformAllOps;
    finally
      LockBack('getsize');
      lcs(lckOp);
    end;
    //CHECK AGAIN after background operations are completed
    if not FReportedSize < 0 then begin
      result := FReportedSize;
      UnlockBack;
      exit;
    end;

    //still not determined, read EOF from unbuffered
    try
      if not REadEOF({var} FReportedSize) then begin

        FReportedSize := inherited Seek(0,soEnd);
        ecs(lckBuffers);
        try
          WriteEOF();
          {$IFDEF USE_LINKED_BUFFERS}
            Self.FlushPage(Fbufferorders[0].buf);
          {$ELSE}
            Self.FlushPage(Fbufferorders[0]);
          {$ENDIF}
        finally
          lcs(lckBuffers);
        end;
      end;
    finally
      UnlockBack;
      lcs(lckState);
    end;
  end else begin
    //perform background operations
    ecs(lckOp);
    try
      PerformAllOps;
    finally
      result := FReportedSize;
      lcs(lckOp);
    end;

  end;
end;

procedure TUnbufferedFileStream.GrowFile(iNewSize: int64);
begin
//  ecs(lckOp);
//  try
//    if not in_perform_op then
//      PerformAllOps;
//  finally
    ECS(lckSTate);
//    lcs(lckOp);
//  end;
  try
    if iNewSize < size then
      exit;
{$IFDEF ALLOW_UB_WRITE_BEHIND}
    seek(0,soEnd);//front
    WriteBehindZeros(Position, iNewSize-size);//front
{$ELSE}
    seek(0,soEnd);//front
    stream_writezeros(self, iNewSize - size);
{$ENDIF}
  finally
    lcs(lckSTate);
  end;

end;

procedure TUnbufferedFileStream.GuaranteeSyncread(pb: Pbyte; count: ni);
var
  ijust: integer;
  cx: int64;
  wptr:pbyte;
  iTo: int64;
begin
  //DOES NOT NECESSARILY TOUCH FILE (may possibly only touch buffers)
  ecs(lckOp);
  LockBack('guaranteeSyncRead');
  try
  cx := count;
  wptr := pb;
  while cx > 0 do begin
    iTo := cx;
    ijust := SyncRead(wptr^, iTo);
    inc(wptr, iJust);
    dec(cx, ijust);
  end;
  finally
    UnlockBack;
    lcs(lckOp);
  end;
end;

procedure TUnbufferedFileStream.GuaranteeSyncWrite(pb: Pbyte; count: ni; bZeroFlag: boolean = false);
var
  ijustwrote: integer;
  cx: int64;
  wptr:pbyte;
  iToWrite: int64;
  tm1, tm2, tmDif: ticker;
begin
  //DOES NOT NECESSARILY TOUCH FILE (may possibly only touch buffers)
  ecs(lckOp);
  LockBack('GuaranteeSyncWrite');
  try
  cx := count;
  wptr := pb;
  tm1 := GetTicker;
  while cx > 0 do begin
    iToWrite := cx;
    ijustwrote := SyncWrite(wptr^, iToWrite, bZeroFlag);
    inc(wptr, iJustWrote);
    dec(cx, ijustwrote);
  end;
  tm2 := GetTicker;
  tmDif := GetTimeSince(tm2,tm1);
//  Debug.Log('Write '+self.FileName+' sz='+count.tostring+' in '+tmDif.tostring);
  if GetTimeSince(tm2,tm1) > 60000 then begin
    inc(warnings);
  end;
  finally
    UnlockBack;
    lcs(lckOp);
  end;

end;


procedure TUnbufferedFileStream.GuaranteeWriteUnder(pb: Pbyte; sz: ni);
var
  iJustWrote, iTotalToWrite, iToGo, writesize, iWritten: int64;
begin
{x$DEFINE SIMPLE}
{$IFDEF SIMPLE}
   iJustWrote := FileWrite(FHandle, pb^, lesserof(iTogo, sz));
{$ELSE}
  {$IFDEF ALERT_WRITE}
    Debug.Log('WRITE ALERT! '+self.filename);
  {$ENDIF}

  ecs(lckUnder);
  try
    iWritten := 0;
    iTotalToWrite := sz;
    iTogo := iTotalToWrite;
    writesize := 262144*8;
    while iTogo > 0 do begin
      iJustWrote := FileWritePx(@self.aligned_temp,FHandle, pb^, lesserof(iTogo, 262144));
//      iJustWrote := FileWrite(FHandle, pb^, lesserof(iTogo, 262144));
//      if iJustWrote < 0 then begin
//        writesize := writesize shr 1;
//        if writesize < 512 then begin
//          inc(writefailures);
//          exit;
//        end;
//      end;
      if iJustWrote > 0 then begin
        inc(iWritten, iJustWrote);
        dec(iTogo, iJustWrote);
        inc(pb, iJustWRote);
      end;
    end;
  finally
    lcs(lckUnder);
  end;
{$ENDIF}
end;

function TUnbufferedFileStream.FindWriteThroughPage(
  iPageNumber: int64): PUnbuffer;
var
  t: ni;
  cur: PUnbuffer;
begin
  ecs(lckBuffers);
  try
    for t:= 1 to high(FBuffers) do begin
      cur := @FBuffers[t];
      if (not cur.ContainsSomeZeroes) and (cur.pagenumber = iPAgeNumber) and ((cur.anydirty and cur.wasfetched) or cur.AllDirty) then begin
        exit(cur);
      end;
    end;
    exit(nil);
  finally
    lcs(lckBuffers);
  end;

end;

function TUnbufferedFileStream.IndexOfPage(iPage: int64): ni;
var
  t: ni;
begin
  ecs(lckBuffers);
  try
{$IFDEF USE_LINKED_BUFFERS}
  result := -1;
  for t:= 0 to high(FBuffers) do begin
    if FBufferorders[t].buf.pagenumber = iPage then begin
      result := t;
      exit;
    end;
  end;
{$ELSE}
  result := -1;
  for t:= 0 to high(FBufferOrders) do begin
    if FBufferorders[t].pagenumber = iPage then begin
      result := t;
      exit;
    end;
  end;
{$ENDIF}
  finally
    lcs(lckBuffers);
  end;
end;

function TUnbufferedFileStream.IsAfterEOF(iPos: int64): boolean;
begin
  result := iPOs >=size;
end;

procedure TUnbufferedFileStream.Lock;
begin
  ecs(lckState);
end;

procedure TUnbufferedFileStream.LockBack(const sReason: string);
begin
{$IFDEF LOCK_UNDER_INSTEAD_OF_BACK}
  ecs(lckUnder);
{$ELSE}
  ecs(lckBack);
{$ENDIF}
//  Debug.Log(GetCurrentThreadid.tostring +' got the back lock - refs='+IntToStr(lckBack.RecursionCount)+' '+sReason);
//  if lckBack.RecursionCount = 4 then
//    Debug.Log('trap');
end;

procedure TUnbuffer.Needread;
begin
{$IFDEF USE_LINKED_BUFFERS}
  if not wasfetched then begin
    if AnyDirty then begin
      if not AllDirty then begin
        ubs.FetchPageAndApplyMask(@self);
      end;
    end else begin
        ubs.FetchPage(PageNumber, @self);
    end;
    inc(ubs.Misses);
  end else
    inc(ubs.Hits);

{$ELSE}
  if not wasfetched then begin
    if not AllDirty then begin
      FetchPageAndApplyMask(@self);
    end;
  end;
{$ENDIF}



end;
procedure TUnbufferedFileStream.OnUnbufferThreadxExecute(thr: Tmanagedthread);
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TUnbufferedFileStream.OptimalFlush;
var
  buf: PUnbuffer;
  iWritten, iTotalToWrite, iToGo, iJustWrote: int64;
  writesize: ni;
  p: pbyte;
begin
{$IFNDEF USE_OPTIMAL_FLUSH}
  exit;
{$ENDIF}
  IF not g_USE_OPTIMAL_FLUSH then
    exit;

  writebuilder.New;
  buf := FindLowestWriteThrough;
  if buf = nil then exit;


//  Debug.Log('Optimal Flush:'+buf.pagenumber.tostring);
  writebuilder.StartPOint := buf.pagenumber shl UNBUFFERSHIFT;
//  Debug.Log('StartPoint = '+inttohex(writebuilder.StartPOint, 2);
  while buf <> nil do begin
    writebuilder.AppendData(@buf.data, UNBUFFERSIZE);
    buf.ClearDirtyMask;
    buf := FindWriteThroughPage(buf.pagenumber+1);
  end;

  FileSeekPx(FHandle, writebuilder.StartPOint, 0);
  iWritten := 0;
  iTotalToWrite := writebuilder.datasize;
  GuaranteeWriteUnder(writebuilder.buffer, writebuilder.DataSize);
end;

procedure TUnbufferedFileStream.PerformAllOps;
begin
  {$IFDEF ALLOW_UB_WRITE_BEHIND}
//  ecs(lckState);
  ecs(lckOp);
  if sfthread <> nil then
    sfthread.StepCount := OpCount;
  while PerformOp do if sfthread <> nil then sfthread.STep := OpCount;
  if sfthread <> nil then sfthread.step := opcount;
  lcs(lckOp);
//  lcs(lckState);
  {$ENDIF}
end;

function TUnbufferedFileStream.PerformOp: boolean;
var
  ol: TUBOp;
begin
//  ecs(lckSTate);
  ol := nil;
  try
  in_perform_op := true;
  try
    ecs(lckOp);
    try
      ol := oplink;
      if ol=nil then
        exit(false);

//        Debug.Log('EX '+ol.classname);
        ol.Doop;
//        Debug.Log('OK '+ol.classname);
         inc(opsPErformed);
        DebugOps;

        oplink := TUBOp(ol.Next);
        dec(opcount);
        if oplink = nil then
          opcount := 0;
        exit(oplink<>nil);
    finally

      lcs(lckOp);
      if (ol<> nil) and (ol.autokill) then
        ol.free;
    end;
  finally
    in_perform_op := false;
  end;
  finally
//    lcs(lckState);
  end;

end;

procedure TUnbufferedFileStream.PerformOps_FallingBehind;
begin
  if opcount < 8 then
    exit;

//  ecs(lckState);
  ecs(lckOp);
  try
    //while opcount > 8 do
    while opcount > 1024 do
      if not PerformOp then break;
  finally
    lcs(lckOp);
//    lcs(lckState);
  end;
end;

function TUnbufferedFileStream.PrefetchStep: ni;
begin
  result := allowed_prefetches;
end;

function TUnbufferedFileStream.PrePareAndLockPAge(iPAge: int64;
  bNeedREad: boolean; bForEof: boolean = false): TUnbufferobj;
begin
  ecs(lckBuffers);
  try
//    Debug.Log('Page: '+iPAge.ToString);

    SeekPage(iPAge, bForEof);
    result := FBufferOrders.First;
    result.buf.Lock;

  finally
    lcs(lckBuffers);
  end;

  if bNEedREad then begin
    result.buf.NEedRead;
  end;

end;

function TUnbufferedFileStream.SYncRead(var Buffer; Count: Integer): Longint;
var
  iPossible: int64;
  iOffSet: int64;
  iPage: int64;
  buf: TUnbufferObj;
  was: ni;
  seekpos: int64;
begin
//  if count = $10000 then
//    Debug.Log('trap');
  seekpos := FBack_SeekPOsition;
//  CancelPrefetches;


{$IFDEF DEBUG_UNBUFFERED_OPS}
  debug.Log(self,classname+'.Read '+inttohex(FSeekPosition, 0)+' cnt='+inttohex(count,0));
{$ENDIF}
//  if eof then
//    raise ECritical.create('read past eof in '+filename);
  //Seek buffer for page
  ecs(lckOp);
  try
    if not in_perform_op then
      PerformAllOps;
  finally
    LockBack('SyncRead');
    lcs(lckOp);
  end;
  try
    FBack_SeekPOsition := seekpos;
    iPage := FBack_SeekPosition shr unbuffershift;
    buf := PrePareAndLockPAge(iPAge, true);
    try
      //determine offset
      iOffset := FBack_SeekPosition and UNBUFFERMASK;
      iPossible := FReportedSize - FBack_SeekPosition;
      iPossible := lesserof(iPossible, UNBUFFERSIZE-(iOffset));

      //determine how many bytes we will read based on possible or bytes requested
      result := lesserof(iPOssible, count);
      //get the stuff from the buffer
  {$IFDEF USE_LINKED_BUFFERS}
      movemem32(@byte(buffer), @buf.buf.data[iOffset],result);
  {$ELSE}
      movemem32(@byte(buffer), @buf.buf.data[iOffset],result);
  {$ENDIF}

      FBack_SeekPosition := FBack_SeekPosition + result;
      //update EOF flag
      //eof := FSeekPosition >= FReportedSize;
      prefetchwritemode := false;
      prefetchposition := FBack_SeekPosition shr unbuffershift;

    finally
      buf.buf.unlock;
    end;
  finally
    UnlockBack;
  end;


end;

function TUnbufferedFileStream.SyncSeek(const offset: int64;
  Origin: TSeekOrigin): int64;
begin
  ecs(lckOp);
  try
    if not in_perform_op then
      PerformAllOps;
  finally
    LockBack('SyncSeek');
    lcs(lckOp);
  end;
  case origin of
    soBeginning: FBack_SeekPosition := offset;
    soEnd: FBack_SeekPosition := FReportedSize - offset;
    soCurrent: FBack_SeekPosition := FBack_SeekPosition + offset;
  end;

  result := FBack_SeekPosition;
  UnlockBack;

//  FSeekPage := FSeekPosition mod UNBUFFERSIZE;
//  SwapPage(FSeekPage);
end;


procedure TUnbufferedFileStream.SyncSetSize(const iLen: int64);
var
  x,y,nsz: int64;
begin
  ecs(lckOp);
  try
    if not in_perform_op then
      PerformAllOps;
  finally
    LockBack('SyncSetSize');
    lcs(lckOp);
  end;
  try

    if iLen = FReportedsize then exit;
  //  inherited;
    x := 1+(FReportedSize div UNBUFFERSIZE);
    y := 1+(iLen div UNBUFFERSIZE);
    if y > x then begin
      ecs(lckBuffers);
      try
      while x < y do begin
  {$IFDEF USE_LINKED_BUFFERS}
{$IFDEF PACKED_MASK}////
        FBufferOrders[0].buf^.SetDirty(0, UNBUFFERSIZE);//ok
{$ELSE}
        fillmem(@FBufferOrders[0].buf.FDirtyMask[0], sizeof(FBufferOrders[0].buf.FDirtyMask), 0);
{$ENDIF}
        FBufferOrders[0].buf.SetAllDirty;
  {$ELSE}
        fillmem(@FBufferOrders[0].FDirtyMask[0], sizeof(FBufferOrders[0].FDirtyMask), 0);
        FBufferOrders[0].FPageIsDirty := true;
  {$ENDIF}


        //FLushPAge(FBufferOrders[0]);
        SeekPage(x);//prefetch grow to new end
        inc(x);
      end;
      if iLen <> FReportedsize then begin
        FReportedSize := iLen;
        WriteEof();
      end;
      finally
        lcs(lckBuffers);
      end;

    end else
    if y < x then begin
      Debug.ConsoleLog('**************TRUNCATING UNBUFFERED FILE TO '+Commaize(iLen)+' x='+inttostr(x)+' '+FileName);
      nsz := ((y) * UNBUFFERSIZE);
      //inherited SetSize(nsz);

      ecs(lckUnder);
      try
      FileSeekPx(FHandle, nsz, 0);
      {$IF Defined(MSWINDOWS)}
        Win32Check(SetEndOfFile(FHandle));
      {$ELSEIF Defined(POSIX)}
        raise ECritical.create('not supported on non-windows');
  //      if ftruncate(FHandle, Position) = -1 then
  //        raise EStreamError('could not set stream size');
      {$ENDIF POSIX}
      finally
        lcs(lckUnder);
      end;
      if iLen <> FReportedsize then begin
        FReportedSize := iLen;
        WriteEof();
      end;


    end else begin
      if iLen <> FReportedsize then begin
        FReportedSize := iLen;
        WriteEof();
      end;

    END;
  finally
    UnlockBack;
  end;
end;


function TUnbufferedFileStream.Seek(const offset: int64;
  Origin: TSeekOrigin): int64;
begin
  lastuse := getticker;
  result := FrontSeek(offset, origin);
end;

procedure TUnbufferedFileStream.SeekLock;
begin
  ecs(lckState);
end;

procedure TUnbufferedFileStream.SEtSize(const iLen: int64);
begin
  ecs(lckSTate);
  try
    SyncSetSize(iLen);
    FReportedSize := iLen;
  finally
    lcs(lckState);
  end;
end;

function TUnbufferedFileStream.SmartSideFetch(EXTQueue: TAbstractSimpleQueue): boolean;
//THIS IS ONLY CALLED FROM the SIDE FETCH THREAD!
//it sets HasWork = false when it is done side-fetching
var
  start: int64;
  tm1, tm2: ticker;
  pidx: ni;
begin
  result := false;

  try
  tm1 := GetHighResTicker;
  //if the last explicit op was a read
  //then fetch
  //if not lastopwaswrite then begin
    if (allowed_prefetches > 0) (*and ((upstreamhints = nil) or (UpStreamHints.pendingOps = 0))*) then begin
          while allowed_prefetches > 0 do begin
            sfthread.step := allowed_prefetches;
//            if EXTQueue.estimated_queue_size > 1 then
//              exit;
            start := prefetchposition;
            if start >=0 then begin
              ecs(lckBuffers);
              pidx := IndexOfPage(prefetchposition);
              lcs(lckBuffers);
              if not (pidx>=0) then begin
                PrePareAndLockPAge(prefetchposition, NOT prefetchwritemode).buf.unlock;
                result := true;
                Dec(allowed_prefetches);
                inc(prefetchposition, 1);
                //exit;   //<<------------------------------------------
              end else begin
                inc(prefetchposition, 1);                          //|
                dec(allowed_prefetches, 1);                        //|
              end;                                                 //|
                                                                   //|
            end else begin                                         //|
              allowed_prefetches := 0;                             //|
              sfthread.step := allowed_prefetches;                 //|
            end;                                                   //|
                                                                   //|
                                                                   //|
            tm2 := GetHighResTicker;                               //|
            if (tm2-tm1) > 1000000 {1ms} then                        //|
              break;                                               //|
                                                                   //|
          end;                                                     //|
                                                                   //|
                                                                   //|
    end;                                                           //|
//  ecs(lckOp);
    sfthread.haswork := false and (oplink<>nil) and (opcount<=0);//<<--gets here only if NOT early exit
    sfthread.runhot := false and (oplink<>nil) and (opcount<=0);
//  lcs(lckOp);
  except
  end;

end;

procedure TUnbufferedFileStream.StartPrefetches;
begin
  allowed_prefetches := RecommendedPrefetchAllowance;
end;

procedure TUnbufferedFileStream.SeekPage(const iPage: int64; bForEof: boolean = false);
var
  i: ni;
  bufo: TUnbufferObj;
  buf: PUnBuffer;
  obj: TUnbufferObj;
  timing: boolean;
begin
  timing := false;
  try
  ecs(lckBuffers);
  try

  if FSeekPAge = iPage then exit;
{$IFDEF USE_LINKED_BUFFERS}



  obj := FindPageObj(iPage);
  if obj <> nil then begin
    bringtofront(obj);
    FSeekPage := iPage;
    exit;
  end;
//  DebugPAges;
  bufo := FBufferOrders.Last;
  buf := bufo.buf;
//  if iPage = 0 then
//    if (buf.data[81+600] <> 1) then
//      Debug.Log('AHA!')
//    else
//      Debug.Log('Ahoy?');
  buf.Lock;
  try
{$ELSE}
  i := IndexOfPage(iPage);
  if i >= 0 then begin
    bringtofront(i);
    FSeekPage := iPage;
    exit;
  end;
  buf := FBufferOrders[LASTPAGE];
{$ENDIF}
  if buf.AnyDirty then begin

    OptimalFlush;
//    if bForEof then begin rsEof.BeginTime; timing := true; end;
    if buf.AnyDirty then
      FlushPage(buf);
  end;


  with buf^ do begin
    pagenumber := iPage;
    wasfetched := false;
  end;
//  DebugPAges;
{$IFDEF USE_LINKED_BUFFERS}
  bringtofront(bufo);
{$ELSE}
  bringtofront(buf);
{$ENDIF}

  FSeekPage := iPage;
  finally
    buf.unlock;
  end;
  finally
    lcs(lckBuffers);
  end;
  finally
    if bForEof and timing then begin
//      rsEof.EndTime;
//      if rsEof.NewBAtch then Debug.Log('Eof='+rsEof.DebugTiming);
    end;
  end;
end;

procedure TUnbufferedFileStream.SeekUnlock;
begin
  lcs(lckState);
end;

procedure TUnbufferedFileStream.SetFlexSeek(const Value: int64);
begin
  if UseBackSeek then
    FBack_SeekPOsition := value
  else
    FFront_SeekPosition := value;
end;

procedure TUnbufferedFileStream.SetPosition(const Value: int64);
begin
  ecs(lckSTate);
  Seek(value, soBeginning);
  lcs(lckState);
end;

procedure TUnbufferedFileStream.SetPrefetchBytePos(const Value: int64);
begin
  prefetchposition := value shr UNBUFFERSHIFT;
end;

function TUnbufferedFileStream.SyncWrite(const Buffer; Count: Integer; bZeroFlag: boolean = false): Longint;
var
  iPossible: int64;
  iOffSet: int64;
  buf: PUnbuffer;
  bMoreDebug: boolean;
  bufin: PByte;
  bufo: TUnbufferObj;
  was: ni;
  seekpos: int64;
begin
//  rsWrite0.BeginTime;
  try
  seekpos := FBack_SeekPOsition;
//  CancelPrefetches;

{x$DEFINE DEBUG_UNBUFFERED_OPS}
{$IFDEF DEBUG_UNBUFFERED_OPS}
  debug.Log(self,classname+'.Write '+inttohex(FSeekPosition, 0)+' cnt='+inttohex(count,0));
{$ENDIF}
  ecs(lckOp);
  try
    if not in_perform_op then
      PerformAllOps;
  finally
    LockBack('SyncWrite');
    lcs(lckOp);
  end;
  try
    FBack_SeekPOsition := seekpos;
    bufo := PrePareAndLockPAge(FBack_SeekPosition shr unbuffershift, false);
    buf := bufo.buf;
    try
//      Debug.Log(FBack_SeekPosition.ToString);
    //  FBufferOrders[0].DebugWAtch(true, FSeekPOsition, count, g_alarm);
      iOffset := FBack_SeekPosition and UNBUFFERMASK;
      iPossible := UNBUFFERSIZE-(iOffset);
      result := lesserof(iPOssible, count);

      bufin := @byte(buffer);
      movemem32(@buf.data[iOffset],bufin, result);
      if not buf.AnyDirty then begin
        buf.FDirtyTime := Getticker;
      end;
    //  if bMoreDebug then begin
    //    Debug.Log('DAta is now : '+memorydebugstring(pbyte(@buf.data[iOffset])+48128, lesserof(count,512)));
    //    Debug.Log('buffer status:'+self.GetBufferStatusString);
    //  end;


//      rsWrite1.BeginTime;
      if not buf.wasfetched then begin
        if not buf.AllDirty then begin
          buf.SetDirty(iOffset, result);
        end;
      end else begin
        buf.SetAllDirty;
      end;
//      rsWrite1.EndTime;


      buf.ContainsSomeZeroes := bZeroFlag;



      FBack_SeekPosition  := FBack_SeekPosition  + result;
      if FBack_SeekPosition  >  FSizeCommittedToBuffers then begin
        FReportedSize := FBack_SeekPosition ;
        if not buf.AnyDirty then begin
          buf.FDirtyTime := Getticker;
        end;
        buf.Unlock;
        buf := nil;
//        rsWrite2.BeginTime;
        WriteEof();
//        rsWrite2.EndTime;
      end
    finally
      if buf <> nil then
        buf.Unlock;
    end;




    prefetchwritemode := true;
    prefetchposition := FBack_SeekPosition  shr unbuffershift;
//    allowed_prefetches := RecommendedPrefetchAllowance;
//    sfthread.runhot := true;
//    sfthread.haswork := true;
{$IFNDEF USE_PERIODIC_QUEUE}
    CheckOrStartFlushCommand;
{$ENDIF}
  finally
    UnlockBack;
  end;
  finally
//    rsWrite0.EndTime;
//    if rsWrite0.NewBatch then
//      debug.log('Write0='+rsWrite0.DebugTiming);
//    if rsWrite1.NewBatch then
//      debug.log('Write1='+rsWrite1.DebugTiming);
//    if rsWrite2.NewBatch then
//      debug.log('Write2='+rsWrite2.DebugTiming);
  end;
end;

procedure TUnbufferedFileStream.SyncWriteZeros(sz: ni);
var
  iJustWritten, iTotalWritten: int64;
  p: pbyte;
begin
  ecs(lckOp);
  try
    if not in_perform_op then
      PerformAllOps;
  finally
    LockBack('SyncWriteZeros');
    lcs(lckOp);
  end;
  try
  p := GetMemory(262144);
  try
    FillMem(p, 262144, 0);
    iTotalWritten := 0;
    while iTotalWritten < sz do begin
      iJustWritten := lesserof(262144, sz-iTotalWritten);
      GuaranteeSyncWrite(p, iJustWritten, true);
      iTotalWritten := iTotalWritten + iJustWritten;
    end;
  finally
    FreeMemory(p);
  end;
  finally
    UnlockBack;
  end;


end;


function TUnbufferedFileStream.TryLock: boolean;
begin
  result := tecs(lckState);
end;

procedure TUnbufferedFileStream.Unlock;
begin
  lcs(lckState);
end;

procedure TUnbufferedFileStream.UnlockBack;
begin
//  Debug.Log(GetCurrentThreadid.tostring +' released the back lock  - refs='+IntToStr(lckBack.RecursionCount));
{$IFDEF LOCK_UNDER_INSTEAD_OF_BACK}
  lcs(lckUnder);
{$ELSE}
  lcs(lckBack);
{$ENDIF}


end;

function TUnbufferedFileStream.Write(const Buffer; Count: Integer): Longint;
var
  pb: PByte;
begin
  lastuse := getticker;
{$IFDEF DISABLE_WRITES}
  Debug.Log('Writes are disabled!');
  exit(count);
{$ENDIF}
{$IFNDEF ALLOW_UB_WRITE_BEHIND}
  ecs(lckState);
  try
    ecs(lckOp);
    LockBack('write behind');
    try
//      Debug.Log(FFront_SeekPOsition.tostring);
      if FFront_SeekPosition < 0 then
        raise Ecritical.create('wtf');
      FBAck_SeekPOsition := FFront_SeekPOsition;
      result := SyncWrite(buffer, count);
      if FBack_SeekPOsition <> result + FFront_SeekPOsition then
        raise ECritical.create('!!!');
      FFront_SeekPOsition := FBAck_SeekPOsition;
    finally
      UnlockBack;
      lcs(lckOp);
    end;
  finally
    lcs(lckState);
  end;
{$ELSE}
  ecs(lckSTate);  //DEADLOCK!
  try
    WriteBehind(FFront_SeekPosition, @byte(Buffer), Count);
    result := count;
  finally
    lcs(lckState);
  end;
{$ENDIF}
end;

procedure TUnbufferedFileStream.WriteBehind(seekpos: int64; pb: PByte; sz: int64);
{$IFDEF ALLOW_UB_WRITE_BEHIND}
var
  wb: TUBWriteBehind;
begin
  ecs(lckOp);
  try
    wb := TUBWriteBehind.create;
    wb.ubs := self;
//    Debug.Log('Create WB:'+seekpos.tohexstring+' '+memorytohex(@pb[600], lesserof(128, sz-600)));
    wb.Assign(seekpos, pb,sz);
    wb.autokill := true;
    AddOp(wb);
  finally
    ecs(lckState);
    lcs(lckOp);
    FFront_SeekPOsition := seekpos + sz;
    FReportedSize := greaterof(seekpos+sz, Size);
//    if seekpos = 0 then
//      Debug.Log('Trap');
    //PerformAllOps;//XXXX
    lcs(lckState);
  end;
end;
{$ELSE}
begin
  ecs(lckSTate);
  try
    Seek(seekpos, soBeginning);
    FBack_SeekPosition := FFront_SeekPOsition;
    GuaranteeSyncWrite(pb, sz);
    FFront_SeekPOsition := FBack_SeekPosition;
  finally
    lcs(lckState);
  end;
end;
{$ENDIF}

procedure TUnbufferedFileStream.WriteBehindZeros(seekpos: int64; sz: int64);
{$IFDEF ALLOW_UB_WRITE_BEHIND}
var
  wb: TUBWriteBehindZeros;
begin
  ecs(lckOp);
  try
    wb := TUBWriteBehindZeros.create;
    wb.ubs := self;
    wb.sz := sz;
    wb.autokill := true;
    wb.seekpos := seekpos;
    AddOp(wb);
  finally
    ecs(lckState);
    lcs(lckOp);
    self.FFront_SeekPOsition := seekpos + sz;
    FReportedSize := greaterof(seekpos+sz, Size);

//    PerformAllOps;
    lcs(lckState);
  end;
//  ecs(lckState);
  try

  finally

//    lcs(lckState);
  end;
end;
{$ELSE}
begin
  ecs(lckSTate);
  try
    Seek(seekpos, soBeginning);
    FBack_SeekPOsition := FFront_SeekPosition;
    self.SyncWriteZeros(sz);
//    Stream_WriteZeros(self,sz);
    FFront_SeekPOsition := FBack_SeekPosition;

  finally
    lcs(lckState);
  end;
end;
{$ENDIF}

procedure TUnbufferedFileStream.WriteEOF();
var
  iPossible: int64;
  iOffSet: int64;
  iTEmp: int64;
  isz: int64;
  buf: PUnbuffer;
  iPAge: int64;
  bufo: TUnbufferObj;
begin
//  if FReportedSize = $2001000 then
//    Debug.Log('here');
  iSZ := fReportedSize+16;
  iPAge := (iSZ-1) shr UNBUFFERSHIFT;

//  rsEof.BeginTime;
  bufo := PrePareAndLockPAge(iPage, false, true);
//  rsEof.endTime;
  buf := bufo.buf;
  try
    iOffset := UNBUFFERSIZE - 16;
    iTemp := FReportedSize xor $FFFFFFFFFFFFFFFF;

//    debug.log('write eof '+inttostr(FReportedsize));

    movemem32(@buf.data[iOffset],@iTemp, 8);
    movemem32(@buf.data[iOffset+8],@FreportedSize, 8);
//    Debug.Log('wrote size '+FReportedSize.tohexstring+' to page '+iPage.ToString);
    buf.SetDirty(iOffset, 16);
    FSizeCommittedToBuffers := FReportedSize;

  finally
    buf.Unlock;
  end;


end;

function TUnbufferedFileStream.Read(var Buffer; Count: Integer): Longint;
begin
  lastuse := getticker;
  try
    ecs(lckOp);
    LockBack('Read');
    try
      ecs(lckState);
//      Debug.Log('READ '+FFront_SeekPosition.tohexstring);
      FBack_SeekPosition := FFront_SeekPOsition;
      result := SyncRead(buffer, count);
      FFront_SeekPOsition := FBAck_SeekPOsition;
//      Debug.Log('READ OK');
      lcs(lckSTate);
    finally
      UnlockBack;
    end;
    lcs(lckOp);
  finally

  end;
end;

function TUnbufferedFileStream.ReadEOF(var iLength: int64): boolean;
var
  iPossible: int64;
  iOffSet: int64;
  iTEmp: int64;
  iSz: int64;
  buf: PUnbuffer;
  iPage: int64;
begin
  ecs(lckSTate);
  try
    PerformAllOps;

    iSZ := inherited Seek(0,soEnd);
    ipage := (iSZ-1) div UNBUFFERSIZE;
    buf := PrePareAndLockPAge(iPAge, true).buf;
    try
      iOffset := UNBUFFERSIZE - 16;

      movemem32(@iTemp, @buf.data[iOffset], 8);
      movemem32(@iLength,@buf.data[iOffset+8], 8);
      result := (iTemp xor $FFFFFFFFFFFFFFFF) = iLength;
      if not result then
        debug.Log(self,'failed to READ EOF of file '+self.filename+' ... must synthesize.');

      FSizeCommittedToBuffers := iLength;
    finally
      buf.unlock;
    end;
  finally
    lcs(lckState);
  end;
end;



function TUnbufferedFileStream.RecommendedPrefetchAllowance: ni;
begin
  result := FRecommendedPrefetchAllowance;
end;

{ TAdaptiveQueuedStream }

procedure TAdaptiveQueuedStream.AdaptiveRead(p: pbyte; iSize: int64);
begin
  EndAdaptiveRead(isize, BeginAdaptiveRead(p, iSize));
end;

procedure TAdaptiveQueuedStream.AdaptiveWrite(p: Pbyte; iSize: int64);
begin
  BeginWrite(POsition, p, iSize);
  Position := Position + iSize;
end;

procedure TAdaptiveQueuedStream.AdaptiveWriteZeroes(addr, iCount: int64);
begin
  if (addr = $3000C291) then
    Debug.Log(self, 'trap! '+TUnbufferedFileSTream(self.UnderStream).filename);
  BeginWriteZeros(addr,icount);
  position := position + icount;

end;

function TAdaptiveQueuedStream.BeginAdaptiveRead(p: pbyte;
  iSize: int64; bForget: boolean): TReadCommand;
begin

  iSize := lesserof(iSize, size-Position);
  result := BeginRead(position, p, iSize, false, bForget);
  Position := Position + iSize;
end;

constructor TAdaptiveQueuedStream.Create(const AStream: TStream;
  bTakeOwnership: boolean);
begin
  inherited Create;
  FUnderStream := AStream;
  OwnsStream := bTakeOwnership;
  ReadInitialSize;
{$IFDEF IDLE_FETCH}
  FQueue.OnIdle := queue_onidle;
  FQueue.NoWorkRunInterval := 444;
//  FQueue.AutoMaintainIdleInterval := true;
{$ENDIF}

  EnableQueue := true;

end;

destructor TAdaptiveQueuedStream.Destroy;
begin

  inherited;

  if OwnsStream then begin
    FUnderStream.free;
    Funderstream := nil;
  end;

end;

procedure TAdaptiveQueuedStream.Detach;
begin
  FQueue.OnIdle := nil;

  inherited;

end;

function TAdaptiveQueuedStream.EndAdaptiveRead(iOriginalsize: int64; qi: TReadCommand): int64;
begin
  result := iOriginalSize;
  if qi = nil then exit;
  qi.WaitFor;
  result := qi.result;
  qi.Free;

end;


procedure TAdaptiveQueuedStream.GRowFile(iSize: int64);
begin
  BeginWriteZeros(size, iSize - size);
end;

function TAdaptiveQueuedStream.IsAfterEOF(iPOs: int64): boolean;
begin
  result := iPOs > Size;
end;

procedure TAdaptiveQueuedStream.queue_onidle(sender: TObject);
begin
  queue.maxitemsinqueue := G_UB_queUE_DEPTH;
//  if FQueue.TryLock then
//  try
//    if understream is TUnbufferedFileStream then begin
//      if TUnbufferedFileStream(FUnderStream).SmartSideFetch(queue) then
//        inc(queue.sidefetches);
//    end;
//  finally
//    FQueue.Unlock;
//  end;
end;

function TAdaptiveQueuedStream.Seek(iPos: int64; origin: TseekOrigin): int64;
begin
  case origin of
    soBeginning: Position := iPOs;
    soCurrent: POsition := Position + iPOs;
    soEnd: POsition := Size - iPos;
  end;

  result := Position;

end;

function TAdaptiveQueuedStream.Seek(iPOs, origin: int64): int64;
begin
  position := iPos + origin;
  result := position;
end;


{ Tunbuffer }

procedure Tunbuffer.Init;
begin
  pagenumber := -1;
  DirtyCount := -1;
  CLearDirtyMAsk;
{$IFDEF DYN_BYTES}
  SetLength(data, UNBUFFER_SIZE);
{$ENDIF}
end;

procedure Tunbuffer.Lock;
begin
  ECS(lck);
end;

function Tunbuffer.PageStart: int64;
begin
  result := pagenumber shl unbuffershift;
end;



procedure Tunbuffer.SetDirty(offset, cnt: ni);
var
  t: ni;
  dc: ni;
  bchanged: boolean;
begin
  //SHORTCUT! - if we're writing to the entire buffer, we can
  //just skip the dirty mask as the time it takes to track the
  //stuff ADDS up to potentially multiple MS per write.
  //when flushing the buffer, if the dirtycount matches
  //the unbuffer size, then the dirty mask is not used anyway.
  if cnt = UNBUFFERSIZE then begin
    DirtyCount := UNBUFFERSIZE;
    exit;
  end;

  dc := DirtyCount;
  if alldirty then exit;
  for t:= offset to offset+cnt-1 do begin
    if SetDirty(t) then inc(dc);
  end;
  DirtyCount := dc;



end;

procedure Tunbuffer.SetAllDirty;
begin
  DirtyCount := UNBUFFERSIZE;
end;

function Tunbuffer.SetDirty(offset: ni): boolean;
//DO NOT CALL EXTERNALLY unless you're MAINTAINING DIRTY COUNT
{$IFDEF PACKED_MASK}
var
  bo: nativeint;
  pb: PByte;
  nu: byte;
  byt: byte;
{$ENDIF}
begin
{$IFDEF PACKED_MASK}
  bo := offset shr 3;//div
  nu := offset and 7;//remainder
  nu := 1 shl nu;

  pb := @FDirtyMask[bo];
  byt := pb^;
  result := (byt and nu) <> 0;//check if bit was set previously
  byt := byt and (not nu);//turn off bit
  pb^ := byt;//save result
{$ELSE}
  result := FDirtyMask[offset] = $FF;
  FDirtyMask[offset] := 0;
{$ENDIF}

end;

procedure Tunbuffer.SetPageNumber(const Value: int64);
begin

  if (value>0) and ((value and $7000000000000000) <> 0) then
    Debug.ConsoleLog('Address is insane.');

  FPageNumber := Value;
end;

procedure Tunbuffer.Unlock;
begin
  LCS(lck);
end;

{ TQueueStreamItem }


{ TQueueStreamItem }

function TQueueStreamItem.DebugString: string;
begin
  result := inherited + '->'+FStream.classname
end;

procedure TQueueStreamItem.Execute;
var
  tmStart, tmEnd: ticker;
begin
  tmStart := Getticker;
  inherited;
  tmEnd := GetTicker;
  ExecutionTime := tmEnd-tmStart;

end;

{ Tevent_UBFSCheckFLush }

procedure Tevent_UBFSCheckFLush.DoExecute;

begin
  inherited;

{$IFDEF USE_PERIODIC_QUEUE}
//  TUnbufferedFileStream(owner).CheckFlush;
  TUnbufferedFileStream(owner).CheckOrStartFlushCommand;



{$ENDIF}

end;



{ Tcommand_FlushStale }

procedure Tcommand_FlushStale.DoExecute;
begin
  inherited;
//  exit;
  UnbufferedStream.CheckFlush;
end;

{ TSideFetchCommand }

procedure TSideFetchCommand.DoExecute;
begin
  inherited;
  if FStream.UnderStream is TUnbufferedFileStream then
    TUnbufferedFileStream(FStream.UnderStream).SmartSideFetch(self.Queue)
  else
    raise ECritical.create('WTF');

end;

procedure TSideFetchCommand.Init;
begin
  inherited;
  AutoDestroy := true;
end;

{ TUnbufferedSideFetchThread }

procedure TUnbufferedSideFetchThread.DoExecute;
begin
  inherited;

{$IFDEF ALLOW_UB_SIDE_FETCH}
  loop := true;
//  Debug.Log(self, 'EXECUTE SF THREAD: '+ubs.filename);

//  ecs(ubs.lckOp);
  ubs.PErformAllOps;

  ubs.SmartSideFetch(nil);
  StepCount := ubs.RecommendedPrefetchAllowance;
  Step := ubs.PrefetchStep;
  IterationCOmplete;
//  lcs(ubs.lckop);
{$ELSE}
  sleep(100);
{$ENDIF}

end;

{ TUBWriteBehind }

procedure TUBWriteBehind.Assign(seekpos: int64; pb: Pbyte; sz: ni);
begin
  FPb := GEtMemory(sz);
  movemem32(FPb, pb, sz);
  self.seekpos := seekpos;
  self.sz := sz;
end;

destructor TUBWriteBehind.Destroy;
begin
  if assigned(Fpb) then
    FreeMemory(FPB);
  inherited;
end;

procedure TUBWriteBehind.Doop;
var
  rem: int64;
begin
  ubs.LockBack('TUBWriteBehind.Doop');
  try
    ubs.FBack_SeekPosition := seekpos;
//    Debug.Log('WB:'+seekpos.tohexstring+' '+memorytohex(@Fpb[600], lesserof(128, sz-600)));
//    if seekpos = 0 then
//      Debug.Log('TRAP!');


    rem := ubs.FBack_SeekPosition;
    ubs.GuaranteeSyncWrite(FPb, sz);
    if ubs.FBAck_SeekPosition <> (rem+sz) then
      raise ECritical.create('wtf!');
  finally
    ubs.UnlockBack;
  end;

end;

{ TUBWriteBehindZeros }

destructor TUBWriteBehindZeros.Destroy;
begin

  inherited;
end;

procedure TUBWriteBehindZeros.Doop;
var
  rem: int64;
  tm1, tm2: ticker;
begin
  ubs.LockBack('TUBWriteBehindZeros.Doop');
  try
    ubs.FBack_SeekPosition := seekPOs;
    rem := ubs.FBack_SeekPOsition;
    tm1 := GetTicker;
    ubs.SyncWriteZeros(sz);
    tm2 := GetTicker;
    if gettimesince(tm2,tm1) > 12000 then begin
      inc(ubs.warnings);//too many warnings and drive will be marked bad
    end;
    if ubs.FBAck_SeekPosition <> (rem+sz) then
      raise ECritical.create('wtf!');

  finally
    ubs.UnlockBack;
  end;
end;

procedure TUBWriteBehindZeros.SetSz(const Value: ni);
begin
  if value < 0 then
    raise ECritical.create('sz cannot be < 0');
  Fsz := Value;
end;

{ TAlignedTempSpace }

{$ENDIF}

procedure Balance;
begin
//  ecs(balancer);
//  lcs(balancer);

end;


initialization
{$IFDEF MSWINDOWS}
  ics(balancer);
  balancer.throttle := 0;
//  g_alarm := false;
{$ENDIF}
end.





