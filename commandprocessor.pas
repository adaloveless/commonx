unit commandprocessor;
{$R-}
{$Message '********************************Compiling CommandProcessor'}
{$DEFINE USE_LINKED_LISTS_OF_COMMANDS}
{$INCLUDE DelphiDefs.inc}
{x$DEFINE DO_SLEEPEX}
{x$DEFINE DETAILED_COMMAND_LOGGING}
{$IFDEF WINDOWS}
  //TODO 1: Find a solution for this on IOS
  {x$DEFINE SUPPORTS_APC}
{$ENDIF}
{$DEFINE ALLOW_FORGET}
{x$DEFINE USE_OPT_GROUPS}
{$DEFINE USE_THREADVARS_FOR_WAITING}
{x$DEFINE USE_CLUSTERED_LIST}
{x$DEFINE PERFORMANCE_LOGGING}
{$DEFINE DONT_USE_DQ}
{$DEFINE FIRE_FORGET_CONTROLLED_BY_CP}
{$DEFINE CLEANSTALE}
interface

uses
  linked_list, better_collections, typex, tickcount,orderlyinit, numbers, System.Diagnostics,clusteredpointerlist, ringstats, signals, destructionqueue, generics.collections.fixed,
{$IFDEF WINDOWS}
  winapi.windows,
{$ENDIF}
  systemx, Debug, sysutils, exceptions, stringx,
  ManagedThread, Classes, SharedObject, BetterObject, ThreadManager,
  backgroundthreads, commandicons;
{$DEFINE RAISE_EXCEPTIONS_FROM_COMMANDS_ON_WAITFOR}
{$DEFINE LOCK_RESOURCE_SCAN}
{x$DEFINE ENABLE_QUICKPOOL}  //reduces potential stress on thread pool, not typically necessary
{x$DEFINE OVERSPILL}
{x$DEFINE QUEUE_INCOMING}
{x$DEFINE ALLOW_EARLY_THREADDONE_SIGNAL}
//{$INLINE AUTO}

const
  MAX_CHILD_THREADS = -1;
  THREAD_SLEEP = 1;
  LARGE_FILE_THRESHOLD = 1000000;
  LARGE_TRANSFER_THRESHOLD = 64000;
type
  TProgressEx = record
    progress: TProgress;
    icon: PCommandIcon;
    isComplete: boolean;
    isBlocked: boolean;
    isRunning: boolean;
    isExecutingNow: boolean;
    isBlockedByDependsOn: boolean;
    isWaitingForResources: boolean;
  end;

  TThreadID = cardinal;
  ECommandException = class(Exception);

  TAsyncStage = (assStart, assFinish, assAll);
  TCommandProcessor = class; // forward
  TCPThreadPoolGrowthManager = class;//forward
  TcommandProcessorChildThread = class;

  TResourceStatRecord = record
    MaxLarge, MaxSmall: double;
  end;

  TResourceHealthData = class(TSharedObject)
  public
    Through: nativefloat;
    Time: nativefloat;
    MaxLarge, MaxSmall: nativefloat;
    Resource: string;
    procedure Init;override;
    procedure ApplyStat(Time, Through: nativefloat);
  end;

  TResourceHealthStats = class(TSharedObject)
  private
    function GEtStatByIndex(idx: integer): TResourceHealthData;
  protected
    FData: TStringList;
    function AddStat(sResourceName: string): TResourceHealthData;
    function IndexOfStat(sResourceName: string): nativeint;
    function FindStat(sResourceName: string): TResourceHealthData;
    function GetStatCount: nativeint;
    function ForceStat(sResourceName: string): TResourceHealthData;
    procedure Clear;
  public
    constructor Create;override;
    destructor Destroy;override;
    function GetStatForResource(sResourceName: string): TResourceHealthData;
    procedure ApplyResourceStat(sResourceName: string; Through, time: nativefloat);
    property Stats[idx: integer]: TResourceHealthData read GEtStatByIndex;
    property StatCount: nativeint read GetStatcount;
  end;

  TCommandResourceRecord = class(TBetterObject)
  private
    FName: string;
    FUsage: single;
  public
    property Name: string read FName write FName;
    property Usage: single read FUsage write FUsage;
  end;

  TCommandResources = class(TSharedObject)
  protected
    FCommandResources: TList<TCommandResourceREcord>;
    function IndexOfResource(s: string): NativeInt;
    function FindResource(s: string): TCommandResourceREcord;
  public
    procedure Init;override;
    destructor Destroy;override;
    procedure Clear;
    function GetResourceUsage(s: string): single;
    procedure SetResourceUsage(s: string; u: single);
    function GetResourceByIndex(i: NativeInt): TCommandResourceRecord;
    function  Count: NativeInt;
  end;

  TWaitableCommandList = class;//forward
  TCommand = class;//forward

  TCommandFinishedProc = reference to procedure (cmd: TCommand);
  TOnCommandFinished = procedure (c: TCommand) of object;


  TCommand = class(TSharedObject, IIndirectlyLinkable<TCommand>)
  strict
  private
    protected
    FLinkages: TDictionary<TObject, TLinkage<TCommand>>;
    FRaiseExceptions: boolean;
    FNameChanged_Volatile: boolean;
    FStatus: string;
    progress: TProgress;
    subprogress: TProgress;

    Fthread: TCommandProcessorChildThread;
    FName: string;

    FIsCancelled: boolean;

    FDependsOn, FWatchers: TList<TCommand>;
    FprocessLater: boolean;
    FOwnedByProcessor: boolean;
    FProcessor: TCommandProcessor;
    FThreadable: boolean;
    FAssigned: boolean;
    FTimeStarted: ticker;
    FTimeCompleted: ticker;
    FError: boolean;
    FErrorMessage: string;
    FIsExecutingNow: boolean;
    FCPUExpense: single;
    FNetworkExpense: single;
    FMemoryExpense: single;
    FMemoryExpenseGB: single;
    FFireForget: boolean;
    FRecommendedSleepTime: ticker;
    FFreeing: boolean;
    FWaitingForResources: boolean;
    FBlocked: boolean;
    FActiveThreadID: TThreadID;
    FStarted: boolean;
    FCompletelyFinished: boolean;
    FOKtoFree: boolean;
    FPriority: TBetterPriority;
    Fresult: boolean;
    FResources: TCommandResources;
    FBlockedByDependsOn: boolean;
    function GetStatus: string;
    function GetStep: NativeInt;inline;
    function GetStepCount: NativeInt;inline;
    function GetIsComplete: boolean;
    procedure SetIsComplete(const Value: boolean);
    function Getthread: TCommandProcessorChildThread;
    procedure SetThread(const Value: TCommandProcessorChildThread);

    procedure SetStatus(value: string);
    procedure SetStep(const Value: NativeInt);inline;
    procedure SetStepCount(const Value: NativeInt);inline;
    function GetProcessor: TCommandProcessor;
    procedure SetProcessor(const Value: TCommandProcessor);
    function GetOwnedByProcessor: boolean;
    procedure SetOwnedByProcessor(const Value: boolean);
    function GetTimeToComplete: ticker;
    function GetSubStep: NativeInt;
    function GetSubStepCount: NativeInt;
    procedure SetSubStep(const Value: NativeInt);
    procedure SetSubStepCount(const Value: NativeInt);
    function GEtPercentComplete: single;
    function GetIsCancelled: boolean;
    procedure SetIsCancelled(const Value: boolean);
    function GetIsExecutingNow: boolean;
    procedure SetIsExecutingNow(const Value: boolean);
    function GetIsConcluded: boolean;
    function GetCPUExpense: single;
    function GetNEtworkExpense: single;
    function GetMEmoryExpense: single;
    procedure SetCPUExpense(Value: single);
    procedure SetMemoryExpense(Value: single);
    procedure SetNetworkExpense(Value: single);
    procedure SetWaitingForResources(Value: boolean);

    function TryRemoveDependency(c: TCommand): boolean;
    function GetName: string;
    procedure SetName(value: string);
  private
    FAffinity: nativeint;
    FTimeOfCompletion: ticker;
    FRefCount: nativeint;
    FKey: string;
    FIcon: PCommandIcon;
    FFutureExecutionTime: ticker;
    FSelfDestructTime: ticker;
    FSelfDestructInitiated: boolean;
    FNoThreadvarWait: boolean;
    procedure SyncFinishGui;
    function GetAffinity: nativeint;
    procedure SetAffinity(const Value: nativeint);
    function GetDiskExpenseByFileNAme(sFile: string): nativefloat;
    procedure SetDiskExpenseByFileName(sFile: string; const Value: nativefloat);
    function getOptimizationGroup: string;
    procedure Setblocked(const Value: boolean);
    function GetTimeuntilExecute: ticker;
    function GEtMemoryExpenseGB: single;
    procedure SetMemoryExpenseGB(const Value: single);

  protected
    OptGRoupset: boolean;
    FOptimizationGRoup: string;
    procedure ResetOPtimzationGRoup;
    procedure InitOptimizationGRoup;virtual;
    property Processor: TCommandProcessor read GetProcessor write SetProcessor;

    procedure DoExecute; virtual; abstract;
  public
    evWait: TSignal;
    onFinish_Anon: TCommandFinishedProc;
    onFinish: TOnCommandFinished;
    onFinishGUI: TCommandFinishedProc;
    DontBlockGUI: boolean;//this is just a flag that the gui can use to decide to put up a spinny or not.
    procedure AddLinkage(link: TLInkage<TCommand>; list: TObject);
    function GetlinkageFor(obj: TObject): TLinkage<TCommand>;
    procedure RemoveLinkage(obj: TObject);
    procedure Detach; override;
    procedure SyncAffinity;
    constructor Create; override;
    procedure Init;override;
    destructor Destroy; override;
    procedure InitExpense; virtual;
    procedure PreProcess; virtual;

    procedure PostProcess; virtual;
    procedure Execute;

    property Icon: PCommandIcon read FIcon write FIcon;

    procedure NotifyProgress;
    property Status: string read GetStatus write SetStatus;
    property Step: NativeInt read GetStep write SetStep;
    property StepCount: NativeInt read GetStepCount write SetStepCount;
    property SubStep: NativeInt read GetSubStep write SetSubStep;
    property SubStepCount: NativeInt read GetSubStepCount write SetSubStepCount;
    property PercentComplete: single read GEtPercentComplete;

    property IsComplete: boolean read GetIsComplete write SetIsComplete;
    property IsCancelled: boolean read GetIsCancelled write SetIsCancelled;
    property CompletelyFinished: boolean read FCompletelyFinished;
    property IsExecutingNow: boolean read GetIsExecutingNow write
      SetIsExecutingNow;
    procedure RemoveFromProcessor;
    property OwnedByProcessor: boolean read GetOwnedByProcessor write
      SetOwnedByProcessor;

    procedure Process; overload;
    procedure Process(by: TCommandProcessor); overload;
    property Freeing: boolean read FFreeing write FFreeing;

    function Start: TCommand; overload;
    function Start(by: TCommandProcessor): TCommand; overload;
    function WaitFor(iUpToMS: nativeint = -1;cFrom: TCommand = nil): boolean;
    property NoThreadVarWait: boolean read FNoThreadvarWait write FNoThreadvarWait;
    property Threadable: boolean read FThreadable write FThreadable;
    property Running: boolean read FAssigned write FAssigned;
    property TimeStarted: ticker read FTimeStarted write FTimeStarted;
    property TimeCompleted: ticker read FTimeCompleted write FTimeCompleted;
    property TimeToComplete: ticker read GetTimeToComplete;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    property Error: boolean read FError write FError;
    procedure Cancel;virtual;
    procedure ProcessLater;
    procedure ContinueProcessing;
    property CPUExpense: single read GetCPUExpense write SetCPUExpense;
    property NetworkExpense
      : single read GetNEtworkExpense write SetNetworkExpense;
    property MemoryExpense: single read GetMEmoryExpense write SetMemoryExpense;
    property MemoryExpenseGB: single read GEtMemoryExpenseGB write SetMemoryExpenseGB;
    property OwnedByCommandProcessor
      : boolean read FOwnedByProcessor write FOwnedByProcessor;
    property Thread
      : TCommandProcessorChildThread read Getthread write SetThread;
    property FireForget: boolean read FFireForget write FFireForget;
    property IsConcluded: boolean read GetIsConcluded;
    // procedure WaitForResources;
    function WaitForResources(iUpToMS: NativeInt = -1): boolean;
    property Blocked: boolean read FBlocked write Setblocked;
    property BlockedByDependsOn: boolean read FBlockedByDependsOn write FBlockedByDependsOn;

    property WAitingForResources: boolean read FWaitingForResources write
      SetWaitingForResources;
    procedure AlertCommandChange(pr: TCommandProcessor);
    property ActiveThreadID: TThreadID read FActiveThreadID;

    procedure EndCommand;

    procedure AddDependency(c: TCommand);
    procedure AddWatcher(c: TCommand);
    procedure RemoveDependency(c: TCommand);
    procedure RemoveWatcher(c: TCommand);
    function AreDependsOnComplete: boolean;
    property Started: boolean read FStarted write FStarted;
    procedure StartChain; overload;
    procedure StartChain(by: TCommandProcessor); overload;
    property OkToFree: boolean read FOKtoFree write FOKtoFree;
    property Priority: TBetterPriority read FPriority write FPriority;
    property CResult: boolean read Fresult write Fresult;
    procedure WaitForAnotherCommand(c: TCommand; MirrorProgress: boolean = true);
    property Resources: TCommandResources read FResources write FResources;
    procedure RaiseErrors;
    property Name: string read GetName write SetName;
    property NameChanged_Volatile: boolean read FNameChanged_Volatile;
    property RaiseExceptions: boolean read FRaiseExceptions write FRaiseExceptions;
    property Affinity:nativeint read GetAffinity write SetAffinity;
    property TimeOfCompletion: ticker read FTimeOfCompletion;
    procedure AddRef;
    procedure ReleaseRef;
    property Key: string read FKey write FKey;
    procedure OnCancel;virtual;
    procedure WaitForCommandList(cl: TWaitableCommandList);
    property DiskExpenseByFileName[sFile: string]: nativefloat read GetDiskExpenseByFileNAme write SetDiskExpenseByFileName;
    property OptimizationGRoup: string read getOptimizationGroup;
    procedure Lock;override;
    procedure Unlock;override;
    function TryLock: boolean;override;
    function WatcherCount: ni;
    property FutureExecutionTime: ticker read FFutureExecutionTime write FFutureExecutionTime;
    property SelfDestructTime: ticker read FSelfDestructTime write FSelfDestructTime;
    property SelfDestructInitiated: boolean read FSelfDestructInitiated write FSelfDestructInitiated;
    property TimeUntilExecute: ticker read GetTimeuntilExecute;
    function GetProgress: TProgressEx;
    procedure SelfDestruct(iTime: ticker);
  end;

  TCommandList<T: TCommand> = class(TSharedList<T>)
  private
    function GEtPercentComplete: nativefloat;
  public
    property PercentComplete: nativefloat read GEtPercentComplete;
    function IsComplete: boolean;
    function FirstIncomplete: T;
    procedure WaitForAll;
    procedure WaitForAll_DestroyWhileWaiting;
    procedure ClearAndDestroyCommands;
    procedure RemoveCompleted;
    procedure DestroyCompleted;
    procedure CancelAll;
    procedure Startall;
  end;

  TCommandHolder = THolder<TCommand>;
  ICommandHolder = IHolder<TCommand>;
  TCommandHolderArray = array of ICommandHolder;



  TWaitableCommandList = class(TCommandList<TCommand>);

{$IFDEF USE_LINKED_LISTS_OF_COMMANDS}
  TLocalCommandLIst = TLInkedList<TCommand>;
{$ELSE}
{$IFDEF USE_CLUSTERED_LIST}
  TLocalCommandLIst = TClusteredPointerList<TCommand>;
{$ELSE}
  TLocalCommandLIst = TBetterList<TCommand>;
{$ENDIF}
{$ENDIF}


  TRolledUpCommand = class(TCommand)
  public
    procedure DoExecute; override;
  end;

  TEmptyCommand = class(TCommand)
  private
    FExecutionTime: ticker;
  public
    procedure InitExpense; override;
    procedure DoExecute; override;
    destructor Destroy; override;
    property ExecutionTime: ticker read FExecutionTime;
  end;

  TRandomCommand = class(TCommand)
  public
    procedure InitExpense; override;
    procedure DoExecute; override;
  end;

{$IFDEF ALLOW_FORGET}
  Tcmd_Forget = class(TCommand)
  private
    FCommandToForget: TCommand;
  public
    procedure Init;override;
    procedure InitExpense;override;
    procedure DoExecute;override;
    property CommandToForget: TCommand read FCommandToForget write FCommandToForget;
    class procedure Forget(c: TCommand);
  end;
{$ENDIF}

  TExceptionTestCommand = class(TCommand)
  public
    procedure DoExecute; override;
  end;

  TCommandProcessorChildThread = class(TProcessorThread)
  private
    Fcommand: TCommand;
    FParent: TCommandProcessor;
    FSleepLength: NativeInt;
    FTransitioning: boolean;
    function GetComand: TCommand;
    procedure SetCommand(const Value: TCommand);
    procedure SetParent(const Value: TCommandProcessor);

    property SleepLength: NativeInt read FSleepLength write FSleepLength;

  protected

    function GetStatus: string; override;
    function GetStepCount: NativeInt; override;
    function GetStep: NativeInt; override;
    function GetAutoSpin: boolean; override;
    function GEtSpin: boolean; override;
    function GetName: string;override;
    procedure Updatethreadinfo; override;
  public
    lastUsed: ticker;
    procedure InitFromPool;override;
    destructor Destroy; override;
    procedure DoExecute; override;
    procedure BuildMenu;override;
    procedure MenuAction(idx: ni);override;

    property Command: TCommand read GetComand write SetCommand;

    property Parent: TCommandProcessor read FParent write SetParent;
    property Transitioning: boolean read FTransitioning write FTransitioning;
    //function GetSignalDebug: string;override;

  end;

  TCommandProcessorMAinThread = class(TExternalEventthread)
  private
    FCP: TCommandProcessor;
    function GetCP: TCommandProcessor;
    procedure SetCP(const Value: TCommandProcessor);
  public
    property CP: TCommandProcessor read GetCP write SetCP;
  end;

  TCommandClass = class of TCommand;

  TCommandProcessorDebugState = class(TSharedObject)
  public
    volState: TArray<TProgressEx>;
  end;

  TCommandProcessor = class(TSharedObject)
  private
    rsGetCommand: TRingStats;
    FGetNextIncompleteCommand: TCommand;
    FSingleThreaded: boolean;
    FRecommendedSleepTime: ticker;
    FThrottle: NativeInt;
    FLAstSTarttime: ticker;
    FResourceScanTime: ticker;
    FOverspill: boolean;
    FResourceStats: TResourceHealthStats;
    FCycles: nativeint;
    LAstOptGRoup: string;
    OptTries: ni;
    procedure REsetOptTries;
    function GetCommandCount: NativeInt; // q
    function GetNextIncompleteCommand(var nextfuture: int64): TCommand; // q
    function Getcommand(idx: NativeInt): TCommand; // q
    function GetCommandIndex: NativeInt; // q
    function GetCompleteCount: NativeInt; // q
    function GetIsComplete: boolean; // q
    procedure SetChildThreadCount(const Value: NativeInt); // t
    function GEtOnWAitForAll: TNotifyEvent;
    procedure SetOnWaitForAll(const Value: TNotifyEvent);
    function GEtPercentComplete: single;
    function GetCancelled: boolean;
    procedure SetCancelled(const Value: boolean);
    function GetCurrentTotalCPUExpense: single;
    function GetCurrentTotalMemoryExpense: single;
    function GetCurrentTotalMemoryExpenseGB: single;
    function GetCurrentTotalNetworkExpense: single;
    function GetChildThreadLimit: NativeInt;
    procedure SetChildThreadLimit(const Value: NativeInt);
    procedure CleanStaleChildThreads;
    procedure CleanFreeCommands;
    function RunningCount: NativeInt;
    function GEtName: string;
    procedure SetName(value: string);
    function GEtPercentCompleteSimple: single;
    function GetPercentCompleteAccurate: single;
    procedure CommandDeactivated(c: TCommand);
    function GetMyThreadID: NativeInt;
    procedure GiveUpQuickThreads;
    function GetCompletecommand(idx: NativeInt): TCommand;
    procedure BuildVolatileProgress;


  protected
    FName: string;
    FCancelled: boolean;
    FSafety: boolean;
    F_atom_IncomingCount: nativeint;
    FCommandIndex, FCOmmandIndexFound: NativeInt;
    FOnWaitForAll: TNotifyEvent;

    FIncomingCommands: TLocalCommandList;
    FIncompleteCommands: TLocalCommandList;
    FActiveCommands: TLocalCommandList;
    FAllCommands: TLocalCommandList;

    FFreeCommands, FCompleteCommands: TLocalCommandList;

    FChildThreadLImit: NativeInt;
    Fthread: TCommandProcessorMAinThread;
    FThreadOwner: TThreadManager;

    FRaiseExceptions: boolean;
    FWaitIndex: NativeInt;
    FthreadCount: integer;
    FIdleSpins: nativeint;
{$IFDEF ENABLE_QUICKPOOL}
    FQuickPool: TCPThreadPoolGrowthManager;
{$Endif}
    sect_incoming: TCLXCriticalSection;
    FActiveThreads: TList<TCommandProcessorChildThread>;//<<---only used to determine how many threads are transitioning to the pool
    disallowSelfDestruct: boolean;
    procedure ProcessCompletedCommands;
    procedure StartThread;
    procedure StopThread;
  public
    volState: IHolder<TCommandProcessorDebugState>;
    procedure NotifyWatchers;
    procedure AssimilateIncoming;
    procedure SetPriority(c: TCommand);
    property RecommendedSleepTime
      : ticker read FRecommendedSleepTime write FRecommendedSleepTime;

    procedure RollupCompletedCommands;
    constructor Create(Threadowner: TThreadManager; sName: string);
      reintroduce; virtual;
    destructor Destroy; override;

    procedure AddCommand(cmd: TCommand; bTakeOwnerShip: boolean = false);
    procedure RemoveCommand(cmd: TCommand;
      bFreeIfOwnedByProcessor: boolean = true);
    procedure DeleteCommand(T: NativeInt);
    procedure MoveToEnd(cmd: TCommand);
    procedure AdjustAffinitys;
    procedure OnThreadExecute(sender: TExternalEventthread);
    procedure OnThreadDetach(sender: TExternalEventthread);
    property SingleThreaded: boolean read FSingleThreaded write FSingleThreaded;
    procedure ProcessSingleThreaded;
    function ResourcesAvailable(c: TCommand): boolean;
    property Overspill: boolean read FOverspill write FOverspill;
    procedure CommandProgress(sender: TCommand);
    procedure Clear;
    property Commands[idx: NativeInt]: TCommand read Getcommand;
    property CompleteCommands[idx: NativeInt]: TCommand read GetCompletecommand;
    property CommandCount: NativeInt read GetCommandCount;
    property CommandIndex: NativeInt read GetCommandIndex;
    property OnWaitForAll: TNotifyEvent read GEtOnWAitForAll write
      SetOnWaitForAll;
    // note that this is a SEPARATE thread from the main thread so events can only trigger postmessages

    procedure CheckIfShouldSTopThread;
    procedure WaitForAll(exceptFor: TCommand = nil);
    property CompleteCount: NativeInt read GetCompleteCount;
    property IsComplete: boolean read GetIsComplete;

    function Needthread: TCommandProcessorChildThread;
    procedure NoNeedThread(thr: TCommandProcessorChildThread);
    property CurrentTotalCPUExpense: single read GetCurrentTotalCPUExpense;
    property CurrentTotalNetworkExpense
      : single read GetCurrentTotalNetworkExpense;
    property CurrentTotalMemoryExpense: single read GetCurrentTotalMemoryExpense;

    function HasCommand(c: TCommandClass): boolean;

    property PercentComplete: single read GEtPercentComplete;
    procedure BeforeCheckCommands; virtual;
    property Cancelled: boolean read GetCancelled write SetCancelled;
    procedure CancelAll;

    property ChildThreadLimit: NativeInt read GetChildThreadLimit write
      SetChildThreadLimit;
    property RaiseExceptions
      : boolean read FRaiseExceptions write FRaiseExceptions;

    procedure WaitForRunningCommands;
    procedure Detach; override;
    procedure DetachCommands;
    procedure RemoveSelfDestructCommands;
    procedure CommandDone(c: TCommand);
    procedure ThreadDone(thr: TCommandProcessorChildThread);
    procedure DebugThreadCounts;
    property Name: string read GEtName write SetName;
    property Throttle: NativeInt read FThrottle write FThrottle;
    property LAstSTartTime: ticker read FLAstSTarttime write FLAstSTarttime;
    property MyThreadID: NativeInt read GetMyThreadID;
    property ResourceScanTime: ticker read FResourceScanTime write FResourceScanTime;
    function GetCurrenttotalCustomResourceExpense(s: string): single;
    function GetCurrentScheduledCustomResourceExpense(s: string): single;
    property ResourceStats: TResourceHealthStats read FResourceStats;
    function TryLockIncoming: boolean;
    procedure LockIncoming;
    procedure UnlockIncoming;
    property Cycles: nativeint read FCycles;
    function GetCyclesSince(var i: nativeint): nativeint;

    procedure Lock;override;
    procedure Unlock;override;
    function TryLock: boolean;override;

  end;

  TLLL = TDirectlyLinkedList<TCommandProcessorChildThread>;

  TCPThreadPoolGrowthManager = class(TManagedThread)
  private
    FIdleCount: ni;
    tmLastNeedtime: ticker;
    FList: TLLL;
    function GetIdleCount: ni;
  protected
    procedure DoExecute; override;
  public
    procedure Detach; override;
    procedure GiveUpThreads;
    procedure CleanStale;
    procedure NoNeedThread(thr: TCommandProcessorChildThread);
    function NeedThread: TCommandProcessorChildThread;
    procedure Init; override;
    procedure InitFromPool; override;

    property IdleCount: ni read GetIdleCount;


  end;

  TSharedObjectWithCommandProcessor = class(TSharedObject)
  private
    function GeTCommandProcessor: TCommandProcessor;
  protected
    FCP: TCommandProcessor;
  public
    constructor Create; override;
    destructor Destroy; override;
    property CP: TCommandProcessor read GeTCommandProcessor;

  end;


procedure CmdPerfLog(s: string);inline;

{$IFDEF SUPPORTS_APC}
type
  TAPCCall = procedure(l: pointer); {$IFDEF STDCALL}stdcall;{$ENDIF}
{$ENDIF}

// var
// ChildThreadPool: TCommandProcessorChildThreadPool;
threadvar threadCommand: TCommand;
  var thread_debug_tag: THandle;

procedure SetCommandStatus(s: string);
procedure WAitForCommandBlind(c: TCommand);

var
  GBGCommands: TCommandProcessor;
  lockthr: int64;
  lockobj:  pointer;

function BGCmd: TCommandProcessor;

implementation

uses
  commands_system;

procedure CmdPerfLog(s: string);
begin
{$IFDEF PERFORMANCE_LOGGING}
  Debug.Log(s, 'CommandPerformance');
{$ENDIF}
end;

function BGCmd: TCommandProcessor;
begin
  if GBGCommands = nil then
    GBGCommands := TCommandProcessor.create(nil, 'Background Commands');

  result := GBGCommands;
end;
// InternalThreadFunction(lpThreadParameter: Pointer): NativeInt; stdcall;
function AlertCommandDone(l: pointer): NativeInt; {$IFDEF STDCALL}stdcall;{$ENDIF}
begin
  // windows.beep(100,100);
  Result := 0;
end;

function AlertCommandChange(l: pointer): NativeInt; {$IFDEF STDCALL}stdcall;{$ENDIF}
begin
  // windows.beep(100,100);
  Result := 0;
end;

{ Tcommand }

function TCommand.GetOwnedByProcessor: boolean;
begin
  Lock;
  try
    Result := FOwnedByProcessor;
  finally
    Unlock;
  end;
end;

function TCommand.GEtPercentComplete: single;
var
  rSubComplete: nativefloat;
  rStepSize: nativefloat;
  rr: nativefloat;
  a,b,c,d: nativefloat;
begin
  a := Step;
  b := StepCount;
  c := SubStep;
  d := SubStepCount;
  if b = 0 then
    b := 1;



  rStepSize := 1 / b;
//  rComplete := a/b;
  if d > 0 then
    rSubComplete := c/d
  else
    rSubComplete := 0;

   rr := ((a * rStepSize) + (rSubComplete * rStepSize));

  result := rr;
  if result > 1 then result := 1;
  if result < 0 then result := 0;


exit;
  Lock;
  try
//    tmStart := tickcount.GetTicker;
    if IsComplete then
      Result := 1
    else begin
      if StepCount = 0 then
        Result := 0
      else begin

        if SubStepCount = 0 then
          rSubComplete := 0
        else begin
          rSubComplete := SubStep / SubStepCount;
        end;

//        rComplete := Step / StepCount;

        rStepSize := 1 / StepCount;

        rr := ((Step * rStepSize) + (rSubComplete * rStepSize));

        Result := rr;
        if Result > 1 then
          Result := 1;
      end;
    end;
  finally
    Unlock;
  end;
end;

//function TCommand.GetPrev: TCommand;
//begin
//  result := FPrev;
//end;

function TCommand.GetProcessor: TCommandProcessor;
begin
  Lock;
  try
    Result := FProcessor;
  finally
    Unlock;
  end;
end;

function TCommand.GetProgress: TProgressEx;
begin
  result.icon := self.icon;
  result.progress := self.progress;
  result.isComplete := self.IsComplete;
  result.isBlocked := self.Blocked;
  result.isRunning := self.Running;
  result.isExecutingNow := self.IsExecutingNow;
  result.isBlockedByDependsOn := self.BlockedByDependsOn;
  result.isWaitingForResources := self.WAitingForResources;
end;

function TCommand.GetStatus: string;
begin
  Lock;
  try
    Result := FStatus;
  finally
    Unlock;
  end;
end;

function TCommand.GetStep: NativeInt;
begin
//  Lock;
//  try
    Result := progress.step;
//  finally
//    Unlock;
//  end;

end;

function TCommand.GetStepCount: NativeInt;
begin
//  Lock;
//  try
    Result := progress.stepcount;
//  finally
//    Unlock;
//  end;

end;

function TCommand.GetSubStep: NativeInt;
begin
//  Lock;
//  try
    Result := subprogress.Step;
//  finally
//    Unlock;
//  end;
end;

function TCommand.GetSubStepCount: NativeInt;
begin
//  Lock;
//  try
    Result := subprogress.StepCount;
//  finally
//    Unlock;
//  end;

end;

function TCommand.GetTimeToComplete: ticker;
begin
  Result := GEtTimeSInce(TimeCompleted, TimeStarted);
end;

function TCommand.GetTimeuntilExecute: ticker;
begin
  if FFutureExecutionTime = 0 then
    exit(0)
  else
    result := greaterof(0,gettimesince(int64(FFutureExecutionTime), int64(GetTicker())));

  if result > (1000*60*60*24) then
    result := 0;
end;

procedure TCommand.Init;
begin
  inherited;
  FIcon := @CMD_ICON_DEFAULT;

end;


procedure TCommand.InitExpense;
begin
  FThreadable := true;
  FCPUExpense := 1.0;
//  FMemoryExpense := 1/16;

end;

procedure TCommand.InitOptimizationGRoup;
begin
  FOptimizationGroup := '';
end;

procedure TCommand.Lock;
begin
//  Debug.Log(self, 'Blocking for lock');
  inherited;
//  Debug.Log(self, 'locked');

end;

procedure TCommand.NotifyProgress;
begin
  Lock;
  try
    if Assigned(Processor) then begin
      Processor.CommandProgress(self);

    end;
  finally
    Unlock;
  end;
end;

procedure TCommand.OnCancel;
begin
  //no imp required
end;

procedure TCommand.Process;
begin
  OkToFree := false;
  FprocessLater := false;
  FProcessor := BGCmd;
  if IsComplete then begin
    BGCmd.RemoveCommand(self);
    Started := false;
    IsComplete := false;
    FCompletelyFinished := false;
  end;

  BGCmd.AddCommand(self, OwnedByCommandProcessor);
  Started := true;
end;

procedure TCommand.PostProcess;
begin
  //no imp
end;

procedure TCommand.PreProcess;
begin
  //no imp
end;

procedure TCommand.Process(by: TCommandProcessor);
begin
  OkToFree := false;
  FprocessLater := false;
  if not Assigned(by) then
    by := BGCmd;
  if Assigned(by) then begin
    FProcessor := by;

    if IsComplete then begin
      IsComplete := false;
      by.RemoveCommand(self);
      FCompletelyFinished := false;
    end;
    by.AddCommand(self, OwnedByCommandProcessor);
  end;
  Started := true;

end;

procedure TCommand.ProcessLater;
var
  b: boolean;
  p: TCommandProcessor;
begin
  b := false;
  while not b do begin
    p := self.processor;
    p.Lock;
    try
      b := self.TryLock;
      if b then
        try
          p.MoveToEnd(self);
          FprocessLater := true;

        finally
          self.Unlock;
        end;
    finally
      p.Unlock;
    end;
{$IFDEF DO_SLEEPEX}
    if not b then
      sleepex(THREAD_SLEEP, true);
{$ENDIF}
  end;
end;

procedure TCommand.RaiseErrors;
begin
  if Error then begin
     raise ECommandException.create(self.ClassName+' encountered an exception: '+ErrorMessage);
  end;
end;

procedure TCommand.ReleaseRef;
begin
  Lock;
  try
    dec(FRefCount);
  finally
    Unlock;
  end;
end;

procedure TCommand.RemoveDependency(c: TCommand);
begin
  if c = nil then
    exit;
  Lock;
  try
    FDependsOn.remove(c);
    c.RemoveWatcher(self);
  finally
    Unlock;
  end;
end;

function TCommand.TryLock: boolean;
begin
//  Debug.Log(self, 'try lock');
  result := Inherited;
//  if result then begin
//    Debug.Log(self,'locked (try)');
//  end else
//    Debug.Log(self, 'try lock FAILED');
end;

function TCommand.TryRemoveDependency(c: TCommand): boolean;
begin
  result := false;
  if c = nil then
    exit;
  if not TryLock then
    exit
  else
  try
    FDependsOn.remove(c);
    c.RemoveWatcher(self);
    result := true;
  finally
    Unlock;
  end;
end;


procedure TCommand.Unlock;
begin
//  Debug.Log(self, 'unlocking');
  inherited;

end;

procedure TCommand.RemoveFromProcessor;
var
  p: TCommandProcessor;
begin
  if Assigned(Processor) then begin
    p := processor;
    p.Lock;
    try
      if Assigned(Processor) then begin
        //Processor.RemoveCommand(self);
        processor := nil;
      end;
    finally
      p.Unlock;
    end;
  end;
end;

procedure TCommand.RemoveLinkage(obj: TObject);
begin
  FLinkages.Remove(obj);

end;

procedure TCommand.RemoveWatcher(c: TCommand);
begin
  if c = nil then
    exit;
  Lock;
  try
    FWatchers.remove(c);
  finally
    Unlock;
  end;

end;

procedure TCommand.REsetOPtimzationGRoup;
begin
  OptGroupSet := false;
  InitOptimizationGRoup;
end;

procedure TCommand.AddDependency(c: TCommand);
begin
  if c = nil then
    exit;

  Lock;
  try
    if c.FireForget and c.Started then
      raise exception.Create(
        'You cannot add a fire-forget command as a dependent once it has been started.');

    FDependsOn.add(c);
//    Debug.Log('AddDependent '+ClassName+' @'+inttohex(nativeint(pointer(self)), sizeof(pointer)*2)+' has '+inttostr(FWatchers.count)+' watchers and '+inttostr(FDependsOn.count)+' DependsOn');
    c.AddWatcher(self);
  finally
    Unlock;
  end;
end;

procedure TCommand.AddLinkage(link: TLInkage<TCommand>; list: TObject);
begin
  FLinkages.Add(list, link);
end;

procedure TCommand.AddRef;
begin
  Lock;
  try
    inc(FRefCount);
  finally
    Unlock;
  end;
end;

procedure TCommand.AddWatcher(c: TCommand);
begin
  if c = nil then
    exit;
  Lock;
  try

    FWatchers.add(c);
//    Debug.Log('AddWatcher '+ClassName+' @'+inttohex(nativeint(pointer(self)), sizeof(pointer)*2)+' has '+inttostr(FWatchers.count)+' watchers and '+inttostr(FDependents.count)+' dependents');
  finally
    Unlock;
  end;
end;

procedure TCommand.AlertCommandChange(pr: TCommandProcessor);
begin

{$IFDEF SUPPORTS_APC}
  if Assigned(pr) then begin
    if pr.TryLock then
      try
        if Assigned(pr.Fthread) then
          QueueUserApc(@AlertCommandDone, pr.Fthread.handle, 0);
      finally
        pr.Unlock;
      end;
  end;
{$ENDIF}
end;

function TCommand.AreDependsOnComplete: boolean;
var
  T: NativeInt;
begin
  Lock;
  try
    Result := true;
    for T := 0 to FDependsOn.count - 1 do begin
      if not FDependsOn[T].IsComplete then begin
        Result := false;
        break;
      end;
    end;
  finally
    Unlock;
  end;

  blockedByDependsOn := not result;
//  blocked := result;

end;

procedure TCommandProcessor.BuildVolatileProgress;
var
  t: ni;
  vs: TCommandProcessorDebugState;
begin
  if trylock then
  try
    vs := volState.o;
    if vs.TryLock then
    try
      setLength(vs.volState, FAllCommands.count);
      for t:= 0 to high(vs.volState) do begin
        vs.volState[t] := FAllCommands[t].GetProgress;
      end;
    finally
      vs.unlock;
    end;
  finally
    unlock;
  end;
end;

procedure TCommand.Cancel;
begin
  Lock;
  try
    if not IsComplete then begin
      IsCancelled := true;
      OnCancel;
      if Thread <> nil then
        Debug.Log(self,'Cancelling an already assigned command');
    end;

  finally
    Unlock;
  end;
end;


procedure TCommand.ContinueProcessing;
begin

  FprocessLater := false;
end;

constructor TCommand.Create;
begin
  inherited;
  evWait := Tsignal.create;
  FLinkages := TDictionary<TObject, TLinkage<TCommand>>.create;

  {$IFDEF RAISE_EXCEPTIONS_FROM_COMMANDS_ON_WAITFOR}
  FRaiseExceptions := true;
  {$ENDIF}

  Faffinity := -1;
  FName := self.ClassName;
  Step := 0;
  StepCount := 1;
  Threadable := true;
  fresources := TCommandResources.Create;
  InitExpense;
  FActiveThreadID := cardinal(-1);
  FDependsOn := TList<TCommand>.Create;
  FWatchers := TList<TCommand>.Create;
  FOKtoFree := true;
{$IFDEF WINDOWS}
  Priority := TBetterPriority.bpLower;
{$ENDIF}


// Debug.Log('Command created '+self.classname, 'command');
end;

procedure TCommand.Detach;
begin
  try
  if detached then
    exit;

  FraiseExceptions := false;
  while true do begin

//    if not TryLock then
//      sleep(1)
//    else
    Lock;
    try
//      Debug.Log(self, 'Detach '+ClassName+' @'+inttohex(nativeint(pointer(self)), sizeof(pointer)*2)+' has '+inttostr(FWatchers.count)+' watchers and '+inttostr(FDependsOn.count)+' DependsOn');
      if assigned(FWatchers)
      and (FWatchers.count > 0) then begin
        FWatchers[0].TryRemoveDependency(self);
      end
      else begin
        break;
      end;
    finally
      Unlock;
    end;
  end;

  while true do begin
    if not TryLock then
      sleep(1)
    else
    try
//      Debug.Log(self, 'Detach '+ClassName+' @'+inttohex(nativeint(pointer(self)), sizeof(pointer)*2)+' has '+inttostr(FWatchers.count)+' watchers and '+inttostr(FDependsOn.count)+' DependsOn');
      if assigned(FDependsOn)
      and (FDependsOn.count > 0) then begin
        TryRemoveDependency(FDependsOn[0]);
      end
      else begin
        break;
      end;
    finally
      Unlock;
    end;
  end;

  finally
    Self.Processor := nil;
  end;

  inherited;


end;

destructor TCommand.Destroy;
begin
//  Debug.Log(self,'In Command destroy '+self.classname, 'command');
  while not OkToFree do
    sleep(0);

//  Debug.Log(self,'OkToFree '+self.classname, 'command');
  while thread<> nil do
    sleep(0);
//  Debug.Log(self,'Thread is clear '+self.classname, 'command');

  if Freeing then
    exit;

//  Debug.Log(self,'Checking if running '+self.classname, 'command');
  Freeing := true;
  WHILE (Running or IsExecutingNow and (not CompletelyFinished)) do
    sleepex(THREAD_SLEEP, true);

//  Debug.Log(self,'Completely finished '+self.classname, 'command');
//  Lock;
  try
    if not Detached then begin
//      Debug.Log(self,'NOT DETACHED, detaching '+self.classname, 'command');
      Detach;
    end;
    FDependsOn.free;
    FWatchers.free;

    if Assigned(Thread) then
      raise EBasicException.Create('Destroying a command that is not detached');
    if assigned(processor) then
//      Debug.Log(self,'Processor still assigned, maybe dangerous '+self.classname, 'command');
    self.Processor := nil;

  finally
//    Unlock;
  end;

  fResources.Free;
  FLinkages.free;
  evWait.free;
  evWait := nil;
  inherited;
//  Debug.Log(self,'Command destroyed '+self.classname, 'command');
end;

// ------------------------------------------------------------------------------
procedure TCommand.EndCommand;
begin
  WaitFor;
{$IFDEF IOS}
  free;
//  detach;
{$ELSE}
  free;
{$ENDIF}

end;

procedure TCommand.Execute;
var
  thr: TCommandProcessorChildThread;
  hadprocessor: boolean;
  p: TCommandProcessor;
begin
  try
    hadprocessor := false;
    if self = nil then
      exit;
    try
      self.CResult := true;
      FprocessLater := false;
      IsExecutingNow := true;
      if assigned(self.Thread) then
        self.Thread.Transitioning := false;


      try

        try
          threadCommand := self;
          FActiveThreadID := GetCurrentThreadID();
  {$IFDEF WINDOWS}
          //SetThreadPriority(FActiveThreadID, self.Priority);
  {$ENDIF}
  //        SyncAffinity;
          PreProcess;
          if not IsCancelled then begin
            DoExecute;
            PostProcess;
            if assigned(onFinish_Anon) then
              onFinish_Anon(self);
            if assigned(onFinish) then
              onFinish(self);
            if assigned(onFinishGUI) then begin
              TThread.Synchronize(TThread.CurrentThread, SyncFinishGUI);
            end;

          end;
          threadcommand := nil;
          FCompletelyFinished := true;
          FTimeOfCompletion := tickcount.GetTicker;
        finally
  //        IsComplete := true;
          threadCommand := nil;
        end;

      except
        on E: exception do begin
          self.Cresult := false;
          Debug.Log(self,CLRE+'Error Executing command ' + self.ClassName + '... ' +
              E.message);
          Error := true;
          ErrorMessage := E.message;
          Status := Status + E.Message;
        end;
      end;
    finally

      thr := self.Thread;

  //    self.Thread := nil;
  //    if Assigned(thr) then
  //      thr.Command := nil; // <--HEY! THIS HAS TO BE DONE **BEFORE** marking the command COMPLETE

      Step := StepCount;

      // CAREFUL HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

//      sleep(8000);///xxx - force failures
      IsExecutingNow := false;
      p := processor;
      if assigned(p) then begin
        p.Lock;
        hadprocessor := true;
      end;
      try
        try
          try
            {$IFDEF ALLOW_EARLY_THREADDONE_SIGNAL}
              thr.Parent.ThreadDone(thr);
            {$ENDIF}
            Lock;
            try
              OkToFree := false;
              Running := false;
            finally
              Unlock;
            end;

            if Assigned(p) then begin
              p.lock;
              try

                p.CommandDeactivated(self);
              finally
                p.Unlock;
              end;
            end
            else begin
              if hadprocessor then
                raise EBasicException.Create('Processor was pulled');
            end;


            IsComplete := not FProcessLater;//dead lock risk
            //AlertCommandChange(FProcessor);
          finally
            if assigned(p) then
              p.Unlock;
          end;
        finally
          IsExecutingNow := false;
        end;
      finally
        OkToFree := true;

        //IsComplete := not FProcessLater;//dead lock risk
      end;
      // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
      OkToFree := true;
    end;
    OkToFree := true;
  except
    on E: Exception do begin
      Status := 'Uncontrolled Exception: '+e.message;
    end;
  end;




end;

procedure TCommand.SyncFinishGui;
begin
  onFinishGUI(self);
end;

// ------------------------------------------------------------------------------
function TCommand.GetAffinity: nativeint;
begin
  result := FAffinity;
end;


function TCommand.GetCPUExpense: single;
begin
  Result := FCPUExpense;
end;



function TCommand.GetDiskExpenseByFileNAme(sFile: string): nativefloat;
begin
  result := Resources.GetResourceUsage(ExtractNetworkRoot(sFile));

end;

function TCommand.GetName: string;
begin
  Lock;
  try
    result := Fname;
    FNameChanged_Volatile := false;
  finally
    Unlock;
  end;
end;

function TCommand.GetNEtworkExpense: single;
begin
  Result := FNetworkExpense;
end;
function TCommand.getOptimizationGroup: string;
begin
  lock;
  try
    if not OptGroupSet then begin
      InitOptimizationGRoup;
      OptGroupSet := true;
    end;
    result := FOptimizationGRoup;
  finally
    Unlock;
  end;
end;

//
//function TCommand.GetNext: TCommand;
//begin
//  result := FNext;
//end;

function TCommand.GetIsCancelled: boolean;
begin
  Lock;
  try
    Result := FIsCancelled;
  finally
    Unlock;
  end;
end;

function TCommand.GetIsComplete: boolean;
begin
  Lock;
  try
    Result := IsSignaled(evWait);
  finally
    Unlock;
  end;
end;

function TCommand.GetIsConcluded: boolean;
begin
  if TryLock then
    try
      Result := IsComplete;
      if not Result then
        Result := IsCancelled and not Running;

    finally
      Unlock;
    end
  else
    Result := false;

end;

function TCommand.GetIsExecutingNow: boolean;
begin
  Lock;
  try
    Result := FIsExecutingNow;
  finally
    Unlock;
  end;
end;

function TCommand.GetlinkageFor(obj: TObject): TLinkage<TCommand>;
begin
  if FLInkages.ContainsKey(obj) then
    result := FLinkages.Items[obj]
  else
    result := nil;



end;

function TCommand.GetMEmoryExpense: single;
begin
  Result := FMemoryExpense;
end;

function TCommand.GEtMemoryExpenseGB: single;
begin
  result := FMemoryExpenseGB
end;

procedure TCommand.SetAffinity(const Value: nativeint);
begin
  FAffinity := value;
  SyncAffinity;
end;

procedure TCommand.Setblocked(const Value: boolean);
begin
//  Debug.log(self, 'blocked = '+booltostrex(value));
  FBlocked := Value;
end;

procedure TCommand.SetCPUExpense(Value: single);
begin
  if Value > GetEnabledCPUCount then
    Value := GetEnabledCPUCount;

  FCPUExpense := Value;
  AlertCommandChange(FProcessor);
end;




procedure TCommand.SetDiskExpenseByFileName(sFile: string;
  const Value: nativefloat);
begin
  Resources.SetResourceUsage(ExtractNetworkRoot(sFile), value);
end;

procedure TCommand.SetIsCancelled(const Value: boolean);
begin
  Lock;
  try
    FIsCancelled := Value;
  finally
    Unlock;
  end;

end;

procedure TCommand.SetIsComplete(const Value: boolean);
begin
  Lock;
  try
    Step := StepCount;
  finally
    Unlock;
  end;

  Signal(evWait,value);
  if (Value) then begin
    if assigned(processor) then
      Processor.CommandDone(self);
  end;
  if evWait.IsSignaled and (not value) then
    raise ECRitical.create('Unsignalling a signaled command, this is probably bad.');





end;

procedure TCommand.SetIsExecutingNow(const Value: boolean);
begin
  Lock;
  try
    FIsExecutingNow := Value;
  finally
    Unlock;
  end;
end;

procedure TCommand.SetMemoryExpense(Value: single);
begin
  if Value > 1.0 then
    Value := 1.0;

  FMemoryExpense := Value;
  AlertCommandChange(FProcessor);
end;

procedure TCommand.SetMemoryExpenseGB(const Value: single);
begin
  FMemoryExpenseGB := value;

end;

procedure TCommand.SetName(value: string);
begin
  Lock;
  try
    Fname := value;
    FNameChanged_Volatile := true;
  finally
    Unlock;
  end;
end;

procedure TCommand.SetNetworkExpense(Value: single);
begin
  if Value > 1.0 then
    Value := 1.0;
  FNetworkExpense := Value;

  AlertCommandChange(FProcessor);
end;

//procedure TCommand.SetNext(const obj: TCommand);
//begin
//  FNext := obj;
//end;

procedure TCommand.SetOwnedByProcessor(const Value: boolean);
begin
  Lock;
  try
    FOwnedByProcessor := Value;
  finally
    Unlock;
  end;
end;

//procedure TCommand.SetPrev(const obj: TCommand);
//begin
//  FPrev := obj;
//end;

procedure TCommand.SetProcessor(const Value: TCommandProcessor);
var
  pr: TCommandProcessor;
  bGood: boolean;
begin
  bGood := false;
  pr := nil;
  if Value = FProcessor then
    exit;

  if Assigned(FProcessor) then begin
    bGood := false;
    while not bGood do begin
      if not FProcessor.TryLock then begin
        sleep(1);
        continue;
      end;
      try

        pr := FProcessor;
        if not TryLock then
          continue;
        try
          bGood := true;
          FProcessor := nil;
          pr.RemoveCommand(self, false);

        finally
          Unlock;
        end;
      finally
        if Assigned(pr) then
          pr.Unlock;
      end;
    end;
    if assigned(pr) then
      AlertCommandChange(pr);
  end;

  if Assigned(Value) then
    try
      Value.Lock;
      Lock;
      try

        FProcessor := Value;
        if FProcessor <> nil then
          FProcessor.AddCommand(self);

      finally
        Unlock;
      end;
    finally
      if Assigned(Value) then
        Value.Unlock;
    end;


  AlertCommandChange(FProcessor);

end;

procedure TCommand.SetStatus(value: string);
begin
  Lock;
  try
    FStatus := Value;
    UniqueString(FStatus);
  finally
    Unlock;
  end;

end;

procedure TCommand.SetStep(const Value: NativeInt);
begin
//  Lock;
//  try
    progress.step := Value;
//  if assigned(Thread) then
//    thread.step := value;
//  finally
//    Unlock;
//  end;

end;

procedure TCommand.SetStepCount(const Value: NativeInt);
begin
//  Lock;
//  try
    progress.StepCount := Value;
//    if assigned(Thread) then
//      thread.stepcount := value;
//  finally
//    Unlock;
//  end;

end;

procedure TCommand.SetSubStep(const Value: NativeInt);
begin
//  Lock;
//  try
    subprogress.Step := Value;
//  finally
//    Unlock;
//  end;

end;

procedure TCommand.SetSubStepCount(const Value: NativeInt);
begin
//  Lock;
//  try
    subprogress.StepCount := Value;
//  finally
//    Unlock;
//  end;

end;

function TCommand.Start(by: TCommandProcessor): TCommand;
begin
  Process(by);
  result := self;
end;

function TCommand.Start: TCommand;
begin
  Process;
  result := self;
end;

procedure TCommand.StartChain(by: TCommandProcessor);
var
  T: NativeInt;
begin
  Lock;
  try
    if Started then exit;

    for T := 0 to FDependsOn.count - 1 do begin
      FDependsOn[T].StartChain(by);
    end;
    Start(by);
  finally
    Unlock;
  end;
end;

procedure TCommand.SyncAffinity;
begin
{$IFDEF WINDOWS}
  if CPuExpense >= 1.0 then
    if assigned(thread) then
      SetThreadAffinityMask(thread.ThreadID, FAffinity);
{$ENDIF}
end;

procedure TCommand.StartChain;
begin
  StartChain(nil);
end;

function TCommand.WaitFor(iUpToMS: nativeint = -1;cFrom: TCommand = nil): boolean;
var
  bFreeAfterWait: boolean;
  tmStart: ticker;
  tm: ticker;
  x: ni;
begin
{$IFDEF USE_THREADVARS_FOR_WAITING}
  if (cFrom=nil) and (threadcommand<> nil) and (not noThreadvarWait) then begin
//    debug.log(self, threadcommand.classname+' was already running in this thread, auto-waiting from other command');
    result := WAitFor(iUpToMS, threadcommand);
    exit;
  end;
{$ENDIF}

  if assigned(cFrom) then
    cFrom.blocked := true;
  try
    bFreeAfterWait := false;
    if fireforget then begin
      fireforget := false;
      bFreeAfterwait := true;
    end;

//    debug.log(self, 'waiting on evWait');
    result := false;
    tm := GEtTicker;
    x := iUpToMS;
    if x<0 then
      x := 4000;
    while not result do begin
//      if IsCancelled then begin
//        exit(false);
//      end;
      result := WaitForSignal(evWait, lesserof(x-GetTimeSince(tm),4000));
      if (iUpToMS >=0) and (GetTimeSince(tm) > iUpToMs) then
        break;
    end;
//    debug.log(self, 'got evWait signal');
    if result then begin
      if RaiseExceptions then begin
        RaiseErrors;
        FraiseExceptions := false;
      end;

{$IFDEF STUUPID}
      if bFreeAfterWait then
        {$IFDEF IOS}
        detach;
        {$ELSE}
        free;
        {$ENDIF}
{$ENDIF}
    end else begin
//      raise ECritical.create('Wait failed in '+self.classname);
    end;
  finally
    if assigned(cFrom) then
      cFrom.blocked := false;
  end;




exit;
  result := true;
  // if we attempt to wait on a fireforget command
  // (as would happen if we wait on the whole queue0
  // fire forget commands must be converted back into non-fireforget commands
  // then freed after waiting
  tmStart := tickcount.GetTicker;
  bFreeAfterWait := false;
  Lock;
  try
    if FireForget then begin
      FireForget := false;
      bFreeAfterWait := true;
    end;
  finally
    Unlock;
  end;

  while true do begin
    while true do begin
      if IsComplete then
        break;
      if IsCancelled and not Running then begin
        exit;
      end;
  {$IFDEF DO_SLEEPEX}
      sleepex(THREAD_SLEEP, true);
  {$ENDIF}
      //Processor.SetPriority(self);
      if (iUpToMS > -1) and (GEtTimeSince(tmStart) > ticker(iUPToMS)) then begin
        result := false;
        exit;
      end;

    end;
    if thread=nil then
      break;

  {$IFDEF DO_SLEEPEX}
      sleepex(THREAD_SLEEP, true);
  {$ENDIF}


  end;


  if RaiseExceptions then
    RaiseErrors;


  if bFreeAfterWait then
    {$IFDEF IOS}
    detach;
    {$ELSE}
    free;
    {$ENDIF}

  result := true;
end;

procedure TCommand.WaitForAnotherCommand(c: TCommand; MirrorProgress: boolean = true);
var
  osc, os: nativeint;
begin
  blocked := true;
  try


    if MirrorProgress then begin
      os := Step;//<---- save for later
      osc := StepCount;//<<--- save for later
      while not c.WaitFor(100) do begin
        self.Status := c.Status;
        if c.StepCount > 1 then begin
          self.Step := c.Step;
          self.StepCount := c.StepCount;
        end;
      end;
      Step := os;
      StepCount := osc;
    end;
    c.WaitFor(-1,self);
  finally
    blocked := false;
  end;

end;

procedure TCommand.WaitForCommandList(cl: TWaitableCommandList);
var
  c: TCommand;
  ccl:TCommandList<TCommand>;
begin
  ccl := TcommandList<TCommand>(cl);
  while not ccl.IsComplete do begin
    StepCount := 100;
    Step := round(ccl.PercentComplete * 100);
    c := ccl.FirstIncomplete;
    if c <> nil then begin
      Status := c.Status;
      SubStep := c.Step;
      SubStepCount := c.StepCount;
    end;
  end;
end;

function TCommand.WaitForResources(iUpToMS: NativeInt = -1): boolean;
var
  tmStart: ticker;
  x: NativeInt;
begin

  Result := true;
  if not assigned(Processor) then
    exit;
  x := 0;
  WAitingForResources := true;
  try
    tmStart := tickcount.GetTicker;

    while not Processor.ResourcesAvailable(self) do begin
      Blocked := true;
      Thread.Blocked := true;

      sleepex(x, true);

      inc(x);
      if x > 10000 then
        x := 10000;
      if (iUpToMS >= 0) and (GEtTimeSInce(tmStart) > ticker(iUpToMS)) then
        begin
        Result := false;
        exit;
      end;
    end;
  finally
    WAitingForResources := false;
    Blocked := false;
    Thread.Blocked := false;
  end;

end;

function TCommand.WatcherCount: ni;
begin
  Lock;
  try
    exit(FWatchers.count);
  finally
    unlock;
  end;
end;

{ TCommandProcessor }





procedure TCommandProcessor.AddCommand(cmd: TCommand;
  bTakeOwnerShip: boolean = false);
begin
//  if cmd.classname = 'Tcmd_RunQueries' then
//    debug.consolelog('add '+inttostr(getticker));
{$IFDEF QUEUE_INCOMING}
  LockIncoming;
  try
    cmd.FProcessor := self;
    // if bTakeOwnerShip then
    cmd.OwnedByProcessor := bTakeOwnerShip;

    if not FIncomingCommands.Has(cmd) then
      FIncomingCommands.add(cmd);
    //FIncomingCommands.Add(cmd);
{$IFDEF ADD_COMMANDS_IMMEDIATELY}
    Lock;
    try
      if FAllCommands.indexof(cmd) < 0 then
        FAllCommands.Add(cmd);
    finally
      Unlock;
    end;
{$ENDIF}

    F_atom_IncomingCount := FincomingCommands.Count;
//    Debug.Log(inttostr(F_atom_INcomingCount));

    if FThread = nil then begin
      Lock;
      try
        if FThread = nil then
          StartThread;
      finally
        Unlock;
      end;
    end;


  finally
    UnlockIncoming;
    if assigned(FThread) then begin
      FThread.RunHot := true;
    end;

  end;


{$ELSE}

  Lock;
  try
    cmd.FProcessor := self;
    if (FIncompleteCommands.count = 0) then
      StartThread;
    // if bTakeOwnerShip then
    cmd.OwnedByProcessor := bTakeOwnerShip;

//    if not FAllCommands.Has(cmd) then
      FAllCommands.add(cmd);
//    if not FIncompleteCommands.has(cmd) then
      FIncompleteCommands.add(cmd);



  finally
    Unlock;
    if assigned(FThread) then begin
      FThread.RunHot := true;
    end;
  end;
{$ENDIF}

end;



procedure TCommandProcessor.AdjustAffinitys;
var
  a: array[0..128] of NativeFloat;
  iCPUS: nativeint;
  t: nativeint;
  c: nativeint;
//  t,u,c: nativeint;
  cmd: TCommand;
  function GetBestCPU: nativeint;
  var
    iBest: integer;
    tt: integer;
  begin
    iBest := 0;
    for tt:= 0 to iCPUs-1 do begin
      if a[tt] < a[ibest] then
        iBest := tt;
    end;
    result := iBest;
  end;
begin
  iCPUs := GetEnabledCPUCount;
  FillMem(pointer(@a[0]), sizeof(a), 0);
  Lock;
  try
    iCPUs := GetCPUThreadCount;

    //go through all the commands
    //and assign each command to a CPU and record CPU expense
    for t:= 0 to FActiveCommands.Count-1 do begin
      c := GetBestCPU;
      cmd := FActiveCommands[t];
      //if (cmd.CPUExpense > 0) then begin
        cmd.Affinity := 1 shl c;
      //end;
      a[c] := a[c] + cmd.CPUExpense;
    end;

  finally
    Unlock;
  end;
end;

procedure TCommandProcessor.AssimilateIncoming;
var
  t: nativeint;
  c: TCommand;
begin
  if F_atom_IncomingCount = 0 then
    exit;

  if TryLock then
  try
    if TryLockIncoming then
    try
      for t:= 0 to FIncomingCommands.Count-1 do begin
        c := FIncomingCommands[t];
        FIncompleteCommands.Add(c);
        FAllCommands.Add(c);

      end;
      FIncomingCommands.clear;
      F_atom_IncomingCount := 0;
    finally
      UnlockIncoming;
    end;
  finally
    Unlock;
    FThread.RunHot := true;
  end;

end;

procedure TCommandProcessor.BeforeCheckCommands;
begin
  // optional
end;

function TCommandProcessor.GetCancelled: boolean;
begin
//  Lock;
  try
    Result := FCancelled;
  finally
//  /  Unlock;
  end;
end;



procedure TCommandProcessor.CancelAll;
var
  T: NativeInt;
begin
  Cancelled := true;
  try
    Lock;
    try
      Debug.Log(self,self.ClassName+'--'+inttostr(CommandCount)+' commands to cancel.');
      for T := 0 to CommandCount - 1 do begin
        Commands[T].Cancel;
      end;
    finally
      Unlock;
    end;
    WaitForAll;

  finally
  end;
end;

procedure TCommandProcessor.CheckIfShouldSTopThread;
begin
  if IsComplete and (FThreadCount = 0) then
    StopThread;
end;

procedure TCommandProcessor.CleanFreeCommands;
begin

//  raise exception.Create('unimplemented');
// TODO -cunimplemented: unimplemented block
end;


procedure TCommandProcessor.CleanStaleChildThreads;
begin
{$IFDEF ENABLE_QUICKPOOL}
//  FQuickPool.CleanStale; //CLEANED BY GROWTH MANAGER THREAD
{$ENDIF}
//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TCommandProcessor.Clear;
var
  c: TCommand;
begin
  DetachCommands;

  while CommandCount > 0 do begin
    DeleteCommand(0);
  end;
  Lock;
  try
    FAllCommands.clear;
    while FCompleteCommands.count > 0 do begin
      c := FCompleteCommands[0];
      if c.OwnedByProcessor then
        c.free;
      FCompleteCommands.delete(0);
    end;
    while FIncompleteCommands.count > 0 do begin
      c := FIncompleteCommands[0];
      if c.OwnedByProcessor then
        c.free;
      FIncompleteCommands.delete(0);
    end;
  finally
    Unlock;
  end;
  Cancelled := false;

end;

procedure TCommandProcessor.CommandDone(c: TCommand);
begin
  Lock;
  try
    c.Lock;
    try
        resetopttries;
//      if (c.IsComplete) or (c.IsCancelled)then begin
        FIncompleteCommands.remove(c);
        // FActiveCommands.remove(c);
        FCompleteCommands.add(c);
//        FThread.RunHot := true;
{$IFDEF SUPPORTS_APC}
//        QueueUserApc(@AlertCommandDone, Fthread.handle, 0);
{$ENDIF}

//      end;
    finally
      c.unlock;
    end;
  finally
    Unlock;
//    if assigned(FThread) then FThread.RunHot := true;
  end;

end;

procedure TCommandProcessor.CommandDeactivated(c: TCommand);
begin
  Lock;
  try
    FActiveCommands.remove(c);
    if assigned(FThread) then BEGIN
      FThread.RunHot := true;
      FThread.HasWork := TRUE;
    END;

  finally
    Unlock;
  end;
end;

procedure TCommandProcessor.CommandProgress(sender: TCommand);
begin
  // todo 2: update progress status etc.
end;

constructor TCommandProcessor.Create(Threadowner: TThreadManager;
  sName: string);
begin
  inherited Create;
  volState := THolder<TCommandProcessorDebugState>.create;
  volState.o := TCommandProcessorDebugState.create;

{$IFDEF OVERSPILL}
  Overspill := true;
{$ENDIF}

  NAme := sName;
{$IFDEF ENABLE_QUICKPOOL}
  FQuickPool := TPM.Needthread<TCPThreadPoolGrowthManager>(nil);
  FQuickPool.beginStart;

{$ENDIF}
  rsGEtCOmmand := TRIngStats.create;
  FCompleteCommands := TLocalCommandList.Create;
  FIncompleteCommands := TLocalCommandList.Create;
  FActiveCommands := TLocalCommandList.Create;
  FAllCommands := TLocalCommandList.Create;
  FFreeCommands := TLocalCommandList.Create;
  FResourceStats := TResourceHealthStats.Create;
  FIncomingCommands := TLocalCommandList.create;
  ics(sect_incoming);
  FActiveThreads := TList<TCommandProcessorChildThread>.create;

  // FRolledupCommands := TLocalCommandList.create;
  FThreadCount := 0;
end;

procedure TCommandProcessor.DebugThreadCounts;
begin

// exit;
  Lock;
  try
    if FThread = nil then
      exit;

    Fthread.Status := 'Incomplete: ' + inttostr(FIncompleteCommands.count)
      + ' ActiveCmd:' + inttostr(FActiveCommands.count)
      + ' Complete:' + inttostr(FCompleteCommands.count)
//    Debug.Log(fThread.Status);
  finally
    Unlock;
  end;
end;

procedure TCommandProcessor.DeleteCommand(T: NativeInt);
var
  c: TCommand;
begin
  Lock;
  try

    c := FAllCommands[t];
    FAllCommands.Delete(t);
    FIncompleteCommands.remove(c);
    FActiveCommands.remove(c);
    FCompleteCommands.remove(c);
    c.Processor := nil;

    if c.OwnedByProcessor then
      c.free;

  finally
    Unlock;
  end;

end;


destructor TCommandProcessor.Destroy;
begin
  if not Detached then
    Detach;

  StopThread;


  Debug.Log(self,'Destroying '+self.ClassName+'--');
  GiveUpQuickThreads;
  // ChildthreadCount := 0;
  Debug.Log(self,self.ClassName+'-- Finalizing');
  Clear;
  FCompleteCommands.free;
  FIncompleteCommands.free;
  FActiveCommands.free;
  while FFreeCommands.count > 0 do
    CleanFreeCommands;
  FFreeCommands.free;
  FallCommands.free;
  FResourceStats.Free;
  FIncomingCommands.Free;
  deleteCriticalSEction(sect_incoming);
{$IFDEF ENABLE_QUICKPOOL}
  FQuickPool.stop;
  FQuickPool.WaitFor;
  TPM.NoNeedThread(FQuickPool);
  FQuickPool := nil;
{$ENDIF}
  rsGetCommand.free;
  FActiveThreads.free;
  volState := nil;
  inherited;

end;

procedure TCommandProcessor.Detach;
begin
  DisallowSelfDestruct := true;
  DetachCommands;
  Debug.Log(self,self.ClassName+'-- Commands are cleared.');
  StopThread;
  if Assigned(Fthread) then begin
    Fthread.Detach;
  end;
  inherited;
end;


procedure TCommandProcessor.DetachCommands;
var
  c: TCommand;
begin
  CancelAll;
  RemoveSelfDestructCommands;
  if CommandCount > 0 then
    Debug.ConsoleLog('WARNING!!! You singlely shouldn''t kill a command processor with '+commandcount.tostring+' commands in it.');
  while CommandCount > 0 do begin
    Debug.ConsoleLog('Still has: '+commands[0].classname);
    Commands[0].WaitFor;
//    commands[0].processor := nil;
    if commands[0].fireforget then
      Commands[0].Free;
  end;
  exit;


  Lock;
  try
    while CommandCount > 0 do begin
      Debug.Log(self,self.ClassName+'--Still has '+inttostr(CommandCount)+' commands.');
      c := self.Commands[0];
      if c.TryLock then
        try
          if c.IsExecutingNow then begin
            Debug.Log(self,self.ClassName+'--Still has EXECUTING commands.');
            FThread.RunHot := true;
{$IFDEF DO_SLEEPEX}
            sleepex(100, true);
{$ENDIF}

          end
          else begin
            c.Processor := nil;
          // if the command is not pointed officially at this processor, we need to remove it manually.
            self.RemoveCommand(c);
            if (c.OwnedByProcessor) then
              c.free;
          end;
        finally
          c.Unlock;
        end
      else begin
{$IFDEF DO_SLEEPEX}
        sleepex(100, true);
{$ENDIF}
      end;

    end;
  finally
    Unlock;
  end;

  inherited;
end;

procedure TCommandProcessor.OnThreadDetach(sender: TExternalEventthread);
begin
  Detach;
end;

procedure TCommandProcessor.OnThreadExecute(sender: TExternalEventthread);
var
  c: TCommand;
  ct: TCommandProcessorChildThread;
  nextfuture: int64;
begin
  inherited;
  CmdPerfLog('Start of CommandProcessor.OnThreadExecute');

  if FSafety then begin
    raise Exception.create('thread still running but it should have been shutdown.');
  end;

  sender.Name := 'Command Processor main thread for ' + Name;

  rsGetCommand.BeginTime;

  if sender.safeTerminated then
    exit;

  inc(FCycles);
  if FCycles > 1000000000 then
    FCycles := 0;



  if sender.pooled then
    exit;
  // Debug.Log('Loop');
  if Assigned(sender) and sender.safeTerminated then
    exit;
{  if Assigned(sender) then begin
    Lock;
    try
      sender.StepCount := 100;
      sender.Step := round(self.PercentComplete * 100);
    finally
      Unlock;
    end;
  end;}
  rsGetCommand.EndTime;

  CmdPerfLog('CommandProcessor.OnThreadExecute 1');
  BeforeCheckCommands;
  // get the next command from the queue
  while GEtTimeSInce(LAstSTartTime) < ticker(Throttle) do begin
{$IFDEF DO_SLEEPEX}
    sleepex(lesserof(1000, ticker(Throttle) - GEtTimeSInce(LAstSTartTime)),
      true);
{$ENDIF}
    //AdjustAffinitys;
    if sender.safeTerminated then
      break;
  end;


//  Lock;
//  try
//    if self.FActiveThreads.Count > 100 then begin
//      exit;
//    end
//  finally
//    Unlock;
//  end;


  // sender.Load := self.CommandCount;
  // sender.Cycles := self.CompleteCount;



//  Debug.ConsoleLog('start scan '+inttostr(getticker));
  CmdPerfLog('CommandProcessor.OnThreadExecute 2');
  ProcessCompletedCommands;
  c := GetNextIncompleteCommand(nextfuture);
  if (c= nil) then begin
    BuildVolatileProgress;
    if nextfuture > 0 then
      self.Fthread.ColdRunInterval := nextfuture;
    self.Fthread.RunHot := false

  end;
  CmdPerfLog('CommandProcessor.OnThreadExecute 3');
//  if (c<>nil) and (c.classname = 'Tcmd_RunQueries') then
//    Debug.ConsoleLog('end scan got '+inttostr(getticker));

  if Cancelled then begin
    WaitForRunningCommands;
    CmdPerfLog('CommandProcessor.OnThreadExecute CAncelled Exiting');
    exit;
  end;


  //todo 2: timebomb
  //self.Fthread.RunHot := c <> nil;


  if c = nil then begin
    CmdPerfLog('CommandProcessor.OnThreadExecute nil 4');
    if trylock then
    try
      CmdPerfLog('CommandProcessor.OnThreadExecute nil 4.1');
      inc(FIdleSpins);
      if FIdleSpins > 10000 then begin
        FThread.RunHot := false;
        FIdleSpins := 0;
        GiveUpQuickThreads;
      end;
      CmdPerfLog('CommandProcessor.OnThreadExecute nil 4.2');
    finally
       unlock;
    end;
    CmdPerfLog('CommandProcessor.OnThreadExecute nil 5');
    AssimilateIncoming;
    CmdPerfLog('CommandProcessor.OnThreadExecute nil 6');
    DebugThreadCounts;
    CmdPerfLog('CommandProcessor.OnThreadExecute nil 7');
    if SingleThreaded then
      exit;
    //sleepex(100, true);
    CmdPerfLog('CommandProcessor.OnThreadExecute nil 8');
    CleanStaleChildThreads;
    CmdPerfLog('CommandProcessor.OnThreadExecute nil 9');
    (* if ChildThreadCount = 0 then begin
      raise EBasicException.create('there are no threads');
    end; *)
{$IFDEF DO_SLEEPEX}
    sleepex(1 + (ResourceScanTime * 4), true);
{$ENDIF}
    CmdPerfLog('CommandProcessor.OnThreadExecute nil 10');
    ProcessCompletedCommands;
    CmdPerfLog('CommandProcessor.OnThreadExecute nil 11');
    if ResourceScanTime > 10 then begin
{$IFDEF DO_SLEEPEX}
      sleepex((nativeint(ResourceScanTime) * nativeint(FActiveCommands.count)), true);
{$ENDIF}
    end;
    exit;
  end
  else begin
// if self.Name = 'DirCommands' then begin
// windows.beep(100,50);
// end;
  end;

// if command is not threadable then wait for other threads
  if (self.SingleThreaded) or (not c.Threadable) then begin
    while FActiveCommands.count > 0 do begin
{$IFDEF DO_SLEEPEX}
      sleepex(1000, true); // <<<----- will slow things down if thread is not alerted
{$ENDIF}
      ProcessCompletedCommands;
    end;

    // execute command in this thread
    LAstSTartTime := tickcount.GetTicker;
    c.Execute;


  end
  else
  // if threadable then execute in subthread
    begin
    // find any ready threads
    {while not(ResourcesAvailable(c)) do begin
      sleepex(1 + (ResourceScanTime * 1000), true);
      ProcessCompletedCommands;
    end;}

    CmdPerfLog('CommandProcessor.OnThreadExecute 4');
    ct := Needthread;
    CmdPerfLog('CommandProcessor.OnThreadExecute 5');


{$IFDEF DETAILED_COMMAND_LOGGING}
    Debug.Log(self,inttostr(ct.threadid) + ' got command #' + inttostr(FCommandIndexFound) + ' @' + inttohex(NativeInt(pointer(c)), 8)+' ('+c.ClassName+')');
{$ENDIF}

    if ct.Command <> nil then
      raise exception.Create('thread already has command ' + ' @' + inttohex
          (NativeInt(pointer(c)), 8));

// assign the command

    CmdPerfLog('CommandProcessor.OnThreadExecute 7');
    Lock;
    try
      CmdPerfLog('CommandProcessor.OnThreadExecute 8');
      ct.Name := c.Name;
      c.Thread := ct;
      ct.Command := c;
      ct.betterpriority := c.Priority;
      c.Running := true;
      CmdPerfLog('CommandProcessor.OnThreadExecute 9');
      FActiveCommands.add(c);

      // process the command
      LAstSTartTime := tickcount.GetTicker;

      AdjustAffinitys;
      CmdPerfLog('CommandProcessor.OnThreadExecute 10');

      ct.HasWork := true;
      if not ct.started then
        ct.BeginStart;

      CmdPerfLog('CommandProcessor.OnThreadExecute 11');

    finally
      Unlock;
    end;

  end;


end;

function TCommandProcessor.GetChildThreadLimit: NativeInt;
begin
  result := FChildThreadLImit;
end;


function TCommandProcessor.Getcommand(idx: NativeInt): TCommand;
begin
  Lock;
  try
    AssimilateIncoming;
    result := FAllCommands[idx];
  finally
    Unlock;
  end;
end;

function TCommandProcessor.GetCommandCount: NativeInt;
begin
//  Lockincoming;
  try
    Lock;
    try
      //Assimilateincoming;
      result := FAllCommands.Count+F_atom_IncomingCount;
  //    Result := FIncompleteCommands.count + FCompleteCommands.count;
    finally
      Unlock;
    end;
  finally
//    UnlockIncoming;
  end;
end;

function TCommandProcessor.GetCommandIndex: NativeInt;
begin
  Lock;
  try
    Result := FCommandIndex;
  finally
    Unlock;
  end;
end;

function TCommandProcessor.GetCompletecommand(idx: NativeInt): TCommand;
begin
  Lock;
  try
    result := FCompleteCommands[idx];
  finally
    Unlock;
  end;
end;

function TCommandProcessor.GetCompleteCount: NativeInt;
var
  T: NativeInt;
begin
  Lock;
  try
    Result := FCompleteCommands.count;
  finally
    Unlock;
  end;
  exit;

  Result := 0;
  Lock;
  try
    for T := 0 to CommandCount - 1 do begin
      if Commands[T].IsComplete then
        inc(Result);
    end;
  finally
    Unlock;
  end;

  // if result = CommandCount then
  // StopThread;
end;

function TCommandProcessor.GetCurrentScheduledCustomResourceExpense(
  s: string): single;
var
  T: NativeInt;
  c: TCommand;
begin
  Lock;
  Result := 0;
  try
    for T := 0 to FActiveCommands.count - 1 do begin
      c := FActiveCommands[T];
      if c.IsComplete then
        continue;

      Result := Result + c.Resources.GetResourceUsage(s);
    end;

    for T := 0 to FIncompleteCommands.count - 1 do begin
      c := FIncompleteCommands[T];
      if c.IsComplete then
        continue;

      Result := Result + c.Resources.GetResourceUsage(s);
    end;

  finally
    Unlock;
  end;
end;

function TCommandProcessor.GetCurrentTotalCPUExpense: single;
var
  T: NativeInt;
  c: TCommand;
begin
  Lock;
  Result := 0;
  try
    for T := 0 to FActiveCommands.count - 1 do begin
      c := FActiveCommands[T];
      if c.IsComplete then
        continue;
      if c.WAitingForResources or c.Blocked then
        continue;

      Result := Result + c.CPUExpense;
    end;
    //add 1 to the result for every active thread that doesn't have a command
    for t:= 0 to FActiveThreads.count-1 do begin
      if FActiveThreads[t].Transitioning then
        result := result + 1;
    end;

  finally
    result := lesserof(GetCPUThreadCount,result);
    Unlock;
  end;
end;

function TCommandProcessor.GetCurrenttotalCustomResourceExpense(s: string): single;
var
  T: NativeInt;
  c: TCommand;
begin
  Lock;
  Result := 0;
  try
    for T := 0 to FActiveCommands.count - 1 do begin
      c := FActiveCommands[T];
      if c.IsComplete then
        continue;
      if c.WAitingForResources or c.Blocked then
        continue;

      Result := Result + c.Resources.GetResourceUsage(s);
    end;
  finally
    Unlock;
  end;
end;



function TCommandProcessor.GetCurrentTotalMemoryExpense: single;
var
  T: NativeInt;
  c: TCommand;
begin
  Lock;
  Result := 0;
  try
    for T := 0 to FActiveCommands.count - 1 do begin
      c := FActiveCommands[T];
      if c.IsComplete then
        continue;
      if c.WAitingForResources or c.Blocked then
        continue;
      Result := Result + c.MemoryExpense;
    end;
  finally
    result := lesserof(1.0,result);
    Unlock;
  end;
end;

function TCommandProcessor.GetCurrentTotalMemoryExpenseGB: single;
var
  T: NativeInt;
  c: TCommand;
begin
  Lock;
  Result := 0;
  try
    for T := 0 to FActiveCommands.count - 1 do begin
      c := FActiveCommands[T];
      if c.IsComplete then
        continue;
      if c.WAitingForResources or c.Blocked then
        continue;
      Result := Result + c.MemoryExpense;
    end;
  finally
    result := lesserof(1.0,result);
    Unlock;
  end;
end;

function TCommandProcessor.GetCurrentTotalNetworkExpense: single;
var
  T: NativeInt;
  c: TCommand;
begin
  Lock;
  Result := 0;
  try
    for T := 0 to FActiveCommands.count - 1 do begin
      c := FActiveCommands[T];
      if c.IsComplete then
        continue;
      if c.WAitingForResources or c.Blocked then
        continue;
      Result := Result + c.NetworkExpense;
    end;
  finally
    result := lesserof(1.0,result);
    Unlock;
  end;
end;

function TCommandProcessor.GetCyclesSince(var i: nativeint): nativeint;
begin
  if Cycles < i then
    result := Cycles
  else
    result := Cycles - i;

  i := Cycles;//<<---- important, var i will be set to current cycle automagically;
end;

function TCommandProcessor.GetIsComplete: boolean;
begin
  Lock;
  try

    // result := not (CommandIndex < CommandCount-1);

    // if result then
    //result := CompleteCount >= COmmandCount;
    Result := (F_atom_IncomingCount = 0) and (CompleteCount >= CommandCount);

    if SingleThreaded and (not Result) then begin
      ProcessSingleThreaded;
    end;
  finally
    Unlock;
  end;

// IF RESULT THEN
// STopThread;
end;

function TCommandProcessor.GetMyThreadID: NativeInt;
begin
  Lock;
  try
    Result := Fthread.threadid;
  finally
    Unlock;
  end;
end;

function TCommandProcessor.GEtName: string;
begin
  l;
  try
    try
      Result := FName;
    except
    end;
  finally
    ul;
  end;
end;

function TCommandProcessorChildThread.GetAutoSpin: boolean;
begin
  Lock;
  try
    if Assigned(Command) then begin
      if Command.TryLock then
        try
          Finfo.autospin := Command.Step = 0;
        finally
          Command.Unlock;
        end;
    end
    else begin
      Finfo.autospin := false;
    end;
    Result := Finfo.autospin;
  finally
    Unlock;
  end;
end;

function TCommandProcessorChildThread.GetComand: TCommand;
begin
  Lock;
  try
    Result := Fcommand;
  finally
    Unlock;
  end;
end;

function TCommandProcessorChildThread.GetName: string;
begin
  if Trylock then
  try
    if assigned(command) then begin
      if Command.NameChanged_Volatile then
        Name := Command.Name;
    end;
  finally
    Unlock;
  end;

  result := inherited;
end;

//function TCommandProcessorChildThread.GetSignalDebug: string;
//begin
//  result := 't';
//  if Transitioning then
//    result := 'T';
//
//  result := inherited GetSignalDebug+'['+result+']';
//end;

// function TCommandProcessorChildThread.GetCycles: NativeInt;
// begin
// Lock;
// try
// if assigned(command) then begin
// if command.TryLock then try
// FCycles := command.Step
// finally
// command.Unlock;
// end;
// end else
// FCycles := 0;
//
// result := FCycles;
//
// finally
// Unlock;
// end;
// end;

function TCommandProcessorChildThread.GetStepCount: NativeInt;
begin
  Lock;
  try
    if Assigned(Command) then begin
      if Command.TryLock then
        try
          Finfo.progress.StepCount := Command.StepCount;
        finally
          Command.Unlock;
        end;

    end
    else begin
      Finfo.progress.StepCount := 0;
    end;
     Result := Finfo.progress.StepCount;
  finally
    Unlock;
  end;
end;

procedure TCommandProcessorChildThread.InitFromPool;
begin
  inherited;
  Command := nil;
  Parent := nil;
end;

procedure TCommandProcessorChildThread.MenuAction(idx: ni);
begin
  inherited;
  if idx = 1 then begin
    lock;
    if assigned(command) then begin
      if assigned(command.processor) then begin
        command.Processor.unlock;
        debug.ConsoleLog('nudge!');
      end;
    end;
  end;


end;

function TCommandProcessorChildThread.GEtSpin: boolean;
begin
  Lock;
  try
    if Assigned(Command) then begin
      if Command.TryLock then
        try
          Finfo.spin := Command.Step = 0;
        finally
          Command.Unlock;
        end;
    end
    else
      Finfo.spin := false;

    Result := Finfo.spin;

  finally
    Unlock;
  end;

end;

function TCommandProcessorChildThread.GetStatus: string;
begin
  Lock;
  try
    if Assigned(Command) then begin
      if Command.TryLock then
        try
          Finfo.Status := Command.Status;
        finally
          Command.Unlock;
        end;
    end
    else begin
      // FStatus := '(unassigned)';
    end;
    Result := Finfo.Status;
  finally
    Unlock;
  end;
end;

function TCommand.Getthread: TCommandProcessorChildThread;
begin
//  Lock;
//  try
    Result := Fthread;
//  finally
//    Unlock;
//  end;
end;

function TCommandProcessorChildThread.GetStep: NativeInt;
begin
  Lock;
  try
    if Assigned(Command) then begin
      if Command.TryLock then
        try
          Finfo.progress.Step := Command.Step;
        finally
          Command.Unlock;
        end;
    end
    else begin
      Finfo.progress.Step := 0;
    end;
    Result := Finfo.progress.Step;
  finally
    Unlock;
  end;
end;

procedure TCommandProcessorChildThread.SetCommand(const Value: TCommand);
begin
  Lock;
  try
    if FCommand <> nil then begin
      //if unassigning a command, copy its progress to remember it
      self.Step := FCommand.Step;
      self.StepCount := FCommand.StepCount;
      self.Status := FCommand.Status;
    end;
    Fcommand := Value;
  finally
    Unlock;
  end;
end;

procedure TCommandProcessorChildThread.SetParent(
  const Value: TCommandProcessor);
begin
  FParent := Value;
end;

procedure TCommandProcessorChildThread.Updatethreadinfo;
begin
  inherited;
  if assigned(command) then begin
    Finfo.progress := Command.progress;
    Finfo.status := command.Status;
  end;
end;

procedure TCommand.SetThread(const Value: TCommandProcessorChildThread);
begin
//  Lock;
//  try
    if Value = Fthread then
      exit;

//    if (Value = nil) and (Fthread <> nil) and
//      (Fthread.threadid <> GetCurrentThreadID) then
//      raise exception.Create
//        ('Command thread must be pulled from command thread');

    Fthread := Value;

//  finally
//    Unlock;
//  end;

end;

procedure TCommand.SetWaitingForResources(Value: boolean);
begin
//  Lock;
//  try
    FWaitingForResources := Value;
//  finally
//    Unlock;
//  end;
end;

function TCommandProcessor.GetNextIncompleteCommand(var nextfuture: int64): TCommand;
var
  T: NativeInt;
  c: TCommand;
  rC, rN, rM: single;
  bdiskOver: boolean;
  tmStart: ticker;
  fut: ticker;
  minfut: ticker;
begin
  nextfuture := 0;
  minfut := 5000;
  if comparetext(name,'garbagecollector') = 0 then begin
//    sleep(1000);
//    Debug.log('Garbage collector');

  end;
  OptTRies := 0;
  Lock;
  tmStart := tickcount.GetTicker;
  try
    Result := nil;

    if FCommandIndex > FIncompleteCommands.count - 1 then begin
      FCommandIndex := 0;
    end;
    rC := self.CurrentTotalCPUExpense;
    rN := self.CurrentTotalNetworkExpense;
    rM := self.CurrentTotalMemoryExpense;

    t := CommandIndex;
    while t < FIncompleteCommands.count do begin
      FCommandIndexFound := T;
      c := FIncompleteCommands[T];
      fut := c.TimeUntilExecute;
      if (fut <>0) then
        minfut := lesserof(minfut,c.TimeUntilExecute);

      if (c.FutureExecutionTime <> 0) and (c.TimeUntilExecute<>0) then begin
        inc(t);
        continue;
      end;

      // if (c.IsComplete) then continue;
      // if (c.running) then continue;
      if not c.AreDependsOnComplete then begin
        inc(t);
        continue;
      end;
      if (c.IsCancelled) and (not FActiveCommands.Has(c)) then begin
        CommandDeactivated(c);
        c.IsComplete := true;
        c.OkToFree := true;
        CommandDone(c);
        continue;
      end;
      if (T = CommandIndex) and c.IsComplete then begin
        inc(FCommandIndex);
        if (FCommandIndex >= FIncompleteCommands.count) then
          FCommandIndex := 0;
      end;
      if ((c.CPUExpense > 0.0) and (rC + c.CPUExpense > GetCPUThreadCount)) or
        ((c.NetworkExpense > 0.0) and (rN + c.NetworkExpense > 1.0))
        or ((c.MemoryExpense > 0.0) and (rM + c.MemoryExpense > 1.0)) then begin
        inc(t);
        continue;
      end;


      bdiskOver := false;

      if bdiskOver then begin
        inc(t);
        continue;
      end;

// if self.Name = 'DirCommands' then begin
// Debug.Log(self.Name+' is allowing command with C:N:M:'+floatprecision(rC, 2)+':'+floatprecision(rN, 2)+':'+floatprecision(rM, 2),'filecopy');
// end;

// if (childthreadcount=0) and ((c.running) and not c.IsCancelled) then
        // raise EBasicException.create('command state parity error c.running:'+booltostr(c.running));
      if (not c.IsComplete) and (not c.Running) and (not c.IsCancelled) then
        begin
        if ResourcesAvailable(c) then begin
          Result := c;
          break;
        end;
      end;
      inc(t);
    end;

  finally
    Unlock;
    ResourceScanTime := getTimeSince(tmStart);
  end;

end;

function TCommandProcessor.GEtOnWAitForAll: TNotifyEvent;
begin
  Lock;
  try
    Result := FOnWaitForAll;
  finally
    Unlock;
  end;

end;

function TCommandProcessor.GEtPercentCompleteSimple: single;
begin
  Lock;
  try
    if CommandCount = 0 then
      Result := 0
    else
      Result := FCompleteCommands.count / CommandCount;

  finally
    Unlock;
  end;
end;

function TCommandProcessor.GEtPercentComplete: single;
begin
  Lock;
  try
    if CommandCount > 100 then
      Result := GEtPercentCompleteSimple
    else
      Result := GetPercentCompleteAccurate;
  finally
    Unlock;
  end;
end;

function TCommandProcessor.GetPercentCompleteAccurate: single;
var
  r: single;
  c: TCommand;
  T: NativeInt;
  tmStart: ticker;
begin
  tmStart := tickcount.GetTicker;
  Lock;
//  Debug.Log('GetPercentComplete '+inttostr(CommandCount)+'='+inttostr(FIncompleteCommands.Count+FCompleteCommands.Count));
  try
    r := 0;
    for T := 0 to CommandCount - 1 do begin
      c := Commands[T];
      r := r + c.PercentComplete;
    end;

    if CommandCount = 0 then
      Result := 1
    else
      Result := r / CommandCount;

  finally
    RecommendedSleepTime := 1 + (GEtTimeSInce(tmStart) * 10);

    Unlock;
  end;

end;


function TCommandProcessor.HasCommand(c: TCommandClass): boolean;
var
  T: NativeInt;
begin
  Result := false;
  Lock;
  try
    for T := 0 to CommandCount - 1 do begin
      if Commands[T].ClassType = c then begin
        Result := true;
        break;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TCommandProcessor.Lock;
begin

//  Debug.log(self,inttostr(getcurrentthreadid)+' wants lock');
  inherited;
//  Debug.log(self,inttostr(getcurrentthreadid)+' locked');
  lockthr := GetCurrentThreadID;
  lockobj := pointer(self);

//  if getcurrentthreadid = thread_debug_tag then begin
//    Debug.consolelog(inttostr(getcurrentthreadid)+' hit lock LockCount='+inttostr(sect.LockCount)+' RecursionCount='+inttostr(sect.RecursionCount-1)+'->'+inttostr(sect.RecursionCount));
//    if sect.RecursionCount = 3 then
//      Debug.ConsoleLog('3');
//  end;


end;

procedure TCommandProcessor.LockIncoming;
begin
  ecs(sect_incoming);
end;

procedure TCommandProcessor.MoveToEnd(cmd: TCommand);
begin
  self.Lock;
  try
    FIncompleteCommands.remove(cmd);
    FActiveCommands.remove(cmd);
    FIncompleteCommands.add(cmd);
    FWaitIndex := 0;
// self.RemoveCommand(cmd, false);
// self.AddCommand(cmd, cmd.OwnedByProcessor);
  finally
    self.Unlock;
  end;

end;

function TCommandProcessor.Needthread: TCommandProcessorChildThread;
var
  ct: TCommandProcessorChildThread;
begin
  interlockedincrement(FthreadCount);
{$IFDEF ENABLE_QUICKPOOL}
  result := FQuickPool.NeedThread;

{$ELSE}
  result := TPM.Needthread<TCommandProcessorChildThread>(self);
{$ENDIF}

  result.Parent := self;


  Lock;
  try
    FActiveThreads.add(result);
  finally
    Unlock;
  end;
end;

procedure TCommandProcessor.GiveUpQuickThreads;
begin
{$IFDEF ENABLE_QUICKPOOL}
{$ENDIF}
end;
procedure TCommandProcessor.NoNeedThread(thr: TCommandProcessorChildThread);
begin
{$IFDEF DETAILED_COMMAND_LOGGING}
  if thr.Command <> nil then
    Debug.Log(self,inttostr(thr.threadid) + ' finished command  @' + inttohex(NativeInt(pointer(thr.command)), 8)+' ('+thr.command.ClassName+')')
  else
    Debug.Log(self,inttostr(thr.threadid) + ' finished command');

{$ENDIF}
  thr.lastUsed := getticker;
  thr.HasWork := false;
  Lock;
  try
    FActiveThreads.remove(thr);
  finally
    Unlock;
  end;

  thr.command := nil;
{$IFDEF ENABLE_QUICKPOOL}
  FQuickPool.NoNeedThread(thr);
{$ELSE}
  tpm.NoNeedthread(thr);
{$ENDIF}
  InterlockedDecrement(fthreadcount);


end;

procedure TCommandProcessor.NotifyWatchers;
begin
  //
end;

procedure TCommandProcessor.ProcessCompletedCommands;
var
  T: NativeInt;
  cc: TCommand;
begin
  Lock;
  try
    for T := FActiveCommands.count - 1 downto 0 do begin
      if (FActiveCommands[T].IsComplete) and (not(FActiveCommands[T].Running))
        then
        FActiveCommands.delete(T);
    end;

    for T := FCompleteCommands.count - 1 downto 0 do begin
      cc := FCompleteCommands[T];
      if not cc.IsComplete then
        continue;
      cc.StepCount := cc.StepCount;
      cc.Step := cc.StepCount;
      if cc.FireForget then begin
//        Removecommand(cc);
//        FCompleteCommands.delete(T);
        if (not cc.SelfDestructInitiated) then begin
          if (not DisallowSelfDestruct) and (cc.SelfDestructTime > 0)  then begin
//            debug.log(cc.classname+' will self destruct');
            cc.SelfDestructInitiated := true;
            commands_system.SelfDestruct(cc, cc.SelfDestructTime, nil)
          end
          else begin
//            debug.log(cc.classname+' will die immediately');
            cc.Processor := nil;
            cc.free;
          end;
        end;

      end;


    end;

  finally
    Unlock;
  end;

end;

procedure TCommandProcessor.RollupCompletedCommands;
var
  T: NativeInt;
  c, cc: TCommand;
begin
  exit;

end;

procedure TCommandProcessor.ProcessSingleThreaded;
begin
  SingleThreaded := true;
  OnThreadExecute(nil);
end;



procedure TCommandProcessor.RemoveCommand(cmd: TCommand;
  bFreeIfOwnedByProcessor: boolean = true);
var
  T: NativeInt;
  bFound: boolean;
begin
  if (cmd.Started) or (cmd.IsComplete) then
    cmd.WaitFor;
  Lock;
  try

    if FActiveCOmmands.has(cmd) then
      raise Exception.Create('Cannot remove an active command: '+cmd.classname);
    FallCommands.remove(cmd);
    bFound := false;
    for T := FCompleteCommands.count - 1 downto 0 do begin
      if FCompleteCommands[T] = cmd then begin
        FCompleteCommands.delete(T);
        bFound := true;
        break;
      end;
    end;
    //if not bFound then
    for T := FIncompleteCommands.count - 1 downto 0 do begin
      if FIncompleteCommands[T] = cmd then begin
        FIncompleteCommands.delete(T);
        if (T < CommandIndex) then
          FCommandIndex := FCommandIndex - 1;
        bFound := true;
        break;
      end;
      //FIncompleteCommands.remove(cmd);
//      FActiveCommands.remove(cmd);
//      FCompleteCommands.remove(cmd);


      // cmd.Processor := nil;
      if bFreeIfOwnedByProcessor then begin
        if cmd.OwnedByProcessor then begin
          if not cmd.Freeing then
            cmd.free;
        end;
      end;
    end;

    if not bFound then begin
      LockIncoming;
      try
        FIncomingCommands.remove(cmd);
      finally
        UnlockIncoming;
      end;
    end;


      // cmd.Processor := nil;
    if bFreeIfOwnedByProcessor then begin
      if cmd.OwnedByProcessor then begin
        if not cmd.Freeing then begin
          cmd.free;
        end
      end;
    end;

  finally
    Unlock;
  end;

  // FCommandIndex := 0;

end;


procedure TCommandProcessor.RemoveSelfDestructCommands;
var
  t: ni;
  c: TCommand;
begin
  Lock;
  try
    for t:= 0 to FCompleteCommands.count-1 do begin
      c := FCompleteCommands[t];
      if c.SelfDestructInitiated then begin
        c.Lock;
        try
          c.Processor := nil;
        finally
          c.Unlock;
        end;
      end;
    end;

  finally
    unlock;
  end;

end;

procedure TCommandProcessor.REsetOptTries;
begin
  OptTries := 0;
end;

function TCommandProcessor.ResourcesAvailable(c: TCommand): boolean;
var
  bOverload: boolean;
  r1, r2, r4,r5 : single;
  r1others, r2others, r4others,r5others : single;
  T: NativeInt;
  tmStart, tmEnd: ticker;
  cc: TCommand;
  cr: TCommandResources;
  crr: TCommandResourceRecord;
  cru: single;
begin
  cc := c;
  try
    if OverSpill then begin
      c := TEmptyCommand.create;
      c.MemoryExpense := 0;
      c.CPUExpense := 0;
    end;

  {$IFDEF LOCK_RESOURCE_SCAN}
    Lock;
    try
  {$ENDIF}
      tmStart := tickcount.GEtTicker();
      try
        r1others := self.GetCurrentTotalCPUExpense;
        r2others := self.GetCurrentTotalNetworkExpense;
        r4others := self.GetCurrentTotalMemoryExpense;
        r5others := self.GetCurrentTotalMemoryExpenseGB;
        r1 := r1others + c.CPUExpense;
        r2 := r2others + c.NetworkExpense;
        r4 := r4others + c.MemoryExpense;
        r5 := r5others + c.MemoryExpenseGB;

        bOverload := (
             ((r1 > GetEnabledCPUCount) and (r1Others>0.0))
          or ((r2 > 1.0) and (r2others > 0.0))
          or ((r4 > 1.0) and (r4others > 0.0))
          or ((r5 * 1000000000) > GetPhysicalMemory) and (r5others > 0.0));

        Result := not bOverload;

        if bOverload then
          exit;

        cr := c.Resources;
        cr.Lock;
        try
          for T := 0 to cr.count-1 do begin
            crr := cr.GetresourceByIndex(t);
            cru := GetCurrenttotalCustomResourceExpense(crr.Name);
            if cru+lesserof(crr.Usage,1.0) > 1.0 then begin
              result := false;
              exit;
            end;
          end;
        finally
          cr.Unlock;
        end;

        Result := true;
      finally
        tmEnd := tickcount.GEtTicker();
        ResourceScanTime := GEtTimeSInce(tmEnd, tmStart);
      end;
  {$IFDEF LOCK_RESOURCE_SCAN}
    finally
      Unlock;
    end;
  {$ENDIF}
  finally
    if cc <> c then
      c.Free;
  end;

end;

procedure TCommand.SelfDestruct(iTime: ticker);
begin
  commands_system.SelfDestruct(self, iTime);
end;

procedure TCommandProcessor.SetCancelled(const Value: boolean);
begin
  Lock;
  try
    FCancelled := Value;
  finally
    Unlock;
  end;
end;

procedure TCommandProcessor.SetChildThreadCount(const Value: NativeInt);
begin
  exit;
end;

procedure TCommandProcessor.SetChildThreadLimit(const Value: NativeInt);
begin
  Lock;
  try
    FChildThreadLImit := Value;
  finally
    Unlock;
  end;

end;

procedure TCommandProcessor.SetName(value: string);
begin
  l;
  try
    FName := Value;
  finally
    ul;
  end;

end;

procedure TCommandProcessor.SetOnWaitForAll(const Value: TNotifyEvent);
begin
  Lock;
  try
    FOnWaitForAll := Value;
  finally
    Unlock;
  end;

end;

procedure TCommandProcessor.SetPriority(c: TCommand);
var
  i: NativeInt;
begin
{$IFNDEF USE_LINKED_LISTS_OF_COMMANDS}
  if TryLock then
    try
      i := FIncompleteCommands.indexof(c);
      if i >= 0 then begin
        FCommandIndex := i;
      end;

    finally
      Unlock;
    end;
{$ENDIF}
end;

procedure TCommandProcessor.StartThread;
begin
  if SingleThreaded then
    exit;

  Lock;
  try
    if not Assigned(Fthread) then begin
      // FThread := TExternalEventThread.create(nil, false, true, OnThreadExecute);
      Debug.Log(self,'CommandProcessor thread is starting');
      Fthread := TCommandProcessorMAinThread
        (TPM.Needthread(TCommandProcessorMAinThread, self));
      Fthread.CP := self;
      Fthread.loop := true;
      Fthread.OnExecute := OnThreadExecute;
      Fthread.OnDetach := OnThreadDetach;
      Fthread.saferesume;//<---calls start
      Debug.Log(self,'CommandProcessor thread has started');
    end;
  finally
    Unlock;
  end;
end;

procedure TCommandProcessor.StopThread;
var
  thr: TCommandProcessorMAinThread;
begin
  GiveUpQuickThreads;


  thr := nil;
  // Debug.Log('CommandProcessor.StopThread called');
  Lock;
  try

    thr := Fthread;


    Fthread := nil;
    // beep(100,100);

    if thr = nil then
      exit;

    Debug.Log(self,self.ClassName+'-- Detaching Thread');
    thr.beginStop;

  finally
    Unlock;
  end;

  if assigned(thr) then begin
    thr.EndStop;
    //thr.WaitForFinish;
    thr.cp := nil;
    Debug.Log(self,self.ClassName+'-- Pushing Thread to Pool');
    TPM.NoNeedthread(thr);

  end;
  //FSafeTy := true;


end;

procedure TCommandProcessor.ThreadDone(thr: TCommandProcessorChildThread);
begin
  if thr.Command<> nil then begin
    raise EBasicException.create('You can''t call threaddone while command is still assigned.');
  end;
  Lock;
  try
{$IFNDEF ALLOW_EARLY_THREADDONE_SIGNAL}
    if (thr.Command <> nil) then
      raise EBasicException.Create(
        'thread cannot be marked as done until detached');
{$ENDIF}

{$IFDEF DETAILED_COMMAND_LOGGING}
    Debug.Log(self,'Thread done');
{$ENDIF}
    NoNeedThread(thr);

    DebugThreadCounts;
{$IFDEF SUPPORTS_APC}
    QueueUserApc(@AlertCommandDone, Fthread.handle,nativeuint(@self));
{$ENDIF}

  finally
    Unlock;
  end;
  if assigned(FThread) then FThread.RunHot := true;
end;

function TCommandProcessor.TryLock: boolean;
begin

//  Debug.log(self,inttostr(getcurrentthreadid)+' tries lock');
  result := inherited;
  if result then begin
//  if getcurrentthreadid = thread_debug_tag then
//    Debug.consolelog(inttostr(getcurrentthreadid)+' hit lock LockCount='+inttostr(sect.LockCount)+' RecursionCount='+inttostr(sect.RecursionCount-1)+'->'+inttostr(sect.RecursionCount));
//
//    Debug.log(self,inttostr(getcurrentthreadid)+' gets lock');
//
//    if sect.RecursionCount = 3 then
//      Debug.ConsoleLog('3');


  end
  else begin
//    Debug.log(self,inttostr(getcurrentthreadid)+' fails to get lock');
  end;
  if result then begin
    lockthr := GetCurrentThreadID;
    lockobj := pointer(self);
  end;
end;

function TCommandProcessor.TryLockIncoming: boolean;
begin
  result := tecs(sect_incoming);
end;

procedure TCommandProcessor.Unlock;
begin

//  if getcurrentthreadid = thread_debug_tag then
//    Debug.consolelog(inttostr(getcurrentthreadid)+' hit lock LockCount='+inttostr(sect.LockCount)+' RecursionCount='+inttostr(sect.RecursionCount)+'->'+inttostr(sect.RecursionCount-1));
  if self = nil then
    raise ECritical.create('wtf!');

  lockobj := nil;
  lockthr := int64(INVALID_HANDLE_VALUe);
  inherited;
//  Debug.log(self,inttostr(getcurrentthreadid)+' unlocked');

//  if getcurrentthreadid = thread_debug_tag then
//    Debug.log(inttostr(getcurrentthreadid)+' hit');



end;

procedure TCommandProcessor.UnlockIncoming;
begin
  LeaveCriticalSection(sect_incoming);
end;

procedure TCommandProcessor.WaitForAll(exceptFor: TCommand);
// NOTE!: FOR EXTERNAL USE ONLY!
// NOTE NOTE NOTE!:  DO NOT CALL FROM WITHIN SAME THREAD
// NOTE! we're using FWaitIndex isntead of the usual T because if the commands
// are reordered then the waitindex must be reset
var
  cnt: ni;
begin
  if Assigned(OnWaitForAll) then
    OnWaitForAll(self);

  // start at 0
  Lock;
  try
    FWaitIndex := 0;
  finally
    Unlock;
  end;
  cnt := 0;
  if exceptFor <> nil then
    cnt := 1;

  while FWaitIndex < CommandCount do begin
    Commands[FWaitIndex].WaitFor;

    Lock;
    try
      inc(FWaitIndex);
    finally
      Unlock;
    end;

  end;
  CheckIfShouldSTopThread;

end;

procedure TCommandProcessor.WaitForRunningCommands;
//var
//  i: NativeInt;
begin
  while RunningCount > 0 do begin
{$IFDEF DO_SLEEPEX}
    sleepex(lesserof(1000, CommandCount), true);
{$ENDIF}
  end;

end;

function TCommandProcessor.RunningCount: NativeInt;
var
  T: NativeInt;
begin
  Result := 0;
  Lock;
  try
    for T := 0 to CommandCount - 1 do begin
      if Commands[T].IsExecutingNow then begin
        inc(Result);
      end;
    end;
  finally
    Unlock;
  end;
end;

{ TCommandProcessorChildThread }

procedure TCommandProcessorChildThread.BuildMenu;
var
  t: ni;
begin
  inherited;
  Lock;
  try
    if command <> nil then begin
      Command.Lock;
      try

        FMenu.add('Nudge processor lock');
        FMenu.add('-');
{$IFDEF WINDOWS}
        FMenu.add('Processor Lock Owner:'+inttostr(command.FProcessor.sect.cs.OwningThread));
{$ELSE}
        FMenu.add('Processor Lock Owner: (unknown on mobile)');
{$ENDIF}
        FMenu.add('-');
        FMenu.Add('CPU:'+floatprecision(command.cpuexpense, 2));
        FMenu.Add('Mem:'+floatprecision(command.Memoryexpense, 2));
        for t:= 0 to command.resources.Count-1 do begin
          FMenu.add(command.Resources.GetResourceByIndex(t).Name+':'+floatprecision(command.Resources.GetResourceByIndex(t).Usage, 2));
        end;
      finally
        command.Unlock;
      end;

    end else begin
      FMenu.add('No command Assigned');
    end;
  finally
    unlock;
  end;
end;

destructor TCommandProcessorChildThread.Destroy;
begin
  Debug.Log(self,'Destroying '+self.ClassName+' #'+inttostr(ThreadID));
  inherited;
end;

procedure TCommandProcessorChildThread.DoExecute;
var
  c: TCommand;
begin
  inherited;
  CmdPerfLog('CommandProcessorChildThread Execute');
  try
    self.LastUsed := tickcount.GetTicker;

    try
      SleepLength := 0;
      try
        CmdPerfLog('CommandProcessorChildThread Execute.Command.Execute');
        try
          self.Name := command.Name;
          Command.Execute;
        finally
          //cOmmand.IsComplete := true;
        end;
        Transitioning := true;
        CmdPerfLog('CommandProcessorChildThread Execute.Command.Execute (returned)');
// Debug.Log('Command '+command.classname+' is finished', 'command');
      finally
        // PArent.CommandDone(Command);
        if Assigned(Command) then begin
          CmdPerfLog('CommandProcessorChildThread Execute.10');
          c := command;
          Command := nil;
          c.Thread := nil;

          CmdPerfLog('CommandProcessorChildThread Execute.11');
          c.Running := false;
          CmdPerfLog('CommandProcessorChildThread Execute.12');

{$IFNDEF FIRE_FORGET_CONTROLLED_BY_CP}
          if c.FireForget then begin
          {$IFDEF DONT_USE_DQ}
            {$IFDEF IOS}
               c.detach;
            {$ELSE}
               c.free;
            {$ENDIF}
          {$ELSE}
            DQ.Add(c);
            c := nil;
          {$ENDIF}
          end;
{$ENDIF}

          CmdPerfLog('CommandProcessorChildThread Execute.13');
         end;
        Command := nil;
        CmdPerfLog('CommandProcessorChildThread Execute.14');
        //Status := Status + '->Done';
        Iterations := Iterations + 1;
{$IFNDEF ALLOW_EARLY_THREADDONE_SIGNAL}
        Parent.ThreadDone(self);
{$ENDIF}
        CmdPerfLog('CommandProcessorChildThread Execute.15');
      end;
    except
      on E: exception do begin
        Debug.Log(self,'Exception in command processor child thread: ' +
            E.message);
        Status := E.ClassName + ' ' + E.message;
        Error := e.Message;
      end;
    end;
  finally

  end;
end;

{ TCommandProcessorChildThreadPool }

{ TSharedObjectWithCommandProcessor }

constructor TSharedObjectWithCommandProcessor.Create;
begin
  inherited;
  FCP := TCommandProcessor.Create(BackGroundThreadMan, ClassName);
end;

destructor TSharedObjectWithCommandProcessor.Destroy;
begin
  FCP.free;

  inherited;
end;

function TSharedObjectWithCommandProcessor.GeTCommandProcessor
  : TCommandProcessor;
begin
  l;
  try
    Result := FCP;
  finally
    ul;
  end;
end;


{ TRolledUpCommand }

procedure TRolledUpCommand.DoExecute;
begin

  // no implementation required
end;

{ TEmptyCommand }

destructor TEmptyCommand.Destroy;
begin
//  Debug.log('Destroying '+self.classname);
  inherited;
end;

procedure TEmptyCommand.DoExecute;
begin
  FExecutionTime := tickcount.GetTicker;
  // no implementation required

  //sleep(random(1000));
end;

procedure TEmptyCommand.InitExpense;
begin
  inherited;
{  DiskExpense['c'] := random(100) / 100;
  MemoryExpense := random(100) / 100;
  CPUExpense := random(100) / 100;
  NetworkExpense := random(100) / 100;
  Resources.SetResourceUsage('cr1', random(100) / 100);
  Resources.SetResourceUsage('cr2', random(100) / 100);}

{
  CPUExpense := 0;
  case random(5) of
    //0: //DiskExpense['c'] := random(100) / 100;
    1:MemoryExpense := random(100) / 100;
    2:CPUExpense := random(100) / 100;
    3:NetworkExpense := random(100) / 100;
    4:Resources.SetResourceUsage('cr1', random(100) / 100);
  else
    Resources.SetResourceUsage('cr2', random(100) / 100);
  end;
}

end;

{ TExceptionTestCommand }

procedure TExceptionTestCommand.DoExecute;
begin
  raise exception.Create('failure');
end;

{ TCommandProcessorMAinThread }

function TCommandProcessorMAinThread.GetCP: TCommandProcessor;
begin
  Lock;
  try
    Result := FCP;
  finally
    Unlock;
  end;
end;

procedure TCommandProcessorMAinThread.SetCP(const Value: TCommandProcessor);
begin
  Lock;
  try
    FCP := Value;
  finally
    Unlock;
  end;
end;

{ TCommandResources }

procedure TCommandResources.Clear;
begin
  Lock;
  try
    while FCommandResources.Count > 0 do begin
      {$IFDEF IOS}
      FCommandResources[0].Detach;
      {$ELSE}
      FCommandResources[0].Free;
      {$ENDIF}

      FcommandResources.Delete(0);
    end;
   FCommandResources.Clear;
  finally
    Unlock;
  end;
end;

function TCommandResources.Count: NativeInt;
begin
  Lock;
  try
    result := FCommandResources.Count;
  finally
    Unlock;
  end;
end;

destructor TCommandResources.Destroy;
begin
  Clear;
  FCommandResources.Free;
  FCommandResources := nil;

  inherited;
end;

function TCommandResources.FindResource(s: string): TCommandResourceREcord;
var
  i: NativeInt;
begin
  i := IndexOfResource(s);
  RESULT := NIL;
  if (i>=0) then
    result := FCommandResources[i];
end;

function TCommandResources.GetResourceByIndex(i: NativeInt): TCommandResourceRecord;
begin
  Lock;
  try
    result := FCommandResources[i];
  finally
    Unlock;
  end;
end;

function TCommandResources.GetResourceUsage(s: string): single;
var
  r: TCommandResourceRecord;
begin
  result := 0.0;
  Lock;
  try
    r := FindResource(s);
    if r <> nil then begin
      result := r.Usage;
    end;
  finally
    Unlock;
  end;
end;

function TCommandResources.IndexOfResource(s: string): NativeInt;
var
  t: NativeInt;
begin
  result := -1;
  s := lowercase(s);
  for t:= 0 to FCommandResources.Count-1 do begin
    if FCommandResources[t].Name = s  then begin
      result := t;
      break;
    end;
  end;

end;

procedure TCommandResources.Init;
begin
  inherited;
  FCommandResources := TList<TCommandResourceRecord>.Create;
end;

procedure TCommandResources.SetResourceUsage(s: string; u: single);
var
  r: TCommandResourceRecord;
begin
  s := lowercase(s);
  Lock;
  try
    r := FindResource(s);
    if r = nil then begin
      r := TCommandResourceRecord.Create;
      FCommandResources.Add(r);

    end;

    r.Name := s;
    r.Usage := u;


  finally
    Unlock;
  end;
end;

{ TRandomCommand }

procedure TRandomCommand.DoExecute;
var
  t: nativeint;
  m: nativeint;
begin
  m := random(100);
  for t:= 0 to m do begin
    StepCount := m;
    Step := t;
    sleep(100);
  end;

end;

procedure TRandomCommand.InitExpense;
var
  r: integer;
begin
  inherited;
{  DiskExpense['c'] := random(100) / 100;
  MemoryExpense := random(100) / 100;
  CPUExpense := random(100) / 100;
  NetworkExpense := random(100) / 100;
  Resources.SetResourceUsage('cr1', random(100) / 100);
  Resources.SetResourceUsage('cr2', random(100) / 100);}
  CPUEXpense := 0.0;
  r := random(50);
  case r of
    //0: DiskExpense['c'] := random(100) / 100;
    1:MemoryExpense := random(100) / 100;
    2:CPUExpense := random(100) / 100;
    3:NetworkExpense := random(100) / 100;
  else
    Resources.SetResourceUsage('cr'+inttostr(r), random(100) / 100);
  end;

end;

{ Tcmd_Forget }

{$IFDEF ALLOW_FORGET}
procedure Tcmd_Forget.DoExecute;
begin
  CommandToForget.WaitFor;
  CommandToForget.Free;
  CommandToForget := nil;

end;
{$ENDIF}


{$IFDEF ALLOW_FORGET}
class procedure Tcmd_Forget.Forget(c: TCommand);
var
  cc: Tcmd_Forget;
begin
  cc := Tcmd_Forget.Create;
  cc.CommandToForget := c;
  cc.Start;

end;



procedure Tcmd_Forget.Init;
begin
  inherited;
  FireForget := true;
end;

procedure Tcmd_Forget.InitExpense;
begin
  inherited;
  CPUExpense := 0.0;
end;
{$ENDIF}

{ TResourceHealthStats }

function TResourceHealthStats.AddStat(
  sResourceName: string): TResourceHealthData;
var
  tmp: TResourceHealthData;
begin
  Lock;
  try
    tmp := TResourceHealthData.Create;
    tmp.Resource := sResourceName;
    FData.Addobject(lowercase(sResourceName), tmp);
    result := TResourceHealthData(FData.Objects[FData.Count-1]);

  finally
    Unlock;
  end;

end;

procedure TResourceHealthStats.ApplyResourceStat(sResourceName: string; Through,
  time: nativefloat);
var
  hd: TResourceHealthData;
begin
  Lock;
  try
    hd := ForceStat(sResourceName);
    hd.ApplyStat(Through, Time);
  finally
    Unlock;
  end;

end;

procedure TResourceHealthStats.Clear;
begin
  lock;
  try
    while FDAta.count > 0 do begin
      {$IFDEF IOS}
      TBetterObject(FData.objects[FData.count-1]).Detach;
      {$ELSE}
      TBetterObject(FData.objects[FData.count-1]).free;
      {$ENDIF}
      FData.delete(FData.count-1);
    end;
  finally
    Unlock;
  end;
end;

constructor TResourceHealthStats.create;
begin
  inherited;
  FData := TStringlist.create;
//  FData.IgnoreCase := true;
end;

destructor TResourceHealthStats.destroy;
begin



  FData.Free;
  inherited;
end;

function TResourceHealthStats.FindStat(
  sResourceName: string): TResourceHealthData;
var
  i: nativeint;
begin
  Lock;
  try
    sResourceName := lowercase(sResourceName);
    i := IndexOfStat(sResourceNAme);
    if i < 0 then
      result := nil
    else
      result := TResourceHealthData(Fdata.Objects[i]);
  finally
    Unlock;
  end;
end;

function TResourceHealthStats.ForceStat(
  sResourceName: string): TResourceHealthData;
begin
  Lock;
  try
    result := FindStat(sResourceName);
    if result = nil then begin
      result := AddSTat(sResourceName);
    end;
  finally
    Unlock;
  end;
end;

function TResourceHealthStats.GEtStatByIndex(idx: integer): TResourceHealthData;
begin
  result := FData.objects[idx] as TResourceHealthData;
end;

function TResourceHealthStats.GetStatCount: nativeint;
begin
  Lock;
  try
    result := FData.count;
  finally
    Unlock;
  end;
end;

function TResourceHealthStats.GetStatForResource(
  sResourceName: string): TResourceHealthData;
begin
  result := ForceStat(sResourceName);

end;


function TResourceHealthStats.IndexOfStat(
  sResourceName: string): nativeint;
begin
  Lock;
  try
    sResourceName := lowercase(sResourceName);
    result := FData.INdexOf(sResourceNAme);
  finally
    Unlock;
  end;

end;


{ TResourceHealthData }

procedure TResourceHealthData.ApplyStat(Time, Through: nativefloat);
begin
  Lock;
  try

    if Time <= 0 then
      exit;

    if Through > LARGE_TRANSFER_THRESHOLD then begin
      if Through/Time > MaxLarge then begin
        MaxLarge := Through/Time;
//        Debug.Log(self,self.Resource+' New Large:'+floattostr(round(MaxLarge))+' Through:'+floattostr(Through)+' Time:'+floattostr(Time));
      end;
    end else begin
      if Through/Time > MaxSmall then begin
        MaxSmall := Through/Time;
//        Debug.Log(self,self.Resource+' New Small:'+floattostr(round(MaxSmall))+' Through:'+floattostr(Through)+' Time:'+floattostr(Time));
      end;
    end;
  finally
    Unlock;
  end;

end;

procedure TResourceHealthData.Init;
begin
  //
end;


{ TCommandList<T> }

procedure TCommandList<T>.CancelAll;
var
  t: nativeint;
begin
  for t := count-1 downto 0 do begin
    if not self[t].iscomplete then begin
      self[t].cancel;
    end;
  end;
end;

procedure TCommandList<T>.ClearAndDestroyCommands;
begin
  while count> 0 do begin
{$IFDEF IOS}
    self[count-1].detach;
{$ELSE}
    self[count-1].free;
{$ENDIF}
    self.Delete(count-1);
  end;
end;

procedure TCommandList<T>.DestroyCompleted;
var
  t: nativeint;
begin
  for t := count-1 downto 0 do begin
    if self[t].iscomplete then begin
{$IFDEF IOS}
      self[t].detach;
{$ELSE}
      self[t].free;
{$ENDIF}
      delete(t);
    end;
  end;
end;

function TCommandList<T>.FirstIncomplete: T;
var
  t: nativeint;
begin
  result := nil;
  for t:= 0 to count-1 do begin
    result := items[t];
    if not result.IsComplete then
      break;
  end;

end;

function TCommandList<T>.GEtPercentComplete: nativefloat;
var
  t: integer;
begin
  Lock;
  try
    result := 0;
    for t:= 0 to count-1 do begin
      result := result + self.Items[t].PercentComplete;
    end;

    result := result / count;
  finally
    Unlock;
  end;

end;



function TCommandList<T>.IsComplete: boolean;
var
  t: integer;
begin
  result := false;
  for t:= 0 to count-1 do begin
    if not items[t].IsComplete then
      exit;
  end;

  result := true;

end;

procedure TCommandList<T>.RemoveCompleted;
var
  t: nativeint;
begin
  for t := count-1 downto 0 do begin
    if self[t].iscomplete then
      delete(t);
  end;
end;

procedure TCommandList<T>.Startall;
var
  t: ni;
begin
  for t:=0 to count-1 do begin
    if not self[t].Started then
      self[t].start;
  end;

end;

//------------------------------------------------------------------------------
procedure TCommandList<T>.WaitForAll;
var
  t: nativeint;
begin
  for t:= 0 to count-1 do begin
    self[t].waitfor;      //todo 1: cannot wait on a fire-forget command
  end;

end;

procedure TCommandList<T>.WaitForAll_DestroyWhileWaiting;
var
  t: nativeint;
  c: TCommand;
begin
  while count > 0 do begin
    c := self[0];
    c.waitfor;      //todo 1: cannot wait on a fire-forget command
    delete(0);
    c.free;
    c := nil;
  end;
end;

procedure oinit;
begin
// ChildThreadPool := TCommandProcessorChildThreadPool.create;
end;

procedure ofinal;
begin
  if BGCmd <> nil then
    BGCmd.free;
// ChildThreadPool.free;
// ChildThreadPOol := nil;


end;

procedure SetCommandStatus(s: string);
begin
  if threadcommand <> nil then
    threadcommand.status := s;
end;

{ TMultiThreadedCommandProcessor }

procedure WAitForCommandBlind(c: TCommand);
begin
  if threadcommand <> nil then
    threadcommand.waitforanothercommand(c)
  else
    c.waitfor;
end;

{ TCPThreadPoolGrowthManager }

procedure TCPThreadPoolGrowthManager.CleanStale;
const
  GIVE_FROM_QP_TO_POOL_TIME = 100;
var
  thr: TCommandProcessorChildThread;
  tmSince: ticker;
begin
{$IFDEF CLEANSTALE}
  thr := nil;
  Lock;
  try
    if IdleCount > 1 then begin
      thr := FList[0];
      tmSince := gettimeSince(thr.lastused);
      if tmSince > GIVE_FROM_QP_TO_POOL_TIME then begin
        FList.remove(thr);
      end else begin
        thr := nil;
      end;
    end;
  finally
    Unlock;
  end;

  if thr <> nil then begin
//    Debug.Log('Quickpool removed thread that was '+tmsince.tostring+'ms. old, stopping');
    thr.loop := false;
    thr.stop;
    thr.WaitFor;
    TPM.NoNeedThread(thr);
//    Debug.Log('Quickpool removed, stopped');
  end;

{$ENDIF}
end;

procedure TCPThreadPoolGrowthManager.Detach;
begin
  if Detached then exit;
  GiveUpThreads;
  inherited;

end;

procedure TCPThreadPoolGrowthManager.DoExecute;
const
  NEED_SILENCE_GIVE_UP = 50;
  RAPID_NEED_TIME = 10;

var
  expanded: TcommandProcessorChildThread;
begin
  inherited;
//  Debug.Log('check expand');
  if (IdleCount =0) or ((tmLastNeedtime > 0) and (gettimesince(tmLastNeedTime) < RAPID_NEED_TIME) and (IdleCount < 4)) then begin //if there are no threads in reserve then make some
//    Debug.Log('epanding');
    expanded := TPM.Needthread<TCommandProcessorChildThread>(nil);
    expanded.lastused := getticker;
    expanded.loop := true;
    expanded.haswork := false;

    //Debug.Log('got thread waiting for out of pool');
    WaitForSignal(expanded.evOutOfPool);
    Lock;
    try
      //Debug.Log('put in list');
      FList.add(expanded);
    finally
      Unlock;
    end;
    expanded.beginstart;
    expanded.endstart;
    runHot := true;
  end else
    runHot := false;

  coldruninterval := 50;
  if ((tmLastNeedtime > 0) and (gettimesince(tmLastNeedTime) > NEED_SILENCE_GIVE_UP) and (IdleCount > 4)) then begin
    CleanStale;
  end;


end;


function TCPThreadPoolGrowthManager.GetIdleCount: ni;
begin
  Lock;
  try
    result := FList.count;
  finally
    Unlock;
  end;
end;

procedure TCPThreadPoolGrowthManager.GiveUpThreads;
var
  thr: TCommandProcessorChildThread;
begin
  Lock;
  try
    while IdleCount > 0 do begin
      thr := FList[0];
      FList.remove(thr);
      thr.loop := false;
      thr.stop;
      thr.WaitFor;
      TPM.NoNeedThread(thr);
    end;
  finally
    Unlock;
  end;
end;

procedure TCPThreadPoolGrowthManager.Init;
begin
  inherited;
  Loop := true;
  FList := TLLL.create;

end;

procedure TCPThreadPoolGrowthManager.InitFromPool;
begin
  inherited;
  Loop := true;
end;

function TCPThreadPoolGrowthManager.NEedThread: TCommandProcessorChildThread;
begin

  result := nil;

  while result = nil do begin
    Lock;
    try

      if IdleCount > 0 then begin
        result := FList[0];
        FList.remove(result);
        result.lastused := getticker;
//        Debug.Log('found idle thread');
      end;
    finally
      Unlock;
    end;

    if result = nil then
      sleep(100);

  end;
  tmLastNeedtime := getticker;

end;

procedure TCPThreadPoolGrowthManager.NoNeedThread(thr: TCommandProcessorChildThread);
begin
  Lock;
  try
    FList.Add(thr);
    thr.lastused := getticker;
  finally
    Unlock;
  end;
end;

initialization
  init.RegisterProcs('CommandProcessor', oinit, ofinal,'ManagedThread');



end.


