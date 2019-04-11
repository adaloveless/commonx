unit managedthread;
{$MESSAGE '*******************COMPILING ManagedThread.pas'}
{$I 'DelphiDefs.inc'}
{$DEFINE NEW_LOOP}
{$DEFINE GWATCH}
interface
{x$INLINE AUTO}
{x$DEFINE SUSPEND_ACK}
{x$DEFINE DEBUG_THREAD_NAMES}
{x$DEFINE DISABLE_THREAD_POOL}
{x$DEFINE DETAILED_DEBUG}
{x$DEFINE QUICK_THREAD_DEBUG}

uses
  system.RTLConsts,//inline
  system.Types, numbers, better_collections,
{$IFDEF STACK_TRACE}
  StackTrace,
{$ENDIF}
  betterobject, Classes,syncobjs, tickcount, signals,
{$IFDEF MSWINDOWS}
  winapi.windows,
  winapi.activex,//<--no coinitialize
{$ENDIF}
  ThreadManager, typex, systemx, sysutils, sharedobject, Generics.collections.fixed, orderlyinit;
const
  DEFAULT_THREAD_POOL_TIME = 60000;
type
  EThreadViolation = class(Exception);
//  TManagedThreadState = (mtsInit, mtsNotStarted, mtsStartRequested, mtsRunning, mtsFinished, mtsTerminated);
  TMAsterThreadPool = class;
  TThreadPoolBase = class;
  TThreadManager = class;
  TExternalEventThread = class;
  TExternalThreadExecutionEvent = procedure(sender: TExternalEventThread) of object;
  TThreadPoolStats = record
    Active: ni;
    Incoming: ni;
    Pooled: ni;
  end;



  TManagedThread = class;//forward
{$IFDEF MSWINDOWS}
  TThreadPriority = system.classes.TThreadPriority;
{$ELSE}
  TThreadPriority = integer;
{$ENDIF}

  TManagedSignal = class(TSignal)
  private
    FName: string;
  public
    SetOnlyByThread: cardinal;
    ClearedOnlyByThread: cardinal;
    ClearedNotByThread: cardinal;
    SetNotByThread: cardinal;
    NoWaitByThreadid: cardinal;
    thr: TManagedThread;
    ignorechecks: boolean;
    procedure Init;override;
    procedure OnlyForThisThread;
    procedure NotForThisthread;
    procedure Signal(tf: boolean);override;
    function WaitFor(iTimeout: ni = -1): boolean;override;
    property Name: string read FName write FName;
  end;

  TRealManagedThread = class(TThread)
  protected
    mt: TManagedThread;
    procedure Execute; override;
  public
  end;

  TThreadInfo = record
    spin: boolean;
    autospin: boolean;
    name: string;
    status: string;
    iterations: int64;
    step: int64;
    stepcount: int64;
    error: string;
    lastused: ticker;
    pooled: boolean;//requires refresh on extract
    Blocked: boolean;
    threadid: int64;//copied when real thread assigned
    signaldebug: string;
    coldruninterval: nativeint;
  private
    function GetAge: ticker;
  public
    property Age: ticker read GetAge;
  end;


  TManagedThread = class(TsharedObject)
  protected
    FTimes: array[0..19] of ticker;
    FTimeIndex: nativeint;
    Finfo: TThreadInfo;

    FthreadManager: TThreadManager;
    FLastUsed: cardinal;
    FOwner: TObject;
    FUnknown: TBetterObject;

    FMenu: TStringlist;
    FPool: TThreadPoolBase;
    function GEtError: string;
    procedure SetError(const Value: string);
    procedure SetAutoSpin(const Value: boolean);
    procedure SEtSpin(const Value: boolean);
    function GetMenu(idx: NativeInt): string;

    function GetPooled: boolean;
    function GetPool: TThreadPoolBase;
    procedure SetPool(const Value: TThreadPoolBase);

    procedure StepComplete(iStepCount: NativeInt);
    function GetAGe: cardinal;
    function GetOwner: TObject;

    procedure SetOwner(const Value: TObject);

    procedure Queue(m: TThreadMethod);


    function GetLastUsed: cardinal;
    procedure SetLastUsed(const Value: cardinal);

    function GetStatus: string;virtual;
    function GetIterations: NativeInt;virtual;
    function GetStepCount: NativeInt;virtual;
    function GetStep: NativeInt;virtual;
    function GetAutoSpin: boolean;virtual;
    function GEtSpin: boolean;virtual;



    procedure SetIterations(const Value: NativeInt);
    procedure SetStatus(const Value: string);virtual;
    function GetName: string;virtual;
    procedure SetName(const Value: string);
    function GetThreadManager: TThreadManager;
    procedure SetThreadManager(const Value: TThreadManager);

    procedure SetStepCount(const Value: NativeInt);

    procedure SetStep(const Value: NativeInt);
    function GEtReady: boolean;
    procedure SEtReady(const Value: boolean);

  private
    FStop: boolean;
    FMMTaskHandle: THandle;
    FReRaiseExceptionsInLoop: boolean;
    FPoolLoop: boolean;
    FLoop: boolean;
    FDebugName: string;
    FPendingPoolAddition: boolean;
    beforestartcalled: boolean;
    FOnIdle: TNotifyEvent;
    FIdleIterations: ni;
    FComReferences: integer;
    FAutoCycle: boolean;
    procedure SetStop(const Value: boolean);
    function GetUnknown: IUnknown;
    procedure DoTermiante;
    function GetHasWork: boolean;
    procedure SetHasWork(const Value: boolean);
    function GetRunHot: boolean;
    procedure SetRunHot(const Value: boolean);
    function GEtAverageExecutionTime: nativefloat;
    function GetStartCalled: boolean;
    function GetNameEx: string;
    function GetIsComplete: boolean;
    function GetIsFinished: boolean;
    function GetPlatformPriority: integer;
    procedure SetPlatformPriority(const Value: integer);
    function GetHandle: THandle;
    function GetBetterPriority: TBetterPriority;
    procedure SetBetterPriority(const Value: TBetterPriority);
    function Getinfo: TThreadInfo;
  protected
    procedure BeforeDoExecute;virtual;
    procedure DoExecute;virtual;
    procedure AfterDoExecute;virtual;
    procedure OnFinish;virtual;

    procedure Execute; virtual;
    procedure IdleCycle;virtual;
    procedure RemoveFromPool;
    procedure TerminatedSet; virtual;


    class var instancecount: integer;
    class procedure IncInstanceCount;
    class procedure DecInstanceCount;
  protected
    FWaitBeforeAbortTime: ticker;
    evHot, evIdle, evHasWork, evStart, evStarted, evStop, evFinished, evOutOfPool, evSleeping, evPoolReady, evConcluded: TManagedSignal;
    function StopREquested: boolean;
    function IdleBreak: boolean;virtual;
    function GetThreadID: cardinal;

  public
    realthread: TRealManagedThread;
    NeverStarted: boolean;
    FromPool: TThreadPoolBase;
    IsCriticalSystemThread: boolean;
    tmLastStatusUpdate: ticker;
    beginstoptime: ticker;
    StatusUpdated: boolean;
    NoWorkRunInterval: single;

    property Info: TThreadInfo read Getinfo;
    property AutoCountCycles: boolean read FAutoCycle write FAutoCycle;
    procedure Terminate;
    procedure WaitForFinish;virtual;
    procedure WaitFor;reintroduce;virtual;
    procedure WaitForLowLevel;
    procedure Stop(bySelf: boolean=false);virtual;
    procedure BeginStop(bySelf: boolean=false);
    procedure EndStop(bySelf: boolean=false);
    function GetSignalDebug: string;
    property WAitBEforeAbortTime: ticker read FWaitBeforeAbortTime write FWaitBEforeAbortTime;
    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;

    procedure SetStartingState;virtual;
    procedure Init;override;
    constructor Create(Owner: TObject; Manager: TThreadManager; pool: TThreadPoolBase);reintroduce;virtual;
    destructor Destroy; override;
    procedure InitSignalState;
    property HasWork: boolean read GetHasWork write SetHasWork;
    property RunHot: boolean read GetRunHot write SetRunHot;
    procedure SignalAllForTerminate;
    procedure IdleStateTasks;
    procedure InitFromPool;virtual;
    //--------------------------------------------------------------------------
    property Name: string read GetName write SetName;
    property NameEx: string read GetNameEx;
    property Status: string read GetStatus write SetStatus;
    procedure IterationComplete;inline;
    property Iterations: NativeInt read GetIterations write SetIterations;
    property StepCount: NativeInt read GetStepCount write SetStepCount;
    property Step: NativeInt read GetStep write SetStep;
    property Manager: TThreadManager read GetThreadManager write SetThreadManager;

    property Ready: boolean read GEtReady write SEtReady;
    procedure InterationComplete;

    property AutoSpin: boolean read GetAutoSpin write SetAutoSpin;
    property Spin: boolean read GEtSpin write SEtSpin;
    property Error: string read GEtError write SetError;
    procedure WaituntilReady;
    procedure FreeAndTerminate;
    property Menu[idx: NativeInt]: string read GetMenu;
    function MenuCount: NativeInt;
    procedure AddMEnuItem(s: string);
    procedure MenuAction(idx: NativeInt);virtual;
    procedure BuildMenu;virtual;
    function MenuBuilt: boolean;
    property LastUsed: cardinal read GetLastUsed write SetLastUsed;
    property Age: cardinal read GetAGe;
    function IsCurrentThread: boolean;
    class function Need(owner: TObject; pool: TMasterThreadPool = nil):TManagedThread;
    property Pooled: boolean read GetPooled;
    property Pool: TThreadPoolBase read GetPool write SetPool;
    property Owner: TObject read GetOwner write SetOwner;
    property Blocked: boolean read Finfo.Blocked write Finfo.Blocked;
    procedure Detach; override;
    procedure SetMM(sType: string);
    procedure ClearMM;
    procedure DetachAndFree;
    function WaitForSafeX(iTimeout: nativeint): boolean;
    procedure AssertThatIsThisThread;
    procedure AssertThatNotThisThread;
    //property Unknown: IUnknown read GetUnknown implements IUnknown;

    property PoolLoop: boolean read FPoolLoop write FPoolLoop;
    property Loop: boolean read FLoop write FLoop;
    property BreakLoopOnException: boolean read FReRaiseExceptionsInLoop write FReRaiseExceptionsInLoop;
    procedure SafeREsume;
    procedure SafeWaitFor;
    function SafeTerminated: boolean;
    procedure Start;virtual;
    procedure BeforeStart;virtual;
    procedure BeginSTart;
    procedure EndStart;
    property AverageExecutionTime: nativefloat read GEtAverageExecutionTime;
    property ColdRunInterval: nativeint read Finfo.ColdRuninterval write finfo.ColdRunInterval;
    property PendingPoolAddition: boolean read FPendingPoolAddition write FPendingPoolAddition;
    procedure PrepareForPool;virtual;
    procedure ToPool(p: TMasterThreadPOol = nil);
    property IdleIterations: ni read FIdleIterations write FIdleIterations;
    property StartCalled: boolean read GetStartCalled;
    procedure Sleep(ms: ni);inline;
    property IsFinished: boolean read GetIsFinished;
    property IsComplete: boolean read GetIsComplete;
    function Started: boolean; reintroduce;
    property ThreadID: cardinal read GetthreadID;
    property Handle: THandle read GetHandle;
    property PlatformPriority: integer read GetPlatformPriority write SetPlatformPriority;
    property BetterPriority: TBetterPriority read GetBetterPriority write SetBetterPriority;
    procedure Synchronize(AMethod: TThreadMethod); overload;
    procedure Synchronize(AThreadProc: TThreadProcedure); overload;
    class procedure Synchronize(const AThread: TThread; AMethod: TThreadMethod); overload; static;
    class procedure StaticSynchronize(const AThread: TThread; AMethod: TThreadMethod); static; deprecated 'From C++ just use Synchronize now that it is just a static method';
    class procedure Synchronize(const AThread: TThread; AThreadProc: TThreadProcedure); overload; static;
    procedure WaitforConclusion;
    function Terminated: boolean;
    procedure WaitForIdle;
    function IsIdle: boolean;

  end;

  TFireWaitThread = class(TManagedThread)
  public
    procedure InitFromPool;override;
    procedure DoExecute;override;
    procedure Fire;
  end;

  TProcessorThread = class(TManagedThread)
  protected
    function GetComplete: boolean;
    procedure SetComplete(b: boolean);
  public
    function IsStartable: boolean;
    procedure Start;override;

    property Complete: boolean read GetComplete write SetComplete;
  end;




  TMasterThread = class(TProcessorThread)
  private
    FTHreads: TThreadManager;
    function GetThreads: TThreadManager;
  public
    constructor Create(Owner: TObject; Manager: TThreadManager; pool: TThreadPoolBase); override;
    destructor Destroy; override;
    property Threads: TThreadManager read GetThreads;
  end;

  TStringProcessorThread = class(TProcessorThread)
  private
    Fresult: string;
    function GetResult: string;
    procedure SetResult(const Value: string);
  public
    property Result: string read GetResult write SetResult;
  end;

  TExternalEventThread = class(TManagedThread)
  private
    FOnDetach, FOnExecute: TExternalThreadExecutionEvent;
//    FLoop: boolean;
    FInEvent: boolean;
//    FReRaiseExceptionsInLoop: boolean;
    function GetTerminateRequested: boolean;
    procedure SetTerminateREquested(const Value: boolean);
    procedure SetInEVent(const Value: boolean);
    function GetInEvent: boolean;
  public
    //property Terminated;
    constructor Create(Owner: TObject; Manager: TThreadManager; pool: TThreadPoolBase);overload;override;
    constructor Create(Owner: TObject; Manager: TThreadManager; pool: TThreadPoolBase; Loop: boolean; ExecuteEvent: TExternalThreadExecutionEvent); reintroduce;overload;virtual;
    destructor Destroy;override;
    property OnDetach: TExternalThreadExecutionEvent read FOnDetach write FOnDetach;

    property OnBeforePoolOrTerminate:TExternalThreadExecutionEvent read FOnExecute write FOnExecute;
    property OnCreateOrWakeFromPool:TExternalThreadExecutionEvent read FOnExecute write FOnExecute;
    property OnExecute: TExternalThreadExecutionEvent read FOnExecute write FOnExecute;
    procedure DoExecute;override;

    property TerminateREquested:boolean read GetTerminateRequested write SetTerminateREquested;
    property InEvent: boolean read GetInEvent write SetInEVent;
    procedure InitFromPool;override;
  end;

  TManagedThreadClass = class of TManagedThread;

  TDoNothingThread = class(TManagedThread)
  public
    [volatile] a,b: ni;
    [volatile] c: ni;
    [volatile] t: ni;
    procedure DoExecute;override;
  end;

  TThreadPoolBase = class(TSharedObject)
  private
    FFreeOnEmpty: boolean;
    FThreadPoolTimeout: nativeint;
    function GEtIsEmpty: boolean;
  protected
    Fthreads: TBetterList<TManagedThread>;//
    FincomingThreads: TBetterList<TManagedThread>;
    FActiveThreads: TBetterList<TManagedThread>;
    Fc: TManagedThreadClass;
    procedure Clear;virtual;//
    function Clean: TManagedThread;
  public
    property ThreadPoolTimeout: nativeint read FThreadPoolTimeout write FThreadPoolTimeout;
    constructor Create;override;//
    destructor Destroy;override;//
    function BaseNeedThread(owner: TObject): TManagedThread;
    procedure BaseNoNeedThread(thr: TManagedThread);
    property C: TManagedThreadClass read Fc write Fc;
    procedure ForceRemove(thr: TManagedThread);
    procedure FreeWithThreads;
    property FreeOnEmpty: boolean read FFreeOnEmpty;
    property IsEmpty: boolean read GEtIsEmpty;
    function GetStats(out iIncoming,iActive,iPooled: nativeint): nativeint;overload;
    function GetStats: TThreadPoolStats;overload;
    function GetStatMessage: string;
    procedure WakeAndFreeAllThreads(bWait: boolean);

  end;

  TThreadpool = class(TThreadPoolBase);


  TMasterThreadPool = class (TSharedObject)
  private
    termlist: TSharedList<TMAnagedThread>;
    FFreeOnEmpty: boolean;
    FhighPerformanceMode: boolean;
    FShutdown: boolean;
    procedure WakeAndFreeAllThreads;
  protected
    Fpools: TBetterList<TThreadPoolBase>;//
    FCleaner: TExternalEventthread;//
    FCleanIndex: NativeInt;
    procedure Clear;//

    function CheckTermList: boolean;
    procedure Clean;//
    procedure OnThreadExecute(sender: TExternalEventThread);//
  public
    procedure StartCleaner;//
    procedure StopCleaner;//

    constructor Create;override;//
    destructor Destroy;override;//

    function NeedthreadPool(c: TManagedthreadClass): TThreadPoolBase;
    function Needthread(c: TManagedThreadClass; owner: TObject): TManagedThread;overload;
    function NeedThread<T: TManagedThread>(owner: TObject): T;overload;

    procedure NoNeedthread(thr: TManagedthread);
    procedure ForceRemove(thr: TManagedThread);
    procedure FreeWithThreads;
    property FreeOnEmpty: boolean read FFreeOnEmpty;
    property HighPerformanceMode: boolean read FhighPerformanceMode write FhighPerformanceMode;
    property Shutdown: boolean read FShutdown write FShutdown;
  end;

  TThreadManager = class(TFakeLockQueuedObject)
  //a: Jason Nelson
  //TThreadManager is essentially a list of Threads.  It can handle any kind of thread, but also
  //implements a few extra bells and whistles if the threads it manages are
  //descendents of TManagedThread.  TManagedThread will report back some
  //handy statistics about thread status, health, and will enable applications
  //to quickly locate background threads and check on their status through the
  //handy "HasThreadClass" and "HasThreadName" functions.  These two functions make
  //it relatively easy to spawn parellel processing and check-up on the status
  //of the processing later in your function.  Particularly if your thread is
  //set to free on terminate.  In this situation you can simply spawn a thread with
  //a unique name, then check the threadmanager until the thread is no longer in
  //the managed list... then you know your thread is done processing.
  private
    function GetCountActiveThreads: integer;
    function GEtCountReadyThreads: integer;
    function GetCount: integer;
    function GetThreads(idx: integer): TManagedThread ;
  protected
    FThreads: TBetterList<TManagedThread>;
  public
    Destroying: boolean;
    constructor Create; override;
    destructor Destroy; override;
    procedure RegisterThread(thread: TManagedThread);overload;
    procedure DeRegisterThread(thread: TManagedThread);overload;
    procedure WaitForThreads;
    property Count: integer read GetCount;
    //Number of threads tracked by the manager.
    property CountReadyThreads: integer read GEtCountReadyThreads;
    property CountActiveThreads: integer read GetCountActiveThreads;

    property Threads[idx: integer]: TManagedThread read GetThreads;default;
    //Array property that indexes all threads managed by the class.
    function HasThread(thread: TManagedThread): boolean;
    function HasThreadName(sName: string): boolean;
    function HasThreadClass(threadclass: TManagedThreadClass): boolean;
    function ThreadClassCount(threadclass: TManagedThreadClass): integer;
    procedure TerminateAllThreads;
    procedure DetachAllThreads;
    function GetInfoList: TArray<TThreadInfo>;
  end;



{$IFDEF WINDOWS}
function AvSetMmThreadCharacteristicsW(s: PChar; var TaskIndex: integer): THandle; stdcall; external 'Avrt.dll' delayed;
function AvRevertMmThreadCharacteristics(taskHandle: THandle): boolean; stdcall; external 'Avrt.dll'  delayed;
{$ELSE}
function AvSetMmThreadCharacteristicsW(s: PChar; var TaskIndex: integer): THandle; stdcall;
function AvRevertMmThreadCharacteristics(taskHandle: THandle): boolean; stdcall;
{$ENDIF}

function AvSetMmThreadCharacteristics_Safe(s: PChar; var TaskIndex: integer): THandle; stdcall;
function AvRevertMmThreadCharacteristics_Safe(taskHandle: THandle): boolean;  stdcall;


var
  gwatchthread: nativeint;
  KillFlag: boolean;
  Killed: boolean;
  TPM: TMasterThreadPool;
  thread_shutdown_tracking_stage: nativeint;




var
  dntdebug: ni;
  dntdebug_t: ni;

implementation

uses backgroundthreads, debug, unittest;

type
  TUnitTestThread = class(TMAnagedThread)
  public
    procedure DoExecute;override;
  end;



  TUT_ManagedThread = class(TUnitTest)
  protected
    Fthr: TUnitTestThread;
  public
    procedure DoExecute;override;
  end;




{$IFNDEF WINDOWS}
function AvSetMmThreadCharacteristicsW(s: PChar; var TaskIndex: integer): THandle; stdcall;
begin
  result := 0;
end;
function AvRevertMmThreadCharacteristics(taskHandle: THandle): boolean; stdcall;
begin
  result := false;
end;
{$ENDIF}


procedure TManagedThread.AddMEnuItem(s: string);
begin
  Lock;
  try
    FMenu.add(s);
  finally
    Unlock;
  end;

end;

procedure TManagedThread.AfterDoExecute;
begin
  //
end;

procedure TManagedThread.AssertThatIsThisThread;
begin
  if TThread.CurrentThread.ThreadID <> threadid then
    raise EThreadViolation.create('You cannot call this function from foreign thread.');
end;

procedure TManagedThread.AssertThatNotThisThread;
begin
  if TThread.CurrentThread.ThreadID = threadid then
    raise EThreadViolation.create('You cannot call this function from this thread.');
end;


procedure TManagedThread.BeforeDoExecute;
begin

//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TManagedThread.BeforeStart;
begin
  //
  beforestartcalled := true;
end;

procedure TManagedThread.BuildMenu;
begin
  if FMenu = nil then begin
    FMenu := TStringList.create;
  end;
  FMenu.clear;
end;

constructor TManagedThread.Create(Owner: TObject;
  Manager: TThreadManager; pool: TThreadPoolBase);
var
  h: THandle;
begin
  Finfo.Name := Classname;
  evStart := TManagedSignal.Create;
  evOutOfPool := TManagedSignal.create;
  evSleeping := TManagedSignal.create;
  evPoolReady := TManagedSignal.create;
  evStarted := TManagedSignal.Create;
  evHasWork := TManagedSignal.create;
  evIdle := TManagedSignal.create;
  evFinished := TManagedSignal.Create;
  evHot := TManagedSignal.create;
  evConcluded := TManagedSignal.create;


  //do normal startup operations
  inherited Create;
  realThread := TRealManagedThread.create(true);
  FInfo.threadid := realThread.threadid;
  realThread.mt := self;
  {$IFDEF STACK_TRACING}InitStackTrace;{$ENDIF}
  IdleIterations := 1;
  NeverSTarted := true;
  ColdRunInterval := 1000;
  evStop := TManagedSignal.Create;
  evStop.Name := 'evStop';
  evStop.thr := self;
  evStop.SetNotByThread := self.ThreadID;
  evStop.ignoreChecks := true;


  evStart.Name := 'evStart';
  evStart.thr := self;
  evStart.SetNotByThread := self.ThreadID;
  evStart.ignoreChecks := true;

  evOutOfPool.Name := 'evOutOfPool';
  evOutOfPool.thr := self;
  evOutOfPool.SetNotByThread := self.ThreadID;
  evOutOfPool.ignoreChecks := true;


  evSleeping.Name := 'evSleeping';
  evSleeping.thr := self;
  evSleeping.SetOnlyByThread := self.ThreadID;
  evSleeping.ignoreChecks := true;
  evSleeping.Signal(true);

  evPoolReady.Name := 'evPoolReady';
  evPoolReady.thr := self;
  evPoolReady.ignoreChecks := true;
  evPoolReady.Signal(true);

  evStarted.Name := 'evStarted';
  evStarted.thr := self;
  evStarted.SetOnlyByThread := self.ThreadID;
  evStarted.ClearedNotByThread := self.ThreadID;
  evStarted.ignoreChecks := true;

  evFinished.Name := 'evFinished';
  evFinished.thr := self;
  evFinished.SetOnlyByThread := self.ThreadID;
  evFinished.ClearedNotByThread := self.ThreadID;
  evFinished.ignoreChecks := true;

  evHasWork.Name := 'evHasWork';
  evHasWork.thr := self;
  evHasWork.ignoreChecks := true;

  evIdle.Name := 'evIdle';
  evIdle.thr := self;
  evIdle.NoWaitByThreadid := self.ThreadID;


  evHot.Name := 'evHot';
  evHot.thr := self;
  evHot.ignoreChecks := true;

  evHasWork.Signal(true);
  evHot.Signal(true);
  evOutOfPool.Signal(true);

  evConcluded.Signal(false);
  evConcluded.Name := 'evConcluded';
  evConcluded.thr := self;
  evConcluded.SetOnlyByThread := self.ThreadID;

//  Name := 'unnamed '+self.ClassName;
//  Debug.Log('NEW Thread '+self.nameex);
  IncInstanceCount;

  PoolLoop := true;

  Funknown := TBetterObject.create;

{$IFNDEF IOS}
  h := INVALID_HANDLE_VALUE;
  FMMTaskHandle := cardinal(PDWord(@h)^);
{$ENDIF}

  Finfo.StepCount := 0;
  Finfo.Step := 0;
  Finfo.Iterations := 0;

  //initialize critical section
  //initialize status vars
  {$IFDEF MORE_THREAD_STATUSES}
  Fstatus := 'Init';
  {$ENDIF}



  //register if applicable with a threadmanager
  FOwner := owner;

  if manager <> nil then
    self.Manager := manager as TThreadManager
  else
    self.Manager := BackGroundThreadMan;
  Init;
//  if not CreateSuspended then
  SetStartingState;
  signal(evSleeping,true);//special case for when we DON'T use TPM.NeedThread to create threads
  signal(evOutOfPool,true);//special case for when we DON'T use TPM.NeedThread to create threads
  realthread.start;
  if pool = nil then begin
    raise ECritical.create('deprecated');
    signal(evSleeping,true);
    signal(evOutOfPool,false);
  end;

  signal(evSleeping,true);


end;


procedure TManagedThread.InterationComplete;
begin
//  Lock;
//  try
    inc(Finfo.Iterations);
    if Finfo.Iterations>2000000000 then
      Finfo.Iterations := 0;

//  finally
//    unlock;
//  end;

end;

class procedure TManagedThread.DecInstanceCount;
begin
{$IFDEF WINDOWS}
  interlockeddecrement(Self.instancecount);
  //if thread_shutdown_tracking_stage > 0 then
    Debug.Log(nil,'--There are now '+inttostr(instancecount)+' threads.');
{$ENDIF}
end;

destructor TManagedThread.Destroy;
begin


  if thread_shutdown_tracking_stage > 0 then
    Debug.Log(self, classname+' should be destroyed already at shutdown stage '+inttostr(thread_shutdown_tracking_stage)+'!!!!!!!!!!!!!!!!!');

//  Debug.Log(self.classname+'('+inttostr(threadid)+') "'+self.name+'" is being destroyed');
  if Pooled then RemoveFromPool;

  if realthread.Suspended and not realthread.Terminated then begin
    realthread.resume;
    realthread.terminate;
  end;
  if not realthread.terminated then begin
    realthread.terminate;
  end;
  realthread.waitfor;
{$IFDEF WINDOWS}
  waitforsingleobject(realthread.Handle, 0);
{$ENDIF}

  poolloop := false;
  loop := false;
  signal(evSleeping,true);
  Debug.Log('Destroy '+nameex);

  WaitForSignal(evConcluded);
  Debug.Log('Concluded '+nameex);

  Manager := nil;
  if not realthread.Terminated and (not realthread.FreeOnTerminate) then begin
    realthread.terminate;
  end;

  if assigned(FromPool) then
    FromPool.ForceRemove(self);

  FromPool := nil;

  //sleepex(100);

  Funknown.Free;
  if assigned(FMenu) then
    FMEnu.free;
  FMenu := nil;
  inherited;
  DecInstanceCount;
  evStart.Free;
  evStarted.Free;
  evFinished.Free;
  evStop.free;
  evHasWork.Free;
  evIdle.free;
  evHot.free;
  evOutOfPool.free;
  evSleeping.free;
  evPoolReady.free;


  {$IFDEF STACK_TRACING}CleanupStackTrace;{$ENDIF}

end;

procedure TManagedThread.Detach;
begin
//
  Self.Manager := nil;
  inherited;
end;

procedure TManagedThread.DetachAndFree;
begin
  if self = nil then exit;

  Detach;
  Free;
end;

procedure TManagedThread.DoExecute;
begin
  //override me!
end;

procedure TManagedThread.DoTermiante;
begin
  inherited;
  SignalAllForTerminate;
end;

procedure TManagedThread.Terminate;
begin
  realthread.terminate;
  SignalallForTerminate;
end;

function TManagedThread.Terminated: boolean;
begin
  result := realthread.terminated;
end;

procedure TManagedThread.TerminatedSet;
begin
  PoolLoop := false;
{$IFDEF NEW_LOOP}
  Signal(evStop);
  Loop := false;

{$ENDIF}

end;

procedure TManagedThread.ToPool(p: TMasterThreadPOol);
begin
  if p = nil then
    p.NoNeedThread(self)
  else
    TPM.NoNeedthread(self);
end;

//------------------------------------------------------------------------------
procedure TManagedThread.Execute;
var
  tm1, tm2: ticker;
  bIdleCycle: boolean;
label
  rep_not_continue;
begin
  try
    try
      SetDebugThreadVar(self);
    {$IFDEF DETAILED_DEBUG}
          Debug.Log(self,'Native Execute: '+self.classname);
    {$ENDIF}
    {$IFDEF WINDOWS}
      winapi.activex.coinitialize(nil);
    {$ENDIF}
      while not realthread.terminated do begin
        repeat
          try
    {$IFDEF QUICK_THREAD_DEBUG}Debug.Log('Starting new cycle '+GetSignalDebug+', not terminated loop '+nameex);{$ENDIF}
            while not WaitForSignal(evOutOfPool, 4000) do begin // REALLY Rare race condition can occur
            {$IFDEF QUICK_THREAD_DEBUG}Debug.Log(GetSignalDebug+'Wait Timeout for evOutOfPool');{$ENDIF}
              if realthread.terminated then begin                   // where evStart is cleared and loses start signal required for terminate
                Signal(evStarted);
                SignalAllForTerminate;
                exit;
              end;
              if Killed then begin
                Signal(evStarted);
                Debug.ConsoleLog(self.classname+' named '+self.name+' is still active after thread-pool is killed.');
                raise ECritical.create(self.classname+' named '+self.name+' is still active after thread-pool is killed.');
              end;
            end;


            Signal(evIdle, true);
            while not WaitForSignal(evStart, 4000) do begin // REALLY Rare race condition can occur
                                                            // where evStart is cleared and loses start signal required for terminate
              {$IFDEF QUICK_THREAD_DEBUG}Debug.Log(GetSignalDebug+'Wait Timeout for evStart');{$ENDIF}
              if realthread.terminated then begin
                Signal(evStarted);
                SignalAllForTerminate;
                exit;
              end;
              if Killed then begin
                Signal(evStarted);
                Debug.ConsoleLog(self.classname+' named '+self.name+' is still active after thread-pool is killed.');
                raise ECritical.create(self.classname+' named '+self.name+' is still active after thread-pool is killed.');
              end;
            end;

{$IFDEF GWATCH}
            if self.threadid = GWATCHTHREAD then
              Debug.Log('Trap Watch '+GetsignalDebug);
{$ENDIF}

    {$IFDEF QUICK_THREAD_DEBUG}Debug.Log('going '+getsignaldebug+' '+nameex);{$ENDIF}
            if IsSignaled(evStop) then begin
              {$IFDEF QUICK_THREAD_DEBUG}Debug.Log('STOPPING IMMEDIATELY '+getsignaldebug+' '+nameex);{$ENDIF}
              break;
            end;
            if not beforestartcalled then
              BeforeSTart;
            Signal(evStart, false);
            //Signal(evStop, false);
            Signal(evStarted);
            if realthread.terminated then exit;//<<---- when terminating, the signal will be set, it could be set just for the purpose of termination wakeup


      {$IFDEF DEBUG_THREAD_NAMES}
            if FName <> FDebugName then
              Realthread.NameThreadForDebugging(FName);
      {$ENDIF}
            FDebugName := Finfo.Name;
      {$IFDEF DETAILED_DEBUG}
            Debug.Log(self,'Execute: '+self.classname);
      {$ENDIF}
            {$IFDEF NEW_LOOP}
            repeat
            {$ENDIF}
rep_not_continue:
              Signal(evIdle, true);
              if not WaitForSignal(evHasWork, round(NoWorkRunInterval)) then //evaluate conditions under lock ALWAYS
              begin
                //IDLE CYCLE
                if realthread.terminated then exit;//<<---- when terminating, the signal will be set, it could be set just for the purpose of termination wakeup
                IdleCycle;

                goto rep_not_continue;
              end;


              Signal(evIdle, false);
              if IsSignaled(evStop) then begin
                Signal(evStarted,true);
                break;
              end;

              BeforeDoExecute;
              try
                tm1 := tickcount.GetTicker;
{$IFDEF GWATCH}
                if self.threadid = GWATCHTHREAD then
                  Debug.Log('Trap Watch '+GetsignalDebug);
{$ENDIF}

                DoExecute;
                if AutoCountCycles then
                  inc(Finfo.Iterations);
                tm2 := tickcount.GetTicker;
                FTimeIndex := FTimeIndex mod length(FTimes);
                FTimes[FTimeIndex] := GetTimeSince(tm2, tm1);
                inc(FTimeIndex);

              except
                on E:Exception do begin
                  Debug.Log(self, 'Unhandled exception in '+self.classname+' id:'+inttostr(threadid)+':' +e.message);

                end;
              end;
              AfterDoExecute;
              //IdleStateTasks;
              if IsSignaled(evStop) then begin
                Signal(evStarted,true);
                break;
              end;

             if not IsSignaled(evHot) then
              WaitForSignal(evHot, ColdRunInterval) //skip waiting for hot signal because this is a no-work cycle
            {$IFDEF NEW_LOOP}

            until (not loop) or IsSignaled(evStop);
            {$endIF}
    //        Debug.Log('Reached out of user loop');
            OnFinish;
{$IFDEF GWATCH}
            if self.threadid = GWATCHTHREAD then
              Debug.Log('Trap Watch '+GetsignalDebug);
{$ENDIF}


          finally
            Signal(evFinished);
            if not IsCriticalSystemThread then
              WaitForSignal(evSleeping);
            evPoolReady.Signal(true);
          end;
        until not poolloop;


  {$IFDEF QUICK_THREAD_DEBUG}Debug.Log(self.nameex+' Finished loop '+GetSignalDebug);{$ENDIF}
        Signal(evFinished);
  //      Signal(evStarted, false);

      end;
    except
      on E: Exception do begin
       DEbug.Log('CATASTROPHE! in '+self.classname+'--'+E.message);
      end;
    end;
  finally

{$IFDEF GWATCH}
    if self.threadid = GWATCHTHREAD then
      Debug.Log('Trap Watch');
{$ENDIF}

    {$IFDEF QUICK_THREAD_DEBUG}Debug.Log(self.nameex+' Concluded '+GetSignalDebug);{$ENDIF}
    Signal(evFinished);
    Signal(evConcluded);
  end;

end;
procedure TManagedThread.FreeAndTerminate;
begin
  if realthread.Terminated then begin  //TODO 2: TEST THIS... thread leaks? Hangs?
    free;
  end
  else begin

    self.Manager := nil;
    realthread.FreeOnTerminate := true;
    realthread.Terminate;
  end;

end;


function TManagedThread.GetHandle: THandle;
begin
{$IFNDEF MSWINDOWS}
  raise Ecritical.create('supported only on windows');
{$ELSE}
  result := realthread.Handle;
{$ENDIF}
end;

function TManagedThread.GetHasWork: boolean;
begin
  result := IsSignaled(evHasWork);
end;

function TManagedThread.Getinfo: TThreadInfo;
begin
  result := FInfo;
  result.pooled := FPool <> nil;//volatile
end;

function TManagedThread.GetIsComplete: boolean;
begin
  result := IsSignaled(evFinished);
end;

function TManagedThread.GetIsFinished: boolean;
begin
  result := IsSignaled(evFinished);
end;

function TManagedThread.GetIterations: NativeInt;
begin
//  Lock;
//  try
    result := Finfo.Iterations;
//  finally
//    UnLock;
//  end;

end;
function TManagedThread.GEtAverageExecutionTime: nativefloat;
var
  t: integer;
begin
  result := 0;
  for t:= low(FTimes) to high(FTimes) do begin
    result := result + FTimes[t];
  end;
  result := result / high(FTimes);

end;

function TManagedThread.GetBetterPriority: TBetterPriority;
begin
  result := PlatformToBetterPriority(platformpriority);
end;

function TManagedThread.GEtError: string;
begin
  Lock;
  try
    result := Finfo.Error;
  finally
    Unlock;
  end;
end;


//------------------------------------------------------------------------------
function TManagedThread.GetStepCount: NativeInt;
begin
  Lock;
  try
    result := Finfo.StepCount;
  finally
    UnLock;
  end;

end;

function TManagedThread.GetMenu(idx: NativeInt): string;
begin
  Lock;
  try
    result := FMenu[idx];
  finally
    Unlock;
  end;
end;

function TManagedThread.GetName: string;
begin
  Lock;
  try
    result := Finfo.Name;
    UniqueString(result);
  finally
    UnLock;
  end;
end;



function TManagedThread.GetNameEx: string;
begin
  result := name+'#'+threadid.tostring
end;

function TManagedThread.GetOwner: TObject;
begin
  Lock;
  try
    result := FOwner;
  finally
    Unlock;
  end;

end;

function TManagedThread.GetPool: TThreadPoolBase;
begin
  Lock;
  try
    result := FPool;
  finally
    Unlock;
  end;

end;

function TManagedThread.GetPooled: boolean;
begin
  Lock;
  try
    result := FPool <> nil;
  finally
    Unlock;
  end;
end;


function TManagedThread.GetPlatformPriority: integer;
begin
  result := ord(realthread.priority);
end;

function TManagedThread.GEtReady: boolean;
begin
  Lock;
  try
    result := not IsSignaled(evStarted);
  finally
    unlock;
  end;
end;

function TManagedThread.GetRunHot: boolean;
begin
  result := IsSignaled(evHot);
end;

function TManagedThread.GetSignalDebug: string;
begin
  //evHot, evHasWork, evStart, evStarted, evStop, evFinished: TBetterSignal;
  result := '[hwsrxfploqzci]';
  if IsSignaled(evHot) then       result[1+STRZ] := 'H';
  if IsSignaled(evHasWork) then   result[2+STRZ] := 'W';
  if IsSignaled(evStart) then     result[3+STRZ] := 'S';
  if IsSignaled(evStarted) then   result[4+STRZ] := 'R';
  if IsSignaled(evStop) then      result[5+STRZ] := 'X';
  if IsSignaled(evFinished) then  result[6+STRZ] := 'F';
  if PoolLoop then                result[7+STRZ] := 'P';
  if Loop then                    result[8+STRZ] := 'L';
  if IsSignaled(evOutOfPool) then                result[9+STRZ] := 'O';
  if IsSignaled(evPoolReady) then                result[10+STRZ] := 'Q';
  if IsSignaled(evSleeping) then                result[11+STRZ] := 'Z';
  if IsSignaled(evConcluded) then                result[12+STRZ] := 'C';
  if IsSignaled(evIdle) then                result[13+STRZ] := 'I';


end;

function TManagedThread.GEtSpin: boolean;
begin
  Lock;
  try
    result := Finfo.Spin;
  finally
    Unlock;
  end;
end;


function TManagedThread.GetStartCalled: boolean;
begin
  result := IsSignaled(evStarted);
end;

function TManagedThread.GetStatus: string;
begin
  Lock;
  try
    result := Finfo.Status;
    UniqueString(result);
    StatusUpdated := false;
  finally
    UnLock;
  end;
end;

function TManagedThread.GetThreadID: cardinal;
begin
  result := realthread.ThreadId;
end;

function TManagedThread.GetThreadManager: TThreadManager;
begin
  Lock;
  try
    result := FThreadManager;
  finally
    UnLock;
  end;

end;


function TManagedThread.GetUnknown: IUnknown;
begin
  result := FUnknown;
end;


function TManagedThread.GetStep: NativeInt;
begin
  Lock;
  try
    result := Finfo.Step;
  finally
    UnLock;
  end;

end;

function TManagedThread.IdleBreak: boolean;
begin
  exit(false);
end;

procedure TManagedThread.IdleCycle;
var
  cx: ni;
begin
  //
  if assigned(FOnIdle) then begin
    cx := IdleIterations;
    while cx > 0 do begin
      dec(cx);
      if IdleBreak then exit;
      if Not TryLock then exit;
      try
        FOnIdle(self);
      finally
        Unlock;
      end;
    end;

  end;
end;

procedure TManagedThread.IdleStateTasks;
begin

  //raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

class procedure TManagedThread.IncInstanceCount;
begin
{$IFDEF WINDOWS}
  interlockedincrement(Self.instancecount);
//  Debug.Log(nil,'++There are now '+inttostr(instancecount)+' threads.');
{$ENDIF}
end;

procedure TManagedThread.Init;
begin
  //
  NoWorkRunInterval := -1;
  AutoCountCycles := true;
end;

procedure TManagedThread.InitFromPool;
begin
  WaitForSignal(evSleeping);
  InitSignalState;
  Pool := nil;
  Status := 'Ready';
  HasWork := true;
  ColdRunInterval := 1000;
  NoWorkRunInterval := -1;
  PoolLoop := true;
  Signal(evStart,false);
  Signal(evSleeping, false);



end;

procedure TManagedThread.InitSignalState;
begin
  {$IFDEF MORE_THREAD_STATUSES}
  Status := 'FromPool';
  {$ENDIF}
//  Signal(evFinished, false);
  Signal(evStop, false);
  Signal(evStart, false);
  Signal(evStarted, false);
  Signal(evHasWork, false);
  Signal(evHot, false);

  Signal(evHasWork);
  Signal(evHot);



end;

function TManagedThread.IsCurrentThread: boolean;
begin
  result := GEtCurrentThreadID = self.ThreadID;
end;


function TManagedThread.IsIdle: boolean;
begin
  result := evIdle.IsSignaled;
end;

procedure TManagedThread.IterationComplete;
begin
  inc(Finfo.Iterations);
end;

procedure TManagedThread.MenuAction(idx: NativeInt);
begin

//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TManagedThread.MenuBuilt: boolean;
begin
  result := FMenu <> nil;
end;

function TManagedThread.MenuCount: NativeInt;
begin
  Lock;
  try
    if FMenu = nil then
      BuildMenu;

    result := FMenu.count;
  finally
    Unlock;
  end;
end;

class function TManagedThread.Need(owner: TObject; pool: TMAsterThreadPool): TManagedThread;
begin
  if pool = nil then
    pool := TPM;
  result := TPM.NeedThread(self, owner);
end;


procedure TManagedThread.OnFinish;
begin
  //
end;

procedure TManagedThread.PrepareForPool;
begin

//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;


procedure TManagedThread.Queue(m: TThreadMethod);
begin
  realthread.Queue(m);
end;

procedure TManagedThread.RemoveFromPool;
begin
  Pool.ForceRemove(self);
end;

//------------------------------------------------------------------------------
function TManagedThread.GetAGe: cardinal;
begin
  if IsSignaled(evStarted) then
    result := 0
  else
    result := GEtTimeSince(LastUsed);

end;

function TManagedThread.GetAutoSpin: boolean;
begin
  Lock;
  try
    result := Finfo.AutoSpin;
  finally
    Unlock;
  end;
end;

procedure TManagedThread.SafeREsume;
begin
  if realthread.suspended then realthread.resume;
  Start;

end;

function TManagedThread.SafeTerminated: boolean;
begin
  result := IsSignaled(evStop) or IsSignaled(evFinished);// or terminated;
end;



procedure TManagedThread.SafeWaitFor;
begin
  if IsCurrentThread then begin
    Debug.Log('YOU CAN''T WAIT ON THE CURRENT THREAD!');
    exit;
  end;

  if PoolLoop then begin
    if IsSignaled(evStarted) then begin
      Signal(evHasWork);
      while not WaitForSignal(evFinished, 1000) do begin
        Debug.Log(self, 'Safe Wait For '+self.classname+' #'+threadid.tostring+' '+getsignaldebug+' '+inttostr(getticker));
        Signal(evHasWork);
        if (WAitBEforeAbortTime > 0 ) and  (GetTimeSince(beginstoptime) > WAitBeforeAbortTime) then begin
          Debug.Log(self, 'Terminating Hung Thread'+self.classname+' '+getsignaldebug+' '+inttostr(getticker));
          Signal(evStop);
//          TerminateThread(ThreadID,0);
//          PoolLoop := false;
//          Loop := false;
//          break;
        end;
      end;
    end;
  end else begin
      Signal(evHasWork);
      while not WaitForSignal(evFinished, 1000) do begin
        Debug.Log(self, 'Safe Wait For '+self.classname+' '+getsignaldebug+' '+inttostr(getticker));
        Signal(evHasWork);
        if (WAitBEforeAbortTime > 0 ) and  (GetTimeSince(beginstoptime) > WAitBeforeAbortTime) then begin
          Debug.Log(self, 'Terminating Hung Thread'+self.classname+' '+getsignaldebug+' '+inttostr(getticker));
          Signal(evStop);
//          TerminateThread(ThreadID,0);
//          PoolLoop := false;
//          Loop := false;
//          break;

        end;
      end;
  end;

end;

procedure TManagedThread.SetAutoSpin(const Value: boolean);
begin
  Lock;
  try
    Finfo.AutoSpin := value;
  finally
    Unlock;
  end;
end;

procedure TManagedThread.SetBetterPriority(const Value: TBetterPriority);
begin
  platformpriority := betterprioritytoPlatform(value);
end;

procedure TManagedThread.SetIterations(const Value: NativeInt);
begin
//  Lock;
//  try
    Finfo.Iterations := Value;
//  finally
//    UnLock;
//  end;

end;
procedure TManagedThread.SetError(const Value: string);
begin
  Lock;
  try
    Finfo.Error := value;
  finally
    Unlock;
  end;
end;

procedure TManagedThread.SetHasWork(const Value: boolean);
begin
  Signal(evHasWork, value);
end;

//------------------------------------------------------------------------------
procedure TManagedThread.SetStepCount(const Value: NativeInt);
begin
//  Lock;
//  try
    Finfo.StepCount := Value;
//  finally
//    UnLock;
//  end;

end;

procedure TManagedThread.SetStop(const Value: boolean);
begin
  FStop := Value;
end;

procedure TManagedThread.SetName(const Value: string);
begin

  Lock;
  try
    if value = '' then
      Debug.Log(self, 'thread name is blank!');
    if Finfo.Name = value then exit;
    Finfo.Name := value;
    //Debug.Log(inttostr(Threadid)+' Thread name changed: '+value);
  finally
    UnLock;
  end;
end;


procedure TManagedThread.SetOwner(const Value: TObject);
begin
  Lock;
  try
    FOwner := value;
  finally
    Unlock;
  end;
end;

procedure TManagedThread.SetPool(const Value: TThreadPoolBase);
begin
  Lock;
  try
    FPool := value;
  finally
    Unlock;
  end;

end;



procedure TManagedThread.SetPlatformPriority(const Value: integer);
begin
//todo 2: setting of thread priorities doesn't do anything on alt platforms because we need to translate "platform priority" to platform specific numbers... else we get runtime errors
{$IFDEF MSWINDOWS}
  realthread.Priority := TThreadPriority(value);
{$ENDIF}
end;

procedure TManagedThread.SEtReady(const Value: boolean);
begin
//  Signal(evStarted, false);
end;

procedure TManagedThread.SetRunHot(const Value: boolean);
begin
  if IsSignaled(evStop) then
    Signal(evhot, true)
  else
    Signal(evhot, value);
end;

procedure TManagedThread.SEtSpin(const Value: boolean);
begin
  Lock;
  try
    Finfo.Spin := value;
  finally
    Unlock;
  end;
end;

procedure TManagedThread.SetStartingState;
begin
  evStop.ignoreChecks := true;
  evStart.ignoreChecks := true;
  evFinished.ignoreChecks := true;
  evOutOfPool.ignoreChecks := true;
  evStarted.ignoreChecks := true;
  evHasWork.ignoreChecks := true;
  evHot.ignoreChecks := true;
  Signal(evSleeping, false);
  InitSignalState;
  evStop.ignoreChecks := false;
  evStart.ignoreChecks := false;
  evFinished.ignoreChecks := false;
  evOutOfPool.ignoreChecks := false;
  evStarted.ignoreChecks := false;
  evHasWork.ignoreChecks := false;
  evHot.ignoreChecks := false;




end;

procedure TManagedThread.SetStatus(const Value: string);
begin
  Lock;
  try
    Finfo.Status := Value;
//    UniqueString(FSTatus);
    tmLastStatusUpdate := GetTicker;
    StatusUpdated := true;
//    Debug.Log(ClassName+' status = '+FSTatus,'managedthread');
  finally
    UnLock;
  end;

end;

procedure TManagedThread.SetThreadManager(const Value: TThreadManager);
begin
  if assigned(FThreadManager) then
    FthreadManager.DeRegisterThread(self);

  Lock;
  try
    FThreadManager := value;
  finally
    Unlock;
  end;

  if assigned(FThreadManager) then
    FthreadManager.RegisterThread(self);

end;


procedure TManagedThread.SignalAllForTerminate;
begin
  evStop.ignorechecks := true;
  evStart.ignorechecks := true;
  evOutOfPool.ignorechecks := true;
  evHasWork.ignorechecks := true;
  Signal(evHasWork, true);
  Signal(evStop);
  Signal(evStart);//<--- must signal start to wake up for terminate
  Signal(evOutOfPool, true);//<---special spot for termination
  //Signal(evIdle,true);


end;

procedure TManagedThread.Sleep(ms: ni);
begin
  systemx.sleep(ms);
end;

procedure TManagedThread.SetStep(const Value: NativeInt);
begin
//  Lock;
//  try
    Finfo.Step := Value;
//  finally
//    UnLock;
//  end;

end;



procedure TManagedThread.BeginSTart;
begin
{$IFDEF QUICK_THREAD_DEBUG}  Debug.Log('At beginning of start signal '+self.GetSignalDebug);{$ENDIF}

    NEverStarted := false;
    if orderlyinit.init.initialized then
      WaitForSignal(evIdle);
  //  Signal(evStarted,false);
    Signal(evStop, false);
    if realthread.suspended then realthread.resume;
  //  Signal(evPoolPause, false);
    Signal(evStarted,false);
    Signal(evFinished,false);
    Signal(evStart);



end;

procedure TManagedThread.BeginStop(bySelf: boolean=false);
var
  bWasSTarted: boolean;
begin
{$IFDEF QUICK_THREAD_DEBUG}  Debug.Log('At BEGIN of STOP signal '+self.GetSignalDebug);{$ENDIF}
//  if WasNeverStarted then
//    Signal(evStart, true);
  evSTop.IgnoreChecks := bySelf;
  evStart.IgnoreChecks := bySelf;
  evOutOfPool.IgnoreChecks := bySelf;


  bWasStarted := IsSignaled(evSTarted);
//  Debug.Log('Signal evStop '+GetSignalDebug);
  Signal(evStop);

  if not bWasStarted then begin
    while not evStarted.IsSignaled do begin
      Debug.log(NameEx+' Was not in a started state '+GetSignalDebug);
      sleep(1000);
      Signal(evStart);//you have to start it before it can reach the stop state
    end;
  end;
  Signal(evHasWork, true);
  Signal(evHot,true);

{  //signal stop
  Debug.Log('Signal evStop '+GetSignalDebug);
  Signal(evStop);
  Debug.Log('Signaled evStop '+GetSignalDebug);
  if not bWasStarted then begin
    //make sure we start without evStartSignaled
    Debug.Log('Was not started! UnSignal evStart '+GetSignalDebug);
    Signal(evStart, false);
    Debug.Log('Signal evStart '+GetSignalDebug);
    Signal(evStart, true);
    Debug.Log('Signaled evStart '+GetSignalDebug);
  end;


  Debug.Log('Signal evStart '+GetSignalDebug+' to wake up from pause.');
  //signal start to wake up from pause
  Signal(evStart);
  if bWasSTarted then begin
    //revert start signal back for good measures
    Debug.Log('Was not started '+GetSignalDebug+' waiting for started signal.');
    WaitForSignal(evStarted);
  end;
  Debug.Log('UnSignal evStart '+GetSignalDebug+'');
  Signal(evStart, false);
  //Debug.Log('Signal evStop '+GetSignalDebug+'');
  //Signal(evStop);
  Debug.Log('UnSignal evHaswork '+GetSignalDebug+'');
  Signal(evHasWork, true);
  Debug.Log('UnSignal evHot '+GetSignalDebug+'');
  Signal(evHot, true);}
  beginstoptime := getticker;
  evSTop.IgnoreChecks := false;
  evStart.IgnoreChecks := false;
end;

procedure TManagedThread.EndStart;
begin
//  Debug.Log('waiting for started');
  WaitForSignal(evStarted);
{$IFDEF QUICK_THREAD_DEBUG}  Debug.Log('At END of start signal '+self.GetSignalDebug);{$ENDIF}

end;

procedure TManagedThread.EndStop(bySelf: boolean=false);
begin
  if IsCurrentThread then begin
    exit;
  end else begin
    SafeWaitFor;
  end;

{$IFDEF QUICK_THREAD_DEBUG}  Debug.Log('At END of STOP signal '+self.GetSignalDebug);{$ENDIF}
end;

procedure TManagedThread.Stop(bySelf: boolean=false);
begin
//  if IsSignaled(self.ev) then begin
//    Debug.Log('WARNING: Why are you calling "Stop" on a thread that is already pooled?');
//    exit;
//  end;
//  Debug.Log('stopping thread with state '+getsignaldebug);
//  if IsSignaled(evFinished) then
//    exit;
//  if IsSignaled(evStop) then
//    exit;
//  if NeverStarted then begin
//    Signal(evFinished,true);
//  end else begin
  if bySelf then
    Loop := false
  else begin
    BeginSTop(byself);
    EndStop(bySelf);
  end;
//  end;
end;

function TManagedThread.StopREquested: boolean;
begin
  result := IsSignaled(evSTop);
end;

procedure TManagedThread.Synchronize(AMethod: TThreadMethod);
begin
  realthread.Synchronize(amethod);
end;

procedure TManagedThread.Synchronize(AThreadProc: TThreadProcedure);
begin
  realthread.Synchronize(athreadproc);
end;

procedure TManagedThread.ClearMM;
begin
  if FMMTaskHandle <> INVALID_HANDLE_VALUE then
    AvRevertMmThreadCharacteristics(FMMTaskHandle);
end;





function TManagedThread.WaitForSafeX(iTimeout: nativeint): boolean;
var
  tmStart: ticker;
begin
  tmStart := tickcount.GetTicker;
  result := WaitForSignal(evFinished, iTimeOut);
//  raise exception.Create('implement');
end;

procedure TManagedThread.WaituntilReady;
begin
  while not self.Ready do
    sleepex(random(10),true);
end;


procedure TManagedThread.Start;
begin
{$IFNDEF NO_INIT_THREAD_CHECKS}
  if not orderlyinit.init.Initialized then
    raise ECritical.create('don''t call TManagedThread.Start during initialization (it won''t work for DLLs). Call beginStart instead and check EndStart later.');
{$ENDIF}

  BeginStart;
  EndStart;
end;

function TManagedThread.Started: boolean;
begin
  result := IsSignaled(evStarted);
end;

class procedure TManagedThread.StaticSynchronize(const AThread: TThread;
  AMethod: TThreadMethod);
begin
  TThread.Synchronize(athread, amethod);
end;

procedure TManagedThread.StepComplete(iStepCount: NativeInt);
begin
  Lock;
  try
    inc(Finfo.Step, iStepCount);
  finally
    UnLock;
  end;

end;

{ TProcessorThread }
function TProcessorThread.GetComplete: boolean;
begin
  result := not IsSignaled(evStarted);
end;

function TProcessorThread.IsStartable: boolean;
begin
  result := not IsSignaled(evStarted);
end;

procedure TProcessorThread.SetComplete(b: boolean);
begin
  if b then
    Signal(evStarted, false);

end;

procedure TManagedThread.WaitFor;
begin
  SafeWaitFor;

end;

procedure TManagedThread.WaitforConclusion;
begin
  evConcluded.WaitFor;
end;

procedure TManagedThread.WaitForFinish;
begin
  SafeWaitFor;
end;

procedure TManagedThread.WaitForIdle;
begin
  if self.IsCurrentThread then
    exit;
  //debug.Log(self, 'wait for idle: '+GetSignalDebug);
  while not WaitForSignal(evIdle, 4000) do
    Debug.Log(self, 'not idle '+GetsignalDebug);
  //evIdle.waitfor;
end;

procedure TManagedThread.WaitForLowLevel;
begin
  realthread.waitFor;
end;

procedure TProcessorThread.Start;
begin
{$IFDEF MORE_THREAD_STATUSES}
  Status := 'Starting...';
{$ENDIF}
  AssertThatNotThisThread;

  Signal(evStart);

{$IFDEF MORE_THREAD_STATUSES}
  Status := 'Start Signal Sent';
{$ENDIF}

end;

{ TStringProcessorThread }

function TStringProcessorThread.GetResult: string;
begin
  Lock;
  try
    result := FResult;
    uniquestring(result);
  finally
    Unlock;
  end;
end;

procedure TStringProcessorThread.SetResult(const Value: string);
begin
  Lock;
  try
    FResult := value;
    uniquestring(FResult);
  finally
    Unlock;
  end;
end;

{ TMasterThread }

constructor TMasterThread.Create(Owner: TObject; Manager: TThreadManager; pool: TThreadpoolBase);
begin
  inherited;
  Fthreads := nil;
end;

destructor TMasterThread.Destroy;
begin
  FThreads.free;
  inherited;
end;

function TMasterThread.GetThreads: TThreadManager;
begin
  if FThreads = nil then
    FThreads := TThreadManager.create;

  result := FThreads;

end;



{ TExternalEventThread }

constructor TExternalEventThread.Create(Owner: TObject; Manager: TThreadManager; pool: TThreadpoolBase; Loop: boolean; ExecuteEvent: TExternalThreadExecutionEvent);
begin
  CReate(owner, manager, pool);

  self.FOnExecute := ExecuteEvent;
{$IFDEF NEW_LOOP}
  Floop := Loop;
{$ENDIF}



end;

constructor TExternalEventThread.Create(Owner: TObject; Manager: TThreadManager; pool: TThreadpoolBase);
begin

  inherited CReate(owner, manager, pool);

end;

destructor TExternalEventThread.Destroy;
begin

  inherited;
end;

procedure TExternalEventThread.DoExecute;
begin
  inherited;
  if IsSignaled(evStop) then exit;
  InEvent := true;
  try
    if assigned(OnExecute) then begin
      try
        OnExecute(self);
        IdleStateTasks;
      except
        on E: Exception do begin
          Debug.Log('Unhandled Exception in '+nameex+' '+e.Message);
          if BreakLoopOnException then begin
            Debug.Log('Thread has crashed! '+nameex+' '+e.message);
            raise;
          end;
        end;
      end;
    end else
      sleepex(1,true);
  finally
    InEvent := false;
  end;

end;

function TExternalEventThread.GetInEvent: boolean;
begin
  Lock;
  try
    result := FInEvent;
  finally
    Unlock;
  end;
end;

function TExternalEventThread.GetTerminateRequested: boolean;
begin
  Lock;
  try
    result := realthread.Terminated;
  finally
    unlock;
  end;
end;

procedure TExternalEventThread.InitFromPool;
begin
  InEvent := false;
  OnExecute := nil;
  inherited;
end;

procedure TExternalEventThread.SetInEVent(const Value: boolean);
begin
  Lock;
  try
    FInEvent := Value;
  finally
    Unlock;
  end;
end;

procedure TExternalEventThread.SetTerminateREquested(const Value: boolean);
begin
  Lock;
  try
    realthread.Terminate;
  finally
    unlock;
  end;
end;

{ TMasterThreadPool }

var
  ix: ni;
function TMasterThreadPool.CheckTermList: boolean;
var
  t: ni;
  thr: TMAnagedThread;
  sLastTerm: string;

begin
//  Lock;
  try
  sLastTerm := '';
  result := false;
  while termlist.count > 0 do begin
    result := true;
    termlist.Lock;
    try
    t := termlist.count-1;
    if t < 0 then
      break;
    thr := termlist[t];
    if assigned(thr) then begin
      if thr.IsFinished then begin
        if thr.classname = 'TSoundDevice_PortAudio' then begin
          inc(ix);
          if ix = 2 then
            Debug.Log('here');
        end;

        Debug.Log('Final Termination '+thr.nameex);
        if sLastTerm = thr.NameEx then
          Debug.Log('SAME!?!?!?!?');
        sLastTerm := thr.NameEx;
        thr.WaitFor;
        thr.WaitForConclusion;
        thr.WaitForLowLevel;
        thr.free;

        if t >= termlist.count then
          Debug.Log('termlist decremented unexpectedly!')
        else
          termlist.delete(t);



        Debug.Log('There are now '+termlist.count.tostring+' termlist items');
      end;

    end;
    finally
      termlist.unlock;
    end;
  end;
  finally
//    Unlock;
  end;
end;

procedure TMasterThreadPool.Clean;
var
  t: NativeInt;
  thr: TManagedThread;
  list: TBetterList<TManagedThread>;
begin
//  Debug.Log('Thread pool cleaner is executing.');
//  Debug.ConsoleLog('Cleaning. '+inttostr(getcurrentthreadid));
  list := TBetterList<TManagedThread>.create;
  try
    if TryLock then
    try
      for t:= FPools.count-1 downto 0 do begin

        thr := FPools[t].Clean;
        if assigned(thr) then begin
          list.add(thr);
        end;

        if FreeOnEmpty then begin
          if FPools[t].IsEmpty then begin
            FPools[t].free;
            FPools.delete(t);
          end;
        end;
      end;
    finally
      Unlock;
    end;

    for t:= 0 to list.count-1 do begin
      thr := list[t];
      if assigned(thr) then begin
        thr.poolloop := false;
        {$IFDEF NEW_LOOP}
        thr.loop := false;//<--this shouldn't happen but whatever
        {$ENDIF}
        thr.realthread.FreeOnTerminate := false;
        thr.terminate;
        //thr.Start;
        thr.SignalAllForTerminate;
        while thr.realthread.suspended do begin
          thr.terminate;
          thr.realthread.resume;
        end;
      end;
    end;

    for t:= 0 to list.count-1 do begin
      Debug.Log('Adding '+list[t].nameex+' to termlist');
      termlist.add(list[t]);
    end;
    while CheckTermList and (HighPerformancemode) do begin
      sleep(100);
    end;
  finally
    list.free;
  end;

end;


procedure TMasterThreadPool.Clear;
var
  t: NativeInt;
begin
  Lock;
  try
    for t:= 0 to FPools.count-1 do begin
      FPools.clear;
    end;
  finally
    Unlock;
  end;
end;

constructor TMasterThreadPool.Create;
begin
  inherited;
  FPools := TBetterList<TThreadPoolBase>.create;
  termlist:= TSharedList<TMAnagedThread>.create;
  Debug.Log(self, 'Master Thread Pool Created');

end;

destructor TMasterThreadPool.Destroy;
var
  thr: TManagedThread;
begin

  HighPerformancemode := false;
  STopCleaner;
  Fpools.free;


  while termlist.count> 0 do begin
    thr := termlist[0];
    termlist.delete(0);
    thr.waitfor;
    thr.waitForConclusion;
    thr.WaitForLowLevel;
    thr.free;
    thr := nil;

  end;
  inherited;

  termlist.free;
end;



procedure TMasterThreadPool.ForceRemove(thr: TManagedThread);
var
  tp: TThreadPoolBase;
begin
  Lock;
  try
    tp := NeedThreadPool(TManagedThreadClass(thr.classtype));
    tp.ForceRemove(thr);


  finally
    Unlock;
  end;

end;

procedure TMasterThreadPool.FreeWithThreads;
var
  t: NativeInt;
begin
  for t:= 0 to Fpools.count-1 do begin
    Fpools[t].FreewithThreads;
  end;
  FFreeOnEmpty := true;
end;

procedure TMasterThreadPool.WakeAndFreeAllThreads;
var
  t: NativeInt;
begin
  for t:= 0 to Fpools.count-1 do begin
    Fpools[t].WakeAndFreeAllThreads(false);
  end;
  for t:= 0 to Fpools.count-1 do begin
    Fpools[t].WakeAndFreeAllThreads(true);
  end;

end;


function TMasterThreadPool.Needthread(c: TManagedThreadClass; owner: TObject): TManagedThread;
var
  tp: TThreadPoolBase;
begin
  Lock;
  try
    tp := NeedThreadPool(c);
    result := tp.BaseNeedthread(owner);
    result.evPoolReady.Signal(false);
    result.evOutOfPool.Signal(true);//<---ONE SPOT
    result.evSleeping.Signal(false);//<---ONE SPOT

  finally
    Unlock;
  end;
end;

function TMasterThreadPool.NeedThread<T>(owner: TObject): T;
begin
//  raise exception.Create('unimplemented');
  result := T(NeedThread(T, owner));
  result.SetStartingState;
  //result.Loop := true;//<--- all returned threads are LOOP because it is needed for thread pooling
end;

function TMasterThreadPool.NeedthreadPool(
  c: TManagedthreadClass): TThreadPoolBase;
var
  t: NativeInt;
begin
  l;
  try
    result := nil;
    for t:= 0 to FPools.count-1 do begin
      if FPools[t].C = c then begin
        result := Fpools[t];
        exit;
      end;
    end;

    if result = nil then begin
      result := TThreadpoolbase.create;
      result.C := c;
      FPools.add(result);
    end;
  finally
    ul;
  end;


end;



procedure TMasterThreadPool.NoNeedthread(thr: TManagedthread);
var
  tp: TThreadPoolBase;
begin
  if thr = nil then
    exit;

  signal(thr.evOutOfPool, false);//<---ONE SPOT!
  //signal(thr.evSleeping, true);//<---ONE SPOT!  MOVED TO AFTER TAKEN FROM INCOMING LIST



  thr.PrePareForpool;


  //if the shutdown flag is set then no more threads will be
  //added to the pool.
  //also the pool does not accept pre-terminated threads.
  if shutdown {or thr.Terminated} then begin
    if thr.IsCurrentThread then begin
      //thr.SignalAllForTerminate;
      thr.realthread.FreeOnTerminate := true;
      thr.realthread.Terminate;

      Lock;
      try
        thr.FromPool := nil;
        ForceRemove(thr);
      finally
        Unlock;
      end;
      exit;
    end else begin
      thr.realthread.Terminate;
      thr.WaitFor;
      WaitForSignal(thr.evConcluded);
      thr.Detach;
      thr.Free;
      exit;
    end;
  end else begin
    Lock;
    try
      StartCleaner;
    finally
      Unlock;
    end;

    //if not the current thread, then we need to wait for the thread to respond to our signals
    //otherwise if this is the current thread, then we must assume that some kind of care has been
    //taken to assure that after exiting this function the thread will find itself in an appropriate state for suspension eventually
    //alternatively I could code something in here that calls idlestate tasks as this point (effectively locking in the Jam flag handling,
    //but I think that some threads might want to do some shit and then bubble up to an idle state.
    if not thr.IsCurrentThread then begin
      thr.Stop;
      while not WAitForSignal(thr.evFinished, 100) do begin
        Debug.Log(self, thr.ClassName+'#'+inttostr(thr.threadid)+' is pending addition to pool, waiting for signal evFinished. '+thr.GetSignalDebug);
        if not thr.StopRequested then
          thr.Stop;
      end;
    end;

    Lock;
    try
      tp := NeedThreadPool(TManagedThreadClass(thr.classtype));
      tp.BaseNoNeedthread(thr);

//      thr.status := thr.status+'->pooled';
//      if thr.IsCurrentThread then
//        thr.Status := thr.Status+'->PASSIVE. Must hit idlestate tasks FIRST.';
      StartCleaner;
    finally
      Unlock;
    end;

  end;

end;


procedure TMasterThreadPool.OnThreadExecute(sender: TExternalEventThread);
var
  t: NativeInt;
  tm: ticker;
begin


  sender.Name := 'Thread Pool Cleaner';


  tm := GetTicker;
  Clean;
  sleepex(10, true);
  //sleepex(greaterof(10, gettimesince(tm)),true);

end;

procedure TMasterThreadPool.StartCleaner;
begin
  if FCleaner = nil then  begin
    Lock;
    try
      if FCleaner = nil then begin
        FCleaner := self.NeedThread<TExternalEventThread>(self);
        FCleaner.BreakLoopOnException := true;

        FCleaner.OnExecute := self.OnThreadExecute;
        FCleaner.Name := 'Master Thread Pool Cleaner';
        FCleaner.IsCriticalSystemThread := true;
        FCleaner.Loop := true;
        FCleaner.Start;
      end;
    finally
      Unlock;
    end;
  end;
end;

procedure TMasterThreadPool.StopCleaner;
var
  thr: TManagedThread;
begin
  if assigned(FCleaner) then begin
    l;
    try
      thr := FCleaner;
      FCleaner := nil;
    finally
      ul;
    end;
    if assigned(thr) then begin
      thr.Terminate;
      thr.stop;

      thr.waitfor;
      thr.WaitforConclusion;
      thr.WaitForLowLevel;
      thr.free;
    end;

  end;

end;

{ TThreadPoolBase }

function TThreadPoolBase.Clean: TMAnagedThread;
//IF CLEAN returns a thread... it should be destroyed outside of clean apparently.
var
  t: NativeInt;
  thr,thr2: TManagedthread;
begin
  result := nil;
  thr2 := nil;
  if TryLock then
  try
    for t:= FIncomingThreads.count-1 downto 0 do begin
      thr := FIncomingThreads[t];
      if IsSignaled(thr.evFinished) then begin
        if not FThreads.Has(thr) then
          FThreads.add(thr);
        FIncomingThreads.remove(thr);
        thr.PendingPoolAddition := false;
      end else begin
      //WaitForSignal(thr.evFinished);//<--- DONT DO THIS!  Unfinished threads should STAY in the incoming list until they are finished,
      //waiting on the thread while the threadpool is locked will cause dead lock!

//        Debug(thr.classname+' '+inttostr(thr.threadid)+' is Not Suspended yet');
      end;
    end;
    //if HighperformanceMode is defined, we will NEVER destroy any threads (until shutdown)
    if not TPM.HighPerformanceMode then begin
      for t:= 0 to Fthreads.count-1 do begin
        thr := Fthreads[t];
        if (GetTimeSince(thr.LastUsed) > ThreadPoolTimeout) then begin
          FIncomingThreads.remove(thr);
          Fthreads.remove(thr);
          thr2 := thr;
          result := thr;
          break;
        end;
      end;
    end;
  finally
    Unlock;
  end;




end;

procedure TThreadPoolBase.Clear;
begin
  ThreadPoolTimeOut := 1000;

  //
end;

constructor TThreadPoolBase.Create;
begin
  inherited;
  FThreadPoolTimeout := DEFAULT_THREAD_POOL_TIME;
  FThreads := TBetterList<TManagedThread>.create;
  FIncomingThreads:= TBetterList<TManagedThread>.create;
  FACtiveThreads := TBetterList<TManagedThread>.create;

end;

destructor TThreadPoolBase.Destroy;
begin
  Clear;
  FThreads.free;
  FIncomingThreads.free;
  FACtiveThreads.free;
  inherited;
end;

procedure TThreadPoolBase.ForceRemove(thr: TManagedThread);
begin
  exit;
  Lock;
  try
    if FIncomingThreads.indexof(thr) >= 0 then begin
      FIncomingThreads.remove(thr);
      Debug.log(self,'*Thread was forcibly removed from incoming pool');
    end;

    if FThreads.indexof(thr) >= 0 then begin
      FThreads.remove(thr);
      Debug.Log(self,'*Thread was forcibly removed from pool');
    end;

    if FActiveThreads.indexof(thr) >= 0 then begin
      FActiveThreads.remove(thr);
      Debug.Log(self,'*Thread was forcibly removed from pool');
    end;

  finally
    Unlock;
  end;
end;

procedure TThreadPoolBase.FreeWithThreads;
begin
  FFreeOnEmpty := true;

end;

function TThreadPoolBase.GEtIsEmpty: boolean;
begin
  result := (FThreads.count+FACtiveThreads.count+FIncomingThreads.count)=0;
end;

function TThreadPoolBase.GetStatMessage: string;
var
  i, i1, i2 ,i3: nativeint;
begin
  i := GetStats(i1, i2, i3);

  result := fc.ClassNAme+' Pool Stats -- Incoming: '+inttostr(i1)+' Inducted: '+inttostr(i3)+' Active:'+inttostr(i2);

end;

function TThreadPoolBase.GetStats: TThreadPoolStats;
begin
  GetStats(result.Incoming, result.Active, result.pooled);
end;

procedure TThreadPoolBase.WakeAndFreeAllThreads(bWait: boolean);
begin
  TPM.HighPerformanceMode := false;
  ThreadPoolTimeout := 1;
  if bWait then
  while (FIncomingThreads.count > 0) or (Fthreads.Count > 0) do begin
    sleep(1000);
    Debug.ConsoleLog('Thread pool for '+self.Fc.ClassName+' still has '+FThreads.count.tostring+' threads + '+FIncomingThreads.count.tostring+' incoming.');
  end;

end;

function TThreadPoolBase.GetStats(out iIncoming,iActive,iPooled: nativeint): nativeint;
begin
  Lock;
  try
    iIncoming := FincomingThreads.Count;
    iActive := FActiveThreads.Count;
    iPooled := Fthreads.Count;
    result := iIncoming + iPooled;
  finally
    Unlock;
  end;

end;

{ TThreadPool<T> }



function TThreadPoolBase.BaseNeedThread(owner: TObject): TManagedThread;
var
  idx: ni;
  tm: ticker;
begin
  Lock;
  try
{$IFDEF SLEEP_IF_INCOMING}
    while (FIncomingThreads.count > 2) and (Fthreads.count=0) do begin
      Unlock;
      sleep(10);
      try
        Clean;
      finally
        Lock;
      end;
    end;
{$ENDIF}

    //Clean;
    //if we have threads in the pool
    result := nil;
    idx := FThreads.count;
    while result = nil do
    while true do begin
      dec(idx);
      if {$IFDEF DISABLE_THREAD_POOL}false and {$ENDIF}(idx >= 0) then begin
        //return the last thread in the pool
        result := Fthreads[idx];
        if not WAitForSignal(result.evPoolReady,5) then begin
          result := nil;
          continue;
        end;
        //set a new parent
        //delete it from the thread list
        Fthreads.delete(idx);
        Unlock;
        try
          //Debug.Log('Pulling '+result.ClassName+' from pull with status '+result.GetSignalDebug);
          result.Pool := nil;
          result.InitFromPool;
          result.HasWork := true;
          Signal(result.evFinished, false);
        finally
          Lock;
        end;
        FACtiveThreads.add(result);

        result.owner := owner;
        break;
      end else begin
        //create a new thread
        Unlock;
        try
          tm := GetTicker;
          result := c.create(owner, BackGroundThreadMan, self);
          result.InitFromPool;
          Debug.Log(self, 'Thread Creation time: '+gettimesince(tm).tostring);
          Signal(result.evFinished, false);
          Signal(result.evSleeping, false);
        finally
          Lock;
        end;

        FACtiveThreads.add(result);
        break;
        //setup
        //result.Parent := parent;
        //result.Resume;
      end;
    end;
  finally
    Unlock;
  end;

  result.FromPool := self;

end;

procedure TThreadPoolBase.BaseNoNeedThread(thr: TManagedThread);
begin
  thr.PendingPoolAddition := true;
  thr.Stop(true);
  if not thr.IsCurrentThread then begin
    WaitForSignal(thr.evFinished);
  end;

  Lock;
  try
    thr.owner := self;
    thr.Pool := self;
    thr.LastUsed := GetTicker;
    //thr.Suspend;
    FACtiveThreads.remove(thr);
    if FIncomingthreads.indexof(thr) > 0 then
      Exception.create('Already in incoming threads');
    if Fthreads.indexof(thr) > 0 then
      Exception.create('Already in pooled threads');
    thr.Pool := self;
    IF thr.IsCurrentThread then begin
      FIncomingThreads.add(thr);
      signal(thr.evSleeping, true);//<---ONE SPOT!
    end else begin
      FThreads.add(thr);
      signal(thr.evSleeping, true);//<---ONE SPOT!
    end;




  finally
    Unlock;
  end;
  //thr.orderlysuspend;
end;


function TManagedThread.GetLastUsed: cardinal;
begin
  Lock;
  try
    result := FLastUsed;
  finally
    Unlock;
  end;
end;


procedure TManagedThread.SetLastUsed(const Value: cardinal);
begin
  Lock;
  try
    FLastUsed := value;
  finally
    Unlock;
  end;
end;



procedure TManagedThread.SetMM(sType: string);
var
  ti: integer;
begin
  if FMMTaskHandle <> INVALID_HANDLE_VALUE then
    exit;

  ti := 0;
  FMMTaskHandle := AvSetMmThreadCharacteristics_Safe('Pro Audio', ti);
end;


function AvSetMmThreadCharacteristics_Safe(s: PChar; var TaskIndex: integer): THandle; stdcall;
begin
  try
    result := AvSetMmThreadCharacteristicsW(s, TaskIndex);
  finally
  end;
end;

function AvRevertMmThreadCharacteristics_Safe(taskHandle: THandle): boolean;  stdcall;
begin
  try
    result := AvRevertMmThreadCharacteristics(taskHandle);
  finally
  end;
end;


procedure oinit;
begin
  gwatchthread := 0;
  KillFlag := false;
  Killed := false;
  TPM := TMasterThreadPool.create;
  UTF.RegisterClass(TUT_ManagedThread);

end;

procedure ofinal;
begin

  TPM.WakeAndFreeallthreads;
  TPM.StopCleaner;
  thread_shutdown_tracking_stage := 1;//<--notify if threads are still hanging around
  TPM.Shutdown := true;
  TPM.freewiththreads;
  TPM.free;
  TPM := nil;
  thread_shutdown_tracking_stage := 2;//<--serious errors if threads still hanging around
  Killed := true;

end;

{ TUT_ManagedThread }

procedure TUT_ManagedThread.DoExecute;
var
  a,b,c: ticker;
  t: integer;
  mt: TManagedThread;
begin
  inherited;
  case Variation of
    1: begin
      VariationName := 'Sleep Thread, then Free';
      Fthr := TUnitTestThread.Create(self, nil, nil);
      try
        a := GetTicker;
        Fthr.Start;
        FThr.WaitForFinish;
        b := GetTicker;
        c := (GetTimeSince(b,a) div 1000);

        utresult := 'Thread took about '+inttostr(c)+' seconds.';
      finally
        Fthr.DetachAndFree;
        FThr := nil;
      end;
    end;
    2: begin
      VariationName := 'Execute Thread, send to pool, get from pool, compare IDs';
      Fthr := TPM.Needthread(TUnitTestThread, self) as TUnitTestThread;
      try
        Fthr.Start;
        a := FThr.ThreadID;
        FThr.WaitForFinish;
      finally
        TPM.NoNeedthread(FThr);
        FThr := nil;
      end;
      sleep(2000);
      Fthr := TPM.Needthread(TUnitTestThread, self) as TUnitTestThread;
      try
        Fthr.Start;
        b := FThr.ThreadID;
        FThr.WaitForFinish;
      finally
        TPM.NoNeedthread(FThr);
        FThr := nil;
      end;
      if (b = a) then
        utresult := 'Thread IDs Match'
      else
        utresult := '**Thread IDs did NOT match.';
    end;
    3: begin
      VariationName := 'Check Thread Manager';
      Fthr := TUnitTestThread.Create(self, nil, nil);
      try
        a := GetTicker;
        Fthr.Start;
        FThr.WaitForFinish;
        b := GetTicker;
        c := (GetTimeSince(b,a) div 1000);

        utresult := '';
        BackGroundThreadMan.LockRead;
        try
          for t:= 0 to BackgroundThreadMan.Count-1 do begin
            if utresult <> '' then
              utresult := utresult + NEWLINE;

            mt := BackGroundThreadMan.Threads[t] as TManagedThread;
            utresult := utresult + mt.classname+' '+mt.status;
          end;
        finally
          BackgroundThreadMan.UnlockRead;
        end;

      finally
        Fthr.DetachAndFree;
        FThr := nil;
      end;
    end;
  end;
end;

{ TUnitTestThread }

procedure TUnitTestThread.DoExecute;
begin
  inherited;
  sleep(1000);
end;

{ TThreadLog }


{ TFiberManager }




{ TDoNothingThread }

procedure TDoNothingThread.DoExecute;
begin
  inherited;
{$IFDEF QUICK_THREAD_DEBUG}Debug.Log('iter '+t.tostring+' start. '+GetSignalDebug);{$ENDIF}
//  sleep(random(100));
  if dntdebug_t = t then
    raise Ecritical.create('t violation.  This thread is not an increment of t.  It ran TWICE');
  c := a + b;
  dntdebug_t := t;
{$IFDEF QUICK_THREAD_DEBUG}Debug.Log('iter '+t.tostring+' finish. '+GetSignalDebug);{$ENDIF}
  inc(dntdebug);
end;

{ TManagedSignal }

procedure TManagedSignal.Init;
begin
  inherited;
  setonlybythread := $FFFFFFFF;
  setnotbythread := $FFFFFFFF;
  Clearedonlybythread := $FFFFFFFF;
  Name := self.ClassName;
end;

procedure TManagedSignal.NotForThisthread;
begin
  SetNotByThread := getcurrentthreadid;
end;

procedure TManagedSignal.OnlyForThisThread;
begin
  SetOnlyBythread := getcurrentthreadid;
end;

procedure TManagedSignal.Signal(tf: boolean);
{var
  t: TManagedThread;}
begin
{  t := self.thr;
  t := nil;
  if tf then begin
    if t <> nil then
      Debug.Log('Signal '+name+' for '+thr.nameex+' '+booltostr(tf))
    else
      Debug.Log('Signal '+name+' for nil '+booltostr(tf));
  end else begin
    if t <> nil then
      Debug.Log('clear '+name+' for '+thr.nameex+' '+booltostr(tf))
    else
      Debug.Log('clear '+name+' for nil '+booltostr(tf));
  end;}
  if not ignorechecks then begin
    if tf then begin
      if GetCurrentThreadID = SetNotByThread then
        raise ECritical.create(self.Name+' owned by '+thr.NameEx+' cannot be set by thread '+getcurrentthreadid.tostring);
      if (SetOnlyByThread <> $FFFFFFFF) and (GetCurrentThreadID <> SetOnlyByThread) then
        raise ECritical.create(self.Name+' owned by '+thr.NameEx+' cannot be set by thread '+getcurrentthreadid.tostring);
    end else begin
      if (ClearedOnlyByThread <> $FFFFFFFF) and (GetCurrentThreadID <> ClearedOnlyByThread) then
        raise ECritical.create(self.Name+' owned by '+thr.NameEx+' cannot be CLEARED by thread '+getcurrentthreadid.tostring);
      if (ClearedNotByThread <> $FFFFFFFF) and (GetCurrentThreadID = ClearedNotByThread) then
        raise ECritical.create(self.Name+' owned by '+thr.NameEx+' cannot be CLEARED by thread '+getcurrentthreadid.tostring);
    end;
  end;

  inherited Signal(tf);

end;

function TManagedSignal.WaitFor(iTimeout: ni): boolean;
begin
  if GetcurrentthreadID = nowaitbythreadid then
    raise ECritical.create(self.name+' owned by '+thr.nameEx+' cannot wait for this signal from threadid '+GetCurrentThreadID.tostring);
  result := inherited;
//  WaitForSignal(self, iTimeout);
end;

{ TRealManagedThread }

procedure TRealManagedThread.Execute;
begin
  inherited;
  if assigned(mt) then
    mt.Execute
  else begin
    if not terminated then begin
      terminate;
    end;
  end;
end;


{ TThreadManager }
//------------------------------------------------------------------------------
constructor TThreadManager.Create;
//Standard.
begin
  inherited;
  FThreads:= TBetterlist<TManagedThread>.create;
end;
//------------------------------------------------------------------------------
procedure TThreadManager.DeRegisterThread(thread: TManagedThread);
//TManagedThread will call DeRegisterThread when it is freed to tell the thread
//manager that it is going away, and should therefore be managed no-longer.
begin
  LockWrite;
  try
    FThreads.remove(thread);
  finally
    UnlockWrite;
  end;
end;

destructor TThreadManager.Destroy;
//Standard.
begin
  Destroying := true;
  TerminateAllThreads;
  DetachAllthreads;
  FThreads.free;
  inherited;
end;

procedure TThreadManager.DetachAllThreads;
var
  thr: TMAnagedThread;
  t: integer;
begin
    for t:= 0 to Count-1 do begin
//      threads[t].freeonterminate := false;
      thr := threads[t];
      if threads[t] is TManagedThread then begin
        (TManagedthread(threads[t]).Detach);
      end;

    end;
end;

function TThreadManager.GetCount: integer;
//Getter for the Count property.
begin
  LockRead;
  try
    result := Fthreads.count;
  finally
    UnlockRead;
  end;
end;

function TThreadManager.GetCountActiveThreads: integer;
begin
  LockRead;
  try
    result := Count - CountReadyThreads;

  finally
    UnlockRead;
  end;
end;

function TThreadManager.GEtCountReadyThreads: integer;
var
  t: integer;
begin
  result := 0;
  LockRead;
  try
    for t:= 0 to count-1 do begin
      if threads[t] Is TManagedThread then begin
        if TManagedThread(Threads[t]).Ready then
          inc(result);

      end;
    end;

  finally
    UnlockRead;
  end;
end;

function TThreadManager.GetInfoList: TArray<TThreadInfo>;
var
  t: ni;
begin
  Lock;
  try
    setlength(result, count);
    for t:= 0 to count-1 do begin
      result[t] := threads[t].info;
      result[t].signaldebug := threads[t].GetSignalDebug;
    end;
  finally
    Unlock;
  end;

end;

function TThreadManager.GetThreads(idx: integer): TManagedThread;
//Getter for the Threads array property.
begin
  LockRead;
  try
    result := FThreads[idx];
  finally
    UnlockRead;
  end;
end;

function TThreadManager.HasThread(thread: TManagedThread): boolean;
//Returns whether the Manager is tracking a thread, at given reference.
begin
  LockRead;
  try
    result := FThreads.IndexOf(thread)>-1
  finally
    UnLockRead;
  end;
end;

function TThreadManager.HasThreadClass(threadclass: TManagedThreadClass): boolean;
//Returns whether or not the thread manager is managing any threads of a particular class (a simple form of flood protection).
//You can use this to make sure that there is no more than one instance of a thread class
//in the manager, or just to check on some backgroudn processing if you can determine that
//your background processing is complete by the disappearance of all instances of your
//class in the threadmanager.
//This is implemented for an interesting technique in asynchronous message processing.
//If you want an easy way to perform a bunch of tasks asynchronously and want to be notified when they
//complete... use this fucntion to see if there are any *managed* threads of
//the particular type active.<BR>
var
  t: integer;
begin
  result := false;
  LockRead;
  try
    for t:= 0 to count-1 do begin
      if TObject(FThreads[t]) is threadclass then begin
        result := true;
        break;
      end;
    end;
  finally
    UnLockRead;
  end;
end;

function TThreadManager.HasThreadName(sName: string): boolean;
//All descendants of TManagedThread can implement a Name property, which
//can then be used to lookup the thread in the thread manager. By giving your
//thread a unique name based on its role, you can implement flood protection, by
//ensuring that the same task is not called repeatedly over and over, or you
//can just checkup on tasks by naming them in a way where you can look them up later.
//<BR>
//One specific implementation of this is in PWLN's parellel fetching of account lists
//from multiple data-tiers.  A user could try to flood the middle tier by repeatedly
//requesting an account that doesn't exist.  PWLN imeplmemnts this kind of flood protection, by
//ensuring that only one thread is running for each data-tier requesting account information
//for data-base selection. A user is then unable to flood the system with threads, because
//if there is already a request being make to a particular data-tier, it just waits
//for that request to finish and uses the results it was already fetching, instead of
//queueing up a new thread, or running another in parellel.
var
  t: integer;
  thr: TManagedThread;
begin
  result := false;
  LockRead;
  try
    for t:= 0 to count-1 do begin
      if TObject(Fthreads[t]) is TManagedThread then begin
        thr := TManagedThread(FThreads[t]);
        if thr.Name = sName then begin
          result := true;
          break;
        end;
      end;
    end;

  finally
    UnlockRead;
  end;
end;
//------------------------------------------------------------------------------
procedure TThreadManager.RegisterThread(thread: TManagedThread);
//p: thread: TThread instance to be managed.
//TManagedThread calls RegisterThread to let the thread manager know its alive.
//Other kinds of threads can also be registered with the Thread Manager, but
//preferably use a descendant of TManagedThread to get some extra
//bells-and-whistles.<BR><BR>
//If you DO wish to use a different thread class, call RegisterThread in the constructor
//of the thread class and DeRegisterThread in the destructor.  The threads will be treated slightly differently than managed threads.
begin
  LockWrite;
  try
    FThreads.add(thread);
  finally
    UnLockWrite;
  end;
end;


procedure TThreadManager.TerminateAllThreads;
var
  t: integer;
begin
  self.LockWrite;
  try
    for t:= Count-1 downto 0  do begin
      threads[t].realthread.freeonterminate := false;
{$IFDEF LEAVE_CRITICAL_THREADS_RunNING}
      if threads[t].IsCriticalSystemThread then begin
        threads[t].Pool := nil;
        Fthreads.remove(threads[t]);
        continue;
      end;
{$ENDIF}

      threads[t].Terminate;

    end;

  finally
    self.UnlockWrite;
  end;

  WaitForThreads;

end;

function TThreadManager.ThreadClassCount(threadclass: TManagedThreadClass): integer;
var
  t: integer;
begin
  result := 0;
  LockRead;
  try
    for t:= 0 to count-1 do begin
      if TObject(FThreads[t]) is threadclass then begin
        inc(result);
      end;
    end;
  finally
    UnLockRead;
  end;
end;

procedure TThreadManager.WaitForThreads;
//Typically this is used before destroying the thread manager.
//It sends a Terminate signal to all threads, uncluding non-descendants of TManagedThread, and
//waits for them all to end gracefully.  The threads should receive the terminate signal,
//break ASAP and DeRegister themselves with the manager.  Once all threads are gone,
//this procedure returns. You can tell of your app is waiting on threads because
//it will sound a low frequecy tone approximately 2 times a second.
var
  t: integer;
  thr: TManagedThread;
  c: integer;
  wait: boolean;
begin
//  clx_windows.beep(100,50);
  c := 0;
  thr := nil;
  wait := false;

  while count > 0 do begin
//    clx_windows.beep(50,75);
    LockRead;
    c := count;
    try
      if count = 0 then break;



        thr := Fthreads[0];
      //thr.freeonterminate := false;
//      if not thr.terminated then thr.terminate;
      if thr.owner <> self then begin
        thr.manager := nil;
        wait := false;
      end else begin
        wait := NOT thr.realthread.freeonterminate;
        thr.manager := nil;
        thr.terminate;


      end;


      //if thr.suspended then
      //  thr.resume;



    finally
      UnLockRead;
    end;

    if count > c then begin
      raise exception.create('a thread was added whilst the thread manager was shutting down');
    end;


    if WAIT then begin
      if assigned(thr) then begin
        if thr.realthread.suspended then
          thr.realthread.resume;
          try
            thr.realthread.waitfor;
          finally
            thr.free;
          end;
  //    thr.freeonterminate := true;
      end;
    end;
    while c=count do
      sleepex(1,true);
    //sleepex(500);
  end;

//  clx_windows.beep(50,50);
end;

class procedure TManagedThread.Synchronize(const AThread: TThread;
  AMethod: TThreadMethod);
begin
  TThread.Synchronize(athread, amethod);
end;

class procedure TManagedThread.Synchronize(const AThread: TThread;
  AThreadProc: TThreadProcedure);
begin
  TThread.Synchronize(aThread, athreadproc);
end;

{ TFireWaitThread }

procedure TFireWaitThread.DoExecute;
begin
  inherited;
  HasWork := false;
end;

procedure TFireWaitThread.Fire;
begin
  WaitForIdle;
  if not Started then
    Start;
  Haswork := true;
end;

procedure TFireWaitThread.InitFromPool;
begin
  inherited;
  Loop := true;
  HasWork := false;
end;

{ TThreadInfo }

function TThreadInfo.GetAge: ticker;
begin
  result := GetTimeSince(LastUsed);
end;

initialization
  init.RegisterProcs('ManagedThread', oinit, nil, ofinal, nil, 'systemx,BackgroundThreads');
  dntdebug := 0;
  dntdebug_t := -1;
  gwatchthread := 0;
  ix := 0;

finalization




end.

