unit betterobject;
{x$DEFINE DEBUG_HOLDER}
{$INCLUDE DelphiDefs.inc}
{$MESSAGE '*******************COMPILING betterobject.pas'}
{$IFNDEF LAZ}
{$inline auto}
{$ENDIF}
{$D+}
{x$DEFINE REGISTER_OBJECTS}
{$IFDEF MACOS}
{$DEFINE NO_INTERLOCKED_INSTRUCTIONS}
{$ENDIF}
{$DEFINE NO_INTERLOCKED_INSTRUCTIONS}
{$DEFINE UNDEAD_PROTECTION}
{x$DEFINE OBJECT_DEBUG_FACILITIES}

{$DEFINE FREEDICT}
{x$DEFINE GIVER_IN_IHOLDER}
{$DEFINE GIVER_IN_TOBJECT}
{$DEFINE GIVER_POOLS}
{x$DEFINE THREAD_BLOCKING_DIAGNOSTICS}
{x$DEFINE LOCK_DEBUG}
{x$DEFINE CONTEXT_SWITCH}
{x$DEFINE DEAD_LOCK_DEBUG}
{x$DEFINE ALLOW_WATCHME}

//todo 1: I think if a RUDP connection fails, there's a chance that the connect packet will be double-freed

interface

uses
  {$IFNDEF FPC}
  system.rtlconsts,//inline
  system.types,//inline
  {$ENDIF}
  classes, sysutils,
  generics.collections.fixed,
  systemx, tickcount,
  //generics.collections.fixed;
  {$IFDEF MSWINDOWS}
    {$IFDEF LAZ}
        windows,
    {$ELSE}
        winapi.windows,
    {$ENDIF}
  {$ENDIF}
  typex;

const
  SHARED_SPIN = 0;

type
  ELockTimeout = class(Exception);
  TBetterObject = class;//forward

  IHolder<T: class> = interface
    function Get__Holding: T;
    procedure Set__Holding(const Value: T);
    property o: T read Get__Holding write Set__Holding;

  end;

  TSharedObject = class;//forward
{$IFDEF GIVER_POOLS}
  TGiver = class;//forward
{$ENDIF}

  THolder<T: class> = class;//forward


  TBetterObject = class(TInterfacedObject)
  private
{$IFDEF UNDEAD_PROTECTION}
    FDead: cardinal;
{$ENDIF}
    FreeAtRef: nativeint;
    FFreeWithReferences: boolean;
    fDetached: boolean;
    detachbegan, detachended: boolean;
    DestroyDangerousCompleted: boolean;
{$IFDEF GIVER_IN_TOBJECT}
    FReturnTo: TGiver;
{$ENDIF}
    procedure SetFreeWithReferences(const Value: boolean);
    function GetIsDead: boolean;
  protected
{$IFDEF NO_INTERLOCKED_INSTRUCTIONS}
{$IFDEF NOARC}
    FRefSect: TCLXCriticalSection;
{$ENDIF}
{$ENDIF}
    procedure DestroyDangerous;
    procedure DoDangerousDestruction;virtual;
  public
    Next, Prev: TBetterObject;
{$IFDEF LINKOWNERS}
    LinkOwner: TBetterOBject;
{$ENDIF}
    class var EnableRegistry: boolean;
    procedure AfterConstruction;override;
    procedure BeforeDestruction;override;

    function _AddRef: Integer;virtual;stdcall;
    function _Release: Integer;virtual; stdcall;
    function _RefCount: Integer;virtual; stdcall;

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;

    constructor Create;reintroduce;virtual;
    procedure DeadCheck;
    destructor Destroy;override;
    procedure BeginDetach;virtual;
    procedure EndDetach;virtual;
    procedure Detach;virtual;
    procedure DetachAndFree;
    procedure PreFree;
    procedure DnF;inline;
    property Detached: boolean read FDetached write Fdetached;

    procedure SafeFree;virtual;
    property FreeWithReferences: boolean read FFreeWithReferences write SetFreeWithReferences;
    function IsInterface(guid: TGUID):boolean;
{$IFNDEF FPC}
    function AsInterface<T:IUnknown>(guid: TGUID):T;
{$ENDIF}
    procedure Init;virtual;
    function GetObjectDebug: string;
    function ToHolder<T: class>(): IHolder<T>;
    procedure FreeByInterface;
    property IsDead: boolean read GetIsDead;
    function ShouldReturn: boolean;virtual;
    function ShouldGive: boolean;virtual;
{$IFDEF GIVER_IN_TOBJECT}
    property ReturnTo: TGiver read FReturnTo;//if taken from a giver, this property will be set.
    procedure Return;//INSTEAD OF calling FREE, try giving it to the giver instead (pooling)
{$ENDIF}

  end;


  TLightObject = TBetterObject;


  TBetterClass = class of TBetterObject;


  TBetterObjectClass = class of TBetterObject;

  THolder<T: class> = class(TBetterObject, IHolder<T>)
  private
    FTakenFrom: TBetterObject;
    function Get__Holding: T;
    procedure Set__Holding(const Value: T);
  protected
    procedure Init;override;
  public
    FO: T;
    constructor create;override;
    destructor Destroy;override;
{$IFDEF DEBUG_HOLDER}
    function _AddRef: Integer; override;
    function _Release: Integer;override;
{$ENDIF}
    property o: T read Get__Holding write Set__Holding;
    property TakenFrom: TBetterObject read FTakenFrom write FTakenFrom;
  end;



  TOwnableObject = class(TBetterObject)
  private
    FOwner: TObject;
  public
    property Owner: TObject read FOwner write FOwner;
  end;

  ELockNotFound = class(Exception);

  TLifeLock = class(TBetterObject)
  public
    o: TSharedObject;
    destructor Destroy;override;
  end;

  ILock = IHolder<TLifeLock>;

  TSharedObject = class(TBetterObject)
  private
    FSpinCount: ni;
    fWatchMe: boolean;
    FLockTimeout: ni;
    procedure SetSpinCount(const Value: ni);
  protected

  public
{$IFDEF CONTEXT_SWITCH}
    lastlocker: ni;
    waiting_threads: integer;
{$ENDIF}
    sect: TCLXCriticalSection;
    lockrefs: ni;
    function LockI: ILock;
    constructor Create; override;
    destructor Destroy; override;
    procedure Lock;virtual;
    procedure Unlock;virtual;
    function TryLock: boolean;overload;virtual;
    function TryLock(iTimeoutMS: integer): boolean;overload;virtual;
    procedure l;inline;
    procedure ul;inline;
    function tl(iTimeOutMs: integer): boolean;overload;inline;
    function tl: boolean;overload;inline;
    procedure Init;override;
    procedure Detach;override;
    property SpinCount: ni read FSpinCount write SetSpinCount;
    property WatchMe: boolean read fWatchMe write FWatchMe;
{$IFDEF MSWINDOWS}
    function IsLocked: boolean;
    function LockOwner: cardinal;
{$ENDIF}
    property LockTimeout: ni read FLockTimeout write FLockTimeout;
  end;

{$IFDEF GIVER_POOLS}
  TGiver = class(TSharedObject)
  private

  protected
    FList: TStringlist;
    function GiveEx(sContextKey: string): TObject;virtual;abstract;
    procedure BeforeReturn(obj: Tobject; var bCanTake: boolean);virtual;
    procedure AfterReturn(obj: Tobject);virtual;
  public
    procedure Return(obj: TObject; sContext: string = '');
    function GivenIsGivable(obj: Tobject): boolean;virtual;


    constructor Create; override;
    destructor Destroy; override;
  end;

  TGiverOf<T: TBetterObject> = class(TGiver)
  protected
    function GiveEx(sContextKey: string): TObject;override;
  public
    function Need(sContext: string = ''): IHolder<T>;virtual;
  end;


{$ENDIF}

  TLockRecordType = (ltRead, ltWrite);

  TLockRecord = class
    //This class represents a LOCK managed by TLockQueuedObject.  It tracks the ThreadID, and a independent reference counts for Read and Write locks.
  private
    FthreadID: cardinal;
    FWriteReferences: integer;
    FReadReferences: integer;
    FReferenceCount: integer;
    function GetReferenceCount(iType: TLockRecordType): integer;

  public
    constructor create;reintroduce;virtual;
    procedure Reference(iType: TLockRecordType);
    procedure Release(iType: TLockRecordType);
    property References[iType: TLockRecordType]: integer read GetReferenceCount;
    //returns the number of references for a given lock type (ltRead/ltWrite).
    property ThreadID: cardinal read FthreadID write FThreadID;
    //the ID of the thread used to make the lock.
  end;


  TFakeLockQueuedObject = class(TSharedObject)
    //This class supports the same interface as TLockQueuedObject, but doesn't
    //go through the motions of actually queuing up the locks, nor distinguishing
    //between read and write access.  As a result it is faster, but less scalable in
    //high-read low write situations.  Typically shared objects start off as
    //true TLockQueuedObjects then are downgraded To FAKE objects if it is
    //decided that there is no real value to the overhead of managing read/write and
    //queing.  See TLockQueuedObject documentation for a list of potential benefits.
  public
    function TryLockRead(iTimeOutMS: integer): boolean; overload;
    function TryLockRead: boolean;overload;
    procedure LockRead;
    function TryLockWrite(iTimeOutMS: integer): boolean;overload;
    function TryLockWrite: boolean;overload;
    procedure LockWrite;
    procedure UnlockRead;
    procedure LockString;
    procedure UnlockString;
   procedure UnlockWrite;
  end;

  TLockQueuedObject = class(TBetterObject)
    //This class is an effort to create shared objects with better balance and
    //flow control than is typically provided with standard Critical sections. It provides
    //an inheritable foundation that aims to provide the following benefits:
    //<LI>Allowing Simultaneous read access with exclusive write access to in-memory resources</LI>
    //<LI>Prevent monopolization of high-traffic critical sections by greedy threads by
    //enforcing a 'wait-your-turn' lock policy rather than the chaotic
    //timeslice fights that can occur with standard critical sections. This is especially benefitial
    //if a thread were to come along and lock an object for a long time, followed
    //by a short unlock period, and another long lock period.  Using the standard
    //system, that thread would most-likely block throughput of other threads
    //because the unlock and relock would typically happen in the same timeslice
    //and the chances of ending the timeslice in an unlocked state would be relatively lower.</LI>
  private
    sect: TCLXCRiticalSection;
    procedure Lock;
    procedure UnLock;
    procedure SetFreeWhenUnlocked(const Value: boolean);


  protected
    FLockQueues: TList<TLockRecord>;
    FFreeWhenUnlocked: boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LockRead;
    procedure LockWrite;
    procedure UnlockRead;
    procedure UnlockWrite;
    procedure LockString;
    procedure UnlockString;

    function TryLockWrite: boolean;
    function TryLockRead: boolean;

    function IsMyTurn: boolean;
    procedure QueueLock(iType: TLockRecordType);
    function DoLock(iType: TLockRecordType; bWait: boolean): boolean;
    procedure DoUnlock(iType: TLockRecordType);
    property FreeWhenUnlocked: boolean read FFreeWhenUnlocked write SetFreeWhenUnlocked;
    function LockQueueLength: ni;
  end;


  TInterfacedLockQueuedObject = class(TLockqueuedObject, IUnknown)
  private
    FDestroyWithReferences: boolean;
    FRefCount: integer;
    function GetDestroyWithReferences: boolean;
    procedure SetDestroyWithReferences(const Value: boolean);
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; override;stdcall;
    function _Release: Integer; override;stdcall;
    property DestroyWithReferences: boolean read GetDestroyWithReferences write SetDestroyWithReferences;
  end;

  TNamedLock = class (TSharedObject)
  private
    FName: string;
    FSpins: nativeint;
  public
    property Name: string read FName write FName;
    function TryGetSpin: boolean;
    procedure GetSpin;
    procedure ReleaseSpin;
  end;

  TNamedRWLock = class (TLockQueuedObject)
  private
    FName: string;
    FSpins: nativeint;
  public
    property Name: string read FName write FName;
    function LockRefs: ni;
  end;

  TNamedLocks = class (TSharedObject)
  protected
    FLocks: TList<TNamedLock>;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Init;override;
    procedure GetLock(sName: string; bSpin: boolean = false);
    function TryGetLock(sName: string; bSpin: boolean = false): boolean;
    procedure ReleaseLock(sName: string; bSpin: boolean = false);
    function IndexOfLock(sName: string): integer;
    function FindLock(sName: string): TNamedLock;
    function GetLockDebug: string;
    procedure DebugLocks;
  end;

  TNamedRWLocks = class (TSharedObject)
  protected
    FLocks: TList<TNamedRWLock>;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Init;override;
    procedure GetLock(sName: string; bForWrite: boolean);
    function TryGetLock(sName: string; bForWrite: boolean): boolean;
    procedure ReleaseLock(sName: string; bForWrite: boolean);
    function IndexOfLock(sName: string): integer;
    function FindLock(sName: string): TNamedRWLock;
  end;

  TStandardMonitor = class;//forward

  TMonitorableObject = class(TSharedObject)
  private
    fstatusHistory: array[0..512] of string;
    fStatusIndex:ni;
    FStatus: string;
    statuslock: TCLXCriticalSection;
    function GetStatus: string;
    procedure SetStatus(const Value: string);
  protected
    FMonitoredBy: TList<TStandardmonitor>;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Detach;override;
    procedure UnregisterFromMonitors;
    procedure RegisterTo(mon: TStandardMonitor);
    property Status: string read GetStatus write SetStatus;
  end;

  TMonitor<T: TMonitorableObject> = class(TSharedObject)
  private
    FList: TList<T>;
    function Getcount: nativeint;
    function GetProcs(idx: nativeint): T;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure RegisterObject(proc: T);
    procedure UnRegisterObject(proc: T);
    property Count: nativeint read Getcount;
    property Monitored[idx: nativeint]: T read GetProcs;
  end;

  TStandardMonitor = class(TMonitor<TMonitorableObject>)
  public
  end;

  IAutoScope = interface
    procedure Track(o: TObject);
  end;

  TAutoScope = class(TSharedObject, IAutoScope)
  protected
    tracking: array of TObject;
    procedure DoDangerousDestruction; override;
  public

    procedure Track(o: TObject);
    procedure DestroyObjectsInScope;

  end;

  TFatMessage = class(TBetterObject)
  public
    //if you add new members also update Copy() constructor
    //if you add new members also update Copy() constructor
    messageClass: string;
    params: TArray<String>;
    //if you add new members also update Copy() constructor
    //if you add new members also update Copy() constructor
    handled: boolean;
    function Copy: IHolder<TFatMessage>;
  end;

  TFatMessageQueue = class(TSharedObject)
  private
    sectSubQueues: TCLXCriticalSection;
    pending: TArray<IHolder<TFatMessage>>;
    subqueues: TList<TFatMessageQueue>;
    function GetNextMessage: IHolder<TFatMessage>;
    procedure Posted;virtual;
  public
    handler: TFunc<IHolder<TFatMessage>, boolean>;
    onposted: TProc;
    procedure Post(m: IHolder<TFatMessage>);//process later
    function Send(m: IHolder<TFatMessage>): boolean;//send through hierarchy... like a broadcast, but synchronous, stops when handled
    procedure Broadcast(m: IHolder<TFatMessage>);//post copies into hierarchy
    function ProcessNextMessage: boolean;
    function NewSubQueue: TFatMessageQueue;
    procedure DeleteSubQueue(fmq: TFatMessageQueue);
    constructor Create; override;
    procedure Detach; override;
    function NewMessage: IHolder<TFatMessage>;
    procedure QuickBroadcast(messageClass: string);overload;
    procedure QuickBroadcast(messageClass: string; params: TArray<String>);overload;

  end;

  TMainMessageQueue = class(TFatMessageQueue);



var
  MMQ, MainMessageQueue: TMainMessageQueue;
  gNamedLocks: TNamedLocks;
  nodebug: boolean;

const
  WRITE_LOCK = 2;
  READ_LOCK = 1;


function AutoScope: IAutoScope;



implementation

{ TBetterObject }

uses BetterObjectRegistry, orderlyinit, debug;

function AutoScope: IAutoScope;
var
  tas: TAutoScope;
begin
  tas := TAutoScope.create;
  result := tas;
{$IFDEF NOARC}
  tas._Release;
{$ENDIF}

end;

procedure TBetterObject.AfterConstruction;
begin
  //no implementation needed
end;

{$IFNDEF FPC}
function TBetterObject.AsInterface<T>(guid: TGUID): T;
begin
  if IsInterface(guid) then begin
    //Supports(self, T, result);
    self.QueryInterface(guid,result);
  end;

end;
{$ENDIF}

procedure TBetterObject.BeforeDestruction;
begin
  Detach;
  if _RefCount > 1 then begin
    FreeWithReferences := true;
    raise EAbort.create('Trying to free '+self.ClassName+' with more than 1 reference');

  end;

end;


procedure TBetterObject.BeginDetach;
begin
  detachbegan := true;
end;

constructor TBetterObject.create;
begin
{$IFDEF NO_INTERLOCKED_INSTRUCTIONS}
{$IFDEF NOARC}
  ics(FRefSect);
{$ENDIF}
{$ENDIF}
{$IFDEF REGISTER_OBJECTS}
  if TBetterObject.EnableRegistry then
    bor.ObjectCreated(TBetterClass(self.ClassType), '');
{$ENDIF}
  inherited create;
  Init;
end;

procedure TBetterObject.DeadCheck;
begin
{$IFDEF UNDEAD_PROTECTION}
  if FDead = $DEAD then
    raise ECritical.create('Double-free attempt detected in '+self.ClassName);
{$ENDIF}
end;

destructor TBetterObject.Destroy;
begin
  DestroyDangerous;
  if not FDetached then
    Detach;
  inherited;
{$IFDEF REGISTER_OBJECTS}
  if TBetterObject.EnableRegistry then
    bor.ObjectDestroyed(TBetterClass(self.ClassType), '');
{$ENDIF}
{$IFDEF NO_INTERLOCKED_INSTRUCTIONS}
{$IFDEF NOARC}
  dcs(FRefSect);
{$ENDIF}
{$ENDIF}
{$IFDEF UNDEAD_PROTECTION}
  FDead := $DEAD;
{$ENDIF}


end;


procedure TBetterObject.DestroyDangerous;
begin
  if DestroyDangerousCompleted then
    exit;

  try
    try
      DoDangerousDestruction;
    except
      on E: Exception do begin
        Debug.Log('Exception in '+self.classname+'.DestroyDangerous was SUPPRESSED because raising Exceptions during Destruction is a very bad thing.  This object will potentially be leaked.');
        Debug.Log('The Exception was: '+e.message);
        Debug.Log('Since there might be other functionality dependent on this failed ');
        Debug.Log('destruction, the proper resolution for this issue is considered "undefined" by rule. ');
        Debug.Log('Recommended action is to do dangerous destruction via public methods before actual calls to Destroy() or Free()');
      end;
    end;
  finally
    DestroyDangerousCompleted := true;//flag "do not call again"... even if we leaked stuff
  end;
end;

procedure TBetterObject.Detach;
begin
  //
  if FDetached then
    exit;
  BeginDetach;
  EndDetach;
  FDetached := true;

end;

procedure TBetterObject.DetachAndFree;
begin
  if self = nil then exit;

  Detach;
{$IFDEF NOARC}
  Free;
{$ENDIF}

end;

procedure TBetterObject.DnF;
begin
  DetachAndFree;
end;

procedure TBetterObject.DoDangerousDestruction;
begin
  {$IFDEF UNDEAD_PROTECTION}
    DeadCheck;
  {$ENDIF}
end;

procedure TBetterObject.EndDetach;
begin
  detachended := true;
end;

procedure TBetterObject.FreeByInterface;
begin
  FreeAtRef := 1;
end;

function TBetterObject.GetIsDead: boolean;
begin
  result := FDead = $DEAD;
end;

function TBetterObject.GetObjectDebug: string;
begin
  result := classname+'@'+inttostr(ni(pointer(self)));
end;

{$IFDEF GIVER_IN_TOBJECT}
procedure TBetterObject.Return;
begin
  if Returnto = nil then begin
    free;
  end else begin
    Returnto.Return(self);
  end;



end;
{$ENDIF}

procedure TBetterObject.Init;
begin
  //
end;

function TBetterObject.IsInterface(guid: TGUID): boolean;
var
  cout:IUnknown;
begin
  result := self.QueryInterface(guid,cout)= 0;
end;


procedure TBetterObject.PreFree;
//This function calls Detach() and DestroyDangerous() ... essentially doing
//essentially doing the heavy lifting before the actual destruction.
//you should call this if there are concerns about exceptions being raise during
//the destructor of an object.  It is a good idea to PreFree stuff if there is
//the potential for issues during destruction... e.g. a Stream might hit an
//error during a required flush() operation during destruction... or
//Generally speaking... anything that does I/O of any kind, network, disk, usb...
//is potentially dangerous.. and should be in DestroyDangerous() and
//PreFree should be called before the object goes out of scope
//if you don't call PreFree, PreFree will be automatically called during the
//destructor... but again... not the best idea in some cases.
//*NOTE*NOTE*NOTE* that calling PreFree() does not suppress Exceptions
//instead opting for you to implement your own exception handling in the calling
//function.
begin
  Detach();
  DoDangerousDestruction;
end;

function TBetterObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;

end;

procedure TBetterObject.SafeFree;
begin
  Detach;
  if RefCount > 0 then begin
    FreeWithReferences := true;
    exit;
  end else
    Free;
end;

procedure TBetterObject.SetFreeWithReferences(const Value: boolean);
begin
//  if self.classname = 'TReliableUDPPacketLogRecord' then
//    FFreeWithReferences := Value;
  FFreeWithReferences := Value;

end;



function TBetterObject.ShouldGive: boolean;
begin
  result := true;
end;

function TBetterObject.ShouldReturn: boolean;
begin
  result := true;
end;

function TBetterObject.ToHolder<T>: IHolder<T>;
begin
  result := THolder<T>.create;
  result.o := T(self);
end;

function TBetterObject._AddRef: Integer;
begin
{$IFNDEF NOARC}
  result := inherited _AddRef;
{$ELSE}
  ecs(FRefSect);
  inc(FrefCount);
  Result := FRefCount;
  lcs(FRefSect);
{$ENDIF}

end;

function TBetterObject._RefCount: Integer;
begin
{$IFNDEF NOARC}
  result := RefCount;
{$ELSE}
  ecs(FRefSect);
  result := FRefCount;
  lcs(FRefSect);
{$ENDIF}

end;

function TBetterObject._Release: Integer;
begin
  if FDead = $DEAD then
    raise ECritical.create('trying to release a dead object');

{$IFDEF NOARC}
  ecs(FRefSect);
{$ENDIF}
  DeadCheck;

{$IFNDEF NOARC}
  result := inherited _Release();
{$ELSE}
  dec(FRefCount);
  Result := FRefCount;

  lcs(FRefSect);

  if (Result = FreeAtRef) or ((Result = 0) and FreeWithReferences) then begin
{$IFDEF NOARC}
    Destroy;//<--- android has ARC, don't destroy on FMX platforms
{$ELSE}
{$IFDEF DEBUG_HOLDER}
    Debug.Log('Will not destroy '+self.classname+' because of ARC');
{$ENDIF}
//    Destroy;
//    Detach;
{$ENDIF}
  end;
{$ENDIF}



end;

procedure oinit;
begin
  MainMessageQueue := TMainMessageQueue.create;
  MMQ := MainMessageQueue;
  gNamedLocks := TNamedLocks.create;

//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure ofinal;
begin
  gNamedLocks.free;
  gNamedLocks := nil;
  MainMessageQueue.free;
  MainMessageQueue := nil;
  MMQ := MainMessageQueue;
//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;




{ THolder<T> }

constructor THolder<T>.create;
begin
  inherited;
{$IFDEF NOARC}
  FreeByInterface;
{$ELSE}
  FreeWithReferences := true;
{$ENDIF}
end;

destructor THolder<T>.Destroy;
begin
{$IFDEF GIVER_IN_TOBJECT}
  IF FO <> nil then begin
{$IFDEF DEBUG_HOLDER}
    Debug.Log('Destroy holder '+self.classname+' @'+inttohex(nativeint(pointer(SELF)),2)+' holding '+FO.Classname);
{$ENDIF}

    if FO is TBetterObject then begin
      TBetterObject(FO).Return; //IF the object came from a pool, it will be given to the pool, else freed
    end else begin
      FO.free;
    end;
  end;
{$ELSE}
  {$IFDEF GIVER_IN_IHOLDER}
    if FTakenFrom <> nil then
      TGiverOf<T>(FTakenFrom).Take(FO);
      FO := nil;
  {$ELSE}
    FO.Free;
  {$ENDIF}
{$ENDIF}
  FO := nil;
  inherited;
end;

function THolder<T>.Get__Holding: T;
begin
  result := FO;
end;

procedure THolder<T>.Init;
begin
  inherited;

end;

procedure THolder<T>.Set__Holding(const Value: T);
begin
//  if assigned(Fo) then
//    Fo.free;

  FO := value;
  if Fo is TBetterObject then
    TBetterObject(Fo).FreeWithReferences := true;

//  if Fo is TSharedObject then begin
//    {$IFNDEF NO_SHARED_OBJECT_HOLDER_REMINDER}
//      Debug.Log('Note that wrapping a '+Fo.classname+' descended from TSharedObject has the effect of releasing the global singleton reference and should not be used for singleton objects.');
//    {$ENDIF}
//    Fo._Release();
//  end;





end;

{$IFDEF DEBUG_HOLDER}
function THolder<T>._AddRef: Integer;
begin
  result := inherited _AddRef();
{$IFDEF DEBUG_HOLDER}
  Debug.Log('Add ref '+self.classname+' @'+inttohex(nativeint(pointer(SELF)),2)+' now has '+inttostr(FRefCount)+' refs.');
{$ENDIF}
end;

function THolder<T>._Release: Integer;
begin
{$IFDEF DEBUG_HOLDER}
  var bTrap := (self.classname = 'THolder<RDTPSQLconnectionClientEx.TRDTPSQLConnectionClientEx>');

  if (FRefCount=1) and bTrap then
    Debug.Log('release here');
  Debug.Log('Release ref '+self.classname+' @'+inttohex(nativeint(pointer(SELF)),2)+' now has '+inttostr(FRefCount-1)+' refs.');
{$ENDIF}
  result := inherited _Release();

  if (FRefCount=0) and bTrap then
    Debug.Log('released!');
end;
{$ENDIF}

{ TLightObject }

{$IFDEF GIVER_POOLS}
procedure Tgiver.Return(obj: TObject; sContext: string = '');
var
  cantake: boolean;
begin
  Debug.Log('Take '+obj.classname);
  Lock;
  try
    cantake := true;
    BeforeReturn(obj, {out}cantake);
    if cantake then begin
      Flist.addObject(sContext, obj);
      Debug.Log('there are now '+Flist.count.ToString+' '+obj.Classname+'(s) in '+classname);
      AfterReturn(obj);
    end else
      obj.free;

  finally
    Unlock;
  end;

end;
procedure TGiver.AfterReturn(obj: Tobject);
begin
//
end;

procedure TGiver.BeforeReturn(obj: Tobject; var bCanTake: boolean);
begin
  bcantake := true;
//
end;

constructor TGiver.Create;
begin
  inherited;
  Flist := TStringList.create;
end;

destructor TGiver.Destroy;
var
  o: TObject;
begin
  Lock;
  try
    try
      while FList.count > 0 do begin
        o := FList.objects[fList.count-1];
        FList.delete(FList.count-1);
        o.free;
        o := nil;
      end;
    except
    end;
  finally
    unlock;
  end;

  FList.free;
  inherited;
end;

function TGiver.GivenIsGivable(obj: Tobject): boolean;
begin
  //
  result := true;
end;

{$ENDIF}

{$IFDEF MSWINDOWS}
function TSharedObject.IsLocked: boolean;
begin
  result := self.sect.cs.LockCount > 0;
end;
{$ENDIF}

procedure TSharedObject.l;
begin
  Lock;
end;

procedure TSharedObject.Lock;
var
  tid: ni;
begin
//  if sect.DebugInfo = nil then
//    raise exception.Create('wtf');
  try
    {$IFDEF LOCK_DEBUG}
    if not nodebug then
    Debug.Log(self.classname+' about to wait for lock @'+inttohex(integer(@sect),8));
    {$ENDIF}

{$IFDEF CONTEXT_SWITCH}
    //if another thread wants this, and we were the one
    //to get it last, ease up and try to allow the other guy to get it
    if (waiting_threads > 0) and (sect.LockCount = 0) and (lastlocker = ni(GetCurrentThreadID)) then
      sleep(0);//force context switch so someone else can get the lock



    InterlockedIncrement(waiting_threads);
{$ENDIF}
    {$IFDEF DEAD_LOCK_DEBUG}
    if not (TryLock(1000)) then begin
//      if not (self.ClassName='TCommandProcessor') then
//      if self.ClassName = 'Tmodosc_EQ' then
      Debug.Log(self, self.ClassName+' is blocked on thread '+inttostr(GetCurrentThreadID())+' by '+inttostr(sect.cs.owningthread),'error');
      if LockTimeout = 0 then
        EnterCriticalSection(sect.cs)
      else
        if not TryLock(LockTimeout) then
          raise ELockTimeout.create('lock timeout waiting for '+self.ClassName+' owned by #'+sect.cs.OwningThread.tostring);
      Debug.Log(self, self.ClassName+' is unblocked on thread '+inttostr(GetCurrentThreadID()),'error');
    end;
    {$ELSE}
    if LockTimeout = 0 then
      ecs(sect)
    else
      if not TryLock(LockTimeout) then begin
{$IFDEF MSWINDOWS}
        raise ELockTimeout.create('lock timeout waiting for '+self.ClassName+' owned by #'+sect.cs.OwningThread.tostring);
{$ELSE}
        raise ELockTimeout.create('lock timeout waiting for '+self.ClassName);
{$ENDIF}
      end;
    {$ENDIF}
{$IFDEF CONTEXT_SWITCH}
    lastlocker := GetCurrentThreadId;
    InterlockedDecrement(waiting_threads);

{$ENDIF}



    {$IFDEF LOCK_DEBUG}
    if not nodebug then
    Debug.Log(self.classname+' got lock @'+inttohex(integer(@sect),8));
    {$ENDIF}
    lockrefs := lockrefs + 1;

  except
    on E: Exception do begin
      e.message := 'Failed to lock '+self.classname+ ':'+e.message;
      raise;
    end;
  end;

end;

function TSharedObject.LockI: ILock;
begin
  self.Lock;
  result := THolder<TLifeLock>.create;
  result.o := TLifeLock.create;
  result.o.o := self;

end;

{$IFDEF MSWINDOWS}
function TSharedObject.LockOwner: cardinal;
begin
  result := sect.cs.OwningThread;
end;
{$ENDIF}

procedure TSharedObject.SetSpinCount(const Value: ni);
begin
  FSpinCount := Value;
{$IFDEF MSWINDOWS}
  SetCriticalSectionSpinCount(sect.cs, value);
{$ENDIF}
end;

function TSharedObject.tl(iTimeOutMs: integer): boolean;
begin
  result := TryLock(itimeOutMs);
end;

function TSharedObject.tl: boolean;
begin
  result := TryLock;
end;

function TSharedObject.TryLock: boolean;
begin
  result := tecs(sect);
  if result then
    lockrefs := lockrefs+1;

end;

function TSharedObject.TryLock(iTimeoutMS: integer): boolean;
var
  tm1, tm2: cardinal;
begin
  tm1 := GetTicker;
  result := true;
  while not tecs(sect) do begin
    sleep(1);
    tm2 := GetTicker;
    if tm2<tm1 then
      tm2 := tm1;

    if (tm2-tm1) > cardinal(iTimeOutMS) then begin
      result := false;
      break;
    end;
  end;

  if result then
    lockrefs := lockrefs + 1;

  {$IFDEF LOCK_DEBUG}
  if not nodebug then
  Debug.Log(self.classname+' Tried to get lock @'+inttohex(integer(@sect),8)+' returned '+booltostr(result));
  {$ENDIF}

end;

procedure TSharedObject.ul;
begin
  Unlock;
end;

procedure TSharedObject.Unlock;
begin
  lockrefs := lockrefs - 1;
  lcs(sect);

  {$IFDEF LOCK_DEBUG}
  if not nodebug then
  Debug.Log(self.classname+' released lock @'+inttohex(integer(@sect),8));
  {$ENDIF}
end;

{ TLockQueuedObject }

constructor TLockQueuedObject.Create;
begin
  inherited;
  ics(sect);
  FLockQueues := TList<TLockREcord>.create;

end;

destructor TLockQueuedObject.Destroy;
begin
  FLockQueues.free;
  dcs(sect);
  inherited;

end;

function TLockQueuedObject.IsMyTurn: boolean;
//Reports whether it is the current threads turn to have the lock.
//Essentially this will
var
  t: integer;
  lck: TLockRecord;
begin
  result := false;
  Lock;
  try
    //if there are no locks at all then exit;
    if FLockQueues.count = 0 then
      raise Exception.create('Expected lock unexpectedly disappeared.');

    //scan up the list until we either find a write lock -- (result false)... or our
    //own thread ID with no write references (result true)
    for t := 0 to FLockQueues.count-1 do begin
      lck := TLockRecord(FLockQueues[t]);
      if (lck.ThreadID = GetCurrentThreadID) then begin
        result := (lck.FWriteReferences = 0) or (t=0);
        exit;
      end
      else if lck.References[ltWrite]>0 then begin
        result := false;
        exit;
      end;
    end;
    //if we got to this point and still false.. then something is terribly wrong
    raise Exception.create('Synchronization error in Lock Queued Object.  Expected lock was not found.');
  finally
    UnLock;
  end;
end;

function TLockQueuedObject.DoLock(iType: TLockRecordType; bWait: boolean): boolean;
begin
  if not bWait then begin

    QueueLock(iType);
    if not IsMyTurn then begin
      DoUnlock(iType);
      result :=  false;
    end else begin
       result := true;
    end;
  end else begin

    QueueLock(iType);

    //wait for my turn
    while not IsMyTurn do if Random(100)>0 then sleep(1) else sleep(0);

    result := true;
  end;

end;


procedure TLockQueuedObject.QueueLock(iType: TLockRecordType);
var
  t,u: integer;
  curLock, foundLock: TLockRecord;
  iThreadID: cardinal;
  bInserted: boolean;
begin
  Lock;
  try
    foundLock := nil;
    //scan all locks for one that matches
    iThreadID := GetCurrentThreadID;
    for t:= 0 to FLockQueues.count-1 do begin
      curLock := TLockRecord(FLockQueues[t]);
      //if we found a matching thread, then add a reference to the existing lock
      if (curlock.ThreadID = iThreadID) then begin
        foundLock := curLock;

        //if we're adding the first reference to a write lock... then move it just before any other queued write lock
        if (itype = ltWrite) and (foundlock.References[ltWrite]=0) then begin
          FLockQueues.Remove(foundlock);

          //insert the write lock
          bInserted := false;
          for u:= 0 to FLockqueues.count-1 do begin
            if TLockREcord(FLockQueues[u]).references[ltwrite] > 0 then begin
              FLockqueues.insert(u, foundlock);
              bInserted := true;
            end;
          end;
          if not bInserted then
            FLockQueues.Add(foundlock);

        end;

        foundlock.Reference(iType);

      end;
    end;

    //if no matching lock was found then create and add one
    if foundLock = nil then begin
      foundLock := TLockRecord.create;
      foundLock.ThreadID := iThreadID;
      foundLock.Reference(iType);
      FLockQueues.add(foundLock);
    end;
  finally
    UnLock;
  end;

  //wait for my turn
//  while not IsMyTurn do if Random(100)>0 then sleep(1) else sleep(0);

end;


procedure TLockQueuedObject.SetFreeWhenUnlocked(const Value: boolean);
begin
  LockWrite;
  try
    FFreeWhenUnlocked := Value;
  finally
    UnlockWrite; //<--- if only reference... then free will happen here... else delay
  end;
end;

function TLockQueuedObject.TryLockWrite: boolean;
begin
  result := DoLock(ltWrite, false);
end;

procedure TLockQueuedObject.LockRead;
begin
  DoLock(ltRead, true);
end;

function TLockQueuedObject.TryLockRead: boolean;
begin
  result := DoLock(ltRead, false);
end;

procedure TLockQueuedObject.LockWrite;
begin
  DoLock(ltWrite, true);
end;

procedure TLockQueuedObject.DoUnlock(iType: TLockRecordType);
var
  t: integer;
  curLock, foundLock: TLockRecord;
  iThreadID: cardinal;
  bFree: boolean;
begin
  bFree := false;
  Lock;
  try
    foundLock := nil;
    //scan all locks for one that matches
    iThreadID := GetCurrentThreadID;
    for t:= 0 to FLockQueues.count-1 do begin
      curLock := TLockRecord(FLockQueues[t]);
      if (curlock.ThreadID = iThreadID) then begin
        foundLock := curLock;
      end;
    end;

    //if no matching lock was found then raise an exception
    if foundLock = nil then begin
      raise Exception.create('fatal lock error');
    end else begin
      foundlock.Release(iType);
      if (foundLock.References[ltRead] = 0) and
        (foundLock.References[ltWrite] = 0) then
      begin
        FLockQueues.remove(foundlock);
        foundlock.Free;
      end;


    end;
    if FreeWhenUnlocked and (FLockQueues.count = 0) then begin
      bFree := true;
    end;
  finally
    UnLock;
  end;

  if bFree then
    free;


end;

procedure TLockQueuedObject.UnlockWrite;
begin
  //same as unlockread
  DoUnLock(ltWrite);
end;

procedure TLockQueuedObject.UnlockRead;
begin
  DoUnLock(ltRead);
end;

procedure TLockQueuedObject.Lock;
begin
  inherited;
  ecs(sect);
end;

function TLockQueuedObject.LockQueueLength: ni;
begin
  Lock;
  try
    result := FLockQueues.count;
  finally
    Unlock;
  end;
end;

procedure TLockQueuedObject.UnLock;
begin
  lcs(sect);
  inherited;

end;

procedure TLockQueuedObject.LockString;
begin
  LockWRite;
end;

procedure TLockQueuedObject.UnlockString;
begin
  UnlockWrite;
end;

{ TLockRecord }

constructor TLockRecord.create;
begin
  inherited;
  FReferenceCount := 0;
  FThreadID := 0;

end;


function TLockRecord.GetReferenceCount(iType: TLockRecordType): integer;
//Returns a reference count for the given lock type.  A given lock
//can have multiple write and read lock references.
begin
  result := 0;
  case iType of
    ltRead: result := FReadReferences;
    ltWrite: result := FWriteReferences;
  end;
end;

procedure TLockRecord.Reference(iType: TLockRecordType);
//Adds a reference of given type to the lock. Valid types are ltRead and ltWrite
begin
  case iType of
    ltRead: inc(FReadReferences);
    ltWrite: inc(FWriteReferences);
  end;
end;

procedure TLockRecord.Release(iType: TLockRecordType);
//Releases a reference of given type.
begin
  case iType of
    ltRead: dec(FReadReferences);
    ltWrite: dec(FWriteReferences);
  end;

end;

{ TFakeLockQueuedObject }

procedure TFakeLockQueuedObject.LockRead;
begin
  Lock;
end;

//------------------------------------------------------------------------------
procedure TFakeLockQueuedObject.LockString;
begin
  LockWrite;
end;

procedure TFakeLockQueuedObject.LockWrite;
begin
  Lock;
end;
//------------------------------------------------------------------------------
function TFakeLockQueuedObject.TryLockRead: boolean;
begin
  result := trylock;
end;
//------------------------------------------------------------------------------
function TFakeLockQueuedObject.TryLockRead(iTimeOutMS: integer): boolean;
begin
  result := TryLock(iTimeOutMS);
end;
//------------------------------------------------------------------------------
function TFakeLockQueuedObject.TryLockWrite: boolean;
begin
  result := TryLock;
end;
//------------------------------------------------------------------------------
function TFakeLockQueuedObject.TryLockWrite(iTimeOutMS: integer): boolean;
begin
  result := TryLock(iTimeOutMS);
end;
//------------------------------------------------------------------------------
procedure TFakeLockQueuedObject.UnlockRead;
begin
  UnLock;
end;
//------------------------------------------------------------------------------
procedure TFakeLockQueuedObject.UnlockString;
begin
  UnlockWRite;
end;

procedure TFakeLockQueuedObject.UnlockWrite;
begin
  UnLock;
end;

{ TinterfacedLockQueuedObject }

function TinterfacedLockQueuedObject.GetDestroyWithReferences: boolean;
begin
  LockRead;
  try
    result := FDestroyWithReferences;
  finally
    UnlockRead;
  end;
end;

function TinterfacedLockQueuedObject.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;

end;

procedure TinterfacedLockQueuedObject.SetDestroyWithReferences(
  const Value: boolean);
begin
  LockWRite;
  try
    FDestroyWithReferences := value;
  finally
    UnlockWrite;
  end;

end;

function TinterfacedLockQueuedObject._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);

end;

function TinterfacedLockQueuedObject._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if (result = 0) and self.DestroyWithReferences then
    Destroy;//todo 5: this is probably not thread safe, but who cares since we never use it


end;

{ TNamedLocks }

constructor TNamedLocks.Create;
begin
  inherited;
  FLocks := TList<TNamedLock>.create;
end;

procedure TNamedLocks.DebugLocks;
begin
  Debug.Log(GetLockDebug);
end;

destructor TNamedLocks.Destroy;
begin
  FLocks.free;
  inherited;
end;

function TNamedLocks.FindLock(sName: string): TNamedLock;
var
  i: integer;
begin
  result := nil;
  Lock;
  try
    result := nil;
    i := IndexOfLock(sName);
    if i >= 0 then
      result := FLocks[i];
  finally
    Unlock;
  end;

end;

procedure TNamedLocks.GetLock(sName: string; bSpin: boolean = false);
begin
  while not TryGetLock(sName, bSpin) do
    sleep(1);

{$IFDEF DEBUG_NAMED_LOCKS}
  DebugLocks;
{$ENDIF}


end;


function TNamedLocks.GetLockDebug: string;
var
  t: ni;
  sName: string;
begin
  Lock;
  try
    sName := lowercase(sName);
    result := 'Locks: ';
    for t:= 0 to FLocks.count-1 do begin
      sNAme := lowercase(FLocks[t].name);
      result := result + sName+' ';
    end;
  finally
    Unlock;
  end;
end;

function TNamedLocks.IndexOfLock(sName: string): integer;
var
  t: integer;
begin
  Lock;
  try
    sName := lowercase(sName);
    result := -1;
    for t:= 0 to FLocks.count-1 do begin
      if lowercase(FLocks[t].name) = sName then begin
        result := t;
        exit;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TNamedLocks.Init;
begin
  inherited;

end;

procedure TNamedLocks.ReleaseLock(sName: string; bSpin: boolean = false);
var
  l: TNamedLock;
begin
  lock;
  try
    l := FindLock(sName);
    if l = nil then
      raise ELockNotFound.create('Lock not found '+sName);

    if bSpin then
      l.ReleaseSpin
    else
      l.Unlock;

//    Debug.Log('Released Lock '+sName+ ' ('+l.lockrefs.tostring+' refs)');

    if l.Lockrefs = 0 then begin
      FLocks.remove(l);
      l.free;
    end;




  finally
    unlock;
  end;
end;

function TNamedLocks.TryGetLock(sName: string; bSpin: boolean): boolean;
var
  l: TNamedLock;
begin
  lock;
  try
    l := FindLock(sName);
    if l = nil then begin
      l := TNamedLock.create;
      l.Name := lowercase(sNAme);
      FLocks.add(l);
    end;

    if bSpin then
      result := l.TryGetSpin
    else
      result := l.TryLock;

//    if result then
//      Debug.Log('Got lock '+sName+ ' ('+l.lockrefs.tostring+' refs)');

  finally
    unlock;
  end;

end;

{ TNamedLock }

procedure TNamedLock.GetSpin;
begin
  Lock;
  try
    while FSpins > 0 do begin
      Unlock;
      sleep(random(100));
      Lock;
    end;
    Inc(FSpins);
  finally
    Unlock;
  end;
end;

procedure TNamedLock.ReleaseSpin;
begin
  Lock;
  try
    dec(FSpins);
    if FSPins < 0 then
      raise Exception.Create('Too many spin releases!');
  finally
    Unlock;
  end;


end;

function TNamedLock.TryGetSpin: boolean;
begin
  Lock;
  try
    result := FSpins = 0;
    if result then
      inc(FSpins);
  finally
    Unlock;
  end;

end;


{ TProcessorMonitor }

constructor TMonitor<T>.Create;
begin
  inherited;
  FList := TList<T>.create;
end;

destructor TMonitor<T>.Destroy;
begin
  FList.free;
  inherited;
end;

function TMonitor<T>.Getcount: nativeint;
begin
  lock;
  try
    result := FList.count;
  finally
    unlock;
  end;
end;

function TMonitor<T>.GetProcs(idx: nativeint): T;
begin
  lock;
  try
    result := FList[idx];
  finally
    unlock;
  end;
end;

procedure TMonitor<T>.Registerobject(proc: T);
begin
  lock;
  try
    FList.add(proc);
  finally
    unlock;
  end;
end;

procedure TMonitor<T>.UnRegisterobject(proc: T);
begin
  lock;
  try
    flist.remove(proc);
  finally
    unlock;
  end;
end;

{ TMonitorableObject }

constructor TMonitorableObject.Create;
begin
  inherited;
  FMonitoredBy := TList<TStandardMonitor>.create;
  icssc(statuslock, SHARED_SPIN);
end;

destructor TMonitorableObject.Destroy;
begin

  FMonitoredBy.free;
  dcs(statuslock);
  inherited;
end;

procedure TMonitorableObject.Detach;
begin
  inherited;
  UnregisterFromMonitors;

end;

function TMonitorableObject.GetStatus: string;
begin
  ecs(statuslock);
  try
    result := FStatus;
  finally
    lcs(statuslock);
  end;
end;

procedure TMonitorableObject.RegisterTo(mon: TStandardMonitor);
begin
  mon.registerobject(self);
  Lock;
  try
    FMonitoredBy.add(mon);
  finally
    unlock;
  end;
end;

procedure TMonitorableObject.SetStatus(const Value: string);
begin
  ecs(statuslock);
  try
    fStatus := value;
    fstatusHistory[FStatusIndex] := FStatus;
    FStatusIndex := (FStatusIndex + 1) mod (length(FStatusHistory));
  finally
    lcs(statuslock);
  end;
end;

procedure TMonitorableObject.UnregisterFromMonitors;
var
  iCount: ni;
  mon: TStandardMonitor;
begin
  repeat
    lock;
    try
      iCount := FMonitoredBy.Count;
      mon := nil;
      if iCount > 0 then
        mon := FMonitoredBy[0];

      if mon <> nil then begin
        FMonitoredBy.Remove(mon);

      end;
    finally
      unlock;
    end;

    if mon <> nil then
      mon.UnRegisterObject(self);
  until mon = nil;


end;

{ TNamedRWLocks }

constructor TNamedRWLocks.Create;
begin
  inherited;
  FLocks := TList<TNamedRWLock>.create;
end;

destructor TNamedRWLocks.Destroy;
begin
  FLocks.free;
  inherited;
end;

function TNamedRWLocks.FindLock(sName: string): TNamedRWLock;
var
  i: integer;
begin
  result := nil;
  Lock;
  try
    result := nil;
    i := IndexOfLock(sName);
    if i >= 0 then
      result := FLocks[i];
  finally
    Unlock;
  end;
end;

procedure TNamedRWLocks.GetLock(sName: string; bForWrite: boolean);
begin
  while not TryGetLock(sName, bForWrite) do
    sleep(1);
end;

function TNamedRWLocks.IndexOfLock(sName: string): integer;
var
  t: integer;
begin
  Lock;
  try
    sName := lowercase(sName);
    result := -1;
    for t:= 0 to FLocks.count-1 do begin
      if lowercase(FLocks[t].name) = sName then begin
        result := t;
        exit;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TNamedRWLocks.Init;
begin
  inherited;

end;

procedure TNamedRWLocks.ReleaseLock(sName: string; bForWrite: boolean);
var
  l: TNamedRWLock;
begin
  lock;
  try
    l := FindLock(sName);
    if l = nil then
      raise ELockNotFound.create('Lock not found '+sName);


    if bForWrite then
      l.unlockwrite
    else
      l.unlockread;

    if l.Lockrefs = 0 then begin
      FLocks.remove(l);
      l.free;
    end;


  finally
    unlock;
  end;
end;

function TNamedRWLocks.TryGetLock(sName: string; bForWrite: boolean): boolean;
var
  l: TNamedRWLock;
begin
  lock;
  try
    l := FindLock(sName);
    if l = nil then begin
      l := TNamedRWLock.create;
      l.Name := lowercase(sNAme);
      FLocks.add(l);
    end;

    if bForWrite then
      result := l.TryLockWrite
    else
      result := l.TryLockRead;

  finally
    unlock;
  end;
end;

{ TNamedRWLock }


function TNamedRWLock.LockRefs: ni;
begin
  result := self.LockQueueLength;
end;



{ TLifeLock }

destructor TLifeLock.Destroy;
begin
  if assigned(o) then
    o.unlock;
  inherited;
end;


constructor TSharedObject.Create;
begin
  ics(sect);
  inherited Create;
  //Init;

end;



destructor TSharedObject.Destroy;
begin

  inherited;
  dcs(sect);

end;

procedure TSharedObject.Detach;
begin
  //no implementation required
  inherited;
end;

procedure TSharedObject.Init;
begin
  //no implementation required

end;




{ TGiverFactory<T> }

function TGiverOf<T>.GiveEx(sContextKey: string): TObject;
var
  i: ni;
begin
  result := nil;
  Lock;
  try
    if FList.count > 0 then
    repeat
      i := Flist.IndexOf(sContextKey);
      if i < 0 then
        break;

      result := FList.objects[i];
      FList.delete(i);
      if not TBetterobject(result).shouldGive then begin
        Debug.Log('FREE from Pool (expired/should not give) '+result.classname);
        result.Free;
        result := nil;
      end else begin
        Debug.Log('Give from Pool '+result.classname);
        Debug.Log('there are now '+Flist.count.ToString);
      end;
    until (result <> nil) or (Flist.count = 0);


  finally
    Unlock;
  end;
  if result = nil then begin
    result := T.create;
    Debug.Log('NEW '+result.classname);
    T(result).FReturnto:= self;
  end;

end;

function TGiverOf<T>.Need(sContext: string): IHolder<T>;
var
  res: T;
begin
  result := nil;
  Lock;
  try
    res := T(GiveEx(sContext));
    if res <> nil then begin
      result := THolder<T>.create;
      result.o := res;
    end;
  finally
    Unlock;
  end;

end;

{ TAutoScope }

procedure TAutoScope.DestroyObjectsInScope;
var
  sFreeing: string;
begin
  inherited;
  for var t:= high(tracking) downto 0 do begin
    try
      sFreeing := tracking[t].classname;
      tracking[t].Free;
    except
      on E:Exception do begin
        Debug.Log('Unable to free '+sFreeing+' from TAutoScope.  Exceptions raised during destruction may yield unexpected behavior by rule.  '+'Destruction of other objects in the the scope WILL continue, however the object in question may have leaked.  Try ensuring no exceptions are raised during destruction.');
      end;
    end;
  end;

  setlength(tracking,0);
end;

procedure TAutoScope.DoDangerousDestruction;
begin
  DestroyObjectsInScope;
end;

procedure TAutoScope.Track(o: TObject);
begin
  setlength(tracking, length(tracking)+1);
  tracking[high(tracking)] := o;
end;

{ TFatMessageQueue }

procedure TFatMessageQueue.Broadcast(m: IHolder<TFatMessage>);
begin
  ecs(sectSubQueues);
  try
    //recursively broadcast a copy to subqueues (ultimately posts)
    for var t := 0 to subqueues.Count-1 do begin
      subqueues[t].Broadcast(m.o.copy);
    end;
  finally
    lcs(sectSubQueues);
  end;


  //post original (which might be a copy) to self
  Post(m);

end;

constructor TFatMessageQueue.Create;
begin
  inherited;
  ics(sectSubQueues);
  subqueues := TList<TFatMessageQueue>.create;

end;

procedure TFatMessageQueue.DeleteSubQueue(fmq: TFatMessageQueue);
begin
  var l := self.locki;
  subqueues.remove(fmq);
  fmq.free;
  fmq := nil;
end;

procedure TFatMessageQueue.Detach;
begin
  if detached then exit;

  subqueues.free;
  subqueues := nil;
  dcs(sectSubQueues);
  inherited;

end;

function TFatMessageQueue.GetNextMessage: IHolder<TFatMessage>;
begin
  var l := Locki;
  result := nil;
  if length(pending) = 0 then
    exit;

  result := pending[0];
  for var t := 1 to high(pending) do
    pending[t-1] := pending[t];

  setlength(pending, length(pending)-1);
end;

function TFatMessageQueue.NewMessage: IHolder<TFatMessage>;
begin
  result := THolder<TFatMessage>.create;
  result.o := TFatMessage.create;
end;

function TFatMessageQueue.NewSubQueue: TFatMessageQueue;
begin
  var l := self.locki;
  result := TFatMessageQueue.create;
  subqueues.add(result);

end;

procedure TFatMessageQueue.Post(m: IHolder<TFatMessage>);
begin
  var l := Locki;

  //queue must have an onposted handler in order to
  //accept posted messages, this is to prevent memory leaks due to
  //unprocessed message buildups
  //onposted simply notifies something (via mechanism of your choice) when
  //a message is available, the case of a queue for a form, it turns on a
  //timer
  //... could be a signal... etc...
  //queues that do not have this handler can still process synchronous messages
  //via send()
  if assigned(onposted) then begin
    setlength(pending, length(pending)+1);
    pending[high(pending)] := m;
    Posted;
  end;

end;

procedure TFatMessageQueue.Posted;
begin
  if Assigned(onposted) then begin
    onposted();
  end;
end;

function TFatMessageQueue.ProcessNextMessage: boolean;
begin
  result := false;
  var m := GetNextMessage;
  if m = nil then
    exit(false);//we didn't handle anything

  Send(m);

end;

procedure TFatMessageQueue.QuickBroadcast(messageClass: string;
  params: TArray<String>);
begin
  var m := MMQ.NewMessage;
  m.o.messageClass := messageClass;
  m.o.params := params;
  mmq.Broadcast(m);

end;

procedure TFatMessageQueue.QuickBroadcast(messageClass: string);
begin
  var m := MMQ.NewMessage;
  m.o.messageClass := messageClass;
  mmq.Broadcast(m);



end;

function TFatMessageQueue.Send(m: IHolder<TFatMessage>): boolean;
begin
  result := false;
  if assigned(handler) then begin
    result := handler(m);
  end;

  if not result then begin
    for var t := 0 to subqueues.count-1 do begin
      result := subqueues[t].Send(m) or result;
    end;
  end;

end;

{ TFatMessage }

function TFatMessage.Copy: IHolder<TFatMessage>;
begin
  result := THolder<TFatMessage>.create;
  result.o := TfatMessage.create;
  result.o.messageClass := self.messageClass;
  result.o.params := self.params;

end;

initialization
orderlyinit.init.RegisterProcs('betterobject', oinit, ofinal, 'betterobjectregistry');

finalization

end.

