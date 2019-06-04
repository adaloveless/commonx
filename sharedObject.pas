unit SharedObject;

{$INCLUDE DelphiDefs.inc}
//The classes contained in sharedobject.pas provide solid, foundations upon which
//to build a thread-safe classes.  See each class for more info.
{$D+}
interface
{$IFNDEF OLD_UNIT}
uses
  betterobject;
{$ELSE}
{$DEFINE FREEDICT}
{x$DEFINE THREAD_BLOCKING_DIAGNOSTICS}
{x$DEFINE LOCK_DEBUG}
{x$DEFINE CONTEXT_SWITCH}
{x$DEFINE DEAD_LOCK_DEBUG}
{x$DEFINE ALLOW_WATCHME}

uses
  {$IFNDEF FPC}
  system.rtlconsts,//inline
  system.types,//inline
  {$ENDIF}

  classes, sysutils,
  {$IFDEF WINDOWS}
    {$IFNDEF FPC}
      winapi.windows,
    {$ELSE}
      windows,
    {$ENDIF}
  {$ENDIF}
  typex, systemx, betterobject, generics.collections.fixed;

const
  SHARED_SPIN = 0;
type
  ELockNotFound = class(Exception);
  TSharedObject = class;//forward

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
  end;

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





const
  WRITE_LOCK = 2;
  READ_LOCK = 1;


{$ENDIF}

var
  MasterMonitor: TStandardMonitor;
  nodebug: boolean;





implementation
{$IFNDEF OLD_UNIT}
uses orderlyinit;
{$ELSE}
uses Debug, tickcount, orderlyinit;

{ TSharedObject }

constructor TSharedObject.Create;
begin
  InitializeCriticalSection(sect);
  inherited Create;
  //Init;

end;

destructor TSharedObject.Destroy;
begin

  inherited;
  DeleteCriticalSection(sect);

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

{$IFDEF MSWINDOWS}
function TSharedObject.IsLocked: boolean;
begin
  result := self.sect.LockCount > 0;
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
      Debug.Log(self, self.ClassName+' is blocked on thread '+inttostr(GetCurrentThreadID())+' by '+inttostr(sect.owningthread),'error');
      EnterCriticalSection(sect);
      Debug.Log(self, self.ClassName+' is unblocked on thread '+inttostr(GetCurrentThreadID()),'error');
    end;
    {$ELSE}
    EnterCriticalSection(sect);
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
  result := sect.OwningThread;
end;
{$ENDIF}

procedure TSharedObject.SetSpinCount(const Value: ni);
begin
  FSpinCount := Value;
{$IFDEF MSWINDOWS}
  SetCriticalSectionSpinCount(sect, value);
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
  result := TryEnterCriticalSection(sect);
  if result then
    lockrefs := lockrefs+1;

end;

function TSharedObject.TryLock(iTimeoutMS: integer): boolean;
var
  tm1, tm2: cardinal;
begin
  tm1 := GetTicker;
  result := true;
  while not TryEnterCriticalSection(sect) do begin
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
  LeaveCriticalSection(sect);

  {$IFDEF LOCK_DEBUG}
  if not nodebug then
  Debug.Log(self.classname+' released lock @'+inttohex(integer(@sect),8));
  {$ENDIF}
end;

{ TLockQueuedObject }

constructor TLockQueuedObject.Create;
begin
  inherited;
  InitializeCriticalSection(sect);
  FLockQueues := TList<TLockREcord>.create;

end;

destructor TLockQueuedObject.Destroy;
begin
  FLockQueues.free;
  DeleteCriticalSection(sect);
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
  EnterCriticalSection(sect);
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
  LeaveCriticalSection(sect);
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


{$ENDIF}


procedure oinit;
begin
  nodebug := true;
  MasterMonitor := TstandardMonitor.create;
end;

procedure ofinal;
begin
//  MasterMonitor.free;
//  MasterMonitor := nil;
end;


initialization
  init.registerprocs('SharedObject', oinit, ofinal);

finalization


end.
