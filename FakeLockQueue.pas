unit FakeLockQueue;

interface

{$DEFINE FAKE_QUEUE}



uses
  typex, debug,  systemx, Signals, sharedobject, generics.collections.fixed, betterobject;

type
  TFakeLockQueue = class;//forward;

  TLock = class(TBetterSignal)
  public
    threadid: ni;
    refcount: ni;
    Q: TFakeLockQueue;
    procedure Init;override;
  end;

  TFakeLockQueue = class(TBetterObject)
  private
    sect: TCLXCriticalSection;
    FLocks: TList<TLock>;
    procedure SignalNextLock;
    function QueueLock: TLock;
    procedure Lock;inline;
    procedure Unlock;inline;
    function FindLock(threadid: ni): TLock;
  public
    procedure Init;override;
    destructor Destroy;override;
    function TryGetLock: TLock;overload;
    function TryGetLock(out l: TLock): boolean;overload;
    function GetLock: TLock;
    procedure UnlockLock(lck: TLock);
  end;

  TFakeLockQueueObject = class(TFakeLockQueue)
  public
  end;

implementation

{ TFakeLockQueue }

destructor TFakeLockQueue.Destroy;
begin
  FLocks.Free;
  FLocks := nil;
  inherited;
  DCS(sect);
end;

function TFakeLockQueue.FindLock(threadid: ni): TLock;
var
  t: ni;
  l: TLock;
begin
  result := nil;
  Lock;
  try

    for t:= 0 to FLocks.count-1 do begin
      l := FLocks[t];
      if l.threadid = threadid then begin
        result := l;
        break;
      end;
    end;
  finally
    unlock;
  end;
end;

function TFakeLockQueue.GetLock: TLock;
begin
{$IFDEF FAKE_QUEUE}
  Lock;
{$ENDIF}
//  Debug.Log(self,'Want Lock');
  result := findlock(getcurrentthreadid);
  if result = nil then begin
    result := QueueLock;

//    Debug.Log(self,'Queued Lock');
  end else begin
//    Debug.log(self,'Found Lock');
  end;

  inc(result.refcount);
  result.WaitFor(-1);
//  Debug.Log(self,'Got Lock');

end;

procedure TFakeLockQueue.Init;
begin
  ICS(sect);
  inherited;
  fLocks := TList<TLock>.create;

end;

procedure TFakeLockQueue.Lock;
begin
  ECS(sect);

end;

function TFakeLockQueue.QueueLock: TLock;
begin
  result := TLock.Create;
  Lock;
  try
    Flocks.Add(result);
    if FLocks.Count = 1 then begin
      result.Signal(true);
    end;
    result.q := self;
    result.threadid := getcurrentthreadid;
  finally
    Unlock;
  end;
end;

procedure TFakeLockQueue.SignalNextLock;
begin
  Lock;
  try
    if FLocks.Count = 0 then
      exit;


//    Debug.Log(self, 'signalling...');
    FLocks[0].Signal(true);
  finally
    Unlock;
  end;

end;



function TFakeLockQueue.TryGetLock(out l: TLock): boolean;
begin
  l := TryGetLock;
  result := l <> nil;

end;

function TFakeLockQueue.TryGetLock: TLock;
var
  b: boolean;
begin
  result := nil;
{$IFDEF FAKE_QUEUE}
  if TECS(sect) then begin
{$ENDIF}
  Lock;
  try
    b := FLocks.count = 0;
    if b then begin
      result :=QueueLock;
      result.Threadid := getcurrentthreadid;
      result.q := self;
      inc(result.refcount);
    end
    else begin
      b := FLocks[0].threadid = int64(getcurrentthreadid);
      if b then begin
        result := FLocks[0];
        inc(result.refcount);
      end else
        result := nil;
    end;
  finally
    Unlock;
  end;
{$IFDEF FAKE_QUEUE}
  end;
{$ENDIF}

end;

procedure TFakeLockQueue.Unlock;
begin
  LCS(sect);
end;

procedure TFakeLockQueue.UnlockLock(lck: TLock);
begin

  Lock;
  try
    dec(lck.refcount);
    if lck.refcount = 0 then begin
      FLocks.Remove(lck);
    end;
  finally
    Unlock;
  end;
  SignalNextLock;
  if lck.refcount = 0 then begin
    lck.Q := nil;
    lck.Free;
    lck := nil;
  end;


{$IFDEF FAKE_QUEUE}
  UnLock;
{$ENDIF}

end;


{ TLock }

procedure TLock.Init;
begin
  inherited;
  threadid := -1;
end;

end.
