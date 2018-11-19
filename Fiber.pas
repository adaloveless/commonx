unit Fiber;

interface

uses
  signals,managedthread, typex, simplequeue, sharedobject, better_collectIons, tickcount, consolelock, orderlyinit, systemx;

type
  TFiberWeaver = class;//forward

  TFiber = class(TSharedObject)
  private
    FColdRunInterval: ticker;
    FHasWork: boolean;
    FRunHot: boolean;
    FLastExecuteTime: ticker;
    FSleepTime: ticker;
    FSlept: boolean;
    function GetDeadline: ticker;
  protected
    evFinished: TSignal;
    procedure DoExecute;virtual;

  public
    weaver: TFiberWeaver;
    queueitem: TqueueItem;
    stopping: boolean;
    stopped: boolean;
    IsExecuting: boolean;
    procedure Init;override;
    destructor Destroy;override;
    procedure Execute;

    property RunHot: boolean read FRunHot write FRunHot;
    property ColdRunInterval: ticker read FColdRunInterval write FColdRunInterval;
    property HasWork: boolean read FHasWork write FHasWork;
    property DeadLine: ticker read GetDeadLIne;
    property LastExecuteTime: ticker read FLastExecuteTime;
    procedure ExecuteAsQueueItem(q: TSimpleQueue);
    procedure Start;
    procedure Stop;
    function TrywaitFor: boolean;
    procedure WaitFor;
    procedure WaitForFinish;inline;
    procedure SleepFor(ms: ticker);
    property SleepTime: ticker read FSleeptime;
    property Slept: boolean read FSlept write FSlept;
    property StopRequested: boolean read stopping;
    property Terminated: boolean read stopping;
  end;

  TFiberExecuteQueueItem = class(TQueueItem)
  protected
    procedure DoExecute; override;
  public
    fib: TFiber;
  end;

  TFiberList = class(TBetterList<TFiber>)
  public

  end;


  TFiberWeaver = class(TManagedThread)
  protected
    Flist: TFiberList;
    FMQ: TMultiQueue;
    FCheckDestroy: TBetterList<TFiberExecuteQueueItem>;
    procedure WaitForAll;
    procedure CheckDestroy;

  public

    procedure Init;override;
    procedure Detach;override;
    procedure OnFInish;override;
    procedure InitFromPool;override;


    function DoWork: nativeint;
    function GetNextWorkItem: TFiber;

    procedure DoExecute;override;
    procedure AddFiber(fib: TFiber);
    procedure RemoveFiber(fib: TFiber);


  end;


  TFiberManager = class(TFiberWeaver)
  public
    function NeedFiber<T: TFiber>(owner: TObject = nil): T;overload;
    procedure NoNeedFiber(fib: TFiber);
    procedure Init; override;
    procedure Detach; override;
  end;



function FIB: TFiberMAnager; inline;
function GEtFiberMAnager: TFiberMAnager;


implementation


var
  FFIB: TFiberMAnager;



function GEtFiberMAnager: TFiberMAnager;
begin
  lockconsole;
  try
    result := FFIB;
    if result = nil then begin
      FFIB := TPM.Needthread<TFiberMAnager>(nil);
      FFIB.start;
      result := FFIB;
    end;
  finally
    unlockconsole;
  end;
end;

function FIB: TFiberManager;
begin
  result := GetFiberManager;
end;


procedure TFiberManager.Detach;
begin

  inherited;

end;

procedure TFiberManager.Init;
begin
  inherited;





end;

function TFiberManager.NeedFiber<T>(owner: TObject = nil): T;
begin
  RESULT := T.create;
  result.Weaver := self;

end;

procedure TFiberManager.NoNeedFiber(fib: TFiber);
begin
  if not fib.stopped then begin
    fib.Stop;
    fib.waitfor;
  end;
  fib.free;

end;



destructor TFiber.Destroy;
begin
  evFinished.free;
  evFinished := nil;
  inherited;
end;

procedure TFiber.DoExecute;
begin
  //no implementation required
end;

procedure TFiber.Execute;
begin
  DoExecute;

end;

procedure TFiber.ExecuteAsQueueItem(q: TSimpleQueue);
var
  fqi: TFiberExecuteQueueItem;
begin
  lock;
  try
    if stopping then begin
      stopped := true;
      signal(evFinished);
      FLastExecuteTime := GetTicker;
      exit;
    end;
    FLastExecuteTime := GetTicker;

    fqi := TFiberExecuteQueueItem.create;
    self.queueitem := fqi;
    self.IsExecuting := true;
    fqi.fib := self;
    fqi.AutoDestroy := true;
    fqi.Queue := q;
    q.AddItem(fqi);
    FsleepTime := 0;
  finally
    unlock;
  end;

end;

function TFiber.GetDeadline: ticker;
begin
  if RunHot then
    result := FLastExecuteTime+FSleepTime
  else
    result := FLAstExecuteTime + FColdRunInterval+FSleepTime;
end;

procedure TFiber.Init;
begin
  inherited;
  FColdRunInterval := 250;
  evFinished := TSignal.create;
end;

procedure TFiber.SleepFor(ms: ticker);
begin
  FSleepTime := ms;
  FSlept := true;
//  raise ECritical.create('sleep is not supported for fibers');
end;


procedure TFiber.Start;
begin
  stopping := false;
  weaver.AddFiber(self);
end;

procedure TFiber.Stop;
begin
  Lock;
  try
    stopping := true;
    if assigned(self.queueitem) and assigned(self.queueitem.queue) and
     (self.queueitem.queue.threadid <> getcurrentthreadid) then
      WaitFor
    else
      IsExecuting := false;
  finally
    Unlock;
  end;

  weaver.RemoveFiber(self);



end;

function TFiber.TrywaitFor: boolean;
begin
  result := queueitem = nil;
end;

procedure TFiber.WaitFor;
var
  qi: TQueueItem;
begin
  waitforSignal(evFinished);


  if assigned(self.queueitem) and assigned(self.queueitem.queue) and
   (self.queueitem.queue.threadid <> getcurrentthreadid) then
    WaitFor
  else
    IsExecuting := false;


  while IsExecuting do systemx.sleep(1);
//  if queueitem.Queue.ThreadID = GetcurrentThreadID then
//    exit;
//  while not TryWaitFor do systemx.sleep(1);
end;

procedure TFiber.WaitForFinish;
begin
  WaitFor;
end;

{ TFiberWeaver }

procedure TFiberWeaver.AddFiber(fib: TFiber);
begin
  Lock;
  try
    FList.add(fib);
  finally
    unlock;
  end;
end;

procedure TFiberWeaver.CheckDestroy;
begin
  if FCheckDestroy.count = 0 then exit;

  if FCheckDestroy[0].IsComplete then
    FCheckDestroy.free;

end;

procedure TFiberWeaver.Detach;
begin
  if detached then
    exit;
  WaitForAll;

  if FList = nil then
    exit;
  FList.free;
  FList := nil;
  FMQ.free;
  FMQ := nil;

  FCheckDestroy.free;
  FCheckDestroy := nil;


  inherited;

end;

procedure TFiberWeaver.DoExecute;
begin
  inherited;
  RunHot := true;
  DoWork;
end;

function TFiberWeaver.DoWork: nativeint;
var
  t: ni;
  fib: TFiber;
  q: TSimpleQueue;
  tmNow, tmDif: ticker;
begin
  result := 0;
  fib := nil;
  lock;
  try
    q := FMQ.WaitForEmptyQUeue;
    if q <> nil then begin
      fib := GetNExtWorkItem;
      if fib <> nil then begin
        RunHot := true;
        tmNow := GEtTicker;
        tmDif := fib.deadline - tmNow;
        if (tmDif > 0) and (tmDif < 10000)  then
          sleep(tmDif);

        if (tmDif < 10000) then begin
          fib.ExecuteAsQueueItem(q);
          CheckDestroy;
        end;
      end else begin
        ColdRunInterval := 15;
        RunHot := false;
      end;

    end;
  finally
    unlock;
  end;

end;

function TFiberWeaver.GetNextWorkItem: TFiber;
var
  t: ni;
  min: ticker;
  fib: TFiber;
begin

  if FList.count < 1 then
    exit(nil);

  min := FList[0].DeadLine;
  result := nil;

  for t:= 0 to FList.count-1 do begin
    fib := FList[t];
    if (fib.DeadLine <= min)
    and (fib.stopping = false)
    and (fib.queueitem = nil)
    and (not fib.stopped) then begin
      result := fib;
      min := fib.deadline;
    end;
  end;

end;

procedure TFiberWeaver.Init;
begin
  inherited;


  FLIst := TFiberList.create;

  FCheckDestroy := TBetterList<TFiberExecuteQueueItem>.create;


end;

procedure TFiberWeaver.InitFromPool;
begin
  inherited;
  Loop := true;
  RunHot := true;
  ColdRunInterval := 15;

  if FMQ = nil then begin
    FMQ := TMultiQueue.create;
    FMQ.Start;
  end;
end;

procedure TFiberWeaver.OnFInish;
begin
  inherited;
  Detach;
end;

procedure TFiberWeaver.RemoveFiber(fib: TFiber);
begin
  lock;
  try
    if fib.Isexecuting then
      raise ECritical.create('cannot remove a fiber that is executing');
    FList.Remove(fib);
  finally
    unlock;
  end;
end;

procedure TFiberWeaver.WaitForAll;
begin
  while FCheckDestroy.count > 0 do begin
    CheckDestroy;
  end;
end;

{ TFiberExecuteQueueItem }

procedure TFiberExecuteQueueItem.DoExecute;
begin
  inherited;
  if fib = nil then
    raise ECritical.create('fib is nil');
  fib.Execute;
  fib.queueitem := nil;
  fib.IsExecuting := false;


END;


procedure oinit;
begin
  LockConsole;
  try
    FFib := nil;
  finally
    UnlockConsole;
  end;
end;

procedure ofinal;
begin

  LockConsole;
  try
    if FFIB <> nil then begin
      FFIB.WaitForFinish;
      TPM.NoNeedthread(FFIB);
      FFIB := nil;
    end;
  finally
    unlockconsole;
  end;
end;

procedure prefinal;
begin
  LockConsole;
  try
    if FFIB <> nil then begin
      FFIB.Stop;
    end;
  finally
    unlockconsole;
  end;
end;

procedure latefinal;
begin
  LockConsole;
  try
    if FFIB <> nil then begin
      FFIB.WAitFor;
      TPM.NoNeedthread(FFIB);
      FFIB := nil;
    end;
  finally
    unlockconsole;
  end;
end;




initialization

init.RegisterProcs('Fiber', oinit, prefinal, ofinal, latefinal, 'ConsoleLock');


end.

