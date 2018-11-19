unit SimpleQueue;
//Prove:
//[ ] Things are added to the queue, queue marked hot.
//[ ] Queue runs
//[ ] Queue empties incoming
//[ ] queue adds to working
//[ ] queue moves to complete (unless autodestroy)
//[ ] Queue items are completed

{$I DELPHIDEFS.inc}
{x$DEFINE EXECUTE_ON_ADD}

interface

uses
{$IFDEF WINDOWS}
  {$IFDEF FPC}
    windows,
  {$ELSE}
    winapi.windows,//inline
  {$ENDIF}
{$ENDIF}
  classes, sysutils, debug,
  typex, systemx, better_collections, signals, betterobject, sharedobject, managedthread, tickcount;


type
  TAbstractSimpleQueue = class;//forward

  TQueueItem = class(TSharedObject)
  strict private
    FWaitSignal: TSignal;
    function GEtWait: boolean;inline;
  strict
  private
    FQueue: TAbstractSimpleQueue;
    FAutoDestroy: boolean; protected
    procedure DoExecute;virtual;
  public
    tmUSerMarker: ticker;
    procedure Init;override;
    procedure Detach;override;
    Destructor Destroy;override;
    property Wait: boolean read GEtWait;
    function WAitFor(iTimeout: ni = -1): boolean;
    procedure Execute;inline;
    procedure SignalComplete;inline;
    property AutoDestroy: boolean read FAutoDestroy write FAutoDestroy;
    function IsComplete: boolean;inline;
    property Queue: TAbstractSimpleQueue read FQueue write FQueue;

  end;

  TAbstractSimpleQueue = class(TManagedThread)
  private
    FExecuteLock: TCLXCriticalSection;
    FMaxItemsInQueue: ni;
    FBlockIncoming: boolean;
    procedure ProcessItem;virtual;abstract;

    procedure BlockIncoming;
    procedure UnBlockIncoming;
    function CollectAdded(bForce: boolean): boolean;
    function CollectIncoming(bForce: boolean): boolean;
    procedure AddItem2(const itm: TQueueItem);
  protected
    FIterations: nativeint;
    Faddeditems, FIncomingItems, FWorkingItems, FCompletedItems: TSharedList<TQueueItem>;
    FTmpList: Tbetterlist<TQueueItem>;
    last_process_time: ni;
    function GetNextItem: TQueueItem;virtual;
  public
    estimated_backlog_size: nativeint;
    estimated_queue_size: nativeint;
    procedure Init;override;
    procedure SetStartingState;override;
    procedure Detach;override;
    procedure AfterProcessItem;virtual;
    procedure AddItem(const itm: TQueueItem);inline;
    procedure RemoveCompletedItem(const itm: TQueueItem);inline;
    procedure DoExecute;override;
    procedure ExecuteLock;inline;
    procedure ExecuteUnlock;inline;
    function TryExecuteLock: boolean;inline;
    procedure ProcessAllSynchronously;
    procedure WaitForAll;
    property MaxItemsInQueue: ni read FMaxItemsInQueue write FMaxItemsInQueue;
    procedure OptimizeIncoming(incomingitem: TQueueItem; idx: ni);virtual;

  end;

  TFakeCommand = class(TQueueItem)
  private
    FJunkFloat: nativefloat;
    function GEtStatus: string;
    procedure SetStatus(const Value: string);
  protected


    procedure InitExpense;virtual;
    procedure Start(queue: TAbstractSimpleQueue);
    property cpuexpense: nativefloat read FJunkFloat write FJunkFloat;
    property Status: string read GEtStatus write SetStatus;

  end;

  TSimpleQueue = class(TAbstractSimpleQueue)
  protected
    procedure ProcessItem;override;

  end;

  TMultiQueue = class(TSharedObject)
  protected
    FList: TBetterList<TSimpleQueue>;
    function GetShortestQueue: TSimpleQueue;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure AddItem(const itm: TQueueItem);inline;

  end;


implementation

{ TAbstractSimpleQueue }


{ TAbstractSimpleQueue<T_ITEM> }

procedure TAbstractSimpleQueue.AddItem(const itm: TQueueItem);
var
  idx: ni;
begin
  itm.Queue := self;
  FAddedItems.Lock;
  try
    idx := FAddedItems.add(itm);
    estimated_backlog_size := FIncomingItems.volatilecount+faddeditems.volatilecount;

  //  Debug.ConsoleLog('Queued item #'+inttostr(FIncomingitems.count));
  finally
    FAddedItems.Unlock;
  end;
{$IFDEF EXECUTE_ON_ADD}
  ProcessItem;
{$ENDIF}
  HasWork := true;
end;
procedure TAbstractSimpleQueue.AddItem2(const itm: TQueueItem);
var
  idx: ni;
begin
  itm.Queue := self;
  FIncomingItems.Lock;
  try
    idx := FIncomingItems.add(itm);
    estimated_backlog_size := FIncomingItems.count;

  //  Debug.ConsoleLog('Queued item #'+inttostr(FIncomingitems.count));
    OptimizeIncoming(itm, idx);
  finally
    FIncomingItems.Unlock;
  end;
{$IFDEF EXECUTE_ON_ADD}
  ProcessItem;
{$ENDIF}
  HasWork := true;


end;

procedure TAbstractSimpleQueue.AfterProcessItem;
begin
  //
end;

procedure TAbstractSimpleQueue.BlockIncoming;
begin
  if not FBlockIncoming then begin
    FIncomingItems.lock;
    FBlockIncoming := true;
  end;
end;

function TAbstractSimpleQueue.CollectAdded(bForce: boolean): boolean;
var
  i:ni;
begin
  if bForce then begin
    Faddeditems.Lock;
    try

      Ftmplist.clear;

      for i := 0 to FAddeditems.count-1 do begin
        fTMPLIST.ADD(FAddeditems[i]);
      end;
      Faddeditems.Clear;
      //Ftmplist.Clear;
    finally
      Faddeditems.Unlock;
    end;
    FincomingItems.Lock;
    try
      for i := 0 to Ftmplist.count-1 do begin
        additem2(Ftmplist[i]);
      end;
      FTmpList.clear;

      estimated_backlog_size := 0;
      estimated_queue_size := FWorkingItems.volatilecount+faddeditems.volatilecount;
    finally
      FIncomingItems.Unlock;
    end;
    result := estimated_queue_size > 0;
  end else begin
{x$DEFINE HOLD_IN_INCOMING_LONGER2}
{$IFDEF HOLD_IN_INCOMING_LONGER2}
    result := false;
{$ELSE}
    Faddeditems.Lock;
    try

      Ftmplist.clear;

      for i := 0 to FAddeditems.count-1 do begin
        fTMPLIST.ADD(FAddeditems[i]);
      end;
      Faddeditems.Clear;

    finally
      Faddeditems.Unlock;
    end;
    FincomingItems.Lock;
    try
      for i := 0 to Ftmplist.count-1 do begin
        additem2(Ftmplist[i]);
      end;
      Ftmplist.Clear;

      estimated_backlog_size := 0;
      estimated_queue_size := FWorkingItems.volatilecount+faddeditems.volatilecount;
    finally
      FIncomingItems.Unlock;
    end;
    result := estimated_queue_size > 0;
{$ENDIF}
  end;

end;

function TAbstractSimpleQueue.CollectIncoming(bForce: boolean): boolean;
begin
  if bForce then begin
    FIncomingItems.Lock;
    try
      FWorkingItems.AddList(FIncomingItems);
      FincomingItems.Clear;
      estimated_backlog_size := 0;
      estimated_queue_size := FWorkingItems.volatilecount+FAddedItems.volatilecount+Fincomingitems.volatilecount;

    finally
      FIncomingItems.Unlock;
    end;
    result := FWorkingItems.count > 0;
  end else begin
{$DEFINE HOLD_IN_INCOMING_LONGER}
{$IFDEF HOLD_IN_INCOMING_LONGER}
    result := false;
{$ELSE}
    result := FIncomingItems.TryLock;
    if result then begin
      FWorkingItems.AddList(FIncomingItems);
      FincomingItems.Clear;
      FIncomingItems.Unlock;
      result := FWorkingItems.Count>0;
    end;
{$ENDIF}
  end;

end;

procedure TAbstractSimpleQueue.Detach;
begin
  inherited;
  if FCompletedItems.count > 0 then
    raise ECritical.create('It is bad to destroy TAbstractSimpleQueue while there are items waiting in the CompletedItems list.');

  FCompletedItems.free;
  FWorkingItems.free;
  FIncomingItems.free;
  Faddeditems.free;
  FTmpList.free;
  DCS(FExecuteLock);


end;

procedure TAbstractSimpleQueue.DoExecute;
var
  tm1, tm2: ticker;
begin
  inherited;
  tm1 := GetTicker;
  ProcessItem;
  tm2 := GetTicker;
  last_process_time := GetTImeSince(tm2,tm1);
  inc(FIterations);

end;

procedure TAbstractSimpleQueue.ExecuteUnlock;
begin
  LCS(FExecuteLock);
end;

function TAbstractSimpleQueue.GetNextItem: TQueueItem;
begin
  result := FWorkingItems[0];//default, override me if you want to implement custom ordering

end;

procedure TAbstractSimpleQueue.ExecuteLock;
begin
  ECS(FExecuteLock);
end;

procedure TAbstractSimpleQueue.Init;
begin
  inherited;
  FIncomingItems := TSharedList<TQueueItem>.create;
  FWorkingItems := TSharedList<TQueueItem>.create;
  FCompletedItems := TSharedList<TQueueItem>.create;
  FAddedItems := TSharedList<TQueueItem>.create;
  Ftmplist := TBetterList<TQueueItem>.create;
  ICS(FExecuteLock);
  Loop := true;

end;

procedure TAbstractSimpleQueue.OptimizeIncoming;
begin
  //
end;

procedure TAbstractSimpleQueue.ProcessAllSynchronously;
begin
  while (FIncomingItems.Count > 0) or (FWorkingItems.count > 0) do begin
    ProcessItem;
  end;
end;


procedure TSimpleQueue.ProcessItem;
var
  itm: TQueueItem;
begin

//  Debug.ConsoleLog('Process');
  if faddeditems.trylock then
  try
    CollectAdded(FWorkingItems.Volatilecount=0);
  finally
    faddeditems.unlock;
  end;
  if FIncomingItems.TryLock then
  try
    CollectIncoming(FWorkingItems.Volatilecount=0);
    HasWork := (FWorkingItems.count > 0) or (FIncomingItems.count > 0) or (Faddeditems.count>0);//NOTE! IMPORTANT!  This should be the ONLY place where HasWork is evaluated
    //if we set a limit on the number of items we can queue then prevent
    //other threads from getting a lock on the incoming list
    Status := inttostr(FWorkingItems.count)+' working items.';
    if (MaxItemsInQueue > 0) and (FWorkingItems.VolatileCount > MaxItemsInQueue) then begin
      BlockIncoming;
    end else begin
      UnBlockIncoming;
    end;
  finally
    FIncomingItems.Unlock;
  end;
  if (haswork) then begin
    itm := nil;

    estimated_queue_size := FWorkingitems.Count;

    if FWorkingItems.count > 0 then
      itm := GEtNextItem;

    //if (FIterations mod 100) = 0 then
//    if GetTimeSince(tmLastStatusUpdate) > 100 then
      Status := inttostr(FWorkingItems.count)+' working items.';


    if itm <> nil then
    try
      ExecuteLock;
      try
      //if (FIterations mod 100) = 0 then
        Status := inttostr(FWorkingItems.count)+' working items.';

        FWorkingItems.Remove(itm);


        //executing the item will signal it
//        status := itm.ClassName;
        itm.Execute;
        dec(estimated_queue_size);



      finally
        ExecuteUnlock;
      end;
    finally
      if itm.AutoDestroy then begin
        itm.free;
      end
      else begin
        FCOmpletedItems.lock;
        FCompletedItems.add(itm);
        FCompletedItems.Unlock;
        itm.SignalComplete;
      end;
    end;
  end;
end;

procedure TAbstractSimpleQueue.RemoveCompletedItem(const itm: TQueueItem);
begin
  FCompletedItems.Lock;
  FCompletedItems.Remove(itm);
  FCompletedItems.unlock;
end;

procedure TAbstractSimpleQueue.SetStartingState;
begin
  inherited;
{$IFDEF WINDOWS}
  priority := tphigher;
{$ENDIF}

end;

function TAbstractSimpleQueue.TryExecuteLock: boolean;
begin
  result := TECS(FExecuteLock);
end;

procedure TAbstractSimpleQueue.UnBlockIncoming;
begin
  if FBlockIncoming then begin
    FIncomingItems.unlock;
    FBlockIncoming := false;
  end;
end;

procedure TAbstractSimpleQueue.WaitForAll;
begin
  while HasWork do begin
    sleep(1);
  end;
end;

{ TQueueItem }

destructor TQueueItem.Destroy;
begin
  FWaitSignal.free;
  inherited;
end;

procedure TQueueItem.Detach;
begin
  FQueue.RemoveCompletedItem(self);

  inherited;
end;

procedure TQueueItem.DoExecute;
begin
  //
end;

procedure TQueueItem.Execute;
begin
  DoExecute;

end;

function TQueueItem.GEtWait: boolean;
begin
  result := IsSignaled(FWaitSignal);

end;

procedure TQueueItem.Init;
begin
  inherited;
  FWaitSignal := TSignal.create;
end;

function TQueueItem.IsComplete: boolean;
begin
  result := GEtWait;
end;

procedure TQueueItem.SignalComplete;
begin
  Signal(FWaitSignal, true);
end;

function TQueueItem.WAitFor(iTimeout: ni = -1): boolean;
begin
  result := WaitForSignal(FWaitSignal, iTimeout);
end;

{ TFakeCommand }

function TFakeCommand.GEtStatus: string;
begin
  //
end;

procedure TFakeCommand.InitExpense;
begin
  //this is a stub that is not used.  here to maintain interface compatability with TCommand
end;

procedure TFakeCommand.SetStatus(const Value: string);
begin
  //
end;

procedure TFakeCommand.Start(queue: TAbstractSimpleQueue);
begin
  queue.AddItem(self);
end;

{ TMultiQueue }

procedure TMultiQueue.AddItem(const itm: TQueueItem);
begin
  GetShortestQueue.AddItem(itm);
end;

constructor TMultiQueue.Create;
begin
  inherited;
  FList := TBetterList<TSimpleQueue>.create;
  while FList.Count < GetnumberofProcessors do begin
    FList.Add(TSimpleQueue.Create(nil, nil));
    FList[FList.count-1].Name := 'MQ'+inttostr(FList.count-1);
    FList[FList.count-1].Start;
  end;
end;

destructor TMultiQueue.Destroy;
var
  q: TSimpleQueue;

begin
  while FList.Count > 0 do begin
    q := FList[FList.count-1];
    q.WaitForAll;
    q.Stop;
    q.WaitForFinish;
    FList.delete(FList.count-1);
    q.Free;
    q := nil;
  end;

  inherited;
end;

function TMultiQueue.GetShortestQueue: TSimpleQueue;
var
  t: ni;
begin
  result := FList[0];
  for t:= 1 to FList.count-1 do begin
    if FList[t].estimated_queue_size < result.estimated_queue_size then begin
      result := FList[t];
    end;
  end;
end;

end.
