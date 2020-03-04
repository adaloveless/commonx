unit PeriodicEvents;
{x$DEFINE RDTPMODULE}

interface

uses
  debug, systemx, typex, lockqueue, tickcount, managedthread, generics.collections.fixed, orderlyinit, commandprocessor, classes, betterobject;

type
  TPeriodicEvent = class;//forward
  TPeriodicEventAggregator = class;//forward

  TPeriodicEventEvent = procedure (sender: TPeriodicEvent) of object;

  TPeriodicEvent = class(TLockQueue)
  strict
  private
    FSynchronized: boolean;
  protected
    FFrequency: ticker;
    FEnabled: boolean;
    FLastExecutionTime: ticker;
    FArmed: boolean;
    FOwner: TObject;
    FExecuting: boolean;
    function GetIsTime: boolean;
    function GetTimeuntilEvent: ticker;
    procedure Unregister;
    procedure DoExecute;virtual;
  protected
    FAggregator: TPeriodicEventAggregator;
  public
    Startimmediately: boolean;
    destructor Destroy;override;
    procedure Execute;
    function CanArm: boolean;virtual;
    function Finished: boolean;virtual;
    property Frequency: ticker read FFrequency write FFrequency;
    property LastExecutionTime: ticker read FLastExecutionTime write FLastExecutionTime;
    property Enabled: boolean read FEnabled write FEnabled;
    property IsTime: boolean read GetIsTime;
    property TimeUntilEvent: ticker read GetTimeuntilEvent;
    property Armed: boolean read FArmed write FArmed;
    property Executing: boolean read FExecuting;
    property Owner: TObject read FOwner write FOwner;
    property Synchronized: boolean read FSynchronized write FSynchronized;
  end;

  TPeriodicCommandEvent = class(TPeriodicEvent)
  protected
    procedure Destroycommand;
  public
    cmd: TCommand;
    function Finished: boolean;override;
    function CanArm: boolean;override;
    procedure Detach;override;
  end;

  TPeriodicEventAggregator = class(TManagedThread)
  protected
    FEvents: TList<TPeriodicEvent>;
    idx: ni;
  public
    procedure Add(event: TPeriodicEvent);
    procedure Remove(event: TPeriodicEvent);
    procedure Init;override;
    destructor Destroy;override;
    procedure DoExecute;override;
  end;

  TExternalPeriodicEvent = class(TPeriodicEvent)
  private
    FOnEvent: TPeriodicEventEvent;
  protected
    procedure DoExecute; override;
  public
    property OnEvent: TPeriodicEventEvent read FOnEvent write FOnEvent;
  end;


  TPeriodicSingleTon = class(TSharedObject)
  private
    FPE: TExternalPeriodicEvent;

    procedure OnPE(sender: TPeriodicEvent);
  protected
    procedure Periodically;
    procedure DoPeriodically;virtual;
    procedure SetupPE(interval: int64; bGoNow: boolean; bSynchronize: boolean);
    procedure CleanupPE;
  public
    constructor Create;override;
    procedure Detach;override;
  end;

var
  PEA: TPeriodicEventAggregator;



implementation

{ TPeriodicEvent }

function TPeriodicEvent.CanArm: boolean;
begin
  result := true;
end;

destructor TPeriodicEvent.Destroy;
begin
  Unregister;

  inherited;
end;

procedure TPeriodicEvent.DoExecute;
begin
  //stuff
end;

procedure TPeriodicEvent.Execute;
begin
  FExecuting := true;
  if Synchronized then begin
    TThread.Synchronize(TThread.CurrentThread, DoExecute);
  end else begin
    DoExecute;
  end;



//  FExecuting := false;
end;

function TPeriodicEvent.Finished: boolean;
begin
  result := true;
end;

function TPeriodicEvent.GetIsTime: boolean;
begin
  result := TimeuntilEvent <=0;
end;

function TPeriodicEvent.GetTimeuntilEvent: ticker;
begin
  if StartImmediately then begin
    result := 0;
  end else
    result := (FLastExecutionTime+FFrequency) - GetTicker;

end;

procedure TPeriodicEvent.Unregister;
begin
  if FAggregator <> nil then begin
    fAggregator.Remove(self);
    FAggregator := nil;
  end;
end;

{ TPeriodicEventAggregator }

procedure TPeriodicEventAggregator.Add(event: TPeriodicEvent);
begin
  Lock;
  try
    FEvents.add(event);
    HAswork := FEvents.count > 0;
  finally
    Unlock;
  end;


end;

destructor TPeriodicEventAggregator.Destroy;
begin
  if FEvents.count > 0 then begin
    raise Ecritical.create('cannot destroy '+self.ClassName+' when there are still events in the list. List contains'+FEvents[0].classname);
  end;
  FEvents.Free;
  FEvents := nil;
  inherited;

end;

procedure TPeriodicEventAggregator.DoExecute;
var
  FCurrent: TPeriodicEvent;
  tm: ticker;
begin
  inherited;
  ColdRunInterval := 1000;
  Lock;
  try
    if idx >= FEvents.count then begin
      RunHot := false;
      idx := 0;
      exit;
    end;

    RunHot := true;
    FCurrent := FEvents[idx];
    if FCurrent.canarm then begin
      tm := GetTicker;
      if FCurrent.StartImmediately or (GetTimeSince(tm,FCurrent.LastExecutionTime) > FCurrent.Frequency) then begin
        FCurrent.StartImmediately := false;
        FCurrent.Armed := true;
        FCurrent.LastExecutionTime := tm;
      end;
    end;

    inc(idx);
  finally
    Unlock;
  end;

  try
    if FCurrent.Armed then
      FCurrent.Execute;
  finally
    FCurrent.Armed := false;
  end;

end;

procedure TPeriodicEventAggregator.Init;
begin
  inherited;
  FEvents := TList<TPeriodicEvent>.create;


end;

procedure TPeriodicEventAggregator.Remove(event: TPeriodicEvent);
begin
  while event.Armed do begin
    Debug.Log(self, 'Waiting to remove event because it is armed.');
    sleep(100);
  end;


  Lock;
  try
    FEvents.remove(event);
    event.FAggregator := nil;
  finally
    Unlock;
  end;
end;

procedure oinit;
begin
  PEA := TPM.Needthread<TPeriodicEventAggregator>(nil);//.create(nil, nil);
  PEA.Loop := true;
  PEA.BeginStart;

end;


procedure ofinal;
begin
//  PEA.EndStart;
  PEA.Stop;
  PEA.WaitForFinish;
  TPM.NoNeedThread(PEA);
  PEA := nil;

end;

procedure oLATEfinal;
begin
//  PEA.Free;
//  PEA := nil;

end;


{ TPeriodicCommandEvent }

function TPeriodicCommandEvent.CanArm: boolean;
begin
  result := cmd = nil;
  if not result then begin
    if cmd.IsComplete then begin
      DestroyCommand;
      exit(cmd = nil);
    end;
  end;
end;

procedure TPeriodicCommandEvent.Destroycommand;
begin
  if cmd = nil then
    exit;
  cmd.WaitFor;
  cmd.free;
  cmd := nil;
end;

procedure TPeriodicCommandEvent.Detach;
begin
  if assigned(cmd) then
    DestroyCommand;

  inherited;
end;

function TPeriodicCommandEvent.Finished: boolean;
begin
  result := cmd.IsComplete;
end;

{ TPeriodicSingleTon }

procedure TPeriodicSingleTon.CleanupPE;
begin
  PEA.Remove(FPE);
  FPE.Free;
  FPE := nil;

end;

constructor TPeriodicSingleTon.Create;
begin
  inherited;
  FPE := nil;
end;

procedure TPeriodicSingleTon.Detach;
begin
  if detached then exit;
  CleanupPE;
  inherited;

end;

procedure TPeriodicSingleTon.DoPeriodically;
begin
  //implement me in descendant
end;


procedure TPeriodicSingleTon.OnPE(sender: TPeriodicEvent);
begin
  Periodically;
end;

procedure TPeriodicSingleTon.Periodically;
begin
  DoPeriodically;
end;


procedure TPeriodicSingleTon.SetupPE(interval: int64; bGoNow: boolean; bSynchronize: boolean);
var
  bNew: boolean;
begin
  bNew := false;
  if FPE = nil then begin
    FPE := TExternalPeriodicEvent.create;
    FPE.OnEvent := self.OnPE;
    bNew := true;
  end;
  FPE.Frequency := interval;
  FPE.Startimmediately := bGoNow;
  FPE.Synchronized := bSynchronize;

  if bNew then
    PEA.Add(FPE);



end;

{ TExternalPeriodicEvent }

procedure TExternalPeriodicEvent.DoExecute;
begin
  inherited;
  if Assigned(FOnEvent) then
    FOnEvent(self);
end;

initialization
  init.RegisterProcs('PeriodicEvents', oinit, nil, ofinal, oLATEfinal,  'managedthread,backgroundthreads');


end.
