unit PeriodicEvents;
{x$DEFINE RDTPMODULE}

interface

uses
  debug, systemx, typex, lockqueue, tickcount, managedthread, generics.collections.fixed, orderlyinit;

type
  TPeriodicEventAggregator = class;//forward

  TPeriodicEvent = class(TLockQueue)
  private
    FFrequency: ticker;
    FEnabled: boolean;
    FLastExecutionTime: ticker;
    FAggregator: TPeriodicEventAggregator;
    FArmed: boolean;
    FOwner: TObject;
    function GetIsTime: boolean;
    function GetTimeuntilEvent: ticker;
    procedure Unregister;
  public
    destructor Destroy;override;
    procedure DoExecute;virtual;
    property Frequency: ticker read FFrequency write FFrequency;
    property LastExecutionTime: ticker read FLastExecutionTime write FLastExecutionTime;
    property Enabled: boolean read FEnabled write FEnabled;
    property IsTime: boolean read GetIsTime;
    property TimeUntilEvent: ticker read GetTimeuntilEvent;
    property Armed: boolean read FArmed write FArmed;
    property Owner: TObject read FOwner write FOwner;
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

var
  PEA: TPeriodicEventAggregator;



implementation

{ TPeriodicEvent }

destructor TPeriodicEvent.Destroy;
begin
  Unregister;

  inherited;
end;

procedure TPeriodicEvent.DoExecute;
begin
  //stuff
end;

function TPeriodicEvent.GetIsTime: boolean;
begin
  result := TimeuntilEvent <=0;
end;

function TPeriodicEvent.GetTimeuntilEvent: ticker;
begin
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
    tm := GetTicker;
    if GetTimeSince(tm,FCurrent.LastExecutionTime) > FCurrent.Frequency then begin
      FCurrent.Armed := true;
      FCurrent.LastExecutionTime := tm;
    end;

    inc(idx);
  finally
    Unlock;
  end;

  try
    if FCurrent.Armed then
      FCurrent.DoExecute;
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
  PEA.EndStart;
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


initialization
  init.RegisterProcs('PeriodicEvents', oinit, nil, ofinal, oLATEfinal,  'managedthread,backgroundthreads');


end.
