unit WatchDog;

interface


uses
  PeriodicEvents, betterobject, tickcount, typex, exe, sysutils, orderlyinit, debug;

type
  TWatchDogIgnoreHandle = class;//forward

  TWatchDog = class(TSharedObject)
  private
    FTimeout: ticker;
    type
      Tpe_WatchDog = class(TPeriodicEvent)
      protected
        procedure DoExecute; override;
      public
        wd: TWatchDog;
      end;
  protected
    tmReset: ticker;
    FAction: string;
    FParams: string;
    Alarmed: boolean;
    pe : Tpe_WatchDog;
    FIgnoreRefCount: ni;
    function Alive: boolean;
    procedure Alarm;
    procedure Ignore;
    procedure ClearIgnore;
  public
    procedure Reset;//<<----Reset the watchdog periodically or the watchdog will "timeout" ... in other words it will trigger
    procedure EnableWatchDog(timeout: ticker; exeaction, exeparams: string);
    procedure Disable;
    procedure Check;
    procedure Detach; override;

    property Timeout: ticker read FTimeout write FTimeout;
    function GetIgnorePass: IHolder<TWatchDogIgnoreHandle>;
    procedure ForceAlarm;
  end;

  TWatchDogIgnoreHandle = class(TBetterObject)
  public
    wd: TWAtchDog;
    constructor Create(aWD: TWatchDog);
    destructor Destroy;override;
  end;






var
  WD: TWatchDog;



implementation




{ TWatchDog.Tpe_WatchDog }

procedure TWatchDog.Tpe_WatchDog.DoExecute;
begin
  inherited;
  wd.Check;

end;

{ TWatchDog }

procedure TWatchDog.Alarm;
begin
  Debug.Log('******* WATCHDOG ALARM TRIGGERED ACTION: '+FAction+' '+FParams);
{$IFDEF MSWINDOWS}
  exe.RunProgram(FAction, FParams, extractfilepath(FAction));
{$ENDIF}
end;

function TWatchDog.Alive: boolean;
begin
  result := (FIgnoreRefCount> 0) or (gettimesince(tmReset) < Timeout);
end;

procedure TWatchDog.Check;
begin
  if Alarmed then
    Debug.Log('Watch dog is already alarmed');

  if not alive then
    Alarm;

end;

procedure TWatchDog.ClearIgnore;
begin
  Lock;
  try
    dec(FIgnoreRefCount);
  finally
    Unlock;
  end;

end;

procedure TWatchDog.Detach;
begin
  Disable;

  inherited;

end;

procedure TWatchDog.Disable;
begin
  if assigned(pe) then begin
    PEA.Remove(pe);
    pe.Free;
    pe := nil;
  end;

  FAction := '';
  FParams := '';

end;

procedure TWatchDog.EnableWatchDog(timeout: ticker; exeaction, exeparams: string);
begin
  Reset;
  FAction := exeaction;
  FParams := exeparams;
  Ftimeout := timeout;

  pe := Tpe_WatchDog.Create;
  pe.wd := self;
  pe.Frequency := 10000;
  pe.Enabled := true;
  PEA.Add(pe);

  Debug.Log('Watchdog enabled with timeout '+timeout.tostring+' ms.');

end;

procedure TWatchDog.ForceAlarm;
begin
  Alarm;
end;

function TWatchDog.GetIgnorePass: IHolder<TWatchDogIgnoreHandle>;
begin
  result := THolder<TWatchDogIgnoreHandle>.create;
  result.o := TWatchDogIgnoreHandle.Create(self);

end;

procedure TWatchDog.Ignore;
begin
  Lock;
  try
    inc(FIgnoreRefCount);
  finally
    Unlock;
  end;
end;

procedure TWatchDog.Reset;
begin
  Lock;
  try
    tmREset := getticker;
  finally
    Unlock;
  end;
end;

{ TWatchDogIgnoreHandle }

constructor TWatchDogIgnoreHandle.Create(aWD: TWatchDog);
begin
  wd := aWD;
  wd.Ignore;
end;

destructor TWatchDogIgnoreHandle.Destroy;
begin
  wd.ClearIgnore;

  inherited;
end;


procedure oinit;
begin
  WD := TWatchDog.create;

end;
procedure ofinal;
begin
  WD.Free;
end;


initialization

init.RegisterProcs('WatchDog', oinit, ofinal, 'PeriodicEvents');

end.
