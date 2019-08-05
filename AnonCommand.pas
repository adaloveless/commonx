unit AnonCommand;

interface

{x$DEFINE ANON_SYNC_EXEC}

uses
  debug, System.Classes, System.SysUtils, System.Generics.Collections, better_collections, typex, systemx, managedthread, commandprocessor;

type
  EAnonymousThreadException = class(Exception);

  Tcmd_Test = class(TCommand)
  public
    procedure DoExecute;override;
  end;



  TAnonymousCommand<T> = class(TCommand)
  private
    Err: Exception;
    FThreadFunc: TFunc<T>;
    FOnErrorProc: TProc<Exception>;
    FOnFinishedProc: TProc<T>;
    FResult: T;
    FStartSuspended: Boolean;
    FSynchronizeFinish: boolean;
    FSynchronizeExecute: boolean;
  strict private
    procedure SyncFinished;
    procedure SyncExecute;
    procedure SyncError;
  protected
    procedure DoExecute; override;
  public
    procedure InitExpense; override;
    procedure Detach; override;
    property SynchronizeFinish: boolean read FSynchronizeFinish write FSynchronizeFinish;
    property SynchronizeExecute: boolean read FSynchronizeExecute write FSynchronizeExecute;
    constructor CreateAndTrack(tracker: TCommandList<TCommand>; AThreadFunc: TFunc<T>; AOnFinishedProc: TProc<T>;AOnErrorProc: TProc<Exception>; ACreateSuspended: Boolean = False);virtual;
    constructor Create(AThreadFunc: TFunc<T>; AOnFinishedProc: TProc<T>;AOnErrorProc: TProc<Exception>; ACreateSuspended: Boolean = False;
      FreeOnComplete: Boolean = True);reintroduce;
    destructor Destroy;override;
//    class constructor Create;
//    class destructor Destroy;
 end;
  TAnonymousGUICommand<T>  = class(TAnonymousCommand<T>)
  //THIS is an ANONYMOUs COMMAND that uses SYNCHRONIZE in OnFinish
  //by default.  You can choose to synchronize Execution and Finish of any anonymous
  //command.  This derivative simply has a different default for SynchronizeFinish
  public
    constructor Create(AThreadFunc: TFunc<T>; AOnFinishedProc: TProc<T>;AOnErrorProc: TProc<Exception>; ACreateSuspended: Boolean = False;
      FreeOnComplete: Boolean = True);reintroduce;
  end;



  TAnonymousFunctionCommand = class(TAnonymousCommand<boolean>)
  public
    constructor CreateInline(AThreadFunc: TProc; ACreateSuspended: Boolean = False;
      FreeOnComplete: Boolean = false);reintroduce;
    constructor CreateInlineWithGui(AThreadFunc, GuiFunc: TProc; ACreateSuspended: Boolean = False;
      FreeOnComplete: Boolean = false);reintroduce;
  end;

  TAnonTimerProc = reference to procedure();

  TAnonymousTimer = class(TAnonymousCommand<boolean>)
  public
    timerproc: TAnonTimerProc;
    procedure InitExpense;override;
  end;

{ TAnonymousTimer }

function InlineProc(proc: TProc): TAnonymousCommand<boolean>;
function InlineProcWithGui(proc, guiproc: TProc): TAnonymousCommand<boolean>;

//function InlineProc<T>(proc: TProc): TAnonymousCommand<T,boolean>;


function SetTimer(interval: ni; ontimerproc: TAnonTimerProc): TAnonymousTimer;
function SetTimerGUI(interval: ni; ontimerproc: TAnonTimerProc): TAnonymousTimer;

implementation

{$IFDEF MACOS}
uses
{$IFDEF IOS}
  iOSapi.Foundation
{$ELSE}
  MacApi.Foundation
{$ENDIF IOS}
  ;
{$ENDIF MACOS}

{ TAnonymousCommand }

//class constructor TAnonymousCommand<T>.Create;
//begin
//  inherited;
//end;

//class destructor TAnonymousCommand<T>.Destroy;
//begin
//  inherited;
//end;

procedure TAnonymousCommand<T>.Detach;
begin
  Debug.log(self, 'Detaching');
  if detached then exit;
  inherited;

end;

function InlineProc(proc: TProc): TAnonymousCommand<boolean>;
var
  res: TAnonymousCommand<boolean>;
begin
  res := TAnonymousFunctionCommand.createinline(proc, false, false);
  result := res;
end;

function InlineProcWithGui(proc, guiproc: TProc): TAnonymousCommand<boolean>;
var
  res: TAnonymousCommand<boolean>;
begin
  res := TAnonymousFunctionCommand.createinlinewithgui(proc, guiproc, false, false);

  result := res;
  result.start;

end;


constructor TAnonymousCommand<T>.Create(AThreadFunc: TFunc<T>; AOnFinishedProc: TProc<T>;
  AOnErrorProc: TProc<Exception>; ACreateSuspended: Boolean = False; FreeOnComplete: Boolean = True);
begin
  Debug.Log('Creating '+self.GetObjectDebug);
  FOnFinishedProc := AOnFinishedProc;
  FOnErrorProc := AOnErrorProc;
  FThreadFunc := AThreadFunc;
  FireForget := FreeOnComplete;

  FStartSuspended := ACreateSuspended;
  FSynchronizeFinish := true;
{$IFDEF ANON_SYNC_EXEC}
  FSynchronizeExecute := true;
{$ENDIF}

  inherited Create();

  if not ACreateSuspended then
    Start;
end;


constructor TAnonymousCommand<T>.CreateAndTrack(tracker: TCommandList<TCommand>;
  AThreadFunc: TFunc<T>; AOnFinishedProc: TProc<T>;
  AOnErrorProc: TProc<Exception>; ACreateSuspended: Boolean);
begin
  Create(AthreadFunc, AonFinishedProc, AOnErrorProc, ACreateSuspended, false);
  tracker.Add(self);
end;

destructor TAnonymousCommand<T>.Destroy;
begin
//  Debug.Log('Destroying Inherited '+self.GetObjectDebug);
  inherited;
//  Debug.Log('Destroying '+self.GetObjectDebug);
end;

procedure TAnonymousCommand<T>.DoExecute;
{$IFDEF MACOS}
var
  lPool: NSAutoreleasePool;
{$ENDIF}
begin
  inherited;
  Debug.Log(self, 'Executing  '+self.GetObjectDebug);
{$IFDEF MACOS}
  //Need to create an autorelease pool, otherwise any autorelease objects
  //may leak.
  //See https://developer.apple.com/library/ios/#documentation/Cocoa/Conceptual/MemoryMgmt/Articles/mmAutoreleasePools.html#//apple_ref/doc/uid/20000047-CJBFBEDI
  lPool := TNSAutoreleasePool.Create;
  try
{$ENDIF}
    try
      if FSynchronizeExecute then begin
        TThread.Synchronize(self.Thread.realthread, SyncExecute)
      end else
        FResult := FThreadFunc;

      if assigned(FonFinishedProc) then begin
        try
          if FSynchronizeFinish then
            TThread.Synchronize(self.Thread.realthread, SyncFinished)
          else
            FOnFinishedProc(FResult);
        except
          on E:Exception do begin
            Debug.Log('Exception during synchronized anon finish: '+e.message);
          end;
        end;
      end;
    except
      on E: Exception do begin
        Err := e;
        if FSynchronizeFinish then
          TThread.Synchronize(self.Thread.realthread, SyncError)
        else
          FOnErrorProc(E);
      end;
    end;
{$IFDEF MACOS}
  finally
    lPool.drain;
  end;
{$ENDIF}
end;


procedure TAnonymousCommand<T>.InitExpense;
begin
  inherited;
//  self.Resources.SetResourceUsage('CTO_Anonymous', 1.0);
  cpuExpense := 0.0;
end;

procedure TAnonymousCommand<T>.SyncError;
begin
  FOnErrorProc(Err);
end;

procedure TAnonymousCommand<T>.SyncExecute;
begin
  FResult := FThreadFunc;
end;

procedure TAnonymousCommand<T>.SyncFinished;
begin
    FOnFinishedProc(fResult);
end;

{ Tcmd_Test }

procedure Tcmd_Test.DoExecute;
begin
  inherited;
  sleep(4000);
end;

procedure TAnonymousTimer.InitExpense;
begin
  inherited;
  CPuExpense := 0;
end;

function SetTimerGUI(interval: ni; ontimerproc: TAnonTimerProc): TAnonymousTimer;
begin
  result := TAnonymousTimer.create(
    function : boolean
    begin
      sleep(interval);
      exit(true);
    end,
    procedure (b: boolean)
    begin
      ontimerproc();
    end,
    procedure (e: exception)
    begin
    end
  );
  result.SynchronizeFinish := true;
  result.FireForget := true;
  result.start;
end;

function SetTimer(interval: ni; ontimerproc: TAnonTimerProc): TAnonymousTimer;
begin
  result := TAnonymousTimer.create(
    function : boolean
    begin
      sleep(interval);
      exit(true);
    end,
    procedure (b: boolean)
    begin
      ontimerproc();
    end,
    procedure (e: exception)
    begin
    end
  );
  result.SynchronizeFinish := false;
  result.FireForget := true;
  result.start;
end;


{ TAnonymousGUICommand<T> }

constructor TAnonymousGUICommand<T>.Create(AThreadFunc: TFunc<T>;
  AOnFinishedProc: TProc<T>; AOnErrorProc: TProc<Exception>; ACreateSuspended,
  FreeOnComplete: Boolean);
begin
  inherited;
  SynchronizeFinish := true;
end;

{ TAnonymousFunctionCommand }

constructor TAnonymousFunctionCommand.CreateInline(AThreadFunc: TProc;
  ACreateSuspended, FreeOnComplete: Boolean);
var
  funct: TFunc<boolean>;
begin
  funct:= function (): boolean
                begin
                  AthreadFunc();
                  result := true;
                end;


  Create(funct, procedure (b: boolean) begin end, procedure (e: exception) begin end);
end;

constructor TAnonymousFunctionCommand.CreateInlineWithGui(AThreadFunc, GuiFunc: TProc;
  ACreateSuspended, FreeOnComplete: Boolean);
var
  func1: TFunc<boolean>;
  func2: TProc<boolean>;
begin
  func1:= function (): boolean
                begin
                  AthreadFunc();
                  result := true;
                end;

  func2:= procedure (b: boolean)
                begin
                  GuiFunc();
                end;


  Create(func1, func2, procedure (e: exception) begin end, true, false);
  SynchronizeFinish := true;
  Start;


end;

end.
