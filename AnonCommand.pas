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
  protected
    procedure SyncFinished;
    procedure SyncExecute;
    procedure SyncError;
    procedure DoExecute; override;
  public
    procedure InitExpense; override;
    procedure Detach; override;
    property SynchronizeFinish: boolean read FSynchronizeFinish write FSynchronizeFinish;
    constructor Create(AThreadFunc: TFunc<T>; AOnFinishedProc: TProc<T>;AOnErrorProc: TProc<Exception>; ACreateSuspended: Boolean = False;
      FreeOnComplete: Boolean = True);
    destructor Destroy;override;


//    class constructor Create;
//    class destructor Destroy;

 end;

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

end.
