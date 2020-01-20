unit commands_system;


interface

uses
  classes, commandprocessor, systemx, commandicons, tickcount, orderlyinit, numbers;

type


  Tcmd_Free = class(TCOmmand)
  protected
    procedure DoExecute; override;
  public
    o: TObject;
  end;
  TSleepCommand = class(TCommand)
  private
    FLength: cardinal;
  public
    property Length: cardinal read FLength write FLength;
    procedure DoExecute;override;
  end;

  Tcmd_Garbage = class(TCommand)
  private
    FCommandObject: TCommand;
    FObject: TObject;
    FSynchronize: boolean;
    public
    procedure Go;
    procedure Init;override;
    constructor Create;override;
    procedure DoExecute;override;
    procedure Cancel;override;//<<--this command cannot be cancelled.
    property CommandObject: TCommand read FCommandObject write FCommandObject;
    property ObjectGarbage: TObject read FObject write FObject;
    property Synchronize: boolean read FSynchronize write FSynchronize;
  end;

  Tcmd_SelfDestruct = class(Tcmd_Garbage)
  private
    Ftime: ticker;
    tmCreate: ticker;
    procedure SEtTime(const Value: ticker);
  protected
    
    procedure DoExecute; override;
  public
    procedure Init; override;
    property time: ticker read Ftime write SEtTime;
  end;



procedure AddSleepCommand(cp: TCommandProcessor; duration: cardinal);
procedure GarbageCollect(o: TObject; by: TCommandProcessor= nil);
procedure GarbageCollectEx(o: TObject; sync: boolean; by: TCommandProcessor= nil);
procedure SelfDestruct(o: TObject; time: Ticker; by: TCommandProcessor = nil);
function BeginFree(o: TObject; by: TCommandProcessor = nil): Tcmd_Free;
procedure EndFree(c: Tcmd_Free);
function FloatCompare(f1,f2: double): integer;

var
  GCP: TCommandProcessor = nil;
  cpcreate: TCLXCriticalSection;


implementation

uses
  managedthread;


procedure AddSleepCommand(cp: TCommandProcessor; duration: cardinal);
var
  c: TSleepCommand;
begin
  c := TSleepCommand.create;
  c.length := duration;
  cp.addcommand(c);
end;



{ TSleepCommand }

procedure TSleepCommand.DoExecute;
var
  t: integer;
  i: cardinal;
begin
  inherited;
  Status := 'Sleeping...';
  i := Length div 10;

  StepCount := 9;
  for t:= 0 to 9 do begin
    Step := t;
    sleep(1000);

  end;


end;


{ Tcmd_Garbage }

procedure Tcmd_Garbage.Cancel;
begin
//  inherited;
  //block this command from being cancelled.

end;

constructor Tcmd_Garbage.Create;
begin
  inherited;
  OwnedByProcessor := true;
  FireForget:= true;
  CPuExpense := 0.1;

end;

procedure Tcmd_Garbage.DoExecute;
begin
  inherited;

  if Synchronize then
    TThread.Synchronize(TThread.CurrentThread, Go)
  else
    Go;

end;

procedure Tcmd_Garbage.Go;
begin
  var obj: TObject;

  if assigned(CommandObject) then begin
    CommandObject.DeadCheck;
//    Status :='wait for';
    CommandObject.WaitFor;
//    Status :='free';
    CommandObject.Free;
//    Status :='complete';
    CPUExpense := 0;
  end;

  if assigned(objectgarbage) then begin
    obj := ObjectGarbage;
    ObjectGarbage := nil;
    obj.free;
    obj := nil;


  end;

end;

procedure Tcmd_Garbage.Init;
begin
  inherited;
  Icon := @CMD_ICON_GARBAGE;
end;


procedure SelfDestruct(o: TObject; time: Ticker; by: TCommandProcessor = nil);
var
  c: Tcmd_SelfDestruct;
BEGIN
  IF O = nil then
    exit;

  IF by = nil then begin
    if GCP = nil then begin
      ecs(cpcreate);          
      try
        if by= nil then begin
          gcp := TCommandProcessor.Create(nil, 'GarbageCollector');            
        end;
      finally
        lcs(cpcreate);
      end;
    end;
    by := gcp;
  end;

  c := Tcmd_SelfDestruct.create;
  c.time := time;
  if o is Tcommand then
    c.CommandObject := TCommand(o)
  else
    c.ObjectGarbage := o;
  c.FireForget := true;
  c.start(by);


END;

function BeginFree(o: TObject; by: TCommandProcessor = nil): Tcmd_Free;
begin
  result := Tcmd_Free.create;
  result.o := o;
  result.start;
end;

procedure EndFree(c: Tcmd_Free);
begin
  c.WaitFor;
  c.free;
end;

procedure GarbageCollectEx(o: TObject; sync: boolean; by: TCommandProcessor= nil);
var
  c: Tcmd_garbage;
begin
  IF O = nil then
    exit;

  IF by = nil then begin
    if GCP = nil then begin
      ecs(cpcreate);
      try
        if by= nil then begin
          gcp := TCommandProcessor.Create(nil, 'GarbageCollector');
        end;
      finally
        lcs(cpcreate);
      end;
    end;
    by := GCP;
  end;

  c := Tcmd_Garbage.create;
  if o is TCommand then
    c.CommandObject := TCommand(o)
  else
    c.ObjectGarbage := o;
  c.Synchronize := sync;
  c.Start(by);

end;

procedure GarbageCollect(o: TObject; by: TCommandProcessor= nil);
begin
  GarbageCollectEx(o, false, by);
end;

function FloatCompare(f1,f2: double): integer;
var
  e: double;
begin
  e := f2-f1;
  if abs(e) < 0.0001 then begin
    result := 0;
  end else begin
    if e > 0 then result := 1
    else result := -1;
  end;

end;

{ Tcmd_SelfDestruct }

procedure Tcmd_SelfDestruct.DoExecute;
var
  tosleep: ticker;
begin
  inherited;

end;


procedure oinit;
begin
  ics(cpcreate);
  
end;

procedure ofinal;
begin
  gcp.free;
  gcp := nil;

  dcs(cpcreate);
end;


procedure Tcmd_SelfDestruct.Init;
begin
  inherited;
  tmCReate := getticker;
end;

procedure Tcmd_SelfDestruct.SEtTime(const Value: ticker);
begin
  Ftime := Value;
{$R-}
  FutureExecutionTime := value+tmCReate;
end;

{ Tcmd_Free }

procedure Tcmd_Free.DoExecute;
begin
  inherited;
  o.free;
end;

initialization

init.RegisterProcs('commands_system', oinit, ofinal, 'CommandProcessor');


end.


