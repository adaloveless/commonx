unit EncoderMaster;
{x$DEFINE REXE}
interface

uses
  debug, systemx, typex, stringx, betterobject, commandprocessor, managedthread,sysutils,
  skill,herro, fileserviceclient, orderlyinit, PeriodicEvents;

type
  TEncoderMaster = class;//forward

  TEncoder = record
    info: TSkillInfo;
    CPUsConsumed: single;
    GPUsConsumed: single;
    GPUsTotal: single;
    MemoryGBConsumed: single;
    CPUsTotal: single;
    MemoryGBTotal: single;
    timeofDeath: TDateTime;
    AliveTime: TdateTime;
    CheckTime: TDateTime;
    cli: IHolder<TFileServiceClient>;
    procedure Init;
    procedure Ping;
    function CPUStr: string;
    function GPUStr: string;
    function MemStr: string;
    function CPUSlack: single;
    function MemSlack: single;
    function GPUSlack: single;
    function HasResourcesAvailable: boolean;
    function IsAlive: boolean;
  end;

  PEncoder = ^TEncoder;

  TEncoderWork = record
    handle: int64;
    assignedto: PEncoder;
    remotehandle: int64;
    prog: string;
    params: string;
    gpuparams: string;
    CPUExpense, MemoryGBExpense, GPUExpense: single;
    GPUNumber: ni;
    Status: string;
    Step: int64;
    StepCount: int64;
    ConsoleOutput: string;
    Complete: boolean;
    FConsoleDrain: string;
    function DrainConsole: string;
    procedure AddToConsoleDrain(cc: string);
    procedure Init;
    function ToString: string;
    function PercentComplete: single;
    function PercentString: string;
  end;
  PEncoderWork = ^TEncoderWork;

  TEncoderCheckPeriodicEvent = class(TPeriodicEvent)
  strict protected
    procedure DoExecute; override;
  public
    em: TEncoderMaster;
  end;

  TEncoderMaster = class(TsharedObject)
  private
    nextworkhandle: int64;
    pe: TEncoderCheckPeriodicEvent;
    Fwork: TArray<TEncoderWork>;
    Fencoders: TArray<TEncoder>;
    procedure StopPE;
    procedure StartPE;

    procedure ScanForEncoders;
    function DispatchNewWork: boolean;
    procedure CheckExistingWork;
    procedure checkWorkItem(pew: PEncoderWork);
    procedure FinishworkItem(pew: PEncoderWork);

   function HasHost(sHost: string): boolean;
    procedure AddEncoderFromSkill(sk: TSkillInfo);
    function GetEncoders: TArray<TEncoder>;
    procedure Periodically;
    function GetUnstartedWork: PEncoderWork;
    function GetWork: TArray<TEncoderWork>;
  public
    constructor Create; override;
    procedure Detach; override;

    function QueueWork(w: TEncoderWork): int64;
    property Encoders: TArray<TEncoder> read GetEncoders;
    property Work: TArray<TEncoderWork> read GetWork;
    function GetWorkStatus(hand: int64): TEncoderWork;
    procedure ReleaseWorkitem(handle: int64);
    function GetKnownResources(out cpus,memoryGB, gpus:single): boolean;
  end;

var
  EM: TEncoderMaster;


implementation

{ TEncoderMaster }

procedure TEncoderMaster.AddEncoderFromSkill(sk: TSkillInfo);
var
  e: TEncoder;
begin
  if sk.host = '192.168.101.119' then
    exit;
  setlength(Fencoders, length(Fencoders)+1);
  e.init;
  e.info := sk;
  Fencoders[high(Fencoders)] := e;
end;


procedure TEncoderMaster.CheckExistingWork;
begin
  var l := locki;
  for var t:=0 to high(Fwork) do begin
    var pew :PEncoderWork := @Fwork[t];
    var penc : PEncoder := pew.assignedto;
    if penc<> nil then begin
      checkWorkItem(pew);
    end;
  end;
end;

procedure TEncoderMaster.checkWorkItem(pew: PEncoderWork);
begin
  if pew.assignedto = nil then raise Ecritical.create('cannot check when pew.penc=nil!');

  if not pew.Complete then begin
    var status := '';
    var step:int64:=0;
    var stepcount:int64:=1;
    var fin: boolean := false;
    var cc := '';
    if assigned(pew.assignedto) then
    if assigned(pew.assignedto.cli) then
    if assigned(pew.assignedto.cli.o) then
    if pew.assignedto.cli.o.GetExeCommandStatus(pew.remotehandle,status, fin, cc) then begin
      pew.status := status;
      pew.AddToConsoleDrain(cc);
      if fin then begin
        FinishworkItem(pew);
      end;
    end;
  end;


end;

constructor TEncoderMaster.Create;
begin
  inherited;
  StartPE;
end;

procedure TEncoderMaster.Detach;
begin
  if detached then exit;
  StopPE;
  inherited;

end;

function TEncoderMaster.DispatchNewWork: boolean;
var
  slack: single;
  bestCPUslack, bestGPUslack: single;
  bestpenc: PEncoder;
  bUseGPU: boolean;
  sAccel: string;
begin
  bestcpuslack := 0;
  bestgpuslack:= 0;
  bestpenc := nil;
  result := false;
  bUseGPU := false;
  var l := LockI;
  var pew : PEncoderwork := GetUnstartedWork;

  if pew = nil then exit;


  //look for an encoder with available cpus and ram
  var penc : PEncoder := nil;
  for var t:= 0 to high(Fencoders) do begin

    if Fencoders[t].HasResourcesAvailable
//    and (FEncoders[t].info.Host <> '192.168.101.119')
    and Fencoders[t].IsAlive then begin
      penc := @FEncoders[t];
      if bUseGPU or (penc.GPUSlack > 0.0) then begin
        bUseGPu := true;
        slack := penc.GPUSlack;
        if slack > bestgpuslack then begin
          bestgpuslack := slack;
          bestpenc := penc;
        end;
      end else begin
        slack := penc.CPUSlack;
        if (slack > bestcpuslack) then begin
          bestcpuslack := slack;
          bestpenc := penc;
        end;
      end;
    end;
  end;

  penc := bestpenc;
  if penc = nil then exit;

  //send work item to encoder (it will reject it if the resource availability changes)
  var ext_resources := '';
  if bUseGPU then begin
    pew.CPUExpense := 1.0;
    pew.GPUExpense := 1.0;
    ext_resources := 'GPUExpense='+floatprecision(pew.gpuexpense,4);
  end;

  var hand := penc.cli.o.StartExeCommandExFFMPEG(extractfilepath(pew.prog),extractfilename(pew.prog), pew.params, pew.gpuparams,pew.CPUExpense, pew.MemoryGBExpense, pew.GPUExpense);
  if hand = 0 then begin
    Debug.Log('this might stall.  could not dispatch work because server rejected it');
    exit(false);
  end;

  //flag local work item and change states
  penc.CPUsConsumed := penc.CPUsConsumed + pew.CPUExpense;
  penc.GPUsConsumed := penc.GPUsConsumed + pew.GPUExpense;
  penc.MemoryGBConsumed := penc.MemoryGBConsumed + pew.MemoryGBExpense;
  pew.assignedto := penc;
  pew.remotehandle := hand;
  pew.Complete := false;
  result := true;



end;

procedure TEncoderMaster.FinishworkItem(pew: PEncoderWork);
begin
  var l := Locki;
  pew.ConsoleOutput := pew.assignedto.cli.o.EndExeCommand(pew.remotehandle);
  pew.Complete := true;
  if pew.assignedto <> nil then
    pew.assignedto.Ping;
end;

function TEncoderMaster.GetEncoders: TArray<TEncoder>;
begin
  var l := Locki;
  result := FEncoders;
  setlength(result, length(result));
end;

function TEncoderMaster.GetKnownResources(out cpus, memoryGB, gpus: single): boolean;
begin
  var l := Locki;
  var a := FEncoders;
  setlength(a, length(a));
  l :=nil;
  cpus := 0;
  memoryGB := 0;
  gpus := 0;
  for var t := 0 to high(a) do begin
    cpus := cpus + a[t].CPUsTotal;
    memoryGB := memoryGB + a[t].MemoryGBTotal;
    gpus := gpus + a[t].GPUsTotal;
  end;

  result := true;

end;

function TEncoderMaster.GetUnstartedWork: PEncoderWork;
begin
  var l := Locki;
  result := nil;
  for var t:= 0 to high(Fwork) do begin
    if Fwork[t].assignedto = nil then begin
      exit(@Fwork[t]);
    end;
  end;

end;

function TEncoderMaster.GetWork: TArray<TEncoderWork>;
begin
  var l:= LockI;
  result := Fwork;
  setlength(result, length(result));

end;

function TEncoderMaster.GetWorkStatus(hand: int64): TEncoderWork;
begin
  result.Init;
  var l := locki;
  for var t:=0 to high(Fwork) do begin
    if FWork[t].handle = hand then begin
      result := Fwork[t];
      FWork[t].DrainConsole;
      exit;
    end;
  end;
end;

function TEncoderMaster.HasHost(sHost: string): boolean;
begin
  var l:= LockI;
  for var t:=0 to high(Fencoders) do begin
    if comparetext(Fencoders[t].info.Host,sHost)=0 then
      exit(true);
  end;
  exit(false);
end;

function TEncoder.HasResourcesAvailable: boolean;
begin
  result := (CPUSlack > 0.0) or (GPUSlack > 0.0);// and (MemSlack > 0.0);

end;


procedure TEncoderMaster.Periodically;
begin
  try
    repeat
    ScanForEncoders;
    CheckExistingWork;
    until not DispatchNewWork;
  except
  end;
end;

function TEncoderMaster.QueueWork(w: TEncoderWork): int64;
begin
  var l:=locki;
  setlength(Fwork, length(Fwork)+1);
  inc(nextworkhandle);
  w.handle := nextworkhandle;
  Fwork[high(Fwork)] := w;
  Debug.Log('work queued '+w.prog+' '+w.params);
  exit(w.handle);

end;

procedure TEncoderMaster.ReleaseWorkitem(handle: int64);
begin
  var l := locki;
  for var t := high(Fwork) downto 0 do begin
    if FWork[t].handle = handle then begin
      for var u := t to high(Fwork)-1 do begin
        FWork[u] := FWork[u+1];
      end;
      setlength(fwork, length(FWork)-1);
    end;
  end;
end;

procedure TEncoderMaster.ScanForEncoders;
begin
  //to find encoders we'll just go through the greeter/herro service
  var sks := skills.GetSkillsOfType('FileService');

  var l := LockI;
  for var t:=0 to high(sks) do begin
    if not HasHost(sks[t].host) then begin
      AddEncoderFromSkill(sks[t]);
    end;
  end;

  for var t:=0 to high(self.Fencoders) do begin
    try
      Fencoders[t].ping;
    except
      FEncoders[t].timeofdeath := now();
    end;
  end;

  var cpus :single := 0.0;
  var mem :single := 0.0;
  var gpus : single := 0.0;
  GetKnownResources(cpus,mem,gpus);
  BGCMD.SetResourceLimit('RemoteCPU', cpus);
  BGCMD.SetResourceLimit('RemoteGB', mem);
  BGCMD.SetResourceLimit('RemoteGPU', gpus);



end;

procedure TEncoderMaster.StartPE;
begin
  if pe <> nil then
    exit;

{$IFDEF REXE}
  pe := TEncoderCheckPeriodicEvent.create;
  pe.Frequency := 1000;
  pe.Startimmediately := true;
  pe.em := self;
  PEA.Add(pe);
{$ENDIF}
end;

procedure TEncoderMaster.StopPE;
begin
{$IFDEF REXE}
  if pe = nil then
    exit;
  pea.Remove(pe);
  pe.Free;
  pe := nil;
{$ENDIF}

end;

{ TEncoder }

function TEncoder.CPUSlack: single;
begin
  result := CPUsTotal-CPUsConsumed;
end;

function TEncoder.CPUStr: string;
begin
  result := FloatPrecision(CPUsConsumed,2)+'/'+floatprecision(CPUsTotal,2);
end;

function TEncoder.GPUSlack: single;
begin
  result := GPUsTotal-GPUsConsumed;
end;

function TEncoder.GPUStr: string;
begin
  result := FloatPrecision(GPUsConsumed,2)+'/'+floatprecision(GPUsTotal,2);

end;

procedure TEncoder.Init;
begin
  TimeofDeath := 0.0;
  CheckTime := 0.0;
  AliveTime := 0.0;
end;



function TEncoder.IsAlive: boolean;
begin
  result := (now()-TImeOfDeath) > (1/(24*120));
end;

procedure oinit;
begin
  EM := TEncoderMaster.Create;
end;

procedure ofinal;
begin
  EM.free;
  EM := nil;
end;

function TEncoder.MemSlack: single;
begin
  result := MemoryGBTotal - MemoryGBConsumed;
end;

function TEncoder.MemStr: string;
begin
  result := FloatPrecision(MemoryGBConsumed,2)+'/'+floatprecision(MemoryGBTotal,2);
end;

procedure TEncoder.Ping;
var
  cpu,cpumax,gb,gbmax: single;
  gpu,gpumax: single;
begin
  if not IsAlive then
    exit;
  if info.host = '' then exit;
  try
    checktime := now;
    if cli = nil then begin
     cli := THolder<TFileServiceClient>.create;
     Debug.Log(info.host);
     cli.o := TFileServiceClient.create(info.host,info.endpoint);
    end;

    var ext_resources := '';
    if CLI.O.GetCommandResourceConsumptionEx(cpu,cpumax,gb,gbmax, gpu, gpumax, {out}ext_resources) then begin
      CPUsConsumed := cpu;
      CPUsTotal := cpumax;
      GPUsTotal := gpumax;
      GPUsconsumed := gpu;
      MemoryGBConsumed := gb;
      MemoryGBTotal := gbmax;
      AliveTime := now;
    end;


  except
    timeofdeath := now;
  end;
end;

{ TEncoderCheckPeriodicEvent }

procedure TEncoderCheckPeriodicEvent.DoExecute;
begin
  inherited;
  em.Periodically;
end;

{ TEncoderWork }

procedure TEncoderWork.AddToConsoleDrain(cc: string);
begin
  if EM.TryLock then
  try
    FConsoleDrain := FConsoleDrain + cc;
  finally
    em.Unlock;
  end;

end;

function TEncoderWork.DrainConsole: string;
begin
  if EM.TryLock then
  try
    result := FconsoleDrain;
    FConsoleDrain := '';
  finally
    em.Unlock;
  end;

end;

procedure TEncoderWork.Init;
begin
  Complete := false;
  AssignedTo := nil;
  remotehandle := 0;
  CPuExpense := 1.0;
  MemoryGBExpense := 1.0;
end;

function TEncoderWork.PercentComplete: single;
begin
  if StepCount = 0 then
    result := 0
  else
    result := step/stepcount;

end;

function TEncoderWork.PercentString: string;
begin
  result := floatprecision(percentcomplete*100, 2)+'%';
end;

function TEncoderWork.ToString: string;
begin
  if Complete then
    result := '*'
  else
    result := ' ';

  result := result + PercentString+' '+params;
end;

initialization
  init.RegisterProcs('EncoderMaster', oinit, ofinal, 'PeriodicEvents');

end.
