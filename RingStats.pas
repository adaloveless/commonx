unit RingStats;

interface

uses
  typex, tickcount, sysutils, orderlyinit, managedthread, generics.collections.fixed, sharedobject, threadmanager, debug, stringx, numbers;
{$DEFINE DISABLE_RING_STATS}
type
  TRingStat = record
    value: nativefloat;
    tick: ticker;
  end;
  TRingStats = class(TSharedobject)
  private
    FArray: array of TRingStat;
    FIdx: nativeint;
    FBeginTime: ticker;
    FPeriodicMax: nativefloat;

    FAccumulator: nativefloat;
    FName: string;
    FPeriodicAverage: nativefloat;
    lastperiodupdate: ticker;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetSize: ni;

  public
    TotalItemsThrough: ni;
    procedure Init;override;
    procedure AddStat(f: nativefloat);
    function GetAverage: nativefloat;
    function GetMax: nativefloat;
    function GetMin: nativefloat;
    procedure BeginTime;inline;
    procedure EndTime;inline;
    function DebugString: string;
    function DebugTiming: string;
    function NewBAtch: boolean;inline;
    procedure Accumulate(f: nativefloat);
    procedure RollUpAccumulator;
    property Name: string read GetName write SetName;
    procedure SetSize(const sz: ni);
    property PeriodicAverage: nativefloat read FPeriodicAverage;
    property Size:ni read GetSize write SetSize;
    function TimeSpan: ticker;
    function Rate: double;
    function Interval: double;
    function Minimum: double;
    function MAximum: double;
    PROPERTY PeriodicMax: nativefloat read FPeriodicMax;
    procedure OptionDebug(additional: string);
  end;

  TRingStatMonitorThread = class(TManagedThread)
  private
    FStats: TList<TRingStats>;
    FPeriodicMax: nativefloat;
  public
    procedure DoExecute;override;
    procedure REgisterRingStat(rs: TRingStats);
    procedure UnregisterRingStat(rs: TRingStats);
    procedure Init;override;
    destructor Destroy;override;
    function GetDebugString: string;
    procedure Start;override;
    property PeriodicMax: nativefloat read FPeriodicMax;

  end;

var
  rsmon: TRingStatMonitorThread;


implementation

{ TRingStats }

procedure TRingStats.Accumulate(f: nativefloat);
begin
  Lock;
  try
    fAccumulator := FAccumulator + f;
  finally
    Unlock;
  end;
end;

procedure TRingStats.AddStat(f: nativefloat);
var
  h: ni;
begin
  h := high(FArray);
  FArray[FIdx].value := f;
  FArray[FIdx].tick := getticker;
  if (FIdx > h) or (GEtTimeSince(lastperiodupdate) > 10000) then begin
    FPeriodicAverage := GetAverage;
    //debug.ConsoleLog(floatprecision(FPeriodicAverage, 2));
    lastperiodupdate := getticker;
    inc(TotalITemsThrough);
  end;
  inc(FIdx);
  if FIDX > h then
    FIDX := 0;

end;

procedure TRingStats.BeginTime;
begin
  FBeginTime := tickcount.GethighResTicker;
end;

function TRingStats.DebugString: string;
begin
  result := Name+' Avg:'+floattostr(GetAverage)+' Min:'+floattostr(GetMin)+' Max:'+floattostr(GetMax);
end;

function TRingStats.DebugTiming: string;
begin
  result := Name+' usec Avg:'+floattostr(GetAverage/10)+' Min:'+floattostr(GetMin/10)+' Max:'+floattostr(GetMax/10);
end;

procedure TRingStats.EndTime;
var
  endtime: ticker;
begin
  endtime := GetHighResTicker;
  AddStat(GetTimeSince(endtime, FBeginTime));

end;

function TRingStats.GetAverage: nativefloat;
var
  x: nativeint;
  v, mx: nativefloat;

begin
  result := 0;

  mx := 0;
  for x := low(FArray) to high(FArray) do begin
    v := FArray[x].value;
    result := result + v;
    mx := greaterof(v,mx);
  end;

  FPeriodicMAx := mx;
  result := result / length(FArray);
end;

function TRingStats.GetMax: nativefloat;
var
  x: nativeint;
begin
  result := FArray[low(FArray)].value;
  for x := low(FArray)+1 to high(FArray) do begin
    if FArray[x].value > result then
      result := FArray[x].value;
  end;
end;

function TRingStats.GetMin: nativefloat;
var
  x: nativeint;
begin
  result := Farray[low(FArray)].value;
  for x := low(FArray)+1 to high(FArray) do begin
    if FArray[x].value < result then
      result := FArray[x].value;
  end;
end;

function TRingStats.GetName: string;
begin
  Lock;
  try
    result := FName;
  finally
    Unlock;
  end;
end;

function TRingStats.GetSize: ni;
begin
  result := length(FArray);
end;

procedure TRingStats.Init;
begin
  inherited;
  setSize(256);
end;

function TRingStats.Interval: double;
var
  temp: ticker;
begin
  //assumes buffer is full of tick counts
  temp := TimeSpan;
  if temp = 0 then
    temp := 1;
  result := temp/length(FArray);
end;

function TRingStats.Minimum: double;
var
  t: ni;
  v: double;
begin
  result := FArray[0].value;
  for t:= 0 to high(FArray) do begin
    v := FArray[t].value;
    if v< result then
      result := v;
  end;

end;

function TRingStats.MAximum: double;
var
  t: ni;
  v: double;
begin
  result := FArray[0].value;
  for t:= 0 to high(FArray) do begin
    v := FArray[t].value;
    if v> result then
      result := v;
  end;

end;


function TRingStats.NewBAtch: boolean;
begin
  result := FIdx = 0;
end;

procedure TRingStats.OptionDebug(additional: string);
begin
  if NewBatch then begin
    Debug.Log(additional+':'+DebugTiming);
  end;

end;

function TRingStats.Rate: double;
var
  temp: ticker;
begin
  //assumes buffer is full of tick counts
  temp := TimeSpan;
  if temp = 0 then
    temp := 1;
  result := (length(FArray)/temp) * 1000;
end;

procedure TRingStats.RollUpAccumulator;
begin
  Lock;
  try
    AddStat(FAccumulator);
    FAccumulator := 0;
  finally
    Unlock;
  end;
end;

procedure TRingStats.SetName(const Value: string);
begin
  Lock;
  try
    FName := value;
  finally
    Unlock;
  end;
end;

procedure TRingStats.SetSize(const sz: ni);
begin
  setlength(Farray, sz);
end;

function TRingStats.TimeSpan: ticker;
begin
  //assuming that the buffer is full of tick counts
  result := Self.FArray[FIdx].tick;
  result := gettimesince(Self.FArray[(FIdx+(length(FArray)-1)) mod Length(FArray)].tick, result);
end;

procedure oinit;
begin
  rsMon := TPM.Needthread<TRingStatMonitorThread>(nil);
  rsMon.ColdRunInterval := 1000;
    rsMon.Loop := true;
{$IFNDEF DISABLE_RING_STATS}
  rsMon.start;
{$ENDIF}
end;

procedure oprefinal;
begin
{$IFNDEF DISABLE_RING_STATS}
  rsMon.Stop;
{$ELSE}
  if not rsMon.Started then begin
    rsMon.Start;

//    sleep(4000);
    rsMon.BeginStop;
  end;
{$ENDIF}
end;

procedure ofinal;
begin
  rsMon.WaitForFinish;
  Debug.Log(rsmon.nameex+' finished with signal state '+rsMon.getsignaldebug);

  TPM.NoNeedthread(rsMon);

  rsMon := nil;
end;

procedure oLATEfinal;
begin
  //
end;



{ TRingStatMonitorThread }


destructor TRingStatMonitorThread.Destroy;
begin
  inherited;
  FStats.free;
end;

procedure TRingStatMonitorThread.DoExecute;
var
  t: ni;
begin
  INHERITED;
  RunHot := false;
  Lock;
  try
    for t:= 0 to FStats.Count-1 do begin
      FStats[t].RollUpAccumulator;
    end;
  finally
    Unlock;
  end;

end;

function TRingStatMonitorThread.GetDebugString: string;
var
  t: ni;
begin
  lock;
  try
    result := '';
    for t:= 0 to FStats.Count-1 do begin
      result := result + FStats[t].DebugString+NEWLINE;
    end;
  finally
    unlock;
  end;

end;


procedure TRingStatMonitorThread.Init;
begin
  FStats := TList<TRingStats>.create;
  inherited;

end;

procedure TRingStatMonitorThread.REgisterRingStat(rs: TRingStats);
begin
  Lock;
  try
    FStats.Add(rs);
  finally
    Unlock;
  end;

end;


procedure TRingStatMonitorThread.Start;
begin
  inherited;
  Debug.Log('Starting Ring Stat Monitor by SOMEONE.');
end;

procedure TRingStatMonitorThread.UnregisterRingStat(rs: TRingStats);
begin
  Lock;
  try
    FStats.Remove(rs);
  finally
    Unlock;
  end;

end;

initialization

orderlyinit.init.RegisterProcs('RingStats', oinit, oPreFinal, ofinal, oLATEfinal,  'ManagedThread');

finalization



end.
