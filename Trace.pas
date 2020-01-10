unit Trace;
{$I 'DelphiDefs.inc'}

interface
{$IFDEF WINDOWS}
{x$DEFINE LOG_TRACE_TO_CONSOLE}
{$ENDIF}
{$DEFINE LOG_TRACE_TO_DISK}
{$DEFINE LOG_TO_MEMORY}
{x$DEFINE CONSOLE_LOG_TRACE_TO_DISK}
{$DEFINE LOG_TO_THREAD_STATUS}

uses
{$IFDEF WINDOWS}
  windows,
{$ELSE}
  {$IFDEF IOS}
  {$ELSE}
    {$IFNDEF OSX}
    androidapi.log,
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
  typex, systemx, sharedobject, sysutils, classes, ringbuffer, stringx;
type
  TLogHook = procedure(s: string) of object;
{$IFDEF WINDOWS}
  logstring = ansistring;
{$ELSE}
  logstring = string;
{$ENDIF}


  TTraceLog = class(Tobject)
  private
    FLogHook: TLogHook;
    FFilter: string;
    slLog: TRingbuffer;
    sect: TCLXCriticalSection;
    iLoggingThread: nativeint;
    fs: TFileStream;
    function GetFilter: string;
    procedure SetFilter(const Value: string);
    function LogFileName: string;

  public
    constructor Create;virtual;
    destructor Destroy;override;
    property LogHook: TLogHook read FLogHook write FLogHook;
    procedure Log(const s: string; const sFilter: string = '');
    function DrainLog: string;
    property Filter: string read GetFilter write SetFilter;
    procedure lock;inline;
    procedure unlock;inline;
  end;


procedure Log(sender: TObject; s: string; sFilter: string = '');overload;
procedure Log(s: string; sFilter: string = '');overload;
procedure Log(sTypeName: string; ptr: pointer; s: string; sFilter: string = '');overload;
procedure ConsoleLog(s: string);

function TraceLog: TTraceLog;

procedure SeTTraceThreadVar(thr: TObject);


var
  GTraceLog: TTraceLog = nil;





implementation

uses OrderlyInit, helpers_stream, tickcount, managedthread;

type
  TThreadLog = record
    thr: TManagedThread;
    procedure Log(s: string);
  end;

threadvar
  threadlog: TThreadLog;


procedure SeTTraceThreadVar(thr: TObject);
begin
  threadlog.thr := TManagedThread(thr);
end;
function DebugLog: TTraceLog;
begin
  if GTraceLog = nil then
    GTraceLog := TTraceLog.create;

  result := GTraceLog;
end;


procedure Log(s: string; sFilter: string = '');overload;
begin
  Log(nil, s,sFilter);
end;
procedure Log(sender: TObject; s: string; sFilter: string = '');
var
  sobj: string;
begin
  if sender = nil then begin
    sObj := 'nil@nil';
  end else begin
    sObj := sender.classname+'@'+inttohex(ni(pointer(sender)), sizeof(ni)*2);
  end;
  DebugLog.Log(IntToStr(TThread.CurrentThread.ThreadID)+'::'+s+' ::::'+sObj+':'+datetimetostr(now)+'___'+inttostr(tickcount.GetHighResTicker), sFilter);
end;

procedure Log(sTypeName: string; ptr: pointer; s: string; sFilter: string = '');
var
  sobj: string;
begin
  sObj := sTypeName+'@'+inttohex(ni(ptr), sizeof(ni)*2);
  DebugLog.Log(IntToStr(TThread.CurrentThread.ThreadID)+'::'+s+' ::::'+sObj+':'+datetimetostr(now)+'___'+inttostr(tickcount.GetHighResTicker), sFilter);
end;


{ TTraceLog }

constructor TTraceLog.Create;
begin
  inherited;
  ics(sect);
  slLog := TRingBuffer.create;
//  slLog := TStringlist.create;
{$IFDEF LOG_TRACE_TO_DISK}
  if fileexists(LogFileNAme) then
    DeleteFile(logfilename);

  fs := TFileStream.create(LogFileNAme, fmCreate);

{$ENDIF}

end;

destructor TTraceLog.Destroy;
begin
  if assigned(fs) then
    fs.free;
  slLog.Free;
  dcs(sect);
  inherited;
end;

function TTraceLog.DrainLog: string;
var
  pb: Pbyte;
  i: nativeint;
  b: TBytes;
begin
  lock;
  try
    i := slLog.AvailableDataSize;
    if i = 0 then
      exit;

    setlength(b, i);
    try
      slLog.GetAvailableChunk(@b[0], i);
      result := TEncoding.ANSI.Default.GetString(b, 0, i);
    finally
//      pb.free;
    end;
  finally
    Unlock;
  end;
end;

function TTraceLog.GetFilter: string;
begin
  Lock;
  try
    result := FFilter;
  finally
    Unlock;
  end;
end;

procedure TTraceLog.lock;
begin
  ecs(sect);
end;

procedure TTraceLog.Log(const s, sFilter: string);
var
  sLog: string;
  sLog2: logstring;
  pb: PByte;
  ss: ansistring;
begin
  {$IFDEF LOG_TRACE_TO_CONSOLE}
    Windows.OutpuTTraceString(pchar(s));
  {$ENDIF}
  {$IFDEF LOG_TO_THREAD_STATUS}
  threadlog.Log(s);
  {$ENDIF}
  Lock;
  try
    sLog := inttostr(GetCurrentThreadID)+':'+s;
    if assigned(LogHook) then
      logHook(sLog);
{$IFDEF LOG_TRACE_TO_DISK}
    sLog2 := sLog+NEWLINE;
    pb := GetMemory(length(sLog2));
    try
      movemem32(pb, @sLog2[STRZ], length(sLog2));
      fs.Seek(0, soEnd);
      Stream_GuaranteeWrite(fs, pb, length(sLog2));

    finally
      FreeMemory(pb);
    end;

{$ENDIF}


{$IFDEF LOG_TO_MEMORY}
    ss := s+NEWLINE;
{$IFDEF MSWINDOWS}
    slLog.BufferData(@ss[STRZ], length(ss));
{$ELSE}
    slLog.BufferData(ss.addrof[STRZ], length(ss));
{$ENDIF}

{$ENDIF}

  finally
    Unlock;
  end;


end;

function TTraceLog.LogFileName: string;
begin
  forcedirectories(GEtTempPath);
  result := GEtTempPath+extractfilename(DLLNAme)+'.'+inttostr(GetCurrentTHreadID)+'.trace.txt';

end;

procedure TTraceLog.SetFilter(const Value: string);
begin
  Lock;
  try
    FFilter := value;
  finally
    Unlock;
  end;

end;

procedure TTraceLog.unlock;
begin
  lcs(sect);
end;


procedure TThreadLog.Log(s: string);
begin
  if assigned(thr) then
    thr.Status := s;
end;


procedure oinit;
begin
  DebugLog;
end;
procedure ofinal;
begin
  GTraceLog.free;
  GTraceLog := nil;

end;

procedure ConsoleLog(s: string);
var
  fs: TFileStream;
  ss: ansistring;
  s1,s2: string;
begin
{$IFDEF WINDOWS}
  s2 := s;
  while SplitString(s2, NEWLINE, s1, s2) do begin
    OutputDebugString(pchar(s1));
  end;
  if s1 <> '' then
    OutputDebugString(pchar(s1));

{$ELSE}
{$IFDEF ANDROID}
  LOGI(pointer(pchar(s)));
{$ENDIF}

{$ENDIF}
end;


{ TThreadLog }

initialization
  orderlyinit.init.RegisterProcs('Debug', oinit, ofinal, '');

{$IFDEF CONSOLE_LOG_TRACE_TO_DISK}
  if fileexists('c:\consolelog.txt') then
    deletefile('c:\consolelog.txt');

{$ENDIF}

end.

