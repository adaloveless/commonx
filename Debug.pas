unit Debug;
{$I 'DelphiDefs.inc'}

interface
{$IFDEF MSWINDOWS}
  {$DEFINE LOG_TO_DISK}
{$ENDIF}
{$DEFINE NO_US}
{$DEFINE NO_THREADID}
{$IFDEF DESIGN_TIME_PACKAGE}
  //none of this
{$ELSE}
  {$IFDEF WINDOWS}
  {$DEFINE LOG_TO_CONSOLE}
  {$ENDIF}
  {x$DEFINE LOG_TO_DISK}
  {$DEFINE LOG_TO_MEMORY}
  {x$DEFINE CONSOLE_LOG_TO_DISK}
  {x$DEFINE LOG_TO_THREAD_STATUS}
  {x$DEFINE LOG_TO_CTO}
{$ENDIF}
{$IFDEF MOARDEBUG}
  {$UNDEF NO_US}
  {$UNDEF NO_THREADID}
{$ENDIF}

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
{$IFDEF LOG_TO_CTO}
xxxx
//  edi_log_jan,
//  edi_global,
{$ENDIF}
  typex, systemx, sharedobject, sysutils, classes, ringbuffer, stringx, numbers, signals;

type
  TLogTarget = (ltDisk, ltConsole, ltThread, ltEDI);
  TLogTargets = set of TLogTarget;
const
  ltAll = [ltDisk, ltConsole, ltThread, ltEDI];


  //**********************************************
  DISK_LOG_DRAIN_INTERVAL = 1/24;   //<------------------- 1/24th = once per hour
  //**********************************************

type

  TLogHook = procedure(s: string) of object;
{$IFDEF WINDOWS}
  logstring = ansistring;
{$ELSE}
  logstring = string;
{$ENDIF}


  TDebugLog = class(Tobject)
  private
    FLogHook: TLogHook;
    FFilter: string;
    slLog: TRingbuffer;
    sect: TCLXCriticalSection;
{$IFNDEF NO_US}
    iLoggingThread: nativeint;
{$ENDIF}
    fs: TFileStream;
    fsInstance: TFileStream;
    lastArchiveTime: Tdatetime;
    function GetFilter: string;
    procedure SetFilter(const Value: string);
    function LogFileName(bDated: boolean = true): string;
    procedure ArchiveLogDataIfTime;
    procedure ArchiveLogData;
  public
    constructor Create;virtual;
    destructor Destroy;override;
    property LogHook: TLogHook read FLogHook write FLogHook;
    procedure Log(targets: TLogTargets; const s: string; const sFilter: string = '');
    function DrainLog: string;
    property Filter: string read GetFilter write SetFilter;
    procedure lock;inline;
    procedure unlock;inline;

  end;


procedure Log(sender: TObject; s: string; sFilter: string = '');overload;
procedure Log(targets: TLogTargets; sender: TObject; s: string; sFilter: string = '');overload;
procedure Log(s: string; sFilter: string = '');overload;
procedure Log(targets: TLogTargets; s: string; sFilter: string = '');overload;
procedure Log(sTypeName: string; ptr: pointer; s: string; sFilter: string = '');overload;
procedure Log(targets: TLogTargets; sTypeName: string; ptr: pointer; s: string; sFilter: string = '');overload;
procedure ConsoleLog(s: string);

function DebugLog: TDebugLog;


procedure SetDebugThreadVar(thr: TObject);
procedure LogToThreadStatus(s: string);

var
  GDebugLog: TDebugLog = nil;
  log_is_shut_down: boolean;




implementation

uses OrderlyInit, helpers.stream, tickcount,
  managedthread;


type
  TThreadLog = record
    thr: TManagedThread;
    procedure Log(s: string);
  end;

threadvar
  threadlog: TThreadLog;

procedure LogToThreadStatus(s: string);
begin
  threadlog.Log(s);
end;


procedure SetDebugThreadVar(thr: TObject);
begin
  threadlog.thr := TManagedThread(thr);
end;

function DebugLog: TDebugLog;
begin
  if GDebugLog = nil then
    GDebugLog := TDebugLog.create;

  result := GDebugLog;
end;


procedure Log(s: string; sFilter: string = '');overload;
begin
  try
    if log_is_shut_down then exit;
    Log(nil, s,sFilter);
  except
  end;
end;

procedure Log(sender: TObject; s: string; sFilter: string = '');
var
  sobj: string;
begin
  try
    if log_is_shut_down then exit;
    if sender = nil then begin
      sObj := '';
    end else begin
      sObj := sender.classname+'@'+inttohex(ni(pointer(sender)), sizeof(ni)*2)+': ';
    end;
    DebugLog.Log(ltAll,
      DateToStr(Date)+', '+TimeToStr(Now)+': '+sObj+StringReplace(s,NEWLINE,' ',[rfReplaceAll]),
      sFilter
    );
  except
  end;
end;

procedure Log(targets: TLogTargets; sender: TObject; s: string; sFilter: string = '');overload;
var
  sobj: string;
begin
  try
    if log_is_shut_down then exit;
    if sender = nil then begin
      sObj := '';
    end else begin
      sObj := sender.classname+'@'+inttohex(ni(pointer(sender)), sizeof(ni)*2)+': ';
    end;
    DebugLog.Log(targets,
      DateToStr(Date)+', '+TimeToStr(Now)+': '+sObj+StringReplace(s,NEWLINE,' ',[rfReplaceAll]),
      sFilter
    );
  except
  end;
end;


procedure Log(targets: TLogTargets; s: string; sFilter: string = '');overload;
begin
  try
    if log_is_shut_down then exit;
    Log(targets, nil, s,sFilter);
  except
  end;
end;
procedure Log(sTypeName: string; ptr: pointer; s: string; sFilter: string = '');
var
  sobj: string;
begin
  try
    if log_is_shut_down then exit;
    if ptr = nil then
      sObj := ''
    else
      sObj := sTypeName+'@'+inttohex(ni(ptr), sizeof(ni)*2)+': ';
    DebugLog.Log(ltAll,
      DateToStr(Date)+', '+TimeToStr(Now)+': '+sObj+StringReplace(s,NEWLINE,' ',[rfReplaceAll]),
      sFilter
     );
  except
  end;
end;

procedure Log(targets: TLogTargets; sTypeName: string; ptr: pointer; s: string; sFilter: string = '');overload;
var
  sobj: string;
begin
  try
    if log_is_shut_down then exit;
    if ptr = nil then
      sObj := ''
    else
      sObj := sTypeName+'@'+inttohex(ni(ptr), sizeof(ni)*2)+': ';
    DebugLog.Log(targets,
      DateToStr(Date)+', '+TimeToStr(Now)+': '+sObj+StringReplace(s,NEWLINE,' ',[rfReplaceAll]),
      sFilter
     );
  except
  end;
end;


{ TDebugLog }

procedure TDebugLog.ArchiveLogData;
begin
  Lock;
  try
    //copy entire contents of instance stream to dated stream
    fs.Seek(0, soEnd);
    fsInstance.Seek(0,soBeginning);
    Stream_GuaranteeCopy(fsInstance, fs, fsInstance.Size);
    fsInstance.Size := 0;
    lastArchiveTime := now;
  finally
    unlock;
  end;
end;

procedure TDebugLog.ARchiveLogDataIfTime;
begin
  if (now - lastArchiveTime) > DISK_LOG_DRAIN_INTERVAL then begin
    ARchiveLogData;
  end;
end;

constructor TDebugLog.Create;
begin
  inherited;
  ics(sect);
  slLog := TRingBuffer.create;
//  slLog := TStringlist.create;
{$IFDEF LOG_TO_DISK}


{$ENDIF}

end;

destructor TDebugLog.Destroy;
begin
  if assigned(fs) then
    fs.free;
  slLog.Free;
  dcs(sect);
  inherited;
end;

function TDebugLog.DrainLog: string;
var
  i: nativeint;
  b: TBytes;
begin
  result := '';
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

function TDebugLog.GetFilter: string;
begin
  Lock;
  try
    result := FFilter;
  finally
    Unlock;
  end;
end;

procedure TDebugLog.lock;
begin
  ecs(sect);
end;

procedure TDebugLog.Log(targets: TLogTargets; const s: string; const sFilter: string = '');
var
  sLog: string;
  sLog2: logstring;
  pb: PByte;
  ss: ansistring;
  sNewFile: string;
begin
  try
  {$IFDEF LOG_TO_CONSOLE}
  if ltConsole in targets then
    Windows.OutputDebugString(pchar(s));
  {$ENDIF}
  {$IFDEF LOG_TO_THREAD_STATUS}
  if ltThread in targets then
    threadlog.Log(s);
  {$ENDIF}

  Lock;
  try
    sLog := s;
    if assigned(LogHook) then
      logHook(sLog);
{$IFDEF LOG_TO_DISK}
    if ltDisk in targets then begin
      sNewFile := LogFileNAme;


      //if we haven't opened the file or the filename has changed
      if (fs = nil) or (sNewFile <> fs.FileName) then begin
        //ditch the old file
        if (fs <> nil) then begin
          fs.free;
          fs := nil;
        end;

        //?????????????????????
        {$IFDEF FREE_INSTANCE_LOG_AT_MIDNIGHT}
        if (fsInstance <> nil) then begin
          fsInstance.Free;
          fsInstance := nil;
        end;
        {$ENDIF}

        //if the target file already exists, openit for read/write
        if fileexists(sNewFile) then begin
          fs := TFileStream.Create(sNewFile, fmOpenReadWrite+fmShareDenyNone);
        end
        //else create a few one then open it for read/write
        else begin
          fs := nil;

          //create the new file
          try
            fs := TFileStream.create(sNewFile, fmCreate);
          finally
            fs.free;
          end;
          //reopen then new file
          fs := TFileStream.Create(sNewFile, fmOpenReadWrite+fmShareDenyNone);
        end;

      end;

      //Deal with the real-time log (non-dated)
      //create a new one or use existing one
      if fsInstance = nil then begin
        if fileexists(LogFileName(false)) then begin
          fsInstance := TFileStream.Create(LogFileName(false), fmOpenReadWrite+fmShareDenyNone);
        end
        else begin
          //create a new one
          fsInstance := nil;
          try
            fsInstance := TFileStream.create(LogFileName(false), fmCreate);
          finally
            fsInstance.free;
          end;
          fsInstance := TFileStream.Create(LogFileName(false), fmOpenReadWrite+fmShareDenyNone);
        end;
      end;



      //write the stuff to the actual log
      sLog2 := ansistring(sLog)+ansistring(NEWLINE);
      pb := GetMemory(length(sLog2));
      try
        movemem32(pb, @sLog2[STRZ], length(sLog2));

        fsInstance.Seek(0, soEnd);
        Stream_GuaranteeWrite(fsInstance, pb, length(sLog2));

        //flush instance log to dated log if it is time to do so (DISK_LOG_DRAIN_INTERVAL)
        ARchiveLogDataIfTime;

      finally
        FreeMemory(pb);
      end;
    end;
{$ENDIF}

{$IFDEF LOG_TO_CTO}
    if ltEDI in targets then begin
      edi_log_jan.WriteLog(sLog);
    end;
{$ENDIF}


{$IFDEF LOG_TO_MEMORY}
    ss := ansistring(s)+ansistring(NEWLINE);
{$IFDEF MSWINDOWS}
    slLog.BufferData(@ss[STRZ], length(ss));
{$ELSE}
    slLog.BufferData(ss.addrof[STRZ], length(ss));
{$ENDIF}

{$ENDIF}

  finally
    Unlock;
  end;
  except
  end;

end;

function TDebugLog.LogFileName(bDated: boolean = true): string;
var
  sPath: string;
  sDateCode: string;
begin
{$IFDEF LOG_TO_TEMP_FOLDER}
  forcedirectories(GEtTempPath);
  result := GEtTempPath+extractfilename(DLLNAme)+'.'+inttostr(GetCurrentTHreadID)+'.txt';
{$ELSE}
  sPath := DLLPath;
  forcedirectories(sPath);
  sDateCode := FormatDateTime('YYYYMMDD', now);
  if bDated then
    result := sPath+(changefileext(extractfilename(DLLNAme),'.'+sDateCode+'.log'))
  else
    result := sPath+(changefileext(extractfilename(DLLNAme),'.log'));

{$ENDIF}

end;

procedure TDebugLog.SetFilter(const Value: string);
begin
  Lock;
  try
    FFilter := value;
  finally
    Unlock;
  end;

end;

procedure TDebugLog.unlock;
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
  log_is_shut_down := false;
  DebugLog;
  Log('***********************************************************************************');
  Log('****                           APPLICATION STARTUP                             ****');
  Log('***********************************************************************************');


end;
procedure ofinal;
begin
  ///
end;

procedure prefinal;
begin
  ///
end;

procedure latefinal;
begin
  Log('***********************************************************************************');
  Log('****  APPLICATION SHUTDOWN  Logs (if any) are Ignored AFter this point         ****');
  Log('***********************************************************************************');
  GDebugLog.free;
  GDebugLog := nil;
  log_is_shut_down := true;

end;



procedure ConsoleLog(s: string);
var
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
{$IFDEF CONSOLE_LOG_TO_DISK}
  if not fileexists('c:\consolelog.txt') then begin
    fs := TFileStream.create('c:\consolelog.txt', fmCreate);
  end else
    fs := TFileSTream.create('c:\consolelog.txt', fmOpenWrite+fmShareExclusive);

  fs.seek(0, soEnd);
  ss := ansistring(s)+ansichar(#13)+ansichar(#10);
  stream_GuaranteeWrite(fs, @ss[STRZ], length(ss));
  fs.Free;
{$ENDIF}
end;


{ TThreadLog }

initialization
{$IFDEF LOG_TO_CTO}
  orderlyinit.init.RegisterProcs('Debug', oinit, ofinal, 'edi_log_jan');
{$ELSE}
  orderlyinit.init.RegisterProcs('Debug', oinit, prefinal, ofinal, latefinal, '');
{$ENDIF}

{$IFDEF CONSOLE_LOG_TO_DISK}
  if fileexists('c:\consolelog.txt') then
    deletefile('c:\consolelog.txt');

{$ENDIF}

end.

