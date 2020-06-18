unit Debug;
{$I 'DelphiDefs.inc'}

interface

{x$DEFINE LOG_TO_IRC}
{$IFDEF MSWINDOWS}
  {$IFNDEF NO_DISK_LOGGING}
    {$DEFINE LOG_TO_DISK}
  {$ELSE}
    {$UNDEF LOG_TO_DISK}
  {$ENDIF}
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
{$IFDEF ANDROID}androidapi.log,{$ENDIF}
{$IFDEF WINDOWS}windows,{$ENDIF}
{$IFDEF LOG_TO_CTO}
xxx
//  edi_log_jan,
//  edi_global,
{$ENDIF}
  typex, systemx, sharedobject, sysutils, classes, ringbuffer, stringx, numbers, signals, commandline;

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
    FHeartBeatOnLog: boolean;
    function GetFilter: string;
    procedure SetFilter(const Value: string);
    function LogFileName(bDated: boolean = true): string;
    function MergedLogFileName(bDated: boolean = true): string;
    procedure ArchiveLogDataIfTime;
    procedure ArchiveLogData;
    procedure WriteToSharedLogEx(sLine, sFile: string;
      out bLogWasCreated: boolean);
  public
    constructor Create;virtual;
    destructor Destroy;override;
    property LogHook: TLogHook read FLogHook write FLogHook;
    procedure Log(targets: TLogTargets; const s: string; const sFilter: string = '');
    function DrainLog: string;
    property Filter: string read GetFilter write SetFilter;
    procedure lock;inline;
    procedure unlock;inline;
    procedure log_to_irc(s, sFilter: string);

    procedure WriteToSharedLog(sLine: string);

    procedure SaveLog(sToFile: string);
    property HeartbeatOnLog: boolean read FHeartBeatOnLog write FHeartBeatOnLog;
    procedure WriteHeartBeat;
  end;


procedure Log(sender: TObject; s: string; sFilter: string = '');overload;
procedure Log(targets: TLogTargets; sender: TObject; s: string; sFilter: string = '');overload;
procedure Log(s: string; sFilter: string = '');overload;
procedure Log(targets: TLogTargets; s: string; sFilter: string = '');overload;
procedure Log(sTypeName: string; ptr: pointer; s: string; sFilter: string = '');overload;
procedure Log(targets: TLogTargets; sTypeName: string; ptr: pointer; s: string; sFilter: string = '');overload;
procedure SaveLog(sToFile:string);
procedure ConsoleLog(s: string);

function DebugLog: TDebugLog;


procedure SetDebugThreadVar(thr: TObject);
procedure LogToThreadStatus(s: string);

{$IFDEF LOG_TO_IRC}
threadvar
  logging : boolean;
{$ENDIF}
var
{$IFDEF LOG_TO_STDOUT}
  logToStdout : boolean = true;
{$ELSE}
  logToStdout : boolean = false;
{$ENDIF}
  g_log_initialized: boolean = false;
  GDebugLog: TDebugLog = nil;
  log_is_shut_down: boolean;




implementation

uses OrderlyInit, helpers_stream, tickcount,
{$IFDEF LOG_TO_IRC}
  irc_monitor, irc_abstract, betterobject, ircconversationd,
{$ENDIF}
{$IFDEF LOG_TO_TOOLBELT}
  ToolBelt_Log,
{$ENDIF}
  managedthread;



{$IFDEF LOG_TO_IRC}
type
  TIRCDebugConversation = class(TIRCConversationDaemon)
  protected
    procedure SendhelpCommands; override;

  public
    function OnCommand(sOriginalLine: string; sCmd: string;
      params: TStringList): Boolean; override;
    procedure Msg(s: string);
  end;

var
  conD: TIRCDebugConversation = nil;

{$ENDIF}


type
  TThreadLog = record
    thr: TManagedThread;
    procedure Log(s: string);
  end;

threadvar
  threadlog: TThreadLog;

procedure SaveLog(sToFile:string);
begin
  GDebugLog.SaveLog(sToFile);
end;

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
{$IFDEF NO_THREADID}
      DateToStr(Date)+', '+TimeToStr(Now)+': '+sObj+StringReplace(s,NEWLINE,' ',[rfReplaceAll]),
{$ELSE}
      GetcurrentThreadid.tostring+':'+DateToStr(Date)+', '+TimeToStr(Now)+': '+sObj+StringReplace(s,NEWLINE,' ',[rfReplaceAll]),
{$ENDIF}
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
{$IFDEF NO_THREADID}
      DateToStr(Date)+', '+TimeToStr(Now)+': '+sObj+StringReplace(s,NEWLINE,' ',[rfReplaceAll]),
{$ELSE}
      GetcurrentThreadid.tostring+':'+DateToStr(Date)+', '+TimeToStr(Now)+': '+sObj+StringReplace(s,NEWLINE,' ',[rfReplaceAll]),
{$ENDIF}
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
{$IFDEF NO_THREADID}
      DateToStr(Date)+', '+TimeToStr(Now)+': '+sObj+StringReplace(s,NEWLINE,' ',[rfReplaceAll]),
{$ELSE}
      GetcurrentThreadid.tostring+':'+DateToStr(Date)+', '+TimeToStr(Now)+': '+sObj+StringReplace(s,NEWLINE,' ',[rfReplaceAll]),
{$ENDIF}
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
{$IFDEF NO_THREADID}
      DateToStr(Date)+', '+TimeToStr(Now)+': '+sObj+StringReplace(s,NEWLINE,' ',[rfReplaceAll]),
{$ELSE}
      GetcurrentThreadid.tostring+':'+DateToStr(Date)+', '+TimeToStr(Now)+': '+sObj+StringReplace(s,NEWLINE,' ',[rfReplaceAll]),
{$ENDIF}
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
{$IFDEF LOG_TO_IRC}
  conD.free;
  conD := nil;
//  irc_monitor.ircmon.EndConversation(IHolder<TIRCConversation>(conversation));
{$ENDIF}
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
  sss: string;
  sNewFile: string;
begin

  try
{$IFDEF LOG_TO_IRC}
  log_to_irc(s, sFilter);
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$IFDEF LOG_TO_CONSOLE}
  if ltConsole in targets then begin
    if length(s) > 256 then
      sss := zcopy(s,0,256)
    else
      sss := s;
    Windows.OutputDebugStringW(pchar(sss));
    if LogToStdout then
      if IsConsole then
        WriteLn(s);
  end;
  {$ENDIF}
{$ENDIF}
{$IFDEF LINUX}
  if ltConsole in targets then begin
    if LogToStdout then
      if IsConsole then
        WriteLn(s);
  end;
{$ENDIF}
  {$IFDEF LOG_TO_THREAD_STATUS}
  if ltThread in targets then
    threadlog.Log(s);
  {$ENDIF}

  Lock;
  try
{$IFNDEF PRESERVE_OLD_PRIMARY_LOG}
    if not g_log_initialized then
    try
      deletefile(changefileext(DLLNAme,'.log'));
    except
    end;
{$ENDIF}
    g_log_initialized := true;
    sLog := s;
    if assigned(LogHook) then
      logHook(sLog);
{$IFDEF LOG_TO_DISK}
    if ltDisk in targets then begin
      {$IFDEF LOG_TO_DISK_SHARED}
      WriteToSharedLog(s);
      if HeartbeatonLog then
        WriteHeartbeat;


      {$ELSE}
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
        //else create a new one then open it for read/write
        else
        begin
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
{$IFDEF PRESERVE_OLD_PRIMARY_LOG}
        if fileexists(LogFileName(false)) then begin
          fsInstance := TFileStream.Create(LogFileName(false), fmOpenReadWrite+fmShareDenyNone);
        end
        else
{$ENDIF}
        begin
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
      {$ENDIF}
    end;
{$ENDIF}

{$IFDEF LOG_TO_CTO}
    if ltEDI in targets then begin
      edi_log_jan.WriteLog(sLog);
    end;
{$ENDIF}

{$IFDEF LOG_TO_TOOLBELT}
    if ltEDI in targets then begin
      ToolBelt_Log.WriteLog(sLog);
    end;
{$ENDIF}


{$IFDEF LOG_TO_MEMORY}
    ss := ansistring(s)+ansistring(NEWLINE);
{$IFDEF NEED_FAKE_ANSISTRING}
    slLog.BufferData(ss.addrof[STRZ], length(ss));

{$ELSE}
    slLog.BufferData(@ss[STRZ], length(ss));
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
  cl: TCommandLine;
begin
  cl.ParseCommandLine();
{$IFDEF LOG_TO_TEMP_FOLDER}
  forcedirectories(GEtTempPath);
  result := GEtTempPath+extractfilename(DLLNAme)+'.'+inttostr(GetCurrentTHreadID)+'.txt';
{$ELSE}
  sPath := DLLPath;
  forcedirectories(sPath);
  sDateCode := FormatDateTime('YYYYMMDD', now);
  var logfileprefix := cl.GetNamedParameterEx('-lfp', '--log-file-prefix', '');
  if bDated then
    result := sPath+(changefileext(logfileprefix+extractfilename(DLLNAme),'.'+sDateCode+'.log'))
  else
    result := sPath+(changefileext(logfileprefix+extractfilename(DLLNAme),'.log'));

{$ENDIF}

end;

procedure TDebugLog.log_to_irc(s, sFilter: string);
begin
{$IFDEF LOG_TO_IRC}
  if logging then exit;

  logging := true;
  try
    if ircmon = nil then
      exit;

    if conD = nil then begin
      if ircmon <> nil then
        conD := TIRCDebugConversation.create(ircmon, '#log');
    end;
    //establish conversations
    if conD <> nil then
      conD.Msg(s);
  finally
    logging := false;
  end;


{$ENDIF}
end;

function TDebugLog.MergedLogFileName(bDated: boolean): string;
begin
{$IFNDEF MOBILE}
  result := changefileext(dllname,'')+'.'+FormatDateTime('YYYYMMDD', now)+'.log';
{$ENDIF}
end;

procedure TDebugLog.SaveLog(sToFile: string);
begin
Lock;
try
  if fsInstance= nil then
    exit;
  var fsOut := TfileStream.create(sToFile, fmCreate);
  try
    fsInstance.Seek(0,soBeginning);
    Stream_GuaranteeCopy(self.fsInstance,fsOut);
    fsInstance.Seek(0,soEnd);

  finally
    fsOut.free;
  end;
finally
  Unlock;
end;
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


procedure TDebugLog.WriteToSharedLogEx(sLine: string; sFile: string; out bLogWasCreated: boolean);
begin

  var strm: TfileStream := nil;

  var tmStart := GetTicker;
  repeat
    try
      //NOTE that at midnight there is a very slight chance that a single
      //log message will be lost

      bLogWasCreated := not fileexists(sFile);
      if not bLogWasCreated {"was" means should-be created in this context} then
        strm := TFileStream.create(sfile, fmopenWrite+fmShareExclusive)
      else
        strm := TFileStream.create(sfile, fmCreate);

      break;
    except
      if gettimesince(tmStart) > 30000 then
        raise Ecritical.create('unable to open log file for more than 30 seconds: '+sFile);
      sleep(random(1000));
    end;
  until false;


  var s: logstring := logstring(sLine+CRLF);
  if s = '' then exit;
  try
    strm.Seek(0, soEnd);
    stream_GuaranteeWrite(strm, @s[STRZ], sizeof(s[STRZ])*length(s));
  finally
    strm.free;
    strm := nil;
  end;

end;

procedure TDebugLog.WriteHeartBeat;
begin
  try
    var cl: TCommandline;
    cl.ParseCommandLine();
    var logfileprefix := cl.GetNamedParameterEx('-lfp', '--log-file-prefix', '');
    SaveStringAsFile(dllpath+slash(LogFilePrefix,'.')+'check.heartbeat','');
  except
  end;
end;

procedure TDebugLog.WriteToSharedLog(sLine: string);
var
  cl: TCommandLine;
begin
  cl.ParseCommandLine();
  var logfileprefix := cl.GetNamedParameterEx('-lfp', '--log-file-prefix', '');


  var sFile := MergedLogFileName;
  var sStandardLogFile := changefileext(dllname,'.log');

  var NewLogFile: boolean := false;

  var sLineMod  := sLine;

  var partialpre :=  '['+zcopy(logfileprefix, 0, 10)+']';
  partialpre := PadString(partialpre, ' ', 12);

{$IFNDEF ALWAYS_PREFIX_LOGS}
  if logfileprefix <> '' then begin
{$ENDIF}
    sLineMod := stringreplace(sLineMod, ': ', ','+partialpre+':', []);
{$IFNDEF ALWAYS_PREFIX_LOGS}
  end;
{$ENDIF}
  WriteToSharedLogEx(sLineMod, sFile, NewLogFile);
  if NewLogFile then begin
    while fileexists(sStandardLogFile) do begin
      try
        deletefile(sStandardLogFile);
      except
        sleep(random(1000));
      end;
    end;
  end;
  WriteToSharedLogEx(sLine, sStandardLogFile, NewLogFile);



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
  Log('****  APPLICATION SHUTDOWN  Logs (if any) are Ignored After this point         ****');
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

{ TIRCDebugConversation }

{$IFDEF LOG_TO_IRC}
procedure TIRCDebugConversation.Msg(s: string);
begin
  var l := conversation.o.LockI;
  conversation.o.PrivMsg(s);
end;

function TIRCDebugConversation.OnCommand(sOriginalLine, sCmd: string;
  params: TStringList): Boolean;
begin
  result := false;
  if not result then
   result := inherited;

end;
{$ENDIF}


{$IFDEF LOG_TO_IRC}
procedure TIRCDebugConversation.SendHelpcommands;
begin
  //inherited;
  Msg('HELP:');
  Msg('  I am a LOG bot.');
  Msg('  Currently there are no commands that I understand.  I just log stuff.');
  Msg('END HELP');
end;
{$ENDIF}

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

