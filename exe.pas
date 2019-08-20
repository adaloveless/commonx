unit exe;
// This unit contains EXE spawning and management functions.

//NOTES ABOUt CONSOLE REDIRECTION
//I thought I was insane, but some apps write to stdout some write to
//stderr so this was recently modified to capture both.
//HOWEVER ffmpeg doesn't seem to like being run inside a batch file and
//maybe it detects the scenario and opens its own console window...
//that's the only way I can explain it.
//tested with BATCH files... works fine with echos to stdout and stderr.



{$DEFINE ALLOW_CONSOLE_REDIRECT}
{x$DEFINE NO_INHERIT_HANDLES}
{$DEFINE SEGREGATE_CONSOLE_REDIRECT}

//TODO 1: Figure out why .BATs run with command but not with direct call to RunProgram()

interface

{$IFDEF MSWINDOWS}
uses debug, sysutils, typex, numbers,
  winapi.windows, betterobject,
  managedthread, classes, rtti_helpers,
  commandprocessor, backgroundthreads, commandicons, tickcount,orderlyinit;

const
  NO_RETURN_HANDLE = DWORD(-2);

type
  TConsoleHandles = record
    stdOUTread, stdOUTWrite: THandle;
    stdINread, stdINWrite: THandle;
    stdERRread, stdERRWrite: THandle;
    procedure Init;
  end;
  TBetterProcessInformation = record
    pi: TProcessInformation;
    DynamicFile: string;
    procedure Init;
  end;
type
  EConsoleRedirect = class(Exception);
  TConsoleCaptureStatus = (ccStart, ccProgress, ccEnd);

  TConsoleCaptureHook = procedure(ccStatus: TConsoleCaptureStatus;
    sBufferData: string) of object;

function RunProgramAndWait(const sProg, sParams, sWorkDir: string;
  bHide: boolean = false; batchwrap: boolean = false; bElevate: boolean = false): boolean;
function RunProgram(sProg, sParams, sWkDir: string; bHide: boolean = false;
  batchwrap: boolean = false; bElevate: boolean = false): TBetterProcessInformation; overload;
function RunProgram(var hands: TConsoleHandles;
  sProg, sParams, sWkDir: string; bHide: boolean = false;
  batchwrap: boolean = false; bElevate: boolean = false): TBetterProcessInformation; overload;

procedure WaitForEXE(var hProcessInfo: TBetterProcessInformation; bCloseHandle: boolean = true);
function TryWaitForEXE(var hProcessInfo: TBetterProcessInformation): boolean;
procedure ForgetExe(var hProcessInformation: TBetterProcessInformation);
procedure RunAndCapture(DosApp: string; cc: TConsoleCaptureHook; Timeout: ticker);
function RunExeAndCapture(app: string; params: string = ''; wkdir: string = ''): string;

type
  TWAitForExeThread = class(TProcessorThread)
  private
    FExehandle: TBetterProcessInformation;
  public
    procedure Init;override;
    procedure DoExecute; override;
    property ExeHandle: TBetterProcessInformation read FExehandle write FExehandle;
  end;

  TWaitForExeCommand = class(TCommand)
  private
    FExehandle: THandle;
  public
    hProcessInfo: TBetterProcessInformation;
    hCreatingThread: THandle;
    procedure Init;override;
    procedure InitExpense; override;
    procedure DoExecute; override;
    property ExeHandle: THandle read FExehandle write FExehandle;
  end;

  Tcmd_RunExe = class(TCommand)
  private
    FExehandle: THandle;
    FProg: string;
    FParams: string;
    FWorkingdir: string;
    FHide: boolean;
    FBatchWrap: boolean;
    FPipeDebug: boolean;
    FDebugFile: string;
    FElevate: boolean;
    FConsoleRedirect: boolean;
    FHung: boolean;
    FTimeOut: cardinal;
    FCaptureConsoleOutput: boolean;
    FConsoleOutput: string;
    FConsoleTemp: string;
    FConsoleOutputPointer: Pbyte;
    FIsWindowed: boolean;
    function GetConsoleOutput: string;
    procedure SetIswindowed(const Value: boolean);
    procedure SetProg(const Value: string);
  protected
    hands: TConsoleHandles;
    LAstACtiveTime: cardinal;
    procedure BuildCommandLine;virtual;
    procedure CC(ccStatus: TConsoleCaptureStatus; sData: string);virtual;
    procedure UseConsoleRedirExecutionPath;
  public

    hProcessInfo: TBetterProcessInformation;
    hCreatingThread: THandle;
    procedure Init;override;
    procedure InitExpense; override;
    procedure Preprocess;override;
    procedure DoExecute; override;
    property ExeHandle: THandle read FExehandle write FExehandle;
    property Prog: string read FProg write SetProg;
    property Params: string read FParams write FParams;
    property WorkingDir: string read FWorkingdir write FWorkingdir;
    property Hide: boolean read FHide write FHide;
    property batchwrap: boolean read FBatchWrap write FBatchWrap;
    property PipeDebug: boolean read FPipeDebug write FPipeDebug;
    procedure ConsoleStatus(cc: TConsoleCaptureStatus; s: string);
    property Elevate: boolean read FElevate write FElevate;
    property ConsoleRedirect: boolean read FConsoleRedirect write fConsoleRedirect;
    property Hung: boolean read FHung write FHung;
    procedure CheckIfHungAndTerminate;
    property Timeout: cardinal read FTimeOut write FTimeout;
    property CaptureConsoleoutput: boolean read FConsoleRedirect write FConsoleRedirect;
    property ConsoleOutput: string read GetConsoleOutput;
    property IsWindowed: boolean read FIsWindowed write SetIswindowed;



  end;

function IsTaskRunning(sImageName: string): boolean;

function NumberOfTasksRunning(sImageName: string): nativeint;
function KillTaskByName(sImageName: string; bKillAll: boolean = true; bKillchildTasks: boolean = false): boolean;
procedure KillTaskByID(pid: ni);


var
  ExeCommands: TCommandProcessor;


procedure CreateElevateScripts(sDir: ansistring);

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses
  stringx, systemx;

function NumberOfTasksRunning(sImageName: string): nativeint;
var
  sTasks: string;
  h: IHolder<TStringlist>;
  t: ni;
  sLine: string;
  sExtra, sProg: string;
begin
  result := 0;
  sImageName := lowercase(sImageName);
  sTasks := exe.RunExeAndCapture(getsystemdir+'tasklist.exe');
//  SaveStringAsFile('d:\tasks.txt', stasks);
  h := stringToStringListH(lowercase(sTasks));
  for t:= 0 to h.o.count-1 do begin
    sLine := h.o[t];
    Debug.Log(sLine);
//    if zcopy(sLine, 0,4) = 'nice' then
//      Debug.Log('here');
    if SplitString(sLine, ' ', sProg, sExtra) then begin
      if comparetext(sLine, sImageName)=0 then
        inc(result);
    end;
  end;

end;




function IsTaskRunningLL(sImageName: string): boolean;
var
  sTasks: string;
  h: IHolder<TStringlist>;
  t: ni;
  sLine: string;
begin
  result := false;
  sImageName := lowercase(sImageName);
  sTasks := exe.RunExeAndCapture(getsystemdir+'tasklist.exe');
//  SaveStringAsFile('d:\tasks.txt', stasks);
  h := stringToStringListH(lowercase(sTasks));
  for t:= 0 to h.o.count-1 do begin
    sLine := h.o[t];
    Debug.Log(sLine);
//    if zcopy(sLine, 0,4) = 'nice' then
//      Debug.Log('here');
    if StartsWith(sLine, sImageName) then
      exit(true);
  end;

end;

function IsTaskRunning(sImageName: string): boolean;
var
  t: ni;
begin
  result := false;
  for t:= 0 to 9 do begin
    if IsTaskRunningLL(sImageName) then
      exit(true);
  end;
end;

function KillTaskByName(sImageName: string; bKillAll: boolean = true; bKillchildTasks: boolean = false): boolean;
var
  sTasks: string;
  tflag: string;
begin
  tflag := '';
  if bKillchildTasks then
    tflag := ' /T';
  result := false;
  exe.RunProgramAndWait(getsystemdir+'taskkill.exe', '/IM "'+sImageName+'"'+tflag+' /F', DLLPath, true, false);
  while IsTaskRunning(sImageName) do begin
    Debug.Log('Task '+sImageName+' is still running, retry terminate.');
    exe.RunProgramAndWait(getsystemdir+'taskkill.exe', '/IM "'+sImageName+'"'+tflag+' /F', DLLPath, true, false);
    sleep(2000);
    result := true;
    if not bKillall then
      exit;
  end;
end;

procedure KillTaskByID(pid: ni);
begin
  exe.RunProgramAndWait(getsystemdir+'taskkill.exe', '/IM "'+pid.tostring+'" /T /F', DLLPath, true, false);
end;

procedure CreateElevateScripts(sDir: ansistring);
var
  sl: TStringLIst;
begin
  sl := TStringLIst.create;
  try

    sl.add('@echo off');
    sl.add('');
    sl.add(
      ':: Pass raw command line agruments and first argument to Elevate.vbs');
    sl.add(':: through environment variables.');
    sl.add('set ELEVATE_CMDLINE=%*');
    sl.add('set ELEVATE_APP=%1');
    sl.add('');
    sl.add('start wscript //nologo "%~dpn0.vbs" %*');
    sl.SaveToFile(slash(sDir) + 'elevate.cmd');

    sl.clear;

    sl.add('Set objShell = CreateObject("Shell.Application")');
    sl.add('Set objWshShell = WScript.CreateObject("WScript.Shell")');
    sl.add('Set objWshProcessEnv = objWshShell.Environment("PROCESS")');
    sl.add('');
    sl.add(
      ''' Get raw command line agruments and first argument from Elevate.cmd passed');
    sl.add(''' in through environment variables.');
    sl.add('strCommandLine = objWshProcessEnv("ELEVATE_CMDLINE")');
    sl.add('strApplication = objWshProcessEnv("ELEVATE_APP")');
    sl.add(
      'strArguments = Right(strCommandLine, (Len(strCommandLine) - Len(strApplication)))');
    sl.add('');
    sl.add('If (WScript.Arguments.Count >= 1) Then');
    sl.add('    strFlag = WScript.Arguments(0)');
    sl.add(
      '    If (strFlag = "") OR (strFlag="help") OR (strFlag="/h") OR (strFlag="\h") OR (strFlag="-h") _');
    sl.add(
      '        OR (strFlag = "\?") OR (strFlag = "/?") OR (strFlag = "-?") OR (strFlag="h") _');
    sl.add('        OR (strFlag = "?") Then');
    sl.add('        DisplayUsage');
    sl.add('        WScript.Quit');
    sl.add('    Else');
    sl.add(
      '        objShell.ShellExecute strApplication, strArguments, "", "runas"'
      );
    sl.add('    End If');
    sl.add('Else');
    sl.add('    DisplayUsage');
    sl.add('    WScript.Quit');
    sl.add('End If');
    sl.add('');
    sl.add('');
    sl.add('Sub DisplayUsage');
    sl.add('');
    sl.add(
      '    WScript.Echo "Elevate - Elevation Command Line Tool for Windows Vista" & vbCrLf & _');
    sl.add('                 "" & vbCrLf & _');
    sl.add('                 "Purpose:" & vbCrLf & _');
    sl.add('                 "--------" & vbCrLf & _');
    sl.add(
      '                 "To launch applications that prompt for elevation (i.e. Run as Administrator)" & vbCrLf & _');
    sl.add(
      '                 "from the command line, a script, or the Run box." & vbCrLf & _');
    sl.add('                 "" & vbCrLf & _');
    sl.add('                 "Usage:   " & vbCrLf & _');
    sl.add('                 "" & vbCrLf & _');
    sl.add
      ('                 "    elevate application <arguments>" & vbCrLf & _');
    sl.add('                 "" & vbCrLf & _');
    sl.add('                 "" & vbCrLf & _');
    sl.add('                 "Sample usage:" & vbCrLf & _');
    sl.add('                 "" & vbCrLf & _');
    sl.add(
      '                 "    elevate notepad ""C:\Windows\win.ini""" & vbCrLf & _');
    sl.add('                 "" & vbCrLf & _');
    sl.add(
      '                 "    elevate cmd /k cd ""C:\Program Files""" & vbCrLf & _');
    sl.add('                 "" & vbCrLf & _');
    sl.add(
      '                 "    elevate powershell -NoExit -Command Set-Location ''C:\Windows''" & vbCrLf & _');
    sl.add('                 "" & vbCrLf & _');
    sl.add('                 "" & vbCrLf & _');
    sl.add(
      '                 "Usage with scripts: When using the elevate command with scripts such as" & vbCrLf & _');
    sl.add(
      '                 "Windows Script Host or Windows PowerShell scripts, you should specify" & vbCrLf & _');
    sl.add(
      '                 "the script host executable (i.e., wscript, cscript, powershell) as the " & vbCrLf & _');
    sl.add('                 "application." & vbCrLf & _');
    sl.add('                 "" & vbCrLf & _');
    sl.add('                 "Sample usage with scripts:" & vbCrLf & _');
    sl.add('                 "" & vbCrLf & _');
    sl.add(
      '                 "    elevate wscript ""C:\windows\system32\slmgr.vbs"" â€“dli" & vbCrLf & _');
    sl.add('                 "" & vbCrLf & _');
    sl.add(
      '                 "    elevate powershell -NoExit -Command & ''C:\Temp\Test.ps1''" & vbCrLf & _');
    sl.add('                 "" & vbCrLf & _');
    sl.add('                 "" & vbCrLf & _');
    sl.add(
      '                 "The elevate command consists of the following files:" & vbCrLf & _');
    sl.add('                 "" & vbCrLf & _');
    sl.add('                 "    elevate.cmd" & vbCrLf & _');
    sl.add('                 "    elevate.vbs" & vbCrLf');
    sl.add('');
    sl.add('End Sub');

    if not fileexists(slash(sDir) + 'elevate.vbs') then
      sl.SaveToFile(slash(sDir) + 'elevate.vbs');

  finally
    sl.free;
  end;
end;

// ------------------------------------------------------------------------------
function RunProgramAndWait(const sProg, sParams, sWorkDir: string;
  bHide: boolean = false; batchwrap: boolean = false; bElevate: boolean = false): boolean;
// Runs an Executable program, and waits for the program to complete.
var
  hTemp: TBetterProcessInformation;
begin
  result := false;
  hTemp := RunProgram(sProg, sParams, sWorkDir, bHide, batchwrap, bElevate);
  if hTemp.pi.hProcess <= 0 then begin
    raise Exception.create('Create process failed, result = ' + inttostr(hTemp.pi.hProcess)+' error='+inttostr(getlasterror)
      );
  end;
  if hTemp.pi.hProcess <> 0 then begin
    result := true;
    WaitForEXE(hTemp);
  end;
end;

// ------------------------------------------------------------------------------
function RunProgram(sProg, sParams, sWkDir: string; bHide: boolean = false;
  batchwrap: boolean = false; bElevate: boolean = false): TBetterProcessInformation;
var
  hands: TConsoleHandles;
begin
  hands.Init;
  result := RunProgram(hands, sProg, sParams, sWkDir, bHide,
    batchwrap, bElevate);



end;

function RunProgram(
  var hands: TConsoleHandles;
  sProg, sParams, sWkDir: string; bHide: boolean = false;
  batchwrap: boolean = false; bElevate: boolean = false): TBetterProcessInformation;
// p: sProg: Program Name, include full path
// p: sParams: commandline parameters for the program... basically just concatinates.
// p: sWkDir: change to this directory before executing
// Runs an Executable program, returns a handle to the Executable.
var
  rStartup: _STARTUPINFOW;
  rProcess: TBetterProcessInformation;
  Security: TSecurityAttributes;
  sCommandLine: string;
// sWorkingDir: ansistring;
  sBat: string;
  sLeft, sRight: string;
  sDynamicFile: string;
  cl, cl2, d: PChar;
  bConRedir: boolean;
  sStartup, sSec: string;
begin
  if bElevate then
    BatchWrap := true;

  Debug.Log(nil,sProg + ' ' + sParams);

//  if batchwrap then
//    bHide := true;
  result.Init;
  sBat := '';
  if (lowercase(sProg) <> 'start') then begin

  end;
  FillMem(Pbyte(@rStartup), Sizeof(rStartup), 0);

  FillMem(Pbyte(@Security), Sizeof(TSecurityAttributes), 0);


  bConRedir := (hands.stdOUTREAD <> NO_RETURN_HANDLE)
           and (hands.stdOUTWrite <> NO_RETURN_HANDLE);
  if bConRedir then
    begin
    with Security do begin
      security.nlength := Sizeof(TSecurityAttributes);
      binherithandle := true;
      lpsecuritydescriptor := nil;
    end;

    hands.stdOUTREad := 0;
    hands.stdOUTWrite := 0;
    if not CreatePipe(hands.stdOUTRead, hands.stdOUTWrite, @Security, 0) then
      raise Exception.create('Unable to create pipe');
    if not CreatePipe(hands.stdINRead, hands.stdINWrite, @Security, 0) then
      raise Exception.create('Unable to create pipe');
    if not CreatePipe(hands.stdERRRead, hands.stdERRWrite, @Security, 0) then
      raise Exception.create('Unable to create pipe');



{$IFDEF NO_INHERIT_HANDLES}
    SetHandleInformation(hands.fromexe, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(hands.toexe, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(hands.fromexe, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(hands.toexe, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(hands.fromexe, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(hands.toexe, HANDLE_FLAG_INHERIT, 0);
{$ENDIF}



  end
  else begin
    hands.init;
  end;

  if sParams = '' then
    sCommandLine := Quote(sProg)
  else begin
    sCommandLine := Quote(sProg) + ' ' + sParams;
  end;

  if batchwrap then begin
    if (SplitString(sWkDir, ':', sBat, sRight)) then begin
      sBat := sBat + ':'#13#10;
      sBat := sBat + 'cd \'#13#10;
      sBat := sBat + 'cd "' + sWkDir + '"' + #13#10;
    end;

// if GetWinVersion in [wvWinVista] then begin
    if bElevate then
      sCommandLine := GetTempPath+'elevate.cmd ' + Quote(sCommandLine);


    sBat := sBat + sCommandLine +#13#10;
    //sBat := sBat + 'pause'#13#10;
    // sBat := sBat+'pause'#13#10;

    sDynamicFile := slash(systemx.GetTempPath) + 'dynamic_' + inttostr(GetCurrentThreadID()) + '.bat';



    //if the file still exists... it might be because windows is still
    //holding onto the batch file (it may signal that the program is done before
    //it actually closes the batch file....
    while fileexists(sDynamicFile) do begin
      deletefile(PChar(sDynamicFile));
      //we will get around this by renaming our dynamic file if
      //the delete fails.
      if fileexists(sDynamicFile) then begin
        sDynamicFile := extractfilename(sDynamicFile)+'.'+inttohex(GetTicker,8)+'.bat';
      end;
    end;
    result.DynamicFile := sdynamicFile;

    SaveStringAsFile(sDynamicFile, sBat);
    sCommandLine := sDynamicFile;
// sCommandLine := GetAppDataFolder+extractfilename(dllname)+'.bat';
  end;

  FillChar(rProcess, Sizeof(rProcess), 0);
  FillChar(rStartup, Sizeof(_STARTUPINFOW), 0);
  rStartup.cb := Sizeof(_STARTUPINFOW);
  rStartup.lpReserved := nil;
  rStartup.lpDesktop := nil;
  rStartup.lpTitle := nil;
  rStartup.cbReserved2 := 0;
  rStartup.lpReserved2 := nil;

  if bConRedir then
  begin
    rStartup.hStdOutput := hands.stdOUTwrite;
    rStartup.hStdError := hands.stdERRwrite;
    rStartup.hStdInput := hands.stdINread;
  end;

  if bHide then begin
    rStartup.wShowWindow := SW_HIDE;

    if bConRedir then
      rStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW
    else
      rStartup.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  end
  else begin
    rStartup.dwFlags := 0;
  end;

  if bConRedir then
    begin
// rStartup.dwFlags := STARTF_USESHOWWINDOW;
//    rStartup.dwFlags := rStartup.dwFlags or STARTF_USESTDHANDLES;
// rStartup.wShowWindow := SW_HIDE;

  end;

  try
    // showmessage('about to run '+sCommandLine+' from working dir: '+sWkDir);
    // run this ridiculously bloated API call to launch the Executable

    if sWkDir <> '' then begin
      if (copy(sWkDir, length(sWkDir), 1) = '/') or
        (copy(sWkDir, length(sWkDir), 1) = '\') then begin
        sWkDir := copy(sWkDir, 1, length(sWkDir) - 1);

      end;
      ChDir(sWkDir);
    end;

    if bElevate then
      CreateElevateScripts(GetTempPath);

// showmessage('click ok to comtinue');
    // if copy(sCommandLine,1,1) <> '"' then
    // sCommandLine := Quote(sCommandLine);

   if BatchWrap then
     sCommandLine := GetSystemDir+'cmd.exe /C '+sCommandline;
//     sCommandLine := GetSystemDir+'start.exe '+sCommandline;

    cl := nil;
//    if bElevate then
//      sCommandLine := gettemppath+'elevate.cmd '+sCommandLIne;

    cl := PChar(sProg);
    cl2 := PChar(sCommandLine);
    // showmessage('Prog:'+cl+#13#10+'CL:'+sCommandLine);
    if batchwrap then
      cl := cl2;


    //FillChar(rProcess, Sizeof(rProcess), 0);
    if bConredir then begin
      sStartup := rtti_helpers.RecToDebugSTring(@rStartup, typeinfo(_STARTUPINFOW));
      sSec := rtti_helpers.RecToDebugSTring(@Security, typeinfo(TSecurityAttributes));
{$IFDEF DEBUG_EXES}
      Debug.Log('Create process');
      Debug.Log('--startup: '+sStartup);
      Debug.Log('--security attributes: '+sSec);
{$ENDIF}
      if CreateprocessW(nil, PChar(cl2), @Security, @Security, true,
        8 or NORMAL_PRIORITY_CLASS, nil, nil, rstartup, rProcess.pi)
        then // PAnsiChar(sWorkingdir)
          result := rProcess;

      if integer(result.pi.hProcess) <= 0 then
        raise Exception.create('Create process failed, for commandline '+cl2+' result = ' + inttostr(result.pi.hProcess)+' error='+inttostr(getlasterror)+newline+sProg+' '+sParams);

    end else begin
      sStartup := rtti_helpers.RecToDebugSTring(@rStartup, typeinfo(_STARTUPINFOW));
      sSec := rtti_helpers.RecToDebugSTring(@Security, typeinfo(TSecurityAttributes));
{$IFDEF DEBUG_EXES}
      Debug.Log('Create process');
      Debug.Log('--startup: '+sStartup);
      Debug.Log('--security attributes: '+sSec);
{$ENDIF}
      if CreateprocessW(nil, cl2, @Security, @Security, false,
        NORMAL_PRIORITY_CLASS, nil, nil, rStartup, rProcess.pi)
        then // PAnsiChar(sWorkingdir)
        result := rProcess;
        if integer(result.pi.hProcess) <= 0 then
          raise Exception.create('Create process failed, for commandline '+cl2+' result = ' + inttostr(result.pi.hProcess)+' error='+inttostr(getlasterror)+newline+sProg+' '+sParams);
    end;
    // StrDispose(cl);
  finally
// CloseHandle(ReadPipe);
// CloseHandle(WritePipe);
  end;
  result := rProcess;
end;

// ------------------------------------------------------------------------------
procedure CloseExe(var hProcessInfo: TBetterProcessInformation);
var
  sDynamicFile: string;
begin
  sDynamicFile := '';
  if hprocessinfo.dynamicfile <> '' then begin
    sDynamicFile :=hProcessinfo.DynamicFile;
  end;

  try
    CloseHandle(hProcessInfo.pi.hProcess);
  except
  end;
  try
    CloseHandle(hProcessInfo.pi.hThread);
  except
  end;

  hProcessInfo.pi.hProcess := INVALID_HANDLE_VALUe;
  hProcessInfo.pi.hthread := INVALID_HANDLE_VALUe;

  try
// while fileexists(sDynamicFile) do begin
//
    if sDynamicFile <> '' then
      if fileexists(sDynamicFile) then
        deletefile(PChar(sDynamicFile));

// if  fileexists(sDynamicFile) then
// sleep(1000);
// end;
  except
  end;

end;
// ------------------------------------------------------------------------------
procedure WaitForEXE(var hProcessInfo: TBetterProcessInformation; bCloseHandle: boolean = true);
// p: hProcessHandle: The process handle of the EXE that is running.
// Waits for an EXE represented by hProcessHandle to exit then returns.
begin

  WaitForSingleObject(hProcessinfo.pi.hProcess, INFINITE);
  if bCloseHandle then begin
    CloseExe(hProcessInfo);
  end;

end;
// ------------------------------------------------------------------------------
function TryWaitForEXE(var hProcessInfo: TBetterProcessInformation): boolean;
// p: hProcessHandle: The process handle of the EXE that is running.
// Waits for an EXE represented by hProcessHandle to exit then returns.
var
  sDynamicFile: string;
begin
  result := true;
  if not(WaitForSingleObject(hProcessInfo.pi.hProcess, 1) = WAIT_OBJECT_0) then begin
    result := false;
    exit;
  end;
  sDynamicFile := 'dynamic_' + inttostr(GetCurrentThreadID()) + '.bat';
  sDynamicFile := slash(GetTempPath) + sDynamicFile;
  CloseExe(hProcessInfo);

  try
    if fileexists(sDynamicFile) then
      deletefile(PChar(sDynamicFile));
  except
  end;

end;

{ TWAitForExeThread }

procedure TWAitForExeThread.DoExecute;
begin
  inherited;
  WaitForEXE(FExehandle);




end;

procedure TWAitForExeThread.Init;
begin
  inherited;
  raise Exception.Create('This is obsolete, you should be using a command');
end;

{ TWaitForExeCommand }

procedure TWaitForExeCommand.DoExecute;
var
  sDynamicFile: string;
begin
  inherited;

  if WaitForSingleObject(self.hProcessInfo.pi.hProcess, 1) = WAIT_TIMEOUT then begin
    KillTaskByID(self.hProcessInfo.pi.dwProcessId);
    if WaitForSingleObject(self.hProcessInfo.pi.hProcess, 1) = WAIT_TIMEOUT then begin
      processlater;
      exit;
    end;
  end;

  sDynamicFile := 'dynamic_' + inttostr(hCreatingThread) + '.bat';
  sDynamicFile := slash(GetTempPAth) + sDynamicFile;
  CloseExe(hProcessInfo);

  try
    if fileexists(sDynamicFile) then
      deletefile(PChar(sDynamicFile));
  except
  end;

end;

procedure ForgetExe(var hProcessInformation: TBetterProcessInformation);
var
  c: TWaitForExeCommand;
begin
  c := TWaitForExeCommand.create;

  c.hProcessInfo := hProcessInformation;
  c.hCreatingThread := GetCurrentThreadID();
  c.OwnedByCommandProcessor := true;
  c.process(ExeCommands);

end;

procedure TWaitForExeCommand.Init;
begin
  inherited;
  Icon := @CMD_ICON_EXe;
end;

procedure TWaitForExeCommand.InitExpense;
begin
  inherited;
  CPUExpense := 0;
end;

{ Tcmd_RunExe }

procedure Tcmd_RunExe.BuildCommandLine;
begin
  //overrideable for descendents
end;



procedure Tcmd_RunExe.CC(ccStatus: TConsoleCaptureStatus; sData: string);
var
  t: ni;
begin
  if ccStatus = ccStart then begin
    FconsoleOutput := '';
    FconsoleTemp := '';
  end;
  Debug.Log(sData);
  Status := sData;

  FConsoleTemp := FConsoleTemp + sData;

  if (ccStatus = ccEnd) or (length(FConsoleTemp) > length(FConsoleOutput)) then begin
    FConsoleOutput := FConsoleOutput + FConsoleTemp;
    FconsoleTemp := '';

  end;


end;

procedure Tcmd_RunExe.CheckIfHungAndTerminate;
begin
  if TimeOut = 0 then exit;

  if GEtTimeSince(LastActiveTime)> Timeout then begin
    TerminateProcess(hProcessInfo.pi.hProcess, 666);
    Hung := true;
  end;

end;

procedure Tcmd_RunExe.ConsoleStatus(cc: TConsoleCaptureStatus; s: string);
begin
  case cc of
    ccProgress:
      Status := s;
  end;
end;

procedure Tcmd_RunExe.DoExecute;
const
 ReadBufferSize = 1048576;  // 1 MB Buffer
//  ReadBufferSize = 4096; // 1 MB Buffer
var
  buffer: PAnsichar;
  sl: TStringLIst;
  BytesRead: DWORD;
  iToREad: integer;
  TotalBytesRead, TotalBytesAvail, BytesLeftThisMessage: integer;
  pTemp: PByte;
  iBufferStart, iBufferEnd: nativeint;
  t: integer;
  tmLastAct: ticker;
  hands: TConsoleHandles;
  apprunning: ni;
  consolestring: string;
  waittime: int64;
begin
  inherited;
  waittime := 1;
  BuildCommandLine;
  Debug.ConsoleLog(FProg+' '+FParams);
  Debug.Log(self,FProg+' '+FParams);
  //ConsoleRedirect := false;
// try
  if PipeDebug then
    batchwrap := true;
  FDebugFile := 'dynamic_' + inttostr(GetCurrentThreadID()) + '.debug';


  if PipeDebug then
    Params := Params + '>' + FDebugFile;

  if ConsoleRedirect then begin
{$IFDEF SEGREGATE_CONSOLE_REDIRECT}
    UseConsoleRedirExecutionPath;
    exit;
{$ENDIF}
  end;
  waittime := 1;

  repeat
  {$IFDEF ALLOW_CONSOLE_REDIRECT}
    if not ConsoleRedirect then begin
      hands.init;
    end else begin
      hands.stdINREAD := INVALID_HANDLE_VALUE;
      hands.stdINWRITE := INVALID_HANDLE_VALUE;
    end;
  {$ELSE}

    ReadPipe := NO_RETURN_HANDLE;
    WritePipe := NO_RETURN_HANDLE;
  {$ENDIF}
    TotalBytesRead := 0;

    Name := Prog+' '+Params;


    hung := false;
    hProcessInfo := RunProgram( hands, Prog, Params,
                                  WorkingDir, Hide, batchwrap, Elevate);


    Status := 'Exe Started';
    LastActiveTime := GetTicker;

    // GetMem(buffer,  ReadBufferSize);
//    buffer := GetMemory(ReadBufferSize + 1);
    buffer := AllocMem(ReadBufferSize + 1);

    sl := TStringLIst.create;
    try
  {$IFDEF ALLOW_CONSOLE_REDIRECT}
      if ConsoleRedirect then
      try
        cc(ccStart, '');
        tmLastAct := getticker;
        repeat
          // wait for end of child process
          Apprunning := WaitForSingleObject(hProcessInfo.pi.hProcess, waittime);
          if Apprunning <> WAIT_TIMEOUT then begin
//            Debug.Log('app ended');
          end;

          waittime := lesserof(250, waittime*2);

          // it is important to read from time to time the output information
          // so that the pipe is not blocked by an overflow. New information
          // can be written from the console app to the pipe only if there is
          // enough buffer space.


          if not PeekNamedPipe(hands.stdOUTRead, @buffer[TotalBytesRead], ReadBufferSize,
            @BytesRead, @TotalBytesAvail, @BytesLeftThisMessage) then
            break
          else if BytesRead > 0 then begin
            ReadFile(hands.stdOUTRead, buffer[0], BytesRead, BytesRead, nil);
            setlength(consolestring, BytesRead);
            movemem32(@consolestring[strz], buffer, bytesread);
            consolestring[bytesread] := #0;
            cc(ccProgress, consolestring);
            tmLAstAct := getticker;
            waittime := 1;
            TotalBytesRead := int64(TotalBytesRead) + int64(BytesRead);
          end;

          if not PeekNamedPipe(hands.stdERRRead, @buffer[TotalBytesRead], ReadBufferSize,
            @BytesRead, @TotalBytesAvail, @BytesLeftThisMessage) then
            break
          else if BytesRead > 0 then begin
            ReadFile(hands.stdERRRead, buffer[0], BytesRead, BytesRead, nil);
            setlength(consolestring, BytesRead);
            movemem32(@consolestring[strz], buffer, bytesread);
            consolestring[bytesread] := #0;
            cc(ccProgress, consolestring);
            tmLAstAct := getticker;
            waittime := 1;
            TotalBytesRead := int64(TotalBytesRead) + int64(BytesRead);
          end;

        until (Apprunning <> WAIT_TIMEOUT) or (GetTimeSince(tmLAstAct) > 300000);

        buffer[TotalBytesRead] := #0;
        // OemToChar(Buffer,Buffer);
        cc(ccEnd, '');
      except
        on E:Exception do begin
          Status := 'Conredir failure:'+e.message;
          FileClose(hands.stdOUTREAD);
          FileClose(hands.stdOUTWRITE);
          FileClose(hands.stdINREAD);
          FileClose(hands.stdINWrite);
          FileClose(hands.stdERRREAD);
          FileClose(hands.stdERRWrite);

          hands.init;
          CloseExe(hProcessInfo);

        end;
      end;
  {$ENDIF}

      //final read

      if hProcessInfo.pi.hProcess <> INVALID_HANDLE_VALUE then
        if not hung then WaitForEXE(hProcessInfo, false);

    finally
      if (hands.stdOutRead <> NO_RETURN_HANDLE) and
        (hands.stdOutWrite  <> NO_RETURN_HANDLE) then begin
        try if hands.stdOutRead  <> NO_RETURN_HANDLE then
            closeHandle(hands.stdOutRead);
        except end;
        try if hands.stdInRead  <> NO_RETURN_HANDLE then
            closeHandle(hands.stdInRead);
        except end;
        try if hands.stdErrRead  <> NO_RETURN_HANDLE then
            closeHandle(hands.stdErrRead);
        except end;
        try if hands.stdOutWrite  <> NO_RETURN_HANDLE then
            closeHandle(hands.stdOutWrite);
        except end;
        try if hands.stdInWrite  <> NO_RETURN_HANDLE then
            closeHandle(hands.stdInWrite);
        except end;
        try if hands.stdErrWrite  <> NO_RETURN_HANDLE then
            closeHandle(hands.stdErrWrite);
        except end;

      end;

      CloseEXe(hProcessinfo);
      FreeMemory(buffer);
      sl.free;
    end;

    if PipeDebug and fileexists(FDebugFile) then begin
      Debug.Log(self,Prog + ' ' + Params + #13#10 + '------' + #13#10 +
          loadstringfromfile(FDebugFile));
      deletefile(PChar(FDebugFile));
    end;
  until Not Hung;

// except
// on E: Exception do begin
      // showmessage(e.Message);
// end;
// end;

end;

function Tcmd_RunExe.GetConsoleOutput: string;
begin
  result := Fconsoleoutput;
  if result <> '' then
    exit;
  result := '';
  if FConsoleOutputPOinter = nil then
    exit;

  result := PWideChar(FConsoleOutputPointer);


end;

procedure Tcmd_RunExe.InitExpense;
begin
  inherited;
  CPUExpense := 0;
  consoleRedirect := true;
end;

procedure Tcmd_RunExe.Init;
begin
  inherited;

  Icon := @CMD_ICON_EXE;

end;

procedure Tcmd_RunExe.Preprocess;
begin
  inherited;

end;

procedure Tcmd_RunExe.SetIswindowed(const Value: boolean);
begin
  FIsWindowed := Value;
  ConsoleRedirect := false;
  CaptureConsoleoutput := false;
  batchwrap := false;
end;

procedure Tcmd_RunExe.SetProg(const Value: string);
begin
  FProg := Value;
  Name := ExtractFileName(FProg+' '+FParams);
end;

procedure Tcmd_RunExe.UseConsoleRedirExecutionPath;
begin
  RunAndCapture(self.FProg+' '+self.FParams, cc, Timeout);
end;


procedure RunAndCapture(DosApp: string; cc: TConsoleCaptureHook; Timeout: ticker);
// todo merge these capabilities into the other EXE thing
const
  ReadBufferSize = 1048576; // 1 MB Buffer
var
  Security: TSecurityAttributes;

  StdInRead, StdInWrite: THandle;
  StdOutRead, StdOutWrite: THandle;
  ErRead, ErWrite: THandle;
  start: TStartUpInfo;
  ProcessInfo: TBetterProcessInformation;
  buffer: Pansichar;
  TotalBytesRead, BytesRead: DWORD;
  Apprunning, BytesLeftThisMessage, TotalBytesAvail: integer;
  sStartup, sSEC: string;
  consolestring: ansistring;
  waittime: int64;
  tmLastAct: ticker;
  bExitAfterPipeCheck: boolean;
begin
  bExitAfterPipeCheck := false;
  with Security do begin
    FillChar(Security, Sizeof(TSecurityAttributes), #0);
    nlength := Sizeof(TSecurityAttributes);
    binherithandle := true;
    lpsecuritydescriptor := nil;
  end;

  if CreatePipe(StdOutRead, StdOutWrite, @Security, 0) then begin
    CreatePipe(ErRead, ErWrite, @Security, 0);
    CreatePipe(StdInRead, StdInWrite, @Security, 0);
    // Redirect In- and Output through STARTUPINFO structure
//    SetHandleInformation(ReadPipe, HANDLE_FLAG_INHERIT, 0);
//    SetHandleInformation(WritePipe, HANDLE_FLAG_INHERIT, 0);
//    SetHandleInformation(ErRead, HANDLE_FLAG_INHERIT, 0);
//    SetHandleInformation(ErWrite, HANDLE_FLAG_INHERIT, 0);


    buffer := AllocMem(ReadBufferSize + 1);
    FillChar(start, Sizeof(start), #0);

    start.cb := Sizeof(start);
    start.hStdOutput := StdOutWrite;
    start.hStdError := ErWrite;
    start.hStdInput := StdInRead;
    start.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
    start.wShowWindow := SW_HIDE;


    // Create a Console Child Process with redirected input and output

    UniqueString(DosApp);
    sStartup := rtti_helpers.RecToDebugSTring(@start, typeinfo(_STARTUPINFOW));
    sSec := rtti_helpers.RecToDebugSTring(@Security, typeinfo(TSecurityAttributes));
{$IFDEF DEBUG_EXES}
    Debug.Log('Create process');
    Debug.Log('--startup: '+sStartup);
    Debug.Log('--security attributes: '+sSec);
{$ENDIF}
    if CreateprocessW(nil, PChar(DosApp), @Security, @Security, true,
      8 or NORMAL_PRIORITY_CLASS, nil, nil, start, ProcessInfo.pi) then begin
      TotalBytesRead := 0;
      cc(ccStart, '');
      waittime := 1;
      tmLAstAct := getticker;
      repeat

        // wait for end of child process
        Apprunning := WaitForSingleObject(ProcessInfo.pi.hProcess, waittime);
        waittime := lesserof(waittime * 2, 250);
        if Apprunning <> WAIT_TIMEOUT then begin
          Debug.Log('app ended');
          bExitAfterPipeCheck := true;
        end;
        // it is important to read from time to time the output information
        // so that the pipe is not blocked by an overflow. New information
        // can be written from the console app to the pipe only if there is
        // enough buffer space.


        repeat
          BytesRead := 0;
          if not PeekNamedPipe(STdoutRead, @buffer[0], ReadBufferSize,
            @BytesRead, @TotalBytesAvail, @BytesLeftThisMessage) then
            Debug.Log('Could not peek std pipe')
          else if BytesRead > 0 then begin
            ReadFile(STdoutRead, buffer[0], BytesRead, BytesRead, nil);
            setlength(consolestring, BytesRead);
            movemem32(@consolestring[strz], buffer, bytesread);
            consolestring[bytesread] := #0;
            cc(ccProgress, consolestring);
            tmLastAct := getticker;
            waittime := 1;
          end;
        until BytesRead = 0;
        TotalBytesRead := TotalBytesRead + BytesRead;

        repeat
          BytesRead := 0;
          if not PeekNamedPipe(ErRead, @buffer[0], ReadBufferSize,
            @BytesRead, @TotalBytesAvail, @BytesLeftThisMessage) then
            Debug.Log('Could not peek err pipe')
          else if BytesRead > 0 then begin
            ReadFile(ErRead, buffer[0], BytesRead, BytesRead, nil);
            setlength(consolestring, BytesRead);
            movemem32(@consolestring[strz], buffer, bytesread);
            consolestring[bytesread] := #0;
            cc(ccProgress, consolestring);
            tmLastAct := getticker;
            waittime := 1;
          end;
          TotalBytesRead := TotalBytesRead + BytesRead;
        until BytesRead = 0;


      until (Apprunning <> WAIT_TIMEOUT) or ((timeout > 0) and (gettimesince(tmLAstAct)>TimeOut));

      if ((gettimesince(tmLAstAct)>TimeOut) and (Timeout>0)) then begin
        Debug.Log('cancel because hang/no output');
        KillTaskByID(processinfo.pi.dwProcessID);
      end;

      // OemToChar(Buffer,Buffer);
//      cc(ccProgress, strpas(buffer));
      cc(ccEnd, '');

    end;
    FreeMem(buffer);
    if ProcessInfo.pi.hProcess <> INVALID_HANDLE_VALUE then
      CloseHandle(ProcessInfo.pi.hProcess);
    if ProcessInfo.pi.hThread <> INVALID_HANDLE_VALUE then
      CloseHandle(ProcessInfo.pi.hThread);

    if STdoutRead <> INVALID_HANDLE_VALUE then
      CloseHandle(STdoutRead);
    if STdoutWrite <> INVALID_HANDLE_VALUE then
      CloseHandle(STdoutWrite);

    if STdinRead <> INVALID_HANDLE_VALUE then
      CloseHandle(STdinRead);
    if STdinWrite <> INVALID_HANDLE_VALUE then
      CloseHandle(STdinWrite);

    if ErRead <> INVALID_HANDLE_VALUE then
      CloseHandle(ErRead);
    if ErWrite <> INVALID_HANDLE_VALUE then
      CloseHandle(ErWrite);
  end;
end;

{ TBetterProcessInformation }

procedure TBetterProcessInformation.Init;
begin
  fillmem(pbyte(@pi), sizeof(pi), 0);
  pi.hProcess := INVALID_HANDLE_VALUE;
  pi.hThread := INVALID_HANDLE_VALUE;

  DynamicFile := '';
end;

procedure oinit;
begin
  ExeCommands := TCommandProcessor.create(BackGroundThreadMan, 'ExeCommands');
end;

procedure ofinal;
begin
  ExeCommands.free;
  ExeCommands := nil;


end;

{ TConsoleHandles }

procedure TConsoleHandles.Init;
begin
  stdINread := NO_RETURN_HANDLE;
  stdINread := NO_RETURN_HANDLE;
  stdOUTRead := NO_RETURN_HANDLE;
  stdOUTWrite := NO_RETURN_HANDLE;
  stdERRread := NO_RETURN_HANDLE;
  stdERRWrite := NO_RETURN_HANDLE;

end;

function RunExeAndCapture(app: string; params: string = ''; wkdir: string = ''): string;
var
  c: Tcmd_RunExe;
begin
  c := Tcmd_RunExe.Create;
  try
    c.Prog := app;
    c.Params := params;
    c.CaptureConsoleoutput := true;
    c.WorkingDir := wkdir;
    c.Start;
    c.WaitFor;
    result := c.ConsoleOutput;
  finally
    c.free;
    c := nil;
  end;

end;

initialization
  init.RegisterProcs('exe', oinit, ofinal, 'ManagedThread');



finalization
{$ENDIF}


end.

