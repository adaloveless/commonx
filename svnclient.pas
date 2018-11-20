unit svnclient;

interface

uses
  tickcount,debug,sysutils, windows, systemx, commandprocessor, dir, exe, stringx, tools, applock;

type
  TSvnCommand = Tcmd_RunExe;

procedure SvnAdd(sFileSpec: string);
procedure ForceSVNDirectory(sPath: string);
procedure SvnCommit(sFilespec: string; sMessage: string; sParams: string = '');
function BeginSvnCommit(p: TCommandProcessor; sFilespec: string; sMessage: string; sParams: string = ''; bAutoStart: boolean = true): TsvnCommand;
function BeginSvnAdd(p: TCommandProcessor; sFilespec: string; sParams: string = ''; bAutoStart: boolean = true): TsvnCommand;
function BeginSvnDelete(p: TCommandProcessor; sFilespec: string; sParams: string = '';bAutoStart: boolean = true): TsvnCommand;
procedure SvnDelete(sFilespec: string);
procedure SvnUpdate(sFilespec: string; sParams: string = '');overload;
procedure SvnUpdate(ss: TConsolecaptureStatus; sFilespec: string; sParams: string = '');overload;

function IsAppOnUNCPath: boolean;
function BeginSvnCommand(p: TCommandProcessor; sCommand: string; sParams: string; bHide: boolean; bAutoStart: boolean = true): TsvnCommand;
function TryEndSvnCommand(h: TsvnCommand): boolean;
function EndSvnCommand(var h: TsvnCommand): boolean;
procedure SvnCommand(sCommand: string; sParams: string; bhide: boolean);
procedure SvnSetIgnores(sFolder: string; sIgnoresFile: string);


function FindSVN: string;

var
  DisableSVN: boolean;
  RelativeSVNPath: string;
  svnLocation: string;
implementation

procedure SvnSetIgnores(sFolder: string; sIgnoresFile: string);
begin
  SvnCommand('propset svn:ignore "'+sFolder+'"','-R --file "'+sIgnoresFile+'"', true);
end;


procedure SvnAdd(sFileSpec: string);
begin
  sFileSpec := unslash(sFileSpec);
  SvnCommand('add "'+sFileSpec+'" --non-interactive', '', false);
end;

function BeginSvnAdd(p: TCommandProcessor; sFilespec: string; sParams: string = ''; bAutoStart: boolean = true): TsvnCommand;
begin
  result := BeginSvnCommand(p, 'add', '"'+sFileSpec+'" --non-interactive', true, bAutostart);
end;

function BeginSvnDelete(p: TCommandProcessor; sFilespec: string; sParams: string = '';bAutoStart: boolean = true): TsvnCommand;
begin
  result := BeginSvnCommand(p, 'delete', '"'+sFileSpec+'" --non-interactive', true, bAutostart);
end;

function BeginSvnCommit(p: TCommandProcessor; sFilespec: string; sMessage: string; sParams: string = ''; bAutoStart: boolean = true): TsvnCommand;
begin
  sFileSpec := unslash(sFileSpec);
  sMessage := StringReplace(sMessage, '"', '', [rfreplaceAll]);
  result := BeginSvnCommand(p, 'commit', '"'+sFilespec+'" -m "'+sMessage+'" --non-interactive'+sParams, false, bAutostart);
end;

procedure SvnCommit(sFilespec: string; sMessage: string; sParams: string = '');
begin
  sFileSpec := unslash(sFileSpec);
  sMessage := StringReplace(sMessage, '"', '', [rfreplaceAll]);
  SvnCommand('commit "'+sFilespec+'"', '-m "'+sMessage+'" --non-interactive'+sParams, false);
end;

procedure SvnDelete(sFilespec: string);
var
  spec,fol: string;
begin
  sFileSpec := unslash(sFileSpec);
  SvnCommand('delete', '"'+sFileSpec+'" --non-interactive --force', false);
  //if its a folder
  if directoryexists(sfileSpec) then begin
    spec := '*.*';
    fol := sFileSpec;
  end else begin
    spec := extractfilename(sFileSpec);
    fol := extractfilepath(sFileSpec);
  end;;

  DeletefileSpec(fol,spec);

end;

procedure SvnUpdate(sFilespec: string; sParams: string = '');
begin
  sfileSpec := unslash(sFileSpec);
  SvnCommand('update "'+sfileSpec+'" --non-interactive', sParams, false);
end;

procedure SvnUpdate(ss: TConsolecaptureStatus; sFilespec: string; sParams: string = '');overload;
begin
  SvnCommand('update "'+sfileSpec+'" --non-interactive', sParams, false);

end;


procedure ForceSVNDirectory(sPath: string);
begin
  sPath := unslash(sPath);
  if not DirectoryExists(sPath) then begin
    forceDirectories(sPath);
  end;
  if not DirectoryExists(slash(sPath)+'.svn') then begin
    SvnCommand('add "'+sPath+'"', '--depth empty', true);
  end;

end;

procedure SvnCommand(sCommand: string; sParams: string; bhide: boolean);
var
  h: TsvnCommand;
  tmStart: cardinal;
  bSaid: boolean;
begin
  if DisableSvn then exit;

  bHide := false;
  h := nil;
  h := BeginSvnCommand(nil, sCommand, sParams, bHide);
//  if h <> INVALID_HANDLE_VALUE then
  begin
    tmStart := GetTicker;

    bSaid := false;
    while not TryEndSvnCommand(h) do begin
      if GetTimeSince(tmStart) > 10000 then begin
        if not bSaid then begin
//          SayNatural('Executing Commands against the repository, please wait.');
          bSaid :=  true;
        end;
      end else begin
        sleep(1);
      end;
    end;
  end;

  if bSaid then begin
//    SayNatural('Done.', true);
  end;

end;

function IsAppOnUNCPath: boolean;
var
  s: string;
begin
  s := DLLPath;
  if length(s) < 2 then
  begin
    raise Exception.create('Bad Path! ' + DLLPath);
  end;

  result := (s[1] = '\') and (s[2] = '\');

end;

function BeginSvnCommand(p: TCommandProcessor; sCommand: string; sParams: string; bHide: boolean; bAutoStart: boolean = true): TsvnCommand;
var
  sfile: string;
  s: string;
  tmStart: cardinal;
  sProg, sP: string;
begin
  result := nil;
  if DisableSvn then exit;
  tmStart := GetTicker();
  try
    bHide := true;
    sProg := findSVN;
    sP := sCommand+' '+sParams;
    result := TsvnCommand.create();
    result.Prog := sProg;
    result.Params := sP;
    result.WorkingDir := DLLPath;
    result.Hide := bHide;
//    result.PipeDebug := true;
    result.CaptureConsoleoutput := true;
    result.MemoryExpense := 1;
    if bautoStart then
      result.Start(p);

    s := LoadStringFromFile(sFile);
    Debug.log(s);
  finally
  end;
  if GetTimeSince(tmStart) = 0 then
    sleep(1);
end;

function TryEndSvnCommand(h: TsvnCommand): boolean;
begin
  result := true;
  if  h = nil then
    exit;
  if DisableSvn then exit;

  result := h.iscomplete;
  if result then begin
    EndSvnCommand(h);

  end;

end;

function EndSvnCommand(var h: TsvnCommand): boolean;
begin
  result := true;
  if h = nil then begin
    exit;
  end;
  if DisableSvn then exit;

  try
    h.waitfor;
    debug.log(h.ConsoleOutput);
  finally
    h.free;
    h := nil;
  end;


end;

function FindSVN: string;
begin
  result := '';
  applock.AL.Lock;
  try
    if svnLocation = '' then
      svnLocation := FindTool('svn.exe', true);

    result := svnLocation;

  finally
    applock.al.Unlock;
  end;
end;

{ Tcmd_SvnOperation }
initialization

DisableSVN := false;
RelativeSVNPath := 'svnbin';
svnLocation := '';

end.
