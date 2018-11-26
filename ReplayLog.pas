unit ReplayLog;
{$IFNDEF VER160}{$INLINE AUTO}{$ENDIF}
interface

uses
  AppLock, dir, dirfile, classes, typex, systemx, orderlyinit;

procedure LogReplay(s: string);
function GetReplayLogs(since: TDateTime): string;


procedure LockReplay;
procedure UnlockReplay;
procedure InitReplay;
procedure FreeReplay;

var
  sect_replay: TCLXCriticalSection;

implementation

uses sysutils, stringx;


procedure LogReplay(s: string);
var
  t: textfile;
  sPath, sFile: string;
begin
  if pos('select ', lowercase(s))=1 then
    exit;

  LockReplay;
  try
    sPath := slash(DLLPath)+'ReplayLog\';
    forcedirectories(sPath);
    sFile := sPath+'replay.'+formatdatetime('YYYYMMDD', now)+'.txt';
    assignfile(t, sFile );
    if fileexists(sFile) then
      append(t)
    else
      rewrite(t);

    try
      s := stringreplace(s, #13#10, ' ', [rfReplaceAll]);
      s := stringreplace(s, #13, ' ', [rfReplaceAll]);
      s := stringreplace(s, #10, ' ', [rfReplaceAll]);

      writeln(t, datetimetostr(now)+'~~'+s);
    finally
      closefile(t);
    end;
  finally
    UnlockReplay;
  end;
end;


function GetReplayLogs(since: TDateTime): string;
var
  dir: TDirectory;
  sPath: string;
  t,u: integer;
  sfile: string;
  sSince: string;
  sl, slFile: TStringList;
  sLineDate, sLineQuery: string;
begin
  sl := TStringList.create;
  slFile := TStringList.create;
  try
    sPath := slash(DLLPath)+'ReplayLog\';
    dir := TDirectory.create(sPath, 'replay.*.txt', 0,0);
    try
      sSince := formatDateTime('YYYYMMDD', since);
      for t:= 0 to dir.Filecount-1 do begin
        sFile := dir.files[t].NamePart;
        if sFile >= ('replay.'+sSince) then begin
          slFile.LoadFromFile(dir.files[t].FullName);
          for u := 0 to slFile.count-1 do begin
            if SplitString(slfile[u], '~~', sLineDate, sLineQuery) then begin
              sl.add(sLineQuery);
            end;
          end;

        end;

      end;
    finally
      dir.free;
    end;
  finally
    result := sl.text;
    sl.free;
    slFile.free;
  end;
end;


procedure LockReplay;
begin
  EnterCriticalSection(sect_replay);
end;

procedure UnlockReplay;
begin
  LeaveCriticalSection(sect_replay);
end;

procedure InitReplay;
begin
  InitializeCriticalSection(sect_replay);
end;

procedure FreeReplay;
begin
  DeleteCriticalSection(sect_replay);
end;

procedure oinit;
begin
  InitReplay();

end;

procedure ofinal;
begin
  FreeReplay();


end;

initialization
  init.RegisterProcs('ReplayLog', oinit, ofinal);
finalization


end.
