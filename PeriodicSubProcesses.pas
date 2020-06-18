unit PeriodicSubProcesses;

interface

uses
  PeriodicEvents, exe, systemx, sysutils, debug, dir, dirfile, tickcount;

type
  TPeroidicSubProcess = class(TPeriodicCommandEvent)
  public
    Prog: string;
    Params: string;
    Hide: boolean;
    HeartbeatTimeOut: int64;
    KillTaskOntimeout: boolean;
    HeartBeatFile: string;
    procedure DoExecute; override;
    procedure HeartBeatcheck; override;


  end;




implementation

{ TPeroidicSubProcess }



procedure TPeroidicSubProcess.DoExecute;
begin
  inherited;

  if fileexists(dllpath+'kill.flag') then begin
    try
      deletefile(dllpath+'kill.flag');
    except
    end;

    if fileexists(dllpath+'kill.flag') then begin
      Debug.Log('Will not execute sub process because kill.flag was set '+Params);
      exit;
    end;
  end;



  var cEXE := Tcmd_RunExe.Create;
  cmd := cEXE;
  cEXE.Prog := Prog;
  cEXE.Params := Params;
  cEXE.Hide := Hide;
  cEXE.ConsoleRedirect := false;


  cEXE.Start;
end;

procedure TPeroidicSubProcess.HeartBeatcheck;
begin
  inherited;
  if cmd = nil then exit;
  if cmd.IsComplete then
    exit;
  if cmd.IsCancelled then
    exit;
  if heartbeattimeout > 0 then begin

    if gettimesince(Self.LastExecutionTime) > HeartbeatTimeOut then begin
      var fildate := GetFileDate(HEartBeatfile);
      var msAgo := round((now-fildate)*1000*60*60*24);
      if (fildate > 0.0) and (msAgo > HeartbeatTimeOut) then begin
        Debug.Log('Heartbeat expired.  Time to kill : '+Tcmd_RunExe(cmd).hProcessInfo.pi.dwProcessId.tostring);
        exe.KillTaskByID(Tcmd_RunExe(cmd).hProcessInfo.pi.dwProcessId);
      end;
    end;
  end;
end;

end.




