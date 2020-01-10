unit Node;

interface

uses
  betterobject, sharedobject, commandprocessor, exe, dir, dirfile, namedcommandlist, stringx, systemx, debug, tickcount;

type
  Tcmd_NodeBuild = class(TCommand)
  protected
    force: boolean;
    output: string;
    procedure DoExecute; override;
    procedure DoBuild;
  public
    WorkingDir: string;

    constructor Create;override;
    destructor Destroy;override;
    procedure InitExpense; override;
  end;


var
  nodeprojectroot: string = '';
  lastchecked: ticker;




procedure CheckNodeBuild;



implementation

procedure CheckNodeBuild;
var
  c: Tcmd_Nodebuild;
  h : IHolder<TCommand>;
  key: string;
begin
  if gettimesince(lastchecked) < 120000 then
    exit;
  lastchecked := getticker;

  GNC.CommandHoldTime := 8000;
  GNC.CombStaleCommands;
  key := 'node '+nodeprojectroot;
  h := GNC.NeedCommand<Tcmd_NodeBuild>(key);
  c := h.o as Tcmd_NodeBuild;
    if not c.IsComplete then begin
    c.WorkingDir := nodeprojectroot;
    c.Start;
    c.waitfor;
  end;
  GNC.NoNeedCommand(key);




end;

{ Tcmd_NodeBuild }

constructor Tcmd_NodeBuild.Create;
begin
  inherited;
  Debug.Log(self.classname+' created');
end;

destructor Tcmd_NodeBuild.Destroy;
begin
  Debug.Log(self.classname+' destroyed');
  inherited;
end;

procedure Tcmd_NodeBuild.DoBuild;
var
  c: Tcmd_RunExe;
begin
  inherited;
  Debug.Log('Node build start');
  c := Tcmd_RunExe.Create;
  try
//    c.Prog := 'c:\program files\nodejs\npm.cmd';
//    c.Params := 'run "'+slash(workingdir)+'build"';
    c.prog := slash(workingdir)+'npmbuild.exe';
    c.WorkingDir := WorkingDir;
    c.CPUExpense := 0;
    c.batchwrap := true;
    c.hide := false;
//    c.CaptureConsoleoutput := true;
    c.NoThreadvarWait := true;
    c.start;
    c.waitfor;

    try
      SaveStringAsFile(slash(workingdir)+'output.txt', c.ConsoleOutput);
    except
    end;


    Debug.Log('Node build end');
  finally
    c.free;
  end;
end;

procedure Tcmd_NodeBuild.DoExecute;
var
  s,b: TFileinfoRec;
begin
  inherited;

  s := dir.GetNewestFile(slash(workingdir)+'src\', true);
  b := dir.GetNewestFile(slash(workingdir)+'build\', true);
  Debug.Log('s='+s.DebugString);
  Debug.Log('b='+b.DebugString);

  if (s.date>b.date) or force then begin
    DoBuild;
  end;

end;

procedure Tcmd_NodeBuild.InitExpense;
begin
  inherited;
  CPUExpense := 0;
  Resources.SetResourceUsage('node', 1.0);
end;

end.
