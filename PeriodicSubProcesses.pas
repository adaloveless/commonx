unit PeriodicSubProcesses;

interface

uses
  PeriodicEvents, exe;

type
  TPeroidicSubProcess = class(TPeriodicCommandEvent)
  public
    Prog: string;
    Params: string;
    Hide: boolean;
    procedure DoExecute; override;
  end;




implementation

{ TPeroidicSubProcess }



procedure TPeroidicSubProcess.DoExecute;
begin
  inherited;
  var cEXE := Tcmd_RunExe.Create;
  cmd := cEXE;
  cEXE.Prog := Prog;
  cEXE.Params := Params;
  cEXE.Hide := Hide;
  cEXE.ConsoleRedirect := false;
  cEXE.Start;
end;

end.




