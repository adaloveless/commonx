unit globalMultiQueue;

interface

uses
  System.SysUtils, System.Classes, orderlyinit, simplequeue, typex;

type
  TAnonymousIteratorQI = class(TQueueItem)
  protected
    procedure DoExecute; override;
  public
    iteration: ni;
    proc: TProc<ni>;
  end;

  TAnonymousQI = class(TQueueItem)
  protected
    procedure DoExecute; override;
  public
    proc: TProc;
  end;


function InlineProcQI(proc: TProc): TAnonymousQI;
function InlineIteratorProcQI(idx: ni; proc: TProc<ni>): TAnonymousIteratorQI;

var
  gmq: TMultiQueue;

implementation

procedure oinit;
begin
  gmq := TmultiQueue.create;

end;

function InlineIteratorProcQI(idx: ni; proc: TProc<ni>): TAnonymousIteratorQI;
begin
  result := TAnonymousIteratorQI.create;
  result.iteration := idx;
  result.proc := proc;
//  result.CPUExpense := 1.0;
  GMQ.AddItem(result);

end;

function InlineProcQI(proc: TProc): TAnonymousQI;
begin
  result := TAnonymousQI.create;
  result.proc := proc;
  GMQ.AddItem(result);
end;




procedure ofinal;
begin
  gmq.free;
  gmq := nil;
end;

{ TAnonymousIteratorCommand }

procedure TAnonymousIteratorQI.DoExecute;
begin
  inherited;
  proc(iteration);
end;

{ TAnonymousQI }

procedure TAnonymousQI.DoExecute;
begin
  inherited;
  proc();
end;

initialization

gmq := nil;
init.RegisterProcs('globalMultiQueue', oinit, ofinal,'managedthread');

finalization





end.
