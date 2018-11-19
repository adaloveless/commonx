unit globalMultiQueue;

interface

uses
  orderlyinit, simplequeue;

var
  gmq: TMultiQueue;

implementation

procedure oinit;
begin
  gmq := TmultiQueue.create;

end;




procedure ofinal;
begin
  gmq.free;
  gmq := nil;
end;

initialization

gmq := nil;
init.RegisterProcs('globalMultiQueue', oinit, ofinal,'managedthread');

finalization





end.
