unit scriptfunctions;

interface

uses webscript,orderlyinit;
var
  sf: TScriptFunctions;


implementation


procedure oinit;
begin
  sf := TScriptFunctions.create;

end;

procedure ofinal;
begin
  sf.free;


end;

initialization
  init.RegisterProcs('scriptfunctions', oinit, ofinal);

finalization



end.
