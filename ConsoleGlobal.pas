unit ConsoleGlobal;

interface


uses
  ConsoleTools;


var
  con: TConsole;


implementation


initialization
  con := TConsole.create;


finalization
  con.free;
  con := nil;



end.
