unit ConsoleLock;

interface

uses
  orderlyinit, systemx;


procedure LockConsole;

procedure UnLockConsole;
var
  sectConsole: TCLXCriticalSection;
implementation

//------------------------------------------------------------------------------
procedure LockConsole;
begin
  ECS(sectConsole);
//  showmessage('console locked');
end;
//------------------------------------------------------------------------------
procedure UnLockConsole;
begin
//  showmessage('console unlocked');
  LCS(sectConsole);
end;
//------------------------------------------------------------------------------
procedure oinit;
begin
  ICS(sectConsole);
end;
//------------------------------------------------------------------------------
procedure oprefinal;
begin
  ///
end;
procedure ofinal;
begin
  //                     p
end;

procedure olatefinal;
begin
  DCS(sectConsole);
end;



initialization


init.RegisterProcs('ConsoleLock', oinit, oprefinal, ofinal, olatefinal,'');


end.
