unit ConsoleLock;

interface

uses
  Windows, orderlyinit;

procedure LockConsole;

procedure UnLockConsole;
var
  sectConsole: _RTL_CRITICAL_SECTION;
implementation

//------------------------------------------------------------------------------
procedure LockConsole;
begin
  EnterCriticalSection(sectConsole);
//  showmessage('console locked');
end;
//------------------------------------------------------------------------------
procedure UnLockConsole;
begin
//  showmessage('console unlocked');
  LeaveCriticalSection(sectConsole);
end;
//------------------------------------------------------------------------------
procedure oinit;
begin
  InitializeCriticalSection(sectConsole);
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
  DeleteCriticalSection(sectConsole);
end;



initialization


init.RegisterProcs('ConsoleLock', oinit, oprefinal, ofinal, olatefinal,'');


end.
