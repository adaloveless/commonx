unit BackGroundCommandProcessor;

interface
{$IFDEF NO_DONT_USE_MOVED_TO_COMMANDPROCESSOR}
uses
  orderlyinit, ManagedThread, BackGroundThreads, CommandProcessor,
{$IFNDEF IOS}
  windows,
{$ENDIF}
  sysutils;

var
  BGCmd: TCommandProcessor = nil;
  KillFlag: boolean = false;

{$ENDIF}
implementation
{$IFDEF NO_DONT_USE_MOVED_TO_COMMANDPROCESSOR}
procedure oinit;
begin
  KillFlag := false;
  BGCmd := TCommandProcessor.create(BackgroundThreadMan, 'BackGroundCommandProcessor.BGCmd');
end;

procedure ofinal;
begin
  BGCmd.CancelAll;
  BGCmd.WaitforAll;
  BGCmd.Detach;
  //sleep(100);
  KillFlag := true;


  BGCmd.free;

end;
{$ENDIF}

initialization

{$IFDEF NOPE}
init.RegisterProcs('BackGroundCommandProcessor', oinit, ofinal,'CommandProcessor,ManagedThread');
{$ENDIF}

finalization


end.
