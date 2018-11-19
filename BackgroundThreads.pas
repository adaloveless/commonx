unit BackgroundThreads;
//This unit holds a global variable for a singleton instance of TThreadManager
//called BackgroudnThreadMan.  Read BackgroundThreadMan to get the status of
//all BAckground threads running in the PWLN system.  This includes the
//"Janitor" cleanup thread, ERater-retry thread, Session Timeout thread, and
//1-n temporary threads that occasionally appear for fetching accounts from the data-tier(s).
//There are no classes defined here, and there is no significant code.


interface
uses
  Managedthread,
{$IFDEF WINDOWS}
  winapi.windows,
{$ENDIF}
  orderlyinit;

var
  BackgroundThreadMan : TThreadManager;


implementation


procedure oinit;
begin
  BackgroundThreadMan := TThreadManager.create;
end;

procedure ofinal;
begin
  BackGroundthreadMan.free;

end;

initialization
  init.RegisterProcs('BackgroundThreads', oinit, ofinal);


finalization



end.

