unit ErrorHandler;
//This unit implements functions for tracking the last error received from the data-tier, and logging errors.

interface
uses Sysutils, Exceptions, Windows, classes, typex, systemx, stringx;

function GetResultCodeForException(e: Exception): integer;
procedure GetErrorLogHTML(slOutput: TStringList);

procedure LogError(sLocation, sMessage: widestring; iCode: integer);
procedure LogErrorToFile(sLocation, sMessage: widestring; iCode: integer);

procedure LockErrorLog;
procedure UnlockErrorLog;


var
  slErrorLog: TStringList;
  sectError: _RTL_CRITICAL_SECTION;




implementation

uses
    {$IFNDEF NOSERVER}
    WebConfig,
    {$ENDIF}
    WebString;



//------------------------------------------------------------------------------
function GetResultCodeForException(e: Exception): integer;
//Checks the class type of the exception passed, and generates an HTTP result code for it
begin
  if e is ETransportError then
    result := 503 //Service Unavailable
  else
    result := 500; //Internal Server Error
end;

//------------------------------------------------------------------------------
procedure LockErrorLog;
//Locks the error log with a critical section so that other threads to not write at the same time.
begin
  EnterCriticalSection(sectError);
end;
//------------------------------------------------------------------------------
procedure UnlockErrorLog;
//UnLocks the error log with a critical section so that other threads may write.
begin
  LeaveCriticalSection(sectError);
end;

//------------------------------------------------------------------------------
procedure LogError(sLocation, sMessage: widestring; iCode: integer);
//Logs an error to the in-memory error log.
begin
  LockErrorLog;
  try
    try
    slErrorLog.add('*'+FormatDateTime('yyyymmdd-hh:nn:ss', now)+inttostr(iCode)+' '+MakeThreadSafe(sLocation));
    slErrorLog.add(MakeThreadSafe(sMessage));

      while slErrorLog.count > 500 do begin
        slErrorLog.delete(0);
      end;
    except
    end;
  finally
    UnLockErrorLog;
  end;
  try
    LogErrorToFile(sLocation, sMessage, iCode);
  except
  end;
end;
//------------------------------------------------------------------------------
procedure LogErrorToFile(sLocation, sMessage: widestring; iCode: integer);
//Logs an error to an error log file.
var
  f: TextFile;
  sfile: ansistring;
  sTemp: ansistring;
begin
  exit;
  LockErrorLog;
  sTemp := MakeThreadSafe(sMessage);

  try
    try
      {$IFNDEF NOSERVER}
      sFile := slash(dllpath+WebServerConfig.LogFileName)+'error.'+formatDateTime('yyyymmdd', now)+'.txt';

      ForceDirectories(extractfilepath(sFile));
      AssignFile(f, sFile);
      if fileexists(sfile) then
        Append(f)
      else
        Rewrite(f);
      try
      writeln(f, '*'+FormatDateTime('yyyymmdd-hh:nn:ss', now)+inttostr(iCode)+' '+MakethreadSafe(sLocation));
      writeln(f, sTemp);
      finally
        CloseFile(f);
      end;
      {$ENDIF}
    except
      Beep(1000,100);
    end;
  finally
    UnLockErrorLog;
  end;
end;

procedure AuditLog(sMessage: ansistring);
//Writes sMessage to the server audit log.
var
  f: TextFile;
  sfile: ansistring;
  sTemp: ansistring;
begin
  sTemp := sMessage;
  LockErrorLog;
  try
    try
      {$IFNDEF NOSERVER}
      sFile := WebServerConfig.LogFileName+'.'+SlashDot(DateToStr(date)+'.txt');
      {$ENDIF}
      AssignFile(f, sFile);
      if fileexists(sfile) then
        Append(f)
      else
        Rewrite(f);
      try
      writeln(f, '*'+FormatDateTime('yyyymmdd-hh:nn:ss', now)+' '+sMessage);
      finally
        CloseFile(f);
      end;
    except
      Beep(1000,100);
    end;
  finally
    UnLockErrorLog;
  end;
end;

procedure GetErrorLogHTML(slOutput: TStringList);
//Returns the in-memory error log (limited to 500 lines) in HTML format for display
//on web pages.
var
  t: integer;
begin
  LockErrorLog;
  try
    for t:= 0 to slErrorLog.count -1 do begin
      if pos('*', slErrorLog[t])>0 then
        slOutput.add('<B>'+slErrorLog[t]+'</B><BR>')
      else
        slOutput.add(slErrorLog[t]+'<BR>');
    end;
  finally
    UnLockErrorLog;
  end;
end;

//------------------------------------------------------------------------------
initialization
  InitializeCriticalSection(sectError);
  LockErrorLog;
  try
    slErrorLog := TStringList.create;
  finally
    UnlockErrorLog;
  end;

//------------------------------------------------------------------------------
finalization
  LockErrorLog;
  try
    slErrorLog.free;
  finally
    UnlockErrorLog;
  end;
  DeleteCriticalSection(secterror);
end.
