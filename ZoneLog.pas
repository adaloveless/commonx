unit ZoneLog;
{x$DEFINE ZONE_LOG}
interface
uses
  typex, systemx, stringx, sysutils, classes, sharedobject, orderlyinit, helpers.stream, debug, queuestream, betterobject;

type
  TZoneLogStream = TUnbufferedFileStream;

  TZoneLog = class(TSharedObject)
  public
    procedure Log(zone: int64; sMessage: string);
  end;


var
  zl: TZoneLog = nil;




implementation

procedure oinit;
begin
  zl := TZoneLog.create;
end;

procedure ofinal;
begin
  zl.free;
  zl := nil;
end;

{ TZoneLog }

procedure TZoneLog.Log(zone: int64; sMessage: string);
var
  spath: string;
  sFile: string;
  fs: TZoneLogStream;
  sLine: ansistring;
begin
{$IFDEF ZONE_LOG}
  Lock;
  try
    sPath := dllpath+'zonelogs\';
    forcedirectories(sPath);
    sLine := datetimetostr(now)+': '+sMessage+CRLF;
    sFile := sPath+inttohex(zone, 16)+'.txt';
    fs := nil;
    try
      if not fileexists(sFile) then begin
        fs := TZoneLogStream.create(sFile, fmCreate);
      end else begin
        fs := TZoneLogStream.create(sFile, fmOpenReadWrite+fmShareExclusive);
      end;
      fs.Seek(0, soEnd);
      stream_GuaranteeWrite(fs, @sLine[STRZ], length(sLine));
      Debug.Log('ZL BB '+zone.tohexstring+': '+sMessage);
    finally
      fs.free;
      fs := nil;
    end;
  finally
    Unlock;
  end;
{$ELSE}
  Debug.Log('ZL BB '+zone.tohexstring+': '+sMessage);
{$ENDIF}
end;

initialization
  init.RegisterProcs('ZoneLog', oinit, ofinal, 'Debug');




end.
