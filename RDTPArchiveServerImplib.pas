  unit RDTPArchiveServerImplib;
{GEN}
{TYPE IMPLIB}
{RQFILE RDTPArchiveRQs.txt}
{END}
interface
uses
  lockqueue, classes, systemx, rdtpprocessor, orderlyinit, rdtpserverlist, rdtpArchiveServer,typex, archub, archiver, virtualdiskconstants, namevaluepair, sysutils;
type
  TRDTPArchiveServer = class(TRDTPArchiveServerBase)
  private
  protected
    function TryGetAndLockArchive(sArc: string): TArchiver;
    procedure RaiseBusyError;
  public
{INTERFACE_START}
    function RQ_LogThis(sArchive:string; fromid:int64; toid:int64; startblock:int64; blocklength:int64; data:TDynByteArray):boolean;overload;override;
    function RQ_GetLog(sArchive:string; pin:TDateTime; startblock:int64; blocklength:int64; out data:TDynByteArray):int64;overload;override;
    function RQ_GetNextLogID(sArchive:string; zone:int64; ids_to_reserve:int64):TDateTime;overload;override;
    function RQ_Flush():boolean;overload;override;
    function RQ_GetLogRev(sArchive:string; idx:int64):int64;overload;override;
    function RQ_GetLogRevs(sArchive:string; startidx:int64; count:int64):TDynInt64Array;overload;override;
    function RQ_GetStoredParam(sArchive:string; sParamName:string; sDefault:string):string;overload;override;
    procedure RQ_SetStoredParam(sArchive:string; sParamName:string; sValue:string);overload;override;
    function RQ_ListArchives():string;overload;override;
    function RQ_GetZoneChecksum(sArchive:string; z:int64; out iSum:int64; out iXor:int64):boolean;overload;override;
    function RQ_GetZoneStackReport(sArchive:string; z:int64; fullstack:boolean):string;overload;override;
    procedure RQ_NextZoneHint(sArchive:string; z:int64);overload;override;
    function RQ_GetArcVatCheckSum(sArchive:string; zStart:int64; zCount:int64):int64;overload;override;

{INTERFACE_END}
  end;
implementation
procedure oinit;
begin
  RDTPServers.RegisterRDTPProcessor('Archive', TRDTPArchiveServer);
end;

procedure ofinal;
begin
  //noimp
end;

{ TRDTPArchiveServer }

procedure TRDTPArchiveServer.RaiseBusyError;
begin
  raise ECritical.create('Archiver not found or busy');
end;

function TRDTPArchiveServer.RQ_Flush: boolean;
begin
  inherited;
  result := true;
end;


function TRDTPArchiveServer.RQ_GetArcVatCheckSum(sArchive: string; zStart,zCount: int64): int64;
var
  arc: Tarchiver;
begin
  result := 0;
  arc := garchub.Find(sArchive);
  if arc <> nil then begin
    result := arc.GetArcVatChecksum(zStart, zcount);
  end;

end;

function TRDTPArchiveServer.RQ_GetLog(sArchive:string; pin:TDateTime;
    startblock:int64; blocklength:int64; out data:TDynByteArray):int64;
var
  arc: Tarchiver;
begin
  inherited;
  arc := garchub.Find(sArchive);
  setlength(data, blocklength*blocksize);
  if arc <> nil then begin
    arc.GuaranteeRebuildDAta(startblock, @data[0], blocklength, pin, nil);
  end;
  result := blocklength;

end;


function TRDTPArchiveServer.RQ_GetLogRev(sArchive: string; idx: int64): int64;
var
  arc: Tarchiver;
begin
  inherited;
  arc := garchub.Find(sArchive);
  result := -2;
  if arc <> nil then begin
    result := arc.GetZoneRevision(idx);
  end;
end;

function TRDTPArchiveServer.RQ_GetLogRevs(sArchive: string; startidx,
  count: int64): TDynInt64Array;
var
  arc: Tarchiver;
begin
  result := nil;
  arc := garchub.Find(sArchive);
  if arc <> nil then begin
    result := arc.GetZoneRevisions(startidx,count);
  end;
end;

function TRDTPArchiveServer.RQ_GetNextLogID(sArchive: string; zone: int64; ids_to_reserve:int64): TDateTime;
var
  arc: Tarchiver;
begin
  inherited;
  arc := garchub.Find(sArchive);
  result := -1;
  if arc <> nil then begin
    result := arc.GEtNextLogId(zone, ids_to_reserve);
  end;

end;


function TRDTPArchiveServer.RQ_GetStoredParam(sArchive:string; sParamName:string; sDefault:string):string;
var
  arc: Tarchiver;
  nvpl: TNameValuePairList;
begin
  result := '';
  arc := garchub.Find(sArchive);
  if arc <> nil then begin
    arc.Lock;
    try
      nvpl := arc.NeedParams;
      try
        result := nvpl.GetItemEx(sParamName, sDefault);
      finally
        arc.NoNeedParams(nvpl);
      end;
    finally
      arc.Unlock;
    end;
  end;

end;



function TRDTPArchiveServer.RQ_GetZoneChecksum(sArchive:string; z:int64; out iSum:int64; out iXor:int64):boolean;
var
  arc: Tarchiver;
begin
  inherited;
  arc := garchub.Find(sArchive);
  if arc <> nil then begin
    arc.GetZoneChecksum(z, NULL_PIN, iSum, iXor);
    result := true;
  end;
end;

function TRDTPArchiveServer.RQ_GetZoneStackReport(sArchive: string;
  z: int64; fullstack:boolean): string;
var
  arc: Tarchiver;
begin
  inherited;
  result := '';
  arc := garchub.Find(sArchive);
  if arc <> nil then begin
    result := arc.GetZoneStackReport(z, NULL_PIN, fullstack);
  end;
end;



function TRDTPArchiveServer.RQ_ListArchives: string;
var
  sl: TStringlist;
  l: TLock;
  t: ni;
begin
  l := garchub.GetLock;
  try
    result := '';
    for t:= 0 to garchub.Count-1 do begin
      result := result + garchub.arcs[t].Name+NEWLINE;
    end;
  finally
    garchub.UnlockLock(l);
  end;


end;

function TRDTPArchiveServer.RQ_LogThis(sArchive:string; fromid:int64;  toid:int64; startblock:int64; blocklength:int64; data:TDynByteArray): boolean;
var
  arc: Tarchiver;
  localid: int64;
  actual: int64;
begin
  inherited;
  result := false;
  arc := garchub.Find(sArchive);
  if arc <> nil then begin
    if (blocklength shl 9) <> (length(data)) then
      raise ECritical.create('lengths don''t match '+inttostr(blocklength)+' <> '+inttostr(length(data)));
    actual := 0;
    result := arc.RecordDataEx(startblock, @data[0], blocklength, fromid, toid, actual);

  end;
end;

procedure TRDTPArchiveServer.RQ_NextZoneHint(sArchive: string; z: int64);
var
  arc: Tarchiver;
begin
  inherited;
{$DEFINE ENABLE_HINTS}
{$IFDEF ENABLE_HINTS}
  arc := garchub.Find(sArchive);
  if arc <> nil then begin
    arc.RebuildZone(z, NULL_PIN);
  end;
{$ENDIF}
end;

procedure TRDTPArchiveServer.RQ_SetStoredParam(sArchive, sParamName,
  sValue: string);
var
  arc: Tarchiver;
  nvpl: TNameValuePairList;
begin
  arc := garchub.Find(sArchive);
  if arc <> nil then begin
    arc.Lock;
    try
      nvpl := arc.NeedParams;
      try
        nvpl.AutoAdd := true;
        nvpl.Items[sParamName].Value := sValue;

      finally
        arc.NoNeedParams(nvpl);
      end;
    finally
      arc.Unlock;
    end;
  end;
end;

function TRDTPArchiveServer.TryGetAndLockArchive(sArc: string): TArchiver;
var
  l: tlock;
  a: TArchiver;
begin
  result := nil;
  l := garchub.GetLock;
  try
    a := garchub.Find(sARc);
    if not a.TryLock(7000) then
      exit;
    try
      result := a;

    finally
      a.Unlock;
    end;

  finally
    garchub.unlocklock(l);
  end;
end;

initialization
  init.RegisterProcs('RDTPArchiveServerImplib', oinit, ofinal, 'RDTPServerList');
end.

