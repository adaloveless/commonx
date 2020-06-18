unit RDTPVirtualDiskServerImplib;
{GEN}
{TYPE IMPLIB}
{SERVICENAME VirtualDisk}
{RQFILE RDTPVirtualDiskRQs.txt}
{END}
{$DEFINE DIRECT}
interface

uses
  orderlyinit, lockqueue, typex, RDTPVirtualDiskServer, RDTPSocketServer, windows, videomarshall, virtualdisk_advanced, virtualdisk_status, classes, sysutils, RDTPServerList, virtualdiskparams, betterobject;

type
  TVirtualDiskServer = class(TVirtualDiskServerBase)
  private
  protected
  public
{INTERFACE_START}
    function RQ_GetPayloadConfiguration(iDiskID:integer):PVirtualDiskPayloadConfiguration;overload;override;
    function RQ_ListDisks():TVirtualDiskStatusList;overload;override;
    function RQ_SetPayloadQuota(iDiskID:integer; iFileID:integer; max_size:int64):boolean;overload;override;
    function RQ_AddPayload(iDiskID:integer; sFile:string; max_size:int64; physical:int64; priority:int64; flags:int64):boolean;overload;override;
    function RQ_Decommissionpayload(iDiskID:integer; sFile:string):boolean;overload;override;
    function RQ_SetDefaultPayloadCacheParams(iDiskID:integer; iSegmentSize:int64; iSegmentCount:int64; bReadAhead:boolean):boolean;overload;override;
    function RQ_SetPayloadCacheParams(iDiskID:integer; iFileID:integer; iSegmentSize:int64; iSegmentCount:int64; bReadAhead:boolean):boolean;overload;override;
    function RQ_SetPayloadPriorty(iDiskID:integer; iFileID:integer; priority:int64):boolean;overload;override;
    function RQ_SetPayloadPhysical(iDiskID:integer; iFileID:integer; physical:int64):boolean;overload;override;
    procedure RQ_UnpauseScrubber(iDISKID:integer);overload;override;
    function RQ_ReSourcePayload(iDISKID:integer; iPayloadID:integer; sNewSource:string):boolean;overload;override;
    function RQ_RefunctPayload(iDISKID:integer; iPayLoadID:integer; sNewSource:string):boolean;overload;override;
    procedure RQ_ForceRepair(iDISKID:integer);overload;override;
    function RQ_GetDebugInfo(iDISKID:integer):string;overload;override;
    function RQ_GetRepairLog(iDISKID:integer):string;overload;override;
    function RQ_DrainRepairLog(iDISKID:integer):string;overload;override;
    procedure RQ_ClearRepairLog(iDISKID:integer);overload;override;
    function RQ_SetCachedStripes(iDISKID:integer; value:integer):boolean;overload;override;
    function RQ_GetCachedStripes(iDISKID:integer):integer;overload;override;
    procedure RQ_QuickOnline(iDISKID:integer);overload;override;
    procedure RQ_SetMaxDriveSpan(iDISKID:integer; ival:integer);overload;override;
    procedure RQ_SetMinDriveSpan(iDISKID:integer; ival:integer);overload;override;
    function RQ_NewDisk(di:TNewDiskParams):boolean;overload;override;
    function RQ_VerifyArcZone(iDiskID:integer; zoneidx:int64):boolean;overload;override;
    function RQ_RepairArcZone(iDiskID:integer; zoneidx:int64):boolean;overload;override;
    function RQ_SelfTest(iDiskID:integer; testid:int64):boolean;overload;override;
    function RQ_SetTargetArchive(sArchive:string; sTargetHost:string; sEndPoint:string):string;overload;override;
    function RQ_DeleteDisk(sDiskName:string; DeletePayloads:boolean):boolean;overload;override;
    function RQ_PauseArchive(iDiskID:integer; Pause:boolean):boolean;overload;override;
    function RQ_ResetAndRepairFromTargetArchive(iDiskID:integer):boolean;overload;override;
    function RQ_ResetZone(iDiskID:integer; iZoneID:int64):boolean;overload;override;
    function RQ_ResetDisk(iDiskID:integer):boolean;overload;override;
    function RQ_VerifyAgainstArchive(diskid:integer; zone:int64; out csa:int64; out csb:int64; out difstart:int64):boolean;overload;override;
    function RQ_DumpBigBlock(diskid:integer; bbid:int64):boolean;overload;override;

{INTERFACE_END}
  end;

implementation



{ TVirtualDiskServer }

uses uDM_iSCSI;



{ TVirtualDiskServer }

function TVirtualDiskServer.RQ_AddPayload(iDiskID:integer; sFile:string; max_size:int64; physical:int64; priority:int64; flags:int64):boolean;
var
  t: ni;
  vd: TVirtualDisk;
  il: ILock;
begin
  il := vdh.locki;
  vdh.vdlist[iDiskID].hint_rdtp_waiting := true;
  try
  if iDISKID >= vdh.vdlist.count then
    raise Exception.create('VAT not found.');
  vd := vdh.vdlist[iDISKID];
  il := nil;

  vd.AddPayload(sFile, max_size, physical, priority, flags);

  result := true;
  finally
    vdh.vdlist[iDiskID].hint_rdtp_waiting := false;
  end;

end;

procedure TVirtualDiskServer.RQ_ClearRepairLog(iDISKID: integer);
var
  il: ILock;
begin
  inherited;
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');

    vdh.vdlist[iDISKID].ClearRepairLog;
    //windows.beep(100,100);
  finally
//    vdh.Unlock;
  end;

end;

function TVirtualDiskServer.RQ_Decommissionpayload(iDiskID: integer;
  sFile: string): boolean;
var
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');
    vdh.vdlist[iDISKID].DecommissionPayload(sFile);
  finally
//    vdh.Unlock;
  end;

  result := true;
end;

function TVirtualDiskServer.RQ_DeleteDisk(sDiskName: string;
  DeletePayloads: boolean): boolean;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TVirtualDiskServer.RQ_DrainRepairLog(iDISKID: integer): string;
var
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');

    result := vdh.vdlist[iDISKID].DrainRepairLog;
    //windows.beep(100,100);
  finally
//    vdh.Unlock;
  end;

end;

function TVirtualDiskServer.RQ_DumpBigBlock(diskid: integer;
  bbid: int64): boolean;
var
  il: Ilock;
begin
  il := vdh.locki;
  try
    if DISKID >= vdh.vdlist.count then
      raise Exception.create('vd not found.');

    vdh.vdlist[DISKID].DumpBigBlock(bbid);

  finally
//    vdh.Unlock;
  end;

  result := true;
end;

procedure TVirtualDiskServer.RQ_ForceRepair(iDISKID: integer);
var
  il: Ilock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');

    vdh.vdlist[iDISKID].BeginRepair;

  finally
//    vdh.Unlock;
  end;

end;


function TVirtualDiskServer.RQ_GetCachedStripes(iDISKID: integer): integer;
var
  l: TLock;
  il: ILock;
begin
  result := 0;
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');

    vdh.vdlist[iDISKID].WantLock;//hint to any heavy processes that we want a lock
    l := vdh.vdlist[iDISKID].getlock;
    try
      result := vdh.vdlist[iDISKID].CachedSTripes;
    finally
      vdh.vdlist[iDISKID].unlocklock(l);
    end;

  finally
//    vdh.Unlock;
  end;

end;

function TVirtualDiskServer.RQ_GetDebugInfo(iDISKID: integer): string;
var
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');

    result := vdh.vdlist[iDISKID].DebugVatStructure;
    //windows.beep(100,100);
  finally
//    vdh.Unlock;
  end;

end;

function TVirtualDiskServer.RQ_GetPayloadConfiguration(
  iDiskID: integer): PVirtualDiskPayloadConfiguration;
var
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');
    vdh.vdlist[iDiskID].hint_rdtp_waiting := true;
    result := vdh.vdlist[iDISKID].GetPayloadConfig;
    //windows.beep(100,100);
  finally
    vdh.vdlist[iDiskID].hint_rdtp_waiting := false;
//    vdh.Unlock;
  end;
end;

function TVirtualDiskServer.RQ_GetRepairLog(iDISKID: integer): string;
var
  il: ILock;
begin
  il := vdh.locki;
  try
    vdh.vdlist[iDiskID].hint_rdtp_waiting := true;
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');

    result := vdh.vdlist[iDISKID].GetRepairLog;
    //windows.beep(100,100);
  finally
    vdh.vdlist[iDiskID].hint_rdtp_waiting := false;
  end;

end;

function TVirtualDiskServer.RQ_ListDisks: TVirtualDiskStatusList;
var
  t: ni;
  stat: TVirtualDiskStatus;
  vd: TVirtualDisk;
  l: TLock;
var
  il: ILock;
begin
  if vdh = nil then
    raise ECritical.create('disk hub disappeared unexpectedly!');
  il := vdh.Locki;
  try
    setlength(result, vdh.vdlist.count);
    for t:= 0 to vdh.vdlist.count-1 do begin
      vd := vdh.vdlist[t];
      vd.hint_rdtp_waiting := true;
//      if vd.TryGetLock(l, 60000,1) then
      try
        stat.FileName := vd.filename;
        stat.operational := vd.Operational;
        stat.Status := vd.OperationalStatus;
      finally
        vd.hint_rdtp_waiting := false;
//        vd.Unlocklock(l);
      end; //else begin
//        raise Ecritical.create('failed to get disk lock');
//      end;

      result[t] := stat;
    end;

  finally
//    vdh.Unlock;
  end;
//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TVirtualDiskServer.RQ_NewDisk(di: TNewDiskParams): boolean;
var
  il: ILock;
begin
  il := vdh.locki;
  try
    vdh.NewDisk(di);
  finally
//    vdh.unlock;
  end;
  exit(true);

end;



function TVirtualDiskServer.RQ_PauseArchive(iDiskID: integer;
  Pause: boolean): boolean;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TVirtualDiskServer.RQ_QuickOnline(iDISKID: integer);
var
  vd: TVirtualDisk;
var
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('vd not found.');

    vd := vdh.vdlist[iDISKID];

  finally
//    vdh.Unlock;
  end;

  vd.QuickOnline;

end;

function TVirtualDiskServer.RQ_RefunctPayload(iDISKID: integer; iPayloadID: integer;
  sNewSource: string): boolean;
var
  vd: TVirtualDisk;
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('vd not found.');

    vd := vdh.vdlist[iDISKID];

  finally
//    vdh.Unlock;
  end;

  vd.RefunctPayload(iPayloadID, sNewSource);

  result := true;
end;

function TVirtualDiskServer.RQ_RepairArcZone(iDiskID: integer;
  zoneidx: int64): boolean;
var
  l: Tlock;
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');


    vdh.vdlist[iDISKID].WantLock;//hint to any heavy processes that we want a lock
    l := vdh.vdlist[iDISKID].getlock;
    try
      result := vdh.vdlist[iDISKID].RepairArcZone(zoneidx);
    finally
      vdh.vdlist[iDISKID].unlockLock(l);
    end;

  finally
//    vdh.Unlock;
  end;
end;


function TVirtualDiskServer.RQ_ResetAndRepairFromTargetArchive(
  iDiskID: integer): boolean;
var
  vd: TVirtualDisk;
var
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('vd not found.');

    vd := vdh.vdlist[iDISKID];
    vd.ResetRepair;
  finally
//    vdh.Unlock;
  end;

  vd.ResetRepair;
  exit(true);
end;

function TVirtualDiskServer.RQ_ResetDisk(iDiskID: integer): boolean;
var
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('vd not found.');

    vdh.vdlist[iDISKID].ResetRepair;

  finally
//    vdh.Unlock;
  end;

  result := true;
end;

function TVirtualDiskServer.RQ_ResetZone(iDiskID: integer;
  iZoneID: int64): boolean;
var
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('vd not found.');

//    vdh.vdlist[iDISKID].ResetZone(iZoneID);

  finally
//    vdh.Unlock;
  end;

  result := true;
end;

function TVirtualDiskServer.RQ_ReSourcePayload(iDISKID: integer; iPayloadID: integer;
  sNewSource: string): boolean;
var
  vd: TVirtualDisk;
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('vd not found.');

    vdh.vdlist[iDISKID].ResourcePayload(iPayloadID, sNewSource);

  finally
//    vdh.Unlock;
  end;

  result := true;

end;

function TVirtualDiskServer.RQ_SelfTest(iDiskID: integer;
  testid: int64): boolean;
var
  l: Tlock;
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');


    vdh.vdlist[iDISKID].WantLock;//hint to any heavy processes that we want a lock
    l := vdh.vdlist[iDISKID].getlock;
    try
      vdh.vdlist[iDISKID].SelfTest(testid);
    finally
      vdh.vdlist[iDISKID].unlockLock(l);
    end;

  finally
//    vdh.Unlock;
  end;
  exit(true);
end;

function TVirtualDiskServer.RQ_SetCachedStripes(iDISKID: integer; value: integer): boolean;
var
  l: TLock;
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');


    vdh.vdlist[iDISKID].WantLock;//hint to any heavy processes that we want a lock
    l := vdh.vdlist[iDISKID].getlock;
    try
      vdh.vdlist[iDISKID].DesiredCachedSTripes := value;
    finally
      vdh.vdlist[iDISKID].unlockLock(l);
    end;

  finally
//    vdh.Unlock;
  end;

  result := true;

end;

function TVirtualDiskServer.RQ_SetDefaultPayloadCacheParams(iDiskID: integer;
  iSegmentSize, iSegmentCount: int64; bReadAhead: boolean): boolean;
var
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');


    vdh.vdlist[iDISKID].SetDefaultCacheParams(iSegmentSize, iSegmentCount, bReadAhead);
  finally
//    vdh.Unlock;
  end;

  result := true;
end;


procedure TVirtualDiskServer.RQ_SetMaxDriveSpan(iDISKID, ival: integer);
var
  il: ILock;
begin
  il := vdh.Locki;
  try
    vdh.vdlist[idiskid].vat.MaxDiskSpan := iVal;
  finally
//    vdh.Unlock;
  end;
end;

procedure TVirtualDiskServer.RQ_SetMinDriveSpan(iDISKID, ival: integer);
var
  il: ILock;
begin
  il := vdh.Locki;
  try
    vdh.vdlist[idiskid].vat.MinDiskSpan := iVal;
  finally
//    vdh.Unlock;
  end;

end;

function TVirtualDiskServer.RQ_SetPayloadCacheParams(iDiskID, iFileID: integer;
  iSegmentSize, iSegmentCount: int64; bReadAhead: boolean): boolean;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TVirtualDiskServer.RQ_SetPayloadPhysical(iDiskID, iFileID: integer;
  physical: int64): boolean;
var
  l: TLock;
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');


    vdh.vdlist[iDISKID].WantLock;//hint to any heavy processes that we want a lock
    l := vdh.vdlist[iDISKID].getlock;
    try
      vdh.vdlist[iDISKID].SetPayloadPhysical(iFileId, physical);
    finally
      vdh.vdlist[iDISKID].unlockLock(l);
    end;

  finally
//    vdh.Unlock;
  end;

  result := true;

end;

function TVirtualDiskServer.RQ_SetPayloadPriorty(iDiskID, iFileID: integer;
  priority: int64): boolean;
var
  l: TLock;
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');


    vdh.vdlist[iDISKID].WantLock;//hint to any heavy processes that we want a lock
    l := vdh.vdlist[iDISKID].getlock;
    try
      vdh.vdlist[iDISKID].SetPayloadPriority(iFileId, Priority);
    finally
      vdh.vdlist[iDISKID].unlockLock(l);
    end;

  finally
//    vdh.Unlock;
  end;

  result := true;

end;

function TVirtualDiskServer.RQ_SetPayloadQuota(iDiskID, iFileID: integer;
  max_size: int64): boolean;
var
  l: TLock;
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');


    vdh.vdlist[iDISKID].WantLock;//hint to any heavy processes that we want a lock
    l := vdh.vdlist[iDISKID].getlock;
    try
      vdh.vdlist[iDISKID].SetPayloadQuota(iFileID, max_size);
    finally
      vdh.vdlist[iDISKID].unlockLock(l);
    end;

  finally
//    vdh.Unlock;
  end;

  result := true;

end;


function TVirtualDiskServer.RQ_SetTargetArchive(sArchive, sTargetHost,
  sEndPoint: string): string;
var
  il: ILock;
begin
  il := vdh.Locki;
  try
    //
  finally
//    vdh.Unlock;
  end;
end;

procedure TVirtualDiskServer.RQ_UnpauseScrubber(iDISKID: integer);
var
  l: Tlock;
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');


    vdh.vdlist[iDISKID].WantLock;//hint to any heavy processes that we want a lock
    l := vdh.vdlist[iDISKID].getlock;
    try
      vdh.vdlist[iDISKID].UnpauseScrubber;
    finally
      vdh.vdlist[iDISKID].unlockLock(l);
    end;

  finally
//    vdh.Unlock;
  end;




end;

function TVirtualDiskServer.RQ_VerifyAgainstArchive(diskid:integer; zone:int64; out csa:int64; out csb:int64; out difstart:int64):boolean;
var
  il: ILock;
begin
  il := vdh.locki;
  try
    if DISKID >= vdh.vdlist.count then
      raise Exception.create('vd not found.');

    vdh.vdlist[DISKID].VerifyAgainstArchive(zone, csa,csb, difstart);

  finally
//    vdh.Unlock;
  end;

  result := true;
end;

function TVirtualDiskServer.RQ_VerifyArcZone(iDiskID: integer;
  zoneidx: int64): boolean;
var
  l: Tlock;
  il: ILock;
begin
  il := vdh.locki;
  try
    if iDISKID >= vdh.vdlist.count then
      raise Exception.create('VAT not found.');


    vdh.vdlist[iDISKID].WantLock;//hint to any heavy processes that we want a lock
    l := vdh.vdlist[iDISKID].getlock;
    try
      result := vdh.vdlist[iDISKID].VerifyArcZone(zoneidx);
    finally
      vdh.vdlist[iDISKID].unlockLock(l);
    end;

  finally
//    vdh.Unlock;
  end;
end;

{ TVirtualDiskStatus }

procedure oinit;
begin
//need uses rdtpserverlist;
RDTPServers.RegisterRDTPProcessor('VirtualDisk', TVirtualDiskServer);

end;

procedure ofinal;
begin

//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;


initialization

orderlyinit.init.RegisterProcs('RDTPVirtualDiskServerImplib', oinit, ofinal, 'rdtpserverlist,virtualdisk_advanced');


end.
