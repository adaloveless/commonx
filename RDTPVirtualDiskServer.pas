unit RDTPVirtualDiskServer;
{GEN}
{TYPE SERVER}
{CLASS TVirtualDiskServer}
{ANCESTOR TRDTPProcessor}
{IMPLIB RDTPVirtualDiskServerImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE RDTPVirtualDiskRQs.txt}
{END}
interface


uses
  PacketHelpers_VirtualDisk, VirtualDisk_Advanced, VirtualDisk_Status, Classes, VirtualDiskParams, typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, rdtpprocessorformysql, packethelpers, debug, RDTPServerList;



type
  TVirtualDiskServerBase = class(TRDTPProcessor)
  private
    
    procedure RQ_HANDLE_GetPayloadConfiguration_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_ListDisks(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetPayloadQuota_integer_integer_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_AddPayload_integer_string_int64_int64_int64_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_Decommissionpayload_integer_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetDefaultPayloadCacheParams_integer_int64_int64_boolean(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetPayloadCacheParams_integer_integer_int64_int64_boolean(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetPayloadPriorty_integer_integer_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetPayloadPhysical_integer_integer_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_UnpauseScrubber_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_ReSourcePayload_integer_integer_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_RefunctPayload_integer_integer_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_ForceRepair_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetDebugInfo_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetRepairLog_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_DrainRepairLog_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_ClearRepairLog_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetCachedStripes_integer_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetCachedStripes_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_QuickOnline_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetMaxDriveSpan_integer_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetMinDriveSpan_integer_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_NewDisk_TNewDiskParams(proc: TRDTPProcessor);
    procedure RQ_HANDLE_VerifyArcZone_integer_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_RepairArcZone_integer_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SelfTest_integer_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetTargetArchive_string_string_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_DeleteDisk_string_boolean(proc: TRDTPProcessor);
    procedure RQ_HANDLE_PauseArchive_integer_boolean(proc: TRDTPProcessor);
    procedure RQ_HANDLE_ResetAndRepairFromTargetArchive_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_ResetZone_integer_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_ResetDisk_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_VerifyAgainstArchive_integer_int64_int64_int64_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_DumpBigBlock_integer_int64(proc: TRDTPProcessor);

  protected
    

  public
    constructor Create;override;
    destructor Destroy;override;

    

    
    function RQ_GetPayloadConfiguration(iDiskID:integer):PVirtualDiskPayloadConfiguration;overload;virtual;abstract;
    function RQ_ListDisks():TVirtualDiskStatusList;overload;virtual;abstract;
    function RQ_SetPayloadQuota(iDiskID:integer; iFileID:integer; max_size:int64):boolean;overload;virtual;abstract;
    function RQ_AddPayload(iDiskID:integer; sFile:string; max_size:int64; physical:int64; priority:int64; flags:int64):boolean;overload;virtual;abstract;
    function RQ_Decommissionpayload(iDiskID:integer; sFile:string):boolean;overload;virtual;abstract;
    function RQ_SetDefaultPayloadCacheParams(iDiskID:integer; iSegmentSize:int64; iSegmentCount:int64; bReadAhead:boolean):boolean;overload;virtual;abstract;
    function RQ_SetPayloadCacheParams(iDiskID:integer; iFileID:integer; iSegmentSize:int64; iSegmentCount:int64; bReadAhead:boolean):boolean;overload;virtual;abstract;
    function RQ_SetPayloadPriorty(iDiskID:integer; iFileID:integer; priority:int64):boolean;overload;virtual;abstract;
    function RQ_SetPayloadPhysical(iDiskID:integer; iFileID:integer; physical:int64):boolean;overload;virtual;abstract;
    procedure RQ_UnpauseScrubber(iDISKID:integer);overload;virtual;abstract;
    function RQ_ReSourcePayload(iDISKID:integer; iPayloadID:integer; sNewSource:string):boolean;overload;virtual;abstract;
    function RQ_RefunctPayload(iDISKID:integer; iPayLoadID:integer; sNewSource:string):boolean;overload;virtual;abstract;
    procedure RQ_ForceRepair(iDISKID:integer);overload;virtual;abstract;
    function RQ_GetDebugInfo(iDISKID:integer):string;overload;virtual;abstract;
    function RQ_GetRepairLog(iDISKID:integer):string;overload;virtual;abstract;
    function RQ_DrainRepairLog(iDISKID:integer):string;overload;virtual;abstract;
    procedure RQ_ClearRepairLog(iDISKID:integer);overload;virtual;abstract;
    function RQ_SetCachedStripes(iDISKID:integer; value:integer):boolean;overload;virtual;abstract;
    function RQ_GetCachedStripes(iDISKID:integer):integer;overload;virtual;abstract;
    procedure RQ_QuickOnline(iDISKID:integer);overload;virtual;abstract;
    procedure RQ_SetMaxDriveSpan(iDISKID:integer; ival:integer);overload;virtual;abstract;
    procedure RQ_SetMinDriveSpan(iDISKID:integer; ival:integer);overload;virtual;abstract;
    function RQ_NewDisk(di:TNewDiskParams):boolean;overload;virtual;abstract;
    function RQ_VerifyArcZone(iDiskID:integer; zoneidx:int64):boolean;overload;virtual;abstract;
    function RQ_RepairArcZone(iDiskID:integer; zoneidx:int64):boolean;overload;virtual;abstract;
    function RQ_SelfTest(iDiskID:integer; testid:int64):boolean;overload;virtual;abstract;
    function RQ_SetTargetArchive(sArchive:string; sTargetHost:string; sEndPoint:string):string;overload;virtual;abstract;
    function RQ_DeleteDisk(sDiskName:string; DeletePayloads:boolean):boolean;overload;virtual;abstract;
    function RQ_PauseArchive(iDiskID:integer; Pause:boolean):boolean;overload;virtual;abstract;
    function RQ_ResetAndRepairFromTargetArchive(iDiskID:integer):boolean;overload;virtual;abstract;
    function RQ_ResetZone(iDiskID:integer; iZoneID:int64):boolean;overload;virtual;abstract;
    function RQ_ResetDisk(iDiskID:integer):boolean;overload;virtual;abstract;
    function RQ_VerifyAgainstArchive(diskid:integer; zone:int64; out csa:int64; out csb:int64; out difstart:int64):boolean;overload;virtual;abstract;
    function RQ_DumpBigBlock(diskid:integer; bbid:int64):boolean;overload;virtual;abstract;


    function Dispatch: boolean;override;
  end;

procedure LocalDebug(s: string; sfilter: string = '');

implementation
uses
  RDTPVirtualDiskServerImplib, ImpJunk;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_GetPayloadConfiguration_integer(proc: TRDTPProcessor);
var
  res: PVirtualDiskPayloadConfiguration;
  iDiskID:integer;
begin
  GetintegerFromPacket(proc.request, iDiskID);
  res := RQ_GetPayloadConfiguration(iDiskID);
  WritePVirtualDiskPayloadConfigurationToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_ListDisks(proc: TRDTPProcessor);
var
  res: TVirtualDiskStatusList;
begin
  res := RQ_ListDisks();
  WriteTVirtualDiskStatusListToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_SetPayloadQuota_integer_integer_int64(proc: TRDTPProcessor);
var
  res: boolean;
  iDiskID:integer;
  iFileID:integer;
  max_size:int64;
begin
  GetintegerFromPacket(proc.request, iDiskID);
  GetintegerFromPacket(proc.request, iFileID);
  Getint64FromPacket(proc.request, max_size);
  res := RQ_SetPayloadQuota(iDiskID, iFileID, max_size);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_AddPayload_integer_string_int64_int64_int64_int64(proc: TRDTPProcessor);
var
  res: boolean;
  iDiskID:integer;
  sFile:string;
  max_size:int64;
  physical:int64;
  priority:int64;
  flags:int64;
begin
  GetintegerFromPacket(proc.request, iDiskID);
  GetstringFromPacket(proc.request, sFile);
  Getint64FromPacket(proc.request, max_size);
  Getint64FromPacket(proc.request, physical);
  Getint64FromPacket(proc.request, priority);
  Getint64FromPacket(proc.request, flags);
  res := RQ_AddPayload(iDiskID, sFile, max_size, physical, priority, flags);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_Decommissionpayload_integer_string(proc: TRDTPProcessor);
var
  res: boolean;
  iDiskID:integer;
  sFile:string;
begin
  GetintegerFromPacket(proc.request, iDiskID);
  GetstringFromPacket(proc.request, sFile);
  res := RQ_Decommissionpayload(iDiskID, sFile);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_SetDefaultPayloadCacheParams_integer_int64_int64_boolean(proc: TRDTPProcessor);
var
  res: boolean;
  iDiskID:integer;
  iSegmentSize:int64;
  iSegmentCount:int64;
  bReadAhead:boolean;
begin
  GetintegerFromPacket(proc.request, iDiskID);
  Getint64FromPacket(proc.request, iSegmentSize);
  Getint64FromPacket(proc.request, iSegmentCount);
  GetbooleanFromPacket(proc.request, bReadAhead);
  res := RQ_SetDefaultPayloadCacheParams(iDiskID, iSegmentSize, iSegmentCount, bReadAhead);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_SetPayloadCacheParams_integer_integer_int64_int64_boolean(proc: TRDTPProcessor);
var
  res: boolean;
  iDiskID:integer;
  iFileID:integer;
  iSegmentSize:int64;
  iSegmentCount:int64;
  bReadAhead:boolean;
begin
  GetintegerFromPacket(proc.request, iDiskID);
  GetintegerFromPacket(proc.request, iFileID);
  Getint64FromPacket(proc.request, iSegmentSize);
  Getint64FromPacket(proc.request, iSegmentCount);
  GetbooleanFromPacket(proc.request, bReadAhead);
  res := RQ_SetPayloadCacheParams(iDiskID, iFileID, iSegmentSize, iSegmentCount, bReadAhead);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_SetPayloadPriorty_integer_integer_int64(proc: TRDTPProcessor);
var
  res: boolean;
  iDiskID:integer;
  iFileID:integer;
  priority:int64;
begin
  GetintegerFromPacket(proc.request, iDiskID);
  GetintegerFromPacket(proc.request, iFileID);
  Getint64FromPacket(proc.request, priority);
  res := RQ_SetPayloadPriorty(iDiskID, iFileID, priority);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_SetPayloadPhysical_integer_integer_int64(proc: TRDTPProcessor);
var
  res: boolean;
  iDiskID:integer;
  iFileID:integer;
  physical:int64;
begin
  GetintegerFromPacket(proc.request, iDiskID);
  GetintegerFromPacket(proc.request, iFileID);
  Getint64FromPacket(proc.request, physical);
  res := RQ_SetPayloadPhysical(iDiskID, iFileID, physical);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_UnpauseScrubber_integer(proc: TRDTPProcessor);
var
  iDISKID:integer;
begin
  GetintegerFromPacket(proc.request, iDISKID);
  RQ_UnpauseScrubber(iDISKID);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_ReSourcePayload_integer_integer_string(proc: TRDTPProcessor);
var
  res: boolean;
  iDISKID:integer;
  iPayloadID:integer;
  sNewSource:string;
begin
  GetintegerFromPacket(proc.request, iDISKID);
  GetintegerFromPacket(proc.request, iPayloadID);
  GetstringFromPacket(proc.request, sNewSource);
  res := RQ_ReSourcePayload(iDISKID, iPayloadID, sNewSource);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_RefunctPayload_integer_integer_string(proc: TRDTPProcessor);
var
  res: boolean;
  iDISKID:integer;
  iPayLoadID:integer;
  sNewSource:string;
begin
  GetintegerFromPacket(proc.request, iDISKID);
  GetintegerFromPacket(proc.request, iPayLoadID);
  GetstringFromPacket(proc.request, sNewSource);
  res := RQ_RefunctPayload(iDISKID, iPayLoadID, sNewSource);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_ForceRepair_integer(proc: TRDTPProcessor);
var
  iDISKID:integer;
begin
  GetintegerFromPacket(proc.request, iDISKID);
  RQ_ForceRepair(iDISKID);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_GetDebugInfo_integer(proc: TRDTPProcessor);
var
  res: string;
  iDISKID:integer;
begin
  GetintegerFromPacket(proc.request, iDISKID);
  res := RQ_GetDebugInfo(iDISKID);
  WritestringToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_GetRepairLog_integer(proc: TRDTPProcessor);
var
  res: string;
  iDISKID:integer;
begin
  GetintegerFromPacket(proc.request, iDISKID);
  res := RQ_GetRepairLog(iDISKID);
  WritestringToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_DrainRepairLog_integer(proc: TRDTPProcessor);
var
  res: string;
  iDISKID:integer;
begin
  GetintegerFromPacket(proc.request, iDISKID);
  res := RQ_DrainRepairLog(iDISKID);
  WritestringToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_ClearRepairLog_integer(proc: TRDTPProcessor);
var
  iDISKID:integer;
begin
  GetintegerFromPacket(proc.request, iDISKID);
  RQ_ClearRepairLog(iDISKID);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_SetCachedStripes_integer_integer(proc: TRDTPProcessor);
var
  res: boolean;
  iDISKID:integer;
  value:integer;
begin
  GetintegerFromPacket(proc.request, iDISKID);
  GetintegerFromPacket(proc.request, value);
  res := RQ_SetCachedStripes(iDISKID, value);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_GetCachedStripes_integer(proc: TRDTPProcessor);
var
  res: integer;
  iDISKID:integer;
begin
  GetintegerFromPacket(proc.request, iDISKID);
  res := RQ_GetCachedStripes(iDISKID);
  WriteintegerToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_QuickOnline_integer(proc: TRDTPProcessor);
var
  iDISKID:integer;
begin
  GetintegerFromPacket(proc.request, iDISKID);
  RQ_QuickOnline(iDISKID);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_SetMaxDriveSpan_integer_integer(proc: TRDTPProcessor);
var
  iDISKID:integer;
  ival:integer;
begin
  GetintegerFromPacket(proc.request, iDISKID);
  GetintegerFromPacket(proc.request, ival);
  RQ_SetMaxDriveSpan(iDISKID, ival);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_SetMinDriveSpan_integer_integer(proc: TRDTPProcessor);
var
  iDISKID:integer;
  ival:integer;
begin
  GetintegerFromPacket(proc.request, iDISKID);
  GetintegerFromPacket(proc.request, ival);
  RQ_SetMinDriveSpan(iDISKID, ival);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_NewDisk_TNewDiskParams(proc: TRDTPProcessor);
var
  res: boolean;
  di:TNewDiskParams;
begin
  GetTNewDiskParamsFromPacket(proc.request, di);
  res := RQ_NewDisk(di);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_VerifyArcZone_integer_int64(proc: TRDTPProcessor);
var
  res: boolean;
  iDiskID:integer;
  zoneidx:int64;
begin
  GetintegerFromPacket(proc.request, iDiskID);
  Getint64FromPacket(proc.request, zoneidx);
  res := RQ_VerifyArcZone(iDiskID, zoneidx);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_RepairArcZone_integer_int64(proc: TRDTPProcessor);
var
  res: boolean;
  iDiskID:integer;
  zoneidx:int64;
begin
  GetintegerFromPacket(proc.request, iDiskID);
  Getint64FromPacket(proc.request, zoneidx);
  res := RQ_RepairArcZone(iDiskID, zoneidx);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_SelfTest_integer_int64(proc: TRDTPProcessor);
var
  res: boolean;
  iDiskID:integer;
  testid:int64;
begin
  GetintegerFromPacket(proc.request, iDiskID);
  Getint64FromPacket(proc.request, testid);
  res := RQ_SelfTest(iDiskID, testid);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_SetTargetArchive_string_string_string(proc: TRDTPProcessor);
var
  res: string;
  sArchive:string;
  sTargetHost:string;
  sEndPoint:string;
begin
  GetstringFromPacket(proc.request, sArchive);
  GetstringFromPacket(proc.request, sTargetHost);
  GetstringFromPacket(proc.request, sEndPoint);
  res := RQ_SetTargetArchive(sArchive, sTargetHost, sEndPoint);
  WritestringToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_DeleteDisk_string_boolean(proc: TRDTPProcessor);
var
  res: boolean;
  sDiskName:string;
  DeletePayloads:boolean;
begin
  GetstringFromPacket(proc.request, sDiskName);
  GetbooleanFromPacket(proc.request, DeletePayloads);
  res := RQ_DeleteDisk(sDiskName, DeletePayloads);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_PauseArchive_integer_boolean(proc: TRDTPProcessor);
var
  res: boolean;
  iDiskID:integer;
  Pause:boolean;
begin
  GetintegerFromPacket(proc.request, iDiskID);
  GetbooleanFromPacket(proc.request, Pause);
  res := RQ_PauseArchive(iDiskID, Pause);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_ResetAndRepairFromTargetArchive_integer(proc: TRDTPProcessor);
var
  res: boolean;
  iDiskID:integer;
begin
  GetintegerFromPacket(proc.request, iDiskID);
  res := RQ_ResetAndRepairFromTargetArchive(iDiskID);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_ResetZone_integer_int64(proc: TRDTPProcessor);
var
  res: boolean;
  iDiskID:integer;
  iZoneID:int64;
begin
  GetintegerFromPacket(proc.request, iDiskID);
  Getint64FromPacket(proc.request, iZoneID);
  res := RQ_ResetZone(iDiskID, iZoneID);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_ResetDisk_integer(proc: TRDTPProcessor);
var
  res: boolean;
  iDiskID:integer;
begin
  GetintegerFromPacket(proc.request, iDiskID);
  res := RQ_ResetDisk(iDiskID);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_VerifyAgainstArchive_integer_int64_int64_int64_int64(proc: TRDTPProcessor);
var
  res: boolean;
  diskid:integer;
  zone:int64;
  csa:int64;
  csb:int64;
  difstart:int64;
begin
  GetintegerFromPacket(proc.request, diskid);
  Getint64FromPacket(proc.request, zone);
  res := RQ_VerifyAgainstArchive(diskid, zone, csa, csb, difstart);
  WritebooleanToPacket(proc.response, res);
  Writeint64ToPacket(proc.response, csa);
  Writeint64ToPacket(proc.response, csb);
  Writeint64ToPacket(proc.response, difstart);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVirtualDiskServerBase.RQ_HANDLE_DumpBigBlock_integer_int64(proc: TRDTPProcessor);
var
  res: boolean;
  diskid:integer;
  bbid:int64;
begin
  GetintegerFromPacket(proc.request, diskid);
  Getint64FromPacket(proc.request, bbid);
  res := RQ_DumpBigBlock(diskid, bbid);
  WritebooleanToPacket(proc.response, res);
end;



{ TVirtualDiskServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TVirtualDiskServerBase.create;
begin
  inherited;
  ServiceName := 'VirtualDisk';
end;

destructor TVirtualDiskServerBase.destroy;
begin

  inherited;
end;


function TVirtualDiskServerBase.Dispatch: boolean;
var
  iRQ: integer;
begin

  result := false;

  iRQ := request.data[0];
  request.seqseek(3);
  case iRQ of
    0: begin
        result := true;
//        beeper.Beep(100,100);
       end;
  
    //GetPayloadConfiguration
    $6674:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetPayloadConfiguration','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetPayloadConfiguration_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetPayloadConfiguration','RDTPCALLS');
{$ENDIF}
      end;

    //ListDisks
    $6675:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of ListDisks','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_ListDisks(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of ListDisks','RDTPCALLS');
{$ENDIF}
      end;

    //SetPayloadQuota
    $6676:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetPayloadQuota','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetPayloadQuota_integer_integer_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetPayloadQuota','RDTPCALLS');
{$ENDIF}
      end;

    //AddPayload
    $6677:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of AddPayload','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_AddPayload_integer_string_int64_int64_int64_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of AddPayload','RDTPCALLS');
{$ENDIF}
      end;

    //Decommissionpayload
    $6678:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Decommissionpayload','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Decommissionpayload_integer_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Decommissionpayload','RDTPCALLS');
{$ENDIF}
      end;

    //SetDefaultPayloadCacheParams
    $6679:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetDefaultPayloadCacheParams','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetDefaultPayloadCacheParams_integer_int64_int64_boolean(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetDefaultPayloadCacheParams','RDTPCALLS');
{$ENDIF}
      end;

    //SetPayloadCacheParams
    $6680:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetPayloadCacheParams','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetPayloadCacheParams_integer_integer_int64_int64_boolean(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetPayloadCacheParams','RDTPCALLS');
{$ENDIF}
      end;

    //SetPayloadPriorty
    $6681:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetPayloadPriorty','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetPayloadPriorty_integer_integer_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetPayloadPriorty','RDTPCALLS');
{$ENDIF}
      end;

    //SetPayloadPhysical
    $6682:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetPayloadPhysical','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetPayloadPhysical_integer_integer_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetPayloadPhysical','RDTPCALLS');
{$ENDIF}
      end;

    //UnpauseScrubber
    $6683:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of UnpauseScrubber','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_UnpauseScrubber_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of UnpauseScrubber','RDTPCALLS');
{$ENDIF}
      end;

    //ReSourcePayload
    $6684:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of ReSourcePayload','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_ReSourcePayload_integer_integer_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of ReSourcePayload','RDTPCALLS');
{$ENDIF}
      end;

    //RefunctPayload
    $6685:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of RefunctPayload','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_RefunctPayload_integer_integer_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of RefunctPayload','RDTPCALLS');
{$ENDIF}
      end;

    //ForceRepair
    $6686:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of ForceRepair','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_ForceRepair_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of ForceRepair','RDTPCALLS');
{$ENDIF}
      end;

    //GetDebugInfo
    $6687:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetDebugInfo','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetDebugInfo_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetDebugInfo','RDTPCALLS');
{$ENDIF}
      end;

    //GetRepairLog
    $6688:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetRepairLog','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetRepairLog_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetRepairLog','RDTPCALLS');
{$ENDIF}
      end;

    //DrainRepairLog
    $6689:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of DrainRepairLog','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_DrainRepairLog_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of DrainRepairLog','RDTPCALLS');
{$ENDIF}
      end;

    //ClearRepairLog
    $6690:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of ClearRepairLog','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_ClearRepairLog_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of ClearRepairLog','RDTPCALLS');
{$ENDIF}
      end;

    //SetCachedStripes
    $6691:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetCachedStripes','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetCachedStripes_integer_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetCachedStripes','RDTPCALLS');
{$ENDIF}
      end;

    //GetCachedStripes
    $6692:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetCachedStripes','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetCachedStripes_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetCachedStripes','RDTPCALLS');
{$ENDIF}
      end;

    //QuickOnline
    $6693:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of QuickOnline','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_QuickOnline_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of QuickOnline','RDTPCALLS');
{$ENDIF}
      end;

    //SetMaxDriveSpan
    $6694:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetMaxDriveSpan','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetMaxDriveSpan_integer_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetMaxDriveSpan','RDTPCALLS');
{$ENDIF}
      end;

    //SetMinDriveSpan
    $6695:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetMinDriveSpan','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetMinDriveSpan_integer_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetMinDriveSpan','RDTPCALLS');
{$ENDIF}
      end;

    //NewDisk
    $6696:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of NewDisk','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_NewDisk_TNewDiskParams(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of NewDisk','RDTPCALLS');
{$ENDIF}
      end;

    //VerifyArcZone
    $6697:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of VerifyArcZone','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_VerifyArcZone_integer_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of VerifyArcZone','RDTPCALLS');
{$ENDIF}
      end;

    //RepairArcZone
    $6698:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of RepairArcZone','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_RepairArcZone_integer_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of RepairArcZone','RDTPCALLS');
{$ENDIF}
      end;

    //SelfTest
    $6699:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SelfTest','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SelfTest_integer_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SelfTest','RDTPCALLS');
{$ENDIF}
      end;

    //SetTargetArchive
    $669A:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetTargetArchive','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetTargetArchive_string_string_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetTargetArchive','RDTPCALLS');
{$ENDIF}
      end;

    //DeleteDisk
    $669B:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of DeleteDisk','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_DeleteDisk_string_boolean(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of DeleteDisk','RDTPCALLS');
{$ENDIF}
      end;

    //PauseArchive
    $669C:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of PauseArchive','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_PauseArchive_integer_boolean(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of PauseArchive','RDTPCALLS');
{$ENDIF}
      end;

    //ResetAndRepairFromTargetArchive
    $669D:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of ResetAndRepairFromTargetArchive','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_ResetAndRepairFromTargetArchive_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of ResetAndRepairFromTargetArchive','RDTPCALLS');
{$ENDIF}
      end;

    //ResetZone
    $669E:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of ResetZone','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_ResetZone_integer_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of ResetZone','RDTPCALLS');
{$ENDIF}
      end;

    //ResetDisk
    $669F:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of ResetDisk','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_ResetDisk_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of ResetDisk','RDTPCALLS');
{$ENDIF}
      end;

    //VerifyAgainstArchive
    $6670:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of VerifyAgainstArchive','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_VerifyAgainstArchive_integer_int64_int64_int64_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of VerifyAgainstArchive','RDTPCALLS');
{$ENDIF}
      end;

    //DumpBigBlock
    $6671:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of DumpBigBlock','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_DumpBigBlock_integer_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of DumpBigBlock','RDTPCALLS');
{$ENDIF}
      end;

  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


