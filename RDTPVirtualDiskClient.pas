unit RDTPVirtualDiskClient;
{GEN}
{TYPE CLIENT}
{CLASS TVirtualDiskClient}
{IMPLIB RDTPVirtualDiskClientImplib}
{TEMPLATE RDTP_gen_client_template.pas}
{RQFILE RDTPVirtualDiskRQs.txt}
{USES VirtualDisk_Advanced}
{END}
interface


uses
  VirtualDisk_Advanced, PacketHelpers_VirtualDisk, VirtualDisk_Status, Classes, VirtualDiskParams, packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



type
  TVirtualDiskClient = class(TGenericRDTPClient)
  public
    procedure Init;override;
    destructor Destroy;override;

    
    function GetPayloadConfiguration(iDiskID:integer):PVirtualDiskPayloadConfiguration;overload;virtual;
    procedure GetPayloadConfiguration_Async(iDiskID:integer);overload;virtual;
    function GetPayloadConfiguration_Response():PVirtualDiskPayloadConfiguration;
    function ListDisks():TVirtualDiskStatusList;overload;virtual;
    procedure ListDisks_Async();overload;virtual;
    function ListDisks_Response():TVirtualDiskStatusList;
    function SetPayloadQuota(iDiskID:integer; iFileID:integer; max_size:int64):boolean;overload;virtual;
    procedure SetPayloadQuota_Async(iDiskID:integer; iFileID:integer; max_size:int64);overload;virtual;
    function SetPayloadQuota_Response():boolean;
    function AddPayload(iDiskID:integer; sFile:string; max_size:int64; physical:int64; priority:int64; flags:int64):boolean;overload;virtual;
    procedure AddPayload_Async(iDiskID:integer; sFile:string; max_size:int64; physical:int64; priority:int64; flags:int64);overload;virtual;
    function AddPayload_Response():boolean;
    function Decommissionpayload(iDiskID:integer; sFile:string):boolean;overload;virtual;
    procedure Decommissionpayload_Async(iDiskID:integer; sFile:string);overload;virtual;
    function Decommissionpayload_Response():boolean;
    function SetDefaultPayloadCacheParams(iDiskID:integer; iSegmentSize:int64; iSegmentCount:int64; bReadAhead:boolean):boolean;overload;virtual;
    procedure SetDefaultPayloadCacheParams_Async(iDiskID:integer; iSegmentSize:int64; iSegmentCount:int64; bReadAhead:boolean);overload;virtual;
    function SetDefaultPayloadCacheParams_Response():boolean;
    function SetPayloadCacheParams(iDiskID:integer; iFileID:integer; iSegmentSize:int64; iSegmentCount:int64; bReadAhead:boolean):boolean;overload;virtual;
    procedure SetPayloadCacheParams_Async(iDiskID:integer; iFileID:integer; iSegmentSize:int64; iSegmentCount:int64; bReadAhead:boolean);overload;virtual;
    function SetPayloadCacheParams_Response():boolean;
    function SetPayloadPriorty(iDiskID:integer; iFileID:integer; priority:int64):boolean;overload;virtual;
    procedure SetPayloadPriorty_Async(iDiskID:integer; iFileID:integer; priority:int64);overload;virtual;
    function SetPayloadPriorty_Response():boolean;
    function SetPayloadPhysical(iDiskID:integer; iFileID:integer; physical:int64):boolean;overload;virtual;
    procedure SetPayloadPhysical_Async(iDiskID:integer; iFileID:integer; physical:int64);overload;virtual;
    function SetPayloadPhysical_Response():boolean;
    procedure UnpauseScrubber(iDISKID:integer);overload;virtual;
    procedure UnpauseScrubber_Async(iDISKID:integer);overload;virtual;
    function ReSourcePayload(iDISKID:integer; iPayloadID:integer; sNewSource:string):boolean;overload;virtual;
    procedure ReSourcePayload_Async(iDISKID:integer; iPayloadID:integer; sNewSource:string);overload;virtual;
    function ReSourcePayload_Response():boolean;
    function RefunctPayload(iDISKID:integer; iPayLoadID:integer; sNewSource:string):boolean;overload;virtual;
    procedure RefunctPayload_Async(iDISKID:integer; iPayLoadID:integer; sNewSource:string);overload;virtual;
    function RefunctPayload_Response():boolean;
    procedure ForceRepair(iDISKID:integer);overload;virtual;
    procedure ForceRepair_Async(iDISKID:integer);overload;virtual;
    function GetDebugInfo(iDISKID:integer):string;overload;virtual;
    procedure GetDebugInfo_Async(iDISKID:integer);overload;virtual;
    function GetDebugInfo_Response():string;
    function GetRepairLog(iDISKID:integer):string;overload;virtual;
    procedure GetRepairLog_Async(iDISKID:integer);overload;virtual;
    function GetRepairLog_Response():string;
    function DrainRepairLog(iDISKID:integer):string;overload;virtual;
    procedure DrainRepairLog_Async(iDISKID:integer);overload;virtual;
    function DrainRepairLog_Response():string;
    procedure ClearRepairLog(iDISKID:integer);overload;virtual;
    procedure ClearRepairLog_Async(iDISKID:integer);overload;virtual;
    function SetCachedStripes(iDISKID:integer; value:integer):boolean;overload;virtual;
    procedure SetCachedStripes_Async(iDISKID:integer; value:integer);overload;virtual;
    function SetCachedStripes_Response():boolean;
    function GetCachedStripes(iDISKID:integer):integer;overload;virtual;
    procedure GetCachedStripes_Async(iDISKID:integer);overload;virtual;
    function GetCachedStripes_Response():integer;
    procedure QuickOnline(iDISKID:integer);overload;virtual;
    procedure QuickOnline_Async(iDISKID:integer);overload;virtual;
    procedure SetMaxDriveSpan(iDISKID:integer; ival:integer);overload;virtual;
    procedure SetMaxDriveSpan_Async(iDISKID:integer; ival:integer);overload;virtual;
    procedure SetMinDriveSpan(iDISKID:integer; ival:integer);overload;virtual;
    procedure SetMinDriveSpan_Async(iDISKID:integer; ival:integer);overload;virtual;
    function NewDisk(di:TNewDiskParams):boolean;overload;virtual;
    procedure NewDisk_Async(di:TNewDiskParams);overload;virtual;
    function NewDisk_Response():boolean;
    function VerifyArcZone(iDiskID:integer; zoneidx:int64):boolean;overload;virtual;
    procedure VerifyArcZone_Async(iDiskID:integer; zoneidx:int64);overload;virtual;
    function VerifyArcZone_Response():boolean;
    function RepairArcZone(iDiskID:integer; zoneidx:int64):boolean;overload;virtual;
    procedure RepairArcZone_Async(iDiskID:integer; zoneidx:int64);overload;virtual;
    function RepairArcZone_Response():boolean;
    function SelfTest(iDiskID:integer; testid:int64):boolean;overload;virtual;
    procedure SelfTest_Async(iDiskID:integer; testid:int64);overload;virtual;
    function SelfTest_Response():boolean;
    function SetTargetArchive(sArchive:string; sTargetHost:string; sEndPoint:string):string;overload;virtual;
    procedure SetTargetArchive_Async(sArchive:string; sTargetHost:string; sEndPoint:string);overload;virtual;
    function SetTargetArchive_Response():string;
    function DeleteDisk(sDiskName:string; DeletePayloads:boolean):boolean;overload;virtual;
    procedure DeleteDisk_Async(sDiskName:string; DeletePayloads:boolean);overload;virtual;
    function DeleteDisk_Response():boolean;
    function PauseArchive(iDiskID:integer; Pause:boolean):boolean;overload;virtual;
    procedure PauseArchive_Async(iDiskID:integer; Pause:boolean);overload;virtual;
    function PauseArchive_Response():boolean;
    function ResetAndRepairFromTargetArchive(iDiskID:integer):boolean;overload;virtual;
    procedure ResetAndRepairFromTargetArchive_Async(iDiskID:integer);overload;virtual;
    function ResetAndRepairFromTargetArchive_Response():boolean;
    function ResetZone(iDiskID:integer; iZoneID:int64):boolean;overload;virtual;
    procedure ResetZone_Async(iDiskID:integer; iZoneID:int64);overload;virtual;
    function ResetZone_Response():boolean;
    function ResetDisk(iDiskID:integer):boolean;overload;virtual;
    procedure ResetDisk_Async(iDiskID:integer);overload;virtual;
    function ResetDisk_Response():boolean;
    function VerifyAgainstArchive(diskid:integer; zone:int64; out csa:int64; out csb:int64; out difstart:int64):boolean;overload;virtual;
    procedure VerifyAgainstArchive_Async(diskid:integer; zone:int64);overload;virtual;
    function VerifyAgainstArchive_Response(out csa:int64; out csb:int64; out difstart:int64):boolean;
    function DumpBigBlock(diskid:integer; bbid:int64):boolean;overload;virtual;
    procedure DumpBigBlock_Async(diskid:integer; bbid:int64);overload;virtual;
    function DumpBigBlock_Response():boolean;


    function DispatchCallback: boolean;override;

  end;

procedure LocalDebug(s: string; sFilter: string = '');


implementation

uses
  sysutils;

procedure LocalDebug(s: string; sFilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;



{ TVirtualDiskClient }


destructor TVirtualDiskClient.destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
function TVirtualDiskClient.GetPayloadConfiguration(iDiskID:integer):PVirtualDiskPayloadConfiguration;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6674);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6674:TVirtualDiskClient.GetPayloadConfiguration');{$ENDIF}
    WriteintegerToPacket(packet, iDiskID);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetPVirtualDiskPayloadConfigurationFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.GetPayloadConfiguration_Async(iDiskID:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6674);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDiskID);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.GetPayloadConfiguration_Response():PVirtualDiskPayloadConfiguration;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetPVirtualDiskPayloadConfigurationFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.ListDisks():TVirtualDiskStatusList;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6675);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6675:TVirtualDiskClient.ListDisks');{$ENDIF}
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetTVirtualDiskStatusListFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.ListDisks_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6675);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.ListDisks_Response():TVirtualDiskStatusList;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetTVirtualDiskStatusListFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.SetPayloadQuota(iDiskID:integer; iFileID:integer; max_size:int64):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6676);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6676:TVirtualDiskClient.SetPayloadQuota');{$ENDIF}
    WriteintegerToPacket(packet, iDiskID);
    WriteintegerToPacket(packet, iFileID);
    Writeint64ToPacket(packet, max_size);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.SetPayloadQuota_Async(iDiskID:integer; iFileID:integer; max_size:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6676);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDiskID);
    WriteintegerToPacket(packet, iFileID);
    Writeint64ToPacket(packet, max_size);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.SetPayloadQuota_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.AddPayload(iDiskID:integer; sFile:string; max_size:int64; physical:int64; priority:int64; flags:int64):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6677);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6677:TVirtualDiskClient.AddPayload');{$ENDIF}
    WriteintegerToPacket(packet, iDiskID);
    WritestringToPacket(packet, sFile);
    Writeint64ToPacket(packet, max_size);
    Writeint64ToPacket(packet, physical);
    Writeint64ToPacket(packet, priority);
    Writeint64ToPacket(packet, flags);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.AddPayload_Async(iDiskID:integer; sFile:string; max_size:int64; physical:int64; priority:int64; flags:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6677);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDiskID);
    WritestringToPacket(packet, sFile);
    Writeint64ToPacket(packet, max_size);
    Writeint64ToPacket(packet, physical);
    Writeint64ToPacket(packet, priority);
    Writeint64ToPacket(packet, flags);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.AddPayload_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.Decommissionpayload(iDiskID:integer; sFile:string):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6678);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6678:TVirtualDiskClient.Decommissionpayload');{$ENDIF}
    WriteintegerToPacket(packet, iDiskID);
    WritestringToPacket(packet, sFile);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.Decommissionpayload_Async(iDiskID:integer; sFile:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6678);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDiskID);
    WritestringToPacket(packet, sFile);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.Decommissionpayload_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.SetDefaultPayloadCacheParams(iDiskID:integer; iSegmentSize:int64; iSegmentCount:int64; bReadAhead:boolean):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6679);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6679:TVirtualDiskClient.SetDefaultPayloadCacheParams');{$ENDIF}
    WriteintegerToPacket(packet, iDiskID);
    Writeint64ToPacket(packet, iSegmentSize);
    Writeint64ToPacket(packet, iSegmentCount);
    WritebooleanToPacket(packet, bReadAhead);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.SetDefaultPayloadCacheParams_Async(iDiskID:integer; iSegmentSize:int64; iSegmentCount:int64; bReadAhead:boolean);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6679);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDiskID);
    Writeint64ToPacket(packet, iSegmentSize);
    Writeint64ToPacket(packet, iSegmentCount);
    WritebooleanToPacket(packet, bReadAhead);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.SetDefaultPayloadCacheParams_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.SetPayloadCacheParams(iDiskID:integer; iFileID:integer; iSegmentSize:int64; iSegmentCount:int64; bReadAhead:boolean):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6680);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6680:TVirtualDiskClient.SetPayloadCacheParams');{$ENDIF}
    WriteintegerToPacket(packet, iDiskID);
    WriteintegerToPacket(packet, iFileID);
    Writeint64ToPacket(packet, iSegmentSize);
    Writeint64ToPacket(packet, iSegmentCount);
    WritebooleanToPacket(packet, bReadAhead);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.SetPayloadCacheParams_Async(iDiskID:integer; iFileID:integer; iSegmentSize:int64; iSegmentCount:int64; bReadAhead:boolean);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6680);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDiskID);
    WriteintegerToPacket(packet, iFileID);
    Writeint64ToPacket(packet, iSegmentSize);
    Writeint64ToPacket(packet, iSegmentCount);
    WritebooleanToPacket(packet, bReadAhead);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.SetPayloadCacheParams_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.SetPayloadPriorty(iDiskID:integer; iFileID:integer; priority:int64):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6681);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6681:TVirtualDiskClient.SetPayloadPriorty');{$ENDIF}
    WriteintegerToPacket(packet, iDiskID);
    WriteintegerToPacket(packet, iFileID);
    Writeint64ToPacket(packet, priority);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.SetPayloadPriorty_Async(iDiskID:integer; iFileID:integer; priority:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6681);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDiskID);
    WriteintegerToPacket(packet, iFileID);
    Writeint64ToPacket(packet, priority);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.SetPayloadPriorty_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.SetPayloadPhysical(iDiskID:integer; iFileID:integer; physical:int64):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6682);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6682:TVirtualDiskClient.SetPayloadPhysical');{$ENDIF}
    WriteintegerToPacket(packet, iDiskID);
    WriteintegerToPacket(packet, iFileID);
    Writeint64ToPacket(packet, physical);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.SetPayloadPhysical_Async(iDiskID:integer; iFileID:integer; physical:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6682);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDiskID);
    WriteintegerToPacket(packet, iFileID);
    Writeint64ToPacket(packet, physical);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.SetPayloadPhysical_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.UnpauseScrubber(iDISKID:integer);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6683);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6683:TVirtualDiskClient.UnpauseScrubber');{$ENDIF}
    WriteintegerToPacket(packet, iDISKID);
    if not Transact(packet, true) then raise ECritical.create('transaction failure');
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.UnpauseScrubber_Async(iDISKID:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6683);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDISKID);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.ReSourcePayload(iDISKID:integer; iPayloadID:integer; sNewSource:string):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6684);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6684:TVirtualDiskClient.ReSourcePayload');{$ENDIF}
    WriteintegerToPacket(packet, iDISKID);
    WriteintegerToPacket(packet, iPayloadID);
    WritestringToPacket(packet, sNewSource);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.ReSourcePayload_Async(iDISKID:integer; iPayloadID:integer; sNewSource:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6684);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDISKID);
    WriteintegerToPacket(packet, iPayloadID);
    WritestringToPacket(packet, sNewSource);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.ReSourcePayload_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.RefunctPayload(iDISKID:integer; iPayLoadID:integer; sNewSource:string):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6685);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6685:TVirtualDiskClient.RefunctPayload');{$ENDIF}
    WriteintegerToPacket(packet, iDISKID);
    WriteintegerToPacket(packet, iPayLoadID);
    WritestringToPacket(packet, sNewSource);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.RefunctPayload_Async(iDISKID:integer; iPayLoadID:integer; sNewSource:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6685);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDISKID);
    WriteintegerToPacket(packet, iPayLoadID);
    WritestringToPacket(packet, sNewSource);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.RefunctPayload_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.ForceRepair(iDISKID:integer);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6686);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6686:TVirtualDiskClient.ForceRepair');{$ENDIF}
    WriteintegerToPacket(packet, iDISKID);
    if not Transact(packet, true) then raise ECritical.create('transaction failure');
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.ForceRepair_Async(iDISKID:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6686);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDISKID);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.GetDebugInfo(iDISKID:integer):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6687);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6687:TVirtualDiskClient.GetDebugInfo');{$ENDIF}
    WriteintegerToPacket(packet, iDISKID);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetstringFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.GetDebugInfo_Async(iDISKID:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6687);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDISKID);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.GetDebugInfo_Response():string;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetstringFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.GetRepairLog(iDISKID:integer):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6688);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6688:TVirtualDiskClient.GetRepairLog');{$ENDIF}
    WriteintegerToPacket(packet, iDISKID);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetstringFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.GetRepairLog_Async(iDISKID:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6688);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDISKID);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.GetRepairLog_Response():string;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetstringFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.DrainRepairLog(iDISKID:integer):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6689);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6689:TVirtualDiskClient.DrainRepairLog');{$ENDIF}
    WriteintegerToPacket(packet, iDISKID);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetstringFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.DrainRepairLog_Async(iDISKID:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6689);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDISKID);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.DrainRepairLog_Response():string;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetstringFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.ClearRepairLog(iDISKID:integer);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6690);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6690:TVirtualDiskClient.ClearRepairLog');{$ENDIF}
    WriteintegerToPacket(packet, iDISKID);
    if not Transact(packet, true) then raise ECritical.create('transaction failure');
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.ClearRepairLog_Async(iDISKID:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6690);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDISKID);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.SetCachedStripes(iDISKID:integer; value:integer):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6691);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6691:TVirtualDiskClient.SetCachedStripes');{$ENDIF}
    WriteintegerToPacket(packet, iDISKID);
    WriteintegerToPacket(packet, value);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.SetCachedStripes_Async(iDISKID:integer; value:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6691);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDISKID);
    WriteintegerToPacket(packet, value);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.SetCachedStripes_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.GetCachedStripes(iDISKID:integer):integer;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6692);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6692:TVirtualDiskClient.GetCachedStripes');{$ENDIF}
    WriteintegerToPacket(packet, iDISKID);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetintegerFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.GetCachedStripes_Async(iDISKID:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6692);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDISKID);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.GetCachedStripes_Response():integer;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetintegerFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.QuickOnline(iDISKID:integer);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6693);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6693:TVirtualDiskClient.QuickOnline');{$ENDIF}
    WriteintegerToPacket(packet, iDISKID);
    if not Transact(packet, true) then raise ECritical.create('transaction failure');
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.QuickOnline_Async(iDISKID:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6693);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDISKID);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.SetMaxDriveSpan(iDISKID:integer; ival:integer);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6694);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6694:TVirtualDiskClient.SetMaxDriveSpan');{$ENDIF}
    WriteintegerToPacket(packet, iDISKID);
    WriteintegerToPacket(packet, ival);
    if not Transact(packet, true) then raise ECritical.create('transaction failure');
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.SetMaxDriveSpan_Async(iDISKID:integer; ival:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6694);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDISKID);
    WriteintegerToPacket(packet, ival);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.SetMinDriveSpan(iDISKID:integer; ival:integer);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6695);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6695:TVirtualDiskClient.SetMinDriveSpan');{$ENDIF}
    WriteintegerToPacket(packet, iDISKID);
    WriteintegerToPacket(packet, ival);
    if not Transact(packet, true) then raise ECritical.create('transaction failure');
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.SetMinDriveSpan_Async(iDISKID:integer; ival:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6695);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDISKID);
    WriteintegerToPacket(packet, ival);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.NewDisk(di:TNewDiskParams):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6696);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6696:TVirtualDiskClient.NewDisk');{$ENDIF}
    WriteTNewDiskParamsToPacket(packet, di);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.NewDisk_Async(di:TNewDiskParams);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6696);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteTNewDiskParamsToPacket(packet, di);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.NewDisk_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.VerifyArcZone(iDiskID:integer; zoneidx:int64):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6697);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6697:TVirtualDiskClient.VerifyArcZone');{$ENDIF}
    WriteintegerToPacket(packet, iDiskID);
    Writeint64ToPacket(packet, zoneidx);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.VerifyArcZone_Async(iDiskID:integer; zoneidx:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6697);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDiskID);
    Writeint64ToPacket(packet, zoneidx);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.VerifyArcZone_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.RepairArcZone(iDiskID:integer; zoneidx:int64):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6698);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6698:TVirtualDiskClient.RepairArcZone');{$ENDIF}
    WriteintegerToPacket(packet, iDiskID);
    Writeint64ToPacket(packet, zoneidx);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.RepairArcZone_Async(iDiskID:integer; zoneidx:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6698);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDiskID);
    Writeint64ToPacket(packet, zoneidx);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.RepairArcZone_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.SelfTest(iDiskID:integer; testid:int64):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6699);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6699:TVirtualDiskClient.SelfTest');{$ENDIF}
    WriteintegerToPacket(packet, iDiskID);
    Writeint64ToPacket(packet, testid);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.SelfTest_Async(iDiskID:integer; testid:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6699);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDiskID);
    Writeint64ToPacket(packet, testid);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.SelfTest_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.SetTargetArchive(sArchive:string; sTargetHost:string; sEndPoint:string):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($669A);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$669A:TVirtualDiskClient.SetTargetArchive');{$ENDIF}
    WritestringToPacket(packet, sArchive);
    WritestringToPacket(packet, sTargetHost);
    WritestringToPacket(packet, sEndPoint);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetstringFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.SetTargetArchive_Async(sArchive:string; sTargetHost:string; sEndPoint:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($669A);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WritestringToPacket(packet, sArchive);
    WritestringToPacket(packet, sTargetHost);
    WritestringToPacket(packet, sEndPoint);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.SetTargetArchive_Response():string;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetstringFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.DeleteDisk(sDiskName:string; DeletePayloads:boolean):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($669B);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$669B:TVirtualDiskClient.DeleteDisk');{$ENDIF}
    WritestringToPacket(packet, sDiskName);
    WritebooleanToPacket(packet, DeletePayloads);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.DeleteDisk_Async(sDiskName:string; DeletePayloads:boolean);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($669B);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WritestringToPacket(packet, sDiskName);
    WritebooleanToPacket(packet, DeletePayloads);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.DeleteDisk_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.PauseArchive(iDiskID:integer; Pause:boolean):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($669C);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$669C:TVirtualDiskClient.PauseArchive');{$ENDIF}
    WriteintegerToPacket(packet, iDiskID);
    WritebooleanToPacket(packet, Pause);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.PauseArchive_Async(iDiskID:integer; Pause:boolean);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($669C);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDiskID);
    WritebooleanToPacket(packet, Pause);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.PauseArchive_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.ResetAndRepairFromTargetArchive(iDiskID:integer):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($669D);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$669D:TVirtualDiskClient.ResetAndRepairFromTargetArchive');{$ENDIF}
    WriteintegerToPacket(packet, iDiskID);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.ResetAndRepairFromTargetArchive_Async(iDiskID:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($669D);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDiskID);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.ResetAndRepairFromTargetArchive_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.ResetZone(iDiskID:integer; iZoneID:int64):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($669E);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$669E:TVirtualDiskClient.ResetZone');{$ENDIF}
    WriteintegerToPacket(packet, iDiskID);
    Writeint64ToPacket(packet, iZoneID);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.ResetZone_Async(iDiskID:integer; iZoneID:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($669E);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDiskID);
    Writeint64ToPacket(packet, iZoneID);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.ResetZone_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.ResetDisk(iDiskID:integer):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($669F);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$669F:TVirtualDiskClient.ResetDisk');{$ENDIF}
    WriteintegerToPacket(packet, iDiskID);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.ResetDisk_Async(iDiskID:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($669F);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, iDiskID);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.ResetDisk_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.VerifyAgainstArchive(diskid:integer; zone:int64; out csa:int64; out csb:int64; out difstart:int64):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6670);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6670:TVirtualDiskClient.VerifyAgainstArchive');{$ENDIF}
    WriteintegerToPacket(packet, diskid);
    Writeint64ToPacket(packet, zone);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
    Getint64FromPacket(packet, csa);
    Getint64FromPacket(packet, csb);
    Getint64FromPacket(packet, difstart);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.VerifyAgainstArchive_Async(diskid:integer; zone:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6670);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, diskid);
    Writeint64ToPacket(packet, zone);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.VerifyAgainstArchive_Response(out csa:int64; out csb:int64; out difstart:int64):boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
    Getint64FromPacket(packet, csa);
    Getint64FromPacket(packet, csb);
    Getint64FromPacket(packet, difstart);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.DumpBigBlock(diskid:integer; bbid:int64):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6671);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6671:TVirtualDiskClient.DumpBigBlock');{$ENDIF}
    WriteintegerToPacket(packet, diskid);
    Writeint64ToPacket(packet, bbid);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVirtualDiskClient.DumpBigBlock_Async(diskid:integer; bbid:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6671);
    packet.AddVariant(0);
    packet.AddString('VirtualDisk');
    WriteintegerToPacket(packet, diskid);
    Writeint64ToPacket(packet, bbid);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVirtualDiskClient.DumpBigBlock_Response():boolean;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetbooleanFromPacket(packet, result);
  finally
    packet.free;
  end;
end;



function TVirtualDiskClient.DispatchCallback: boolean;
var
  iRQ: integer;
begin

  result := false;

  iRQ := callback.request.data[0];
  callback.request.seqseek(3);
  case iRQ of
    0: begin
        //beeper.Beep(100,100);
        result := true;
       end;
  
  end;

  if not result then
    result := Inherited DispatchCallback;
end;



procedure TVirtualDiskClient.Init;
begin
  inherited;
  ServiceName := 'VirtualDisk';
end;

end.


