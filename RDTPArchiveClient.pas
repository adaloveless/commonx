unit RDTPArchiveClient;
{GEN}
{TYPE CLIENT}
{CLASS TRDTPArchiveClient}
{IMPLIB RDTPArchiveClientImplib}
{TEMPLATE RDTP_gen_client_template.pas}
{RQFILE RDTPArchiveRQs.txt}
{END}
interface


uses
  packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



type
  TRDTPArchiveClient = class(TGenericRDTPClient)
  public
    procedure Init;override;
    destructor Destroy;override;

    
    function LogThis(sArchive:string; fromid:int64; toid:int64; startblock:int64; blocklength:int64; data:TDynByteArray):boolean;overload;virtual;
    procedure LogThis_Async(sArchive:string; fromid:int64; toid:int64; startblock:int64; blocklength:int64; data:TDynByteArray);overload;virtual;
    function LogThis_Response():boolean;
    function GetLog(sArchive:string; pin:TDateTime; startblock:int64; blocklength:int64; out data:TDynByteArray):int64;overload;virtual;
    procedure GetLog_Async(sArchive:string; pin:TDateTime; startblock:int64; blocklength:int64);overload;virtual;
    function GetLog_Response(out data:TDynByteArray):int64;
    function GetNextLogID(sArchive:string; zone:int64; ids_to_reserve:int64):TDateTime;overload;virtual;
    procedure GetNextLogID_Async(sArchive:string; zone:int64; ids_to_reserve:int64);overload;virtual;
    function GetNextLogID_Response():TDateTime;
    function Flush():boolean;overload;virtual;
    procedure Flush_Async();overload;virtual;
    function Flush_Response():boolean;
    function GetLogRev(sArchive:string; idx:int64):int64;overload;virtual;
    procedure GetLogRev_Async(sArchive:string; idx:int64);overload;virtual;
    function GetLogRev_Response():int64;
    function GetLogRevs(sArchive:string; startidx:int64; count:int64):TDynInt64Array;overload;virtual;
    procedure GetLogRevs_Async(sArchive:string; startidx:int64; count:int64);overload;virtual;
    function GetLogRevs_Response():TDynInt64Array;
    function GetStoredParam(sArchive:string; sParamName:string; sDefault:string):string;overload;virtual;
    procedure GetStoredParam_Async(sArchive:string; sParamName:string; sDefault:string);overload;virtual;
    function GetStoredParam_Response():string;
    procedure SetStoredParam(sArchive:string; sParamName:string; sValue:string);overload;virtual;
    procedure SetStoredParam_Async(sArchive:string; sParamName:string; sValue:string);overload;virtual;
    function ListArchives():string;overload;virtual;
    procedure ListArchives_Async();overload;virtual;
    function ListArchives_Response():string;
    function GetZoneChecksum(sArchive:string; z:int64; out iSum:int64; out iXor:int64):boolean;overload;virtual;
    procedure GetZoneChecksum_Async(sArchive:string; z:int64);overload;virtual;
    function GetZoneChecksum_Response(out iSum:int64; out iXor:int64):boolean;
    function GetZoneStackReport(sArchive:string; z:int64; fullstack:boolean):string;overload;virtual;
    procedure GetZoneStackReport_Async(sArchive:string; z:int64; fullstack:boolean);overload;virtual;
    function GetZoneStackReport_Response():string;
    procedure NextZoneHint(sArchive:string; z:int64);overload;virtual;
    procedure NextZoneHint_Async(sArchive:string; z:int64);overload;virtual;
    function GetArcVatCheckSum(sArchive:string; zStart:int64; zCount:int64):int64;overload;virtual;
    procedure GetArcVatCheckSum_Async(sArchive:string; zStart:int64; zCount:int64);overload;virtual;
    function GetArcVatCheckSum_Response():int64;


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



{ TRDTPArchiveClient }


destructor TRDTPArchiveClient.destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
function TRDTPArchiveClient.LogThis(sArchive:string; fromid:int64; toid:int64; startblock:int64; blocklength:int64; data:TDynByteArray):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($5500);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    Writeint64ToPacket(packet, fromid);
    Writeint64ToPacket(packet, toid);
    Writeint64ToPacket(packet, startblock);
    Writeint64ToPacket(packet, blocklength);
    WriteTDynByteArrayToPacket(packet, data);
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
procedure TRDTPArchiveClient.LogThis_Async(sArchive:string; fromid:int64; toid:int64; startblock:int64; blocklength:int64; data:TDynByteArray);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($5500);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    Writeint64ToPacket(packet, fromid);
    Writeint64ToPacket(packet, toid);
    Writeint64ToPacket(packet, startblock);
    Writeint64ToPacket(packet, blocklength);
    WriteTDynByteArrayToPacket(packet, data);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.LogThis_Response():boolean;
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
function TRDTPArchiveClient.GetLog(sArchive:string; pin:TDateTime; startblock:int64; blocklength:int64; out data:TDynByteArray):int64;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($5501);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    WriteTDateTimeToPacket(packet, pin);
    Writeint64ToPacket(packet, startblock);
    Writeint64ToPacket(packet, blocklength);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    Getint64FromPacket(packet, result);
    GetTDynByteArrayFromPacket(packet, data);
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
procedure TRDTPArchiveClient.GetLog_Async(sArchive:string; pin:TDateTime; startblock:int64; blocklength:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($5501);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    WriteTDateTimeToPacket(packet, pin);
    Writeint64ToPacket(packet, startblock);
    Writeint64ToPacket(packet, blocklength);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.GetLog_Response(out data:TDynByteArray):int64;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    Getint64FromPacket(packet, result);
    GetTDynByteArrayFromPacket(packet, data);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.GetNextLogID(sArchive:string; zone:int64; ids_to_reserve:int64):TDateTime;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($5502);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    Writeint64ToPacket(packet, zone);
    Writeint64ToPacket(packet, ids_to_reserve);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetTDateTimeFromPacket(packet, result);
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
procedure TRDTPArchiveClient.GetNextLogID_Async(sArchive:string; zone:int64; ids_to_reserve:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($5502);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    Writeint64ToPacket(packet, zone);
    Writeint64ToPacket(packet, ids_to_reserve);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.GetNextLogID_Response():TDateTime;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetTDateTimeFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.Flush():boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($5503);
    packet.AddVariant(0);
    packet.AddString('Archive');
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
procedure TRDTPArchiveClient.Flush_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($5503);
    packet.AddVariant(0);
    packet.AddString('Archive');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.Flush_Response():boolean;
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
function TRDTPArchiveClient.GetLogRev(sArchive:string; idx:int64):int64;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($5504);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    Writeint64ToPacket(packet, idx);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    Getint64FromPacket(packet, result);
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
procedure TRDTPArchiveClient.GetLogRev_Async(sArchive:string; idx:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($5504);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    Writeint64ToPacket(packet, idx);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.GetLogRev_Response():int64;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    Getint64FromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.GetLogRevs(sArchive:string; startidx:int64; count:int64):TDynInt64Array;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($5505);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    Writeint64ToPacket(packet, startidx);
    Writeint64ToPacket(packet, count);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetTDynInt64ArrayFromPacket(packet, result);
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
procedure TRDTPArchiveClient.GetLogRevs_Async(sArchive:string; startidx:int64; count:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($5505);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    Writeint64ToPacket(packet, startidx);
    Writeint64ToPacket(packet, count);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.GetLogRevs_Response():TDynInt64Array;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetTDynInt64ArrayFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.GetStoredParam(sArchive:string; sParamName:string; sDefault:string):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($5506);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    WritestringToPacket(packet, sParamName);
    WritestringToPacket(packet, sDefault);
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
procedure TRDTPArchiveClient.GetStoredParam_Async(sArchive:string; sParamName:string; sDefault:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($5506);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    WritestringToPacket(packet, sParamName);
    WritestringToPacket(packet, sDefault);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.GetStoredParam_Response():string;
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
procedure TRDTPArchiveClient.SetStoredParam(sArchive:string; sParamName:string; sValue:string);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($5507);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    WritestringToPacket(packet, sParamName);
    WritestringToPacket(packet, sValue);
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
procedure TRDTPArchiveClient.SetStoredParam_Async(sArchive:string; sParamName:string; sValue:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($5507);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    WritestringToPacket(packet, sParamName);
    WritestringToPacket(packet, sValue);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.ListArchives():string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($5508);
    packet.AddVariant(0);
    packet.AddString('Archive');
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
procedure TRDTPArchiveClient.ListArchives_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($5508);
    packet.AddVariant(0);
    packet.AddString('Archive');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.ListArchives_Response():string;
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
function TRDTPArchiveClient.GetZoneChecksum(sArchive:string; z:int64; out iSum:int64; out iXor:int64):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($550E);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    Writeint64ToPacket(packet, z);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
    Getint64FromPacket(packet, iSum);
    Getint64FromPacket(packet, iXor);
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
procedure TRDTPArchiveClient.GetZoneChecksum_Async(sArchive:string; z:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($550E);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    Writeint64ToPacket(packet, z);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.GetZoneChecksum_Response(out iSum:int64; out iXor:int64):boolean;
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
    Getint64FromPacket(packet, iSum);
    Getint64FromPacket(packet, iXor);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.GetZoneStackReport(sArchive:string; z:int64; fullstack:boolean):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($550F);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    Writeint64ToPacket(packet, z);
    WritebooleanToPacket(packet, fullstack);
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
procedure TRDTPArchiveClient.GetZoneStackReport_Async(sArchive:string; z:int64; fullstack:boolean);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($550F);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    Writeint64ToPacket(packet, z);
    WritebooleanToPacket(packet, fullstack);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.GetZoneStackReport_Response():string;
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
procedure TRDTPArchiveClient.NextZoneHint(sArchive:string; z:int64);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($5510);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    Writeint64ToPacket(packet, z);
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
procedure TRDTPArchiveClient.NextZoneHint_Async(sArchive:string; z:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($5510);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    Writeint64ToPacket(packet, z);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.GetArcVatCheckSum(sArchive:string; zStart:int64; zCount:int64):int64;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($5511);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    Writeint64ToPacket(packet, zStart);
    Writeint64ToPacket(packet, zCount);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    Getint64FromPacket(packet, result);
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
procedure TRDTPArchiveClient.GetArcVatCheckSum_Async(sArchive:string; zStart:int64; zCount:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($5511);
    packet.AddVariant(0);
    packet.AddString('Archive');
    WritestringToPacket(packet, sArchive);
    Writeint64ToPacket(packet, zStart);
    Writeint64ToPacket(packet, zCount);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPArchiveClient.GetArcVatCheckSum_Response():int64;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    Getint64FromPacket(packet, result);
  finally
    packet.free;
  end;
end;



function TRDTPArchiveClient.DispatchCallback: boolean;
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



procedure TRDTPArchiveClient.Init;
begin
  inherited;
  ServiceName := 'Archive';
end;

end.


