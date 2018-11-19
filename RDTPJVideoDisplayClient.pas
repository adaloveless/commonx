unit RDTPVideoDisplayClient;
{GEN}
{TYPE CLIENT}
{CLASS TVideoDisplayClient}
{IMPLIB RDTPVideoDisplayClientImplib}
{TEMPLATE RDTP_gen_client_template.pas}
{RQFILE RDTPVideoDisplayRQs.txt}
{END}
interface


uses
  VideoMarshall, packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



type
  TVideoDisplayClient = class(TGenericRDTPClient)
  public
    procedure Init;override;
    destructor Destroy;override;

    
    procedure PrepareVideo(sFile:string; sAudioFile:string);overload;virtual;
    procedure PrepareVideo_Async(sFile:string; sAudioFile:string);overload;virtual;
    procedure PlayVideo(rStartingPoint:nativefloat);overload;virtual;
    procedure PlayVideo_Async(rStartingPoint:nativefloat);overload;virtual;
    function GetPosition():NativeFloat;overload;virtual;
    procedure GetPosition_Async();overload;virtual;
    function GetPosition_Response():NativeFloat;
    procedure Pause();overload;virtual;
    procedure Pause_Async();overload;virtual;
    procedure Rewind();overload;virtual;
    procedure Rewind_Async();overload;virtual;
    procedure SetVolume(rVol:NativeFloat);overload;virtual;
    procedure SetVolume_Async(rVol:NativeFloat);overload;virtual;
    procedure Fade(rVolFrom:NativeFloat; rVolTo:NativeFloat; rTime:NativeFloat; bNoVideo:boolean);overload;virtual;
    procedure Fade_Async(rVolFrom:NativeFloat; rVolTo:NativeFloat; rTime:NativeFloat; bNoVideo:boolean);overload;virtual;
    function GetLength():NativeFloat;overload;virtual;
    procedure GetLength_Async();overload;virtual;
    function GetLength_Response():NativeFloat;
    function GetPlaybackInfo(out position:NativeFloat; out length:NativeFloat; out playing:boolean):boolean;overload;virtual;
    procedure GetPlaybackInfo_Async();overload;virtual;
    function GetPlaybackInfo_Response(out position:NativeFloat; out length:NativeFloat; out playing:boolean):boolean;
    procedure SetPosition(pos:NativeFloat);overload;virtual;
    procedure SetPosition_Async(pos:NativeFloat);overload;virtual;
    procedure Unload();overload;virtual;
    procedure Unload_Async();overload;virtual;
    procedure SetBoost(rMultiplier:NativeFloat);overload;virtual;
    procedure SetBoost_Async(rMultiplier:NativeFloat);overload;virtual;
    function GetPlaybackInfoEx(out pi:TPlaybackInfo):boolean;overload;virtual;
    procedure GetPlaybackInfoEx_Async();overload;virtual;
    function GetPlaybackInfoEx_Response(out pi:TPlaybackInfo):boolean;
    function SetVideoSync(rSeconds:NativeFloat):boolean;overload;virtual;
    procedure SetVideoSync_Async(rSeconds:NativeFloat);overload;virtual;
    function SetVideoSync_Response():boolean;
    function SetVideoZoom(rZoom:NativeFloat):boolean;overload;virtual;
    procedure SetVideoZoom_Async(rZoom:NativeFloat);overload;virtual;
    function SetVideoZoom_Response():boolean;
    function SetVideoCentering(x:integer; y:integer):boolean;overload;virtual;
    procedure SetVideoCentering_Async(x:integer; y:integer);overload;virtual;
    function SetVideoCentering_Response():boolean;
    function SetEQ(x1:NativeFloat; x2:NativeFloat; x3:NativeFloat; x4:NativeFloat; b1:NativeFloat; b2:NativeFloat; b3:NativeFloat; b4:NativeFloat; b5:NativeFloat):boolean;overload;virtual;
    procedure SetEQ_Async(x1:NativeFloat; x2:NativeFloat; x3:NativeFloat; x4:NativeFloat; b1:NativeFloat; b2:NativeFloat; b3:NativeFloat; b4:NativeFloat; b5:NativeFloat);overload;virtual;
    function SetEQ_Response():boolean;
    function SetVideoState(bOn:boolean):boolean;overload;virtual;
    procedure SetVideoState_Async(bOn:boolean);overload;virtual;
    function SetVideoState_Response():boolean;


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



{ TVideoDisplayClient }


destructor TVideoDisplayClient.destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
procedure TVideoDisplayClient.PrepareVideo(sFile:string; sAudioFile:string);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6000);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WritestringToPacket(packet, sFile);
    WritestringToPacket(packet, sAudioFile);
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
procedure TVideoDisplayClient.PrepareVideo_Async(sFile:string; sAudioFile:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6000);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WritestringToPacket(packet, sFile);
    WritestringToPacket(packet, sAudioFile);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TVideoDisplayClient.PlayVideo(rStartingPoint:nativefloat);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6001);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WritenativefloatToPacket(packet, rStartingPoint);
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
procedure TVideoDisplayClient.PlayVideo_Async(rStartingPoint:nativefloat);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6001);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WritenativefloatToPacket(packet, rStartingPoint);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVideoDisplayClient.GetPosition():NativeFloat;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6002);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetNativeFloatFromPacket(packet, result);
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
procedure TVideoDisplayClient.GetPosition_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6002);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVideoDisplayClient.GetPosition_Response():NativeFloat;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetNativeFloatFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVideoDisplayClient.Pause();
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6003);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
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
procedure TVideoDisplayClient.Pause_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6003);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TVideoDisplayClient.Rewind();
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6004);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
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
procedure TVideoDisplayClient.Rewind_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6004);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TVideoDisplayClient.SetVolume(rVol:NativeFloat);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6005);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WriteNativeFloatToPacket(packet, rVol);
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
procedure TVideoDisplayClient.SetVolume_Async(rVol:NativeFloat);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6005);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WriteNativeFloatToPacket(packet, rVol);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TVideoDisplayClient.Fade(rVolFrom:NativeFloat; rVolTo:NativeFloat; rTime:NativeFloat; bNoVideo:boolean);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6006);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WriteNativeFloatToPacket(packet, rVolFrom);
    WriteNativeFloatToPacket(packet, rVolTo);
    WriteNativeFloatToPacket(packet, rTime);
    WritebooleanToPacket(packet, bNoVideo);
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
procedure TVideoDisplayClient.Fade_Async(rVolFrom:NativeFloat; rVolTo:NativeFloat; rTime:NativeFloat; bNoVideo:boolean);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6006);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WriteNativeFloatToPacket(packet, rVolFrom);
    WriteNativeFloatToPacket(packet, rVolTo);
    WriteNativeFloatToPacket(packet, rTime);
    WritebooleanToPacket(packet, bNoVideo);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVideoDisplayClient.GetLength():NativeFloat;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6007);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetNativeFloatFromPacket(packet, result);
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
procedure TVideoDisplayClient.GetLength_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6007);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVideoDisplayClient.GetLength_Response():NativeFloat;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetNativeFloatFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVideoDisplayClient.GetPlaybackInfo(out position:NativeFloat; out length:NativeFloat; out playing:boolean):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6008);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
    GetNativeFloatFromPacket(packet, position);
    GetNativeFloatFromPacket(packet, length);
    GetbooleanFromPacket(packet, playing);
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
procedure TVideoDisplayClient.GetPlaybackInfo_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6008);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVideoDisplayClient.GetPlaybackInfo_Response(out position:NativeFloat; out length:NativeFloat; out playing:boolean):boolean;
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
    GetNativeFloatFromPacket(packet, position);
    GetNativeFloatFromPacket(packet, length);
    GetbooleanFromPacket(packet, playing);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TVideoDisplayClient.SetPosition(pos:NativeFloat);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6009);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WriteNativeFloatToPacket(packet, pos);
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
procedure TVideoDisplayClient.SetPosition_Async(pos:NativeFloat);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6009);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WriteNativeFloatToPacket(packet, pos);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TVideoDisplayClient.Unload();
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6010);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
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
procedure TVideoDisplayClient.Unload_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6010);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TVideoDisplayClient.SetBoost(rMultiplier:NativeFloat);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6011);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WriteNativeFloatToPacket(packet, rMultiplier);
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
procedure TVideoDisplayClient.SetBoost_Async(rMultiplier:NativeFloat);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6011);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WriteNativeFloatToPacket(packet, rMultiplier);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVideoDisplayClient.GetPlaybackInfoEx(out pi:TPlaybackInfo):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6012);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
    GetTPlaybackInfoFromPacket(packet, pi);
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
procedure TVideoDisplayClient.GetPlaybackInfoEx_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6012);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVideoDisplayClient.GetPlaybackInfoEx_Response(out pi:TPlaybackInfo):boolean;
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
    GetTPlaybackInfoFromPacket(packet, pi);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TVideoDisplayClient.SetVideoSync(rSeconds:NativeFloat):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6013);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WriteNativeFloatToPacket(packet, rSeconds);
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
procedure TVideoDisplayClient.SetVideoSync_Async(rSeconds:NativeFloat);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6013);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WriteNativeFloatToPacket(packet, rSeconds);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVideoDisplayClient.SetVideoSync_Response():boolean;
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
function TVideoDisplayClient.SetVideoZoom(rZoom:NativeFloat):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6014);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WriteNativeFloatToPacket(packet, rZoom);
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
procedure TVideoDisplayClient.SetVideoZoom_Async(rZoom:NativeFloat);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6014);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WriteNativeFloatToPacket(packet, rZoom);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVideoDisplayClient.SetVideoZoom_Response():boolean;
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
function TVideoDisplayClient.SetVideoCentering(x:integer; y:integer):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6015);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WriteintegerToPacket(packet, x);
    WriteintegerToPacket(packet, y);
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
procedure TVideoDisplayClient.SetVideoCentering_Async(x:integer; y:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6015);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WriteintegerToPacket(packet, x);
    WriteintegerToPacket(packet, y);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVideoDisplayClient.SetVideoCentering_Response():boolean;
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
function TVideoDisplayClient.SetEQ(x1:NativeFloat; x2:NativeFloat; x3:NativeFloat; x4:NativeFloat; b1:NativeFloat; b2:NativeFloat; b3:NativeFloat; b4:NativeFloat; b5:NativeFloat):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6016);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WriteNativeFloatToPacket(packet, x1);
    WriteNativeFloatToPacket(packet, x2);
    WriteNativeFloatToPacket(packet, x3);
    WriteNativeFloatToPacket(packet, x4);
    WriteNativeFloatToPacket(packet, b1);
    WriteNativeFloatToPacket(packet, b2);
    WriteNativeFloatToPacket(packet, b3);
    WriteNativeFloatToPacket(packet, b4);
    WriteNativeFloatToPacket(packet, b5);
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
procedure TVideoDisplayClient.SetEQ_Async(x1:NativeFloat; x2:NativeFloat; x3:NativeFloat; x4:NativeFloat; b1:NativeFloat; b2:NativeFloat; b3:NativeFloat; b4:NativeFloat; b5:NativeFloat);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6016);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WriteNativeFloatToPacket(packet, x1);
    WriteNativeFloatToPacket(packet, x2);
    WriteNativeFloatToPacket(packet, x3);
    WriteNativeFloatToPacket(packet, x4);
    WriteNativeFloatToPacket(packet, b1);
    WriteNativeFloatToPacket(packet, b2);
    WriteNativeFloatToPacket(packet, b3);
    WriteNativeFloatToPacket(packet, b4);
    WriteNativeFloatToPacket(packet, b5);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVideoDisplayClient.SetEQ_Response():boolean;
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
function TVideoDisplayClient.SetVideoState(bOn:boolean):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6017);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WritebooleanToPacket(packet, bOn);
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
procedure TVideoDisplayClient.SetVideoState_Async(bOn:boolean);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6017);
    packet.AddVariant(0);
    packet.AddString('VideoDisplay');
    WritebooleanToPacket(packet, bOn);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TVideoDisplayClient.SetVideoState_Response():boolean;
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



function TVideoDisplayClient.DispatchCallback: boolean;
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



procedure TVideoDisplayClient.Init;
begin
  inherited;
  ServiceName := 'VideoDisplay';
end;

end.


