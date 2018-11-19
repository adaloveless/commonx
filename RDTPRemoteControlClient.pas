unit RDTPRemoteControlClient;
{GEN}
{TYPE CLIENT}
{CLASS TRemoteControlClient}
{IMPLIB RDTPRemoteControlClientImplib}
{TEMPLATE RDTP_gen_client_template.pas}
{RQFILE RDTPRemoteControlRQs.txt}
{END}
interface


uses
  classes, packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



type
  TRemoteControlClient = class(TGenericRDTPClient)
  public
    procedure Init;override;
    destructor Destroy;override;

    
    function MouseClick(x:integer; y:integer; button:integer):boolean;overload;virtual;
    procedure MouseClick_Async(x:integer; y:integer; button:integer);overload;virtual;
    function MouseClick_Response():boolean;
    function MouseDown(x:integer; y:integer; button:integer):boolean;overload;virtual;
    procedure MouseDown_Async(x:integer; y:integer; button:integer);overload;virtual;
    function MouseDown_Response():boolean;
    function MouseMove(x:integer; y:integer; button:integer):boolean;overload;virtual;
    procedure MouseMove_Async(x:integer; y:integer; button:integer);overload;virtual;
    function MouseMove_Response():boolean;
    function MouseUp(x:integer; y:integer; button:integer):boolean;overload;virtual;
    procedure MouseUp_Async(x:integer; y:integer; button:integer);overload;virtual;
    function MouseUp_Response():boolean;
    function ScreenShot():TStream;overload;virtual;
    procedure ScreenShot_Async();overload;virtual;
    function ScreenShot_Response():TStream;


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



{ TRemoteControlClient }


destructor TRemoteControlClient.destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
function TRemoteControlClient.MouseClick(x:integer; y:integer; button:integer):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($7000);
    packet.AddVariant(0);
    packet.AddString('RemoteControl');
    WriteintegerToPacket(packet, x);
    WriteintegerToPacket(packet, y);
    WriteintegerToPacket(packet, button);
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
procedure TRemoteControlClient.MouseClick_Async(x:integer; y:integer; button:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($7000);
    packet.AddVariant(0);
    packet.AddString('RemoteControl');
    WriteintegerToPacket(packet, x);
    WriteintegerToPacket(packet, y);
    WriteintegerToPacket(packet, button);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRemoteControlClient.MouseClick_Response():boolean;
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
function TRemoteControlClient.MouseDown(x:integer; y:integer; button:integer):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($7001);
    packet.AddVariant(0);
    packet.AddString('RemoteControl');
    WriteintegerToPacket(packet, x);
    WriteintegerToPacket(packet, y);
    WriteintegerToPacket(packet, button);
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
procedure TRemoteControlClient.MouseDown_Async(x:integer; y:integer; button:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($7001);
    packet.AddVariant(0);
    packet.AddString('RemoteControl');
    WriteintegerToPacket(packet, x);
    WriteintegerToPacket(packet, y);
    WriteintegerToPacket(packet, button);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRemoteControlClient.MouseDown_Response():boolean;
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
function TRemoteControlClient.MouseMove(x:integer; y:integer; button:integer):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($7002);
    packet.AddVariant(0);
    packet.AddString('RemoteControl');
    WriteintegerToPacket(packet, x);
    WriteintegerToPacket(packet, y);
    WriteintegerToPacket(packet, button);
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
procedure TRemoteControlClient.MouseMove_Async(x:integer; y:integer; button:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($7002);
    packet.AddVariant(0);
    packet.AddString('RemoteControl');
    WriteintegerToPacket(packet, x);
    WriteintegerToPacket(packet, y);
    WriteintegerToPacket(packet, button);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRemoteControlClient.MouseMove_Response():boolean;
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
function TRemoteControlClient.MouseUp(x:integer; y:integer; button:integer):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($7003);
    packet.AddVariant(0);
    packet.AddString('RemoteControl');
    WriteintegerToPacket(packet, x);
    WriteintegerToPacket(packet, y);
    WriteintegerToPacket(packet, button);
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
procedure TRemoteControlClient.MouseUp_Async(x:integer; y:integer; button:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($7003);
    packet.AddVariant(0);
    packet.AddString('RemoteControl');
    WriteintegerToPacket(packet, x);
    WriteintegerToPacket(packet, y);
    WriteintegerToPacket(packet, button);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRemoteControlClient.MouseUp_Response():boolean;
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
function TRemoteControlClient.ScreenShot():TStream;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($7004);
    packet.AddVariant(0);
    packet.AddString('RemoteControl');
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetTStreamFromPacket(packet, result);
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
procedure TRemoteControlClient.ScreenShot_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($7004);
    packet.AddVariant(0);
    packet.AddString('RemoteControl');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRemoteControlClient.ScreenShot_Response():TStream;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetTStreamFromPacket(packet, result);
  finally
    packet.free;
  end;
end;



function TRemoteControlClient.DispatchCallback: boolean;
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



procedure TRemoteControlClient.Init;
begin
  inherited;
  ServiceName := 'RemoteControl';
end;

end.


