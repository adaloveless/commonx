unit RDTPKeyBotClient;
{GEN}
{TYPE CLIENT}
{CLASS TKeyBotClient}
{IMPLIB RDTPKeyBotClientImplib}
{TEMPLATE RDTP_gen_client_template.pas}
{RQFILE RDTPKeyBotRQs.txt}
{SERVICENAME KeyBot}
{END}
interface


uses
  packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



type
  TKeyBotClient = class(TGenericRDTPClient)
  public
    procedure Init;override;
    destructor Destroy;override;

    
    function GetNextID(iID:integer):int64;overload;virtual;
    procedure GetNextID_Async(iID:integer);overload;virtual;
    function GetNextID_Response():int64;
    function GetNextID_str(sID:string):int64;overload;virtual;
    procedure GetNextID_str_Async(sID:string);overload;virtual;
    function GetNextID_str_Response():int64;
    function SetNextID_str(sID:string; val:int64):boolean;overload;virtual;
    procedure SetNextID_str_Async(sID:string; val:int64);overload;virtual;
    function SetNextID_str_Response():boolean;
    function SetNextID(iID:integer; val:int64):boolean;overload;virtual;
    procedure SetNextID_Async(iID:integer; val:int64);overload;virtual;
    function SetNextID_Response():boolean;


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



{ TKeyBotClient }


destructor TKeyBotClient.destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
function TKeyBotClient.GetNextID(iID:integer):int64;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($0050);
    packet.AddVariant(0);
    packet.AddString('KeyBot');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$0050:TKeyBotClient.GetNextID');{$ENDIF}
    WriteintegerToPacket(packet, iID);
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
procedure TKeyBotClient.GetNextID_Async(iID:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($0050);
    packet.AddVariant(0);
    packet.AddString('KeyBot');
    WriteintegerToPacket(packet, iID);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TKeyBotClient.GetNextID_Response():int64;
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
function TKeyBotClient.GetNextID_str(sID:string):int64;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($0051);
    packet.AddVariant(0);
    packet.AddString('KeyBot');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$0051:TKeyBotClient.GetNextID_str');{$ENDIF}
    WritestringToPacket(packet, sID);
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
procedure TKeyBotClient.GetNextID_str_Async(sID:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($0051);
    packet.AddVariant(0);
    packet.AddString('KeyBot');
    WritestringToPacket(packet, sID);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TKeyBotClient.GetNextID_str_Response():int64;
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
function TKeyBotClient.SetNextID_str(sID:string; val:int64):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($0052);
    packet.AddVariant(0);
    packet.AddString('KeyBot');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$0052:TKeyBotClient.SetNextID_str');{$ENDIF}
    WritestringToPacket(packet, sID);
    Writeint64ToPacket(packet, val);
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
procedure TKeyBotClient.SetNextID_str_Async(sID:string; val:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($0052);
    packet.AddVariant(0);
    packet.AddString('KeyBot');
    WritestringToPacket(packet, sID);
    Writeint64ToPacket(packet, val);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TKeyBotClient.SetNextID_str_Response():boolean;
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
function TKeyBotClient.SetNextID(iID:integer; val:int64):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($0053);
    packet.AddVariant(0);
    packet.AddString('KeyBot');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$0053:TKeyBotClient.SetNextID');{$ENDIF}
    WriteintegerToPacket(packet, iID);
    Writeint64ToPacket(packet, val);
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
procedure TKeyBotClient.SetNextID_Async(iID:integer; val:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($0053);
    packet.AddVariant(0);
    packet.AddString('KeyBot');
    WriteintegerToPacket(packet, iID);
    Writeint64ToPacket(packet, val);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TKeyBotClient.SetNextID_Response():boolean;
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



function TKeyBotClient.DispatchCallback: boolean;
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



procedure TKeyBotClient.Init;
begin
  inherited;
  ServiceName := 'KeyBot';
end;

end.


