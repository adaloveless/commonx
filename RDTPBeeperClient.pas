unit RDTPBeeperClient;
{GEN}
{TYPE CLIENT}
{CLASS TRDTPBeeperClient}
{TEMPLATE RDTP_gen_client_template.pas}
{SERVICENAME BEEPER}
{RQFILE RDTPBeeperRQs.txt}

{END}


interface


uses
  packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



type
  TRDTPBeeperClient = class(TGenericRDTPClient)
  public
    procedure Init;override;
    destructor Destroy;override;

    
    function Beep(freq:integer; duration:integer):boolean;overload;virtual;
    procedure Beep_Async(freq:integer; duration:integer);overload;virtual;
    function Beep_Response():boolean;
    procedure BeepForget(freq:integer; duration:integer);overload;virtual;
    procedure BeepForget_Async(freq:integer; duration:integer);overload;virtual;
    function TestInteger(a:integer; b:integer):integer;overload;virtual;
    procedure TestInteger_Async(a:integer; b:integer);overload;virtual;
    function TestInteger_Response():integer;
    function TestInt64(a:Int64; b:Int64):Int64;overload;virtual;
    procedure TestInt64_Async(a:Int64; b:Int64);overload;virtual;
    function TestInt64_Response():Int64;
    function TestString(a:string; b:string):string;overload;virtual;
    procedure TestString_Async(a:string; b:string);overload;virtual;
    function TestString_Response():string;
    function HelloTroy(ErectPenis:integer):string;overload;virtual;
    procedure HelloTroy_Async(ErectPenis:integer);overload;virtual;
    function HelloTroy_Response():string;
    function HelloIvana(WriteToLog:string; freq:integer; duration:integer; attack:integer; release:integer):string;overload;virtual;
    procedure HelloIvana_Async(WriteToLog:string; freq:integer; duration:integer; attack:integer; release:integer);overload;virtual;
    function HelloIvana_Response():string;


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



{ TRDTPBeeperClient }


destructor TRDTPBeeperClient.destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
function TRDTPBeeperClient.Beep(freq:integer; duration:integer):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1001);
    packet.AddVariant(0);
    packet.AddString('BEEPER');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$1001:TRDTPBeeperClient.Beep');{$ENDIF}
    WriteintegerToPacket(packet, freq);
    WriteintegerToPacket(packet, duration);
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
procedure TRDTPBeeperClient.Beep_Async(freq:integer; duration:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1001);
    packet.AddVariant(0);
    packet.AddString('BEEPER');
    WriteintegerToPacket(packet, freq);
    WriteintegerToPacket(packet, duration);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPBeeperClient.Beep_Response():boolean;
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
procedure TRDTPBeeperClient.BeepForget(freq:integer; duration:integer);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1002);
    packet.AddVariant(0);
    packet.AddString('BEEPER');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$1002:TRDTPBeeperClient.BeepForget');{$ENDIF}
    WriteintegerToPacket(packet, freq);
    WriteintegerToPacket(packet, duration);
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
procedure TRDTPBeeperClient.BeepForget_Async(freq:integer; duration:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1002);
    packet.AddVariant(0);
    packet.AddString('BEEPER');
    WriteintegerToPacket(packet, freq);
    WriteintegerToPacket(packet, duration);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPBeeperClient.TestInteger(a:integer; b:integer):integer;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1003);
    packet.AddVariant(0);
    packet.AddString('BEEPER');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$1003:TRDTPBeeperClient.TestInteger');{$ENDIF}
    WriteintegerToPacket(packet, a);
    WriteintegerToPacket(packet, b);
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
procedure TRDTPBeeperClient.TestInteger_Async(a:integer; b:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1003);
    packet.AddVariant(0);
    packet.AddString('BEEPER');
    WriteintegerToPacket(packet, a);
    WriteintegerToPacket(packet, b);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPBeeperClient.TestInteger_Response():integer;
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
function TRDTPBeeperClient.TestInt64(a:Int64; b:Int64):Int64;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1004);
    packet.AddVariant(0);
    packet.AddString('BEEPER');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$1004:TRDTPBeeperClient.TestInt64');{$ENDIF}
    WriteInt64ToPacket(packet, a);
    WriteInt64ToPacket(packet, b);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetInt64FromPacket(packet, result);
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
procedure TRDTPBeeperClient.TestInt64_Async(a:Int64; b:Int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1004);
    packet.AddVariant(0);
    packet.AddString('BEEPER');
    WriteInt64ToPacket(packet, a);
    WriteInt64ToPacket(packet, b);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPBeeperClient.TestInt64_Response():Int64;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetInt64FromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPBeeperClient.TestString(a:string; b:string):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1005);
    packet.AddVariant(0);
    packet.AddString('BEEPER');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$1005:TRDTPBeeperClient.TestString');{$ENDIF}
    WritestringToPacket(packet, a);
    WritestringToPacket(packet, b);
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
procedure TRDTPBeeperClient.TestString_Async(a:string; b:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1005);
    packet.AddVariant(0);
    packet.AddString('BEEPER');
    WritestringToPacket(packet, a);
    WritestringToPacket(packet, b);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPBeeperClient.TestString_Response():string;
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
function TRDTPBeeperClient.HelloTroy(ErectPenis:integer):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1006);
    packet.AddVariant(0);
    packet.AddString('BEEPER');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$1006:TRDTPBeeperClient.HelloTroy');{$ENDIF}
    WriteintegerToPacket(packet, ErectPenis);
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
procedure TRDTPBeeperClient.HelloTroy_Async(ErectPenis:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1006);
    packet.AddVariant(0);
    packet.AddString('BEEPER');
    WriteintegerToPacket(packet, ErectPenis);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPBeeperClient.HelloTroy_Response():string;
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
function TRDTPBeeperClient.HelloIvana(WriteToLog:string; freq:integer; duration:integer; attack:integer; release:integer):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1007);
    packet.AddVariant(0);
    packet.AddString('BEEPER');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$1007:TRDTPBeeperClient.HelloIvana');{$ENDIF}
    WritestringToPacket(packet, WriteToLog);
    WriteintegerToPacket(packet, freq);
    WriteintegerToPacket(packet, duration);
    WriteintegerToPacket(packet, attack);
    WriteintegerToPacket(packet, release);
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
procedure TRDTPBeeperClient.HelloIvana_Async(WriteToLog:string; freq:integer; duration:integer; attack:integer; release:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1007);
    packet.AddVariant(0);
    packet.AddString('BEEPER');
    WritestringToPacket(packet, WriteToLog);
    WriteintegerToPacket(packet, freq);
    WriteintegerToPacket(packet, duration);
    WriteintegerToPacket(packet, attack);
    WriteintegerToPacket(packet, release);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPBeeperClient.HelloIvana_Response():string;
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



function TRDTPBeeperClient.DispatchCallback: boolean;
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



procedure TRDTPBeeperClient.Init;
begin
  inherited;
  ServiceName := 'BEEPER';
end;

end.


