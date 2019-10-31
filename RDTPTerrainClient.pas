unit RDTPTerrainClient;
{GEN}
{TYPE CLIENT}
{CLASS TTerrainClient}
{IMPLIB RDTPTerrainClientImplib}
{TEMPLATE RDTP_gen_client_template.pas}
{RQFILE RDTPTerrainRQs.txt}
{END}
interface


uses
  PacketHelpers_TerrainData, packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



type
  TTerrainClient = class(TGenericRDTPClient)
  public
    procedure Init;override;
    destructor Destroy;override;

    
    function Test():integer;overload;virtual;
    procedure Test_Async();overload;virtual;
    function Test_Response():integer;
    function GetSingleTileData(Long:double; Lat:double):TTileData;overload;virtual;
    procedure GetSingleTileData_Async(Long:double; Lat:double);overload;virtual;
    function GetSingleTileData_Response():TTileData;


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



{ TTerrainClient }


destructor TTerrainClient.destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
function TTerrainClient.Test():integer;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1110);
    packet.AddVariant(0);
    packet.AddString('Terrain');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$1110:TTerrainClient.Test');{$ENDIF}
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
procedure TTerrainClient.Test_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1110);
    packet.AddVariant(0);
    packet.AddString('Terrain');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TTerrainClient.Test_Response():integer;
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
function TTerrainClient.GetSingleTileData(Long:double; Lat:double):TTileData;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1112);
    packet.AddVariant(0);
    packet.AddString('Terrain');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$1112:TTerrainClient.GetSingleTileData');{$ENDIF}
    WritedoubleToPacket(packet, Long);
    WritedoubleToPacket(packet, Lat);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetTTileDataFromPacket(packet, result);
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
procedure TTerrainClient.GetSingleTileData_Async(Long:double; Lat:double);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1112);
    packet.AddVariant(0);
    packet.AddString('Terrain');
    WritedoubleToPacket(packet, Long);
    WritedoubleToPacket(packet, Lat);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TTerrainClient.GetSingleTileData_Response():TTileData;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetTTileDataFromPacket(packet, result);
  finally
    packet.free;
  end;
end;



function TTerrainClient.DispatchCallback: boolean;
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



procedure TTerrainClient.Init;
begin
  inherited;
  ServiceName := 'Terrain';
end;

end.


