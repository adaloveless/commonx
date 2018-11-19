unit RDTPAmbassadorClient;
{GEN}
{TYPE CLIENT}
{CLASS TRDTPAmbassadorClient}
{IMPLIB RDTPAmbassadorClientImplib}
{TEMPLATE RDTP_gen_client_template.pas}
{RQFILE RDTPAmbassadorRQs.txt}
{END}
interface


uses
  packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



type
  TRDTPAmbassadorClient = class(TGenericRDTPClient)
  public
    procedure Init;override;
    destructor Destroy;override;

    
    procedure RouteMe(ComputerNAme:string; ApplicationName:string; myport:int64);overload;virtual;
    procedure RouteMe_Async(ComputerNAme:string; ApplicationName:string; myport:int64);overload;virtual;
    function HasRoute(ComputerNAme:string; ApplicationName:string):boolean;overload;virtual;
    procedure HasRoute_Async(ComputerNAme:string; ApplicationName:string);overload;virtual;
    function HasRoute_Response():boolean;
    procedure CrossPing(ComputerName:string; ApplicationNAme:string);overload;virtual;
    procedure CrossPing_Async(ComputerName:string; ApplicationNAme:string);overload;virtual;


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



{ TRDTPAmbassadorClient }


destructor TRDTPAmbassadorClient.destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
procedure TRDTPAmbassadorClient.RouteMe(ComputerNAme:string; ApplicationName:string; myport:int64);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($7000);
    packet.AddVariant(0);
    packet.AddString('Ambassador');
    WritestringToPacket(packet, ComputerNAme);
    WritestringToPacket(packet, ApplicationName);
    Writeint64ToPacket(packet, myport);
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
procedure TRDTPAmbassadorClient.RouteMe_Async(ComputerNAme:string; ApplicationName:string; myport:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($7000);
    packet.AddVariant(0);
    packet.AddString('Ambassador');
    WritestringToPacket(packet, ComputerNAme);
    WritestringToPacket(packet, ApplicationName);
    Writeint64ToPacket(packet, myport);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPAmbassadorClient.HasRoute(ComputerNAme:string; ApplicationName:string):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($7001);
    packet.AddVariant(0);
    packet.AddString('Ambassador');
    WritestringToPacket(packet, ComputerNAme);
    WritestringToPacket(packet, ApplicationName);
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
procedure TRDTPAmbassadorClient.HasRoute_Async(ComputerNAme:string; ApplicationName:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($7001);
    packet.AddVariant(0);
    packet.AddString('Ambassador');
    WritestringToPacket(packet, ComputerNAme);
    WritestringToPacket(packet, ApplicationName);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPAmbassadorClient.HasRoute_Response():boolean;
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
procedure TRDTPAmbassadorClient.CrossPing(ComputerName:string; ApplicationNAme:string);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($7002);
    packet.AddVariant(0);
    packet.AddString('Ambassador');
    WritestringToPacket(packet, ComputerName);
    WritestringToPacket(packet, ApplicationNAme);
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
procedure TRDTPAmbassadorClient.CrossPing_Async(ComputerName:string; ApplicationNAme:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($7002);
    packet.AddVariant(0);
    packet.AddString('Ambassador');
    WritestringToPacket(packet, ComputerName);
    WritestringToPacket(packet, ApplicationNAme);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;



function TRDTPAmbassadorClient.DispatchCallback: boolean;
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



procedure TRDTPAmbassadorClient.Init;
begin
  inherited;
  ServiceName := 'Ambassador';
end;

end.


