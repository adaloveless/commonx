unit RDTPSQLConnection;
{GEN}
{TYPE CLIENT}
{CLASS TRDTPSQLConnectionClient}
{IMPLIB RDTPSQLConnectionClienttImplib}
{TEMPLATE RDTP_gen_client_template.pas}
{RQFILE RDTPSQLConnectionRQs.txt}
{END}
interface


uses
  StorageEngineTypes, RDTPProcessorForMySQL, packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



type
  TRDTPSQLConnectionClient = class(TGenericRDTPClient)
  public
    procedure Init;override;
    destructor Destroy;override;

    
    function Test():integer;overload;virtual;
    procedure Test_Async();overload;virtual;
    function Test_Response():integer;
    function WriteQuery(sQuery:string):boolean;overload;virtual;
    procedure WriteQuery_Async(sQuery:string);overload;virtual;
    function WriteQuery_Response():boolean;
    function ReadyToWriteBehind():boolean;overload;virtual;
    procedure ReadyToWriteBehind_Async();overload;virtual;
    function ReadyToWriteBehind_Response():boolean;
    procedure WriteBehind(sQuery:string);overload;virtual;
    procedure WriteBehind_Async(sQuery:string);overload;virtual;
    function ReadQuery(sQuery:string):TSERowSet;overload;virtual;
    procedure ReadQuery_Async(sQuery:string);overload;virtual;
    function ReadQuery_Response():TSERowSet;


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



{ TRDTPSQLConnectionClient }


destructor TRDTPSQLConnectionClient.destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.Test():integer;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1110);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
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
procedure TRDTPSQLConnectionClient.Test_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1110);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.Test_Response():integer;
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
function TRDTPSQLConnectionClient.WriteQuery(sQuery:string):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1111);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WritestringToPacket(packet, sQuery);
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
procedure TRDTPSQLConnectionClient.WriteQuery_Async(sQuery:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1111);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WritestringToPacket(packet, sQuery);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.WriteQuery_Response():boolean;
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
function TRDTPSQLConnectionClient.ReadyToWriteBehind():boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1112);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
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
procedure TRDTPSQLConnectionClient.ReadyToWriteBehind_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1112);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.ReadyToWriteBehind_Response():boolean;
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
procedure TRDTPSQLConnectionClient.WriteBehind(sQuery:string);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1113);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WritestringToPacket(packet, sQuery);
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
procedure TRDTPSQLConnectionClient.WriteBehind_Async(sQuery:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1113);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WritestringToPacket(packet, sQuery);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.ReadQuery(sQuery:string):TSERowSet;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1114);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WritestringToPacket(packet, sQuery);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetTSERowSetFromPacket(packet, result);
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
procedure TRDTPSQLConnectionClient.ReadQuery_Async(sQuery:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1114);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WritestringToPacket(packet, sQuery);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.ReadQuery_Response():TSERowSet;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetTSERowSetFromPacket(packet, result);
  finally
    packet.free;
  end;
end;



function TRDTPSQLConnectionClient.DispatchCallback: boolean;
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



procedure TRDTPSQLConnectionClient.Init;
begin
  inherited;
  ServiceName := 'RDTPSQLConnection';
end;

end.


