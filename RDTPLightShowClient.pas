unit RDTPLightShowClient;
{GEN}
{TYPE CLIENT}
{CLASS TLightShowClient}
{IMPLIB RDTPLightShowClientImplib}
{TEMPLATE RDTP_gen_client_template.pas}
{RQFILE RDTPLightShowRQs.txt}
{END}
interface


uses
  classes, packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



type
  TLightShowClient = class(TGenericRDTPClient)
  public
    procedure Init;override;
    destructor Destroy;override;

    
    function GetFiles():TStringList;overload;virtual;
    procedure GetFiles_Async();overload;virtual;
    function GetFiles_Response():TStringList;
    function GetQueue():TStringList;overload;virtual;
    procedure GetQueue_Async();overload;virtual;
    function GetQueue_Response():TStringList;
    procedure QueueItem(sName:string);overload;virtual;
    procedure QueueItem_Async(sName:string);overload;virtual;
    procedure NextItem();overload;virtual;
    procedure NextItem_Async();overload;virtual;
    procedure Pause();overload;virtual;
    procedure Pause_Async();overload;virtual;
    procedure Rewind();overload;virtual;
    procedure Rewind_Async();overload;virtual;
    function GetRemoteData():TRemoteData;overload;virtual;
    procedure GetRemoteData_Async();overload;virtual;
    function GetRemoteData_Response():TRemoteData;
    procedure UnQueueItem(iIndex:integer);overload;virtual;
    procedure UnQueueItem_Async(iIndex:integer);overload;virtual;
    procedure QueueMoveUp(iIndex:integer);overload;virtual;
    procedure QueueMoveUp_Async(iIndex:integer);overload;virtual;
    procedure QueueMoveDown(iIndex:integer);overload;virtual;
    procedure QueueMoveDown_Async(iIndex:integer);overload;virtual;


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



{ TLightShowClient }


destructor TLightShowClient.destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
function TLightShowClient.GetFiles():TStringList;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($4442);
    packet.AddVariant(0);
    packet.AddString('LightShow');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$4442:TLightShowClient.GetFiles');{$ENDIF}
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetTStringListFromPacket(packet, result);
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
procedure TLightShowClient.GetFiles_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($4442);
    packet.AddVariant(0);
    packet.AddString('LightShow');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TLightShowClient.GetFiles_Response():TStringList;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetTStringListFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TLightShowClient.GetQueue():TStringList;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($4443);
    packet.AddVariant(0);
    packet.AddString('LightShow');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$4443:TLightShowClient.GetQueue');{$ENDIF}
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetTStringListFromPacket(packet, result);
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
procedure TLightShowClient.GetQueue_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($4443);
    packet.AddVariant(0);
    packet.AddString('LightShow');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TLightShowClient.GetQueue_Response():TStringList;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetTStringListFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TLightShowClient.QueueItem(sName:string);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($4444);
    packet.AddVariant(0);
    packet.AddString('LightShow');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$4444:TLightShowClient.QueueItem');{$ENDIF}
    WritestringToPacket(packet, sName);
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
procedure TLightShowClient.QueueItem_Async(sName:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($4444);
    packet.AddVariant(0);
    packet.AddString('LightShow');
    WritestringToPacket(packet, sName);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TLightShowClient.NextItem();
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($4445);
    packet.AddVariant(0);
    packet.AddString('LightShow');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$4445:TLightShowClient.NextItem');{$ENDIF}
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
procedure TLightShowClient.NextItem_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($4445);
    packet.AddVariant(0);
    packet.AddString('LightShow');
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TLightShowClient.Pause();
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($4446);
    packet.AddVariant(0);
    packet.AddString('LightShow');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$4446:TLightShowClient.Pause');{$ENDIF}
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
procedure TLightShowClient.Pause_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($4446);
    packet.AddVariant(0);
    packet.AddString('LightShow');
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TLightShowClient.Rewind();
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($4447);
    packet.AddVariant(0);
    packet.AddString('LightShow');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$4447:TLightShowClient.Rewind');{$ENDIF}
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
procedure TLightShowClient.Rewind_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($4447);
    packet.AddVariant(0);
    packet.AddString('LightShow');
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TLightShowClient.GetRemoteData():TRemoteData;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($4448);
    packet.AddVariant(0);
    packet.AddString('LightShow');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$4448:TLightShowClient.GetRemoteData');{$ENDIF}
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetTRemoteDataFromPacket(packet, result);
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
procedure TLightShowClient.GetRemoteData_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($4448);
    packet.AddVariant(0);
    packet.AddString('LightShow');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TLightShowClient.GetRemoteData_Response():TRemoteData;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetTRemoteDataFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TLightShowClient.UnQueueItem(iIndex:integer);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($4450);
    packet.AddVariant(0);
    packet.AddString('LightShow');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$4450:TLightShowClient.UnQueueItem');{$ENDIF}
    WriteintegerToPacket(packet, iIndex);
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
procedure TLightShowClient.UnQueueItem_Async(iIndex:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($4450);
    packet.AddVariant(0);
    packet.AddString('LightShow');
    WriteintegerToPacket(packet, iIndex);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TLightShowClient.QueueMoveUp(iIndex:integer);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($4451);
    packet.AddVariant(0);
    packet.AddString('LightShow');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$4451:TLightShowClient.QueueMoveUp');{$ENDIF}
    WriteintegerToPacket(packet, iIndex);
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
procedure TLightShowClient.QueueMoveUp_Async(iIndex:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($4451);
    packet.AddVariant(0);
    packet.AddString('LightShow');
    WriteintegerToPacket(packet, iIndex);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TLightShowClient.QueueMoveDown(iIndex:integer);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($4452);
    packet.AddVariant(0);
    packet.AddString('LightShow');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$4452:TLightShowClient.QueueMoveDown');{$ENDIF}
    WriteintegerToPacket(packet, iIndex);
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
procedure TLightShowClient.QueueMoveDown_Async(iIndex:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($4452);
    packet.AddVariant(0);
    packet.AddString('LightShow');
    WriteintegerToPacket(packet, iIndex);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;



function TLightShowClient.DispatchCallback: boolean;
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



procedure TLightShowClient.Init;
begin
  inherited;
  ServiceName := 'LightShow';
end;

end.


