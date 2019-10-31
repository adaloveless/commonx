unit RDTPBernBabyBernClient;
{GEN}
{TYPE CLIENT}
{CLASS TRDTPBernBabyBernClient}
{IMPLIB RDTPBernBabyBernClientImplib}
{TEMPLATE RDTP_gen_client_template.pas}
{RQFILE RDTPBernBabyBernRQs.txt}
{END}
interface


uses
  classes, packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



type
  TRDTPBernBabyBernClient = class(TGenericRDTPClient)
  public
    procedure Init;override;
    destructor Destroy;override;

    
    function ListFiles(filespec:string):string;overload;virtual;
    procedure ListFiles_Async(filespec:string);overload;virtual;
    function ListFiles_Response():string;
    function GetFile(filename:string):TStream;overload;virtual;
    procedure GetFile_Async(filename:string);overload;virtual;
    function GetFile_Response():TStream;
    function PutFile(filename:string; bytes:TStream):boolean;overload;virtual;
    procedure PutFile_Async(filename:string; bytes:TStream);overload;virtual;
    function PutFile_Response():boolean;
    procedure PutSpriteSheet(filename:string; bytes0:TStream; bytes1:TStream; bytes2:TStream);overload;virtual;
    procedure PutSpriteSheet_Async(filename:string; bytes0:TStream; bytes1:TStream; bytes2:TStream);overload;virtual;
    function GetSpriteSheet(filename:string; out bytes0:TStream; out bytes1:TStream; out bytes2:TStream):boolean;overload;virtual;
    procedure GetSpriteSheet_Async(filename:string);overload;virtual;
    function GetSpriteSheet_Response(out bytes0:TStream; out bytes1:TStream; out bytes2:TStream):boolean;
    procedure PutSpriteSheet2(filename:string; bytes0:TStream; bytes1:TStream; bytes2:TStream; refmap:TStream);overload;virtual;
    procedure PutSpriteSheet2_Async(filename:string; bytes0:TStream; bytes1:TStream; bytes2:TStream; refmap:TStream);overload;virtual;
    function GetSpriteSheet2(filename:string; out bytes0:TStream; out bytes1:TStream; out bytes2:TStream; out refmap:TStream):boolean;overload;virtual;
    procedure GetSpriteSheet2_Async(filename:string);overload;virtual;
    function GetSpriteSheet2_Response(out bytes0:TStream; out bytes1:TStream; out bytes2:TStream; out refmap:TStream):boolean;


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



{ TRDTPBernBabyBernClient }


destructor TRDTPBernBabyBernClient.destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
function TRDTPBernBabyBernClient.ListFiles(filespec:string):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($3001);
    packet.AddVariant(0);
    packet.AddString('BernBabyBern');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$3001:TRDTPBernBabyBernClient.ListFiles');{$ENDIF}
    WritestringToPacket(packet, filespec);
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
procedure TRDTPBernBabyBernClient.ListFiles_Async(filespec:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($3001);
    packet.AddVariant(0);
    packet.AddString('BernBabyBern');
    WritestringToPacket(packet, filespec);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPBernBabyBernClient.ListFiles_Response():string;
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
function TRDTPBernBabyBernClient.GetFile(filename:string):TStream;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($3002);
    packet.AddVariant(0);
    packet.AddString('BernBabyBern');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$3002:TRDTPBernBabyBernClient.GetFile');{$ENDIF}
    WritestringToPacket(packet, filename);
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
procedure TRDTPBernBabyBernClient.GetFile_Async(filename:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($3002);
    packet.AddVariant(0);
    packet.AddString('BernBabyBern');
    WritestringToPacket(packet, filename);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPBernBabyBernClient.GetFile_Response():TStream;
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
//------------------------------------------------------------------------------
function TRDTPBernBabyBernClient.PutFile(filename:string; bytes:TStream):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($3003);
    packet.AddVariant(0);
    packet.AddString('BernBabyBern');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$3003:TRDTPBernBabyBernClient.PutFile');{$ENDIF}
    WritestringToPacket(packet, filename);
    WriteTStreamToPacket(packet, bytes);
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
procedure TRDTPBernBabyBernClient.PutFile_Async(filename:string; bytes:TStream);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($3003);
    packet.AddVariant(0);
    packet.AddString('BernBabyBern');
    WritestringToPacket(packet, filename);
    WriteTStreamToPacket(packet, bytes);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPBernBabyBernClient.PutFile_Response():boolean;
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
procedure TRDTPBernBabyBernClient.PutSpriteSheet(filename:string; bytes0:TStream; bytes1:TStream; bytes2:TStream);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($3004);
    packet.AddVariant(0);
    packet.AddString('BernBabyBern');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$3004:TRDTPBernBabyBernClient.PutSpriteSheet');{$ENDIF}
    WritestringToPacket(packet, filename);
    WriteTStreamToPacket(packet, bytes0);
    WriteTStreamToPacket(packet, bytes1);
    WriteTStreamToPacket(packet, bytes2);
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
procedure TRDTPBernBabyBernClient.PutSpriteSheet_Async(filename:string; bytes0:TStream; bytes1:TStream; bytes2:TStream);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($3004);
    packet.AddVariant(0);
    packet.AddString('BernBabyBern');
    WritestringToPacket(packet, filename);
    WriteTStreamToPacket(packet, bytes0);
    WriteTStreamToPacket(packet, bytes1);
    WriteTStreamToPacket(packet, bytes2);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPBernBabyBernClient.GetSpriteSheet(filename:string; out bytes0:TStream; out bytes1:TStream; out bytes2:TStream):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($3005);
    packet.AddVariant(0);
    packet.AddString('BernBabyBern');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$3005:TRDTPBernBabyBernClient.GetSpriteSheet');{$ENDIF}
    WritestringToPacket(packet, filename);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
    GetTStreamFromPacket(packet, bytes0);
    GetTStreamFromPacket(packet, bytes1);
    GetTStreamFromPacket(packet, bytes2);
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
procedure TRDTPBernBabyBernClient.GetSpriteSheet_Async(filename:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($3005);
    packet.AddVariant(0);
    packet.AddString('BernBabyBern');
    WritestringToPacket(packet, filename);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPBernBabyBernClient.GetSpriteSheet_Response(out bytes0:TStream; out bytes1:TStream; out bytes2:TStream):boolean;
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
    GetTStreamFromPacket(packet, bytes0);
    GetTStreamFromPacket(packet, bytes1);
    GetTStreamFromPacket(packet, bytes2);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TRDTPBernBabyBernClient.PutSpriteSheet2(filename:string; bytes0:TStream; bytes1:TStream; bytes2:TStream; refmap:TStream);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($3006);
    packet.AddVariant(0);
    packet.AddString('BernBabyBern');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$3006:TRDTPBernBabyBernClient.PutSpriteSheet2');{$ENDIF}
    WritestringToPacket(packet, filename);
    WriteTStreamToPacket(packet, bytes0);
    WriteTStreamToPacket(packet, bytes1);
    WriteTStreamToPacket(packet, bytes2);
    WriteTStreamToPacket(packet, refmap);
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
procedure TRDTPBernBabyBernClient.PutSpriteSheet2_Async(filename:string; bytes0:TStream; bytes1:TStream; bytes2:TStream; refmap:TStream);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($3006);
    packet.AddVariant(0);
    packet.AddString('BernBabyBern');
    WritestringToPacket(packet, filename);
    WriteTStreamToPacket(packet, bytes0);
    WriteTStreamToPacket(packet, bytes1);
    WriteTStreamToPacket(packet, bytes2);
    WriteTStreamToPacket(packet, refmap);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPBernBabyBernClient.GetSpriteSheet2(filename:string; out bytes0:TStream; out bytes1:TStream; out bytes2:TStream; out refmap:TStream):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($3007);
    packet.AddVariant(0);
    packet.AddString('BernBabyBern');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$3007:TRDTPBernBabyBernClient.GetSpriteSheet2');{$ENDIF}
    WritestringToPacket(packet, filename);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
    GetTStreamFromPacket(packet, bytes0);
    GetTStreamFromPacket(packet, bytes1);
    GetTStreamFromPacket(packet, bytes2);
    GetTStreamFromPacket(packet, refmap);
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
procedure TRDTPBernBabyBernClient.GetSpriteSheet2_Async(filename:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($3007);
    packet.AddVariant(0);
    packet.AddString('BernBabyBern');
    WritestringToPacket(packet, filename);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPBernBabyBernClient.GetSpriteSheet2_Response(out bytes0:TStream; out bytes1:TStream; out bytes2:TStream; out refmap:TStream):boolean;
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
    GetTStreamFromPacket(packet, bytes0);
    GetTStreamFromPacket(packet, bytes1);
    GetTStreamFromPacket(packet, bytes2);
    GetTStreamFromPacket(packet, refmap);
  finally
    packet.free;
  end;
end;



function TRDTPBernBabyBernClient.DispatchCallback: boolean;
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



procedure TRDTPBernBabyBernClient.Init;
begin
  inherited;
  ServiceName := 'BernBabyBern';
end;

end.


