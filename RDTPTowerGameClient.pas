unit RDTPTowerGameClient;
{GEN}
{TYPE CLIENT}
{CLASS TTowerGameClient}
{IMPLIB RDTPTowerGameClientImplib}
{TEMPLATE RDTP_gen_client_template.pas}
{RQFILE RDTPTowerGameRQs.txt}
{END}
interface


uses
  GameList, packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



type
  TTowerGameClient = class(TGenericRDTPClient)
  public
    procedure Init;override;
    destructor Destroy;override;

    
    function Login(UserName:string; Password:string):int64;overload;virtual;
    procedure Login_Async(UserName:string; Password:string);overload;virtual;
    function Login_Response():int64;
    procedure Logout(SessionID:int64);overload;virtual;
    procedure Logout_Async(SessionID:int64);overload;virtual;
    function JoinBestGame(SessionID:int64; out iGameID:int64):boolean;overload;virtual;
    procedure JoinBestGame_Async(SessionID:int64);overload;virtual;
    function JoinBestGame_Response(out iGameID:int64):boolean;
    function GetGameStatus(iGameID:int64; out status:string; out PLayerCount:integer):boolean;overload;virtual;
    procedure GetGameStatus_Async(iGameID:int64);overload;virtual;
    function GetGameStatus_Response(out status:string; out PLayerCount:integer):boolean;
    function GetUserIDForSession(SessionID:int64):int64;overload;virtual;
    procedure GetUserIDForSession_Async(SessionID:int64);overload;virtual;
    function GetUserIDForSession_Response():int64;
    function CrossStreams(iGameID:int64; iSessionID:int64; InStream:TGameStream; TotalEventsPreviouslyReceived:int64; out OutStream:TGameStream; out TotalEventsReceivedFromClient:int64):boolean;overload;virtual;
    procedure CrossStreams_Async(iGameID:int64; iSessionID:int64; InStream:TGameStream; TotalEventsPreviouslyReceived:int64);overload;virtual;
    function CrossStreams_Response(out OutStream:TGameStream; out TotalEventsReceivedFromClient:int64):boolean;
    function ShareClientDetails(iGameID:int64; iSessionID:int64; LocalGameTime:double):boolean;overload;virtual;
    procedure ShareClientDetails_Async(iGameID:int64; iSessionID:int64; LocalGameTime:double);overload;virtual;
    function ShareClientDetails_Response():boolean;
    function GetSimplePlayerList(iGameID:int64):string;overload;virtual;
    procedure GetSimplePlayerList_Async(iGameID:int64);overload;virtual;
    function GetSimplePlayerList_Response():string;
    function StorePerformanceMetrics(sUserName:string; sData:string):string;overload;virtual;
    procedure StorePerformanceMetrics_Async(sUserName:string; sData:string);overload;virtual;
    function StorePerformanceMetrics_Response():string;
    function TransportLayerTest(sSomethingToConvertToLowercase:string):string;overload;virtual;
    procedure TransportLayerTest_Async(sSomethingToConvertToLowercase:string);overload;virtual;
    function TransportLayerTest_Response():string;


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



{ TTowerGameClient }


destructor TTowerGameClient.destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
function TTowerGameClient.Login(UserName:string; Password:string):int64;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6666);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    WritestringToPacket(packet, UserName);
    WritestringToPacket(packet, Password);
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
procedure TTowerGameClient.Login_Async(UserName:string; Password:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6666);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    WritestringToPacket(packet, UserName);
    WritestringToPacket(packet, Password);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TTowerGameClient.Login_Response():int64;
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
procedure TTowerGameClient.Logout(SessionID:int64);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6667);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    Writeint64ToPacket(packet, SessionID);
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
procedure TTowerGameClient.Logout_Async(SessionID:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6667);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    Writeint64ToPacket(packet, SessionID);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TTowerGameClient.JoinBestGame(SessionID:int64; out iGameID:int64):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6668);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    Writeint64ToPacket(packet, SessionID);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
    Getint64FromPacket(packet, iGameID);
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
procedure TTowerGameClient.JoinBestGame_Async(SessionID:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6668);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    Writeint64ToPacket(packet, SessionID);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TTowerGameClient.JoinBestGame_Response(out iGameID:int64):boolean;
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
    Getint64FromPacket(packet, iGameID);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TTowerGameClient.GetGameStatus(iGameID:int64; out status:string; out PLayerCount:integer):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6669);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    Writeint64ToPacket(packet, iGameID);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
    GetstringFromPacket(packet, status);
    GetintegerFromPacket(packet, PLayerCount);
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
procedure TTowerGameClient.GetGameStatus_Async(iGameID:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6669);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    Writeint64ToPacket(packet, iGameID);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TTowerGameClient.GetGameStatus_Response(out status:string; out PLayerCount:integer):boolean;
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
    GetstringFromPacket(packet, status);
    GetintegerFromPacket(packet, PLayerCount);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TTowerGameClient.GetUserIDForSession(SessionID:int64):int64;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6670);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    Writeint64ToPacket(packet, SessionID);
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
procedure TTowerGameClient.GetUserIDForSession_Async(SessionID:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6670);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    Writeint64ToPacket(packet, SessionID);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TTowerGameClient.GetUserIDForSession_Response():int64;
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
function TTowerGameClient.CrossStreams(iGameID:int64; iSessionID:int64; InStream:TGameStream; TotalEventsPreviouslyReceived:int64; out OutStream:TGameStream; out TotalEventsReceivedFromClient:int64):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6671);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    Writeint64ToPacket(packet, iGameID);
    Writeint64ToPacket(packet, iSessionID);
    WriteTGameStreamToPacket(packet, InStream);
    Writeint64ToPacket(packet, TotalEventsPreviouslyReceived);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
    GetTGameStreamFromPacket(packet, OutStream);
    Getint64FromPacket(packet, TotalEventsReceivedFromClient);
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
procedure TTowerGameClient.CrossStreams_Async(iGameID:int64; iSessionID:int64; InStream:TGameStream; TotalEventsPreviouslyReceived:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6671);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    Writeint64ToPacket(packet, iGameID);
    Writeint64ToPacket(packet, iSessionID);
    WriteTGameStreamToPacket(packet, InStream);
    Writeint64ToPacket(packet, TotalEventsPreviouslyReceived);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TTowerGameClient.CrossStreams_Response(out OutStream:TGameStream; out TotalEventsReceivedFromClient:int64):boolean;
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
    GetTGameStreamFromPacket(packet, OutStream);
    Getint64FromPacket(packet, TotalEventsReceivedFromClient);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TTowerGameClient.ShareClientDetails(iGameID:int64; iSessionID:int64; LocalGameTime:double):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6672);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    Writeint64ToPacket(packet, iGameID);
    Writeint64ToPacket(packet, iSessionID);
    WritedoubleToPacket(packet, LocalGameTime);
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
procedure TTowerGameClient.ShareClientDetails_Async(iGameID:int64; iSessionID:int64; LocalGameTime:double);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6672);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    Writeint64ToPacket(packet, iGameID);
    Writeint64ToPacket(packet, iSessionID);
    WritedoubleToPacket(packet, LocalGameTime);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TTowerGameClient.ShareClientDetails_Response():boolean;
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
function TTowerGameClient.GetSimplePlayerList(iGameID:int64):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6673);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    Writeint64ToPacket(packet, iGameID);
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
procedure TTowerGameClient.GetSimplePlayerList_Async(iGameID:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6673);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    Writeint64ToPacket(packet, iGameID);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TTowerGameClient.GetSimplePlayerList_Response():string;
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
function TTowerGameClient.StorePerformanceMetrics(sUserName:string; sData:string):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6674);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    WritestringToPacket(packet, sUserName);
    WritestringToPacket(packet, sData);
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
procedure TTowerGameClient.StorePerformanceMetrics_Async(sUserName:string; sData:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6674);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    WritestringToPacket(packet, sUserName);
    WritestringToPacket(packet, sData);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TTowerGameClient.StorePerformanceMetrics_Response():string;
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
function TTowerGameClient.TransportLayerTest(sSomethingToConvertToLowercase:string):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6675);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    WritestringToPacket(packet, sSomethingToConvertToLowercase);
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
procedure TTowerGameClient.TransportLayerTest_Async(sSomethingToConvertToLowercase:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6675);
    packet.AddVariant(0);
    packet.AddString('TOWERGAME');
    WritestringToPacket(packet, sSomethingToConvertToLowercase);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TTowerGameClient.TransportLayerTest_Response():string;
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



function TTowerGameClient.DispatchCallback: boolean;
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



procedure TTowerGameClient.Init;
begin
  inherited;
  ServiceName := 'TOWERGAME';
end;

end.


