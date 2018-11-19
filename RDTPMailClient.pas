unit RDTPMailClient;
{GEN}
{TYPE CLIENT}
{CLASS TMailClient}
{IMPLIB RDTPMailClientImplib}
{TEMPLATE RDTP_gen_client_template.pas}
{RQFILE RDTPMailRQs.txt}
{END}
interface


uses
  RDTPSpamPacketHelpers, packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



type
  TMailClient = class(TGenericRDTPClient)
  public
    procedure Init;override;
    destructor Destroy;override;

    
    function GetBlacklistStatus(sIP:string; bFullCheck:boolean):integer;overload;virtual;
    procedure GetBlacklistStatus_Async(sIP:string; bFullCheck:boolean);overload;virtual;
    function GetBlacklistStatus_Response():integer;
    procedure Blacklist(sIP:string; sReason:string; iCode:integer; iDays:integer);overload;virtual;
    procedure Blacklist_Async(sIP:string; sReason:string; iCode:integer; iDays:integer);overload;virtual;
    function GetSpamProbability(sText:string):real;overload;virtual;
    procedure GetSpamProbability_Async(sText:string);overload;virtual;
    function GetSpamProbability_Response():real;
    function BayesianCommit(sText:string; bSpam:boolean; iMultiplier:real):boolean;overload;virtual;
    procedure BayesianCommit_Async(sText:string; bSpam:boolean; iMultiplier:real);overload;virtual;
    function BayesianCommit_Response():boolean;
    procedure Debug(sText:string; bSay:boolean);overload;virtual;
    procedure Debug_Async(sText:string; bSay:boolean);overload;virtual;
    function GetBlacklistCountIncludingContent(sIP:string; sPLainText:string; sHTMLText:string; bFullCheck:boolean):integer;overload;virtual;
    procedure GetBlacklistCountIncludingContent_Async(sIP:string; sPLainText:string; sHTMLText:string; bFullCheck:boolean);overload;virtual;
    function GetBlacklistCountIncludingContent_Response():integer;
    procedure SetWordCommonality(sWord:string; iCommonality:int64);overload;virtual;
    procedure SetWordCommonality_Async(sWord:string; iCommonality:int64);overload;virtual;
    procedure MailCommand(sSubject:string; sBody:string);overload;virtual;
    procedure MailCommand_Async(sSubject:string; sBody:string);overload;virtual;


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



{ TMailClient }


destructor TMailClient.destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
function TMailClient.GetBlacklistStatus(sIP:string; bFullCheck:boolean):integer;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6005);
    packet.AddVariant(0);
    packet.AddString('Mail');
    WritestringToPacket(packet, sIP);
    WritebooleanToPacket(packet, bFullCheck);
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
procedure TMailClient.GetBlacklistStatus_Async(sIP:string; bFullCheck:boolean);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6005);
    packet.AddVariant(0);
    packet.AddString('Mail');
    WritestringToPacket(packet, sIP);
    WritebooleanToPacket(packet, bFullCheck);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TMailClient.GetBlacklistStatus_Response():integer;
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
procedure TMailClient.Blacklist(sIP:string; sReason:string; iCode:integer; iDays:integer);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6006);
    packet.AddVariant(0);
    packet.AddString('Mail');
    WritestringToPacket(packet, sIP);
    WritestringToPacket(packet, sReason);
    WriteintegerToPacket(packet, iCode);
    WriteintegerToPacket(packet, iDays);
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
procedure TMailClient.Blacklist_Async(sIP:string; sReason:string; iCode:integer; iDays:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6006);
    packet.AddVariant(0);
    packet.AddString('Mail');
    WritestringToPacket(packet, sIP);
    WritestringToPacket(packet, sReason);
    WriteintegerToPacket(packet, iCode);
    WriteintegerToPacket(packet, iDays);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TMailClient.GetSpamProbability(sText:string):real;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6007);
    packet.AddVariant(0);
    packet.AddString('Mail');
    WritestringToPacket(packet, sText);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetrealFromPacket(packet, result);
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
procedure TMailClient.GetSpamProbability_Async(sText:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6007);
    packet.AddVariant(0);
    packet.AddString('Mail');
    WritestringToPacket(packet, sText);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TMailClient.GetSpamProbability_Response():real;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetrealFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TMailClient.BayesianCommit(sText:string; bSpam:boolean; iMultiplier:real):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6008);
    packet.AddVariant(0);
    packet.AddString('Mail');
    WritestringToPacket(packet, sText);
    WritebooleanToPacket(packet, bSpam);
    WriterealToPacket(packet, iMultiplier);
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
procedure TMailClient.BayesianCommit_Async(sText:string; bSpam:boolean; iMultiplier:real);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6008);
    packet.AddVariant(0);
    packet.AddString('Mail');
    WritestringToPacket(packet, sText);
    WritebooleanToPacket(packet, bSpam);
    WriterealToPacket(packet, iMultiplier);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TMailClient.BayesianCommit_Response():boolean;
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
procedure TMailClient.Debug(sText:string; bSay:boolean);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6009);
    packet.AddVariant(0);
    packet.AddString('Mail');
    WritestringToPacket(packet, sText);
    WritebooleanToPacket(packet, bSay);
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
procedure TMailClient.Debug_Async(sText:string; bSay:boolean);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6009);
    packet.AddVariant(0);
    packet.AddString('Mail');
    WritestringToPacket(packet, sText);
    WritebooleanToPacket(packet, bSay);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TMailClient.GetBlacklistCountIncludingContent(sIP:string; sPLainText:string; sHTMLText:string; bFullCheck:boolean):integer;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6010);
    packet.AddVariant(0);
    packet.AddString('Mail');
    WritestringToPacket(packet, sIP);
    WritestringToPacket(packet, sPLainText);
    WritestringToPacket(packet, sHTMLText);
    WritebooleanToPacket(packet, bFullCheck);
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
procedure TMailClient.GetBlacklistCountIncludingContent_Async(sIP:string; sPLainText:string; sHTMLText:string; bFullCheck:boolean);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6010);
    packet.AddVariant(0);
    packet.AddString('Mail');
    WritestringToPacket(packet, sIP);
    WritestringToPacket(packet, sPLainText);
    WritestringToPacket(packet, sHTMLText);
    WritebooleanToPacket(packet, bFullCheck);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TMailClient.GetBlacklistCountIncludingContent_Response():integer;
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
procedure TMailClient.SetWordCommonality(sWord:string; iCommonality:int64);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6011);
    packet.AddVariant(0);
    packet.AddString('Mail');
    WritestringToPacket(packet, sWord);
    Writeint64ToPacket(packet, iCommonality);
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
procedure TMailClient.SetWordCommonality_Async(sWord:string; iCommonality:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6011);
    packet.AddVariant(0);
    packet.AddString('Mail');
    WritestringToPacket(packet, sWord);
    Writeint64ToPacket(packet, iCommonality);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TMailClient.MailCommand(sSubject:string; sBody:string);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6012);
    packet.AddVariant(0);
    packet.AddString('Mail');
    WritestringToPacket(packet, sSubject);
    WritestringToPacket(packet, sBody);
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
procedure TMailClient.MailCommand_Async(sSubject:string; sBody:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6012);
    packet.AddVariant(0);
    packet.AddString('Mail');
    WritestringToPacket(packet, sSubject);
    WritestringToPacket(packet, sBody);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;



function TMailClient.DispatchCallback: boolean;
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



procedure TMailClient.Init;
begin
  inherited;
  ServiceName := 'Mail';
end;

end.


