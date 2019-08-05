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
  StorageEngineTypes, classes, packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



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
    function BackProc(exe_no_path:string; commandlineparams:string; backinputstringcontent:string; backinputfile:string; backoutputfile:string):TStream;overload;virtual;
    procedure BackProc_Async(exe_no_path:string; commandlineparams:string; backinputstringcontent:string; backinputfile:string; backoutputfile:string);overload;virtual;
    function BackProc_Response():TStream;
    procedure BeginTransaction();overload;virtual;
    procedure BeginTransaction_Async();overload;virtual;
    function Commit():boolean;overload;virtual;
    procedure Commit_Async();overload;virtual;
    function Commit_Response():boolean;
    function Rollback():boolean;overload;virtual;
    procedure Rollback_Async();overload;virtual;
    function Rollback_Response():boolean;
    procedure BeginTransactionOn(channel_const:integer);overload;virtual;
    procedure BeginTransactionOn_Async(channel_const:integer);overload;virtual;
    procedure CommitOn(channel_const:integer);overload;virtual;
    procedure CommitOn_Async(channel_const:integer);overload;virtual;
    procedure RollbackOn(channel_const:integer);overload;virtual;
    procedure RollbackOn_Async(channel_const:integer);overload;virtual;
    function ReadOn(channel_const:integer; query:string):TSERowSet;overload;virtual;
    procedure ReadOn_Async(channel_const:integer; query:string);overload;virtual;
    function ReadOn_Response():TSERowSet;
    function WriteOn(channel_const:integer; query:string):boolean;overload;virtual;
    procedure WriteOn_Async(channel_const:integer; query:string);overload;virtual;
    function WriteOn_Response():boolean;
    procedure WriteBehindOn(channel_const:integer; query:string);overload;virtual;
    procedure WriteBehindOn_Async(channel_const:integer; query:string);overload;virtual;
    function GetNextID(key:string):int64;overload;virtual;
    procedure GetNextID_Async(key:string);overload;virtual;
    function GetNextID_Response():int64;
    procedure SetNextID(key:string; id:int64);overload;virtual;
    procedure SetNextID_Async(key:string; id:int64);overload;virtual;
    function GetNextIDEx(key:string; table:string; field:string):int64;overload;virtual;
    procedure GetNextIDEx_Async(key:string; table:string; field:string);overload;virtual;
    function GetNextIDEx_Response():int64;


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
//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.BackProc(exe_no_path:string; commandlineparams:string; backinputstringcontent:string; backinputfile:string; backoutputfile:string):TStream;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1115);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WritestringToPacket(packet, exe_no_path);
    WritestringToPacket(packet, commandlineparams);
    WritestringToPacket(packet, backinputstringcontent);
    WritestringToPacket(packet, backinputfile);
    WritestringToPacket(packet, backoutputfile);
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
procedure TRDTPSQLConnectionClient.BackProc_Async(exe_no_path:string; commandlineparams:string; backinputstringcontent:string; backinputfile:string; backoutputfile:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1115);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WritestringToPacket(packet, exe_no_path);
    WritestringToPacket(packet, commandlineparams);
    WritestringToPacket(packet, backinputstringcontent);
    WritestringToPacket(packet, backinputfile);
    WritestringToPacket(packet, backoutputfile);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.BackProc_Response():TStream;
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
procedure TRDTPSQLConnectionClient.BeginTransaction();
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1116);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
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
procedure TRDTPSQLConnectionClient.BeginTransaction_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1116);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.Commit():boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1117);
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
procedure TRDTPSQLConnectionClient.Commit_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1117);
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
function TRDTPSQLConnectionClient.Commit_Response():boolean;
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
function TRDTPSQLConnectionClient.Rollback():boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1118);
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
procedure TRDTPSQLConnectionClient.Rollback_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1118);
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
function TRDTPSQLConnectionClient.Rollback_Response():boolean;
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
procedure TRDTPSQLConnectionClient.BeginTransactionOn(channel_const:integer);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($111A);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WriteintegerToPacket(packet, channel_const);
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
procedure TRDTPSQLConnectionClient.BeginTransactionOn_Async(channel_const:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($111A);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WriteintegerToPacket(packet, channel_const);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TRDTPSQLConnectionClient.CommitOn(channel_const:integer);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($111B);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WriteintegerToPacket(packet, channel_const);
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
procedure TRDTPSQLConnectionClient.CommitOn_Async(channel_const:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($111B);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WriteintegerToPacket(packet, channel_const);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TRDTPSQLConnectionClient.RollbackOn(channel_const:integer);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($111C);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WriteintegerToPacket(packet, channel_const);
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
procedure TRDTPSQLConnectionClient.RollbackOn_Async(channel_const:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($111C);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WriteintegerToPacket(packet, channel_const);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.ReadOn(channel_const:integer; query:string):TSERowSet;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($111D);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WriteintegerToPacket(packet, channel_const);
    WritestringToPacket(packet, query);
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
procedure TRDTPSQLConnectionClient.ReadOn_Async(channel_const:integer; query:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($111D);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WriteintegerToPacket(packet, channel_const);
    WritestringToPacket(packet, query);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.ReadOn_Response():TSERowSet;
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
//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.WriteOn(channel_const:integer; query:string):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($111E);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WriteintegerToPacket(packet, channel_const);
    WritestringToPacket(packet, query);
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
procedure TRDTPSQLConnectionClient.WriteOn_Async(channel_const:integer; query:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($111E);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WriteintegerToPacket(packet, channel_const);
    WritestringToPacket(packet, query);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.WriteOn_Response():boolean;
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
procedure TRDTPSQLConnectionClient.WriteBehindOn(channel_const:integer; query:string);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($111F);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WriteintegerToPacket(packet, channel_const);
    WritestringToPacket(packet, query);
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
procedure TRDTPSQLConnectionClient.WriteBehindOn_Async(channel_const:integer; query:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($111F);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WriteintegerToPacket(packet, channel_const);
    WritestringToPacket(packet, query);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.GetNextID(key:string):int64;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1120);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WritestringToPacket(packet, key);
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
procedure TRDTPSQLConnectionClient.GetNextID_Async(key:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1120);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WritestringToPacket(packet, key);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.GetNextID_Response():int64;
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
procedure TRDTPSQLConnectionClient.SetNextID(key:string; id:int64);
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1121);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WritestringToPacket(packet, key);
    Writeint64ToPacket(packet, id);
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
procedure TRDTPSQLConnectionClient.SetNextID_Async(key:string; id:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1121);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WritestringToPacket(packet, key);
    Writeint64ToPacket(packet, id);
    BeginTransact2(packet, outpacket,nil, true);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.GetNextIDEx(key:string; table:string; field:string):int64;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($1122);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WritestringToPacket(packet, key);
    WritestringToPacket(packet, table);
    WritestringToPacket(packet, field);
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
procedure TRDTPSQLConnectionClient.GetNextIDEx_Async(key:string; table:string; field:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($1122);
    packet.AddVariant(0);
    packet.AddString('RDTPSQLConnection');
    WritestringToPacket(packet, key);
    WritestringToPacket(packet, table);
    WritestringToPacket(packet, field);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TRDTPSQLConnectionClient.GetNextIDEx_Response():int64;
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


