unit FileServiceClient;
{GEN}
{TYPE CLIENT}
{CLASS TFileServiceClient}
{IMPLIB FileServiceServerImplib}
{TEMPLATE RDTP_gen_client_template.pas}
{RQFILE FileServiceRQs.txt}
{ANCESTOR TGenericRDTPClient}
{END}


interface


uses
  Dir, DirFile, Classes, rdtp_file, packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



type
  TFileServiceClient = class(TGenericRDTPClient)
  public
    procedure Init;override;
    destructor Destroy;override;

    
    function PutFile(oFile:TFileTransferReference):boolean;overload;virtual;
    procedure PutFile_Async(oFile:TFileTransferReference);overload;virtual;
    function PutFile_Response():boolean;
    function GetFile(var oFile:TFileTransferReference):boolean;overload;virtual;
    procedure GetFile_Async(var oFile:TFileTransferReference);overload;virtual;
    function GetFile_Response(var oFile:TFileTransferReference):boolean;
    function OpenFile(sFile:string; out oFile:TFileTransferReference; iMode:integer):boolean;overload;virtual;
    procedure OpenFile_Async(sFile:string; iMode:integer);overload;virtual;
    function OpenFile_Response(out oFile:TFileTransferReference):boolean;
    function CloseFile(oFile:TFileTransferReference):boolean;overload;virtual;
    procedure CloseFile_Async(oFile:TFileTransferReference);overload;virtual;
    function CloseFile_Response():boolean;
    function Dir(sRemotePath:string):TDirectory;overload;virtual;
    procedure Dir_Async(sRemotePath:string);overload;virtual;
    function Dir_Response():TDirectory;
    function GetUpgradePath(sProgramName:string):string;overload;virtual;
    procedure GetUpgradePath_Async(sProgramName:string);overload;virtual;
    function GetUpgradePath_Response():string;
    function GetUpgradeScript(sProgramName:string; iFromVersion:integer; iToVersion:integer):string;overload;virtual;
    procedure GetUpgradeScript_Async(sProgramName:string; iFromVersion:integer; iToVersion:integer);overload;virtual;
    function GetUpgradeScript_Response():string;
    function GetUpgradeVersion(sProgramName:string; bBeta:boolean):integer;overload;virtual;
    procedure GetUpgradeVersion_Async(sProgramName:string; bBeta:boolean);overload;virtual;
    function GetUpgradeVersion_Response():integer;
    function GetFileChecksum(sFile:string):TAdvancedFileChecksum;overload;virtual;
    procedure GetFileChecksum_Async(sFile:string);overload;virtual;
    function GetFileChecksum_Response():TAdvancedFileChecksum;
    function BuildHueFile(sFile:string; LengthInSeconds:real):boolean;overload;virtual;
    procedure BuildHueFile_Async(sFile:string; LengthInSeconds:real);overload;virtual;
    function BuildHueFile_Response():boolean;
    function DeleteFile(sFile:string):boolean;overload;virtual;
    procedure DeleteFile_Async(sFile:string);overload;virtual;
    function DeleteFile_Response():boolean;
    function Execute(sPath:string; sProgram:string; sParams:string):boolean;overload;virtual;
    procedure Execute_Async(sPath:string; sProgram:string; sParams:string);overload;virtual;
    function Execute_Response():boolean;
    function BuildHueFileFromStream(str:TStream; sExt:string; LengthInSeconds:real):TStream;overload;virtual;
    procedure BuildHueFileFromStream_Async(str:TStream; sExt:string; LengthInSeconds:real);overload;virtual;
    function BuildHueFileFromStream_Response():TStream;
    function EchoStream(strin:TStream):TStream;overload;virtual;
    procedure EchoStream_Async(strin:TStream);overload;virtual;
    function EchoStream_Response():TStream;
    function AppendTextFile(filename:string; text:string):boolean;overload;virtual;
    procedure AppendTextFile_Async(filename:string; text:string);overload;virtual;
    function AppendTextFile_Response():boolean;
    function GetFileSize(filename:string):int64;overload;virtual;
    procedure GetFileSize_Async(filename:string);overload;virtual;
    function GetFileSize_Response():int64;
    function ExecuteAndCapture(sPath:string; sProgram:string; sParams:string):string;overload;virtual;
    procedure ExecuteAndCapture_Async(sPath:string; sProgram:string; sParams:string);overload;virtual;
    function ExecuteAndCapture_Response():string;
    function GetFileList(sRemotePath:string; sFileSpec:string; attrmask:integer; attrresult:integer):TRemoteFileArray;overload;virtual;
    procedure GetFileList_Async(sRemotePath:string; sFileSpec:string; attrmask:integer; attrresult:integer);overload;virtual;
    function GetFileList_Response():TRemoteFileArray;
    function StartExeCommand(sPath:string; sProgram:string; sParams:string; cpus:single; memgb:single):int64;overload;virtual;
    procedure StartExeCommand_Async(sPath:string; sProgram:string; sParams:string; cpus:single; memgb:single);overload;virtual;
    function StartExeCommand_Response():int64;
    function GetCommandStatus(handle:int64; out status:string; out step:int64; out stepcount:int64; out finished:boolean):boolean;overload;virtual;
    procedure GetCommandStatus_Async(handle:int64);overload;virtual;
    function GetCommandStatus_Response(out status:string; out step:int64; out stepcount:int64; out finished:boolean):boolean;
    function EndCommand(handle:int64):boolean;overload;virtual;
    procedure EndCommand_Async(handle:int64);overload;virtual;
    function EndCommand_Response():boolean;
    function GetCPUCount():int64;overload;virtual;
    procedure GetCPUCount_Async();overload;virtual;
    function GetCPUCount_Response():int64;
    function GetCommandResourceConsumption(out cpusUsed:single; out cpuMax:single; out memGBUsed:single; out memGBMax:single):boolean;overload;virtual;
    procedure GetCommandResourceConsumption_Async();overload;virtual;
    function GetCommandResourceConsumption_Response(out cpusUsed:single; out cpuMax:single; out memGBUsed:single; out memGBMax:single):boolean;
    function EndExeCommand(handle:int64):string;overload;virtual;
    procedure EndExeCommand_Async(handle:int64);overload;virtual;
    function EndExeCommand_Response():string;
    function GetEXECommandStatus(handle:int64; out status:string; out finished:boolean; out consoleCapture:string):boolean;overload;virtual;
    procedure GetEXECommandStatus_Async(handle:int64);overload;virtual;
    function GetEXECommandStatus_Response(out status:string; out finished:boolean; out consoleCapture:string):boolean;
    function StartExeCommandEx(sPath:string; sProgram:string; sParams:string; cpus:single; memgb:single; ext_resources:string):int64;overload;virtual;
    procedure StartExeCommandEx_Async(sPath:string; sProgram:string; sParams:string; cpus:single; memgb:single; ext_resources:string);overload;virtual;
    function StartExeCommandEx_Response():int64;
    function GetCommandResourceConsumptionEx(out cpusUsed:single; out cpuMax:single; out memGBUsed:single; out memGBMax:single; out gpusUsed:single; out gpuMax:single; out ext_resources:string):boolean;overload;virtual;
    procedure GetCommandResourceConsumptionEx_Async();overload;virtual;
    function GetCommandResourceConsumptionEx_Response(out cpusUsed:single; out cpuMax:single; out memGBUsed:single; out memGBMax:single; out gpusUsed:single; out gpuMax:single; out ext_resources:string):boolean;
    function GetGPUList():string;overload;virtual;
    procedure GetGPUList_Async();overload;virtual;
    function GetGPUList_Response():string;
    function GetGPUCount():integer;overload;virtual;
    procedure GetGPUCount_Async();overload;virtual;
    function GetGPUCount_Response():integer;
    function StartExeCommandExFFMPEG(sPath:string; sProgram:string; sParams:string; sGPUParams:string; cpus:single; memgb:single; gpu:single):int64;overload;virtual;
    procedure StartExeCommandExFFMPEG_Async(sPath:string; sProgram:string; sParams:string; sGPUParams:string; cpus:single; memgb:single; gpu:single);overload;virtual;
    function StartExeCommandExFFMPEG_Response():int64;
    function FileExists(sFile:string):boolean;overload;virtual;
    procedure FileExists_Async(sFile:string);overload;virtual;
    function FileExists_Response():boolean;
    function PathExists(sPath:string):boolean;overload;virtual;
    procedure PathExists_Async(sPath:string);overload;virtual;
    function PathExists_Response():boolean;


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



{ TFileServiceClient }


destructor TFileServiceClient.destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
function TFileServiceClient.PutFile(oFile:TFileTransferReference):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6000);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6000:TFileServiceClient.PutFile');{$ENDIF}
    WriteTFileTransferReferenceToPacket(packet, oFile);
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
procedure TFileServiceClient.PutFile_Async(oFile:TFileTransferReference);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6000);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WriteTFileTransferReferenceToPacket(packet, oFile);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.PutFile_Response():boolean;
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
function TFileServiceClient.GetFile(var oFile:TFileTransferReference):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6001);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6001:TFileServiceClient.GetFile');{$ENDIF}
    WriteTFileTransferReferenceToPacket(packet, oFile);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
    GetTFileTransferReferenceFromPacket(packet, oFile);
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
procedure TFileServiceClient.GetFile_Async(var oFile:TFileTransferReference);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6001);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WriteTFileTransferReferenceToPacket(packet, oFile);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.GetFile_Response(var oFile:TFileTransferReference):boolean;
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
    GetTFileTransferReferenceFromPacket(packet, oFile);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.OpenFile(sFile:string; out oFile:TFileTransferReference; iMode:integer):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6002);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6002:TFileServiceClient.OpenFile');{$ENDIF}
    WritestringToPacket(packet, sFile);
    WriteintegerToPacket(packet, iMode);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
    GetTFileTransferReferenceFromPacket(packet, oFile);
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
procedure TFileServiceClient.OpenFile_Async(sFile:string; iMode:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6002);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, sFile);
    WriteintegerToPacket(packet, iMode);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.OpenFile_Response(out oFile:TFileTransferReference):boolean;
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
    GetTFileTransferReferenceFromPacket(packet, oFile);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.CloseFile(oFile:TFileTransferReference):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6003);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6003:TFileServiceClient.CloseFile');{$ENDIF}
    WriteTFileTransferReferenceToPacket(packet, oFile);
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
procedure TFileServiceClient.CloseFile_Async(oFile:TFileTransferReference);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6003);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WriteTFileTransferReferenceToPacket(packet, oFile);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.CloseFile_Response():boolean;
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
function TFileServiceClient.Dir(sRemotePath:string):TDirectory;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6004);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6004:TFileServiceClient.Dir');{$ENDIF}
    WritestringToPacket(packet, sRemotePath);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetTDirectoryFromPacket(packet, result);
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
procedure TFileServiceClient.Dir_Async(sRemotePath:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6004);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, sRemotePath);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.Dir_Response():TDirectory;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetTDirectoryFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.GetUpgradePath(sProgramName:string):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6005);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6005:TFileServiceClient.GetUpgradePath');{$ENDIF}
    WritestringToPacket(packet, sProgramName);
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
procedure TFileServiceClient.GetUpgradePath_Async(sProgramName:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6005);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, sProgramName);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.GetUpgradePath_Response():string;
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
function TFileServiceClient.GetUpgradeScript(sProgramName:string; iFromVersion:integer; iToVersion:integer):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6006);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6006:TFileServiceClient.GetUpgradeScript');{$ENDIF}
    WritestringToPacket(packet, sProgramName);
    WriteintegerToPacket(packet, iFromVersion);
    WriteintegerToPacket(packet, iToVersion);
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
procedure TFileServiceClient.GetUpgradeScript_Async(sProgramName:string; iFromVersion:integer; iToVersion:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6006);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, sProgramName);
    WriteintegerToPacket(packet, iFromVersion);
    WriteintegerToPacket(packet, iToVersion);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.GetUpgradeScript_Response():string;
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
function TFileServiceClient.GetUpgradeVersion(sProgramName:string; bBeta:boolean):integer;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6007);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6007:TFileServiceClient.GetUpgradeVersion');{$ENDIF}
    WritestringToPacket(packet, sProgramName);
    WritebooleanToPacket(packet, bBeta);
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
procedure TFileServiceClient.GetUpgradeVersion_Async(sProgramName:string; bBeta:boolean);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6007);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, sProgramName);
    WritebooleanToPacket(packet, bBeta);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.GetUpgradeVersion_Response():integer;
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
function TFileServiceClient.GetFileChecksum(sFile:string):TAdvancedFileChecksum;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6008);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6008:TFileServiceClient.GetFileChecksum');{$ENDIF}
    WritestringToPacket(packet, sFile);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetTAdvancedFileChecksumFromPacket(packet, result);
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
procedure TFileServiceClient.GetFileChecksum_Async(sFile:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6008);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, sFile);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.GetFileChecksum_Response():TAdvancedFileChecksum;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetTAdvancedFileChecksumFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.BuildHueFile(sFile:string; LengthInSeconds:real):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6009);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6009:TFileServiceClient.BuildHueFile');{$ENDIF}
    WritestringToPacket(packet, sFile);
    WriterealToPacket(packet, LengthInSeconds);
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
procedure TFileServiceClient.BuildHueFile_Async(sFile:string; LengthInSeconds:real);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6009);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, sFile);
    WriterealToPacket(packet, LengthInSeconds);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.BuildHueFile_Response():boolean;
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
function TFileServiceClient.DeleteFile(sFile:string):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6010);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6010:TFileServiceClient.DeleteFile');{$ENDIF}
    WritestringToPacket(packet, sFile);
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
procedure TFileServiceClient.DeleteFile_Async(sFile:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6010);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, sFile);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.DeleteFile_Response():boolean;
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
function TFileServiceClient.Execute(sPath:string; sProgram:string; sParams:string):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6011);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6011:TFileServiceClient.Execute');{$ENDIF}
    WritestringToPacket(packet, sPath);
    WritestringToPacket(packet, sProgram);
    WritestringToPacket(packet, sParams);
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
procedure TFileServiceClient.Execute_Async(sPath:string; sProgram:string; sParams:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6011);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, sPath);
    WritestringToPacket(packet, sProgram);
    WritestringToPacket(packet, sParams);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.Execute_Response():boolean;
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
function TFileServiceClient.BuildHueFileFromStream(str:TStream; sExt:string; LengthInSeconds:real):TStream;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6012);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6012:TFileServiceClient.BuildHueFileFromStream');{$ENDIF}
    WriteTStreamToPacket(packet, str);
    WritestringToPacket(packet, sExt);
    WriterealToPacket(packet, LengthInSeconds);
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
procedure TFileServiceClient.BuildHueFileFromStream_Async(str:TStream; sExt:string; LengthInSeconds:real);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6012);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WriteTStreamToPacket(packet, str);
    WritestringToPacket(packet, sExt);
    WriterealToPacket(packet, LengthInSeconds);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.BuildHueFileFromStream_Response():TStream;
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
function TFileServiceClient.EchoStream(strin:TStream):TStream;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6013);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6013:TFileServiceClient.EchoStream');{$ENDIF}
    WriteTStreamToPacket(packet, strin);
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
procedure TFileServiceClient.EchoStream_Async(strin:TStream);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6013);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WriteTStreamToPacket(packet, strin);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.EchoStream_Response():TStream;
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
function TFileServiceClient.AppendTextFile(filename:string; text:string):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6014);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6014:TFileServiceClient.AppendTextFile');{$ENDIF}
    WritestringToPacket(packet, filename);
    WritestringToPacket(packet, text);
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
procedure TFileServiceClient.AppendTextFile_Async(filename:string; text:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6014);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, filename);
    WritestringToPacket(packet, text);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.AppendTextFile_Response():boolean;
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
function TFileServiceClient.GetFileSize(filename:string):int64;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6015);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6015:TFileServiceClient.GetFileSize');{$ENDIF}
    WritestringToPacket(packet, filename);
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
procedure TFileServiceClient.GetFileSize_Async(filename:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6015);
    packet.AddVariant(0);
    packet.AddString('FileService');
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
function TFileServiceClient.GetFileSize_Response():int64;
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
function TFileServiceClient.ExecuteAndCapture(sPath:string; sProgram:string; sParams:string):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6016);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6016:TFileServiceClient.ExecuteAndCapture');{$ENDIF}
    WritestringToPacket(packet, sPath);
    WritestringToPacket(packet, sProgram);
    WritestringToPacket(packet, sParams);
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
procedure TFileServiceClient.ExecuteAndCapture_Async(sPath:string; sProgram:string; sParams:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6016);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, sPath);
    WritestringToPacket(packet, sProgram);
    WritestringToPacket(packet, sParams);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.ExecuteAndCapture_Response():string;
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
function TFileServiceClient.GetFileList(sRemotePath:string; sFileSpec:string; attrmask:integer; attrresult:integer):TRemoteFileArray;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6017);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6017:TFileServiceClient.GetFileList');{$ENDIF}
    WritestringToPacket(packet, sRemotePath);
    WritestringToPacket(packet, sFileSpec);
    WriteintegerToPacket(packet, attrmask);
    WriteintegerToPacket(packet, attrresult);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetTRemoteFileArrayFromPacket(packet, result);
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
procedure TFileServiceClient.GetFileList_Async(sRemotePath:string; sFileSpec:string; attrmask:integer; attrresult:integer);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6017);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, sRemotePath);
    WritestringToPacket(packet, sFileSpec);
    WriteintegerToPacket(packet, attrmask);
    WriteintegerToPacket(packet, attrresult);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.GetFileList_Response():TRemoteFileArray;
var
  packet: TRDTPPacket;
begin
  packet := nil;
  try
    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create('Transaction Failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    //packet.SeqRead;//read off the service name and forget it (it is already known)
    GetTRemoteFileArrayFromPacket(packet, result);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.StartExeCommand(sPath:string; sProgram:string; sParams:string; cpus:single; memgb:single):int64;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6018);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6018:TFileServiceClient.StartExeCommand');{$ENDIF}
    WritestringToPacket(packet, sPath);
    WritestringToPacket(packet, sProgram);
    WritestringToPacket(packet, sParams);
    WritesingleToPacket(packet, cpus);
    WritesingleToPacket(packet, memgb);
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
procedure TFileServiceClient.StartExeCommand_Async(sPath:string; sProgram:string; sParams:string; cpus:single; memgb:single);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6018);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, sPath);
    WritestringToPacket(packet, sProgram);
    WritestringToPacket(packet, sParams);
    WritesingleToPacket(packet, cpus);
    WritesingleToPacket(packet, memgb);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.StartExeCommand_Response():int64;
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
function TFileServiceClient.GetCommandStatus(handle:int64; out status:string; out step:int64; out stepcount:int64; out finished:boolean):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6019);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6019:TFileServiceClient.GetCommandStatus');{$ENDIF}
    Writeint64ToPacket(packet, handle);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
    GetstringFromPacket(packet, status);
    Getint64FromPacket(packet, step);
    Getint64FromPacket(packet, stepcount);
    GetbooleanFromPacket(packet, finished);
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
procedure TFileServiceClient.GetCommandStatus_Async(handle:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6019);
    packet.AddVariant(0);
    packet.AddString('FileService');
    Writeint64ToPacket(packet, handle);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.GetCommandStatus_Response(out status:string; out step:int64; out stepcount:int64; out finished:boolean):boolean;
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
    Getint64FromPacket(packet, step);
    Getint64FromPacket(packet, stepcount);
    GetbooleanFromPacket(packet, finished);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.EndCommand(handle:int64):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6020);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6020:TFileServiceClient.EndCommand');{$ENDIF}
    Writeint64ToPacket(packet, handle);
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
procedure TFileServiceClient.EndCommand_Async(handle:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6020);
    packet.AddVariant(0);
    packet.AddString('FileService');
    Writeint64ToPacket(packet, handle);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.EndCommand_Response():boolean;
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
function TFileServiceClient.GetCPUCount():int64;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6021);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6021:TFileServiceClient.GetCPUCount');{$ENDIF}
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
procedure TFileServiceClient.GetCPUCount_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6021);
    packet.AddVariant(0);
    packet.AddString('FileService');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.GetCPUCount_Response():int64;
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
function TFileServiceClient.GetCommandResourceConsumption(out cpusUsed:single; out cpuMax:single; out memGBUsed:single; out memGBMax:single):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6022);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6022:TFileServiceClient.GetCommandResourceConsumption');{$ENDIF}
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
    GetsingleFromPacket(packet, cpusUsed);
    GetsingleFromPacket(packet, cpuMax);
    GetsingleFromPacket(packet, memGBUsed);
    GetsingleFromPacket(packet, memGBMax);
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
procedure TFileServiceClient.GetCommandResourceConsumption_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6022);
    packet.AddVariant(0);
    packet.AddString('FileService');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.GetCommandResourceConsumption_Response(out cpusUsed:single; out cpuMax:single; out memGBUsed:single; out memGBMax:single):boolean;
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
    GetsingleFromPacket(packet, cpusUsed);
    GetsingleFromPacket(packet, cpuMax);
    GetsingleFromPacket(packet, memGBUsed);
    GetsingleFromPacket(packet, memGBMax);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.EndExeCommand(handle:int64):string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6023);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6023:TFileServiceClient.EndExeCommand');{$ENDIF}
    Writeint64ToPacket(packet, handle);
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
procedure TFileServiceClient.EndExeCommand_Async(handle:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6023);
    packet.AddVariant(0);
    packet.AddString('FileService');
    Writeint64ToPacket(packet, handle);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.EndExeCommand_Response():string;
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
function TFileServiceClient.GetEXECommandStatus(handle:int64; out status:string; out finished:boolean; out consoleCapture:string):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6024);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6024:TFileServiceClient.GetEXECommandStatus');{$ENDIF}
    Writeint64ToPacket(packet, handle);
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
    GetstringFromPacket(packet, status);
    GetbooleanFromPacket(packet, finished);
    GetstringFromPacket(packet, consoleCapture);
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
procedure TFileServiceClient.GetEXECommandStatus_Async(handle:int64);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6024);
    packet.AddVariant(0);
    packet.AddString('FileService');
    Writeint64ToPacket(packet, handle);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.GetEXECommandStatus_Response(out status:string; out finished:boolean; out consoleCapture:string):boolean;
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
    GetbooleanFromPacket(packet, finished);
    GetstringFromPacket(packet, consoleCapture);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.StartExeCommandEx(sPath:string; sProgram:string; sParams:string; cpus:single; memgb:single; ext_resources:string):int64;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6025);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6025:TFileServiceClient.StartExeCommandEx');{$ENDIF}
    WritestringToPacket(packet, sPath);
    WritestringToPacket(packet, sProgram);
    WritestringToPacket(packet, sParams);
    WritesingleToPacket(packet, cpus);
    WritesingleToPacket(packet, memgb);
    WritestringToPacket(packet, ext_resources);
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
procedure TFileServiceClient.StartExeCommandEx_Async(sPath:string; sProgram:string; sParams:string; cpus:single; memgb:single; ext_resources:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6025);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, sPath);
    WritestringToPacket(packet, sProgram);
    WritestringToPacket(packet, sParams);
    WritesingleToPacket(packet, cpus);
    WritesingleToPacket(packet, memgb);
    WritestringToPacket(packet, ext_resources);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.StartExeCommandEx_Response():int64;
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
function TFileServiceClient.GetCommandResourceConsumptionEx(out cpusUsed:single; out cpuMax:single; out memGBUsed:single; out memGBMax:single; out gpusUsed:single; out gpuMax:single; out ext_resources:string):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6026);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6026:TFileServiceClient.GetCommandResourceConsumptionEx');{$ENDIF}
    if not Transact(packet) then raise ECritical.create('transaction failure');
    if not packet.result then raise ECritical.create('server error: '+packet.message);
    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);
    GetbooleanFromPacket(packet, result);
    GetsingleFromPacket(packet, cpusUsed);
    GetsingleFromPacket(packet, cpuMax);
    GetsingleFromPacket(packet, memGBUsed);
    GetsingleFromPacket(packet, memGBMax);
    GetsingleFromPacket(packet, gpusUsed);
    GetsingleFromPacket(packet, gpuMax);
    GetstringFromPacket(packet, ext_resources);
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
procedure TFileServiceClient.GetCommandResourceConsumptionEx_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6026);
    packet.AddVariant(0);
    packet.AddString('FileService');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.GetCommandResourceConsumptionEx_Response(out cpusUsed:single; out cpuMax:single; out memGBUsed:single; out memGBMax:single; out gpusUsed:single; out gpuMax:single; out ext_resources:string):boolean;
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
    GetsingleFromPacket(packet, cpusUsed);
    GetsingleFromPacket(packet, cpuMax);
    GetsingleFromPacket(packet, memGBUsed);
    GetsingleFromPacket(packet, memGBMax);
    GetsingleFromPacket(packet, gpusUsed);
    GetsingleFromPacket(packet, gpuMax);
    GetstringFromPacket(packet, ext_resources);
  finally
    packet.free;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.GetGPUList():string;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6027);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6027:TFileServiceClient.GetGPUList');{$ENDIF}
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
procedure TFileServiceClient.GetGPUList_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6027);
    packet.AddVariant(0);
    packet.AddString('FileService');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.GetGPUList_Response():string;
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
function TFileServiceClient.GetGPUCount():integer;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6028);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6028:TFileServiceClient.GetGPUCount');{$ENDIF}
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
procedure TFileServiceClient.GetGPUCount_Async();
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6028);
    packet.AddVariant(0);
    packet.AddString('FileService');
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.GetGPUCount_Response():integer;
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
function TFileServiceClient.StartExeCommandExFFMPEG(sPath:string; sProgram:string; sParams:string; sGPUParams:string; cpus:single; memgb:single; gpu:single):int64;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6029);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6029:TFileServiceClient.StartExeCommandExFFMPEG');{$ENDIF}
    WritestringToPacket(packet, sPath);
    WritestringToPacket(packet, sProgram);
    WritestringToPacket(packet, sParams);
    WritestringToPacket(packet, sGPUParams);
    WritesingleToPacket(packet, cpus);
    WritesingleToPacket(packet, memgb);
    WritesingleToPacket(packet, gpu);
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
procedure TFileServiceClient.StartExeCommandExFFMPEG_Async(sPath:string; sProgram:string; sParams:string; sGPUParams:string; cpus:single; memgb:single; gpu:single);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6029);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, sPath);
    WritestringToPacket(packet, sProgram);
    WritestringToPacket(packet, sParams);
    WritestringToPacket(packet, sGPUParams);
    WritesingleToPacket(packet, cpus);
    WritesingleToPacket(packet, memgb);
    WritesingleToPacket(packet, gpu);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.StartExeCommandExFFMPEG_Response():int64;
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
function TFileServiceClient.FileExists(sFile:string):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6030);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6030:TFileServiceClient.FileExists');{$ENDIF}
    WritestringToPacket(packet, sFile);
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
procedure TFileServiceClient.FileExists_Async(sFile:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6030);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, sFile);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.FileExists_Response():boolean;
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
function TFileServiceClient.PathExists(sPath:string):boolean;
var
  packet: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try try
    packet.AddVariant($6031);
    packet.AddVariant(0);
    packet.AddString('FileService');
{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log('RDTP$6031:TFileServiceClient.PathExists');{$ENDIF}
    WritestringToPacket(packet, sPath);
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
procedure TFileServiceClient.PathExists_Async(sPath:string);
var
  packet,outpacket: TRDTPPacket;
begin
  if not connect then
     raise ETransportError.create('Failed to connect');
  packet := NeedPacket;
  try
    packet.AddVariant($6031);
    packet.AddVariant(0);
    packet.AddString('FileService');
    WritestringToPacket(packet, sPath);
    BeginTransact2(packet, outpacket,nil, false);
  except
    on E:Exception do begin
      e.message := 'RDTP Call Failed:'+e.message;
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceClient.PathExists_Response():boolean;
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



function TFileServiceClient.DispatchCallback: boolean;
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



procedure TFileServiceClient.Init;
begin
  inherited;
  ServiceName := 'FileService';
end;

end.


