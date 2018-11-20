unit FileServiceServer;
{GEN}
{TYPE SERVER}
{CLASS TFileServiceServer}
{ANCESTOR TRDTPProcessor}
{IMPLIB FileServiceServerImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE FileServiceRQs.txt}
{END}

interface


uses
  Dir, DirFile, Classes, rdtp_file, typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, rdtpprocessorformysql, packethelpers, debug, RDTPServerList;



type
  TFileServiceServerBase = class(TRDTPProcessor)
  private
    
    procedure RQ_HANDLE_PutFile_TFileTransferReference(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetFile_TFileTransferReference(proc: TRDTPProcessor);
    procedure RQ_HANDLE_OpenFile_string_TFileTransferReference_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_CloseFile_TFileTransferReference(proc: TRDTPProcessor);
    procedure RQ_HANDLE_Dir_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetUpgradePath_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetUpgradeScript_string_integer_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetUpgradeVersion_string_boolean(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetFileChecksum_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_BuildHueFile_string_real(proc: TRDTPProcessor);
    procedure RQ_HANDLE_DeleteFile_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_Execute_string_string_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_BuildHueFileFromStream_TStream_string_real(proc: TRDTPProcessor);
    procedure RQ_HANDLE_EchoStream_TStream(proc: TRDTPProcessor);
    procedure RQ_HANDLE_AppendTextFile_string_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetFileSize_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_ExecuteAndCapture_string_string_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetFileList_string_string_integer_integer(proc: TRDTPProcessor);

  protected
    

  public
    constructor Create;override;
    destructor Destroy;override;

    

    
    function RQ_PutFile(oFile:TFileTransferReference):boolean;overload;virtual;abstract;
    function RQ_GetFile(var oFile:TFileTransferReference):boolean;overload;virtual;abstract;
    function RQ_OpenFile(sFile:string; out oFile:TFileTransferReference; iMode:integer):boolean;overload;virtual;abstract;
    function RQ_CloseFile(oFile:TFileTransferReference):boolean;overload;virtual;abstract;
    function RQ_Dir(sRemotePath:string):TDirectory;overload;virtual;abstract;
    function RQ_GetUpgradePath(sProgramName:string):string;overload;virtual;abstract;
    function RQ_GetUpgradeScript(sProgramName:string; iFromVersion:integer; iToVersion:integer):string;overload;virtual;abstract;
    function RQ_GetUpgradeVersion(sProgramName:string; bBeta:boolean):integer;overload;virtual;abstract;
    function RQ_GetFileChecksum(sFile:string):TAdvancedFileChecksum;overload;virtual;abstract;
    function RQ_BuildHueFile(sFile:string; LengthInSeconds:real):boolean;overload;virtual;abstract;
    function RQ_DeleteFile(sFile:string):boolean;overload;virtual;abstract;
    function RQ_Execute(sPath:string; sProgram:string; sParams:string):boolean;overload;virtual;abstract;
    function RQ_BuildHueFileFromStream(str:TStream; sExt:string; LengthInSeconds:real):TStream;overload;virtual;abstract;
    function RQ_EchoStream(strin:TStream):TStream;overload;virtual;abstract;
    function RQ_AppendTextFile(filename:string; text:string):boolean;overload;virtual;abstract;
    function RQ_GetFileSize(filename:string):int64;overload;virtual;abstract;
    function RQ_ExecuteAndCapture(sPath:string; sProgram:string; sParams:string):string;overload;virtual;abstract;
    function RQ_GetFileList(sRemotePath:string; sFileSpec:string; attrmask:integer; attrresult:integer):TRemoteFileArray;overload;virtual;abstract;


    function Dispatch: boolean;override;
  end;

procedure LocalDebug(s: string; sfilter: string = '');

implementation
uses
  FileServiceServerImplib, ImpJunk;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_PutFile_TFileTransferReference(proc: TRDTPProcessor);
var
  res: boolean;
  oFile:TFileTransferReference;
begin
  GetTFileTransferReferenceFromPacket(proc.request, oFile);
  res := RQ_PutFile(oFile);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_GetFile_TFileTransferReference(proc: TRDTPProcessor);
var
  res: boolean;
  oFile:TFileTransferReference;
begin
  GetTFileTransferReferenceFromPacket(proc.request, oFile);
  res := RQ_GetFile(oFile);
  WritebooleanToPacket(proc.response, res);
  WriteTFileTransferReferenceToPacket(proc.response, oFile);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_OpenFile_string_TFileTransferReference_integer(proc: TRDTPProcessor);
var
  res: boolean;
  sFile:string;
  oFile:TFileTransferReference;
  iMode:integer;
begin
  GetstringFromPacket(proc.request, sFile);
  GetintegerFromPacket(proc.request, iMode);
  res := RQ_OpenFile(sFile, oFile, iMode);
  WritebooleanToPacket(proc.response, res);
  WriteTFileTransferReferenceToPacket(proc.response, oFile);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_CloseFile_TFileTransferReference(proc: TRDTPProcessor);
var
  res: boolean;
  oFile:TFileTransferReference;
begin
  GetTFileTransferReferenceFromPacket(proc.request, oFile);
  res := RQ_CloseFile(oFile);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_Dir_string(proc: TRDTPProcessor);
var
  res: TDirectory;
  sRemotePath:string;
begin
  GetstringFromPacket(proc.request, sRemotePath);
  res := RQ_Dir(sRemotePath);
  WriteTDirectoryToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_GetUpgradePath_string(proc: TRDTPProcessor);
var
  res: string;
  sProgramName:string;
begin
  GetstringFromPacket(proc.request, sProgramName);
  res := RQ_GetUpgradePath(sProgramName);
  WritestringToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_GetUpgradeScript_string_integer_integer(proc: TRDTPProcessor);
var
  res: string;
  sProgramName:string;
  iFromVersion:integer;
  iToVersion:integer;
begin
  GetstringFromPacket(proc.request, sProgramName);
  GetintegerFromPacket(proc.request, iFromVersion);
  GetintegerFromPacket(proc.request, iToVersion);
  res := RQ_GetUpgradeScript(sProgramName, iFromVersion, iToVersion);
  WritestringToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_GetUpgradeVersion_string_boolean(proc: TRDTPProcessor);
var
  res: integer;
  sProgramName:string;
  bBeta:boolean;
begin
  GetstringFromPacket(proc.request, sProgramName);
  GetbooleanFromPacket(proc.request, bBeta);
  res := RQ_GetUpgradeVersion(sProgramName, bBeta);
  WriteintegerToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_GetFileChecksum_string(proc: TRDTPProcessor);
var
  res: TAdvancedFileChecksum;
  sFile:string;
begin
  GetstringFromPacket(proc.request, sFile);
  res := RQ_GetFileChecksum(sFile);
  WriteTAdvancedFileChecksumToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_BuildHueFile_string_real(proc: TRDTPProcessor);
var
  res: boolean;
  sFile:string;
  LengthInSeconds:real;
begin
  GetstringFromPacket(proc.request, sFile);
  GetrealFromPacket(proc.request, LengthInSeconds);
  res := RQ_BuildHueFile(sFile, LengthInSeconds);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_DeleteFile_string(proc: TRDTPProcessor);
var
  res: boolean;
  sFile:string;
begin
  GetstringFromPacket(proc.request, sFile);
  res := RQ_DeleteFile(sFile);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_Execute_string_string_string(proc: TRDTPProcessor);
var
  res: boolean;
  sPath:string;
  sProgram:string;
  sParams:string;
begin
  GetstringFromPacket(proc.request, sPath);
  GetstringFromPacket(proc.request, sProgram);
  GetstringFromPacket(proc.request, sParams);
  res := RQ_Execute(sPath, sProgram, sParams);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_BuildHueFileFromStream_TStream_string_real(proc: TRDTPProcessor);
var
  res: TStream;
  str:TStream;
  sExt:string;
  LengthInSeconds:real;
begin
  GetTStreamFromPacket(proc.request, str);
  GetstringFromPacket(proc.request, sExt);
  GetrealFromPacket(proc.request, LengthInSeconds);
  res := RQ_BuildHueFileFromStream(str, sExt, LengthInSeconds);
  WriteTStreamToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_EchoStream_TStream(proc: TRDTPProcessor);
var
  res: TStream;
  strin:TStream;
begin
  GetTStreamFromPacket(proc.request, strin);
  res := RQ_EchoStream(strin);
  WriteTStreamToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_AppendTextFile_string_string(proc: TRDTPProcessor);
var
  res: boolean;
  filename:string;
  text:string;
begin
  GetstringFromPacket(proc.request, filename);
  GetstringFromPacket(proc.request, text);
  res := RQ_AppendTextFile(filename, text);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_GetFileSize_string(proc: TRDTPProcessor);
var
  res: int64;
  filename:string;
begin
  GetstringFromPacket(proc.request, filename);
  res := RQ_GetFileSize(filename);
  Writeint64ToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_ExecuteAndCapture_string_string_string(proc: TRDTPProcessor);
var
  res: string;
  sPath:string;
  sProgram:string;
  sParams:string;
begin
  GetstringFromPacket(proc.request, sPath);
  GetstringFromPacket(proc.request, sProgram);
  GetstringFromPacket(proc.request, sParams);
  res := RQ_ExecuteAndCapture(sPath, sProgram, sParams);
  WritestringToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TFileServiceServerBase.RQ_HANDLE_GetFileList_string_string_integer_integer(proc: TRDTPProcessor);
var
  res: TRemoteFileArray;
  sRemotePath:string;
  sFileSpec:string;
  attrmask:integer;
  attrresult:integer;
begin
  GetstringFromPacket(proc.request, sRemotePath);
  GetstringFromPacket(proc.request, sFileSpec);
  GetintegerFromPacket(proc.request, attrmask);
  GetintegerFromPacket(proc.request, attrresult);
  res := RQ_GetFileList(sRemotePath, sFileSpec, attrmask, attrresult);
  WriteTRemoteFileArrayToPacket(proc.response, res);
end;



{ TFileServiceServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TFileServiceServerBase.create;
begin
  inherited;
  ServiceName := 'FileService';
end;

destructor TFileServiceServerBase.destroy;
begin

  inherited;
end;


function TFileServiceServerBase.Dispatch: boolean;
var
  iRQ: integer;
begin

  result := false;

  iRQ := request.data[0];
  request.seqseek(3);
  case iRQ of
    0: begin
        result := true;
//        beeper.Beep(100,100);
       end;
  
    //PutFile
    $6000:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of PutFile','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_PutFile_TFileTransferReference(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of PutFile','RDTPCALLS');
{$ENDIF}
      end;

    //GetFile
    $6001:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetFile','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetFile_TFileTransferReference(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetFile','RDTPCALLS');
{$ENDIF}
      end;

    //OpenFile
    $6002:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of OpenFile','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_OpenFile_string_TFileTransferReference_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of OpenFile','RDTPCALLS');
{$ENDIF}
      end;

    //CloseFile
    $6003:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of CloseFile','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_CloseFile_TFileTransferReference(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of CloseFile','RDTPCALLS');
{$ENDIF}
      end;

    //Dir
    $6004:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Dir','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Dir_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Dir','RDTPCALLS');
{$ENDIF}
      end;

    //GetUpgradePath
    $6005:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetUpgradePath','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetUpgradePath_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetUpgradePath','RDTPCALLS');
{$ENDIF}
      end;

    //GetUpgradeScript
    $6006:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetUpgradeScript','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetUpgradeScript_string_integer_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetUpgradeScript','RDTPCALLS');
{$ENDIF}
      end;

    //GetUpgradeVersion
    $6007:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetUpgradeVersion','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetUpgradeVersion_string_boolean(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetUpgradeVersion','RDTPCALLS');
{$ENDIF}
      end;

    //GetFileChecksum
    $6008:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetFileChecksum','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetFileChecksum_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetFileChecksum','RDTPCALLS');
{$ENDIF}
      end;

    //BuildHueFile
    $6009:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of BuildHueFile','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_BuildHueFile_string_real(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of BuildHueFile','RDTPCALLS');
{$ENDIF}
      end;

    //DeleteFile
    $6010:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of DeleteFile','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_DeleteFile_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of DeleteFile','RDTPCALLS');
{$ENDIF}
      end;

    //Execute
    $6011:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Execute','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Execute_string_string_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Execute','RDTPCALLS');
{$ENDIF}
      end;

    //BuildHueFileFromStream
    $6012:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of BuildHueFileFromStream','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_BuildHueFileFromStream_TStream_string_real(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of BuildHueFileFromStream','RDTPCALLS');
{$ENDIF}
      end;

    //EchoStream
    $6013:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of EchoStream','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_EchoStream_TStream(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of EchoStream','RDTPCALLS');
{$ENDIF}
      end;

    //AppendTextFile
    $6014:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of AppendTextFile','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_AppendTextFile_string_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of AppendTextFile','RDTPCALLS');
{$ENDIF}
      end;

    //GetFileSize
    $6015:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetFileSize','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetFileSize_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetFileSize','RDTPCALLS');
{$ENDIF}
      end;

    //ExecuteAndCapture
    $6016:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of ExecuteAndCapture','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_ExecuteAndCapture_string_string_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of ExecuteAndCapture','RDTPCALLS');
{$ENDIF}
      end;

    //GetFileList
    $6017:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetFileList','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetFileList_string_string_integer_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetFileList','RDTPCALLS');
{$ENDIF}
      end;

  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


