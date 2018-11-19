unit RDTPArchiveServer;
{GEN}
{TYPE SERVER}
{CLASS TRDTPArchiveServer}
{ANCESTOR TRDTPProcessor}
{IMPLIB RDTPArchiveServerImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE RDTPArchiveRQs.txt}
{END}
interface


uses
  typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, rdtpprocessorformysql, packethelpers, debug, RDTPServerList;



type
  TRDTPArchiveServerBase = class(TRDTPProcessor)
  private
    
    procedure RQ_HANDLE_LogThis_string_int64_int64_int64_int64_TDynByteArray(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetLog_string_TDateTime_int64_int64_TDynByteArray(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetNextLogID_string_int64_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_Flush(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetLogRev_string_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetLogRevs_string_int64_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetStoredParam_string_string_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetStoredParam_string_string_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_ListArchives(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetZoneChecksum_string_int64_int64_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetZoneStackReport_string_int64_boolean(proc: TRDTPProcessor);
    procedure RQ_HANDLE_NextZoneHint_string_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetArcVatCheckSum_string_int64_int64(proc: TRDTPProcessor);

  protected
    

  public
    constructor Create;override;
    destructor Destroy;override;

    

    
    function RQ_LogThis(sArchive:string; fromid:int64; toid:int64; startblock:int64; blocklength:int64; data:TDynByteArray):boolean;overload;virtual;abstract;
    function RQ_GetLog(sArchive:string; pin:TDateTime; startblock:int64; blocklength:int64; out data:TDynByteArray):int64;overload;virtual;abstract;
    function RQ_GetNextLogID(sArchive:string; zone:int64; ids_to_reserve:int64):TDateTime;overload;virtual;abstract;
    function RQ_Flush():boolean;overload;virtual;abstract;
    function RQ_GetLogRev(sArchive:string; idx:int64):int64;overload;virtual;abstract;
    function RQ_GetLogRevs(sArchive:string; startidx:int64; count:int64):TDynInt64Array;overload;virtual;abstract;
    function RQ_GetStoredParam(sArchive:string; sParamName:string; sDefault:string):string;overload;virtual;abstract;
    procedure RQ_SetStoredParam(sArchive:string; sParamName:string; sValue:string);overload;virtual;abstract;
    function RQ_ListArchives():string;overload;virtual;abstract;
    function RQ_GetZoneChecksum(sArchive:string; z:int64; out iSum:int64; out iXor:int64):boolean;overload;virtual;abstract;
    function RQ_GetZoneStackReport(sArchive:string; z:int64; fullstack:boolean):string;overload;virtual;abstract;
    procedure RQ_NextZoneHint(sArchive:string; z:int64);overload;virtual;abstract;
    function RQ_GetArcVatCheckSum(sArchive:string; zStart:int64; zCount:int64):int64;overload;virtual;abstract;


    function Dispatch: boolean;override;
  end;

procedure LocalDebug(s: string; sfilter: string = '');

implementation
uses
  RDTPArchiveServerImplib, ImpJunk;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPArchiveServerBase.RQ_HANDLE_LogThis_string_int64_int64_int64_int64_TDynByteArray(proc: TRDTPProcessor);
var
  res: boolean;
  sArchive:string;
  fromid:int64;
  toid:int64;
  startblock:int64;
  blocklength:int64;
  data:TDynByteArray;
begin
  GetstringFromPacket(proc.request, sArchive);
  Getint64FromPacket(proc.request, fromid);
  Getint64FromPacket(proc.request, toid);
  Getint64FromPacket(proc.request, startblock);
  Getint64FromPacket(proc.request, blocklength);
  GetTDynByteArrayFromPacket(proc.request, data);
  res := RQ_LogThis(sArchive, fromid, toid, startblock, blocklength, data);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPArchiveServerBase.RQ_HANDLE_GetLog_string_TDateTime_int64_int64_TDynByteArray(proc: TRDTPProcessor);
var
  res: int64;
  sArchive:string;
  pin:TDateTime;
  startblock:int64;
  blocklength:int64;
  data:TDynByteArray;
begin
  GetstringFromPacket(proc.request, sArchive);
  GetTDateTimeFromPacket(proc.request, pin);
  Getint64FromPacket(proc.request, startblock);
  Getint64FromPacket(proc.request, blocklength);
  res := RQ_GetLog(sArchive, pin, startblock, blocklength, data);
  Writeint64ToPacket(proc.response, res);
  WriteTDynByteArrayToPacket(proc.response, data);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPArchiveServerBase.RQ_HANDLE_GetNextLogID_string_int64_int64(proc: TRDTPProcessor);
var
  res: TDateTime;
  sArchive:string;
  zone:int64;
  ids_to_reserve:int64;
begin
  GetstringFromPacket(proc.request, sArchive);
  Getint64FromPacket(proc.request, zone);
  Getint64FromPacket(proc.request, ids_to_reserve);
  res := RQ_GetNextLogID(sArchive, zone, ids_to_reserve);
  WriteTDateTimeToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPArchiveServerBase.RQ_HANDLE_Flush(proc: TRDTPProcessor);
var
  res: boolean;
begin
  res := RQ_Flush();
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPArchiveServerBase.RQ_HANDLE_GetLogRev_string_int64(proc: TRDTPProcessor);
var
  res: int64;
  sArchive:string;
  idx:int64;
begin
  GetstringFromPacket(proc.request, sArchive);
  Getint64FromPacket(proc.request, idx);
  res := RQ_GetLogRev(sArchive, idx);
  Writeint64ToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPArchiveServerBase.RQ_HANDLE_GetLogRevs_string_int64_int64(proc: TRDTPProcessor);
var
  res: TDynInt64Array;
  sArchive:string;
  startidx:int64;
  count:int64;
begin
  GetstringFromPacket(proc.request, sArchive);
  Getint64FromPacket(proc.request, startidx);
  Getint64FromPacket(proc.request, count);
  res := RQ_GetLogRevs(sArchive, startidx, count);
  WriteTDynInt64ArrayToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPArchiveServerBase.RQ_HANDLE_GetStoredParam_string_string_string(proc: TRDTPProcessor);
var
  res: string;
  sArchive:string;
  sParamName:string;
  sDefault:string;
begin
  GetstringFromPacket(proc.request, sArchive);
  GetstringFromPacket(proc.request, sParamName);
  GetstringFromPacket(proc.request, sDefault);
  res := RQ_GetStoredParam(sArchive, sParamName, sDefault);
  WritestringToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPArchiveServerBase.RQ_HANDLE_SetStoredParam_string_string_string(proc: TRDTPProcessor);
var
  sArchive:string;
  sParamName:string;
  sValue:string;
begin
  GetstringFromPacket(proc.request, sArchive);
  GetstringFromPacket(proc.request, sParamName);
  GetstringFromPacket(proc.request, sValue);
  RQ_SetStoredParam(sArchive, sParamName, sValue);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPArchiveServerBase.RQ_HANDLE_ListArchives(proc: TRDTPProcessor);
var
  res: string;
begin
  res := RQ_ListArchives();
  WritestringToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPArchiveServerBase.RQ_HANDLE_GetZoneChecksum_string_int64_int64_int64(proc: TRDTPProcessor);
var
  res: boolean;
  sArchive:string;
  z:int64;
  iSum:int64;
  iXor:int64;
begin
  GetstringFromPacket(proc.request, sArchive);
  Getint64FromPacket(proc.request, z);
  res := RQ_GetZoneChecksum(sArchive, z, iSum, iXor);
  WritebooleanToPacket(proc.response, res);
  Writeint64ToPacket(proc.response, iSum);
  Writeint64ToPacket(proc.response, iXor);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPArchiveServerBase.RQ_HANDLE_GetZoneStackReport_string_int64_boolean(proc: TRDTPProcessor);
var
  res: string;
  sArchive:string;
  z:int64;
  fullstack:boolean;
begin
  GetstringFromPacket(proc.request, sArchive);
  Getint64FromPacket(proc.request, z);
  GetbooleanFromPacket(proc.request, fullstack);
  res := RQ_GetZoneStackReport(sArchive, z, fullstack);
  WritestringToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPArchiveServerBase.RQ_HANDLE_NextZoneHint_string_int64(proc: TRDTPProcessor);
var
  sArchive:string;
  z:int64;
begin
  GetstringFromPacket(proc.request, sArchive);
  Getint64FromPacket(proc.request, z);
  RQ_NextZoneHint(sArchive, z);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPArchiveServerBase.RQ_HANDLE_GetArcVatCheckSum_string_int64_int64(proc: TRDTPProcessor);
var
  res: int64;
  sArchive:string;
  zStart:int64;
  zCount:int64;
begin
  GetstringFromPacket(proc.request, sArchive);
  Getint64FromPacket(proc.request, zStart);
  Getint64FromPacket(proc.request, zCount);
  res := RQ_GetArcVatCheckSum(sArchive, zStart, zCount);
  Writeint64ToPacket(proc.response, res);
end;



{ TRDTPArchiveServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TRDTPArchiveServerBase.create;
begin
  inherited;
  ServiceName := 'Archive';
end;

destructor TRDTPArchiveServerBase.destroy;
begin

  inherited;
end;


function TRDTPArchiveServerBase.Dispatch: boolean;
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
  
    //LogThis
    $5500:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of LogThis','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_LogThis_string_int64_int64_int64_int64_TDynByteArray(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of LogThis','RDTPCALLS');
{$ENDIF}
      end;

    //GetLog
    $5501:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetLog','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetLog_string_TDateTime_int64_int64_TDynByteArray(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetLog','RDTPCALLS');
{$ENDIF}
      end;

    //GetNextLogID
    $5502:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetNextLogID','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetNextLogID_string_int64_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetNextLogID','RDTPCALLS');
{$ENDIF}
      end;

    //Flush
    $5503:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Flush','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Flush(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Flush','RDTPCALLS');
{$ENDIF}
      end;

    //GetLogRev
    $5504:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetLogRev','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetLogRev_string_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetLogRev','RDTPCALLS');
{$ENDIF}
      end;

    //GetLogRevs
    $5505:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetLogRevs','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetLogRevs_string_int64_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetLogRevs','RDTPCALLS');
{$ENDIF}
      end;

    //GetStoredParam
    $5506:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetStoredParam','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetStoredParam_string_string_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetStoredParam','RDTPCALLS');
{$ENDIF}
      end;

    //SetStoredParam
    $5507:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetStoredParam','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetStoredParam_string_string_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetStoredParam','RDTPCALLS');
{$ENDIF}
      end;

    //ListArchives
    $5508:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of ListArchives','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_ListArchives(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of ListArchives','RDTPCALLS');
{$ENDIF}
      end;

    //GetZoneChecksum
    $550E:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetZoneChecksum','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetZoneChecksum_string_int64_int64_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetZoneChecksum','RDTPCALLS');
{$ENDIF}
      end;

    //GetZoneStackReport
    $550F:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetZoneStackReport','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetZoneStackReport_string_int64_boolean(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetZoneStackReport','RDTPCALLS');
{$ENDIF}
      end;

    //NextZoneHint
    $5510:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of NextZoneHint','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_NextZoneHint_string_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of NextZoneHint','RDTPCALLS');
{$ENDIF}
      end;

    //GetArcVatCheckSum
    $5511:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetArcVatCheckSum','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetArcVatCheckSum_string_int64_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetArcVatCheckSum','RDTPCALLS');
{$ENDIF}
      end;

  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


