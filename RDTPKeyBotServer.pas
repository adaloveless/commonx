unit RDTPKeyBotServer;
{GEN}
{TYPE SERVER}
{CLASS TKeyBotServer}
{USES RDTPProcessorForKEYBOT}
{ANCESTOR TRDTPProcessorForKEYBOT}
{IMPLIB RDTPKeyBotServerImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE RDTPKeyBotRQs.txt}
{SERVICENAME KeyBot}
{END}
interface


uses
  RDTPProcessorForKEYBOT, typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, rdtpprocessorformysql, packethelpers, debug, RDTPServerList;



type
  TKeyBotServerBase = class(TRDTPProcessorForKEYBOT)
  private
    
    procedure RQ_HANDLE_GetNextID_integer(proc: TRDTPProcessorForKEYBOT);
    procedure RQ_HANDLE_GetNextID_str_string(proc: TRDTPProcessorForKEYBOT);
    procedure RQ_HANDLE_SetNextID_str_string_int64(proc: TRDTPProcessorForKEYBOT);
    procedure RQ_HANDLE_SetNextID_integer_int64(proc: TRDTPProcessorForKEYBOT);

  protected
    

  public
    constructor Create;override;
    destructor Destroy;override;

    

    
    function RQ_GetNextID(iID:integer):int64;overload;virtual;abstract;
    function RQ_GetNextID_str(sID:string):int64;overload;virtual;abstract;
    function RQ_SetNextID_str(sID:string; val:int64):boolean;overload;virtual;abstract;
    function RQ_SetNextID(iID:integer; val:int64):boolean;overload;virtual;abstract;


    function Dispatch: boolean;override;
  end;

procedure LocalDebug(s: string; sfilter: string = '');

implementation
uses
  RDTPKeyBotServerImplib, ImpJunk;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TKeyBotServerBase.RQ_HANDLE_GetNextID_integer(proc: TRDTPProcessorForKEYBOT);
var
  res: int64;
  iID:integer;
begin
  GetintegerFromPacket(proc.request, iID);
  res := RQ_GetNextID(iID);
  Writeint64ToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TKeyBotServerBase.RQ_HANDLE_GetNextID_str_string(proc: TRDTPProcessorForKEYBOT);
var
  res: int64;
  sID:string;
begin
  GetstringFromPacket(proc.request, sID);
  res := RQ_GetNextID_str(sID);
  Writeint64ToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TKeyBotServerBase.RQ_HANDLE_SetNextID_str_string_int64(proc: TRDTPProcessorForKEYBOT);
var
  res: boolean;
  sID:string;
  val:int64;
begin
  GetstringFromPacket(proc.request, sID);
  Getint64FromPacket(proc.request, val);
  res := RQ_SetNextID_str(sID, val);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TKeyBotServerBase.RQ_HANDLE_SetNextID_integer_int64(proc: TRDTPProcessorForKEYBOT);
var
  res: boolean;
  iID:integer;
  val:int64;
begin
  GetintegerFromPacket(proc.request, iID);
  Getint64FromPacket(proc.request, val);
  res := RQ_SetNextID(iID, val);
  WritebooleanToPacket(proc.response, res);
end;



{ TKeyBotServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TKeyBotServerBase.create;
begin
  inherited;
  ServiceName := 'KeyBot';
end;

destructor TKeyBotServerBase.destroy;
begin

  inherited;
end;


function TKeyBotServerBase.Dispatch: boolean;
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
  
    //GetNextID
    $0050:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetNextID','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetNextID_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetNextID','RDTPCALLS');
{$ENDIF}
      end;

    //GetNextID_str
    $0051:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetNextID_str','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetNextID_str_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetNextID_str','RDTPCALLS');
{$ENDIF}
      end;

    //SetNextID_str
    $0052:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetNextID_str','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetNextID_str_string_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetNextID_str','RDTPCALLS');
{$ENDIF}
      end;

    //SetNextID
    $0053:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetNextID','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetNextID_integer_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetNextID','RDTPCALLS');
{$ENDIF}
      end;

  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


