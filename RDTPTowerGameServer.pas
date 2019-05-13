unit RDTPTowerGameServer;
{GEN}
{TYPE SERVER}
{CLASS TTowerGameServer}
{ANCESTOR TRDTPProcessor}
{IMPLIB RDTPTowerGameServerImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE RDTPTowerGameRQs.txt}
{END}
interface


uses
  GameList, typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, packethelpers, debug, RDTPServerList;



type
  TTowerGameServerBase = class(TRDTPProcessor)
  private
    
    procedure RQ_HANDLE_Login_string_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_Logout_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_JoinBestGame_int64_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetGameStatus_int64_string_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetUserIDForSession_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_CrossStreams_int64_int64_TGameStream_int64_TGameStream_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_ShareClientDetails_int64_int64_double(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetSimplePlayerList_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_StorePerformanceMetrics_string_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_TransportLayerTest_string(proc: TRDTPProcessor);

  protected
    

  public
    constructor Create;override;
    destructor Destroy;override;

    

    
    function RQ_Login(UserName:string; Password:string):int64;overload;virtual;abstract;
    procedure RQ_Logout(SessionID:int64);overload;virtual;abstract;
    function RQ_JoinBestGame(SessionID:int64; out iGameID:int64):boolean;overload;virtual;abstract;
    function RQ_GetGameStatus(iGameID:int64; out status:string; out PLayerCount:integer):boolean;overload;virtual;abstract;
    function RQ_GetUserIDForSession(SessionID:int64):int64;overload;virtual;abstract;
    function RQ_CrossStreams(iGameID:int64; iSessionID:int64; InStream:TGameStream; TotalEventsPreviouslyReceived:int64; out OutStream:TGameStream; out TotalEventsReceivedFromClient:int64):boolean;overload;virtual;abstract;
    function RQ_ShareClientDetails(iGameID:int64; iSessionID:int64; LocalGameTime:double):boolean;overload;virtual;abstract;
    function RQ_GetSimplePlayerList(iGameID:int64):string;overload;virtual;abstract;
    function RQ_StorePerformanceMetrics(sUserName:string; sData:string):string;overload;virtual;abstract;
    function RQ_TransportLayerTest(sSomethingToConvertToLowercase:string):string;overload;virtual;abstract;


    function Dispatch: boolean;override;
  end;

procedure LocalDebug(s: string; sfilter: string = '');

implementation
uses
  RDTPTowerGameServerImplib, ImpJunk;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TTowerGameServerBase.RQ_HANDLE_Login_string_string(proc: TRDTPProcessor);
var
  res: int64;
  UserName:string;
  Password:string;
begin
  GetstringFromPacket(proc.request, UserName);
  GetstringFromPacket(proc.request, Password);
  res := RQ_Login(UserName, Password);
  Writeint64ToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TTowerGameServerBase.RQ_HANDLE_Logout_int64(proc: TRDTPProcessor);
var
  SessionID:int64;
begin
  Getint64FromPacket(proc.request, SessionID);
  RQ_Logout(SessionID);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TTowerGameServerBase.RQ_HANDLE_JoinBestGame_int64_int64(proc: TRDTPProcessor);
var
  res: boolean;
  SessionID:int64;
  iGameID:int64;
begin
  Getint64FromPacket(proc.request, SessionID);
  res := RQ_JoinBestGame(SessionID, iGameID);
  WritebooleanToPacket(proc.response, res);
  Writeint64ToPacket(proc.response, iGameID);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TTowerGameServerBase.RQ_HANDLE_GetGameStatus_int64_string_integer(proc: TRDTPProcessor);
var
  res: boolean;
  iGameID:int64;
  status:string;
  PLayerCount:integer;
begin
  Getint64FromPacket(proc.request, iGameID);
  res := RQ_GetGameStatus(iGameID, status, PLayerCount);
  WritebooleanToPacket(proc.response, res);
  WritestringToPacket(proc.response, status);
  WriteintegerToPacket(proc.response, PLayerCount);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TTowerGameServerBase.RQ_HANDLE_GetUserIDForSession_int64(proc: TRDTPProcessor);
var
  res: int64;
  SessionID:int64;
begin
  Getint64FromPacket(proc.request, SessionID);
  res := RQ_GetUserIDForSession(SessionID);
  Writeint64ToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TTowerGameServerBase.RQ_HANDLE_CrossStreams_int64_int64_TGameStream_int64_TGameStream_int64(proc: TRDTPProcessor);
var
  res: boolean;
  iGameID:int64;
  iSessionID:int64;
  InStream:TGameStream;
  TotalEventsPreviouslyReceived:int64;
  OutStream:TGameStream;
  TotalEventsReceivedFromClient:int64;
begin
  Getint64FromPacket(proc.request, iGameID);
  Getint64FromPacket(proc.request, iSessionID);
  GetTGameStreamFromPacket(proc.request, InStream);
  Getint64FromPacket(proc.request, TotalEventsPreviouslyReceived);
  res := RQ_CrossStreams(iGameID, iSessionID, InStream, TotalEventsPreviouslyReceived, OutStream, TotalEventsReceivedFromClient);
  WritebooleanToPacket(proc.response, res);
  WriteTGameStreamToPacket(proc.response, OutStream);
  Writeint64ToPacket(proc.response, TotalEventsReceivedFromClient);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TTowerGameServerBase.RQ_HANDLE_ShareClientDetails_int64_int64_double(proc: TRDTPProcessor);
var
  res: boolean;
  iGameID:int64;
  iSessionID:int64;
  LocalGameTime:double;
begin
  Getint64FromPacket(proc.request, iGameID);
  Getint64FromPacket(proc.request, iSessionID);
  GetdoubleFromPacket(proc.request, LocalGameTime);
  res := RQ_ShareClientDetails(iGameID, iSessionID, LocalGameTime);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TTowerGameServerBase.RQ_HANDLE_GetSimplePlayerList_int64(proc: TRDTPProcessor);
var
  res: string;
  iGameID:int64;
begin
  Getint64FromPacket(proc.request, iGameID);
  res := RQ_GetSimplePlayerList(iGameID);
  WritestringToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TTowerGameServerBase.RQ_HANDLE_StorePerformanceMetrics_string_string(proc: TRDTPProcessor);
var
  res: string;
  sUserName:string;
  sData:string;
begin
  GetstringFromPacket(proc.request, sUserName);
  GetstringFromPacket(proc.request, sData);
  res := RQ_StorePerformanceMetrics(sUserName, sData);
  WritestringToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TTowerGameServerBase.RQ_HANDLE_TransportLayerTest_string(proc: TRDTPProcessor);
var
  res: string;
  sSomethingToConvertToLowercase:string;
begin
  GetstringFromPacket(proc.request, sSomethingToConvertToLowercase);
  res := RQ_TransportLayerTest(sSomethingToConvertToLowercase);
  WritestringToPacket(proc.response, res);
end;



{ TTowerGameServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TTowerGameServerBase.create;
begin
  inherited;
  ServiceName := 'TOWERGAME';
end;

destructor TTowerGameServerBase.destroy;
begin

  inherited;
end;


function TTowerGameServerBase.Dispatch: boolean;
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
  
    //Login
    $6666:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Login','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Login_string_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Login','RDTPCALLS');
{$ENDIF}
      end;

    //Logout
    $6667:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Logout','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Logout_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Logout','RDTPCALLS');
{$ENDIF}
      end;

    //JoinBestGame
    $6668:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of JoinBestGame','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_JoinBestGame_int64_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of JoinBestGame','RDTPCALLS');
{$ENDIF}
      end;

    //GetGameStatus
    $6669:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetGameStatus','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetGameStatus_int64_string_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetGameStatus','RDTPCALLS');
{$ENDIF}
      end;

    //GetUserIDForSession
    $6670:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetUserIDForSession','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetUserIDForSession_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetUserIDForSession','RDTPCALLS');
{$ENDIF}
      end;

    //CrossStreams
    $6671:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of CrossStreams','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_CrossStreams_int64_int64_TGameStream_int64_TGameStream_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of CrossStreams','RDTPCALLS');
{$ENDIF}
      end;

    //ShareClientDetails
    $6672:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of ShareClientDetails','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_ShareClientDetails_int64_int64_double(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of ShareClientDetails','RDTPCALLS');
{$ENDIF}
      end;

    //GetSimplePlayerList
    $6673:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetSimplePlayerList','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetSimplePlayerList_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetSimplePlayerList','RDTPCALLS');
{$ENDIF}
      end;

    //StorePerformanceMetrics
    $6674:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of StorePerformanceMetrics','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_StorePerformanceMetrics_string_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of StorePerformanceMetrics','RDTPCALLS');
{$ENDIF}
      end;

    //TransportLayerTest
    $6675:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of TransportLayerTest','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_TransportLayerTest_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of TransportLayerTest','RDTPCALLS');
{$ENDIF}
      end;

  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


