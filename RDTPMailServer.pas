unit RDTPMailServer;
{GEN}
{TYPE SERVER}
{CLASS TMailServer}
{ANCESTOR TRDTPProcessor}
{IMPLIB RDTPMailServerImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE RDTPMailRQs.txt}
{END}
interface


uses
  RDTPSpamPacketHelpers, typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, packethelpers, debug, RDTPServerList;



type
  TMailServerBase = class(TRDTPProcessor)
  private
    
    procedure RQ_HANDLE_GetBlacklistStatus_string_boolean(proc: TRDTPProcessor);
    procedure RQ_HANDLE_Blacklist_string_string_integer_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetSpamProbability_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_BayesianCommit_string_boolean_real(proc: TRDTPProcessor);
    procedure RQ_HANDLE_Debug_string_boolean(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetBlacklistCountIncludingContent_string_string_string_boolean(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetWordCommonality_string_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_MailCommand_string_string(proc: TRDTPProcessor);

  protected
    

  public
    constructor Create;override;
    destructor Destroy;override;

    

    
    function RQ_GetBlacklistStatus(sIP:string; bFullCheck:boolean):integer;overload;virtual;abstract;
    procedure RQ_Blacklist(sIP:string; sReason:string; iCode:integer; iDays:integer);overload;virtual;abstract;
    function RQ_GetSpamProbability(sText:string):real;overload;virtual;abstract;
    function RQ_BayesianCommit(sText:string; bSpam:boolean; iMultiplier:real):boolean;overload;virtual;abstract;
    procedure RQ_Debug(sText:string; bSay:boolean);overload;virtual;abstract;
    function RQ_GetBlacklistCountIncludingContent(sIP:string; sPLainText:string; sHTMLText:string; bFullCheck:boolean):integer;overload;virtual;abstract;
    procedure RQ_SetWordCommonality(sWord:string; iCommonality:int64);overload;virtual;abstract;
    procedure RQ_MailCommand(sSubject:string; sBody:string);overload;virtual;abstract;


    function Dispatch: boolean;override;
  end;

procedure LocalDebug(s: string; sfilter: string = '');

implementation
uses
  RDTPMailServerImplib, ImpJunk;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TMailServerBase.RQ_HANDLE_GetBlacklistStatus_string_boolean(proc: TRDTPProcessor);
var
  res: integer;
  sIP:string;
  bFullCheck:boolean;
begin
  GetstringFromPacket(proc.request, sIP);
  GetbooleanFromPacket(proc.request, bFullCheck);
  res := RQ_GetBlacklistStatus(sIP, bFullCheck);
  WriteintegerToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TMailServerBase.RQ_HANDLE_Blacklist_string_string_integer_integer(proc: TRDTPProcessor);
var
  sIP:string;
  sReason:string;
  iCode:integer;
  iDays:integer;
begin
  GetstringFromPacket(proc.request, sIP);
  GetstringFromPacket(proc.request, sReason);
  GetintegerFromPacket(proc.request, iCode);
  GetintegerFromPacket(proc.request, iDays);
  RQ_Blacklist(sIP, sReason, iCode, iDays);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TMailServerBase.RQ_HANDLE_GetSpamProbability_string(proc: TRDTPProcessor);
var
  res: real;
  sText:string;
begin
  GetstringFromPacket(proc.request, sText);
  res := RQ_GetSpamProbability(sText);
  WriterealToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TMailServerBase.RQ_HANDLE_BayesianCommit_string_boolean_real(proc: TRDTPProcessor);
var
  res: boolean;
  sText:string;
  bSpam:boolean;
  iMultiplier:real;
begin
  GetstringFromPacket(proc.request, sText);
  GetbooleanFromPacket(proc.request, bSpam);
  GetrealFromPacket(proc.request, iMultiplier);
  res := RQ_BayesianCommit(sText, bSpam, iMultiplier);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TMailServerBase.RQ_HANDLE_Debug_string_boolean(proc: TRDTPProcessor);
var
  sText:string;
  bSay:boolean;
begin
  GetstringFromPacket(proc.request, sText);
  GetbooleanFromPacket(proc.request, bSay);
  RQ_Debug(sText, bSay);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TMailServerBase.RQ_HANDLE_GetBlacklistCountIncludingContent_string_string_string_boolean(proc: TRDTPProcessor);
var
  res: integer;
  sIP:string;
  sPLainText:string;
  sHTMLText:string;
  bFullCheck:boolean;
begin
  GetstringFromPacket(proc.request, sIP);
  GetstringFromPacket(proc.request, sPLainText);
  GetstringFromPacket(proc.request, sHTMLText);
  GetbooleanFromPacket(proc.request, bFullCheck);
  res := RQ_GetBlacklistCountIncludingContent(sIP, sPLainText, sHTMLText, bFullCheck);
  WriteintegerToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TMailServerBase.RQ_HANDLE_SetWordCommonality_string_int64(proc: TRDTPProcessor);
var
  sWord:string;
  iCommonality:int64;
begin
  GetstringFromPacket(proc.request, sWord);
  Getint64FromPacket(proc.request, iCommonality);
  RQ_SetWordCommonality(sWord, iCommonality);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TMailServerBase.RQ_HANDLE_MailCommand_string_string(proc: TRDTPProcessor);
var
  sSubject:string;
  sBody:string;
begin
  GetstringFromPacket(proc.request, sSubject);
  GetstringFromPacket(proc.request, sBody);
  RQ_MailCommand(sSubject, sBody);
  proc.ForgetResult := true
end;



{ TMailServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TMailServerBase.create;
begin
  inherited;
  ServiceName := 'Mail';
end;

destructor TMailServerBase.destroy;
begin

  inherited;
end;


function TMailServerBase.Dispatch: boolean;
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
  
    //GetBlacklistStatus
    $6005:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetBlacklistStatus','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetBlacklistStatus_string_boolean(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetBlacklistStatus','RDTPCALLS');
{$ENDIF}
      end;

    //Blacklist
    $6006:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Blacklist','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Blacklist_string_string_integer_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Blacklist','RDTPCALLS');
{$ENDIF}
      end;

    //GetSpamProbability
    $6007:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetSpamProbability','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetSpamProbability_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetSpamProbability','RDTPCALLS');
{$ENDIF}
      end;

    //BayesianCommit
    $6008:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of BayesianCommit','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_BayesianCommit_string_boolean_real(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of BayesianCommit','RDTPCALLS');
{$ENDIF}
      end;

    //Debug
    $6009:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Debug','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Debug_string_boolean(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Debug','RDTPCALLS');
{$ENDIF}
      end;

    //GetBlacklistCountIncludingContent
    $6010:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetBlacklistCountIncludingContent','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetBlacklistCountIncludingContent_string_string_string_boolean(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetBlacklistCountIncludingContent','RDTPCALLS');
{$ENDIF}
      end;

    //SetWordCommonality
    $6011:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetWordCommonality','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetWordCommonality_string_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetWordCommonality','RDTPCALLS');
{$ENDIF}
      end;

    //MailCommand
    $6012:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of MailCommand','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_MailCommand_string_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of MailCommand','RDTPCALLS');
{$ENDIF}
      end;

  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


