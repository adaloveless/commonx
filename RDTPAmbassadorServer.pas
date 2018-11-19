unit RDTPAmbassadorServer;
{GEN}
{TYPE SERVER}
{CLASS TRDTPAmbassadorServer}
{ANCESTOR TRDTPProcessor}
{IMPLIB RDTPAmbassadorServerImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE RDTPAmbassadorRQs.txt}
{END}
interface


uses
  typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, rdtpprocessorformysql, packethelpers, debug, RDTPServerList;



type
  TRDTPAmbassadorServerBase = class(TRDTPProcessor)
  private
    
    procedure RQ_HANDLE_RouteMe_string_string_int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_HasRoute_string_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_CrossPing_string_string(proc: TRDTPProcessor);

  protected
    

  public
    constructor Create;override;
    destructor Destroy;override;

    

    
    procedure RQ_RouteMe(ComputerNAme:string; ApplicationName:string; myport:int64);overload;virtual;abstract;
    function RQ_HasRoute(ComputerNAme:string; ApplicationName:string):boolean;overload;virtual;abstract;
    procedure RQ_CrossPing(ComputerName:string; ApplicationNAme:string);overload;virtual;abstract;


    function Dispatch: boolean;override;
  end;

procedure LocalDebug(s: string; sfilter: string = '');

implementation
uses
  RDTPAmbassadorServerImplib, ImpJunk;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPAmbassadorServerBase.RQ_HANDLE_RouteMe_string_string_int64(proc: TRDTPProcessor);
var
  ComputerNAme:string;
  ApplicationName:string;
  myport:int64;
begin
  GetstringFromPacket(proc.request, ComputerNAme);
  GetstringFromPacket(proc.request, ApplicationName);
  Getint64FromPacket(proc.request, myport);
  RQ_RouteMe(ComputerNAme, ApplicationName, myport);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPAmbassadorServerBase.RQ_HANDLE_HasRoute_string_string(proc: TRDTPProcessor);
var
  res: boolean;
  ComputerNAme:string;
  ApplicationName:string;
begin
  GetstringFromPacket(proc.request, ComputerNAme);
  GetstringFromPacket(proc.request, ApplicationName);
  res := RQ_HasRoute(ComputerNAme, ApplicationName);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPAmbassadorServerBase.RQ_HANDLE_CrossPing_string_string(proc: TRDTPProcessor);
var
  ComputerName:string;
  ApplicationNAme:string;
begin
  GetstringFromPacket(proc.request, ComputerName);
  GetstringFromPacket(proc.request, ApplicationNAme);
  RQ_CrossPing(ComputerName, ApplicationNAme);
  proc.ForgetResult := true
end;



{ TRDTPAmbassadorServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TRDTPAmbassadorServerBase.create;
begin
  inherited;
  ServiceName := 'Ambassador';
end;

destructor TRDTPAmbassadorServerBase.destroy;
begin

  inherited;
end;


function TRDTPAmbassadorServerBase.Dispatch: boolean;
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
  
    //RouteMe
    $7000:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of RouteMe','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_RouteMe_string_string_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of RouteMe','RDTPCALLS');
{$ENDIF}
      end;

    //HasRoute
    $7001:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of HasRoute','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_HasRoute_string_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of HasRoute','RDTPCALLS');
{$ENDIF}
      end;

    //CrossPing
    $7002:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of CrossPing','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_CrossPing_string_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of CrossPing','RDTPCALLS');
{$ENDIF}
      end;

  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


