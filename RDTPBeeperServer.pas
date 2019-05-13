unit RDTPBeeperServer;
{GEN}
{TYPE SERVER}
{CLASS TRDTPBeeperServer}
{ANCESTOR TRDTPProcessor}
{IMPLIB RDTPBeeperImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE RDTPBeeperRQs.txt}

{END}
interface


uses
  typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, packethelpers, debug, RDTPServerList;



type
  TRDTPBeeperServerBase = class(TRDTPProcessor)
  private
    
    procedure RQ_HANDLE_Beep_integer_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_BeepForget_integer_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_TestInteger_integer_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_TestInt64_Int64_Int64(proc: TRDTPProcessor);
    procedure RQ_HANDLE_TestString_string_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_HelloTroy_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_HelloIvana_string_integer_integer_integer_integer(proc: TRDTPProcessor);

  protected
    

  public
    constructor Create;override;
    destructor Destroy;override;

    

    
    function RQ_Beep(freq:integer; duration:integer):boolean;overload;virtual;abstract;
    procedure RQ_BeepForget(freq:integer; duration:integer);overload;virtual;abstract;
    function RQ_TestInteger(a:integer; b:integer):integer;overload;virtual;abstract;
    function RQ_TestInt64(a:Int64; b:Int64):Int64;overload;virtual;abstract;
    function RQ_TestString(a:string; b:string):string;overload;virtual;abstract;
    function RQ_HelloTroy(ErectPenis:integer):string;overload;virtual;abstract;
    function RQ_HelloIvana(WriteToLog:string; freq:integer; duration:integer; attack:integer; release:integer):string;overload;virtual;abstract;


    function Dispatch: boolean;override;
  end;

procedure LocalDebug(s: string; sfilter: string = '');

implementation
uses
  RDTPBeeperImplib, ImpJunk;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPBeeperServerBase.RQ_HANDLE_Beep_integer_integer(proc: TRDTPProcessor);
var
  res: boolean;
  freq:integer;
  duration:integer;
begin
  GetintegerFromPacket(proc.request, freq);
  GetintegerFromPacket(proc.request, duration);
  res := RQ_Beep(freq, duration);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPBeeperServerBase.RQ_HANDLE_BeepForget_integer_integer(proc: TRDTPProcessor);
var
  freq:integer;
  duration:integer;
begin
  GetintegerFromPacket(proc.request, freq);
  GetintegerFromPacket(proc.request, duration);
  RQ_BeepForget(freq, duration);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPBeeperServerBase.RQ_HANDLE_TestInteger_integer_integer(proc: TRDTPProcessor);
var
  res: integer;
  a:integer;
  b:integer;
begin
  GetintegerFromPacket(proc.request, a);
  GetintegerFromPacket(proc.request, b);
  res := RQ_TestInteger(a, b);
  WriteintegerToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPBeeperServerBase.RQ_HANDLE_TestInt64_Int64_Int64(proc: TRDTPProcessor);
var
  res: Int64;
  a:Int64;
  b:Int64;
begin
  GetInt64FromPacket(proc.request, a);
  GetInt64FromPacket(proc.request, b);
  res := RQ_TestInt64(a, b);
  WriteInt64ToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPBeeperServerBase.RQ_HANDLE_TestString_string_string(proc: TRDTPProcessor);
var
  res: string;
  a:string;
  b:string;
begin
  GetstringFromPacket(proc.request, a);
  GetstringFromPacket(proc.request, b);
  res := RQ_TestString(a, b);
  WritestringToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPBeeperServerBase.RQ_HANDLE_HelloTroy_integer(proc: TRDTPProcessor);
var
  res: string;
  ErectPenis:integer;
begin
  GetintegerFromPacket(proc.request, ErectPenis);
  res := RQ_HelloTroy(ErectPenis);
  WritestringToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPBeeperServerBase.RQ_HANDLE_HelloIvana_string_integer_integer_integer_integer(proc: TRDTPProcessor);
var
  res: string;
  WriteToLog:string;
  freq:integer;
  duration:integer;
  attack:integer;
  release:integer;
begin
  GetstringFromPacket(proc.request, WriteToLog);
  GetintegerFromPacket(proc.request, freq);
  GetintegerFromPacket(proc.request, duration);
  GetintegerFromPacket(proc.request, attack);
  GetintegerFromPacket(proc.request, release);
  res := RQ_HelloIvana(WriteToLog, freq, duration, attack, release);
  WritestringToPacket(proc.response, res);
end;



{ TRDTPBeeperServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TRDTPBeeperServerBase.create;
begin
  inherited;
  ServiceName := 'BEEPER';
end;

destructor TRDTPBeeperServerBase.destroy;
begin

  inherited;
end;


function TRDTPBeeperServerBase.Dispatch: boolean;
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
  
    //Beep
    $1001:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Beep','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Beep_integer_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Beep','RDTPCALLS');
{$ENDIF}
      end;

    //BeepForget
    $1002:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of BeepForget','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_BeepForget_integer_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of BeepForget','RDTPCALLS');
{$ENDIF}
      end;

    //TestInteger
    $1003:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of TestInteger','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_TestInteger_integer_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of TestInteger','RDTPCALLS');
{$ENDIF}
      end;

    //TestInt64
    $1004:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of TestInt64','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_TestInt64_Int64_Int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of TestInt64','RDTPCALLS');
{$ENDIF}
      end;

    //TestString
    $1005:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of TestString','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_TestString_string_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of TestString','RDTPCALLS');
{$ENDIF}
      end;

    //HelloTroy
    $1006:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of HelloTroy','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_HelloTroy_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of HelloTroy','RDTPCALLS');
{$ENDIF}
      end;

    //HelloIvana
    $1007:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of HelloIvana','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_HelloIvana_string_integer_integer_integer_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of HelloIvana','RDTPCALLS');
{$ENDIF}
      end;

  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


