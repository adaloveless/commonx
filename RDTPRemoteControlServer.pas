unit RDTPRemoteControlServer;
{GEN}
{TYPE SERVER}
{CLASS TRemoteControlServer}
{ANCESTOR TRDTPProcessor}
{IMPLIB RDTPRemoteControlServerImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE RDTPRemoteControlRQs.txt}
{END}
interface


uses
  classes, typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, rdtpprocessorformysql, packethelpers, debug, RDTPServerList;



type
  TRemoteControlServerBase = class(TRDTPProcessor)
  private
    
    procedure RQ_HANDLE_MouseClick_integer_integer_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_MouseDown_integer_integer_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_MouseMove_integer_integer_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_MouseUp_integer_integer_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_ScreenShot(proc: TRDTPProcessor);

  protected
    

  public
    constructor Create;override;
    destructor Destroy;override;

    

    
    function RQ_MouseClick(x:integer; y:integer; button:integer):boolean;overload;virtual;abstract;
    function RQ_MouseDown(x:integer; y:integer; button:integer):boolean;overload;virtual;abstract;
    function RQ_MouseMove(x:integer; y:integer; button:integer):boolean;overload;virtual;abstract;
    function RQ_MouseUp(x:integer; y:integer; button:integer):boolean;overload;virtual;abstract;
    function RQ_ScreenShot():TStream;overload;virtual;abstract;


    function Dispatch: boolean;override;
  end;

procedure LocalDebug(s: string; sfilter: string = '');

implementation
uses
  RDTPRemoteControlServerImplib, ImpJunk;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRemoteControlServerBase.RQ_HANDLE_MouseClick_integer_integer_integer(proc: TRDTPProcessor);
var
  res: boolean;
  x:integer;
  y:integer;
  button:integer;
begin
  GetintegerFromPacket(proc.request, x);
  GetintegerFromPacket(proc.request, y);
  GetintegerFromPacket(proc.request, button);
  res := RQ_MouseClick(x, y, button);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRemoteControlServerBase.RQ_HANDLE_MouseDown_integer_integer_integer(proc: TRDTPProcessor);
var
  res: boolean;
  x:integer;
  y:integer;
  button:integer;
begin
  GetintegerFromPacket(proc.request, x);
  GetintegerFromPacket(proc.request, y);
  GetintegerFromPacket(proc.request, button);
  res := RQ_MouseDown(x, y, button);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRemoteControlServerBase.RQ_HANDLE_MouseMove_integer_integer_integer(proc: TRDTPProcessor);
var
  res: boolean;
  x:integer;
  y:integer;
  button:integer;
begin
  GetintegerFromPacket(proc.request, x);
  GetintegerFromPacket(proc.request, y);
  GetintegerFromPacket(proc.request, button);
  res := RQ_MouseMove(x, y, button);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRemoteControlServerBase.RQ_HANDLE_MouseUp_integer_integer_integer(proc: TRDTPProcessor);
var
  res: boolean;
  x:integer;
  y:integer;
  button:integer;
begin
  GetintegerFromPacket(proc.request, x);
  GetintegerFromPacket(proc.request, y);
  GetintegerFromPacket(proc.request, button);
  res := RQ_MouseUp(x, y, button);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRemoteControlServerBase.RQ_HANDLE_ScreenShot(proc: TRDTPProcessor);
var
  res: TStream;
begin
  res := RQ_ScreenShot();
  WriteTStreamToPacket(proc.response, res);
end;



{ TRemoteControlServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TRemoteControlServerBase.create;
begin
  inherited;
  ServiceName := 'RemoteControl';
end;

destructor TRemoteControlServerBase.destroy;
begin

  inherited;
end;


function TRemoteControlServerBase.Dispatch: boolean;
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
  
    //MouseClick
    $7000:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of MouseClick','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_MouseClick_integer_integer_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of MouseClick','RDTPCALLS');
{$ENDIF}
      end;

    //MouseDown
    $7001:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of MouseDown','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_MouseDown_integer_integer_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of MouseDown','RDTPCALLS');
{$ENDIF}
      end;

    //MouseMove
    $7002:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of MouseMove','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_MouseMove_integer_integer_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of MouseMove','RDTPCALLS');
{$ENDIF}
      end;

    //MouseUp
    $7003:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of MouseUp','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_MouseUp_integer_integer_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of MouseUp','RDTPCALLS');
{$ENDIF}
      end;

    //ScreenShot
    $7004:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of ScreenShot','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_ScreenShot(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of ScreenShot','RDTPCALLS');
{$ENDIF}
      end;

  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


