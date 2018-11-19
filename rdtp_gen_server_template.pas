unit rdtp_gen_server_template;

interface

uses
  {USES}typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, rdtpprocessorformysql, packethelpers, debug, RDTPServerList;



type
  T__RDTP_SERVER_CLASSBase = class{ANCESTOR}
  private
    {PRIVATE_MEMBERS}
  protected
    {PROTECTED_MEMBERS}
  public
    constructor Create;override;
    destructor Destroy;override;

    {CLASS_INTERFACE}
    {PUBLIC_MEMBERS}

    function Dispatch: boolean;override;
  end;

procedure LocalDebug(s: string; sfilter: string = '');

implementation
uses
  {IMPLIB}ImpJunk;
{IMPLEMENTATION2}


{ T__RDTP_CLIENT_CLASS }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor T__RDTP_SERVER_CLASSBase.create;
begin
  inherited;
  ServiceName := '{SERVICENAME}';
end;

destructor T__RDTP_SERVER_CLASSBase.destroy;
begin

  inherited;
end;


function T__RDTP_SERVER_CLASSBase.Dispatch: boolean;
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
  {CLASS_IMPLEMENTATION}
  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


