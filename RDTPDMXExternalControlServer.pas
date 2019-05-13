unit RDTPDMXExternalControlServer;
{GEN}
{TYPE SERVER}
{CLASS TDMXExternalControlServer}
{ANCESTOR TRDTPProcessor}
{IMPLIB RDTPDMXExternalControlServerImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE RDTPDMXExternalControlRQs.txt}
{END}
interface


uses
  typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, packethelpers, debug, RDTPServerList;



type
  TDMXExternalControlServerBase = class(TRDTPProcessor)
  private
    

  protected
    

  public
    constructor Create;override;
    destructor Destroy;override;

    

    


    function Dispatch: boolean;override;
  end;

procedure LocalDebug(s: string; sfilter: string = '');

implementation
uses
  RDTPDMXExternalControlServerImplib, ImpJunk;



{ TDMXExternalControlServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TDMXExternalControlServerBase.create;
begin
  inherited;
  ServiceName := '';
end;

destructor TDMXExternalControlServerBase.destroy;
begin

  inherited;
end;


function TDMXExternalControlServerBase.Dispatch: boolean;
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
  
  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


