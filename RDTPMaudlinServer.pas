unit RDTPMaudlinServer;
{GEN}
{TYPE SERVER}
{CLASS TMaudlinServer}
{ANCESTOR TRDTPProcessor}
{IMPLIB RDTPMaudlinServerImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE RDTPMaudlinRQs.txt}
{SERVICENAME Maudlin}
{END}
interface


uses
  typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, packethelpers, debug, RDTPServerList;



type
  TMaudlinServerBase = class(TRDTPProcessor)
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
  RDTPMaudlinServerImplib, ImpJunk;



{ TMaudlinServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TMaudlinServerBase.create;
begin
  inherited;
  ServiceName := 'Maudlin';
end;

destructor TMaudlinServerBase.destroy;
begin

  inherited;
end;


function TMaudlinServerBase.Dispatch: boolean;
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


