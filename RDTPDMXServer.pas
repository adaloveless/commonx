unit RDTPDMXServer;
{GEN}
{TYPE SERVER}
{CLASS TDMXServer}
{ANCESTOR TRDTPProcessor}
{IMPLIB RDTPDMXServerImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE RDTPDMXRQs.txt}
{END}
interface


uses
  typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, rdtpprocessorformysql, packethelpers, debug, RDTPServerList;



type
  TDMXServerBase = class(TRDTPProcessor)
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
  RDTPDMXServerImplib, ImpJunk;



{ TDMXServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TDMXServerBase.create;
begin
  inherited;
  ServiceName := 'DMX';
end;

destructor TDMXServerBase.destroy;
begin

  inherited;
end;


function TDMXServerBase.Dispatch: boolean;
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


