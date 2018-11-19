unit RDTPDMXClient;
{GEN}
{TYPE CLIENT}
{CLASS TDMXClient}
{IMPLIB RDTPDMXClientImplib}
{TEMPLATE RDTP_gen_client_template.pas}
{RQFILE RDTPDMXRQs.txt}
{END}
interface


uses
  packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



type
  TDMXClient = class(TGenericRDTPClient)
  public
    procedure Init;override;
    destructor Destroy;override;

    


    function DispatchCallback: boolean;override;

  end;

procedure LocalDebug(s: string; sFilter: string = '');


implementation

uses
  sysutils;

procedure LocalDebug(s: string; sFilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;



{ TDMXClient }


destructor TDMXClient.destroy;
begin

  inherited;
end;





function TDMXClient.DispatchCallback: boolean;
var
  iRQ: integer;
begin

  result := false;

  iRQ := callback.request.data[0];
  callback.request.seqseek(3);
  case iRQ of
    0: begin
        //beeper.Beep(100,100);
        result := true;
       end;
  
  end;

  if not result then
    result := Inherited DispatchCallback;
end;



procedure TDMXClient.Init;
begin
  inherited;
  ServiceName := 'DMX';
end;

end.


