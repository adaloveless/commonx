unit rdtp_gen_client_template;

interface

uses
  {USES}packet, betterobject, systemx, genericRDTPClient, variants, packethelpers, debug, typex, exceptions;



type
  T__RDTP_CLIENT_CLASS = class(TGenericRDTPClient)
  public
    procedure Init;override;
    destructor Destroy;override;

    {CLASS_INTERFACE}

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



{ T__RDTP_CLIENT_CLASS }


destructor T__RDTP_CLIENT_CLASS.destroy;
begin

  inherited;
end;


{CLASS_IMPLEMENTATION}


function T__RDTP_CLIENT_CLASS.DispatchCallback: boolean;
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
  {IMPLEMENTATION2}
  end;

  if not result then
    result := Inherited DispatchCallback;
end;



procedure T__RDTP_CLIENT_CLASS.Init;
begin
  inherited;
  ServiceName := '{SERVICENAME}';
end;

end.


