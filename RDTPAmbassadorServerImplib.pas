unit RDTPAmbassadorServerImplib;
{GEN}
{TYPE IMPLIB}
{RQFILE RDTPAmbassadorRQs.txt}
{END}
interface
uses
  systemx, rdtpprocessor, orderlyinit, rdtpserverlist, RDTPAmbassadorServer, governor;
type
  TRDTPAmbassadorServer = class(TRDTPAmbassadorServerBase)
  private
  protected
  public
{INTERFACE_START}
    procedure RQ_RouteMe(ComputerNAme:string; ApplicationName:string; myport:int64);overload;override;
    function RQ_HasRoute(ComputerNAme:string; ApplicationName:string):boolean;overload;override;
    procedure RQ_CrossPing(ComputerName:string; ApplicationNAme:string);overload;override;

{INTERFACE_END}
  end;
implementation
procedure oinit;
begin
  RDTPServers.RegisterRDTPProcessor('Ambassador', TRDTPAmbassadorServer);
end;

procedure ofinal;
begin
  //noimp
end;

{ TRDTPAmbassadorServer }

procedure TRDTPAmbassadorServer.RQ_CrossPing(ComputerName,
  ApplicationNAme: string);
begin
  inherited;

end;

function TRDTPAmbassadorServer.RQ_HasRoute(ComputerNAme,
  ApplicationName: string): boolean;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TRDTPAmbassadorServer.RQ_RouteMe(ComputerNAme,
  ApplicationName: string; myport: int64);
begin
  inherited;

  G_Governor.Add(ComputerName, ApplicationNAme, myport);

end;

initialization
  init.RegisterProcs('RDTPAmbassadorServerImplib', oinit, ofinal, 'RDTPServerList');
end.
