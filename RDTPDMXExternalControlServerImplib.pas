unit RDTPDMXExternalControlServerImplib;
{GEN}
{TYPE IMPLIB}
{RQFILE RDTPDMXExternalControlRQs.txt}
{END}
interface


uses
  packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, packethelpers, debug, RDTPServerList;



type
  TDMXExternalControlServer = class(TRDTPProcessor)
  private
  protected
  public
{INTERFACE_START}

{INTERFACE_END}
  end;
implementation
end.
