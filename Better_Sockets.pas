unit better_Sockets;

interface
{$DEFINE FIX}
uses
{$IFDEF FIX}
  sockfix,
{$ELSE}
  sockets,
{$ENDIF}
  typex,
  classes;

type
  TBetterCustomIPClient = TCustomIPClient;
  ESocketError = ECritical;
  TBetterTCpClient = TTcpClient;
  TBetterTcpServer = TTcpServer;





procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Digital Tundra', [TBetterTCPServer, TBetterTCPClient]);
end;



end.
