unit better_Sockets;

interface

uses
  sockfix, classes;

type
  TBetterCustomIPClient = TCustomIPClient;
  ESocketError = sockfix.ESocketError;
  TBetterTCpClient = TTcpClient;
  TBetterTcpServer = TTcpServer;





procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Digital Tundra', [TBetterTCPServer, TBetterTCPClient]);
end;



end.
