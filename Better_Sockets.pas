unit better_Sockets;

interface

uses
  better_sockets, classes;

type
  TBetterCustomIPClient = TCustomIPClient;
  ESocketError = web.win.sockets.ESocketError;
  TBetterTCpClient = TTcpClient;
  TBetterTcpServer = TTcpServer;





procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Digital Tundra', [TBetterTCPServer, TBetterTCPClient]);
end;



end.
