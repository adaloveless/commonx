unit better_Sockets;

{$IFDEF MSWINDOWS}
{$DEFINE ENABLETHIS}
{$ENDIF}

interface
{$IFDEF ENABLETHIS}
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
{$ENDIF ENABLETHIS}

implementation
{$IFDEF ENABLETHIS}

procedure Register;
begin
  RegisterComponents('Digital Tundra', [TBetterTCPServer, TBetterTCPClient]);
end;
{$ENDIF}



end.
