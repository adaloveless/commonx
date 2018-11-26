unit sockfixreg;

interface

procedure Register;

implementation

uses
  DesignIntf, SocketsEditor, System.Classes, sockfix;

resourcestring
  sCat = 'WinSock';

procedure Register;
begin
  RegisterComponents(sCat, [TTcpClient, TTcpServer, TUdpSocket]);
  RegisterPropertyEditor(TypeInfo(TSocketPort), TIpSocket, 'Port', TSocketPortProperty);
end;

end.
