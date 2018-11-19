unit SimpleAbstractPrivateServerSocket;

interface

uses
  helpers_winsock, typex, simpleAbstractConnection, classes, better_sockets, sysutils;

type
  ESimpleAbstractPrivateServerSocketException = class(Exception);


  TSimpleAbstractPrivateServerSocket = class(TSimpleAbstractConnection)
  strict protected
    function DoCheckForData: boolean;override;
    function DoWaitForData(timeout: cardinal): boolean;override;
  public
    socket: TBetterCustomIPClient;//<----------------------------------------------
    function Connect: boolean; override;
    procedure Disconnect; override;
    function GetConnected: boolean;override;
    function DoReadData(buffer: pbyte; length: integer): integer;override;
    function DoSendData(buffer: pbyte; length: integer): integer;override;

  end;


implementation

{ TSimpleAbstractPrivateServerSocket }

function TSimpleAbstractPrivateServerSocket.Connect: boolean;
begin
  raise ESimpleAbstractPrivateServerSocketException.create(classname+' represents a server socket, so calling Connect is irrelevant.');
end;

procedure TSimpleAbstractPrivateServerSocket.Disconnect;
begin
  inherited;
  socket.close;
end;

function TSimpleAbstractPrivateServerSocket.DoCheckForData: boolean;
begin
  raise ECritical.create('no implemented for this class');
end;

function TSimpleAbstractPrivateServerSocket.DoReadData(buffer: pbyte;
  length: integer): integer;
begin
  result := socket.ReceiveBuf(buffer[0], length);
end;

function TSimpleAbstractPrivateServerSocket.DoSendData(buffer: pbyte;
  length: integer): integer;
begin
  result := socket.SendBuf(buffer[0], length);
end;

function TSimpleAbstractPrivateServerSocket.GetConnected: boolean;
begin
  result := socket.Connected;
end;

function TSimpleAbstractPrivateServerSocket.DoWaitForData(
  timeout: cardinal): boolean;
begin
  result := BetterWaitForData(Socket, timeout);
end;

end.
