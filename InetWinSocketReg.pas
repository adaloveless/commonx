{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2012 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit InetWinSocketReg;

interface

procedure Register;

implementation

uses
  DesignIntf, SocketsEditor, System.Classes, Web.Win.Sockets;

resourcestring
  sInternet = 'Internet';

procedure Register;
begin
  RegisterComponents(sInternet, [TTcpClient, TTcpServer, TUdpSocket]);
  // Sockets property editors
  RegisterPropertyEditor(TypeInfo(TSocketPort), TIpSocket, 'Port', TSocketPortProperty);
end;

end.
