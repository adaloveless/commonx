unit udmDumbProxy;

interface

uses
  System.SysUtils, System.Classes, Web.Win.Sockets, simplewinsock, helpers.sockets, dumbproxy, typex;

type
  TDataModule1 = class(TDataModule)
    TcpServer1: TTcpServer;
    procedure TcpServer1Accept(Sender: TObject; ClientSocket: TCustomIpClient);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TDataModule1.TcpServer1Accept(Sender: TObject;
  ClientSocket: TCustomIpClient);
var
  sws: TsimpleWinsockConnection;
  head: TDumbproxyHead;
  atemp: array[0..1499] of byte;
  iGot: ni;
begin
  //all connections to this server are expected to say "hello" with a
  //TDumbProxyHead header.  Let's get it

  Socket_GuaranteeRead(clientsocket, @head, sizeof(head));
  //check that it is valid
  if not head.isheadervalid then
    exit;

  //try to connect to expected target
  sws := TSimpleWinsockconnection.Create;
  try
    sws.HostName := head.TargetIPString;
    sws.EndPoint := head.TargetPort.ToString;
    sws.connect;
    while sws.connected do begin
      //read stuff from back-end, give to front-end
      if sws.WaitForData(6) then begin
        iGot := sws.ReadData(@atemp[0], sizeof(atemp), false);
        if iGot = 0 then exit;//socket failed/dropped if clientsocket says "true" to WaitForData but datacount is 0, that indicates failure
        Socket_GuaranteeWrite(clientsocket, @atemp[0], iGot);
      end;

      //read stuff from front-end, give to back-end
      if ClientSocket.WaitForData(6) then begin
        iGot := Socket_Read(clientsocket, @atemp[0], sizeof(atemp));
        if iGot = 0 then exit;//socket failed/dropped if socket says "true" to WaitForData but datacount is 0, that indicates failure
        sws.SendData(@atemp[0], iGot, true);
      end;
    end;
  finally
    sws.free;
  end;
end;

end.
