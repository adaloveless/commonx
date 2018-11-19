unit RDTPServerDM;

interface

{$DEFINE BETTERTCP}
uses
  SysUtils, Classes, better_sockets, rdtpprocessor, SimpleReliableUDP, simpleabstractprivateserversocket;

type
  TRDTPServer = class(TDataModule)
    procedure tcpAccept(Sender: TObject; ClientSocket: TBetterCustomIpClient);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FConn: integer;
    FProcessorClass: TRDTPProcessorClass;


    procedure udpAccept(endpoint: TReliableUDPEndpoint);
    function Getconnections: integer;
    { Private declarations }

  public
    { Public declarations }
    tcp: TBetterTCPServer;
    udp: TMultiplexedUDPServer;
    Fdebug: TStringList;
    constructor create(iPort: integer; processor: TRDTPProcessorClass);reintroduce;virtual;
    property Connections: integer read Getconnections;
    procedure IncConn;
    procedure DecConn;

    function DrainDebug: ansistring;
    procedure LocalDebug(s: ansistring);



  end;

var
  RDTPServer: TRDTPServer;


implementation

uses AppLock, debug;

{$R *.dfm}

procedure TRDTPServer.tcpAccept(Sender: TObject;
  ClientSocket: TBetterCustomIpClient);
var
  proc: TRDTPProcessor;
  ac: TSimpleAbstractPrivateServerSocket;
begin
  IncConn;
  try
    proc := FProcessorClass.create;
    try
      ac := TSimpleAbstractPrivateServerSocket.create;
      try
        ac.socket := ClientSocket;
        proc.Socket := ac;
        proc.ProcessMultiple;
      finally
        ac.free;
      end;
    finally
      proc.Free;
    end;
  finally
    DecConn;
  end;

end;


procedure TRDTPServer.udpAccept(endpoint: TReliableUDPEndpoint);
var
  proc: TRDTPProcessor;
  ac: TSimpleReliablePrivateServerEndpoint;
begin
  IncConn;
  try
    proc := FProcessorClass.create;
    try
      ac := TsimpleReliablePrivateServerEndpoint.create;
      try

        ac.cli := endpoint;

        proc.Socket := ac;
        proc.ProcessMultiple;
      finally
        ac.free;
      end;
    finally
      proc.Free;
    end;
  finally
    DecConn;
  end;
end;

constructor TRDTPServer.create(iPort: integer; processor: TRDTPProcessorClass);
begin
  FProcessorClass := processor;
  inherited create(nil);


{$IFDEF BETTERTCP}
  tcp := TBetterTCPServer.create(self);
  tcp.localport := inttostr(iPort);
  tcp.OnAccept := self.tcpAccept;

{$ELSE}
  tcp := TTCPServer.create(self);
  tcp.localport := inttostr(iPort);
  tcp.OnAccept := self.tcpAccept;
{$ENDIF}
  udp := TMultiplexedUDPServer.CReate(self);
  udp.OnDataAvailable := udpAccept;
  udp.BindToport(iPort);



  tcp.active := true;
  FDebug := tStringList.create;

end;

procedure TRDTPServer.DecConn;
begin
  al.lock;
  try
    dec(Fconn);
    LocalDebug('connection closed');
  finally
    al.unlock;
  end;
end;

function TRDTPServer.Getconnections: integer;
begin
  al.lock;
  try
    result := FConn;
  finally
    al.unlock;
  end;

end;

procedure TRDTPServer.IncConn;
begin
  al.lock;
  try
    inc(FConn);
    LocalDebug('connection opened');
  finally
    al.unlock;
  end;

end;

procedure TRDTPServer.DataModuleDestroy(Sender: TObject);
begin
  FDebug.free;


end;

function TRDTPServer.DrainDebug: ansistring;
begin
  al.lock;
  try
    result := FDebug.text;
    FDebug.Clear;
  finally
    al.unlock;
  end;
end;

procedure TRDTPServer.LocalDebug(s: ansistring);
begin
  al.lock;
  try
    fDebug.add(s);
  finally
    al.unlock;
  end;
end;





end.
