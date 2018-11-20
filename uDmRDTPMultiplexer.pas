unit uDmRDTPMultiplexer;

interface

uses
  SysUtils, Classes, better_Sockets, RDTPServerList, RDTPMultiplexerServer, applicationparams, rdtpprocessor,orderlyinit,
  Web.Win.Sockets, simpleabstractprivateserversocket, simplereliableudp, typex, skill;

type
  TdmRDTPMultiServer = class(TDataModule)
    tcp: TTCPServer;
    procedure DataModuleCreate(Sender: TObject);
    procedure tcpAccept(Sender: TObject; ClientSocket: TBetterCustomIPClient);
  private
    FOnIdle: TRDTPIdleEvent;
    procedure udpAccept(endpoint: TReliableUDPEndpoint);

    { Private declarations }
  public
    udp: TMultiplexedUDPServer;
    { Public declarations }
    property OnIdle: TRDTPIdleEvent read FOnIdle write FOnIdle;
  end;

var
  G_RDTP_DEFAULT_MULTIPLEXER_PORT: nativeint;
  dmRDTPMultiServer: TdmRDTPMultiServer;

implementation


{$R *.dfm}

procedure TdmRDTPMultiServer.DataModuleCreate(Sender: TObject);
var
  ap:  TAppParams;
  t: ni;
begin

  ap := NeedAppParams;
  try
    tcp.localPort := ap.GetItemEx('MultiplexerPort', inttostr(G_RDTP_DEFAULT_MULTIPLEXER_PORT));

    tcp.active := true;

    udp := TMultiplexedUDPServer.create(self);
    udp.BindToport(ap.GetItemEx('MultiplexerPort', G_RDTP_DEFAULT_MULTIPLEXER_PORT));
    udp.OnDataAvailable := Self.udpAccept;

    rdtpservers.Lockread;
    try
      for t:= 0 to RDTPServers.Count-1 do begin
        skills.RegisterLocalSkill(RDTPServers.ServiceNames[t], 0, tcp.localPort, 'RDTP/RUDP');
        skills.RegisterLocalSkill(RDTPServers.ServiceNames[t], 0, tcp.localPort, 'RDTP/TCP');
      end;
    finally
      rdtpservers.UnlockRead;
    end;

    //speech.SayNatural('Listening on port '+tcp.localport);
  finally
    NoNeedAppParams(ap);
  end;
end;

procedure TdmRDTPMultiServer.tcpAccept(Sender: TObject;
  ClientSocket: TBetterCustomIPClient);
var
  proc: TRDTPMultiplexerProcessor;
  ac: TSimpleAbstractPrivateServerSocket;
begin

  proc := TRDTPMultiplexerProcessor.create;
  try
    ac := TSimpleAbstractPrivateServerSocket.create;
    ac.socket := clientsocket;
    try
      proc.OnIdle := self.OnIdle;
      proc.Socket := ac;
      proc.ProcessMultiple;
    finally
      ac.free;
    end;
  finally
    proc.free;
  end;

end;

procedure TdmRDTPMultiServer.udpAccept(endpoint: TReliableUDPEndpoint);
var
  proc: TRDTPMultiplexerProcessor;
  ac: TSimpleReliablePrivateServerEndpoint;
begin

  proc := TRDTPMultiplexerProcessor.create;
  try
    ac := TSimpleReliablePrivateServerEndpoint.create;
    try
      while endpoint.connecting do begin
        sleep(100);
      end;
      ac.cli := endpoint;
      while ac.Connected do begin
        proc.OnIdle := self.OnIdle;
        proc.Socket := ac;
        proc.ProcessSingle;
      end;
    finally
      ac.free;
    end;
  finally
    proc.free;
  end;

end;


initialization
  G_RDTP_DEFAULT_MULTIPLEXER_PORT := 420;

end.
