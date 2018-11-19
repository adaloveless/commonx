unit RDTPSocketServer;

interface

uses
  windows, rdtpprocessor, sharedobject, better_sockets, sysutils;

type
  TRDTPSocketServer <T: TRDTPProcessor, constructor> = class (TSharedObject)
  protected
    procedure tcpAccept(Sender: TObject; ClientSocket: TBetterCustomIPClient);
  public
    tcp: TBetterTCPServer;
    pp: T;

    constructor Create(iPort: integer);reintroduce;virtual;
    destructor Destroy;override;
    procedure listen;
    procedure Stoplistening;
  end;

implementation

{ TRDTPSocketServer<T> }

constructor TRDTPSocketServer<T>.Create(iPort: integer);
begin
  inherited Create;
  tcp := TBetterTCPServer.create(nil);
  tcp.LocalPort := inttostr(iPort);
  tcp.OnAccept := tcpAccept;
  tcp.Active := true;

end;

destructor TRDTPSocketServer<T>.Destroy;
begin
  tcp.Active := false;
  tcp.free;
  inherited;
end;

procedure TRDTPSocketServer<T>.listen;
begin
  tcp.active := true;
end;

procedure TRDTPSocketServer<T>.Stoplistening;
begin
  tcp.Active := false;
end;

procedure TRDTPSocketServer<T>.tcpAccept(Sender: TObject;
  ClientSocket: TBetterCustomIPClient);
var
  proc: T;
begin
  raise exception.Create('not implemented... deprecated');
//  proc := T.create;
//  try
//    proc.Socket := clientsocket;
//    proc.Process;
//  finally
//    proc.free;
//  end;
end;

end.
