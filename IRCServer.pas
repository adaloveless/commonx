unit IRCServer;

interface

uses
  typex, systemx, stringx, idtcpserver,idglobal, betterobject;


type
  TIRCServer = class (TSharedObject)
  private
    FPort: int64;
  protected
    idsrv: TIdTCPServer;

    procedure StartListening;
    procedure StopListening;
  public
    property Port: int64 read FPort write FPort;
  end;

implementation

{ TIRCServer }

procedure TIRCServer.StartListening;
begin
  if idsrv = nil then begin
    idsrv := TIdTCPServer.create(nil);
    idsrv.DefaultPort := port;
    idsrv.MaxConnections := 10000;
    idsrv.OnConnect :=
  end;

end;

procedure TIRCServer.StopListening;
begin
  idsrv.free;
  idsrv := nil;

end;

end.
