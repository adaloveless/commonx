unit DebugPinger;

interface

uses
  idglobal, idudpclient, betterobject, classes, systemx;

type
  TDebugPinger = class(TBetterObject)
  public
    udpc: TIDUDPCLient;
    constructor Create;override;
    destructor Destroy;override;
    procedure Ping(p: pbyte; iSize: nativeint);
  end;

implementation

{ TDebugPinger }

constructor TDebugPinger.Create;
begin
  inherited;
  udpc := TIdUDPClient.Create;
  udpc.BroadcastEnabled := true;
end;

destructor TDebugPinger.Destroy;
begin
  udpc.Free;
  inherited;
end;

procedure TDebugPinger.Ping(p: pbyte; iSize: nativeint);
var
  idb: TIdBytes;
begin
  setlength(idb, iSize);
  movemem32(@idb[0], p, isize);
  udpc.Port := 666;
  udpc.Active := true;
  udpc.Host := '255.255.255.255';
  udpc.SendBuffer(idb);


end;

end.
