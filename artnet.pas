unit artnet;

interface

uses
  betterobject, sharedobject, typex, systemx, classes, idglobal, betteridudpserver, betteridsockethandle, debug, sysutils, orderlyinit, endian, numbers, better_collections, stringx, consolelock;

type
  TArtCC = packed record
    head: array[0..7] of ansichar;
    procedure Init;
  end;

  TArtPacket = packed record
  public
    head : TArtCC;
  private
    F_opcode: smallint;
  public
  private
    function getopcode: smallint;
    procedure setopcode(Value: smallint);
  public
    procedure Init;
    property opcode: smallint read getopcode write setopcode;
    procedure FromPointer(p: Pbyte; sz: ni);
  end;

  TArtPollPacket = packed record
    head: TArtPacket;
  private
    Fprotocolversion: smallint;
    function GetProtocolVersion: smallint;
    procedure SetProtocolVersion(Value: smallint);
  public
    procedure Init;
    property ProtocolVersion: smallint read GetProtocolVersion write SetProtocolVersion;
  end;

  TArtPollReplyPacket = packed record
    head: TArtPacket;
    ip: array[0..3] of byte;
    port: word;
    versioninfo: word;
    netswitch: byte;
    subswitch: byte;
    oeminfo: word;
    ubea_version: byte;
    status: byte;
    estacode: word;
    shortname: array[0..17] of ansichar;
    longname: array[0..63] of ansichar;
    portinfo: array[0..21] of ansichar;
    status2: byte;
  private
  public
    procedure FromPointer(p: pbyte; sz: ni);
  end;

  TArtDMXPacket = packed record
    head: TArtPacket;
  private
    Fprotocolversion: smallint;
    function GetProtocolVersion: smallint;
    procedure SetProtocolVersion(const Value: smallint);
  public

    sequence: byte;
    physical: byte;
    universe: smallint;
  private
    Flength: smallint;
    function GetLength: smallint;
    procedure SetLength(const Value: smallint);
  public

    data:array[0..511] of byte;
    procedure Init;
    property ProtocolVersion: smallint read GetProtocolVersion write SetProtocolVersion;
    property Length: smallint read GetLength write SetLength;
  end;

  TArtNetEndpoint = class(TBetterObject)
  private
    function GetIP: string;
    procedure SetIP(value: string);
  public
    reply: TArtPollReplyPacket;
    property IP: string read GetIP write SetIP;

  end;

  TArtNetUDPHandler = class(TSharedObject)
  private
  protected
    FKnownFixtures: TBetterList<TArtNetEndpoint>;
    udpc: TIdUDPServer;
    procedure UDPDispatch(athread:TIdUDPListenerThread; const adata:tidbytes; abinding:tidsockethandle);
    procedure Dispatch_ArtPollReply(var head: TArtPAcket; athread:TIdUDPListenerThread; const adata:tidbytes; abinding:tidsockethandle);
    procedure UDPOnUDPRead(athread:TIdUDPListenerThread; const adata:tidbytes; abinding:tidsockethandle);
  public
    constructor Create;override;
    procedure Detach; override;
    procedure ClearEndpoints;

    procedure Poll;
    function FindByReply(var reply: TArtPollReplyPacket): TArtNetEndPoint;
    function FindByIP(const sIp: string): TArtNetEndpoint;
    procedure MapDMXData(ep: TArtNetEndPoint; p: pbyte; sz: ni);
    procedure AddKnownFixture(ep: TArtNetEndpoint);
  end;

function ToIDBytes(p: pbyte; sz: ni): TIdBytes;

function art: TArtNetUDPHandler;


implementation

var
  Fart: TArtnetUDPHandler;



function art: TArtNetUDPHandler;
begin
  lockconsole;
  try
    if FArt = nil then
      FArt := TArtNetUDPHandler.create;
    result := Fart;
  finally
    unlockconsole;
  end;
end;
{ TArtNetUDPHandler }



procedure TArtNetUDPHandler.AddKnownFixture(ep: TArtNetEndpoint);
begin
  lock;
  try
    FKnownFixtures.add(ep);
  finally
    unlock;
  end;
end;

procedure TArtNetUDPHandler.ClearEndpoints;
var
  ep:  TArtNetEndpoint;
begin
  while FKnownFixtures.count > 0 do begin
    ep := FKnownFixtures[0];
    FKnownFixtures.delete(0);
    ep.free;
    ep := nil;
  end;
end;

constructor TArtNetUDPHandler.Create;
var
  hand: TIdSocketHandle;
begin
  inherited;
  FKnownFixtures := TBetterList<TArtNetEndpoint>.create;
  udpc := TIdUDPServer.Create(nil);
  udpc.BroadcastEnabled := true;
  udpc.ThreadedEvent := true;
  udpc.DefaultPort := 6454;
//  hand := udpc.Bindings.add;
//  hand.IP := '2.0.0.254';
//  hand.Port := 6454;
//  hand.BroadcastEnabled := true;

  udpc.OnUDPRead:= self.UDPOnUDPRead;
  udpc.Active := true;
  Debug.Log('Hello');

  Poll;



end;

procedure TArtNetUDPHandler.Detach;
begin
  if detached then
    exit;

  udpc.active := false;
  ClearEndpoints;
  FKnownFixtures.free;
  FKnownFixtures := nil;

  inherited;

end;

procedure TArtNetUDPHandler.Dispatch_ArtPollReply(var head: TArtPAcket;
  athread: TIdUDPListenerThread; const adata: tidbytes;
  abinding: tidsockethandle);
var
  reply: TArtPollReplyPacket;
  ep: TArtNetEndpoint;
begin
  Debug.Log('Got ArtPollReply');

  reply.FromPointer(@adata[0], length(adata));

  Debug.Log('ShortName: '+reply.shortname);
  Debug.Log('LongName: '+reply.longname);


  ep := FindByReply(reply);
  if ep = nil then begin
    ep := TArtNetEndpoint.create;
    ep.reply := reply;
    FKnownFixtures.add(ep);
    Debug.Log('There are now '+FKnownFixtures.count.tostring+' known endpoints');
  end else begin
    ep.reply := reply;
  end;




end;

function TArtNetUDPHandler.FindByIP(const sIp: string): TArtNetEndpoint;
var
  t: ni;
begin
  result := nil;
  for t:= 0 to FKnownFixtures.Count-1 do begin
    if sIP = FKNownFixtures[t].IP then
      exit(FknownFixtures[t]);
  end;
end;

function TArtNetUDPHandler.FindByReply(var reply: TArtPollReplyPacket): TArtNetEndpoint;
var
  t: ni;
begin
  result := nil;
  for t:= 0 to FKnownFixtures.Count-1 do begin
    if CompareMem(@Fknownfixtures[t].reply.ip[0], @reply.ip[0], 4)then
      exit(FknownFixtures[t]);
  end;

end;

procedure TArtNetUDPHandler.MapDMXData(ep: TArtNetEndPoint; p: pbyte; sz: ni);
var
  dmxp: TArtDMXPacket;
  idb: TIDBytes;
begin
  dmxp.Init;
  movemem32(@dmxp.Data[0], p, sz);
  idb := ToIDBytes(@dmxp, sizeof(dmxp));
  self.udpc.SendBuffer(ep.IP, 6454, idb);

end;

procedure TArtNetUDPHandler.Poll;
var
  p: TArtPollPacket;
  idb: TIDBytes;
begin
  p.Init;
  idb := ToIDBytes(pbyte(@p), sizeof(p));
  Self.udpc.SendBuffer('255.255.255.255', 6454, idb);

end;

procedure TArtNetUDPHandler.UDPDispatch(athread: TIdUDPListenerThread;
  const adata: tidbytes; abinding: tidsockethandle);
var
  head: TArtPacket;
begin
  head.FromPointer(@adata[0], length(adata));

  case head.opcode of
    $2100: begin
      Dispatch_ArtPollReply(head, athread, adata, abinding);
    end;

  end;


end;

procedure TArtNetUDPHandler.UDPOnUDPRead(athread:TIdUDPListenerThread; const adata:tidbytes; abinding:tidsockethandle);
begin
  if udpc.Active = false then
    exit;
  debug.Log('Got '+length(adata).tostring+' bytes from '+abinding.peerip+':'+abinding.PeerPort.tostring);
  UDPDispatch(athread, adata, abinding);
end;

{ TArtHeader }

procedure TArtCC.Init;
begin
  head[0] := 'A';
  head[1] := 'r';
  head[2] := 't';
  head[3] := '-';
  head[4] := 'N';
  head[5] := 'e';
  head[6] := 't';
  head[7] := #0;
end;


procedure TArtPacket.FromPointer(p: Pbyte; sz: ni);
begin
  movemem32(@self, p, lesserof(sz, sizeof(self)));
end;

function TArtPacket.getopcode: smallint;
begin
  result := F_opcode;
//  EndianSwap(@result, sizeof(result));
end;




procedure TArtPacket.Init;
begin
  head.Init;
end;

procedure TArtPacket.setopcode(Value: smallint);
begin
//  EndianSwap(@value, sizeof(value));
  F_opcode := value;
end;



{ TArtPollPacket }

function TArtPollPacket.GetProtocolVersion: smallint;
begin
  result := FProtocolVersion;
  endianswap(@result, sizeof(result));
end;

procedure TArtPollPacket.Init;
begin
  head.Init;
  head.opcode := $2000;
  protocolversion := 14;
end;

function ToIDBytes(p: pbyte; sz: ni): TIdBytes;
begin
  setlength(result, sz);
  movemem32(@result[0], p, sz);
end;

procedure TArtPollPacket.SetProtocolVersion(Value: smallint);
begin

  endianswap(@Value, sizeof(Value));
  FProtocolVersion := value;


end;

{ TArtDMXPacket }

function TArtDMXPacket.GetLength: smallint;
begin
  result := FLength;
  endianswap(@result, sizeof(result));

end;

function TArtDMXPacket.GetProtocolVersion: smallint;
begin
  result := FProtocolVersion;
  endianswap(@result, sizeof(result));

end;

procedure TArtDMXPacket.Init;
begin
  head.init;
  head.opcode := $5000;
  protocolversion := 14;
  sequence := 0;
  physical := 0;
  universe := 0;
  length := 512;

end;

procedure TArtDMXPacket.SetLength(const Value: smallint);
begin
  endianswap(@Value, sizeof(Value));
  FLength := value;
end;

procedure TArtDMXPacket.SetProtocolVersion(const Value: smallint);
begin
  endianswap(@Value, sizeof(Value));
  FProtocolVersion := value;

end;

{ TArtPollReplyPacket }

procedure TArtPollReplyPacket.FromPointer(p: pbyte; sz: ni);
begin
  head.FromPointer(p, sz);
  movemem32(@ip[0], pbyte(p)+sizeof(head), lesserof(sz, sizeof(self))-sizeof(head))
end;

{ TArtNetEndpoint }

function TArtNetEndpoint.GetIP: string;
begin
  result := reply.ip[0].ToString+'.'+
            reply.ip[1].ToString+'.'+
            reply.ip[2].ToString+'.'+
            reply.ip[3].ToString;

end;


procedure oinit;
begin
  Fart := nil;

end;

procedure ofinal;
begin
  Fart.free;
  Fart := nil;
end;


procedure TArtNetEndpoint.SetIP(value: string);
var
  s,s1,s2: string;
begin
  s2 := value;
  SplitString(s2, '.', s1,s2);
  reply.ip[0] := strtoint(s1);
  SplitString(s2, '.', s1,s2);
  reply.ip[1] := strtoint(s1);
  SplitString(s2, '.', s1,s2);
  reply.ip[2] := strtoint(s1);
  reply.ip[3] := strtoint(s2);

end;

initialization
  orderlyinit.init.RegisterProcs('artnet', oinit, ofinal, 'managedthread,consolelock');


end.
