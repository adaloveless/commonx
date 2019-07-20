  unit herro;

{$I DelphiDefs.inc}
{x$DEFINE MCAST}
{$DEFINE REUSE}
{x$DEFINE SEND_ONLY_TO_MCAST}
interface


uses
  IdUDPClient, IdIPMCastClient, IdIPMCastServer, betterobject, classes,systemx, idsockethandle, idglobal, skill, typex, stringx, better_indy,IdUDPServer, orderlyinit, tickcount, simplequeue, networkx, PeriodicEvents;


const
  HERRO_PAYLOAD_START = 5;
  HERRO_PORT = 1112;
{$IFDEF MCAST}
  MCAST_GROUP = '224.6.6.6';
{$ENDIF}
type
{x$DEFINE MCAST_AS_SENDER}
{$IFDEF MCAST_AS_SENDER}
  TSenderType = TIdIPMCastServer;
{$ELSE}
  {x$DEFINE SRV_AS_SENDER}
  {$IFDEF SRV_AS_SENDER}
    TSenderType = TIDUDPServer;
  {$ELSE}
    TSenderType = TIDUDPServer;
  {$ENDIF}
{$ENDIF}

  TGreeterPacketHeader = packed record
    hi: array[0..4] of byte;
    procedure Init;
    procedure SetHerro;
    procedure SetWassup;
    procedure SetValid;
    procedure Setcheck;
    procedure SEtInval;
    function IsHerro: boolean;
    function IsWassup: boolean;
    function IsValid: boolean;
    function IsCheck: boolean;
    function IsInval: boolean;
  end;
  PGreeterPacketHeader = ^TGreeterPacketHeader;

{$IFDEF MCAST}
  TGreeter = class(TIdIPMCastClient)
{$ELSE}
  TGreeter = class(TIdUdpServer)
  private
{$ENDIF}
  protected
    sender: TSenderType;
    epheremal: TIDUDPServer;
{$IFNDEF MCAST}
    procedure DoUDPRead(AThread: TIdUDPListenerThread; const XData: TIdBytes; ABinding: TIdSocketHandle);override;
{$ELSE}
    procedure Send(host: string; port: ni; data: string; junk: pointer);
    procedure SendBuffer(host: string; port: ni; data: TIdBytes);
    procedure DoIPMCastRead(const XData: TIdBytes; ABinding: TIdSocketHandle);override;
{$ENDIF}
    procedure Announce(host: string; port: ni);
    procedure AnnounceValid(host: string; port: ni; sk: ansistring);
    procedure ReplyWithServices(ABinding: TIDSocketHandle);
    procedure HandleWassupPacket(const XData: TIdBytes; ABinding: TIdSocketHandle);
    procedure HandleCheckPacket(const XData: TIdBytes; ABinding: TIdSocketHandle);
    procedure HandleInValPacket(const XData: TIdBytes; ABinding: TIdSocketHandle);
    procedure HandleValidPacket(const XData: TIdBytes;
      ABinding: TIdSocketHandle);
    function GetSkillFromXData(const XData: TIDBytes): ansistring;
  public

    procedure LookForSkills(waittime: ticker = 0);
    procedure CheckValid(var sk: TSkillInfo);
    constructor Create(AOwner: TComponent);

    procedure ScrubSkills;
    destructor Destroy; override;


  end;

  TPeriodicGreeting = class(TPeriodicEvent)
  public
    procedure DoExecute; override;
  end;



var
  greeter: TGreeter = nil;
  periodicGreeting: TPeriodicGreeting;

procedure RegisterLocalSkill(servicename: string; version: cardinal; endpoint: string = '420';protocol: string = 'RDTP/RUDP');


implementation



uses
  debug;


procedure RegisterLocalSkill(servicename: string; version: cardinal; endpoint: string = '420';protocol: string = 'RDTP/RUDP');
begin
  if greeter = nil then
    raise ECritical.create('Skill registration too early, Greeter not started! check oinit dependencies.');

  skills.RegisterLocalSkill(greeter.sender.binding.Port, servicename, version, endpoint, protocol);


end;

{ TPeriodicGreeting }

procedure TPeriodicGreeting.DoExecute;
begin
  inherited;
  Frequency := 8000;
  greeter.LookForSkills();
  greeter.scrubskills;


end;

{ TGreeter }

procedure TGreeter.Announce(host: string; port: ni);
var
  sl: IHolder<TStringlist>;
  t: ni;
  a: TIDBytes;
  s: ansistring;
  h: TGreeterPacketHeader;
  eport: ni;
begin
  ScrubSkills;
  eport := 0;
  if self.epheremal <> nil then
    eport := self.epheremal.Binding.Port;

  sl := skills.GetSkillList(false);
  for t:= 0 to sl.o.count-1 do begin
    s := ansistring(sl.o[t]);
    setlength(a, HERRO_PAYLOAD_START+length(s));
    h.Init;
    h.SetWassup;
    PGreeterPacketHeader(Pbyte(@a[0]))^ := h;
{$IFDEF NEED_FAKE_ANSISTRING}
    movemem32(@a[HERRO_PAYLOAD_START], s.addrof[STRZ], length(s));
{$ELSE}
    movemem32(@a[HERRO_PAYLOAD_START], @s[STRZ], length(s));
{$ENDIF}
    self.sender.SendBuffer(host, port, a);
  end;
end;

procedure TGreeter.AnnounceValid(host: string; port: ni; sk: ansistring);
var
  a: TIDBytes;
  h: TGreeterPacketHeader;
begin
  ScrubSkills;
  setlength(a, HERRO_PAYLOAD_START+length(sk));
  h.Init;
  h.SetValid;
  PGreeterPacketHeader(Pbyte(@a[0]))^ := h;
{$IFDEF NEED_FAKE_ANSISTRING}
  movemem32(@a[HERRO_PAYLOAD_START], sk.addrof[STRZ], length(sk));
{$ELSE}
  movemem32(@a[HERRO_PAYLOAD_START], @sk[STRZ], length(sk));
{$ENDIF}
  self.sender.SendBuffer(host, port, a);

end;

procedure TGreeter.CheckValid(var sk: TSkillInfo);
var
  s: ansistring;
  h: TGreeterPacketHeader;
  b: TIDBytes;
begin
  h.init;
  h.SetCheck;
  s := sk.ToString;
  setlength(b, HERRO_PAYLOAD_START+length(s));
  movemem32(@b[0], @h, sizeof(h));
{$IFDEF NEED_FAKE_ANSISTRING}
  movemem32(@b[HERRO_PAYLOAD_START], s.addrof[STRZ], length(s));
{$ELSE}
  movemem32(@b[HERRO_PAYLOAD_START], @s[STRZ], length(s));
{$ENDIF}
//  Debug.Log('check'+s);
  try
    self.sender.SendBuffer(sk.host, sk.greeterport, b);
  except
  end;

//  self.Send(sk.host, sk.greeterport, s , nil);
end;

constructor TGreeter.Create(AOwner: TComponent);
begin
  inherited;
{$IFNDEF MCAST}
  self.DefaultPort := HERRO_PORT;
  //sender for sending over random port
  sender := TSenderType.create;
  sender.BroadcastEnabled := true;
  sender.defaultport := 0;
  sender.OnUDPRead := self.DoUDPRead;
  sender.active := true;




{$else}
  {$IFDEF SRV_AS_SENDER}
    sender.DefaultPort := HERRO_PORT;
    sender.BroadcastEnabled := true;
  {$IFDEF REUSE}
    sender.ReuseSocket := rsTrue;
  {$ENDIF}
  {$ELSE}
    sender.Port := HERRO_PORT;
    {$IFDEF MCAST_AS_SENDER}
      sender.MulticastGroup := MCAST_GROUP;
      //sender.Active := true;
    {$else}
      sender.BroadcastEnabled := true;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;

{$IFDEF MCAST}
procedure TGreeter.DoIPMCastRead(const XData: TIdBytes;
  ABinding: TIdSocketHandle);
var
  h: TGreeterPacketHeader;
begin
  inherited;
  if length(xData) < sizeof(h) then
    exit;
  h := PGreeterPacketHeader(Pbyte(@XData[0]))^;
  if h.IsHerro then begin
    ReplyWithServices(ABinding);
  end;
  if h.IsWassup then begin
    HandleWassupPacket(XData, ABinding);
  end;
end;
{$ELSE}
destructor TGreeter.Destroy;
begin
  sender.active := false;
  sender.free;
  sender := nil;

  inherited;
end;

procedure TGreeter.DoUDPRead(AThread: TIdUDPListenerThread;
  const XData: TIdBytes; ABinding: TIdSocketHandle);
var
  h: TGreeterPacketHeader;
var
  s: ansistring;
begin
  inherited;
//  if abinding.port  <> 1112 then
//    Debug.Log('ephemeral');
  if length(xData) < sizeof(h) then
    exit;
  h := PGreeterPacketHeader(Pbyte(@XData[0]))^;
{$IFDEF NEED_FAKE_ANSISTRING}
  s.SetLength(5);
  movemem32(s.addrof[STRZ], @XData[0], 5);
{$ELSE}
  setlength(s, 5);
  movemem32(@s[STRZ], @XData[0], 5);
{$ENDIF}

//  Debug.Log(s);
  if h.IsHerro then begin
    ReplyWithServices(ABinding);
  end else
  if h.IsWassup then begin
    HandleWassupPacket(XData, ABinding);
  end else
  if h.IsValid then begin
    HandleValidPacket(XData, ABinding);
  end else
  if h.IsInval then begin
    HandleInValPacket(XData, ABinding);
  end else
  if h.IsCheck then begin
    HandleCheckPacket(XData, ABinding);
  end;



end;
function TGreeter.GetSkillFromXData(const XData: TIDBytes): ansistring;
begin
{$IFDEF NEED_FAKE_ANSISTRING}
  result.SetLength(length(XData)-HERRO_PAYLOAD_START);
  movemem32(result.addrof[STRZ], @XData[HERRO_PAYLOAD_START], length(XData)-HERRO_PAYLOAD_START);
{$ELSE}
  setlength(result, length(XData)-HERRO_PAYLOAD_START);
  movemem32(@result[STRZ], @XData[HERRO_PAYLOAD_START], length(XData)-HERRO_PAYLOAD_START);
{$ENDIF}
end;

{$ENDIF}


procedure oinit;
begin
  greeter := TGreeter.create(nil);
  greeter.ThreadedEvent := true;
  greeter.DefaultPort := HERRO_PORT;
{$IFDEF REUSE}
  greeter.ReuseSocket := rsTrue;
{$ENDIF}

{$IFDEF MCAST}
  greeter.MulticastGroup := MCAST_GROUP;
{$ELSE}
  greeter.BroadcastEnabled := true;
{$ENDIF}
  greeter.Active := true;

  periodicGreeting := TPeriodicGreeting.Create;
  PEA.Add(periodicGreeting);
  periodicGreeting.Enabled := true;

end;

procedure oprefinal;
begin
  PEA.Remove(periodicGreeting);
  periodicGreeting.free;
  periodicGreeting := nil;
  greeter.Active := false;

end;

procedure ofinal;
begin

  greeter.free;
  greeter := nil;
  //
end;

procedure olatefinal;
begin
  //
end;



procedure TGreeter.HandleCheckPacket(const XData: TIdBytes;
  ABinding: TIdSocketHandle);
var
  sk: ansistring;
begin
  sk := GetSkillFromXData(XData);
  if skills.HasLocal(sk,true) then
    AnnounceValid(abinding.peerip, abinding.PeerPort, sk);

end;


procedure TGreeter.HandleInValPacket(const XData: TIdBytes;
  ABinding: TIdSocketHandle);
var
  s: ansistring;
begin
  s := GetSkillFromXData(XData);
  Skills.Remove(s, true);

end;

procedure TGreeter.HandleValidPacket(const XData: TIdBytes;
  ABinding: TIdSocketHandle);
var
  sk: ansistring;
begin
  sk := GetSkillFromXData(XData);
  skills.MarkValid(sk, abinding.peerip, abinding.peerport, true);


end;

procedure TGreeter.HandleWassupPacket(const XData: TIdBytes;
  ABinding: TIdSocketHandle);
var
  s: ansistring;
begin
  s := GetSkillFromXData(XData);

  skills.RegisterSkill(s, abinding.PeerIP, abinding.peerport);
//  Debug.Log(self,CLRC+'Wassup!'+abinding.peerip+':'+s);


end;

procedure TGreeter.LookForSkills(waittime: ticker);
var
  bcast: IHolder<TStringList>;
  t: ni;
begin
  bcast := GetBroadcastIPs();
  for t:= 0 to bcast.o.count-1 do begin
//    Debug.Log(self,CLRD+'Herro?'+bcast.o[t]);
    self.sender.Send(bcast.o[t], HERRO_PORT, 'herro', nil);
    self.Announce(bcast.o[t], HERRO_PORT);
  end;
//  Debug.Log(self,CLRD+'Herro?172.16.255.255');
//  self.Send('172.16.255.255', HERRO_PORT, 'herro', nil);
//  Debug.Log(self,CLRD+'Herro?192.168.255.255');
  self.sender.Send('192.168.255.255', HERRO_PORT, 'herro', nil);
//  Debug.Log(self,CLRD+'Herro?10.255.255.255');
//  self.Send('10.255.255.255', HERRO_PORT, 'herro', nil);
//  Debug.Log(self,CLRD+'Herro?127.0.0.1');
  self.sender.Send('127.0.0.1', HERRO_PORT, 'herro', nil);

end;

procedure TGreeter.ReplyWithServices(ABinding: TIDSocketHandle);
begin
  Announce(ABinding.PeerIP, abinding.peerport);
end;

procedure TGreeter.ScrubSkills;
var
  sl: TSkillArray;
  t: ni;
begin
//  skills.Lock;
  try

    sl := skills.GetSkillListToCheck;
    for t := 0 to high(sl) do begin
      CheckValid(sl[t]);
    end;
    skills.ScrubSkills;

  finally
//    skills.Unlock;
  end;

end;

{$IFDEF MCAST}
procedure TGreeter.Send(host: string; port: ni; data: string; junk: pointer);
begin
{$IFDEF SEND_ONLY_TO_MCAST}
  host := MCAST_GROUP;
{$ENDIF}
{$IFDEF MCAST_AS_SENDER}
  sender.Send(data);
{$ELSE}
  sender.Send(host, port, data);
{$ENDIF}
end;
{$ENDIF}

{$IFDEF MCAST}
procedure TGreeter.SendBuffer(host: string; port: ni; data: TIdBytes);
begin
{$IFDEF SEND_ONLY_TO_MCAST}
  host := MCAST_GROUP;
{$ENDIF}
{$IFDEF MCAST_AS_SENDER}
  sender.Send(data);
{$ELSE}
  sender.SendBuffer(host, port, data);
{$ENDIF}
end;
{$ENDIF}

{ TGreeterPacketHeader }

procedure TGreeterPacketHeader.Init;
begin
  fillmem(@self, sizeof(self), 0);
end;

function TGreeterPacketHeader.IsCheck: boolean;
begin
  result := true;
  if hi[0] <> ord(char('c')) then exit(false);
  if hi[1] <> ord(char('h')) then exit(false);
  if hi[2] <> ord(char('e')) then exit(false);
  if hi[3] <> ord(char('c')) then exit(false);
  if hi[4] <> ord(char('k')) then exit(false);
end;

function TGreeterPacketHeader.IsHerro: boolean;
begin
  result := true;
  if hi[0] <> ord(char('h')) then exit(false);
  if hi[1] <> ord(char('e')) then exit(false);
  if hi[2] <> ord(char('r')) then exit(false);
  if hi[3] <> ord(char('r')) then exit(false);
  if hi[4] <> ord(char('o')) then exit(false);


end;

function TGreeterPacketHeader.IsInval: boolean;
begin
  result := true;
  if hi[0] <> ord(char('i')) then exit(false);
  if hi[1] <> ord(char('n')) then exit(false);
  if hi[2] <> ord(char('v')) then exit(false);
  if hi[3] <> ord(char('a')) then exit(false);
  if hi[4] <> ord(char('l')) then exit(false);

end;

function TGreeterPacketHeader.IsValid: boolean;
begin
  result := true;
  if hi[0] <> ord(char('v')) then exit(false);
  if hi[1] <> ord(char('a')) then exit(false);
  if hi[2] <> ord(char('l')) then exit(false);
  if hi[3] <> ord(char('i')) then exit(false);
  if hi[4] <> ord(char('d')) then exit(false);

end;

function TGreeterPacketHeader.IsWassup: boolean;
begin
  result := true;
  if hi[0] <> ord(char('w')) then exit(false);
  if hi[1] <> ord(char('a')) then exit(false);
  if hi[2] <> ord(char('s')) then exit(false);
  if hi[3] <> ord(char('u')) then exit(false);
  if hi[4] <> ord(char('p')) then exit(false);

end;

procedure TGreeterPacketHeader.SetHerro;
begin
  hi[0] := ord(char('h'));
  hi[1] := ord(char('e'));
  hi[2] := ord(char('r'));
  hi[3] := ord(char('r'));
  hi[4] := ord(char('o'));
end;

procedure TGreeterPacketHeader.SEtInval;
begin
  hi[0] := ord(char('i'));
  hi[1] := ord(char('n'));
  hi[2] := ord(char('v'));
  hi[3] := ord(char('a'));
  hi[4] := ord(char('l'));
end;

procedure TGreeterPacketHeader.SetValid;
begin
  hi[0] := ord(char('v'));
  hi[1] := ord(char('a'));
  hi[2] := ord(char('l'));
  hi[3] := ord(char('i'));
  hi[4] := ord(char('d'));
end;

procedure TGreeterPacketHeader.SetCheck;
begin
  hi[0] := ord(char('c'));
  hi[1] := ord(char('h'));
  hi[2] := ord(char('e'));
  hi[3] := ord(char('c'));
  hi[4] := ord(char('k'));
end;

procedure TGreeterPacketHeader.SetWassup;
begin
  hi[0] := ord(char('w'));
  hi[1] := ord(char('a'));
  hi[2] := ord(char('s'));
  hi[3] := ord(char('u'));
  hi[4] := ord(char('p'));
end;

initialization

orderlyinit.init.RegisterProcs('herro', oinit, oprefinal, ofinal, olatefinal, 'skill,better_indy,PeriodicEvents');

end.
