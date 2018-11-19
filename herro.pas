unit herro;

{$I DelphiDefs.inc}
interface


uses
  betterobject, classes,systemx, betteridsockethandle, idglobal, skill, typex, stringx, better_indy,BetterIdUDPServer, orderlyinit, tickcount, simplequeue, networkx, PeriodicEvents;


const
  HERRO_PAYLOAD_START = 5;
  HERRO_PORT = 3333;
type
  TGreeterPacketHeader = packed record
    hi: array[0..4] of byte;
    procedure Init;
    procedure SetHerro;
    procedure SetWassup;
    procedure SetValid;
    procedure SEtInval;
    function IsHerro: boolean;
    function IsWassup: boolean;
    function IsValid: boolean;
    function IsInval: boolean;
  end;
  PGreeterPacketHeader = ^TGreeterPacketHeader;

  TGreeter = class(TIdUDPServer)
  protected
    procedure DoUDPRead(AThread: TIdUDPListenerThread; const XData: TIdBytes; ABinding: TIdSocketHandle);override;
    procedure ReplyWithServices(ABinding: TIDSocketHandle);
    procedure HandleWassupPacket(const XData: TIdBytes; ABinding: TIdSocketHandle);
  public

    procedure LookForSkills(waittime: ticker = 0);
    procedure CheckValid(sk: TSkillDef);
  end;

  TPeriodicGreeting = class(TPeriodicEvent)
  public
    procedure DoExecute; override;
  end;



var
  greeter: TGreeter;
  periodicGreeting: TPeriodicGreeting;
implementation

uses
  debug;

{ TPeriodicGreeting }

procedure TPeriodicGreeting.DoExecute;
begin
  inherited;
  Frequency := 8000;
  greeter.LookForSkills();
  skills.ScrubSkills;

end;

{ TGreeter }

procedure TGreeter.CheckValid(sk: TSkillDef);
begin
  self.Send(sk.info.host, HERRO_PORT, 'check'+sk.info.ToString, nil);

end;

procedure TGreeter.DoUDPRead(AThread: TIdUDPListenerThread;
  const XData: TIdBytes; ABinding: TIdSocketHandle);
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


procedure oinit;
begin
  greeter := TGreeter.create;
  greeter.ThreadedEvent := true;
  greeter.DefaultPort := HERRO_PORT;
  greeter.ReuseSocket := rsTrue;
  greeter.BroadcastEnabled := true;
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
  greeter.free;
  greeter := nil;
end;

procedure ofinal;
begin
  //
end;

procedure olatefinal;
begin
  //
end;



procedure TGreeter.HandleWassupPacket(const XData: TIdBytes;
  ABinding: TIdSocketHandle);
var
  s: ansistring;
begin

{$IFDEF NEED_FAKE_ANSISTRING}
  s.SetLength(length(XData)-HERRO_PAYLOAD_START);
  movemem32(s.addrof[STRZ], @XData[HERRO_PAYLOAD_START], length(XData)-HERRO_PAYLOAD_START);
{$ELSE}
  setlength(s, length(XData)-HERRO_PAYLOAD_START);
  movemem32(@s[STRZ], @XData[HERRO_PAYLOAD_START], length(XData)-HERRO_PAYLOAD_START);
{$ENDIF}

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
    self.Send(bcast.o[t], HERRO_PORT, 'herro', nil);
  end;
//  Debug.Log(self,CLRD+'Herro?172.16.255.255');
  self.Send('172.16.255.255', HERRO_PORT, 'herro', nil);
//  Debug.Log(self,CLRD+'Herro?192.168.255.255');
  self.Send('192.168.255.255', HERRO_PORT, 'herro', nil);
//  Debug.Log(self,CLRD+'Herro?10.255.255.255');
  self.Send('10.255.255.255', HERRO_PORT, 'herro', nil);
//  Debug.Log(self,CLRD+'Herro?127.0.0.1');
  self.Send('127.0.0.1', HERRO_PORT, 'herro', nil);

end;

procedure TGreeter.ReplyWithServices(ABinding: TIDSocketHandle);
var
  sl: IHolder<TStringlist>;
  t: ni;
  a: TIDBytes;
  s: ansistring;
  h: TGreeterPacketHeader;
begin
  sl := skills.GetSkillList(true);
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

    self.SendBuffer(abinding.PeerIP, abinding.PeerPort, a);
  end;

end;

{ TGreeterPacketHeader }

procedure TGreeterPacketHeader.Init;
begin
  fillmem(@self, sizeof(self), 0);
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

procedure TGreeterPacketHeader.SetWassup;
begin
  hi[0] := ord(char('w'));
  hi[1] := ord(char('a'));
  hi[2] := ord(char('s'));
  hi[3] := ord(char('u'));
  hi[4] := ord(char('p'));
end;

initialization

orderlyinit.init.RegisterProcs('herro', oinit, oprefinal, ofinal, olatefinal, 'skill,indy,PeriodicEvents');

end.
