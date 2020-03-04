unit IRCServer;

interface

uses
  classes, typex, systemx, stringx, idtcpserver,idglobal, betterobject, idcontext, better_collections, debug, sysutils;

type
  TIRCFlags = record
    namesx: boolean;
    uhnames: boolean;
  end;

  TIRCChannel = class;//forward
  TIRCServer = class;//forward)

  TIRCConnection = class(TSharedObject)
  public
    nick: string;
    user: string;
    user_num: string;
    user_wild: string;
    context: TIdContext;
    gotcaps: boolean;
    flags: TIRCFlags;

    procedure send_generic_msg(id: string; sTrailer: string);overload;
    procedure send_generic_msg(id: string; params: TArray<string>; sTrailer: string);overload;

    procedure channel_msg(from: TIRCConnection; chan: TIRCChannel; sTrailer: string);overload;
    procedure msg_chan_to_specific_user_raw(chan: TIRCChannel; sRaw: string; sPreChan: string = '');
    procedure privmsg(sTarget: string; sMessage: string; sFrom: string = '');
    procedure cmdmsg(sCmd: string; sFrom: string = '');
    procedure ircmsg(sCmd: string; sParams: string = '');
    function FullIdent: string;
  end;

  TIRCChannel = class(TSharedObject)
  protected
    procedure NotifyJoined(con: TIRCConnection);
    procedure NotifyLeft(con: TIRCConnection);
  public
    [weak] irc: TIRCSErver;
    name: string;
    users: TSharedList<TIRCConnection>;
    constructor Create; override;
    procedure Detach; override;
    procedure join(con: TIRCConnection);
    procedure leave(con: TIRCConnection);
    procedure msg_everyone_in_channel(confrom: TIRCConnection; sMsg: string; bExcludeSelf: boolean = true);
    procedure privmsg_everyone_in_channel(confrom: TIRCConnection; sMsg: string);

    procedure EchoNames(toCon: TIRCConnection);
    procedure EchoWhoHere(toCon: TIRCConnection);


  end;

  TIRCServer = class (TSharedObject)
  private
    FCons: TSharedList<TIRCConnection>;
    FChannels: TSharedList<TIRCChannel>;
    FPort: int64;
  protected
    idsrv: TIdTCPServer;
    function FindChannel(sName: string): TIRCChannel;
    function HasChannel(sName: string) : boolean;
    function AddChannel(sName: string): boolean;

    procedure StartListening;
    procedure StopListening;
    procedure SRVOnConnect(AContext: TIdContext);
    procedure SRVOnDisconnect(AContext: TIdContext);
    procedure SRVOnExecute(AContext: TIdContext);

    procedure handle_line(con: TIRCConnection; sLine: string);
    procedure handle_command(con: TIRCConnection; sLine: string; sl: TStringList);
    procedure handle_command_join(con: TIRCConnection; sLine: string; sl: TStringList);
    procedure handle_command_chanL(con: TIRCConnection; sLine: string; sl: TStringList);
    procedure handle_command_whohere(con: TIRCConnection; sLine: string; sl: TStringList);
    procedure handle_command_part(con: TIRCConnection; sLine: string; sl: TStringList);
    procedure handle_command_names(con: TIRCConnection; sLine: string; sl: TStringList);
    procedure handle_command_privmsg(con: TIRCConnection; sLine: string; sl: TStringList);
    procedure handle_welcome(con: TIRCConnection);
    procedure RemoveFromAllChannels(con: TIRCConnection);
    procedure SendChannels();overload;
    procedure SendChannels(con: TIRCconnection);overload;
    procedure ChannelEmpty(chan: TIRCChannel);


  public
    constructor Create; override;
    procedure Detach; override;
    property Port: int64 read FPort write FPort;
  end;

implementation

{ TIRCServer }

function TIRCServer.AddChannel(sName: string): boolean;
begin
  var lck := FChannels.locki;

  if HasChannel(sName) then
    exit(false);

  var chan := TIRCChannel.create;
  chan.name := sName;
  chan.irc := self;
  FChannels.Add(chan);
  exit(true);

end;

procedure TIRCServer.ChannelEmpty(chan: TIRCChannel);
begin
//  if FChannels.trylock then
  FChannels.lock;
  try
    FChannels.Remove(chan);
    chan.free;
    chan := nil;
  finally
    fChannels.unlock;
  end;
  SendChannels;
end;

constructor TIRCServer.Create;
begin
  inherited;
  fCons := TSharedList<TIRCConnection>.create;
  fChannels := TSharedList<TIRCChannel>.create;
  port := 223;
  StartListening;

end;

procedure TIRCServer.Detach;
begin
  if detached then exit;
  StopListening;
  FCons.free;
  FChannels.free;
  inherited;

end;

function TIRCServer.FindChannel(sName: string): TIRCChannel;
begin

  var lck := FChannels.locki;

  for var t := 0 to FChannels.count-1 do begin
    if (comparetext(FChannels[t].name, sName)=0) then
      exit(FChannels[t]);
  end;

  result := nil;
end;

procedure TIRCServer.handle_command(con: TIRCConnection; sLine: string;
  sl: TStringList);
begin
  if sl.count = 0 then exit;
  var cmd := uppercase(sl[0]);
  if cmd = 'CAP' then begin
    if uppercase(sl[1]) = 'END' then begin
      con.gotcaps := true;
    end;
  end else
  if cmd = 'NICK' then begin
    if sl.count < 2 then
      exit;
    con.nick := sl[1];
  end else
  if cmd = 'USER' then begin
    con.user := sl[1];
    con.user_num := sl[2];
    con.user_wild := sl[3];
    con.context.connection.Socket.writeln('compa.net CAP * LS unrealircd.org/plaintext-policy unrealircd.org/link-security extended-join chghost cap-notify userhost-in-names multi-prefix away-notify account-notify sasl tls');
    handle_welcome(con);
  end else
  if cmd = 'PING' then begin
    if sl.count > 1 then
      con.send_generic_msg('PONG', [sl[1]],'pong')
    else
      con.send_generic_msg('PONG', 'pong');
  end else
  if cmd = 'NAMESX' then begin
    con.flags.namesx := true;
  end else
  if cmd = 'UHNAMES' then begin
    con.flags.uhnames := true;
  end else
  if cmd = 'JOIN' then begin
    handle_command_join(con, sLIne, sl);
  end else
  if cmd = 'PART' then begin
    handle_command_part(con, sLIne, sl);
  end else
  if cmd = 'NAMES' then begin
    handle_command_names(con, sLIne, sl);
  end else
  if cmd = 'MODE' then begin
//    handle_command_join(con, sLIne, sl);
  end else
  if cmd = 'CHANL' then begin
    handle_command_chanL(con, sLIne, sl);
  end else
  if cmd = 'WHOHERE' then begin
    handle_command_whohere(con, sLIne, sl);
  end else
  if cmd = 'PRIVMSG' then begin
    handle_command_privmsg(con, sLIne, sl);
  end else begin
    Debug.Log('WARNING!!!!! *** unknown command '+cmd+' line:'+sLine);
  end;



end;

procedure TIRCServer.handle_command_chanL(con: TIRCConnection; sLine: string;
  sl: TStringList);
begin
  SendChannels(con);


end;

procedure TIRCServer.handle_command_join(con: TIRCConnection; sLine: string;
  sl: TStringList);
begin
  if sl.count < 2 then
    exit;

  var ilock := FChannels.LockI;




  var slh := parsestringh(sl[1], ',');
  for var t:= 0 to slh.o.count-1 do begin
    if AddChannel(slh.o[t]) then
      sendChannels();
    var chan := FindChannel(slh.o[t]);
    if chan <> nil then
      chan.join(con);
  end;



end;


procedure TIRCServer.handle_command_names(con: TIRCConnection; sLine: string;
  sl: TStringList);
begin
  var ilock := FChannels.LockI;
  for var t := 0 to FChannels.count-1 do begin
    Fchannels[t].echonames(con);
  end;


end;

procedure TIRCServer.handle_command_part(con: TIRCConnection; sLine: string;
  sl: TStringList);
begin
  var ilock := FChannels.LockI;

  var slh := parsestringh(sl[1], ',');
  for var t:= 0 to slh.o.count-1 do begin
    var chan := FindChannel(slh.o[t]);
    if chan <> nil then
      chan.leave(con);
  end;

end;

procedure TIRCServer.handle_command_privmsg(con: TIRCConnection; sLine: string;
  sl: TStringList);
begin
  Fchannels.lock;
  try
    if sl.count > 1 then
    for var t := 0 to FChannels.count-1 do begin
      if sl.count < 2 then exit;

      var chan := FChannels[t];
      if comparetext(chan.name, sl[1])=0 then begin
        sl.delete(0);
        sl.delete(0);
        var msg := unparsestring(' ',sl);
        chan.msg_everyone_in_channel(con, msg);
      end;
    end;
  finally
    FChannels.unlock;
  end;
end;

procedure TIRCServer.handle_command_whohere(con: TIRCConnection; sLine: string;
  sl: TStringList);
begin
  var ilock := FChannels.LockI;

  if sl.count < 2 then
    exit;


  var slh := parsestringh(sl[1], ',');
  for var t:= 0 to slh.o.count-1 do begin
    var chan := FindChannel(slh.o[t]);
    if chan <> nil then begin
      chan.EchoWhoHere(con);
//      chan.echonames(con);
//      chan.leave(con);
    end;
  end;
end;

procedure TIRCServer.handle_line(con: TIRCConnection; sLine: string);
begin
  Debug.log(sLine);
  var slh := ParseStringh(sLine, ' ');
  handle_command(con, sLine, slh.o);
end;

procedure TIRCServer.handle_welcome(con: TIRCConnection);
begin
  con.send_generic_msg('001', 'Welcome to CompaNet... ');
  con.send_generic_msg('002', [con.nick], 'is your nickname');//your nick is now whatever
  con.send_generic_msg('003', 'Pointless message about server creation time');//this server was create ... date... whatever
//  con.send_generic_msg('005', ['AWAYLEN=307 CASEMAPPING=ascii CHANLIMIT=#:50 CHANMODES=beIqa,kLf,l,psmntirzMQNRTOVKDdGPZSCc CHANNELLEN=32 CHANTYPES=# DEAF=d ELIST=MNUCT EXCEPTS EXTBAN=~,tmTSOcaRrnqj HCN INVEX'], 'are supported by this server');
//  con.send_generic_msg('005', ['KICKLEN=30/join 7 KNOCK MAP MAXCHANNELS=5 MAXLIST=b:100,e:100,I:100 MAXNICKLEN=256 MODES=12 NAMESX NETWORK=CompaNet NICKLEN=256 PREFIX=(ohv)@%+) QUITLEN=307'], 'are supported by this server');
//  con.send_generic_msg('005', ['SAFELIST SILENCE=10 STATUSMSG=@%+ TARGMAX=DCALLOW:,ISON:,JOIN:,KICK:,KILL:,LIST:,NAMES:1,'+'NOTICE:1,PART:,PRIVMSG:4,SAJOIN:SAPART:,USERHOST:,USERIP:,WATCH:,WHOIS:1,WHOWAS:1 TOPICLEN=360 UHNAMES USERIP WALLCHOPS WATCH=128 WATCHOPTS=A'],'are supported by this server');
  con.send_generic_msg('396', [con.nick],'is nao your displayed host');
  con.send_generic_msg('251', [FCons.count.tostring], 'There are '+Fcons.count.tostring+' users/computers connected');
  con.send_generic_msg('252', ['0'],'operators, unknown');
  con.send_generic_msg('253', ['0'],'unknown connections... unknown');
  con.send_generic_msg('254', [Fchannels.count.tostring],'channels');
  con.send_generic_msg('255', [Fcons.count.tostring], 'clients');
  con.send_generic_msg('265', [Fcons.count.tostring, '999'], 'local users/max');
  con.send_generic_msg('266', [Fcons.count.tostring, '999'], 'global users/max');
  con.send_generic_msg('422', 'MOTD');
  con.send_generic_msg('MODE', '+ix');
  con.send_generic_msg('NOTICE', 'Notice: Jason is a genius');
  SendChannels(con);




end;

function TIRCServer.HasChannel(sName: string): boolean;
begin
  result := FindChannel(sName) <> nil;
end;

procedure TIRCServer.RemoveFromAllChannels(con: TIRCConnection);
begin
  var lck := FChannels.LockI;
  for var t := FChannels.count-1 downto 0 do begin
    FChannels[t].leave(con);
  end;
end;

procedure TIRCServer.SendChannels(con: TIRCconnection);
begin
  var cl := con.Locki;
  var chans: string := '';
  begin
    var l := FChannels.LockI;//LOCKS until the "end" of this scope
    for var t:= 0 to FChannels.count-1 do begin
      if t < (FChannels.count-1) then
        chans := chans + FChannels[t].name+' '
      else
        chans := chans + FChannels[t].name;
    end;
  end;

  if con <> nil then
    con.ircmsg('CHANL '+trim(chans));
end;

procedure TIRCServer.SendChannels;
begin
  var l := FCons.locki;
  for var t := 0 to FCons.count-1 do begin
    var cl := FCons[t].locki;
    SendChannels(FCons[t]);
  end;

end;

procedure TIRCServer.SRVOnConnect(AContext: TIdContext);
begin
  Debug.Log('Hark! A connnexion!');
end;

procedure TIRCServer.SRVOnDisconnect(AContext: TIdContext);
begin
  Debug.Log('Buhbye!');

end;

procedure TIRCServer.SRVOnExecute(AContext: TIdContext);
begin
  //THIS function represts the >>> ENTIRE LIFETIME <<< of an
  //IRC connection.   Resources are setup in the start of the function
  //cleaned up at the end... pretty simple.
  Debug.Log('Hai!');


  var cli := TIRCConnection.Create;
  try
    cli.context := Acontext;
    self.FCons.Add(cli);//<<connection is added to shared so we connection can
                        //find by nick and and broadcast messages around

    while AContext.Connection.Connected do begin
      Acontext.Connection.IOHandler.ReadTimeout := 10;
      var s := Acontext.Connection.IOHandler.ReadLn();
      if s <> '' then begin
        handle_line(cli, s);
      end;
    end;

  finally
    cli.context := nil;
    RemoveFromAllchannels(cli);

    FCons.Remove(cli);
    cli.free;
  end;


end;

procedure TIRCServer.StartListening;
begin
  if idsrv = nil then begin
    idsrv := TIdTCPServer.create(nil);
    idsrv.DefaultPort := port;
    idsrv.MaxConnections := 10000;
    idsrv.OnConnect := SRVOnConnect;
    idsrv.OnDisconnect := SRVOnDisconnect;
    idsrv.OnExecute := SRVOnExecute;
    idsrv.active :=true;
  end;

end;

procedure TIRCServer.StopListening;
begin
  idsrv.free;
  idsrv := nil;

end;

{ TIRCConnection }

procedure TIRCConnection.send_generic_msg(id: string; sTrailer: string);
begin
  var sLine := id+' '+Nick+' :'+sTrailer;
  context.Connection.IOHandler.WriteLn(sLine);
  debug.log('>>>>>'+sLine);
end;

procedure TIRCConnection.channel_msg(from: TIRCConnection; chan: TIRCChannel;
  sTrailer: string);
begin
  //:[MG]-Request|Bot|Charlie!root@Movie.Gods.Rules PRIVMSG #moviegods :#337  33x [2.8G] Tropic.Thunder.2008.720p.BluRay.x264-x0r.mkv
  //^^^^user^^^^^^^^^^^^^^^^^!^^^^identsoemthing^^^ PRIVMSG ^^CHAN^^^^ :msg.......


 // :[MG]-MISC|EU|S|SpeedStick!root@Movie.Gods.Rules PRIVMSG #moviegods :#732   9x [5.5G] Kingdom.2019.GERMAN.720p.BluRay.x264-UNiVERSUM.mkv Process MovieWeb.exe (17676)

//  var sLine := ':'+chan.name+'!'+nick+' PRIVMSG '+chan.name+' ';
  var sLine := '';


  if from = nil then
    sLine := ':'+chan.name+'!'+chan.name+' PRIVMSG '+chan.name+' '
  else
    sLine := ':'+from.nick+'!'+from.nick+'@nowhere'+' PRIVMSG '+chan.name+' ';


  sLine := sLine+sTrailer;

  if context.connection <> nil then
    if context.connection.iohandler <> nil then
      context.Connection.IOHandler.WriteLn(sLine);
end;

procedure TIRCConnection.cmdmsg(sCmd, sFrom: string);
begin
  if sFrom = '' then
    sFrom := 'system!system@system';
  var sLine := ':'+sFrom+' '+sCmd;
  debug.log('>>cmd>>'+sLine);
  if assigned(context) then
    if assigned(context.connection) then
      if assigned(context.connection.iohandler) then
        context.connection.iohandler.writeln(sLine);

end;

function TIRCConnection.FullIdent: string;
begin
  result := nick+'!'+user;
end;


procedure TIRCConnection.ircmsg(sCmd, sParams: string);
begin
  var sLine := sCmd;
  if sParams <> '' then
    sLine := sLine+' '+sParams;
  if assigned(context) then
    if assigned(context.connection) then
      if assigned(context.connection.iohandler) then
        context.connection.iohandler.writeln(sLine);
end;

procedure TIRCConnection.msg_chan_to_specific_user_raw(chan: TIRCChannel;
  sRaw: string; sPreChan: string = '');
begin
  var sLine := ':system!system@system '+sPreChan+chan.name+' '+sRaw;
  context.Connection.IOHandler.WriteLn(sLine);
  debug.log('>>ch>>'+sLine);
end;

procedure TIRCConnection.privmsg(sTarget, sMessage, sFrom: string);
begin
  if sFrom = '' then
    sFrom := 'system!system@system';

  var sLine := ':'+sFrom+' PRIVMSG '+sTarget+' :'+sMessage;
  context.Connection.IOHandler.WriteLn(sLine);
  debug.log('>>>>>'+sLine);
end;

procedure TIRCConnection.send_generic_msg(id: string; params: TArray<string>;
  sTrailer: string);
begin
  var sLine := id+' '+Nick+' ';
  for var t:= 0 to high(params) do begin
    sLine := sLine + params[t]+' ';
  end;
  sLine := sLine+':'+sTrailer;

  context.Connection.IOHandler.WriteLn(sLine);
  debug.log('>>>>>'+sLine);
end;

{ TIRCChannel }

constructor TIRCChannel.Create;
begin
  inherited;
  users := TSharedList<TIRCConnection>.create;


end;

procedure TIRCChannel.Detach;
begin
  if detached then exit;

  Users.free;
  Users := nil;
  inherited;

end;

procedure TIRCChannel.EchoNames(toCon: TIRCConnection);
begin
  var l := users.LockI;
  for var t:= 0 to users.count-1 do begin
    //:sniper.ny.us.abjects.net 353 panda-WEB2012R2-43862 = #mg-chat :lbws1999 +BRS72 +daniagatha +goldrak +AtomicFrost +LammMann +asdffdgdfg +bzzbrr usera0zG6bh2mh Marlin +icekold +tectec +fedorano +m0rn +bookerman  Process MovieWeb.exe (17676)
    toCon.cmdmsg('353 '+toCon.nick+' = '+name+' :'+users[t].nick);
  end;
  //:sniper.ny.us.abjects.net 366 panda-WEB2012R2-43862 #beast-xdcc :End of /NAMES list. Process MovieWeb.exe (17676)
  toCon.cmdmsg('366 '+toCon.nick+' '+name+' :END OF /NAMES list');
end;

procedure TIRCChannel.EchoWhoHere(toCon: TIRCConnection);
begin
  var l := users.LockI;
  var sUsers := '';
  for var t:= 0 to users.count-1 do begin
    //:sniper.ny.us.abjects.net 353 panda-WEB2012R2-43862 = #mg-chat :lbws1999 +BRS72 +daniagatha +goldrak +AtomicFrost +LammMann +asdffdgdfg +bzzbrr usera0zG6bh2mh Marlin +icekold +tectec +fedorano +m0rn +bookerman  Process MovieWeb.exe (17676)
    if t < users.count-1 then
      susers := susers + users[t].nick+' '
    else
      susers := susers + users[t].nick;
  end;
  toCon.cmdmsg('WHOHERE '+name+' '+sUsers);
end;

procedure TIRCChannel.join(con: TIRCConnection);
begin
  var lck := users.locki;

  if not users.Has(con) then begin
    con.cmdmsg('JOIN :'+name, con.FullIdent);
    con.cmdmsg('332 '+con.nick+' '+name+' :Welcome to the channel... i think...');
    con.cmdmsg('333 '+con.nick+' '+name+' yourmom 12341234');
//    con.msg_chan_to_specific_user_raw(self, '333', 'yourmom 123412341234');
  end;
  if users.Has(con) then
    exit;

  users.Add(con);
  EchoNames(con);
  NotifyJoined(con);

//  msg_everyone_in_channel(con,con.nick+' joined '+name+' there are now '+users.count.tostring+' users in this channel');

end;

procedure TIRCChannel.leave(con: TIRCConnection);
begin
  var lck := users.locki;
  users.remove(con);
  NotifyLeft(con);
  if users.count = 0 then
    irc.channelempty(self);



end;

procedure TIRCChannel.msg_everyone_in_channel(confrom: TIRCConnection; sMsg: string; bExcludeSelf: boolean = true);
begin
  var l := users.locki;
  for var t := 0 to users.count-1 do begin
    var con := users[t];
    if (not bExcludeSelf) or (con<>conFrom) then
      con.channel_msg(conFrom, self,sMsg);

  end;

end;

procedure TIRCChannel.NotifyJoined(con: TIRCConnection);
begin
  var l := users.locki;
  for var t := 0 to users.count-1 do begin
    users[t].cmdmsg('JOIN '+name+' '+con.nick, con.fullident);
  end;
end;

procedure TIRCChannel.NotifyLeft(con: TIRCConnection);
begin
  var l := users.locki;
  for var t := 0 to users.count-1 do begin
    users[t].cmdmsg('PART '+name+' '+con.nick,con.fullident);
  end;

end;

procedure TIRCChannel.privmsg_everyone_in_channel(confrom: TIRCConnection;
  sMsg: string);
begin
  var l := users.locki;
  for var t := 0 to users.count-1 do begin
    users[t].privmsg(name, sMsg, confrom.fullident);
  end;
end;

end.
