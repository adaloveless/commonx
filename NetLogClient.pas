unit NetLogClient;

interface

uses
  betterobject, classes,systemx, betteridsockethandle, idglobal, skill, typex, stringx, better_indy,BetterIdUDPServer, orderlyinit, tickcount, simplequeue, networkx, PeriodicEvents, netlogpacket, sysutils, herro, managedthread, debug, numbers;


const
  LOG_PAYLOAD_START = 4;
{$IFDEF MSWINDOWS}
  LOG_PACKET_LIMIT = 40000;
{$ELSE}
  LOG_PACKET_LIMIT = 400;
{$ENDIF}
  SEND_AHEAD_INC = 0.01;
type
  TLogEvent = function (sLog: string; sLogID, sApp, sPeerID: string; sPeerEndPoint: string): int64 of object;

  TlogSendThread = class;//forward
  TNetLogClient = class;//forward


  Tqi_UDPIn = class(TQueueItem)
  protected
    procedure DoExecute; override;
  public
    nlc: TNetLogClient;
    bytes: TIdBytes;
    ABinding: TPeerInfo;
  end;

  Tqi_UDPOut = class(TQueueItem)
  protected
    procedure DoExecute; override;
  public
    nlc: TNetLogClient;
    bytes: TIdBytes;
    ABinding: TPeerInfo;
  end;


  TPendingLog = record
    log: string;
    time: ticker;
    resendtime: ticker;
    id: int64;
  end;

  TNetLogClient = class(TIdUDPServer)
  private
    FIncomingLogs: TArray<TPendingLog>;
    FPendingLogs: TArray<TPendingLog>;

    FOnLog: TLogEvent;
    pendingid: int64;
    FLogPort: word;
    FLogHost: string;
    lastseen: ticker;
    lastconnect: ticker;
    lastSend: ticker;
    lastPing: ticker;
    bestAckTime: ticker;
    thr: TLogSendThread;
    lck: TCLXCriticalSection;
    lckIn: TCLXCriticalSection;
    procedure SetLogHost(const Value: string);
    procedure SetLogPort(const Value: word);
    procedure Lock;
    procedure Unlock;
    procedure LogHook(s: string);
    procedure GatherIncomingLogs;
  protected
    sendahead: single;
    queue: TSimpleQueue;
    queueout: TSimpleQueue;
    procedure DoUDPRead(AThread: TIdUDPListenerThread; const XData: TIdBytes; ABinding: TIdSocketHandle);override;
    procedure DoUDPRead_Sync(XData: TIdBytes; ABinding: TPeerInfo);
    procedure SendLogs;
    procedure SendReset;
    procedure HandleAck(XData: TIdBytes; ABinding: TPeerInfo);
    procedure AddLog(s: string);
    procedure ClearLogs(ackto: int64);
    procedure QueueSend(peerip: string; peerport: word; XData: TidBytes);overload;
    procedure QueueSend(peerip: string; peerport: word; s: string);overload;
    procedure Reset;
    procedure DoWerk;
    procedure HookIntoLog;
    procedure UnhookFromLog;
  public

    Connected: boolean;
    constructor Create;
    destructor Destroy;override;
    property LogHost: string read FLogHost write SetLogHost;
    property LogPort: word read FLogPort write SetLogPort;
    procedure CheckForLogServer;

  end;


  TLogSendThread = class(TManagedThread)
  public
    nlc: TNetLogClient;
    procedure DoExecute; override;
    procedure Init; override;
  end;



var
  LogClient: Tnetlogclient;
  LogServerHostOverride: string = '';
  LogServerEndpointOverride: string = '';
implementation

uses
  applicationparams;

{ TPeriodicGreeting }


{ Tnetlogclient }


procedure TNetLogClient.AddLog(s: string);
begin
  ecs(lckIn);
  try
    setlength(FIncomingLogs, Length(FIncomingLogs)+1);
    FIncomingLogs[high(FIncomingLogs)].log := s;
    FIncomingLogs[high(FIncomingLogs)].time := getticker;
    FIncomingLogs[high(FIncomingLogs)].resendtime := 0;
    FIncomingLogs[high(FIncomingLogs)].id := pendingid;
    inc(pendingid);
  finally
    lcs(lckin);
  end;

end;

procedure TNetLogClient.CheckForLogServer;
var
  sk: TSkillDef;
begin
  if gettimesince(lastseen) > 12000 then begin
    if connected then begin
//      Debug.ConsoleLog('NetLog Disconnected (timed out)!');
      connected := false;
      UnhookFromLog;
    end;
  end;
  if not connected then begin
    if logServerHostOverride = '' then begin
      sk := skills.Find('NetLogServer');
      if sk <> nil then begin
        if gettimesince(lastconnect) > 1000 then begin
          LogHost := sk.info.host;
          LogPort := strtoint(sk.info.endpoint);
          Reset;
          lastconnect := getticker;
        end;

      end;
    end else begin
      if gettimesince(lastconnect) > 1000 then begin
        LogHost := LogServerHostOverride;
        LogPort := strtoint(LogServerEndpointOverride);
        Reset;
        lastconnect := getticker;
      end;
    end;
  end else begin
    if (gettimesince(lastping) > 1000)
    and (gettimesince(lastseen) > 1000) then begin
      lastping := getticker;
      QueueSend(Loghost, Logport, 'PING');
    end;
  end;
end;

procedure TNetLogClient.ClearLogs(ackto: int64);
var
  newlen: ni;
  firstunacked: ni;
  t: ni;
  tmNow: ticker;
begin

  tmNow := Getticker;
  firstunacked := -1;

  for t:= 0 to high(fPendingLogs) do begin
    if FPendingLogs[t].id > ackto then begin
      firstunacked:= t;
      break;
    end;
  end;
  if firstunacked < 0 then
    firstunacked := length(FPendingLogs);


  newlen := length(Fpendinglogs)-firstunacked;
  if newlen < 0 then begin
    exit;
  end;



  for t:= 0 to firstunacked-1 do begin
    bestacktime := lesserof(bestacktime, gettimesince(tmNow,greaterof(FPendingLogs[t].resendtime,FPendingLogs[t].time)));
  end;



  for t:= 0 to newlen-1 do begin
//    if FpendingLogs[t].id < ackto then
//      Debug.ConsoleLog('Clearing Log:'+FpendingLogs[t].id.tostring);
    FPendingLogs[t] := FPendingLogs[t+firstunacked];
    sendahead := sendahead+ SEND_AHEAD_INC;
  end;
  setlength(fPendingLogs, newlen);

end;

constructor TNetLogClient.Create;
begin

  queue := TPM.Needthread<TSimpleQueue>(nil);
  queue.beginstart;
  queueout := TPM.Needthread<TSimpleQueue>(nil);
  queueout.beginstart;


  inherited Create;

  bestAckTime := 200;
  self.ThreadedEvent := true;

  ics(lck);
  ics(lckin);
  setlength(fPendingLogs, 0);
  Bindings.DefaultPort := 0;
  Port := 0;
  Active := true;


  thr := TPM.Needthread<TlogSendThread>(nil);
  thr.nlc := self;
  thr.loop := true;
  thr.betterpriority := bpHigher;
  thr.beginstart;




end;

destructor TNetLogClient.Destroy;
begin
  thr.stop;
  thr.waitfor;
  queue.Stop;
  queueout.stop;
  queue.WaitForFinish;
  queueout.WaitForFinish;
  TPM.NoNeedthread(queue);
  TPM.NoNeedthread(thr);
  thr := nil;

  dcs(lck);
  dcs(lckin);
  inherited;
end;

procedure Tnetlogclient.DoUDPRead(AThread: TIdUDPListenerThread;
  const XData: TIdBytes; ABinding: TIdSocketHandle);
var
  qi: Tqi_UDPIn;
  dba: TIdBytes;
begin
  inherited;
  if queue.estimated_queue_size > 4 then begin
//    Debug.ConsoleLog('queue too full ! ' +queue.estimated_queue_size.tostring);
//    exit;
  end;

  if length(XData) = 0 then
    exit;
  qi := Tqi_UDPIn.create;
  setlength(dba, length(xData));
  movemem32(@dba[0], @XData[0], length(xdata));
  qi.bytes := dba;
  qi.ABinding.PeerIP := ABinding.PeerIP;
  UniqueString(qi.ABinding.PeerIP);
  qi.ABinding.PeerPort := ABinding.PeerPort;
  qi.nlc := self;
  qi.AutoDestroy := true;
  queue.AddItem(qi);


end;

procedure Tnetlogclient.DoUDPRead_Sync(XData: TIdBytes; ABinding: TPeerInfo);
var
  h: TLogPacketHeader;
begin
  inherited;
  ecs(lck);
  try
    if length(xData) < sizeof(h) then
      exit;
    h := PLogPacketHeader(Pbyte(@XData[0]))^;
    if h.IsLogA then begin
      HandleAck(XData, ABinding);
    end;
    if h.IsPong then begin
      lastSeen := getticker;
      if not connected then begin
        connected := true;
        HookIntoLog;
        Debug.ConsoleLog('NetLog Connected by PONG!');
      end;
{$IFDEF LOG_PONG}
      FPendingLogs.Add('PONG!');
      thr.haswork := true;
{$ENDIF}
    end;
    if h.IsLogO then begin
       sendahead := 0;
    end;
  finally
    lcs(lck);
  end;


end;


procedure TNetLogClient.DoWerk;
begin
  try
    Lock;
    try
      CheckForLogServer;
      if length(FPendingLogs) = 0 then
        GatherIncomingLogs;

//      Debug.ConsoleLog('dowerk');
      thr.ColdRunInterval := 30;
      thr.haswork := (length(FPendingLogs) > 0) or not connected;
      thr.NoWorkRunInterval := 100;

      thr.RunHot := false;

      if connected then
        Self.SendLogs;

    finally
      unlock;
    end;
  except
    on e:exception do begin
      thr.status := e.message;
    end;
  end;


end;

procedure TNetLogClient.GatherIncomingLogs;
begin
  ecs(lckIn);
  try
    FPendingLogs := FIncomingLogs;
    setlength(Fincominglogs, 0);
//    Debug.ConsoleLog('Gathered '+length(FPendingLogs).tostring);
  finally
    lcs(lckin);
  end;
end;

procedure TNetLogClient.HandleAck(XData: TIdBytes; ABinding: TPeerInfo);
var
  ackto: int64;
begin
  lock;
  try
    lastseen := getticker;
    if not connected then begin
      Connected := true;
      HookIntoLog;
      AddLog('ACK!');
      Debug.ConsoleLog('Connected!');
    end;
    thr.haswork := true;
    ackto := PInt64(@XDAta[LOG_PAYLOAD_START])^;
//    debug.consolelog('got ack '+ackto.tostring);
    ClearLogs(ackto);
  finally
    Unlock;
  end;

end;

procedure TNetLogClient.HookIntoLog;
begin
  Debug.consoleLog('hook to netlog');
  Debug.DebugLog.LogHook := self.LogHook;
end;

procedure TNetLogClient.Lock;
begin
  ecs(lck);
end;

procedure TNetLogClient.LogHook(s: string);
begin
  ecs(lckIn);
  try
    AddLog(s);
    thr.HasWork := true;
  finally
    lcs(lckIn);
  end;
end;

procedure TNetLogClient.QueueSend(peerip: string; peerport: word; s: string);
begin
  QueueSend(peerip, peerport, ToBytes(s, nil));
end;

procedure TNetLogClient.QueueSend(peerip: string; peerport: word;
  XData: TidBytes);
var
  qi: Tqi_UDPOut;
begin
  qi := Tqi_UDPOut.create;
  qi.autodestroy := true;
  qi.nlc := self;
  qi.bytes := XData;
  qi.ABinding.PeerIP := peerip;
  qi.ABinding.PeerPort := peerport;
  queueout.AddItem(qi);

end;

procedure TNetLogClient.Reset;
begin
  SendReset;
end;

procedure oinit;
begin
  Debug.Log('Binding NetLogClient');
  LogClient := Tnetlogclient.create;
  Debug.Log('Bound');

  LogServerHostOverride := APGEt('LogServerHost','');
  LogServerEndpointOverride := APGEt('LogServerEndpoint','1111');


end;

procedure oprefinal;
begin
  LogClient.UnhookFromLog;

  LogClient.Active := false;
  LogClient.free;
  LogClient := nil;
end;

procedure ofinal;
begin
  //
end;

procedure olatefinal;
begin
  //
end;








procedure TNetLogClient.SendLogs;
var
  t,p: ni;
  sLine: ansistring;
  sPacket: ansistring;
  sent: ni;
  lastp: ni;
begin
  if length(FPendingLogs) = 0 then
    exit;


  lastSend := getTicker;

  sPacket := 'LOGL';
  t :=0;
  p := 0;
  lastp := 0;
  sent := 0;
  while t <= high(FPendingLogs) do begin
    while p<= high(FPendingLogs) do begin
      if gettimesince(FPendingLogs[p].resendtime) > greaterof(bestAckTime,20) then begin
        sLine := #27+(FPendingLogs[p].id).tostring+','+extractfilename(dllname)+','+FPendingLogs[p].log;
        if (p = t) and (length(sLine)>(LOG_PACKET_LIMIT-4)) then begin
          sline := zcopy(sLine, 0, (LOG_PACKET_LIMIT-4));
        end else begin
          if length(sLIne) + length(sPacket) > LOG_PACKET_LIMIT then begin
            inc(p);
            break;
          end;
        end;

//        Debug.ConsoleLog('Sending: '+FPendingLogs[p].id.tostring);
        sPacket := sPacket + sLine;
        FPendingLogs[p].resendtime := GetTicker;
        lastp := p;
      end else
        break;
      inc(p);
    end;

    if p>t then begin
      if length(sPacket) > 4 then begin
//        Debug.ConsoleLog('Sending: '+FPendingLogs[0].id.tostring+'-'+FPendingLogs[lastp].id.tostring);
        self.QueueSend(Loghost, LogPort, sPacket);
        sPacket := 'LOGL';
        inc(sent);
        if sent > greaterof(sendahead,2.0) then
          break;
      end;
    end;
    t := p;
  end;



end;

procedure TNetLogClient.SendReset;
begin
  Lock;
  try
    if active then begin
      QueueSend(Loghost, Logport, 'REST');
      QueueSend(Loghost, Logport, 'PING');
    end;
  finally
    Unlock;
  end;
end;


procedure TNetLogClient.SetLogHost(const Value: string);
begin
  FLogHost := Value;
end;

procedure TNetLogClient.SetLogPort(const Value: word);
begin
  FLogPort := Value;
end;

procedure TNetLogClient.UnhookFromLog;
begin
  Debug.consoleLog('unhook from netlog');
  Debug.DebugLog.LogHook := nil;
end;

procedure TNetLogClient.Unlock;
begin
  lcs(lck);
end;

{ TLogPacketHeader }


{ TLogSendThread }

procedure TLogSendThread.DoExecute;
begin
  inherited;
  nlc.DoWerk;
end;

procedure TLogSendThread.Init;
begin
  inherited;

end;

{ Tqi_UDPIn }

procedure Tqi_UDPIn.DoExecute;
begin
  inherited;
  nlc.DoUDPRead_Sync(self.bytes, ABinding);

end;

{ Tqi_UDPOut }

procedure Tqi_UDPOut.DoExecute;
begin
  inherited;
  nlc.SendBuffer(ABinding.PeerIP, ABinding.PeerPort, Bytes);
end;

initialization

orderlyinit.init.RegisterProcs('netlogclient', oinit, oprefinal, ofinal, olatefinal, 'skill,indy,debug');

end.




