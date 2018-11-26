unit NetLogServer;

interface

uses
{$IFDEF MSWINDOWS}
  winapi.windows,//for inline support
{$ENDIF}
  SYstem.SysUtils, numbers, herro,
  betterobject, classes,systemx, betteridsockethandle, idglobal, skill, typex, stringx, better_indy,BetterIdUDPServer, orderlyinit, tickcount, simplequeue, networkx, PeriodicEvents, netlogpacket, debug, managedthread;


const
  LOG_PAYLOAD_START = 4;
type
  TLogResult = record
    oldAckLevel: int64;
    ackLevel: int64;
  end;
  TLogEvent = function (sLog: string; sLogID, sApp, sPeerIP: string; sPeerEndPoint: string): TLogResult of object;

  TNetLogServer = class;//forward



  Tqi_UDPIn = class(TQueueItem)
  protected
    procedure DoExecute; override;
  public
    nls: TNetLogServer;
    bytes: TDynByteArray;
    ABinding: TPeerInfo;
  end;

  Tqi_UDPOut = class(TQueueItem)
  protected
    procedure DoExecute; override;
  public
    nls: TNetLogServer;
    bytes: TIdBytes;
    ABinding: TPeerInfo;
  end;





  TNetLogServer = class(TIdUDPServer)
  private
    queue: TSimplequeue;
    QUEUEOUT: TsimpleQueue;
    FOnLog: TLogEvent;
    lck: TCLXCriticalSection;
  protected
    procedure DoUDPRead(AThread: TIdUDPListenerThread; const XData: TIdBytes; ABinding: TIdSocketHandle);override;
    procedure DoUDPRead_Sync(XData: TDynByteArray; ABinding: TPeerInfo);
    procedure HandleLogL(XData: TDynByteArray; ABinding: TPeerInfo);
    procedure HandlePing(ABinding: TPeerInfo);
    procedure SendAck(ABinding: TPeerInfo; acklevel: int64);
    procedure QueueSend(peerip: string; peerport: word; XData: TidBytes);overload;
    procedure QueueSend(peerip: string; peerport: word; s: string);overload;

  public
    constructor create;
    destructor Destroy;override;
    property OnLog: TLogEvent read FOnLog write FOnLog;
  end;




var
  LogServer: TNetLogServer;
implementation

{ TPeriodicGreeting }


{ TNetLogServer }


constructor TNetLogServer.create;
begin
  queue := TPM.Needthread<TSimpleQueue>(nil);
  queue.MaxItemsInQueue := 512;
  queue.start;

  queueout := TPM.Needthread<TSimpleQueue>(nil);
  queueout.MaxItemsInQueue := 512;
  queueout.start;


  self.ThreadedEvent := true;
  inherited create;
  ics(lck);

end;

destructor TNetLogServer.Destroy;
begin
  queueout.Stop;
  queue.Stop;
  queueout.waitfor;
  queue.waitfor;
  TPM.NoNeedThread(queueout);
  TPM.NoNeedThread(queue);

  dcs(lck);
  inherited;
end;

procedure TNetLogServer.DoUDPRead(AThread: TIdUDPListenerThread;
  const XData: TIdBytes; ABinding: TIdSocketHandle);
var
  qi: Tqi_UDPIn;
  dba: TDynByteArray;
begin
  inherited;
  if length(XData) = 0 then
    exit;
  qi := Tqi_UDPIn.create;
  setlength(dba, length(xData));
  movemem32(@dba[0], @XData[0], length(xdata));
  qi.bytes := dba;
  qi.ABinding.PeerIP := ABinding.PeerIP;
  UniqueString(qi.ABinding.PeerIP);
  qi.ABinding.PeerPort := ABinding.PeerPort;
  qi.nls := self;
  qi.AutoDestroy := true;
  queue.AddItem(qi);
end;


procedure TNetLogServer.DoUDPRead_Sync(XData: TDynByteArray; ABinding: TPeerInfo);
var
  h: TLogPacketHeader;
begin
  ecs(lck);
  try
  if length(xData) < sizeof(h) then
    exit;
  h := PLogPacketHeader(Pbyte(@XData[0]))^;
  if h.IsLogL then begin
    HandleLogL(XData, ABinding);
  end;
  IF H.IsPing then begin
    HandlePing(ABinding);
  end;
  finally
    lcs(lck);
  end;



end;


procedure oinit;
begin
  Debug.Log('Binding NetLogSever');
  LogServer := TNetLogServer.create;
  LogServer.DefaultPort := 1111;
//  LogServer.ReuseSocket := rsFalse;
  LogServer.BroadcastEnabled := true;
  LogServer.Active := true;

  skill.RegisterLocalSkill('NetLogServer', 0, '1111', 'NetLog/UDP');
  Debug.Log('Bound');

end;

procedure oprefinal;
begin

  LogServer.Active := false;
  LogServer.free;
  LogServer := nil;
end;

procedure ofinal;
begin
  //
end;

procedure olatefinal;
begin
  //
end;



procedure TNetLogServer.HandleLogL(XData: TDynByteArray;
  ABinding: TPeerInfo);
var
  s: ansistring;
  ss,sr: string;
  sLogID: string;
  sAPP: string;
  strl: TStringlist;
  t: ni;
  ll,lres: TLogResult;
  ackLevel: int64;
  maxack: int64;
  outoforder: boolean;
begin
  setlength(s, length(XData)-LOG_PAYLOAD_START);
  movemem32(@s[STRZ], @XData[LOG_PAYLOAD_START], length(XData)-LOG_PAYLOAD_START);

  ackLevel := -1;
  ss := s;
  strl := ParseString(ss, #27);
  try
    maxack := -1;
    outoforder := false;
    for t := 1 to strl.count-1 do begin
      ss := strl[t];
      sr := ss;
      SplitString(sr, ',', sLogID,sr);
      SplitString(sr, ',', sAPP,sr);

      if assigned(OnLog) then begin
        lRes := OnLog(sr,sLogID,sApp, ABinding.PeerIP, Abinding.PeerPort.tostring);
        if lres.acklevel < 0 then
          Debug.consolelog('here');
        if lres.oldAckLevel <> lres.acklevel then begin
          acklevel := lres.acklevel;
          maxack := greaterof(ackLevel, maxack);

        end else begin
          acklevel := lres.acklevel;
          outoforder := true;
          maxack := greaterof(ackLevel, maxack);
        end;
      end;
    end;
    if maxack >= 0 then
      SendAck(ABINDING, ackLevel);

    if outoforder then
      QueueSend(ABinding.PeerIP, abinding.PeerPort, 'LOGO');


  finally
    strl.free;
  end;




end;



procedure TNetLogServer.HandlePing(ABinding: TPeerInfo);
begin
  QueueSend(ABinding.PeerIP, abinding.PeerPort, 'PONG');
end;

procedure TNetLogServer.QueueSend(peerip: string; peerport: word;
  XData: TidBytes);
var
  qi: Tqi_UDPOut;
begin
  qi := Tqi_UDPOut.create;
  qi.autodestroy := true;
  qi.nls := self;
  qi.bytes := XData;
  qi.ABinding.PeerIP := peerip;
  qi.ABinding.PeerPort := peerport;
  queueout.AddItem(qi);

end;

procedure TNetLogServer.QueueSend(peerip: string; peerport: word; s: string);
begin
  QueueSend(peerip, peerport, ToBytes(s, nil));
end;

procedure TNetLogServer.SendAck(ABinding: TPeerInfo; acklevel: int64);
var
  ack: TLogAckPacket;
  idb: TIDBytes;
begin
  ack.h.SetLogA;
  ack.ackLevel := ackLevel;
  setlength(idb, sizeof(ack));
  movemem32(@idb[0], pbyte(@ack), sizeof(ack));
//  Debug.ConsoleLog('Sending ACK:'+ack.acklevel.tostring);
  Self.SendBuffer(abinding.PeerIP, abinding.PeerPort, idb);


end;

{ TLogPacketHeader }


{ Tqi_UDPIn }

procedure Tqi_UDPIn.DoExecute;
begin
  inherited;
//  Debug.ConsoleLog('Server:'+MemoryToString(@bytes[0], length(bytes)));
  nls.DoUDPRead_Sync(bytes, ABinding);
end;

{ Tqi_UDPOut }

procedure Tqi_UDPOut.DoExecute;
begin
  inherited;
  nls.SendBuffer(ABinding.PeerIP, ABinding.PeerPort, Bytes);

end;

initialization

orderlyinit.init.RegisterProcs('NetLogServer', oinit, oprefinal, ofinal, olatefinal, 'skill,indy,managedthread');

end.




