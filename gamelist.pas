unit GameList;

interface

uses debug, tickcount, sysutils, sharedobject, systemx, generics.collections.fixed,
    managedthread, serverinterface, serverinterfaceinterface,
    DataObjectServices, dataobject, dataobjectcache, dataobjectcachejanitor,
    Xref, dataobjectcachemanager, betterobject, packet, commands_system, orderlyinit;

const
  KEY_GAME_ID = 11111;
  MT_NO_VECTOR = 0;
  MT_VECTOR3 = 1;
  MT_VECTOR4 = 2;
  MT_DUAL_VECTOR3 = 3;
  MT_DUAL_VECTOR4 = 4;
  NETID_GAMEBOARD = -1;
  LAG_OUT_TIME = 10000;

type
  TMSTime = ticker;
  TUnityTime = double;
  TGame = class;//forward

  TGameState = (gsForming, gsStarted, gsFinished);




  TgameFunction = (
      gfHello, gfGoodbye, gfMoveCursor, gfMarkBlock, gfBreakBlock, gfDropBlock, gfCommitBlock, gfGameBoardOffset, gfDefeat, gfNewOpponent, gfGarbageDump, gfReady, gfNotReady, gfGameStart, gfSpawn, gfAssignOpponents,gfLaggedOut, gfVictory
  );

  TGameEvent = class(TBetterObject)
  public
    GameTick: double;
    MessageType: byte;
    FuncID: TGameFunction;
    SessionID:int64;
    NetObjectID: int64;
    StringData: string;
    Singularity: boolean;
    x,y,z,w: double;
    x1,y1,z1,w1: double;
    procedure WriteToPacket(p: TRDTPPacket);
    procedure ReadFromPacket(p: TRDTPPacket);
    function Copy: TGameEvent;
  end;

  TGameStream = class(TSharedObject)
  private
    function GetCount: nativeint;
    function GetEvent(idx: integer): TGameEvent;
  protected
    Fevents: TList<TGameEvent>;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure ClearEvents;
    procedure WriteToPacket(p: TRDTPPacket);
    procedure ReadFromPacket(p: TRDTPPacket);
    procedure SyncEventCount(count: integer);
    procedure AddEvent(ge: TGameEvent);
    function Copy: TGameStream;overload;
    function Copy(iStartingAt: nativeint): TGameStream;overload;
    procedure CopyTo(gs: TGameStream);
    property Count: nativeint read GetCount;
    property Events[idx: integer]: TGameEvent read GetEvent; default;
    procedure Merge(gs: TGameStream);
  end;





  TPlayer = class(TSharedObject)
  strict private
    FInEvents: TGameStream;
    FOutEvents: TGameStream;
    FEventInddx: nativeint;
  private
    FLocalJoinTime: double;
    FUserID: int64;
    FSessionID: int64;
    FGame: TGame;
    FReady: boolean;
    FEventIndex: integer;
    FOpponent: TPlayer;
    FDefeated: boolean;
    FLastSeen: TMSTime;
    function GetTimeOFfset: double;
    function GEtTotalEVentsReceived: int64;
    function GetIncomingEventCount: nativeint;
    function GetLaggedOut: boolean;
  public
    constructor create;override;
    destructor destroy;override;

    property UserID: int64 read FUserID write FUserID;
    property SessionID: int64 read FSessionID write FSessionID;
    property TimeOffset: double read GetTimeOFfset;
    property LocalJoinTime: double read FLocalJoinTime write FLocalJoinTime;
    property Game: TGame read FGame write FGame;
    function GetAvailableEvents(STartingAt: int64): TGameStream;overload;
    procedure GetAvailableEvents(gs: TGameStream; STartingAt: int64);overload;
    property TotalEventsReceived: int64 read GEtTotalEVentsReceived;
    procedure InfuseIncomingEvents(gs: TGameStream; bFromThisPlayer: boolean);
    procedure InfuseEvent(e: TGameEvent; bFromThisPlayer: boolean);
    procedure ProcessEvents();
    function ProcessEvent(e: TGameEvent): boolean;
    property Ready: boolean read FReady write FReady;
    property IncomingEventCount: nativeint read GetIncomingEventCount;
    property Opponent: TPlayer read FOpponent write FOpponent;
    property Defeated: boolean read FDefeated write FDefeated;
    property LAstSeen: TMSTime read FLastSeen;
    property LaggedOut: boolean read GetLaggedOut;
    procedure Seen;
  end;


  TGame = class(TSharedObject)
  private
    FTimeBase: double;
    FStatusMessage: string;
    FID: int64;
    FCreationTime: TMSTime;
    FLastACtivityTime: TMSTime;
    FPlayers: TList<TPLayer>;
    FEvents: TGameStream;
    FGameState: TGameState;
    function GetStatusMessage: string;
    procedure SetStatusMessage(const Value: string);
    function GetPlayerCount: integer;
    function GetPlayer(idx: integer): TPLayer;
    function AllPlayersREady: boolean;
    procedure StartGame;
    procedure StartGameIfAllReady;
    procedure REassignOpponents();
    function ReassignOpponent: boolean;
    function FindUnassignedOpponent(pNot: TPlayer): TPlayer;
    procedure AddEvent_ServerToClient(e: TGameEvent);
    procedure DefeatPlayer(p: TPlayer);
    function GEtPlayersRemaining: nativeint;
    function GetundefeatedPLayer: TPlayer;
    procedure Victory;
    function GetIsStale: boolean;
  public
    constructor create;override;
    destructor destroy;override;
    property StatusMessage: string read GetStatusMessage write SetStatusMessage;
    property ID: int64 read FID write FID;
    property CreationTime: TMSTime read FCreationtime;
    property LastActivityTime: TMSTime read FLastACtivityTime;
    property Players[idx: integer]: TPLayer read GetPlayer;
    function AddPlayer(iSessionID, iUserID: int64): TPlayer;
    procedure RemovePlayer(iUserID: int64);
    property PlayerCount: integer read GetPlayerCount;
    function GetPlayerBySessionID(iSessionID: int64): TPlayer;
    procedure AddEvent(e: TGameEvent);
    function InfuseIncomingEvents(gs: TGameStream; p: TPlayer): integer;
    procedure ProcessEvents(gs: TGameStream);
    procedure CheckGameConditions;
    property PlayersRemaining: nativeint read GEtPlayersRemaining;
    procedure ProcessEvent(e: TGameEvent);
    procedure InfuseEvent(e: TGameEvent; iFromSessionID: int64);
    property GameState: TGAmeState read FGameState;
    property IsStale: boolean read GetIsStale;

  end;

  TGameList = class(TLockQueuedObject)
  private
    FList: TList<TGame>;
    function GEtCount: nativeint;
    function GetGame(idx: integer): TGame;
    procedure RemoveGame(g: TGame);
  public
    constructor create;override;
    destructor destroy;override;

    property Games[idx: integer]: TGame read GetGame;
    procedure AddGame(g: TGame);
    function IndexOf(g: TGame): nativeint;overload;
    function HasGame(g: TGame): boolean;overload;
    function IndexOf(iGameID: int64): nativeint;overload;
    function HasGame(iGameID: int64): boolean;overload;
    property Count: nativeint read GEtCount;
    function GetGameById(iID: int64): TGame;
  end;

  TMasterGameList = class (TLockQueuedObject)
  private
    FFinished: TGameList;
    FPLaying: TGameList;
    FForming: TGameList;
    thr: TExternalEventThread;
    docache: TDataobjectCache;
    xref : TXRefPool;
    bnotfirst: boolean;
  public
    constructor create;override;
    destructor destroy;override;
    procedure OnThreadExecute(thr: TExternalEventThread);

    property Forming: TGameList read FForming;
    property Playing: TGameList read FPLaying;
    property Finished: TGameList read FFinished;


    function JoinBestGame(iSessionID: integer; out iGameID: int64): boolean;
    function NeedServerInterface: IServerInterface;
    function GetGameByID(iID: int64): TGame;
    function SessionToUserID(iSession: int64): int64;
    procedure UpdateGamelists;
  end;

procedure WriteTGameStreamToPacket(packet: TRDTPPacket; gs: TGameStream);
procedure GetTGameStreamFromPacket(packet: TRDTPPacket; out gs: TGameStream);


implementation
uses TowerGameAI;

{ TMasterGameList }

constructor TMasterGameList.create;
begin
  inherited;
  FForming := TGameList.create;
  FPlaying := TGameList.create;
  FFinished := TGameList.create;
  THR := TPM.Needthread<TExternalEventThread>(self);
  thr.Loop := true;
  thr.OnExecute := OnThreadExecute;
  xref := TXRefPool.create;
  DOCM.AllocateCache(doCache, -1, -1, -1, TDataObjectCacheBias.cbSmall);
  thr.Start;

end;

destructor TMasterGameList.destroy;
begin
  thr.Terminate;
  thr.WaitFor;
  thr := nil;
  DOCM.FreeCache(doCAche);

  fForming.free;
  FPLaying.free;
  FFinished.free;


  inherited;
end;

function TMasterGameList.GetGameByID(iID: int64): TGame;
begin

  result := Playing.GetGameById(iid);
  if result = nil then
    result := Forming.GetGameById(iid);

  if result = nil  then
    result := Finished.GetGameById(iid);




end;

function TMasterGameList.JoinBestGame(iSessionID: integer; out iGameID: int64): boolean;
var
  doSession, doUser: TDataObject;
  si: IServerInterface;
  ai: TTowerGamePlayerAI;
  bGood: boolean;
  tm: ticker;
begin
  si := NeedServerInterface;
  try
    result := false;


    if not  si.QueryMap(docache, doSession,
                        'Select * from session where sessionid='+inttostr(iSessionID),
                        iSessionID, 0, true, 'TdoSession', iSessionid, nil) then
      exit;


    doSession['userid'].AsVariant;
    bGood := false;
    tm := GetTicker;
    while not bGood do begin
      if forming.Count = 0 then
        sleep(random(4000)+1000);
      Forming.LockWrite;
      try
        UpdateGamelists;



        if forming.Count = 0 then begin
          if GetTimeSince(tm) > 20000 then begin
            exit;
          end else begin
            continue;
          end;
        end;
        bGood := true;

        if forming.games[0].GameState <> gsForming then
          exit;

        if forming.Games[0].PlayerCount = 0 then begin
            ai := TTowerGamePlayerAI.create;
            ai.Start;
        end;


        forming.Games[0].AddPlayer(iSessionID, doSession['userid'].AsVariant);


        iGameID := forming.Games[0].ID;
        result := true;
      finally
        Forming.Unlockwrite;
      end;
    end;//while




  finally
  end;



end;

function TMasterGameList.NeedServerInterface: IServerInterface;
begin
  result := DOSVPool.Servers[0].NewServer;
end;

procedure TMasterGameList.OnThreadExecute(thr: TExternalEventThread);
var
  g: TGame;
  id: int64;
begin
  thr.Name := 'Master Game List';
  //always make sure theres at least one game forming
  if not bNotFirst then begin
    sleep(6000);
    bNotFirst := true;
  end;

  if Forming.Count = 0 then begin
    g := TGame.Create;
    g.StatusMessage := 'Game is initializing';
    g.ID := NeedServerInterface().GetNextID(KEY_GAME_ID);
    g.StatusMessage := 'Game '+inttostr(g.ID)+' is forming.';

    Forming.AddGame(g);

  end;
  sleep(1);

end;

function TMasterGameList.SessionToUserID(iSession: int64): int64;
var
  si: IServerInterface;
  doSession: TDataObject;
begin
  si := NeedServerInterface;
  try
    if not  si.QueryMap(docache, doSession,
                        'Select * from session where sessionid='+inttostr(iSession),
                        iSession, 0, true, 'TdoSession', iSession, nil) then
      exit;


    result := doSession['userid'].AsVariant;
  finally
  end;

end;

procedure TMasterGameList.UpdateGamelists;
var
  t: integer;
  g: TGame;
begin
  LockWrite;
  try
    Forming.LockWrite;
    try
      for t:= Forming.Count-1 downto 0 do begin
        g := Forming.Games[t];
        if g.GameSTate = gsStarted then begin
          Forming.RemoveGame(g);
          Playing.AddGame(g);
        end;
        if g.GameSTate = gsFinished then begin
          Forming.RemoveGame(g);
          Finished.AddGame(g);
        end;

      end;

      for t:= Playing.Count-1 downto 0 do begin
        g := Playing.Games[t];
        if g.GameSTate = gsFinished then begin
          Playing.RemoveGame(g);
          Finished.AddGame(g);
        end;
      end;

      for t:= Finished.Count-1 downto 0 do begin
        g := Finished.Games[t];
        if g.IsStale then begin
          Finished.RemoveGame(g);
          //SayNatural('Game #'+inttostr(g.id)+' is stale and is being terminated.');
          GarbageCollect(g);
        end;
      end;


    finally
      Forming.UnlockWrite;
    end;


  finally
    UnlockWrite;
  end;
end;

{ TGame }

procedure TGame.AddEvent(e: TGameEvent);
var
  t: integer;
begin
  Lock;
  try
    FEvents.AddEvent(e);
    for t:= 0 to Playercount-1 do begin
      FEvents.AddEvent(e);
    end;
  finally
    Unlock;
  end;

end;

function TGame.AddPlayer(iSessionID, iUserID: int64): TPlayer;
begin
  result := TPlayer.Create;
  result.SessionID := iSessionid;
  result.UserID := iUserID;

  Lock;
  try
    RemovePlayer(iUserID);
    result.Game := self;
    FPlayers.add(result);
    FLastActivityTime := tickcount.GetTicker;
    result.InfuseIncomingEvents(FEvents, false);

//    SayNatural('Added a player, there are now '+inttostr(FPlayers.count)+' players in game #'+inttostr(self.FID));





  finally
    Unlock;
  end;
end;

constructor TGame.create;
begin
  inherited;
  FCreationTime := tickcount.GetTicker;
  FPlayers := TList<TPlayer>.create;
  FEvents := TGameStream.create;
//  SayNatural('Created a game');
end;

destructor TGame.destroy;
begin
  FPlayers.Free;
  FEvents.free;
  inherited;
end;

function TGame.FindUnassignedOpponent(pNot: TPlayer): TPlayer;
var
  t: integer;
begin
  Lock;
  try
    result := nil;
    for t:= 0 to Playercount-1 do begin
      if players[t] <> pNot then begin
        if (players[t].opponent = nil) or (players[t].Opponent.Defeated) then begin
            result := players[t];
            exit;
        end;
      end;
    end;
  finally
    Unlock;
  end;

end;

function TGame.GetIsStale: boolean;
var
  t: integer;
begin
  lock;
  try
    result := true;
    for t:= 0 to playercount-1 do begin
      if not players[t].laggedout then begin
        result := false;
        exit;
      end;
    end;

  finally
    unlock;
  end;
end;

function TGame.GetPlayer(idx: integer): TPLayer;
begin
  Lock;
  try
    result := FPlayers[idx];
  finally
    Unlock;
  end;
end;

function TGame.GetPlayerBySessionID(iSessionID: int64): TPlayer;
var
  t: integer;
begin
  result := nil;
  Lock;
  try
    for t:= 0 to PLayercount-1 do begin
      if players[t].SessionID = iSessionID then begin
        result := players[t];
        exit;
      end;

    end;
  finally
    Unlock;
  end;

end;

function TGame.GetPlayerCount: integer;
begin
  Lock;
  try
    result := FPlayers.Count;
  finally
    Unlock;
  end;

end;

function TGame.GEtPlayersRemaining: nativeint;
var
  t: integer;
begin
  result := 0;
  Lock;
  try
    for t:= 0 to playercount-1 do begin
      if not players[t].Defeated then
        inc(result);
    end;
  finally
    Unlock;
  end;
end;

function TGame.GetStatusMessage: string;
begin
  Lock;
  try
    result := FStatusMessage;
  finally
    Unlock;
  end;
end;

procedure TGame.InfuseEvent(e: TGameEvent; iFromSessionID: int64);
var
  t: integer;
begin
  lock;
  try

    for t:= 0 to playercount-1 do begin
      players[t].InfuseEvent(e, iFromSessionID=players[t].SessionID);
    end;
    FEvents.AddEvent(e);
    processevent(e);
  finally
    Unlock;
  end;

end;

function TGame.InfuseIncomingEvents(gs: TGameStream; p: TPlayer): integer;
var
  t: integer;
  pp: TPLayer;
begin
  if gs.count = 0 then exit;

  Lock;
  try
    FEvents.Merge(gs);


    p.Seen;
    for t:= 0 to Playercount-1 do begin
      pp := players[t];
      pp.InfuseIncomingEvents(gs, pp=p);
    end;
    result := p.IncomingEventCount;

    ProcessEvents(gs);

  finally
    Unlock;
  end;

end;

procedure TGame.StartGame;
var
  ge: TGameEvent;
begin
  ReassignOpponents();


  ge := TGameEvent.Create;
  ge.FuncID := gfGameStart;
  AddEvent_ServertoClient(ge);
  FGameState := gsStarted;
//  SayNatural('Starting Game');

end;

procedure TGame.AddEvent_ServerToClient(e: TGameEvent);
begin
  InfuseEvent(e, -1);
end;

procedure TGame.StartGameIfAllReady();
begin
  if AllPlayersREady then
    StartGame;
end;

function TGame.AllPlayersREady: boolean;
var
  t: integer;
begin
  Lock;
  try
    result := PLayerCount > 1;
    if not result then
      exit;

    for t:= 0 to PLayercount-1 do begin
      if not players[t].Ready then begin
        result := false;
        break;
      end;
    end;
  finally
    Unlock;
  end;

end;

procedure TGame.DefeatPlayer(p: TPlayer);
var
  e: TGameEvent;
begin
  e := TGameEvent.create;
  e.SessionID := -1;
  e.FuncID := gfDefeat;
  e.StringData := inttostr(p.SessionID);
  self.InfuseEvent(e, -1);
  p.Defeated := true;
end;

function TGame.GetundefeatedPLayer: TPlayer;
var
  t: Integer;
begin
  lock;
  try
    for t:= 0 to playercount-1 do begin
      if not players[t].Defeated then begin
        result := players[t];
        break;
      end;
    end;
  finally
    Unlock;
  end;
end;


procedure TGame.Victory;
var
  p: TPlayer;
  e: TGameEvent;
begin
  p := GetundefeatedPLayer;
  if p <> nil then begin
    e := TGameEvent.Create;
    e.SessionID := -1;
    e.funcid := gfVictory;
    e.StringData := inttostr(p.sessionid);
    InfuseEvent(e,-1);
  end;


  FGameState := gsFinished;



end;

procedure TGame.CheckGameConditions;
var
  t: integer;
begin
  Lock;
  try
    if GameState = gsStarted then begin
      for t:= 0 to playercount-1 do begin
//        players[t].Seen;
        if players[t].LaggedOut then begin
          DefeatPlayer(players[t]);
        end;
      end;

      if PlayersRemaining < 2 then begin
        Victory;
      end;
    end;



  finally
    Unlock;
  end;

end;

procedure TGame.ProcessEvent(e: TGameEvent);
begin
  case e.funcid of
    gfReady: StartGameIfAllReady();
  end;
end;

procedure TGame.ProcessEvents(gs: TGameStream);
var
  t: integer;
begin
  for t:= 0 to gs.Count-1 do begin
    processevent(gs[t]);
  end;

  CheckGameConditions;


end;

function TGame.ReassignOpponent: boolean;
var
  p1, p2: TPlayer;
  ge: TGameEvent;
begin
  Lock;
  try
    result := false;
    p1 := FindUnassignedOpponent(nil);
    if p1 <> nil then begin
      p2 := FindUnassignedOpponent(p1);
      if p2 <> nil then begin
        result := true;
        ge := TGameEvent.Create;
        ge.FuncID := gfAssignOpponents;
        ge.SessionID := p1.SessionID;
//        ge.NetObjectID := NETID_GAMEBOARD;
        ge.StringData := inttostr(p2.SessionID);
        p1.Opponent := p2;
        AddEvent_ServerToClient(ge);

        ge := TGameEvent.Create;
        ge.FuncID := gfAssignOpponents;
        ge.SessionID := p2.SessionID;
//        ge.NetObjectID := NETID_GAMEBOARD;
        ge.StringData := inttostr(p1.SessionID);
        p2.Opponent := p1;
        AddEvent_ServerToClient(ge);




      end;
    end;

  finally
    Unlock;
  end;
end;

procedure TGame.REassignOpponents;
begin
  lock;
  try
    while ReassignOpponent do ;
  finally
    Unlock;
  end;
end;

procedure TGame.RemovePlayer(iUserID: int64);
var
  t: integer;
begin
  Lock;
  try
    for t:= PlayerCount-1 downto 0 do begin
      if Players[t].UserID = iUserID then begin
        FPlayers[t].free;
        FPlayers.Delete(t);
      end;
    end;
  finally
    Unlock;
  end;

end;

procedure TGame.SetStatusMessage(const Value: string);
begin
  Lock;
  try
    FstatusMessage := value;
  finally
    Unlock;
  end;

end;

{ TGameList }

procedure TGameList.AddGame(g: TGame);
begin
  LockWrite;
  try
    FList.Add(g);
  finally
    UnlockWrite;
  end;
end;

procedure TGameList.RemoveGame(g: TGame);
begin
  LockWrite;
  try
    FList.Remove(g);
  finally
    UnlockWrite;
  end;
end;


constructor TGameList.create;
begin
  inherited;
  FList := TList<TGame>.create;
end;

destructor TGameList.destroy;
begin
  FList.free;
  inherited;
end;

function TGameList.GEtCount: nativeint;
begin
  LockRead;
  try
    result := FList.count;
  finally
    UnlockRead;
  end;
end;

function TGameList.GetGame(idx: integer): TGame;
begin
  LockRead;
  try
    result := FList[idx];
  finally
    UnlockREad;
  end;

end;

function TGameList.GetGameById(iID: int64): TGame;
var
  i: nativeint;
begin
  result := nil;
  LockRead;
  try
    i := IndexOf(iID);
    if i>=0 then
      result := Games[i];
  finally
    UnlockRead;
  end;
end;

function TGameList.HasGame(g: TGame): boolean;
begin
  result := IndexOf(g) >=0;
end;

function TGameList.HasGame(iGameID: int64): boolean;
begin
  result := IndexOf(iGameID) >=0;
end;

function TGameList.IndexOf(iGameID: int64): nativeint;
var
  t: integer;
begin
  result := -1;
  LockRead;
  try
    for t:= 0 to FList.count-1 do begin
      if FList[t].ID = iGameID then begin
        result := t;
        exit;
      end;
    end;
  finally
    UnlockRead;
  end;
end;


function TGameList.IndexOf(g: TGame): nativeint;
begin
  result := -1;
  LockRead;
  try
    result := Flist.IndexOf(g);
  finally
    UnlockRead;
  end;
end;

{ TPlayer }

constructor TPlayer.create;
begin
  inherited;
  FInEvents := TGameStream.create;
  FOutEvents := TGameStream.create;
  Seen;
end;

destructor TPlayer.destroy;
begin
  FInEvents.free;
  FOutEvents.free;

  inherited;
end;

function TPlayer.GetAvailableEvents(STartingAt: int64): TGameStream;
begin
  result := TGameStream.Create;
  GetAvailableEvents(result, StartingAT);
end;

procedure TPlayer.GetAvailableEvents(gs: TGameStream; STartingAt: int64);
var
  t: integer;
begin
  Lock;
  try
    for t:= StartingAT to FOutEvents.count-1 do begin
      gs.AddEvent(FOutEvents[t].Copy);
    end;
  finally
    Unlock;
  end;

end;

function TPlayer.GetIncomingEventCount: nativeint;
begin
  Lock;
  try
    Result := FInEvents.Count;
  finally
    Unlock;
  end;
end;

function TPlayer.GetLaggedOut: boolean;
var
  tm: TMSTime;
begin
  Debug.Log('Lag Out Check Last Seen = '+inttostr(FLastSeen));
  tm := GetTIMESince(LastSeen);
  Debug.Log('Lag Out Check Tickcount = '+inttostr(tickcount.GetTicker));
  Debug.Log('Lag Out Check Time Since Last Seen = '+inttostr(tm));

  result :=  tm > LAG_OUT_TIME;
//  if result then begin
//    SayNatural('Session '+inttostr(sessionid)+' lagged out at '+inttostr(tm)+' milleseconds.');
//  end;
end;

function TPlayer.GetTimeOFfset: double;
begin
  result := LocalJoinTime - game.CreationTime;
end;

function TPlayer.GEtTotalEVentsReceived: int64;
begin
  result := FINEvents.Count;
end;

procedure TPlayer.InfuseEvent(e: TGameEvent; bFromThisPlayer: boolean);
begin
  Lock;
  try
    if bFromThisPlayer then
      FInEvents.addevent(e);
    FOUtEvents.AddEvent(e);

    processevent(e);
  finally
    Unlock;
  end;
end;

procedure TPlayer.InfuseIncomingEvents(gs: TGameStream; bFromThisPlayer: boolean);
begin
  Lock;
  try
    if bFromThisPlayer then begin
      FInEvents.Merge(gs);
      Seen;
    end;
    FOutEvents.Merge(gs);
    ProcessEvents();
  finally
    Unlock;
  end;
end;

function TPlayer.ProcessEvent(e: TGameEvent): boolean;
begin
  result := true;
  if e.SessionID = self.SessionID then begin
    case e.FuncID of
      gfReady: begin
        Ready := true;
//        SayNatural('Session #'+inttostr(self.sessionid)+' is ready.');
      end;
      gfNotReady: begin
        Ready := false;
//        SayNatural('Session #'+inttostr(self.sessionid)+' is not ready.');
      end;
    end;
  end;
end;

procedure TPlayer.ProcessEvents();
var
  t: integer;
begin
  while FEventIndex < FOutEvents.count do begin
    if processevent(FOutEvents[FEventIndex]) then begin
      inc(FEventIndex);
    end;

  end;

end;

procedure TPlayer.Seen;
var
  t: ticker;
begin
  t := tickcount.GetTicker;
  FLastSeen := tickcount.GetTicker;
end;

procedure WriteTGameStreamToPacket(packet: TRDTPPacket; gs: TGameStream);
begin
  gs.WriteToPacket(packet);
  gs.Free;

end;

procedure GetTGameStreamFromPacket(packet: TRDTPPacket; out gs: TGameStream);
begin
  gs := TGameStream.Create;
  gs.ReadFromPacket(packet);
end;



{ TGameStream }

procedure TGameStream.AddEvent(ge: TGameEvent);
begin
  Lock;
  try
    FEvents.add(ge);
  finally
    Unlock;
  end;
end;

procedure TGameStream.ClearEvents;
begin
  Lock;
  try
    while FEvents.Count > 0 do begin
      FEvents[FEvents.Count-1].Free;
      FEvents.Delete(FEvents.Count-1);
    end;
  finally
    Unlock;
  end;
end;


function TGameStream.Copy(iStartingAt: nativeint): TGameStream;
var
  t: integer;
begin
  result := TGameStream.create;
  Lock;
  try
    for t:= iStartingAt to Fevents.count-1 do begin
      result.AddEvent(FEvents[t].copy);
    end;
  finally
    Unlock;
  end;

end;

function TGameStream.Copy: TGameStream;
var
  t: integer;
begin
  result := TGameStream.create;
  Lock;
  try
    for t:= 0 to Fevents.count-1 do begin
      result.AddEvent(FEvents[t].copy);
    end;
  finally
    Unlock;
  end;

end;

procedure TGameStream.CopyTo(gs: TGameStream);
var
  t: integer;
begin
  self.Lock;
  try
    gs.Lock;
    try
      for t:= 0 to FEvents.Count-1 do begin
        gs.AddEvent(FEvents[t].Copy);
      end;
    finally
      gs.Unlock;
    end;
  finally
    self.Unlock;
  end;
end;

constructor TGameStream.Create;
begin
  inherited;
  FEvents := TList<TGameEvent>.create;
end;

destructor TGameStream.Destroy;
begin
  ClearEvents;
  FEvents.Free;
  inherited;
end;

function TGameStream.GetCount: nativeint;
begin
  result := FEvents.Count;
end;

function TGameStream.GetEvent(idx: integer): TGameEvent;
begin
  Lock;
  try
    result := FEvents[idx];
  finally
    Unlock;
  end;
end;

procedure TGameStream.Merge(gs: TGameStream);
begin
  Lock;
  try
    gs.CopyTo(self);
  finally
    Unlock;
  end;
end;

procedure TGameStream.ReadFromPacket(p: TRDTPPacket);
var
  t,c: integer;
  e: TGameEvent;
begin
  Lock;
  try
    c := p.SeqRead;
    SyncEventCount(c);
    for t:= 0 to c-1 do begin
      FEvents[t].ReadFromPacket(p);
    end;
  finally
    Unlock;
  end;
end;

procedure TGameStream.SyncEventCount(count: integer);
begin
  Lock;
  try
    while FEvents.Count < count do begin
      FEvents.Add(TGameEVent.Create);
    end;

    while FEvents.count > count do begin
      FEvents[FEvents.Count-1].Free;
      FEvents.Delete(FEvents.Count-1);

    end;
  finally
    Unlock;
  end;
end;

procedure TGameStream.WriteToPacket(p: TRDTPPacket);
var
  t: integer;
begin
  Lock;
  try
    p.AddInt(FEvents.Count);
    for t:= 0 to FEvents.count-1 do begin
      FEvents[t].WriteToPacket(p);
    end;
  finally
    Unlock;
  end;
end;

{ TGameEvent }

function TGameEvent.Copy: TGameEvent;
begin
  result := TGameEvent.create;
  result.GameTick := GameTick;
  result.MessageType := MessageType;
  result.FuncID := FuncID;
  result.SessionID := SessionID;
  result.StringData := StringData;
  result.Singularity := Singularity;
  result.NetObjectID := NetobjectID;
  Result.x1 := x1;
  Result.x := x;
  Result.y1 := y1;
  Result.y := y;
  Result.z1 := z1;
  Result.z := z;
  Result.w1 := w1;
  Result.w := w;




end;



procedure TGameEvent.ReadFromPacket(p: TRDTPPacket);
begin
  Gametick := p.SeqRead;
  MessageType := p.SeqRead;
  FuncID := TGameFunction(p.SeqRead);
  SessionID := p.SeqRead;
  NetObjectID := p.SeqRead;
  StringData := p.SeqRead;
  Singularity := p.SeqRead;


  case (MessageType) of
    MT_VECTOR3:
    begin
      x := p.SeqRead;
      y := p.SeqRead;
      z := p.SeqRead;
    end;
    MT_VECTOR4:
    begin
      x := p.SeqRead;
      y := p.SeqRead;
      z := p.SeqRead;
      w := p.SeqRead;
    end;
    MT_DUAL_VECTOR3:
    begin
      x := p.SeqRead;
      y := p.SeqRead;
      z := p.SeqRead;
      x1 := p.SeqRead;
      y1 := p.SeqRead;
      z1 := p.SeqRead;
    end;
    MT_DUAL_VECTOR4:
    begin
      x := p.SeqRead;
      y := p.SeqRead;
      z := p.SeqRead;
      w := p.SeqRead;
      x1 := p.SeqRead;
      y1 := p.SeqRead;
      z1 := p.SeqRead;
      w1 := p.SeqRead;
    end;
  end;

end;

procedure TGameEvent.WriteToPacket(p: TRDTPPacket);
begin
  p.AddDouble(Gametick);
  p.AddInt(MessageType);
  p.AddInt(ord(FuncID));
  p.AddInt(SessionID);
  p.AddInt(NetObjectID);
  p.AddString(StringData);
  p.AddBoolean(Singularity);


  case (MessageType) of
    MT_VECTOR3:
    begin
      p.AddDouble(x);
      p.AddDouble(y);
      p.AddDouble(z);
    end;
    MT_VECTOR4:
    begin
      p.AddDouble(x);
      p.AddDouble(y);
      p.AddDouble(z);
      p.AddDouble(w);
    end;
    MT_DUAL_VECTOR3:
    begin
      p.AddDouble(x);
      p.AddDouble(y);
      p.AddDouble(z);
      p.AddDouble(x1);
      p.AddDouble(y1);
      p.AddDouble(z1);
    end;
    MT_DUAL_VECTOR4:
    begin
      p.AddDouble(x);
      p.AddDouble(y);
      p.AddDouble(z);
      p.AddDouble(w);
      p.AddDouble(x1);
      p.AddDouble(y1);
      p.AddDouble(z1);
      p.AddDouble(w1);
    end;
  end;

end;

procedure oinit;
begin
//  raise Exception.create('unimplemented');

//  GL := TMasterGameList.Create;
end;

procedure ofinal;
begin
//  raise Exception.create('unimplemented');
//  GL.free;


end;

initialization


  orderlyinit.init.RegisterProcs('Gamelist', oinit, ofinal, 'DataObjectServices,ServerInterfaceFactory,DataObjectCacheManager');



finalization


end.
