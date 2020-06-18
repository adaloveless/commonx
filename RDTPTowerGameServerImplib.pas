unit RDTPTowerGameServerImplib;
{GEN}
{TYPE IMPLIB}
{RQFILE RDTPTowerGameRQs.txt}
{END}
interface

uses debug, gamestream, RDTPTowerGameServer, RDTPKeybotClient, windows, ServerInterfaceInterface, DataobjectCache,xref, systemobjects, dataobject, mastergamelist,
  Dataobjectservices, dataobjectcachemanager, ServerInterfaceFactory, serverinterface, ExceptionsX, gamelist, sysutils, commands_system, systemx, stringx;

const
  MW_HOST = 'localhost';
  MW_PORT = '919';

type
  TTowerGameServer = class(TTowerGameServerBase)
  private
    FMWConnection: IServerInterface;
    cache: TDataObjectCache;
  protected
  public
  constructor Create;override;
  destructor Destroy;override;
{INTERFACE_START}
    function RQ_Login(UserName:string; Password:string):int64;overload;override;
    procedure RQ_Logout(SessionID:int64);overload;override;
    function RQ_JoinBestGame(SessionID:int64; out iGameID:int64):boolean;overload;override;
    function RQ_GetGameStatus(iGameID:int64; out status:string; out PLayerCount:integer):boolean;overload;override;
    function RQ_GetUserIDForSession(SessionID:int64):int64;overload;override;
    function RQ_CrossStreams(iGameID:int64; iSessionID:int64; InStream:TGameStream; TotalEventsPreviouslyReceived:int64; out OutStream:TGameStream; out TotalEventsReceivedFromClient:int64):boolean;overload;override;
    function RQ_ShareClientDetails(iGameID:int64; iSessionID:int64; LocalGameTime:double):boolean;overload;override;
    function RQ_GetSimplePlayerList(iGameID:int64):string;overload;override;
    function RQ_StorePerformanceMetrics(sUserName:string; sData:string):string;overload;override;
    function RQ_TransportLayerTest(sSomethingToConvertToLowercase:string):string;overload;override;

{INTERFACE_END}
  end;
implementation
{ TTowerGameServer }

uses rdtpserverlist;

constructor TTowerGameServer.Create;
begin
  inherited;
  FMWConnection := DOSVPool.Servers[0].NewServer;

  DOCM.AllocateCAche(cache, 0, 0, 0, cbSmall);
end;

destructor TTowerGameServer.Destroy;
begin
  FMWConnection := nil;
  DOCM.FreeCache(cache);
  inherited;
end;



function TTowerGameServer.RQ_CrossStreams(iGameID:int64; iSessionID:int64; InStream:TGameStream; TotalEventsPreviouslyReceived:int64; out OutStream:TGameStream; out TotalEventsReceivedFromClient:int64):boolean;
var
  g: TGame;
  p: TPlayer;
begin
  OutStream := TGameStream.Create;

  g := GL.GetGameByID(iGameID);
  result := g <> nil;
  if not result then exit;

  if result then begin

    p := g.GetPlayerBySessionID(iSessionID);

    result := p <> nil;
    if not result then exit;

    p.seen;

    g.InfuseIncomingEvents(InStream,p);
    INStream.Free;
    //misccommands.GarbageCollect(InStream);

    OutStream := p.GetAvailableEvents(TotalEventsPreviouslyReceived);


    TotalEventsReceivedFromClient  := p.TotalEventsReceived;
//    OutputDebugString(pchar(inttostr(InStream.count)+'/'+inttostr(TotalEventsPreviouslyReceived)+'/'+inttostr(OutStream.count)+'/'+inttostr(TotalEventsReceivedFromClient)));
  end;



end;

function TTowerGameServer.RQ_GetGameStatus(iGameID: int64; out status:string; out PLayerCount:integer): boolean;
var
  g: TGame;
begin
  g := GL.GetGameByID(iGameID);
  result := g <> nil;
  if result then begin
    status := g.StatusMessage;
    PlayerCount := g.PlayerCount;
  end else begin
    status := 'game not found';
    PLayerCount := 0;
  end;


end;

function TTowerGameServer.RQ_GetSimplePlayerList(iGameID: int64): string;
var
  g: TGame;
  p: TPLayer;
  t: integer;
begin
  g := GL.GetGameByID(iGameID);
  if g = nil then begin
    Debug.Log(inttostr(iGameID)+' was not FOUND!', 'error');
    result := '';
    exit;
  end;
  g.Lock;
  try
    result := '';
    for t:= 0 to g.PlayerCount-1 do begin
      result := result + inttostr(t)+'. User#'+inttostr(g.Players[t].UserID);

    end;

  finally
    g.Unlock;
  end;


end;

function TTowerGameServer.RQ_GetUserIDForSession(SessionID: int64): int64;
begin
  result := gl.SessionToUserID(SessionID);


end;

function TTowerGameServer.RQ_JoinBestGame(SessionID: int64; out iGameID:int64): boolean;
begin
  result := GL.JoinBestGame(SessionID, iGameID);

end;

function TTowerGameServer.RQ_Login(UserName, Password: string): int64;
var
  doSession: TDataObject;
begin
//  SayNatural('Hello');
  if not FMWconnection.Login(cache, doSession, UserName, '', Password, 0, 0, '', false) then begin
    raise ENewException.create(0,FMWconnection.GetLastErrorMessage(),FMWconnection.GetLastErrorMessage());
  end;

  result := doSession.token.params[0];

end;

procedure TTowerGameServer.RQ_Logout(SessionID: int64);
begin
  inherited;

end;


function TTowerGameServer.RQ_ShareClientDetails(iGameID, iSessionID: int64;
  LocalGameTime: double): boolean;
var
  g: TGame;
  p: TPLayer;
begin
  g := GL.GetGameByID(iGameID);
  g.GetPlayerBySessionID(iSessionID);
  p.LocalJoinTime := LocalGameTime;


end;


function TTowerGameServer.RQ_StorePerformanceMetrics(sUserName,
  sData: string): string;
var
  sFile: string;
  sPath: string;
begin
  try
    sPath := DLLPath+'metrics\';
    sFile := sPath+sUserName+'.txt';
    ForceDirectories(sPath);
    SaveStringAsFile(sFile, sData);
  except

  end;
end;

function TTowerGameServer.RQ_TransportLayerTest(
  sSomethingToConvertToLowercase: string): string;
begin
  result := lowercase(sSomethingToConvertToLowercase);
end;

initialization

RDTPServers.RegisterRDTPProcessor('towergame', TTowerGameServer);
DOSVPool.Add('Main', 'localhost', '101', 'towers', 0);
SIF.Product := TServerInterface;


end.
