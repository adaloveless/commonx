unit MothershipWebserver;
//Contains the TMotherShipWebServer class and related classes.

interface
uses
  WebConfig, WebStats, DataObjectServices, sysutils, windows, REquestManager, classes, betterobject, sharedobject, webdispatch, RequestInfo, backgroundthreads, versioninfo, systemx, simplewinsock, stringx, stringx.ansi, helplookup, dir, dirfile;

type
  Tmothershipwebserver = class;//forward
  TMothershipStartupRoutine = procedure (sender: TMothershipWebServer);
  TMothershipShutdownRoutine = procedure (sender: TMothershipWebServer);

  TMothershipWebServerState = (
    wssInit, //The Web Server is initializing resources.
    wssReady, //The Web Server is Ready for a command.
    wssStarting, //The web server is starting.
    wssRunning, //The Web server is running, Listening.
    wssStopping, //The Web Server is in the process of stopping.
    wssStopped, //The Web Server is stopped, not listening.
    wssDestroy //The Web Server object is being destroyed.
  );

  TMothershipWebServerOptions = set of (pwsNoDOS, pwsNoHelp, pwsNoINI, pwsNoPlugins);
  TMothershipWebServer = class (TSharedObject)
    //TMothershipWebServer represents the abstracted web server in PWLN.  It is essentially
    //the ENGINE, encapsulating all the parts required to dispatch and produce web pages.
    //It is important to note that it DOES NOT:
    //<li>Establish a listening socket</li>

    //<li>Parse HTTP Request Headers</li>
    //<li>Build HTTP Response Headers (directly)</li>
    //The key word here is "ABSTRACTED".  Requests come to TMothershipWebServer after
    //the outer abstraction layer converts them to an abstract format that TMothershipWebServer can work with.
    //<H2>PWLN Functional Layering</H2>
    //<TABLE border="1">
    //<TR><TD colspan="1">Web Server (IIS)</TD><TD colspan="1">Web Server (native listener/other)</TD><TR>
    //<TR><TD colspan="1">Web Server Access Later (IIS)</TD><TD colspan="1">Web Server Access Layer(other)</TD><TR>
    //<TR><TD colspan="1">Web Server Adapter [TPWLN]</TD><TD colspan="1">WebS erver Adapter [TWebThread]</TD><TR>
    //<TR><TD colspan="2">Web Server Dispatcher</TD><TR>
    //<TR><TD colspan="2">Web Request (Individual dispatched page)</TD><TR>
    //<TR><TD colspan="2">Middle Tier to Data-Tier Interface [MTDTInterface]</TD><TR>
    //<TR><TD colspan="2">Data Object Services/Caching</TD><TR>
    //<TR><TD colspan="2">Server Interface Layer</TD><TR>
    //<TR><TD colspan="2">Network Language Layer [TPacket]</TD><TR>
    //<TR><TD colspan="2">Transport Layer [TTransport]</TD><TR>
    //<TR><TD colspan="2">Communication Layer [TSimpleAbstractConnection]</TD><TR>
    //</TABLE>


  private
    FHits, FHitAccepts: int64;
    FState: TMothershipWebServerState;
    FStopListening, FStartListening: TNotifyEvent;
    FHelp: THelpLookup;
    Foptions: TMothershipWebServerOptions;
    FStartupRoutines: TList;
    FShutdownRoutines: Tlist;
    function GetState: TMothershipWebServerState;
    procedure SetState(const Value: TMothershipWebServerState);
    function GetStateString: ansistring;
    function GetHitCount: int64;
    function GEtShutdownRoutine(idx: integer): TMothershipShutdownRoutine;
    function GetStartupRoutine(idx: integer): TMothershipStartupRoutine;
    function GetHitAcceptCount: int64;
  public
    constructor create;override;
    procedure Configure(sPublicKeyName: ansistring; options: TMothershipWebServerOptions);reintroduce;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function DispatchWebRequest(rqInfo: TRequestInfo): boolean;
    property State: TMothershipWebServerState read GetState write SetState;
      //Reports the state of the server.  Running, Stopped, etc.  See TMothershipWebServerState.
    property StateString: ansistring read GetStateString;
      //Returns a displayable ansistring representation of the server's state.  May also return diagnostic information about error states.
    property OnStartListening: TNotifyEvent read FStartListening write FStartListening;
      //Hookable event property.  Implement a hook to be notified when the server starts listening.
    property OnStopListening: TNotifyEvent read FStopListening write FStopListening;
      //Hookable event property.  Implement a hook to be notified when the server stops listening.
      //Points to TPluginVersions object for getting information about the download of plugins from this server.
      //Points to the background thread object for timing out user sessions.
    property HitCount: int64 read GetHitCount;
    procedure Hit(rqInfo: TrequestInfo);
    procedure HitAccept;
    property HitAcceptCount: int64 read GetHitAcceptCount;
    procedure ClearHits;
    property Help: THelpLookup read FHelp;
    property StartupRoutines[idx: integer]: TMothershipStartupRoutine read GetStartupRoutine;
    property ShutdownRoutines[idx: integer]: TMothershipShutdownRoutine read GEtShutdownRoutine;
    procedure AddStartupRoutine(proc: TMothershipStartupRoutine);
    procedure AddShutdownRoutine(proc: TMothershipShutdownRoutine);
    function StartupRoutineCount: integer;
    function ShutdownRoutineCount: integer;
    procedure RunStartupRoutines;
    procedure RunShutdownRoutines;

  end;


var
  WebServer : TMothershipWebServer;
implementation


{ TMothershipWebServer }
//------------------------------------------------------------------------------
procedure TMothershipWebServer.AddShutdownRoutine(
  proc: TMothershipShutdownRoutine);
var
  p: pointer;
begin
  p := @proc;
  FShutdownRoutines.Add(p);
end;

procedure TMothershipWebServer.AddStartupRoutine(proc: TMothershipStartupRoutine);
var
  p: pointer;
begin
  p := @proc;
  FStartupRoutines.add(p);
end;

procedure TMothershipWebServer.ClearHits;
begin
  lock;
  try
    FHits:= 0;
    FHitAccepts := 0;
  finally
    unlock;
  end;

end;

procedure TMothershipWebServer.Configure(sPublicKeyName: ansistring;
  options: TMothershipWebServerOptions);
//a: Jason Nelson
//Constructor: Creates aggregate objects and initializes internal state.
var
  t: integer;
  dest: TNetConnection;
begin
  inherited create;

  FOptions := options;
  //create the configuration handler
  WebServerconfig.free;
  WebServerconfig := nil;
  WebServerConfig := TWebConfig.create(sPublicKeyName);

  if not (pwsNoHelp in options) then try

    Help.FileName := slash(WebServerConfig.ExternalResourcePath)+'helpfiles.txt';
  except
  end;

  DOSVpool.clear;
  //create all the data-tier communications handlers
  if not (pwsNoDOS in options) then
  for t:= 0 to WebServerconfig.ConnectionCount-1 do begin
    dest := WebServerConfig.DestByIndex[t];
    if lowercase(copy(dest.name, 1, 8)) = 'datatier' then begin
      DOSVPool.Add(dest.name, dest.host, dest.endpoint, dest.Context, WebServerConfig.DataCenterID);
    end;
  end;

  //update server state
  state := wssReady;

end;

constructor TMothershipWebServer.create;
//a: Jason Nelson
//Constructor: Creates aggregate objects and initializes internal state.
var
  t: integer;
  dest: TNetConnection;
begin
  inherited create;
  FHelp := THelpLookup.create;
  FHits:=0;
  FState := wssInit;
  FStartListening := nil;
  FStopListening := nil;

  FStartupRoutines := TList.create;
  FShutdownRoutines := TList.create;
  WebServerStats := TWebStats.create;

  //update server state
  state := wssReady;

end;
//------------------------------------------------------------------------------
destructor TMothershipWebServer.Destroy;
//a: Jason Nelson
//Shutsdown the server if not already shut down, then frees aggregate resoureces.
begin
  if state = wssRunning then
    stop;

  //set state
  state := wssDestroy;

//  DOSV.free;



  WebServerStats.free;
  WebServerConfig.free;

  rqMan.Shutdown;
  {$IFDEF BEEP}
//  beeper.beep(100,100);
  {$ENDIF}
  Fhelp.free;

  FStartupRoutines.free;
  FShutdownRoutines.free;
  inherited;
end;
//------------------------------------------------------------------------------
function TMothershipWebServer.DispatchWebRequest(rqInfo: TRequestInfo): boolean;
//a: Jason Nelson
//This is the key function in the object.  Pass this function a TRequestInfo class and it will
//be sent thorugh the entire dispatching process, including database, template conversionm etc.
//Typically you want to have the TRequestInfo.Request property pre-filled out BEFORE you call this function.  The long dispatching process will then
//respond by filling out the TRequestInfo.Response portion.
//<BR>
//It is rare that a typical PWLN programmer will have to call this function.  The only
//situations where this would be necessary would be if the programmer were writing an interface
//to a new WebServer.  For example, if we decided to run as an ISAPI filter instead of
//an ISAPI servlet, or decided to run on Apache, or as a stand-alone service process (been done).
//In the layer ABOVE this call, the program would be required to interface with the 3rd Party
//web server and extract the HTTP request Information in whatever way available, fill out the
//TRequestInfo.Request property and THEN pass the entire TRequestInfo object to THIS function.
//Once this function returns, the TRequestInfo.Response property is read and the information is pushed back out to the client.
begin
  {$IFDEF Serialize}
  //Lock;
  {$ENDIF}
  try
    result := WebDispatch.DispatchWebRequest(rqInfo);
  finally
    {$IFDEF Serialize}
    //UnLock;
    {$ENDIF}
  end;

  if rqINfo.request.hasparam('User-Agent') and
    (lowercase(rqInfo.request['User-Agent']) = 'monitor') then begin
  end
  else
    self.Hit(rqInfo);

end;

//------------------------------------------------------------------------------
function TMothershipWebServer.GetHitAcceptCount: int64;
begin
  Lock;
  try
    result := FHitAccepts;
  finally
    Unlock;
  end;

end;

function TMothershipWebServer.GetHitCount: int64;
begin
  Lock;
  try
    result := FHits;
  finally
    Unlock;
  end;

end;

function TMothershipWebServer.GEtShutdownRoutine(idx: integer): TMothershipShutdownRoutine;
begin
  result := TMothershipShutdownRoutine(FStartupRoutines[idx]);
end;

function TMothershipWebServer.GetStartupRoutine(idx: integer): TMothershipStartupRoutine;
begin
  result := TMothershipStartupRoutine(FStartupRoutines[idx]);
end;

function TMothershipWebServer.GetState: TMothershipWebServerState;
//a: Jason Nelson
//Getter for the State property.
begin
  Lock;
  try
    result := FState;
  finally
    Unlock;
  end;
end;
//------------------------------------------------------------------------------
function TMothershipWebServer.GetStateString: ansistring;
//a: Jason Nelson
//Getter for the StateString Property.
begin
  Lock;
  try
    Case FState of
      wssInit: result := MakeThreadSafe('Initializing');
      wssReady: result := MakeThreadSafe('Ready');
      wssStarting: result := MakeThreadSafe('Starting');
      wssRunning: result := MakeThreadSafe('Running');
      wssStopping: result := MakeThreadSafe('Stopping');
      wssStopped: result := MakeThreadSafe('Stopped');
      wssDestroy: result := MakeThreadSafe('Destroying');
    end;
  finally
    UnLock;
  end;
end;
//------------------------------------------------------------------------------
procedure TMothershipWebServer.Hit(rqInfo: TrequestInfo);
begin
  Lock;
  try
    if not rqInfo.NoHit then
      inc(FHits);
    if FHits>2000000000 then
      FHits := 0;
  finally
    Unlock;
  end;
end;

procedure TMothershipWebServer.HitAccept;
begin
  Lock;
  try
    inc(FHitAccepts);
  finally
    Unlock;
  end;

end;

procedure TMothershipWebServer.RunShutdownRoutines;
var
  t: integer;
begin
  for t:= 0 to self.ShutdownRoutineCount-1 do begin
    shutdownroutines[t](self);
  end;
end;

procedure TMothershipWebServer.RunStartupRoutines;
var
  t: integer;
begin
  for t:= 0 to self.StartupRoutineCount-1 do begin
    startuproutines[t](self);
  end;

end;
  
procedure TMothershipWebServer.SetState(const Value: TMothershipWebServerState);
//a: Jason Nelson
//Setter for the State property.
begin
  Lock;
  try
    FState := value;
  finally
    Unlock;
  end;

end;
function TMothershipWebServer.ShutdownRoutineCount: integer;
begin
  result := FShutdownroutines.count;
end;

//------------------------------------------------------------------------------
procedure TMothershipWebServer.Start;
//a: Jason Nelson
//Starts the WebServer.  Enables dormant background threads, and enables listening if it applies to the access layer.
var
  dir: Tdirectory;
  t: integer;
begin
  if state = wssRunning then
    exit;

//  AuditLog('The Web Service is starting');

  dir := nil;
  try
    dir := TDirectory.create(slash(WebServerconfig.ExternalResourcePath)+'firmware\', '*.longhex', 0,0,true);

    //ReviveSockets;

    //set state
    state := wssStarting;
    if Assigned(FStartListening) then
      FStartListening(self);

    //set state
    state := wssRunning;

    rqMan.Start;


    try
      for t:= 0 to dir.filecount-1 do begin
        try
          DeleteFile(PChar(dir.files[t].FullName));
          {$IFDEF BEEP}
//          beeper.Beep(100,100);
          {$ENDIF}
        except
        end;
      end;
    finally
      dir.free;
    end;
  except
  end;

  RunStartupRoutines;
  {$IFDEF BEEP}
//  beeper.beep(800,50);
  {$ENDIF}
//  AuditLog('The Web Service is started');


end;
function TMothershipWebServer.StartupRoutineCount: integer;
begin
  result := FStartupRoutines.count;
end;

//------------------------------------------------------------------------------
procedure TMothershipWebServer.Stop;
//a: Jason Nelson
//Stops the WebServer.  Disables background threads, and disables listening if it applies to the access layer.
begin
//  SayNatural('The Web Service is stopping.');
//  AuditLog('The Web Service is stopping');


  {$IFDEF BEEP}
//  beeper.beep(800,50);
  {$ENDIF}

//  KillSockets;
  RunShutdownRoutines;

  //set state
  state := wssStopping;

  if Assigned(FStopListening) then
    FStopListening(self);

  backgroundthreads.BackgroundThreadMan.TerminateAllThreads;
  backgroundthreads.BackgroundThreadMan.WaitForThreads;

  rqMan.shutdown;

  

  //set state
  state := wssStopped;

//  SayNatural('Stopped successfully.');
//  AuditLog('Stopped successfully.');
end;

initialization
  webserver := TMothershipWebServer.create;

end.
