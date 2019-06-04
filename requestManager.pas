unit RequestManager;

interface

uses classes, windows, requestInfo, exceptions, sysutils, betterobject, sharedobject, stringx, beeper,orderlyinit;
type
  TUserAgents = class;

  TRequestManager = class(TFakeLockQueuedObject)
    //Maintains a list of ALL TRequestINfo objects active.  Used for server monitoring.
  private
    function GetCount: integer;
    function GetRequests(idx: integer): ansistring;
    function GetAges(idx: integer): cardinal;
    function GetTicks(idx: integer): cardinal;
    function GEtDTs(idx: integer): integer;
    procedure SetDTs(idx: integer; const Value: integer);
    function GetREalRequests(idx: integer): TRequestInfo;
  protected
    FRequests: TStringList;
    FTicks: TList;
    FDTs: TList;
    bShuttingDown: boolean;
    FAgents: TUserAgents;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure UpdateDTID(Request: TRequestInfo; DTID: integer);
    function RegisterRequest(Request: TRequestInfo): boolean;
    procedure DeRegisterRequest(Request: TRequestInfo);
    procedure UpdateStatus(request: TRequestInfo; sStatus: ansistring);
    property Requests[idx: integer]:ansistring read GetRequests;
    property Ticks[idx: integer]: cardinal read GetTicks;
    property Ages[idx: integer]: cardinal read GetAges;
    property DTs[idx: integer]: integer read GEtDTs write SetDTs;
    procedure WaitForThreads;
    procedure Start;
    procedure Shutdown;
    property IsShuttingDown: boolean read bShuttingDown;
    property Count: integer read GetCount;
    property UserAgents: TUserAgents read FAgents;
    property RealRequests[idx: integer]:TRequestInfo read GetREalRequests;
    PROCEDURE sync(i: integer);
    function HasRequest(sURL: ansistring): boolean;
  end;

  TUserAgents = class(TSharedObject)
    //Tracks the various useragents that come into the WebServer.
    //Can be used to grab statistics about how many people use
    //IE vs. Netscape... and how many PC's vs. Macs.
  private
    FAgents: TStringList;
    FAgentCount: integer;
    function GetAgent(index: integer): ansistring;
    function GetAgentStats(index: integer): integer;
    function GetAgentCount: integer;


  public
    constructor create; override;
    destructor destroy; override;
    procedure Clear;
    procedure Tally(sAgent: ansistring);

    function FindAgent(sAgent: ansistring): integer;
    function HasAgent(sAgent: ansistring): boolean;
    property Count: integer read GetAgentCount;
    //Returns the number of unique agents recorded.
    property AgentStats[index: integer]: integer read GetAgentStats;
    //Returns the number of hits for agent at index.
    property Agents[index: integer]: ansistring read GetAgent;
    //Returns the agent name at index.

  end;


var
  rqMan: TRequestManager;

implementation



{ TRequestManager }

constructor TRequestManager.Create;
//Standard.
begin
  bShuttingDown := false;
  inherited;
  FRequests := TStringList.create;
  FAgents := TUserAgents.create;
  FTicks := TList.create;
  FDTs := TList.create;
end;
//------------------------------------------------------------------------------
procedure TRequestManager.DeRegisterRequest(Request: TRequestInfo);
//Called when a request is freed.  Removes it from the manager.
var
  idx: integer;
begin
  LockWrite;
  try
    //AuditLog('Removed request:'+request.request.document);

    idx := FRequests.IndexOfObject(request);
    if idx >=0 then begin
      FRequests.delete(idx);
      FTicks.delete(idx);
      FDTs.delete(idx);
    end;

  finally
    UnLockWrite;
  end;
end;
//------------------------------------------------------------------------------
destructor TRequestManager.Destroy;
//Standard.
begin
  bShuttingDown := true;
  WaitForThreads;
  FRequests.free;
  FAgents.free;
  FTicks.free;
  FDTs.free;
  inherited;
end;
//------------------------------------------------------------------------------
function TRequestManager.GetAges(idx: integer): cardinal;
var
  c: Cardinal;
begin
  LockRead;
  try
    c := GetTickCount;
    if c< cardinal(integer(FTicks[idx])) then
      result := c
    else
      result := c-cardinal(FTicks[idx]);
  finally
    UnlockRead;
  end;
end;

function TRequestManager.GetCount: integer;
//Getter for the Count property
begin
  LockRead;
  try
    result := FRequests.count;
  finally
    UnlockRead;
  end;
end;

function TRequestManager.GEtDTs(idx: integer): integer;
begin
  LockRead;
  try
    result := integer(FDts[idx]);
  finally
    UnlockRead;
  end;
end;

function TRequestManager.GetREalRequests(idx: integer): TRequestInfo;
begin
  LockRead;
  try
    result := TrequestInfo(Frequests.objects[idx]);
  finally
    UnlockRead;
  end;
end;

function TRequestManager.GetRequests(idx: integer): ansistring;
//Getter for the requests property.
begin
  LockRead;
  try
    result := FRequests[idx];
    UniqueSTring(result);
  finally
    UnlockRead;
  end;

end;

//------------------------------------------------------------------------------
function TRequestManager.GetTicks(idx: integer): cardinal;
begin
  LockRead;
  try
    result := cardinal(FTicks[idx]);
  finally
    UnlockRead;
  end;

end;

function TRequestManager.HasRequest(sURL: ansistring): boolean;
var
  t: integer;
begin
  result := false;
  self.LockWrite;
  try
    for t:= 0 to self.Count-1 do begin
      if self.Requests[t] = sURL then begin
        result:= true;
        exit;
      end;
    end;
  finally
    UnlockWRite;
  end;

end;

function TRequestManager.RegisterRequest(Request: TRequestInfo): boolean;
//Called when a request is created.  Adds it to the manager for observation.
var
  s: ansistring;
begin
  result := true;
  LockWrite;
  try
    if bShuttingDown then begin
      raise Exception.create('Server is shutting down.');
      result := false;
      exit;
    end;
    s := request.Request.originaldocument;
    UniqueString(s);
    FRequests.addObject(s, request);
        FTicks.add(pointer(GetTickCount));
    FDTs.add(pointer(254));


  finally
    UnlockWrite;
  end;
end;
//------------------------------------------------------------------------------
procedure TRequestManager.SetDTs(idx: integer; const Value: integer);
begin
  LockWrite;
  try
    FDTs := pointer(value);
  finally
    LockWrite;
  end;
end;

procedure TRequestManager.Shutdown;
//Flags the requestmanager as shutting down.  Any new requests that come in will
//be met with immediate exceptions when they attempt to be added to the manager.
begin
  LockWrite;
  try
    bShuttingDown := true;
  finally
    UnlockWrite;
  end;
  WaitForThreads;
end;

procedure TRequestManager.Start;
//Initilizes... basically just ensures that bShuttingDown = false. opposite of TRequestManager.Shutdown
begin
  LockWrite;
  try
    bShuttingDown := false;
  finally
    UnLockWrite;
  end;
end;

procedure TRequestManager.sync(i: integer);
begin
  FRequests[i] := realrequests[i].Request.Document;
end;

procedure TRequestManager.UpdateDTID(Request: TRequestInfo; DTID: integer);
var
  i: integer;
begin
  LockWRite;
  try
    i := FRequests.IndexOfObject(request);
    if i> -1 then begin
      FDTs[i] := pointer(DTID);
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TRequestManager.UpdateStatus(request: TRequestInfo;
  sStatus: ansistring);
//Refreshes the status of a thread in the manager's list of statuses.
//Status ansistring passed in is made thread safe via forced copy.
var
  idx: integer;
begin
  LockWrite;
  try
    idx := FRequests.IndexOfObject(request);

    UniqueString(sStatus);
    if idx>=0 then
      FRequests[idx] := sStatus;

    UniqueString(sStatus); //<--this is being done because sStatus has been added to array, but will go out of scope outside lock


  finally
    unlockWrite;
  end;
end;
procedure TRequestManager.WaitForThreads;
//Waits for all Requests to clear out of the manager.
begin
  while count>0 do begin
    beeper.beep(80,50);
    sleep(1000);
  end;
end;

{ TUserAgents }

procedure TUserAgents.Clear;
//Wipes out all agents and stats.
begin
  FAgents.clear;
end;

constructor TUserAgents.create;
//standard.
begin
  inherited;
  FAgents := TStringList.create;
end;

destructor TUserAgents.destroy;
//standard.
begin
  FAgents.free;
  inherited;
end;

function TUserAgents.FindAgent(sAgent: ansistring): integer;
//Returns the index to a particular agent record.
begin
  Lock;
  try
    UniqueString(sAgent);
    result := FAgents.indexof(sAgent);
  finally
    UNlock;
  end;

end;

function TUserAgents.GetAgent(index: integer): ansistring;
//Getter for Agent property.
begin
  Lock;
  try
    result := FAgents[index];
    UniqueSTring(result);
  finally
    UNlock;
  end;

end;

function TUserAgents.GetAgentCount: integer;
//Getter for Agent count property.
begin
  Lock;
  try
    result := FAgents.count;
  finally
    Unlock;
  end;

end;

function TUserAgents.GetAgentStats(index: integer): integer;
//Getter for Agent Stats property.
begin
  Lock;
  try
    result := Integer(FAgents.objects[index]);
  finally
    Unlock;
  end;
end;

function TUserAgents.HasAgent(sAgent: ansistring): boolean;
//Returns whether or not a particular agent is recorded.
begin
  Lock;
  try
    result := FAgents.indexof(sAgent) >=0;
  finally
    UNlock;
  end;

end;


procedure TUserAgents.Tally(sAgent: ansistring);
//Tallies a HIT given a particular UserAgent.  If The agent is unique, starts
//a new record for the agent.  If the agent has been at the server before, then
//increments the hitcount for the agent record.
var
  index: integer;
begin
  Lock;
  try
    UniqueSTring(sAgent);
    if HasAgent(sAgent) then begin
      index := FindAgent(sAgent);
      FAgents.objects[index] := Pointer(integer(FAgents.objects[index])+1);
    end else begin
      FAgents.addobject(sAgent, pointer(1));
    end;
  finally
    unlock;
  end;
end;

procedure oinit;
begin
  rqMan := TRequestManager.create;
end;

procedure ofinal;
begin
  rqMan.free;
  rqMan := nil;


end;

initialization
  init.RegisterProcs('RequestManager', oinit, ofinal);


finalization

end.
