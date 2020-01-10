unit ProblemDomain_DataObjectBased;

interface

uses
  betterobject, bettercomponent, dataobjectcache, dataobjectcachemanager, system.classes, dataobject, EasyData,  ServerInterfaceFactory;

type
  TProblemDomain = class(TBetterComponent)
  public
    globalCache: TDataObjectCache;
    FServer: IUnknown;
    constructor Create(owner: TComponent);override;
    destructor Destroy;override;


    function Fetch<T: TDataObject>(keys: variant): T;
    function Ghost<T: TDataObject>(keys: variant): T;
    function Query(s: string): TDataObject;
    function FunctionQuery(s: string; sDefault: string): string;
    procedure Save(o: TDataObject);
    procedure Delete(o: TDataObject; bPending: boolean = true);
    function New<T: TDataobject>(keys: variant): T;
    procedure ProcessPendingDeletes;
    procedure ClearCache;virtual;


  end;

implementation

{ TProblemDomain }

procedure TProblemDomain.ClearCache;
begin
  DOCM.FreeCache(globalCache);
  DOCM.AllocateCache(GlobalCache, 0, 0, 0);
  globalcache.Server := FServer;
end;

constructor TProblemDomain.Create(owner: TComponent);
begin
  inherited;
  DOCM.AllocateCache(GlobalCache, 0, 0, 0);
  FServer := SIF.NeedServer(g_MWHost, g_MWEndpoint, g_MWContext);
  globalcache.server := FServer;



end;

procedure TProblemDomain.Delete(o: TDataObject; bPending: boolean);
begin
  EDB.Delete(o, globalcache, bPending);
end;

destructor TProblemDomain.Destroy;
begin
  docm.freeCache(GlobalCache);
  inherited;
end;

function TProblemDomain.Fetch<T>(keys: variant): T;
begin
  result := EDB.Fetch<T>(keys, globalcache);
end;

function TProblemDomain.FunctionQuery(s, sDefault: string): string;
begin
  result := EDB.FunctionQuery(s, globalcache, sDefault);
end;

function TProblemDomain.Ghost<T>(keys: variant): T;
begin
  result := EDB.Ghost<T>(keys, globalcache);
end;

function TProblemDomain.New<T>(keys: variant): T;
begin
  result := EDB.New<T>(keys, globalcache);
end;

procedure TProblemDomain.ProcessPendingDeletes;
begin
  EDB.ProcessPendingDeletes(globalcache);
end;

function TProblemDomain.Query(s: string): TDataObject;
begin
  result := EDB.Query(s, globalcache);
end;

procedure TProblemDomain.Save(o: TDataObject);
begin
  ProcessPendingDeletes;
  EDB.Save(o, globalcache);

end;

end.
