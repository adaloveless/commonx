unit EasyData;

interface

uses
  debug, typex, betterobject, dataobject, ServerInterfaceInterface, dataobjectcache, RDTPKeybotClient, DataObjectXref, DataObjectFactory, orderlyinit, consolelock;


type
  TDB = class(TBetterObject)
  strict
  private
  public

    function Fetch<T: TDataObject>(keys: variant; cache: TDataObjectCache): T;
    function Query(s: string; cache: TDataObjectCache): TDataObject;
    function FunctionQuery(s: string; cache: TDataObjectCache; sDefault: string) : string;
    function Ghost<T: TDataObject>(keys: variant; cache: TDataObjectCache): T;
    procedure Save(o: TDataObject; cache: TDataObjectCache);
    function New<T: TDataobject>(keys: variant;  cache: TDataObjectCache): T;
    procedure Delete(o: TDataObject; cache: TDataObjectCache; bPending: boolean = true);
    procedure ProcessPendingDeletes(cache: TDataObjectCache);
    constructor Create; override;
    destructor Destroy; override;

  end;


function EDB: TDB;
var
  fEDB: TDB = nil;
  g_MWHost, g_MWEndpoint, g_MWContext: string;

implementation

{ TDB }

constructor TDB.Create;
begin
  inherited;


end;

procedure TDB.Delete(o: TDataObject; cache: TDataObjectCache;
  bPending: boolean);
var
  si: IServerInterface;
begin
  if bPending then
    cache.PendingDeletes.add(o)
  else begin
    si := IServerInterface(cache.server);
    si.Delete(cache, o);
  end;

end;

destructor TDB.Destroy;
begin

  inherited;
end;

function TDB.Fetch<T>(keys: variant;  cache: TDataObjectCache): T;
var
  si: IServerInterface;
  datObj: TDataObject;
begin
  //T.Classname;
  si := IServerInterface(cache.server);
  if not si.Fetch(cache, datObj, T.ClassName, keys, 0,0) then
    raise Ecritical.create('could not fetch '+T.classname);

  debug.Log(datObj.Name);
  result := datObj as T;

end;



function TDB.FunctionQuery(s: string; cache: TDataObjectCache;
  sDefault: string): string;
begin
  var obj := Query(s, cache);
  if obj.ObjectCount = 0 then begin
    exit(sDefault);
  end;
  result := obj.obj[0].FieldByIndex[0].AsString;
end;

function TDB.Ghost<T>(keys: variant;  cache: TDataObjectCache): T;
var
  si: IServerInterface;
  datObj: TDataObject;
begin
  si := IServerInterface(cache.Server);
  si.Ghost(cache, datObj, T.ClassName, keys,0);
  Debug.Log(datobj.name);
  result := T(datObj);
  Debug.Log(result.name);
end;

function TDB.New<T>(keys: variant;  cache: TDataObjectCache): T;
var
  si: IServerInterface;
  obj: Tdataobject;
  tt: ni;
  xr: TClassXRef;
  doTemp: TDataobject;
begin
  si := IServerInterface(cache.server);
  si.New(cache, obj, T.ClassName, keys,0);
  result := T(obj);
end;

procedure TDB.ProcessPendingDeletes(cache: TDataObjectCache);
var
  si: IServerInterface;
  obj: Tdataobject;
begin
  si := IServerInterface(cache.server);
  while cache.PendingDeletes.Count > 0 do begin
    obj := cache.PendingDeletes[cache.pendingdeletes.count-1];
    si.Delete(cache, obj);
    cache.pendingdeletes.delete(cache.pendingdeletes.count-1);
  end;
end;

function TDB.Query(s: string; cache: TDataObjectCache): TDataObject;
var
  si: IServerInterface;
begin
  si := IServerInterface(cache.server);
  si.Query(cache, result, s, 0, true);
end;

procedure TDB.Save(o: TDataObject;  cache: TDataObjectCache);
var
  si: IServerInterface;
begin
  si := IServerInterface(cache.server);
  si.Post(o, 0, o.IsList);
end;


function EDB(): TDB;
begin
  if fEDB = nil then begin
    LockConsole;
    try
      if fEDB = nil then begin
        fEDB := TDB.create;
      end;
    finally
      unlockconsole;
    end;
  end;

  result := fEDB;
end;

procedure oinit;
begin
  //
end;
procedure ofinal;
begin
  FEDB.free;
  FEDB := nil;

end;

initialization

init.RegisterProcs('EasyData', oinit, ofinal, '');


end.
