unit DataObjectCacheManager;
//[x]When cache is free... move from active to inactive list
//[ ] When cache is allocated... search for dead cache matching key and move
//from dead to active list
//[ ] Add a thread to periodically scan the list of dead caches and officially
//free ones that have been around for more than 15-20 seconds



interface
uses
  DataObjectCache, PersistentInterfacedObject, classes, Dataobjectfactory, tickcount, systemx,
  DataObject, DataObjectCacheJanitor, backgroundthreads, betterobject, sharedobject, stringx, orderlyinit, managedthread;


type

  TDataObjectCacheManager = class(TFakeLockQueuedObject)
  protected
    lstCaches: TList;
    lstDeadCaches2: TList;
    lstDecommisionedCaches: TList;
  private
    FJanitor: TDataObjectCacheJanitor;
    FDOSV: TObject;
    FJanitorRegistered: boolean;
    function GetJanitorRegistered: boolean;
  public
    constructor create(DOSV: TObject); reintroduce; virtual;
    destructor destroy; override;

    procedure RegisterCache(cache: TDataObjectCache);
    procedure DeRegisterCache(cache: TDataObjectCache);

    function AllocateCache(out cache: TDataObjectCache; iSessionKey: integer; iDataCenterID, iDataTierID: integer; bias: TDataObjectCacheBias = cbSmall): boolean;

    procedure FreeCache(cache: TDataObjectCache); overload;
    function ExpireCaches: integer;


    function RealObject: TObject;
    function ActiveCacheCount: integer;
    function DeadCacheCount: integer;
    function DeCommisionedCacheCount: integer;
    function IndexOfDeadCache(sCacheKey: string): integer;
    property Janitor: TDataObjectCacheJanitor read FJanitor;
    property JanitorRegistered: boolean read GetJanitorRegistered;
    procedure RegisterJanitor(jan: TDataObjectCacheJanitor);
    procedure DeRegisterJanitor(jan: TDataObjectCacheJanitor);
    property DOSVLocal: TObject read FDOSV write FDOSV;

  end;

var
  DOCM: TDataObjectCacheManager;


implementation
uses
  DataObjectServices, sysutils;


{ TDataObjectCacheManager }

//------------------------------------------------------------------------------
function TDataObjectCacheManager.AllocateCache(out cache: TDataObjectCache;
  iSessionKey: integer; iDataCenterID, iDataTierID: integer; bias: TDataObjectCacheBias = cbSmall): boolean;
var
  iIndex: integer;
  sCacheKey: string;
begin
  result := false;

  Lock;
  try
    //if a dead cache is found with the session key then
    sCacheKEy := inttohex(iDataCenterID,2)+inttohex(iDataTierID,2)+inttohex(iSessionKey,8);
    iIndex := IndexOfDeadCache(sCacheKey);

    if iIndex > -1 then begin
      //set result to the cache found
      cache := TDataObjectCache(lstDeadCaches2[iIndex]);
      //move that cache from dead list to active
      lstDeadCaches2.Delete(iIndex);
      lstCaches.Add(cache);

      //Set Dead Flag to false
      cache.Dead := false;
    end
    //If no dead cache was found then
    else begin
      cache := TDataObjectCache.create(self, DOCF.XRefPool, MakethreadSafe(sCacheKey));
      result := (cache <> nil);
    end;
  finally
    UnLock;
  end;

end;

//------------------------------------------------------------------------------
function TDataObjectCacheManager.ActiveCacheCount: integer;
begin
  Lock;
  try
    result := lstCaches.count;
  finally
    Unlock;
  end;
end;

//------------------------------------------------------------------------------
constructor TDataObjectCacheManager.create(DOSV: TObject);
begin
  inherited Create;
  ics(sect);
  lstCaches:= Tlist.create;
  lstDeadCaches2 := TList.create;
  lstDecommisionedCaches := TList.create;
  FDOSV := DOSV;
  FJanitor := TPM.Needthread<TDataObjectCacheJanitor>(BackgroundThreadMan);
  FJAnitor.CAcheManager := self;

  //Fjanitor := TDataObjectCacheJanitor.create(BackGroundThreadMan, BackGroundThreadMan, self);

 FJanitor.beginstart;


end;

//------------------------------------------------------------------------------
procedure TDataObjectCacheManager.DeRegisterCache(
  cache: TDataObjectCache);
begin
  lock;
  try
//    lstCaches.Remove(cache);
//    lstDeadCaches.Remove(cache);
    lstDecommisionedCaches.Remove(cache);
  finally
    Unlock;
  end;

end;

//------------------------------------------------------------------------------
destructor TDataObjectCacheManager.destroy;
begin
  Janitor.Stop;
  Janitor.WaitForFinish;
  Janitor.Detach;
  TPM.NoNeedthread(Janitor);
  FJanitor := nil;
  while (lstDeadCaches2.count > 0)
  or (lstCaches.count > 0)
  or (lstDecommisionedCaches.count > 0)
  do
    ExpireCaches;
  lstDeadCaches2.free;
  lstCaches.free;
  lstDecommisionedCaches.free;
  DeleteCriticalSection(sect);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TDataObjectCacheManager.FreeCache(cache: TDataObjectCache);
var
  iIndex: integer;
  cTemp: TDataObjectCache;
const
  always_DOD = TRUE;
begin
  //Move the cache from main list to other list;

  Lock;
  try
    iIndex := lstCaches.IndexOf(cache);
    if iIndex > -1 then begin
      cTemp := TDataObjectCache(lstCaches[iIndex]);
      lstCaches.delete(iIndex);
      //if DecommisionOnDead then move straight to DecommisionedList
      if ALWAYS_DOD or cTemp.DecommisionOnDead then
        lstDecommisionedCaches.Add(cTemp)
      //otherwise do the standard add
      else
        lstDeadCaches2.Add(cTemp);

      cTemp.Dead := true;
      cTemp.DeadTime := GetTicker;
    end;

  finally
    UnLock;
  end;




end;


//------------------------------------------------------------------------------
function TDataObjectCacheManager.RealObject: TObject;
begin
  result := self;
end;

//------------------------------------------------------------------------------
procedure TDataObjectCacheManager.RegisterCache(
  cache: TDataObjectCache);
begin
  Lock;
  try
    lstCaches.Add(cache);
  finally
    Unlock;
  end;
end;

function TDataObjectCacheManager.IndexOfDeadCache(sCacheKey: string): integer;
var
  t: integer;
  c: TDataObjectCache;
begin
  result := -1;

  Lock;
  try
    sCacheKey := lowercase(sCacheKey);
    //cycle through dead caches for a cache that matches the key
    for t:= 0 to lstDeadCaches2.count-1 do begin
      c := TDataObjectCache(lstDeadCaches2[t]);
      if lowercase(c.CacheKey) = sCacheKey then begin
        result :=t;
        break;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TDataObjectCacheManager.ExpireCaches: integer;
const
  CACHE_TIMEOUT = 0;
//returns number of caches Expired (work load)
var

  t: integer;
  cache : TDataObjectCache;
  tmNow: cardinal;
begin
  result := 0;

  tmNow := GetTicker;
  //Cycle through dead caches for ones that are over
  //CACHE_TIMEOUT seconds old
  Lock;
  try
    for t:= lstDeadCAches2.count-1 downto 0 do begin
      //Get the cache from the dead list
      cache := TDataObjectCache(lstDeadCaches2[t]);
      //if expired then free
      if ((tmNow >= cache.DeadTime+CACHE_TIMEOUT) or (cache.DeadTime > tmNow)
          or cache.DecommisionOnDead) then begin

        //remove NOW (to prevent race conditions)
        lstDeadCaches2.delete(t);

        //put in decommisioned list
        lstDecommisionedCaches.add(cache);
      end;
    end;
  finally
    Unlock;
  end;

  if lstDecommisionedCaches.count > 0 then
  for t:= lstDecommisionedCaches.count-1 downto lstDecommisionedCaches.count-1 do begin
    cache := TDataObjectCache(lstDecommisionedCaches[t]);
//    DOThreadCAche := cache;
    if cache <> nil then begin
      //showmessage('freeing cache for session: '+inttostr(cache.CacheKey));
      cache.freeWithObjects; //cache is deregistered in destructor
      inc(result);
    end;
  end;


end;

//------------------------------------------------------------------------------
function TDataObjectCacheManager.DeadCacheCount: integer;
begin
  Lock;
  try
    result := lstDeadCaches2.count;
  finally
    Unlock;
  end;
end;
//------------------------------------------------------------------------------
function TDataObjectCacheManager.DeCommisionedCacheCount: integer;
begin
  Lock;
  try
    result := lstDecommisionedCaches.count;
  finally
    Unlock;
  end;
end;
//------------------------------------------------------------------------------
procedure TDataObjectCacheManager.DeRegisterJanitor(
  jan: TDataObjectCacheJanitor);
begin
  Lock;
  try
    FJanitorRegistered := false;
  finally
    UnLock;
  end;
end;
//------------------------------------------------------------------------------
procedure TDataObjectCacheManager.RegisterJanitor(
  jan: TDataObjectCacheJanitor);
begin
  Lock;
  try
    FJanitorRegistered := true;
  finally
    UnLock;
  end;
end;
//------------------------------------------------------------------------------
function TDataObjectCacheManager.GetJanitorRegistered: boolean;
begin
  Lock;
  try
    result := FJanitorRegistered;
  finally
    UnLock;
  end;
end;

procedure oinit;
begin
  doOb := TDataObjectobserver.create;
  DOCM := TDataObjectCacheManager.create(nil);

end;

procedure ofinal;
begin
  DOCM.free;
  DOCM := nil;
  doob.free;

end;


initialization
  orderlyinit.init.RegisterProcs('DataObjectCacheManager', oinit, ofinal, 'ManagedThread,SimpleReliableUDP');


finalization
end.
