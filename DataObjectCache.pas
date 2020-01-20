unit DataObjectCache;
interface
//{$Message Warning 'This code using OLD, primitive threading techniques'}

uses
  debug, DataObjectXref, classes, sysutils, Dataobject, generics.collections,
  PersistentInterfacedObject, betterobject, sharedobject, stringx;

const
  XREF_STRING_CACHE_SIZE = 5;
type
//##############################################################################
  TDeadCacheThread =class;//forward

//##############################################################################

  TDataObjectCache = class(TFakeLockQueuedObject)
  private
    FDeadTime: cardinal;
    FCacheKey: string;
    FDeadWeight: integer;
    FDecommisionOnDead: boolean;
    FServer: IUnknown;
    function GetServer: IUnknown;
    function GetCachedXrefStringIndex(s: string): integer;
    function GetDead: boolean;
    procedure SetDead(const Value: boolean);
    function GetCacheKey: string;
    function GetString: string;
  protected
    FXRef: TXrefPool;
    slObjects: TStringList;
    Fowner: TObject;
    bReleased : boolean;
    FDead: boolean;
    sLastXrefSTringSearch: array[1..XREF_STRING_CACHE_SIZE] of string;
    iLastXrefStringSearchResult: array[1..XREF_STRING_CACHE_SIZE] of integer;
    iXRefRoundRobinIndex: integer;
    function GetCount: integer;
    function GetObjects(idx: integer): TDataObject;

    procedure Add(obj: TDataObject);
    procedure Remove(obj: TDataObject);
  public
    PendingDeletes: Tlist<TDataObject>;
    procedure NotifyParentKeyUpdate(parentObject: TDataObject; priorkeys: variant);
    function RealObject: TObject;
    constructor Create(owner: TObject; XRefPool: TXRefpool; sCacheKey: string); reintroduce;
    destructor Destroy; override;

    function GetExistingObject(sNameWithTypeID: string): TDataObject; overload;//secondary
    function GetExistingObject(sType: string; vParams: variant; DataCenter, DataTier: integer): TDataObject;overload;//secondary


    property objects[idx: integer]: TDataObject read GetObjects; default;
    property count: integer read GetCount;

    procedure RegisterDataObject(doTemp: TDataObject);
    procedure DeRegisterDataObject(doTemp: TDataObject);

    property DecommisionOnDead: boolean read FDecommisionOnDead write FDecommisionOnDead;
    property Dead: boolean read GetDead write SetDead;
    property DeadWeight: integer read FDeadWeight;
    property DeadTime: cardinal read FDeadTime write FDeadTime;
    procedure FreeWithObjects;
    property CacheKey: string read GetCacheKey;

    property RQs: string read GetString;
    procedure CacheThisXRef(s: string; index: integer);
    property Server: IUnknown read GetServer write FServer;
    procedure ExpireAll;
  end;

//##############################################################################
  TDeadCacheThread = class(TThread)
  private
    { Private declarations }
    FCache: TDataObjectCache;
  protected
    procedure Execute; override;
  public
    constructor Create(cache: TDataObjectCache);
  end;


//##############################################################################

implementation

uses
  systemx, DataObjectServices, DataObjectCacheManager;

//----------------------------------------------------------------------------
procedure TDataObjectCache.Add(obj: TDataObject);
begin
  slObjects.AddObject(obj.token.constname, obj);


end;
//----------------------------------------------------------------------------
constructor TDataObjectCache.Create(owner: TObject; XRefPool: TXRefpool;sCacheKey: string);
begin
  inherited Create;
  bReleased := false;
  Fowner := owner;
  FXRef := XrefPool;
  FDead:= false;
  PendingDeletes := TList<TDataObject>.create;
  slObjects := TStringList.create;
  slObjects.Sorted := true;
  FCacheKey := sCacheKey;
  FDeadWeight := 1;

  if FOwner<> nil then
    TDataObjectCacheManager(FOwner).RegisterCache(self);



end;
//----------------------------------------------------------------------------
procedure TDataObjectCache.DeRegisterDataObject(doTemp: TDataObject);
begin
  try
//    LockWrite;
    Remove(doTemp);
  finally
//    UnLockWrite;

    if bReleased and (slObjects.count = 0) then begin
      //if there are references then difer the free
      if ((*_Refcount*)1 > 0) then begin
//        self.FreeWithReferences;

      //if there are not references, then free immediately
      end else begin
        self.Free;
      end;
    end;
  end;

end;
//----------------------------------------------------------------------------
destructor TDataObjectCache.Destroy;
begin
  if FOwner<> nil then
    TDataObjectCacheManager(FOwner).DeRegisterCache(self);


  FOwner := nil;
  slObjects.free;
  PendingDeletes.free;
  PendingDeletes := nil;

  inherited;
end;
procedure TDataObjectCache.ExpireAll;
var
  t: integer;
begin
  for t:= 0 to self.count -1 do begin
    self.objects[t].Expired := true;
  end;

end;

//----------------------------------------------------------------------------
function TDataObjectCache.GetCount: integer;
//Returns the total number of objects in caches
begin
  LockRead;
  try
    result := slObjects.count;
  finally
    UnLockRead;
  end;

end;
//----------------------------------------------------------------------------
function TDataObjectCache.GetExistingObject(sType: string;
  vParams: variant; DataCenter, DataTier: integer): TDataObject;
var
  xr: PClassXref;
  i: integer;
  sFullName: string;
begin
  i := self.GetCachedXrefStringIndex(sType);
  if i > -1 then
    xr := FXref.XRefs[i]
  else begin
    i := Fxref.IndexOfXrefByString(sType);
    self.CacheThisXRef(sType, i);
    xr := FXref.XRefs[i];
  end;

  sfullName := BuildObjectName(sType, vParams, Datacenter, datatier);
  result := GetExistingObject(sFullName);

end;
//----------------------------------------------------------------------------
function TDataObjectCache.RealObject: TObject;
begin
  result := self;
end;
//----------------------------------------------------------------------------
procedure TDataObjectCache.RegisterDataObject(doTemp: TDataObject);
begin
//  LockWrite;
//  try
    if slobjects.indexof(doTemp.name) >=0 then
      raise Exception.Create('duplicate object in cache!');
//    debug.log('register '+doTemp.name);
     Add(doTemp);
//  finally
//    UnLockWrite;
//  end;
end;
//----------------------------------------------------------------------------
procedure TDataObjectCache.FreeWithObjects;
//This should be called on a decommissioned cache...
//so any objects should be reference-free
var
  t: integer;
begin
//  showmessage('Free with objects refcount: '+inttostr(_RefCount));
//  AuditLog('**********************************************************Cache Destroy Start', 'cache', false);
//  AuditLog('CacheKey:'+self.CacheKey, 'cache', false);
//  AuditLog('Thread:'+inttostr(GetCurrentThreadID), 'cache', false);
//  AuditLog('**********************************************************Cache Destroy Start', 'cache', false);
  try
    for t:=slObjects.count-1 downto 0 do begin
      //**IMPORTANT** MUST RECHECK object count each time through
      //because a single DETACH call could potentially
      //remove all objects from the cache at once
      if t<slObjects.count then begin
//        AuditLog('Destroy count:'+inttostr(slObjects.count),'cache');
//        AuditLog('Enter Detach:'+TDataObject(slObjects.objects[t]).Name,'cache');
        TDataObject(slObjects.objects[t]).Detach;
      end;
    end;

    for t:=slObjects.count-1 downto 0 do begin
      if slObjects.IndexOfObject(slObjects.objects[t]) <> t then begin
        slObjects.delete(t);
      end;
    end;

  except
    on E: Exception do begin
//      AuditLog('Exeception**'+E.Message,'cache', false, false);
    end;
  end;
  //Once all objects have allegedly detached from
  //each other... free the objects that were never implicitly
  //referenced.


  while Count>0 do begin
//    AuditLog('Freeeing object @'+inttohex(integer(pointer(slObjects.objects[count-1])),8),'cache', false, false);
    TDataObject(slObjects.objects[count-1]).Free;
  end;

  self.Free;

end;
//----------------------------------------------------------------------------
function TDataObjectCache.GetExistingObject(
  sNameWithTypeID: string): TDataObject;
var
  iTemp: integer;
begin
  iTemp := slObjects.indexOf(sNameWithTypeID);
  if iTemp < 0 then
    result := nil
  else begin
    result := TDataObject(slObjects.objects[iTemp]);
    //optimize, push to front
    //slObjects.Move(iTemp, 0);

    if result.LinkTo <> 'nil' then
    if result.LinkTo <> '' then begin
      result := GetExistingObject(result.LinkTo);
    end else begin
      //result := nil;
    end;


  end;




end;
//----------------------------------------------------------------------------
procedure TDataObjectCache.Remove(obj: TDataObject);
var
  idx: integer;
begin
//  showmessage('Remove refcount: '+inttostr(_RefCount));

  idx := slObjects.IndexOF(obj.token.constname);

  if idx >-1 then
    slObjects.Delete(idx);


//  showmessage('End Remove refcount: '+inttostr(_RefCount));


end;

//----------------------------------------------------------------------------
function TDataObjectCache.GetObjects(idx: integer): TDataObject;
begin
  result := TDataObject(slObjects.objects[idx]);

end;
//----------------------------------------------------------------------------
function TDataObjectCache.GetDead: boolean;
begin
   result := FDead;
end;
//----------------------------------------------------------------------------
procedure TDataObjectCache.SetDead(const Value: boolean);
//When DEAD is set to TRUE (which occurs when FreeWithObjects is called) then
//the cache will add references to all obejcts in the cache, then spawn
//a lower-priority thread to remove them.  This is so that the thread that
//is PROCESSING THE WEB REQUEST can return without having to free all the
//Data objects.  The objects wil be freed in their own thread, independently.
begin
  //IF Already set to appropriate value then exit
  if (Value = FDead) then
    exit;

  //IF setting to TRUE then
  if VALUE then begin
    //Wait to start until all objects

    //reference all objects (to delay their destruction)
//    for t:= 0 to lstObjects.count-1 do begin
//      TDataObject(lstObjects.objects[t]).Reference;
//    end;

    //Spawn a thread that will release the objects over time
//    TDeadCacheThread.create(self);
    //deadThread is freed on termination

    inc(FDeadWeight);
  end;

  Fdead := value;

end;

{ TDeadCacheThread }

constructor TDeadCacheThread.Create(cache: TDataObjectCache);
begin
  inherited Create(true);
  FCache := cache;
  FreeOnTerminate := True;
//  priority := tpLower;
  Resume;
end;

procedure TDeadCacheThread.Execute;
var
  t: integer;
begin
  //release references from all objects in the cache;
  for t:= Fcache.count-1 downto 0 do begin
    TDataObject(Fcache.objects[t]).Release;

    //Termination control
    if Terminated then
      exit;
  end;

end;

function TDataObjectCache.GetCacheKey: string;
begin
  LockRead;
  try
    result := MakeThreadSafe(FCAcheKey);

  finally
    UnlockRead;
  end;
end;

function TDataObjectCache.GetServer: IUnknown;
begin

  result := FServer;




end;

function TDataObjectCache.GetString: string;
var
  sl : TStringList;
  sLine: string;
  t: integer;
begin
  sl := TStringList.create;

  result := '';
  try
    for t:=0 to count-1 do begin
      sLine := objects[t].Token.TypeName;
      sl.add(sLIne);
    end;
  finally
    sl.Duplicates := dupIgnore;
    sl.Sort;
    result := sl.text;
    sl.free;
  end;

end;

procedure TDataObjectCache.NotifyParentKeyUpdate(parentObject: TDataObject; priorkeys: variant);
var
  t: nativeint;
  o: TDataObject;
  slTemp: Tstringlist;
begin
  slTemp := TStringList.create;
  try
    for t:= 0 to slObjects.count-1 do begin
      slTemp.AddObject(slObjects[t], slObjects.objects[t]);
    end;

    for t:= 0 to slTemp.count-1 do begin
      o := slTemp.objects[t] as TDataObject;
      if o.IsNew then
        o.OnParentKeyUpdate(parentObject, priorkeys);
    end;

  finally
    slTemp.free;
  end;

end;

function TDataObjectCache.GetCachedXrefStringIndex(s: string): integer;
var
  t: integer;
begin
  if iXRefRoundRobinIndex = 0 then
    iXRefRoundRobinIndex := 1;

  result := -1;
  s:= lowercase(s);
  for t:= 1 to XREF_STRING_CACHE_SIZE do begin
    if self.sLastXrefSTringSearch[self.iXRefRoundRobinIndex] = s then begin
      result := self.iLastXrefStringSearchResult[iXRefRoundRobinIndex];
      break;
    end;
    inc(self.iXRefRoundRobinIndex);
    if self.iXRefRoundRobinIndex > XREF_STRING_CACHE_SIZE then
      self.iXRefRoundRobinIndex := 1;

  end;


end;



procedure TDataObjectCache.CacheThisXRef(s: string; index: integer);
begin
  inc(self.iXRefRoundRobinIndex);
  if self.iXRefRoundRobinIndex > XREF_STRING_CACHE_SIZE then
    self.iXRefRoundRobinIndex := 1;

  sLastXrefSTringSearch[self.iXRefRoundRobinIndex] := lowercase(s);
  iLastXrefSTringSearchResult[self.iXRefRoundRobinIndex] := index;


end;

end.
