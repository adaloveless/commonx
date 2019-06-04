unit MathGate;

interface


uses
  tickcount, typex, systemx, sysutils, variants, stringx, classes, betterobject, sharedobject, JSONHelpers, commandprocessor, generics.collections.fixed;



type
  TValueGate = class(TSharedObject)
  private
    procedure SetInput(sName: string; obj: IHolder<TJSON>);
    function GetInput(sNAme: string): IHolder<TJSON>;
  public
    property Inputs[sName: string]: IHolder<TJSON> read GetInput write SetInput;
  end;

  TValueCacheRec = record
    key: string;
    filename: string;
//    value: string;
    expires: TDateTime;
  end;

  TAbstractValueCache = class(TSharedObject)
  private
    nl: TNamedLocks;
    cache: TDictionary<string, TValueCacheRec>;
    function GetItems(key: string): TValueCacheRec;
  protected
    function DoAcquire_Sync(name: string; params: TStringlist; uncachedparams, nonceparam: string; nonce: string): string; virtual; abstract;
    property Items[key: string]: TValueCacheRec read GetItems;
    procedure ExpireOld;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Acquire(urn_SPACE_params: string; uncachedparams: string; nonceparam: string; nonce: string; ttl: ticker): string;

  end;






implementation

{ TAbstractValueCache }

function TAbstractValueCache.Acquire(urn_SPACE_params: string; uncachedparams: string; nonceparam: string; nonce: string; ttl: ticker): string;
var
  sl: TSTringlist;
  urn: string;
  vcr: TValueCacheRec;
begin
  sl := nil;
  try
    sl := ParseString(urn_SPACE_params, ' ');
    urn := sl[0];
    nl.GetLock(urn);
    try
      if cache.ContainsKey(urn) then begin
        vcr := cache.Items[urn];
        if now < vcr.expires then
          exit(loadfileasstring(vcr.filename));
      end;

      vcr.expires := now+(ttl/(1000*60*60*24));
      vcr.key := urn;
      vcr.filename := dllpath+'datacache\'+inttostr(getticker)+'-'+inttostr(random(999999999))+'-'+inttostr(getcurrentthreadid)+'.dat';
      forcedirectories(extractfilepath(vcr.filename));
      SaveStringAsFile(vcr.filename,
        DoAcquire_Sync(urn, sl, uncachedparams, nonceparam, nonce)
        //^^^ HEY LOOK HERE
      );

      result := LoadFileAsString(vcr.filename);
      Lock;
      try
        cache.remove(urn);
        ExpireOld;
        cache.Add(urn, vcr);

      finally
        Unlock;
      end;


    finally
      nl.ReleaseLock(urn);
    end;
  finally
    sl.free;
  end;
end;

constructor TAbstractValueCache.Create;
begin
  inherited;
  nl := TNamedLocks.Create;
  cache := TDictionary<string, TValueCacheRec>.create;
end;

destructor TAbstractValueCache.Destroy;
begin
  nl.Free;
  nl := nil;
  cache.free;
  cache := nil;
  inherited;
end;



procedure TAbstractValueCache.ExpireOld;
var
  t: ni;
  bDone: boolean;
begin
  repeat
    bDone := true;
    for t:= 0 to cache.Keys.Count-1 do begin
      if cache.Items[cache.Keys.ToArray[t]].expires < now then begin
        try
          deletefile(cache.Items[cache.Keys.ToArray[t]].filename);
          cache.Remove(cache.Items[cache.Keys.ToArray[t]].key);
          bDone := false;
          break;
        except
        end;
      end;
    end;
  until bDone;

end;

function TAbstractValueCache.GetItems(key: string): TValueCacheRec;
begin
  Lock;
  try
    result := cache.Items[key];
  finally
    Unlock;
  end;

end;


{ TValueGate }

function TValueGate.GetInput(sNAme: string): IHolder<TJSON>;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TValueGate.SetInput(sName: string; obj: IHolder<TJSON>);
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

end.
