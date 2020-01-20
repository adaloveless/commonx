unit ServerInterfaceFactory;

{$DEFINE POOL}//<--- don't use this because...
//1. It is not needed because you should hold onto your own instance to preserve
//   context (e.g. transaction state)
//2. Pooling someone else's connect is likely to just get you in a bad transaction state
//   even if you carefully call rollback or commit or whatever... just better practice to NOT pool this stuff.

interface

uses
  debug, typex, ServerInterfaceInterface, sharedobject, betterobject, sysutils,orderlyinit, generics.collections, tickcount;

const
  KEY_RESERVE_COUNT = 100;
  MAX_POOL_TIME = 10000;
type
  TNextIDCacheRec = record
    keytoken: string;
    nextvalue: int64;
    next_unreserved: int64;
  end;
  PNextIDCacheRec = ^TNextIDCacheRec;
  TNextIDCache = class(TSharedObject)
   private
    keys: array of TNextIDCacheRec;
    function GetKeyToken(FHost, FEndPoint, FContext, sKey: string): string;
    function FindNextIDRec(sKeyToken: string):PNExtIDCacheRec;
  public
    function GetNextID(FHost, FEndPoint, FContext, sKey: string): int64;
  end;

  TServerInterfaceFactory = class(TSharedObject)
  private
    FPool: TList<IServerInterface>;
    FProduct: TBetterObjectClass;
    function GetProduct: TBetterObjectClass;
    procedure SetProduct(const Value: TBetterObjectClass);
    function GetFromPool(key: string): IServerInterface;
    procedure CleanPool;
  public
    constructor create;override;
    destructor destroy;override;
    property Product: TBetterObjectClass read GetProduct write SetProduct;
    function NeedServer(FHost, FEndPoint, FContext: string): IServerInterface;overload;
    function NeedServer(FHost: string; FEndPoint: nativeint; FContext: string): IServerInterface;overload;
    procedure NoNeedServer(si: IServerInterface);
  end;

var
  SIF: TServerInterfaceFactory;
  NextIDCache: TNextIDCache;


implementation

{ TServerInterfaceFactory }

procedure TServerInterfaceFactory.CleanPool;
{$IFDEF POOL}
var
  t: ni;
  o: IServerInterface;
{$ENDIF}
begin
{$IFDEF POOL}
  o := nil;
  Lock;
  try
    for t:= FPool.count-1 downto 0 do begin
      if gettimesince(FPool[t].Pooltime) > MAX_POOL_TIME then begin
        debug.log('delete from pool '+inttostr(t));
        o := FPool[t];//o will go out of scope at the end of the function
        FPool.delete(t);
        break;
      end;

    end;
  finally
    Unlock;
  end;
{$ENDIF}
end;

constructor TServerInterfaceFactory.create;
begin
  inherited;
  FPool := TList<IServerInterface>.create;
end;

destructor TServerInterfaceFactory.destroy;
begin
  FPool.free;
  FPool := nil;
  inherited;
end;

function TServerInterfaceFactory.GetFromPool(key: string): IServerInterface;
var
  t: ni;
  si: IServerInterface;

begin
  Lock;
  try
    result := nil;
//    repeat
      CleanPool;
  //    debug.log('1. there are '+FPool.count.tostring+' in the pool.');
      for t:= 0 to FPool.count-1 do begin
  //      debug.log('2. there are '+FPool.count.tostring+' in the pool.');
        si := FPool[t];

        if (si.MWHost+'/'+si.MWEndPoint+'/'+si.Context) = key  then begin
          result := FPool[t];
          if gettimesince(result.Pooltime) > MAX_POOL_TIME then
            result := nil;
          FPool.delete(t);
  //        debug.log('get from pool '+inttostr(t));
  //        debug.log('3. there are '+FPool.count.tostring+' in the pool.');
          if result <> nil then
           break;
        end;
      end;

//    until result <> nil;
  finally
    Unlock;
  end;

end;

function TServerInterfaceFactory.GetProduct: TBetterObjectClass;
begin
  Lock;
  try
    result := FProduct;
  finally
    Unlock;
  end;
end;

function TServerInterfaceFactory.NeedServer(FHost: string; FEndPoint: nativeint; FContext: string): IServerInterface;
begin
  result := NeedServer(FHost, inttostr(FEndpoint), FContext);
end;

procedure TServerInterfaceFactory.NoNeedServer(si: IServerInterface);
begin
{$IFDEF POOL}
  if si = nil then
    exit;
  Lock;
  try
    CleanPool;
    si.pooltime := GetTicker;


    if FPool.IndexOf(si) >=0 then
      raise ECritical.create('already in pool!');
    if si.IsInTransaction then
      si.Rollback;
    if not si.IsInTransaction then
      FPool.add(si)
    else
      Debug.Log('IServerInterface will not be added to pool because it is in transaction');

    CleanPool;
  finally
    Unlock;
  end;
{$ELSE}
//  si._Release;
{$ENDIF}

end;

function TServerInterfaceFactory.NeedServer(FHost, FEndPoint, FContext: string): IServerInterface;
var
  iu: IUnknown;
begin
  Lock;
  try
{$IFDEF POOL}
    CleanPool;
    result := GetFromPool(Fhost+'/'+FEndpoint+'/'+FContext);
    if result <> nil then
      exit;
{$ENDIF}
    iu := FProduct.create;
    iu.QueryInterface(IServerInterface, result);
{$IFDEF MSWINDOWS}
    result._Release;
{$ENDIF}
    result.MWHost:= FHost;
    result.MWEndpoint:= FEndpoint;
    result.Context := FContext;

    if result = nil then
      raise exception.create('Object does not support the interface IServerInterface');




  finally
    Unlock;
  end;
end;

procedure TServerInterfaceFactory.SetProduct(const Value: TBetterObjectClass);
begin
  Lock;
  try
    FProduct := value;
  finally
    Unlock;
  end;
end;


procedure oinit;
begin
  SIF := TServerInterfaceFactory.create;
  NextIDCache := TNextIDCache.create;


end;

procedure ofinal;
begin
  SIF.free;
  NextIDCache.free;
  SIF := nil;
  NextIDCache := nil;

end;

{ TNextIDCache }

function TNextIDCache.FindNextIDRec(sKeyToken: string): PNExtIDCacheRec;
begin
  for var t:= 0 to high(self.keys) do begin
    if CompareText(keys[t].keytoken, sKeyToken) = 0 then begin
      exit(@keys[t]);
    end;
  end;
  exit(nil);
end;

function TNextIDCache.GetKeyToken(FHost, FEndPoint, FContext, sKey: string): string;
begin
  result := sKey+','+FHost+','+FEndPoint+','+FContext;
end;

function TNextIDCache.GetNextID(FHost, FEndPoint, FContext, sKey: string): int64;
begin
  var l := self.LockI;
  var keytok := GetKeyTOken(fhost, fendpoint, fcontext, sKey);

  var p: PNextIDCacheRec := Self.FindNextIDRec(keytok);

  if p = nil then begin
    setlength(keys, length(keys)+1);
    keys[high(keys)].keytoken := keytok;
    keys[high(keys)].next_unreserved := -1;
    keys[high(keys)].nextvalue := 0;
    p := @keys[high(keys)];
  end;

  if p = nil then
    raise ECritical.create('p should never be nil here');

  if (p.nextvalue >= p.next_unreserved) then begin
    var si := SIF.NeedServer(fhost, fendpoint, fcontext);
    p.nextvalue := si.GetNextIDEx(sKey, KEY_RESERVE_COUNT);
    p.next_unreserved := p.nextvalue+KEY_RESERVE_COUNT;
  end;

  result := p.nextvalue;
  inc(p.nextvalue);


end;

initialization
  init.RegisterProcs('ServerInterfaceFactory', oinit, ofinal);




finalization



end.
