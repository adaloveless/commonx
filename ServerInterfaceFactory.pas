unit ServerInterfaceFactory;

{$DEFINE POOL}

interface

uses
  debug, typex, ServerInterfaceInterface, sharedobject, betterobject, sysutils,orderlyinit, generics.collections, tickcount;

type
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


implementation

{ TServerInterfaceFactory }

procedure TServerInterfaceFactory.CleanPool;
{$IFDEF POOL}
var
  t: ni;
{$ENDIF}
begin
{$IFDEF POOL}
  Lock;
  try
    for t:= FPool.count-1 downto 0 do begin
      if gettimesince(FPool[t].Pooltime) > 300000 then begin
        debug.log('delete from pool '+inttostr(t));
        FPool.delete(t);
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
    CleanPool;
    debug.log('1. there are '+FPool.count.tostring+' in the pool.');
    for t:= 0 to FPool.count-1 do begin
      debug.log('2. there are '+FPool.count.tostring+' in the pool.');
      si := FPool[t];

      if (si.MWHost+'/'+si.MWEndPoint+'/'+si.Context) = key  then begin
        result := FPool[t];
        FPool.delete(t);
        debug.log('get from pool '+inttostr(t));
        debug.log('3. there are '+FPool.count.tostring+' in the pool.');
        exit;
      end;
    end;
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
    si.pooltime := GetTicker;


    if FPool.IndexOf(si) >=0 then
      raise ECritical.create('already in pool!');
    FPool.add(si);

  finally
    Unlock;
  end;
{$ENDIF}
end;

function TServerInterfaceFactory.NeedServer(FHost, FEndPoint, FContext: string): IServerInterface;
var
  iu: IUnknown;
begin
  Lock;
  try
{$IFDEF POOL}
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


end;

procedure ofinal;
begin
  SIF.free;


end;

initialization
  init.RegisterProcs('ServerInterfaceFactory', oinit, ofinal);




finalization



end.
