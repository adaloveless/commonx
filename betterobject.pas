unit betterobject;
{$INCLUDE DelphiDefs.inc}
{$MESSAGE '*******************COMPILING betterobject.pas'}

{$IFNDEF LAZ}
{$inline auto}
{$ENDIF}
{$D+}
{x$DEFINE REGISTER_OBJECTS}
{$IFDEF MACOS}
{$DEFINE NO_INTERLOCKED_INSTRUCTIONS}
{$ENDIF}
{$DEFINE NO_INTERLOCKED_INSTRUCTIONS}
{$DEFINE UNDEAD_PROTECTION}
{x$DEFINE OBJECT_DEBUG_FACILITIES}

//todo 1: I think if a RUDP connection fails, there's a chance that the connect packet will be double-freed

interface

uses
  RTLConsts,
  sysutils,
  systemx,
  //generics.collections.fixed;
  {$IFDEF WINDOWS}
    {$IFDEF LAZ}
        windows,
    {$ELSE}
        winapi.windows,
    {$ENDIF}
  {$ENDIF}
  typex;

type
  TBetterObject = class;//forward

  IHolder<T: class> = interface
    function Get__Holding: T;
    procedure Set__Holding(const Value: T);
    property o: T read Get__Holding write Set__Holding;

  end;

  THolder<T: class> = class;//forward


  TBetterObject = class(TInterfacedObject)
  private
{$IFDEF UNDEAD_PROTECTION}
    FDead: cardinal;
{$ENDIF}
    FreeAtRef: nativeint;
    FFreeWithReferences: boolean;
    fDetached: boolean;
    detachbegan, detachended: boolean;
    procedure SetFreeWithReferences(const Value: boolean);
  protected
{$IFDEF NO_INTERLOCKED_INSTRUCTIONS}
    FRefSect: TCLXCriticalSection;
{$ENDIF}
  public
    Next, Prev: TBetterObject;
{$IFDEF LINKOWNERS}
    LinkOwner: TBetterOBject;
{$ENDIF}
    class var EnableRegistry: boolean;
    procedure AfterConstruction;override;
    procedure BeforeDestruction;override;

    function _AddRef: Integer; virtual;stdcall;
    function _Release: Integer;virtual; stdcall;
    function _RefCount: Integer;virtual; stdcall;

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;

    constructor Create;reintroduce;virtual;
    procedure DeadCheck;
    destructor Destroy;override;
    procedure BeginDetach;virtual;
    procedure EndDetach;virtual;
    procedure Detach;virtual;
    procedure DetachAndFree;
    procedure DnF;inline;
    property Detached: boolean read FDetached write Fdetached;

    procedure SafeFree;virtual;
    property FreeWithReferences: boolean read FFreeWithReferences write SetFreeWithReferences;
    function IsInterface(guid: TGUID):boolean;
{$IFNDEF FPC}
    function AsInterface<T:IUnknown>(guid: TGUID):T;
{$ENDIF}
    procedure Init;virtual;
    function GetObjectDebug: string;
    function ToHolder<T: class>(): IHolder<T>;
    procedure FreeByInterface;

  end;

  TLightObject = TBetterObject;


  TBetterClass = class of TBetterObject;


  TBetterObjectClass = class of TBetterObject;

  THolder<T: class> = class(TBetterObject, IHolder<T>)
  private
    function Get__Holding: T;
    procedure Set__Holding(const Value: T);
  protected
    procedure Init;override;

  public
    FO: T;
    constructor create;override;
    destructor Destroy;override;
    property o: T read Get__Holding write Set__Holding;
  end;




  TOwnableObject = class(TBetterObject)
  private
    FOwner: TObject;
  public
    property Owner: TObject read FOwner write FOwner;
  end;






implementation

{ TBetterObject }

uses BetterObjectRegistry, orderlyinit;

procedure TBetterObject.AfterConstruction;
begin
  //no implementation needed
end;

{$IFNDEF FPC}
function TBetterObject.AsInterface<T>(guid: TGUID): T;
begin
  if IsInterface(guid) then begin
    //Supports(self, T, result);
    self.QueryInterface(guid,result);
  end;

end;
{$ENDIF}

procedure TBetterObject.BeforeDestruction;
begin
  Detach;
  if RefCount > 1 then begin
    FreeWithReferences := true;
    raise EAbort.create('Trying to free '+self.ClassName+' with more than 1 reference');

  end;

end;


procedure TBetterObject.BeginDetach;
begin
  detachbegan := true;
end;

constructor TBetterObject.create;
begin
{$IFDEF NO_INTERLOCKED_INSTRUCTIONS}
  InitializeCriticalSection(FRefSect);
{$ENDIF}
{$IFDEF REGISTER_OBJECTS}
  if TBetterObject.EnableRegistry then
    bor.ObjectCreated(TBetterClass(self.ClassType), '');
{$ENDIF}
  inherited create;
  Init;
end;

procedure TBetterObject.DeadCheck;
begin
{$IFDEF UNDEAD_PROTECTION}
  if FDead = $DEAD then
    raise ECritical.create('Double-free attempt detected in '+self.ClassName);
{$ENDIF}
end;

destructor TBetterObject.Destroy;
begin
{$IFDEF UNDEAD_PROTECTION}
  DeadCheck;
{$ENDIF}
  if not FDetached then
    Detach;
  inherited;
{$IFDEF REGISTER_OBJECTS}
  if TBetterObject.EnableRegistry then
    bor.ObjectDestroyed(TBetterClass(self.ClassType), '');
{$ENDIF}
{$IFDEF NO_INTERLOCKED_INSTRUCTIONS}
  DeleteCriticalSection(FRefSect);
{$ENDIF}
{$IFDEF UNDEAD_PROTECTION}
  FDead := $DEAD;
{$ENDIF}


end;


procedure TBetterObject.Detach;
begin
  //
  if FDetached then
    exit;
  BeginDetach;
  EndDetach;
  FDetached := true;

end;

procedure TBetterObject.DetachAndFree;
begin
  if self = nil then exit;

  Detach;
{$IFDEF WINDOWS}
  Free;
{$ENDIF}

end;

procedure TBetterObject.DnF;
begin
  DetachAndFree;
end;

procedure TBetterObject.EndDetach;
begin
  detachended := true;
end;

procedure TBetterObject.FreeByInterface;
begin
  FreeAtRef := 1;
end;

function TBetterObject.GetObjectDebug: string;
begin
  result := classname+'@'+inttostr(ni(pointer(self)));
end;

procedure TBetterObject.Init;
begin
  //
end;

function TBetterObject.IsInterface(guid: TGUID): boolean;
var
  cout:IUnknown;
begin
  result := self.QueryInterface(guid,cout)= 0;
end;


function TBetterObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;

end;

procedure TBetterObject.SafeFree;
begin
  Detach;
  if RefCount > 0 then begin
    FreeWithReferences := true;
    exit;
  end else
    Free;
end;

procedure TBetterObject.SetFreeWithReferences(const Value: boolean);
begin
//  if self.classname = 'TReliableUDPPacketLogRecord' then
//    FFreeWithReferences := Value;
  FFreeWithReferences := Value;

end;

function TBetterObject.ToHolder<T>: IHolder<T>;
begin
  result := THolder<T>.create;
  result.o := T(self);
end;

function TBetterObject._AddRef: Integer;
begin
{$IFDEF NO_INTERLOCKED_INSTRUCTIONS}
  EnterCriticalSection(FRefSect);
  inc(FrefCount);
  Result := FRefCount;
  LeaveCriticalSection(FRefSect);
{$ELSE}
  result := InterlockedIncrement(FRefCount);
{$ENDIF}
end;

function TBetterObject._RefCount: Integer;
begin
{$IFDEF NO_INTERLOCKED_INSTRUCTIONS}
  EnterCriticalSection(FRefSect);
{$ENDIF}
  result := FRefCount;
{$IFDEF NO_INTERLOCKED_INSTRUCTIONS}
  LeaveCriticalSection(FRefSect);
{$ENDIF}
end;

function TBetterObject._Release: Integer;
begin
  if FDead = $DEAD then
    raise ECritical.create('trying to release a dead object');
{$IFNDEF NO_INTERLOCKED_INSTRUCTIONS}
  result := InterlockedDecrement(FRefCount);
{$ELSE}
  EnterCriticalSection(FRefSect);
  DeadCheck;
  dec(FRefCount);
  Result := FRefCount;
  LeaveCriticalSection(FRefSect);
{$ENDIF}
  if (Result = FreeAtRef) or ((Result = 0) and FreeWithReferences) then begin
{$IFDEF WINDOWS}
    Destroy;//<--- android has ARC, don't destroy on FMX platforms
{$ENDIF}
  end;


end;

procedure oinit;
begin
//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure ofinal;
begin
//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;




{ THolder<T> }

constructor THolder<T>.create;
begin
  inherited;
//  FreeWithReferences := true;
  FreeByInterface;
end;

destructor THolder<T>.Destroy;
begin
  FO.Free;
  FO := nil;
  inherited;
end;

function THolder<T>.Get__Holding: T;
begin
  result := FO;
end;

procedure THolder<T>.Init;
begin
  inherited;

end;

procedure THolder<T>.Set__Holding(const Value: T);
begin
//  if assigned(Fo) then
//    Fo.free;

  FO := value;


end;

{ TLightObject }


initialization
orderlyinit.init.RegisterProcs('betterobject', oinit, ofinal, 'betterobjectregistry');

finalization

end.
