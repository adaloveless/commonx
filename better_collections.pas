unit better_collections;
{$MESSAGE '*******************COMPILING Better_Collections.pas'}
{$INCLUDE DelphiDefs.inc}
{$IFDEF MSWINDOWS}
{$DEFINE USE_FAST_LIST}
{$ENDIF}
{$DEFINE DEBUG_ITEMS}
interface


uses
  debug, generics.collections.fixed, systemx, sharedobject, typex, betterobject, classes,


{$IFDEF USE_FAST_LIST}
  fastlist,
{$ENDIF}
  sysutils;

type
{$IFDEF USE_FAST_LIST}
  TBetterList<T: class> = class(TFastList<T>)
{$ELSE}
  TBetterList<T: class> = class(TList<T>)
{$ENDIF}
  private
    function GetLast: T;
  public
    constructor Create;
    function Has(obj: T): boolean;
    procedure ClearandFree;
    procedure Replace(old, nu: T);
    procedure BetterRemove(obj: T);
    procedure AddList(list: TBetterList<T>);
    property Last: T read GetLast;
  end;

  TBetterStack<T: class> = class(TStack<T>)
  private
    function GetTop: T;
  public
    property Top: T read GetTop;
  end;

  TSharedList<T: class> = class(TSharedObject)
  private
    FOwnsObjects: boolean;
    function GEtItem(idx: nativeint): T;
    procedure SetItem(idx: nativeint; const Value: T);
  protected
    FList: TBetterList<T>;
    FVolatileCount: ni;
    function GetCount: nativeint;virtual;
  public
    constructor Create;override;
    destructor Destroy;override;

    function Add(obj: T): nativeint;virtual;
    procedure AddList(l: TSharedList<T>);virtual;
    procedure Delete(idx: nativeint);virtual;
    procedure Remove(obj: T);virtual;
    procedure BetterRemove(obj: T);

    procedure Insert(idx: ni; obj: T);virtual;

    property Count: nativeint read GetCount;
    function IndexOf(obj: T): nativeint;virtual;
    property Items[idx: nativeint]: T read GEtItem write SetItem;default;
    procedure Clear;
    procedure FreeAndClear;
    property VOlatileCount: ni read FVolatileCount;
    property OwnsObjects: boolean read FOwnsObjects write FOwnsObjects;
  end;

  TSharedStringList = class(TSharedObject)
  strict private
    FList: TStringList;
  private
    function GetItem(idx: ni): string;
    procedure Setitem(idx: ni; const Value: string);
    function GetText: string;
    procedure SetText(const Value: string);
  public
    property Text: string read GetText write SetText;
    procedure Add(s: string);
    procedure Remove(s: string);
    property Items[idx: ni]: string read GetItem write Setitem;default;
    procedure Delete(i: ni);
    function IndexOf(s: string): ni;


  end;



  TStringObjectList<T_OBJECTTYPE: class> = class(TBetterObject)
  strict private
    FItems: TStringlist;
  private

    FTakeOwnership: boolean;
    FDuplicates: TDuplicates;
    function GetItem(sKey: string): T_OBJECTTYPE;
    procedure SetItem(sKey: string; const Value: T_OBJECTTYPE);
    function GetKey(idx: ni): string;
    function GetItemByIndex(idx: ni): T_ObjectType;
  public
    enable_debug: boolean;
    procedure Add(sKey: string; obj: T_OBJECTTYPE);
    procedure Clear;
    procedure Delete(i: ni);
    procedure Remove(o: T_OBJECTTYPE);
    function IndexOfKey(sKey: string): ni;
    function IndexOfObject(o: T_OBJECTTYPE): ni;
    function Count: ni;

    procedure Init; override;
    constructor Create; override;
    destructor Destroy; override;
    property Items[sKey: string]: T_OBJECTTYPE read GetItem write SetItem;default;
    property ItemsByIndex[idx: ni]: T_ObjectType read GetItemByIndex;
    property Keys[idx: ni]: string read GetKey;
    property TakeOwnership: boolean read FTakeOwnership write FTakeOwnership;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;

    procedure DebugItems;

  end;


implementation

{$IFDEF DEBUG_ITEMS}
uses
  JSONHelpers;
{$ENDIF}

{ TBetterList<T> }

procedure TBetterList<T>.AddList(list: TBetterList<T>);
var
  t: ni;
begin
  for t := 0 to list.count-1 do begin
    self.Add(list[t]);
  end;

end;

procedure TBetterList<T>.BetterRemove(obj: T);
var
  t: ni;
begin
  for t:= count-1 downto 0 do begin
    if items[t] = obj then
      delete(t);
  end;

end;



procedure TBetterList<T>.ClearandFree;
var
  o: T;
begin
  while count > 0 do begin
    o := items[0];
    remove(items[0]);
    o.free;
    o := nil;

  end;

end;

constructor TBetterList<T>.Create;
begin
  inherited;
end;

function TBetterList<T>.GetLast: T;
begin
  result := self[self.count-1];
end;

function TBetterList<T>.Has(obj: T): boolean;
begin
  result := IndexOf(obj) >= 0;
end;


procedure TBetterList<T>.Replace(old, nu: T);
var
  t: ni;
begin
  for t:= 0 to count-1 do begin
    if Self.Items[t] = old then
      self.items[t] := nu;
  end;
end;

{ TSharedList<T> }

{$MESSAGE '*******************1 COMPILING Better_Collections.pas'}
function TSharedList<T>.Add(obj: T): nativeint;
begin
  Lock;
  try
    result := FList.add(obj);
    FVolatileCount := FList.count;
  finally
    Unlock;
  end;
end;
{$MESSAGE '*******************2 COMPILING Better_Collections.pas'}
procedure TSharedList<T>.AddList(l: TSharedList<T>);
var
  x: nativeint;
begin
  l.Lock;
  try
    Lock;
    try
      for x := 0 to l.count-1 do begin
        self.Add(l[x]);
      end;
    finally
      Unlock;
    end;
  finally
    l.Unlock;
  end;
end;

{$MESSAGE '*******************3 COMPILING Better_Collections.pas'}
procedure TSharedList<T>.BetterRemove(obj: T);
begin
  Remove(obj);
end;

{$MESSAGE '*******************4 COMPILING Better_Collections.pas'}
procedure TSharedList<T>.Clear;
begin
  Lock;
  try
    FList.Clear;
    FVolatileCount := FList.count;
  finally
    Unlock;
  end;
end;

{$MESSAGE '*******************5 COMPILING Better_Collections.pas'}
constructor TSharedList<T>.Create;
{$MESSAGE '*******************5.1 COMPILING Better_Collections.pas'}
begin
  {$MESSAGE '*******************5.2 COMPILING Better_Collections.pas'}
  inherited;
  {$MESSAGE '*******************5.3 COMPILING Better_Collections.pas'}
  FList := TBetterList<T>.create();
  {$MESSAGE '*******************5.4 COMPILING Better_Collections.pas'}
end;
{$MESSAGE '*******************5 COMPILING Better_Collections.pas'}

{$MESSAGE '*******************6 COMPILING Better_Collections.pas'}
procedure TSharedList<T>.Delete(idx: nativeint);
begin
  Lock;
  try
    FList.Delete(idx);
    FVolatileCount := FList.count;
  finally
    Unlock;
  end;
end;

{$MESSAGE '*******************7 COMPILING Better_Collections.pas'}
destructor TSharedList<T>.Destroy;
begin
  if OwnsObjects then begin
    while FList.count > 0 do begin
      FList[FList.count].free;
      FList.delete(FList.count);
    end;
  end;
  FList.free;
  FList := nil;
  inherited;
end;

procedure TSharedList<T>.FreeAndClear;
begin
  while count > 0 do begin
    items[count-1].free;
    delete(count-1);
  end;
end;

{$MESSAGE '*******************8 COMPILING Better_Collections.pas'}
function TSharedList<T>.GetCount: nativeint;
begin
  Lock;
  try
    result := FList.count;
  finally
    Unlock;
  end;
end;

{$MESSAGE '*******************9 COMPILING Better_Collections.pas'}
function TSharedList<T>.GEtItem(idx: nativeint): T;
begin
  lock;
  try
    result := FList[idx];
  finally
    Unlock;
  end;
end;

{$MESSAGE '*******************10 COMPILING Better_Collections.pas'}
function TSharedList<T>.IndexOf(obj: T): nativeint;
begin
  Lock;
  try
    result := FList.IndexOf(obj);
  finally
    Unlock;
  end;

end;

{$MESSAGE '*******************11 COMPILING Better_Collections.pas'}
procedure TSharedList<T>.Insert(idx: ni; obj: T);
begin
  Lock;
  try
    FList.Insert(idx, obj);
    FVolatileCount := FList.count;
  finally
    unlock;
  end;

end;

{$MESSAGE '*******************12 COMPILING Better_Collections.pas'}
procedure TSharedList<T>.Remove(obj: T);
begin
  Lock;
  try
    FList.BetterRemove(obj);
    FVolatileCount := FList.count;
  finally
    Unlock;
  end;
end;


{$MESSAGE '*******************13 COMPILING Better_Collections.pas'}
procedure TSharedList<T>.SetItem(idx: nativeint; const Value: T);
begin
  lock;
  try
    FLIst[idx] := value;
  finally
    Unlock;
  end;
end;



{ TBetterStack<T> }

function TBetterStack<T>.GetTop: T;
begin
  result := Peek;
end;

{ TStringObjectList<T_OBJECTTYPE> }

procedure TStringObjectList<T_OBJECTTYPE>.Add(sKey: string; obj: T_OBJECTTYPE);
begin
  case duplicates of
    Tduplicates.dupIgnore: begin

      if FItems.IndexOf(sKey) >=0 then begin
        self[sKey] := obj;
        exit;
      end;
    end;
    Tduplicates.dupError: begin
      raise ECritical.create(classname+' already has item with key '+sKey);
    end;
  end;
  FItems.AddObject(sKey, obj);
  DebugItems();
end;

procedure TStringObjectList<T_OBJECTTYPE>.Clear;
begin
  if takeownership then begin
    while count > 0 do begin
      delete(count-1);
    end;
  end;
  FItems.Clear;

end;

function TStringObjectList<T_OBJECTTYPE>.Count: ni;
begin
  result := FItems.count;
end;

constructor TStringObjectList<T_OBJECTTYPE>.Create;
begin
  inherited;
  FItems := TStringList.create;
  Fitems.CaseSensitive := true;

end;

procedure TStringObjectList<T_OBJECTTYPE>.DebugItems;

var
  t: ni;
begin
{$IFDEF DEBUG_ITEMS}
  if not enable_debug then
    exit;
  Debug.Log('----------------');
  for t:= 0 to FItems.count-1 do begin
    if FItems.Objects[t] is TJSON then begin
      Debug.Log(FItems[t]+' = '+TJSON(FItems.Objects[t]).tojson);
    end;

  end;
{$ENDIF}


end;

procedure TStringObjectList<T_OBJECTTYPE>.Delete(i: ni);
begin
  if takeownership then
    FItems.objects[i].free;
  FItems.Delete(i);
end;

destructor TStringObjectList<T_OBJECTTYPE>.Destroy;
begin
  Clear;
  FItems.free;
  FItems := nil;
  inherited;
end;

function TStringObjectList<T_OBJECTTYPE>.GetItem(sKey: string): T_OBJECTTYPE;
var
  i: ni;
begin
  i := IndexofKey(sKey);
  result := nil;
  if i >=0 then
    result := T_OBJECTTYPE(FItems.Objects[i])
  else
    raise ECritical.create('no object named '+sKey+' was found in '+self.ClassName);

end;

function TStringObjectList<T_OBJECTTYPE>.GetItemByIndex(idx: ni): T_ObjectType;
begin
  result := T_OBJECTTYPE(FItems.Objects[idx]);
end;

function TStringObjectList<T_OBJECTTYPE>.GetKey(idx: ni): string;
begin
  result := FItems[idx];
end;

function TStringObjectList<T_OBJECTTYPE>.IndexOfKey(sKey: string): ni;
begin
  result := FItems.IndexOf(sKey);

end;

function TStringObjectList<T_OBJECTTYPE>.IndexOfObject(o: T_OBJECTTYPE): ni;
begin
  result := FItems.IndexOfObject(o);

end;

procedure TStringObjectList<T_OBJECTTYPE>.Init;
begin
  inherited;

end;

procedure TStringObjectList<T_OBJECTTYPE>.Remove(o: T_OBJECTTYPE);
var
  i: ni;
begin
  i := FItems.IndexOfObject(o);
  if i >=0 then begin
    Delete(i);
  end;



end;

procedure TStringObjectList<T_OBJECTTYPE>.SetItem(sKey: string;
  const Value: T_OBJECTTYPE);
var
  i: ni;
begin
  i := Fitems.IndexOf(sKey);
  if i >= 0 then
    FItems.Objects[i] := value;
end;

{ TSharedStringList }

procedure TSharedStringList.Add(s: string);
var
  l : ILock;
begin
  l := self.LockI;
  FList.add(s);
end;

procedure TSharedStringList.Delete(i: ni);
var
  l: ILock;
begin
  l := self.LockI;
  FList.delete(i);
end;

function TSharedStringList.GetItem(idx: ni): string;
var
  l: ILock;
begin
  l := self.LockI;
  result := FList[idx];
end;

function TSharedStringList.GetText: string;
var
  l: ILock;
begin
  l := self.LockI;
  result := Flist.Text;
end;

function TSharedStringList.IndexOf(s: string): ni;
var
  l: ILock;
begin
  l := self.LockI;
  result := Flist.IndexOf(s);

end;

procedure TSharedStringList.Remove(s: string);
var
  l: ILock;
  i: ni;
begin
  l := self.LockI;
  i := FList.IndexOf(s);
  if i>=0 then
    FList.delete(i);

end;

procedure TSharedStringList.Setitem(idx: ni; const Value: string);
var
  l: ILock;
begin
  l := self.LockI;
  Flist[idx] := value;

end;

procedure TSharedStringList.SetText(const Value: string);
var
  l: ILock;
begin
  l := self.LockI;
  Flist.Text := value;
end;

end.

