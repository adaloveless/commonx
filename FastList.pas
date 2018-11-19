unit FastList;
{$INCLUDE DelphiDefs.inc}
{$IFNDEF WINDOWS}
  !error, this is only intended for windows (non-arc) compilers
{$ENDIF}

interface

uses
  systemx, typex, sysutils, generics.collections, generics.defaults;

type
  PPointer = ^pointer;
  TFastList<TYP: class>= class(TObject)
  private
    FObjects: PPointer;
    FCount: ni;
    FCapacity: ni;
    function GetPointerToItem(idx: ni): PPointer;
    function GetCount: ni;
    function GetItem(idx: ni): TYP;
    procedure SetItem(idx: ni; const Value: TYP);
  protected
    procedure SetCapacity(sz: ni);

  public
    procedure Sort;overload;
    procedure Sort(const AComparer: IComparer<TYP>);overload;
    function Add(o: TYP): ni;
    function Remove(o: TYP): ni;
    function Insert(const idx: ni; o: TYP): ni;
    procedure Delete(const idx: ni);
    property Items[idx: ni]: TYP read GetItem write SetItem;default;
    property Count: ni read GetCount;
    function IndexOf(o: TYP): ni;
    function Has(o: TYP): boolean;
    procedure Clear;

  end;

implementation

{ TFastList<TYP> }

function TFastList<TYP>.Add(o: TYP): ni;
var
  refidx: ni;
begin
  refidx := FCount;
  SetCapacity(refidx+1);
  Items[refidx] := o;
  inc(FCount);
end;

procedure TFastList<TYP>.Clear;
begin
  FCount := 0;
  SetCapacity(0);
end;

procedure TFastList<TYP>.Delete(const idx: ni);
var
  pp: PByte;
begin
  pp := PByte(GetPointerToItem(idx));
  MoveMem_Up(pp, pp+sizeof(ni), (FCount-idx)*sizeof(nativeuint));
  dec(FCount);
end;

function TFastList<TYP>.GetCount: ni;
begin
  result := FCount;
end;

function TFastList<TYP>.GetItem(idx: ni): TYP;
begin
  result := PPointer((pbyte(FObjects)+(idx*sizeof(pointer))))^;
end;

function TFastList<TYP>.GetPointerToItem(idx: ni): PPointer;
begin
  result := PPointer((pbyte(FObjects)+(idx*sizeof(pointer))));
end;

function TFastList<TYP>.Has(o: TYP): boolean;
begin
  exit(Indexof(o)>=0);
end;

function TFastList<TYP>.IndexOf(o: TYP): ni;
var
  t: ni;
begin
  for t:= 0 to FCount-1 do begin
    if items[t]=o then
      exit(t);
  end;

  exit(-1);

end;

function TFastList<TYP>.Insert(const idx: ni; o: TYP): ni;
var
  pp: PPointer;
  toMove: ni;
begin
  SetCapacity(FCapacity+1);
  pp := GEtPointerToItem(idx);
  toMove := (FCount-idx)*sizeof(nativeuint);
  MoveMem_Down(Pbyte(pp)+sizeof(ni), pp, toMove);
  pp^ := pointer(o);
  inc(FCount);
end;

function TFastList<TYP>.Remove(o: TYP): ni;
var
  t: ni;
  itm: TYP;
begin
  result := -1;
  for t:= FCount-1 downto 0 do begin
    itm := items[t];
    if itm = o then begin
      delete(t);
      exit(t);
    end;
  end;
end;

procedure TFastList<TYP>.SetCapacity(sz: ni);
begin

  if FObjects <> nil then begin
    if sz = 0 then begin
      Freememory(FObjects);
      FObjects := nil;
    end else begin
      ReallocMem(FObjects, sz*sizeof(pointer));
    end;
  end else begin
    if sz > 0 then
      FObjects := AllocMem(sz*sizeof(pointer))
  end;

  FCapacity := sz;



end;

procedure TFastList<TYP>.SetItem(idx: ni; const Value: TYP);
begin
  GetPointerToItem(idx)^ := pointer(value);
end;
procedure TFastList<TYP>.Sort(const AComparer: IComparer<TYP>);
var
  l: TList<TYP>;
  t: ni;
begin
  l := TList<TYP>.create;
  try
    for t:= 0 to count-1 do begin
      l.Add(items[t]);
    end;
    l.Sort(AComparer);
    FCount := 0;
    for t:= 0 to l.count-1 do begin
      Add(l.items[t]);
    end;
  finally
    l.free;
  end
end;

procedure TFastList<TYP>.Sort;
var
  l: TList<TYP>;
  t: ni;
begin
  l := TList<TYP>.create;
  try
    for t:= 0 to count-1 do begin
      l.Add(items[t]);
    end;
    l.Sort;
    FCount := 0;
    for t:= 0 to l.count-1 do begin
      Add(l.items[t]);
    end;
  finally
    l.free;
  end

end;

end.
