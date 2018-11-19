unit ClusteredPointerList;

interface

uses
  generics.collections.fixed, betterobject;


const
  CLUSTERS = 64;
type
  TClusteredPointerList<T:class> = class(TBetterObject)
  private
    FListList: TList<TList<T>>;
    function GetListNumberForObject(obj: T): nativeint;
    function GetCount: nativeint;
    function GEtListNumberForIndex(idx: nativeint): nativeint;
    function GetListBase(iListNumber: nativeint): nativeint;
    function GetItem(idx: nativeint): T;
    procedure SetItem(idx: nativeint; const Value: T);
  public
    procedure Init;override;
    procedure Detach;override;
    destructor Destroy;override;

    procedure Add(obj: T);
    property Count: nativeint read GetCount;
    function IndexOf(obj: T): nativeint;
    property Items[idx: nativeint]: T read GetItem write SetItem;default;
    procedure REmove(obj: T);
    procedure Clear;
    procedure Delete(idx: nativeint);

  end;

implementation

{ TClusteredPointerList<T> }

procedure TClusteredPointerList<T>.Add(obj: T);
var
  nl: nativeint;
begin

  nl := GetListNumberForObject(obj);
  FListlist[nl].add(obj);





end;

procedure TClusteredPointerList<T>.Clear;
var
  l: TList<T>;
  x: nativeint;
begin
  inherited;
  for x := 0 to CLUSTERS-1 do begin
    l := FListlist[x];
    l.Clear;
  end;
end;

procedure TClusteredPointerList<T>.Delete(idx: nativeint);
var
  ln, base, sub: nativeint;
begin
  ln :=  Self.GEtListNumberForIndex(idx);
  base := GetListBase(ln);
  sub := idx-base;
  FListList[ln].delete(sub);

end;

destructor TClusteredPointerList<T>.Destroy;
var
  l: TLIst<T>;
begin

  inherited;
  while FListList.count > 0 do begin
    l := FListlist[FlistLIst.count-1];
    l.free;
    l := nil;
    FListlist.Delete(FListlist.count-1);
  end;

  FListList.free;
end;

procedure TClusteredPointerList<T>.Detach;
begin
  Clear;
  inherited;


end;

function TClusteredPointerList<T>.GetCount: nativeint;
var
  x: nativeint;
begin
  result := 0;
  for x := 0 to CLUSTERS-1 do begin
    inc(result, FListList[x].Count);
  end;

end;

function TClusteredPointerList<T>.GetItem(idx: nativeint): T;
var
  ln, base, sub: nativeint;
begin
  ln :=  Self.GEtListNumberForIndex(idx);
  base := GetListBase(ln);
  sub := idx-base;
  result := FListList[ln][sub];

end;

function TClusteredPointerList<T>.GetListBase(
  iListNumber: nativeint): nativeint;
var
  x: nativeint;
begin
  result := 0;
  for x := 0 to iListNumber-1 do begin
    result := result + FListLIst[x].count;
  end;

end;

function TClusteredPointerList<T>.GEtListNumberForIndex(
  idx: nativeint): nativeint;
var
  x: nativeint;
  base: nativeint;
begin
  inherited;
  result := -1;
  base := 0;
  for x := 0 to CLUSTERS-1 do begin
    if idx < (FListList[x].Count + base) then begin
      result := x;
      exit;
    end;
    inc(base, FListList[x].count);
  end;
end;

function TClusteredPointerList<T>.GetListNumberForObject(obj: T): nativeint;
var
  p: pointer;
  ni: nativeint;
begin
  p := pointer(obj);
  ni := (nativeint(p) div 16) mod CLUSTERS;

  result := ni;

end;

function TClusteredPointerList<T>.IndexOf(obj: T): nativeint;
var
  ln: nativeint;
begin
  ln := GetListNumberForObject(obj);
  result := FListList[ln].IndexOf(obj);
  if result < 0 then begin
    result := -1;
    exit;
  end else begin
    result := result+ GetListBase(ln);
  end;

end;

procedure TClusteredPointerList<T>.Init;
var
  x: nativeint;
begin
  inherited;
  FListList := TList<TList<T>>.create;
  for x := 0 to CLUSTERS-1 do begin
    FListList.add(TList<T>.create);
  end;

end;

procedure TClusteredPointerList<T>.REmove(obj: T);
var
  ln: nativeint;
begin
  ln := GetListNumberForObject(obj);
  if ln >=0 then
    FListList[ln].Remove(obj);


end;

procedure TClusteredPointerList<T>.SetItem(idx: nativeint; const Value: T);
var
  ln, base, sub: nativeint;
begin
  ln :=  Self.GEtListNumberForIndex(idx);
  base := GetListBase(ln);
  sub := idx-base;
  FListList[ln][sub] := value;
end;

end.
