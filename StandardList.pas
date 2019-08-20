unit StandardList;

interface

uses
  typex, systemx, sysutils, betterobject, sharedobject, generics.collections.fixed, system.generics.defaults, search, better_collections;
type

  IIndexable<T_IDX> = interface
    function GetIndexValue: T_IDX;
    property IndexValue: T_IDX read GetIndexValue;
  end;

  TStandardList<T_IDX; T_OBJ: class, IIndexable<T_IDX>> = class(TSharedObject)
  private
    function GetItemByIndex(idx: T_IDX): T_OBJ;
  protected
    FList: TBetterList<T_OBJ>;
    FOwnsObjects: boolean;
    function GetItem(idx: nativeint): T_OBJ;
    procedure SetItem(idx: nativeint; const Value: T_OBJ);
    function GetCount: nativeint;

  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Detach;override;
    property OwnsObjects: boolean read FOwnsObjects write FOwnsObjects;
    property Items[idx: nativeint]: T_OBJ read GetItem write SetItem;default;
    property ItemsByIndex[idx: T_IDX]: T_OBJ read GetItemByIndex;
    procedure AfterAdd(p: T_OBJ);virtual;
    procedure AfterRemove(p: T_OBJ);virtual;
    procedure Add(p: T_OBJ);virtual;
    procedure Remove(p: T_OBJ);
    procedure Delete(idx: nativeint);
    function Find(p: T_IDX): T_OBJ;overload;
    function Find(p: T_OBJ): T_OBJ;overload;
    function IndexOf(p: T_IDX): nativeint;overload;
    function IndexOf(p: T_OBJ): nativeint;overload;
    function Has(p: T_IDX): boolean;overload;
    function Has(p: T_OBJ): boolean;overload;
    procedure FreeAndClear;
    procedure Clear;
    property Count: nativeint read GetCount;
    function HighestIndex: T_IDX;
    function LowestIndex: T_IDX;
  end;

  TReferencedList<T_IDX; T_OBJ: class, IIndexable<T_IDX>> = class(TStandardList<T_IDX, T_OBJ>)
    procedure AfterAdd(p: T_OBJ);override;
    procedure AfterRemove(p: T_OBJ);override;
  end;


  TBinarySearchableList<T_OBJ:class,IIndexable<int64>> = class(TStandardList<int64, T_OBJ>)
  protected
    function IndexOf_Binary(p: int64): nativeint;
  public


  end;

  TStandardOwnedList<T_IDX; T_OBJ: class, IIndexable<T_IDX>> = class(TStandardList<T_IDX, T_OBJ>)
  public
    constructor Create;override;
  end;



implementation


{ TStandardList<T_OBJ> }



{ TStandardList<T_IDX, T_OBJ> }

procedure TStandardList<T_IDX, T_OBJ>.Add(p: T_OBJ);
begin
  Lock;
  try
    FList.add(p);
    AfterAdd(p);
  finally
    Unlock;
  end;

end;

procedure TStandardList<T_IDX, T_OBJ>.AfterAdd(p: T_OBJ);
begin
//
end;

procedure TStandardList<T_IDX, T_OBJ>.AfterRemove(p: T_OBJ);
begin
//
end;

procedure TStandardList<T_IDX, T_OBJ>.Clear;
begin
  Lock;
  try
    while Count > 0 do begin
      Remove(Flist[Count-1]);
    end;
  finally
    Unlock;
  end;


end;

constructor TStandardList<T_IDX, T_OBJ>.CReate;
begin
  inherited;
  FList := TBetterList<T_OBJ>.create;
end;

procedure TStandardList<T_IDX, T_OBJ>.Delete(idx: nativeint);
var
  tt: T_OBJ;
begin
  Lock;
  try
    if ownsobjects then begin
      tt := FList[idx];
    end;
    Flist.delete(idx);
    AfterRemove(tt);
    if ownsobjects then begin
      tt.free;
      tt := nil;
    end;
  finally
    Unlock;
  end;
end;

destructor TStandardList<T_IDX, T_OBJ>.Destroy;
begin

  inherited;
end;

procedure TStandardList<T_IDX, T_OBJ>.Detach;
begin
  Clear;
  FList.free;
  FList := nil;
  inherited;

end;

function TStandardList<T_IDX, T_OBJ>.Find(p: T_IDX): T_OBJ;
var
  i: nativeint;
begin
  result := nil;
  Lock;
  try
    i := IndexOf(p);
    if i >= 0 then
      result := FList[i];
  finally
    Unlock;
  end;
end;

function TStandardList<T_IDX, T_OBJ>.Find(p: T_OBJ): T_OBJ;
var
  i: nativeint;
begin
  result := nil;
  Lock;
  try
    i := IndexOf(p);
    if i >= 0 then
      result := FList[i];
  finally
    Unlock;
  end;
end;

procedure TStandardList<T_IDX, T_OBJ>.FreeAndClear;
var
  o: T_OBJ;
begin
  Lock;
  try
    while Count > 0 do begin
      o := Flist[Count-1];
      if ownsobjects then begin
        o.free;
        Remove(o);
      end;
    end;
  finally
    Unlock;
  end;
end;

function TStandardList<T_IDX, T_OBJ>.GetCount: nativeint;
begin
  Lock;
  try
    result := FList.Count;
  finally
    Unlock;
  end;
end;

function TStandardList<T_IDX, T_OBJ>.GetItem(idx: nativeint): T_OBJ;
begin
  Lock;
  try
    result := FList[idx];
  finally
    Unlock;
  end;
end;

function TStandardList<T_IDX, T_OBJ>.GetItemByIndex(idx: T_IDX): T_OBJ;
begin
  result := Find(idx);

end;

function TStandardList<T_IDX, T_OBJ>.Has(p: T_IDX): boolean;
begin
  result := IndexOf(p) >= 0;

end;

function TStandardList<T_IDX, T_OBJ>.has(p: T_OBJ): boolean;
begin
  result := FList.IndexOf(p) >= 0;
end;

function TStandardList<T_IDX, T_OBJ>.HighestIndex: T_IDX;
var
  t: integer;
  a: T_IDX;
  comp: IComparer<T_IDX>;
begin
  Lock;
  try
    if count = 0 then
      raise Exception.create('check for count of 0 before calling highestindex');

    result := FList[0].IndexValue;
    comp := TComparer<T_IDX>.Default;

    for t:= 1 to count-1 do begin
      a := FList[t].IndexValue;
      if comp.Compare(result, a) > 0 then begin
        result := a;
      end;


    end;
  finally
    Unlock;
  end;
end;

function TStandardList<T_IDX, T_OBJ>.IndexOf(p: T_IDX): nativeint;
var
  t: nativeint;
  o: T_OBJ;
  comp: IEqualityComparer<T_IDX>;
begin
  result := -1;
  Lock;
  try
    comp := TEqualityComparer<T_IDX>.Default;
    for t:= 0 to Count-1 do begin
      o := FList[t];

      if (comp.Equals(o.IndexValue, p)) then begin
        result := t;
        break;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TStandardList<T_IDX, T_OBJ>.IndexOf(p: T_OBJ): nativeint;
begin
  Lock;
  try
    result := FList.IndexOf(p);
  finally
    Unlock;
  end;
end;


function TStandardList<T_IDX, T_OBJ>.LowestIndex: T_IDX;
var
  t: integer;
  a: T_IDX;
  comp: IComparer<T_IDX>;
begin
  Lock;
  try
    if count = 0 then
      raise Exception.create('check for count of 0 before calling highestindex');

    result := FList[0].IndexValue;
    comp := TComparer<T_IDX>.Default;

    for t:= 1 to count-1 do begin
      a := FList[t].IndexValue;
      if comp.Compare(result, a) < 0 then begin
        result := a;
      end;


    end;
  finally
    Unlock;
  end;
end;

procedure TStandardList<T_IDX, T_OBJ>.Remove(p: T_OBJ);
begin
  Lock;
  try
    fList.Remove(p);
    AfterRemove(p);
    if OwnsObjects then
      p.free;



  finally
    Unlock;
  end;
end;

procedure TStandardList<T_IDX, T_OBJ>.SetItem(idx: nativeint; const Value: T_OBJ);
begin
  Lock;
  try
    FList[idx] := value;
  finally
    Unlock;
  end;

end;


{ TStandardOwnedList<T_IDX, T_OBJ> }

constructor TStandardOwnedList<T_IDX, T_OBJ>.Create;
begin
  inherited;
  OwnsObjects := true;
end;

{ TBinarySearchableList<T_OBJ> }

function TBinarySearchableList<T_OBJ>.IndexOf_Binary(p: int64): nativeint;
begin
  raise ECritical.create('Not implemented');
end;

{ TReferencedList<T_IDX, T_OBJ> }

procedure TReferencedList<T_IDX, T_OBJ>.AfterAdd(p: T_OBJ);
begin
  inherited;
  p._AddRef;
end;

procedure TReferencedList<T_IDX, T_OBJ>.AfterRemove(p: T_OBJ);
begin
  inherited;
  p._Release;

end;

end.
