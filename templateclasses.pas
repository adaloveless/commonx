unit templateclasses;
{$MESSAGE '*******************COMPILING templateclasses.pas'}

{x$INLINE AUTO}
{x$DEFINE VALIDATE_INDEXES}
interface

uses classes, betterobject, sysutils, typex, systemx, Generics.Collections, sharedobject, debug;

type
  TListOf<T> = class(TList<T>);

  TSharedList<T: class> = class(TSharedObject)
  private
    FList: TList<T>;
    function GetCount: integer;
    function GetItem(idx: integer): T;
    procedure SetItem(idx: integer; Value: T);

  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Add(obj: T);
    procedure Remove(obj: T);
    function Has(obj: T): boolean;
    function IndexOf(obj: T): integer;
    property Items[idx: integer]:T read GetItem write SetItem;default;
    procedure Delete(idx: integer);
    procedure FreeItems;
    procedure Clear;
    property Count: integer read GetCount;
  end;

  TLinkedItem<T: class> = class(TBetterObject)
  public

    Prev: TLinkedItem<T>;
    Next: TLinkedItem<T>;
    Index: nativeint;
    Ptr: T;
    constructor Create;override;
  end;


  TLinkedList<T: class> = class(TBetterObject)
  private
    FCount: nativeint;
    function GetItems(idx: nativeint): T;
    function GetLinkedItems(idx: nativeint): TLInkedItem<T>;
  protected
    IndexOutofDate: boolean;
    Head: TLinkedItem<T>;
    current: TLinkedItem<T>;
    Tail: TLinkedItem<T>;
    FReindexOrigin: TLinkedItem<T>;
    procedure SetReindexOrigin(itm: TLInkedItem<T>);
    procedure ValidateIndex;
  public
    procedure Add(obj: T);
    procedure Remove(li: TLinkedItem<T>);overload;
    procedure Remove(obj: T);overload;
    function FindLInkedItem(obj: T): TLinkedItem<T>;
    procedure Reindex();

    property Count: nativeint read FCount;

    property LInkedItems[idx: nativeint]: TLInkedItem<T> read GetLinkedItems;
    property Items[idx: nativeint]: T read GetItems;default;
    procedure Delete(idx: nativeint);
    function IndexOF(obj: T): nativeint;
    procedure CLear;
  end;


implementation


{ TSharedList<T> }

procedure TSharedList<T>.Add(obj: T);
begin
  Lock;
  try
    FList.add(obj);
  finally
    Unlock;
  end;

end;

procedure TSharedList<T>.Clear;
begin
  Lock;
  try
    FList.clear;
  finally
    Unlock;
  end;
end;

constructor TSharedList<T>.Create;
begin
  inherited;
  FList := TList<T>.create;
end;

procedure TSharedList<T>.Delete(idx: integer);
begin
  Lock;
  try
      FList.delete(idx);
  finally
    Unlock;
  end;
end;

destructor TSharedList<T>.Destroy;
begin
  FList.free;
  inherited;
end;

procedure TSharedList<T>.FreeItems;
begin
  Lock;
  try
    while (Count > 0) do begin
      FList[Count-1].Free;
      FList.delete(count-1);
    end;

  finally
    Unlock;
  end;
end;

function TSharedList<T>.GetCount: integer;
begin
  Lock;
  try
    result := FList.count;
  finally
    Unlock;
  end;
end;

function TSharedList<T>.GetItem(idx: integer): T;
begin
  Lock;
  try
    result := FList[idx];
  finally
    Unlock;
  end;
end;

function TSharedList<T>.Has(obj: T): boolean;
begin
  result := IndexOf(obj) >= 0;
end;

function TSharedList<T>.IndexOf(obj: T): integer;
begin
  Lock;
  try
    result := FList.indexof(obj);
  finally
    Unlock;
  end;
end;

procedure TSharedList<T>.Remove(obj: T);
begin
  Lock;
  try
    FList.Remove(obj);
  finally
    Unlock;
  end;
end;

procedure TSharedList<T>.SetItem(idx: integer; Value: T);
begin
  Lock;
  try
    FList[idx] := value;
  finally
    Unlock;
  end;
end;

{ TLinkedList<T> }

procedure TLinkedList<T>.Add(obj: T);
var
  li: TLinkedItem<T>;
begin
  //create item
  li := TLinkedItem<T>.Create;
  li.Ptr := obj;

  //if there's no head, then make this the head
  if head = nil then
    self.head := li;


  //put it after the tail (shoudl be nil if empty)
  li.Prev := Tail;
  if self.Tail <> nil then begin
    self.Tail.Next := li;
  end;

  //make this the tail, always
  self.Tail := li;

  //build index if out of date
  if IndexOutOfDate then
    reindex();

  //set the index of the item
  if li.Prev <> nil then
    li.Index := li.Prev.Index +1;

  //record count
  inc(FCount);


end;

procedure TLinkedList<T>.CLear;
begin
  while head <> nil do begin
    Remove(head);
  end;
  Current := nil;
  Head := nil;
  Tail := nil;
end;

procedure TLinkedList<T>.Delete(idx: nativeint);
var
  li : TLinkedItem<T>;
begin
  li := linkeditems[idx];
  if li <> nil then begin
    Remove(li);
  end;

end;

function TLinkedList<T>.FindLInkedItem(obj: T): TLinkedItem<T>;
var
  li: TLinkedItem<T>;
begin
  result := nil;
  li := Head;
  while li <> nil do begin
    //if pointer(li.Ptr) = pointer(obj) then begin
    if TObject(li.Ptr) = TObject(obj) then begin
      result := li;
      break;
    end;
    li := li.Next;

  end;
end;

function TLinkedList<T>.GetItems(idx: nativeint): T;
var
  li: TLinkedItem<T>;
begin

  result := nil;

  li := LInkedItems[idx];

  if li <> nil then
    result := li.Ptr
  else
    raise Exception.Create('Item not found at index '+inttostr(idx));


end;

function TLinkedList<T>.GetLinkedItems(idx: nativeint): TLInkedItem<T>;
begin
  result := nil;
  if IndexOutofDate then
    Reindex;

  if Current = nil then
    Current := head;

  while (current <> nil) and (current.Index > idx) do
    current := current.prev;

  while (current <> nil) and (current.Index < idx) do
    current := current.next;

  result := current;

end;

function TLinkedList<T>.IndexOF(obj: T): nativeint;
var
  li: TLinkedItem<T>;
begin
  li := FindLInkedItem(obj);
  if li = nil then
    result := -1
  else
    result := li.Index;
end;

procedure TLinkedList<T>.Reindex();
var
  li: TLinkedItem<T>;
  idx: nativeint;
begin
  if FReindexorigin = nil then begin
    li := head;
    if li = nil then exit;
    idx := 0;
  end else begin
    li := FReindexorigin;
    if li = nil then exit;
    if li = head then
      idx := 0
    else
      idx := li.index;
  end;

  while li <> nil do begin
    li.Index := idx;
    inc(idx);
    li := li.Next;
  end;

  IndexOutofDate := false;
  FReindexOrigin := nil;
{$IFDEF VALIDATE_INDEXES}
  ValidateIndex;
{$ENDIF}

end;

procedure TLinkedList<T>.Remove(obj: T);
var
  li: TLInkedItem<T>;
begin
  li := FindLInkedItem(obj);
  if li <> nil then begin
    Remove(li);
  end;

end;

procedure TLinkedList<T>.SetReindexOrigin(itm: TLInkedItem<T>);
begin
  if itm = nil then begin
    itm := head;
    if itm <> nil then
      itm.index := 0;
  end;
  if not assigned(itm) then
    exit;

  if not assigned(FReindexOrigin) then begin
    FreindexOrigin := itm;
//    Debug.Log('~Reindex @'+inttostr(FReindexOrigin.index));
  end else begin
    if itm.Index <= FReindexOrigin.Index then begin
      FReindexOrigin := itm;
//      Debug.Log('^Reindex @'+inttostr(FReindexOrigin.index));
    end;
  end;
end;

procedure TLinkedList<T>.ValidateIndex;
var
  li: TLinkedItem<T>;
  chk: nativeint;
begin
  li := head;
  if li = nil then exit;

  chk := 0;
  while li <> nil do begin

    if li.index <> chk then
      raise Exception.create('index validation failure @'+inttostr(chk))
    else
      Debug.Log('Good @'+inttostr(chk));
    inc(chk);
    li := li.next;
  end;

end;

procedure TLinkedList<T>.Remove(li: TLInkedItem<T>);
begin
  if li<> nil then begin
    //if this is the head, then change the head to the next
    if li = head then begin
      head := head.Next;
    end;
    //if this is the tail, then change the tail to the previous
    if li = tail then
      tail := tail.Prev;

    //if there's a previous, then point the previous to the next
    if li.Prev <> nil then
      li.Prev.Next := li.Next;
    //if there's a next, then point the next to the previous
    if li.Next <> nil then
      li.Next.Prev := li.Prev;




    current := li;

    //clear the current
    if li=current then begin
      if assigned(current.prev) then
        current := current.prev
      else
        current := head;
    end;

    //flag the index as out of date
    IndexOutofdate := true;
    SetReindexOrigin(li.prev);

   //free the item, this does not free the object
    li.Free;
  end else
    raise Exception.Create('Cannot remove Nil '+li.ClassName);

  dec(FCount);

end;

{ TLinkedItem<T> }

constructor TLinkedItem<T>.Create;
begin
  inherited;
  Prev := nil;
  Next := nil;
  Index := 0;
  PTr := nil;
end;

end.

