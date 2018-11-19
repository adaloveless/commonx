unit linked_list_btree_compatible;

interface

uses
  typex, debug, sysutils, betterobject, unittest;

type
  TLinkage<T> = class(TObject)
  public
    next: TLinkage<T>;
    prev: TLinkage<T>;
    obj: T;
  end;

  TLinkable = class(TObject)
  public
    NExt: TObject;
    Prev: TObject;
    Tree: TObject;
    list: TObject;
    constructor CReate;reintroduce;virtual;

  end;

  TDirectlyLinkedList<T: TLinkable> = class(TBetterObject)
  private
    FCount: nativeint;
    function GetItem(idx: nativeint): T;
  protected
    cached_index: nativeint;
    cached_link: T;
    procedure MoveCacheTo(idx: nativeint);

    procedure ManageFirstLast(objLink: T);
    PROCEDURE ClearCache;

  public
    FirstItem: T;
    LastItem: T;

    procedure Add(obj: T);
    procedure AddFirst(obj: T);
    procedure Remove(obj: T; bDontFree: boolean);
    procedure Replace(old, new: T);
    property Items[idx: nativeint]: T read GetItem;default;
    procedure Delete(idx: nativeint);
    function Has(obj: T): boolean;
    property Count: nativeint read FCount;

    procedure Clear;
    procedure ClearBruteForce;

  end;









implementation



procedure TDirectlyLinkedList<T>.Add(obj: T);
begin
  if FirstItem = nil then begin
    FirstItem := obj;
    LAstItem := obj;
    obj.next := nil;
    obj.prev := nil;
  end else begin
    if assigned(LAstItem) then
      LAstItem.Next := obj;

    obj.prev := LastItem;
    LAstItem := obj;
    obj.next := nil;
  end;


  if obj.list = nil then begin
    inc(FCount);
    obj.list := self;
  end;


end;

procedure TDirectlyLinkedList<T>.AddFirst(obj: T);
begin
  if LAstItem = nil then begin
    FirstItem := obj;
    LAstItem := obj;
    obj.Next := nil;
    obj.Prev := nil;
  end else begin
    if assigned(FirstItem) then
      FirstItem.Prev := obj;


    obj.Next := FirstItem;
    FirstItem := obj;
    obj.Prev := nil;
  end;


  if obj.list = nil then begin
    inc(FCount);
    obj.list := self;
  end;


end;

procedure TDirectlyLinkedList<T>.Clear;
begin
  FirstItem := nil;
  LastItem := nil;
  cached_link := nil;
  cached_index := -1;
  FCount := 0;
end;

procedure TDirectlyLinkedList<T>.ClearBruteForce;
begin
  Clear;
end;

procedure TDirectlyLinkedList<T>.ClearCache;
begin
  cached_index := -1;
  cached_link := nil;

end;

procedure TDirectlyLinkedList<T>.Delete(idx: nativeint);
begin
  MoveCacheTo(idx);
  Remove(cached_link,true);
end;

function TDirectlyLinkedList<T>.GetItem(idx: nativeint): T;
begin
  MoveCacheTo(idx);
  result := (cached_link);

end;

function TDirectlyLinkedList<T>.Has(obj: T): boolean;
var
  i: ni;
begin
  result := false;
  for i := 0 to count-1 do begin
    if self.Items[i] = obj then begin
      result := true;
      break;
    end;
  end;
end;

procedure TDirectlyLinkedList<T>.ManageFirstLast(objLink: T);
begin
  if FirstItem = nil then
    FirstItem := objlink;

  if LastItem = nil then
    LastItem := objlink;

end;

procedure TDirectlyLinkedList<T>.MoveCacheTo(idx: nativeint);
begin
  if (idx = 0) then begin
    cached_index := 0;
    cached_link := FirstItem;
    exit;
  end;

  if (idx = (FCount-1)) then begin
    cached_index := FCount-1;
    cached_link := lastItem;
    exit;
  end;

  if cached_link = nil then
    cached_index := -1;

  if cached_index < 0 then begin
    cached_link := firstItem;
    cached_index := 0;
  end;
  while cached_index < idx do begin
//    Debug.Log(inttostr(cached_index));
    inc(cached_index);
    cached_link := T(cached_link.next);
    if cached_link = nil then
      raise ECritical.create('seek past end of linked list @'+inttostr(cached_index)+' count='+inttostr(count));
  end;

  while cached_index > idx do begin
//    Debug.Log(inttostr(cached_index));
    dec(cached_index);
    cached_link := T(cached_link.prev);

    if cached_link = nil then
      raise ECritical.create('seek past end of linked list @'+inttostr(cached_index)+' count='+inttostr(count));

  end;
end;

procedure TDirectlyLinkedList<T>.Remove(obj: T; bDontFree: boolean);
begin
  if obj = lastItem then begin
    if lastitem.prev <> nil then begin
      lastitem := T(lastitem.prev);
      lastitem.next := nil;
    end
    else begin
      lastitem := nil;
      firstitem := nil;
    end;
  end else
  if obj = firstitem then begin
    if firstitem.next <> nil then begin
      firstitem := T(firstitem.next);
      firstitem.Prev := nil;
    end
    else begin
      lastitem := nil;
      firstitem := nil;
    end;
  end else begin
    if obj.next<>nil then
      T(obj.next).prev := obj.prev;
    if obj.prev<>nil then
      T(obj.prev).next := obj.next;
  end;

  obj.Next := nil;
  obj.prev := nil;
  ClearCache;

  ClearCache;

  if not bDontFree then
    obj.free;

  if obj.list = self then begin
    dec(FCount);
    obj.list := nil;
  end;

end;


procedure TDirectlyLinkedList<T>.Replace(old, new: T);
begin
  if cached_link = old then
    cached_link := new;
  new.Prev := old.Prev;
  new.next := old.next;
  if new.next <> nil then
    T(new.next).prev := new;

  if new.prev <> nil then
    T(new.Prev).Next := new;

  if old = firstitem then
    firstitem := new;
  if old = lastitem then
    lastitem := new;

  if old <> nil then begin
    old.next := nil;
    old.Prev := nil;
  end;

  new.list := old.list;
  old.list := nil;


end;



{ TLinkable }

constructor TLinkable.CReate;
begin
  //
end;

end.
