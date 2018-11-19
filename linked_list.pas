unit linked_list;

interface

uses
  typex, debug, sysutils, betterobject, unittest, sharedobject;

{x$DEFINE DBG}  //debugs the paths taken on removal
{x$DEFINE validate_count} //will validate count after every operation
{x$DEFINE HAS_CHECKS} //will validate that the linked list HAS an item. Important for maintaining counts.. but WAY slow.


type
  TDebuggableMorph = class(TBetterObject)
          public
            function GetDebug: string;
          end;

  TLinkage<T> = class(TObject)
  public
    next: TLinkage<T>;
    prev: TLinkage<T>;
    obj: T;
    destructor Destroy;override;
  end;


  IIndirectlyLinkable<T> = interface(IUnknown)
    function GetLinkageFor(obj: TObject): TLinkage<T>;
    procedure AddLinkage(link: TLinkage<T>; list: TObject);
    procedure RemoveLinkage(list: TObject);
  end;

  TDirectlyLinkedList<T_TYPE: TBetterObject> = class(TBetterObject)
  private
    FCount: nativeint;
    function GetItem(idx: nativeint): T_TYPE;
  protected
    cached_index: nativeint;
    cached_link: T_TYPE;
    procedure MoveCacheTo(idx: nativeint);

    procedure ManageFirstLast(objLink: T_TYPE);
    PROCEDURE ClearCache;inline;

  public
    First: T_TYPE;
    Last: T_TYPE;
    VolatileCount: ni;
    procedure Add(const obj: T_TYPE);
    procedure AddList(list: TDirectlyLinkedList<T_TYPE>);
    procedure AddFirst(const obj: T_TYPE);
    procedure Remove(obj: T_TYPE);
    procedure Replace(old, new: T_TYPE);
    property Items[idx: nativeint]: T_TYPE read GetItem;default;
    procedure Delete(idx: nativeint);
    function Has(obj: T_TYPE): boolean;
    property Count: nativeint read FCount;

    function GetDebugString: string;

    procedure Clear;
    function SlowCount(): nativeint;
    procedure ValidateFirstLast;

  end;

  TDirectlyLinkedList_Shared<T_TYPE: TBetterObject> = class(TSharedObject)
  private
    FCount: nativeint;
    function GetItem(idx: nativeint): T_TYPE;inline;
    function GetCount: nativeint;
  protected
    cached_index: nativeint;
    cached_link: T_TYPE;
    procedure MoveCacheTo(idx: nativeint);//

    procedure ManageFirstLast(objLink: T_TYPE);inline;//
    PROCEDURE ClearCache;//do under lock

  public
    First: T_TYPE;
    Last: T_TYPE;
    VolatileCount: ni;
    procedure Add(const obj: T_TYPE);//
    procedure AddList(const list: TDirectlyLinkedList_Shared<T_TYPE>);//
    procedure AddFirst(const obj: T_TYPE);//
    procedure Remove(obj: T_TYPE);//
    procedure Replace(old, new: T_TYPE);//
    property Items[idx: nativeint]: T_TYPE read GetItem;default;//
    procedure Delete(idx: nativeint);
    function Has(obj: T_TYPE): boolean;
    property Count: nativeint read GetCount;

    function GetDebugString: string;

    procedure Clear;
    function SlowCount(): nativeint;
    procedure VAlidateCount;inline;
    procedure ValidateFirstLast;
    function DebugContents: string;

  end;




  TLinkedList<T: IIndirectlyLinkable<T>> = class(TObject)
  type
    TLocalLinkage = TLinkage<T>;
  private
    FCount: nativeint;
    function GetItem(idx: nativeint): T;
  protected
    cached_index: nativeint;
    cached_link: TLocalLinkage;
    procedure MoveCacheTo(idx: nativeint);
    procedure ManageFirstLast(objLink: TLocalLinkage);
    PROCEDURE ClearCache;
  public
    volatilecount: ni;
    First: TLocalLinkage;
    Last: TLocalLinkage;

    procedure Add(obj: T);
    procedure Remove(obj: T);
    property Items[idx: nativeint]: T read GetItem;default;
    procedure Delete(idx: nativeint);
    function Has(obj: T): boolean;
    property Count: nativeint read FCount;

    procedure Clear;
    procedure Sync_DestroyLinkChain;

  end;



implementation

{ TLinkedList<T> }

procedure TLinkedList<T>.Add(obj: T);
var
  linkage: TLocalLinkage;
begin
  linkage := obj.GetLInkageFor(self);
  if linkage <> nil then
    exit;

  linkage := TlocalLinkage.create;
  linkage.obj := obj;

  obj.Addlinkage(linkage, self);


  ManageFirstLast(linkage);//GUARANTEES THAT FIRST/LAST WILL NOT BE NIL

  //put it on der end
  if linkage <> last then begin
    if last <> nil then
      last.next := linkage;
    linkage.prev := last;
    last := linkage;
  end;

  inc(FCount);
  volatileCount := FCount;




end;

procedure TLinkedList<T>.Clear;
begin
{$IFDEF QuICK_CLEAR}//DOES NOT WORK.  No parity with Has() function
  Sync_DestroyLinkChain;
  First := nil;
  Last := nil;
  cached_link := nil;
  cached_index := -1;
  FCount := 0;
  volatilecount := FCount;
{$ELSE}
  while First <> nil do
    Remove(First.obj);
{$ENDIF}


end;

procedure TLinkedList<T>.ClearCache;
begin
  cached_index := -1;
  cached_link := nil;
end;

procedure TLinkedList<T>.Delete(idx: nativeint);
begin
  MoveCacheTo(idx);
  Remove(cached_link.obj);

end;

function TLinkedList<T>.GetItem(idx: nativeint): T;
begin
  MoveCacheTo(idx);
  if cached_link <> nil then
    result := cached_link.obj
  else
    result := nil;
end;

function TLinkedList<T>.Has(obj: T): boolean;
begin
  result := obj.GetLinkageFor(self) <> nil;

end;

procedure TLinkedList<T>.ManageFirstLast(objLink: TLocalLinkage);
begin
  if First = nil then
    First := objlink;

  if Last = nil then
    Last := objlink;

end;

procedure TLinkedList<T>.MoveCacheTo(idx: nativeint);
begin
  if (idx = 0) then begin
    cached_index := 0;
    cached_link := First;
    exit;
  end;

  if (idx = (FCount-1)) then begin
    cached_index := FCount-1;
    cached_link := last;
    exit;
  end;

  if cached_link = nil then
    cached_index := -1;

  if cached_index < 0 then begin
    cached_link := first;
    cached_index := 0;
  end;
  while cached_index < idx do begin
//    Debug.Log(inttostr(cached_index));
    inc(cached_index);
    cached_link := cached_link.next;
    if cached_link = nil then
      raise ECritical.create('seek past end of linked list');
  end;

  while cached_index > idx do begin
//    Debug.Log(inttostr(cached_index));
    dec(cached_index);
    cached_link := cached_link.prev;

    if cached_link = nil then
      raise ECritical.create('seek past start of linked list');
  end;

end;

procedure TLinkedList<T>.Remove(obj: T);
var
  link: TLocalLinkage;
begin
  link := obj.GetLinkageFor(self);
  if link = nil then
    exit;

  obj.RemoveLInkage(self);


  if (link.Prev <> nil ) then begin
    link.Prev.next := link.next;
  end;

  if (link.Next <> nil) then begin
    link.next.prev := link.prev;
  end;

  if link = first then begin
    first := link.next;
    if first <> nil then
      first.prev := nil;
  end;

  if link = last then begin
    last := link.prev;
    if last <> nil then
      last.next := nil;
  end;

  if link = cached_link then begin
    cached_link := link.next;
  end else begin
    cached_link := nil;
    cached_index := -1;
  end;

  link.free;
  link := nil;
  dec(FCount);
  volatilecount := FCount;
  if FCount < 0 then FCount := 0;


end;

procedure TLinkedList<T>.Sync_DestroyLinkChain;
var
  f,l: TLocalLinkage;
begin

  f := First;
  l := Last;

  while f <> nil do begin
    l := F.next;
    f.free;
    f := l;
  end;

end;

{ TDirectlyLinkedList<T> }

procedure TDirectlyLinkedList<T_TYPE>.Add(const obj: T_TYPE);
begin
  ValidateFirstLast;
  if First = nil then begin
    First := obj;
    LAst := obj;
    obj.next := nil;
    obj.prev := nil;
  end else begin
    if assigned(LAst) then
      LAst.Next := obj;

//    if (first.next = nil) then
//      first.next := obj;

    obj.prev := Last;
    LAst := obj;
    obj.next := nil;
  end;


  inc(FCount);
  VolatileCount := FCount;

  ClearCache;
  ValidateFirstLast;

end;

procedure TDirectlyLinkedList<T_TYPE>.AddFirst(const obj: T_TYPE);
begin
  ValidateFirstLast;
  if LAst = nil then begin
    First := obj;
    LAst := obj;
    obj.Next := nil;
    obj.Prev := nil;
  end else begin
//    Debug.ConsoleLog('List '+getdebugstring);
    if assigned(First) then
      First.Prev := obj;


    obj.Next := First;
//    Debug.ConsoleLog('List '+getdebugstring);
    First := obj;
//    Debug.ConsoleLog('List '+getdebugstring);
    obj.Prev := nil;
  end;

  inc(FCount);
  volatilecount := FCount;

  ClearCache;
  ValidateFirstLast;
end;

procedure TDirectlyLinkedList<T_TYPE>.AddList(
  list: TDirectlyLinkedList<T_TYPE>);
var
  mineLAst, listFirst: T_TYPE;
begin
  ValidateFirstLast;
  if list.first = nil then
    exit;
  if last = nil then begin
    first := list.First;
    last := list.Last;
    if FCount > 0 then
      raise ECritical.create('catastrophe');
    inc(FCount, list.count);
    LIST.CLEAR;
  end else begin
    //link first item of IN list to last item of THIS
    mineLAst := last;
    listFirst := list.First;
    mineLast.next := listFirst;
    listFirst.prev := mineLAst;
    last := listFirst;
    inc(FCount, list.count);
    LIST.CLEAR;
  end;
  ValidateFirstLast;
end;

procedure TDirectlyLinkedList<T_TYPE>.Clear;
begin
  First := nil;
  Last := nil;
  cached_link := nil;
  cached_index := -1;
  FCount := 0;
end;

procedure TDirectlyLinkedList<T_TYPE>.ClearCache;
begin
  cached_index := -1;
  cached_link := nil;

end;

procedure TDirectlyLinkedList<T_TYPE>.Delete(idx: nativeint);
begin
  ValidateFirstLast;
  MoveCacheTo(idx);
  Remove(cached_link);
  ValidateFirstLast;
end;

function TDirectlyLinkedList<T_TYPE>.GetDebugString: string;
var
  obj: TBetterObject;
  i: ni;
begin
  result := '';

  i := 0;
  obj := first;
  repeat
    result := result+'['+inttohex(ni(pointer(obj)),1)+']';
    obj := obj.next;
    inc(i);
    if i > 20 then break;

  until obj = nil;


end;

function TDirectlyLinkedList<T_TYPE>.GetItem(idx: nativeint): T_TYPE;
begin
  MoveCacheTo(idx);
  result := T_TYPE((cached_link));

end;

function TDirectlyLinkedList<T_TYPE>.Has(obj: T_TYPE): boolean;
var
  i: ni;
begin
{$IFDEF LINKOWNERS}
  result := obj.linkowner = self;
{$ELSE}
  result := false;
  for i := 0 to count-1 do begin
    if self.Items[i] = obj then begin
      result := true;
      break;
    end;
  end;
{$ENDIF}
end;

procedure TDirectlyLinkedList<T_TYPE>.ManageFirstLast(objLink: T_TYPE);
begin
  if First = nil then
    First := objlink;

  if Last = nil then
    Last := objlink;

end;

procedure TDirectlyLinkedList<T_TYPE>.MoveCacheTo(idx: nativeint);
begin
  if (idx = 0) then begin
    cached_index := 0;
    cached_link := First;
    exit;
  end;

  if (idx = (FCount-1)) then begin
    cached_index := FCount-1;
    cached_link := last;
    exit;
  end;

  if cached_link = nil then
    cached_index := -1;

  if cached_index < 0 then begin
    cached_link := first;
    cached_index := 0;
  end;
  while cached_index < idx do begin
//    Debug.Log(inttostr(cached_index));
    inc(cached_index);
    cached_link := T_TYPE(cached_link.next);
    if cached_link = nil then
      raise ECritical.create('seek past end of linked list @'+inttostr(cached_index)+' count='+inttostr(count));
  end;

  while cached_index > idx do begin
//    Debug.Log(inttostr(cached_index));
    dec(cached_index);
    cached_link := T_TYPE(cached_link.prev);

    if cached_link = nil then
      raise ECritical.create('seek past end of linked list @'+inttostr(cached_index)+' count='+inttostr(count));

  end;
end;

procedure TDirectlyLinkedList<T_TYPE>.Remove(obj: T_TYPE);
begin
  ValidateFirstLast;
{$IFDEF HAS_CHECKS}
  if not HAs(obj) then
    raise ECRitical.create('removing an item that was already removed will cause counting problems!');
{$ENDIF}
  if obj = last then begin
    if last.prev <> nil then begin
      last := T_TYPE(last.prev);
      last.next := nil;
    end
    else begin
      last := nil;
      if first = obj then begin
        first := nil;
      end;
    end;
  end else
  if obj = first then begin
    if first.next <> nil then begin
      first := T_TYPE(first.next);
      first.Prev := nil;
    end
    else begin
      last := nil;
      first := nil;
    end;
  end else begin
    if obj.next<>nil then
      obj.next.prev := obj.prev;
    if obj.prev<>nil then
      obj.prev.next := obj.next;
  end;

  obj.Next := nil;
  obj.prev := nil;
  ClearCache;

  dec(FCount);
  volatilecount := FCount;
  if FCount < 0 then FCount := 0;
  ValidateFirstLast;
end;


procedure TDirectlyLinkedList<T_TYPE>.Replace(old, new: T_TYPE);
begin
  ValidateFirstLast;
  if cached_link = old then
    cached_link := new;
  new.Prev := old.Prev;
  new.next := old.next;
  if new.next <> nil then
    new.next.prev := new;

  if new.prev <> nil then
    new.Prev.Next := new;

  if old = first then
    first := new;
  if old = last then
    last := new;

  if old <> nil then begin
    old.next := nil;
    old.Prev := nil;
  end;
  ValidateFirstLast;

end;

function TDirectlyLinkedList<T_TYPE>.SlowCount: nativeint;
var
  o: TBetterObject;
begin
  result := 0;
  o := First;
  while o <> nil do begin
    inc(result);
    o := o.next;
  end;

end;

procedure TDirectlyLinkedList<T_TYPE>.ValidateFirstLast;
begin
  if (last <> nil) and (first = nil) then
    raise ECritical.create('last is set, but first is not');
  if (first <> nil) and (last = nil) then
    raise ECritical.create('first is set, but last is not');


end;

destructor TLinkage<T>.Destroy;
begin
//  Debug.ConsoleLog('Destroying LInkage');
  inherited;
end;

{ TDirectlyLinkedList_Shared<T_TYPE> }

procedure TDirectlyLinkedList_Shared<T_TYPE>.Add(const obj: T_TYPE);
begin
  lock;
  try
{$IFDEF HAS_CHECKS}
{$IFDEF VALIDATE_COUNT}
    Debug.Log('Begnin Add '+obj.getobjectdebug);
    Debug.Log(DebugContents);
{$ENDIF}
    if HAs(obj) then begin
      Debug.Log(self, 'object '+obj.getobjectdebug+'already in linked list!');
      raise ECRitical.create('object '+obj.getobjectdebug+'already in linked list!');
    end;
{$ENDIF}
    ValidateFirstLast;
    validatecount;

    if First = nil then begin

      First := obj;
      LAst := obj;
      obj.next := nil;
      obj.prev := nil;
    end else begin
      if assigned(LAst) then
        LAst.Next := obj;

  //    if (first.next = nil) then
  //    first.next := obj;

      obj.prev := Last;
      LAst := obj;
      obj.next := nil;
    end;




    inc(FCount);
    VolatileCount := FCount;
{$IFDEF VALIDATE_COUNT}
    Debug.Log('After Add: ');
    Debug.Log(DebugContents);
{$ENDIF}

    validatecount;
    ValidateFirstLast;
    ClearCache;



  finally
    unlock;
  end;

end;

procedure TDirectlyLinkedList_Shared<T_TYPE>.AddFirst(const obj: T_TYPE);
begin
  lock;
  try
{$IFDEF HAS_CHECKS}
    if HAs(obj) then
      raise ECRitical.create('object already in linked list!');
{$ENDIF}
    ValidateFirstLast;
    ValidateCount;
    if LAst = nil then begin
      First := obj;
      LAst := obj;
      obj.Next := nil;
      obj.Prev := nil;
    end else begin
  //    Debug.ConsoleLog('List '+getdebugstring);
      if assigned(First) then
        First.Prev := obj;


      obj.Next := First;
  //    Debug.ConsoleLog('List '+getdebugstring);
      First := obj;
  //    Debug.ConsoleLog('List '+getdebugstring);
      obj.Prev := nil;
    end;

    inc(FCount);
    volatilecount := FCount;
    ClearCache;
    ValidateCount;
    VAlidateFirstLast;

  finally
    unlock;
  end;
end;

procedure TDirectlyLinkedList_Shared<T_TYPE>.AddList(
  const list: TDirectlyLinkedList_Shared<T_TYPE>);
var
  mineLAst, listFirst: T_TYPE;

begin
  lock;
  list.lock;
  try
{$IFDEF VALIDATE_COUNT}
    Debug.Log('Begin Add List '+list.DebugContents);
    Debug.Log('to '+DebugContents);
{$ENDIF}

    ValidateFirstLast;
    ValidateCount;

    if list.first = nil then
      exit;
    if last = nil then begin
{$IFDEF DBG}      Debug.log(self, 'empty list, adding list with count='+inttostr(list.count));{$ENDIF}
      if FCount > 0 then
        raise ECritical.create('catastrophe 2 first='+nativeint(pointer(first)).tostring+' last='+nativeint(pointer(last)).tostring);
      first := list.First;
      last := list.Last;

      Fcount := list.count;
      volatilecount := FCount;
{$IFDEF DBG}      Debug.log(self, 'resulting  count='+inttostr(self.count));{$ENDIF}
      list.clear;
    end else begin
      //link first item of IN list to last item of THIS
{$IFDEF DBG}      Debug.log(self, 'list has items of count='+inttostr(self.count)+', adding list with count='+inttostr(list.count));{$ENDIF}
      mineLAst := last;
      listFirst := list.First;
      mineLast.next := listFirst;   //[1][2][3]-->[a][b][c]
      listFirst.prev := mineLAst;   //[1][2][3]<--[a][b][c]
      last := list.LAst;
      inc(FCount, list.count);
      volatilecount :=FCount;

{$IFDEF DBG}      Debug.log(self, 'resulting  count='+inttostr(self.count));{$ENDIF}

      list.clear;//SOURCE LIST MUST NOT CONTAIN ITEMS AFTER MERGE

    end;
{$IFDEF VALIDATE_COUNT}
    Debug.Log('Add List Results '+DebugContents);
{$ENDIF}
    ValidateCount;
    ValidateFirstLast;
  finally
    list.unlock;
    unlock;
  end;
end;

procedure TDirectlyLinkedList_Shared<T_TYPE>.Clear;
begin
  lock;
  try
    First := nil;
    Last := nil;
    cached_link := nil;
    cached_index := -1;
    FCount := 0;
    volatilecount := FCount;
  finally
    unlock;
  end;
end;

procedure TDirectlyLinkedList_Shared<T_TYPE>.ClearCache;
begin
  cached_index := -1;
  cached_link := nil;

end;

function TDirectlyLinkedList_Shared<T_TYPE>.DebugContents: string;
var
  t: ni;
  o: T_TYPE;
  d: string;
begin
  result := '';
  result := result + ' count:'+Count.tostring+' slowcount:'+slowcount.tostring;
  o := first;
  while o <> nil do begin
    d := '';
    if o = first then d:= d + '(F)';
    if o = last then d:= d + '(L)';
    result := result + '['+d+TDebuggableMorph(o).GetDebug+']';
    o := T_TYPE(o.next)
  end;

end;

procedure TDirectlyLinkedList_Shared<T_TYPE>.Delete(idx: nativeint);
begin
  lock;
  try

    MoveCacheTo(idx);
    Remove(cached_link);

  finally
    unlock;
  end;
end;

function TDirectlyLinkedList_Shared<T_TYPE>.GetCount: nativeint;
begin
  result := FCount;//atomic volatile, use external lock if you want integrity
end;

function TDirectlyLinkedList_Shared<T_TYPE>.GetDebugString: string;
var
  obj: TBetterObject;
  i: ni;
begin
  result := '';

  i := 0;
  obj := first;
  repeat
    result := result+'['+inttohex(ni(pointer(obj)),1)+']';
    obj := obj.next;
    inc(i);
    if i > 20 then break;

  until obj = nil;


end;

function TDirectlyLinkedList_Shared<T_TYPE>.GetItem(idx: nativeint): T_TYPE;
begin
  lock;
  try
    MoveCacheTo(idx);
    result := T_TYPE((cached_link));

  finally
    unlock;
  end;
end;

function TDirectlyLinkedList_Shared<T_TYPE>.Has(obj: T_TYPE): boolean;
var
  i: ni;
begin
{$IFDEF LINKOWNERS}
  result := obj.linkowner = self;
{$ELSE}
  lock;
  try
    result := false;
    for i := 0 to count-1 do begin
      if self.Items[i] = obj then begin
        result := true;
        break;
      end;
    end;

  finally
    unlock;
  end;
{$ENDIF}
end;

procedure TDirectlyLinkedList_Shared<T_TYPE>.ManageFirstLast(objLink: T_TYPE);
begin
  lock;
  try
    if First = nil then
      First := objlink;

    if Last = nil then
      Last := objlink;

  finally
    unlock;
  end;

end;

procedure TDirectlyLinkedList_Shared<T_TYPE>.MoveCacheTo(idx: nativeint);
begin
  lock;
  try
    if (idx = 0) then begin
      cached_index := 0;
      cached_link := First;
      exit;
    end;

    if (idx = (FCount-1)) then begin
      cached_index := FCount-1;
      cached_link := last;
      exit;
    end;

    if cached_link = nil then
      cached_index := -1;

    if cached_index < 0 then begin
      cached_link := first;
      cached_index := 0;
    end;
    while cached_index < idx do begin
  //    Debug.Log(inttostr(cached_index));
      inc(cached_index);
      cached_link := T_TYPE(cached_link.next);
      if cached_link = nil then
        raise ECritical.create('seek past end of linked list @'+inttostr(cached_index)+' count='+inttostr(count));
    end;

    while cached_index > idx do begin
  //    Debug.Log(inttostr(cached_index));
      dec(cached_index);
      cached_link := T_TYPE(cached_link.prev);

      if cached_link = nil then
        raise ECritical.create('seek past end of linked list @'+inttostr(cached_index)+' count='+inttostr(count));

    end;

  finally
    unlock;
  end;

end;

procedure TDirectlyLinkedList_Shared<T_TYPE>.Remove(obj: T_TYPE);
begin

  lock;
  try
{$IFDEF HAS_CHECKS}
    if not HAs(obj) then
      raise ECRitical.create('removing an item that was already removed will cause counting problems!');
{$ENDIF}

    ValidateFirstLast;
    validatecount;
{$IFDEF VALIDATE_COUNT}
    Debug.Log('Begin Remove '+obj.getobjectdebug);
{$ENDIF}



    if obj = last then begin
//      if Count = 2 then begin
//        Debug.Consolelog('here');
//      end;
{$IFDEF DBG}      DEbug.log(self, '1');{$ENDIF}
      if last.prev <> nil then begin
{$IFDEF DBG}        DEbug.log(self, '1a');{$ENDIF}
        last := T_TYPE(last.prev);
        last.next := nil;
      end
      else begin
{$IFDEF DBG}        DEbug.log(self, '1b');{$ENDIF}
        last := nil;
        if first = obj then begin
          first := nil;
        end;

      end;
    end else
    if obj = first then begin
{$IFDEF DBG}      DEbug.log(self, '2');{$ENDIF}
      if first.next <> nil then begin
{$IFDEF DBG}        DEbug.log(self, '2a');{$ENDIF}
        first := T_TYPE(first.next);
        first.Prev := nil;
      end
      else begin
{$IFDEF DBG}        DEbug.log(self, '2b');{$ENDIF}
        last := nil;
        first := nil;
      end;
    end else begin
{$IFDEF DBG}      DEbug.log(self, '3');{$ENDIF}
      if obj.next<>nil then
        obj.next.prev := obj.prev;
      if obj.prev<>nil then
        obj.prev.next := obj.next;
    end;

    obj.Next := nil;
    obj.prev := nil;
    ClearCache;


    dec(FCount);
    volatilecount := FCount;
    if FCount < 0 then FCount := 0;
    validatecount;
    ValidateFirstLast;
  finally
{$IFDEF VALIDATE_COUNT}
    Debug.Log('End Remove '+obj.getobjectdebug);
{$ENDIF}
    unlock;
  end;
end;

procedure TDirectlyLinkedList_Shared<T_TYPE>.Replace(old, new: T_TYPE);
begin
  lock;
  try
    ValidateFirstLast;
    validatecount;
    if cached_link = old then
      cached_link := new;
    new.Prev := old.Prev;
    new.next := old.next;
    if new.next <> nil then
      new.next.prev := new;

    if new.prev <> nil then
      new.Prev.Next := new;

    if old = first then
      first := new;
    if old = last then
      last := new;

    if old <> nil then begin
      old.next := nil;
      old.Prev := nil;
    end;

    validatecount;
    ValidateFirstLast;

  finally
    unlock;
  end;
end;

function TDirectlyLinkedList_Shared<T_TYPE>.SlowCount: nativeint;
var
  o: TBetterObject;
begin
  lock;
  try
    result := 0;
    o := First;
    while o <> nil do begin
      inc(result);
      o := o.next;
    end;
  finally
    unlock;
  end;
end;

procedure TDirectlyLinkedList_Shared<T_TYPE>.VAlidateCount;
var
  t: ni;
begin
{$IFDEF VALIDATE_COUNT}
  Lock;
  try
    if (FCount = 0) and (first <> nil) then
      raise ECritical.create('count integrity error');

    if (FCount <> 0) and (first = nil) then
      raise ECritical.create('count integrity error');


    if (FCount = 1) and (first <> last) then
      raise ECritical.create('first should = last if count=1');

    if SlowCount <> FCount then
      raise ECritical.create('count does not match FCount '+inttostr(FCount));


    for t:= 0 to count-1 do begin
      if items[t] = nil then
        raise ECritical.create('count integrity error @'+inttostr(t));
    end;

  finally
    Unlock;
  end;
{$ENDIF}
end;

procedure TDirectlyLinkedList_Shared<T_TYPE>.ValidateFirstLast;
begin
  if (last <> nil) and (first = nil) then
    raise ECritical.create('last is set, but first is not');
  if (first <> nil) and (last = nil) then
    raise ECritical.create('first is set, but last is not');
end;

{ TDebuggableMorph }

function TDebuggableMorph.GetDebug: string;
var
  sPrev: string;
  sNExt: string;
begin
  sPrev := '';
  sNExt := '';
  if prev <> nil then
    sPrev := TDebuggableMorph(prev).Classname+'@'+(ni(pointer(prev))).tostring+'<--';
  if next <> nil then
    sNExt := '-->'+TDebuggableMorph(next).Classname+'@'+ni(pointer(next)).tostring;


  result := sPRev+ClassName+'@'+ni(pointer(self)).tostring+sNext;
end;

initialization

end.
