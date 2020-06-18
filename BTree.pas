unit BTree;
//About this unit
//This unit contains a heavily modified "AVL" Binary tree.
//It is "heavily" modified in that it is, in fact, not strictly
//a binary tree anymore.
//
//In a traditional binary tree each node has a left and a right node.
//The Left node contains the next lower index entry in the index
//The right node contains the next greater index entry
//Duplicate index values are not allowed.
//
//However, in this tree contains actually 3 NODES
//- Left as above
//- Right as above
//- and Center: which contains a LINKED LIST of items with duplicate key entries.
//The order of the items in the center list is arbitrary and not guaranteed to
//remain constant as the sorting of these nodes is technically undefined
//
//Search time remains log(n) as long as the tree contains unique values
//however, in the case of duplicate keys, it will be log(n)+number of duplicates on a particular node
//
//By using a linked list, duplicate items can be removed and added with basically
//just a couple of memory writes
//
//Iterate() works just as you would expect.
//In a traditional AVL Binary Tree, you would walk the left node, current node, then right node
//In this modified tree, Iterate() walks left, center, then right
//
//This tree is pointer-perfect, in that you can guarantee that pointers/objects
//you add to it follow their nodes when sorted, and nodes are never copied by value
//Many college-book examples are not pointer perfect due to one of the complex
//rotations that comes up occasionally.
//
//This Tree is ALSO GENERIC, but the nodes in the tree should inherit from
//  TBTreeItem
//
//If you want to sort objects that are not inherited from TBtreeitem
//Then you can just attach the TBtreeItem as a member/field to an object.
//You can also use this method to create multiple sorting mechanisms for
//a single set of objects by creating mutiple linkages.
//
//I implemented a similar technique in the linked_list.pas unit for generic linked lists.




// Original Source: http://www.ibrtses.com/delphi/binarytree.html
//
// Taken from Nicklaus Wirth :
// Algorithmen und Datenstrukturen ( in Pascal )
// Balanced Binary Trees p 250 ++
//
//
// Fixed By Giacomo Policicchio
// pgiacomo@tiscalinet.it
// 19/05/2000
//
// Enhanced further by Jason Nelson
// published 2018/11/19
// jasonrobertnelson@gmail.com
//
//


{$DEFINE POINTER_PERFECT}
{x$DEFINE EXTRA_CHECKS}
interface
{x$DEFINE DEBUG_TREE}
uses
  debug, numbers, stringx,SysUtils, Classes, betterobject, typex, generics.collections.fixed, sharedobject;

type
  TBTreeItem = class abstract(TBetterObject)
  strict private

    procedure SetTree(const [unsafe] Value: TObject);inline;
  protected
    procedure AddDuplicate(const [unsafe] itm: TBTreeItem);

    function Compare(const [unsafe] ACompareTo:TBTreeItem):ni; virtual; abstract;
      // a < self :-1  a=self :0  a > self :+1

    procedure Copy(const [unsafe] ACopyTo:TBTreeItem); virtual;
  public
    FBalance : - 2 .. 2;
    [unsafe] FLeftNode, FRightNode, FUpperNode, FLowerNode : TBTreeItem;
    [unsafe] Ftree: TObject;
    //Data : pointer;
    destructor Destroy;override;
    procedure Isolate;inline;
    [result: unsafe] function LastChild: TBtreeItem;
    procedure CorrectBalance;inline;
    function GetHeight: ni;inline;
    function NeedsRebalance: boolean;inline;
    property tree: TObject read FTree write SetTree;
  end;

  [unsafe] TUnsafeBtreeItem = TBtreeItem;

  TSimpleIterateProcedure = reference to procedure([unsafe] ABTreeItem:TBTreeItem);
  TIterateProcedure = reference to procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean);

  TBTree = class(TSharedObject)
  private
    FCount: ni;
    procedure Delete(const [unsafe]aitem: TBTreeItem; var [unsafe]p: TBTreeItem; out balance_changed: boolean; bDontFree: boolean);
    procedure DeleteNode(var [unsafe]  AItem: TBTreeItem);inline;
    procedure SearchAndInsert(const [unsafe]AItem: TBTreeItem; var [unsafe]p: TBTreeItem; var balance_changed: boolean;
      out Found: boolean);
  protected

    [result: unsafe]
    function SearchItem(const [unsafe] AItem:TBTreeItem; var [unsafe] ATargetNode:TBTreeItem):TBTreeItem;inline;
    function GetCount: ni;inline;
  public
    [unsafe] FRoot: TBTreeItem;

    constructor Create; override;
    destructor Destroy; override;


    function IsEmpty:boolean;inline;

    procedure ClearBruteForce;
    function Add(const [unsafe] AItem:TUnsafeBTreeItem):boolean;virtual;
    function Remove(const [unsafe] AItem:TBTreeItem; const bDontFree: boolean = false):boolean;inline;
    function RemoveTI(const [unsafe] AItem:TBTreeItem; const bDontFree: boolean = false):boolean;inline;
//    function Search(const [unsafe] AItem:TBTreeItem):boolean; overload;inline;
//    function Search(const [unsafe] AItem:TBTreeItem; var [unsafe] ASearchResult:TBTreeItem):boolean; overload;inline;
    function SlowSearch(const [unsafe] Aitem:TBtreeItem):ni;
    procedure Iterate(const AProcedure:TSimpleIterateProcedure); overload;
    procedure Iterate(AProcedure:TIterateProcedure); overload;
    [Result: unsafe] function LastItem: TBTreeItem;
    [Result: unsafe] function FirstItem: TBTreeItem;

    procedure AddTree(tr: TBtree);
    property Root : TBTreeItem read FRoot;
    property Count: ni read FCount;
    procedure CheckNotHas([unsafe] itm: TBtreeItem);
    procedure DecBalance(var [unsafe]p: TBTreeItem; var balance_changed: boolean; fordelete: boolean);
    procedure IncBalance(var [unsafe]p: TBTreeItem; var balance_changed: boolean; fordelete: boolean);

  end;

implementation
{$IFDEF REQUIRE_RAID}
uses
  raid;
{$ENDIF}
{ TBTree }

procedure TBTree.DecBalance(var [unsafe] p: TBTreeItem; var balance_changed: boolean; fordelete: boolean);

Begin
  case p.FBalance of
    1:
      begin
        p.FBalance := 0;
        if not fordelete then
          balance_changed := false;
      end;
    0:
      begin
        p.FBalance := -1;
        if fordelete then
          balance_changed := false;
      end;
    -1: (* if (p.Left<>nil) or not dl then *)
      begin // new balancing
        var [unsafe]  p1, p2: TBTreeItem;
        p1 := p.FLeftNode;
        if (p1.FBalance = -1) or ((p1.FBalance = 0) and fordelete) then
        begin // single ll rotation
          p.FLeftNode := p1.FRightNode;
          p1.FRightNode := p;
          if not fordelete then
            p.FBalance := 0
          else
          begin
            if p1.FBalance = 0 then
            begin
              p.FBalance := -1;
              p1.FBalance := +1;
              balance_changed := false;
            end
            else
            begin
              p.FBalance := 0;
              p1.FBalance := 0;
              (* h:=false; *)
            end;
          end;
          p := p1;
        end
        else
        begin // double lr rotation
          p2 := p1.FRightNode;
          if p2=nil then
            raise Ecritical.create('p2 is nil!');
          p1.FRightNode := p2.FLeftNode;
          p2.FLeftNode := p1;
          p.FLeftNode := p2.FRightNode;
          p2.FRightNode := p;
          if p2.FBalance = -1 then
            p.FBalance := +1
          else
            p.FBalance := 0;
          if p2.FBalance = +1 then
            p1.FBalance := -1
          else
            p1.FBalance := 0;
          p := p2;
          if fordelete then
            p2.FBalance := 0;
        end;
        if not fordelete then
        begin
          p.FBalance := 0;
          balance_changed := false;
        end;
      end; { -1 }
  end; { case }
End;

procedure TBTree.Delete(const [unsafe] aitem: TBTreeItem; var [unsafe] p: TBTreeItem; out balance_changed: boolean;
  bDontFree: boolean);
var
  iTemp: ni;
  [unsafe] delete_me,replace_me: TBTreeItem; // h=false;
  [unsafe] restack_bottom, restack_top: TBTreeItem;
  [unsafe] tmp,tmp2: TBTreeItem;
  procedure del(var [unsafe] r: TBTreeItem; var balance_changed: boolean);
  begin // h=false
    if r.FRightNode <> nil then
    begin
      del(r.FRightNode, balance_changed);
      //if (r=restack_top) then r := restack_bottom;//correct stack issues
      if balance_changed then
        DecBalance(r, balance_changed, true);
    end
    else
    begin
      {$IFDEF POINTER_PERFECT}
      tmp := r;
      tmp2 := r.FLeftNode;
      tmp.FBalance := replace_me.FBalance;
      tmp.FLeftNode := replace_me.FLeftNode;
      tmp.FRightNode := replace_me.FRightNode;
//      tmp.FLowerNode := replace_me.FLowerNode;
//    tmp.FUpperNode := replace_me.FUpperNode;

      replace_me := tmp;
      //delete_me := r;
      restack_top := delete_me;
      restack_bottom := tmp;
      r := tmp2;

      balance_changed := true;
      {$ELSE}
      r.Copy(replace_me); { q.key:=r.key; }
      delete_me := r;
      r := r.FLeftNode;
      balance_changed := true;
      {$ENDIF}
    end;
  end;

begin { main of delete }
  delete_me :=nil;
  restack_top := nil;
  restack_bottom := nil;
  if (p = nil) then
  begin
    balance_changed := false;
    if slowsearch(aitem) >=0 then
       raise ECritical.create('the item was found,  but not properly placed in the tree');
    raise ECritical.create('apparently, whatever you were looking for was not found... key changed?');
  end
  else if (AItem.Compare(p) > 0) { (x < p^.key) } then
  begin
    Delete(AItem, p.FLeftNode, balance_changed, bDontFree);
    if (p=restack_top) then p := restack_bottom;//correct stack issues
    if balance_changed then
      IncBalance(p, balance_changed, true);
  end
  else if (AItem.Compare(p) < 0) { (x > p^.key) } then
  begin
    Delete(AItem, p.FRightNode, balance_changed, bDontFree);
    if (p=restack_top) then p := restack_bottom;//correct stack issues
    if p <> nil then
      if balance_changed then
        DecBalance(p, balance_changed, true);
  end
  else
  begin // remove q
    tmp := p;
    if (tmp.FLowerNode <> nil) then begin
      while (tmp <> aitem) do begin
        tmp := tmp.FLowerNode;
        if tmp = nil then
          raise Ecritical.create('linked node not found');
      end;

      delete_me := tmp;
      balance_changed := false;
      if tmp.FLowerNode <> nil then begin
//        debug.consolelog('unlink @'+inttohex(ni(pointer(p)),1));
        tmp.FLowerNode.FUpperNode := tmp.FupperNode;
        tmp.FLowerNode.FLeftNode := tmp.FLeftNode;
        tmp.FLowerNode.FRightNode := tmp.FRightNode;
        tmp.FLowerNode.FBalance := tmp.FBalance;
        if tmp.Fuppernode <> nil then
          tmp.FUpperNode.FLowernode := tmp.FLowerNode;
        tmp := tmp.FLowerNode;
        IF p=delete_me then p := tmp;
{$ifdef EXTRA_CHECKS}
        CheckNotHas(delete_me);
{$ENDIF}
//        debug.consolelog('replaced with @'+inttohex(ni(pointer(p)),1));
      end else
      if tmp.Fuppernode <> nil then begin
        tmp.FUpperNode.FLowernode := tmp.FLowerNode;
        delete_me := tmp;
        tmp := nil;
{$ifdef EXTRA_CHECKS}
        CheckNotHas(delete_me);
{$ENDIF}
        //p := p.FUpperNode;//pprolly useless but whatever
      end;
    end else begin
      replace_me := p;
      delete_me := replace_me;
      if delete_me.FRightNode = nil then
      begin
        p := delete_me.FLeftNode;
        balance_changed := true;
      end
      else if (delete_me.FLeftNode = nil) then
      begin
        p := delete_me.FRightNode;
        balance_changed := true;
      end
      else
      begin
        del(delete_me.FLeftNode, balance_changed);
        if (p=restack_top) then begin
            iTemp := restack_bottom.FBalance;
            //restack_bottom's left node, was assigned by this reference variable
            //therefore, the address still points to the original deletion point
            //if needs to get reassigned as well
            restack_bottom.FLeftNode := delete_me.FLeftNode;
            p := restack_bottom;//correct stack issues
            p.FBalance := iTemp;

        end;
        if balance_changed then
          IncBalance(p, balance_changed, true);

        //p := restack_bottom;
      end;
    end;

    if delete_me = nil then
      raise Ecritical.create('did not find anything to delete when deleting @'+inttohex(ni(pointer(aitem)),2));
    if not bDontFree then
      delete_me.free
    else
      delete_me.isolate;

    dec(FCount);
  end;
end;

procedure TBTree.DeleteNode(var [unsafe] AItem: TBTreeItem);
begin
  if AItem = nil then Exit;

  if (AItem.FLeftNode <> nil) then DeleteNode(AItem.FLeftNode);
  if (AItem.FRightNode <> nil) then DeleteNode(AItem.FRightNode);

  AItem.DetachAndFree;
  AItem := nil;
end;

procedure TBTree.CheckNotHas([unsafe] itm: TBtreeItem);
begin
{$IFDEF EXTRA_CHECKS}
  Iterate( procedure (AItem: TBtreeItem)
    begin
      if itm = aitem then
        raise ECritical.create('has item when it shouldn''t');
    end
  );
{$ENDIF}

end;

procedure TBTree.ClearBruteForce;
begin
  while FRoot <> nil do Remove(FRoot);

//  DeleteNode(FRoot);
  fcount := 0;
end;

procedure TBTreeItem.Copy(const [unsafe] ACopyTo: TBTreeItem);
begin
{$IFDEF POINTER_PERFECT}
  raise ECritical.create('copy is not pointer perfect');
{$ENDIF}
  ACopyTo.FLowerNode := FLowerNode;
  ACopyTo.FUpperNode := FUpperNode;
end;

procedure TBTreeItem.CorrectBalance;
begin
  exit;
  if (FLeftNode = nil) and (FRightNode = nil) then FBalance := 0
  else if (FRightNode = nil) then FBalance := -1
  else if (FLEftNode = nil) then FBalance := 1;
end;

destructor TBTreeItem.Destroy;
begin
  {$IFDEF DEBUG_TREE}  DEBUG.CONSOLELOG('destroy @'+inttohex(ni(pointer(self)),1));{$ENDIF}
  inherited;
end;

function TBTreeItem.GetHeight: ni;
var
  l,r: ni;
begin
//  debug.consolelog('check height node='+inttostr(TRaidTreeItem(self).rab.startingblock));

  l := -1;
  r := -1;
  if FLeftNode <> nil then
    l := FLeftNode.GetHeight;
  if FRightNode <> nil then
    r := FRightNode.GetHeight;

  result := greaterof(l,r)+1;
  if abs(l-r) > 1 then begin
    debug.consolelog('tree balance error');

  end;
{$IFDEF EXTRA_CHECKS}
  if (r-l) <> Fbalance then begin
    debug.consolelog('balance does not match height difference ('+inttostr(r)+'-'+inttostr(l)+')'+inttostr(r-l)+' bal='+inttostr(FBAlance)+' node='+inttostr(TRaidTreeItem(self).rab.startingblock));
  end;
{$ENDIF}
end;

constructor TBTree.Create;
begin
  inherited;
  FRoot := nil;

end;

destructor TBTree.Destroy;
begin
  while FRoot <> nil do Remove(FRoot);

  if FCount > 0 then
    raise ECritical.create('after all nodes were destroyed, Count was not 0');


  inherited;
end;

procedure TBTree.IncBalance(var [unsafe] p: TBTreeItem; var balance_changed: boolean; fordelete: boolean);

Begin
  case p.FBalance of
    - 1:
      begin
        p.FBalance := 0;
        if not fordelete then
          balance_changed := false;
      end;
    0:
      begin
        p.FBalance := +1;
        if fordelete then
          balance_changed := false;
      end;
    +1:
      begin // new balancing
        var [unsafe] p1, p2: TBTreeItem;
        p1 := p.FRightNode;
        if (p1.FBalance = +1) or ((p1.FBalance = 0) and fordelete) then
        begin // single rr rotation
          p.FRightNode := p1.FLeftNode;
          p1.FLeftNode := p;
          if not fordelete then
            p.FBalance := 0
          else
          begin
            if p1.FBalance = 0 then
            begin
              p.FBalance := +1;
              p1.FBalance := -1;
              balance_changed := false;
            end
            else
            begin
              p.FBalance := 0;
              p1.FBalance := 0;
              (* h:=false; *)
            end;
          end;
          p := p1;
        end
        else
        begin // double rl rotation
          p2 := p1.FLeftNode;
          p1.FLeftNode := p2.FRightNode;
          p2.FRightNode := p1;
          p.FRightNode := p2.FLeftNode;
          p2.FLeftNode := p;
          if p2.FBalance = +1 then
            p.FBalance := -1
          else
            p.FBalance := 0;
          if p2.FBalance = -1 then
            p1.FBalance := +1
          else
            p1.FBalance := 0;
          p := p2;
          if fordelete then
            p2.FBalance := 0;
        end;
        if not fordelete then
        begin
          p.FBalance := 0;
          balance_changed := false;
        end;
      end;
  end; // case
End;

function TBTree.IsEmpty: boolean;
begin
  Result := FRoot = nil;
end;



procedure ListItems(const [unsafe] ABTreeItem:TBTreeItem; const AProcedure:TIterateProcedure; var ANeedStop:boolean);
var
  [unsafe] lower: TBTreeItem;
begin
  if ABTreeItem = nil then Exit;

  if ANeedStop then Exit;
  //----------------------------
  if (ABTreeItem.FLeftNode <> nil) then ListItems(ABTreeItem.FLeftNode, AProcedure, ANeedStop);
  if ANeedStop then Exit;
  //----------------------------
  AProcedure(ABTreeItem, ANeedStop);
  if ANeedStop then Exit;
  //----------------------------
  lower := ABTreeItem.FLowerNode;
  while lower <> nil do begin
    AProcedure(lower,ANeedStop);
    if ANeedStop then exit;
    lower := lower.FLowerNode;
  end;
  if ANeedStop then Exit;
  //----------------------------

  if (ABTreeItem.FRightNode <> nil) then ListItems(ABTreeItem.FRightNode, AProcedure, ANeedStop);
end;

procedure TBTree.Iterate(AProcedure: TIterateProcedure);
var
  NeedStop : boolean;
begin
  NeedStop := false;
  ListItems(FRoot, AProcedure, NeedStop);
end;

[result: unsafe]
function TBTree.LastItem: TBTreeItem;
begin
  result := FRoot;
  if result <> nil then begin
    while result.FRightNode <> nil do
      result := result.FRightNode;
  end;

end;

[result: unsafe]
function TBTree.FirstItem: TBTreeItem;
begin
  result := FRoot;
  if result <> nil then begin
    while result.FLEftNode <> nil do
      result := result.FLEftNode;
  end;

end;


procedure SimpleListItems(const [unsafe] ABTreeItem:TBTreeItem; const AProcedure:TSimpleIterateProcedure);
var
  [unsafe] lower: TBTreeItem;
begin
  if ABTreeItem = nil then Exit;

  if (ABTreeItem.FLeftNode <> nil) then SimpleListItems(ABTreeItem.FLeftNode, AProcedure);

  AProcedure(ABTreeItem);
  lower := ABtreeItem.FlowerNode;

  while lower <> nil do begin
    AProcedure(lower);
    lower := lower.FLowerNode;
  end;

  if (ABTreeItem.FRightNode <> nil) then SimpleListItems(ABTreeItem.FRightNode, AProcedure);
end;

procedure TBTree.Iterate(const AProcedure: TSimpleIterateProcedure);
begin
  SimpleListItems(FRoot, AProcedure);
end;

procedure TBTree.SearchAndInsert(const [unsafe] AItem: TBTreeItem; var [unsafe] p: TBTreeItem;
  var balance_changed: boolean; out Found: boolean);
begin
  Found := false;
  if p = nil then begin // word not in tree, insert it
    p := AItem;
    balance_changed := true;
    with p do begin
      if FRoot = nil then FRoot := p;
      FLeftNode := nil;
      FRightNode := nil;
      FBalance := 0;
    end;
  end else if (AItem.Compare(p) > 0) then begin
    SearchAndInsert(AItem, p.FLeftNode, balance_changed, Found);
    if balance_changed and (not found) then
      DecBalance(p, balance_changed, false);
  end else if (AItem.Compare(p) < 0) then begin
    SearchAndInsert(AItem, p.FRightNode, balance_changed, Found);
    if balance_changed and (not found) then
      IncBalance(p, balance_changed, false);
  end else begin
    balance_changed := false;
    p.AddDuplicate(aitem);
  end;

end;

// returns true and a pointer to the equal item if found, false otherwise
[result: unsafe]
function TBTree.SearchItem(const [unsafe] AItem: TBTreeItem; var [unsafe] ATargetNode: TBTreeItem): TBTreeItem;
begin
  Result := nil;
  if ATargetNode = nil then Exit;

  if (AItem.Compare(ATargetNode) = 0) then Result := ATargetNode
  else begin
    if (AItem.Compare(ATargetNode) > 0) then Result := SearchItem(AItem, ATargetNode.FLeftNode)
    else begin
      if (AItem.Compare(ATargetNode) < 0) then Result := SearchItem(AItem, ATargetNode.FRightNode)
    end;
  end;
end;



function TBTree.SlowSearch(const [unsafe] Aitem: TBtreeItem): ni;
var
  [unsafe] itm: TBTreeItem;
  res,cnt: ni;
begin
  itm := nil;
  res := -1;
  cnt := 0;
  Iterate(
    procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
    begin
      itm := ABTreeItem;
      if itm = Aitem then begin
        res := cnt-1;
      end;

      inc(cnt);
    end
  );

  result := res;

end;

function TBTree.Add(const [unsafe] AItem: TBTreeItem): boolean;
var
  h, Found : boolean;
begin
{$ifdef EXTRA_CHECKS}
  CheckNotHas(aitem);
  if Aitem.tree <> nil then
    raise ECritical.create('cannot add item already in tree');
{$ENDIF}
  Aitem.tree := self;
  SearchAndInsert(AItem, FRoot, h, Found);
  // "Found = true" means AItem is not inserted.
  Result := not Found;
  if result then
    inc(FCount)
  else
    DEbug.Log('item is already in tree!');
end;

function TBTree.Remove(const [unsafe] AItem: TBTreeItem; const bDontFree: boolean = false): boolean;
var
  balance_changed : boolean;
begin
  result := true;
  Delete(aitem, FRoot, balance_changed, bDontFree);
  //aitem.tree := nil;
{$ifdef EXTRA_CHECKS}
  if bDontFree then
    CheckNotHas(aitem);
{$ENDIF}
end;


function TBTree.RemoveTI(const [unsafe] AItem:TBTreeItem; const bDontFree: boolean = false):boolean;
begin
  result := Remove(AItem, bDontFree);
end;


//function TBTree.Search(const [unsafe] AItem: TBTreeItem): boolean;
//begin
//  Result := SearchItem(AItem, FRoot) <> nil;
//end;
//
//function TBTree.Search(const [unsafe] AItem:TBTreeItem; var [unsafe] ASearchResult:TBTreeItem):boolean;
//begin
//  ASearchResult := SearchItem(AItem, FRoot);
//  Result := ASearchResult <> nil;
//end;

{ TBTreeItem }

procedure TBTreeItem.AddDuplicate(const [unsafe] itm: TBTreeItem);
var
  [unsafe] oldlower: TBTreeItem;
begin
  if itm = self then
    raise ecritical.create('wtf');

  oldlower := FLowerNode;

  FLowerNode := itm;//MY lower node is the new item
  itm.Fuppernode := self;//the new item's upper node is me

  //if I had a lower item before,
  if oldlower <> nil then begin
    //that item's upper item is the new item
    oldlower.FupperNode := itm;
    //the item's lower item is the old lowe item
    itm.FLowerNode := oldlower;

  end;

end;

procedure TBTreeItem.Isolate;
begin
  FLowerNode := nil;
  FUpperNode := nil;
  FLeftNode := nil;
  FRightNode := nil;
  FBalance := 0;
  tree := nil;
end;

[result: unsafe]
function TBTreeItem.LastChild: TBtreeItem;
begin
  if FRightnode = nil then
    result := self
  else
    result := FRightNode.LastChild;

end;

function TBTreeItem.NeedsRebalance: boolean;
begin
  result := abs(FBalance) > 1;
end;

procedure TBTreeItem.SetTree(const [unsafe] Value: TObject);
var
  [unsafe] old_tree: TObject;
begin
  old_tree := FTree;


  FTree := Value;

{$IFDEF AUTOREFCOUNT}
  //if moved from tree to no tree, then release
  if assigned(old_tree) and (not assigned(value)) then
    __ObjRelease;//manually maintain reference to items if tree property is set is set (then all other references can be [unsafe]


  //if moved from no-tree to tree then addref
  if (not assigned(old_tree)) and (assigned(value)) then
    __ObjAddRef;//manually maintain reference to items if tree property is set is set (then all other references can be [unsafe]
{$ENDIF}

end;

function TBTree.GetCount: ni;
begin
//  Debug.Log(self, 'Count ');
//  Debug.Log(self, 'Count '+inttostr(FCount));
  result := FCount;
end;

procedure TBTree.AddTree(tr: TBtree);
var
  ti: TBTreeItem;
begin
  repeat
    ti := tr.Root;
    if ti = nil then
      break;

    tr.Remove(ti, true);
    self.Add(ti);
  until false;


end;




end.

