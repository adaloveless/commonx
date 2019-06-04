unit JsonTreeView;

interface

uses
  debug, comctrls, jsonhelpers, typex, stringx, systemx, betterobject, guihelpers, better_collections, classes;

type
  TJSONTreeView = class(TTreeView)
  private
    FJson: IHolder<TJSON>;
    FNodeRelations: TStringObjectList<TTreeNode>;
    procedure SetJSon(const Value: IHolder<TJSON>);
  protected
    procedure Expand(Node: TTreeNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property json: IHolder<TJSON> read FJson write SetJSon;
    procedure LoadNodeChildren(tn: TTreeNode; deep: ni);
    procedure AddNodeRelation(addr: string; tn: TTreeNode);

  published

  end;


implementation

{ TJSONTreeView }

procedure TJSONTreeView.AddNodeRelation(addr: string; tn: TTreeNode);
var
  s: string;
begin
  s := 'nil';
  if tn <> nil then
    s := tn.text;
  if tn <> nil then begin
    FNodeRelations.add(addr, tn);
  end;
end;

constructor TJSONTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FNodeRelations :=  TStringObjectList<TTreeNode>.create;
  FNodeRelations.Duplicates := dupIgnore;
end;

destructor TJSONTreeView.Destroy;
begin
  Fnoderelations.free;
  FNodeRelations := nil;
  inherited;
end;

procedure TJSONTreeView.Expand(Node: TTreeNode);
begin
  inherited;
  //walk through all nodes and sync
  Debug.Log('Expand node with text: '+node.Text);
  LoadNodeChildren(node,2);

end;

procedure TJSONTreeView.LoadNodeChildren(tn: TTreeNode; deep: ni);
var
  idx: ni;
  nodeAddr: string;
  t, cnt: ni;
  n: TJSON;
  ttn: TTreeNode;
begin
  if json = nil then
    exit;
  if json.o = nil then
    exit;
  if deep = 0 then
    exit;
  idx := fnodeRelations.IndexOfObject(tn);
  if idx < 0 then
    exit;
  nodeAddr := FNodeRelations.Keys[idx];
//  Debug.Log('Expanding '+nodeAddr);
  cnt := Treenode_GetChildCount(self, tn);
  if cnt > 0 then begin
    for t := 0 to cnt-1 do begin
      ttn := TreeNode_GetChild(self, tn, t);
      LoadNodeChildren(ttn,deep-1);
    end;
  end else begin
    n := json.o.GetNode(nodeAddr);
    guihelpers.JSONtoTreeNode(n, self, tn,deep);
  end;


end;

procedure TJSONTreeView.SetJSOn(const Value: IHolder<TJSON>);
begin
  items.Clear;
  FNodeRelations.Clear;

  FJson := Value;
  if FJSON <> nil then
    JSONtoTreeView(FJson.o, self,1);

end;

end.
