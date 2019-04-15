unit SpreadTree;

interface


uses
  debug, numbers, classes, jsonhelpers, systemx, stringx, betterobject, typex, Generics.Collections.Fixed, variants, sysutils, better_collections, commandprocessor;

type
  TJSONScalar = TJSON;
  TJSONArray = TJSON;

  TSpreadTreeNode = class(TBetterObject)
  protected
    function GetCode: TJson;
    function GetResults: TJSON;
  public
    codeH: IHolder<TJSON>;
    resultsH: IHolder<TJSON>;
    property code: TJson read GetCode;
    property results: TJSON read GetResults;
    constructor Create;override;
  end;

  TCodeResultPair = record
    code, results: TJSON;
  end;

  TSpreadTree = class(TStringObjectList<TSpreadTreeNode>)
  public
    LastScriptError: string;
    function HasCode(sAddr: string): boolean;
    function GetCode(sAddr: string): TJSON;
    function HasResult(sAddr: string): boolean;
    function GetResult(sAddr: string): TJSON;

    function AddNew(sName: string): TSpreadTreeNode;
    function GetResultsJSON: string;
    function GetCodeJSON: string;
    procedure SetCodeJSON(s: string);
    function ToQuickSave_Code: string;
    procedure FromQuickSave_Code(s: string);
    function Calc(sScript: string): IHolder<TJSON>;
    procedure Solve;
    procedure NodeError(sNode: string; sError: string);
    property JSONCode: string read GetCodeJSON write SetCODEJSON;
    property JSONResults: string read GetRESULTSJSON;
  end;

  Tcmd_SolveSpreadTree = class(TCommand)
  public
    st: TSpreadTree;
    procedure DoExecute;override;
  end;


function JSONAdd(j1,j2: TJSONScalar): IHolder<TJSON>;
function JSONSub(j1,j2: TJSONScalar): IHolder<TJSON>;
function JSONMul(j1,j2: TJSONScalar): IHolder<TJSON>;
function JSONDiv(j1,j2: TJSONScalar): IHolder<TJSON>;
function JSONMin(j1,j2: TJSONScalar): IHolder<TJSON>;overload;
function JSONMax(j1,j2: TJSONScalar): IHolder<TJSON>;overload;
function JSONLT(j1,j2: TJSONScalar): IHolder<TJSON>;
function JSONGT(j1,j2: TJSONScalar): IHolder<TJSON>;
function JSONLTE(j1,j2: TJSONScalar): IHolder<TJSON>;
function JSONGTE(j1,j2: TJSONScalar): IHolder<TJSON>;
function JSONNE(j1,j2: TJSONScalar): IHolder<TJSON>;



function JSONToScalar(Item: TJSON): IHolder<TJSON>;
function JSONToArray(arraybase: TJSON; subItem: string = ''): IHolder<TJSON>;
//function JSONMin(j1: TJSON): IHolder<TJSON>;overload;
//function JSONMax(j1: TJSON): IHolder<TJSON>;overload;

function StrToST(s: string): TSpreadTree;
function StrToSTh(s: string): IHolder<TSpreadTree>;
function JStrToSTh(s: string): IHolder<TSpreadTree>;
function JStrToST(s: string): TSpreadTree;


implementation


type
  TSpreadTreeUnsolvedNodes = class(TStringObjectList<TJSON>)
  public
  end;


function JSONAdd(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value + j2.value;
end;

function JSONToScalar(Item: TJSON): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := item.value;
end;

function JSONSub(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value - j2.value;
end;
function JSONMul(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value * j2.value;
end;
function JSONDiv(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value * j2.value;
end;

function JSONMin(j1,j2: TJSONScalar): IHolder<TJSON>;overload;
begin
  result.o := TJSON.create;
  if j1.value <= j2.value then
    result.o.value := j1.value
  else
    result.o.value := j2.value;
end;

function JSONLT(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value < j2.value;
end;

function JSONGT(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value > j2.value;
end;

function JSONLTE(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value <= j2.value;
end;

function JSONGTE(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value >= j2.value;
end;

function JSONNE(j1,j2: TJSONScalar): IHolder<TJSON>;
begin
  result.o := TJSON.create;
  result.o.value := j1.value <> j2.value;
end;




function JSONMax(j1,j2: TJSONScalar): IHolder<TJSON>;overload;
begin
  result.o := TJSON.create;
  if j1.value >= j2.value then
    result.o.value := j1.value
  else
    result.o.value := j2.value;
end;


function JSONToArray(arraybase: TJSON; subItem: string = ''): IHolder<TJSON>;
var
  t: ni;
begin
  result.o := TJSON.create;
  for t:= 0 to arraybase.iCount-1 do begin
    if subitem = '' then
      result.o.AddIndexed(arraybase[t])
    else
      result.o.AddIndexed(arraybase[t][subItem]);
  end;

end;

{ TSpreadTree }


function TSpreadTree.AddNew(sName: string): TSpreadTreeNode;
begin
  result :=  TSpreadTreeNode.create;
  self.Add(sName, result);
end;

function TSpreadTree.Calc(sScript: string): IHolder<TJSON>;
begin
  result := nil;
  if length(sScript) < 1 then begin
    LastScriptError := 'Blank Script';
    exit;
  end;
  if length(sScript) < 2 then begin
    LastScriptError := 'A valid script must have at least 2 characters';
    exit;
  end;

  if sScript[STRZ] <> '=' then begin
    LAstScriptError := 'A Script must start with =';
    exit;
  end;






end;




procedure TSpreadTree.FromQuickSave_Code(s: string);
var
  t: ni;
  n: string;
  ns: TArray<string>;
  sl: TStringlist;
  s1, s2: string;
  bForceFM: boolean;
begin
  self.Clear;

  sl := nil;
  try
    bforceFM := zcopy(s, 0,2) = 'FM';
    if bForceFM then
      s := zcopy(s,2,length(s));

    if not bForceFM then
      bForceFM := (zcopy(s, 0, 1) = '"');

    if bForceFM then begin
      sl := stringToStringList(s);
      for t:= 0 to sl.count-1 do begin
        SplitString(sl[t], ':', s1,s2);
        s1 := unquote(s1);
        self.AddNew(s1).code.FromString(s2);
      end;
    end else begin
      JSONCode := s;
    end;
  finally
    sl.free;
  end;

end;

function TSpreadTree.GetCode(sAddr: string): TJSON;
var
  s1,s2: string;
begin
  try
    SplitString(sAddr, '.', s1,s2);
    self[s1].code.GetNode(s2);
    exit(self[s1].code.GetNode(s2));

  except
    raise EJSONNodeNotFound.create(sAddr+' was not found.');
  end;

end;




function TSpreadTree.GetCodeJSON: string;
var
  t: ni;
  k: string;
  v: TSpreadTreeNode;
  ks: TArray<string>;
  vs: Tarray<IHolder<TJSON>>;
begin
  result := '{';
  for t:= 0 to count-1 do begin
    if t > 0 then
      result := result + ',';
    k := keys[t];
    v := self.Items[k];
    result := result + quote(k)+': '+v.GetCode.ToJson;
  end;

  result := result + '}';

end;


function TSpreadTree.GetResult(sAddr: string): TJSON;
var
  s1,s2: string;
begin
  try
    SplitString(sAddr, '.', s1,s2);
    self[s1].results.GetNode(s2);
    exit(self[s1].results.GetNode(s2));

  except
    raise EJSONNodeNotFound.create(sAddr+' was not found.');
  end;

end;

function TSpreadTree.GetResultsJSON: string;
var
  t: ni;
  k: string;
  v: TSpreadTreeNode;
  ks: TArray<string>;
  vs: Tarray<IHolder<TJSON>>;
begin
  result := '{';
  for t:= 0 to count-1 do begin
    if t > 0 then
      result := result + ',';
    k := keys[t];
    v := self.Items[k];
    result := result + quote(k)+': '+v.GetResults.ToJson;
  end;

  result := result + '}';
end;

function TSpreadTree.HasCode(sAddr: string): boolean;
var
  s1,s2: string;
  t: ni;
begin
  SplitString(sAddr, '.', s1,s2);
  for t:= 0 to count-1 do
    if keys[t] = s1 then begin
      exit(self[s1].code.HasNode(s2));
    end;

  exit(false);

end;


function TSpreadTree.HasResult(sAddr: string): boolean;
var
  s1,s2: string;
  t: ni;
begin
  SplitString(sAddr, '.', s1,s2);
  for t:= 0 to count-1 do
    if keys[t] = s1 then begin
      exit(self[s1].results.HasNode(s2));
    end;

  exit(false);

end;

procedure TSpreadTree.NodeError(sNode, sError: string);
begin
  Debug.Log(sNode+' error:'+sError);
end;

procedure TSpreadTree.SetCodeJSON(s: string);
begin

end;


procedure TSpreadTree.Solve;
var
  t: ni;
  unsolved: TSpreadTreeUnsolvedNodes;
  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TSpreadTree.Solve
  procedure FindUnsolvedNodes(nodeaddr:string; node: TJSON);
  var
    i: ni;
  begin
    if VarIsStr(node.value) then begin
      if copy(node.value, STRZ, 1) = '=' then begin
        Debug.Log('unsolved! '+nodeaddr);
        unsolved.Add(nodeaddr, node);
      end;
    end;
    if node.named.Count > 0 then begin
      for i := 0 to node.named.Count-1 do begin
        var nn := node.named.ItemsByIndex[i];
        FindUnsolvedNodes(nodeaddr+nn.addr, nn);
      end;
    end;
    if length(node.indexed) > 0 then begin
      for i := 0 to high(node.indexed) do begin
        FindUnsolvedNodes(nodeaddr+'['+inttostr(i)+']', node.indexed[i]);
      end;
    end;
  end;
  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TSpreadTree.Solve
  function TryResolveAddrToValue(nAddr: string; nscope: TJSON; out nRes: TJSON): boolean;
  begin
    if nscope = nil then
      exit(false);
    if not nscope.HasNode(naddr) then
      result := TryResolveAddrToValue(nAddr, nscope.parent, nRes)
    else begin
      nRes := nscope.GetNode(nAddr);
      exit(true);
    end;

  end;
  function TrySolveNode(n: TJSON): boolean;
  var
    s1, s2: string;
    code: string;
    nSolved: TJSON;
  begin
    code := n.AsString;
    code := copy(code, strz+1, length(code)-1);
    nSolved := nil;
    result := TryResolveAddrToValue(code, n, nSolved);
    if unsolved.IndexOfObject(nSolved) >= 0 then begin
      result := false;//OOPS-- source node still needs to be solved
    end else begin
      n.JSON := nsolved.ToJson;
    end;

  end;

begin
  unsolved := TSpreadTreeUnsolvedNodes.create;
  try
    for t:= 0 to Count-1 do begin
      var n := self.ItemsByIndex[t];
      n.results.json := n.code.json;
      FindUnsolvedNodes(self.Keys[t],n.results);
    end;

    begin
      var somesolved := false;

      repeat
        somesolved := false;
        for t:= unsolved.count-1 downto 0 do begin
          var unode := unsolved.ItemsByIndex[t];
          var solved := TrySolveNode(unode);
          if solved then begin
            unsolved.Delete(t);
          end;
          somesolved := somesolved or solved;
        end;
      until somesolved = false;
    end;

  finally
    unsolved.free;
  end;
end;

function TSpreadTree.ToQuickSave_code: string;
var
  t,tt: ni;
  k: string;
  v: TJSON;
  s,ss: string;
begin
  ss := '';

  result := '';

  for t:= 0 to count-1 do begin
    k := self.Keys[t];
    v := self[k].code;

    s := v.ToJson;
    s := stringreplace(s, CRLF, '', [rfReplaceAll]);
    s := (quote(k)+': ')+s+CRLF;

    result := result + s;
    v := nil;
  end;
  result := 'FM'+result;

end;

function StrToST(s: string): TSpreadTree;
begin
  result := TSpreadTree.create;
  result.FromQuickSave_code(s);
end;

function JStrToST(s: string): TSpreadTree;
begin
  result := TSpreadTree.create;
  result.JSONCode := s;
end;

function JStrToSTh(s: string): IHolder<TSpreadTree>;
begin
  result := THolder<TSpreadTree>.create;
  result.o := JStrtoST(s);
end;

function StrToSTh(s: string): IHolder<TSpreadTree>;
begin
  result := THolder<TSpreadTree>.create;
  result.o := StrtoST(s);
end;






{ TSpreadTreeNode }

constructor TSpreadTreeNode.Create;
begin
  inherited;
  codeH := THolder<TJSON>.create;
  codeh.o := TJSON.create;
  resultsH := THolder<TJSON>.create;
  resultsh.o := TJSON.create;

end;

function TSpreadTreeNode.GetCode: TJson;
begin
  result := codeH.o;
end;

function TSpreadTreeNode.GetResults: TJSON;
begin
  result := resultsH.o;

end;

{ Tcmd_SolveSpreadTree }

procedure Tcmd_SolveSpreadTree.DoExecute;
begin
  inherited;
  raise ECritical.create('not implemented');
end;

end.
