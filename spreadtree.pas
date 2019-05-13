unit SpreadTree;

interface


uses
  debug, numbers, classes, jsonhelpers, systemx, stringx, betterobject, typex, Generics.Collections.Fixed, variants, sysutils, better_collections, commandprocessor;

type
  ESourceTreeExpression = class(Exception);
  TJSONScalar = TJSON;
  TJSONArray = TJSON;

  TTokenType = (ttNormal, ttOpenParen, ttCloseParen);

  TExpressionToken = record
    tokentype: TTokenType;
    leftop: char;
    code: string;
    value: variant;
    procedure Init;
  end;

  TTokenArray = TArray<TExpressionToken>;
  TTokenSolutionType = (tstValue, tstNode);
  TTokenSolution = record
    solved: boolean;
    solutiontype: TTokenSolutionType;
    value: variant;
    nodesolution: TJSON;
    procedure Init;
  end;

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
    function ToQuickSave_Results: string;
    procedure FromQuickSave_Code(s: string);
    function Calc(sScript: string): IHolder<TJSON>;
    procedure Solve;
    procedure NodeError(sNode: string; sError: string);
    property JSONCode: string read GetCodeJSON write SetCODEJSON;
    property JSONResults: string read GetRESULTSJSON;
    function FindResultEx(sAddr: string): TFindNodeRes;
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




function TSpreadTree.FindResultEx(sAddr: string): TFindNodeRes;
begin
  result.Init;
  if self.HasResult(sAddr) then begin
    result.success := true;
    result.node := self.GetResult(sAddr);
    result.FoundAddr := sAddr;
    exit;
  end;
  var t: ni;
  for t:= 0 to Count-1 do begin
    var n := self.ItemsByIndex[t];
    result := n.results.FindNodeEx(sAddr);
    if result.success then
      break;
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

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
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
  function TryResolveAddrToValueXDoc(nAddr: string; out nRes: TJSON): boolean;
  begin
    nRes := nil;
    var res := FindResultEx(nAddr);
    result := res.success;
    if result then begin
      if unsolved.IndexOfObject(res.node) >=0 then
        exit(false);
      nRes := res.node;
    end;
  end;
  function TryResolveAddrToValue(nAddr: string; nscope: TJSON; out nRes: TJSON): boolean;
  begin
    if nscope = nil then
      exit(TryResolveAddrToValueXDoc(nAddr, nres));
    if not nscope.HasNode(naddr) then
      result := TryResolveAddrToValue(nAddr, nscope.parent, nRes)
    else begin
      nRes := nscope.GetNode(nAddr);
      exit(true);
    end;
  end;
  function TokenAddrToValue(ts: TTokenSolution): TTokenSolution;
  begin
    result.init;
    result.solutiontype := tstValue;
    if copy(ts.nodesolution.value,STRZ,1) = '=' then begin
      result.solved := false;
      exit;
    end else begin
      result.solved := true;
      if result.solved then begin
        result.value := ts.nodesolution.value;
      end;
    end;


  end;
  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TSpreadTree.Solve
  const
    parens = ['(',')'];
    operators = ['+','-','/','*'];
  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TSpreadTree.Solve
  function IsParen(c: char): boolean;
  begin
    result := charinset(c, parens);
  end;
  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TSpreadTree.Solve
  function IsOperator(c: char): boolean;
  begin
    result := charinset(c, operators);
  end;
  function IsBreak(c: char): boolean;
  begin
    result := IsOperator(c) or IsParen(c);
  end;
  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TSpreadTree.Solve
  function TokenizeExpression(code: string; n: TJSON): TTokenArray;
  var
    currenttok: TExpressionToken;
    procedure CommitToken;
    begin
      if (currenttok.code <> '') or (currenttok.leftop <> #0) or (currenttok.tokentype<>ttNormal) then begin
        setlength(result, length(result)+1);
        result[high(result)] := currenttok;
        currenttok.init;
      end;
    end;
  begin
    currenttok.init;
    var tokencharcount: ni := 0;
    for var t := low(code) to high(code) do begin
      var c := code[t];
      if c = ' ' then continue;//ignore whitespace
      if c = '(' then begin
        committoken;
        currenttok.tokentype := ttOpenParen;
        committoken;
        continue;
      end;
      if c = ')' then begin
        committoken;
        currenttok.tokentype := ttCloseParen;
        committoken;
        continue;
      end;

      if ((tokencharcount = 0) and (c = '-')) or (not IsOperator(c)) then begin
        currenttok.code := currenttok.code  + c;
        inc(tokencharcount);//increment changes the behavior of the '-' (becomes minus instead of negative)
      end else begin
        currenttok.code  := trim(currenttok.code);
        CommitToken;
        currenttok.leftop := c;
        tokencharcount := 0;
      end;
    end;

    currenttok.code := trim(currenttok.code);
    CommitToken;


  end;
  function ExtractSubExpression(n: TJSON; a: TTokenArray; var startidx_returns_end: ni): TTokenArray;
  begin
    var deep: ni := 0;
    //find end index
    var endidx: ni  := -1;
    var startidx := startidx_returns_end;
    for var t := startidx to high(a) do begin
      var tok := a[t];
      if tok.tokentype = ttOpenPAren then
        inc(deep);
      if tok.tokentype = ttClosePAren then begin
        dec(deep);
        if deep = 0 then begin
          endidx := t;
          break;
        end;
      end;
    end;

    if endidx < 0 then
      raise ECritical.Create('Expected '')''');

    setlength(result, (endidx - startidx)-1);
    var idx: ni := 0;
    for var t:= (startidx+1) to (endidx-1) do begin
      result[idx] := a[t];
      inc(idx);
    end;
    startidx_returns_end := endidx + 1;
  end;
  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TSpreadTree.Solve
  function TrySolveCode(code: string;n:TJSON): TTokenSolution;
  begin
    result.Init;
    if code = '' then
      exit;
    //if starts with a number, then this is a constant
    //if start with a '-' then this is a constant
    var c := copy(code, strz, 1);
    if IsInteger(code) then begin
      result.solved := true;
      result.value := StrToInt64(code);
    end else
    if IsFloat(code) then begin
      result.solved := true;
      result.value := strtofloat(code);
    end else begin
      //else it is a token, resolve the token
      result.solved :=  TryResolveAddrToValue(code, n, result.nodesolution);
      result.solutiontype := tstNode;
      if result.solved then begin
        //todo 1: recursive?
      end else begin
        result.value := '!@#$ '+code+' !@#$';
        //raise ESourceTreeExpression.create('cannot solve '+code);
      end;
    end;
  end;

  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TSpreadTree.Solve
  function Operate(v1: variant; op: char; v2: variant): variant;
  begin

    case op of
      '+': exit(v1+v2);
      '-': exit(v1-v2);
      '*': exit(v1*v2);
      '/': exit(v1/v2);
      #0: begin
        if varIsNull(v1) then
          exit(v2)
        else
          exit(v1);
      end;
    else
      raise ECritical.create('unhandled operator '+op);
    end;
    result := null;
  end;
  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TSpreadTree.Solve
  function TrySolveTokenArray(tokens: TTokenArray;scope: TJSON): TTokenSolution;
  begin
//    Debug.Log('>>>> TRY SOLVE TOKEN ARRAY');
//    try
    result.Init;
    var t: ni := 0;
    var val: variant := null;
    var prevop: char := #0;
    while t < length(tokens) do begin
      var tok := tokens[t];
      if tok.tokentype = ttOpenParen then begin
        var subexpr := ExtractSubExpression(scope, tokens, {VAR}t);
        var subsol := TrySolveTokenArray(subexpr, scope);
        if not subsol.solved then
          exit;//can't solve now, subsolution did not solve

        if t = 0 then begin
          val := subsol.value;
        end else begin
          val := operate(val, prevop, subsol.value);
        end;
        continue;
      end else begin
        if tok.code = '' then begin //if there are TWO operators in a row code will be ''
          prevop := tok.leftop;
          inc(t);
          continue;
        end;
      end;

      var tokensolution := TrySolveCode(tok.code, scope);
      if tokensolution.solved then begin
        if tokensolution.solutiontype <> tstValue then begin
          tokensolution := TokenAddrToValue(tokensolution);
          if tokensolution.solutiontype <> tstValue then begin
            exit;
          end;
        end;
      end else begin
        exit;
      end;

      if t = 0 then begin
        val := tokensolution.value;
      end else begin
        val := operate(val, tok.leftop, tokensolution.value);
      end;
      inc(t);
    end;
    result.value := val;
    result.solved := true;
//    finally
//      Debug.Log('<<<<<<< EXIT TRY SOLVE TOKEN ARRAY Solved = '+booltostr(result.solved)+' val='+vartojsonstorage(result.value));
//    end;
  end;
  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TSpreadTree.Solve
  function TrySolveExpression(code: string; n: TJSON): boolean;
  var
    nSolved: TJSON;
    tokens: TTokenArray;
    lastop: char;
  begin
    lastop := #0;
    tokens := TokenizeExpression(code, n);
    var res := TrySolveTokenArray(tokens, n);
    result := res.solved;
    if result then
      n.json := VArtoJSONStorage(res.value);

  end;
  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TSpreadTree.Solve
  function TrySolveNode(n: TJSON): boolean;
  var
    s1, s2: string;
    code: string;
    nSolved: TJSON;
  begin
    code := n.AsString;
    code := copy(code, strz+1, length(code)-1);
    nSolved := nil;
    result := TrySolveExpression(code, n);

  end;
  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TSpreadTree.Solve
begin
  unsolved := TSpreadTreeUnsolvedNodes.create;
  try
    for t:= 0 to Count-1 do begin
      var n := self.ItemsByIndex[t];
      n.results.json := n.code.json;
    end;

    begin
      var somesolved := false;
      var unsolvedcount: ni := 0;
      var lastcount: ni := 0;
      repeat
        unsolved.clear;
        for t:= 0 to Count-1 do begin
          var n := self.ItemsByIndex[t];
          FindUnsolvedNodes(self.Keys[t],n.results);
        end;
        lastcount := unsolvedcount;
        unsolvedcount := unsolved.count;

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
      until (somesolved = false) and (lastcount = unsolvedcount);
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

function TSpreadTree.ToQuickSave_Results: string;
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
    v := self[k].results;

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

{ TTokensolution }

procedure TTokensolution.Init;
begin
  solved := false;
  value := null;
  solutiontype := tstValue;

end;



{ TExpressionToken }

procedure TExpressionToken.Init;
begin
  tokentype := ttNormal;
  code := '';
  leftop := #0;
  value := null;

end;

end.
