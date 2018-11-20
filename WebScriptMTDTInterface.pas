unit WebScriptMTDTInterface;

interface

uses Variants,classes, sysutils, requestinfo, MTDTInterface, DataobjectServices, DataObject, Exceptions, ErrorResource, clientconstants, windows, rights, webscript;

procedure ScriptNew(rqInfo: TRequestInfo; bGhost: boolean; slParams: TScriptParamList);
function ScriptFetch(rqInfo: TRequestInfo; bLazy: boolean; slParams: TScriptParamList; bNoException: boolean = false): boolean;
procedure ScriptGhostFetch(rqInfo: TRequestInfo; slParams: TScriptParamList);


implementation

//uses RolesAndRights;

procedure ScriptGhostFetch(rqInfo: TRequestInfo; slParams: TScriptParamList);
begin
  //try to fetch
  if not ScriptFetch(rqInfo, false, slParams, true) then begin
    ScriptNew(rqInfo, true, slParams);
  end;
end;


procedure ScriptNew(rqInfo: TRequestInfo; bGhost: boolean; slParams: TScriptParamList);
var
  sObjectPoolName, sObjectType: ansistring;
   v: variant;
  t: integer;
  ipos: integer;
begin
  if slParams.count < 2 then
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, 'Incorect number of parameters passed to ScriptNew ('+inttostr(slParams.count)+').');

  if (slParams.count mod 2)<>0 then
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, ':New requires an even number of parameters. '+inttostr(slParams.count)+' parameters were passed.');

  //objectpool name is param[0]
  sObjectPoolName := slParams[0];
  //object type is param[1]
  sObjectType := slParams[1];

  //create the variant array
  v:= VarArrayCreate([0, slParams.Count-4], varVariant);
  iPos := 0;
  //params 2..n are alternating type, value
  for t:=2 to slParams.count-1 do begin
    if vartostr(slParams[t]) = '$' then
      v[iPos] := slParams[t+1];

    try
      if (vartostr(slParams[t]) = '%'){ or (slParams[t] = '%29')} then
        v[iPos] := slParams[t+1];
    except
      raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, vartostr(slParams[t+1])+' could not be converted to an integer.');
    end;

    inc(iPos);

  end;

  //finally... call away
  if bGhost then
    rqInfo.response.ObjectPool[sObjectPoolName] := Ghost(rqInfo, sObjectType, v)
  else
    rqInfo.response.ObjectPool[sObjectPoolName] := New(rqInfo, sObjectType, v)
  ;


end;


function ScriptFetch(rqInfo: TRequestInfo; bLazy: boolean; slParams: TScriptParamList; bNoException: boolean = false): boolean;
var
  o: TDataObject;
  sObjectPoolName, sObjectType: ansistring;
   v: variant;
  t: integer;
  ipos: integer;
begin
  if slParams.count < 2 then
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, 'Incorect number of parameters passed to ScriptFetch ('+inttostr(slParams.count)+').');

  if (slParams.count mod 2)<>0 then
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, ':Fetch requires an even number of parameters. '+inttostr(slParams.count)+' parameters were passed.');

  //objectpool name is param[0]
  sObjectPoolName := slParams[0];
  //object type is param[1]
  sObjectType := slParams[1];

  //create the variant array
  v:= VarArrayCreate([0, slParams.Count-4], varVariant);
  iPos := 0;
  //params 2..n are alternating type, value
  for t:=2 to slParams.count-1 do begin
    if vartostr(slParams[t]) = '$' then
      v[iPos] := slParams[t+1];

    try
      if (vartostr(slParams[t]) = '%'){ or (slParams[t] = '%29')} then
        v[iPos] := strtoint(vartostr(slParams[t+1]));
    except
      raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, slParams[t+1]+' could not be converted to an integer.');
    end;

    inc(iPos);

  end;

  if bNoException then begin
    if bLazy then begin
      result := rqInfo.Server.LazyFetch(rqInfo.Response.docache, o, sobjectType, v, rQInfo.SessionID);
    end else begin
      result := rqInfo.Server.Fetch(rqInfo.Response.docache, o, sobjectType, v, rQInfo.SessionID);
    end;

    if o <> nil then begin
      rqInfo.response.ObjectPool[sObjectPoolName] := o;
    end;
  end else begin
    result := true;
    if bLazy then begin
      result := rqInfo.Server.LazyFetch(rqInfo.Response.docache, o, sobjectType, v, rQInfo.SessionID);
    end else begin
      result := rqInfo.Server.Fetch(rqInfo.Response.docache, o, sobjectType, v, rQInfo.SessionID);
    end;

    if o = nil then begin
      raise EClassException.create(rqINfo.server.GetLastErrorMessage);
    end else begin
      rqInfo.response.ObjectPool[sObjectPoolName] := o;
    end;




  end;



end;










end.
