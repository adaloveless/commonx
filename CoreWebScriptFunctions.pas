unit CoreWebScriptFunctions;

interface

uses typex, classes, requestinfo, math, Dataobject, webscript, scriptfunctions, variants, variantlist, beeper;

function DispatchScriptCommand(sTagBody, sCommand: string; params: TVariantList; rqInfo: TRequestInfo; var iStartRow, iStartCol: integer; out bHandled: boolean; out bDispatched: boolean): string;


function TestScriptFunction(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList;  out vt: TScriptParameterType): variant;


function Scriptfloatprecision(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
function DecodeTimeonTask(iSeconds: integer; bDiscardSeconds: boolean): string;
function LoadStringFromFile(sFile: string): string;
function CountObjects(doMaster: TDataObject; sSubFieldName, sSubFieldValue: string): integer;

const

  MonthName: array[1..12] of string = ('January', 'February', 'March', 'April',
            'May', 'June', 'July', 'August', 'September', 'October', 'November',
            'December');

implementation

uses  sysutils, webstring, windows, MothershipwebServer,
  webfunctions, webScriptMTDTInterface, exceptions, webresource, errorresource, stringx, stringx.ansi,
  MTDTInterface, VersionInfo, WebConfig, systemx,rights,
  RightsClasses, SystemQueries, DataObjectDefinitions;

function DispatchScriptCommand(sTagBody, sCommand: string; params: TVariantList; rqInfo: TRequestInfo; var iStartRow, iStartCol: integer; out bHandled: boolean; out bDispatched: boolean): string;
begin

  sCommand := lowercase(sCommand);
  bHandled := false;

  raise Exception.create('obsolete');


end;

//----------------------------------------------------------------------------
function DecodeTimeonTask(iSeconds: integer; bDiscardSeconds: boolean): string;
var
  iTemp1, iTemp2: integer;
begin
  iTemp1 := iSeconds;
  //seconds
  iTemp2 := iTemp1 mod 60;
  if iTemp2<10 then
    result := ':0'+inttostr(iTemp2)
  else
    result := ':'+inttostr(iTemp2);

  //if param[1] exists then discard seconds
  if bDiscardSeconds then
    result := '';

  //minutes
  iTemp1 := iTemp1 div 60;
  iTemp2 := iTemp1 mod 60;
  if iTemp2<10 then
    result := ':0'+inttostr(iTemp2)+result
  else
    result := ':'+inttostr(iTemp2)+result;

  //hours
  iTemp2 := iTemp1 div 60;
  //!don't do this iTemp2 := iTemp1 mod 60;
  if iTemp2<10 then
    result := '0'+inttostr(iTemp2)+result
  else
    result := ''+inttostr(iTemp2)+result;
end;



function CountObjects(doMaster: TDataObject; sSubFieldName, sSubFieldValue: string): integer;
var
  t: integer;
begin
  result := 0;
  for t := 0 to doMaster.Objectcount-1 do begin
    if doMaster.obj[t][sSubFieldName].AsString = sSubFieldValue then
      inc(result);
  end;
end;

function LoadStringFromFile(sFile: string): string;
begin
  result := LoadStringFromFile(slash(webserverconfig.ExternalResourcePath)+sFile);
end;





function TestScriptFunction(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList;  out vt: TScriptParameterType): variant;
begin
  result := params[0]*params[1];
  vt := sptNumber;
end;


function ScriptInc(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  stemp: string;
begin
  stemp := sev.rqInfo.response.VarPool[params[0]];
  sTemp := inttostr(strtoint(sTemp)+1);
  sev.rqInfo.response.VarPool[params[0]] := sTemp;
  vt := sptNumber;

end;

function ScriptLoadXML(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  sTemp: string;
begin
  LoadWebResource(sev.rqInfo, sTemp, params[1]); 
  sev.rqInfo.response.XMLPool[params[0]] := sTemp;

end;

function ScriptDateAdd(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  dt1: TDateTime;
  dt2: real;
begin
  dt1 := varAsType(params[0], varDate);
  dt2 := VarAsType(params[1], varDouble);

  result := dt1+dt2;
  vt := sptDateTime;

end;

function ScriptDateSub(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  dt1: TDateTime;
  dt2: real;
begin
  vt := sptDateTime;
  dt1 := varAsType(params[0], varDate);
  dt2 := VarAsType(params[1], varDouble);

  result := dt1-dt2;


end;

function ScriptDecleanInjection(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := uncleaninjection(params[0]);


end;


function ScriptCodeStyle(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  iParens: integer;
  t: integer;
  s,r: string;
  bQuote: boolean;
begin
  vt := sptString;

  s := params[0];


  bQuote := false;
  iParens := 0;
  r := '';

  t := 0;
  while t<length(s) do begin
    inc(t);

    //if a quote then toggle quote state
    if (s[t] = '&') and (copy(s,t,6) = '&#039;') then begin

      bQuote := not bQuote;
      if bQuote then begin
        r := r + '<span class="codestring">&#039;';
      end else begin
        r := r + '&#039;</span>';
      end;

      inc(t,5);
      continue;
    end else
    if not bQuote then begin
      if s[t] = '(' then begin
        r := r + '<span class="codeparen'+(inttostr(iParens mod 4))+'">(</span>';
        inc(iParens);
        continue;
      end;
      if s[t] = ')' then begin
        dec(iParens);
        r := r + '<span class="codeparen'+(inttostr(iParens mod 4))+'">)</span>';
        continue;
      end else begin
        r := r + s[t];
      end;
    end else begin
      r := r + s[t];
    end;

  end;

  if bQuote then begin
    r := r+'</span><span class="codeerror">(!)Unterminated&nbsp;String</span>';
  end;
  if iParens > 0  then begin
    r := r+'<span class="codeerror">(!)Unterminated&nbsp;Paren</span>';
  end;


  result := r;



end;

function ScriptNonBreaking(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := stringReplace(params[0], ' ', '&nbsp;', [rfReplaceAll]);
end;



function ScriptEnd(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  result := '';
  vt := sptString;
end;


function Scriptfor(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  iEndRow: integer;
begin
  //Syntax
  //:for('varname', lowval, highval)
  //OR :for('varname', highval) // defaults the lowval to 1
  //..
  //:++('varname');

  //add a parameter named the same as the first parameter -- set its value to
  //the second parameter
  if params.Count < 3 then begin
    params.add(params[2]);
    params[2] := '1';
  end;
  sev.rqInfo.response.varpool[params[0]] := params[1];

  //find the end of the repeat
  iEndRow := FindEndRepeat(sev.rqInfo.response.content, sev.iStartrow, params[0]);

  //Repeat the html in the area--
  //number of times defined by difference of params 3 and 2
  ReplicateStringListArea(sev.rqInfo.response.content, sev.iStartRow+1, iEndRow,
    strtoint(params[2])-strtoint(params[1])+1,'[[[:inc('''+vartostr(params[0])+''')]]]');

  result := 'Start Repeat for '+vartostr(params[0]);
  vt := sptString;
  sev.bHandled := false;

end;



function ScriptincludeSnip(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  bTemp: boolean;
  sTemp: string;
begin
  //param 0 is file name
  //param 1 TRUE=ignore errors
  if params.count<1 then
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, 'Recursion Variable not specified');

  result := '';
//    LoadWebResource(rqInfo, result, params[0]);

  //load if file exists.. try loading anyway if param[1] is ommitted or is false
  if FileExists(webserverconfig.externalresourcepath+vartostr(params[0])) then begin
    LoadWebResourceSnippet(sev.rqInfo, sTemp, params[0], params[1]);
    sev.rQInfo.response.default('current_scope','root');
    sTemp := '<!--[[[:set(''current_scope'', '''+sev.rqInfo.response.varpool['current_scope']+'/'+vartostr(params[0])+'::'+vartostr(params[1])+''')]]]-->'#13#10+sTemp+#13#10'<!--[[[:set(''current_scope'', '''+sev.rqInfo.response.varpool['current_scope']+''')]]]-->';
    MergeStringLists(sev.rqInfo.response.content, sTemp, sev.iStartRow, sev.iStartCol, sev.iStartrow, sev.iStartcol+length(sev.FullTagBody)+6);
    sev.bHandled := true;
  end;
end;


function Scriptinclude(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  bTemp: boolean;
  sTemp: string;
begin
  //param 0 is file name
  //param 1 TRUE=ignore errors
  if params.count<1 then
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, 'Recursion Variable not specified');

  result := '';

  bTemp := false;
  if params.count > 1 then
    bTemp := params[1] = true;

  //load if file exists.. try loading anyway if param[1] is ommitted or is false
  if (not bTemp) or FileExists(webserverconfig.externalresourcepath+vartostr(params[0])) then begin
    LoadwebResource(sev.rqInfo, sTemp, params[0]);
    sev.rQInfo.response.default('current_scope','root');
    sTemp := '<!--[[[:set(''current_scope'', '''+sev.rqInfo.response.varpool['current_scope']+'/'+vartostr(params[0])+''')]]]Scope:[[[current_scope]]]-->'#13#10+sTemp+#13#10'<!--[[[:set(''current_scope'', '''+sev.rqInfo.response.varpool['current_scope']+''')]]]-->';
    MergeStringLists(sev.rqInfo.response.content, sTemp, sev.iStartRow, sev.iStartCol, sev.iStartrow, sev.iStartcol+length(sev.FullTagBody)+6);
    sev.bHandled := true;
  end;

end;

function ScriptincludeJS(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  bTemp: boolean;
  sTemp: string;
begin
  //param 0 is file name
  //param 1 TRUE=ignore errors
  if params.count<1 then
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, 'Recursion Variable not specified');

  result := '';

  bTemp := false;
  if params.count > 1 then
    bTemp := params[1] = true;

  //load if file exists.. try loading anyway if param[1] is ommitted or is false
  if (not bTemp) or FileExists(webserverconfig.externalresourcepath+vartostr(params[0])) then begin
    LoadwebResource(sev.rqInfo, sTemp, params[0]);
    sev.rQInfo.response.default('current_scope','root');
    //sTemp := '<!--[[[:set(''current_scope'', '''+sev.rqInfo.response.varpool['current_scope']+'/'+vartostr(params[0])+''')]]]Scope:[[[current_scope]]]-->'#13#10+sTemp+#13#10'<!--[[[:set(''current_scope'', '''+sev.rqInfo.response.varpool['current_scope']+''')]]]-->';
    MergeStringLists(sev.rqInfo.response.content, sTemp, sev.iStartRow, sev.iStartCol, sev.iStartrow, sev.iStartcol+length(sev.FullTagBody)+6);
    sev.bHandled := true;
  end;

end;

function ScriptincludeBody(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  bTemp: boolean;
  sTemp: string;
begin
  //param 0 is file name
  //param 1 TRUE=ignore errors
  if params.count<1 then
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, 'Recursion Variable not specified');

  result := '';

  bTemp := true;

  //load if file exists.. try loading anyway if param[1] is ommitted or is false
  if (not bTemp) or FileExists(webserverconfig.externalresourcepath+vartostr(params[0])) then begin
    LoadwebResource(sev.rqInfo, sTemp, params[0]);
    sev.rQInfo.response.default('current_scope','root');
    sTemp := '<!--[[[:set(''current_scope'', '''+sev.rqInfo.response.varpool['current_scope']+'/'+vartostr(params[0])+''')]]]Scope:[[[current_scope]]]-->'#13#10+sTemp+#13#10'<!--[[[:set(''current_scope'', '''+sev.rqInfo.response.varpool['current_scope']+''')]]]-->';
    MergeStringLists(sev.rqInfo.response.content, sTemp, sev.iStartRow, sev.iStartCol, sev.iStartrow, sev.iStartcol+length(sev.FullTagBody)+6);
    sev.bHandled := true;
  end;

end;



function ScriptWebSafe(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  result := EncodeXMLString(DecodeWebString(params[0]));
  vt := sptString;
end;



function Scriptrightscheckboxlist(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  result := RightsCheckboxList(sev.rqInfo, strtoint(params[0]),params[1], sev.rqInfo.request.hasparam('nofilter'));
  vt := sptString;
end;

//------------------------------------------------------------------------------

function Scriptreadonlyrightscheckboxlist(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  result := readonlyRightsCheckboxList(sev.rqInfo, strtoint(params[0]),params[1], sev.rqInfo.request.hasparam('nofilter'));
  vt := sptString;
end;

//------------------------------------------------------------------------------

function Scriptrealsession(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := inttostr(sev.rqInfo.sessionid);
end;

//----------------------------------------------------------------------------

function ScriptBuildDate(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := vartostr(params[0])+'/'+vartostr(params[1])+'/'+vartostr(params[2]);
end;

//------------------------------------------------------------------------------

function Scriptlinkif(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  sTemp: string;
  iEndcol, iEndRow: integer;

begin
  vt := sptString;
  //if condition NOT definted -- erase HTML in conditional area...else
  //do nothing but replace the tag

  if not ('true'= lowercase(params[0])) then begin
    result := '';
    //Find the end of the conditional area [[[:end/fieldname]]]

    if params.count<2 then
      sTemp := ''
    else
      sTemp := ''''+vartostr(params[1])+'''';
    //Find the endiftag
    FindEnd(sev.rqInfo.response.content, '[[[:end('+sTemp+')]]', sev.iStartRow, iEndCol, iEndRow);
    //Erase the conditional area
    result := DeleteStringHyperlinks(sev.rqINfo.response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow);

    sev.bHandled := true;

  end else begin
    result := '';
  end;
end;
//------------------------------------------------------------------------------

function Scriptwrapif(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  //if condition TRUE then ADD the string from params[1]
  // and WRAP params[1] with  params[2] string
  //else just ADD params[1]
  if ('true'= lowercase(params[0])) then begin
    result := vartostr(params[2]) + vartostr(params[1]) +vartostr(params[2]);
  end else begin
    result := params[1];
  end;
end;

//------------------------------------------------------------------------------

function Scriptneed(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  sTemp: string;
  iEndcol, iEndRow: integer;
begin
  vt := sptString;
  //if condition NOT definted -- erase HTML in conditional area...else
  //do nothing but replace the tag

  if not (CurrentUserHasRight(sev.rqINfo, lowercase(params[0]))) then begin
    result := '';
    //Find the end of the conditional area [[[:end/fieldname]]]

    if params.count<3 then
      sTemp := params[0]
    else
      sTemp := ''''+vartostr(params[3])+'''';
    //Find the endiftag
    FindEnd(sev.rqInfo.response.content, '[[[:end('''+sTemp+''')]]]', sev.iStartRow, iEndCol, iEndRow);
    //Erase the conditional area
    if (params.count > 1) and (lowercase(params[1]) = 'true') then
      result := DeleteStringHyperlinks(sev.rqInfo.response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow)
    else begin
      DeleteStringListArea(sev.rqInfo.response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow);
    end;

    sev.bHandled := true;

  end else begin
    result := '';
  end;
end;
//------------------------------------------------------------------------------

function Scriptban(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  sTemp: string;
  iEndCol, iEndRow: integer;
begin
  vt := sptString;

  //if condition NOT definted -- erase HTML in conditional area...else
  //do nothing but replace the tag

  if (CurrentUserHasRight(sev.rqINfo, lowercase(params[0]))) then begin
    result := '';
    //Find the end of the conditional area [[[:end/fieldname]]]

    if params.count<3 then
      sTemp := params[0]
    else
      sTemp := ''''+vartostr(params[3])+'''';
    //Find the endiftag
    FindEnd(sev.rqInfo.response.content, '[[[:end('''+sTemp+''')]]]', sev.iStartRow, iEndCol, iEndRow);
    //Erase the conditional area
    if (params.count > 1) and (lowercase(params[1]) = 'true') then
      result := DeleteStringHyperlinks(sev.rqInfo.response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow)
    else begin
      DeleteStringListArea(sev.rqInfo.response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow);
    end;

    sev.bHandled := true;

  end else begin
    result := '';
  end;
end;

//------------------------------------------------------------------------------

function Scriptneedall(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  bTemp: boolean;
  t: integer;
  sTemp: string;
  iendRow, iEndcol: integer;
begin
  vt := sptString;
  //if condition NOT definted -- erase HTML in conditional area...else
  //do nothing but replace the tag

  bTemp := true;
  for t:= 0 to params.count-2 do begin
    bTemp := CurrentUserHasRight(sev.rqInfo, params[t]) and bTemp;

    {$IFNDEF EVAL_ALL_RIGHTS}
    if not bTemp then
      break;
    {$ENDIF}
  end;

  if not (bTemp) then begin
    result := '';
    //Find the end of the conditional area [[[:end/fieldname]]]

    sTemp := ''''+vartostr(params[0])+'''';
    //Find the endiftag
    FindEnd(sev.rqInfo.response.content, '[[[:end('+sTemp+')]]]', sev.iStartRow, iEndCol, iEndRow);
    //Erase the conditional area
    bTemp := (sCommand = ':needall_link ');
    if (bTemp) then
      result := DeleteStringHyperlinks(sev.rqInfo.response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow)
    else begin
      DeleteStringListArea(sev.rqInfo.response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow);
    end;

    sev.bHandled := true;

  end else begin
    result := '';
  end;
end;
//------------------------------------------------------------------------------

function Scriptneedany(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  bTemp: boolean;
  stemp: string;
  iEndRow, iendCol: integer;
  t: integer;
begin
  vt := sptString;
  //if condition NOT definted -- erase HTML in conditional area...else
  //do nothing but replace the tag

  bTemp := false;
  for t:= 0 to params.count-1 do begin
    bTemp := CurrentUserHasRight(sev.rqInfo, params[t]) or bTemp;

    {$IFNDEF EVAL_ALL_RIGHTS}
    if bTemp then
      break;
    {$ENDIF}
  end;

  if not (bTemp) then begin
    result := '';
    //Find the end of the conditional area [[[:end/fieldname]]]

    sTemp := ''''+vartostr(params[0])+'''';
    //Find the endiftag
    FindEnd(sev.rqINfo.response.content, '[[[:end('+sTemp+')]]]', sev.iStartRow, iEndCol, iEndRow);
    //Erase the conditional area
    bTemp := (sCommand = ':needany_link ');
    if bTemp then
      result := DeleteStringHyperlinks(sev.rqINfo.Response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow)
    else begin
      DeleteStringListArea(sev.rqinfo.response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow);
    end;

    sev.bHandled := true;

  end else begin
    result := '';
  end;
end;
//------------------------------------------------------------------------------

function Scriptgreaterorequal(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if strtofloat(params[0]) >= strtofloat(params[1]) then
    result := 'TRUE'
  else
    result := 'FALSE';

end;

//------------------------------------------------------------------------------

function Scriptlessorequal(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if strtofloat(params[0]) <= strtofloat(params[1]) then
    result := 'TRUE'
  else
    result := 'FALSE';

end;

//------------------------------------------------------------------------------

function Scriptlessthan(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;

  if strtofloat(params[0]) < strtofloat(params[1]) then
    result := 'TRUE'
  else
    result := 'FALSE';

end;

//------------------------------------------------------------------------------

function Scriptgreaterthan(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;

  if strtofloat(params[0]) > strtofloat(params[1]) then
    result := 'TRUE'
  else
    result := 'FALSE';

end;
//------------------------------------------------------------------------------

function Scriptifgreater(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  sTemp: string;
  iEndRow, iEndcol: integer;

begin
  vt := sptString;

  //if condition NOT definted -- erase HTML in conditional area...else
  //do nothing but replace the tag
  if NOT (strtoint(lowercase(params[0])) > strtoint(lowercase(params[1]))) then begin
    result := '';
    //Find the end of the conditional area [[[:end/fieldname]]]

    if params.count<3 then
      sTemp := ''
    else
      sTemp := ''''+vartostr(params[2])+'''';
    //Find the endiftag
    FindEnd(sev.rqInfo.response.content, '[[[:end('+sTemp+')]]]', sev.iStartRow, iEndCol, iEndRow);
    //Erase the conditional area
    DeleteStringListArea(sev.rqInfo.response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow);
    sev.bHandled := true;
  end else begin
    result := '';
  end;
end;
//------------------------------------------------------------------------------

function Scriptifngreater(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  sTemp: string;
  iEndCol, iEndrow: integer;
begin
  vt := sptString;
  //if condition NOT definted -- erase HTML in conditional area...else
  //do nothing but replace the tag
  if (strtoint(lowercase(params[0])) > strtoint(lowercase(params[1]))) then begin
    result := '';
    //Find the end of the conditional area [[[:end/fieldname]]]

   if params.count<3 then
      sTemp := ''
    else
      sTemp := ''''+vartostr(params[2])+'''';
    //Find the endiftag
    FindEnd(sev.rqInfo.response.content, '[[[:end('+sTemp+')]]]', sev.iStartRow, iEndCol, iEndRow);
    //Erase the conditional area
    DeleteStringListArea(sev.rqInfo.response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow);
    sev.bHandled := true;
  end else begin
    result := '';
  end;
end;
//------------------------------------------------------------------------------

function Scriptifnequal(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  sTemp: string;
  iEndRow, iEndcol: integer;

begin
  vt := sptString;
  //if condition definted -- erase HTML in conditional area...else
  //do nothing but replace the tag
  if lowercase(params[0]) = lowercase(params[1]) then begin
    result := '';
    //Find the end of the conditional area [[[:end/fieldname]]]

    if params.count<3 then
      sTemp := ''
    else
      sTemp := ''''+vartostr(params[2])+'''';
    //Find the endiftag
    FindEnd(sev.rqInfo.response.content, '[[[:end('+sTemp+')]]]', sev.iStartRow, iEndCol, iEndRow);
    //Erase the conditional area
    DeleteStringListArea(sev.rqInfo.response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow);
    sev.bHandled := true;
  end else begin
    result := '';
  end;
end;
//------------------------------------------------------------------------------

function Scriptband(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := inttostr(strtoint(params[0]) and strtoint(params[1]));
end;



//------------------------------------------------------------------------------

function Scriptsleep(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  windows.sleep(strtoint(params[0]));
  result := '';
end;

//------------------------------------------------------------------------------

function Scriptjava(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  stemp2: string;
begin
  vt := sptString;
  sTemp2 := params[0];
  result := stringReplace(sTemp2, '''', '\''', [RFReplaceAll]);
  result := stringReplace(result, '"','%'+inttohex(ord('"'), 2), [RFReplaceAll]);
end;

//------------------------------------------------------------------------------

function Scriptrebuildinlineparameters(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := sev.rqInfo.Request.RebuildInlineParameters;
end;
//------------------------------------------------------------------------------

function Scriptstringreplace(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := stringReplace(params[0],(params[1]),(params[2]),[rfReplaceAll, rfIgnoreCase]);
end;
//------------------------------------------------------------------------------

function Scripteostyle(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if strtoint(params[0]) mod 2 = 0 then
    result := 'class='+vartostr(params[1])+'even'
  else
    result := 'class='+vartostr(params[1])+'odd';
end;
//------------------------------------------------------------------------------

function Scriptrightscheckbox(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  iTemp: integer;
  bTemp: boolean;
  t: integer;

begin
  vt := sptString;
  //param[0] is EXECTYPE
  iTemp := strtoint(params[0]); //roleid
  result := '';

  //QueryPerformanceCounter(bench1);
  bTemp := RoleHasRight(sev.rqInfo, params[1], iTemp);
  //QueryPerformanceCounter(bench2);
  //result := inttostr(bench2-bench1);
  if not CurrentUserHasRight(sev.rqInfo, params[1]) then begin
    result := ''
  end else begin
    //QueryPerformanceCounter(bench1);
    //result := result+':'+inttostr(bench1-bench2)+':';
    result := result +'<input type="checkbox" name="RC_'+vartostr(params[1])+'" value="'+vartostr(params[1])+'"';
    if bTemp then result := result +'CHECKED';
    result := result + '>'+DecodeWebString(params[2]);

  end;
end;
//------------------------------------------------------------------------------

function Scriptset(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if params.count<2 then
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, ':Set requires two params, only '+inttostr(params.count)+' was passed');
  //sev.rqInfo.request[params[0]] := params[1];
  sev.rqInfo.response.varpool[params[0]] := params[1];
  result := '';
end;
//------------------------------------------------------------------------------

function Scriptgethelpfile(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  //endif tag
  if params.count > 0 then
    result := WebServer.Help.LookupHelpPage(sev.rqInfo, params[0])
  else
    result := WebServer.Help.LookupHelpPage(sev.rqInfo);
end;
//------------------------------------------------------------------------------

function Scriptbookmarkthis(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if params.count > 0 then
    BookmarkThis(sev.rqInfo, params[0])
  else
    BookmarkThis(sev.rqInfo);
  result := '';
end;
//------------------------------------------------------------------------------

function Scriptgetbookmark(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if params.count > 0 then
    result := GetBookMark(sev.rQInfo, params[0])
  else
    result := GetBookMark(sev.rQInfo);

end;
//------------------------------------------------------------------------------

function Scriptsetobjectfromsubobject(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;

  //endif tag
  if params.count<3 then
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, ':Set requires two params, only '+inttostr(params.count)+' was passed');
  sev.rqInfo.response.objectpool[params[0]] := sev.rqInfo.response.objectpool[params[1]].obj[strtoint(params[2])];
  result := '';
end;
//------------------------------------------------------------------------------

function Scriptdefault(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  //endif tag
  if not sev.rqInfo.response.HasVar(params[0]) then begin
    if params.count<2 then
      raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, ':Set requires two params, only '+inttostr(params.count)+' was passed');
    sev.rqInfo.request[params[0]] := params[1];
    sev.rqInfo.response.varpool[params[0]] := params[1];
  end;
  result := '';
end;
//------------------------------------------------------------------------------

function Scriptif(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  iEndRow, iEndcol: integer;
begin
  vt := sptString;
  //Find the end of the conditional area [[[:end/fieldname]]]

  //if condition NOT definted -- erase HTML in conditional area...else
  //do nothing but replace the tag
  if NOT sev.rqInfo.response.HasVar(params[0]) then begin
    result := '';
    //Find the end of the conditional area [[[:end/fieldname]]]
    FindEnd(sev.rqInfo.response.content, '[[[:end('''+vartostr(params[0])+''')]]]', sev.iStartRow, iEndCol, iEndRow);
    //Erase the conditional area
    DeleteStringListArea(sev.rqInfo.response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow);
    sev.bHandled := true;
  end else begin
    result := '';
  end;
end;
//------------------------------------------------------------------------------

function Scriptbif(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  sTemp: string;
  iendcol, iEndrow: integer;
begin
  vt := sptString;
  //Find the end of the conditional area [[[:end/fieldname]]]

  //if condition NOT definted -- erase HTML in conditional area...else
  //do nothing but replace the tag
  if NOT (lowercase(params[0]) = 'true') then begin
    result := '';

    if params.count > 1 then
      sTemp := ''''+vartostr(params[1])+''''
    else
      sTemp := '';


    //Find the end of the conditional area [[[:end/fieldname]]]
    FindEnd(sev.rqInfo.response.content, '[[[:end('+sTemp+')]]]', sev.iStartRow, iEndCol, iEndRow);
    //Erase the conditional area
    DeleteStringListArea(sev.rqInfo.response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow);
    sev.bHandled := true;
  end else begin
    result := '';
  end;
end;
//------------------------------------------------------------------------------

function Scriptifn(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  iEndRow, iEndCol: integer;
begin
  vt := sptString;
  //if condition NOT definted -- erase HTML in conditional area...else
  //do nothing but replace the tag
  if sev.rqInfo.response.HasVar(params[0]) then begin
    result := 'conditional not met - '+vartostr(params[0]);
    //Find the end of the conditional area [[[:end/fieldname]]]
    FindEnd(sev.rqInfo.response.content, '[[[:end('''+vartostr(params[0])+''')]]]', sev.iStartRow, iEndCol, iEndRow);
    //Erase the conditional area
    DeleteStringListArea(sev.rqInfo.response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow);
    sev.bHandled := true;
  end else begin
    result := '';
  end;
end;

//------------------------------------------------------------------------------

function Scriptmonthstr(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  iTemp1: integer;
begin
  vt := sptString;
  // get month name for month number.
  iTemp1 := StrToIntDef(params[0],0);
  if (iTemp1>0) and (iTemp1<13) then begin
    result:=MonthName[iTemp1];
  end else begin
    result := '';
  end;
end;
//------------------------------------------------------------------------------

function Scriptchunk(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := '';
  sev.rqInfo.response.SendChunk(sev.istartrow-1);
  sev.rqInfo.Response.content.delete(0);
  sev.iStartRow := 0;
  sev.iStartCol := 1;
end;
//------------------------------------------------------------------------------
function Scriptfloatprecision(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  iTemp: integer;
  iPow: integer;
  rTemp: real;
  iTemp2: integer;
begin
  vt := sptString;


  iTemp := strtoint(params[1]);
  iPOw := round(power(10, iTemp));
  rTemp := params[0]*iPow;
  iTemp2 := round(rTemp);
  rTemp := iTemp2 / iPow;

  result := floattostr(rTemp);
  if iTemp > 0 then begin
    if pos('.', result) = 0 then
      result := result+'.';

    while (length(result) - pos('.', result) ) < iTemp do
      result := result + '0';

  end;
end;
//------------------------------------------------------------------------------

function ScriptDollar(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  iTemp: integer;
begin
  vt := sptString;
  iTemp := round(strtofloat(params[0])*100) mod 100;
  result := inttostr(iTemp);
  if length(result) = 1 then
    result := '0'+result;

  result := '$'+inttostr(trunc(strtofloat(params[0])))+'.'+result;
end;
//------------------------------------------------------------------------------

function Scripteval(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := handlescript(params[0], sev.rqInfo);
end;

//------------------------------------------------------------------------------

function Scriptpercentprecision(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  iTemp: integer;
begin
  vt := sptString;
  if vartostr(params[0]) = '' then
    result := '0%'
  else begin
    if params.count > 1 then
      iTemp := strtoint(params[1])
    else
      iTemp := 2;
    result := floattostr(100*(round(strtofloat(params[0])*(power(10,iTemp))) / power(10,iTemp)))+'%';
  end;
end;

//------------------------------------------------------------------------------

function Scriptcheck(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if lowercase(params[0]) = 'true' then
    result := 'CHECKED'
  else
    result := '';
end;
//------------------------------------------------------------------------------

function Scriptselect(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if lowercase(vartostr(params[0])) = 'true' then
    result := 'SELECTED'
  else
    result := '';
end;

//------------------------------------------------------------------------------

function Scripteq(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  v1,v2: variant;
begin
  vt := sptString;

  v1 := params[0];
  v2 := params[1];

  if (vartype(v1)=varString) or (varTYpe(v2)=varString) then begin
    v1 := lowercase(vartostr(params[0]));
    v2 := lowercase(vartostr(params[1]));
  end;

  if v1 = v2 then
    result := 'true'
  else
    result := 'false';
end;

function Scriptnoteq(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  v1,v2: variant;
begin
  vt := sptString;

  v1 := params[0];
  v2 := params[1];

  if (vartype(v1)=varString) or (varTYpe(v2)=varString) then begin
    v1 := lowercase(vartostr(params[0]));
    v2 := lowercase(vartostr(params[1]));
  end;
    if v1 <> v2 then
      result := 'true'
    else
      result := 'false';
end;

//------------------------------------------------------------------------------

function Scriptgeq(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if strtofloat(lowercase(params[0])) >= strtofloat(lowercase(params[1])) then
    result := 'true'
  else
    result := 'false';
end;
//------------------------------------------------------------------------------

function Scriptgt(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if strtofloat(lowercase(params[0])) > strtofloat(lowercase(params[1])) then
    result := 'true'
  else
    result := 'false';
end;
//------------------------------------------------------------------------------
function Scriptlt(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if strtofloat(lowercase(params[0])) < strtofloat(lowercase(params[1])) then
    result := 'true'
  else
    result := 'false';
end;

//------------------------------------------------------------------------------

function Scriptleq(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;

  if strtofloat(lowercase(params[0])) <= strtofloat(lowercase(params[1])) then
    result := 'true'
  else
    result := 'false';
end;
//------------------------------------------------------------------------------

function Scriptdocument(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := sev.rqInfo.request.document;
end;

function Scriptoriginaldocument(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := sev.rqInfo.request.document+'?'+sev.rqInfo.request.rebuildinlineparameters;
end;


//------------------------------------------------------------------------------
function Scriptdocumentnoslash(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := copy(sev.rqInfo.request.document, 2, length(sev.rqInfo.request.document));
end;

//------------------------------------------------------------------------------

function Scriptsort(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  sev.rqInfo.response.objectpool[params[0]].Sort(strtoint(params[1]));
  result := '';
end;
//------------------------------------------------------------------------------

function Scriptyear(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptNumber;
  result := 0;
  if vartostr(params[0]) <> '' then
    result := GetYearFromDate(params[0]);
end;
//------------------------------------------------------------------------------

function Scriptmonth(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  f: real;
begin
  vt := sptNumber;
  result := 0;
  if varType(params[0]) in [varCurrency, varSingle, varDouble, varInteger] then begin
    f := params[0];
    result := GetMonthFromDate(params[0]);
  end else
  if (vartostr(params[0]) <> '') and (vartostr(params[0])  <> '0')then
    result := GetMonthFromDate(params[0]);
end;
//------------------------------------------------------------------------------

function Scriptday(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptNumber;
  result := 0;
  if vartostr(params[0]) <> '' then
    result := GetDayFromDate(params[0]);
end;
//------------------------------------------------------------------------------

function Scriptabs(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  iTemp: integer;
begin
  vt := sptString;
  iTemp := strtoint(params[0]);
  if iTemp < 0 then
    result := '('+inttostr(abs(iTemp))+')'
  else
    result := inttostr(iTemp);
end;
//------------------------------------------------------------------------------

function ScriptdecodeInlineURL(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := DecodeInlineURL(params[0]);
end;
//------------------------------------------------------------------------------

function Scriptslice(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := copy(params[0], strtoint(params[1]), strtoint(params[2]));
end;
//------------------------------------------------------------------------------

function Scriptobjectcount(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := inttostr((sev.rqInfo.response.objectpool[params[0]].ObjectCount));
end;
//------------------------------------------------------------------------------

function Scriptcountbyvalue(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  iTemp1: integer;
begin
  vt := sptString;
  if params.count <> 3 then
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, ':CountByValue requires three params, '+inttostr(params.count)+' were passed');
  iTemp1 := 0;

  result := inttostr(CountObjects(sev.rqInfo.response.objectpool[params[0]], params[1], params[2]));
end;
//------------------------------------------------------------------------------

function Scriptrolehasright(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if RoleHasRight(sev.rqInfo, lowercase(params[0]), strtoint(params[1])) then begin
    result := 'true';
  end else begin
    result := 'false';
  end;
end;

//------------------------------------------------------------------------------

function Scripthasright(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if CurrentUserHasRight(sev.rqInfo, lowercase(params[0])) then begin
    result := 'true';
  end else begin
    result := 'false';
  end;
end;
//------------------------------------------------------------------------------

function Scripthasallrights(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  btemp: boolean;
  t: integer;
begin
  vt := sptString;
  bTemp := true;
  for t:= 0 to params.count-1 do begin
    bTemp := CurrentUserHasRight(sev.rqInfo, params[t]) and bTemp;

    {$IFNDEF EVAL_ALL_RIGHTS}
    if not bTemp then
      break;
    {$ENDIF}
  end;

  if bTemp then
    result := 'true'
  else
    result := 'false';
end;
//------------------------------------------------------------------------------

function Scripthasanyrights(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  btemp: boolean;
  t: integer;
begin
  vt := sptString;
  bTemp := false;
  for t:= 0 to params.count-1 do begin
    bTemp := CurrentUserHasRight(sev.rqInfo, params[t]) or bTemp;

    {$IFNDEF EVAL_ALL_RIGHTS}
    if bTemp then
      break;
    {$ENDIF}
  end;

  if bTemp then
    result := 'true'
  else
    result := 'false';
end;
//------------------------------------------------------------------------------

function Scriptloadvar(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;

  if params.count > 1 then
    result := sev.rqInfo.LoadVar(params[0], string(params[1]))
  else
    result := sev.rqInfo.LoadVar(params[0])
end;

//------------------------------------------------------------------------------
function Scriptloaduservar(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  s: string;
  i: ni;
  s1,s2: string;
begin
  vt := sptString;
  result := '';
  if params.count > 2 then begin
    i := params[3];
    s1 := params[0];
    s2 := params[2];
    s := systemqueries.LoadUserVar(sev.rqINfo, s1, s2, i);
    sev.rqINfo.Response.VarPool[params[1]] := s;
  end else
  if params.count > 1 then begin
    s := systemqueries.LoadUserVar(sev.rqINfo, params[0]);
    sev.rqINfo.Response.VarPool[params[1]] := s;
  end else begin
    s := systemqueries.LoadUserVar(sev.rqINfo, params[0]);
    sev.rqINfo.Response.VarPool[params[0]] := s;
  end;
end;
//------------------------------------------------------------------------------
function Scriptsaveuservar(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
var
  s: string;
begin
  vt := sptString;
  result := '';
  if params.count > 2 then begin
    systemqueries.SaveUserVar(sev.rqInfo, params[0], params[1], params[2]);
  end else begin
    systemqueries.SaveUserVar(sev.rqInfo, params[0], params[1]);
  end;
end;
//------------------------------------------------------------------------------

function Scriptdateonly(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := formatdatetime('M/D/YYYY', params[0]);
end;

//------------------------------------------------------------------------------

function Scriptsavevar(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if params.count = 1 then begin
    sev.rqInfo.savevar(params[0], sev.rqInfo.response.varpool[params[0]]);
  end else begin
    sev.rqInfo.SaveVar(params[0], params[1]);
  end;
  result := '';
end;


//------------------------------------------------------------------------------

function Scriptformatdatetime(sTagbody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := formatdatetime(params[0], strtodatetime(params[1]));
end;


//------------------------------------------------------------------------------

function Scripttoday(sTagBody:string; sCommand: string; var sev: TScriptEngineVArs; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptDAteTime;
  result := (trunc(now)+0.0);
end;


function Scriptnow(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptNumber;
  result := now;
end;


//----------------------------------------------------------------------------

function Scriptwebsafetext(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := CRtoBR(EncodeXMLString(DecodeWebString(params[0])));

end;


//----------------------------------------------------------------------------

function Scriptinttorank(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := inttorank(strtoint(params[0]));
end;
//----------------------------------------------------------------------------

function Scriptgetobjectcount(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := inttostr(sev.rqInfo.response.objectpool[params[0]].objectcount);
end;


//----------------------------------------------------------------------------

function ScriptUrl(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := EncodeWebString(params[0]);
end;

//----------------------------------------------------------------------------
function ScriptAmp(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := AmpEncode(params[0]);
end;


//------------------------------------------------------------------------------

function Scriptor(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType):variant;
var
  sTemp: string;
  iEndCol, iEndrow: integer;

begin
  vt := sptString;
  // If params[0]==params[2] OR params[1]==params[3] then show HTML
  // if NOT then erase the HTML area.
  if not ( (lowercase(params[0]) = lowercase(params[1])) or
   (lowercase(params[2]) = lowercase(params[3]))) then begin

    result := '';
    //Find the end of the conditional area [[[:end/fieldname]]]

    if params.count<5 then
      sTemp := ''
    else
      sTemp := ''''+vartostr(params[4])+'''';
    //Find the endiftag
    FindEnd(sev.rqInfo.response.content, '[[[:end('+sTemp+')]]]', sev.iStartRow, iEndCol, iEndRow);
    //Erase the conditional area
    DeleteStringListArea(sev.rqInfo.response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow);
    sev.bHandled := true;
  end else begin
    result := '';
  end;
end;
//------------------------------------------------------------------------------

function Scriptrepeatforobject(sTagBody, sCommand: string; var sev: TScriptEngineVars; params: TScriptParamList; out vt: TScriptParameterType):variant;
var
  iEndrow: integer;
begin
  vt := sptString;
  iEndRow := FindEndRepeat(sev.rqInfo.response.content, sev.iStartrow, params[0]);

  result := '';

  ReplicateStringListArea(sev.rqInfo.response.content, sev.iStartRow+1, iEndRow,
    sev.rqInfo.response.ObjectPool[params[0]].ObjectCount);


  sev.rqInfo.response.VarPool[params[0]] := '0';
end;
//------------------------------------------------------------------------------

function ScriptforeachobjectJS(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  iEndrow: integer;
  iTemp1: integer;
  iTemp: integer;
  iEndcol: integer;
begin
  vt := sptString;

  iEndRow := FindEndRepeat(sev.rqInfo.response.content, sev.iStartrow, params[0]);



  result := 'Start Repeat for '+vartostr(params[0])+' -- x '+inttostr(sev.rqInfo.response.ObjectPool[params[0]].ObjectCount);


  if params.count > 1 then begin
    iTemp1 := strtoint(params[1]);//number to show
    sev.rqInfo.request.default(params[0]+'.pagesize', inttostr(iTEmp1));
  end else begin
    sev.rqInfo.request.default(params[0]+'.pagesize', '999999');
    sev.rqInfo.SetupDefaultVarPool;
    iTemp1 := strtoint(sev.rqInfo.response.varpool[params[0]+'.pagesize']);
  end;

  if params.count > 2 then begin
    iTemp := strtoint(params[2]);//start row
  end else begin
    sev.rqInfo.request.default(params[0]+'.start', '1');
    sev.rqInfo.SetupDefaultVarPool;
    iTemp := strtoint(sev.rqInfo.response.varpool[params[0]+'.start'])-1;
  end;

  //if end of set is closer than number to show
  if sev.rqInfo.response.ObjectPool[params[0]].ObjectCount - iTemp < iTemp1 then
    //reduce number of rows to see
    iTemp1 := sev.rqInfo.response.ObjectPool[params[0]].ObjectCount - iTemp;

  //set variable to start
  sev.rqInfo.response.varpool[params[0]] := inttostr(iTemp);


  //setup helper variables
  sev.rqinfo.response.varpool[params[0]+'.start'] := inttostr(iTemp+1);
  sev.rqinfo.response.varpool[params[0]+'.end'] := inttostr(iTemp+iTemp1);
  sev.rqinfo.response.varpool[params[0]+'.total'] := inttostr(sev.rqInfo.response.objectpool[params[0]].ObjectCount);
//    rqinfo.response.varpool[params[0]+'.pagesize'] := inttostr(iTemp1);

  sev.rqinfo.response.varpool[params[0]] := inttostr(iTemp);
//    rqInfo.SetupDefaultVarPool;

  ReplicateStringListArea(sev.rqInfo.response.content, sev.iStartRow+1, iEndRow, iTemp1, '//[[['+vartostr(params[0])+']]]--[[[:inc('''+vartostr(params[0])+''')]]]--[[['+vartostr(params[0])+']]]');
end;
//------------------------------------------------------------------------------

function Scriptforeachobject(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  iEndrow: integer;
  iTemp1: integer;
  iTemp: integer;
  iEndcol: integer;
begin
  //object_and_varpool_iterator_name,[limit_to_show],[limit_start]
  vt := sptString;

  iEndRow := FindEndRepeat(sev.rqInfo.response.content, sev.iStartrow, params[0]);



  result := 'Start Repeat for '+vartostr(params[0])+' -- x '+inttostr(sev.rqInfo.response.ObjectPool[params[0]].ObjectCount);


  if params.count > 1 then begin
    iTemp1 := strtoint(params[1]);//number to show
    sev.rqInfo.request.default(params[0]+'.pagesize', inttostr(iTEmp1));
  end else begin
    sev.rqInfo.request.default(params[0]+'.pagesize', '999999');
    sev.rqInfo.SetupDefaultVarPool;
    iTemp1 := strtoint(sev.rqInfo.response.varpool[params[0]+'.pagesize']);
  end;

  if params.count > 2 then begin
    iTemp := strtoint(params[2]);//start row
  end else begin
    sev.rqInfo.request.default(params[0]+'.start', '1');
    sev.rqInfo.SetupDefaultVarPool;
    iTemp := strtoint(sev.rqInfo.response.varpool[params[0]+'.start'])-1;
  end;

  //if end of set is closer than number to show
  if sev.rqInfo.response.ObjectPool[params[0]].ObjectCount - iTemp < iTemp1 then
    //reduce number of rows to see
    iTemp1 := sev.rqInfo.response.ObjectPool[params[0]].ObjectCount - iTemp;

  //set variable to start
  sev.rqInfo.response.varpool[params[0]] := iTemp;


  //setup helper variables
  sev.rqinfo.response.varpool[params[0]+'.start'] := iTemp+1;
  sev.rqinfo.response.varpool[params[0]+'.end'] := iTemp+iTemp1;
  sev.rqinfo.response.varpool[params[0]+'.total'] := sev.rqInfo.response.objectpool[params[0]].ObjectCount;
//    rqinfo.response.varpool[params[0]+'.pagesize'] := inttostr(iTemp1);

  sev.rqinfo.response.varpool[params[0]] := iTemp;
//    rqInfo.SetupDefaultVarPool;

  ReplicateStringListArea(sev.rqInfo.response.content, sev.iStartRow+1, iEndRow, iTemp1, '<!--[[['+vartostr(params[0])+']]]--><!--[[[:inc('''+vartostr(params[0])+''')]]]--><!--[[['+vartostr(params[0])+']]]-->');
end;
//------------------------------------------------------------------------------

function Scriptforeachfooter(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
//format: foreachfooter('friendly name', 'varname', varname, color)
  sTemp: string;
  itemp: integer;
  iTemp1: integer;
  iTemp2: integer;
  t: integer;
begin
  vt := sptString;
  sTemp := params[1];//entity being tracked
  iTemp := strtoint(sev.rqInfo.response.varpool[params[1]+'.start'])-1;
  iTemp1 := strtoint(sev.rqInfo.response.varpool[params[1]+'.pagesize']);
  iTemp2 := sev.rqInfo.response.objectpool[params[1]].objectcount;

  if iTemp2 = 0 then begin
    result := '<i>(empty)</i>';
    exit;
  end;

  result := 'showing '+vartostr(params[0])+' <font color="'+vartostr(params[2])+'">[[['+sTemp+'.start]]]</font> to <font color="'+vartostr(params[2])+'">[[['+sTemp+'.end]]]</font> of <font color="'+vartostr(params[2])+'">[[['+sTemp+'.total]]]</font>';

  result := result + '<br>';

  if (iTemp) > 1 then
    result := result + '<BR><a href="'+AdjustURLForParams(Params[3])+vartostr(params[1])+'.start='+inttostr((iTemp-iTemp1)+1)+'">&lt;prev&lt;</a>';

  if iTemp1<strtoint(sev.rqINfo.response.varpool[sTemp+'.total']) then begin
    sTemp := params[3];
    for t:=0 to (strtoint(sev.rqinfo.response.varpool[params[1]+'.total'])-1) div iTEmp1 do begin
      if (t*iTemp1) = iTemp then begin
        result := result + '<span class="pageland">'+inttostr(t+1)+'</span> ';
      end else begin
        result := result +' <a class="pagelink" href="'+AdjustURLForParams(vartostr(Params[3]))+vartostr(params[1])+'.start='+inttostr((t*(iTemp1)+1))+'">'+ inttostr(t+1)+'</a> ';
      end;
    end;
  end;
  if (iTemp+iTemp1) < iTemp2 then
    result := result + '<a href="'+AdjustURLForParams(vartostr(Params[3]))+vartostr(params[1])+'.start='+inttostr((iTemp+iTemp1)+1)+'">&gt;next&gt;</a>';

end;
//--------------------------------------------------------------------------------

function Scriptxml(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  sLeft: string;
  sRight: string;
begin
  vt := sptString;
  //syntax :xml(sBaseNode, sLeafNode)
  splitstring(params[0],'$', sLeft, sRight);

  result := sev.rqInfo.response.xmlDocs[sLeft].ReadValue(sRight+vartostr(params[1]));

end;
//------------------------------------------------------------------------------

function Scriptrepeatfornode(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  iEndrow, iendcol: integer;
  iTemp, t: integer;
  sTemp: string;
  sLeft, sRight: string;
begin
  vt := sptString;
  iEndRow := FindEndRepeat(sev.rqInfo.response.content, sev.iStartrow, params[0]);

  splitstring(params[1],'$', sLeft, sRight);

  sev.rqInfo.response.varpool[params[0]] := params[1]+'[0]';
  iTemp := sev.rqInfo.response.XMLDocs[sLeft].ReadIntegerValue(0, sRight+'*');
  sTemp := '';
  for t:= 1 to iTEmp do begin
    sTemp := sTemp + '[[[:set('''+vartostr(params[0])+''','''+vartostr(params[1])+'['+inttostr(t)+']'')]]]'+#13#10;
  end;

  ReplicateStringListArea(sev.rqInfo.response.content, sev.iStartRow+1, iEndRow, iTemp ,sTemp);
  //bHandled := true;

end;
//------------------------------------------------------------------------------

function Scriptmod(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  itemp1, iTemp2: integer;
begin
  vt := sptNumber;
  try
    iTemp1 := strtoint(params[0]);
    iTemp2 := strtoint(params[1]);
    if iTemp2 = 0 then
      result := 0
    else
      result := iTemp1 mod iTemp2;
  except
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, 'Problem handling the :MOD function. '+Exception(ExceptObject).Message);
  end;
end;
//------------------------------------------------------------------------------

function Scriptadd(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;

  try
    result := floattostr(strtofloat(params[0])+strtofloat(params[1]));
  except
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, 'Problem handling the :add function. '+Exception(ExceptObject).Message);
  end;
end;
//------------------------------------------------------------------------------

function Scriptmultiply(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  try
    result := floattostr(strtofloat(params[0])*strtofloat(params[1]));
  except
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, 'Problem handling the :multiply function. '+Exception(ExceptObject).Message);
  end;
end;

//------------------------------------------------------------------------------

function Scriptdivide(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptnumber;
  try
    result := floattostr(params[0]/params[1]);
  except
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, 'Problem handling the :divide function. '+Exception(ExceptObject).Message);
  end;
end;
//------------------------------------------------------------------------------

function Scriptsumpattern(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  try
    result := floattostr(sev.rqInfo.request.parampatternsum('users'));
  except
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, 'Problem handling the :sumpattern function. '+Exception(ExceptObject).Message);
  end;
end;
//------------------------------------------------------------------------------

function Scriptblanktozeropattern(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  t: integer;
  sTemp: string;
begin
  vt := sptString;
  result := '';
  for t:=0 to sev.rQInfo.request.parampatterncount[params[0]]-1 do begin
    sTemp := sev.rqInfo.request.paramsbypatternmatch[params[0],t];
    result := result + sev.rqInfo.request.paramnamesbypatternmatch[params[0],t]+'='+sTemp+'<br>';
    if sTemp = '' then begin
      params.add(sev.rqInfo.request.paramnamesbypatternmatch[params[0],t]);
    end;
  end;

  for t:=1 to params.count-1 do begin
    sev.rqInfo.Request[params[t]] := '0';
  end;



  sev.rqINfo.SetupDefaultVarPool;
end;
//------------------------------------------------------------------------------

function Scriptsub(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  iTemp1, iTemp2: real;
begin
  vt := sptString;
  try
    iTemp1 := strtofloat(params[0]);
    iTemp2 := strtofloat(params[1]);
    result := floattostr(iTemp1 - iTemp2);
  except
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, 'Problem handling the :sub function. '+Exception(ExceptObject).Message);
  end;
end;
//------------------------------------------------------------------------------

function Scriptdec(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  iTemp1: integer;
begin
  vt := sptString;
  //increment the varpool value
  try
    iTemp1 := strtoint(sev.rqInfo.response.VarPool[params[0]]);
    sev.rqInfo.response.VarPool[params[0]] := inttostr(iTemp1-1);
    result := '';
  except
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, 'Problem handling the :dec function. '+Exception(ExceptObject).Message);
  end;
end;

//------------------------------------------------------------------------------

function Scriptprerequisitestyletext(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  iTemp1: integer;
begin
  vt := sptString;
  iTemp1 := strtoint(params[0]);
  result := '';
  case iTemp1 of
    -1: result := 'mastered';
    -2: result := 'complete';
    -3: result := 'executed';
    -4: result := 'mastered or complete';
  end;
end;
//------------------------------------------------------------------------------

function Scriptifassociatehasobject(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  iTemp: integer;
  iendrow, iEndcol: integer;
  stemp: string;
begin
  vt := sptString;
  //param[0] is a list object
  //param[1] is the associate of the object in the list at index param[1]
  //param[2] is a reference to the object you're looking for as the associate

  if sev.rqInfo.response.objectpool[params[0]].AssociateHasObject(sev.rqInfo.response.objectpool[params[2]], params[1]) then begin
                        //^lookup list object  --  has --        ^master list  ^detail  ^master index
    if params.count > 4 then begin
      iTemp := sev.rqInfo.response.objectpool[params[0]].IndexOfAssociateObject(sev.rqInfo.response.objectpool[params[2]], params[1]);
      sev.rqInfo.response.objectpool[params[4]] := sev.rqInfo.response.objectpool[params[0]].obj[iTemp];
    end;
    result := ''
  end
  //if the object is found, then hack out code until :end is found
  else begin
    result := '';
    //Find the end of the conditional area [[[:end(nestname)]]]

    //Get the nest name for finding the :end tag
    if params.count<4 then
      sTemp := ''
    else
      sTemp := ''''+vartostr(params[3])+'''';
    //Find the endiftag
    FindEnd(sev.rqInfo.response.content, '[[[:end('+sTemp+')]]]', sev.iStartRow, iEndCol, iEndRow);
    //Erase the conditional area
    DeleteStringListArea(sev.rqInfo.response.content, sev.iStartCol, sev.iStartRow, iEndCol, iEndRow);
    sev.bHandled := true;
  end;
end;
//------------------------------------------------------------------

function Scripthasobject(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  // :hasobject('masterlist', 'lookuplist', masteridx)
  //returns true/false
  if sev.rqInfo.response.ObjectPool[params[1]].HasObject(sev.rqInfo.response.ObjectPool[params[0]].obj[strtoint(params[2])]) then
                        //^lookup list object  --  has --        ^master list  ^detail  ^master index
    result := 'true'
  else
    result := 'false';
end;
//------------------------------------------------------------------------------

function Scriptgetsubobjectcount(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
   result := IntToStr(sev.rqInfo.response.ObjectPool[params[0]].objectcount);
end;
//------------------------------------------------------------------------------

function Scriptgetsubobject(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  sev.rqInfo.response.objectpool[params[0]] := sev.rqInfo.response.objectpool[params[1]].obj[strtoint(params[2])];
  result := '';
end;
//------------------------------------------------------------------------------

function Scriptconcat(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := params[0]+vartostr(params[1]);
end;
//------------------------------------------------------------------------------

function Scriptispc(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;

  if pos('win', lowercase(sev.rqInfo.response.request['User-Agent']))>0 then
    result := 'true'
  else
    result := 'false';
end;
//------------------------------------------------------------------------------

function Scriptismac(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if pos('mac', lowercase(sev.rqInfo.response.request['User-Agent']))>0 then
    result := 'true'
  else
    result := 'false';
end;
//------------------------------------------------------------------------------

function Scriptisosx(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := BooltoStr(isosx(sev.rqInfo));
end;
//------------------------------------------------------------------------------

function Scriptisnetscape(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if pos('netscape', lowercase(sev.rqInfo.response.request['User-Agent']))>0 then
    result := 'true'
  else
    result := 'false';
end;
//------------------------------------------------------------------------------

function Scriptisnetscape6(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if IsNetscape6(sev.rqInfo) then
    result := 'true'
  else
    result := 'false';
end;
//------------------------------------------------------------------------------

function Scriptislayer(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if IsLayer(sev.rqInfo) then
    result := 'true'
  else
    result := 'false';
end;
//------------------------------------------------------------------------------

function Scriptisdiv(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if IsDiv(sev.rqInfo) then
    result := 'true'
  else
    result := 'false';
end;
//------------------------------------------------------------------------------

function Scriptisspan(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if IsSpan(sev.rqInfo) then
    result := 'true'
  else
    result := 'false';
end;
//------------------------------------------------------------------------------

function Scriptserverversion(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := GetserverVersion;
end;
//------------------------------------------------------------------------------

function Scriptumbrellaversion(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := GetUmbrellaVersion;
end;
//------------------------------------------------------------------------------

function Scriptfarmrouter(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := adjustwebpath(WebConfig.WebServerConfig.FarmRouter);
end;
//------------------------------------------------------------------------------
function Scriptghost(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  //varpool object name is param[0]
  //object type is param[1]
  //params 2..n are alternating type, value
  ScriptNew(sev.rqInfo, true, Params);
  result := '';
end;

//------------------------------------------------------------------------------
function ScriptFetch(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  //varpool object name is param[0]
  //object type is param[1]
  //params 2..n are alternating type, value
  WebScriptMTDTInterface.ScriptFetch(sev.rqInfo, false, Params);
  result := '';
end;

//------------------------------------------------------------------------------
function ScriptLazyFetch(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  //varpool object name is param[0]
  //object type is param[1]
  //params 2..n are alternating type, value
  WebScriptMTDTInterface.ScriptFetch(sev.rqInfo, true, Params);
  result := '';
end;

//------------------------------------------------------------------------------
function ScriptGhostFetch(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  //varpool object name is param[0]
  //object type is param[1]
  //params 2..n are alternating type, value
  WebScriptMTDTInterface.ScriptGhostFetch(sev.rqInfo, Params);
  result := '';
end;


//------------------------------------------------------------------------------
function Scriptghostquery(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin

//TODO -cunimplemented: unimplemented block
end;
//------------------------------------------------------------------------------

function Scriptmysql_today(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  stemp: string;
begin
  vt := sptString;
  //varpool object name is param[0]
  //object type is param[1]
  //params 2..n are alternating type, value
  stemp := DateToMYSQLDate(now);
  result := sTemp;
end;
//------------------------------------------------------------------------------

function ScrNew(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  //varpool object name is param[0]
  //object type is param[1]
  //params 2..n are alternating type, value
  ScriptNew(sev.rqInfo, false, Params);
  result := '';
end;
//------------------------------------------------------------------------------

function ScriptRound(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  r1, r2 : real;
  i1, i2: integer;
begin
  try
    vt := sptNumber;
    if params.count < 2 then begin
      r1 := params[0];
      result := round(r1);
    end else begin
      r1 := params[0];
      r2 := params[1];
      i1 := round(r1);
      i2 := round(r2);


      result := ((i1 div i2) * i2);
    end;
  except
    vt := sptString;
    result := params[0];
  end;


end;

function Scriptpower(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
    //varpool object name is param[0]
    //object type is param[1]
    //params 2..n are alternating type, value
  result := inttostr(round(Power(strtoint(params[0]),strtoint(params[1]))));
end;
//------------------------------------------------------------------------------

function Scriptbsr(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  //varpool object name is param[0]
  //object type is param[1]
  //params 2..n are alternating type, value
  result := inttostr(HighOrderBit(strtoint(params[0])));
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function Scriptinsertwebresource(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  s: string;
  sresult: string;
begin
  vt := sptString;
  //web resource file name is param[0]
  s := params[0];
  LoadWebResource(sev.rqInfo, sresult, s);
  result := sresult;
end;
//------------------------------------------------------------------------------

function Scriptgetassociate(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  //param[0] output object
  //param[1] source object in object pool
  //param[2] associate name to getg from param[1] (object in object pool)
  sev.rqInfo.response.objectpool[params[0]] := sev.rqInfo.response.objectpool[params[1]].assoc[params[2]];
  result := '';
end;
//------------------------------------------------------------------------------

function Scriptquicksession(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  //varpool object name is param[0]
  if params.count > 0 then
    sev.rqInfo.response.objectpool[params[0]] := QuickSession(sev.rqInfo)
  else
    sev.rqInfo.response.objectpool['session'] := QuickSession(sev.rqInfo);
//    ScriptFetch(rqInfo, true, Params);
  result := '';
end;

//------------------------------------------------------------------------------

function Scripthashtoint(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := inttostr(HashtoInt(params[0]));
end;
//------------------------------------------------------------------------------
function ScriptStaticSession(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := inttohash([0,0],0,HashtoInt(params[0]), false);

end;

//------------------------------------------------------------------------------

function Scriptredirect(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if params.count<1 then
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, 'URL not specified');
  sev.rqInfo.response.Location := params[0];
end;
//------------------------------------------------------------------------------

function Scriptrecurse(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if params.count<1 then
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, 'Recursion Variable not specified');

  result := RecursiveDispatch(sev.rqInfo, params[0], true, params[1]);
end;
//------------------------------------------------------------------------------

function Scriptincludelib(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  btemp: boolean;
  stemp: string;
begin
  vt := sptString;
  //param 0 is file name
  //param 1 TRUE=ignore errors
  if params.count<1 then
    raise ENewException.create(ERC_SCRIPT, ERR_INTERNAL, 'Recursion Variable not specified');


//    LoadWebResource(rqInfo, result, params[0]);

  //load if file exists.. try loading anyway if param[1] is ommitted or is false
  if FileExists(webserverconfig.externalresourcepath+vartostr(params[0])) then begin
    LoadWebResourceSnippet(sev.rqInfo, sTemp, params[0], params[1]);
    sev.rQInfo.response.default('current_scope','root');
    sTemp := '<!--[[[:set(''current_scope'', '''+sev.rqInfo.response.varpool['current_scope']+'/'+vartostr(params[0])+'::'+vartostr(params[1])+''')]]]-->'#13#10+sTemp+#13#10'<!--[[[:set(''current_scope'', '''+sev.rqInfo.response.varpool['current_scope']+''')]]]-->';

    result := sTemp;
  end;
end;
//------------------------------------------------------------------------------

function Scriptdc(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  iTemp: integer;
begin
  vt := sptString;
  iTemp := pos('-', params[0]);
  if iTemp >0 then
    result := copy(params[0], 1, iTemp-1)
  else
    result := '00';
end;

//------------------------------------------------------------------------------

function Scriptdispatch(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := RecursiveDispatch(sev.rqInfo, params[0], false, '');
end;
//------------------------------------------------------------------------------

function Scriptnegate(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  //returns negative version of value in params[0]
  result := inttostr(0-strtoint(params[0]));
end;
//------------------------------------------------------------------------------

function Scriptsetfield(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
    //returns blank
    //param[0] = object name (string)
    //param[1] = field name (string)
    //param[2] = value (constant)
  result := '';
  sev.rqInfo.response.ObjectPool[params[0]][params[1]].AsString := params[2];
end;
//------------------------------------------------------------------------------

function Scriptbeep(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  //beep at freqency, duration
  {$IFDEF DEBUG}
    beeper.beep(strtoint(params[0]), strtoint(params[1]));
  {$ENDIF}
  result := '';
end;
//------------------------------------------------------------------------------

function Scriptcrtobr(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := CRtoBR(params[0]);
end;
//------------------------------------------------------------------------------

function Scriptxmlstringtohtml(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := XMLSTringToPLainText(CRtoBR(params[0]));
end;
//------------------------------------------------------------------------------

function Scriptx(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  //beep at freqency, duration
  result := SimpleEncrypt(params[0]);
end;
//------------------------------------------------------------------------------

function Scriptstathash(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := InttoStaticHash(strtoint(params[0]));
end;
//------------------------------------------------------------------------------

function Scriptleftsplit(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
iTemp: integer;
begin
  vt := sptString;
  iTemp := pos(params[1], params[0]);
  result := copy(params[0], 1, iTemp-1);
end;
//------------------------------------------------------------------------------

function Scriptrightsplit(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  iTemp: integer;
begin
  vt := sptString;
  iTemp := pos(params[1], params[0]);
  result := copy(params[0], iTemp+length(params[1]), length(params[0]));
end;
//------------------------------------------------------------------------------

function Scriptconn(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := WebServerConfig.Conn[params[0]].URL;
end;
//------------------------------------------------------------------------------

function Scriptgetfiledate(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := FileDate(slash(WebServerConfig.TemplateRoot)+vartostr(params[0]));
end;
//------------------------------------------------------------------------------

function Scriptgetfilesize(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  try
    result := QuickFileSize(slash(WebServerConfig.TemplateRoot)+vartostr(params[0]));
  except
    result := 'unknown';
  end;
end;
//------------------------------------------------------------------------------

function Scriptgetyear(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  iM,iY, iD: word;
begin
  vt := sptNumber;

  DecodeDate(params[0], iY, iM, iD);
  result := inttostr(iY);
end;
//------------------------------------------------------------------------------

function Scriptgetmonth(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  im,id,iy: word;
begin
  vt := sptNumber;
  DecodeDate(params[0], iY, iM, iD);
  result := inttostr(iM);
end;
//------------------------------------------------------------------------------

function Scriptgetday(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  im,id,iy: word;
begin
  vt := sptNumber;
  DecodeDate(params[0], iY, iM, iD);
  result := inttostr(iD);
end;
//------------------------------------------------------------------------------

function Scriptdate(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := FormatDateTime('mm/dd/yyyy', date);
end;
//------------------------------------------------------------------------------

function Scripttimeformat(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := FormatDateTime(params[0], StrToDateTime(params[1]));
end;
//------------------------------------------------------------------------------

function Scriptuppercase(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := uppercase(params[0]);
end;
//------------------------------------------------------------------------------

function Scriptfixcaps(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  t: integer;
  s: string;
begin
  vt := sptString;
  s := params[0];
  s := lowercase(s);
  if length(s) > 0 then
    s[1] := char(uppercase(s[1])[1]);

  for t:= 1 to length(s) do begin
    if ((t< length(s)) and (s[t] in [' ','.'])) then begin
      s[t+1] := char(uppercase(s[t+1])[1]);
    end;
  end;
  result := s;
end;
//------------------------------------------------------------------------------

function Scriptisnulldate(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if strtodatetime(params[0]) = TDateTime(NULL_DATETIME) then
    result := 'true'
  else
    result := 'false';
end;
//------------------------------------------------------------------------------

function Scriptgetbit(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
var
  iTemp, iTemp1: integer;
begin
  vt := sptString;
  iTemp := strtoint(params[0]);
  iTemp1 := strtoint(params[1]);
  result := inttostr((iTemp shr iTemp1) and 1);
end;
//------------------------------------------------------------------------------

function Scriptisindaterange(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if (strtodatetime(params[0])>=strtodatetime(params[1]))
  and (strtodatetime(params[0])<=strtodatetime(params[2])) then
    result := 'true'
  else
    result := 'false';

end;
//------------------------------------------------------------------------------

function Scriptnotnulldate(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  if not ((params[0] = '12:00:00 AM') or (params[0] = '1/1/1900')) then
    result := params[0]
  else
    result := '';
end;
//------------------------------------------------------------------------------

function Scriptrehash(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := LoadStringFromFile(params[0]);
end;
//------------------------------------------------------------------------------


function Scriptloadfromfile(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := LoadStringFromFile(params[0]);
end;

function ScriptVar(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := vartostr(sev.rqINfo.response.varpool[params[0]]);
end;


function ScriptMinutesToTime(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptDateTime;
  result := TDateTime(params[0]/(24*60));

end;

function ScriptRandom(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptNumber;
  result := random(params[0]);


end;

function ScriptJstr(sTagBody, sCommand: string; var sev: TscriptEngineVars; params: TscriptParamList; out vt: TScriptParameterType): variant;
begin
  vt := sptString;
  result := JStr(params[0]);
end;







initialization


scriptfunctions.sf.RegisterFunction('test', TestScriptFunction);
scriptfunctions.sf.RegisterFunction(':for', ScriptFor);
scriptfunctions.sf.RegisterFunction(':inc', ScriptInc);
scriptfunctions.sf.RegisterFunction(':end', ScriptEnd);
scriptfunctions.sf.RegisterFunction(':include', ScriptInclude);
scriptfunctions.sf.RegisterFunction(':includesnip', ScriptIncludeSnip);
scriptfunctions.sf.RegisterFunction(':url', ScriptUrl);
scriptfunctions.sf.RegisterFunction(':amp', ScriptAmp);
scriptfunctions.sf.RegisterFunction(':websafe', ScriptWebSafe);
scriptfunctions.sf.RegisterFunction(':getobjectcount', ScriptGetObjectCount);
scriptfunctions.sf.RegisterFunction(':inttorank', ScriptInttoRank);
scriptfunctions.sf.RegisterFunction(':rightscheckboxlist', ScriptRightsCheckboxList);
scriptfunctions.sf.RegisterFunction(':readonlyrightscheckboxlist', ScriptReadonlyRightsCheckboxList);
scriptfunctions.sf.RegisterFunction(':websafetext', ScriptWebSafeText);
scriptfunctions.sf.RegisterFunction(':now', scriptnow);
scriptfunctions.sf.RegisterFunction(':today', scripttoday);

//DONE 1: Check that all script function returns have VT assigned


scriptfunctions.sf.registerfunction(':loadfromfile', Scriptloadfromfile);
scriptfunctions.sf.registerfunction(':rehash', Scriptrehash);
scriptfunctions.sf.registerfunction(':notnulldate', Scriptnotnulldate);
scriptfunctions.sf.registerfunction(':isindaterange', Scriptisindaterange);
scriptfunctions.sf.registerfunction(':getbit', Scriptgetbit);
scriptfunctions.sf.registerfunction(':isnulldate', Scriptisnulldate);
scriptfunctions.sf.registerfunction(':fixcaps', Scriptfixcaps);
scriptfunctions.sf.registerfunction(':uppercase', Scriptuppercase);
scriptfunctions.sf.registerfunction(':date', ScriptDate);
scriptfunctions.sf.registerfunction(':getday', Scriptgetday);
scriptfunctions.sf.registerfunction(':getmonth', Scriptgetmonth);
scriptfunctions.sf.registerfunction(':getyear', Scriptgetyear);
scriptfunctions.sf.registerfunction(':getfilesize', Scriptgetfilesize);
scriptfunctions.sf.registerfunction(':getfiledate', Scriptgetfiledate);
scriptfunctions.sf.registerfunction(':conn', Scriptconn);
scriptfunctions.sf.registerfunction(':rightsplit', Scriptrightsplit);
scriptfunctions.sf.registerfunction(':leftsplit', Scriptleftsplit);
scriptfunctions.sf.registerfunction(':stathash', Scriptstathash);
scriptfunctions.sf.registerfunction(':x', Scriptx);
scriptfunctions.sf.registerfunction(':xmlstringtohtml', Scriptxmlstringtohtml);
scriptfunctions.sf.registerfunction(':crtobr', Scriptcrtobr);
scriptfunctions.sf.registerfunction(':beep', Scriptbeep);
scriptfunctions.sf.registerfunction(':setfield', Scriptsetfield);
scriptfunctions.sf.registerfunction(':negate', Scriptnegate);
scriptfunctions.sf.registerfunction(':dispatch', Scriptdispatch);
scriptfunctions.sf.registerfunction(':dc', Scriptdc);
scriptfunctions.sf.registerfunction(':includelib', Scriptincludelib);
scriptfunctions.sf.registerfunction(':recurse', Scriptrecurse);
scriptfunctions.sf.registerfunction(':redirect', Scriptredirect);
scriptfunctions.sf.registerfunction(':hashtoint', Scripthashtoint);
scriptfunctions.sf.registerfunction(':quicksession', Scriptquicksession);
scriptfunctions.sf.registerfunction(':getassociate', Scriptgetassociate);
scriptfunctions.sf.registerfunction(':insertwebresource', Scriptinsertwebresource);
scriptfunctions.sf.registerfunction(':round', ScriptRound);
scriptfunctions.sf.registerfunction(':bsr', Scriptbsr);
scriptfunctions.sf.registerfunction(':power', Scriptpower);
scriptfunctions.sf.registerfunction(':new', ScrNew);
scriptfunctions.sf.registerfunction(':mysql_today', Scriptmysql_today);
scriptfunctions.sf.registerfunction(':ghost', Scriptghost);
scriptfunctions.sf.registerfunction(':farmrouter', Scriptfarmrouter);
scriptfunctions.sf.registerfunction(':umbrellaversion', Scriptumbrellaversion);
scriptfunctions.sf.registerfunction(':serverversion', Scriptserverversion);
scriptfunctions.sf.registerfunction(':isspan', Scriptisspan);
scriptfunctions.sf.registerfunction(':isdiv', Scriptisdiv);
scriptfunctions.sf.registerfunction(':islayer', Scriptislayer);
scriptfunctions.sf.registerfunction(':isnetscape6', Scriptisnetscape6);
scriptfunctions.sf.registerfunction(':isnetscape', Scriptisnetscape);
scriptfunctions.sf.registerfunction(':isosx', Scriptisosx);
scriptfunctions.sf.registerfunction(':ismac', Scriptismac);
scriptfunctions.sf.registerfunction(':ispc', Scriptispc);
scriptfunctions.sf.registerfunction(':concat', Scriptconcat);
scriptfunctions.sf.registerfunction(':getsubobject', Scriptgetsubobject);
scriptfunctions.sf.registerfunction(':getsubobjectcount', Scriptgetsubobjectcount);
scriptfunctions.sf.registerfunction(':hasobject', Scripthasobject);
scriptfunctions.sf.registerfunction(':ifassociatehasobject', Scriptifassociatehasobject);
scriptfunctions.sf.registerfunction(':prerequisitestyletext', Scriptprerequisitestyletext);
scriptfunctions.sf.registerfunction(':dec', Scriptdec);
scriptfunctions.sf.registerfunction(':sub', Scriptsub);
scriptfunctions.sf.registerfunction(':blanktozeropattern', Scriptblanktozeropattern);
scriptfunctions.sf.registerfunction(':sumpattern', Scriptsumpattern);
scriptfunctions.sf.registerfunction(':divide', Scriptdivide);
scriptfunctions.sf.registerfunction(':multiply', Scriptmultiply);
scriptfunctions.sf.registerfunction(':add', Scriptadd);
scriptfunctions.sf.registerfunction(':mod', Scriptmod);
scriptfunctions.sf.registerfunction(':repeatfornode', Scriptrepeatfornode);
scriptfunctions.sf.registerfunction(':xml', Scriptxml);
scriptfunctions.sf.registerfunction(':foreachfooter', Scriptforeachfooter);
scriptfunctions.sf.registerfunction(':foreachobject', Scriptforeachobject);
scriptfunctions.sf.registerfunction(':foreachobjectjs', ScriptforeachobjectJS);
scriptfunctions.sf.registerfunction(':repeatforobject', Scriptrepeatforobject);
scriptfunctions.sf.registerfunction(':or', Scriptor);
scriptfunctions.sf.registerfunction(':Url', ScriptUrl);
scriptfunctions.sf.registerfunction(':getobjectcount', Scriptgetobjectcount);
scriptfunctions.sf.registerfunction(':inttorank', Scriptinttorank);
scriptfunctions.sf.registerfunction(':websafetext', Scriptwebsafetext);
scriptfunctions.sf.registerfunction(':now', Scriptnow);
scriptfunctions.sf.registerfunction(':today', Scripttoday);
scriptfunctions.sf.registerfunction(':formatdatetime', Scriptformatdatetime);
scriptfunctions.sf.registerfunction(':savevar', Scriptsavevar);
scriptfunctions.sf.registerfunction(':dateonly', Scriptdateonly);
scriptfunctions.sf.registerfunction(':loadvar', Scriptloadvar);
scriptfunctions.sf.registerfunction(':saveuservar', Scriptsaveuservar);
scriptfunctions.sf.registerfunction(':loaduservar', Scriptloaduservar);
scriptfunctions.sf.registerfunction(':hasanyrights', Scripthasanyrights);
scriptfunctions.sf.registerfunction(':hasallrights', Scripthasallrights);
scriptfunctions.sf.registerfunction(':hasright', Scripthasright);
scriptfunctions.sf.registerfunction(':rolehasright', Scriptrolehasright);
scriptfunctions.sf.registerfunction(':countbyvalue', Scriptcountbyvalue);
scriptfunctions.sf.registerfunction(':objectcount', Scriptobjectcount);
scriptfunctions.sf.registerfunction(':slice', Scriptslice);
scriptfunctions.sf.registerfunction(':decodeInlineURL', ScriptdecodeInlineURL);
scriptfunctions.sf.registerfunction(':abs', Scriptabs);
scriptfunctions.sf.registerfunction(':day', Scriptday);
scriptfunctions.sf.registerfunction(':month', Scriptmonth);
scriptfunctions.sf.registerfunction(':year', Scriptyear);
scriptfunctions.sf.registerfunction(':sort', Scriptsort);
scriptfunctions.sf.registerfunction(':document', Scriptdocument);
scriptfunctions.sf.registerfunction(':originaldocument', Scriptoriginaldocument);
scriptfunctions.sf.registerfunction(':documentnoslash', Scriptdocumentnoslash);
scriptfunctions.sf.registerfunction(':leq', Scriptleq);
scriptfunctions.sf.registerfunction(':gt', Scriptgt);
scriptfunctions.sf.registerfunction(':geq', Scriptgeq);
scriptfunctions.sf.registerfunction(':eq', Scripteq);
scriptfunctions.sf.registerfunction(':noteq', Scriptnoteq);
scriptfunctions.sf.registerfunction(':select', Scriptselect);
scriptfunctions.sf.registerfunction(':check', Scriptcheck);
scriptfunctions.sf.registerfunction(':percentprecision', Scriptpercentprecision);
scriptfunctions.sf.registerfunction(':eval', Scripteval);
scriptfunctions.sf.registerfunction(':Dollar', ScriptDollar);
scriptfunctions.sf.registerfunction(':floatprecision', Scriptfloatprecision);
scriptfunctions.sf.registerfunction(':chunk', Scriptchunk);
scriptfunctions.sf.registerfunction(':monthstr', Scriptmonthstr);
scriptfunctions.sf.registerfunction(':if', Scriptif);
scriptfunctions.sf.registerfunction(':ifn', Scriptifn);
scriptfunctions.sf.registerfunction(':bif', Scriptbif);
scriptfunctions.sf.registerfunction(':default', Scriptdefault);
scriptfunctions.sf.registerfunction(':setobjectfromsubobject', Scriptsetobjectfromsubobject);
scriptfunctions.sf.registerfunction(':getbookmark', Scriptgetbookmark);
scriptfunctions.sf.registerfunction(':bookmarkthis', Scriptbookmarkthis);
scriptfunctions.sf.registerfunction(':gethelpfile', Scriptgethelpfile);
scriptfunctions.sf.registerfunction(':set', Scriptset);
scriptfunctions.sf.registerfunction(':rightscheckbox', Scriptrightscheckbox);
scriptfunctions.sf.registerfunction(':eostyle', Scripteostyle);
scriptfunctions.sf.registerfunction(':stringReplace', Scriptstringreplace);
scriptfunctions.sf.registerfunction(':rebuildinlineparameters', Scriptrebuildinlineparameters);
scriptfunctions.sf.registerfunction(':java', Scriptjava);
scriptfunctions.sf.registerfunction(':sleep', Scriptsleep);
scriptfunctions.sf.registerfunction(':band', Scriptband);
scriptfunctions.sf.registerfunction(':ifnequal', Scriptifnequal);
scriptfunctions.sf.registerfunction(':ifngreater', Scriptifngreater);
scriptfunctions.sf.registerfunction(':ifgreater', Scriptifgreater);
scriptfunctions.sf.registerfunction(':greaterthan', Scriptgreaterthan);
scriptfunctions.sf.registerfunction(':lessthan', Scriptlessthan);
scriptfunctions.sf.registerfunction(':lessorequal', Scriptlessorequal);
scriptfunctions.sf.registerfunction(':greaterorequal', Scriptgreaterorequal);
scriptfunctions.sf.registerfunction(':needany', Scriptneedany);
scriptfunctions.sf.registerfunction(':needall', Scriptneedall);
scriptfunctions.sf.registerfunction(':ban', Scriptban);
scriptfunctions.sf.registerfunction(':need', Scriptneed);
scriptfunctions.sf.registerfunction(':wrapif', Scriptwrapif);
scriptfunctions.sf.registerfunction(':linkif', Scriptlinkif);
scriptfunctions.sf.registerfunction(':BuildDate', ScriptBuildDate);
scriptfunctions.sf.registerfunction(':realsession', Scriptrealsession);
scriptfunctions.sf.registerfunction(':readonlyrightscheckboxlist', Scriptreadonlyrightscheckboxlist);
scriptfunctions.sf.registerfunction(':rightscheckboxlist', Scriptrightscheckboxlist);
scriptfunctions.sf.registerfunction(':WebSafe', ScriptWebSafe);
scriptfunctions.sf.registerfunction(':include', Scriptinclude);
scriptfunctions.sf.registerfunction(':includeJS', ScriptincludeJS);
scriptfunctions.sf.registerfunction(':includeSnip', ScriptincludeSnip);
scriptfunctions.sf.registerfunction(':for', Scriptfor);
scriptfunctions.sf.registerfunction(':End', ScriptEnd);
scriptfunctions.sf.registerfunction(':Inc', ScriptInc);
scriptfunctions.sf.registerfunction(':date_add', ScriptDateAdd);
scriptfunctions.sf.registerfunction(':date_sub', ScriptDateSub);
scriptfunctions.sf.registerfunction(':var', ScriptVar);
scriptfunctions.sf.registerfunction(':random', ScriptRandom);
scriptfunctions.sf.registerfunction(':fetch', ScriptFetch);
scriptfunctions.sf.registerfunction(':lazyfetch', ScriptLazyFetch);
scriptfunctions.sf.registerfunction(':ghostfetch', ScriptGhostFetch);
scriptfunctions.sf.registerfunction(':decleaninjection', ScriptDecleanInjection);
scriptfunctions.sf.registerfunction(':uncleaninjection', ScriptDecleanInjection);
scriptfunctions.sf.registerfunction(':codestyle', ScriptCodeStyle);
scriptfunctions.sf.registerfunction(':minutestotime', scriptMinutesToTime);
scriptfunctions.sf.registerfunction(':staticsession', scriptStaticSession);
scriptfunctions.sf.registerfunction(':nonbreaking', scriptNonBreaking);
scriptfunctions.sf.registerfunction(':loadxml',scriptLoadXML);
scriptfunctions.sf.registerfunction(':includebody',scriptincludebody);
scriptfunctions.sf.registerfunction(':jstr',scriptjstr);

end.
