unit WebScript;
{$INLINE AUTO}
interface

uses Variants, variantlist, classes, sysutils, requestinfo, dialogs, MTDTInterface, WebDispatch, windows, Exceptions, ErrorResource,systemx, Dataobject, webfunctions, betterobject, sharedobject, simplewinsock, debug, stringx, typex;


const
  operators = ['!','-','+','=','*','/','>','<','(',')'];
  whitespace = operators + [' ', ';', '"'];

type
  EScriptError = class (Exception);
  EScriptErrorEx = class (ENewException);


  TScriptParameterType = (sptNull, sptNumber, sptDatetime, sptString, sptBoolean, sptFunction, sptVariable, sptIdentifier);
  TScriptResultType = TScriptParameterType;
  TProgressCallbackProcedure = procedure (pos, max: integer; sMessage: string) of object;

  TParamList = TVAriantList;

  TScriptResult = record
    result: variant;
    resulttype: TScriptParameterType;
  end;



  TScriptEngineVars = record
    rqINfo: TRequestInfo;
    iStartCol: integer;
    iStartRow: integer;
    bHandled: boolean;
    FullTagBody: string;
  end;


  TScriptParamList = TVariantList;

  TScriptFunction = function(sTagBody, sCommand: string; var sev: TScriptEngineVArs; params: TScriptParamList;  out vt: TScriptParameterType): variant;


  TScriptFunctions = class(TFAkeLockQueuedObject)
  protected
    FFuncs: TStringList;
  public
    constructor create;override;
    destructor destroy;override;
    procedure RegisterFunction(name: string; func: TScriptFunction);
    function GetFunction(sName: string): TScriptFunction;
  end;


function HandleObjectContext(objectcontext: TDataObject; sDelimiter, sTagPart: string; rqInfo: TRequestInfo; objpoolName: string): variant;

procedure MoveThroughParens(var s: string);
procedure ReplaceEscSequences(rqInfo: TRequestInfo; slMaster: TStringList; progress_callback: TProgressCallbackProcedure);
function FindEnd(sl: TStringList; sSearch: string; iStartingAtRow: integer): integer; overload;
procedure FindEnd(sl: TStringList; sSearch: string; iStartingAtRow: integer; out iCol, iRow: integer); overload;
function FindEndRepeat(sl: TStringList; iStartingAtRow: integer; sObjectName: string): integer; overload;

function IsQuote(sTag: string; iPos: integer): boolean;
function IsOperator(sTag: string; iPos: integer; sPriorOperators: string): boolean;
function IsWhiteSpace(sTag: string; iPos: integer): boolean;
function GetIdentifier(var sTagBody: string; out sRemainder: string): boolean;
function GetOperator(sTagBody: string; out sOperator: string; var sRemainder: string): boolean;
function ScriptParamToVariant(v: variant; spt: TScriptParameterType): variant;

procedure DecodeScriptFunction(sTagBody:string; out sCommand: string; params: TParamList);
//procedure HandleOperation(var sev: TScriptEngineVars; sTagBody: string; sOp: string; var vResult: variant; var vtResult: TScriptParameterType);
procedure HandleOperation(var sev: TScriptEngineVars; sTagBody: string; var vResult: variant; var vtResult: TScriptParameterType);

function HandleScript(sTagBody: string; rqInfo: TRequestInfo): variant; overload;
function HandleScript(sTagBody: string; rqInfo: TRequestInfo; var iStartRow, iStartCol: integer; out bHandled: boolean): variant; overload;
function HandleScriptCommand(sTagBody: string; rqInfo: TRequestInfo; var iStartRow, iStartCol: integer; out bHandled: boolean; out ReturnType: TScriptParameterType): variant;
function HandleStringLiteralTag(sTagBody: string; rqInfo: TRequestInfo; iStartRow, iStartCol: integer): variant;
function HandleNumericLiteralTag(sTagBody: string; rqInfo: TRequestInfo; iStartRow, iStartCol: integer): variant;
function HandleVariableTag(sTagBody: string; rqInfo: TRequestInfo; iStartRow, iStartCol: integer; objectcontext: TDataObject = nil): variant;

function HandleObjectField(sTagBody: string; obj: TDataObject; sField: string): variant;
function HandleTokenField(sTagBody: string; obj: TDataObjectToken; sField: string): string;
function DeleteStringHyperLinks(sl: TStringList; iStartCol, iStartRow, iEndCol, iEndRow: integer): string;

function GetYearFromDate(vDate: variant): integer;
function GetMonthFromDate(vDate: variant): integer;
function GetDayFromDate(vDate: variant): integer;
function RecursiveDispatch(rqInfoOLD: TrequestInfo; sdocument: string; bDoRecursion: boolean; sRecursionLevelVariable: string): string;
function IndependentDispatch(rqInfoOLD: TrequestInfo; sdocument: string): string;

procedure ProcessPostScript(rqInfo: TRequestInfo);
procedure ProcessPostPostScript(rqInfo: TRequestInfo);
procedure ProcessPrePostScript(rqInfo: TRequestInfo);
procedure ProcessPostScriptFields(rqInfo: TRequestInfo);
procedure ProcessPostScriptAutoRedirect(rqInfo: TRequestInfo);
function QuickFileSize(sFile: string): string;
function FileDate(sFile: string): string;
function VarTypeToScriptType(v: variant):TScriptParameterType;
function BetterVarToStr(v: variant): string;

const
	CStructure 				= 0;   { Menu/sequence }
	CLesson 					= 1;   { Actual computer-based courseware lessons }
	COffLine 					= 2;   { Non-computer based courseware }
  CTest             = 3;   { Examiner test }
  CAssessment       = 4;   { Multiple Examiner tests, for placement }
  CRecord           = 5;   { Performance records only (no execution) }
  CNoActivity       = 30000;   { Used for missing activities }

  {Type}
	COrganizer 				= 1;
	CCurriculum 			= 2;
	CCourse 					= 3;
	CModule 					= 4;
	CCustom 					= 5;


implementation

uses webstring, webscriptfunctions, stringx.ansi,
  scriptfunctions;


function BetterVarToStr(v: variant): string;
begin
  if varType(v) = varNull then
    result := ''
  else
    result := varToStr(v);
end;
procedure MoveThroughParens(var s: string);
var
  t: integer;
  iCount: integer;
  bQuote: boolean;
begin
  bQuote := false;
  iCount := 0;


  for t:= 1 to length(s) do begin
    case s[t] of
      '''': begin
        bQuote := not bQuote;
      end;
      '(': begin
        if not bQuote then begin
          inc(iCount)
        end;
      end;
      ')': begin
        if not bQuote then begin
          dec(iCount);
          if iCount < 0 then begin
            s := copy(s, t+1, length(s));
            break;
          end;
        end;
      end;
    end;
  end;


end;

function VarTypeToScriptType(v: variant):TScriptParameterType;
begin
  if varType(v) = varSingle then
    result := sptNumber
  else
  if varType(v) = varDouble then
    result := sptNumber
  else
  if varType(v) = varInteger then
    result := sptNumber
  else
  if varType(v) = varSmallInt then
    result := sptNumber
  else
  if varType(v) = varInt64 then
    result := sptNumber
  else
  if varType(v) = varByte then
    result := sptNumber
  else
  if varType(v) = varString then
    result := sptString
  else
    if varType(v) = varOleStr then
    result := sptString
  else
  if varType(v) = varNull then
    result := sptString
  else
  if varType(v) = varBoolean then
    result := sptBoolean
  else
  if varType(v) = varCurrency then
    result := sptNumber
  else
  if varType(v) = varUString then
    result := sptString
  else
  if varType(v) = varDate then
    result := sptDateTime
  else begin
    raise Exception.create('Cannot convert variant type '+inttostr(vartype(v))+' into script parameter type');
  end;




//  TScriptParameterType = (sptNull, sptNumber, sptDatetime, sptString, sptBoolean, sptFunction, sptVariable, sptIdentifier);
end;

//------------------------------------------------------------------------------
function HandleCompoundScript(sTagBody: string; rqInfo: TRequestInfo; var iStartRow, iStartCol: integer; out bHandled: boolean): string;
var
  sLeft, sRight, sJunk, sJunk2: string;
  sDelimiter: string;
begin
  sRight := sTagBody;
  result := '';
  while SplitString(sRight, [';','+','-','**', '/'], sLeft, sRight, sDelimiter, ['''','''','(',')']) do begin
    if not SplitString(sRight, [';','+','-','**', '/'], sLeft, sJunk, sJunk2, ['''','''','(',')']) then
      sLeft := sRight;

    if sDelimiter = ';' then begin
      result := result + HandleScript(sLEft, rqInfo, iStartRow, iStartCol, bHandled);
    end else
    if sDelimiter = '+' then begin
      if result = '' then
        result := '0';
      result := floattostr(strtofloat(result) + strtofloat(HandleScript(sLEft, rqInfo, iStartRow, iStartCol, bHandled)));
    end else
    if sDelimiter = '-' then begin
      if result = '' then
        result := '0';
      result := floattostr(strtofloat(result) - strtofloat(HandleScript(sLEft, rqInfo, iStartRow, iStartCol, bHandled)));
    end else
    if sDelimiter = '**' then begin
      if result = '' then
        result := '0';
      result := floattostr(strtofloat(result) * strtofloat(HandleScript(sLEft, rqInfo, iStartRow, iStartCol, bHandled)));
    end else
    if sDelimiter = '/' then begin
      if result = '' then
        result := '0';
      result := floattostr(strtofloat(result) / strtofloat(HandleScript(sLEft, rqInfo, iStartRow, iStartCol, bHandled)));
    end;



  end;
end;
//------------------------------------------------------------------------------
function HandleScript(sTagBody: string; rqInfo: TRequestInfo; var iStartRow, iStartCol: integer; out bHandled: boolean): variant;
var
  sDel: string;
  bIsCompound: boolean;
  iPos, iPos2:  integer;
  bPos, bPos2: boolean;
  res: TScriptResult;
  vLeft, vRight: variant;
  vtLeft, vtRight: TScriptResultType;
  sRemainder: string;
  sOp: string;
  sIDENT: string;
  sev: TScriptEngineVars;
begin
//  Debug.Log(rqInfo.request.document+' Handle script '+sTagbody);
  bHandled := false;

  //if it starts with :repeat then its a command... so processs command
  sTagBody := Trim(sTagBody);
  if sTagbody = '' then begin
    result := '';
    exit;
  end;

  if lowercase(sTagBody) = 'true' then
    sTagBody := '''true''';

  if lowercase(sTagBody) = 'false' then
    sTagBody := '''false''';

  bIsCompound := sTagBody[1] = ';';

  vRight := 0;
  vtRight := sptNumber;

  sOP := '';

  vtLeft := vtRight;
  vLeft := vRight;
  sev.rqINfo := rqInfo;
  sev.iStartCol := iStartCol;
  sev.iStartRow := iStartRow;
  sev.FullTagBody := sTagBody;
  sev.bHandled := false;
  try
    Handleoperation(sev, sTagBody,vRight, vtRight);
    bHandled := sev.bHandled;
    iStartCol := sev.iStartCol;
    iStartRow := sev.iStartRow;
    result := vRight;
  except
    on E: ESocketsDisabled do raise;
    on E: EScriptErrorEx do begin
      raise EScriptErrorEx.create(e.ErrorCode, 'Script Error', 'Error in:'+sTagBody+ '--'+E.Message);
    end;
    on E: EScriptError do begin
      raise EScriptError.create('Error in:'+sTagBody+ '--'+E.Message);
    end;
    on E: Exception do begin
      Debug.Log('In '+rqInfo.request.document+' Error in:'+sTagBody+ '--'+E.Message);
      raise EScriptError.create('In '+rqInfo.request.document+' Error in:'+sTagBody+ '--'+E.Message);
    end;
  end;
end;
//------------------------------------------------------------------------------
function HandleScriptCommand(sTagBody: string; rqInfo: TRequestInfo; var iStartRow, iStartCol: integer; out bHandled: boolean; out ReturnType: TScriptParameterType): variant;
var
  sCommand: string;
  params: TParamList;
  t: integer;
  sMore: string;
  i64Start, i64End: int64;
begin
  ReturnType := sptString;

  params := TParamList.create;
  try
    //Decode the tag
    result := '';
    QueryPerformanceCounter(i64Start);
    DecodeScriptFunction(sTagBody, sCommand, params);
    if sCommand = '' then
    sCommand := sTagBody;

    //Recursively handle all parameters first
    for t:= 0 to params.count-1 do begin
      params[t] := HandleScript(params[t], rqInfo, iStartRow, iStartCol, bHandled);
    end;

    result := result+DispatchScriptCommand(sTagBody, sCommand, params, rqInfo, iStartRow, iStartCol, bHandled);


    //showmessage(sCommand);
    QueryPerformanceCounter(i64End);
    rqInfo.Response.DebugLog.Add(inttostr(i64End-i64Start)+'~'+sTagBody);


  finally
    params.free;
  end;
end;
//------------------------------------------------------------------------------
function FindEndRepeat(sl: TStringList; iStartingAtRow: integer; sObjectName: string): integer;
var
  sSearch: string;
begin
  sSearch := '[[[:end('''+sObjectName+''')]]]';
  result := FindEnd(sl, sSearch, iStartingAtRow);
end;

//------------------------------------------------------------------------------
function FindEnd(sl: TStringList; sSearch: string; iStartingAtRow: integer): integer; overload;
var
  iCol: integer;
begin
  FindEnd(sl, sSearch, iStartingAtRow, iCol, result);
end;
//------------------------------------------------------------------------------
procedure FindEnd(sl: TStringList; sSearch: string; iStartingAtRow: integer; out iCol, iRow: integer); overload;
var
  iPos: integer;
  t: integer;
  sSearch2: string;
begin
  iPos := 0;

//  sSearch2 := stringReplace(sSearch, '''''', '', [rfReplaceAll]);

  //scan the string list for the search string
  for t:= iStartingAtRow to sl.count-1 do begin
    iPos := pos(sSearch, sl[t]);
//    if iPos < 1 then
//      iPos := pos(sSearch2, sl[t]);
    //iPos := pos(sSearch, sl[t]);
    if iPos>0 then begin
      iRow := t;
      iCol := iPos;
      break;
    end;
  end;

  //if search string was found then
  if iPos <1  then begin
    //raise exception
    raise EScriptErrorEx.create(ERC_SCRIPT, ERR_INTERNAL, 'End tag not found for search -- '+StringReplace(sSearch,'[[[','[{{]', [rfReplaceAll]));
  end;

end;

//------------------------------------------------------------------------------
procedure DecodeScriptFunction(sTagBody:string; out sCommand: string; params: TParamList);
//Put in a tag.... get out the command name and parameters as a string list
type
  TScanState = (ssString, ssParameter, ssCommand);
var
  ss : TScanState;
  iStart, iEnd, t: integer;
  iParamRecursion: integer;
  sTemp: string;
begin
  sCommand := '';
  iStart := -1;
  if params = nil then
    raise EScriptErrorEx.create(ERC_SCRIPT, ERR_INTERNAL, 'Nil params object passed to Decode Script Function');

  if not (sTagBody[1]=':') then
    raise EScriptErrorEx.create(ERC_SCRIPT, ERR_INTERNAL, sTagbody+' is not a valid command');

  ss := ssCommand;
  iParamRecursion:=0;
  for t:= 1 to length(sTagBody) do begin

    case sTagBody[t] of
      //------------------------------------
      //if a '(' is found
      '(':
        //if in commmand state
        CASE ss OF
          ssCommand:begin
            //mark beginning of parameters
            iStart := t+1;
            //Kick into Param state
            ss := ssParameter;
            sCommand := copy(sTagbody, 1, t-1)
          end;
          //ignore if in string state
          ssString: ;
          //if in parameter state
          ssParameter: begin
            //increment the parameter recursion level
            inc(iParamRecursion);
          end;
        END
      ;
      //------------------------------------
      //if a ')' is found
      ')':
        CASE ss OF
          //if in commmand state then BOMB
          ssCommand:
            raise EScriptErrorEx.create(ERC_SCRIPT, ERR_INTERNAL, 'Script syntax error in '+sTagBody);
          //ignore if in string state
          ssString: ;
          //if in parameter state
          ssParameter: begin
            //if recursed then
            if iParamRecursion>0 then begin
              //decrement the parameter recursion level
              dec(iParamRecursion);
            end
            else begin
              //mark the end of the whole shabang
              iEnd:= t;
              //add the parameter to the param list
              sTemp := copy(sTagBody, iStart, iEnd-iStart);
              params.Add(sTemp);
            end;
          end;
        END
      ;
      //------------------------------------
      //if a quote is found
      '''':
        //string begin and case
        CASE ss OF
          //quote not allowed in command section
          ssCommand:
            raise EScriptErrorEx.create(ERC_SCRIPT, ERR_INTERNAL, 'Script syntax error in '+sTagBody);
          ssParameter: begin
            ss := ssString;
          end;
          ssString: begin
            ss := ssParameter;
          end;
        END
      ;
      //------------------------------------
      //if a ',' is found
      ',':
        CASE ss OF
          //if in commmand state then BOMB
          ssCommand:
            raise EScriptErrorEx.create(ERC_SCRIPT, ERR_INTERNAL, 'Script syntax error in '+sTagBody);
          //ignore if in string state
          ssString: ;
          //if in parameter state
          ssParameter: begin
            //*ignore if recursed

            //if NOT recursed then
            IF iParamRecursion=0 THEN BEGIN
              //mark the end of the whole shabang
              iEnd:= t-1;
              //add the parameter to the param list
              params.Add(copy(sTagBody, iStart, iEnd-iStart+1));
              //start a new parameter
              iStart := t+1;
            END;
          end;
        END
      end;
  end;
end;


function GetFullDelimiterFrom(posdeets: TPosDetails): string;
var
  sl,sr: string;
begin
  if zcopy(posDeets.rightof_including,0,1) = '[' then begin
    splitString(posDeets.rightof_including, '[', result, sr);
    splitString(sr, ']', result, sr);
    result := '['+result+']';
  end
  else if zCopy(posDeets.rightof_including,0,2) = ':.' then begin
    splitString(posDeets.rightof_including, ':.', result, sr);
    splitString(result, ':', result, sr);
    result := ':.'+result+':';
  end
  else if zCopy(posDeets.rightof_including,0,1) = ':' then begin
    splitString(posDeets.rightof_including, ':', result, sr);
    splitString(result, ':', result, sr);
    result := ':'+result+':';
  end else begin
    raise EScriptError.create('unknown delimiter in '+posdeets.rightof_including);
  end;

end;
//------------------------------------------------------------------------------
function HandleObjectContext(objectcontext: TDataObject; sDelimiter, sTagPart: string; rqInfo: TRequestInfo; objpoolName: string): variant;
var
  moredelimiters: TPosDetails;
  taguntilnext: string;
  hasmore: boolean;
  sl,sr: string;
  sIndex: string;
  fulldelimiter: string;
  d1,d2: boolean;
begin
//  Debug.Log(sDelimiter+' '+sTagPart);


  sTagPart := trimcharsfromfront(sTagPart, '::');
  moredelimiters := ZFirstPos(sTagPart, ['[',':']);
  hasmore := moredelimiters.search <> '';

  d1 := (zcopy(sDelimiter,0,1) = '[');
  d2 := (zcopy(sDelimiter,0,2)=':.');

  //indexed objects are handled regardless of HASMORE
  if d1 or d2 then begin
    if d1 then begin
      if not SplitString(sDelimiter, '[', sl, sr) then
        raise EScriptError.Create('Expected ''[''');
      if not SplitString(sr, ']', sl, sr) then
        raise EScriptError.Create('Expected '']''');
    end else
    //d2
    begin
      if not SplitString(sDelimiter, ':.', sl, sr) then
        raise EScriptError.Create('Expected '':.''');
      if not SplitString(sr, ':', sl, sr) then
        raise EScriptError.Create('Expected '':''');
    end;
    if sl = '' then
      sl := rqInfo.response.varpool[objpoolname];
    sl := trim(sl);
    if not IsInteger(sl) then
      raise EScriptError.create('expected integer, got "'+sl+'"');

    objectcontext := objectcontext.obj[strtoint(sl)];
    result := HandleObjectContext(objectcontext, '', sTagPart, rqInfo, objpoolname);
  end else
  //if NO more delimiters are found, then we have a field or token in the object context
  if not hasmore then begin
    if objectcontext = nil then
      raise EScriptError.create('Expected object context');


    //token params
    if zcopy(sTagPart, 0,1) = '#' then begin
      result := objectcontext.Token.Params[strtoint64(zCopy(sTagPart, 1, length(sTagPart)))];
    //fields
    end else begin
      result := objectcontext.fld[sTagPart].AsVariant;
    end;
  end else
  //------------------------------------------------
  //ELSE WE NEED A NEW OBJECT CONTEXT FOR RECURSION... it doesn't matter what the delimiter is... its an ASSOC
  //------------------------------------------------
  begin
    fullDelimiter := GetFullDelimiterFrom(moredelimiters);
    objectcontext := objectcontext.assoc[moredelimiters.leftof];
    result := HandleObjectContext(
                objectcontext,
                fullDelimiter,
                zcopy(
                  moredelimiters.rightof_including,
                  length(fullDelimiter),
                  length(moredelimiters.rightof_including)
                ), rqInfo, objpoolname);
  end;


end;

function HandleVariableTag(sTagBody: string; rqInfo: TRequestInfo; iStartRow, iStartCol: integer; objectcontext: TDataObject = nil): variant;
var
  x,xx: integer;
  iPos, iPos2: integer;
  sObject, sField, sTemp: string;
  sLeft, sRight: string;
  iTemp, iTemp2: integer;
  iDLength: integer; //delimiter length
  thispos: TPosDetails;
  moredelimiters: TPosDetails;
  fulldelimiter: string;
  objctx: TDataObject;
begin
//  Debug.Log(sTagBody);

  if lowercase(sTagBody) = 'true' then begin
    result := 'true';
    exit;
  end;

  if lowercase(sTagBody) = 'false' then begin
    result := 'false';
    exit;
  end;

  thispos := ZFirstPos(sTagBody, ['$', '[', ':']);
  iDLength := length(thispos.search);

  if thispos.search = '$' then begin
    SplitString(sTagBody,'$', sLeft, sRight);
    result := DecodeWebSTring(rqInfo.response.xmldocs[sLeft].ReadValue(sRight));

  end else
  //if THIS IS AN OBJECT
  if ((thispos.search = '[') or (thispos.search = ':')) then begin
    fullDelimiter := GetFullDelimiterFrom(thispos);
    objctx := rqInfo.response.ObjectPool[thispos.leftof];
    result := HandleObjectcontext(
            objctx,
            fullDelimiter,
            zcopy(thispos.rightof_including, length(fullDelimiter), length(thispos.rightof_including)),
            rqInfo, thispos.leftof);

  END
  //otherwise Get the value from the variable pool
  ELSE BEGIN

    try
      result := rqInfo.Response.VarPool[sTagBody];
    except
      on E: Exception do begin
        raise EScriptErrorEx.create(ERC_SCRIPT, ERR_INTERNAL, 'Variable '+sTagBody+' needed but undefined -- '+e.message);
      end;
    end;
  END
end;
//------------------------------------------------------------------------------
function HandleTokenField(sTagBody: string; obj: TDataObjectToken; sField: string): string;
begin
  result := BetterVarToStr(obj.params[strtoint(copy(sField, 2, length(sfield)))])
end;
//------------------------------------------------------------------------------
function HandleObjectField(sTagBody: string; obj: TDataObject; sField: string): variant;
var
  iPos, iPos2: integer;
  sAssociate, sObject: string;
begin
  iPos := pos('::', sField);
  iPos2 := pos('+-', sField);
  //if :: is found in field name, then get the associate from the object with the name and
  //recursively call this function (until the field is found)
  if (iPos > 0) and ((iPos < iPos2) or (iPos2<1)) then begin
    sAssociate := copy(sField, 1, iPos-1);
    sField := copy (sField, iPos+2, length(sTagBody));
    obj := obj.assoc[sAssociate];
    result := HandleObjectField(sTagBody, obj, sField);
  end else
  //if :. is found in field name (and before :.) then get sub object from the
  //object with the name and
  //recursively call this function (until the field is found)
  if (iPos2 > 0) and ((iPos2< iPos) or (iPos<1)) then begin
    sObject := copy(sField, 1, iPos2-1);
    sField := copy (sField, iPos2+2, length(sTagBody));
    try
      strtoint(sObject);
    except
      on E: Exception do
        raise Exception.create('Integer expected, '''+sObject+''' found');
    end;
    obj := obj.obj[strtoint(sObject)];
    result := HandleObjectField(sTagBody, obj, sField);
  end else
  //handle datatier ID request
  if sField[1] = '@' then begin
    result := obj.token.DataTier;
  end else
  //handle key field
  if (sField<>'') and (sField[1] = '#') then
    result := BetterVarToStr(obj.token.params[strtoint(copy(sField, 2, length(sfield)))])
  //handle normal fields
  else begin
    if copy(sfield,1,1) = '%' then
      result := obj.fieldbyindex[strtoint(Trim(copy(sfield, 2, 99999)))].AsVariant
    else begin
      result := obj[sField].AsVariant;
    end;
  end;
end;
//------------------------------------------------------------------------------
function HandleNumericLiteralTag(sTagBody: string; rqInfo: TRequestInfo; iStartRow, iStartCol: integer): variant;
begin
  try
    strtofloat(sTagBody);
    result := sTagBody;
  except
    raise EScriptErrorEx.create(ERC_SCRIPT, ERR_INTERNAL, 'Invalid numeric constant: '+sTagBody);
  end;
end;
//------------------------------------------------------------------------------
function DeleteStringHyperLinks(sl: TStringList; iStartCol, iStartRow, iEndCol, iEndRow: integer): string;
type
  Tsearchstate = (ssRoot, ssHyperlink, ssEndHyperlink);
var
  sREplacement: string;
  sZone: string;
  state: Tsearchstate;
  iStart, iEnd: integer;
  t,u: integer;
  sLEft, sRight: string;
begin
  //build the resulting replacement string
  state := ssRoot;
  sReplacement := '';
  for u:= iStartRow to iEndRow do begin

    if iStartRow = u then
      iStart := iStartCol
    else
      iStart := 1;

    if iEndRow = u then
      iEnd := iEndCol-1
    else
      iEnd := length(sl[u]);

    for t:= iStart to iEnd do begin
      case state of
        ssroot: begin
          sZone := copy(sl[u], t, 3);
          sZone := lowercase(sZone);
          if sZone = '</a' then
            state := ssEndHyperlink
          else
          if (sZone[1] = '<') and (sZone[2] = 'a') then
            state := ssHyperlink
          else begin
            sReplacement := sReplacement + sl[u][t];
          end;
        end;
        ssHyperLink: begin
          sZone := sl[u][t];
          if sZone = '>' then
            state := ssRoot;
        end;
        ssEndHyperLink: begin
          sZone := sl[u][t];
          if sZone = '>' then
            state := ssRoot;
        end;
      end;
    end;
  end;

  SplitSTring(sReplacement, '[[[', sLeft, sRight, false);
  SplitSTring(sRight, ']]]', sLeft, sREplacement, false);

//  SplitSTring(sReplacement, '[[[', sLeft, sRight, true);
//  SplitSTring(sLeft, ']]]', sReplacement, sRight, true);

  result := sReplacement;


  //Setup the end result
  sl[iStartRow] := copy(sl[iStartRow], 1, iStartCol-1)+ result+
                   copy(sl[iEndRow], iEndCol, length(sl[iEndRow]));

  if not (iEndRow = iStartRow) then begin
    //Delete the rows starting after the startrow upto the endrow
    ReplicateStringListArea(sl, iStartrow+1, iEndRow, 0);
  end;
end;

//------------------------------------------------------------------------------
function GetMonthFromDate(vDate: variant): integer;
var
  dtTemp: TDateTime;
  wMonth, wYear, wDay: WORD;
begin
  try
    //Convert the string to a read date
    if varType(vDate) = varString then
      vDate := StrtoDate(vDAte);

    dtTEmp := vDate;

    //Decode the date
    DecodeDate(dtTemp, wYear, wMonth, wDay);

    //Return the Month
    result := wMonth;
  except
    result := 0;
  end;

end;
//------------------------------------------------------------------------------
function GetDayFromDate(vDate: variant): integer;
var
  dtTemp: TDateTime;
  wMonth, wYear, wDay: WORD;
begin
  try
    //Convert the string to a read date
    if varType(vDate) = varString then
      vDate := StrtoDate(vDAte);

    dtTEmp := vDate;

    //Decode the date
    DecodeDate(dtTemp, wYear, wMonth, wDay);

    //Return the Day
    result := wDay;
  except
    result := 0;
  end;

end;
//------------------------------------------------------------------------------
function GetYearFromDate(vDate: variant): integer;
var
  dtTemp: TDateTime;
  wMonth, wYear, wDay: WORD;
begin
  try
    //Convert the string to a read date
    if varType(vDate) = varString then
      vDate := StrtoDate(vDAte);

    dtTEmp := vDate;

    //Decode the date
    DecodeDate(dtTemp, wYear, wMonth, wDay);

    //Return the Month
    result := wYear;
  except
    result := 0;
  end;

end;

//------------------------------------------------------------------------------
function IndependentDispatch(rqInfoOLD: TrequestInfo; sdocument: string): string;
begin
  result := Recursivedispatch(rqInfoOLD, sdocument, false, '');
end;

//------------------------------------------------------------------------------
function RecursiveDispatch(rqInfoOLD: TrequestInfo; sdocument: string; bDoRecursion: boolean; sRecursionLevelVariable: string): string;
var
  rqInfo: TRequestInfo;
begin
  //copy create
  rqInfo := nil;
  try
    rqInfo := TrequestInfo.CopyCreate(rqInfoOLD);
    if bDoRecursion then begin
      //increment the variable indicated in sRecursionLevelVariable
      rqInfo.response.varpool[sRecursionLevelVariable] := inttostr(strtoint(rqInfo.response.varpool[sRecursionLevelVariable])+1);
      rqInfo.response.request[sRecursionLevelVariable] := rqInfo.response.varpool[sRecursionLevelVariable];
    end;

    //dispatch
    rqInfo.request.Document := sDocument;
    DispatchwebRequest(rqInfo);

    if bDoRecursion then begin
      //decrement
      //increment the variable indicated in sRecursionLevelVariable
      rqInfo.response.varpool[sRecursionLevelVariable] := inttostr(strtoint(rqInfo.response.varpool[sRecursionLevelVariable])-1);
      rqInfo.response.request[sRecursionLevelVariable] := rqInfo.response.varpool[sRecursionLevelVariable];
    end;


    result := rqInfo.response.content.text;
  finally
    rqinfo.free;
  end;
end;

function HandleScript(sTagBody: string; rqInfo: TRequestInfo): variant; overload;
var
  bDummy: boolean;
  iDummy1, iDummy2: integer;
begin
  iDummy1 := 0;
  iDummy2 := 0;
  result := HandleScript(sTagBody, rqInfo, iDummy1, iDummy2, bDummy);

end;

//------------------------------------------------------------------------------
procedure ProcessPrePostScript(rqInfo: TRequestInfo);
begin
  //First run the command indicated in the Prescript variable
  if rqInfo.request.HasParam('PreScript') then
    HandleScript(rqInfo.request['PreScript'], rqInfo);
  if rqInfo.request.HasParam('PreScript1') then
    HandleScript(rqInfo.request['PreScript1'], rqInfo);
  if rqInfo.request.HasParam('PreScript2') then
    HandleScript(rqInfo.request['PreScript2'], rqInfo);
  if rqInfo.request.HasParam('PreScript2') then
    HandleScript(rqInfo.request['PreScript2'], rqInfo);
  if rqInfo.request.HasParam('PreScript3') then
    HandleScript(rqInfo.request['PreScript3'], rqInfo);
end;

//------------------------------------------------------------------------------
procedure ProcessPostScriptFields(rqInfo: TRequestInfo);
//Processes special Digital Tundrascript stuff passed via hidden values in a form post
var
  t, iPos: integer;
  sObject, sField: string;
  sOF: string;
begin

  //cycle through all the parameters if the parameter starts with 'fps:' then
  //process
  for t:= 0 to rqInfo.request.ParamCount-1 do begin
    sOF := rqInfo.request.ParamNames[t];
    if copy(lowercase(sOF), 1, 4) = 'fps:' then begin
      //Ditch the fps: part of the string
      sOF := copy(sOF, 5, length(sOF));

      //decrypt the value
      //sOF := SimpleDecrypt(sOF);

      //Split the remaining string into objectpoolname and fieldname
      SplitString(sOF, '::', sObject, sField);

      //if the field name has a '^' in it... ignore everything after and including it
      iPos := pos('^', sField);
      if iPos > 0 then begin
        sField := copy(sField, 1, iPos-1);
      end;

      //if the field name starts with a '$' then
      if (length(sField)>1) and (sField[1] = '$') then begin
        //Set the token parameter to the string value inputted
        sField := copy(sField, 2, length(sField));
        rqInfo.Response.ObjectPool[sObject].token.params[strtoint(sField)] := rqInfo.request.ParamsbyIndex[t];
      end else
      //if the field name starts with a '*' then
      if (length(sField)>1) and (sField[1] = '*') then begin
        //Set the token parameter to the integer value inputted
        sField := copy(sField, 2, length(sField));
        rqInfo.Response.ObjectPool[sObject].token.params[strtoint(sField)] := strtoint(rqInfo.request.ParamsbyIndex[t]);
      end else
        //OTHERWISE Assign the value of the current parameter to the field in the object
        rqInfo.Response.ObjectPool[sObject][sField].AsString := rqInfo.request.ParamsbyIndex[t];
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure ProcessPostPostScript(rqInfo: TRequestInfo);
begin
  //run the command indicated in the PostScript variables
  if rqInfo.request.HasParam('PostScript') then
    HandleScript(rqInfo.request['PostScript'], rqInfo);
  if rqInfo.request.HasParam('PostScript1') then
    HandleScript(rqInfo.request['PostScript1'], rqInfo);
  if rqInfo.request.HasParam('PostScript2') then
    HandleScript(rqInfo.request['PostScript2'], rqInfo);
  if rqInfo.request.HasParam('PostScript3') then
    HandleScript(rqInfo.request['PostScript3'], rqInfo);


  //Redirect to the redirect variable
//  if rqInfo.request.HasParam('AutoRedirect') then
//    rqInfo.response.Location := rqInfo.request.Params['AutoRedirect'];

end;
//------------------------------------------------------------------------------
procedure ProcessPostScriptAutoRedirect(rqInfo: TRequestInfo);
begin
  //Redirect to the redirect variable
  if rqInfo.request.HasParam('AutoRedirect') then
    rqInfo.response.Location := rqInfo.request.Params['AutoRedirect'];
end;

//------------------------------------------------------------------------------
procedure ProcessPostScript(rqInfo: TRequestInfo);
begin
  ProcessPrePostScript(rqInfo);
  ProcessPostScriptFields(rqInfo);
  ProcessPostPostScript(rqInfo);
  ProcessPostScriptAutoRedirect(rqInfo);
end;

function FileDate(sFile: string): string;
var
  h: integer;
begin
  h := FileOpen(sFile, FMOPENREAD+FMSHAREDENYNONE);

  if h>-1 then begin
    result := DateToStr(FileDateToDateTime(FileGetDate(h)));
    FileClose(h);
  end else begin
    result := 'unknown'
  end;




end;

function QuickFileSize(sFile: string): string;
var
  f: textfile;
begin
  assignfile(f, sFile);
  try
    reset(f);
    result := inttostr(Filesize(f))
  finally
    closefile(f);
  end


end;


procedure ReplaceEscSequences(rqInfo: TRequestInfo; slMaster: TStringList; progress_callback: TProgressCallbackProcedure);
//Scans the HTML in the passed stringlist for variables and replaces them
//with values from the dynamic-variable-pool in rqInfo.Response.VArpool[];
//TO USE:
//1. Build a web page with dynamic data marked with variable names surrounded
// by [[[... eg.   ]]]first_name%10
//2. In the CGI code define a value for the variable e.g.:
// rqInfo.Response.VarPool['first_name'] := 'jason';
//Call this function and pass it the string list that is to be scanned and the
//TRequestInfo class that has the response information.
var
  iEndRow, iEndCol: integer;
  sCompleteTag: string;
  sTagBody: string;
  iLeftStart, iRightStart: integer;
  iLeftLength, iRightLength: integer;
  iStartCol, iStartRow: integer;
  bFound: boolean;
  sReplacement: string;
  bHandled: boolean;
  t: integer;

  i64start, i64end, i64dif: int64;
const
  iPad = 3; //length of delimiters
begin
  //exit;
  //setup initial values
  iStartCol := STRZ;
  iStartRow := 0;
  rqInfo.response.content.text := rqInfo.response.content.text;
//  SaveStringAsFile(dllpath+'tempresult.txt', rqInfo.response.Content.text);

  rqInfo.PercentComplete := 0;
    repeat
//      QueryPerformanceCounter(i64Start);
//      Debug.Log(rqInfo.request.document+' looking for tags');
      bFound := FindTagPos(slMaster, '[[[', ']]]',
        iStartCol, iStartRow, iEndCol, iEndRow, sCompleteTag, sTagBody);

//      Debug.Log(rqInfo.request.document+' found tag '+sCompleteTag);

      if assigned(progress_callback) then
        progress_callback(iStartRow, slMaster.Count-1, 'Processing HTML...');

      //Get the variable named identical to returned TagBody
      //**note: exception is raised if variable doesn't exist in rqInfo.Response.Varpool[]
      if bFound then begin
        //process script commands
        //function HandleScript(sTagBody: string; response: TMotherShipWebResponse; iStartRow, iStartCol: integer): string;
        {$IFDEF DEBUG}
        try
        {$ENDIF}

//        OutputDebugString(PAnsiChar(sTagBody));
        sReplacement := BetterVarToSTr(HandleScript(sTagBody, rqInfo, iStartRow, iStartCol, bHandled));


        {$IFDEF DEBUG}
        except
          On E:Exception do begin
            sReplacement := 'EXCEPTION:' + e.message;
            slMaster[iStartRow] := sReplacement;
            for t:= iStartRow+1 to slMaster.count-1 do begin
              slMaster[t] := '';
            end;
            //break;
          end;
        end;
        {$ENDIF}


        if not bHandled then begin
          //calc the starting position of the string that is on the left of the tag (1 or 0)
          iLeftStart := 1; //=beginning of string
          iLeftLength := iStartCol-1; //= just before beginning of shpeal
          iRightStart :=  iEndCol+iPad; //= end of whole schpeal +1
          iRightLength := length(slMaster[iStartRow]);  //= length of row (auto-truncated)

          //Concatinate the row
          slMaster[iStartRow] := copy(slMaster[iStartRow],iLeftStart, iLeftLength) +
                                 sReplacement +
                                 copy(slMaster[iStartRow],iRightStart, iRightLength);
        end;

        bHandled := false;

      end;
//      QueryPerformanceCounter(i64End);
//      rqInfo.Response.DebugLog.Add(inttostr(i64End-i64Start)+'~'+sTagBody);
      if rqInfo.response.content.count > 0 then
        rqInfo.PercentComplete := iStartRow / rqInfo.response.content.count;

    until not bFound;

end;

//------------------------------------------------------------------------------
function HandleStringLiteralTag(sTagBody:string; rqInfo: TRequestInfo; iStartRow, iStartCol: integer): variant;
var
  iLength: integer;
  t: integer;
  sTemp: string;
  sScriptResult: string;
  bInString, bInPlus: boolean;
begin
  iLength := length(sTagBody);
  result := '';
  sTemp := '';
  bInSTring := false;
  bInPlus := false;

  //anything in quotes gets returned as literal
  FOR t:= 1 to length(sTagBody) do begin
    if not bInPLus then begin
      IF sTagBody[t] = '''' then begin
        bInString := not bInString;

        if not bInString then begin
          result := result + sTemp;
          sTemp := '';
        end;

        continue;
      END;
    end;

    if not bInString then begin
      IF sTagBody[t] = '+' then begin
        bInPlus := not bInPlus;

        if not bInPlus then begin
          sScriptResult := HandleSCript(sTemp, rQInfo);
          result := result + sScriptResult;

          sTemp := '';
        end;
        continue;
      END;

    end;

    if bInString or bInPLus then begin
      sTemp := sTemp + sTagBody[t];
    end;
  END;

  if bInPLus then
    result := result+HandleSCript(sTemp, rQInfo);


end;



function GetIdentifier(var sTagBody: string; out sRemainder: string): boolean;
var
  bInStr: boolean;
  t: integer;
  iLen: integer;
  iStart, iEnd: integer;
begin
  bInStr := false;

  t:=1;

  if sTagBody = '' then begin
    sREmainder := '';
    result := false;
    exit;
  end;


  iLen := length(sTagBody);
  //scan string until not ' '
  while (sTagBody[t] = ' ') do begin
    inc(t);
    if t> iLen then
      raise Exception.create('statement or expression expected');
  end;

  //scan until the next qualifying whitespace
  iStart := t;
  bInStr := IsQuote(sTagBody,t);
  //if a string then read until end quote found
  if bInStr then begin
    repeat
      inc(t);
      if t> iLen then
        raise Exception.create('unterminated string');
    until IsQuote(sTagBody,t);
    //set end to point of quote
    //e.g. "hello world"
    iEnd := t;
  end
  else begin
    //else read until qualifying white space found
    while not IsWhiteSpace(sTagBody, t) do begin
      inc(t);
      if t > iLen then begin
        break;
      end;
    end;

    iEnd := t-1;
  end;

  sRemainder := copy(sTagBody, iEnd+1, ilen);
  sTagBody := copy(sTagBody, iStart, (iend-iStart)+1);
  result := true;
end;


function GetOperator(sTagBody: string; out sOperator: string; var sRemainder: string): boolean;
var
  bInStr: boolean;
  t: integer;
  iLen: integer;
  iStart, iEnd: integer;
  sPriorOps: string;
begin
  result := true;
  bInStr := false;

  if sTagBody = '' then begin
    result := false;
    exit;
  end;

  t:=1;
  iLen := length(sTagBody);
  //scan string until not ' '
  while (sTagBody[t] = ' ') do begin
    inc(t);
    if t> iLen then
      raise Exception.create('statement or expression expected');
  end;

  //scan until the next qualifying whitespace
  sPriorOps := '';
  iStart := t;
  begin
    //read while operator found
    while  IsOperator(sTagBody,t, sPriorOps) do begin
      sPriorOps := sPriorOps + sTagBody[t];

      if t >= iLen then begin
        iEnd := iLen;
        break;
      end;
      inc(t);
    end;
    iEnd := t;
  end;


  if (iEnd < iStart) then begin
    iEnd := iStart;
  end;

  if iend > ilen then
    sREmainder := ''
  else
    sRemainder := copy(sTagBody, iEnd, ilen);
  soperator := copy(sTagBody, iStart, (iend-iStart));



end;

function IsWhiteSpace(sTag: string; iPos: integer): boolean;
begin
  result := sTag[iPos] = ' ';

  if not result then
    result := IsOperator(sTag, iPos,'');

end;

function IsQuote(sTag: string; iPos: integer): boolean;
begin
  result := sTag[iPos] = '''';
end;

function IsOperator(sTag: string; iPos: integer; sPriorOperators: string): boolean;
var
  s1: Char;
//  s2: string[2];
begin

  s1 := sTag[iPos];
  result := (s1 in operators);

  if (s1 = '(') and (sPriorOperators <> '') then
    result := false;
//  if not result then begin
//    s2 := sTag[iPos]+sTag[iPos+1];
//    result := s2 in operators;
//  end;


end;


procedure EvaluateTag(var sev: TScriptEngineVars; sTagBody: string; params: TScriptParamlist; out vResult: variant; out vtResult: TScriptParameterType);
var
  fc: string;
begin

  fc := copy(sTagBody,1,1);
  if fc = '' then begin
    vResult := '' ;
    vtResult := sptString;
  end else
  if fc = '''' then begin
    vResult := webscript.HandleStringLiteralTag(sTagBody,sev.rqINfo, sev.iStartCol, sev.iStartCol);
    vtResult := sptString;
  end else
  if fc[1] in ['0'..'9', '.','-'] then begin
    vResult := strtofloat(sTagBody);
    vtResult := sptNumber;
  end
  else begin
    vResult := webscript.HandleVariableTag(sTagBody, sev.rqINfo, sev.iStartRow, sev.iStartCol);
    vtResult := varTypeToScriptType(vResult);
  end;


end;


procedure GetParams(var sev: tScriptEngineVars; var sTagBody: string; params: TScriptParamList);
var
  iStart: integer;
  iEnd: integer;
  bInStr: boolean;
  bInParens: integer;
  sNewTag: string;
  v: variant;
  vt: tScriptParameterType;
  t: integer;
begin
  params.clear;
  iEnd := 0;
  iStart := 1;
  bInStr := false;
  bInParens := 0;
  for t:= 1 to length(sTagBody) do begin
    if sTagBody[t] = '''' then begin
      bInStr := not bInStr;
    end else
    if sTagBody[t] = '(' then begin
      inc(bInParens);

    end else
    if sTagBody[t] = ')' then begin
      DEC(bInParens);
    end;

    if (not bInStr) and (bInParens<=0) then begin
      if (sTagBody[t] in [',',')']) then begin
        if sTagBody[t] = ',' then
          iEnd := t-1
        else
          iEnd := t+bInParens;
        sNewtag :=  copy(sTagBody, iStart, ((iEnd)-iStart)+1);

        Handleoperation(sev,sNewTag, v, vt);
        if (iEnd >= iStart) then
          params.Add(v);
        iStart := iEnd +2;
        if bInParens < 0 then break;
      end;
    end;
  end;

  sTagBody := copy(sTagBody, iEnd+2, length(sTagBody));

end;

procedure HandleOperation(var sev: TScriptEngineVars; sTagBody: string; var vResult: variant; var vtResult: TScriptParameterType);
var
  params: TScriptParamList;
  vLeft,vRight: variant;
  vtLeft, vtRight: TScriptParameterType;
  sRemainder: string;
  sOP: string;
  func: TScriptFunction;
  bWasParen: boolean;
  bGoodLeft: boolean;
  sOriginalTagBody: string;

begin
  bWasParen := false;
  vLeft := NULL;
  vRight := null;
  sOriginalTagBody := sTagBody;
  sOp := '';
  vLeft := null;
  params := TScriptPAramList.create;
  try

    bGoodLeft := false;

    //get the current identifier
    if not GetIdentifier(sTagBody, sRemainder) then begin
//      vResult :=
//      vResult := null;
//      vtREsult := sptString;

      exit;
    end;



    while not bGoodLeft do begin
      //if there is an operator to the right
      repeat
         bGoodLeft := true;
        if GetOperator(sRemainder, sOP, sRemainder) then begin
          //evaluate (recursively) everything on the right
          if pos(')', sOP) > 0 then begin
            sOp := '';
            bGoodLeft := true;
            break;
          end;
          if (sOP <> '') and (sOP <> '(') then
            HandleOperation(sev, sRemainder, vRight, vtRight);

          bWasParen := sOp = '(';
          if bWasPAren then begin
            if assigned(sf.GEtFunction(sTagBody)) then
              GetParams(sev, sRemainder, params)
            else begin

//              GLOG.Debug('Enter Sub op:'+sRemainder,'webscript');
              HandleOperation(sev, sRemainder, vRight, vtRight);
              MoveThroughParens(sRemainder);
//              if sOP = '' then begin
                vLeft := vRight;
                vtLeft := vtRight;
                bGoodLEft := false;
//              GLOG.Debug('Exit Sub op result:'+BetterVarToStr(vLeft),'webscript');
//              end;

            end;

          end;
        end else begin
          break;
        end;
      until not bWasParen;

    end;

    //check if its a registered function
    func := sf.GetFunction(sTagBody);
    if assigned(func) then begin
      vLeft := func(sTagBody, sTagBody, sev, params, vtLeft);
    end else begin
        //if starting with parens then left is eval of first parens
        if (trim(sTagBody) = '') and (params.count=1) then begin
          vLEft := params[0];
          vtLEft := VarTypeToSCriptType(vLeft);
        end else begin
          //evaluate what's on the left
          if (varType(vLEft) = varNull) or (vLeft = unassigned) then begin
            EvaluateTag(sev, sTagBody, params, vLeft, vtLeft);
            vtResult := vtLeft;
          end;
        end;
    end;


    //handle operator
    if sev.bHandled then
      exit;
//    GLOG.Debug('Script: '+sTagBody+' is '+BetterVarToStr(vLeft), 'webscript');
    vLeft := ScriptParamToVariant(vLeft, vtLeft);
    vRight := ScriptParamToVariant(vRight, vtRight);

    if sOP = '' then begin
      if varType(vRight) <> varNull then begin
        vResult := vRight;
        vtResult := vtRight;
      end else begin
        vResult := vLeft;
        vtResult := vtLeft;
      end;
    end else

    if varType(vRight) = varNull then begin
      vResult := vLeft;
      vtREsult := vtLEft;
    end else begin
      if (sOP = '(') and assigned(func) then begin
        vresult := vLeft;
        vtResult := vtLeft;
      end else
      if (sOP = '(') and (NOT assigned(func)) then begin
        if (sTagBody <> '') and (varType(vLeft) = varnull) then
          raise Exception.create('"'+sTagBody+'" is undefined');
        vresult := params[0];
        vtResult := sptString;
      end else
       if sOP = ')' then begin
        vResult := vLeft;
        vtResult := vtLeft;
      end else
      if sOP = '+' then begin
        if (vtLeft = sptString) or (vtRight = sptString) then begin
           vResult := BetterVarToStr(vLeft)+BetterVarToStr(vRight);
           vtResult := sptString;
        end else begin
          vResult := vLeft+vRight;
          vtResult := vtLeft;
        end;
      end else
      if sOP = '-' then begin
        if (VarType(vLeft) = varNull) or (BetterVarToStr(vLeft)='') then begin
          vLeft := 0;
          vtLeft := sptNumber;
        end;
        vResult := vLeft-vRight;
        vtResult := vtLeft;
      end else
      if sOP = '*' then begin
        vResult := vLeft*vRight;
        vtResult := vtLeft;
      end else
      if sOP = '<' then begin
        vResult := vLeft<vRight;
        vtResult := sptBoolean;
      end else
      if sOP = '>' then begin
        vResult := vLeft>vRight;
        vtResult := sptBoolean;
      end else
      if sOP = '<=' then begin
        vResult := vLeft<=vRight;
        vtResult := sptBoolean;
      end else
      if sOP = '>=' then begin
        vResult := vLeft>=vRight;
        vtResult := sptBoolean;
      end else
      if sOP = '/' then begin
        vResult := vLeft/vRight;
        vtResult := vtLeft;
      end else
      if sOP = '=' then begin
        vResult := vLeft=vRight;
        vtResult := sptBoolean;
      end else
      if sOP = '!=' then begin
        vResult := not(BetterVarToStr(vLeft)=BetterVarToStr(vRight));
        vtResult := sptBoolean;
      end else
      if sOP = '^' then begin

      end else
      if sOP = '@' then begin

      end;
    end;

  finally
    params.free;
  end;

end;

function ScriptParamToVariant(v: variant; spt: TScriptParameterType): variant;
//This function forces a variant parameter into the type that is
//specified by spt
begin
  if (varType(v) <> varString) or (spt = sptString) then begin
    result := v;
    exit;
  end else begin
    case spt of
      sptNumber: result := strtofloat(v);
    end;
  end;
end;


{ TScriptFunctions }

constructor TScriptFunctions.create;
begin
  inherited;
  fFuncs := TStringlist.create;
  fFuncs.Sorted := true;
end;

destructor TScriptFunctions.destroy;
begin
  FFuncs.free;
  inherited;
end;

function TScriptFunctions.GetFunction(sName: string): TScriptFunction;
var
  i: integer;
begin
  LockRead;
  try
    i := FFuncs.IndexOf(lowercase(sname));
    if i < 0 then
      result := nil
    else
      result := TScriptFunction(FFuncs.objects[i]);
  finally
    UnlockRead;
  end;
end;

procedure TScriptFunctions.RegisterFunction(name: string; func: TScriptFunction);
begin
  LockWrite;
  try
    FFuncs.addObject(lowercase(name), @func);
  finally
    UnlockWrite;
  end;
end;


end.

