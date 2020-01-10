unit MTDTInterface;
//This unit is a wrapper around the ServerInterface unit.  MTDTInterface stands
//for "Middle Tier to Data Tier Interface".  It makes making calls to the ServerInterface
//easier in the context of a webserver in the folloowing ways:
//<LI>It puts exception handling around return results so that you don't have to check return results after each fetch, save, etc.</LI>
//<LI>It uses standard variables from the TRequestInfo class as parameters to ServerInterface calls so that you don't have to manually allocate TDataObjectCache classes, and lookup data-tiers in the data-tier array.  Functions in MTDTInterface require far fewer parameters than ServerInterface.pas</LI>
interface

uses
  Variants, typex, systemx, signals, RequestInfo, WebFunctions, DataObject, DataObjectCache, windows, classes, Exceptions, ErrorResource, WebConfig, managedthread;

type
  TInsertSelectThread = class(TProcessorThread)
  private
    FMod: integer;
    FrqINfo: TRequestInfo;
    FSelect: string;
    FInsert: string;
    FModField: string;
    procedure SetRqInfo(const Value: TRequestInfo);
  protected
    procedure DoExecute;override;
  public
    destructor destroy;override;
    property rqInfo: TRequestInfo read FrqINfo write Frqinfo;
    property INsert: string read FInsert write FInsert;
    property Select: string read FSelect write FSelect;
    class procedure LockAll;
    class procedure UnlockAll;
  end;


function SaveQuery2(rqInfo: TrequestInfo; obj: TDataOBject; sTable: string; Keys: array of string; bForget:boolean=false): boolean;

procedure InsertSelectMod(rqInfo2: TRequesTInfo; sInsert: string; sSelect: string; iMod: integer; sModField: string; bUseStandard : boolean = false);
procedure InsertSelect(rqInfo2: TRequesTInfo; sInsert: string; sSelect: string; thr: TInsertSElectThread = nil);
function Queryfn(rqInfo: TRequestInfo; sQuery: string):string;overload;
function QueryfnV(rqInfo: TRequestInfo; sQuery: string):variant;overload;
function LazyQueryfn(rqInfo: TRequestInfo; sQuery: string):string;overload;
function LazyQueryfnV(rqInfo: TRequestInfo; sQuery: string):variant;overload;
function GhostQueryMap(rqInfo: TRequestInfo; sQuery: string; sType: string; vKeys: variant; sSubType: string = ''; iSubKeys: integer = 0): TDataObject;
function LazyQueryMap(rqInfo: TRequestInfo; sQuery: string; iIgnoreKeys: integer; sType: string; vKeys: variant; sSubType: string = ''; iSubKeys: integer = 0): TDataObject;overload;
function LazyQueryMap(rqInfo: TRequestInfo; sQuery: string; sType: string; vKeys: variant; sSubType: string = ''; iSubKeys: integer = 0): TDataObject;overload;
function RecordQuery(rqInfo: TRequestInfo; sQuery: string; bExpectMany: boolean = true): TDataObject;

function DeleteQuery(rqInfo: TrequestInfo; obj: TDataOBject; sTable: string; Keys: array of string): boolean;
procedure WebObjectFill(rqInfo: TRequestInfo; objectName: string);
function GetFieldSum(doSource: TDataObject; sFieldName: string): integer;

function GetFieldTotal(doSource: TDataObject; sFieldName: string): integer;
function New(rqInfo: TRequestInfo; sType: string; params: variant): TDataObject;
function Ghost(rqInfo: TRequestInfo; sType: string; params: variant): TDataObject;
function GhostFetch(rqInfo: TRequestInfo; sType: string; params: variant): TDataObject;
function Fetch(rqInfo: TRequestInfo; sType: string; params: variant): TDataObject;
function LazyFetch(rqInfo: TRequestInfo; sType: string; params: variant): TDataObject;



procedure CopySubObjects(doSource, doTarget: TDataObject; bClearTarget: boolean = true; bRemoveDuplicates: boolean = false);

function QuickSession(rqInfo: TRequestInfo; bLazy: boolean = true; bNoPackage: boolean=false): TDataObject;
function GettempTableName(rqInfo: TRequestInfo; sTable: string): string;
function CReateTempTableLike(rqINfo: TRequestInfo; sLikeTable: string; bEmpty: boolean= true): string;
procedure DropTempTable(rQInfo: TRequestInfo; sTableName: string);
procedure CreateTempTableFromQuery(rqInfo: TRequestInfo; sTableName: string; sQuery: string; bAutoDrop: boolean = true);
function GetTempTableFromQuery(rqInfo: TRequestInfo; sTableName: string; sQuery: string; bAutoDrop: boolean = true): TDataObject;
procedure GenerateRekeyMap(rqInfo: TRequestInfo; sTempTable: string; sField: string; iKEYConstant: integer);
procedure ApplyKeyMap(rqInfo: TRequestInfo; sDataTableNonTemp: string; sField: string; sTargetField: string = '');
procedure CopyTempTableToPublic(rqInfo: TRequestInfo; sTempTable: string; sTable: string);





function UpdateQuery(rqInfo: TRequestInfo; sQuery: string; bExpectMany: boolean = true):boolean;
function NoTransUpdateQuery(rqInfo: TRequestInfo; sQuery: string):boolean;
function FireForgetQuery(rqInfo: TRequestInfo; sQuery: string; bSerialize: boolean=false):boolean;
function Query(rqInfo: TRequestInfo; sQuery: string; bExpectMany: boolean = true): TDataObject;
function SaveQuery(rqInfo: TrequestInfo; obj: TDataOBject; sTable: string; Keys: array of string; bNoTrans :boolean= false): boolean;
function LazyQuery(rqInfo: TRequestInfo; sQuery: string; bExpectMany: boolean = true): TDataObject;


procedure LinkQuery(rqInfo: TRequestInfo; sTable: string; sLeftColumn, sRightColumn, sLeftValue, sRightValue: string);
function CopyObject(rqInfo: TRequestInfo; source: TDataObject): TDataObject;

var
  sectInsertSelect: TCLXCriticalSection;

implementation
uses
  DataObjectServices, SysUtils, CommonRequests, dialogs, orderlyinit,
  ErrorHandler, WebResource, Rights, stringx, BackgroundThreads;

function Query(rqInfo: TRequestInfo; sQuery: string; bExpectMany: boolean = true): TDataObject;
//a: Jason Nelson
begin
  if pos('[[[', sQuery) > 0 then
    sQuery := rqInfo.ProcessQuery(sQuery);

  if not rqINfo.server.Query(rqInfo.Response.DOCache, result,sQuery, rqInfo.SessionID, bExpectMany,rqInfo.response.DebugLog) then begin
    raise ENewException.create(rqInfo.Server.GetLastErrorCode, ERR_FETCH, 'Could not query : '+sQuery+'-- '+rqInfo.Server.GetLastErrorMessage);
  end;
end;

function RecordExists(rqInfo: TRequestInfo; sQuery: string): boolean;
var
  q: TDataObject;
begin
  q:= LazyQuery(rqInfo, sQuery);

  result := q.objectcount > 0;

end;


function RecordQuery(rqInfo: TRequestInfo; sQuery: string; bExpectMany: boolean = true): TDataObject;
//a: Jason Nelson
begin
  if not rqINfo.server.RecordQuery(rqInfo.Response.DOCache, result, sQuery, rqInfo.SessionID, bExpectMany) then begin
    raise ENewException.create(rqInfo.server.GetLastErrorCode, ERR_FETCH, 'Could not query : '+sQuery+'-- '+rqInfo.server.GetLastErrorMessage);
  end;
end;



//------------------------------------------------------------------------------
function New(rqInfo: TRequestInfo; sType: string; params: variant): TDataObject;
//a: Jason Nelson
//Attempts to create a NEW object/record.  The parameters passed become a template for
//the keys of the record(s) to be created, however, individual types of objects
//can override the keys (and most DO).  Each class of object typically implements a
//way to fetch the new keys from the database to automatically guarantee uniqueness.  You should
//always pass keys, howerver, because some relationships are dictated, depending on the type of object being created.
//p: sType: The typename of the Data object e.g. "TdoUser", "TdoClass"
//p: param: a variant or variant array of parameters for the object.
begin
  if not rqINfo.server.New(rqInfo.Response.DOCache, result, sType, params, rqInfo.SessionID) then begin
    raise ENewException.create(rqInfo.server.GetLastErrorCode, ERR_SAVE, 'Could not create '+sType+': '+rqInfo.server.GetLastErrorMessage);
  end;
end;

//------------------------------------------------------------------------------
function GhostFetch(rqInfo: TRequestInfo; sType: string; params: variant): TDataObject;
//TODO 1:for the time being this just does the same thing as Ghost this is
//probably BAD
begin
  if not rqINfo.server.GhostFetch(rqInfo.Response.DOCache, result, sType, params, rqInfo.SessionID) then begin
    raise ENewException.create(rqInfo.server.GetLastErrorCode, ERR_INTERNAL, 'Could not create '+sType+': '+rqInfo.server.GetLastErrorMessage);
  end;
end;

//------------------------------------------------------------------------------
function Ghost(rqInfo: TRequestInfo; sType: string; params: variant): TDataObject;
//a: Jason Nelson
//Creates a NEW object, however, does not allow the object to manage its own keys and instead uses the keys passed in the PARAMs parameter.
//p: sType: The typename of the Data object e.g. "TdoUser", "TdoClass"
//p: param: a variant or variant array of parameters for the object.
begin
  if not rqINfo.server.Ghost(rqInfo.Response.DOCache, result, sType, params, rqInfo.SessionID) then begin
    raise ENewException.create(rqInfo.server.GetLastErrorCode, ERR_INTERNAL, 'Could not create '+sType+': '+rqInfo.server.GetLastErrorMessage);
  end;
end;

//------------------------------------------------------------------------------
function Fetch(rqInfo: TRequestInfo; sType: string; params: variant): TDataObject;
//a: Jason Nelson
//Fetches an object that has a query definition defined with it
//p: sType: The typename of the Data object e.g. "TdoUser", "TdoClass"
//p: param: a variant or variant array of parameters for the object.
begin
  if not rqINfo.server.Fetch(rqInfo.Response.DOCache, result, sType, params, rqInfo.SessionID) then begin
    raise ENewException.create(rqInfo.server.GetLastErrorCode, ERR_INTERNAL, 'Could not create '+sType+': '+rqInfo.server.GetLastErrorMessage);
  end;
end;

function LazyFetch(rqInfo: TRequestInfo; sType: string; params: variant): TDataObject;
//a: Jason Nelson
//Fetches an object that has a query definition defined with it
//p: sType: The typename of the Data object e.g. "TdoUser", "TdoClass"
//p: param: a variant or variant array of parameters for the object.
begin
  if not rqINfo.server.LazyFetch(rqInfo.Response.DOCache, result, sType, params, rqInfo.SessionID) then begin
    raise ENewException.create(rqInfo.server.GetLastErrorCode, ERR_INTERNAL, 'Could not create '+sType+': '+rqInfo.server.GetLastErrorMessage);
  end;
end;



//------------------------------------------------------------------------------
function GetFieldSum(doSource: TDataObject; sFieldName: string): integer;
//a: Jason Nelson
//Given a Data object that is a LIST (contains detail objects), adds up all values
//in the given field for all detail objects and returns a sum.
//p: doSource: The LIST of objects you wish to calculate on.
//p: sFieldName: the Name of the field in the DETAIL objects that you wish to sum.
var
  t: integer;
begin
  result := 0;
  for t:= 0 to doSource.ObjectCount-1 do begin
    try
      if doSource.obj[t][sFieldName].AsVariant > 0 then
        result := result + doSource.obj[t][sFieldName].AsVariant;
    except
      result := -1;
    end;
  end;
end;

//------------------------------------------------------------------------------
function GetFieldAverage(doSource: TDataObject; sFieldName: string): real;
//a: Jason Nelson
//Given a Data object that is a LIST (contains detail objects), adds up all values
//in the given field for all detail objects and returns a sum.
//p: doSource: The LIST of objects you wish to calculate on.
//p: sFieldName: the Name of the field in the DETAIL objects that you wish to sum.
var
  t: integer;
begin
  result := 0;

  //prevent divide by zero error
  if doSOurce.objectcount = 0 then
    exit;


  for t:= 0 to doSource.ObjectCount-1 do begin
    try
      if doSource.obj[t][sFieldName].AsVariant > 0 then
        result := result + doSource.obj[t][sFieldName].AsVariant;
    except
      result := -1;
    end;
  end;

  result := result / (doSource.ObjectCount);


end;


function GetFieldTotal(doSource: TDataObject; sFieldName: string): integer;
//a: Jason Nelson
//THIS FUNCTION is custom made for the account statistics.  It is used only by
//the acount page to get the total users on the sytsem.
//p: doSource: The LIST of objects you wish to calculate on.
//p: sFieldName: the Name of the field in the DETAIL objects that you wish to sum.
var
  t: integer;
begin
  result := 0;
  for t:= 0 to doSource.ObjectCount-1 do begin
    try
      if doSource.obj[t].assoc['statistics'][sFieldName].AsVariant > 0 then
        result := result + doSource.obj[t].assoc['statistics'][sFieldName].AsVariant;
    except
      result := -1;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure CopyAccountSubObjects_CrossCache(rqInfo: TRequestInfo; dtID: integer; doSource, doTarget: TDataObject);
//a: Jason Nelson
//Used to build a giant list of accounts from multiple data-tiers.
var
  t,u: integer;
  doTargetSub: TDataObject;
  doSourceSub: TDataObject;
  doNewStats: TDataObject;
  doSourceStats: TDataObject;
begin
  //ghost a new object
  for t:= 0 to doSource.objectcount-1 do begin
    doSourceSub := doSource.obj[t];
    //ghost a copy of the object in the new cache
    doTargetSub := Ghost(rqInfo, doSource.obj[t].token.typename, VarArrayOf([doSource.obj[t].token.params[0], dtID]));
(*    //add the datatier ID so that it is unique
    doTargetSub.token.AddParam(dtID);
    //reregister the Data object because the parameters changed
    rqInfo.response.DOCache.DeRegisterDataObject(doTargetSub);
    rqInfo.response.DOCache.RegisterDataObject(doTargetSub);
*)
    //copy the fields from the source to the target -- no source cache required
    for u:= 0 to doSource.obj[t].FieldCount-1 do begin
      doTargetSub.FieldByIndex[u].AsVariant := doSource.obj[t].FieldByIndex[u].AsVariant;
    end;
    //add the newly ghosted object to the target object
    doTarget.AddObject(doTargetSub);

    //session statistics object
    doSourceStats := doSourceSub.assoc['statistics'];
    doNewStats := Ghost(rqInfo, 'TdoSessionStatistics', doSourceSub.token.params[0]);
    //copy the fields from the source to the target -- no source cache required

    for u:= 0 to doSourceStats.FieldCount-1 do begin
      doNewStats.FieldByIndex[u].AsVariant := doSourceStats.FieldByIndex[u].AsVariant;
    end;


  end;
end;


//------------------------------------------------------------------------------
procedure CopySubObjects(doSource, doTarget: TDataObject; bClearTarget: boolean = true; bRemoveDuplicates: boolean = false);
//a: Jason Nelson
//Copies Detail objects from doSource to doTarget.
//p: doSource: The Source Data Object.
//p: doTarget: The target Data object+.
//p: bClearTarget: if TRUE clears the detail objects from the Target before copy.
var
  t: integer;
begin
  if bClearTarget then
    doTarget.clearObjects;

  for t:= 0 to doSource.objectcount-1 do begin
    if pos('dated', lowercase(doSource.token.typename))>0 then begin
      doTarget.AddObject(doSource.obj[t].token.typename, VarArrayOf([doTarget.token.params[0], doSource.obj[t].token.params[1]]));
    end else begin
      if not bRemoveDuplicates then
        doTarget.AddObject(doSource.obj[t])
      else
        if not doTarget.HasObject(doSource.obj[t]) then
          doTarget.AddObject(doSource.obj[t]);
    end;

  end;


end;
//------------------------------------------------------------------------------
function QuickSession(rqInfo: TRequestInfo; bLazy: boolean = true; bNoPackage: boolean=false): TDataObject;
//Quick helper function returns the session object automatically given the TReqeustInfo object.
//It will prefetch a packaage that includes user information, account information, and rights information
//if the request type is a GET, otherwise only fetches a Session Object.
//You can force it to fetch only the session with the bNoPackage parameter.
//p: bLazy: defaults to TRUE, if TRUE uses a cached version of the session if available.
//p: bNoPackage: avoids fetching the session package (for optimization)
begin

  result := LazyQueryMap(rqInfo, 'SELECT * from Session where sessionid='+inttostr(rqInfo.sessionid), 'TdoSession', rqInfo.SessionID);
  if result = nil then begin
    raise ENewException.create(117, 'Your session has expired','Session not found');
  end;

  rqinfo.response.objectpool['session'] := result;


end;
















function DeleteQuery(rqInfo: TrequestInfo; obj: TDataOBject; sTable: string; Keys: array of string): boolean;
var
  sQuery: string;
//  iKeyCount: integer;
//  sCommand: string;
  sWhere: string;
  t: integer;
begin

  result := true;
//  iKeyCount := High(Keys)-Low(Keys)+1;

  if 1=1 then begin
    sQuery := 'DELETE FROM '+sTable+' WHERE ';

    for t:= 0 to obj.token.paramcount-1 do begin
      sWhere := sWhere+Keys[t+Low(Keys)]+'='+vartostr(obj.token.params[t]);
      if t <obj.token.paramcount-1 then
        sWhere := sWhere + ' and ';
    end;

    sQuery := sQuery+sWhere+'';
    Query(rqInfo, sQuery);
  end;
end;

function SaveQuery(rqInfo: TrequestInfo; obj: TDataOBject; sTable: string; Keys: array of string; bNoTrans:boolean=false): boolean;
var
  sQuery: string;
//  iKeyCount: integer;
  sCommand: string;
  sWhere: string;
  t: integer;
begin
  result := true;
//  iKeyCount := High(Keys)-Low(Keys)+1;

  if 1=1 then begin
    sQuery := 'DELETE FROM '+sTable+' WHERE ';

    for t:= 0 to obj.token.paramcount-1 do begin
      sWhere := sWhere+Keys[t+Low(Keys)]+'="'+vartostr(obj.token.params[t])+'"';
      if t <obj.token.paramcount-1 then
        sWhere := sWhere + ' and ';
    end;

    sQuery := sQuery+sWhere+'';
    if bNoTrans then
      NoTransupdateQuery(rQInfo, sQuery)
    else
      updatequery(rQInfo, sQuery, false);
//    rqINfo.server.UpdateQuery(rqInfo.response.DOCache, sQuery, rqInfo.sessionid, false, rQInfo.response.DebugLog);
  end;

  sCommand := 'INSERT INTO '+sTable+' Values (';

  for t:= 0 to obj.token.paramcount-1 do begin
    sCommand := sCommand+'"'+vartostr(obj.token.params[t])+'"';
    if t <obj.token.paramcount-1+obj.fieldcount then
      sCommand := sCommand + ',';
  end;

  for t:= 0 to obj.fieldcount-1 do begin

    sCommand := sCommand+obj.fieldbyindex[t].QuotedStorageString;

    if t <obj.fieldcount-1 then
      sCommand := sCommand + ',';
  end;

  sCommand := sCommand + ')';

  sQuery := sCommand;

  if bNoTrans then
    NoTransupdateQuery(rQInfo, sQuery)
  else
    updatequery(rQInfo, sQuery, false);

end;


function SaveQuery2(rqInfo: TrequestInfo; obj: TDataOBject; sTable: string; Keys: array of string; bForget:boolean=false): boolean;
var
  sQuery: string;
//  iKeyCount: integer;
  sCommand: string;
  t: integer;
begin
  result := true;
//  iKeyCount := High(Keys)-Low(Keys)+1;

  sCommand := 'UPDATE '+sTable+ ' SET ';

  for t:= 0 to obj.fieldcount-1 do begin
    sCommand := sCommand+obj.fieldbyindex[t].name+'="'+obj.fieldbyindex[t].StorageString+'"';
    if t <obj.fieldcount-1 then
      sCommand := sCommand + ',';
  end;

  sCommand := sCommand + ' WHERE ';

  for t:= low(keys) to high(keys) do begin
    sCommand := sCommand+keys[t]+'="'+vartostr(obj.token.params[t])+'"';
    if t <high(keys) then
      sCommand := sCommand + ' and ';
  end;

//  sCommand := sCommand + ')';

  sQuery := sCommand;

  if bForget then
    FireForgetQuery(rQInfo, sQuery,false)
  else
    updatequery(rQInfo, sQuery, false);

end;


procedure WebObjectFill(rqInfo: TRequestInfo; objectName: string);
var
  t: integer;
  iCount: integer;
  s, sObjectName: string;
  sObj, sField, sValue: string;
  obj: TDataObject;
begin
  obj := rqINfo.response.objectpool[objectName];

  sObjectName := ObjectName+'::';

  //blank out checkboxes first
  iCount := rqInfo.request.ParamPatternCount['_'+sObjectName];
  for t:= 0 to iCount-1 do begin
    s := rqInfo.Request.ParamNamesbyPatternMatch['_'+sObjectName, t];
    sValue := rqInfo.request.Params[s];
    sValue := CleanInjection(sValue);
    SplitString(s, '::', sObj, sField);
    if not (sField[1] = '#') then
      obj[sField].AsString := sValue;
  end;

  //do the rest
  iCount := rqInfo.request.ParamPatternCount[sObjectName];
  for t:= 0 to iCount-1 do begin
    s := rqInfo.Request.ParamNamesbyPatternMatch[sObjectName, t];
    sValue := rqInfo.request.Params[s];
    sValue := CleanInjection(sValue);
    SplitString(s, '::', sObj, sField);
    if not (sField[1] = '#') then
      obj[sField].AsString := sValue;
  end;

  sObjectName := ObjectName+'::#';
  iCount := rqInfo.request.ParamPatternCount[sObjectName];
  for t:= 0 to iCount-1 do begin
    s := rqInfo.Request.ParamNamesbyPatternMatch[sObjectName, t];
    sValue := rqInfo.request.Params[s];
    sValue := CleanInjection(sValue);
    SplitString(s, '::#', sObj, sField);
    obj.token.params[strtoint(sField)] := strtoint(sValue);
  end;

end;









function UpdateQuery(rqInfo: TRequestInfo; sQuery: string; bExpectMany: boolean = true):boolean;
begin
  if pos(']]]', sQuery) > 0 then
    sQuery := rqInfo.ProcessQuery(sQuery);

  if not rqINfo.server.UpdateQuery(rqInfo.Response.DOCache, sQuery, rqInfo.SessionID, bExpectMany,rqInfo.response.DebugLog) then begin
    raise ENewException.create(rqInfo.server.GetLastErrorCode, ERR_FETCH, 'Could not query : '+sQuery+'-- '+rqInfo.server.GetLastErrorMessage);
  end;
  result := true;
end;

function NoTransUpdateQuery(rqInfo: TRequestInfo; sQuery: string):boolean;
begin
  if pos('[[[', sQuery) > 0 then
    sQuery := rqInfo.ProcessQuery(sQuery);

  if not rqINfo.server.NoTransUpdateQuery(rqInfo.Response.DOCache, sQuery, rqInfo.SessionID) then begin
    raise ENewException.create(rqInfo.server.GetLastErrorCode, ERR_FETCH, 'Could not query : '+sQuery+'-- '+rqInfo.server.GetLastErrorMessage);
  end;
  result := true;
end;


procedure LinkQuery(rqInfo: TRequestInfo; sTable: string; sLeftColumn, sRightColumn, sLeftValue, sRightValue: string);
var
  doQuery: TDataObject;
  sSQL: string;
begin
  sSQL := 'SELECT * from '+sTable+' WHERE ('+sLeftColumn+'='+sLeftValue+') and ('+sRightColumn+'='+sRightValue+')';

  if rqINfo.server.Query(rQInfo.Response.DOCache, doQuery, sSQL, rqInfo.SessionID, true, rqInfo.response.DebugLog) then begin
    if doQUery.objectcount = 0 then
      UpdateQuery(rqInfo, 'INSERT into '+sTable+' VALUES ('+sLEftValue+','+sRightValue+')');
  end;



end;


function LazyQuery(rqInfo: TRequestInfo; sQuery: string; bExpectMany: boolean = true): TDataObject;
begin
  sQuery := rqInfo.ProcessQuery(sQuery);

  if not rqINfo.server.LazyQuery(rqInfo.Response.DOCache, result, sQuery, rqInfo.SessionID, bExpectMany,rqInfo.response.DebugLog) then begin
    raise ENewException.create(rqInfo.server.GetLastErrorCode, ERR_FETCH, 'Could not query : '+sQuery+'-- '+rqInfo.server.GetLastErrorMessage);
  end;
end;

//------------------------------------------------------------------------------
function GhostQueryMap(rqInfo: TRequestInfo; sQuery: string; sType: string; vKeys: variant; sSubType: string = ''; iSubKeys: integer = 0): TDataObject;
var
  iIgnoreKeys: integer;
//  iKeyCount: integer;
begin
  iIgnoreKeys := 0;
//  iKeyCount := VarArrayHighBound(vKeys,1)-VarArrayLowBound(vkeys,1);
  if not rqINfo.server.LazyQueryMap(rqInfo.Response.DOCache, result, sQuery, rqInfo.SessionID, 300000, iIgnoreKeys, sType, vKeys, rqInfo.Response.DebugLog, sSubType, iSubKeys) then begin
    result := Ghost(rqInfo, sType, vKeys);
  end;
end;
//------------------------------------------------------------------------------
function LazyQueryMap(rqInfo: TRequestInfo; sQuery: string; sType: string; vKeys: variant; sSubType: string = ''; iSubKeys: integer = 0): TDataObject;
begin
  result := LazyQueryMap(rQInfo, squery, 0, sType, vKeys, sSubType, iSubKeys);
end;
//------------------------------------------------------------------------------
function LazyQueryMap(rqInfo: TRequestInfo; sQuery: string; iIgnoreKeys: integer; sType: string; vKeys: variant; sSubType: string = ''; iSubKeys: integer = 0): TDataObject;
begin
  if pos(']]]', sQuery) > 0 then
    sQuery := rqInfo.ProcessQuery(sQuery);


  if not rqINfo.server.LazyQueryMap(rqInfo.Response.DOCache, result, sQuery, rqInfo.SessionID, 300000, iIgnoreKeys, sType, vKeys, rqInfo.Response.DebugLog, sSubType, iSubKeys) then begin

    if rqInfo.request.hasparam('template') and (not (lowercase(rqInfo.request['template']) = 'shop')) and (pos('select * from session', lowercase(sQuery)) > 0) and not rQInfo.request.hasparam('shop') then
      rqInfo.response.location := 'out/login.ms'
    else
      raise ENewException.create(rqInfo.server.GetLastErrorCode, ERR_FETCH, 'Could not query : '+sQuery+'-- '+rqInfo.server.GetLastErrorMessage);

  end;
end;

function LazyQueryfn(rqInfo: TRequestInfo; sQuery: string):string;
begin
  result := vartostr(LazyQueryFnV(rqInfo, sQuery));

end;


function Queryfn(rqInfo: TRequestInfo; sQuery: string):string;
begin
  result := vartostr(QueryFnV(rqInfo, sQuery));

end;

function LazyQueryfnV(rqInfo: TRequestInfo; sQuery: string):variant;
var
  d: TDataObject;
begin
  if pos(']]]', sQuery) > 0 then
    sQuery := rqInfo.ProcessQuery(sQuery);

  d := LazyQuery(rQInfo, sQuery, true);
  if d.objectcount = 0 then
    result := null
  else
    result := d.obj[0].fieldbyindex[0].AsVariant;


end;

function QueryfnV(rqInfo: TRequestInfo; sQuery: string):variant;
var
  d: TDataObject;
begin
  if pos('[[[', sQuery) > 0 then
    sQuery := rqInfo.ProcessQuery(sQuery);

  d := Query(rQInfo, sQuery, true);
  if d.objectcount = 0 then
    result := null
  else
    result := d.obj[0].fieldbyindex[0].AsVariant;


end;



function CopyObject(rqInfo: TRequestInfo; source: TDataObject): TDataObject;
var
  t: integer;
begin
  result := New(rqInfo, source.Token.TypeName, source.Token.VariantParams);
  for t:= 0 to source.FieldCount-1 do begin
    result.FieldByIndex[t].AsVariant := source.FieldByIndex[t].AsVariant;
  end;


end;

function FireForgetQuery(rqInfo: TRequestInfo; sQuery: string; bSerialize: boolean=false):boolean;
begin
  if pos(']]]', sQuery) > 0 then
    sQuery := rqInfo.ProcessQuery(sQuery);

  if not rqINfo.server.FireForgetQuery(rqInfo.Response.DOCache, sQuery, rqInfo.SessionID) then begin
    raise ENewException.create(rqInfo.server.GetLastErrorCode, ERR_FETCH, 'Could not query : '+sQuery+'-- '+rqInfo.server.GetLastErrorMessage);
  end;
  result := true;
  
end;

function GettempTableName(rqInfo: TRequestInfo; sTable: string): string;
begin
  if lowercase(copy(sTable, 1,4)) = 'temp' then
    result := sTable
  else
    result := 'Temp'+inttostr(rqInfo.sessionid)+'_'+sTable;




end;

procedure DropTempTable(rQInfo: TRequestInfo; sTableName: string);
begin
  NoTransupdatequery(rqInfo, 'drop table if exists '+GetTempTableNAme(rqINfo, sTABleName));
end;
procedure CreateTempTableFromQuery(rqInfo: TRequestInfo; sTableName: string; sQuery: string; bAutoDrop: boolean = true);
begin
  if bAutoDrop then
    DropTempTable(rqInfo, sTableName);

  NoTransUpdateQuery(rqInfo, 'create table '+GetTempTableName(rqInfo, sTableName)+' '+sQuery);

end;

function CReateTempTableLike(rqINfo: TRequestInfo; sLikeTable: string; bEmpty: boolean= true): string;
begin
  NoTransUpdateQuery(rqInfo, 'create table if not exists '+GetTempTableName(RQinfo, sLIkeTable)+' like '+sLikeTable);
  if bEmpty then begin
    NoTransUpdateQuery(rQInfo, 'delete from '+GetTempTableName(RQinfo, sLIkeTable));
  end;
end;

function GetTempTableFromQuery(rqInfo: TRequestInfo; sTableName: string; sQuery: string; bAutoDrop: boolean = true): TDataObject;
begin
  CreateTempTableFromQuery(rqInfo, sTAbleName, sQuery);
  result := Query(rqInfo, 'select * from '+GetTempTableName(rqinfo, sTableName));

end;

function GetRekeyTempTableName(rqInfo: TRequestInfo; sField: string; bIncludePrefix: boolean = true): string;
begin
  result := 'rekey_'+sfield;

  if bInCludePrefix then
    result := GetTempTAbleName(rqInfo, result);


end;

procedure GenerateRekeyMap(rqInfo: TRequestInfo; sTempTable: string; sField: string; iKEYConstant: integer);
(*var
  sReKeyTable: string;
  doTemp: TDataObject;
  t: integer;
  k1: string;
  k2: string;*)
begin
(*  sReKeyTable := GetRekeyTemptableName(rqInfo, sField);

  //select distinct keys from table in order into another temptable with naming convention
  doTemp := GetTempTableFromQuery(
    rqInfo,
    sReKeyTable,
    'select distinct '+sField+' as key1, '+sfield+' as key2 from '+GetTempTableName(rqInfo, sTempTable)
  );

  //scan and generate queries to update keys in keymap
  for t:= 0 to doTemp.objectcount-1 do begin
    k1 := doTemp.obj[t]['key1'].AsString;
    k2 := inttostr(rqInfo.Server.GetNextID(iKeyConstant));

    NoTransUpdateQuery(rQinfo, 'update '+sRekeyTable+' set key2='+k2+' where key1='+k1);
  end;*)

end;

procedure ApplyKeyMap(rqInfo: TRequestInfo; sDataTableNonTemp: string; sField: string; sTargetField: string = '');
var
  sREKeyTable: string;
  t: integer;
  q: TDataObject;
  k1,k2: string;
  sTempDataTable: string;
begin
  //to apply a keymap, a keymap must be prebuilt!!


  //if no alternate target field is supplied then use the regular field name
  if sTargetField = '' then
    sTargetField := sField;

  //determine the name of the temp tab;e used for the field in question
  sReKeyTable := GetRekeyTempTableName(rqInfo, sField);
  sTempDataTAble := GetTempTableName(rqInfo, sDataTableNonTemp);


  //fetch the keymap
  q := Query(rqInfo, 'select * from '+sReKEyTAble);
  //apply key changes to table
  for t:= 0 to q.objectcount-1 do begin
    k1 := q.obj[t]['key1'].asString;
    k2 := q.obj[t]['key2'].AsString;
    NoTransUpdateQuery(rqInfo, 'update '+sTempDataTable+' set '+sTargetField+'='+k2+' where '+sTargetField+'='+k1);
  end;

end;

procedure CopyTempTableToPublic(rqInfo: TRequestInfo; sTempTable: string; sTable: string);
begin
  sTempTable := GetTempTAbleName(rqINfo, sTEmpTable);
  UpdateQuery(rqInfo, 'insert ignore into '+sTable+' select * from '+stempTable);
end;


procedure InsertSelectMod2(rqInfo2: TRequesTInfo; sInsert: string; sSelect: string; iMod: integer; sModField: string);
var
  t: integer;
begin
  for t:= 0 to iMod -1 do begin
    rqInfo2.UpdateProgress(t, iMod, 'Mod Update...'+inttostr(t)+' of '+inttostr(iMod));
    sSelect := stringReplace(sSelect, '%%%%', ' (mod('+sModfield+','+inttostr(iMod)+')='+inttostr(t)+') ', [rfReplaceall]);
    InsertSelect(rqInfo2, sInsert, sSelect);
  end;

end;

procedure InsertSelectMod(rqInfo2: TRequesTInfo; sInsert: string; sSelect: string; iMod: integer; sModField: string; bUseStandard : boolean = false);
var
  t: integer;
  ist: TInsertSElectThread;
  sss: string;
begin
  if bUseStandard then begin
    for t:= 0 to iMod-1 do begin
      rqINfo2.UpdateProgress(t, iMod, '');
      sss := stringReplace(sSelect, '%%%%', ' (mod('+sModfield+','+inttostr(iMod)+')='+inttostr(t)+') ', [rfReplaceall]);
      NoTransUpdateQuery(rqInfo2, sInsert + '('+sss+')');
    end;
    exit;
  end;
  for t:= 0 to iMod -1 do begin
    while backgroundthreadman.ThreadClassCount(TInsertSelectThread) > 1 do begin
      sleep(1000);
    end;


    ist := TPM.NeedThread<TInsertSelectThread>(nil);
    sss := stringReplace(sSelect, '%%%%', ' (mod('+sModfield+','+inttostr(iMod)+')='+inttostr(t)+') ', [rfReplaceall]);
    ist.name := 'IST'+inttostr(t);
    ist.rqINfo := rqInfo2;
    ist.Select := sss;
    ist.Insert := sInsert;
    ist.Start;

//    rqInfo2.UpdateProgress(t, iMod, 'Mod Update...'+inttostr(t)+' of '+inttostr(iMod));
//    InsertSelect(rqInfo2, sInsert, sSelect);
  end;

  while backgroundthreadman.HasThreadClass(TInsertSelectThread) do begin
    sleep(1000);
  end;

end;

procedure InsertSelect(rqInfo2: TRequesTinfo; sInsert: string; sSelect: string; thr: TInsertSElectThread = nil);
var
  q,qq: TDataObject;
  t: integer;
  u: integer;
  s,ss: string;
  rqInfo: TRequestInfo;
begin

  rqInfo := TRequesTInfo.CopyCreate(rQInfo2);
  try
    if assigned(thr) then thr.lockall;
    try
      q:=Query(rqInfo, sSelect);
    finally
      if assigned(thr) then thr.unlockall;
    end;
    s := '';
    for t:= 0 to q.objectcount-1 do begin
      qq := q.obj[t];

      if s <> '' then
        s := s + ',';


      ss := '';
      for u := 0 to qq.FieldCount-1 do begin
        if u > 0 then ss := ss + ',';
        ss := ss+qq.FieldByIndex[u].StorageString;
      end;
      s := s+'('+ss+ ')';


      if (t mod 100) = 0 then
        rqInfo.UpdateProgress(t,q.objectcount,'Rolling up...');


      if ((t mod 100) = 99) or ((t div 100) = (q.objectcount div 100)) then begin
        FireForgetQuery(rqInfo, sInsert+' values '+s,true);
        s := '';
      end;

    end;
  finally
    rqInfo.free;
  end;

end;



{ TInsertSelectThread }

destructor TInsertSelectThread.destroy;
begin
//  rqINfo.free;
  inherited;
end;

procedure TInsertSelectThread.DoExecute;
begin
  InsertSelect(rqInfo, insert, select,self);
end;

class procedure TInsertSelectThread.LockAll;
begin
//  entercriticalsection(sectinsertselect);
end;

procedure TInsertSelectThread.SetRqInfo(const Value: TRequestInfo);
begin
//  FrqINfo := TRequestINfo.copyCreate(value);
end;

class procedure TInsertSelectThread.UnlockAll;
begin
  lcs(sectinsertselect);
end;

procedure oinit;
begin
  ics(sectinsertselect);

end;

procedure ofinal;
begin
  dcs(sectInsertSelect);


end;

initialization
  init.RegisterProcs('MTDTInterface', oinit, ofinal);

finalization

end.
