unit SystemQueries;

interface

uses Requestinfo, MTDTInterface, sysutils, Dataobject, webstring;

procedure BookmarkThis(rQInfo: TrequestInfo; sName: ansistring='');
function GetBookMark(rqInfo: TrequestInfo; sName: ansistring=''): ansistring;
procedure SaveBookMark(rqInfo: TRequestInfo; sURL: ansistring; sName: ansistring='');
procedure GotoBookmark(rqInfo: TrequestInfo; sName: ansistring='');
procedure SaveUserVar(rqInfo: TRequestInfo; sName: ansistring; sValue: variant; iUserID: integer=0);
function LoadUserVar(rqInfo: TRequestInfo; sName: ansistring; out sValue: variant; iUserID: integer=0): boolean;overload;
function LoadUserVar(rqInfo: TRequestInfo; sName: ansistring; iUserID: integer=0): ansistring;overload;
function LoadUserVar(rqInfo: TRequestInfo; sName: ansistring; sDefault: ansistring ;iUserID: integer=0): ansistring;overload;

implementation

uses WebFunctions, CommonRequests, SimpleWinSock;


procedure BookmarkThis(rQInfo: TrequestInfo; sName: ansistring='');
begin
//  rqinfo.CloseServer;
  rQInfo.SaveVar('BOOKMARK_'+sName, rqINfo.request.fullURL);
//  UpdateQuery(rqInfo, 'UPDATE session set BookMark="'+rqInfo.Request.FullURL+'" where sessionid='+inttostr(rqInfo.sessionid));
//  closeServer(rqinfo);
end;

function GetBookMark(rqInfo: TrequestInfo; sName: ansistring=''): ansistring;
var
  doSession: TDataObject;
begin
  result := rqINfo.LoadVar('BOOKMARK_'+sName, '');
//  doSession := quickSession(rqInfo);
//  result := doSession['bookmark'].AsString;
end;

procedure SaveBookMark(rqInfo: TRequestInfo; sURL: ansistring; sName: ansistring='');
begin
  rqInfo.SaveVar('BOOKMARK_'+sName, sURL);
end;


procedure GotoBookmark(rqInfo: TrequestInfo; sName: ansistring='');
var
  bm: ansistring;
begin
  bm := getBookmark(rQInfo, sName);
  if bm = '' then
    bm := getBookMark(rQInfo);

  SendJavaRedirect(rqInfo, bm, false, false);
end;

procedure SaveUserVar(rqInfo: TRequestInfo; sName: ansistring; sValue: variant; iUserID: integer=0);
begin
  if iUserID = 0 then
    iUserID := QuickSession(rqInfo)['userid'].AsVariant;

  if assigned(rqINfo) then begin
    NoTransUpdateQuery(rqInfo, 'delete from user_vars where userid='+inttostr(iUseRID)+' and VarID="'+lowercase(sName)+'"');
    NoTransUpdateQuery(rqInfo, 'insert ignore into user_vars values('+inttostr(iUseRID)+', "'+lowercase(sName)+'","'+sName+'","'+EncodeWebString(sValue)+'")');
  end;
end;



function LoadUserVar(rqInfo: TRequestInfo; sName: ansistring; out sValue: variant; iUserID: integer=0): boolean;
begin
  result := true;
  if iUserID = 0 then
    iUserID := QuickSession(rqInfo)['userid'].AsVariant;
  try
    sValue := DecodeWebString(LazyQueryFn(rqInfo, 'select varvalue from user_vars where userid='+inttostr(iUserid)+' and varid="'+lowercase(sName)+'"'));

    if sValue = '' then
      result := false;
  except
    result := false;
  end;
end;

function LoadUserVar(rqInfo: TRequestInfo; sName: ansistring; sDefault: ansistring ;iUserID: integer=0): ansistring;
begin
  result := LoadUserVar(rQInfo, sName, iUserID);
  if result = '' then
    result :=  sDefault;
end;
function LoadUserVar(rqInfo: TRequestInfo; sName: ansistring; iUserID: integer=0): ansistring;
var
  vv: variant;
begin
  if rqInfo.sessionid = 0 then begin
    result := '';
    exit;
  end;

  if iUserID = 0 then
    iUserID := QuickSession(rqInfo)['userid'].AsVariant;
  try
    vv := DecodeWebString(LazyQueryFn(rqInfo, 'select varvalue from user_vars where userid='+inttostr(iUserid)+' and varid="'+lowercase(sName)+'"'));

    result := vv;

  except
    on E: ESocketsDisabled do begin
      raise;
    end;
    on E: exception do begin
      result := '';
    end;
  end;
end;



end.
