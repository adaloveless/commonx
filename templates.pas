unit Templates;

interface

uses
  WebConfig, RequestInfo, sysutils, requestdispatcher, stringx.ansi;

procedure LoadTemplate(rqInfo: TRequestInfo; sTitle: ansistring; sMenuItem: ansistring; sTemplate: ansistring = ''; sSubHeading: ansistring = '');
procedure SendJavaRedirect(rqInfo: TRequestInfo; sURL: ansistring; sMessage: ansistring = '');
function BuildWhereClause(rqInfo: TrequestInfo; bExtended: boolean = false): ansistring;
procedure WRQ_LazyDispatch(rqInfo: TrequestInfo);

implementation

uses WebResource, stringx, AsyncClasses;

procedure LoadTemplate(rqInfo: TRequestInfo; sTitle: ansistring; sMenuItem: ansistring; sTemplate: ansistring = ''; sSubHeading: ansistring = '');
begin
  if rqInfo.response.hasvar('template') then begin
    sTemplate := rQInfo.response.varpool['template'];

    if rqInfo.response.varpool['template']='sales' then
      sTemplate := 's';
  end ;



  if (sSubHeading = '') and rqInfo.request.hasParam('sub_heading') then
    sSubHeading := rqInfo.request['sub_heading'];

  if sTemplate <> '' then
    sTemplate := '_'+sTemplate;

  if rqInfo.request.hasparam('page_subtitle') then begin
    sSubHeading := rqInfo.request['page_subtitle'];
  end;

  LoadWebResource(rqInfo, 'template'+sTemplate+'.html');
//  rqINfo.response.Content.LoadFromFile(WebServerConfig.ExternalResourcePath+'template'+sTemplate+'.html');
  rqINfo.response.VarPool['page_title'] := sTitle;
  rqInfo.response.VArPool['page_menu'] := sMenuItem;
  rqInfo.Response.varpool['page_subtitle'] := sSubHeading;

end;

procedure SendJavaRedirect(rqInfo: TRequestInfo; sURL: ansistring; sMessage: ansistring = '');
begin
  rqInfo.response.varpool['url'] := sURL;
  rqInfo.response.varpool['message'] := sMessage;
  rqInfo.response.varpool['altmessage'] := sMessage;


  LoadwebResource(rqInfo, 'java_redirect.html');


end;

//------------------------------------------------------------------------------
function BuildWhereClause(rqInfo: TrequestInfo; bExtended: boolean = false): ansistring;
var
  t,u: integer;
  sLeft, sRight: ansistring;
  sValue: ansistring;
begin
  result := '';
  u := 0;
  for t:=0 to rqInfo.request.paramPatternCount['where.']-1 do begin
    if result = '' then
      SplitString(rqInfo.request.paramNamesByPatternMatch['where.', t], '.', sLeft, sRight);
      sValue := rqInfo.request.ParamsByPatternMatch['where.', t];
      if sValue <> '' then begin
        if (u>0) or bExtended then
          result := result + 'AND '
        else
          result := ' WHERE ';

        result := result+sRight+'="'+sValue+'" ';

        inc(u);
      end;
  end;

  for t:=0 to rqInfo.request.paramPatternCount['likewhere.']-1 do begin
    if result = '' then
      SplitString(rqInfo.request.paramNamesByPatternMatch['likewhere.', t], '.', sLeft, sRight);
      sValue := rqInfo.request.ParamsByPatternMatch['likewhere.', t];
      if sValue <> '' then begin
        if (u>0) or bExtended then
          result := result + ' AND '
        else
          result := ' WHERE ';

        result := result+sRight+' like "%'+sValue+'%" ';

        inc(u);
      end;
  end;


end;


procedure WRQ_LazyDispatchSync(rqInfo: TRequestInfo);
var
  sFile: ansistring;
begin
    sFile := changefileext(copy(rqInfo.request.document, 2, length(rqInfo.request.document)), '.html');
    LoadWebResourceAndMergeWithBestTemplate(rqInfo, sFile, '');

end;

procedure WRQ_LazyDispatch(rqInfo: TrequestInfo);
begin
  if rQInfo.request.hasparam('async') then begin
    AsyncAdapt(rqInfo, WRQ_LazyDispatchSync);
  end else begin
    WRQ_LazyDispatchSync(rqInfo);
  end;
end;




end.
