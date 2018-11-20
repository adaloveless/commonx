unit WebResource;
//A placeholder currently.  This unit will implement the ability for loading
//HTML files and graphics from a compiled resource.  Currently, however, the
//resource is just the files on-disk
interface

uses
  classes, requestInfo, sysutils, typex, stringx;

procedure LoadWebResource(rqInfo: TRequestInfo; sResourceName: string; sl: TStringList); overload;
procedure LoadWebResource(rqInfo: TRequestInfo; sl: TStringList;sResourceName: string); overload;
procedure LoadWebResource(rqInfo: TRequestInfo; out s: string; sResourceName: string); overload;
procedure LoadWebResourceSnippet(rqInfo: TRequestInfo; out s: string; sResourceName: string; sSnippet: string); overload;
procedure LoadWebResource(rqInfo: TRequestInfo; sResourceName: string); overload;
function GetWebResource(rqInfo: TRequestInfo; sResourceName: string): string;
procedure LoadWebResourceAndMergeWithBestTemplate(rqInfo:TRequestInfo; sResourceName: string; sDefaultTemplate: string = '');

procedure ProcessResource(rQInfo: TRequestInfo; slLoadedResource: TStringList; sSnippet: string = '');

function ChangeDocumentExtensionToFileExtension(rqInfo: TrequestInfo; sfile: string): string;

function GetResourceFile(rqInfo: TrequestInfo; sResource: string): string;
implementation

uses
  WebFunctions, WebConfig;


//------------------------------------------------------------------------------
procedure LoadWebResource(rqInfo: TRequestInfo; sResourceName: string; sl: TStringList);overload;
//Loads a text file stored in the Digital Tundra WLN resource.  Currently implemented
//as a directory on disk, but could be something else.
begin
  sl.Clear;
  sl.LoadFromFile(GetResourceFile(rqInfo, sResourceName));
  ProcessResource(rqInfo, sl);
end;
//------------------------------------------------------------------------------
function GetWebResource(rqInfo: TRequestInfo; sResourceName: string): string;
var
  sl: TStringList;
begin
  sl := nil;
  try
    sl := TStringList.create;
    sl.LoadFromFile(GetResourceFile(rqInfo, sResourceName));
    ProcessResource(rqInfo, sl);
    result := sl.text;
  finally
    sl.free;
  end;
end;
procedure LoadWebResource(rqInfo: TRequestInfo; sResourceName: string);
//Loads a text file stored in the Digital Tundra WLN resource.  Currently implemented
//as a directory on disk, but could be something else.
var
  sl: TStringList;
begin
  sl := nil;
  try
    sl := TStringList.create;
    sl.LoadFromFile(GetResourceFile(rqInfo, sResourceName));
    ProcessResource(rqInfo, sl);
    rqInfo.response.content.text := sl.text;

  finally
    sl.free;
  end;
end;
//------------------------------------------------------------------------------
procedure LoadWebResource(rqinfo: TRequestInfo; out s: string; sResourceName: string); overload;
var
  sl : TStringList;
begin
  sl := TStringList.create;
  try
    LoadWebResource(rqInfo,sResourceName, sl);
    s := sl.text;
  finally
    sl.free;
  end;
end;
//------------------------------------------------------------------------------
procedure LoadWebResourceAndMergeWithBestTemplate(rqInfo:TRequestInfo; sResourceName: string; sDefaultTemplate: string = ''); overload;
var
  slTemplate: TStringlist;
  slBody: TStringList;
  t,u: integer;
  s, s1, s2, sName: string;
  iPrecut, iPostcut: integer;
  sTemplateToLoad: string;
begin

  slBody := TStringList.create;
  try
    slTemplate := TStringlist.create;
    try
      //load the web resource
      LoadWebResource(rqInfo, slBody, sResourceName);


      //process attributes of resource <![p] tags and such... this may change the template variable
      ProcessResource(rqInfo, slBody);
      rqInfo.SetupDefaultVarPool;

      //now that we know what template to load lets load it or choose the default
      if rqInfo.request.HasParamWithValue('template') then
        sTemplateToLoad := 'template_'+rqInfo.Request.Params['template']+'.html'
      else begin
        if sDefaultTemplate <> '' then
          sTemplateToLoad := 'template_'+sDefaultTemplate+'.html'
        else
          sTemplateToLoad := 'template.html';
      end;

      if comparetext(sTemplateToLoad, 'template_none.html') =0 then begin
        rqInfo.Response.Content.text := slBody.Text;
      end else begin
        LoadWebResource(rqInfo, slTemplate, sTemplateToLoad);
        rqInfo.Response.content.Text := slTemplate.Text;
        //Body and shell are merged together
        MergeAtToken(rqInfo.Response.Content, 'body', slBody);

      end;






    finally
      slTemplate.free;
    end;
  finally
    slBody.free;
  end;
end;


function GetResourceFile(rqInfo: TrequestInfo; sResource: string): string;
var
  t: integer;
  sSkin: string;
  s: string;
  sSkinFile: string;
  sURLPath: string;
begin
  if zcopy(sResource,0,1) <> '/' then
    sURLPath := extractfilepath(stringReplace(rqInfo.request.document,'/','\', [rfReplaceAll]))
  else
    sURLPath := '';
  sResource := stringReplace(sResource,'/','\', [rfReplaceAll]);
  result := WebServerConfig.ExternalResourcePath+sURLPath+sResource;

  //defines for palms
(*  if pos('PPC',rqInfo.request['User-Agent']) > 0 then begin
    rqinfo.request.addparam('_dirSKIN', '_palm', pcHeader);
    sSkin := '_palm';
  end;
  if pos('AvantGO',rqInfo.request['User-Agent']) > 0 then begin
    rqinfo.request.addparam('_dirSKIN', '_palm', pcHeader);
(*    sSkin := '_palm';
  end;*)


(*  for t:= 0 to rqInfo.Request.ParamPatternCount['_dir']-1 do begin
    sSkin := rqInfo.request.ParamsbyPatternMatch['_dir', t];
    if copy(sSkin, 1,1) <> '_' then
      continue;


    s := WebServerConfig.ExternalResourcePath+sSkin+'\'+sResource;
    if FileExists(s) then begin
      //rqInfo.request.document := stringReplace(rqInfo.request.document, '/'+sSkin+'/','/', [rfIgnoreCase]);
      result := stringReplace(s, '/'+sSkin+'/','/', [rfIgnoreCase]);
      break;
    end;
  end;*)


  sSkin := '';

  if rQInfo.request.hasparam('skin') then
    sSkin := rqInfo.request['skin']+'\';

  sSkinFile := WebServerConfig.ExternalResourcePath+sSkin+sResource;
  if fileexists(sSkinFile) then
    result := sSkinFile;





end;
procedure LoadWebResource(rqInfo: TRequestInfo; sl: TStringList; sResourceName: string); overload;
begin
  loadWebResource(rqInfo, sResourceName, sl);
end;


procedure ProcessResource(rQInfo: TRequestInfo; slLoadedResource: TStringList; sSnippet: string = '');
var
  slBody: tStringList;
  t,u: integer;
  s, s1,s2: string;
  iPrecut: integer;
  sName: string;
  iSnipTop, iSnipBottom: integer;
  iSnipLen: integer;
  sTemp: string;
begin
  iPrecut := -1;

  slbody := slLoadedResource;

  slBody.text := stringReplace(slBody.text, #01, '[[[', [rfReplaceAll]);

//  if slBody.text <> sTemp then
//    slBody.text := sTemp;

  if sSnippet <> '' then
    sSnippet := '<!['+sSnippet+']';
  iSnipLen := length(sSnippet);
  iSnipTop := -1;
  try
    for t:= 0 to slBody.count-1 do begin
      if (copy(slBody[t], 1, 10) = '<![precut]') and (sSnippet = '') then begin
        iPrecut := t;
      end;
      if (sSnippet<> '') and (lowercase(copy(slBody[t], 1, iSnipLen)) = sSnippet) and (iSnipTop = -1) then begin
        iSnipTop := t;
        continue;
      end;
      if copy(slBody[t], 1, 5) = '<![p]' then begin
        s := slBody[t];
        SplitString(s, '[p]', s1,s2);
        SplitString(s2, '=', sName, s2);
        SplitString(s2, '"', s1, s2);
        SplitString(s2, '"', s1, s2);
        rqInfo.request.addParam(sName, s1, pcHeader);
      end;
      if (copy(slBody[t], 1, 11) = '<![postcut]') and (sSnippet = '')  then begin
        for u := slbody.count-1 downto t do begin
          slBody.delete(u);
        end;
        break;
      end;
      if (sSnippet<> '') and (lowercase(copy(slBody[t], 1, iSnipLen)) = sSnippet) and (iSnipTop > -1) then begin
        iSnipBottom := t;
        for u := slbody.count-1 downto 0 do begin
          if (u <=iSnipTop) or (u >= iSnipBottom) then
            slBody.delete(u);
        end;
        break;
      end;
    end;

    for u := iPrecut downto 0 do begin
      slBody.delete(u);
    end;
    if (sSnippet <> '') and (iSnipTop= -1) then
      raise exception.create('snippet '+sSnippet+' was not found');
  finally
  end;
end;

procedure LoadWebResourceSnippet(rqInfo: TRequestInfo; out s: string; sResourceName: string; sSnippet: string); overload;
var
  sl: TStringlist;
begin
  sl := TStringlist.create;
  try
    sl.LoadFromFile(GetResourcefile(rqInfo, sResourceName));

    ProcessResource(rqInfo,  sl, sSnippet);
    s := sl.text;

  finally
    sl.free;
  end;

end;

function ChangeDocumentExtensionToFileExtension(rqInfo: TrequestInfo; sfile: string): string;
var
  sExt: string;
begin
  sExt := ExtractFileExt(sFile);
  sExt := lowercase(sExt);
  if sExt = '.wml' then begin
    result := ChangeFileExt(sFile, '.wml')
  end else begin
    result := ChangeFileExt(sFile, '.html');
  end;
end;
end.
