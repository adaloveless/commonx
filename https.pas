unit https;

interface
{$I DelphiDefs.inc}
{$IFDEF MSWINDOWS}
uses MSXML2_TLB, sysutils, variants, typex, commandprocessor, classes, debug, IdSSLOpenSSL, systemx, IdSSLOpenSSLHeaders, betterobject, helpers.stream;


const
  DEFAULT_CONTENT_TYPE = 'application/x-www-form-urlencoded';
type
  TExtraHeader = record
    name: string;
    value: string;
    class function make(n,v: string): TExtraHeader;static;
  end;
  THttpsMethod = (mGet, mPost);
  THTTPSRequest = record
    addHead: string;
    addHeadValue: string;
    method: ThttpsMethod;
    acceptranges: string;
    contentlength: int64;
    range: string;
    url: string;
    PostData: string;
    Referer: string;
    ContentType: string;
    PostBody: string;
  end;

  THTTPResults = record
    ResultCode: ni;
    Body: string;
    Success: boolean;
    contentType: string;
    contentRange: string;
    error: string;
    bodystream: IHolder<TStream>;
  end;


  Tcmd_HTTPS = class(TCommand)
  private

  public
    Request: THTTPSRequest;
    Results: THTTPResults;
    procedure DoExecute; override;
    procedure Init; override;
  end;

//function QuickHTTPSGet(sURL: ansistring): ansistring;
function QuickHTTPSGet(sURL: ansistring; out sOutREsponse: string; addHead: string =''; addHeadValue: string = ''): boolean;overload;
function QuickHTTPSGet(sURL: ansistring; out sOutREsponse: string; addHeaders: TArray<TExtraHeader>): boolean;overload;
function QuickHTTPSPost(sURL: ansistring; sPostBody: ansistring; out sOutREsponse: string; contentType: string = DEFAULT_CONTENT_TYPE; addHead: string =''; addHeadValue: string = ''; method: string = 'POST'): boolean;
function QuickHTTPSPostOld(sURL: ansistring; PostData: ansistring; ContentType: ansistring = 'application/x-www-form-urlencoded'): ansistring;

function HTTPSGet(sURL: string; referer: string = ''): THTTPResults;
function HTTPSPost(sURL: string; PostData: string; ContentType: string = 'application/x-www-form-urlencoded'): THTTPResults;

procedure https_SetHeaderIfSet(htp: IXMLHttpRequest; sheader: string; sValue: string);


{$ENDIF}
implementation
{$IFDEF MSWINDOWS}

function QuickHTTPSPost(sURL: ansistring; sPostBody: ansistring; out sOutREsponse: string; contentType: string = DEFAULT_CONTENT_TYPE; addHead: string =''; addHeadValue: string = ''; method: string = 'POST'): boolean;
var
  htp: IXMLhttprequest;
begin
{$IFDEF LOG_HTTP}
  Debug.Log(sURL);
{$ENDIF}

  htp := ComsXMLHTTP30.create();
  try
    htp.open(method, sURL, false, null, null);
    htp.setRequestHeader('Content-Type', ContentType);
    if addHead <> '' then
      htp.setRequestHeader(addHead, addHeadValue);
    htp.send(sPostBody);

    result := htp.status = 200;
    if result then
      sOutREsponse := htp.responsetext
    else begin
      soutResponse := htp.responsetext;
    end;
  except
    on e: Exception do begin
      result := false;
      sOutResponse := 'error '+e.message;
    end;
  end;

end;


function QuickHTTPSGet(sURL: ansistring; out sOutREsponse: string; addHeaders: TArray<TExtraHeader>): boolean;
var
  t: ni;
  htp: IXMLhttprequest;
begin
{$IFDEF LOG_HTTP}
  Debug.Log(sURL);
{$ENDIF}

  htp := ComsXMLHTTP30.create();
  try
    htp.open('GET', sURL, false, null, null);
    for t := 0 to high(addHeaders) do begin
      htp.setRequestHeader(addHeaders[t].name, addHeaders[t].value);
    end;
    htp.send('');
    result := htp.status = 200;
    if result then
      sOutREsponse := htp.responsetext
    else begin
      soutResponse := htp.responsetext;
    end;
  except
    on e: Exception do begin
      result := false;
      sOutResponse := 'error '+e.message;
    end;
  end;

end;

function QuickHTTPSGet(sURL: ansistring; out sOutREsponse: string; addHead: string =''; addHeadValue: string = ''): boolean;
var
  htp: IXMLhttprequest;
begin
{$IFDEF LOG_HTTP}
  Debug.Log(sURL);
{$ENDIF}

  htp := ComsXMLHTTP30.create();
  try
    htp.open('GET', sURL, false, null, null);
    if addHead <> '' then
      htp.setRequestHeader(addHead, addHeadValue);
    htp.send('');
    result := htp.status = 200;
    if result then
      sOutREsponse := htp.responsetext
    else begin
      soutResponse := htp.responsetext;
    end;
  except
    on e: Exception do begin
      result := false;
      sOutResponse := 'error '+e.message;
    end;
  end;

end;


function QuickHTTPSPostOld(sURL: ansistring; PostData: ansistring; ContentType: ansistring = 'application/x-www-form-urlencoded'): ansistring;
var
  htp: IXMLhttprequest;
begin
//  raise Exception.create('carry forward');
  htp := ComsXMLHTTP30.create();
  try
    htp.open('POST', sURL, false,null,null);
    htp.setRequestHeader('Accept-Language', 'en');
    //htp.setRequestHeader('Connection:', 'Keep-Alive');
    htp.setRequestHeader('Content-Type', ContentType);
    htp.setRequestHeader('Content-Length', inttostr(length(PostData)));
    htp.send(PostData);
    result := htp.responsetext;
  finally
//    htp.free;
  end;

end;

function HTTPSGet(sURL: string; referer: string = ''): THTTPResults;
var
  htp: IXMLhttprequest;
begin
//  raise Exception.create('carry forward');
  result.error := '';
  htp := ComsXMLHTTP30.create();
  try
    try
      htp.open('GET', sURL, false, null, null);
      htp.setRequestHeader('Referer', referer);
      htp.send('');
      result.ResultCode := htp.status;
      result.Body := htp.responseText;
      result.contentType := htp.getResponseHeader('Content-type');
      result.Success := true;
    except
      on e: Exception do begin
        result.success := false;
        result.error := e.message;
      end;
    end;
  finally
//    htp.free;
  end;
end;
function HTTPSPost(sURL: string; PostData: string; ContentType: string = 'application/x-www-form-urlencoded'): THTTPResults;
var
  htp: IXMLhttprequest;
begin
//  raise Exception.create('carry forward');
  result.error := '';
  htp := ComsXMLHTTP30.create();
  try
    htp.open('POST', sURL, false,null,null);
    htp.setRequestHeader('Accept-Language', 'en');
    htp.setRequestHeader('Connection:', 'Keep-Alive');
    htp.setRequestHeader('Content-Type', ContentType);
    htp.setRequestHeader('Content-Length', inttostr(length(PostData)));
    htp.send(PostData);
    result.ResultCode := htp.status;
    result.Body := htp.responsetext;

  finally
//    htp.free;
  end;

end;



{ Tcmd_HTTPS }

procedure Tcmd_HTTPS.DoExecute;
var
  htp: IXMLhttprequest;
  sMeth: string;
begin
  inherited;
  if request.method = mGet then begin
    try
    {$IFDEF LOG_HTTP}
      Debug.Log(sURL);
    {$ENDIF}
      htp := ComsXMLHTTP30.create();
      try
        if self.request.method = THttpsMethod.mPost then
          sMeth := 'POST'
        else
          sMeth := 'GET';


        htp.open(sMeth, self.request.url, false, null, null);

        https_setheaderifset(htp, 'Content-Length', inttostr(request.contentlength));
        https_setheaderifset(htp, 'Content-Type', request.contenttype);
        https_setheaderifset(htp, 'Accept-Ranges', request.acceptranges);
        https_setheaderifset(htp, 'Range', request.range);
        https_setheaderifset(htp, 'Referer', request.referer);


        if request.addHead <> '' then
          htp.setRequestHeader(self.request.addHead, self.request.addHeadValue);
        if self.request.method = mGet then
          htp.send('')
        else
          htp.send(request.postBody);

        results.ResultCode := htp.status;
        results.contentRange := htp.getResponseHeader('Content-Range');
        results.contentType := htp.getResponseHeader('Content-Type');
        results.bodystream := THolder<TStream>.create;
        self.Results.bodystream.o := olevarianttomemoryStream(htp.responsebody);

//        results.Body := htp.responsetext;
      except
        on e: Exception do begin
          ErrorMessage := e.message;
          results.Body := 'error '+e.message;
        end;
      end;
    finally
    end;
  end else
  if request.method = mPost then begin
    results := HTTPSPost(request.url, request.PostData, request.contenttype);
  end;

end;

procedure Tcmd_HTTPS.Init;
begin
  inherited;
  request.ContentType := 'application/x-www-form-urlencoded';

end;


procedure https_SetHeaderIfSet(htp: IXMLHttpRequest; sheader: string; sValue: string);
begin
  if sValue = '' then
    exit;
  htp.setRequestHeader(sHeader, sValue);
end;

{ THTTPSRequest }



{ TExtraHeader }

class function TExtraHeader.make(n, v: string): TExtraHeader;
begin
  result.name := n;
  result.value := v;
end;

initialization


if fileexists(dllpath+'ssleay32.dll') then begin
  IdOpenSSLSetLibPath(DLLPath);
  IdSSLOpenSSL.LoadOpenSSLLibrary;
end;


{$ENDIF}
end.
