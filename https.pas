unit https;

interface

uses MSXML2_TLB, sysutils, variants, typex, commandprocessor, classes, debug, IdSSLOpenSSL, systemx, IdSSLOpenSSLHeaders;



type
  THTTPResults = record
    ResultCode: ni;
    Body: string;
    Success: boolean;
    error: string;
  end;

  THttpsMethod = (mGet, mPost);
  Tcmd_HTTPS = class(TCommand)
  private

  public
    addHead: string;
    addHeadValue: string;
    method: ThttpsMethod;
    url: string;
    PostData: string;
    Results: THTTPResults;
    ContentType: string;
    procedure DoExecute; override;
    procedure Init; override;
  end;

//function QuickHTTPSGet(sURL: ansistring): ansistring;
function QuickHTTPSGet(sURL: ansistring; out sOutREsponse: string; addHead: string =''; addHeadValue: string = ''): boolean;
function QuickHTTPSPost(sURL: ansistring; PostData: ansistring; ContentType: ansistring = 'application/x-www-form-urlencoded'): ansistring;

function HTTPSGet(sURL: string; referer: string = ''): THTTPResults;
function HTTPSPost(sURL: string; PostData: string; ContentType: string = 'application/x-www-form-urlencoded'): THTTPResults;




implementation

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


function QuickHTTPSPost(sURL: ansistring; PostData: ansistring; ContentType: ansistring = 'application/x-www-form-urlencoded'): ansistring;
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
begin
  inherited;
  if method = mGet then begin
    try
      results.success := QuickHTTPSGet(self.url, results.body, addhead, addHeadValue);
    except
      results.success := false;
    end;
  end else
  if method = mPost then begin
    results := HTTPSPost(url, postdata, contenttype);
  end;

end;

procedure Tcmd_HTTPS.Init;
begin
  inherited;
  ContentType := 'application/x-www-form-urlencoded';
end;

initialization


if fileexists(dllpath+'ssleay32.dll') then begin
  IdOpenSSLSetLibPath(DLLPath);
  IdSSLOpenSSL.LoadOpenSSLLibrary;
end;



end.
