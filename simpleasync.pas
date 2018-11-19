unit simpleasync;

interface

uses
  managedthread, mothershipwebserver, requestinfo, windows, backgroundthreads, systemx;

type
  TAsyncPage = class(TProcessorThread)
  private
    FURL: string;
    rqInfo: tRequestInfo;
    function GetURL: string;
    procedure SetURL(const Value: string);
  protected
    procedure DoExecute;override;
  public
    property URL: string read GetURL write SetURL;



  end;

var
  active_async_pages: integer;

procedure Asyncdispatch(sPage: string);


implementation


uses WebDispatch, ThreadManager, stringx, stringx.ansi, webstring;

{ TAsyncPage }

procedure TAsyncPage.DoExecute;
var
  rqInfo: Trequestinfo;
  sLeft, sright, sJunk, sName, sValue, sDoc, sPage: string;
begin
  inherited;

  rqInfo := TrequestInfo.create;
  try

    sPage := URL;
    splitString(sPage, '?', sDoc, sRight);

    rqInfo.Request.Document := sDoc;
    while splitstring(sright, '&', sLeft, sRight) do begin
      SplitString(sLeft, '=', sName, sValue);
      rQInfo.request.AddParam(sName, DecodeWebstring(sValue), pcHeader);
    end;

    SplitString(sLeft, '=', sName, sValue);
    rQInfo.request.AddParam(sName, DecodeWebString(sValue), pcHeader);

    rqInfo.request.addparam('host', '', pcHeader);

    while active_async_pages > 3 do begin
      sleep(1);
      if terminated then exit;
    end;

    interlockedincrement(active_async_pages);

    try
      try
        webdispatch.DispatchWebRequest(rqInfo);
      except
      end;
    finally
      interlockeddecrement(active_async_pages);
    end;

    self.Terminate;

  finally
    rqInfo.free;
  end;

end;

function TAsyncPage.GetURL: string;
begin
  Lock;
  try
    result := FUrl;
  finally
    Unlock;
  end;

end;

procedure TAsyncPage.SetURL(const Value: string);
begin
  Lock;
  try
    FURL := Value;
  finally
    Unlock;
  end;

end;


procedure Asyncdispatch(sPage: string);
var
  ap: TAsyncPage;
  sDoc, sname, sValue, sJunk, sLeft,sRight: string;
begin
  ap := tpm.needthread<TAsyncPage>(nil);
  try

    ap.URL := sPage;




    ap.Start;





    NotImplemented;//needs fire forget fix
  finally
//    ap.free;
  end;


end;

end.
