unit WebThread;

interface

uses
  Classes,ScktComp, RequestInfo, ErrorHandler, WebSTring, dialogs, SharedObject, mothershipwebserver, WebFunctions, stringx, stringx.ansi, windows{$ifdef DELPHI7},SOCKETS{$ENDIF},orderlyinit;

type
  TRequestState = (rqsInit, rqsRegister, rqsExecute, rqsClientExecute, rqsInheritedExecute, rqsReadHeader, rqsReadBody, rqsDispatch, rqsScript, rqsWriteHeader, rqsWriteBody, rqsClosed);

  //TWebThread = class(TFakeLockQueuedObject)
  TWebThread = class(TServerClientThread)
  private
    { Private declarations }
    rqInfo: TrequestInfo;
    FRequestState: TRequestState;
    sect: _RTL_CRITICAL_SECTION;

    function GetStateString: widestring;
    procedure SetRequestState(const Value: TRequestState);
    function GetRequestState: TRequestState;
  protected
    procedure Execute; override;
    procedure ClientExecute; override;
    procedure ReadRequest;
    procedure ReadAvailable(bHeaderComplete:boolean);
    procedure WriteResponse;
    procedure WriteHeader;
    procedure WriteContent;
    procedure WriteChunk;
    procedure WriteResponseData(s: string);
  public
    property RequestState: TRequestState read GetRequestState write SetRequestState;
    property StateString: widestring read GetStateString;
    constructor Create(CreateSuspended: Boolean; ASocket: TServerClientWinSocket); reintroduce; virtual;
    destructor Destroy; override;
    function TryLock: boolean;
    procedure Lock;
    procedure Unlock;
  end;

  TWebThreadMan = class(TFakeLockQueuedObject)
  private
    function GetThreadCount: integer;
    function GetThreads(idx: integer): TWebThread;
  protected
    FThreads: TList;
  public
    constructor create; override;
    destructor destroy; override;
    property threads[idx: integer]: TWebThread read GetThreads;
    property ThreadCount: integer read GetThreadCount;
    procedure RegisterThread(thread: TWebThread);
    procedure DeRegisterThread(thread: TWebThread);
    procedure WaitforThreads;
    procedure KillAllThreads;

  end;

var
  WebThreadMan: TWebThreadMan;

procedure QuickLog(s: string);
implementation

uses
  WebDispatch, Sysutils;
//------------------------------------------------------------------------------
procedure TWebThread.ClientExecute;
//This is the main thread execution code.
var
  tm1, tm2: cardinal;
begin
  RequestState := rqsClientExecute;
//  FreeOnTerminate := true;
//  WebThreadMan.RegisterThread(self);
  try
    try
      try

        //Check to see if the Server is stopped, if so, exit immediately.
        if not (WebServer.State = wssRunning) then
          Terminate;

        if Terminated then
          exit;

        //Read the stuff
        ReadRequest;

        //More termination checking
        if Terminated then
          exit;

        //Dispatch the request
        RequestState := rqsDispatch;
        WebServer.DispatchWebRequest(rqInfo);

        //More termination Checking
        if Terminated then
          exit;

      except
        //If expception was not handled by the more-elegant exception handlers
        //underneath, then generate a simple plain-text exception message.
        rqInfo.response.resultcode := 500;
        rqInfo.response.Content.text := '<HTML>'+Exception(ExceptObject).Message+'</HTML>';;
      end;

      //Write the response
      tm1 := GetTickCount;

      //Set a content length for the response if not explicitly set... based on the length of the response content.
      if rqInfo.Response.ContentLength = -1 then
        rqInfo.Response.ContentLength := length(rqInfo.response.content.text);

      //Write the response down the socket.
      WriteResponse;

      //Report this thread as closed
      RequestState := rqsClosed;

      tm2 := GetTickCount;
      rqInfo.response.SendTime := tm2-tm1;

      //Call hook if used, (not used in produciton)
      if assigned(rqInfo.response.OnResponseSent) then
        rqInfo.response.OnResponseSent(rqInfo);



    //Log exceptions if not handled elegantly
    except
      rqInfo.response.ResultCode := 500;
      LogError('WebThread Execute ', 'Exception:' +Exception(ExceptObject).Message, 500);
    end;
  finally

    {$IFDEF DELAY}
    //Delay for 5 seconds (to slow
//    if not Terminated then
      sleep(5000);
    {$ENDIF}
    self.Terminate;
//    WebThreadMan.Deregisterthread(self);
  end;
end;
//------------------------------------------------------------------------------
procedure TWebThread.ReadAvailable(bHeaderComplete:boolean);
var
  s: string;
//Reads bytes available on the socket.
begin

  s := ClientSocket.ReceiveText;

  //if the header has already been received, read into the content
  //else read into the "raw" buffer.
  if bHeaderComplete then
    rqInfo.Request.Content := rqInfo.Request.Content + s
  else
    rqInfo.Request.Raw := rqInfo.Request.Raw + s;
end;
//------------------------------------------------------------------------------
procedure TWebThread.ReadRequest;
var
  tm2, tm1: cardinal;
  iFirstSpace: integer;
  iTemp: integer;
  sTemp: string;
  t: integer;
  sLeft, sRight : string;
  sLeft2, sRight2: string;
  sInlineParams: string;
  sFirstParam: string;
  sTempContent: string;
  bDoneReading: boolean;
  iContentLength : integer;
  iLastReceiveLength: integer;
  bHeaderComplete: boolean;
  sl: TStringList;
const
  THREAD_READ_TIMEOUT = 30000;

begin
  RequestState := rqsReadHeader;
  { Place thread code here }
  bHeaderComplete := false;
  sTempContent := '';
  iContentLength := 0;

  //Init start timer
  tm1 := GetTickCount;
//  tmBench1 := GetTickCount;
  //Read... and be DEAD SURE... you've got it all


  repeat
    try
      //Check now many bytes are in the pipe
      iLastReceiveLength:=ClientSocket.ReceiveLength;
      //If bytes are in the pipe, read them
      IF iLastReceiveLength > 0 THEN BEGIN
        ReadAvailable(bHeaderComplete);

        //Reset timeout timer (client sender is still active)
        tm1 := GetTickCount;
      END ELSE
        //Free up processor time if client is a slow sender
        Sleep(random(10));

    except
      on E: Exception do begin
        raise Exception.create('Exception reading header: '+E.Message);
        self.rqInfo.response.ResultCode := 400;
        self.rqInfo.Request.Document := 'erroneous';
        exit;
      end;
    end;

    tm2 := GetTickCount;

    //check timer rollover (happens only once every 18 days)
    //if the new time is less than the old timer then just set the old time
    //to the new time (rolled-over timeouts will take a little longer with this approach)
    if tm2<tm1 then
      tm1 := tm2;

    //Set status to reading body
    if bHeaderComplete then RequestState := rqsReadBody;

    //If header has not been received then search of end of header and get the content length from the header
    if not bHeaderComplete then begin
      //if found end of header....
      iTemp := pos(#13#10#13#10, rqInfo.Request.Raw);
      if iTemp<>0 then begin
        //split the request at the delimiter into the request[header] and requestcontent
        sTemp := rqInfo.Request.Raw;
        rqInfo.Request.Content := copy(sTemp,  1, iTemp+3);
        //separate out and content bytes
        rqInfo.Request.Content := copy(sTemp, iTemp+4, length(sTemp) - iTemp+3);
        bheaderComplete := true;
        iContentLength := HackContentLength(rqInfo.Request.Raw);
      end;
    end;

    //determine that request is COMPLETE if header flag has been set indicating
    //that the header has been read AND the length of the RequestContent is
    //equal-to or greater than the content length specified in the header

    bDoneReading := bHeaderComplete and (length(rqInfo.Request.Content)>=iContentLength);

    //Thread-hang prevention
    //if haven't recieved data in THREAD_READ_TIMEOUT seconds, then kill the thread
    if (not bDoneReading) and ((tm2-tm1) > THREAD_READ_TIMEOUT) then
      raise Exception.create('Thread read timeout');
  until
    bDoneReading;

    //This triggers the "raw" stringlist to split the request into multiple lines for easier parsing.
    rqInfo.request.raw := rqInfo.request.raw;

  try
    //if nothing was recieved then exit
    if length(rqInfo.Request.Raw)<1 then
      exit;

    sTemp := rqInfo.Request.Raw;

    //Get the 'GET/POST' part of the request

    iFirstSpace := pos(' ', sTemp);
    rqInfo.Request.Command := copy(sTemp, 1, iFirstSpace-1);


    //Remove the GET/POST part form the request line so that we can continue parsing
    sTemp := copy(sTemp, iFirstSpace+1, length(sTemp) - iFirstSpace);

    //Get the DOCUMENT part of the request
    iFirstSpace := pos(#10, sTemp);
    sTemp := copy(sTemp, 1, iFirstSpace-1);
    iFirstSpace := lastpos(' ', sTemp);
    rqInfo.Request.Document := copy(sTemp, 1, iFirstSpace-1);

    rqInfo.Request.command := uppercase(rqInfo.Request.Command);
    rqInfo.Request.Document := rqInfo.Request.Document;

    //showmessage(rqInfo.Request.text);

    //Get HEADER PARAMETERS
    //Params are passed as the 2nd through 2nd-to-last lines in the header
    t:= 0;
    sl := TStringList.create;
    try
      sl.text := rqInfo.request.raw;
      repeat
        rqInfo.request.Header.add(sl[t]);
        inc(t);

      until
        (sl[t] = '') or (t>=sl.count);
    finally
      sl.free;
    end;

    for t:= 1 to rqInfo.Request.Header.count-1 do begin
      //Split the line based on the first occurrence of a colon (:)
      sTemp := rqInfo.Request.Header[t];

      SplitString(sTemp, ':', sLeft, sRight);

      sRight := Trim(sRight);

      if (sLeft <> '') then begin
      //Left side is the name
      //Right side is param value
      rqInfo.Request.AddParam(Trim(sLeft), Trim(sRight), pcHeader);//<--Don't URL Decode the header parameters
      end;
    end;


    //GET COOKIE PARAMETERS
    if (not rqInfo.request.HasParam('content-type') or (not (rqInfo.request['content-type'] = 'application/x-Digital Tundra'))) then
    if rqInfo.request.HasParam('cookie')  then begin
      sInlineParams := rqInfo.Request['cookie'];
      repeat
        //segregate the first parameter definition from the inline parameters
        SplitString(sInlineParams, ';', sFirstParam, sInlineParams);
        //Separate the parameter from the parameter value
        SplitString(sFirstParam, '=', sLeft, sRight);

        while pos('+', sRight) > 0 do begin
          SplitString(sRight, '+', sLeft2, sRight2);
          sRight := sLeft2+' '+sRight2;
        end;

        if sLeft <> '' then begin
          //Left side is the name
          //Right side is param value
          rqInfo.Request.AddParam(DecodeWebString(Trim(sLeft)), DecodeWebString(Trim(sRight)), pcCookie);
        end;
      until (sInlineParams = '');
    end;

    //INLINE PARAMETERS
    //-------------------
    //Get User-defined parameters from document name
    //split document name based on whatever comes first ? or #
    if (  (pos('?', rqInfo.Request.Document) < pos('#', rqInfo.Request.Document)  )
        or (pos('#', rqInfo.Request.Document) = 0)) then
      SplitString(rqInfo.Request.Document, '?', sLeft, sRight)
    else
      SplitString(rqInfo.Request.Document, '#', sLeft, sRight);
    //ReAssign docuement name to just the part BEFORE the "?"
    rqInfo.Request.Document := sLeft;

    sInlineParams := sRight;
    if sRight<>'' then begin
      repeat

        //segregate the first parameter definition from the inline parameters
        SplitString(sInlineParams, '&', sFirstParam, sInlineParams);
        //Separate the parameter from the parameter value
        SplitString(sFirstParam, '=', sLeft, sRight);


        if sLeft <> '' then begin
          //Left side is the name
          //Right side is param value
          rqInfo.Request.AddParam(DecodeWebString(Trim(sLeft)), DecodeWebString(Trim(sRight)), pcInline);
        end;
      until (sInlineParams = '');
    end;

    //If a multipart message
    if NOT IsMultiPart(rqInfo) then begin
      //GET CONTENT PARAMETERS
      if length(rqInfo.Request.Content) > 0 then begin
        sInlineParams := rqInfo.Request.Content;
        repeat

          //segregate the first parameter definition from the inline parameters
          SplitString(sInlineParams, '&', sFirstParam, sInlineParams);
          //Separate the parameter from the parameter value
          SplitString(sFirstParam, '=', sLeft, sRight);

  (*        while pos('+', sRight) > 0 do begin
            SplitString(sRight, '+', sLeft2, sRight2);
            sRight := sLeft2+' '+sRight2;
          end;*)

          if sLeft <> '' then begin
            //Left side is the name
            //Right side is param value
            rqInfo.Request.AddParam(DecodeWebString(Trim(sLeft)), DecodeWebString(Trim(sRight)), pcContent);
          end;
        until (sInlineParams = '');
      end;
    end;

(*    EchoParametersAsComments(rqInfo);
    showmessage(rqInfo.response.content.text);*)

    //super session hook -- used to aid Load-testing scripts
    if (rqInfo.request.HasParam('supersession')) and (rqInfo.request.HasParam('sessionid')) then begin
      if rqInfo.sessionid = 0 then begin
        rqInfo.request.AddParam('sessionid', rqInfo.request['supersession'], pcCookie);
      end;
    end;

    rqInfo.request.document := rqInfo.request.document;

  finally

  end;
end;
//------------------------------------------------------------------------------
procedure TWebThread.WriteHeader;
var
  s: string;
  t: integer;
begin
  RequestState := rqsWriteHeader;

  //Generate some default messages for certain return codes if no result message
  //was specified
  if rqInfo.Response.Location <> '' then begin
    rqInfo.Response.ResultCode := 302;
    rqInfo.Response.Message := 'Moved';
  end;

  if rqInfo.Response.ResultCode = 200 then
//    rqInfo.response.message := 'OK'
  else
  if rqInfo.Response.ResultCode = 500 then
//    rqINfo.response.message := ;




  //Allocate a chunk of memory to reduce memory fragmentation
  SetLength(s, 1000);
  //calc first line of header
  s:='HTTP/1.1 '+inttoStr(rqInfo.Response.Resultcode)+' '+rqInfo.response.message+#13#10;


  //if content encoding is not chunked
  if not (lowercase(rqInfo.Response.TransferEncoding)='chunked') then begin
    //Set the content length -- length of string if never set.. else override with real value
    if rqInfo.Response.ContentLength = -1 then
      s:= s+'Content-Length: '+inttostr(length(rqInfo.Response.Content.Text))+#13#10
    else
      s:= s+'Content-Length: '+inttostr(rqInfo.Response.ContentLength)+#13#10;
  end;

  //Transfer encoding header result
  if rqInfo.Response.TransferEncoding <> '' then
    s:=s+'Transfer-Encoding: '+rqInfo.Response.TransferEncoding+#13#10;


  //More header params
  s:=s+'Server: Mothership Server'+#13#10;
  if rqInfo.Response.Location <>'' then
    s:=s+'Location: '+rqInfo.Response.Location+#13#10;
  s:=s+'Content-Type: '+rqInfo.Response.ContentType+#13#10;
  s:=s+'Last-Modified: '+FormatDateTime('ddd'','' d-mmm-yyyy hh:nn:ss', now)+' GMT'+#13#10;
//  s:=s+'Last-Modified: Wed, 25 Jul 2001 17:26:20 GMT'+#13#10;
//  s:=s+'ETag: "b0a29beb2e15c11:b5d"'+#13#10;
//  s:=s+'Accept-Ranges: bytes'+#13#10;
//  s:=s+'Pragma: no-cache'+#13#10;
//  s:=s+'Expires: Tuesday, 14-Dec-1971 04:30:00 GMT'+#13#10;

  //Cookie header params
  if rqInfo.response.CookieCount > 0 then begin
    for t:= 0 to rqInfo.response.cookiecount-1 do begin
      s:=s+'Set-Cookie: '+rqInfo.response.cookienames[t]+'='+rqInfo.response.cookievalues[t]+';expires=Monday, 10-Aug-2099 00:00:00 GMT;path=/'+#13#10;
    end;
  end;

  s:=s+'Connection: close'+#13#10;
  s:=s+'Content:'+#13#10+#13#10;


  //Send response down pipe
  self.WriteResponseData(s);

  rqInfo.request.raw := rqInfo.request.raw;

end;
//------------------------------------------------------------------------------
procedure TWebThread.WriteResponse;
begin
  if not rqInfo.response.HeaderSent then
    WriteHeader;

  if not (rqInfo.response.TransferEncoding = 'chunked') then
    WriteContent;

  ClientSocket.close;

end;
//------------------------------------------------------------------------------
procedure TWebThread.WriteResponseData(s: string);
const
  BUF_SIZE = 2;
begin
  if Terminated then
    exit;

  try
(*//  buf := StrAlloc(BUF_SIZE);
    for t := 0 to rqInfo.Response.ContentLength-1 div BUF_SIZE do begin
      //setup base address
      iBase := t*BUF_SIZE;

      //setup length to write
      iWriteLength := (rqInfo.response.contentlength -1)- iBase;
      if iWriteLength>BUF_SIZE then
        iWriteLength := BUF_SIZE;

      for u:= 1 to iWritelength do begin
        //write the bytes to the temporary buffer.
        buf[u] := s[iBase+u];
      end;

      iAddress := @buf[1];
      self.ClientSocket.SendBuf(iAddress^, iWriteLength);
    end;*)

    self.ClientSocket.SendText(s);

  except
    LogError('Send:', 'Exception: '+ exception(exceptobject).Message, 500);
  end;

end;
//------------------------------------------------------------------------------
procedure TWebThread.WriteContent;
var
  stream : TStream;
begin
  RequestState := rqsWriteBody;

  //Send stream if assigned
  if rqInfo.response.ContentStream <> nil then begin
    rqInfo.response.contentstream.Seek(0,0);

    stream := rqInfo.response.contentstream;
    //Set stream to nil to make sure memory isn't trashed on rqInfo.free.
    //Stream WILL BE FREED if still assigned (in case an internal failure prevents the stream from being sent).
    rqInfo.response.contentStream := nil;

    //**NOTE!! IMPORTANT.... stream becomes OWNED by ClientSocket on SendStream
    //... this means DON'T FREE IT!!
    self.ClientSocket.SendStream(stream);
    //stream := nil;

  //Otherwise send as text
  end else begin
    self.WriteResponseData(copy(rqInfo.Response.Content.text, 1, rqInfo.response.contentlength));
  end;
end;

//------------------------------------------------------------------------------
destructor TWebThread.Destroy;
begin
  self.RequestState := rqsClosed;
  WebThreadMan.DeRegisterThread(self);
  rqInfo.Free;
  DeleteCriticalSection(sect);
  if handle<>0 then
    inherited;
end;
//------------------------------------------------------------------------------
constructor TWebThread.Create(CreateSuspended: Boolean;
  ASocket: TServerClientWinSocket);
begin
  inherited Create(True, TServerCLientWinSocket(ASocket));

  //InitializeResources
  (*if self.handle = 0 then
    exit;*)

  if self.handle<>0 then begin
    InitializeCRiticalSection(sect);
    rqInfo := nil;
    rqInfo := TrequestInfo.create;

    //Hooks for chunked transfer sending on-demand
    rqInfo.response.DoSendHeader := WriteHeader;
    rqInfo.response.DoSendChunk := WriteChunk;
    self.RequestState := rqsREgister;

    priority := tpNormal;

    WebThreadMan.RegisterThread(self);
    if not CreateSuspended then
      resume;
  end;

  //Resume;
end;

procedure TWebThread.WriteChunk;
begin
  self.WriteResponseData(rqInfo.Response.Content.text);
end;

procedure QuickLog(s: string);
var
  f: textfile;
begin
  try
    assignfile(f, 'd:\headers.txt');
    append(f);
    writeln(f, s);
  except
    closefile(f);
  end;

end;
procedure TWebThread.Lock;
begin
  //self.Suspended := true;!<<<<-------WTF
  EnterCriticalSection(sect);
end;

procedure TWebThread.Unlock;
begin
  LeaveCriticalSection(sect);
  //self.Suspended := false; !<<<<-------WTF
end;

{ TWebThreadMan }

constructor TWebThreadMan.create;
begin
  inherited;
  FThreads := TList.create;


end;

procedure TWebThreadMan.DeRegisterThread(thread: TWebThread);
begin
  LockWrite;
  try
    FThreads.remove(thread);
  finally
    UnLockWrite;
  end;
end;


destructor TWebThreadMan.destroy;
begin


  FThreads.free;
  inherited;

end;

function TWebThreadMan.GetThreadCount: integer;
begin
  LockRead;
  try
    result := FThreads.count;
  finally
    UnlockRead;
  end;
end;

function TWebThreadMan.GetThreads(idx: integer): TWebThread;
begin
  LockRead;
  try
    result := FThreads[idx];
  finally
    UnlockRead;
  end;
end;

procedure TWebThreadMan.KillAllThreads;
var
  t: integer;
begin
  self.LockWrite;
  try
    for t:= 0 to self.threadcount-1 do begin
      threads[t].FreeonTerminate := true;
      threads[t].Terminate;
    end;
  finally
    self.UnlockWrite;
  end;

  WaitForThreads;

end;

procedure TWebThreadMan.RegisterThread(thread: TWebThread);
begin
  LockWrite;
  try
    FThreads.add(thread);
  finally
    UnLockWrite;
  end;

end;

function TWebThread.GetStateString: widestring;
begin
  if TryLock then
  try
    case FRequestState of
      rqsInit: result := 'Initializing....';
      rqsExecute: result := 'Executing....';
      rqsInheritedExecute: result := 'Executing Inherited....';
      rqsClientExecute: result := 'Client Execute....';
      rqsRegister: result := 'Registering....';
      rqsReadHeader: result := 'Reading Header '+self.rqInfo.request.document;
      rqsReadBody: result := 'Reading Body '+self.rqInfo.request.document;
      rqsDispatch: result := 'Dispatching '+self.rqInfo.request.document;
      rqsScript: result := 'Processing Scripts '+self.rqInfo.request.document;
      rqsWriteHeader: result := 'Writing Header '+self.rqInfo.request.document;
      rqsWriteBody: result := 'Writing Body '+self.rqInfo.request.document;
      rqsClosed: result := 'Complete '+self.rqInfo.request.document;
//      rqsClosed: result := inttostr(self.rqInfo.response.resultcode)+' '+self.rqInfo.request.document+' '+self.rqInfo.response.Message;
    else
      result := 'unknown';
    end;
  finally
    UnLock;
  end else result := 'Locked';
end;


function TWebThread.TryLock: boolean;
begin
  result := TryEnterCriticalSection(sect);
end;

procedure TWebThreadMan.WaitforThreads;
begin
  while self.ThreadCount > 0 do
    sleep(100);

end;

procedure TWebThread.SetRequestState(const Value: TRequestState);
begin
  Lock;
  try
    FREquestState := value;
  finally
    unlock;
  end;
end;

function TWebThread.GetRequestState: TRequestState;
begin
  Lock;
  try
    result := FRequestState;
  finally
    unlock;
  end;
end;

procedure TWebThread.Execute;
begin
  RequestState := rqsInheritedExecute;
  inherited;
  //ClientExecute;
  {$IFDEF DELPHI7}
  ClientExecute;
  {$ENDIF}

  RequestState := rqsExecute;
end;

procedure oinit;
begin
  WebThreadMan := TWebThreadMan.create;
end;

procedure ofinal;
begin
  WebThreadMan.free;

end;

initialization
  init.RegisterProcs('WebThread', oinit, ofinal,'ManagedThread');



finalization


end.
