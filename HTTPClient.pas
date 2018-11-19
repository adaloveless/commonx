unit HTTPClient;

interface
{$DEFINE DISABLE_AUTOLOG}
uses
  tickcount, stringx.ansi, systemx, WideStrUtils, SimpleWinsock, sysutils, winapi.windows, classes, stringx, debug, sharedobject, exe, beeper, webstring,orderlyinit, typex;
//const
 // rar = 'G:\Archives\util\gzip-1.3.5-3-bin\bin\gunzip.exe';
type
//------------------------------------------------------------------------------
  EHTTPError = class(exception);
  THTTPProgressEvent = procedure (pos, max: int64) of object;
  TRawHttp = class; //forward declaration
//------------------------------------------------------------------------------
  TRawHttp = class
  private
    FOnProgress: THTTPProgressEvent;
    FLastEndpoint: HTTPstring;
    FLastHost: HTTPstring;
    procedure SetOutHeader(const Value: HTTPString);
    //This is the base class for acting as an HTTP client.  3rd party
    //solutions were not designed to be friendly in a multi-threaded
    //environment.

    //This class performs HTTP transaction WITHOUT cares for things
    //such as redirection, or parsing the variables the come back.
    //It merely sends a pre-defined HTTP header and receives the response.
    //It DOES NOT, however BUILD HTTP headers or interpret them any more than
    //it needs to to ensure the transaction is receieved properly.
    //You can't just call "GET('http://www.someplace.com'); and have it
    //download the webpage for you.  See the following THTTPClient class for that.
    //this one just handles transports.

    //Some important thing about transports are implemented that you wouldn't
    //think about until you start having problems with them.  This includes
    //the ability to properly download responses that have "100 Continue" result
    //codes, and the ability to handle HTTP bodies that use "chunked transfer
    //encoding"-- denoted when the "Transfer-Encoding" header parameter
    //is set to "chunked".
  protected
    FOutbody: HTTPstring;
    FOutHeader: HTTPstring;
    FInHeader: HTTPstring;
    FInBody: HTTPstring;

    FSizeLimit: integer;
    FtimeOut: cardinal;
    FAutoLog: boolean;
    Fconnections: TStringList;
    FKeepConnectionAlive: boolean;
    procedure SetRequest(const Value: HTTPString);
    procedure SetResponse(const Value: HTTPString);
    function GetRequest: HTTPstring;
    function GetResponse: HTTPstring;
    procedure CheckDownload(bWaitForDataResult: boolean; bDataWasReturned: boolean; var tm1, tm2: cardinal);
    procedure LogTransaction;
  public
    constructor Create; reintroduce;
    destructor Destroy;override;

    property OutHeader:  HTTPString read FOutHeader write SetOutHeader;
    //The header sent/to-be-sent to the server.
    property Outbody: HTTPString read FOutbody write FOutbody;
    //The body sent/to-be-sent to the server.
    property Request: HTTPString read GetRequest write SetRequest;
    //Combination of OutHeader+OutBody.
    property InHeader: HTTPString read FInHeader write FInHeader;
    //The header received from the server.
    property InBody: HTTPString read FInBody write FInBody;
    //The body Received from the server.
    property Response: HTTPString read GetResponse write SetResponse;
    //Combination of InHeader+InBody.
    property SizeLimit: integer read FSizeLimit write FSizeLimit;
    //If > 0 stops the HTTP download once the limit is reached.
    property TimeOut: cardinal read FtimeOut write FTimeout;
    //Time to wait for download before timing out.

    function Transact(bHeadOnly:boolean= false): boolean;overload;
    function Transact(sAltHost: HTTPstring; iAltPort: integer; bHeadOnly:boolean= false): boolean;overload;
    function Getconnection(sHost: HTTPstring; sEndPoint: HTTPstring): TSimpleWinsockConnection;
    procedure Closeconnection(sHost: HTTPstring; sPort: HTTPstring);overload;
    procedure CloseConnection(conn: TSimpleWinsockConnection);overload;

    property KeepConnectionAlive:boolean read FKeepConnectionAlive write FKeepConnectionAlive;

    procedure SaveToFile(sFile: string);
    procedure SaveToStream(s: TStream);
    procedure RaisetimeoutError;
    property AutoLog: boolean read FAutoLog write FAutoLog;
    procedure OnDeCompress;virtual;
    property OnProgress: THTTPProgressEvent read FOnProgress write FOnProgress;
    property LastHost: HTTPstring read FLastHost;
    property LastEndpoint: HTTPstring read FLastEndpoint;
  end;
//------------------------------------------------------------------------------
  THTTPClient = class(TRawHTTP)
  private
    function GetCookieHosts(idx: integer): HTTPstring;

    //This class allows you to make simple "GET" and "POST" calls without
    //care for how the actually HTTP header is formatter up.  It will also
    //automatically handle redirects.  It will handle more and parse
    //more properties as needed. Very few properties are
    //defined as of yet.  They are being defined as-needed, and this class
    //has not seen much use yet.
    //SPECIAL NOTE: Most of the functionality of this class is in its
    //base class TRawHTTP;
  protected
    FUserAgent: HTTPstring;
    FAutoChunk: boolean;
    FAutoRedirect: boolean;
    FMode10: boolean;
    FOutContentTYpe: HTTPstring;
    FHostCache: HTTPstring;
    FCookies: TStringList;
    FCookieHosts: TStringList;
    FLastURLRequested: HTTPstring;
    FSoundOnCookie: boolean;

    FLogNumber: integer;
    FDownloadLinks: boolean;
    FDontSendCookies: boolean;
    FSendFullURL: boolean;
    FETags: TStringlist;
    FDates: TStringlist;
    FAuthorization: HTTPstring;


    function GetCookie(sName: HTTPstring): HTTPstring;
    procedure TrackDate(sURL, sDate: HTTPstring);
    function GoOpera(bPost: boolean; sURL:HTTPstring; sReferer: HTTPstring = ''; iSize: integer =-1): boolean;
    function GoIE(bPost: boolean; sURL:HTTPstring; sReferer: HTTPstring = ''; iSize: integer =-1): boolean;
    function Go(bPost: boolean; sURL:HTTPstring; sReferer: HTTPstring = ''; iSize: integer =-1): boolean;
    function Get_ResultCode: HTTPstring;
    function GetCookieCount: integer;
    function GetCookies(idx: integer): HTTPstring;
    function GetEnableGZIP: boolean;

  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    //transactions
    function Get(sURL: HTTPstring; sReferer: HTTPstring = ''): boolean;
    function Head(sURL: HTTPstring; sReferer: HTTPstring = ''): boolean;
    function Post(sURL:HTTPstring; sReferer: HTTPstring = ''; iSize: integer =-1): boolean;
    function Put(sURL: HTTPstring; iLength: integer = -1; sReferer: HTTPstring = ''): boolean;

    procedure AddPostParam(sName, sValue: HTTPstring; bNoEncode:boolean= false);
    property ResultCode: HTTPstring read Get_ResultCode;
    //The error/status code returned from the server after the last transaction.
    //e.g. 200, 404, 500....
    property UserAgent: HTTPstring read FUserAgent write FUserAgent;
    //The user agent to use when sending the request.  Can be used to mimic other browsers.
    //Reads a cookie that came out of the request, for testing purposes, not very robust.
    property AutoChunk: boolean read FAutoChunk write FAutoChunk;
    //If true, chunked responses will be automatically decoded when a transaction occurs.
    property AutoRedirect: boolean read FAutoRedirect write FAutoRedirect;
    //If true, 302 (Object moved) responses will be automatically forwarded to the
    //given forwarding address without intervention.
    property Mode10: boolean read FMode10 write FMode10;
    //If true, sends the HTTP request as HTTP 1.0.
    property OutContentType: HTTPstring read FOutContentTYpe write FOutContentType;
    //Content type to send the Body as. e.g 'application/x-www-form-urlencoded', 'text/plain', 'text/html'...
    property HostCache: HTTPstring read FHostCache write FHostCache;
    //Caches the host name in the class, because sometimes have found that hostname lookups fail unexplicably.


    procedure AddCookie(sNAme, sValue, sHost: HTTPstring);overload;
    procedure AddCookie(sNameAndValueAndHost:HTTPstring);overload;
    property Cookies[idx: integer]: HTTPstring read GetCookies;
    property CookieHosts[idx: integer]: HTTPstring read GetCookieHosts;
    property Cookie[sName: HTTPstring]: HTTPstring read GetCookie;

    procedure ClearCookies;
    function ShouldSendCookie(sHost: HTTPstring; idx: integer): boolean;
    property CookieCount: integer read GetCookieCount;
    property LastURLRequested: HTTPstring read FLastURLRequested write FLastURLRequested;
    property SoundOnCookie: boolean read FSoundOnCookie write FSoundOnCookie;
    property Lognumber: integer read FLogNumber write FLogNumber;
    property DownloadLinks: boolean read FDownloadLinks write FDownloadLinks;
    procedure Extract;


    function GUnzipFile: HTTPstring;
    property EnableGZIP: boolean read GetEnableGZIP;
    procedure OnDeCompress;override;
    property DontSendCookies: boolean read FDontSendCookies write FDontSendCookies;
    property SendFullURL: boolean read FSendFullURL write FSendFullURL;

    procedure TrackETag(sURL: HTTPstring; sTag: HTTPstring);
    function GetETag(sURL: HTTPstring): HTTPstring;
    function GetDate(sURL: HTTPstring): HTTPstring;
    property Authorization: HTTPstring read FAuthorization write FAuthorization;


  end;

  THTTPStatus = class(TSharedObject)
  private
    FRetrying, FReceive, FTransmit, FTransactions: integer;
    function GetInRetrying: integer;
    function GetInReceive: integer;
    function GetInTransaction: integer;
    function GetInTransmit: integer;
  public
    property InTransaction: integer read GetInTransaction;
    property InTransmit: integer read GetInTransmit;
    property InReceive: integer read GetInReceive;
    property Retrying: integer read GetInRetrying;

    procedure Transmit(inc: integer);
    procedure Receive(inc: integer);
    procedure Transaction(inc: integer);
    procedure Retry(inc: integer);

  end;

function TimeZoneBias : HTTPstring;
function Rfc822DateTime(t : TDateTime) : HTTPstring;

procedure DownloadLinks(htp: THTTPClient; sBaseURL, sBody, sTag, sAttribute: HTTPstring);
procedure DownloadAllLinks(htp: THTTPClient; sBaseURL, sBody: HTTPstring);
function HTTPStrSize(s: HTTPString): integer;

procedure DebugStuff(sBufferString: string);inline;

var
  httpstatus: THTTPStatus;
  poop: string;


function QuickHTTPGet(sURL: HTTPstring): HTTPstring;

implementation


{ THTTPClient }

procedure DebugStuff(sBufferString: string);
begin
  exit;
  if sBufferSTring = poop then begin
    Debug.Log('nil',nil, 'Received DUP! '+inttostr(length(sBufferString))+' starting with ['+copy(sBufferString, 1,5)+'] ending with ['+copy(sBufferString, length(sbufferstring)-4, 5)+']');
  end;
  poop := sBufferSTring;

  Debug.Log('nil',nil, 'Received '+inttostr(length(sBufferString))+' starting with ['+copy(sBufferString, 1,5)+'] ending with ['+copy(sBufferString, length(sbufferstring)-4, 5)+']');

end;

function HTTPStrSize(s: HTTPString): integer;
begin
  result := length(s)*SizeOf(HTTPChar);
end;

//------------------------------------------------------------------------------
procedure TRawHttp.Closeconnection(sHost: HTTPstring; sPort: HTTPstring);
var
  t: integer;
  conn: TSimplewinsockconnection;
begin
  //check if we have a connection
  t := fConnections.IndexOf(shost+':'+sPort);

  if t>=0 then begin
    conn := TSimpleWinsockConnection(Fconnections.objects[t]);
    Debug.Log(self,'Freeing connection to '+shost+':'+sPort +'@'+inttohex(cardinal(pointer(conn)), sizeof(cardinal)), 'forwarder');
    FConnections.delete(t);
    conn.Disconnect;
    conn.Free;
    conn := nil;
  end;



end;

procedure TRawHttp.Closeconnection(conn: TSimpleWinsockConnection);
var
  t: integer;
begin
  //check if we have a connection
  t := fConnections.IndexOfObject(conn);

  if t>=0 then begin
    FConnections.delete(t);
    conn.Disconnect;
    conn.Free;
  end;



end;

constructor TRawHttp.Create;
begin
  inherited Create;
  FOutbody:= '';
  FOutHeader:= '';
  FInHeader:= '';
  FInBody:= '';
  FSizeLimit := 0;
  FTimeOut := 500000;
  Fconnections := TStringlist.create;


end;

destructor TRawHttp.destroy;
var
  conn: TSimpleWinsockConnection;
begin
  while Fconnections.count > 0 do begin
    conn := FConnections.objects[0] as TSimpleWinsockConnection;
    conn.free;
    FConnections.delete(0);
  end;


  FConnections.free;
  inherited;
end;

//------------------------------------------------------------------------------
function THttpCLient.GetEnableGZIP: boolean;
begin
  result := false;
//  result := fileexists(GUnzipFile);

end;

function THTTPClient.GetETag(sURL: HTTPstring): HTTPstring;
var
  sLeft, sright: string;
  t: integer;
  astr: string;
begin
  if not assigned(FETags) then FETags := TStringlist.create;

  result := '';
  astr := '=';
  for t:= FETags.count-1 downto 0 do begin
    SplitString(FETags[t], astr, sLeft, sRight);
    if sright = sURL then begin
      result := sLEft;
      exit;
    end;
  end;


end;

function TRawHttp.Getconnection(sHost: HTTPstring; sEndPoint: HTTPstring): TSimpleWinsockConnection;
var
  t: integer;
begin
  result := nil;
  //check if we have a connection
  t := fConnections.IndexOf(shost+':'+sEndPoint);

  //if we have connection use it
  if t>=0 then begin
    result := TSimpleWinsockConnection(FConnections.objects[t]);
    //if the connection was recently disconnected (there shouldn't be any data on the pipe, and if it closes, WFD returns true)
    if result.WaitForData(1) then begin
      result.Disconnect;
      FConnections.delete(t);
      result.free;
      result := nil;
    end;

  end;
  //else create the connection
  if result = nil then begin
    result := TSimpleWinsockConnection.create;
    FConnections.addObject(shost+':'+sEndPoint, result);
    result.HostName := sHost;
    result.EndPoint := sEndPoint;
//    if not result.Connected then
//      result.connect;
  end;




end;

function TRawHttp.GetRequest: HTTPstring;
begin
  result := OutHeader+OutBody;
end;
//------------------------------------------------------------------------------
function TRawHttp.GetResponse: HTTPstring;
begin
  result := InHeader+InBody;
end;
procedure TRawHttp.RaisetimeoutError;
begin
  raise EHTTPError.create('HTTP Download Timed Out');
end;

//------------------------------------------------------------------------------
procedure TRawHttp.SetOutHeader(const Value: HTTPString);
var
  s: HTTPstring;
begin
  FOutHeader := Value;

  s := WebString.GetHeaderParam(FOutHeader, 'Connection');
  Debug.Log(self,'Forward Keep-Alive setting = '+s, 'Forwarder');

  self.KeepConnectionAlive := lowercase(s) = 'keep-alive';

  if self.KeepConnectionAlive then begin
    WebString.ChangeHeaderParam(FOutHeader, 'Connection', 'close');
    self.KeepConnectionAlive := false;
  end;

end;

procedure TRawHttp.SetRequest(const Value: HTTPString);
//Setter for the request property
begin
  OutHeader := ExtractHeader(Value);
  OutBody := ExtractContent(Value);
end;
//------------------------------------------------------------------------------
procedure TRawHttp.SetResponse(const Value: HTTPString);
//Setter for the response property.
begin
  InHeader := ExtractHeader(Value);
  InBody := ExtractContent(Value);

end;
//------------------------------------------------------------------------------
function TRawHttp.Transact(bHeadOnly: boolean = false): boolean;
var
  sHostName: HTTPstring;
  iPort: integer;
  iTemp: integer;
begin
    //parse the http header to determine which HOST and port to connect to
    sHostName := GetHeaderParam(OutHeader, 'host');
    if sHostName = '' then
      raise exception.Create('No Host Specified');
    //set the port default to 80 //Todo:note https handling needs to be implemented
    iPort := 80;
    //determine if an alternate port is specified
    iTemp := pos(':', sHostName);

    //if alternate port then...
    if iTemp > 0 then begin
      //assign the endpoint to everything after the ':'
      try
        iPort := strtoint(copy(sHostName, iTemp+1, length(sHostName)));
      except
        on E: Exception do begin
          raise Exception.create('Cannot set port: '+e.Message);
        end;
      end;
      //assign the hostname to everything BEFORE the ':'
      sHostName:= copy(sHostName, 1, iTemp-1);
    end;

    result := Transact(sHostName, iPort, bHeadOnly);


end;
function TRawHttp.Transact(sAltHost: HTTPstring; iAltPort: integer; bHeadOnly: boolean = false): boolean;
//Function sends out the Request, composed of OutHeader+OutBody and returns
//reply from server in InHeader, InBody.... response.
//Sorry it's enormously complex... blame the "innovators" of the HTTP protocol
var
  conn: TSimpleWinsockConnection;
  iLength, iChunkLength: integer;
  iContentLength: integer;
  sBuffer: PByte;
  sBufferString: HTTPstring;
  sResponse: HTTPstring;
  sContent: HTTPstring;
  sTemp: HTTPstring;
  bFirstTime : boolean;
  sChunk: HTTPstring;
  sRemainder: HTTPstring;
  ssss: HTTPstring;
  pctemp: PHTTPChar;
  pbtemp: PByte;
  tmStart, tmEnd: cardinal; //timer values for managing download timeout condition
const
  BUFFER_SIZE = 10000;
begin
  iLength := 0;
  httpstatus.Transaction(1);
  try

    //Intialize Timeout Conditional Variables
    tmStart := GetTicker;
    tmEnd := GetTicker;

    //Create the connection object (same one used for Data-Tier comms
    conn := GetConnection(sAlthost, inttostr(iAltPort));
    Debug.Log(self,'Got Connection '+inttohex(integer(pointer(conn)),8),'Forwarder');

    //if this is a KEEP-Alive connection and the host/endpoint is different than before
    //the disconnect from host.

    if (FLastHost <> '')
    and ((FLastHost <> sAltHost)
        or (FLastEndpoint <> inttostr(iAltPort)))
    then begin

      if conn.connected then
        conn.disconnect;
    end;

    FLastHost := sAltHost;
    FLastEndpoint:= inttostr(iAltPort);

    conn.TimeOut := self.timeout;

    sbuffer := nil;
    //Allocate a buffers for passing around download contents
    GetMem(sBuffer, BUFFER_SIZE+1);
    SetLength(sBufferString, BUFFER_SIZE);
    try

      //Connect
      if conn.Connected then
        Debug.Log(self,'Conn is connected already '+inttohex(integer(pointer(conn)),8), 'Forwarder')
      else
        Debug.Log(self,'Conn is virgin '+inttohex(integer(pointer(conn)),8), 'Forwarder');

      if conn.connected or conn.Connect then begin
        //Send the request
        httpstatus.Transmit(1);
        try
          ssss := OutHeader;
          pctemp := PHTTPChar(ssss);
          pbtemp := PBYTE(pctemp);
          conn.SendData(PByte(PHTTPchar(ssss)), HTTPStrSize(ssss));
          if outbody <> '' then begin
            //sleep(1000);
            ssss := OutBody;
            conn.SendData(PByte(PHTTPchar(ssss)), HTTPStrSize(ssss));
          end;
        finally
          httpstatus.Transmit(-1);
        end;
        Debug.Log(self,'Data sent: '+inttohex(integer(pbyte(conn)),8), 'Forwarder');

        //Read until header complete
        sResponse := '';

        bFirstTime := true;
        //keep looping through this code while the result code is
        //"100 Continue" -- a 100 result code indicates that the header is BS and
        // should be thrown away... allegedly, a new header should be following
        while (GetResultCode(sResponse) = '100') or bFirstTime do begin
          if not bFirsttime then begin
            //if NOT the first time through this code, then we need to put
            //what we originally thought was the CONTENT of the HTTP request so-far
            //into the response header... as the header was invalid
            sResponse := sContent;
          end;

          //Set firsttime flag to false.
          bFirstTime := false;

          //IsHeader complete is in WebString.pas.. and scans the
          //recieved buffer for two consecutive CRLFs... which is
          //how you determine whether the header is complete or not
          while not IsHeaderComplete(sResponse) do begin
            if conn = nil then
              raise ECritical.create('connection dropped');
            if not conn.WaitForData(ftimeout) then
              RaiseTimeoutError;

            httpstatus.Receive(1);
            try
              iLength := conn.ReadData(sBuffer, BUFFER_SIZE);
            finally
              httpstatus.Receive(-1);
            end;

            CheckDownload(true, iLength>0, tmStart, tmEnd);
            if iLength> 0 then begin
              sBufferString := '';
              setlength(sBufferString, iLength);
              movemem32(@sBufferString[1], sBuffer, iLength);
              DebugStuff(sBufferString);
              sResponse := sResponse + sBufferstring;
            end;
          end;

          if bHeadonly then begin
            result := true;
            sResponse := ExtractHeader(sResponse);
            self.InHeader := sResponse;
            exit;
          end;


          //Attempt to determine the content length -- if NOT 100 Continue
          sTemp := GetResultCode(sResponse);
          if sTemp='100' then
            iContentLength := 0
          else begin
            sTemp := GetHeaderParam(sResponse, 'Content-Length');
            if sTemp<>'' then
              iContentLength := strtoint(sTemp)
            else begin
//              if uppercase(copy(sResponse, 1, 8)) = 'HTTP/1.0' then begin
                if pos('content:', lowercase(sResponse)) > 0 then begin
                  iContentLength := -1
                end else
                  icontentLength := -1;
//              end else
//                icontentLength := 0;
            end;
          end;


          //Separate out the Response Header from the Content of the response
          sContent := ExtractContent(sResponse);
          sResponse := ExtractHeader(sResponse);

          //if a NORMAL response then there shold be a contentlength that
          //determines how much data is to follow
          if iContentLength > 0 then begin
            //Read the content -- until content length is reached
            CheckDownload(true,true, tmstart, tmEnd);//<--- this resets the timer
            while (length(sContent)< iContentLength) do begin
              if conn.WaitForData(1) then begin
                httpstatus.Receive(1);
                try
                  iLength := conn.ReadData(sBuffer, BUFFER_SIZE);
                finally
                  httpstatus.Receive(-1);
                end;
                CheckDownload(true, iLength>0, tmStart, tmEnd);
                if iLength> 0 then begin
                  sBufferString := '';
                  setlength(sBufferString, iLength);
                  movemem32(@sBufferstring[1], sBuffer, iLength);
                  DebugStuff(sBufferString);
                  sContent := sContent + sBufferstring;
                end;
                if assigned(OnProgress) then
                  OnProgress(length(sContent), iContentLength);

                //size limit... for download testing (not for practical use)
                if (length(sContent) >= FSizeLimit) and (FSizeLimit>0) then
                  break;
              end else
                CheckDownload(false, false, tmStart, tmEnd);
            end;
          end
          //Otherwise if NO CONTENT LENGTH is specified, then the response
          //could either be using "Chunked" encoding, or it could be an HTTP/1.0
          //response with an unknown length... in which case we need to wait for
          //the server to request the socket to be closed.
          else begin
            //If NOT chunked-- download until connection closes.
            IF NOT (lowercase(GetHeaderParam(sResponse, 'Transfer-Encoding')) = 'chunked') then BEGIN

              //IF no content length specified then wait for connection to close
              if iContentLength = -1 then begin
                CheckDownload(true, true, tmstart, tmEnd);//<--- this resets the timer
                while conn.IsConnected do begin
                  if conn.WaitForData(1) then begin

                    httpstatus.Receive(1);
                    try

                      iLength := conn.ReadData(sBuffer, BUFFER_SIZE);
                      CheckDownload(true, iLength>0, tmStart, tmEnd);

                    finally
                      httpstatus.Receive(-1);
                    end;


                    if iLength> 0 then begin
                      sBufferString := '';
                      setlength(sBufferString, iLength);
                      movemem32(@sBufferString[1], sBuffer, iLength);

                      DebugStuff(sBufferString);
                      sContent := sContent + sBufferstring;
                    end;
                  end else
                    CheckDownload(false, false, tmStart, tmEnd);

                  //size limit... for download testing (not for practical use)
                  if (length(sContent) >= FSizeLimit) and (FSizeLimit>0) then
                    break;

                end;
                result := true;
              //if CONTENT LENGTH is 0 then there is not body and we're done
              end else begin
                result := true;
              end;
            END
            //******CHUNKED ENCODING********
            ELSE BEGIN
              //Assign the current content to chunk variable for processing
              sChunk := sContent;
              //null out the content (its not the real content... thankyou)
              sContent := '';

              while (2+2=4) do begin //this infinite loop is exited with BREAK

                //download until chunk complete (look for CRLF)
                while not IsChunkComplete(sChunk) do begin
                  if conn.WaitForData(1) then begin
                    httpstatus.Receive(1);
                    try
                      iLength := conn.ReadData(sBuffer, BUFFER_SIZE);
                      CheckDownload(true, iLength>0, tmStart, tmEnd);
                    finally
                      httpstatus.Receive(-1);
                    end;

                    if iLength> 0 then begin
                      sBufferString := '';
                      setlength(sBufferString, iLength);
                      movemem32(@sBufferString[1], sBuffer, iLength);
                      DebugStuff(sBufferString);
                      sChunk := sChunk + sBufferstring;
                    end;
                  end;
                end;

                //once chunk is complete...
                //Get the chunk size (note that it is a hexadecimal string)
                iChunkLength := GetChunkLength(sChunk);

                //if the chunksize is 0 then break because we've reached the FOOTER
                //the download algorythm following this BREAK line does not
                //apply to footers (thankyou)
                if iChunkLength = 0 then
                  break;

                //keep downloading until the we have enough data to cover the
                //chunk size
                while (length(sChunk)< iChunkLength) do begin
                  if conn.WaitForData(1) then begin
                    httpstatus.Receive(1);
                    try
                      iLength := conn.ReadData(sBuffer, BUFFER_SIZE);
                      CheckDownload(true, iLength>0, tmStart, tmEnd);
                    finally
                      httpstatus.Receive(-1);
                    end;


                    if iLength> 0 then begin
                      sBufferString := '';
                      setlength(sBufferString, iLength);
                      movemem32(@sBufferstring[1], sBuffer, iLength);
                      DebugStuff(sBufferString);
                      sChunk := sChunk + sBufferstring;
                    end;
                  end;
                end;
                //crack the chunk
                DecodeChunk(sChunk, sRemainder);

                //Piece together the content
                sContent := sContent + sChunk;

                //Set the Current chunk to the remainder for the next round
                sChunk := sRemainder;

              end; //while 2+2=4

              //*****CHUNK FOOTER********
              //continue downloading the final chunk until CRLF+CRLF is found
              while not IsFooterComplete(sChunk) do begin
                if conn.WaitForData(1) then begin
                  httpstatus.Receive(1);
                  try
                    iLength := conn.ReadData(sBuffer, BUFFER_SIZE);
                  finally
                    httpstatus.Receive(-1);
                  end;
                  CheckDownload(true, iLength>0, tmStart, tmEnd);
                  if iLength> 0 then begin
                    sBufferString := '';
                    setlength(sBufferString, iLength);
                    movemem32(@sBufferstring[1], sBuffer, iLength);
                    DebugStuff(sBufferString);
                    sChunk := sChunk + sBufferstring;
                  end;
                end;
              end;
              //Decode the final chunk
              DecodeChunk(sChunk, sRemainder);
              //Merge the remainder with the header
              MergeHeaderAndFooter(sResponse, sRemainder);

              ChangeHeaderParam(sResponse, 'Content-Length', inttostr(length(sContent)));
            END;
            result := true;
          end;
        end;

//        conn.Disconnect;
        result := true;

        //finally setup the properties as the results.
        InHeader := sResponse;
        InBody := sContent;
      end else begin
        result := false;
        Raise EHTTPError.create(conn.Error);
      end;
      OnDecompress;

    finally
      Freemem(sBuffer);
      sTemp := GetHeaderParam(sResponse, 'Connection');
      Debug.Log(self,'Forward Connection: '+sTemp, 'Forwarder');
      if lowercase(stemp) <> 'keep-alive' then
        Closeconnection(conn);

      //NoNeedConnection(conn);
    end;
  finally
    httpstatus.Transaction(-1);
    if AutoLog then LogTransaction;
  end;
end;
//------------------------------------------------------------------------------
procedure TRawHTTP.CheckDownload(bWaitForDataResult: boolean; bDataWasReturned: boolean; var tm1, tm2: cardinal);
//Manages a timeout condition for the download.  If connection is inactive for
//too long... an exception is raised
begin
  if bDataWasReturned then begin
    tm1 := GetTicker;
    tm2 := tm1;
  end else begin
    if bWaitForDataResult then begin
      //if WaitForData returns TRUE but no data is returned, the connection was normally closed
      if not bDataWasReturned then begin
        self.Closeconnection(self.FLastHost, FLastEndpoint);
        raise Ecritical.create('connection dropped');
      end;
    end else begin
      tm2 := GetTicker;

      //Handle timer rollover
      if tm2 < tm1 then
        tm2 := tm1;

      if (tm2-tm1) > self.TimeOut then begin
        RaiseTimeoutError;

      end;
    end;
  end;
end;
//------------------------------------------------------------------------------
function THTTPClient.Go(bPost: boolean; sURL:HTTPstring; sReferer: HTTPstring = ''; iSize: integer =-1): boolean;
//Posts to a URL, e.g. like submitting a FORM on the internet.  Set OutBody before
//posting.
begin
  result := GoIE(bPost, sURL, sReferer, iSize);


end;
function THTTPClient.GoIE(bPost: boolean; sURL, sReferer: HTTPstring;
  iSize: integer): boolean;
//SAMPLE
//POST http://login.myspace.com/index.cfm?fuseaction=login.process&MyToken=6b3cee52-0ab3-4798-807a-3ea183ed3f89 HTTP/1.0
//Referer: http://www.myspace.com/index.cfm?fuseaction=splash
//Accept-Language: en-us
//Content-Type: application/x-www-form-urlencoded
//UA-CPU: x86
//Pragma: no-cache
//User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.2; SV1; .NET CLR 1.1.4322; .NET CLR 2.0.40607)
//Host: login.myspace.com
//Content-Length: 177
//Proxy-Connection: Keep-Alive
//Cookie: MSCulture=IP=209.240.86.83&IPCulture=en-US&PreferredCulture=en-US&Country=US

var
  sHost, sDoc: HTTPstring;
  sLocation: HTTPstring;
  sVer: HTTPstring;
  t: integer;
  sLeft, sRight: HTTPstring;
const
  EOL = #13#10;
begin
  sURL := stringreplace(sURL, ' ', '%20', [rfReplaceAll]);
  if bPost then begin
    Debug.Log(self,'POST '+sURL);
  end else
    Debug.Log(self,'GET '+sURL);

  FLastURLREquested := sURL;

  DecodeURL(sURL, sHost, sdoc);

  if sHost='' then
    sHost := HostCache;

  HostCache := sHost;

  if Mode10 then
    sVer := 'HTTP/1.0'
  else
    sVer := 'HTTP/1.1';

  if SendFullURL then begin
    if bPost then begin
      OutHeader := 'POST '+sURL+' '+sVer+EOL;
    end else begin
      OutHeader := 'GET '+sURL+' '+sVer+EOL;
    end;
  end else begin
    if bPost then begin
      OutHeader := 'POST '+sDoc+' '+sVer+EOL;
    end else begin
      OutHeader := 'GET '+sDoc+' '+sVer+EOL;
    end;
  end;


  OutHeader := OutHeader +
              'Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*'+EOL;

  if sReferer <>'' then
    OutHeader := OutHeader+'Referer: '+sReferer+EOL;

  OutHeader := OutHeader +
              'Accept-Language: en-us'+EOL;

  if bPost then begin
    OutHeader := OutHeader+
      'Content-Type: '+self.outcontenttype+EOL;
  end;

  OutHeader := OutHeader +
              'UA-CPU: x86'+eol;

  if self.Authorization <> '' then
    OutHeader := OutHeader + 'Authorization: '+Authorization+eol;


  if self.EnableGZIP then
    OutHeader := OutHeader + 'Accept-Encoding: gzip, deflate'+EOL;

//  OutHeader := OutHeader +
//              'Pragma: no-cache'+eol;


  if GetDate(sURL) <> '' then
    OutHeader := OutHeader + 'If-Modified-Since: '+GetDate(sURL)+EOL;


  if GetETag(sURL) <> '' then
    OutHeader := OutHeader + 'If-None-Match: '+GetETag(sURL)+EOL;


  if FUserAgent <> '' then
    OutHeader := OUtHeader+'User-Agent: '+UserAgent+EOL;

  OutHeader := OutHeader +
              'Host: '+sHost+EOL;

  if bPost then begin
    OutHeader := OutHeader+
      'Content-Length: '+inttostr(length(OutBody))+EOL;
  end;







//              'Date:'+Rfc822DateTime(now)+#13#10;


  if not DontsendCookies then begin

    if FCookies.Count > 0 then begin

      for t:= 0 to FCookies.count-1 do begin
        OutHeader := OutHeader+'Cookie: ';
        if ShouldSendCookie(sHost, t) then begin
          SplitString(Fcookies[t], '=', sLeft, sright);

          //if sright <> '' then begin
            OutHeader := OutHeader + sLeft+'='+sRight;
            if t < FCookies.count-1 then
              OutHeader := OutHeader +'; ';
          //end;
          OutHeader := OutHeader+EOL;
        end;

      end;
    end;
  end;


//  OutHeader := OutHeader+'Cookie2: $Version=1'+EOL;
//  OutHeader := OutHeader+'Proxy-Connection: close'#13#10;



  //CRLF to mark end of header
//  if Mode10 then
//    OutHeader:= OutHeader+'Content:'+#13#10#13#10
//  else
  if not Mode10 then begin
      if KeepConnectionAlive then
        OutHeader := outHeader+'Connection: Keep-Alive'+EOL
      else
        OutHeader := outHeader+'Connection: close'+EOL;

  end;

  OutHeader := outHeader+'Cache-Control: no-cache'+EOL;

  OutHeader:= OutHeader+#13#10;

  if not bPost then
    OutBody := '';


  Debug.Log(self,OutHeader+'-------'#13#10+OutBody);


  result := Transact;


  t:=1;
//  if not (GetResultCode(InHeader)='302') then

  while GetHeaderParam(InHeader, 'Set-Cookie', t)<>'' do begin
    AddCookie(GetHeaderParam(InHeader, 'Set-Cookie', t));
    inc(t);
  end;

  if GetHeaderParam(InHeader, 'ETag')<>'' then begin
    TrackEtag(sURL, GetHeaderParam(InHeader, 'ETag'));
  end;

  if GetHeaderParam(InHeader, 'Last-Modified')<>'' then begin
    TrackDate(sURL, GetHeaderParam(InHeader, 'Date'));
  end;




  if downloadLinks and not (GetResultCode(InHeader)='302') then begin
    self.downloadlinks := false;
    try
      sLeft := request;
      sright := response;
      DownloadallLinks(self,sURL, InBody);
    finally
      self.downloadlinks := true;
      FLastURLRequested := sURL;
      Request := sLeft;
      Response:= sright;
    end;

  end;

  FLastURLRequested := sURL;

  if AutoRedirect then begin
    sLocation := GetHeaderParam(InHeader, 'Location');
    if (sLocation <> '') and (GetResultCode(InHeader)='302') then begin
      result := Get(sLocation, sReferer);
    end;
  end;

  FLastURLRequested := sURL;


end;

function THTTPClient.GoOpera(bPost: boolean; sURL, sReferer: HTTPstring;
  iSize: integer): boolean;
var
  sHost, sDoc: HTTPstring;
  sLocation: HTTPstring;
  sVer: HTTPstring;
  t: integer;
  sLeft, sRight: HTTPstring;
const
  EOL = #13#10;
begin
  sURL := stringreplace(sURL, ' ', '+', [rfReplaceAll]);
  if bPost then begin
    Debug.Log(self,'POST '+sURL);
  end else
    Debug.Log(self,'GET '+sURL);

  FLastURLREquested := sURL;

  DecodeURL(sURL, sHost, sdoc);

  if sHost='' then
    sHost := HostCache;

  HostCache := sHost;

  if Mode10 then
    sVer := 'HTTP/1.0'
  else
    sVer := 'HTTP/1.1';

  if SendFullURL then begin
    if bPost then begin
      OutHeader := 'POST '+sURL+' '+sVer+EOL;
    end else begin
      OutHeader := 'GET '+sURL+' '+sVer+EOL;
    end;
  end else begin
    if bPost then begin
      OutHeader := 'POST '+sDoc+' '+sVer+EOL;
    end else begin
      OutHeader := 'GET '+sDoc+' '+sVer+EOL;
    end;
  end;

  if FUserAgent <> '' then
    OutHeader := OUtHeader+'User-Agent: '+UserAgent+EOL;

  OutHeader := OutHeader +
              'Host: '+sHost+EOL+
              'Accept: text/html, application/xml;q=0.9, application/xhtml+xml, image/png, image/jpeg, image/gif, image/x-xbitmap, */*;q=0.1'+EOL+
//              'Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/x-shockwave-flash, */*'#13#10+
              'Accept-Language: en'+EOL+
              'Accept-Charset: iso-8859-1, utf-8, utf-16, *;q=0.1'+EOL;

  if self.EnableGZIP then
    OutHeader := OutHeader + 'Accept-Encoding: deflate, gzip, x-gzip, identity, *;q=0'+EOL;


  if sReferer <>'' then
    OutHeader := OutHeader+'Referer: '+sReferer+EOL;




//              'Date:'+Rfc822DateTime(now)+#13#10;


  if not DontsendCookies then begin

    if FCookies.Count > 0 then begin
      OutHeader := OutHeader+'Cookie: ';

      for t:= 0 to FCookies.count-1 do begin
        SplitString(Fcookies[t], '=', sLeft, sright);

        if sright <> '' then
          OutHeader := OutHeader + sLeft+'='+sRight;

        if t < FCookies.count-1 then
          OutHeader := OutHeader +'; ';

      end;
      OutHeader := OutHeader+EOL;
    end;
  end;


  OutHeader := OutHeader+'Cookie2: $Version=1'+EOL;
//  OutHeader := OutHeader+'Proxy-Connection: close'#13#10;

  if bPost then begin
    OutHeader := OutHeader+
      'Content-Type: '+self.outcontenttype+EOL+
      'Content-Length: '+inttostr(length(OutBody))+EOL;
  end;


  //CRLF to mark end of header
//  if Mode10 then
//    OutHeader:= OutHeader+'Content:'+#13#10#13#10
//  else
    OutHeader:= OutHeader+#13#10;

  if not bPost then
    OutBody := '';


  Debug.Log(self,OutHeader+'-------'#13#10+OutBody);


  result := Transact;


  t:=1;
//  if not (GetResultCode(InHeader)='302') then

  if downloadLinks and not (GetResultCode(InHeader)='302') then begin
    self.downloadlinks := false;
    try
      sLeft := request;
      sright := response;
      DownloadallLinks(self,sURL, InBody);
    finally
      self.downloadlinks := true;
      FLastURLRequested := sURL;
      Request := sLeft;
      Response:= sright;
    end;

  end;
  FLastURLRequested := sURL;

  while GetHeaderParam(InHeader, 'Set-Cookie', t)<>'' do begin
    AddCookie(GetHeaderParam(InHeader, 'Set-Cookie', t));
    inc(t);
  end;



  if AutoRedirect then begin
    sLocation := GetHeaderParam(InHeader, 'Location');
    if (sLocation <> '') and (GetResultCode(InHeader)='302') then begin
      sLocation := stringreplace(sLocation,'==','%3D%3D', [rfReplaceAll]);
      result := Get(sLocation, sReferer);
    end;
  end;

end;

function THTTPClient.GUnZipFile: HTTPstring;
begin
  result := slash(extractfilepath(DLLName))+'gunzip.exe';
end;

function THTTPClient.Post(sURL, sReferer: HTTPstring; iSize: integer): boolean;
begin
  result := Go(true, sURL, sReferer, iSize);
end;

function THTTPClient.Put(sURL: HTTPstring; iLength: integer = -1; sReferer: HTTPstring = ''): boolean;
//Calls PUT at a url, never been fully tested.
var
  sHost, sDoc: HTTPstring;
  sLocation: HTTPstring;
begin
  FLastURLREquested := sURL;
  DecodeURL(sURL, sHost, sdoc);

  if iLength = -1 then
    iLength := length(OutBody);

  OutHeader := 'PUT '+sDoc+' HTTP/1.1'#13#10+
              'Host: '+sHost+#13#10+
              'Content-Length: '+inttostr(iLength)+#13#10+
              'User-Agent: '+UserAgent+#13#10+
              'Referer: '+sReferer+#13#10#13#10;

  result := Transact;

  if AutoRedirect then begin
    sLocation := GetHeaderParam(InHeader, 'Location');
    if sLocation <> '' then begin
      result := Get(sLocation, sURL);
    end;
  end;

end;


function THTTPClient.ShouldSendCookie(sHost: HTTPstring; idx: integer): boolean;
var
  sCookiehost: HTTPstring;
begin
  sCookiehost := FCookiehosts[idx];

  result := pos(lowercase(sCookieHost), lowercase(sHost)) > 0;

end;

procedure THTTPClient.TrackETag(sURL, sTag: HTTPstring);
begin
  if not assigned(FETags) then FETags := TStringlist.create;

  FETags.add(sTag+'='+sURL);
end;

procedure THTTPClient.TrackDate(sURL, sDate: HTTPstring);
begin
  if not assigned(FDates) then FDates := TStringlist.create;

  FDates.add(sDate+'='+sURL);
end;


{ THTTPClient }
//------------------------------------------------------------------------------
procedure THTTPClient.ClearCookies;
begin
  self.FCookies.clear;
  self.FCookieHosts.clear;
end;

constructor THTTPClient.Create;
//standard constructor.
begin
  inherited;
  FCookies := tStringlist.create;
  FCookieHosts :=  TStringlist.create;

//  FUserAgent := 'Mozilla/4.0 (compatible; MSIE 5.0; Windows 98; DigExt)';
//  FUserAgent := 'Mozilla/2.0';
  FUserAgent := 'Mozilla/5.0 (X11; U; Gecko)';
//  FUserAgent := 'Mozilla/4.0 (compatible; MSIE 6.0; X11; Linux i686; en) Opera 8.02';
//  FUserAgent := 'Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.2; SV1; .NET CLR 1.1.4322)';


  FAutoChunk := true;
  FautoRedirect := true;
  autolog := true;
  FMode10 := false;
  FKeepConnectionAlive := true;    
  FOutcontentType := 'application/x-www-form-urlencoded';

end;

//------------------------------------------------------------------------------
function THTTPClient.Get(sURL: HTTPstring; sReferer: HTTPstring = ''): boolean;
//Gets a URL. Returns TRUE if success.  Read related properties for results.
begin
  result := Go(false, sURL, sReferer, 0);

end;

function Rfc822DateTime(t : TDateTime) : HTTPstring;
var
    I                   : Integer;
    SaveShortDayNames   : array[1..7] of HTTPstring;
    SaveShortMonthNames : array[1..12] of HTTPstring;
const
    MyShortDayNames: array[1..7] of string =
        ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
    MyShortMonthNames: array[1..12] of string =
        ('Jan', 'Feb', 'Mar', 'Apr',
         'May', 'Jun', 'Jul', 'Aug',
         'Sep', 'Oct', 'Nov', 'Dec');
begin
    if FormatSettings.ShortDayNames[1] = MyShortDayNames[1] then
        Result := FormatDateTime('ddd, d mmm yyyy hh:mm:ss', t) +
                  ' ' + TimeZoneBias
    else begin
        { We used a localized Delphi version, the day and month names are no }
        { more english names ! We need to save and replace them              }
        for I := Low(FormatSettings.ShortDayNames) to High(FormatSettings.ShortDayNames) do begin
            SaveShortDayNames[I] := FormatSettings.ShortDayNames[I];
            FormatSettings.ShortDayNames[I]     := MyShortDayNames[I];
        end;

        for I := Low(FormatSettings.ShortMonthNames) to High(FormatSettings.ShortMonthNames) do begin
            SaveShortMonthNames[I] := FormatSettings.ShortMonthNames[I];
            FormatSettings.ShortMonthNames[I]     := MyShortMonthNames[I];
        end;

        Result := FormatDateTime('ddd, d mmm yyyy hh:mm:ss', t) +
                  ' ' + TimeZoneBias;

        for I := Low(FormatSettings.ShortDayNames) to High(FormatSettings.ShortDayNames) do
            FormatSettings.ShortDayNames[I] := SaveShortDayNames[I];
        for I := Low(FormatSettings.ShortMonthNames) to High(FormatSettings.ShortMonthNames) do
            FormatSettings.ShortMonthNames[I] := SaveShortMonthNames[I];
    end;
end;

function THTTPClient.Get_ResultCode: HTTPstring;
//Getter for the ResultCode property.
begin
  result := GetResultCode(InHeader);
end;

procedure TRAWHTTP.SaveToFile(sFile: string);
var
  f: textfile;
begin
  assignfile(f, sFile);
  rewrite(f);
  try
    write(f,self.InBody);
  finally
    CloseFile(f);
  end;

end;


procedure TRawHttp.SaveToStream(s: TStream);
var
  ary: array[0..1000000] of HTTPChar;
  c: HTTPChar;
  ss: HTTPstring;
var
  t: integer;
  iLen: integer;
begin
  s.Seek(0,0);


  ss := GetHeaderParam(InHeader, 'Content-length');
  if ss = '' then
    iLen := 0
  else
    iLen := strtoint(ss);


  for t:= 1 to iLen do begin
    c := InBody[t];
    s.Write(c,sizeof(HTTPChar));

  end;


end;

destructor THTTPClient.destroy;
begin
  FCookieHosts.free;
  FCookies.free;
  FETags.free;
  FDates.free;
  inherited;
end;

function TimeZoneBias : HTTPstring;
const
    Time_Zone_ID_DayLight = 2;
var
    TZI       : tTimeZoneInformation;
    TZIResult : Integer;
    aBias     : Integer;




{ TPostThread }


begin
    TZIResult := GetTimeZoneInformation(TZI);
    if TZIResult = -1 then
        Result := '-0000'
    else begin
         if TZIResult = Time_Zone_ID_DayLight then   { 10/05/99 }
             aBias := TZI.Bias + TZI.DayLightBias
         else
             aBias := TZI.Bias + TZI.StandardBias;
         Result := Format('-%.2d%.2d', [Abs(aBias) div 60, Abs(aBias) mod 60]);
         if aBias < 0 then
             Result[1] := '+';
    end;
end;



procedure THTTPClient.AddPostParam(sName, sValue: HTTPstring; bNoEncode:boolean= false);
var
  s1,s2: HTTPstring;
begin
  if OutBody <> '' then
    OutBody := OutBody + '&';

  s1 := sName;
  s2 := sValue;
  if not bNoEncode then begin
    s1 := EncodeWebString(sName);
    s2 := EncodeWebString(sValue);
  end;
  OutBody := OutBody+s1+'='+s2;

end;


function THTTPClient.Head(sURL, sReferer: HTTPstring): boolean;
var
  sHost, sDoc: HTTPstring;
  sTemp : HTTPstring;
  sl : TStringList;
  sVersion: HTTPstring;
begin
  FLastURLREquested := sURL;
  sl := TStringList.create;
  try
    DecodeURL(sURL, sHost, sdoc);

    if sHost = '' then
      sHost := HostCache;

    TrimStr(shost);
    TrimStr(sDoc);

    if Fmode10 then
      sVersion := '1.0'
    else
      sVersion := '1.1';

    sTemp := 'HEAD '+sDoc+' HTTP/'+sVersion+#13#10+
                'Host: '+sHost+#13#10+
                'Accept: www/source, text/html, video/mpeg, image/jpeg, image/x-tiff'#13#10+
                'Accept: image/x-rgb, image/x-xbm, image/gif, */*, application/postscript'#13#10+
                'User-Agent:'+UserAgent+#13#10+
                'Content-type: application/x-www-form-urlencoded'#13#10;

    if sReferer <> '' then begin
      request := sTemp+ 'Referer: '+sReferer+#13#10;
    end;

    sl.text := sTemp+#13#10;

    request := sl.text;
    result := Transact(true);

  finally
    sl.free;
  end;
end;


procedure THTTPClient.OnDeCompress;
begin
  inherited;
  if not EnableGZIP then
    exit;
  if lowercase(GetHeaderParam(InHeader, 'Content-Encoding')) = 'gzip' then begin
    Extract;
  end;

end;

function QuickHTTPGet(sURL: HTTPstring): HTTPstring;
var
  htp: THTTPClient;
begin
  htp := THTTPClient.create;
  try
    if htp.Get(sURL) then begin
      result := htp.InBody;
    end else
      result := '';
  finally
    htp.free;
  end;
end;



procedure THTTPClient.AddCookie(sNAme, sValue, sHost: HTTPstring);
var
  t: integer;
  sLeft, sRight: HTTPstring;
begin
  if SoundOnCookie then begin
    beeper.Beep(750,10);
    beeper.Beep(1000,10);

  end;
  for t:= 0 to FCookies.Count-1 do begin
    if SplitString(FCookies[t], '=', sLeft, sRight) then begin
      if (Lowercase(sLeft) = lowercase(sName)) and (lowercase(FCookiehosts[t]) = lowercase(sHost)) then begin
        FCookies[t] := sName+'='+sValue;
        if sVAlue = '' then begin
          FCookies.Delete(t);
          FcookieHosts.delete(t);
        end;
        exit;
      end;
    end;
  end;

  //code will exit BEFORE this line if an existing cooke was found
  FCookies.add(sName+'='+sValue);
  FCookieHosts.add(sHost);



end;

procedure THTTPClient.AddCookie(sNameAndValueAndHost: HTTPstring);
var
  sLeft, sRight: HTTPstring;
  sHost: HTTPstring;
  sName, sValue: HTTPstring;
begin
//  Debug.Log('Add cookie:'+sNameAndValueAndHost, 'myspace');
  SplitString(sNameAndValueAndHost, ';', sLeft, sHost);
  SplitString(sLeft, '=', sName, sValue);
  SplitString(sHost, 'domain=', sLeft, sHost);
  SplitString(sHost, ';', sHost, sRight);

  AddCookie(sName, sValue, sHost);



end;

procedure TRawHTTP.LogTransaction;
var
  f: TextFile;
  sCT: HTTPstring;
  s: HTTPstring;
  sCrap: HTTPstring;
begin
  {$IFDEF DISABLE_AUTOLOG}
  exit;
  {$ENDIF}


  s := 'httplog.txt';

  assign(f, s);
  if fileexists(s) then
    append(f)
  else
    rewrite(f);

  try
    writeln(f, '########################################################');
    writeln(f, OutHeader);
    writeln(f, '--------------------------------------------------------');
    writeln(f, OutBody);
    writeln(f, '********************************************************');
    sCT := GetHEaderParam(InHeader, 'Content-Type', 1);
    SplitString(sCT, ';', sCT, sCrap);
    if (lowercase(sCT) = 'text/html') or (lowercase(sCT) = 'application/x-javascript') or (lowercase(sCT) = 'text/html')then begin
      writeln(f, InHeader);
      writeln(f, '--------------------------------------------------------');
      writeln(f, InBody);
    end else begin
      writeln(f, InHeader);
      writeln(f, '--------------------------------------------------------');
      writeln(f, '[[['+inttostr(length(InBody))+' bytes]]]');
    end;

  finally
    closefile(f);
  end;

end;

procedure TRawHttp.OnDeCompress;
begin

//TODO -cunimplemented: unimplemented block
end;

function THTTPClient.GetCookie(sName: HTTPstring): HTTPstring;
var
  t: integer;
  s: HTTPstring;
  sl,sr: HTTPstring;
begin
  result := '';

  for t:= 0 to CookieCount-1 do begin
    s := Getcookies(t);
    SplitString(s,'=', sl,sr);
    if lowercase(sl) = lowercase(sName) then
      result := sr;

  end;

end;

function THTTPClient.GetCookieCount: integer;
begin
  result := FCookies.count;
end;

function THTTPClient.GetCookieHosts(idx: integer): HTTPstring;
begin
  result := FCookieHosts[idx];
end;

function THTTPClient.GetCookies(idx: integer): HTTPstring;
begin
  result := FCookies[idx];
end;

function THTTPClient.GetDate(sURL: HTTPstring): HTTPstring;
var
  sLeft, sright: HTTPstring;
  t: integer;
begin
  if not assigned(FETags) then FDates := TStringlist.create;

  result := '';
  for t:= FDates.count-1 downto 0 do begin
    SplitString(FDAtes[t], '=', sLeft, sRight);
    if sright = sURL then begin
      result := sLEft;
      exit;
    end;
  end;


end;

{ THTTPStatus }

procedure THTTPClient.Extract;
var
  s: HTTPstring;
  sFile: string;
begin
  sfile := systemx.GetTempPath+inttostr(GetCurrentThreadID());
  try
    self.SaveToFile(sFile+'.gz');
    exe.RunProgramAndWait('"'+GunzipFile+'"', sFile+'.gz', extractfilepath(self.GunzipFile),true);

    self.InBody := LoadStringFromFile(sfile);


  finally
    if fileexists(sFile) then
      deletefile(PChar(sFile));

    if fileexists(sFile+'.gz') then
      deletefile(PChar(sFile+'.gz'));


  end;


end;

function THTTPStatus.GetInReceive: integer;
begin
  Lock;
  try
    result := self.FReceive;
  finally
    Unlock;
  end;

end;

function THTTPStatus.GetInRetrying: integer;
begin
  Lock;
  try
    result := self.FRetrying;
  finally
    Unlock;
  end;


end;

function THTTPStatus.GetInTransaction: integer;
begin
  Lock;
  try
    result := self.FTransactions;
  finally
    Unlock;
  end;

end;

function THTTPStatus.GetInTransmit: integer;
begin
  Lock;
  try
    result := self.FTransmit;
  finally
    Unlock;
  end;

end;

procedure THTTPStatus.Receive(inc: integer);
begin
  Lock;
  try
    FReceive := FReceive + inc;
  finally
    Unlock;
  end;

end;

procedure THTTPStatus.Retry(inc: integer);
begin
  Lock;
  try
    FRetrying := FRetrying + inc;
  finally
    Unlock;
  end;
end;

procedure THTTPStatus.Transaction(inc: integer);
begin
  Lock;
  try
    FTransactions := FTransactions + inc;
  finally
    Unlock;
  end;

end;

procedure THTTPStatus.Transmit(inc: integer);
begin
  Lock;
  try
    FTransmit := FTransmit + inc;
  finally
    Unlock;
  end;
end;


procedure DownloadAllLinks(htp: THTTPClient; sBaseURL, sBody: HTTPstring);
begin
  DownloadLinks(htp, sBaseURL, sBody, 'img', 'src');
  DownloadLinks(htp, sBaseURL, sBody, 'embed', 'src');
  DownloadLinks(htp, sBaseURL, sBody, 'script', 'src');
  DownloadLinks(htp, sBaseURL, sBody, 'iframe', 'src');
  DownloadLinks(htp, sBaseURL, sBody, 'link', 'href');

end;
procedure DownloadLinks(htp: THTTPClient; sBaseURL, sBody, sTag, sAttribute: HTTPstring);
var
  sLeft, sRight: HTTPstring;
  sLink: HTTPstring;

  sl: TStringlist;
  t: integer;
begin
  sl := TStringlist.create;
  sl.Duplicates := dupIgnore;
  try

    sright := sBody;

    while splitstringNocase(sBody, '<'+sTag, sLeft, sRight) do begin
      SplitStringNoCase(sright, '>', sLeft, sRight);
      sBody := sright;
      if SplitStringNoCase(sLeft, sAttribute, sLeft, sright) then begin
        if splitStringNoCase(sright,'=', sLeft, sRight) then begin
          if splitStringNocase(sRight, '"', sLeft, sright) then begin
            SplitStringnoCase(sright, '"', sLink, sright);
          end else begin
            SplitStringnoCase(sright, ' ', sLink, sright)

          end;
        end else begin
          sLink := '';
        end;


        if sLInk <> '' then
          sl.add(sLInk);


      end;
    end;


    for t:= 0 to sl.Count-1 do begin
      sLInk := sl[t];
      if sLink <> '' then begin
        sLink := applyrelativeurl(sBaseURL, sLink);
        Debug.Log('nil',nil,'download link:'+sLInk);
        try
//          htp.DownloadLinks := false;
          htp.Get(sLink, sBaseURL);
//          htp.DownloadLinks := true;
        except
          on E: Exception do begin
            Debug.Log('nil',nil,'Warning: Failed to download link:'+sLink);
          end;
        end;
      end;
    end;
  finally
    sl.free;
  end;


end;

procedure oinit;
begin
  httpStatus := THTTPSTatus.create;
  poop := '';

end;

procedure ofinal;
begin
  httpStatus.free;


end;

initialization
  init.RegisterProcs('HTTPClient', oinit, ofinal);


finalization


end.

