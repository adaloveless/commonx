unit RequestInfo;


interface

uses typex, classes, DataObjectCache, DataObjectServices, DataObjectCacheManager, DataObject, ServerInterfaceInterface, windows,
  errorHandler, Exceptions, ErrorResource, webstring, webconfig, betterobject, sharedobject, stringx, XMLTools, variants, simpleserverinterface, httpclient, variantlist, interfacelist, debug, sysutils;

type
  EObjectMissing = class(Exception);
  TRequestInfo = class;
  TParamSource = (pcHeader, pcContent, pcCookie, pcInline, pcDirectory, pcGenerated);
  TMotherShipWebRequest = class;
  TMotherShipWebResponse = class;
  TMotherShipWebHook = procedure of object;
  TMotherShipWebChunkEvent = procedure of object;
  TWebHookProcedure = procedure(rqInfo: TRequestInfo);
  TRQConnectionCommand = (rqcAuto, rqcClose, rqcKeepAlive);
  TProgressCallbackProcedure = procedure (pos, max: integer; sMessage: string) of object;

  IVarpoolVarExtender = interface
  ['{E5075499-372E-4E01-A9C7-D2CB3D7BF14A}']


  function LoadVar(sName: string): variant;
  function AgeOfVar(sName: string):real;
  function IsDefined(sVar: string): boolean;



  end;

//------------------------------------------------------------------------------
  TRequestInfo = class(TSharedObject)
  //This class encapsulates TMotherShipWebRequest and TMotherShipWebResponse.  And that's
  //about it.  The two classes it encapsulates represent all the information
  //needed to be known about the request being made, and all the data
  //that needs to be spoken for in the response.
  private
    FCopied: boolean;
    FDTID: integer;
    FISRNDTID: integer;
    FSessionID: integer;
    FPercentComplete: single;
    FServer: IServerInterface;

    FHTTPClient: THTTPClient;
    FDoTransactions: boolean;
    FNoHit: boolean;
    FProgressMax: integer;
    FProgressPos: integer;
    FStatus: string;
    FProgressHook: TProgressCallbackProcedure;
    FPopto: TRequestInfo;
    FThreadID: integer;

    procedure SetHTTPClient(const Value: THTTPClient);
    function GetHTTPClient: THTTPClient;
    function GetThreadID: integer;

    function InitDTID: integer;
    function GetSessionID: integer;
    procedure SetDTID(const Value: integer);
    procedure SetISRNDTID(const Value: integer);
    procedure SetSessionID(const Value: integer);
    function GetSessionHash: string;
    procedure SetSessionHash(const Value: string);
    function GetDTID: integer;
    function GetServer: IServerInterface;
    function GetDoTransactions: boolean;



  protected
    bInitialized: boolean;
    FRequest: TMotherShipWebRequest;
    FResponse: TMotherShipWebResponse;
  public

    constructor Create;override;
    constructor CopyCreate(source: TRequestInfo; bNewCache: boolean = true);
    procedure BeforeDestruction;override;
    destructor destroy; override;

    property Request: TMotherShipWebRequest read FRequest;
    property Response: TMotherShipWebResponse read FResponse;


    procedure SetupDefaultVarPool;
    procedure InitializeEngine;


    property DTID: integer read GetDTID write SetDTID;
    property SessionID: integer read GetSessionID write SetSessionID;
    property SessionHash: string read GetSessionHash write SetSessionHash;


    property Copied: boolean read FCopied;
    procedure ForceInit;
    property PercentComplete: single read FPErcentComplete write FPercentComplete;

    property Server: IServerInterface read GetServer write FServer;
    property DoTransactions: boolean read FDoTransactions write FDoTransactions;

    procedure CloseServer;
    property ProgressMax: integer read FProgressMax write FProgressMax;
    property ProgressPos: integer read FProgressPos write FProgressPos;
    property Status: string read FStatus write FStatus;
    property ProgressHook: TProgressCallbackProcedure read FProgressHook write FProgresshook;
    procedure UpdateProgress(pos, max: integer; sMessage: string);

    function ProcessQuery(sQuery: string): string;

    procedure Commit;
    procedure Rollback;
    property NoHit: boolean read FNoHit write FNoHit;

    procedure SaveVar(sName, sValue: string);
    procedure DeleteVar(sName: string);
    function LoadVar(sName: string; sDefault: string = ''): string;overload;
    function LoadVar(sName: string; iDefault: integer): integer;overload;
    function LoadVar(sName: string; rDefault: real): real;overload;
    procedure LoadVars;
    procedure SaveVars;
    property ThreadID: integer read GetThreadID write FThreadID;

    property PopTo: TRequestInfo read FPopto write FPopTo;
    function Push: TRequestInfo;
    function Pop: TrequestInfo;

    property HTTPClient: THTTPClient read GetHTTPClient write SetHTTPClient;
    function StealHTTPClient: THTTPClient;
  end;
//------------------------------------------------------------------------------
  TMotherShipWebRequest = class(TFakeLockQueuedObject)
  private
    FClientIP: string;
    FCopied: boolean;
    FRequestInfo: TRequestInfo;
    FOriginalDocument: string;
    FIsSecure: boolean;
    function GetUserAgent: string;
    procedure SetParams(sName: string; const Value: string);
    function GetReferer: string;
    function GetParamsByPatternMatch(sLeftMatch: string;
      idx: integer): string;
    function GetParamNamesByPatternMatch(sLeftMatch: string;
      idx: integer): string;

    function GetParamPatternCount(sLeftMatch: string): integer;
    procedure SetDocument(const Value: string);
    function GetDocument: string;
    function GetOriginalDocument: string;
    function GetFullURL: string;
  //Provides all needed information about the request being made of the
  //Web server.
  //Note: the only information that is represented is the information that
  //we've determined we needed for MotherShip Web Learning Network.
  protected
    FRaw: string;
    FContent: string;

    Fheader: TStringList;
    FCommand: string;
    FDocument: string;
    FDocumentExt: string;
    FParams: TStringList;
    FParamNames: TStringList;
    FParamSources: TList;
    FRequestContent: TStringList;
    procedure SetBinaryContentLength(iLength: integer);
    function GetParams(sName: string): string;
    function GetParamCount: integer;
    function GetParamNames(idx: integer): string;
    function GetDocumentExt: string;
    function GetParamSources(idx: integer): TParamSource;
    function GetParamsByIndex(idx: integer): string;

  public
    constructor Create;override;
    constructor CopyCreate(source: TMotherShipWebRequest); virtual;
    destructor destroy; override;
    procedure DecodeMultiPart;

    procedure AddDirectoriesAsParameters;
    //Request Parameters
    procedure AddParam(sName, value: string; source: TParamSource);
    //Used by the abstraction driver.  Define all parameters passed through the
    //HTTP request here
    procedure RemoveParam(sName: string);

    property IsSecure: boolean read FIsSecure write FIsSecure;
    property Params[sName: string]: string read GetParams write SetParams; default;
    //Lookup a parameter by name
    property ParamCount: integer read GetParamCount;
    property ParamNames[idx: integer]: string read GetParamNames;
    //Lookup the name of a parameter by index
    property ParamSources[idx: integer]: TParamSource read GetParamSources;
    //Find the source of a param by index
    property ParamsbyIndex[idx: integer]: string read GetParamsByIndex;
    //Find a parameter by matching a pattern
    property ParamsbyPatternMatch[sLeftMatch: string; idx: integer]: string read GetParamsByPatternMatch;
    function ParamPatternSum(sPattern: string): real;
    property ParamNamesbyPatternMatch[sLeftMatch: string; idx: integer]: string read GetParamNamesByPatternMatch;
    //
    property ParamPatternCount[sLeftMatch: string]: integer read GetParamPatternCount;
    //Find the value of a param by index
    function HasParam(sName: string): boolean;
    function HasParamWithValue(sName: string): boolean;
    function IndexOfParam(sName: string): integer;
    //Returns whether or not a parameter has been defined by given name
    property Command: string read FCommand write FCommand;
    //GET/POST/PUT Command (also called METHOD)
    property Document: string read GetDocument write SetDocument;
    //Document being requested from server excluding HOST information.
    property OriginalDocument: string read GetOriginalDocument;
    //If document name was mangled by proxy, this will be different than Document.  Shows the original document name


    property DocumentExt: string read GetDocumentExt;
    //Extension of document being requested.
    property Raw: string read FRaw write Fraw;
    //Place to put the raw HTTP header that was sent (may not be applicable in
    //some implementations)
    property Content: string read FContent write FContent;

    property Header: TStringList read FHeader write FHeader;
    //Content of HTTP request body -- used only for POST operations.
    //property SessionID: integer read GetSessionID;
    //Special place for database session as an integer (figured we would always
    //need it)
    property ClientIP: string read FClientIP write FClientIP;
    property UserAgent: string read GetUserAgent;
    property Copied: boolean read FCopied;
    property Referer: string read GetReferer;

    function RebuildInlineParameters: string;
    function RebuildParameters: string;
    procedure Default(sParamName, sDefaultValue: string);

    property RequestInfo: TRequestInfo read FRequestInfo write FRequestInfo;
    function PatternQuery(sPattern: string): TStringList;
    property FullURL: string read GetFullURL;




  end;
//------------------------------------------------------------------------------
  TMotherShipWebResponse = class
  private
    FRequest: TMotherShipWebRequest;
    FTransferEncoding: string;
    FDoSendChunk: TMotherShipWebChunkEvent;
    FDoSendHeader: TMotherShipWebHook;
    FHeaderSent: boolean;
    FNoDebug: boolean;
    FRequestInfo: TRequestInfo;
    FCopiedWithOldCache: boolean;
    FFramed: boolean;
    FSendTime: cardinal;
    FOnResponseSent: TWebHookProcedure;
    FMessage: string;
    FcookieNames, FCookieValues: TStringList;
    FDeleteStreamedFile: boolean;
    FDeleteFile: string;
 		FDeleteTempFile: string;
    FDEbugLog: TStringList;
    FRawHeader: string;
    FConnection: TRQConnectionCommand;
    FExtenderStack: TInterfaceList;
    FRangeStart: int64;
    FRangeEnd: int64;
    FAcceptRanges: boolean;
    FFakeContentLength: boolean;
    FIgnore: boolean;
    function GetDataObjectCache: TDataObjectCache;
    function GetObjectPool(sName: string): TDataObject;
    procedure SetObjectPool(sName: string; const Value: TDataObject);
    function GetDOSV: TDataObjectServices;
    function getVarByIndex(idx: integer): string;
    function GetVarNames(idx: integer): string;
    procedure SetVarByIndex(idx: integer; const Value: string);
    procedure SetVarNames(idx: integer; const Value: string);
    function GetCookieCount: integer;
    function GetCookieNames(idx: integer): string;
    function GetCookieValues(idx: integer): string;
    procedure SetContentStream(const Value: TStream);
    function GetHasCache: boolean;
    function GetXMLPool(sName: string): string;
    procedure SetXMLPool(sName: string; const Value: string);
    function GetXMLDocs(sName: string): TXmlDocument;
    function GetDebugLog: TStringList;
    procedure SetVArPoolExtender(const Value: IVarpoolVarExtender);
    function GetVArPoolExtender: IVarpoolVarExtender;
  protected
    FContent: TStringList;
    FContentType: string;
    FContentEncoding: string;
    FcontentStream: TStream;
    FVarNames: TStringList;
    FVarValues: TVariantlist;
    FLocation: string;
    FResultCode: integer;
    FContentLength: int64;
    FDOCache: TDataObjectCache;
    bNeedsProcessing: boolean;
    FObjectPool: TStringList;
    FXMLPool: TStringList;
    FAuthHeaders: tStringlist;
    function GetVarpool(sVarName: string): variant;
    procedure SetVarPool(sVarName: string; const Value: variant);
    function GetVarCount: integer;
    procedure Setlocation(sLoc: string);
  public
    constructor Create;reintroduce;virtual;
    constructor CopyCreate(source: TMotherShipWebResponse; bNewCache: boolean = true);
    destructor destroy; override;

    procedure EmptyXMLPool;

    procedure ProcessDynamicVariables(bAllowAlternateContentTypes: boolean);overload;
    procedure ProcessDynamicVariables(progress_callback: TProgressCallbackProcedure);overload;
    procedure ProcessDynamicVariables(bAllowAlternateContentTypes: boolean; progress_callback: TProgressCallbackProcedure);overload;
    procedure ProcessDynamicVariables();overload;
    //processes in-code variables marked [[[varname]]]
    property Content: TStringList read FContent;
    //Body of HTTP response (HTML or whatever goes here)
    property ContentStream: TStream read FContentStream write SetContentStream;
    //If assigned, response will override the use of the CONTENT parameter
    //and instead use this stream class-- better suited for streaming files,
    //CGI-generated graphics, or other binary types.

    property Connection: TRQConnectionCommand read FConnection write FConnection;
    property DeleteFileOnDestroy: string read FDeleteFile write FDeleteFile;
		property DeleteTempFileOnDestroy: string read FDeleteTempFile write FDeleteTempFile;
    procedure ProcessHTML;


    //If a file is streamed via a TFileStream object attaached to the ContentStream property,
    //setting this to TRUE will delete the file when the request is destroyed.

    property ContentType: string read FContentType write FcontentType;
    property ContentEncoding: string read FContentEncoding write FcontentEncoding;
    //Content type as defined in HTTP response header -- defaults to "text/html"

    property FakeContentLength: boolean read FFakeContentLength write FFakeContentLength;
    property ContentLength: int64 read FContentLength write FContentLength;
    //Length of Content -- if not specified, the implementation of the
    //abstraction should automatically set it to the length of the
    //content-stream or length
    property TransferEncoding: string read FTransferEncoding write FTransferEncoding;
    property Location: string read FLocation write Setlocation;
    //Set location to send a redirect in the header of the HTTP response
    property ResultCode: integer read FResultCode write FResultCode;
    //Result code e.g. 404 Not Found, 200 Ok

    property VarNames[idx: integer]: string read GetVarNames write SetVarNames;
    property VarByIndex[idx: integer]: string read getVarByIndex write SetVarByIndex;
    property VarPool[sVarName: string]: variant read GetVarpool write SetVarPool;
    //Variable pool.  Set variables here named identical to parameters in
    //html encoded as such: [[[varname]]].  The variable will then be
    //replaced with the value in the varpool.
    property VarCount: integer read GetVarCount;
    //Count of the numbder of variable in varpool.
    function HasVar(sVarName: string): boolean;
    //Returns whether or not the particular variable name is defined in the varpool.
    procedure REmoveVAr(sName: string);
    //remove a var

    procedure SetupDefaultVarPool(request: TMotherShipWebRequest);
    //Sets up a default Auto-Variable Pool from the parameters passed in an
    //HTTP Request represented in the passed "request" parameter
    property DOCache: TDataObjectCache read GetDataObjectCache;
    property ObjectPool[sName: string]: TDataObject read GetObjectPool write SetObjectPool;
    function HasObject(sName: string): boolean;
    property Request: TMotherShipWebRequest read FRequest write FRequest;

    property XMLPool[sName: string]: string read GetXMLPool write SetXMLPool;
    //attaches XML to the document which may be used to supply dynamic data to the
    //scripts.  The documents are indexed by a string name.  Don't start the name with a number
    property XMlDocs[sName: string]: TXMLDocument read GetXMLDocs;
    //when an XML string is added to the XMLPool it actually creates an instance
    //of TXMLDocument and stores the string in it.  you can get read-only access
    //to the XML using the methods of TXMLDocument returned from this array property.

    procedure AddAuthHeader(sValue: string);

    procedure AddCookie(sName, sValue: string);overload;
    procedure AddCookie(sFullCookie: string);overload;
    //property CookieName: string read FCookieName write FCookieName;
    //property CookieValue: string read FCookieValue write FCookieValue;
    property CookieNames[idx: integer]: string read GetCookieNames;
    property CookieValues[idx: integer]:string read GetCookieValues;
    property CookieCount: integer read GetCookieCount;

    property Message: string read FMessage write FMessage;

    function VarPoolStatus: string;
    property NeedsProcessing: boolean read bNeedsProcessing write bNeedsProcessing;

    property DoSendChunk: TMotherShipWebChunkEvent read FDoSendChunk write FDoSendChunk;
    property DoSendHeader: TMotherShipWebHook read FDoSendHeader write FDoSendHeader;
    property HeaderSent: boolean read FHeaderSent;
    property NoDebug: boolean read FNoDebug write FNoDebug;
    property RequestInfo: TRequestInfo read FRequestInfo write FRequestInfo;

    procedure SendChunk;overload;
    procedure SendChunk(endrow: integer);overload;
    procedure SendHeader;
    procedure SendFooter;
    property CopiedWithOldCache: boolean read FCopiedWithOldCAche;

    property Framed: boolean read FFramed write Fframed;

    property SendTime: cardinal read FSendTime write FSendTime;
    property OnResponseSent: TWebHookProcedure read FOnResponseSent write FOnResponseSent;

    property DOSV: TDataObjectServices read GetDOSV;
    function ExportVarPool: string;
    procedure ImportVarPool(sImportString: string);
    property HasCache: boolean read GetHasCache;

    procedure Default(varName, varValue: variant);

    property DebugLog: TStringList read FDEbugLog;
    procedure MassageDebugStats;
    property RawHeader: string read FRawHeader write FRawHeader;
    property VArpoolExtender: IVarpoolVarExtender read GetVArPoolExtender write SetVArPoolExtender;

    procedure RaiseVarPool;

    property RangeStart: int64 read FRangeStart write FRangeStart;
    property RangeEnd: int64 read FRangeEnd write FRangeEnd;
    property AcceptRanges: boolean read FAcceptRanges write FAcceptRanges;
    property Ignore: boolean read FIgnore write FIgnore;

  end;


implementation
uses   webfunctions, requestManager, WebScript,
  mothership_html_compiler;

//------------------------------------------------------------------------------
procedure CopyList(lstSource, lstTarget: TList);
var
  t: integer;
begin
  lstTarget.clear;
  for t:= 0 to lstSource.count-1 do begin
    lstTarget.Add(lstSource[t]);
  end;
end;
//------------------------------------------------------------------------------
procedure CopyStringList(slSource: TStringList; slTarget: TStringList; bMakeThreadSafe: boolean = false);
var
  t: integer;
  s: string;
begin
  slTarget.clear;

  for t:= 0 to slSource.count-1 do begin
    s := slSource[t];

    //Make string safe across threads if specified
    if bMakeThreadSafe then
      UniqueString(s);

    slTarget.AddObject(s, slSource.Objects[t]);
  end;

end;

//------------------------------------------------------------------------------
procedure CopyVariantList(slSource: TVariantList; slTarget: TVariantList);
var
  t: integer;
  s: variant;
begin
  slTarget.clear;

  for t:= 0 to slSource.count-1 do begin
    s := slSource[t];

    slTarget.Add(s);
  end;

end;



//------------------------------------------------------------------------------

{ TRequestInfo }

procedure TMotherShipWebRequest.AddDirectoriesAsParameters;
//This function takes the path to the file and add each subdirectory as a prameter
//dir1, dir2, dir3 etc.
var
  sLeft, sRight: string;
  t: integer;
begin
  //ignore the first slash
  SplitString(document, '/', sLeft, sRight);

  t:= 1;
  while SplitString(sRight, '/', sLeft, sRight) do begin
    self.AddParam('_dir'+inttostr(t), DecodeWebString(sLeft, '*'), pcDirectory);
    inc(t);
  end;

  self.AddParam('_dir'+inttostr(t), sLeft, pcDirectory);


end;

procedure TMotherShipWebRequest.AddParam(sName, value: string; source: TParamSource);
//Defines a parameter.  To be used by the abstraction driver.  The driver
//of the abstraction should do what is necessary to extract the parameters
//beit from the actual RAW HTTP header string itself, or by calling functions
//in ISAPI to return definitions that are from already-parsed HTTP headers.
//Source is an ennumerated type with values:
//  pcHeader  -- for parameters as part of the HTTP header e.g. User-Agent,
//               referer, Content-Type, etc.
//  pcContent -- parameters passed as a part of a HTTP post response.
//               !!!Abstraction driver should ONLY parse the content
//               of a POST operation if the CONTENT-TYPE of the body
//               is "x-www-form-urlencoded" else it could be a JPG or something
//  pcCookie -- cookie parameters
//  pcInline -- parameters passed in-line as part of the URL
var
  idx: integer;
begin

  sName := lowercase(sName);

  //if already has a param by name... overwrite it
  idx := FParamNames.indexof(sName);
  if idx>-1 then begin
    //change existsing param
    FParamNames[idx] := lowercase(sName);
    FParams[idx] := value;
    FParamSources[idx]:=pointer(source);
  end
  else begin
    //add new param
    FParamNames.add(lowercase(sName));
    FParams.add(value);
    FParamSources.add(pointer(source));

    //sessionID hook
    if (sName = 'sessionid') or (sName = 'key') then begin
      RequestInfo.SessionHash := value;
    end;
    //user agent registration hook
    if (sName = 'user-agent') then begin
      RqMan.UserAgents.Tally(value);
    end;
  end;

end;

//------------------------------------------------------------------------------
procedure TRequestInfo.BeforeDestruction;
begin
  try
    FHttpClient.free;
  except
  end;
  inherited;

end;

procedure TRequestInfo.CloseServer;
begin
  if self.Response.FDOCache <> nil then begin
    self.Response.FDOCache.server := nil;
    DOCM.FreeCache(self.Response.FDOCache);
    self.response.FDOCache := nil;
  end;

  Response.FObjectPool.Clear;
  //FServer.free;
  DOSVPool[DTID].NoNeedServer(FServer);
  FServer := nil;
end;

procedure TRequestInfo.Commit;
begin
  if assigned(self.FServer) then
    server.Commit;

end;

constructor TRequestInfo.CopyCreate(source: TRequestInfo; bNewCache: boolean = true);
begin
  debug.log('Request Info Copied','leak');
  inherited;
  FRequest := TMotherShipWebRequest.CopyCreate(source.request);
  FResponse := TMotherShipWebResponse.CopyCreate(source.FResponse, bNewCache);
  FResponse.Request := FRequest;
  self.DoTransactions := source.DoTransactions;

  FResponse.RequestInfo := self;
  FRequest.RequestInfo := self;
  FSessionID := source.FSessionID;
  FDTID := source.FDTID;
  FISRNDTID := source.FISRNDTID;
  FCopied := true;
  if bNewCache then begin
    FServer := nil;
  end else begin
    FServer := source.FServer;
  end;

  //inc(GelementCount);
  //writeln('+',GElementCount);

end;
//------------------------------------------------------------------------------
constructor TRequestInfo.Create;
begin
//  debug.log('Request Info Created','leak');
  inherited;
  FThreadID := GetCurrentThreadID;
  FSessionID := -1;
  bInitialized := false;
  FRequest := nil;
  FResponse := nil;
  try
    FRequest := TMotherShipWebRequest.create;
    FResponse := TMotherShipWebResponse.create;
  finally
  end;
  FResponse.Request := FRequest;
  FResponse.RequestInfo := self;
  FRequest.RequestInfo := self;
  FRequest.Default('Host', '');
  Fcopied:=false;
  FDTID := 0;
  rqMan.RegisterRequest(self);
  DoTransactions := true;
  //StrAlloc(1000000);
                                       //inc(GelementCount);
  //writeln('+',GElementCount);

end;

procedure TRequestInfo.DeleteVar(sName: string);
begin
  server.UpdateQuery(response.DOCache,'DELETE from session_vars where varname="'+sName+'" and sessionid='+inttostr(sessionid), sessionid);

end;

//------------------------------------------------------------------------------
destructor TRequestInfo.destroy;
begin
//  debug.log('Request Info Destroyed','leak');
  rqMan.DeRegisterRequest(self);
  FRequest.free;
  FResponse.free;



  if DoTransactions then begin
    if DTID < DOSVPool.count then
      DOSVPool[DTID].NoNeedServer(FServer);
    FServer := nil;
  end;
//    FServer.free;

  inherited;

  //dec(GelementCount);
  //writeln('-',GElementCount);

end;
//------------------------------------------------------------------------------



constructor TMotherShipWebRequest.CopyCreate(source: TMotherShipWebRequest);
begin
//  result := TRequestInfo.create;
  inherited Create;


  //CopyStringList(source.FContent, FContent);
  Fcontent :=  source.FContent;

  FRaw:= '';
  FRaw := source.Fraw;
  UniqueString(FRaw);

  FParams:= TStringList.create;
  CopyStringList(source.FParams, FParams, true);

  FParamNames := TStringList.create;
  CopyStringList(source.FParamNames, FParamNames, true);

  FParamSources := TList.create;
  CopyList(source.FParamSources, FParamSources);

  FRequestContent := TStringList.create;
  CopyStringList(source.FRequestContent, FRequestContent, true);

  Fheader := TStringList.create;
  CopyStringList(source.FHeader, FHEader, true);


  FCommand := source.FCommand;
  UniqueString(FCommand);
  FDocument := source.FDocument;
  FClientIP := source.FClientIP;

  FCopied := true;

end;

constructor TMotherShipWebRequest.Create;
begin
  inherited;
  FIsSecure := false;
  FRaw:= '';
  FHeader:= TStringList.create;
  FParams:= TStringList.create;
  FParamNames := TStringList.create;
  FParamSources := TList.create;
  FRequestInfo := nil;
  FRequestContent := TStringList.create;
  FOriginalDocument := 'untitled';
  FCommand:= '';
  FDocument:= 'untitled';
  FClientIP := '0.0.0.0';
  Fcopied:=false;

end;
//------------------------------------------------------------------------------
procedure TMotherShipWebRequest.DecodeMultiPart;
var
  sTemp: string;
begin
  sTemp := self.Content;
  //Get the multipart header

  //Get the multipart content-type

  //if the content type is application/www-form-urlencoded

    //add the vars as parameters


end;
//------------------------------------------------------------------------------
procedure TMotherShipWebRequest.Default(sParamName, sDefaultValue: string);
begin
  if not HasParam(sParamName) then
    params[sParamName] := sDefaultValue;

end;
//------------------------------------------------------------------------------
destructor TMotherShipWebRequest.destroy;
begin
  Fheader.free;
  FParams.free;
  FParamNames.free;
  FRequestContent.free;
  FParamSources.free;
  inherited;
end;
//------------------------------------------------------------------------------
procedure TRequestInfo.ForceInit;
begin
    request.AddParam('accountid', '0', pcCookie);
  self.SessionId := 0;
  request.AddParam('sessionid', self.SessionHash, pcCookie);

end;

function TMotherShipWebRequest.GetDocument: string;
//WARNING!: This function may be read by other threads. 
begin
  LockRead;
  try
    Result := FDocument;
  finally
    UnlockRead;
  end;
end;

function TMotherShipWebRequest.GetDocumentExt: string;
begin
  LockString;
  try
    if FDocumentExt = '' then
      FDocumentExt := ExtractFileExt(FDocument);

    result := FDocumentExt;
    uniquestring(result);
  finally
    UnlockString;
  end;

end;
//------------------------------------------------------------------------------

function TMotherShipWebRequest.GetFullURL: string;
begin
  result := copy(document, 2, length(document))+'?'+RebuildInlineParameters;
end;

function TMotherShipWebRequest.GetOriginalDocument: string;
begin
  Lock;
  try
    result := FOriginalDocument;
    UniqueString(result);
  finally
    Unlock;
  end;
end;

function TMotherShipWebRequest.GetParamCount: integer;
begin
  result := FParams.count;

end;
//------------------------------------------------------------------------------
function TMotherShipWebRequest.GetParamNames(idx: integer): string;
begin
  result := FParamNames[idx];
end;
//------------------------------------------------------------------------------
function TMotherShipWebRequest.GetParamNamesByPatternMatch(sLeftMatch: string;
  idx: integer): string;
var
  iLength, icount, t: integer;

begin
  icount := 0;
  sLeftMatch := lowercase(sLeftMatch);
  iLength := length(sLeftMatch);
  result := '';
  for t:= 0 to paramcount-1 do begin
    if copy(ParamNames[t], 1, iLength) = sLeftMatch then begin
      if icount = idx then begin
        result := paramnames[t];
        exit;
      end;
      inc(icount);
    end;
  end;

  Raise Exception.create('Pattern match not found.  Index: '+ inttostr(iCount)+' Pattern: '+sLeftMatch);

end;

function TMotherShipWebRequest.GetParamPatternCount(sLeftMatch: string): integer;
var
  t: integer;
begin
  result := 0;
  sLeftMatch := lowercase(sLeftMatch);
  for t:= 0 to paramcount-1 do begin
    if copy(paramnames[t], 1, length(sLeftMatch)) = sLeftMatch then
      inc(result);
  end;


end;
//------------------------------------------------------------------------------
function TMotherShipWebRequest.GetParams(sName: string): string;
var
  i: integer;
begin
  sName := lowercase(sName);
  i := FParamNames.indexof(sName);
  if i = -1 then
    raise EMissingInputValue.create(sName);

  result := FParams[i];
end;
//------------------------------------------------------------------------------
function TMotherShipWebRequest.GetParamsByIndex(idx: integer): string;
begin
  result := FParams[idx];
end;
//------------------------------------------------------------------------------
function TMotherShipWebRequest.GetParamsByPatternMatch(sLeftMatch: string;
  idx: integer): string;
//description: Finds the IDXth parameter matching the left side of sLeftMatch
//(case insensitive)
var
  iLength, icount, t: integer;

begin
  icount := 0;
  sLeftMatch := lowercase(sLeftMatch);
  iLength := length(sLeftMatch);
  result := '';
  for t:= 0 to paramcount-1 do begin
    if copy(ParamNames[t], 1, iLength) = sLeftMatch then begin
      if icount = idx then begin
        result := paramsbyIndex[t];
        exit;
      end;
      inc(icount);
    end;
  end;

  Raise Exception.create('Pattern match not found.  Index: '+ inttostr(iCount)+' Pattern: '+sLeftMatch);

end;

function TMotherShipWebRequest.GetParamSources(idx: integer): TParamSource;
begin
  result := TParamSource(FParamSources[idx]);
end;
//------------------------------------------------------------------------------
function TMotherShipWebRequest.GetReferer: string;
begin
  if self.HasParam('referer') then
    result := self['referer']
  else
    result := '';

end;



//------------------------------------------------------------------------------
function TMotherShipWebRequest.GetUserAgent: string;
begin
  result := '';
  if HasParam('User-Agent') then
    result := params['User-Agent'];

end;

function TMotherShipWebRequest.HasParam(sName: string): boolean;
//Returns whether or not a parameter with the specific name is defined
begin
  result := (Fparamnames.indexof(sName) > -1)
end;
//------------------------------------------------------------------------------
function TMotherShipWebResponse.GetVarCount: integer;
begin
  result := FVarNames.count;

end;
//------------------------------------------------------------------------------
function TMotherShipWebResponse.GetVarpool(sVarName: string): variant;
var
  idx: integer;
begin

  //Get the index
  idx := FVarNames.IndexOf(sVarName);

  //if found then
  if idx >=0 then
    result := FVarValues[idx]
  else begin
    if Assigned(VArPoolExtender) then begin
      if varpoolextender.IsDefined(sVarNAme) then begin
        result := varpoolextender.LoadVar(sVarName);
      end else begin
        raise EScriptVarError.create('Variable '+sVarName+' needed but undefined');
      end;
    end else begin
      raise EScriptVarError.create('Variable '+sVarName+' needed but undefined');
    end;
  end;


end;
function TMotherShipWebResponse.GetVArPoolExtender: IVarpoolVarExtender;
begin
  if FExtenderStack.count = 0 then begin
    result := nil;
  end
  else begin
    result := FExtenderStack[FExtenderStack.count-1] as IVarPoolVarExtender;

  end;
end;

//------------------------------------------------------------------------------
procedure TMotherShipWebResponse.SetVarPool(sVarName: string; const Value: variant);
//Sets a dynamic variable in the var pool (settter for VarPool property)
var
  idx: integer;
begin
  bNeedsProcessing := true;

  //Get the index of the variable in question
  idx := FVarNames.IndexOf(sVarName);

  //Change the value of the variable IF FOUND
  if not (idx<0) then begin
    FVArValues[idx] := Value;
  end
  //otherwise add it and set the value
  else begin
    FVarNames.Add(sVarName);
    FVarValues.Add(Value);
  end;
end;

procedure TMotherShipWebResponse.SetVArPoolExtender(
  const Value: IVarpoolVarExtender);
begin
  if value = nil then begin
    if FExtenderStack.count > 0 then begin
      FExtenderStack.delete(FExtenderstack.count-1);
    end;
  end else begin
    FExtenderStack.add(value);
  end;


end;

{ TMotherShipWebResponse }
//------------------------------------------------------------------------------
constructor TMotherShipWebResponse.Create;
//Constructor
begin
  inherited;
  FRangeEnd := -1;
  FExtenderStack := TInterfaceList.create;
  FDebugLog := TStringList.create;
  FOnResponseSent:= nil;
  FSendTime := 0;
  FAuthHeaders:= TStringList.create;


  FConnection := rqcAuto;
  FFramed := false;
  FContentLength := -1;
  FContent := TStringList.create;
  FcontentStream := nil;
  FObjectPool := nil;
  FContentType := 'text/html';
  FResultCode := 200;
  FObjectPool := TStringList.create;
  FVarNames := TStringList.create;
  FVarValues := TVariantList.create;
  FCookieNames := TStringList.create;
  FCookieValues := TStringList.create;
  FXMLPool := TStringList.create;
  FDOCache := nil;
  FDoSendChunk := nil;
  FDoSendHeader := nil;
  FHeaderSent := false;
  FNoDebug:= false;
  FRequestInfo := nil;
  FcopiedWithOldCache:=false;
  FDeleteFile := '';
	FDeleteTempFile := '';

end;

//------------------------------------------------------------------------------
destructor TMotherShipWebResponse.destroy;

begin

  FauthHeaders.free;
  FContentStream.free;
  FContentStream := nil;
  FVarValues.free;
  FVarNames.free;
  FObjectPool.free;
  FContent.free;
  FCookieNames.free;
  FCookieValues.free;
  FExtenderStack.free;

  EmptyXMlPool;
  FXMLPool.free;
//  FContentstream.free;
  if (FDOCache <> nil) and (not CopiedWithOldCache) then begin
    DOCM.FreeCache(FDOCache);
  end;

  inherited;

  try
    if FDeleteFile <>'' then begin
      DeleteFile(FDeleteFile);
    end;
    if FDeleteTempFile <> '' then begin
      DeleteFile(FDeleteTempFile);
    end;
  except
  end;

  FDebugLog.free;
end;

//------------------------------------------------------------------------------
procedure TMotherShipWebResponse.ProcessDynamicVariables();
var
  nullmethod: TProgressCallbackProcedure;
begin
  nullmethod := nil;
  ProcessDynamicVariables(false, nullmethod);
 end;

procedure TMotherShipWebResponse.ProcessDynamicVariables(progress_callback: TProgressCallbackProcedure);
begin
  ProcessDynamicVariables(false, progress_callback);
end;


procedure TMotherShipWebResponse.ProcessDynamicVariables(bAllowAlternateContentTypes: boolean ; progress_callback: TProgressCallbackProcedure);
//Replaces all variables marked with [[[varname]]] in the content with
//variables from the variable pool.
var
  t: integer;
begin


  //fix script problems that might occur if session ID is 0 or -1
  if HasVar('sessionid') then begin
    if VarPool['sessionid'] = '0' then begin
      Varpool['sessionid'] := Inttohash([INVALID_DATATIER, INVALID_DATATIER],0);     end;// else
      //Varpool['sessionid'] := Inttohash([self.RequestInfo.DTID, self.RequestInfo.ISRNDTID], hashtoint(Varpool['sessionid']));
  end else begin
    Varpool['sessionid'] := Inttohash([INVALID_DATATIER, INVALID_DATATIER],-1);
    self.Request.AddParam('sessionid', '-1', pcCookie);
  end;

  if not bNeedsProcessing then begin
    exit;
  end;

  if (not bAllowAlternateContentTypes) and (not (lowercase(copy(ContentType,1,4)) = 'text')) then
    exit;

//  if lowercase(contenttype) <> 'text/html' then
//    exit;

//  contentlength := -1;

  content.text := stringReplace(content.text, #01, '[[[', [rfReplaceAll]);

  ReplaceEscSequences(self.RequestInfo, Content, progress_callback);
  if lowercase(contenttype) = 'text/html' then
  begin
    ProcessHTML;
  end;
//  for t:= content.count-1 downto 0 do begin
//    //content[t] := Trim(StringReplace(content[t], '<!---->', '', [rfReplaceAll]));
////    content[t] := stringReplace(content[t], #9, '', [rfReplaceAll]);
//    if content[t] = '' then
//      content.delete(t);

//  end;

  contentlength := length(requestinfo.response.content.text);
  bNeedsProcessing := false;
end;

//------------------------------------------------------------------------------
procedure TMotherShipWebResponse.SetupDefaultVarPool(request: TMotherShipWebRequest);
//This sets up a default Auto-VarPool from the parameters passed through an
//HTTP request represented by request:TMotherShipWebRequest;
var
  t: integer;
begin
  //raise exception if this is called without a corresponding request partner object
  if request = nil then
    raise Exception.create('Invalid class instance of TMotherShipWebRequest passed to TMotherShipWebResponse.SetupDefaultVarPool');

  //morph the referer in case someone tries to use it as the target of the
  //next page
  if request.hasparam('referer') then begin
    if not (lowercase(request.DocumentExt) = '.asp') or (lowercase(request.DocumentExt) = 'asp') then
      request.AddParam('referer', MorphURL(request['referer']), request.paramsources[request.IndexOfParam('referer')]);
  end;

  for t:= 0 to request.ParamCount-1 do begin
    VarPool[request.ParamNames[t]] := request.ParamsByIndex[t];
  end;

  VarPool['document'] := copy(request.document, 2, length(request.document));

end;


function TRequestInfo.GetDoTransactions: boolean;
begin
  Result := FDoTransactions and (FServer <> nil);
end;

function TRequestInfo.GetDTID: integer;
var
  iAccountID: integer;
begin
//  AuditLog('Selecting Data Tier');
  //if we couldn't find via DTID then try by accountID
  if (FDTID = INVALID_DATATIER) or (FDTID= 254)then begin
//    AuditLog('No Data Tier found in Session ID Scanning for account');
    if request.HasParam('accountid') then begin
      iAccountID := strtoint(request['accountid']);
      if iAccountid = 1 then
        FDTID := 0
      else begin
//        AuditLog('Entering DOSV Select Function');
        FDTID := 0;
//        AuditLog('DOSV Selected: '+ inttostr(FDTID));
      end;
      request['sessionid'] := IntToHash([FDTID, FISRNDTID], sessionid);
    end;
  end;

  result := FDTID;

end;



function TRequestInfo.GetHTTPClient: THTTPClient;
begin
  if not assigned(FHTTPClient) then begin
    FHTTPClient := THTTPClient.create;
    FHTTPClient.DownloadLinks := false;
  end;

  result := FHTTPclient;


end;

function TRequestInfo.GetServer: IServerInterface;
begin

  if DTID = 255 then
    DTID := 0;

  if FServer = nil then begin
    if DoTransactions then
      FServer := DOSVpool[DTID].NewServer
    else begin
      raise exception.create('only transactional servers allowed');
    end;

//      FServer := DOSVpool[DTID].NewServer;

    LoadVars;
  end;

  result := FServer;




end;

function TRequestInfo.GetSessionHash: string;
begin
    result := InttoHash([FDTID, FISRNDTID], sessionid);

end;

function TRequestInfo.GetSessionID: integer;
begin
  result := FSessionID;
end;

function TRequestInfo.GetThreadID: integer;
begin
  Lock;
  try
    Result := FThreadID;
  finally
    Unlock;
  end;
end;

function TRequestInfo.InitDTID: integer;
begin
  if request.HasParam('sessionid') then begin
    result := HashToDataTierID(request['sessionid'], DT_INDEX_PWLN);
  end else
    result := INVALID_DATATIER;

end;

procedure TRequestInfo.InitializeEngine;
begin
  self.bInitialized := true;


  //convert 'key' param to 'sessionid' if session id not provided
  if request.hasParam('key') and (not request.hasParam('sessionid')) then
    request['sessionid'] := request['key'];

  //convert 'pwlnacocuntnumber' param to 'accountid'
  if request.hasParam('pwlnaccountnumber')  then
    request['accountid'] := request['pwlnaccountnumber'];

  //fake a sessionID if not provided
  if not request.hasparam('sessionid') then begin
    request['sessionid'] := InttoHash([INVALID_DATATIER, INVALID_DATATIER], 0, 0);
  end;

  //windows.beep(800,100);
  response.SetupDefaultVarPool(request);

  try
    if request.HasParam('accountid') then
       strtoint(request['accountid']);
  except
    request.RemoveParam('accountid');
  end;

  //allocate a data tier to handle stuff for this account;
  DTID := HashToDataTierID(request['sessionid'], DT_INDEX_PWLN);
  request.AddPAram('sessionid', self.SessionHash, pcInline);

  //windows.beep(1200,100);

//  AuditLog('Entering InitDTID');
  InitDTID;
//  AuditLog('Done InitDTID');

  //windows.beep(1600,100);

  //allocate a cache for Data objects
//  DOSV.CacheManager.AllocateCAche(FDOCache, request.Sessionid, cbSmall);
  bInitialized := true;



end;


function TRequestInfo.LoadVar(sName: string; iDefault: integer): integer;
begin
  result := strtoint(LoadVar(sName, inttostr(iDefault)));
end;


function TRequestInfo.LoadVar(sName: string; rDefault: real): real;
begin
  result := strtofloat(LoadVar(sName, floattostr(rDefault)));
end;

function TRequestInfo.LoadVar(sName, sDefault: string): string;
var
  obj: TDataObject;
  t: integer;
begin
  obj := response.DOCache.GetExistingObject('TdoSessionVar', vararrayof([sessionid,lowercase(sName)]), 0,0);
  if obj <> nil then begin
    result := obj['varvalue'].AsString;
  end;

  self.server.LazyQueryMap(self.Response.DOCache,  obj, 'SELECT * from session_vars where sessionid='+inttostr(sessionid), sessionid, 10000,'TdoSessionVars', sessionid, nil, 'TdoSessionVar', 2);

  result := sDefault;
  for t:= 0 to obj.ObjectCount-1 do begin
    if lowercase(obj.obj[t].token.params[1]) = lowercase(sName) then begin
      result := obj.obj[t]['varValue'].AsString;
    end;
  end;
end;

procedure TRequestInfo.LoadVars;
var
  obj, obj2: TDataObject;
  t: integer;
begin
  try
    if sessionid <= 0 then
      exit;

    if not self.server.LazyQueryMap(self.Response.DOCache,  obj, 'SELECT * from session_vars where sessionid='+inttostr(sessionid), sessionid, 10000,'TdoSessionVars', sessionid, nil, 'TdoSessionVar', 2)
    then
      exit;

    for t:= 0 to obj.ObjectCount-1 do begin
      request.Default(obj.obj[t]['varName'].AsString,obj.obj[t]['varValue'].AsString);
  //    request.AddParam(obj.obj[t]['varName'].AsString,obj.obj[t]['varValue'].AsString, pcCookie);
    end;

    if self.server.LazyQueryMap(self.Response.DOCache,  obj2, 'SELECT user_vars.* from session join user_vars on session.userid=user_vars.userid where sessionid='+inttostr(sessionid), sessionid, 10000,'TdoUserVars', sessionid, nil, 'TdoUserVar', 2) then
    for t:= 0 to obj2.ObjectCount-1 do begin
      request.Default(obj2.obj[t]['varName'].AsString,decodewebstring(obj2.obj[t]['varValue'].AsString));
  //    request.AddParam(obj.obj[t]['varName'].AsString,obj.obj[t]['varValue'].AsString, pcCookie);
    end;

  finally
    self.SetupDefaultVarPool;
  end;

end;


procedure TRequestInfo.SaveVars;
var
  obj: TDataObject;
  t: integer;
  sValues: string;
begin
  if sessionid = 0 then
    exit;

  server.UpdateQuery(response.docache, 'delete * from session_vars where sessionid='+inttostr(sessionid), sessionid);

  sValues := '';
  for t:= 0 to self.Response.VarCount-1 do begin

    sValues := sValues + '('+inttostr(sessionid)+',"'+
                   response.VarNames[t]+'","'+
                   response.VarNames[t]+'","'+
                   response.VarByIndex[t]+'")';

    if t < self.Response.varcount-1 then
      sValues := sValues + ',';

//    SaveVar(response.VarNames[t], response.VarByIndex[t]);
  end;

  server.NoTransupdateQuery(response.docache, 'INSERT INTO SESSION_VARS Values'+sValues, sessionid);


end;


function TRequestInfo.Pop: TrequestInfo;
begin
  result := FPopto;
  self.FPopto := nil;
  self.free;
end;

function TRequestInfo.ProcessQuery(sQuery: string): string;
var
  s: string;
  sT: string;
  b: boolean;
  cl: int64;
begin
  if pos('[[[',sQuery) < 1 then begin
    result := sQuery;
    exit;
  end;

  b := self.response.NeedsProcessing;
  sT := self.response.ContentType;
  s := self.response.content.text;
  cl := self.Response.ContentLength;
  self.response.contenttype := 'text/plain';
  self.response.content.text := sQuery;
  self.response.ProcessDynamicVariables(true);
  result := self.response.content.text;
  self.response.content.text := s;
  self.response.contentType := sT;
  self.response.NeedsProcessing := b;
  self.response.ContentLength := cl;



end;

function TRequestInfo.Push: TRequestInfo;
begin
  result := TRequestInfo.copycreate(self);
  result.FPopto := self;

  result.ProgressHook := self.ProgressHook;

end;

procedure TRequestInfo.Rollback;
begin
  if assigned(self.FServer) then
    server.Rollback;

end;

procedure TRequestInfo.SaveVar(sName, sValue: string);
var
  obj: TDataObject;
  t: integer;
  bFound: boolean;
begin
  if sessionid = 0 then
    exit;

  self.server.LazyQueryMap(self.Response.DOCache,  obj, 'SELECT * from session_vars where sessionid='+inttostr(sessionid), sessionid, 10000,'TdoSessionVars', sessionid, nil, 'TdoSessionVar', 2);

  bFound := false;
  for t:= 0 to obj.ObjectCount-1 do begin
    if lowercase(obj.obj[t].token.params[1]) = lowercase(sName) then begin
      obj.obj[t]['VarValue'].AsString := sValue;
      server.NoTRansUpdateQuery(response.DOCache,'UPDATE session_vars set varvalue="'+sValue+'" where varname="'+sName+'" and sessionid='+inttostr(sessionid), sessionid);
      bFound := true;
    end;
  end;

  if not bFound then begin
    server.NoTRansUpdateQuery(response.DOCache,'DELETE from session_vars where varname="'+sName+'" and sessionid='+inttostr(sessionid), sessionid);
    server.NoTransUpdateQuery(response.DOCache,'INSERT into session_vars values ('+inttostr(sessionid)+',"'+lowercase(sName)+'","'+lowercase(sName)+'","'+sValue+'")', sessionid);
  end;

  request[sName] := sValue;
  response.varpool[sName] := sValue;

end;
procedure TRequestInfo.SetDTID(const Value: integer);
begin
  FDTID := Value;

  rqMan.UpdateDTID(self, value);

end;

procedure TRequestInfo.SetHTTPClient(const Value: THTTPClient);
begin
  if assigned(FHTTPClient) then
    raise exception.create('HTTPClient already assigned');
  FHTTPClient := value;


end;

procedure TRequestInfo.SetISRNDTID(const Value: integer);
begin
  FISRNDTID := Value;
end;

procedure TRequestInfo.SetSessionHash(const Value: string);
begin
    FSessionID := HashToInt(Value);
  FDTID := HashToDataTierID(value, 0);
  FISRNDTID := HashToDataTierID(value, 1);
end;

procedure TRequestInfo.SetSessionID(const Value: integer);
begin
    FSessionID := value;
    response.varpool['sessionid'] := sessionhash;


end;

procedure TRequestInfo.SetupDefaultVarPool;
//A friendly helper function that calls the same function in the RESPONSE half
//of the class, passing the REQUEST half.
//See TMotherShipWebResponse.SetupDefaultVarPool for more information
begin
  response.SetupDefaultVarPool(request);

end;

function TRequestInfo.StealHTTPClient: THTTPClient;
begin
  result := FHTTPClient;
  FHTTPClient := nil;
end;

//------------------------------------------------------------------------------
function TMotherShipWebResponse.GetDataObjectCache: TDataObjectCache;
begin
  if FDOCache = nil then begin
    //assign threadvar DOSV
    //if DataTierID is part of session... then use the datatier ID in the session
    DOCM.AllocateCAche(FDOCache, REquestInfo.Sessionid, WebServerconfig.DataCenterID, RequestInfo.DTID, cbSmall);
    FDOCache.Server := self.RequestInfo.server;

  end;
  result := FDOCache;




end;

//------------------------------------------------------------------------------
function TMotherShipWebResponse.GetObjectPool(sName: string): TDataObject;
var
  idx: integer;
begin
  //Find index of name
  idx := FObjectPool.IndexOf(sName);

  //if name not found then EXCEPT
  if idx<0 then begin
    raise EObjectMissing.create('Object '+sName+' needed but undefined.');
  end
  //else result = object
  else
    result := TDataObject(FObjectPool.objects[idx]);

end;

function TMotherShipWebResponse.GetXMLPool(sName: string): string;
var
  idx: integer;
begin
  //Find index of name
  idx := FXMLPool.IndexOf(lowercase(sName));

  //if name not found then EXCEPT
  if idx<0 then begin
    raise exception.create('XML document '''+sName+''' needed but undefined.');
  end
  //else result = object
  else
    result := TXMLDOcument(FXMLPool.objects[idx]).Value;

end;

function TMotherShipWebResponse.GetXMLDocs(sName: string): TXmlDocument;
var
  idx: integer;
begin
  //Find index of name
  idx := FXMLPool.IndexOf(lowercase(sName));

  //if name not found then EXCEPT
  if idx<0 then begin
    raise exception.create('XML document '''+sName+''' needed but undefined.');
  end
  //else result = object
  else
    result := TXMLDocument(FXMLPool.objects[idx]);

end;


//------------------------------------------------------------------------------
procedure TMotherShipWebResponse.SetObjectPool(sName: string;
  const Value: TDataObject);
var
  idx: integer;
begin
  if Value = nil then
    Raise Exception.create('NIL object in object pool: '+sName);


  if not (value is TDataObject) then
    Raise Exception.create('Object in object pool is not a Data object');
  //Find index of name
  idx := FObjectPool.IndexOf(sName);

  //if name not found then add
  if idx<0 then begin
    FObjectPool.AddObject(sName, Value);
  end
  //else result = object
  else
    FObjectPool.objects[idx] := Value;


end;

procedure TMotherShipWebResponse.SetXMLPool(sName: string;
  const Value: string);
var
  idx: integer;
  doc: TXMLDocument;
begin

  //Find index of name
  idx := FObjectPool.IndexOf(lowercase(sName));

  //if name not found then add
  if idx<0 then begin
    doc := TXMLDocument.create;
    doc.value := VAlue;
    FXMLPOOL.AddObject(lowercase(sName), doc);
  end
  //else result = object
  else
    TXMLDocument(FXMLPool.objects[idx]).Value := Value;


end;


//------------------------------------------------------------------------------
function TMotherShipWebResponse.HasVar(sVarName: string): boolean;
begin
  result := FVarNames.IndexOf(sVarName)>=0;

end;

//------------------------------------------------------------------------------
function TMotherShipWebResponse.VarPoolStatus: string;
var
  sl: TStringList;
  obj: TDataObject;
  t: integer;
begin
  sl := TStringList.create;
  try
    sl.add('<table width="100%">');
    for t:= 0 to FObjectPool.count-1 do begin
      obj:= TDataObject(FObjectPool.objects[t]);
      sl.add('<TR>');
      sl.add('<td><B>'+FObjectPool[t]+'</B></td>');
      try
        sl.add('<td><B>'+obj.name+'</B></td>');
      except
        sl.add('<td>exception</td>');
      end;
      try
        sl.add('<td>'+obj.classname+'</td>');
      except
        sl.add('<td>exception</td>');
      end;
      try
        sl.add('<td>'+inttostr(obj.fieldcount)+'</td>');
      except
        sl.add('<td>exception</td>');
      end;
      try
        sl.add('<td>'+inttostr(obj.objectcount)+'</td>');
      except
        sl.add('<td>exception</td>');
      end;

      sl.add('</TR>');
    end;
    sl.add('</table>');

    //Variables
    sl.add('<table width="100%">');
    for t:= 0 to FVarNames.count-1 do begin
      sl.add('<TR>');
      try
        sl.add('<td><B>'+FVarNames[t]+'</B></td>');
        sl.add('<td><B>'+FVarValues[t]+'</B></td>');
      except
        sl.add('<td>exception</td>');
      end;
      sl.add('</TR>');
    end;
    sl.add('</table>');

    result := sl.text;
  finally
    sl.free;
  end;
end;

//------------------------------------------------------------------------------
procedure TMotherShipWebResponse.SendChunk;
begin
  SendChunk(FContent.count-1);
end;
//------------------------------------------------------------------------------
procedure TMotherShipWebResponse.SendChunk(endrow: integer);
var
  sTemp: string;
  t: integer;
  slTemp: TStringList;
begin
  if (EndRow<0) or (FContent.count=0) then
    exit;

  if not assigned(FDoSendChunk)then
    exit;

  if not(pos('ie', lowercase(request['User-Agent']))>0) and not(pos('explorer', lowercase(request['User-Agent']))>0) then
    exit;


  slTemp := TStringList.create;
  try
    //isolate the chunked area
    if EndRow<FContent.count-1 then begin
      for t:=EndRow to Fcontent.count-1 do begin
        slTemp.Add(Fcontent[t]);
      end;

      while ((FContent.count-1)>endRow) do begin
        if FContent.count>0 then
          Fcontent.delete(Fcontent.count-1);
      end;
    end;

    //Force transfer encoding to chunked
    self.TransferEncoding := 'chunked';

    if not HeaderSent then
      SendHeader;

    self.ProcessDynamicVariables;

    //Insert length as ASCII line at beginning of content
    sTemp := Fcontent.text;

    FContent.insert(0, inttohex(length(sTemp), 1));
    //Insert CRLF at end of content
    FContent.add('');

    //Make the callback to send the chunk*)
    DoSendChunk;

    //Clear the content for further encoding
    FContent.clear;

    for t:=0 to slTemp.count-1 do begin
      FContent.add(slTemp[t]);
    end;

  finally
    slTemp.free;
  end;
end;

//------------------------------------------------------------------------------
procedure TMotherShipWebResponse.SendHeader;
begin
  if not assigned(FDoSendHeader) then
    exit;

  DoSendHeader;

  FHeaderSent := true;

end;
//------------------------------------------------------------------------------
procedure TMotherShipWebResponse.SendFooter;
begin
  if not assigned(DoSendChunk) then
    exit;

  if not(pos('ie', lowercase(request['User-Agent']))>0) and not(pos('explorer', lowercase(request['User-Agent']))>0) then
    exit;



  if FContent.count >0 then
    SendChunk;


  FContent.insert(0,'0');
  FContent.add('');

  DoSendChunk;
end;


function TMotherShipWebRequest.HasParamWithValue(sName: string): boolean;
begin
  result := HasParam(sName) and (Params[sName] <> '');

end;

function TMotherShipWebRequest.IndexOfParam(sName: string): integer;
begin
  result := FParamNames.IndexOf(sName)
end;

function TMotherShipWebRequest.ParamPatternSum(sPattern: string): real;
var
  t: integer;
begin
  result := 0.0;
  for t:= 0 to parampatterncount[sPattern]-1 do begin
    result := result + strtofloat(paramsbypatternmatch[sPattern,t]);
  end;
end;

function TMotherShipWebRequest.PatternQuery(sPattern: string): TStringList;
var
  t: integer;
begin
  result := TStringList.create;
  for t := 0 to ParamPatternCount[sPattern]-1 do begin
    result.add(ParamsByPatternMatch[sPattern, t]);
  end;


end;

function TMotherShipWebRequest.RebuildInlineParameters: string;
//returns all Header/Inline parameters in URL encoded
//form.  Effectively used to rebuild the URL from which the
//request was originated when encapsulated in APIs that
//do not directly provide such information
var
  t: integer;
  sTemp : string;
begin
  self['sessionid'] := self.FRequestInfo.SessionHash;
  result := '';

  //cycle through all parameters
  for t:= 0 to self.ParamCount-1 do begin
    //if an inline parameter
    if self.ParamSources[t] in [pcInline] then begin

      //if NOT the first parameter add '&'
      if not (result = '') then
        result := result + '&';

      //encode the parameter
      sTemp := self.ParamNames[t]+'='+EncodeWebString(self.ParamsbyIndex[t]);

      //add the parameter to the result
      result := result + sTemp;
    end;
  end;
end;

function TMotherShipWebRequest.RebuildParameters: string;
//returns all Header/Inline parameters in URL encoded
//form.  Effectively used to rebuild the URL from which the
//request was originated when encapsulated in APIs that
//do not directly provide such information
var
  t: integer;
  sTemp : string;
begin
  result := '';

  //cycle through all parameters
  for t:= 0 to self.ParamCount-1 do begin
    //if an inline parameter
    if self.ParamSources[t] in [pcInline, pcContent] then begin

      //if NOT the first parameter add '&'
      if not (result = '') then
        result := result + '&';

      //encode the parameter
      sTemp := self.ParamNames[t]+'='+EncodeWebString(self.ParamsbyIndex[t]);

      //add the parameter to the result
      result := result + sTemp;
    end;
  end;
end;


procedure TMotherShipWebRequest.RemoveParam(sName: string);
var
  i: integer;
begin
  i := FParamNames.IndexOf(sName);
  if i >-1 then begin
    FParamNames.delete(i);
    FParams.delete(i);
    FParamSources.delete(i);

  end;
end;

procedure TMotherShipWebRequest.SetBinaryContentLength(iLength: integer);
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TMotherShipWebRequest.SetDocument(const Value: string);
var
  sTemp: string;
begin
//  rqMan.Lock;
  try
    Lock;
    try
//      rqMan.DeRegisterRequest(self.FRequestInfo);
      FDocument := Value;
      if FOriginalDocument = 'untitled' then
        FOriginalDocument := Value;

//      rqMan.RegisterRequest(self.FRequestInfo);

    finally
      Unlock;
    end;
  finally
//    rqMan.Unlock;
  end;




end;

procedure TMotherShipWebRequest.SetParams(sName: string; const Value: string);
var
  i: integer;
  pc: TParamsource;
begin
  i := FParamNames.IndexOf(sName);

  pc := pcGenerated;


  if i>-1 then begin
    pc := Tparamsource(FParamSources[i]);
    FParams.delete(i);
    FParamNames.delete(i);
    FParamSources.delete(i);
  end;

  AddParam(sName, Value, pc);

  self.RequestInfo.SetupDefaultVarPool;

end;

constructor TMotherShipWebResponse.CopyCreate(source: TMotherShipWebResponse; bNewCache: boolean = true);
var
  t: integer;
begin
  FExtenderStack := TInterfaceList.create;
  FDebugLog := TStringList.create;
  FContent := TStringList.create;
//  FNeedsProcessing := true;
  //nocopy

  FcontentStream := nil;
  //nocopy

  FContentType := 'text/html';
  FResultCode := 200;
  FObjectPool := TStringList.create;
  CopyStringList(source.FObjectPool, FObjectPool, true);

  FXMLPool := TStringList.create;


  for t:= 0 to source.FXMLPool.Count-1 do begin
    self.XMLPool[source.FXMLpool[t]] := Source.XMLPool[source.FXMLpool[t]];
  end;


  //CopyStringList(source.FXMLPool, FXMLPool, true);

  FVarNames := TStringList.create;
  CopyStringList(source.FVarNames, FVarNames, true);

  FVarValues := TVariantList.create;
  CopyVariantList(source.FVarValues, FVarValues);

  if bNewCache then begin
    FDoCache := nil;
    FCopiedWithOldCache := false;
  end
  else begin

    FCopiedWithOldCache := true;
    FDOCache := source.DoCache;
  end;


  FDoSendChunk := source.FDoSendChunk;
  FDoSendHeader := source.FDoSendHeader;

  FHeaderSent := false;
  FNoDebug:= false;


  FRequestInfo := nil;
  bNeedsProcessing := true;

  VarpoolExtender := source.VarPoolExtender;

end;
//------------------------------------------------------------------------------
function TMotherShipWebResponse.GetDOSV: TDataObjectServices;
//this is a cludge for correcting namespace and DOSV initialization issues
begin
  result := DOSVPool[RequestInfo.DTID];
end;

//------------------------------------------------------------------------------
procedure TMotherShipWebResponse.ImportVarPool(sImportString: string);
var
  sLeft, sRight: string;
  sName, sValue: string;
begin
  sRight := sImportString;
  while SplitString(sRight, '&', sLeft, sRight) do begin
    SplitString(sLeft, '=', sName, sValue);
    VarPool[sName] := sValue;
  end;

  SplitString(sLeft, '=', sName, sValue);
  VarPool[sName] := sValue;



end;
//------------------------------------------------------------------------------
function TMotherShipWebResponse.ExportVarPool: string;
var
  t: integer;
begin
  result := '';
  for t:= 0 to VarCount -1 do begin
    result := result + EncodeWebString(VarNames[t])+'='+EncodeWebString(VarByIndex[t])+'&';
  end;

end;
//------------------------------------------------------------------------------
function TMotherShipWebResponse.getVarByIndex(idx: integer): string;
begin
  result := FVarValues[idx]
end;
//------------------------------------------------------------------------------
function TMotherShipWebResponse.GetVarNames(idx: integer): string;
begin
  result := FVarNames[idx]
end;
//------------------------------------------------------------------------------
procedure TMotherShipWebResponse.SetVarByIndex(idx: integer;
  const Value: string);
begin
  FVarValues[idx] := value;
end;
//------------------------------------------------------------------------------
procedure TMotherShipWebResponse.SetVarNames(idx: integer; const Value: string);
begin
  FVarNames[idx] := value;
end;


procedure TMotherShipWebResponse.AddCookie(sName, sValue: string);
var
  iPos: integer;
begin
  iPos := FCookieNames.IndexOf(sName);
  //if a cookie by the name already exists then change existing value
  if iPos>-1 then begin
    FCookieValues[iPos] := sValue
  end
  //else, add a new cookie to the list
  else begin
    FcookieNames.add(sName);
    FcookieValues.add(sValue);
  end;
end;

procedure TMotherShipWebResponse.AddAuthHeader(sValue: string);
begin
  fauthHeaders.Add(sValue);
end;

procedure TMothershipWebResponse.AddCookie(sFullCookie: string);
var
  sLeft, sright, sCookie, sValue: string;
begin
  if SplitString(sFullCookie, '=', sCookie, sValue) then begin
    SplitString(sValue, ';', sValue, sRight);
    AddCookie(sCookie, sValue);
  end;




end;

function TMotherShipWebResponse.GetCookieCount: integer;
begin
  result := FCookieValues.count;
end;

function TMotherShipWebResponse.GetCookieNames(idx: integer): string;
begin
  result := FCookieNames[idx];
end;

function TMotherShipWebResponse.GetCookieValues(idx: integer): string;
begin
  result := FCookieValues[idx];
end;


procedure TMotherShipWebResponse.SetContentStream(const Value: TStream);
begin
  FContentStream := Value;
  if value <> nil then begin
    ContentLength := value.Size-value.position;
  end;


end;

function TMotherShipWebResponse.GetHasCache: boolean;
begin
  result := FDOCache <> nil;

end;

procedure TMotherShipWebResponse.Default(varName, varValue: variant);
begin
  if self.FVarNames.IndexOf(varNAme) < 0 then begin
    self.varpool[varName] := varValue;
  end;

end;

procedure TMotherShipWebResponse.EmptyXMLPool;
begin
  while FXMLPool.Count > 0 do begin
    TXMLDocument(FXMLPool.Objects[0]).free;
    FXMlPool.Delete(0);
  end;
end;

function TMotherShipWebResponse.GetDebugLog: TStringList;
begin
  result := FDebugLog;

end;

procedure TMotherShipWebResponse.MassageDebugStats;
type
  TStat = record
    time: int64;
    name: string;
  end;
var
  iFreq: int64;
  t: integer;
  stats: array of TStat;

  function IndexOfStat(var stats: array of TStat; name: string): integer;
  var
    t: integer;
  begin
    result := -1;
    for t:= Low(Stats) to High(stats) do begin
      if stats[t].Name = name then begin
        result := t;
      end;
    end;
  end;

  function GetMax(var stats: array of TStat): integer;
  var
    t: integer;
    i: integer;
  begin
    i := -1;
    for t:= Low(Stats) to High(stats) do begin
      if (i> -1) and (stats[t].time > stats[i].time) then begin
        i := t;
      end else if i = -1 then i:= t;
    end;

    result := i;
  end;


var
  sLeft, sRight: string;
  i: integer;
begin
  setLength(stats, 0);

  for t:= 0 to debuglog.count-1 do begin
    try
      if not SplitString(debuglog[t], '~', sLeft, sRight) then
        continue;

      i := IndexOfStat(stats, sRight);

      if i > -1 then begin
        stats[i].time := stats[i].time + strtoint64(sLeft);
      end else begin
        setLength(stats, length(stats)+1);
        i := High(stats);
        stats[i].time := strtoint64(sLeft);
        stats[i].Name := sRight;
      end;
    except
    end;
  end;

  debuglog.Clear;

  QueryPerformanceFrequency(iFreq);
  repeat
    i := GetMax(stats);
    if i >= 0 then begin
      debuglog.Add(floattostr(stats[i].time / iFreq)+': '+stats[i].name);
      stats[i].time := 0;
    end;

  until i<= 0;


end;

function TMotherShipWebResponse.HasObject(sName: string): boolean;
begin
  result := FObjectPool.IndexOf(sName) > -1;

end;

procedure TMotherShipWebResponse.ProcessDynamicVariables(
  bAllowAlternateContentTypes: boolean);
var
  nullmethod: TProgressCallbackProcedure;
begin
  nullmethod := nil;

  processdynamicvariables(bAllowAlternateContentTypes, nullmethod);

end;

procedure TRequestInfo.UpdateProgress(pos, max: integer; sMessage: string);
begin
  ProgressPos := pos;
  ProgressMax := max;
  if sMEssage <> '' then
    Status := sMessage;
  if assigned(ProgressHook) then begin
    try
      ProgressHook(pos,max,Status);
    except
    end;
  end;


end;

procedure TMothershipwebResponse.Setlocation(sLoc: string);
begin
  fLocation := sLoc;
  if sLoc <> '' then
    self.ResultCode := 302;

end;

procedure TMotherShipWebREsponse.ProcessHTML;
var
  compiler: TMothershipHTMLPromoter;
begin
  {$IFDEF NOHTMLPROCESS}
  exit;
  {$ENDIF}

  default('post_process_html', 'false');
  if not strtobool(VarPool['post_process_html']) then
    exit;


  compiler := TMothershipHTMLPromoter.create;
  try
    compiler.rqInfo := self.FRequestInfo;
    compiler.Feed := self.Content.text;
    compiler.Process;
    self.Content.text := compiler.Result;
  finally
    compiler.free;
  end;




end;



procedure TMotherShipWebResponse.RaiseVarPool;
var
  t: integer;
  sName: string;
  vValue: variant;
begin
  if self.RequestInfo.popto = nil then
    exit;

  for t:= 0 to self.VarCount -1 do begin
    sName := self.VarNames[t];
    vValue := self.VarByIndex[t];
    requestinfo.popto.response.VarPool[sName] := vValue;
  end;


end;

procedure TMotherShipWebResponse.RemoveVAr(sName: string);
var
  i: integer;
begin
  i := self.FVarNames.indexof(lowercase(sName));
  if i > -1 then begin
    FVArNames.Delete(i);
    FVarValues.delete(i);
  end;

end;

initialization


end.


