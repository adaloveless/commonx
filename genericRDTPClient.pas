unit GenericRDTPClient;
{$INCLUDE DelphiDefs.inc}
//SERVER SCENARIOS
//--SYSTEM THREAD is waiting for USER ACTIONS to complete and connection times out - receive AV

{$DEFine ENABLE_RDTP_COMPRESSION}
{x$DEFINE ALLOW_CALLBACKS}

{x$DEFINE PACKET_DEBUG_MESSAGES}
interface

uses
  tickcount, stringx,
  simplereliableudp, simplebufferedconnection, betterobject, sharedobject, packet,
  classes,
  sysutils, exceptions,
{$IFDEF WINDOWS}
  simplewinsock,
  windows,
{$ENDIF}
  simpleabstractconnection,
  managedthread, DtNetConst, networkbuffer, debug, typex, systemx;

const DEFAULT_TIMEOUT = 300000;

const ERC_SERVER_BUSY = 932;

type


  //TGenericConnectionType = TSimpleBufferedConnection<TSimpleWinsockconnection>;
{$DEFINE USE_RELIABLE_UDP}
{$IFNDEF USE_RELIABLE_UDP}
  TGenericConnectionType = TSimpleWinsockconnection;
{$ELSE}
  TGenericConnectionType = TSimpleReliableUDPClient;
{$ENDIF}


  TRDTPCallback = class;
  TRDTPProgressEvent = procedure (sMessage: string; sDebugLog: string; iPos, iMax: integer) of object;



  TGenericRDTPClient = class(TFakeLockQueuedObject)
  private
    fdebugTag: string;
    FLastSuccessfulFunction: string;
    FLastConnectionAttempt: ticker;
    FUseTCP: boolean;
    FUseTor: boolean;
    function GetDebugTag: string;
    procedure SetDebugTag(const Value: string);
    function GetSupportedPlatformoptions: cardinal;
    procedure SetPlatformOptions(const Value: cardinal);
    procedure SEtcontext(const Value: string);
  protected
    FTimeout: integer;
    Fconnection: TsimpleAbstractConnection;
    FDisconnectImmediately: boolean;
    Fhost, FEndpoint: string;
    FServiceName: string;

    FOnProgress: TRDTPProgressEvent;
    FCallback: TRDTPCallback;
    FContext: string;
    FParentContext: string;
    FConfig: string;
    FParentConfig: string;
    FLastError: string;
    FActiveTransactions: nativeint;
    FPlatformOptions: cardinal;


    function Connect: boolean;
    procedure OnHello;virtual;
    procedure NegotiateOptions;


    function InitConnectionClass: TSimpleAbstractConnection;

    function BeginTransact2(inPacket: TRDTPPacket; var outPacket: TRDTPPacket; sender: TObject; bForget: boolean  = false):boolean;
    function EndTransact2(inPacket: TRDTPPacket; var outPacket: TRDTPPacket; sender: TObject; bForget: boolean  = false):boolean;
    function Transact2(inPacket: TRDTPPacket; var outPacket: TRDTPPacket; sender: TObject; iTimeOutMS: integer = 900000; bForget: boolean  = false):boolean;

    procedure WritePacket(packet: TRDTPPacket);


    function ThreadTransact(var Packet: TRDTPPacket; bForget: boolean): boolean;overload;


  public
    property UseTCP: boolean read FUseTCP write FUseTCP;
    property UseTor: boolean read FUseTor write FUseTor;

    procedure InitPlatformOptions;
    constructor create(sHost, sEndpoint: string);reintroduce;virtual;
    procedure Detach;override;
    destructor Destroy;override;
    procedure Init;override;
    procedure IncTotalTransactions;
    procedure DecTotalTransactions;
    procedure IncActiveTransactions;
    procedure DecActiveTransactions;
    procedure IncRetryingTransactions;
    procedure DecRetryingTransactions;
    procedure IncAbortedTransactions;
    procedure DecAbortedTransactions;

    property Timeout: integer read FTimeout write FTimeout;

    function Transact(var Packet: TRDTPPacket; iTimeOutMS: integer = -1; slDebug: TStringList = nil; bForget: boolean = false): boolean;overload;
    function Transact(var Packet: TRDTPPacket; bForget: boolean): boolean;overload;
    procedure TransactThreadStart(Packet: TRDTPPacket; bForget: boolean);
    function IsThreadComplete: boolean;
    function GetThreadResult: TRDTPPacket;
    function IsInTransaction: boolean;


    procedure SetLastError(s: string);overload;
    procedure SetLastError(iCode: integer; s: string);overload;
    function GetLastErrorMessage: string;
    function GetLastErrorCode: integer;
    property DisconnectImmediately: boolean read FDisconnectImmediately write FDisconnectImmediately;

    procedure DoProgressClose;
    procedure DoProgress(prog: TRDTPPacket);
    property OnProgress: TRDTPProgressEvent read FOnProgress write FOnProgress;

    function DispatchCallback: boolean;virtual;

    property Callback: TRDTPCallback read FCallback write FCallback;
    property Context: string read FContext write SEtcontext;
    property ParentContext: string read FParentContext write FParentContext;
    property Config: string read FConfig write FConfig;
    property ParentConfig: string read FParentConfig write FParentConfig;
    property ServiceName: string read FServiceName write FServiceName;
    property Host: string read FHost write Fhost;
    property endPoint: string read FEndpoint write FEndpoint;

    procedure CheckConnected;
    property LastSuccessfulFunction: string read FLastSuccessfulFunction write FLastSuccessfulFunction;
    property DebugTag: string read GetDebugTag write SetDebugTag;
    function Connected: boolean;
    property LastconnectionAttempt: ticker read FLastConnectionAttempt write FLastConnectionAttempt;
    procedure Disconnect;
    property PlatformOptions: cardinal read FplatformOptions write SetPlatformOptions;
    property SupportedPlatformOptions: cardinal read GetSupportedPlatformoptions;
    function NeedPacket: TRDTPPacket;
  end;

  TRDTPCallback = class(TObject)
  private
    FRequest: TRDTPPacket;
    FResponse: TRDTPPacket;
    Client: TGenericRDTPCLient;
    procedure SetResponse(const Value: TRDTPPAcket);
    function GetResponse: TRDTPPacket;
  public

    destructor Destroy;override;
    property Request: TRDTPPacket read FRequest write FRequest;
    property Response: TRDTPPacket read GetResponse write SetResponse;
  end;

var
  RDTP_USE_TCP: boolean = false;
  RDTP_USE_SOCKS: boolean = false;

implementation


//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure CB_HANDLE_GetContext(cli: TGenericRDTPclient);
var
  res : string;
begin
  res := cli.Context;
  cli.Callback.response.AddString(res);
  Debug.consoleLog('server wanted context... sending:'+res);
  Debug.consoleLog('data count is now '+inttostr(cli.callback.response.datacount));

end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure CB_HANDLE_GetParentContext(cli: TGenericRDTPclient);
var
  res : string;
begin
  res := cli.ParentContext;
  if res = '' then
    raise exception.create('Server has requested the parent context of '+cli.ClassName+' but the parent context was blank. Make sure the client has Context AND ParentContext set.');
  cli.Callback.response.AddString(res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure CB_HANDLE_GetConfig(cli: TGenericRDTPclient);
var
  res : string;
begin
  res := cli.Config;
  if res = '' then
    raise exception.create('Server has requested the config of '+cli.ClassName+' but the config was not set.');
  cli.Callback.response.AddString(res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure CB_HANDLE_GetParentConfig(cli: TGenericRDTPclient);
var
  res : string;
begin
  res := cli.ParentConfig;
  if res = '' then
    raise exception.create('Server has requested the parent config of '+cli.ClassName+' but the parent config was not set.');
  cli.Callback.response.AddString(res);
end;

function TGenericRDTPClient.BeginTransact2(inPacket: TRDTPPacket;
  var outPacket: TRDTPPacket; sender: TObject;
  bForget: boolean): boolean;
var
  buffer : PByte;
  length : integer;
  pcHeader: PByte;
  iBytesRead: integer;
  iLength, t: integer;
  bufTemp: PByte;
  iBytesToRead: integer;
  tmBegin: cardinal;
  iBytesPerSec: cardinal;
  iBytesThisRead: cardinal;
//  bLegitResponse: boolean;
begin
  Lock;
  try
  try
//    bLegitResponse := false;
    {$IFDEF BEEPS} beeper.beep(100, 25);{$ENDIF}
    //initializevariables
    result := false;
    buffer := nil;
    length := 0;
    outPacket := nil;

    //disable message processing on forms
      //Connect
      CheckConnected;

      if not Connect then begin
  //      SetLastServerError(999, 'Unable to connect to server '''+self.HostName+''' endpoint '''+self.endpoint+''': '+GetLastServerErrorMessage);
              //raise ETransPortError.create('Unable to connect to Pathways server '''+self.HostName+''' endpoint '''+self.endpoint+''': '+GetLastServerErrorMessage);
        result := false;
        exit;
      end;

      //Define buffer to be sent
  //    buffer := inPacket.EncryptedBuffer.RawBuffer;
  //    length := inpacket.length;

      if FConnection = nil then
        raise ETransportError.create('Connection dropped unexpectedly');


      //Send
  {$IFDEF PACKET_DEBUG_MESSAGES}
      Debug.ConsoleLog('***********Client-To-Server REQUEST PAcklet');
  {$ENDIF}
      self.WritePacket(inPacket);
  //    FConnection.SendData(buffer, length);

      if FConnection = nil then
        raise ETransportError.create('Connection dropped unexpectedly');
  finally
    inPacket.free;
    inPacket := nil;
  end;
  except
    Unlock;
    raise;
  end;
end;

procedure TGenericRDTPClient.CheckConnected;
begin
  if assigned(FConnection) then
    self.FConnection.CheckConnected;

end;

function TGenericRDTPClient.Connect: boolean;
begin
  if connected then exit(true);
  LAstConnectionAttempt := GetTicker;



  if FConnection = nil then begin
    FConnection := InitConnectionClass;
  end;

  if Fconnection.Connected then
    exit(true);


  InitPlatformOptions;

  result := FConnection.connected;
  if not result then begin
//{$IFDEF PACKET_DEBUG_MESSAGES}
    Debug.ConsoleLog('Attempting to connect to '+self.FHost+':'+self.FEndpoint);
//{$ENDIF}
//    FConnection.free;
//    FConnection := InitConnectionClass;
    try
      Fconnection.Disconnect;
      result := FConnection.connect;
    except
      on E: Exception do begin
        Fconnection.Disconnect;
        raise;
      end;
    end;



    if result then
      OnHello;
  end;


end;

function TGenericRDTPClient.Connected: boolean;
begin
  result := Fconnection <> nil;
  if result then
    result := Fconnection.Connected;
end;

constructor TGenericRDTPClient.create(sHost, sEndpoint: string);
begin
  inherited create;
  FHost := sHost;
  FEndpoint := sEndpoint;
  FTIMEOUT := 300000*20;
  FCallback := TRDTPCAllback.create;
  FCallback.client := self;
  UseTCP := RDTP_USE_TCP;
  UseTor := RDTP_USE_SOCKS;
  Init;
end;

procedure TGenericRDTPClient.DecAbortedTransactions;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TGenericRDTPClient.DecActiveTransactions;
begin
  dec(FActiveTransactions);
//TODO -cunimplemented: unimplemented block
end;

procedure TGenericRDTPClient.DecRetryingTransactions;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TGenericRDTPClient.DecTotalTransactions;
begin

//TODO -cunimplemented: unimplemented block
end;

destructor TGenericRDTPClient.Destroy;
begin

  inherited;
end;

procedure TGenericRDTPClient.Detach;
begin
  if detached then exit;

  FCAllback.Free;
  FCallback := nil;

  if FConnection <> nil then begin
    try
      self.FConnection.Disconnect;
    except
    end;
    self.Fconnection.DetachAndFree;
  end;
  FConnection := nil;


  inherited;



end;

procedure TGenericRDTPClient.Disconnect;
begin
  if assigned(FConnection) then
    FConnection.Disconnect;

end;

function TGenericRDTPClient.DispatchCallback: boolean;
var
  iRQ: integer;
begin
{$IFNDEF ALLOW_CALLBACKS}
  raise ECritical.Create('callbacks are disabled/deprecated');
{$ENDIF}
  result := false;

  iRQ := callback.request.data[0];
  callback.request.seqseek(3);
  case iRQ of
    //GetContext
    $9000:
      begin
{$IFDEF PACKET_DEBUG_MESSAGES}
        Debug.ConsoleLog('Begin Inherited Client Callback handling of GetContext');
{$ENDIF}
        CB_HANDLE_GetContext(self);
{$IFDEF PACKET_DEBUG_MESSAGES}
        Debug.ConsoleLog('End Inherited Client Callback handling of GetContext');
{$ENDIF}
        result := true;
      end;
    //GetParentContext
    $9001:
      begin
{$IFDEF PACKET_DEBUG_MESSAGES}
        Debug.ConsoleLog('Begin Inherited Client Callback handling of GetParentContext');
{$ENDIF}
        CB_HANDLE_GetParentContext(self);
{$IFDEF PACKET_DEBUG_MESSAGES}
        Debug.ConsoleLog('End Inherited Client Callback handling of GetParentContext');
{$ENDIF}
        result := true;
      end;
    //GetConfig
    $9002:
      begin
{$IFDEF PACKET_DEBUG_MESSAGES}
        Debug.ConsoleLog('Begin Inherited Client Callback handling of GetConfig');
{$ENDIF}
        CB_HANDLE_GetConfig(self);
{$IFDEF PACKET_DEBUG_MESSAGES}
        Debug.ConsoleLog('End Inherited Client Callback handling of GetConfig');
{$ENDIF}
        result := true;
      end;
    //GetParentConfig
    $9003:
      begin
{$IFDEF PACKET_DEBUG_MESSAGES}
        Debug.ConsoleLog('Begin Inherited Client Callback handling of GetParentConfig');
{$ENDIF}
        CB_HANDLE_GetParentConfig(self);
{$IFDEF PACKET_DEBUG_MESSAGES}
        Debug.ConsoleLog('End Inherited Client Callback handling of GetParentConfig');
{$ENDIF}
        result := true;
      end;



  end;
end;

function TGenericRDTPClient.GetDebugTag: string;
begin
  lock;
  try
    result := FDebugTag;
  finally
    unlock;
  end;
end;

function TGenericRDTPClient.GetLastErrorCode: integer;
begin
  RESULT := 0;
end;

function TGenericRDTPClient.GetLastErrorMessage: string;
begin
  raise exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;



function TGenericRDTPClient.GetSupportedPlatformoptions: cardinal;
begin
{$IFDEF ENABLE_RDTP_COMPRESSION}
  result := PACKET_PLATFORM_OPTION_SUPPORTS_COMPRESSION;
{$ELSE}
  result := 0;
{$ENDIF}
end;

function TGenericRDTPClient.GetThreadResult: TRDTPPacket;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TGenericRDTPClient.IncAbortedTransactions;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TGenericRDTPClient.IncActiveTransactions;
begin
  inc(FActiveTransactions);
//TODO -cunimplemented: unimplemented block
end;

procedure TGenericRDTPClient.IncRetryingTransactions;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TGenericRDTPClient.IncTotalTransactions;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TGenericRDTPClient.Init;
begin
  inherited;

end;

function TGenericRDTPClient.InitConnectionClass: TSimpleAbstractConnection;
var
  s: string;
begin
  if length(FHost) > 6 then begin
    s := zcopy(Fhost, length(FHost)-6, 6);
    if lowercase(s) ='.onion' then begin
      UseTCP := true;
      UseTor := true;
    end;
  end;

  IF UseTCP then begin
    result := TSimpleWinsockconnection.create;
    if UseTor then
      tSimpleWinsockConnection(result).UseSocks := true;
  end
  else
    result := TGenericConnectionType.create;


  result.HostName := FHost;
  result.EndPoint := FEndpoint;
end;




procedure TGenericRDTPClient.InitPlatformOptions;
begin
{$IFDEF ENABLE_RDTP_COMPRESSION}
  PLatformOptions := PACKET_PLATFORM_OPTION_SUPPORTS_COMPRESSION;
{$ENDIF}
end;

function TGenericRDTPClient.IsInTransaction: boolean;
begin
  result := FActiveTransactions > 0;
end;

function TGenericRDTPClient.IsThreadComplete: boolean;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TGenericRDTPClient.NeedPacket: TRDTPPacket;
begin
  result := TRDTPPacket.create;
  result.PlatFormOptions := self.PlatformOptions;
end;

procedure TGenericRDTPClient.NegotiateOptions;
var
  packet: TRDTPPacket;
  bTRans: boolean;
begin
{$IFDEF PACKET_DEBUG_MESSAGES}
  Debug.ConsoleLog('********H*E*L*L*O*******');
{$ENDIF}
  packet := TRDTPPacket.create;
  try
    packet.AddLong($9999);
    packet.AddLong(0);
    packet.AddString('');//negotiate with the master dispatcher
    packet.AddString(FContext);
    packet.Addlong(SupportedPLatformOptions);//platform options that are desired from the server
//    if (FContext = '') then begin
//      raise exception.create('attempting to set a blank context on a remote server from '+self.ClassName);
//    end;

    bTrans := Transact(Packet,20000, nil, false);


    //server will return platform options that it supports using the input from the request
    if packet.result = false then
      raise EServerError.Create(packet.Message);

    if bTrans and (packet.result) and (packet.DataCount > (PACKET_INDEX_RESULT_DETAILS-1)) then begin
      PlatformOptions := packet.Data[PACKET_INDEX_RESULT_DETAILS];
    end else
      FPlatformOptions := 0;//older version of this protocol do not support platform options


  finally
    packet.free;
  end;
end;

procedure TGenericRDTPClient.OnHello;
var
  packet: TRDTPPacket;
begin
{$IFDEF PACKET_DEBUG_MESSAGES}
  Debug.ConsoleLog('********H*E*L*L*O*******');
{$ENDIF}
  packet := TRDTPPacket.create;
  try
    packet.AddLong($9000);
    packet.AddLong(0);
    packet.AddString(ServiceName);
    packet.AddString(FContext);
//    if (FContext = '') then begin
//      raise exception.create('attempting to set a blank context on a remote server from '+self.ClassName);
//    end;

    Transact(Packet,20000, nil, true);


  finally
    packet.free;
  end;

  NegotiateOptions;
end;

procedure TGenericRDTPClient.SetLastError(s: string);
begin

  FLastError := s;
end;

procedure TGenericRDTPClient.SEtcontext(const Value: string);
begin
  FContext := Value;
  debug.log('context set to: '+value);
end;

procedure TGenericRDTPClient.SetDebugTag(const Value: string);
begin
  lock;
  try
    FDebugTag := value;
  finally
    unlock;
  end;
end;

procedure TGenericRDTPClient.SetLastError(iCode: integer; s: string);
begin
  FLastError := s;
end;

procedure TGenericRDTPClient.SetPlatformOptions(const Value: cardinal);
begin
  FplatformOptions := SupportedPlatformOptions and Value;
end;

function TGenericRDTPClient.ThreadTransact(var Packet: TRDTPPacket;
  bForget: boolean): boolean;
begin

//TODO -cunimplemented: unimplemented block
  raise exception.create('unimplemented');
end;

function TGenericRDTPClient.Transact(var Packet: TRDTPPacket;
  bForget: boolean): boolean;
begin
  result := Transact(packet, FTIMEOUT, nil, bForget);
end;

function TGenericRDTPClient.Transact(var Packet: TRDTPPacket; iTimeOutMS: integer = -1; slDebug: TStringList = nil; bForget: boolean = false): boolean;
//Accepts a packet and sends the packet as a transaction to the data-tier.
//p: Packet: TRDTPPacket class.  Create a packet and pass it in.  It will be replaced with a response.
//p: iTimeOutMS: The amount of time you want to wait for a response.  Defaults to 300000ms.

var
  b: boolean;
  iCount : integer;
  inPacket, outPacket: TRDTPPacket;
  bWasBusy: boolean;
begin

  lock;
  try
    self.IncTotalTransactions;

    bWasbusy := false;

    result := false;

    {$IFDEF ANALYSIS}
    //JG: this logging is for analysis purposes
  //  AuditLog('DATATIER RQ, ' + 'ThrID' + inttohex(GetCurrentThreadID,4) + ', ' + ('0x'+ inttohex(packet.Data[0],4)) + #13#10,'Pg_Rq_Data', false);
    {$ENDIF}

    b := false;
    inPacket := packet;
    //create a packet class for the output data
    outPacket := nil;
    iCount := 0;
    try
      self.IncActiveTransactions;
      //this code was originally written to retry connections to the DT.  its
      //form is such that it can be easily changed to reallow the retry functionality

      repeat
        //Issue the transaction to the Data-Tier
        try

          //windows.beep(100,10);
          inc(iCount);
          if iTimeoutMS = -1 then
            iTimeoutMS := self.timeout;
          b := Transact2(inPacket, outPacket, self, iTimeOutMS, bforget);
          if bForget then begin
            result := b;
            exit;
          end;

        except
  {$IFDEF WINDOWS}
          on E:ESocketsDisabled do begin
            raise;
          end;
  {$ENDIF}
          on E:Exception do begin
            Debug.Log(self,'Exception from server: '+e.message,'error');
            if assigned(outPacket) then
              outPacket.DecryptedBuffer.SaveDebugPacket(e.Message);

            raise;
          end;
        end;

        //If the transaction was successful but.. the server claims it is too busy... raise exception
(*        if b and outPacket.IsResponse and (outpacket.ErrorCode=1) then begin
          //raise exception( formerly slept and retried) to stop processing on the page.

          if Not bWasbusy then
            self.IncRetryingTransactions;
          bWasBusy := true;

          {$IFNDEF FAILONBUSY}
          sleep(2000);
          {$ELSE}
            raise ENewException.create(ERC_SERVER_BUSY, 'Server Busy', outpacket.Message);
          {$ENDIF}

        end;*)

      //break out of loop when transcation fails entirely... or an error other than 1 is returned.
  //      if outPacket = nil then
  //        continue;
      until (iCount>0) or ((outPAcket <> nil) and (outpacket.IsResponse and (outpacket.ErrorCode<>1)));
    finally

      packet := outpacket;

      if not bForget then begin
        if (not assigned(packet)) then
        raise ETransportError.create('Server did not respond, packet is nil');

        if slDebug <> nil then
          slDebug.add(packet.GetDebugMessage);
      end;

      inPacket.free;
      self.DecActiveTransactions;

      if bWasBusy then
        self.DecRetryingTransactions;

    end;

    if b then begin
      //raise error if packet returned was not a response.
      if not bForget then begin
        if not packet.isresponse then
          raise exception.create('Server did not respond');

        result := b;

        //raise error contained in packet if it shows error
        if NOT packet.result then
          SetLastError(packet.ErrorCode, packet.Message);
      end else begin
        result := b;
      end;
    end else begin
      //raise "BUSY" exception if connection failed.
      self.IncAbortedTransactions;
      {$IFDEF FAILONBUSY}
        raise EBusyFailure.create(ERC_SERVER_BUSY, 'Server Busy', 'Data Server too busy (cf)');
      {$ENDIF}
      result := false;
    end;

    {$IFDEF DEBUG}
  (*    if DOSV.EnableDebugMessages then
        packet.ShowDebugMessage;*)
    {$ENDIF}
  finally
    unlock;
  end;
end;





procedure TGenericRDTPClient.WritePacket(packet: TRDTPPacket);
var
  iLen: integer;
begin
  packet.Encrypt;
  iLen := packet.packedlength;
{$IFDEF PACKET_DEBUG_MESSAGES}
  Debug.Log('Client Write Packet:'+packet.BufferAsString);
{$ENDIF}

{$IFDEF PACKET_DEBUG_MESSAGES}
  Debug.Log(packet.GetDebugMessage);
{$ENDIF}

  FConnection.CheckConnected;
//  Debug.Log('Sending Packet: '+commaize(iLen)+' bytes');
  FConnection.SendData(packet.EncryptedBuffer.FRealBuffer, iLen);
  FConnection.Flush;

end;

{ TRDTPTransactionThread }




procedure TGenericRDTPClient.DoProgress(prog: TRDTPPacket);
var
  sMessage: string;
  sDebugLog: string;
  iPos, iMax: integer;
begin
  if assigned(OnProgress) then begin
    prog.SeqSeek(0);
    sMessage := prog.SeqRead;
    sDebugLog := prog.SeqRead;
    ipos := prog.SeqRead;
    iMax := prog.SeqRead;
    OnProgress(sMessage, sDebugLog, iPos, iMax);
  end;

end;

procedure TGenericRDTPClient.DoProgressClose;
begin
  if assigned(OnProgress) then
    OnProgress('','',0,0);
end;


function TGenericRDTPClient.EndTransact2(inPacket: TRDTPPacket;
  var outPacket: TRDTPPacket; sender: TObject;
  bForget: boolean): boolean;
var
  buffer : PByte;
  length : integer;
  pcHeader: PByte;
  iBytesRead: integer;
  iLength, t: integer;
  bufTemp: PByte;
  iBytesToRead: integer;
  tmBegin: cardinal;
  iBytesPerSec: cardinal;
  iBytesThisRead: cardinal;
  tmActive: ticker;
//  bLegitResponse: boolean;
const
  iTimeoutMS = 300000;
begin
  result := false;
  try
//    bLegitResponse := false;
    if bForget then begin
      result := true;
      exit;
    end;

//    while not bLegitResponse do begin

      //Wait
      if not FConnection.WaitForData(iTimeOutMS) then
        raise ETransportError.create('Server did not respond in time');

      //READ
      tmActive := getticker;
      //allocate memory for header
      getmem(pcHeader, 18);
      //Read Header
      iBytesRead := 0;
      while iBytesRead < 18 do begin
        if FConnection = nil then
          raise ETransportError.create('Connection dropped unexpectedly');

        if not FConnection.WaitForData(iTimeOutMS) then
          raise exception.create('Server connection dropped');

        if FConnection = nil then
          raise ETransportError.create('Connection dropped unexpectedly');

        iBytesThisREad := FConnection.ReadData(pcHeader+iBytesRead, 18-iBytesRead);

        if iBytesThisRead = 0 then
          raise ETransportError.create('client disconnected');

        inc(iBytesRead, iBytesThisREAd);

        if GetTimeSince(tmActive) > itimeoutMS then
          raise ETransportError.create('Connection dropped during header read');

      end;

      //Get info on packet
      //Extract length from header
      iLength := ord(pcHeader[4])+(ord(pcHeader[5])*256)+(ord(pcHeader[6])*65536)+(cardinal(ord(pcHeader[7]))*(256*65536));

      //Allocate buffer based on size reported in header
      //note: old buffer must be referenced and destroyed externally
      reallocmem(pcHeader, iLength);
      buffer := pcHeader;

      tmActive := getticker;
      //read rest of stream
      iBytesRead := 18;
      while iBytesRead < iLength do begin


        //determine maximum single read based on bandwidthlimit
        iBytesToRead := iLength-iBytesRead;

        if iBytesToRead>8000 then iBytesToRead := 8000;
        //Incremental read
        bufTemp := buffer+iBytesRead;
        if Fconnection.WaitForData(1) then begin
          iBytesThisRead := FConnection.ReadData(bufTemp, iBytesToRead);
          if iBytesThisREad = 0 then begin
            raise ETransportError.create('Client disconnected');
          end;
          inc(iBytesRead, iBytesThisRead);
          tmActive := getticker;
        end;

        if GetTimeSince(tmActive) > itimeoutMS then
          raise ETransportError.create('Connection dropped during header read');
      end;


      //Reinitialize packet
      outPacket := NeedPacket;//TRDTPPacket.create;  NEW! Always get packets from the client, so that platformoptions can be SET
      outPacket.Initialize;

      //assign the new buffer to the packet
      outpacket.AssignBuffer(buffer, ilength);
      outPacket.Origin := poServer;
      if outpacket.PacketType = PACKET_TYPE_PROGRESS then begin
{$IFDEF PACKET_DEBUG_MESSAGES}
        Debug.Log('******************Progress Packet');
        Debug.Log(outpacket.getdebugmessage);
{$ENDIF}
        DoProgress(outpacket);//DONE 1: Make sure packet gets discarded
        outPacket.free;
        outPacket := nil;
      end else
      if outpacket.PacketType = PACKET_TYPE_CALLBACK then begin
        callback.Request := outpacket;
{$IFDEF PACKET_DEBUG_MESSAGES}
        Debug.Log('******************Callback REQUEST Packet');
        Debug.Log(outpacket.getdebugmessage);
{$ENDIF}

        try
          with callback do begin
            response.Origin := poServer;
            response.AddShort(request.data[0]);//echo requestid
            response.AddLong(request.data[1]);//echo sessionid

            response.AddBoolean(true);//result
            response.AddShort(0);//error code
            response.AddString('');//error message
          end;
          self.DispatchCallback;
          callback.Response.PacketType := PACKET_TYPE_CALLBACK_RESPONSE;
{$IFDEF PACKET_DEBUG_MESSAGES}
          Debug.Log('***********THIS IS A CALLBACK RESPONSE');
{$ENDIF}
          self.WritePacket(callback.response);
          //self.FConnection.SendData(callback.response.encryptedbuffer.FRealBuffer, callback.response.encryptedbuffer.length);
        finally

          callback.Request.free;
          callback.Request := nil;
          callback.Response := nil; //<<--Response is freed when property is unassigned
        end;
      end else begin

        DoProgressClose;
//        bLegitResponse := true;

{$IFDEF PACKET_DEBUG_MESSAGES}
        Debug.Log('******************Vanilla Response Packet');
        Debug.Log(outpacket.getdebugmessage);
{$ENDIF}

      end;

//    end;



    //Disconnect
    if FDisconnectImmediately then begin
      FConnection.disconnect;
      FConnection.free;
      FConnection := nil;
    end;


    result := true;

  {$IFDEF BEEP} beeper.beep(50, 25);{$ENDIF}
  finally
    Unlock;
  end;
end;

function TGenericRDTPClient.Transact2(inPacket: TRDTPPacket; var outPacket: TRDTPPacket; sender: TObject; iTimeOutMS: integer = 900000; bForget: boolean  = false):boolean;
var
  buffer : PByte;
  length : integer;
  pcHeader: PByte;
  iBytesRead: integer;
  iLength, t: integer;
  bufTemp: PByte;
  iBytesToRead: integer;
  tmBegin: cardinal;
  iBytesPerSec: cardinal;
  iBytesThisRead: cardinal;
//  bLegitResponse: boolean;
  iMarker: cardinal;
begin
  if iTimeoutMS = 0  then
    iTimeoutMS := DEFAULT_TIMEOUT;

  Lock;
  try
  //  bLegitResponse := false;
    {$IFDEF BEEPS} beeper.beep(100, 25);{$ENDIF}
    //initializevariables
    result := false;
    buffer := nil;
    length := 0;
    outPacket := nil;

    //disable message processing on forms
      try
      //Connect
      CheckConnected;
      if not Connect then begin
  //      SetLastServerError(999, 'Unable to connect to server '''+self.HostName+''' endpoint '''+self.endpoint+''': '+GetLastServerErrorMessage);
              //raise ETransPortError.create('Unable to connect to Pathways server '''+self.HostName+''' endpoint '''+self.endpoint+''': '+GetLastServerErrorMessage);

        result := false;
        exit;
      end;

      //Define buffer to be sent
  //    buffer := inPacket.EncryptedBuffer.RawBuffer;
  //    length := inpacket.length;

      if FConnection = nil then
        raise ETransportError.create('Connection dropped unexpectedly');


      //Send
  {$IFDEF PACKET_DEBUG_MESSAGES}
      Debug.Log('***********Client-To-Server REQUEST PAcklet');
  {$ENDIF}
      self.WritePacket(inPacket);
  //    FConnection.SendData(buffer, length);

      if FConnection = nil then
        raise ETransportError.create('Connection dropped unexpectedly');

      if bForget then begin
        result := true;
        exit;
      end;

  //    while not bLegitResponse do begin

        //Wait
        if not FConnection.WaitForData(iTimeOutMS) then
          raise ETransportError.create('Server did not respond in time '+inttostr(iTimeOutMS));

        //READ

        //allocate memory for header
        getmem(pcHeader, 18);
        //Read Header
        iBytesRead := 0;

  {$IFDEF OLDCODE}
        while iBytesRead < 18 do begin
          if FConnection = nil then
            raise ETransportError.create('Connection dropped unexpectedly');

          if not FConnection.WaitForData(iTimeOutMS) then
            raise exception.create('Server connection dropped');

          if FConnection = nil then
            raise ETransportError.create('Connection dropped unexpectedly');

          iBytesThisREad := FConnection.ReadData(pcHeader+iBytesRead, 18-iBytesRead);

          if iBytesThisRead = 0 then
            raise ETransportError.create('client disconnected');

          inc(iBytesRead, iBytesThisREAd);
        end;
  {$ENDIF}

        Fconnection.ReadData(pcHeader, 18, true);

        //Get info on packet
        //Extract length from header
        iMarker := ord(pcHeader[0])+(ord(pcHeader[1])*256)+(ord(pcHeader[2])*65536)+(cardinal(ord(pcHeader[3]))*(256*65536));
        if iMarker <> PACKET_MARKER then begin
          outPacket.Free;
          outPacket := nil;

          raise ETransportError.create('packet marker invalid found '+memorytohex(pcHeader, 18));
        end;

        if assigned(outPacket) then begin
          if outPacket.DataCount > 0 then
            LAstSuccessfulFunction := inttohex(outPacket.ResponseType,4);
        end;

        iLength := ord(pcHeader[4])+(ord(pcHeader[5])*256)+(ord(pcHeader[6])*65536)+(cardinal(ord(pcHeader[7]))*(256*65536));

        //Allocate buffer based on size reported in header
        //note: old buffer must be referenced and destroyed externally
        reallocmem(pcHeader, iLength);
        buffer := pcHeader;

        //read rest of stream
        iBytesRead := 18;
        while iBytesRead < iLength do begin


          //determine maximum single read based on bandwidthlimit
          iBytesToRead := iLength-iBytesRead;

          if iBytesToRead>8000 then iBytesToRead := 8000;
          //Incremental read
          bufTemp := buffer+iBytesRead;
          if Fconnection.WaitForData(1000) then begin
            iBytesThisRead := FConnection.ReadData(bufTemp, iBytesToRead);
            if iBytesThisREad = 0 then begin
              raise ETransportError.create('Client disconnected');
            end;
            inc(iBytesRead, iBytesThisRead);

          end;
        end;


        //Reinitialize packet
        outPacket := TRDTPPacket.create;
        outPacket.Initialize;

        //assign the new buffer to the packet
        outpacket.EncryptedBuffer.AssignBuffer(buffer, ilength);
        outPacket.Origin := poServer;
        if outpacket.PacketType = PACKET_TYPE_PROGRESS then begin
  {$IFDEF PACKET_DEBUG_MESSAGES}
          Debug.Log('******************Progress Packet');
          Debug.Log(outpacket.getdebugmessage);
  {$ENDIF}
          DoProgress(outpacket);//DONE 1: Make sure packet gets discarded
          outPacket.free;
          outPacket := nil;
        end else
        if outpacket.PacketType = PACKET_TYPE_CALLBACK then begin
          callback.Request := outpacket;
          outPacket := nil;
  {$IFDEF PACKET_DEBUG_MESSAGES}
          Debug.Log('******************Callback REQUEST Packet');
          Debug.Log(outpacket.getdebugmessage);
  {$ENDIF}

          try
            with callback do begin
              response.Origin := poServer;
              response.AddShort(request.data[0]);//echo requestid
              response.AddLong(request.data[1]);//echo sessionid

              response.AddBoolean(true);//result
              response.AddShort(0);//error code
              response.AddString('');//error message
            end;
            self.DispatchCallback;
            callback.Response.PacketType := PACKET_TYPE_CALLBACK_RESPONSE;
  {$IFDEF PACKET_DEBUG_MESSAGES}
            Debug.Log('***********THIS IS A CALLBACK RESPONSE');
  {$ENDIF}
            self.WritePacket(callback.response);
            //self.FConnection.SendData(callback.response.encryptedbuffer.FRealBuffer, callback.response.encryptedbuffer.length);
          finally

            callback.Request.free;
            callback.Request := nil;
            callback.Response := nil; //<<--Response is freed when property is unassigned
          end;
        end else begin

          DoProgressClose;
  //        bLegitResponse := true;

  {$IFDEF PACKET_DEBUG_MESSAGES}
          Debug.Log('******************Vanilla Response Packet');
          Debug.Log(outpacket.getdebugmessage);
  {$ENDIF}

        end;

  //    end;



      //Disconnect
      if FDisconnectImmediately then begin
        FConnection.disconnect;
        FConnection.free;
        FConnection := nil;
      end;


      result := true;

    finally
    end;
    {$IFDEF BEEP} beeper.beep(50, 25);{$ENDIF}
  finally
    Unlock;
  end;
end;


procedure TGenericRDTPClient.TransactThreadStart(Packet: TRDTPPacket;
  bForget: boolean);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

{ TRDTPCallback }

destructor TRDTPCallback.Destroy;
begin
  FRequest.free;
  FRequest := nil;
  FResponse.free;
  FResponse := nil;
  inherited;
end;

function TRDTPCallback.GetResponse: TRDTPPacket;
begin
  if not assigned(FResponse) then
    FResponse := TRDTPPacket.create;

  result := FResponse;
end;

procedure TRDTPCallback.SetResponse(const Value: TRDTPPacket);
begin
  if assigned(FResponse) then
    FResponse.free;
  FResponse := value;
end;

initialization
//  RDTP_USE_TCP := false;//globally initialized
//  RDTP_USE_SOCKS := false;

end.


