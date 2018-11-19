unit SimpleAbstractConnection;
{x$DEFINE VERBOSE COMMUNICATION LOG}
{$DEFINE READ_AHEAD}

interface
uses SysUtils, systemx, sharedobject, debug, numbers, typex, tickcount, signals, ringbuffer, commandprocessor;

type
  ETransportError = class(Exception);


  Tcmd_ConnectionConnector = class;//forward

//############################################################################
  TDebugEvent = procedure;
  TConnectionBufferState = record
    size: ni;
    consumed: ni;
    function PercentConsumed: single;
  end;
  TSimpleAbstractConnection = class(TSharedObject)
  //Description: This is the abstract base class that all types of connection
  //classes inherit from.  All connection classes have a hostname and enpoint,
  //and must implement the Connnect, Disconnect, Waitfordata, ReadData, and
  //Senddata procedures.
  private
    FOnData: TDebugEvent;
    FError: string;
    FTimeOut: cardinal;
    FdebugTag: string;
    temp: array[0..16383] of byte;
    FRequiresPolling: boolean;
    FPolledByReader: boolean;
    procedure SetHostName(const Value: string);
    function GetDebugTag: string;
    procedure SetDebugTag(const Value: string);




  protected
    rbReadAhead: TRingBuffer;

    FHostName: string;  //Identifies the computer to connected to
    FEndPoint: string;  //Identifies the PORT/SOCKET/PIPE or other kind of listener
    FBaudRate: ni;

    procedure DebugTagUpdated(s: string);virtual;
    function GetConnected: boolean;virtual;abstract;//<---------------------------------------------
    function DoReadData(buffer: pbyte; length: integer): integer;virtual;abstract;//<<------------------------------
    function DoSendData(buffer: pbyte; length: integer): integer;virtual;abstract;//<---------------------------------------------
    function GetBaudRate: integer;virtual;
    procedure SetBaudRate(const Value: integer);virtual;
    function HasLeftOvers: boolean;inline;
    function HasLeftOverSpace: boolean;inline;
    procedure SetEndPOint(const Value: string);virtual;
    function GetISDataAvailable: boolean;virtual;
  strict protected
    tmLastDebugTime: ticker;
    function CheckForData: boolean;
    function DoCheckForData: boolean;virtual;abstract;
    function DoWaitForData(timeout: cardinal): boolean;virtual;abstract;//<---------------------------------------------

    procedure DebugIfTime;
    function BufferStatusString: string;
    property PolledByReader: boolean read FPolledByReader write FPolledByReader;
    procedure UpdateBufferStatus;
  public
    Disconnecting: boolean;
    bufferState: TConnectionBufferState;
    evData: TSignal;
    constructor Create; override;
    destructor Destroy;override;
    property HostName: string read FHostName write SetHostName;
    property EndPoint: string read FEndPoint write SetEndPOint;

    function Connect: boolean;
    function DoConnect: boolean; virtual;abstract;//<---------------------------------------------
    procedure Disconnect;
    procedure DoDisconnect; virtual;abstract;//<---------------------------------------------


    function SendData(buffer: pbyte; length: integer; bSendAll: boolean = true): integer;
    function WaitForData(timeout: cardinal): boolean;



    property Connected: boolean read GetConnected;

    procedure Flush;virtual;
    function ReadData(buffer: pbyte; length: ni; bReadAll: boolean; iTimeOut: ticker = 0): ni;overload;//override DoReadDAta
    function ReadData(buffer: pbyte; length: ni): ni;overload;
    function GuaranteeReadData(buffer: pbyte; length: ni; iTimeOut: ticker): ni;


    property OnData: TDebugEvent read FOnData write FOnData;
    property Error: string read FError write FError;
    function CheckConnected: boolean;virtual;

    property BaudRate: integer read GetBaudRate write SetBaudRate;
    property Timeout: cardinal read FTimeOut write FTimeout;
    function IsConnected: boolean;
    property DebugTag: string read GetDebugTag write SetDebugTag;
    property RequiresPolling: boolean read FRequiresPolling write FRequiresPolling;

    function BeginConnect: Tcmd_ConnectionConnector;
    function EndConnect(c:Tcmd_ConnectionConnector): boolean;
    property IsDataAvailable: boolean read GetISDataAvailable;
    function CheckConnectedOrConnect: boolean;
  end;


  Tcmd_ConnectionConnector = class(TCommand)
  public
    c: TSimpleAbstractConnection;
    Cresult: boolean;
    procedure InitExpense;override;
    procedure DoExecute;override;
  end;






implementation

function TSimpleAbstractConnection.BeginConnect: Tcmd_ConnectionConnector;
begin
  result := Tcmd_ConnectionConnector.create;
  result.c := self;
  result.start;

end;

function TSimpleAbstractConnection.BufferStatusString: string;
begin
  result := 'leftOvers (ReadAhead): '+rbReadAhead.AvailableDataSize.tostring;
end;

function TSimpleAbstractConnection.CheckConnected: boolean;
begin
  try
    result := connected;
    if not connected then result := connect;
  except
    result := false;
  end;

end;


function TSimpleAbstractConnection.CheckConnectedOrConnect: boolean;
begin
  result := connected;
  if not result then
    result := connect;
end;

function TSimpleAbstractConnection.CheckForData: boolean;
begin
  result := false;
  Lock;
  try
    if DoCheckForData then begin
      signal(evData, true);
      result := true;
    end;
  finally
    Unlock;
  end;
end;

function TSimpleAbstractConnection.Connect: boolean;
begin
  Disconnecting := false;
  result := DoConnect;

end;

constructor TSimpleAbstractConnection.Create;
begin
  inherited;
  FHostName := '';
  FEndPoint := '';
  FOnData := nil;
  FError := '';
  evData := TSignal.create;
  rbReadAhead := TRingBuffer.create;
  rbReadAhead.Size := 65536;

end;


procedure TSimpleAbstractConnection.DebugIfTime;
begin
  if gettimesince(tmLastDebugTime) > 1000 then begin
//    debug.log(self.BufferStatusString);
    tmLastDebugTime := getticker;
  end;
end;

procedure TSimpleAbstractConnection.DebugTagUpdated(s: string);
begin
  //no implementation required
end;

destructor TSimpleAbstractConnection.Destroy;
begin
  rbReadAhead.free;
  rbReadAhead := nil;
  evData.free;
  inherited;
end;

procedure TSimpleAbstractConnection.Disconnect;
begin
  Disconnecting := true;
  DoDisconnect;
end;

function TSimpleAbstractConnection.EndConnect(
  c: Tcmd_ConnectionConnector): boolean;
begin
  c.WaitFor;
  result := c.cresult;
  c.free;
  c := nil;

end;

procedure TSimpleAbstractConnection.Flush;
begin
  //no implementation required
end;

function TSimpleAbstractConnection.GetBaudRate: integer;
begin
  //no implementation required
  result := FBaudRate;
end;

function TSimpleAbstractConnection.GetDebugTag: string;
begin
  Lock;
  try
    result := FDebugTag;
  finally
    Unlock;
  end;
end;

function TSimpleAbstractConnection.GetISDataAvailable: boolean;
begin
  if PolledByReader then
    CheckForData;
  result := evData.IsSignaled;
end;

function TSimpleAbstractConnection.GuaranteeReadData(buffer: pbyte;
  length: ni; iTimeOut: ticker): ni;
var
  pp: pbyte;
  togo: ni;
  justread: ni;
  tmStart: ticker;
begin
  pp := buffer;
  togo := length;
  tmStart := getticker;
  result := 0;
  while togo > 0 do begin
    justread := 0;
    if WaitforData(iTimeout) then begin
      justread := ReadData(pp, togo);
      if justread <=0 then
        raise ETransportError.Create(classname+' returned zero bytes');
    end;

    dec(togo, justread);
    inc(pp, justread);
    if justread > 0 then begin
      tmStart := getticker;
    end;
    if (iTimeOut > 0) and (gettimesince(tmStart) > iTimeOut) then begin
      raise ETransportError.Create(classname+' timeout during guaranteed read.');
    end;
  end;
  result := length;
end;

function TSimpleAbstractConnection.HasLeftOvers: boolean;
begin
  result := rbReadAhead.IsDataAvailable;
end;

function TSimpleAbstractConnection.HasLeftOverSpace: boolean;
begin
  result := rbReadAhead.SpaceAvailable > 0;
end;

function TSimpleAbstractConnection.IsConnected: boolean;
begin
  result := GetConnected;
end;

function TSimpleAbstractConnection.ReadData(buffer: pbyte; length: ni): ni;
var
  iJustread, iToRead: nativeint;
  tempread: ni;
begin
  result := 0;
  //if there are leftovers
  DebugIfTime;
  If HasLeftOvers then begin
    result := rbReadAhead.GetAvailableChunk(buffer, length);
    UpdateBufferStatus;
    exit;
  end;

  //if we didn't read anything
  //OR we didn't read enough (and the readall flag is set)
  if (result = 0) then begin
    //move stuff to temp
{$IFDEF READ_AHEAD}
    itoRead := lesserof(sizeof(temp), rbReadAhead.BufferSpaceAvailable);
{$ELSE}
    itoRead := lesserof(sizeof(temp), length);
{$ENDIF}
    tempread := doReadData(@temp[0], iToRead);
    if tempread > 0 then begin
      //pull what we need out of temp array
      result := lesserof(tempread, length);
      MoveMem32(buffer, @temp[0], result);
      //save whatever is left
      rbReadAhead.BufferData(@temp[result], tempread-result);
      UpdateBufferStatus;
    end
    else
      exit(tempread);
  end

  {$IFDEF VERBOSE COMMUNICATION LOG}
  Debug.Consolelog('Just Read '+inttostr(result)+' of '+inttostr(length)+' bytes: '+memorytohex(buffer, result));
  {$ENDIF}

end;

function TSimpleAbstractConnection.ReadData(buffer: pbyte; length: ni;
  bReadAll: boolean; iTimeOut: ticker = 0): ni;
begin
  if bReadAll then
    result := GuaranteeReadData(buffer, length, iTimeOut)
  else
    result := readData(buffer, length);
end;



function TSimpleAbstractConnection.SendData(buffer: pbyte; length: integer; bSendAll: boolean = true): integer;
var
  iSent: integer;
  iToSend: integer;
  iJustSent: integer;
begin
  iSent := 0;
  while iSent < length do begin
    iToSend := length-isent;
    ijustSent := DoSendData(@buffer[iSent], length-iSent);
    if (iJustSent=0) and (not Connected) then
      raise ETransportError.Create('Connection dropped during send.');
    inc(iSent, iJustSent);
    if not bSendAll then break;
  end;

  result := iSent;

  {$IFDEF VERBOSE COMMUNICATION LOG}
  Debug.log(self,'Just Sent '+inttostr(result)+' of '+inttostr(length)+' bytes: '+memorytohex(buffer, result),'');
  {$ENDIF}

end;

procedure TSimpleAbstractConnection.SetBaudRate(const Value: integer);
begin

  //raise Exception.create('unimplemented');
  FBaudRate := value;

end;

procedure TSimpleAbstractConnection.SetDebugTag(const Value: string);
begin
  Lock;
  try
    FDebugTag := value;
    DebugTagUpdated(value);
  finally
    Unlock;
  end;
end;

procedure TSimpleAbstractConnection.SetEndPOint(const Value: string);
begin
  FEndPoint := Value;
end;

procedure TSimpleAbstractConnection.SetHostName(const Value: string);
begin
  FHostName := Value;
end;

procedure TSimpleAbstractConnection.UpdateBufferStatus;
begin
  bufferState.size := rbReadAhead.Size;
  bufferstate.consumed := rbReadAhead.DataAvailable;
end;

function TSimpleAbstractConnection.WaitForData(timeout: cardinal): boolean;
begin
  if HasLeftOvers then begin
    result := true;
  end else
    result := DoWaitForData(timeout);
end;

{ Tcmd_ConnectionConnector }

procedure Tcmd_ConnectionConnector.DoExecute;
begin
  c.Connect;
end;

procedure Tcmd_ConnectionConnector.InitExpense;
begin
  CPuExpense := 0;
end;

{ TConnectionBufferState }

function TConnectionBufferState.PercentConsumed: single;
begin
  if size = 0 then
    exit(1.0);
  exit(consumed/size);
end;

end.
