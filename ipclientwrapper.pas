unit IPClientWrapper;

//TODO 1: If wrapper is created then immediately freed -- AV occurs.

interface
uses helpers.sockets, helpers_stream, systemx, typex, betterobject, SharedObject, better_sockets, sysutils, managedthread, classes, winsock, windows, beeper, tickcount;

type
  TMyIPClient = class;

  TMyIPCLientHelper = class(TManagedThread)
  private
    FExit: boolean;
    FStarted: boolean;
    FMyIPClient: TMyIPClient;
    function GetMyIPCLient: TMyIPClient;
    procedure SetMyIPCLient(const Value: TMyIPClient);
  public
    property IPCLient: TMyIPClient read GetMyIPCLient write SetMyIPCLient;
    procedure DoExecute; override;
  end;


  TMyIPClient = class (TSharedObject)
  private
    Incoming1: array[0..9000] of AnsiChar;
    Incoming2: array of AnsiChar;
    FSendBuffer: TStringList;
    FSocket: TBetterCustomIPclient;
    tmStart, tmNow: cardinal;
    FTimeOut: cardinal;
    FSendImmediate: boolean;
    FExit: boolean;
    FHelper: TMyIPClientHelper;
    function Getsocket: TBetterCustomIPClient;
    procedure SetSocket(const Value: TBetterCustomIPClient);
    function GetReceiveLength: integer;


    procedure PrivSendText(s: ansistring; iLength: integer = -1);
  public
    constructor create(bMultiThread: boolean = false);reintroduce; virtual;
    procedure BeforeDestruction;override;
    destructor destroy; override;
    property socket : TBetterCustomIPClient read Getsocket write SetSocket;
    procedure OnDisconnect(sender: TObject);
    function BufferIncoming: boolean;
    function SendOutgoing: boolean;
    property ReceiveLength: integer read GetReceiveLength;
    procedure SendStream(s: TStream; iBytes: int64 = -1);
    procedure Close;
    procedure SendText(s: ansistring; iLength: integer = -1);
    procedure SendBytes(p: pbyte; iLength: ni);
    procedure SendLine(s: ansistring; eol: ansistring = #13#10);
    function ReadLine(eol: ansistring = #13#10): ansistring;

    function ReadText: ansistring;
    function ReceiveText: ansistring; overload;
    function ReceiveText(iLength: integer): ansistring;overload;

    function CRReceived: boolean;
    property TimeOut: cardinal read FTimeOut write FTimeOut;
    procedure ResetTimeout;
    procedure CheckTimeOut;
    procedure CheckConnected;

    property Sendimmediate: boolean read FSendImmediate write FSendimmediate;
    function WaitForData(iTimeOutMs: integer): boolean;
  end;

function LesserOf(i1,i2: integer): integer;





implementation

uses SimpleWinSock;


{ TMyIPClient }

function TMyIPClient.BufferIncoming: boolean;
var
  iLength, iLength2: integer;
  pc: PAnsiChar;

begin
  result := false;
  //STAGE 1
  //Pull data off the socket into Incoming1, the static buffer
  if socket = nil then
    raise ESocketError.create('Socket Dropped');

//  socket.s;
  Lock;
  try
  if socket.WaitforData(1) then try
    //Lock;
    ResetTimeout;

    result := true;
    iLength := socket.ReceiveBuf(Incoming1, 9000);

    iLength2 := length(Incoming2);
    if iLength <= 0 then begin
      FSocket.Disconnect;
      exit;
//      raise ESocketError.create('Socket Dropped');
    end;

    //if we have data left over from before
//    if iLength2>0 then begin
      //grow the layer2 buffer (the dynamic one) to be big enough to hold all the shit
      SetLength(Incoming2, iLength2+iLength);
      //set pc to point to the beginning of the NEW section of the grown buffer
      pc := @Incoming2[iLength2];
//    end
    //otherwise
//    else begin
      //grow the layer2 buffer to be big enough for what we just read
//      SetLength(Incoming2, iLength2+iLength);
      //set pc to the beginning of the dynamic buffer
//      pc := @Incoming2[0];
//    end;

    //move memory from the static to the dynamic buffer

    MoveMem32(pc, @Incoming1, iLength);
  finally
//    Unlock;
  end else begin
    CheckConnected;
    CheckTimeout;
//    if not socket.connected then
//      raise exception.create('Socket disconnected');
  end;
  finally
    Unlock;
//    socket.unlock;
  end;
end;

procedure TMyIPClient.Close;
begin
  if assigned(socket) then begin
    while sendoutgoing do
      sleep(1);
    socket.Close;
  end;
end;

constructor TMyIPClient.create(bMultithread: boolean = false);
begin
  FExit := true;
  tmStart := GetTicker;
  inherited create;
  setLength(incoming2, 0);
  if not bMultiThread then
    FSendimmediate := true
  else begin
    Fhelper := TPM.Needthread<TMyIPClientHelper>(nil);
    FHelper.IPCLient := self;
    FHelper.SafeREsume;
  end;



  FSendBuffer := TStringList.create;
  FTimeOUt := 300000;
end;

destructor TMyIPClient.destroy;
begin
  self.Close;

  FSendBuffer.free;
  FSendBuffer := nil;

  TPM.NoNeedthread(FHelper);
  FHelper := nil;

  inherited;
end;

procedure TMyIPClientHelper.DoExecute;
begin
  inherited;
  FExit := false;
  try
    FStarted := true;
    while not safeterminated do begin
      if self.IPCLient <> nil then with IPClient do
      if (socket <> nil) and socket.connected then begin
        BufferIncoming;
        SendOutgoing;
      end;
      sleep(1);
    end;
  finally
    FExit := true;
  end;
end;

function TMyIPClient.GetReceiveLength: integer;
begin
  Lock;
  try
    if FSendImmediate then begin
      BufferIncoming;
      SendOutGoing;
    end;



    result := Length(Incoming2);
  finally
    Unlock;
  end;


end;

function TMyIPClient.Getsocket: TBetterCustomIPClient;
begin
  Lock;
  try
    result := FSocket;
  finally
    Unlock;
  end;
end;

procedure TMyIPClient.OnDisconnect(sender: TObject);
begin
//  socket.Disconnect;
end;

function TMyIPClient.ReadLine(eol: ansistring= #13#10): ansistring;
var
  t: integer;
begin
  result := '';
  while not CRReceived do begin
    if FSendImmediate then begin
      BufferIncoming;
      SendOutGoing;
    end;

    sleep(10);
    checktimeout;
  end;

  repeat
    if FSendImmediate then begin
      BufferIncoming;
      SendOutGoing;
    end;

    Lock;
    try
      t := pos(eol, PAnsiChar(incoming2));
      if t>0 then begin
         setlength(result, t-1);
      end;

      Movemem32(@result[1], incoming2, t-1);
      incoming2 := copy(incoming2, t+(length(eol)-1), length(incoming2));

      if result = '' then
        sleep(1);

    finally
      Unlock;
    end;

  until result <> '';
end;


function TMyIPClient.ReadText: ansistring;
var
  t: integer;
begin
  result := '';
  repeat
    if FSendImmediate then begin
      BufferIncoming;
      SendOutGoing;
    end;

    Lock;
    try
      t:= length(incoming2);
       setlength(result, t);
      Movemem32(@result[1], incoming2, t);
      incoming2 := copy(incoming2, t, length(incoming2));

      if result = '' then
        sleep(1);

    finally
      Unlock;
    end;

  until result <> '';
end;

function TMyIPClient.ReceiveText: ansistring;
begin
  beeper.beep(777,50);

  while not CRReceived do begin
    if FSendImmediate then begin
      BufferIncoming;
      SendOutGoing;
    end;

    sleep(10);
    checktimeout;
  end;

  beeper.beep(444,50);

  result := readText;
end;

function TMyIPClient.ReceiveText(iLength: integer): ansistring;
var
  tm1,tm2: cardinal;
begin
  if FSendImmediate then begin
    BufferIncoming;
    SendOutGoing;
  end;

  result := '';
  if iLength = 0 then
    exit;


  repeat
    tm1 := GetTicker;
    while ReceiveLength < iLength do begin
      sleep(10);
//      BufferIncoming;
      tm2 := GetTicker;
      if tm2 < tm1 then tm1 := tm2;
      if tm2-tm1 > 200000 then
        raise exception.create('timeout in ReceiveText');
    end;

    Lock;
    try
     //get length of buffered text
     //iLength := length(incoming2);
     //set length of result
     setlength(result, iLength);
     //remove requested bytes into result from buffer
     MoveMem32(@result[1], incoming2, iLength);
     //remove frint byte(s) from buffer
     incoming2 := copy(incoming2, iLength, length(incoming2));
    finally
      Unlock;
    end;

    if result = '' then
      sleep(1);
  until result <> '';

end;

procedure TMyIPClient.SendStream(s: TStream; iBytes: int64 = -1);
var
  a: array[0..511] of byte;
  iSent: int64;
  iJustSent: int64;
  iToRead: int64;
begin
  raise ECritical.create('not supported');
  if iBytes < 0 then
    iBytes := s.size - s.Position;

  iSent := 0;
  while iSent < iBytes do begin
    iToRead := lesserof(512, iBytes-iSent);
    helpers_stream.Stream_GuaranteeRead(s, @a[0], iToRead);

//    Socket_GuaranteeWrite(self, @a[0], iToRead);
    inc(iSent, iToRead);
  end;

  s.free;
  s := nil;


end;

procedure TMyIPClient.PrivSendText(s: ansistring; iLength: integer = -1);
var
  i, iSent: integer;
  tmStart, tmNow: cardinal;
begin

  if iLength = -1 then
    iLength := length(s);


  iSent := 0;

  tmStart := GetTicker;

  if iLength = 0 then
    exit;


  while iSent < iLength do try
    Lock;
    UniqueString(s);
    ResetTimeout;
//    BufferIncoming;
//    socket.WaitForData(1);
    i := socket.SendBuf(s[(iSent+1)], LesserOf(iLength-iSent, 5000));
    if i < 0 then begin
      i := 0;
      socket.disconnect;
      if not socket.connected then
        exit;
      sleep(1000);
    end else begin
      tmStart := GetTicker;
    end;

    tmnow := GetTicker;

    if tmnow < tmStart then
      tmStart := tmNow;

    if tmnow - tmStart > 25000 then
     raise ESocketError.create('connection dropped');

    inc(iSent, i);
  finally
    Unlock;

  end;


end;

procedure TMyIPClient.SetSocket(const Value: TBetterCustomIPClient);
begin
  Lock;
  try
    if assigned(FSocket) then
      FSocket.OnDisconnect := nil;
    FSocket := value;
    if assigned(FSocket) then
      FSocket.OnDisconnect := self.OnDisconnect;
  finally
    Unlock;
  end;
end;


function TMyIPClient.WaitForData(iTimeOutMs: integer): boolean;
begin
  if ReceiveLength > 0 then
    result := true
  else
    result := FSocket.WaitForData(iTimeOutMs);

end;

{ TCommThread }


procedure TMyIPClient.SendText(s: ansistring; iLength: integer);
begin
  if Sendimmediate then
    PrivSendText(s, iLength)
  else begin
    Lock;
    try
      UniqueString(s);
      FSendBuffer.Add(s);
    finally
      Unlock;
    end;
  end;
end;

function TMyIPClient.SendOutgoing: boolean;
begin
  Lock;
  try
    result := FSendBuffer.count > 0;
    if result then begin
      self.PrivSendText(FSendBuffer[0], -1);
      FSendBuffer.delete(0);
      ResetTimeout;
    end;
  finally
    Unlock;
  end;
end;

function LEsserOf(i1,i2: integer): integer;
begin
  if i1 < i2 then
    result := i1
  else
    result := i2;

end;


function TMyIPClient.CRReceived: boolean;
var
  t: integer;
begin
  Lock;
  try
    result := false;

    for t:= low(Incoming2) to high(Incoming2) do begin
      if Incoming2[t] = #13 then begin
        result := true;
        break;
      end;

    end;
  finally
    Unlock;
  end;
end;


procedure TMyIPClient.CheckConnected;
begin
  if length(Incoming2) > 0 then
    exit;

  if not  socket.Connected then begin
     raise EAbort.create('Socket disconnected');
  end;
end;

procedure TMyIPClient.CheckTimeOut;
begin
  if Timeout = 0 then
    exit;

  tmNow := GetTicker;

  if tmStart > tmNow then
    tmSTart := tmNow;

  if tmNow-tmStart > Timeout then begin
    socket.disconnect;
    raise ESocketError.create('IP Client Timeout');
//    windows.beep(333,200);
//    windows.beep(222,200);
  end;

  if not SImpleWinsock.SocketsAllowed then
    raise ESocketError.create('Socket terminated because the application is shutting down');


end;

procedure TMyIPClient.ResetTimeout;
begin
  tmSTart := GetTicker;
end;


procedure TMyIPClient.BeforeDestruction;
begin
  inherited;
end;



function TMyIPCLientHelper.GetMyIPCLient: TMyIPClient;
begin
  Lock;
  try
    result := FMyIPClient;
  finally
    Unlock;
  end;
end;

procedure TMyIPCLientHelper.SetMyIPCLient(const Value: TMyIPClient);
begin
  Lock;
  try
    FMyIPClient := value;
  finally
    Unlock;
  end;

end;

procedure TMyIPClient.SendBytes(p: pbyte; iLength: ni);
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TMyIPClient.SendLine(s, eol: ansistring);
begin
  SendText(s+eol);

end;




end.
