unit SimpleBufferedConnection;
{x$INLINE AUTO}

interface

uses
{$IFDEF WINDOWS}
  windows, //for setting thread priorities
{$ENDIF}
  tickcount, ringbuffer, SimpleAbstractConnection, betterobject, sharedobject, debug,
  ManagedThread, threadmanager, systemx, sysutils, classes, numbers, typex, signals;

type
  TSimpleBufferedConnection<T: constructor,
    TSimpleAbstractConnection> = class(TSimpleAbstractConnection)
  private
    FGetOutBufferFill: integer;
    FGetInBufferFill: integer;
    FEnabled: boolean;
//    FBaudRate: integer;
//    FEndpoint: string;
    function GetInBufSize: integer;
    function GetOutBufSize: integer;
    procedure SetInbufSize(const Value: integer);
    procedure SetOutbufSize(const Value: integer);

  protected
    FShuttingDown: boolean;
    rbOut, rbIn: TRingbuffer;

    thr: TExternalEventThread;

    function GetConnected: boolean;override;

    procedure ReadIncoming;
    procedure WriteOutgoing;
  strict protected

    function DoWaitforData(iTimeout: cardinal): boolean;override;
    function DoCheckForData: Boolean; override;//<<------------------------------
  public
    conn: T;

    procedure SignalAll;
    constructor Create; override;
    destructor Destroy; override;
    procedure DoExecute(sender: TExternalEventThread);
    function WaitForSomethingToDo(iTimeout: ni = -1): boolean;

    procedure Start;
    procedure Stop;
    function DoConnect: boolean; override;
    procedure DoDisconnect; override;

    function DoSendData(buffer: pbyte; length: integer): integer;override;//<<------------------------------

    function DoReadData(buffer: pbyte; length: integer): integer;override;//<<------------------------------

    function IsDataAvailable: boolean;
    property InBufferSize: integer read GetInBufSize write SetInbufSize;
    property OutBufferSize: integer read GetOutBufSize write SetOutbufSize;
    function GetInBufferFill: integer;
    function GetOutBufferFill: integer;
    property Enabled: boolean read FEnabled write FEnabled;

//    property BaudRate: integer read FBaudRate write FBaudRate;
//    property EndPoint: string read FEndpoint write FEndPOint;

  end;

implementation

{ TSimpleBufferedConnection<T> }

function TSimpleBufferedConnection<T>.DoConnect: boolean;
begin
  conn.hostname := Hostname;
  conn.Endpoint := EndPoint;
  result := conn.connect;
  conn.BaudRate := BaudRate;
  Enabled := true;
  Start;

  //conn.connect;
end;

constructor TSimpleBufferedConnection<T>.Create;
begin
  inherited;

  rbOut := TRingbuffer.Create;
  rbIn := TRingbuffer.Create;

  conn := T.Create;



end;

destructor TSimpleBufferedConnection<T>.Destroy;
begin
  disconnect;
  if assigned(thr) then begin
    thr.Stop;
    thr.waitfor;
    thr.Free;
  end;
  THR := nil;
  conn.free;
  conn := nil;
  inherited;
  rbOut.free;
  rbIn.free;
end;

procedure TSimpleBufferedConnection<T>.DoDisconnect;
begin
//  GLog.debug('Destroying '+self.classname);
  Stop;
  enabled := false;
  inherited;
  conn.disconnect;

end;

function TSimpleBufferedConnection<T>.DoCheckForData: Boolean;
begin
  raise Ecritical.create(self.ClassName+' is not to be polled.');
end;

procedure TSimpleBufferedConnection<T>.DoExecute(sender: TExternalEventThread);
begin
  inherited;
  try
  if enabled then begin
    if not conn.connected then begin
//      conn.Endpoint := EndPoint;
//      conn.connect;
//      conn.BaudRate := BaudRate;
      Connect;

    end;
    if conn.connected then begin
//      GLOG.Debug('alive');
      if conn.RequiresPolling then begin
        ReadIncoming;//-----v
        WriteOutgoing;//!!JRN - I had commented out these two lines for some reason... did it break something?
      end;
//      sleep(15);
      WaitForSomethingTodo(1000);
    end else begin
      sleep(1);
//      GLOG.Debug('dropped');
    end;
  end else
    sleep(100);
  except
    on e: Exception do begin
      Debug.Log(self,e.Message);
      disconnect;
    end;
  end;


end;


function TSimpleBufferedConnection<T>.GetConnected: boolean;
begin
  result := conn.Connected or IsDataAvailable;
end;

function TSimpleBufferedConnection<T>.GetInBufferFill: integer;
begin
  result := rbIn.Size-rbIn.BufferSpaceAvailable;
end;

function TSimpleBufferedConnection<T>.GetInBufSize: integer;
begin
  result := rbIn.Size;
end;

function TSimpleBufferedConnection<T>.GetOutBufferFill: integer;
begin
  result := rbOut.Size-rbOut.BufferSpaceAvailable;
end;

function TSimpleBufferedConnection<T>.GetOutBufSize: integer;
begin
  result := rbOut.Size;
end;

function TSimpleBufferedConnection<T>.IsDataAvailable: boolean;
begin
  result := rbIn.IsDataAvailable or HasLeftOvers;
end;

function TSimpleBufferedConnection<T>.DoReadData(buffer: Pbyte;
  length: integer): integer;
var
  bb: pbyte;
begin
  bb := buffer;
  result := rbIn.GetAvailableChunk(bb, length);
end;

procedure TSimpleBufferedConnection<T>.ReadIncoming;
var
  iHead, iTail: integer;
  b: array[0..8000] of byte;
  iToRead, iRead: integer;
  t: integer;
begin
  if conn.WaitforData(100) then begin
    //windows.beep(500,10);
    iToRead := lesserof(8000, GreaterOf(rbIn.BufferSpaceAvailable,0));
    if iToRead = 0 then exit;



    iRead := conn.readDAta(Pbyte(@b), iToRead, false,8000);
    for t:= 0 to iRead-1 do begin
      rbIn.BufferChar(b[t]);
    end;
    signal(evData, true);
//    sleep(1);
  end;
end;


function TSimpleBufferedConnection<T>.DoSendData(buffer: pbyte; length: integer): integer;
begin
  inherited;
  result := rbOut.BufferData(buffer, length);

end;

procedure TSimpleBufferedConnection<T>.SetInbufSize(const Value: integer);
begin
  rbIn.Size := value;
end;

procedure TSimpleBufferedConnection<T>.SetOutbufSize(const Value: integer);
begin
  rbOut.Size := value;
end;



procedure TSimpleBufferedConnection<T>.Start;
begin
  if thr <> nil then
    exit;

  thr := TPM.Needthread<TExternalEventThread>(self);
  thr.OnExecute := Self.DoExecute;
{$IFDEF WINDOWS}
  thr.Priority := tptimeCritical;
{$ENDIF}
  thr.loop := true;
  Fshuttingdown := false;
  thr.Start;

end;

procedure TSimpleBufferedConnection<T>.Stop;
begin

  if assigned(thr) then begin
    thr.Stop;
    thr.SafeWaitFor;
    TPM.NoNeedThread(thr);
    thr := nil;
  end;

end;

function TSimpleBufferedConnection<T>.DoWaitforData(iTimeout: cardinal): boolean;
var
  tm1, tm2: cardinal;
begin
  tm1 := GetTicker;

  while true do begin
    if rbIn.IsDataAvailable then begin
      result := true;
      break;
    end
    else begin
      rbIn.WaitForData(lesserof(100, iTimeout));
//      conn.WaitForData(lesserof(100, iTimeout));
//      sleepex(1,true);
    end;

    if GetTimeSince(tm1) > cardinal(iTimeout) then begin
      result := false;
      exit;
    end;
  end;
end;

procedure TSimpleBufferedConnection<T>.SignalAll;
begin
  FShuttingdown := true;
  Signal(rbOut.evData as TSignal, true);
  Signal(conn.evData, true);
  Signal(evData, true);
end;

function TSimpleBufferedConnection<T>.WaitForSomethingToDo(
  iTimeout: ni): boolean;
var
  sig: TSignal;
  s1,s2: TSignal;
begin
  s1 := rbOut.evData as TSignal;
  s2 := conn.evData;
  if signals.WaitForAnyOfTwoSignals(s1,s2, sig, iTimeout) then begin
    if Fshuttingdown then exit;

    if s1.IsSignaled then
      Self.WriteOutgoing;
    if s2.IsSignaled then begin
      Lock;
      try
        self.ReadIncoming;
        signal(evData, true);
      finally
        unlock;
      end;
    end;



  end;

end;

procedure TSimpleBufferedConnection<T>.WriteOutgoing;
var
  p: array[0..64000] of byte;
  iToSend: integer;
begin
  //DONE 1: is this even right?

  while rbOut.IsDataAvailable do begin
    rbOut.Lock;
    try

      iToSend := rbOut.GetAvailableChunk(@p[0], 64000);
    finally
      rbOut.Unlock;
    end;



    conn.sendData(@p[0], iToSend, true);



  end;




end;

end.
