unit SimpleBufferedConnection2;
{$INLINE AUTO}

interface

uses
  ringbuffer, SimpleAbstractConnection, betterobject, sharedobject,
  ManagedThread, threadmanager, windows, miscroutines, sysutils, classes;

type
  TSimpleBufferedConnection<T: constructor,
    TSimpleAbstractConnection> = class(TSimpleAbstractConnection)
  private
    FGetOutBufferFill: integer;
    FGetInBufferFill: integer;
    FEnabled: boolean;
    FBaudRate: integer;
    FEndpoint: string;
    function GetInBufSize: integer;
    function GetOutBufSize: integer;
    procedure SetInbufSize(const Value: integer);
    procedure SetOutbufSize(const Value: integer);
  protected
    rbOut, rbIn: TRingbuffer;
    conn: T;
    procedure ReadIncoming;
    procedure WriteOutgoing;
  public

    constructor Create(Owner: TObject; Manager: TThreadManager;
      CreateSuspended: boolean); override;
    destructor Destroy; override;
    procedure DoExecute; override;

    function WaitforData(iTimeout: integer): boolean;
    function ReadData(buffer: pointer; iSize: integer; bReadall: boolean): integer;
    function IsDataAvailable: boolean;
    property InBufferSize: integer read GetInBufSize write SetInbufSize;
    property OutBufferSize: integer read GetOutBufSize write SetOutbufSize;
    function GetInBufferFill: integer;
    function GetOutBufferFill: integer;
    property Enabled: boolean read FEnabled write FEnabled;

    property BaudRate: integer read FBaudRate write FBaudRate;
    property EndPoint: string read FEndpoint write FEndPOint;

  end;

implementation

{ TSimpleBufferedConnection<T> }

constructor TSimpleBufferedConnection<T>.Create(Owner: TObject;
  Manager: TThreadManager; CreateSuspended: boolean);
begin
  inherited;
  rbOut := TRingbuffer.Create;
  rbIn := TRingbuffer.Create;
  Priority := tptimeCritical;




end;

destructor TSimpleBufferedConnection<T>.Destroy;
begin
  if not terminated then begin
    terminate;
  end;
  waitfor;
  conn.free;
  inherited;
end;

procedure TSimpleBufferedConnection<T>.DoExecute;
begin
  inherited;
  conn := T.Create;
  while not terminated do begin
    if enabled then begin
      if not conn.connected then begin
        conn.Endpoint := EndPoint;
        conn.connect;
        conn.BaudRate := BaudRate;

      end;
      if conn.connected then begin
        ReadIncoming;
        WriteOutgoing;
      end;
    end;
//    sleep(10);
  end;
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
  result := rbIn.IsDataAvailable;
end;

function TSimpleBufferedConnection<T>.ReadData(buffer: pointer;
  iSize: integer; bReadall: boolean): integer;
var
  bb: pbyte;
begin
   bb := buffer;


  if not bReadAll then begin
    result := rbIn.GetAvailableChunk(bb, iSize);
  end else begin
    //read all methodology
    result := 0;
    result := rbIn.GetAvailableChunk(@bb[result], isize - result);
    while result < isize do begin
      result := result + rbIn.GetAvailableChunk(@bb[result], isize - result);
    end;

  end;
end;

procedure TSimpleBufferedConnection<T>.ReadIncoming;
var
  iHead, iTail: integer;
  b: array[0..8000] of byte;
  iToRead, iRead: integer;
  t: integer;
begin
  if conn.WaitforData(1) then begin
    //windows.beep(500,10);
    iToRead := lesserof(8000, GreaterOf(rbIn.BufferSpaceAvailable,0));
    if iToRead = 0 then exit;

    iRead := conn.readDAta(@b, iToRead, false);
    for t:= 0 to iRead-1 do begin
      rbIn.BufferChar(b[t]);
    end;
//    sleep(1);
  end;
end;


procedure TSimpleBufferedConnection<T>.SetInbufSize(const Value: integer);
begin
  rbIn.Size := value;
end;

procedure TSimpleBufferedConnection<T>.SetOutbufSize(const Value: integer);
begin
  rbOut.Size := value;
end;


function TSimpleBufferedConnection<T>.WaitforData(iTimeout: integer): boolean;
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
      sleep(1);
    end;

    if GetTimeSince(tm1) > cardinal(iTimeout) then begin
      result := false;
      exit;
    end;
  end;
end;

procedure TSimpleBufferedConnection<T>.WriteOutgoing;
begin

//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

end.
