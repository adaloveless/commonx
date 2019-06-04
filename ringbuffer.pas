unit ringbuffer;
{$I 'DelphiDefs.inc'}
interface
{$DEFINE OPTIMIZED}
uses
  sysutils, betterobject, sharedobject, classes, typex, systemx, ios.stringx.iosansi,
  {$IFDEF WINDOWS}
   {$IFDEF FPC}
      windows,
   {$ELSE}
     winapi.windows,
    {$ENDIF}
  {$ENDIF}
numbers;

const
  DEFAULT_RING_BUFFER_SIZE = 65536;
  //DEFAULT_RING_BUFFER_SIZE = 16;

type
  TRingBuffer = class(TSharedObject)
   private
    FHead: ni;
    FTail: ni;
    FSize: ni;
    FOnPollForData: TnotifyEvent;
    FLogOnAdd: boolean;
    procedure SetSize(const Value: ni);
    function GetBufferSpaceAvailable: ni;
    function GetAvailableDataSize: nativeint;
    function GetMAxNextWrite: nativeint;inline;


  protected
    Fbuffer: PByte;
    procedure evalempty;inline;
  public
    evData: TObject;
    evEmpty: TObject;
    procedure WipeBuffer;
    constructor Create;override;
    destructor Destroy;override;

    //stuff for waiting for data
    function IsDataAvailable: boolean;inline;
    function WaitForData(OnWaiting: TNotifyEvent=nil; iTimeOut: ni = 0): boolean;overload;
    function WaitForData(iTimeOut: ni = 0; OnWaiting: TNotifyEvent=nil): boolean;overload;
    function WaitForSomethingToDo(iTimeOut: ni = -1): boolean;

    function GetByte(out b: byte; bMove: boolean = true): boolean;inline;
    function GetAvailableChunk(p: PByte; iSize: nativeint): nativeint;inline;
//    function GuaranteeGetData(p: Pbyte; iSize: nativeint):nativeint;overload;



    function BufferDataPart(p: Pbyte; iLength: nativeint): nativeint;overload;inline;
    function BufferData(p: Pbyte; iLength: nativeint): nativeint;overload;inline;
    procedure BufferChar(c: AnsiChar);overload;inline;
    procedure BufferChar(b: byte);overload;inline;
    property StompTail: ni read FTail;
    property WriteHead: ni read FHead write FHead;



    property Size: ni read FSize write SetSize;

    property OnPollForData: TnotifyEvent read FOnPollForData write FOnpollforData;

    procedure PollForData;
    property BufferSpaceAvailable: ni read GetBufferSpaceAvailable;
    property AvailableDataSize: nativeint read GetAvailableDataSize;
    property DataAvailable: nativeint read GetAvailableDataSize;//dup for interface compatability
    property SpaceAvailable: nativeint read GetBufferSpaceAvailable;//dup for interface compatability
    property MaxNextWrite: nativeint read GetMAxNextWrite;
    function NextWritePointer: Pbyte;inline;

    procedure MovePointer(var ptr: ni; const iBy: ni = 1);inline;
    procedure MoveWriteHeadPointer(iby: ni = 1);inline;
    procedure MoveStompTailPointer(iby: ni = 1);inline;
    function GetWriteableChunkVars(out p: pbyte; out iWriteableChunkSize: nativeint): boolean;
    function GetReadableChunkVars(out p: pbyte;  out iReadableChunkSize: nativeint): boolean;
    function DebugContents: string;
    property LogOnAdd: boolean read FLogOnAdd write FLogOnAdd;
    function Multithreaded: boolean;
    function GuaranteePutData(const p: PByte; const l: ni): ni;
    function GuaranteeGetData(const p: pbyte; const l: ni; iTimeOut: ni = 16000): ni;overload;
    function PutData(const p: PByte; const l: ni): ni;
    function GetData(const p: PByte; const l: ni): ni;
    function Debugstring: string;inline;
  end;

  TDoubleRingBuffer = class(TsharedObject)
  private
    FSize: ni;
    procedure SetSize(const Value: ni);
    function GetAvailableDataSize: nativeint;
    function GetBufferSpaceAvailable: nativeint;
  protected
    FWriteBuffer: TRingBuffer;
    FReadBuffer: TRingBuffer;
    procedure ReadPoll(sender: TObject);
    procedure Shovel;
  public
    constructor Create;override;
    destructor Destroy;override;

    //stuff for waiting for data
    function IsDataAvailable: boolean;inline;
    property DataAvailable: nativeint read GetAvailableDataSize;//dup for interface compatability
    property SpaceAvailable: nativeint read GetBufferSpaceAvailable;//dup for interface compatability
    function WaitForData(OnWaiting: TNotifyEvent=nil; iTimeOut: ni = 0): boolean;overload;
    function WaitForData(iTimeOut: ni = 0; OnWaiting: TNotifyEvent=nil): boolean;overload;
//    function WaitForSomethingToDo(iTimeOut: ni = -1): boolean;

    function GetByte(out b: byte; bMove: boolean = true): boolean;inline;
    function GetAvailableChunk(p: PByte; iSize: nativeint): nativeint;inline;
//    function GuaranteeGetData(p: Pbyte; iSize: nativeint):nativeint;overload;

    function GuaranteePutData(const p: PByte; const l: ni): ni;
    function GuaranteeGetData(const p: pbyte; const l: ni; iTimeOut: ni = 16000): ni;overload;



    function BufferDataPart(p: Pbyte; iLength: nativeint): nativeint;overload;inline;
    function BufferData(p: Pbyte; iLength: nativeint): nativeint;overload;inline;
    procedure BufferChar(c: AnsiChar);overload;inline;
    procedure BufferChar(b: byte);overload;inline;
    function WaitForEmptySignal(iTimeout: ni): boolean;

    property Size: ni read FSize write SetSize;
    function WaitForDataSignal(iTimeout: ni): boolean;
    function PutData(const p: PByte; const l: ni): ni;
    function GetData(const p: PByte; const l: ni): ni;


  end;

function ShovelDataFromRingToRing(r1,r2: TRingBuffer): ni;


implementation

uses
  unittest,tickcount, debug, stringx, signals;
{ TRingBuffer }

function ShovelDataFromRingToRing(r1,r2: TRingBuffer): ni;
var
  a: array [0..2047] of byte;
  iToShovel: ni;
  iGot: ni;
begin
  iToShovel := lesserof(r1.DataAvailable, r2.Spaceavailable);
  result := iToShovel;
  while iToShovel > 0 do begin
    iGot := r1.GetAvailablechunk(@a[0], lesserof(sizeof(a), iToShovel));
    r2.BufferData(@a[0], iGot);
    dec(iToshovel, iGot);
  end;

end;


procedure TRingBuffer.BufferChar(c: AnsiChar);
begin
  BufferChar(ord(c));
//  GLOG.Debug('Buffered ['+inttohex(ord(c),2)+']');
end;

procedure TRingBuffer.BufferChar(b: byte);
begin
  Lock;
  try
    if BufferSpaceAvailable > 1 then begin
      self.Fbuffer[FHead] := b;
      MovePointer(FHead);
{$IFDEF ADD_LOGGING}if LogOnAdd then Debug.ConsoleLog('Buffered: 0x'+inttohex(b,1));{$ENDIF}

    end else begin
      //raise ECritical.create('MIDI overflow');
    end;
{$IFDEF ADD_LOGGING}if LogOnAdd then Debug.ConsoleLog('Ring Buffer Full: 0x'+inttohex(b,1));{$ENDIF}
  finally
    Unlock;
  end;
  Signal(TSignal(evData), true);


end;

function TRingBuffer.BufferDataPart(p: PByte; iLength: nativeint): nativeint;
{$IFNDEF OPTIMIZED}
var
  idx: nativeint;
  iPArt1Length: nativeint;
  iPart2Length: nativeint;
begin
  //GLOG.Debug('WTF');
  result := 0;
  Lock;
  try
    idx := 0;
    while (bufferspaceavailable > 0) and (idx<iLength) do begin
      BufferChar(ansichar(p[idx]));
      inc(idx);
    end;

    result := idx;
  finally
    unlock;
  end;
end;
{$ELSE}
var
  iPArt1Length: nativeint;
begin
  result := 0;
  Lock;
  try
    if FHead >= FTail then begin
      iPart1Length := lesserof((Size)-FHead, lesserof(iLength, self.BufferSpaceAvailable));
      movemem32(@FBuffer[Fhead], p, iPArt1Length);
      FHead := (FHead + iPart1Length) mod Size;
      result := iPart1Length;
    end else begin
      iPart1Length := lesserof(FTail-FHead-1, lesserof(iLength, self.BufferSpaceAvailable));
      movemem32(@FBuffer[Fhead], p, iPArt1Length);
      FHead := (FHead + iPart1Length) mod Size;
      result := iPart1Length;

    end;


  finally
    Unlock;
  end;
  Signal(evData as TSignal, true);

end;
{$ENDIF}
function TRingBuffer.BufferData(p: Pbyte; iLength: nativeint): nativeint;
begin
  result := 0;
  while (result < iLEngth) and (BufferspaceAVailable > 0) do begin
    inc(result, bufferdatapart(@p[result], iLength-result));
  end;

end;




constructor TRingBuffer.Create;
begin
  //nodebug := true;
  inherited;
  Getmem(Fbuffer, DEFAULT_RING_BUFFER_SIZE);
  FSize := DEFAULT_RING_BUFFER_SIZE;
  evData := TSignal.create;
  evEmpty := TSignal.create;
  (evEmpty as TSignal).Signal(true);
end;

function TRingBuffer.DebugContents: string;
begin
  result := memorydebugstring(@FBuffer[0], size)+CRLF;
  result := result + StringRepeat('   ', self.FHead)+'^head(write)'+CRLF;
  result := result + StringRepeat('   ', self.FTail)+'^tail(read)'+CRLF;
  result := result + inttostr(BufferSpaceAvailable)+' space avail.'+CRLF;
  result := result + inttostr(AvailableDataSize)+' data avail.'+CRLF;
end;

function TRingBuffer.Debugstring: string;
begin
  result := '';
end;

destructor TRingBuffer.Destroy;
begin
  Freemem(FBuffer);
  evData.free;
  evEmpty.Free;
  inherited;
end;

procedure TRingBuffer.evalempty;
begin
  Signal(TSignal(evEmpty), AvailableDataSize = 0);
end;

function TRingBuffer.GetAvailableChunk(p: PByte;
  iSize: nativeint): nativeint;
var
  iResultSize: ni;
  pp: PByte;
begin
  Lock;
  try
    if FHead = FTail then begin
      result := 0;
      exit;
    end;
    pp := @self.Fbuffer[Ftail];
    if FHead < FTail then begin
      iResultSize := lesserOf(iSize, FSize-FTail);
      result := iResultSize;
      movemem32(p, pp, result);
      iResultSize := lesserOf(iSize-iREsultSize, FHead-0);
      movemem32(@p[result], @self.Fbuffer[0], iREsultSize);
      result := result + iResultSize;
//      OutputDebugString(pchar('<'+inttostr(iREsultsize)));
    end else begin
      iResultSize := lesserOf(iSize, FHead-FTail);
      result := iResultSize;
      movemem32(p, pp, result);
//      OutputDebugString(pchar('>'+inttostr(iREsultsize)));
    end;

    MovePointer(FTail, result);

    Signal(TSignal(evData), IsDataAvailable);
    evalempty;

  finally
    Unlock;
  end;




end;

function TRingBuffer.GetAvailableDataSize: nativeint;
begin
  result := Size - (BufferSpaceAvailable+1);
end;

function TRingBuffer.GetBufferSpaceAvailable: ni;
var
  i,ii: ni;
begin
  Lock;
  try
    i := WriteHead;
    ii := StompTail;
    if ii <= i then
      ii := ii + Size;

    result := ii-i;

    result := result -1;
  finally
    Unlock;
  end;

end;

function TRingBuffer.GetByte(out b: byte; bMove: boolean): boolean;
begin

  b := ord(FBuffer[Ftail]);
  if bmove then begin
    MovePointer(Ftail);

  end;
  result := true;
  Signal(TSignal(evData), IsDataAvailable);
  evalempty;

end;


function TRingBuffer.GetData(const p: PByte; const l: ni): ni;
begin
  result := GetAvailableChunk(p, l);
  evalEmpty;
end;

function TRingBuffer.GetMAxNextWrite: nativeint;
var
  i,ii: ni;
begin
  Lock;
  try
    i := WriteHead;
    ii := StompTail;
    if ii <= i then
      result := size - i
    else
      result := (ii-i)-1;

  finally
    Unlock;
  end;

end;

function TRingBuffer.GetWriteableChunkVars(out p: pbyte; out iWriteableChunkSize: nativeint): boolean;
begin
  p := @Fbuffer[WriteHead];
  iWriteableChunkSize := Size-WriteHead;
  result := iWriteableChunkSize > 0;
end;

function TRingBuffer.GetReadableChunkVars(out p: pbyte; out iReadableChunkSize: nativeint): boolean;
begin
  p := @Fbuffer[StompTail];
  iReadableChunkSize := Size-StompTail;
  result := iReadableChunkSize > 0;
end;

function TRingBuffer.GuaranteeGetData(const p: pbyte; const l: ni;
  iTimeOut: ni): ni;
var
  tm: ticker;
begin
  result := GetData(p, l);
  tm := GetTicker;
  while result < l do begin
    if WaitForSignal(TSignal(evData), 1000) then begin
      result := result + GetData(@p[result], l-result);
      tm := GetTicker;
    end else
      if GetTimeSince(tm) > iTimeOut then begin
        raise ECritical.create('Ring File timeout');
      end;
  end;
end;


//function TRingBuffer.GuaranteeGetData(p: Pbyte; iSize: nativeint): nativeint;
//var
//  iRead: ni;
//begin
//  result := 0;
//  while result < iSize do begin
//    iREad := GetAvailableChunk(p, iSize-result);
//    if iRead = 0 then
//      raise ECritical.create('Unable to guarantee read data from ring buffer, because not enoughdata was in the buffer... check AvailableDataSize before requesting Guaranteed Read.  This function does not support reading more data than is in the buffer.');
//    inc(result, iRead);
//    inc(p, iRead);
//
//  end;
//end;

function TRingBuffer.IsDataAvailable: boolean;
begin
  result := WriteHead <> StompTail;
end;

procedure TRingBuffer.MovePointer(var ptr: ni; const iBy: ni = 1);
begin
  inc(ptr, iBy);
  ptr := ptr mod size;
end;

procedure TRingBuffer.MoveStompTailPointer(iby: ni);
begin
  MovePointer(FTail, iBy);
end;

procedure TRingBuffer.MoveWriteHeadPointer(iby: ni = 1);
begin
  MovePointer(FHead, iBy);
end;

function TRingBuffer.Multithreaded: boolean;
begin
  result := @OnPollForData = nil;
end;

function TRingBuffer.NextWritePointer: Pbyte;
begin
  result := @Fbuffer[WriteHead];
end;

procedure TRingBuffer.PollForData;
begin
  if BufferSpaceAvailable > 0 then begin
    if Assigned(OnPollForData) then
      OnPollForData(self);

  end;
end;

function TRingBuffer.PutData(const p: PByte; const l: ni): ni;
begin
  result := BufferDataPart(p, l);
end;

procedure TRingBuffer.SetSize(const Value: ni);
begin
  FSize := Value;
  ReallocMem(FBuffer, FSize);
end;

function TRingBuffer.WaitForData(iTimeOut: ni;
  OnWaiting: TNotifyEvent): boolean;
var
  tmStart, tmnow: cardinal;
begin
  if Multithreaded then begin
    if Assigned(OnWaiting) then begin
      OnWaiting(self);
    end;
    result := WaitForSignal(TSignal(evData), iTimeOut);
  end else begin
    tmStart := GetTicker;
    while not IsDataAvailable do begin
      PollForData;
      tmNow := GetTicker;
      if GetTimeSince(tmNow, tmStart) > 100 then begin
        if Assigned(OnWaiting) then begin
          OnWaiting(self);
        end;
      end;
      if int64(GEtTimeSince(tmNow, tmStart)) > iTimeOut then begin
        result := false;
        break;
      end;
    end;
    result := true;
  end;
end;

function TRingBuffer.WaitForSomethingToDo(iTimeOut: ni): boolean;
begin
  result := WaitForData(iTimeOut);
end;

procedure TRingBuffer.WipeBuffer;
begin
  fillmem(FBuffer, FSize, 0);
end;

function TRingBuffer.WaitForData(OnWaiting: TNotifyEvent;
  iTimeOut: ni): boolean;
begin
  result := waitForData(iTimeOut, OnWaiting);

end;

type
  TUT_RingBuffer = class (TUnitTest)
  public
    procedure DoExecute;override;
  end;



function TRingBuffer.GuaranteePutData(const p: PByte; const l: ni): ni;
begin
  result := PutData(p, l);
  if result < l then
    result := result + PutData(@p[result], l-result);
end;

{ TUT_RingBuffer }

procedure TUT_RingBuffer.DoExecute;
var
  sl1,sl2: TStringlist;
  sTemp: string;
  ioff: nativeint;
  sin, sLeft, sRight: string;
  rb: TRingBuffer;
  a1,a2: array of byte;
  r1,r2,r3: string;
  t,u: ni;
begin
  inherited;
  sl1 := TStringlist.create;
  sl2 := TStringlist.create;
  try
    case Variation of
      10: begin
        VariationName := 'create small ringbuffer, check initial state';
        rb := TRingBuffer.create;
        try
          rb.Size := 7;
          rb.WipeBuffer;
          utresult := rb.DebugContents;
        finally
          rb.free;
          rb := nil;
        end;
      end;
      11: begin
        VariationName := 'create small ringbuffer, list contents 00 01 02 03 04 00 00';
        rb := TRingBuffer.create;
        try
          rb.Size := 7;
          rb.WipeBuffer;
          setlength(a1,5);
          a1[0] := 0;          a1[1] := 1;          a1[2] := 2;          a1[3] := 3;          a1[4] := 4;
          rb.BufferData(@a1[0], 5);
          utresult := rb.DebugContents;
        finally
          rb.free;
          rb := nil;
        end;
      end;
      20: begin
        VariationName := 'expect the following (after write 5, read 4, write 5)'+CRLF+
                         '02 03 04 03 04 00 01'+CRLF+
                         '         ^(write)'+CRLF+
                         '            ^(read)'+CRLF;

        rb := TRingBuffer.create;
        try
          rb.Size := 7;
          rb.WipeBuffer;
          setlength(a1,5);
          setlength(a2,4);
          a1[0] := 0;          a1[1] := 1;          a1[2] := 2;          a1[3] := 3;          a1[4] := 4;
          rb.BufferData(@a1[0], 5);
          rb.GetAvailableChunk(@a2[0], length(a2));
          rb.BufferData(@a1[0], 5);
          utresult := rb.DebugContents;
          utresult := utresult + CRLF+'Read:'+MemoryDebugString(@a2[0], length(a2));
        finally
          rb.free;
          rb := nil;
        end;
      end;
      30: begin
        VariationName := 'expect the following (after write 5, read 4, write 5, read 4)'+CRLF+
                         '02 03 04 03 04 00 01'+CRLF+
                         '         ^(write)'+CRLF+
                         '   ^(read)'+CRLF;

        rb := TRingBuffer.create;
        try
          rb.Size := 7;
          rb.WipeBuffer;
          setlength(a1,5);
          setlength(a2,4);
          a1[0] := 0;          a1[1] := 1;          a1[2] := 2;          a1[3] := 3;          a1[4] := 4;
          rb.BufferData(@a1[0], 5);
          rb.GetAvailableChunk(@a2[0], length(a2));
          r1 := MemoryDebugString(@a2[0], length(a2));
          rb.BufferData(@a1[0], 5);
          rb.GetAvailableChunk(@a2[0], length(a2));
          r2 := MemoryDebugString(@a2[0], length(a2));

          utresult := rb.DebugContents;
          utresult := utresult + CRLF+'Read1:'+r1+CRLF;
          utresult := utresult + CRLF+'Read2:'+r2+CRLF;
        finally
          rb.free;
          rb := nil;
        end;
      end;
      40: begin
        VariationName := 'round robin self-check';

        rb := TRingBuffer.create;
        try
          rb.Size := 7;
          rb.WipeBuffer;
          setlength(a1,5);
          setlength(a2,5);
          a1[0] := 0;          a1[1] := 1;          a1[2] := 2;          a1[3] := 3;          a1[4] := 4;
          utresult := 'pass';
          for t:= 0 to 77777 do begin
            fillmem(@a2[0], length(a2), 0);
            rb.BufferData(@a1[0], 5);
            if rb.GetAvailableChunk(@a2[0], length(a2)) < 5 then begin
              utresult := 'GetAvailableChunk returned < 5';
              break;
            end;
            if not CompareMem(@a1[0], @a2[0], 5) then begin
              utresult := 'fail at' +inttostr(t);
            end;
          end;

        finally
          rb.free;
          rb := nil;
        end;
      end;
      50: begin
        VariationName := 'round robin self-check';

        rb := TRingBuffer.create;
        try
          rb.Size := 6;
          rb.WipeBuffer;
          setlength(a1,5);
          setlength(a2,5);
          a1[0] := 0;          a1[1] := 1;          a1[2] := 2;          a1[3] := 3;          a1[4] := 4;
          utresult := 'pass';
          for t:= 0 to 77777 do begin
            fillmem(@a2[0], length(a2), 0);
            rb.BufferData(@a1[0], 5);
            if rb.GetAvailableChunk(@a2[0], length(a2)) < 5 then begin
              utresult := 'GetAvailableChunk returned < 5';
              break;
            end;
            if not CompareMem(@a1[0], @a2[0], 5) then begin
              utresult := 'fail at' +inttostr(t);
            end;
          end;

        finally
          rb.free;
          rb := nil;
        end;
      end;
      60: begin
        rb := TRingBuffer.create;
        try
          rb.Size := 77777*6;
          rb.WipeBuffer;
          setlength(a1,5);
          setlength(a2,5);
          a1[0] := 0;          a1[1] := 1;          a1[2] := 2;          a1[3] := 3;          a1[4] := 4;
          utresult := 'pass';
          for u:= 0 to 9 do begin
            //PUT
            for t:= 0 to 77777 do begin
              rb.BufferData(@a1[0], 5);
            end;
            //GET
            for t:= 0 to 77777 do begin
              fillmem(@a2[0], length(a2), 0);
              if rb.GetAvailableChunk(@a2[0], length(a2)) < 5 then begin
                utresult := 'GetAvailableChunk returned < 5';
                break;
              end;
              if not CompareMem(@a1[0], @a2[0], 5) then begin
                utresult := 'fail at' +inttostr(t);
              end;
            end;
          end;

        finally
          rb.free;
          rb := nil;
        end;
      end;

    end;



  finally
    sl1.free;
    sl2.free;
  end;

end;

{ TDoubleRingBuffer }

procedure TDoubleRingBuffer.BufferChar(b: byte);
begin
  FWritebuffer.BufferChar(b);
end;

procedure TDoubleRingBuffer.BufferChar(c: AnsiChar);
begin
  FWriteBuffer.BufferChar(c);
end;

function TDoubleRingBuffer.BufferData(p: Pbyte; iLength: nativeint): nativeint;
begin
  result := FWriteBuffer.BufferData(p, iLength);
end;

function TDoubleRingBuffer.BufferDataPart(p: Pbyte;
  iLength: nativeint): nativeint;
begin
  result := FWriteBuffer.BufferDataPart(p, iLength);
end;

constructor TDoubleRingBuffer.Create;
begin
  inherited;
  FWriteBuffer := TRingBuffer.create;
  FreadBuffer := TRingBuffer.create;
  FReadBuffer.OnPollForData := self.ReadPoll;
  Size := DEFAULT_RING_BUFFER_SIZE;
end;

destructor TDoubleRingBuffer.Destroy;
begin
  FwriteBuffer.free;
  FreadBuffer.free;
  FwriteBuffer := nil;
  FReadBuffer := nil;

  inherited;
end;

function TDoubleRingBuffer.GetAvailableChunk(p: PByte;
  iSize: nativeint): nativeint;
begin
  Shovel;
  result := FreadBuffer.GetAvailableChunk(p, iSize);
end;

function TDoubleRingBuffer.GetAvailableDataSize: nativeint;
begin
  Shovel;
  result := FReadBuffer.AvailableDataSize;
end;

function TDoubleRingBuffer.GetBufferSpaceAvailable: nativeint;
begin
  result := FWriteBuffer.SpaceAvailable;

end;

function TDoubleRingBuffer.GetByte(out b: byte; bMove: boolean): boolean;
begin
  Shovel;
  result := FReadBuffer.GetByte(b, bMove);

end;

function TDoubleRingBuffer.GetData(const p: PByte; const l: ni): ni;
begin
  Shovel;
  result := GetAvailableChunk(p, l);
end;

function TDoubleRingBuffer.GuaranteeGetData(const p: pbyte; const l: ni;
  iTimeOut: ni): ni;
var
  tm: ticker;
begin
  result := GetData(p, l);
  tm := GetTicker;
  while result < l do begin
    if WaitForDataSignal(200) then begin
      result := result + GetData(@p[result], l-result);
      tm := GetTicker;
    end else
      if GetTimeSince(tm) > iTimeOut then begin
        raise ECritical.create('Ring File timeout');
      end;
  end;
end;

function TDoubleRingBuffer.GuaranteePutData(const p: PByte; const l: ni): ni;
begin
  result := FWriteBuffer.GuaranteePutData(p, l);

end;

function TDoubleRingBuffer.IsDataAvailable: boolean;
begin
  Shovel;
  result := FReadBuffer.IsDataAvailable;

end;

function TDoubleRingBuffer.PutData(const p: PByte; const l: ni): ni;
begin
  result := BufferDataPart(p, l);
end;

procedure TDoubleRingBuffer.ReadPoll(sender: TObject);
begin
  Shovel;
end;

procedure TDoubleRingBuffer.SetSize(const Value: ni);
begin
  FSize := Value;
  FWriteBuffer.size := value shr 1;
  FReadBuffer.size := value shr 1;
end;

procedure TDoubleRingBuffer.Shovel;
begin

    if FReadBuffer.AvailableDataSize = 0 then begin
      Lock;
      try
        ShovelDataFromRingToRing(FWriteBuffer, FReadBuffer);
      finally
        Unlock;
      end;
    end;
    if FWriteBuffer.AvailableDataSize > FReadBuffer.AvailableDataSize then begin
      Lock;
      try
        ShovelDataFromRingToRing(FWriteBuffer, FReadBuffer);
      finally
        Unlock;
      end;
    end;

end;

function TDoubleRingBuffer.WaitForData(iTimeOut: ni;
  OnWaiting: TNotifyEvent): boolean;
begin
  Shovel;
  result := FReadBuffer.WaitForData(iTimeOut, OnWaiting);
end;

function TDoubleRingBuffer.WaitForDataSignal(iTimeout: ni): boolean;
begin
  Readpoll(self);
  result := WaitForSignal(TSignal(FReadbuffer.evData), iTimeout);
end;

function TDoubleRingBuffer.WaitForEmptySignal(iTimeout: ni): boolean;
begin
  result := WaitForSignal(TSignal(FWriteBuffer.evEmpty), iTimeout)
end;

function TDoubleRingBuffer.WaitForData(OnWaiting: TNotifyEvent;
  iTimeOut: ni): boolean;
begin
  Shovel;
  result := FreadBuffer.WaitForData(OnWaiting, iTimeout);
end;

//function TDoubleRingBuffer.WaitForSomethingToDo(iTimeOut: ni): boolean;
//begin
//  result := FReadBuffer.WaitForSomethingToDo(iTimeout);
//end;



initialization
  UTF.RegisterClass(TUT_RingBuffer);




end.

