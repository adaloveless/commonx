unit MultiBufferQueueStream;
{x$DEFINE SMALL_TEST}
interface

// upon fetching buffer
// 1. wait for command
// 2. if buffer needed is secondary buffer, swap buffers... else fetch into primary
{x$INLINE AUTO}
{x$DEFINE DEBUG_MEMORY_FILE_STREAM}
{x$DEFINE USE_COMMANDS}
{$DEFINE SECTSEEK}
{$DEFINE WTF}
{x$DEFINE SIZE_CHECK}
{$DEFINE USE_QUICK_BUFFER}
{$DEFINE USE_ARRAYS}


uses typex, numbers, systemx, sysutils, classes, sharedobject, queuestream, betterobject, linked_list, signals,
{$IFDEF MSWINDOWS}
  windows,//for inlines
{$ENDIF}
{$IFDEF USE_COMMANDS}
  commandprocessor,
{$ELSE}
  simplequeue,
{$ENDIF}
  managedthread,
{$IFDEF WINDOWS}
  winapi.windows,
{$ENDIF}
  debug, commandicons, tickcount, stringx;

const
  MINIMUM_READ_GUARANTEE = 0;
//  MEMORY_STREAM_DEFAULT_BUFFER_SIZE = 4;
  DEFAULT_BUFFER_SEGMENTS = 32;
  MEMORY_STREAM_DEFAULT_BUFFER_SIZE = 65536*DEFAULT_BUFFER_SEGMENTS;
  DEFAULT_MINIMUM_PREFETCH_SIZE = 250000;


{$IFDEF SMALL_TEST}
  MAX_BUFFERS = 4;
{$ELSE}
  MAX_BUFFERS = 1024;
{$ENDIF}
  MAX_BASE_WRITE = 16384;

// MAX_BUFFER_SIZE = 256000;

type
  EBaseStreamGuarantee = class(EXception);
  TMultiBufferQueueStream = class; // forward



  TBufferInfo = record
  private
    ____ptr: PByte;
    ____originallyfetched: nativeint;
    ____highestwriteposition: nativeint;
    ____offset: int64;
    ____dirty: boolean;
    ____size: int64;
    ____wasread: boolean;
    ____mask: Pbyte;
    ____wasreadbyclient: boolean;
    FbufStream: TMultiBufferQueueStream;

    procedure SetSize(const Value: int64);
    function GetBytesAppendedFromOriginal: nativeint;
    function GetSpaceAvailable: nativeint;
    function GEtBytesToFlush: nativeint;
    procedure Cleanup;
    procedure AllocatePointerIfNeeded;

  private
    function FindMAskValue(iStartingAt:  nativeint; bValue: byte): nativeint;

  public
    function DebugString: string;
    function Write(iOffsetIntoBuffer: nativeint; pIn: Pbyte; iLen: nativeint): nativeint;
          //writes to buffer
    function Read(iOffsetIntoBuffer: nativeint; pOut: Pbyte; iMaxLen: nativeint): integer;
          //reads from buffer


    procedure FlushToStream(sStream: TMultiBufferQueueStream);inline;
          //flushes the buffer out to a stream

    procedure FetchFromStream(sStream: TMultiBufferQueueStream);
          //fetches as much as possible from a given stream



    procedure Finalize;
    procedure Init;
    property Size: int64 read ____size write SetSize;
    property Offset: int64 read ____offset write ____offset;
    property OriginallyFetched: nativeint read ____originallyfetched write ____originallyfetched;
    //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    //KEY STUFF ABOVE
    //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    function CanWriteFromLocalPosition(iFromLocalPosition: nativeint): nativeint;
          //returns the number of bytes can can be written to the buffer given a local offset

    property BytesAppendedFromOriginal: nativeint read GetBytesAppendedFromOriginal;
          //returns the number of bytes added after the fetched part, not all that important

    property BytesToFlush: nativeint read GEtBytesToFlush;
          //Returns the number of bytes that should be written when flushing the buffer


    property Dirty: boolean read ____dirty;

    function IsInRange(iPosition: int64): boolean;inline;

    property Stream: TMultiBufferQueueStream read FBufStream write FBufStream;
    property WasReadbyClient: boolean read ____wasreadbyclient;

  end;

  PMBBufferInfo = ^TBufferInfo;

  TBufOrder = class(TBetterObject)
  public
    buf: PMBBufferInfo;
  end;


{$IFDEF USE_COMMANDS}
  TLOCALBaseCommand = Tcommand;
{$ELSE}
  TlocalBaseCommand = TFakeCommand;
{$ENDIF}
  TMemoryStreamCommand = class(TLocalBaseCommand)
  private
    FStream: TMultiBufferQueueStream;
    FBuffer: PMBBufferInfo;
  public
    procedure InitExpense; override;
    property Stream: TMultiBufferQueueStream read FStream write FStream;
    property Buffer: PMBBufferInfo read FBuffer write FBuffer;

  end;




  Tcmd_MBMFS_FlushAndPrefetch = class(TMemoryStreamcommand)
  private
    FFlushOnly: boolean;
  protected
    FPosition: int64;
  public
    procedure Init;override;
    procedure DoExecute; override;
    property Position: int64 read FPosition write FPosition;
    property FlushOnly: boolean read FFlushOnly write FFlushonly;
  end;

  TBetterStream = class(TStream)
  public
    constructor create;virtual;
    procedure Init;virtual;
  end;

  TSharedStream = class(TBetterStream)
  protected
    sect: TCLXCriticalSection;

  public
    procedure Init;override;
    destructor Destroy;override;
    procedure Lock;
    function TryLock: boolean;
    procedure Unlock;
    constructor Create;override;
  end;


  TMultiBufferQueueStream = class(TSharedStream)
  private
    FBufferSegmentShift: nativeint;
    FBufferSEgments: nativeint;
    FWriteable: boolean;
    FAllowReadPastEOF: boolean;
    FDisableLookAhead: boolean;
    FMinimumPrefetchSize: nativeint;
    function GetBufferSize: integer;inline;
    procedure SetBufferSize(const Value: integer);
    function GetFPrimaryBuffer: PMBBufferInfo;inline;
    function GetFSecondaryBuffer: PMBBufferInfo;inline;
    function GetFTertiaryBuffer: PMBBufferInfo;inline;
    function GetBufferForRange(iPosition: int64; pExcept: PMBBufferInfo): PMBBufferInfo;inline;
{$IFDEF USE_ARRAYS}
    function GetBufferIndexForRange(iPosition: int64; pExcept: PMBBufferInfo): integer;inline;
{$ELSE}
    function GetBufferOrderForRange(iPosition: int64; pExcept: PMBBufferInfo): TBufOrder;inline;
{$ENDIF}
    function IsInAnyBufferRange(iPosition: int64; pExcept: PMBBufferInfo): boolean;inline;
    procedure SEtBufferSegments(const Value: nativeint);
    procedure SEtDisableLookAhead(const Value: boolean);
    procedure AllocateBufferOrderObjects;
  protected
//    FPrimaryBuffer: PMBBufferInfo;
//    FSecondaryBuffer: PMBBufferInfo;
//    FTertiaryBuffer: PMBBufferInfo;


{$IFDEF USE_ARRAYS}
    BufferOrders: array[0..MAX_BUFFERS-1] of PMBBufferInfo;
{$ELSE}
    BufferOrders: TDirectlyLinkedList<TBufOrder>;
{$ENDIF}
    Buffers: array [0 .. MAX_BUFFERS-1] of TBufferInfo;
    sectSize, sectSEEK: TCLXCriticalSection;

    iSeekPosition: int64;
    iFileSizeAtFetch: int64;
    iFileSize: integer;
    bgotsize: boolean;
{$IFDEF GROW_CHECK}
    GrowCheck: boolean;
    DoNotGrowPast: int64;
{$ENDIF}
    cmd: Tcmd_MBMFS_FlushAndPrefetch;
{$IFDEF USE_COMMANDS}
    cmd_proc: TCommandProcessor;
{$ELSE}
    cmd_proc: TSimpleQueue;
{$ENDIF}
    iSegmentsAhead: ni;

    procedure Prefetch(iPosition: int64; iLength: int64 = 0);
    procedure FetchBuffer(iPosition: int64; iLength: integer = 0;
      bForce: boolean = false; bForWriting: boolean = false);
    function IsAfterEOF(iPosition: integer): boolean;inline;
    function GetSize_Native(): int64;inline;
    function GetSize_Slow(): int64;inline;
    function GetSize: int64; override;
    procedure UpdateSizeFromUnder;
    procedure AllocateBufferIfNeeded(buf: PMBBufferInfo);inline;
    procedure LockSize;inline;
    procedure UnlockSize;inline;
    property PrimaryBuffer: PMBBufferInfo read GetFPrimaryBuffer;
    property SecondaryBuffer: PMBBufferInfo read GetFSecondaryBuffer;
    property TertiaryBuffer: PMBBufferInfo read GetFTertiaryBuffer;
    procedure ModAfterLoad(buf: PMBBufferInfo);virtual;
    procedure AssertOpenFlags(mode: nativeint);
    procedure SetSize(const newsize: int64);override;
  public
    lastopWasWrite: boolean;
    allowedReadAheads: ni;
    FTrackedSize: int64;
    FOwnsStream: boolean;
    understream: TAdaptiveQueuedStream;
    SizeLimit: int64;
    DisableMinimumPrefetch: boolean;
    allowed_prefetches: ni;
    prefetchposition: int64;
    procedure Needthread;inline;
    procedure Init;override;
    procedure OptimizeForBinarySearch;
    destructor Destroy; override;
    procedure FInalizeBuffers;
{$IFNDEF USE_ARRAYS}
    procedure FinalizeBufferObjects;
{$ENDIF}
    procedure Sync_FlushBuffer(buf: PMBBufferInfo);
    procedure Sync_Fetch(iPosition: int64; buf: PMBBufferInfo; bDontRead: boolean);
    procedure SeekLock;inline;
    procedure SeekUnlock;inline;
    function BaseGuaranteeRead(p: PByte; iSize: integer; bThrowExceptions: boolean = true): integer;
    function BaseGuaranteeWrite(p: PByte; iSize: int64): int64;
    function BaseRead(var Buffer; Count: Longint): Longint;inline;
    function BaseSize: int64;
    function BaseSeek(iPosition: int64; so: TSeekOrigin = soBeginning): int64;
    function BaseWrite(const Buffer; Count: Longint): Longint;inline;
    procedure BaseWriteZeros(iStart, iEnd: int64);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const offset: int64; Origin: TSeekOrigin): int64; override;
    property BufferSize: integer read GetBufferSize write SetBufferSize;
    function BufferSegmentSize_CALC: nativeint;inline;

{$IFNDEF USE_ARRAYS}
    procedure SwapBuffers(buf: TBufOrder);
{$ELSE}
    procedure SwapBuffers(iIndex: integer);
{$ENDIF}

    procedure FinishFlush;
    property BufferSEgments: nativeint read FBufferSEgments write SEtBufferSegments;
    procedure LogFatalDiagnosticInformation;
    procedure Flush;
    property Writeable: boolean read FWriteable write FWriteable;
    function LastBufferOffset: nativeint;
    property AllowReadPastEOF: boolean read FAllowReadPastEOF write FAllowReadPastEOF;
    procedure GrowFile(iSizE: int64);
    property DisableLookAhead: boolean read FDisableLookAhead write SEtDisableLookAhead;
    property MinimumPrefetchSize: nativeint read FMinimumPrefetchSize write FMinimumPrefetchSize;
    function DirtyBufferCount: ni;
    constructor Create(const Astream: TAdaptiveQueuedStream; bTakeOwnerShip: boolean = true);reintroduce;virtual;
    procedure AdjustBufferBounds(oldsize, newsize: int64);
    function SmartSideFetch: boolean;
    procedure SetPRefetchHints(addr: int64; allowed: ni = -1);
  end;



implementation

uses
  dirfile,helpers.stream;

{ TMultiBufferQueueStream }

procedure TMultiBufferQueueStream.AdjustBufferBounds(oldsize, newsize: int64);
var
  t: ni;
  iStart, iCount: int64;
  p: array of byte;
  bufStart, bufLast: int64;
begin
  lock;
  try
    for t:= 0 to self.BufferSEgments-1 do begin
      bufStart := buffers[t].____offset;
      bufLast := buffers[t].____offset + buffers[t].____size;

      if newsize > oldsize then begin
        if newsize < bufStart then continue;
        if bufLast < oldsize then continue;
        iStart := buffers[t].____highestwriteposition+1;
        if iStart < 0 then
          iStart := 0;
        iCount := lesserof((size)-(iStart+buffers[t].____offset), (buffers[t].____size)-(iStart+buffers[t].____offset));
        setlength(p, buffers[t].Size);
        buffers[t].Write(iStart, @p[0], iCount);

      end else begin
        if newsize > bufLast then
          continue;
        if newsize < bufstart then
          buffers[t].____offset := -1;
        if newsize < bufLast then
          buffers[t].____highestwriteposition := (newsize - (buffers[t].____offset))-1;

      end;

    end;
  finally
    Unlock;
  end;
end;

procedure TMultiBufferQueueStream.AllocateBufferIfNeeded(buf: PMBBufferInfo);
// allocates if the buffersize changes
begin
  if buf.Size <> BufferSegmentSize_CALC then begin
      buf.Size := BufferSegmentSize_CALC;//<<<----- allocates memory
      buf.Stream := self;
  end;
end;

procedure TMultiBufferQueueStream.AllocateBufferOrderObjects;
var
  t: ni;
  buf: TBufOrder;
begin
{$IFNDEF USE_ARRAYS}
  FinalizeBuffers;
  for t:= 0 to high(buffers) do begin
    buf := TBufOrder.create;
    buf.buf := @Buffers[t];

    bufferorders.Add(buf);

  end;
{$ENDIF}
end;

function TMultiBufferQueueStream.BaseGuaranteeRead(p: PByte; iSize: integer; bThrowExceptions: boolean = true): integer;
begin
  result := stream_guaranteeread(understream, p, isize);
end;


function TMultiBufferQueueStream.BaseGuaranteeWrite(p: PByte;
  iSize: int64): int64;
begin
  result := stream_guaranteewrite(understream, p, isize);
end;

function TMultiBufferQueueStream.BaseRead(var Buffer; Count: integer): Longint;
begin
  result := understream.EndAdaptiveRead(count, understream.BeginAdaptiveRead(pbyte(@char(buffer)), count));
end;

function TMultiBufferQueueStream.BaseSeek(iPosition: int64; so: TSeekOrigin = soBeginning): int64;
begin
  result := understream.Seek(iposition, so);
end;

function TMultiBufferQueueStream.BaseSize: int64;
begin
  result := understream.Size;
end;

function TMultiBufferQueueStream.BaseWrite(const Buffer; Count: integer): Longint;
begin
  understream.AdaptiveWrite(pbyte(@char(buffer)), count);
  result := count;
end;

procedure TMultiBufferQueueStream.BaseWriteZeros(iStart, iEnd: int64);
begin
  understream.Seek(iStart, soBeginning);
  understream.AdaptiveWriteZeroes(POsition, iEnd-iStart);


end;

function TMultiBufferQueueStream.IsInAnyBufferRange(iPosition: int64; pExcept: PMBBufferInfo): boolean;
begin
  result := GetBufferForRange(iPosition, pExcept) <> nil;
end;


function TMultiBufferQueueStream.BufferSegmentSize_CALC: nativeint;
begin
  result := 1 shl FBufferSegmentShift;
  if result < 1 then
    result := 1;

end;


constructor TMultiBufferQueueStream.Create(const Astream: TAdaptiveQueuedStream;
  bTakeOwnerShip: boolean);
begin
  inherited Create;
  understream := AStream;
  FOwnsStream := bTakeOwnership;
  FTrackedSize := astream.size;
end;

destructor TSharedStream.Destroy;
begin
  DeleteCriticalSection(sect);
  inherited;

end;

procedure TSharedStream.Init;
begin
  InitializeCriticalSection(sect);
end;


constructor TSharedStream.Create;
begin
  inherited Create;
end;


destructor TMultiBufferQueueStream.Destroy;
begin

{$IFNDEF USE_COMMANDS}
  if assigned(cmd_proc) then begin
    cmd_proc.Stop;
    cmd_proc.ProcessAllSynchronously;
    TPM.NoNeedthread(cmd_proc);
  end;
  cmd_proc := nil;
{$ENDIF}
  FinalizeBuffers;

  DeleteCriticalSection(sectSize);
{$IFDEF SECTSEEK}  dcs(sectSeek);{$ENDIF}
  if FOwnsStream then
     understream.free;

{$IFNDEF USE_ARRAYS}
  FinalizeBufferObjects;
  bufferorders.free;
  bufferorders := nil;
{$ENDIF}

  inherited;
end;

function TMultiBufferQueueStream.DirtyBufferCount: ni;
var
  t: ni;
begin
  result := 0;
  for t:= 0 to BufferSegments-1 do begin
    if buffers[t].Dirty then
      inc(result);
  end;

end;



procedure TMultiBufferQueueStream.Sync_Fetch(iPosition: int64; buf: PMBBufferInfo; bDontRead: boolean);
var
  b: boolean;
  itoRead: integer;
  t: integer;
  iFetched: nativeint;
  iSize: int64;
  shift: ni;
begin
  if buf.dirty then
    Sync_FlushBuffer(buf);

  shift := FBufferSegmentSHift;
  if iPosition < 0 then
    raise Exception.create('Cannot seek to a negative position! @'+inttohex(iPosition, 16));
  iPosition := (iPosition shr shift) shl shift;
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
  Debug.Log(self,'Prefetch @'+inttostr(iPosition)+' into @block:'+floatprecision(iPosition/BufferSegmentSize,2)+' @ptr'+inttostr(integer(pbyte(buf))),'MFS');
{$ENDIF}

  if iPosition < 0 then
    exit;


  buf.finalize;
  buf.init;
  AllocateBufferIfNeeded(buf);


//  if iPosition > 160000000 then begin
//    debug.log('value outside acceptable range in sync_fetch.');
//  end;
  buf.offset := iPosition;
  buf.Size := buffersegmentsize_calc;
  if not bDontRead then begin
    buf.FetchFromStream(self);
    ModAfterLoad(buf);
  end;




end;

procedure TMultiBufferQueueStream.FetchBuffer(iPosition: int64; iLength: integer = 0;
  bForce: boolean = false; bForWriting: boolean = false);
var
  pb: PMBBufferInfo;
  i: integer;
  tmStart: cardinal;
  bo: TBufOrder;
begin
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
  Debug.Log(self,'Fetch buffer @'+inttohex(iPOsition, 16)+' ForWriting:'+booltostr(bForWriting));
{$ENDIF}

  if iPOsition < 0 then
    raise ECritical.create('position cannot be < 0');
  AllocateBufferIfNeeded(PrimaryBuffer);
  if (not bForce) and (PrimaryBuffer.IsInRange(iPosition)) then begin
    exit;
  end;



{$IFDEF USE_ARRAYS}
  i := GetBufferIndexForRange(iPosition,PrimaryBuffer);
  if i>0 then begin
    SwapBuffers(i);
{$ELSE}
  bo := GetBufferOrderForRange(iPosition,PrimaryBuffer);
  if bo <> nil then begin
    SwapBuffers(bo);
{$ENDIF}

{$IFDEF DEBUG_MEMORY_FILE_STREAM}
    Debug.Log(self,'Swapping buffers to:'+primarybuffer.DebugString);
{$ENDIF}
//    if PrimaryBuffer.____offset = $000000000000F424 then begin
//      if PrimaryBuffer.____ptr[0] = $0 then begin
//        Debug.Log('Position Trap BAD!');
//      end else begin
//        Debug.Log('Position Trap Good!');
//      end;
//    end;

  end else begin
    tmStart := GetTicker;
//    if iPosition = $000000000000F424 then begin
//      Debug.Log('Position Trap Good!');
//    end;

{$IFDEF USE_ARRAYS}
    Sync_Fetch(iPosition, bufferorders[BufferSegments-1], (not PrimaryBuffer.WasReadbyClient) or bForWriting);
{$ELSE}
    Sync_Fetch(iPosition, bufferorders.last.buf, (not PrimaryBuffer.WasReadbyClient) or bForWriting);
{$ENDIF}
//    if iPosition = $000000000000F424 then begin
//      if PrimaryBuffer.____ptr[1] = $0 then begin
//        Debug.Log('Position Trap BAD!');
//      end else begin
//        Debug.Log('Position Trap Good!');
//      end;


    //Debug.Log('Complete IO Miss costs you '+inttostr(GEtTimeSInce(tmSTart))+' ms.');
{$IFDEF USE_ARRAYS}
    SwapBuffers(BufferSegments-1);
{$ELSE}
    SwapBuffers(bufferorders.last);
{$ENDIF}
    iSegmentsAhead := 1;


  end;
  iSegmentsAhead := 1;

{$IFDEF USE_ARRAYS}
  AllocateBufferIfNeeded(bufferorders[0]);
{$ELSE}
  AllocateBufferIfNeeded(bufferorders.First.buf);
{$ENDIF}

end;

{$IFNDEF USE_ARRAYS}
procedure TMultiBufferQueueStream.FinalizeBufferObjects;
var
  buf: TBufOrder;
begin
  while bufferorders.first <> nil do begin
    buf := bufferorders.First;
    buf.buf.finalize;
    bufferorders.remove(buf);
    buf.free;
  end;
end;
{$ENDIF}



procedure TMultiBufferQueueStream.Flush;
var
  t: integer;
  buf: TBufOrder;
begin

{$IFDEF USE_ARRAYS}
  for t:= high(bufferorders) downto low(bufferorders) do begin
    if bufferorders[t] <> nil then begin
      Sync_FlushBuffer(bufferorders[t]);
    end;
  end;
{$ELSE}
  buf := bufferorders.last;
  while buf <> nil do begin
    Sync_FlushBuffer(buf.buf);
    buf := TBufOrder(buf.Prev);
  end;
{$ENDIF}


end;
procedure TMultiBufferQueueStream.FInalizeBuffers;
var
  t: integer;
  buf: TBufOrder;
begin
  Flush;

{$IFNDEF USE_ARRAYS}
  buf := bufferorders.last;
  while buf <> nil do begin
    if buf.buf <> nil then
      buf.buf.Finalize;
    buf :=  TBufOrder(buf.Prev);
  end;

{$ELSE}
  for t:= high(bufferorders) downto low(bufferorders) do begin
    if bufferorders[t] <> nil then
      bufferorders[t].finalize;
  end;



{$ENDIF}
end;

procedure TMultiBufferQueueStream.FinishFlush;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TMultiBufferQueueStream.Sync_FlushBuffer(buf: PMBBufferInfo);
var
  ijustWritten: integer;
  itotalWritten: integer;
  iTotalToWrite: integer;
  iSize: int64;
begin
  if buf = nil then
    exit;

  if not buf.dirty then
    exit;

  GrowFile(buf.Offset);

{$IFDEF DEBUG_MEMORY_FILE_STREAM}
  Debug.Log(self, classname + ' is flushing buffer @'+inttohex(ni(buf.____ptr),0)+' to '+self.Understream.classname+' starting='+memorydebugstring(buf.____ptr, 16));
{$ENDIF}
  buf.FlushToStream(self);



end;

procedure TMultiBufferQueueStream.UnlockSize;
begin
  LeaveCriticalSection(sectSize);
  seekunlock;
end;

procedure TMultiBufferQueueStream.UpdateSizeFromUnder;
begin
  FTrackedSize := GetSize_Native;
end;

procedure TMultiBufferQueueStream.Init;
var
  t: integer;
  buf: TBufOrder;
begin
  inherited;
{$IFDEF USE_ARRAYS}
{$ELSE}
  bufferorders := TDirectlyLinkedList<TBufOrder>.create;
{$ENDIF}

{$IFDEF SECTSEEK}  ics(sectSeek);{$ENDIF}
  InitializeCriticalSection(sectSize);
  BufferSegments := DEFAULT_BUFFER_SEGMENTS;
  MinimumPrefetchSize := DEFAULT_MINIMUM_PREFETCH_SIZE;


//  FPrimaryBuffer := @Buffers[0];
//  FSecondaryBuffer := @Buffers[1];
//  FTertiaryBuffer := @Buffers[2];
{$IFNDEF USE_ARRAYS}

  for t:= low(buffers) to high(buffers) do begin
    buffers[t].init;
    buf := TBufOrder.create;
    buf.buf := @buffers[t];
    bufferorders.Add(buf);
  end;
{$ELSE}
  for t:= low(buffers) to high(buffers) do begin
    buffers[t].init;
    bufferorders[t] := @buffers[t];
  end;

{$ENDIF}

{$IFNDEF USE_COMMANDS}
//  NeedThread;
{$ENDIF}

end;

function TMultiBufferQueueStream.IsAfterEOF(iPosition: integer): boolean;
begin
  result := iPosition >= self.size;
end;


function TMultiBufferQueueStream.LastBufferOffset: nativeint;
begin
  result := (Size shr Fbuffersegmentshift) shl FBufferSEgmentShift;
end;

procedure TMultiBufferQueueStream.LockSize;
begin
  seeklock;
  EnterCriticalSection(sectSize);
end;

procedure TMultiBufferQueueStream.LogFatalDiagnosticInformation;
var
  t: integer;
  s,ss,sss: string;
begin
  for t:= low(buffers) to (high(buffers)) do begin
    s := 'Buffer['+inttostr(t)+'] Dirty:'+booltostr(buffers[t].dirty)+' Offset:'+inttostr(buffers[t].offset)+' ToFlush:'+inttostr(buffers[t].BytesToFlush)+' Size:'+inttostr(buffers[t].Size);
    Debug.Log(self,s,'MFS');
  end;

end;

procedure TMultiBufferQueueStream.ModAfterLoad(buf: PMBBufferInfo);
begin
  //no implementation required
//TODO -cunimplemented: unimplemented block
end;

procedure TMultiBufferQueueStream.Needthread;
begin
{$IFNDEF USE_COMMANDS}
  if cmd_proc = nil then begin
    cmd_proc := TPM.NeedThread<TSimpleQueue>(self);
    cmd_proc.name := cmd_proc.classname+' for '+classname;
    cmd_proc.Loop := true;
    cmd_proc.start;
  end;
{$ENDIF}

end;

procedure TMultiBufferQueueStream.OptimizeForBinarySearch;
begin
  BufferSegments := 45;
end;

procedure TMultiBufferQueueStream.Prefetch(iPosition: int64; iLength: int64 = 0);
begin
// raise exception.Create('unimplemented');

end;

function TMultiBufferQueueStream.Read(var Buffer; Count: integer): Longint;
var
  iCanRead: nativeint;
  pb: PByte;
  iRelPos: nativeint;
  prime: PMBBufferInfo;
begin
  result := 0;
  Lock;
  try
  prime := primarybuffer;
  // check that the read buffer is in the right spot and move it if not
  {$IFDEF DEBUG_MEMORY_FILE_STREAM}
  if prime <> nil then
    Debug.Log(self,self.classname+' Read Pre PrimaryBuffer '+prime.DebugString,'MFS');

  Debug.Log(self,'Read @'+inttostr(iSeekPosition),'MFS');
  if iSeekPosition = 0 then begin
    Debug.Log(self,'Reading at ZERO, this is the danger part late in the game.');
  end;
  {$ENDIF}

//  if count = 476 then
//    Debug.Log('trap.');

  FetchBuffer(iSeekPosition);
  prime := primarybuffer;
  {$IFDEF DEBUG_MEMORY_FILE_STREAM}


  if prime <> nil then
    Debug.Log(self,self.classname+' Read Post PrimaryBuffer '+primarybuffer.DebugString,'MFS');



//  if Count < 512 then begin
//    Debug.Log('Debug trap!');
//  end;
  Debug.Log(self,'TMultiBufferQueueStream.Read  @'+inttostr(iSeekPosition)+' cnt:'+inttostr(Count),'MFS');
  {$ENDIF}


  if prime = nil then begin
    raise Exception.create('wtf');
  end;
  iRelPos := iSeekPosition - prime.offset;


  // Read stuff from the read buffer
  pb := @byte(Buffer);
//  if count = 476 then
//    Debug.Log('trap.');

  if iRelPos < 0 then
    raise ECritical.create('WTF.  Relpos < 0 '+inttostr(iRElpos));
  iCanRead := prime.Read(iRelPos, @pb[0], Count);
  Self.lastOpWasWrite := false;
  allowed_prefetches := buffersegments shr 2;
  prefetchposition := Self.PrimaryBuffer.____offset+BufferSegmentSize_CALC;

//  if count = 476 then
//    Debug.Log('trap.');

  inc(iSeekPosition, iCanRead);

  // return number of bytes we were actually able to read
  result := iCanRead;
  if result = 0 then
    Debug.Log(self,'Read operation is returning 0 bytes. where stream understream.size is '+inttohex(understream.Size,0)+' and local size is '+inttohex(self.size,0)+' and local highwrite is '+inttohex(prime.____highestwriteposition, 0)+' and iRelPos='+inttohex(iRelpos, 0)+' and offset='+inttohex(prime.____offset,0) );
  finally
    Unlock;
  end;

end;

function TMultiBufferQueueStream.Seek(const offset: int64;
  Origin: TSeekOrigin): int64;
begin
  case Origin of
      soBeginning:  begin
        iSeekPosition := offset;
        result := iSeekPosition;
      end;

      soEnd:  begin
      iSeekPosition := size - offset;
      result := iSeekPosition;
    end;

    soCurrent:  begin
      inc(iSeekPosition, offset);
      result := iSeekPosition;
    end;
  else
    result := iSeekPosition;
  end;

end;

procedure TMultiBufferQueueStream.SEtBufferSegments(
  const Value: nativeint);
begin
  FinalizeBuffers;
  if Value > MAX_BUFFERS then
    FBufferSegments := MAX_BUFFERS
  else
    FBufferSEgments := Value;

  AllocateBufferOrderObjects;

end;

procedure TMultiBufferQueueStream.SetBufferSize(const Value: integer);
var
  adj: ni;
  bit1, bit2, bit3: ni;
begin
  if bufferSegmentSize_CALC*FBufferSEgments = value then
    exit;

  FinalizeBuffers;
  adj := (value shr HighOrderBit(value)) shl highorderbit(value);

  bit1 := highorderbit(value);      //32MB ~ 25
  bit2 := highorderbit(FBufferSEgments); //32 segments ~ 5
  bit3 := bit1-bit2;
  if bit3 < 2 then
    bit3 := 2;

//  if bit3 > 10 then
//    bit3 := 10;

  FBufferSegmentShift := bit3;


end;

procedure TMultiBufferQueueStream.SEtDisableLookAhead(const Value: boolean);
begin
  FDisableLookAhead := Value;
end;

procedure TMultiBufferQueueStream.SetPRefetchHints(addr: int64; allowed: ni);
begin
  allowed_prefetches := 0;//SET 0 FIRST BECAUSE VOLATILE

  prefetchposition := addr;
  if allowed < 0 then
    allowed_prefetches := BufferSEgments shr 2
  else
    allowed_prefetches := allowed;

end;

procedure TMultiBufferQueueStream.SetSize(const newsize: int64);
begin
  FinalizeBuffers;
  seeklock;
  try
  {$IFNDEF ALLOW_BUFFER_ADJUST}
    AdjustBufferBounds(self.size, size);
  {$ENDIF}
//    AdjustBufferBounds(size,newsize);
    BaseSeek(newsize);
    understream.size := newsize;
    FTrackedSize := newsize;
  finally
    seekunlock;
  end;



end;

function TMultiBufferQueueStream.SmartSideFetch: boolean;
var
  start: int64;
begin
  result := false;
  //if the last explicit op was a read
  //then fetch
  //if not lastopwaswrite then begin
    if allowed_prefetches > 0 then begin
//      if FQueue.TryLock then
//      try
        if TryLock then
        try
          if allowed_prefetches > 0 then begin
            start := prefetchposition;
            if start >=0 then begin
              if not IsInAnyBufferRange(start, nil) then begin
//                debug.Log([ltThread], 'Side Fetch:'+start.tohexstring);
                self.FetchBuffer(start);
                result := true;
              end;
              Dec(allowed_prefetches);
              inc(prefetchposition, BufferSegmentSize_CALC);
            end;
          end;
        finally
          unlock;
        end;
//      finally
//        FQueue.Unlock;
//      end;
    end;
  //end;

end;

{$IFNDEF USE_ARRAYS}
procedure TMultiBufferQueueStream.SwapBuffers(buf: TBufOrder);
var
  pnew: PMBBufferInfo;
  t: integer;
begin
  bufferorders.remove(buf);
  bufferorders.addfirst(buf);

end;
{$ELSE}
procedure TMultiBufferQueueStream.SwapBuffers(iIndex: integer);
var
  pnew: PMBBufferInfo;
  t: integer;
begin
  pnew := bufferorders[iIndex];
  for t := iIndex downto 1 do begin
    bufferorders[t] := bufferorders[t-1];
  end;
  bufferorders[0] := pnew;
end;
{$ENDIF}

function TMultiBufferQueueStream.Write(const Buffer; Count: integer): Longint;
var
  pb: PByte;
  iTemp: int64;
begin
  Lock;
  try
  // make sure the buffer is in window
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
  if primarybuffer <> nil then
    Debug.Log(self,'Write Pre PrimaryBuffer '+primarybuffer.DebugString,'MFS');
{$ENDIF}

  FetchBuffer(iSeekPosition, 0, false, true);//TODO 1: If we're writing to a buffer and the write would cover the ENTIRE buffer, then we can ignore the fetch.  Figure that out someday.
  pb := PByte(@Buffer);



{$IFDEF DEBUG_MEMORY_FILE_STREAM}
  if primarybuffer <> nil then
    Debug.Log(self,'Write Post PrimaryBuffer '+primarybuffer.DebugString,'MFS');
{$ENDIF}
  iTemp := position-primarybuffer.Offset;
  result := PrimaryBuffer.Write(iTemp, pb, Count);
  Self.lastOpWasWrite := true;
  allowed_prefetches := buffersegments shr 2;
  prefetchposition := Self.PrimaryBuffer.____offset+BufferSegmentSize_CALC;
  iTemp := position+result;
  if iTemp > FTrackedSize then
    FTrackedSize := iTemp;


  iSeekPosition := iSeekPosition + result;       //<<--move the seek position
  finally
    Unlock;
  end;

end;

function TMultiBufferQueueStream.GetBufferForRange(iPosition: int64; pExcept: PMBBufferInfo): PMBBufferInfo;
var
  t: integer;
begin
  result := nil;
  for t:= 0 to BufferSegments-1 do begin
{$IFNDEF USE_ARRAYS}
    if bufferorders[t].buf.IsInRange(iPosition) then begin
{$ELSE}
    if bufferorders[t].IsInRange(iPosition) then begin
{$ENDIF}
{$IFDEF USE_ARRAYS}
      result := bufferorders[t];
{$ELSE}
      result := bufferorders[t].buf;
{$ENDIF}
      exit;
    end;
  end;
end;


{$IFDEF USE_ARRAYS}
function TMultiBufferQueueStream.GetBufferIndexForRange(iPosition: int64; pExcept: PMBBufferInfo): integer;
var
  t: integer;
begin

  for t:= 0 to BufferSegments-1 do begin
    if bufferorders[t].IsInRange(iPosition) then begin
      result := t;
      exit;
    end;
  end;
  result := -1;
end;
{$ENDIF}

{$IFNDEF USE_ARRAYS}
function TMultiBufferQueueStream.GetBufferOrderForRange(iPosition: int64;
  pExcept: PMBBufferInfo): TBufOrder;
var
  t: integer;
begin
  for t:= 0 to BufferSegments-1 do begin
    if bufferorders[t].buf.IsInRange(iPosition) then begin
      result := bufferorders[t];
      exit;
    end;
  end;
  result := nil;

end;
{$ENDIF}

function TMultiBufferQueueStream.GetBufferSize: integer;
begin
  result := FBufferSEgments shl FBufferSegmentShift;
  if result = 0 then
    result := MEMORY_STREAM_DEFAULT_BUFFER_SIZE;
end;

function TMultiBufferQueueStream.GetFPrimaryBuffer: PMBBufferInfo;
begin
{$IFDEF USE_ARRAYS}
  result := bufferorders[0];
{$ELSE}
  if bufferorders.first = nil then
    raise ECritical.create('no primary buffer!');
  result := bufferorders.first.buf;
{$ENDIF}
end;

function TMultiBufferQueueStream.GetFSecondaryBuffer: PMBBufferInfo;
begin
{$IFDEF USE_ARRAYS}
  result := bufferorders[1];
{$ELSE}
  result := TBufOrder(bufferorders.first.Next).buf;
{$ENDIF}
end;

function TMultiBufferQueueStream.GetFTertiaryBuffer: PMBBufferInfo;
begin
{$IFDEF USE_ARRAYS}
  result := bufferorders[BufferSegments-1];
{$ELSE}
  result := TbufOrder(bufferorders.Last).buf;
{$ENDIF}
end;

function TMultiBufferQueueStream.GetSize(): int64;
begin
  result := FTrackedSize;
end;


function TMultiBufferQueueStream.GetSize_Native: int64;
begin
  RESULT := understream.size;
end;

function TMultiBufferQueueStream.GetSize_Slow: int64;
var
  Pos: int64;
begin
  RESULT := understream.size;
end;

procedure TMultiBufferQueueStream.GrowFile(iSizE: int64);
var
  iSize2: int64;
begin

  //make sure that if buf.offset is after the end of the file, we expand the file out to that point
  //to do this ultra-efficiently we will
  if Self.Writeable then begin
    LockSize;
    if iSize >=0 then begin
      TRY
        iSize2 := inherited Size;

        if iSize > iSize2 then begin

          if assigned(cmd) then begin
            UnlockSize;
            try
              cmd.WaitFor;
            finally
              LockSize;
            end;
          end;
            iSize2 := inherited Size;

//          self.Lock;
//          try
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
            Debug.Log(self,'Writing Zeroes out to '+inttostr(iSize)+' because file size is only '+inttostr(iSize2));
{$ENDIF}
            if (sizeLimit > 0) and (iSize > SizeLimit) then
              raise ECritical.create('File size exceeds quota set in SizeLimit.');
            BaseWriteZeros(iSize2, iSize);
            Self.FTrackedSize := iSize;
//          finally
//            Unlock;
//          end;
        end;
      FINALLY
        UnlockSize;
      end;
    end;
  end;
end;


procedure TMultiBufferQueueStream.AssertOpenFlags(mode: nativeint);
begin
  if ((mode and fmOpenWrite) > 0) and (not ((mode and fmOpenRead)>0)) then begin
    raise Exception.Create(self.ClassName+' does not support opening for write only mode.  Open read-write instead.');
  end;
end;





procedure TMultiBufferQueueStream.SeekLock;
begin
//  LockSize;
  ECS(sectSeek);
end;

procedure TMultiBufferQueueStream.SeekUnlock;
begin
  LCS(sectSeek);
//  UnlockSize;

end;


{ Tcmd_Prefetch }

{ TMemoryStreamcommand }

procedure TMemoryStreamcommand.InitExpense;
begin
  inherited;
  cpuexpense := 0;
end;

{ Tcmd_Flush }

{ Tcmd_MBMFS_FlushAndPrefetch }

procedure Tcmd_MBMFS_FlushAndPrefetch.DoExecute;
begin
  inherited;
  Self.Status := 'Block:'+inttostr(Position shr Stream.FBufferSegmentShift);
  Stream.Sync_FlushBuffer(Stream.TertiaryBuffer);
  if not FlushOnly then
    Stream.Sync_Fetch(Position, Stream.TertiaryBuffer, (stream.primarybuffer=nil) or (not stream.PrimaryBuffer.wasreadbyclient));
end;

procedure Tcmd_MBMFS_FlushAndPrefetch.Init;
begin
  inherited;
{$IFDEF USE_COMMANDS}
  Icon := @CMD_ICON_FLUSH;
{$ENDIF}
end;

{ TBufferInfo }

procedure TBufferInfo.AllocatePointerIfNeeded;
begin
  if Size = 0 then exit;

  if (____ptr = nil) then
    ____ptr := GetMemory(Size);
end;

function TBufferInfo.CanWriteFromLocalPosition(
  iFromLocalPosition: nativeint): nativeint;
begin
  result := Size - iFromLocalposition;
end;

procedure TBufferInfo.Cleanup;
begin
  if assigned(____ptr) then
    FreeMemory(____ptr);

  if assigned(____mask) then
    FreeMemory(____mask);

  ____ptr := nil;
  ____mask := nil;
  ____dirty := false;
  ____highestwriteposition := -1;
  ____offset := -1;

end;


function TBufferInfo.DebugString: string;
begin
//    ____ptr: PByte;
//    ____originallyfetched: nativeint;
//    ____highestwriteposition: nativeint;
//    ____offset: int64;
//    ____dirty: boolean;
//    ____size: int64;
//    ____wasread: boolean;
//    ____mask: Pbyte;
//    ____wasreadbyclient: boolean;
//
    result := '';
    result := result + '[ptr:'+inttohex(integer(pointer(____ptr)),16)+']'+
                       '[origfetched:'+inttostr(____originallyfetched)+']';
    result := result + '[highestwrite:'+inttostr(____highestwriteposition)+']';
    result := result + '[offset:'+inttohex(____offset,16)+']';
    result := result + '[size:'+inttostr(____size)+']';
    result := result + '[wasread:'+booltostr(____wasread)+']';
    result := result + '[mask:'+inttohex(integer(pointer(____mask)),16)+']';
    result := result + '[wasreadByClient:'+booltostr(____wasreadbyclient)+']';
//    result := result + 'Mem:'+MemoryDebugString(self.____ptr, self.Size


end;

procedure TBufferInfo.FetchFromStream(sStream: TMultiBufferQueueStream);
var
  iCanRead: nativeint;
begin
  if sStream <> nil then
    FBufStream := sStream;



  AllocatePointerIfNeeded;

  if self.____offset >= sStream.Size then begin
    fillmem(____ptr, size, 0);
  end else begin
//    if offset = 62500 then begin
//      Debug.Log('Key base seek.');
//    end;
{$IFDEF SECTSEEK}    ecs(sStream.sectseek);{$ENDIF}
    try
      fbufstream.BaseSeek(offset);
      iCanRead := lesserof(fbufstream.baseSize-offset, Size);
    //  ____offset := sStream.Position;
      iCanRead := fbufstream.BaseGuaranteeRead(____ptr, iCanRead);
{$IFDEF SIZE_CHECK}
      if iCanRead < size then begin
        if (FStream.size - offset) > iCanRead then
          raise ECritical.create('Fetched bytes in final buffer is inconsistent with stream size. Where Stream.size='+inttostr(FStream.size)+' and read bytes = '+inttostr(iCanRead)+'.  Difference is '+inttostr(FStream.size-(offset+iCanRead)));
      end;
{$ENDIF}
    finally
{$IFDEF SECTSEEK}      lcs(sSTream.sectSeek);{$ENDIF}
    end;
    self.OriginallyFetched := iCanRead;
    ____highestWriteposition := iCanRead -1;
    if ____highestWriteposition = 32 then
      Debug.Log(nil,'trap');
  end;

  self.____wasread := true;
end;

procedure TBufferInfo.Finalize;
begin
  Cleanup;
  ____offset := -1;
  ____size := 0;
end;



function TBufferInfo.FindMAskValue(iStartingAt: nativeint; bValue: byte): nativeint;
var
  t: nativeint;
begin
  for t:= iStartingAt to size-1 do begin
    if ____mask[t] = bValue then begin
      result := t;
      exit;
    end;
  end;
  result := size;
end;

procedure TBufferInfo.FlushToStream(sStream: TMultiBufferQueueStream);
var
  iPos: int64;
  iStart: int64;
  iEnd: int64;
  i: int64;
begin
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
  Debug.Log('TBufferInfo', @self,'Flushing: '+DebugString);
{$ENDIF}

  if sStream <> nil then
    FBufStream := sStream;


  //if we don't have a mask, this task is easy
  if ____mask=nil then begin
{$IFDEF SECTSEEK}    ecs(FBufStream.sectseek);{$ENDIF}
    try
      if offset < 0 then begin
        exit;
//        raise Ecritical.create('offset < 0 '+inttostr(offset));
      end;

      fbufstream.baseSeek(offset);
      fbufstream.baseguaranteewrite( ____ptr, Self.BytesToFlush);
    finally
{$IFDEF SECTSEEK}      lcs(FBufstream.sectseek);{$ENDIF}
    end;
    ____dirty := false;
  end else begin
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
    Debug.Log('TBufferInfo', @self,'MFS has a mask for flushing.');
{$ENDIF}
    //look for beginning of zeros in mask
    iPos := 0;
    repeat

      //Find the first byte written
      iStart := FindMaskValue(iPos, $00);

      //find the last byte written after gap
      iEnd := FindMaskValue(iStart, $FF);

{$IFDEF DEBUG_MEMORY_FILE_STREAM}
      Debug.Log('TBufferInfo', @self,'Mask Range '+inttostr(iSTart)+'-'+inttostr(iEnd)+' flushing ends when start > '+ inttostr(Self.____highestwriteposition));
{$ENDIF}
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
      Debug.Log('TBufferInfo', @self,'Base seek to '+inttohex(offset+iStart,0));
{$ENDIF}
{$IFDEF SECTSEEK}      ecs(sstream.sectseek);{$ENDIF}
      try
        i := offset+iSTart;
        if i < 0 then  begin
          exit;
          //raise ECritical.create('i<0 very bad i='+inttostr(i));
        end;
        //if FBufStream.understream.size > offset+iStart then begin
          Fbufstream.baseSeek(offset+iStart);
          //writing masked values
          Fbufstream.BaseGuaranteeWrite(@____ptr[istart], iEnd-iStart);
        //end;
      finally
{$IFDEF SECTSEEK}        lcs(sStream.sectSeek);{$ENDIF}
      end;

      iPos := iEnd;

    until iPos > Self.____highestwriteposition;

    //Fill Mask With FF so that we know it has been written, although typically
    //this flush operation is done just before the block is destroyed, so it is
    //sorta irrelevant
    //IF however, this block were to stay active, we'd want to only
    //flush out NEW writes in the future.
    FillMem(____mask, size, $FF);

  end;

  if (____mask<>nil) then begin
    //FreeMemory(____mask);
    //____mask := nil;
  end;

end;


function TBufferInfo.GetBytesAppendedFromOriginal: nativeint;
begin
  result := ____highestwriteposition - ____originallyfetched;
end;

function TBufferInfo.GEtBytesToFlush: nativeint;
begin
  result := ____highestwriteposition+1;
end;

function TBufferInfo.GetSpaceAvailable: nativeint;
begin
  result := ____size - ____highestwriteposition;
end;

procedure TBufferInfo.Init;
begin
  OriginallyFetched := 0;
  ____dirty := false;
  ____ptr := nil;
  ____offset := -1;
  ____wasread := false;
  ____wasreadbyclient := false;
  ____mask := nil;
  ____highestwriteposition := -1;


end;



function TBufferInfo.IsInRange(iPosition: int64): boolean;
var
  iTemp: int64;
begin
  result := false;
  if Offset < 0 then begin
    exit;
  end;
  iTemp := (iPosition - Offset);
  result := (iTemp < Size) and (iTemp >= 0);
end;

function TBufferInfo.Read(iOffsetIntoBuffer: nativeint; pOut: Pbyte;
  iMaxLen: nativeint): integer;
var
  iAvail: nativeint;
  p: pbyte;
  bReMarkAsDirty: boolean;
  preserve: int64;
begin
  Self.____wasreadbyclient := true;


  //if this block has a write mask
  bRemarkAsDirty := false;
  if (not ____wasread) and (____mask <> nil) then begin
    //1. we need to preserve the data in the current buffer
    //2. we need to read the data from the stream
    //3. we need to apply the data that WAS in the buffer with the help of the mask
    //4. we need to destroy the writemask so we don't need to do this again for this block

    p := GetMemory(size);
    try
      //1. copy main buffer to temp buffer
      movemem32(@p[0], @self.____ptr[0], size);

      //2. Fetch stream
      if not self.____wasread then begin
//        debug.log('TBufferInfo', @self, 'before mask fetch highest write position = '+inttostr(____highestwriteposition));
        preserve := ____highestwriteposition;
        self.FetchFromStream(fbufStream);
        ____highestwriteposition := greaterof(preserve,____highestwriteposition);
      end
      else
        raise Ecritical.create('I don''t think this condition should ever happen.');


      //3. Apply with mask
      //Destination will be ANDed with mask then ORed with Temp buffer
       MoveMem32WithMask(@____ptr[0], @p[0], ____mask, Size);



      //4. free the mask
      FreeMemory(____mask);
      ____mask := nil;


      bRemarkAsDirty := true;



    finally
      //free the temporary buffer
      FreeMemory(p);
    end;

  end else begin
    if not self.____wasread then
      self.FetchFromStream(fbufstream);

  end;

  //normal buffer read operation
  if Stream.AllowReadPastEOF then begin
    iAvail :=  (self.Size) - iOffsetIntoBuffer;
    iAvail := lesserof(iAvail, iMaxLen);
  end else begin
    //if the file was expanded by manually setting the Size property, then there
    //may not be readable buffers committed for areas of the file... therefore we need
    //to allow the manufacturing of buffers that don't really exist
    if ____highestwriteposition < 0 then
      ____highestwriteposition := lesserof(size-1, (self.Fbufstream.size-____offset)-1);
    iAvail :=  (self.____highestwriteposition+1) - iOffsetIntoBuffer;
    iAvail := lesserof(iAvail, iMaxLen);
    if iAvail < 0 then begin
      raise ECritical.create('Read past EOF at '+inttostr(iOffsetIntoBuffer)+' '+debugstring);
    end;

  end;
  movemem32(@pout[0], @____ptr[iOffsetIntoBuffer], iAvail);
  result := iAvail;
  ____wasreadbyclient := true;
  ____dirty := bRemarkAsDirty or ____dirty;


  if result < 0 then
    raise ECritical.create('About to return a negative result from Read... not good.');


end;

procedure TBufferInfo.SetSize(const Value: int64);
begin
  if value = size then
    exit;
  Cleanup;
  ____size := value;

  AllocatePointerIfNeeded;

end;


function TBufferInfo.Write(iOffsetIntoBuffer: nativeint; pIn: Pbyte;
  iLen: nativeint): nativeint;
var
  iTemp: nativeint;
begin



  //if this block was not read from disk, then we need to allocate a writemask
  //in case we decide to read from disk later
  if (not ____wasread) and (____mask = nil) then begin
    //allocate the mask
    Self.____mask := GetMemory(self.Size);
    FillMem(self.____mask, self.Size, $FF);

    //also need to initialize main buffer to zeros
    FillMem(self.____ptr, self.size, $00);


  end;

  //do the actual writing
  result := lesserof(iLen, self.Size-iOffsetIntoBuffer);
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
  Debug.Log('TBufferInfo', @self,'Write requested to buffer: '+inttostr(iLen)+' bytes. Actual to be written is '+inttostr(result)+'. Size is '+inttostr(self.size));
{$ENDIF}
  if result < 0 then begin
    raise ECritical.create('Trying to write to a buffer that doesn''t represent this part of the file.');
    exit;
  end;
  movemem32(@____ptr[iOffsetIntoBuffer], pIn, result);
  iTemp := (iOffsetIntoBuffer + result)-1;
  if iTemp > (____highestwriteposition) then begin
//    if iTemp = 32 then begin
//      debug.log('TBufferInfo', @self,'trap');
//    end;

    ____highestwriteposition := iTemp;
  end;

  //if we have a mask, we need to use it by
  if (____mask <> nil) then begin
    //write zeroes into mask for all the locations we wrote to the buffer
    fillmem(@____mask[iOffsetIntoBuffer], result, 0);
  end;



  //mark dirty
  ____dirty := true;
//  Debug.Log('After writing to '+inttohex(iOffsetIntoBuffer, 1)+' size is '+inttostr(self.Stream.size));

end;

{ TBinarySearchOptimizedMEmoryStringStream }

procedure TSharedStream.Lock;
begin
  EnterCriticalSection(sect);
end;

function TSharedStream.TryLock: boolean;
begin
  result := tecs(sect);
end;

procedure TSharedStream.Unlock;
begin
  LeaveCriticalSection(sect);
end;


{ TBetterStream }

constructor TBetterStream.create;
begin
  inherited CReate;
  init;
end;

procedure TBetterStream.Init;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

initialization
//  UT_CheckSizingAbility;



end.


