unit MultiBufferStream;
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


uses typex, numbers, systemx, sysutils, classes, sharedobject,
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
  TMultiBufferStream = class; // forward


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
    FStream: TMultiBufferStream;

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


    procedure FlushToStream(sStream: TMultiBufferStream);inline;
          //flushes the buffer out to a stream

    procedure FetchFromStream(sStream: TMultiBufferStream);
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

    property Stream: TMultiBufferStream read FStream write FStream;
    property WasReadbyClient: boolean read ____wasreadbyclient;

  end;

  PMBBufferInfo = ^TBufferInfo;


{$IFDEF USE_COMMANDS}
  TLOCALBaseCommand = Tcommand;
{$ELSE}
  TlocalBaseCommand = TFakeCommand;
{$ENDIF}
  TMemoryStreamCommand = class(TLocalBaseCommand)
  private
    FStream: TMultiBufferStream;
    FBuffer: PMBBufferInfo;
  public
    procedure InitExpense; override;
    property Stream: TMultiBufferStream read FStream write FStream;
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
    procedure Unlock;
    constructor Create;override;
  end;


  TMultiBufferStream = class(TSharedStream)
  private
    FBufferSize: integer;
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
    function GetBufferIndexForRange(iPosition: int64; pExcept: PMBBufferInfo): integer;inline;
    function IsInAnyBufferRange(iPosition: int64; pExcept: PMBBufferInfo): boolean;inline;
    procedure SEtBufferSegments(const Value: nativeint);
    procedure SEtDisableLookAhead(const Value: boolean);
    procedure AdjustBufferBounds(oldsize, newsize: int64);

  protected
//    FPrimaryBuffer: PMBBufferInfo;
//    FSecondaryBuffer: PMBBufferInfo;
//    FTertiaryBuffer: PMBBufferInfo;


    BufferOrders: array[0..MAX_BUFFERS-1] of PMBBufferInfo;
    Buffers: array [0 .. MAX_BUFFERS-1] of TBufferInfo;
    sectSize, sectSEEK: TCLXCriticalSection;

    iSeekPosition: int64;
    iFileSizeAtFetch: int64;
    iFileSize: integer;
    bgotsize: boolean;
    FSizeCached: int64;
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

    procedure Prefetch(iPosition: int64; iLength: int64 = 0);
    procedure FetchBuffer(iPosition: int64; iLength: integer = 0;
      bForce: boolean = false; bForWriting: boolean = false);

    function IsAfterEOF(iPosition: integer): boolean;inline;
    function GetSize_Native(): int64;inline;
    function GetSize_Slow(): int64;inline;
    function GetSize: int64; override;
    procedure AllocateBufferIfNeeded(buf: PMBBufferInfo);inline;
    procedure LockSize;inline;
    procedure UnlockSize;inline;
    property PrimaryBuffer: PMBBufferInfo read GetFPrimaryBuffer;
    property SecondaryBuffer: PMBBufferInfo read GetFSecondaryBuffer;
    property TertiaryBuffer: PMBBufferInfo read GetFTertiaryBuffer;
    procedure ModAfterLoad(buf: PMBBufferInfo);virtual;
    procedure AssertOpenFlags(mode: nativeint);
    procedure SetSize(const size: int64);override;
  public
    FTrackedSize: int64;
    FOwnsStream: boolean;
    FStream: TStream;
    SizeLimit: int64;
    DisableMinimumPrefetch: boolean;
    procedure Needthread;
    procedure Init;override;
    procedure OptimizeForBinarySearch;
    destructor Destroy; override;
    procedure FInalizeBuffers;
    procedure Sync_FlushBuffer(buf: PMBBufferInfo);
    procedure Sync_Fetch(iPosition: int64; buf: PMBBufferInfo; bDontRead: boolean);
    procedure SeekLock;inline;
    procedure SeekUnlock;inline;
    function BaseGuaranteeRead(p: PByte; iSize: integer; bThrowExceptions: boolean = true): integer;inline;
    function BaseGuaranteeWrite(p: PByte; iSize: int64): int64;inline;
    function BaseRead(var Buffer; Count: Longint): Longint;inline;
    function BaseSize: int64;
    function BaseSeek(iPosition: int64; so: TSeekOrigin = soBeginning): int64;
    function BaseWrite(const Buffer; Count: Longint): Longint;inline;
    procedure BaseWriteZeros(iStart, iEnd: int64);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const offset: int64; Origin: TSeekOrigin): int64; override;
    property BufferSize: integer read GetBufferSize write SetBufferSize;
    function BufferSegmentSize: integer;

    procedure EndAllCommands;inline;
    procedure SwapBuffers(iIndex: integer);
    procedure BeginFlushAndPrefetch(iBufferOffset: int64 );
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
    constructor Create(const Astream: TStream; bTakeOwnerShip: boolean = true);reintroduce;virtual;
  end;



implementation

uses
  dirfile,helpers.stream;

{ TMultiBufferStream }

procedure TMultiBufferStream.AllocateBufferIfNeeded(buf: PMBBufferInfo);
// allocates if the buffersize changes
begin
  if buf.Size <> BufferSegmentSize then begin
      buf.Size := BufferSegmentSize;//<<<----- allocates memory
      buf.Stream := self;
  end;
end;

function TMultiBufferStream.BaseGuaranteeRead(p: PByte; iSize: integer; bThrowExceptions: boolean = true): integer;
begin
  result := stream_guaranteeread(Fstream, p, isize);
end;


function TMultiBufferStream.BaseGuaranteeWrite(p: PByte;
  iSize: int64): int64;
begin
  result := stream_guaranteewrite(Fstream, p, isize);
end;

function TMultiBufferStream.BaseRead(var Buffer; Count: integer): Longint;
begin
  result := FStream.Read(buffer, count);
end;

function TMultiBufferStream.BaseSeek(iPosition: int64; so: TSeekOrigin = soBeginning): int64;
begin
  result := FStream.Seek(iposition, so);
end;

function TMultiBufferStream.BaseSize: int64;
begin
  result := FStream.Size;
end;

function TMultiBufferStream.BaseWrite(const Buffer; Count: integer): Longint;
begin
  result := FStream.Write(buffer, count);
end;

procedure TMultiBufferStream.BaseWriteZeros(iStart, iEnd: int64);
begin
  FStream.Seek(iStart, soBeginning);
  stream_writezeros(FStream, (iEnd-iStart));


end;

function TMultiBufferStream.IsInAnyBufferRange(iPosition: int64; pExcept: PMBBufferInfo): boolean;
begin
  result := GetBufferForRange(iPosition, pExcept) <> nil;
end;

procedure TMultiBufferStream.BeginFlushAndPrefetch(iBufferOffset: int64);
var
  iPos: int64;
begin
  if cmd <> nil then
    exit;

  iPos := iBufferOffset;
  if IsInAnyBufferRange(iPos, nil) then
    exit;

  if not DisableLookAhead then begin
{
    //if we're at the end of the file (particularly for tiny files)
    //it is probably going to be far more efficient to do this synchronously...
    //so we'll skip creating the command
    if (SIZE - iPos) > BufferSegmentSize then begin
      //otherwise create an asynchronous command adjacent to our current buffer
      cmd := Tcmd_MBMFS_FlushAndPrefetch.Create;
      cmd.Stream := self;
      cmd.Position := iPos;
      cmd.Start;
    end;
}
    //if the buffer was never read by the client, then we'll assume that the
    //next one probably won't be either so we'll opt to NOT fetch the data from
    //disk
    if (not TertiaryBuffer.WasReadbyClient) or (iBufferOffset=-1) then begin
      NeedThread;
      cmd := Tcmd_MBMFS_FlushAndPrefetch.Create;
      cmd.Stream := self;
      cmd.FlushOnly := true;
      //cmd.Position := iPos;
      cmd.Start(cmd_proc);
    end else begin
      //otherwise create an asynchronous command adjacent to our current buffer
      NEedThread;
      cmd := Tcmd_MBMFS_FlushAndPrefetch.Create;
      cmd.Stream := self;
      cmd.Position := iPos;
      cmd.Start(cmd_proc);
    end;
  end;

end;

function TMultiBufferStream.BufferSegmentSize: integer;
begin
  result := BufferSize div BufferSegments;
  if result < 1 then
    result := 1;

end;



constructor TMultiBufferStream.Create(const Astream: TStream;
  bTakeOwnerShip: boolean);
begin
  inherited Create;
  FStream := AStream;
  FOwnsStream := bTakeOwnership;
  FTrackedSize := AStream.Size;
end;

destructor TSharedStream.Destroy;
begin
  DeleteCriticalSection(sect);
  inherited;

end;

procedure TSharedStream.Init;
begin
  ics(sect);
end;


constructor TSharedStream.Create;
begin
  inherited Create;
end;


destructor TMultiBufferStream.Destroy;
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
     FStream.free;

  inherited;
end;

function TMultiBufferStream.DirtyBufferCount: ni;
var
  t: ni;
begin
  result := 0;
  for t:= 0 to BufferSegments-1 do begin
    if buffers[t].Dirty then
      inc(result);
  end;

end;

procedure TMultiBufferStream.EndAllCommands;
var
  tmStart: cardinal;
  s: string;
begin
  if cmd <> nil then begin
    if not cmd.iscomplete then begin
      tmStart := GetTicker;
      cmd.WaitFor;
      {$IFDEF DEBUG_MEMORY_FILE_STREAM}
      s := 'IO Command Lag costs you '+inttostr(gettimesince(tmStart))+'!';
      Debug.Log(s);
      {$ENDIF}
    end;
    cmd.WaitFor;
//    GarbageCollect(cmd);
    cmd.Free;
    cmd := nil;
  end;
end;

procedure TMultiBufferStream.Sync_Fetch(iPosition: int64; buf: PMBBufferInfo; bDontRead: boolean);
var
  b: boolean;
  itoRead: integer;
  t: integer;
  iFetched: nativeint;
  iSize: int64;
begin
  if buf.dirty then
    Sync_FlushBuffer(buf);

  if iPosition < 0 then
    raise Exception.create('Cannot seek to a negative position! @'+inttohex(iPosition, 16));
  iPosition := (iPosition div BufferSegmentSize) * BufferSegmentSize;
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
  Debug.Log(extractfilename(self.filename)+' Prefetch @'+inttostr(iPosition)+' into @block:'+floatprecision(iPosition/BufferSegmentSize,2)+' @ptr'+inttostr(integer(pbyte(buf))),'MFS');
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
  buf.Size := buffersegmentsize;
  if not bDontRead then begin
    buf.FetchFromStream(self);
    ModAfterLoad(buf);
  end;




end;

procedure TMultiBufferStream.FetchBuffer(iPosition: int64; iLength: integer = 0;
  bForce: boolean = false; bForWriting: boolean = false);
var
  pb: PMBBufferInfo;
  i: integer;
  tmStart: cardinal;
begin
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
  Debug.Log(FileName+'Fetch buffer @'+inttohex(iPOsition, 16)+' ForWriting:'+booltostr(bForWriting));
{$ENDIF}

  if iPOsition < 0 then
    raise Exception.create('position cannot be < 0');
  AllocateBufferIfNeeded(PrimaryBuffer);
  if (not bForce) and (PrimaryBuffer.IsInRange(iPosition)) then
    exit;

  // wait for all commands to end
  EndAllCommands;
  //GrowFile(iPosition);

  i := GetBufferIndexForRange(iPosition,PrimaryBuffer);
  if i>0 then begin
//    if PrimaryBuffer.____offset = $000000000000F424 then begin
//      Debug.Log(FileName+' Swapping Away our target buffer');
//      if PrimaryBuffer.____ptr[0] = $0 then begin
//        Debug.Log('Position Trap BAD!');
//      end else begin
//        Debug.Log('Position Trap Good!');
//      end;
//    end;
//
    SwapBuffers(i);
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
    Debug.Log(FileName+' Swapping buffers to:'+primarybuffer.DebugString);
{$ENDIF}
//    if PrimaryBuffer.____offset = $000000000000F424 then begin
//      if PrimaryBuffer.____ptr[0] = $0 then begin
//        Debug.Log('Position Trap BAD!');
//      end else begin
//        Debug.Log('Position Trap Good!');
//      end;
//    end;
    if (BufferSegmentSize >= MinimumPrefetchSize) or  (DisableMinimumPrefetch) then
        BeginFlushAndPrefetch(iPOsition+BufferSegmentSize);
  end else begin
    EndAllCommands;
    tmStart := GetTicker;
//    if iPosition = $000000000000F424 then begin
//      Debug.Log('Position Trap Good!');
//    end;

    Sync_Fetch(iPosition, bufferorders[BufferSegments-1], (not PrimaryBuffer.WasReadbyClient) or bForWriting);
//    if iPosition = $000000000000F424 then begin
//      if PrimaryBuffer.____ptr[1] = $0 then begin
//        Debug.Log('Position Trap BAD!');
//      end else begin
//        Debug.Log('Position Trap Good!');
//      end;


    //Debug.Log('Complete IO Miss costs you '+inttostr(GEtTimeSInce(tmSTart))+' ms.');
    SwapBuffers(BufferSegments-1);
    if (BufferSegmentSize >= MinimumPrefetchSize) or (DisableMinimumPrefetch) then
      BeginFlushAndPrefetch(iPosition+BufferSegmentSize);

  end;
  AllocateBufferIfNeeded(bufferorders[0]);

end;

procedure TMultiBufferStream.Flush;
var
  t: integer;
begin
  EndAllCommands;
  for t:= high(bufferorders) downto low(bufferorders) do begin
    if bufferorders[t] <> nil then begin
      Sync_FlushBuffer(bufferorders[t]);
    end;
  end;
end;
procedure TMultiBufferStream.FInalizeBuffers;
var
  t: integer;
begin
  Flush;

  for t:= high(bufferorders) downto low(bufferorders) do begin
    if bufferorders[t] <> nil then
      bufferorders[t].finalize;

  end;
end;

procedure TMultiBufferStream.FinishFlush;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TMultiBufferStream.Sync_FlushBuffer(buf: PMBBufferInfo);
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

  buf.FlushToStream(self);



end;

procedure TMultiBufferStream.UnlockSize;
begin
  LeaveCriticalSection(sectSize);
  seekunlock;
end;

procedure TMultiBufferStream.Init;
var
  t: integer;
begin
  inherited;
{$IFDEF SECTSEEK}  ics(sectSeek);{$ENDIF}
  ics(sectSize);
  BufferSegments := DEFAULT_BUFFER_SEGMENTS;
  MinimumPrefetchSize := DEFAULT_MINIMUM_PREFETCH_SIZE;

//  FPrimaryBuffer := @Buffers[0];
//  FSecondaryBuffer := @Buffers[1];
//  FTertiaryBuffer := @Buffers[2];
  for t:= low(buffers) to high(buffers) do begin
    buffers[t].init;
    bufferorders[t] := @buffers[t];
  end;

{$IFNDEF USE_COMMANDS}
//  NeedThread;
{$ENDIF}

end;

function TMultiBufferStream.IsAfterEOF(iPosition: integer): boolean;
begin
  result := iPosition >= self.size;
end;


function TMultiBufferStream.LastBufferOffset: nativeint;
begin
  result := (Size div BufferSegmentSize) * BufferSegmentSize;
end;

procedure TMultiBufferStream.LockSize;
begin
  seeklock;
  ecs(sectSize);
end;

procedure TMultiBufferStream.LogFatalDiagnosticInformation;
var
  t: integer;
  s,ss,sss: string;
begin
  for t:= low(buffers) to (high(buffers)) do begin
    s := 'Buffer['+inttostr(t)+'] Dirty:'+booltostr(buffers[t].dirty)+' Offset:'+inttostr(buffers[t].offset)+' ToFlush:'+inttostr(buffers[t].BytesToFlush)+' Size:'+inttostr(buffers[t].Size);
    Debug.Log(self,s,'MFS');
  end;

end;

procedure TMultiBufferStream.ModAfterLoad(buf: PMBBufferInfo);
begin
  //no implementation required
//TODO -cunimplemented: unimplemented block
end;

procedure TMultiBufferStream.Needthread;
begin
{$IFNDEF USE_COMMANDS}
  if cmd_proc = nil then begin
    cmd_proc := TPM.NeedThread<TSimpleQueue>(self);
    cmd_proc.name := 'Commands for MultiBufferStream';
    cmd_proc.Loop := true;
    cmd_proc.start;
  end;
{$ENDIF}

end;

procedure TMultiBufferStream.OptimizeForBinarySearch;
begin
  BufferSegments := 45;
end;

procedure TMultiBufferStream.Prefetch(iPosition: int64; iLength: int64 = 0);
begin
// raise exception.Create('unimplemented');

end;

function TMultiBufferStream.Read(var Buffer; Count: integer): Longint;
var
  iCanRead: nativeint;
  pb: PByte;
  iRelPos: nativeint;
  prime: PMBBufferInfo;
begin
  prime := primarybuffer;
  // check that the read buffer is in the right spot and move it if not
  {$IFDEF DEBUG_MEMORY_FILE_STREAM}
  if prime <> nil then
    Debug.Log('Read Pre PrimaryBuffer '+Self.FileName+' '+prime.DebugString,'MFS');

  Debug.Log('Read @'+inttostr(iSeekPosition),'MFS');
  {$ENDIF}

//  if count = 476 then
//    Debug.Log('trap.');

  FetchBuffer(iSeekPosition);
  prime := primarybuffer;
  {$IFDEF DEBUG_MEMORY_FILE_STREAM}


  if prime <> nil then
    Debug.Log('Read Post PrimaryBuffer '+Self.FileName+' '+primarybuffer.DebugString,'MFS');



//  if Count < 512 then begin
//    Debug.Log('Debug trap!');
//  end;
  Debug.Log('TMultiBufferStream.Read '+Self.FileName+' @'+inttostr(iSeekPosition)+' cnt:'+inttostr(Count),'MFS');
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
//  if count = 476 then
//    Debug.Log('trap.');

  inc(iSeekPosition, iCanRead);

  // return number of bytes we were actually able to read
  result := iCanRead;
end;

function TMultiBufferStream.Seek(const offset: int64;
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

procedure TMultiBufferStream.SEtBufferSegments(
  const Value: nativeint);
begin
  FinalizeBuffers;
  if Value > MAX_BUFFERS then
    FBufferSegments := MAX_BUFFERS
  else
    FBufferSEgments := Value;

end;

procedure TMultiBufferStream.SetBufferSize(const Value: integer);
begin
  if FBufferSize = value then
    exit;

  FinalizeBuffers;
  FbufferSize := value;


end;

procedure TMultiBufferStream.SEtDisableLookAhead(const Value: boolean);
begin
  FDisableLookAhead := Value;
end;

procedure TMultiBufferStream.AdjustBufferBounds(oldsize, newsize: int64);
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

procedure TMultiBufferStream.SetSize(const size: int64);
begin
  seeklock;
  try
  {$IFNDEF ALLOW_BUFFER_ADJUST}
    AdjustBufferBounds(self.size, size);
  {$ENDIF}

    FinalizeBuffers;
    BaseSeek(size);
    FStream.size := size;
    FTrackedSize := size;
  finally
    seekunlock;
  end;



end;

procedure TMultiBufferStream.SwapBuffers(iIndex: integer);
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

function TMultiBufferStream.Write(const Buffer; Count: integer): Longint;
var
  pb: PByte;
  iTemp: int64;
begin
  // make sure the buffer is in window
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
  if primarybuffer <> nil then
    Debug.Log('Write Pre PrimaryBuffer '+Self.FileName+' '+primarybuffer.DebugString,'MFS');
{$ENDIF}

  FetchBuffer(iSeekPosition, 0, false, true);//TODO 1: If we're writing to a buffer and the write would cover the ENTIRE buffer, then we can ignore the fetch.  Figure that out someday.
  pb := PByte(@Buffer);



{$IFDEF DEBUG_MEMORY_FILE_STREAM}
  if primarybuffer <> nil then
    Debug.Log('Write Post PrimaryBuffer '+Self.FileName+' '+primarybuffer.DebugString,'MFS');
{$ENDIF}
  result := PrimaryBuffer.Write(position-primarybuffer.Offset, pb, Count);
  iTemp := position+result;
  if iTemp > FTrackedSize then
    FTrackedSize := iTemp;




  iSeekPosition := iSeekPosition + result;       //<<--move the seek position

end;

function TMultiBufferStream.GetBufferForRange(iPosition: int64; pExcept: PMBBufferInfo): PMBBufferInfo;
var
  t: integer;
begin
  result := nil;
  for t:= 0 to BufferSegments-1 do begin
    if bufferorders[t].IsInRange(iPosition) then begin
      result := bufferorders[t];
      exit;
    end;
  end;
end;

function TMultiBufferStream.GetBufferIndexForRange(iPosition: int64; pExcept: PMBBufferInfo): integer;
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

function TMultiBufferStream.GetBufferSize: integer;
begin
  result := FBufferSize;
  if result = 0 then
    result := MEMORY_STREAM_DEFAULT_BUFFER_SIZE;
end;

function TMultiBufferStream.GetFPrimaryBuffer: PMBBufferInfo;
begin
  result := bufferorders[0];
end;

function TMultiBufferStream.GetFSecondaryBuffer: PMBBufferInfo;
begin
  result := bufferorders[1];
end;

function TMultiBufferStream.GetFTertiaryBuffer: PMBBufferInfo;
begin
  result := bufferorders[BufferSegments-1];
end;

function TMultiBufferStream.GetSize(): int64;
begin
  result := FTrackedSize;
end;




function TMultiBufferStream.GetSize_Native: int64;
begin
  RESULT := FStream.size;
end;

function TMultiBufferStream.GetSize_Slow: int64;
var
  Pos: int64;
begin
  RESULT := FStream.size;
end;

procedure TMultiBufferStream.GrowFile(iSizE: int64);
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
            Debug.Log('Writing Zeroes out to '+inttostr(iSize)+' because file size is only '+inttostr(iSize2));
{$ENDIF}
            if (sizeLimit > 0) and (iSize > SizeLimit) then
              raise ECritical.create('File size exceeds quota set in SizeLimit.');
            BaseWriteZeros(iSize2, iSize);
            Self.FSizeCached := iSize;
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


procedure TMultiBufferStream.AssertOpenFlags(mode: nativeint);
begin
  if ((mode and fmOpenWrite) > 0) and (not ((mode and fmOpenRead)>0)) then begin
    raise Exception.Create(self.ClassName+' does not support opening for write only mode.  Open read-write instead.');
  end;
end;





procedure TMultiBufferStream.SeekLock;
begin
//  LockSize;
  ECS(sectSeek);
end;

procedure TMultiBufferStream.SeekUnlock;
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
  Self.Status := 'Block:'+inttostr(Position div Stream.BufferSegmentSize);
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

procedure TBufferInfo.FetchFromStream(sStream: TMultiBufferStream);
var
  iCanRead: nativeint;
begin
  if sStream <> nil then
    FStream := sStream;



  AllocatePointerIfNeeded;

  if self.____offset >= sStream.BaseSize then begin
    fillmem(____ptr, size, 0);
  end else begin
//    if offset = 62500 then begin
//      Debug.Log('Key base seek.');
//    end;
{$IFDEF SECTSEEK}    ecs(sStream.sectseek);{$ENDIF}
    try
      sStream.BaseSeek(offset);
      iCanRead := lesserof(sStream.Size-offset, Size);
    //  ____offset := sStream.Position;
      iCanRead := sStream.BaseGuaranteeRead(____ptr, iCanRead, false);
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

procedure TBufferInfo.FlushToStream(sStream: TMultiBufferStream);
var
  iPos: int64;
  iStart: int64;
  iEnd: int64;
  i: int64;
begin
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
  Debug.Log('Flushing: '+DebugString);
{$ENDIF}

  if sStream <> nil then
    FStream := sStream;


  //if we don't have a mask, this task is easy
  if ____mask=nil then begin
{$IFDEF SECTSEEK}    ecs(FStream.sectseek);{$ENDIF}
    try
      if offset < 0 then
        raise Ecritical.create('offset < 0 '+inttostr(offset));

      FStream.BaseSeek(offset);
      FStream.BaseGuaranteeWrite(____ptr, Self.BytesToFlush);
    finally
{$IFDEF SECTSEEK}      lcs(Fstream.sectseek);{$ENDIF}
    end;
    ____dirty := false;
  end else begin
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
    Debug.Log('MFS has a mask for flushing.');
{$ENDIF}
    //look for beginning of zeros in mask
    iPos := 0;
    repeat

      //Find the first byte written
      iStart := FindMaskValue(iPos, $00);

      //find the last byte written after gap
      iEnd := FindMaskValue(iStart, $FF);

{$IFDEF DEBUG_MEMORY_FILE_STREAM}
      Debug.Log('Mask Range '+inttostr(iSTart)+'-'+inttostr(iEnd)+' flushing ends when start > '+ inttostr(Self.____highestwriteposition));
{$ENDIF}
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
      Debug.Log('Base seek to '+inttohex(offset+iStart,0));
{$ENDIF}
{$IFDEF SECTSEEK}      ecs(sstream.sectseek);{$ENDIF}
      try
        i := offset+iSTart;
        if i < 0 then
          raise ECritical.create('i<0 very bad i='+inttostr(i));
        FStream.BaseSeek(offset+iStart);
        //writing masked values
        FStream.BaseGuaranteeWrite(@____ptr[istart], iEnd-iStart);
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
begin



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
      if not self.____wasread then
        self.FetchFromStream(FStream)
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
      self.FetchFromStream(FStream);

  end;

  //normal buffer read operation
  if Stream.AllowReadPastEOF then begin
    iAvail :=  (self.Size) - iOffsetIntoBuffer;
    iAvail := lesserof(iAvail, iMaxLen);
  end else begin
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
  Debug.Log('Write requested to buffer: '+inttostr(iLen)+' bytes. Actual to be written is '+inttostr(result)+'. Size is '+inttostr(self.size));
{$ENDIF}
  if result < 0 then
    raise ECritical.create('Trying to write to a buffer that doesn''t represent this part of the file.');
  movemem32(@____ptr[iOffsetIntoBuffer], pIn, result);
  iTemp := (iOffsetIntoBuffer + result)-1;
  if iTemp > (____highestwriteposition) then begin
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
  ecs(sect);
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


