unit MemoryFileStream;

interface

// upon fetching buffer
// 1. wait for command
// 2. if buffer needed is secondary buffer, swap buffers... else fetch into primary
{$INLINE AUTO}
{$DEFINE DEBUG_MEMORY_FILE_STREAM}


uses systemx, stringx, numbers, sysutils, classes, commandprocessor,typex,
{$IFDEF WINDOWS}
  winapi.windows,
  QueueStream,
{$ENDIF}
  debug, multibuffermemoryfilestream;

const
  MINIMUM_READ_GUARANTEE = 0;
//  MEMORY_STREAM_DEFAULT_BUFFER_SIZE = 4;
  MEMORY_STREAM_DEFAULT_BUFFER_SIZE = 256000*8;



// MAX_BUFFER_SIZE = 256000;

type
  EBaseStreamGuarantee = class(EXception);
  TTripleBufferMemoryFileStream = class; // forward

  TBufferInfo = record
  strict private
    Fappended_Length: integer;
    function GetAppendedLength: integer;
    function GetAppendableLength: integer;
  private
    procedure SetUsed(const Value: integer);
  public
    dirty: boolean;
    ptr: PByte;
    offset: int64;
    Fused: integer;
    allocatedSize: integer;
    appended: boolean;

    property appended_Length: integer read GetAppendedLength write FAppended_length;
    property appendable_Length: integer read GetAppendableLength;
    procedure Finalize;
    procedure Init;
    property used: integer read FUsed write SetUsed;
  end;

  PBufferInfo = ^TBufferInfo;

  TMemoryStreamcommand = class(TCommand)
  private
    FStream: TTripleBufferMemoryFileStream;
    FBuffer: PBufferInfo;
  public
    procedure InitExpense; override;
    property Stream: TTripleBufferMemoryFileStream read FStream write FStream;
    property Buffer: PBufferInfo read FBuffer write FBuffer;

  end;

  Tcmd_MFS_FlushAndPrefetch = class(TMemoryStreamcommand)
  protected
    FPosition: int64;
  public
    procedure DoExecute; override;
    property Position: int64 read FPosition write FPosition;
  end;

  TTripleBufferMemoryFileStream = class(TFileStream)
  private
    FBufferSize: integer;
    function GetBufferSize: integer;inline;
    procedure SetBufferSize(const Value: integer);
  protected
    FPrimaryBuffer: PBufferInfo;
    FSecondaryBuffer: PBufferInfo;
    FTertiaryBuffer: PBufferInfo;
    Buffers: array [0 .. 2] of TBufferInfo;
    sectSize: TCLXCriticalSection;

    iSeekPosition: int64;
    iFileSizeAtFetch: int64;
    iFileSize: integer;
    bgotsize: boolean;
    FSizeCached: int64;
    cmd: Tcmd_MFS_FlushAndPrefetch;

    function IsInBufferRange(iPosition: int64; buf: PBufferInfo): boolean;
    procedure Prefetch(iPosition: int64; iLength: int64 = 0);
    procedure FetchBuffer(iPosition: int64; iLength: integer = 0;
      bForce: boolean = false);

    function IsAfterEOF(iPosition: integer): boolean;inline;
    function GetSize_Native(): int64;inline;
    function GetSize_Slow(): int64;inline;
    function GetSize_Cached(): int64;inline;
    function GetSize: int64; override;
    procedure AllocateBufferIfNeeded(buf: PBufferInfo);inline;
    procedure LockSize;inline;
    procedure UnlockSize;inline;
  public
    constructor Create(const AFileName: string; Mode: Word); overload;
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal);
      overload;
    procedure Init;
    destructor Destroy; override;
    procedure FInalizeBuffers;
    procedure Sync_FlushBuffer(buf: PBufferInfo);
    procedure Sync_Fetch(iPosition: int64; buf: PBufferInfo);

    function BaseGuaranteeRead(p: PByte; iSize: integer; bThrowExceptions: boolean = true): integer;
    function BaseRead(var Buffer; Count: Longint): Longint;inline;
    function BaseWrite(const Buffer; Count: Longint): Longint;inline;
    procedure BaseWriteZeros(iStart, iEnd: int64);inline;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const offset: int64; Origin: TSeekOrigin): int64; override;
    property BufferSize: integer read GetBufferSize write SetBufferSize;

    procedure EndAllCommands;
    procedure SwapBuffers(b: PBufferInfo);
    procedure BeginFlushAndPrefetch;
    procedure FinishFlush;

  end;

  TMemoryFileStream = TMultiBufferMemoryFileStream;
//  TMemoryFileStream = TAdaptiveQueuedFileStream;

{$IFDEF WINDOWS}
  LocalStringType = UTF8String;
  localchar = char;
{$ELSE}
  LocalStringType = string;
  localchar = char;
{$ENDIF}

  TMemoryStringStream = class(TMultiBufferMemoryFileStream)
  private
    function GetLIne(idx: integer): string;
    procedure SetLine(idx: integer; const Value: string );
    function GetLineCount: integer;
  protected
    FIndexBuilt: boolean;
    FIndex: array of integer;
    function IndexBuilt: boolean;
    procedure BuildIndex;
    function SlowLIneCount: integer;
  public
    constructor Create(const AFileName: string; Mode: cardinal; Rights: Cardinal; Flags: cardinal);override;

    property Lines[idx: integer]: string read GetLIne write SetLine;
    default;
    property Count: integer read GetLineCount;
    procedure Add(ss: string);
    procedure LoadFromFile(sfile: string);
    procedure SaveToFile(sfile: string);
  end;



implementation

{ TTripleBufferMemoryFileStream }

procedure TTripleBufferMemoryFileStream.AllocateBufferIfNeeded(buf: PBufferInfo);
// allocates if the buffersize changes
begin
  if (buf.allocatedSize <> BufferSize) or (buf.ptr = nil) then begin
    if buf.ptr <> nil then begin
      Sync_FlushBuffer(buf);
      FreeMem(buf.ptr);
      buf.ptr := nil;
    end;
    if BufferSize = 0 then
      BufferSize := MEMORY_STREAM_DEFAULT_BUFFER_SIZE;
    GetMem(buf.ptr, BufferSize);
    buf.allocatedSize := BufferSize;
  end;
end;

function TTripleBufferMemoryFileStream.BaseGuaranteeRead(p: PByte; iSize: integer; bThrowExceptions: boolean = true): integer;
var
  iRead, iJustRead: integer;
begin
  result := 0;
  if iSize = 0 then  exit;
  iREad := 0;
  while iRead < iSize do begin
    {$IFDEF DEBUG_MEMORY_FILE_STREAM}
    Debug.Log(self,'About to read at base position '+inttostr(FileSeek(FHandle, 0, ord(soCurrent))));
    {$ENDIF}
    iJustread := BaseRead(p[iRead], iSize-iRead);
    if ijustread = 0 then begin
      if bThrowExceptions then
        raise EBaseStreamGuarantee.create('Unable to guarantee read at position '+inttostr(Position))
      else begin
        result := iRead;
        exit;
      end;
    end;
    inc(iRead, iJustRead);
  end;

  result := iRead;

end;


function TTripleBufferMemoryFileStream.BaseRead(var Buffer; Count: Longint): Longint;
begin
  Result := FileRead(FHandle, Buffer, Count);
  if Result = -1 then Result := 0;

end;

function TTripleBufferMemoryFileStream.BaseWrite(const Buffer; Count: Longint): Longint;
begin
  result := inherited Write(Buffer, Count);
end;

procedure TTripleBufferMemoryFileStream.BaseWriteZeros(iStart, iEnd: int64);
var
  a: array[0..65535] of byte;
  iPtr: integer;
  iToWrite: integer;
begin
//  inherited size := iEnd;
//  exit;
  fillmem(@a[0], 65536, $55);

  iPtr := istart;
  inherited Seek(iStart, soBeginning);

  repeat
    iToWrite := LesserOf(iEnd-iPtr, 65536);
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
//    Debug.Log('Appending '+inttostr(iToWrite)+' zeros @block:'+floatprecision(iStart,2)+' @ptr:'+inttostr(iPTR), 'MFS');
{$ENDIF}

    if iToWrite < 0 then break;


    iPtr := iPtr + BaseWrite(a[0], iToWrite);
  until iPtr >= iEnd;




end;

procedure TTripleBufferMemoryFileStream.BeginFlushAndPrefetch;
begin
  cmd := Tcmd_MFS_FlushAndPrefetch.Create;
  cmd.Stream := self;

  // setup a position adjacent to what we're looking at.
  cmd.Position := FPrimaryBuffer.offset + FPrimaryBuffer.used;
  if IsInBufferRange(cmd.Position, FSecondaryBuffer) then begin
    cmd.Position := -1;
    cmd.free;
    cmd := nil;
  end else
    cmd.Start;

end;

constructor TTripleBufferMemoryFileStream.Create(const AFileName: string; Mode: Word;
  Rights: Cardinal);
begin
  inherited Create(AFileName, Mode);
  Init;
end;

constructor TTripleBufferMemoryFileStream.Create(const AFileName: string; Mode: Word);
begin
  inherited Create(AFileName, Mode);
  Init;
end;

destructor TTripleBufferMemoryFileStream.Destroy;
begin
  FinalizeBuffers;

  DeleteCriticalSection(sectSize);
  inherited;
end;

procedure TTripleBufferMemoryFileStream.EndAllCommands;
begin
  if cmd <> nil then begin
    if not cmd.iscomplete then
      cmd.WaitFor;
    cmd.Free;
    cmd := nil;
  end;
end;

procedure TTripleBufferMemoryFileStream.Sync_Fetch(iPosition: int64; buf: PBufferInfo);
var
  b: boolean;
  itoRead: integer;
begin
  if iPosition < 0 then
    raise Exception.create('Cannot seek to a negative position! @'+inttohex(iPosition, 8));
  iPosition := (iPosition div BufferSize) * BufferSize;
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
  Debug.Log(self,extractfilename(self.filename)+' Prefetch @'+inttostr(iPosition)+' into @block:'+floatprecision(iPosition/BufferSize,2)+' @ptr'+inttostr(integer(pbyte(buf))),'MFS');
{$ENDIF}

  if iPosition < 0 then
    exit;

  b := IsAfterEOF(iPosition+(BufferSize-1));

  buf.appended := b;
//  if b then begin
//    if buf = FPrimaryBuffer then
//      buf.appended := b
//    else begin
//      buf.appended := false;
//      iPosition := 0;
//      buf.appended := IsAfterEOF(iPosition+(BufferSize-1));//!!!
//    end;
//  end;

  if buf.appended then begin
    AllocateBufferIfNeeded(buf);
    buf.used := self.Size - iPosition;
    buf.offset := iPosition;
    buf.appended_Length := 0;
    iToRead := buf.used;

    //read the stuff
    if buf.used > 0 then begin
//      while (inherited Seek(buf.offset, soBeginning)) <> buf.offset do
//        sleep(1);
      while (FileSeek(FHandle, buf.Offset, Ord(soBeginning))) <> buf.offset do
        sleep(1);
      buf.used := BaseGuaranteeRead(@buf.ptr[0], itoRead, false);
    end;
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
    Debug.Log(self,'Prefetch (append) got '+inttostr(buf.used)+' bytes ','MFS');
{$ENDIF}
    iFileSizeAtFetch := self.size;
  end
  else begin
    buf.offset := iPosition;
    AllocateBufferIfNeeded(buf);

    itoRead := BufferSize;

    //make sure we're not reading overlapping something pending commit
    if (buf <> FPrimaryBuffer) and (buf.offset < FPrimaryBuffer.offset) and
      (buf.offset + (BufferSize - 1) >= FPrimaryBuffer.offset) then begin
      itoRead := FPrimaryBuffer.offset - buf.offset;
    end;

    //make sure we're not reading overlapping something pending commit
    if (buf <> FSecondaryBuffer) and (buf.offset < FSecondaryBuffer.offset) and
      (buf.offset + (BufferSize - 1) >= FSecondaryBuffer.offset) then begin
      itoRead := FSecondaryBuffer.offset - buf.offset;
    end;

    //make sure we're not reading overlapping something pending commit
    if (buf <> FTertiaryBuffer) and (buf.offset < FTertiaryBuffer.offset) and
      (buf.offset + (BufferSize - 1) >= FTertiaryBuffer.offset) then begin
      itoRead := FTertiaryBuffer.offset - buf.offset;
    end;

    //read the stuff
    inherited Seek(buf.offset, soBeginning);
    buf.used := BaseGuaranteeRead(@buf.ptr[0], itoRead, false);
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
    Debug.Log(self,'Prefetch got '+inttostr(buf.used)+' bytes ','MFS');
{$ENDIF}
    iFileSizeAtFetch := self.size;
  end;
end;

procedure TTripleBufferMemoryFileStream.FetchBuffer(iPosition: int64; iLength: integer = 0;
  bForce: boolean = false);
begin
  AllocateBufferIfNeeded(FPrimaryBuffer);
  if (not bForce) and (IsInBufferRange(iPosition, FPrimaryBuffer)) then
    exit;

  // wait for all commands to end
  EndAllCommands;

  if IsInBufferRange(iPosition, FSecondaryBuffer) then begin
    SwapBuffers(FSecondaryBuffer);
    BeginFlushAndPrefetch;
  end
  else begin
    if IsInBufferRange(iPosition, FTertiaryBuffer) then begin
      SwapBuffers(FTertiaryBuffer);
      BeginFlushAndPrefetch;
    end
    else begin
      SwapBuffers(FPrimaryBuffer);
      Sync_Fetch(iPosition, FPrimaryBuffer);
      BeginFlushAndPrefetch;
    end;
  end;

  AllocateBufferIfNeeded(FPrimaryBuffer);

end;

procedure TTripleBufferMemoryFileStream.FInalizeBuffers;
begin
  EndAllCommands;
  Sync_FlushBuffer(FPrimaryBuffer);
  Sync_FlushBuffer(FSecondaryBuffer);
  Sync_FlushBuffer(FTertiaryBuffer);

  if assigned(FPrimaryBuffer) then
    if FPrimaryBuffer.ptr <> nil then begin
      FPrimaryBuffer.finalize;
//      FPrimaryBuffer.appended := 0;

    end;

  if assigned(FSecondaryBuffer) then
    if FSecondaryBuffer.ptr <> nil then begin
      FSecondaryBuffer.finalize;
//      FSecondaryBuffer.appended := 0;

    end;

  if assigned(FTertiaryBuffer) then
    if FTertiaryBuffer.ptr <> nil then begin
      FTertiaryBuffer.finalize;
//      FTertiaryBuffer.appended := 0;


    end;
end;

procedure TTripleBufferMemoryFileStream.FinishFlush;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TTripleBufferMemoryFileStream.Sync_FlushBuffer(buf: PBufferInfo);
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

  itotalWritten := 0;
  if buf.appended then begin
    iTotalToWrite := buf.appended_Length+buf.used;
    bgotsize := false;
    LockSize;
  end
  else
    iTotalToWrite := buf.used;

  //make sure that if buf.offset is after the end of the file, we expand the file out to that point
  //to do this ultra-efficiently we will
  LockSize;
  TRY
    iSize := inherited Size;
    if buf.offset > iSize then begin

      BaseWriteZeros(iSize, buf.offset);
    end;
  FINALLY
    UnlockSize;
  end;
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
  Debug.Log(self,'begin flush @'+inttostr(buf.offset)+' size:'+inttostr(iTotalToWrite),'MFS');
{$ENDIF}
  repeat
    inherited Seek(buf.offset, soBeginning);
    ijustWritten := inherited Write(buf.ptr[itotalWritten], iTotalToWrite - itotalWritten);
    inc(itotalWritten, ijustWritten);
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
    Debug.Log(self,'wrote '+inttostr(iJustWritten)+' total:'+inttostr(itotalWritten),'MFS');
{$ENDIF}
  until itotalWritten = iTotalToWrite;
{$IFDEF DEBUG_MEMORY_FILE_STREAM}
  Debug.Log(self,'flush complete','MFS');
{$ENDIF}

  buf.dirty := false;

  if buf.appended then
    UnlockSize;
end;

procedure TTripleBufferMemoryFileStream.UnlockSize;
begin
  LeaveCriticalSection(sectSize);
end;

procedure TTripleBufferMemoryFileStream.Init;
begin
  ics(sectSize);
  FPrimaryBuffer := @Buffers[0];
  FSecondaryBuffer := @Buffers[1];
  FTertiaryBuffer := @Buffers[2];
  Buffers[0].offset := -1;
  Buffers[1].offset := -1;
  Buffers[2].offset := -1;

end;

function TTripleBufferMemoryFileStream.IsAfterEOF(iPosition: integer): boolean;
begin
  result := iPosition >= self.size;
end;

function TTripleBufferMemoryFileStream.IsInBufferRange(iPosition: int64;
  buf: PBufferInfo): boolean;
var
  localguarantee: int64;
begin
  AllocateBufferIfNeeded(buf);
  localguarantee := MINIMUM_READ_GUARANTEE;
  if (iPosition + localguarantee) > size then begin
    localguarantee := size - Position;
    if localguarantee < 0 then
      localguarantee := 0;
  end;

//  if appended then begin
  if buf.offset >= 0 then begin
    result := (iPosition >= buf.offset) and
      (iPosition < ((buf.offset + (buf.used+ buf.appendable_Length) - localguarantee)));
  end else
    result := false;

//  end else begin
//    result := (iPosition >= buf.offset) and
//      (iPosition < ((buf.offset + buf.used) - localguarantee));
//
//  end;
end;

procedure TTripleBufferMemoryFileStream.LockSize;
begin
  ecs(sectSize);
end;

procedure TTripleBufferMemoryFileStream.Prefetch(iPosition: int64; iLength: int64 = 0);
begin
// raise exception.Create('unimplemented');

end;

function TTripleBufferMemoryFileStream.Read(var Buffer; Count: Longint): Longint;
var
  iCanRead: integer;
  pb: PByte;
begin
  // check that the read buffer is in the right spot and move it if not
  FetchBuffer(iSeekPosition);

  // calculate the actual number of bytes we can read
  iCanRead := (FPrimaryBuffer.used + FPrimaryBuffer.appended_Length) - (iSeekPosition - FPrimaryBuffer.offset);
  iCanRead := LesserOf(iCanRead, Count);

  // Read stuff from the read buffer
  pb := @byte(Buffer);
  MoveMem32(pb, @self.FPrimaryBuffer.ptr
      [iSeekPosition - FPrimaryBuffer.offset], iCanRead);
  iSeekPosition := iSeekPosition + iCanRead;

  // return number of bytes we were actually able to read
  result := iCanRead;
end;

function TTripleBufferMemoryFileStream.Seek(const offset: int64;
  Origin: TSeekOrigin): int64;
begin
  if Origin = soBeginning then begin
    iSeekPosition := offset;
    result := iSeekPosition;
  end;

  if Origin = soEnd then begin
    iSeekPosition := size - offset;
    result := iSeekPosition;
  end;

  if Origin = soCurrent then begin
    inc(iSeekPosition, offset);
    result := iSeekPosition;
  end;

  result := iSeekPosition;

end;

procedure TTripleBufferMemoryFileStream.SetBufferSize(const Value: integer);
begin
  FinalizeBuffers;
  FbufferSize := value;

end;

procedure TTripleBufferMemoryFileStream.SwapBuffers(b: PBufferInfo);
var
  pb: PBufferInfo;
  t: integer;
begin
  if b = FPrimaryBuffer then begin
    pb := FPrimaryBuffer;
    FPrimaryBuffer := FTertiaryBuffer;
    FTertiaryBuffer := FSecondaryBuffer;
    FSecondaryBuffer := pb;
  end
  else if b = FSecondaryBuffer then begin
    pb := FPrimaryBuffer;
    FPrimaryBuffer := FSecondaryBuffer;
    FSecondaryBuffer := pb;
  end
  else begin
    pb := FPrimaryBuffer;
    FPrimaryBuffer := FTertiaryBuffer;
    FTertiaryBuffer := FSecondaryBuffer;
    FSecondaryBuffer := pb;
  end;

end;

function TTripleBufferMemoryFileStream.Write(const Buffer; Count: Longint): Longint;
var
  iCanWrite: integer;
  pb: PByte;
  iTemp: integer;
begin
  // make sure the buffer is in window
  FetchBuffer(iSeekPosition);

  // determine how many bytes we can write
  iCanWrite := FPrimaryBuffer.used + FPrimaryBuffer.appendable_length - (iSeekPosition - FPrimaryBuffer.offset);
  iCanWrite := LesserOf(iCanWrite, Count);

  // write stuff into the buffer
  pb := PByte(@Buffer);
  MoveMem32(@self.FPrimaryBuffer.ptr[iSeekPosition - FPrimaryBuffer.offset],
    pb, iCanWrite);

  iSeekPosition := iSeekPosition + iCanWrite;

  FPrimaryBuffer.dirty := true;

  result := iCanWrite;

  if (FPrimaryBuffer.appended) then begin
    iTemp := iSeekPosition - FPrimaryBuffer.offset;
    if (iTemp > FPrimaryBuffer.appended_Length) then
      FPrimaryBuffer.appended_Length := iTemp;
  end;

end;

function TTripleBufferMemoryFileStream.GetBufferSize: integer;
begin
  result := FBufferSize;
  if result = 0 then
    result := MEMORY_STREAM_DEFAULT_BUFFER_SIZE;
end;

function TTripleBufferMemoryFileStream.GetSize(): int64;
begin
  result := GetSize_Cached;
end;

function TTripleBufferMemoryFileStream.GetSize_Cached: int64;
var
  al1, al2: integer;
  t: integer;
begin
  //EndAllcommands;
  LockSize;
  try
    if bgotsize then begin
      al1 := 0;
      for t:= 0 to 2 do begin
        al1 := al1 + buffers[t].appended_Length
      end;

      result := FSizeCached + al1;

    end
    else begin
      FSizeCached := GetSize_Slow;
      bgotsize := true;
      result := FSizeCached;
    end;
  finally
    Unlocksize;
  end;
end;

function TTripleBufferMemoryFileStream.GetSize_Native: int64;
var
  Pos: int64;
begin
  Pos := inherited Seek(0, soCurrent);
  result := inherited Seek(0, soEnd);
  inherited Seek(Pos, soBeginning);
end;

function TTripleBufferMemoryFileStream.GetSize_Slow: int64;
var
  Pos: int64;
begin
  Pos := inherited Seek(0, soCurrent);
  result := inherited Seek(0, soEnd);
  inherited Seek(Pos, soBeginning);
end;

{ TMemoryStringStream }

procedure TMemoryStringStream.Add(ss: string);
{$IFDEF WINDOWS}
var
  s: RawByteString;
begin
  s := UTF8Encode(ss);
  s := s + #13#10;
  Seek(0, soEnd);
  FIndex[length(FIndex) - 1] := Position;
  Write(s[1], length(s) * sizeof(s[1]));
  setlength(FIndex, length(FIndex) + 1);
  FIndex[length(FIndex) - 1] := Position;

end;
{$ELSE}
var
  s: string;
begin
  s := ss;
  s := s + NEWLINE;
  Seek(0, soEnd);
  FIndex[length(FIndex) - 1] := Position;
  Write(s[1], length(s) * sizeof(s[1]));
  setlength(FIndex, length(FIndex) + 1);
  FIndex[length(FIndex) - 1] := Position;
end;
{$ENDIF}

procedure TMemoryStringStream.BuildIndex;
var
  t: integer;
  c: localchar;
  i: integer;
  iOB: integer;
begin
  iOB := BufferSize;
  BufferSize := 50000000;
  try
    Seek(0, soBeginning);
    setlength(FIndex, SlowLIneCount + 1);
    i := 1;
    FIndex[0] := 0;
    Seek(0, soBeginning);
    for t := 0 to size - 1 do begin
      read(c, 1);
      if c = #10 then begin
        FIndex[i] := t + 1;
        inc(i);
      end;
    end;
    FIndexBuilt := true;
  finally
    BufferSize := iOB;
  end;

// Findex[i] := size;
end;

constructor TMemoryStringStream.Create(const AFileName: string; Mode: cardinal; Rights: Cardinal; Flags: cardinal);
begin
  inherited Create(AFileName, Mode, rights,flags);
  if Mode = fmCreate then
    FIndexBuilt := true;

end;

function TMemoryStringStream.GetLIne(idx: integer): string;
{$IFDEF WINDOWS}
var
  t: integer;
  c: localchar;
  r: string;
  res: RawByteString;
begin
  if not IndexBuilt then
    BuildIndex;

  setlength(res, FIndex[idx + 1] - FIndex[idx]);
  Seek(FIndex[idx], soBeginning);
  for t := 1 to length(res) do begin
    Read(c, 1);
    res[t] := c;
  end;
  result := UTF8Decode(res);

  r := result;
  r := StringReplace(r, #13, '', [rfReplaceall]);
  r := StringReplace(r, #10, '', [rfReplaceall]);
  result := r;
end;
{$ELSE}
var
  t: integer;
  c: localchar;
  r: string;
  res: string;
begin
  if not IndexBuilt then
    BuildIndex;

  setlength(res, FIndex[idx + 1] - FIndex[idx]);
  Seek(FIndex[idx], soBeginning);
  for t := 1 to length(res) do begin
    Read(c, 1);
    res[t] := c;
  end;
  result := UTF8Decode(ansistring(res));

  r := result;
  r := StringReplace(r, #13, '', [rfReplaceall]);
  r := StringReplace(r, #10, '', [rfReplaceall]);
  result := r;
end;
{$ENDIF}

function TMemoryStringStream.GetLineCount: integer;
begin
  if not IndexBuilt then
    BuildIndex;

  result := length(FIndex) - 1;
end;

function TMemoryStringStream.IndexBuilt: boolean;
begin
  result := FIndexBuilt;
end;

procedure TMemoryStringStream.LoadFromFile(sfile: string);
begin
  // kept only for stringlist compatability;
end;

procedure TMemoryStringStream.SaveToFile(sfile: string);
begin
  // kept only for stringlist compatability;
end;

procedure TMemoryStringStream.SetLine(idx: integer; const Value: string);
begin
  raise exception.Create('unimplemented');
end;

function TMemoryStringStream.SlowLIneCount: integer;
var
  t: integer;
  c: localchar;
  i: integer;
begin
  Seek(0, soBeginning);
  i := 0;
  for t := 0 to size - 1 do begin
    read(c, sizeof(c));
    if c = #10 then begin
      inc(i);
    end;
  end;
  result := i;
end;

{ Tcmd_Prefetch }

{ TMemoryStreamcommand }

procedure TMemoryStreamcommand.InitExpense;
begin
  inherited;
  cpuexpense := 0;
end;

{ Tcmd_Flush }

{ Tcmd_MFS_FlushAndPrefetch }

procedure Tcmd_MFS_FlushAndPrefetch.DoExecute;
begin
  inherited;
  Self.Status := 'Block:'+inttostr(Position div Stream.BufferSize)+' '+Stream.FileName;
  Stream.Sync_FlushBuffer(Stream.FTertiaryBuffer);
  Stream.Sync_Fetch(Position, Stream.FTertiaryBuffer);
end;

{ TBufferInfo }

procedure TBufferInfo.Finalize;
begin
  if ptr <> nil then
    FreeMem(ptr);

  used := 0;
  ptr := nil;
  dirty := false;
  allocatedsize := 0;
  offset := -1;

end;

function TBufferInfo.GetAppendableLength: integer;
begin
  result := allocatedSize - used;
end;

function TBufferInfo.GetAppendedLength: integer;
begin
  if appended then
    result := Fappended_Length
  else
    result := 0;
end;

procedure TBufferInfo.Init;
begin
  used := 0;
  ptr := nil;
  dirty := false;
  allocatedsize := 0;
  offset := -1;

end;

procedure TBufferInfo.SetUsed(const Value: integer);
begin
  FUsed := Value;
  if fused < 0 then
    FUsed := 0;

end;

end.



