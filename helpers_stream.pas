unit helpers_stream;

{$INCLUDE DelphiDefs.inc}
interface

uses
  classes, sysutils, debug, numbers, systemx, variants, tickcount,
{$IFDEF MSWINDOWS}
  queuestream,
{$ENDIF}
  typex, ios.stringx.iosansi;

const
  STREAM_CHUNK_SIZE = 262144*8;

function Stream_GuaranteeWrite(const s: TStream; const p: PByte; const iSize: nativeint; iStartingPositionHint:int64 = 0; iAlign:int64 =65536): nativeint;overload;
function Stream_GuaranteeWrite(const s: TStream; const p: PByte; const iSize: nativeint; prog: PProgress; iStartingPositionHint:int64 = 0; iAlign:int64 =65536): nativeint;overload;
{$IFDEF MSWINDOWS}
function Stream_GuaranteeWrite(const s: TAdaptiveQueuedFileSTream; const p: PByte; const iSize: nativeint): nativeint;overload;
function Stream_GuaranteeWrite(const s: TAdaptiveQueuedSTream; const p: PByte; const iSize: nativeint): nativeint;overload;inline;
{$ENDIF}

function Stream_GuaranteeRead_NoExceptions(const s: TStream; const p: PByte; const iSize: nativeint): nativeint;
{$IFDEF WINDOWS}
function Stream_GuaranteeRead(const s: TUnbufferedFileStream; const p: PByte; const iSize: nativeint; const bThrowExceptions: boolean = true): nativeint;overload;inline;
{$ENDIF}
function Stream_GuaranteeRead(const s: TStream; const p: PByte; const iSize: nativeint; const bThrowExceptions: boolean = true): nativeint;overload;inline;
function Stream_GuaranteeRead(const s: TStream; const p: PByte; const iSize: nativeint; prog: PProgress; const bThrowExceptions: boolean = true): nativeint;overload;
{$IFDEF WINDOWS}
function Stream_GuaranteeRead(const s: TAdaptiveQueuedSTream; const p: PByte; const iSize: nativeint; const bThrowExceptions: boolean = true; bForget: boolean = false): nativeint;overload;
function Stream_GuaranteeRead(const s: TAdaptiveQueuedFileSTream; const p: PByte; const iSize: nativeint; const bThrowExceptions: boolean = true): nativeint;overload;
function Stream_GuaranteeRead_Begin(const s: TAdaptiveQueuedFileSTream; const p: PByte; const iSize: nativeint; const bThrowExceptions: boolean = true; bForget: boolean = false): TObject;overload;inline;
function Stream_GuaranteeRead_End(const s: TAdaptiveQueuedFileStream; const o: TObject): nativeint;
{$ENDIF}

function Stream_GuaranteeCopy(const sFrom: TStream; const sTo: Tstream; const iSize: int64 = -1): int64;overload;
function Stream_GuaranteeCopy(const sFrom: TStream; const sTo: Tstream; prog: PPRogress; const iSize: int64 = -1): int64;overload;
{$IFDEF WINDOWS}
function Stream_GuaranteeCopy(const sFrom: TAdaptiveQueuedSTream; const sTo: TAdaptiveQueuedFileSTream; const iSize: int64): int64;overload;
function Stream_GuaranteeCopy(const sFrom: TAdaptiveQueuedFileSTream; const sTo: TAdaptiveQueuedFileSTream; const iSize: int64): int64;overload;
{$ENDIF}
procedure Stream_Grow(const s: TStream; iSize: int64);overload;
{$IFDEF WINDOWS}
procedure Stream_Grow(const s: TAdaptiveQueuedFileStream; iSize: int64);overload;
{$ENDIF}

function stream_Compare(const s1, s2: TStream): integer;overload;
function stream_Compare(const s1, s2: TStream; out dif_addr: int64): integer;overload;

procedure ScrambleFile(f1: string);

function Stream_CalculateChecksum(const s: TStream): int64;overload;
function Stream_CalculateChecksum(const s: TStream; start, length: int64): int64;overload;
function LoadFileAsByteArray(sFile: string): TDynByteArray;
function OleVariantToMemoryStream(OV: OleVariant): TMemoryStream;

function Stream_ReadString(const s: TStream; terminator: byte = 10; nobacktrack: boolean = false): ansistring;



{$IFDEF WINDOWS}
procedure Stream_WriteZeros(const s: TUnbufferedFileStream; const iCount: int64);overload;
{$ENDIF}
procedure Stream_WriteZeros(const s: TStream; const iCount: int64);overload;
{$IFDEF WINDOWS}
procedure Stream_WriteZeros(const s: TAdaptiveQueuedFileSTream; const iCount: int64);overload;
procedure Stream_WriteZerosXX(const s: TAdaptiveQueuedFileSTream; const iCount: int64);
{$ENDIF}



type
  EStreamGuarantee = class(Exception)
  public
  end;

implementation


{$IFDEF MSWINDOWS}
procedure Stream_WriteZeros(const s: TUnbufferedFileStream; const iCount: int64);overload;
begin
  s.WriteBehindZeros(s.POsition, iCount);
end;
{$ENDIF}
procedure Stream_WriteZeros(const s: TStream; const iCount: int64);
var
  iJustWritten, iTotalWritten: int64;
  p: pbyte;
begin
  p := GetMemory(STREAM_CHUNK_SIZE);
  try
    FillMem(p, STREAM_CHUNK_SIZE, 0);
    iTotalWritten := 0;
    while iTotalWritten < iCount do begin
      iJustWritten := Stream_GuaranteeWrite(s, p, lesserof(STREAM_CHUNK_SIZE, iCount-iTotalWritten));
      iTotalWritten := iTotalWritten + iJustWritten;
    end;
  finally
    FreeMemory(p);
  end;


end;

function Stream_GuaranteeWrite(const s: TStream; const p: PByte; const iSize: nativeint; iStartingPositionHint:int64 = 0; iAlign:int64 =65536): nativeint;overload;
begin
  result := Stream_GuaranteeWrite(s, p, iSize, nil, iStartingPositionHint, iAlign);
end;
function Stream_GuaranteeWrite(const s: TStream; const p: PByte; const iSize: nativeint; prog: PProgress; iStartingPositionHint:int64 = 0; iAlign:int64 =65536): nativeint;
var
  ijustwrote: integer;
  iLEft: int64;
  wptr:pbyte;
  iToAlign: int64;
  blocksize: int64;
  iToWrite: int64;
begin
  if iSize = 1 then begin
    ijustwrote := s.Write(p^, 1);
    if (ijustwrote = 0) then
      raise EStreamGuarantee.create('Unable to guarantee WRITE of SINGLE BYTE in '+s.classname+' at position '+inttostr(s.Position)+'.  where size='+inttostr(s.Size));
    exit(iJustWrote);
  end;


  result := 0;
  iLeft := iSize;
  if iSize = 0 then  exit;
  wptr := p;
  //blocksize := 65536;
  blocksize := 262144*128;
  iToAlign := blocksize;
  if iAlign > 0 then begin
    blocksize := iAlign;
    iToAlign := iAlign - (iStartingPositionHint mod iAlign);
  end;

  if prog <> nil then begin
    prog.step := 0;
    prog.stepcount := iSize;
  end;
  while iLEft > 0 do begin
    iToWrite := iLEft;
    if itowrite = 0 then
      iToWrite := blocksize;
    ijustwrote := s.Write(wptr^, iToWrite);
    iToAlign := blocksize;//after first chunk, write entire blocks
    if ijustwrote <= 0 then begin
      if s is TFileStream then
        raise EStreamGuarantee.create('Unable to guarantee write to: '+TFileStream(s).FileName+' Err='+inttostr(GetLAstError)+' Pos='+inttostr(s.position)+' Size='+inttostr(s.size)+' Len='+inttostr(iSize)+' After Written='+inttostr(ni(wptr-ni(p))))
      else
        raise EStreamGuarantee.create('Unable to guarantee write Err='+inttostr(GetLAstError)+' Pos='+inttostr(s.position)+' Size='+inttostr(s.size)+' Len='+inttostr(iSize)+' After Written='+inttostr(ni(wptr-ni(p))))
    end;
    inc(wptr, iJustWrote);
    dec(iLeft, ijustwrote);
    if prog <> nil then begin
      inc(prog.step, iJustWrote);
    end;
  end;

  result := ni(wptr-ni(p));

end;


function Stream_GuaranteeRead(const s: TStream; const p: PByte; const iSize: nativeint; const bThrowExceptions: boolean = true): nativeint;
begin
  result := Stream_GuaranteeRead(s, p, iSize, nil, bThrowExceptions);
end;


function Stream_GuaranteeRead(const s: TStream; const p: PByte; const iSize: nativeint; prog: PProgress; const bThrowExceptions: boolean = true): nativeint;overload;
var
  iLeft, iRead, iJustRead: int64;
  op, rptr: Pbyte;
begin
  //if reading just 1 byte, use a simpler path (faster)

  if iSize = 1 then begin
    iJustRead := s.Read(p^, 1);
    if (iJustRead = 0) and bThrowExceptions then
      raise EStreamGuarantee.create('Unable to guarantee read of SINGLE BYTE in '+s.classname+' at position '+inttostr(s.Position)+'.  where size='+inttostr(s.Size));
    exit(iJustRead);
  end;
  //ELSE do the more complex stuff
  rptr := p;
  if rptr = nil then
    rptr  := GetMemory(iSize);
  op := rptr;
  try
    result := 0;
    if iSize = 0 then  exit;
    iREad := 0;
    iLEft := iSize;
    if prog <> nil then begin
      prog.stepcount := iSize;
    end;
    while iLeft > 0 do begin
      if prog <> nil then begin
        prog.step := result;
      end;
  {$IFDEF LOCAL_DEBUG}    Debug.Log('Inner read in Stream_GuaranteeRead ToGo:'+inttostr(nativeint(rptr-ni(p))), 'helpers_stream');{$ENDIF}
      //Debug.Log('Read@'+inttostr(s.position)+' where size is '+inttostr(s.size));
      iJustread := s.Read(rptr^, iLeft);

  {$IFDEF LOCAL_DEBUG}    Debug.Log('Inner read in Stream_GuaranteeRead JustRead:'+inttostr(iJustRead), 'helpers_stream');{$ENDIF}
      inc(rptr, iJustRead);
      dec(iLEft, iJustRead);
      inc(result, iJustRead);

      //extra checking.
      if ijustread <= 0 then begin
        if bThrowExceptions then
          raise EStreamGuarantee.create('Unable to guarantee read of '+s.classname+' at position '+inttostr(s.Position)+' after ' +inttostr(nativeint(rptr-ni(p)))+' bytes.  (justread='+ijustread.tostring()+') where size='+inttostr(s.Size))
        else begin
          result := 0;
          exit;
        end;
      end;
    end;
  finally
    if p = nil then begin
      FreeMemory(op);
    end;
  end;
end;

function Stream_GuaranteeRead_NoExceptions(const s: TStream; const p: PByte; const iSize: nativeint): nativeint;
var
  iLeft, iRead, iJustRead: int64;
  rptr: Pbyte;

begin
  //if reading just 1 byte, use a simpler path (faster)
  if iSize = 1 then begin
    iJustRead := s.Read(p^, 1);
    exit(iJustRead);
  end;
  result := 0;
  if iSize = 0 then  exit;
  iREad := 0;
{$IFDEF LOCAL_DEBUG}  Debug.Log('Stream_GuaranteeRead cnt:'+inttostr(iSize), 'helpers_stream');{$ENDIF}
  rptr := p;
  iLEft := iSize;
  while iLeft > 0 do begin
{$IFDEF LOCAL_DEBUG}    Debug.Log('Inner read in Stream_GuaranteeRead ToGo:'+inttostr(nativeint(rptr-ni(p))), 'helpers_stream');{$ENDIF}
    //Debug.Log('Read@'+inttostr(s.position)+' where size is '+inttostr(s.size));
    iJustread := s.Read(rptr^, iLeft);

{$IFDEF LOCAL_DEBUG}    Debug.Log('Inner read in Stream_GuaranteeRead JustRead:'+inttostr(iJustRead), 'helpers_stream');{$ENDIF}
    inc(rptr, iJustRead);
    dec(iLEft, iJustRead);
    inc(result, iJustRead);
    if ijustread = 0 then begin
      exit;
    end;
  end;
end;




function Stream_GuaranteeCopy(const sFrom: TStream; const sTo: Tstream; const iSize: int64): int64;
begin
  result := Stream_GuaranteeCopy(sFrom, sTo, nil, iSize);
end;
function Stream_GuaranteeCopy(const sFrom: TStream; const sTo: Tstream; prog: PPRogress; const iSize: int64 = -1): int64;overload;
var
  iToRead, iRead, iJustRead: int64;
  p: pbyte;
//  bFromMB, bToMB: boolean;
begin
//  bFromMB := sFrom is TMultiBufferMemoryFileStream;
//  bToMB := sTo is TMultiBufferMemoryFileStream;


  p := GetMemory(65536);
  try
    iREad := 0;
    if iSize < 0 then begin
      if prog <> nil then begin
        prog.step := 1;
        prog.stepcount := 1;
      end;
      repeat


        iJustRead := sFrom.Read(p[0], 65536);
        Stream_GuaranteeWrite(sTo, p, iJustRead);
      until iJustRead = 0;
    end else
    while iRead < iSize do begin
      if prog <> nil then begin
        prog.step := iRead;
        prog.stepcount := iSize;
      end;
      iToRead := lesserof(iSize-iRead, 65536);
      iJustRead := Stream_GuaranteeRead(sFrom, p, itoRead);
      Stream_GuaranteeWrite(sto, p, iJustRead);
      inc(iRead, iJustRead);
    end;
  finally
    freememory(p);
  end;

  result := iSize;

end;


{$IFDEF MSWINDOWS}
function Stream_GuaranteeWrite(const s: TAdaptiveQueuedFileStream; const p: PByte; const iSize: nativeint): nativeint;overload;
begin
  s.AdaptiveWrite(p, iSize);
  result := iSize;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function Stream_GuaranteeWrite(const s: TAdaptiveQueuedStream; const p: PByte; const iSize: nativeint): nativeint;overload;
begin
  s.AdaptiveWrite(p, iSize);
  result := iSize;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function Stream_GuaranteeRead(const s: TAdaptiveQueuedFileSTream; const p: PByte; const iSize: nativeint; const bThrowExceptions: boolean = true): nativeint;overload;inline;
var
  qi: TReadCommand;
begin
  qi := s.BeginAdaptiveRead(p, iSize, false, false);
  result := s.EndAdaptiveRead(qi);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function Stream_GuaranteeRead_Begin(const s: TAdaptiveQueuedFileSTream; const p: PByte; const iSize: nativeint; const bThrowExceptions: boolean = true; bForget: boolean = false): TObject;overload;inline;
begin
  result := s.BeginAdaptiveRead(p, iSize, true, bForget);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function Stream_GuaranteeRead_End(const s: TAdaptiveQueuedFileStream; const o: TObject): nativeint;
begin
  result := s.EndAdaptiveRead( TReadCommand(o));
end;
{$ENDIF}


{$IFDEF MSWINDOWS}
function Stream_GuaranteeRead(const s: TAdaptiveQueuedSTream; const p: PByte; const iSize: nativeint; const bThrowExceptions: boolean = true; bForget: boolean = false): nativeint;overload;
var
  qi: TReadCommand;
begin
  qi := s.BeginAdaptiveRead(p, iSize, false);
  result := s.EndAdaptiveRead(iSize, qi);


end;
{$ENDIF}



{$IFDEF MSWINDOWS}
function Stream_GuaranteeCopy(const sFrom: TAdaptiveQueuedFileSTream; const sTo: TAdaptiveQueuedFileSTream; const iSize: int64): int64;overload;
const
  MOVESIZE: int64 = STREAM_CHUNK_SIZE;
var
  iMoved, iJustMoved, iToMove: int64;
  p: pbyte;
begin
  result := iSize;
  p := GetMemory(MOVESIZE);
  try
    iMoved := 0;
    while iMoved < iSize do begin
      iToMove := LesserOf(MOVESIZE, iSize-iMoved);
      iJustMoved := sFrom.EndAdaptiveRead(sFrom.BeginAdaptiveRead(p, iToMove, false, false));
      sTo.AdaptiveWrite(p, iJustMoved);
      inc(iMoved, iJustMoved);
    end;
  finally
    FreeMemory(p);
  end;
end;
{$ENDIF}


{$IFDEF MSWINDOWS}
function Stream_GuaranteeCopy(const sFrom: TAdaptiveQueuedSTream; const sTo: TAdaptiveQueuedFileSTream; const iSize: int64): int64;overload;
const
  MOVESIZE: int64 = 65536;
var
  iMoved, iJustMoved, iToMove: int64;
  p: pbyte;
begin
  result := iSize;
  p := GetMemory(MOVESIZE);
  try
    iMoved := 0;
    while iMoved < iSize do begin
      iToMove := LesserOf(MOVESIZE, iSize-iMoved);
      sFrom.EndAdaptiveRead(iToMove, sFrom.BeginAdaptiveRead(p, iToMove, false));
      sTo.AdaptiveWrite(p, iToMove);
      inc(iMoved, iToMove);
    end;
  finally
    FreeMemory(p);
  end;



end;
{$ENDIF}


{$IFDEF MSWINDOWS}
procedure Stream_WriteZeros(const s: TAdaptiveQueuedFileSTream; const iCount: int64);overload;
begin
  if iCount > int64(500)*BILLION then
    raise ECritical.create('ZeroWrite size is a bit insane');
  s.AdaptiveWriteZeroes(s.Position, iCount);



end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure Stream_WriteZerosXX(const s: TAdaptiveQueuedFileSTream; const iCount: int64);
var
  iJustWritten, iTotalWritten: int64;
  p: pbyte;
begin
  p := GetMemory(65536);
  try
    //FillMem(p, 65536, 0);
    iTotalWritten := 0;
    while iTotalWritten < iCount do begin
      iJustWritten := Stream_GuaranteeWrite(s, p, lesserof(65536, iCount-iTotalWritten));
      iTotalWritten := iTotalWritten + iJustWritten;
    end;
  finally
    FreeMemory(p);
  end;


end;
{$ENDIF}

function Stream_ReadString(const s: TStream; terminator: byte = 10; nobacktrack: boolean = false): ansistring;
var
  c: byte;
  a: array[0..63] of ansichar;
  iJustRead: ni;
  t: ni;
  ipos: ni;
  iPrevLen: ni;
begin
  result := '';
  if nobacktrack then begin
    result  := '';

    while s.Read(c, 1) > 0 do begin
      if (c <> 10) and (c <> 13) and (c <> terminator) then
        result := result + ansichar(c);

      if c = terminator then
          break;
    end;
  end else begin
    repeat
      iJustREad := s.read(a, 64);
      if iJustRead = 0 then break;
      iPos := iJustRead;
      for t:= 0 to iJustREad-1 do begin
        if ord(a[t]) = terminator then begin
          iPos := t;
          break;
        end;
      end;
      iPrevLen := length(result);

{$IFDEF NEED_FAKE_ANSISTRING}
    result.SetLength(iPrevLen+iPos);
    movemem32(result.addrof[strz+iPrevlen], @a[0], iPos);
{$ELSE}
    setlength(result, iPrevLen+iPos);
    movemem32(@result[strz+iPrevlen], @a[0], iPos);
{$ENDIF}

      if iPos < iJustRead then begin
        s.seek((iPos-iJustRead)+1, soCurrent);
        break;
      end;


    until false;

  end;

end;

procedure Stream_Grow(const s: TStream; iSize: int64);
begin
  s.Seek(0,soEnd);
  if s.Position < iSize then begin
    Stream_WriteZeros(s, iSize-s.Position);
  end;
end;

{$IFDEF MSWINDOWS}
procedure Stream_Grow(const s: TAdaptiveQueuedFileStream; iSize: int64);overload;
begin
  s.GrowFile(iSize);
end;
{$ENDIF}

function Stream_CalculateChecksum(const s: TStream): int64;
var
  iLeft: int64;
  l: array[0..65535] of int64;
  iJustRead: ni;
  isz: int64;
  t: ni;
begin
  s.seek(0,soBeginning);
  iLeft := s.Size;
  iSZ := iLeft;
  result := 0;
  while s.Position < iSZ do
  begin
    FillMem(@l[0], sizeof(l), 0);
    iJustRead := Stream_GuaranteeRead(s, PByte(@l), lesserof(iLeft, sizeof(l)));
    for t:= 0 to high(l) do begin//since we're just Adding, it is okay to read over end, because we initialized the data to 0
      result := result + l[t];
    end;
    dec(iLeft, iJustRead);
  end;
end;


function Stream_CalculateChecksum(const s: TStream; start, length: int64): int64;
var
  iLeft: int64;
  b: byte;
  t: ni;
  iJustRead: ni;
  isz: int64;
  a: array of byte;
  len: int64;
begin
  result := 0;
  if start >= s.size then
    exit;
  s.seek(start,soBeginning);
  iLeft := lesserof(s.Size-start, length);
  iSZ := iLeft;

  setlength(a,65536);

  while s.Position < iSZ do
  begin
    len := 65536;
    iJustRead := Stream_GuaranteeRead(s, PByte(@a[0]), lesserof(iLeft, len));
    for t:= 0 to iJustRead -1 do begin
      result := result + a[t];
    end;
    dec(iLeft, iJustRead);
  end;
end;



function LoadFileAsByteArray(sFile: string): TDynByteArray;
var
  fs: TFileStream;
begin
  fs := TFileStream.create(sFile, fmOpenRead+fmShareDenyNone);
  try
    setlength(result, fs.size);
    stream_GuaranteeRead(fs, @result[0], fs.size);

  finally
    fs.free;
  end;

end;

function stream_Compare(const s1, s2: TStream): integer;
var
  junk: int64;
begin
  result := stream_Compare(s1,s2,junk);
end;

function stream_Compare(const s1, s2: TStream; out dif_addr: int64): integer;overload;
var
  a,aa: array[0..255] of byte;
  b,bb: byte;
  cx: int64;
  iGot: ni;
  iGet: ni;
  iAt: int64;
  t: ni;

begin
  s1.seek(0,soBeginning);
  s2.seek(0,soBeginning);
  if s1.size < s2.size then
    exit(-1);
  if s2.size < s1.size then
    exit(1);

  cx := s1.size;
  iAT := 0;
  while cx > 0 do begin
    iGet := LesserOf(cx, sizeof(a));
    stream_guaranteeRead(s1, @a[0], iGet);
    stream_guaranteeRead(s2, @aa[0], iGet);
    iGot := iGet;
    dec(cx, iGot);
    for t:= 0 to iGot-1 do begin
      b := a[t];
      bb := aa[t];
      if b < bb then begin
        dif_addr := t+iAt;
        exit(-1);
      end;
      if b > bb then begin
        dif_addr := t+iAt;
        exit(1);
      end;
    end;
    inc(iAt, iGot);
  end;

  exit(0);


end;

{$IFDEF MSWINDOWS}
function Stream_GuaranteeRead(const s: TUnbufferedFileStream; const p: PByte; const iSize: nativeint; const bThrowExceptions: boolean = true): nativeint;overload;inline;
var
  iLeft, iRead, iJustRead: int64;
  rptr: Pbyte;

begin

  //if reading just 1 byte, use a simpler path (faster)
//  s.SeekLock;
  if iSize = 1 then begin
    iJustRead := s.Read(p^, 1);
    if (iJustRead = 0) and bThrowExceptions then
      raise EStreamGuarantee.create('Unable to guarantee read of SINGLE BYTE in '+s.classname+' at position '+inttostr(s.Position)+'.  where size='+inttostr(s.Size));
    exit(iJustRead);
  end;
  //ELSE do the more complex stuff
  rptr := p;
  result := 0;
  if iSize = 0 then  exit;
  iREad := 0;
  iLEft := iSize;

  while iLeft > 0 do begin

{$IFDEF LOCAL_DEBUG}    Debug.Log('Inner read in Stream_GuaranteeRead ToGo:'+inttostr(nativeint(rptr-ni(p))), 'helpers_stream');{$ENDIF}
    //Debug.Log('Read@'+inttostr(s.position)+' where size is '+inttostr(s.size));
    iJustread := s.Read(rptr^, iLeft);

{$IFDEF LOCAL_DEBUG}    Debug.Log('Inner read in Stream_GuaranteeRead JustRead:'+inttostr(iJustRead), 'helpers_stream');{$ENDIF}
    inc(rptr, iJustRead);
    dec(iLEft, iJustRead);
    inc(result, iJustRead);

    //extra checking.
    if ijustread = 0 then begin
      if bThrowExceptions then
        raise EStreamGuarantee.create('Unable to guarantee read of '+s.classname+' at position '+inttostr(s.Position)+' after ' +inttostr(nativeint(rptr-ni(p)))+' bytes.  where size='+inttostr(s.Size))
      else begin
        result := 0;
        exit(0);
      end;
    end;
  end;
end;
{$ENDIF}


function OleVariantToMemoryStream(OV: OleVariant): TMemoryStream;
{$IFDEF MINE}
var
  DataPtr : Pointer;
  len: ni;
begin
   result:=TMemoryStream.Create;
   try
    result.Seek(0,0);
//    if not (varType(data) = varArray) then
//      raise ECritical.create('this is not an array');
    len := length(data);
    raise ECritical.create('len = '+length(data).tostring);
    DataPtr :=VarArrayLock(Data);
    try
      result.WriteBuffer(DataPtr^,len); //Get the pointer to the variant variable.
    finally
      VarArrayUnlock(Data); //when you are done , you must call to VarArrayUnlock
    end;
  finally
    result.Seek(0,soBeginning);
  end;
end;
{$ELSE}
var
  Data: PByte;
  Size: ni;
  toWrite: ni;
  thisWrite: ni;
begin
  Result := TMemoryStream.Create;
  try

    Size := VarArrayHighBound (OV, 1) - VarArrayLowBound
      (OV, 1) + 1;
    result.Size := size;
    Data := VarArrayLock(OV);
    try
      Result.Position := 0;
      towrite := size;
      Result.WriteBuffer(Data^, Size);
    finally
      VarArrayUnlock(OV);
    end;
    result.Seek(0,soBeginning);
  except
    Result.Free;
    Result := nil;
    raise;
  end;
end;
{$ENdif}

procedure ScrambleFile(f1: string);
var
  garbage: int64;
  tmStarT: ticker;
begin
  begin
    var fs := TFileStream.create(f1, fmCReate);
    fs.free;
    exit;
  end;
  begin
    var fs := TFileStream.create(f1, fmOpenWrite);
    try
      fs.Seek(0, soBeginning);
      tmSTart := GetTicker;
      while fs.Position < fs.size do begin
        garbage := (random($7FFFFFFF) shl 32)+random($7fffffff);
        Stream_GuaranteeWrite(fs, @garbage, sizeof(garbage));
        if gettimesince(tmStart) > 10000 then
          exit;
      end;
      fs.size := fs.position;
    finally
      fs.free;
    end;
  end;
end;

end.



