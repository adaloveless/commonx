unit systemx;
{$INCLUDE DelphiDefs.inc}
interface
{$IFNDEF WINDOWS}
  {$DEFINE USE_SYNCOBJS}
{$ENDIF}
{$DEFINE USE_SYNCOMMONS}
{$DEFINE HOB_NOASM}
{$IFDEF DEAD_LOCK_DEBUG}
  {$DEFINE DEBUG_BLOCKING}
{$ENDIF}
{x$DEFINE ALWAYS_SINGLE_BYTE_MASK_MOVE}

uses
  typex, numbers,
{$IFDEF IOS}
  Macapi.CoreFoundation, Macapi.CoreServices, macapi.mach,
  iosapi.foundation,
{$ENDIF}
{$IFDEF MSWINDOWS}
  windows,
  USock,
{$ELSE}
  stringx.ansi,
{$ENDIF}

  {$IFNDEF LAZ}
  ioutils,
  {$ENDIF}
  variants,
  classes,
  {$IFDEF LAZ}
  syncobjs,
  {$ELSE}
  system.SyncObjs,
  {$ENDIF}
  {$IFDEF USE_SYNCOMMONS}
  //SynCommons,
  {$ENDIF}
  sysutils;

type
  TAlignedTempSpace = record
    aligned: Pbyte;
    memsize: ni;
    procedure Allocate;
    procedure Unallocate;

  end;

  PAlignedTempSpace = ^TalignedTempSpace;

  TLogicalProcessorInformation = record
    LogicalProcessorCount : integer;
    NumaNodeCount : integer;
    ProcessorCoreCount : integer;
    ProcessorL1CacheCount : integer;
    ProcessorL2CacheCount : integer;
    ProcessorL3CacheCount : integer;
    ProcessorPackageCount : integer;
  end;

{$IFDEF USE_SYNCOBJS}

  TCLXCriticalSection = TCriticalSection;
  _SYSTEM_INFO = record
    dwNumberOfProcessors: nativeint;
  end;
{$ELSE}
  TCLXCriticalSection = _RTL_CRITICAL_SECTION;
{$ENDIF}


function StringToAnsiByteArray(s: string): TDynByteArray;
function StringToAnsiByteArray_NULL_TERMINATED(s: string): TDynByteArray;
function AnsiByteArrayToString(a: TDynByteArray): string;
function AnsiPointerToString(a: Pbyte): string;
function GetWriteablePath: string;
function FindDeployedFile(sName: string): string;

procedure FreeListContents(list: TList);
function GetCurrentThreadID: cardinal;
function DLLName: string;
function DLLPath: string;
procedure MoveMem32(const D,S: pointer; const Size: ni);inline;
procedure MoveMem_Up(const DD,SS: pointer; const Size: ni);
procedure MoveMem_Down(const D,S: pointer; const Size: ni);
procedure MoveMem32Xor(const D,S: pointer; const size: ni);inline;
procedure MoveMem32WithMask(D,S,M: pointer; Size: integer);
procedure MoveMem32WithMask_NOT(D,S,M: pointer; Size: integer);
function JStr(s: string): string;
procedure ZeroMemory(p: Pbyte;length: ni);inline;
procedure FillMem(p:pbyte; length:integer; fill:byte);
function StrToIntEx(s: string): int64;
procedure FillMemRandom(p:pbyte; length:integer);
procedure FillMemInc(p:pbyte; length:integer);
function IsAllZeros(p: pbyte; length: ni): boolean;
function GetTempPath: string;
function GetDownloadsFolder: string;
function GEtPDFPath: string;
function GetTempPathForThread: string;overload;
function GetComputerName: string;
function Get7BitEncodedBufferLength(i8BitLength: nativeint): nativeint;
function Get7BitEncodedBuffer8bitLength(i7BitLength: nativeint): nativeint;
function Get8BitEncodedBufferLength(i7BitLength: nativeint): nativeint;
procedure Convert7BitsTo8Bits(input: pbyte; output: pbyte; InputLength: nativeint);
function Convert8BitsTo7Bits(input: PByte; inputSize: ni; output: PByte; outputSize: ni): boolean;
procedure XOREncrypt(d: PByte; sz: ni; key: Pbyte; keysize: ni);
procedure XORDecrypt(d: PByte; sz: ni; key: Pbyte; keysize: ni);

function GetLogicalDrives: DWORD; stdcall;
function GetVolumeInformation(lpRootPathName: PChar;
  lpVolumeNameBuffer: PChar; nVolumeNameSize: DWORD; lpVolumeSerialNumber: PDWORD;
  var lpMaximumComponentLength, lpFileSystemFlags: DWORD;
  lpFileSystemNameBuffer: PChar; nFileSystemNameSize: DWORD): BOOL; stdcall;


function InterlockedIncrement(var whatever: integer): integer;inline;
function InterlockedDecrement(var whatever: integer): integer;inline;

function InitializeCriticalSection(var sect: TCLXCriticalSection): Integer;stdcall;
function EnterCriticalSection(var sect: TCLXCriticalSection): Integer;stdcall;
function LeaveCriticalSection(var sect: TCLXCriticalSection): Integer;stdcall;
function TryEnterCriticalSection(var sect: TCLXCriticalSection): Boolean;stdcall;
function DeleteCriticalSection(var sect: TCLXCriticalSection): Integer;stdcall;
function PathSeparator: char;
function ComplyFilePath(const sPath: string; sSlash: string = ''): string;
function ResolveRelativePath(const sPAth: string): string;
function Slash(const sPath: string; sSlashType: string = ''): string;
function UnSlash(const sPath: string): string;
function ExtractNetworkRoot(s: string): string;
function GetnumberofPhysicalProcessors: integer;
function GetNumberOfLogicalProcessors: integer;
function GetCPUCount: integer;
function GetCPUThreadCount: integer;
function GetEnabledCPUCount: int64;
function CountSetBits(bitMask : int64) : ni;
function xGetProcessorCount: integer;deprecated;
procedure CopyFile(sSource: string; sTarget: string; bFailIfExists: boolean = false);
procedure BitSet(byt: pbyte; bit: nativeint; bState: boolean);inline;
function BitGet(byt: pbyte; bit: nativeint): boolean;inline;
procedure MaskSet(byt: pbyte; startingbit: ni; bitwidth: ni; value: ni);
function MaskGet(byt: pbyte; startingbit: ni; bitwidth: ni): ni;
function ConcatinateMemory(p1_willNOTbefreed: pointer; p1_size: nativeint; p2_willNOTbefreed:pointer; p2_size:nativeint): Pbyte;
function ConcatinateMemory_AndFree(p1_willbefreed: pointer; p1_size: nativeint; p2_willbefreed:pointer; p2_size:nativeint): Pbyte;
procedure SaveMemoryAsFile(m: pointer; size: integer; sFile: string);
procedure LoadFileIntoMemory(m: pointer; size: integer; sFile: string);
function HighOrderBit(Value:integer):integer;overload;
function HighOrderBit(const Value:int64):int64;overload;
function HighOrderBit(const Value:TGiantInt):int64;overload;
function NearPower2(value:int64): int64;
function RAndomOrder(iItems: ni): TDynInt64Array;



function LowOrderBit(Value:NativeInt):NativeInt;
function HextoMemory(s: string): TDynByteArray;
function boolToInt(b: boolean): nativeint;
function MemoryToHex(p: pointer; iSize: integer): string;
function MemoryToHexAndASCII(p: pointer; iSize: integer): string;
procedure FreeAndNil(var o: TObject);
function GetCheckSumForVariant(v: variant): integer;
function GetCheckSumForString(s: string): cardinal;
function MemoryDebugString(p: pointer; length: integer): string;
function ExtractFileNamePart(sFile: string): string;
procedure sysBeep(freq, dur: ni);
procedure NotImplemented;

procedure ics(var cs: TCLXCriticalSection);inline;
procedure icssc(var cs: TCLXCriticalSection; spin: ni);inline;
procedure dcs(var cs: TCLXCriticalSectioN);inline;
procedure ecs(var cs: TCLXcriticalSection);inline;
procedure lcs(var cs: TCLXcriticalSection);inline;
function tecs(var cs: TCLXcriticalSection; iTimeout: integer=0): boolean;inline;
procedure ecs2(var cs1,cs2: TCLXCRiticalSection; retryInterval: ni);inline;
procedure lcs2(var cs1,cs2: TCLXCRiticalSection);inline;
function ResolveFileName(sFile: string): string;
function RenameFile_Verify(source, dest: string; iRetryCount: nativeint = 1000): boolean;

function SecondsToTimeString(r:nativefloat): string;
function StrtoDate_Better(s: string): TDateTime;
function UTCToDateTime(d: TDateTime): TDateTime;
function UTCStringToDateTime(d: string): TDateTime;
function DatetoStr_Better(d: TDateTime): string;
function DateTimeToUTC(d: TDateTime): TDateTime;
function GetPaddedSize(iSize: nativeint; iPad: nativeint): nativeint;
function CompareMemEx(const p1,p2: Pbyte; iCompareSize: ni; out pDifferent: ni; out iDifferentSize: ni): boolean;
function StrToInt64X(s: string): int64;
procedure ConsoleLog(s: string);inline;
function FileNameCompare(const s1, s2: string): boolean;
procedure Beep(Freq, Duration: integer);
function GetSystemDir(bNative: boolean = false): string;
procedure FindClose(var sr: TSearchRec);
function GetFreeSpaceOnPath(sPath: string): int64;
function GetKeyboardShiftState: TShiftSTate;
function DebugHeapStatus: string;
function FileCreateX(const FileName: string; Mode: LongWord; Rights: Integer; Flags: cardinal): THandle;
function FileOpenX(const FileName: string; Mode: LongWord; Flags: cardinal): THandle;
function strtoni(s: string): ni;inline;
procedure CalculateChecksum(p: pbyte; l: ni; out iSum, iXor: int64);overload;
procedure CalculateChecksum(p: pbyte; l: ni; out iSum: int64);overload;

procedure pmm(d: pointer; s: pointer; sz: NativeUInt; src_bucket_start: pointer; src_bucket_sz: NativeUInt; dst_bucket_start: pointer; dst_bucket_sz: NativeUInt);
function Isfile(sPathOrFileName: string): boolean;
function FileDateToDateTimeEx(fd: longint): TDateTime;
function Varsame(v1,v2: variant): boolean;
function stridelength(p1,p2: pointer): ni;inline;
function LocalTimeToGMT(dt: TDateTime): TDateTime;
function GMTtoLocalTime(dt: TDateTime): TDatetime;
{$IFDEF MSWINDOWS}
function GetLogicalProcessorInfo: TLogicalProcessorInformation;
{$ENDIF}


function FileSeek64(Handle: THandle; offset: int64; origin: ni): int64;

procedure AlertMemoryPattern(patternPtr: Pbyte; patternSz: ni; poolPointer: Pbyte; poolSz: ni);
function FindInMemory(patternPtr: Pbyte; patternSz: ni; poolPointer: Pbyte; poolSz: ni): ni;


function GetPhysicalMemory: int64;
{$IFDEF MSWINDOWS}
{$IFNDEF CPUX86}
function WinGetPhysicallyInstalledSystemMemory(var TotalMemoryInKilobytes: int64): Boolean; stdcall;
//{$EXTERNALSYM WinGetPhysicallyInstalledSystemMemory}
{$ENDIF}
{$ENDIF}


//type              S
//  TBinarySearchFunctionEval = function(test: int64; data: pointer): nativeint;
//  TBinarySearchFunctionEvalOfObject = function(test: int64): nativeint of object;
//
//function BinarySearch_Procedural(func: TBinarySearchFunctionEVal; data: pointer): nativeint;
//function BinarySearch(func: TBinarySearchFunctionEValOfObject): nativeint;

type
  TBetterPriority = (bpIdle, bpLowest, bpLower, bpNormal, bpHigher, bpHighest,
    bpTimeCritical);

{$IFDEF MSWINDOWS}
const
  BetterPriorities: array[0..6] of Integer =
   (ord(tpIdle), ord(tpLowest), ord(tpLower), ord(tpNormal), ord(tpHigher), ord(tpHighest), ord(tpTimeCritical));
{$ELSE}
const
  TPB = 50;
  BetterPriorities: array[0..6] of Integer =
   (99, 90, 70, 50, 40, 10, 1);

{$ENDIF}


function PlatformToBetterPriority(ordval: ni): TBetterPriority;
function BetterPriorityToPlatform(bp: TBetterPriority): integer;


{$IFNDEF MSWINDOWS}
var
  ios_lock: TCLXCriticalSection;
{$ENDIF}

procedure Sleep(ms: integer);




implementation



uses
{$IFDEF MSWINDOWS}
  betterobject,
{$ENDIF}
  orderlyinit,
  stringx,
  debug,
  tickcount;


{$IFDEF MSWINDOWS}
function GetLogicalProcessorInfo: TLogicalProcessorInformation;
var
  i: Integer;
  ReturnLength: DWORD;
  Buffer: array [0..2047] of TSystemLogicalProcessorInformation;
  cnt: ni;
  logicals: ni;
begin
  result.LogicalProcessorCount := 0;
  result.NumaNodeCount := 0;
  result.ProcessorCoreCount := 0;
  result.ProcessorL1CacheCount := 0;
  result.ProcessorL2CacheCount := 0;
  result.ProcessorL3CacheCount := 0;
  result.ProcessorPackageCount := 0;
  returnlength := sizeof(buffer);
  if not GetLogicalProcessorInformation(@Buffer[0], ReturnLength) then
  begin
    if GetLastError = ERROR_INSUFFICIENT_BUFFER then begin
      if not GetLogicalProcessorInformation(@Buffer[0], ReturnLength) then
        RaiseLastOSError;
    end else
      RaiseLastOSError;
  end;

  cnt := (ReturnLength div (SizeOf(TSystemLogicalProcessorInformation)));
  for i := 0 to cnt-1 do begin
    case Buffer[i].Relationship of
        RelationNumaNode: Inc(result.NumaNodeCount);
        RelationProcessorCore:
          begin
            logicals := CountSetBits(Buffer[i].ProcessorMask);
            if logicals > 0 then begin
              Inc(result.ProcessorCoreCount);
              result.LogicalProcessorCount := result.LogicalProcessorCount + logicals;
            end;
          end;
        RelationCache:
          begin
            if (Buffer[i].Cache.Level = 1) then Inc(result.ProcessorL1CacheCount)
            else if (Buffer[i].Cache.Level = 2) then Inc(result.ProcessorL2CacheCount)
            else if (Buffer[i].Cache.Level = 3) then Inc(result.ProcessorL3CacheCount);
          end;
        RelationProcessorPackage: Inc(result.ProcessorPackageCount);
        else
          raise Exception.Create('Error: Unsupported LOGICAL_PROCESSOR_RELATIONSHIP value: '+inttostr(ord(buffer[i].relationship)));
    end;
  end;
end;
{$ENDIF}


function BetterPriorityToPlatform(bp: TBetterPriority): integer;
var
  i: ni;
begin
  i := ord(bp);
  result := BetterPriorities[i];
end;

function PlatformToBetterPriority(ordval: ni): TBetterPriority;
var
  t: ni;
begin
  result := bpNormal;
  for t:= 0 to high(BetterPriorities) do
    if (BetterPriorities[t] = ordval) then exit(TBetterPriority(t));



end;

function FileSeek64(Handle: THandle; offset: int64; origin: ni): int64;
var
  high_order_stuff: dword;
begin
{$IFDEF MSWINDOWS}
  high_order_stuff := 0;
  Result := SetFilePointer(Handle, Offset, @high_order_stuff, Origin);
  result := result and (int64(high_order_stuff) shl 32);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  notImplemented;
  //Result := lseek(Handle, Offset, Origin);
{$ENDIF POSIX}
end;


function DLLName: string;
{$IFDEF IOS}
begin
  result := 'ios_app';
end;
{$ELSE}
var
  FileName : array[0..MAX_PATH] of WideChar;
begin
  FillChar(FileName, SizeOf(FileName), #0);
  GetModuleFileName(HInstance, FileName, SizeOf(FileName));
  Result:= FileName;
end;
{$ENDIF}


function DLLPath: string;
begin
  result := slash(ExtractFilePath(DLLName));
end;

procedure FillMem(p:pbyte; length:integer; fill:byte);
var
  t: integer;
  pp:pbyte;
begin
  for t := 0 to length - 1 do
  begin
    pp := p+t;
    pp^ := fill;
  end;

end;

procedure ZeroMemory(p: Pbyte;length: ni);inline;
begin
  FillMem(p, length, 0);
end;

procedure FillMemRandom(p:pbyte; length:integer);
var
  t: integer;
  pp:pbyte;
begin
  for t := 0 to length - 1 do
  begin
    pp := p+t;
    pp^ := random(255);
  end;

end;

procedure FillMemInc(p:pbyte; length:integer);
var
  t: integer;
  pp:pbyte;
begin
  for t := 0 to length - 1 do
  begin
    pp := p+t;
    pp^ := t and $FF;
  end;

end;




procedure XOREncrypt(d: PByte; sz: ni; key: Pbyte; keysize: ni);
var
  t, k: ni;
begin
  k := 0;
  for t := 0 to sz-1 do begin
    d[t] := d[t] xor key[k];
    inc(k);
    if k >= keysize then
      k := 0;
  end;
end;

procedure XORDecrypt(d: PByte; sz: ni; key: Pbyte; keysize: ni);
begin
  //SAME as encrypt
  XOREncrypt(d,sz, key, keysize);
end;

procedure MoveMem32(const D,S: pointer; const Size: ni);
//a: Jason Nelson
//High speed ASM memory copy.  Safe for overlapped memory regions.
//p: D: Destination Pointer
//p: S: Source Pointer
//p: Size: Size to move in bytes
{$IFDEF USE_SYNCOMMONS}
begin
  move(Pbyte(s)^,PBYte(d)^,size);
end;
{$ELSE}
{$IFDEF CPUX86}
asm
{     ->EAX     Pointer to source       }
{       EDX     Pointer to destination  }
{       ECX     Count                   }

        PUSH    ESI
        PUSH    EDI

        MOV     EDI,EAX
        MOV     ESI,EDX

        MOV     EAX,ECX

        CMP     EDI,ESI
        JA      @@down
        JE      @@exit

        SAR     ECX,2           { copy count DIV 4 dwords       }
        JS      @@exit

        REP     MOVSD

        MOV     ECX,EAX
        AND     ECX,03H
        REP     MOVSB           { copy count MOD 4 bytes        }
        JMP     @@exit

@@down:
        LEA     ESI,[ESI+ECX-4] { point ESI to last dword of source     }
        LEA     EDI,[EDI+ECX-4] { point EDI to last dword of dest       }

        SAR     ECX,2           { copy count DIV 4 dwords       }
        JS      @@exit
        STD
        REP     MOVSD

        MOV     ECX,EAX
        AND     ECX,03H         { copy count MOD 4 bytes        }
        ADD     ESI,4-1         { point to last byte of rest    }
        ADD     EDI,4-1
        REP     MOVSB
        CLD
@@exit:
        POP     EDI
        POP     ESI
end;
{$else}
{$IFDEF USE_ASM_64}
asm
{     ->EAX     Pointer to source       }
{       EDX     Pointer to destination  }
{       ECX     Count                   }

        PUSH    RSI
        PUSH    RDI

        MOV     RDI,RCX
        MOV     RSI,RDX
        MOV     RCX,R8

        MOV     RAX,R8

        CMP     RDI,RSI
        JA      @@down
        JE      @@exit

        SAR     RCX,3           { copy count DIV 8 dwords       }
        JS      @@exit

        REP     MOVSQ

        MOV     RCX,RAX
        AND     RCX,07H
        REP     MOVSB           { copy count MOD 8 bytes        }
        JMP     @@exit

@@down:
        LEA     RSI,[RSI+RCX-8] { point ESI to last dword of source     }
        LEA     RDI,[RDI+RCX-8] { point EDI to last dword of dest       }

        SAR     RCX,3           { copy count DIV 8 qwords       }
        JS      @@exit
        STD
        REP     MOVSQ

        MOV     RCX,RAX
        AND     RCX,07H         { copy count MOD 8 bytes        }
        ADD     RSI,8-1         { point to last byte of rest    }
        ADD     RDI,8-1
        REP     MOVSB
        CLD
@@exit:
        POP     RDI
        POP     RSI
end;
{$else}
VAR
  t: NativeInt;
  dd,ss: PNativeInt;
  bd,bs: PByte;
begin
  if size = 0  then
    exit;

  t := size;
  dd := PNativeInt(pbyte(D)+(size));
  ss := PNativeInt(pbyte(S)+(size));
  if t > SizeOf(NativeInt) then
  repeat
    dec(t,SizeOf(NativeInt));
    dd := PNativeInt(pbyte(dd)-SizeOf(NativeInt));
    ss := PNativeInt(pbyte(ss)-SizeOf(NativeInt));
    dd^ := ss^;
  until t < SizeOf(NativeInt);

  bd := PByte(dd);
  bs := PByte(ss);
  if t > 0 then
  repeat
    dec(t);
    bd := bd-1;
    bs := bs-1;
    bd^ := bs^;

  until t = 0;

end;
{$ENDIF}
{$ENDIF}
{$ENDIF}
{//p: D: Destination Pointer
//p: S: Source Pointer
//p: Size: Size to move in bytes
VAR
  t: NativeInt;
  dd,ss: PNativeInt;
  bd,bs: PByte;
begin
  if size = 0  then
    exit;

  t := size;

  bd := PByte(pbyte(D)+size);
  bs := PByte(pbyte(S)+size);

  //move single bytes until we're aligned on an 8 or 4 byte boundary (depending on x86/x64)
  if (t > 0) and ((t mod sizeof(nativeint)) > 0) then
  repeat
    dec(t);
    bd := bd-1;
    bs := bs-1;
    bd^ := bs^;

  until (t mod sizeof(nativeint)) = 0;


  //convert pointers to PNAtiveINT pointers so we will be moving 4 or 8 byte chunks from now on
  dd := PNativeInt(bd);
  ss := PNativeInt(bs);
  //move bigger chunks until 0
  if t >= SizeOf(NativeInt) then
  repeat
    dec(t,SizeOf(NativeInt));
    dd := PNativeInt(pbyte(dd)-SizeOf(NativeInt));
    ss := PNativeInt(pbyte(ss)-SizeOf(NativeInt));
    dd^ := ss^;
  until t =0;
end;}


procedure MoveMem32WithMask(D,S,M: pointer; Size: integer);
//p: D: Destination Pointer
//p: S: Source Pointer
//p: Size: Size to move in bytes
VAR
  t: NativeInt;
  dd,ss,mm: PNativeInt;
  bd,bs,bm: PByte;
  sz: nativeint;
const
{$IFDEF CPUX64}
  andmask: nativeint = 7;
{$ELSE}
  andmask: nativeint = 3;
{$ENDIF}
begin
  if size = 0  then
    exit;

  t := size;

  bd := PByte(pbyte(D)+size);
  bs := PByte(pbyte(S)+size);
  bm := PByte(pbyte(M)+size);

  //move single bytes until we're aligned on an 8 or 4 byte boundary (depending on x86/x64)
  if (t > 0) {$IFNDEF ALWAYS_SINGLE_BYTE_MASK_MOVE}and ((t and andMask) > 0){$ENDIF} then
  repeat
    dec(t);
    bd := bd-1;
    bs := bs-1;
    bm := bm-1;
    bd^ := ((bd^) and (bm^)) or ((bs^) and not (bm^));

  until (t {$IFNDEF ALWAYS_SINGLE_BYTE_MASK_MOVE}and andMask{$ENDIF}) = 0;


{$IFNDEF ALWAYS_SINGLE_BYTE_MASK_MOVE}
  //convert pointers to PNAtiveINT pointers so we will be moving 4 or 8 byte chunks from now on
  dd := PNativeInt(bd);
  ss := PNativeInt(bs);
  mm := PNativeInt(bm);
  sz := sizeof(Nativeint);
  //move bigger chunks until 0
  if t >= SizeOf(NativeInt) then
  repeat
    dec(t,sz);
    dd := PNativeInt(pbyte(dd)-sz);
    ss := PNativeInt(pbyte(ss)-sz);
    mm := PNativeInt(pbyte(mm)-sz);
    dd^ := ((dd^) and (mm^)) or ((ss^) and not (mm^));//destination will be ANDed with Mask, then ORed with source
  until t = 0;
{$ENDIF}
end;


procedure MoveMem32WithMask_NOT(D,S,M: pointer; Size: integer);
//p: D: Destination Pointer
//p: S: Source Pointer
//p: Size: Size to move in bytes
VAR
  t: NativeInt;
  dd,ss,mm: PNativeInt;
  bd,bs,bm: PByte;
  sz: nativeint;
const
{$IFDEF CPUX64}
  andmask: nativeint = 7;
{$ELSE}
  andmask: nativeint = 3;
{$ENDIF}
begin
  if size = 0  then
    exit;

  t := size;

  bd := PByte(pbyte(D)+size);
  bs := PByte(pbyte(S)+size);
  bm := PByte(pbyte(M)+size);

  //move single bytes until we're aligned on an 8 or 4 byte boundary (depending on x86/x64)
  if (t > 0) {$IFNDEF ALWAYS_SINGLE_BYTE_MASK_MOVE}and ((t and andMask) > 0){$ENDIF} then
  repeat
    dec(t);
    bd := bd-1;
    bs := bs-1;
    bm := bm-1;
    bd^ := ((bd^) and not (bm^)) or ((bs^) and (bm^));

  until (t {$IFNDEF ALWAYS_SINGLE_BYTE_MASK_MOVE}and andMask{$ENDIF}) = 0;


{$IFNDEF ALWAYS_SINGLE_BYTE_MASK_MOVE}
  //convert pointers to PNAtiveINT pointers so we will be moving 4 or 8 byte chunks from now on
  dd := PNativeInt(bd);
  ss := PNativeInt(bs);
  mm := PNativeInt(bm);
  sz := sizeof(NativeInt);
  //move bigger chunks until 0
  if t >= sz then
  repeat
    dec(t,sz);
    dd := PNativeInt(pbyte(dd)-sz);
    ss := PNativeInt(pbyte(ss)-sz);
    mm := PNativeInt(pbyte(mm)-sz);
    dd^ := ((dd^) and not (mm^)) or ((ss^) and (mm^));//destination will be ANDed with Mask, then ORed with source
  until t = 0;
{$ENDIF}
end;






function GetTickCount: cardinal;stdcall;
begin
  result := tickcount.GetTicker;

end;



function GetCurrentThreadID: cardinal;

begin

  {$IFNDEF WINDOWS}

  result :=   classes.TThread.CurrentThread.threadID;
  {$ELSE}
  {$IFDEF LINUX}

  result := Libc.GetCurrentThreadID;

  {$ELSE}

  result := windows.GetCurrentThreadID;

  {$ENDIF}
  {$ENDIF}

end;



function InitializeCriticalSection(var sect: TCLXCriticalSection): Integer;

begin

  {$IFDEF USE_SYNCOBJS}
    sect := TCriticalSection.Create;
    result := 0;
  {$ELSE}
  {$IFDEF LINUX}

  result := Libc.InitializeCriticalSection(sect);

  {$ELSE}

  Windows.InitializeCriticalSection(sect);

  result := 0;

  {$ENDIF}
  {$ENDIF}


end;



function EnterCriticalSection(var sect: TCLXCriticalSection): Integer;

begin

  {$IFDEF USE_SYNCOBJS}
    sect.Enter;
  {$ELSE}
  {$IFDEF LINUX}

  result := Libc.EnterCriticalSection(sect);

  {$ELSE}

  Windows.EnterCriticalSection(sect);

  result := 0;

  {$ENDIF}
  {$ENDIF}

end;



function LeaveCriticalSection(var sect: TCLXCriticalSection): Integer;

begin

  {$IFDEF USE_SYNCOBJS}
    sect.Leave;
  {$ELSE}
  {$IFDEF LINUX}

  result := Libc.LeaveCriticalSection(sect);

  {$ELSE}

  Windows.LeaveCriticalSection(sect);



  result := 0;

  {$ENDIF}
  {$ENDIF}

end;



function TryEnterCriticalSection(var sect: TCLXCriticalSection): Boolean;
begin

  {$IFDEF USE_SYNCOBJS}
    result := sect.TryEnter;
  {$ELSE}
  {$IFDEF LINUX}

  result := Libc.TryEnterCriticalSection(sect);

  {$ELSE}

  result := Windows.TryEnterCriticalSection(sect);

  {$ENDIF}
  {$ENDIF}

end;



function DeleteCriticalSection(var sect: TCLXCriticalSection): Integer;
begin
  {$IFDEF USE_SYNCOBJS}
    sect.Free;
    sect := nil;
  {$ELSE}
  {$IFDEF LINUX}

  result := Libc.DeleteCriticalSection(sect);

  {$ELSE}

  Windows.DeleteCriticalSection(sect);

  result := 0;

  {$ENDIF}
  {$ENDIF}

end;



procedure Beep(Freq, Duration: integer);

begin

  {$IFDEF MACOS}
  {$ELSE}
  {$IFDEF LINUX}

  {$ELSE}

{$IFDEF BEEPER}
  beeper.Beep(Freq, Duration);
{$ENDIF}

  {$ENDIF}
  {$ENDIF}



end;





procedure Sleep(ms: integer);
begin

  {$IFNDEF WINDOWS}
  SysUtils.Sleep(ms);
  {$ELSE}
  {$IFDEF LINUX}

  SysUtils.Sleep(ms);

  {$ELSE}

  Windows.Sleep(ms);

  {$ENDIF}
  {$ENDIF}



end;



function FileGetAttr(sFileName: string): integer;

begin

  {$IFDEF MACOS}
  {$ELSE}
  {$IFDEF LINUX}

  result := 0;

  {$ELSE}

  result := Sysutils.FileGetAttr(sFileName);

  {$ENDIF}
  {$ENDIF}



end;



procedure FileSetAttr(sFileName: string; Attr: integer);
begin
  {$IFDEF MSWINDOWS}
  Sysutils.FileSetAttr(sFileName, Attr);
  {$ELSE}
  //todo 3: implement?
  {$ENDIF}



end;



function GetModuleFileName(hModule: HINST; lpFilename: PChar; nSize: DWORD): DWORD; stdcall;
begin

  {$IFDEF MSWINDOWS}
  result := Windows.GetModuleFileName(hModule, lpFileName, nSize)
  {$ELSE}
  result := 0;//todo 3: implement?

  {$ENDIF}



end;





function GetComputerName: string;
var
  sTemp: string;
  nSize: cardinal;
begin

  {$IFDEF MACOS}
  result := 'ios.stub.computername';
  {$ELSE}
  {$IFDEF LINUX}
  result := 'linux.stub.computername';
  {$ELSE}
  {$IFDEF ANDROID}
  result := 'android.stub.computername';
  {$ELSE}
  nSize := 1024;
  SetLength(sTemp, nsize);
  Windows.GetComputerName(@sTEmp[strz], nsize);
  sTemp := zcopy(sTemp, 0, nsize);
  result := sTemp;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;



function GetLogicalDrives: DWORD; stdcall;
begin

  {$IFDEF MSWINDOWS}
  result := Windows.GetLogicalDrives;
  {$ELSE}
  result := 0;
  {$ENDIF}

end;


function GetVolumeInformation(lpRootPathName: PChar;

  lpVolumeNameBuffer: PChar; nVolumeNameSize: DWORD; lpVolumeSerialNumber: PDWORD;
  var lpMaximumComponentLength, lpFileSystemFlags: DWORD;
  lpFileSystemNameBuffer: PChar; nFileSystemNameSize: DWORD): BOOL; stdcall;
begin
  {$IFDEF MSWINDOWS}
  result := Windows.GetVolumeInformation(lpRootPathName, lpVolumeNameBuffer, nVolumeNameSize, lpVolumeSerialNumber, lpMaximumComponentLength, lpFileSystemFlags, lpFileSystemNamebuffer, nFileSystemNameSize);
  {$ELSE}
  result := false;
  {$ENDIF}
end;


function GetLastError: cardinal;
begin
  {$IFDEF MSWINDOWS}
  result := Windows.GetLastError;
  {$ELSE}
  result := 0;
  {$ENDIF}
end;


function IsThreadTerminated: boolean;
var
  iHandle: cardinal;
begin
  {$IFDEF MSWINDOWS}
  iHandle := GetCurrentThreadID;
  result := WaitForSingleObject(iHandle, 0) = 0;
  {$ELSE}
  result := false;

  {$ENDIF}


end;


function InterlockedIncrement(var whatever: integer): integer;inline;
begin
{$IFNDEF WINDOWS}
  EnterCriticalSection(ios_lock);
  inc(whatever);
  result := whatever;
  LeaveCriticalSection(ios_lock);
{$ELSE}
  result := windows.InterlockedIncrement(whatever);
{$ENDIF}

end;

function InterlockedDecrement(var whatever: integer): integer;inline;
begin
{$IFNDEF WINDOWS}
  EnterCriticalSection(ios_lock);
  dec(whatever);
  result := whatever;
  LeaveCriticalSection(ios_lock);

{$ELSE}
  result := windows.InterlockedDecrement(whatever);
{$ENDIF}
end;








function GetTempPathForThread: string;overload;
begin
  result := GetTempPath+inttostr(GetCurrentThreadID)+'\';
end;

function GetDownloadsFolder: string;
begin
  result := slash(TPath.GetDownloadsPath);
end;

function GEtPDFPath: string;
begin
  result := slash(TPath.GetDownloadsPath);
end;

function GetTempPath: string;overload;
var
  sTemp: string;
{$IFNDEF IOS}
  iTemp: cardinal;
{$ENDIF}
begin

{$IFNDEF WINDOWS}
  result := GEtWriteablePath;
{$ELSE}
  SetLength(sTemp, 1024);
  result := slash(TPath.GetTempPath);
{$ENDIF}




end;


function GetWriteablePath: string;
begin
  result := TPath.GetHomePath + TPath.DirectorySeparatorChar + 'Documents'+ TPath.DirectorySeparatorChar;
end;

function FindDeployedFile(sName: string): string;
var
  s: string;
begin
  result := '';
  s := TPath.GetHomePath+TPath.DirectorySeparatorChar+'Documents'+TPath.DirectorySeparatorChar+ sName;
  if fileexists(s) then
    result := s;
end;




function PathSeparator: char;
begin
{$IFDEF MSWINDOWS}
  result := '\'
{$ELSE}
  result := '/';
{$ENDIF}
end;


function ResolveRelativePath(const sPAth: string): string;
var
  sl: TStringlist;
  function ResolvePass: boolean;
  var
    t: ni;
  begin
    for t:= 0 to sl.count-1 do begin
      if sl[t] = '..' then begin
        sl.delete(t);
        sl.Delete(t-1);
        exit(true);
      end;
    end;
    exit(false);
  end;
begin
  sl := TStringlist.create;
  try
    ParseString(sPAth, '\', sl);

    while resolvepass do ;

    result := UnParseString('\', sl);



  finally
    sl.free;
  end;


end;

function ComplyFilePath(const sPath: string; sSlash: string = ''): string;
var
  sFrom: string;
  sTo: string;
begin
  if sSlash = '' then begin
    sTo := PathSeparator;
    if sTo = '/' then
      sFrom := '\'
    else
      sFRom := '/';
  end else begin
    sTo := sSlash;
    if sTo = '/' then
      sFrom := '\'
    else
      sFrom := '/';
  end;


  result := StringReplace(sPath, sFrom, sTo, [rfReplaceAll]);

end;
function Slash(const sPath: string; sSlashType: string = ''): string;
begin
  if sSlashType = '' then
    sSlashType := PathSeparator;

  if sPath = '' then
    result := ''
  else begin
    result := ComplyFilePath(sPath, sSlashType);
    if (result[high(result)] <> sSlashType) then
      result := result+sSlashType;
  end;
end;



function UnSlash(const sPath: string): string;
begin
  if sPAth = '' then begin
    result := '';
    exit;
  end;
  result := ComplyFilePath(sPath);


  if (result[high(result)] = PathSeparator) then
    result := zcopy(result, 0, length(result)-1);
end;



function ExtractNetworkRoot(s: string): string;
{$IFDEF MSWINDOWS}
var
  ss,sLeft, sright, sJunk, sHost, sDrive: string;

  b: boolean;
begin

  if copy(s, 1,2) = '\\' then begin
     ss := copy(s,3,length(s));
    b := true;
  end else begin
    ss := s;
    b := false;
  end;

  if b then begin
    SplitString(ss,'\', sLeft, sRight);
    sHost := sLeft;
    splitString(sRight, '\', sDrive, sJunk);
    result := '\\'+sHost+'\'+sDrive+'\';
  end else begin
    SplitString(ss,'\', sLeft, sRight);
    shost := '';
    sDrive := sLeft;
    result := sDrive+'\';
  end;
end;
{$ELSE}
begin
  result := PathSeparator;
end;
{$ENDIF}





function GetNumberOfPhysicalProcessors: integer;
{$IFDEF MSWINDOWS}
var
  lpi: TLogicalProcessorInformation;
begin
  lpi := GetLogicalProcessorInfo;
  result := lpi.ProcessorCoreCount;
end;
{$ELSE}
begin
  result := TThread.ProcessorCount;
end;
{$ENDIF}

function GetCPUCount: integer;
begin
  result := GetEnabledCPUCount;
end;

function GetCPUThreadCount: integer;
begin
  result := GetNumberOfLogicalProcessors;
end;

function GetEnabledCPUCount: int64;
{$IFDEF MSWINDOWS}
var
  procafmask, sysafmask: int64;
  p,s: DWORD_PTR;
begin
  p := DWORD_PTR(@procafmask);
  s := DWORD_PTR(@sysafmask);
  GetProcessAffinityMask( GetCurrentProcess, p,s);
  result := countsetbits(int64(p));
end;
{$ELSE}
begin
  result := GetNumberOfLogicalProcessors;
end;
{$ENDIF}

function xGetProcessorCount: integer;
begin
  result := GetEnabledCPUCount;
end;

function GetNumberOfLogicalProcessors: integer;
{$IFDEF MSWINDOWS}
var
  lpi: TLogicalProcessorInformation;
begin
  lpi := GetLogicalProcessorInfo;
  result := lpi.LogicalProcessorCount;
end;
{$ELSE}
begin
  result := TThread.ProcessorCount;
end;
{$ENDIF}


function FileExists(sFile: string): boolean;
{$IFDEF IOS}
var
  fileManager: iosapi.foundation.NSFileManager;
  error: NSError;
begin
  //create the file manager
  fileManager := TNSFileManager.Create;

  //create error bucket
  error := TNSError.Create;

  result := fileManager.fileExistsAtPath(nsstr(sFile));
end;
{$ELSE}
  {$IFNDEF WINDOWS}
  begin
    result := TFile.Exists(sFile);
  end;
  {$ELSE}
  begin
    result := TFile.Exists(sFile);
  end;
  {$ENDIF}
{$ENDIF}

procedure DeleteFile(sFile: string);
{$IFDEF IOS}
var
  fileManager: iosapi.foundation.NSFileManager;
  error: NSError;
begin
  //create the file manager
  fileManager := TNSFileManager.Create;

  //create error bucket
  error := TNSError.Create;

  if fileManager.fileExistsAtPath(nsstr(sFile)) then begin
    TFile.Delete(sFile);
  end;
end;
{$ELSE}
  {$IFNDEF WINDOWS}
  begin
    TFile.Delete(sFile);
  end;
  {$ELSE}
  begin
    windows.DeleteFile(pchar(sFile));
  end;
  {$ENDIF}
{$ENDIF}

procedure CopyFile(sSource: string; sTarget: string; bFailIfExists: boolean = false);
{$IFDEF IOS}
var
  fileManager: iosapi.foundation.NSFileManager;
  error: NSError;
begin

  //create the file manager
  fileManager := TNSFileManager.Create;

  //create error bucket
  error := TNSError.Create;


  if fileManager.fileExistsAtPath(nsstr(sTarget)) then begin
    if bFailIfExists then begin
      raise Exception.create('File '+sTarget+' already exists.');
    end else begin
      TFile.Delete(sTarget);

      if error.code <> 0 then
        raise Exception.Create('Failed to remove file when overwriting '+sTarget+' NSError code:'+inttostr(error.code));
    end;
  end;

  TFile.Copy(sSource, sTarget);
end;
{$ELSE}
{$IFNDEF WINDOWS}
begin
  TFile.Copy(sSource, sTarget, not bFailIfExists);
end;
{$ELSE}
begin
  windows.copyfile(pchar(sSource), pchar(sTarget), bFailIfExists);
end;
{$ENDIF}
{$ENDIF}

{ TVolatileProgression }




procedure BitSet(byt: pbyte; bit: nativeint; bState: boolean);
begin
//  if (bit =0)  then
//    Debug.Log('Trap bit 0 set '+booltoint(bState).tostring);
  if bState then
    byt[0] := byt[0] or (1 shl bit)
  else
    byt[0] := byt[0] and (not (1 shl bit));
end;

function BitGet(byt: pbyte; bit: nativeint): boolean;
begin
  result := (byt[0] and (1 shl bit)) <> 0
end;

function ConcatinateMemory(p1_willNOTbefreed: pointer; p1_size: nativeint; p2_willNOTbefreed:pointer; p2_size:nativeint): Pbyte;
var
  sz: nativeint;
begin
  sz := p1_size + p2_size;
  if p1_willnotbefreed = nil then
    p1_size := 0;

  if p2_willnotbefreed = nil then
    p2_size := 0;


  result := GEtMemory(sz);
  if result=nil then begin
    raise Exception.create('out of memory trying to allocate '+inttostr(p1_size)+'+'+inttostr(p2_size)+' bytes of data.');
  end else begin
    if p1_willnotbefreed <> nil then begin
      MoveMem32(result, p1_willnotbefreed, p1_size);
    end;
    if p2_willnotbefreed <> nil then begin
      MoveMem32(result+p1_size, p2_willnotbefreed, p2_size);
    end;
  end;
end;

function ConcatinateMemory_AndFree(p1_willbefreed: pointer; p1_size: nativeint; p2_willbefreed:pointer; p2_size:nativeint): Pbyte;
var
  sz: nativeint;
begin
  if (p2_willbefreed = nil) then begin
    result := p1_willbefreed;
    exit;
  end;

  if (p1_willbefreed = nil) then begin
    result := p2_willbefreed;
    exit;
  end;

  sz := p1_size + p2_size;
  result := p1_willbefreed;
  result := system.REallocMemory(result, p1_size+p2_size);
  if result=nil then begin
    FreeMemory(p2_willbefreed);
    raise Exception.create('out of memory trying to reallocate '+inttostr(p1_size)+'+'+inttostr(p2_size)+' bytes of data.');
  end else begin
    //tag on the stuff on the end
    MoveMem32(result+p1_size, p2_willbefreed, p2_size);
    FreeMemory(p2_willbefreed);
  end;
end;


function IsRelative(sFile: string): boolean;
begin
{$IFDEF MSWINDOWS}
  result := (not (copy(sFile, STRZ,2) = '\\')) and (not (pos(':', sFile) >= STRZ));
{$ELSE}
  result := not ((copy(sFile, STRZ,1) = '/'));
{$ENDIF}
end;


procedure SaveMemoryAsFile(m: pointer; size: integer; sFile: string);
var
  fs: TFileStream;
  pb, pb2: PByte;
  iTotalWritten: integer;
begin
  if (m = nil) then
    raise EBetterException.create('Nil pointer in SaveMemoryAsFile');

  if not isRelative(ExtractFilePath(sFile)) then
    ForceDirectories(ExtractFilePath(sFile));
  fs := TFileStream.create(sFile, fmCreate);
  try
    pb := m;
    iTotalWritten := 0;
    while (iTotalWritten < size) do begin
      pb2 := @pb[iTotalWritten];
      inc(iTotalWritten, fs.Write(pb[iTotalWritten], LesserOf(size-iTotalWritten, 65536)));
    end;
  finally
    fs.free;
  end;

end;

procedure LoadFileIntoMemory(m: pointer; size: integer; sFile: string);
var
  fs: TFileStream;
  iTotalRead: integer;
  iJustRead: integer;
  pb: PByte;
begin
  fs := TFileStream.create(sFile, fmOpenRead+fmShareDenyNone);
  try
    iTotalRead := 0;
    pb := m;
    while (iTotalRead< size) do begin
      iJustRead := fs.Read(pb[iTotalRead], LesserOf(size-iTotalRead, 8192));
      if iJustread = 0 then
        raise EBetterException.create('Load file into memory reached end of file');
      inc(itotalRead, iJustRead);

    end;
  finally
    fs.free;
  end;
end;


function NearPower2(value:int64): int64;
begin
  result := 1 shl (highorderbit(value));
end;


function HighOrderBit(Value:integer):integer;
{$IFDEF NOASM}
{$DEFINE HOB32_NO_ASM}
{$ENDIF}
{$IFDEF HOB32_NO_ASM}
var
  t: int64;
  high: int64;
  m: int64;
begin
  result := -1;

  high := (sizeof(nativeint) * 8) -1;
  m := 1 shl high;

  for t:= high downto 0 do
  begin
    if 0<>(value and m) then begin
      result := t;
      exit;
    end;
    m := m shr 1;
  end;
end;
{$ELSE}
{$IFDEF CPUX64}
  asm
    bsr rdx,rcx  // find high order bit
    mov rax,rdx
  end;
  {$ELSE}
  asm
    bsr edx,eax  // find high order bit
    mov eax,edx
  end;
  {$ENDIF}
{$ENDIF}

function HighOrderBit(const Value:int64):int64;
var
  cx: int64;
  v: UINt64;
begin
  cx := 64;
  v := UINT64(value);
  repeat
    dec(cx);
    if (v and (int64(1) shl cx)) <> 0 then
      exit(cx);
  until cx = 0;

  exit(-1);



end;


function HighOrderBit(const Value:TGiantInt):int64;
var
  cx: int64;
  zone: ni;
  v: uint64;
begin
  zone := 3;
  v := value.i4;
  cx := 64;
  repeat
    dec(cx);
    if (v and (uint64(1) shl cx)) <> 0 then
      exit(cx*(zone shl 6));
  until cx = 0;

  zone := 2;
  v := value.i3;
  cx := 64;
  repeat
    dec(cx);
    if (v and (uint64(1) shl cx)) <> 0 then
      exit(cx*(zone shl 6));
  until cx = 0;

  zone := 1;
  v := value.i2;
  cx := 64;
  repeat
    dec(cx);
    if (v and (uint64(1) shl cx)) <> 0 then
      exit(cx*(zone shl 6));
  until cx = 0;

  zone := 0;
  v := value.i1;
  cx := 64;
  repeat
    dec(cx);
    if (v and (uint64(1) shl cx)) <> 0 then
      exit(cx);
  until cx = 0;


  exit(-1);
end;



{$IFDEF FUCK}
{$IFDEF NOASM}
{$DEFINE HOB64_NO_ASM}
{$ENDIF}
{$IFNDEF CPUX64}
{$DEFINE HOB64_NO_ASM}
{$ENDIF}
{$IFDEF HOB64_NO_ASM}
var
  t: integer;
  high: integer;
  m: integer;
begin
  result := -1;

  high := (sizeof(integer) * 8) -1;
  m := 1 shl high;

  for t:= high downto 0 do
  begin
    if 0<>(value and m) then begin
      result := t;
      exit;
    end;
    m := m shr 1;
  end;
end;
{$ELSE}
{$IFDEF CPUX64}
  asm
    bsr rdx,rcx  // find high order bit
    mov rax,rdx
  end;
  {$ELSE}
  asm
    bsr edx,eax  // find high order bit
    mov eax,edx
  end;
  {$ENDIF}
{$ENDIF}
{$ENDIF}


function LowOrderBit(Value:NativeInt):NativeInt;
{$IFDEF NOASM}
var
  t: nativeint;
  high: nativeint;
  m: nativeint;
begin
  result := -1;

  high := (sizeof(nativeint) * 8) -1;
  m := 1;

  for t:= 0 to high do
  begin
    if 0<>(value and m) then begin
      result := t;
      exit;
    end;
    m := m shl 1;
  end;
end;
{$ELSE}
{$IFDEF CPUX64}
asm
  bsf rdx,rcx  // find high order bit
  mov rax,rdx
end;
{$ELSE}
asm
  bsf edx,eax  // find high order bit
  mov eax,edx
end;
{$ENDIF}
{$ENDIF}


function MemoryToHex(p: pointer; iSize: integer): string;
var
  b: string;
  t: integer;
begin
  result := '';
  for t := 0 to iSize-1 do begin
    b := inttohex((pbyte(p))[t],2)+' ';
    result := result + b;
  end;

end;

function MemoryToHexAndASCII(p: pointer; iSize: integer): string;
var
  b: string;
  t: integer;
  c: ansichar;
begin
  result := '';
  for t := 0 to iSize-1 do begin
    c := ' ';
    if (pbyte(p)[t]) > $20 then
      c := pansichar(p)[t];

    b := inttohex((pbyte(p))[t],2)+'('+c+') ';
    result := result + b;
  end;

end;



function GetCheckSumForString(s: string): cardinal;
var
  t: integer;
begin
  result := 0;
  for t:= 1 to length(s) do begin
    result := result + ord(s[t]);
  end;
end;
function GetCheckSumForVariant(v: variant): integer;
var
  vt: integer;
begin
  result := 0;
  vt := VarType(v);
  case vt of
    varString: inc(result, GetCheckSumForString(v));
    varUSTring: inc(result, GetCheckSumForString(v));
    varShortInt,varInteger: result := result + v;
    varInt64: result := result + v;
    varSmallint: result := result + v;
    varWord: result := result + v;
    varByte: result := result + v;
    varDouble, varSingle: result := result + trunc(v*1000);
    varDate: result := result + trunc(v);
    varBoolean: if v then inc(result);
  else
    raise EClassException.create('Variant Type not handled in GetCheckSumFOrVariant')
  end;


end;

function MemoryDebugString(p: pointer; length: integer): string;
var
  pc: Pchar;
  t: integer;
begin
  result := memoryToHexAndASCII(p, length);

  exit;
//  result := '';
//  pc := p;
//  for t:= 0 to length-1 do begin
//    result := result+'['+inttohex(ord(pc[t]),2)+']';
//  end;



end;

function ExtractFileNamePart(sFile: string): string;
var
  sLeft, sRight: string;
begin
  result := ExtractFileName(sFile);
  SplitString(result, '.', result, sRight, true);
  if result = '' then
    result := sRight;


end;




{-------------------------------------------------------------------------}
procedure FreeAndNil(var o: TObject);
begin
  o.free;
  o := nil;
end;


procedure ics(var cs: TCLXCriticalSection);inline;
begin
  initializecriticalsection(cs);
end;
procedure icssc(var cs: TCLXCriticalSection; spin: ni);inline;
begin
{$IFDEF MSWINDOWS}
  InitializeCriticalSectionAndSpinCount(cs, spin);
{$ELSE}
  ics(cs);
{$ENDIF}
end;
procedure dcs(var cs: TCLXCriticalSectioN);inline;
begin
  deletecriticalsection(cs);
end;
procedure ecs(var cs: TCLXcriticalSection);inline;
begin
{$IFDEF DEBUG_BLOCKING}
  if not tecs(cs, 1000) then begin
    Debug.Log('blocked on thread '+inttostr(GetCurrentThreadID())+' by '+inttostr(cs.owningthread),'error');
{$ENDIF}
    entercriticalsection(cs);
{$IFDEF DEBUG_BLOCKING}
  end;
{$ENDIF}
end;
procedure lcs(var cs: TCLXcriticalSection);inline;
begin
  leavecriticalsection(cs);
end;
function tecs(var cs: TCLXcriticalSection; iTimeout: integer=0): boolean;inline;
var
  tm1, tm2: cardinal;
begin
  if iTimeOUt <= 0 then
    exit(TryEnterCriticalSection(cs));

  tm1 := GetTicker;
  result := true;
  while not TryEnterCriticalSection(cs) do begin
    sleep(1);
    tm2 := GetTicker;
    if tm2<tm1 then
      tm2 := tm1;

    if (tm2-tm1) > cardinal(iTimeOut) then begin
      result := false;
      break;
    end;
  end;

end;


function ResolveFileName(sFile: string): string;
begin
  if (pos(':', sFile) >=0) or (pos('\\',sFile) = 1) then
    result := sFile
  else
    result := dllpath+sFile;
end;

function RenameFile_Verify(source, dest: string; iRetryCount: nativeint = 1000): boolean;
begin
  if not fileexists(source) then begin
    result := false;
    exit;
  end;
  if fileexists(dest) and fileexists(source) then
    deletefile(dest);


  while not fileexists(dest) do begin
    RenameFile(source, dest);
    dec(iRetryCount);
    if iRetryCount <= 0 then begin
      result := false;
      exit;
    end;

  end;

  result := true;

end;


function SecondsToTimeString(r:nativefloat): string;
begin
  result := inttostr((round(r) div (60*60)) mod 60)+':'+padstring(inttostr((round(r) div 60) mod 60),'0',2)+':'+padstring(inttostr(round(r) mod 60),'0',2);

end;

function StrtoDate_Better(s: string): TDateTime;
var
  bIsGMT: boolean;

begin
  bIsGMT := false;
  if s ='' then
    raise Exception.Create('empty date passed to StrtoDate_Better()');

  if s[1] = 'U' then begin
    bIsGMT := true;
    s := copy(s, 2, length(s)-1);
  end;

  if pos('/', s) > 0 then begin
    s := floattostr(strtodatetime(s));
  end;

  if bIsGMT then
    result := UTCtoDatetime(strtofloat(s))
  else
{$IFDEF APPLY_DLS_BUG_FIX}
    result := strtofloat(s)+(1/24);
{$ELSE}
    result := strtofloat(s);
{$ENDIF}
end;



function UTCToDateTime(d: TDateTime): TDateTime;
{$IFDEF MSWINDOWS}
var
  tz: TTimeZoneInformation;
begin
  GettimeZoneInformation(tz);

  result := d -((tz.Bias(*+tz.Daylightbias*))/(60*24));

end;
{$ELSE}
begin
  raise Exception.Create('Error Message');
end;
{$ENDIF}

function UTCStringToDateTime(d: string): TDateTime;
var
  sYear, sMonth, sDay, sHour, sMinute, sSecond: string;
  sLeft, sRight: string;
  //Monday, 2006-01-25 12:50:43
begin
  SplitString(d, ', ', sLeft, sRight);
  SplitString(sRight, '-', sYear, sRight);
  SplitString(sRight, '-', sMonth, sRight);
  SplitString(sRight, ' ', sDay, sRight);
  SplitString(sRight, ':', sHour, sRight);
  SplitString(sRight, ':', sMinute, sRight);
  SplitString(sRight, ' ', sSecond, sRight);

  result := StrToDAteTime(sDay+'/'+sMonth+'/'+sYear+' '+sHour+':'+sMinute+':'+sSecond);


end;


function DatetoStr_Better(d: TDateTime): string;
begin
  result := 'U'+floattostr(DateTimeToUTC(d));
end;


function DateTimeToUTC(d: TDateTime): TDateTime;
{$IFDEF MSWINDOWS}
var
  tz: TTimeZoneInformation;
begin
  GettimeZoneInformation(tz);

  result := d +((tz.Bias(*+tz.Daylightbias*))/(60*24));


end;
{$ELSE}
begin
  raise ENotImplemented.create('Not Implemented');
end;
{$ENDIF}

function StrToInt64X(s: string): int64;
var
  t: nativeint;
begin
  result := 0;
  for t := low(s) to high(s) do begin
    result := (result * 10)+strtoint(s[t]);
    end;

end;

procedure ConsoleLog(s: string);
begin
{$IFDEF MSWINDOWS}
  windows.OutputDebugString(pchar(s));
{$ENDIF}
end;

function FileNameCompare(const s1, s2: string): boolean;
begin
  {$IFDEF MSWINDOWS}
    result := lowercase(s1) = lowercase(s2);
  {$ELSE}
    result := s1 = s2;
  {$ENDIF}
end;

function GetSystemDir(bNative: boolean = false): string;
{$IFDEF MSWINDOWS}
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  GetSystemDirectory(Buffer, MAX_PATH - 1);
  SetLength(Result, StrLen(Buffer));
  Result := Buffer;
  result := slash(result);
  if bNative then
    result := stringreplace(result, 'system32', 'sysnative', [rfReplaceAll, rfignoreCase]);
end;
{$ELSE}
begin
  raise ECritical.create('GetSystemDir not implemented for this platform.');
end;
{$ENDIF}


function GetPaddedSize(iSize: nativeint; iPad: nativeint): nativeint;
begin
  if iSize = 0 then
    result := 0
  else
    result := (((iSize - 1) div iPad)+1) * iPad;
end;

function CompareMemEx(const p1,p2: Pbyte; iCompareSize: ni; out pDifferent: ni; out iDifferentSize: ni): boolean;
var
  t,u: ni;
begin
  result := true;
  for t:= 0 to iCompareSize-1 do begin
    if (p1[t] <> p2[t]) then begin
      result := false;
      u := t;
      pDifferent := t;
      while u < iCompareSize do begin
        iDifferentSize := (u-t)+1;
        if p1[u] = p2[u] then begin
          exit;
        end else
          inc(u);
      end;
    end;
  end;

end;



procedure FindClose(var sr: TSearchRec);
{$IFDEF MSWINDOWS}
begin
  windows.FindClose(sr.findhandle);
end;
{$ELSE}
begin
  sysutils.findclose(sr);
end;
{$ENDIF}
function GetFreeSpaceOnPath(sPath: string): int64;
var
  totalspace, freeavailable: int64;
begin
{$IFDEF MSWINDOWS}
  result := 0;
  if SysUtils.GetDiskFreeSpaceEx(PChar(sPath), FreeAvailable, TotalSpace, nil) then begin
//    Writeln(TotalSpace div (1024*1024*1024), 'GB total');
//    Writeln(FreeAvailable div (1024*1024*1024), 'GB free');
    result := freeavailable;
  end;
{$ELSE}
  result := 1000000000;
{$ENDIF}



end;


function GetKeyboardShiftState: TShiftSTate;
{$IFDEF MSWINDOWS}
var
  ks: TKeyboardState;
{$endIF}
begin
{$IFDEF MSWINDOWS}
  GetKeyboardState(ks);
  result := [];
  if (ks[VK_SHIFT] and 128) <> 0 then
    result := result + [ssShift];
  if ((ks[VK_LMENU] and 128) <> 0) or ((ks[VK_RMENU] and 128) <> 0) then
    result := result + [ssAlt];

  if (ks[VK_CONTROL] and 128) <> 0 then
    result := result + [ssCtrl];
{$ELSE}
  result := [];
{$ENDIF}


end;


function DebugHeapStatus: string;
var
  hs: THeapStatus;
begin
{$IFDEF MSWINDOWS}
  hs := GetHEapStatus;

  result := 'Total Allocated: '+inttostr(hs.TotalAllocated)+#13#10;
  result := result + 'Total Committed: '+inttostr(hs.TotalCommitted)+#13#10;
  result := result + 'Total Free: '+inttostr(hs.TotalFree);
{$ELSE}
  result := 'heap status... blah';
{$ENDIF}

end;


function FileCreateX(const FileName: string; Mode: LongWord; Rights: Integer; Flags: cardinal): THandle;
{$IFDEF MSWINDOWS}
const
  Exclusive: array[0..1] of LongWord = (
    CREATE_ALWAYS,
    CREATE_NEW);
  ShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  Result := INVALID_HANDLE_VALUE;
  if (Mode and $F0) <= fmShareDenyNone then
    Result := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
      ShareMode[(Mode and $F0) shr 4], nil, Exclusive[(Mode and $0004) shr 2], FILE_ATTRIBUTE_NORMAL or flags, 0);
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
{$IFDEF LT_XE6}
const
  Exclusive: array[0..1] of LongWord = (
    0,
    O_EXCL);
  ShareMode: array[0..fmShareDenyNone shr 4] of Byte = (
    0,        //No share mode specified
    F_WRLCK,  //fmShareExclusive
    F_RDLCK,  //fmShareDenyWrite
    0);       //fmShareDenyNone
var
  FileHandle, Tvar: Integer;
  LockVar: flock;
  smode: Byte;
  Code: Integer;
  M:TMarshaller;
begin
  Result := INVALID_HANDLE_VALUE;
  if (Mode and $F0) <= fmShareDenyNone then
  begin
    FileHandle := Integer(__open(M.AsAnsi(FileName, CP_UTF8).ToPointer,
      O_RDWR or O_CREAT or O_TRUNC or Exclusive[(Mode and $0004) shr 2], Rights));
    if FileHandle = -1 then
      Exit;

    smode := Mode and $F0 shr 4;
    if ShareMode[smode] <> 0 then
    begin
      LockVar.l_whence := SEEK_SET;
      LockVar.l_start := 0;
      LockVar.l_len := 0;
      LockVar.l_type := ShareMode[smode];
      Tvar :=  fcntl(FileHandle, F_SETLK, LockVar);
      Code := errno;
      if (Tvar = -1) and (Code <> EINVAL) and (Code <> ENOTSUP) then
      // EINVAL/ENOTSUP - file doesn't support locking
      begin
        __close(FileHandle);
        Exit;
      end;
    end;
    Result := FileHandle;
  end;
end;
{$ELSE}
begin
  raise ECRitical.create('filecreateX is not supported on posix');
end;
{$ENDIF}
{$ENDIF POSIX}



function FileOpenX(const FileName: string; Mode: LongWord; Flags: cardinal): THandle;
{$IFDEF MSWINDOWS}
const
  AccessMode: array[0..2] of LongWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  Result := INVALID_HANDLE_VALUE;
  if ((Mode and 3) <= fmOpenReadWrite) and
    ((Mode and $F0) <= fmShareDenyNone) then
    Result := CreateFile(PChar(FileName), AccessMode[Mode and 3],
      ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL or Flags, 0);
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
{$IFDEF LT_XE6}
const
  ShareMode: array[0..fmShareDenyNone shr 4] of Byte = (
    0,        //No share mode specified
    F_WRLCK,  //fmShareExclusive
    F_RDLCK,  //fmShareDenyWrite
    0);       //fmShareDenyNone
var
  FileHandle, Tvar: Integer;
  LockVar: flock;
  smode: Byte;
  Code: Integer;
  M:TMarshaller;
begin
  Result := INVALID_HANDLE_VALUE;
  if FileExists(FileName) and
     ((Mode and 3) <= fmOpenReadWrite) and
     ((Mode and $F0) <= fmShareDenyNone) then
  begin
    FileHandle := __open(M.AsAnsi(FileName, CP_UTF8).ToPointer, (Mode and 3), FileAccessRights);
    if FileHandle = -1 then  Exit;

    smode := Mode and $F0 shr 4;
    if ShareMode[smode] <> 0 then
    begin
      LockVar.l_whence := SEEK_SET;
      LockVar.l_start := 0;
      LockVar.l_len := 0;
      LockVar.l_type := ShareMode[smode];
      Tvar := fcntl(FileHandle, F_SETLK, LockVar);
      Code := errno;
      if (Tvar = -1) and (Code <> EINVAL) and (Code <> ENOTSUP) then
      // EINVAL/ENOTSUP - file doesn't support locking!
      begin
        __close(FileHandle);
        Exit;
      end;
    end;
    Result := FileHandle;
  end;
end;
{$ELSE}
begin
  raise ECRitical.create('filecreateX is not supported on posix');
end;
{$ENDIF}
{$ENDIF POSIX}


procedure oinit;
begin
{$IFNDEF WINDOWS}
  InitializeCriticalSection(ios_lock);
{$ENDIF}

end;


//function BinarySearch_Procedural(func: TBinarySearchFunctionEVal; data: pointer): nativeint;
//var
//  ipos: nativeint;
//  test: int64;
//  iTemp: nativeint;
//begin
//  iPos := 63;
//  result := 0;
//
//  for iPos := 63 downto 0 do begin
//    test :=   result or (1 shl iPos);
//    iTemp := func(test,data);
//    if the function reports that the value is LOW then
//    if iTemp < 0 then
//      put a 1 in the result position
//      result := result or test;
//
//    if we're dead on, break
//    if iTemp = 0 then
//      break;
//  end;
//end;

{------------------------------------------------------------------------------}
procedure FreeListContents(list: TList);
var
  t: integer;
begin
  for t:= 0 to list.count-1 do begin
    TObject(list[t]).free;
  end;
  list.Clear;
end;

//function BinarySearch(func: TBinarySearchFunctionEValOfObject): nativeint;
//var
//  ipos: nativeint;
//  test: int64;
//  iTemp: nativeint;
//begin
//  iPos := 63;
//  result := 0;
//
//  for iPos := 62 downto 0 do begin
//    test :=   result or (int64(1) shl int64(iPos));
//    iTemp := func(test);
//    if the function reports that the value is LOW then
//    if iTemp < 0 then
//      put a 1 in the result position
//      result := result or test;
//
//    if we're dead on, break
//    if iTemp = 0 then begin
//      result := test;
//      exit;
//    end;
//  end;
//
//  if func(0)=0 then
//    result := 0
//  else
//    result := -1;
//
//
//end;






procedure ofinal;
begin
{$IFNDEF WINDOWS}
  DeleteCriticalSection(ios_lock);
{$ENDIF}
end;
function Get7BitEncodedBufferLength(i8BitLength: nativeint): nativeint;
var
  bits: nativeint;
  bytes: nativeint;
begin
  //returns the number of bytes that must actualyl be read from a 7-bit encoded byte stream/array in order to an array of
  //the 8-bit length.
  bits := i8BitLength * 8;
  bytes := ((bits) div 7) + 1;

  result := bytes;

end;
function Get8BitEncodedBufferLength(i7BitLength: nativeint): nativeint;
begin
  result := Get7BitEncodedBuffer8BitLength(i7BitLength);
end;


function Get7BitEncodedBuffer8bitLength(i7BitLength: nativeint): nativeint;
var
  bits: nativeint;
  bytes: nativeint;
begin
  //returns the number of 8-bit bytes contained in a 7-bit encoded stream
  bits := i7BitLength * 7;
  bytes := ((bits) div 8);

  result := bytes;

end;

procedure Convert7BitsTo8Bits(input: pbyte; output: pbyte; InputLength: nativeint);
var
  in_pos, in_bit_pos, out_pos, out_bit_pos: fi;
  outLength: ni;
  curByte: byte;
  temp: byte;
  bits_to_move: fi;
begin
  in_pos := 0;
  in_bit_pos:=6;
  out_pos := 0;
  out_bit_pos := 7;

  outLength := Get7BitEncodedBuffer8bitLength(INputLength);
  fillmem(@output[0], outLength,0);
  while out_pos < outLength do begin
    //move a bit
    bits_to_move := lesserof(in_bit_pos+1, out_bit_pos+1);//determine how many bits to move
    temp := input[in_pos];//get the input byte
    temp := temp shr (in_bit_pos-(bits_to_move-1));//right-justify the input bits
    temp := temp and ((1 shl (bits_to_move))-1);//mask off the input bits
    temp := temp shl ((out_bit_pos+1)-bits_to_move);//justify bits to target
    temp := output[out_pos] or temp;
    output[out_pos] := temp;


    dec(out_bit_pos, bits_to_move);
    dec(in_bit_pos, bits_to_move);

    //check for bit rollovers
    if (out_bit_pos < 0) then begin
      out_bit_pos := 7;
      inc(out_pos);
    end;
    if (in_bit_pos <0) then begin
      in_bit_pos := 6;
      inc(in_pos);
    end;

  end;


end;


function Convert8BitsTo7Bits(input: PByte; inputSize: ni; output: PByte; outputSize: ni): boolean;
var
  b: ni;
  bitinbyte: fi;
  bitInTx: fi;
  tx: fi;
  bit: fi;
  count: ni;
  bytesToTransmit: ni;
begin
  bytesToTransmit := inputSize;
 bitInTx := 0;
  result := false;
  if OutputSize < Get7BitEncodedBufferLength(inputSize) then
    exit;
  tx := 0;
  while (bytesToTransmit > 0) do begin
    b := input[0];
    input := input + 1;
    dec(bytestoTransmit);

    for bitInbyte := 0 to 7 do begin
      if (b and $80)<>0 then
        bit := 1
      else
        bit := 0;

      b := b shl 1;

      //put the extracted bit into the 7-bit data
      tx := tx shl 1;
      tx := tx or bit;
      inc(bitInTx);

      //if we have a complete 7 bit data set, transmit it
      if (bitInTx = 7) THEN BEGIN
        output[0] := tx;
        output := output + 1;

        //reset
        tx := 0;
        bitInTx := 0;

      END;

    end;
  end;

	// if we have some leftover bits to transmit
	if (bitInTx > 0) then begin
    tx := tx shl (7-bitInTx);//justify to the MSBit
    output[0] := tx;
    output := output + 1;
  end;
  result := true;
end;

procedure MaskSet(byt: pbyte; startingbit: ni; bitwidth: ni; value: ni);
var
  mask: ni;
begin
  mask := ((1 shl bitwidth) -1) shl startingbit;
  value := value shl startingbit;
  value := value and mask;
  byt[0] := (byt[0] and not mask) or value;



end;

function MaskGet(byt: pbyte; startingbit: ni; bitwidth: ni): ni;
var
  mask: ni;
begin
  mask := (1 shl bitwidth) - 1;
  result := byt[0];
  result := result shr startingbit;
  result := result and mask;

end;


function StringToAnsiByteArray(s: string): TDynByteArray;
//converts a wide string into an ansi byte array (loses unicode support, for legacy stuff)
//gets around lack of ansi support on mobile devices
var
  t: ni;
begin
  system.setlength(result, length(s));
  t := 0;
  while t< length(result) do begin
    result[t] := ord(s[t+STRZ]) and 255;
    inc(t);
  end;

end;

function StringToAnsiByteArray_NULL_TERMINATED(s: string): TDynByteArray;
//converts a wide string into an ansi byte array (loses unicode support, for legacy stuff)
//gets around lack of ansi support on mobile devices
var
  t: ni;
begin
  system.setlength(result, length(s)+1);
  t := 0;
  while t< (length(result)-1) do begin
    result[t] := ord(s[t+STRZ]) and 255;
    inc(t);
  end;
  result[high(result)] := 0;
end;

function AnsiByteArrayToString(a: TDynByteArray): string;
var
  t: ni;
begin
  setlength(result, length(a));
  t := 0;
  while t< length(a) do begin
    if a[t] = 0 then begin
      setlength(result, t-STRZ);
      exit;
    end;
    result[t+STRZ] := char(a[t]);
    inc(t);
  end;
end;

function AnsiPointerToString(a: Pbyte): string;
var
  t: ni;
begin
  setlength(result, 256);
  t := 0;
  while true do begin
    if a[t] = 0 then begin
      //set final length when done
      setlength(result, t-STRZ);
      exit;
    end;
    result[t+STRZ] := char(a[t]);
    inc(t);

    //double capacity if we're going out of bounds
    if (t >= length(result)) then
      setlength(result, length(result) *2);
  end;




end;



function strtoni(s: string): ni;
begin
  {$IF CPU64BITS}
  result := strtoint64(s);
  {$ELSE}
  result := strtoint(s);
  {$ENDIF}
end;


procedure CalculateChecksum(p: pbyte; l: ni; out iSum: int64);
var
  t: ni;
  a: int64;
  b: byte;
begin
  iSum := 0;

  for t:= 0 to l-1 do begin
{$Q-}//overflow checking off
    iSum := iSum + p^;
    inc(p);
  end;
end;

procedure CalculateChecksum(p: pbyte; l: ni; out iSum, iXor: int64);
var
  t: ni;
  a: int64;
  b: byte;
begin
  iSum := 0;
  iXor := 0;

  for t:= 0 to l-1 do begin
    //XOR part.  Trying to do both parts with a single memory fetch
    if t and $7 = 0 then begin
      if (l-t) >= 8 then begin
        a := PInt64(p)^;
        iXor := iXor xor a;
      end;
    end;

{$Q-}//overflow checking off
    iSum := iSum + p^;

    inc(p);
  end;
end;

procedure sysBeep(freq, dur: ni);
begin
{$IFDEF MSWINDOWS}
  windows.beep(freq,dur);
{$ENDIF}
end;

procedure MoveMem32Xor(const D,S: pointer; const size: ni);inline;
VAR
  t: NativeInt;
  dd,ss: PNativeInt;
  bd,bs: PByte;
begin
  if size = 0  then
    exit;

  t := size;
  dd := PNativeInt(pbyte(D)+(size));
  ss := PNativeInt(pbyte(S)+(size));
  if t > SizeOf(NativeInt) then
  repeat
    dec(t,SizeOf(NativeInt));
    dd := PNativeInt(pbyte(dd)-SizeOf(NativeInt));
    ss := PNativeInt(pbyte(ss)-SizeOf(NativeInt));
    dd^ := dd^ xor ss^;
  until t < SizeOf(NativeInt);

  bd := PByte(dd);
  bs := PByte(ss);
  if t > 0 then
  repeat
    dec(t);
    bd := bd-1;
    bs := bs-1;
    bd^ := bd^ xor bs^;

  until t = 0;

end;

function StrToIntEx(s: string): int64;
begin
  if (s = '') OR (s='---') then
    exit(0)
  else
    exit(strtoint64(s));
end;

function RandomOrder(iItems: ni): TDynInt64Array;
var
  t: ni;
  a,b,c: int64;
begin
  system.setlength(result, iItems);
  t := 0;
  while t < iItems do begin
    result[t] := t;
    inc(t);
  end;

  t := 0;
  while t < iItems do begin
    a := Random(iItems);
    b := random(iItems);

    c := result[a];
    result[a] := result[b];
    result[b] := c;
    inc(t);
  end;


end;

function IsAllZeros(p: pbyte; length: ni): boolean;
var
  cx: ni;
  val: ni;
begin
  cx := length;
  while cx > sizeof(ni) do begin
    val := p^;
    if val <> 0 then exit(false);
    inc(p);
    dec(cx);
  end;

  exit(true);


end;


procedure MoveMem_Up(const DD,SS: pointer; const Size: ni);
var
  cx: ni;
  i64: int64;
  i32: int32;
  i16: smallint;
  i: byte;
  d,s: Pbyte;
type
  PInt32 = ^int32;
  PInt16 = ^int16;

begin
  cx := size;

  D := DD;
  s := ss;
  //cannot be used if the source and destination are too close together
  if nativeuint(ss)-nativeuint(dd) >= sizeof(nativeuint) then begin
    //move individual bytes until we're aligned
    while (cx > 0) and ((nativeint(s) and 7) <> 0) do begin
      i := PByte(s)^;
      PByte(d)^ := i;
      inc(s);
      inc(d);
      dec(cx);
    end;

    while cx >= 8 do begin
      i64 := PInt64(s)^;
      PInt64(d)^ := i64;
      inc(s,8);
      inc(d,8);
      dec(cx,8);
    end;
    if (cx > 4) then begin
      i32 := PInt32(s)^;
      PInt32(d)^ := i32;
      inc(s,4);
      inc(d,4);
      dec(cx,4);
    end;
    if (cx >= 2) then begin
      i16 := PInt16(s)^;
      PInt16(d)^ := i16;
      inc(s,2);
      inc(d,2);
      dec(cx,2);
    end;
  end;
  while (cx > 0) do  begin
    i := PByte(s)^;
    PByte(d)^ := i;
    inc(s);
    inc(d);
    dec(cx);
  end;





end;
procedure MoveMem_Down(const D,S: pointer; const Size: ni);
VAR
  cx: NativeInt;
  dd,ss: PNativeInt;
  bd,bs: PByte;
begin
  if size = 0  then
    exit;

  cx := size;
  dd := PNativeInt(pbyte(D)+(size));
  ss := PNativeInt(pbyte(S)+(size));
  if (nativeuint(dd)-nativeuint(ss) >= sizeof(NativeUInt)) then begin
    while cx >= SizeOf(NativeInt) do begin
      dec(cx,SizeOf(NativeInt));
      dd := PNativeInt(pbyte(dd)-SizeOf(NativeInt));
      ss := PNativeInt(pbyte(ss)-SizeOf(NativeInt));
      dd^ := ss^;
    end;
  end;

  bd := PByte(dd);
  bs := PByte(ss);
  while cx > 0 do begin
    dec(cx);
    bd := bd-1;
    bs := bs-1;
    bd^ := bs^;

  end;

end;



procedure pmm(d: pointer; s: pointer; sz: NativeUInt; src_bucket_start: pointer; src_bucket_sz: NativeUInt; dst_bucket_start: pointer; dst_bucket_sz: NativeUInt);
//protected move memory, helps you debug situation where memory might be gettins stepped on
var
  dd,ss,sb,db: NativeUInt;
begin
  dd := nativeuint(d);
  ss := nativeuint(s);
  sb := nativeuint(src_bucket_start);
  db := nativeuint(dst_bucket_start);
  if (ss) > (sb+src_bucket_sz) then
    raise ECritical.create('memory range check error #1 source is not in source bucket!' +dd.tohexstring+' '+ss.tohexstring+' '+sz.tostring+' '+sb.tohexstring+' '+' '+src_bucket_sz.tostring+' '+db.tohexstring+' '+dst_bucket_sz.tostring);
  if (ss) < (sb) then
    raise ECritical.create('memory range check error #2 source is before source bucket!' +dd.tohexstring+' '+ss.tostring+' '+sz.tohexstring+' '+sb.tostring+' '+' '+src_bucket_sz.tohexstring+' '+db.tostring+' '+dst_bucket_sz.tohexstring);

  if (dd) > (db+dst_bucket_sz) then
    raise ECritical.create('memory range check error #3 dst is not in dst bucket!' +dd.tostring+' '+ss.tohexstring+' '+sz.tostring+' '+sb.tohexstring+' '+' '+src_bucket_sz.tostring+' '+db.tohexstring+' '+dst_bucket_sz.tostring);
  if (dd) < (db) then
    raise ECritical.create('memory range check error #4 dst is before dst bucket!' +dd.tohexstring+' '+ss.tostring+' '+sz.tohexstring+' '+sb.tostring+' '+' '+src_bucket_sz.tohexstring+' '+db.tostring+' '+dst_bucket_sz.tohexstring);

  if (dd+sz) > (db+dst_bucket_sz) then
    raise ECritical.create('memory range check error #5 write past end!' +dd.tostring+' '+ss.tohexstring+' '+sz.tostring+' '+sb.tohexstring+' '+' '+src_bucket_sz.tostring+' '+db.tohexstring+' '+dst_bucket_sz.tostring);
  if (ss+sz) > (sb+src_bucket_sz) then
    raise ECritical.create('memory range check error #6 read past end!' +dd.tohexstring+' '+ss.tostring+' '+sz.tohexstring+' '+sb.tostring+' '+' '+src_bucket_sz.tohexstring+' '+db.tostring+' '+dst_bucket_sz.tohexstring);

  movemem32(d,s,sz);


end;


procedure ecs2(var cs1,cs2: TCLXCRiticalSection; retryInterval: ni);
begin
  while true do begin
    ecs(cs1);
    if not (tecs(cs2)) then begin
      lcs(cs1);
      sleep(random(retryinterval));
      continue;
    end;
    break;
  end;
end;
procedure lcs2(var cs1,cs2: TCLXCRiticalSection);
begin
  lcs(cs1);
  lcs(cs2);
end;


function HextoMemory(s: string): TDynByteArray;
var
  sHexOnly: string;
  idxOut, idxIn: ni;
  c: char;
  sTemp: string;
begin
  //first build a string that is only the hex digits, no commas or spaces or anything
  sHexOnly := s;
  idxOut := STRZ;
  for idxIn := low(s) to high(s) do begin
    c := s[idxIN];
    case c of
      '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','a','b','c','d','e','f': begin
        sHexOnly[idxOut] := c;
        inc(idxOUt);
      end;
    else
    end;
  end;


  if ((idxOut-STRZ) mod 2) = 1 then
    raise ECritical.create('HexToMemory requires even number of digits for input');

  system.setlength(result, (idxOut-STRZ) div 2);

  for idxIn := 0 to high(result) do begin
    sTemp := '$'+zcopy(sHexOnly, idxIn*2, 2);
    result[idxIn] := strtoint(sTemp);
  end;






end;


function boolToInt(b: boolean): nativeint;
begin
  if (b) then exit(1) else exit(0);
end;


function Isfile(sPathOrFileName: string): boolean;
begin
  if fileexists(sPathOrFileName) then
    exit(true)
  else
    result := not DirectoryExists(extractfilepath(sPathOrfileName));

end;


function FileDateToDateTimeEx(fd: longint): TDateTime;
begin
  if fd < 09 then
    result := 0.0
  else
    result := FileDateToDateTime(fd);
end;

function Varsame(v1,v2: variant): boolean;
begin
  result := false;
  if vartype(v1)<>vartype(v2) then
    exit(false);

  if (v1=v2) then
    exit(true);


end;

procedure NotImplemented;
begin
  raise ECritical.create('not implemented');
end;


function stridelength(p1,p2: pointer): ni;inline;
begin
  result := abs(PByte(p2)-PByte(p1));
end;


function GMTtoLocalTime(dt: TDateTime): TDatetime;
{$IFDEF MSWINDOWS}
var
  TZI: TTimeZoneInformation;
begin
  case GetTimeZoneInformation(TZI) of
    TIME_ZONE_ID_STANDARD:
      begin
        Result := dt- (TZI.Bias / 60 / 24);
      end;
    TIME_ZONE_ID_DAYLIGHT:
      begin
        Result := dt- ((TZI.Bias+ TZI.DaylightBias) / 60 / 24) ;
      end
  else
    raise
      Exception.Create('Error converting to UTC Time. Time zone could not be determined.');
  end;
end;
{$ELSE}
begin
  NotImplemented;
end;
{$ENDIF}

function LocalTimeToGMT(dt: TDateTime): TDateTime;
{$IFDEF MSWINDOWS}
var
  TZI: TTimeZoneInformation;
begin
  case GetTimeZoneInformation(TZI) of
    TIME_ZONE_ID_STANDARD:
      begin
        Result := dt+ (TZI.Bias / 60 / 24);
      end;
    TIME_ZONE_ID_DAYLIGHT:
      begin
        Result := dt+ ((TZI.Bias+ TZI.DaylightBias) / 60 / 24) ;
      end
  else
    raise
      Exception.Create('Error converting to UTC Time. Time zone could not be determined.');
  end;
end;
{$ELSE}
begin
  NotImplemented;
end;
{$ENDIF}

function FindInMemory(patternPtr: Pbyte; patternSz: ni; poolPointer: Pbyte; poolSz: ni): ni;
var
  t: ni;
  cmp: boolean;
  sz: ni;
begin
  result := -1;
  sz := lesserof(patternsz, poolsz);
  for t:= 0 to poolSz-patternSz do begin
    cmp := CompareMem(patternPtr, poolPointer+t, sz);
    if cmp then begin
      exit(t);
    end;
  end;
end;

procedure AlertMemoryPattern(patternPtr: Pbyte; patternSz: ni; poolPointer: Pbyte; poolSz: ni);
var
  i: ni;
begin
  i := FindInmemory(patternPtr, patternSz, poolPointer, poolSz);
  if i >=0 then
    Debug.Log('Found Memory Pattern! @0x'+inttohex(i, 1));
end;

function JStr(s: string): string;
begin
  result := s;
  result := stringreplace(result, '\','\\', [rfReplaceAll]);
  result := stringreplace(result, '''','\''', [rfReplaceAll]);
  result := stringreplace(result, '"','\"', [rfReplaceAll]);
end;


{$IFDEF MSWINDOWS}
{$IFNDEF CPUx86}
function WinGetPhysicallyInstalledSystemMemory; external kernel32 name 'GetPhysicallyInstalledSystemMemory';
{$ENDIF}
{$ENDIF}

function GetPhysicalMemory: int64;
begin
{$IFDEF MSWINDOWS}
{$IFDEF CPUx86}
  result := 2000000000;
{$ELSE}
  if not WinGetPhysicallyInstalledSystemMemory(result) then
    exit(0);
{$ENDIF}
{$ELSE}
  result := 2000000000;//todo 2: other platforms arbitrarily use 2GB for reported memory size
                       //this does not create actual memory limits, but should be fixed
{$ENDIF}

end;



function CountSetBits(bitMask : int64) : ni;
var
  i : integer;
begin
  result := 0;
  for i := 0 to 63 do begin
    if (bitMask and 1) <> 0 then Inc(result);
    bitMask := bitmask shr 1;
  end;
end;



{ TAlignedTempSpace }

procedure TAlignedTempSpace.Allocate;
begin
  if memsize = 0 then
    raise ECritical.create('temp space size is 0!');
{$IFDEF MSWINDOWS}
  aligned := VirtualAlloc(nil, memsize,  MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);
{$ELSE}
  aligned := getmemory(memsize);
{$ENDIF}
end;

procedure TAlignedTempSpace.Unallocate;
begin
{$IFDEF MSWINDOWS}
  VirtualFree(aligned, memsize, MEM_RELEASE);
{$ELSE}
  freemem(aligned);
{$ENDIF}
end;



initialization

 init.RegisterProcs('systemx', oinit, ofinal);

finalization





end.
