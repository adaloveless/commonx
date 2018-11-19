{$IFNDEF DISABLE_MASTER_KEYWORDS}
unit BigBrainUltra2013;
interface
{$ENDIF}
{$IFNDEF DISABLE_INTERFACE}
{$DEFINE SIMPLEHEAPS}


type
{$IFDEF USE_LARGER_BOOLEANS}
  BBBool = LongBool;
{$ELSE}
  BBBool = boolean;
{$ENDIF}

type
  DWORD = cardinal;
  BOOL = LONGBOOL;

type
  THeapObject=class
  public
    class function NewInstance:TObject; override;
    procedure FreeInstance; override;
  end;
  {$IFNDEF DELPHI64}
  NativeInt = integer;
  NativeUInt = cardinal;
  PNativeInt = ^integer;
  PNativeUInt = ^cardinal;
  {$ENDIF}


{$IFDEF USE_SPINLOCK}
type
  PLockMap = ^TLockMap;
  TLockMap = record
    Lock: Cardinal;
    LockCount: Cardinal;
  end;

const
  PageSize = 65536;
  MaxLocks = PageSize div SizeOf(TLockMap);
  TSLOwnerOffset = SizeOf(TLockMap);
  TSLNextOffset  = TSLOwnerOffset + SizeOf(Word);
{$IFDEF CPUX64}
  kernel64 = 'kernel32.dll';
{$ELSE}
  kernel32 = 'kernel32.dll';
{$ENDIF}





type
  TSpinLock = class(THeapObject)
  protected
    FNoSleepCount: Cardinal;
    FOwner: PLockMap;
    FSleepAmount: Cardinal;
    procedure InitLockMap; {$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure InitNoSleepCount(const NoSleepCount: Cardinal); {$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure ReleaseLockMap; {$IFNDEF NOINLINE}inline;{$ENDIF}
  public
    constructor Create(const SleepAmount: Cardinal = 0;
      const NoSleepCount: Cardinal = 0); virtual;
    destructor Destroy; override;
    procedure Lock;{$IFNDEF ASM}{$IFNDEF NOINLINE}inline;{$ENDIF}{$ENDIF}
    function Trylock(TryCount: Cardinal): BBBool;{$IFNDEF ASM}{$IFNDEF NOINLINE}inline;{$ENDIF}{$ENDIF}
    procedure Unlock;{$IFNDEF ASM}{$IFNDEF NOINLINE}inline;{$ENDIF}{$ENDIF}
  end;

  TSpinIndex = 0..MaxLocks-1;

function ProcessIdToSessionId(const dwProcessId: DWORD; out pSessionId: DWORD): BOOL; stdcall; external kernel32;



function GetProcessAffinityMask(hProcess: THandle;
  var lpProcessAffinityMask, lpSystemAffinityMask: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetProcessAffinityMask}
function GetProcessAffinityMask; external kernel32 name 'GetProcessAffinityMask';

//function GetCurrentProcess: THandle; stdcall;
//{$EXTERNALSYM GetCurrentProcess}
//function GetCurrentProcess; external kernel32 name 'GetCurrentProcess';
{$ENDIF}

//uses CLX_Windows, Libc;
//----------------------------KERNEL--------------------------------------------
const
  SPIN_COUNT = 10000;
  OS_BLOCK_HOLD_TIME = 8000;
{$IFNDEF XE2}
  MAXBIT = 31;
{$ELSE}
  {$IFDEF CPUX86}
    MAXBIT = 31;
  {$ELSE}
    MAXBIT = 56;
  {$ENDIF}
{$ENDIF}
{$IFDEF USE_MULTIPLE_THREAD_BLOCK_HOLD_TIMES}
  THREAD_BLOCK_HOLD_TIME: array[0..56] of cardinal =
   (4000,4000,4000,4000,4000,4000,4000,4000,4000,4000,4000,4000,4000,4000,4000,
    4000,8000,8000,8000,8000,8000,8000,8000,8000,8000, 100, 100, 100, 100, 100,
     100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
     100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100);
{$ELSE}
  THREAD_BLOCK_HOLD_TIME = 8000;
{$ENDIF}
  LITE_HEADER_SIZE = 4;
  MANAGER_TIMEOUT = 10000;


const
  LARGE_BLOCK_INDEX = MAXBIT;
  FLAG_AVAILABLE = 1;
{$IFDEF DONT_CACHE_LARGE_BLOCKS}
  FLAG_LARGE = 2;

{$ENDIF}
{$IFNDEF LINUX}
{$IFDEF CPUX64}
  kern = 'kernel32.dll';
{$ELSE}
  kern = 'kernel32.dll';
{$ENDIF}




const
//  PAGE_NOACCESS  = 1;
  PAGE_READWRITE = 4;
  LMEM_FIXED = 0;
  LMEM_MOVEABLE = 2;
  LMEM_ZEROINIT = $40;
  MEM_COMMIT   = $1000;
  MEM_RESERVE  = $2000;
  MEM_DECOMMIT = $4000;
  MEM_RELEASE  = $8000;


const
  STATUS_NO_MEMORY                = pointer($C0000017);
  STATUS_ACCESS_VIOLATION         = pointer($C0000005);



procedure Sleep(dwMilliseconds: DWORD); stdcall;
{$EXTERNALSYM Sleep}
procedure Sleep; external kern name 'Sleep';

{$ENDIF}

type
  TSystemInfo = record
    case Integer of
      0: (
        dwOemId: DWORD);
      1: (
        wProcessorArchitecture: Word;
        wReserved: Word;
        dwPageSize: DWORD;
        lpMinimumApplicationAddress: Pointer;
        lpMaximumApplicationAddress: Pointer;
        dwActiveProcessorMask: DWORD;
        dwNumberOfProcessors: DWORD;
        dwProcessorType: DWORD;
        dwAllocationGranularity: DWORD;
        wProcessorLevel: Word;
        wProcessorRevision: Word);
  end;

{$IFNDEF LINUX}
procedure GetSystemInfo(var lpSystemInfo: TSystemInfo); stdcall;
{$EXTERNALSYM GetSystemInfo}
procedure GetSystemInfo; external kern name 'GetSystemInfo';
{$ELSE}
procedure GetSystemInfo(var lpSystemInfo: TsystemInfo); stdcall;
{$ENDIF}



{$IFDEF REALLINUX}
const
  libcmodulename = 'libc.so.6';
  libcryptmodulename = 'libcrypt.so.1';
  libdlmodulename = 'libdl.so.2';
  libmmodulename = 'libm.so.6';
  libpthreadmodulename = 'libpthread.so.0';
  libresolvmodulename = 'libresolv.so.2';
  librtmodulename = 'librt.so.1';
  libutilmodulename = 'libutil.so.1';
{$ENDIF}

type
  THandle = NativeUInt;

  _RTL_CRITICAL_SECTION = record
    DebugInfo: Pointer;
    LockCount: Longint;
    RecursionCount: Longint;
    OwningThread: THandle;
    LockSemaphore: THandle;
    Reserved: PNativeUInt;
  end;

  TAtomicLock = record
    lock: integer;
    handle: NativeInt;
  end;

{$IFNDEF ATOMICLOCKS}
{$IFDEF MACOS}
{$ELSE}
  TCLXCriticalSection = _RTL_CRITICAL_SECTION;
{$ENDIF}
{$ELSE}
  TCLXCriticalSection = TAtomicLock;
{$ENDIF}



{$IFDEF LINUX}
{$IFDEF MACOS}
{$I BBOSX.inc}
type
  TCLXCriticalSection = pthread_mutex_t;

{$ENDIF}
{L}
{L}const
{L}  EBUSY                      = 16;        {  Device or resource busy  }
{L}  PTHREAD_MUTEX_RECURSIVE_NP = 1;
{L}  //PTHREAD_MUTEX_RECURSIVE = PTHREAD_MUTEX_RECURSIVE_NP;
{L}type
{L}  _pthread_descr = Pointer;
{L}  _pthread_fastlock = {packed} record
{L}    __status: Longint;          { "Free" or "taken" or head of waiting list }
{L}    __spinlock: Integer;        { For compare-and-swap emulation }
{L}  end;
{L}  TPthreadFastlock = _pthread_fastlock;
{L}  PPthreadFastlock = ^TPthreadFastlock;
{L}
{L}  TPthreadMutex = {packed} record
{L}    __m_reserved: Integer;        { Reserved for future use }
{L}    __m_count: Integer;           { Depth of recursive locking }
{L}    __m_owner: _pthread_descr;    { Owner thread (if recursive or errcheck) }
{L}    __m_kind: Integer;            { Mutex kind: fast, recursive or errcheck }
{L}    __m_lock: _pthread_fastlock;     { Underlying fast lock }
{L}  end;
{L}
//{L}  {$EXTERNALSYM TPthreadMutex}
//{L}  pthread_mutex_t = TPthreadMutex;
{L}  {$EXTERNALSYM pthread_mutex_t}
{L}  TRTLCriticalSection = TPthreadMutex;
{L}  PRTLCriticalSection = ^TCLXCriticalSection;
{L}  {$EXTERNALSYM PRTLCriticalSection}
{L}
{L}{ Attribute for mutex.  }
//{L}  pthread_mutexattr_t = {packed} record
//{L}    __mutexkind: Integer;
//{L}  end;
{L}  {$EXTERNALSYM pthread_mutexattr_t}
{L}  TMutexAttribute = pthread_mutexattr_t;
{L}  {$EXTERNALSYM TMutexAttribute}
{L}  PMutexAttribute = ^TMutexAttribute;
{L}  {$EXTERNALSYM PMutexAttribute}
{L}
{L}  __time_t = type Longint;
{L}  __useconds_t = Cardinal;
{L}  __suseconds_t = Longint;
{L}
{L}  timeval = {packed} record
{L}    tv_sec: __time_t;           { Seconds.  }
{L}    tv_usec: __suseconds_t;     { Microseconds.  }
{L}
{L}  end;
{L}
//{L}  timespec = {packed} record
//{L}    tv_sec: Longint;            { Seconds.  }
//{L}    tv_nsec: Longint;           { Nanoseconds.  }
//{L}  end;
//{L}  {$EXTERNALSYM timespec}
//{L}  TTimeSpec = timespec;
//{L}  {$EXTERNALSYM TTimeSpec}
//{L}  PTimeSpec = ^TTimeSpec;
{L}  {$EXTERNALSYM PTimeSpec}
{L}  timezone = {packed} record
{L}    tz_minuteswest: Integer;    { Minutes west of GMT.  }
{L}    tz_dsttime: Integer;        { Nonzero if DST is ever in effect.  }
{L}  end;
{L}
{L}
{L}  TTimeZone = timezone;
{L}  PTimeZone = ^TTimeZone;
{L}
{L}
{L}  TTimeVal = timeval;
{L}  PTimeVal = ^TTimeVal;
{L}
{L}
{L}{ Attributes for threads.  }
//{L}  pthread_attr_t = TThreadAttr;  // TThreadAttr in System.pas
{L}  {$EXTERNALSYM pthread_attr_t}
{L}
{L}
//{L}  TCLXCriticalSection = TCLXCriticalSection;
{L}
{L}
{L}
{L}//function gettimeofday(var timeval: TTimeVal; var timezone: TTimeZone): Integer; external libcmodulename name 'gettimeofday';
{L}//function gettimeofday(var timeval: TTimeVal; timezone: PTimeZone): Integer; external libcmodulename name 'gettimeofday';
{L}
{L}function DeleteCriticalSection(var lpCriticalSection: TCLXCriticalSection): Integer; cdecl;
//{L}function pthread_mutex_trylock(var Mutex: TCLXCriticalSection): Integer; cdecl;
{L}
//{L}function pthread_mutex_init(var Mutex: TCLXCriticalSection;
//{L}  var Attr: TMutexAttribute): Integer; cdecl; overload;
//{L}function pthread_mutex_init(var Mutex: TCLXCriticalSection;
//{L}  Attr: PMutexAttribute): Integer; cdecl; overload;
//{L}function pthread_mutex_destroy(var Mutex: TCLXCriticalSection): Integer; cdecl;
//{L}function pthread_mutex_timedlock(var Mutex: TCLXCriticalSection; const AbsTime: timespec): Integer; cdecl;
//{L}function pthread_mutex_unlock(var Mutex: TCLXCriticalSection): Integer; cdecl;
//{L}function pthread_mutexattr_init(var Attr: TMutexAttribute): Integer; cdecl;
//{L}function pthread_mutexattr_destroy(var Attr: TMutexAttribute): Integer; cdecl;
//{L}function pthread_mutexattr_getpshared(var Attr: TMutexAttribute; // Actually __const pthread_mutexattr_t *
//{L}  var ProcessShared: Integer): Integer; cdecl;
//{L}function pthread_mutexattr_setpshared(var Attr: TMutexAttribute;
//{L}  ProcessShared: Integer): Integer; cdecl;
//{L}function pthread_mutexattr_settype(var Attr: TMutexAttribute; Kind: Integer): Integer; cdecl;
//{L}function pthread_mutexattr_gettype(var Attr: TMutexAttribute; var Kind: Integer): Integer; cdecl;
{L}
{L}
//{L}function pthread_mutex_destroy;         external libpthreadmodulename name 'pthread_mutex_destroy';
//{L}function pthread_mutex_init(var Mutex: TCLXCriticalSection; var Attr: TMutexAttribute): Integer; external libpthreadmodulename name 'pthread_mutex_init';
//{L}function pthread_mutex_init(var Mutex: TCLXCriticalSection; Attr: PMutexAttribute): Integer; external libpthreadmodulename name 'pthread_mutex_init';
//{L}function pthread_mutex_timedlock;       external libpthreadmodulename name 'pthread_mutex_timedlock';
{L}//function pthread_mutex_trylock;         external libpthreadmodulename name 'pthread_mutex_trylock';
//{L}function pthread_mutex_unlock;          external libpthreadmodulename name 'pthread_mutex_unlock';
//{L}function pthread_mutexattr_destroy;     external libpthreadmodulename name 'pthread_mutexattr_destroy';
//{L}function pthread_mutexattr_getpshared;  external libpthreadmodulename name 'pthread_mutexattr_getpshared';
//{L}function pthread_mutexattr_setpshared;  external libpthreadmodulename name 'pthread_mutexattr_setpshared';
//{L}function pthread_mutexattr_gettype;     external libpthreadmodulename name 'pthread_mutexattr_gettype';
//{L}function pthread_mutexattr_init;        external libpthreadmodulename name 'pthread_mutexattr_init';
//{L}function pthread_mutexattr_settype;     external libpthreadmodulename name 'pthread_mutexattr_settype';
//{L}function pthread_mutex_trylock;         external libpthreadmodulename name 'pthread_mutex_trylock';
{L}
{L}function InitializeCriticalSection(var lpCriticalSection: TCLXCriticalSection): Integer; cdecl;
{L}function InitializeCriticalSectionAndSpinCount(var lpCriticalSection: TCLXCriticalSection; dwSpinCount: DWORD): BOOL; cdecl;
{L}function EnterCriticalSection(var lpCriticalSection: TCLXCriticalSection): Integer; cdecl;
{L}function LeaveCriticalSection(var lpCriticalSection: TCLXCriticalSection): Integer; cdecl;
//{L}function DeleteCriticalSection(var lpCriticalSection: TCLXCriticalSection): Integer; cdecl;
{L}function TryEnterCriticalSection(var lpCriticalSection: TCLXCriticalSection): LongBool; cdecl;
{L}{$IFNDEF ATOMICLOCKS}

function DeleteCriticalSection(var lpCriticalSection: TCLXCriticalSection): Integer; cdecl; external libpthread name _PU + 'pthread_mutex_destroy';
{$EXTERNALSYM pthread_mutex_destroy}

{ Try to lock MUTEX.  }

function TryEnterCriticalSection(var lpCriticalSection: TCLXCriticalSection): LongBool; cdecl; external libpthread name _PU + 'pthread_mutex_trylock';
{$EXTERNALSYM pthread_mutex_trylock}
{ Wait until lock for MUTEX becomes available and lock it.  }
function EnterCriticalSection(var lpCriticalSection: TCLXCriticalSection): Integer; cdecl; external libpthread name _PU + 'pthread_mutex_lock';
{$EXTERNALSYM pthread_mutex_lock}

function LeaveCriticalSection(var lpCriticalSection: TCLXCriticalSection): Integer; cdecl;
  external libpthread name _PU + 'pthread_mutex_unlock';
{$EXTERNALSYM pthread_mutex_unlock}

{L}{$ENDIF}
{L}
//{L}procedure pthread_exit(pexit: pointer);   external libpthreadmodulename name 'pthread_exit';
{L}procedure Beep(freq, duration: nativeint);
{L}function GetTickCount: Cardinal;
{L}
{L}


function GetCurrentThreadID: TThreadID; cdecl;  external libpthread name _PU + 'pthread_self';



{$ENDIF}

{$IFNDEF LINUX}
//  TCLXCriticalSection = TRTLCriticalSection;

{$IFDEF MESSAGES}
  procedure OutputDebugString(lpOutputString: PWideChar); stdcall;
  {$EXTERNALSYM OutputDebugString}
  {$IFDEF UNICODE}
    procedure OutputDebugString; external kern name 'OutputDebugStringW';
    procedure OutputDebugStringW(lpOutputString: PWideChar); stdcall;
    {$EXTERNALSYM OutputDebugStringW}
    procedure OutputDebugStringW; external kern name 'OutputDebugStringW';


  {$ELSE}
    procedure OutputDebugString; external kern name 'OutputDebugStringA';
    procedure OutputDebugStringA(lpOutputString: PAnsiChar); stdcall;
    {$EXTERNALSYM OutputDebugStringA}
    procedure OutputDebugStringA; external kern name 'OutputDebugStringA';
  {$ENDIF}
{$ENDIF}
function FlushInstructionCache(hProcess: THandle; const lpBaseAddress:
  Pointer; dwSize: DWORD): BOOL; stdcall;
  external kern name 'FlushInstructionCache';
function GetCurrentProcess: THandle; stdcall;
  external kern name 'GetCurrentProcess';



function GetCurrentThreadId: integer; external kern name 'GetCurrentThreadId';

function VirtualProtect(lpAddress:pointer; dwSize, flNewProtect: DWORD;
  var lpflOldProtect:DWord):BOOL; stdcall;
  external kern name 'VirtualProtect';
{$IFNDEF ATOMICLOCKS}
function InitializeCriticalSectionAndSpinCount(var lpCriticalSection: TCLXCriticalSection; dwSpinCount: DWORD): BOOL; stdcall;
  external kern name 'InitializeCriticalSectionAndSpinCount';
procedure InitializeCriticalSection(var lpCriticalSection: TCLXCriticalSection); stdcall;
  external kern name 'InitializeCriticalSection';
procedure EnterCriticalSection(var lpCriticalSection: TCLXCriticalSection); stdcall;
  external kern name 'EnterCriticalSection';
function TryEnterCriticalSection(var lpCriticalSection: TCLXCriticalSection): BOOL; stdcall;
  external kern name 'TryEnterCriticalSection';
procedure LeaveCriticalSection(var lpCriticalSection: TCLXCriticalSection); stdcall;
  external kern name 'LeaveCriticalSection';
procedure DeleteCriticalSection(var lpCriticalSection: TCLXCriticalSection); stdcall;
  external kern name 'DeleteCriticalSection';
{$ELSE}
procedure InitializeCriticalSection(var lpCriticalSection: TCLXCriticalSection); stdcall;
procedure EnterCriticalSection(var lpCriticalSection: TCLXCriticalSection);  {$IFNDEF ATOMICLOCKS} stdcall;{$ELSE}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}
function TryEnterCriticalSection(var lpCriticalSection: TCLXCriticalSection): BOOL; {$IFNDEF ATOMICLOCKS} stdcall;{$ELSE}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}
procedure LeaveCriticalSection(var lpCriticalSection: TCLXCriticalSection); {$IFNDEF ATOMICLOCKS} stdcall;{$ELSE}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}
procedure DeleteCriticalSection(var lpCriticalSection: TCLXCriticalSection); {$IFNDEF ATOMICLOCKS} stdcall;{$ELSE}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}
function TryLockAtomic(var lock: TAtomicLock):BBBool; {$IFNDEF ATOMICLOCKS} stdcall;{$ELSE}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}
procedure LockAtomic(var lock: TAtomicLock); {$IFNDEF ATOMICLOCKS} stdcall;{$ELSE}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}
procedure UnLockAtomic(var lock: TAtomicLock); {$IFNDEF ATOMICLOCKS} stdcall;{$ELSE}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}

{$ENDIF}

procedure ExitThread(ExitCode: Integer); stdcall;
  external kern name 'ExitThread';
function LocalAlloc(flags, size: Integer): Pointer; stdcall;
  external kern name 'LocalAlloc';
function HeapCreate(flOptions, dwInitialSize, dwMaximumSize: DWORD): THandle; stdcall;
{$EXTERNALSYM HeapCreate}
function HeapDestroy(hHeap: THandle): BOOL; stdcall;
{$EXTERNALSYM HeapDestroy}
function HeapAlloc(hHeap: THandle; dwFlags, dwBytes: DWORD): Pointer; stdcall;
{$EXTERNALSYM HeapAlloc}
function HeapReAlloc(hHeap: THandle; dwFlags: DWORD; lpMem: Pointer; dwBytes: DWORD): Pointer; stdcall;
{$EXTERNALSYM HeapReAlloc}
function HeapFree(hHeap: THandle; dwFlags: DWORD; lpMem: Pointer): BOOL; stdcall;
{$EXTERNALSYM HeapFree}
function HeapSize(hHeap: THandle; dwFlags: DWORD; lpMem: Pointer): DWORD; stdcall;
{$EXTERNALSYM HeapSize}
function HeapValidate(hHeap: THandle; dwFlags: DWORD; lpMem: Pointer): BOOL; stdcall;
{$EXTERNALSYM HeapValidate}
function HeapSetInformation(hHeap: THandle; heapinfo: pointer; var HeapFragValue; ValueSize: integer): BOOL; stdcall;
{$IFNDEF WINDOWS2000_COMPATIBLE}
{$EXTERNALSYM HeapSetInformation}
{$ENDIF}

function HeapCompact(hHeap: THandle; dwFlags: DWORD): integer; stdcall;
{$EXTERNALSYM HeapCompact}
function GetProcessHeap: THandle; stdcall;
{$EXTERNALSYM GetProcessHeap}
function LocalReAlloc(addr:Pointer; size,flags : Integer): Pointer; stdcall;
  external kern name 'LocalReAlloc';
function LocalFree(addr: Pointer): Pointer; stdcall;
  external kern name 'LocalFree';
function VirtualAlloc(lpAddress: Pointer;
  dwSize, flAllocationType, flProtect: DWORD): Pointer; stdcall;
  external kern name 'VirtualAlloc';
function VirtualFree(lpAddress: Pointer; dwSize, dwFreeType: DWORD): BOOL; stdcall;
  external kern name 'VirtualFree';
{$IFNDEF LINUX}
function Beep(dwFreq, dwDuration: DWORD): BOOL; stdcall; external kern name 'Beep';
{$ENDIF}
function GetTickCount: cardinal; stdcall; external kern name 'GetTickCount';
{$IFNDEF DELPHI64}
  {$IFNDEF USE_ASM_INTERLOCKED_FUNCTIONS}
  function InterlockedIncrement(var Addend: Integer): Integer; stdcall; external kern name 'InterlockedIncrement';
  function InterlockedCompareExchangePointer(var destination: pointer; var Exchange: integer; var Comperand: integer): pointer; stdcall; external kern name 'InterlockedCompareExhangePointer';
  function InterlockedDecrement(var Addend: Integer): Integer; stdcall; external kern name 'InterlockedDecrement';
  {$ENDIF}
{$ELSE}
  {$IFDEF CPUX86}
    function InterlockedAdd(var Addend: Integer; Increment: Integer): Integer;
    function InterlockedCompareExchange(var Target: Integer; Exchange: Integer; Comparand: Integer): Integer;
    function InterlockedCompareExchangePointer(var Target: Pointer; Exchange: Pointer; Comparand: Pointer): Pointer;
    function InterlockedDecrement(var Addend: Integer): Integer;
    function InterlockedExchange(var Target: Integer; Value: Integer): Integer;
    function InterlockedExchangePointer(var Target: Pointer; Value: Pointer): Pointer;
    function InterlockedIncrement(var Addend: Integer): Integer;
  {$ENDIF CPUX86}

  {$IFDEF CPUX64}
    function InterlockedExchangeAdd(var Addend: Integer; Value: Integer): Integer;
    function InterlockedDecrement(var Addend: LongInt): LongInt;
    function InterlockedIncrement(var Addend: LongInt): LongInt;
    function InterlockedCompareExchange(var Destination: Integer; Exchange: Integer; Comparand: Integer): Integer;
    function InterlockedCompareExchange64(var Destination: Int64; Exchange: Int64; Comparand: Int64): Int64; overload;
    function InterlockedCompareExchangePointer(var Destination: Pointer; Exchange: Pointer; Comparand: Pointer): Pointer;
    function InterlockedExchangePointer(var Target: Pointer; Value: Pointer): Pointer;
    function InterlockedExchange(var Target: Integer; Value: Integer): Integer;// inline;
  {$ENDIF CPUX64}
{$ENDIF}

function HeapLock(hHeap: THandle): BOOL; stdcall;
{$EXTERNALSYM HeapLock}
function HeapUnlock(hHeap: THandle): BOOL; stdcall;
{$EXTERNALSYM HeapUnlock}
function HeapAlloc; external kern name 'HeapAlloc';
function HeapCompact; external kern name 'HeapCompact';
function HeapCreate; external kern name 'HeapCreate';
function HeapDestroy; external kern name 'HeapDestroy';
function HeapFree; external kern name 'HeapFree';
function HeapLock; external kern name 'HeapLock';
function HeapReAlloc; external kern name 'HeapReAlloc';
{$IFNDEF WINDOWS2000_COMPATIBLE}
function HeapSetInformation; external kern name 'HeapSetInformation';
{$ENDIF}

function HeapSize; external kern name 'HeapSize';
function HeapUnlock; external kern name 'HeapUnlock';
function HeapValidate; external kern name 'HeapValidate';
function GetProcessHeap; external kern name 'GetProcessHeap';


{$ENDIF}

//----------------------------KERNEL--------------------------------------------
const
  SUPER_MEM_BLOCK_CORE_SIZE =
    {$IFDEF STATS}SizeOf(NativeInt)*2+{$ENDIF}
    SizeOf(pointer)*3+
    {$IFDEF PACK_HEADERS}
    SizeOf(byte)*2;
    {$ELSE}
    SizeOf(nativeInt)*2;
    {$ENDIF}
  {$IFDEF CPUX64}
    {$IFDEF STATS}
      INFLATE_CEILING = 48;
    {$ELSE}
      INFLATE_CEILING = 32;
    {$ENDIF}
  {$ENDIF}



type
  PSuperMemBlock = ^TSuperMemBlock;

  TSuperMemBlock = {$IFDEF PACK_HEADERS}packed{$ENDIF} record
    FPreviousBlock: pointer;
    FNextBlock: pointer;
    ManagedBy: pointer;
    {$IFDEF STATS}
    UsedBytes: NativeInt;
    ReportedUsedBytes: NativeInt;
    {$ENDIF STATS}
    Flags: byte;
    SizeIndex: byte;
    {$IFDEF CPUX86}
      {$IFDEF INFLATE_TO_24}
      junk: array[1..28-SUPER_MEM_BLOCK_CORE_SIZE] of byte;
      {$ENDIF}
      {$IFDEF INFLATE_TO_32}
      junk: array[1..32-SUPER_MEM_BLOCK_CORE_SIZE] of byte;
      {$ENDIF}
    {$ENDIF}
    {$IFDEF CPUX64}
      {$IFDEF INFLATE_TO_24}
      junk: array[1..INFLATE_CEILING-SUPER_MEM_BLOCK_CORE_SIZE] of byte;//24 is not possible
      {$ENDIF}
      {$IFDEF INFLATE_TO_32}
      junk: array[1..INFLATE_CEILING-SUPER_MEM_BLOCK_CORE_SIZE] of byte;
      {$ENDIF}
    {$ENDIF}


  end;
    procedure smb_SetNextBlock(const self: PSuperMemBlock; const Value: pointer);{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure smb_SetPreviousBlock(const self: PSuperMemBlock; const Value: pointer);{$IFNDEF NOINLINE}inline;{$ENDIF}
    function smb_GetisLinked(const self: PSuperMemBlock): BBBool;{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure smb_Init(const self: PSuperMemBlock; const UserSize: NativeInt);{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure smb_SetAvailable(const self: PSuperMemBlock; const b: BBBool);{$IFNDEF NOINLINE}inline;{$ENDIF}

//    function smb_GetSizeIndexSize(self: PSuperMemBlock): NativeInt;
    function smb_GetUserDataArea(const self: PSuperMemBlock): pointer;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function smb_GetHeaderSize(const self: PSuperMemBlock): NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function smb_GetUserAreaSize(const self: PSuperMemBlock): NativeInt;
    function smb_GetDelphiAllocSize(const self: PSuperMemblock): NativeInt;
    function smb_GetWasteBytes(const self: PSuperMemBlock): NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function smb_GetAvailable(const self: PSuperMemBlock): BBBool;{$IFNDEF NOINLINE}inline;{$ENDIF}

//    property Available:BBBool read GetAvailable write SetAvailable;
//    property NextBlock: pointer read FNextBlock write SetNextBlock;
//    property PreviousBlock: pointer read FPreviousBlock  write SetPreviousBlock;
//    property IsLinked: BBBool read GetisLinked;

type



  TBlockManager = class;

  TlockedBlockList = class;

  PLockedBlockList = {$IFNDEF USELISTCLASS}^{$ENDIF}TLockedBlockList;

  TLockedBlockList = {$IFDEF USELISTCLASS}class(THeapObject){$ELSE}object{$ENDIF}
  private
    FWasteBytes: NativeInt;
    FBlockCount: NativeInt;
    FUsedBytes: NativeInt;
    FFreeBytes: NativeInt;
    {$IFDEF USE_SPINLOCK}
    sl: TSpinLock;
    {$ELSE}
    sect: TCLXCriticalSection;
    {$ENDIF}
    tip: PSuperMemBlock;
    FFreePool: BBBool;
    Fowner: TBlockManager;
    FAllocTo: TLockedBlockList;
    FFreeTo: TLockedBlockList;
    FFinalized: BBBool;
    FlastUsed: cardinal;

    function DiscreetExtract: PSuperMemBlock;//{$IFNDEF NOINLINE}inline;{$ENDIF}
    function Extract: PSuperMemBlock;//{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure TallyStats(const block: PSuperMemBlock);{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure UnTallyStats(const block: PSuperMemBlock);{$IFNDEF NOINLINE}inline;{$ENDIF}
    function GEtFreeBytes: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function GetAllocBytes: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function GEtBlockCount: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function GetUsedBytes: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function GetWasteBytes: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}

  public

    {$IFDEF USELISTCLASS}
    constructor Create(Aowner: pointer); reintroduce; virtual;
    destructor Destroy; override;
    {$ENDIF}
    procedure Lock;{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure Unlock;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function TryLock: BBBool;{$IFNDEF NOINLINE}inline;{$ENDIF}

    procedure Link(const block: PSuperMemBlock);{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure Unlink(const block: PSuperMemBlock);{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure DiscreetLink(const block: PSuperMemBlock);{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure DiscreetUnlink(const block: PSuperMemBlock);{$IFNDEF NOINLINE}inline;{$ENDIF}

    procedure LockedUnlink(const block: PSuperMemBlock);{$IFNDEF NOINLINE}inline;{$ENDIF}
    function LockedExtract: PSuperMemBlock;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function TryLockedExtract: PSuperMemBlock;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function TryDiscreetLockedExtract: PSuperMemBlock;{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure LockedLInk(const block: PSuperMemBlock);{$IFNDEF NOINLINE}inline;{$ENDIF}

    property FreeBytes: NativeInt read GEtFreeBytes;
    property AllocBytes: NativeInt read GetAllocBytes;
    property BlockCount: NativeInt read GEtBlockCount;
    property UsedBytes: NativeInt read GetUsedBytes;
    property WasteBytes: NativeInt read GetWasteBytes;

    property FreePool: BBBool read FFreePool write FFreePool;

    procedure Init(AOwner: TBlockManager);{$IFNDEF NOINLINE}inline;{$ENDIF}
    function Finalize: BBBool;{$IFNDEF NOINLINE}inline;{$ENDIF}
    property Owner: TBlockMAnager read FOwner;
    property AllocTo: TLockedBlockList read FallocTo write FAllocTo;
    property FreeTo: TLockedBlocklist read FFreeTo write FFreeTo;
{$IFDEF HOLD_MEMORY}
    function TimeSinceUsed: cardinal;
{$ENDIF}
  end;


  //PLockedBlockList = {$IFNDEF USELISTCLASS}^{$ENDIF}TLockedBlockList;

  TBlockManager=class(THeapObject)
  private
    function GetFree: BBBool;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function GetAllocBytes: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function GetBlockCount: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function GetUsedBlockCount: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function GetFreeBlockCount: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function GetFreeBytes: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function GetUsedBytes: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function GetWasteBytes: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
  protected
    //!Free flags/variables
    FFree: BBBool;
    FFreeTime: cardinal;
    FFreeing: BBBool;
    Fblocks: array[0..MAXBIT] of TLockedBlockList;
    FFreeBlocks: array[0..MAXBIT] of TLockedBlockList;
    FDestroyCountdown: integer;
    FTimeOfLastClean: cardinal;


    sect: TCLXCriticalSection;

    //!Statistics


    //!Key Block Management functions
    function ResizeBlock(block: pSuperMEmBlock;
      Size: NativeInt): PSuperMemBlock;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function NeedBlock(UserSize: NativeInt): PSuperMemBlock;{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure PrefetchExtraBlock(UserSize: NativeInt);{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure NoNeedBlock(const block: pSuperMemBlock);{$IFNDEF NOINLINE}inline;{$ENDIF}

    //!Key Block Management functions for talking to OS
    function AllocateNewBlock(const UserSize: NativeInt): PSuperMemBlock; virtual;
    procedure FreeBlockToOS(const block:PSuperMemBlock); {$IFNDEF NOINLINE}inline;{$ENDIF}

    //!Key Link list management functions
    procedure LinkAllocated(const block: PSuperMemBlock);{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure UnlinkAllocated(const block: PSuperMemBlock);{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure LinkFreed(const block: PSuperMemBlock);{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure  UnlinkFreed(const block: PSuperMemBlock);{$IFNDEF NOINLINE}inline;{$ENDIF}

    //!Debug
    function CheckAlloc: BBBool;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function CheckFree: BBBool;{$IFNDEF NOINLINE}inline;{$ENDIF}

    //!Construction
    procedure InitBlocks;{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure FinalizeBlocks;virtual;

    //!Getters/Setters
    function GEtBlockHeaderOverhead: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
//    function GetFreeBlockCount(idx: NativeInt): NativeInt;
//    function GetRealWasteBytes: NativeInt;
    procedure SetFree(const Value: BBBool);{$IFNDEF NOINLINE}inline;{$ENDIF}
    function GetDestroyCount: NativeInt;
    function GetEmptying: BBBool;{$IFNDEF NOINLINE}inline;{$ENDIF}
  public
    //create/destroy
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;


    //!Newmem, FreeMem, Reallocmem handlers
    function ResizeMemory(p: pointer; Size: NativeInt): pointer;virtual;
    function NeedMemory(const UserSize: NativeInt): pointer;virtual;
    procedure NoNeedMemory(const p: pointer);virtual;

    //!Managers talking to other managers
    function ExtractBlock(const UserSize: NativeInt): PSuperMemBlock;{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure InjectBlock(const block: PSuperMemBlock);{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure RemanageBlock(const block: PSuperMemBlock);{$IFNDEF NOINLINE}inline;{$ENDIF}

    //!Instrumentation/Statistics

    property BlockHeaderOverhead: NativeInt read GEtBlockHeaderOverhead;

    //!Flags
    property Freeing: BBBool read FFreeing write FFreeing;
    property Emptying: BBBool read GEtEmptying;

    property IsFree: BBBool read GetFree write SetFree;
    property DestroyCount: NativeInt read GetDestroycount;
    procedure DestroyCountdown;
    procedure ResetCountdown;{$IFNDEF NOINLINE}inline;{$ENDIF}
    property FreeTime: cardinal read FFreeTime;

    //!Locks
    function TryLock: BBBool;{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure Lock;{$IFNDEF NOINLINE}inline;{$ENDIF}
    Procedure Unlock;{$IFNDEF NOINLINE}inline;{$ENDIF}

    //!Garbage collection
    procedure Clean; virtual;

    //!debug
    function CheckMem: BBBool;{$IFNDEF NOINLINE}inline;{$ENDIF}
//    property RealWasteBytes: NativeInt read GetRealWasteBytes;
//    property FreeBlockCount[idx: NativeInt]: NativeInt read GetFreeBlockCount;

    property FreeBytes: NativeInt read GetFreeBytes;
    property AllocBytes: NativeInt read GetAllocBytes;
    property BlockCount: NativeInt read GetBlockCount;
    property UsedBlockCount: NativeInt read GetUsedBlockCount;
    property FreeBlockCount: NativeInt read GetFreeBlockCount;
    property UsedBytes: NativeInt read GetUsedBytes;
    property WasteBytes: NativeInt read GetWasteBytes;

  end;

  TThreadBlockManager = class(TBlockManager)
  //Manager for a specific thread
  private
    FMainMan: TBlockManager;
    FLastOwnedThreadID: cardinal;
  protected
    procedure FinalizeBlocks;override;
  public
    //!Construction/destruction
    constructor Create;override;
    destructor Destroy;override;

    //!New Properties/Methods
    procedure EmptyBlocksToMain;
    property MainMan: TBlockManager read FMainMan write FMainMan;

    //!Overrides
    function AllocateNewBlock(const Size: NativeInt): PSuperMemBlock; override;
    procedure NoNeedBlock(const block: pSuperMemBlock);{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure Clean; override;
    property LastOwnedThreadID: cardinal read FLastOwnedThreadID write FLastOwnedThreadID;
  end;

  TBlockManagerManager=class(THeapObject)
  //Manages block managers
  private
    list: array [0..15000] of TBlockManager;
//    reserve: array[0..30000] of TBlockManager;
    deadlist: array[0..15000] of TBlockManager;
    sect: TCLXCriticalSection;
    dangerlock: TCLXCriticalSection;
    Fcount: NativeInt;
    Fdanger: integer;
    FDeadCount: NativeInt;


    //!Getters/setters
    function GetBlockManager(idx: NativeInt): TBlockManager;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function GetManagerCount: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function GetDeadCount: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}


  public
    //!Construction/destruction
    constructor create;reintroduce; virtual;
    destructor Destroy; override;

    //!Managers
    property Managers[idx: NativeInt]: TBlockManager read GetBlockManager;
    property ManagerCount: NativeInt read GetManagerCount;
    property DeadCount: NativeInt read GetDeadCount;


    function ExtractBlockFromAny(iSize: nativeint): PSuperMemBlock;

    function GetDeadMan: TBlockManager;
    function NewManager: TThreadBlockManager;{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure FreeManager(man: TBlockManager);{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure RegisterManager(bm: TBlockManager);{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure DeregisterManager(bm: TblockManager);{$IFNDEF NOINLINE}inline;{$ENDIF}
    function IndexOf(bm: TblockManager): NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure AddMan(bm: TBlockManager);{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure DeleteMan(idx: NativeInt);{$IFNDEF NOINLINE}inline;{$ENDIF}

    //!Danger zone
    procedure EnterDangerZone;{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure LeaveDangerZone;{$IFNDEF NOINLINE}inline;{$ENDIF}

    //!Locks
    procedure Lock;{$IFNDEF NOINLINE}inline;{$ENDIF}
    procedure Unlock;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function TryLock: BBBool;{$IFNDEF NOINLINE}inline;{$ENDIF}

    //!Garbage collection
    procedure Clean;{$IFNDEF NOINLINE}inline;{$ENDIF}

    //!Experimental
    function LockFirstAvailable: TThreadBlockManager;{$IFNDEF NOINLINE}inline;{$ENDIF}

    function TotalBlockCount: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function TotalFreeBytes: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
    function TotalBytes: NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}

  end;




//!This is crazy stuff for hooking into when a thread ends
type
  PJump=^TJump;
  TJump=packed record
    OpCode:byte;
    Distance:NativeInt;
  end;

var
  OldCode:TJump;
  NewCode:TJump=(OpCode:$E9; Distance:0);
{$IFDEF SIMPLEHEAPS}
  FHeap: NativeInt;
  FHeaps: array[0..31] of NativeInt;
{$ENDIF}
  {$IFDEF USE_SPINLOCKS}
  FHeapLocks: array[0..31] of TSPinLock;
  {$ELSE}
  FHeapLocks: array[0..31] of TCLXCriticalSection;
  {$ENDIF}


//var
//  SingleCPU: BBBool;

procedure NewEndThread(exitCode:integer); register;

//!Helper functions
//!*****************************************************************************
//!Binary Search
function HighOrderBit(const Value:NativeInt):NativeInt;register;


//!Size conversion
function InflateSize(const Size: NativeInt): NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
function GetInflatedIndex(const size: nativeint): nativeint;{$IFNDEF NOINLINE}inline;{$ENDIF}
function GetSizeIndexSize(const BlockSize: NativeInt): NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}
function GetsizeIndex(const BlockSize: NativeInt): NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}

//!Other
{x$DEFINE USE_CUSTOM_MOVEMEM}
{$IFDEF USE_CUSTOM_MOVEMEM}
  {$IFDEF CPUX64}
    {$DEFINE USE_ASM_64}
    {$DEFINE CANT_INLINE_ASM}
  {$ENDIF}
{$ENDIF}

procedure MoveMem32(D,S:Pointer;Size:NativeInt);{$IFNDEF CANT_INLINE_ASM}{$IFNDEF NOINLINE}inline;{$ENDIF}{$ENDIF}//!!
function LesserOf(i1, i2: NativeInt): NativeInt;{$IFNDEF NOINLINE}inline;{$ENDIF}

//!Primary memory manager replacement functions
function NewGetMem(Size: NativeInt): pointer;{$IFDEF ALIGN_MEMORY}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}
function NewFreeMem(p: pointer): integer;{$IFDEF ALIGN_MEMORY}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}
function NewReallocMem(p: pointer; Size: NativeInt): pointer;{$IFDEF ALIGN_MEMORY}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}
function DisabledGetMem(Size: NativeInt): pointer;{$IFDEF ALIGN_MEMORY}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}
function DisabledFreeMem(p: pointer): integer;{$IFDEF ALIGN_MEMORY}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}
function DisabledReallocMem(p: pointer; Size: NativeInt): pointer;{$IFDEF ALIGN_MEMORY}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}

{$IFDEF SIMPLEHEAPS}
function LiteGetMem(Size: NativeInt): pointer;
function LiteFreeMem(p: pointer): Integer;
function LiteReallocMem(p: pointer; Size: NativeInt): pointer;
{$ENDIF}
function OldGetMemA(Size: NativeInt):pointer; {$IFDEF ALLOW_INLINE}inline;{$ENDIF}
procedure OldFreeMemA(p: pointer);{$IFDEF ALLOW_INLINE}inline;{$ENDIF}
function NewGetMemA(Size: NativeInt):pointer;
function NewFreeMemA(p: pointer): integer;
function NewReallocMemA(p: pointer; Size: NativeInt): pointer;
{$IFDEF HOLD_MEMORY}
function LocalGetTickCount: cardinal;{$IFDEF ALLOW_INLINE}inline;{$ENDIF}
{$ENDIF}
procedure UpdateLocalTickCount;{$IFDEF ALLOW_INLINE}inline;{$ENDIF}


procedure ECS(var lpCriticalSection: TCLXCriticalSection);inline;


var
  OldMan: TMemoryManager;
  NewMan: TMemoryManager;
  MainMan: TBlockManager;
  ManMan: TBlockManagerManager;
  HeapOperations: nativeint;
  LocalTickCount: cardinal;

threadvar
  ThreadMemMan: TBlockManager;

var
  SYS: TSystemInfo;


{$ENDIF}
{$IFNDEF DISABLE_MASTER_KEYWORDS}
implementation
{$ENDIF}
{$IFNDEF DISABLE_IMPLEMENTATION}

{$IFDEF TRY_TO_PATCH_ENDTHREAD64_FROM_CORE}
uses Winapi.windows;
{$ENDIF}



function LesserOf(i1, i2: NativeInt): NativeInt;
begin
  if i1 < i2 then
    result := i1
  else
    result := i2;
end;

{ THeapObject }

procedure THeapObject.FreeInstance;
begin
{$IFDEF ALIGN_MEMORY_OF_HEAP_OBJECTS}
  OldFreeMemA(self);
{$ELSE}
  OldMan.FreeMem(self);
{$ENDIF}
end;

class function THeapObject.NewInstance: TObject;
var
  p: pointer;
begin
{$IFDEF ALIGN_MEMORY_OF_HEAP_OBJECTS}
  p := OldGetMemA(InstanceSize);
{$ELSE}
  p := OldMan.GetMem(InstanceSize);
{$ENDIF}
  Result:=InitInstance(p);
end;

function HighOrderBit(const Value:NativeInt):NativeInt;register;
{$IFDEF CPUX64}
asm
  mov rax,0
  bsr rdx,rcx  // find high order bit
  mov rax,rdx

end;
{$ELSE}
asm
  bsr edx,eax  // find high order bit
  mov eax,edx
end;
{$ENDIF}



{ TBlockManager }


function TBlockManager.AllocateNewBlock(const UserSize: NativeInt): PSuperMemBlock;
var
  iHEaderSize: NativeInt;
  iNEwSize: NativeInt;
  bLarge: BBBool;
begin
  result := nil;
  while result = nil do begin
  //increment the size to include enough room for header
  iHeaderSize := SizeOf(TSuperMemBlock);

  //allocate memory from OS
  iNewSize := UserSize+iHeaderSize;
  {$IFDEF DONT_CACHE_LARGE_BLOCKS}
  bLarge := iNewSize > MAX_CACHEABLE_BLOCK_SIZE;
  if (not bLarge) then begin
    iNewSize := GetSizeIndexSize(iNEwSize);
  end;
  {$ELSE}
  iNewSize := GetSizeIndexSize(iNEwSize);
  {$ENDIF}
  {$IFNDEF USELITEASOS}
  result := oldMan.GetMem(iNewSize);
//  result := HeapAlloc(FHeap, 0 , iNewSize);
  {$ELSE}
  result := LiteGetMem(iNewSize);
  {$ENDIF}


  if result = nil then continue;
  //Initialize the result
  smb_Init(result, UserSize);

  {$IFDEF DONT_CACHE_LARGE_BLOCKS}
//  if bLarge then
//    result.Flags := result.Flags or FLAG_LARGE;
  {$ENDIF}

  //Add the block to one of the lists
//  LinkAllocated(result);
  //CheckMem;
  end;

end;

function TBlockManager.CheckAlloc: BBBool;
begin
  result := true;
end;

function TBlockManager.CheckFree: BBBool;
begin
  result := true;
end;

function TBlockManager.CheckMem: BBBool;
begin
//  result := true;
//  exit;
  Lock;
  try
    result := CheckAlloc and CheckFree;
  finally
    Unlock;
  end;
end;



constructor TBlockManager.Create;
begin
  inherited;

  FFree := false;
  FFreeTime := 0;



  InitializeCRiticalSectionAndSPinCount(sect, sys.dwNumberofProcessors*SPIN_COUNT);
  InitBlocks;

end;


destructor TBlockManager.Destroy;
begin
  self.FinalizeBlocks;
  DeleteCriticalSEction(sect);
  inherited;
end;

function TBlockManager.ExtractBlock(const UserSize: NativeInt): PSuperMemBlock;
var
  freeBlock: PSuperMemBlock;
  iSizeIndex: NativeInt;
  bLarge: BBBool;
begin
{$IFDEF LT2018}
  Lock;
{$ENDIF}
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    iSizeIndex := GetInflatedIndex(UserSize);//  iSizeIndex := GetSizeIndex(InflateSize(UserSize));
    freeblock := FFreeBlocks[iSizeIndex].LockedExtract;

//    if freeblock = nil then begin
//      freeBlock := AllocateNewBlock(UserSize);
//    end;

    //CheckMem;

    result := freeBlock;
{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
{$IFDEF LT2018}
    Unlock;
{$ENDIF}
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}


end;

procedure TBlockManager.FreeBlockToOS(const block: PSuperMemBlock);
begin

  {$IFNDEF USELITEASOS}
  oldman.FreeMem(block);
//  BigBrainPro.HeapFree(FHeap, 0, block);
  {$ELSE}
  LiteFreeMem(block);
  {$ENDIF}

//  result := HeapAlloc(FHeap, 0 , iNewSize);
end;


function TBlockManager.GEtBlockHeaderOverhead: NativeInt;
begin
  result := BlockCount*(SizeOf(TSuperMemBlock));
end;



procedure TBlockManager.InitBlocks;
var
  t: NativeInt;
begin
  for t:=0 to MAXBIT do begin
    {$IFDEF USELISTCLASS}
    self.Fblocks[t] := TLockedBlockList.create(self);
    self.FFreeblocks[t] := TLockedBlockList.create(self);
    self.Fblocks[t].FreeTo := self.FFreeblocks[t];
    self.FFreeblocks[t].AllocTo := self.Fblocks[t];

    {$ELSE}
    self.Fblocks[t].init(self);
    self.FFreeblocks[t].init(self);
    self.Fblocks[t].FreeTo := self.FFreeblocks[t];
    self.FFreeblocks[t].AllocTo := self.Fblocks[t];
    {$ENDIF}

    self.FFreeblocks[t].FreePool := true;
  end;

end;

procedure TBlockManager.InjectBlock(const block: PSuperMemBlock);
begin

{$IFDEF LT2018}  Lock;{$ENDIF}
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    LinkFreed(block);
{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
{$IFDEF LT2018}      Unlock;{$ENDIF}
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}

end;

procedure TBlockManager.LinkAllocated(const block: PSuperMemBlock);
begin
  smb_SetAvailable(block, false);
  {$IFNDEF DONT_LINK_ALLOCATED}
  FBlocks[block.SizeIndex].LockedLInk(block);
  {$ELSE}
  block.ManagedBy := FBlocks[block.SizeIndex];
  {$ENDIF}
end;

procedure TBlockManager.LinkFreed(const block: PSuperMemBlock);
begin
  smb_SetAvailable(block, true);
  FFreeBlocks[block.SizeIndex].LockedLink(block);

end;

procedure TBlockManager.Lock;
begin
  {$IFDEF LOCKMAN}
  ECS(sect);
  {$ENDIF}
end;

function TBlockManager.NeedBlock(USerSize: NativeInt): PSuperMemBlock;
var
  freeBlock: PSuperMemBlock;
  iSizeIndex: NativeInt;
begin
{$IFDEF ALLOW_ZERO_LENGTH}
  if UserSize = 0 then
    UserSize := 1;
{$ENDIF}


  //see if there are free blocks of the given size index

  //determine which list to look in
  iSizeIndex := GetSizeIndex(InflateSize(UserSize));

  {$IFDEF DONT_CACHE_LARGE_BLOCKS}
  if iSizeIndex = LARGE_BLOCK_INDEX then
    freeBlock := nil
  else
  {$ENDIF}
    //get a pointer to the first recorded free block of the given size index
    freeBlock := FFreeBlocks[iSizeIndex].LockedExtract;


  if freeBlock = nil then begin
    //if there are no free blocks of the given size then allocate a new one
    result := self.AllocateNewBlock(UserSize);
    {$IFDEF STATS}
    PSuperMemBlock(result).UsedBytes := UserSize; //! this kinda appears to be redundant
    {$ENDIF}
    LinkAllocated(result);
  end else begin
    //otherwise reactivate the freed block
    result := freeBlock;
    if not smb_GetAvailable(result) then
      beep(100,10);
    //UnlinkFreed(result);
    {$IFDEF STATS}
    PSuperMemBlock(result).UsedBytes := UserSize;
    {$ENDIF}
    LinkAllocated(result);
  end;




end;

function TBlockManager.NeedMemory(const UserSize: NativeInt): pointer;
begin
  result := PAnsiChar(NeedBlock(UserSize))+SizeOf(TSuperMemBlock);
end;

procedure TBlockManager.NoNeedBlock(const block: pSuperMemBlock);
begin
  UnlinkAllocated(block);
//  if block.pad <> 0 then
//    beep(100,10);

  if not IsFree then
    LinkFreed(block)
  else
    MainMan.LinkFreed(block);






end;

procedure TBlockManager.NoNeedMemory(const p: pointer);
var
  block: PSuperMemBlock;
begin
  block := pSuperMEmBlock(PAnsiChar(p)-pointer(SizeOf(TsuperMemBlock)));
  NoNeedBlock(block);
end;

procedure TBlockManager.RemanageBlock(const block: PSuperMemBlock);
begin
  Lock;
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    LinkAllocated(block);
{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
    Unlock;
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}

end;

procedure TBlockManager.ResetCountdown;
begin
  FDestroyCountdown := 0;
end;

function TBlockManager.ResizeBlock(block: pSuperMEmBlock; Size: NativeInt): PSuperMemBlock;
var
  uas: nativeint;
begin
  uas := smb_GetUserAreaSize(block);
  if (Size < uas) then begin
    result := block;
    {$IFDEF STATS}block.UsedBytes := Size;{$ENDIF}

//    CheckMem;
  end else begin
    result := NeedBlock(Size);
    MoveMem32(smb_GetUserDataArea(result), smb_GetUserDataArea(block), LesserOf(Size, uas));
    NoNeedBlock(block);
  end;

  {$IFDEF STATS}result.UsedBytes := Size;{$ENDIF}


end;

function TBlockManager.ResizeMemory(p: pointer; Size: NativeInt): pointer;
var
  block: PSuperMemBlock;
begin
  block := PSuperMemBlock(PAnsiChar(p)-pointer(SizeOf(TsuperMemBlock)));
  block := ResizeBlock(block, Size);

(*  if block<>block.Addr then
    result := OldMan.ReallocMem(p, size)
  else*)
    result := PAnsiChar(block)+SizeOf(TsuperMemBlock);

end;

function TBlockManager.TryLock: BBBool;
begin
  {$IFDEF LOCKMAN}
  result := TryEnterCriticalSection(sect);
  {$ELSE}
  result := true;
  {$ENDIF}
end;

procedure TBlockManager.UnlinkAllocated(const block: PSuperMemBlock);
begin
  {$IFNDEF DONT_LINK_ALLOCATED}
  FBlocks[block.SizeIndex].LockedUnlink(block);
  {$ENDIF}
end;

procedure TBlockManager.UnlinkFreed(const block: PSuperMemBlock);
begin
  FFreeBlocks[block.SizeIndex].LockedUnlink(block);
end;

procedure TBlockManager.Unlock;
begin
  {$IFDEF LOCKMAN}
  LeaveCriticalSection(sect);
  {$ENDIF}
end;

function TBlockManager.GetEmptying: BBBool;
begin
  result := DestroyCount >= 8;
end;


procedure TBlockManager.SetFree(const Value: BBBool);
begin
  FFree := Value;
  if value then
    FFreeTime := GetTickCount
  else begin
    FFreeTime := 0;
    FDestroyCountdown := 0;
    if FDestroyCountdown>0 then
      FDestroyCountdown := 0;

  end;




end;

function TBlockManager.GetFree: BBBool;
begin
  result := FFree;
end;

procedure TBlockManager.PrefetchExtraBlock(UserSize: NativeInt);
var
  pb: PSuperMemBlock;
begin
  pb  := AllocateNewBlock(UserSize);
  LInkFreed(pb);

end;

procedure TBlockManager.FinalizeBlocks;
begin
  inherited;

end;

function TBlockManager.GetAllocBytes: NativeInt;
var
  t: NativeInt;
begin
  result :=0;
  for t:=0 to MAXBIT do begin
    inc(result,FBlocks[t].AllocBytes);
  end;

end;

function TBlockManager.GetBlockCount: NativeInt;
var
  t: NativeInt;
begin
  result :=0;
  for t:=0 to MAXBIT do begin
    inc(result,FBlocks[t].BlockCount);
    inc(result,FFreeBlocks[t].BlockCount);
  end;
end;

function TBlockManager.GetUsedBlockCount: NativeInt;
var
  t: NativeInt;
begin
  result :=0;
  for t:=0 to MAXBIT do begin
    inc(result,FBlocks[t].BlockCount);
    inc(result,FFreeBlocks[t].BlockCount);
  end;
end;
function TBlockManager.GetFreeBlockCount: NativeInt;
var
  t: NativeInt;
begin
  result :=0;
  for t:=0 to MAXBIT do begin
    inc(result,FFreeBlocks[t].BlockCount);
  end;
end;


function TBlockManager.GetFreeBytes: NativeInt;
var
  t: NativeInt;
begin
  result :=0;
  for t:=0 to MAXBIT do begin
    inc(result,FFreeBlocks[t].FreeBytes);
  end;
end;

function TBlockManager.GetUsedBytes: NativeInt;
var
  t: NativeInt;
begin
  result :=0;
  for t:=0 to MAXBIT do begin
    inc(result,FBlocks[t].UsedBytes);
  end;


end;

function TBlockManager.GetWasteBytes: NativeInt;
var
  t: NativeInt;
begin
  result :=0;
  for t:=0 to MAXBIT do begin
    inc(result,FBlocks[t].WasteBytes);
  end;
end;

{ TSuperMemBlock }
function GetSizeIndexSize(const BlockSize: NativeInt): NativeInt;
begin
  result := (1 shl (HighOrderBit(BlockSize{$IFDEF LESSWASTE}-SizeOf(TSuperMemBlock){$ENDIF})+1)){$IFDEF LESSWASTE}+SizeOf(TSuperMemBlock){$ENDIF};
end;

function GetSizeIndex(const BlockSize: NativeInt): NativeInt;
var
  ni: NativeInt;
begin

  ni := BlockSize{$IFDEF LESSWASTE}-SizeOf(TSuperMemBlock){$ENDIF};
//  result := (HighOrderBit(ni)+1);

{$IFDEF DONT_CACHE_LARGE_BLOCKS}
  if ni > MAX_CACHEABLE_BLOCK_SIZE then
    result := LARGE_BLOCK_INDEX
  else
{$ENDIF}
    result := (HighOrderBit(ni)+1);

end;

function AddHeaderToSize(UserSize: NativeInt): NativeInt;
begin
  result := Usersize+SizeOf(TsuperMemBlock);
end;


function smb_GetAvailable(const self: PSuperMemBlock): BBBool;
var
  f: byte;
begin
  f := self.Flags;
  result := (f and FLAG_AVAILABLE) > 0;
//  result := (self.Flags or 1) = 1;
end;

function smb_GetisLinked(const self: PSuperMemBlock): BBBool;
begin
  result := (self.FNextBlock<> nil) or (self.FPreviousBlock <> nil);
end;

function smb_HeaderSize(const self: PSuperMemBlock): NativeInt;
begin
  result := SizeOf(TSuperMemBlock);
end;

procedure smb_Init(const self: PSuperMemBlock; const UserSize: NativeInt);
begin
//  Addr := @self;
  smb_SetAvailable(self, true);
  self.FPreviousBlock := nil;
  self.FNextBlock := nil;
  self.ManagedBy := nil;
  self.SizeIndex := GetSizeIndex(UserSize+SizeOf(TSuperMemBlock));
  {$IFDEF STATS}self.UsedBytes := UserSize;{$ENDIF}
  {$IFDEF STATS}self.ReportedUsedBytes := 0;{$ENDIF}

end;

function InflateSize(const Size: NativeInt): NativeInt;
begin
  result := size+SizeOf(TsuperMemBlock);
end;

function GetInflatedIndex(const size: nativeint): nativeint;
var
  ni: nativeint;

begin
  ni := size +sizeof(TSuperMemBlock);
  ni := ni{$IFDEF LESSWASTE}-SizeOf(TSuperMemBlock){$ENDIF};
//  result := (HighOrderBit(ni)+1);

{$IFDEF DONT_CACHE_LARGE_BLOCKS}
  if ni > MAX_CACHEABLE_BLOCK_SIZE then
    result := LARGE_BLOCK_INDEX
  else
{$ENDIF}
    result := (HighOrderBit(ni)+1);

end;

//Memory Manager Hooks


procedure MoveMem32(D,S: pointer; Size: NativeInt);
{$IFNDEF USE_CUSTOM_MOVEMEM}
begin
  move(Pbyte(s)^,PBYte(d)^,size);
end;
{$ELSE}
//a: Jason Nelson
//High speed ASM memory copy.  Safe for overlapped memory regions.
//p: D: Destination Pointer
//p: S: Source Pointer
//p: Size: Size to move in bytes
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

(*procedure MoveMem32(D,S:Pointer;Size:NativeInt);//!!
asm
  push edi
  push esi
  push ecx
  shr ecx,2
  and ecx, $3FFFFFFF
  mov edi,eax
  mov esi,edx
  rep movsd
  pop ecx
  and ecx,3
  rep movsb
  pop esi
  pop edi
end;*)







procedure smb_SetAvailable(const self: PSuperMemblock; const b: BBBool);
var
  f: byte;
begin
  f := self.Flags;
  if b then
    self.Flags := f or FLAG_AVAILABLE
  else
    self.Flags := f and not FLAG_AVAILABLE;
end;

procedure smb_SetNextBlock(const self: PSuperMemblock; const Value: pointer);
begin
  self.FNextBlock := Value;
end;

procedure smb_SetPreviousBlock(const self: PSuperMemblock; const Value: pointer);
begin
  self.FPreviousBlock := Value;
end;

function smb_GetSizeIndexSize(self: PSuperMemblock): NativeInt;
begin
{$IFDEF DONT_CACHE_LARGE_BLOCKS}
  if self.SizeIndex = LARGE_BLOCK_INDEX then
    result := 0
  else
{$ENDIF}
    result := (1 shl self.SizeIndex){$IFDEF LESSWASTE}+SizeOf(TSuperMemBlock){$ENDIF}
end;

function smb_GetDelphiAllocSize(const self: PSuperMemblock): NativeInt;
begin
  result := PNativeInt(PByte(self)-sizeof(NativeInt))^;

end;

function smb_GetUserAreaSize(const self: PSuperMemblock): NativeInt;
begin
  result := smb_GetSizeIndexSize(self)-smb_GetHeaderSize(self);
end;

function smb_GetUserDataArea(const self: PSuperMemBlock): pointer;
var
  i: NativeInt;
begin
  i := NativeInt(Self);
  inc(i, smb_GetHeaderSize(self));

  result := pointer(i);
end;


function smb_GetWasteBytes(const self: PSuperMemblock): NativeInt;
var
  uas: NativeInt;
  rub: nativeint;
begin
  {$IFDEF STATS}
  uas := smb_GetUserAreaSize(self);
  {$IFDEF DONT_CACHE_LARGE_BLOCKS}
  if uas > MAX_CACHEABLE_BLOCK_SIZE then begin
    result := 0;
    exit;
  end;
  {$ENDIF}

  rub := self.ReportedUsedBytes;
  if rub > 0 then
    result := uas - rub
  else
    result := uas - self.UsedBytes;
{$ELSE}
  result := 0;
{$ENDIF}
end;

{ TThreadBlockManager }



procedure UnPatchEndThread;
var
  EndThreadAddr:PJump;
  OldProtect,Protect:DWord;
begin
  EndThreadAddr:=Pointer(@EndThread);
  NewCode.Distance:=NativeInt(@NewEndThread)-(NativeInt(@EndThread)+5);
  {$IFNDEF LINUX}
  VirtualProtect(EndThreadAddr,5,PAGE_READWRITE,OldProtect);
  {$ENDIF}
  EndThreadAddr^:=OldCode;
  {$IFNDEF LINUX}
  VirtualProtect(EndThreadAddr,5,OldProtect,Protect);
  FlushInstructionCache(GetCurrentProcess, EndThreadAddr,5);
  {$ENDIF}
end;



function TThreadBlockManager.AllocateNewBlock(const Size: NativeInt): PSuperMemBlock;
begin
  //ask the main manager for a block

{$IFDEF LT2018}
  MainMan.Lock;
{$ENDIF}
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
  {$IFDEF DONT_CACHE_LARGE_BLOCKS}
    result := nil;
    if Size <= MAX_CACHEABLE_BLOCK_SIZE then
  {$ENDIF}
      result := MAinMan.ExtractBlock(Size);

{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
{$IFDEF LT2018}
    MainMan.Unlock;
{$ENDIF}
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}

  if result = nil then begin
    result := ManMan.ExtractBlockFromAny(size);
    if result = nil then
      result := inherited AllocateNewBlock(Size);
  end;

  //LinkAllocated(result);

end;

procedure TBlockManager.Clean;
{$IFNDEF DO_NOT_GIVE_TO_OS}
var
  t,cx: NativeInt;
  block: PSuperMemBlock;
  bl: TLockedBlockList;
{$ENDIF}
begin
  UpdateLocalTickCount;
{$IFNDEF DO_NOT_GIVE_TO_OS}
  if not trylock then exit;

{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    cx := 0;
    for t:= MAXBIT downto 0 do begin

      repeat
        bl := FFreeBlocks[t];
        if bl.TimeSinceUsed > (OS_BLOCK_HOLD_TIME shr (t shr 8)) then begin
          block := bl.TryDiscreetLockedExtract;
          if block<> nil then begin
            Resetcountdown;

            self.FreeBlockToOS(block);
          end;
          inc(cx);
        end else
          block := nil;



      until (block = nil) or (cx > 64);
    end;

    //CheckMem;

{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
    Unlock;
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}
{$ENDIF}
end;


procedure TThreadBlockManager.Clean;
var
  t,u: NativeInt;
  block: PSuperMemBlock;
  cx: NativeInt;
  bl: TLockedBlockList;
begin

  Lock;
  try
    if IsFree then
      cx := 10
    else
      cx := 3;



    for t:= MAXBIT downto 0 do begin
      for u := 0 to cx do begin
        bl := FFreeblocks[t];

{$IFDEF HOLD_MEMORY}
        if bl.TimeSinceUsed < THREAD_BLOCK_HOLD_TIME[t] then begin
          //{$IFDEF MESSAGES}OutputDebugString(pchar('waiting'));{$ENDIF MESSAGES}
          continue;
        end;
{$ENDIF}


        if bl.TryLock then
        try
          block := bl.DiscreetExtract;
          if block<> nil then begin
            //UnlinkFreed(block);
            MainMan.InjectBlock(block);
          end else
            break;
        finally
          bl.unlock;
        end;
      end;
    end;

    //CheckMem;

  finally
    Unlock;
  end;
end;
constructor TThreadBlockManager.Create;
begin
  inherited;
  ManMan.RegisterManager(self);
end;

destructor TThreadBlockManager.Destroy;
begin
  ManMan.DeregisterManager(self);
  EmptyBlocksToMain;
  inherited;
end;

procedure TThreadBlockManager.EmptyBlocksToMain;
var
  t: NativeInt;
  block: PSuperMemBlock;
  plist_main, plist_this: TlockedBlockList;
begin
//  beep(100,100);
  Lock;
//    while FreeBlockCount > 0 do
    for t:=MAXBIT downto 0 do begin

      repeat

        plist_main := MainMan.FFreeBlocks[t];
        plist_this := FFreeBlocks[t];


        plist_main.Lock;
        plist_this.Lock;
        block := plist_this.Extract;
        if block<> nil then begin
          MainMan.InjectBlock(block);
        end;
        plist_this.UnLock;
        plist_main.UnLock;





      until block = nil;
  //      sleep(1);
    end;
    UnLock;

    Lock;
//    while UsedBlockCount > 0 do
    for t:=MAXBIT downto 0 do begin
      repeat
        MainMan.FBlocks[t].Lock;
        FBLocks[t].Lock;

        block := FBlocks[t].Extract;
        if block <> nil then
//          MainMan.FBlocks[t].LockedLInk(block);
          MainMan.REmanageBlock(block);

        FBLocks[t].UnLock;
        MainMan.FBlocks[t].UnLock;



      until block = nil;
  //    sleep(1);
    end;
  Unlock;

//  sleep(4000);
//  beep(200,100);


end;

procedure TThreadBlockManager.FinalizeBlocks;
var
  t: NativeInt;
  b: PSuperMemBlock;
begin
  inherited;
  for t:=0 to MAXBIT do begin
    while not self.Fblocks[t].Finalize do begin
      b := FBlocks[t].LockedExtract;
      MainMan.RemanageBlock(b);
    end;

    while not self.FFreeblocks[t].Finalize do begin
      b := FFreeBlocks[t].LockedExtract;
      MainMan.InjectBlock(b);
    end;

  end;

end;

procedure TThreadBlockManager.NoNeedBlock(const block: pSuperMemBlock);
begin
  inherited NoNeedBlock(block);

(*  if FFreeBytes > FAllocBytes then begin
    UnlinkFreed(block);
    MainMan.InjectBlock(block);
  end;*)


end;

{ TBlockManagerManager }

procedure TBlockManagerManager.AddMan(bm: TBlockManager);
var
  i: NativeInt;
begin
  i := FCount;
  inc(FCount);
(*  if FCount > length(list) then
    SetLength(list, Fcount+1);)*)


  list[i] := bm;
end;


procedure TBlockManagerManager.Clean;
var
  t: NativeInt;
  man: TBlockManager;
//  tm1, tm2: cardinal;
begin
  {$IFNDEF FREEMAN}exit;{$ENDIF}

  //tm2:= GetTickCount;
  try
    for t:= ManagerCount-1 downto 0 do begin
      man := Managers[t];
      if man.IsFree then begin
        man.Lock;
        try
          //tm1 := man.FreeTime;
          man.Freeing := true;

          man.DestroyCountdown;


          if man.DestroyCount = 8 then begin
            TThreadBlockManager(man).EmptyBlocksToMain;
          end;
          if man.DestroyCount > 10 then begin
             {$IFNDEF USEDEADLIST}
               man.free;
               man := nil;
             {$ELSE}
               self.DeregisterManager(man);
             {$ENDIF}
          end;

        finally
          if man <> nil then begin
            man.Unlock;
          end


        end;


(*        //lock no longer needed because blocks should be emptied to main
        //except in the case where a block may have chosen this manager but not yet locked it
        //if threads are in this "danger zone" then exit
        if FDanger > 0 then exit;

        man.Free;*)


        //if (tm2< tm1) or ((tm1+MANAGER_TIMEOUT)<tm2) then begin
      end;
    end;
  finally
  end;



end;

constructor TBlockManagerManager.create;
begin
  inherited;
  InitializeCRiticalSectionAndSPinCount(sect, sys.dwNumberofProcessors*SPIN_COUNT);
  InitializeCRiticalSectionAndSPinCount(dangerlock, sys.dwNumberofProcessors*SPIN_COUNT);
  FCount := 0;
//  SetLength(list, 3000);
  FDanger := 0;


end;

procedure TBlockManagerManager.DeleteMan(idx: NativeInt);
var
  t: NativeInt;
  i: NativeInt;
begin
  if idx<0 then exit;

  i := ManagerCount;
  {$IFDEF USEDEADLIST}
  deadlist[DeadCount] := list[idx];
  inc(Fdeadcount);
  {$ENDIF}

  for t:= idx+1 to i-1 do begin
    list[t-1] := list[t];
  end;

  dec(FCount);

end;


function TBlockManagerManager.GetDeadMan: TBlockManager;
begin
  Lock;
  try
    if deadcount=0 then begin
      result := nil;
      exit;
    end;

    result := deadlist[DeadCount-1];
    result.ResetCountdown;
    result.IsFree := false;
    deadlist[deadcount-1] := nil;
    dec(FDeadCount);
    self.AddMan(result);
  finally
    Unlock;
  end;


end;

procedure TBlockManagerManager.DeregisterManager(bm: TblockManager);
begin
  ManMan.Lock;
  try
    self.DeleteMan(IndexOf(bm));
  finally
    ManMan.Unlock;
  end;
end;

destructor TBlockManagerManager.Destroy;
begin


  DeleteCriticalSection(dangerlock);
  DeleteCriticalSection(Sect);
  inherited;

end;

procedure TBlockManagerManager.EnterDangerZone;
begin
//  Lock;
  {$IFDEF FREEMAN}
    {$IFDEF USE_ALTERNATE_DANGER_LOCK}
      EnterCriticalSection(dangerlock);
      inc(FDanger);
    {$ELSE}
      InterlockedIncrement(Fdanger);
    {$ENDIF}
  {$ENDIF}
end;

var
  volatile_last_man: nativeint;

function TBlockManagerManager.ExtractBlockFromAny(iSize: nativeint): PSuperMemBlock;
var
  start: nativeint;
  t: nativeint;
begin
  Lock;
  try
    if volatile_last_man > FCount then start := 0
    else start := volatile_last_man;
    for t := start to Fcount-1 do begin
      result := Managers[volatile_last_man].ExtractBlock(iSize);
      if result <> nil then begin
        volatile_last_man := t;
        exit;
      end;
    end;

    for t := 0 to start -1 do begin
      result := Managers[volatile_last_man].ExtractBlock(iSize);
      if result <> nil then begin
        volatile_last_man := t;
        exit;
      end;
    end;
    result := nil;
  finally
    Unlock;
  end;

end;

procedure TBlockManagerManager.FreeManager(man: TBlockManager);
begin
  man.lock;
  try
    man.IsFree := true;


  finally
    man.unlock;
  end;
end;

function TBlockManagerManager.GetBlockManager(idx: NativeInt): TBlockManager;
begin
  result := list[idx];
end;

function TBlockManagerManager.GetDeadCount: NativeInt;
begin
  result := FDeadCount;
end;

function TBlockManagerManager.GetManagerCount: NativeInt;
begin
  result := FCount;
end;

function TBlockManagerManager.IndexOf(bm: TblockManager): NativeInt;
var
  t: NativeInt;
begin
  result := -1;
  for t:= 0 to ManagerCount-1 do begin
    if list[t]=bm then begin
      result :=t;
      break;
    end;
  end;

end;

procedure TBlockManagerManager.LeaveDangerZone;
begin
//  Unlock;
  {$IFDEF FREEMAN}
    {$IFDEF USE_ALTERNATE_DANGER_LOCK}
      dec(FDanger);
      LeaveCriticalSection(dangerlock);
    {$ELSE}
      InterlockedDecrement(Fdanger);
    {$ENDIF}

  {$ENDIF}
end;

procedure TBlockManagerManager.Lock;
begin
  ECS(sect);
end;

function TBlockManagerManager.LockFirstAvailable: TThreadBlockManager;
var
  icount: NativeInt;
  t: NativeInt;
begin
  result := nil;
  while result = nil do begin
    {$IFDEF FREEMAN}Lock;try{$ENDIF}
    iCount := self.FCount;

    for t:= 1 to iCount-1 do
    if self.Managers[t].TryLock then begin
      result := TThreadBlockManager(Managers[t]);
      break;
    end;



    {$IFDEF FREEMAN}finally UnLock; end;{$ENDIF}





  end;

end;

function TBlockManagerManager.NewManager: TThreadBlockManager;
var
  t: NativeInt;
  man : TBlockManager;
begin
  {$IFDEF MESSAGES}OutputDebugString('TBlockManagerManager.NewManager');{$ENDIF}
  Lock;
  try
    result := nil;
    for t:= 1 to managercount-1 do begin
      man := Managers[t];
      man.Lock;
      try
        if (man.Isfree) and not (man.Freeing) and (man.destroycount = 0) then begin
          man.Isfree := false;
          result := tThreadBlockManager(man);
          break;
        end;
      finally
        man.Unlock;
      end;
    end;

    //if we didn't find a free manager then
    //check the dead list
    if result=nil then
      result := TThreadBlockManager(GetDeadMan);

    //else create an entirely new one
    if (result=nil) then
      result := TThreadBlockManager.create;

    result.LastOwnedThreadId := GetCurrentThreadId;


  finally
    Unlock;
  end;
end;

procedure TBlockManagerManager.RegisterManager(bm: TBlockManager);
begin
  ManMan.Lock;
  try
    self.AddMan(bm);
  finally
    ManMan.Unlock;
  end;
end;


function TBlockManagerManager.TotalBlockCount: NativeInt;
var
  t: NativeInt;
begin
  result := 0;
  for t:= 0 to ManagerCount-1 do begin
    Managers[t].Lock;
    try
      inc(result, Managers[t].BlockCount);
    finally
      Managers[t].Unlock;
    end;
  end;

end;

function TBlockManagerManager.TotalFreeBytes: NativeInt;
var
  t: NativeInt;
begin
  result := 0;
  for t:= 0 to ManagerCount-1 do begin
    Managers[t].Lock;
    try
      inc(result, Managers[t].FreeBytes);
    finally
      Managers[t].Unlock;
    end;
  end;

end;

function TBlockManagerManager.TotalBytes: NativeInt;
var
  t: NativeInt;
begin
  result := 0;
  for t:= 0 to ManagerCount-1 do begin
    Managers[t].Lock;
    try
      inc(result, Managers[t].AllocBytes);
    finally
      Managers[t].Unlock;
    end;
  end;

end;



function TBlockManagerManager.TryLock: BBBool;
begin
  result := TryEnterCriticalSection(sect);
end;

procedure TBlockManagerManager.Unlock;
begin
  LeaveCriticalSection(sect);
end;

procedure PatchEndThread;
// redirect calls to System.EndThread to NewEndThread
{$DEFINE TRY_TO_PATCH_ENDTHREAD64_FROM_CORE}
{$IFDEF TRY_TO_PATCH_ENDTHREAD64_FROM_CORE}
begin
  SystemThreadEndProc := NewEndThread;
end;
{$ELSE}
{$IFDEF CPUX64}
begin
//TODO -cunimplemented: unimplemented block
end;
{$ELSE CPUX64}
var
  EndThreadAddr:PJump;
  OldProtect,Protect:DWord;
begin

  {$IFDEF MESSAGES}OutputDebugString('**** PatchEndThread');{$ENDIF}

  EndThreadAddr:=Pointer(@EndThread);
  NewCode.Distance:=NativeInt(@NewEndThread)-(NativeInt(@EndThread)+5);
  {$IFNDEF LINUX}
  VirtualProtect(EndThreadAddr,5,PAGE_READWRITE,OldProtect);
  {$ENDIF}
  OldCode:=EndThreadAddr^;
  EndThreadAddr^:=NewCode;
  {$IFNDEF LINUX}
  VirtualProtect(EndThreadAddr,5,OldProtect,Protect);
  FlushInstructionCache(GetCurrentProcess, EndThreadAddr,5);
  {$ENDIF}
end;
{$ENDIF CPUX64}
{$ENDIF}

//------------------------------------------------------------------------------
procedure NewEndThread(exitCode:integer); register;
begin

  if ThreadMemMan <> nil then begin
    if ThreadMemMan is TThreadBlockManager then begin
      ManMan.FreeManager(ThreadMemMan);
    end;
  end;

  ThreadMemMan := nil;
  {$IFDEF LINUX}
  pthread_exit(exitCode)
  {$ELSE}
  ExitThread(exitCode);
  {$ENDIF}
end;
//------------------------------------------------------------------------------
function NewGetMem(Size: NativeInt): pointer;
var
  lman: TBlockManager;
begin
  {$IFDEF COUNT_HEAP_OPERATIONS}inc(HeapOperations);{$ENDIF}
//  {$IFDEF MESSAGES}OutputDebugString('**** NewGetMem');{$ENDIF}
//  b := SingleCPU;
//  if b then
//    MainMan.Lock;


  lman := ThreadMemMan;
  if lman = nil then begin
    lman := ManMan.NewManager;
    ThreadMemMan := lman;
    TThreadBlockManager(lman).MainMan := MainMan;
  end;

  result := lMan.NeedMemory(Size);

//  if b then
//    MainMan.Unlock;

end;

function DisabledGetMem(Size: NativeInt): pointer;{$IFDEF ALIGN_MEMORY}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}
begin
  ManMan.Lock;
  inc(HeapOperations);
  result := oldman.GetMem(size);
  ManMan.Unlock;
end;
function DisabledFreeMem(p: pointer): integer;{$IFDEF ALIGN_MEMORY}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}
begin
  ManMan.Lock;
  inc(HeapOperations);
  result := oldman.FreeMem(p);
  ManMan.Unlock;
end;

function DisabledReallocMem(p: pointer; Size: NativeInt): pointer;{$IFDEF ALIGN_MEMORY}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}
begin
  ManMan.Lock;
  inc(HeapOperations);
  result := oldman.ReallocMem(p,size);
  ManMan.Unlock;
end;



//------------------------------------------------------------------------------
function NewFreeMem(p: pointer): integer;
var
  block: PSuperMemBlock;
//  man: TBlockManager;
  list: PLockedBlockList;
//  b: BBBool;
  cx: NativeInt;
  bLarge: bbbool;
label
  retry;
begin
  {$IFDEF COUNT_HEAP_OPERATIONS}inc(HeapOperations);{$ENDIF}
//  b := SingleCPU;
//  if b then
//    MainMan.Lock;

  result := 0;
  cx := 0;
(*(  if ThreadMemMan = nil then begin
    ThreadMemMan := ManMan.NewManager;
    TThreadBlockManager(ThreadMemMan).MainMan := MainMan;
  end;
  man := TThreadBlockManager(ThreadMemMan);*)

retry:
  block := PSuperMemBlock(PAnsiChar(p)-SizeOf(TSuperMemblock));
  list := PLockedBlockList(block.ManagedBy);
//  man := list.Owner;

{$IFNDEF NO_REMANAGE_TRAP}
  if list=nil then begin
    //sleep;
    inc(cx);
    if cx < 50 then
      goto retry
    else begin
      MainMan.RemanageBlock(block);
      goto retry;
    end;
  end;
{$ENDIF}

  list.Lock;
  if (PLockedBlockList(block.ManagedBy) <> list) then begin
    if list <> nil then
      list.Unlock;
    goto retry;
  end;
  try
    list.Unlink(block);

  finally
    list.Unlock;
  end;

  //there is no sense in returning a block to a manager that is not owned by a thread
  //therefore, if the owning manager is not assigned to a thread, give its blocks to the
  //main manager instead.  It should be rare that a block is freed after its original thread
  //has disappeared
  if list.Owner.IsFree then
    MainMan.InjectBlock(block)
  else begin
    {$IFDEF DONT_CACHE_LARGE_BLOCKS}
    if block.SizeIndex = LARGE_BLOCK_INDEX then
      MainMan.FreeBlockToOS(block)
    else
    {$ENDIF}
      list.FreeTo.LockedLink(block);
  end;

//  if b then
//    MainMan.UnLock;

end;
//------------------------------------------------------------------------------
function NewReallocMem(p: pointer; Size: NativeInt): pointer;
var
  block: PSuperMemBlock;
//  man: TBlockManager;
  dest: pointer;
  i: NativeInt;
//  b: BBBool;
label retry;
begin
  {$IFDEF COUNT_HEAP_OPERATIONS}inc(HeapOperations);{$ENDIF}
//  b := SingleCPU;
//  if b then
//    MainMan.Lock;


  //determine if size is small enough to just return same pointer
  block := PSuperMemBlock(PAnsiChar(p)-SizeOf(TSuperMemblock));

  i := smb_GetUserAreaSize(block);
  if size < i then  begin
    result := p;
    {$IFDEF STATS}if block.ReportedUsedBytes = 0 then{$ENDIF}
      {$IFDEF STATS}block.ReportedUsedBytes := block.UsedBytes;{$ENDIF}

    {$IFDEF STATS}block.UsedBytes := size;{$ENDIF}

    exit;
  end;

  dest := NewgetMem(size);
  if i = 0 then
    i := smb_GetDelphiAllocSize(block);
  MoveMem32(dest, p, LesserOf(Size, i));

  NewFreeMem(p);
  result := dest;

//  if b then
//    MainMan.Unlock;

end;


function OldNewGetMem(Size: NativeInt): pointer;
var
  lman: TThreadBlockManager;
begin
  if ThreadMemMan = nil then begin
    ThreadMemMan := ManMan.NewManager;
    TThreadBlockManager(ThreadMemMan).MainMan := MainMan;
  end;

  lman := TThreadBlockManager(ThreadMemMan);
  {$IFDEF HYPERALLOC}
  if not lman.TryLock then begin
    lman := ManMan.LockFirstAvailable;
  end;
  {$ELSE}
  {$IFNDEF LOCKLISTS}
  lMan.Lock;
  {$ENDIF}
  {$ENDIF}
  try

    result := lMan.NeedMemory(Size);

  finally
    {$IFNDEF LOCKLISTS}
      lman.Unlock;
    {$ENDIF}
  end;

end;

//------------------------------------------------------------------------------
function OldNewFreeMem(p: pointer): NativeInt;
var
  block: PSuperMemBlock;
  man: TBlockManager;
label
  retry;
begin
(*  if ThreadMemMan = nil then begin
    ThreadMemMan := TThreadBlockManager.create;
    TThreadBlockManager(ThreadMemMan).MainMan := MainMan;
    ManMan.RegisterManager(ThreadMemMan);
  end;*)

  block := psupermemblock(PAnsiChar(p)-SizeOf(TSuperMemBlock));

retry:
  {$IFDEF FREEMAN}ManMan.EnterDangerZone;{$ENDIF}


  man := TBlockManager(PLockedBlockList(block.ManagedBy).owner);
  {$IFNDEF LOCKLISTS}
  Man.Lock;

  {$IFDEF FREEMAN}ManMan.LeaveDangerZone;{$ENDIF}
  if (PLockedBlockList(block.ManagedBy).owner <> man) or (Man=nil) then begin
    Man.Unlock;
    goto retry;
  end;
  {$ENDIF}
  try

    if Man <> nil then
      Man.NoNeedMemory(p);


  finally
    {$IFNDEF LOCKLISTS}
    Man.Unlock;
    {$ENDIF}
  end;


  result := 0;


end;
//------------------------------------------------------------------------------
function OldNewReallocMem(p: pointer; Size: NativeInt): pointer;
var
  block: PSuperMemBlock;
  man: TBlockManager;
label retry;
begin
(*  if ThreadMemMan = nil then begin
    ThreadMemMan := TThreadBlockManager.create;
    TThreadBlockManager(ThreadMemMan).MainMan := MainMan;
    ManMan.RegisterManager(ThreadMemMan);
  end;*)

  block := psupermemblock(PAnsiChar(p)-SizeOf(TSuperMemBlock));

retry:
  {$IFDEF FREEMAN}ManMan.EnterDangerZone;{$ENDIF}
  man := TBlockManager(plockedblocklist(block.ManagedBy).owner);
  {$IFNDEF LOCKLISTS}
  Man.Lock;

  {$IFDEF FREEMAN}ManMan.LeaveDangerZone;{$ENDIF}
  if (plockedblocklist(block.ManagedBy).owner <> man) or (Man=nil) then begin
    man.unlock;
    goto retry;
  end;
  {$ENDIF}
  try
    result := Man.ResizeMemory(p, Size);


  finally
    {$IFNDEF LOCKLISTS}
    Man.Unlock;
    {$ENDIF}
  end;

(*  if ThreadMEmMan is TThreadBlockManager then
  with ThreadMemMan as TThreadBlockManager do begin
    if TryLock then begin
      EmptyBlocksToMain;
      UnLock;
    end;
  end;*)





end;

{$IFDEF LINUX}

{L}function InitializeCriticalSectionAndSpinCount(var lpCriticalSection: TCLXCriticalSection; dwSpinCount: DWORD): BOOL; cdecl;
begin
  InitializeCriticalSection(lpCriticalSection);
  result := true;
end;

{L}function InitializeCriticalSection(var lpCriticalSection: TCLXCriticalSection): integer;
{L}var
{L}  Attribute: TMutexAttribute;
{L}begin
{L}  Result := pthread_mutexattr_init(Attribute);
{L}  if Result <> 0 then Exit;
{L}  try
{L}    Result := pthread_mutexattr_settype(Attribute, PTHREAD_MUTEX_RECURSIVE);
{L}    if Result <> 0 then Exit;
{L}
{L}    Result := pthread_mutex_init(lpCriticalSection, Attribute);
{L}  finally
{L}    pthread_mutexattr_destroy(Attribute);
{L}  end;
{L}end;


{L}procedure Beep(freq, duration: NativeInt);
{L}begin
{L}
{L}//TODO -cunimplemented: unimplemented block
{L}end;
{L}
{L}function GetTickCount: Cardinal;
{L}var
{L}  tv: timeval;
{L}begin
//{L}  gettimeofday(tv, nil);
{L}  {$RANGECHECKS OFF}
{L}  Result := int64(tv.tv_sec) * 1000 + tv.tv_usec div 1000;
{L
{L     I've implemented this correctly for now. I'll argue for using
{L     an int64 internally, since apparently quite some functionality
{L     (throttle, etc etc) depends on it, and this value may wrap
{L     at any point in time.
{L     For Windows: Uptime > 72 hours isn't really that rare any more,
{L     For Linux: no control over when this wraps.
{L
{L     IdEcho has code to circumvent the wrap, but its not very good
{L     to have code for that at all spots where it might be relevant.
{L
{L   }
{L}end;
{L}
//{L}function TryEnterCriticalSection(var lpCriticalSection: TCLXCriticalSection): BBBool;
//{L}begin
//{L}  Result := pthread_mutex_trylock(lpCriticalSection) <> EBUSY;
//{L}end;
{$ENDIF}




{ TLockedArray }

{$IFDEF USELISTCLASS}
constructor TLockedBlockList.Create(Aowner: pointer);
begin
  inherited create;
  Init(Aowner);
  fFinalized := false;

end;

destructor TLockedBlockList.Destroy;
begin
  if not FFinalized  then
    Finalize;

  inherited;
end;
function TLockedBlockList.DiscreetExtract: PSuperMemBlock;
begin

  if tip = nil then begin
    result := nil;
    exit;
  end
  else begin
    result := tip;
    DiscreetUnlink(tip);

  end;
end;

procedure TLockedBlockList.DiscreetLink(const block: PSuperMemBlock);
//THE ONLY DIFFERENCE BETWEEN THIS AND THE REGULAR LINK/UNLINK IS THAT IT DOESNT
//RESET THE CACHE TIMER
begin
  if (block.Fpreviousblock <> nil)
  or (block.Fnextblock <> nil) then
    halt;

  block.ManagedBy :=   {$IFNDEF USELISTCLASS}@{$ENDIF}self;

  if tip <> nil then begin
    tip.FPreviousBlock := block;
  end;
  block.FNextBlock := tip;


  tip := block;

  smb_SetAvailable(block, FreePool);

  if smb_GetAvailable(block) then begin
    {$IFDEF STATS}block.UsedBytes := 0;{$ENDIF}
//    block.WasteBytes := 0;
  end;


  TallyStats(block);
end;

procedure TLockedBlockList.DiscreetUnlink(const block: PSuperMemBlock);
//THE ONLY DIFFERENCE BETWEEN THIS AND THE REGULAR LINK/UNLINK IS THAT IT DOESNT
//RESET THE CACHE TIMER
var
  bHasPrior, bHasNext: BBBool;
begin
  block.ManagedBy := nil;

  if block = nil then
    exit;

  UnTallyStats(block);

  bHasPrior := block.FPreviousBlock <> nil;
  bHasNext := block.FNextBlock <> nil;


  if bHasPrior then begin
    PSuperMemBlock(block.FPreviousBlock).FNextBlock := block.FNextBlock;
    //block.PreviousBlock := nil;
  end;

  if bHasNext then begin
    PSuperMemBlock(block.FNextBlock).FPreviousBlock := block.FPreviousblock;
    //block.NextBlock := nil;
  end;

  if (block = tip) and bHasNext then
    tip := block.FNextBlock
  else
    tip := nil;


  block.FPreviousBlock := nil;
  block.FNextBlock := nil;

end;

{$ENDIF}

function TLockedBlockList.Extract: PSuperMemBlock;
begin

  if tip = nil then begin
    result := nil;
    exit;
  end
  else begin
    result := tip;
    Unlink(tip);

  end;
end;

function TLockedBlockLIst.Finalize: BBBool;
begin
  if tip <> nil then begin
    result := false;
    exit;
  end;
  result := true;
  {$IFNDEF USE_SPINLOCK}
    DeleteCriticalSection(sect);
  {$ELSE}
    sl.Free;
  {$ENDIF}
  FWasteBytes := 0;
  FFreeBytes := 0;
  FBlockCount := 0;
  FUsedBytes := 0;
  FFreepool := false;

  Fowner := nil;
  self.tip := nil;
  FFinalized := true;

end;

procedure TLockedBlockLIst.Init(AOwner: TBlockManager);
begin
  FWasteBytes := 0;
  FFreeBytes := 0;
  FBlockCount := 0;
  FUsedBytes := 0;
  FFreepool := false;

  Fowner := Aowner;
  self.tip := nil;

  {$IFNDEF USE_SPINLOCK}
    InitializeCriticalSectionAndSpinCount(sect, sys.dwNumberOfProcessors*SPIN_COUNT);
  {$ELSE}
    sl := Tspinlock.Create;
  {$ENDIF}

end;

procedure TlockedBlockList.Link(const block: PSuperMemBlock);
var
  tippy: PSuperMemBlock;
begin
  {$IFDEF EXTRA_ERROR_CHECKING}
  if (block.Fpreviousblock <> nil)
  or (block.Fnextblock <> nil) then
    halt;
  {$ENDIF}

  block.ManagedBy :=   {$IFNDEF USELISTCLASS}@{$ENDIF}self;

  tippy := tip;

  if tippy <> nil then begin
    tippy.FPreviousBlock := block;
  end;
  block.FNextBlock := tippy;

  tip := block;

  smb_SetAvailable(block, FreePool);

  if smb_GetAvailable(block) then begin
    {$IFDEF STATS}block.UsedBytes := 0;{$ENDIF}
//    block.WasteBytes := 0;
  end;


  TallyStats(block);
{$IFDEF HOLD_MEMORY}
  FLastUsed := LocalGetTickCount;
{$ENDIF}


end;

function TLockedBlockLIst.GEtFreeBytes: NativeInt;
begin
  {$IFDEF STATS}
  Lock;
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    result := FFreeBytes;
{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
    Unlock;
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}
  {$ELSE}
      result := 0;
  {$ENDIF}
end;

function TLockedBlockLIst.GetAllocBytes: NativeInt;
begin
  {$IFDEF STATS}
  Lock;
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    result := FUsedBytes+FWasteBytes;
{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
    Unlock;
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}
  {$ELSE}
      result := 0;
  {$ENDIF}
end;
function TLockedBlockLIst.GEtBlockCount: NativeInt;
begin
  {$IFDEF STATS}
  Lock;
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    result := FBlockCount;
{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
    Unlock;
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}
  {$ELSE}
      result := 0;
  {$ENDIF}
end;
function TLockedBlockLIst.GetUsedBytes: NativeInt;
begin
  {$IFDEF STATS}
  Lock;
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    result := FUsedBytes;
{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
    Unlock;
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}
  {$ELSE}
      result := 0;
  {$ENDIF}
end;

function TLockedBlockLIst.GetWasteBytes: NativeInt;
begin
  {$IFDEF STATS}
  Lock;
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    result := FWasteBytes;
{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
    Unlock;
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}
  {$ELSE}
      result := 0;

  {$ENDIF}
end;

procedure TLockedBlockLIst.Lock;
begin
  {$IFDEF LOCKLISTS}
    {$IFDEF USE_SPINLOCK}
      sl.Lock;
    {$ELSE}
      ECS(sect);
    {$ENDIF}
  {$ENDIF}
end;

function TLockedBlockList.TryDiscreetLockedExtract: PSuperMemBlock;
begin
  result := nil;
  if not TryLock then exit;
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    result := DiscreetExtract;
{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
    Unlock;
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}
end;

function TLockedBlockList.TryLock: BBBool;
begin
  {$IFDEF LOCKLISTS}
    {$IFDEF USE_SPINLOCK}
      result := sl.TryLock(1);
    {$ELSE}
      result := TryEnterCriticalSection(sect);
    {$ENDIF}
  {$ELSE}
  result := true;
  {$ENDIF}

end;

function  TLockedBlockList.LockedExtract: PSuperMemBlock;
begin
  Lock;
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    result := Extract;
{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
    Unlock;
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}
end;

function  TLockedBlockList.TryLockedExtract: PSuperMemBlock;
begin
  result := nil;
  if not TryLock then exit;
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    result := Extract;
{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
    Unlock;
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}
end;


procedure TlockedBlockList.LockedLInk(const block: PSuperMemBlock);
begin
  Lock;
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    Link(block);
{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
    Unlock;
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}
end;

procedure TlockedBlockList.LockedUnlink(const block: PSuperMemBlock);
begin
  Lock;
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    Unlink(block);
{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
    Unlock;
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}

end;

procedure TlockedBlockList.TallyStats(const block: PSuperMemBlock);
begin
  {$IFDEF STATS}
  if not FreePool then begin
    block.ReportedUsedBytes := 0;
    inc(self.FWasteBytes, smb_GetWasteBytes(block));
    inc(self.FUsedBytes, block.UsedBytes);
  end else begin
    block.UsedBytes := 0;
    block.ReportedUsedBytes := 0;
    inc(self.FFreeBytes, smb_GetWasteBytes(block));
  end;
  inc(self.FBlockCount);
  {$ENDIF}

end;

{$IFDEF HOLD_MEMORY}
function TLockedBlockList.TimeSinceUsed: cardinal;
begin
  if tip = nil then
    result := 0
  else
    result := localGetTickCount- FlastUsed;
end;
{$ENDIF}

procedure TblockManager.DestroyCountdown;
begin
{$IFDEF MACOS}
  Lock;
  try
    inc(FDestroyCountDown);
  finally
    Unlock;
  end;
{$ELSE}
  interlockedincrement(FDestroyCountdown);
{$ENDIF}

end;

function TblockManager.GetDestroyCount: NativeInt;
begin
  result := FDestroyCountdown;

end;



procedure TlockedBlockList.Unlink(const block: PSuperMemBlock);
var
  bHasPrior, bHasNext: BBBool;
  nxt,prev: PSuperMemBlock;
begin
  if block = nil then
    exit;

  block.ManagedBy := nil;


  UnTallyStats(block);

  bHasPrior := block.FPreviousBlock <> nil;
  bHasNext := block.FNextBlock <> nil;

  nxt := PSuperMemBlock(block.FNextBlock);
  prev := PSuperMemBlock(block.FPreviousBlock);


  if bHasPrior then begin
    prev.FNextBlock := nxt;
    //block.PreviousBlock := nil;
  end;

  if bHasNext then begin
    nxt.FPreviousBlock := prev;
    //block.NextBlock := nil;
  end;

  if (block = tip) and bHasNext then
    tip := block.FNextBlock
  else
    tip := nil;


  block.FPreviousBlock := nil;
  block.FNextBlock := nil;
{$IFDEF HOLD_MEMORY}
  FLastUsed := LocalGetTickCount;
{$ENDIF}
end;

function smb_GetHeaderSize(const self: PSuperMemBlock): NativeInt;
begin
  result := sizeof(TSuperMemBlock);
end;

procedure TLockedBlockLIst.Unlock;
begin
  {$IFDEF LOCKLISTS}
    {$IFDEF USE_SPINLOCK}
      sl.Unlock;
    {$ELSE}
      LeaveCriticalSection(sect);
    {$ENDIF}
  {$ENDIF}
end;

procedure TLockedBlockList.UnTallyStats(const block: PSuperMemBlock);
begin
  {$IFDEF STATS}
  if not FreePool then begin
    if block.ReportedUsedBytes > 0 then begin
      dec(self.FUsedBytes, block.ReportedUsedBytes);
      dec(self.FWasteBytes, smb_GetWasteBytes(block));
      block.ReportedUsedBytes := 0;
    end else begin
      dec(self.FUsedBytes, block.UsedBytes);
      dec(self.FWasteBytes, smb_GetWasteBytes(block));
    end;

  end else begin
    block.UsedBytes := 0;
    block.ReportedUsedBytes := 0;
    dec(self.FFreeBytes, smb_GetWasteBytes(block));
  end;
  dec(self.FBlockCount);
  {$ENDIF}
end;

{$IFDEF LINUX}
procedure GetSystemInfo(var lpSystemInfo: TsystemInfo); stdcall;
begin
  lpSystemInfo.dwNumberOfProcessors := 2;
end;
{$ENDIF}


procedure EnableLowFragmentation(h: NativeInt);
var
  frag_value: NativeInt;
  heap_type: NativeInt;
begin
    {$IFDEF LOW_FRAG}
    heap_type := 2;
//    heap_type := 99;
    {$ELSE}{$IFDEF LOOK_ASIDE}
    heap_type := 1;
    {$ENDIF}
    {$ELSE}
    heap_type := 0;
    {$ENDIF}


    {$IFNDEF LINUX}
    if not HeapSetInformation(h, @heap_type, frag_value, sizeof(frag_value)) then begin
//      beep(1000,10);
      frag_value := 0;
      if not HeapSetInformation(h, @heap_type, frag_value, sizeof(frag_value)) then begin
        heap_type := 0;
        if not HeapSetInformation(h, @heap_type, frag_value, sizeof(frag_value)) then
          halt;
      end;
    end else begin
//      beep(100,10);
    end;
    {$ENDIF}

end;

{$IFDEF ATOMICLOCKS}
function TryLockAtomic(var lock: TAtomicLock):BBBool;
var
  iBefore, iAfter: NativeInt;
begin
  result := false;
  iBefore := lock.lock;

  if (iBefore > 0) and (lock.handle <> GetCurrentThreadID) then
    exit;

  iAfter := InterlockedIncrement(lock.lock);

  if (iBefore+1)<>iAfter then begin
    InterlockedDecrement(lock.lock);
  end else begin
    result := true;
    lock.handle := GetCurrentThreadID;
  end;
end;

procedure LockAtomic(var lock: TAtomicLock);
begin
  while not TryLockAtomic(lock) do ;

end;
procedure UnLockAtomic(var lock: TAtomicLock);
begin
  InterlockedDecrement(lock.lock);
end;

procedure InitAtomic(var lock: TAtomicLock);
begin
  lock.lock := 0;

end;
{$ENDIF}

procedure ECS(var lpCriticalSection: TCLXCriticalSection);inline;
begin
  {$IFDEF TRY_BEFORE_BUY}
  if not TryEnterCriticalSection(lpCriticalSection) then begin
    //sleep(0);
    EnterCriticalSection(lpCriticalSection);
  end;
  {$ELSE}
    EnterCriticalSection(lpCriticalSection);
  {$ENDIF}
end;

{$IFDEF ATOMICLOCKS}
procedure InitializeCriticalSection(var lpCriticalSection: TCLXCriticalSection);{$IFNDEF ATOMICLOCKS} stdcall;{$ENDIF}
begin
  InitAtomic(lpCriticalSection);
end;

procedure EnterCriticalSection(var lpCriticalSection: TCLXCriticalSection);  {$IFNDEF ATOMICLOCKS} stdcall;{$ELSE}{$IFDEF ALLOW_INLINE}inline;{$ENDIF}{$ENDIF}
begin
  LockAtomic(lpCriticalSection);
end;

function TryEnterCriticalSection(var lpCriticalSection: TCLXCriticalSection): BOOL; {$IFNDEF ATOMICLOCKS} stdcall;{$ENDIF}
begin
  result := TryLockAtomic(lpCriticalSection);
end;

procedure LeaveCriticalSection(var lpCriticalSection: TCLXCriticalSection); {$IFNDEF ATOMICLOCKS} stdcall;{$ENDIF}
begin
  UnlockAtomic(lpCriticalSection);
end;

procedure DeleteCriticalSection(var lpCriticalSection: TCLXCriticalSection); {$IFNDEF ATOMICLOCKS} stdcall;{$ENDIF}
begin
//  DeleteAtomic(lpCriticalSection);
end;


{$ENDIF}


{$IFDEF SIMPLEHEAPS}
procedure InitHeaps;
var
  t: NativeInt;
begin
  for t:= 0 to 31 do begin
    FHeaps[t] := HeapCreate({$IFDEF LITE_EXTERNAL_LOCKS}$00000005{$ELSE}$00000004{$ENDIF}+$00040000, 0, 0);
    EnableLowFragmentation(FHeaps[t]);
    {$IFNDEF ATOMICLOCKS}
    InitializeCriticalSectionAndSpinCount(FHeapLocks[t], sys.dwNumberOfProcessors*SPIN_COUNT);
    {$ELSE}

    {$ENDIF}

  end;
end;


procedure LockHeap(n: NativeInt);
begin
  {$IFDEF LITE_EXTERNAL_LOCKS}
    {$IFDEF USE_SPINLOCKS}
      FHeaplocks[n].Lock;
    {$ELSE}
      EnterCriticalSection(FHeapLocks[n]);
    {$ENDIF}
  {$ENDIF}
end;
procedure UnlockHeap(n: NativeInt);
begin
  {$IFDEF LITE_EXTERNAL_LOCKS}
    {$IFDEF USE_SPINLOCKS}
      FHeaplocks[n].UnLock;
    {$ELSE}
      LeaveCriticalSection(FHeapLocks[n]);
    {$ENDIF}

  {$ENDIF}
end;
{$ENDIF}


function SelectHeap(Size: NativeInt): NativeInt;
//var
//  t: NativeInt;
begin
  result := highorderbit(size);
//  inc(FHeap);
//  result := FHeap;
//
//  if result > 3 then begin
//    FHeap := 0;
//    result := 0;
//  end;
//
//  exit;
//
//  result := -1;
//  while result = -1 do
//  for t:= 0 to 31 do begin
//    if TryLockHeap(FHeaps[t]) then begin
//      result := t;
//      exit;
//    end;
//  end;

end;

{$IFDEF SIMPLEHEAPS}
function LiteGetMem(Size: NativeInt): pointer;
type
  pint = ^NativeInt;
var
  h: NativeInt;
begin
  h := SelectHeap(Size);

  LockHeap(h);
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    result := HeapAlloc(FHeaps[h], 0, Size+LITE_HEADER_SIZE);


{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
    UnlockHeap(h);
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}


//  pint(result)^ := Size;
  pint(result)^ := size;
  result := pointer(PAnsiChar(result)+LITE_HEADER_SIZE);


end;


function LiteFreeMem(p: pointer): Integer;
type
  pint = ^NativeInt;
var
  size: NativeInt;
  h: NativeInt;
  pp: pointer;
begin
  pp := pointer(PAnsiChar(p)-LITE_HEADER_SIZE);
  size := pint(pp)^;
  h := highorderbit(size);
  LockHeap(h);
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    HeapFree(FHeaps[h], 0, pp);


{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
    UnLockHeap(h);
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}

  result := 0;
end;

function LiteReallocMem(p: pointer; Size: NativeInt): pointer;
type
  pint = ^NativeInt;
var
  h: NativeInt;
  pp: pointer;
  h2: NativeInt;
  oldsize: NativeInt;
  i: NativeInt;
begin
  pp := pointer(PAnsiChar(p)-LITE_HEADER_SIZE);
  oldsize := pint(pp)^;
  h := Highorderbit(oldsize);

  LockHeap(h);
{$IFDEF EXTRA_ERR_CHECKING}try{$ENDIF}
    h2 := h;
    h2 := SelectHeap(size);

    //this is for reallocation from the same heap
    if h=h2 then begin
      result := HeapRealloc(FHeaps[h], 0, pp, Size+LITE_HEADER_SIZE);


    end else
    //allocation from a different heap
    begin
      LockHeap(h2);
      try
        result := HeapAlloc(FHeaps[h2], 0, Size+LITE_HEADER_SIZE);

      finally
        UnLockHeap(h2);
      end;
      i := oldsize;
      if size < oldsize then
        i := size;

      Movemem32(pointer(PAnsiChar(result)+LITE_HEADER_SIZE), p, i);
      HeapFree(FHeaps[h],0,pp);
    end;
{$IFDEF EXTRA_ERR_CHECKING}finally{$ENDIF}
    UnLockHeap(h);
{$IFDEF EXTRA_ERR_CHECKING}end;{$ENDIF}

  pint(PAnsiChar(result))^ := size;
  result := pointer(PAnsiChar(result)+LITE_HEADER_SIZE);


end;
{$ENDIF}

{$IFDEF WINDOWS2000_COMPATIBLE}
function HeapSetInformation(hHeap: THandle; heapinfo: pointer; var HeapFragValue; ValueSize: integer): BOOL; stdcall;
begin
  result := true;
end;
{$ENDIF}



{$IFDEF USE_SPINLOCK}
const
  SGlobalSharedSpinLockMapping = '';//'\Session\%d\$$SharedSpinLockMapping$$'; // DO NOT LOCALIZE


type
  PLockMapArray = ^TLockMapArray;
  TLockMapArray = array[0..MaxLocks-1] of TLockMap;

{ compares CompareVal and Target and returns Target's old value
  if they're equal, Target is set to NewVal }
function LockCmpxchg(CompareVal, NewVal: Cardinal; var Target: Cardinal): Cardinal;
asm
  lock cmpxchg [ecx], edx
end;

{ increments Target by Source and returns Target's old value }
function LockXadd(const Source: Cardinal; var Target: Cardinal): Cardinal;
asm
  lock xadd [edx], eax
end;

constructor TSpinLock.Create(const SleepAmount: Cardinal;
  const NoSleepCount: Cardinal);
begin
  inherited Create;
  FSleepAmount := SleepAmount;

  InitNoSleepCount(NoSleepCount);
  InitLockMap;
end;

destructor TSpinLock.Destroy;
begin
  ReleaseLockMap;
  FOwner := nil;
  inherited;
end;

procedure TSpinLock.Lock;
{$IFNDEF ASM}
var
  LOwner: Cardinal;
  LLock: Cardinal;
  LSleepCounter: Cardinal;
  LSleepAmount: Cardinal;
begin
  LLock := GetCurrentThreadId;
  if FNoSleepCount > 0 then
  begin
    LSleepCounter := 0;
    LOwner := LockCmpxchg(0, LLock, FOwner.Lock);
    while (LOwner <> 0) and (LOwner <> LLock) and (LSleepCounter < FNoSleepCount) do
    begin
      Inc(LSleepCounter);
      LOwner := LockCmpxchg(0, LLock, FOwner.Lock);
    end;
  end;

  LSleepAmount := 0;
  LOwner := LockCmpxchg(0, LLock, FOwner.Lock);
  while (LOwner <> 0) and (LOwner <> LLock) do
  begin
    Sleep(LSleepAmount);
    LOwner := LockCmpxchg(0, LLock, FOwner.Lock);
    {$IFDEF DynamicSleep}
    if LSleepAmount < FSleepAmount then
      Inc(LSleepAmount, LSleepAmount + 1);
    {$ENDIF}
  end;
  Inc(FOwner.LockCount);
{$ELSE}
asm
  {
    EBX - Pointer to Self
    ECX - ThreadID
    EDX - FLock
    ESI - FNoSleepCount to 0 (decrementing), 0 to FSleepAmount (incrementing)
  }

  mov ecx, fs:[$00000018]       // get thread information block
  mov edx, [eax].FOwner         // move owner to edx
  mov ecx, [ecx+$24]            // get thread id into ecx (moved here for instruction pairing)
  push ebx                      // save ebx and store self in it
  mov ebx, eax
  xor eax, eax                  // clear eax for comparison to 0
  lock cmpxchg [edx], ecx       // if no owner, then set our thread id
  je @@exitImmediate            // if wasn't owned then we've locked and exit
  cmp eax, ecx                  // if we own it then exit (recursive locks)
  je @@exitImmediate
  push esi
  mov esi, [ebx].FNoSleepCount  // set nosleep counter
  test esi, esi                 // if nosleep is 0 then jump to waitLoop
  jz @@waitLoop
  inc esi
  xor eax, eax

@@noSleepLoop:
  dec esi                       // decrement local NoSleepCount
  jz @@waitLoop                 // if we reached 0 then go to waitable loop
  pause
  cmp [edx], 0                  // check the owner (volatile read)
  jne @@noSleepLoop             // if it's owned then repeat the loop
  lock cmpxchg [edx], ecx       // try to obtain lock
  jz @@exitNoSleep              // if obtained we exit else enter waitable loop

@@waitLoop:
  push ecx                      // save ecx and edx coz Sleep modifies them
  push edx
{$IFDEF DynamicSleep}
  push esi                      // esi has our sleep amount
  xor eax, eax
  cmp esi, [ebx].FSleepAmount   // compare local SleepAmount counter
  cmovc eax, esi                // if less than SleepAmount then copy to eax
  adc esi, eax                  // if compare was less we double esi and add 1
{$ELSE}
  push [ebx].FSleepAmount
{$ENDIF}
  call Sleep
  pop edx                       // restore edx and ecx
  pop ecx
  pause
  cmp [edx], 0                  // check the owner (volatile read)
  jne @@waitLoop                // if it's owned then repeat the loop
  xor eax, eax
  lock cmpxchg [edx], ecx       // try to obtain lock
  jnz @@waitLoop                // if lock failed then reenter the loop

@@exitNoSleep:
  pop esi

@@exitImmediate:
  inc [edx+$04]                 // increment lock count
  pop ebx
{$ENDIF}
end;

function TSpinLock.trylock(TryCount: Cardinal): BBBool;
{$IFNDEF ASM}
var
  LOwner: Cardinal;
  LLock: Cardinal;
  LSleepCounter: Cardinal;
  LSleepAmount: Cardinal;
begin
  if TryCount = 0 then
    Inc(TryCount);

  LLock := GetCurrentThreadId;
  // if not owned, set ourself as owner
  if FNoSleepCount > 0 then
  begin
    LSleepCounter := 0;
    LOwner := LockCmpxchg(0, LLock, FOwner.Lock);
    while (LOwner <> 0) and (LOwner <> LLock)
      and (LSleepCounter < FNoSleepCount) and (TryCount > 0) do
    begin
      Inc(LSleepCounter);
      Dec(TryCount);
      LOwner := LockCmpxchg(0, LLock, FOwner.Lock);
    end;
  end;

  LSleepAmount := 0;
  LOwner := LockCmpxchg(0, LLock, FOwner.Lock);
  while (LOwner <> 0) and (LOwner <> LLock) and (TryCount > 0) do
  begin
    Sleep(LSleepAmount);
    Dec(TryCount);
    LOwner := LockCmpxchg(0, LLock, FOwner.Lock);
    {$IFDEF DynamicSleep}
    if LSleepAmount < FSleepAmount then
      Inc(LSleepAmount, LSleepAmount + 1);
    {$ENDIF}
  end;

  Result := TryCount > 0;
  if Result then
    Inc(FOwner.LockCount);
{$ELSE}
asm
  {
    EBX - Pointer to Self
    ECX - ThreadID
    EDX - FLock
    ESI - FNoSleepCount to 0 (decrementing), 0 to FSleepAmount (incrementing)
    EDI - TryCount
  }

  mov ecx, 1                    // set ecx to 1 for comparing TryCount
  push edi                      // save edi
  mov edi, edx                  // copy TryCount to edi
  cmp edi, ecx                  // check if TryCount is less than 1
  adc edi, 1                    // increment TryCount and add 1 if it was 0

  mov ecx, fs:[$00000018]       // get thread id into ecx
  mov edx, [eax].FOwner         // move owner to edx
  mov ecx, [ecx+$24]            // moved here for instruction pairing
  push ebx                      // save ebx and store self in it
  mov ebx, eax
  xor eax, eax                  // clear eax for comparison to 0
  lock cmpxchg [edx], ecx       // if no owner, then set our thread id
  je @@exitImmediate            // if wasn't owned then we've locked and exit
  cmp eax, ecx                  // if we own it then exit (recursive locks)
  je @@exitImmediate
  push esi
  mov esi, [ebx].FNoSleepCount  // set nosleep counter
  test esi, esi                 // if nosleep is 0 then jump to waitLoop
  jz @@waitLoop
  inc esi
  xor eax, eax

@@noSleepLoop:
  dec esi                       // decrement local NoSleepCount
  jz @@waitLoop                 // if we reached 0 then go to waitable loop
  dec edi                       // decrement TryCount
  jz @@exitNoSleep              // if we reached 0 then exit
  pause
  cmp [edx], 0                  // check the owner (volatile read)
  jne @@noSleepLoop             // if it's owned then repeat the loop
  lock cmpxchg [edx], ecx       // try to obtain lock
  jz @@exitNoSleep              // if obtained we exit else enter waitable loop

@@waitLoop:
  dec edi                       // decrement TryCount
  jz @@exit                     // if we reached 0 then exit
  push ecx                      // save ecx and edx coz Sleep modifies them
  push edx
{$IFDEF DynamicSleep}
  push esi                      // esi has our sleep amount
  xor eax, eax
  cmp esi, [ebx].FSleepAmount   // compare local SleepAmount counter
  cmovc eax, esi                // if less than SleepAmount then copy to eax
  adc esi, eax                  // if compare was less we double esi and add 1
{$ELSE}
  push [ebx].FSleepAmount
{$ENDIF}
  call Sleep
  pop edx                       // restore edx and ecx
  pop ecx
  pause
  cmp [edx], 0                  // check the owner (volatile read)
  jne @@waitLoop                // if it's owned then repeat the loop
  xor eax, eax
  lock cmpxchg [edx], ecx       // try to obtain lock
  jnz @@waitLoop                // if lock failed then reenter the loop

@@exit:
@@exitNoSleep:
  pop esi

@@exitImmediate:
  xor eax, eax
  cmp eax, edi                  // check TryCount
  adc [edx+$04], 0              // if it's above 0 then increment lock count
  mov eax, edi                  // store result
  pop ebx
  pop edi
{$ENDIF}
end;

procedure TSpinLock.InitLockMap;
begin
  FOwner := OldMan.GetMem(SizeOf(TLockMap));
//  FOwner := AllocMem(SizeOf(TLockMap));
end;

procedure TSpinLock.InitNoSleepCount(const NoSleepCount: Cardinal);
var
  LPAM: Cardinal;
  LSAM: Cardinal;
  LCPUCount: Cardinal;
  I: NativeInt;
begin
  // check on how many CPUs our process can run
  // if it's only 1 then there's no use in looping without sleep
  if GetProcessAffinityMask(GetCurrentProcess, LPAM, LSAM) then
  begin
    LCPUCount := 0;
    for I := 0 to 31 do
      if (LPAM and (1 shl I)) <> 0 then
        Inc(LCPUCount);
  end
  else
    LCPUCount := 1;

  if LCPUCount > 1 then
    FNoSleepCount := NoSleepCount
  else
    FNoSleepCount := 0;
end;

procedure TSpinLock.unlock;
{$IFNDEF ASM}
var
  LLock: Cardinal;
begin
  LLock := GetCurrentThreadId;
  if (FOwner.Lock <> LLock) then
    Exit;

  if (FOwner.LockCount > 0) then
    Dec(FOwner.LockCount);

  if FOwner.LockCount = 0 then
    FOwner.Lock := 0;
{$ELSE}
asm
  mov edx, [eax].FOwner         // store self in edx
  mov eax, fs:[$00000018]       // get thread id
  mov eax, [eax+$24]            // store thread id in eax
  cmp [edx], eax                // test if we own the lock, exit if not
  jne @@exit
  xor ecx, ecx                  // clear ecx for reseting the lock
  dec [edx+$04]                 // decrement lock count
  cmovz eax, ecx                // if reached 0, copy ecx to eax
  mov [edx], eax                // update the lock with threadId or 0
@@exit:
  db $F3, $C3                   // Two-Byte Near-Return RET
{$ENDIF}
end;

procedure TSpinLock.ReleaseLockMap;
begin
  OldMan.FreeMem(Fowner);
//  FreeMem(FOwner);
end;
{$ENDIF}

{$IFDEF CPUX86}

function InterlockedAdd(var Addend: Integer; Increment: Integer): Integer;
asm
      MOV   ECX,EAX
      MOV   EAX,EDX
 LOCK XADD  [ECX],EAX
      ADD   EAX,EDX
end;

function InterlockedCompareExchange(var Target: Integer; Exchange: Integer; Comparand: Integer): Integer;
asm
      XCHG    EAX,ECX
 LOCK CMPXCHG [ECX],EDX
end;

function InterlockedCompareExchangePointer(var Target: Pointer; Exchange: Pointer; Comparand: Pointer): Pointer;
asm
      JMP InterlockedCompareExchange
end;

function InterlockedDecrement(var Addend: Integer): Integer;
asm
      MOV   EDX,-1
      JMP   InterlockedAdd
end;

function InterlockedExchange(var Target: Integer; Value: Integer): Integer;
asm
      MOV     ECX,EAX
      MOV     EAX,[ECX]
@@loop:
 LOCK CMPXCHG [ECX],EDX
      JNZ     @@loop
end;

function InterlockedExchangePointer(var Target: Pointer; Value: Pointer): Pointer;
asm
      JMP InterlockedExchange
end;

function InterlockedIncrement(var Addend: Integer): Integer;
asm
      MOV   EDX,1
      JMP   InterlockedAdd
end;

{$ENDIF CPUX86}

{$IFDEF CPUX64}

function InterlockedExchangeAdd(var Addend: Integer; Value: Integer): Integer;
asm
      .NOFRAME
      MOV   EAX,EDX
 LOCK XADD  [RCX].Integer,EAX
end;

function InterlockedDecrement(var Addend: LongInt): LongInt;
asm
      .NOFRAME
      MOV   EAX,-1
 LOCK XADD  [RCX].Integer,EAX
      DEC   EAX
end;

function InterlockedIncrement(var Addend: LongInt): LongInt;
asm
      MOV   EAX,1
 LOCK XADD  [RCX].Integer,EAX
      INC   EAX
end;

function InterlockedCompareExchange(var Destination: Integer; Exchange: Integer; Comparand: Integer): Integer;
asm
      .NOFRAME
      MOV     EAX,R8d
 LOCK CMPXCHG [RCX].Integer,EDX
end;

function InterlockedCompareExchange64(var Destination: Int64; Exchange: Int64; Comparand: Int64): Int64; overload;
asm
      .NOFRAME
      MOV     RAX,R8
 LOCK CMPXCHG [RCX],RDX
end;

function InterlockedCompareExchangePointer(var Destination: Pointer; Exchange: Pointer; Comparand: Pointer): Pointer;
asm
      .NOFRAME
      MOV     RAX,R8
 LOCK CMPXCHG [RCX],RDX
end;

function InterlockedExchangePointer(var Target: Pointer; Value: Pointer): Pointer;
asm
       .NOFRAME
  LOCK XCHG [RCX],RDX
       MOV RAX,RDX
end;

function InterlockedExchange(var Target: Integer; Value: Integer): Integer;// inline;
asm
       .NOFRAME
  LOCK XCHG [RCX],EDX
       MOV EAX,EDX
end;

{$ENDIF CPUX64}

//GetMem wrapper that aligns the pointer on a 16 bit boundry
//function NewGetMem(Size: NativeInt): pointer;
//function NewFreeMem(p: pointer): integer;
const
  ALIGN_PADDING = 32;
  ALIGN_SIZE = 16;
  ALIGN_MASK = ALIGN_SIZE-1;

function OldGetMemA(Size: NativeInt):pointer; {$IFDEF ALLOW_INLINE}inline;{$ENDIF}
var
  OriginalAddress : Pointer;
begin
  result := nil;
  OriginalAddress := oldman.GetMem(Size + ALIGN_PADDING); //Allocate users size plus extra for storage
  If OriginalAddress = nil Then Exit; //If not enough memory then exit
  result := PByte(OriginalAddress) + sizeof(nativeint); //We want at least enough room for storage
  NativeInt(result) := (NativeInt(result) + (ALIGN_MASK)) And (Not(ALIGN_MASK)); //align the pointer
  If NativeInt(result) < NativeInt(OriginalAddress) Then Inc(PByte(result),ALIGN_SIZE); //If we went into storage goto next boundry
  Dec(PNativeInt(result)); //Move back 4 bytes so we can save original pointer
  PNativeInt(result)^ := NativeInt(OriginalAddress); //Save original pointer
  Inc(PNativeInt(result)); //Back to the boundry
end;

//Freemem wrapper to free aligned memory
procedure OldFreeMemA(P: Pointer); {$IFDEF ALLOW_INLINE}inline;{$ENDIF}
begin
  Dec(PNativeInt(P)); //Move back to where we saved the original pointer
  NativeInt(P) := PNativeInt(P)^; //Set P back to the original
  Oldman.FreeMem(P); //Free the memory
end;

function NewGetMemA(Size: NativeInt):pointer; {$IFDEF ALLOW_INLINE}inline;{$ENDIF}
var
  OriginalAddress : Pointer;
begin
  result := nil;
  OriginalAddress := NewGetMem(Size + ALIGN_PADDING); //Allocate users size plus extra for storage
  If OriginalAddress = nil Then Exit; //If not enough memory then exit
  result := PByte(OriginalAddress) + sizeof(nativeint); //We want at least enough room for storage
  NativeInt(result) := (NativeInt(result) + (ALIGN_MASK)) And (Not(ALIGN_MASK)); //align the pointer
  If NativeInt(result) < NativeInt(OriginalAddress) Then Inc(PByte(result),ALIGN_SIZE); //If we went into storage goto next boundry
  Dec(PNativeInt(result)); //Move back 4 bytes so we can save original pointer
  PNativeInt(result)^ := NativeInt(OriginalAddress); //Save original pointer
  Inc(PNativeInt(result)); //Back to the boundry
end;

//Freemem wrapper to free aligned memory
function NewFreeMemA(P: Pointer): integer; {$IFDEF ALLOW_INLINE}inline;{$ENDIF}
begin
  Dec(PNativeInt(P)); //Move back to where we saved the original pointer
  NativeInt(P) := PNativeInt(P)^; //Set P back to the original
  result := NewFreeMem(P); //Free the memory
end;

function NewReallocMemA(p: pointer; Size: NativeInt): pointer;
var
  OriginalAddress : Pointer;
begin
  Dec(PNativeInt(P)); //Move back to where we saved the original pointer
  NativeInt(P) := PNativeInt(P)^; //Set P back to the original
  result := NewReallocMem(P,size); //realloc the memory

  OriginalAddress := result;
  If OriginalAddress = nil Then Exit; //If not enough memory then exit
  result := PByte(OriginalAddress) + sizeof(nativeint); //We want at least enough room for storage
  NativeInt(result) := (NativeInt(result) + (ALIGN_MASK)) And (Not(ALIGN_MASK)); //align the pointer
  If NativeInt(result) < NativeInt(OriginalAddress) Then Inc(PByte(result),ALIGN_SIZE); //If we went into storage goto next boundry
  Dec(PNativeInt(result)); //Move back 4 bytes so we can save original pointer
  PNativeInt(result)^ := NativeInt(OriginalAddress); //Save original pointer
  Inc(PNativeInt(result)); //Back to the boundry
end;

{$IFDEF HOLD_MEMORY}
function LocalGetTickCount: cardinal;
begin
  result := localTickcount;
end;
{$ENDIF}

procedure UpdateLocalTickCount;
begin
  localtickCount := GetTickcount;
end;


{$ENDIF}
{$IFNDEF DISABLE_MASTER_KEYWORDS}
initialization
{$ENDIF}
{$IFNDEF DISABLE_INITIALIZATION}

{$IFNDEF NO_INIT}
//  SingleCPU := false;
  {$IFDEF MESSAGES}OutputDebugString('Memory manager is initializing');{$ENDIF}
  Heapoperations := 0;
//  FHeap := GetProcessHeap;
//  FHeap := 0;

  GetMemoryManager(OldMan);
  MainMan := TBlockManager.create;
  ThreadMemMan := nil;

  {$IFNDEF MEMTEST}
  {$IFNDEF LINUX}
  PatchEndThread;
  {$ENDIF}
  {$ENDIF}
{$IFDEF SIMPLEHEAPS}
  InitHeaps;
{$ENDIF}

  {$IFDEF DISABLE_ALL}
  NewMan.GetMem := DisabledGetMem;
  NewMan.FreeMem := DisabledFreeMem;
  NewMan.ReallocMem := DisabledReallocMem;
  {$ELSE}
  {$IFDEF LITEMODE}
  NewMan.GetMem := LiteGetMem;
  NewMan.FreeMem := LiteFreeMem;
  NewMan.ReallocMem := LiteReallocMem;
  {$ELSE}
  {$IFDEF ALIGN_MEMORY}
  NewMan.GetMem := NewGetMemA;
  NewMan.FreeMem := NewFreeMemA;
  NewMan.ReallocMem := NewReallocMemA;
  {$ELSE}
  NewMan.GetMem := NewGetMem;
  NewMan.FreeMem := NewFreeMem;
  NewMan.ReallocMem := NewReallocMem;
  {$ENDIF}

  {$ENDIF}
  {$ENDIF}



  {$IFNDEF ENABLE_ON_SINGLE_CPU}
    GetSystemInfo(sys);
    if sys.dwNumberOfProcessors > 1 then
   {$ENDIF}
      SetMemoryManager(NewMan);


  ManMan := TBlockManagerManager.create;
  ManMan.RegisterManager(MainMan);

  {$IFDEF MESSAGES}OutputDebugString('Memory manager is initialized');{$ENDIF}

  UpdateLocalTickCount;
{$ENDIF}

{$ENDIF}
{$IFNDEF DISABLE_MASTER_KEYWORDS}
finalization
{$ENDIF}
{$IFNDEF DISABLE_FINALIZATION}

{$IFNDEF NO_INIT}

  {$IFNDEF MEMTEST}
  {$IFNDEF LINUX}
  UnPatchEndThread;
  {$ENDIF}
  {$ENDIF}


  ManMan.DeregisterManager(MainMan);
  MainMan.free;
  ManMan.free;

  {$IFNDEF MEMTEST}
  SetMemoryManager(OldMan);
  {$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFNDEF DISABLE_MASTER_KEYWORDS}
end.
{$ENDIF}




