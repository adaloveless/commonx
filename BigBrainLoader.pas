unit BigBrainLoader;

interface

{$IFDEF MEMORY_DIAG}
type
  TBlockEnumProc = function (Block: Pointer): Boolean;
{$ENDIF}

function SysGetMem(Size: NativeInt): Pointer;
function SysFreeMem(P: Pointer): Integer;
function SysReallocMem(P: Pointer; Size: NativeInt): Pointer;
function SysAllocMem(Size: NativeInt): Pointer;
function SysRegisterExpectedMemoryLeak(P: Pointer): Boolean;
function SysUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
function GetHeapStatus: THeapStatus;
function GetAllocMemCount: Integer;
function GetAllocMemSize: Integer;
procedure DumpBlocks;
procedure HeapAddRef;
procedure HeapRelease;

implementation

{$IFDEF GLOBALALLOC}
uses Windows;
{$ENDIF}

var
  {Need access to the shared memory manager structure to be able to call the
   default AllocMem and leak registration handlers for BBMM.dll libraries
   that do not implement these functions.}
  SharedMemoryManager: TMemoryManagerEx;

const
{$IFDEF CPUX64}
  BBMM = 'BigBrainShareMem64.dll';
{$ELSE}
  BBMM = 'BigBrainShareMem.dll';
{$ENDIF}

function SysGetMem(Size: NativeInt): Pointer; external BBMM name '@Borlndmm@SysGetMem$qqri';
function SysFreeMem(P: Pointer): Integer; external BBMM name '@Borlndmm@SysFreeMem$qqrpv';
function SysReallocMem(P: Pointer; Size: NativeInt): Pointer; external BBMM name '@Borlndmm@SysReallocMem$qqrpvi';
function GetHeapStatus: THeapStatus; external BBMM;
function GetAllocMemCount: Integer; external BBMM;
function GetAllocMemSize: Integer; external BBMM;
procedure DumpBlocks; external BBMM;
procedure ReferenceMemoryCleaner; external BBMM;
procedure DereferenceMemoryCleaner; external BBMM;

function GetModuleHandle(lpModuleName: PChar): Integer; stdcall; external 'kernel32.dll' name 'GetModuleHandleW';
function GetProcAddress(hModule: Integer; lpProcName: PAnsiChar): Pointer; stdcall; external 'kernel32.dll' name 'GetProcAddress';

{$IFDEF GLOBALALLOC}
{$MESSAGE Error Not Supported}
function xSysGetMem(Size: NativeInt): Pointer;
begin
  Result := GlobalAllocPtr(HeapAllocFlags, Size);
end;

function xSysFreeMem(P: Pointer): Integer;
begin
  Result := GlobalFreePtr(P);
end;

function xSysReallocMem(P: Pointer; Size: NativeInt): Pointer;
begin
  Result := GlobalReallocPtr(P, Size, 0);
end;
{$ENDIF}

procedure HeapAddRef;
var
  MM: Integer;
  Proc: procedure;
begin
  MM := GetModuleHandle(BBMM);
  Proc := GetProcAddress(MM, '@Borlndmm@HeapAddRef$qqrv');
  if Assigned(Proc) then
    Proc;
end;

procedure HeapRelease;
var
  MM: Integer;
  Proc: procedure;
begin
  MM := GetModuleHandle(BBMM);
  Proc := GetProcAddress(MM, '@Borlndmm@HeapRelease$qqrv');
  if Assigned(Proc) then
    Proc;
end;

{The default AllocMem implementation - for older BBMM.dll libraries that do
 not implement this themselves.}
function DefaultAllocMem(Size: NativeInt): Pointer;
begin
  Result := SysGetMem(Size);
  if (Result <> nil) then
    FillChar(Result^, Size, 0);
end;

{The default (do nothing) leak registration function for backward compatibility
 with older BBMM.dll libraries.}
function DefaultRegisterAndUnregisterExpectedMemoryLeak(P: Pointer): boolean;
begin
  Result := False;
end;

function SysAllocMem(Size: NativeInt): Pointer;
begin
  {Indirect call, because the library may not implement this functionality}
  Result := SharedMemoryManager.AllocMem(Size);
end;

function SysRegisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  {Indirect call, because the library may not implement this functionality}
  Result := SharedMemoryManager.RegisterExpectedMemoryLeak(P);
end;

function SysUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  {Indirect call, because the library may not implement this functionality}
  Result := SharedMemoryManager.UnregisterExpectedMemoryLeak(P);
end;

procedure InitMemoryManager;
var
  ProcAddr: Pointer;
  MM: Integer;
begin
  // force a static reference to BBMM.dll, so we don't have to LoadLibrary
  SharedMemoryManager.GetMem := SysGetMem;

  MM := GetModuleHandle(BBMM);
{$IFDEF GLOBALALLOC}
  SharedMemoryManager.GetMem := xSysGetMem;
  SharedMemoryManager.FreeMem := xSysFreeMem;
  SharedMemoryManager.ReallocMem := xSysReallocMem;
{$ELSE}
  SharedMemoryManager.GetMem := GetProcAddress(MM,'@Borlndmm@SysGetMem$qqri');
  SharedMemoryManager.FreeMem := GetProcAddress(MM,'@Borlndmm@SysFreeMem$qqrpv');
  SharedMemoryManager.ReallocMem := GetProcAddress(MM, '@Borlndmm@SysReallocMem$qqrpvi');
// Cannot assume that the functions below are implemented. Default handlers are set in initialization section.
  ProcAddr := GetProcAddress(MM,'@Borlndmm@SysAllocMem$qqri');
  if ProcAddr <> nil then
    SharedMemoryManager.AllocMem := ProcAddr;
  ProcAddr := GetProcAddress(MM,'@Borlndmm@SysRegisterExpectedMemoryLeak$qqrpi');
  if ProcAddr <> nil then
    SharedMemoryManager.RegisterExpectedMemoryLeak := ProcAddr;
  ProcAddr := GetProcAddress(MM, '@Borlndmm@SysUnregisterExpectedMemoryLeak$qqrpi');
  if ProcAddr <> nil then
    SharedMemoryManager.UnregisterExpectedMemoryLeak := ProcAddr;
{$ENDIF}
  SetMemoryManager(SharedMemoryManager);
end;

initialization
  SharedMemoryManager.AllocMem := DefaultAllocMem;
  SharedMemoryManager.RegisterExpectedMemoryLeak := DefaultRegisterAndUnregisterExpectedMemoryLeak;
  SharedMemoryManager.UnregisterExpectedMemoryLeak := DefaultRegisterAndUnregisterExpectedMemoryLeak;
  HeapAddRef;
  if not IsMemoryManagerSet then
    InitMemoryManager;

  REferenceMemoryCleaner;
finalization
  DeReferenceMemoryCleaner;
  HeapRelease;
end.
