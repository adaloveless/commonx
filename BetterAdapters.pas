unit BetterAdapters;

interface

uses
{$IFDEF MSWINDoWS}
  windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Dlfcn, Posix.Fcntl, Posix.SysStat, Posix.SysTime, Posix.SysTypes,
{$ENDIF}
  types,
  sysutils;



function BetterFileOpen(const FileName: string; Mode: LongWord; Rights: cardinal; flags: cardinal): THandle;
function BetterFileCreate(const FileName: string; Mode: LongWord; Rights: cardinal; flags: cardinal): THandle;

implementation

uses
{$IFDEF POSIX}
  {$IFNDEF ANDROID}
  Posix.Iconv,
  {$ENDIF}
  Posix.Base, Posix.Dirent, Posix.Errno, Posix.Fnmatch,
  Posix.Langinfo, Posix.Locale, Posix.Pthread, Posix.Stdio, Posix.Stdlib,
  Posix.String_, Posix.SysSysctl, Posix.Time, Posix.Unistd, Posix.Utime,
  Posix.Wordexp, Posix.Pwd, Posix.Signal,
{$ENDIF POSIX}
  stringx;


function BetterFileOpen(const FileName: string; Mode: LongWord; Rights: cardinal; flags: cardinal): THandle;
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
      FILE_ATTRIBUTE_NORMAL or flags, 0);
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
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
      LockVar.l_whence := 0;//SEEK_SET;
      LockVar.l_start := 0;
      LockVar.l_len := 0;
      LockVar.l_type := ShareMode[smode];
      Tvar := fcntl(FileHandle, F_SETLK, @LockVar);
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
{$ENDIF POSIX}


function BetterFileCreate(const FileName: string; Mode: LongWord; Rights: cardinal; flags: cardinal): THandle;
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
      ShareMode[(Mode and $F0) shr 4], nil, Exclusive[(Mode and $0004) shr 2], FILE_ATTRIBUTE_NORMAL or Flags, 0);
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
const
  Exclusive: array[0..1] of Cardinal = (
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
      LockVar.l_whence := 0;//SEEK_SET;
      LockVar.l_start := 0;
      LockVar.l_len := 0;
      LockVar.l_type := ShareMode[smode];
      Tvar := fcntl(FileHandle, F_SETLK, @LockVar);
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
{$ENDIF POSIX}


end.
