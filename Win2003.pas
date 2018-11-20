unit Win2003;
//Use of this unit causes your application to require windows 2003 or later

interface

uses windows, typex, sysutils;

type
  localsizet = nativeint;

function SetSystemFileCacheSize(min, max: localsizet; flags: dword): BOOL;stdcall;external windows.kernel32;

function SetPrivilege(hToken: THandle; lpszPrivilege: LPCTSTR; bEnable: BOOL): boolean;overload;
function SetPrivilege(hToken: THandle; priv: widestring; bEnable: BOOL): boolean;overload;

function SetPrivilege(lpszPrivilege: LPCTSTR; bEnable: BOOL): boolean;overload;

function FlushFileCache: boolean;

const
  FILE_CACHE_MAX_HARD_ENABLE = 1;
  FILE_CACHE_MAX_HARD_DISABLE = 2;
  FILE_CACHE_MIN_HARD_DISABLE = 8;
  FILE_CACHE_MIN_HARD_ENABLE = 4;




implementation

function SetPrivilege(lpszPrivilege: LPCTSTR; bEnable: BOOL): boolean;overload;
var
  h: NativeUInt;
begin
  if not OpenProcessToken(GetCurrentProcess, TOKEN_READ+TOKEN_ADJUST_PRIVILEGES, h) then begin
    raise ECritical.create('Failed to Open Process Token');
  end;
  try
    result := SetPrivilege(h, lpszPrivilege, bEnable);
  finally
    CloseHandle(h);
  end;
end;

function SetPrivilege(hToken: THandle; priv: widestring; bEnable: BOOL): boolean;overload;
begin
  result := SetPrivilege(hToken, PWideChar(priv), bEnable);
end;

function SetPrivilege(hToken: THandle; lpszPrivilege: LPCTSTR; bEnable: BOOL): boolean;
var
  tp, prev: TOKEN_PRIVILEGES;
  looid: TLargeInteger;
  sz: cardinal;
begin
  result := false;
  if not LookupPrivilegeValue(nil,lpszPrivilege, looid) then begin
    raise ECritical.create('failed to look up privilege '+inttostr(GetLastError()));
  end;

  tp.PrivilegeCount := 1;
  tp.Privileges[0].Luid  := looid;
  if (bEnable) then
    tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
  else
    tp.Privileges[0].Attributes := 0;


  sz := sizeof(TOKEN_PRIVILEGES);
  if not AdjustTokenPrivileges(htoken, false, tp, sz, prev,sz) then begin
    raise ECritical.create('error in AdjustTokenPrivileges'+inttostr(GetLastError()));
  end;

  if GetLastError = ERROR_NOT_ALL_ASSIGNED then begin
    raise ECritical.create('the security token does not have this privilege: '+lpszPrivilege);
  end;

  result := true;

end;


function FlushFileCache: boolean;
begin
  result := false;
  exit;
  if not SetSystemFileCacheSize(-1,-1,0) then begin
    raise ECritical.create('Failed to flush file cache error '+inttostr(getlasterror));
  end;
  if not SetSystemFileCacheSize(0,32000000,FILE_CACHE_MAX_HARD_ENABLE) then begin
    raise ECritical.create('Failed to set file cache error '+inttostr(getlasterror));
  end;


  result := true;

end;



end.
