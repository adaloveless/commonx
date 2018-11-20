unit ProcessInfo;

interface

uses
  typex, types, windows, classes, sysutils, debug;



type

  TPIDList = array of DWORD;
  TProcInfo = record
    id: DWORD;
    name: string;
  end;
  TProcIterate = reference to Procedure(inf: TProcInfo);


function GetProcessName(PID: DWORD; var ProcessName: string): DWORD;
function GetProcessList(var ProcessList: TPIDList): DWORD;
procedure SetProcessPriority(processname: string; priorityclass: DWORD);
procedure IterateProcesses(pi: TProcIterate);




implementation


uses
  PSAPI, TlHelp32;

procedure IterateProcesses(pi: TProcIterate);
  function GetOSVersionInfo(var Info: TOSVersionInfo): Boolean;
  begin
    FillChar(Info, SizeOf(TOSVersionInfo), 0);
    Info.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
    Result := GetVersionEx(TOSVersionInfo(Addr(Info)^));
    if (not Result) then
    begin
      FillChar(Info, SizeOf(TOSVersionInfo), 0);
      Info.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
      Result := GetVersionEx(TOSVersionInfo(Addr(Info)^));
      if (not Result) then
        Info.dwOSVersionInfoSize := 0;
    end;
  end;

var
  thisproc: TProcInfo;
  dwReturn     : DWORD;
  OS           : TOSVersionInfo;
  // EnumProcesses
  PidProcesses : PDWORD;
  PidWork      : PDWORD;
  BufferSize   : Cardinal;
  Needed       : DWORD;
  cntProcesses : Cardinal;
  i            : Cardinal;
  // CreateToolhelp32Snapshot
  hProcSnapShot: THandle;
  pe32         : TProcessEntry32;
  j            : Cardinal;

begin
  dwReturn := 0;
  // What OS are we running on?
  if GetOSVersionInfo(OS) then
  begin
    if (OS.dwPlatformId = VER_PLATFORM_WIN32_NT) and (OS.dwMajorVersion = 4) then
    // WinNT and higher
    begin
      Needed := 0;
      BufferSize := 1024;
      GetMem(PidProcesses, BufferSize);
      // make sure memory is allocated
      if Assigned(PidProcesses) then
      begin
        try
          // enumerate the processes
          if EnumProcesses(PidProcesses, BufferSize, Needed) then
          begin
            dwReturn := 0;
            cntProcesses := Needed div sizeof(DWORD) - 1;
            PidWork := PidProcesses;
            // walk the processes
            for i := 0 to cntProcesses - 1 do
            begin
              thisproc.id := PidWork^;
              GetProcessName(thisproc.id, thisproc.name);
              pi(thisproc);
            end;
          end
          else // EnumProcesses = False
            dwReturn := GetLastError;
        finally
          // clean up no matter what happend
          FreeMem(PidProcesses, BufferSize);
        end;
      end
      else // GetMem = nil
        dwReturn := GetLastError;
    end
    // Win 9x and higher except WinNT
    else
    begin
      // make the snapshot
      hProcSnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
      if hProcSnapShot <> INVALID_HANDLE_VALUE then
      begin
        pe32.dwSize := sizeof(TProcessEntry32);
        if Process32First(hProcSnapShot, pe32) then
        begin
          // first process
          thisproc.id := pe32.th32ProcessID;
          GetProcessName(thisproc.id, thisproc.name);
          pi(thisproc);
          // walk the processes
          while Process32Next(hProcSnapShot, pe32) do
          begin
            thisproc.id := pe32.th32ProcessID;
            GetProcessName(thisproc.id, thisproc.name);
            pi(thisproc);
          end;
        end
        else // Process32First = False
          dwReturn := GetLastError;
        CloseHandle(hProcSnapShot);
      end
      else // hSnapShot = INVALID_HANDLE_VALUE
        dwReturn := GetLastError;
    end;
  end;
end;

function GetProcessName(PID: DWORD; var ProcessName: string): DWORD;
var
  dwReturn     : DWORD;
  hProc        : Cardinal;
  buffer       : array[0..MAX_PATH - 1] of Char;
begin
  dwReturn := 0;
  Zeromemory(@buffer, sizeof(buffer));
  hProc := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, FALSE, PID);
  if hProc <> 0 then
  begin
    GetModulebaseName(hProc, 0, buffer, sizeof(buffer));
    ProcessName := (string(buffer));
    CloseHandle(hProc);
  end
  else
    dwReturn := GetLastError;
  result := dwReturn;
end;



function GetProcessList(var ProcessList: TPIDList): DWORD;

  function GetOSVersionInfo(var Info: TOSVersionInfo): Boolean;
  begin
    FillChar(Info, SizeOf(TOSVersionInfo), 0);
    Info.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
    Result := GetVersionEx(TOSVersionInfo(Addr(Info)^));
    if (not Result) then
    begin
      FillChar(Info, SizeOf(TOSVersionInfo), 0);
      Info.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
      Result := GetVersionEx(TOSVersionInfo(Addr(Info)^));
      if (not Result) then
        Info.dwOSVersionInfoSize := 0;
    end;
  end;

var
  dwReturn     : DWORD;
  OS           : TOSVersionInfo;
  // EnumProcesses
  PidProcesses : PDWORD;
  PidWork      : PDWORD;
  BufferSize   : Cardinal;
  Needed       : DWORD;
  cntProcesses : Cardinal;
  i            : Cardinal;
  // CreateToolhelp32Snapshot
  hProcSnapShot: THandle;
  pe32         : TProcessEntry32;
  j            : Cardinal;

begin
  dwReturn := 0;
  // What OS are we running on?
  if GetOSVersionInfo(OS) then
  begin
    if (OS.dwPlatformId = VER_PLATFORM_WIN32_NT) and (OS.dwMajorVersion = 4) then
    // WinNT and higher
    begin
      Needed := 0;
      BufferSize := 1024;
      GetMem(PidProcesses, BufferSize);
      // make sure memory is allocated
      if Assigned(PidProcesses) then
      begin
        try
          // enumerate the processes
          if EnumProcesses(PidProcesses, BufferSize, Needed) then
          begin
            dwReturn := 0;
            cntProcesses := Needed div sizeof(DWORD) - 1;
            PidWork := PidProcesses;
            setlength(ProcessList, cntProcesses);
            // walk the processes
            for i := 0 to cntProcesses - 1 do
            begin
              ProcessList[i] := PidWork^;
              Inc(PidWork);
            end;
          end
          else // EnumProcesses = False
            dwReturn := GetLastError;
        finally
          // clean up no matter what happend
          FreeMem(PidProcesses, BufferSize);
        end;
      end
      else // GetMem = nil
        dwReturn := GetLastError;
    end
    // Win 9x and higher except WinNT
    else
    begin
      // make the snapshot
      hProcSnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
      if hProcSnapShot <> INVALID_HANDLE_VALUE then
      begin
        pe32.dwSize := sizeof(TProcessEntry32);
        j := 0;
        setlength(ProcessList, j + 1);
        if Process32First(hProcSnapShot, pe32) then
        begin
          // first process
          ProcessList[j] := pe32.th32ProcessID;
          // walk the processes
          while Process32Next(hProcSnapShot, pe32) do
          begin
            Inc(j);
            setlength(ProcessList, j + 1);
            ProcessList[j] := pe32.th32ProcessID;
          end;
        end
        else // Process32First = False
          dwReturn := GetLastError;
        CloseHandle(hProcSnapShot);
      end
      else // hSnapShot = INVALID_HANDLE_VALUE
        dwReturn := GetLastError;
    end;
  end;
  result := dwReturn;
end;

procedure SetProcessPriority(processname: string; priorityclass: DWORD);
var
  h: THandle;
begin
  IterateProcesses(procedure (pi: TProcInfo)
    begin
      try
        if comparetext(pi.name, processname)=0 then begin
          Debug.Log(pi.id.tostring);
          h := OpenProcess(PROCESS_SET_INFORMATION ,TRUE, pi.id);
          try
            if not SetPriorityClass(h, priorityclass) then begin
              Debug.Log('failed');
            end;
          finally
            CloseHandle(h);
          end;



        end;
      except
      end;
    end
  );
end;

end.
