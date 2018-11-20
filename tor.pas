unit tor;

interface

uses
  orderlyinit, exe, tools, sysutils, windows, stringx, systemx;

implementation

var
  hTor: THandle;

function FindTor: string;
begin
  result := dllpath+'tor\tor.exe';
end;


procedure oinit;
begin
  hTor := invalid_handle_value;
  if fileexists(findtor) then
    hTor := RunProgram(findtor, '-f '+quote(dllpath+'tor\torrc'), dllpath+'tor\', true, false, false).pi.hProcess;
end;

procedure ofinal;
begin
  if htor <> INVALID_HANDLE_VALUE then
    CloseHandle(htor);

end;


initialization
  init.RegisterProcs('tor', oinit, ofinal, 'exe');


end.
