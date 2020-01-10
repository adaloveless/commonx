unit nVidiaTools;

interface

uses
  orderlyinit, typex, exe, windows, systemx, stringx, sysutils, betterobject, classes, commandprocessor;

function GetGPUList: Iholder<TStringList>;
function getGPUCount: ni;

function GetNVidiaSMIExe:string;

implementation

function getGPUCount: ni;
begin
  result := GetGPUList.o.Count;
end;


function GetGPUList: Iholder<TStringList>;
begin
  result := stringtostringlistH('');
  var sexe := GetNVIDIASmiExe;
  if sexe <> '' then begin
    result.o.text := exe.RunExeAndCapture(sexe,'-L', extractfilepath(sexe));
  end;
  for var t := result.o.Count-1 downto 0 do begin
    if trim(result.o[t]) = '' then
      result.o.delete(t);
  end;
end;

function GetNVidiaSMIExe:string;
begin
  var sTry: string :=  GetSystemDir(false);
  sTry := zcopy(sTry, 0, 1);
  stry := sTry+':\Program Files\NVIDIA Corporation\NVSMI\Nvidia-smi.exe';
  if fileexists(sTry) then
    exit(sTry)
  else
    sTry := sTry+':\Program Files (x86)\NVIDIA Corporation\NVSMI\Nvidia-smi.exe';

  if fileexists(sTry) then
    exit(sTry)
  else
    exit('');


end;


procedure oinit;
begin
  BGCmd.SetResourceLimit('GPUExpense', getGPUCount);
end;
initialization

init.RegisterProcs('nVidiaTools', oinit, nil, 'CommandProcessor');

end.
