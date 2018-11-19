unit Tools;

interface

uses
  systemx, system.ioutils, sysutils, typex;

function FindProgramFiles(sExe: string): string;
function FindTool(sExe: string; bExhaustive: boolean = false): string;
function FindMedia(sFile: string): string;

function FindRecursive(sPath, sFile: string; out sFullFile: string): boolean;



implementation

uses
  dir, dirfile;


function FindProgramFiles(sExe: string): string;
var
  sOut: string;
begin
{$IFDEF MSWINDOWS}
    try  if FindRecursive('c:\program files\', sExe, sOut) then exit(sOut); except  end;
    try  if FindRecursive('c:\program files (x86)\', sExe, sOut) then exit(sOut); except  end;
{$ENDIF}

  result := sExe;
end;


function FindRecursive(sPath, sFile: string; out sFullFile: string): boolean;
var
  sLook: string;
  fi: TFileInformation;
  t: ni;
  dir: TDirectory;
begin
  result := false;
  sLook := slash(sPath)+sFile;
  if TFile.Exists(sLook) then begin
    sFullFile := sLook;
    exit(true);
  end;

  dir := Tdirectory.create(sPath, sFile, 0,0, false, false, false);
  try
    for t:= 0 to dir.foldercount-1 do begin
      if FindRecursive(dir.folders[t].fullname, sFile, sFullFile) then
        exit(true);
    end;
  finally
    dir.free;
  end;

end;


function FindTool(sExe: string; bExhaustive: boolean): string;
var
  sOut: string;
begin
  try  result := slash(DLLPath)+sExe; if TFile.Exists(result) then exit;  except  end;
  try  result := slash(DLLPath)+'..\'+sExe; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\'+sExe; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\..\'+sExe; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\tools\'+sExe; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\'+sExe; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\tools\'+sExe; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\..\'+sExe; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\..\tools\'+sExe; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\..\..\'+sExe; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\..\..\tools\'+sExe; if TFile.Exists(result) then exit; except  end;


  try  if FindRecursive(slash(DLLPath)+'..\tools\', sExe, sOut) then exit(sOut); except  end;
  try  if FindRecursive(slash(DLLPath)+'..\..\tools\', sExe, sOut) then exit(sOut); except  end;
  try  if FindRecursive(slash(DLLPath)+'..\..\..\tools\', sExe, sOut) then exit(sOut); except  end;
  try  if FindRecursive(slash(DLLPath)+'..\..\..\..\tools\', sExe, sOut) then exit(sOut); except  end;
  if bExhaustive then begin
{$IFDEF MSWINDOWS}
    try  if FindRecursive('c:\program files\', sExe, sOut) then exit(sOut); except  end;
    try  if FindRecursive('c:\program files (x86)\', sExe, sOut) then exit(sOut); except  end;
{$ENDIF}
  end;



  result := sExe;

end;

function FindMedia(sFile: string): string;
begin
  try  result := slash(DLLPath)+sFile; if TFile.Exists(result) then exit;  except  end;
  try  result := slash(DLLPath)+'..\'+sFile; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\Media\'+sFile; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\art\'+sFile; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\'+sFile; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\Media\'+sFile; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\art\'+sFile; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\..\'+sFile; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\..\Media\'+sFile; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\..\art\'+sFile; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\..\..\'+sFile; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\..\..\Media\'+sFile; if TFile.Exists(result) then exit; except  end;
  try  result := slash(DLLPath)+'..\..\..\..\art\'+sFile; if TFile.Exists(result) then exit; except  end;

  raise ECritical.Create('could not find media '+sFile+' using FindMedia()');


end;


end.
