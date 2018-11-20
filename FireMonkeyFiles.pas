unit FireMonkeyFiles;

interface

uses
  classes,
{$IFDEF IOS}
  Macapi.CoreFoundation,
  Macapi.CoreServices,
{$ENDIF}
  sysutils, ioutils;

function GetWriteablePath: string;


procedure TestWrite();
function TestRead(): string;
function FindDeployedFile(sName: string): string;

implementation


//{$IFDEF FPC}
var
content : string;
//{$ENDIF}

//{$IFDEF FPC}
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

  s := TPath.GetHomePath+TPath.DirectorySeparatorChar+sName;
  if fileexists(s) then
    result := s;






end;

function MyFileName : string;
begin
  result := GetHomePath+'/Documents/poop.txt';
end;


//{$ENDIF}

procedure TestWrite();
var
  s: string;
  sl: TStringlist;
begin
  sl := TStringList.create;
  try
    sl.add('poop');
    //TFile.WriteAllText(TPath.GetHomePath() + TPath.DirectorySeparatorChar + 'Documents/sample.txt', sl.text);
    sl.SaveToFile(MyFileName);
  finally
    sl.free;
  end;
//  {$ENDIF}
end;

function TestRead(): string;
var
  sl: TStringlist;
begin
  sl := TStringlist.create;
  try
    result := 'empty';
    if fileexists(MyFileNAme) then begin
      sl.LoadFromFile(MyFileName);
      result := sl.text;
    end;
  finally
    sl.free;
  end;


end;

end.
