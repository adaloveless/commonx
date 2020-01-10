unit tsc;

interface

uses
  systemx, exe, sysutils, sharedobject;


function TStoJS(sTSFile: string): string;
function AutoCompile(sTSorJSFile: string): string;

function TSCPath: string;
function TSCEXE: string;


implementation

var
  sectCompile: TCLXCriticalSection;

function TSCPath: string;
begin
  result := dllpath+'CGI\TypeScript\3.0\';
end;

function TSCEXE: string;
begin
  result := TSCPath+'tsc.exe';
end;

function TStoJS(sTSFile: string): string;
var
  c: Tcmd_RunExe;
begin
  c := nil;
  try
    c := Tcmd_RunExe.create;

    c.Prog := TSCEXE;
    c.Params := sTSFile;
    c.CaptureConsoleoutput := true;
    c.start;
    c.WaitFor;
    result := c.ConsoleOutput;
  finally
    c.free;
  end;

end;

function AutoCompile(sTSorJSFile: string): string;
var
  sJSFile: string;
  sTSFile: string;
begin
  sJSfile := changefileext(sTSorJSFile, '.js');
  sTSfile := changefileext(sTSorJSFile, '.ts');
  if not fileexists(sTSFile) then begin
    //if js exists then use the JS file
    if fileexists(sJSFile) then
      result := ''
    else
      result := sTSorJSFile+' not found.';
  end else begin
    //if the TS is newer than the JS then compile it
    if (not fileexists(sJsFile))
    or (FileAge(sTSFile) > FileAge(sJSfile)) then begin
      ecs(sectCompile);
      try
        TStoJS(sTSfile);
      finally
        lcs(sectCompile);
      end;
    end;
  end;

end;

initialization

ics(sectCompile);

finalization

dcs(sectCompile);


end.
