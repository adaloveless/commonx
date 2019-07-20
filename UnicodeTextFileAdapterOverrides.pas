unit UnicodeTextFileAdapterOverrides;

interface

uses
  UnicodeTextFileAdapter;


procedure AssignFile(out tf: TUCTextFile; sFileName: string);
procedure Reset(var tf: TUCTextFile);
procedure Close(var tf: TUCTextFile);
function ReadLn(var tf: TUCTextFile; out s: string): boolean;



implementation

procedure AssignFile(out tf: TUCTextFile; sFileName: string);
begin
  UnicodeTextFileAdapter.UCAssignFile(tf, sFileName);
end;

procedure Reset(var tf: TUCTextFile);
begin
  Reset(tf);
end;
procedure Close(var tf: TUCTextFile);
begin
  Close(tf);
end;

function ReadLn(var tf: TUCTextFile; out s: string): boolean;
begin
  result := ReadLn(tf, s)
end;


end.
