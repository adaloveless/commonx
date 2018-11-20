unit win.Debug;

interface

uses
  windows;


procedure Log(s: string);

implementation

procedure Log(s: string);
begin
  //not implemented
  OutputDebugString(pchar(s));
end;

end.
