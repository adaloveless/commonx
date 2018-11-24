unit helpers_winsock;

interface

uses
  sockfix;
function BetterWaitForDAta(s: TBaseSocket; Timeout: cardinal): boolean;


implementation


function BetterWaitForDAta(s: TBaseSocket; Timeout: cardinal): boolean;
var
  ReadReady, ExceptFlag: Boolean;
  DataByte: Byte;
begin
  Result := False;
  // Select also returns True when connection is broken.
  if s.Select(@ReadReady, nil, @ExceptFlag, TimeOut) then
    Result := ReadReady;// and not ExceptFlag and
//      (s.PeekBuf(DataByte, sizeof(DataByte)) = 1);

end;

end.
