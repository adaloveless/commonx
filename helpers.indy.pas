unit helpers.indy;

interface

uses
  idglobal, types, classes, sysutils, typex, idiohandlersocket;


function TBytesToIDBytes(b: TBytes): TIDBytes;
function AnsiStringToIDBytes(a: ansistring): TIDBytes;
function idsocket_GuaranteeRead(idsocket: TIdIOHandlerSocket; iCount: ni): TidBytes;


implementation


function TBytesToIDBytes(b: TBytes): TIDBytes;
var
  t: ni;
begin
  setlength(result, length(b));
  for t := low(b) to high(b) do
    result[t] := b[t];




end;
function AnsiStringToIDBytes(a: ansistring): TIDBytes;
{$IFDEF MSWINDOWS}
var
  t: ni;
begin
  setlength(result, length(a));
  for t := 0  to length(a)-1 do
    result[t] := ord(a[STRZ+t]);

end;
{$ELSE}
var
  t: ni;
begin
  setlength(result, length(a));
  for t := 0  to length(a)-1 do
    result[t] := a.bytes[STRZ+t];
end;
{$ENDIF}

function idsocket_GuaranteeRead(idsocket: TIdIOHandlerSocket; iCount: ni): TidBytes;
var
  iToGo: ni;
begin
  setlength(result, 0);
  iToGo := iCount;
  while iToGo > 0 do begin
    idsocket.ReadBytes(result, iToGo);
    iTogo := length(result) - iCount;
  end;


end;




end.
