unit Endian;
{$INLINE AUTO}
interface

uses
  typex, systemx, sysutils;

procedure EndianSwap(const p: PByte; const iLength: nativeint);inline;


implementation


procedure EndianSwap(const p: PByte; const iLength: nativeint);inline;
var
  a: array[0..32] of byte;
  t: nativeint;
begin
  if iLength> 32 then
    raise Exception.create('Endian Swap does not support this type');
    MoveMem32(@a[0], p, iLength);
  for t := 0 to iLength-1 do begin
    p[t] := a[iLength-(t+1)];
  end;
end;


end.
