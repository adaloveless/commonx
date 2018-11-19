unit NetBytes;
{x$INLINE AUTO}
{$DEFINE IL}
interface

uses
  system.SysUtils,
  Endian, systemx;


function AsWord(const p: pbyte): word;{$IFDEF IL}inline;{$ENDIF}
function AsSmallint(const p: pbyte): smallint;{$IFDEF IL}inline;{$ENDIF}
function AsCardinal(const p: pbyte): cardinal;{$IFDEF IL}inline;{$ENDIF}
function AsInteger(const p: pbyte): integer;{$IFDEF IL}inline;{$ENDIF}
function As3ByteUnsignedInt(const p: pbyte): cardinal;{$IFDEF IL}inline;{$ENDIF}
function As3ByteSignedInt(const p: pbyte): integer;{$IFDEF IL}inline;{$ENDIF}
function AsInt64(const p: pbyte): int64;{$IFDEF IL}inline;{$ENDIF}
function As6ByteUnsigned(const p: pbyte): int64;{$IFDEF IL}inline;{$ENDIF}

procedure FromWord(const p: pbyte; const value: word);{$IFDEF IL}inline;{$ENDIF}
procedure FromSmallint(const p: pbyte; const value: smallint);{$IFDEF IL}inline;{$ENDIF}
procedure FromCardinal(const p: pbyte; const value: cardinal);{$IFDEF IL}inline;{$ENDIF}
procedure FromInteger(const p: pbyte; const value: smallint);{$IFDEF IL}inline;{$ENDIF}
procedure From3ByteUnsignedInt(const p: pbyte; const value: cardinal);{$IFDEF IL}inline;{$ENDIF}
procedure From3ByteSignedInt(const p: pbyte; const value: integer);{$IFDEF IL}inline;{$ENDIF}
procedure FromInt64(const p: pbyte; const value: int64);{$IFDEF IL}inline;{$ENDIF}
procedure From6ByteUnsigned(const p: pbyte; const value: int64);{$IFDEF IL}inline;{$ENDIF}



implementation


function AsWord(const p: pbyte): word;
begin
  movemem32(@result, p, sizeof(result));
  EndianSwap(Pbyte(@result), sizeof(result));
end;

function AsSmallint(const p: pbyte): smallint;
begin
  movemem32(@result, p, sizeof(result));
  EndianSwap(Pbyte(@result), sizeof(result));
end;
function AsCardinal(const p: pbyte): cardinal;
begin
  movemem32(@result, p, sizeof(result));
  EndianSwap(Pbyte(@result), sizeof(result));
end;
function AsInteger(const p: pbyte): integer;
begin
  movemem32(@result, p, sizeof(result));
  EndianSwap(Pbyte(@result), sizeof(result));
end;
function As3ByteUnsignedInt(const p: pbyte): cardinal;
begin
  result := 0;
  movemem32(@result, p, 3);
  EndianSwap(Pbyte(@result), 3);

end;


function As6ByteUnsigned(const p: pbyte): int64;
begin
  result := 0;
  movemem32(@result, p, 6);
  EndianSwap(Pbyte(@result), 6);


end;


function As3ByteSignedInt(const p: pbyte): integer;
begin
  result := 0;
  movemem32(@result, p, 3);
  EndianSwap(Pbyte(@result), 3);

end;
function AsInt64(const p: pbyte): int64;
begin
  movemem32(@result, p, sizeof(result));
  EndianSwap(Pbyte(@result), sizeof(result));
end;

procedure FromWord(const p: pbyte; const value: word);
begin
  movemem32(p, @value, sizeof(value));
  EndianSwap(p, sizeof(value));
end;

procedure FromSmallint(const p: pbyte; const value: smallint);
begin
  movemem32(p, @value, sizeof(value));
  EndianSwap(p, sizeof(value));
end;
procedure FromCardinal(const p: pbyte; const value: cardinal);
begin
  movemem32(p, @value, sizeof(value));
  EndianSwap(p, sizeof(value));
end;
procedure FromInteger(const p: pbyte; const value: smallint);
begin
  movemem32(p, @value, sizeof(value));
  EndianSwap(p, sizeof(value));
end;
procedure From3ByteUnsignedInt(const p: pbyte; const value: cardinal);
begin
  movemem32(p, @value, 3);
  EndianSwap(p, 3);
end;

procedure From6ByteUnsigned(const p: pbyte; const value: int64);
begin
  movemem32(p, @value, 6);
  EndianSwap(p, 6);
end;



procedure From3ByteSignedInt(const p: pbyte; const value: integer);
begin
  movemem32(p, @value, 3);
  EndianSwap(p, 3);
end;

procedure FromInt64(const p: pbyte; const value: int64);
begin
  movemem32(p, @value, sizeof(value));
  EndianSwap(p, sizeof(value));
end;


end.
