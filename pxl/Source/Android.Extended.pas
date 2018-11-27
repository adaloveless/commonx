unit Android.Extended;

{
  Copyright (C) 1988-1991 Apple Computer, Inc. All rights reserved.

  Machine-independent I/O routines for IEEE floating-point numbers.

  NaN's and infinities are converted to HUGE_VAL or HUGE, which happens to be infinity on IEEE machines. Unfortunately,
  it is impossible to preserve NaN's in a machine-independent way. Infinities are, however, preserved on IEEE machines.

  These routines have been tested on the following machines:
     Apple Macintosh, MPW 3.1 C compiler
     Apple Macintosh, THINK C compiler
     Silicon Graphics IRIS, MIPS compiler
     Cray X/MP and Y/MP
     Digital Equipment VAX

  Implemented by Malcolm Slaney and Ken Turkowski.

  Malcolm Slaney contributions during 1988-1990 include big- and little-endian file I/O, conversion to and from
  Motorola's extended 80-bit floating-point format, and conversions to and from IEEE single-precision floating-point
  format.

  In 1991, Ken Turkowski implemented the conversions to and from IEEE double-precision format, added more precision to
  the extended conversions, and accommodated conversions involving +/- infinity, NaN's, and denormalized numbers.
 }

interface

{$INCLUDE Android.Config.inc}
{$INCLUDE Android.LibDefs.inc}

procedure ConvertToIeeeExtended(num: Double; bytes: PByte);
function ConvertFromIeeeExtended(bytes: PByte): Double;

implementation

function frexp(Value: Double; exp: PLongInt): Double; cdecl; external libm Name 'frexp';
function ldexp(Value: Double; param: LongInt): Double; cdecl; external libm Name 'ldexp';
function floor(Value: Double): Double; cdecl; external libm Name 'floor';

function FloatToUnsigned(f: Double): LongWord;
begin
  Result := LongWord(LongInt(Trunc(f - 2147483648.0)) + 2147483647) + 1;
end;

function UnsignedToFloat(u: LongWord): Double;
begin
  Result := LongInt(u - 2147483647 - 1) + Double(2147483648.0);
end;

procedure ConvertToIeeeExtended(num: Double; bytes: PByte);
var
  sign, expon: LongInt;
  fMant, fsMant: Double;
  hiMant, loMant: LongWord;
begin
  if num < 0 then
  begin
    sign := $8000;
    num := -num;
  end
  else
    sign := 0;

  if num = 0 then
  begin
    expon := 0;
    hiMant := 0;
    loMant := 0;
  end
  else
  begin
    fMant := frexp(num, @expon);

    if (expon > 16384) or not (fMant < 1) then { Infinity or NaN }
    begin
      expon := sign or $7FFF;
      hiMant := 0;
      loMant := 0;
    end
    else
    begin
      Inc(expon, 16382);
      if expon < 0 then { denormalized }
      begin
        fMant := ldexp(fMant, expon);
        expon := 0;
      end;

      expon := expon or sign;
      fMant := ldexp(fMant, 32);
      fsMant := floor(fMant);
      hiMant := FloatToUnsigned(fsMant);
      fMant := ldexp(fMant - fsMant, 32);
      fsMant := floor(fMant);
      loMant := FloatToUnsigned(fsMant);
    end;
  end;

  bytes[9] := expon shr 8;
  bytes[8] := expon and $FF;
  bytes[7] := hiMant shr 24;
  bytes[6] := hiMant shr 16;
  bytes[5] := hiMant shr 8;
  bytes[4] := hiMant;
  bytes[3] := loMant shr 24;
  bytes[2] := loMant shr 16;
  bytes[1] := loMant shr 8;
  bytes[0] := loMant;
end;

function ConvertFromIeeeExtended(bytes: PByte): Double;
var
  f: Double;
  expon: LongInt;
  hiMant, loMant: LongWord;
begin
  expon := (LongInt(bytes[9] and $7F) shl 8) or bytes[8];

  hiMant := (LongWord(bytes[7]) shl 24) or (LongWord(bytes[6]) shl 16) or (LongWord(bytes[5]) shl 8) or bytes[4];
  loMant := (LongWord(bytes[3]) shl 24) or (LongWord(bytes[2]) shl 16) or (LongWord(bytes[1]) shl 8) or bytes[0];

  if (expon = 0) and (hiMant = 0) and (loMant = 0) then
    f := 0.0
  else
  begin
    if expon = $7FFF then { Infinity or NaN }
      f := 1.7e308
    else
    begin
      Dec(expon, 16383);

      Dec(expon, 31);
      f := ldexp(UnsignedToFloat(hiMant), expon);

      Dec(expon, 32);
      f := f + ldexp(UnsignedToFloat(loMant), expon);
    end;
  end;

  if bytes[9] and $80 <> 0 then
    Result := -f
  else
    Result := f;
end;

end.
