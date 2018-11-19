unit z3_sysex_config;

interface

uses
  typex, systemx, endian, sysutils;

type
  TConfigValueTransferType = (stt7bitByte, stt8bitByte, sttUInt32, sttSInt32, stt7bitASCII, stt8bitASCII, sttSInt16, sttUInt16,sttSint64, sttUInt64);
  TConfigValueTreatmentType = (ttCheckbox, ttSpin, ttManualEntryDecimal, ttReadOnlyHex, ttReadOnlyDecimal);

  TSysexConfigRecord = record
    CCCode: string;
    CatCode: string;
    Desc: string;
    flags: ni;
    transfer_type: TConfigValueTransferType;
    treatment_type: TConfigValueTreatmentType;
    function FlagDebug: string;
  end;


function Get7bitsizeofvaluetype(vt: TConfigValueTransferType): ni;
function TransferTypeToNativeInt(vt: TConfigValueTransferType; data: array of byte): int64;
function NativeIntToTransferType(vt: TConfigValueTransferType; nat: int64): TArray<byte>;

implementation


function Get7bitsizeofvaluetype(vt: TConfigValueTransferType): ni;
begin
  case vt of
    stt7BitByte:  result := 1;
    stt8BitByte:  result := 2;
    sttUInt16: result := 3;
    sttSInt16: result := 3;
    sttUInt32: result := 5;
    sttSInt32: result := 5;
    sttSInt64: result := 10;
    sttUInt64: result := 10;
    stt7bitascii: result := 0;
    stt8bitascii: result := 0;
  else
    raise ECritical.create('did not understand size of TConfigValueTransferType '+inttostr(ord(vt)));
  end;
end;


function TransferTypeToNativeInt(vt: TConfigValueTransferType; data: array of byte): int64;
var
  b: byte;
  s: smallint;
  l: integer;
  ll: cardinal;
  s64: int64;
  u64: uint64;
  w: word;
begin
  case vt of
    stt7BitByte:  result := data[0];
    stt8BitByte:
    begin
      Convert7BitsTo8Bits(@data[0], @b, 2);
      result := b;
    end;
    sttSInt64:
    begin
      Convert7BitsTo8Bits(@data[0], @s64, 10);
      EndianSwap(@s64, sizeof(s64));
      result := s64;
    end;
    sttUInt64:
    begin
      Convert7BitsTo8Bits(@data[0], @u64, 10);
      EndianSwap(@u64, sizeof(u64));
      result := u64;
    end;
    sttUInt32:
    begin
      Convert7BitsTo8Bits(@data[0], @ll, 5);
      EndianSwap(@ll, sizeof(ll));
      result := ll;
    end;
    sttSInt32:
    begin
      Convert7BitsTo8Bits(@data[0], @l, 5);
      EndianSwap(@l, sizeof(l));
      result := l;
    end;
    stt7bitascii:
      result := 0;

    stt8bitascii:
      result := 0;
    sttSInt16:
    begin
      Convert7BitsTo8Bits(@data[0], @s, 3);
      EndianSwap(@s, sizeof(s));
      result := s;
    end;
    sttUInt16:
    begin
      Convert7BitsTo8Bits(@data[0], @w, 3);
      EndianSwap(@w, sizeof(w));
      result := w;
    end;
  else
    raise ECritical.create('did not understand size of TConfigValueTransferType');
  end;
end;


function NativeIntToTransferType(vt: TConfigValueTransferType; nat: int64): TArray<byte>;
var
  b: byte;
  s: smallint;
  l: integer;
  ll: cardinal;
begin
  case vt of
    stt7BitByte:  begin
      setlength(result, 1);
      result[0] := nat;
    end;
    stt8BitByte:
    begin
      setlength(result, 2);
      b := nat;
      Convert8BitsTo7Bits(@b, 1, @result[0], 2);
    end;
    sttSInt64:
    begin
      setlength(result, 10);
      l := nat;
      endianswap(@l, sizeof(l));
      Convert8BitsTo7Bits(@l, 8, @result[0], 10);
    end;
    sttUInt32:
    begin
      setlength(result, 5);
      ll := nat;
      endianswap(@ll, sizeof(ll));
      Convert8BitsTo7Bits(@ll, 4, @result[0], 5);

    end;
    sttSInt32:
    begin
      setlength(result, 5);
      l := nat;
      endianswap(@l, sizeof(l));
      Convert8BitsTo7Bits(@l, 4, @result[0], 5);
    end;
    stt7bitascii:
      setlength(result,0);

    stt8bitascii:
      setlength(result,0);
    sttSInt16:
    begin
      setlength(result, 3);
      s := nat;
      endianswap(@s, sizeof(s));
      Convert8BitsTo7Bits(@s, 2, @result[0], 3);    end;
    sttUInt16:
    begin
      setlength(result, 3);
      s := nat;
      endianswap(@s, sizeof(s));
      Convert8BitsTo7Bits(@s, 2, @result[0], 3);
    end;
  else
    raise ECritical.create('did not understand size of TConfigValueTransferType');
  end;
end;

{ TSysexConfigRecord }

function TSysexConfigRecord.FlagDebug: string;
begin
//  #define SF_ENGINEERING		1
//  #define SF_ADVANCED			2
//  #define SF_CRITICAL			4
//  #define SF_BETA				8
///  #define SF_NEVERSHOW		16
//  #define SF_WIFIONLY			32
//  #define SF_BTONLY			64

  result := '';
  if BitGet(@self.flags, 0) then begin
    result := result + 'En';
  end;

  if BitGet(@self.flags, 1) then begin
    result := result + 'Av';
  end;

  if BitGet(@self.flags, 2) then begin
    result := result + 'Cr';
  end;

  if BitGet(@self.flags, 3) then begin
    result := result + 'Be';
  end;

  if BitGet(@self.flags, 4) then begin
    result := result + 'Ns';
  end;

  if BitGet(@self.flags, 5) then begin
    result := result + 'Wf';
  end;

  if BitGet(@self.flags, 6) then begin
    result := result + 'Bt';
  end;








end;

end.
