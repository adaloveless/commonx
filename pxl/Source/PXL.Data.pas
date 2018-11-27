unit PXL.Data;
{
  This file is part of Asphyre Framework, also known as Pascal eXtended Library (PXL).
  Copyright (c) 2000 - 2015  Yuriy Kotsarenko

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General
  Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
  details.
}
{< Utility routines for working with binary data including compression, encryption and checksum calculation. }
interface

{$INCLUDE PXL.Config.inc}

type
  { The level used for data compression. }
  TCompressionLevel = (
    { Default compression ratio and performance. }
    Default,

    { Optimize for performance at cost of compression ratio. }
    BestSpeed,

    { Optimize for better compression ratio at cost of performance. }
    BestCompression);

  { Pointer to @link(TCipherBlock). }
  PCipherBlock = ^TCipherBlock;

  { Initialization block used for XTEA cipher. }
  TCipherBlock = record
    { Defines all values randomly. }
    procedure Init;

    { Overwrites all values back to zero. }
    procedure Burn;

    case Integer of
      0: { Block values represented as 8-bit unsigned integers. }
         (ByteValues: array[0..7] of Byte);
      1: { Block values represented as 16-bit unsigned integers. }
         (WordValues: array[0..3] of Word);
      2: { Block values represented as 32-bit unsigned integers. }
         (Values: array[0..1] of LongWord);
      3: { Block values represented as a single 64-bit unsigned integer. }
         (LongValue: UInt64);
  end;

  { Pointer to @link(TCipherKey). }
  PCipherKey = ^TCipherKey;

  { Security password used for XTEA cipher. }
  TCipherKey = record
    { Defines all values randomly. }
    procedure Init;

    { Overwrites all values back to zero. }
    procedure Burn;

    case Integer of
      0: { Key values represented as 8-bit unsigned integers. }
         (ByteValues: array[0..15] of Byte);
      1: { Key values represented as 16-bit unsigned integers. }
         (WordValues: array[0..7] of Word);
      2: { Key values represented as 32-bit unsigned integers. }
         (Values: array[0..3] of LongWord);
      3: { Key values represented as 64-bit unsigned integers. }
         (LongValues: array[0..1] of UInt64);
  end;

  { Pointer to @link(TMD5Checksum). }
  PMD5Checksum = ^TMD5Checksum;

  { MD5 checksum value. }
  TMD5Checksum = record
    { @exclude } class operator Equal(const Value1, Value2: TMD5Checksum): Boolean; inline;
    { @exclude } class operator NotEqual(const Value1, Value2: TMD5Checksum): Boolean; inline;

    case Integer of
      0: { Checksum values represented as 8-bit unsigned integers. }
         (ByteValues: array[0..15] of Byte);
      1: { Checksum values represented as 16-bit unsigned integers. }
         (WordValues: array[0..7] of Word);
      2: { Checksum values represented as 32-bit unsigned integers. }
         (Values: array[0..3] of LongWord);
      3: { Checksum values represented as 64-bit unsigned integers. }
         (LongValues: array[0..1] of UInt64);
  end;

  { Standard string type used for base-64 encoding. }
  TBase64String = {$IFDEF STANDARD_STRINGS_ONLY} string {$else} AnsiString {$endif};

{ Compresses data block from the source buffer to destination buffer. The memory for both buffers must be allocated.
  @italic(MaxDestSize) can be used to specify the maximum data length that can be saved at the destination buffer to
  prevent overflow. }
function CompressData(const Source, Dest: Pointer; const SourceSize, MaxDestSize: Cardinal;
  const Compression: TCompressionLevel = TCompressionLevel.Default): Cardinal;

{ Decompresses data block from the source buffer to destination buffer. The memory for both buffers must be allocated.
  @italic(MaxDestSize) can be used to specify the maximum data length that can be saved at the destination buffer to
  prevent overflow. }
function DecompressData(const Source, Dest: Pointer; const SourceSize, MaxDestSize: Cardinal): Cardinal;

{ Calculates CRC32 checksum from the specified data. This checkum can be used in integrity tests to verify that the
  data has not been modified. }
function ChecksumCRC32(const Source: Pointer; const SourceSize: Cardinal): Cardinal; inline;

{ Encrypts data using 128-bit XTEA cipher in CBC chaining mode and residual block termination (in case the data buffer
  is not multiple of 8 bytes). }
procedure EncryptData(const Source, Dest: Pointer; const SourceSize: Integer; const InitVector: TCipherBlock;
  const Key: TCipherKey);

{ Decrypts data using 128-bit XTEA cipher in CBC chaining mode and residual block termination (in case the data buffer
  is not multiple of 8 bytes). }
procedure DecryptData(const Source, Dest: Pointer; const SourceSize: Integer; const InitVector: TCipherBlock;
  const Key: TCipherKey);

{ Calculates and returns 128-bit MD5 checksum of the given data block. This checksum can be used as a signature to
  verify that the data has not been altered in any way. }
function ChecksumMD5(const Source: Pointer; const SourceSize: Cardinal): TMD5Checksum;

{ Encodes the binary source into base-64 readable text. This effectively increases the size by 4/3 (rounded up).
  If the source is not divisible by 3, it will be padded with zeros. }
function Base64String(const Source: Pointer; const SourceSize: Integer): TBase64String;

{ Decodes the base-64 readable text back into the binary format. }
function Base64Binary(const Source: TBase64String; const Dest: Pointer): Integer;

implementation

uses
{$IFDEF FPC}
  paszlib, crc, md5, base64, Classes,
{$ELSE}
  System.ZLib, System.Hash, System.NetEncoding,
{$ENDIF}

  SysUtils;

{$REGION 'Compression'}

function CompressData(const Source, Dest: Pointer; const SourceSize, MaxDestSize: Cardinal;
  const Compression: TCompressionLevel): Cardinal;
var
  OutSize: Cardinal;
  Level: Integer;
begin
  OutSize := MaxDestSize;

  case Compression of
    TCompressionLevel.BestSpeed:
      Level := Z_BEST_SPEED;

    TCompressionLevel.BestCompression:
      Level := Z_BEST_COMPRESSION;

    else
      Level := Z_DEFAULT_COMPRESSION;
  end;

  if compress2(Dest, OutSize, Source, SourceSize, Level) = Z_OK then
    Result := OutSize
  else
    Result := 0;
end;

function DecompressData(const Source, Dest: Pointer; const SourceSize, MaxDestSize: Cardinal): Cardinal;
var
  OutSize: Cardinal;
begin
  OutSize := MaxDestSize;

  if uncompress(Dest, OutSize, Source, SourceSize) = Z_OK then
    Result := OutSize
  else
    Result := 0;
end;

{$ENDREGION}
{$REGION 'CRC32 Checksum'}

function ChecksumCRC32(const Source: Pointer; const SourceSize: Cardinal): Cardinal;
begin
  Result := crc32(0, Source, SourceSize);
end;

{$ENDREGION}
{$REGION 'Encryption'}

const
  XTEACipherDelta = $9E3779B9;
  XTEACipherDelta2 = $C6EF3720;

procedure TCipherBlock.Init;
var
  I: Integer;
begin
  for I := 0 to High(Self.WordValues) do
    Self.WordValues[I] := Cardinal(Random($10000));
end;

procedure TCipherBlock.Burn;
begin
  Self.LongValue := 0;
end;

procedure TCipherKey.Init;
var
  I: Integer;
begin
  for I := 0 to High(Self.WordValues) do
    Self.WordValues[I] := Cardinal(Random($10000));
end;

procedure TCipherKey.Burn;
var
  I: Integer;
begin
  for I := 0 to High(Self.Values) do
    Self.Values[I] := 0;
end;

procedure EncodeEntryXTEA(const Source: TCipherBlock; out Dest: TCipherBlock; const Key: TCipherKey);
var
  Index: Integer;
  SumValues, Value1, Value2: Cardinal;
begin
  SumValues := 0;
  Value1 := Source.Values[0];
  Value2 := Source.Values[1];

  for Index := 0 to 31 do
  begin
    Inc(Value1, (((Value2 shl 4) xor (Value2 shr 5)) + Value2) xor (SumValues + Key.Values[SumValues and 3]));
    Inc(SumValues, XTEACipherDelta);
    Inc(Value2, (((Value1 shl 4) xor (Value1 shr 5)) + Value1) xor (SumValues + Key.Values[(SumValues shr 11) and 3]));
  end;

  Dest.Values[0] := Value1;
  Dest.Values[1] := Value2;
end;

procedure DecodeEntryXTEA(const Source: TCipherBlock; out Dest: TCipherBlock; const Key: TCipherKey);
var
  Index: Integer;
  SumValues, Value1, Value2: Cardinal;
begin
  SumValues := XTEACipherDelta2;
  Value1 := Source.Values[0];
  Value2 := Source.Values[1];

  for Index := 0 to 31 do
  begin
    Dec(Value2, (((Value1 shl 4) xor (Value1 shr 5)) + Value1) xor (SumValues + Key.Values[(SumValues shr 11) and 3]));
    Dec(SumValues, XTEACipherDelta);
    Dec(Value1, (((Value2 shl 4) xor (Value2 shr 5)) + Value2) xor (SumValues + Key.Values[SumValues and 3]));
  end;

  Dest.Values[0] := Value1;
  Dest.Values[1] := Value2;
end;

procedure EncryptData(const Source, Dest: Pointer; const SourceSize: Integer; const InitVector: TCipherBlock;
  const Key: TCipherKey);
var
  Index: Integer;
  SourceBlock, DestBlock: PCipherBlock;
  TempBlock, LastBlock: TCipherBlock;
begin
  // Apply CBC mode in the block cipher.
  Move(InitVector, LastBlock, SizeOf(TCipherBlock));

  SourceBlock := Source;
  DestBlock := Dest;

  for Index := 0 to (SourceSize div 8) - 1 do
  begin
    TempBlock.Values[0] := SourceBlock.Values[0] xor LastBlock.Values[0];
    TempBlock.Values[1] := SourceBlock.Values[1] xor LastBlock.Values[1];

    EncodeEntryXTEA(TempBlock, LastBlock, Key);

    DestBlock.Values[0] := LastBlock.Values[0];
    DestBlock.Values[1] := LastBlock.Values[1];

    Inc(SourceBlock);
    Inc(DestBlock);
  end;

  // Residual block termination.
  if SourceSize mod 8 > 0 then
  begin
    // Use encrypted IV, if message is too small.
    if SourceSize < 8 then
      EncodeEntryXTEA(InitVector, LastBlock, Key);

    // Encrypt last block again.
    EncodeEntryXTEA(LastBlock, LastBlock, Key);

    // Fill the auxiliary block with remaining bytes.
    TempBlock.Values[0] := 0;
    TempBlock.Values[1] := 0;
    Move(SourceBlock^, TempBlock, SourceSize mod 8);

    // Encrypt the remaining bytes.
    TempBlock.Values[0] := TempBlock.Values[0] xor LastBlock.Values[0];
    TempBlock.Values[1] := TempBlock.Values[1] xor LastBlock.Values[1];

    // Write the remaining bytes to destination.
    Move(TempBlock, DestBlock^, SourceSize mod 8);
  end;
end;

procedure DecryptData(const Source, Dest: Pointer; const SourceSize: Integer; const InitVector: TCipherBlock;
  const Key: TCipherKey);
var
  Index: Integer;
  SourceBlock, DestBlock: PCipherBlock;
  TempBlock, LastBlock: TCipherBlock;
begin
  // Apply CBC mode in block cipher.
  Move(InitVector, LastBlock, SizeOf(TCipherBlock));

  SourceBlock := Source;
  DestBlock := Dest;

  for Index := 0 to (SourceSize div 8) - 1 do
  begin
    DecodeEntryXTEA(SourceBlock^, TempBlock, Key);

    TempBlock.Values[0] := TempBlock.Values[0] xor LastBlock.Values[0];
    TempBlock.Values[1] := TempBlock.Values[1] xor LastBlock.Values[1];

    LastBlock.Values[0] := SourceBlock.Values[0];
    LastBlock.Values[1] := SourceBlock.Values[1];

    DestBlock.Values[0] := TempBlock.Values[0];
    DestBlock.Values[1] := TempBlock.Values[1];

    Inc(SourceBlock);
    Inc(DestBlock);
  end;

  // Residual block termination.
  if SourceSize mod 8 > 0 then
  begin
    // Use encrypted IV, if message is too small.
    if SourceSize < 8 then
      EncodeEntryXTEA(InitVector, LastBlock, Key);

    // Encrypt last block again.
    EncodeEntryXTEA(LastBlock, LastBlock, Key);

    // Fill the auxiliary block with remaining bytes.
    TempBlock.Values[0] := 0;
    TempBlock.Values[1] := 0;
    Move(SourceBlock^, TempBlock, SourceSize mod 8);

    // Decrypt the remaining bytes.
    TempBlock.Values[0] := TempBlock.Values[0] xor LastBlock.Values[0];
    TempBlock.Values[1] := TempBlock.Values[1] xor LastBlock.Values[1];

    // Write the remaining bytes to destination.
    Move(TempBlock, DestBlock^, SourceSize mod 8);
  end;
end;

{$ENDREGION}
{$REGION 'MD5 Checksum'}

class operator TMD5Checksum.Equal(const Value1, Value2: TMD5Checksum): Boolean;
begin
  Result := CompareMem(@Value1, @Value2, SizeOf(TMD5Checksum));
end;

class operator TMD5Checksum.NotEqual(const Value1, Value2: TMD5Checksum): Boolean;
begin
  Result := not (Value1 = Value2);
end;

function ChecksumMD5(const Source: Pointer; const SourceSize: Cardinal): TMD5Checksum;
{$IFDEF FPC}
var
  Context: TMDContext;
  Digest: TMDDigest;
begin
  MDInit(Context, MD_VERSION_5);
  MDUpdate(Context, Source^, SourceSize);
  MDFinal(Context, Digest);

  Move(Digest, Result, SizeOf(TMD5Checksum));
end;
{$ELSE}
var
  Context: THashMD5;
  Digest: TBytes;
begin
  Context := THashMD5.Create;
  Context.Update(Source^, SourceSize);

  Digest := Context.HashAsBytes;
  Move(Digest[0], Result, SizeOf(TMD5Checksum));
end;
{$ENDIF}

{$ENDREGION}
{$REGION 'Base64'}

function Base64String(const Source: Pointer; const SourceSize: Integer): TBase64String;
{$IFDEF FPC}
var
  Stream: TMemoryStream;
  Encoder: TBase64EncodingStream;
  I: Integer;
begin
  if (SourceSize <= 0) or (Source = nil) then
    Exit('');
  try
    Stream := TMemoryStream.Create;
    try
      Encoder := TBase64EncodingStream.Create(Stream);
      try
        Encoder.WriteBuffer(Source^, SourceSize);
      finally
        Encoder.Free;
      end;

      SetLength(Result, Stream.Size);

      for I := 0 to Stream.Size - 1 do
        Result[I + 1] := PAnsiChar(PtrUInt(Stream.Memory) + Cardinal(I))^;
    finally
      Stream.Free;
    end;
  except
    Exit('');
  end;
end;
{$ELSE}
var
  Encoding: TBase64Encoding;
begin
  if (SourceSize <= 0) or (Source = nil) then
    Exit(string.Empty);

  Encoding := TBase64Encoding.Create;
  try
    Result := Encoding.EncodeBytesToString(Source, SourceSize);
  finally
    Encoding.Free;
  end;
end;
{$ENDIF}

function Base64Binary(const Source: TBase64String; const Dest: Pointer): Integer;
{$IFDEF FPC}
const
  BlockSize = 1024;
var
  Stream: TStringStream;
  Decoder: TBase64DecodingStream;
  BytesRead: Integer;
begin
  if (Length(Source) <= 0) or (Dest = nil) then
    Exit(0);
  try
    Stream := TStringStream.Create(Source);
    try
      Decoder := TBase64DecodingStream.Create(Stream, bdmMIME);
      try
        Result := 0;

        while not Decoder.EOF do
        begin
          BytesRead := Decoder.Read(Pointer(PtrUInt(Dest) + Cardinal(Result))^, BlockSize);
          if BytesRead <= 0 then
            Break;

          Inc(Result, BytesRead);
        end;
      finally
        Decoder.Free;
      end;
    finally
      Stream.Free;
    end;
  except
    Exit(-1);
  end;
end;
{$ELSE}
var
  Encoding: TBase64Encoding;
  Values: TBytes;
begin
  if (Length(Source) <= 0) or (Dest = nil) then
    Exit(0);

  Encoding := TBase64Encoding.Create;
  try
    Values := Encoding.DecodeStringToBytes(Source);
  finally
    Encoding.Free;
  end;

  Result := Length(Values);
  if Result > 0 then
    Move(Values[0], Dest^, Result);
end;
{$ENDIF}

{$ENDREGION}

end.
