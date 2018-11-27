unit PXL.Classes;
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
{< Extensions and utility classes that extend functionality of streams and provide ways for reading assets. }
interface

{$INCLUDE PXL.Config.inc}

{$IFNDEF PASDOC}
  {$IF DEFINED(FPC) AND DEFINED(ANDROID)}
    {$DEFINE ANDROID_ASSETS}
  {$ENDIF}
{$ENDIF}

uses
{$IFDEF ANDROID_ASSETS}
  Android.AssetManager,
{$ENDIF}

  Classes, PXL.TypeDef, PXL.Types;

type
{$IFDEF ANDROID_ASSETS}
  EAssetManagerNotSpecified = class(EStreamError);
{$ENDIF}

  { Extensions to TStream class for reading and writing different values depending on platform. Although TStream in
    recent versions of FPC and Delphi introduced similar functions, this extension class provides a more comprehensive
    and unified set of functions that work across all platforms. }
  TStreamHelper = class helper for TStream
  public type
    { Value stored as unsigned 8-bit integer, but represented as unsigned 32-bit or 64-bit value depending on platform. }
    TStreamByte = SizeUInt;

    { Value stored as unsigned 16-bit integer, but represented as unsigned 32-bit or 64-bit value depending on platform. }
    TStreamWord = SizeUInt;

    { Value stored as unsigned 32-bit integer, but represented as unsigned 32-bit or 64-bit value depending on platform. }
    TStreamLongWord = {$IF SIZEOF(SizeUInt) >= 4} SizeUInt {$ELSE} LongWord {$ENDIF};

    { Value stored and represented as unsigned 64-bit integer. }
    TStreamUInt64 = UInt64;

    { Value stored as 8-bit signed integer, but represented as signed 32-bit or 64-bit value depending on platform. }
    TStreamShortInt = SizeInt;

    { Value stored as 16-bit signed integer, but represented as signed 32-bit or 64-bit value depending on platform. }
    TStreamSmallInt = SizeInt;

    { Value stored as 32-bit signed integer, but represented as signed 32-bit or 64-bit value depending on platform. }
    TStreamLongInt = {$IF SIZEOF(SizeInt) >= 4} SizeInt {$ELSE} LongInt {$ENDIF};

    { Value stored and represented as signed 64-bit integer. }
    TStreamInt64 = Int64;

    { Value stored and represented as 32-bit (single-precision) floating-point. }
    TStreamSingle = Single;

    { Value stored and represented as 64-bit (double-precision) floating-point. }
    TStreamDouble = Double;

    { Value stored as 8-bit unsigned integer, but represented as Boolean. }
    TStreamByteBool = Boolean;

    { Value stored as unsigned 8-bit integer, but represented as signed 32-bit or 64-bit index depending on platform. }
    TStreamByteIndex = SizeInt;

    { Value stored as unsigned 16-bit integer, but represented as signed 32-bit or 64-bit index depending on platform. }
    TStreamWordIndex = {$IF SIZEOF(SizeInt) >= 4} SizeInt {$ELSE} LongInt {$ENDIF};
  public
    { Saves 8-bit unsigned integer to the stream. If the value is outside of [0..255] range, it will be clamped. }
    procedure PutByte(const Value: TStreamByte); inline;

    { Loads 8-bit unsigned integer from the stream. }
    function GetByte: TStreamByte; inline;

    { Saves 16-bit unsigned integer to the stream. If the value is outside of [0..65535] range, it will be clamped. }
    procedure PutWord(const Value: TStreamWord); inline;

    { Loads 16-bit unsigned integer value from the stream. }
    function GetWord: TStreamWord; inline;

    { Saves 32-bit unsigned integer to the stream. }
    procedure PutLongWord(const Value: TStreamLongWord); inline;

    { Loads 32-bit unsigned integer from the stream. }
    function GetLongWord: TStreamLongWord; inline;

    { Saves 64-bit unsigned integer to the stream. }
    procedure PutUInt64(const Value: TStreamUInt64); inline;

    { Loads 64-bit unsigned integer from the stream. }
    function GetUInt64: TStreamUInt64; inline;

    { Saves 8-bit signed integer to the stream. If the value is outside of [-128..127] range, it will be clamped. }
    procedure PutShortInt(const Value: TStreamShortInt); inline;

    { Loads 8-bit signed integer from the stream. }
    function GetShortInt: TStreamShortInt; inline;

    { Saves 16-bit signed integer to the stream. If the value is outside of [-32768..32767] range, it will be clamped. }
    procedure PutSmallInt(const Value: TStreamSmallInt); inline;

    { Loads 16-bit signed integer from the stream. }
    function GetSmallInt: TStreamSmallInt; inline;

    { Saves 32-bit signed integer to the stream. }
    procedure PutLongInt(const Value: TStreamLongInt); inline;

    { Loads 32-bit signed integer from the stream. }
    function GetLongInt: TStreamLongInt; inline;

    { Saves 64-bit signed integer to the stream. }
    procedure PutInt64(const Value: TStreamInt64); inline;

    { Loads 64-bit signed integer from the stream. }
    function GetInt64: TStreamInt64; inline;

    { Saves 32-bit floating-point value (single-precision) to the stream. }
    procedure PutSingle(const Value: TStreamSingle); inline;

    { Loads 32-bit floating-point value (single-precision) from the stream. }
    function GetSingle: TStreamSingle; inline;

    { Saves 64-bit floating-point value (double-precision) to the stream. }
    procedure PutDouble(const Value: TStreamDouble); inline;

    { Loads 64-bit floating-point value (double-precision) from the stream. }
    function GetDouble: TStreamDouble; inline;

    { Saves @bold(Boolean) value to the stream as 8-bit unsigned integer. A value of @False is saved as 255, while
      @True is saved as 0. }
    procedure PutByteBool(const Value: TStreamByteBool); inline;

    { Loads @bold(Boolean) value from the stream previously saved by @link(PutByteBool). The resulting value is treated
      as 8-bit unsigned integer with values of [0..127] considered as @True and values of [128..255] considered
      as @False. }
    function GetByteBool: TStreamByteBool; inline;

    { Saves 8-bit unsigned index to the stream. A value of -1 (and other negative values) is stored as 255. Positive
      numbers that are outside of [0..254] range will be clamped. }
    procedure PutByteIndex(const Value: TStreamByteIndex); inline;

    { Loads 8-bit unsigned index from the stream. The range of returned values is [0..254], the value of 255 is
      returned as -1. }
    function GetByteIndex: TStreamByteIndex; inline;

    { Saves 16-bit unsigned index to the stream. A value of -1 (and other negative values) is stored as 65535.
      Positive numbers that are outside of [0..65534] range will be clamped. }
    procedure PutWordIndex(const Value: TStreamWordIndex); inline;

    { Loads 16-bit unsigned index from the stream. The range of returned values is [0..65534], the value of 65535 is
      returned as -1. }
    function GetWordIndex: TStreamWordIndex; inline;

    { Saves 2D integer point to the stream. Each coordinate is saved as 8-bit unsigned integer. }
    procedure PutBytePoint2px(const Value: TPoint2px);

    { Loads 2D integer point from the stream. Each coordinate is loaded as 8-bit unsigned integer. }
    function GetBytePoint2px: TPoint2px;

    { Saves 2D integer point to the stream. Each coordinate is saved as 16-bit unsigned integer. }
    procedure PutWordPoint2px(const Value: TPoint2px);

    { Loads 2D integer point from the stream. Each coordinate is loaded as 16-bit unsigned integer.}
    function GetWordPoint2px: TPoint2px;

    { Saves 2D integer point to the stream. Each coordinate is saved as 32-bit signed integer. }
    procedure PutLongPoint2px(const Value: TPoint2px);

    { Loads 2D integer point from the stream. Each coordinate is loaded as 32-bit signed integer. }
    function GetLongPoint2px: TPoint2px;

    { Saves floating-point value as 8-bit signed byte to the stream using 1:3:4 fixed-point format with values outside
      of [-8..7.9375] range will be clamped. }
    procedure PutFloat34(const Value: Single);

    { Loads floating-point value as 8-bit signed byte from the stream using 1:3:4 fixed-point format. The possible
      values are in [-8..7.9375] range. }
    function GetFloat34: Single;

    { Saves floating-point value as 8-bit signed byte to the stream using 1:4:3 fixed-point format with values outside
      of [-16..15.875] range will be clamped. }
    procedure PutFloat43(const Value: Single);

    { Loads floating-point value as 8-bit signed byte from the stream using 1:4:3 fixed-point format. The possible
      values are in [-16..15.875] range.  }
    function GetFloat43: Single;

    { Saves two floating-point values as a single 8-bit unsigned byte to the stream with each value having 4-bits.
      Values outside of [-8..7] range will be clamped. }
    procedure PutFloats44(const Value1, Value2: Single);

    { Loads two floating-point values as a single 8-bit unsigned byte from the stream with each value having 4-bits.
      The possible values are in [-8..7] range. }
    procedure GetFloats44(out Value1, Value2: Single);

    { Saves two floating-point values as a single 8-bit unsigned byte to the stream with each value stored in
      fixed-point 1:2:1 format. Values outside of [-4..3.5] range will be clamped. }
    procedure PutFloats3311(const Value1, Value2: Single);

    { Loads two floating-point values as a single 8-bit unsigned byte from the stream with each value stored in
      fixed-point 1:2:1 format. The possible values are in [-4..3.5] range. }
    procedure GetFloats3311(out Value1, Value2: Single);

    { Saves Unicode string to the stream in UTF-8 encoding. The resulting UTF-8 string is limited to a maximum
      of 255 characters; therefore, for certain charsets the actual string is limited to either 127 or even 85
      characters in worst case. If MaxCount is not zero, the input string will be limited to the given number of
      characters. }
    procedure PutShortString(const Text: UniString; const MaxCount: Integer = 0);

    { Loads Unicode string from the stream in UTF-8 encoding previously saved by @link(PutShortString). }
    function GetShortString: UniString;

    { Saves Unicode string to the stream in UTF-8 encoding. The resulting UTF-8 string is limited to a maximum
      of 65535 characters; therefore, for certain charsets the actual string is limited to either 32767 or even 21845
      characters in worst case. If MaxCount is not zero, the input string will be limited to the given number of
      characters. }
    procedure PutMediumString(const Text: UniString; const MaxCount: Integer = 0);

    { Loads Unicode string from the stream in UTF-8 encoding previously saved by @link(PutMediumString). }
    function GetMediumString: UniString;

    { Stores Unicode string to the stream in UTF-8 encoding. }
    procedure PutLongString(const Text: UniString);

    { Loads Unicode string from the stream in UTF-8 encoding previously saved by @link(PutLongString). }
    function GetLongString: UniString;
  end;

  { Special stream type that is used to read assets on Android platform. This stream serves no purpose on other
    platforms. }
  TAssetStream = class(TStream)
  private
    FFileName: StdString;

  {$IFDEF ANDROID_ASSETS}
    FAsset: PAAsset;
    FFileSize: Int64;
  {$ENDIF}
  protected
    { @exclude } function GetSize: Int64; override;
  public
    { Creates instance of asset stream to read from the specified file, which must be located in /assets sub-folder of
      current Android package. }
    constructor Create(const AFileName: StdString);

    { @exclude } destructor Destroy; override;

    { @exclude } function Read(var Buffer; Count: LongInt): LongInt; override;
    { @exclude } function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    { Asset file name, which must be located in /assets sub-folder. }
    property FileName: StdString read FFilename;
{$IFDEF ANDROID_ASSETS}
  public
    property Asset: PAAsset read FAsset;
  private
    class var FAssetManager: PAAssetManager;
  public
    class property AssetManager: PAAssetManager read FAssetManager write FAssetManager;
{$ENDIF}
  end;

{ A quick method for replacing "\" with "/" and vice-versa depending on platform. This calls makes sure that the
  provided path uses correct path delimiter. }
function CrossFixFileName(const FileName: StdString): StdString;

implementation

uses
{$IFDEF DELPHI_NEXTGEN}
  SysUtils,
{$ENDIF}

  RTLConsts, PXL.Consts;

{$REGION 'Globals'}
{$IFDEF DELPHI_NEXTGEN}

const
  DefaultCodePage = 65001; // UTF-8

function StringToBytes(const Text: string): TBytes;
var
  ByteCount: Integer;
begin
  if Text.IsEmpty then
    Exit(nil);

  ByteCount := LocaleCharsFromUnicode(DefaultCodePage, 0, Pointer(Text), Length(Text), nil, 0, nil, nil);

  SetLength(Result, ByteCount);
  LocaleCharsFromUnicode(DefaultCodePage, 0, Pointer(Text), Length(Text), Pointer(Result), ByteCount, nil, nil);
end;

function BytesToString(const Bytes: TBytes): string;
var
  TextLength: Integer;
begin
  if Length(Bytes) < 1 then
    Exit(string.Empty);

  TextLength := UnicodeFromLocaleChars(DefaultCodePage, 0, Pointer(Bytes), Length(Bytes), nil, 0);
  if TextLength < 1 then
    Exit(string.Empty);

  SetLength(Result, TextLength);
  UnicodeFromLocaleChars(DefaultCodePage, 0, Pointer(Bytes), Length(Bytes), Pointer(Result), TextLength);
end;

{$ENDIF}

function CrossFixFileName(const FileName: StdString): StdString;
const
{$IFDEF MSWINDOWS}
  PrevChar = '/';
  NewChar = '\';
{$ELSE}
  PrevChar = '\';
  NewChar = '/';
{$ENDIF}
var
  I: Integer;
begin
  Result := FileName;
  UniqueString(Result);

  for I := 1 to Length(Result) do
    if Result[I] = PrevChar then
      Result[I] := NewChar;
end;

{$ENDREGION}
{$REGION 'TStreamHelper'}

procedure TStreamHelper.PutByte(const Value: TStreamByte);
var
  ByteValue: Byte;
begin
  if Value <= High(Byte) then
    ByteValue := Value
  else
    ByteValue := High(Byte);

  WriteBuffer(ByteValue, SizeOf(Byte));
end;

function TStreamHelper.GetByte: TStreamByte;
var
  ByteValue: Byte;
begin
  ReadBuffer(ByteValue, SizeOf(Byte));
  Result := ByteValue;
end;

procedure TStreamHelper.PutWord(const Value: TStreamWord);
{$IF SIZEOF(TStream.TStreamWord) > 2}
var
  WordValue: Word;
{$ENDIF}
begin
{$IF SIZEOF(TStream.TStreamWord) > 2}
  if Value <= High(Word) then
    WordValue := Value
  else
    WordValue := High(Word);

  WriteBuffer(WordValue, SizeOf(Word));
{$ELSE}
  WriteBuffer(Value, SizeOf(Word));
{$ENDIF}
end;

function TStreamHelper.GetWord: TStreamWord;
{$IF SIZEOF(TStream.TStreamWord) > 2}
var
  WordValue: Word;
{$ENDIF}
begin
{$IF SIZEOF(TStream.TStreamWord) > 2}
  ReadBuffer(WordValue, SizeOf(Word));
  Result := WordValue;
{$ELSE}
  ReadBuffer(Result, SizeOf(Word));
{$ENDIF}
end;

procedure TStreamHelper.PutLongWord(const Value: TStreamLongWord);
{$IF SIZEOF(TStream.TStreamLongWord) > 4}
var
  LongWordValue: LongWord;
{$ENDIF}
begin
{$IF SIZEOF(TStream.TStreamLongWord) > 4}
  if Value <= High(LongWord) then
    LongWordValue := Value
  else
    LongWordValue := High(LongWord);

  WriteBuffer(LongWordValue, SizeOf(LongWord));
{$ELSE}
  WriteBuffer(Value, SizeOf(LongWord));
{$ENDIF}
end;

function TStreamHelper.GetLongWord: TStreamLongWord;
{$IF SIZEOF(TStream.TStreamLongWord) > 4}
var
  LongWordValue: LongWord;
{$ENDIF}
begin
{$IF SIZEOF(TStream.TStreamLongWord) > 4}
  ReadBuffer(LongWordValue, SizeOf(LongWord));
  Result := LongWordValue;
{$ELSE}
  ReadBuffer(Result, SizeOf(LongWord));
{$ENDIF}
end;

procedure TStreamHelper.PutUInt64(const Value: TStreamUInt64);
begin
  WriteBuffer(Value, SizeOf(UInt64));
end;

function TStreamHelper.GetUInt64: TStreamUInt64;
begin
  ReadBuffer(Result, SizeOf(UInt64));
end;

procedure TStreamHelper.PutShortInt(const Value: TStreamShortInt);
var
  ShortValue: ShortInt;
begin
  if Value < Low(ShortInt) then
    ShortValue := Low(ShortInt)
  else if Value > High(ShortInt) then
    ShortValue := High(ShortInt)
  else
    ShortValue := Value;

  WriteBuffer(ShortValue, SizeOf(ShortInt));
end;

function TStreamHelper.GetShortInt: TStreamShortInt;
var
  ShortValue: ShortInt;
begin
  ReadBuffer(ShortValue, SizeOf(ShortInt));
  Result := ShortValue;
end;

procedure TStreamHelper.PutSmallInt(const Value: TStreamSmallInt);
{$IF SIZEOF(TStream.TStreamSmallInt) > 2}
var
  SmallValue: SmallInt;
{$ENDIF}
begin
{$IF SIZEOF(TStream.TStreamSmallInt) > 2}
  if Value < Low(SmallInt) then
    SmallValue := Low(SmallInt)
  else if Value > High(SmallInt) then
    SmallValue := High(SmallInt)
  else
    SmallValue := Value;

  WriteBuffer(SmallValue, SizeOf(SmallInt));
{$ELSE}
  WriteBuffer(Value, SizeOf(SmallInt));
{$ENDIF}
end;

function TStreamHelper.GetSmallInt: TStreamSmallInt;
{$IF SIZEOF(TStream.TStreamSmallInt) > 2}
var
  SmallValue: SmallInt;
{$ENDIF}
begin
{$IF SIZEOF(TStream.TStreamSmallInt) > 2}
  ReadBuffer(SmallValue, SizeOf(SmallInt));
  Result := SmallValue;
{$ELSE}
  ReadBuffer(Result, SizeOf(SmallInt));
{$ENDIF}
end;

procedure TStreamHelper.PutLongInt(const Value: TStreamLongInt);
{$IF SIZEOF(TStream.TStreamLongInt) > 4}
var
  LongValue: LongInt;
{$ENDIF}
begin
{$IF SIZEOF(TStream.TStreamLongInt) > 4}
  if Value < Low(LongInt) then
    LongValue := Low(LongInt)
  else if Value > High(LongInt) then
    LongValue := High(LongInt)
  else
    LongValue := Value;

  WriteBuffer(LongValue, SizeOf(LongInt));
{$ELSE}
  WriteBuffer(Value, SizeOf(LongInt));
{$ENDIF}
end;

function TStreamHelper.GetLongInt: TStreamLongInt;
{$IF SIZEOF(TStream.TStreamLongInt) > 4}
var
  LongValue: LongInt;
{$ENDIF}
begin
{$IF SIZEOF(TStream.TStreamLongInt) > 4}
  ReadBuffer(LongValue, SizeOf(LongInt));
  Result := LongValue;
{$ELSE}
  ReadBuffer(Result, SizeOf(LongInt));
{$ENDIF}
end;

procedure TStreamHelper.PutInt64(const Value: TStreamInt64);
begin
  WriteBuffer(Value, SizeOf(Int64));
end;

function TStreamHelper.GetInt64: TStreamInt64;
begin
  ReadBuffer(Result, SizeOf(Int64));
end;

procedure TStreamHelper.PutSingle(const Value: TStreamSingle);
begin
  WriteBuffer(Value, SizeOf(Single));
end;

function TStreamHelper.GetSingle: TStreamSingle;
begin
  ReadBuffer(Result, SizeOf(Single));
end;

procedure TStreamHelper.PutDouble(const Value: TStreamDouble);
begin
  WriteBuffer(Value, SizeOf(Double));
end;

function TStreamHelper.GetDouble: TStreamDouble;
begin
  ReadBuffer(Result, SizeOf(Double));
end;

procedure TStreamHelper.PutByteBool(const Value: TStreamByteBool);
var
  ByteValue: Byte;
begin
  ByteValue := 255;

  if Value then
    ByteValue := 0;

  WriteBuffer(ByteValue, SizeOf(Byte));
end;

function TStreamHelper.GetByteBool: TStreamByteBool;
var
  ByteValue: Byte;
begin
  ReadBuffer(ByteValue, SizeOf(Byte));
  Result := ByteValue < 128;
end;

procedure TStreamHelper.PutByteIndex(const Value: TStreamByteIndex);
var
  ByteValue: Byte;
begin
  if Value < 0 then
    ByteValue := 255
  else if Value > 254 then
    ByteValue := 254
  else
    ByteValue := Value;

  WriteBuffer(ByteValue, SizeOf(Byte));
end;

function TStreamHelper.GetByteIndex: TStreamByteIndex;
var
  ByteValue: Byte;
begin
  Result := -1;

  if (Read(ByteValue, SizeOf(Byte)) = SizeOf(Byte)) and (ByteValue <> 255) then
    Result := ByteValue;
end;

procedure TStreamHelper.PutWordIndex(const Value: TStreamWordIndex);
var
  WordValue: Word;
begin
  if Value < 0 then
    WordValue := 65535
  else if Value > 65534 then
    WordValue := 65534
  else
    WordValue := Value;

  Write(WordValue, SizeOf(Word));
end;

function TStreamHelper.GetWordIndex: TStreamWordIndex;
var
  WordValue: Word;
begin
  Result := -1;

  if (Read(WordValue, SizeOf(Word)) = SizeOf(Word)) and (WordValue <> 65535) then
    Result := WordValue;
end;

procedure TStreamHelper.PutBytePoint2px(const Value: TPoint2px);
var
  ByteValue: Byte;
begin
  if Value.X <> Undefined2px.X then
    ByteValue := Saturate(Value.X, 0, 254)
  else
    ByteValue := 255;

  Write(ByteValue, SizeOf(Byte));

  if Value.Y <> Undefined2px.Y then
    ByteValue := Saturate(Value.Y, 0, 254)
  else
    ByteValue := 255;

  Write(ByteValue, SizeOf(Byte));
end;

function TStreamHelper.GetBytePoint2px: TPoint2px;
var
  ByteValue1, ByteValue2: Byte;
  ValueInvalid: Boolean;
begin
  ValueInvalid := False;

  if Read(ByteValue1, SizeOf(Byte)) <> SizeOf(Byte) then
    ValueInvalid := True;

  if Read(ByteValue2, SizeOf(Byte)) <> SizeOf(Byte) then
    ValueInvalid := True;

  if (ByteValue1 = 255) or (ByteValue2 = 255) or ValueInvalid then
    Exit(Undefined2px);

  Result.X := ByteValue1;
  Result.Y := ByteValue2;
end;

procedure TStreamHelper.PutWordPoint2px(const Value: TPoint2px);
var
  InpValue: TStreamWordIndex;
  WordValue: Word;
begin
  if Value.X <> Undefined2px.X then
  begin
    InpValue := Value.X;
    WordValue := Saturate(InpValue, 0, 65534)
  end
  else
    WordValue := 65535;

  Write(WordValue, SizeOf(Word));

  if Value.Y <> Undefined2px.Y then
  begin
    InpValue := Value.Y;
    WordValue := Saturate(InpValue, 0, 65534)
  end
  else
    WordValue := 65535;

  Write(WordValue, SizeOf(Word));
end;

function TStreamHelper.GetWordPoint2px: TPoint2px;
var
  WordValue1, WordValue2: Word;
  ValueInvalid: Boolean;
begin
  ValueInvalid := False;

  if Read(WordValue1, SizeOf(Word)) <> SizeOf(Word) then
    ValueInvalid := True;

  if Read(WordValue2, SizeOf(Word)) <> SizeOf(Word) then
    ValueInvalid := True;

  if (WordValue1 = 65535) or (WordValue2 = 65535) or ValueInvalid then
    Exit(Undefined2px);

  Result.X := WordValue1;
  Result.Y := WordValue2;
end;

procedure TStreamHelper.PutLongPoint2px(const Value: TPoint2px);
begin
  PutLongInt(Value.X);
  PutLongInt(Value.Y);
end;

function TStreamHelper.GetLongPoint2px: TPoint2px;
begin
  Result.X := GetLongInt;
  Result.Y := GetLongInt;
end;

procedure TStreamHelper.PutFloat34(const Value: Single);
begin
  PutShortInt(Saturate(Round(Value * 16), -128, 127));
end;

function TStreamHelper.GetFloat34: Single;
begin
  Result := GetShortInt / 16;
end;

procedure TStreamHelper.PutFloat43(const Value: Single);
begin
  PutShortInt(Saturate(Round(Value * 8), -128, 127));
end;

function TStreamHelper.GetFloat43: Single;
begin
  Result := GetShortInt / 8;
end;

procedure TStreamHelper.PutFloats44(const Value1, Value2: Single);
var
  Temp1, Temp2: Integer;
begin
  Temp1 := Saturate(Round(Value1), -8, 7) + 8;
  Temp2 := Saturate(Round(Value2), -8, 7) + 8;

  PutByte(Temp1 or (Temp2 shl 4));
end;

procedure TStreamHelper.GetFloats44(out Value1, Value2: Single);
var
  Temp: Integer;
begin
  Temp := GetByte;

  Value1 := (Temp and $0F) - 8;
  Value2 := (Temp shr 4) - 8;
end;

procedure TStreamHelper.PutFloats3311(const Value1, Value2: Single);
var
  Temp1, Temp2: Integer;
begin
  Temp1 := Saturate(Round(Value1 * 2), -8, 7) + 8;
  Temp2 := Saturate(Round(Value2 * 2), -8, 7) + 8;

  PutByte(Temp1 or (Temp2 shl 4));
end;

procedure TStreamHelper.GetFloats3311(out Value1, Value2: Single);
var
  Temp: Integer;
begin
  Temp := GetByte;

  Value1 := ((Temp and $0F) - 8) / 2;
  Value2 := ((Temp shr 4) - 8) / 2;
end;

{$IFDEF DELPHI_NEXTGEN}

procedure TStreamHelper.PutShortString(const Text: UniString; const MaxCount: Integer);
var
  Count: Integer;
  Bytes: TBytes;
begin
  Bytes := StringToBytes(Text);
  Count := Length(Bytes);

  if Count > 255 then
    Count := 255;

  if (MaxCount > 0) and (MaxCount < Count) then
    Count := MaxCount;

  PutByte(Count);

  Write(Pointer(Bytes)^, Count);
end;

function TStreamHelper.GetShortString: UniString;
var
  Count: Integer;
  Bytes: TBytes;
begin
  Count := GetByte;
  SetLength(Bytes, Count);

  if Read(Pointer(Bytes)^, Count) = Count then
    Result := BytesToString(Bytes)
  else
    Result := string.Empty;
end;

procedure TStreamHelper.PutMediumString(const Text: UniString; const MaxCount: Integer);
var
  Count: Integer;
  Bytes: TBytes;
begin
  Bytes := StringToBytes(Text);
  Count := Length(Bytes);

  if Count > 65535 then
    Count := 65535;

  if (MaxCount > 0) and (MaxCount < Count) then
    Count := MaxCount;

  PutWord(Count);

  Write(Pointer(Bytes)^, Count);
end;

function TStreamHelper.GetMediumString: UniString;
var
  Count: Integer;
  Bytes: TBytes;
begin
  Count := GetWord;
  SetLength(Bytes, Count);

  if Read(Pointer(Bytes)^, Count) = Count then
    Result := BytesToString(Bytes)
  else
    Result := string.Empty;
end;

procedure TStreamHelper.PutLongString(const Text: UniString);
var
  Count: Integer;
  Bytes: TBytes;
begin
  Bytes := StringToBytes(Text);
  Count := Length(Bytes);

  PutLongInt(Count);
  Write(Pointer(Bytes)^, Count);
end;

function TStreamHelper.GetLongString: UniString;
var
  Count: Integer;
  Bytes: TBytes;
begin
  Count := GetLongInt;
  SetLength(Bytes, Count);

  if Read(Pointer(Bytes)^, Count) = Count then
    Result := BytesToString(Bytes)
  else
    Result := string.Empty;
end;

{$ELSE}

procedure TStreamHelper.PutShortString(const Text: UniString; const MaxCount: Integer);
var
  Count: Integer;
  ShortText: AnsiString;
begin
  ShortText := UTF8Encode(Text);
  Count := Length(ShortText);

  if Count > 255 then
    Count := 255;

  if (MaxCount > 0) and (MaxCount < Count) then
    Count := MaxCount;

  PutByte(Count);

  Write(Pointer(ShortText)^, Count);
end;

function TStreamHelper.GetShortString: UniString;
var
  Count: Integer;
  ShortText: AnsiString;
begin
  Count := GetByte;
  SetLength(ShortText, Count);

  if Read(Pointer(ShortText)^, Count) <> Count then
    Exit('');

{$IFDEF FPC}
  Result := UTF8Decode(ShortText);
{$ELSE}
  Result := UTF8ToWideString(ShortText);
{$IFEND}
end;

procedure TStreamHelper.PutMediumString(const Text: UniString; const MaxCount: Integer);
var
  Count: Integer;
  MediumText: AnsiString;
begin
  MediumText := UTF8Encode(Text);

  Count := Length(MediumText);
  if Count > 65535 then
    Count := 65535;

  if (MaxCount > 0) and (MaxCount < Count) then
    Count := MaxCount;

  PutWord(Count);

  Write(Pointer(MediumText)^, Count);
end;

function TStreamHelper.GetMediumString: UniString;
var
  Count: Integer;
  MediumText: AnsiString;
begin
  Count := GetWord;
  SetLength(MediumText, Count);

  if Read(Pointer(MediumText)^, Count) <> Count then
    Exit('');

{$IFDEF FPC}
  Result := UTF8Decode(MediumText);
{$ELSE}
  Result := UTF8ToWideString(MediumText);
{$IFEND}
end;

procedure TStreamHelper.PutLongString(const Text: UniString);
var
  Count: Integer;
  LongText: AnsiString;
begin
  LongText := UTF8Encode(Text);

  Count := Length(LongText);
  PutLongInt(Count);

  Write(Pointer(LongText)^, Count);
end;

function TStreamHelper.GetLongString: UniString;
var
  Count: Integer;
  LongText: AnsiString;
begin
  Count := GetLongInt;
  SetLength(LongText, Count);

  if Read(Pointer(LongText)^, Count) <> Count then
    Exit('');

{$IFDEF FPC}
  Result := UTF8Decode(LongText);
{$ELSE}
  Result := UTF8ToWideString(LongText);
{$IFEND}
end;

{$ENDIF}

{$ENDREGION}
{$REGION 'TAssetStream'}

constructor TAssetStream.Create(const AFileName: StdString);
begin
  inherited Create;

  FFileName := AFileName;

{$IFDEF ANDROID_ASSETS}
  if FAssetManager = nil then
    raise EAssetManagerNotSpecified.Create(SAssetManagerNotSpecified);

  FAsset := AAssetManager_open(FAssetManager, PAnsiChar(FFileName), AASSET_MODE_STREAMING);
  if FAsset = nil then
    raise EFOpenError.CreateResFmt(@SFOpenError, [FFileName]);

  FFileSize := AAsset_getLength64(FAsset);
{$ELSE}
  inherited Create;
{$ENDIF}
end;

destructor TAssetStream.Destroy;
begin
{$IFDEF ANDROID_ASSETS}
  if FAsset <> nil then
  begin
    AAsset_close(FAsset);
    FAsset := nil;
  end;

  FFileSize := 0;
{$ENDIF}

  inherited;
end;

function TAssetStream.GetSize: Int64;
begin
{$IFDEF ANDROID_ASSETS}
  Result := FFileSize;
{$ELSE}
  Result := inherited;
{$ENDIF}
end;

function TAssetStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
{$IFDEF ANDROID_ASSETS}
  Result := AAsset_seek64(FAsset, Offset, Ord(Origin));
{$ELSE}
  Result := inherited;
{$ENDIF}
end;

function TAssetStream.Read(var Buffer; Count: LongInt): LongInt;
begin
{$IFDEF ANDROID_ASSETS}
  Result := AAsset_read(FAsset, @Buffer, Count);
{$ELSE}
  Result := inherited;
{$ENDIF}
end;

{$ENDREGION}

end.
