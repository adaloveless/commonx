{
 *  Copyright (c) 2013 Maciej Izak (aka. HNB or mrmizak)
 *
 *  This software is provided 'as-is', without any express or
 *  implied warranty. In no event will the authors be held
 *  liable for any damages arising from the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute
 *  it freely, subject to the following restrictions:
 *
 *  1. The origin of this software must not be misrepresented;
 *     you must not claim that you wrote the original software.
 *     If you use this software in a product, an acknowledgment
 *     in the product documentation would be appreciated but
 *     is not required.
 *
 *  2. Altered source versions must be plainly marked as such,
 *     and must not be misrepresented as being the original software.
 *
 *  3. This notice may not be removed or altered from any
 *     source distribution.
 *
 *  Homepage: https://code.google.com/p/fpc-generics-collections/
 *  SVN: http://fpc-generics-collections.googlecode.com/svn/trunk/
 *  ZIP: https://code.google.com/p/fpc-generics-collections/downloads/list
 *
 *  *LICENSE* to choose from:
 *  -zlib
 *  -beerware mixed with zlib
 *  -postcardware mixed with zlib
 *
 * If you select beerware (preferred) or postcardware, please send me email
 * (hnb.code[at]gmail.com) to get my home address.
}

unit Generics.Helpers experimental;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TValueHelper }

  TValueHelper<T> = record
  private
    F: T;
  public
    class operator Implicit(const A: T): TValueHelper<T>; inline;
    class operator Implicit(const A: TValueHelper<T>): T; inline;

    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;
  end;

  { TValueAnsiStringHelper }

  TValueAnsiStringHelper = record helper for AnsiString
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;
  end;

  { TValueUnicodeStringHelper }

  TValueUnicodeStringHelper = record helper for UnicodeString
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;
  end;

  { TValueShortStringHelper }

  TValueShortStringHelper = record helper for ShortString
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;
  end;

  { TValueUTF8StringHelper }

  TValueUTF8StringHelper = record helper for UTF8String
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;
  end;

  { TValueAnsiCharHelper }

  TValueAnsiCharHelper = record helper for AnsiChar
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;
  end;

  { TValueWideCharHelper }

  TValueWideCharHelper = record helper for WideChar
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;
  end;

  { TValueInt8Helper }

  TValueInt8Helper = record helper for Int8
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;

    function High: Integer; inline;
    function Low: Integer; inline;

    class function GetSignMask: UInt32; static; inline;
  end;

  { TValueInt16Helper }

  TValueInt16Helper = record helper for Int16
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;

    function High: Integer; inline;
    function Low: Integer; inline;

    class function GetSignMask: UInt32; static; inline;
  end;

  { TValueInt32Helper }

  TValueInt32Helper = record helper for Int32
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;

    function High: LongInt; inline;
    function Low: LongInt; inline;

    class function GetSignMask: UInt32; static; inline;
  end;

  { TValueUInt8Helper }

  TValueUInt8Helper = record helper for UInt8
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;

    function High: LongInt; inline;
    function Low: LongInt; inline;

    class function GetSignMask: UInt32; static; inline;
  end;

  { TValueUInt16Helper }

  TValueUInt16Helper = record helper for UInt16
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;

    function High: LongInt; inline;
    function Low: LongInt; inline;

    class function GetSignMask: UInt32; static; inline;
  end;

  { TValueUInt32Helper }

  TValueUInt32Helper = record helper for UInt32
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;

    class function GetSignMask: UInt32; static; inline;
  end;

  { TValueSingleHelper }

  TValueSingleHelper = record helper for Single
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;
  end;

  { TValueDoubleHelper }

  TValueDoubleHelper = record helper for Double
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;
  end;

  { TValueExtendedHelper }

  TValueExtendedHelper = record helper for Extended
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;
  end;

  { TValueCompHelper }

  TValueCompHelper = record helper for Comp
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;
  end;

  { TValueReal48Helper }

  TValueReal48Helper = record helper for Real48
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;
  end;

  { TValueRealHelper }

  TValueRealHelper = record helper for Real
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;
  end;

  { TValueCurrencyHelper }

  TValueCurrencyHelper = record helper for Currency
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;
  end;

  { TValueVariantHelper }

//  TValueVariantHelper = record helper for Variant
//    function GetValueSize: Integer; inline;
//    function GetReferenceToValue: Pointer; inline;
//  end;

  { TValueTObjectHelper }

  TValueTObjectHelper = class helper for TObject
    function GetValueSize: Integer; inline;
    function GetReferenceToValue: Pointer; inline;
  end;

implementation

{ TValueHelper<T> }

class operator TValueHelper<T>.Implicit(const A: T): TValueHelper<T>;
begin
  Result.F := A;
end;

class operator TValueHelper<T>.Implicit(const A: TValueHelper<T>): T;
begin
  Result := A.F;
end;

function TValueHelper<T>.GetValueSize: Integer;
begin
  Result := SizeOf(T);
end;

function TValueHelper<T>.GetReferenceToValue: Pointer;
begin
  Result := @F;
end;

{ TValueTObjectHelper }

function TValueTObjectHelper.GetValueSize: Integer;
begin
  Result := SizeOf(TObject);
end;

function TValueTObjectHelper.GetReferenceToValue: Pointer;
begin
  Result := @Self;
end;

{ TValueWideCharHelper }

function TValueWideCharHelper.GetValueSize: Integer;
begin
  Result := SizeOf(WideChar);
end;

function TValueWideCharHelper.GetReferenceToValue: Pointer;
begin
  Result := @Self;
end;

{ TValueAnsiCharHelper }

function TValueAnsiCharHelper.GetValueSize: Integer;
begin
  Result := SizeOf(Ansichar);
end;

function TValueAnsiCharHelper.GetReferenceToValue: Pointer;
begin
  Result := @Self;
end;

{ TValueCurrencyHelper }

function TValueCurrencyHelper.GetValueSize: Integer;
begin
  Result := SizeOf(Currency);
end;

function TValueCurrencyHelper.GetReferenceToValue: Pointer;
begin
  Result := @Self;
end;

{ TValueRealHelper }

function TValueRealHelper.GetValueSize: Integer;
begin
  Result := SizeOf(Real);
end;

function TValueRealHelper.GetReferenceToValue: Pointer;
begin
  Result := @Self;
end;

{ TValueReal48Helper }

function TValueReal48Helper.GetValueSize: Integer;
begin
  Result := SizeOf(Real48);
end;

function TValueReal48Helper.GetReferenceToValue: Pointer;
begin
    Result := @Self;
end;

{ TValueCompHelper }

function TValueCompHelper.GetValueSize: Integer;
begin
  Result := SizeOf(Comp);
end;

function TValueCompHelper.GetReferenceToValue: Pointer;
begin
  Result := @Self;
end;

{ TValueUTF8StringHelper }

function TValueUTF8StringHelper.GetValueSize: Integer;
begin
  Result := Length(Self);
end;

function TValueUTF8StringHelper.GetReferenceToValue: Pointer;
begin
  if Length(Self) = 0 then
    Result := nil
  else
    Result := @Self[1];
end;

{ TValueShortStringHelper }

function TValueShortStringHelper.GetValueSize: Integer;
begin
  Result := Length(Self);
end;

function TValueShortStringHelper.GetReferenceToValue: Pointer;
begin
  if Length(Self) = 0 then
    Result := nil
  else
    Result := @Self[1];
end;

{ TValueExtendedHelper }

function TValueExtendedHelper.GetValueSize: Integer;
begin
  Result := SizeOf(Extended);
end;

function TValueExtendedHelper.GetReferenceToValue: Pointer;
begin
  Result := @Self;
end;

{ TValueDoubleHelper }

function TValueDoubleHelper.GetValueSize: Integer;
begin
  Result := SizeOf(Double);
end;

function TValueDoubleHelper.GetReferenceToValue: Pointer;
begin
  Result := @Self;
end;

{ TValueSingleHelper }

function TValueSingleHelper.GetValueSize: Integer;
begin
  Result := SizeOf(Single);
end;

function TValueSingleHelper.GetReferenceToValue: Pointer;
begin
  Result := @Self;
end;

{ TValueUnicodeStringHelper }

function TValueUnicodeStringHelper.GetValueSize: Integer;
begin
  Result := Length(Self) * SizeOf(UnicodeChar);
end;

function TValueUnicodeStringHelper.GetReferenceToValue: Pointer;
begin
  if Length(Self) <> 0 then
    Result := @Self[1]
  else
    Result := nil;
end;

{ TValueUInt32Helper }

function TValueUInt32Helper.GetValueSize: Integer;
begin
  Result := SizeOf(UInt32);
end;

function TValueUInt32Helper.GetReferenceToValue: Pointer;
begin
  Result := @Self;
end;

class function TValueUInt32Helper.GetSignMask: UInt32;
begin
  Result := $80000000;
end;

{ TValueUInt16Helper }

function TValueUInt16Helper.GetValueSize: Integer;
begin
  Result := SizeOf(UInt16);
end;

function TValueUInt16Helper.GetReferenceToValue: Pointer;
begin
  Result := @Self;
end;

function TValueUInt16Helper.High: LongInt;
begin
  Result := System.High(UInt16);
end;

function TValueUInt16Helper.Low: LongInt;
begin
  Result := System.Low(UInt16);
end;

class function TValueUInt16Helper.GetSignMask: UInt32;
begin
  Result := $8000;
end;

{ TValueUInt8Helper }

function TValueUInt8Helper.GetValueSize: Integer;
begin
  Result := SizeOf(UInt8);
end;

function TValueUInt8Helper.GetReferenceToValue: Pointer;
begin
  Result := @Self;
end;

function TValueUInt8Helper.High: LongInt;
begin
  Result := System.High(UInt8);
end;

function TValueUInt8Helper.Low: LongInt;
begin
  Result := System.Low(UInt8);
end;

class function TValueUInt8Helper.GetSignMask: UInt32;
begin
  Result := $80;
end;

{ TValueInt32Helper }

function TValueInt32Helper.GetValueSize: Integer;
begin
  Result := SizeOf(Int32);
end;

function TValueInt32Helper.GetReferenceToValue: Pointer;
begin
  Result := @Self;
end;

function TValueInt32Helper.High: LongInt;
begin
  Result := System.High(Int32);
end;

function TValueInt32Helper.Low: LongInt;
begin
  Result := System.Low(Int32);
end;

class function TValueInt32Helper.GetSignMask: UInt32;
begin
  Result := $80000000;
end;

{ TValueInt16Helper }

function TValueInt16Helper.GetValueSize: Integer;
begin
  Result := SizeOf(Int16);
end;

function TValueInt16Helper.GetReferenceToValue: Pointer;
begin
  Result := @Self;
end;

function TValueInt16Helper.High: Integer;
begin
  Result := System.High(Int16);
end;

function TValueInt16Helper.Low: Integer;
begin
  Result := System.Low(Int16);
end;

class function TValueInt16Helper.GetSignMask: UInt32;
begin
  Result := $8000
end;

{ TValueInt8Helper }

function TValueInt8Helper.GetValueSize: Integer;
begin
  Result := SizeOf(Int8);
end;

function TValueInt8Helper.GetReferenceToValue: Pointer;
begin
  Result := @Self;
end;

function TValueInt8Helper.High: Integer;
begin
  Result := System.High(Int8);
end;

function TValueInt8Helper.Low: Integer;
begin
  Result := System.Low(Int8);
end;

class function TValueInt8Helper.GetSignMask: UInt32;
begin
  Result := $80;
end;

{ TRawDataStringHelper }

function TValueAnsiStringHelper.GetValueSize: Integer;
begin
  Result := Length(Self) * SizeOf(AnsiChar);
end;

function TValueAnsiStringHelper.GetReferenceToValue: Pointer;
begin
  if Length(Self) <> 0 then
    Result := @Self[1]
  else
    Result := nil;
end;

end.

