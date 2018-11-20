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

unit Generics.Defaults experimental;

{$MODE DELPHI}{$H+}
{$POINTERMATH ON}
{$MACRO ON}
{$COPERATORS ON}

interface

uses
  Classes, SysUtils, Generics.Hashes;


  //TEqualityComparison<T> = reference to function(const Left, Right: T): Boolean;
  //THasher<T> = reference to function(const Value: T): Integer;

  //IEqualityComparer<T> = interface
  //  function Equals(const Left, Right: T): Boolean;
  //  function GetHashCode(const Value: T): Integer;
  //end;
  //
  //TEqualityComparer<T> = class(TInterfacedObject, IEqualityComparer<T>)
  //public
  //  class function Default: IEqualityComparer<T>; static;
  //
  //  class function Construct(const EqualityComparison: TEqualityComparison<T>;
  //    const Hasher: THasher<T>): IEqualityComparer<T>;
  //
  //  function Equals(const Left, Right: T): Boolean;
  //    reintroduce; overload; virtual;
  //  function GetHashCode(const Value: T): Integer;
  //    reintroduce; overload; virtual;
  //end;

//
//  TComparison<T> = reference to function(const Left, Right: T): Integer;
//
//  // Abstract base class for IComparer<T> implementations, and a provider
//  // of default IComparer<T> implementations.
//  TComparer<T> = class(TInterfacedObject, IComparer<T>)
//  public
//    class function Default: IComparer<T>;
//    class function Construct(const Comparison: TComparison<T>): IComparer<T>;
//    function Compare(const Left, Right: T): Integer; virtual; abstract;
//  end;


// Delphi version of Bob Jenkins Hash
function BobJenkinsHash(const Data; Len, InitData: Integer): Integer; // same result as HashLittle_Delphi, different interface

type
  { Delphi EqualityComparer version }
  //IComparer<T> = interface
  //  function Compare(const Left, Right: T): Integer;
  //end;

  //IEqualityComparer<T> = interface
  //  function Equals(const Left, Right: T): Boolean;
  //  function GetHashCode(const Value: T): Integer;
  //end;

  { FreePascal EqualityComparer version }
  IComparer = interface
    function Compare(ALeft, ARight: Pointer; ALeftLength, ARightLength: Integer): Integer;
  end;

  IComparer<T> = interface(IComparer)
  end;

  TComparer<T> = class(TInterfacedObject, IComparer<T>)
  public
    class function Default: IComparer<T>; static;

    function Compare(ALeft, ARight: Pointer; ALeftLength, ARightLength: Integer): Integer; virtual; abstract;
  end;

  TDefaultComparer<T> = class(TComparer<T>)
  public
    function Compare(ALeft, ARight: Pointer; ALeftLength, ARightLength: Integer): Integer; override;
  end;

  IEqualityComparer = interface
    function Equals(ALeft, ARight: Pointer; ALeftLength, ARightLength: Integer): Boolean;

    function GetHashCode(AKey: Pointer; ALength: Integer): UInt32; overload;
    function GetHashCode(AKey: Pointer; ALength: Integer; out AHash: UInt32): UInt32; overload;
  end;

  IEqualityComparer<T> = interface(IEqualityComparer)
  end;

  { THashFactory }
  THashFactoryClass = class of THashFactory;
  THashFactory = class
  public
    class function Hash(AKey: Pointer; ASize: Integer): UInt32; virtual; overload; abstract;
    class function Hash(AKey: Pointer; ASize: Integer; out AHash: UInt32): UInt32; virtual; overload;
  end;

  //TEqualityComparerClass = class of TEqualityComparer;
  TEqualityComparer<T> = class(TInterfacedObject, IEqualityComparer<T>)
  public
    class function Default: IEqualityComparer<T>; static;

    function Equals(ALeft, ARight: Pointer; ALeftSize, ARightSize: Integer): Boolean; reintroduce; overload; virtual;

    function GetHashCode(AKey: Pointer; ASize: Integer): UInt32; virtual; overload; abstract;
    function GetHashCode(AKey: Pointer; ASize: Integer; out AHash: UInt32): UInt32; virtual; overload;
  end;

  { TDelphiHashFactory }
  TDelphiHashFactory = class(THashFactory)
  public
    class function Hash(AKey: Pointer; ASize: Integer): UInt32; override;
  end;

  TDelphiHashFactory2 = class(THashFactory)
  public
    class function Hash(AKey: Pointer; ASize: Integer): UInt32; override;
    class function Hash(AKey: Pointer; ASize: Integer; out AHash: UInt32): UInt32; override;
  end;

  TDelphiEqualityComparer<T> = class(TEqualityComparer<T>)
    function GetHashCode(AKey: Pointer; ALength: Integer): UInt32; override;
  end;

  TDelphiEqualityComparer2<T> = class(TEqualityComparer<T>)
    function GetHashCode(AKey: Pointer; ALength: Integer): UInt32; override; overload;
    function GetHashCode(AKey: Pointer; ALength: Integer; out AHash: UInt32): UInt32; override; overload;
  end;

  { THashLittleFactory }
  THashLittleFactory = class(THashFactory)
  public
    class function Hash(AKey: Pointer; ASize: Integer): UInt32; override;
  end;

  THashLittleEqualityComparer<T> = class(TEqualityComparer<T>)
    function GetHashCode(AKey: Pointer; ALength: Integer): UInt32; override;
  end;

implementation


{ TEqualityComparer }

class function TEqualityComparer<T>.Default: IEqualityComparer<T>;
begin
  Result := TDelphiEqualityComparer<T>.Create;
end;

function TEqualityComparer<T>.GetHashCode(AKey: Pointer; ASize: Integer; out AHash: UInt32): UInt32;
begin
  Result := GetHashCode(AKey, ASize);
  AHash := Result;
end;

function TEqualityComparer<T>.Equals(ALeft, ARight: Pointer; ALeftSize, ARightSize: Integer): Boolean;
begin
  if ALeftSize <> ARightSize then
    Exit(False);
  Result := CompareMem(ALeft, ARight, ALeftSize);
end;

{ TComparer<T> }

class function TComparer<T>.Default: IComparer<T>;
begin
  Result := TDefaultComparer<T>.Create;
end;

{ TDefaultComparer<T> }

function TDefaultComparer<T>.Compare(ALeft, ARight: Pointer; ALeftLength, ARightLength: Integer): Integer;
var
  LLength: Integer;
  LLengthDiff: Integer;
begin
  LLength := ALeftLength;
  LLengthDiff := LLength - ARightLength;
  if LLengthDiff < 0 then
  begin
    Inc(LLength, LLengthDiff);
    Result := CompareMemRange(ALeft, ARight, ALeftLength);
  end
  else
    Result := CompareMemRange(ALeft, ARight, ARightLength);
  if Result = 0 then
    Result := LLengthDiff;
end;

//begin
//  if ALeftLength > ARightLength then
//  begin
//    //WriteLn('ALeftLength = ', ALeftLength, ' ARightLength = ', ARightLength, ' CompareMemRange = ', CompareMemRange(ALeft, ARight, ARightLength));
//    Result := CompareMemRange(ALeft, ARight, ARightLength);
//
//    if Result = 0 then
//      Result := (ARightLength - ALeftLength);
//  end
//  else
//  begin
//    Result := CompareMemRange(ALeft, ARight, ALeftLength);
//
//    if Result = 0 then
//      Result := (ALeftLength - ARightLength);
//  end;
//
//end;

{ THashLittleFactory }

class function THashLittleFactory.Hash(AKey: Pointer; ASize: Integer): UInt32;
begin
  Result := HashLittle(AKey, ASize, 0);
end;

{ TDelphiHashFactory }

class function TDelphiHashFactory.Hash(AKey: Pointer; ASize: Integer): UInt32;
begin
  Result := DelphiHashLittle(AKey, ASize, 0);
end;

{ TDelphiHashFactory2 }

class function TDelphiHashFactory2.Hash(AKey: Pointer; ASize: Integer): UInt32;
begin
  Result := DelphiHashLittle(AKey, ASize, 0);
end;

class function TDelphiHashFactory2.Hash(AKey: Pointer; ASize: Integer; out AHash: UInt32): UInt32;
begin
  Result := 0;
  AHash := 0;
  DelphiHashLittle2(AKey, ASize, Result, AHash);
end;

{ THashFactory }

class function THashFactory.Hash(AKey: Pointer; ASize: Integer; out
  AHash: UInt32): UInt32;
begin
  Result := Hash(AKey, ASize);
  AHash := Result;
end;

{ TValueVariantHelper }

//function TValueVariantHelper.GetValueSize: Integer;
//begin
//  //Result :=
//end;
//
//function TValueVariantHelper.GetReferenceToValue: Pointer;
//begin
//
//end;

{ THashLittleFactory }

function THashLittleEqualityComparer<T>.GetHashCode(AKey: Pointer; ALength: Integer): UInt32;
begin
  Result := THashLittleFactory.Hash(AKey, ALength);
end;

{ TDelphiHashFactory }

function TDelphiEqualityComparer<T>.GetHashCode(AKey: Pointer; ALength: Integer): UInt32;
begin
  Result := TDelphiHashFactory.Hash(AKey, ALength);
end;

{ TDelphiHashFactory2 }

function TDelphiEqualityComparer2<T>.GetHashCode(AKey: Pointer; ALength: Integer): UInt32;
begin
  Result := TDelphiHashFactory2.Hash(AKey, ALength);
end;

function TDelphiEqualityComparer2<T>.GetHashCode(AKey: Pointer; ALength: Integer; out AHash: UInt32): UInt32;
begin
  Result := TDelphiHashFactory2.Hash(AKey, ALength, AHash);
end;

function BobJenkinsHash(const Data; Len, InitData: Integer): Integer;
begin
  Result := DelphiHashLittle(@Data, Len, InitData);
end;

end.

