unit PXL.Formats;
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
{< Information, utilities and conversion between different pixel formats. }
interface

{$INCLUDE PXL.Config.inc}

uses
  PXL.TypeDef, PXL.Types;

{ Enable the following option to allow format selector to choose RGB pixel formats as potential alternatives to
  luminance formats when no other options are available. }
{$DEFINE CONVERT_LUMINANCE_TO_RGB}

type
  { Extension for @link(TPixelFormat) to provide number of bits, bytes and string conversion. }
  TPixelFormatHelper = record helper for TPixelFormat
  private
    function GetBits: Integer;
    function GetBytes: Integer;
  public
    { Converts the current pixel format to readable string. }
    function ToString: StdString;

    { Returns @True for pixel formats that contain alpha-channel and @False otherwise. }
    function HasAlphaChannel: Boolean;

    { Returns @True when a simple copy operation can be performed between pixels of source and destination format
      without apparent loss of information. For instance, it is possible to copy pixels from A8R8G8B8 to X8R8G8B8
      since the second one doesn't have meaningful alpha-channel. }
    function CanBulkCopyTo(const DestFormat: TPixelFormat): Boolean;

    { Returns number of bits that each pixel in current format occupies. }
    property Bits: Integer read GetBits;

    { Return number of bytes that each pixel in current format occupies. }
    property Bytes: Integer read GetBytes;
  public
    { Converts readable string to actual pixel format. }
    class function CreateFromString(const Text: StdString): TPixelFormat; static;
  end;

  { List of one or more pixel format elements. }
  TPixelFormatList = class
  {$IFNDEF PASDOC} // workaround for PasDoc issue #86
  public type
    TEnumerator = class
    private
      {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FList: TPixelFormatList;
      Index: Integer;
      function GetCurrent: TPixelFormat;
    public
      constructor Create(const AList: TPixelFormatList);
      destructor Destroy; override;
      function MoveNext: Boolean;
      property Current: TPixelFormat read GetCurrent;
    end;
  {$ENDIF}
  private const
    ListGrowIncrement = 5;
    ListGrowFraction = 10;
  private
    Data: array of TPixelFormat;
    DataCount: Integer;

    SortSampleFormat: TPixelFormat;

    function GetCount: Integer;
    procedure SetCount(const NewCount: Integer);
    function GetItem(const Index: Integer): TPixelFormat;
    procedure SetItem(const Index: Integer; const Value: TPixelFormat);
    procedure Request(const NeedCapacity: Integer);

    procedure ListSwap(const Index1, Index2: Integer);
    function ListCompare(const Format1, Format2: TPixelFormat): Integer;
    function ListSplit(const Start, Stop: Integer): Integer;
    procedure ListSort(const Start, Stop: Integer);
  public
    { @exclude } constructor Create;
    { @exclude } destructor Destroy; override;

    { Inserts specified pixel format entry to the list and returns its index. }
    function Insert(const Format: TPixelFormat): Integer;

    { Inserts all known pixel formats to the list. }
    procedure InsertAll;

    { Returns index of the specified pixel format in the list. If no such entry is found, -1 is returned. }
    function IndexOf(const Format: TPixelFormat): Integer;

    { Includes specified pixel format entry to the list and returns its index. This involves searching for the entry
      first to determine if it's not in the list. For a faster alternative, use just @link(Insert). }
    function Include(const Format: TPixelFormat): Integer;

    { Remove pixel format specified by the given index from the list. If the specified index is invalid, this function
      does nothing. }
    procedure Remove(const Index: Integer);

    { Removes all entries from the list. }
    procedure Clear;

    { Sorts existing pixel format entries in the list according to their similarity to the given pixel format. }
    procedure SortBestMatch(const Format: TPixelFormat);

    { @exclude } function GetEnumerator: TEnumerator;

    property Count: Integer read GetCount write SetCount;
    property Items[const Index: Integer]: TPixelFormat read GetItem write SetItem; default;
  end;

  { Classification type of pixel format. }
  TPixelFormatType = (
    { Typical integer pixel format (e.g. RGBA). }
    Normal,

    { Floating-point pixel format. }
    Float,

    { Luminance pixel format (e.g. L8A8). }
    Luminance,

    { Indexed (palettized) pixel format. }
    Indexed);

  { Pointer to @link(TPixelFormatColorBits). }
  PPixelFormatColorBits = ^TPixelFormatColorBits;

  { Channel bit information. }
  TPixelFormatColorBits = record
    { Number of bits. }
    Count: Integer;

    { Starting bit position. }
    Shift: Integer;

    { @exclude } class operator Equal(const ColorBits1, ColorBits2: TPixelFormatColorBits): Boolean; inline;
    { @exclude } class operator NotEqual(const ColorBits1, ColorBits2: TPixelFormatColorBits): Boolean; inline;
  end;

  { Pointer to @link(TPixelFormatDescription). }
  PPixelFormatDescription = ^TPixelFormatDescription;

  { Complete information about specific pixel format. }
  TPixelFormatDescription = record
    { Classification type of pixel format. }
    FormatType: TPixelFormatType;

    { Total number of bits occupied by each pixel in this format. }
    BitCount: Integer;

    { Actual number of useful bits that this format occupies. }
    UsedBitCount: Integer;

    { Bit information for red channel. }
    RedBits: TPixelFormatColorBits;

    { Bit information for green channel. }
    GreenBits: TPixelFormatColorBits;

    { Bit information for blue channel. }
    BlueBits: TPixelFormatColorBits;

    { Bit information for alpha-channel. }
    AlphaBits: TPixelFormatColorBits;

    { Bit information for luminance channel. }
    LuminanceBits: TPixelFormatColorBits;

    { @exclude } class operator Equal(const Desc1, Desc2: TPixelFormatDescription): Boolean;
    { @exclude } class operator NotEqual(const Desc1, Desc2: TPixelFormatDescription): Boolean; inline;

    { Calculates the number of useful bits. }
    procedure CalculateUsedBitCount;
  end;

{ Converts a single pixel from an arbitrary pixel format back to 32-bit RGBA format (@code(TPixelFormat.A8R8G8B8)).
  If the specified format is not supported, this function returns zero.
    @param(Source Pointer to a valid block of memory where the source pixel is located at.)
    @param(SourceFormat Pixel format that is used to describe the source pixel.)
    @returns(Resulting pixel in 32-bit RGBA format (@code(TPixelFormat.A8R8G8B8)).) }
function PixelXTo32(const Source: Pointer; const SourceFormat: TPixelFormat): TIntColor;

{ Converts a single pixel from 32-bit RGBA format (@code(TPixelFormat.A8R8G8B8)) to an arbitrary format. If the
  specified format is not supported, this function does nothing.
    @param(Source Source pixel specified in 32-bit RGBA format (@code(TPixelFormat.A8R8G8B8)).)
    @param(Dest Pointer to the memory block where the resulting pixel should be written to. This memory should be
      previously allocated.)
    @param(DestFormat Pixel format that is used to describe the destination pixel.) }
procedure Pixel32ToX(const Source: TIntColor; const Dest: Pointer; const DestFormat: TPixelFormat);

{ Converts an array of pixels from an arbitrary format back to 32-bit RGBA format (@code(TPixelFormat.A8R8G8B8)). If
  the specified format is not supported, this function does nothing.
    @param(Source Pointer to a valid memory block that holds the source pixels.)
    @param(Dest Pointer to a valid memory block where destination pixels will be written to.)
    @param(SourceFormat Pixel format that is used to describe the source pixels.)
    @param(Elements The number of pixels to convert.) }
procedure PixelXTo32Array(const Source: Pointer; const Dest: PIntColor; const SourceFormat: TPixelFormat;
  const ElementCount: Integer);

{ Converts an array of pixels from 32-bit RGBA format (@code(TPixelFormat.A8R8G8B8)) to an arbitrary format. If the
  specified format is not supported, this function does nothing.
    @param(Source Pointer to a valid memory block that holds the source pixels.)
    @param(Dest Pointer to a valid memory block where destination pixels will be written to.)
    @param(DestFormat Pixel format that is used to describe the destination pixels.)
    @param(Elements The number of pixels to convert.) }
procedure Pixel32ToXArray(const Source: PIntColor; const Dest: Pointer; const DestFormat: TPixelFormat;
  const ElementCount: Integer);

{ Takes a list of existing pixel formats and tries to find in it a format that closely resembles the provided format
  sample. The heuristics used by this function tries not to add new channels and will never return a format that has
  less channels than the sample; it also tries to avoid converting between different format types like integer and
  floating-point formats. }
function FindClosestPixelFormat(const Format: TPixelFormat; const ExistingFormats: TPixelFormatList): TPixelFormat;

{ Provides detailed description regarding the specified pixel format. Returns @True when successful and @False when
  such information is not available (e.g. for non-standard pixel formats). }
function GetPixelFormatDescription(const Format: TPixelFormat; out Description: TPixelFormatDescription): Boolean;

{ Takes the provided pixel format description and tries to find a pixel format that matches this description. }
function GetMatchingPixelFormat(const Description: TPixelFormatDescription): TPixelFormat;

implementation

uses
  SysUtils;

type
  PPixelFormatInfo = ^TPixelFormatInfo;
  TPixelFormatInfo = packed record
    Rp, Rb: Byte; // Red
    Gp, Gb: Byte; // Green
    Bp, Bb: Byte; // Blue
    Ap, Ab: Byte; // Alpha
    Lp, Lb: Byte; // Luminance (if Dt = 2) or Extra/Unused (if Dt = 0)
    Dt: Byte;     // Type: 0 = Normal, 1 = Float, 2 = Luminance, 3 - Indexed, 255 = Invalid
    Bi: Byte;     // Bit Count
  end;

const
  PixelFormatInfo: packed array[TPixelFormat] of TPixelFormatInfo = (
    (Rp: 255; Rb:  0; Gp: 255; Gb:  0; Bp: 255; Bb:  0; Ap: 255; Ab:  0; Lp: 255; Lb:  0; Dt: 255; Bi:   0), // Unknown
    (Rp:  16; Rb:  8; Gp:   8; Gb:  8; Bp:   0; Bb:  8; Ap:  24; Ab:  8; Lp: 255; Lb:  0; Dt:   0; Bi:  32), // A8R8G8B8
    (Rp:  16; Rb:  8; Gp:   8; Gb:  8; Bp:   0; Bb:  8; Ap: 255; Ab:  0; Lp:  24; Lb:  8; Dt:   0; Bi:  32), // X8R8G8B8
    (Rp:   8; Rb:  4; Gp:   4; Gb:  4; Bp:   0; Bb:  4; Ap:  12; Ab:  4; Lp: 255; Lb:  0; Dt:   0; Bi:  16), // A4R4G4B4
    (Rp:   8; Rb:  4; Gp:   4; Gb:  4; Bp:   0; Bb:  4; Ap: 255; Ab:  0; Lp:  12; Lb:  4; Dt:   0; Bi:  16), // X4R4G4B4
    (Rp:  11; Rb:  5; Gp:   5; Gb:  6; Bp:   0; Bb:  5; Ap: 255; Ab:  0; Lp: 255; Lb:  0; Dt:   0; Bi:  16), // R5G6B5
    (Rp:  10; Rb:  5; Gp:   5; Gb:  5; Bp:   0; Bb:  5; Ap:  15; Ab:  1; Lp: 255; Lb:  0; Dt:   0; Bi:  16), // A1R5G5B5
    (Rp:  10; Rb:  5; Gp:   5; Gb:  5; Bp:   0; Bb:  5; Ap: 255; Ab:  0; Lp:  15; Lb:  1; Dt:   0; Bi:  16), // X1R5G5B5
    (Rp:   4; Rb:  2; Gp:   2; Gb:  2; Bp:   0; Bb:  2; Ap:   6; Ab:  2; Lp: 255; Lb:  0; Dt:   0; Bi:   8), // A2R2G2B2
    (Rp:   5; Rb:  3; Gp:   2; Gb:  3; Bp:   0; Bb:  2; Ap: 255; Ab:  0; Lp: 255; Lb:  0; Dt:   0; Bi:   8), // R3G3B2
    (Rp:   5; Rb:  3; Gp:   2; Gb:  3; Bp:   0; Bb:  2; Ap:   8; Ab:  8; Lp: 255; Lb:  0; Dt:   0; Bi:  16), // A8R3G3B2
    (Rp:   0; Rb: 10; Gp:  10; Gb: 10; Bp:  20; Bb: 10; Ap:  30; Ab:  2; Lp: 255; Lb:  0; Dt:   0; Bi:  32), // A2B10G10R10
    (Rp:   0; Rb: 16; Gp:  16; Gb: 16; Bp:  32; Bb: 16; Ap:  48; Ab: 16; Lp: 255; Lb:  0; Dt:   0; Bi:  64), // A16B16G16R16
    (Rp: 255; Rb:  0; Gp: 255; Gb:  0; Bp: 255; Bb:  0; Ap:   8; Ab:  8; Lp:   0; Lb:  8; Dt:   2; Bi:  16), // A8L8
    (Rp: 255; Rb:  0; Gp: 255; Gb:  0; Bp: 255; Bb:  0; Ap:   4; Ab:  4; Lp:   0; Lb:  4; Dt:   2; Bi:   8), // A4L4
    (Rp: 255; Rb:  0; Gp: 255; Gb:  0; Bp: 255; Bb:  0; Ap: 255; Ab:  0; Lp:   0; Lb: 16; Dt:   2; Bi:  16), // L16
    (Rp: 255; Rb:  0; Gp: 255; Gb:  0; Bp: 255; Bb:  0; Ap: 255; Ab:  0; Lp:   0; Lb:  8; Dt:   2; Bi:   8), // L8
    (Rp:   0; Rb: 16; Gp: 255; Gb:  0; Bp: 255; Bb:  0; Ap: 255; Ab:  0; Lp: 255; Lb:  0; Dt:   1; Bi:  16), // R16F
    (Rp:   0; Rb: 16; Gp:  16; Gb: 16; Bp: 255; Bb:  0; Ap: 255; Ab:  0; Lp: 255; Lb:  0; Dt:   1; Bi:  32), // G16R16F
    (Rp:   0; Rb: 16; Gp:  16; Gb: 16; Bp:  32; Bb: 16; Ap:  48; Ab: 16; Lp: 255; Lb:  0; Dt:   1; Bi:  64), // A16B16G16R16F
    (Rp:   0; Rb: 32; Gp: 255; Gb:  0; Bp: 255; Bb:  0; Ap: 255; Ab:  0; Lp: 255; Lb:  0; Dt:   1; Bi:  32), // R32F
    (Rp:   0; Rb: 32; Gp:  32; Gb: 32; Bp: 255; Bb:  0; Ap: 255; Ab:  0; Lp: 255; Lb:  0; Dt:   1; Bi:  64), // G32R32F
    (Rp:   0; Rb: 32; Gp:  32; Gb: 32; Bp:  64; Bb: 32; Ap:  96; Ab: 32; Lp: 255; Lb:  0; Dt:   1; Bi: 128), // A32B32G32R32F
    (Rp: 255; Rb:  0; Gp: 255; Gb:  0; Bp: 255; Bb:  0; Ap:   0; Ab:  8; Lp: 255; Lb:  0; Dt:   0; Bi:   8), // A8
    (Rp:   0; Rb: 16; Gp:  16; Gb: 16; Bp: 255; Bb:  0; Ap: 255; Ab:  0; Lp: 255; Lb:  0; Dt:   0; Bi:  32), // G16R16
    (Rp:  20; Rb: 10; Gp:  10; Gb: 10; Bp:   0; Bb: 10; Ap:  30; Ab:  2; Lp: 255; Lb:  0; Dt:   0; Bi:  32), // A2R10G10B10
    (Rp:   0; Rb:  8; Gp:   8; Gb:  8; Bp:  16; Bb:  8; Ap:  24; Ab:  8; Lp: 255; Lb:  0; Dt:   0; Bi:  32), // A8B8G8R8
    (Rp:   0; Rb:  8; Gp:   8; Gb:  8; Bp:  16; Bb:  8; Ap: 255; Ab:  0; Lp:  24; Lb:  8; Dt:   0; Bi:  32), // X8B8G8R8
    (Rp:  16; Rb:  8; Gp:   8; Gb:  8; Bp:   0; Bb:  8; Ap: 255; Ab:  0; Lp: 255; Lb:  0; Dt:   0; Bi:  24), // R8G8B8
    (Rp:   8; Rb:  8; Gp:  16; Gb:  8; Bp:  24; Bb:  8; Ap:   0; Ab:  8; Lp: 255; Lb:  0; Dt:   0; Bi:  32), // B8G8R8A8
    (Rp:   8; Rb:  8; Gp:  16; Gb:  8; Bp:  24; Bb:  8; Ap: 255; Ab:  0; Lp:   0; Lb:  8; Dt:   0; Bi:  32), // B8G8R8X8
    (Rp: 255; Rb:  0; Gp: 255; Gb:  0; Bp: 255; Bb:  0; Ap: 255; Ab:  0; Lp: 255; Lb:  0; Dt:   3; Bi:   8)  // I8
  );

  PixelFormatNames: array[TPixelFormat] of StdString = ('UNKNOWN', 'A8R8G8B8', 'X8R8G8B8', 'A4R4G4B4', 'X4R4G4B4',
    'R5G6B5', 'A1R5G5B5', 'X1R5G5B5', 'A2R2G2B2', 'R3G3B2', 'A8R3G3B2', 'A2B10G10R10', 'A16B16G16R16', 'A8L8', 'A4L4',
    'L16', 'L8', 'R16F', 'G16R16F', 'A16B16G16R16F', 'R32F', 'G32R32F', 'A32B32G32R32F', 'A8', 'G16R16', 'A2R10G10B10',
    'A8B8G8R8', 'X8B8G8R8', 'R8G8B8',  'B8G8R8A8', 'B8G8R8X8', 'I8');

class operator TPixelFormatColorBits.Equal(const ColorBits1, ColorBits2: TPixelFormatColorBits): Boolean;
begin
  Result := (ColorBits1.Count = ColorBits2.Count) and (ColorBits1.Shift = ColorBits2.Shift);
end;

class operator TPixelFormatColorBits.NotEqual(const ColorBits1, ColorBits2: TPixelFormatColorBits): Boolean;
begin
  Result := not (ColorBits1 = ColorBits2);
end;

class operator TPixelFormatDescription.Equal(const Desc1, Desc2: TPixelFormatDescription): Boolean;
begin
  if Desc1.FormatType <> Desc2.FormatType then
    Exit(False);

  case Desc1.FormatType of
    TPixelFormatType.Normal,
    TPixelFormatType.Float:
      Result := (Desc1.RedBits = Desc2.RedBits) and (Desc1.GreenBits = Desc2.GreenBits) and
        (Desc1.BlueBits = Desc2.BlueBits) and (Desc1.AlphaBits = Desc2.AlphaBits);

    TPixelFormatType.Luminance:
      Result := (Desc1.LuminanceBits = Desc2.LuminanceBits) and (Desc1.AlphaBits = Desc2.AlphaBits);
  else
    Result := False;
  end;
end;

class operator TPixelFormatDescription.NotEqual(const Desc1, Desc2: TPixelFormatDescription): Boolean;
begin
  Result := not (Desc1 = Desc2);
end;

procedure TPixelFormatDescription.CalculateUsedBitCount;
begin
  UsedBitCount := RedBits.Count + GreenBits.Count + BlueBits.Count + AlphaBits.Count;
end;

function PixelXTo32(const Source: Pointer; const SourceFormat: TPixelFormat): TIntColor;
var
  Bits: Integer;
  Value, Mask: Cardinal;
  Info: PPixelFormatInfo;
begin
  Bits := SourceFormat.Bits;
  if Bits < 8 then
    Exit(0);

  if Bits > 32 then
  begin
    if (SourceFormat = TPixelFormat.A16B16G16R16) then
    begin
      Value := PLongWord(Source)^;
      Result := (((Value shl 16) shr 24) shl 16) or ((Value shr 24) shl 8);

      Value := PLongWord(PtrUInt(Source) + 4)^;
      Result := Result or ((Value shl 16) shr 24) or ((Value shr 24) shl 24);

      Exit;
    end
    else
      Exit(0);
  end;

  Value := 0;
  Move(Source^, Value, Bits div 8);

  case SourceFormat of
    TPixelFormat.R8G8B8,
    TPixelFormat.X8R8G8B8:
      Result := Value or $FF000000;

    TPixelFormat.A8R8G8B8:
      Result := Value;

    TPixelFormat.A8:
      Result := (Value shl 24) or $FFFFFF;

    TPixelFormat.L8,
    TPixelFormat.I8:
      Result := Value or (Value shl 8) or (Value shl 16) or $FF000000;

    TPixelFormat.A8L8:
      begin
        Result := Value and $FF;
        Result := Result or (Result shl 8) or (Result shl 16) or ((Value shr 8) shl 24)
      end;

    TPixelFormat.A4L4:
      begin
        Result := ((Value and $0F) * 255) div 15;
        Result := Result or (Result shl 8) or (Result shl 16) or ((((Value shr 4) * 255) div 15) shl 24);
      end;

    TPixelFormat.L16:
      begin
        Result := Value shr 8;
        Result := Result or (Result shl 8) or (Result shl 16) or $FF000000;
      end;

    TPixelFormat.A8B8G8R8:
      Result := (Value and $FF00FF00) or ((Value shr 16) and $FF) or ((Value and $FF) shl 16);

    TPixelFormat.X8B8G8R8:
      Result := (Value and $0000FF00) or ((Value shr 16) and $FF) or ((Value and $FF) shl 16) or ($FF000000);

    TPixelFormat.B8G8R8A8:
      Result := ((Value shr 24) and $FF) or (((Value shr 16) and $FF) shl 8) or (((Value shr 8) and $FF) shl 16) or
        ((Value and $FF) shl 24);

    TPixelFormat.B8G8R8X8:
      Result := ((Value shr 24) and $FF) or (((Value shr 16) and $FF) shl 8) or (((Value shr 8) and $FF) shl 16) or
        $FF000000;

  else
    begin
      Info := @PixelFormatInfo[SourceFormat];

      if Info.Dt = 0 then
      begin
        // -> Blue Component
        if Info.Bb > 0 then
        begin
          Mask := (1 shl Info.Bb) - 1;
          Result := (((Value shr Info.Bp) and Mask) * 255) div Mask;
        end
        else
          Result := 255;

        // -> Green Component
        if Info.Gb > 0 then
        begin
          Mask := (1 shl Info.Gb) - 1;
          Result := Result or (((((Value shr Info.Gp) and Mask) * 255) div Mask) shl 8);
        end
        else
          Result := Result or $FF00;

        // -> Red Component
        if Info.Rb > 0 then
        begin
          Mask := (1 shl Info.Rb) - 1;
          Result := Result or (((((Value shr Info.Rp) and Mask) * 255) div Mask) shl 16);
        end
        else
          Result := Result or $FF0000;

        // -> Alpha Component
        if Info.Ab > 0 then
        begin
          Mask := (1 shl Info.Ab) - 1;
          Result := Result or (((((Value shr Info.Ap) and Mask) * 255) div Mask) shl 24);
        end
        else
          Result := Result or $FF000000;
      end
      else
        Result := 0;
    end;
  end;
end;

procedure Pixel32ToX(const Source: TIntColor; const Dest: Pointer; const DestFormat: TPixelFormat);
var
  Bits: Integer;
  Value: Cardinal;
  Info: PPixelFormatInfo;
begin
  Bits := DestFormat.Bits;
  if Bits < 8 then
    Exit;

  if Bits > 32 then
  begin
    if (DestFormat = TPixelFormat.A16B16G16R16) then
    begin
      PLongWord(Dest)^ := (((((Source shl 16) shr 24) * $FFFF) div $FF) shl 16) or
        ((((Source shl 8) shr 24) * $FFFF) div $FF);

      PLongWord(PtrUInt(Dest) + 4)^ := ((((Source shl 24) shr 24) * $FFFF) div $FF) or
        ((((Source shr 24) * $FFFF) div $FF) shl 16);

      Exit;
    end
    else
      Exit;
  end;

  Value := 0;

  case DestFormat of
    TPixelFormat.R8G8B8,
    TPixelFormat.X8R8G8B8,
    TPixelFormat.A8R8G8B8:
      Value := Source;

    TPixelFormat.A8:
      Value := Source shr 24;

    TPixelFormat.A8B8G8R8:
      Value := (Source and $FF00FF00) or ((Source shr 16) and $FF) or ((Source and $FF) shl 16);

    TPixelFormat.X8B8G8R8:
      Value := (Source and $0000FF00) or ((Source shr 16) and $FF) or ((Source and $FF) shl 16);

    TPixelFormat.L8,
    TPixelFormat.I8:
      Value := Cardinal(PixelToGray(Source));

    TPixelFormat.A8L8:
      Value := ((Source shr 24) shl 8) or Cardinal(PixelToGray(Source));

    TPixelFormat.A4L4:
      Value := ((Source shr 28) shl 4) or Cardinal(PixelToGray(Source) shr 4);

    TPixelFormat.L16:
      Value := Round(PixelToGrayFloat(Source) * 65535.0);

    TPixelFormat.B8G8R8A8:
      Value := ((Source shr 24) and $FF) or (((Source shr 16) and $FF) shl 8) or (((Source shr 8) and $FF) shl 16) or
        ((Source and $FF) shl 24);

    TPixelFormat.B8G8R8X8:
      Value := (((Source shr 16) and $FF) shl 8) or (((Source shr 8) and $FF) shl 16) or ((Source and $FF) shl 24);

  else
    begin
      Info := @PixelFormatInfo[DestFormat];

      // -> Blue Component
      if Info.Bb > 0 then
        Value := ((Source and $FF) shr (8 - Info.Bb)) shl Info.Bp;

      if Info.Gb > 0 then
        Value := Value or (((Source shr 8) and $FF) shr (8 - Info.Gb)) shl Info.Gp;

      if Info.Rb > 0 then
        Value := Value or (((Source shr 16) and $FF) shr (8 - Info.Bb)) shl Info.Rp;

      if Info.Ab > 0 then
        Value := Value or (((Source shr 24) and $FF) shr (8 - Info.Ab)) shl Info.Ap;
    end;
  end;

  Move(Value, Dest^, Bits div 8);
end;

procedure PixelXTo32Array(const Source: Pointer; const Dest: PIntColor; const SourceFormat: TPixelFormat;
  const ElementCount: Integer);
var
  SourceValue: Pointer;
  DestValue: PIntColor;
  I, Bits, BytesPerPixel: Integer;
begin
  Bits := SourceFormat.Bits;
  if Bits < 8 then
    Exit;

  BytesPerPixel := Bits div 8;

  SourceValue := Source;
  DestValue := Dest;

  case SourceFormat of
    TPixelFormat.A8R8G8B8:
      Move(Source^, Dest^, ElementCount * SizeOf(TIntColor));

    TPixelFormat.X8R8G8B8:
      for I := 0 to ElementCount - 1 do
      begin
        DestValue^ := PIntColor(SourceValue)^ or $FF000000;

        Inc(PtrUInt(SourceValue), SizeOf(TIntColor));
        Inc(DestValue);
      end;

    TPixelFormat.A8B8G8R8:
      for I := 0 to ElementCount - 1 do
      begin
        DestValue^ := DisplaceRB(PIntColor(SourceValue)^) or $FF000000;

        Inc(PtrUInt(SourceValue), SizeOf(TIntColor));
        Inc(DestValue);
      end;

    TPixelFormat.X8B8G8R8:
      for I := 0 to ElementCount - 1 do
      begin
        DestValue^ := DisplaceRB(PIntColor(SourceValue)^) or $FF000000;

        Inc(PtrUInt(SourceValue), SizeOf(TIntColor));
        Inc(DestValue);
      end;

    TPixelFormat.R8G8B8:
      for I := 0 to ElementCount - 1 do
      begin
        Move(SourceValue^, DestValue^, BytesPerPixel);
        DestValue^ := DestValue^ or $FF000000;

        Inc(PtrUInt(SourceValue), Cardinal(BytesPerPixel));
        Inc(DestValue);
      end;

    else
      for I := 0 to ElementCount - 1 do
      begin
        DestValue^ := PixelXto32(SourceValue, SourceFormat);

        Inc(PtrUInt(SourceValue), Cardinal(BytesPerPixel));
        Inc(DestValue);
      end;
  end;
end;

procedure Pixel32ToXArray(const Source: PIntColor; const Dest: Pointer; const DestFormat: TPixelFormat;
  const ElementCount: Integer);
var
  SourceValue: PIntColor;
  DestValue: Pointer;
  I, Bits, BytesPerPixel: Integer;
begin
  Bits := DestFormat.Bits;
  if Bits < 8 then
    Exit;

  BytesPerPixel := Bits div 8;

  SourceValue := Source;
  DestValue := Dest;

  case DestFormat of
    TPixelFormat.A8R8G8B8:
      Move(Source^, Dest^, ElementCount * SizeOf(TIntColor));

    TPixelFormat.X8R8G8B8:
      for I := 0 to ElementCount - 1 do
      begin
        PIntColor(DestValue)^ := SourceValue^ and $00FFFFFF;

        Inc(SourceValue);
        Inc(PtrUInt(DestValue), SizeOf(TIntColor));
      end;

    TPixelFormat.A8B8G8R8:
      for I := 0 to ElementCount - 1 do
      begin
        PIntColor(DestValue)^ := DisplaceRB(SourceValue^);

        Inc(SourceValue);
        Inc(PtrUInt(DestValue), SizeOf(TIntColor));
      end;

    TPixelFormat.X8B8G8R8:
      for I := 0 to ElementCount - 1 do
      begin
        PIntColor(DestValue)^ := DisplaceRB(SourceValue^) and $00FFFFFF;

        Inc(SourceValue);
        Inc(PtrUInt(DestValue), SizeOf(TIntColor));
      end;

    TPixelFormat.R8G8B8:
      for I := 0 to ElementCount - 1 do
      begin
        Move(SourceValue^, DestValue^, BytesPerPixel);

        Inc(SourceValue);
        Inc(PtrUInt(DestValue), Cardinal(BytesPerPixel));
      end;

    else
      for I := 0 to ElementCount - 1 do
      begin
        Pixel32toX(SourceValue^, DestValue, DestFormat);

        Inc(SourceValue);
        Inc(PtrUInt(DestValue), Cardinal(BytesPerPixel));
      end;
  end;
end;

function GetChannelNegativeDistance(const SampleFormat, ReqFormat: TPixelFormat): Integer;

  function ComputeDifference(const SampleBits, ReqBits: Integer): Integer;
  begin
    if (SampleBits > 0) and (ReqBits > 0) and (SampleBits < ReqBits) then
      Result := Sqr(ReqBits - SampleBits)
    else
      Result := 0;
  end;

var
  SampleInfo, ReqInfo: PPixelFormatInfo;
begin
  SampleInfo := @PixelFormatInfo[SampleFormat];
  ReqInfo := @PixelFormatInfo[ReqFormat];

  if SampleInfo.Dt <> ReqInfo.Dt then
    Exit(0);

  if SampleInfo.Dt <= 1 then
  begin // Unsigned / Float
    Result := ComputeDifference(SampleInfo.Rb, ReqInfo.Rb) + ComputeDifference(SampleInfo.Gb, ReqInfo.Gb) +
      ComputeDifference(SampleInfo.Bb, ReqInfo.Bb) + ComputeDifference(SampleInfo.Ab, ReqInfo.Ab);
  end
  else if SampleInfo.Dt = 2 then
  begin // Luminance
    Result := ComputeDifference(SampleInfo.Lb, ReqInfo.Lb) + ComputeDifference(SampleInfo.Ab, ReqInfo.Ab);
  end
  else
    Result := 0;
end;

function GetChannelDistance(const Format1, Format2: TPixelFormat): Integer;

  function ComputeDifference(const ChannelBits1, ChannelBits2: Integer): Integer;
  begin
    if (ChannelBits1 > 0) and (ChannelBits2 > 0) then
      Result := Sqr(ChannelBits2 - ChannelBits1)
    else
      Result := 0;
  end;

var
  Info1, Info2: PPixelFormatInfo;
begin
  Info1 := @PixelFormatInfo[Format1];
  Info2 := @PixelFormatInfo[Format2];

  if Info1.Dt <> Info2.Dt then
    Exit(0);

  if Info1.Dt <= 1 then
  begin // Unsigned / Float
    Result := ComputeDifference(Info1.Rb, Info2.Rb) + ComputeDifference(Info1.Gb, Info2.Gb) +
      ComputeDifference(Info1.Bb, Info2.Bb) + ComputeDifference(Info1.Ab, Info2.Ab);
  end
  else if Info1.Dt = 2 then
  begin // Luminance
    Result := ComputeDifference(Info1.Lb, Info2.Lb) + ComputeDifference(Info1.Ab, Info2.Ab);
  end
  else
    Result := 0;
end;

function GetChannelExtraBits(const SampleFormat, ReqFormat: TPixelFormat): Integer;
var
  SampleInfo, ReqInfo: PPixelFormatInfo;
begin
  SampleInfo := @PixelFormatInfo[SampleFormat];
  ReqInfo := @PixelFormatInfo[ReqFormat];

  Result := 0;

  if SampleInfo.Dt <> ReqInfo.Dt then
    Exit;

  // Unsigned and has more extra/unused bits?
  if (SampleInfo.Dt = 0) and (SampleInfo.Lb > ReqInfo.Lb) then
    Inc(Result, SampleInfo.Lb - ReqInfo.Lb);

  if SampleInfo.Dt <= 1 then
  begin // Unsigned / Float
    if (SampleInfo.Rb > 0) and (ReqInfo.Rb < 1) then
      Inc(Result, SampleInfo.Rb);

    if (SampleInfo.Gb > 0) and (ReqInfo.Gb < 1) then
      Inc(Result, SampleInfo.Gb);

    if (SampleInfo.Bb > 0) and (ReqInfo.Bb < 1) then
      Inc(Result, SampleInfo.Bb);

    if (SampleInfo.Ab > 0) and (ReqInfo.Ab < 1) then
      Inc(Result, SampleInfo.Ab);
  end
  else if SampleInfo.Dt = 2 then
  begin // Luminance
    if (SampleInfo.Lb > 0) and (ReqInfo.Lb < 1) then
      Inc(Result, SampleInfo.Lb);

    if (SampleInfo.Ab > 0) and (ReqInfo.Ab < 1) then
      Inc(Result, SampleInfo.Ab);
  end;
end;

function GetChannelPosDistance(const Format1, Format2: TPixelFormat): Integer;
var
  Info1, Info2: PPixelFormatInfo;
begin
  Info1 := @PixelFormatInfo[Format1];
  Info2 := @PixelFormatInfo[Format2];

  if Info1.Dt <> Info2.Dt then
    Exit(0);

  Result := 0;

  if Info1.Dt <= 1 then
  begin // Unsigned / Float
    if (Info1.Rb > 0) and (Info2.Rb > 0) then
      Inc(Result, Sqr(Integer(Info2.Rp) - Info1.Rp));

    if (Info1.Gb > 0) and (Info2.Gb > 0) then
      Inc(Result, Sqr(Integer(Info2.Gp) - Info1.Gp));

    if (Info1.Bb > 0) and (Info2.Bb > 0) then
      Inc(Result, Sqr(Integer(Info2.Bp) - Info1.Bp));

    if (Info1.Ab > 0) and (Info2.Ab > 0) then
      Inc(Result, Sqr(Integer(Info2.Ap) - Info1.Ap));
  end
  else if Info1.Dt = 2 then
  begin // Luminance
    if (Info1.Lb > 0) and (Info2.Lb > 0) then
      Inc(Result, Sqr(Integer(Info2.Lp) - Info1.Lp));

    if (Info1.Ab > 0) and (Info2.Ab > 0) then
      Inc(Result, Sqr(Integer(Info2.Ap) - Info1.Ap));
  end;
end;

function GetChannelCount(const Format: TPixelFormat): Integer;
var
  Info: PPixelFormatInfo;
begin
  Info := @PixelFormatInfo[Format];

  Result := 0;

  if Info.Rb > 0 then
    Inc(Result);

  if Info.Gb > 0 then
    Inc(Result);

  if Info.Bb > 0 then
    Inc(Result);

  if Info.Ab > 0 then
    Inc(Result);

  if (Info.Dt = 2) and (Info.Lb > 0) then
    Inc(Result);
end;

function CanAcceptFormat(const SampleFormat, ReqFormat: TPixelFormat): Boolean;
var
  SampleInfo, ReqInfo: PPixelFormatInfo;
begin
  SampleInfo := @PixelFormatInfo[SampleFormat];
  ReqInfo := @PixelFormatInfo[ReqFormat];

  if SampleInfo.Dt <> ReqInfo.Dt then
    Exit(False);

  if (ReqInfo.Rb > 0) and (SampleInfo.Rb < 1) then
    Exit(False);

  if (ReqInfo.Gb > 0) and (SampleInfo.Gb < 1) then
    Exit(False);

  if (ReqInfo.Bb > 0) and (SampleInfo.Bb < 1) then
    Exit(False);

  if (ReqInfo.Ab > 0) and (SampleInfo.Ab < 1) then
    Exit(False);

  if (ReqInfo.Dt = 2) and (ReqInfo.Lb > 0) and (SampleInfo.Lb < 1) then
    Exit(False);

  Result := True;
end;

class function TPixelFormatHelper.CreateFromString(const Text: StdString): TPixelFormat;
var
  TempText: StdString;
  Sample: TPixelFormat;
begin
  TempText := Trim(UpperCase(Text));

  if Pos('APF_', TempText) = 1 then
    Delete(TempText, 1, 4)
  else if Pos('D3DFMT_', TempText) = 1 then
    Delete(TempText, 1, 7)
  else if Pos('COLOR_', TempText) = 1 then
    Delete(TempText, 1, 6)
  else if Pos('PF', TempText) = 1 then
    Delete(TempText, 1, 2);

  for Sample := Low(TPixelFormat) to High(TPixelFormat) do
    if SameText(TempText, Sample.ToString) then
      Exit(Sample);

  Result := TPixelFormat.Unknown;
end;

function TPixelFormatHelper.GetBits: Integer;
begin
  if (Self >= Low(TPixelFormat)) and (Self <= High(TPixelFormat)) then
    Result := PixelFormatInfo[Self].Bi
  else
    Result := 0;
end;

function TPixelFormatHelper.GetBytes: Integer;
begin
  Result := GetBits div 8;
end;

function TPixelFormatHelper.HasAlphaChannel: Boolean;
begin
  Result := PixelFormatInfo[Self].Ab > 0;
end;

function TPixelFormatHelper.ToString: StdString;
begin
  if (Self >= Low(TPixelFormat)) and (Self <= High(TPixelFormat)) then
    Result := PixelFormatNames[Self]
  else
    Result := '';
end;

function TPixelFormatHelper.CanBulkCopyTo(const DestFormat: TPixelFormat): Boolean;
begin
  Result := (Self = DestFormat) or
    ((Self = TPixelFormat.A8R8G8B8) and (DestFormat = TPixelFormat.X8R8G8B8)) or
    ((Self = TPixelFormat.A8B8G8R8) and (DestFormat = TPixelFormat.X8B8G8R8)) or
    ((Self = TPixelFormat.A4R4G4B4) and (DestFormat = TPixelFormat.X4R4G4B4)) or
    ((Self = TPixelFormat.A1R5G5B5) and (DestFormat = TPixelFormat.X1R5G5B5)) or
    ((Self = TPixelFormat.L8) and (DestFormat = TPixelFormat.I8)) or
    ((Self = TPixelFormat.I8) and (DestFormat = TPixelFormat.L8));
end;

constructor TPixelFormatList.TEnumerator.Create(const AList: TPixelFormatList);
begin
  inherited Create;

  Inc(PXL_ClassInstances);

  FList := AList;
  Index := -1;
end;

destructor TPixelFormatList.TEnumerator.Destroy;
begin
  Dec(PXL_ClassInstances);

  inherited;
end;

function TPixelFormatList.TEnumerator.GetCurrent: TPixelFormat;
begin
  Result := FList[Index];
end;

function TPixelFormatList.TEnumerator.MoveNext: Boolean;
begin
  Result := Index < FList.Count - 1;

  if Result then
    Inc(Index);
end;

constructor TPixelFormatList.Create;
begin
  inherited;

  Inc(PXL_ClassInstances);
  DataCount := 0;
end;

destructor TPixelFormatList.Destroy;
begin
  Dec(PXL_ClassInstances);

  DataCount := 0;
  SetLength(Data, 0);

  inherited;
end;

function TPixelFormatList.GetCount: Integer;
begin
  Result := DataCount;
end;

procedure TPixelFormatList.SetCount(const NewCount: Integer);
begin
  if NewCount > 0 then
  begin
    Request(NewCount);
    DataCount := NewCount;
  end
  else
    Clear;
end;

function TPixelFormatList.GetItem(const Index: Integer): TPixelFormat;
begin
  if (Index >= 0) and (Index < DataCount) then
    Result := Data[Index]
  else
    Result := TPixelFormat.Unknown;
end;

procedure TPixelFormatList.SetItem(const Index: Integer; const Value: TPixelFormat);
begin
  if (Index >= 0) and (Index < DataCount) then
    Data[Index] := Value;
end;

procedure TPixelFormatList.Request(const NeedCapacity: Integer);
var
  NewCapacity, Capacity: Integer;
begin
  if NeedCapacity < 1 then
    Exit;

  Capacity := Length(Data);

  if Capacity < NeedCapacity then
  begin
    NewCapacity := ListGrowIncrement + Capacity + (Capacity div ListGrowFraction);

    if NewCapacity < NeedCapacity then
      NewCapacity := ListGrowIncrement + NeedCapacity + (NeedCapacity div ListGrowFraction);

    SetLength(Data, NewCapacity);
  end;
end;

function TPixelFormatList.Insert(const Format: TPixelFormat): Integer;
var
  Index: Integer;
begin
  Index := DataCount;
  Request(DataCount + 1);

  Data[Index] := Format;
  Inc(DataCount);

  Result := Index;
end;

procedure TPixelFormatList.InsertAll;
var
  Format: TPixelFormat;
begin
  Clear;

  for Format := Low(TPixelFormat) to High(TPixelFormat) do
    if Format <> TPixelFormat.Unknown then
      Insert(Format);
end;

function TPixelFormatList.IndexOf(const Format: TPixelFormat): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to DataCount - 1 do
    if Data[I] = Format then
    begin
      Result := I;
      Break;
    end;
end;

function TPixelFormatList.Include(const Format: TPixelFormat): Integer;
begin
  Result := IndexOf(Format);

  if Result = -1 then
    Result := Insert(Format);
end;

procedure TPixelFormatList.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= DataCount) then
    Exit;

  for I := Index to DataCount - 2 do
    Data[I] := Data[I + 1];

  Dec(DataCount);
end;

procedure TPixelFormatList.Clear;
begin
  DataCount := 0;
end;

procedure TPixelFormatList.ListSwap(const Index1, Index2: Integer);
var
  TempValue: TPixelFormat;
begin
  TempValue := Data[Index1];
  Data[Index1] := Data[Index2];
  Data[Index2] := TempValue;
end;

function TPixelFormatList.ListCompare(const Format1, Format2: TPixelFormat): Integer;
var
  Delta1, Delta2: Integer;
begin
  Delta1 := GetChannelNegativeDistance(Format1, SortSampleFormat);
  Delta2 := GetChannelNegativeDistance(Format2, SortSampleFormat);

  if Delta1 = Delta2 then
  begin
    Delta1 := GetChannelDistance(Format1, SortSampleFormat);
    Delta2 := GetChannelDistance(Format2, SortSampleFormat);

    if Delta1 = Delta2 then
    begin
      Delta1 := GetChannelExtraBits(Format1, SortSampleFormat);
      Delta2 := GetChannelExtraBits(Format2, SortSampleFormat);

      if Delta1 = Delta2 then
      begin
        Delta1 := GetChannelPosDistance(Format1, SortSampleFormat);
        Delta2 := GetChannelPosDistance(Format2, SortSampleFormat);

        if Delta1 = Delta2 then
        begin
          Delta1 := GetChannelCount(Format1);
          Delta2 := GetChannelCount(Format2);

          if Delta1 = Delta2 then
          begin
            Delta1 := Abs(Format1.Bits - SortSampleFormat.Bits);
            Delta2 := Abs(Format2.Bits - SortSampleFormat.Bits);
          end;
        end;
      end;
    end;
  end;

  if Delta1 > Delta2 then
    Result := 1
  else if Delta1 < Delta2 then
    Result := -1
  else
    Result := 0;
end;

function TPixelFormatList.ListSplit(const Start, Stop: Integer): Integer;
var
  Left, Right: Integer;
  Pivot: TPixelFormat;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := Data[Start];

  while Left <= Right do
  begin
    while (Left <= Stop) and (ListCompare(Data[Left], Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (ListCompare(Data[Right], Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      ListSwap(Left, Right);
  end;

  ListSwap(Start, Right);

  Result := Right;
end;

procedure TPixelFormatList.ListSort(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := ListSplit(Start, Stop);

    ListSort(Start, SplitPt - 1);
    ListSort(SplitPt + 1, Stop);
  end;
end;

procedure TPixelFormatList.SortBestMatch(const Format: TPixelFormat);
begin
  SortSampleFormat := Format;

  if DataCount > 1 then
    ListSort(0, DataCount - 1);
end;

function TPixelFormatList.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function FindClosestPixelFormat(const Format: TPixelFormat; const ExistingFormats: TPixelFormatList): TPixelFormat;

  function FindInternal(const Format: TPixelFormat; const ExistingFormats: TPixelFormatList): TPixelFormat;
  var
    Accepted: TPixelFormatList;
    Sample: TPixelFormat;
  begin
    Accepted := TPixelFormatList.Create;

    try
      for Sample in ExistingFormats do
        if CanAcceptFormat(Sample, Format) then
          Accepted.Insert(Sample);

      Accepted.SortBestMatch(Format);

      if Accepted.Count > 0 then
        Result := Accepted[0]
      else
        Result := TPixelFormat.Unknown;
    finally
      Accepted.Free;
    end;
  end;

begin
  Result := FindInternal(Format, ExistingFormats);

{$IFDEF CONVERT_LUMINANCE_TO_RGB}
  if Result = TPixelFormat.Unknown then
    case Format of
      TPixelFormat.L16:
        Result:= FindInternal(TPixelFormat.A16B16G16R16, ExistingFormats);

      TPixelFormat.L8:
        Result:= FindInternal(TPixelFormat.R8G8B8, ExistingFormats);

      TPixelFormat.A8L8:
        Result:= FindInternal(TPixelFormat.A8R8G8B8, ExistingFormats);

      TPixelFormat.A4L4:
        Result:= FindInternal(TPixelFormat.A4R4G4B4, ExistingFormats);
    end;
{$ENDIF}
end;

function GetPixelFormatDescription(const Format: TPixelFormat; out Description: TPixelFormatDescription): Boolean;
begin
  if (Format < Low(TPixelFormat)) or (Format > High(TPixelFormat)) then
    Exit(False);

  Description.FormatType := TPixelFormatType(PixelFormatInfo[Format].Dt);
  Description.BitCount := PixelFormatInfo[Format].Bi;
  Description.UsedBitCount := PixelFormatInfo[Format].Rb + PixelFormatInfo[Format].Gb + PixelFormatInfo[Format].Bb +
    PixelFormatInfo[Format].Ab;

  Description.RedBits.Count := PixelFormatInfo[Format].Rb;
  Description.GreenBits.Count := PixelFormatInfo[Format].Gb;
  Description.BlueBits.Count := PixelFormatInfo[Format].Bb;
  Description.AlphaBits.Count := PixelFormatInfo[Format].Ab;
  Description.LuminanceBits.Count := PixelFormatInfo[Format].Lb;

  if PixelFormatInfo[Format].Rp <> 255 then
    Description.RedBits.Shift := PixelFormatInfo[Format].Rp
  else
    Description.RedBits.Shift := 0;

  if PixelFormatInfo[Format].Gp <> 255 then
    Description.GreenBits.Shift := PixelFormatInfo[Format].Gp
  else
    Description.GreenBits.Shift := 0;

  if PixelFormatInfo[Format].Bp <> 255 then
    Description.BlueBits.Shift := PixelFormatInfo[Format].Bp
  else
    Description.BlueBits.Shift := 0;

  if PixelFormatInfo[Format].Ap <> 255 then
    Description.AlphaBits.Shift := PixelFormatInfo[Format].Ap
  else
    Description.AlphaBits.Shift := 0;

  if PixelFormatInfo[Format].Lp <> 255 then
    Description.LuminanceBits.Shift := PixelFormatInfo[Format].Lp
  else
    Description.LuminanceBits.Shift := 0;

  Result := True;
end;

function GetMatchingPixelFormat(const Description: TPixelFormatDescription): TPixelFormat;
var
  Format: TPixelFormat;
  FormatDesc: TPixelFormatDescription;
begin
  for Format := Low(TPixelFormat) to High(TPixelFormat) do
    if GetPixelFormatDescription(Format, FormatDesc) and (FormatDesc = Description) then
      Exit(Format);

  Result := TPixelFormat.Unknown;
end;

end.
