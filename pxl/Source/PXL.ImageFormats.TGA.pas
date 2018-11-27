unit PXL.ImageFormats.TGA;
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
interface

{$INCLUDE PXL.Config.inc}

uses
  Classes, PXL.TypeDef, PXL.Types, PXL.Surfaces, PXL.ImageFormats;

const
  TargaQualityUncompressed: Pointer = nil;
  TargaQualityCompressed: Pointer = Pointer(1);

  MaxRLEBlockCount = 128;

type
  TTargaHeader = packed record
    IdLength: Byte;
    ColorMapType: Byte;
    ImageType: Byte;
    ColorMapSpec: packed array[0..4] of Byte;
    OriginX: Word;
    OriginY: Word;
    Width: Word;
    Height: Word;
    BitDepth: Byte;
    ImageDesc: Byte;
  end;

  TElementCount = {$IFDEF MSDOS}LongInt{$ELSE}Integer{$ENDIF};

procedure DecodeTargaRLE(const Stream: TStream; const Dest: Pointer; const ElementCount: TElementCount;
  const BytesPerPixel: Integer);

function DecodeTargaStream(const Stream: TStream; const DestSurface: TPixelSurface): Boolean;

function PixelScanRLE(const Source: PIntColor; const RemainingElements: TElementCount;
  var PatternCount: Integer): Boolean;

procedure EncodeTargaRLE(const Stream: TStream; const Source: PIntColor; const ElementCount: TElementCount;
  const BytesPerPixel: Integer);

procedure EncodeTargaStream(const Stream: TStream; const SourceSurface: TPixelSurface; const HasAlphaChannel: Boolean;
  const Quality: Pointer);

type
  TTargaImageFormatHandler = class(TCustomImageFormatHandler)
  protected
    procedure RegisterExtensions; override;
  public
    function LoadFromStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
      const DestSurface: TPixelSurface; const AlphaFormatRequest: TAlphaFormatRequest): Boolean; override;
    function SaveToStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
      const SourceSurface: TPixelSurface; const Quality: Pointer): Boolean; override;
  end;

implementation

uses
  PXL.Classes, PXL.Formats;

{$REGION 'Global Functions'}

procedure DecodeTargaRLE(const Stream: TStream; const Dest: Pointer; const ElementCount: TElementCount;
  const BytesPerPixel: Integer);
var
  DestPixel: Pointer;
  ElementsRemaining: TElementCount;
  RunLengthHeader, BlockLength: Integer;
  SourcePixel: TIntColor;
begin
  ElementsRemaining := ElementCount;
  DestPixel := Dest;
  SourcePixel := IntColorWhite;

  while ElementsRemaining > 0 do
  begin
    RunLengthHeader := Stream.GetByte;
    BlockLength := (RunLengthHeader and $7F) + 1;

    if RunLengthHeader and $80 > 0 then
    begin // Highest bit is set, read one pixel and repeat BlockLength times.
      Stream.ReadBuffer(SourcePixel, BytesPerPixel);

      while BlockLength > 0 do
      begin
        Move(SourcePixel, DestPixel^, BytesPerPixel);

        Inc(PtrUInt(DestPixel), Cardinal(BytesPerPixel));
        Dec(ElementsRemaining);
        Dec(BlockLength);
      end;
    end
    else
    begin // Highest bit is not set, just copy next BlockLength elements.
      while BlockLength > 0 do
      begin
        Stream.ReadBuffer(DestPixel^, BytesPerPixel);

        Inc(PtrUInt(DestPixel), Cardinal(BytesPerPixel));
        Dec(ElementsRemaining);
        Dec(BlockLength);
      end;
    end;
  end;
end;

function DecodeTargaStream(const Stream: TStream; const DestSurface: TPixelSurface): Boolean;
var
  Header: TTargaHeader;
  I: TElementCount;
  BytesPerPixel: Integer;
  DestPixel: Pointer;
begin
  Stream.ReadBuffer(Header, SizeOf(TTargaHeader));

  // Image must be either True Color or RLE encoded.
  if ((Header.ImageType <> 2) and (Header.ImageType <> 10)) or (Header.ColorMapType <> 0) then
    Exit(False);

  // Image must have non-zero size.
  if (Header.Width < 1) or (Header.Height < 1) then
    Exit(False);

  // Image must have either 24 or 32 bits per pixel.
  BytesPerPixel := Header.BitDepth div 8;
  if (BytesPerPixel < 3) or (BytesPerPixel > 4) then
    Exit(False);

  // Skip Image ID field.
  if Header.IdLength <> 0 then
    Stream.Seek(Header.IdLength, soFromCurrent);

  if BytesPerPixel = 3 then
    DestSurface.SetSize(Header.Width, Header.Height, TPixelFormat.R8G8B8)
  else
    DestSurface.SetSize(Header.Width, Header.Height, TPixelFormat.A8R8G8B8);

  if Header.ImageType <> 10 then
  begin
    DestPixel := DestSurface.Bits;

    for I := 0 to (DestSurface.Width * DestSurface.Height) - 1 do
    begin
      Stream.ReadBuffer(DestPixel^, BytesPerPixel);
      Inc(PtrUInt(DestPixel), Cardinal(BytesPerPixel));
    end;
  end
  else // RLE encoded
    DecodeTargaRLE(Stream, DestSurface.Bits, DestSurface.Width * DestSurface.Height, BytesPerPixel);

  if Header.ImageDesc and $10 > 0 then
    DestSurface.Mirror;

  if Header.ImageDesc and $20 = 0 then
    DestSurface.Flip;

  Result := True;
end;

function PixelScanRLE(const Source: PIntColor; const RemainingElements: TElementCount;
  var PatternCount: Integer): Boolean;
type
  PPixel3 = ^TPixel3;
  TPixel3 = array[0..2] of TIntColor;
var
  SourcePixels: PPixel3;
  SourcePixel: PIntColor;
begin
  if RemainingElements < 3 then
  begin
    PatternCount := RemainingElements;
    Exit(False);
  end;

  SourcePixels := PPixel3(Source);

  if (SourcePixels[0] = SourcePixels[1]) and (SourcePixels[1] = SourcePixels[2]) then
  begin // Repeating pixels.
    PatternCount := 3;

    SourcePixel := Pointer(PtrUInt(Source) + SizeOf(TIntColor) * 3);

    while (PatternCount < RemainingElements) and (PatternCount < MaxRLEBlockCount) and
      (SourcePixel^ = SourcePixels[0]) do
    begin
      Inc(PatternCount);
      Inc(SourcePixel);
    end;

    Result := True;
  end
  else
  begin // Non-repeating pixels.
    PatternCount := 2;
    SourcePixels := PPixel3(PtrUInt(Source) + SizeOf(TIntColor) * 2);

    // Find next chain of three repeating pixels.
    while (PatternCount < RemainingElements) and (PatternCount < MaxRLEBlockCount) do
    begin
      if (SourcePixels[0] = SourcePixels[1]) and (SourcePixels[1] = SourcePixels[2]) then
        Break;

      Inc(PatternCount);
      Inc(PtrUInt(SourcePixels), SizeOf(TIntColor));
    end;

    Result := False;
  end;
end;

procedure EncodeTargaRLE(const Stream: TStream; const Source: PIntColor; const ElementCount: TElementCount;
  const BytesPerPixel: Integer);
var
  SourcePixel: PIntColor;
  RemainingElements: TElementCount;
  I, PatternCount, RunLengthHeader: Integer;
  Repeated: Boolean;
begin
  RemainingElements := ElementCount;
  SourcePixel := Source;

  while RemainingElements > 0 do
  begin
    Repeated := PixelScanRLE(SourcePixel, RemainingElements, PatternCount);
    RunLengthHeader := (PatternCount - 1) and $7F;

    if Repeated then
    begin
      RunLengthHeader := RunLengthHeader or $80; // set RLE bit

      Stream.PutByte(RunLengthHeader);
      Stream.WriteBuffer(SourcePixel^, BytesPerPixel);

      Inc(SourcePixel, PatternCount);
    end
    else
    begin
      Stream.PutByte(RunLengthHeader);

      for I := 0 to PatternCount - 1 do
      begin
        Stream.WriteBuffer(SourcePixel^, BytesPerPixel);
        Inc(SourcePixel);
      end;
    end;

    Dec(RemainingElements, PatternCount);
  end;
end;

procedure EncodeTargaStream(const Stream: TStream; const SourceSurface: TPixelSurface; const HasAlphaChannel: Boolean;
  const Quality: Pointer);
var
  Header: TTargaHeader;
  SourcePixel: PIntColor;
  I: TElementCount;
  BytesPerPixel: Integer;
begin
  FillChar(Header, SizeOf(TTargaHeader), 0);

  if not HasAlphaChannel then
    BytesPerPixel := 3
  else
    BytesPerPixel := 4;

  Header.BitDepth := BytesPerPixel * 8;
  Header.Width := SourceSurface.Width;
  Header.Height := SourceSurface.Height;

  Header.ImageType := 2; // TrueColor image.

  if Quality = TargaQualityCompressed then
    Header.ImageType := 10; // RLE encoded.

  Header.ImageDesc := $20; // Non-flipped, non-mirrored image.

  Stream.WriteBuffer(Header, SizeOf(TTargaHeader));

  if Quality = TargaQualityCompressed then
    EncodeTargaRLE(Stream, SourceSurface.Bits, SourceSurface.Width * SourceSurface.Height, BytesPerPixel)
  else
    begin // uncompressed data
      SourcePixel := SourceSurface.Bits;

      for I := 0 to (SourceSurface.Width * SourceSurface.Height) - 1 do
      begin
        Stream.WriteBuffer(SourcePixel^, BytesPerPixel);
        Inc(SourcePixel);
      end;
    end;
end;

{$ENDREGION}
{$REGION 'TTargaImageFormatHandler'}

procedure TTargaImageFormatHandler.RegisterExtensions;
begin
  RegisterExtension('.tga');
end;

function TTargaImageFormatHandler.LoadFromStream(const Context: Pointer; const Extension: StdString;
  const Stream: TStream; const DestSurface: TPixelSurface; const AlphaFormatRequest: TAlphaFormatRequest): Boolean;
begin
  try
    if not DecodeTargaStream(Stream, DestSurface) then
      Exit(False);
  except
    Exit(False);
  end;

  if AlphaFormatRequest = TAlphaFormatRequest.Premultiplied then
  begin
    if DestSurface.PixelFormat.HasAlphaChannel and DestSurface.HasAlphaChannel then
      DestSurface.PremultiplyAlpha;

    DestSurface.PremultipliedAlpha := True;
  end;

  Result := True;
end;

function TTargaImageFormatHandler.SaveToStream(const Context: Pointer; const Extension: StdString;
  const Stream: TStream; const SourceSurface: TPixelSurface; const Quality: Pointer): Boolean;
var
  TempSurface: TPixelSurface;
  HasAlphaChannel: Boolean;
begin
  if (SourceSurface.PixelFormat <> TPixelFormat.A8R8G8B8) or SourceSurface.PremultipliedAlpha then
  begin
    TempSurface := TPixelSurface.Create;
    TempSurface.CopyFrom(SourceSurface);

    if TempSurface.PixelFormat <> TPixelFormat.A8R8G8B8 then
      TempSurface.ConvertPixelFormat(TPixelFormat.A8R8G8B8);

    HasAlphaChannel := TempSurface.HasAlphaChannel;

    if TempSurface.PremultipliedAlpha then
    begin
      if HasAlphaChannel then
        TempSurface.UnpremultiplyAlpha;

      TempSurface.PremultipliedAlpha := False;
    end;
  end
  else
  begin
    TempSurface := SourceSurface;
    HasAlphaChannel := TempSurface.HasAlphaChannel;
  end;

  try
    try
      EncodeTargaStream(Stream, TempSurface, HasAlphaChannel, Quality);
    except
      Exit(False);
    end;

    Result := True;
  finally
    if TempSurface <> SourceSurface then
      TempSurface.Free;
  end;
end;

{$ENDREGION}

end.
