unit PXL.Archives.Loading;
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
  PXL.TypeDef, PXL.Types, PXL.Archives, PXL.Images, PXL.Fonts;

function LoadImageFromArchive(const Key: UniString; const Images: TAtlasImages; const Archive: TArchive;
  const MipMapping: Boolean = False; const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Integer;

function LoadFontFromArchive(const Key: UniString; const Fonts: TBitmapFonts; const Archive: TArchive;
  const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Integer;

implementation

uses
  SysUtils, Classes, PXL.Classes, PXL.Formats, PXL.Textures;

{$REGION 'RAW pixels conversion'}

function ReadPixelsRaw(const Stream: TStream; const Texture: TCustomLockableTexture): Boolean;
var
  LockedPixels: TLockedPixels;
  ScanlineBytes, I: Integer;
begin
  if not Texture.Lock(LockedPixels) then
    Exit(False);
  try
    ScanlineBytes := Texture.Width * SizeOf(TIntColor);

    for I := 0 to Texture.Height - 1 do
    begin
      Result := Stream.Read(LockedPixels.Scanline[I]^, ScanlineBytes) = ScanlineBytes;
      if not Result then
        Break;
    end;
  finally
    Texture.Unlock;
  end;
end;

function ReadPixels32toX(const Stream: TStream; const Texture: TCustomLockableTexture): Boolean;
var
  LockedPixels: TLockedPixels;
  SourcePixels: Pointer;
  SourceSize, I: Integer;
begin
  if not Texture.Lock(LockedPixels) then
    Exit(False);
  try
    SourceSize := Texture.Width * SizeOf(TIntColor);
    GetMem(SourcePixels, SourceSize);
    try
      for I := 0 to Texture.Height - 1 do
      begin
        Result := Stream.Read(SourcePixels^, SourceSize) = SourceSize;
        if not Result then
          Break;

        Pixel32toXArray(SourcePixels, LockedPixels.Scanline[I], Texture.PixelFormat, Texture.Width);
      end;
    finally
      FreeMem(SourcePixels);
    end;
  finally
    Texture.Unlock;
  end;
end;

function ReadPixelsXToX(const Stream: TStream; const Texture: TCustomLockableTexture;
  const SourceFormat: TPixelFormat): Boolean;
var
  LockedPixels: TLockedPixels;
  SourcePixels, RawPixels: Pointer;
  SourceSize, RawSize, I: Integer;
begin
  if not Texture.Lock(LockedPixels) then
    Exit(False);
  try
    SourceSize := Texture.Width * SourceFormat.Bytes;
    GetMem(SourcePixels, SourceSize);
    try
      RawSize := Texture.Width * SizeOf(TIntColor);
      GetMem(RawPixels, RawSize);
      try
        for I := 0 to Texture.Height - 1 do
        begin
          Result := Stream.Read(SourcePixels^, SourceSize) = SourceSize;
          if not Result then
            Break;

          PixelXto32Array(SourcePixels, RawPixels, SourceFormat, Texture.Width);
          Pixel32toXArray(RawPixels, LockedPixels.Scanline[I], Texture.PixelFormat, Texture.Width);
        end;
      finally
        FreeMem(RawPixels);
      end;
    finally
      FreeMem(SourcePixels);
    end;
  finally
    Texture.Unlock;
  end;
end;

{$ENDREGION}
{$REGION 'ASDb (VTDb) image loading'}

function ASDbToPixelFormat(const Format: Integer): TPixelFormat;
begin
  if Format and $80 > 0 then
    case Format and $7F of
      1: Result := TPixelFormat.R8G8B8;
      2: Result := TPixelFormat.A8R8G8B8;
      3: Result := TPixelFormat.X8R8G8B8;
      4: Result := TPixelFormat.R5G6B5;
      5: Result := TPixelFormat.X1R5G5B5;
      6: Result := TPixelFormat.A1R5G5B5;
      7: Result := TPixelFormat.A4R4G4B4;
      8: Result := TPixelFormat.R3G3B2;
      9: Result := TPixelFormat.A8;
      10: Result := TPixelFormat.A8R3G3B2;
      11: Result := TPixelFormat.X4R4G4B4;
      12: Result := TPixelFormat.A2B10G10R10;
      13: Result := TPixelFormat.G16R16;
      14: Result := TPixelFormat.A2R10G10B10;
      15: Result := TPixelFormat.A16B16G16R16;
      16: Result := TPixelFormat.L8;
      17: Result := TPixelFormat.A8L8;
      18: Result := TPixelFormat.A4L4;
      34: Result := TPixelFormat.L16;
      43: Result := TPixelFormat.A8B8G8R8;
      44: Result := TPixelFormat.X8B8G8R8;
    else
      Result := TPixelFormat.Unknown;
    end
  else
    case Format of
      0: Result := TPixelFormat.R3G3B2;
      1: Result := TPixelFormat.R5G6B5;
      2: Result := TPixelFormat.X8R8G8B8;
      3: Result := TPixelFormat.X1R5G5B5;
      4: Result := TPixelFormat.X4R4G4B4;
      5: Result := TPixelFormat.A8R8G8B8;
      6: Result := TPixelFormat.A1R5G5B5;
      7: Result := TPixelFormat.A4R4G4B4;
      8: Result := TPixelFormat.A8R3G3B2;
      9: Result := TPixelFormat.A2R2G2B2;
    else
      Result := TPixelFormat.Unknown;
    end;
end;

function LoadImageASDb(const Image: TAtlasImage; const Stream: TStream): Boolean;
var
  PixelFormat: TPixelFormat;
  TextureSize, PatternSize, VisibleSize: TPoint2px;
  TextureCount, PatternCount, I: Integer;
  Texture: TCustomLockableTexture;
begin
  // --> Pixel Format
  PixelFormat := ASDbToPixelFormat(Stream.GetByte);
  // --> Pattern Size
  PatternSize.X := Stream.GetLongInt;
  PatternSize.Y := Stream.GetLongInt;
  // --> Visible Size
  VisibleSize.X := Stream.GetLongInt;
  VisibleSize.Y := Stream.GetLongInt;
  // --> Pattern Count
  PatternCount := Stream.GetLongInt;
  // --> Texture Size
  TextureSize.X := Stream.GetLongInt;
  TextureSize.Y := Stream.GetLongInt;
  // --> Texture Count
  TextureCount := Stream.GetLongInt;

  if Image.PixelFormat = TPixelFormat.Unknown then
    Image.PixelFormat := PixelFormat;

  Result := False;

  for I := 0 to TextureCount - 1 do
  begin
    Texture := Image.InsertTexture(TextureSize.X, TextureSize.Y);
    if Texture = nil then
      Exit(False);

    if Texture.PixelFormat.CanBulkCopyTo(PixelFormat) then
      // Direct copy of pixels (very fast).
      Result := ReadPixelsRaw(Stream, Texture)
    else if PixelFormat = TPixelFormat.A8R8G8B8 then
      // Conversion from 32-bit RGBA to custom pixel format (moderately slow).
      Result := ReadPixels32toX(Stream, Texture)
    else
      // Conversion from one pixel format to another (quite slow).
      Result := ReadPixelsXToX(Stream, Texture, PixelFormat);

    if not Result then
      Exit;
  end;

  Image.SetupRegionPatterns(PatternSize, VisibleSize, PatternCount);
end;

{$ENDREGION}
{$REGION 'PXLA (ASVF) image loading'}

function PXLAToPixelFormat(const Format: Integer): TPixelFormat;
begin
  if Format and $80 > 0 then
    Result := TPixelFormat(Format and $7F)
  else
    case Format of
      1: Result := TPixelFormat.R8G8B8;
      2: Result := TPixelFormat.A8R8G8B8;
      3: Result := TPixelFormat.X8R8G8B8;
      4: Result := TPixelFormat.R5G6B5;
      5: Result := TPixelFormat.X1R5G5B5;
      6: Result := TPixelFormat.A1R5G5B5;
      7: Result := TPixelFormat.A4R4G4B4;
      8: Result := TPixelFormat.R3G3B2;
      9: Result := TPixelFormat.A8;
      10: Result := TPixelFormat.A8R3G3B2;
      11: Result := TPixelFormat.X4R4G4B4;
      12: Result := TPixelFormat.A2B10G10R10;
      13: Result := TPixelFormat.G16R16;
      14: Result := TPixelFormat.A2R10G10B10;
      15: Result := TPixelFormat.A16B16G16R16;
      16: Result := TPixelFormat.L8;
      17: Result := TPixelFormat.A8L8;
      18: Result := TPixelFormat.A4L4;
      19: Result := TPixelFormat.L16;
      20: Result := TPixelFormat.R16F;
      21: Result := TPixelFormat.G16R16F;
      22: Result := TPixelFormat.A16B16G16R16F;
      23: Result := TPixelFormat.R32F;
      24: Result := TPixelFormat.G32R32F;
      25: Result := TPixelFormat.A32B32G32R32F;
      26: Result := TPixelFormat.A8B8G8R8;
      27: Result := TPixelFormat.X8B8G8R8;
      29: Result := TPixelFormat.A2R2G2B2;
    else
      Result := TPixelFormat.Unknown;
    end;
end;

function LoadImagePXLA(const Image: TAtlasImage; const Stream: TStream): Boolean;
var
  PixelFormat: TPixelFormat;
  TextureSize, PatternSize, VisibleSize: TPoint2px;
  TextureCount, PatternCount, I: Integer;
  Texture: TCustomLockableTexture;
begin
  // --> Pixel Format
  PixelFormat := PXLAToPixelFormat(Stream.GetByte);
  // --> Pattern Size
  PatternSize.X := Stream.GetWord;
  PatternSize.Y := Stream.GetWord;
  // --> Pattern Count
  PatternCount := Stream.GetLongInt;
  // --> Visible Size
  VisibleSize.X := Stream.GetWord;
  VisibleSize.Y := Stream.GetWord;
  // --> Texture Size
  TextureSize.X := Stream.GetWord;
  TextureSize.Y := Stream.GetWord;
  // --> Texture Count
  TextureCount := Stream.GetWord;

  if Image.PixelFormat = TPixelFormat.Unknown then
    Image.PixelFormat := PixelFormat;

  Result := False;

  for I := 0 to TextureCount - 1 do
  begin
    Texture := Image.InsertTexture(TextureSize.X, TextureSize.Y);
    if Texture = nil then
      Exit(False);

    if Texture.PixelFormat.CanBulkCopyTo(PixelFormat) then
      // Direct copy of pixels (very fast).
      Result := ReadPixelsRaw(Stream, Texture)
    else if PixelFormat = TPixelFormat.A8R8G8B8 then
      // Conversion from 32-bit RGBA to custom pixel format (moderately slow).
      Result := ReadPixels32toX(Stream, Texture)
    else
      // Conversion from one pixel format to another (quite slow).
      Result := ReadPixelsXToX(Stream, Texture, PixelFormat);

    if not Result then
      Exit;
  end;

  Image.SetupRegionPatterns(PatternSize, VisibleSize, PatternCount);
end;

{$ENDREGION}
{$REGION 'Extended Bitmap Font'}

type
  TExtendedBitmapFont = class(TBitmapFont)
  public
    function LoadLegacyFromStream(const Stream: TStream; const PixelFormat: TPixelFormat): Boolean;
    function LoadEntriesFromStream(const Stream: TStream): Boolean; inline;

    property Image: TAtlasImage read FImage write FImage;
  end;

function TExtendedBitmapFont.LoadLegacyFromStream(const Stream: TStream; const PixelFormat: TPixelFormat): Boolean;
var
  FirstLetter, LetterCount, I, Index: Integer;
  LetterSizes: array of TPoint2px;
  Region: TIntRect;
begin
  try
    // --> First Letter
    FirstLetter := Stream.GetLongInt;
    // --> Padding
    Stream.GetLongInt;
    // --> Spacing
    Stream.GetLongInt;
    // --> Letter Count
    LetterCount := Stream.GetLongInt;

    if LetterCount <= 0 then
      Exit(False);

    SetLength(LetterSizes, LetterCount);

    // --> Letter Sizes
    for I := 0 to LetterCount - 1 do
    begin
      LetterSizes[I].X := Stream.GetLongInt;
      LetterSizes[I].Y := Stream.GetLongInt;
    end;
  except
    Exit(False);
  end;

  FImage := TAtlasImage.Create(Device, SubscribedTextures);
  FImage.DynamicImage := False;
  FImage.MipMapping := False;
  FImage.PixelFormat := PixelFormat;

  if not LoadImageASDb(FImage, Stream) then
    Exit(False);

  for I := 0 to FImage.Regions.Count - 1 do
  begin
    Index := FirstLetter + I;

    Region := FImage.Regions[I].Rect;

    if I = 0 then
      FSize := Region.Size;

    FEntries[Index].MapLeft := Region.Left;
    FEntries[Index].MapTop := Region.Top;
    FEntries[Index].MapWidth := Region.Width;
    FEntries[Index].MapHeight := Region.Height;

    FEntries[Index].TrailingSpace := LetterSizes[I].X - Region.Width;
  end;

  Interleave := 0.0;
  Result := True;
end;

function TExtendedBitmapFont.LoadEntriesFromStream(const Stream: TStream): Boolean;
begin
  Result := LoadEntriesFromXMLStream(Stream);
end;

{$ENDREGION}
{$REGION 'Global Functions'}

function LoadImageFromArchive(const Key: UniString; const Images: TAtlasImages; const Archive: TArchive;
  const MipMapping: Boolean; const PixelFormat: TPixelFormat): Integer;
var
  Stream: TMemoryStream;
  Image: TAtlasImage;
  EntryIndex: Integer;
  Success: Boolean;
begin
  Result := -1;
  Image := nil;
  Success := False;

  EntryIndex := Archive.IndexOf(Key);
  if EntryIndex = -1 then
    Exit;

  Stream := TMemoryStream.Create;
  try
    if not Archive.ReadStream(Key, Stream) then
      Exit;
    try
      Stream.Position := 0;

      Image := TAtlasImage.Create(Images.Device, False);
      Image.MipMapping := MipMapping;
      Image.PixelFormat := PixelFormat;

      if Archive.Entries[EntryIndex].EntryType = TArchive.TEntryType.Image then
        // The entry is an image in optimal RAW format.
        case Archive.Format of
          TArchive.TFormat.VTDb,
          TArchive.TFormat.ASDb:
            Success := LoadImageASDb(Image, Stream);

          TArchive.TFormat.ASVF:
            Success := LoadImagePXLA(Image, Stream);
        end
      else
        // The entry is just an embedded file.
        Success := Image.LoadFromStream(ExtractFileExt(Key), Stream);

      if Success then
        Result := Images.Insert(Image)
      else
        FreeAndNil(Image);
    except
      Image.Free;
      Exit;
    end;
  finally
    Stream.Free;
  end;
end;

function LoadLegacyFontBinaryFromStream(const Name: StdString; const Stream: TStream; const Fonts: TBitmapFonts;
  const PixelFormat: TPixelFormat): Integer;
var
  Font: TExtendedBitmapFont;
begin
  Font := TExtendedBitmapFont.Create(Fonts.Device, False);
  Font.Name := Name;

  if not Font.LoadLegacyFromStream(Stream, PixelFormat) then
  begin
    Font.Free;
    Exit(-1);
  end;

  Result := Fonts.Insert(Font)
end;

function LoadFontBinaryFromStream(const Name: StdString; const Stream: TStream; const Fonts: TBitmapFonts;
  const PixelFormat: TPixelFormat): Integer;
var
  Font: TBitmapFont;
begin
  Font := TBitmapFont.Create(Fonts.Device, False);
  Font.Name := Name;

  if not Font.LoadFromBinaryStream(Stream, PixelFormat) then
  begin
    Font.Free;
    Exit(-1);
  end;

  Result := Fonts.Insert(Font);
end;

function LoadFontAndRawImageFromStreams(const Name: StdString; const XMLStream, RAWStream: TStream;
  const Fonts: TBitmapFonts; const Archive: TArchive; const PixelFormat: TPixelFormat): Integer;
var
  Font: TExtendedBitmapFont;
  Success: Boolean;
begin
  Font := TExtendedBitmapFont.Create(Fonts.Device, False);
  Font.Name := Name;

  if not Font.LoadEntriesFromStream(XMLStream) then
  begin
    Font.Free;
    Exit(-1);
  end;

  Font.Image := TAtlasImage.Create(Fonts.Device, False);
  with Font do
  begin
    Image.DynamicImage := False;
    Image.MipMapping := False;
    Image.PixelFormat := PixelFormat;
  end;

  case Archive.Format of
    TArchive.TFormat.VTDb,
    TArchive.TFormat.ASDb:
      Success := LoadImageASDb(Font.Image, RAWStream);
  else
    Success := LoadImagePXLA(Font.Image, RAWStream);
  end;

  if not Success then
  begin
    Font.Free;
    Exit(-1);
  end;

  Result := Fonts.Insert(Font);
end;

function LoadFontAndImageFromStreams(const Name: StdString; const XMLStream, ImageStream: TStream;
  const Fonts: TBitmapFonts; const PixelFormat: TPixelFormat): Integer;
var
  Font: TBitmapFont;
begin
  Font := TBitmapFont.Create(Fonts.Device, False);
  Font.Name := Name;

  if not Font.LoadFromXMLStream(TBitmapFont.DefaultImageExtension, ImageStream, XMLStream, PixelFormat) then
  begin
    Font.Free;
    Exit(-1);
  end;

  Result := Fonts.Insert(Font);
end;

function LoadFontFromArchive(const Key: UniString; const Fonts: TBitmapFonts; const Archive: TArchive;
  const PixelFormat: TPixelFormat): Integer;
const
  RawImageExtension = '.image';
var
  Stream, SepStream: TMemoryStream;
  EntryIndex, SepEntryIndex: Integer;
  FontName, Extension: StdString;
  SepEntry: UniString;
begin
  Result := -1;
  SepStream := nil;

  EntryIndex := Archive.IndexOf(Key);
  if EntryIndex = -1 then
    Exit;

  Stream := TMemoryStream.Create;
  try
    if not Archive.ReadStream(Key, Stream) then
      Exit;
    try
      Stream.Position := 0;

      FontName := ChangeFileExt(Key, '');

      if Archive.Entries[EntryIndex].EntryType <> TArchive.TEntryType.Font then
      begin
        Extension := ExtractFileExt(Key);

        if not SameText(Extension, TBitmapFont.DefaultBinaryExtension) then
        begin
          if SameText(Extension, TBitmapFont.DefaultXMLExtension) then
          begin // XML file
            SepEntry := ChangeFileExt(Key, RawImageExtension);
            SepEntryIndex := Archive.IndexOf(SepEntry);

            if SepEntryIndex = -1 then
            begin
              SepEntry := ChangeFileExt(Key, TBitmapFont.DefaultImageExtension);
              SepEntryIndex := Archive.IndexOf(SepEntry);

              if SepEntryIndex = -1 then
                Exit;
            end;

            SepStream := TMemoryStream.Create;
            if not Archive.ReadStream(SepEntry, SepStream) then
              Exit;

            SepStream.Position := 0;

            if Archive.Entries[SepEntryIndex].EntryType = TArchive.TEntryType.Image then
              Result := LoadFontAndRawImageFromStreams(FontName, Stream, SepStream, Fonts, Archive, PixelFormat)
            else
              Result := LoadFontAndImageFromStreams(FontName, Stream, SepStream, Fonts, PixelFormat);
          end
          else
          begin // Image file
            SepEntry := ChangeFileExt(Key, TBitmapFont.DefaultXMLExtension);

            SepEntryIndex := Archive.IndexOf(SepEntry);
            if SepEntryIndex = -1 then
              Exit;

            SepStream := TMemoryStream.Create;
            if not Archive.ReadStream(SepEntry, SepStream) then
              Exit;

            SepStream.Position := 0;

            if Archive.Entries[EntryIndex].EntryType = TArchive.TEntryType.Image then
              Result := LoadFontAndRawImageFromStreams(FontName, SepStream, Stream, Fonts, Archive, PixelFormat)
            else
              Result := LoadFontAndImageFromStreams(FontName, SepStream, Stream, Fonts, PixelFormat);
          end;
        end
        else
          Result := LoadFontBinaryFromStream(FontName, Stream, Fonts, PixelFormat)
      end
      else
        { Earlier versions of Asphyre had a special entry type, where font would be stored in binary form,
          which would include its graphics in optimal RAW format. }
        Result := LoadLegacyFontBinaryFromStream(FontName, Stream, Fonts, PixelFormat);
    except
      Exit(-1);
    end;
  finally
    SepStream.Free;
    Stream.Free;
  end;
end;

{$ENDREGION}

end.
