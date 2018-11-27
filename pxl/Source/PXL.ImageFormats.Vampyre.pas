unit PXL.ImageFormats.Vampyre;
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

type
  TVampyreImageFormatHandler = class(TCustomImageFormatHandler)
  protected
    procedure RegisterExtensions; override;
  public
    function LoadFromStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
      const DestSurface: TPixelSurface; const AlphaFormatRequest: TAlphaFormatRequest): Boolean; override;
    function SaveToStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
      const SourceSurface: TPixelSurface; const Quality: Pointer): Boolean; override;
{  private
    class var FCurrent: TVampyreImageFormatHandler;
    class destructor Finalize;
  public
    class procedure Allocate;
    class procedure Release;
    class property Current: TVampyreImageFormatHandler read FCurrent;}
  end;

implementation

uses
  ImagingTypes, ImagingClasses, ImagingUtility;

{$REGION 'Global Functions'}

function VampyreToFormat(const Format: TImageFormat): TPixelFormat;
begin
  Result := TPixelFormat.Unknown;

  case Format of
    ifIndex8:
      Result := TPixelFormat.I8;

    ifGray8:
      Result := TPixelFormat.L8;

    ifA8Gray8:
      Result := TPixelFormat.A8L8;

    ifGray16:
      Result := TPixelFormat.L16;

    ifR3G3B2:
      Result := TPixelFormat.R3G3B2;

    ifR5G6B5:
      Result := TPixelFormat.R5G6B5;

    ifA1R5G5B5:
      Result := TPixelFormat.A1R5G5B5;

    ifA4R4G4B4:
      Result := TPixelFormat.A4R4G4B4;

    ifX1R5G5B5:
      Result := TPixelFormat.X1R5G5B5;

    ifX4R4G4B4:
      Result := TPixelFormat.X4R4G4B4;

    ifR8G8B8:
      Result := TPixelFormat.R8G8B8;

    ifA8R8G8B8:
      Result := TPixelFormat.A8R8G8B8;

    ifX8R8G8B8:
      Result := TPixelFormat.X8R8G8B8;

    ifA16B16G16R16:
      Result := TPixelFormat.A16B16G16R16;

    ifR32F:
      Result := TPixelFormat.R32F;

    ifA32B32G32R32F:
      Result := TPixelFormat.A32B32G32R32F;

    ifR16F:
      Result := TPixelFormat.R16F;

    ifA16B16G16R16F:
      Result := TPixelFormat.A16B16G16R16F;
  end;
end;

function FormatToVampyre(const Format: TPixelFormat): TImageFormat;
begin
  Result := ifUnknown;

  case Format of
    TPixelFormat.I8:
      Result := ifIndex8;

    TPixelFormat.L8:
      Result := ifGray8;

    TPixelFormat.A8L8:
      Result := ifA8Gray8;

    TPixelFormat.L16:
      Result := ifGray16;

    TPixelFormat.R3G3B2:
      Result := ifR3G3B2;

    TPixelFormat.R5G6B5:
      Result := ifR5G6B5;

    TPixelFormat.A1R5G5B5:
      Result := ifA1R5G5B5;

    TPixelFormat.A4R4G4B4:
      Result := ifA4R4G4B4;

    TPixelFormat.X1R5G5B5:
      Result := ifX1R5G5B5;

    TPixelFormat.X4R4G4B4:
      Result := ifX4R4G4B4;

    TPixelFormat.R8G8B8:
      Result := ifR8G8B8;

    TPixelFormat.A8R8G8B8:
      Result := ifA8R8G8B8;

    TPixelFormat.X8R8G8B8:
      Result := ifX8R8G8B8;

    TPixelFormat.A16B16G16R16:
      Result := ifA16B16G16R16;

    TPixelFormat.R32F:
      Result := ifR32F;

    TPixelFormat.A32B32G32R32F:
      Result := ifA32B32G32R32F;

    TPixelFormat.R16F:
      Result := ifR16F;

    TPixelFormat.A16B16G16R16F:
      Result := ifA16B16G16R16F;
  end;
end;

function RemoveDotFromExtension(const Extension: StdString): StdString;
var
  DotAt: Integer;
begin
  Result := Extension;

  DotAt := Pos('.', Result);
  if DotAt <> 0 then
    Delete(Result, 1, DotAt);
end;

{$ENDREGION}
{$REGION 'TVampyreImageFormatHandler'}

procedure TVampyreImageFormatHandler.RegisterExtensions;
begin
  RegisterExtension('.bmp');
  RegisterExtension('.dds');
  RegisterExtension('.gif');
  RegisterExtension('.jng');
  RegisterExtension('.jpeg');
  RegisterExtension('.jpg');
  RegisterExtension('.mng');
  RegisterExtension('.pam');
  RegisterExtension('.pbm');
  RegisterExtension('.pcx');
  RegisterExtension('.pfm');
  RegisterExtension('.pgm');
  RegisterExtension('.png');
  RegisterExtension('.ppm');
  RegisterExtension('.psd');
  RegisterExtension('.tga');
  RegisterExtension('.tif');
  RegisterExtension('.tiff');
  RegisterExtension('.xpm');
end;

function TVampyreImageFormatHandler.LoadFromStream(const Context: Pointer; const Extension: StdString;
  const Stream: TStream; const DestSurface: TPixelSurface; const AlphaFormatRequest: TAlphaFormatRequest): Boolean;
var
  Image: TSingleImage;
  SourceFormat: TPixelFormat;
  I, CopyBytes: Integer;
begin
  try
    Image:= TSingleImage.CreateFromStream(Stream);
  except
    Exit(False);
  end;

  try
    SourceFormat := VampyreToFormat(Image.Format);
    if SourceFormat = TPixelFormat.Unknown then
    begin
       // Attempt to convert unknown pixel format to something more common.
      try
        Image.Format := ifA8R8G8B8;
      except
        Exit(False);
      end;

      SourceFormat := VampyreToFormat(Image.Format);
      if SourceFormat = TPixelFormat.Unknown then
        Exit(False);
    end;

    DestSurface.SetSize(Image.Width, Image.Height, SourceFormat);

    CopyBytes := DestSurface.Width * Integer(DestSurface.BytesPerPixel);

    for I:= 0 to DestSurface.Height - 1 do
      Move(Image.ScanLine[I]^, DestSurface.Scanline[I]^, CopyBytes);
  finally
    Image.Free;
  end;

  Result:= True;
end;

function TVampyreImageFormatHandler.SaveToStream(const Context: Pointer; const Extension: StdString;
  const Stream: TStream; const SourceSurface: TPixelSurface; const Quality: Pointer): Boolean;
var
  DestFormat: TImageFormat;
  Image: TSingleImage;
  I, CopyBytes: Integer;
begin
  DestFormat := FormatToVampyre(SourceSurface.PixelFormat);
  if DestFormat = ifUnknown then
    Exit(False);

  try
    Image := TSingleImage.CreateFromParams(SourceSurface.Width, SourceSurface.Height, DestFormat);
  except
    Exit(False);
  end;

  try
    CopyBytes := SourceSurface.Width * Integer(SourceSurface.BytesPerPixel);

    for I:= 0 to SourceSurface.Height - 1 do
      Move(SourceSurface.Scanline[I]^, Image.ScanLine[I]^, CopyBytes);

    try
      Image.SaveToStream(RemoveDotFromExtension(Extension), Stream);
    except
      Exit(False);
    end;
  finally
    Image.Free;
  end;

  Result := True;
end;

{$ENDREGION}

end.
