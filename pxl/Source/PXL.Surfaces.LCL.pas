unit PXL.Surfaces.LCL;
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
  Graphics, PXL.Types, PXL.Surfaces;

function LoadSurfaceFromBitmap(const Surface: TPixelSurface; const Bitmap: TBitmap): Boolean;
function SaveSurfaceToBitmap(const Surface: TPixelSurface; const Bitmap: TBitmap): Boolean;
function DrawSurfaceToCanvas(const Surface: TPixelSurface; const Canvas: TCanvas; const X, Y: Integer): Boolean;

implementation

uses
  GraphType, IntfGraphics, PXL.Formats;

function PixelFormatToRawDesc(const Format: TPixelFormat; out RawDesc: TRawImageDescription): Boolean;
var
  Desc: TPixelFormatDescription;
begin
  if (not GetPixelFormatDescription(Format, Desc)) or (Desc.FormatType = TPixelFormatType.Float) then
    Exit(False);

  if Format in [TPixelFormat.B8G8R8A8, TPixelFormat.B8G8R8X8] then
  begin // MSB ordering formats.
    if Format = TPixelFormat.B8G8R8A8 then
      Result := GetPixelFormatDescription(TPixelFormat.A8R8G8B8, Desc)
    else
      Result := GetPixelFormatDescription(TPixelFormat.X8R8G8B8, Desc);

    if not Result then
      Exit;

    RawDesc.ByteOrder := riboMSBFirst;
  end
  else // LSB ordering
    RawDesc.ByteOrder := riboLSBFirst;

  RawDesc.Depth := Desc.UsedBitCount;
  RawDesc.BitsPerPixel := Desc.BitCount;

  case Desc.FormatType of
    TPixelFormatType.Normal:
      begin
        RawDesc.Format := ricfRGBA;
        RawDesc.RedPrec := Desc.RedBits.Count;
        RawDesc.RedShift := Desc.RedBits.Shift;
        RawDesc.GreenPrec := Desc.GreenBits.Count;
        RawDesc.GreenShift := Desc.GreenBits.Shift;
        RawDesc.BluePrec := Desc.BlueBits.Count;
        RawDesc.BlueShift := Desc.BlueBits.Shift;
        RawDesc.AlphaPrec := Desc.AlphaBits.Count;
        RawDesc.AlphaShift := Desc.AlphaBits.Shift;
      end;

    TPixelFormatType.Luminance:
      begin
        RawDesc.Format := ricfGray;
        RawDesc.RedPrec := Desc.LuminanceBits.Count;
        RawDesc.RedShift := Desc.LuminanceBits.Shift;
        RawDesc.AlphaPrec := Desc.AlphaBits.Count;
        RawDesc.AlphaShift := Desc.AlphaBits.Shift;
      end;
  end;

  Result := True;
end;

function RawDescToPixelFormat(const RawDesc: TRawImageDescription): TPixelFormat;
var
  Format: TPixelFormat;
  AnotherDesc: TRawImageDescription;
begin
  Move(RawDesc, AnotherDesc, SizeOf(TRawImageDescription));

  for Format := Low(TPixelFormat) to High(TPixelFormat) do
    if PixelFormatToRawDesc(Format, AnotherDesc) and RawDesc.IsEqual(AnotherDesc) then
      Exit(Format);

  Result := TPixelFormat.Unknown;
end;

function LoadSurfaceFromBitmap(const Surface: TPixelSurface; const Bitmap: TBitmap): Boolean;
var
  Image: TLazIntfImage;
  SourceFormat, DestFormat: TPixelFormat;
  TempBuffer: Pointer;
  I: Integer;
begin
  if (Bitmap = nil) or (Bitmap.Width < 1) or (Bitmap.Height < 1) then
    Exit(False);

  try
    Image := Bitmap.CreateIntfImage;
    if (Image = nil) or (Image.Width < 1) or (Image.Height < 1) then
      Exit(False);
    try
      SourceFormat := RawDescToPixelFormat(Image.DataDescription);
      if SourceFormat = TPixelFormat.Unknown then
        Exit(False);

      DestFormat := TPixelFormat.Unknown;

      if SourceFormat in [TPixelFormat.X8R8G8B8, TPixelFormat.A8R8G8B8] then
        DestFormat := SourceFormat;

      Surface.SetSize(Image.Width, Image.Height, DestFormat);

      if Surface.PixelFormat = SourceFormat then
      begin // Direct copy.
        for I := 0 to Surface.Height - 1 do
          Move(Image.GetDataLineStart(I)^, Surface.Scanline[I]^, Surface.Width * Surface.BytesPerPixel);
      end
      else
      begin // Pixel format conversion.
        if Surface.PixelFormat in [TPixelFormat.X8R8G8B8, TPixelFormat.A8R8G8B8] then
        begin // Custom to native format.
          for I := 0 to Surface.Height - 1 do
            PixelXTo32Array(Image.GetDataLineStart(I), Surface.Scanline[I], SourceFormat, Surface.Width);
        end
        else
        begin // Custom to custom format.
          GetMem(TempBuffer, Surface.Width * SizeOf(TIntColor));
          try
            for I := 0 to Surface.Height - 1 do
            begin
              PixelXTo32Array(Image.GetDataLineStart(I), TempBuffer, SourceFormat, Surface.Width);
              Pixel32ToXArray(TempBuffer, Surface.Scanline[I], Surface.PixelFormat, Surface.Width);
            end;
          finally
            FreeMem(TempBuffer);
          end;
        end;
      end;
    finally
      Image.Free;
    end;
  except
    Exit(False);
  end;

  Result := True;
end;

function SaveSurfaceToBitmap(const Surface: TPixelSurface; const Bitmap: TBitmap): Boolean;
var
  Image: TLazIntfImage;
  RawImage: TRawImage;
  DestFormat: TPixelFormat;
  TempBuffer: Pointer;
  I: Integer;
begin
  if Surface.IsEmpty or (Bitmap = nil) then
    Exit(False);

  try
    Image := TLazIntfImage.Create(0, 0);
    try
      RawImage.Init;
      RawImage.Description := GetDescriptionFromDevice(0, Surface.Width, Surface.Height);

      DestFormat := RawDescToPixelFormat(RawImage.Description);
      if DestFormat = TPixelFormat.Unknown then
        Exit(False);

      RawImage.CreateData(False);
      Image.SetRawImage(RawImage);

      if Surface.PixelFormat = DestFormat then
      begin // Direct copy.
        for I := 0 to Surface.Height - 1 do
          Move(Surface.Scanline[I]^, Image.GetDataLineStart(I)^, Surface.Width * Surface.BytesPerPixel);
      end
      else
      begin // Pixel format conversion.
        if Surface.PixelFormat in [TPixelFormat.X8R8G8B8, TPixelFormat.A8R8G8B8] then
        begin // Custom to native format.
          for I := 0 to Surface.Height - 1 do
            Pixel32ToXArray(Surface.Scanline[I], Image.GetDataLineStart(I), DestFormat, Surface.Width);
        end
        else
        begin // Custom to custom format.
          GetMem(TempBuffer, Surface.Width * SizeOf(TIntColor));
          try
            for I := 0 to Surface.Height - 1 do
            begin
              PixelXTo32Array(Surface.Scanline[I], TempBuffer, Surface.PixelFormat, Surface.Width);
              Pixel32ToXArray(TempBuffer, Image.GetDataLineStart(I), DestFormat, Surface.Width);
            end;
          finally
            FreeMem(TempBuffer);
          end;
        end;
      end;

      Bitmap.LoadFromIntfImage(Image);
    finally
      Image.Free;
    end;
  except
    Exit(False);
  end;

  Result := True;
end;

function DrawSurfaceToCanvas(const Surface: TPixelSurface; const Canvas: TCanvas; const X, Y: Integer): Boolean;
var
  Bitmap: TBitmap;
begin
  if (Surface = nil) or (Canvas = nil) then
    Exit(False);

  Surface.ResetAlpha({$IFDEF MSWINDOWS}False{$ELSE}True{$ENDIF});

  Bitmap := TBitmap.Create;
  try
    if not SaveSurfaceToBitmap(Surface, Bitmap) then
      Exit(False);

    Canvas.Draw(X, Y, Bitmap);
  finally
    Bitmap.Free;
  end;
end;

end.
