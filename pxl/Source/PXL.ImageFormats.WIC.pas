unit PXL.ImageFormats.WIC;
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
{$IFDEF FPC}
  PXL.Windows.Wincodec,
{$ELSE}
  Winapi.Wincodec,
{$ENDIF}

  Classes, PXL.TypeDef, PXL.Types, PXL.Surfaces, PXL.ImageFormats;

type
  TWICImageFormatHandler = class(TCustomImageFormatHandler)
  private
    FImagingFactory: IWICImagingFactory;
  protected
    procedure RegisterExtensions; override;
  public
    constructor Create(const AManager: TImageFormatManager);
    destructor Destroy; override;

    function LoadFromStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
      const DestSurface: TPixelSurface; const AlphaFormatRequest: TAlphaFormatRequest): Boolean; override;
    function SaveToStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
      const SourceSurface: TPixelSurface; const Quality: Pointer): Boolean; override;

    property ImagingFactory: IWICImagingFactory read FImagingFactory;
  end;

implementation

uses
  Windows, ActiveX, SysUtils;

{$REGION 'Global Functions'}

const
  IID_EMPTY: TGuid = '{00000000-0000-0000-0000-000000000000}';

function GUIDToPixelFormat(const Format: WICPixelFormatGUID; var Premultiplied: Boolean): TPixelFormat;
begin
  Premultiplied := False;

  if CompareMem(@Format, @GUID_WICPixelFormat32bppBGRA, SizeOf(TGuid)) then
    Exit(TPixelFormat.A8R8G8B8);

  if CompareMem(@Format, @GUID_WICPixelFormat32bppPBGRA, SizeOf(TGuid)) then
  begin
    Premultiplied := True;
    Exit(TPixelFormat.A8R8G8B8);
  end;

  if CompareMem(@Format, @GUID_WICPixelFormat32bppBGR, SizeOf(TGuid)) then
    Exit(TPixelFormat.X8R8G8B8);

  if CompareMem(@Format, @GUID_WICPixelFormat32bppRGBA, SizeOf(TGuid)) then
    Exit(TPixelFormat.A8B8G8R8);

  if CompareMem(@Format, @GUID_WICPixelFormat32bppPRGBA, SizeOf(TGuid)) then
  begin
    Premultiplied := True;
    Exit(TPixelFormat.A8B8G8R8);
  end;

  if CompareMem(@Format, @GUID_WICPixelFormat24bppBGR, SizeOf(TGuid)) then
    Exit(TPixelFormat.R8G8B8);

  if CompareMem(@Format, @GUID_WICPixelFormat64bppRGBA, SizeOf(TGuid)) then
    Exit(TPixelFormat.A16B16G16R16);

  if CompareMem(@Format, @GUID_WICPixelFormat64bppPRGBA, SizeOf(TGuid)) then
  begin
    Premultiplied := True;
    Exit(TPixelFormat.A16B16G16R16);
  end;

  if CompareMem(@Format, @GUID_WICPixelFormat16bppBGR565, SizeOf(TGuid)) then
    Exit(TPixelFormat.R5G6B5);

  if CompareMem(@Format, @GUID_WICPixelFormat16bppBGRA5551, SizeOf(TGuid)) then
    Exit(TPixelFormat.A1R5G5B5);

  if CompareMem(@Format, @GUID_WICPixelFormat16bppBGR555, SizeOf(TGuid)) then
    Exit(TPixelFormat.X1R5G5B5);

  if CompareMem(@Format, @GUID_WICPixelFormat16bppBGR555, SizeOf(TGuid)) then
    Exit(TPixelFormat.X1R5G5B5);

  if CompareMem(@Format, @GUID_WICPixelFormat32bppRGBA1010102, SizeOf(TGuid)) then
    Exit(TPixelFormat.A2B10G10R10);

  if CompareMem(@Format, @GUID_WICPixelFormat8bppAlpha, SizeOf(TGuid)) then
    Exit(TPixelFormat.A8);

  if CompareMem(@Format, @GUID_WICPixelFormat8bppGray, SizeOf(TGuid)) then
    Exit(TPixelFormat.L8);

  if CompareMem(@Format, @GUID_WICPixelFormat16bppGray, SizeOf(TGuid)) then
    Exit(TPixelFormat.L16);

  Result := TPixelFormat.Unknown;
end;

function PixelFormatToGUID(const Format: TPixelFormat; const Premultiplied: Boolean): WICPixelFormatGUID;
begin
  case Format of
    TPixelFormat.A8R8G8B8:
      if Premultiplied then
        Result := GUID_WICPixelFormat32bppPBGRA
      else
        Result := GUID_WICPixelFormat32bppBGRA;

    TPixelFormat.X8R8G8B8:
      Result := GUID_WICPixelFormat32bppBGR;

    TPixelFormat.A8B8G8R8:
      if Premultiplied then
        Result := GUID_WICPixelFormat32bppPRGBA
      else
        Result := GUID_WICPixelFormat32bppRGBA;

    TPixelFormat.R8G8B8:
      Result := GUID_WICPixelFormat24bppBGR;

    TPixelFormat.R5G6B5:
      Result := GUID_WICPixelFormat16bppBGR565;

    TPixelFormat.A1R5G5B5:
      Result := GUID_WICPixelFormat16bppBGRA5551;

    TPixelFormat.X1R5G5B5:
      Result := GUID_WICPixelFormat16bppBGR555;

    TPixelFormat.A2B10G10R10:
      if not Premultiplied then
        Result := GUID_WICPixelFormat32bppRGBA1010102
      else
        Result := GUID_WICPixelFormatUndefined;

    TPixelFormat.A16B16G16R16:
      if Premultiplied then
        Result := GUID_WICPixelFormat64bppPRGBA
      else
        Result := GUID_WICPixelFormat64bppRGBA;

    TPixelFormat.A8:
      Result := GUID_WICPixelFormat8bppAlpha;

    TPixelFormat.L8:
      Result := GUID_WICPixelFormat8bppGray;

    TPixelFormat.L16:
      Result := GUID_WICPixelFormat16bppGray;

    else
      Result := GUID_WICPixelFormatUndefined;
  end;
end;

{$ENDREGION}
{$REGION 'TWICImageFormatHandler'}

constructor TWICImageFormatHandler.Create(const AManager: TImageFormatManager);
begin
  inherited;

  if Failed(CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IUnknown,
    FImagingFactory)) then
    FImagingFactory := nil;
end;

destructor TWICImageFormatHandler.Destroy;
begin
  FImagingFactory := nil;

  inherited;
end;

procedure TWICImageFormatHandler.RegisterExtensions;
begin
  RegisterExtension('.bmp', @GUID_ContainerFormatBmp);
  RegisterExtension('.png', @GUID_ContainerFormatPng);
  RegisterExtension('.jpg', @GUID_ContainerFormatJpeg);
  RegisterExtension('.jpeg', @GUID_ContainerFormatJpeg);
  RegisterExtension('.tiff', @GUID_ContainerFormatTiff);
  RegisterExtension('.tif', @GUID_ContainerFormatTiff);
  RegisterExtension('.gif', @GUID_ContainerFormatGif);
  RegisterExtension('.ico', @GUID_ContainerFormatIco);
  RegisterExtension('.hdp', @GUID_ContainerFormatWmp);
end;

function TWICImageFormatHandler.LoadFromStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
  const DestSurface: TPixelSurface; const AlphaFormatRequest: TAlphaFormatRequest): Boolean;
var
  Decoder: IWICBitmapDecoder;
  Frame: IWICBitmapFrameDecode;
  Converter: IWICFormatConverter;
  NewWidth, NewHeight: UINT;
  NativeFormat: WICPixelFormatGUID;
  PixelFormat: TPixelFormat;
  Premultiplied: Boolean;
begin
  if FImagingFactory = nil then
    Exit(False);

  try
    if Failed(FImagingFactory.CreateDecoderFromStream(TStreamAdapter.Create(Stream) as IStream, IID_EMPTY,
      WICDecodeMetadataCacheOnDemand, Decoder)) or (Decoder = nil) then
      Exit(False);

    if Failed(Decoder.GetFrame(0, Frame)) or (Frame = nil) then
      Exit(False);

    if Failed(Frame.GetPixelFormat(NativeFormat)) then
      NativeFormat := GUID_WICPixelFormatUndefined;

    PixelFormat := GUIDToPixelFormat(NativeFormat, Premultiplied);

    if (PixelFormat <> TPixelFormat.Unknown) and
      ((AlphaFormatRequest = TAlphaFormatRequest.DontCare) or
      ((AlphaFormatRequest = TAlphaFormatRequest.NonPremultiplied) and (not Premultiplied)) or
      ((AlphaFormatRequest = TAlphaFormatRequest.Premultiplied) and Premultiplied)) then
    begin // Native Pixel Format
      if Failed(Frame.GetSize(NewWidth, NewHeight)) then
        Exit(False);

      DestSurface.SetSize(NewWidth, NewHeight, PixelFormat);
      DestSurface.PremultipliedAlpha := Premultiplied;

      if (NewWidth > 0) and (NewHeight > 0) then
        Result := Succeeded(Frame.CopyPixels(nil, DestSurface.Pitch, DestSurface.Pitch * DestSurface.Height,
          DestSurface.Bits))
      else
        Result := True;
    end
    else
    begin // Pixel Format Conversion
      if PixelFormat <> TPixelFormat.Unknown then
        NativeFormat := PixelFormatToGUID(PixelFormat, AlphaFormatRequest = TAlphaFormatRequest.Premultiplied)
      else
        NativeFormat := GUID_WICPixelFormatUndefined;

      if CompareMem(@NativeFormat, @GUID_WICPixelFormatUndefined, SizeOf(TGuid)) then
        if AlphaFormatRequest = TAlphaFormatRequest.Premultiplied then
          NativeFormat := GUID_WICPixelFormat32bppPBGRA
        else
          NativeFormat := GUID_WICPixelFormat32bppBGRA;

      PixelFormat := GUIDToPixelFormat(NativeFormat, Premultiplied);
      if PixelFormat = TPixelFormat.Unknown then
        Exit(False);

      if Failed(FImagingFactory.CreateFormatConverter(Converter)) or (Converter = nil) then
        Exit(False);

      if Failed(Converter.Initialize(Frame, NativeFormat, WICBitmapDitherTypeNone, nil, 0, 0)) then
        Exit(False);

      if Failed(Converter.GetSize(NewWidth, NewHeight)) then
        Exit(False);

      DestSurface.SetSize(NewWidth, NewHeight, PixelFormat);
      DestSurface.PremultipliedAlpha := Premultiplied;

      Result := Succeeded(Converter.CopyPixels(nil, DestSurface.Pitch, DestSurface.Pitch * DestSurface.Height,
        DestSurface.Bits));
    end;
  except
    Exit(False);
  end;
end;

function TWICImageFormatHandler.SaveToStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
  const SourceSurface: TPixelSurface; const Quality: Pointer): Boolean;
var
  EncoderType: TGuid;
  Encoder: IWICBitmapEncoder;
  WStream: IWICStream;
  Frame: IWICBitmapFrameEncode;
  PropertyBag: IPropertyBag2;
  PropertyName: TPropBag2;
  PropertyValue: TPropVariant;
  NativeFormat, NewNativeFormat: WICPixelFormatGUID;
  Bitmap: IWICBitmap;
  Converter: IWICFormatConverter;
begin
  if FImagingFactory = nil then
    Exit(False);

  EncoderType := PGuid(Context)^;

  if CompareMem(@EncoderType, @IID_EMPTY, SizeOf(TGuid)) then
    Exit(False);

  NativeFormat := PixelFormatToGUID(SourceSurface.PixelFormat, SourceSurface.PremultipliedAlpha);
  if CompareMem(@NativeFormat, @GUID_WICPixelFormatUndefined, SizeOf(TGuid)) then
    Exit(False);

  try
    if Failed(FImagingFactory.CreateEncoder(EncoderType, IID_EMPTY, Encoder)) or (Encoder = nil) then
      Exit(False);

    if Failed(FImagingFactory.CreateStream(WStream)) or (WStream = nil) then
      Exit(False);

    if Failed(WStream.InitializeFromIStream(TStreamAdapter.Create(Stream) as IStream)) then
      Exit(False);

    if Failed(Encoder.Initialize(WStream, WICBitmapEncoderNoCache)) then
      Exit(False);

    PropertyBag := nil;

    if Failed(Encoder.CreateNewFrame(Frame, PropertyBag)) or (Frame = nil) then
      Exit(False);

    if CompareMem(@EncoderType, @GUID_ContainerFormatJpeg, SizeOf(TGuid)) then
    begin
      FillChar(PropertyName, SizeOf(TPropBag2), 0);
      FillChar(PropertyValue, SizeOf(TPropVariant), 0);

      PropertyName.dwType := 1;
      PropertyName.vt := VT_R4;
      PropertyName.pstrName := POleStr(UniString('ImageQuality'#0));
      PropertyValue.vt := VT_R4;
      PropertyValue.fltVal := SizeInt(Quality) / 100.0;

      if Failed(PropertyBag.Write(1, @PropertyName, @PropertyValue)) then
        Exit(False);
    end;

    if Failed(Frame.Initialize(PropertyBag)) then
      Exit(False);

    if Failed(Frame.SetSize(SourceSurface.Width, SourceSurface.Height)) then
      Exit(False);

    NewNativeFormat := NativeFormat;

    if Failed(Frame.SetPixelFormat(NewNativeFormat)) then
      Exit(False);

    if CompareMem(@NewNativeFormat, @NativeFormat, SizeOf(TGuid)) then
    begin // Native Pixel Format
      if Failed(Frame.WritePixels(SourceSurface.Height, SourceSurface.Pitch, SourceSurface.Pitch * SourceSurface.Height,
        SourceSurface.Bits)) then
        Exit(False);

      if Failed(Frame.Commit) then
        Exit(False);

      Result := Succeeded(Encoder.Commit);
    end
    else
    begin // Pixel Format Conversion
      if Failed(FImagingFactory.CreateBitmapFromMemory(SourceSurface.Width, SourceSurface.Height, NativeFormat,
        SourceSurface.Pitch, SourceSurface.Pitch * SourceSurface.Height, SourceSurface.Bits, Bitmap)) or
        (Bitmap = nil) then
        Exit(False);

      if Failed(FImagingFactory.CreateFormatConverter(Converter)) or (Converter = nil) then
        Exit(False);

      if Failed(Converter.Initialize(Bitmap, NewNativeFormat, WICBitmapDitherTypeNone, nil, 0, 0)) then
        Exit(False);

      if Failed(Frame.WriteSource(Bitmap, nil)) then
        Exit(False);

      if Failed(Frame.Commit) then
        Exit(False);

      Result := Succeeded(Encoder.Commit);
    end;
  except
    Exit(False);
  end;
end;

{$ENDREGION}

end.
