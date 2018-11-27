unit PXL.FMBridge;
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
  System.Classes, FMX.Types, FMX.Types3D, PXL.TypeDef, PXL.Types, PXL.Surfaces, PXL.Providers, PXL.ImageFormats;

type
  TFMBridge = class(TCustomImageFormatHandler)
  private
    FManager: TImageFormatManager;
    FScreenScale: VectorFloat;

    procedure UpdateScreenScale;
    function GetScreenScale: VectorFloat;
  protected
    function GetManager: TImageFormatManager; override;
    procedure RegisterExtensions; override;
  public
    constructor Create;
    destructor Destroy; override;

    function CreateProvider: TGraphicsDeviceProvider;

    function LoadFromStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
      const DestSurface: TPixelSurface; const AlphaFormatRequest: TAlphaFormatRequest): Boolean; override;

    function SaveToStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
      const SourceSurface: TPixelSurface; const Quality: Pointer): Boolean; override;

    property ScreenScale: VectorFloat read GetScreenScale;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
//  PXL.Providers.GL,//DONE 1: UNDO ME!
  PXL.Providers.FM.DX9, PXL.Providers.FM.DX11, FMX.Context.DX11,
{$ENDIF}

{$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
  PXL.Providers.FM.GL,
{$ENDIF}

{$IF DEFINED(ANDROID) OR DEFINED(IOS)}
  PXL.Providers.FM.GLES,
{$ENDIF}

  System.SysUtils, FMX.Platform, FMX.Surfaces, FMX.Graphics;

{$REGION 'Global Functions'}

{$IFDEF MSWINDOWS}
function IsDefaultDX11Context: Boolean;
var
  Context: TContext3D;
begin
  Context := TContextManager.DefaultContextClass.Create;
  try
    Result := Context is TCustomDX11Context;
  finally
    Context.Free;
  end;
end;
{$ENDIF}

function FMXToPixelFormat(const Format: FMX.Types.TPixelFormat): PXL.Types.TPixelFormat;
begin
  case Format of
    FMX.Types.TPixelFormat.RGB:
      Result := PXL.Types.TPixelFormat.X8B8G8R8;

    FMX.Types.TPixelFormat.RGBA:
      Result := PXL.Types.TPixelFormat.A8B8G8R8;

    FMX.Types.TPixelFormat.BGR:
      Result := PXL.Types.TPixelFormat.X8R8G8B8;

    FMX.Types.TPixelFormat.BGRA:
      Result := PXL.Types.TPixelFormat.A8R8G8B8;

    FMX.Types.TPixelFormat.RGBA16:
      Result := PXL.Types.TPixelFormat.A16B16G16R16;

    FMX.Types.TPixelFormat.BGR_565:
      Result := PXL.Types.TPixelFormat.R5G6B5;

    FMX.Types.TPixelFormat.BGRA4:
      Result := PXL.Types.TPixelFormat.A4R4G4B4;

    FMX.Types.TPixelFormat.BGR4:
      Result := PXL.Types.TPixelFormat.X4R4G4B4;

    FMX.Types.TPixelFormat.BGR5_A1:
      Result := PXL.Types.TPixelFormat.A1R5G5B5;

    FMX.Types.TPixelFormat.BGR5:
      Result := PXL.Types.TPixelFormat.X1R5G5B5;

    FMX.Types.TPixelFormat.BGR10_A2:
      Result := PXL.Types.TPixelFormat.A2R10G10B10;

    FMX.Types.TPixelFormat.RGB10_A2:
      Result := PXL.Types.TPixelFormat.A2B10G10R10;

    FMX.Types.TPixelFormat.L:
      Result := PXL.Types.TPixelFormat.L8;

    FMX.Types.TPixelFormat.LA:
      Result := PXL.Types.TPixelFormat.A8L8;

    FMX.Types.TPixelFormat.LA4:
      Result := PXL.Types.TPixelFormat.A4L4;

    FMX.Types.TPixelFormat.L16:
      Result := PXL.Types.TPixelFormat.L16;

    FMX.Types.TPixelFormat.A:
      Result := PXL.Types.TPixelFormat.A8;

    FMX.Types.TPixelFormat.R16F:
      Result := PXL.Types.TPixelFormat.R16F;

    FMX.Types.TPixelFormat.RG16F:
      Result := PXL.Types.TPixelFormat.G16R16F;

    FMX.Types.TPixelFormat.RGBA16F:
      Result := PXL.Types.TPixelFormat.A16B16G16R16F;

    FMX.Types.TPixelFormat.R32F:
      Result := PXL.Types.TPixelFormat.R32F;

    FMX.Types.TPixelFormat.RG32F:
      Result := PXL.Types.TPixelFormat.G32R32F;

    FMX.Types.TPixelFormat.RGBA32F:
      Result := PXL.Types.TPixelFormat.A32B32G32R32F;
  else
    Result := PXL.Types.TPixelFormat.Unknown;
  end;
end;

{$ENDREGION}
{$REGION 'TFMBridge'}

constructor TFMBridge.Create;
begin
  FManager := TImageFormatManager.Create;

  inherited Create(FManager);
end;

destructor TFMBridge.Destroy;
begin
  inherited;

  FManager.Free;
end;

function TFMBridge.GetManager: TImageFormatManager;
begin
  Result := FManager;
end;

procedure TFMBridge.UpdateScreenScale;
var
  ScreenService: IFMXScreenService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService) then
    FScreenScale := ScreenService.GetScreenScale
  else
    FScreenScale := 1.0;
end;

function TFMBridge.GetScreenScale: VectorFloat;
begin
  if Abs(FScreenScale) < VectorEpsilon then
    UpdateScreenScale;

  Result := FScreenScale;
end;

procedure TFMBridge.RegisterExtensions;
var
  Extensions, Extension: StdString;
  ExtPos, DotPos: Integer;
begin
  Extensions := TBitmapCodecManager.GetFileTypes;

  ExtPos := 0;
  while ExtPos < Extensions.Length do
  begin
    DotPos := Extensions.IndexOf(';', ExtPos);

    if DotPos <> -1 then
    begin
      Extension := Extensions.Substring(ExtPos, DotPos - ExtPos);
      ExtPos := DotPos + 1;
    end
    else
    begin
      Extension := Extensions.Substring(ExtPos);
      ExtPos := Extensions.Length;
    end;

    if (Extension.Length > 0) and (Extension.Chars[0] = '*') then
      Extension := Extension.Substring(1);

    if (Extension.Length > 0) and (Extension.Chars[0] <> '.') then
      Extension := '.' + Extension;

    RegisterExtension(Extension, nil);
  end;
end;

function TFMBridge.LoadFromStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
  const DestSurface: TPixelSurface; const AlphaFormatRequest: TAlphaFormatRequest): Boolean;
var
  Bitmap: TBitmapSurface;
  I, BytesToCopy: Integer;
begin
  Bitmap := TBitmapSurface.Create;
  try
    if not TBitmapCodecManager.LoadFromStream(Stream, Bitmap) then
      Exit(False);

    if not DestSurface.SetSize(Bitmap.Width, Bitmap.Height, FMXToPixelFormat(Bitmap.PixelFormat)) then
      Exit(False);

    BytesToCopy := Int64(DestSurface.Width) * DestSurface.BytesPerPixel;

    for I := 0 to Bitmap.Height - 1 do
      Move(Bitmap.Scanline[I]^, DestSurface.Scanline[I]^, BytesToCopy);
  finally
    Bitmap.Free;
  end;

  Result := True;
end;

function TFMBridge.SaveToStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
  const SourceSurface: TPixelSurface; const Quality: Pointer): Boolean;

  procedure SurfaceToBitmap(const Surface: TPixelSurface; const Bitmap: TBitmapSurface);
  var
    I, BytesToCopy: Integer;
  begin
    BytesToCopy := Int64(Surface.Width) * Surface.BytesPerPixel;

    for I := 0 to Surface.Height - 1 do
      Move(Surface.Scanline[I]^, Bitmap.Scanline[I]^, BytesToCopy);
  end;

var
  TempSurface: TPixelSurface;
  TempBitmap: TBitmapSurface;
  NewExt: StdString;
begin
  TempBitmap := TBitmapSurface.Create;
  try
    TempBitmap.SetSize(SourceSurface.Width, SourceSurface.Height, FMX.Types.TPixelFormat.BGRA);

    if SourceSurface.PixelFormat <> TPixelFormat.A8R8G8B8 then
    begin
      TempSurface := TPixelSurface.Create;
      try
        if not TempSurface.CopyFrom(SourceSurface) then
          Exit(False);

        if not TempSurface.ConvertPixelFormat(TPixelFormat.A8R8G8B8) then
          Exit(False);

        SurfaceToBitmap(TempSurface, TempBitmap);
      finally
        TempSurface.Free;
      end;
    end
    else
      SurfaceToBitmap(SourceSurface, TempBitmap);

    NewExt := Extension;
    if (NewExt.Length > 0) and (NewExt.Chars[0] <> '.') then
      NewExt := '.' + NewExt;

    Result := TBitmapCodecManager.SaveToStream(Stream, TempBitmap, NewExt);
  finally
    TempBitmap.Free;
  end;
end;

function TFMBridge.CreateProvider: TGraphicsDeviceProvider;
begin
{$IFDEF MSWINDOWS}
//  Result := TGLProvider.Create(FManager);//TODO 1: UNDO ME!

  if (TContextManager.ContextCount < 2) or (not IsDefaultDX11Context) then
    Result := TFireDX9Provider.Create(FManager)
  else
    Result := TFireDX11Provider.Create(FManager);
{$ENDIF}

{$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
  Result := TFireGLProvider.Create(FManager);
{$ENDIF}

{$IF DEFINED(ANDROID) OR DEFINED(IOS)}
  Result := TFireGLESProvider.Create(FManager);
{$ENDIF}
end;

{$ENDREGION}

initialization
  GlobalUseGPUCanvas := True;

end.
