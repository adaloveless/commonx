unit PXL.Types.DX11;
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
  PXL.Windows.D3DCommon, PXL.Windows.DXGI, PXL.Windows.D3D11, PXL.TypeDef, PXL.Types, PXL.Devices;

type
  TDX11DeviceContextWriter = class abstract(TCustomDeviceContextWriter)
  private
    procedure SetFactory(const Value: IDXGIFactory1); virtual; abstract;
    procedure SetDevice(const Value: ID3D11Device); virtual; abstract;
    procedure SetDeviceContext(const Value: ID3D11DeviceContext); virtual; abstract;

    procedure SetFeatureLevel(const Value: D3D_FEATURE_LEVEL); virtual; abstract;
    procedure SetDriverType(const Value: D3D_DRIVER_TYPE); virtual; abstract;
  public
    property Factory: IDXGIFactory1 write SetFactory;
    property Device: ID3D11Device write SetDevice;
    property DeviceContext: ID3D11DeviceContext write SetDeviceContext;

    property FeatureLevel: D3D_FEATURE_LEVEL write SetFeatureLevel;
    property DriverType: D3D_DRIVER_TYPE write SetDriverType;
  end;

  TDX11DeviceContext = class(TCustomDeviceContext)
  private type
    TWriter = class(TDX11DeviceContextWriter)
    protected
      procedure SetFactory(const Value: IDXGIFactory1); override;
      procedure SetDevice(const Value: ID3D11Device); override;
      procedure SetDeviceContext(const Value: ID3D11DeviceContext); override;
      procedure SetFeatureLevel(const Value: D3D_FEATURE_LEVEL); override;
      procedure SetDriverType(const Value: D3D_DRIVER_TYPE); override;
    end;
  private
    FFactory: IDXGIFactory1;
    FDevice: ID3D11Device;
    FContext: ID3D11DeviceContext;

    FFeatureLevel: D3D_FEATURE_LEVEL;
    FDriverType: D3D_DRIVER_TYPE;
  public
    constructor Create(const ADevice: TCustomDevice; out AWriter: TDX11DeviceContextWriter);

    function FindTextureFormat(const Format: TPixelFormat; const MipMapping: Boolean): TPixelFormat;
    function FindRenderTargetFormat(const Format: TPixelFormat; const MipMapping: Boolean): TPixelFormat;
    function FindDisplayFormat(const Format: TPixelFormat): TPixelFormat;
    function FindDepthStencilFormat(const DepthStencil: TDepthStencil): DXGI_FORMAT;
    procedure FindBestMultisampleType(const Format: DXGI_FORMAT; const Multisamples: Integer; out SampleCount,
      QualityLevel: Integer);

    class function NativeToFormat(const Format: TPixelFormat): DXGI_FORMAT; static;
    class function FormatToNative(const Format: DXGI_FORMAT): TPixelFormat; static;
    class function GetFormatBitDepth(const Format: DXGI_FORMAT): Integer; static;

    property Factory: IDXGIFactory1 read FFactory;
    property Device: ID3D11Device read FDevice;
    property Context: ID3D11DeviceContext read FContext;

    property FeatureLevel: D3D_FEATURE_LEVEL read FFeatureLevel;
    property DriverType: D3D_DRIVER_TYPE read FDriverType;
  end;

implementation

uses
  Windows, Math, PXL.Formats;

{$REGION 'TDX11DeviceContext.TWriter'}

procedure TDX11DeviceContext.TWriter.SetFactory(const Value: IDXGIFactory1);
begin
  TDX11DeviceContext(Context).FFactory := Value;
end;

procedure TDX11DeviceContext.TWriter.SetDevice(const Value: ID3D11Device);
begin
  TDX11DeviceContext(Context).FDevice := Value;
end;

procedure TDX11DeviceContext.TWriter.SetDeviceContext(const Value: ID3D11DeviceContext);
begin
  TDX11DeviceContext(Context).FContext := Value;
end;

procedure TDX11DeviceContext.TWriter.SetFeatureLevel(const Value: D3D_FEATURE_LEVEL);
begin
  TDX11DeviceContext(Context).FFeatureLevel := Value;
end;

procedure TDX11DeviceContext.TWriter.SetDriverType(const Value: D3D_DRIVER_TYPE);
begin
  TDX11DeviceContext(Context).FDriverType := Value;
end;

{$ENDREGION}
{$REGION 'TDX11DeviceContext'}

constructor TDX11DeviceContext.Create(const ADevice: TCustomDevice; out AWriter: TDX11DeviceContextWriter);
begin
  inherited Create(ADevice);

  AWriter := TWriter.Create(Self);
end;

function TDX11DeviceContext.FindTextureFormat(const Format: TPixelFormat; const MipMapping: Boolean): TPixelFormat;
var
  Supported: TPixelFormatList;
  Sample: TPixelFormat;
  TestFormat: DXGI_FORMAT;
  FormatSup: Cardinal;
begin
  if FDevice = nil then
    Exit(TPixelFormat.Unknown);

  Supported := TPixelFormatList.Create;
  try
    PushClearFPUState;
    try
      for Sample := Low(TPixelFormat) to High(TPixelFormat) do
      begin
        TestFormat := NativeToFormat(Sample);
        if TestFormat = DXGI_FORMAT_UNKNOWN then
          Continue;

        if Failed(FDevice.CheckFormatSupport(TestFormat, FormatSup)) then
          Continue;

        if FormatSup and D3D11_FORMAT_SUPPORT_TEXTURE2D = 0 then
          Continue;

        if MipMapping and (FormatSup and D3D11_FORMAT_SUPPORT_MIP = 0) then
          Continue;

        Supported.Insert(Sample);
      end;
    finally
      PopFPUState;
    end;

    Result := FindClosestPixelFormat(Format, Supported);
  finally
    Supported.Free;
  end;
end;

function TDX11DeviceContext.FindRenderTargetFormat(const Format: TPixelFormat;
  const MipMapping: Boolean): TPixelFormat;
var
  Supported: TPixelFormatList;
  Sample: TPixelFormat;
  TestFormat: DXGI_FORMAT;
  FormatSup: Cardinal;
begin
  if FDevice = nil then
    Exit(TPixelFormat.Unknown);

  Supported := TPixelFormatList.Create;
  try
    PushClearFPUState;
    try
      for Sample := Low(TPixelFormat) to High(TPixelFormat) do
      begin
        TestFormat := NativeToFormat(Sample);
        if TestFormat = DXGI_FORMAT_UNKNOWN then
          Continue;

        if Failed(FDevice.CheckFormatSupport(TestFormat, FormatSup)) then
          Continue;

        if FormatSup and D3D11_FORMAT_SUPPORT_TEXTURE2D = 0 then
          Continue;

        if FormatSup and D3D11_FORMAT_SUPPORT_RENDER_TARGET = 0 then
          Continue;

        if MipMapping then
        begin
          if FormatSup and D3D11_FORMAT_SUPPORT_MIP = 0 then
            Continue;

          if FormatSup and D3D11_FORMAT_SUPPORT_MIP_AUTOGEN = 0 then
            Continue;
        end;

        Supported.Insert(Sample);
      end;
    finally
      PopFPUState;
    end;

    Result := FindClosestPixelFormat(Format, Supported);
  finally
    Supported.Free;
  end;
end;

function TDX11DeviceContext.FindDisplayFormat(const Format: TPixelFormat): TPixelFormat;
var
  Supported: TPixelFormatList;
  Sample: TPixelFormat;
  TestFormat: DXGI_FORMAT;
  FormatSup: Cardinal;
begin
  if FDevice = nil then
    Exit(TPixelFormat.Unknown);

  Supported := TPixelFormatList.Create;
  try
    PushClearFPUState;
    try
      for Sample := Low(TPixelFormat) to High(TPixelFormat) do
      begin
        TestFormat := NativeToFormat(Sample);
        if TestFormat = DXGI_FORMAT_UNKNOWN then
          Continue;

        if Failed(FDevice.CheckFormatSupport(TestFormat, FormatSup)) then
          Continue;

        if FormatSup and D3D11_FORMAT_SUPPORT_DISPLAY = 0 then
          Continue;

        if FormatSup and D3D11_FORMAT_SUPPORT_BUFFER = 0 then
          Continue;

        if FormatSup and D3D11_FORMAT_SUPPORT_RENDER_TARGET = 0 then
          Continue;

        Supported.Insert(Sample);
      end;
    finally
      PopFPUState;
    end;

    Result := FindClosestPixelFormat(Format, Supported);
  finally
    Supported.Free;
  end;
end;

function TDX11DeviceContext.FindDepthStencilFormat(const DepthStencil: TDepthStencil): DXGI_FORMAT;
const
  DepthStencilFormats: array[0..3] of DXGI_FORMAT = (DXGI_FORMAT_D32_FLOAT_S8X24_UINT, DXGI_FORMAT_D24_UNORM_S8_UINT,
    DXGI_FORMAT_D32_FLOAT, DXGI_FORMAT_D16_UNORM);
  FormatIndexes: array[TDepthStencil, 0..3] of Integer = ((-1, -1, -1, -1), (2, 0, 1, 3), (0, 1, 2, 3));
var
  I: Integer;
  Format: DXGI_FORMAT;
  FormatSup: Cardinal;
begin
  Result := DXGI_FORMAT_UNKNOWN;
  if (DepthStencil <= TDepthStencil.None) or (FDevice = nil) then
    Exit;

  for I := 0 to 3 do
  begin
    Format := DepthStencilFormats[FormatIndexes[DepthStencil, I]];
    if Failed(FDevice.CheckFormatSupport(Format, FormatSup)) then
      Continue;

    if (FormatSup and D3D11_FORMAT_SUPPORT_TEXTURE2D > 0) and
      (FormatSup and D3D11_FORMAT_SUPPORT_DEPTH_STENCIL > 0) then
      Exit(Format);
  end;
end;

procedure TDX11DeviceContext.FindBestMultisampleType(const Format: DXGI_FORMAT; const Multisamples: Integer;
  out SampleCount, QualityLevel: Integer);
var
  I, MaxSampleNo: Integer;
  QuaLevels: Cardinal;
begin
  SampleCount := 1;
  QualityLevel := 0;

  if (FDevice = nil) or (Multisamples < 2) or (Format = DXGI_FORMAT_UNKNOWN) then
    Exit;

  MaxSampleNo := Min(Multisamples, D3D11_MAX_MULTISAMPLE_SAMPLE_COUNT);

  PushClearFPUState;
  try
    for I := MaxSampleNo downto 2 do
    begin
      if Failed(FDevice.CheckMultisampleQualityLevels(Format, I, QuaLevels)) then
        Continue;

      if QuaLevels > 0 then
      begin
        SampleCount := I;
        QualityLevel := QuaLevels - 1;
        Break;
      end;
    end;
  finally
    PopFPUState;
  end;
end;

class function TDX11DeviceContext.NativeToFormat(const Format: TPixelFormat): DXGI_FORMAT;
begin
  case Format of
    TPixelFormat.A8R8G8B8:
      Result := DXGI_FORMAT_B8G8R8A8_UNORM;

    TPixelFormat.X8R8G8B8:
      Result := DXGI_FORMAT_B8G8R8X8_UNORM;

    TPixelFormat.A4R4G4B4:
      Result := DXGI_FORMAT_B4G4R4A4_UNORM;

    TPixelFormat.R5G6B5:
      Result := DXGI_FORMAT_B5G6R5_UNORM;

    TPixelFormat.A1R5G5B5:
      Result := DXGI_FORMAT_B5G5R5A1_UNORM;

    TPixelFormat.A8:
      Result := DXGI_FORMAT_A8_UNORM;

    TPixelFormat.A2B10G10R10:
      Result := DXGI_FORMAT_R10G10B10A2_UNORM;

    TPixelFormat.G16R16:
      Result := DXGI_FORMAT_R16G16_UNORM;

    TPixelFormat.A16B16G16R16:
      Result := DXGI_FORMAT_R16G16B16A16_UNORM;

    TPixelFormat.L8:
      Result := DXGI_FORMAT_R8_UNORM;

    TPixelFormat.A8L8:
      Result := DXGI_FORMAT_R8G8_UNORM;

    TPixelFormat.L16:
      Result := DXGI_FORMAT_R16_UNORM;

    TPixelFormat.R16F:
      Result := DXGI_FORMAT_R16_FLOAT;

    TPixelFormat.G16R16F:
      Result := DXGI_FORMAT_R16G16_FLOAT;

    TPixelFormat.A16B16G16R16F:
      Result := DXGI_FORMAT_R16G16B16A16_FLOAT;

    TPixelFormat.R32F:
      Result := DXGI_FORMAT_R32_FLOAT;

    TPixelFormat.G32R32F:
      Result := DXGI_FORMAT_R32G32_FLOAT;

    TPixelFormat.A32B32G32R32F:
      Result := DXGI_FORMAT_R32G32B32A32_FLOAT;

    TPixelFormat.A8B8G8R8:
      Result := DXGI_FORMAT_R8G8B8A8_UNORM;

    TPixelFormat.I8:
      Result := DXGI_FORMAT_R8_UNORM;

    else
      Result := DXGI_FORMAT_UNKNOWN;
  end;
end;

class function TDX11DeviceContext.FormatToNative(const Format: DXGI_FORMAT): TPixelFormat;
begin
  case Format of
    DXGI_FORMAT_B8G8R8A8_UNORM:
      Result := TPixelFormat.A8R8G8B8;

    DXGI_FORMAT_B8G8R8X8_UNORM:
      Result := TPixelFormat.X8R8G8B8;

    DXGI_FORMAT_B5G6R5_UNORM:
      Result := TPixelFormat.R5G6B5;

    DXGI_FORMAT_B5G5R5A1_UNORM:
      Result := TPixelFormat.A1R5G5B5;

    DXGI_FORMAT_A8_UNORM:
      Result := TPixelFormat.A8;

    DXGI_FORMAT_R10G10B10A2_UNORM:
      Result := TPixelFormat.A2B10G10R10;

    DXGI_FORMAT_R16G16_UNORM:
      Result := TPixelFormat.G16R16;

    DXGI_FORMAT_R16G16B16A16_UNORM:
      Result := TPixelFormat.A16B16G16R16;

    DXGI_FORMAT_R16_FLOAT:
      Result := TPixelFormat.R16F;

    DXGI_FORMAT_R16G16_FLOAT:
      Result := TPixelFormat.G16R16F;

    DXGI_FORMAT_R16G16B16A16_FLOAT:
      Result := TPixelFormat.A16B16G16R16F;

    DXGI_FORMAT_R32_FLOAT:
      Result := TPixelFormat.R32F;

    DXGI_FORMAT_R32G32_FLOAT:
      Result := TPixelFormat.G32R32F;

    DXGI_FORMAT_R32G32B32A32_FLOAT:
      Result := TPixelFormat.A32B32G32R32F;

    DXGI_FORMAT_R8G8B8A8_UNORM:
      Result := TPixelFormat.A8B8G8R8;

    else
      Result := TPixelFormat.Unknown;
  end;
end;

class function TDX11DeviceContext.GetFormatBitDepth(const Format: DXGI_FORMAT): Integer;
begin
  case Format of
    DXGI_FORMAT_R32G32B32A32_TYPELESS,
    DXGI_FORMAT_R32G32B32A32_FLOAT,
    DXGI_FORMAT_R32G32B32A32_UINT,
    DXGI_FORMAT_R32G32B32A32_SINT:
      Result := 128;

    DXGI_FORMAT_R32G32B32_TYPELESS,
    DXGI_FORMAT_R32G32B32_FLOAT,
    DXGI_FORMAT_R32G32B32_UINT,
    DXGI_FORMAT_R32G32B32_SINT:
      Result := 96;

    DXGI_FORMAT_R16G16B16A16_TYPELESS,
    DXGI_FORMAT_R16G16B16A16_FLOAT,
    DXGI_FORMAT_R16G16B16A16_UNORM,
    DXGI_FORMAT_R16G16B16A16_UINT,
    DXGI_FORMAT_R16G16B16A16_SNORM,
    DXGI_FORMAT_R16G16B16A16_SINT,
    DXGI_FORMAT_R32G32_TYPELESS,
    DXGI_FORMAT_R32G32_FLOAT,
    DXGI_FORMAT_R32G32_UINT,
    DXGI_FORMAT_R32G32_SINT,
    DXGI_FORMAT_R32G8X24_TYPELESS,
    DXGI_FORMAT_D32_FLOAT_S8X24_UINT,
    DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS,
    DXGI_FORMAT_X32_TYPELESS_G8X24_UINT:
      Result := 64;

    DXGI_FORMAT_R10G10B10A2_TYPELESS,
    DXGI_FORMAT_R10G10B10A2_UNORM,
    DXGI_FORMAT_R10G10B10A2_UINT,
    DXGI_FORMAT_R11G11B10_FLOAT,
    DXGI_FORMAT_R8G8B8A8_TYPELESS,
    DXGI_FORMAT_R8G8B8A8_UNORM,
    DXGI_FORMAT_R8G8B8A8_UNORM_SRGB,
    DXGI_FORMAT_R8G8B8A8_UINT,
    DXGI_FORMAT_R8G8B8A8_SNORM,
    DXGI_FORMAT_R8G8B8A8_SINT,
    DXGI_FORMAT_R16G16_TYPELESS,
    DXGI_FORMAT_R16G16_FLOAT,
    DXGI_FORMAT_R16G16_UNORM,
    DXGI_FORMAT_R16G16_UINT,
    DXGI_FORMAT_R16G16_SNORM,
    DXGI_FORMAT_R16G16_SINT,
    DXGI_FORMAT_R32_TYPELESS,
    DXGI_FORMAT_D32_FLOAT,
    DXGI_FORMAT_R32_FLOAT,
    DXGI_FORMAT_R32_UINT,
    DXGI_FORMAT_R32_SINT,
    DXGI_FORMAT_R24G8_TYPELESS,
    DXGI_FORMAT_D24_UNORM_S8_UINT,
    DXGI_FORMAT_R24_UNORM_X8_TYPELESS,
    DXGI_FORMAT_X24_TYPELESS_G8_UINT,
    DXGI_FORMAT_B8G8R8A8_UNORM,
    DXGI_FORMAT_B8G8R8X8_UNORM,
    DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM,
    DXGI_FORMAT_B8G8R8A8_TYPELESS,
    DXGI_FORMAT_B8G8R8A8_UNORM_SRGB,
    DXGI_FORMAT_B8G8R8X8_TYPELESS,
    DXGI_FORMAT_B8G8R8X8_UNORM_SRGB,
    DXGI_FORMAT_R9G9B9E5_SHAREDEXP:
      Result := 32;

    DXGI_FORMAT_R8G8_TYPELESS,
    DXGI_FORMAT_R8G8_UNORM,
    DXGI_FORMAT_R8G8_UINT,
    DXGI_FORMAT_R8G8_SNORM,
    DXGI_FORMAT_R8G8_SINT,
    DXGI_FORMAT_R16_TYPELESS,
    DXGI_FORMAT_R16_FLOAT,
    DXGI_FORMAT_D16_UNORM,
    DXGI_FORMAT_R16_UNORM,
    DXGI_FORMAT_R16_UINT,
    DXGI_FORMAT_R16_SNORM,
    DXGI_FORMAT_R16_SINT,
    DXGI_FORMAT_B5G6R5_UNORM,
    DXGI_FORMAT_B5G5R5A1_UNORM,
    DXGI_FORMAT_R8G8_B8G8_UNORM,
    DXGI_FORMAT_G8R8_G8B8_UNORM:
      Result := 16;

    DXGI_FORMAT_R8_TYPELESS,
    DXGI_FORMAT_R8_UNORM,
    DXGI_FORMAT_R8_UINT,
    DXGI_FORMAT_R8_SNORM,
    DXGI_FORMAT_R8_SINT,
    DXGI_FORMAT_A8_UNORM:
      Result := 8;

    DXGI_FORMAT_R1_UNORM:
      Result := 1;

    DXGI_FORMAT_BC1_TYPELESS,
    DXGI_FORMAT_BC1_UNORM,
    DXGI_FORMAT_BC1_UNORM_SRGB,
    DXGI_FORMAT_BC4_TYPELESS,
    DXGI_FORMAT_BC4_UNORM,
    DXGI_FORMAT_BC4_SNORM:
      Result := 4;

    DXGI_FORMAT_BC2_TYPELESS,
    DXGI_FORMAT_BC2_UNORM,
    DXGI_FORMAT_BC2_UNORM_SRGB,
    DXGI_FORMAT_BC3_TYPELESS,
    DXGI_FORMAT_BC3_UNORM,
    DXGI_FORMAT_BC3_UNORM_SRGB,
    DXGI_FORMAT_BC5_TYPELESS,
    DXGI_FORMAT_BC5_UNORM,
    DXGI_FORMAT_BC5_SNORM,
    DXGI_FORMAT_BC6H_TYPELESS,
    DXGI_FORMAT_BC6H_UF16,
    DXGI_FORMAT_BC6H_SF16,
    DXGI_FORMAT_BC7_TYPELESS,
    DXGI_FORMAT_BC7_UNORM,
    DXGI_FORMAT_BC7_UNORM_SRGB:
      Result := 8;

    else
      Result := 0;
  end;
end;

{$ENDREGION}

end.
