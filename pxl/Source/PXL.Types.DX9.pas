unit PXL.Types.DX9;
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
  Jedi.Direct3D9, PXL.Types, PXL.Devices;

type
  TD3D9Support = (Undefined, Legacy, Vista);

  TDX9DeviceContextWriter = class abstract(TCustomDeviceContextWriter)
  private
    procedure SetSupport(const Value: TD3D9Support); virtual; abstract;

    procedure SetDisplayMode(const Value: D3DDISPLAYMODEEX); virtual; abstract;
    procedure SetPresentParams(const Value: D3DPRESENT_PARAMETERS); virtual; abstract;
    procedure SetCaps(const Value: D3DCaps9); virtual; abstract;

    procedure SetDirect3D(const Value: IDirect3D9); virtual; abstract;
    procedure SetDirect3DDevice(const Value: IDirect3DDevice9); virtual; abstract;
  public
    procedure ClearDisplayMode; virtual; abstract;
    procedure ClearPresentParams; virtual; abstract;
    procedure ClearCaps; virtual; abstract;

    property Support: TD3D9Support write SetSupport;

    property DisplayMode: D3DDISPLAYMODEEX write SetDisplayMode;
    property PresentParams: D3DPRESENT_PARAMETERS write SetPresentParams;
    property Caps: D3DCaps9 write SetCaps;

    property Direct3D: IDirect3D9 write SetDirect3D;
    property Direct3DDevice: IDirect3DDevice9 write SetDirect3DDevice;
  end;

  TDX9DeviceContext = class(TCustomDeviceContext)
  private type
    TWriter = class(TDX9DeviceContextWriter)
    protected
      procedure SetSupport(const Value: TD3D9Support); override;
      procedure SetDisplayMode(const Value: D3DDISPLAYMODEEX); override;
      procedure SetPresentParams(const Value: D3DPRESENT_PARAMETERS); override;
      procedure SetCaps(const Value: D3DCaps9); override;
      procedure SetDirect3D(const Value: IDirect3D9); override;
      procedure SetDirect3DDevice(const Value: IDirect3DDevice9); override;
    public
      procedure ClearDisplayMode; override;
      procedure ClearPresentParams; override;
      procedure ClearCaps; override;
    end;
  private
    FSupport: TD3D9Support;

    FDisplayMode: D3DDISPLAYMODEEX;
    FPresentParams: D3DPRESENT_PARAMETERS;
    FCaps: D3DCaps9;

    FDirect3D: IDirect3D9;
    FDirect3DDevice: IDirect3DDevice9;
  public
    constructor Create(const ADevice: TCustomDevice; out AWriter: TDX9DeviceContextWriter);

    function FindBackBufferFormat(Format: TPixelFormat): D3DFORMAT;
    function FindDepthStencilFormat(const DepthStencil: TDepthStencil): D3DFORMAT;
    procedure FindBestMultisampleType(const BackBufferFormat, DepthFormat: D3DFORMAT; const Multisamples: Integer;
      out SampleType: D3DMULTISAMPLE_TYPE; out QualityLevel: Cardinal);

    function FindTextureFormat(const ReqFormat: TPixelFormat; const Usage: Cardinal): TPixelFormat;
    function FindTextureFormatEx(const ReqFormat: TPixelFormat; const Usage1, Usage2: Cardinal): TPixelFormat;

    class function FormatToNative(const Format: D3DFORMAT): TPixelFormat; static;
    class function NativeToFormat(const Format: TPixelFormat): D3DFORMAT; static;

    property Support: TD3D9Support read FSupport;

    property DisplayMode: D3DDISPLAYMODEEX read FDisplayMode;
    property PresentParams: D3DPRESENT_PARAMETERS read FPresentParams;
    property Caps: D3DCaps9 read FCaps;

    property Direct3D: IDirect3D9 read FDirect3D;
    property Direct3DDevice: IDirect3DDevice9 read FDirect3DDevice;
  end;

implementation

uses
  Windows, PXL.Formats;

{$REGION 'TDX9DeviceContext.TWriter'}

procedure TDX9DeviceContext.TWriter.SetSupport(const Value: TD3D9Support);
begin
  TDX9DeviceContext(Context).FSupport := Value;
end;

procedure TDX9DeviceContext.TWriter.SetDisplayMode(const Value: D3DDISPLAYMODEEX);
begin
  TDX9DeviceContext(Context).FDisplayMode := Value;
end;

procedure TDX9DeviceContext.TWriter.SetPresentParams(const Value: D3DPRESENT_PARAMETERS);
begin
  TDX9DeviceContext(Context).FPresentParams := Value;
end;

procedure TDX9DeviceContext.TWriter.SetCaps(const Value: D3DCaps9);
begin
  TDX9DeviceContext(Context).FCaps := Value;
end;

procedure TDX9DeviceContext.TWriter.SetDirect3D(const Value: IDirect3D9);
begin
  TDX9DeviceContext(Context).FDirect3D := Value;
end;

procedure TDX9DeviceContext.TWriter.SetDirect3DDevice(const Value: IDirect3DDevice9);
begin
  TDX9DeviceContext(Context).FDirect3DDevice := Value;
end;

procedure TDX9DeviceContext.TWriter.ClearDisplayMode;
begin
  FillChar(TDX9DeviceContext(Context).FDisplayMode, SizeOf(D3DDISPLAYMODEEX), 0);
  TDX9DeviceContext(Context).FDisplayMode.Size := SizeOf(D3DDISPLAYMODEEX);
end;

procedure TDX9DeviceContext.TWriter.ClearPresentParams;
begin
  FillChar(TDX9DeviceContext(Context).FPresentParams, SizeOf(D3DPRESENT_PARAMETERS), 0);
end;

procedure TDX9DeviceContext.TWriter.ClearCaps;
begin
  FillChar(TDX9DeviceContext(Context).FCaps, SizeOf(D3DCaps9), 0);
end;

{$ENDREGION}
{$REGION 'TDX9DeviceContext'}

constructor TDX9DeviceContext.Create(const ADevice: TCustomDevice; out AWriter: TDX9DeviceContextWriter);
begin
  inherited Create(ADevice);

  AWriter := TWriter.Create(Self);
end;

function TDX9DeviceContext.FindBackBufferFormat(Format: TPixelFormat): D3DFORMAT;
const
  BackBufferFormats: array[0..5] of TPixelFormat = (
    TPixelFormat.A2R10G10B10, // 0
    TPixelFormat.A8R8G8B8,    // 1
    TPixelFormat.X8R8G8B8,    // 2
    TPixelFormat.A1R5G5B5,    // 3
    TPixelFormat.X1R5G5B5,    // 4
    TPixelFormat.R5G6B5);     // 5
var
  FormatList: TPixelFormatList;
  ModeFormat, TestFormat: D3DFORMAT;
  Sample: TPixelFormat;
  I: Integer;
begin
  if FDirect3D = nil then
    Exit(D3DFMT_UNKNOWN);

  if Format = TPixelFormat.Unknown then
    Format := TPixelFormat.A8R8G8B8;

  ModeFormat := FDisplayMode.Format;
  if ModeFormat = D3DFMT_UNKNOWN then
    ModeFormat := D3DFMT_X8R8G8B8;

  FormatList := TPixelFormatList.Create;
  try
    for I := Low(BackBufferFormats) to High(BackBufferFormats) do
    begin
      Sample := BackBufferFormats[I];

      TestFormat := NativeToFormat(Sample);
      if TestFormat = D3DFMT_UNKNOWN then
        Continue;

      if Succeeded(FDirect3D.CheckDeviceType(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, ModeFormat, TestFormat, True)) then
        FormatList.Insert(Sample);
    end;

    Result := NativeToFormat(FindClosestPixelFormat(Format, FormatList));
  finally
    FormatList.Free;
  end;
end;

function TDX9DeviceContext.FindDepthStencilFormat(const DepthStencil: TDepthStencil): D3DFORMAT;
const
  DepthStencilFormats: array[0..5] of TD3DFormat = (
    D3DFMT_D24S8,   // 0
    D3DFMT_D24X4S4, // 1
    D3DFMT_D15S1,   // 2
    D3DFMT_D32,     // 3
    D3DFMT_D24X8,   // 4
    D3DFMT_D16);    // 5
  FormatIndexes: array[0..1, 0..5] of Integer = ((3, 0, 1, 4, 5, 2), (0, 1, 2, 3, 4, 5));
var
  I: Integer;
begin
  if (FDirect3D = nil) or (DepthStencil <= TDepthStencil.None) then
    Exit(D3DFMT_UNKNOWN);

  for I := 0 to 5 do
  begin
    Result := DepthStencilFormats[FormatIndexes[Ord(DepthStencil) - 1, I]];

    if Succeeded(FDirect3D.CheckDeviceFormat(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, FDisplayMode.Format,
      D3DUSAGE_DEPTHSTENCIL, D3DRTYPE_SURFACE, Result)) then
      Exit;
  end;

  Result := D3DFMT_UNKNOWN;
end;

procedure TDX9DeviceContext.FindBestMultisampleType(const BackBufferFormat, DepthFormat: D3DFORMAT; const Multisamples: Integer;
  out SampleType: D3DMULTISAMPLE_TYPE; out QualityLevel: Cardinal);
var
  TempType: D3DMULTISAMPLE_TYPE;
  TempLevels: Cardinal;
  I: Integer;
begin
  SampleType := D3DMULTISAMPLE_NONE;
  QualityLevel := 0;

  if (FDirect3D = nil) or (Multisamples < 2) then
    Exit;

  for I := Multisamples downto 2 do
  begin
    TempType := D3DMULTISAMPLE_TYPE(I);

    if Failed(FDirect3D.CheckDeviceMultiSampleType(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, BackBufferFormat, True,
      TempType, @TempLevels)) then
      Continue;

    if (DepthFormat <> D3DFMT_UNKNOWN) and Failed(FDirect3D.CheckDeviceMultiSampleType(D3DADAPTER_DEFAULT,
      D3DDEVTYPE_HAL, DepthFormat, True, TempType, nil)) then
      Continue;

    SampleType := TempType;
    QualityLevel := TempLevels - 1;
    Break;
  end;
end;

function TDX9DeviceContext.FindTextureFormat(const ReqFormat: TPixelFormat; const Usage: Cardinal): TPixelFormat;
var
  FormatList: TPixelFormatList;
  Entry: TPixelFormat;
  DXFormat: D3DFORMAT;
begin
  if FDirect3D = nil then
    Exit(TPixelFormat.Unknown);

  FormatList := TPixelFormatList.Create;
  try
    for Entry := Low(TPixelFormat) to High(TPixelFormat) do
    begin
      DXFormat := NativeToFormat(Entry);
      if DXFormat = D3DFMT_UNKNOWN then
        Continue;

      if Succeeded(FDirect3D.CheckDeviceFormat(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, FDisplayMode.Format, Usage,
        D3DRTYPE_TEXTURE, DXFormat)) then
        FormatList.Insert(Entry);
    end;

    Result := FindClosestPixelFormat(ReqFormat, FormatList);
  finally
    FormatList.Free;
  end;
end;

function TDX9DeviceContext.FindTextureFormatEx(const ReqFormat: TPixelFormat; const Usage1, Usage2: Cardinal): TPixelFormat;
var
  FormatList: TPixelFormatList;
  Entry: TPixelFormat;
  DXFormat: D3DFORMAT;
begin
  if FDirect3D = nil then
    Exit(TPixelFormat.Unknown);

  FormatList := TPixelFormatList.Create;
  try
    for Entry := Low(TPixelFormat) to High(TPixelFormat) do
    begin
      DXFormat := NativeToFormat(Entry);
      if DXFormat = D3DFMT_UNKNOWN then
        Continue;

      if Failed(FDirect3D.CheckDeviceFormat(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, FDisplayMode.Format, Usage1,
        D3DRTYPE_TEXTURE, DXFormat)) then
        Continue;

      if Failed(FDirect3D.CheckDeviceFormat(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, FDisplayMode.Format, Usage2,
        D3DRTYPE_TEXTURE, DXFormat)) then
        Continue;

      FormatList.Insert(Entry);
    end;

    Result := FindClosestPixelFormat(ReqFormat, FormatList);
  finally
    FormatList.Free;
  end;
end;

class function TDX9DeviceContext.FormatToNative(const Format: D3DFORMAT): TPixelFormat;
begin
  case Format of
    D3DFMT_A8R8G8B8:
      Result := TPixelFormat.A8R8G8B8;

    D3DFMT_X8R8G8B8:
      Result := TPixelFormat.X8R8G8B8;

    D3DFMT_A8B8G8R8:
      Result := TPixelFormat.A8B8G8R8;

    D3DFMT_X8B8G8R8:
      Result := TPixelFormat.X8B8G8R8;

    D3DFMT_R8G8B8:
      Result := TPixelFormat.R8G8B8;

    D3DFMT_A4R4G4B4:
      Result := TPixelFormat.A4R4G4B4;

    D3DFMT_X4R4G4B4:
      Result := TPixelFormat.X4R4G4B4;

    D3DFMT_R5G6B5:
      Result := TPixelFormat.R5G6B5;

    D3DFMT_A1R5G5B5:
      Result := TPixelFormat.A1R5G5B5;

    D3DFMT_X1R5G5B5:
      Result := TPixelFormat.X1R5G5B5;

    D3DFMT_R3G3B2:
      Result := TPixelFormat.R3G3B2;

    D3DFMT_A8R3G3B2:
      Result := TPixelFormat.A8R3G3B2;

    D3DFMT_A2B10G10R10:
      Result := TPixelFormat.A2B10G10R10;

    D3DFMT_A2R10G10B10:
      Result := TPixelFormat.A2R10G10B10;

    D3DFMT_A16B16G16R16:
      Result := TPixelFormat.A16B16G16R16;

    D3DFMT_A8:
      Result := TPixelFormat.A8;

    D3DFMT_L8:
      Result := TPixelFormat.L8;

    D3DFMT_A4L4:
      Result := TPixelFormat.A4L4;

    D3DFMT_A8L8:
      Result := TPixelFormat.A8L8;

    D3DFMT_L16:
      Result := TPixelFormat.L16;

    D3DFMT_G16R16:
      Result := TPixelFormat.G16R16;

    D3DFMT_R16F:
      Result := TPixelFormat.R16F;

    D3DFMT_G16R16F:
      Result := TPixelFormat.G16R16F;

    D3DFMT_A16B16G16R16F:
      Result := TPixelFormat.A16B16G16R16F;

    D3DFMT_R32F:
      Result := TPixelFormat.R32F;

    D3DFMT_G32R32F:
      Result := TPixelFormat.G32R32F;

    D3DFMT_A32B32G32R32F:
      Result := TPixelFormat.A32B32G32R32F;

  else
    Result := TPixelFormat.Unknown;
  end;
end;

class function TDX9DeviceContext.NativeToFormat(const Format: TPixelFormat): D3DFORMAT;
begin
  case Format of
    TPixelFormat.A8R8G8B8:
      Result := D3DFMT_A8R8G8B8;

    TPixelFormat.X8R8G8B8:
      Result := D3DFMT_X8R8G8B8;

    TPixelFormat.A8B8G8R8:
      Result := D3DFMT_A8B8G8R8;

    TPixelFormat.X8B8G8R8:
      Result := D3DFMT_X8B8G8R8;

    TPixelFormat.R8G8B8:
      Result := D3DFMT_R8G8B8;

    TPixelFormat.A4R4G4B4:
      Result := D3DFMT_A4R4G4B4;

    TPixelFormat.X4R4G4B4:
      Result := D3DFMT_X4R4G4B4;

    TPixelFormat.R5G6B5:
      Result := D3DFMT_R5G6B5;

    TPixelFormat.A1R5G5B5:
      Result := D3DFMT_A1R5G5B5;

    TPixelFormat.X1R5G5B5:
      Result := D3DFMT_X1R5G5B5;

    TPixelFormat.R3G3B2:
      Result := D3DFMT_R3G3B2;

    TPixelFormat.A8R3G3B2:
      Result := D3DFMT_A8R3G3B2;

    TPixelFormat.A2B10G10R10:
      Result := D3DFMT_A2B10G10R10;

    TPixelFormat.A2R10G10B10:
      Result := D3DFMT_A2R10G10B10;

    TPixelFormat.A16B16G16R16:
      Result := D3DFMT_A16B16G16R16;

    TPixelFormat.A8:
      Result := D3DFMT_A8;

    TPixelFormat.L8:
      Result := D3DFMT_L8;

    TPixelFormat.A4L4:
      Result := D3DFMT_A4L4;

    TPixelFormat.A8L8:
      Result := D3DFMT_A8L8;

    TPixelFormat.L16:
      Result := D3DFMT_L16;

    TPixelFormat.G16R16:
      Result := D3DFMT_G16R16;

    TPixelFormat.R16F:
      Result := D3DFMT_R16F;

    TPixelFormat.G16R16F:
      Result := D3DFMT_G16R16F;

    TPixelFormat.A16B16G16R16F:
      Result := D3DFMT_A16B16G16R16F;

    TPixelFormat.R32F:
      Result := D3DFMT_R32F;

    TPixelFormat.G32R32F:
      Result := D3DFMT_G32R32F;

    TPixelFormat.A32B32G32R32F:
      Result := D3DFMT_A32B32G32R32F;

  else
    Result := D3DFMT_UNKNOWN;
  end;
end;

{$ENDREGION}

end.
