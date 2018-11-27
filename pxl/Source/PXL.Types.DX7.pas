unit PXL.Types.DX7;
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
  PXL.Windows.DDraw, PXL.Windows.D3D7, PXL.Types, PXL.Devices;

type
  TDX7DeviceContextWriter = class abstract(TCustomDeviceContextWriter)
  protected
    procedure SetDD7Object(const Value: IDirectDraw7); virtual; abstract;
    procedure SetD3D7Object(const Value: IDirect3D7); virtual; abstract;
    procedure SetD3D7Device(const Value: IDirect3DDevice7); virtual; abstract;
  public
    property DD7Object: IDirectDraw7 write SetDD7Object;
    property D3D7Object: IDirect3D7 write SetD3D7Object;
    property D3D7Device: IDirect3DDevice7 write SetD3D7Device;
  end;

  TDX7DeviceContext = class(TCustomDeviceContext)
  private type
    TWriter = class(TDX7DeviceContextWriter)
    protected
      procedure SetDD7Object(const Value: IDirectDraw7); override;
      procedure SetD3D7Object(const Value: IDirect3D7); override;
      procedure SetD3D7Device(const Value: IDirect3DDevice7); override;
    end;
  private
    FDD7Object: IDirectDraw7;
    FD3D7Object: IDirect3D7;
    FD3D7Device: IDirect3DDevice7;
  public
    constructor Create(const ADevice: TCustomDevice; out AWriter: TDX7DeviceContextWriter);

    function FindBackBufferFormat(const Width, Height: Integer; const Format: TPixelFormat): TPixelFormat;
    function FindTextureFormat(const Format: TPixelFormat): TPixelFormat;

    class function FormatToNative(const Format: DDPIXELFORMAT): TPixelFormat; static;
    class function NativeToFormat(const PixelFormat: TPixelFormat; var DDPixelFormat: DDPIXELFORMAT): Boolean; static;

    property DD7Object: IDirectDraw7 read FDD7Object;
    property D3D7Object: IDirect3D7 read FD3D7Object;
    property D3D7Device: IDirect3DDevice7 read FD3D7Device;
  end;

implementation

uses
  Windows, PXL.Formats;

{$REGION 'Global Types and Functions'}

const
  ZeroColorBits: TPixelFormatColorBits = (Count: 0; Shift: 0);

function LearnBitMask(const BitMask: Cardinal): TPixelFormatColorBits;
var
  BitPos: Integer;
begin
  BitPos := 0;

  while (BitPos < 32) and (BitMask and (1 shl BitPos) = 0) do
    Inc(BitPos);

  if BitPos >= 32 then
    Exit(ZeroColorBits);

  Result.Count := 0;
  Result.Shift := BitPos;

  while (BitPos < 32) and (BitMask and (1 shl BitPos) > 0) do
  begin
    Inc(BitPos);
    Inc(Result.Count);
  end;
end;

function CreateBitMask(const ColorBits: TPixelFormatColorBits): Cardinal;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to ColorBits.Count - 1 do
    Result := Result or Cardinal(1 shl (ColorBits.Shift + I));
end;

function EnumBackBufferCallback(Surface: IDirectDrawSurface7; const SurfaceDesc: DDSURFACEDESC2;
  Context: Pointer): HResult; stdcall;
var
  Format: TPixelFormat;
begin
  Result := DDENUMRET_OK;

  Format := TDX7DeviceContext.FormatToNative(SurfaceDesc.ddpfPixelFormat);
  if Format <> TPixelFormat.Unknown then
    TPixelFormatList(Context).Include(Format);
end;

function EnumTextureCallback(var DDPixelFormat: DDPIXELFORMAT; Context: Pointer): HResult; stdcall;
var
  Format: TPixelFormat;
begin
  Result := DDENUMRET_OK;

  Format := TDX7DeviceContext.FormatToNative(DDPixelFormat);
  if Format <> TPixelFormat.Unknown then
    TPixelFormatList(Context).Include(Format);
end;

{$ENDREGION}
{$REGION 'TDX7DeviceContext.TWriter'}

procedure TDX7DeviceContext.TWriter.SetDD7Object(const Value: IDirectDraw7);
begin
  TDX7DeviceContext(Context).FDD7Object := Value;
end;

procedure TDX7DeviceContext.TWriter.SetD3D7Object(const Value: IDirect3D7);
begin
  TDX7DeviceContext(Context).FD3D7Object := Value;
end;

procedure TDX7DeviceContext.TWriter.SetD3D7Device(const Value: IDirect3DDevice7);
begin
  TDX7DeviceContext(Context).FD3D7Device := Value;
end;

{$ENDREGION}
{$REGION 'TDX7DeviceContext.TWriter'}

constructor TDX7DeviceContext.Create(const ADevice: TCustomDevice; out AWriter: TDX7DeviceContextWriter);
begin
  inherited Create(ADevice);

  AWriter := TWriter.Create(Self);
end;

function TDX7DeviceContext.FindBackBufferFormat(const Width, Height: Integer;
  const Format: TPixelFormat): TPixelFormat;
var
  FormatList: TPixelFormatList;
  SurfaceDesc: DDSURFACEDESC2;
begin
  if FDD7Object = nil then
    Exit(TPixelFormat.Unknown);

  FillChar(SurfaceDesc, SizeOf(DDSURFACEDESC2), 0);

  SurfaceDesc.dwSize := SizeOf(DDSURFACEDESC2);
  SurfaceDesc.dwFlags := DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or DDSD_PIXELFORMAT;
  SurfaceDesc.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_3DDEVICE;
  SurfaceDesc.dwWidth := Width;
  SurfaceDesc.dwHeight := Height;
  SurfaceDesc.ddpfPixelFormat.dwFlags := DDPF_RGB;

  FormatList := TPixelFormatList.Create;
  try
    if Failed(FDD7Object.EnumSurfaces(DDENUMSURFACES_CANBECREATED or DDENUMSURFACES_MATCH, SurfaceDesc, FormatList,
      EnumBackBufferCallback)) then
      Exit(TPixelFormat.Unknown);

    Result := FindClosestPixelFormat(Format, FormatList);
  finally
    FormatList.Free;
  end;
end;

function TDX7DeviceContext.FindTextureFormat(const Format: TPixelFormat): TPixelFormat;
var
  FormatList: TPixelFormatList;
begin
  if FDD7Object = nil then
    Exit(TPixelFormat.Unknown);

  FormatList := TPixelFormatList.Create;
  try
    if Failed(FD3D7Device.EnumTextureFormats(EnumTextureCallback, FormatList)) then
      Exit(TPixelFormat.Unknown);

    Result := FindClosestPixelFormat(Format, FormatList);
  finally
    FormatList.Free;
  end;
end;

class function TDX7DeviceContext.FormatToNative(const Format: DDPIXELFORMAT): TPixelFormat;
var
  Desc: TPixelFormatDescription;
begin
  FillChar(Desc, SizeOf(TPixelFormatDescription), 0);

  if Format.dwFlags and DDPF_RGB > 0 then
  begin
    Desc.FormatType := TPixelFormatType.Normal;

    Desc.RedBits := LearnBitMask(Format.dwRBitMask);
    Desc.GreenBits := LearnBitMask(Format.dwGBitMask);
    Desc.BlueBits := LearnBitMask(Format.dwBBitMask);
    Desc.AlphaBits := LearnBitMask(Format.dwRGBAlphaBitMask);
    Desc.BitCount := Format.dwRGBBitCount;

    Desc.CalculateUsedBitCount;
  end
  else if Format.dwFlags and DDPF_LUMINANCE > 0 then
  begin
    Desc.FormatType := TPixelFormatType.Luminance;

    Desc.LuminanceBits := LearnBitMask(Format.dwLuminanceBitMask);
    Desc.AlphaBits := LearnBitMask(Format.dwLuminanceAlphaBitMask);
    Desc.BitCount := Format.dwLuminanceBitCount;

    Desc.CalculateUsedBitCount;
  end
  else if Format.dwFlags and DDPF_ALPHA > 0 then
  begin
    Desc.FormatType := TPixelFormatType.Normal;

    Desc.AlphaBits.Count := Format.dwAlphaBitDepth;
    Desc.BitCount := Desc.AlphaBits.Count;
    Desc.UsedBitCount := Desc.BitCount;
  end
  else
    Exit(TPixelFormat.Unknown);

  Result := GetMatchingPixelFormat(Desc);
end;

class function TDX7DeviceContext.NativeToFormat(const PixelFormat: TPixelFormat;
  var DDPixelFormat: DDPIXELFORMAT): Boolean;
var
  Desc: TPixelFormatDescription;
begin
  FillChar(DDPixelFormat, SizeOf(DDPIXELFORMAT), 0);
  DDPixelFormat.dwSize := SizeOf(DDPIXELFORMAT);

  if PixelFormat = TPixelFormat.A8 then
  begin
    DDPixelFormat.dwFlags := DDPF_ALPHA;
    DDPixelFormat.dwAlphaBitDepth := 8;
    Exit(True);
  end;

  if not GetPixelFormatDescription(PixelFormat, Desc) then
    Exit(False);

  case Desc.FormatType of
    TPixelFormatType.Normal:
      begin
        DDPixelFormat.dwFlags := DDPF_RGB;
        DDPixelFormat.dwRGBBitCount := Desc.BitCount;

        if Desc.AlphaBits.Count > 0 then
          DDPixelFormat.dwFlags := DDPixelFormat.dwFlags or DDPF_ALPHAPIXELS;

        DDPixelFormat.dwRBitMask := CreateBitMask(Desc.RedBits);
        DDPixelFormat.dwGBitMask := CreateBitMask(Desc.GreenBits);
        DDPixelFormat.dwBBitMask := CreateBitMask(Desc.BlueBits);
        DDPixelFormat.dwRGBAlphaBitMask := CreateBitMask(Desc.AlphaBits);

        Result := True;
      end;

    TPixelFormatType.Luminance:
      begin
        DDPixelFormat.dwFlags := DDPF_LUMINANCE;
        DDPixelFormat.dwLuminanceBitCount := Desc.BitCount;

        if Desc.AlphaBits.Count > 0 then
          DDPixelFormat.dwFlags := DDPixelFormat.dwFlags or DDPF_ALPHAPIXELS;

        DDPixelFormat.dwLuminanceBitMask := CreateBitMask(Desc.LuminanceBits);
        DDPixelFormat.dwLuminanceAlphaBitMask := CreateBitMask(Desc.AlphaBits);

        Result := True;
      end;
  else
    Result := False;
  end;
end;

{$ENDREGION}

end.
