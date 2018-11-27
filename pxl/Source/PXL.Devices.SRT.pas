unit PXL.Devices.SRT;
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

{ Enable the following option to render directly to form's surface (as with any other provider) by using GDI.
  Note that this option works only on Windows. }
{.$DEFINE SRT_RENDER_TO_GDI}

{$IF DEFINED(MSWINDOWS) AND DEFINED(SRT_RENDER_TO_GDI)}
  {$DEFINE SRT_RENDER_TO_GDI_ENABLED}
{$ENDIF}

uses
{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
  PXL.Surfaces.GDI,
{$ENDIF}

  PXL.Types, PXL.Devices, PXL.Surfaces, PXL.SwapChains, PXL.Types.SRT;

type
  TSRTDevice = class(TCustomSwapChainDevice)
  private
    FContext: TSRTDeviceContext;
    FContextWriter: TSRTDeviceContextWriter;

    FSurface: TPixelSurface;
  {$IFDEF SRT_RENDER_TO_GDI_ENABLED}
    FPrimarySurface: TGDIPixelSurface;
  {$ENDIF}
  protected
    function GetDeviceContext: TCustomDeviceContext; override;

    function InitDevice: Boolean; override;
    procedure DoneDevice; override;
  public
    constructor Create(const AProvider: TCustomDeviceProvider);
    destructor Destroy; override;

    function Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
      const StencilValue: Cardinal): Boolean; override;

    function BeginScene(const SwapChainIndex: Integer): Boolean; override;
    function EndScene: Boolean; override;

    property Surface: TPixelSurface read FSurface;

  {$IFDEF SRT_RENDER_TO_GDI_ENABLED}
    property PrimarySurface: TGDIPixelSurface read FPrimarySurface;
  {$ENDIF}
  end;

implementation

{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
uses
  Windows;
{$ENDIF}

constructor TSRTDevice.Create(const AProvider: TCustomDeviceProvider);
begin
  inherited;

  FContext := TSRTDeviceContext.Create(Self, FContextWriter);

  FTechnology := TDeviceTechnology.Software;
  FTechVersion := $100;

{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
  FPrimarySurface := TGDIPixelSurface.Create;
{$ELSE}
  FSurface := TPixelSurface.Create;
{$ENDIF}
end;

destructor TSRTDevice.Destroy;
begin
  inherited;
  
{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
  if FSurface <> FPrimarySurface then
    FSurface.Free;
  FPrimarySurface.Free;
{$ELSE}
  FSurface.Free;
{$ENDIF}

  FContext.Free;
end;

function TSRTDevice.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FContext;
end;

function TSRTDevice.InitDevice: Boolean;
{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
var
  SwapChainInfo: PSwapChainInfo;
{$ENDIF}
begin
{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
  SwapChainInfo := SwapChains[0];
  if SwapChainInfo = nil then
    Exit(False);

  FPrimarySurface.SetSize(SwapChainInfo.Width, SwapChainInfo.Height, SwapChainInfo.Format);

  if (SwapChainInfo.Format <> TPixelFormat.Unknown) and (FPrimarySurface.PixelFormat <> SwapChainInfo.Format) then
  begin
    if (FSurface = nil) or (FSurface = FPrimarySurface) then
      FSurface := TPixelSurface.Create;

    FSurface.SetSize(FPrimarySurface.Size, SwapChainInfo.Format);
  end
  else
  begin
    if (FSurface <> FPrimarySurface) and (FSurface <> nil) then
      FSurface.Free;

    FSurface := FPrimarySurface;
  end;

  FSurface.Clear(0);
{$ENDIF}

  FContextWriter.Surface := FSurface;
  FContextWriter.SurfaceSize := FSurface.Size;

  Result := True;
end;

procedure TSRTDevice.DoneDevice;
begin
  FContextWriter.Surface := nil;
end;

function TSRTDevice.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
  const StencilValue: Cardinal): Boolean;
begin
  if ClearTypes <> [TClearType.Color] then
    Exit(False);

  if (FSurface = nil) or FSurface.IsEmpty then
    Exit(False);

  FSurface.Clear(ColorValue);

  Result := True;
end;

function TSRTDevice.BeginScene(const SwapChainIndex: Integer): Boolean;
{$IFNDEF SRT_RENDER_TO_GDI_ENABLED}
var
  SwapChainInfo: PSwapChainInfo;
{$ENDIF}
begin
{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
  Result := SwapChainIndex = 0;
{$ELSE}
  SwapChainInfo := SwapChains[SwapChainIndex];
  if SwapChainInfo = nil then
    Exit(False);

  FSurface.SetSize(SwapChainInfo.Width, SwapChainInfo.Height, SwapChainInfo.Format);
  SwapChainInfo.Format := FSurface.PixelFormat;

  FContextWriter.SurfaceSize := FSurface.Size;

  Result := True;
{$ENDIF}
end;

function TSRTDevice.EndScene: Boolean;
{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
var
  SwapChainInfo: PSwapChainInfo;
  DestDC: TUntypedHandle;
{$ENDIF}
begin
{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
  SwapChainInfo := SwapChains[0];
  if SwapChainInfo = nil then
    Exit(False);

  if FSurface <> FPrimarySurface then
    FPrimarySurface.CopyFrom(FSurface);

  FPrimarySurface.ResetAlpha(False);

  DestDC := GetDC(SwapChainInfo.WindowHandle);
  if DestDC <> 0 then
  try
    FPrimarySurface.BitBlt(DestDC, ZeroPoint2px, FPrimarySurface.Size, ZeroPoint2px);
  finally
    ReleaseDC(SwapChainInfo.WindowHandle, DestDC);
  end;
{$ENDIF}

  Result := True;
end;

end.
