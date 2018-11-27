unit PXL.Devices.FM.DX11;
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
  PXL.TypeDef, PXL.Types, PXL.Devices, PXL.Types.DX11;

type
  TFireDX11Device = class(TCustomStateDevice)
  private
    FContext: TDX11DeviceContext;
    FContextWriter: TDX11DeviceContextWriter;

    procedure UpdateTechFeatureVersion;
    function ExtractFactory: Boolean;
  protected
    function GetDeviceContext: TCustomDeviceContext; override;

    function InitDevice: Boolean; override;
    procedure DoneDevice; override;
  public
    constructor Create(const AProvider: TCustomDeviceProvider);
    destructor Destroy; override;

    function Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
      const StencilValue: Cardinal): Boolean; override;
  end;

implementation

uses
  Winapi.Windows, System.SysUtils, PXL.Windows.D3DCommon, PXL.Windows.DXGI, PXL.Windows.D3D11, FMX.Context.DX11;

constructor TFireDX11Device.Create;
begin
  inherited;

  FContext := TDX11DeviceContext.Create(Self, FContextWriter);
  FTechnology := TDeviceTechnology.Direct3D;
end;

destructor TFireDX11Device.Destroy;
begin
  inherited;
  
  FContextWriter.Free;
  FContext.Free;
end;

function TFireDX11Device.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FContext;
end;

procedure TFireDX11Device.UpdateTechFeatureVersion;
begin
  FTechVersion := $B00;

  if Supports(FContext.Device, ID3D11Device1) then
    FTechVersion := $B10;

  case FContext.FeatureLevel of
    D3D_FEATURE_LEVEL_9_1:
      FTechFeatureVersion := $910;

    D3D_FEATURE_LEVEL_9_2:
      FTechFeatureVersion := $920;

    D3D_FEATURE_LEVEL_9_3:
      FTechFeatureVersion := $930;

    D3D_FEATURE_LEVEL_10_0:
      FTechFeatureVersion := $A00;

    D3D_FEATURE_LEVEL_10_1:
      FTechFeatureVersion := $A10;

    D3D_FEATURE_LEVEL_11_0:
      FTechFeatureVersion := $B00;

    D3D_FEATURE_LEVEL_11_1:
      FTechFeatureVersion := $B10;
  else
    FTechFeatureVersion := 0;
  end;

  FTechFeatures := [];

  if FContext.DriverType = D3D_DRIVER_TYPE_HARDWARE then
    FTechFeatures := FTechFeatures + [TTechnologyFeature.Hardware];

  if FContext.DriverType = D3D_DRIVER_TYPE_WARP then
    FTechFeatures := FTechFeatures + [TTechnologyFeature.Software];
end;

function TFireDX11Device.ExtractFactory: Boolean;
var
  Device1: IDXGIDevice1;
  Adapter1: IDXGIAdapter1;
  Factory1: IDXGIFactory1;
begin
  FContextWriter.Factory := nil;

  if Supports(FContext.Device, IDXGIDevice1, Device1) then
    if Succeeded(Device1.GetParent(IDXGIAdapter1, Adapter1)) and (Adapter1 <> nil) then
      if Succeeded(Adapter1.GetParent(IDXGIFactory1, Factory1)) and (Factory1 <> nil) then
        FContextWriter.Factory := Factory1;

  Result := FContext.Factory <> nil;
end;

function TFireDX11Device.InitDevice: Boolean;
begin
  if not LinkD3D11 then
    Exit(False);

  FContextWriter.Device := PXL.Windows.D3D11.ID3D11Device(TCustomDX11Context.SharedDevice);
  FContextWriter.DeviceContext := PXL.Windows.D3D11.ID3D11DeviceContext(TCustomDX11Context.SharedContext);

  if (FContext.Device = nil) or (FContext.Context = nil) or (not ExtractFactory) then
  begin
    FContextWriter.DeviceContext := nil;
    FContextWriter.Device := nil;
    Exit(False);
  end;

  FContextWriter.FeatureLevel := FContext.Device.GetFeatureLevel;

  UpdateTechFeatureVersion;
  Result := True;
end;

procedure TFireDX11Device.DoneDevice;
begin
  FContextWriter.Factory := nil;
  FContextWriter.DeviceContext := nil;
  FContextWriter.Device := nil;
end;

function TFireDX11Device.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
  const StencilValue: Cardinal): Boolean;
var
  ActiveRenderTarget: ID3D11RenderTargetView;
  ActiveDepthStencil: ID3D11DepthStencilView;
  ClearColor: TFourSingleArray;
  ClearFlags: Cardinal;
begin
  if (FContext = nil) or (FContext.Context = nil) or (ClearTypes = []) then
    Exit(False);

  Result := True;

  PushClearFPUState;
  try
    FContext.Context.OMGetRenderTargets(1, @ActiveRenderTarget, @ActiveDepthStencil);

    if TClearType.Color in ClearTypes then
      if ActiveRenderTarget <> nil then
      begin
        ClearColor[0] := ((ColorValue shr 16) and $FF) / 255.0;
        ClearColor[1] := ((ColorValue shr 8) and $FF) / 255.0;
        ClearColor[2] := (ColorValue and $FF) / 255.0;
        ClearColor[3] := ((ColorValue shr 24) and $FF) / 255.0;

        FContext.Context.ClearRenderTargetView(ActiveRenderTarget, ClearColor);
      end
      else
        Result := False;

    if (TClearType.Depth in ClearTypes) or (TClearType.Stencil in ClearTypes) then
      if ActiveDepthStencil <> nil then
      begin
        ClearFlags := 0;

        if TClearType.Depth in ClearTypes then
          ClearFlags := ClearFlags or Cardinal(Ord(D3D11_CLEAR_DEPTH));

        if TClearType.Stencil in ClearTypes then
          ClearFlags := ClearFlags or Cardinal(Ord(D3D11_CLEAR_STENCIL));

        FContext.Context.ClearDepthStencilView(ActiveDepthStencil, ClearFlags, DepthValue, StencilValue);
      end
      else
        Result := False;
  finally
    PopFPUState;
  end;
end;

end.
