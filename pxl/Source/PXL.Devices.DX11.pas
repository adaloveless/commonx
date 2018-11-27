unit PXL.Devices.DX11;
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
  Classes, PXL.TypeDef, PXL.Types, PXL.Devices, PXL.SwapChains, PXL.Types.DX11, PXL.SwapChains.DX11;

type
  TDX11Device = class(TCustomSwapChainDevice)
  private
    FContext: TDX11DeviceContext;
    FContextWriter: TDX11DeviceContextWriter;
    FDXSwapChains: TDX11SwapChains;
    FCurrentSwapChain: Integer;
    FPreferSoftwareMode: Boolean;

    procedure UpdateTechFeatureVersion;

    function ExtractFactory: Boolean;
    procedure ReleaseFactory;

    function CreateHALDevice: Boolean;
    function CreateWARPDevice: Boolean;

    function CreateDeviceContext: Boolean;
    procedure ReleaseDeviceContext;

    function UpdateWindowAssociation: Boolean;
  protected
    function GetDeviceContext: TCustomDeviceContext; override;

    function InitDevice: Boolean; override;
    procedure DoneDevice; override;

    function ResizeSwapChain(const SwapChainIndex: Integer; const NewSwapChainInfo: PSwapChainInfo): Boolean; override;
    function MayRender(const SwapChainIndex: Integer): Boolean; override;
  public
    constructor Create(const AProvider: TCustomDeviceProvider);
    destructor Destroy; override;

    function Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
      const StencilValue: Cardinal): Boolean; override;

    procedure Reset; override;

    function BeginScene(const SwapChainIndex: Integer = 0): Boolean; override;
    function EndScene: Boolean; override;

    property DXSwapChains: TDX11SwapChains read FDXSwapChains;
    property PreferSoftwareMode: Boolean read FPreferSoftwareMode write FPreferSoftwareMode;
  end;

implementation

uses
  Windows, SysUtils, PXL.Windows.D3DCommon, PXL.Windows.DXGI, PXL.Windows.D3D11;

{$REGION 'Global Types and Functions'}

const
  SearchFeatureLevels: array[0..5] of D3D_FEATURE_LEVEL = (D3D_FEATURE_LEVEL_11_0, D3D_FEATURE_LEVEL_10_1,
    D3D_FEATURE_LEVEL_10_0, D3D_FEATURE_LEVEL_9_3, D3D_FEATURE_LEVEL_9_2, D3D_FEATURE_LEVEL_9_1);

  BestFeatureLevel: D3D_FEATURE_LEVEL = D3D_FEATURE_LEVEL_11_1;

  DefaultDeviceCreationFlags = {$IFDEF DX11DEBUG} Ord(D3D11_CREATE_DEVICE_DEBUG) {$ELSE} 0 {$ENDIF};

{$ENDREGION}
{$REGION 'TDX11Device'}

constructor TDX11Device.Create;
begin
  inherited;

  FContext := TDX11DeviceContext.Create(Self, FContextWriter);
  FDXSwapChains := TDX11SwapChains.Create(FContext);

  FTechnology := TDeviceTechnology.Direct3D;
  FCurrentSwapChain := -1;
end;

destructor TDX11Device.Destroy;
begin
  inherited;

  FDXSwapChains.Free;
  FContextWriter.Free;
  FContext.Free;
end;

function TDX11Device.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FContext;
end;

function TDX11Device.ExtractFactory: Boolean;
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

procedure TDX11Device.ReleaseFactory;
begin
  FContextWriter.Factory := nil;
end;

procedure TDX11Device.UpdateTechFeatureVersion;
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

function TDX11Device.CreateHALDevice: Boolean;
var
  Device: ID3D11Device;
  Context: ID3D11DeviceContext;
  FeatureLevel: D3D_FEATURE_LEVEL;
begin
  PushClearFPUState;
  try
    if Failed(D3D11CreateDevice(nil, D3D_DRIVER_TYPE_HARDWARE, 0, DefaultDeviceCreationFlags, @BestFeatureLevel, 1,
      D3D11_SDK_VERSION, @Device, @FeatureLevel, @Context)) then
      if Failed(D3D11CreateDevice(nil, D3D_DRIVER_TYPE_HARDWARE, 0, DefaultDeviceCreationFlags, @SearchFeatureLevels[0],
        High(SearchFeatureLevels) + 1, D3D11_SDK_VERSION, @Device, @FeatureLevel, @Context)) then
        Exit(False);
  finally
    PopFPUState;
  end;

  FContextWriter.DriverType := D3D_DRIVER_TYPE_HARDWARE;
  FContextWriter.FeatureLevel := FeatureLevel;
  FContextWriter.Device := Device;
  FContextWriter.DeviceContext := Context;

  UpdateTechFeatureVersion;
  Result := True;
end;

function TDX11Device.CreateWARPDevice: Boolean;
var
  Device: ID3D11Device;
  Context: ID3D11DeviceContext;
  FeatureLevel: D3D_FEATURE_LEVEL;
begin
  PushClearFPUState;
  try
    if Failed(D3D11CreateDevice(nil, D3D_DRIVER_TYPE_WARP, 0, DefaultDeviceCreationFlags, @BestFeatureLevel, 1,
      D3D11_SDK_VERSION, @Device, @FeatureLevel, @Context)) then
      if Failed(D3D11CreateDevice(nil, D3D_DRIVER_TYPE_WARP, 0, DefaultDeviceCreationFlags, @SearchFeatureLevels[0],
        High(SearchFeatureLevels) + 1, D3D11_SDK_VERSION, @Device, @FeatureLevel, @Context)) then
        Exit(False);
  finally
    PopFPUState;
  end;

  FContextWriter.DriverType := D3D_DRIVER_TYPE_WARP;
  FContextWriter.FeatureLevel := FeatureLevel;
  FContextWriter.Device := Device;
  FContextWriter.DeviceContext := Context;

  UpdateTechFeatureVersion;
  Result := True;
end;

function TDX11Device.CreateDeviceContext: Boolean;
begin
  if not FPreferSoftwareMode then
  begin
    if not CreateHALDevice then
      if not CreateWARPDevice then
        Exit(False);
  end
  else if not CreateWARPDevice then
    Exit(False);

  Result := True;
end;

procedure TDX11Device.ReleaseDeviceContext;
begin
  FContextWriter.DeviceContext := nil;
  FContextWriter.Device := nil;
  FContextWriter.DriverType := D3D_DRIVER_TYPE_UNKNOWN;
end;

function TDX11Device.UpdateWindowAssociation: Boolean;
var
  SwapChainInfo: PSwapChainInfo;
  I: Integer;
begin
  if (FContext = nil) or (FContext.Factory = nil) then
    Exit(False);

  for I := 0 to SwapChains.Count - 1 do
  begin
    SwapChainInfo := SwapChains[I];
    if (SwapChainInfo = nil) or (SwapChainInfo.WindowHandle = 0) then
      Continue;

    PushClearFPUState;
    try
      if Failed(FContext.Factory.MakeWindowAssociation(SwapChainInfo.WindowHandle, DXGI_MWA_NO_WINDOW_CHANGES or
        DXGI_MWA_NO_ALT_ENTER)) then
        Exit(False);
    finally
      PopFPUState;
    end;
  end;

  Result := True;
end;

function TDX11Device.InitDevice: Boolean;
begin
  if (not LinkDXGI) or (not LinkD3D11) then
    Exit(False);

  if not CreateDeviceContext then
    Exit(False);

  if not ExtractFactory then
  begin
    ReleaseDeviceContext;
    Exit(False);
  end;

  if not FDXSwapChains.Recreate(SwapChains) then
  begin
    ReleaseFactory;
    ReleaseDeviceContext;
    Exit(False);
  end;

  if not UpdateWindowAssociation then
  begin
    FDXSwapChains.Clear;
    ReleaseFactory;
    ReleaseDeviceContext;
    Exit(False);
  end;

  Result := True;
end;

procedure TDX11Device.DoneDevice;
begin
  FDXSwapChains.Clear;
  ReleaseFactory;
  ReleaseDeviceContext;
end;

procedure TDX11Device.Reset;
begin
  if FContext.Context = nil then
    Exit;

  PushClearFPUState;
  try
    FContext.Context.ClearState;
  finally
    PopFPUState;
  end;
end;

function TDX11Device.ResizeSwapChain(const SwapChainIndex: Integer; const NewSwapChainInfo: PSwapChainInfo): Boolean;
var
  SwapChain: TDX11SwapChain;
begin
  SwapChain := FDXSwapChains[SwapChainIndex];
  if SwapChain <> nil then
    Result := SwapChain.Resize(NewSwapChainInfo)
  else
    Exit(False);
end;

function TDX11Device.MayRender(const SwapChainIndex: Integer): Boolean;
var
  SwapChain: TDX11SwapChain;
  OpRes: HResult;
begin
  if SwapChainIndex = -1 then
    Exit(True);

  SwapChain := FDXSwapChains[SwapChainIndex];
  if SwapChain = nil then
    Exit(False);

  if SwapChain.IdleState then
  begin
    Result := False;
    if not SwapChain.Initialized then
      Exit;

    OpRes := SwapChain.PresentTest;
    if OpRes = S_OK then
    begin
      SwapChain.IdleState := False;
      Result := True;
    end;
  end
  else
    Result := True;
end;

function TDX11Device.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
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

function TDX11Device.BeginScene(const SwapChainIndex: Integer): Boolean;
var
  SwapChain: TDX11SwapChain;
begin
  SwapChain := FDXSwapChains[SwapChainIndex];

  Result := (SwapChain <> nil) and SwapChain.SetRenderTargets and SwapChain.SetDefaultViewport;
  if Result then
    FCurrentSwapChain := SwapChainIndex;
end;

function TDX11Device.EndScene: Boolean;
var
  SwapChain: TDX11SwapChain;
  OpRes: HResult;
  SwapChainInfo: PSwapChainInfo;
begin
  if FCurrentSwapChain = -1 then
    Exit(False);
  try
    SwapChain := FDXSwapChains[FCurrentSwapChain];
    if SwapChain = nil then
      Exit(False);

    OpRes := SwapChain.Present;
    SwapChain.ResetRenderTargets;

    case OpRes of
      DXGI_STATUS_OCCLUDED,
      DXGI_STATUS_MODE_CHANGE_IN_PROGRESS:
        begin
          SwapChain.IdleState := True;
          Result := True;
        end;

      DXGI_STATUS_MODE_CHANGED:
        begin
          SwapChainInfo := SwapChains[FCurrentSwapChain];
          if SwapChainInfo <> nil then
            Result := SwapChain.Resize(SwapChainInfo, True)
          else
            Result := False;
        end;

      DXGI_ERROR_DEVICE_HUNG,
      DXGI_ERROR_DRIVER_INTERNAL_ERROR,
      DXGI_ERROR_DEVICE_REMOVED,
      DXGI_ERROR_DEVICE_RESET:
        begin
          OnRelease.Notify(Self);
          DoneDevice;

          if InitDevice then
          begin
            OnRestore.Notify(Self);
            Result := True;
          end
          else
          begin
            Finalize;
            Result := False;
          end;
        end;
    else
      Result := Succeeded(OpRes);
    end;
  finally
    FCurrentSwapChain := -1;
  end;
end;

{$ENDREGION}

end.
