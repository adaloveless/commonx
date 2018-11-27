unit PXL.Devices.DX9;
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
  Jedi.Direct3D9, PXL.Types, PXL.Devices, PXL.SwapChains, PXL.Types.DX9, PXL.SwapChains.DX9;

// Remove the dot to enable multi-threading mode.
{.$DEFINE DX9_MULTITHREADED}

type
  TDX9Device = class(TCustomSwapChainDevice)
  private
    FContext: TDX9DeviceContext;
    FContextWriter: TDX9DeviceContextWriter;
    FDXSwapChains: TDX9SwapChains;

    FDepthBufferEnabled: Boolean;
    FStencilBufferEnabled: Boolean;
    FLostState: Boolean;
    FCurrentSwapChain: Integer;
    FEnableVistaSupport: Boolean;

    function CreateDirect3D: Boolean;
    procedure DestroyDirect3D;

    function GetDisplayMode: Boolean;
    function UpdatePresentParams: Boolean;
    function CreateDevice: Boolean;
    procedure DestroyDevice;

    procedure MoveIntoLostState;
    function AttemptRecoverState: Boolean;
    function CheckDeviceCondition(const SwapChainIndex: Integer): Boolean;

    function SetDefaultViewport: Boolean;
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

    function BeginScene(const SwapChainIndex: Integer): Boolean; override;
    function EndScene: Boolean; override;

    function ResetDevice: Boolean;

    property DXSwapChains: TDX9SwapChains read FDXSwapChains;
    property EnableVistaSupport: Boolean read FEnableVistaSupport write FEnableVistaSupport;
  end;

implementation

uses
  Windows, SysUtils;

constructor TDX9Device.Create(const AProvider: TCustomDeviceProvider);
begin
  inherited;

  FContext := TDX9DeviceContext.Create(Self, FContextWriter);
  FDXSwapChains := TDX9SwapChains.Create(FContext);

  FTechnology := TDeviceTechnology.Direct3D;
  FTechVersion := $900;

  FEnableVistaSupport := True;
  FCurrentSwapChain := -1;
end;

destructor TDX9Device.Destroy;
begin
  inherited;

  FDXSwapChains.Free;
  FContextWriter.Free;
  FContext.Free;
end;

function TDX9Device.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FContext;
end;

function TDX9Device.CreateDirect3D: Boolean;
var
  Direct3D: IDirect3D9;
  Direct3DEx: IDirect3D9Ex;
begin
  if not LoadDirect3D9 then
    Exit(False);

  FContextWriter.Direct3D := nil;

  if FEnableVistaSupport and Assigned(Direct3DCreate9Ex) then
  begin
    if Succeeded(Direct3DCreate9Ex(D3D_SDK_VERSION, Direct3DEx)) then
    begin
      FContextWriter.Direct3D := Direct3DEx;
      FContextWriter.Support := TD3D9Support.Vista;
      FTechFeatureVersion := $901;
    end;
  end;

  if FContext.Direct3D = nil then
  begin
    Direct3D := Direct3DCreate9(D3D_SDK_VERSION);
    if Direct3D <> nil then
    begin
      FContextWriter.Direct3D := Direct3D;
      FContextWriter.Support := TD3D9Support.Legacy;
      FTechFeatureVersion := $900;
    end;
  end;

  Result := FContext.Direct3D <> nil;
end;

procedure TDX9Device.DestroyDirect3D;
begin
  FContextWriter.Direct3D := nil;
  FContextWriter.Support := TD3D9Support.Undefined;
  FTechFeatureVersion := 0;
end;

function TDX9Device.GetDisplayMode: Boolean;
var
  DisplayModeEx: D3DDISPLAYMODEEX;
  DisplayMode: D3DDISPLAYMODE;
begin
  if FContext.Direct3D = nil then
    Exit(False);

  FillChar(DisplayModeEx, SizeOf(D3DDISPLAYMODEEX), 0);
  DisplayModeEx.Size := SizeOf(D3DDISPLAYMODEEX);

  if FContext.Support = TD3D9Support.Vista then
  begin // Vista enhanced mode.
    if Failed(IDirect3D9Ex(FContext.Direct3D).GetAdapterDisplayModeEx(D3DADAPTER_DEFAULT, @DisplayModeEx, nil)) then
      Exit(False);
  end
  else
  begin // XP compatibility mode.
    if Failed(FContext.Direct3D.GetAdapterDisplayMode(D3DADAPTER_DEFAULT, DisplayMode)) then
      Exit(False);

    DisplayModeEx.Width := DisplayMode.Width;
    DisplayModeEx.Height := DisplayMode.Height;
    DisplayModeEx.RefreshRate := DisplayMode.RefreshRate;
    DisplayModeEx.Format := DisplayMode.Format;
  end;

  FContextWriter.DisplayMode := DisplayModeEx;
  Result := True;
end;

function TDX9Device.UpdatePresentParams: Boolean;
var
  SwapChain: PSwapChainInfo;
  PresentParams: D3DPRESENT_PARAMETERS;
begin
  SwapChain := SwapChains[0];
  if SwapChain = nil then
    Exit(False);

  FillChar(PresentParams, SizeOf(D3DPRESENT_PARAMETERS), 0);

  with PresentParams do
  begin
    BackBufferWidth := SwapChain.Width;
    BackBufferHeight := SwapChain.Height;

    Windowed := True;
    SwapEffect := D3DSWAPEFFECT_DISCARD;

    hDeviceWindow := SwapChain.WindowHandle;

    PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;

    if SwapChain.VSync then
      PresentationInterval := D3DPRESENT_INTERVAL_ONE;

    BackBufferFormat := FContext.FindBackBufferFormat(SwapChain.Format);

    if SwapChain.DepthStencil <> TDepthStencil.None then
    begin
      EnableAutoDepthStencil := True;
      Flags := D3DPRESENTFLAG_DISCARD_DEPTHSTENCIL;

      AutoDepthStencilFormat := FContext.FindDepthStencilFormat(SwapChain.DepthStencil);
    end;

    FContext.FindBestMultisampleType(BackBufferFormat, AutoDepthStencilFormat, SwapChain.Multisamples, MultiSampleType,
      MultiSampleQuality);
  end;

  FDepthBufferEnabled := SwapChain.DepthStencil > TDepthStencil.None;
  FStencilBufferEnabled := SwapChain.DepthStencil > TDepthStencil.DepthOnly;

  FContextWriter.PresentParams := PresentParams;
  Result := True;
end;

function TDX9Device.CreateDevice: Boolean;
var
  Flags: Cardinal;
  SwapChain: PSwapChainInfo;
  PresentParams: D3DPRESENT_PARAMETERS;
  Caps: D3DCaps9;
  Direct3DDeviceEx: IDirect3DDevice9Ex;
  Direct3DDevice: IDirect3DDevice9;
begin
  if FContext.Direct3D = nil then
    Exit(False);

  SwapChain := SwapChains[0];
  if SwapChain = nil then
    Exit(False);

  PresentParams := FContext.PresentParams;

  Flags := D3DCREATE_NOWINDOWCHANGES or D3DCREATE_FPU_PRESERVE;

{$IFDEF DX9_MULTITHREADED}
  Flags := Flags or D3DCREATE_MULTITHREADED;
{$ENDIF}

  if FContext.Support = TD3D9Support.Vista then
  begin // Vista enhanced mode.
    if Failed(IDirect3D9Ex(FContext.Direct3D).CreateDeviceEx(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL,
      SwapChain.WindowHandle, Flags or D3DCREATE_HARDWARE_VERTEXPROCESSING, @PresentParams, nil,
      Direct3DDeviceEx)) then
    begin // Try software vertex processing.
      if Failed(IDirect3D9Ex(FContext.Direct3D).CreateDeviceEx(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL,
        SwapChain.WindowHandle, Flags or D3DCREATE_SOFTWARE_VERTEXPROCESSING, @PresentParams, nil,
        Direct3DDeviceEx)) then
        Exit(False);
    end;

    Direct3DDevice := Direct3DDeviceEx;
  end
  else
  begin // XP compatibility mode.
    if Failed(FContext.Direct3D.CreateDevice(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, SwapChain.WindowHandle,
      Flags or D3DCREATE_HARDWARE_VERTEXPROCESSING, @PresentParams, Direct3DDevice)) then
    begin // Try software vertex processing.
      if Failed(FContext.Direct3D.CreateDevice(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, SwapChain.WindowHandle,
        Flags or D3DCREATE_SOFTWARE_VERTEXPROCESSING, @PresentParams, Direct3DDevice)) then
        Exit(False);
    end;
  end;

  if Failed(Direct3DDevice.GetDeviceCaps(Caps)) then
    Exit(False);

  FContextWriter.PresentParams := PresentParams;
  FContextWriter.Direct3DDevice := Direct3DDevice;
  FContextWriter.Caps := Caps;

  SwapChain.Format := TDX9DeviceContext.FormatToNative(PresentParams.BackBufferFormat);
  SwapChain.Multisamples := Ord(PresentParams.MultiSampleType);

  Result := True;
end;

procedure TDX9Device.DestroyDevice;
begin
  FContextWriter.ClearCaps;
  FContextWriter.ClearPresentParams;
  FContextWriter.Direct3DDevice := nil;
end;

function TDX9Device.InitDevice: Boolean;
begin
  if not CreateDirect3D then
    Exit(False);

  if not GetDisplayMode then
  begin
    DestroyDirect3D;
    Exit(False);
  end;

  if not UpdatePresentParams then
  begin
    FContextWriter.ClearPresentParams;
    FContextWriter.ClearDisplayMode;
    DestroyDirect3D;
    Exit(False);
  end;

  if not CreateDevice then
  begin
    FContextWriter.ClearPresentParams;
    FContextWriter.ClearDisplayMode;
    DestroyDirect3D;
    Exit(False);
  end;

  if not FDXSwapChains.Recreate(SwapChains) then
  begin
    DestroyDevice;
    FContextWriter.ClearDisplayMode;
    DestroyDirect3D;
    Exit(False);
  end;

  FLostState := False;
  Result := True;
end;

procedure TDX9Device.DoneDevice;
begin
  FDXSwapChains.Clear;
  DestroyDevice;
  FContextWriter.ClearDisplayMode;
  DestroyDirect3D;
end;

procedure TDX9Device.MoveIntoLostState;
begin
  if not FLostState then
  begin
    OnRelease.Notify(Self);
    FDXSwapChains.Clear;
    FLostState := True;
  end;
end;

function TDX9Device.AttemptRecoverState: Boolean;
var
  PresentParams: D3DPRESENT_PARAMETERS;
begin
  if FContext.Direct3DDevice = nil then
    Exit(False);

  if FLostState then
  begin
    PresentParams := FContext.PresentParams;

    if FContext.Support = TD3D9Support.Vista then
    begin // Vista enhanced mode.
      if Failed(IDirect3DDevice9Ex(FContext.Direct3DDevice).ResetEx(PresentParams, nil)) then
        Exit(False);
    end
    else
    begin // XP compatibility mode.
      if Failed(FContext.Direct3DDevice.Reset(PresentParams)) then
        Exit(False);
    end;

    FContextWriter.PresentParams := PresentParams;

    if not FDXSwapChains.Recreate(SwapChains) then
      Exit(False);

    FLostState := False;
    OnRestore.Notify(Self);
  end;

  Result := True;
end;

function TDX9Device.CheckDeviceCondition(const SwapChainIndex: Integer): Boolean;
var
  SwapChainInfo: PSwapChainInfo;
  Res: HResult;
begin
  if FContext.Direct3DDevice = nil then
    Exit(False);

  if FContext.Support = TD3D9Support.Vista then
  begin // Vista enhanced mode.
    if SwapChainIndex = -1 then
      Exit(True);

    SwapChainInfo := SwapChains[SwapChainIndex];
    if SwapChainInfo = nil then
      Exit(False);

    if Failed(IDirect3DDevice9Ex(FContext.Direct3DDevice).CheckDeviceState(SwapChainInfo.WindowHandle)) then
    begin
      MoveIntoLostState;
      Result := AttemptRecoverState;
    end
    else
      Result := True;
  end
  else
  begin // XP compatibility mode.
    Res := FContext.Direct3DDevice.TestCooperativeLevel;

    case Res of
      D3DERR_DEVICELOST:
      begin
        MoveIntoLostState;
        Result := False;
      end;

      D3DERR_DEVICENOTRESET:
      begin
        if not FLostState then
          MoveIntoLostState;

        Result := AttemptRecoverState;
      end;

      D3DERR_DRIVERINTERNALERROR:
      begin
        MoveIntoLostState;
        Result := AttemptRecoverState;
      end;

      D3D_OK:
        Result := True;

      else
        Result := False;
    end;
  end;
end;

function TDX9Device.ResizeSwapChain(const SwapChainIndex: Integer; const NewSwapChainInfo: PSwapChainInfo): Boolean;
var
  SwapChainInfo: PSwapChainInfo;
  SwapChain: TDX9SwapChain;
  PresentParams: D3DPRESENT_PARAMETERS;
begin
  if FContext.Direct3DDevice = nil then
    Exit(False);

  SwapChainInfo := SwapChains[SwapChainIndex];
  if SwapChainInfo = nil then
    Exit(False);

  SwapChain := nil;

  if SwapChainIndex > 0 then
  begin
    SwapChain := FDXSwapChains[SwapChainIndex - 1];
    if SwapChain = nil then
      Exit(False);
  end;

  SwapChainInfo.Width := NewSwapChainInfo.Width;
  SwapChainInfo.Height := NewSwapChainInfo.Height;

  if SwapChain = nil then
  begin
    PresentParams := FContext.PresentParams;

    if FContext.Support = TD3D9Support.Vista then
    begin // Vista enhanced mode.
      PresentParams.BackBufferWidth := SwapChainInfo.Width;
      PresentParams.BackBufferHeight := SwapChainInfo.Height;

      if Failed(IDirect3DDevice9Ex(FContext.Direct3DDevice).ResetEx(PresentParams, nil)) then
        Exit(False);

      FContextWriter.PresentParams := PresentParams;
      Result := True;
    end
    else
    begin // XP compatibility mode.
      MoveIntoLostState;

      PresentParams.BackBufferWidth := SwapChainInfo.Width;
      PresentParams.BackBufferHeight := SwapChainInfo.Height;

      FContextWriter.PresentParams := PresentParams;

      Result := AttemptRecoverState;
    end;
  end
  else
  begin
    SwapChain.Finalize;
    Result := SwapChain.Initialize(SwapChainInfo);
  end;
end;

function TDX9Device.MayRender(const SwapChainIndex: Integer): Boolean;
begin
  Result := CheckDeviceCondition(SwapChainIndex);
end;

function TDX9Device.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
  const StencilValue: Cardinal): Boolean;
var
  ClearFlags: Cardinal;
begin
  if (FContext.Direct3DDevice = nil) or (ClearTypes = []) then
    Exit(False);

  ClearFlags := 0;

  if TClearType.Color in ClearTypes then
    ClearFlags := D3DCLEAR_TARGET;

  if TClearType.Depth in ClearTypes then
    ClearFlags := ClearFlags or D3DCLEAR_ZBUFFER;

  if TClearType.Stencil in ClearTypes then
    ClearFlags := ClearFlags or D3DCLEAR_STENCIL;

  Result := Succeeded(FContext.Direct3DDevice.Clear(0, nil, ClearFlags, ColorValue, DepthValue, StencilValue));
end;

function TDX9Device.SetDefaultViewport: Boolean;
var
  SwapChainInfo: PSwapChainInfo;
  Viewport: TD3DViewport9;
begin
  if FContext.Direct3DDevice = nil then
    Exit(False);

  SwapChainInfo := SwapChains[0];
  if SwapChainInfo = nil then
    Exit(False);

  Viewport.X := 0;
  Viewport.Y := 0;
  Viewport.Width := SwapChainInfo.Width;
  Viewport.Height := SwapChainInfo.Height;
  Viewport.MinZ := 0.0;
  Viewport.MaxZ := 1.0;

  Result := Succeeded(FContext.Direct3DDevice.SetViewport(Viewport));
end;

function TDX9Device.BeginScene(const SwapChainIndex: Integer): Boolean;
var
  SwapChainInfo: PSwapChainInfo;
  SwapChain: TDX9SwapChain;
begin
  if FContext.Direct3DDevice = nil then
    Exit(False);

  SwapChainInfo := SwapChains[SwapChainIndex];
  if SwapChainInfo = nil then
    Exit(False);

  SwapChain := nil;

  if SwapChainIndex > 0 then
  begin
    SwapChain := FDXSwapChains[SwapChainIndex - 1];
    if SwapChain = nil then
      Exit(False);
  end;

  if SwapChain <> nil then
  begin
    if (not SwapChain.SetRenderBuffers) or (not SwapChain.SetDefaultViewport) then
      Exit(False);
  end
  else
  begin
    if not SetDefaultViewport then
      Exit(False);
  end;

  Result := Succeeded(FContext.Direct3DDevice.BeginScene);
  if Result then
    FCurrentSwapChain := SwapChainIndex;
end;

function TDX9Device.EndScene: Boolean;
var
  SwapChain: TDX9SwapChain;
begin
  if (FContext.Direct3DDevice = nil) or (FCurrentSwapChain = -1) then
    Exit(False);
  try
    SwapChain := nil;

    if FCurrentSwapChain > 0 then
    begin
      SwapChain := FDXSwapChains[FCurrentSwapChain - 1];
      if SwapChain = nil then
        Exit(False);
    end;

    if Failed(FContext.Direct3DDevice.EndScene) then
      Exit(False);

    if SwapChain <> nil then
    begin
      if not SwapChain.RestoreRenderBuffers then
        Exit(False);

      Result := SwapChain.Present;
    end
    else
    begin
      if FContext.Support = TD3D9Support.Vista then
        Result := Succeeded(IDirect3DDevice9Ex(FContext.Direct3DDevice).PresentEx(nil, nil, 0, nil, 0))
      else
        Result := Succeeded(FContext.Direct3DDevice.Present(nil, nil, 0, nil));
    end;
  finally
    FCurrentSwapChain := -1;
  end;
end;

function TDX9Device.ResetDevice: Boolean;
begin
  MoveIntoLostState;
  Result := AttemptRecoverState;
end;

end.
