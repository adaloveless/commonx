unit PXL.Devices.DX7;
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
  PXL.Windows.DDraw, PXL.Types, PXL.Devices, PXL.SwapChains, PXL.Types.DX7, PXL.SwapChains.DX7;

// Remove the dot to enable multi-threading mode.
{.$DEFINE DX7_MULTITHREADED}

type
  TDX7Device = class(TCustomSwapChainDevice)
  private
    FContext: TDX7DeviceContext;
    FContextWriter: TDX7DeviceContextWriter;
    FDXSwapChains: TDX7SwapChains;

    FPrimarySurface: IDirectDrawSurface7;
    FLostState: Boolean;
    FCurrentSwapChain: Integer;

    function CreateDirectDraw: Boolean;
    procedure DestroyDirectDraw;

    function CreateDirect3D: Boolean;
    procedure DestroyDirect3D;

    function SetCooperativeLevel: Boolean;

    function CreatePrimarySurface: Boolean;
    procedure DestroyPrimarySurface;

    function CreateDevice: Boolean;
    procedure DestroyDevice;

    procedure MoveIntoLostState;
    function MoveIntoRecoveredState: Boolean;
  protected
    function GetDeviceContext: TCustomDeviceContext; override;

    function InitDevice: Boolean; override;
    procedure DoneDevice; override;

    function ResizeSwapChain(const SwapChainIndex: Integer; const NewSwapChainInfo: PSwapChainInfo): Boolean; override;
    function MayRender(const SwapChainIndex: Integer): Boolean; override;
  public
    constructor Create(const AProvider: TCustomDeviceProvider);
    destructor Destroy; override;

    function Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single = 1.0;
      const StencilValue: Cardinal = 0): Boolean; override;

    function BeginScene(const SwapChainIndex: Integer = 0): Boolean; override;
    function EndScene: Boolean; override;

    property DXSwapChains: TDX7SwapChains read FDXSwapChains;
    property PrimarySurface: IDirectDrawSurface7 read FPrimarySurface;
  end;

implementation

uses
  Windows, SysUtils, PXL.Windows.D3D7;

constructor TDX7Device.Create(const AProvider: TCustomDeviceProvider);
begin
  inherited;

  FContext := TDX7DeviceContext.Create(Self, FContextWriter);
  FDXSwapChains := TDX7SwapChains.Create(FContext);

  FTechnology := TDeviceTechnology.Direct3D;
  FTechVersion := $700;

  FCurrentSwapChain := -1;
end;

destructor TDX7Device.Destroy;
begin
  inherited;

  FDXSwapChains.Free;
  FContextWriter.Free;
  FContext.Free;
end;

function TDX7Device.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FContext;
end;

function TDX7Device.CreateDirectDraw: Boolean;
var
  LocalObject: IDirectDraw7;
begin
  if Failed(DirectDrawCreateEx(nil, LocalObject, IID_IDirectDraw7, nil)) then
    Exit(False);

  FContextWriter.DD7Object := LocalObject;
  Result := True;
end;

procedure TDX7Device.DestroyDirectDraw;
begin
  FContextWriter.DD7Object := nil;
end;

function TDX7Device.CreateDirect3D: Boolean;
var
  LocalObject: IDirect3D7;
begin
  if not Supports(FContext.DD7Object, IID_IDirect3D7, LocalObject) then
    Exit(False);

  FContextWriter.D3D7Object := LocalObject;
  Result := True;
end;

procedure TDX7Device.DestroyDirect3D;
begin
  FContextWriter.D3D7Object := nil;
end;

function TDX7Device.SetCooperativeLevel: Boolean;
var
  Flags: Cardinal;
begin
  if FContext.DD7Object = nil then
    Exit(False);

  Flags := DDSCL_NORMAL or DDSCL_NOWINDOWCHANGES or DDSCL_FPUPRESERVE;

{$IFDEF DX7_MULTITHREADED}
  Flags := Flags or DDSCL_MULTITHREADED;
{$ENDIF}

  Result := Succeeded(FContext.DD7Object.SetCooperativeLevel(0, Flags));
end;

function TDX7Device.CreatePrimarySurface: Boolean;
var
  Desc: DDSURFACEDESC2;
begin
  if FContext.DD7Object = nil then
    Exit(False);

  FillChar(Desc, SizeOf(DDSURFACEDESC2), 0);

  Desc.dwSize := SizeOf(DDSURFACEDESC2);
  Desc.dwFlags := DDSD_CAPS;
  Desc.ddsCaps.dwCaps := DDSCAPS_PRIMARYSURFACE;

  Result := Succeeded(FContext.DD7Object.CreateSurface(Desc, FPrimarySurface, nil));
end;

procedure TDX7Device.DestroyPrimarySurface;
begin
  FPrimarySurface := nil;
end;

function TDX7Device.CreateDevice: Boolean;
var
  SwapChain: TDX7SwapChain;
  LocalObject: IDirect3DDevice7;
begin
  if FContext.D3D7Object = nil then
    Exit(False);

  SwapChain := FDXSwapChains[0];
  if SwapChain = nil then
    Exit(False);

  if Failed(FContext.D3D7Object.CreateDevice(IID_IDirect3DTnLHalDevice, SwapChain.Surface, LocalObject)) then
  begin // Hardware T&L not available, try software T&L.
    if Failed(FContext.D3D7Object.CreateDevice(IID_IDirect3DHALDevice, SwapChain.Surface, LocalObject)) then
      Exit(False);
  end;

  FContextWriter.D3D7Device := LocalObject;
  Result := True;
end;

procedure TDX7Device.DestroyDevice;
begin
  FContextWriter.D3D7Device := nil;
end;

function TDX7Device.InitDevice: Boolean;
var
  SwapChainInfo: PSwapChainInfo;
begin
  if not LinkDDraw then
    Exit(False);

  SwapChainInfo := SwapChains[0];
  if SwapChainInfo = nil then
    Exit(False);

  if not CreateDirectDraw then
    Exit(False);

  if not CreateDirect3D then
  begin
    DestroyDirectDraw;
    Exit(False);
  end;

  if not SetCooperativeLevel then
  begin
    DestroyDirect3D;
    DestroyDirectDraw;
    Exit(False);
  end;

  if not CreatePrimarySurface then
  begin
    DestroyDirect3D;
    DestroyDirectDraw;
    Exit(False);
  end;

  if not FDXSwapChains.Recreate(SwapChains) then
  begin
    DestroyPrimarySurface;
    DestroyDirect3D;
    DestroyDirectDraw;
    Exit(False);
  end;

  if not CreateDevice then
  begin
    FDXSwapChains.Clear;
    DestroyPrimarySurface;
    DestroyDirect3D;
    DestroyDirectDraw;
    Exit(False);
  end;

  FLostState := False;
  Result := True;
end;

procedure TDX7Device.DoneDevice;
begin
  FDXSwapChains.Clear;
  DestroyDevice;
  DestroyPrimarySurface;
  DestroyDirect3D;
  DestroyDirectDraw;
end;

procedure TDX7Device.MoveIntoLostState;
begin
  if not FLostState then
  begin
    OnRelease.Notify(Self);
    FDXSwapChains.Clear;
    FLostState := True;
  end;
end;

function TDX7Device.MoveIntoRecoveredState: Boolean;
begin
  if Failed(FPrimarySurface.Restore) then
    Exit(False);

  if not FDXSwapChains.Recreate(SwapChains) then
    Exit(False);

  FLostState := False;
  OnRestore.Notify(Self);

  Result := True;
end;

function TDX7Device.ResizeSwapChain(const SwapChainIndex: Integer; const NewSwapChainInfo: PSwapChainInfo): Boolean;
var
  SwapChainInfo: PSwapChainInfo;
  SwapChain: TDX7SwapChain;
begin
  SwapChainInfo := SwapChains[SwapChainIndex];
  if SwapChainInfo = nil then
    Exit(False);

  SwapChain := FDXSwapChains[SwapChainIndex];
  if SwapChain = nil then
    Exit(False);

  SwapChainInfo.Width := NewSwapChainInfo.Width;
  SwapChainInfo.Height := NewSwapChainInfo.Height;

  SwapChain.Finalize;
  Result := SwapChain.Initialize(SwapChainInfo);
end;

function TDX7Device.MayRender(const SwapChainIndex: Integer): Boolean;
var
  Res: HResult;
  IsLostState, IsRecoverable: Boolean;
begin
  if FContext.DD7Object = nil then
    Exit(False);

  Res := FContext.DD7Object.TestCooperativeLevel;

  IsLostState := Failed(Res);
  IsRecoverable := (FLostState and (not IsLostState)) or (Res = DDERR_WRONGMODE);

  // Transition from normal to lost state.
  if IsLostState and (not FLostState) then
  begin
    MoveIntoLostState;
    Exit(False);
  end;

  // Transition from lost to recovered state.
  if FLostState and (not IsLostState) then
    Exit(MoveIntoRecoveredState);

  // Lost state but may be potentially recoverable (maybe later on?)
  if IsLostState and IsRecoverable then
  begin
    MoveIntoLostState;
    MoveIntoRecoveredState;
    Exit(False);
  end;

  Result := not IsLostState;
end;

function TDX7Device.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
  const StencilValue: Cardinal): Boolean;
begin
  if (FContext.D3D7Device = nil) or (ClearTypes = []) or (TClearType.Depth in ClearTypes) or
    (TClearType.Stencil in ClearTypes) then
    Exit(False);

  Result := Succeeded(FContext.D3D7Device.Clear(0, nil, D3DCLEAR_TARGET, ColorValue, DepthValue, StencilValue));
end;

function TDX7Device.BeginScene(const SwapChainIndex: Integer): Boolean;
var
  SwapChain: TDX7SwapChain;
begin
  if FContext.D3D7Device = nil then
    Exit(False);

  SwapChain := FDXSwapChains[SwapChainIndex];
  if SwapChain = nil then
    Exit(False);

  if not SwapChain.SetRenderBuffers then
    Exit(False);

  if not SwapChain.SetDefaultViewport then
  begin
    SwapChain.RestoreRenderBuffers;
    Exit(False);
  end;

  if Failed(FContext.D3D7Device.BeginScene) then
  begin
    SwapChain.RestoreRenderBuffers;
    Exit(False);
  end;

  FCurrentSwapChain := SwapChainIndex;
  Result := True;
end;

function TDX7Device.EndScene: Boolean;
var
  SwapChain: TDX7SwapChain;
begin
  if (FContext.D3D7Device = nil) or (FCurrentSwapChain = -1) then
    Exit(False);

  if Failed(FContext.D3D7Device.EndScene) then
    Exit(False);

  SwapChain := FDXSwapChains[FCurrentSwapChain];
  if SwapChain = nil then
    Exit(False);

  if not SwapChain.RestoreRenderBuffers then
    Exit(False);

  Result := SwapChain.Present(FPrimarySurface);
end;

end.
