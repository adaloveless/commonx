unit PXL.Devices.FM.DX9;
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
  PXL.TypeDef, PXL.Types, PXL.Devices, PXL.Types.DX9;

type
  TFireDX9Device = class(TCustomStateDevice)
  private
    FContext: TDX9DeviceContext;
    FContextWriter: TDX9DeviceContextWriter;

    function GetDisplayMode: Boolean;
  protected
    function GetDeviceContext: TCustomDeviceContext; override;

    function InitDevice: Boolean; override;
    procedure DoneDevice; override;
  public
    constructor Create(const AProvider: TCustomDeviceProvider);
    destructor Destroy; override;

    function Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single = 1.0;
      const StencilValue: Cardinal = 0): Boolean; override;
  end;

implementation

uses
  Windows, SysUtils, Jedi.Direct3D9, FMX.Context.DX9;

constructor TFireDX9Device.Create(const AProvider: TCustomDeviceProvider);
begin
  inherited;

  FContext := TDX9DeviceContext.Create(Self, FContextWriter);

  FTechnology := TDeviceTechnology.Direct3D;
  FTechVersion := $B00;
end;

destructor TFireDX9Device.Destroy;
begin
  inherited;
  
  FContextWriter.Free;
  FContext.Free;
end;

function TFireDX9Device.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FContext;
end;

function TFireDX9Device.GetDisplayMode: Boolean;
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

function TFireDX9Device.InitDevice: Boolean;
var
  Caps: D3DCaps9;
begin
  if not LoadDirect3D9 then
    Exit(False);

  FContextWriter.Direct3D := Jedi.Direct3D9.IDirect3D9(TCustomDX9Context.Direct3D9Obj);
  FContextWriter.Direct3DDevice := Jedi.Direct3D9.IDirect3DDevice9(TCustomDX9Context.SharedDevice);

  if FContext.Direct3DDevice = nil then
  begin
    FContextWriter.Direct3D := nil;
    Exit(False);
  end;

  if FContext.Direct3D <> nil then
  begin
    FContextWriter.Support := TD3D9Support.Legacy;
    FTechFeatureVersion := $900;

    if Supports(FContext.Direct3D, IDirect3D9Ex) then
    begin
      FContextWriter.Support := TD3D9Support.Vista;
      FTechFeatureVersion := $901;
    end;
  end;

  if not GetDisplayMode then
  begin
    FContextWriter.Direct3DDevice := nil;
    FContextWriter.Direct3D := nil;
    Exit(False);
  end;

  if Failed(FContext.Direct3DDevice.GetDeviceCaps(Caps)) then
  begin
    FContextWriter.Direct3DDevice := nil;
    FContextWriter.Direct3D := nil;
    FContextWriter.ClearDisplayMode;
    Exit(False);
  end;

  FContextWriter.Caps := Caps;
  FContextWriter.ClearPresentParams;
  Result := True;
end;

procedure TFireDX9Device.DoneDevice;
begin
  FContextWriter.ClearCaps;
  FContextWriter.ClearDisplayMode;
  FContextWriter.Direct3DDevice := nil;
  FContextWriter.Direct3D := nil;
end;

function TFireDX9Device.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
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

end.
