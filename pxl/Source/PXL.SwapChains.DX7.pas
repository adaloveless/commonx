unit PXL.SwapChains.DX7;
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
  PXL.Windows.DDraw, PXL.TypeDef, PXL.SwapChains, PXL.Types.DX7;

type
  TDX7SwapChain = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX7DeviceContext;
    FInitialized: Boolean;

    FSwapChainInfo: TSwapChainInfo;
    FSurface: IDirectDrawSurface7;
    FSurfaceDesc: DDSURFACEDESC2;
    FClipper: IDirectDrawClipper;
    FPrevSurface: IDirectDrawSurface7;

    function CreateSurface(var SwapChainInfo: TSwapChainInfo): Boolean;
    procedure DestroySurface;

    function CreateClipper(const SwapChainInfo: TSwapChainInfo): Boolean;
    procedure DestroyClipper;
  public
    constructor Create(const AContext: TDX7DeviceContext);
    destructor Destroy; override;

    function Initialize(const SwapChainInfo: PSwapChainInfo): Boolean;
    procedure Finalize;

    function SetRenderBuffers: Boolean;
    function SetDefaultViewport: Boolean;
    function RestoreRenderBuffers: Boolean;

    function Present(const PrimarySurface: IDirectDrawSurface7): Boolean;

    property Context: TDX7DeviceContext read FContext;
    property Initialized: Boolean read FInitialized;

    property SwapChainInfo: TSwapChainInfo read FSwapChainInfo;
    property Surface: IDirectDrawSurface7 read FSurface;
    property SurfaceDesc: DDSURFACEDESC2 read FSurfaceDesc;
    property Clipper: IDirectDrawClipper read FClipper;
  end;

  TDX7SwapChains = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX7DeviceContext;
    FSwapChains: array of TDX7SwapChain;

    function GetCount: Integer;
    function GetItem(const Index: Integer): TDX7SwapChain;
  public
    constructor Create(const AContext: TDX7DeviceContext);
    destructor Destroy; override;

    function Add(const SwapChainInfo: PSwapChainInfo): Integer;
    procedure Clear;

    function Recreate(const UserChains: TSwapChains): Boolean;

    property Context: TDX7DeviceContext read FContext;

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TDX7SwapChain read GetItem; default;
  end;

implementation

uses
  Windows, SysUtils, PXL.Windows.D3D7, PXL.Types;

{$REGION 'TDX7SwapChain'}

constructor TDX7SwapChain.Create(const AContext: TDX7DeviceContext);
begin
  inherited Create;

  FContext := AContext;
  Increment_PXL_ClassInstances;
end;

destructor TDX7SwapChain.Destroy;
begin
  try
    if FInitialized then
      Finalize;

    FContext := nil;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TDX7SwapChain.CreateSurface(var SwapChainInfo: TSwapChainInfo): Boolean;
var
  PixelFormat: TPixelFormat;
begin
  if FContext.DD7Object = nil then
    Exit(False);

  // Determine if pixel format needs to be specified.
  PixelFormat := TPixelFormat.Unknown;

  if SwapChainInfo.Format <> TPixelFormat.Unknown then
    PixelFormat := FContext.FindBackBufferFormat(SwapChainInfo.Width, SwapChainInfo.Height, SwapChainInfo.Format);

  // Create back buffer surface description.
  FillChar(FSurfaceDesc, SizeOf(DDSURFACEDESC2), 0);

  FSurfaceDesc.dwSize := SizeOf(DDSURFACEDESC2);
  FSurfaceDesc.dwFlags := DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH;
  FSurfaceDesc.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_3DDEVICE;
  FSurfaceDesc.dwWidth := SwapChainInfo.Width;
  FSurfaceDesc.dwHeight := SwapChainInfo.Height;

  // If pixel format was specified above, apply it.
  if PixelFormat <> TPixelFormat.Unknown then
  begin
    FSurfaceDesc.dwFlags := FSurfaceDesc.dwFlags or DDSD_PIXELFORMAT;
    TDX7DeviceContext.NativeToFormat(PixelFormat, FSurfaceDesc.ddpfPixelFormat);
  end;

  // Create back buffer surface.
  if Failed(FContext.DD7Object.CreateSurface(FSurfaceDesc, FSurface, nil)) then
    Exit(False);

  // Retreive created back buffer surface description.
  FillChar(FSurfaceDesc, SizeOf(DDSURFACEDESC2), 0);

  FSurfaceDesc.dwSize := SizeOf(DDSURFACEDESC2);

  if Failed(FSurface.GetSurfaceDesc(FSurfaceDesc)) then
  begin
    FSurface := nil;
    Exit(False);
  end;

  SwapChainInfo.Format := TDX7DeviceContext.FormatToNative(FSurfaceDesc.ddpfPixelFormat);

  Result := True;
end;

procedure TDX7SwapChain.DestroySurface;
begin
  FSurface := nil;
end;

function TDX7SwapChain.CreateClipper(const SwapChainInfo: TSwapChainInfo): Boolean;
begin
  if FContext.DD7Object = nil then
    Exit(False);

  if Failed(FContext.DD7Object.CreateClipper(0, FClipper, nil)) then
    Exit(False);

  if Failed(FClipper.SetHWnd(0, SwapChainInfo.WindowHandle)) then
  begin
    FClipper := nil;
    Exit(False);
  end;

  Result := True;
end;

procedure TDX7SwapChain.DestroyClipper;
begin
  FClipper := nil;
end;

function TDX7SwapChain.Initialize(const SwapChainInfo: PSwapChainInfo): Boolean;
begin
  if FInitialized or (SwapChainInfo = nil) or (FContext = nil) then
    Exit(False);

  if not CreateSurface(SwapChainInfo^) then
    Exit(False);

  if not CreateClipper(SwapChainInfo^) then
  begin
    DestroySurface;
    Exit(False);
  end;

  Move(SwapChainInfo^, FSwapChainInfo, SizeOf(TSwapChainInfo));

  FInitialized := True;
  Result := True;
end;

procedure TDX7SwapChain.Finalize;
begin
  if not FInitialized then
    Exit;

  DestroyClipper;
  DestroySurface;

  FInitialized := False;
end;

function TDX7SwapChain.SetRenderBuffers: Boolean;
begin
  if (FContext.D3D7Device = nil) or (FSurface = nil) then
    Exit(False);

  if Failed(FContext.D3D7Device.GetRenderTarget(FPrevSurface)) then
    Exit(False);

  if Failed(FContext.D3D7Device.SetRenderTarget(FSurface, 0)) then
  begin
    FPrevSurface := nil;
    Exit(False);
  end;

  Result := True;
end;

function TDX7SwapChain.SetDefaultViewport: Boolean;
var
  Viewport: D3DVIEWPORT7;
begin
  if (FContext.D3D7Device = nil) or (FSurfaceDesc.dwWidth < 1) or (FSurfaceDesc.dwHeight < 1) then
    Exit(False);

  Viewport.dwX := 0;
  Viewport.dwY := 0;
  Viewport.dwWidth := FSurfaceDesc.dwWidth;
  Viewport.dwHeight := FSurfaceDesc.dwHeight;
  Viewport.dvMinZ := 0.0;
  Viewport.dvMaxZ := 1.0;

  Result := Succeeded(FContext.D3D7Device.SetViewport(Viewport));
end;

function TDX7SwapChain.RestoreRenderBuffers: Boolean;
begin
  try
    if (FContext.D3D7Device <> nil) and (FPrevSurface <> nil) then
      Result := Succeeded(FContext.D3D7Device.SetRenderTarget(FPrevSurface, 0))
    else
      Result := False;
  finally
    FPrevSurface := nil;
  end;
end;

function TDX7SwapChain.Present(const PrimarySurface: IDirectDrawSurface7): Boolean;
var
  Res: HResult;
  PrevClipper: IDirectDrawClipper;
  WindowInfo: TWindowInfo;
  WindowRect: Windows.TRect;
begin
  if (FContext.DD7Object = nil) or (FSurface = nil) or (PrimarySurface = nil) then
    Exit(False);

  Res := PrimarySurface.GetClipper(PrevClipper);
  if Res = DDERR_NOCLIPPERATTACHED then
    PrevClipper := nil
  else if Failed(Res) then
    Exit(False);

  try
    if Failed(PrimarySurface.SetClipper(FClipper)) then
      Exit(False);

    FillChar(WindowInfo, SizeOf(TWindowInfo), 0);

    WindowInfo.cbSize := SizeOf(TWindowInfo);

    if not GetWindowInfo(FSwapChainInfo.WindowHandle, WindowInfo) then
      Exit(False);

    WindowRect := WindowInfo.rcClient;

    if FSwapChainInfo.VSync then
      FContext.DD7Object.WaitForVerticalBlank(DDWAITVB_BLOCKBEGIN, 0);

    Result := Succeeded(PrimarySurface.Blt(@WindowRect, FSurface, nil, DDBLT_WAIT, nil));
  finally
    PrimarySurface.SetClipper(PrevClipper);
  end;
end;

{$ENDREGION}
{$REGION 'TDX7SwapChains'}

constructor TDX7SwapChains.Create(const AContext: TDX7DeviceContext);
begin
  inherited Create;

  FContext := AContext;
  Increment_PXL_ClassInstances;
end;

destructor TDX7SwapChains.Destroy;
begin
  try
    Clear;
    FContext := nil;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TDX7SwapChains.GetCount: Integer;
begin
  Result := Length(FSwapChains);
end;

function TDX7SwapChains.GetItem(const Index: Integer): TDX7SwapChain;
begin
  if (Index >= 0) and (Index < Length(FSwapChains)) then
    Result := FSwapChains[Index]
  else
    Result := nil;
end;

procedure TDX7SwapChains.Clear;
var
  I: Integer;
begin
  for I := Length(FSwapChains) - 1 downto 0 do
    FreeAndNil(FSwapChains[I]);

  SetLength(FSwapChains, 0);
end;

function TDX7SwapChains.Add(const SwapChainInfo: PSwapChainInfo): Integer;
var
  NewItem: TDX7SwapChain;
begin
  NewItem := TDX7SwapChain.Create(FContext);

  if not NewItem.Initialize(SwapChainInfo) then
  begin
    NewItem.Free;
    Exit(-1);
  end;

  Result := Length(FSwapChains);
  SetLength(FSwapChains, Result + 1);

  FSwapChains[Result] := NewItem;
end;

function TDX7SwapChains.Recreate(const UserChains: TSwapChains): Boolean;
var
  I, Index: Integer;
  SwapChainInfo: PSwapChainInfo;
begin
  if UserChains = nil then
    Exit(False);

  Clear;
  Result := True;

  for I := 0 to UserChains.Count - 1 do
  begin
    SwapChainInfo := UserChains[I];
    if SwapChainInfo = nil then
    begin
      Result := False;
      Break;
    end;

    Index := Add(SwapChainInfo);

    Result := Index <> -1;
    if not Result then
      Break;
  end;

  if not Result then
    Clear;
end;

{$ENDREGION}

end.
