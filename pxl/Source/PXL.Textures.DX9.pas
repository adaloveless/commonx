unit PXL.Textures.DX9;
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
  Jedi.Direct3D9, PXL.Types, PXL.Devices, PXL.Textures, PXL.Types.DX9;

type
  TDX9SystemTexture = class(TCustomLockableTexture)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX9DeviceContext;
    FTexture: IDirect3DTexture9;

    function CreateTexture: Boolean;
    procedure DestroyTexture;
    function GetSurface: IDirect3DSurface9;
  protected
    function DoInitialize: Boolean; override;
    procedure DoFinalize; override;

    function DoLock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean; override;
    function DoUnlock: Boolean; override;
  public
    constructor Create(const ADevice: TCustomDevice; const AutoSubscribe: Boolean = False); override;

    property Context: TDX9DeviceContext read FContext;

    property Texture: IDirect3DTexture9 read FTexture;
    property Surface: IDirect3DSurface9 read GetSurface;
  end;

  TDX9LockableTexture = class(TCustomLockableTexture)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX9DeviceContext;
    FSysTexture: IDirect3DTexture9;
    FVidTexture: IDirect3DTexture9;

    FSysUsage: Cardinal;
    FVidUsage: Cardinal;
    FVidPool: TD3DPool;

    function ComputeParams: Boolean;

    function CreateSystemTexture: Boolean;
    procedure DestroySystemTexture;

    function CreateVideoTexture: Boolean;
    procedure DestroyVideoTexture;

    function CopySystemToVideo: Boolean;
    function GetSysSurface: IDirect3DSurface9;
    function GetVidSurface: IDirect3DSurface9;
  protected
    function DoInitialize: Boolean; override;
    procedure DoFinalize; override;

    function DoLock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean; override;
    function DoUnlock: Boolean; override;

    function DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
      const DestPos: TPoint2px): Boolean; override;
  public
    function Bind(const Channel: Integer): Boolean; override;

    function DeviceRestore: Boolean; override;
    procedure DeviceRelease; override;

    property Context: TDX9DeviceContext read FContext;

    property SysTexture: IDirect3DTexture9 read FSysTexture;
    property VidTexture: IDirect3DTexture9 read FVidTexture;

    property SysSurface: IDirect3DSurface9 read GetSysSurface;
    property VidSurface: IDirect3DSurface9 read GetVidSurface;
  end;

  TDX9DrawableTexture = class(TCustomDrawableTexture)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX9DeviceContext;
    FTexture: IDirect3DTexture9;
    FDepthBuffer: IDirect3DSurface9;
    FDepthStencilFormat: D3DFORMAT;

    FSavedBackBuffer: IDirect3DSurface9;
    FSavedDepthBuffer: IDirect3DSurface9;
    FSavedViewport: D3DVIEWPORT9;

    function GetSurface: IDirect3DSurface9;

    function CreateVidTexture: Boolean;
    procedure DestroyVidTexture;

    function SaveRenderBuffers: Boolean;
    procedure RestoreRenderBuffers;
    function SetRenderBuffers: Boolean;

    function UpdateViewport: Boolean;
    function RestoreViewport: Boolean;
  protected
    function DoInitialize: Boolean; override;
    procedure DoFinalize; override;

    function DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
      const DestPos: TPoint2px): Boolean; override;
  public
    function Bind(const Channel: Integer): Boolean; override;

    function Clear: Boolean; override;

    function DeviceRestore: Boolean; override;
    procedure DeviceRelease; override;

    function BeginDraw: Boolean; override;
    procedure EndDraw; override;

    property Context: TDX9DeviceContext read FContext;

    property Texture: IDirect3DTexture9 read FTexture;
    property Surface: IDirect3DSurface9 read GetSurface;

    property DepthBuffer: IDirect3DSurface9 read FDepthBuffer;
  end;

implementation

uses
  Windows, PXL.Formats;

{$REGION 'TDX9SystemTexture'}

constructor TDX9SystemTexture.Create(const ADevice: TCustomDevice; const AutoSubscribe: Boolean);
begin
  inherited;

  // This method is only needed to pass default "AutoSubscribe = False" parameter to inherited class.
end;

function TDX9SystemTexture.CreateTexture: Boolean;
var
  DXFormat: D3DFORMAT;
begin
  DXFormat := TDX9DeviceContext.NativeToFormat(FPixelFormat);
  if DXFormat = D3DFMT_UNKNOWN then
    Exit(False);

  Result := Succeeded(FContext.Direct3DDevice.CreateTexture(FWidth, FHeight, 1, 0, DXFormat, D3DPOOL_SYSTEMMEM,
    FTexture, nil));
end;

procedure TDX9SystemTexture.DestroyTexture;
begin
  FTexture := nil;
end;

function TDX9SystemTexture.DoInitialize: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TDX9DeviceContext)) then
    Exit(False);

  FContext := TDX9DeviceContext(Device.Context);

  if FContext.Direct3DDevice = nil then
    Exit(False);

  if FPixelFormat = TPixelFormat.Unknown then
    FPixelFormat := TPixelFormat.A8R8G8B8;

  FPixelFormat := FContext.FindTextureFormat(FPixelFormat, 0);
  if FPixelFormat = TPixelFormat.Unknown then
    Exit(False);

  FBytesPerPixel := FPixelFormat.Bytes;

  Result := CreateTexture;
end;

procedure TDX9SystemTexture.DoFinalize;
begin
  DestroyTexture;
  FContext := nil;
end;

function TDX9SystemTexture.DoLock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean;
var
  LockedRect: D3DLOCKED_RECT;
  LockRect: Windows.TRect;
  LockRectPtr: Windows.PRect;
begin
  if FTexture = nil then
  begin
    LockedPixels.Reset;
    Exit(False);
  end;

  if not IsLockRectFull(Rect) then
  begin
    LockRect.Left := Rect.Left;
    LockRect.Top := Rect.Top;
    LockRect.Right := Rect.Right;
    LockRect.Bottom := Rect.Bottom;
    LockRectPtr := @LockRect;
  end
  else
    LockRectPtr := nil;

  if Failed(FTexture.LockRect(0, LockedRect, LockRectPtr, 0)) then
  begin
    LockedPixels.Reset;
    Exit(False);
  end;

  LockedPixels.Bits := LockedRect.pBits;
  LockedPixels.Pitch := LockedRect.Pitch;

  LockedPixels.BytesPerPixel := FBytesPerPixel;
  LockedPixels.PixelFormat := FPixelFormat;

  if LockRectPtr <> nil then
    LockedPixels.LockedRect := Rect
  else
    LockedPixels.LockedRect := IntRect(0, 0, FWidth, FHeight);

  Result := True;
end;

function TDX9SystemTexture.DoUnlock: Boolean;
begin
  Result := (FTexture <> nil) and Succeeded(FTexture.UnlockRect(0));
end;

function TDX9SystemTexture.GetSurface: IDirect3DSurface9;
begin
  if FTexture = nil then
    Exit(nil);

  if Failed(FTexture.GetSurfaceLevel(0, Result)) then
    Result := nil;
end;

{$ENDREGION}
{$REGION 'TDX9LockableTexture'}

function TDX9LockableTexture.GetSysSurface: IDirect3DSurface9;
begin
  if FSysTexture = nil then
    Exit(nil);

  if Failed(FSysTexture.GetSurfaceLevel(0, Result)) then
    Result := nil;
end;

function TDX9LockableTexture.GetVidSurface: IDirect3DSurface9;
begin
  if FVidTexture = nil then
    Exit(nil);

  if Failed(FVidTexture.GetSurfaceLevel(0, Result)) then
    Result := nil;
end;

function TDX9LockableTexture.ComputeParams: Boolean;
begin
  FSysUsage := 0;
  FVidUsage := 0;

  if FMipMapping then
    FVidUsage := FVidUsage or D3DUSAGE_AUTOGENMIPMAP;

  if FContext.Support = TD3D9Support.Vista then
  begin // Vista enhanced mode.
    FVidPool := D3DPOOL_DEFAULT;

    if DynamicTexture then
    begin
      FSysUsage := FSysUsage or D3DUSAGE_DYNAMIC;
      FVidUsage := FVidUsage or D3DUSAGE_DYNAMIC;
    end;

    FPixelFormat := FContext.FindTextureFormatEx(FPixelFormat, FSysUsage, FVidUsage);
  end
  else
  begin // XP compatibility mode.
    FVidPool := D3DPOOL_MANAGED;

    if DynamicTexture then
    begin
      FVidUsage := FVidUsage or D3DUSAGE_DYNAMIC;
      FVidPool := D3DPOOL_DEFAULT;
    end;

    FPixelFormat := FContext.FindTextureFormat(FPixelFormat, FVidUsage);
  end;

  Result := FPixelFormat <> TPixelFormat.Unknown;
end;

function TDX9LockableTexture.CreateSystemTexture: Boolean;
var
  DXFormat: D3DFORMAT;
begin
  DXFormat := TDX9DeviceContext.NativeToFormat(FPixelFormat);
  if DXFormat = D3DFMT_UNKNOWN then
    Exit(False);

  Result := Succeeded(FContext.Direct3DDevice.CreateTexture(FWidth, FHeight, 1, FSysUsage, DXFormat, D3DPOOL_SYSTEMMEM,
    FSysTexture, nil));
end;

procedure TDX9LockableTexture.DestroySystemTexture;
begin
  FSysTexture := nil;
end;

function TDX9LockableTexture.CreateVideoTexture: Boolean;
var
  DXFormat: D3DFORMAT;
  MipLevels: Integer;
begin
  DXFormat := TDX9DeviceContext.NativeToFormat(FPixelFormat);
  if DXFormat = D3DFMT_UNKNOWN then
    Exit(False);

  MipLevels := 1;

  if FMipMapping then
    MipLevels := 0;

  Result := Succeeded(FContext.Direct3DDevice.CreateTexture(FWidth, FHeight, MipLevels, FVidUsage, DXFormat, FVidPool,
    FVidTexture, nil));
end;

procedure TDX9LockableTexture.DestroyVideoTexture;
begin
  FVidTexture := nil;
end;

function TDX9LockableTexture.DoInitialize: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TDX9DeviceContext)) then
    Exit(False);

  FContext := TDX9DeviceContext(Device.Context);

  if FContext.Direct3DDevice = nil then
    Exit(False);

  if FPixelFormat = TPixelFormat.Unknown then
    FPixelFormat := TPixelFormat.A8R8G8B8;

  if not ComputeParams then
    Exit(False);

  FBytesPerPixel := FPixelFormat.Bytes;

  if (FContext.Support = TD3D9Support.Vista) and (not CreateSystemTexture) then
    Exit(False);

  Result := CreateVideoTexture;
end;

procedure TDX9LockableTexture.DoFinalize;
begin
  DestroyVideoTexture;
  DestroySystemTexture;
  FContext := nil;
end;

function TDX9LockableTexture.Bind(const Channel: Integer): Boolean;
begin
  if (FContext = nil) or (FContext.Direct3DDevice = nil) or (FVidTexture = nil) then
    Exit(False);

  Result := Succeeded(FContext.Direct3DDevice.SetTexture(Channel, FVidTexture));
end;

function TDX9LockableTexture.DeviceRestore: Boolean;
begin
  if (FState = TTextureState.Lost) and (FContext <> nil) and (FContext.Support <> TD3D9Support.Vista) and
    (FVidPool = D3DPOOL_DEFAULT) then
  begin
    if not CreateVideoTexture then
    begin
      FState := TTextureState.NotRecovered;
      Exit(False);
    end;

    FState := TTextureState.Initialized;
    Result := True;
  end
  else
    Result := False;
end;

procedure TDX9LockableTexture.DeviceRelease;
begin
  if (FState = TTextureState.Initialized) and (FContext <> nil) and (FContext.Support <> TD3D9Support.Vista) and
    (FVidPool = D3DPOOL_DEFAULT) then
  begin
    DestroyVideoTexture;
    FState := TTextureState.Lost;
  end;
end;

function TDX9LockableTexture.DoLock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean;
var
  LockedRect: D3DLOCKED_RECT;
  Usage: Cardinal;
  LockRect: Windows.TRect;
  LockRectPtr: Windows.PRect;
begin
  // If the rectangle specified in Rect is the entire texture, then provide null pointer instead.
  if not IsLockRectFull(Rect) then
  begin
    LockRect.Left := Rect.Left;
    LockRect.Top := Rect.Top;
    LockRect.Right := Rect.Right;
    LockRect.Bottom := Rect.Bottom;
    LockRectPtr := @LockRect;
  end
  else
    LockRectPtr := nil;

  Usage := 0;

  if DynamicTexture then
  begin
    Usage := D3DLOCK_DISCARD;

    // Only the entire texture can be locked at a time when dealing with dynamic textures.
    if LockRectPtr <> nil then
      Exit(False);
  end;

  if FContext.Support = TD3D9Support.Vista then
    Result := (FSysTexture <> nil) and Succeeded(FSysTexture.LockRect(0, LockedRect, LockRectPtr, Usage))
  else
    Result := (FVidTexture <> nil) and Succeeded(FVidTexture.LockRect(0, LockedRect, LockRectPtr, Usage));

  if Result then
  begin
    LockedPixels.Bits := LockedRect.pBits;
    LockedPixels.Pitch := LockedRect.Pitch;

    LockedPixels.BytesPerPixel := FBytesPerPixel;
    LockedPixels.PixelFormat := FPixelFormat;

    if LockRectPtr <> nil then
      LockedPixels.LockedRect := Rect
    else
      LockedPixels.LockedRect := IntRect(0, 0, FWidth, FHeight);
  end
  else
    LockedPixels.Reset;
end;

function TDX9LockableTexture.CopySystemToVideo: Boolean;
begin
  if (FContext = nil) or (FContext.Direct3DDevice = nil) or (FSysTexture = nil) or (FVidTexture = nil) then
    Exit(False);

  Result := Succeeded(FContext.Direct3DDevice.UpdateTexture(FSysTexture, FVidTexture));
end;

function TDX9LockableTexture.DoUnlock: Boolean;
begin
  if FContext.Support = TD3D9Support.Vista then
  begin // Vista enhanced mode.
    if (FSysTexture = nil) or Failed(FSysTexture.UnlockRect(0)) then
      Exit(False);

    Result := CopySystemToVideo;
  end
  else
  begin // XP compatibility mode.
    if FVidTexture <> nil then
      Result := Succeeded(FVidTexture.UnlockRect(0))
    else
      Result := False;
  end;
end;

function TDX9LockableTexture.DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
  const DestPos: TPoint2px): Boolean;
var
  SysTexture: TDX9SystemTexture;
begin
  if Source is TDX9DrawableTexture then
  begin
    if (Size <> Source.Size) or (DestPos <> ZeroPoint2px) or (SourceRect.TopLeft <> ZeroPoint2px) or
      (SourceRect.Size <> Source.Size) or (FPixelFormat <> Source.PixelFormat) or
      (FContext.Support <> TD3D9Support.Vista) then
    begin // Retrieve render target data to intermediary system texture.
      if (FContext = nil) or (FContext.Direct3DDevice = nil) then
        Exit(False);

      SysTexture := TDX9SystemTexture.Create(Device);
      try
        SysTexture.Width := Source.Width;
        SysTexture.Height := Source.Height;
        SysTexture.PixelFormat := Source.PixelFormat;

        if (not SysTexture.Initialize) or (SysTexture.PixelFormat <> Source.PixelFormat) then
          Exit(False);

        if Failed(FContext.Direct3DDevice.GetRenderTargetData(TDX9DrawableTexture(Source).Surface,
          SysTexture.Surface)) then
          Exit(False);

        Result := CopyRect(DestPos, SysTexture, SourceRect);
      finally
        SysTexture.Free;
      end;
    end
    else
    begin // Retrieve render target data directly into current system texture.
      if (FContext = nil) or (FContext.Direct3DDevice = nil) or (FSysTexture = nil) or (FVidTexture = nil) then
        Exit(False);

      if Failed(FContext.Direct3DDevice.GetRenderTargetData(TDX9DrawableTexture(Source).Surface, SysSurface)) then
        Exit(False);

      Result := Succeeded(FContext.Direct3DDevice.UpdateTexture(FSysTexture, FVidTexture));
    end;
  end
  else
    Result := inherited;
end;

{$ENDREGION}
{$REGION 'TDX9DrawableTexture'}

function TDX9DrawableTexture.GetSurface: IDirect3DSurface9;
begin
  if FTexture = nil then
    Exit(nil);

  if Failed(FTexture.GetSurfaceLevel(0, Result)) then
    Result := nil;
end;

function TDX9DrawableTexture.CreateVidTexture: Boolean;
var
  MipLevels: Integer;
  FVidUsage: Cardinal;
  DXFormat: D3DFORMAT;
begin
  MipLevels := 1;
  FVidUsage := D3DUSAGE_RENDERTARGET;

  if FMipMapping then
  begin
    MipLevels := 0;
    FVidUsage := FVidUsage or D3DUSAGE_AUTOGENMIPMAP;
  end;

  FPixelFormat := FContext.FindTextureFormat(FPixelFormat, FVidUsage);
  if FPixelFormat = TPixelFormat.Unknown then
    Exit(False);

  if DepthStencil > TDepthStencil.None then
  begin
    FDepthStencilFormat := FContext.FindDepthStencilFormat(DepthStencil);
    if FDepthStencilFormat = D3DFMT_UNKNOWN then
      Exit(False);
  end;

  DXFormat := TDX9DeviceContext.NativeToFormat(FPixelFormat);

  if Failed(FContext.Direct3DDevice.CreateTexture(FWidth, FHeight, MipLevels, FVidUsage, DXFormat, D3DPOOL_DEFAULT,
    FTexture, nil)) then
    Exit(False);

  if DepthStencil > TDepthStencil.None then
  begin
    if Failed(FContext.Direct3DDevice.CreateDepthStencilSurface(FWidth, FHeight, FDepthStencilFormat,
      D3DMULTISAMPLE_NONE, 0, True, FDepthBuffer, nil)) then
    begin
      FTexture := nil;
      Exit(False);
    end;
  end;

  Result := True;
end;

procedure TDX9DrawableTexture.DestroyVidTexture;
begin
  FDepthBuffer := nil;
  FTexture := nil;
  FDepthStencilFormat := D3DFMT_UNKNOWN;
end;

function TDX9DrawableTexture.DoInitialize: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TDX9DeviceContext)) then
    Exit(False);

  FContext := TDX9DeviceContext(Device.Context);

  if FContext.Direct3DDevice = nil then
    Exit(False);

  if FPixelFormat = TPixelFormat.Unknown then
    FPixelFormat := TPixelFormat.A8R8G8B8;

  Result := CreateVidTexture;
end;

procedure TDX9DrawableTexture.DoFinalize;
begin
  DestroyVidTexture;
end;

function TDX9DrawableTexture.Bind(const Channel: Integer): Boolean;
begin
  if (FContext = nil) or (FContext.Direct3DDevice = nil) or (FTexture = nil) then
    Exit(False);

  Result := Succeeded(FContext.Direct3DDevice.SetTexture(Channel, FTexture));
end;

function TDX9DrawableTexture.Clear: Boolean;
var
  ClearFlags: Cardinal;
begin
  if (FContext = nil) or (FContext.Direct3DDevice = nil) then
    Exit(False);

  if not BeginDraw then
    Exit(False);
  try
    ClearFlags := D3DCLEAR_TARGET;

    if FDepthStencil >= TDepthStencil.DepthOnly then
      ClearFlags := ClearFlags or D3DCLEAR_ZBUFFER;

    if FDepthStencil >= TDepthStencil.Full then
      ClearFlags := ClearFlags or D3DCLEAR_STENCIL;

    Result := Succeeded(FContext.Direct3DDevice.Clear(0, nil, ClearFlags, 0, 1.0, 0));
  finally
    EndDraw;
  end;
end;

function TDX9DrawableTexture.DeviceRestore: Boolean;
begin
  if FContext.Support <> TD3D9Support.Vista then
    Result := CreateVidTexture
  else
    Result := True;
end;

procedure TDX9DrawableTexture.DeviceRelease;
begin
  if FContext.Support <> TD3D9Support.Vista then
    DestroyVidTexture;
end;

function TDX9DrawableTexture.SaveRenderBuffers: Boolean;
var
  Res: HResult;
begin
  if (FContext = nil) or (FContext.Direct3DDevice = nil) then
    Exit(False);

  if Failed(FContext.Direct3DDevice.GetRenderTarget(0, FSavedBackBuffer)) then
    Exit(False);

  Res := FContext.Direct3DDevice.GetDepthStencilSurface(FSavedDepthBuffer);
  if Res = D3DERR_NOTFOUND then
    FSavedDepthBuffer := nil
  else if Failed(Res) then
  begin
    FSavedBackBuffer := nil;
    Exit(False);
  end;

  Result := True;
end;

procedure TDX9DrawableTexture.RestoreRenderBuffers;
begin
  if (FContext = nil) or (FContext.Direct3DDevice = nil) then
    Exit;

  FContext.Direct3DDevice.SetDepthStencilSurface(FSavedDepthBuffer);
  FContext.Direct3DDevice.SetRenderTarget(0, FSavedBackBuffer);

  FSavedDepthBuffer := nil;
  FSavedBackBuffer := nil;
end;

function TDX9DrawableTexture.SetRenderBuffers: Boolean;
var
  Surface: IDirect3DSurface9;
begin
  if (FContext = nil) or (FContext.Direct3DDevice = nil) or (FTexture = nil) then
    Exit(False);

  if Failed(FTexture.GetSurfaceLevel(0, Surface)) or (Surface = nil) then
    Exit(False);

  if Failed(FContext.Direct3DDevice.SetRenderTarget(0, Surface)) then
    Exit(False);

  Result := Succeeded(FContext.Direct3DDevice.SetDepthStencilSurface(FDepthBuffer));
end;

function TDX9DrawableTexture.UpdateViewport: Boolean;
var
  NewViewport: D3DVIEWPORT9;
begin
  if (FContext = nil) or (FContext.Direct3DDevice = nil) then
    Exit(False);

  if Failed(FContext.Direct3DDevice.GetViewport(FSavedViewport)) then
    Exit(False);

  NewViewport.X := 0;
  NewViewport.Y := 0;
  NewViewport.Width := FWidth;
  NewViewport.Height := FHeight;
  NewViewport.MinZ := 0;
  NewViewport.MaxZ := 1;

  Result := Succeeded(FContext.Direct3DDevice.SetViewport(NewViewport));
end;

function TDX9DrawableTexture.RestoreViewport: Boolean;
begin
  if (FContext = nil) or (FContext.Direct3DDevice = nil) then
    Exit(False);

  Result := Succeeded(FContext.Direct3DDevice.SetViewport(FSavedViewport));
  FillChar(FSavedViewport, SizeOf(D3DVIEWPORT9), 0);
end;

function TDX9DrawableTexture.BeginDraw: Boolean;
begin
  if not SaveRenderBuffers then
    Exit(False);

  if not SetRenderBuffers then
  begin
    RestoreRenderBuffers;
    Exit(False);
  end;

  UpdateViewport;
  Result := True;
end;

procedure TDX9DrawableTexture.EndDraw;
begin
  RestoreRenderBuffers;
  RestoreViewport;
end;

function TDX9DrawableTexture.DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
  const DestPos: TPoint2px): Boolean;
var
  SysTexture: TDX9SystemTexture;
  WinSourceRect, WinDestRect: Windows.TRect;
begin
  if (Device <> nil) and (Source.Device = Device) and (FContext <> nil) and (FContext.Direct3DDevice <> nil) and
    (FTexture <> nil) then
  begin
    if Source is TDX9LockableTexture then
    begin
      SysTexture := TDX9SystemTexture.Create(Device);
      try
        SysTexture.Width := FWidth;
        SysTexture.Height := FHeight;
        SysTexture.PixelFormat := FPixelFormat;

        if (not SysTexture.Initialize) or (SysTexture.PixelFormat <> FPixelFormat) then
          Exit(False);

        if ((DestPos <> ZeroPoint2px) or (SourceRect.TopLeft <> ZeroPoint2px) or
          (SourceRect.Size <> SysTexture.Size)) and (not SysTexture.Clear) then
          Exit(False);

        if not SysTexture.CopyRect(DestPos, Source, SourceRect) then
          Exit(False);

        Result := Succeeded(FContext.Direct3DDevice.UpdateTexture(SysTexture.Texture, FTexture));
      finally
        SysTexture.Free;
      end;
    end
    else if (Source is TDX9DrawableTexture) and (Source.PixelFormat = FPixelFormat) then
    begin
      WinSourceRect.Left := SourceRect.Left;
      WinSourceRect.Top := SourceRect.Top;
      WinSourceRect.Right := SourceRect.Right;
      WinSourceRect.Bottom := SourceRect.Bottom;

      WinDestRect.Left := DestPos.X;
      WinDestRect.Top := DestPos.Y;
      WinDestRect.Right := DestPos.X + SourceRect.Width;
      WinDestRect.Bottom := DestPos.Y + SourceRect.Height;

      Result := Succeeded(FContext.Direct3DDevice.StretchRect(TDX9DrawableTexture(Source).Surface, @WinSourceRect,
        Surface, @WinDestRect, D3DTEXF_NONE));
    end
    else
      Result := inherited;
  end
  else
    Result := inherited;
end;

{$ENDREGION}

end.
