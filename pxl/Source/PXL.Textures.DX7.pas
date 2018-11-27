unit PXL.Textures.DX7;
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
  Windows, PXL.Windows.DDraw, PXL.Windows.D3D7, PXL.Types, PXL.Surfaces, PXL.Textures, PXL.Types.DX7;

type
  TDX7LockableTexture = class(TCustomLockableTexture)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX7DeviceContext;
    FSurface: IDirectDrawSurface7;
    FSurfaceDesc: DDSURFACEDESC2;

    FLockedRect: TIntRect;
    FLockedRectPtr: Pointer;

    function InitSurfaceDesc: Boolean;
    function CreateTextureSurface: Boolean;
    procedure DestroyTextureSurface;

    function GetSubSurface(const Level: Integer): IDirectDrawSurface7;
    function GetSubSurfaceSize(const Level: Integer): TPoint2px;

    procedure IntToLockRect(const Rect: PIntRect; var LockRect: Windows.TRect; var LockRectPtr: Windows.PRect);

    function LockRect(const Rect: PIntRect; const Level: Integer; out LockedPixels: TLockedPixels): Boolean;
    function UnlockRect(const Rect: PIntRect; const Level: Integer): Boolean;

    function GetPixelData(const Level: Integer; const Surface: TPixelSurface): Boolean;
    function SetPixelData(const Level: Integer; const Surface: TPixelSurface): Boolean;
  protected
    function DoInitialize: Boolean; override;
    procedure DoFinalize; override;

    function DoLock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean; override;
    function DoUnlock: Boolean; override;

    function DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
      const DestPos: TPoint2px): Boolean; override;

    function UpdateMipMaps: Boolean; virtual;
  public
    function Bind(const Channel: Integer): Boolean; override;

    property Context: TDX7DeviceContext read FContext;

    property Surface: IDirectDrawSurface7 read FSurface;
    property SurfaceDesc: DDSURFACEDESC2 read FSurfaceDesc;
  end;

  TDX7DrawableTexture = class(TCustomDrawableTexture)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX7DeviceContext;
    FSurface: IDirectDrawSurface7;
    FSurfaceDesc: DDSURFACEDESC2;
    FPrevSurface: IDirectDrawSurface7;
    FPrevViewport: D3DVIEWPORT7;

    function InitSurfaceDesc: Boolean;
    function CreateTextureSurface: Boolean;
    procedure DestroyTextureSurface;
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

    property Context: TDX7DeviceContext read FContext;

    property Surface: IDirectDrawSurface7 read FSurface;
    property SurfaceDesc: DDSURFACEDESC2 read FSurfaceDesc;
  end;

implementation

uses
  Math, PXL.Formats;

{$REGION 'Global Functions'}

function ComputeMipLevels(Width, Height: Integer): Integer;
begin
  Result := 1;

  while (Width > 1) and (Height > 1) and (Width and 1 = 0) and (Height and 1 = 0) do
  begin
    Width := Width div 2;
    Height := Height div 2;
    Inc(Result);
  end;
end;

function RectToWinRect(const Rect: TIntRect): Windows.TRect;
begin
  Result.Left := Rect.Left;
  Result.Top := Rect.Top;
  Result.Right := Rect.Right;
  Result.Bottom := Rect.Bottom;
end;

{$ENDREGION}
{$REGION 'TDX7LockableTexture'}

function TDX7LockableTexture.InitSurfaceDesc: Boolean;
begin
  FillChar(FSurfaceDesc, SizeOf(DDSURFACEDESC2), 0);

  FSurfaceDesc.dwSize := SizeOf(DDSURFACEDESC2);
  FSurfaceDesc.dwFlags := DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or DDSD_PIXELFORMAT or DDSD_TEXTURESTAGE;
  FSurfaceDesc.ddsCaps.dwCaps := DDSCAPS_TEXTURE;
  FSurfaceDesc.ddsCaps.dwCaps2 := DDSCAPS2_TEXTUREMANAGE;
  FSurfaceDesc.dwWidth := Width;
  FSurfaceDesc.dwHeight := Height;

  if MipMapping then
  begin
    FSurfaceDesc.dwFlags := FSurfaceDesc.dwFlags or DDSD_MIPMAPCOUNT;
    FSurfaceDesc.ddsCaps.dwCaps := FSurfaceDesc.ddsCaps.dwCaps or DDSCAPS_MIPMAP or DDSCAPS_COMPLEX;
    FSurfaceDesc.dwMipMapCount := ComputeMipLevels(Width, Height);
  end;

  Result := TDX7DeviceContext.NativeToFormat(FPixelFormat, FSurfaceDesc.ddpfPixelFormat);
end;

function TDX7LockableTexture.CreateTextureSurface: Boolean;
begin
  if (FContext.DD7Object = nil) or (not InitSurfaceDesc) then
    Exit(False);

  if Failed(FContext.DD7Object.CreateSurface(FSurfaceDesc, FSurface, nil)) then
    Exit(False);

  if Failed(FSurface.GetSurfaceDesc(FSurfaceDesc)) then
  begin
    FSurface := nil;
    Exit(False);
  end;

  Result := True;
end;

procedure TDX7LockableTexture.DestroyTextureSurface;
begin
  FSurface := nil;
  FillChar(FSurfaceDesc, SizeOf(DDSURFACEDESC2), 0);
end;

function TDX7LockableTexture.DoInitialize: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TDX7DeviceContext)) then
    Exit(False);

  FContext := TDX7DeviceContext(Device.Context);

  if FPixelFormat = TPixelFormat.Unknown then
    FPixelFormat := TPixelFormat.A8R8G8B8;

  FPixelFormat := FContext.FindTextureFormat(FPixelFormat);
  if FPixelFormat = TPixelFormat.Unknown then
    Exit(False);

  FBytesPerPixel := FPixelFormat.Bytes;

  Result := CreateTextureSurface;
end;

procedure TDX7LockableTexture.DoFinalize;
begin
  DestroyTextureSurface;
  FContext := nil;
end;

function TDX7LockableTexture.Bind(const Channel: Integer): Boolean;
begin
  if (FContext.D3D7Device <> nil) and (FSurface <> nil) then
    Result := Succeeded(FContext.D3D7Device.SetTexture(Channel, FSurface))
  else
    Result := False;
end;

function TDX7LockableTexture.GetSubSurface(const Level: Integer): IDirectDrawSurface7;
var
  CurSurface, TempSurface: IDirectDrawSurface7;
  SubLevel: Integer;
  Caps: DDSCAPS2;
begin
  if (FSurface = nil) or (Level < 0) then
    Exit(nil);

  if Level = 0 then
    Exit(FSurface);

  CurSurface := FSurface;
  SubLevel := Level;

  repeat
    FillChar(Caps, SizeOf(DDSCAPS2), 0);
    Caps.dwCaps := DDSCAPS_MIPMAP;

    if Failed(CurSurface.GetAttachedSurface(Caps, TempSurface)) then
      Exit(nil);

    CurSurface := TempSurface;
    Dec(SubLevel);
  until SubLevel <= 0;

  Result := CurSurface;
end;

function TDX7LockableTexture.GetSubSurfaceSize(const Level: Integer): TPoint2px;
var
  SubSurface: IDirectDrawSurface7;
  SubDesc: DDSURFACEDESC2;
begin
  SubSurface := GetSubSurface(Level);
  if SubSurface = nil then
    Exit(ZeroPoint2px);

  FillChar(SubDesc, SizeOf(DDSURFACEDESC2), 0);

  SubDesc.dwSize := SizeOf(DDSURFACEDESC2);

  if Succeeded(SubSurface.GetSurfaceDesc(SubDesc)) then
  begin
    Result.X := SubDesc.dwWidth;
    Result.Y := SubDesc.dwHeight;
  end
  else
    Result := ZeroPoint2px;
end;

procedure TDX7LockableTexture.IntToLockRect(const Rect: PIntRect; var LockRect: Windows.TRect;
  var LockRectPtr: Windows.PRect);
begin
  if Rect <> nil then
  begin
    LockRect.Left := Rect.Left;
    LockRect.Top := Rect.Top;
    LockRect.Right := Rect.Right;
    LockRect.Bottom := Rect.Bottom;
    LockRectPtr := @LockRect;
  end
  else
    LockRectPtr := nil;
end;

function TDX7LockableTexture.LockRect(const Rect: PIntRect; const Level: Integer;
  out LockedPixels: TLockedPixels): Boolean;
var
  SubSurface: IDirectDrawSurface7;
  SubDesc: DDSURFACEDESC2;
  LockRect: Windows.TRect;
  LockRectPtr: Windows.PRect;
begin
  SubSurface := GetSubSurface(Level);
  if SubSurface = nil then
  begin
    LockedPixels.Reset;
    Exit(False);
  end;

  FillChar(SubDesc, SizeOf(DDSURFACEDESC2), 0);

  SubDesc.dwSize := SizeOf(DDSURFACEDESC2);

  IntToLockRect(Rect, LockRect, LockRectPtr);

  Result := Succeeded(SubSurface.Lock(LockRectPtr, SubDesc, DDLOCK_SURFACEMEMORYPTR or DDLOCK_WAIT, 0));
  if Result then
  begin
    LockedPixels.Bits := SubDesc.lpSurface;
    LockedPixels.Pitch := SubDesc.lPitch;

    LockedPixels.BytesPerPixel := FBytesPerPixel;
    LockedPixels.PixelFormat := FPixelFormat;

    if Rect <> nil then
      LockedPixels.LockedRect := Rect^
    else
      LockedPixels.LockedRect := IntRect(0, 0, Width, Height);
  end
  else
    LockedPixels.Reset;
end;

function TDX7LockableTexture.UnlockRect(const Rect: PIntRect; const Level: Integer): Boolean;
var
  SubSurface: IDirectDrawSurface7;
  LockRect: Windows.TRect;
  LockRectPtr: Windows.PRect;
begin
  SubSurface := GetSubSurface(Level);
  if SubSurface <> nil then
  begin
    IntToLockRect(Rect, LockRect, LockRectPtr);
    Result := Succeeded(SubSurface.Unlock(LockRectPtr));
  end
  else
    Result := False;
end;

function TDX7LockableTexture.DoLock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean;
begin
  if not IsLockRectFull(Rect) then
  begin
    FLockedRect := Rect;
    FLockedRectPtr := @FLockedRect;
  end
  else
    FLockedRectPtr := nil;

  if not LockRect(FLockedRectPtr, 0, LockedPixels) then
  begin
    LockedPixels.Reset;
    Exit(False)
  end;

  Result := True;
end;

function TDX7LockableTexture.DoUnlock: Boolean;
begin
  if not UnlockRect(FLockedRectPtr, 0) then
    Exit(False);

  if MipMapping and (FSurfaceDesc.dwMipMapCount > 1) then
    Result := UpdateMipMaps
  else
    Result := True;
end;

function TDX7LockableTexture.GetPixelData(const Level: Integer; const Surface: TPixelSurface): Boolean;
var
  SubSize: TPoint2px;
  LockedPixels: TLockedPixels;
  I, CopyBytes: Integer;
begin
  if Surface = nil then
    Exit(False);

  SubSize := GetSubSurfaceSize(Level);
  if (SubSize.X < 1) or (SubSize.Y < 1) then
    Exit(False);

  Surface.SetSize(SubSize, FPixelFormat);
  if Surface.PixelFormat <> FPixelFormat then
    Exit(False);

  if not LockRect(nil, Level, LockedPixels) then
    Exit(False);
  try
    CopyBytes := SubSize.X * FBytesPerPixel;

    for I := 0 to SubSize.Y - 1 do
      Move(LockedPixels.Scanline[I]^, Surface.Scanline[I]^, CopyBytes);
  finally
    Result := UnlockRect(nil, Level);
  end;
end;

function TDX7LockableTexture.SetPixelData(const Level: Integer; const Surface: TPixelSurface): Boolean;
var
  SubSize: TPoint2px;
  LockedPixels: TLockedPixels;
  I, CopyBytes: Integer;
begin
  if (Surface = nil) or (Surface.PixelFormat <> FPixelFormat) then
    Exit(False);

  SubSize := GetSubSurfaceSize(Level);
  if (SubSize.X < 1) or (SubSize.Y < 1) then
    Exit(False);

  if not LockRect(nil, Level, LockedPixels) then
    Exit(False);
  try
    CopyBytes := Min(SubSize.X, Surface.Width) * FBytesPerPixel;

    for I := 0 to Min(SubSize.Y, Surface.Height) - 1 do
      Move(Surface.Scanline[I]^, LockedPixels.Scanline[I]^, CopyBytes);
  finally
    Result := UnlockRect(nil, Level);
  end;
end;

function TDX7LockableTexture.UpdateMipMaps: Boolean;
var
  MipSurface: TPixelMipMapSurface;
  SubSurface: TPixelSurface;
  I: Integer;
begin
  if (FSurface = nil) or (FSurfaceDesc.dwMipMapCount < 2) then
    Exit(False);

  MipSurface := TPixelMipMapSurface.Create;
  try
    if not GetPixelData(0, MipSurface) then
      Exit(False);

    MipSurface.GenerateMipMaps;

    for I := 1 to FSurfaceDesc.dwMipMapCount - 1 do
    begin
      SubSurface := MipSurface.MipMaps[I - 1];
      if SubSurface = nil then
        Break;

      if not SetPixelData(I, SubSurface) then
        Break;
    end;
  finally
    MipSurface.Free;
  end;

  Result := True;
end;

function TDX7LockableTexture.DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
  const DestPos: TPoint2px): Boolean;
var
  WinDestRect, WinSourceRect: Windows.TRect;
begin
  if (Device <> nil) and (Source.Device = Device) and (FSurface <> nil) then
  begin
    if Source is TDX7LockableTexture then
    begin
      WinSourceRect := RectToWinRect(SourceRect);
      WinDestRect := RectToWinRect(IntRect(DestPos, SourceRect.Size));

      Result := Succeeded(FSurface.Blt(@WinDestRect, TDX7LockableTexture(Source).Surface, @WinSourceRect, DDBLT_WAIT,
        nil));
    end
    else if Source is TDX7DrawableTexture then
    begin
      WinSourceRect := RectToWinRect(SourceRect);
      WinDestRect := RectToWinRect(IntRect(DestPos, SourceRect.Size));

      Result := Succeeded(FSurface.Blt(@WinDestRect, TDX7DrawableTexture(Source).Surface, @WinSourceRect, DDBLT_WAIT,
        nil));
    end
    else
      Result := inherited;
  end
  else
    Result := inherited;
end;

{$ENDREGION}
{$REGION 'TDX7DrawableTexture'}

function TDX7DrawableTexture.InitSurfaceDesc: Boolean;
begin
  FillChar(FSurfaceDesc, SizeOf(DDSURFACEDESC2), 0);

  FSurfaceDesc.dwSize := SizeOf(DDSURFACEDESC2);
  FSurfaceDesc.dwFlags := DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or DDSD_PIXELFORMAT or DDSD_TEXTURESTAGE;
  FSurfaceDesc.ddsCaps.dwCaps := DDSCAPS_TEXTURE or DDSCAPS_3DDEVICE or DDSCAPS_VIDEOMEMORY;
  FSurfaceDesc.dwWidth := Width;
  FSurfaceDesc.dwHeight := Height;

  Result := TDX7DeviceContext.NativeToFormat(FPixelFormat, FSurfaceDesc.ddpfPixelFormat);
end;

function TDX7DrawableTexture.CreateTextureSurface: Boolean;
begin
  if (FContext.DD7Object = nil) or (not InitSurfaceDesc) then
    Exit(False);

  if Failed(FContext.DD7Object.CreateSurface(FSurfaceDesc, FSurface, nil)) then
    Exit(False);

  if Failed(FSurface.GetSurfaceDesc(FSurfaceDesc)) then
  begin
    FSurface := nil;
    Exit(False);
  end;

  Result := True;
end;

procedure TDX7DrawableTexture.DestroyTextureSurface;
begin
  FPrevSurface := nil;
  FSurface := nil;
  FillChar(FSurfaceDesc, SizeOf(DDSURFACEDESC2), 0);
end;

function TDX7DrawableTexture.DoInitialize: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TDX7DeviceContext)) then
    Exit(False);

  FContext := TDX7DeviceContext(Device.Context);

  FPixelFormat := FContext.FindTextureFormat(FPixelFormat);
  if FPixelFormat = TPixelFormat.Unknown then
    Exit(False);

  Result := CreateTextureSurface;
end;

procedure TDX7DrawableTexture.DoFinalize;
begin
  DestroyTextureSurface;
  FContext := nil;
end;

function TDX7DrawableTexture.Bind(const Channel: Integer): Boolean;
begin
  if (FContext.D3D7Device <> nil) and (FSurface <> nil) then
    Result := Succeeded(FContext.D3D7Device.SetTexture(Channel, FSurface))
  else
    Result := False;
end;

function TDX7DrawableTexture.Clear: Boolean;
var
  ClearFlags: Cardinal;
begin
  if (FContext = nil) or (FContext.D3D7Device = nil) then
    Exit(False);

  if not BeginDraw then
    Exit(False);
  try
    ClearFlags := D3DCLEAR_TARGET;

    if FDepthStencil >= TDepthStencil.DepthOnly then
      ClearFlags := ClearFlags or D3DCLEAR_ZBUFFER;

    if FDepthStencil >= TDepthStencil.Full then
      ClearFlags := ClearFlags or D3DCLEAR_STENCIL;

    Result := Succeeded(FContext.D3D7Device.Clear(0, nil, ClearFlags, 0, 1.0, 0));
  finally
    EndDraw;
  end;
end;

function TDX7DrawableTexture.DeviceRestore: Boolean;
begin
  Result := CreateTextureSurface;
end;

procedure TDX7DrawableTexture.DeviceRelease;
begin
  DestroyTextureSurface;
end;

function TDX7DrawableTexture.BeginDraw: Boolean;
var
  Viewport: D3DVIEWPORT7;
begin
  if (FContext.D3D7Device = nil) or (FSurface = nil) then
    Exit(False);

  FillChar(FPrevViewport, SizeOf(D3DVIEWPORT7), 0);

  if Failed(FContext.D3D7Device.GetViewport(FPrevViewport)) then
    Exit(False);

  if Failed(FContext.D3D7Device.GetRenderTarget(FPrevSurface)) then
    Exit(False);

  if Failed(FContext.D3D7Device.SetRenderTarget(FSurface, 0)) then
  begin
    FPrevSurface := nil;
    Exit(False);
  end;

  Viewport.dwX := 0;
  Viewport.dwY := 0;
  Viewport.dwWidth := Width;
  Viewport.dwHeight := Height;
  Viewport.dvMinZ := 0.0;
  Viewport.dvMaxZ := 1.0;

  Result := Succeeded(FContext.D3D7Device.SetViewport(Viewport));
end;

procedure TDX7DrawableTexture.EndDraw;
begin
  if FContext.D3D7Device <> nil then
  begin
    FContext.D3D7Device.SetRenderTarget(FPrevSurface, 0);
    FContext.D3D7Device.SetViewport(FPrevViewport);
  end;

  FPrevSurface := nil;
  FillChar(FPrevViewport, SizeOf(D3DVIEWPORT7), 0);
end;

function TDX7DrawableTexture.DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
  const DestPos: TPoint2px): Boolean;
var
  WinDestRect, WinSourceRect: Windows.TRect;
begin
  if (Device <> nil) and (Source.Device = Device) and (FSurface <> nil) then
  begin
    if Source is TDX7LockableTexture then
    begin
      WinSourceRect := RectToWinRect(SourceRect);
      WinDestRect := RectToWinRect(IntRect(DestPos, SourceRect.Size));

      Result := Succeeded(FSurface.Blt(@WinDestRect, TDX7LockableTexture(Source).Surface, @WinSourceRect, DDBLT_WAIT,
        nil));
    end
    else if Source is TDX7DrawableTexture then
    begin
      WinSourceRect := RectToWinRect(SourceRect);
      WinDestRect := RectToWinRect(IntRect(DestPos, SourceRect.Size));

      Result := Succeeded(FSurface.Blt(@WinDestRect, TDX7DrawableTexture(Source).Surface, @WinSourceRect, DDBLT_WAIT,
        nil));
    end
    else
      Result := inherited;
  end
  else
    Result := inherited;
end;

{$ENDREGION}

end.
