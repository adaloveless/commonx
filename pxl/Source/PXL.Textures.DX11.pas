unit PXL.Textures.DX11;
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
  PXL.Windows.DXGI, PXL.Windows.D3D11, PXL.TypeDef, PXL.Types, PXL.Surfaces, PXL.Devices, PXL.Textures, PXL.Types.DX11;

type
  TDX11LockableTexture = class(TCustomLockableTexture)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX11DeviceContext;
    FSurface: TPixelMipMapSurface;
    FTexture: ID3D11Texture2D;
    FResourceView: ID3D11ShaderResourceView;

    function CreateTextureInstance: Boolean;
    procedure DestroyTextureInstance;
    function CreateDynamicTexture: Boolean;
    function UploadDynamicTexture: Boolean;
    function CreateDefaultTexture: Boolean;
    function UploadTexture: Boolean;
    function CreateShaderResourceView: Boolean;
    function CreateStagingTexture(const Size: TPoint2px; out Texture: ID3D11Texture2D): Boolean;
    function DownloadStagingTexture(const DestPos, Size: TPoint2px; const Texture: ID3D11Texture2D): Boolean;
  protected
    function DoInitialize: Boolean; override;
    procedure DoFinalize; override;

    function DoLock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean; override;
    function DoUnlock: Boolean; override;

    function DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
      const DestPos: TPoint2px): Boolean; override;
  public
    constructor Create(const ADevice: TCustomDevice; const AutoSubscribe: Boolean); override;
    destructor Destroy; override;

    function Bind(const Channel: Integer): Boolean; override;

    function DeviceRestore: Boolean; override;
    procedure DeviceRelease; override;

    property Context: TDX11DeviceContext read FContext;

    property Surface: TPixelMipMapSurface read FSurface;
    property Texture: ID3D11Texture2D read FTexture;
    property ResourceView: ID3D11ShaderResourceView read FResourceView;
  end;

  TDX11DrawableTexture = class(TCustomDrawableTexture)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX11DeviceContext;

    FTexture: ID3D11Texture2D;

    FResourceView: ID3D11ShaderResourceView;
    FRenderTargetView: ID3D11RenderTargetView;

    FDepthStencilTex: ID3D11Texture2D;
    FDepthStencilView: ID3D11DepthStencilView;

    FSavedTargetView: ID3D11RenderTargetView;
    FSavedStencilView: ID3D11DepthStencilView;
    FSavedViewport: D3D11_VIEWPORT;

    function CreateTargetTexture: Boolean;
    function CreateShaderResourceView: Boolean;
    function CreateRenderTargetView: Boolean;
    function CreateDepthStencil: Boolean;

    function CreateTextureInstance: Boolean;
    procedure DestroyTextureInstance;

    procedure PreserveRenderTargets;
    procedure RestoreRenderTargets;

    procedure UpdateViewport;
    procedure RestoreViewport;

    procedure UpdateMipMaps;
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

    property Context: TDX11DeviceContext read FContext;

    property Texture: ID3D11Texture2D read FTexture;
    property ResourceView: ID3D11ShaderResourceView read FResourceView;
    property RenderTargetView: ID3D11RenderTargetView read FRenderTargetView;

    property DepthStencilTex: ID3D11Texture2D read FDepthStencilTex;
    property DepthStencilView: ID3D11DepthStencilView read FDepthStencilView;
  end;

implementation

uses
  Windows, PXL.Windows.D3DCommon, PXL.Formats;

{$REGION 'TDX11LockableTexture'}

constructor TDX11LockableTexture.Create(const ADevice: TCustomDevice; const AutoSubscribe: Boolean);
begin
  inherited;

  FSurface := TPixelMipMapSurface.Create;
end;

destructor TDX11LockableTexture.Destroy;
begin
  inherited;

  FSurface.Free;
end;

function TDX11LockableTexture.CreateDynamicTexture: Boolean;
var
  Desc: D3D11_TEXTURE2D_DESC;
begin
  if (FContext = nil) or (FContext.Device = nil) then
    Exit(False);

  FillChar(Desc, SizeOf(D3D11_TEXTURE2D_DESC), 0);

  Desc.Width := FWidth;
  Desc.Height := FHeight;

  Desc.MipLevels := 1;
  Desc.ArraySize := 1;

  Desc.Format := TDX11DeviceContext.NativeToFormat(FPixelFormat);

  Desc.SampleDesc.Count := 1;
  Desc.SampleDesc.Quality := 0;

  Desc.Usage := D3D11_USAGE_DYNAMIC;
  Desc.BindFlags := Ord(D3D11_BIND_SHADER_RESOURCE);
  Desc.CPUAccessFlags := Ord(D3D11_CPU_ACCESS_WRITE);

  PushClearFPUState;
  try
    Result := Succeeded(FContext.Device.CreateTexture2D(Desc, nil, @FTexture));
  finally
    PopFPUState;
  end;
end;

function TDX11LockableTexture.CreateShaderResourceView: Boolean;
begin
  if (FContext = nil) or (FContext.Device = nil) or (FTexture = nil) then
    Exit(False);

  PushClearFPUState;
  try
    Result := Succeeded(FContext.Device.CreateShaderResourceView(FTexture, nil, @FResourceView));
  finally
    PopFPUState;
  end;
end;

function TDX11LockableTexture.CreateTextureInstance: Boolean;
begin
  { Non-dynamic textures are (re)created each time new data is uploaded. This assumes that the contents of texture is
    modified only once. }
  if FDynamicTexture then
  begin
    if not CreateDynamicTexture then
      Exit(False);

    if not CreateShaderResourceView then
    begin
      FTexture := nil;
      Exit(False);
    end;
  end;

  Result := True;
end;

procedure TDX11LockableTexture.DestroyTextureInstance;
begin
  FResourceView := nil;
  FTexture := nil;
end;

function TDX11LockableTexture.DoInitialize: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TDX11DeviceContext)) then
    Exit(False);

  FContext := TDX11DeviceContext(Device.Context);

  if FContext.Device = nil then
    Exit(False);

  if FPixelFormat = TPixelFormat.Unknown then
    FPixelFormat := TPixelFormat.A8R8G8B8;

  FPixelFormat := FContext.FindTextureFormat(FPixelFormat, FMipMapping);
  if FPixelFormat = TPixelFormat.Unknown then
    Exit(False);

  FBytesPerPixel := FPixelFormat.Bytes;

  if not FSurface.SetSize(FWidth, FHeight, FPixelFormat) then
    Exit(False);

  Result := CreateTextureInstance;
end;

procedure TDX11LockableTexture.DoFinalize;
begin
  DestroyTextureInstance;
  FContext := nil;
end;

function TDX11LockableTexture.UploadDynamicTexture: Boolean;
var
  Mapped: D3D11_MAPPED_SUBRESOURCE;
  I, BytesToCopy: Integer;
begin
  if (FContext = nil) or (FContext.Context = nil) or (FTexture = nil) or (FSurface = nil) or FSurface.IsEmpty then
    Exit(False);

  PushClearFPUState;
  try
    if Failed(FContext.Context.Map(FTexture, 0, D3D11_MAP_WRITE_DISCARD, 0, Mapped)) then
      Exit(False);
  finally
    PopFPUState;
  end;

  BytesToCopy := FSurface.Width * FSurface.BytesPerPixel;

  for I := 0 to FSurface.Height - 1 do
    Move(FSurface.Scanline[I]^, Pointer(PtrUInt(Mapped.Data) + Cardinal(I) * Mapped.RowPitch)^, BytesToCopy);

  PushClearFPUState;
  try
    FContext.Context.Unmap(FTexture, 0);
  finally
    PopFPUState;
  end;

  Result := True;
end;

function TDX11LockableTexture.CreateDefaultTexture: Boolean;
var
  Desc: D3D11_TEXTURE2D_DESC;
  SubResData: array of D3D11_SUBRESOURCE_DATA;
  I: Integer;
begin
  if (FContext = nil) or (FContext.Device = nil) then
    Exit(False);

  if FMipMapping then
    FSurface.GenerateMipMaps
  else
    FSurface.ClearMipMaps;

  FillChar(Desc, SizeOf(D3D11_TEXTURE2D_DESC), 0);

  Desc.Width := FWidth;
  Desc.Height := FHeight;

  Desc.MipLevels := 1 + FSurface.MipMaps.Count;
  Desc.ArraySize := 1;

  Desc.Format := TDX11DeviceContext.NativeToFormat(FPixelFormat);

  Desc.SampleDesc.Count := 1;
  Desc.SampleDesc.Quality := 0;

  Desc.Usage := D3D11_USAGE_DEFAULT;
  Desc.BindFlags := Ord(D3D11_BIND_SHADER_RESOURCE);

  SetLength(SubResData, 1 + FSurface.MipMaps.Count);

  for I := 0 to Length(SubResData) - 1 do
  begin
    SubResData[I].SysMemSlicePitch := 0;

    if I = 0 then
    begin
      SubResData[I].SysMem := FSurface.Bits;
      SubResData[I].SysMemPitch := FSurface.Pitch;
    end
    else
    begin
      SubResData[I].SysMem := FSurface.MipMaps[I - 1].Bits;
      SubResData[I].SysMemPitch := FSurface.MipMaps[I - 1].Pitch;
    end;
  end;

  PushClearFPUState;
  try
    Result := Succeeded(FContext.Device.CreateTexture2D(Desc, @SubResData[0], @FTexture));
  finally
    PopFPUState;
  end;
end;

function TDX11LockableTexture.UploadTexture: Boolean;
begin
  if not FDynamicTexture then
  begin
    FResourceView := nil;
    FTexture := nil;

    if not CreateDefaultTexture then
      Exit(False);

    if not CreateShaderResourceView then
    begin
      FTexture := nil;
      Exit(False);
    end;

    Result := True;
  end
  else
    Result := UploadDynamicTexture;
end;

function TDX11LockableTexture.DoLock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean;
begin
  Result := LockSurface(FSurface, Rect, LockedPixels);
end;

function TDX11LockableTexture.DoUnlock: Boolean;
begin
  Result := UploadTexture;
end;

function TDX11LockableTexture.Bind(const Channel: Integer): Boolean;
begin
  if (FContext = nil) or (FContext.Context = nil) or (FResourceView = nil) then
    Exit(False);

  PushClearFPUState;
  try
    FContext.Context.PSSetShaderResources(Channel, 1, @FResourceView);

  finally
    PopFPUState;
  end;

  Result := True;
end;

function TDX11LockableTexture.CreateStagingTexture(const Size: TPoint2px; out Texture: ID3D11Texture2D): Boolean;
var
  Desc: D3D11_TEXTURE2D_DESC;
begin
  if (FContext = nil) or (FContext.Device = nil) then
    Exit(False);

  FillChar(Desc, SizeOf(D3D11_TEXTURE2D_DESC), 0);

  Desc.Width := Size.X;
  Desc.Height := Size.Y;

  Desc.MipLevels := 1;
  Desc.ArraySize := 1;

  Desc.Format := TDX11DeviceContext.NativeToFormat(FPixelFormat);

  Desc.SampleDesc.Count := 1;
  Desc.SampleDesc.Quality := 0;

  Desc.Usage := D3D11_USAGE_STAGING;
  Desc.CPUAccessFlags := Ord(D3D11_CPU_ACCESS_READ);

  PushClearFPUState;
  try
    Result := Succeeded(FContext.Device.CreateTexture2D(Desc, nil, @Texture));
  finally
    PopFPUState;
  end;
end;

function TDX11LockableTexture.DownloadStagingTexture(const DestPos, Size: TPoint2px;
  const Texture: ID3D11Texture2D): Boolean;
var
  Mapped: D3D11_MAPPED_SUBRESOURCE;
  Pixels: Pointer;
  I, BytesToCopy: Integer;
begin
  if (FContext = nil) or (FContext.Context = nil) or (Texture = nil) or (FSurface = nil) or FSurface.IsEmpty then
    Exit(False);

  PushClearFPUState;
  try
    if Failed(FContext.Context.Map(Texture, 0, D3D11_MAP_READ, 0, Mapped)) then
      Exit(False);
  finally
    PopFPUState;
  end;

  BytesToCopy := Size.X * FSurface.BytesPerPixel;

  for I := 0 to Size.Y - 1 do
  begin
    Pixels := Pointer(PtrUInt(Mapped.Data) + Cardinal(I) * Mapped.RowPitch);
    Move(Pixels^, FSurface.PixelPtr[DestPos.X, DestPos.Y + I]^, BytesToCopy);
  end;

  PushClearFPUState;
  try
    FContext.Context.Unmap(Texture, 0);
    Result := True;
  finally
    PopFPUState;
  end;
end;

function TDX11LockableTexture.DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
  const DestPos: TPoint2px): Boolean;
var
  StagingTexture: ID3D11Texture2D;
  StagingBox: D3D11_BOX;
begin
  if Source is TDX11DrawableTexture then
  begin
    if not CreateStagingTexture(SourceRect.Size, StagingTexture) then
      Exit(False);

    if (Size <> Source.Size) or (DestPos <> ZeroPoint2px) or (SourceRect.TopLeft <> ZeroPoint2px) or
      (SourceRect.Size <> Source.Size) then
    begin
      StagingBox.Left := SourceRect.Left;
      StagingBox.Top := SourceRect.Top;
      StagingBox.Right := SourceRect.Right;
      StagingBox.Bottom := SourceRect.Bottom;
      StagingBox.Front := 0;
      StagingBox.Back := 1;

      FContext.Context.CopySubresourceRegion(StagingTexture, 0, 0, 0, 0, TDX11DrawableTexture(Source).FTexture, 0,
        @StagingBox);
    end
    else
      FContext.Context.CopyResource(StagingTexture, TDX11DrawableTexture(Source).FTexture);

    Result := DownloadStagingTexture(DestPos, SourceRect.Size, StagingTexture) and UploadTexture;
  end
  else
    Result := inherited;
end;

function TDX11LockableTexture.DeviceRestore: Boolean;
begin
  if FState = TTextureState.Lost then
  begin
    if not CreateTextureInstance then
    begin
      FState := TTextureState.NotRecovered;
      Exit(False);
    end;

    if not UploadTexture then
    begin
      DestroyTextureInstance;
      FState := TTextureState.NotRecovered;
      Exit(False);
    end;

    FState := TTextureState.Initialized;
    Result := True;
  end
  else
    Result := False;
end;

procedure TDX11LockableTexture.DeviceRelease;
begin
  if FState = TTextureState.Initialized then
  begin
    DestroyTextureInstance;
    FState := TTextureState.Lost;
  end;
end;

{$ENDREGION}
{$REGION 'TDX11DrawableTexture'}

function TDX11DrawableTexture.CreateTargetTexture: Boolean;
var
  Desc: D3D11_TEXTURE2D_DESC;
  SampleCount, QualityLevel: Integer;
begin
  if (FContext = nil) or (FContext.Device = nil) then
    Exit(False);

  FillChar(Desc, SizeOf(D3D11_TEXTURE2D_DESC), 0);

  Desc.Width := Width;
  Desc.Height := Height;

  Desc.MipLevels := 1;
  if FMipMapping then
    Desc.MipLevels := 0;

  Desc.ArraySize := 1;

  Desc.Format := TDX11DeviceContext.NativeToFormat(FPixelFormat);

  Desc.SampleDesc.Count := 1;
  Desc.SampleDesc.Quality := 0;

  if (not FMipMapping) and (FMultisamples > 1) then
  begin
    FContext.FindBestMultisampleType(Desc.Format, FMultisamples, SampleCount, QualityLevel);

    Desc.SampleDesc.Count := SampleCount;
    Desc.SampleDesc.Quality := QualityLevel;

    FMultisamples := SampleCount;
  end;

  Desc.Usage := D3D11_USAGE_DEFAULT;
  Desc.BindFlags := Ord(D3D11_BIND_SHADER_RESOURCE) or Ord(D3D11_BIND_RENDER_TARGET);

  if FMipMapping then
    Desc.MiscFlags := Ord(D3D11_RESOURCE_MISC_GENERATE_MIPS);

  PushClearFPUState;
  try
    Result := Succeeded(FContext.Device.CreateTexture2D(Desc, nil, @FTexture));
  finally
    PopFPUState;
  end;
end;

function TDX11DrawableTexture.CreateShaderResourceView: Boolean;
begin
  if (FContext = nil) or (FContext.Device = nil) then
    Exit(False);

  PushClearFPUState;
  try
    Result := Succeeded(FContext.Device.CreateShaderResourceView(FTexture, nil, @FResourceView));
  finally
    PopFPUState;
  end;
end;

function TDX11DrawableTexture.CreateRenderTargetView: Boolean;
begin
  if (FContext = nil) or (FContext.Device = nil) then
    Exit(False);

  PushClearFPUState;
  try
    Result := Succeeded(FContext.Device.CreateRenderTargetView(FTexture, nil, @FRenderTargetView));
  finally
    PopFPUState;
  end;
end;

function TDX11DrawableTexture.CreateDepthStencil: Boolean;
var
  Format: DXGI_FORMAT;
  TextureDesc, DepthStencilDesc: D3D11_TEXTURE2D_DESC;
begin
  if FDepthStencil <= TDepthStencil.None then
    Exit(True);

  if (FContext = nil) or (FContext.Device = nil) then
    Exit(False);

  // Find suitable depth-stencil format.
  Format := FContext.FindDepthStencilFormat(FDepthStencil);
  if Format = DXGI_FORMAT_UNKNOWN then
    Exit(False);

  // Retrieve description of texture surface.
  FillChar(TextureDesc, SizeOf(D3D11_TEXTURE2D_DESC), 0);

  PushClearFPUState;
  try
    FTexture.GetDesc(TextureDesc);
  finally
    PopFPUState;
  end;

  // Prepare the description for depth-stencil surface.
  FillChar(DepthStencilDesc, SizeOf(D3D11_TEXTURE2D_DESC), 0);

  DepthStencilDesc.Format := Format;
  DepthStencilDesc.Width := FWidth;
  DepthStencilDesc.Height := FHeight;

  DepthStencilDesc.MipLevels := TextureDesc.MipLevels;
  DepthStencilDesc.ArraySize := 1;

  DepthStencilDesc.SampleDesc.Count := TextureDesc.SampleDesc.Count;
  DepthStencilDesc.SampleDesc.Quality := TextureDesc.SampleDesc.Quality;

  DepthStencilDesc.Usage := D3D11_USAGE_DEFAULT;
  DepthStencilDesc.BindFlags := Ord(D3D11_BIND_DEPTH_STENCIL);

  // Create depth-stencil texture.
  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateTexture2D(DepthStencilDesc, nil, @FDepthStencilTex)) then
      Exit(False);
  finally
    PopFPUState;
  end;

  // Create a depth-stencil view.
  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateDepthStencilView(FDepthStencilTex, nil, @FDepthStencilView)) then
    begin
      FDepthStencilTex := nil;
      Exit(False);
    end;
  finally
    PopFPUState;
  end;

  Result := True;
end;

function TDX11DrawableTexture.CreateTextureInstance: Boolean;
begin
  if not CreateTargetTexture then
    Exit(False);

  if not CreateShaderResourceView then
  begin
    FTexture := nil;
    Exit(False);
  end;

  if not CreateRenderTargetView then
  begin
    FResourceView := nil;
    FTexture := nil;
    Exit(False);
  end;

  if not CreateDepthStencil then
  begin
    FRenderTargetView := nil;
    FResourceView := nil;
    FTexture := nil;
    Exit(False);
  end;

  Result := True;
end;

procedure TDX11DrawableTexture.DestroyTextureInstance;
begin
  FDepthStencilView := nil;
  FDepthStencilTex := nil;
  FRenderTargetView := nil;
  FResourceView := nil;
  FTexture := nil;
end;

function TDX11DrawableTexture.DoInitialize: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TDX11DeviceContext)) then
    Exit(False);

  FContext := TDX11DeviceContext(Device.Context);

  if FPixelFormat = TPixelFormat.Unknown then
    FPixelFormat := TPixelFormat.A8R8G8B8;

  Result := CreateTextureInstance;
end;

procedure TDX11DrawableTexture.DoFinalize;
begin
  DestroyTextureInstance;
  FContext := nil;
end;

function TDX11DrawableTexture.Bind(const Channel: Integer): Boolean;
begin
  if (FContext = nil) or (FContext.Context = nil) or (FResourceView = nil) then
    Exit(False);

  PushClearFPUState;
  try
    FContext.Context.PSSetShaderResources(Channel, 1, @FResourceView);
  finally
    PopFPUState;
  end;

  Result := True;
end;

function TDX11DrawableTexture.DeviceRestore: Boolean;
begin
  if FState = TTextureState.Lost then
  begin
    if not CreateTextureInstance then
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

procedure TDX11DrawableTexture.DeviceRelease;
begin
  if FState = TTextureState.Initialized then
  begin
    DestroyTextureInstance;
    FState := TTextureState.Lost;
  end;
end;

procedure TDX11DrawableTexture.PreserveRenderTargets;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit;

  PushClearFPUState;
  try
    FContext.Context.OMGetRenderTargets(1, @FSavedTargetView, @FSavedStencilView);
  finally
    PopFPUState;
  end;
end;

procedure TDX11DrawableTexture.RestoreRenderTargets;
begin
  if (FContext <> nil) and (FContext.Context <> nil) and (FSavedTargetView <> nil) then
  begin
    PushClearFPUState;
    try
      FContext.Context.OMSetRenderTargets(1, @FSavedTargetView, FSavedStencilView);
    finally
      PopFPUState;
    end;
  end;

  FSavedStencilView := nil;
  FSavedTargetView := nil;
end;

procedure TDX11DrawableTexture.UpdateViewport;
var
  NumViewports: LongWord;
  Viewport: D3D11_VIEWPORT;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit;

  FillChar(Viewport, SizeOf(D3D11_VIEWPORT), 0);
  FillChar(FSavedViewport, SizeOf(D3D11_VIEWPORT), 0);

  Viewport.Width := FWidth;
  Viewport.Height := FHeight;
  Viewport.MinDepth := 0.0;
  Viewport.MaxDepth := 1.0;

  NumViewports := 1;

  PushClearFPUState;
  try
    FContext.Context.RSGetViewports(NumViewports, @FSavedViewport);
    FContext.Context.RSSetViewports(1, @Viewport);
  finally
    PopFPUState;
  end;
end;

procedure TDX11DrawableTexture.RestoreViewport;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit;

  PushClearFPUState;
  try
    FContext.Context.RSSetViewports(1, @FSavedViewport);
  finally
    PopFPUState;
  end;

  FillChar(FSavedViewport, SizeOf(D3D11_VIEWPORT), 0);
end;

procedure TDX11DrawableTexture.UpdateMipMaps;
begin
  if (FContext = nil) or (FContext.Context = nil) or (FResourceView = nil) then
    Exit;

  PushClearFPUState;
  try
    FContext.Context.GenerateMips(FResourceView);
  finally
    PopFPUState;
  end;
end;

function TDX11DrawableTexture.BeginDraw: Boolean;
begin
  if (FContext = nil) or (FContext.Context = nil) or (FRenderTargetView = nil) then
    Exit(False);

  PreserveRenderTargets;

  PushClearFPUState;
  try
    FContext.Context.OMSetRenderTargets(1, @FRenderTargetView, FDepthStencilView);
  finally
    PopFPUState;
  end;

  UpdateViewport;
  Result := True;
end;

procedure TDX11DrawableTexture.EndDraw;
begin
  RestoreRenderTargets;
  RestoreViewport;

  if FMipMapping then
    UpdateMipMaps;
end;

function TDX11DrawableTexture.Clear: Boolean;
var
  ClearFlags: Cardinal;
begin
  if (FContext = nil) or (FContext.Context = nil) or (FRenderTargetView = nil) then
    Exit(False);

  FContext.Context.ClearRenderTargetView(FRenderTargetView, FourSingleArray(0.0, 0.0, 0.0, 0.0));

  if FDepthStencilView <> nil then
  begin
    ClearFlags := Cardinal(Ord(D3D11_CLEAR_DEPTH));

    if FDepthStencil >= TDepthStencil.Full then
      ClearFlags := ClearFlags or Cardinal(Ord(D3D11_CLEAR_STENCIL));

    FContext.Context.ClearDepthStencilView(FDepthStencilView, ClearFlags, 1.0, 0);
  end;

  Result := True;
end;

function TDX11DrawableTexture.DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
  const DestPos: TPoint2px): Boolean;
var
  SourceRes: ID3D11Resource;
  SourceBox: D3D11_BOX;
begin
  if (Device <> nil) and (Source.Device = Device) and (FContext <> nil) and (FContext.Context <> nil) and
    (FTexture <> nil) and (Source.PixelFormat = FPixelFormat) then
  begin
    if Source is TDX11LockableTexture then
      SourceRes := TDX11LockableTexture(Source).FTexture
    else if Source is TDX11DrawableTexture then
      SourceRes := TDX11DrawableTexture(Source).FTexture
    else
      SourceRes := nil;

    if SourceRes <> nil then
    begin
      if (Size <> Source.Size) or (DestPos <> ZeroPoint2px) or (SourceRect.TopLeft <> ZeroPoint2px) or
        (SourceRect.Size <> Source.Size) then
      begin
        SourceBox.Left := SourceRect.Left;
        SourceBox.Top := SourceRect.Top;
        SourceBox.Right := SourceRect.Right;
        SourceBox.Bottom := SourceRect.Bottom;
        SourceBox.Front := 0;
        SourceBox.Back := 1;

        FContext.Context.CopySubresourceRegion(FTexture, 0, DestPos.X, DestPos.Y, 0, SourceRes, 0, @SourceBox);
      end
      else
        FContext.Context.CopyResource(FTexture, SourceRes);

      if FMipMapping then
        UpdateMipMaps;

      Result := True;
    end
    else
      Result := inherited;
  end
  else
    Result := inherited;
end;

{$ENDREGION}

end.
