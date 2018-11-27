unit PXL.Textures;
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
{< Abstract texture specification with basic implementation that is common across different providers and platforms. }
interface

{$INCLUDE PXL.Config.inc}

uses
  PXL.TypeDef, PXL.Types, PXL.Surfaces, PXL.Devices;

type
  { Pointer to @link(TLockedPixels). }
  PLockedPixels = ^TLockedPixels;

  { This structure stores information about locked texture's portion that can be accessed by CPU. It is only valid
    while that region remains locked. }
  TLockedPixels = record
  public
    { Reference to top/left memory location (arranged in series of horizontal rows) that can be accessed. }
    Bits: Pointer;

    { The number of bytes each horizontal row of pixels occupies. This may differ than the actual calculated number and
      may include unused or even protected memory locations, which should simply be skipped.}
    Pitch: Integer;

    { Number of bytes each pixel occupies. }
    BytesPerPixel: Integer;

    { Pixel format that each pixel is stored in. }
    PixelFormat: TPixelFormat;

    { Rectangle that represents the area that was locked. }
    LockedRect: TIntRect;
  private
    function GetScanline(const Index: Integer): Pointer; inline;
    function GetPixelPtr(const X, Y: Integer): Pointer; inline;

    function GetPixel(const X, Y: Integer): TIntColor;
    procedure SetPixel(const X, Y: Integer; const Value: TIntColor);

    function GetValid: Boolean;
  public
    { Resets all values of the structure to zero. }
    procedure Reset;

    { Reference to each individual scanline in the locked region. }
    property Scanline[const Index: Integer]: Pointer read GetScanline;

    { Reference to each individual pixel in the locked region. }
    property PixelPtr[const X, Y: Integer]: Pointer read GetPixelPtr;

    { Provides access along with the appropriate pixel-format conversion to each of the pixels in the locked region. }
    property Pixels[const X, Y: Integer]: TIntColor read GetPixel write SetPixel;

    { Provides a sanity check on structure's values to make sure it remains valid. }
    property Valid: Boolean read GetValid;
  end;

  { Current state of texture. }
  TTextureState = (
    { The texture has not been initialized yet. }
    NotInitialized,

    { The texture has been initialized and is working fine. }
    Initialized,

    { The surface of the texture has been lost and cannot be used right now. It is possible that the application has
      been minimized and/or paused (depending on platform). }
    Lost,

    { The surface of the texture has been lost and could not be recovered. If the texture is found in this state, it
      should be released and, if needed, re-created. }
    NotRecovered);

  { Base definition of hardware-assisted texture. Each texture is typically bound to one specific device, which
    contains any hardware-specific context information. }
  TCustomBaseTexture = class abstract
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FDevice: TCustomDevice;

    DeviceRestoreHandle: Cardinal;
    DeviceReleaseHandle: Cardinal;

    procedure SetPixelFormat(const Value: TPixelFormat);

    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);

    function GetSize: TPoint2px;
    procedure SetSize(const Value: TPoint2px);

    procedure SetMipMapping(const Value: Boolean);
    procedure SetPremultipliedAlpha(const Value: Boolean);

    procedure OnDeviceRestore(const Sender: TObject; const EventData, UserData: Pointer);
    procedure OnDeviceRelease(const Sender: TObject; const EventData, UserData: Pointer);
  protected
    { Current state the texture is in. }
    FState: TTextureState;

    { Current format that texture pixels are represented in. }
    FPixelFormat: TPixelFormat;

    { Current texture width. }
    FWidth: Integer;

    { Current texture height. }
    FHeight: Integer;

    { Whether the current texture uses mipmapping or not. }
    FMipMapping: Boolean;

    { Whether the current texture has RGB elements premultiplied by alpha-channel or not. }
    FPremultipliedAlpha: Boolean;

    { This method is called by @link(Initialize) and should be implemented by derived classes to create necessary
      hardware elements, and initialize the texture. }
    function DoInitialize: Boolean; virtual;

    { This method is called by @link(Finalize) and should be implemented by derived classes to release any remaining
      hardware elements, and finalize the texture.}
    procedure DoFinalize; virtual;

    { This method is called by @link(CopyRect) and should provide copy mechanism between this and other textures
      available on given provider. }
    function DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
      const DestPos: TPoint2px): Boolean; virtual;
  public
    { Creates new instance of texture bound to the specific device. }
    constructor Create(const ADevice: TCustomDevice; const AutoSubscribe: Boolean); virtual;
    { @exclude } destructor Destroy; override;

    { Initializes the texture with its currently set parameters and prepares for usage in rendering. }
    function Initialize: Boolean;

    { Finalizes the texture and releases its resources that were used for rendering. }
    procedure Finalize;

    { Copies a portion of source texture area to this one. This function handles the clipping and possibly pixel
      format conversion. @True is returned when the operation succeeds and @False otherwise. }
    function CopyRect(DestPos: TPoint2px; const Source: TCustomBaseTexture; SourceRect: TIntRect): Boolean;

    { Copies the entire source texture area to this one. If the sizes of both textures do not match, then intersection
      of both will be copied. This function handles the clipping and possibly pixel format conversion. @True is
      returned when the operation succeeds and @False otherwise. }
    function CopyFrom(const Source: TCustomBaseTexture): Boolean; inline;

    { Binds this texture to the specified hardware channel. The actual meaning and functionality of this method varies
      on different types of hardware and platforms. }
    function Bind(const Channel: Integer): Boolean; virtual;

    { Clears the entire texture and fills pixels with zeros. }
    function Clear: Boolean; virtual;

    { Restores the texture after its surface has been lost (that is, after @link(DeviceRelease) call). This should be
      implemented by derived classes to handle "device lost" scenario. }
    function DeviceRestore: Boolean; virtual;

    { Releases the texture's surface when the device has been lost. This should be implemented by derived classes to
      handle "device lost" scenario. }
    procedure DeviceRelease; virtual;

    { Reference to the device class to which this texture is bound to. }
    property Device: TCustomDevice read FDevice;

    { Indicates the state in which the texture currently is. }
    property State: TTextureState read FState;

    { Determines the pixel format in which to store texture's pixels. This can be written to only before the texture is
      initialized, but can be read at any time. }
    property PixelFormat: TPixelFormat read FPixelFormat write SetPixelFormat;

    { Determines the texture width in pixels. This can be written to only before the texture is initialized, but can
      be read at any time. }
    property Width: Integer read FWidth write SetWidth;

    { Determines the texture height in pixels. This can be written to only before the texture is initialized, but can
      be read at any time. }
    property Height: Integer read FHeight write SetHeight;

    { Determines the texture size in pixels. This can be written to only before the texture is initialized, but can
      be read at any time. }
    property Size: TPoint2px read GetSize write SetSize;

    { Determines whether the texture uses mipmapping or not. Mipmapping can improve visual quality when the texture is
      drawn in different sizes, especially in smaller ones. This can be written to only before the texture is
      initialized, but can be read at any time. }
    property MipMapping: Boolean read FMipMapping write SetMipMapping;

    { Determines whether the texture has RGB components premultiplied by alpha-channel or not. Premultiplied alpha
      implies permanent loss of information as the components are multiplied by alpha value and stored (so, for
      example, pixels with alpha value of zero permanently lose all color information), however this can improve visual
      quality on mipmaps with translucent pixels. This parameter is merely a hint for rendering system, it does not
      change the actual pixels - this is something that should be done as a separate step. This parameter can only be
      changed before the texture is initialized, but can be read at any time. }
    property PremultipliedAlpha: Boolean read FPremultipliedAlpha write SetPremultipliedAlpha;
  end;

  { Base definition of "lockable" texture; that is, a texture that can have regions "locked" so they become accessible
    to CPU. These textures are typical to most GPUs and also provide efficient means of storing dynamically changing
    data that is provided by CPU. }
  TCustomLockableTexture = class abstract(TCustomBaseTexture)
  private type
    TLockedPixelsSurfaceWrapper = class(TPixelSurface)
    private
      FLockedPixels: TLockedPixels;
      FLockedTexture: TCustomLockableTexture;
    protected
      procedure ResetAllocation; override;
      function Reallocate(const NewWidth, NewHeight: Integer; const NewPixelFormat: TPixelFormat): Boolean; override;
    public
      constructor Create(const ALockedTexture: TCustomLockableTexture;
        const ALockedPixels: TLockedPixels); reintroduce;
      destructor Destroy; override;
      function ApproximatePixelFormat(const NewPixelFormat: TPixelFormat): TPixelFormat; override;
    end;
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FLockSurface: TLockedPixelsSurfaceWrapper;
    FLocked: Boolean;

    function IsLockRectInvalid(const Rect: TIntRect): Boolean;
    procedure SetDynamicTexture(const Value: Boolean);
  protected
    { Current number of bytes each pixel occupies. }
    FBytesPerPixel: Integer;

    { Determines whether the current texture is dynamic (that is, can have content changed intensively) or not. }
    FDynamicTexture: Boolean;

    { Returns @True when the specified rectangle covers the entire texture and @False otherwise. On some occasions,
      this can be useful to determine whether to pass rectangle pointer to locking mechanism or just pass @nil to
      cover the entire area. }
    function IsLockRectFull(const Rect: TIntRect): Boolean;

    { Helper function that provides locking mechanism to a simple @link(TPixelSurface). That is, a quick shortcut to
      "lock" non-GPU surface. Returns @True when the operation is successful and @False otherwise. }
    function LockSurface(const Surface: TPixelSurface; const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean;

    { Locks the specified rectangle and provides access information regarding this region. This should be implemented
      by derived classes. Returns @True when the operation is successful and @False otherwise.}
    function DoLock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean; virtual; abstract;

    { Unlocks the specified rectangle and makes sure the texture remains updated after these changes. This should be
      implemented by derived classes. Returns @True when the operation is successful and @False otherwise. }
    function DoUnlock: Boolean; virtual; abstract;

    { This class implements internally limited functionality of "DoCopyRect" by locking both textures and copying data
      on CPU. For typical applications this could be okay, but it is generally inefficient and should be re-implemented
      by derived classes to provide copy mechanism directly on the GPU. }
    function DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
      const DestPos: TPoint2px): Boolean; override;
  public
    { Locks a portion of texture so that it can be accessed by CPU and provides information in @link(TLockedPixels)
      structure regarding this area. }
    function Lock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean; overload;

    { Locks the entire texture so that it can be accessed by CPU and provides information in @link(TLockedPixels)
      structure regarding this area. }
    function Lock(out LockedPixels: TLockedPixels): Boolean; overload; inline;

    { Locks a portion of texture so that it can be accessed by CPU and provides @link(TPixelSurface) wrapper around
      this area for easy access. The texture can be unlocked either by calling @link(Unlock) or by freeing the surface
      returned by this function. Calling @link(Unlock) also releases the surface returned by this call. }
    function Lock(const Rect: TIntRect; out Surface: TPixelSurface): Boolean; overload;

    { Locks the entire texture so that it can be accessed by CPU and provides @link(TPixelSurface) wrapper around
      this area for easy access. The texture can be unlocked either by calling @link(Unlock) or by freeing the surface
      returned by this function. Calling @link(Unlock) also releases the surface returned by this call. }
    function Lock(out Surface: TPixelSurface): Boolean; overload; inline;

    { Unlocks the texture and updates its contents. }
    function Unlock: Boolean;

    { Clears the texture and fills its pixels with zeros. }
    function Clear: Boolean; override;

    { Copies a portion from source surface to this texture. This method does clipping when applicable and calls
      @link(Lock) / @link(Unlock) method pair appropriately during the process. }
    function CopyFromSurfaceRect(DestPos: TPoint2px; const Source: TPixelSurface; SourceRect: TIntRect): Boolean;

    { Copies an entire source surface to this texture. This method does clipping when applicable and calls
      @link(Lock) / @link(Unlock) method pair appropriately during the process. }
    function CopyFromSurface(const Source: TPixelSurface): Boolean;

    { Copies a region of this texture to the specified destination surface. This method does clipping when
      applicable and calls @link(Lock) / @link(Unlock) method pair appropriately during the process.}
    function CopyToSurfaceRect(DestPos: TPoint2px; const Dest: TPixelSurface; SourceRect: TIntRect): Boolean;

    { Copies the entire contents of this texture to the specified destination surface. This method does clipping when
      applicable and calls @link(Lock) / @link(Unlock) method pair appropriately during the process.}
    function CopyToSurface(const Dest: TPixelSurface): Boolean;

    { Number of bytes each pixel in the texture occupies. }
    property BytesPerPixel: Integer read FBytesPerPixel;

    { Determines whether the texture is "dynamic"; that is, can have content changing continuously without major
      impact on rendering performance. This can provide significant performance benefit but may or may not be
      supported on specific provider and platform. }
    property DynamicTexture: Boolean read FDynamicTexture write SetDynamicTexture;
  end;

  { Base definition of "drawable" texture; that is, a texture that can be drawn to. Typically, this means that the
    texture is a @italic(render target) or @italic(render buffer). }
  TCustomDrawableTexture = class abstract(TCustomBaseTexture)
  private
    procedure SetDepthStencil(const Value: TDepthStencil);
    procedure SetMultisamples(const Value: Integer);
  protected
    { Currently set level of depth/stencil support. }
    FDepthStencil: TDepthStencil;

    { The number of multisamples that is currently selected. }
    FMultisamples: Integer;
  public
    { Activates this texture as destination rendering surface, after which normal rendering calls would draw directly
      on this texture's surface. This should be implemented by derived classes to implement the appropriate
      functionality depending on provider and platform. }
    function BeginDraw: Boolean; virtual; abstract;

    { Deactivates this texture as destination rendering surface and updates its contents to reflect what has been drawn.
      This should be implemented by derived classes to implement the appropriate functionality depending on provider
      and platform. }
    procedure EndDraw; virtual; abstract;

    { The required level of depth/stencil support that should be provided. This can be written to only before the texture is
      initialized, but can be read at any time.}
    property DepthStencil: TDepthStencil read FDepthStencil write SetDepthStencil;

    { The required number of multisamples that should be used for rendering. This is merely a hint and the actual number
      of multisamples that is being used can be overwritten when the texture is initialized. This property can be
      written to only before the texture is initialized, but can be read at any time.}
    property Multisamples: Integer read FMultisamples write SetMultisamples;
  end;

const
  { Special constant that defines @link(TLockedPixels) that have all fields set to zero, meaning that the data is no
    longer valid. }
  InvalidLockedPixels: TLockedPixels = (Bits: nil; Pitch: 0; BytesPerPixel: 0; PixelFormat: TPixelFormat.Unknown;
    LockedRect: (Left: 0; Top: 0; Right: 0; Bottom: 0));

implementation

uses
  SysUtils, PXL.Consts, PXL.Logs, PXL.Formats;

{$REGION 'TLockedPixels'}

function TLockedPixels.GetScanline(const Index: Integer): Pointer;
begin
  Result := Pointer(PtrInt(Bits) + Index * Pitch);
end;

function TLockedPixels.GetPixelPtr(const X, Y: Integer): Pointer;
begin
  Result := Pointer(PtrInt(Bits) + Y * Pitch + X * BytesPerPixel);
end;

function TLockedPixels.GetValid: Boolean;
begin
  Result := (Bits <> nil) and (Pitch > 0) and (BytesPerPixel > 0) and (PixelFormat <> TPixelFormat.Unknown) and
    (not LockedRect.Empty);
end;

function TLockedPixels.GetPixel(const X, Y: Integer): TIntColor;
begin
  Result := PixelXto32(GetPixelPtr(X, Y), PixelFormat);
end;

procedure TLockedPixels.SetPixel(const X, Y: Integer; const Value: TIntColor);
begin
  Pixel32toX(Value, GetPixelPtr(X, Y), PixelFormat);
end;

procedure TLockedPixels.Reset;
begin
  FillChar(Self, SizeOf(TLockedPixels), 0);
end;

{$ENDREGION}
{$REGION 'TCustomBaseTexture'}

constructor TCustomBaseTexture.Create(const ADevice: TCustomDevice; const AutoSubscribe: Boolean);
begin
  inherited Create;

  FDevice := ADevice;

  try
    if AutoSubscribe and (FDevice <> nil) then
    begin
      if FDevice.OnRestore <> nil then
        DeviceRestoreHandle := FDevice.OnRestore.Subscribe(OnDeviceRestore);

      if FDevice.OnRelease <> nil then
        DeviceReleaseHandle := FDevice.OnRelease.Subscribe(OnDeviceRelease);
    end;
  finally
    Increment_PXL_ClassInstances;
  end;
end;

destructor TCustomBaseTexture.Destroy;
begin
  try
    if FState <> TTextureState.NotInitialized then
      Finalize;

    if (DeviceReleaseHandle <> 0) and (FDevice <> nil) and (FDevice.OnRelease <> nil) then
      FDevice.OnRelease.Unsubscribe(DeviceReleaseHandle);

    if (DeviceRestoreHandle <> 0) and (FDevice <> nil) and (FDevice.OnRestore <> nil) then
      FDevice.OnRestore.Unsubscribe(DeviceRestoreHandle);

    FDevice := nil;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

procedure TCustomBaseTexture.SetPixelFormat(const Value: TPixelFormat);
begin
  if FState = TTextureState.NotInitialized then
    FPixelFormat := Value;
end;

procedure TCustomBaseTexture.SetWidth(const Value: Integer);
begin
  if FState = TTextureState.NotInitialized then
    FWidth := Value;
end;

procedure TCustomBaseTexture.SetHeight(const Value: Integer);
begin
  if FState = TTextureState.NotInitialized then
    FHeight := Value;
end;

function TCustomBaseTexture.GetSize: TPoint2px;
begin
  Result := Point2px(FWidth, FHeight);
end;

procedure TCustomBaseTexture.SetSize(const Value: TPoint2px);
begin
  if FState = TTextureState.NotInitialized then
  begin
    FWidth := Value.X;
    FHeight := Value.Y;
  end;
end;

procedure TCustomBaseTexture.SetMipMapping(const Value: Boolean);
begin
  if FState = TTextureState.NotInitialized then
    FMipMapping := Value;
end;

procedure TCustomBaseTexture.SetPremultipliedAlpha(const Value: Boolean);
begin
  if FState = TTextureState.NotInitialized then
    FPremultipliedAlpha := Value;
end;

function TCustomBaseTexture.DoInitialize: Boolean;
begin
  Result := True;
end;

procedure TCustomBaseTexture.DoFinalize;
begin
end;

function TCustomBaseTexture.Initialize: Boolean;
begin
  if FState <> TTextureState.NotInitialized then
    Exit(False);

  Result := DoInitialize;

  if Result then
    FState := TTextureState.Initialized;
end;

procedure TCustomBaseTexture.Finalize;
begin
  if FState <> TTextureState.NotInitialized then
  begin
    DoFinalize;
    FState := TTextureState.NotInitialized;
  end;
end;

function TCustomBaseTexture.DeviceRestore: Boolean;
begin
  Result := True;
end;

procedure TCustomBaseTexture.DeviceRelease;
begin
end;

function TCustomBaseTexture.DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
  const DestPos: TPoint2px): Boolean;
begin
  Result := False;
end;

function TCustomBaseTexture.CopyRect(DestPos: TPoint2px; const Source: TCustomBaseTexture;
  SourceRect: TIntRect): Boolean;
begin
  if (FState <> TTextureState.Initialized) or (Source = nil) or (Source.State <> TTextureState.Initialized) then
    Exit(False);

  if SourceRect.Empty then
    SourceRect := IntRect(ZeroPoint2px, Source.Size);

  if not ClipCoords(Source.Size, GetSize, SourceRect, DestPos) then
    Exit(False);

  Result := DoCopyRect(Source, SourceRect, DestPos);
end;

function TCustomBaseTexture.CopyFrom(const Source: TCustomBaseTexture): Boolean;
begin
  Result := CopyRect(ZeroPoint2px, Source, ZeroIntRect);
end;

function TCustomBaseTexture.Bind(const Channel: Integer): Boolean;
begin
  Result := False;
end;

function TCustomBaseTexture.Clear: Boolean;
begin
  Result := False;
end;

procedure TCustomBaseTexture.OnDeviceRestore(const Sender: TObject; const EventData, UserData: Pointer);
begin
  if not DeviceRestore then
    LogText(SCannotRestoreTexture, TLogType.Error);
end;

procedure TCustomBaseTexture.OnDeviceRelease(const Sender: TObject; const EventData, UserData: Pointer);
begin
  DeviceRelease;
end;

{$ENDREGION}
{$REGION 'TLockedPixelsSurfaceWrapper'}

constructor TCustomLockableTexture.TLockedPixelsSurfaceWrapper.Create(const ALockedTexture: TCustomLockableTexture;
  const ALockedPixels: TLockedPixels);
begin
  inherited Create;

  FLockedTexture := ALockedTexture;
  FLockedPixels := ALockedPixels;
  ResetAllocation;
end;

destructor TCustomLockableTexture.TLockedPixelsSurfaceWrapper.Destroy;
begin
  if FLockedTexture <> nil then
  begin
    FLockedTexture.FLockSurface := nil;
    FLockedTexture.Unlock;
    FLockedTexture := nil;
  end;

  inherited;
end;

procedure TCustomLockableTexture.TLockedPixelsSurfaceWrapper.ResetAllocation;
begin
  FBits := FLockedPixels.Bits;
  FPitch := FLockedPixels.Pitch;
  FWidth := FLockedPixels.LockedRect.Width;
  FHeight := FLockedPixels.LockedRect.Height;
  FPixelFormat := FLockedPixels.PixelFormat;
  FBytesPerPixel := FLockedPixels.BytesPerPixel;
  FBufferSize := FWidth * FHeight * FBytesPerPixel;
end;

function TCustomLockableTexture.TLockedPixelsSurfaceWrapper.Reallocate(const NewWidth, NewHeight: Integer;
  const NewPixelFormat: TPixelFormat): Boolean;
begin
  Result := (NewWidth = FLockedPixels.LockedRect.Width) and (NewHeight = FLockedPixels.LockedRect.Height) and
    (NewPixelFormat = FLockedPixels.PixelFormat);
end;

function TCustomLockableTexture.TLockedPixelsSurfaceWrapper.ApproximatePixelFormat(
  const NewPixelFormat: TPixelFormat): TPixelFormat;
begin
  Result := FLockedPixels.PixelFormat;
end;

{$ENDREGION}
{$REGION 'TCustomLockableTexture'}

procedure TCustomLockableTexture.SetDynamicTexture(const Value: Boolean);
begin
  if FState = TTextureState.NotInitialized then
    FDynamicTexture := Value;
end;

function TCustomLockableTexture.IsLockRectInvalid(const Rect: TIntRect): Boolean;
begin
  Result := ((Rect.Left < 0) or (Rect.Top < 0) or (Rect.Right > Width) or (Rect.Bottom > Height));
end;

function TCustomLockableTexture.IsLockRectFull(const Rect: TIntRect): Boolean;
begin
  Result := Rect.Empty or ((Rect.Left = 0) and (Rect.Top = 0) and (Rect.Right = Width) and (Rect.Bottom = Height));
end;

function TCustomLockableTexture.LockSurface(const Surface: TPixelSurface; const Rect: TIntRect;
  out LockedPixels: TLockedPixels): Boolean;
var
  LockRect: TIntRect;
begin
  if (Surface = nil) or Surface.IsEmpty then
  begin
    LockedPixels.Reset;
    Exit(False);
  end;

  if not IsLockRectFull(Rect) then
    LockRect := Rect
  else
    LockRect := IntRect(0, 0, Width, Height);

  LockedPixels.Bits := Surface.PixelPtr[LockRect.Left, LockRect.Top];
  LockedPixels.Pitch := Surface.Pitch;
  LockedPixels.BytesPerPixel := FBytesPerPixel;
  LockedPixels.PixelFormat := FPixelFormat;
  LockedPixels.LockedRect := LockRect;

  Result := True;
end;

function TCustomLockableTexture.Lock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean;
begin
  if FLocked or (FLockSurface <> nil) or IsLockRectInvalid(Rect) then
  begin
    LockedPixels.Reset;
    Exit(False);
  end;

  if not DoLock(Rect, LockedPixels) then
    Exit(False);

  FLocked := True;
  Result := True;
end;

function TCustomLockableTexture.Lock(out LockedPixels: TLockedPixels): Boolean;
begin
  Result := Lock(ZeroIntRect, LockedPixels);
end;

function TCustomLockableTexture.Lock(const Rect: TIntRect; out Surface: TPixelSurface): Boolean;
var
  LockedPixels: TLockedPixels;
begin
  if not Lock(Rect, LockedPixels) then
  begin
    Surface := nil;
    Exit(False);
  end;

  FLockSurface := TLockedPixelsSurfaceWrapper.Create(Self, LockedPixels);

  Surface := FLockSurface;
  Result := True;
end;

function TCustomLockableTexture.Lock(out Surface: TPixelSurface): Boolean;
begin
  Result := Lock(ZeroIntRect, Surface);
end;

function TCustomLockableTexture.Unlock: Boolean;
begin
  if not FLocked then
    Exit(False);

  if FLockSurface <> nil then
  begin
    FLockSurface.FLockedTexture := nil;
    {$IFNDEF AUTOREFCOUNT}FreeAndNil(FLockSurface);{$ENDIF}
  end;

  try
    Result := DoUnlock;
  finally
    FLocked := False;
  end;
end;

function TCustomLockableTexture.DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
  const DestPos: TPoint2px): Boolean;
var
  SourceSurface, DestSurface: TPixelSurface;
begin
  if Source is TCustomLockableTexture then
  begin
    if not TCustomLockableTexture(Source).Lock(SourceSurface) then
      Exit(False);
    try
      if not Lock(DestSurface) then
        Exit(False);
      try
        Result := DestSurface.CopyRect(DestPos, SourceSurface, SourceRect);
      finally
        DestSurface.Free;
      end;
    finally
      SourceSurface.Free;
    end;
  end
  else
    Result := inherited;
end;

function TCustomLockableTexture.Clear: Boolean;
var
  LockedPixels: TLockedPixels;
  I, ByteCount: Integer;
begin
  if not Lock(LockedPixels) then
    Exit(False);
  try
    ByteCount := LockedPixels.LockedRect.Width * LockedPixels.BytesPerPixel;
    if ByteCount < 1 then
      Exit(False);

    for I := 0 to FHeight - 1 do
      FillChar(LockedPixels.Scanline[I]^, ByteCount, 0);
  finally
    Result := Unlock;
  end;
end;

function TCustomLockableTexture.CopyFromSurfaceRect(DestPos: TPoint2px; const Source: TPixelSurface;
  SourceRect: TIntRect): Boolean;
var
  Surface: TPixelSurface;
begin
  if (FState <> TTextureState.Initialized) or (Source = nil) then
    Exit(False);

  if SourceRect.Empty then
    SourceRect := IntRect(ZeroPoint2px, Source.Size);

  if not ClipCoords(Source.Size, GetSize, SourceRect, DestPos) then
    Exit(False);

  if not Lock(IntRect(DestPos.X, DestPos.Y, SourceRect.Width, SourceRect.Height), Surface) then
    Exit(False);
  try
    Result := Surface.CopyRect(ZeroPoint2px, Source, SourceRect);
  finally
    Surface.Free;
  end;
end;

function TCustomLockableTexture.CopyFromSurface(const Source: TPixelSurface): Boolean;
var
  Surface: TPixelSurface;
begin
  if (FState <> TTextureState.Initialized) or (Source = nil) then
    Exit(False);

  if not Lock(Surface) then
    Exit(False);
  try
    Result := Surface.CopyFrom(Source);
  finally
    Surface.Free;
  end;
end;

function TCustomLockableTexture.CopyToSurfaceRect(DestPos: TPoint2px; const Dest: TPixelSurface;
  SourceRect: TIntRect): Boolean;
var
  Surface: TPixelSurface;
begin
  if (FState <> TTextureState.Initialized) or (Dest = nil) then
    Exit(False);

  if SourceRect.Empty then
    SourceRect := IntRect(ZeroPoint2px, GetSize);

  if not ClipCoords(GetSize, Dest.Size, SourceRect, DestPos) then
    Exit(False);

  if not Lock(SourceRect, Surface) then
    Exit(False);
  try
    Result := Dest.CopyRect(DestPos, Surface, IntRect(0, 0, SourceRect.Width, SourceRect.Height));
  finally
    Surface.Free;
  end;
end;

function TCustomLockableTexture.CopyToSurface(const Dest: TPixelSurface): Boolean;
var
  Surface: TPixelSurface;
begin
  if (FState <> TTextureState.Initialized) or (Dest = nil) then
    Exit(False);

  if not Lock(Surface) then
    Exit(False);
  try
    Result := Dest.CopyFrom(Surface);
  finally
    Surface.Free;
  end;
end;

{$ENDREGION}
{$REGION 'TCustomDrawableTexture'}

procedure TCustomDrawableTexture.SetDepthStencil(const Value: TDepthStencil);
begin
  if FState = TTextureState.NotInitialized then
    FDepthStencil := Value;
end;

procedure TCustomDrawableTexture.SetMultisamples(const Value: Integer);
begin
  if FState = TTextureState.NotInitialized then
    FMultisamples := Value;
end;

{$ENDREGION}

end.
