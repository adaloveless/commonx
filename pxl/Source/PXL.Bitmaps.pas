unit PXL.Bitmaps;
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
{< High-level bitmaps that in addition to store image data have canvas and can be drawn into. }
interface

{$INCLUDE PXL.Config.inc}

uses
  SysUtils, Classes, PXL.TypeDef, PXL.Types, PXL.Lists, PXL.Surfaces, PXL.Devices, PXL.Textures, PXL.Canvas;

type
  { Type of storage used by bitmap. }
  TBitmapStorage = (
    { Image is currently stored in system memory, typically @link(TPixelSurface). }
    System,

    { Image is currently stored in lockable texture, typically @link(TCustomLockableTexture). }
    Lockable,

    { Image is currently stored in drawable texture, typically @link(TCustomDrawableTexture). }
    Drawable);

  { High-level bitmap that contains image data with multiple storage options (e.g. system, texture, etc.) and has
    canvas so it can be drawn into. Bitmap operations such as resizing or storage change attempts to preserve existing
    data as much as possible. In contrast to many other hardware-assisted components in the framework, the methods of
    this class return no result type but raise exceptions when issues arise. }
  TBitmap = class(TCustomCanvasImage)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FDevice: TCustomDevice;
    FRegions: TIntRectList;

    FStorage: TBitmapStorage;

    FSurface: TPixelSurface;
    FLockable: TCustomLockableTexture;
    FDrawable: TCustomDrawableTexture;

    FCanvas: TCustomCanvas;
    FCanvasSceneCount: Integer;

    FWidth: Integer;
    FHeight: Integer;
    FPixelFormat: TPixelFormat;

    FDeviceRestoreHandle: Cardinal;
    FDeviceReleaseHandle: Cardinal;

    function CreateLockableTexture: TCustomLockableTexture;
    function CreateDrawableTexture: TCustomDrawableTexture;

    procedure SwitchFromSurfaceToLockable;
    procedure SwitchFromLockableToSystem;
    procedure SwitchFromLockableToDrawable;
    procedure SwitchFromDrawableToLockable;

    procedure ResizeSurface(const NewWidth, NewHeight: Integer);
    procedure ResizeLockable(const NewWidth, NewHeight: Integer);
    procedure ResizeDrawable(const NewWidth, NewHeight: Integer);

    procedure ConvertLockable(const NewPixelFormat: TPixelFormat);

    function GetCurrentSurface: TPixelSurface;
    function GetCurrentTexture: TCustomBaseTexture;
    function GetCanvas: TCustomCanvas;

    function GetSize: TPoint2px;
    procedure SetPixelFormat(const Value: TPixelFormat);
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure DiscardStorage;

    procedure OnDeviceRestore(const Sender: TObject; const EventData, UserData: Pointer);
    procedure OnDeviceRelease(const Sender: TObject; const EventData, UserData: Pointer);
  protected
    { Returns number of textures as needed by canvas. In this implementation, this will always return one. }
    function GetTextureCount: Integer; override;

    { Returns either lockable or drawable texture currently occupied by bitmap. @code(Index) should always be zero as
      bitmap can have only one texture. This may trigger storage change if current storage has no texture support. }
    function GetTexture(const Index: Integer): TCustomBaseTexture; override;

    { Returns region specified by the given index from list of existing regions and @nil if @code(Index) is outside of
      valid range.}
    function GetRegion(const Index: Integer): TIntRectList.PItem; override;

    { This function is called during @code(TBitmap.Canvas.BeginScene) call, which essentially tells bitmap's drawable
      texture to activate. This may trigger storage range if current storage is not drawable texture. }
    function CanvasBeginScene: Boolean; override;

    { This function is called during @code(TBitmap.Canvas.EndScene) call, which essentially tells bitmap's drawable
      texture to deactivate.  }
    procedure CanvasEndScene; override;

    { Attempts to change current storage to a new one, preserving the data. If operation fails for some reason, the
      appropriate exception will be raised. }
    procedure ChangeStorage(const NewStorage: TBitmapStorage); virtual;
  public
    { Creates new instance of bitmap bound to the specific device. }
    constructor Create(const ADevice: TCustomDevice);
    { @exclude } destructor Destroy; override;

    { Returns @True if the bitmap has zero size (in other words, is empty) and @False otherwise. }
    function IsEmpty: Boolean;

    { Clears the bitmap by filling image data with zeros. }
    procedure Clear;

    { Copies the entire contents of source bitmap to current one. This ensures that current bitmap has the same size
      as the source. The current pixel format is not changed and if it remains unspecified before this call, it will
      be set to the default one. This may change bitmap storage for both source and current bitmaps depending on the
      currently set values to be able to reliably copy image information. }
    procedure CopyFrom(const Source: TBitmap);

    { Changes bitmap width and height to new parameters preserving original data as much as possible. }
    procedure SetSize(const NewWidth, NewHeight: Integer); overload;

    { Changes bitmap size to new value preserving original data as much as possible. }
    procedure SetSize(const NewSize: TPoint2px); overload; inline;

    { Loads bitmap from external file. This may change current pixel format, size and storage type. The function uses
      image format manager reference provided by the associated device.}
    procedure LoadFromFile(const FileName: StdString);

    { Saves bitmap to external file. This may change current storage type. The function uses image format manager
      reference provided by the associated device. @code(Quality) parameter is a hint that may have different
      interpretations depending on destination file format, platform and provider; for instance, for JPEG files, on
      most implementations this is just a typecast integer representing image quality between 0 and 100. }
    procedure SaveToFile(const FileName: StdString; const Quality: Pointer = nil);

    { Loads bitmap from specified stream with image format identified by the given extension. This may change current
      pixel format, size and storage type. The function uses image format manager reference provided by the associated
      device.}
    procedure LoadFromStream(const Stream: TStream; const Extension: StdString);

    { Saves bitmap to specified stream with image format identified by the given extension. This may change current
      storage type. The function uses image format manager reference provided by the associated device. @code(Quality)
      parameter is a hint that may have different interpretations depending on destination file format, platform and
      provider; for instance, for JPEG files, on most implementations this is just a typecast integer representing
      image quality between 0 and 100. }
    procedure SaveToStream(const Stream: TStream; const Extension: StdString; const Quality: Pointer = nil);

    { Reference to the device class to which this bitmap is bound to. }
    property Device: TCustomDevice read FDevice;

    { This list contains a list of rectangles that define sub-images (also know as "patterns" or "tiles") located on
      this bitmap. When drawing the current bitmap to canvas, there are methods that can take region number as
      parameter, therefore facilitating use of image atlases or tiles. }
    property Regions: TIntRectList read FRegions;

    { Storage type currently occupied by bitmap. Changing this to a different value may create new type of resources,
      while releasing others, but the image data is preserved. Many operations may automatically change storage type
      depending on context, so this should be changed manually only when doing so can improve performance (for example,
      to start immediately with drawable texture instead of going through system/lockable stages). }
    property Storage: TBitmapStorage read FStorage write ChangeStorage;

    { Provides access to image data wrapped in @link(TPixelSurface) for direct pixel access. This changes storage type
      to @code(TBitmapStorage.System) while preserving image data. }
    property Surface: TPixelSurface read GetCurrentSurface;

    { Provides access to image data wrapped in @link(TCustomBaseTexture), so it can be used for rendering. If current
      storage type is system (which has no texture), this will change storage type to @code(TBitmapStorage.Lockable). }
    property Texture: TCustomBaseTexture read GetCurrentTexture;

    { Provides access to bitmap's canvas wrapped in @link(TCustomCanvas), so that bitmap can be rendered into. This
      changes storage type to @code(TBitmapStorage.Drawable) while preserving image data. }
    property Canvas: TCustomCanvas read GetCanvas;

    { Width of bitmap in pixels. Changing this preserves image contents as much as possible. }
    property Width: Integer read FWidth write SetWidth;

    { Height of bitmap in pixels. Changing this preserves image contents as much as possible. }
    property Height: Integer read FHeight write SetHeight;

    { Size of bitmap in pixels. Changing this preserves image contents as much as possible. }
    property Size: TPoint2px read GetSize write SetSize;

    { Determines the pixel format in which to store bitmap's pixels. Changing this preserves image contents as much as
      possible. }
    property PixelFormat: TPixelFormat read FPixelFormat write SetPixelFormat;
  end;

  { Generic bitmap exception. }
  EBitmapException = class(Exception);

  { Device to which bitmap should be bound to is invalid. }
  EParentDeviceInvalid = class(EBitmapException);

  { Device to which bitmap should be bound to is @nil. }
  EParentDeviceNull = class(EParentDeviceInvalid);

  { Provider associated with bitmap's device is invalid or @nil. }
  EInvalidProvider = class(EParentDeviceInvalid);

  { Image format manager associated with bitmap's device is invalid or @nil. }
  EInvalidImageFormatManager = class(EInvalidProvider);

  { There was a problem converting to new pixel format or new pixel format may not be supported. }
  EPixelFormatConvert = class(EBitmapException);

  { Bitmap's canvas could not be initialized. }
  ECanvasInitialize = class(EBitmapException);

  { There was a problem trying to start rendering using bitmap's canvas. }
  ECanvasBeginScene = class(EBitmapException);

  { Generic bitmap's texture exception. }
  ETextureException = class(EBitmapException);

  { Generic bitmap's lockable texture exception. }
  ELockableTexture = class(ETextureException);

  { Could not create lockable texture instance. }
  ELockableTextureCreate = class(ELockableTexture);

  { There was a problem trying to initialize lockable texture. }
  ELockableTextureInitialize = class(ELockableTexture);

  { An error occurred when trying to access lockable texture. }
  ELockableTextureAccess = class(ELockableTexture);

  { Lockable texture could not be restored after its surface was released due to "lost device" scenario. }
  ELockableTextureRestore = class(ELockableTexture);

  { Could not clear lockable texture. }
  ELockableTextureClear = class(ELockableTexture);

  { Error during copy operation in lockable texture. }
  ELockableTextureCopy = class(ELockableTexture);

  { Generic bitmap's drawable texture exception. }
  EDrawableTexture = class(ETextureException);

  { Could not create drawable texture instance. }
  EDrawableTextureCreate = class(EDrawableTexture);

  { There was a problem trying to initialize drawable texture. }
  EDrawableTextureInitialize = class(EDrawableTexture);

  { Drawable texture could not be restored after its surface was released due to "lost device" scenario. }
  EDrawableTextureRestore = class(EDrawableTexture);

  { Could not clear drawable texture. }
  EDrawableTextureClear = class(EDrawableTexture);

  { Error during copy operation in drawable texture. }
  EDrawableTextureCopy = class(EDrawableTexture);

  { Attempting to start rendering on drawable texture failed. }
  EDrawableBeginScene = class(EDrawableTexture);

  { The specified bitmap size is invalid. }
  EInvalidBitmapSize = class(EBitmapException);

  { There was a problem loading bitmap. }
  EBitmapLoad = class(EBitmapException);

  { There was a problem saving bitmap. }
  EBitmapSave = class(EBitmapException);

  { The specified bitmap is invalid or @nil. }
  EBitmapInvalid = class(EBitmapException);

resourcestring
  { @exclude } SDeviceNotSpecified = 'Device parameter is not specified or is null.';
  { @exclude } SDeviceProviderInvalid = 'Device is parented to a non-compatible provider.';
  { @exclude } SImageFormatManagerNotSpecified = 'Image format manager in provider is not specified or is null.';

  { @exclude } SCannotConvertPixelFormat = 'Cannot convert to the specified pixel format.';
  { @exclude } SErrorCanvasInitialize = 'Error while trying to initialize canvas.';
  { @exclude } SErrorCanvasBeginScene = 'Error while trying to start new scene on canvas.';

  { @exclude } SCannotCreateLockableTexture = 'Cannot create lockable texture.';
  { @exclude } SCannotInitializeLockableTexture = 'Cannot initialize lockable texture.';
  { @exclude } SCannotAccessLockableTexture = 'Cannot access lockable texture.';
  { @exclude } SCannotRestoreLockableTexture = 'Cannot restore lockable texture.';
  { @exclude } SCannotClearLockableTexture = 'Cannot clear lockable texture.';
  { @exclude } SErrorLockableTextureCopy = 'Error while copying contents in lockable texture.';

  { @exclude } SCannotCreateDrawableTexture = 'Cannot create drawable texture.';
  { @exclude } SCannotInitializeDrawableTexture = 'Cannot initialize drawable texture.';
  { @exclude } SCannotRestoreDrawableTexture = 'Cannot restore drawable texture.';
  { @exclude } SCannotClearDrawableTexture = 'Cannot clear drawable texture.';
  { @exclude } SErrorDrawableTextureCopy = 'Error while copying contents in drawable texture.';
  { @exclude } SErrorDrawableBeginScene = 'Error while trying to start new scene on drawable texture.';

  { @exclude } SSpecifiedBitmapSizeIsInvalid = 'The specified bitmap size is invalid.';
  { @exclude } SCannotLoadBitmapFromFile = 'Could not load bitmap from specified file.';
  { @exclude } SCannotLoadBitmapFromStream = 'Could not load bitmap from the stream.';
  { @exclude } SCannotSaveBitmapToFile = 'Could not save bitmap to specified file.';
  { @exclude } SCannotSaveBitmapToStream = 'Could not save bitmap to the stream.';
  { @exclude } SSpecifiedBitmapIsInvalid = 'The specified bitmap is invalid.';

implementation

uses
  PXL.Providers, PXL.ImageFormats;

constructor TBitmap.Create(const ADevice: TCustomDevice);
begin
  inherited Create;

  FDevice := ADevice;
  if FDevice = nil  then
    raise EParentDeviceNull.Create(SDeviceNotSpecified);

  if not (FDevice.Provider is TGraphicsDeviceProvider) then
    raise EInvalidProvider.Create(SDeviceProviderInvalid);

  FRegions := TIntRectList.Create;
  FSurface := TPixelSurface.Create;

  FDeviceRestoreHandle := FDevice.OnRestore.Subscribe(OnDeviceRestore);
  FDeviceReleaseHandle := FDevice.OnRelease.Subscribe(OnDeviceRelease);

  Increment_PXL_ClassInstances;
end;

destructor TBitmap.Destroy;
begin
  try
    if FDevice <> nil then
    begin
      if FDeviceReleaseHandle <> 0 then
        FDevice.OnRelease.Unsubscribe(FDeviceReleaseHandle);

      if FDeviceRestoreHandle <> 0 then
        FDevice.OnRestore.Unsubscribe(FDeviceRestoreHandle);
    end;

    FCanvas.Free;
    FDrawable.Free;
    FLockable.Free;
    FSurface.Free;
    FRegions.Free;
    FDevice := nil;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TBitmap.GetTextureCount: Integer;
begin
  Result := 1;
end;

function TBitmap.GetTexture(const Index: Integer): TCustomBaseTexture;
begin
  if Index = 0 then
  begin
    if FStorage = TBitmapStorage.System then
      ChangeStorage(TBitmapStorage.Lockable);

    case FStorage of
      TBitmapStorage.Lockable:
        Result := FLockable;

      TBitmapStorage.Drawable:
        Result := FDrawable;

    else
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

function TBitmap.GetRegion(const Index: Integer): TIntRectList.PItem;
begin
  Result := FRegions[Index];
end;

function TBitmap.IsEmpty: Boolean;
begin
  Result := (FWidth < 1) or (FHeight < 1);
end;

function TBitmap.CreateLockableTexture: TCustomLockableTexture;
begin
  Result := TGraphicsDeviceProvider(FDevice.Provider).CreateLockableTexture(FDevice, False);
  if Result = nil then
    raise ELockableTextureCreate.Create(SCannotCreateLockableTexture);
end;

function TBitmap.CreateDrawableTexture: TCustomDrawableTexture;
begin
  Result := TGraphicsDeviceProvider(FDevice.Provider).CreateDrawableTexture(FDevice, False);
  if Result = nil then
    raise EDrawableTextureCreate.Create(SCannotCreateDrawableTexture);
end;

procedure TBitmap.SwitchFromSurfaceToLockable;
begin
  FLockable := CreateLockableTexture;

  FLockable.Width := FWidth;
  FLockable.Height := FHeight;
  FLockable.PixelFormat := FPixelFormat;

  try
    if not IsEmpty then
    begin
      if not FLockable.Initialize then
        raise ELockableTextureInitialize.Create(SCannotInitializeLockableTexture);

      FPixelFormat := FLockable.PixelFormat;

      if not FLockable.CopyFromSurface(FSurface) then
        raise ELockableTextureCopy.Create(SErrorLockableTextureCopy);
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(FLockable);
      raise E;
    end;
  end;

  FreeAndNil(FSurface);
end;

procedure TBitmap.SwitchFromLockableToSystem;
begin
  FSurface := TPixelSurface.Create;

  try
    if not IsEmpty then
    begin
      FSurface.SetSize(FWidth, FHeight, FPixelFormat);

      FPixelFormat := FSurface.PixelFormat;

      if not FLockable.CopyToSurface(FSurface) then
        raise ELockableTextureCopy.Create(SErrorLockableTextureCopy);
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(FSurface);
      raise E;
    end;
  end;

  FreeAndNil(FLockable);
end;

procedure TBitmap.SwitchFromLockableToDrawable;
begin
  FDrawable := CreateDrawableTexture;

  FDrawable.Width := FWidth;
  FDrawable.Height := FHeight;
  FDrawable.PixelFormat := FPixelFormat;

  try
    if not IsEmpty then
    begin
      if not FDrawable.Initialize then
        raise EDrawableTextureInitialize.Create(SCannotInitializeDrawableTexture);

      FPixelFormat := FDrawable.PixelFormat;

      if not FDrawable.CopyFrom(FLockable) then
        raise EDrawableTextureCopy.Create(SErrorDrawableTextureCopy);
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(FDrawable);
      raise E;
    end;
  end;

  FreeAndNil(FLockable);
end;

procedure TBitmap.SwitchFromDrawableToLockable;
begin
  FLockable := CreateLockableTexture;

  FLockable.Width := FWidth;
  FLockable.Height := FHeight;
  FLockable.PixelFormat := FPixelFormat;

  try
    if not IsEmpty then
    begin
      if not FLockable.Initialize then
        raise ELockableTextureInitialize.Create(SCannotInitializeLockableTexture);

      FPixelFormat := FLockable.PixelFormat;

      if not FLockable.CopyFrom(FDrawable) then
        raise EDrawableTextureCopy.Create(SErrorDrawableTextureCopy);
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(FLockable);
      raise E;
    end;
  end;

  FreeAndNil(FDrawable);
end;

procedure TBitmap.ChangeStorage(const NewStorage: TBitmapStorage);
var
  PrevStorage: TBitmapStorage;
begin
  if FStorage <> NewStorage then
  begin
    PrevStorage := FStorage;
    FStorage := NewStorage;

    case PrevStorage of
      TBitmapStorage.System:
        case NewStorage of
          TBitmapStorage.Lockable:
            SwitchFromSurfaceToLockable;

          TBitmapStorage.Drawable:
            begin
              SwitchFromSurfaceToLockable;
              FStorage := TBitmapStorage.Lockable;

              SwitchFromLockableToDrawable;
              FStorage := TBitmapStorage.Drawable;
            end;
        end;

      TBitmapStorage.Lockable:
        case NewStorage of
          TBitmapStorage.System:
            SwitchFromLockableToSystem;

          TBitmapStorage.Drawable:
            SwitchFromLockableToDrawable;
        end;

      TBitmapStorage.Drawable:
        case NewStorage of
          TBitmapStorage.System:
            begin
              SwitchFromDrawableToLockable;
              FStorage := TBitmapStorage.Lockable;

              SwitchFromLockableToSystem;
              FStorage := TBitmapStorage.System;
            end;

          TBitmapStorage.Lockable:
            SwitchFromDrawableToLockable;
        end;
    end;
  end;
end;

function TBitmap.GetCurrentSurface: TPixelSurface;
begin
  ChangeStorage(TBitmapStorage.System);
  Result := FSurface;
end;

function TBitmap.GetCurrentTexture: TCustomBaseTexture;
begin
  Result := GetTexture(0);
end;

function TBitmap.GetCanvas: TCustomCanvas;
begin
  if FCanvas = nil then
  begin
    FCanvas := TGraphicsDeviceProvider(FDevice.Provider).CreateCanvas(FDevice);
    if FCanvas = nil then
      raise ECanvasInitialize.Create(SErrorCanvasInitialize);

    if not FCanvas.Initialize then
    begin
      FreeAndNil(FCanvas);
      raise ECanvasInitialize.Create(SErrorCanvasInitialize);
    end;

    SetCanvasParent(FCanvas, Self);
  end;

  Result := FCanvas;
end;

function TBitmap.GetSize: TPoint2px;
begin
  Result := Point2px(FWidth, FHeight);
end;

procedure TBitmap.SetPixelFormat(const Value: TPixelFormat);
begin
  if FPixelFormat <> Value then
  begin
    case FStorage of
      TBitmapStorage.System:
        begin
          if not FSurface.IsEmpty then
            if not FSurface.ConvertPixelFormat(Value) then
              raise EPixelFormatConvert.Create(SCannotConvertPixelFormat);

          FPixelFormat := Value;
        end;

      TBitmapStorage.Lockable:
        if not IsEmpty then
          ConvertLockable(Value);

      TBitmapStorage.Drawable:
        if not IsEmpty then
        begin
          ChangeStorage(TBitmapStorage.Lockable);
          ConvertLockable(Value);
        end;
    end;
  end;
end;

procedure TBitmap.SetWidth(const Value: Integer);
begin
  SetSize(Value, FHeight);
end;

procedure TBitmap.SetHeight(const Value: Integer);
begin
  SetSize(FWidth, Value);
end;

procedure TBitmap.ResizeSurface(const NewWidth, NewHeight: Integer);
var
  NewSurface: TPixelSurface;
begin
  if (NewWidth > 0) and (NewHeight > 0) then
  begin
    NewSurface := TPixelSurface.Create;
    try
      NewSurface.SetSize(NewWidth, NewHeight, FPixelFormat);

      FPixelFormat := NewSurface.PixelFormat;

      if (NewWidth > FWidth) or (NewHeight > FHeight) then
        NewSurface.Clear;

      NewSurface.CopyRect(ZeroPoint2px, FSurface, ZeroIntRect);
    except
      on E: Exception do
        begin
          NewSurface.Free;
          raise E;
        end;
    end;

    try
      FSurface.Free;
    finally
      FSurface := NewSurface;
    end;
  end
  else
    FSurface.SetSize(NewWidth, NewHeight, FPixelFormat);
end;

procedure TBitmap.ResizeLockable(const NewWidth, NewHeight: Integer);
var
  NewLockable: TCustomLockableTexture;
begin
  if (NewWidth > 0) and (NewHeight > 0) then
  begin
    NewLockable := CreateLockableTexture;
    try
      NewLockable.Width := NewWidth;
      NewLockable.Height := NewHeight;
      NewLockable.PixelFormat := FPixelFormat;

      if not NewLockable.Initialize then
        raise ELockableTextureInitialize.Create(SCannotInitializeLockableTexture);

      FPixelFormat := NewLockable.PixelFormat;

      if (NewWidth > FWidth) or (NewHeight > FHeight) then
        if not NewLockable.Clear then
          raise ELockableTextureClear.Create(SCannotClearLockableTexture);

      if not NewLockable.CopyFrom(FLockable) then
        raise ELockableTextureCopy.Create(SErrorLockableTextureCopy);
    except
      on E: Exception do
        begin
          NewLockable.Free;
          raise E;
        end;
    end;

    try
      FLockable.Free;
    finally
      FLockable := NewLockable;
    end;
  end
  else
  begin
    FLockable.Finalize;

    FLockable.Width := NewWidth;
    FLockable.Height := NewHeight;
  end;
end;

procedure TBitmap.ResizeDrawable(const NewWidth, NewHeight: Integer);
var
  NewDrawable: TCustomDrawableTexture;
begin
  if (NewWidth > 0) and (NewHeight > 0) then
  begin
    NewDrawable := CreateDrawableTexture;
    try
      NewDrawable.Width := NewWidth;
      NewDrawable.Height := NewHeight;
      NewDrawable.PixelFormat := FPixelFormat;

      if not NewDrawable.Initialize then
        raise EDrawableTextureInitialize.Create(SCannotInitializeDrawableTexture);

      FPixelFormat := NewDrawable.PixelFormat;

      if (NewWidth > FWidth) or (NewHeight > FHeight) then
        if not NewDrawable.Clear then
          raise EDrawableTextureClear.Create(SCannotClearDrawableTexture);

      if not NewDrawable.CopyFrom(FDrawable) then
        raise EDrawableTextureCopy.Create(SErrorDrawableTextureCopy);
    except
      on E: Exception do
        begin
          NewDrawable.Free;
          raise E;
        end;
    end;

    try
      FDrawable.Free;
    finally
      FDrawable := NewDrawable;
    end;
  end
  else
  begin
    FDrawable.Finalize;

    FDrawable.Width := NewWidth;
    FDrawable.Height := NewHeight;
  end;
end;

procedure TBitmap.ConvertLockable(const NewPixelFormat: TPixelFormat);
var
  NewLockable: TCustomLockableTexture;
begin
  NewLockable := CreateLockableTexture;
  try
    NewLockable.Width := FWidth;
    NewLockable.Height := FHeight;
    NewLockable.PixelFormat := NewPixelFormat;

    if not NewLockable.Initialize then
      raise ELockableTextureInitialize.Create(SCannotInitializeLockableTexture);

    FPixelFormat := NewLockable.PixelFormat;

    if not NewLockable.CopyFrom(FLockable) then
      raise ELockableTextureCopy.Create(SErrorLockableTextureCopy);
  except
    on E: Exception do
      begin
        NewLockable.Free;
        raise E;
      end;
  end;

  try
    FLockable.Free;
  finally
    FLockable := NewLockable;
  end;
end;

procedure TBitmap.SetSize(const NewWidth, NewHeight: Integer);
begin
  if (NewWidth < 0) or (NewHeight < 0) then
    raise EInvalidBitmapSize.Create(SSpecifiedBitmapSizeIsInvalid);

  if (FWidth <> NewWidth) or (FHeight <> NewHeight) then
  begin
    if not IsEmpty then
    begin
      case FStorage of
        TBitmapStorage.System:
          ResizeSurface(NewWidth, NewHeight);

        TBitmapStorage.Lockable:
          ResizeLockable(NewWidth, NewHeight);

        TBitmapStorage.Drawable:
          ResizeDrawable(NewWidth, NewHeight);
      end;

      FWidth := NewWidth;
      FHeight := NewHeight;
    end
    else
    begin
      FWidth := NewWidth;
      FHeight := NewHeight;

      if not IsEmpty then
        case FStorage of
          TBitmapStorage.System:
            begin
              FSurface.SetSize(FWidth, FHeight, FPixelFormat);

              FPixelFormat := FSurface.PixelFormat;

              FSurface.Clear;
            end;

          TBitmapStorage.Lockable:
            begin
              FLockable.Width := FWidth;
              FLockable.Height := FHeight;
              FLockable.PixelFormat := FPixelFormat;

              if not FLockable.Initialize then
                raise ELockableTextureInitialize.Create(SCannotInitializeLockableTexture);

              FPixelFormat := FLockable.PixelFormat;

              if not FLockable.Clear then
                raise ELockableTextureClear.Create(SCannotClearLockableTexture);
            end;

          TBitmapStorage.Drawable:
            begin
              FDrawable.Width := FWidth;
              FDrawable.Height := FHeight;
              FDrawable.PixelFormat := FPixelFormat;

              if not FDrawable.Initialize then
                raise ELockableTextureInitialize.Create(SCannotInitializeLockableTexture);

              FPixelFormat := FDrawable.PixelFormat;

              if not FDrawable.Clear then
                raise EDrawableTextureClear.Create(SCannotClearDrawableTexture);
            end;
        end;
    end;
  end;
end;

procedure TBitmap.SetSize(const NewSize: TPoint2px);
begin
  SetSize(NewSize.X, NewSize.Y);
end;

procedure TBitmap.DiscardStorage;
begin
  SetSize(0, 0);
  ChangeStorage(TBitmapStorage.System);
end;

procedure TBitmap.Clear;
begin
  case FStorage of
    TBitmapStorage.System:
      FSurface.Clear(0);

    TBitmapStorage.Lockable:
      if not FLockable.Clear then
        raise ELockableTextureClear.Create(SCannotClearLockableTexture);

    TBitmapStorage.Drawable:
      if not FDrawable.Clear then
        raise EDrawableTextureClear.Create(SCannotClearDrawableTexture);
  end;
end;

procedure TBitmap.CopyFrom(const Source: TBitmap);
begin
  if (Source = nil) or Source.IsEmpty then
    EBitmapInvalid.Create(SSpecifiedBitmapIsInvalid);

  if GetSize <> Source.Size then
    SetSize(Source.Size);

  if (Source.Device <> FDevice) or (Source.Storage = TBitmapStorage.System) or (FStorage = TBitmapStorage.System) then
  begin // System-level copy.
    Source.ChangeStorage(TBitmapStorage.System);
    ChangeStorage(TBitmapStorage.System);

    FSurface.CopyFrom(Source.FSurface);
  end
  else if (Source.Storage = TBitmapStorage.Lockable) or (FStorage = TBitmapStorage.Lockable) then
  begin // Lockable texture copy.
    Source.ChangeStorage(TBitmapStorage.Lockable);
    ChangeStorage(TBitmapStorage.Lockable);

    FLockable.CopyFrom(Source.FLockable);
  end
  else // Drawable texture copy.
    FDrawable.CopyFrom(Source.FDrawable);
end;

procedure TBitmap.LoadFromFile(const FileName: StdString);
var
  Manager: TCustomImageFormatManager;
begin
  DiscardStorage;

  Manager := FDevice.ImageFormatManager;
  if Manager = nil then
    raise EInvalidImageFormatManager.Create(SImageFormatManagerNotSpecified);

  if not Manager.LoadFromFile(FileName, FSurface, TAlphaFormatRequest.NonPremultiplied) then
    raise EBitmapLoad.Create(SCannotLoadBitmapFromFile);

  FWidth := FSurface.Width;
  FHeight := FSurface.Height;
  FPixelFormat := FSurface.PixelFormat;
end;

procedure TBitmap.SaveToFile(const FileName: StdString; const Quality: Pointer);
var
  Manager: TCustomImageFormatManager;
begin
  ChangeStorage(TBitmapStorage.System);

  Manager := FDevice.ImageFormatManager;
  if Manager = nil then
    raise EInvalidImageFormatManager.Create(SImageFormatManagerNotSpecified);

  if not Manager.SaveToFile(FileName, FSurface, Quality) then
    raise EBitmapSave.Create(SCannotSaveBitmapToFile);
end;

procedure TBitmap.LoadFromStream(const Stream: TStream; const Extension: StdString);
var
  Manager: TCustomImageFormatManager;
begin
  DiscardStorage;

  Manager := FDevice.ImageFormatManager;
  if Manager = nil then
    raise EInvalidImageFormatManager.Create(SImageFormatManagerNotSpecified);

  if not Manager.LoadFromStream(Extension, Stream, FSurface, TAlphaFormatRequest.NonPremultiplied) then
    raise EBitmapLoad.Create(SCannotLoadBitmapFromStream);

  FWidth := FSurface.Width;
  FHeight := FSurface.Height;
  FPixelFormat := FSurface.PixelFormat;
end;

procedure TBitmap.SaveToStream(const Stream: TStream; const Extension: StdString; const Quality: Pointer);
var
  Manager: TCustomImageFormatManager;
begin
  ChangeStorage(TBitmapStorage.System);

  Manager := FDevice.ImageFormatManager;
  if Manager = nil then
    raise EInvalidImageFormatManager.Create(SImageFormatManagerNotSpecified);

  if not Manager.SaveToStream(Extension, Stream, FSurface, Quality) then
    raise EBitmapSave.Create(SCannotSaveBitmapToStream);
end;

function TBitmap.CanvasBeginScene: Boolean;
begin
  if FCanvasSceneCount < 1 then
  begin
    ChangeStorage(TBitmapStorage.Drawable);

    if not FDrawable.BeginDraw then
      raise EDrawableBeginScene.Create(SErrorDrawableBeginScene);
  end;

  if not CanvasInternalBeginScene(FCanvas) then
  begin
    if FCanvasSceneCount < 1 then
      FDrawable.EndDraw;
    raise ECanvasBeginScene.Create(SErrorCanvasBeginScene);
  end;

  Inc(FCanvasSceneCount);
  Result := True;
end;

procedure TBitmap.CanvasEndScene;
begin
  CanvasInternalEndScene(FCanvas);

  Dec(FCanvasSceneCount);

  if FCanvasSceneCount <= 0 then
    FDrawable.EndDraw;
end;

procedure TBitmap.OnDeviceRestore(const Sender: TObject; const EventData, UserData: Pointer);
begin
  case FStorage of
    TBitmapStorage.Lockable:
      if not FLockable.DeviceRestore then
        raise ELockableTextureRestore.Create(SCannotRestoreLockableTexture);

    TBitmapStorage.Drawable:
      if not FDrawable.DeviceRestore then
        raise EDrawableTextureRestore.Create(SCannotRestoreDrawableTexture);
  end;
end;

procedure TBitmap.OnDeviceRelease(const Sender: TObject; const EventData, UserData: Pointer);
begin
  case FStorage of
    TBitmapStorage.Lockable:
      FLockable.DeviceRelease;

    TBitmapStorage.Drawable:
      FDrawable.DeviceRelease;
  end;
end;

end.
