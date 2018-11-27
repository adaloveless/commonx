unit PXL.Images;
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
{< Atlas images that may contain one or more textures and multiple regions, optimized for rendering with the canvas. }
interface

{$INCLUDE PXL.Config.inc}

uses
  Classes, PXL.TypeDef, PXL.Types, PXL.Lists, PXL.Devices, PXL.Textures, PXL.Canvas;

type
  { Atlas image may contain one or more textures and multiple regions, which can be selected when rendering on canvas. }
  TAtlasImage = class(TCustomCanvasImage)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FDevice: TCustomDevice;
    FName: StdString;

    FTextures: array of TCustomLockableTexture;
    FRegions: TIntRectList;

    FMipMapping: Boolean;
    FPixelFormat: TPixelFormat;
    FDynamicImage: Boolean;
    FPremultipliedAlpha: Boolean;
    FSubscribedTextures: Boolean;

    function GetLockableTexture(const Index: Integer): TCustomLockableTexture; inline;
  protected
    { Returns the number of textures present in the image. }
    function GetTextureCount: Integer; override;

    { Provides access to individual textures in the image based on the specified index. If the index is outside of
      valid range, @nil is returned. }
    function GetTexture(const Index: Integer): TCustomBaseTexture; override;

    { Retrieves the region specified by its index with information regarding texture number and margins. If the index
      is outside of valid range, @nil is returned. }
    function GetRegion(const Index: Integer): TIntRectList.PItem; override;
  public
    { Creates new instance of @code(TAtlasImage) class bound to the specified device. @code(ASubscribedTextures)
      indicates whether the textures contained in this image should be subscribed to handle "lost device" events.
      If this image is part of @link(TAtlasImages) list, which provides its own notification events regarding device
      status, then @code(ASubscribedTextures) should be set to @False. }
    constructor Create(const ADevice: TCustomDevice; const ASubscribedTextures: Boolean = True);
    { @exclude } destructor Destroy; override;

    { Inserts a new texture (not initialized) to the list of existing textures and returns its index. }
    function InsertTexture: Integer; overload;

    { Inserts a new texture to the list of existing textures and tries to initialize with the given width and height,
      returning its index. If initialization fails, then @nil is returned and the texture is not added to the list. }
    function InsertTexture(const InitWidth, InitHeight: Integer): TCustomLockableTexture; overload;

    { Removes texture with the specified index from the list. }
    procedure RemoveTexture(const Index: Integer);

    { Returns index of the specified texture in the list. If no such texture is found, -1 is returned. }
    function IndexOfTexture(const Texture: TCustomLockableTexture): Integer;

    { Removes any existing textures from the list. }
    procedure ClearTextures;

    { Loads image with format corresponding to the given extension from the stream. This uses image format manager
      reference from the associated device. Returns @True when succeeded and @False otherwise. }
    function LoadFromStream(const Extension: StdString; const Stream: TStream;
      const AlphaFormatRequest: TAlphaFormatRequest = TAlphaFormatRequest.DontCare): Boolean;

    { Loads image with the specified file name from disk. This uses image format manager reference from the associated
      device. Returns @True when succeeded and @False otherwise. }
    function LoadFromFile(const FileName: StdString;
      const AlphaFormatRequest: TAlphaFormatRequest = TAlphaFormatRequest.DontCare): Boolean;

    { Loads image with the specified file name from /assets sub-folder on @italic(Android) platform. This uses image
      format manager reference from the associated device. Returns @True when succeeded and @False otherwise.
      The method is unsupported on non-Android platforms. }
    function LoadFromAsset(const AssetName: StdString;
      const AlphaFormatRequest: TAlphaFormatRequest = TAlphaFormatRequest.DontCare): Boolean;

    { Configures the list of regions as if they were rectangular patterns in the image tiled horizontally in series of
      rows and columns. This fills @link(Regions) with new data.
        @param(PatternSize Defines the size of individual pattern.)
        @param(VisibleSize Defines the "visible" size inside each individual pattern. This should either match
          @code(PatternSize) or be smaller.)
        @param(PatternCount Determines the total number of patterns inside the image. The total number of resulting
          regions will be equal or smaller than this number.) }
    procedure SetupRegionPatterns(const PatternSize: TPoint2px; VisibleSize: TPoint2px;
      const PatternCount: Integer = 0); overload;

    { Configures the list of regions as if they were rectangular patterns in the image tiled horizontally in series of
      rows and columns. This fills @link(Regions) with new data. }
    procedure SetupRegionPatterns(const PatternSize: TPoint2px); overload; inline;

    { Restores the image after its texture surfaces have been lost (that is, after @link(DeviceRelease) call). Note
      that this method should only be called when @code(ASubscribedTextures) has been set to @False during creation. }
    function DeviceRestore: Boolean; virtual;

    { Releases the image's texture surfaces when the device has been lost. Note that this method should only be
      called when @code(ASubscribedTextures) has been set to @False during creation. }
    procedure DeviceRelease; virtual;

    { The device to which this image is bound to. }
    property Device: TCustomDevice read FDevice;

    { Unique name of the image by which it can be referenced in @link(TAtlasImages) list. }
    property Name: StdString read FName write FName;

    { Total number of textures present in this image. }
    property TextureCount: Integer read GetTextureCount;

    { Provides access to each texture present in the image by its index. If the index is specified outside of valid
      range, then @nil is returned. }
    property Texture[const Index: Integer]: TCustomLockableTexture read GetLockableTexture;

    { This list contains rectangles (with texture number stored as extra pointer value) that define individual
      sub-images located on one or several textures. }
    property Regions: TIntRectList read FRegions;

    { Determines whether the image should contain mipmap data. This can increase memory consumption and slow down the
      loading of images, but produces better visual results when the image is shrunk to smaller sizes. }
    property MipMapping: Boolean read FMipMapping write FMipMapping;

    { The pixel format that will be used in all image's textures. This parameter is merely a hint and different format
      may be used in the textures, depending on hardware support; if this format is not supported, usually the closest
      format will be chosen in textures. When loading pixel data, the conversion will be done automatically, if the
      texture format does not match the stored pixel format. }
    property PixelFormat: TPixelFormat read FPixelFormat write FPixelFormat;

    { Indicates whether the pixels in this image have premultiplied or non-premultiplied alpha. This does not affect
      the actual pixels, it is merely a hint for rendering canvas. }
    property PremultipliedAlpha: Boolean read FPremultipliedAlpha write FPremultipliedAlpha;

    { Determines whether this image requires frequent access to its pixel data. If this parameter is set to @True,
      dynamic textures will be used, where pixel data can be updated frequently. This should be used when image pixels
      need to be updated at least once per frame. }
    property DynamicImage: Boolean read FDynamicImage write FDynamicImage;

    { Indicates whether this class has textures subscribed to device events, handling scenarios such as "device lost".
      If this is set to @False, likely the handling is provided by parent list class such as @link(TAtlasImages)
      through use of @link(DeviceRestore) and @link(DeviceRelease) events. }
    property SubscribedTextures: Boolean read FSubscribedTextures;
  end;

  { The list that may contain one or more instances of @link(TAtlasImage) and provide facilities to search for images
    by their unique names, image loading and handling "device lost" scenario. }
  TAtlasImages = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FDevice: TCustomDevice;
    FImages: array of TAtlasImage;

    FSearchList: array of Integer;
    FSearchListDirty: Boolean;

    FDeviceRestoreHandle: Cardinal;
    FDeviceReleaseHandle: Cardinal;

    function GetItem(const Index: Integer): TAtlasImage;
    function GetItemCount: Integer;
    function GetImage(const Name: StdString): TAtlasImage;

    function FindEmptySlot: Integer;

    procedure OnDeviceRestore(const Sender: TObject; const EventData, UserData: Pointer);
    procedure OnDeviceRelease(const Sender: TObject; const EventData, UserData: Pointer);

    procedure InitSearchList;
    procedure SwapSearchList(const Index1, Index2: Integer);
    function CompareSearchList(const Index1, Index2: Integer): Integer;
    function SplitSearchList(const Start, Stop: Integer): Integer;
    procedure SortSearchList(const Start, Stop: Integer);
    procedure UpdateSearchList;
  public
    { Creates new instance of @code(TAtlasImages) class bound to the specified device. Elements inside this list should
      be bound to the same device (and if they are created by this class, they automatically are). }
    constructor Create(const ADevice: TCustomDevice);
    { @exclude } destructor Destroy; override;

    { Removes all existing images from the list. }
    procedure Clear;

    { Returns index of the specified image element in the list. If no image is found, -1 is returned. }
    function IndexOf(const Element: TAtlasImage): Integer; overload;

    { Returns index of the specified image element by its name in the list. If no image is found, -1 is returned. }
    function IndexOf(const ImageName: StdString): Integer; overload;

    { Inserts the specified image to the list. }
    function Insert(const Image: TAtlasImage): Integer;

    { Includes the specified image to the list, if it wasn't included previously. This implies searching the list
      before adding the element, which may impact performance. }
    function Include(const Element: TAtlasImage): Integer;

    { Removes image with the specified index from the list. }
    procedure Remove(const Index: Integer);

    { Loads image with format corresponding to the given extension from the stream. This uses image format manager
      reference from the associated device.
        @param(Extension Extension (including dot, e.g. ".png") that represents the format in which the image is
          stored in the stream.)
        @param(Stream Stream which will be used for reading the image from. The current position of the stream will be
          used and after the call it will be adjusted to be right at the end of image data.)
        @param(ImageName New name of the image that should be added to the list.)
        @param(AlphaFormatRequest The preference for premultipled or non-premultiplied alpha that should be used
          while loading the image.)
        @param(PixelFormat The preference for certain pixel format when loading the image. It may or may not be
          followed, depending on platform and support and a closest match may potentially be selected.)
        @returns(Image index in the list or -1 on error.) }
    function AddFromStream(const Extension: StdString; const Stream: TStream; const ImageName: StdString = '';
      const MipMapping: Boolean = True; const AlphaFormatRequest: TAlphaFormatRequest = TAlphaFormatRequest.DontCare;
      const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Integer;

    { Loads image from the specified file on disk. This uses image format manager reference from the associated device.
        @param(FileName A valid file name (with extension) that includes full path that represents the image.)
        @param(ImageName New name of the image that should be added to the list.)
        @param(AlphaFormatRequest The preference for premultipled or non-premultiplied alpha that should be used
          while loading the image.)
        @param(PixelFormat The preference for certain pixel format when loading the image. It may or may not be
          followed, depending on platform and support and a closest match may potentially be selected.)
        @returns(Image index in the list or -1 on error.) }
    function AddFromFile(const FileName: StdString; const ImageName: StdString = ''; const MipMapping: Boolean = True;
      const AlphaFormatRequest: TAlphaFormatRequest = TAlphaFormatRequest.DontCare;
      const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Integer;

    { Loads image from the specified file located in /assets sub-folder. This function only works on @italic(Android)
      platform. This uses image format manager reference from the associated device.
        @param(FileName A valid file name (with extension) that represents the image.)
        @param(ImageName New name of the image that should be added to the list.)
        @param(AlphaFormatRequest The preference for premultipled or non-premultiplied alpha that should be used
          while loading the image.)
        @param(PixelFormat The preference for certain pixel format when loading the image. It may or may not be
          followed, depending on platform and support and a closest match may potentially be selected.)
        @returns(Image index in the list or -1 on error.) }
    function AddFromAsset(const AssetName: StdString; const ImageName: StdString = ''; const MipMapping: Boolean = True;
      const AlphaFormatRequest: TAlphaFormatRequest = TAlphaFormatRequest.DontCare;
      const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Integer;

    { Indicates that one of the images had its name changed, so the list needs to be refreshed to make searching by
      name (@link(IndexOf) function and @link(Image) property) work properly. }
    procedure MarkSearchDirty;

    { The device to which this list is bound to. }
    property Device: TCustomDevice read FDevice;

    { The total number of images in the list. }
    property ItemCount: Integer read GetItemCount;

    { Provides access to individual images in the list by the corresponding index. If the index is outside of valid
      range, @nil is returned. }
    property Items[const Index: Integer]: TAtlasImage read GetItem; default;

    { Provides access to individual images in the list by unique image name (not case-sensitive). If no image with such
      name is found, @nil is returned. }
    property Image[const Name: StdString]: TAtlasImage read GetImage;
  end;

implementation

uses
  SysUtils, PXL.Consts, PXL.Logs, PXL.Classes, PXL.ImageFormats, PXL.Formats, PXL.Surfaces, PXL.Providers;

{$REGION 'TAtlasImage'}

constructor TAtlasImage.Create(const ADevice: TCustomDevice; const ASubscribedTextures: Boolean);
begin
  inherited Create;

  FDevice := ADevice;
  FSubscribedTextures := ASubscribedTextures;
  FRegions := TIntRectList.Create;

  FMipMapping := False;
  FPixelFormat := TPixelFormat.Unknown;
  FPremultipliedAlpha := False;
  FDynamicImage := False;

  Increment_PXL_ClassInstances;
end;

destructor TAtlasImage.Destroy;
begin
  try
    ClearTextures;
    FRegions.Free;
    FDevice := nil;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TAtlasImage.GetTexture(const Index: Integer): TCustomBaseTexture;
begin
  if (Index >= 0) and (Index < Length(FTextures)) then
    Result := FTextures[Index]
  else
    Result := nil;
end;

function TAtlasImage.GetLockableTexture(const Index: Integer): TCustomLockableTexture;
begin
  Result := TCustomLockableTexture(GetTexture(Index));
end;

function TAtlasImage.GetTextureCount: Integer;
begin
  Result := Length(FTextures);
end;

function TAtlasImage.GetRegion(const Index: Integer): TIntRectList.PItem;
begin
  Result := FRegions[Index];
end;

function TAtlasImage.InsertTexture: Integer;
var
  Texture: TCustomLockableTexture;
begin
  if (FDevice <> nil) and (not (FDevice.Provider is TGraphicsDeviceProvider)) then
    Exit(-1);

  Texture := TGraphicsDeviceProvider(FDevice.Provider).CreateLockableTexture(FDevice, FSubscribedTextures);
  if Texture = nil then
    Exit(-1);

  Result := Length(FTextures);
  SetLength(FTextures, Result + 1);

  FTextures[Result] := Texture;
end;

function TAtlasImage.InsertTexture(const InitWidth, InitHeight: Integer): TCustomLockableTexture;
var
  Index: Integer;
begin
  if (FDevice <> nil) and (not (FDevice.Provider is TGraphicsDeviceProvider)) then
    Exit(nil);

  Result := TGraphicsDeviceProvider(FDevice.Provider).CreateLockableTexture(FDevice, FSubscribedTextures);
  if Result = nil then
    Exit;

  Result.Width := InitWidth;
  Result.Height := InitHeight;
  Result.PixelFormat := FPixelFormat;
  Result.PremultipliedAlpha := FPremultipliedAlpha;
  Result.MipMapping := FMipMapping;
  Result.DynamicTexture := FDynamicImage;

  if not Result.Initialize then
  begin
    FreeAndNil(Result);
    Exit;
  end;

  Index := Length(FTextures);
  SetLength(FTextures, Index + 1);

  FTextures[Index] := Result;
end;

function TAtlasImage.IndexOfTexture(const Texture: TCustomLockableTexture): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(FTextures) - 1 do
    if FTextures[I] = Texture then
      Exit(I);

  Result := -1;
end;

procedure TAtlasImage.RemoveTexture(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= Length(FTextures)) then
    Exit;

  FTextures[Index].Free;

  for I := Index to Length(FTextures) - 2 do
    FTextures[I] := FTextures[I + 1];

  SetLength(FTextures, Length(FTextures) - 1);
end;

procedure TAtlasImage.ClearTextures;
var
  I: Integer;
begin
  for I := Length(FTextures) - 1 downto 0 do
    FTextures[I].Free;

  SetLength(FTextures, 0);
end;

function TAtlasImage.LoadFromStream(const Extension: StdString; const Stream: TStream;
  const AlphaFormatRequest: TAlphaFormatRequest): Boolean;
var
  Manager: TCustomImageFormatManager;
  Surface: TPixelSurface;
  Texture: TCustomLockableTexture;
  I, DestBytesPerPixel: Integer;
  LockedPixels: TLockedPixels;
  PixelBuffer: Pointer;
begin
  if FDevice = nil then
    Exit(False);

  Manager := FDevice.ImageFormatManager;
  if Manager = nil then
    Exit(False);

  ClearTextures;
  FRegions.Clear;

  Surface := TPixelSurface.Create;
  try
    if not Manager.LoadFromStream(Extension, Stream, Surface, AlphaFormatRequest) then
      Exit(False);

    if FPixelFormat = TPixelFormat.Unknown then
      FPixelFormat := Surface.PixelFormat;

    FPremultipliedAlpha := Surface.PremultipliedAlpha;

    Texture := InsertTexture(Surface.Width, Surface.Height);
    if Texture = nil then
      Exit(False);

    if not Texture.Lock(LockedPixels) then
      Exit(False);
    try
      DestBytesPerPixel := Texture.Width * Texture.BytesPerPixel;

      if Surface.PixelFormat.CanBulkCopyTo(Texture.PixelFormat) then
      begin // Direct copy of pixels (very fast).
        for I := 0 to Texture.Height - 1 do
          Move(Surface.Scanline[I]^, LockedPixels.Scanline[I]^, DestBytesPerPixel);
      end
      else if Surface.PixelFormat = TPixelFormat.A8R8G8B8 then
      begin // Conversion from 32-bit RGBA to custom pixel format (moderately slow).
        for I := 0 to Texture.Height - 1 do
          Pixel32toXArray(Surface.Scanline[I], LockedPixels.Scanline[I], Texture.PixelFormat, Texture.Width);
      end
      else
      begin // Conversion from one pixel format to another (quite slow).
        GetMem(PixelBuffer, Texture.Width * TPixelFormat.A8R8G8B8.Bytes);
        try
          for I := 0 to Texture.Height - 1 do
          begin
            PixelXto32Array(Surface.Scanline[I], PixelBuffer, Surface.PixelFormat, Surface.Width);
            Pixel32toXArray(PixelBuffer, LockedPixels.Scanline[I], Texture.PixelFormat, Texture.Width);
          end;
        finally
          FreeMem(PixelBuffer);
        end;
      end;
    finally
      Texture.Unlock;
    end;

    Texture.PremultipliedAlpha := Surface.PremultipliedAlpha;
  finally
    Surface.Free;
  end;

  Result := True;
end;

function TAtlasImage.LoadFromFile(const FileName: StdString; const AlphaFormatRequest: TAlphaFormatRequest): Boolean;
var
  Stream: TFileStream;
begin
  ClearTextures;

  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
    Exit(False);
  end;

  try
    Result := LoadFromStream(ExtractFileExt(FileName), Stream, AlphaFormatRequest);
  finally
    Stream.Free;
  end;
end;

function TAtlasImage.LoadFromAsset(const AssetName: StdString; const AlphaFormatRequest: TAlphaFormatRequest): Boolean;
var
  Stream: TAssetStream;
begin
  ClearTextures;

  try
    Stream := TAssetStream.Create(AssetName);
  except
    Exit(False);
  end;

  try
    Result := LoadFromStream(ExtractFileExt(AssetName), Stream, AlphaFormatRequest);
  finally
    Stream.Free;
  end;
end;

procedure TAtlasImage.SetupRegionPatterns(const PatternSize: TPoint2px; VisibleSize: TPoint2px;
  const PatternCount: Integer);
var
  Texture: TCustomLockableTexture;
  I, TextureIndex, PatInRow, PatInCol, PatInTex, RegionIndex: Integer;
begin
  FRegions.Clear;

  if (PatternSize.X <= 0) or (PatternSize.Y <= 0) then
    Exit;

  if VisibleSize.X <= 0 then
    VisibleSize.X := PatternSize.X;

  if VisibleSize.Y <= 0 then
    VisibleSize.Y := PatternSize.Y;

  RegionIndex := 0;

  for TextureIndex := 0 to Length(FTextures) - 1 do
  begin
    Texture := FTextures[TextureIndex];
    if Texture = nil then
      Continue;

    PatInRow := Texture.Width div PatternSize.X;
    PatInCol := Texture.Height div PatternSize.Y;
    PatInTex := PatInRow * PatInCol;

    if PatInTex > 0 then
      for I := 0 to PatInTex - 1 do
      begin
        FRegions.Add((I mod PatInRow) * PatternSize.X, (I div PatInRow) * PatternSize.Y, VisibleSize.X, VisibleSize.Y,
          Pointer(TextureIndex));

        Inc(RegionIndex);
        if (PatternCount > 0) and (RegionIndex >= PatternCount) then
          Break;
      end
    else
    begin
      FRegions.Add(0, 0, Texture.Width, Texture.Height, Pointer(TextureIndex));
      Inc(RegionIndex);
    end;

    if (PatternCount > 0) and (RegionIndex >= PatternCount) then
      Break;
  end;
end;

procedure TAtlasImage.SetupRegionPatterns(const PatternSize: TPoint2px);
begin
  SetupRegionPatterns(PatternSize, ZeroPoint2px);
end;

function TAtlasImage.DeviceRestore: Boolean;
var
  I: Integer;
begin
  Result := True;

  for I := 0 to Length(FTextures) - 1 do
    if (FTextures[I] <> nil) and (not FTextures[I].DeviceRestore) then
      Result := False;
end;

procedure TAtlasImage.DeviceRelease;
var
  I: Integer;
begin
  for I := 0 to Length(FTextures) - 1 do
    if FTextures[I] <> nil then
      FTextures[I].DeviceRelease;
end;

{$ENDREGION}
{$REGION 'TAtlasImages'}

constructor TAtlasImages.Create(const ADevice: TCustomDevice);
begin
  inherited Create;

  FDevice := ADevice;

  if FDevice <> nil then
  begin
    FDeviceRestoreHandle := FDevice.OnRestore.Subscribe(OnDeviceRestore);
    FDeviceReleaseHandle := FDevice.OnRelease.Subscribe(OnDeviceRelease);
  end
  else
  begin
    FDeviceRestoreHandle := 0;
    FDeviceReleaseHandle := 0;
  end;

  Inc(PXL_ClassInstances);
end;

destructor TAtlasImages.Destroy;
begin
  try
    if FDevice <> nil then
    begin
      if FDeviceReleaseHandle <> 0 then
        FDevice.OnRelease.Unsubscribe(FDeviceReleaseHandle);

      if FDeviceRestoreHandle <> 0 then
        FDevice.OnRestore.Unsubscribe(FDeviceRestoreHandle);
    end
    else
    begin
      FDeviceReleaseHandle := 0;
      FDeviceRestoreHandle := 0;
    end;

    Clear;
    FDevice := nil;
  finally
    Dec(PXL_ClassInstances);
  end;

  inherited;
end;

function TAtlasImages.GetItem(const Index: Integer): TAtlasImage;
begin
  if (Index >= 0) and (Index < Length(FImages)) then
    Result := FImages[Index]
  else
    Result := nil;
end;

function TAtlasImages.GetItemCount: Integer;
begin
  Result := Length(FImages);
end;

function TAtlasImages.FindEmptySlot: Integer;
var
  I: Integer;
begin
  for I := 0 to Length(FImages) - 1 do
    if FImages[I] = nil then
      Exit(I);

  Result := -1;
end;

function TAtlasImages.Insert(const Image: TAtlasImage): Integer;
var
  Index: Integer;
begin
  Index := FindEmptySlot;
  if Index = -1 then
  begin
    Index := Length(FImages);
    SetLength(FImages, Index + 1);
  end;

  FImages[Index] := Image;
  Result := Index;

  FSearchListDirty := True;
end;

function TAtlasImages.IndexOf(const Element: TAtlasImage): Integer;
var
  I: Integer;
begin
  if Element = nil then
    Exit(-1);

  for I := 0 to Length(FImages) - 1 do
    if FImages[I] = Element then
      Exit(I);

  Result := -1;
end;

function TAtlasImages.Include(const Element: TAtlasImage): Integer;
begin
  Result := IndexOf(Element);
  if Result = -1 then
    Result := Insert(Element);
end;

procedure TAtlasImages.Remove(const Index: Integer);
begin
  if (Index < 0) or (Index >= Length(FImages)) then
    Exit;

  FreeAndNil(FImages[Index]);
  FSearchListDirty := True;
end;

procedure TAtlasImages.Clear;
var
  I: Integer;
begin
  for I := Length(FImages) - 1 downto 0 do
    FImages[I].Free;

  SetLength(FImages, 0);
  FSearchListDirty := True;
end;

procedure TAtlasImages.OnDeviceRestore(const Sender: TObject; const EventData, UserData: Pointer);
var
  I: Integer;
  FailedToRestore: Boolean;
begin
  FailedToRestore := False;

  for I := 0 to Length(FImages) - 1 do
    if (FImages[I] <> nil) and (not FImages[I].SubscribedTextures) and (not FImages[I].DeviceRestore) then
      FailedToRestore := True;

  if FailedToRestore then
    LogText(SCannotRestoreImages, TLogType.Error);
end;

procedure TAtlasImages.OnDeviceRelease(const Sender: TObject; const EventData, UserData: Pointer);
var
  I: Integer;
begin
  for I := 0 to Length(FImages) - 1 do
    if (FImages[I] <> nil) and (not FImages[I].SubscribedTextures) then
      FImages[I].DeviceRelease;
end;

function TAtlasImages.AddFromStream(const Extension: StdString; const Stream: TStream; const ImageName: StdString;
  const MipMapping: Boolean; const AlphaFormatRequest: TAlphaFormatRequest; const PixelFormat: TPixelFormat): Integer;
var
  TempImage: TAtlasImage;
begin
  TempImage := TAtlasImage.Create(FDevice, False);
  TempImage.Name := ImageName;

  TempImage.MipMapping := MipMapping;
  TempImage.PixelFormat := PixelFormat;

  if not TempImage.LoadFromStream(Extension, Stream, AlphaFormatRequest) then
  begin
    TempImage.Free;
    Exit(-1);
  end;

  Result := Insert(TempImage);
end;

function TAtlasImages.AddFromFile(const FileName: StdString; const ImageName: StdString; const MipMapping: Boolean;
  const AlphaFormatRequest: TAlphaFormatRequest; const PixelFormat: TPixelFormat): Integer;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
    Exit(-1);
  end;

  try
    Result := AddFromStream(ExtractFileExt(FileName), Stream, ImageName, MipMapping, AlphaFormatRequest, PixelFormat);
  finally
    Stream.Free;
  end;
end;

function TAtlasImages.AddFromAsset(const AssetName: StdString; const ImageName: StdString; const MipMapping: Boolean;
  const AlphaFormatRequest: TAlphaFormatRequest; const PixelFormat: TPixelFormat): Integer;
var
  Stream: TAssetStream;
begin
  try
    Stream := TAssetStream.Create(AssetName);
  except
    Exit(-1);
  end;

  try
    Result := AddFromStream(ExtractFileExt(AssetName), Stream, ImageName, MipMapping, AlphaFormatRequest, PixelFormat);
  finally
    Stream.Free;
  end;
end;

procedure TAtlasImages.InitSearchList;
var
  I, ImageCount, Index: Integer;
begin
  ImageCount := 0;

  for I := 0 to Length(FImages) - 1 do
    if FImages[I] <> nil then
      Inc(ImageCount);

  if Length(FSearchList) <> ImageCount then
    SetLength(FSearchList, ImageCount);

  Index := 0;

  for I := 0 to Length(FImages) - 1 do
    if FImages[I] <> nil then
    begin
      FSearchList[Index] := I;
      Inc(Index);
    end;
end;

procedure TAtlasImages.SwapSearchList(const Index1, Index2: Integer);
var
  TempValue: Integer;
begin
  TempValue := FSearchList[Index1];
  FSearchList[Index1] := FSearchList[Index2];
  FSearchList[Index2] := TempValue;
end;

function TAtlasImages.CompareSearchList(const Index1, Index2: Integer): Integer;
begin
  Result := CompareText(FImages[Index1].Name, FImages[Index2].Name);
end;

function TAtlasImages.SplitSearchList(const Start, Stop: Integer): Integer;
var
  Left, Right, Pivot: Integer;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := FSearchList[Start];

  while Left <= Right do
  begin
    while (Left <= Stop) and (CompareSearchList(FSearchList[Left], Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (CompareSearchList(FSearchList[Right], Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      SwapSearchList(Left, Right);
  end;

  SwapSearchList(Start, Right);
  Result := Right;
end;

procedure TAtlasImages.SortSearchList(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := SplitSearchList(Start, Stop);

    SortSearchList(Start, SplitPt - 1);
    SortSearchList(SplitPt + 1, Stop);
  end;
end;

procedure TAtlasImages.UpdateSearchList;
begin
  InitSearchList;

  if Length(FSearchList) > 1 then
    SortSearchList(0, Length(FSearchList) - 1);

  FSearchListDirty := False;
end;

function TAtlasImages.IndexOf(const ImageName: StdString): Integer;
var
  Left, Right, Pivot, Res: Integer;
begin
  if FSearchListDirty then
    UpdateSearchList;

  Left := 0;
  Right := Length(FSearchList) - 1;

  while Left <= Right do
  begin
    Pivot := (Left + Right) div 2;
    Res := CompareText(FImages[FSearchList[Pivot]].Name, ImageName);

    if Res = 0 then
      Exit(FSearchList[Pivot]);

    if Res > 0 then
      Right := Pivot - 1
    else
      Left := Pivot + 1;
  end;

  Result := -1;
end;

function TAtlasImages.GetImage(const Name: StdString): TAtlasImage;
var
  Index: Integer;
begin
  Index := IndexOf(Name);

  if Index <> -1 then
    Result := FImages[Index]
  else
    Result := nil;
end;

procedure TAtlasImages.MarkSearchDirty;
begin
  FSearchListDirty := True;
end;

{$ENDREGION}

end.
