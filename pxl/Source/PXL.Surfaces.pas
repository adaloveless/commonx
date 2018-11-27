unit PXL.Surfaces;
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
{< Surfaces that provide cross-platform means of storing, converting and processing pixels.  }
interface

{$INCLUDE PXL.Config.inc}

uses
  PXL.TypeDef, PXL.Types;

type
  { Conceptual surface that provide means of reading, writing and drawing individual pixels. }
  TConceptualPixelSurface = class abstract
  protected
    { Returns pixel located at specified coordinates. This also includes a sanity check for pixel coordinates to be
      within valid range. If coordinates are outside of valid range, zero should be returned. }
    function GetPixel(X, Y: Integer): TIntColor; virtual; abstract;

    { Sets pixel located at specified coordinates. This also includes a sanity check for pixel coordinates to be
      within valid range. If coordinates are outside of valid range, nothing should be done. }
    procedure SetPixel(X, Y: Integer; const Color: TIntColor); virtual; abstract;

    { Returns pixel located at specified coordinates similarly to @link(GetPixel), but without sanity check for
      increased performance. }
    function GetPixelUnsafe(X, Y: Integer): TIntColor; virtual; abstract;

    { Sets pixel located at specified coordinates similarly to @link(GetPixel), but without sanity check for
      increased performance. }
    procedure SetPixelUnsafe(X, Y: Integer; const Color: TIntColor); virtual; abstract;
  public
    { Draws a single pixel at specified coordinates and color with alpha-blending. It also does a sanity check for
      specified coordinates and if they are outside of valid range, does nothing. }
    procedure DrawPixel(const X, Y: Integer; const Color: TIntColor); overload; virtual;

    { Draws a single pixel at specified position and color with alpha-blending. It also does a sanity check for
      specified position and if it is outside of valid range, does nothing. }
    procedure DrawPixel(const Position: TPoint2px; const Color: TIntColor); overload; inline;

    { Draws a single pixel at specified coordinates similarly to @link(DrawPixel), but without sanity check for
      increased performance. }
    procedure DrawPixelUnsafe(const X, Y: Integer; const Color: TIntColor); overload;

    { Draws a single pixel at specified position similarly to @link(DrawPixel), but without sanity check for
      increased performance. }
    procedure DrawPixelUnsafe(const Position: TPoint2px; const Color: TIntColor); overload; inline;

    { Provides access to surface's individual pixels. See @link(GetPixel) and @link(SetPixel) on how this actually
      works. }
    property Pixels[X, Y: Integer]: TIntColor read GetPixel write SetPixel;

    { Provides access to surface's individual pixels without sanity check for increased performance.
      See @link(GetPixelUnsafe) and @link(SetPixelUnsafe) on how this actually works. }
    property PixelsUnsafe[X, Y: Integer]: TIntColor read GetPixelUnsafe write SetPixelUnsafe;
  end;

  { Surface that stores pixels in one of supported formats, with facilities for pixel format conversion, resizing,
    copying, drawing, shrinking and so on. This can serve as a base for more advanced hardware-based surfaces, but
    it also provides full software implementation for all the functions. }
  TPixelSurface = class(TConceptualPixelSurface)
  private
    FName: StdString;
    FPremultipliedAlpha: Boolean;

    function GetSize: TPoint2px;

    function GetScanlineAddress(const Index: Integer): Pointer; inline;
    function GetPixelAddress(const X, Y: Integer): Pointer; inline;

    function GetScanline(const Index: Integer): Pointer;
    function GetPixelPtr(const X, Y: Integer): Pointer;
  protected
    { Memory reference to top/left corner of pixel data contained by this surface, with horizontal rows arranged
      linearly from top to bottom. }
    FBits: Pointer;

    { Currently set number of bytes each horizontal row of pixels occupies. This may differ than the actual calculated
      number and may include unused or even protected memory locations, which should simply be skipped. }
    FPitch: Cardinal;

    { Current width of surface in pixels. }
    FWidth: Integer;

    { Current height of surface in pixels. }
    FHeight: Integer;

    { Size of current surface in bytes. }
    FBufferSize: Cardinal;

    { Current pixel format in which pixels are stored. }
    FPixelFormat: TPixelFormat;

    { Current number of bytes each pixel occupies. }
    FBytesPerPixel: Cardinal;

    { Reads pixel from the surface and provides necessary pixel format conversion based on parameters such as
      @link(FBits), @link(FPitch), @link(FPixelFormat), @link(FBytesPerPixel), @link(FWidth) and @link(FHeight).
      This function does range checking for @italic(X) and @italic(Y) parameters and if they are outside of valid
      range, returns completely black/translucent color (in other words, zero). }
    function GetPixel(X, Y: Integer): TIntColor; override;

    { Writes pixel to the surface and provides necessary pixel format conversion based on parameters such as
      @link(FBits), @link(FPitch), @link(FPixelFormat), @link(FBytesPerPixel), @link(FWidth) and @link(FHeight).
      This function does range checking for @italic(X) and @italic(Y) parameters and if they are outside of valid
      range, does nothing. }
    procedure SetPixel(X, Y: Integer; const Color: TIntColor); override;

    { Reads pixel from the surface similarly to @link(GetPixel), but does not do any range checking for @italic(X) and
      @italic(Y) with the benefit of increased performance. }
    function GetPixelUnsafe(X, Y: Integer): TIntColor; override;

    { Write pixel to the surface similarly to @link(SetPixel), but does not do any range checking for @italic(X) and
      @italic(Y) with the benefit of increased performance. }
    procedure SetPixelUnsafe(X, Y: Integer; const Color: TIntColor); override;

    { Resets pixel surface allocation, releasing any previously allocated memory and setting all relevant parameters
      to zero. }
    procedure ResetAllocation; virtual;

    { Reallocates current pixel surface to new size and pixel format, discarding any previous written content.
      This function returns @True when the operation was successful and @False otherwise. }
    function Reallocate(const NewWidth, NewHeight: Integer; const NewPixelFormat: TPixelFormat): Boolean; virtual;
  public
    { Creates new instance of this class with empty name. }
    constructor Create; virtual;

    { Creates new instance of this class with the specified name. }
    constructor CreateNamed(const AName: StdString);

    { @exclude } destructor Destroy; override;

    { Checks whether the surface has non-zero width and height. }
    function IsEmpty: Boolean;

    { Redefines surface size to the specified width, height and pixel format, discarding previous contents.
      This function provide sanity check on specified parameters and calls @link(Reallocate) accordingly.
      @True is returned when the operation has been successful and @False otherwise. }
    function SetSize(NewWidth, NewHeight: Integer;
      NewPixelFormat: TPixelFormat = TPixelFormat.Unknown): Boolean; overload;

    { Redefines surface size to the specified size and pixel format, discarding previous contents.
      This function provide sanity check on specified parameters and calls @link(Reallocate) accordingly.
      @True is returned when the operation has been successful and @False otherwise. }
    function SetSize(const NewSize: TPoint2px;
      const NewPixelFormat: TPixelFormat = TPixelFormat.Unknown): Boolean; overload; inline;

    { Takes the provided pixel format and returns one of pixel formats currently supported, which is a closest match
      to the provided one. If there is no possible match, this function returns @italic(TPixelFormat.Unknown). }
    function ApproximatePixelFormat(const NewPixelFormat: TPixelFormat): TPixelFormat; virtual;

    { Converts surface from its currently set pixel format to the new one. If both format match, the function does
      nothing. @True is returned when the operation was successful and @False otherwise. }
    function ConvertPixelFormat(const NewPixelFormat: TPixelFormat): Boolean; virtual;

    { Copies entire contents from source surface to this one. If the current surface has size and/or pixel format
      not specified, these will be copied from the source surface as well. If current surface is not empty, then its
      pixel format will not be modified - in this case, pixel format conversion may occur. This function will try to
      ensure that current surface size matches the source surface and if if this cannot be achieved, will fail; as an
      alternative, @link(CopyRect) can be used to instead copy a portion of source surface to this one. @True is
      returned when the operation was successful and @False otherwise. }
    function CopyFrom(const Source: TPixelSurface): Boolean;

    { Copies a portion of source surface to this one according to specified source rectangle and destination position.
      If source rectangle is empty, then the entire source surface will be copied. This function does the appropriate
      clipping and pixel format conversion. It does not change current surface size or pixel format. @True is returned
      when the operation was successful and @False otherwise. }
    function CopyRect(DestPos: TPoint2px; const Source: TPixelSurface; SourceRect: TIntRect): Boolean;

    { Renders a portion of source surface onto this one at the specified origin, using alpha-blending and
      premultiplying pixels taken from the source surface with specified color gradient. This does pixel pixel
      conversion and clipping as necessary.  }
    procedure DrawSurface(DestPos: TPoint2px; const Source: TPixelSurface; SourceRect: TIntRect;
      const Colors: TIntColor4);

    { Draws a rectangle filled with specified gradient onto this surface with alpha-blending. For filling areas with
      the same color without alpha-blending, a better performance can be achieved with @link(FillRect). }
    procedure DrawFilledRect(DestRect: TIntRect; const Colors: TIntColor4);

    { Draws a single pixel on this surface with alpha-blending. }
    procedure DrawPixel(const X, Y: Integer; const Color: TIntColor); override;

    { Fills specified rectangle area with the given color. This also does clipping when appropriate. Note that unlike
      @link(DrawFilledRect) method, this just sets pixels to given color, without alpha-blending. }
    procedure FillRect(Rect: TIntRect; const Color: TIntColor); overload;

    { Fills rectangle area of the specified coordinates with the given color. This also does clipping when appropriate.
      Note that unlike @link(DrawFilledRect) method, this just sets pixels to given color, without alpha-blending. }
    procedure FillRect(const X, Y, Width, Height: Integer; const Color: TIntColor); overload; inline;

    { Fills surface with horizontal line of single color at the specified coordinates. }
    procedure HorizLine(X, Y, LineWidth: Integer; const Color: TIntColor);

    { Fills surface with vertical line of single color at the specified coordinates. }
    procedure VertLine(X, Y, LineHeight: Integer; const Color: TIntColor);

    { Fills surface with rectangle of one pixel wide and single color at the specified area. }
    procedure FrameRect(const Rect: TIntRect; const Color: TIntColor); overload;

    { Fills surface with rectangle of one pixel wide and single color at the specified coordinates. }
    procedure FrameRect(const X, Y, Width, Height: Integer; const Color: TIntColor); overload; inline;

    { Clears the entire surface with the given color. This does pixel format conversion when appropriate, so for
      better performance, consider using @italic(Clear) without parameters. }
    procedure Clear(const Color: TIntColor); overload;

    { Clears the entire surface with zeros. }
    procedure Clear; overload;

    { Processes surface pixels, setting alpha-channel to either fully translucent or fully opaque depending on
      @italic(Opaque) parameter. }
    procedure ResetAlpha(const Opaque: Boolean = True);

    { Processes the whole surface to determine whether it has meaningful alpha-channel. A surface that has all its
      pixels with alpha-channel set to fully translucent or fully opaque (but not mixed) is considered lacking
      alpha-channel. On the other hand, a surface that has at least one pixel with alpha-channel value different than
      any other pixel, is considered to have alpha-channel. This is useful to determine whether the surface can be
      stored in one of pixel formats lacking alpha-channel, to avoid losing any transparency information. }
    function HasAlphaChannel: Boolean;

    { Processes the whole surface, premultiplying each pixel's red, green and blue values by the corresponding
      alpha-channel value, resulting in image with premultiplied alpha. Note that this is an irreversible process,
      during which some color information is lost permanently (smaller alpha values contribute to bigger information
      loss). This is generally useful to prepare the image for generating mipmaps and/or alpha-blending, to get more
      accurate visual results. }
    procedure PremultiplyAlpha;

    { Processes the whole surface, dividing each pixel by its alpha-value, resulting in image with non-premultiplied
      alpha. This can be considered an opposite or reversal process of @link(PremultiplyAlpha). During this process,
      some color information may be lost due to precision issues. This can be useful to obtain original pixel
      information from image that has been previously premultiplied; however, this does not recover lost information
      during premultiplication process. For instance, pixels that had alpha value of zero and were premultiplied lose
      all information and cannot be recovered; pixels with alpha value of 128 (that is, 50% opaque) lose half of their
      precision and after "unpremultiply" process will have values multiple of 2. }
    procedure UnpremultiplyAlpha;

    { Mirrors the visible image on surface horizontally. }
    procedure Mirror;

    { Flips the visible image on surface vertically. }
    procedure Flip;

    { This function works similarly to @link(CopyFrom), except that it produces image with half of size, averaging
      each four pixels to one. This is specifically useful to generate mipmaps. }
    function ShrinkToHalfFrom(const Source: TPixelSurface): Boolean;

    { Takes first source image with content rendered on white background and second source image with content rendered
      on black background (discarding alpha-channels, if such are present), calculating resulting grayscale image with
      alpha-channel. This is useful in situations when an application does not generate alpha-channel, but can
      pre-render the image on different backgrounds - by rendering this image on black and white backgrounds, the
      actual alpha-channel can then be calculated using this function. }
    function ComposeWhiteBlackToGrayAlpha(const SourceWhite, SourceBlack: TPixelSurface): Boolean;

    { Retrieves pixel from integer and fractional coordinates. This works similarly to other @italic(GetBilinearPixel)
      variant that receives floating-point coordinates, but this one considers @italic(FracX) and @italic(FracY) as
      values ranging from 0 to 255, representing fractions of @italic(X) and @italic(Y). }
    function GetBilinearPixel(const X, Y, FracX, FracY: Integer): TIntColor; overload;

    { Retrieves pixel from floating-point coordinates, interpolating linearly between four neighbor pixels as
      necessary to get an accurate match. This can be used for limitless stretching, to get color values that lie
      between individual pixels and slowly change from one pixel to another. }
    function GetBilinearPixel(const X, Y: VectorFloat): TIntColor; overload;

    { Returns color value of pixel at the specified coordinates with an additional sanity check: if the coordinates
      are outside of valid range, they will be clamped so that they stay within. }
    function GetPixelWithCheck(X, Y: Integer): TIntColor; inline;

    { This function works similarly to @link(CopyRect), but provides stretching and/or shrinking. That is, it copies
      source surface rectangle onto destination rectangle with point filtering. Clipping and pixel format conversion is
      done as necessary. For a more accurate stretching, consider using @link(StretchBilinearFrom). }
    procedure StretchFrom(const Source: TPixelSurface; DestRect, SourceRect: TFloatRect);

    { This function works similarly to @link(CopyRect), but provides bilinear stretching. That is, it copies
      source surface rectangle onto destination rectangle with linear filtering (uses @link(GetBilinearPixel)
      function). Clipping and pixel format conversion is done as necessary. Note that this function is meant for
      stretching only; shrinking although will also work, but result in inaccurate results as shrinking requires
      calculating average of variable number of pixels depending on shrink ratio. }
    procedure StretchBilinearFrom(const Source: TPixelSurface; DestRect, SourceRect: TFloatRect);

    { Unique name of this surface. }
    property Name: StdString read FName;

    { Pointer to top/left corner of pixel data contained by this surface, with horizontal rows arranged linearly from
      top to bottom. }
    property Bits: Pointer read FBits;

    { The number of bytes each horizontal row of pixels occupies. This may differ than the actual calculated number and
      may include unusued or even protected memory locations, which should simply be skipped.}
    property Pitch: Cardinal read FPitch;

    { Size of the surface in bytes. }
    property BufferSize: Cardinal read FBufferSize;

    { Width of surface in pixels. }
    property Width: Integer read FWidth;

    { Height of surface in pixels. }
    property Height: Integer read FHeight;

    { Size of surface in pixels. }
    property Size: TPoint2px read GetSize;

    { Pixel format in which individual pixels are stored. }
    property PixelFormat: TPixelFormat read FPixelFormat;

    { Number of bytes each pixel occupies. }
    property BytesPerPixel: Cardinal read FBytesPerPixel;

    { Indicates whether the pixels in this surface have their alpha premultiplied or not. This is just an informative
      parameter; to actually convert pixels from one mode to another, use @link(PremultiplyAlpha) and
      @link(UnpremultiplyAlpha) methods. }
    property PremultipliedAlpha: Boolean read FPremultipliedAlpha write FPremultipliedAlpha;

    { Provides pointer to left corner of pixel data at the given scanline index (that is, row number). If the specified
      index is outside of valid range, @nil is returned. }
    property Scanline[const Index: Integer]: Pointer read GetScanline;

    { Provides pointer to the pixel data at the given coordinates. If the specified coordinates are outside of valid
      range, @nil is returned. }
    property PixelPtr[const X, Y: Integer]: Pointer read GetPixelPtr;
  end;

  TPixelSurfaces = class;

  { This class closely resembles @link(TPixelSurface), except that it also contains other surfaces, mainly useful for
    generating and storing mipmaps. }
  TPixelMipMapSurface = class(TPixelSurface)
  private
    FMipMaps: TPixelSurfaces;
  protected
    { Creates the list of mipmaps for the current surface. This method can be overriden by more extended and/or
      hardware-assisted surfaces to provide extended versions of @link(TPixelSurfaces) implementation. }
    function CreatePixelSurfaces: TPixelSurfaces; virtual;
  public
    { @exclude } constructor Create; override;
    { @exclude } destructor Destroy; override;

    { Generates mipmaps by using @link(ShrinkToHalfFrom) method sequentially until the final surface has size of one
      by one. }
    procedure GenerateMipMaps;

    { Removes any existing mipmaps from the list. }
    procedure ClearMipMaps;

    { Access to mipmaps associated with this surface. }
    property MipMaps: TPixelSurfaces read FMipMaps;
  end;

  { List of @link(TPixelSurface) elements with a function of quickly finding by unique name. }
  TPixelSurfaces = class
  private const
    ListGrowIncrement = 5;
    ListGrowFraction = 5;
  private
    FData: array of TPixelSurface;
    FDataCount: Integer;

    FSearchList: array of Integer;
    FSearchListDirty: Boolean;

    function GetCount: Integer;
    procedure Request(const NeedCapacity: Integer);
    procedure SetCount(const NewCount: Integer);
    function GetItem(const Index: Integer): TPixelSurface;
    function FindEmptySlot: Integer;

    procedure InitSearchList;
    procedure SearchListSwap(const Index1, Index2: Integer);
    function SearchListCompare(const Index1, Index2: Integer): Integer;
    function SearchListSplit(const Start, Stop: Integer): Integer;
    procedure SearchListSort(const Start, Stop: Integer);
    procedure UpdateSearchList;
    function GetSurface(const Name: StdString): TPixelSurface;
  protected
    { Creates a new instance of @link(TPixelSurface) when called by @link(Add) method. This resembles @italic(factory
      pattern) and provides a way to instantiate extended @italic(TPixelSurface) classes, for example, in
      hardware-assisted implementations. }
    function CreatePixelSurface(const SurfaceName: StdString): TPixelSurface; virtual;
  public
    { @exclude } constructor Create;
    { @exclude } destructor Destroy; override;

    { Inserts given surface element to the list and returns its index. }
    function Insert(const Surface: TPixelSurface): Integer;

    { Inserts a new surface with given name to the list and returns its index. }
    function Add(const SurfaceName: StdString = ''): Integer;

    { Removes element with specified index from the list. }
    procedure Remove(const Index: Integer);

    { Removes all elements from the list. }
    procedure Clear;

    { Returns index of the surface with given name if such exists or -1 otherwise. }
    function IndexOf(const SurfaceName: StdString): Integer;

    { Determines how many surfaces are in the list. }
    property Count: Integer read GetCount write SetCount;

    { Provides access to individual surface elements by their index. }
    property Items[const Index: Integer]: TPixelSurface read GetItem; default;

    { Provides reference to the surface with given name. If no surface with such name exists, @nil is returned. }
    property Surface[const Name: StdString]: TPixelSurface read GetSurface;
  end;

implementation

uses
  SysUtils, Math, PXL.Formats;

{$REGION 'TConceptualPixelSurface'}

procedure TConceptualPixelSurface.DrawPixel(const X, Y: Integer; const Color: TIntColor);
begin
  SetPixel(X, Y, BlendPixels(GetPixel(X, Y), Color, TIntColorRec(Color).Alpha));
end;

procedure TConceptualPixelSurface.DrawPixel(const Position: TPoint2px;
  const Color: TIntColor);
begin
  DrawPixel(Position.X, Position.Y, Color);
end;

procedure TConceptualPixelSurface.DrawPixelUnsafe(const X, Y: Integer; const Color: TIntColor);
begin
  SetPixelUnsafe(X, Y, BlendPixels(GetPixelUnsafe(X, Y), Color, TIntColorRec(Color).Alpha));
end;

procedure TConceptualPixelSurface.DrawPixelUnsafe(const Position: TPoint2px; const Color: TIntColor);
begin
  DrawPixelUnsafe(Position.X, Position.Y, Color);
end;

{$ENDREGION}
{$REGION 'TPixelSurface'}

constructor TPixelSurface.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;
end;

constructor TPixelSurface.CreateNamed(const AName: StdString);
begin
  Create;
  FName := AName;
end;

destructor TPixelSurface.Destroy;
begin
  try
    ResetAllocation;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TPixelSurface.GetSize: TPoint2px;
begin
  Result := Point2px(FWidth, FHeight);
end;

function TPixelSurface.GetScanlineAddress(const Index: Integer): Pointer;
begin
  Result := Pointer(PtrUInt(FBits) + FPitch * Cardinal(Index));
end;

function TPixelSurface.GetPixelAddress(const X, Y: Integer): Pointer;
begin
  Result := Pointer(PtrUInt(FBits) + FPitch * Cardinal(Y) + FBytesPerPixel * Cardinal(X));
end;

function TPixelSurface.GetScanline(const Index: Integer): Pointer;
begin
  if (Index >= 0) and (Index < FHeight) then
    Result := GetScanlineAddress(Index)
  else
    Result := nil;
end;

function TPixelSurface.GetPixelPtr(const X, Y: Integer): Pointer;
begin
  if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
    Result := GetPixelAddress(X, Y)
  else
    Result := nil;
end;

function TPixelSurface.GetPixel(X, Y: Integer): TIntColor;
begin
  if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
    Result := PixelXTo32(GetPixelAddress(X, Y), FPixelFormat)
  else
    Result := IntColorBlack;
end;

procedure TPixelSurface.SetPixel(X, Y: Integer; const Color: TIntColor);
begin
  if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
    Pixel32ToX(Color, GetPixelAddress(X, Y), FPixelFormat);
end;

function TPixelSurface.GetPixelUnsafe(X, Y: Integer): TIntColor;
begin
  Result := PixelXTo32(GetPixelAddress(X, Y), FPixelFormat);
end;

procedure TPixelSurface.SetPixelUnsafe(X, Y: Integer; const Color: TIntColor);
begin
  Pixel32ToX(Color, GetPixelAddress(X, Y), FPixelFormat);
end;

function TPixelSurface.IsEmpty: Boolean;
begin
  Result := (FWidth <= 0) or (FHeight <= 0) or (FBits = nil) or (FPixelFormat = TPixelFormat.Unknown);
end;

procedure TPixelSurface.ResetAllocation;
begin
  FWidth := 0;
  FHeight := 0;
  FPixelFormat := TPixelFormat.Unknown;
  FBytesPerPixel := 0;
  FPitch := 0;
  FBufferSize := 0;

  FreeMemAndNil(FBits);
end;

function TPixelSurface.Reallocate(const NewWidth, NewHeight: Integer; const NewPixelFormat: TPixelFormat): Boolean;
begin
  FWidth := NewWidth;
  FHeight := NewHeight;
  FPixelFormat := NewPixelFormat;
  FBytesPerPixel := FPixelFormat.Bytes;
  FPitch := FWidth * FBytesPerPixel;
  FBufferSize := FHeight * FPitch;

  ReallocMem(FBits, FBufferSize);
  Result := True;
end;

function TPixelSurface.SetSize(NewWidth, NewHeight: Integer; NewPixelFormat: TPixelFormat): Boolean;
begin
  NewWidth := Max(NewWidth, 0);
  NewHeight := Max(NewHeight, 0);
  NewPixelFormat := ApproximatePixelFormat(NewPixelFormat);

  if (FWidth <> NewWidth) or (FHeight <> NewHeight) or (FPixelFormat <> NewPixelFormat) then
  begin
    if (NewWidth <= 0) or (NewHeight <= 0) then
    begin
      ResetAllocation;
      Result := True
    end
    else
      Result := Reallocate(NewWidth, NewHeight, NewPixelFormat);
  end
  else
    Result := True;
end;

function TPixelSurface.SetSize(const NewSize: TPoint2px; const NewPixelFormat: TPixelFormat): Boolean;
begin
  Result := SetSize(NewSize.X, NewSize.Y, NewPixelFormat);
end;

function TPixelSurface.ApproximatePixelFormat(const NewPixelFormat: TPixelFormat): TPixelFormat;
begin
  Result := NewPixelFormat;

  if Result = TPixelFormat.Unknown then
    Result := TPixelFormat.A8R8G8B8;
end;

function TPixelSurface.ConvertPixelFormat(const NewPixelFormat: TPixelFormat): Boolean;
var
  TempBits, CopyBits: Pointer;
  TempWidth, TempHeight, I: Integer;
  TempPixelFormat: TPixelFormat;
  TempPitch: Cardinal;
begin
  if IsEmpty or (NewPixelFormat = TPixelFormat.Unknown) then
    Exit(False);

  Result := True;

  if FPixelFormat = NewPixelFormat then
    Exit;

  TempPixelFormat := FPixelFormat;
  TempWidth := FWidth;
  TempHeight := FHeight;
  TempPitch := TempWidth * TempPixelFormat.Bytes;

  GetMem(TempBits, TempPitch * TempHeight);
  try
    for I := 0 to TempHeight - 1 do
      Move(GetScanline(I)^, Pointer(PtrUInt(TempBits) + Cardinal(I) * TempPitch)^, TempPitch);

    if not SetSize(FWidth, FHeight, NewPixelFormat) then
      Exit(False);

    if (FWidth <> TempWidth) or (FHeight <> TempHeight) then
      Exit(False);

    if FPixelFormat = TempPixelFormat then
    begin // No pixel format change, direct copy.
      for I := 0 to TempHeight - 1 do
        Move(Pointer(PtrUInt(TempBits) + Cardinal(I) * TempPitch)^, GetScanline(I)^, TempPitch);
    end
    else if FPixelFormat = TPixelFormat.A8R8G8B8 then
    begin // Convert to 32-bit RGBA.
      for I := 0 to TempHeight - 1 do
        PixelXTo32Array(Pointer(PtrUInt(TempBits) + Cardinal(I) * TempPitch), GetScanline(I), TempPixelFormat,
          TempWidth);
    end
    else if TempPixelFormat = TPixelFormat.A8R8G8B8  then
    begin // Convert from 32-bit RGBA.
      for I := 0 to TempHeight - 1 do
        Pixel32ToXArray(Pointer(PtrUInt(TempBits) + Cardinal(I) * TempPitch), GetScanline(I), FPixelFormat, TempWidth);
    end
    else
    begin // Convert from one pixel format to another.
      GetMem(CopyBits, TempWidth * SizeOf(TIntColor));
      try
        for I := 0 to TempHeight - 1 do
        begin
          PixelXTo32Array(Pointer(PtrUInt(TempBits) + Cardinal(I) * TempPitch), CopyBits, TempPixelFormat, TempWidth);
          Pixel32ToXArray(CopyBits, GetScanline(I), FPixelFormat, TempWidth);
        end;
      finally
        FreeMem(CopyBits);
      end;
    end;
  finally
    FreeMem(TempBits);
  end;
end;

function TPixelSurface.CopyFrom(const Source: TPixelSurface): Boolean;
var
  NewPixelFormat: TPixelFormat;
  I: Integer;
begin
  if (Source = nil) or (Source.Bits = nil) or (Source.PixelFormat = TPixelFormat.Unknown) then
    Exit(False);

  if (FWidth <> Source.Width) or (FHeight <> Source.Height) then
  begin
    if (FWidth > 0) and (FHeight > 0) then
      NewPixelFormat := FPixelFormat
    else
      NewPixelFormat := TPixelFormat.Unknown;

    if NewPixelFormat = TPixelFormat.Unknown then
      NewPixelFormat := Source.PixelFormat;

    if not SetSize(Source.Width, Source.Height, NewPixelFormat) then
      Exit(False);

    if (FWidth <> Source.Width) or (FHeight <> Source.Height) then
      Exit(False);
  end;

  if (FPixelFormat = TPixelFormat.Unknown) or (FBits = nil) then
    Exit(False);

  if FPixelFormat = Source.PixelFormat then
  begin
    for I := 0 to FHeight - 1 do
      Move(Source.Scanline[I]^, GetScanline(I)^, FWidth * FBytesPerPixel);
  end
  else
    CopyRect(ZeroPoint2px, Source, IntRect(0, 0, Source.Width, Source.Height));

  FPremultipliedAlpha := Source.PremultipliedAlpha;
  Result := True;
end;

function TPixelSurface.CopyRect(DestPos: TPoint2px; const Source: TPixelSurface; SourceRect: TIntRect): Boolean;
var
  I: Integer;
  TempBits: Pointer;
begin
  if (FBytesPerPixel <= 0) or (FPixelFormat = TPixelFormat.Unknown) or (Source = nil) or
    (Source.PixelFormat = TPixelFormat.Unknown) then
    Exit(False);

  if SourceRect.Empty then
    SourceRect := IntRect(ZeroPoint2px, Source.Size);

  if not ClipCoords(Source.Size, GetSize, SourceRect, DestPos) then
    Exit(False);

  if FPixelFormat = Source.PixelFormat then
  begin // Native Pixel Format, no conversion
    for I := 0 to SourceRect.Height - 1 do
      Move(
        Source.GetPixelPtr(SourceRect.Left, SourceRect.Top + I)^,
        GetPixelPtr(DestPos.X, DestPos.Y + I)^,
        SourceRect.Width * FBytesPerPixel);
  end
  else if FPixelFormat = TPixelFormat.A8R8G8B8 then
  begin // Source Custom Format to Current 32-bit RGBA
    for I := 0 to SourceRect.Height - 1 do
      PixelXTo32Array(
        Source.GetPixelPtr(SourceRect.Left, SourceRect.Top + I),
        GetPixelPtr(DestPos.X, DestPos.Y + I),
        Source.PixelFormat, SourceRect.Width);
  end
  else if Source.PixelFormat = TPixelFormat.A8R8G8B8 then
  begin // Source 32-bit RGBA to Current Custom Format
    for I := 0 to SourceRect.Height - 1 do
      Pixel32toXArray(
        Source.GetPixelPtr(SourceRect.Left, SourceRect.Top + I),
        GetPixelPtr(DestPos.X, DestPos.Y + I),
        FPixelFormat, SourceRect.Width);
  end
  else
  begin // Source Custom Format to Current Custom Format
    GetMem(TempBits, SourceRect.Width * SizeOf(TIntColor));
    try
      for I := 0 to SourceRect.Height - 1 do
      begin
        PixelXTo32Array(Source.GetPixelPtr(
          SourceRect.Left, SourceRect.Top + I), TempBits,
          Source.PixelFormat, SourceRect.Width);

        Pixel32toXArray(
          TempBits, GetPixelPtr(DestPos.X, DestPos.Y + I),
          FPixelFormat, SourceRect.Width);
      end;
    finally
      FreeMem(TempBits);
    end;
  end;

  Result := True;
end;

procedure TPixelSurface.DrawSurface(DestPos: TPoint2px; const Source: TPixelSurface; SourceRect: TIntRect;
  const Colors: TIntColor4);
var
  I, J, X, Y, GradientVertDiv, GradientHorizDiv, Alpha: Integer;
  SourceColor, GradientLeft, GradientRight, GradientColor: TIntColor;
begin
  if (FBytesPerPixel <= 0) or (FPixelFormat = TPixelFormat.Unknown) or (Source = nil) or
    (Source.PixelFormat = TPixelFormat.Unknown) then
    Exit;

  if SourceRect.Empty then
    SourceRect := IntRect(ZeroPoint2px, Source.Size);

  if not ClipCoords(Source.Size, GetSize, SourceRect, DestPos) then
    Exit;

  GradientHorizDiv := Max(SourceRect.Width - 1, 1);
  GradientVertDiv := Max(SourceRect.Height - 1, 1);

  for J := 0 to SourceRect.Height - 1 do
  begin
    Alpha := (J * 255) div GradientVertDiv;
    GradientLeft := BlendPixels(Colors.TopLeft, Colors.BottomLeft, Alpha);
    GradientRight := BlendPixels(Colors.TopRight, Colors.BottomRight, Alpha);

    for I := 0 to SourceRect.Width - 1 do
    begin
      Alpha := (I * 255) div GradientHorizDiv;
      GradientColor := BlendPixels(GradientLeft, GradientRight, Alpha);

      SourceColor := MultiplyPixels(Source.Pixels[SourceRect.Left + I, SourceRect.Top + J], GradientColor);

      X := DestPos.X + I;
      Y := DestPos.Y + J;

      SetPixel(X, Y, BlendPixels(GetPixel(X, Y), SourceColor, TIntColorRec(SourceColor).Alpha));
    end;
  end;
end;

procedure TPixelSurface.DrawFilledRect(DestRect: TIntRect; const Colors: TIntColor4);
var
  I, J, X, Y, GradientVertDiv, GradientHorizDiv, Alpha: Integer;
  GradientLeft, GradientRight, GradientColor: TIntColor;
begin
  if Self.IsEmpty then
    Exit;

  if DestRect.Empty then
    DestRect := IntRect(0, 0, FWidth, FHeight);

  GradientHorizDiv := Max(DestRect.Width - 1, 1);
  GradientVertDiv := Max(DestRect.Height - 1, 1);

  for J := 0 to DestRect.Height - 1 do
  begin
    Alpha := (J * 255) div GradientVertDiv;
    GradientLeft := BlendPixels(Colors.TopLeft, Colors.BottomLeft, Alpha);
    GradientRight := BlendPixels(Colors.TopRight, Colors.BottomRight, Alpha);

    for I := 0 to DestRect.Width - 1 do
    begin
      Alpha := (I * 255) div GradientHorizDiv;
      GradientColor := BlendPixels(GradientLeft, GradientRight, Alpha);

      X := DestRect.Left + I;
      Y := DestRect.Top + J;

      SetPixel(X, Y, BlendPixels(GetPixel(X, Y), GradientColor, TIntColorRec(GradientColor).Alpha));
    end;
  end;
end;

procedure TPixelSurface.DrawPixel(const X, Y: Integer; const Color: TIntColor);
begin
  if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
    SetPixelUnsafe(X, Y, BlendPixels(GetPixelUnsafe(X, Y), Color, TIntColorRec(Color).Alpha));
end;

procedure TPixelSurface.FillRect(Rect: TIntRect; const Color: TIntColor);
var
  I, J: Integer;
  DestPixel, ColorBits: Pointer;
begin
  if IsEmpty or (FBytesPerPixel <= 0) then
    Exit;

  if Rect.Left < 0 then
    Rect.Left := 0;

  if Rect.Top < 0 then
    Rect.Top := 0;

  if Rect.Right > FWidth then
    Rect.Right := FWidth;

  if Rect.Bottom > FHeight then
    Rect.Bottom := FHeight;

  if Rect.Empty then
    Exit;

  GetMem(ColorBits, FBytesPerPixel);
  try
    Pixel32ToX(Color, ColorBits, FPixelFormat);

    for J := 0 to Rect.Height - 1 do
    begin
      DestPixel := GetPixelAddress(Rect.Left, Rect.Top + J);

      for I := 0 to Rect.Width - 1 do
      begin
        Move(ColorBits^, DestPixel^, FBytesPerPixel);
        Inc(PtrUInt(DestPixel), FBytesPerPixel);
      end;
    end;
  finally
    FreeMem(ColorBits);
  end;
end;

procedure TPixelSurface.FillRect(const X, Y, Width, Height: Integer; const Color: TIntColor);
begin
  FillRect(IntRect(X, Y, Width, Height), Color);
end;

procedure TPixelSurface.HorizLine(X, Y, LineWidth: Integer; const Color: TIntColor);
var
  I: Integer;
  DestPixel, ColorBits: Pointer;
begin
  if (Y < 0) or (Y >= FHeight) then
    Exit;

  if X < 0 then
  begin
    Inc(LineWidth, X);
    X := 0;
  end;

  if X + LineWidth > FWidth then
    LineWidth := FWidth - X;

  if LineWidth <= 0 then
    Exit;

  DestPixel := GetPixelAddress(X, Y);

  GetMem(ColorBits, FBytesPerPixel);
  try
    Pixel32ToX(Color, ColorBits, FPixelFormat);

    for I := 0 to LineWidth - 1 do
    begin
      Move(ColorBits^, DestPixel^, FBytesPerPixel);
      Inc(PtrUInt(DestPixel), FBytesPerPixel);
    end;
  finally
    FreeMem(ColorBits);
  end;
end;

procedure TPixelSurface.VertLine(X, Y, LineHeight: Integer; const Color: TIntColor);
var
  I: Integer;
  DestPixel, ColorBits: Pointer;
begin
  if (X < 0) or (X >= FWidth) then
    Exit;

  if Y < 0 then
  begin
    Inc(LineHeight, Y);
    Y := 0;
  end;

  if Y + LineHeight > FHeight then
    LineHeight := FHeight - Y;

  if LineHeight < 1 then
    Exit;

  DestPixel := GetPixelAddress(X, Y);

  GetMem(ColorBits, FBytesPerPixel);
  try
    Pixel32ToX(Color, ColorBits, FPixelFormat);

    for I := 0 to LineHeight - 1 do
    begin
      Move(ColorBits^, DestPixel^, FBytesPerPixel);
      Inc(PtrUInt(DestPixel), FPitch);
    end;
  finally
    FreeMem(ColorBits);
  end;
end;

procedure TPixelSurface.FrameRect(const Rect: TIntRect; const Color: TIntColor);
begin
  HorizLine(Rect.Left, Rect.Top, Rect.Width, Color);

  if Rect.Bottom > Rect.Top + 1 then
    HorizLine(Rect.Left, Rect.Bottom - 1, Rect.Width, Color);

  if Rect.Height > 2 then
  begin
    VertLine(Rect.Left, Rect.Top + 1, Rect.Height - 2, Color);

    if Rect.Right > Rect.Left + 1 then
      VertLine(Rect.Right - 1, Rect.Top + 1, Rect.Height - 2, Color);
  end;
end;

procedure TPixelSurface.FrameRect(const X, Y, Width, Height: Integer; const Color: TIntColor);
begin
  FrameRect(IntRect(X, Y, Width, Height), Color);
end;

procedure TPixelSurface.Clear(const Color: TIntColor);
var
  I, J: Integer;
  DestPixel, ColorBits: Pointer;
begin
  if IsEmpty or (FBytesPerPixel <= 0) then
    Exit;

  GetMem(ColorBits, FBytesPerPixel);
  try
    Pixel32ToX(Color, ColorBits, FPixelFormat);

    for J := 0 to FHeight - 1 do
    begin
      DestPixel := GetScanline(J);

      for I := 0 to FWidth - 1 do
      begin
        Move(ColorBits^, DestPixel^, FBytesPerPixel);
        Inc(PtrUInt(DestPixel), FBytesPerPixel);
      end;
    end;
  finally
    FreeMem(ColorBits);
  end;
end;

procedure TPixelSurface.Clear;
var
  I: Integer;
begin
  if IsEmpty or (FBytesPerPixel <= 0) then
    Exit;

  for I := 0 to FHeight - 1 do
    FillChar(GetScanline(I)^, FWidth * FBytesPerPixel, 0);
end;

procedure TPixelSurface.ResetAlpha(const Opaque: Boolean);
var
  I, J: Integer;
  DestPixel: Pointer;
begin
  if IsEmpty then
    Exit;

  for J := 0 to FHeight - 1 do
  begin
    DestPixel := GetScanline(J);

    if Opaque then
      for I := 0 to FWidth - 1 do
      begin
        Pixel32ToX(PixelXTo32(DestPixel, FPixelFormat) or $FF000000, DestPixel, FPixelFormat);
        Inc(PtrUInt(DestPixel), FBytesPerPixel);
      end
    else
      for I := 0 to FWidth - 1 do
      begin
        Pixel32ToX(PixelXTo32(DestPixel, FPixelFormat) and $FFFFFF, DestPixel, FPixelFormat);
        Inc(PtrUInt(DestPixel), FBytesPerPixel);
      end;
  end;
end;

function TPixelSurface.HasAlphaChannel: Boolean;
var
  I, J: Integer;
  SrcPixel: Pointer;
  Color: TIntColor;
  HasNonZero, HasNonMax: Boolean;
begin
  if IsEmpty then
    Exit(False);

  HasNonZero := False;
  HasNonMax := False;

  for J := 0 to FHeight - 1 do
  begin
    SrcPixel := GetScanline(J);

    for I := 0 to FWidth - 1 do
    begin
      Color := PixelXTo32(SrcPixel, FPixelFormat);

      if (not HasNonZero) and (Color shr 24 > 0) then
        HasNonZero := True;

      if (not HasNonMax) and (Color shr 24 < 255) then
        HasNonMax := True;

      if HasNonZero and HasNonMax then
        Exit(True);

      Inc(PtrUInt(SrcPixel), FBytesPerPixel);
    end;
  end;

  Result := False;
end;

procedure TPixelSurface.PremultiplyAlpha;
var
  I, J: Integer;
  DestPixel: Pointer;
begin
  if IsEmpty then
    Exit;

  for J := 0 to FHeight - 1 do
  begin
    DestPixel := GetScanline(J);

    for I := 0 to FWidth - 1 do
    begin
      Pixel32ToX(PXL.Types.PremultiplyAlpha(PixelXTo32(DestPixel, FPixelFormat)), DestPixel, FPixelFormat);
      Inc(PtrUInt(DestPixel), FBytesPerPixel);
    end;
  end;
end;

procedure TPixelSurface.UnpremultiplyAlpha;
var
  I, J: Integer;
  DestPixel: Pointer;
begin
  if IsEmpty then
    Exit;

  for J := 0 to FHeight - 1 do
  begin
    DestPixel := GetScanline(J);

    for I := 0 to FWidth - 1 do
    begin
      Pixel32ToX(PXL.Types.UnpremultiplyAlpha(PixelXTo32(DestPixel, FPixelFormat)), DestPixel, FPixelFormat);
      Inc(PtrUInt(DestPixel), FBytesPerPixel);
    end;
  end;
end;

procedure TPixelSurface.Mirror;
var
  I, J: Integer;
  CopyBits, DestPixel, SourcePixel: Pointer;
  CopyWidth: Cardinal;
begin
  if IsEmpty then
    Exit;

  CopyWidth := Cardinal(FWidth) * FBytesPerPixel;

  GetMem(CopyBits, CopyWidth);
  try
    for J := 0 to FHeight - 1 do
    begin
      Move(GetScanline(J)^, CopyBits^, CopyWidth);

      DestPixel := CopyBits;
      SourcePixel := Pointer((PtrUInt(GetScanline(J)) + CopyWidth) - FBytesPerPixel);

      for I := 0 to FWidth - 1 do
      begin
        Move(SourcePixel^, DestPixel^, FBytesPerPixel);

        Dec(PtrUInt(SourcePixel), FBytesPerPixel);
        Inc(PtrUInt(DestPixel), FBytesPerPixel);
      end;

      Move(CopyBits^, GetScanline(J)^, CopyWidth);
    end;
  finally
    FreeMem(CopyBits);
  end;
end;

procedure TPixelSurface.Flip;
var
  I, J: Integer;
  CopyBits: Pointer;
  CopyWidth: Cardinal;
begin
  if IsEmpty then
    Exit;

  CopyWidth := Cardinal(FWidth) * FBytesPerPixel;

  GetMem(CopyBits, CopyWidth);
  try
    for I := 0 to (FHeight div 2) - 1 do
    begin
      J := (FHeight - 1) - I;

      Move(GetScanline(I)^, CopyBits^, CopyWidth);
      Move(GetScanline(J)^, GetScanline(I)^, CopyWidth);
      Move(CopyBits^, GetScanline(J)^, CopyWidth);
    end;
  finally
    FreeMem(CopyBits);
  end;
end;

function TPixelSurface.ShrinkToHalfFrom(const Source: TPixelSurface): Boolean;
var
  I, J: Integer;
  NewSize: TPoint2px;
begin
  NewSize.X := Max(Source.Width div 2, 1);
  NewSize.Y := Max(Source.Height div 2, 1);

  if ((NewSize.X >= Source.Width) and (NewSize.Y >= Source.Height)) or (Source.PixelFormat = TPixelFormat.Unknown) then
    Exit(False);

  if not SetSize(NewSize, Source.PixelFormat) then
    Exit(False);

  if Source.Height < 2 then
  begin // horizontal shrink
    for I := 0 to FWidth - 1 do
      SetPixel(I, 0, AveragePixels(Source.GetPixel(I * 2, 0), Source.GetPixel((I * 2) + 1, 0)));
  end
  else if Source.Width < 2 then
  begin // vertical shrink
    for J := 0 to FHeight - 1 do
      SetPixel(0, J, AveragePixels(Source.GetPixel(0, J * 2), Source.GetPixel(0, (J * 2) + 1)));
  end
  else
  begin // full shrink
    for J := 0 to FHeight - 1 do
      for I := 0 to FWidth - 1 do
      begin
        SetPixel(I, J, AverageFourPixels(Source.GetPixel(I * 2, J * 2), Source.GetPixel((I * 2) + 1, J * 2),
          Source.GetPixel(I * 2, (J * 2) + 1), Source.GetPixel((I * 2) + 1, (J * 2) + 1)));
      end;
  end;

  Result := True;
end;

function TPixelSurface.ComposeWhiteBlackToGrayAlpha(const SourceWhite, SourceBlack: TPixelSurface): Boolean;
var
  NewPixelFormat: TPixelFormat;
  SourceWhitePixel, SourceBlackPixel, DestPixel: Pointer;
  I, J: Integer;
begin
  if (SourceWhite.Width <= 0) or (SourceWhite.Height <= 0) or (SourceWhite.Width <> SourceBlack.Width) or
    (SourceWhite.Height <> SourceBlack.Height) then
    Exit(False);

  if (FWidth <> SourceWhite.Width) or (FHeight <> SourceWhite.Height) then
  begin
    NewPixelFormat := FPixelFormat;
    if NewPixelFormat = TPixelFormat.Unknown then
      NewPixelFormat := TPixelFormat.A8L8;

    if not SetSize(SourceWhite.Size, NewPixelFormat) then
      Exit(False);
  end;

  for J := 0 to FHeight - 1 do
  begin
    SourceWhitePixel := SourceWhite.GetScanline(J);
    SourceBlackPixel := SourceBlack.GetScanline(J);
    DestPixel := GetScanline(J);

    for I := 0 to FWidth - 1 do
    begin
      Pixel32ToX(
        IntGrayToColor(
          FloatToIntGrayAlpha(
            ExtractGrayAlpha(
              PixelToGrayFloat(PixelXTo32(SourceBlackPixel, SourceWhite.PixelFormat)),
              PixelToGrayFloat(PixelXTo32(SourceWhitePixel, SourceBlack.PixelFormat)), 0.0, 1.0))),
        DestPixel, FPixelFormat);

      Inc(PtrUInt(SourceWhitePixel), SourceWhite.BytesPerPixel);
      Inc(PtrUInt(SourceBlackPixel), SourceBlack.BytesPerPixel);
      Inc(PtrUInt(DestPixel), FBytesPerPixel);
    end;
  end;

  Result := True;
end;

function TPixelSurface.GetBilinearPixel(const X, Y, FracX, FracY: Integer): TIntColor;
var
  NextX, NextY: Integer;
begin
  NextX := Min(X + 1, FWidth);
  NextY := Min(Y + 1, FHeight);

  Result := BlendFourPixels(GetPixel(X, Y), GetPixel(NextX, Y), GetPixel(NextX, NextY), GetPixel(X, NextY),
    FracX, FracY);
end;

function TPixelSurface.GetBilinearPixel(const X, Y: VectorFloat): TIntColor;
var
  CurX, CurY, NextX, NextY: Integer;
begin
  CurX := Trunc(X);
  CurY := Trunc(Y);
  NextX := Min(CurX + 1, FWidth);
  NextY := Min(CurY + 1, FHeight);

  Result := LerpFourPixels(GetPixel(CurX, CurY), GetPixel(NextX, CurY), GetPixel(NextX, NextY), GetPixel(CurX, NextY),
    Frac(CurX), Frac(CurY));
end;

function TPixelSurface.GetPixelWithCheck(X, Y: Integer): TIntColor;
begin
  if X < 0 then
    X := 0
  else if X >= FWidth then
    X := FWidth - 1;

  if Y < 0 then
    Y := 0
  else if Y >= FHeight then
    Y := FHeight - 1;

  Result := GetPixel(X, Y);
end;

procedure TPixelSurface.StretchFrom(const Source: TPixelSurface; DestRect, SourceRect: TFloatRect);
var
  I, J: Integer;
  Position, Velocity: TPoint2px;
  IntSourceRect, IntDestRect: TIntRect;
begin
  if IsEmpty then
    Exit;

  if SourceRect.Empty then
    SourceRect := FloatRect(0.0, 0.0, Source.Width, Source.Height);

  if DestRect.Empty then
    DestRect := FloatRect(0, 0, FWidth, FHeight);

  if not ClipCoords(Source.Size, GetSize, SourceRect, DestRect) then
    Exit;

  IntSourceRect := SourceRect.ToIntRect;
  IntDestRect := DestRect.ToIntRect;

  if (IntSourceRect.Width <= 0) or (IntSourceRect.Height <= 0) or (IntDestRect.Width <= 0) or
    (IntDestRect.Height <= 0) then
    Exit;

  Position.Y := IntSourceRect.Top * 65536;
  Velocity.X := (Int64(IntSourceRect.Width) * 65536) div IntDestRect.Width;
  Velocity.Y := (Int64(IntSourceRect.Height) * 65536) div IntDestRect.Height;

  for J := 0 to IntDestRect.Height - 1 do
  begin
    Position.X := IntSourceRect.Left * 65536;

    for I := 0 to IntDestRect.Width - 1 do
    begin
      SetPixel(
        IntDestRect.Left + I, IntDestRect.Top + J,
        Source.GetPixelWithCheck(Position.X div 65536, Position.Y div 65536));

      Inc(Position.X, Velocity.X);
    end;

    Inc(Position.Y, Velocity.Y);
  end;
end;

procedure TPixelSurface.StretchBilinearFrom(const Source: TPixelSurface; DestRect, SourceRect: TFloatRect);
var
  I, J: Integer;
  Position, Velocity: TPoint2px;
  IntSourceRect, IntDestRect: TIntRect;
begin
  if IsEmpty then
    Exit;

  if SourceRect.Empty then
    SourceRect := FloatRect(0.0, 0.0, Source.Width, Source.Height);

  if DestRect.Empty then
    DestRect := FloatRect(0, 0, FWidth, FHeight);

  if not ClipCoords(Source.Size, GetSize, SourceRect, DestRect) then
    Exit;

  IntSourceRect := SourceRect.ToIntRect;
  IntDestRect := DestRect.ToIntRect;

  if (IntSourceRect.Width <= 0) or (IntSourceRect.Height <= 0) or (IntDestRect.Width <= 0) or
    (IntDestRect.Height <= 0) then
    Exit;

  Position.Y := IntSourceRect.Top * 65536;
  Velocity.X := (Int64(IntSourceRect.Width) * 65536) div IntDestRect.Width;
  Velocity.Y := (Int64(IntSourceRect.Height) * 65536) div IntDestRect.Height;

  for J := 0 to IntDestRect.Height - 1 do
  begin
    Position.X := IntSourceRect.Left * 65536;

    for I := 0 to IntDestRect.Width - 1 do
    begin
      SetPixel(
        IntDestRect.Left + I, IntDestRect.Top + J,
        BlendFourPixels(
          Source.GetPixelWithCheck(Position.X div 65536, Position.Y div 65536),
          Source.GetPixelWithCheck((Position.X div 65536) + 1, Position.Y div 65536),
          Source.GetPixelWithCheck((Position.X div 65536) + 1, (Position.Y div 65536) + 1),
          Source.GetPixelWithCheck((Position.X div 65536), (Position.Y div 65536) + 1),
          (Position.X and $FFFF) shr 8, (Position.Y and $FFFF) shr 8));

      Inc(Position.X, Velocity.X);
    end;

    Inc(Position.Y, Velocity.Y);
  end;
end;

{$ENDREGION}
{$REGION 'TPixelMipMapSurface'}

constructor TPixelMipMapSurface.Create;
begin
  inherited;

  FMipMaps := CreatePixelSurfaces;
end;

function TPixelMipMapSurface.CreatePixelSurfaces: TPixelSurfaces;
begin
  Result := TPixelSurfaces.Create;
end;

destructor TPixelMipMapSurface.Destroy;
begin
  FMipMaps.Free;

  inherited;
end;

procedure TPixelMipMapSurface.GenerateMipMaps;
var
  Source, Dest: TPixelSurface;
  NewIndex: Integer;
begin
  FMipMaps.Clear;

  Source := Self;
  while ((Source.Width > 1) or (Source.Height > 1)) and (Source.PixelFormat <> TPixelFormat.Unknown) do
  begin
    NewIndex := FMipMaps.Add;

    Dest := FMipMaps[NewIndex];
    if Dest = nil then
      Break;

    Dest.ShrinkToHalfFrom(Source);
    Source := Dest;
  end;
end;

procedure TPixelMipMapSurface.ClearMipMaps;
begin
  FMipMaps.Clear;
end;

{$ENDREGION}
{$REGION 'TPixelSurfaces'}

constructor TPixelSurfaces.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;

  SetLength(FData, 0);
  FSearchListDirty := True;
end;

destructor TPixelSurfaces.Destroy;
begin
  try
    Clear;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TPixelSurfaces.GetCount: Integer;
begin
  Result := FDataCount;
end;

procedure TPixelSurfaces.Request(const NeedCapacity: Integer);
var
  NewCapacity, Capacity, I: Integer;
begin
  if NeedCapacity < 1 then
    Exit;

  Capacity := Length(FData);

  if Capacity < NeedCapacity then
  begin
    NewCapacity := ListGrowIncrement + Capacity + (Capacity div ListGrowFraction);

    if NewCapacity < NeedCapacity then
      NewCapacity := ListGrowIncrement + NeedCapacity + (NeedCapacity div ListGrowFraction);

    SetLength(FData, NewCapacity);

    for I := Capacity to NewCapacity - 1 do
      FData[I] := nil;
  end;
end;

procedure TPixelSurfaces.SetCount(const NewCount: Integer);
var
  I: Integer;
begin
  if NewCount <> FDataCount then
  begin
    if NewCount > 0 then
    begin
      Request(NewCount);

      for I := FDataCount to NewCount - 1 do
        FData[I] := TPixelSurface.Create;

      FDataCount := NewCount;
      FSearchListDirty := True;
    end
    else
      Clear;
  end;
end;

function TPixelSurfaces.GetItem(const Index: Integer): TPixelSurface;
begin
  if (Index >= 0) and (Index < FDataCount) then
    Result := FData[Index]
  else
    Result := nil;
end;

function TPixelSurfaces.FindEmptySlot: Integer;
var
  I: Integer;
begin
  for I := 0 to FDataCount - 1 do
    if FData[I] = nil then
      Exit(I);

  Result := -1;
end;

function TPixelSurfaces.Insert(const Surface: TPixelSurface): Integer;
begin
  Result := FindEmptySlot;
  if Result = -1 then
  begin
    Result := FDataCount;
    Request(FDataCount + 1);
    Inc(FDataCount);
  end;

  FData[Result] := Surface;
  FSearchListDirty := True;
end;

function TPixelSurfaces.CreatePixelSurface(const SurfaceName: StdString): TPixelSurface;
begin
  Result := TPixelSurface.CreateNamed(SurfaceName);
end;

function TPixelSurfaces.Add(const SurfaceName: StdString): Integer;
begin
  Result := Insert(CreatePixelSurface(SurfaceName));
end;

procedure TPixelSurfaces.Remove(const Index: Integer);
begin
  if (Index < 0) or (Index >= FDataCount) then
    Exit;

  FreeAndNil(FData[Index]);
  FSearchListDirty := True;
end;

procedure TPixelSurfaces.Clear;
var
  I: Integer;
begin
  for I := FDataCount - 1 downto 0 do
    FreeAndNil(FData[I]);

  FDataCount := 0;
  FSearchListDirty := True;
end;

procedure TPixelSurfaces.InitSearchList;
var
  I, ObjCount, Index: Integer;
begin
  ObjCount := 0;

  for I := 0 to FDataCount - 1 do
    if FData[I] <> nil then
      Inc(ObjCount);

  if Length(FSearchList) <> ObjCount then
    SetLength(FSearchList, ObjCount);

  Index := 0;

  for I := 0 to FDataCount - 1 do
    if FData[I] <> nil then
    begin
      FSearchList[Index] := I;
      Inc(Index);
    end;
end;

procedure TPixelSurfaces.SearchListSwap(const Index1, Index2: Integer);
var
  TempValue: Integer;
begin
  TempValue := FSearchList[Index1];
  FSearchList[Index1] := FSearchList[Index2];
  FSearchList[Index2] := TempValue;
end;

function TPixelSurfaces.SearchListCompare(const Index1, Index2: Integer): Integer;
begin
  Result := CompareText(FData[Index1].Name, FData[Index2].Name);
end;

function TPixelSurfaces.SearchListSplit(const Start, Stop: Integer): Integer;
var
  Left, Right, Pivot: Integer;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := FSearchList[Start];

  while Left <= Right do
  begin
    while (Left <= Stop) and (SearchListCompare(FSearchList[Left], Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (SearchListCompare(FSearchList[Right], Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      SearchListSwap(Left, Right);
  end;

  SearchListSwap(Start, Right);
  Result := Right;
end;

procedure TPixelSurfaces.SearchListSort(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := SearchListSplit(Start, Stop);

    SearchListSort(Start, SplitPt - 1);
    SearchListSort(SplitPt + 1, Stop);
  end;
end;

procedure TPixelSurfaces.UpdateSearchList;
begin
  InitSearchList;

  if Length(FSearchList) > 1 then
    SearchListSort(0, Length(FSearchList) - 1);

  FSearchListDirty := False;
end;

function TPixelSurfaces.IndexOf(const SurfaceName: StdString): Integer;
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
    Res := CompareText(FData[FSearchList[Pivot]].Name, SurfaceName);

    if Res = 0 then
      Exit(FSearchList[Pivot]);

    if Res > 0 then
      Right := Pivot - 1
    else
      Left := Pivot + 1;
  end;

  Result := -1;
end;

function TPixelSurfaces.GetSurface(const Name: StdString): TPixelSurface;
begin
  Result := GetItem(IndexOf(Name));
end;

{$ENDREGION}

end.
