unit PXL.Types;
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
{< Essential types, constants and functions working with vectors, colors, pixels and rectangles that are used
   throughout the entire framework. }

//The x64 Application Binary Interface (ABI) is a 4 register fast-call calling
//convention, with stack-backing for those registers. There is a strict
//one-to-one correspondence between arguments in a function, and the
//registers for those arguments. Any argument that doesn’t fit in 8 bytes,
//or is not 1, 2, 4, or 8 bytes, must be passed by reference.
//There is no attempt to spread a single argument across multiple
//registers. The x87 register stack is unused. It may be used, but must
//be considered volatile across function calls. All floating point
//operations are done using the 16 XMM registers. The arguments are
//passed in registers RCX, RDX, R8, and R9. If the arguments are
//float/double, they are passed in XMM0L, XMM1L, XMM2L, and XMM3L.
//16 byte arguments are passed by reference. Parameter passing is
//described in detail in Parameter Passing. In addition to these
//registers, RAX, R10, R11, XMM4, and XMM5 are volatile. All other
//registers are non-volatile. Register usage is documented in
//detail in Register Usage and Caller/Callee Saved Registers.

//Basically this means that you can use these WIHTOUT push
//RAX
//RCX
//RDX
//R8
//R9
//R10
//R11
//XMM0
//XMM1
//XMM2
//XMM3
//XMM4
//XMM5



//The caller is responsible for allocating space for parameters to the callee,
//and must always allocate sufficient space for the 4 register parameters,
//even if the callee doesn’t have that many parameters. This aids in the
//simplicity of supporting C unprototyped functions, and vararg
//C/C++ functions. For vararg or unprototyped functions, any float values
//must be duplicated in the corresponding general-purpose register.
//Any parameters above the first 4 must be stored on the stack,
//above the backing store for the first 4, prior to the call. Vararg function details can be found in Varargs. Unprototyped function information is detailed in Unprototyped Functions.
interface

{$INCLUDE PXL.Config.inc}
{$IFNDEF CPUX64}
{$DEFINE PURE_PASCAL}
{$DEFINE PURE_MATRIX_MULTIPLY}
{$ENDIF}
{$DEFINE PURE_MATRIX_MULTIPLY}
{$DEFINE PURE_PASCAL}


uses
  PXL.TypeDef, typex, geometry, types, sysutils, stringx, numbers;


{$REGION 'Basic Types'}

type
  { This type is used to pass @link(TPixelFormat) by reference. }
  PPixelFormat = ^TPixelFormat;

  { Defines how individual pixels and their colors are encoded in images and textures. The order of letters in the
    constants defines the order of the  encoded components; R stands for Red, G for Green, B for Blue, A for Alpha,
    L for Luminance and X for Not Used (or discarded); F at the end means floating-point format. }
  TPixelFormat = (

    { Unknown pixel format. It is usually returned when no valid pixel format is available. In some cases, it can be
      specified to indicate that the format should be selected by default or automatically. @br @br }
    Unknown,

    { 32-bit RGBA pixel format. The most commonly used pixel format for storing and loading textures and
      images. @br @br }
    A8R8G8B8,

    { 32-bit RGB pixel format that has no alpha-channel. Should be used for images and textures that have no
      transparency information in them. @br @br }
    X8R8G8B8,

    { 16-bit RGBA pixel format with 4 bits for each channel. This format can be used as a replacement for
      @italic(A8R8G8B8) format in cases where memory footprint is important at the expense of visual quality. @br @br }
    A4R4G4B4,

    { 16-bit RGB pixel format with 4 bits unused. It is basically @italic(A4R4G4B4) with alpha-channel discarded.
      This format is widely supported, but in typical applications it is more convenient to use @italic(R5G6B5)
      instead. @br @br }
    X4R4G4B4,

    { 16-bit RGB pixel format. This format can be used as an alternative to A8R8G8B8 in cases where memory footprint
      is important at the expense of visual quality. @br @br }
    R5G6B5,

    { 16-bit RGBA pixel format with one bit dedicated for alpha-channel. This format can be used for images where a
      transparency mask is used; that is, the pixel is either transparent or not, typical for those images where a
      single color is picked to be transparent. In this product, there is little need for this format because
      @italic(AlphaTool) can be used to generate alpha channel for images with masked color, which then can be used
      with any other higher-quality format. @br @br }
    A1R5G5B5,

    { 16-bit RGB pixel format with only 15 bits used for actual storage. This format was common on legacy hardware
      but recently it is rarely used or even supported. @br @br }
    X1R5G5B5,

    { 8-bit RGBA pixel format that was originally supported by OpenGL in earlier implementations. This format can
      significantly save disk space and memory consumption (if supported in hardware) but at the expense of very low
      visual quality. @br @br }
    A2R2G2B2,

    { 8-bit RGB pixel format. An extreme low-quality format useful only in special circumstances and mainly for storage.
      It is more commonly supported on ATI video cards than on Nvidia, being really scarce on newer hardware. @br @br }
    R3G3B2,

    { 16-bit RGBA pixel format with uneven bit distribution among the components. It is more supported on ATI video
      cards and can be rarely found on newer hardware. In many cases it is more useful to use @italic(A4R4G4B4)
      format. @br @br }
    A8R3G3B2,

    { 32-bit RGBA pixel format with 10 bits used for each component of red, green and blue, being a higher-quality
      variant of @italic(A8R8G8B8). It is more commonly supported on some video cards than its more practical cousin
      @italic(A2R10G10B10). @br @br }
    A2B10G10R10,

    { 64-bit RGBA pixel format with each channel having 16 bits. @br @br }
    A16B16G16R16,

    { 16-bit luminance pixel format. One of the best formats to be used with bitmap fonts, which is also widely
      supported. @br @br }
    A8L8,

    { 8-bit luminance pixel format. This format can be used as a low quality replacement for @italic(A8L8) to
      represent bitmap fonts. @br @br }
    A4L4,

    { 16-bit luminance pixel format that can be used to represent high-quality grayscale images and textures. @br @br }
    L16,

    { 8-bit luminance pixel format. This format can be used for grayscale images and textures. @br @br }
    L8,

    { 16-bit floating-point pixel format, which has only one component. This is useful in shaders either as a render
      target or as a data source. @br @br }
    R16F,

    { 32-bit floating-point pixel format containing two components with 16 bits each. This can be used in shaders as a
      data source. @br @br }
    G16R16F,

    { 64-bit floating-point RGBA pixel format with each component having 16 bits. It can be used as a special purpose
      texture or a render target with shaders. @br @br }
    A16B16G16R16F,

    { 32-bit floating-point pixel format, which has only one component. This format is typically used as render target
      for shadow mapping. @br @br }
    R32F,

    { 64-bit floating-point pixel format containing two components with 32 bits each, mainly useful in shaders as a
      data source. @br @br }
    G32R32F,

    { 128-bit floating-point RGBA pixel format with each component having 32 bits. It can be used as a special purpose
      texture or a render target with shaders. @br @br }
    A32B32G32R32F,

    { 8-bit alpha pixel format. This format can be used as an alpha-channel format for applications that require low
      memory footprint and require transparency information only. Its usefulness, however, is severely limited because
      it is only supported only on newer video cards and when converted in hardware to @italic(A8R8G8B8), it has zero
      values for red, green and blue components; in other words, it is basically a black color that also has alpha
      channel. @br @br }
    A8,

    { 32-bit pixel format that has only green and red components 16 bits each. This format is more useful for shaders
      where only one or two components are needed but with extra resolution. @br @br }
    G16R16,

    { 32-bit RGBA pixel format with 10 bits used for each component of red, green and blue, with only 2 bits dedicated
      to alpha channel. @br @br }
    A2R10G10B10,

    { 32-bit BGRA pixel format. This is similar to @italic(A8R8G8B8) format but with red and blue components
      exchanged. @br @br }
    A8B8G8R8,

    { 32-bit BGR pixel format that has no alpha-channel, similar to @italic(X8R8G8B8) but with red and blue components
      exchanged. @br @br }
    X8B8G8R8,

    { 24-bit RGB pixel format. This format can be used for storage and it is unsuitable for rendering both on
      @italic(DirectX) and @italic(OpenGL). @br @br }
    R8G8B8,

    { 32-bit ABGR pixel format. This format is common to some MSB configurations such as @italic(Apple Carbon)
      interface. @br @br }
    B8G8R8A8,

    { 32-bit BGR pixel format that has no alpha-channel. This format is common to some MSB configurations such as
      the one used by @italic(LCL) in @italic(Apple Carbon) interface. @br @br }
    B8G8R8X8,

    { 8-bit palette indexed format, where each value points to a list of colors, which was popular in DOS
      applications. @br @br }
    I8
  );

  { Pointer to @link(TDepthStencil). }
  PDepthStencil = ^TDepthStencil;

  { Support level for depth and stencil buffers. }
  TDepthStencil = (
    { No depth or stencil buffers should be supported. @br @br }
    None,

    { Depth but not stencil buffers should be supported. @br @br }
    DepthOnly,

    { Both depth and stencil buffers should be supported. }
    Full);

  { Defines how alpha-channel should be handled in the loaded image. }
  TAlphaFormatRequest = (
    { Alpha-channel can be handled either way. @br @br }
    DontCare,

    { Alpha-channel in the image should not be premultiplied. Under normal circumstances, this is the recommended
      approach as it preserves RGB color information in its original form. However, when using mipmapping for images
      that have alpha-channel, @italic(Premultiplied) gives more accurate results. @br @br }
    NonPremultiplied,

    { Alpha-channel in the image should be premultiplied. Under normal circumstances, this is not recommended as the
      image would lose information after RGB components are premultiplied by alpha (and for smaller alpha values,
      less information is preserved). However, when using mipmapping for images that have alpha-channel, this gives
      more accurate results. }
    Premultiplied);

  { Standard notification event used throughout the framework. }
  TStandardNotifyEvent = procedure(const Sender: TObject) of object;

{$ENDREGION}
{$REGION 'TIntGrayAlpha'}

type
  { Pointer to @link(TIntGrayAlphaValue). }
  PIntGrayAlphaValue = ^TIntGrayAlphaValue;

  { Raw (untyped) grayscale value that is represented as a raw 16-bit unsigned integer, where first byte corresponds
    to gray and second one to alpha-channel. }
  TIntGrayAlphaValue = Word;

  { Pointer to @link(TIntGrayAlpha). }
  PIntGrayAlpha = ^TIntGrayAlpha;

  { General-purpose grayscale value that is represented as 16-bit unsigned integer, where first byte corresponds to
    gray and second one to alpha-channel. }
  TIntGrayAlpha = Word;

  { Pointer to @link(TIntGrayAlphaRec). }
  PIntGrayAlphaRec = ^TIntGrayAlphaRec;

  { Alternative representation of @link(TIntGrayAlpha), where each element can be accessed as an individual value.
    This can be safely typecast to @link(TIntGrayAlpha) and vice-versa. }
  TIntGrayAlphaRec = record
    case Word of
      0:
        (
          { Gray value ranging from 0 (black) to 255 (white). }
          Gray: Byte;

          { Alpha-channel value ranging from 0 (translucent) to 255 (opaque). }
          Alpha: Byte;
        );
      1:
        { Values represented as an array, where gray has index of 0 and alpha-channel index of 1. }
        (Values: packed array[0..1] of Byte;);
  end;

const
  { Predefined constant for opaque Black color. }
  IntGrayAlphaBlack = $FF00;

  { Predefined constant for opaque White color. }
  IntGrayAlphaWhite = $FFFF;

  { Predefined constant for translucent Black color. }
  IntGrayAlphaTranslucentBlack = $0000;

  { Predefined constant for translucent White color. }
  IntGrayAlphaTranslucentWhite = $00FF;

{ Creates 16-bit grayscale color using its individual components. }
function IntGrayAlpha(const Gray: Integer; const Alpha: Integer = 255): TIntGrayAlpha; overload;

{ Creates 16-bit grayscale color using its individual components (both multiplied by 255). }
function IntGrayAlpha(const Gray: VectorFloat; const Alpha: VectorFloat = 1.0): TIntGrayAlpha; overload; inline;

{ Takes 16-bit grayscale color with unpremultiplied alpha and multiplies gray value by its alpha-channel, resulting in
  premultiplied grayscale color. }
function PremultiplyGrayPixel(const Color: TIntGrayAlpha): TIntGrayAlpha;

{ Takes 16-bit grayscale color with premultiplied alpha-channel and divides its gray value by alpha, resulting in
  unpremultiplied grayscale color. }
function UnpremultiplyGrayPixel(const Color: TIntGrayAlpha): TIntGrayAlpha;

{ Adds two 16-bit grayscale colors together clamping the resulting values if necessary. }
function AddGrayPixels(const Color1, Color2: TIntGrayAlpha): TIntGrayAlpha;

{ Subtracts two 16-bit grayscale colors clamping the resulting values if necessary. }
function SubtractGrayPixels(const Color1, Color2: TIntGrayAlpha): TIntGrayAlpha;

{ Multiplies two 16-bit grayscale colors together. }
function MultiplyGrayPixels(const Color1, Color2: TIntGrayAlpha): TIntGrayAlpha;

{ Computes average of two given 16-bit grayscale colors. }
function AverageGrayPixels(const Color1, Color2: TIntGrayAlpha): TIntGrayAlpha;

{ Computes alpha-blending for a pair of 16-bit grayscale colors.
    @italic(Alpha) can be in [0..255] range. }
function BlendGrayPixels(const Color1, Color2: TIntGrayAlpha; const Alpha: Integer): TIntGrayAlpha;

{ Computes alpha-blending for a pair of 16-bit grayscale colors using floating-point approach. For a faster alternative,
  use @link(BlendGrayPixels).
    @italic(Alpha) can be in [0..1] range.  }
function LerpGrayPixels(const Color1, Color2: TIntGrayAlpha; const Alpha: VectorFloat): TIntGrayAlpha;

{$ENDREGION}
{$REGION 'TFloatGrayAlpha'}
type
  { Pointer to @link(TFloatGrayAlpha). }
  PFloatGrayAlpha = ^TFloatGrayAlpha;

  { Grayscale floating-point color representation, where each of the components range from 0 to 1. Values outside
    of this range are also supported, but they cannot be reliably displayed on the screen. }
  TFloatGrayAlpha = record
  public
    { @exclude } class operator Add(const Color1, Color2: TFloatGrayAlpha): TFloatGrayAlpha;
    { @exclude } class operator Subtract(const Color1, Color2: TFloatGrayAlpha): TFloatGrayAlpha;
    { @exclude } class operator Multiply(const Color1, Color2: TFloatGrayAlpha): TFloatGrayAlpha;
    { @exclude } class operator Divide(const Color1, Color2: TFloatGrayAlpha): TFloatGrayAlpha;
    { @exclude } class operator Multiply(const Color: TFloatGrayAlpha; const Theta: VectorFloat): TFloatGrayAlpha;
    { @exclude } class operator Multiply(const Theta: VectorFloat; const Color: TFloatGrayAlpha): TFloatGrayAlpha;
    { @exclude } class operator Divide(const Color: TFloatGrayAlpha; const Theta: VectorFloat): TFloatGrayAlpha;
    { @exclude } class operator Equal(const Color1, Color2: TFloatGrayAlpha): Boolean;
    { @exclude } class operator NotEqual(const Color1, Color2: TFloatGrayAlpha): Boolean; inline;
  public
    case Cardinal of
      0:
        (
          { Gray value ranging from 0.0 (black) to 1.0 (white). }
          Gray: VectorFloat;

          { Alpha-channel value ranging from 0.0 (translucent) to 1.0 (opaque). }
          Alpha: VectorFloat;
        );
      1:
        { Values represented as an array, where gray has index of 0 and alpha-channel index of 1. }
        (Values: array[0..1] of VectorFloat;);
  end;

const
  { Predefined constant for opaque Black color. }
  FloatGrayAlphaBlack: TFloatGrayAlpha = (Gray: 0.0; Alpha: 1.0);

  { Predefined constant for opaque White color. }
  FloatGrayAlphaWhite: TFloatGrayAlpha = (Gray: 1.0; Alpha: 1.0);

  { Predefined constant for translucent Black color. }
  FloatGrayAlphaTranslucentBlack: TFloatGrayAlpha = (Gray: 0.0; Alpha: 0.0);

  { Predefined constant for translucent White color. }
  FloatGrayAlphaTranslucentWhite: TFloatGrayAlpha = (Gray: 1.0; Alpha: 0.0);

{ Creates floating-point grayscale color from its 16-bit representation. }
function FloatGrayAlpha(const Color: TIntGrayAlpha): TFloatGrayAlpha; overload;

{ Creates floating-point grayscale color using individual components. }
function FloatGrayAlpha(const Gray: VectorFloat; const Alpha: VectorFloat = 1.0): TFloatGrayAlpha; overload;

{ Inverts floating-point grayscale color applying formula "Xn = 1.0 - X" for each component. }
function InvertGrayColor(const Color: TFloatGrayAlpha): TFloatGrayAlpha;

{ Takes floating-point grayscale color with unpremultiplied alpha and multiplies gray value by its alpha-channel,
  resulting in premultiplied grayscale color. }
function PremultiplyGrayColor(const Color: TFloatGrayAlpha): TFloatGrayAlpha; overload;

{ Takes floating-point grayscale color with premultiplied alpha-channel and divides its gray value by alpha, resulting
  in unpremultiplied grayscale color. }
function UnpremultiplyGrayColor(const Color: TFloatGrayAlpha): TFloatGrayAlpha; overload;

{ Computes average of two given floating-point grayscale colors. }
function AverageGrayColors(const Color1, Color2: TFloatGrayAlpha): TFloatGrayAlpha;

{ Computes alpha-blending for a pair of floating-point grayscale colors.
    @italic(Alpha) can be in [0..1] range. }
function LerpGrayColors(const Color1, Color2: TFloatGrayAlpha; const Alpha: VectorFloat): TFloatGrayAlpha;

{ Takes original floating-point grayscale color and clamps each component within [0, 1] range. }
function SaturateGrayColor(const Color: TFloatGrayAlpha): TFloatGrayAlpha;

{ Converts floating-point grayscale color to its 16-bit integer equivalent. }
function FloatToIntGrayAlpha(const Color: TFloatGrayAlpha): TIntGrayAlpha;

{$ENDREGION}
{$REGION 'TIntColor'}

type
  { Pointer to @link(TIntColorValue). }
  PIntColorValue = ^TIntColorValue;

  { Raw (untyped) color value is represented as a raw 32-bit unsigned integer, with components allocated according to
    @italic(TPixelFormat.A8R8G8B8) format. }
  TIntColorValue = {$IFDEF NEXTGEN}Cardinal{$ELSE}LongWord{$ENDIF};

  { Pointer to @link(TIntColor). }
  PIntColor = ^TIntColor;

  { General-purpose color value that is represented as 32-bit unsigned integer, with components allocated according to
    @italic(TPixelFormat.A8R8G8B8) format. }
  TIntColor = {$IFDEF NEXTGEN}Cardinal{$ELSE}LongWord{$ENDIF};

  { Pointer to @link(TIntColorRec). }
  PIntColorRec = ^TIntColorRec;

  { Alternative representation of @link(TIntColor), where each element can be accessed as an individual value.
    This can be safely typecast to @link(TIntColor) and vice-versa. }
  TIntColorRec = record
    case Cardinal of
      0:
        (
          { Blue value ranging from 0 (no intensity) to 255 (fully intense). }
          Blue: Byte;

          { Green value ranging from 0 (no intensity) to 255 (fully intense). }
          Green: Byte;

          { Red value ranging from 0 (no intensity) to 255 (fully intense). }
          Red: Byte;

          { Alpha-channel value ranging from 0 (translucent) to 255 (opaque). }
          Alpha: Byte;
        );
      1:
        { Values represented as an array, with indexes corresponding to blue (0), green (1), red (2) and
          alpha-channel (3). }
        (Values: packed array[0..3] of Byte);
  end;

  { Pointer to @link(TIntColorPalette). }
  PIntColorPalette = ^TIntColorPalette;

  { A fixed palette of 256 colors, typically used to emulate legacy 8-bit indexed modes. }
  TIntColorPalette = array[0..255] of TIntColor;

const
  { Predefined constant for opaque Black color. }
  IntColorBlack = $FF000000;

  { Predefined constant for opaque White color. }
  IntColorWhite = $FFFFFFFF;

  { Predefined constant for translucent Black color. }
  IntColorTranslucentBlack = $00000000;

  { Predefined constant for translucent White color. }
  IntColorTranslucentWhite = $00FFFFFF;

{ Creates 32-bit RGBA color with the specified color value, having its alpha-channel multiplied by the specified
  coefficient and divided by 255. }
function IntColor(const Color: TIntColor; const Alpha: Integer): TIntColor; overload;

{ Creates 32-bit RGBA color where the specified color value has its alpha-channel multiplied by the given
  coefficient. }
function IntColor(const Color: TIntColor; const Alpha: VectorFloat): TIntColor; overload; inline;

{ Creates 32-bit RGBA color where the original color value has its components multiplied by the given grayscale value
  and alpha-channel multiplied by the specified coefficient, and all components divided by 255. }
function IntColor(const Color: TIntColor; const Gray, Alpha: Integer): TIntColor; overload;

{ Creates 32-bit RGBA color where the original color value has its components multiplied by the given grayscale value
  and alpha-channel multiplied by the specified coefficient. }
function IntColor(const Color: TIntColor; const Gray, Alpha: VectorFloat): TIntColor; overload; inline;

{ Creates 32-bit RGBA color using specified individual components for red, green, blue and alpha channel. }
function IntColorRGB(const Red, Green, Blue: Integer; const Alpha: Integer = 255): TIntColor; overload;

{ Creates 32-bit RGBA color using specified grayscale and alpha values. }
function IntColorGray(const Gray: Integer; const Alpha: Integer = 255): TIntColor; overload;

{ Creates 32-bit RGBA color using specified grayscale and alpha-channel values (both multiplied by 255). }
function IntColorGray(const Gray: VectorFloat; const Alpha: VectorFloat = 1.0): TIntColor; overload; inline;

{ Creates 32-bit RGBA color with the specified alpha-channel and each of red, green and blue components set to 255. }
function IntColorAlpha(const Alpha: Integer): TIntColor; overload;

{ Creates 32-bit RGBA color with alpha-channel specified by the given coefficient (multiplied by 255) and the rest of
  components set to 255. }
function IntColorAlpha(const Alpha: VectorFloat): TIntColor; overload; inline;

{ Switches red and blue channels in 32-bit RGBA color value. }
function DisplaceRB(const Color: TIntColor): TIntColor;

{ Inverts each of the components in the pixel, including alpha-channel. }
function InvertPixel(const Color: TIntColor): TIntColor;

{ Takes 32-bit RGBA color with unpremultiplied alpha and multiplies each of red, green, and blue components by its
  alpha channel, resulting in premultiplied alpha color. }
function PremultiplyAlpha(const Color: TIntColor): TIntColor; overload;

{ Takes 32-bit RGBA color with premultiplied alpha channel and divides each of its red, green, and blue components by
  alpha, resulting in unpremultiplied alpha color. }
function UnpremultiplyAlpha(const Color: TIntColor): TIntColor; overload;

{ Adds two 32-bit RGBA color values together clamping the resulting values if necessary. }
function AddPixels(const Color1, Color2: TIntColor): TIntColor;

{ Subtracts two 32-bit RGBA color values clamping the resulting values if necessary. }
function SubtractPixels(const Color1, Color2: TIntColor): TIntColor;

{ Multiplies two 32-bit RGBA color values together. }
function MultiplyPixels(const Color1, Color2: TIntColor): TIntColor;

{ Computes average of two given 32-bit RGBA color values. }
function AveragePixels(const Color1, Color2: TIntColor): TIntColor;

{ Computes average of four given 32-bit RGBA color values. }
function AverageFourPixels(const Color1, Color2, Color3, Color4: TIntColor): TIntColor;

{ Computes average of six given 32-bit RGBA color values. }
function AverageSixPixels(const Color1, Color2, Color3, Color4, Color5, Color6: TIntColor): TIntColor;

{ Computes alpha-blending for a pair of 32-bit RGBA colors values.
    @italic(Alpha) can be in [0..255] range. }
function BlendPixels(const Color1, Color2: TIntColor; const Alpha: Integer): TIntColor;

{ Computes resulting alpha-blended value between four 32-bit RGBA colors using linear interpolation.
    @italic(AlphaX) and @italic(AlphaY) can be in [0..255] range. }
function BlendFourPixels(const TopLeft, TopRight, BottomRight, BottomLeft: TIntColor; const AlphaX,
  AlphaY: Integer): TIntColor;

{ Computes alpha-blending for a pair of 32-bit RGBA colors values using floating-point approach. For a faster
  alternative, use @link(BlendPixels).
    @italic(Alpha) can be in [0..1] range.  }
function LerpPixels(const Color1, Color2: TIntColor; const Alpha: VectorFloat): TIntColor;

{ Computes resulting alpha-blended value between four 32-bit RGBA colors using linear interpolation.
    @italic(AlphaX) and @italic(AlphaY) can be in [0..1] range. }
function LerpFourPixels(const TopLeft, TopRight, BottomRight, BottomLeft: TIntColor; const AlphaX,
  AlphaY: VectorFloat): TIntColor;

{ Interpolates between four colors using Catmull-Rom curve.
    @italic(Theta) can be in [0..1] range. }
function CatmullRomPixels(const Color1, Color2, Color3, Color4: TIntColor; const Theta: VectorFloat): TIntColor;

{ Returns grayscale value in range of [0..255] from the given 32-bit RGBA color value. The resulting value can be
  considered the color's @italic(luma). The alpha-channel is ignored. }
function PixelToGray(const Color: TIntColor): Integer;

{ Returns grayscale value in range of [0..1] from the given 32-bit RGBA color value. The resulting value can be
  considered the color's @italic(luma). The alpha-channel is ignored. }
function PixelToGrayFloat(const Color: TIntColor): VectorFloat;

{ Converts 16-bit grayscale color into 32-bit RGBA color. }
function IntGrayToColor(const Color: TIntGrayAlpha): TIntColor;

{ Converts 32-bit RGBA color to 16-bit grayscale color. }
function IntColorToGray(const Color: TIntColor): TIntGrayAlpha;

{ Extracts alpha-channel from two grayscale samples. The sample must be rendered with the same color on two different
  backgrounds, preferably on black and white; the resulting colors are provided in @italic(Src1) and @italic(Src2),
  with original backgrounds in @italic(Bk1) and @italic(Bk2). The resulting alpha-channel and original color are
  computed and returned. This method is particularly useful for calculating alpha-channel when rendering GDI fonts or
  in tools that generate resulting images without providing alpha-channel (therefore rendering the same image on two
  backgrounds is sufficient to calculate its alpha-channel). }
procedure ExtractGrayAlpha(const SourceGray1, SourceGray2, Background1, Background2: VectorFloat;
  out Alpha, Gray: VectorFloat); overload;

{ Extracts alpha-channel from two grayscale samples. The sample must be rendered with the same color on two different
  backgrounds, preferably on black and white; the resulting colors are provided in @italic(Src1) and @italic(Src2),
  with original backgrounds in @italic(Bk1) and @italic(Bk2). The resulting alpha-channel and original color are
  computed and returned. This method is particularly useful for calculating alpha-channel when rendering GDI fonts or
  in tools that generate resulting images without providing alpha-channel (therefore rendering the same image on two
  backgrounds is sufficient to calculate its alpha-channel). }
function ExtractGrayAlpha(const SourceGray1, SourceGray2, Background1,
  Background2: VectorFloat): TFloatGrayAlpha; overload; inline;

{$ENDREGION}
{$REGION 'TIntColor2'}

type
  { This type is used to pass @link(TIntColor2) by reference. }
  PIntColor2 = ^TIntColor2;

  { A combination of two colors, primarily used for displaying text with the first color being on top and the second
    being on bottom. The format for specifying colors is defined as @italic(TPixelFormat.A8R8G8B8). }
  TIntColor2 = record
  public
    { @exclude } class operator Implicit(const Color: TIntColor): TIntColor2; inline;

    { Returns @True if two colors are different at least in one of their red, green, blue or alpha components. }
    function HasGradient: Boolean;

    { Returns @True if at least one of the colors has non-zero alpha channel. }
    function HasAlpha: Boolean;
  public
    case Cardinal of
      0:
        (
          { First color entry, which can be reinterpreted as top or left color depending on context. }
          First: TIntColor;

          { Second color entry, which can be reinterpreted as bottom or right color depending on context. }
          Second: TIntColor;
        );
      1:
        { Two color pair represented as an array. }
        (Values: array[0..1] of TIntColor;);
  end;

const
  { Predefined constant for a pair of opaque Black colors. }
  IntColorBlack2: TIntColor2 = (First: $FF000000; Second: $FF000000);

  { Predefined constant for a pair of opaque White colors. }
  IntColorWhite2: TIntColor2 = (First: $FFFFFFFF; Second: $FFFFFFFF);

  { Predefined constant for a pair of translucent Black colors. }
  IntColorTranslucentBlack2: TIntColor2 = (First: $00000000; Second: $00000000);

  { Predefined constant for a pair of translucent White colors. }
  IntColorTranslucentWhite2: TIntColor2 = (First: $00FFFFFF; Second: $00FFFFFF);

{ Creates two 32-bit RGBA color gradient from specified pair of values. }
function IntColor2(const First, Second: TIntColor): TIntColor2; overload; inline;

{ Creates two 32-bit RGBA color gradient with both values set to specified color. }
function IntColor2(const Color: TIntColor): TIntColor2; overload; inline;

{$ENDREGION}
{$REGION 'TIntColor4'}

type
  { This type is used to pass @link(TIntColor4) by reference. }
  PIntColor4 = ^TIntColor4;

  { A combination of four colors, primarily used for displaying colored quads, where each color corresponds to top/left,
    top/right, bottom/right and bottom/left accordingly (clockwise). The format for specifying colors is defined as
    @italic(TPixelFormat.A8R8G8B8). }
  TIntColor4 = record
  public
    { @exclude } class operator Implicit(const Color: TIntColor): TIntColor4; inline;

    { Returns @True if at least one of four colors is different from others in red, green, blue or alpha components. }
    function HasGradient: Boolean;

    { Returns @True if at least one of the colors has non-zero alpha channel. }
    function HasAlpha: Boolean;
  public
    case Cardinal of
      0:
        (
          { Color corresponding to top/left corner. }
          TopLeft: TIntColor;

          { Color corresponding to top/right corner. }
          TopRight: TIntColor;

          { Color corresponding to bottom/right corner. }
          BottomRight: TIntColor;

          { Color corresponding to bottom/left corner. }
          BottomLeft: TIntColor;
        );
      1:
        { Four colors represented as an array. }
        (Values: array[0..3] of TIntColor);
  end;

const
  { Predefined constant for four opaque Black colors. }
  IntColorBlack4: TIntColor4 = (TopLeft: $FF000000; TopRight: $FF000000; BottomRight: $FF000000;
    BottomLeft: $FF000000);

  { Predefined constant for four opaque White colors. }
  IntColorWhite4: TIntColor4 = (TopLeft: $FFFFFFFF; TopRight: $FFFFFFFF; BottomRight: $FFFFFFFF;
    BottomLeft: $FFFFFFFF);

  { Predefined constant for four translucent Black colors. }
  IntColorTranslucentBlack4: TIntColor4 = (TopLeft: $00000000; TopRight: $00000000; BottomRight: $00000000;
    BottomLeft: $00000000);

  { Predefined constant for four translucent White colors. }
  IntColorTranslucentWhite4: TIntColor4 = (TopLeft: $00FFFFFF; TopRight: $00FFFFFF; BottomRight: $00FFFFFF;
    BottomLeft: $00FFFFFF);

{ Creates a construct of four colors using individual components. }
function IntColor4(const TopLeft, TopRight, BottomRight, BottomLeft: TIntColor): TIntColor4; overload; inline;

{ Creates a construct of four colors having the same component in each corner. }
function IntColor4(const Color: TIntColor): TIntColor4; overload; inline;

{ Creates a construct of four colors from two color pair to create horizontal gradient. }
function IntColor4H(const Color: TIntColor2): TIntColor4; overload; inline;

{ Creates a construct of four colors from two color values to create horizontal gradient. }
function IntColor4H(const Left, Right: TIntColor): TIntColor4; overload; inline;

{ Creates a construct of four colors from two color pair to create vertical gradient. }
function IntColor4V(const Color: TIntColor2): TIntColor4; overload; inline;

{ Creates a construct of four colors from two color values to create vertical gradient. }
function IntColor4V(const Top, Bottom: TIntColor): TIntColor4; overload; inline;

{$ENDREGION}
{$REGION 'TFloatColor'}

type
  { Pointer to @link(TFloatColor). }
  PFloatColor = ^TFloatColor;

  { A special high-precision color value that has each individual component represented as 32-bit floating-point value
    in range of [0, 1]. Although components may have values outside of aforementioned range, such colors cannot be
    reliably displayed on the screen. }
  TFloatColor = record
  public
    { @exclude } class operator Add(const Color1, Color2: TFloatColor): TFloatColor;
    { @exclude } class operator Subtract(const Color1, Color2: TFloatColor): TFloatColor;
    { @exclude } class operator Multiply(const Color1, Color2: TFloatColor): TFloatColor;
    { @exclude } class operator Divide(const Color1, Color2: TFloatColor): TFloatColor;
    { @exclude } class operator Multiply(const Color: TFloatColor; const Theta: VectorFloat): TFloatColor;
    { @exclude } class operator Multiply(const Theta: VectorFloat; const Color: TFloatColor): TFloatColor;
    { @exclude } class operator Divide(const Color: TFloatColor; const Theta: VectorFloat): TFloatColor;
    { @exclude } class operator Equal(const Color1, Color2: TFloatColor): Boolean;
    { @exclude } class operator NotEqual(const Color1, Color2: TFloatColor): Boolean; inline;
  public
    case Cardinal of
      0:
        (
          { Red value ranging from 0.0 (no intensity) to 1.0 (fully intense). }
          Red: VectorFloat;

          { Green value ranging from 0.0 (no intensity) to 1.0 (fully intense). }
          Green: VectorFloat;

          { Blue value ranging from 0.0 (no intensity) to 1.0 (fully intense). }
          Blue: VectorFloat;

          { Alpha-channel value ranging from 0.0 (translucent) to 1.0 (opaque). }
          Alpha: VectorFloat;
        );
      1:
        { Values represented as an array, with indexes corresponding to red (0), green (1), blue (2) and
          alpha-channel (3). }
        (Values: array[0..3] of VectorFloat);
  end;

const
  { Predefined constant for opaque Black color. }
  FloatColorBlack: TFloatColor = (Red: 0.0; Green: 0.0; Blue: 0.0; Alpha: 1.0);

  { Predefined constant for opaque White color. }
  FloatColorWhite: TFloatColor = (Red: 1.0; Green: 1.0; Blue: 1.0; Alpha: 1.0);

  { Predefined constant for translucent Black color. }
  FloatColorTranslucentBlack: TFloatColor = (Red: 0.0; Green: 0.0; Blue: 0.0; Alpha: 0.0);

  { Predefined constant for translucent White color. }
  FloatColorTranslucentWhite: TFloatColor = (Red: 1.0; Green: 1.0; Blue: 1.0; Alpha: 0.0);

{ Creates floating-point color from its 32-bit integer representation. }
function FloatColor(const Color: TIntColor): TFloatColor; overload;

{ Creates floating-point color using specified individual components for red, green, blue and alpha channel. }
function FloatColor(const Red, Green, Blue: VectorFloat; const Alpha: VectorFloat = 1.0): TFloatColor; overload;

{ Inverts floating-point color applying formula "Xn = 1.0 - X" for each component. }
function InvertColor(const Color: TFloatColor): TFloatColor;

{ Takes floating-point color with unpremultiplied alpha and multiplies each of red, green, and blue components by its
  alpha channel, resulting in premultiplied alpha color. }
function PremultiplyAlpha(const Color: TFloatColor): TFloatColor; overload;

{ Takes floating-point color with premultiplied alpha channel and divides each of its red, green, and blue components by
  alpha, resulting in unpremultiplied alpha color. }
function UnpremultiplyAlpha(const Color: TFloatColor): TFloatColor; overload;

{ Computes average of two given floating-point color values. }
function AverageColors(const Color1, Color2: TFloatColor): TFloatColor;

{ Computes alpha-blending for a pair of floating-point colors values.
    @italic(Alpha) can be in [0..1] range. }
function LerpColors(const Color1, Color2: TFloatColor; const Alpha: VectorFloat): TFloatColor;

{ Returns grayscale value in range of [0..1] from the given floating-point color value. The resulting value can be
  considered the color's @italic(luma). The alpha-channel is ignored. }
function ColorToGray(const Color: TFloatColor): VectorFloat;

{ Takes original floating-point color and clamps each component within [0, 1] range. }
function SaturateColor(const Color: TFloatColor): TFloatColor;

{ Takes original floating-point color and attempts to normalize it so each component stays within [0, 1] range.
  The process is different from @link(SaturateColor) approach and may produce some interesting visual effects. }
function WarpColor(const Color: TFloatColor): TFloatColor;

{ Interpolates between four floating-point colors using Catmull-Rom curve.
    @italic(Theta) can be in [0..1] range. }
function CatmullRomColors(const Color1, Color2, Color3, Color4: TFloatColor; const Theta: VectorFloat): TFloatColor;

{ Converts floating-point color to 32-bit integer representation. }
function FloatToIntColor(const Color: TFloatColor): TIntColor;

{ Converts floating-point grayscale color to floating-point RGBA color. }
function FloatGrayToColor(const Color: TFloatGrayAlpha): TFloatColor;

{ Converts floating-point RGBA color to floating-point grayscale color. Resulting gray component can be considered the
  color's @italic(luma). }
function FloatColorToGray(const Color: TFloatColor): TFloatGrayAlpha;

{$ENDREGION}
{$REGION 'TPoint2px declarations'}

type
  { This type is used to pass @link(TPoint2px) by reference. }
  PPoint2px = ^TPoint2px;

  { 2D integer vector. }
  TPoint2px = record
    { The coordinate in 2D space. }
    X, Y: VectorInt;

    { @exclude } class operator Add(const Point1, Point2: TPoint2px): TPoint2px;
    { @exclude } class operator Subtract(const Point1, Point2: TPoint2px): TPoint2px;
    { @exclude } class operator Multiply(const Point1, Point2: TPoint2px): TPoint2px;
    { @exclude } class operator Divide(const Point1, Point2: TPoint2px): TPoint2px;
    { @exclude } class operator Negative(const Point: TPoint2px): TPoint2px;
    { @exclude } class operator Multiply(const Point: TPoint2px; const Theta: VectorInt): TPoint2px;
    { @exclude } class operator Multiply(const Theta: VectorInt; const Point: TPoint2px): TPoint2px;
    { @exclude } class operator Divide(const Point: TPoint2px; const Theta: VectorInt): TPoint2px;
    { @exclude } class operator Divide(const Point: TPoint2px; const Theta: VectorFloat): TPoint2px;
    { @exclude } class operator Equal(const Point1, Point2: TPoint2px): Boolean;
    { @exclude } class operator NotEqual(const Point1, Point2: TPoint2px): Boolean;
  end;

const
  { Predefined constant, where X and Y are zero. }
  ZeroPoint2px: TPoint2px = (X: 0; Y: 0);

  { Predefined constant, where X and Y are one. }
  UnityPoint2px: TPoint2px = (X: 1; Y: 1);

  { Predefined constant, where X = 1 and Y = 0. }
  AxisXPoint2px: TPoint2px = (X: 1; Y: 0);

  { Predefined constant, where X = 0 and Y = 1. }
  AxisYPoint2px: TPoint2px = (X: 0; Y: 1);

  { Predefined constant for "negative infinity". }
  MinusInfinity2px: TPoint2px = (X: Low(VectorInt) + 1; Y: Low(VectorInt) + 1);

  { Predefined constant for "positive infinity". }
  PlusInfinity2px: TPoint2px = (X: High(VectorInt); Y: High(VectorInt));

  { Predefined constant that can be interpreted as "undefined value". }
  Undefined2px: TPoint2px = (X: Low(VectorInt); Y: Low(VectorInt));

{ Creates a @link(TPoint2px) record using the specified coordinates. }
function Point2px(const X, Y: VectorInt): TPoint2px;

{ Returns the length of the specified 2D vector. }
function Length2px(const Point: TPoint2px): VectorFloat;

{ Returns the angle (in radians) at which the 2D vector is pointing at. }
function Angle2px(const Point: TPoint2px): VectorFloat;

{ Interpolates between specified two 2D integer vectors.
  @param(Point1 The first vector to be used in the interpolation)
  @param(Point2 The second vector to be used in the interpolation)
  @param(Theta The mixture of the two vectors with the a range of [0..1].) }
function Lerp2px(const Point1, Point2: TPoint2px; const Theta: VectorFloat): TPoint2px;

{ Interpolates between four integer 2D vectors using @italic(Catmull-Rom) curve.
  @param(PredPoint1 Predecessor to @italic(Point1) that defines initial part of curve.)
  @param(Point1 First base point used in interpolation (for Theta = 0).)
  @param(Point2 Last base point used in interpolation (for Theta = 1).)
  @param(SuccPoint2 Successor to @italic(Point2) that defines final part of curve..)
  @param(Theta Mixture coefficient between two vector in range of [0..1].) }
function CatmullRom2px(const PredPoint1, Point1, Point2, SuccPoint2: TPoint2px;
  const Theta: VectorFloat): TPoint2px;

{ Calculates the dot-product of the specified 2D vectors. The dot-product is an indirect measure of the angle between
  two vectors. }
function Dot2px(const Point1, Point2: TPoint2px): VectorInt;

{ Calculates a so-called "cross product" of 2D vectors, or analog of thereof. }
function Cross2px(const Point1, Point2: TPoint2px): VectorInt;

{$ENDREGION}
{$REGION 'TPoint2 declarations'}

type
  { This type is used to pass @link(TPoint2) by reference. }
  PPoint2 = ^TPoint2;

  { 2D floating-point vector. }
  TPoint2 = record
    { The coordinate in 2D space. }
    X, Y: VectorFloat;

    { @exclude } class operator Add(const APoint1, APoint2: TPoint2): TPoint2;
    { @exclude } class operator Subtract(const APoint1, APoint2: TPoint2): TPoint2;
    { @exclude } class operator Multiply(const APoint1, APoint2: TPoint2): TPoint2;
    { @exclude } class operator Divide(const APoint1, APoint2: TPoint2): TPoint2;
    { @exclude } class operator Negative(const Point: TPoint2): TPoint2;
    { @exclude } class operator Multiply(const Point: TPoint2; const Theta: VectorFloat): TPoint2;
    { @exclude } class operator Multiply(const Theta: VectorFloat; const Point: TPoint2): TPoint2;
    { @exclude } class operator Divide(const Point: TPoint2; const Theta: VectorFloat): TPoint2;
    { @exclude } class operator Divide(const Point: TPoint2; const Theta: Integer): TPoint2;
    { @exclude } class operator Implicit(const Point: TPoint2px): TPoint2;
    { @exclude } class operator Equal(const APoint1, APoint2: TPoint2): Boolean;
    { @exclude } class operator NotEqual(const APoint1, APoint2: TPoint2): Boolean; inline;

{$IFDEF POINT2_TO_PX_IMPLICIT}
    { This implicit conversion is only allowed as a compiler directive because it causes ambiguity and precision
      problems when excessively used. In addition, it can make code more confusing. }
    { @exclude } class operator Implicit(const Point: TPoint2): TPoint2px;
{$ENDIF}
    procedure FromPointF(p: TpointF);
  end;

function Order(var ul,br: TPoint2): boolean;overload;


const
  { Predefined constant, where X and Y are zero. }
  ZeroPoint2: TPoint2 = (X: 0.0; Y: 0.0);

  { Predefined constant, where X and Y are one. }
  UnityPoint2: TPoint2 = (X: 1.0; Y: 1.0);

  { Predefined constant, where X = 1 and Y = 0. }
  AxisXPoint2: TPoint2 = (X: 1.0; Y: 0.0);

  { Predefined constant, where X = 0 and Y = 1. }
  AxisYPoint2: TPoint2 = (X: 0.0; Y: 1.0);

{ Creates a @link(TPoint2) record using the specified coordinates. }
function Point2(const X, Y: VectorFloat): TPoint2; inline;

{ Converts @link(TPoint2) to @link(TPoint2px) by using floating-point rounding. }
function Point2ToPx(const Point: TPoint2): TPoint2px;

{ Returns the length of the specified 2D vector. }
function Length2(const Point: TPoint2): VectorFloat;

{ Normalizes the specified 2D vector to unity length. If the vector is of zero length, it will remain unchanged. }
function Norm2(const Point: TPoint2): TPoint2;

{ Returns the angle (in radians) at which the 2D vector is pointing at. }
function Angle2(const Point: TPoint2): VectorFloat;

{ Interpolates between specified two 2D vectors.
  @param(Point1 The first vector to be used in the interpolation)
  @param(Point2 The second vector to be used in the interpolation)
  @param(Theta The mixture of the two vectors with the a range of [0..1].) }
function Lerp2(const Point1, Point2: TPoint2; const Theta: VectorFloat): TPoint2;

{ Interpolates between four 2D vectors using @italic(Catmull-Rom) curve.
  @param(PredPoint1 Predecessor to @italic(Point1) that defines initial part of curve.)
  @param(Point1 First base point used in interpolation (for Theta = 0).)
  @param(Point2 Last base point used in interpolation (for Theta = 1).)
  @param(SuccPoint2 Successor to @italic(Point2) that defines final part of curve..)
  @param(Theta Mixture coefficient between two vector in range of [0..1].) }
function CatmullRom2(const PredPoint1, Point1, Point2, SuccPoint2: TPoint2;
  const Theta: VectorFloat): TPoint2;

{ Calculates the dot-product of the specified 2D vectors. The dot-product is an indirect measure of the angle between
  two vectors. }
function Dot2(const Point1, Point2: TPoint2): VectorFloat;

{ Calculates a so-called "cross product" of 2D vectors, or analog of thereof. }
function Cross2(const Point1, Point2: TPoint2): VectorFloat;

{$ENDREGION}
{$REGION 'TVector3px declarations'}

type
  { This type is used to pass @link(TVector3px) by reference. }
  PVector3px = ^TVector3px;

  { 3D integer vector. }
  TVector3px = record
    { The coordinate in 3D space. }
    X, Y, Z: VectorInt;

    { @exclude } class operator Add(const Vector1, Vector2: TVector3px): TVector3px;
    { @exclude } class operator Subtract(const Vector1, Vector2: TVector3px): TVector3px;
    { @exclude } class operator Multiply(const Vector1, Vector2: TVector3px): TVector3px;
    { @exclude } class operator Divide(const Vector1, Vector2: TVector3px): TVector3px;
    { @exclude } class operator Negative(const Vector: TVector3px): TVector3px;
    { @exclude } class operator Multiply(const Vector: TVector3px; const Theta: VectorInt): TVector3px;
    { @exclude } class operator Multiply(const Theta: VectorInt; const Vector: TVector3px): TVector3px;
    { @exclude } class operator Divide(const Vector: TVector3px; const Theta: VectorInt): TVector3px;
    { @exclude } class operator Equal(const Vector1, Vector2: TVector3px): Boolean;
    { @exclude } class operator NotEqual(const Vector1, Vector2: TVector3px): Boolean; inline;

    { Returns (x, y) portion of 3D vector as @link(TPoint2px). }
    function GetXY: TPoint2px;
  end;

const
  { Predefined constant, where X, Y and Z are zero. }
  ZeroVec3px: TVector3px = (X: 0; Y: 0; Z: 0);

  { Predefined constant, where X, Y and Z are one. }
  UnityVec3px: TVector3px = (X: 1; Y: 1; Z: 1);

  { Predefined constant, where X = 1, Y = 0 and Z = 0. }
  AxisXVec3px: TVector3px = (X: 1; Y: 0; Z: 0);

  { Predefined constant, where X = 0, Y = 1 and Z = 0. }
  AxisYVec3px: TVector3px = (X: 0; Y: 1; Z: 0);

  { Predefined constant, where X = 0, Y = 0 and Z = 1. }
  AxisZVec3px: TVector3px = (X: 0; Y: 0; Z: 1);

{ Creates @link(TVector3px) record using the specified coordinates. }
function Vector3px(const X, Y, Z: VectorInt): TVector3px; overload; inline;

{ Creates @link(TVector3px) record using 2D vector and specified Z coordinate. }
function Vector3px(const Point: TPoint2px; const Z: VectorInt = 0): TVector3px; overload; inline;

{ Returns length of specified 3D integer vector. }
function Length3px(const Vector: TVector3px): VectorFloat;

{ Interpolates between specified two integer 3D vectors.
  @param(Vector1 The first vector to be used in the interpolation)
  @param(Vector2 The second vector to be used in the interpolation)
  @param(Theta The mixture of the two vectors with the a range of [0..1].) }
function Lerp3px(const Vector1, Vector2: TVector3px; const Theta: VectorFloat): TVector3px;

{ Interpolates between four integer 3D vectors using @italic(Catmull-Rom) curve.
  @param(PreVector1 Predecessor to @italic(Vector1) that defines initial part of curve.)
  @param(Vector1 First base vector used in interpolation (for Theta = 0).)
  @param(Vector2 Second base vector used in interpolation (for Theta = 1).)
  @param(PostVector2 Successor to @italic(Vector2) that defines final part of curve..)
  @param(Theta Mixture coefficient between two vector in range of [0..1].) }
function CatmullRom3px(const PredVector1, Vector1, Vector2, SuccVector2: TVector3px;
  const Theta: VectorFloat): TVector3px;

{ Calculates dot product of specified 3D vectors. The dot-product is an indirect measure of angle between two
  vectors. }
function Dot3px(const Vector1, Vector2: TVector3px): VectorInt;

{ Calculates cross product of specified 3D vectors. The resulting vector is perpendicular to both source
  vectors and normal to the plane containing them. }
function Cross3px(const Vector1, Vector2: TVector3px): TVector3px;

{ Calculates angle between two 3D vectors. The returned value has range of [0..Pi]. }
function Angle3px(const Vector1, Vector2: TVector3px): VectorFloat;

{$ENDREGION}
{$REGION 'TVector3 declarations'}

type
  { This type is used to pass @link(TVector3) by reference. }
  PVector3 = ^TVector3;

  { 3D floating-point vector. }
  TVector3 = record
    { The coordinate in 3D space. }
    X, Y, Z: VectorFloat;

    { @exclude } class operator Add(const Vector1, Vector2: TVector3): TVector3;
    { @exclude } class operator Subtract(const Vector1, Vector2: TVector3): TVector3;
    { @exclude } class operator Multiply(const Vector1, Vector2: TVector3): TVector3;
    { @exclude } class operator Divide(const Vector1, Vector2: TVector3): TVector3;
    { @exclude } class operator Negative(const Vector: TVector3): TVector3;
    { @exclude } class operator Multiply(const Vector: TVector3; const Theta: VectorFloat): TVector3;
    { @exclude } class operator Multiply(const Theta: VectorFloat; const Vector: TVector3): TVector3;
    { @exclude } class operator Divide(const Vector: TVector3; const Theta: VectorFloat): TVector3;
    { @exclude } class operator Implicit(const Vector: TVector3px): TVector3;
    { @exclude } class operator Equal(const Vector1, Vector2: TVector3): Boolean;
    { @exclude } class operator NotEqual(const Vector1, Vector2: TVector3): Boolean; inline;

    { Returns (x, y) portion of 3D vector as @link(TPoint2). }
    function GetXY: TPoint2;

    { Returns (x, y) portion of 3D vector as @link(TPoint2px) rounding values down to integers. }
    function GetXYpx: TPoint2px;
    function ToString: string;
    procedure FromString(s: string);
  end;

const
  { Predefined constant, where X, Y and Z are zero. }
  ZeroVec3: TVector3 = (X: 0.0; Y: 0.0; Z: 0.0);

  { Predefined constant, where X, Y and Z are one. }
  UnityVec3: TVector3 = (X: 1.0; Y: 1.0; Z: 1.0);

  { Predefined constant, where X = 1, Y = 0 and Z = 0. }
  AxisXVec3: TVector3 = (X: 1.0; Y: 0.0; Z: 0.0);

  { Predefined constant, where X = 0, Y = 1 and Z = 0. }
  AxisYVec3: TVector3 = (X: 0.0; Y: 1.0; Z: 0.0);

  { Predefined constant, where X = 0, Y = 0 and Z = 1. }
  AxisZVec3: TVector3 = (X: 0.0; Y: 0.0; Z: 1.0);

{ Creates @link(TVector3) record using the specified coordinates. }
function Vector3(const X, Y, Z: VectorFloat): TVector3; overload; inline;

{ Creates @link(TVector3) record using 2D vector and specified Z coordinate. }
function Vector3(const Point: TPoint2; const Z: VectorFloat = 0.0): TVector3; overload; inline;

{ Converts @link(TVector3) to @link(TVector3px) by using floating-point rounding. }
function Vector3ToPx(const Vector: TVector3): TVector3px;

{ Returns length of specified 3D vector. }
function Length3(const Vector: TVector3): VectorFloat;

{ Normalizes specified 3D vector to unity length. If the vector is of zero length, it will remain unchanged. }
function Norm3(const Vector: TVector3): TVector3;

{ Interpolates between specified two 3D vectors.
  @param(Vector1 The first vector to be used in the interpolation)
  @param(Vector2 The second vector to be used in the interpolation)
  @param(Theta The mixture of the two vectors with the a range of [0..1].) }
function Lerp3(const Vector1, Vector2: TVector3; const Theta: VectorFloat): TVector3;

{ Interpolates between four 3D vectors using @italic(Catmull-Rom) curve.
  @param(PreVector1 Predecessor to @italic(Vector1) that defines initial part of curve.)
  @param(Vector1 First base vector used in interpolation (for Theta = 0).)
  @param(Vector2 Second base vector used in interpolation (for Theta = 1).)
  @param(PostVector2 Successor to @italic(Vector2) that defines final part of curve..)
  @param(Theta Mixture coefficient between two vector in range of [0..1].) }
function CatmullRom3(const PredVector1, Vector1, Vector2, SuccVector2: TVector3;
  const Theta: VectorFloat): TVector3;

{ Calculates dot product of specified 3D vectors. The dot-product is an indirect measure of angle between two
  vectors. }
function Dot3(const Vector1, Vector2: TVector3): VectorFloat;

{ Calculates cross product of specified 3D vectors. The resulting vector is perpendicular to both source
  vectors and normal to the plane containing them. }
function Cross3(const Vector1, Vector2: TVector3): TVector3;

{ Calculates angle between two 3D vectors. The returned value has range of [0..Pi]. }
function Angle3(const Vector1, Vector2: TVector3): VectorFloat;

{ Calculates portion of given 3D vector that is parallel to the direction vector. }
function Parallel3(const Vector, Direction: TVector3): TVector3;

{ Calculates portion of given 3D vector that is perpendicular to the direction vector. }
function Perp3(const Vector, Direction: TVector3): TVector3;

{ Calculates 3D vector that is a reflection of given vector from surface given by specified normal. }
function Reflect3(const Vector, Normal: TVector3): TVector3;

{ Converts Red, Green and Blue components of 32-bit color to X, Y and Z components of 3D vector accordingly. }
function ColorToVec3(const Color: TIntColor): TVector3;

{ Converts X, Y and Z components of 3D vector to Red, Green and Blue components of 32-bit color accordingly. }
function Vec3ToColor(const Vector: TVector3): TIntColor;

{$ENDREGION}
{$REGION 'TVector4 declarations'}

type
  { This type is used to pass @link(TVector4) by reference. }
  PVector4 = ^TVector4;

  { 4D (3D + w) floating-point vector. }
  TVector4 = record
  private
    { The coordinate in 3D space. }
    FX, FY, FZ: VectorFloat;

    { Homogeneous transform coordinate, mostly used for perspective projection.
      Typically, this component is set to 1.0. }
    FW: VectorFloat;
    procedure SetZ(const Value: VectorFloat);inline;
    function GetRGB: TVector3;
    procedure SetRGB(const Value: TVector3);
  public


    { @exclude } class operator Add(const Vector1, Vector2: TVector4): TVector4;
    { @exclude } class operator Add(const Vector1: TVEctor4; Vector2: TVector3): TVector4;
    { @exclude } class operator Subtract(const Vector1, Vector2: TVector4): TVector4;
    { @exclude } class operator Multiply(const Vector1, Vector2: TVector4): TVector4;
    { @exclude } class operator Divide(const Vector1, Vector2: TVector4): TVector4;
    { @exclude } class operator Negative(const Vector: TVector4): TVector4;
    { @exclude } class operator Multiply(const Vector: TVector4; const Theta: VectorFloat): TVector4;
    { @exclude } class operator Multiply(const Theta: VectorFloat; const Vector: TVector4): TVector4;
    { @exclude } class operator Divide(const Vector: TVector4; const Theta: VectorFloat): TVector4;
    { @exclude } class operator Equal(const Vector1, Vector2: TVector4): Boolean;
    { @exclude } class operator NotEqual(const Vector1, Vector2: TVector4): Boolean; inline;
    class operator Implicit(Vector3: TVector3): Tvector4;inline;

    { Returns (X, Y, Z) portion of 4D vector. }
    function GetXYZ: TVector3;
    { Returns (X, Y, Z) portion of current 4D (3D + W) vector, projecting to W = 1 whenever necessary. }
    function ProjectToXYZ: TVector3;
    function GetXy: TPoint2;
    procedure SetXY(const Value: TPoint2);
    property XY: TPoint2 read GetXy write SetXY;
    procedure Init;
    property x: Vectorfloat read Fx write fx;
    property y: VectorFloat read Fy write Fy;
    property Z: VectorFloat read Fz write SetZ;
    property w: Vectorfloat read Fw write fw;
    property R: Vectorfloat read Fx write fx;
    property G: VectorFloat read Fy write Fy;
    property B: VectorFloat read Fz write SetZ;
    property A: Vectorfloat read Fw write fw;
    property rgb: TVector3 read GetRGB write SetRGB;
    function ToString: string;
    function Length: VEctorFloat;
  end;

const
  { Predefined constant, where all components are zero. }
  ZeroVec4: TVector4 = (FX: 0.0; FY: 0.0; FZ: 0.0; FW: 0.0);

  { Predefined constant, where X, Y and Z are zero, while W = 1. }
  ZeroVec4H: TVector4 = (FX: 0.0; FY: 0.0; FZ: 0.0; FW: 1.0);

  { Predefined constant, where all components are one. }
  UnityVec4: TVector4 = (FX: 1.0; FY: 1.0; FZ: 1.0; FW: 1.0);

  { Predefined constant, where X = 1, Y = 0, Z = 0 and W = 1. }
  AxisXVec4H: TVector4 = (FX: 1.0; FY: 0.0; FZ: 0.0; FW: 1.0);

  { Predefined constant, where X = 0, Y = 1, Z = 0 and W = 1. }
  AxisYVec4H: TVector4 = (FX: 0.0; FY: 1.0; FZ: 0.0; FW: 1.0);

  { Predefined constant, where X = 0, Y = 0, Z = 1 and W = 1. }
  AxisZVec4H: TVector4 = (FX: 0.0; FY: 0.0; FZ: 1.0; FW: 1.0);

  { Predefined constant, where X = 1, Y = 0, Z = 0 and W = 0. }
  AxisXVec4: TVector4 = (FX: 1.0; FY: 0.0; FZ: 0.0; FW: 0.0);

  { Predefined constant, where X = 0, Y = 1, Z = 0 and W = 0. }
  AxisYVec4: TVector4 = (FX: 0.0; FY: 1.0; FZ: 0.0; FW: 0.0);

  { Predefined constant, where X = 0, Y = 0, Z = 1 and W = 0. }
  AxisZVec4: TVector4 = (FX: 0.0; FY: 0.0; FZ: 1.0; FW: 0.0);

  { Predefined constant, where X = 0, Y = 0, Z = 0 and W = 1. }
  AxisWVec4: TVector4 = (FX: 0.0; FY: 0.0; FZ: 0.0; FW: 1.0);

{ Creates a @link(TVector4) record using the specified X, Y, Z and W coordinates. }
function Vector4(const X, Y, Z: VectorFloat; const W: VectorFloat = 1.0): TVector4; overload; inline;

{ Creates a @link(TVector4) record using the specified 3D vector and W coordinate. }
function Vector4(const Vector: TVector3; const W: VectorFloat = 1.0): TVector4; overload; inline;

{ Returns the length of the specified 4D vector. }
function Length4(const Vector: TVector4): VectorFloat;

{ Normalizes the specified 4D vector to unity length. The second parameter is used to prevent division by zero in
  vectors that are of zero length. }
function Norm4(const Vector: TVector4): TVector4;

{ Interpolates between specified two 4D vectors.
  @param(Vector1 The first vector to be used in the interpolation)
  @param(Vector2 The second vector to be used in the interpolation)
  @param(Theta The mixture of the two vectors with the a range of [0..1].) }
function Lerp4(const Vector1, Vector2: TVector4; const Theta: VectorFloat): TVector4;

{ Interpolates between four 4D vectors using @italic(Catmull-Rom) curve.
  @param(PredVector1 Predecessor to @italic(Vector1) that defines initial part of curve.)
  @param(Vector1 First base vector used in interpolation (for Theta = 0).)
  @param(Vector2 Second base vector used in interpolation (for Theta = 1).)
  @param(SuccVector2 Successor to @italic(Vector2) that defines final part of curve..)
  @param(Theta Mixture coefficient between two vector in range of [0..1].) }
function CatmullRom4(const PredVector1, Vector1, Vector2, SuccVector2: TVector4;
  const Theta: VectorFloat): TVector4;

{ Converts Red, Green, Blue and Alpha components of 32-bit color to X, Y, Z and W components of 4D vector accordingly. }
function ColorToVec4(const Color: TIntColor): TVector4;

{ Converts X, Y, Z and W components of 4D vector to Red, Green, Blue and Alpha components of 32-bit color accordingly. }
function Vec4ToColor(const Vector: TVector4): TIntColor;

{$ENDREGION}
{$REGION 'TMatrix3 declarations'}

type
  { This type is used to pass @link(TMatrix3) by reference. }
  PMatrix3 = ^TMatrix3;

  { 3x3 transformation matrix. }
  TMatrix3 = record
    { Individual matrix values. }
    Data: array[0..2, 0..2] of VectorFLoat;

    { @exclude } class operator Add(const Matrix1, Matrix2: TMatrix3): TMatrix3;
    { @exclude } class operator Subtract(const Matrix1, Matrix2: TMatrix3): TMatrix3;
    { @exclude } class operator Multiply(const Matrix1, Matrix2: TMatrix3): TMatrix3;
    { @exclude } class operator Multiply(const Matrix: TMatrix3; const Theta: VectorFloat): TMatrix3;
    { @exclude } class operator Multiply(const Theta: VectorFloat; const Matrix: TMatrix3): TMatrix3; inline;
    { @exclude } class operator Divide(const Matrix: TMatrix3; const Theta: VectorFloat): TMatrix3;
    { @exclude } class operator Multiply(const Point: TPoint2; const Matrix: TMatrix3): TPoint2;

    { Calculates determinant of current matrix. }
    function Determinant: VectorFloat;
  end;

const
  { Predefined constant with values corresponding to @italic(Identity) matrix. }
  IdentityMtx3: TMatrix3 = (Data: ((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0)));

  { Predefined constant, where all matrix values are zero. }
  ZeroMtx3: TMatrix3 = (Data: ((0.0, 0.0, 0.0), (0.0, 0.0, 0.0), (0.0, 0.0, 0.0)));

{ Creates 2D translation matrix with specified offset. }
function TranslateMtx3(const Offset: TPoint2): TMatrix3; overload;

{ Creates 2D translation matrix with specified coordinates. }
function TranslateMtx3(const X, Y: VectorFloat): TMatrix3; overload; inline;

{ Creates 2D scaling matrix with specified coefficients. }
function ScaleMtx3(const Scale: TPoint2): TMatrix3; overload;

{ Creates 2D scaling matrix with specified individual coefficients. }
function ScaleMtx3(const X, Y: VectorFloat): TMatrix3; overload; inline;

{ Creates 2D scaling matrix with with X and Y equal to the specified coefficient. }
function ScaleMtx3(const Scale: VectorFloat): TMatrix3; overload; inline;

{ Creates 2D rotation matrix with specified angle (in radiants). }
function RotateMtx3(const Angle: VectorFLoat): TMatrix3;

{ Returns transposed 2D matrix. That is, rows become columns and vice-versa. }
function TransposeMtx3(const Matrix: TMatrix3): TMatrix3;

{ Calculates 2D adjoint matrix for given matrix. }
function AdjointMtx3(const Matrix: TMatrix3): TMatrix3;

{ Calculates inverse of given 2D matrix. }
function InverseMtx3(const Matrix: TMatrix3): TMatrix3;

{$ENDREGION}
{$REGION 'TMatrix4 declarations'}

type
  { This type is used to pass @link(TMatrix4) by reference. }
  PMatrix4 = ^TMatrix4;

  { 4x4 transformation matrix. }
  TMatrix4 = record
  private
    class function DetSub3(const A1, A2, A3, B1, B2, B3, C1, C2, C3: VectorFloat): VectorFloat; static;
  public
    { Individual matrix values. }
    Data: array[0..3, 0..3] of VectorFloat;

    { @exclude } class operator Add(const Matrix1, Matrix2: TMatrix4): TMatrix4;
    { @exclude } class operator Subtract(const Matrix1, Matrix2: TMatrix4): TMatrix4;
    { @exclude } class operator Multiply(const Matrix1, Matrix2: TMatrix4): TMatrix4;
    { @exclude } class operator Multiply(const Matrix: TMatrix4; const Theta: VectorFloat): TMatrix4;
    { @exclude } class operator Multiply(const Theta: VectorFloat; const Matrix: TMatrix4): TMatrix4; inline;
    { @exclude } class operator Divide(const Matrix: TMatrix4; const Theta: VectorFloat): TMatrix4;
    { @exclude } class operator Multiply(const Vector: TVector3; const Matrix: TMatrix4): TVector3;
    { @exclude } class operator Multiply(const Vector: TVector4; const Matrix: TMatrix4): TVector4;

    { Calculates determinant of current matrix. }
    function Determinant: VectorFloat;

    { Assuming that the current matrix is a view matrix, this method calculates the 3D position where the camera
      (or "eye") is supposedly located. }
    function EyePos: TVector3;

    { Assuming that the specified matrix is a world matrix, this method calculates the 3D position where the object
      (or "world") is supposedly located. }
    function WorldPos: TVector3;

    function ToString(): string;
  end;

const
  { Predefined constant with values corresponding to @italic(Identity) matrix. }
  IdentityMtx4: TMatrix4 = (Data: ((1.0, 0.0, 0.0, 0.0), (0.0, 1.0, 0.0, 0.0), (0.0, 0.0, 1.0, 0.0),
    (0.0, 0.0, 0.0, 1.0)));
  MirrorMtx4: TMatrix4 = (Data: ((-1.0, 0.0, 0.0, 0.0), (0.0, -1.0, 0.0, 0.0), (0.0, 0.0, -1.0, 0.0),
    (0.0, 0.0, 0.0, 1.0)));
  MoarMirrorMtx4: TMatrix4 = (Data: ((-1.0, 0.0, 0.0, 0.0), (0.0, -1.0, 0.0, 0.0), (0.0, 0.0, -1.0, 0.0),
    (0.0, 0.0, 0.0, -1.0)));

  { Predefined constant, where all matrix values are zero. }
  ZeroMtx4: TMatrix4 = (Data: ((0.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 0.0),
    (0.0, 0.0, 0.0, 0.0)));

{ Creates 3D translation matrix with specified offset. }
function TranslateMtx4(const Offset: TVector3): TMatrix4; overload;
function TranslateMtx4(const Offset: TVector4): TMatrix4;overload;

{ Creates 3D translation matrix with specified X, Y and (optional) Z coordinates. }
function TranslateMtx4(const X, Y: VectorFloat; const Z: VectorFloat = 0.0): TMatrix4; overload; inline;

{ Creates 3D translation matrix with specified coefficients. }
function ScaleMtx4(const Scale: TVector3): TMatrix4; overload;

{ Creates 3D translation matrix with specified X, Y and (optional) Z coefficients. }
function ScaleMtx4(const X, Y: VectorFloat; const Z: VectorFloat = 0.0): TMatrix4; overload; inline;

{ Creates 3D translation matrix with X, Y and Z equal to the specified coefficient. }
function ScaleMtx4(const Scale: VectorFloat): TMatrix4; overload; inline;

{ Creates 3D rotation matrix around X axis with the specified angle. }
function RotateXMtx4(const Angle: VectorFloat): TMatrix4;

{ Creates 3D rotation matrix around Y axis with the specified angle. }
function RotateYMtx4(const Angle: VectorFloat): TMatrix4;

{ Creates 3D rotation matrix around Z axis with the specified angle. }
function RotateZMtx4(const Angle: VectorFloat): TMatrix4;

{ Creates 3D rotation matrix around specified axis and angle (in radiants). }
function RotateMtx4(const Axis: TVector3; const Angle: VectorFloat): TMatrix4;

{ Creates 3D rotation matrix based on parameters similar to flight dynamics, specifically heading, pitch and Bank.
  Each of the components is specified individually. }
function HeadingPitchBankMtx4(const Heading, Pitch, Bank: VectorFloat): TMatrix4; overload;

{ Creates 3D rotation matrix based on parameters similar to flight dynamics, specifically heading, pitch and Bank.
  The components are taken from the specified vector with Y corresponding to heading, X to pitch and Z to bank. }
function HeadingPitchBankMtx4(const Vector: TVector3): TMatrix4; overload;

{ Creates 3D rotation matrix based on parameters similar to flight dynamics, specifically yaw, pitch and roll.
  Each of the components is specified individually. }
function YawPitchRollMtx4(const Yaw, Pitch, Roll: VectorFloat): TMatrix4; overload;

{ Creates 3D rotation matrix based on parameters similar to flight dynamics, specifically yaw, pitch and roll.
  The components are taken from the specified vector with Y corresponding to yaw, X to pitch and Z to roll. }
function YawPitchRollMtx4(const Vector: TVector3): TMatrix4; overload;

{ Creates a reflection matrix specified by the given vector defining the orientation of the reflection. }
function ReflectMtx4(const Axis: TVector3): TMatrix4;

{ Creates a view matrix that is defined by the camera's position, its target and vertical axis or "ceiling". }
function LookAtMtx4(const Origin, Target, Ceiling: TVector3): TMatrix4;

{ Creates perspective projection matrix defined by a field of view on Y axis. This is a common way for typical 3D
  applications. In 3D shooters special care is to be taken because on wide-screen monitors the visible area will be
  bigger when using this constructor. The parameters that define the viewed range are important for defining the
  precision of the depth transformation or a depth-buffer.
    @param(FieldOfView The camera's field of view in radians. For example Pi/4.)
    @param(AspectRatio The screen's aspect ratio. Can be calculated as y/x.)
    @param(MinRange The closest range at which the scene will be viewed.)
    @param(MaxRange The farthest range at which the scene will be viewed.) }
function PerspectiveFOVYMtx4(const FieldOfView, AspectRatio, MinRange, MaxRange: VectorFloat): TMatrix4;

{ Creates perspective projection matrix defined by a field of view on X axis. In 3D shooters the field of view
  needs to be adjusted to allow more visible area on wide-screen monitors. The parameters that define the viewed
  range are important for defining the precision of the depth transformation or a depth-buffer.
    @param(FieldOfView The camera's field of view in radians. For example Pi/4.)
    @param(AspectRatio The screen's aspect ratio. Can be calculated as x/y.)
    @param(MinRange The closest range at which the scene will be viewed.)
    @param(MaxRange The farthest range at which the scene will be viewed.) }
function PerspectiveFOVXMtx4(const FieldOfView, AspectRatio, MinRange, MaxRange: VectorFloat): TMatrix4;

{ Creates perspective projection matrix defined by the viewing volume in 3D space. }
function PerspectiveVOLMtx4(const Width, Height, MinRange, MaxRange: VectorFloat): TMatrix4;

{ Creates perspective projection matrix defined by the individual axis's boundaries. }
function PerspectiveBDSMtx4(const Left, Right, Top, Bottom, MinRange, MaxRange: VectorFloat): TMatrix4;

{ Creates orthogonal projection matrix defined by the viewing volume in 3D space. }
function OrthogonalVOLMtx4(const Width, Height, MinRange, MaxRange: VectorFloat): TMatrix4;

{ Creates orthogonal projection matrix defined by the individual axis's boundaries. }
function OrthogonalBDSMtx4(const Left, Right, Top, Bottom, MinRange, MaxRange: VectorFloat): TMatrix4;

{ Returns transposed 3D matrix. That is, rows become columns and vice-versa. }
function TransposeMtx4(const Matrix: TMatrix4): TMatrix4;

{ Calculates adjoint values for given 3D matrix. }
function AdjointMtx4(const Matrix: TMatrix4): TMatrix4;

{ Calculates inverse of given matrix. In other words, the new matrix will "undo" any transformations that the given
  matrix would have made. }
function InvertMtx4(const Matrix: TMatrix4): TMatrix4;

{$ENDREGION}

type
    PPXLLIght = ^TPXLLIght;
    TPXLLIght = record
      pos: TVector4;
      color: TVector4;
      param: TVector4; //x = ambience, y = point
    end;

{$REGION 'TQuaternion declarations'}

type
  { This type is used to pass @link(TQuaternion) by reference. }
  PQuaternion = ^TQuaternion;

  { 3D quaternion }
  TQuaternion = record
    { Individual quaternion values. }
    X, Y, Z, W: VectorFloat;

    { @exclude } class operator Multiply(const Quaternion1, Quaternion2: TQuaternion): TQuaternion;
    { @exclude } class operator Implicit(const Quaternion: TQuaternion): TMatrix4;
    { @exclude } class operator Explicit(const Matrix: TMatrix4): TQuaternion;
  end;

const
  { Identity quaternion that can be used to specify an object with no rotation. }
  IdentityQuat: TQuaternion = (X: 0.0; Y: 0.0; Z: 0.0; W: 1.0);

{ Creates 3D quaternion containing rotation around X axis with given angle (in radians). }
function RotateAboutXQuat(const Angle: VectorFloat): TQuaternion;

{ Creates 3D quaternion containing rotation around Y axis with given angle (in radians). }
function RotateAboutYQuat(const Angle: VectorFloat): TQuaternion;

{ Creates 3D quaternion containing rotation around Z axis with given angle (in radians). }
function RotateAboutZQuat(const Angle: VectorFloat): TQuaternion;

{ Creates 3D quaternion containing rotation around an arbitrary axis with given angle (in radians). }
function RotateAboutAxisQuat(const Axis: TVector3; const Angle: VectorFloat): TQuaternion;

{ Creates 3D quaternion setup to perform Object-To-Inertial rotation using the angles specified in Euler format. }
function RotateObjectToIntertialQuat(const Heading, Pitch, Bank: VectorFloat): TQuaternion;

{ Creates 3D quaternion setup to perform Inertial-To-Object rotation using the angles specified in Euler format. }
function RotateInertialToObjectQuat(const Heading, Pitch, Bank: VectorFloat): TQuaternion;

{ Returns the magnitude of given quaternion. }
function LengthQuat(const Quat: TQuaternion): VectorFloat;

{ Normalizes the given quaternion. Note that normally quaternions are always normalized (of course, within limits
  of numerical precision). This function is provided mainly to combat floating point "error creep", which occurs
  after many successive quaternion operations. }
function NormalizeQuat(const Quat: TQuaternion): TQuaternion;

{ Returns rotational angle "theta" that is present in current quaternion. }
function RotationAngleQuat(const Quat: TQuaternion): VectorFloat;

{ Returns rotational axis that is present in current quaternion. }
function RotationAxisQuat(const Quat: TQuaternion): TVector3;

{ Computes the dot product of two given quaternions. }
function DotQuat(const Quat1, Quat2: TQuaternion): VectorFloat;

{ Applies spherical linear interpolation between current and given quaternions.
    @param(Quat1 Source quaternion to be used in the interpolation)
    @param(Quat2 Destination quaternion to be used in the interpolation)
    @param(Theta The mixture of the two quaternions with range of [0..1].) }
function SlerpQuat(const Quat1, Quat2: TQuaternion; const Theta: VectorFloat): TQuaternion;

{ Computes quaternion's conjugate. The resulting quaternion has opposite rotation to the original quaternion. }
function ConjugateQuat(const Quat: TQuaternion): TQuaternion;

{ Computes exponentiation of the given quaternion. }
function ExpQuat(const Quat: TQuaternion; const Exponent: VectorFloat): TQuaternion;

{$ENDREGION}
{$REGION 'TIntRect declarations'}
type
  { This type is used to pass @link(TIntRect) by reference. }
  PIntRect = ^TIntRect;

  { General-purpose integer rectangle type defined by four margins in range of [Left, Right) and [Top, Bottom). }
  TIntRect = record
  private
    function GetWidth: VectorInt;
    procedure SetWidth(const Value: VectorInt);
    function GetHeight: VectorInt;
    procedure SetHeight(const Value: VectorInt);
    function GetSize: TPoint2px;
    procedure SetSize(const Value: TPoint2px);
    function GetEmpty: Boolean;
  public
    { @exclude } class operator Equal(const Rect1, Rect2: TIntRect): Boolean;
    { @exclude } class operator NotEqual(const Rect1, Rect2: TIntRect): Boolean; inline;

    { Width of rectangle in respect to its left position. }
    property Width: VectorInt read GetWidth write SetWidth;

    { Height of rectangle in respect to its top position. }
    property Height: VectorInt read GetHeight write SetHeight;

    { Size of rectangle in respect to its top/left position. }
    property Size: TPoint2px read GetSize write SetSize;

    { Indicates whether the rectangle is empty, that is, having @link(Width) and @italic(Height) of zero or less. }
    property Empty: Boolean read GetEmpty;

    case Integer of
      0:
        (
          { Left margin of the rectangle (inclusive). }
          Left: VectorInt;

          { Top margin of the rectangle (inclusive). }
          Top: VectorInt;

          { Right margin of the rectangle (exclusive). }
          Right: VectorInt;

          { Bottom margin of the rectangle (exclusive). }
          Bottom: VectorInt;
        );
      1:
        (
          { Top/left point of the rectangle (inclusive). }
          TopLeft: TPoint2px;

          { Bottom/right point of the rectangle (exclusive). }
          BottomRight: TPoint2px;
        );
      2:
        { Individual margins represented as an array. }
        (Values: array[0..3] of VectorInt);
      3:
        { Top/left and bottom/right rectangle points represented as an array. }
        (Bounds: array[0..1] of TPoint2px);
  end;

const
  { Zero (empty) rectangle with integer coordinates. }
  ZeroIntRect: TIntRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

{ Creates rectangle based on top/left position, width and height. }
function IntRect(const Left, Top, Width, Height: VectorInt): TIntRect; overload;

{ Creates rectangle based on top/left position and size. }
function IntRect(const Origin, Size: TPoint2px): TIntRect; overload;

{ Creates rectangle based on individual margin bounds. }
function IntRectBDS(const Left, Top, Right, Bottom: VectorInt): TIntRect; overload;

{ Creates rectangle based on top/left and bottom/right margin bounds. }
function IntRectBDS(const TopLeft, BottomRight: TPoint2px): TIntRect; overload;

{ Returns @True when point is inside specified rectangle or @False otherwise. }
function PointInRect(const Point: TPoint2px; const Rect: TIntRect): Boolean; overload;

{ Returns @True when given rectangle is contained within another rectangle or @False otherwise. }
function RectInRect(const Rect1, Rect2: TIntRect): Boolean; overload;

{ Returns @True when two specified rectangles overlap or @False otherwise. }
function OverlapRect(const Rect1, Rect2: TIntRect): Boolean; overload;

{ Calculates rectangle that results from intersection of given two rectangles. }
function IntersectRect(const Rect1, Rect2: TIntRect): TIntRect; overload;

{ Calculates rectangle that results from union of given two rectangles. }
function UnionRect(const Rect1, Rect2: TIntRect): TIntRect; overload;

{ Displaces given rectangle by certain offset. }
function OffsetRect(const Rect: TIntRect; const Delta: TPoint2px): TIntRect; overload;

{ Displaces given rectangle by each of offset values. }
function OffsetRect(const Rect: TIntRect; const DeltaX, DeltaY: VectorInt): TIntRect; overload; inline;

{ Decrements @italic(Left) and @italic(Top) of given rectangle and increments @italic(Right) and @italic(Bottom) by
  given offset. }
function InflateRect(const Rect: TIntRect; const Delta: TPoint2px): TIntRect; overload;

{ Decrements @italic(Left) and @italic(Top) of given rectangle and increments @italic(Right) and @italic(Bottom) by
  each of corresponding offset values. }
function InflateRect(const Rect: TIntRect; const DeltaX, DeltaY: VectorInt): TIntRect; overload;

{$ENDREGION}
{$REGION 'TFloatRect declarations'}

type
  { This type is used to pass @link(TFloatRect) by reference. }
  PFloatRect = ^TFloatRect;

  { General-purpose floating-point rectangle type defined by four margins in range of [Left, Right) and [Top, Bottom). }
  TFloatRect = record
  private
    function GetWidth: VectorFloat;
    procedure SetWidth(const Value: VectorFloat);
    function GetHeight: VectorFloat;
    procedure SetHeight(const Value: VectorFloat);
    function GetSize: TPoint2;
    procedure SetSize(const Value: TPoint2);
    function GetEmpty: Boolean;
  public
    { @exclude } class operator Equal(const Rect1, Rect2: TFloatRect): Boolean;
    { @exclude } class operator NotEqual(const Rect1, Rect2: TFloatRect): Boolean; inline;

    { Width of rectangle in respect to its left position. }
    property Width: VectorFloat read GetWidth write SetWidth;

    { Height of rectangle in respect to its top position. }
    property Height: VectorFloat read GetHeight write SetHeight;

    { Size of rectangle in respect to its top/left position. }
    property Size: TPoint2 read GetSize write SetSize;

    { Indicates whether the rectangle is empty, that is, having @link(Width) and @italic(Height) of zero or less. }
    property Empty: Boolean read GetEmpty;

    { Converts floating-point rectangle to integer rectangle by rounding margins down. }
    function ToIntRect: TIntRect;

    case Integer of
      0:
        (
          { Left margin of the rectangle (inclusive). }
          Left: VectorFloat;

          { Top margin of the rectangle (inclusive). }
          Top: VectorFloat;

          { Right margin of the rectangle (exclusive). }
          Right: VectorFloat;

          { Bottom margin of the rectangle (exclusive). }
          Bottom: VectorFloat;
        );
      1:
        (
          { Top/left point of the rectangle (inclusive). }
          TopLeft: TPoint2;

          { Bottom/right point of the rectangle (exclusive). }
          BottomRight: TPoint2;
        );
      2:
        { Individual margins represented as an array. }
        (Values: array[0..3] of VectorFloat);
      3:
        { Top/left and bottom/right rectangle points represented as an array. }
        (Bounds: array[0..1] of TPoint2);
  end;

const
  { Zero (empty) rectangle with floating-point coordinates. }
  ZeroFloatRect: TFloatRect = (Left: 0.0; Top: 0.0; Right: 0.0; Bottom: 0.0);

{ Creates rectangle based on top/left position, width and height. }
function FloatRect(const Left, Top, Width, Height: VectorFloat): TFloatRect; overload;

{ Creates rectangle based on top/left position and size. }
function FloatRect(const Origin, Size: TPoint2): TFloatRect; overload;

{ Creates rectangle based on individual margin bounds. }
function FloatRectBDS(const Left, Top, Right, Bottom: VectorFloat): TFloatRect; overload;

{ Creates rectangle based on top/left and bottom/right margin bounds. }
function FloatRectBDS(const TopLeft, BottomRight: TPoint2): TFloatRect; overload;

{ Returns @True when point is inside specified rectangle or @False otherwise. }
function PointInRect(const Point: TPoint2; const Rect: TFloatRect): Boolean; overload;

{ Returns @True when given rectangle is contained within another rectangle or @False otherwise. }
function RectInRect(const Rect1, Rect2: TFloatRect): Boolean; overload;

{ Returns @True when two specified rectangles overlap or @False otherwise. }
function OverlapRect(const Rect1, Rect2: TFloatRect): Boolean; overload;

{ Calculates rectangle that results from intersection of given two rectangles. }
function IntersectRect(const Rect1, Rect2: TFloatRect): TFloatRect; overload;

{ Calculates rectangle that results from union of given two rectangles. }
function UnionRect(const Rect1, Rect2: TFloatRect): TFloatRect; overload;

{ Displaces given rectangle by certain offset. }
function OffsetRect(const Rect: TFloatRect; const Delta: TPoint2): TFloatRect; overload;

{ Displaces given rectangle by each of offset values. }
function OffsetRect(const Rect: TFloatRect; const DeltaX, DeltaY: VectorFloat): TFloatRect; overload; inline;

{ Decrements @italic(Left) and @italic(Top) of given rectangle and increments @italic(Right) and @italic(Bottom) by
  given offset. }
function InflateRect(const Rect: TFloatRect; const Delta: TPoint2): TFloatRect; overload;

{ Decrements @italic(Left) and @italic(Top) of given rectangle and increments @italic(Right) and @italic(Bottom) by
  each of corresponding offset values. }
function InflateRect(const Rect: TFloatRect; const DeltaX, DeltaY: VectorFloat): TFloatRect; overload;

{$ENDREGION}
{$REGION 'TFloatRect4 declarations'}

type
  { This type is used to pass @link(TFloatRect4) by reference. }
  PFloatRect4 = ^TFloatRect4;

  { Special floating-point quadrilateral defined by four vertices starting from top/left in clockwise order.
    This is typically used for rendering color filled and textured quads. }
  TFloatRect4 = record
    { @exclude } class operator Equal(const Rect1, Rect2: TFloatRect4): Boolean;
    { @exclude } class operator NotEqual(const Rect1, Rect2: TFloatRect4): Boolean; inline;

    function Width: vectorfloat;inline;
    function Height: vectorfloat;inline;
    case Integer of
      0:
        (
          { Top/left vertex position. }
          TopLeft: TPoint2;

          { Top/right vertex position. }
          TopRight: TPoint2;

          { Bottom/right vertex position. }
          BottomRight: TPoint2;

          { Bottom/left vertex position. }
          BottomLeft: TPoint2;
        );
      1:
        { Quadrilateral vertices represented as an array. }
        (Values: array[0..3] of TPoint2);

    end;


  TFloatRect4Helper = record helper for TFloatRect4
    procedure FromRectF(r: TRectF);
    function ToRectF: TRectF;
  end;

  TPoint2Helper = record helper for TPOint2
    function ToPOintF: TPOintF;
    procedure FromPOintF(p: TPointF);
  end;

{ Creates quadrilateral with individually specified vertex coordinates. }
function FloatRect4(const TopLeftX, TopLeftY, TopRightX, TopRightY, BottomRightX, BottomRightY, BottomLeftX,
  BottomLeftY: VectorFloat): TFloatRect4; overload;

{ Creates quadrilateral with individually specified vertices. }
function FloatRect4(const TopLeft, TopRight, BottomRight, BottomLeft: TPoint2): TFloatRect4; overload;

{ Creates quadrilateral rectangle with top/left position, width and height. }
function FloatRect4(const Left, Top, Width, Height: VectorFloat): TFloatRect4; overload;

{ Creates quadrilateral rectangle from specified floating-point rectangle. }
function FloatRect4(const Rect: TFloatRect): TFloatRect4; overload;

{ Creates quadrilateral rectangle from specified integer rectangle. }
function FloatRect4(const Rect: TIntRect): TFloatRect4; overload;

{ Creates quadrilateral with the specified top left corner and the given dimensions, which are scaled by the
  provided coefficient. }
function FloatRect4S(const Left, Top, Width, Height, Scale: VectorFloat;
  const Centered: Boolean = True): TFloatRect4;

{ Creates quadrilateral specified by its dimensions. The rectangle is then rotated and scaled around the specified
  middle point (assumed to be inside rectangle's dimensions) and placed in center of the specified origin. }
function FloatRect4R(const RotationOrigin, Size, RotationCenter: TPoint2; const Angle: VectorFloat;
  const Scale: VectorFloat = 1.0): TFloatRect4;

{ Creates quadrilateral specified by its dimensions. The rectangle is then rotated and scaled around its center
  and placed at the specified origin. }
function FloatRect4RC(const RotationOrigin, Size: TPoint2; const Angle: VectorFloat;
  const Scale: VectorFloat = 1.0): TFloatRect4; inline;

{ Creates quadrilateral specified by top-left corner and size. The rectangle is then rotated and scaled around the
  specified middle point (assumed to be inside rectangle's dimensions) and placed in the center of the specified
  origin. The difference between this method and @link(FloatRect4R) is that the rotation does not preserve centering
  of the rectangle in case where middle point is not actually located in the middle. }
function FloatRect4RTL(const TopLeft, Size, RotationCenter: TPoint2; const Angle: VectorFloat;
  const Scale: VectorFloat = 1.0): TFloatRect4; inline;

{ Rescales vertices of the given quadrilateral by provided coefficient, optionally centering them around zero origin. }
function ScaleRect4(const Rect: TFloatRect4; const Scale: VectorFloat; const Centered: Boolean = True): TFloatRect4;

{ Creates quadrilateral from another quadrilateral but having left vertices exchanged with the right ones,
  effectively mirroring it horizontally. }
function MirrorRect4(const Rect: TFloatRect4): TFloatRect4;

{ Creates quadrilateral from another quadrilateral but having top vertices exchanged with the bottom ones,
  effectively flipping it vertically. }
function FlipRect4(const Rect: TFloatRect4): TFloatRect4;

{ Transforms (multiplies) vertices of given quadrilateral by the specified matrix. }
function TransformRect4(const Rect: TFloatRect4; const Matrix: TMatrix3): TFloatRect4;

{ Displaces vertices of given quadrilateral by the specified offset. }
function OffsetRect4(const Rect: TFloatRect4; const Delta: TPoint2): TFloatRect4; overload;

{ Displaces vertices of given quadrilateral by the specified displacement values. }
function OffsetRect4(const Rect: TFloatRect4; const DeltaX, DeltaY: VectorFloat): TFloatRect4; overload; inline;

{ Returns @True when point is inside specified quadrilateral or @False otherwise. }
function PointInRect4(const Point: TPoint2; const Rect: TFloatRect4): Boolean;

{$ENDREGION}
{$REGION 'Global declarations'}

{ Interpolates between two values linearly, where @italic(Theta) must be specified in [0..1] range. }
function Lerp(const Value1, Value2, Theta: VectorFloat): VectorFloat;
{$IFNDEF PASDOC} overload; {$ENDIF}

{$IF SIZEOF(VectorFloat) > 4} { @exclude }
function Lerp(const Value1, Value2, Theta: Single): Single; overload;
{$ENDIF}

{ Interpolates between four values using Catmull-Rom spline, where @italic(Theta) must be in [0..1] range. }
function CatmullRom(const PredValue1, Value1, Value2, SuccValue2, Theta: VectorFloat): VectorFloat;
{$IFNDEF PASDOC} overload; {$ENDIF}

{$IF SIZEOF(VectorFloat) > 4} { @exclude }
function CatmullRom(const PredValue1, Value1, Value2, SuccValue2, Theta: Single): Single; overload;
{$ENDIF}

{ Ensures that the given value stays within specified range limit, clamping it if necessary. }
function Saturate(const Value, MinLimit, MaxLimit: VectorFloat): VectorFloat; inline;
{$IFNDEF PASDOC} overload; {$ENDIF}

{$IF SIZEOF(VectorFloat) > 4} { @exclude }
function Saturate(const Value, MinLimit, MaxLimit: Single): Single; overload; inline;
{$ENDIF}

{ Ensures that the given value stays within specified range limit, clamping it if necessary. }
function Saturate(const Value, MinLimit, MaxLimit: VectorInt): VectorInt; inline;
{$IFNDEF PASDOC} overload; {$ENDIF}

{$IF SIZEOF(VectorInt) <> 4} { @exclude }
function Saturate(const Value, MinLimit, MaxLimit: LongInt): LongInt; overload; inline;
{$ENDIF}

{ Returns @True if the specified value is a power of two or @False otherwise. }
function IsPowerOfTwo(const Value: VectorInt): Boolean;

{ Returns the least power of two greater or equal to the specified value. }
function CeilPowerOfTwo(const Value: VectorInt): VectorInt;

{ Returns the greatest power of two lesser or equal to the specified value. }
function FloorPowerOfTwo(const Value: VectorInt): VectorInt;

{ Returns @True if specified point is inside the triangle specified by given three vertices or @False otherwise. }
function PointInTriangle(const Point, Vertex1, Vertex2, Vertex3: TPoint2px): Boolean; overload;

{ Returns @True if specified point is inside the triangle specified by given three vertices or @False otherwise. }
function PointInTriangle(const Point, Vertex1, Vertex2, Vertex3: TPoint2): Boolean; overload;

{ Calculates random number in [0..1] range using gaussian distribution with higher probability located near 0. }
function RandomGaussValueStart: VectorFloat;

{ Calculates random number in [0..1] range using gaussian distribution with higher probability located near 1. }
function RandomGaussValueEnd: VectorFloat;

{ Calculates random number in [0..1] range using gaussian distribution with higher probability located near 0.5. }
function RandomGaussValueMiddle: VectorFloat;

{ Calculates random number in [-1..1] range using gaussian distribution with higher probability located near 0.
  Note that this function does not do limit checks, so numbers outside of [-1..1] range might rarely occur. }
function RandomGaussValueOmni: VectorFloat;

{ Transforms value in range of [0, 1] using sine wave (accelerate, decelerate). }
function SineTransform(const Value: VectorFloat): VectorFloat;

{ Transforms value in range of [0, 1] using sine wave accelerating. The curve starts at 0 with almost zero
  acceleration, but in the end going almost linearly. }
function SineAccelerate(const Value: VectorFloat): VectorFloat;

{ Transforms value in range of [0, 1] using sine wave decelerating. The curve starts accelerating quickly at 0, but
  slowly stops at 1. }
function SineDecelerate(const Value: VectorFloat): VectorFloat;

{ Transforms value in range of [0, 1] to go through full cycle  (0 -> 1 -> 0).
  Note that the curve goes from 0 to 1 and then back to 0. }
function SineCycle(const Value: VectorFloat): VectorFloat;

{ Transforms value in range of [0, 1] to go full cycle through sine function (0 -> 1 -> 0 -> -1 -> 0).}
function SineSymCycle(const Value: VectorFloat): VectorFloat;

{ Takes source and destination sizes, source rectangle and destination position, then applies clipping to ensure that
  final rectangle stays within valid boundaries of both source and destination sizes. Returns @True when the clipping
  was successful or @False when there is nothing left. }
function ClipCoords(const SourceSize, DestSize: TPoint2px; var SourceRect: TIntRect;
  var DestPos: TPoint2px): Boolean; overload;

{ Takes source and destination sizes, source and destination rectangles, then applies clipping to ensure that final
  rectangle stays within valid boundaries of both source and destination sizes. Returns @True when the clipping
  was successful or @False when there is nothing left. }
function ClipCoords(const SourceSize, DestSize: TPoint2; var SourceRect, DestRect: TFloatRect): Boolean; overload;

{$ENDREGION}

implementation

uses
  Math;

{$REGION 'Global Functions'}

const
  TwoPi = Pi * 2.0;
  PiHalf = Pi * 0.5;

function Lerp(const Value1, Value2, Theta: VectorFloat): VectorFloat;
begin
  Result := Value1 + (Value2 - Value1) * Theta;
end;

{$IF SIZEOF(VectorFloat) > 4}
function Lerp(const Value1, Value2, Theta: Single): Single;
begin
  Result := Value1 + (Value2 - Value1) * Theta;
end;
{$ENDIF}

function CatmullRom(const PredValue1, Value1, Value2, SuccValue2, Theta: VectorFloat): VectorFloat;
begin
  Result := ((2.0 * Value1) + Theta * (-PredValue1 + Value2 + Theta * (2.0 * PredValue1 - 5.0 *
    Value1 + 4.0 * Value2 - SuccValue2 + Theta * (-PredValue1 + 3.0 * Value1 - 3.0 * Value2 +
    SuccValue2)))) * 0.5;
end;

{$IF SIZEOF(VectorFloat) > 4}
function CatmullRom(const PredValue1, Value1, Value2, SuccValue2, Theta: Single): Single;
begin
  Result := ((2.0 * Value1) + Theta * (-PredValue1 + Value2 + Theta * (2.0 * PredValue1 - 5.0 *
    Value1 + 4.0 * Value2 - SuccValue2 + Theta * (-PredValue1 + 3.0 * Value1 - 3.0 * Value2 +
    SuccValue2)))) * 0.5;
end;
{$ENDIF}

function Saturate(const Value, MinLimit, MaxLimit: VectorFloat): VectorFloat;
begin
  Result := Value;

  if Result < MinLimit then
    Result := MinLimit;

  if Result > MaxLimit then
    Result := MaxLimit;
end;

{$IF SIZEOF(VectorFloat) > 4}
function Saturate(const Value, MinLimit, MaxLimit: Single): Single;
begin
  Result := Value;

  if Result < MinLimit then
    Result := MinLimit;

  if Result > MaxLimit then
    Result := MaxLimit;
end;
{$ENDIF}

function Saturate(const Value, MinLimit, MaxLimit: VectorInt): VectorInt;
begin
  Result := Value;

  if Result < MinLimit then
    Result := MinLimit;

  if Result > MaxLimit then
    Result := MaxLimit;
end;

{$IF SIZEOF(VectorInt) <> 4}
function Saturate(const Value, MinLimit, MaxLimit: LongInt): LongInt; overload;
begin
  Result := Value;

  if Result < MinLimit then
    Result := MinLimit;

  if Result > MaxLimit then
    Result := MaxLimit;
end;
{$ENDIF}

function IsPowerOfTwo(const Value: VectorInt): Boolean;
begin
  Result := (Value >= 1) and ((Value and (Value - 1)) = 0);
end;

function CeilPowerOfTwo(const Value: VectorInt): VectorInt;
begin
  Result := Round(Power(2, Ceil(Log2(Value))))
end;

function FloorPowerOfTwo(const Value: VectorInt): VectorInt;
begin
  Result := Round(Power(2, Floor(Log2(Value))))
end;

function PointInTriangle(const Point, Vertex1, Vertex2, Vertex3: TPoint2px): Boolean;
var
  Det: VectorInt;
begin
  Det := (Point.Y - Vertex2.Y) * (Vertex3.X - Vertex2.X) - (Point.X - Vertex2.X) * (Vertex3.Y - Vertex2.Y);

  Result := (Det * ((Point.Y - Vertex1.Y) * (Vertex2.X - Vertex1.X) - (Point.X - Vertex1.X) *
    (Vertex2.Y - Vertex1.Y)) > 0) and (Det * ((Point.Y - Vertex3.Y) * (Vertex1.X - Vertex3.X) -
    (Point.X - Vertex3.X) * (Vertex1.Y - Vertex3.Y)) > 0);
end;

function PointInTriangle(const Point, Vertex1, Vertex2, Vertex3: TPoint2): Boolean;
var
  Det: VectorFloat;
begin
  Det := (Point.Y - Vertex2.Y) * (Vertex3.X - Vertex2.X) - (Point.X - Vertex2.X) * (Vertex3.Y - Vertex2.Y);

  Result := (Det * ((Point.Y - Vertex1.Y) * (Vertex2.X - Vertex1.X) - (Point.X - Vertex1.X) *
    (Vertex2.Y - Vertex1.Y)) > 0.0) and (Det * ((Point.Y - Vertex3.Y) * (Vertex1.X - Vertex3.X) -
    (Point.X - Vertex3.X) * (Vertex1.Y - Vertex3.Y)) > 0.0);
end;

function RandomGaussValueStart: VectorFloat;
var
  Phi, Rho: VectorFloat;
begin
  Phi := TwoPi * Random;
  Rho := Sqrt(-2.0 * Ln(1.0 - Random));
  Result := Min(system.Abs(Cos(Phi) * Rho) * 0.5, 1.0);
end;

function RandomGaussValueEnd: VectorFloat;
var
  Phi, Rho: VectorFloat;
begin
  Phi := TwoPi * Random;
  Rho := Sqrt(-2.0 * Ln(1.0 - Random));
  Result := Max(1.0 - system.Abs(Cos(Phi) * Rho) * 0.5, 0.0);
end;

function RandomGaussValueMiddle: VectorFloat;
var
  Phi, Rho: VectorFloat;
begin
  Phi := TwoPi * Random;
  Rho := Sqrt(-2.0 * Ln(1.0 - Random));
  Result := Min(Max(0.25 + 0.25 * (Cos(Phi) * Rho), 0.0), 1.0);
end;

function RandomGaussValueOmni: VectorFloat;
var
  Phi, Rho: VectorFloat;
begin
  Phi := TwoPi * Random;
  Rho := Sqrt(-2.0 * Ln(1.0 - Random));
  Result := Cos(Phi) * Rho;
end;

function SineTransform(const Value: VectorFloat): VectorFloat;
begin
  Result := 0.5 + 0.5 * Sin(Value * Pi - PiHalf);
end;

function SineAccelerate(const Value: VectorFloat): VectorFloat;
begin
  Result := 1.0 + Sin(Value * PiHalf - PiHalf);
end;

function SineDecelerate(const Value: VectorFloat): VectorFloat;
begin
  Result := Sin(Value * PiHalf);
end;

function SineCycle(const Value: VectorFloat): VectorFloat;
begin
  Result := 0.5 + 0.5 * Sin(Value * TwoPi - PiHalf);
end;

function SineSymCycle(const Value: VectorFloat): VectorFloat;
var
  Theta: Single;
begin
  Theta := system.Abs(Frac(Value));

  if Theta < 0.5 then
    Result := SineCycle(Theta * 2.0)
  else
    Result := -SineCycle((Theta - 0.5) * 2.0);
end;

function ClipCoords(const SourceSize, DestSize: TPoint2px; var SourceRect: TIntRect;
  var DestPos: TPoint2px): Boolean; overload;
var
  Delta: VectorInt;
begin
  if SourceRect.Left < 0 then
  begin
    Delta := -SourceRect.Left;
    Inc(SourceRect.Left, Delta);
    Inc(DestPos.X, Delta);
  end;

  if SourceRect.Top < 0 then
  begin
    Delta := -SourceRect.Top;
    Inc(SourceRect.Top, Delta);
    Inc(DestPos.Y, Delta);
  end;

  if SourceRect.Right > SourceSize.X then
    SourceRect.Right := SourceSize.X;

  if SourceRect.Bottom > SourceSize.Y then
    SourceRect.Bottom := SourceSize.Y;

  if DestPos.X < 0 then
  begin
    Delta := -DestPos.X;
    Inc(DestPos.X, Delta);
    Inc(SourceRect.Left, Delta);
  end;

  if DestPos.Y < 0 then
  begin
    Delta := -DestPos.Y;
    Inc(DestPos.Y, Delta);
    Inc(SourceRect.Top, Delta);
  end;

  if DestPos.X + SourceRect.Width > DestSize.X then
  begin
    Delta := DestPos.X + SourceRect.Width - DestSize.X;
    Dec(SourceRect.Right, Delta);
  end;

  if DestPos.Y + SourceRect.Height > DestSize.Y then
  begin
    Delta := DestPos.Y + SourceRect.Height - DestSize.Y;
    Dec(SourceRect.Bottom, Delta);
  end;

  Result := (SourceRect.Width > 0) and (SourceRect.Height > 0);
end;

function ClipCoords(const SourceSize, DestSize: TPoint2; var SourceRect, DestRect: TFloatRect): Boolean; overload;
var
  Delta: VectorFloat;
  Scale: TPoint2;
begin
  if (SourceRect.Width <= 0) or (SourceRect.Height <= 0) or (DestRect.Width <= 0) or (DestRect.Height <= 0) then
    Exit(False);

  Scale.X := DestRect.Width / SourceRect.Width;
  Scale.Y := DestRect.Height / SourceRect.Height;

  if SourceRect.Left < 0 then
  begin
    Delta := -SourceRect.Left;
    SourceRect.Left := SourceRect.Left + Delta;
    DestRect.Left := DestRect.Left + (Delta * Scale.X);
  end;

  if SourceRect.Top < 0 then
  begin
    Delta := -SourceRect.Top;
    SourceRect.Top := SourceRect.Top + Delta;
    DestRect.Top := DestRect.Top + (Delta * Scale.Y);
  end;

  if SourceRect.Right > SourceSize.X then
  begin
    Delta := SourceRect.Right - SourceSize.X;
    SourceRect.Right := SourceRect.Right - Delta;
    DestRect.Right := DestRect.Right - (Delta * Scale.X);
  end;

  if SourceRect.Bottom > SourceSize.Y then
  begin
    Delta := SourceRect.Bottom - SourceSize.Y;
    SourceRect.Bottom := SourceRect.Bottom - Delta;
    DestRect.Bottom := DestRect.Bottom - (Delta * Scale.Y);
  end;

  if DestRect.Left < 0 then
  begin
    Delta := - DestRect.Left;
    DestRect.Left := DestRect.Left + Delta;
    SourceRect.Left := SourceRect.Left + (Delta / Scale.X);
  end;

  if DestRect.Top < 0 then
  begin
    Delta := - DestRect.Top;
    DestRect.Top := DestRect.Top + Delta;
    SourceRect.Top := SourceRect.Top + (Delta / Scale.Y);
  end;

  if DestRect.Right > DestSize.X then
  begin
    Delta := DestRect.Right - DestSize.X;
    DestRect.Right := DestRect.Right - Delta;
    SourceRect.Right := SourceRect.Right - (Delta / Scale.X);
  end;

  if DestRect.Bottom > DestSize.Y then
  begin
    Delta := DestRect.Bottom - DestSize.Y;
    DestRect.Bottom := DestRect.Bottom - Delta;
    SourceRect.Bottom := SourceRect.Bottom - (Delta / Scale.Y);
  end;

  Result := (SourceRect.Width > 0) and (SourceRect.Height > 0) and (DestRect.Width > 0) and (DestRect.Height > 0);
end;

{$ENDREGION}
{$REGION 'TIntGrayAlpha'}

function IntGrayAlpha(const Gray: Integer; const Alpha: Integer): TIntGrayAlpha;
begin
  Result := (TIntGrayAlphaValue(Gray) and $FF) or ((TIntGrayAlphaValue(Alpha) and $FF) shl 8);
end;

function IntGrayAlpha(const Gray, Alpha: VectorFloat): TIntGrayAlpha;
begin
  Result := IntGrayAlpha(Integer(Round(Gray * 255.0)), Integer(Round(Alpha * 255.0)));
end;

function PremultiplyGrayPixel(const Color: TIntGrayAlpha): TIntGrayAlpha;
begin
  Result := (((Color and $FF) * (Color shr 8)) div 255) or  (Color and $FF00);
end;

function UnpremultiplyGrayPixel(const Color: TIntGrayAlpha): TIntGrayAlpha;
begin
  if Color shr 8 > 0 then
    Result := (((Color and $FF) * 255) div (Color shr 8)) or (Color and $FF00)
  else
    Result := Color;
end;

function AddGrayPixels(const Color1, Color2: TIntGrayAlpha): TIntGrayAlpha;
begin
  Result :=
    TIntGrayAlphaValue(Min(Integer(Color1 and $FF) + Integer(Color2 and $FF), 255)) or
    (TIntGrayAlphaValue(Min(Integer((Color1 shr 8) and $FF) + Integer((Color2 shr 8) and $FF), 255)) shl 8);
end;

function SubtractGrayPixels(const Color1, Color2: TIntGrayAlpha): TIntGrayAlpha;
begin
  Result :=
    TIntGrayAlphaValue(Max(Integer(Color1 and $FF) - Integer(Color2 and $FF), 0)) or
    (TIntGrayAlphaValue(Max(Integer((Color1 shr 8) and $FF) - Integer((Color2 shr 8) and $FF), 0)) shl 8);
end;

function MultiplyGrayPixels(const Color1, Color2: TIntGrayAlpha): TIntGrayAlpha;
begin
  Result :=
    TIntGrayAlphaValue((Integer(Color1 and $FF) * Integer(Color2 and $FF)) div 255) or
    (TIntGrayAlphaValue((Integer((Color1 shr 8) and $FF) * Integer((Color2 shr 8) and $FF)) div 255) shl 8);
end;

function AverageGrayPixels(const Color1, Color2: TIntGrayAlpha): TIntGrayAlpha;
begin
  Result :=
    (((Color1 and $FF) + (Color2 and $FF)) div 2) or
    (((((Color1 shr 8) and $FF) + ((Color2 shr 8) and $FF)) div 2) shl 8);
end;

function BlendGrayPixels(const Color1, Color2: TIntGrayAlpha; const Alpha: Integer): TIntGrayAlpha;
begin
  Result :=
    TIntGrayAlphaValue(Integer(Color1 and $FF) + (((Integer(Color2 and $FF) -
    Integer(Color1 and $FF)) * Alpha) div 255)) or

    (TIntGrayAlphaValue(Integer((Color1 shr 8) and $FF) + (((Integer((Color2 shr 8) and $FF) -
    Integer((Color1 shr 8) and $FF)) * Alpha) div 255)) shl 8);
end;

function LerpGrayPixels(const Color1, Color2: TIntGrayAlpha; const Alpha: VectorFloat): TIntGrayAlpha;
begin
  Result :=
    TIntGrayAlphaValue(Integer(Color1 and $FF) + Round((Integer(Color2 and $FF) -
    Integer(Color1 and $FF)) * Alpha)) or

    (TIntGrayAlphaValue(Integer((Color1 shr 8) and $FF) + Round((Integer((Color2 shr 8) and $FF) -
    Integer((Color1 shr 8) and $FF)) * Alpha)) shl 8);
end;

{$ENDREGION}
{$REGION 'TFloatGrayAlpha'}

class operator TFloatGrayAlpha.Add(const Color1, Color2: TFloatGrayAlpha): TFloatGrayAlpha;
begin
  Result.Gray := Color1.Gray + Color2.Gray;
  Result.Alpha := Color1.Alpha + Color2.Alpha;
end;

class operator TFloatGrayAlpha.Subtract(const Color1, Color2: TFloatGrayAlpha): TFloatGrayAlpha;
begin
  Result.Gray := Color1.Gray - Color2.Gray;
  Result.Alpha := Color1.Alpha - Color2.Alpha;
end;

class operator TFloatGrayAlpha.Multiply(const Color1, Color2: TFloatGrayAlpha): TFloatGrayAlpha;
begin
  Result.Gray := Color1.Gray * Color2.Gray;
  Result.Alpha := Color1.Alpha * Color2.Alpha;
end;

class operator TFloatGrayAlpha.Divide(const Color1, Color2: TFloatGrayAlpha): TFloatGrayAlpha;
begin
  Result.Gray := Color1.Gray / Color2.Gray;
  Result.Alpha := Color1.Alpha / Color2.Alpha;
end;

class operator TFloatGrayAlpha.Multiply(const Color: TFloatGrayAlpha; const Theta: VectorFloat): TFloatGrayAlpha;
begin
  Result.Gray := Color.Gray * Theta;
  Result.Alpha := Color.Alpha * Theta;
end;

class operator TFloatGrayAlpha.Multiply(const Theta: VectorFloat; const Color: TFloatGrayAlpha): TFloatGrayAlpha;
begin
  Result.Gray := Theta * Color.Gray;
  Result.Alpha := Theta * Color.Alpha;
end;

class operator TFloatGrayAlpha.Divide(const Color: TFloatGrayAlpha; const Theta: VectorFloat): TFloatGrayAlpha;
begin
  Result.Gray := Color.Gray / Theta;
  Result.Alpha := Color.Alpha / Theta;
end;

class operator TFloatGrayAlpha.Equal(const Color1, Color2: TFloatGrayAlpha): Boolean;
begin
  Result := (system.Abs(Color1.Gray - Color2.Gray) < VectorEpsilon) and (system.Abs(Color1.Alpha - Color2.Alpha) < VectorEpsilon);
end;

class operator TFloatGrayAlpha.NotEqual(const Color1, Color2: TFloatGrayAlpha): Boolean;
begin
  Result := not (Color1 = Color2);
end;

function FloatGrayAlpha(const Color: TIntGrayAlpha): TFloatGrayAlpha;
begin
  Result.Gray := (Color and $FF) / 255.0;
  Result.Alpha := ((Color shr 8) and $FF) / 255.0;
end;

function FloatGrayAlpha(const Gray, Alpha: VectorFloat): TFloatGrayAlpha;
begin
  Result.Gray := Gray;
  Result.Alpha := Alpha;
end;

function InvertGrayColor(const Color: TFloatGrayAlpha): TFloatGrayAlpha;
begin
  Result.Gray := 1.0 - Color.Gray;
  Result.Alpha := 1.0 - Color.Alpha;
end;

function PremultiplyGrayColor(const Color: TFloatGrayAlpha): TFloatGrayAlpha;
begin
  Result.Gray := Color.Gray * Color.Alpha;
  Result.Alpha := Color.Alpha;
end;

function UnpremultiplyGrayColor(const Color: TFloatGrayAlpha): TFloatGrayAlpha;
begin
  if Color.Alpha > 0.0 then
  begin
    Result.Gray := Color.Gray / Color.Alpha;
    Result.Alpha := Color.Alpha;
  end
  else
    Result := Color;
end;

function AverageGrayColors(const Color1, Color2: TFloatGrayAlpha): TFloatGrayAlpha;
begin
  Result.Gray := (Color1.Gray + Color2.Gray) * 0.5;
  Result.Alpha := (Color1.Alpha + Color2.Alpha) * 0.5;
end;

function LerpGrayColors(const Color1, Color2: TFloatGrayAlpha; const Alpha: VectorFloat): TFloatGrayAlpha;
begin
  Result.Gray := Color1.Gray + (Color2.Gray - Color1.Gray) * Alpha;
  Result.Alpha := Color1.Alpha + (Color2.Alpha - Color1.Alpha) * Alpha;
end;

function SaturateGrayColor(const Color: TFloatGrayAlpha): TFloatGrayAlpha;
begin
  Result.Gray := Saturate(Color.Gray, 0.0, 1.0);
  Result.Alpha := Saturate(Color.Alpha, 0.0, 1.0);
end;

function FloatToIntGrayAlpha(const Color: TFloatGrayAlpha): TIntGrayAlpha;
begin
  Result := Cardinal(Round(Color.Gray * 255.0)) or (Cardinal(Round(Color.Alpha * 255.0)) shl 8);
end;

{$ENDREGION}
{$REGION 'TIntColor'}

function IntColor(const Color: TIntColor; const Alpha: Integer): TIntColor;
begin
  Result := (Color and $00FFFFFF) or TIntColorValue((Integer(Color shr 24) * Alpha) div 255) shl 24;
end;

function IntColor(const Color: TIntColor; const Alpha: VectorFloat): TIntColor;
begin
  Result := IntColor(Color, Integer(Round(Alpha * 255.0)));
end;

function IntColor(const Color: TIntColor; const Gray, Alpha: Integer): TIntColor;
begin
  Result := TIntColorValue((Integer(Color and $FF) * Gray) div 255) or
    (TIntColorValue((Integer((Color shr 8) and $FF) * Gray) div 255) shl 8) or
    (TIntColorValue((Integer((Color shr 16) and $FF) * Gray) div 255) shl 16) or
    (TIntColorValue((Integer((Color shr 24) and $FF) * Alpha) div 255) shl 24);
end;

function IntColor(const Color: TIntColor; const Gray, Alpha: VectorFloat): TIntColor;
begin
  Result := IntColor(Color, Integer(Round(Gray * 255.0)), Integer(Round(Alpha * 255.0)));
end;

function IntColorRGB(const Red, Green, Blue: Integer; const Alpha: Integer = 255): TIntColor;
begin
  Result := TIntColorValue(Blue) or (TIntColorValue(Green) shl 8) or (TIntColorValue(Red) shl 16) or
    (TIntColorValue(Alpha) shl 24);
end;

function IntColorGray(const Gray: Integer; const Alpha: Integer = 255): TIntColor;
begin
  Result := ((TIntColorValue(Gray) and $FF) or ((TIntColorValue(Gray) and $FF) shl 8) or
    ((TIntColorValue(Gray) and $FF) shl 16)) or ((TIntColorValue(Alpha) and $FF) shl 24);
end;

function IntColorGray(const Gray: VectorFloat; const Alpha: VectorFloat = 1.0): TIntColor;
begin
  Result := IntColorGray(Integer(Round(Gray * 255.0)), Integer(Round(Alpha * 255.0)));
end;

function IntColorAlpha(const Alpha: Integer): TIntColor;
begin
  Result := $00FFFFFF or ((TIntColorValue(Alpha) and $FF) shl 24);
end;

function IntColorAlpha(const Alpha: VectorFloat): TIntColor;
begin
  Result := IntColorAlpha(Integer(Round(Alpha * 255.0)));
end;

function DisplaceRB(const Color: TIntColor): TIntColor;
begin
  Result := ((Color and $FF) shl 16) or (Color and $FF00FF00) or ((Color shr 16) and $FF);
end;

function InvertPixel(const Color: TIntColor): TIntColor;
begin
  Result := (255 - (Color and $FF)) or ((255 - ((Color shr 8) and $FF)) shl 8) or
    ((255 - ((Color shr 16) and $FF)) shl 16) or ((255 - ((Color shr 24) and $FF)) shl 24);
end;

function PremultiplyAlpha(const Color: TIntColor): TIntColor;
begin
  Result :=
    (((Color and $FF) * (Color shr 24)) div 255) or
    (((((Color shr 8) and $FF) * (Color shr 24)) div 255) shl 8) or
    (((((Color shr 16) and $FF) * (Color shr 24)) div 255) shl 16) or
    (Color and $FF000000);
end;

function UnpremultiplyAlpha(const Color: TIntColor): TIntColor;
var
  Alpha: Cardinal;
begin
  Alpha := Color shr 24;

  if Alpha > 0 then
    Result := (((Color and $FF) * 255) div Alpha) or (((((Color shr 8) and $FF) * 255) div Alpha) shl 8) or
      (((((Color shr 16) and $FF) * 255) div Alpha) shl 16) or (Color and $FF000000)
  else
    Result := Color;
end;

function AddPixels(const Color1, Color2: TIntColor): TIntColor;
begin
  Result :=
    TIntColorValue(Min(Integer(Color1 and $FF) + Integer(Color2 and $FF), 255)) or
    (TIntColorValue(Min(Integer((Color1 shr 8) and $FF) + Integer((Color2 shr 8) and $FF), 255)) shl 8) or
    (TIntColorValue(Min(Integer((Color1 shr 16) and $FF) + Integer((Color2 shr 16) and $FF), 255)) shl 16) or
    (TIntColorValue(Min(Integer((Color1 shr 24) and $FF) + Integer((Color2 shr 24) and $FF), 255)) shl 24);
end;

function SubtractPixels(const Color1, Color2: TIntColor): TIntColor;
begin
  Result :=
    TIntColorValue(Max(Integer(Color1 and $FF) - Integer(Color2 and $FF), 0)) or
    (TIntColorValue(Max(Integer((Color1 shr 8) and $FF) - Integer((Color2 shr 8) and $FF), 0)) shl 8) or
    (TIntColorValue(Max(Integer((Color1 shr 16) and $FF) - Integer((Color2 shr 16) and $FF), 0)) shl 16) or
    (TIntColorValue(Max(Integer((Color1 shr 24) and $FF) - Integer((Color2 shr 24) and $FF), 0)) shl 24);
end;

function MultiplyPixels(const Color1, Color2: TIntColor): TIntColor;
begin
  Result :=
    TIntColorValue((Integer(Color1 and $FF) * Integer(Color2 and $FF)) div 255) or
    (TIntColorValue((Integer((Color1 shr 8) and $FF) * Integer((Color2 shr 8) and $FF)) div 255) shl 8) or
    (TIntColorValue((Integer((Color1 shr 16) and $FF) * Integer((Color2 shr 16) and $FF)) div 255) shl 16) or
    (TIntColorValue((Integer((Color1 shr 24) and $FF) * Integer((Color2 shr 24) and $FF)) div 255) shl 24);
end;

function AveragePixels(const Color1, Color2: TIntColor): TIntColor;
begin
  Result :=
    (((Color1 and $FF) + (Color2 and $FF)) div 2) or
    (((((Color1 shr 8) and $FF) + ((Color2 shr 8) and $FF)) div 2) shl 8) or
    (((((Color1 shr 16) and $FF) + ((Color2 shr 16) and $FF)) div 2) shl 16) or
    (((((Color1 shr 24) and $FF) + ((Color2 shr 24) and $FF)) div 2) shl 24);
end;

function AverageFourPixels(const Color1, Color2, Color3, Color4: TIntColor): TIntColor;
begin
  Result :=
    (((Color1 and $FF) + (Color2 and $FF) + (Color3 and $FF) + (Color4 and $FF)) div 4) or

    (((((Color1 shr 8) and $FF) + ((Color2 shr 8) and $FF) + ((Color3 shr 8) and $FF) +
    ((Color4 shr 8) and $FF)) div 4) shl 8) or

    (((((Color1 shr 16) and $FF) + ((Color2 shr 16) and $FF) + ((Color3 shr 16) and $FF) +
    ((Color4 shr 16) and $FF)) div 4) shl 16) or

    (((((Color1 shr 24) and $FF) + ((Color2 shr 24) and $FF) + ((Color3 shr 24) and $FF) +
    ((Color4 shr 24) and $FF)) div 4) shl 24);
end;

function AverageSixPixels(const Color1, Color2, Color3, Color4, Color5, Color6: TIntColor): TIntColor;
begin
  Result :=
    (((Color1 and $FF) + (Color2 and $FF) + (Color3 and $FF) + (Color4 and $FF) + (Color5 and $FF) +
    (Color6 and $FF)) div 6) or

    (((((Color1 shr 8) and $FF) + ((Color2 shr 8) and $FF) + ((Color3 shr 8) and $FF) + ((Color4 shr 8) and $FF) +
    ((Color5 shr 8) and $FF) + ((Color6 shr 8) and $FF)) div 6) shl 8) or

    (((((Color1 shr 16) and $FF) + ((Color2 shr 16) and $FF) + ((Color3 shr 16) and $FF) + ((Color4 shr 16) and $FF) +
    ((Color5 shr 16) and $FF) + ((Color6 shr 16) and $FF)) div 6) shl 16) or

    (((((Color1 shr 24) and $FF) + ((Color2 shr 24) and $FF) + ((Color3 shr 24) and $FF) + ((Color4 shr 24) and $FF) +
    ((Color5 shr 24) and $FF) + ((Color6 shr 24) and $FF)) div 6) shl 24);
end;

function BlendPixels(const Color1, Color2: TIntColor; const Alpha: Integer): TIntColor;
begin
  Result :=
    TIntColorValue(Integer(Color1 and $FF) + (((Integer(Color2 and $FF) -
    Integer(Color1 and $FF)) * Alpha) div 255)) or

    (TIntColorValue(Integer((Color1 shr 8) and $FF) + (((Integer((Color2 shr 8) and $FF) -
    Integer((Color1 shr 8) and $FF)) * Alpha) div 255)) shl 8) or

    (TIntColorValue(Integer((Color1 shr 16) and $FF) + (((Integer((Color2 shr 16) and $FF) -
    Integer((Color1 shr 16) and $FF)) * Alpha) div 255)) shl 16) or

    (TIntColorValue(Integer((Color1 shr 24) and $FF) + (((Integer((Color2 shr 24) and $FF) -
    Integer((Color1 shr 24) and $FF)) * Alpha) div 255)) shl 24);
end;

function BlendFourPixels(const TopLeft, TopRight, BottomRight, BottomLeft: TIntColor; const AlphaX,
  AlphaY: Integer): TIntColor;
begin
  Result := BlendPixels(BlendPixels(TopLeft, TopRight, AlphaX), BlendPixels(BottomLeft, BottomRight, AlphaX), AlphaY);
end;

function LerpPixels(const Color1, Color2: TIntColor; const Alpha: VectorFloat): TIntColor;
begin
  Result :=
    TIntColorValue(Integer(Color1 and $FF) + Round((Integer(Color2 and $FF) - Integer(Color1 and $FF)) * Alpha)) or

    (TIntColorValue(Integer((Color1 shr 8) and $FF) + Round((Integer((Color2 shr 8) and $FF) -
    Integer((Color1 shr 8) and $FF)) * Alpha)) shl 8) or

    (TIntColorValue(Integer((Color1 shr 16) and $FF) + Round((Integer((Color2 shr 16) and $FF) -
    Integer((Color1 shr 16) and $FF)) * Alpha)) shl 16) or

    (TIntColorValue(Integer((Color1 shr 24) and $FF) + Round((Integer((Color2 shr 24) and $FF) -
    Integer((Color1 shr 24) and $FF)) * Alpha)) shl 24);
end;

function LerpFourPixels(const TopLeft, TopRight, BottomRight, BottomLeft: TIntColor; const AlphaX,
  AlphaY: VectorFloat): TIntColor;
begin
  Result := LerpPixels(LerpPixels(TopLeft, TopRight, AlphaX), LerpPixels(BottomLeft, BottomRight, AlphaX), AlphaY);
end;

function CatmullRomPixels(const Color1, Color2, Color3, Color4: TIntColor; const Theta: VectorFloat): TIntColor;
begin
  Result := Saturate(Round(CatmullRom(Color1 and $FF, Color2 and $FF, Color3 and $FF, Color4 and $FF, Theta)), 0,
    255) or (Saturate(Round(CatmullRom((Color1 shr 8) and $FF, (Color2 shr 8) and $FF, (Color3 shr 8) and $FF,
    (Color4 shr 8) and $FF, Theta)), 0, 255) shl 8) or (Saturate(Round(CatmullRom((Color1 shr 16) and $FF,
    (Color2 shr 16) and $FF, (Color3 shr 16) and $FF, (Color4 shr 16) and $FF, Theta)), 0, 255) shl 16) or
    (Saturate(Round(CatmullRom((Color1 shr 24) and $FF, (Color2 shr 24) and $FF, (Color3 shr 24) and $FF,
    (Color4 shr 24) and $FF, Theta)), 0, 255) shl 24);
end;

function PixelToGray(const Color: TIntColor): Integer;
begin
  Result := ((Integer(Color and $FF) * 77) + (Integer((Color shr 8) and $FF) * 150) +
    (Integer((Color shr 16) and $FF) * 29)) div 256;
end;

function PixelToGrayFloat(const Color: TIntColor): VectorFloat;
begin
  Result := ((Color and $FF) * 0.299 + ((Color shr 8) and $FF) * 0.587 + ((Color shr 16) and $FF) * 0.114) / 255.0;
end;

procedure ExtractGrayAlpha(const SourceGray1, SourceGray2, Background1, Background2: VectorFloat; out Alpha,
  Gray: VectorFloat);
begin
  Alpha := (1.0 - (SourceGray2 - SourceGray1)) / (Background2 - Background1);

  if Alpha > 0.0 then
    Gray := (SourceGray1 - (1.0 - Alpha) * Background1) / Alpha
  else
    Gray := SourceGray1;
end;

function ExtractGrayAlpha(const SourceGray1, SourceGray2, Background1, Background2: VectorFloat): TFloatGrayAlpha;
begin
  ExtractGrayAlpha(SourceGray1, SourceGray2, Background1, Background2, Result.Alpha, Result.Gray);
end;

function IntGrayToColor(const Color: TIntGrayAlpha): TIntColor;
begin
  Result :=
    TIntColorValue(Color and $FF) or
    (TIntColorValue(Color and $FF) shl 8) or
    (TIntColorValue(Color and $FF) shl 16) or
    (TIntColorValue((Color shr 8) and $FF) shl 24);
end;

function IntColorToGray(const Color: TIntColor): TIntGrayAlpha;
begin
  Result :=
    (TIntColorValue(PixelToGray(Color)) and $FF) or
    ((TIntColorValue(Color) shr 24) and $FF) shl 8;
end;

{$ENDREGION}
{$REGION 'TIntColor2'}

class operator TIntColor2.Implicit(const Color: TIntColor): TIntColor2;
begin
  Result.First := Color;
  Result.Second := Color;
end;

function TIntColor2.HasGradient: Boolean;
begin
  Result := First <> Second;
end;

function TIntColor2.HasAlpha: Boolean;
begin
  Result := ((First shr 24) > 0) or ((Second shr 24) > 0);
end;

function IntColor2(const First, Second: TIntColor): TIntColor2;
begin
  Result.First := First;
  Result.Second := Second;
end;

function IntColor2(const Color: TIntColor): TIntColor2;
begin
  Result.First := Color;
  Result.Second := Color;
end;

{$ENDREGION}
{$REGION 'TIntColor4'}

class operator TIntColor4.Implicit(const Color: TIntColor): TIntColor4;
begin
  Result.TopLeft := Color;
  Result.TopRight := Color;
  Result.BottomRight := Color;
  Result.BottomLeft := Color;
end;

function TIntColor4.HasGradient: Boolean;
begin
  Result := (TopLeft <> TopRight) or (TopRight <> BottomRight) or (BottomRight <> BottomLeft);
end;

function TIntColor4.HasAlpha: Boolean;
begin
  Result := (TopLeft shr 24 > 0) or (TopRight shr 24 > 0) or (BottomRight shr 24 > 0) or (BottomLeft shr 24 > 0);
end;

function IntColor4(const TopLeft, TopRight, BottomRight, BottomLeft: TIntColor): TIntColor4;
begin
  Result.TopLeft := TopLeft;
  Result.TopRight := TopRight;
  Result.BottomRight := BottomRight;
  Result.BottomLeft := BottomLeft;
end;

function IntColor4(const Color: TIntColor): TIntColor4;
begin
  Result.TopLeft := Color;
  Result.TopRight := Color;
  Result.BottomRight := Color;
  Result.BottomLeft := Color;
end;

function IntColor4H(const Color: TIntColor2): TIntColor4;
begin
  Result.TopLeft := Color.First;
  Result.TopRight := Color.Second;
  Result.BottomRight := Color.Second;
  Result.BottomLeft := Color.First;
end;

function IntColor4H(const Left, Right: TIntColor): TIntColor4;
begin
  Result.TopLeft := Left;
  Result.TopRight := Right;
  Result.BottomRight := Right;
  Result.BottomLeft := Left;
end;

function IntColor4V(const Color: TIntColor2): TIntColor4;
begin
  Result.TopLeft := Color.First;
  Result.TopRight := Color.First;
  Result.BottomRight := Color.Second;
  Result.BottomLeft := Color.Second;
end;

function IntColor4V(const Top, Bottom: TIntColor): TIntColor4;
begin
  Result.TopLeft := Top;
  Result.TopRight := Top;
  Result.BottomRight := Bottom;
  Result.BottomLeft := Bottom;
end;

{$ENDREGION}
{$REGION 'TFloatColor'}

class operator TFloatColor.Add(const Color1, Color2: TFloatColor): TFloatColor;
begin
  Result.Red := Color1.Red + Color2.Red;
  Result.Green := Color1.Green + Color2.Green;
  Result.Blue := Color1.Blue + Color2.Blue;
  Result.Alpha := Color1.Alpha + Color2.Alpha;
end;

class operator TFloatColor.Subtract(const Color1, Color2: TFloatColor): TFloatColor;
begin
  Result.Red := Color1.Red - Color2.Red;
  Result.Green := Color1.Green - Color2.Green;
  Result.Blue := Color1.Blue - Color2.Blue;
  Result.Alpha := Color1.Alpha - Color2.Alpha;
end;

class operator TFloatColor.Multiply(const Color1, Color2: TFloatColor): TFloatColor;
begin
  Result.Red := Color1.Red * Color2.Red;
  Result.Green := Color1.Green * Color2.Green;
  Result.Blue := Color1.Blue * Color2.Blue;
  Result.Alpha := Color1.Alpha * Color2.Alpha;
end;

class operator TFloatColor.Divide(const Color1, Color2: TFloatColor): TFloatColor;
begin
  Result.Red := Color1.Red / Color2.Red;
  Result.Green := Color1.Green / Color2.Green;
  Result.Blue := Color1.Blue / Color2.Blue;
  Result.Alpha := Color1.Alpha / Color2.Alpha;
end;

class operator TFloatColor.Multiply(const Color: TFloatColor; const Theta: VectorFloat): TFloatColor;
begin
  Result.Red := Color.Red * Theta;
  Result.Green := Color.Green * Theta;
  Result.Blue := Color.Blue * Theta;
  Result.Alpha := Color.Alpha * Theta;
end;

class operator TFloatColor.Multiply(const Theta: VectorFloat; const Color: TFloatColor): TFloatColor;
begin
  Result.Red := Theta * Color.Red;
  Result.Green := Theta * Color.Green;
  Result.Blue := Theta * Color.Blue;
  Result.Alpha := Theta * Color.Alpha;
end;

class operator TFloatColor.Divide(const Color: TFloatColor; const Theta: VectorFloat): TFloatColor;
begin
  Result.Red := Color.Red / Theta;
  Result.Green := Color.Green / Theta;
  Result.Blue := Color.Blue / Theta;
  Result.Alpha := Color.Alpha / Theta;
end;

class operator TFloatColor.Equal(const Color1, Color2: TFloatColor): Boolean;
begin
  Result := (system.Abs(Color1.Red - Color2.Red) < VectorEpsilon) and (system.Abs(Color1.Green - Color2.Green) < VectorEpsilon)
    and (system.Abs(Color1.Blue - Color2.Blue) < VectorEpsilon) and (system.Abs(Color1.Alpha - Color2.Alpha) < VectorEpsilon);
end;

class operator TFloatColor.NotEqual(const Color1, Color2: TFloatColor): Boolean;
begin
  Result := not (Color1 = Color2);
end;

function FloatColor(const Color: TIntColor): TFloatColor; overload;
begin
  Result.Red := ((Color shr 16) and $FF) / 255.0;
  Result.Green := ((Color shr 8) and $FF) / 255.0;
  Result.Blue := (Color and $FF) / 255.0;
  Result.Alpha := ((Color shr 24) and $FF) / 255.0;
end;

function FloatColor(const Red, Green, Blue: VectorFloat; const Alpha: VectorFloat = 1.0): TFloatColor; overload;
begin
  Result.Red := Red;
  Result.Green := Green;
  Result.Blue := Blue;
  Result.Alpha := Alpha;
end;

function InvertColor(const Color: TFloatColor): TFloatColor;
begin
  Result.Red := 1.0 - Color.Red;
  Result.Green := 1.0 - Color.Green;
  Result.Blue := 1.0 - Color.Blue;
  Result.Alpha := 1.0 - Color.Alpha;
end;

function PremultiplyAlpha(const Color: TFloatColor): TFloatColor; overload;
begin
  Result.Red := Color.Red * Color.Alpha;
  Result.Green := Color.Green * Color.Alpha;
  Result.Blue := Color.Blue * Color.Alpha;
  Result.Alpha := Color.Alpha;
end;

function UnpremultiplyAlpha(const Color: TFloatColor): TFloatColor; overload;
begin
  if Color.Alpha > 0.0 then
  begin
    Result.Red := Color.Red / Color.Alpha;
    Result.Green := Color.Green / Color.Alpha;
    Result.Blue := Color.Blue / Color.Alpha;
    Result.Alpha := Color.Alpha;
  end
  else
    Result := Color;
end;

function AverageColors(const Color1, Color2: TFloatColor): TFloatColor;
begin
  Result.Red := (Color1.Red + Color2.Red) * 0.5;
  Result.Green := (Color1.Green + Color2.Green) * 0.5;
  Result.Blue := (Color1.Blue + Color2.Blue) * 0.5;
  Result.Alpha := (Color1.Alpha + Color2.Alpha) * 0.5;
end;

function LerpColors(const Color1, Color2: TFloatColor; const Alpha: VectorFloat): TFloatColor;
begin
  Result.Red := Color1.Red + (Color2.Red - Color1.Red) * Alpha;
  Result.Green := Color1.Green + (Color2.Green - Color1.Green) * Alpha;
  Result.Blue := Color1.Blue + (Color2.Blue - Color1.Blue) * Alpha;
  Result.Alpha := Color1.Alpha + (Color2.Alpha - Color1.Alpha) * Alpha;
end;

function ColorToGray(const Color: TFloatColor): VectorFloat;
begin
  Result := Color.Red * 0.299 + Color.Green * 0.587 + Color.Blue * 0.114;
end;

function SaturateColor(const Color: TFloatColor): TFloatColor;
begin
  Result.Red := Saturate(Color.Red, 0.0, 1.0);
  Result.Green := Saturate(Color.Green, 0.0, 1.0);
  Result.Blue := Saturate(Color.Blue, 0.0, 1.0);
  Result.Alpha := Saturate(Color.Alpha, 0.0, 1.0);
end;

function WarpColor(const Color: TFloatColor): TFloatColor;
begin
  Result := Color;

  if Result.Red > 1.0 then
    Result.Red:= 2.0 - Result.Red;

  if Result.Red < 0 then
    Result.Red:= -Result.Red;

  if Result.Green > 1.0 then
    Result.Green:= 2.0 - Result.Green;

  if Result.Green < 0 then
    Result.Green:= -Result.Green;

  if Result.Blue > 1.0 then
    Result.Blue:= 2.0 - Result.Blue;

  if Result.Blue < 0 then
    Result.Blue:= -Result.Blue;

  if Result.Alpha > 1.0 then
    Result.Alpha:= 2.0 - Result.Alpha;

  if Result.Alpha < 0 then
    Result.Alpha:= -Result.Alpha;
end;

function CatmullRomColors(const Color1, Color2, Color3, Color4: TFloatColor; const Theta: VectorFloat): TFloatColor;
begin
  Result.Red := CatmullRom(Color1.Red, Color2.Red, Color3.Red, Color4.Red, Theta);
  Result.Green := CatmullRom(Color1.Green, Color2.Green, Color3.Green, Color4.Green, Theta);
  Result.Blue := CatmullRom(Color1.Blue, Color2.Blue, Color3.Blue, Color4.Blue, Theta);
  Result.Alpha := CatmullRom(Color1.Alpha, Color2.Alpha, Color3.Alpha, Color4.Alpha, Theta);
end;

function FloatToIntColor(const Color: TFloatColor): TIntColor;
begin
  Result := Cardinal(Round(Color.Blue * 255.0)) or (Cardinal(Round(Color.Green * 255.0)) shl 8) or
    (Cardinal(Round(Color.Red * 255.0)) shl 16) or (Cardinal(Round(Color.Alpha * 255.0)) shl 24);
end;

function FloatGrayToColor(const Color: TFloatGrayAlpha): TFloatColor;
begin
  Result.Red := Color.Gray;
  Result.Green := Color.Gray;
  Result.Blue := Color.Gray;
  Result.Alpha := Color.Alpha;
end;

function FloatColorToGray(const Color: TFloatColor): TFloatGrayAlpha;
begin
  Result.Gray := ColorToGray(Color);
  Result.Alpha := Color.Alpha;
end;

{$ENDREGION}
{$REGION 'TPoint2px functions'}

class operator TPoint2px.Add(const Point1, Point2: TPoint2px): TPoint2px;
begin
  Result.X := Point1.X + Point2.X;
  Result.Y := Point1.Y + Point2.Y;
end;

class operator TPoint2px.Subtract(const Point1, Point2: TPoint2px): TPoint2px;
begin
  Result.X := Point1.X - Point2.X;
  Result.Y := Point1.Y - Point2.Y;
end;

class operator TPoint2px.Multiply(const Point1, Point2: TPoint2px): TPoint2px;
begin
  Result.X := Point1.X * Point2.X;
  Result.Y := Point1.Y * Point2.Y;
end;

class operator TPoint2px.Divide(const Point1, Point2: TPoint2px): TPoint2px;
begin
  Result.X := Point1.X div Point2.X;
  Result.Y := Point1.Y div Point2.Y;
end;

class operator TPoint2px.Negative(const Point: TPoint2px): TPoint2px;
begin
  Result.X := -Point.X;
  Result.Y := -Point.Y;
end;

class operator TPoint2px.Multiply(const Point: TPoint2px; const Theta: VectorInt): TPoint2px;
begin
  Result.X := Point.X * Theta;
  Result.Y := Point.Y * Theta;
end;

class operator TPoint2px.Multiply(const Theta: VectorInt; const Point: TPoint2px): TPoint2px;
begin
  Result.X := Theta * Point.X;
  Result.Y := Theta * Point.Y;
end;

class operator TPoint2px.Divide(const Point: TPoint2px; const Theta: VectorInt): TPoint2px;
begin
  Result.X := Point.X div Theta;
  Result.Y := Point.Y div Theta;
end;

class operator TPoint2px.Divide(const Point: TPoint2px; const Theta: VectorFloat): TPoint2px;
begin
  Result.X := Round(Point.X / Theta);
  Result.Y := Round(Point.Y / Theta);
end;

class operator TPoint2px.Equal(const Point1, Point2: TPoint2px): Boolean;
begin
  Result := (Point1.X = Point2.X) and (Point1.Y = Point2.Y);
end;

class operator TPoint2px.NotEqual(const Point1, Point2: TPoint2px): Boolean;
begin
  Result := (Point1.X <> Point2.X) or (Point1.Y <> Point2.Y);
end;

function Point2px(const X, Y: VectorInt): TPoint2px;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Length2px(const Point: TPoint2px): VectorFloat;
begin
  Result := Hypot(Point.X, Point.Y);
end;

function Angle2px(const Point: TPoint2px): VectorFloat;
begin
  Result := ArcTan2(Point.Y, Point.X);
end;

function Lerp2px(const Point1, Point2: TPoint2px; const Theta: VectorFloat): TPoint2px;
begin
  Result.X := Round(Point1.X + (Point2.X - Point1.X) * Theta);
  Result.Y := Round(Point1.Y + (Point2.Y - Point1.Y) * Theta);
end;

function CatmullRom2px(const PredPoint1, Point1, Point2, SuccPoint2: TPoint2px;
  const Theta: VectorFloat): TPoint2px; inline;
begin
  Result.X := Round(CatmullRom(PredPoint1.X, Point1.X, Point2.X, SuccPoint2.X, Theta));
  Result.Y := Round(CatmullRom(PredPoint1.Y, Point1.Y, Point2.Y, SuccPoint2.Y, Theta));
end;

function Dot2px(const Point1, Point2: TPoint2px): VectorInt;
begin
  Result := (Point1.X * Point2.X) + (Point1.Y * Point2.Y);
end;

function Cross2px(const Point1, Point2: TPoint2px): VectorInt;
begin
  Result := (Point1.X * Point2.Y) - (Point1.Y * Point2.X);
end;

{$ENDREGION}
{$REGION 'TPoint2 functions'}

class operator TPoint2.Add(const APoint1, APoint2: TPoint2): TPoint2;
begin
  Result.X := APoint1.X + APoint2.X;
  Result.Y := APoint1.Y + APoint2.Y;
end;

class operator TPoint2.Subtract(const APoint1, APoint2: TPoint2): TPoint2;
begin
  Result.X := APoint1.X - APoint2.X;
  Result.Y := APoint1.Y - APoint2.Y;
end;

class operator TPoint2.Multiply(const APoint1, APoint2: TPoint2): TPoint2;
begin
  Result.X := APoint1.X * APoint2.X;
  Result.Y := APoint1.Y * APoint2.Y;
end;

class operator TPoint2.Divide(const APoint1, APoint2: TPoint2): TPoint2;
begin
  Result.X := APoint1.X / APoint2.X;
  Result.Y := APoint1.Y / APoint2.Y;
end;

class operator TPoint2.Negative(const Point: TPoint2): TPoint2;
begin
  Result.X := -Point.X;
  Result.Y := -Point.Y;
end;

class operator TPoint2.Multiply(const Point: TPoint2; const Theta: VectorFloat): TPoint2;
begin
  Result.X := Point.X * Theta;
  Result.Y := Point.Y * Theta;
end;

class operator TPoint2.Multiply(const Theta: VectorFloat; const Point: TPoint2): TPoint2;
begin
  Result.X := Theta * Point.X;
  Result.Y := Theta * Point.Y;
end;

class operator TPoint2.Divide(const Point: TPoint2; const Theta: VectorFloat): TPoint2;
begin
  Result.X := Point.X / Theta;
  Result.Y := Point.Y / Theta;
end;

class operator TPoint2.Divide(const Point: TPoint2; const Theta: Integer): TPoint2;
begin
  Result.X := Point.X / Theta;
  Result.Y := Point.Y / Theta;
end;

class operator TPoint2.Implicit(const Point: TPoint2px): TPoint2;
begin
  Result.X := Point.X;
  Result.Y := Point.Y;
end;

class operator TPoint2.Equal(const APoint1, APoint2: TPoint2): Boolean;
begin
  Result := (system.Abs(APoint1.X - APoint2.X) < VectorEpsilon) and (system.Abs(APoint1.Y - APoint2.Y) < VectorEpsilon);
end;

procedure TPoint2.FromPointF(p: TpointF);
begin
  x := p.x;
  y := p.y;
end;

class operator TPoint2.NotEqual(const APoint1, APoint2: TPoint2): Boolean;
begin
  Result := not (APoint1 = APoint2);
end;

{$IFDEF POINT2_TO_PX_IMPLICIT}
class operator TPoint2.Implicit(const Point: TPoint2): TPoint2px;
begin
  Result.X := Round(Point.X);
  Result.Y := Round(Point.Y);
end;
{$ENDIF}

function Point2(const X, Y: VectorFloat): TPoint2; inline;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Point2ToPx(const Point: TPoint2): TPoint2px;
begin
  Result.X := Round(Point.X);
  Result.Y := Round(Point.Y);
end;

function Length2(const Point: TPoint2): VectorFloat;
begin
  Result := Hypot(Point.X, Point.Y);
end;

function Norm2(const Point: TPoint2): TPoint2;
var
  Amp: VectorFloat;
begin
  Amp := Length2(Point);

  if system.Abs(Amp) > VectorEpsilon then
  begin
    Result.X := Point.X / Amp;
    Result.Y := Point.Y / Amp;
  end
  else
    Result := Point;
end;

function Angle2(const Point: TPoint2): VectorFloat;
begin
  if system.Abs(Point.X) > VectorEpsilon then
    Result := ArcTan2(Point.Y, Point.X)
  else
    Result := 0.0;
end;

function Lerp2(const Point1, Point2: TPoint2; const Theta: VectorFloat): TPoint2;
begin
  Result.X := Point1.X + (Point2.X - Point1.X) * Theta;
  Result.Y := Point1.Y + (Point2.Y - Point1.Y) * Theta;
end;

function CatmullRom2(const PredPoint1, Point1, Point2, SuccPoint2: TPoint2;
  const Theta: VectorFloat): TPoint2; inline;
begin
  Result.X := CatmullRom(PredPoint1.X, Point1.X, Point2.X, SuccPoint2.X, Theta);
  Result.Y := CatmullRom(PredPoint1.Y, Point1.Y, Point2.Y, SuccPoint2.Y, Theta);
end;

function Dot2(const Point1, Point2: TPoint2): VectorFloat;
begin
  Result := (Point1.X * Point2.X) + (Point1.Y * Point2.Y);
end;

function Cross2(const Point1, Point2: TPoint2): VectorFloat;
begin
  Result := (Point1.X * Point2.Y) - (Point1.Y * Point2.X);
end;

{$ENDREGION}
{$REGION 'TVector3px functions'}

class operator TVector3px.Add(const Vector1, Vector2: TVector3px): TVector3px;
begin
  Result.X := Vector1.X + Vector2.X;
  Result.Y := Vector1.Y + Vector2.Y;
  Result.Z := Vector1.Z + Vector2.Z;
end;

class operator TVector3px.Subtract(const Vector1, Vector2: TVector3px): TVector3px;
begin
  Result.X := Vector1.X - Vector2.X;
  Result.Y := Vector1.Y - Vector2.Y;
  Result.Z := Vector1.Z - Vector2.Z;
end;

class operator TVector3px.Multiply(const Vector1, Vector2: TVector3px): TVector3px;
begin
  Result.X := Vector1.X * Vector2.X;
  Result.Y := Vector1.Y * Vector2.Y;
  Result.Z := Vector1.Z * Vector2.Z;
end;

class operator TVector3px.Divide(const Vector1, Vector2: TVector3px): TVector3px;
begin
  Result.X := Vector1.X div Vector2.X;
  Result.Y := Vector1.Y div Vector2.Y;
  Result.Z := Vector1.Z div Vector2.Z;
end;

class operator TVector3px.Negative(const Vector: TVector3px): TVector3px;
begin
  Result.X := -Vector.X;
  Result.Y := -Vector.Y;
  Result.Z := -Vector.Z;
end;

class operator TVector3px.Multiply(const Vector: TVector3px; const Theta: VectorInt): TVector3px;
begin
  Result.X := Vector.X * Theta;
  Result.Y := Vector.Y * Theta;
  Result.Z := Vector.Z * Theta;
end;

class operator TVector3px.Multiply(const Theta: VectorInt; const Vector: TVector3px): TVector3px;
begin
  Result.X := Theta * Vector.X;
  Result.Y := Theta * Vector.Y;
  Result.Z := Theta * Vector.Z;
end;

class operator TVector3px.Divide(const Vector: TVector3px; const Theta: VectorInt): TVector3px;
begin
  Result.X := Vector.X div Theta;
  Result.Y := Vector.Y div Theta;
  Result.Z := Vector.Z div Theta;
end;

class operator TVector3px.Equal(const Vector1, Vector2: TVector3px): Boolean;
begin
  Result := (Vector1.X = Vector2.X) and (Vector1.Y = Vector2.Y) and (Vector1.Z = Vector2.Z);
end;

class operator TVector3px.NotEqual(const Vector1, Vector2: TVector3px): Boolean;
begin
  Result := not (Vector1 = Vector2);
end;

function TVector3px.GetXY: TPoint2px;
begin
  Result.X := Self.X;
  Result.Y := Self.Y;
end;

function Vector3px(const X, Y, Z: VectorInt): TVector3px;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Vector3px(const Point: TPoint2px; const Z: VectorInt): TVector3px; overload;
begin
  Result.X := Point.X;
  Result.Y := Point.Y;
  Result.Z := Z;
end;

function Length3px(const Vector: TVector3px): VectorFloat;
begin
  Result := Sqrt((Vector.X * Vector.X) + (Vector.Y * Vector.Y) + (Vector.Z * Vector.Z));
end;

function Lerp3px(const Vector1, Vector2: TVector3px; const Theta: VectorFloat): TVector3px;
begin
  Result.X := Round(Vector1.X + (Vector2.X - Vector1.X) * Theta);
  Result.Y := Round(Vector1.Y + (Vector2.Y - Vector1.Y) * Theta);
  Result.Z := Round(Vector1.Z + (Vector2.Z - Vector1.Z) * Theta);
end;

function CatmullRom3px(const PredVector1, Vector1, Vector2, SuccVector2: TVector3px;
  const Theta: VectorFloat): TVector3px;
begin
  Result.X := Round(CatmullRom(PredVector1.X, Vector1.X, Vector2.X, SuccVector2.X, Theta));
  Result.Y := Round(CatmullRom(PredVector1.Y, Vector1.Y, Vector2.Y, SuccVector2.Y, Theta));
  Result.Z := Round(CatmullRom(PredVector1.Z, Vector1.Z, Vector2.Z, SuccVector2.Z, Theta));
end;

function Dot3px(const Vector1, Vector2: TVector3px): VectorInt;
begin
  Result := (Vector1.X * Vector2.X) + (Vector1.Y * Vector2.Y) + (Vector1.Z * Vector2.Z);
end;

function Cross3px(const Vector1, Vector2: TVector3px): TVector3px;
begin
  Result.X := (Vector1.Y * Vector2.Z) - (Vector1.Z * Vector2.Y);
  Result.Y := (Vector1.Z * Vector2.X) - (Vector1.X * Vector2.Z);
  Result.Z := (Vector1.X * Vector2.Y) - (Vector1.Y * Vector2.X);
end;

function Angle3px(const Vector1, Vector2: TVector3px): VectorFloat;
var
  Amp, CosValue: VectorFloat;
begin
  Amp := Length3px(Vector1) * Length3px(Vector2);

  if Amp > VectorEpsilon then
    CosValue := Dot3px(Vector1, Vector2) / Amp
  else
    CosValue := Dot3px(Vector1, Vector2) / VectorEpsilon;

  if CosValue < -1.0 then
    CosValue := -1.0
  else if CosValue > 1.0 then
    CosValue := 1.0;

  Result := ArcCos(CosValue);
end;

{$ENDREGION}
{$REGION 'TVector3 functions'}

class operator TVector3.Add(const Vector1, Vector2: TVector3): TVector3;
begin
  Result.X := Vector1.X + Vector2.X;
  Result.Y := Vector1.Y + Vector2.Y;
  Result.Z := Vector1.Z + Vector2.Z;
end;

class operator TVector3.Subtract(const Vector1, Vector2: TVector3): TVector3;
begin
  Result.X := Vector1.X - Vector2.X;
  Result.Y := Vector1.Y - Vector2.Y;
  Result.Z := Vector1.Z - Vector2.Z;
end;

function TVector3.ToString: string;
begin
  result := '('+floattostr(x)+','+floattostr(y)+','+floattostr(z)+')';
end;

class operator TVector3.Multiply(const Vector1, Vector2: TVector3): TVector3;
begin
  Result.X := Vector1.X * Vector2.X;
  Result.Y := Vector1.Y * Vector2.Y;
  Result.Z := Vector1.Z * Vector2.Z;
end;

class operator TVector3.Divide(const Vector1, Vector2: TVector3): TVector3;
begin
  Result.X := Vector1.X / Vector2.X;
  Result.Y := Vector1.Y / Vector2.Y;
  Result.Z := Vector1.Z / Vector2.Z;
end;

class operator TVector3.Negative(const Vector: TVector3): TVector3;
begin
  Result.X := -Vector.X;
  Result.Y := -Vector.Y;
  Result.Z := -Vector.Z;
end;

class operator TVector3.Multiply(const Vector: TVector3; const Theta: VectorFloat): TVector3;
begin
  Result.X := Vector.X * Theta;
  Result.Y := Vector.Y * Theta;
  Result.Z := Vector.Z * Theta;
end;

class operator TVector3.Multiply(const Theta: VectorFloat; const Vector: TVector3): TVector3;
begin
  Result.X := Theta * Vector.X;
  Result.Y := Theta * Vector.Y;
  Result.Z := Theta * Vector.Z;
end;

class operator TVector3.Divide(const Vector: TVector3; const Theta: VectorFloat): TVector3;
begin
  Result.X := Vector.X / Theta;
  Result.Y := Vector.Y / Theta;
  Result.Z := Vector.Z / Theta;
end;

class operator TVector3.Implicit(const Vector: TVector3px): TVector3;
begin
  Result.X := Vector.X;
  Result.Y := Vector.Y;
  Result.Z := Vector.Z;
end;

class operator TVector3.Equal(const Vector1, Vector2: TVector3): Boolean;
begin
  Result := (system.Abs(Vector1.X - Vector2.X) < VectorEpsilon) and (system.Abs(Vector1.Y - Vector2.Y) < VectorEpsilon) and
    (system.Abs(Vector1.Z - Vector2.Z) < VectorEpsilon);
end;

procedure TVector3.FromString(s: string);
var
  sx,sy,sz: string;
  s2: string;
begin
  s2 := s;
  SplitString(s, '(', s, s2);
  splitString(s2, ',', sx, s2);
  splitString(s2, ',', sy, s2);
  splitString(s2, ')', sz, s2);
  x := strtofloat(sx);
  y := strtofloat(sy);
  z := strtofloat(sz);


end;

class operator TVector3.NotEqual(const Vector1, Vector2: TVector3): Boolean;
begin
  Result := not (Vector1 = Vector2);
end;

function TVector3.GetXY: TPoint2;
begin
  Result.X := Self.X;
  Result.Y := Self.Y;
end;

function TVector3.GetXYpx: TPoint2px;
begin
  Result.X := Round(Self.X);
  Result.Y := Round(Self.Y);
end;

function Vector3(const X, Y, Z: VectorFloat): TVector3;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Vector3(const Point: TPoint2; const Z: VectorFloat): TVector3; overload;
begin
  Result.X := Point.X;
  Result.Y := Point.Y;
  Result.Z := Z;
end;

function Vector3ToPx(const Vector: TVector3): TVector3px;
begin
  Result.X := Round(Vector.X);
  Result.Y := Round(Vector.Y);
  Result.Z := Round(Vector.Z);
end;

function Length3(const Vector: TVector3): VectorFloat;
begin
  Result := Sqrt((Vector.X * Vector.X) + (Vector.Y * Vector.Y) + (Vector.Z * Vector.Z));
end;

function Norm3(const Vector: TVector3): TVector3;
var
  Amp: VectorFloat;
begin
  Amp := Length3(Vector);

  if system.Abs(Amp) > VectorEpsilon then
  begin
    Result.X := Vector.X / Amp;
    Result.Y := Vector.Y / Amp;
    Result.Z := Vector.Z / Amp;
  end
  else
    Result := Vector;
end;

function Lerp3(const Vector1, Vector2: TVector3; const Theta: VectorFloat): TVector3;
begin
  Result.X := Vector1.X + (Vector2.X - Vector1.X) * Theta;
  Result.Y := Vector1.Y + (Vector2.Y - Vector1.Y) * Theta;
  Result.Z := Vector1.Z + (Vector2.Z - Vector1.Z) * Theta;
end;

function CatmullRom3(const PredVector1, Vector1, Vector2, SuccVector2: TVector3; const Theta: VectorFloat): TVector3;
begin
  Result.X := CatmullRom(PredVector1.X, Vector1.X, Vector2.X, SuccVector2.X, Theta);
  Result.Y := CatmullRom(PredVector1.Y, Vector1.Y, Vector2.Y, SuccVector2.Y, Theta);
  Result.Z := CatmullRom(PredVector1.Z, Vector1.Z, Vector2.Z, SuccVector2.Z, Theta);
end;

function Dot3(const Vector1, Vector2: TVector3): VectorFloat;
begin
  Result := (Vector1.X * Vector2.X) + (Vector1.Y * Vector2.Y) + (Vector1.Z * Vector2.Z);
end;

function Cross3(const Vector1, Vector2: TVector3): TVector3;
begin
  Result.X := (Vector1.Y * Vector2.Z) - (Vector1.Z * Vector2.Y);
  Result.Y := (Vector1.Z * Vector2.X) - (Vector1.X * Vector2.Z);
  Result.Z := (Vector1.X * Vector2.Y) - (Vector1.Y * Vector2.X);
end;

function Angle3(const Vector1, Vector2: TVector3): VectorFloat;
var
  Amp, CosValue: VectorFloat;
begin
  Amp := Length3(Vector1) * Length3(Vector2);

  if Amp > VectorEpsilon then
    CosValue := Dot3(Vector1, Vector2) / Amp
  else
    CosValue := Dot3(Vector1, Vector2) / VectorEpsilon;

  if CosValue < -1.0 then
    CosValue := -1.0
  else if CosValue > 1.0 then
    CosValue := 1.0;

  Result := ArcCos(CosValue);
end;

function Parallel3(const Vector, Direction: TVector3): TVector3;
begin
  Result := Direction * (Dot3(Vector, Direction) / Sqr(Length3(Direction)));
end;

function Perp3(const Vector, Direction: TVector3): TVector3;
begin
  Result := Vector - Parallel3(Vector, Direction);
end;

function Reflect3(const Vector, Normal: TVector3): TVector3;
begin
  Result := Vector - 2.0 * Normal * Dot3(Vector, Normal);
end;

function ColorToVec3(const Color: TIntColor): TVector3;
begin
  Result.X := ((Color shl 8) shr 24) / 255.0;
  Result.Y := ((Color shl 16) shr 24) / 255.0;
  Result.Z := ((Color shl 24) shr 24) / 255.0;
end;

function Vec3ToColor(const Vector: TVector3): TIntColor;
begin
  Result := (Round(Vector.X * 255.0) shl 16) or (Round(Vector.Y * 255.0) shl 8) or Round(Vector.Z * 255.0) or
    $FF000000;
end;

{$ENDREGION}
{$REGION 'TVector4 functions'}

class operator TVector4.Add(const Vector1, Vector2: TVector4): TVector4;
{$IFDEF PURE_PASCAL}
begin
  Result.X := Vector1.X + Vector2.X;
  Result.Y := Vector1.Y + Vector2.Y;
  Result.Z := Vector1.Z + Vector2.Z;
  Result.W := Vector1.W + Vector2.W;
end;
{$ELSE}
asm
{$IFDEF NOFRAMES} .noframe{$ENDIF}
  //SoundTools.pas.3952: result.Left := a.Left+b.Left;
  //mov rax,[rbp+$40]
  movups xmm0, [Vector1]
  movups xmm1, [Vector2]
  addps xmm0,xmm1
  movups [result], xmm0
  ret
end;
{$ENDIF}

procedure TVector4.SetRGB(const Value: TVector3);
begin
  x := value.x;
  y := value.y;
  z := value.z;
end;

procedure TVector4.SetXY(const Value: TPoint2);
begin
  x := value.x;
  y := value.y;
end;

procedure TVector4.SetZ(const Value: VectorFloat);
begin
  Fz := Value;
end;

class operator TVector4.Subtract(const Vector1, Vector2: TVector4): TVector4;
{$IFDEF PURE_PASCAL}
begin
  Result.X := Vector1.X - Vector2.X;
  Result.Y := Vector1.Y - Vector2.Y;
  Result.Z := Vector1.Z - Vector2.Z;
  Result.W := Vector1.W - Vector2.W;
end;
{$ELSE}
asm
{$IFDEF NOFRAMES} .noframe{$ENDIF}
  //SoundTools.pas.3952: result.Left := a.Left+b.Left;
  //mov rax,[rbp+$40]
  movups xmm0, [Vector1]
  movups xmm1, [Vector2]
  subps xmm0,xmm1
  movups [result], xmm0
  ret
end;
{$ENDIF}

function TVector4.ToString: string;
begin
  result := '('+floattostr(x)+','+floattostr(y)+','+floattostr(z)+','+floattostr(w)+')';
end;

class operator TVector4.Multiply(const Vector1, Vector2: TVector4): TVector4;
{$IFDEF PURE_PASCAL}
begin
  Result.X := Vector1.X * Vector2.X;
  Result.Y := Vector1.Y * Vector2.Y;
  Result.Z := Vector1.Z * Vector2.Z;
  Result.W := Vector1.W * Vector2.W;
end;
{$ELSE}
asm
{$IFDEF NOFRAMES} .noframe{$ENDIF}
  //SoundTools.pas.3952: result.Left := a.Left+b.Left;
  //mov rax,[rbp+$40]
  movups xmm0, [Vector1]
  movups xmm1, [Vector2]
  mulps xmm0,xmm1
  movups [result], xmm0
  ret
end;
{$ENDIF}

class operator TVector4.Divide(const Vector1, Vector2: TVector4): TVector4;
{$IFDEF PURE_PASCAL}
begin
  Result.X := Vector1.X / Vector2.X;
  Result.Y := Vector1.Y / Vector2.Y;
  Result.Z := Vector1.Z / Vector2.Z;
  Result.W := Vector1.W / Vector2.W;
end;
{$ELSE}
asm
{$IFDEF NOFRAMES} .noframe{$ENDIF}
  //SoundTools.pas.3952: result.Left := a.Left+b.Left;
  //mov rax,[rbp+$40]
  movups xmm0, [Vector1]
  movups xmm1, [Vector2]
  divps xmm0,xmm1
  movups [result], xmm0
  ret
end;
{$ENDIF}

class operator TVector4.Negative(const Vector: TVector4): TVector4;
//{$IFDEF PURE_PASCAL}
begin
  Result.X := -Vector.X;
  Result.Y := -Vector.Y;
  Result.Z := -Vector.Z;
  Result.W := -Vector.W;
end;
//{$ELSE}
//asm
//{$IFDEF NOFRAMES} .noframe{$ENDIF}
//  movups xmm1, [Vector]
//  mov rax,0
//  movq xmm0, rax
//  subps xmm0,xmm1
//  movups [result], xmm0
//  ret
//end;
//{$ENDIF}

class operator TVector4.Multiply(const Vector: TVector4; const Theta: VectorFloat): TVector4;
//{$IFDEF PURE_PASCAL}
begin
  Result.X := Vector.X * Theta;
  Result.Y := Vector.Y * Theta;
  Result.Z := Vector.Z * Theta;
  Result.W := Vector.W * Theta;
end;
//{$ELSE}
//asm
//{$IFDEF NOFRAMES} .noframe{$ENDIF}
//  movq xmm0, [Vector]
//  movups xmm1, [Theta]
//  shufps xmm1,xmm1, 0
//  mulps xmm1,xmm0
//  movups [result], xmm1
//  ret
//end;
//{$ENDIF}

class operator TVector4.Multiply(const Theta: VectorFloat; const Vector: TVector4): TVector4;
//{$IFDEF PURE_PASCAL}
begin
  Result.X := Theta * Vector.X;
  Result.Y := Theta * Vector.Y;
  Result.Z := Theta * Vector.Z;
  Result.W := Theta * Vector.W;
end;
//{$ELSE}
//asm
//  shufps xmm0,xmm0, 0
//  movups xmm1, [Vector]
//  mulps xmm0,xmm1
//  movups [result], xmm0
//  ret
//end;
//{$ENDIF}

class operator TVector4.Add(const Vector1: TVEctor4;
  Vector2: TVector3): TVector4;
begin
  result.x := vector1.x+vector2.X;
  result.y := vector1.y+vector2.y;
  result.z := vector1.z+vector2.z;
  result.w := vector1.w;
end;

class operator TVector4.Divide(const Vector: TVector4; const Theta: VectorFloat): TVector4;
begin
  Result.X := Vector.X / Theta;
  Result.Y := Vector.Y / Theta;
  Result.Z := Vector.Z / Theta;
  Result.W := Vector.W / Theta;
end;

class operator TVector4.Equal(const Vector1, Vector2: TVector4): Boolean;
begin
  Result := (system.Abs(Vector1.X - Vector2.X) < VectorEpsilon) and (system.Abs(Vector1.Y - Vector2.Y) < VectorEpsilon) and
    (system.Abs(Vector1.Z - Vector2.Z) < VectorEpsilon) and (system.Abs(Vector1.W - Vector2.W) < VectorEpsilon);
end;

class operator TVector4.NotEqual(const Vector1, Vector2: TVector4): Boolean;
begin
  Result := not (Vector1 = Vector2);
end;

function TVector4.GetRGB: TVector3;
begin
  result.x := x;
  result.y := y;
  result.z := z;

end;

function TVector4.GetXy: TPoint2;
begin
  result.x := x;
  result.y := y;
end;

function TVector4.GetXYZ: TVector3;
begin
  Result.X := Self.X;
  Result.Y := Self.Y;
  Result.Z := Self.Z;
end;

class operator TVector4.Implicit(Vector3: TVector3): Tvector4;
begin
  result.x := vector3.X;
  result.y := vector3.Y;
  result.Z := vector3.Z;
  result.w := 1;
end;


procedure TVector4.Init;
begin
  x := 0;
  y := 0;
  z := 0;
  w := 1.0;
end;

function TVector4.Length: VEctorFloat;
var
  lx,ly,lz,lw: vectorfloat;
begin
  lx := x;
  ly := y;
  lz := z;
  lw := w;
  result := (lx*lx)+(ly*ly)+(lz*lz)+(lw*lw);
  if result > 0 then
    result := sqrt(result)
  else
    result := 0;
end;

function TVector4.ProjectToXYZ: TVector3;
begin
  if system.Abs(Self.W) > VectorEpsilon then
  begin
    Result.X := Self.X / Self.W;
    Result.Y := Self.Y / Self.W;
    Result.Z := Self.Z / Self.W;
  end
  else
    Result := Self.GetXYZ;
end;

function Vector4(const X, Y, Z: VectorFloat; const W: VectorFloat = 1.0): TVector4;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

function Vector4(const Vector: TVector3; const W: VectorFloat = 1.0): TVector4; overload;
begin
  Result.X := Vector.X;
  Result.Y := Vector.Y;
  Result.Z := Vector.Z;
  Result.W := W;
end;

function Length4(const Vector: TVector4): VectorFloat;
begin
  Result := Sqrt((Vector.X * Vector.X) + (Vector.Y * Vector.Y) + (Vector.Z * Vector.Z) + (Vector.W * Vector.W));
end;

function Norm4(const Vector: TVector4): TVector4;
var
  Amp: VectorFloat;
begin
  Amp := Length4(Vector);

  if system.Abs(Amp) > VectorEpsilon then
  begin
    Result.X := Vector.X / Amp;
    Result.Y := Vector.Y / Amp;
    Result.Z := Vector.Z / Amp;
    Result.W := Vector.W / Amp;
  end
  else
    Result := ZeroVec4;
end;

function Lerp4(const Vector1, Vector2: TVector4; const Theta: VectorFloat): TVector4;
begin
  Result.X := Vector1.X + (Vector2.X - Vector1.X) * Theta;
  Result.Y := Vector1.Y + (Vector2.Y - Vector1.Y) * Theta;
  Result.Z := Vector1.Z + (Vector2.Z - Vector1.Z) * Theta;
  Result.W := Vector1.W + (Vector2.W - Vector1.W) * Theta;
end;

function CatmullRom4(const PredVector1, Vector1, Vector2, SuccVector2: TVector4; const Theta: VectorFloat): TVector4;
begin
  Result.X := CatmullRom(PredVector1.X, Vector1.X, Vector2.X, SuccVector2.X, Theta);
  Result.Y := CatmullRom(PredVector1.Y, Vector1.Y, Vector2.Y, SuccVector2.Y, Theta);
  Result.Z := CatmullRom(PredVector1.Z, Vector1.Z, Vector2.Z, SuccVector2.Z, Theta);
  Result.W := CatmullRom(PredVector1.W, Vector1.W, Vector2.W, SuccVector2.W, Theta);
end;

function ColorToVec4(const Color: TIntColor): TVector4;
begin
  Result.X := ((Color shl 8) shr 24) / 255.0;
  Result.Y := ((Color shl 16) shr 24) / 255.0;
  Result.Z := ((Color shl 24) shr 24) / 255.0;
  Result.W := (Color shr 24) / 255.0;
end;

function Vec4ToColor(const Vector: TVector4): TIntColor;
begin
  Result := (Round(Vector.X * 255.0) shl 16) or (Round(Vector.Y * 255.0) shl 8) or Round(Vector.Z * 255.0) or
    (Round(Vector.W * 255.0) shl 24);
end;

{$ENDREGION}
{$REGION 'TMatrix3 functions'}

class operator TMatrix3.Add(const Matrix1, Matrix2: TMatrix3): TMatrix3;
var
  I, J: Integer;
begin
  for J := 0 to 2 do
    for I := 0 to 2 do
      Result.Data[J, I] := Matrix1.Data[J, I] + Matrix2.Data[J, I];
end;

class operator TMatrix3.Subtract(const Matrix1, Matrix2: TMatrix3): TMatrix3;
var
  I, J: Integer;
begin
  for J := 0 to 2 do
    for I := 0 to 2 do
      Result.Data[J, I] := Matrix1.Data[J, I] - Matrix2.Data[J, I];
end;

class operator TMatrix3.Multiply(const Matrix1, Matrix2: TMatrix3): TMatrix3;
var
  I, J: Integer;
begin
  for J := 0 to 2 do
    for I := 0 to 2 do
      Result.Data[J, I] := (Matrix1.Data[J, 0] * Matrix2.Data[0, I]) + (Matrix1.Data[J, 1] * Matrix2.Data[1, I]) +
        (Matrix1.Data[J, 2] * Matrix2.Data[2, I]);
end;

class operator TMatrix3.Multiply(const Matrix: TMatrix3; const Theta: VectorFloat): TMatrix3;
var
  I, J: Integer;
begin
  for J := 0 to 2 do
    for I := 0 to 2 do
      Result.Data[J, I] := Matrix.Data[J, I] * Theta;
end;

class operator TMatrix3.Multiply(const Theta: VectorFloat; const Matrix: TMatrix3): TMatrix3;
begin
  Result := Matrix * Theta;
end;

class operator TMatrix3.Divide(const Matrix: TMatrix3; const Theta: VectorFloat): TMatrix3;
var
  I, J: Integer;
begin
  for J := 0 to 2 do
    for I := 0 to 2 do
      Result.Data[J, I] := Matrix.Data[J, I] / Theta;
end;

class operator TMatrix3.Multiply(const Point: TPoint2; const Matrix: TMatrix3): TPoint2;
begin
  Result.X := (Point.X * Matrix.Data[0, 0]) + (Point.Y * Matrix.Data[1, 0]) + Matrix.Data[2, 0];
  Result.Y := (Point.X * Matrix.Data[0, 1]) + (Point.Y * Matrix.Data[1, 1]) + Matrix.Data[2, 1];
end;

function TMatrix3.Determinant: VectorFloat;
begin
  Result := Self.Data[0, 0] * (Self.Data[1, 1] * Self.Data[2, 2] - Self.Data[2, 1] * Self.Data[1, 2]) -
    Self.Data[0, 1] * (Self.Data[1, 0] * Self.Data[2, 2] - Self.Data[2, 0] * Self.Data[1, 2]) + Self.Data[0, 2] *
    (Self.Data[1, 0] * Self.Data[2, 1] - Self.Data[2, 0] * Self.Data[1, 1]);
end;

function TranslateMtx3(const Offset: TPoint2): TMatrix3;
begin
  Result := IdentityMtx3;
  Result.Data[2, 0] := Offset.X;
  Result.Data[2, 1] := Offset.Y;
end;

function TranslateMtx3(const X, Y: VectorFloat): TMatrix3;
begin
  Result := TranslateMtx3(Point2(X, Y));
end;

function ScaleMtx3(const Scale: TPoint2): TMatrix3;
begin
  Result := IdentityMtx3;
  Result.Data[0, 0] := Scale.X;
  Result.Data[1, 1] := Scale.Y;
end;

function ScaleMtx3(const X, Y: VectorFloat): TMatrix3;
begin
  Result := ScaleMtx3(Point2(X, Y));
end;

function ScaleMtx3(const Scale: VectorFloat): TMatrix3;
begin
  Result := ScaleMtx3(Point2(Scale, Scale));
end;

function RotateMtx3(const Angle: VectorFLoat): TMatrix3;
var
  SinValue, CosValue: VectorFloat;
begin
  SinCos(Angle, SinValue, CosValue);

  Result := IdentityMtx3;
  Result.Data[0, 0] := CosValue;
  Result.Data[0, 1] := SinValue;
  Result.Data[1, 0] := -Result.Data[0, 1];
  Result.Data[1, 1] := Result.Data[0, 0];
end;

function TransposeMtx3(const Matrix: TMatrix3): TMatrix3;
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
      Result.Data[I, J] := Matrix.Data[J, I];
end;

function AdjointMtx3(const Matrix: TMatrix3): TMatrix3;
begin
  Result.Data[0, 0] := Matrix.Data[1, 1] * Matrix.Data[2, 2] - Matrix.Data[2, 1] * Matrix.Data[1, 2];
  Result.Data[0, 1] := Matrix.Data[2, 1] * Matrix.Data[0, 2] - Matrix.Data[0, 1] * Matrix.Data[2, 2];
  Result.Data[0, 2] := Matrix.Data[0, 1] * Matrix.Data[1, 2] - Matrix.Data[1, 1] * Matrix.Data[0, 2];
  Result.Data[1, 0] := Matrix.Data[2, 0] * Matrix.Data[1, 2] - Matrix.Data[1, 0] * Matrix.Data[2, 2];
  Result.Data[1, 1] := Matrix.Data[0, 0] * Matrix.Data[2, 2] - Matrix.Data[2, 0] * Matrix.Data[0, 2];
  Result.Data[1, 2] := Matrix.Data[1, 0] * Matrix.Data[0, 2] - Matrix.Data[0, 0] * Matrix.Data[1, 2];
  Result.Data[2, 0] := Matrix.Data[1, 0] * Matrix.Data[2, 1] - Matrix.Data[2, 0] * Matrix.Data[1, 1];
  Result.Data[2, 1] := Matrix.Data[2, 0] * Matrix.Data[0, 1] - Matrix.Data[0, 0] * Matrix.Data[2, 1];
  Result.Data[2, 2] := Matrix.Data[0, 0] * Matrix.Data[1, 1] - Matrix.Data[1, 0] * Matrix.Data[0, 1];
end;

function InverseMtx3(const Matrix: TMatrix3): TMatrix3;
var
  Det: VectorFloat;
begin
  Det := Matrix.Determinant;

  if system.Abs(Det) > VectorEpsilon then
    Result := AdjointMtx3(Matrix) * (1.0 / Det)
  else
    Result := IdentityMtx3;
end;

{$ENDREGION}
{$REGION 'TMatrix4 functions'}

class operator TMatrix4.Add(const Matrix1, Matrix2: TMatrix4): TMatrix4;
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      Result.Data[J, I] := Matrix1.Data[J, I] + Matrix2.Data[J, I];
end;

class operator TMatrix4.Subtract(const Matrix1, Matrix2: TMatrix4): TMatrix4;
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      Result.Data[J, I] := Matrix1.Data[J, I] - Matrix2.Data[J, I];
end;

function TMatrix4.ToString: string;
var
  I, J: Integer;
begin
  result := '';

  for J := 0 to 3 do begin
    for I := 0 to 3 do begin
      case i of
        0: result := result + ('(' + floatprecision(data[j,i],4)+',');
        3: result := result + (')' + floatprecision(data[j,i],4));
      else
        result := result + (floatprecision(data[j,i],4)+',');
      end;
    end;
  end;

end;

class operator TMatrix4.Multiply(const Matrix1, Matrix2: TMatrix4): TMatrix4;
{$IFDEF PURE_MATRIX_MULTIPLY}
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      Result.Data[J, I] := (Matrix1.Data[J, 0] * Matrix2.Data[0, I]) + (Matrix1.Data[J, 1] * Matrix2.Data[1, I]) +
        (Matrix1.Data[J, 2] * Matrix2.Data[2, I]) + (Matrix1.Data[J, 3] * Matrix2.Data[3, I]);
end;
{$ELSE}

//NOTE NOTE NOTE:
//First thing to note is that the Delphi documentation regarding return values
//is completely DEAD WRONG

// General rule: Integer and pointer arguments are passed left to right in RCX, RDX, R8
// and R9. HOWEVER, when there is a large return value, this is the case, RCX contains
// a pointer to the return space when the callee is called and all Registers usage are
// pushed one to the right
label
  reppy;
asm
//  .noframe
//  push R10
//  push RCX
//  mov RDX, matrix1;
//  mov r8, matrix2;

  movups xmm0, [R8]      //    movaps   (%rdi), %xmm0
  movups xmm1, [R8+16]   //    movaps 16(%rdi), %xmm1
  movups xmm2, [R8+32]   //    movaps 32(%rdi), %xmm2
  movups xmm3, [R8+48]   //    movaps 48(%rdi), %xmm3
  mov r10, RCX
  mov RCX, 48            //    movq $48, %rcx
reppy:                    //1:
  movss xmm4, [rDX+rcx+0]    //    movss (%rsi, %rcx), %xmm4
  shufps xmm4,xmm4,0      //    shufps $0, %xmm4, %xmm4
  mulps  xmm4, xmm0       //    mulps %xmm0, %xmm4
  movaps xmm5,xmm4        //    movaps %xmm4, %xmm5
  movss xmm4, [rDX+rcx+4]  //    movss 4(%rsi, %rcx), %xmm4
  shufps xmm4, xmm4, 0    //    shufps $0, %xmm4, %xmm4
  mulps xmm4, xmm1        //    mulps %xmm1, %xmm4
  addps xmm5, xmm4        //    addps %xmm4, %xmm5
  movss xmm4,  [rDX+rcx+8] //    movss 8(%rsi, %rcx), %xmm4
  shufps xmm4, xmm4, 0    //    shufps $0, %xmm4, %xmm4
  mulps xmm4, xmm2        //    mulps %xmm2, %xmm4
  addps xmm5, xmm4        //    addps %xmm4, %xmm5
  movss xmm4, [rDX+rcx+12] //    movss 12(%rsi, %rcx), %xmm4
  shufps xmm4, xmm4, 0    //    shufps $0, %xmm4, %xmm4
  mulps xmm4, xmm3        //    mulps %xmm3, %xmm4
  addps xmm5, xmm4        //    addps %xmm4, %xmm5
//  mov rax, r10
//  add rax, rcx
  movups [r10+rcx], xmm5   //    movaps %xmm5, (%rdx, %rcx)
  sub rcx, 16            //    subq $16, %rcx
  jge reppy               //    jge 1b
//  mov result, r10
//  mov rax, rcx
//  pop RCX
//  pop R10


end;


{$ENDIF}

class operator TMatrix4.Multiply(const Matrix: TMatrix4; const Theta: VectorFloat): TMatrix4;
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      Result.Data[J, I] := Matrix.Data[J, I] * Theta;
end;

class operator TMatrix4.Multiply(const Theta: VectorFloat; const Matrix: TMatrix4): TMatrix4;
begin
  Result := Matrix * Theta;
end;

class operator TMatrix4.Divide(const Matrix: TMatrix4; const Theta: VectorFloat): TMatrix4;
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      Result.Data[J, I] := Matrix.Data[J, I] / Theta;
end;

class operator TMatrix4.Multiply(const Vector: TVector3; const Matrix: TMatrix4): TVector3;
begin
  Result.X := Vector.X * Matrix.Data[0, 0] + Vector.Y * Matrix.Data[1, 0] + Vector.Z * Matrix.Data[2, 0] +
    Matrix.Data[3, 0];
  Result.Y := Vector.X * Matrix.Data[0, 1] + Vector.Y * Matrix.Data[1, 1] + Vector.Z * Matrix.Data[2, 1] +
    Matrix.Data[3, 1];
  Result.Z := Vector.X * Matrix.Data[0, 2] + Vector.Y * Matrix.Data[1, 2] + Vector.Z * Matrix.Data[2, 2] +
    Matrix.Data[3, 2];
end;

class operator TMatrix4.Multiply(const Vector: TVector4; const Matrix: TMatrix4): TVector4;
{$IFDEF PURE_MATRIX_MULTIPLY}
begin
  Result.X := Vector.X * Matrix.Data[0, 0] + Vector.Y * Matrix.Data[1, 0] + Vector.Z * Matrix.Data[2, 0] + Vector.W *
    Matrix.Data[3, 0];
  Result.Y := Vector.X * Matrix.Data[0, 1] + Vector.Y * Matrix.Data[1, 1] + Vector.Z * Matrix.Data[2, 1] + Vector.W *
    Matrix.Data[3, 1];
  Result.Z:= Vector.X * Matrix.Data[0, 2] + Vector.Y * Matrix.Data[1, 2] + Vector.Z * Matrix.Data[2, 2] + Vector.W *
    Matrix.Data[3, 2];
  Result.W:= Vector.X * Matrix.Data[0, 3] + Vector.Y * Matrix.Data[1, 3] + Vector.Z * Matrix.Data[2, 3] + Vector.W *
    Matrix.Data[3, 3];
end;
{$ELSE}
//NOTE NOTE NOTE:
//First thing to note is that the Delphi documentation regarding return values
//is completely DEAD WRONG

// General rule: Integer and pointer arguments are passed left to right in RCX, RDX, R8
// and R9. HOWEVER, when there is a large return value, this is the case, RCX contains
// a pointer to the return space when the callee is called and all Registers usage are
// pushed one to the right

//CASE (SELECT.[1-0]) OF
//0: DEST[31-0]  DEST[31-0];
//1: DEST[31-0]  DEST[63-32];
//2: DEST[31-0]  DEST[95-64];
//3: DEST[31-0]  DEST[127-96];
//ESAC;
//CASE (SELECT.[3-2]) OF
//0: DEST[63-32]  DEST[31-0];
//1: DEST[63-32]  DEST[63-32];
//2: DEST[63-32]  DEST[95-64];
//3: DEST[63-32]  DEST[127-96];
//ESAC;
//CASE (SELECT.[5-4]) OF
//0: DEST[95-64]  SRC[31-0];
//1: DEST[95-64]  SRC[63-32];
//2: DEST[95-64]  SRC[95-64];
//3: DEST[95-64]  SRC[127-96];
//ESAC;
//CASE (SELECT.[7-6]) OF
//0: DEST[127-96]  SRC[31-0];
//1: DEST[127-96]  SRC[63-32];
//2: DEST[127-96]  SRC[95-64];
//3: DEST[127-96]  SRC[127-96];
//ESAC;
label
  reppy;
asm
  sub     esp, 16
  movdqu  dqword [esp], xmm10
  sub     esp, 16
  movdqu  dqword [esp], xmm11
  sub     esp, 16
  movdqu  dqword [esp], xmm12
  sub     esp, 16
  movdqu  dqword [esp], xmm13


  //xmm0 will contain the result vector
  //xmm0 will contain the vector param
  //RCX will contain the MAtrix pointer
  movups xmm0, [RDX]
  movups xmm1, [R8]      //    movaps   (%rdi), %xmm0
  movups xmm2, [R8+16]   //    movaps 16(%rdi), %xmm1
  movups xmm3, [R8+32]   //    movaps 32(%rdi), %xmm2
  movups xmm4, [R8+48]   //    movaps 48(%rdi), %xmm3
  //now we just need to push xmm0 though our matrix in [xmm1-xmm4]
reppy:                    //1:
  //we'll convert this code into 4 columns
  //           [vvv compute in xmm10   vvv ]   [vvv compute in xmm11   vvv ]  [vvv compute in xmm12   vvv ]  [vvv compute in xmm13   vvv ]
//  Result.X := Vector.X * Matrix.Data[0, 0] + Vector.Y * Matrix.Data[1, 0] + Vector.Z * Matrix.Data[2, 0] + Vector.W *  Matrix.Data[3, 0];
//  Result.Y := Vector.X * Matrix.Data[0, 1] + Vector.Y * Matrix.Data[1, 1] + Vector.Z * Matrix.Data[2, 1] + Vector.W *  Matrix.Data[3, 1];
//  Result.Z:= Vector.X * Matrix.Data[0, 2] + Vector.Y * Matrix.Data[1, 2] + Vector.Z * Matrix.Data[2, 2] + Vector.W *   Matrix.Data[3, 2];
//  Result.W:= Vector.X * Matrix.Data[0, 3] + Vector.Y * Matrix.Data[1, 3] + Vector.Z * Matrix.Data[2, 3] + Vector.W *   Matrix.Data[3, 3];

  //column 1
  movaps xmm10, xmm0
  shufps xmm10, xmm10, 0 //moves X to all 4 vector spaces
  mulps xmm10, xmm1
  //column 2
  movaps xmm11,xmm0
  shufps xmm11, xmm11, (1+4+16+64)
  mulps xmm11,xmm2
  //column 3
  movaps xmm12,xmm0
  shufps xmm12, xmm12, (2+8+32+128)
  mulps xmm12,xmm3
  //column 4
  movaps xmm13,xmm0
  shufps xmm13, xmm13, (255)
  mulps xmm13,xmm4

  //add them up in the result register
  movaps xmm0, xmm10
  addps xmm0,xmm11
  addps xmm0,xmm12
  addps xmm0,xmm13
  movups [RCX],xmm0

  movdqu  xmm10, dqword [esp]
  add     esp, 16
  movdqu  xmm11, dqword [esp]
  add     esp, 16
  movdqu  xmm12, dqword [esp]
  add     esp, 16
  movdqu  xmm13, dqword [esp]
  add     esp, 16


//  mov rax, rcx

end;
{$ENDIF}


class function TMatrix4.DetSub3(const A1, A2, A3, B1, B2, B3, C1, C2, C3: VectorFloat): VectorFloat;
begin
  Result := A1 * (B2 * C3 - B3 * C2) - B1 * (A2 * C3 - A3 * C2) + C1 * (A2 * B3 - A3 * B2);
end;

function TMatrix4.Determinant: VectorFloat;
begin
  Result := Self.Data[0, 0] * DetSub3(Self.Data[1, 1], Self.Data[2, 1], Self.Data[3, 1], Self.Data[1, 2],
    Self.Data[2, 2], Self.Data[3, 2], Self.Data[1, 3], Self.Data[2, 3], Self.Data[3, 3]) - Self.Data[0, 1] *
    DetSub3(Self.Data[1, 0], Self.Data[2, 0], Self.Data[3, 0], Self.Data[1, 2], Self.Data[2, 2], Self.Data[3, 2],
    Self.Data[1, 3], Self.Data[2, 3], Self.Data[3, 3]) + Self.Data[0, 2] * DetSub3(Self.Data[1, 0], Self.Data[2, 0],
    Self.Data[3, 0], Self.Data[1, 1], Self.Data[2, 1], Self.Data[3, 1], Self.Data[1, 3], Self.Data[2, 3],
    Self.Data[3, 3]) - Self.Data[0, 3] * DetSub3(Self.Data[1, 0], Self.Data[2, 0], Self.Data[3, 0], Self.Data[1, 1],
    Self.Data[2, 1], Self.Data[3, 1], Self.Data[1, 2], Self.Data[2, 2], Self.Data[3, 2]);
end;

function TMatrix4.EyePos: TVector3;
begin
  Result.X := -Self.Data[0, 0] * Self.Data[3, 0] - Self.Data[0, 1] * Self.Data[3, 1] - Self.Data[0, 2] *
    Self.Data[3, 2];
  Result.Y := -Self.Data[1, 0] * Self.Data[3, 0] - Self.Data[1, 1] * Self.Data[3, 1] - Self.Data[1, 2] *
    Self.Data[3, 2];
  Result.Z := -Self.Data[2, 0] * Self.Data[3, 0] - Self.Data[2, 1] * Self.Data[3, 1] - Self.Data[2, 2] *
    Self.Data[3, 2];
end;

function TMatrix4.WorldPos: TVector3;
begin
  Result.X := Self.Data[3, 0];
  Result.Y := Self.Data[3, 1];
  Result.Z := Self.Data[3, 2];
end;

function TranslateMtx4(const Offset: TVector3): TMatrix4;
begin
  Result := IdentityMtx4;
  Result.Data[3, 0] := Offset.X;
  Result.Data[3, 1] := Offset.Y;
  Result.Data[3, 2] := Offset.Z;
end;

function TranslateMtx4(const Offset: TVector4): TMatrix4;
begin
  Result := IdentityMtx4;
  Result.Data[3, 0] := Offset.X;
  Result.Data[3, 1] := Offset.Y;
  Result.Data[3, 2] := Offset.Z;
  Result.Data[3, 3] := Offset.W;
end;

function TranslateMtx4(const X, Y, Z: VectorFloat): TMatrix4;
begin
  Result := TranslateMtx4(Vector3(X, Y, Z));
end;

function ScaleMtx4(const Scale: TVector3): TMatrix4;
begin
  Result := IdentityMtx4;
  Result.Data[0, 0] := Scale.X;
  Result.Data[1, 1] := Scale.Y;
  Result.Data[2, 2] := Scale.Z;
end;

function ScaleMtx4(const X, Y, Z: VectorFloat): TMatrix4;
begin
  Result := ScaleMtx4(Vector3(X, Y, Z));
end;

function ScaleMtx4(const Scale: VectorFloat): TMatrix4;
begin
  Result := ScaleMtx4(Vector3(Scale, Scale, Scale));
end;

function RotateXMtx4(const Angle: VectorFloat): TMatrix4;
var
  SinValue, CosValue: VectorFloat;
begin
  SinCos(Angle, SinValue, CosValue);

  Result := IdentityMtx4;
  Result.Data[1, 1] := CosValue;
  Result.Data[1, 2] := SinValue;
  Result.Data[2, 1] := -Result.Data[1, 2];
  Result.Data[2, 2] := Result.Data[1, 1];
end;

function RotateYMtx4(const Angle: VectorFloat): TMatrix4;
var
  SinValue, CosValue: VectorFloat;
begin
  SinCos(Angle, SinValue, CosValue);

  Result := IdentityMtx4;
  Result.Data[0, 0] := CosValue;
  Result.Data[0, 2] := -SinValue;
  Result.Data[2, 0] := -Result.Data[0, 2];
  Result.Data[2, 2] := Result.Data[0, 0];
end;

function RotateZMtx4(const Angle: VectorFloat): TMatrix4;
var
  SinValue, CosValue: VectorFloat;
begin
  SinCos(Angle, SinValue, CosValue);

  Result := IdentityMtx4;
  Result.Data[0, 0] := CosValue;
  Result.Data[0, 1] := SinValue;
  Result.Data[1, 0] := -Result.Data[0, 1];
  Result.Data[1, 1] := Result.Data[0, 0];
end;

function RotateMtx4(const Axis: TVector3; const Angle: VectorFloat): TMatrix4;
var
  CosValue, InvCosValue, SinValue: VectorFloat;
  XYMul, XZMul, YZMul, XSin, YSin, ZSin: VectorFloat;
begin
  SinCos(Angle, SinValue, CosValue);
  InvCosValue := 1.0 - CosValue;

  XYMul := Axis.X * Axis.Y * InvCosValue;
  XZMul := Axis.X * Axis.Z * InvCosValue;
  YZMul := Axis.Y * Axis.Z * InvCosValue;

  XSin := Axis.X * SinValue;
  YSin := Axis.Y * SinValue;
  ZSin := Axis.Z * SinValue;

  Result := IdentityMtx4;
  Result.Data[0, 0] := Sqr(Axis.X) * InvCosValue + CosValue;
  Result.Data[0, 1] := XYMul + ZSin;
  Result.Data[0, 2] := XZMul - YSin;
  Result.Data[1, 0] := XYMul - ZSin;
  Result.Data[1, 1] := Sqr(Axis.Y) * InvCosValue + CosValue;
  Result.Data[1, 2] := YZMul + XSin;
  Result.Data[2, 0] := XZMul + YSin;
  Result.Data[2, 1] := YZMul - XSin;
  Result.Data[2, 2] := Sqr(Axis.Z) * InvCosValue + CosValue;
end;

function HeadingPitchBankMtx4(const Heading, Pitch, Bank: VectorFloat): TMatrix4;
var
  CosHeading, SinHeading, CosPitch, SinPitch, CosBank, SinBank: VectorFloat;
begin
  SinCos(Heading, SinHeading, CosHeading);
  SinCos(Pitch, SinPitch, CosPitch);
  SinCos(Bank, SinBank, CosBank);

  Result := IdentityMtx4;
  Result.Data[0, 0] := CosHeading * CosBank + SinHeading * SinPitch * SinBank;
  Result.Data[0, 1] := SinHeading * SinPitch * CosBank - CosHeading * SinBank;
  Result.Data[0, 2] := SinHeading * CosPitch;
  Result.Data[1, 0] := SinBank * CosPitch;
  Result.Data[1, 1] := CosBank * CosPitch;
  Result.Data[1, 2] := - SinPitch;
  Result.Data[2, 0] := CosHeading * SinPitch * SinBank - SinHeading * CosBank;
  Result.Data[2, 1] := SinBank * SinHeading + CosHeading * SinPitch * CosBank;
  Result.Data[2, 2] := CosHeading * CosPitch;
end;

function HeadingPitchBankMtx4(const Vector: TVector3): TMatrix4;
begin
  Result := HeadingPitchBankMtx4(Vector.Y, Vector.X, Vector.Z);
end;

function YawPitchRollMtx4(const Yaw, Pitch, Roll: VectorFloat): TMatrix4;
var
  SinYaw, CosYaw, SinPitch, CosPitch, SinRoll, CosRoll: VectorFloat;
begin
  SinCos(Yaw, SinYaw, CosYaw);
  SinCos(Pitch, SinPitch, CosPitch);
  SinCos(Roll, SinRoll, CosRoll);

  Result := IdentityMtx4;
  Result.Data[0, 0] := CosRoll * CosYaw + SinPitch * SinRoll * SinYaw;
  Result.Data[0, 1] := CosYaw * SinPitch * SinRoll - CosRoll * SinYaw;
  Result.Data[0, 2] := -CosPitch * SinRoll;
  Result.Data[1, 0] := CosPitch * SinYaw;
  Result.Data[1, 1] := CosPitch * CosYaw;
  Result.Data[1, 2] := SinPitch;
  Result.Data[2, 0] := CosYaw * SinRoll - CosRoll * SinPitch * SinYaw;
  Result.Data[2, 1] := -CosRoll * CosYaw * SinPitch - SinRoll * SinYaw;
  Result.Data[2, 2] := CosPitch * CosRoll;
end;

function YawPitchRollMtx4(const Vector: TVector3): TMatrix4;
begin
  Result := YawPitchRollMtx4(Vector.Y, Vector.X, Vector.Z);
end;

function ReflectMtx4(const Axis: TVector3): TMatrix4;
var
  XYMul, YZMul, XZMul: VectorFloat;
begin
  XYMul := -2.0 * Axis.X * Axis.Y;
  XZMul := -2.0 * Axis.X * Axis.Z;
  YZMul := -2.0 * Axis.Y * Axis.Z;

  Result := IdentityMtx4;
  Result.Data[0, 0] := 1.0 - (2.0 * Sqr(Axis.X));
  Result.Data[0, 1] := XYMul;
  Result.Data[0, 2] := XZMul;
  Result.Data[1, 0] := XYMul;
  Result.Data[1, 1] := 1.0 - (2.0 * Sqr(Axis.Y));
  Result.Data[1, 2] := YZMul;
  Result.Data[2, 0] := XZMul;
  Result.Data[2, 1] := YZMul;
  Result.Data[2, 2] := 1.0 - (2.0 * Sqr(Axis.Z));
end;

function LookAtMtx4(const Origin, Target, Ceiling: TVector3): TMatrix4;
var
  XAxis, YAxis, ZAxis: TVector3;
begin
  ZAxis := Norm3(Target - Origin);
  XAxis := Norm3(Cross3(Ceiling, ZAxis));
  YAxis := Cross3(ZAxis, XAxis);

  Result.Data[0, 0] := XAxis.X;
  Result.Data[0, 1] := YAxis.X;
  Result.Data[0, 2] := ZAxis.X;
  Result.Data[0, 3] := 0.0;

  Result.Data[1, 0] := XAxis.Y;
  Result.Data[1, 1] := YAxis.Y;
  Result.Data[1, 2] := ZAxis.Y;
  Result.Data[1, 3] := 0.0;

  Result.Data[2, 0] := XAxis.Z;
  Result.Data[2, 1] := YAxis.Z;
  Result.Data[2, 2] := ZAxis.Z;
  Result.Data[2, 3] := 0.0;

  Result.Data[3, 0] := -Dot3(XAxis, Origin);
  Result.Data[3, 1] := -Dot3(YAxis, Origin);
  Result.Data[3, 2] := -Dot3(ZAxis, Origin);
  Result.Data[3, 3] := 1.0;
end;

function PerspectiveFOVYMtx4(const FieldOfView, AspectRatio, MinRange, MaxRange: VectorFloat): TMatrix4;
var
  XScale, YScale, ZCoef: VectorFloat;
begin
  Result := ZeroMtx4;

  YScale := Cot(FieldOfView * 0.5);
  XScale := YScale / AspectRatio;
  ZCoef := MaxRange / (MaxRange - MinRange);

  Result.Data[0, 0] := XScale;
  Result.Data[1, 1] := YScale;
  Result.Data[2, 2] := ZCoef;
  Result.Data[2, 3] := 1.0;
  Result.Data[3, 2] := -MinRange * ZCoef;
end;

function PerspectiveFOVXMtx4(const FieldOfView, AspectRatio, MinRange, MaxRange: VectorFloat): TMatrix4;
var
  XScale, YScale, ZCoef: VectorFloat;
begin
  Result := ZeroMtx4;

  XScale := Cot(FieldOfView * 0.5);
  YScale := XScale / AspectRatio;
  ZCoef := MaxRange / (MaxRange - MinRange);

  Result.Data[0, 0] := XScale;
  Result.Data[1, 1] := YScale;
  Result.Data[2, 2] := ZCoef;
  Result.Data[2, 3] := 1.0;
  Result.Data[3, 2] := -MinRange * ZCoef;
end;



function PerspectiveOffCenterLHMtx4(const FieldOfView, AspectRatio, MinRange, MaxRange: VectorFloat): TMatrix4;
var
  XScale, YScale, ZCoef: VectorFloat;
begin
  Result := ZeroMtx4;

  XScale := Cot(FieldOfView * 0.5);
  YScale := XScale / AspectRatio;
  ZCoef := MaxRange / (MaxRange - MinRange);

  Result.Data[0, 0] := XScale;
  Result.Data[1, 1] := YScale;
  Result.Data[2, 2] := ZCoef;
  Result.Data[0, 3] := 0;
  Result.Data[1, 3] := 0;
  Result.Data[2, 3] := 1;
  Result.Data[3, 2] := -MinRange * ZCoef;
end;

function PerspectiveVOLMtx4(const Width, Height, MinRange, MaxRange: VectorFloat): TMatrix4;
begin
  Result := ZeroMtx4;

  Result.Data[0, 0] := (2.0 * MinRange) / Width;
  Result.Data[1, 1] := (2.0 * MinRange) / Height;
  Result.Data[2, 2] := MaxRange / (MaxRange - MinRange);
  Result.Data[2, 3] := 1.0;
  Result.Data[3, 2] := MinRange * (MinRange - MaxRange);
end;

function PerspectiveBDSMtx4(const Left, Right, Top, Bottom, MinRange, MaxRange: VectorFloat): TMatrix4;
begin
  Result := ZeroMtx4;

  Result.Data[0, 0] := (2.0 * MinRange) / (Right - Left);
  Result.Data[1, 1] := (2.0 * MinRange) / (Top - Bottom);

  Result.Data[2, 0] := (Left + Right) / (Left - Right);
  Result.Data[2, 1] := (Top + Bottom) / (Bottom - Top);
  Result.Data[2, 2] := MaxRange / (MaxRange - MinRange);
  Result.Data[2, 3] := 1.0;
  Result.Data[3, 2] := MinRange * MaxRange / (MinRange - MaxRange);
end;

function OrthogonalVOLMtx4(const Width, Height, MinRange, MaxRange: VectorFloat): TMatrix4;
begin
  Result := ZeroMtx4;

  Result.Data[0, 0] := 2.0 / Width;
  Result.Data[1, 1] := 2.0 / Height;
  Result.Data[2, 2] := 1.0 / (MaxRange - MinRange);
  Result.Data[2, 3] := MinRange / (MinRange - MaxRange);
  Result.Data[3, 3] := 1.0;
end;

function OrthogonalBDSMtx4(const Left, Right, Top, Bottom, MinRange, MaxRange: VectorFloat): TMatrix4;
begin
  Result := ZeroMtx4;

  Result.Data[0, 0] := 2.0 / (Right - Left);
  Result.Data[1, 1] := 2.0 / (Top - Bottom);
  Result.Data[2, 2] := 1.0 / (MaxRange - MinRange);
  Result.Data[2, 3] := MinRange / (MinRange - MaxRange);
  Result.Data[3, 0] := (Left + Right) / (Left - Right);
  Result.Data[3, 1] := (Top + Bottom) / (Bottom - Top);
  Result.Data[3, 2] := MinRange / (MinRange - MaxRange);
  Result.Data[3, 3] := 1.0;
end;

function TransposeMtx4(const Matrix: TMatrix4): TMatrix4;
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result.Data[I, J] := Matrix.Data[J, I];
end;

function AdjointMtx4(const Matrix: TMatrix4): TMatrix4;
begin
  Result.Data[0, 0] := TMatrix4.DetSub3(Matrix.Data[1, 1], Matrix.Data[2, 1], Matrix.Data[3, 1], Matrix.Data[1, 2],
    Matrix.Data[2, 2], Matrix.Data[3, 2], Matrix.Data[1, 3], Matrix.Data[2, 3], Matrix.Data[3, 3]);
  Result.Data[1, 0] := -TMatrix4.DetSub3(Matrix.Data[1, 0], Matrix.Data[2, 0], Matrix.Data[3, 0], Matrix.Data[1, 2],
    Matrix.Data[2, 2], Matrix.Data[3, 2], Matrix.Data[1, 3], Matrix.Data[2, 3], Matrix.Data[3, 3]);
  Result.Data[2, 0] := TMatrix4.DetSub3(Matrix.Data[1, 0], Matrix.Data[2, 0], Matrix.Data[3, 0], Matrix.Data[1, 1],
    Matrix.Data[2, 1], Matrix.Data[3, 1], Matrix.Data[1, 3], Matrix.Data[2, 3], Matrix.Data[3, 3]);
  Result.Data[3, 0] := -TMatrix4.DetSub3(Matrix.Data[1, 0], Matrix.Data[2, 0], Matrix.Data[3, 0], Matrix.Data[1, 1],
    Matrix.Data[2, 1], Matrix.Data[3, 1], Matrix.Data[1, 2], Matrix.Data[2, 2], Matrix.Data[3, 2]);

  Result.Data[0, 1] := -TMatrix4.DetSub3(Matrix.Data[0, 1], Matrix.Data[2, 1], Matrix.Data[3, 1], Matrix.Data[0, 2],
    Matrix.Data[2, 2], Matrix.Data[3, 2], Matrix.Data[0, 3], Matrix.Data[2, 3], Matrix.Data[3, 3]);
  Result.Data[1, 1] := TMatrix4.DetSub3(Matrix.Data[0, 0], Matrix.Data[2, 0], Matrix.Data[3, 0], Matrix.Data[0, 2],
    Matrix.Data[2, 2], Matrix.Data[3, 2], Matrix.Data[0, 3], Matrix.Data[2, 3], Matrix.Data[3, 3]);
  Result.Data[2, 1] := -TMatrix4.DetSub3(Matrix.Data[0, 0], Matrix.Data[2, 0], Matrix.Data[3, 0], Matrix.Data[0, 1],
    Matrix.Data[2, 1], Matrix.Data[3, 1], Matrix.Data[0, 3], Matrix.Data[2, 3], Matrix.Data[3, 3]);
  Result.Data[3, 1] := TMatrix4.DetSub3(Matrix.Data[0, 0], Matrix.Data[2, 0], Matrix.Data[3, 0], Matrix.Data[0, 1],
    Matrix.Data[2, 1], Matrix.Data[3, 1], Matrix.Data[0, 2], Matrix.Data[2, 2], Matrix.Data[3, 2]);

  Result.Data[0, 2] := TMatrix4.DetSub3(Matrix.Data[0, 1], Matrix.Data[1, 1], Matrix.Data[3, 1], Matrix.Data[0, 2],
    Matrix.Data[1, 2], Matrix.Data[3, 2], Matrix.Data[0, 3], Matrix.Data[1, 3], Matrix.Data[3, 3]);
  Result.Data[1, 2] := -TMatrix4.DetSub3(Matrix.Data[0, 0], Matrix.Data[1, 0], Matrix.Data[3, 0], Matrix.Data[0, 2],
    Matrix.Data[1, 2], Matrix.Data[3, 2], Matrix.Data[0, 3], Matrix.Data[1, 3], Matrix.Data[3, 3]);
  Result.Data[2, 2] := TMatrix4.DetSub3(Matrix.Data[0, 0], Matrix.Data[1, 0], Matrix.Data[3, 0], Matrix.Data[0, 1],
    Matrix.Data[1, 1], Matrix.Data[3, 1], Matrix.Data[0, 3], Matrix.Data[1, 3], Matrix.Data[3, 3]);
  Result.Data[3, 2] := -TMatrix4.DetSub3(Matrix.Data[0, 0], Matrix.Data[1, 0], Matrix.Data[3, 0], Matrix.Data[0, 1],
    Matrix.Data[1, 1], Matrix.Data[3, 1], Matrix.Data[0, 2], Matrix.Data[1, 2], Matrix.Data[3, 2]);

  Result.Data[0, 3] := -TMatrix4.DetSub3(Matrix.Data[0, 1], Matrix.Data[1, 1], Matrix.Data[2, 1], Matrix.Data[0, 2],
    Matrix.Data[1, 2], Matrix.Data[2, 2], Matrix.Data[0, 3], Matrix.Data[1, 3], Matrix.Data[2, 3]);
  Result.Data[1, 3] := TMatrix4.DetSub3(Matrix.Data[0, 0], Matrix.Data[1, 0], Matrix.Data[2, 0], Matrix.Data[0, 2],
    Matrix.Data[1, 2], Matrix.Data[2, 2], Matrix.Data[0, 3], Matrix.Data[1, 3], Matrix.Data[2, 3]);
  Result.Data[2, 3] := -TMatrix4.DetSub3(Matrix.Data[0, 0], Matrix.Data[1, 0], Matrix.Data[2, 0], Matrix.Data[0, 1],
    Matrix.Data[1, 1], Matrix.Data[2, 1], Matrix.Data[0, 3], Matrix.Data[1, 3], Matrix.Data[2, 3]);
  Result.Data[3, 3] := TMatrix4.DetSub3(Matrix.Data[0, 0], Matrix.Data[1, 0], Matrix.Data[2, 0], Matrix.Data[0, 1],
    Matrix.Data[1, 1], Matrix.Data[2, 1], Matrix.Data[0, 2], Matrix.Data[1, 2], Matrix.Data[2, 2]);
end;

function InvertMtx4(const Matrix: TMatrix4): TMatrix4;
var
  Det: VectorFloat;
begin
  Det := Matrix.Determinant;

  if system.Abs(Det) > VectorEpsilon then
    Result := AdjointMtx4(Matrix) * (1.0 / Det)
  else
    Result := IdentityMtx4;
end;

{$ENDREGION}
{$REGION 'TQuaternion functions'}

class operator TQuaternion.Multiply(const Quaternion1, Quaternion2: TQuaternion): TQuaternion;
begin
  Result.X := Quaternion2.W * Quaternion1.X + Quaternion2.X * Quaternion1.W + Quaternion2.Z * Quaternion1.Y -
    Quaternion2.Y * Quaternion1.Z;
  Result.Y := Quaternion2.W * Quaternion1.Y + Quaternion2.Y * Quaternion1.W + Quaternion2.X * Quaternion1.Z -
    Quaternion2.Z * Quaternion1.X;
  Result.Z := Quaternion2.W * Quaternion1.Z + Quaternion2.Z * Quaternion1.W + Quaternion2.Y * Quaternion1.X -
    Quaternion2.X * Quaternion1.Y;
  Result.W := Quaternion2.W * Quaternion1.W - Quaternion2.X * Quaternion1.X - Quaternion2.Y * Quaternion1.Y -
    Quaternion2.Z * Quaternion1.Z;
end;

class operator TQuaternion.Implicit(const Quaternion: TQuaternion): TMatrix4;
begin
  Result.Data[0, 0] := 1.0 - 2.0 * Quaternion.Y * Quaternion.Y - 2.0 * Quaternion.Z * Quaternion.Z;
  Result.Data[0, 1] := 2.0 * Quaternion.X * Quaternion.Y + 2.0 * Quaternion.W * Quaternion.Z;
  Result.Data[0, 2] := 2.0 * Quaternion.X * Quaternion.Z - 2.0 * Quaternion.W * Quaternion.Y;
  Result.Data[0, 3] := 0.0;
  Result.Data[1, 0] := 2.0 * Quaternion.X * Quaternion.Y - 2.0 * Quaternion.W * Quaternion.Z;
  Result.Data[1, 1] := 1.0 - 2.0 * Quaternion.X * Quaternion.X - 2 * Quaternion.Z * Quaternion.Z;
  Result.Data[1, 2] := 2.0 * Quaternion.Y * Quaternion.Z + 2.0 * Quaternion.W * Quaternion.X;
  Result.Data[1, 3] := 0.0;
  Result.Data[2, 0] := 2.0 * Quaternion.X * Quaternion.Z + 2.0 * Quaternion.W * Quaternion.Y;
  Result.Data[2, 1] := 2.0 * Quaternion.Y * Quaternion.Z - 2.0 * Quaternion.W * Quaternion.X;
  Result.Data[2, 2] := 1.0 - 2.0 * Quaternion.X * Quaternion.X - 2.0 * Quaternion.Y * Quaternion.Y;
  Result.Data[2, 3] := 0.0;
  Result.Data[3, 0] := 0.0;
  Result.Data[3, 1] := 0.0;
  Result.Data[3, 2] := 0.0;
  Result.Data[3, 3] := 1.0;
end;

class operator TQuaternion.Explicit(const Matrix: TMatrix4): TQuaternion;
var
  MaxValue, HighValue, MultValue: VectorFloat;
  TempQuat: TQuaternion;
  Index: Integer;
begin
  // Determine wich of W, X, Y, Z has the largest absolute value.
  TempQuat.X := Matrix.Data[0, 0] - Matrix.Data[1, 1] - Matrix.Data[2, 2];
  TempQuat.Y := Matrix.Data[1, 1] - Matrix.Data[0, 0] - Matrix.Data[2, 2];
  TempQuat.Z := Matrix.Data[2, 2] - Matrix.Data[0, 0] - Matrix.Data[1, 1];
  TempQuat.W := Matrix.Data[0, 0] + Matrix.Data[1, 1] + Matrix.Data[2, 2];

  Index := 0;
  MaxValue := TempQuat.W;
  if TempQuat.X > MaxValue then
  begin
    MaxValue := TempQuat.X;
    Index := 1;
  end;
  if TempQuat.Y > MaxValue then
  begin
    MaxValue := TempQuat.Y;
    Index := 2;
  end;
  if TempQuat.Z > MaxValue then
  begin
    MaxValue := TempQuat.Z;
    Index := 3;
  end;

  // Perform square root and division.
  HighValue := Sqrt(MaxValue + 1.0) * 0.5;
  MultValue := 0.25 / HighValue;

  // Apply table to compute quaternion values.
  case Index of
    0:
      begin
        Result.W := HighValue;
        Result.X := (Matrix.Data[1, 2] - Matrix.Data[2, 1]) * MultValue;
        Result.Y := (Matrix.Data[2, 0] - Matrix.Data[0, 2]) * MultValue;
        Result.Z := (Matrix.Data[0, 1] - Matrix.Data[1, 0]) * MultValue;
      end;
    1:
      begin
        Result.X := HighValue;
        Result.W := (Matrix.Data[1, 2] - Matrix.Data[2, 1]) * MultValue;
        Result.Z := (Matrix.Data[2, 0] + Matrix.Data[0, 2]) * MultValue;
        Result.Y := (Matrix.Data[0, 1] + Matrix.Data[1, 0]) * MultValue;
      end;
    2:
      begin
        Result.Y := HighValue;
        Result.Z := (Matrix.Data[1, 2] + Matrix.Data[2, 1]) * MultValue;
        Result.W := (Matrix.Data[2, 0] - Matrix.Data[0, 2]) * MultValue;
        Result.X := (Matrix.Data[0, 1] + Matrix.Data[1, 0]) * MultValue;
      end;
  else
    begin
      Result.Z := HighValue;
      Result.Y := (Matrix.Data[1, 2] + Matrix.Data[2, 1]) * MultValue;
      Result.X := (Matrix.Data[2, 0] + Matrix.Data[0, 2]) * MultValue;
      Result.W := (Matrix.Data[0, 1] - Matrix.Data[1, 0]) * MultValue;
    end;
  end;
end;

function RotateAboutXQuat(const Angle: VectorFloat): TQuaternion;
var
  SinValue, CosValue: VectorFloat;
begin
  SinCos(Angle * 0.5, SinValue, CosValue);

  Result.X := SinValue;
  Result.Y := 0.0;
  Result.Z := 0.0;
  Result.W := CosValue;
end;

function RotateAboutYQuat(const Angle: VectorFloat): TQuaternion;
var
  SinValue, CosValue: VectorFloat;
begin
  SinCos(Angle * 0.5, SinValue, CosValue);

  Result.X := 0.0;
  Result.Y := SinValue;
  Result.Z := 0.0;
  Result.W := CosValue;
end;

function RotateAboutZQuat(const Angle: VectorFloat): TQuaternion;
var
  SinValue, CosValue: VectorFloat;
begin
  SinCos(Angle * 0.5, SinValue, CosValue);

  Result.X := 0.0;
  Result.Y := 0.0;
  Result.Z := SinValue;
  Result.W := CosValue;
end;

function RotateAboutAxisQuat(const Axis: TVector3; const Angle: VectorFloat): TQuaternion;
var
  Amp, SinValue, CosValue, SinDivAmp: VectorFloat;
begin
  Amp := Length3(Axis);

  if Amp > VectorEpsilon then
  begin
    SinCos(Angle * 0.5, SinValue, CosValue);

    SinDivAmp := SinValue / Amp;

    Result.X := Axis.X * SinDivAmp;
    Result.Y := Axis.Y * SinDivAmp;
    Result.Z := Axis.Z * SinDivAmp;
    Result.W := CosValue;
  end
  else
    Result := IdentityQuat;
end;

function RotateObjectToIntertialQuat(const Heading, Pitch, Bank: VectorFloat): TQuaternion;
var
  SinPitch, SinBank, SinHeading, CosPitch, CosBank, CosHeading: VectorFloat;
begin
  SinCos(Heading * 0.5, SinHeading, CosHeading);
  SinCos(Pitch * 0.5, SinPitch, CosPitch);
  SinCos(Bank * 0.5, SinBank, CosBank);

  Result.X := CosHeading * SinPitch * CosBank + SinHeading * CosPitch * SinBank;
  Result.Y := -CosHeading * SinPitch * SinBank + SinHeading * CosPitch * CosBank;
  Result.Z := -SinHeading * SinPitch * CosBank + CosHeading * CosPitch * SinBank;
  Result.W := CosHeading * CosPitch * CosBank + SinHeading * SinPitch * SinBank;
end;

function RotateInertialToObjectQuat(const Heading, Pitch, Bank: VectorFloat): TQuaternion;
var
  SinPitch, SinBank, SinHeading, CosPitch, CosBank, CosHeading: VectorFloat;
begin
  SinCos(Heading * 0.5, SinHeading, CosHeading);
  SinCos(Pitch * 0.5, SinPitch, CosPitch);
  SinCos(Bank * 0.5, SinBank, CosBank);

  Result.X := -CosHeading * SinPitch * CosBank - SinHeading * CosPitch * SinBank;
  Result.Y := CosHeading * SinPitch * SinBank - SinHeading * CosPitch * CosBank;
  Result.Z := SinHeading * SinPitch * CosBank - CosHeading * CosPitch * SinBank;
  Result.W := CosHeading * CosPitch * CosBank + SinHeading * SinPitch * SinBank;
end;

function LengthQuat(const Quat: TQuaternion): VectorFloat;
begin
  Result := Sqrt(Quat.X * Quat.X + Quat.Y * Quat.Y + Quat.Z * Quat.Z + Quat.W * Quat.W);
end;

function NormalizeQuat(const Quat: TQuaternion): TQuaternion;
var
  Amp, InvMag: VectorFloat;
begin
  Amp := LengthQuat(Quat);

  if Amp > VectorEpsilon then
  begin
    InvMag := 1.0 / Amp;
    Result.X := Quat.X * InvMag;
    Result.Y := Quat.Y * InvMag;
    Result.Z := Quat.Z * InvMag;
    Result.W := Quat.W * InvMag;
  end
  else
    Result := IdentityQuat;
end;

function RotationAngleQuat(const Quat: TQuaternion): VectorFloat;
begin
  Result := ArcCos(Quat.W) * 2.0;
end;

function RotationAxisQuat(const Quat: TQuaternion): TVector3;
var
  Temp1, Temp2: VectorFloat;
begin
  Temp1 := 1.0 - Quat.W * Quat.W;
  if Temp1 <= 0.0 then
    Exit(AxisYVec3);

  Temp2 := 1.0 / Sqrt(Temp1);

  Result.X := Quat.X * Temp2;
  Result.Y := Quat.Y * Temp2;
  Result.Z := Quat.Z * Temp2;
end;

function DotQuat(const Quat1, Quat2: TQuaternion): VectorFloat;
begin
  Result := (Quat1.X * Quat2.X) + (Quat1.Y * Quat2.Y) + (Quat1.Z * Quat2.Z) + (Quat1.W * Quat2.W);
end;

function SlerpQuat(const Quat1, Quat2: TQuaternion; const Theta: VectorFloat): TQuaternion;
var
  TempQuat: TQuaternion;
  SinOmega, CosOmega, Omega, Kappa1, Kappa2: VectorFloat;
  OneOverSinOmega: VectorFloat;
begin
  if Theta <= 0.0 then
    Exit(Quat1)
  else if Theta >= 1.0 then
    Exit(Quat2);

  CosOmega := DotQuat(Quat1, Quat2);
  TempQuat := Quat1;

  if CosOmega < 0.0 then
  begin
    TempQuat.X := -TempQuat.X;
    TempQuat.Y := -TempQuat.Y;
    TempQuat.Z := -TempQuat.Z;
    TempQuat.W := -TempQuat.W;
    CosOmega := -CosOmega;
  end;

  if CosOmega > 1.0 - VectorEpsilon then
  begin
    Kappa1 := 1.0 - Theta;
    Kappa2 := Theta;
  end
  else
  begin
    SinOmega := Sqrt(1.0 - CosOmega * CosOmega);
    Omega := ArcTan2(SinOmega, CosOmega);

    OneOverSinOmega := 1.0 / SinOmega;

    Kappa1 := Sin((1.0 - Theta) * Omega) * OneOverSinOmega;
    Kappa2 := Sin(Theta * Omega) * OneOverSinOmega;
  end;

  Result.Z := Kappa1 * Quat1.Z + Kappa2 * TempQuat.X;
  Result.Y := Kappa1 * Quat1.Y + Kappa2 * TempQuat.Y;
  Result.Z := Kappa1 * Quat1.Z + Kappa2 * TempQuat.Z;
  Result.W := Kappa1 * Quat1.W + Kappa2 * TempQuat.W;
end;

function ConjugateQuat(const Quat: TQuaternion): TQuaternion;
begin
  Result.X := -Quat.X;
  Result.Y := -Quat.Y;
  Result.Z := -Quat.Z;
  Result.W := Quat.W;
end;

function ExpQuat(const Quat: TQuaternion; const Exponent: VectorFloat): TQuaternion;
var
  Alpha, NewAlpha, CosNewAlpha, SinNewAlpha, CompMult: VectorFloat;
begin
  if system.Abs(Quat.W) > 1.0 - VectorEpsilon then
    Exit(Quat);

  Alpha := ArcCos(Quat.W);
  NewAlpha := Alpha * Exponent;

  SinCos(NewAlpha, SinNewAlpha, CosNewAlpha);

  CompMult := SinNewAlpha / Sin(Alpha);

  Result.X := Quat.X * CompMult;
  Result.Y := Quat.Y * CompMult;
  Result.Z := Quat.Z * CompMult;
  Result.W := CosNewAlpha;
end;

{$ENDREGION}
{$REGION 'TIntRect'}

class operator TIntRect.Equal(const Rect1, Rect2: TIntRect): Boolean;
begin
  Result := (Rect1.TopLeft = Rect2.TopLeft) and (Rect1.BottomRight = Rect2.BottomRight);
end;

class operator TIntRect.NotEqual(const Rect1, Rect2: TIntRect): Boolean;
begin
  Result := not (Rect1 = Rect2);
end;

function TIntRect.GetWidth: VectorInt;
begin
  Result := Self.Right - Self.Left;
end;

procedure TIntRect.SetWidth(const Value: VectorInt);
begin
  Self.Right := Self.Left + Value;
end;

function TIntRect.GetHeight: VectorInt;
begin
  Result := Self.Bottom - Self.Top;
end;

procedure TIntRect.SetHeight(const Value: VectorInt);
begin
  Self.Bottom := Self.Top + Value;
end;

function TIntRect.GetSize: TPoint2px;
begin
  Result.X := Self.Right - Self.Left;
  Result.Y := Self.Bottom - Self.Top;
end;

procedure TIntRect.SetSize(const Value: TPoint2px);
begin
  Self.Right := Self.Left + Value.X;
  Self.Bottom := Self.Top + Value.Y;
end;

function TIntRect.GetEmpty: Boolean;
begin
  Result := (Self.Left >= Self.Right) or (Self.Top >= Self.Bottom);
end;

function IntRect(const Left, Top, Width, Height: VectorInt): TIntRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
end;

function IntRect(const Origin, Size: TPoint2px): TIntRect;
begin
  Result.TopLeft := Origin;
  Result.BottomRight := Origin + Size;
end;

function IntRectBDS(const Left, Top, Right, Bottom: VectorInt): TIntRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function IntRectBDS(const TopLeft, BottomRight: TPoint2px): TIntRect;
begin
  Result.TopLeft := TopLeft;
  Result.BottomRight := BottomRight;
end;

function PointInRect(const Point: TPoint2px; const Rect: TIntRect): Boolean;
begin
  Result := (Point.X >= Rect.Left) and (Point.X < Rect.Right) and (Point.Y >= Rect.Top) and (Point.Y < Rect.Bottom);
end;

function RectInRect(const Rect1, Rect2: TIntRect): Boolean;
begin
  Result := (Rect1.Left >= Rect2.Left) and (Rect1.Right <= Rect2.Right) and (Rect1.Top >= Rect2.Top) and
    (Rect1.Bottom <= Rect2.Bottom);
end;

function OverlapRect(const Rect1, Rect2: TIntRect): Boolean;
begin
  Result := (Rect2.Left < Rect1.Right) and (Rect2.Right > Rect1.Left) and (Rect2.Top < Rect1.Bottom) and
    (Rect2.Bottom > Rect1.Top);
end;

function IntersectRect(const Rect1, Rect2: TIntRect): TIntRect;
begin
  if OverlapRect(Rect1, Rect2) then
  begin
    Result.Left := Max(Rect1.Left, Rect2.Left);
    Result.Top := Max(Rect1.Top, Rect2.Top);
    Result.Right := Min(Rect1.Right, Rect2.Right);
    Result.Bottom := Min(Rect1.Bottom, Rect2.Bottom);
  end
  else
    Result := ZeroIntRect;
end;

function UnionRect(const Rect1, Rect2: TIntRect): TIntRect;
begin
  Result.Left := Min(Rect1.Left, Rect2.Left);
  Result.Top := Min(Rect1.Top, Rect2.Top);
  Result.Right := Max(Rect1.Right, Rect2.Right);
  Result.Bottom := Max(Rect1.Bottom, Rect2.Bottom);
end;

function OffsetRect(const Rect: TIntRect; const Delta: TPoint2px): TIntRect;
begin
  Result.TopLeft := Rect.TopLeft + Delta;
  Result.BottomRight := Rect.BottomRight + Delta;
end;

function OffsetRect(const Rect: TIntRect; const DeltaX, DeltaY: VectorInt): TIntRect;
begin
  Result := OffsetRect(Rect, Point2px(DeltaX, DeltaY));
end;

function InflateRect(const Rect: TIntRect; const Delta: TPoint2px): TIntRect;
begin
  Result.TopLeft := Rect.TopLeft - Delta;
  Result.BottomRight := Rect.BottomRight + Delta;
end;

function InflateRect(const Rect: TIntRect; const DeltaX, DeltaY: VectorInt): TIntRect; overload;
begin
  Result := InflateRect(Rect, Point2px(DeltaX, DeltaY));
end;

{$ENDREGION}
{$REGION 'TFloatRect'}

class operator TFloatRect.Equal(const Rect1, Rect2: TFloatRect): Boolean;
begin
  Result := (Rect1.TopLeft = Rect2.TopLeft) and (Rect1.BottomRight = Rect2.BottomRight);
end;

class operator TFloatRect.NotEqual(const Rect1, Rect2: TFloatRect): Boolean;
begin
  Result := not (Rect1 = Rect2);
end;

function TFloatRect.GetWidth: VectorFloat;
begin
  Result := Self.Right - Self.Left;
end;

procedure TFloatRect.SetWidth(const Value: VectorFloat);
begin
  Self.Right := Self.Left + Value;
end;

function TFloatRect.GetHeight: VectorFloat;
begin
  Result := Self.Bottom - Self.Top;
end;

procedure TFloatRect.SetHeight(const Value: VectorFloat);
begin
  Self.Bottom := Self.Top + Value;
end;

function TFloatRect.GetSize: TPoint2;
begin
  Result.X := Self.Right - Self.Left;
  Result.Y := Self.Bottom - Self.Top;
end;

procedure TFloatRect.SetSize(const Value: TPoint2);
begin
  Self.Right := Self.Left + Value.X;
  Self.Bottom := Self.Top + Value.Y;
end;

function TFloatRect.GetEmpty: Boolean;
begin
  Result := (Self.Left >= Self.Right) or (Self.Top >= Self.Bottom);
end;

function TFloatRect.ToIntRect: TIntRect;
begin
  Result.Left := Round(Self.Left);
  Result.Top := Round(Self.Top);
  Result.Right := Round(Self.Right);
  Result.Bottom := Round(Self.Bottom);
end;

function FloatRect(const Left, Top, Width, Height: VectorFloat): TFloatRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
end;

function FloatRect(const Origin, Size: TPoint2): TFloatRect;
begin
  Result.TopLeft := Origin;
  Result.BottomRight := Origin + Size;
end;

function FloatRectBDS(const Left, Top, Right, Bottom: VectorFloat): TFloatRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function FloatRectBDS(const TopLeft, BottomRight: TPoint2): TFloatRect;
begin
  Result.TopLeft := TopLeft;
  Result.BottomRight := BottomRight;
end;

function PointInRect(const Point: TPoint2; const Rect: TFloatRect): Boolean;
begin
  Result := (Point.X >= Rect.Left) and (Point.Y < Rect.Right) and (Point.Y >= Rect.Top) and (Point.Y < Rect.Bottom);
end;

function RectInRect(const Rect1, Rect2: TFloatRect): Boolean;
begin
  Result := (Rect1.Left >= Rect2.Left) and (Rect1.Right <= Rect2.Right) and (Rect1.Top >= Rect2.Top) and
    (Rect1.Bottom <= Rect2.Bottom);
end;

function OverlapRect(const Rect1, Rect2: TFloatRect): Boolean;
begin
  Result := (Rect2.Left < Rect1.Right) and (Rect2.Right > Rect1.Left) and (Rect2.Top < Rect1.Bottom) and
    (Rect2.Bottom > Rect1.Top);
end;

function IntersectRect(const Rect1, Rect2: TFloatRect): TFloatRect;
begin
  if OverlapRect(Rect1, Rect2) then
  begin
    Result.Left := Max(Rect1.Left, Rect2.Left);
    Result.Top := Max(Rect1.Top, Rect2.Top);
    Result.Right := Min(Rect1.Right, Rect2.Right);
    Result.Bottom := Min(Rect1.Bottom, Rect2.Bottom);
  end
  else
    Result := FloatRectBDS(0.0, 0.0, 0.0, 0.0);
end;

function UnionRect(const Rect1, Rect2: TFloatRect): TFloatRect;
begin
  Result.Left := Min(Rect1.Left, Rect2.Left);
  Result.Top := Min(Rect1.Top, Rect2.Top);
  Result.Right := Max(Rect1.Right, Rect2.Right);
  Result.Bottom := Max(Rect1.Bottom, Rect2.Bottom);
end;

function OffsetRect(const Rect: TFloatRect; const Delta: TPoint2): TFloatRect;
begin
  Result.TopLeft := Rect.TopLeft + Delta;
  Result.BottomRight := Rect.BottomRight + Delta;
end;

function OffsetRect(const Rect: TFloatRect; const DeltaX, DeltaY: VectorFloat): TFloatRect;
begin
  Result := OffsetRect(Rect, Point2(DeltaX, DeltaY));
end;

function InflateRect(const Rect: TFloatRect; const Delta: TPoint2): TFloatRect;
begin
  Result.TopLeft := Rect.TopLeft - Delta;
  Result.BottomRight := Rect.BottomRight + Delta;
end;

function InflateRect(const Rect: TFloatRect; const DeltaX, DeltaY: VectorFloat): TFloatRect; overload;
begin
  Result := InflateRect(Rect, Point2(DeltaX, DeltaY));
end;

{$ENDREGION}
{$REGION 'TFloatRect4'}

function FloatRect4(const TopLeftX, TopLeftY, TopRightX, TopRightY, BottomRightX, BottomRightY, BottomLeftX,
  BottomLeftY: VectorFloat): TFloatRect4;
begin
  Result.TopLeft.X := TopLeftX;
  Result.TopLeft.Y := TopLeftY;
  Result.TopRight.X := TopRightX;
  Result.TopRight.Y := TopRightY;
  Result.BottomRight.X := BottomRightX;
  Result.BottomRight.Y := BottomRightY;
  Result.BottomLeft.X := BottomLeftX;
  Result.BottomLeft.Y := BottomLeftY;
end;

function FloatRect4(const TopLeft, TopRight, BottomRight, BottomLeft: TPoint2): TFloatRect4;
begin
  Result.TopLeft := TopLeft;
  Result.TopRight := TopRight;
  Result.BottomRight := BottomRight;
  Result.BottomLeft := BottomLeft;
end;

function FloatRect4(const Left, Top, Width, Height: VectorFloat): TFloatRect4;
begin
  Result.TopLeft.X := Left;
  Result.TopLeft.Y := Top;
  Result.TopRight.X := Left + Width;
  Result.TopRight.Y := Top;
  Result.BottomRight.X := Result.TopRight.X;
  Result.BottomRight.Y := Top + Height;
  Result.BottomLeft.X := Left;
  Result.BottomLeft.Y := Result.BottomRight.Y;
end;

function FloatRect4(const Rect: TFloatRect): TFloatRect4;
begin
  Result.TopLeft := Rect.TopLeft;
  Result.TopRight := Point2(Rect.Right, Rect.Top);
  Result.BottomRight := Rect.BottomRight;
  Result.BottomLeft := Point2(Rect.Left, Rect.Bottom);
end;

function FloatRect4(const Rect: TIntRect): TFloatRect4;
begin
  Result.TopLeft := Rect.TopLeft;
  Result.TopRight := Point2(Rect.Right, Rect.Top);
  Result.BottomRight := Rect.BottomRight;
  Result.BottomLeft := Point2(Rect.Left, Rect.Bottom);
end;

function FloatRect4S(const Left, Top, Width, Height, Scale: VectorFloat; const Centered: Boolean): TFloatRect4;
var
  NewLeft, NewTop, NewWidth, NewHeight: VectorFloat;
begin
  if system.Abs(Scale - 1.0) <= VectorEpsilon then
    Exit(FloatRect4(Left, Top, Width, Height));

  if Centered then
  begin
    NewWidth := Width * Scale;
    NewHeight := Height * Scale;
    NewLeft := Left + (Width - NewWidth) * 0.5;
    NewTop := Top + (Height - NewHeight) * 0.5;

    Result := FloatRect4(NewLeft, NewTop, NewWidth, NewHeight);
  end
  else
    Result := FloatRect4(Left, Top, Width * Scale, Height * Scale);
end;

function FloatRect4R(const RotationOrigin, Size, RotationCenter: TPoint2; const Angle,
  Scale: VectorFloat): TFloatRect4;
var
  SinAngle, CosAngle: VectorFloat;
  IsScaled: Boolean;
  NewPoint: TPoint2;
  Index: Integer;
begin
  SinCos(Angle, SinAngle, CosAngle);

  Result := FloatRect4(-RotationCenter.X, -RotationCenter.Y, Size.X, Size.Y);

  IsScaled := system.Abs(Scale - 1.0) > VectorEpsilon;

  for Index := 0 to High(Result.Values) do
  begin
    if IsScaled then
      Result.Values[Index] := Result.Values[Index] * Scale;

    NewPoint.X := Result.Values[Index].X * CosAngle - Result.Values[Index].Y * SinAngle;
    NewPoint.Y := Result.Values[Index].Y * CosAngle + Result.Values[Index].X * SinAngle;

    Result.Values[Index] := NewPoint + RotationOrigin;
  end;
end;

function FloatRect4RC(const RotationOrigin, Size: TPoint2; const Angle, Scale: VectorFloat): TFloatRect4;
begin
  Result := FloatRect4R(RotationOrigin, Size, Size * 0.5, Angle, Scale);
end;

function FloatRect4RTL(const TopLeft, Size, RotationCenter: TPoint2; const Angle: VectorFloat;
  const Scale: VectorFloat = 1.0): TFloatRect4; inline;
begin
  Result := OffsetRect4(FloatRect4R(TopLeft, Size, RotationCenter, Angle, Scale), RotationCenter);
end;

class operator TFloatRect4.Equal(const Rect1, Rect2: TFloatRect4): Boolean;
begin
  Result := (Rect1.TopLeft = Rect2.TopLeft) and (Rect1.TopRight = Rect2.TopRight) and
    (Rect1.BottomRight = Rect2.BottomRight) and (Rect1.BottomLeft = Rect2.BottomLeft);
end;

function TFloatRect4.Height: vectorfloat;
begin
  result := BottomLeft.y - TopLeft.y;
end;

class operator TFloatRect4.NotEqual(const Rect1, Rect2: TFloatRect4): Boolean;
begin
  Result := not (Rect1 = Rect2);
end;

function TFloatRect4.Width: vectorfloat;
begin
  result := TopRight.x - TopLeft.x;
end;

function ScaleRect4(const Rect: TFloatRect4; const Scale: VectorFloat; const Centered: Boolean): TFloatRect4;
var
  Center: TPoint2;
begin
  if system.Abs(Scale - 1.0) <= VectorEpsilon then
    Exit(Rect);

  if Centered then
  begin
    Center := (Rect.TopLeft + Rect.TopRight + Rect.BottomRight + Rect.BottomLeft) * 0.25;

    Result.TopLeft := Lerp2(Center, Rect.TopLeft, Scale);
    Result.TopRight := Lerp2(Center, Rect.TopRight, Scale);
    Result.BottomRight := Lerp2(Center, Rect.BottomRight, Scale);
    Result.BottomLeft := Lerp2(Center, Rect.BottomLeft, Scale);
  end
  else
  begin
    Result.TopLeft := Rect.TopLeft * Scale;
    Result.TopRight := Rect.TopRight * Scale;
    Result.BottomRight := Rect.BottomRight * Scale;
    Result.BottomLeft := Rect.BottomLeft * Scale;
  end;
end;

function MirrorRect4(const Rect: TFloatRect4): TFloatRect4;
begin
  Result.TopLeft := Rect.TopRight;
  Result.TopRight := Rect.TopLeft;
  Result.BottomRight := Rect.BottomLeft;
  Result.BottomLeft := Rect.BottomRight;
end;

function FlipRect4(const Rect: TFloatRect4): TFloatRect4;
begin
  Result.TopLeft := Rect.BottomLeft;
  Result.TopRight := Rect.BottomRight;
  Result.BottomRight := Rect.TopRight;
  Result.BottomLeft := Rect.TopLeft;
end;

function TransformRect4(const Rect: TFloatRect4; const Matrix: TMatrix3): TFloatRect4;
begin
  Result.TopLeft := Rect.TopLeft * Matrix;
  Result.TopRight := Rect.TopRight * Matrix;
  Result.BottomRight := Rect.BottomRight * Matrix;
  Result.BottomLeft := Rect.BottomLeft * Matrix;
end;

function OffsetRect4(const Rect: TFloatRect4; const Delta: TPoint2): TFloatRect4;
begin
  Result.TopLeft := Rect.TopLeft + Delta;
  Result.TopRight := Rect.TopRight + Delta;
  Result.BottomRight := Rect.BottomRight + Delta;
  Result.BottomLeft := Rect.BottomLeft + Delta;
end;

function OffsetRect4(const Rect: TFloatRect4; const DeltaX, DeltaY: VectorFloat): TFloatRect4;
begin
  Result := OffsetRect4(Rect, Point2(DeltaX, DeltaY));
end;

function PointInRect4(const Point: TPoint2; const Rect: TFloatRect4): Boolean;
begin
  Result := PointInTriangle(Point, Rect.Values[0], Rect.Values[1], Rect.Values[2]) or PointInTriangle(Point,
    Rect.Values[2], Rect.Values[3], Rect.Values[0]);
end;

{$ENDREGION}

{ TFloatRect4Helper }

procedure TFloatRect4Helper.FromRectF(r: TRectF);
begin
  self := FloatRect4(r.Topleft.x, r.TopLEft.y, r.Width, r.height);
//  self.TopLeft := POint2(r.TopLeft.x, r.TopLeft.y);
//  self.BottomRight := POint2(r.BottomRight.x, r.BottomRight.Y);
//  self.TopRight := POint2(r.BottomRight.x, r.TopLEft.Y);
//  self.BottomLeft := POint2(r.TopLeft.x, r.BottomRight.Y);
end;

function TFloatRect4Helper.ToRectF: TRectF;
begin
  result.Left := TopLeft.x;
  result.Right := BottomRight.x;
  result.Top := TopLeft.y;
  result.Bottom := bottomright.Y;

end;

{ TPoint2Helper }

procedure TPoint2Helper.FromPOintF(p: TPointF);
begin

  x := p.x;
  y := p.y;

end;

function TPoint2Helper.ToPOintF: TPOintF;
begin
  result.x := x;
  result.y := y;
end;

function Order(var ul,br: TPoint2): boolean;overload;
var
  a,b: TPoint2;
begin
  result := false;

  a := ul;
  b := br;
  a.X := lesserof(ul.x, br.x);
  a.y := lesserof(ul.y, br.y);
  b.x := greaterof(ul.X, br.X);
  b.y := greaterof(ul.y, br.y);
  ul := a;  br := b;
end;

end.
