unit PXL.Boards.PI.BCM;
{
  Copyright (c) 2012, Broadcom Europe Ltd. All rights reserved.

  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
  following conditions are met:
    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
      disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the
      following disclaimer in the documentation and/or other materials provided with the distribution.
    * Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote
      products derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}
interface

{$IFDEF FPC}
  {$PACKRECORDS C}
{$ELSE}
  {$ALIGN ON}
{$ENDIF}

{ vcos_stdint.h }

type
  Pint8_t = ^int8_t;
  int8_t = ShortInt;

  Puint8_t = ^uint8_t;
  uint8_t = Byte;

  Pint16_t = ^int16_t;
  int16_t = SmallInt;

  Puint16_t = ^uint16_t;
  uint16_t = Word;

  Pint32_t = ^int32_t;
  int32_t = LongInt;

  Puint32_t = ^uint32_t;
  uint32_t = LongWord;

  Pintptr_t = ^intptr_t;
  intptr_t = LongInt;

  Puintptr_t = ^uintptr_t;
  uintptr_t = LongWord;

  Pintmax_t = ^intmax_t;
  intmax_t = LongInt;

  Puintmax_t = ^uintmax_t;
  uintmax_t = LongWord;

const
  INT8_MIN = Low(ShortInt);
  INT8_MAX = High(ShortInt);
  UINT8_MAX = High(Byte);

  INT16_MIN = Low(SmallInt);
  INT16_MAX = High(SmallInt);
  UINT16_MAX = High(Word);

  INT32_MIN = Low(LongInt);
  INT32_MAX = High(LongInt);
  UINT32_MAX = High(LongWord);

  INTPTR_MIN = Low(LongInt);
  INTPTR_MAX = High(LongInt);
  UINTPTR_MAX = High(LongWord);

  INTMAX_MIN = Low(LongInt);
  INTMAX_MAX = High(LongInt);
  UINTMAX_MAX = High(LongWord);

type
{ N.B. 64-bit integer types are not currently supported by lcc. However, these symbols are referenced in header files
  included by files compiled by lcc for VCE, so removing them would break the build. The solution here then is to
  define them, as the correct size, but in a way that should make them unusable in normal arithmetic operations. }
  Pint64_t = ^int64_t;
  int64_t = record
    a: uint32_t;
    b: uint32_t;
  end;

  Puint64_t = ^uint64_t;
  uint64_t = record
    a: uint32_t;
    b: uint32_t;
  end;

{ vc_display_types.h }

{ Common image types used by the vc_image library. }

const
  VCOS_DISPLAY_INPUT_FORMAT_INVALID = 0;
  VCOS_DISPLAY_INPUT_FORMAT_RGB888 = 1;
  VCOS_DISPLAY_INPUT_FORMAT_RGB565 = 2;

type
  PVCOS_DISPLAY_INPUT_FORMAT_T = ^VCOS_DISPLAY_INPUT_FORMAT_T;
  VCOS_DISPLAY_INPUT_FORMAT_T = Cardinal;

const
{ For backward compatibility }
  DISPLAY_INPUT_FORMAT_INVALID = VCOS_DISPLAY_INPUT_FORMAT_INVALID;
  DISPLAY_INPUT_FORMAT_RGB888 = VCOS_DISPLAY_INPUT_FORMAT_RGB888;
  DISPLAY_INPUT_FORMAT_RGB565 = VCOS_DISPLAY_INPUT_FORMAT_RGB565;

type
  PDISPLAY_INPUT_FORMAT_T = ^DISPLAY_INPUT_FORMAT_T;
  DISPLAY_INPUT_FORMAT_T = VCOS_DISPLAY_INPUT_FORMAT_T;


const
{ Enum determining how image data for 3D displays has to be supplied }
  DISPLAY_3D_UNSUPPORTED = 0;    // default
  DISPLAY_3D_INTERLEAVED = 1;    // For autosteroscopic displays
  DISPLAY_3D_SBS_FULL_AUTO = 2;  // Side-By-Side, Full Width (also used by some autostereoscopic displays)
  DISPLAY_3D_SBS_HALF_HORIZ = 3; // Side-By-Side, Half Width, Horizontal Subsampling (see HDMI spec)
  DISPLAY_3D_TB_HALF = 4;        // Top-bottom 3D

type
  PDISPLAY_3D_FORMAT_T = ^DISPLAY_3D_FORMAT_T;
  DISPLAY_3D_FORMAT_T = Cardinal;

const
{ enums of display types }
  DISPLAY_INTERFACE_MIN = 0;
  DISPLAY_INTERFACE_SMI = 1;
  DISPLAY_INTERFACE_DPI = 2;
  DISPLAY_INTERFACE_DSI = 3;
  DISPLAY_INTERFACE_LVDS = 4;
  DISPLAY_INTERFACE_MAX = 5;

type
  PDISPLAY_INTERFACE_T = ^DISPLAY_INTERFACE_T;
  DISPLAY_INTERFACE_T = Cardinal;

const
{ display dither setting, used on B0 }
  DISPLAY_DITHER_NONE = 0;    // default if not set
  DISPLAY_DITHER_RGB666 = 1;
  DISPLAY_DITHER_RGB565 = 2;
  DISPLAY_DITHER_RGB555 = 3;
  DISPLAY_DITHER_MAX = 4;

type
  PDISPLAY_DITHER_T = ^DISPLAY_DITHER_T;
  DISPLAY_DITHER_T = Cardinal;

{ info struct }
  PDISPLAY_INFO_T = ^DISPLAY_INFO_T;
  DISPLAY_INFO_T = record
    { type }
    _type: DISPLAY_INTERFACE_T;
    { width / height }
    width: uint32_t;
    height: uint32_t;
    { output format }
    input_format: DISPLAY_INPUT_FORMAT_T;
    { interlaced? }
    interlaced: uint32_t;
    { output dither setting (if required) }
    output_dither: DISPLAY_DITHER_T;
    { Pixel frequency }
    pixel_freq: uint32_t;
    { Line rate in lines per second }
    line_rate: uint32_t;
    { Format required for image data for 3D displays }
    format_3d: DISPLAY_3D_FORMAT_T;
    { If display requires PV1 (e.g. DSI1), special config is required in HVS }
    use_pixelvalve_1: uint32_t;
    { Set for DSI displays which use video mode. }
    dsi_video_mode: uint32_t;
    { Select HVS channel (usually 0). }
    hvs_channel: uint32_t;
  end;

{ vc_image_types.h }

{ Common image types used by the vc_image library }

type
{ We have so many rectangle types; let's try to introduce a common one. }
  PVC_RECT_T = ^VC_RECT_T;
  VC_RECT_T = record
    x: int32_t;
    y: int32_t;
    width: int32_t;
    height: int32_t;
  end;

  PVC_IMAGE_T = ^VC_IMAGE_T;
  VC_IMAGE_T = record
  end;

const
{ Types of image supported. }
  VC_IMAGE_RGB565 = 1;
  VC_IMAGE_1BPP = 2;
  VC_IMAGE_YUV420 = 3;
  VC_IMAGE_48BPP = 4;
  VC_IMAGE_RGB888 = 5;
  VC_IMAGE_8BPP = 6;
  VC_IMAGE_4BPP = 7;           // 4bpp palettised image
  VC_IMAGE_3D32 = 8;           // A separated format of 16 colour/light shorts followed by 16 z values
  VC_IMAGE_3D32B = 9;          // 16 colours followed by 16 z values
  VC_IMAGE_3D32MAT = 10;       // A separated format of 16 material/colour/light shorts followed by 16 z values
  VC_IMAGE_RGB2X9 = 11;        // 32 bit format containing 18 bits of 6.6.6 RGB, 9 bits per short
  VC_IMAGE_RGB666 = 12;        // 32-bit format holding 18 bits of 6.6.6 RGB
  VC_IMAGE_PAL4_OBSOLETE = 13; // 4bpp palettised image with embedded palette
  VC_IMAGE_PAL8_OBSOLETE = 14; // 8bpp palettised image with embedded palette
  VC_IMAGE_RGBA32 = 15;        // RGB888 with an alpha byte after each pixel */ /* xxx: isn't it BEFORE each pixel?
  VC_IMAGE_YUV422 = 16;        // a line of Y (32-byte padded), a line of U (16-byte padded), and a line of V (16-byte padded)
  VC_IMAGE_RGBA565 = 17;       // RGB565 with a transparent patch
  VC_IMAGE_RGBA16 = 18;        // Compressed (4444) version of RGBA32
  VC_IMAGE_YUV_UV = 19;        // VCIII codec format
  VC_IMAGE_TF_RGBA32 = 20;     // VCIII T-format RGBA8888
  VC_IMAGE_TF_RGBX32 = 21;     // VCIII T-format RGBx8888
  VC_IMAGE_TF_FLOAT = 22;      // VCIII T-format float
  VC_IMAGE_TF_RGBA16 = 23;     // VCIII T-format RGBA4444
  VC_IMAGE_TF_RGBA5551 = 24;   // VCIII T-format RGB5551
  VC_IMAGE_TF_RGB565 = 25;     // VCIII T-format RGB565
  VC_IMAGE_TF_YA88 = 26;       // VCIII T-format 8-bit luma and 8-bit alpha
  VC_IMAGE_TF_BYTE = 27;       // VCIII T-format 8 bit generic sample
  VC_IMAGE_TF_PAL8 = 28;       // VCIII T-format 8-bit palette
  VC_IMAGE_TF_PAL4 = 29;       // VCIII T-format 4-bit palette
  VC_IMAGE_TF_ETC1 = 30;       // VCIII T-format Ericsson Texture Compressed
  VC_IMAGE_BGR888 = 31;        // RGB888 with R & B swapped
  VC_IMAGE_BGR888_NP = 32;     // RGB888 with R & B swapped, but with no pitch, i.e. no padding after each row of pixels
  VC_IMAGE_BAYER = 33;         // Bayer image, extra defines which variant is being used
  VC_IMAGE_CODEC = 34;         // General wrapper for codec images e.g. JPEG from camera
  VC_IMAGE_YUV_UV32 = 35;      // VCIII codec format
  VC_IMAGE_TF_Y8 = 36;         // VCIII T-format 8-bit luma
  VC_IMAGE_TF_A8 = 37;         // VCIII T-format 8-bit alpha
  VC_IMAGE_TF_SHORT = 38;      // VCIII T-format 16-bit generic sample
  VC_IMAGE_TF_1BPP = 39;       // VCIII T-format 1bpp black/white
  VC_IMAGE_OPENGL = 40;
  VC_IMAGE_YUV444I = 41;       // VCIII-B0 HVS YUV 4:4:4 interleaved samples
  VC_IMAGE_YUV422PLANAR = 42;  // Y, U, & V planes separately (VC_IMAGE_YUV422 has them interleaved on a per line basis)
  VC_IMAGE_ARGB8888 = 43;      // 32bpp with 8bit alpha at MS byte, with R, G, B (LS byte)
  VC_IMAGE_XRGB8888 = 44;      // 32bpp with 8bit unused at MS byte, with R, G, B (LS byte)

  VC_IMAGE_YUV422YUYV = 45;    // interleaved 8 bit samples of Y, U, Y, V
  VC_IMAGE_YUV422YVYU = 46;    // interleaved 8 bit samples of Y, V, Y, U
  VC_IMAGE_YUV422UYVY = 47;    // interleaved 8 bit samples of U, Y, V, Y
  VC_IMAGE_YUV422VYUY = 48;    // interleaved 8 bit samples of V, Y, U, Y

  VC_IMAGE_RGBX32 = 49;        // 32bpp like RGBA32 but with unused alpha
  VC_IMAGE_RGBX8888 = 50;      // 32bpp, corresponding to RGBA with unused alpha
  VC_IMAGE_BGRX8888 = 51;      // 32bpp, corresponding to BGRA with unused alpha

  VC_IMAGE_YUV420SP = 52;      // Y as a plane, then UV byte interleaved in plane with with same pitch, half height
  VC_IMAGE_YUV444PLANAR = 53;  // Y, U, & V planes separately 4:4:4

  VC_IMAGE_TF_U8 = 54;         // T-format 8-bit U - same as TF_Y8 buf from U plane
  VC_IMAGE_TF_V8 = 55;         // T-format 8-bit U - same as TF_Y8 buf from V plane

type
  PVC_IMAGE_TYPE_T = ^VC_IMAGE_TYPE_T;
  VC_IMAGE_TYPE_T = Cardinal;

const
{ Image transformations (flips and 90 degree rotations). These are made out of 3 primitives (transpose is done first).
  These must match the DISPMAN and Media Player definitions. }
  TRANSFORM_HFLIP = 1 shl 0;
  TRANSFORM_VFLIP = 1 shl 1;
  TRANSFORM_TRANSPOSE = 1 shl 2;

  VC_IMAGE_ROT0 = 0;
  VC_IMAGE_MIRROR_ROT0 = TRANSFORM_HFLIP;
  VC_IMAGE_MIRROR_ROT180 = TRANSFORM_VFLIP;
  VC_IMAGE_ROT180 = TRANSFORM_HFLIP or TRANSFORM_VFLIP;
  VC_IMAGE_MIRROR_ROT90 = TRANSFORM_TRANSPOSE;
  VC_IMAGE_ROT270 = TRANSFORM_TRANSPOSE or TRANSFORM_HFLIP;
  VC_IMAGE_ROT90 = TRANSFORM_TRANSPOSE or TRANSFORM_VFLIP;
  VC_IMAGE_MIRROR_ROT270 = TRANSFORM_TRANSPOSE or TRANSFORM_HFLIP or TRANSFORM_VFLIP;

type
  PVC_IMAGE_TRANSFORM_T = ^VC_IMAGE_TRANSFORM_T;
  VC_IMAGE_TRANSFORM_T = Cardinal;

const
{ defined to be identical to register bits }
  VC_IMAGE_BAYER_RGGB = 0;
  VC_IMAGE_BAYER_GBRG = 1;
  VC_IMAGE_BAYER_BGGR = 2;
  VC_IMAGE_BAYER_GRBG = 3;

type
  PVC_IMAGE_BAYER_ORDER_T = ^VC_IMAGE_BAYER_ORDER_T;
  VC_IMAGE_BAYER_ORDER_T = Cardinal;

const
{ defined to be identical to register bits }
  VC_IMAGE_BAYER_RAW6 = 0;
  VC_IMAGE_BAYER_RAW7 = 1;
  VC_IMAGE_BAYER_RAW8 = 2;
  VC_IMAGE_BAYER_RAW10 = 3;
  VC_IMAGE_BAYER_RAW12 = 4;
  VC_IMAGE_BAYER_RAW14 = 5;
  VC_IMAGE_BAYER_RAW16 = 6;
  VC_IMAGE_BAYER_RAW10_8 = 7;
  VC_IMAGE_BAYER_RAW12_8 = 8;
  VC_IMAGE_BAYER_RAW14_8 = 9;
  VC_IMAGE_BAYER_RAW10L = 11;
  VC_IMAGE_BAYER_RAW12L = 12;
  VC_IMAGE_BAYER_RAW14L = 13;
  VC_IMAGE_BAYER_RAW16_BIG_ENDIAN = 14;
  VC_IMAGE_BAYER_RAW4 = 15;

type
  PVC_IMAGE_BAYER_FORMAT_T = ^VC_IMAGE_BAYER_FORMAT_T;
  VC_IMAGE_BAYER_FORMAT_T = LongWord;

{ vc_dispmanx_types.h }

type
{ Opaque handles }
  PDISPMANX_DISPLAY_HANDLE_T = ^DISPMANX_DISPLAY_HANDLE_T;
  DISPMANX_DISPLAY_HANDLE_T = uint32_t;

  PDISPMANX_UPDATE_HANDLE_T = ^DISPMANX_UPDATE_HANDLE_T;
  DISPMANX_UPDATE_HANDLE_T = uint32_t;

  PDISPMANX_ELEMENT_HANDLE_T = ^DISPMANX_ELEMENT_HANDLE_T;
  DISPMANX_ELEMENT_HANDLE_T = uint32_t;

  PDISPMANX_RESOURCE_HANDLE_T = ^DISPMANX_RESOURCE_HANDLE_T;
  DISPMANX_RESOURCE_HANDLE_T = uint32_t;

  PDISPMANX_PROTECTION_T = ^DISPMANX_PROTECTION_T;
  DISPMANX_PROTECTION_T = uint32_t;

const
  DISPMANX_NO_HANDLE = 0;

  DISPMANX_PROTECTION_MAX = $0F;
  DISPMANX_PROTECTION_NONE = 0;
  DISPMANX_PROTECTION_HDCP = 11; // Derived from the WM DRM levels, 101-300

{ Default display IDs. Note: if you overwrite with you own dispmanx_platfrom_init function, you should use IDs you
  provided during dispmanx_display_attach. }
  DISPMANX_ID_MAIN_LCD = 0;
  DISPMANX_ID_AUX_LCD = 1;
  DISPMANX_ID_HDMI = 2;
  DISPMANX_ID_SDTV = 3;
  DISPMANX_ID_FORCE_LCD = 4;
  DISPMANX_ID_FORCE_TV = 5;
  DISPMANX_ID_FORCE_OTHER = 6; // non-default display

{ Return codes. Nonzero ones indicate failure. }
  DISPMANX_SUCCESS = 0;
  DISPMANX_INVALID = -1;

type
  PDISPMANX_STATUS_T = ^DISPMANX_STATUS_T;
  DISPMANX_STATUS_T = Cardinal;

const
{ Bottom 2 bits sets the orientation }
  DISPMANX_NO_ROTATE = 0;
  DISPMANX_ROTATE_90 = 1;
  DISPMANX_ROTATE_180 = 2;
  DISPMANX_ROTATE_270 = 3;

  DISPMANX_FLIP_HRIZ = 1 shl 16;
  DISPMANX_FLIP_VERT = 1 shl 17;

{ extra flags for controlling snapshot behaviour }
  DISPMANX_SNAPSHOT_NO_YUV = 1 shl 24;
  DISPMANX_SNAPSHOT_NO_RGB = 1 shl 25;
  DISPMANX_SNAPSHOT_FILL = 1 shl 26;
  DISPMANX_SNAPSHOT_SWAP_RED_BLUE = 1 shl 27;
  DISPMANX_SNAPSHOT_PACK = 1 shl 28;

type
  PDISPMANX_TRANSFORM_T = ^DISPMANX_TRANSFORM_T;
  DISPMANX_TRANSFORM_T = Cardinal;

const
{ Bottom 2 bits sets the alpha mode }
  DISPMANX_FLAGS_ALPHA_FROM_SOURCE = 0;
  DISPMANX_FLAGS_ALPHA_FIXED_ALL_PIXELS = 1;
  DISPMANX_FLAGS_ALPHA_FIXED_NON_ZERO = 2;
  DISPMANX_FLAGS_ALPHA_FIXED_EXCEED_0X07 = 3;

  DISPMANX_FLAGS_ALPHA_PREMULT = 1 shl 16;
  DISPMANX_FLAGS_ALPHA_MIX = 1 shl 17;

type
  PDISPMANX_FLAGS_ALPHA_T = ^DISPMANX_FLAGS_ALPHA_T;
  DISPMANX_FLAGS_ALPHA_T = Cardinal;

  PDISPMANX_ALPHA_T = ^DISPMANX_ALPHA_T;
  DISPMANX_ALPHA_T = record
    flags: DISPMANX_FLAGS_ALPHA_T;
    opacity: uint32_t;
    mask: PVC_IMAGE_T;
  end;

  PVC_DISPMANX_ALPHA_T = ^VC_DISPMANX_ALPHA_T;
  VC_DISPMANX_ALPHA_T = record // for use with vmcs_host
    flags: DISPMANX_FLAGS_ALPHA_T;
    opacity: uint32_t;
    mask: DISPMANX_RESOURCE_HANDLE_T;
  end;

const
  DISPMANX_FLAGS_CLAMP_NONE = 0;
  DISPMANX_FLAGS_CLAMP_LUMA_TRANSPARENT = 1;
  DISPMANX_FLAGS_CLAMP_TRANSPARENT = 2;
  DISPMANX_FLAGS_CLAMP_REPLACE = 3;

type
  PDISPMANX_FLAGS_CLAMP_T = ^DISPMANX_FLAGS_CLAMP_T;
  DISPMANX_FLAGS_CLAMP_T = Cardinal;

const
  DISPMANX_FLAGS_KEYMASK_OVERRIDE = 1;
  DISPMANX_FLAGS_KEYMASK_SMOOTH = 1 shl 1;
  DISPMANX_FLAGS_KEYMASK_CR_INV = 1 shl 2;
  DISPMANX_FLAGS_KEYMASK_CB_INV = 1 shl 3;
  DISPMANX_FLAGS_KEYMASK_YY_INV = 1 shl 4;

type
  PDISPMANX_FLAGS_KEYMASK_T = ^DISPMANX_FLAGS_KEYMASK_T;
  DISPMANX_FLAGS_KEYMASK_T = LongWord;

  PDISPMANX_CLAMP_KEYS_T = ^DISPMANX_CLAMP_KEYS_T;
  DISPMANX_CLAMP_KEYS_T = record
  case Integer of
    0: (yy_upper: uint8_t;
        yy_lower: uint8_t;
        cr_upper: uint8_t;
        cr_lower: uint8_t;
        cb_upper: uint8_t;
        cb_lower: uint8_t);
    1: (red_upper: uint8_t;
        red_lower: uint8_t;
        blue_upper: uint8_t;
        blue_lower: uint8_t;
        green_upper: uint8_t;
        green_lower: uint8_t);
  end;

  PDISPMANX_CLAMP_T = ^DISPMANX_CLAMP_T;
  DISPMANX_CLAMP_T = record
    mode: DISPMANX_FLAGS_CLAMP_T;
    key_mask: DISPMANX_FLAGS_KEYMASK_T;
    key_value: DISPMANX_CLAMP_KEYS_T;
    replace_value: uint32_t;
  end;

  PDISPMANX_MODEINFO_T = ^DISPMANX_MODEINFO_T;
  DISPMANX_MODEINFO_T = record
    width: int32_t;
    height: int32_t;
    transform: DISPMANX_TRANSFORM_T;
    input_format: DISPLAY_INPUT_FORMAT_T;
  end;

{ Update callback. }
  DISPMANX_CALLBACK_FUNC_T = function(u: DISPMANX_UPDATE_HANDLE_T; arg: Pointer): Pointer; cdecl;

{ Progress callback }
  DISPMANX_PROGRESS_CALLBACK_FUNC_T = function(u: DISPMANX_UPDATE_HANDLE_T; line: uint32_t; arg: Pointer): Pointer; cdecl;

{ Pluggable display interface }
  PDISPMANX_DISPLAY_FUNCS_T = ^DISPMANX_DISPLAY_FUNCS_T;
  DISPMANX_DISPLAY_FUNCS_T = record
  public type
    PIntArray3 = ^TIntArray3;
    TIntArray3 = array[0..2] of int32_t;
    PUIntArray3 = ^TUIntArray3;
    TUIntArray3 = array[0..2] of uint32_t;
  public
    { Get essential HVS configuration to be passed to the HVS driver. Options is any combination of the following
      flags: HVS_ONESHOT, HVS_FIFOREG, HVS_FIFO32, HVS_AUTOHSTART, HVS_INTLACE; and if HVS_FIFOREG, one of
      HVS_FMT_RGB888, HVS_FMT_RGB565, HVS_FMT_RGB666, HVS_FMT_YUV. }
    get_hvs_config: function(instance: Pointer; chan: Puint32_t; options: Puint32_t; info: PDISPLAY_INFO_T;
      bg_colour: Puint32_t; test_mode: Puint32_t): int32_t; cdecl;

    { Get optional HVS configuration for gamma tables, OLED matrix and dither controls.
      Set these function pointers to NULL if the relevant features are not required. }
    get_gamma_params: function(instance: Pointer; gain, offset, gamma: PIntArray3): int32_t; cdecl;
    get_oled_params: function(instance: Pointer; offsets: Puint32_t; coeffs: PUIntArray3): int32_t; cdecl;
    get_dither: function(instance: Pointer; dither_depth, dither_type: Puint32_t): int32_t; cdecl;

    { Get mode information, which may be returned to the applications as a courtesy.
      Transform should be set to 0, and [width,height] should be final dimensions. }
    get_info: function(instance: Pointer; info: PDISPMANX_MODEINFO_T): int32_t; cdecl;

    { Inform driver that the application refcount has become nonzero / zero
      These callbacks might perhaps be used for backlight and power management. }
    open: function(instance: Pointer): int32_t; cdecl;
    close: function(instance: Pointer): int32_t; cdecl;

    { Display list updated callback. Primarily of use to a "one-shot" display.
      For convenience of the driver, we pass the register address of the HVS FIFO. }
    dlist_updated: procedure(instance: Pointer; fifo_reg: Puint32_t); cdecl;

    { End-of-field callback. This may occur in an interrupt context. }
    eof_callback: procedure(instance: Pointer); cdecl;

    { Return screen resolution format }
    get_input_format: function(instance: Pointer): DISPLAY_INPUT_FORMAT_T; cdecl;

    suspend_resume: function(instance: Pointer; up: int32_t): int32_t; cdecl;
    get_3d_format: function(instance: Pointer): DISPLAY_3D_FORMAT_T; cdecl;
  end;

{ eglplatform.h }

type
  PEGL_DISPMANX_WINDOW_T = ^EGL_DISPMANX_WINDOW_T;
  EGL_DISPMANX_WINDOW_T = record
    element: DISPMANX_ELEMENT_HANDLE_T;
    width: LongInt;
    height: LongInt;
  end;

const
  lib_bcm_host = 'bcm_host';

{ vc_dispmanx.h }

{ Opens a display on the given device }
function vc_dispmanx_display_open(device: LongWord): DISPMANX_DISPLAY_HANDLE_T; cdecl;
  external lib_bcm_host name 'vc_dispmanx_display_open';

{ Closes a display }
function vc_dispmanx_display_close(display: DISPMANX_DISPLAY_HANDLE_T): Integer; cdecl;
  external lib_bcm_host name 'vc_dispmanx_display_close';

{ Start a new update, DISPMANX_NO_HANDLE on error }
function vc_dispmanx_update_start(priority: LongInt): DISPMANX_UPDATE_HANDLE_T; cdecl;
  external lib_bcm_host name 'vc_dispmanx_update_start';

{ Add an elment to a display as part of an update }
function vc_dispmanx_element_add(update: DISPMANX_UPDATE_HANDLE_T; display: DISPMANX_DISPLAY_HANDLE_T; layer: LongInt;
  dest_rect: PVC_RECT_T; src: DISPMANX_RESOURCE_HANDLE_T; src_rect: PVC_RECT_T; protection: DISPMANX_PROTECTION_T;
  alpha: PVC_DISPMANX_ALPHA_T; clamp: PDISPMANX_CLAMP_T;
  transform: DISPMANX_TRANSFORM_T): DISPMANX_ELEMENT_HANDLE_T; cdecl;
  external lib_bcm_host name 'vc_dispmanx_element_add';

{ End an update and wait for it to complete }
function vc_dispmanx_update_submit_sync(update: DISPMANX_UPDATE_HANDLE_T): LongInt; cdecl;
  external lib_bcm_host name 'vc_dispmanx_update_submit_sync';

{ bcm_host.h }

procedure bcm_host_init; cdecl; external lib_bcm_host name 'bcm_host_init';
procedure bcm_host_deinit; cdecl; external lib_bcm_host name 'bcm_host_deinit';

function graphics_get_display_size(display_number: Word; width, height: PLongWord): LongInt; cdecl;
  external lib_bcm_host name 'graphics_get_display_size';

implementation

end.

