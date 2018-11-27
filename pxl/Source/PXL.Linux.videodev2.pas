unit PXL.Linux.videodev2;
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
{
  "videodev2.h" translation to Pascal by Yuriy Kotsarenko, 2015.
  The original C header file was obtained at:
    http://www.cs.fsu.edu/~baker/devices/lxr/http/source/linux/include/linux/videodev2.h
}
interface

{$INCLUDE PXL.Linux.config.inc}

uses
  Unix;

const
{ Common stuff for both V4L1 and V4L2 }
  VIDEO_MAX_FRAME = 32;

{ These defines are V4L1 specific and should not be used with the V4L2 API!
  They will be removed from this header in the future. }
  VID_TYPE_CAPTURE = 1; { Can capture }
  VID_TYPE_TUNER = 2; { Can tune }
  VID_TYPE_TELETEXT = 4; { Does teletext }
  VID_TYPE_OVERLAY = 8; { Overlay onto frame buffer }
  VID_TYPE_CHROMAKEY = 16; { Overlay by chromakey }
  VID_TYPE_CLIPPING = 32; { Can clip }
  VID_TYPE_FRAMERAM = 64; { Uses the frame buffer memory }
  VID_TYPE_SCALES = 128; { Scalable }
  VID_TYPE_MONOCHROME = 256; { Monochrome only }
  VID_TYPE_SUBCAPTURE = 512; { Can capture subareas of the image }
  VID_TYPE_MPEG_DECODER = 1024; { Can decode MPEG streams }
  VID_TYPE_MPEG_ENCODER = 2048; { Can encode MPEG streams }
  VID_TYPE_MJPEG_DECODER = 4096; { Can decode MJPEG streams }
  VID_TYPE_MJPEG_ENCODER = 8192; { Can encode MJPEG streams }

{ Miscellaneous }
{  Four-character-code (FOURCC) }
function v4l2_fourcc(const a, b, c, d: AnsiChar): Cardinal; inline;

{ Enums }
type
  Pv4l2_field = ^v4l2_field;
  v4l2_field = Cardinal;

const
  V4L2_FIELD_ANY = 0; { driver can choose from none, top, bottom, interlaced depending on whatever it thinks is approximate }
  V4L2_FIELD_NONE = 1; { this device has no fields }
  V4L2_FIELD_TOP = 2; { top field only }
  V4L2_FIELD_BOTTOM = 3; { bottom field only }
  V4L2_FIELD_INTERLACED = 4; { both fields interlaced }
  V4L2_FIELD_SEQ_TB = 5; { both fields sequential into one buffer, top-bottom order }
  V4L2_FIELD_SEQ_BT = 6; { same as above + bottom-top order }
  V4L2_FIELD_ALTERNATE = 7; { both fields alternating into separate buffers }
  V4L2_FIELD_INTERLACED_TB = 8; { both fields interlaced, top field first and the top field is transmitted first }
  V4L2_FIELD_INTERLACED_BT = 9; { both fields interlaced, top field first and the bottom field is transmitted first }

function V4L2_FIELD_HAS_TOP(const field: v4l2_field): Boolean; inline;
function V4L2_FIELD_HAS_BOTTOM(const field: v4l2_field): Boolean; inline;
function V4L2_FIELD_HAS_BOTH(const field: v4l2_field): Boolean; inline;

type
  Pv4l2_buf_type = ^v4l2_buf_type;
  v4l2_buf_type = Cardinal;

const
  V4L2_BUF_TYPE_VIDEO_CAPTURE = 1;
  V4L2_BUF_TYPE_VIDEO_OUTPUT = 2;
  V4L2_BUF_TYPE_VIDEO_OVERLAY = 3;
  V4L2_BUF_TYPE_VBI_CAPTURE = 4;
  V4L2_BUF_TYPE_VBI_OUTPUT = 5;
  V4L2_BUF_TYPE_SLICED_VBI_CAPTURE = 6;
  V4L2_BUF_TYPE_SLICED_VBI_OUTPUT = 7;
  V4L2_BUF_TYPE_VIDEO_OUTPUT_OVERLAY = 8; { Experimental }
  V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE = 9;
  V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE = 10;
  V4L2_BUF_TYPE_PRIVATE = $80;

type
  Pv4l2_ctrl_type = ^v4l2_ctrl_type;
  v4l2_ctrl_type = Cardinal;

const
  V4L2_CTRL_TYPE_INTEGER = 1;
  V4L2_CTRL_TYPE_BOOLEAN = 2;
  V4L2_CTRL_TYPE_MENU = 3;
  V4L2_CTRL_TYPE_BUTTON = 4;
  V4L2_CTRL_TYPE_INTEGER64 = 5;
  V4L2_CTRL_TYPE_CTRL_CLASS = 6;

type
  Pv4l2_tuner_type = ^v4l2_tuner_type;
  v4l2_tuner_type = Cardinal;

const
  V4L2_TUNER_RADIO = 1;
  V4L2_TUNER_ANALOG_TV = 2;
  V4L2_TUNER_DIGITAL_TV = 3;

type
  Pv4l2_memory = ^v4l2_memory;
  v4l2_memory = Cardinal;

const
  V4L2_MEMORY_MMAP = 1;
  V4L2_MEMORY_USERPTR = 2;
  V4L2_MEMORY_OVERLAY = 3;

{ see also http://vektor.theorem.ca/graphics/ycbcr/ }
type
  Pv4l2_colorspace = ^v4l2_colorspace;
  v4l2_colorspace = Cardinal;

const
  V4L2_COLORSPACE_SMPTE170M = 1; { ITU-R 601 -- broadcast NTSC/PAL }
  V4L2_COLORSPACE_SMPTE240M = 2; { 1125-Line (US) HDTV }
  V4L2_COLORSPACE_REC709 = 3; { HD and modern captures. }
  V4L2_COLORSPACE_BT878 = 4; { broken BT878 extents (601, luma range 16-253 instead of 16-235) }
  V4L2_COLORSPACE_470_SYSTEM_M = 5; { These should be useful. Assume 601 extents. }
  V4L2_COLORSPACE_470_SYSTEM_BG = 6;
  V4L2_COLORSPACE_JPEG = 7; { I know there will be cameras that send this. So, this is unspecified chromaticities and
                              full 0-255 on each of the Y'CbCr components }
  V4L2_COLORSPACE_SRGB = 8; { For RGB colourspaces, this is probably a good start. }

type
  Pv4l2_priority = ^v4l2_priority;
  v4l2_priority = Cardinal;

const
  V4L2_PRIORITY_UNSET = 0; { not initialized }
  V4L2_PRIORITY_BACKGROUND = 1;
  V4L2_PRIORITY_INTERACTIVE = 2;
  V4L2_PRIORITY_RECORD = 3;
  V4L2_PRIORITY_DEFAULT = V4L2_PRIORITY_INTERACTIVE;

type
  Pv4l2_rect = ^v4l2_rect;
  v4l2_rect = record
    left: Integer;
    top: Integer;
    width: Integer;
    height: Integer;
  end;

  Pv4l2_fract = ^v4l2_fract;
  v4l2_fract = record
    numerator: Cardinal;
    denominator: Cardinal;
  end;

{ Driver Capabilities }
type
  Pv4l2_capability = ^v4l2_capability;
  v4l2_capability = record
    driver: array[0..15] of AnsiChar; { i.e. "bttv" }
    card: array[0..31] of AnsiChar; { i.e. "Hauppauge WinTV" }
    bus_info: array[0..31] of AnsiChar; { "PCI:" + pci_name(pci_dev) }
    version: Cardinal; { should use KERNEL_VERSION() }
    capabilities: Cardinal; { Device capabilities }
    reserved: array[0..3] of Cardinal;
  end;

const
{ Values for 'capabilities' field }
  V4L2_CAP_VIDEO_CAPTURE = $00000001; { Is a video capture device }
  V4L2_CAP_VIDEO_OUTPUT = $00000002; { Is a video output device }
  V4L2_CAP_VIDEO_OVERLAY = $00000004; { Can do video overlay }
  V4L2_CAP_VBI_CAPTURE = $00000010; { Is a raw VBI capture device }
  V4L2_CAP_VBI_OUTPUT = $00000020; { Is a raw VBI output device }
  V4L2_CAP_SLICED_VBI_CAPTURE = $00000040; { Is a sliced VBI capture device }
  V4L2_CAP_SLICED_VBI_OUTPUT = $00000080; { Is a sliced VBI output device }
  V4L2_CAP_RDS_CAPTURE = $00000100; { RDS data capture }
  V4L2_CAP_VIDEO_OUTPUT_OVERLAY = $00000200; { Can do video output overlay }
  V4L2_CAP_HW_FREQ_SEEK = $00000400; { Can do hardware frequency seek }
  V4L2_CAP_RDS_OUTPUT = $00000800; { Is an RDS encoder }
  V4L2_CAP_VIDEO_CAPTURE_MPLANE = $00001000; { Is a video capture device that supports multiplanar formats }
  V4L2_CAP_VIDEO_OUTPUT_MPLANE = $00002000; { Is a video output device that supports multiplanar formats }
  V4L2_CAP_TUNER = $00010000; { has a tuner }
  V4L2_CAP_AUDIO = $00020000; { has audio support }
  V4L2_CAP_RADIO = $00040000; { is a radio device }
  V4L2_CAP_MODULATOR = $00080000; { has a modulator }
  V4L2_CAP_READWRITE = $01000000; { read/write systemcalls }
  V4L2_CAP_ASYNCIO = $02000000; { async I/O }
  V4L2_CAP_STREAMING = $04000000; { streaming I/O ioctls }

type
{ Video Image Format}
  Pv4l2_pix_format = ^v4l2_pix_format;
  v4l2_pix_format = record
    width: Cardinal;
    height: Cardinal;
    pixelformat: Cardinal;
    field: v4l2_field;
    bytesperline: Cardinal; { for padding, zero if unused }
    sizeimage: Cardinal;
    colorspace: v4l2_colorspace;
    priv: Cardinal; { private data, depends on pixelformat }
  end;

{ RGB formats }
function V4L2_PIX_FMT_RGB332: Cardinal; inline; { 8 RGB-3-3-2 }
function V4L2_PIX_FMT_RGB444: Cardinal; inline; { 16 xxxxrrrr ggggbbbb }
function V4L2_PIX_FMT_RGB555: Cardinal; inline; { 16 RGB-5-5-5 }
function V4L2_PIX_FMT_RGB565: Cardinal; inline; { 16 RGB-5-6-5 }
function V4L2_PIX_FMT_RGB555X: Cardinal; inline; { 16 RGB-5-5-5 BE }
function V4L2_PIX_FMT_RGB565X: Cardinal; inline; { 16 RGB-5-6-5 BE }
function V4L2_PIX_FMT_BGR666: Cardinal; inline; { 18 BGR-6-6-6 }
function V4L2_PIX_FMT_BGR24: Cardinal; inline; { 24 BGR-8-8-8 }
function V4L2_PIX_FMT_RGB24: Cardinal; inline; { 24 RGB-8-8-8 }
function V4L2_PIX_FMT_BGR32: Cardinal; inline; { 32 BGR-8-8-8-8 }
function V4L2_PIX_FMT_RGB32: Cardinal; inline; { 32 RGB-8-8-8-8 }

{ Grey formats }
function V4L2_PIX_FMT_GREY: Cardinal; inline; { 8 Greyscale }
function V4L2_PIX_FMT_Y4: Cardinal; inline; { 4 Greyscale }
function V4L2_PIX_FMT_Y6: Cardinal; inline; { 6 Greyscale }
function V4L2_PIX_FMT_Y10: Cardinal; inline; { 10 Greyscale }
function V4L2_PIX_FMT_Y12: Cardinal; inline; { 12 Greyscale }
function V4L2_PIX_FMT_Y16: Cardinal; inline; { 16 Greyscale }

{ Grey bit-packed formats }
function V4L2_PIX_FMT_Y10BPACK: Cardinal; inline; { 10 Greyscale bit-packed }

{ Palette formats }
function V4L2_PIX_FMT_PAL8: Cardinal; inline; { 8 8-bit palette }

{ Luminance+Chrominance formats }
function V4L2_PIX_FMT_YVU410: Cardinal; inline; { 9 YVU 4:1:0 }
function V4L2_PIX_FMT_YVU420: Cardinal; inline; { 12 YVU 4:2:0 }
function V4L2_PIX_FMT_YUYV: Cardinal; inline; { 16 YUV 4:2:2 }
function V4L2_PIX_FMT_YYUV: Cardinal; inline; { 16 YUV 4:2:2 }
function V4L2_PIX_FMT_YVYU: Cardinal; inline; { 16 YVU 4:2:2 }
function V4L2_PIX_FMT_UYVY: Cardinal; inline; { 16 YUV 4:2:2 }
function V4L2_PIX_FMT_VYUY: Cardinal; inline; { 16 YUV 4:2:2 }
function V4L2_PIX_FMT_YUV422P: Cardinal; inline; { 16 YVU422 planar }
function V4L2_PIX_FMT_YUV411P: Cardinal; inline; { 16 YVU411 planar }
function V4L2_PIX_FMT_Y41P: Cardinal; inline; { 12 YUV 4:1:1 }
function V4L2_PIX_FMT_YUV444: Cardinal; inline; { 16 xxxxyyyy uuuuvvvv }
function V4L2_PIX_FMT_YUV555: Cardinal; inline; { 16 YUV-5-5-5 }
function V4L2_PIX_FMT_YUV565: Cardinal; inline; { 16 YUV-5-6-5 }
function V4L2_PIX_FMT_YUV32: Cardinal; inline; { 32 YUV-8-8-8-8 }
function V4L2_PIX_FMT_YUV410: Cardinal; inline; { 9 YUV 4:1:0 }
function V4L2_PIX_FMT_YUV420: Cardinal; inline; { 12 YUV 4:2:0 }
function V4L2_PIX_FMT_HI240: Cardinal; inline; { 8 8-bit color }
function V4L2_PIX_FMT_HM12: Cardinal; inline; { 8 YUV 4:2:0 16x16 macroblocks }
function V4L2_PIX_FMT_M420: Cardinal; inline; { 12 YUV 4:2:0 2 lines y, 1 line uv interleaved }

{ two planes -- one Y, one Cr + Cb interleaved }
function V4L2_PIX_FMT_NV12: Cardinal; inline; { 12 Y/CbCr 4:2:0 }
function V4L2_PIX_FMT_NV21: Cardinal; inline; { 12 Y/CrCb 4:2:0 }
function V4L2_PIX_FMT_NV16: Cardinal; inline; { 16 Y/CbCr 4:2:2 }
function V4L2_PIX_FMT_NV61: Cardinal; inline; { 16 Y/CrCb 4:2:2 }

{ two non contiguous planes - one Y, one Cr + Cb interleaved }
function V4L2_PIX_FMT_NV12M: Cardinal; inline; { 12 Y/CbCr 4:2:0 }
function V4L2_PIX_FMT_NV12MT: Cardinal; inline; { 12 Y/CbCr 4:2:0 64x32 macroblocks }

{ three non contiguous planes - Y, Cb, Cr }
function V4L2_PIX_FMT_YUV420M: Cardinal; inline; { 12 YUV420 planar }

{ Bayer formats - see http://www.siliconimaging.com/RGB%20Bayer.htm }
function V4L2_PIX_FMT_SBGGR8: Cardinal; inline; { 8 BGBG.. GRGR.. }
function V4L2_PIX_FMT_SGBRG8: Cardinal; inline; { 8 GBGB.. RGRG.. }
function V4L2_PIX_FMT_SGRBG8: Cardinal; inline; { 8 GRGR.. BGBG.. }
function V4L2_PIX_FMT_SRGGB8: Cardinal; inline; { 8 RGRG.. GBGB.. }
function V4L2_PIX_FMT_SBGGR10: Cardinal; inline; { 10 BGBG.. GRGR.. }
function V4L2_PIX_FMT_SGBRG10: Cardinal; inline; { 10 GBGB.. RGRG.. }
function V4L2_PIX_FMT_SGRBG10: Cardinal; inline; { 10 GRGR.. BGBG.. }
function V4L2_PIX_FMT_SRGGB10: Cardinal; inline; { 10 RGRG.. GBGB.. }
function V4L2_PIX_FMT_SBGGR12: Cardinal; inline; { 12 BGBG.. GRGR.. }
function V4L2_PIX_FMT_SGBRG12: Cardinal; inline; { 12 GBGB.. RGRG.. }
function V4L2_PIX_FMT_SGRBG12: Cardinal; inline; { 12 GRGR.. BGBG.. }
function V4L2_PIX_FMT_SRGGB12: Cardinal; inline; { 12 RGRG.. GBGB.. }

{ 10bit raw bayer DPCM compressed to 8 bits }
function V4L2_PIX_FMT_SGRBG10DPCM8: Cardinal; inline;

{ 10bit raw bayer, expanded to 16 bits
  xxxxrrrrrrrrrrxxxxgggggggggg xxxxggggggggggxxxxbbbbbbbbbb... }
function V4L2_PIX_FMT_SBGGR16: Cardinal; inline; { 16 BGBG.. GRGR.. }

{ compressed formats }
function V4L2_PIX_FMT_MJPEG: Cardinal; inline; { Motion-JPEG }
function V4L2_PIX_FMT_JPEG: Cardinal; inline; { JFIF JPEG }
function V4L2_PIX_FMT_DV: Cardinal; inline; { 1394 }
function V4L2_PIX_FMT_MPEG: Cardinal; inline; { MPEG-1/2/4 }

{ Vendor-specific formats }
function V4L2_PIX_FMT_CPIA1: Cardinal; inline; { cpia1 YUV }
function V4L2_PIX_FMT_WNVA: Cardinal; inline; { Winnov hw compress }
function V4L2_PIX_FMT_SN9C10X: Cardinal; inline; { SN9C10x compression }
function V4L2_PIX_FMT_SN9C20X_I420: Cardinal; inline; { SN9C20x YUV 4:2:0 }
function V4L2_PIX_FMT_PWC1: Cardinal; inline; { pwc older webcam }
function V4L2_PIX_FMT_PWC2: Cardinal; inline; { pwc newer webcam }
function V4L2_PIX_FMT_ET61X251: Cardinal; inline; { ET61X251 compression }
function V4L2_PIX_FMT_SPCA501: Cardinal; inline; { YUYV per line }
function V4L2_PIX_FMT_SPCA505: Cardinal; inline; { YYUV per line }
function V4L2_PIX_FMT_SPCA508: Cardinal; inline; { YUVY per line }
function V4L2_PIX_FMT_SPCA561: Cardinal; inline; { compressed GBRG bayer }
function V4L2_PIX_FMT_PAC207: Cardinal; inline; { compressed BGGR bayer }
function V4L2_PIX_FMT_MR97310A: Cardinal; inline; { compressed BGGR bayer }
function V4L2_PIX_FMT_SN9C2028: Cardinal; inline; { compressed GBRG bayer }
function V4L2_PIX_FMT_SQ905C: Cardinal; inline; { compressed RGGB bayer }
function V4L2_PIX_FMT_PJPG: Cardinal; inline; { Pixart 73xx JPEG }
function V4L2_PIX_FMT_OV511: Cardinal; inline; { ov511 JPEG }
function V4L2_PIX_FMT_OV518: Cardinal; inline; { ov518 JPEG }
function V4L2_PIX_FMT_STV0680: Cardinal; inline; { stv0680 bayer }
function V4L2_PIX_FMT_TM6000: Cardinal; inline; { tm5600/tm60x0 }
function V4L2_PIX_FMT_CIT_YYVYUY: Cardinal; inline; { one line of Y then 1 line of VYUY }
function V4L2_PIX_FMT_KONICA420: Cardinal; inline; { YUV420 planar in blocks of 256 pixels }
function V4L2_PIX_FMT_JPGL: Cardinal; inline; { JPEG-Lite }

type
{ Format Enumeration }
  Pv4l2_fmtdesc = ^v4l2_fmtdesc;
  v4l2_fmtdesc = record
    index: Cardinal; { Format number }
    &type: v4l2_buf_type; { buffer type }
    flags: Cardinal;
    description: array[0..31] of AnsiChar; { Description string }
    pixelformat: Cardinal; { Format fourcc }
    reserved: array[0..3] of Cardinal;
  end;

const
  V4L2_FMT_FLAG_COMPRESSED = $0001;

{ Experimental Frame Size and frame rate enumeration }
type
{ Frame Size Enumeration}
  Pv4l2_frmsizetypes = ^v4l2_frmsizetypes;
  v4l2_frmsizetypes = Cardinal;

const
  V4L2_FRMSIZE_TYPE_DISCRETE = 1;
  V4L2_FRMSIZE_TYPE_CONTINUOUS = 2;
  V4L2_FRMSIZE_TYPE_STEPWISE = 3;

type
  Pv4l2_frmsize_discrete = ^v4l2_frmsize_discrete;
  v4l2_frmsize_discrete = record
    width: Cardinal; { Frame width [pixel] }
    height: Cardinal; { Frame height [pixel] }
  end;

  Pv4l2_frmsize_stepwise = ^v4l2_frmsize_stepwise;
  v4l2_frmsize_stepwise = record
    min_width: Cardinal; { Minimum frame width [pixel] }
    max_width: Cardinal; { Maximum frame width [pixel] }
    step_width: Cardinal; { Frame width step size [pixel] }
    min_height: Cardinal; { Minimum frame height [pixel] }
    max_height: Cardinal; { Maximum frame height [pixel] }
    step_height: Cardinal; { Frame height step size [pixel] }
  end;

  Pv4l2_frmsizeenum = ^v4l2_frmsizeenum;
  v4l2_frmsizeenum = record
    index: Cardinal; { Frame size number }
    pixel_format: Cardinal; { Pixel format }
    &type: Cardinal; { Frame size type the device supports. }
    case Integer of
      { Frame size }
      0: (discrete: v4l2_frmsize_discrete);
      1: (stepwise: v4l2_frmsize_stepwise;
          reserved: array[0..1] of Cardinal);
  end;

{ Frame Rate Enumeration }
type
  Pv4l2_frmivaltypes = ^v4l2_frmivaltypes;
  v4l2_frmivaltypes = Cardinal;

const
  V4L2_FRMIVAL_TYPE_DISCRETE = 1;
  V4L2_FRMIVAL_TYPE_CONTINUOUS = 2;
  V4L2_FRMIVAL_TYPE_STEPWISE = 3;

type
  Pv4l2_frmival_stepwise = ^v4l2_frmival_stepwise;
  v4l2_frmival_stepwise = record
    min: v4l2_fract; { Minimum frame interval [s] }
    max: v4l2_fract; { Maximum frame interval [s] }
    step: v4l2_fract; { Frame interval step size [s] }
  end;

  Pv4l2_frmivalenum = ^v4l2_frmivalenum;
  v4l2_frmivalenum = record
    index: Cardinal; { Frame format index }
    pixel_format: Cardinal; { Pixel format }
    width: Cardinal; { Frame width }
    height: Cardinal; { Frame height }
    &type: Cardinal; { Frame interval type the device supports. }
    case Integer of
    { Frame interval }
      0: (discrete: v4l2_fract);
      1: (stepwise: v4l2_frmival_stepwise;
          reserved: array[0..1] of Cardinal)
  end;

{ Time Code }
  Pv4l2_timecode = ^v4l2_timecode;
  v4l2_timecode = record
    &type: Cardinal;
    flags: Cardinal;
    frames: Byte;
    seconds: Byte;
    minutes: Byte;
    hours: Byte;
    userbits: array[0..3] of Byte;
  end;

const
{ Type }
  V4L2_TC_TYPE_24FPS = 1;
  V4L2_TC_TYPE_25FPS = 2;
  V4L2_TC_TYPE_30FPS = 3;
  V4L2_TC_TYPE_50FPS = 4;
  V4L2_TC_TYPE_60FPS = 5;

{ Flags }
  V4L2_TC_FLAG_DROPFRAME = $0001; { "drop-frame" mode }
  V4L2_TC_FLAG_COLORFRAME = $0002;
  V4L2_TC_USERBITS_field = $000C;
  V4L2_TC_USERBITS_USERDEFINED = $0000;
  V4L2_TC_USERBITS_8BITCHARS = $0008;

type
{ The above is based on SMPTE timecodes }
  Pv4l2_jpegcompression = ^v4l2_jpegcompression;
  v4l2_jpegcompression = record
    quality: Integer;
    APPn: Integer; { Number of APP segment to be written, must be 0..15 }
    APP_len: Integer; { Length of data in JPEG APPn segment }
    APP_data: array[0..59] of AnsiChar; { Data in the JPEG APPn segment. }
    COM_len: Integer; { Length of data in JPEG COM segment }
    COM_data: array[0..59] of AnsiChar; { Data in JPEG COM segment }
    jpeg_markers: Cardinal; { Which markers should go into the JPEG output. Unless you exactly know what you do, leave
                              them untouched. Inluding less markers will make the resulting code smaller, but there
                              will be fewer applications which can read it. The presence of the APP and COM marker is
                              influenced by APP_len and COM_len ONLY, not by this property! }
  end;

const
  V4L2_JPEG_MARKER_DHT = 1 shl 3; { Define Huffman Tables }
  V4L2_JPEG_MARKER_DQT = 1 shl 4; { Define Quantization Tables }
  V4L2_JPEG_MARKER_DRI = 1 shl 5; { Define Restart Interval }
  V4L2_JPEG_MARKER_COM = 1 shl 6; { Comment segment }
  V4L2_JPEG_MARKER_APP = 1 shl 7; { App segment, driver will allways use APP0 }

{ Memory-mapping Buffers }
type
  Pv4l2_requestbuffers = ^v4l2_requestbuffers;
  v4l2_requestbuffers = record
    count: Cardinal;
    &type: v4l2_buf_type;
    memory: v4l2_memory;
    reserved: array[0..1] of Cardinal;
  end;

  Pv4l2_buffer = ^v4l2_buffer;
  v4l2_buffer = record
    index: Cardinal;
    &type: v4l2_buf_type;
    bytesused: Cardinal;
    flags: Cardinal;
    field: v4l2_field;
    timestamp: timeval;
    timecode: v4l2_timecode;
    sequence: Cardinal;
    { memory location }
    memory: v4l2_memory;
    case Integer of
      0: (offset: Cardinal);
      1: (userptr: PtrUInt;
          length: Cardinal;
          input: Cardinal;
          reserved: Cardinal)
  end;

const
{ Flags for 'flags' field }
  V4L2_BUF_FLAG_MAPPED = $0001; { Buffer is mapped (flag) }
  V4L2_BUF_FLAG_QUEUED = $0002; { Buffer is queued for processing }
  V4L2_BUF_FLAG_DONE = $0004; { Buffer is ready }
  V4L2_BUF_FLAG_KEYFRAME = $0008; { Image is a keyframe (I-frame) }
  V4L2_BUF_FLAG_PFRAME = $0010; { Image is a P-frame }
  V4L2_BUF_FLAG_BFRAME = $0020; { Image is a B-frame }
  V4L2_BUF_FLAG_ERROR = $0040; { Buffer is ready, but the data contained within is corrupted. }
  V4L2_BUF_FLAG_TIMECODE = $0100; { timecode field is valid }
  V4L2_BUF_FLAG_INPUT = $0200; { input field is valid }

{ Overlay Preview}
type
  Pv4l2_framebuffer = ^v4l2_framebuffer;
  v4l2_framebuffer = record
    capability: Cardinal;
    flags: Cardinal;
    { FIXME: in theory we should pass something like PCI device + memory region + offset instead of some physical
      address }
    base: Pointer;
    fmt: v4l2_pix_format;
  end;

const
{ Flags for the 'capability' field. Read only }
  V4L2_FBUF_CAP_EXTERNOVERLAY = $0001;
  V4L2_FBUF_CAP_CHROMAKEY = $0002;
  V4L2_FBUF_CAP_LIST_CLIPPING = $0004;
  V4L2_FBUF_CAP_BITMAP_CLIPPING = $0008;
  V4L2_FBUF_CAP_LOCAL_ALPHA = $0010;
  V4L2_FBUF_CAP_GLOBAL_ALPHA = $0020;
  V4L2_FBUF_CAP_LOCAL_INV_ALPHA = $0040;
  V4L2_FBUF_CAP_SRC_CHROMAKEY = $0080;
{ Flags for the 'flags' field. }
  V4L2_FBUF_FLAG_PRIMARY = $0001;
  V4L2_FBUF_FLAG_OVERLAY = $0002;
  V4L2_FBUF_FLAG_CHROMAKEY = $0004;
  V4L2_FBUF_FLAG_LOCAL_ALPHA = $0008;
  V4L2_FBUF_FLAG_GLOBAL_ALPHA = $0010;
  V4L2_FBUF_FLAG_LOCAL_INV_ALPHA = $0020;
  V4L2_FBUF_FLAG_SRC_CHROMAKEY = $0040;

type
  Pv4l2_clip = ^v4l2_clip;
  v4l2_clip = record
    c: v4l2_rect;
    next: Pv4l2_clip;
  end;

  Pv4l2_window = ^v4l2_window;
  v4l2_window = record
    w: v4l2_rect;
    field: v4l2_field;
    chromakey: Cardinal;
    clips: Pv4l2_clip;
    clipcount: Cardinal;
    bitmap: Pointer;
    global_alpha: Byte;
  end;

{ Capture Parameters }
type
  Pv4l2_captureparm = ^v4l2_captureparm;
  v4l2_captureparm = record
    capability: Cardinal; { Supported modes }
    capturemode: Cardinal; { Current mode }
    timeperframe: v4l2_fract; { Time per frame in .1us units }
    extendedmode: Cardinal; { Driver-specific extensions }
    readbuffers: Cardinal; { # of buffers for read }
    reserved: array[0..3] of Cardinal;
  end;

const
{ Flags for 'capability' and 'capturemode' fields }
  V4L2_MODE_HIGHQUALITY = $0001; { High quality imaging mode }
  V4L2_CAP_TIMEPERFRAME = $1000; { timeperframe field is supported }

type
  Pv4l2_outputparm = ^v4l2_outputparm;
  v4l2_outputparm = record
    capability: Cardinal; { Supported modes }
    outputmode: Cardinal; { Current mode }
    timeperframe: v4l2_fract; { Time per frame in seconds }
    extendedmode: Cardinal; { Driver-specific extensions }
    writebuffers: Cardinal; { # of buffers for write }
    reserved: array[0..3] of Cardinal;
  end;

{ Input Image Cropping}
  Pv4l2_cropcap = ^v4l2_cropcap;
  v4l2_cropcap = record
    &type: v4l2_buf_type;
    bounds: v4l2_rect;
    defrect: v4l2_rect;
    pixelaspect: v4l2_fract;
  end;

  Pv4l2_crop = ^v4l2_crop;
  v4l2_crop = record
    &type: v4l2_buf_type;
    c: v4l2_rect;
  end;

{ Analog Video Standard }
  v4l2_std_id = UInt64;

const
{ one bit for each }
  V4L2_STD_PAL_B = $00000001;
  V4L2_STD_PAL_B1 = $00000002;
  V4L2_STD_PAL_G = $00000004;
  V4L2_STD_PAL_H = $00000008;
  V4L2_STD_PAL_I = $00000010;
  V4L2_STD_PAL_D = $00000020;
  V4L2_STD_PAL_D1 = $00000040;
  V4L2_STD_PAL_K = $00000080;

  V4L2_STD_PAL_M = $00000100;
  V4L2_STD_PAL_N = $00000200;
  V4L2_STD_PAL_Nc = $00000400;
  V4L2_STD_PAL_60 = $00000800;

  V4L2_STD_NTSC_M = $00001000;
  V4L2_STD_NTSC_M_JP = $00002000;
  V4L2_STD_NTSC_443 = $00004000;
  V4L2_STD_NTSC_M_KR = $00008000;

  V4L2_STD_SECAM_B = $00010000;
  V4L2_STD_SECAM_D = $00020000;
  V4L2_STD_SECAM_G = $00040000;
  V4L2_STD_SECAM_H = $00080000;
  V4L2_STD_SECAM_K = $00100000;
  V4L2_STD_SECAM_K1 = $00200000;
  V4L2_STD_SECAM_L = $00400000;
  V4L2_STD_SECAM_LC = $00800000;

  { ATSC/HDTV }
  V4L2_STD_ATSC_8_VSB = $01000000;
  V4L2_STD_ATSC_16_VSB = $02000000;

  { some common needed stuff }
  V4L2_STD_PAL_BG = V4L2_STD_PAL_B or V4L2_STD_PAL_B1 or V4L2_STD_PAL_G;
  V4L2_STD_PAL_DK = V4L2_STD_PAL_D or V4L2_STD_PAL_D1 or V4L2_STD_PAL_K;
  V4L2_STD_PAL = V4L2_STD_PAL_BG or V4L2_STD_PAL_DK or V4L2_STD_PAL_H or V4L2_STD_PAL_I;
  V4L2_STD_NTSC = V4L2_STD_NTSC_M or V4L2_STD_NTSC_M_JP or V4L2_STD_NTSC_M_KR;
  V4L2_STD_SECAM_DK = V4L2_STD_SECAM_D or V4L2_STD_SECAM_K or V4L2_STD_SECAM_K1;
  V4L2_STD_SECAM = V4L2_STD_SECAM_B or V4L2_STD_SECAM_G or V4L2_STD_SECAM_H or V4L2_STD_SECAM_DK or V4L2_STD_SECAM_L or
    V4L2_STD_SECAM_LC;
  V4L2_STD_525_60 = V4L2_STD_PAL_M or V4L2_STD_PAL_60 or V4L2_STD_NTSC or V4L2_STD_NTSC_443;
  V4L2_STD_625_50 = V4L2_STD_PAL or V4L2_STD_PAL_N or V4L2_STD_PAL_Nc or V4L2_STD_SECAM;
  V4L2_STD_ATSC = V4L2_STD_ATSC_8_VSB or V4L2_STD_ATSC_16_VSB;
  V4L2_STD_UNKNOWN = 0;
  V4L2_STD_ALL = V4L2_STD_525_60 or V4L2_STD_625_50;

  { FIXME: Although std_id is 64 bits, there is an issue on PPC32 architecture that makes switch(__u64) to break. So,
    there's a hack on v4l2-common.c rounding this value to 32 bits. As, currently, the max value is for
    V4L2_STD_ATSC_16_VSB (30 bits wide), it should work fine. However, if needed to add more than two standards,
    v4l2-common.c should be fixed. }
  { some merged standards }
  V4L2_STD_MN = V4L2_STD_PAL_M or V4L2_STD_PAL_N or V4L2_STD_PAL_Nc or V4L2_STD_NTSC;
  V4L2_STD_B = V4L2_STD_PAL_B or V4L2_STD_PAL_B1 or V4L2_STD_SECAM_B;
  V4L2_STD_GH = V4L2_STD_PAL_G or V4L2_STD_PAL_H or V4L2_STD_SECAM_G or V4L2_STD_SECAM_H;
  V4L2_STD_DK = V4L2_STD_PAL_DK or V4L2_STD_SECAM_DK;

type
  Pv4l2_standard = ^v4l2_standard;
  v4l2_standard = record
    index: Cardinal;
    id: v4l2_std_id;
    name: array[0..23] of AnsiChar;
    frameperiod: v4l2_fract; { Frames, not fields }
    framelines: Cardinal;
    reserved: array[0..3] of Cardinal;
  end;

{ Video Inputs }
  Pv4l2_input = ^v4l2_input;
  v4l2_input = record
    index: Cardinal; { Which input }
    name: array[0..31] of AnsiChar; { Label }
    &type: Cardinal; { Type of input }
    audioset: Cardinal; { Associated audios (bitfield) }
    tuner: Cardinal; { Associated tuner }
    std: v4l2_std_id;
    status: Cardinal;
    capabilities: Cardinal;
    reserved: array[0..3] of Cardinal;
  end;

const
{ Values for the 'type' field }
  V4L2_INPUT_TYPE_TUNER = 1;
  V4L2_INPUT_TYPE_CAMERA = 2;

{ field 'status' - general }
  V4L2_IN_ST_NO_POWER = $00000001; { Attached device is off }
  V4L2_IN_ST_NO_SIGNAL = $00000002;
  V4L2_IN_ST_NO_COLOR = $00000004;

{ field 'status' - sensor orientation }
{ If sensor is mounted upside down set both bits }
  V4L2_IN_ST_HFLIP = $00000010; { Frames are flipped horizontally }
  V4L2_IN_ST_VFLIP = $00000020; { Frames are flipped vertically }

{ field 'status' - analog }
  V4L2_IN_ST_NO_H_LOCK = $00000100; { No horizontal sync lock }
  V4L2_IN_ST_COLOR_KILL = $00000200; { Color killer is active }

{ field 'status' - digital }
  V4L2_IN_ST_NO_SYNC = $00010000; { No synchronization lock }
  V4L2_IN_ST_NO_EQU = $00020000; { No equalizer lock }
  V4L2_IN_ST_NO_CARRIER = $00040000; { Carrier recovery failed }

{ field 'status' - VCR and set-top box }
  V4L2_IN_ST_MACROVISION = $01000000; { Macrovision detected }
  V4L2_IN_ST_NO_ACCESS = $02000000; { Conditional access denied }
  V4L2_IN_ST_VTR = $04000000; { VTR time constant }

{ Video Outputs }
type
  Pv4l2_output = ^v4l2_output;
  v4l2_output = record
    index: Cardinal; { Which output }
    name: array[0..31] of AnsiChar; { Label }
    &type: Cardinal; { Type of output }
    audioset: Cardinal; { Associated audios (bitfield) }
    modulator: Cardinal; { Associated modulator }
    std: v4l2_std_id;
    capabilities: Cardinal;
    reserved: array[0..3] of Cardinal;
  end;

const
{ Values for the 'type' field }
  V4L2_OUTPUT_TYPE_MODULATOR = 1;
  V4L2_OUTPUT_TYPE_ANALOG = 2;
  V4L2_OUTPUT_TYPE_ANALOGVGAOVERLAY = 3;

{ Controls }
type
  Pv4l2_control = ^v4l2_control;
  v4l2_control = record
    id: Cardinal;
    value: Integer;
  end;

  Pv4l2_ext_control = ^v4l2_ext_control;
  v4l2_ext_control = packed record
    id: Cardinal;
    reserved2: array[0..1] of Cardinal;
    case Integer of
      0: (value: Integer);
      1: (value64: Int64);
      2: (reserved: Pointer);
  end;

  Pv4l2_ext_controls = ^v4l2_ext_controls;
  v4l2_ext_controls = record
    ctrl_class: Cardinal;
    count: Cardinal;
    error_idx: Cardinal;
    reserved: array[0..1] of Cardinal;
    controls: Pv4l2_ext_control;
  end;

const
{ Values for ctrl_class field }
  V4L2_CTRL_CLASS_USER = $00980000; { Old-style 'user' controls }
  V4L2_CTRL_CLASS_MPEG = $00990000; { MPEG-compression controls }
  V4L2_CTRL_CLASS_CAMERA = $009A0000; { Camera class controls }

  V4L2_CTRL_ID_MASK = $0FFFFFFF;

function V4L2_CTRL_ID2CLASS(const Id: PtrUInt): PtrUInt; inline;
function V4L2_CTRL_DRIVER_PRIV(const Id: PtrUInt): Boolean; inline;

type
{ Used in the VIDIOC_QUERYCTRL ioctl for querying controls }
  Pv4l2_queryctrl = ^v4l2_queryctrl;
  v4l2_queryctrl = record
    id: Cardinal;
    &type: v4l2_ctrl_type;
    name: array[0..31] of AnsiChar; { Whatever }
    minimum: Integer; { Note signedness }
    maximum: Integer;
    step: Integer;
    default_value: Integer;
    flags: Cardinal;
    reserved: array[0..1] of Cardinal;
  end;

{ Used in the VIDIOC_QUERYMENU ioctl for querying menu items }
  Pv4l2_querymenu = ^v4l2_querymenu;
  v4l2_querymenu = record
    id: Cardinal;
    index: Cardinal;
    name: array[0..31] of AnsiChar;
    reserved: Cardinal;
  end;

const
{ Control flags }
  V4L2_CTRL_FLAG_DISABLED = $0001;
  V4L2_CTRL_FLAG_GRABBED = $0002;
  V4L2_CTRL_FLAG_READ_ONLY = $0004;
  V4L2_CTRL_FLAG_UPDATE = $0008;
  V4L2_CTRL_FLAG_INACTIVE = $0010;
  V4L2_CTRL_FLAG_SLIDER = $0020;
  V4L2_CTRL_FLAG_WRITE_ONLY = $0040;

{ Query flag, to be ORed with the control ID }
  V4L2_CTRL_FLAG_NEXT_CTRL = $80000000;

{ User-class control IDs defined by V4L2 }
  V4L2_CID_MAX_CTRLS = 1024;
  V4L2_CID_BASE = V4L2_CTRL_CLASS_USER or $900;

{ IDs reserved for driver specific controls }
  V4L2_CID_PRIVATE_BASE = $08000000;

  V4L2_CID_USER_CLASS = V4L2_CTRL_CLASS_USER or 1;
  V4L2_CID_BRIGHTNESS = V4L2_CID_BASE + 0;
  V4L2_CID_CONTRAST = V4L2_CID_BASE + 1;
  V4L2_CID_SATURATION = V4L2_CID_BASE + 2;
  V4L2_CID_HUE = V4L2_CID_BASE + 3;
  V4L2_CID_AUDIO_VOLUME = V4L2_CID_BASE + 5;
  V4L2_CID_AUDIO_BALANCE = V4L2_CID_BASE + 6;
  V4L2_CID_AUDIO_BASS = V4L2_CID_BASE + 7;
  V4L2_CID_AUDIO_TREBLE = V4L2_CID_BASE + 8;
  V4L2_CID_AUDIO_MUTE = V4L2_CID_BASE + 9;
  V4L2_CID_AUDIO_LOUDNESS = V4L2_CID_BASE + 10;
  V4L2_CID_BLACK_LEVEL = V4L2_CID_BASE + 11; { Deprecated }
  V4L2_CID_AUTO_WHITE_BALANCE = V4L2_CID_BASE + 12;
  V4L2_CID_DO_WHITE_BALANCE = V4L2_CID_BASE + 13;
  V4L2_CID_RED_BALANCE = V4L2_CID_BASE + 14;
  V4L2_CID_BLUE_BALANCE = V4L2_CID_BASE + 15;
  V4L2_CID_GAMMA = V4L2_CID_BASE + 16;
  V4L2_CID_WHITENESS = V4L2_CID_GAMMA; { Deprecated }
  V4L2_CID_EXPOSURE = V4L2_CID_BASE + 17;
  V4L2_CID_AUTOGAIN = V4L2_CID_BASE + 18;
  V4L2_CID_GAIN = V4L2_CID_BASE + 19;
  V4L2_CID_HFLIP = V4L2_CID_BASE + 20;
  V4L2_CID_VFLIP = V4L2_CID_BASE + 21;

{ Deprecated; use V4L2_CID_PAN_RESET and V4L2_CID_TILT_RESET }
  V4L2_CID_HCENTER = V4L2_CID_BASE + 22;
  V4L2_CID_VCENTER = V4L2_CID_BASE + 23;

  V4L2_CID_POWER_LINE_FREQUENCY = V4L2_CID_BASE + 24;

type
  Pv4l2_power_line_frequency = ^v4l2_power_line_frequency;
  v4l2_power_line_frequency = Cardinal;

const
  V4L2_CID_POWER_LINE_FREQUENCY_DISABLED = 0;
  V4L2_CID_POWER_LINE_FREQUENCY_50HZ = 1;
  V4L2_CID_POWER_LINE_FREQUENCY_60HZ = 2;

  V4L2_CID_HUE_AUTO = V4L2_CID_BASE + 25;
  V4L2_CID_WHITE_BALANCE_TEMPERATURE = V4L2_CID_BASE + 26;
  V4L2_CID_SHARPNESS = V4L2_CID_BASE + 27;
  V4L2_CID_BACKLIGHT_COMPENSATION = V4L2_CID_BASE + 28;
  V4L2_CID_CHROMA_AGC = V4L2_CID_BASE + 29;
  V4L2_CID_COLOR_KILLER = V4L2_CID_BASE + 30;
  V4L2_CID_COLORFX = V4L2_CID_BASE + 31;

type
  Pv4l2_colorfx = ^v4l2_colorfx;
  v4l2_colorfx = Cardinal;

const
  V4L2_COLORFX_NONE = 0;
  V4L2_COLORFX_BW = 1;
  V4L2_COLORFX_SEPIA = 2;

  V4L2_CID_AUTOBRIGHTNESS = V4L2_CID_BASE + 32;

{ last CID + 1 }
  V4L2_CID_LASTP1 = V4L2_CID_BASE + 33;

{ MPEG-class control IDs defined by V4L2 }
  V4L2_CID_MPEG_BASE = V4L2_CTRL_CLASS_MPEG or $900;
  V4L2_CID_MPEG_CLASS = V4L2_CTRL_CLASS_MPEG or 1;

{ MPEG streams }
  V4L2_CID_MPEG_STREAM_TYPE = V4L2_CID_MPEG_BASE + 0;

type
  Pv4l2_mpeg_stream_type = ^v4l2_mpeg_stream_type;
  v4l2_mpeg_stream_type = Cardinal;

const
  V4L2_MPEG_STREAM_TYPE_MPEG2_PS = 0; { MPEG-2 program stream }
  V4L2_MPEG_STREAM_TYPE_MPEG2_TS = 1; { MPEG-2 transport stream }
  V4L2_MPEG_STREAM_TYPE_MPEG1_SS = 2; { MPEG-1 system stream }
  V4L2_MPEG_STREAM_TYPE_MPEG2_DVD = 3; { MPEG-2 DVD-compatible stream }
  V4L2_MPEG_STREAM_TYPE_MPEG1_VCD = 4; { MPEG-1 VCD-compatible stream }
  V4L2_MPEG_STREAM_TYPE_MPEG2_SVCD = 5; { MPEG-2 SVCD-compatible stream }

  V4L2_CID_MPEG_STREAM_PID_PMT = V4L2_CID_MPEG_BASE + 1;
  V4L2_CID_MPEG_STREAM_PID_AUDIO = V4L2_CID_MPEG_BASE + 2;
  V4L2_CID_MPEG_STREAM_PID_VIDEO = V4L2_CID_MPEG_BASE + 3;
  V4L2_CID_MPEG_STREAM_PID_PCR = V4L2_CID_MPEG_BASE + 4;
  V4L2_CID_MPEG_STREAM_PES_ID_AUDIO = V4L2_CID_MPEG_BASE + 5;
  V4L2_CID_MPEG_STREAM_PES_ID_VIDEO = V4L2_CID_MPEG_BASE + 6;
  V4L2_CID_MPEG_STREAM_VBI_FMT = V4L2_CID_MPEG_BASE + 7;

type
  Pv4l2_mpeg_stream_vbi_fmt = ^v4l2_mpeg_stream_vbi_fmt;
  v4l2_mpeg_stream_vbi_fmt = Cardinal;

const
  V4L2_MPEG_STREAM_VBI_FMT_NONE = 0; { No VBI in the MPEG stream }
  V4L2_MPEG_STREAM_VBI_FMT_IVTV = 1; { VBI in private packets, IVTV format }

{ MPEG audio }
  V4L2_CID_MPEG_AUDIO_SAMPLING_FREQ = V4L2_CID_MPEG_BASE + 100;

type
  Pv4l2_mpeg_audio_sampling_freq = ^v4l2_mpeg_audio_sampling_freq;
  v4l2_mpeg_audio_sampling_freq = Cardinal;

const
  V4L2_MPEG_AUDIO_SAMPLING_FREQ_44100 = 0;
  V4L2_MPEG_AUDIO_SAMPLING_FREQ_48000 = 1;
  V4L2_MPEG_AUDIO_SAMPLING_FREQ_32000 = 2;

  V4L2_CID_MPEG_AUDIO_ENCODING = V4L2_CID_MPEG_BASE + 101;

type
  Pv4l2_mpeg_audio_encoding = ^v4l2_mpeg_audio_encoding;
  v4l2_mpeg_audio_encoding = Cardinal;

const
  V4L2_MPEG_AUDIO_ENCODING_LAYER_1 = 0;
  V4L2_MPEG_AUDIO_ENCODING_LAYER_2 = 1;
  V4L2_MPEG_AUDIO_ENCODING_LAYER_3 = 2;
  V4L2_MPEG_AUDIO_ENCODING_AAC = 3;
  V4L2_MPEG_AUDIO_ENCODING_AC3 = 4;

  V4L2_CID_MPEG_AUDIO_L1_BITRATE = V4L2_CID_MPEG_BASE + 102;

type
  Pv4l2_mpeg_audio_l1_bitrate = ^v4l2_mpeg_audio_l1_bitrate;
  v4l2_mpeg_audio_l1_bitrate = Cardinal;

const
  V4L2_MPEG_AUDIO_L1_BITRATE_32K = 0;
  V4L2_MPEG_AUDIO_L1_BITRATE_64K = 1;
  V4L2_MPEG_AUDIO_L1_BITRATE_96K = 2;
  V4L2_MPEG_AUDIO_L1_BITRATE_128K = 3;
  V4L2_MPEG_AUDIO_L1_BITRATE_160K = 4;
  V4L2_MPEG_AUDIO_L1_BITRATE_192K = 5;
  V4L2_MPEG_AUDIO_L1_BITRATE_224K = 6;
  V4L2_MPEG_AUDIO_L1_BITRATE_256K = 7;
  V4L2_MPEG_AUDIO_L1_BITRATE_288K = 8;
  V4L2_MPEG_AUDIO_L1_BITRATE_320K = 9;
  V4L2_MPEG_AUDIO_L1_BITRATE_352K = 10;
  V4L2_MPEG_AUDIO_L1_BITRATE_384K = 11;
  V4L2_MPEG_AUDIO_L1_BITRATE_416K = 12;
  V4L2_MPEG_AUDIO_L1_BITRATE_448K = 13;

  V4L2_CID_MPEG_AUDIO_L2_BITRATE = V4L2_CID_MPEG_BASE + 103;

type
  Pv4l2_mpeg_audio_l2_bitrate = ^v4l2_mpeg_audio_l2_bitrate;
  v4l2_mpeg_audio_l2_bitrate = Cardinal;

const
  V4L2_MPEG_AUDIO_L2_BITRATE_32K = 0;
  V4L2_MPEG_AUDIO_L2_BITRATE_48K = 1;
  V4L2_MPEG_AUDIO_L2_BITRATE_56K = 2;
  V4L2_MPEG_AUDIO_L2_BITRATE_64K = 3;
  V4L2_MPEG_AUDIO_L2_BITRATE_80K = 4;
  V4L2_MPEG_AUDIO_L2_BITRATE_96K = 5;
  V4L2_MPEG_AUDIO_L2_BITRATE_112K = 6;
  V4L2_MPEG_AUDIO_L2_BITRATE_128K = 7;
  V4L2_MPEG_AUDIO_L2_BITRATE_160K = 8;
  V4L2_MPEG_AUDIO_L2_BITRATE_192K = 9;
  V4L2_MPEG_AUDIO_L2_BITRATE_224K = 10;
  V4L2_MPEG_AUDIO_L2_BITRATE_256K = 11;
  V4L2_MPEG_AUDIO_L2_BITRATE_320K = 12;
  V4L2_MPEG_AUDIO_L2_BITRATE_384K = 13;

  V4L2_CID_MPEG_AUDIO_L3_BITRATE = V4L2_CID_MPEG_BASE + 104;

type
  Pv4l2_mpeg_audio_l3_bitrate = ^v4l2_mpeg_audio_l3_bitrate;
  v4l2_mpeg_audio_l3_bitrate = Cardinal;

const
  V4L2_MPEG_AUDIO_L3_BITRATE_32K = 0;
  V4L2_MPEG_AUDIO_L3_BITRATE_40K = 1;
  V4L2_MPEG_AUDIO_L3_BITRATE_48K = 2;
  V4L2_MPEG_AUDIO_L3_BITRATE_56K = 3;
  V4L2_MPEG_AUDIO_L3_BITRATE_64K = 4;
  V4L2_MPEG_AUDIO_L3_BITRATE_80K = 5;
  V4L2_MPEG_AUDIO_L3_BITRATE_96K = 6;
  V4L2_MPEG_AUDIO_L3_BITRATE_112K = 7;
  V4L2_MPEG_AUDIO_L3_BITRATE_128K = 8;
  V4L2_MPEG_AUDIO_L3_BITRATE_160K = 9;
  V4L2_MPEG_AUDIO_L3_BITRATE_192K = 10;
  V4L2_MPEG_AUDIO_L3_BITRATE_224K = 11;
  V4L2_MPEG_AUDIO_L3_BITRATE_256K = 12;
  V4L2_MPEG_AUDIO_L3_BITRATE_320K = 13;

  V4L2_CID_MPEG_AUDIO_MODE = V4L2_CID_MPEG_BASE + 105;

type
  Pv4l2_mpeg_audio_mode = ^v4l2_mpeg_audio_mode;
  v4l2_mpeg_audio_mode = Cardinal;

const
  V4L2_MPEG_AUDIO_MODE_STEREO = 0;
  V4L2_MPEG_AUDIO_MODE_JOINT_STEREO = 1;
  V4L2_MPEG_AUDIO_MODE_DUAL = 2;
  V4L2_MPEG_AUDIO_MODE_MONO = 3;

  V4L2_CID_MPEG_AUDIO_MODE_EXTENSION = V4L2_CID_MPEG_BASE + 106;

type
  Pv4l2_mpeg_audio_mode_extension = ^v4l2_mpeg_audio_mode_extension;
  v4l2_mpeg_audio_mode_extension = Cardinal;

const
  V4L2_MPEG_AUDIO_MODE_EXTENSION_BOUND_4 = 0;
  V4L2_MPEG_AUDIO_MODE_EXTENSION_BOUND_8 = 1;
  V4L2_MPEG_AUDIO_MODE_EXTENSION_BOUND_12 = 2;
  V4L2_MPEG_AUDIO_MODE_EXTENSION_BOUND_16 = 3;

  V4L2_CID_MPEG_AUDIO_EMPHASIS = V4L2_CID_MPEG_BASE + 107;

type
  Pv4l2_mpeg_audio_emphasis = ^v4l2_mpeg_audio_emphasis;
  v4l2_mpeg_audio_emphasis = Cardinal;

const
  V4L2_MPEG_AUDIO_EMPHASIS_NONE = 0;
  V4L2_MPEG_AUDIO_EMPHASIS_50_DIV_15_uS = 1;
  V4L2_MPEG_AUDIO_EMPHASIS_CCITT_J17 = 2;

  V4L2_CID_MPEG_AUDIO_CRC = V4L2_CID_MPEG_BASE + 108;

type
  Pv4l2_mpeg_audio_crc = ^v4l2_mpeg_audio_crc;
  v4l2_mpeg_audio_crc = Cardinal;

const
  V4L2_MPEG_AUDIO_CRC_NONE = 0;
  V4L2_MPEG_AUDIO_CRC_CRC16 = 1;

  V4L2_CID_MPEG_AUDIO_MUTE = V4L2_CID_MPEG_BASE + 109;
  V4L2_CID_MPEG_AUDIO_AAC_BITRATE = V4L2_CID_MPEG_BASE + 110;
  V4L2_CID_MPEG_AUDIO_AC3_BITRATE = V4L2_CID_MPEG_BASE + 111;

type
  Pv4l2_mpeg_audio_ac3_bitrate = ^v4l2_mpeg_audio_ac3_bitrate;
  v4l2_mpeg_audio_ac3_bitrate = Cardinal;

const
  V4L2_MPEG_AUDIO_AC3_BITRATE_32K = 0;
  V4L2_MPEG_AUDIO_AC3_BITRATE_40K = 1;
  V4L2_MPEG_AUDIO_AC3_BITRATE_48K = 2;
  V4L2_MPEG_AUDIO_AC3_BITRATE_56K = 3;
  V4L2_MPEG_AUDIO_AC3_BITRATE_64K = 4;
  V4L2_MPEG_AUDIO_AC3_BITRATE_80K = 5;
  V4L2_MPEG_AUDIO_AC3_BITRATE_96K = 6;
  V4L2_MPEG_AUDIO_AC3_BITRATE_112K = 7;
  V4L2_MPEG_AUDIO_AC3_BITRATE_128K = 8;
  V4L2_MPEG_AUDIO_AC3_BITRATE_160K = 9;
  V4L2_MPEG_AUDIO_AC3_BITRATE_192K = 10;
  V4L2_MPEG_AUDIO_AC3_BITRATE_224K = 11;
  V4L2_MPEG_AUDIO_AC3_BITRATE_256K = 12;
  V4L2_MPEG_AUDIO_AC3_BITRATE_320K = 13;
  V4L2_MPEG_AUDIO_AC3_BITRATE_384K = 14;
  V4L2_MPEG_AUDIO_AC3_BITRATE_448K = 15;
  V4L2_MPEG_AUDIO_AC3_BITRATE_512K = 16;
  V4L2_MPEG_AUDIO_AC3_BITRATE_576K = 17;
  V4L2_MPEG_AUDIO_AC3_BITRATE_640K = 18;

{ MPEG video }
  V4L2_CID_MPEG_VIDEO_ENCODING = V4L2_CID_MPEG_BASE + 200;

type
  Pv4l2_mpeg_video_encoding = ^v4l2_mpeg_video_encoding;
  v4l2_mpeg_video_encoding = Cardinal;

const
  V4L2_MPEG_VIDEO_ENCODING_MPEG_1 = 0;
  V4L2_MPEG_VIDEO_ENCODING_MPEG_2 = 1;
  V4L2_MPEG_VIDEO_ENCODING_MPEG_4_AVC = 2;

  V4L2_CID_MPEG_VIDEO_ASPECT = V4L2_CID_MPEG_BASE + 201;

type
  Pv4l2_mpeg_video_aspect = ^v4l2_mpeg_video_aspect;
  v4l2_mpeg_video_aspect = Cardinal;

const
  V4L2_MPEG_VIDEO_ASPECT_1x1 = 0;
  V4L2_MPEG_VIDEO_ASPECT_4x3 = 1;
  V4L2_MPEG_VIDEO_ASPECT_16x9 = 2;
  V4L2_MPEG_VIDEO_ASPECT_221x100 = 3;

  V4L2_CID_MPEG_VIDEO_B_FRAMES = V4L2_CID_MPEG_BASE + 202;
  V4L2_CID_MPEG_VIDEO_GOP_SIZE = V4L2_CID_MPEG_BASE + 203;
  V4L2_CID_MPEG_VIDEO_GOP_CLOSURE = V4L2_CID_MPEG_BASE + 204;
  V4L2_CID_MPEG_VIDEO_PULLDOWN = V4L2_CID_MPEG_BASE + 205;
  V4L2_CID_MPEG_VIDEO_BITRATE_MODE = V4L2_CID_MPEG_BASE + 206;

type
  Pv4l2_mpeg_video_bitrate_mode = ^v4l2_mpeg_video_bitrate_mode;
  v4l2_mpeg_video_bitrate_mode = Cardinal;

const
  V4L2_MPEG_VIDEO_BITRATE_MODE_VBR = 0;
  V4L2_MPEG_VIDEO_BITRATE_MODE_CBR = 1;

  V4L2_CID_MPEG_VIDEO_BITRATE = V4L2_CID_MPEG_BASE + 207;
  V4L2_CID_MPEG_VIDEO_BITRATE_PEAK = V4L2_CID_MPEG_BASE + 208;
  V4L2_CID_MPEG_VIDEO_TEMPORAL_DECIMATION = V4L2_CID_MPEG_BASE + 209;
  V4L2_CID_MPEG_VIDEO_MUTE = V4L2_CID_MPEG_BASE + 210;
  V4L2_CID_MPEG_VIDEO_MUTE_YUV = V4L2_CID_MPEG_BASE + 211;

{ MPEG-class control IDs specific to the CX2341x driver as defined by V4L2 }
  V4L2_CID_MPEG_CX2341X_BASE = V4L2_CTRL_CLASS_MPEG or $1000;
  V4L2_CID_MPEG_CX2341X_VIDEO_SPATIAL_FILTER_MODE = V4L2_CID_MPEG_CX2341X_BASE + 0;

type
  Pv4l2_mpeg_cx2341x_video_spatial_filter_mode = ^v4l2_mpeg_cx2341x_video_spatial_filter_mode;
  v4l2_mpeg_cx2341x_video_spatial_filter_mode = Cardinal;

const
  V4L2_MPEG_CX2341X_VIDEO_SPATIAL_FILTER_MODE_MANUAL = 0;
  V4L2_MPEG_CX2341X_VIDEO_SPATIAL_FILTER_MODE_AUTO = 1;

  V4L2_CID_MPEG_CX2341X_VIDEO_SPATIAL_FILTER = V4L2_CID_MPEG_CX2341X_BASE + 1;
  V4L2_CID_MPEG_CX2341X_VIDEO_LUMA_SPATIAL_FILTER_TYPE = V4L2_CID_MPEG_CX2341X_BASE + 2;

type
  Pv4l2_mpeg_cx2341x_video_luma_spatial_filter_type = ^v4l2_mpeg_cx2341x_video_luma_spatial_filter_type;
  v4l2_mpeg_cx2341x_video_luma_spatial_filter_type = Cardinal;

const
  V4L2_MPEG_CX2341X_VIDEO_LUMA_SPATIAL_FILTER_TYPE_OFF = 0;
  V4L2_MPEG_CX2341X_VIDEO_LUMA_SPATIAL_FILTER_TYPE_1D_HOR = 1;
  V4L2_MPEG_CX2341X_VIDEO_LUMA_SPATIAL_FILTER_TYPE_1D_VERT = 2;
  V4L2_MPEG_CX2341X_VIDEO_LUMA_SPATIAL_FILTER_TYPE_2D_HV_SEPARABLE = 3;
  V4L2_MPEG_CX2341X_VIDEO_LUMA_SPATIAL_FILTER_TYPE_2D_SYM_NON_SEPARABLE = 4;

  V4L2_CID_MPEG_CX2341X_VIDEO_CHROMA_SPATIAL_FILTER_TYPE = V4L2_CID_MPEG_CX2341X_BASE + 3;

type
  Pv4l2_mpeg_cx2341x_video_chroma_spatial_filter_type = ^v4l2_mpeg_cx2341x_video_chroma_spatial_filter_type;
  v4l2_mpeg_cx2341x_video_chroma_spatial_filter_type = Cardinal;

const
  V4L2_MPEG_CX2341X_VIDEO_CHROMA_SPATIAL_FILTER_TYPE_OFF = 0;
  V4L2_MPEG_CX2341X_VIDEO_CHROMA_SPATIAL_FILTER_TYPE_1D_HOR = 1;

  V4L2_CID_MPEG_CX2341X_VIDEO_TEMPORAL_FILTER_MODE = V4L2_CID_MPEG_CX2341X_BASE + 4;

type
  Pv4l2_mpeg_cx2341x_video_temporal_filter_mode = ^v4l2_mpeg_cx2341x_video_temporal_filter_mode;
  v4l2_mpeg_cx2341x_video_temporal_filter_mode = Cardinal;

const
  V4L2_MPEG_CX2341X_VIDEO_TEMPORAL_FILTER_MODE_MANUAL = 0;
  V4L2_MPEG_CX2341X_VIDEO_TEMPORAL_FILTER_MODE_AUTO = 1;

  V4L2_CID_MPEG_CX2341X_VIDEO_TEMPORAL_FILTER = V4L2_CID_MPEG_CX2341X_BASE + 5;
  V4L2_CID_MPEG_CX2341X_VIDEO_MEDIAN_FILTER_TYPE = V4L2_CID_MPEG_CX2341X_BASE + 6;

type
  Pv4l2_mpeg_cx2341x_video_median_filter_type = ^v4l2_mpeg_cx2341x_video_median_filter_type;
  v4l2_mpeg_cx2341x_video_median_filter_type = Cardinal;

const
  V4L2_MPEG_CX2341X_VIDEO_MEDIAN_FILTER_TYPE_OFF = 0;
  V4L2_MPEG_CX2341X_VIDEO_MEDIAN_FILTER_TYPE_HOR = 1;
  V4L2_MPEG_CX2341X_VIDEO_MEDIAN_FILTER_TYPE_VERT = 2;
  V4L2_MPEG_CX2341X_VIDEO_MEDIAN_FILTER_TYPE_HOR_VERT = 3;
  V4L2_MPEG_CX2341X_VIDEO_MEDIAN_FILTER_TYPE_DIAG = 4;

  V4L2_CID_MPEG_CX2341X_VIDEO_LUMA_MEDIAN_FILTER_BOTTOM = V4L2_CID_MPEG_CX2341X_BASE + 7;
  V4L2_CID_MPEG_CX2341X_VIDEO_LUMA_MEDIAN_FILTER_TOP = V4L2_CID_MPEG_CX2341X_BASE + 8;
  V4L2_CID_MPEG_CX2341X_VIDEO_CHROMA_MEDIAN_FILTER_BOTTOM = V4L2_CID_MPEG_CX2341X_BASE + 9;
  V4L2_CID_MPEG_CX2341X_VIDEO_CHROMA_MEDIAN_FILTER_TOP = V4L2_CID_MPEG_CX2341X_BASE + 10;
  V4L2_CID_MPEG_CX2341X_STREAM_INSERT_NAV_PACKETS = V4L2_CID_MPEG_CX2341X_BASE + 11;

{ Camera class control IDs }
  V4L2_CID_CAMERA_CLASS_BASE = V4L2_CTRL_CLASS_CAMERA or $900;
  V4L2_CID_CAMERA_CLASS = V4L2_CTRL_CLASS_CAMERA or 1;

  V4L2_CID_EXPOSURE_AUTO = V4L2_CID_CAMERA_CLASS_BASE + 1;

type
  Pv4l2_exposure_auto_type = ^v4l2_exposure_auto_type;
  v4l2_exposure_auto_type = Cardinal;

const
  V4L2_EXPOSURE_AUTO = 0;
  V4L2_EXPOSURE_MANUAL = 1;
  V4L2_EXPOSURE_SHUTTER_PRIORITY = 2;
  V4L2_EXPOSURE_APERTURE_PRIORITY = 3;

  V4L2_CID_EXPOSURE_ABSOLUTE = V4L2_CID_CAMERA_CLASS_BASE + 2;
  V4L2_CID_EXPOSURE_AUTO_PRIORITY = V4L2_CID_CAMERA_CLASS_BASE + 3;

  V4L2_CID_PAN_RELATIVE = V4L2_CID_CAMERA_CLASS_BASE + 4;
  V4L2_CID_TILT_RELATIVE = V4L2_CID_CAMERA_CLASS_BASE + 5;
  V4L2_CID_PAN_RESET = V4L2_CID_CAMERA_CLASS_BASE + 6;
  V4L2_CID_TILT_RESET = V4L2_CID_CAMERA_CLASS_BASE + 7;

  V4L2_CID_PAN_ABSOLUTE = V4L2_CID_CAMERA_CLASS_BASE + 8;
  V4L2_CID_TILT_ABSOLUTE = V4L2_CID_CAMERA_CLASS_BASE + 9;

  V4L2_CID_FOCUS_ABSOLUTE = V4L2_CID_CAMERA_CLASS_BASE + 10;
  V4L2_CID_FOCUS_RELATIVE = V4L2_CID_CAMERA_CLASS_BASE + 11;
  V4L2_CID_FOCUS_AUTO = V4L2_CID_CAMERA_CLASS_BASE + 12;

  V4L2_CID_ZOOM_ABSOLUTE = V4L2_CID_CAMERA_CLASS_BASE + 13;
  V4L2_CID_ZOOM_RELATIVE = V4L2_CID_CAMERA_CLASS_BASE + 14;
  V4L2_CID_ZOOM_CONTINUOUS = V4L2_CID_CAMERA_CLASS_BASE + 15;

  V4L2_CID_PRIVACY = V4L2_CID_CAMERA_CLASS_BASE + 16;

{ Tuning }
type
  Pv4l2_tuner = ^v4l2_tuner;
  v4l2_tuner = record
    index: Cardinal;
    name: array[0..31] of AnsiChar;
    &type: v4l2_tuner_type;
    capability: Cardinal;
    rangelow: Cardinal;
    rangehigh: Cardinal;
    rxsubchans: Cardinal;
    audmode: Cardinal;
    signal: Integer;
    afc: Integer;
    reserved: array[0..3] of Cardinal;
  end;

  Pv4l2_modulator = ^v4l2_modulator;
  v4l2_modulator = record
    index: Cardinal;
    name: array[0..31] of AnsiChar;
    capability: Cardinal;
    rangelow: Cardinal;
    rangehigh: Cardinal;
    txsubchans: Cardinal;
    reserved: array[0..3] of Cardinal;
  end;

const
{ Flags for the 'capability' field }
  V4L2_TUNER_CAP_LOW = $0001;
  V4L2_TUNER_CAP_NORM = $0002;
  V4L2_TUNER_CAP_STEREO = $0010;
  V4L2_TUNER_CAP_LANG2 = $0020;
  V4L2_TUNER_CAP_SAP = $0020;
  V4L2_TUNER_CAP_LANG1 = $0040;
  V4L2_TUNER_CAP_RDS = $0080;
  V4L2_TUNER_CAP_RDS_BLOCK_IO = $0100;
  V4L2_TUNER_CAP_RDS_CONTROLS = $0200;

{ Flags for the 'rxsubchans' field }
  V4L2_TUNER_SUB_MONO = $0001;
  V4L2_TUNER_SUB_STEREO = $0002;
  V4L2_TUNER_SUB_LANG2 = $0004;
  V4L2_TUNER_SUB_SAP = $0004;
  V4L2_TUNER_SUB_LANG1 = $0008;
  V4L2_TUNER_SUB_RDS = $0010;

{ Values for the 'audmode' field }
  V4L2_TUNER_MODE_MONO = $0000;
  V4L2_TUNER_MODE_STEREO = $0001;
  V4L2_TUNER_MODE_LANG2 = $0002;
  V4L2_TUNER_MODE_SAP = $0002;
  V4L2_TUNER_MODE_LANG1 = $0003;
  V4L2_TUNER_MODE_LANG1_LANG2 = $0004;

type
  Pv4l2_frequency = ^v4l2_frequency;
  v4l2_frequency = record
    tuner: Cardinal;
    &type: v4l2_tuner_type;
    frequency: Cardinal;
    reserved: array[0..7] of Cardinal;
  end;

  Pv4l2_hw_freq_seek = ^v4l2_hw_freq_seek;
  v4l2_hw_freq_seek = record
    tuner: Cardinal;
    &type: v4l2_tuner_type;
    seek_upward: Cardinal;
    wrap_around: Cardinal;
    reserved: array[0..7] of Cardinal;
  end;

{ Audio }
  Pv4l2_audio = ^v4l2_audio;
  v4l2_audio = record
    index: Cardinal;
    name: array[0..31] of AnsiChar;
    capability: Cardinal;
    mode: Cardinal;
    reserved: array[0..1] of Cardinal;
  end;

const
{ Flags for the 'capability' field }
  V4L2_AUDCAP_STEREO = $00001;
  V4L2_AUDCAP_AVL = $00002;

{ Flags for the 'mode' field }
  V4L2_AUDMODE_AV = $00001;

type
  Pv4l2_audioout = ^v4l2_audioout;
  v4l2_audioout = record
    index: Cardinal;
    name: array[0..31] of AnsiChar;
    capability: Cardinal;
    mode: Cardinal;
    reserved: array[0..1] of Cardinal;
  end;

{ MPEG Services }
{ NOTE: EXPERIMENTAL API }
const
  V4L2_ENC_IDX_FRAME_I = 0;
  V4L2_ENC_IDX_FRAME_P = 1;
  V4L2_ENC_IDX_FRAME_B = 2;
  V4L2_ENC_IDX_FRAME_MASK= $F;

type
  Pv4l2_enc_idx_entry = ^v4l2_enc_idx_entry;
  v4l2_enc_idx_entry = record
    offset: UInt64;
    pts: UInt64;
    length: Cardinal;
    flags: Cardinal;
    reserved: array[0..1] of Cardinal;
  end;

const
  V4L2_ENC_IDX_ENTRIES = 64;

type
  Pv4l2_enc_idx = ^v4l2_enc_idx;
  v4l2_enc_idx = record
    entries: Cardinal;
    entries_cap: Cardinal;
    reserved: array[0..3] of Cardinal;
    entry: array[0..V4L2_ENC_IDX_ENTRIES - 1] of v4l2_enc_idx_entry;
  end;

const
  V4L2_ENC_CMD_START = 0;
  V4L2_ENC_CMD_STOP = 1;
  V4L2_ENC_CMD_PAUSE = 2;
  V4L2_ENC_CMD_RESUME = 3;

{ Flags for V4L2_ENC_CMD_STOP }
  V4L2_ENC_CMD_STOP_AT_GOP_END = 1 shl 0;

type
  Pv4l2_encoder_cmd = ^v4l2_encoder_cmd;
  v4l2_encoder_cmd = record
    cmd: Cardinal;
    flags: Cardinal;
    data: array[0..7] of Cardinal;
  end;

{ Data Services (VBI)
  Data services API by Michael Schimek }
{ Raw VBI }
  Pv4l2_vbi_format = ^v4l2_vbi_format;
  v4l2_vbi_format = record
    sampling_rate: Cardinal; { in 1 Hz }
    offset: Cardinal;
    samples_per_line: Cardinal;
    sample_format: Cardinal; { V4L2_PIX_FMT_* }
    start: array[0..1] of Integer;
    count: array[0..1] of Cardinal;
    flags: Cardinal; { V4L2_VBI_* }
    reserved: array[0..1] of Cardinal; { must be zero }
  end;

const
{ VBI flags }
  V4L2_VBI_UNSYNC = 1 shl 0;
  V4L2_VBI_INTERLACED = 1 shl 1;

{ Sliced VBI
  This implements is a proposal V4L2 API to allow SLICED VBI required for some hardware encoders. It should change
  without notice in the definitive implementation. }
type
  Pv4l2_sliced_vbi_format = ^v4l2_sliced_vbi_format;
  v4l2_sliced_vbi_format = record
    service_set: Word;
    { service_lines[0][...] specifies lines 0-23 (1-23 used) of the first field
    service_lines[1][...] specifies lines 0-23 (1-23 used) of the second field
    (equals frame lines 313-336 for 625 line video
    standards, 263-286 for 525 line standards) }
    service_lines: array[0..1, 0..23] of Word;
    io_size: Cardinal;
    reserved: array[0..1] of Cardinal; { must be zero }
  end;

const
{ Teletext World System Teletext
(WST), defined on ITU-R BT.653-2 }
  V4L2_SLICED_TELETEXT_B = $0001;
{ Video Program System, defined on ETS 300 231}
  V4L2_SLICED_VPS = $0400;
{ Closed Caption, defined on EIA-608 }
  V4L2_SLICED_CAPTION_525 = $1000;
{ Wide Screen System, defined on ITU-R BT1119.1 }
  V4L2_SLICED_WSS_625 = $4000;
  V4L2_SLICED_VBI_525 = V4L2_SLICED_CAPTION_525;
  V4L2_SLICED_VBI_625 = V4L2_SLICED_TELETEXT_B or V4L2_SLICED_VPS or V4L2_SLICED_WSS_625;

type
  Pv4l2_sliced_vbi_cap = ^v4l2_sliced_vbi_cap;
  v4l2_sliced_vbi_cap = record
    service_set: Word;
    { service_lines[0][...] specifies lines 0-23 (1-23 used) of the first field
    service_lines[1][...] specifies lines 0-23 (1-23 used) of the second field
    (equals frame lines 313-336 for 625 line video
    standards, 263-286 for 525 line standards) }
    service_lines: array[0..1, 0..23] of Word;
    &type: v4l2_buf_type;
    reserved: array[0..2] of Cardinal; { must be 0 }
  end;

  Pv4l2_sliced_vbi_data = ^v4l2_sliced_vbi_data;
  v4l2_sliced_vbi_data = record
    id: Cardinal;
    field: Cardinal; { 0: first field, 1: second field }
    line: Cardinal; { 1-23 }
    reserved: Cardinal; { must be 0 }
    data: array[0..47] of Byte;
  end;

{ Sliced VBI data inserted into MPEG Streams }

{ V4L2_MPEG_STREAM_VBI_FMT_IVTV:
  Structure of payload contained in an MPEG 2 Private Stream 1 PES Packet in an MPEG-2 Program Pack that contains
  V4L2_MPEG_STREAM_VBI_FMT_IVTV Sliced VBI data
  Note, the MPEG-2 Program Pack and Private Stream 1 PES packet header definitions are not included here. See the
  MPEG-2 specifications for details on these headers. }

const
{ Line type IDs }
  V4L2_MPEG_VBI_IVTV_TELETEXT_B = 1;
  V4L2_MPEG_VBI_IVTV_CAPTION_525 = 4;
  V4L2_MPEG_VBI_IVTV_WSS_625 = 5;
  V4L2_MPEG_VBI_IVTV_VPS = 7;

type
  Pv4l2_mpeg_vbi_itv0_line = ^v4l2_mpeg_vbi_itv0_line;
  v4l2_mpeg_vbi_itv0_line = packed record
    id: Byte; { One of V4L2_MPEG_VBI_IVTV_* above }
    data: array[0..41] of Byte; { Sliced VBI data for the line }
  end;

  Pv4l2_mpeg_vbi_itv0 = ^v4l2_mpeg_vbi_itv0;
  v4l2_mpeg_vbi_itv0 = packed record
    linemask: array[0..1] of Cardinal; { Bitmasks of VBI service lines present }
    line: array[0..34] of v4l2_mpeg_vbi_itv0_line;
  end;

  P_v4l2_mpeg_vbi_ITV0 = ^_v4l2_mpeg_vbi_ITV0;
  _v4l2_mpeg_vbi_ITV0 = packed record
    line: array[0..35] of v4l2_mpeg_vbi_itv0_line;
  end;

const
  V4L2_MPEG_VBI_IVTV_MAGIC0 = 'itv0';
  V4L2_MPEG_VBI_IVTV_MAGIC1 = 'ITV0';

type
  Pv4l2_mpeg_vbi_fmt_ivtv = ^v4l2_mpeg_vbi_fmt_ivtv;
  v4l2_mpeg_vbi_fmt_ivtv = packed record
    magic: array[0..3] of AnsiChar;
    case Integer of
      0: (itv0: v4l2_mpeg_vbi_itv0);
      1: (_ITV0: _v4l2_mpeg_vbi_ITV0);
  end;

{ Aggregate Structures }
  Pv4l2_format = ^v4l2_format;
  v4l2_format = record
    &type: v4l2_buf_type;
    case Integer of
      0: (pix: v4l2_pix_format); { V4L2_BUF_TYPE_VIDEO_CAPTURE }
      1: (win: v4l2_window); { V4L2_BUF_TYPE_VIDEO_OVERLAY }
      2: (vbi: v4l2_vbi_format); { V4L2_BUF_TYPE_VBI_CAPTURE }
      3: (sliced: v4l2_sliced_vbi_format); { V4L2_BUF_TYPE_SLICED_VBI_CAPTURE }
      4: (raw_data: array[0..199] of Byte); { user-defined }
  end;

{ Stream type-dependent parameters }
  Pv4l2_streamparm = ^v4l2_streamparm;
  v4l2_streamparm = record
    &type: v4l2_buf_type;
    case Integer of
      0: (capture: v4l2_captureparm);
      1: (output: v4l2_outputparm);
      2: (raw_data: array[0..199] of Byte); { user-defined }
  end;

{ Advanced Debugging }
{ VIDIOC_DBG_G_REGISTER and VIDIOC_DBG_S_REGISTER }
const
  V4L2_CHIP_MATCH_HOST = 0; { Match against chip ID on host (0 for the host) }
  V4L2_CHIP_MATCH_I2C_DRIVER = 1; { Match against I2C driver name }
  V4L2_CHIP_MATCH_I2C_ADDR = 2; { Match against I2C 7-bit address }
  V4L2_CHIP_MATCH_AC97 = 3; { Match against anciliary AC97 chip }

type
  Pv4l2_dbg_match = ^v4l2_dbg_match;
  v4l2_dbg_match = packed record
    &type: Cardinal; { Match type }
    case Integer of { Match this chip, meaning determined by type }
      0: (addr: Cardinal);
      1: (name: array[0..31] of AnsiChar)
  end;

  Pv4l2_dbg_register = ^v4l2_dbg_register;
  v4l2_dbg_register = packed record
    match: v4l2_dbg_match;
    size: Cardinal; { register size in bytes }
    reg: UInt64;
    val: UInt64;
  end;

{ VIDIOC_DBG_G_CHIP_IDENT }
  Pv4l2_dbg_chip_ident = ^v4l2_dbg_chip_ident;
  v4l2_dbg_chip_ident = packed record
    match: v4l2_dbg_match;
    ident: Cardinal; { chip identifier as specified in <media/v4l2-chip-ident.h> }
    revision: Cardinal; { chip revision, chip specific }
  end;

const
  VIDIOC_IOC_MAGIC = Ord('V');

{ ioctl Codes For Video Devices }
function VIDIOC_QUERYCAP: Integer; inline;
function VIDIOC_RESERVED: Integer; inline;
function VIDIOC_ENUM_FMT: Integer; inline;
function VIDIOC_G_FMT: Integer; inline;
function VIDIOC_S_FMT: Integer; inline;
function VIDIOC_REQBUFS: Integer; inline;
function VIDIOC_QUERYBUF: Integer; inline;
function VIDIOC_G_FBUF: Integer; inline;
function VIDIOC_S_FBUF: Integer; inline;
function VIDIOC_OVERLAY: Integer; inline;
function VIDIOC_QBUF: Integer; inline;
function VIDIOC_DQBUF: Integer; inline;
function VIDIOC_STREAMON: Integer; inline;
function VIDIOC_STREAMOFF: Integer; inline;
function VIDIOC_G_PARM: Integer; inline;
function VIDIOC_S_PARM: Integer; inline;
function VIDIOC_G_STD: Integer; inline;
function VIDIOC_S_STD: Integer; inline;
function VIDIOC_ENUMSTD: Integer; inline;
function VIDIOC_ENUMINPUT: Integer; inline;
function VIDIOC_G_CTRL: Integer; inline;
function VIDIOC_S_CTRL: Integer; inline;
function VIDIOC_G_TUNER: Integer; inline;
function VIDIOC_S_TUNER: Integer; inline;
function VIDIOC_G_AUDIO: Integer; inline;
function VIDIOC_S_AUDIO: Integer; inline;
function VIDIOC_QUERYCTRL: Integer; inline;
function VIDIOC_QUERYMENU: Integer; inline;
function VIDIOC_G_INPUT: Integer; inline;
function VIDIOC_S_INPUT: Integer; inline;
function VIDIOC_G_OUTPUT: Integer; inline;
function VIDIOC_S_OUTPUT: Integer; inline;
function VIDIOC_ENUMOUTPUT: Integer; inline;
function VIDIOC_G_AUDOUT: Integer; inline;
function VIDIOC_S_AUDOUT: Integer; inline;
function VIDIOC_G_MODULATOR: Integer; inline;
function VIDIOC_S_MODULATOR: Integer; inline;
function VIDIOC_G_FREQUENCY: Integer; inline;
function VIDIOC_S_FREQUENCY: Integer; inline;
function VIDIOC_CROPCAP: Integer; inline;
function VIDIOC_G_CROP: Integer; inline;
function VIDIOC_S_CROP: Integer; inline;
function VIDIOC_G_JPEGCOMP: Integer; inline;
function VIDIOC_S_JPEGCOMP: Integer; inline;
function VIDIOC_QUERYSTD: Integer; inline;
function VIDIOC_TRY_FMT: Integer; inline;
function VIDIOC_ENUMAUDIO: Integer; inline;
function VIDIOC_ENUMAUDOUT: Integer; inline;
function VIDIOC_G_PRIORITY: Integer; inline;
function VIDIOC_S_PRIORITY: Integer; inline;
function VIDIOC_G_SLICED_VBI_CAP: Integer; inline;
function VIDIOC_LOG_STATUS: Integer; inline;
function VIDIOC_G_EXT_CTRLS: Integer; inline;
function VIDIOC_S_EXT_CTRLS: Integer; inline;
function VIDIOC_TRY_EXT_CTRLS: Integer; inline;
function VIDIOC_ENUM_FRAMESIZES: Integer; inline;
function VIDIOC_ENUM_FRAMEINTERVALS: Integer; inline;
function VIDIOC_G_ENC_INDEX: Integer; inline;
function VIDIOC_ENCODER_CMD: Integer; inline;
function VIDIOC_TRY_ENCODER_CMD: Integer; inline;

{ Experimental, meant for debugging, testing and internal use. Only implemented if CONFIG_VIDEO_ADV_DEBUG is defined.
  You must be root to use these ioctls. Never use these in applications! }
function VIDIOC_DBG_S_REGISTER: Integer; inline;
function VIDIOC_DBG_G_REGISTER: Integer; inline;

{ Experimental, meant for debugging, testing and internal use. Never use this ioctl in applications! }
function VIDIOC_DBG_G_CHIP_IDENT: Integer; inline;
function VIDIOC_S_HW_FREQ_SEEK: Integer; inline;

implementation

uses
  PXL.Linux.ioctl;

function v4l2_fourcc(const a, b, c, d: AnsiChar): Cardinal;
begin
  Result := Cardinal(a) or (Cardinal(b) shl 8) or (Cardinal(c) shl 16) or (Cardinal(d) shl 24);
end;

function V4L2_FIELD_HAS_TOP(const field: v4l2_field): Boolean;
begin
  Result := field in [V4L2_FIELD_TOP, V4L2_FIELD_INTERLACED, V4L2_FIELD_INTERLACED_TB, V4L2_FIELD_INTERLACED_BT,
    V4L2_FIELD_SEQ_TB, V4L2_FIELD_SEQ_BT];
end;

function V4L2_FIELD_HAS_BOTTOM(const field: v4l2_field): Boolean;
begin
  Result := field in [V4L2_FIELD_BOTTOM, V4L2_FIELD_INTERLACED, V4L2_FIELD_INTERLACED_TB, V4L2_FIELD_INTERLACED_BT,
    V4L2_FIELD_SEQ_TB, V4L2_FIELD_SEQ_BT];
end;

function V4L2_FIELD_HAS_BOTH(const field: v4l2_field): Boolean;
begin
  Result := field in [V4L2_FIELD_INTERLACED, V4L2_FIELD_INTERLACED_TB, V4L2_FIELD_INTERLACED_BT, V4L2_FIELD_SEQ_TB,
    V4L2_FIELD_SEQ_BT];
end;

function V4L2_PIX_FMT_RGB332: Cardinal;
begin
  Result := v4l2_fourcc('R', 'G', 'B', '1');
end;

function V4L2_PIX_FMT_RGB444: Cardinal;
begin
  Result := v4l2_fourcc('R', '4', '4', '4');
end;

function V4L2_PIX_FMT_RGB555: Cardinal;
begin
  Result := v4l2_fourcc('R', 'G', 'B', 'O');
end;

function V4L2_PIX_FMT_RGB565: Cardinal;
begin
  Result := v4l2_fourcc('R', 'G', 'B', 'P');
end;

function V4L2_PIX_FMT_RGB555X: Cardinal;
begin
  Result := v4l2_fourcc('R', 'G', 'B', 'Q');
end;

function V4L2_PIX_FMT_RGB565X: Cardinal;
begin
  Result := v4l2_fourcc('R', 'G', 'B', 'R');
end;

function V4L2_PIX_FMT_BGR666: Cardinal;
begin
  Result := v4l2_fourcc('B', 'G', 'R', 'H');
end;

function V4L2_PIX_FMT_BGR24: Cardinal;
begin
  Result := v4l2_fourcc('B', 'G', 'R', '3');
end;

function V4L2_PIX_FMT_RGB24: Cardinal;
begin
  Result := v4l2_fourcc('R', 'G', 'B', '3');
end;

function V4L2_PIX_FMT_BGR32: Cardinal;
begin
  Result := v4l2_fourcc('B', 'G', 'R', '4');
end;

function V4L2_PIX_FMT_RGB32: Cardinal;
begin
  Result := v4l2_fourcc('R', 'G', 'B', '4');
end;

function V4L2_PIX_FMT_GREY: Cardinal;
begin
  Result := v4l2_fourcc('G', 'R', 'E', 'Y');
end;

function V4L2_PIX_FMT_Y4: Cardinal;
begin
  Result := v4l2_fourcc('Y', '0', '4', ' ');
end;

function V4L2_PIX_FMT_Y6: Cardinal;
begin
  Result := v4l2_fourcc('Y', '0', '6', ' ');
end;

function V4L2_PIX_FMT_Y10: Cardinal;
begin
  Result := v4l2_fourcc('Y', '1', '0', ' ');
end;

function V4L2_PIX_FMT_Y12: Cardinal;
begin
  Result := v4l2_fourcc('Y', '1', '2', ' ');
end;

function V4L2_PIX_FMT_Y16: Cardinal;
begin
  Result := v4l2_fourcc('Y', '1', '6', ' ');
end;

function V4L2_PIX_FMT_Y10BPACK: Cardinal;
begin
  Result := v4l2_fourcc('Y', '1', '0', 'B');
end;

function V4L2_PIX_FMT_PAL8: Cardinal;
begin
  Result := v4l2_fourcc('P', 'A', 'L', '8');
end;

function V4L2_PIX_FMT_YVU410: Cardinal;
begin
  Result := v4l2_fourcc('Y', 'V', 'U', '9');
end;

function V4L2_PIX_FMT_YVU420: Cardinal;
begin
  Result := v4l2_fourcc('Y', 'V', '1', '2');
end;

function V4L2_PIX_FMT_YUYV: Cardinal;
begin
  Result := v4l2_fourcc('Y', 'U', 'Y', 'V');
end;

function V4L2_PIX_FMT_YYUV: Cardinal;
begin
  Result := v4l2_fourcc('Y', 'Y', 'U', 'V');
end;

function V4L2_PIX_FMT_YVYU: Cardinal;
begin
  Result := v4l2_fourcc('Y', 'V', 'Y', 'U');
end;

function V4L2_PIX_FMT_UYVY: Cardinal;
begin
  Result := v4l2_fourcc('U', 'Y', 'V', 'Y');
end;

function V4L2_PIX_FMT_VYUY: Cardinal;
begin
  Result := v4l2_fourcc('V', 'Y', 'U', 'Y');
end;

function V4L2_PIX_FMT_YUV422P: Cardinal;
begin
  Result := v4l2_fourcc('4', '2', '2', 'P');
end;

function V4L2_PIX_FMT_YUV411P: Cardinal;
begin
  Result := v4l2_fourcc('4', '1', '1', 'P');
end;

function V4L2_PIX_FMT_Y41P: Cardinal;
begin
  Result := v4l2_fourcc('Y', '4', '1', 'P');
end;

function V4L2_PIX_FMT_YUV444: Cardinal;
begin
  Result := v4l2_fourcc('Y', '4', '4', '4');
end;

function V4L2_PIX_FMT_YUV555: Cardinal;
begin
  Result := v4l2_fourcc('Y', 'U', 'V', 'O');
end;

function V4L2_PIX_FMT_YUV565: Cardinal;
begin
  Result := v4l2_fourcc('Y', 'U', 'V', 'P');
end;

function V4L2_PIX_FMT_YUV32: Cardinal;
begin
  Result := v4l2_fourcc('Y', 'U', 'V', '4');
end;

function V4L2_PIX_FMT_YUV410: Cardinal;
begin
  Result := v4l2_fourcc('Y', 'U', 'V', '9');
end;

function V4L2_PIX_FMT_YUV420: Cardinal;
begin
  Result := v4l2_fourcc('Y', 'U', '1', '2');
end;

function V4L2_PIX_FMT_HI240: Cardinal;
begin
  Result := v4l2_fourcc('H', 'I', '2', '4');
end;

function V4L2_PIX_FMT_HM12: Cardinal;
begin
  Result := v4l2_fourcc('H', 'M', '1', '2');
end;

function V4L2_PIX_FMT_M420: Cardinal;
begin
  Result := v4l2_fourcc('M', '4', '2', '0');
end;

function V4L2_PIX_FMT_NV12: Cardinal;
begin
  Result := v4l2_fourcc('N', 'V', '1', '2');
end;

function V4L2_PIX_FMT_NV21: Cardinal;
begin
  Result := v4l2_fourcc('N', 'V', '2', '1');
end;

function V4L2_PIX_FMT_NV16: Cardinal;
begin
  Result := v4l2_fourcc('N', 'V', '1', '6');
end;

function V4L2_PIX_FMT_NV61: Cardinal;
begin
  Result := v4l2_fourcc('N', 'V', '6', '1');
end;

function V4L2_PIX_FMT_NV12M: Cardinal;
begin
  Result := v4l2_fourcc('N', 'M', '1', '2');
end;

function V4L2_PIX_FMT_NV12MT: Cardinal;
begin
  Result := v4l2_fourcc('T', 'M', '1', '2');
end;

function V4L2_PIX_FMT_YUV420M: Cardinal;
begin
  Result := v4l2_fourcc('Y', 'M', '1', '2');
end;

function V4L2_PIX_FMT_SBGGR8: Cardinal;
begin
  Result := v4l2_fourcc('B', 'A', '8', '1');
end;

function V4L2_PIX_FMT_SGBRG8: Cardinal;
begin
  Result := v4l2_fourcc('G', 'B', 'R', 'G');
end;

function V4L2_PIX_FMT_SGRBG8: Cardinal;
begin
  Result := v4l2_fourcc('G', 'R', 'B', 'G');
end;

function V4L2_PIX_FMT_SRGGB8: Cardinal;
begin
  Result := v4l2_fourcc('R', 'G', 'G', 'B');
end;

function V4L2_PIX_FMT_SBGGR10: Cardinal;
begin
  Result := v4l2_fourcc('B', 'G', '1', '0');
end;

function V4L2_PIX_FMT_SGBRG10: Cardinal;
begin
  Result := v4l2_fourcc('G', 'B', '1', '0');
end;

function V4L2_PIX_FMT_SGRBG10: Cardinal;
begin
  Result := v4l2_fourcc('B', 'A', '1', '0');
end;

function V4L2_PIX_FMT_SRGGB10: Cardinal;
begin
  Result := v4l2_fourcc('R', 'G', '1', '0');
end;

function V4L2_PIX_FMT_SBGGR12: Cardinal;
begin
  Result := v4l2_fourcc('B', 'G', '1', '2');
end;

function V4L2_PIX_FMT_SGBRG12: Cardinal;
begin
  Result := v4l2_fourcc('G', 'B', '1', '2');
end;

function V4L2_PIX_FMT_SGRBG12: Cardinal;
begin
  Result := v4l2_fourcc('B', 'A', '1', '2');
end;

function V4L2_PIX_FMT_SRGGB12: Cardinal;
begin
  Result := v4l2_fourcc('R', 'G', '1', '2');
end;

function V4L2_PIX_FMT_SGRBG10DPCM8: Cardinal;
begin
  Result := v4l2_fourcc('B', 'D', '1', '0');
end;

function V4L2_PIX_FMT_SBGGR16: Cardinal;
begin
  Result := v4l2_fourcc('B', 'Y', 'R', '2');
end;

function V4L2_PIX_FMT_MJPEG: Cardinal;
begin
  Result := v4l2_fourcc('M', 'J', 'P', 'G');
end;

function V4L2_PIX_FMT_JPEG: Cardinal;
begin
  Result := v4l2_fourcc('J', 'P', 'E', 'G');
end;

function V4L2_PIX_FMT_DV: Cardinal;
begin
  Result := v4l2_fourcc('d', 'v', 's', 'd');
end;

function V4L2_PIX_FMT_MPEG: Cardinal;
begin
  Result := v4l2_fourcc('M', 'P', 'E', 'G');
end;

function V4L2_PIX_FMT_CPIA1: Cardinal;
begin
  Result := v4l2_fourcc('C', 'P', 'I', 'A');
end;

function V4L2_PIX_FMT_WNVA: Cardinal;
begin
  Result := v4l2_fourcc('W', 'N', 'V', 'A');
end;

function V4L2_PIX_FMT_SN9C10X: Cardinal;
begin
  Result := v4l2_fourcc('S', '9', '1', '0');
end;

function V4L2_PIX_FMT_SN9C20X_I420: Cardinal;
begin
  Result := v4l2_fourcc('S', '9', '2', '0');
end;

function V4L2_PIX_FMT_PWC1: Cardinal;
begin
  Result := v4l2_fourcc('P', 'W', 'C', '1');
end;

function V4L2_PIX_FMT_PWC2: Cardinal;
begin
  Result := v4l2_fourcc('P', 'W', 'C', '2');
end;

function V4L2_PIX_FMT_ET61X251: Cardinal;
begin
  Result := v4l2_fourcc('E', '6', '2', '5');
end;

function V4L2_PIX_FMT_SPCA501: Cardinal;
begin
  Result := v4l2_fourcc('S', '5', '0', '1');
end;

function V4L2_PIX_FMT_SPCA505: Cardinal;
begin
  Result := v4l2_fourcc('S', '5', '0', '5');
end;

function V4L2_PIX_FMT_SPCA508: Cardinal;
begin
  Result := v4l2_fourcc('S', '5', '0', '8');
end;

function V4L2_PIX_FMT_SPCA561: Cardinal;
begin
  Result := v4l2_fourcc('S', '5', '6', '1');
end;

function V4L2_PIX_FMT_PAC207: Cardinal;
begin
  Result := v4l2_fourcc('P', '2', '0', '7');
end;

function V4L2_PIX_FMT_MR97310A: Cardinal;
begin
  Result := v4l2_fourcc('M', '3', '1', '0');
end;

function V4L2_PIX_FMT_SN9C2028: Cardinal;
begin
  Result := v4l2_fourcc('S', 'O', 'N', 'X');
end;

function V4L2_PIX_FMT_SQ905C: Cardinal;
begin
  Result := v4l2_fourcc('9', '0', '5', 'C');
end;

function V4L2_PIX_FMT_PJPG: Cardinal;
begin
  Result := v4l2_fourcc('P', 'J', 'P', 'G');
end;

function V4L2_PIX_FMT_OV511: Cardinal;
begin
  Result := v4l2_fourcc('O', '5', '1', '1');
end;

function V4L2_PIX_FMT_OV518: Cardinal;
begin
  Result := v4l2_fourcc('O', '5', '1', '8');
end;

function V4L2_PIX_FMT_STV0680: Cardinal;
begin
  Result := v4l2_fourcc('S', '6', '8', '0');
end;

function V4L2_PIX_FMT_TM6000: Cardinal;
begin
  Result := v4l2_fourcc('T', 'M', '6', '0');
end;

function V4L2_PIX_FMT_CIT_YYVYUY: Cardinal;
begin
  Result := v4l2_fourcc('C', 'I', 'T', 'V');
end;

function V4L2_PIX_FMT_KONICA420: Cardinal;
begin
  Result := v4l2_fourcc('K', 'O', 'N', 'I');
end;

function V4L2_PIX_FMT_JPGL: Cardinal;
begin
  Result := v4l2_fourcc('J', 'P', 'G', 'L');
end;

function V4L2_CTRL_ID2CLASS(const Id: PtrUInt): PtrUInt;
begin
  Result := id and $0FFF0000;
end;

function V4L2_CTRL_DRIVER_PRIV(const Id: PtrUInt): Boolean;
begin
  Result := (id and $FFFF) >= $1000;
end;

function VIDIOC_QUERYCAP: Integer;
begin
  Result := _IOR(VIDIOC_IOC_MAGIC, 0, SizeOf(v4l2_capability));
end;

function VIDIOC_RESERVED: Integer;
begin
  Result := _IO(VIDIOC_IOC_MAGIC, 1);
end;

function VIDIOC_ENUM_FMT: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 2, SizeOf(v4l2_fmtdesc));
end;

function VIDIOC_G_FMT: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 4, SizeOf(v4l2_format));
end;

function VIDIOC_S_FMT: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 5, SizeOf(v4l2_format));
end;

function VIDIOC_REQBUFS: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 8, SizeOf(v4l2_requestbuffers));
end;

function VIDIOC_QUERYBUF: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 9, SizeOf(v4l2_buffer));
end;

function VIDIOC_G_FBUF: Integer;
begin
  Result := _IOR(VIDIOC_IOC_MAGIC, 10, SizeOf(v4l2_framebuffer));
end;

function VIDIOC_S_FBUF: Integer;
begin
  Result := _IOW(VIDIOC_IOC_MAGIC, 11, SizeOf(v4l2_framebuffer));
end;

function VIDIOC_OVERLAY: Integer;
begin
  Result := _IOW(VIDIOC_IOC_MAGIC, 14, SizeOf(Integer));
end;

function VIDIOC_QBUF: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 15, SizeOf(v4l2_buffer));
end;

function VIDIOC_DQBUF: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 17, SizeOf(v4l2_buffer));
end;

function VIDIOC_STREAMON: Integer;
begin
  Result := _IOW(VIDIOC_IOC_MAGIC, 18, SizeOf(Integer));
end;

function VIDIOC_STREAMOFF: Integer;
begin
  Result := _IOW(VIDIOC_IOC_MAGIC, 19, SizeOf(Integer));
end;

function VIDIOC_G_PARM: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 21, SizeOf(v4l2_streamparm));
end;

function VIDIOC_S_PARM: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 22, SizeOf(v4l2_streamparm));
end;

function VIDIOC_G_STD: Integer;
begin
  Result := _IOR(VIDIOC_IOC_MAGIC, 23, SizeOf(v4l2_std_id));
end;

function VIDIOC_S_STD: Integer;
begin
  Result := _IOW(VIDIOC_IOC_MAGIC, 24, SizeOf(v4l2_std_id));
end;

function VIDIOC_ENUMSTD: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 25, SizeOf(v4l2_standard));
end;

function VIDIOC_ENUMINPUT: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 26, SizeOf(v4l2_input));
end;

function VIDIOC_G_CTRL: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 27, SizeOf(v4l2_control));
end;

function VIDIOC_S_CTRL: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 28, SizeOf(v4l2_control));
end;

function VIDIOC_G_TUNER: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 29, SizeOf(v4l2_tuner));
end;

function VIDIOC_S_TUNER: Integer;
begin
  Result := _IOW(VIDIOC_IOC_MAGIC, 30, SizeOf(v4l2_tuner));
end;

function VIDIOC_G_AUDIO: Integer;
begin
  Result := _IOR(VIDIOC_IOC_MAGIC, 33, SizeOf(v4l2_audio));
end;

function VIDIOC_S_AUDIO: Integer;
begin
  Result := _IOW(VIDIOC_IOC_MAGIC, 34, SizeOf(v4l2_audio));
end;

function VIDIOC_QUERYCTRL: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 36, SizeOf(v4l2_queryctrl));
end;

function VIDIOC_QUERYMENU: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 37, SizeOf(v4l2_querymenu));
end;

function VIDIOC_G_INPUT: Integer;
begin
  Result := _IOR(VIDIOC_IOC_MAGIC, 38, SizeOf(Integer));
end;

function VIDIOC_S_INPUT: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 39, SizeOf(Integer));
end;

function VIDIOC_G_OUTPUT: Integer;
begin
  Result := _IOR(VIDIOC_IOC_MAGIC, 46, SizeOf(Integer));
end;

function VIDIOC_S_OUTPUT: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 47, SizeOf(Integer));
end;

function VIDIOC_ENUMOUTPUT: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 48, SizeOf(v4l2_output));
end;

function VIDIOC_G_AUDOUT: Integer;
begin
  Result := _IOR(VIDIOC_IOC_MAGIC, 49, SizeOf(v4l2_audioout));
end;

function VIDIOC_S_AUDOUT: Integer;
begin
  Result := _IOW(VIDIOC_IOC_MAGIC, 50, SizeOf(v4l2_audioout));
end;

function VIDIOC_G_MODULATOR: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 54, SizeOf(v4l2_modulator));
end;

function VIDIOC_S_MODULATOR: Integer;
begin
  Result := _IOW(VIDIOC_IOC_MAGIC, 55, SizeOf(v4l2_modulator));
end;

function VIDIOC_G_FREQUENCY: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 56, SizeOf(v4l2_frequency));
end;

function VIDIOC_S_FREQUENCY: Integer;
begin
  Result := _IOW(VIDIOC_IOC_MAGIC, 57, SizeOf(v4l2_frequency));
end;

function VIDIOC_CROPCAP: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 58, SizeOf(v4l2_cropcap));
end;

function VIDIOC_G_CROP: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 59, SizeOf(v4l2_crop));
end;

function VIDIOC_S_CROP: Integer;
begin
  Result := _IOW(VIDIOC_IOC_MAGIC, 60, SizeOf(v4l2_crop));
end;

function VIDIOC_G_JPEGCOMP: Integer;
begin
  Result := _IOR(VIDIOC_IOC_MAGIC, 61, SizeOf(v4l2_jpegcompression));
end;

function VIDIOC_S_JPEGCOMP: Integer;
begin
  Result := _IOW(VIDIOC_IOC_MAGIC, 62, SizeOf(v4l2_jpegcompression));
end;

function VIDIOC_QUERYSTD: Integer;
begin
  Result := _IOR(VIDIOC_IOC_MAGIC, 63, SizeOf(v4l2_std_id));
end;

function VIDIOC_TRY_FMT: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 64, SizeOf(v4l2_format));
end;

function VIDIOC_ENUMAUDIO: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 65, SizeOf(v4l2_audio));
end;

function VIDIOC_ENUMAUDOUT: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 66, SizeOf(v4l2_audioout));
end;

function VIDIOC_G_PRIORITY: Integer;
begin
  Result := _IOR(VIDIOC_IOC_MAGIC, 67, SizeOf(v4l2_priority));
end;

function VIDIOC_S_PRIORITY: Integer;
begin
  Result := _IOW(VIDIOC_IOC_MAGIC, 68, SizeOf(v4l2_priority));
end;

function VIDIOC_G_SLICED_VBI_CAP: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 69, SizeOf(v4l2_sliced_vbi_cap));
end;

function VIDIOC_LOG_STATUS: Integer;
begin
  Result := _IO(VIDIOC_IOC_MAGIC, 70);
end;

function VIDIOC_G_EXT_CTRLS: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 71, SizeOf(v4l2_ext_controls));
end;

function VIDIOC_S_EXT_CTRLS: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 72, SizeOf(v4l2_ext_controls));
end;

function VIDIOC_TRY_EXT_CTRLS: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 73, SizeOf(v4l2_ext_controls));
end;

function VIDIOC_ENUM_FRAMESIZES: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 74, SizeOf(v4l2_frmsizeenum));
end;

function VIDIOC_ENUM_FRAMEINTERVALS: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 75, SizeOf(v4l2_frmivalenum));
end;

function VIDIOC_G_ENC_INDEX: Integer;
begin
  Result := _IOR(VIDIOC_IOC_MAGIC, 76, SizeOf(v4l2_enc_idx));
end;

function VIDIOC_ENCODER_CMD: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 77, SizeOf(v4l2_encoder_cmd));
end;

function VIDIOC_TRY_ENCODER_CMD: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 78, SizeOf(v4l2_encoder_cmd));
end;

function VIDIOC_DBG_S_REGISTER: Integer;
begin
  Result := _IOW(VIDIOC_IOC_MAGIC, 79, SizeOf(v4l2_dbg_register));
end;

function VIDIOC_DBG_G_REGISTER: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 80, SizeOf(v4l2_dbg_register));
end;

function VIDIOC_DBG_G_CHIP_IDENT: Integer;
begin
  Result := _IOWR(VIDIOC_IOC_MAGIC, 81, SizeOf(v4l2_dbg_chip_ident));
end;

function VIDIOC_S_HW_FREQ_SEEK: Integer;
begin
  Result := _IOW(VIDIOC_IOC_MAGIC, 82, SizeOf(v4l2_hw_freq_seek));
end;

end.
