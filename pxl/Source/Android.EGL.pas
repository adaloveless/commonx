unit Android.EGL;

{
  Copyright (c) 2007-2010 The Khronos Group Inc.

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and/or associated
  documentation files (the "Materials"), to deal in the Materials without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Materials, and
  to permit persons to whom the Materials are furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of
  the Materials.

  THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
  THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
  CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE MATERIALS OR THE USE OR OTHER DEALINGS
  IN THE MATERIALS.

  Android NDK C/C++ files:
    KHR/khrplatform.h
    EGL/eglplatform.h
    EGL/egl.h
    EGL/eglext.h

  Original source code was taken from:
    %NDK_DIR%/platforms/android-9/arch-arm/usr/include/

  Pascal translation by Yuriy Kotsarenko, August 2015.
}

interface

{$INCLUDE Android.Config.inc}

uses
  Android.NativeWindow;

{$INCLUDE Android.LibDefs.inc}

type
{ basic type definitions }
  khronos_int32_t = LongInt;
  khronos_uint32_t = LongWord;
  khronos_int64_t = Int64;
  khronos_uint64_t = UInt64;

{ Types that are (so far) the same on all platforms }
  khronos_int8_t = ShortInt;
  khronos_uint8_t = Byte;
  khronos_int16_t = SmallInt;
  khronos_uint16_t = Word;
  khronos_intptr_t = PtrInt;
  khronos_uintptr_t = PtrUInt;
  khronos_ssize_t = LongInt;
  khronos_usize_t = LongWord;

{ Float type }
  khronos_float_t = Single;

{ Time types

  These types can be used to represent a time interval in nanoseconds or an absolute Unadjusted System Time.
  Unadjusted System Time is the number of nanoseconds since some arbitrary system event (e.g. since the last time
  the system booted).  The Unadjusted System Time is an unsigned 64 bit value that wraps back to 0 every 584 years.
  Time intervals may be either signed or unsigned. }
  khronos_utime_nanoseconds_t = khronos_uint64_t;
  khronos_stime_nanoseconds_t = khronos_int64_t;

  PEGLint = ^EGLint;
  EGLint = khronos_int32_t;

  Pegl_native_pixmap_t = ^egl_native_pixmap_t;
  egl_native_pixmap_t = record
  end;

  EGLNativeWindowType = PANativeWindow;
  EGLNativePixmapType = Pegl_native_pixmap_t;
  EGLNativeDisplayType = Pointer;

{ EGL 1.2 types, renamed for consistency in EGL 1.3 }
  NativeDisplayType = EGLNativeDisplayType;
  NativePixmapType = EGLNativePixmapType;
  NativeWindowType = EGLNativeWindowType;

{ EGL Types }
  EGLBoolean = LongWord;
  EGLenum = LongWord;

  PEGLConfig = ^EGLConfig;
  EGLConfig = Pointer;

  EGLContext = Pointer;
  EGLDisplay = Pointer;
  EGLSurface = Pointer;
  EGLClientBuffer = Pointer;

  EGLImageKHR = Pointer;
  EGLSyncKHR = Pointer;
  EGLTimeKHR = khronos_utime_nanoseconds_t;
  EGLSyncNV = Pointer;
  EGLTimeNV = UInt64;

  ANativeWindowBuffer = record
  end;

  EGLint64NV = khronos_int64_t;
  EGLuint64NV = khronos_uint64_t;

const
{ EGL Versioning }
  EGL_VERSION_1_0 = 1;
  EGL_VERSION_1_1 = 1;
  EGL_VERSION_1_2 = 1;
  EGL_VERSION_1_3 = 1;
  EGL_VERSION_1_4 = 1;

{ EGL Enumerants. Bitmasks and other exceptional cases aside, most enums are assigned unique values
  starting at 0x3000. }

{ EGL aliases }
  EGL_FALSE = 0;
  EGL_TRUE = 1;

{ Out-of-band handle values }
  EGL_DEFAULT_DISPLAY =	EGLNativeDisplayType(0);
  EGL_NO_CONTEXT = EGLContext(0);
  EGL_NO_DISPLAY = EGLDisplay(0);
  EGL_NO_SURFACE = EGLSurface(0);

{ Out-of-band attribute value }
  EGL_DONT_CARE = -1;

{ Errors / GetError return values }
  EGL_SUCCESS = $3000;
  EGL_NOT_INITIALIZED = $3001;
  EGL_BAD_ACCESS = $3002;
  EGL_BAD_ALLOC = $3003;
  EGL_BAD_ATTRIBUTE = $3004;
  EGL_BAD_CONFIG = $3005;
  EGL_BAD_CONTEXT = $3006;
  EGL_BAD_CURRENT_SURFACE = $3007;
  EGL_BAD_DISPLAY = $3008;
  EGL_BAD_MATCH = $3009;
  EGL_BAD_NATIVE_PIXMAP = $300A;
  EGL_BAD_NATIVE_WINDOW = $300B;
  EGL_BAD_PARAMETER = $300C;
  EGL_BAD_SURFACE = $300D;
  EGL_CONTEXT_LOST = $300E; // EGL 1.1 - IMG_power_management

{ Reserved 0x300F-0x301F for additional errors }

{ Config attributes }
  EGL_BUFFER_SIZE = $3020;
  EGL_ALPHA_SIZE = $3021;
  EGL_BLUE_SIZE = $3022;
  EGL_GREEN_SIZE = $3023;
  EGL_RED_SIZE = $3024;
  EGL_DEPTH_SIZE = $3025;
  EGL_STENCIL_SIZE = $3026;
  EGL_CONFIG_CAVEAT = $3027;
  EGL_CONFIG_ID = $3028;
  EGL_LEVEL = $3029;
  EGL_MAX_PBUFFER_HEIGHT = $302A;
  EGL_MAX_PBUFFER_PIXELS = $302B;
  EGL_MAX_PBUFFER_WIDTH = $302C;
  EGL_NATIVE_RENDERABLE = $302D;
  EGL_NATIVE_VISUAL_ID = $302E;
  EGL_NATIVE_VISUAL_TYPE = $302F;
  EGL_SAMPLES = $3031;
  EGL_SAMPLE_BUFFERS = $3032;
  EGL_SURFACE_TYPE = $3033;
  EGL_TRANSPARENT_TYPE = $3034;
  EGL_TRANSPARENT_BLUE_VALUE = $3035;
  EGL_TRANSPARENT_GREEN_VALUE = $3036;
  EGL_TRANSPARENT_RED_VALUE = $3037;
  EGL_NONE = $3038; // Attrib list terminator
  EGL_BIND_TO_TEXTURE_RGB = $3039;
  EGL_BIND_TO_TEXTURE_RGBA = $303A;
  EGL_MIN_SWAP_INTERVAL = $303B;
  EGL_MAX_SWAP_INTERVAL = $303C;
  EGL_LUMINANCE_SIZE = $303D;
  EGL_ALPHA_MASK_SIZE = $303E;
  EGL_COLOR_BUFFER_TYPE = $303F;
  EGL_RENDERABLE_TYPE = $3040;
  EGL_MATCH_NATIVE_PIXMAP = $3041; // Pseudo-attribute (not queryable)
  EGL_CONFORMANT = $3042;

{ Reserved 0x3041-0x304F for additional config attributes }

{ Config attribute values }
  EGL_SLOW_CONFIG = $3050;
  EGL_NON_CONFORMANT_CONFIG = $3051;
  EGL_TRANSPARENT_RGB = $3052;
  EGL_RGB_BUFFER = $308E;
  EGL_LUMINANCE_BUFFER = $308F;

{ More config attribute values, for EGL_TEXTURE_FORMAT }
  EGL_NO_TEXTURE = $305C;
  EGL_TEXTURE_RGB = $305D;
  EGL_TEXTURE_RGBA = $305E;
  EGL_TEXTURE_2D = $305F;

{ Config attribute mask bits  }
  { EGL_SURFACE_TYPE mask bits }
  EGL_PBUFFER_BIT = $0001;
  EGL_PIXMAP_BIT = $0002;
  EGL_WINDOW_BIT = $0004;
  EGL_VG_COLORSPACE_LINEAR_BIT = $0020;
  EGL_VG_ALPHA_FORMAT_PRE_BIT = $0040;
  EGL_MULTISAMPLE_RESOLVE_BOX_BIT = $0200;
  EGL_SWAP_BEHAVIOR_PRESERVED_BIT = $0400;

  { EGL_RENDERABLE_TYPE mask bits }
  EGL_OPENGL_ES_BIT = $0001;
  EGL_OPENVG_BIT = $0002;
  EGL_OPENGL_ES2_BIT = $0004;
  EGL_OPENGL_BIT = $0008;

{ QueryString targets }
  EGL_VENDOR = $3053;
  EGL_VERSION = $3054;
  EGL_EXTENSIONS = $3055;
  EGL_CLIENT_APIS = $308D;

{ QuerySurface / SurfaceAttrib / CreatePbufferSurface targets }
  EGL_HEIGHT = $3056;
  EGL_WIDTH = $3057;
  EGL_LARGEST_PBUFFER = $3058;
  EGL_TEXTURE_FORMAT = $3080;
  EGL_TEXTURE_TARGET = $3081;
  EGL_MIPMAP_TEXTURE = $3082;
  EGL_MIPMAP_LEVEL = $3083;
  EGL_RENDER_BUFFER = $3086;
  EGL_VG_COLORSPACE = $3087;
  EGL_VG_ALPHA_FORMAT = $3088;
  EGL_HORIZONTAL_RESOLUTION = $3090;
  EGL_VERTICAL_RESOLUTION = $3091;
  EGL_PIXEL_ASPECT_RATIO = $3092;
  EGL_SWAP_BEHAVIOR = $3093;
  EGL_MULTISAMPLE_RESOLVE = $3099;

{ EGL_RENDER_BUFFER values / BindTexImage / ReleaseTexImage buffer targets  }
  EGL_BACK_BUFFER = $3084;
  EGL_SINGLE_BUFFER = $3085;

{ OpenVG color spaces }
  { EGL_VG_COLORSPACE value }
  EGL_VG_COLORSPACE_sRGB = $3089;
  EGL_VG_COLORSPACE_LINEAR = $308A;

{ OpenVG alpha formats }
  { EGL_ALPHA_FORMAT value }
  EGL_VG_ALPHA_FORMAT_NONPRE = $308B;
  EGL_VG_ALPHA_FORMAT_PRE = $308C;

{ Constant scale factor by which fractional display resolutions & aspect ratio are scaled when queried as integer
  values. }
  EGL_DISPLAY_SCALING = 10000;

{ Unknown display resolution/aspect ratio }
  EGL_UNKNOWN = -1;

{ Back buffer swap behaviors }
  { EGL_SWAP_BEHAVIOR value }
  EGL_BUFFER_PRESERVED = $3094;
  EGL_BUFFER_DESTROYED = $3095;

{ CreatePbufferFromClientBuffer buffer types }
  EGL_OPENVG_IMAGE = $3096;

{ QueryContext targets }
  EGL_CONTEXT_CLIENT_TYPE = $3097;

{ CreateContext attributes }
  EGL_CONTEXT_CLIENT_VERSION = $3098;

{ Multisample resolution behaviors }
  { EGL_MULTISAMPLE_RESOLVE value }
  EGL_MULTISAMPLE_RESOLVE_DEFAULT = $309A;
  EGL_MULTISAMPLE_RESOLVE_BOX = $309B;

{ BindAPI/QueryAPI targets }
  EGL_OPENGL_ES_API = $30A0;
  EGL_OPENVG_API = $30A1;
  EGL_OPENGL_API = $30A2;

{ GetCurrentSurface targets }
  EGL_DRAW = $3059;
  EGL_READ = $305A;

{ WaitNative engines }
  EGL_CORE_NATIVE_ENGINE = $305B;

{ EGL 1.2 tokens renamed for consistency in EGL 1.3 }
  EGL_COLORSPACE = EGL_VG_COLORSPACE;
  EGL_ALPHA_FORMAT = EGL_VG_ALPHA_FORMAT;
  EGL_COLORSPACE_sRGB = EGL_VG_COLORSPACE_sRGB;
  EGL_COLORSPACE_LINEAR = EGL_VG_COLORSPACE_LINEAR;
  EGL_ALPHA_FORMAT_NONPRE = EGL_VG_ALPHA_FORMAT_NONPRE;
  EGL_ALPHA_FORMAT_PRE = EGL_VG_ALPHA_FORMAT_PRE;

{ Header file version number
  Current version at http://www.khronos.org/registry/egl/ }
  EGL_EGLEXT_VERSION = 5;

  { EGLConfig attribute }
  EGL_CONFORMANT_KHR = $3042;

  { EGL_SURFACE_TYPE bitfield }
  EGL_VG_COLORSPACE_LINEAR_BIT_KHR = $0020;
  EGL_VG_ALPHA_FORMAT_PRE_BIT_KHR = $0040;

  { EGL_LOCK_USAGE_HINT_KHR bitfield }
  EGL_READ_SURFACE_BIT_KHR = $0001;
  EGL_WRITE_SURFACE_BIT_KHR = $0002;

  { EGL_SURFACE_TYPE bitfield }
  EGL_LOCK_SURFACE_BIT_KHR = $0080;
  EGL_OPTIMAL_FORMAT_BIT_KHR = $0100;

  { EGLConfig attribute }
  EGL_MATCH_FORMAT_KHR = $3043;

  { EGL_MATCH_FORMAT_KHR value }
  EGL_FORMAT_RGB_565_EXACT_KHR = $30C0;
  EGL_FORMAT_RGB_565_KHR = $30C1;
  EGL_FORMAT_RGBA_8888_EXACT_KHR = $30C2;
  EGL_FORMAT_RGBA_8888_KHR = $30C3;

  { eglLockSurfaceKHR attribute }
  EGL_MAP_PRESERVE_PIXELS_KHR = $30C4;
  EGL_LOCK_USAGE_HINT_KHR = $30C5;

  { eglQuerySurface attribute }
  EGL_BITMAP_POINTER_KHR = $30C6;
  EGL_BITMAP_PITCH_KHR = $30C7;
  EGL_BITMAP_ORIGIN_KHR = $30C8;
  EGL_BITMAP_PIXEL_RED_OFFSET_KHR = $30C9;
  EGL_BITMAP_PIXEL_GREEN_OFFSET_KHR = $30CA;
  EGL_BITMAP_PIXEL_BLUE_OFFSET_KHR = $30CB;
  EGL_BITMAP_PIXEL_ALPHA_OFFSET_KHR = $30CC;
  EGL_BITMAP_PIXEL_LUMINANCE_OFFSET_KHR = $30CD;

  { EGL_BITMAP_ORIGIN_KHR value }
  EGL_LOWER_LEFT_KHR = $30CE;
  EGL_UPPER_LEFT_KHR = $30CF;

  { eglCreateImageKHR target }
  EGL_NATIVE_PIXMAP_KHR = $30B0;

  { eglCreateImageKHR target }
  EGL_VG_PARENT_IMAGE_KHR = $30BA;

  { eglCreateImageKHR target }
  EGL_GL_TEXTURE_2D_KHR = $30B1;

  { eglCreateImageKHR attribute }
  EGL_GL_TEXTURE_LEVEL_KHR = $30BC;

  { eglCreateImageKHR target }
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_X_KHR = $30B3;
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_X_KHR = $30B4;
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Y_KHR = $30B5;
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_KHR = $30B6;
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Z_KHR = $30B7;
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_KHR = $30B8;

  { eglCreateImageKHR target }
  EGL_GL_TEXTURE_3D_KHR = $30B2;
  EGL_GL_TEXTURE_ZOFFSET_KHR = $30BD;

  { eglCreateImageKHR target }
  EGL_GL_RENDERBUFFER_KHR = $30B9;

  EGL_SYNC_STATUS_KHR = $30F1;
  EGL_SIGNALED_KHR = $30F2;
  EGL_UNSIGNALED_KHR = $30F3;
  EGL_TIMEOUT_EXPIRED_KHR = $30F5;
  EGL_CONDITION_SATISFIED_KHR = $30F6;
  EGL_SYNC_TYPE_KHR = $30F7;
  EGL_SYNC_REUSABLE_KHR = $30FA;
  EGL_SYNC_FLUSH_COMMANDS_BIT_KHR = $0001; // eglClientWaitSyncKHR <flags> bitfield
  EGL_FOREVER_KHR = $FFFFFFFFFFFFFFFF;
  EGL_NO_SYNC_KHR	= EGLSyncKHR(0);

{ Most interfaces defined by EGL_KHR_image_pixmap above }
  { eglCreateImageKHR attribute }
  EGL_IMAGE_PRESERVED_KHR = $30D2;

{ Interfaces defined by EGL_KHR_image above }

  EGL_CONTEXT_PRIORITY_LEVEL_IMG = $3100;
  EGL_CONTEXT_PRIORITY_HIGH_IMG = $3101;
  EGL_CONTEXT_PRIORITY_MEDIUM_IMG = $3102;
  EGL_CONTEXT_PRIORITY_LOW_IMG = $3103;

  EGL_COVERAGE_BUFFERS_NV = $30E0;
  EGL_COVERAGE_SAMPLES_NV = $30E1;

  EGL_DEPTH_ENCODING_NV = $30E2;
  EGL_DEPTH_ENCODING_NONE_NV = 0;
  EGL_DEPTH_ENCODING_NONLINEAR_NV = $30E3;

  EGL_SYNC_PRIOR_COMMANDS_COMPLETE_NV = $30E6;
  EGL_SYNC_STATUS_NV = $30E7;
  EGL_SIGNALED_NV = $30E8;
  EGL_UNSIGNALED_NV = $30E9;
  EGL_SYNC_FLUSH_COMMANDS_BIT_NV = $0001;
  EGL_FOREVER_NV = $FFFFFFFFFFFFFFFF;
  EGL_ALREADY_SIGNALED_NV = $30EA;
  EGL_TIMEOUT_EXPIRED_NV = $30EB;
  EGL_CONDITION_SATISFIED_NV = $30EC;
  EGL_SYNC_TYPE_NV = $30ED;
  EGL_SYNC_CONDITION_NV = $30EE;
  EGL_SYNC_FENCE_NV = $30EF;
  EGL_NO_SYNC_NV = EGLSyncNV(0);

{ Reuses most tokens and entry points from EGL_KHR_reusable_sync }
  EGL_SYNC_PRIOR_COMMANDS_COMPLETE_KHR = $30F0;
  EGL_SYNC_CONDITION_KHR = $30F8;
  EGL_SYNC_FENCE_KHR = $30F9;

  { eglCreateImageKHR target }
  EGL_NATIVE_BUFFER_ANDROID = $3140;

  { EGLConfig attribute }
  EGL_RECORDABLE_ANDROID = $3142;

{ EGL Functions }
function eglGetError: EGLint; cdecl;
  external libEGL name 'eglGetError';

function eglGetDisplay(display_id: EGLNativeDisplayType): EGLDisplay; cdecl;
  external libEGL name 'eglGetDisplay';

function eglInitialize(dpy: EGLDisplay; major, minor: PEGLint): EGLBoolean; cdecl;
  external libEGL name 'eglInitialize';

function eglTerminate(dpy: EGLDisplay): EGLBoolean; cdecl;
  external libEGL name 'eglTerminate';

function eglQueryString(dpy: EGLDisplay; name: EGLint): PAnsiChar; cdecl;
  external libEGL name 'eglQueryString';

function eglGetConfigs(dpy: EGLDisplay; configs: PEGLConfig; config_size: EGLint;
  num_config: PEGLint): EGLBoolean; cdecl;
  external libEGL name 'eglGetConfigs';

function eglChooseConfig(dpy: EGLDisplay; attrib_list: PEGLint; configs: PEGLConfig; config_size: EGLint;
  num_config: PEGLint): EGLBoolean; cdecl;
  external libEGL name 'eglChooseConfig';

function eglGetConfigAttrib(dpy: EGLDisplay; config: EGLConfig; attribute: EGLint; value: PEGLint): EGLBoolean; cdecl;
  external libEGL name 'eglGetConfigAttrib';

function eglCreateWindowSurface(dpy: EGLDisplay; config: EGLConfig; win: EGLNativeWindowType;
  attrib_list: PEGLint): EGLSurface; cdecl;
  external libEGL name 'eglCreateWindowSurface';

function eglCreatePbufferSurface(dpy: EGLDisplay; config: EGLConfig; attrib_list: PEGLint): EGLSurface; cdecl;
  external libEGL name 'eglCreatePbufferSurface';

function eglCreatePixmapSurface(dpy: EGLDisplay; config: EGLConfig; pixmap: EGLNativePixmapType;
  attrib_list: PEGLint): EGLSurface; cdecl;
  external libEGL name 'eglCreatePixmapSurface';

function eglDestroySurface(dpy: EGLDisplay; surface: EGLSurface): EGLBoolean; cdecl;
  external libEGL name 'eglDestroySurface';

function eglQuerySurface(dpy: EGLDisplay; surface: EGLSurface; attribute: EGLint; value: PEGLint): EGLBoolean; cdecl;
  external libEGL name 'eglQuerySurface';

function eglBindAPI(api: EGLenum): EGLBoolean; cdecl;
  external libEGL name 'eglBindAPI';

function eglQueryAPI: EGLenum; cdecl;
  external libEGL name 'eglQueryAPI';

function eglWaitClient: EGLBoolean; cdecl;
  external libEGL name 'eglWaitClient';

function eglReleaseThread: EGLBoolean; cdecl;
  external libEGL name 'eglReleaseThread';

function eglCreatePbufferFromClientBuffer(dpy: EGLDisplay; buftype: EGLenum; buffer: EGLClientBuffer;
  config: EGLConfig; attrib_list: PEGLint): EGLSurface; cdecl;
  external libEGL name 'eglCreatePbufferFromClientBuffer';

function eglSurfaceAttrib(dpy: EGLDisplay; surface: EGLSurface; attribute, value: EGLint): EGLBoolean; cdecl;
  external libEGL name 'eglSurfaceAttrib';

function eglBindTexImage(dpy: EGLDisplay; surface: EGLSurface; buffer: EGLint): EGLBoolean; cdecl;
  external libEGL name 'eglBindTexImage';

function eglReleaseTexImage(dpy: EGLDisplay; surface: EGLSurface; buffer: EGLint): EGLBoolean; cdecl;
  external libEGL name 'eglReleaseTexImage';

function eglSwapInterval(dpy: EGLDisplay; interval: EGLint): EGLBoolean; cdecl;
  external libEGL name 'eglSwapInterval';

function eglCreateContext(dpy: EGLDisplay; config: EGLConfig; share_context: EGLContext;
  attrib_list: PEGLint): EGLContext; cdecl;
  external libEGL name 'eglCreateContext';

function eglDestroyContext(dpy: EGLDisplay; ctx: EGLContext): EGLBoolean; cdecl;
  external libEGL name 'eglDestroyContext';

function eglMakeCurrent(dpy: EGLDisplay; draw, read: EGLSurface; ctx: EGLContext): EGLBoolean; cdecl;
  external libEGL name 'eglMakeCurrent';

function eglGetCurrentContext: EGLContext; cdecl;
  external libEGL name 'eglGetCurrentContext';

function eglGetCurrentSurface(readdraw: EGLint): EGLSurface; cdecl;
  external libEGL name 'eglGetCurrentSurface';

function eglGetCurrentDisplay: EGLDisplay; cdecl;
  external libEGL name 'eglGetCurrentDisplay';

function eglQueryContext(dpy: EGLDisplay; ctx: EGLContext; attribute: EGLint; value: PEGLint): EGLBoolean; cdecl;
  external libEGL name 'eglQueryContext';

function eglWaitGL: EGLBoolean; cdecl;
  external libEGL name 'eglWaitGL';

function eglWaitNative(engine: EGLint): EGLBoolean; cdecl;
  external libEGL name 'eglWaitNative';

function eglSwapBuffers(dpy: EGLDisplay; surface: EGLSurface): EGLBoolean; cdecl;
  external libEGL name 'eglSwapBuffers';

function eglCopyBuffers(dpy: EGLDisplay; surface: EGLSurface; target: EGLNativePixmapType): EGLBoolean; cdecl;
  external libEGL name 'eglCopyBuffers';

function eglGetProcAddress(procname: PAnsiChar): Pointer; cdecl;
  external libEGL name 'eglGetProcAddress';

var
  eglLockSurfaceKHR: function(display: EGLDisplay; surface: EGLSurface; attrib_list: PEGLint): EGLBoolean; cdecl;
  eglUnlockSurfaceKHR: function(display: EGLDisplay; surface: EGLSurface): EGLBoolean; cdecl;
  eglCreateImageKHR: function(dpy: EGLDisplay; ctx: EGLContext; target: EGLenum; buffer: EGLClientBuffer;
    attrib_list: PEGLint): EGLImageKHR; cdecl;
  eglDestroyImageKHR: function(dpy: EGLDisplay; image: EGLImageKHR): EGLBoolean; cdecl;
  eglCreateSyncKHR: function(dpy: EGLDisplay; _type: EGLenum; attrib_list: PEGLint): EGLSyncKHR; cdecl;
  eglDestroySyncKHR: function(dpy: EGLDisplay; sync: EGLSyncKHR): EGLBoolean; cdecl;
  eglClientWaitSyncKHR: function(dpy: EGLDisplay; sync: EGLSyncKHR; flags: EGLint; timeout: EGLTimeKHR): EGLint; cdecl;
  eglSignalSyncKHR: function(dpy: EGLDisplay; sync: EGLSyncKHR; mode: EGLenum): EGLBoolean; cdecl;
  eglGetSyncAttribKHR: function(dpy: EGLDisplay; sync: EGLSyncKHR; attribute: EGLint;
    value: PEGLint): EGLBoolean; cdecl;
  eglCreateFenceSyncNV: function(dpy: EGLDisplay; condition: EGLenum; attrib_list: PEGLint): EGLSyncNV; cdecl;
  eglDestroySyncNV: function(sync: EGLSyncNV): EGLBoolean; cdecl;
  eglFenceNV: function(sync: EGLSyncNV): EGLBoolean; cdecl;
  eglClientWaitSyncNV: function(sync: EGLSyncNV; flags: EGLint; timeout: EGLTimeNV): EGLint; cdecl;
  eglSignalSyncNV: function(sync: EGLSyncNV; mode: EGLenum): EGLBoolean; cdecl;
  eglGetSyncAttribNV: function(sync: EGLSyncNV; attribute: EGLint; value: PEGLint): EGLBoolean; cdecl;
  eglSetSwapRectangleANDROID: function(dpy: EGLDisplay; draw: EGLSurface; left: EGLint; top: EGLint; width: EGLint;
    height: EGLint): EGLBoolean; cdecl;
  eglGetSystemTimeFrequencyNV: function: EGLuint64NV; cdecl;
  eglGetSystemTimeNV: function: EGLuint64NV; cdecl;

function LoadEGLExtensions: Boolean;
procedure UnloadEGLExtensions;

function glGetProcAddress(Handle: Pointer; ProcName: PAnsiChar): Pointer;

implementation

uses
  Android.DlFcn;

function glGetProcAddress(Handle: Pointer; ProcName: PAnsiChar): Pointer;
begin
  Result := dlsym(Handle, ProcName);

  if Result = nil then
    Result := eglGetProcAddress(ProcName);
end;

var
  libEGLHandle: Pointer = nil;

procedure ResetExtensions;
begin
  eglLockSurfaceKHR := nil;
  eglUnlockSurfaceKHR := nil;
  eglCreateImageKHR := nil;
  eglDestroyImageKHR := nil;
  eglCreateSyncKHR := nil;
  eglDestroySyncKHR := nil;
  eglClientWaitSyncKHR := nil;
  eglSignalSyncKHR := nil;
  eglGetSyncAttribKHR := nil;
  eglCreateFenceSyncNV := nil;
  eglDestroySyncNV := nil;
  eglFenceNV := nil;
  eglClientWaitSyncNV := nil;
  eglSignalSyncNV := nil;
  eglGetSyncAttribNV := nil;
  eglSetSwapRectangleANDROID := nil;
  eglGetSystemTimeFrequencyNV := nil;
  eglGetSystemTimeNV := nil;
end;

function LoadEGLExtensions: Boolean;
begin
  if libEGLHandle <> nil then
    Exit(True);

  libEGLHandle := dlopen(libEGL, RTLD_LAZY);
  if libEGLHandle = nil then
    Exit(False);

  eglLockSurfaceKHR := glGetProcAddress(libEGLHandle, 'eglLockSurfaceKHR');
  eglUnlockSurfaceKHR := glGetProcAddress(libEGLHandle, 'eglUnlockSurfaceKHR');
  eglCreateImageKHR := glGetProcAddress(libEGLHandle, 'eglCreateImageKHR');
  eglDestroyImageKHR := glGetProcAddress(libEGLHandle, 'eglDestroyImageKHR');
  eglCreateSyncKHR := glGetProcAddress(libEGLHandle, 'eglCreateSyncKHR');
  eglDestroySyncKHR := glGetProcAddress(libEGLHandle, 'eglDestroySyncKHR');
  eglClientWaitSyncKHR := glGetProcAddress(libEGLHandle, 'eglClientWaitSyncKHR');
  eglSignalSyncKHR := glGetProcAddress(libEGLHandle, 'eglSignalSyncKHR');
  eglGetSyncAttribKHR := glGetProcAddress(libEGLHandle, 'eglGetSyncAttribKHR');
  eglCreateFenceSyncNV := glGetProcAddress(libEGLHandle, 'eglCreateFenceSyncNV');
  eglDestroySyncNV := glGetProcAddress(libEGLHandle, 'eglDestroySyncNV');
  eglFenceNV := glGetProcAddress(libEGLHandle, 'eglFenceNV');
  eglClientWaitSyncNV := glGetProcAddress(libEGLHandle, 'eglClientWaitSyncNV');
  eglSignalSyncNV := glGetProcAddress(libEGLHandle, 'eglSignalSyncNV');
  eglGetSyncAttribNV := glGetProcAddress(libEGLHandle, 'eglGetSyncAttribNV');
  eglSetSwapRectangleANDROID := glGetProcAddress(libEGLHandle, 'eglSetSwapRectangleANDROID');
  eglGetSystemTimeFrequencyNV := glGetProcAddress(libEGLHandle, 'eglGetSystemTimeFrequencyNV');
  eglGetSystemTimeNV := glGetProcAddress(libEGLHandle, 'eglGetSystemTimeNV');
  
  Result := True;
end;

procedure UnloadEGLExtensions;
begin
  if libEGLHandle <> nil then
  begin
    dlclose(libEGLHandle);
    libEGLHandle := nil;

    ResetExtensions;
  end;
end;

initialization
  ResetExtensions;

end.

