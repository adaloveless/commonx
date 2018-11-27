unit PXL.Windows.DDraw;
{
  DirectX Headers translation by Yuriy Kotsarenko, August 2015. Revision 1.0.

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General
  Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
  details.

  Translated DirectX C/C++ files:
    ddraw.h

  Original source code was taken from:
    %WINDOWS_KITS%\8.1\Include\um\
    %WINDOWS_KITS%\8.1\Include\shared\

  Note: portions of this file were translated using automated tool.
}
interface

{$IFDEF FPC}
  {$PACKRECORDS C}
  {$MODE DELPHI}
{$ELSE}
  {$ALIGN ON}
{$ENDIF}

uses
  Windows, MultiMon;

const
{ If you wish an application built against the newest version of DirectDraw to run against an older DirectDraw run time
  then define DIRECTDRAW_VERSION to be the earlies version of DirectDraw you wish to run against. For, example if you
  wish an application to run against a DX 3 runtime define DIRECTDRAW_VERSION to be 0x0300. }
  DIRECTDRAW_VERSION = $0700;

{ ddsCaps field is valid. }
  DDSD_CAPS = $00000001; // default

{ dwHeight field is valid. }
  DDSD_HEIGHT = $00000002;

{ dwWidth field is valid. }
  DDSD_WIDTH = $00000004;

{ lPitch is valid. }
  DDSD_PITCH = $00000008;

{ dwBackBufferCount is valid. }
  DDSD_BACKBUFFERCOUNT = $00000020;

{ dwZBufferBitDepth is valid.  (shouldn't be used in DDSURFACEDESC2) }
  DDSD_ZBUFFERBITDEPTH = $00000040;

{ dwAlphaBitDepth is valid. }
  DDSD_ALPHABITDEPTH = $00000080;

{ lpSurface is valid. }
  DDSD_LPSURFACE = $00000800;

{ ddpfPixelFormat is valid. }
  DDSD_PIXELFORMAT = $00001000;

{ ddckCKDestOverlay is valid. }
  DDSD_CKDESTOVERLAY = $00002000;

{ ddckCKDestBlt is valid. }
  DDSD_CKDESTBLT = $00004000;

{ ddckCKSrcOverlay is valid. }
  DDSD_CKSRCOVERLAY = $00008000;

{ ddckCKSrcBlt is valid. }
  DDSD_CKSRCBLT = $00010000;

{ dwMipMapCount is valid. }
  DDSD_MIPMAPCOUNT = $00020000;

{ dwRefreshRate is valid. }
  DDSD_REFRESHRATE = $00040000;

{ dwLinearSize is valid. }
  DDSD_LINEARSIZE = $00080000;

{ dwTextureStage is valid. }
  DDSD_TEXTURESTAGE = $00100000;

{ dwFVF is valid. }
  DDSD_FVF = $00200000;

{ dwSrcVBHandle is valid. }
  DDSD_SRCVBHANDLE = $00400000;

{ dwDepth is valid. }
  DDSD_DEPTH = $00800000;

{ All input fields are valid. }
  DDSD_ALL = $00FFF9EE;

{ guid field is valid. }
  DDOSD_GUID = $00000001;

{ dwCompressionRatio field is valid. }
  DDOSD_COMPRESSION_RATIO = $00000002;

{ ddSCaps field is valid. }
  DDOSD_SCAPS = $00000004;

{ ddOSCaps field is valid. }
  DDOSD_OSCAPS = $00000008;

{ All input fields are valid. }
  DDOSD_ALL = $0000000F;

{ The surface's optimized pixelformat is compressed. }
  DDOSDCAPS_OPTCOMPRESSED = $00000001;

{ The surface's optimized pixelformat is reordered. }
  DDOSDCAPS_OPTREORDERED = $00000002;

{ The opt surface is a monolithic mipmap. }
  DDOSDCAPS_MONOLITHICMIPMAP = $00000004;

{ The valid Surf caps:
    DDSCAPS_SYSTEMMEMORY = $00000800
    DDSCAPS_VIDEOMEMORY = $00004000
    DDSCAPS_LOCALVIDMEM = $10000000
    DDSCAPS_NONLOCALVIDMEM = $20000000 }
  DDOSDCAPS_VALIDSCAPS = $30004800;

{ The valid OptSurf caps }
  DDOSDCAPS_VALIDOSCAPS = $00000007;

{ lBrightness field is valid. }
  DDCOLOR_BRIGHTNESS = $00000001;

{ lContrast field is valid. }
  DDCOLOR_CONTRAST = $00000002;

{ lHue field is valid. }
  DDCOLOR_HUE = $00000004;

{ lSaturation field is valid. }
  DDCOLOR_SATURATION = $00000008;

{ lSharpness field is valid. }
  DDCOLOR_SHARPNESS = $00000010;

{ lGamma field is valid. }
  DDCOLOR_GAMMA = $00000020;

{ lColorEnable field is valid. }
  DDCOLOR_COLORENABLE = $00000040;

{ Direct Draw Capability Flags

  These flags are used to describe the capabilities of a given Surface.
  All flags are bit flags. }

{ DIRECTDRAWSURFACE CAPABILITY FLAGS }

{ This bit is reserved. It should not be specified. }
  DDSCAPS_RESERVED1 = $00000001;

{ Indicates that this surface contains alpha-only information.
  (To determine if a surface is RGBA/YUVA, the pixel format must be interrogated.) }
  DDSCAPS_ALPHA = $00000002;

{ Indicates that this surface is a backbuffer. It is generally set by CreateSurface when the DDSCAPS_FLIP capability
  bit is set. It indicates that this surface is THE back buffer of a surface flipping structure. DirectDraw supports
  N surfaces in a surface flipping structure. Only the surface that immediately precedeces the DDSCAPS_FRONTBUFFER
  has this capability bit set. The other surfaces are identified as back buffers by the presence of the DDSCAPS_FLIP
  capability, their attachment order, and the absence of the DDSCAPS_FRONTBUFFER and DDSCAPS_BACKBUFFER capabilities.
  The bit is sent to CreateSurface when a standalone back buffer is being created. This surface could be attached to a
  front buffer and/or back buffers to form a flipping surface structure after the CreateSurface call.
  See AddAttachments for a detailed description of the behaviors in this case. }
  DDSCAPS_BACKBUFFER = $00000004;

{ Indicates a complex surface structure is being described. A complex surface structure results in the creation of
  more than one surface. The additional surfaces are attached to the root surface. The complex structure can only be
  destroyed by destroying the root. }
  DDSCAPS_COMPLEX = $00000008;

{ Indicates that this surface is a part of a surface flipping structure. When it is passed to CreateSurface the
  DDSCAPS_FRONTBUFFER and DDSCAP_BACKBUFFER bits are not set. They are set by CreateSurface * on the resulting
  creations. The dwBackBufferCount field in the DDSURFACEDESC structure must be set to at least 1 in order for the
  CreateSurface call to succeed. The DDSCAPS_COMPLEX capability must always be set with creating multiple surfaces
  through CreateSurface. }
  DDSCAPS_FLIP = $00000010;

{ Indicates that this surface is THE front buffer of a surface flipping structure. It is generally set by
  CreateSurface when the DDSCAPS_FLIP capability bit is set. If this capability is sent to CreateSurface then a
  standalonw front buffer is created. This surface will not have the DDSCAPS_FLIP capability. It can be attached to
  other back buffers to form a flipping structure. See AddAttachments for a detailed description of the behaviors in
  this case. }
  DDSCAPS_FRONTBUFFER = $00000020;

{ Indicates that this surface is any offscreen surface that is not an overlay, texture, zbuffer, front buffer, back
  buffer, or alpha surface.  It is used to identify plain vanilla surfaces. }
  DDSCAPS_OFFSCREENPLAIN = $00000040;

{ Indicates that this surface is an overlay. It may or may not be directly visible depending on whether or not it is
  currently being overlayed onto the primary surface. DDSCAPS_VISIBLE can be used to determine whether or not it is
  being overlayed at the moment. }
  DDSCAPS_OVERLAY = $00000080;

{ Indicates that unique DirectDrawPalette objects can be created and attached to this surface. }
  DDSCAPS_PALETTE = $00000100;

{ Indicates that this surface is the primary surface.
  The primary surface represents what the user is seeing at the moment. }
  DDSCAPS_PRIMARYSURFACE = $00000200;

{ This flag used to be DDSCAPS_PRIMARYSURFACELEFT, which is now obsolete. }
  DDSCAPS_RESERVED3 = $00000400;
  DDSCAPS_PRIMARYSURFACELEFT = $00000000;

{ Indicates that this surface memory was allocated in system memory. }
  DDSCAPS_SYSTEMMEMORY = $00000800;

{ Indicates that this surface can be used as a 3D texture. It does not indicate whether or not the surface is being
  used for that purpose. }
  DDSCAPS_TEXTURE = $00001000;

{ Indicates that a surface may be a destination for 3D rendering. This bit must be set in order to query for a
  Direct3D Device Interface from this surface. }
  DDSCAPS_3DDEVICE = $00002000;

{ Indicates that this surface exists in video memory. }
  DDSCAPS_VIDEOMEMORY = $00004000;

{ Indicates that changes made to this surface are immediately visible. It is always set for the primary surface and
  is set for overlays while they are being overlayed and texture maps while they are being textured. }
  DDSCAPS_VISIBLE = $00008000;

{ Indicates that only writes are permitted to the surface. Read accesses from the surface may or may not generate a
  protection fault, but the results of a read from this surface will not be meaningful.  READ ONLY. }
  DDSCAPS_WRITEONLY = $00010000;

{ Indicates that this surface is a z buffer. A z buffer does not contain displayable information. Instead it contains
  bit depth information that is used to determine which pixels are visible and which are obscured. }
  DDSCAPS_ZBUFFER = $00020000;

{ Indicates surface will have a DC associated long term. }
  DDSCAPS_OWNDC = $00040000;

{ Indicates surface should be able to receive live video. }
  DDSCAPS_LIVEVIDEO = $00080000;

{ Indicates surface should be able to have a stream decompressed to it by the hardware. }
  DDSCAPS_HWCODEC = $00100000;

{ Surface is a ModeX surface. }
  DDSCAPS_MODEX = $00200000;

{ Indicates surface is one level of a mip-map. This surface will be attached to other DDSCAPS_MIPMAP surfaces to form
  the mip-map. This can be done explicitly, by creating a number of surfaces and attaching them with AddAttachedSurface
  or by implicitly by CreateSurface. If this bit is set then DDSCAPS_TEXTURE must also be set. }
  DDSCAPS_MIPMAP = $00400000;

{ This bit is reserved. It should not be specified. }
  DDSCAPS_RESERVED2 = $00800000;

{ Indicates that memory for the surface is not allocated until the surface is loaded (via the Direct3D texture Load()
  function). }
  DDSCAPS_ALLOCONLOAD = $04000000;

{ Indicates that the surface will recieve data from a video port. }
  DDSCAPS_VIDEOPORT = $08000000;

{ Indicates that a video memory surface is resident in true, local video memory rather than non-local video memory.
  If this flag is specified then so must DDSCAPS_VIDEOMEMORY. This flag is mutually exclusive with
  DDSCAPS_NONLOCALVIDMEM. }
  DDSCAPS_LOCALVIDMEM = $10000000;

{ Indicates that a video memory surface is resident in non-local video memory rather than true, local video memory.
  If this flag is specified then so must DDSCAPS_VIDEOMEMORY. This flag is mutually exclusive with DDSCAPS_LOCALVIDMEM. }
  DDSCAPS_NONLOCALVIDMEM = $20000000;

{ Indicates that this surface is a standard VGA mode surface, and not a ModeX surface.
  (This flag will never be set in combination with the DDSCAPS_MODEX flag). }
  DDSCAPS_STANDARDVGAMODE = $40000000;

{ Indicates that this surface will be an optimized surface. This flag is currently only valid in conjunction with the
  DDSCAPS_TEXTURE flag. The surface will be created without any underlying video memory until loaded. }
  DDSCAPS_OPTIMIZED = $80000000;

{ This bit is reserved }
  DDSCAPS2_RESERVED4 = $00000002;
  DDSCAPS2_HARDWAREDEINTERLACE = $00000000;

{ Indicates to the driver that this surface will be locked very frequently (for procedural textures, dynamic
  lightmaps, etc). Surfaces with this cap set must also have DDSCAPS_TEXTURE. This cap cannot be used with
  DDSCAPS2_HINTSTATIC and DDSCAPS2_OPAQUE. }
  DDSCAPS2_HINTDYNAMIC = $00000004;

{ Indicates to the driver that this surface can be re-ordered/retiled on load. This operation will not change the size
  of the texture. It is relatively fast and symmetrical, since the application may lock these bits (although it will
  take a performance hit when doing so). Surfaces with this cap set must also have DDSCAPS_TEXTURE. This cap cannot be
  used with DDSCAPS2_HINTDYNAMIC and DDSCAPS2_OPAQUE. }
  DDSCAPS2_HINTSTATIC = $00000008;

{ Indicates that the client would like this texture surface to be managed by the DirectDraw/Direct3D runtime.
  Surfaces with this cap set must also have DDSCAPS_TEXTURE set. }
  DDSCAPS2_TEXTUREMANAGE = $00000010;

{ These bits are reserved for internal use. }
  DDSCAPS2_RESERVED1 = $00000020;
  DDSCAPS2_RESERVED2 = $00000040;

{ Indicates to the driver that this surface will never be locked again. The driver is free to optimize this surface
  via retiling and actual compression. All calls to Lock() or Blts from this surface will fail. Surfaces with this
  cap set must also have DDSCAPS_TEXTURE. This cap cannot be used with DDSCAPS2_HINTDYNAMIC and DDSCAPS2_HINTSTATIC. }
  DDSCAPS2_OPAQUE = $00000080;

{ Applications should set this bit at CreateSurface time to indicate that they intend to use antialiasing.
  Only valid if DDSCAPS_3DDEVICE is also set. }
  DDSCAPS2_HINTANTIALIASING = $00000100;

{ This flag is used at CreateSurface time to indicate that this set of surfaces is a cubic environment map. }
  DDSCAPS2_CUBEMAP = $00000200;

{ These flags preform two functions:
  - At CreateSurface time, they define which of the six cube faces are required by the application.
  - After creation, each face in the cubemap will have exactly one of these bits set. }
  DDSCAPS2_CUBEMAP_POSITIVEX = $00000400;
  DDSCAPS2_CUBEMAP_NEGATIVEX = $00000800;
  DDSCAPS2_CUBEMAP_POSITIVEY = $00001000;
  DDSCAPS2_CUBEMAP_NEGATIVEY = $00002000;
  DDSCAPS2_CUBEMAP_POSITIVEZ = $00004000;
  DDSCAPS2_CUBEMAP_NEGATIVEZ = $00008000;

{ This macro may be used to specify all faces of a cube map at CreateSurface time. }
  DDSCAPS2_CUBEMAP_ALLFACES = DDSCAPS2_CUBEMAP_POSITIVEX or DDSCAPS2_CUBEMAP_NEGATIVEX or
    DDSCAPS2_CUBEMAP_POSITIVEY or DDSCAPS2_CUBEMAP_NEGATIVEY or DDSCAPS2_CUBEMAP_POSITIVEZ or
    DDSCAPS2_CUBEMAP_NEGATIVEZ;

{ This flag is an additional flag which is present on mipmap sublevels from DX7 onwards. It enables easier use of
  GetAttachedSurface rather than EnumAttachedSurfaces for surface constructs such as Cube Maps, wherein there are
  more than one mipmap surface attached to the root surface. This caps bit is ignored by CreateSurface. }
  DDSCAPS2_MIPMAPSUBLEVEL = $00010000;

{ This flag indicates that the texture should be managed by D3D only. }
  DDSCAPS2_D3DTEXTUREMANAGE = $00020000;

{ This flag indicates that the managed surface can be safely lost. }
  DDSCAPS2_DONOTPERSIST = $00040000;

{ This flag indicates that this surface is part of a stereo flipping chain. }
  DDSCAPS2_STEREOSURFACELEFT = $00080000;

{ Indicates that the surface is a volume. Can be combined with DDSCAPS_MIPMAP to indicate a multi-level volume. }
  DDSCAPS2_VOLUME = $00200000;

{ Indicates that the surface may be locked multiple times by the application.
  This cap cannot be used with DDSCAPS2_OPAQUE. }
  DDSCAPS2_NOTUSERLOCKABLE = $00400000;

{ Indicates that the vertex buffer data can be used to render points and point sprites. }
  DDSCAPS2_POINTS = $00800000;

{ Indicates that the vertex buffer data can be used to render rt pactches. }
  DDSCAPS2_RTPATCHES = $01000000;

{ Indicates that the vertex buffer data can be used to render n patches. }
  DDSCAPS2_NPATCHES = $02000000;

{ This bit is reserved for internal use. }
  DDSCAPS2_RESERVED3 = $04000000;

{ Indicates that the contents of the backbuffer do not have to be preserved the contents of the backbuffer after they
  are presented. }
  DDSCAPS2_DISCARDBACKBUFFER = $10000000;

{ Indicates that all surfaces in this creation chain should be given an alpha channel. This flag will be set on
  primary surface chains that may have no explicit pixel format  (and thus take on the format of the current display
  mode). The driver should infer that all these surfaces have a format having an alpha channel.
  (e.g. assume D3DFMT_A8R8G8B8 if the display mode is x888.) }
  DDSCAPS2_ENABLEALPHACHANNEL = $20000000;

{ Indicates that all surfaces in this creation chain is extended primary surface format. This flag will be set on
  extended primary surface chains that always have explicit pixel format and the pixel format is typically GDI
  (Graphics Device Interface) couldn't handle, thus only used with fullscreen application.
  (e.g. D3DFMT_A2R10G10B10 format) }
  DDSCAPS2_EXTENDEDFORMATPRIMARY = $40000000;

{ Indicates that all surfaces in this creation chain is additional primary surface. This flag will be set on primary
  surface chains which must present on the adapter id provided on dwCaps4. Typically this will be used to create
  secondary primary surface on DualView display adapter. }
  DDSCAPS2_ADDITIONALPRIMARY = $80000000;

{ This is a mask that indicates the set of bits that may be set at createsurface time to indicate number of samples
  per pixel when multisampling. }
  DDSCAPS3_MULTISAMPLE_MASK = $0000001F;

{ This is a mask that indicates the set of bits that may be set at createsurface time to indicate the quality level
  of rendering for the current number of samples per pixel. }
  DDSCAPS3_MULTISAMPLE_QUALITY_MASK = $000000E0;
  DDSCAPS3_MULTISAMPLE_QUALITY_SHIFT = 5;

{ This bit is reserved for internal use. }
  DDSCAPS3_RESERVED1 = $00000100;

{ This bit is reserved for internal use. }
  DDSCAPS3_RESERVED2 = $00000200;

{ This indicates whether this surface has light-weight miplevels. }
  DDSCAPS3_LIGHTWEIGHTMIPMAP = $00000400;

{ This indicates that the mipsublevels for this surface are auto-generated. }
  DDSCAPS3_AUTOGENMIPMAP = $00000800;

  DDSCAPS3_DMAP = $00001000;

{ D3D9Ex only }

{ This indicates that this surface is to be shared by processes. }
  DDSCAPS3_CREATESHAREDRESOURCE = $00002000;

{ This indicates that this surface need to be initialized before being shared, this bit implies that this surface is
  read only after initialization absence of this bit implies that this surface allows both read and write. }
  DDSCAPS3_READONLYRESOURCE = $00004000;

{ This indicates that this surface is to share an existing video memory with another surface created with
  DDSCAPS3_CREATESHAREDRESOURCE, This bit is never used with DDSCAPS3_CREATESHAREDRESOURCE. }
  DDSCAPS3_OPENSHAREDRESOURCE = $00008000;

{ DIRECTDRAW DRIVER CAPABILITY FLAGS }

{ Display hardware has 3D acceleration. }
  DDCAPS_3D = $00000001;

{ Indicates that DirectDraw will support only dest rectangles that are aligned on DIRECTDRAWCAPS.dwAlignBoundaryDest
  boundaries of the surface, respectively. READ ONLY. }
  DDCAPS_ALIGNBOUNDARYDEST = $00000002;

{ Indicates that DirectDraw will support only source rectangles  whose sizes in BYTEs are
  DIRECTDRAWCAPS.dwAlignSizeDest multiples, respectively. READ ONLY. }
  DDCAPS_ALIGNSIZEDEST = $00000004;

{ Indicates that DirectDraw will support only source rectangles that are aligned on DIRECTDRAWCAPS.dwAlignBoundarySrc
  boundaries of the surface, respectively. READ ONLY. }
  DDCAPS_ALIGNBOUNDARYSRC = $00000008;

{ Indicates that DirectDraw will support only source rectangles whose sizes in BYTEs are DIRECTDRAWCAPS.dwAlignSizeSrc
  multiples, respectively. READ ONLY. }
  DDCAPS_ALIGNSIZESRC = $00000010;

{ Indicates that DirectDraw will create video memory surfaces that have a stride alignment equal to
  DIRECTDRAWCAPS.dwAlignStride. READ ONLY. }
  DDCAPS_ALIGNSTRIDE = $00000020;

{ Display hardware is capable of blt operations. }
  DDCAPS_BLT = $00000040;

{ Display hardware is capable of asynchronous blt operations. }
  DDCAPS_BLTQUEUE = $00000080;

{ Display hardware is capable of color space conversions during the blt operation. }
  DDCAPS_BLTFOURCC = $00000100;

{ Display hardware is capable of stretching during blt operations. }
  DDCAPS_BLTSTRETCH = $00000200;

{ Display hardware is shared with GDI. }
  DDCAPS_GDI = $00000400;

{ Display hardware can overlay. }
  DDCAPS_OVERLAY = $00000800;

{ Set if display hardware supports overlays but can not clip them. }
  DDCAPS_OVERLAYCANTCLIP = $00001000;

{ Indicates that overlay hardware is capable of color space conversions during the overlay operation.}
  DDCAPS_OVERLAYFOURCC = $00002000;

{ Indicates that stretching can be done by the overlay hardware. }
  DDCAPS_OVERLAYSTRETCH = $00004000;

{ Indicates that unique DirectDrawPalettes can be created for DirectDrawSurfaces other than the primary surface.}
  DDCAPS_PALETTE = $00008000;

{ Indicates that palette changes can be syncd with the veritcal refresh. }
  DDCAPS_PALETTEVSYNC = $00010000;

{ Display hardware can return the current scan line. }
  DDCAPS_READSCANLINE = $00020000;

{ This flag used to be DDCAPS_STEREOVIEW, which is now obsolete }
  DDCAPS_RESERVED1 = $00040000;

{ Display hardware is capable of generating a vertical blank interrupt. }
  DDCAPS_VBI = $00080000;

{ Supports the use of z buffers with blt operations. }
  DDCAPS_ZBLTS = $00100000;

{ Supports Z Ordering of overlays. }
  DDCAPS_ZOVERLAYS = $00200000;

{ Supports color key. }
  DDCAPS_COLORKEY = $00400000;

{ Supports alpha surfaces. }
  DDCAPS_ALPHA = $00800000;

{ Colorkey is hardware assisted (DDCAPS_COLORKEY will also be set). }
  DDCAPS_COLORKEYHWASSIST = $01000000;

{ No hardware support at all. }
  DDCAPS_NOHARDWARE = $02000000;

{ Display hardware is capable of color fill with bltter. }
  DDCAPS_BLTCOLORFILL = $04000000;

{ Display hardware is bank switched, and potentially very slow at random access to VRAM. }
  DDCAPS_BANKSWITCHED = $08000000;

{ Display hardware is capable of depth filling Z-buffers with bltter. }
  DDCAPS_BLTDEPTHFILL = $10000000;

{ Display hardware is capable of clipping while bltting. }
  DDCAPS_CANCLIP = $20000000;

{ Display hardware is capable of clipping while stretch bltting. }
  DDCAPS_CANCLIPSTRETCHED = $40000000;

{ Display hardware is capable of bltting to or from system memory. }
  DDCAPS_CANBLTSYSMEM = $80000000;

{ MORE DIRECTDRAW DRIVER CAPABILITY FLAGS (dwCaps2) }

{ Display hardware is certified. }
  DDCAPS2_CERTIFIED = $00000001;

{ Driver cannot interleave 2D operations (lock and blt) to surfaces with Direct3D rendering operations between calls
  to BeginScene() and EndScene(). }
  DDCAPS2_NO2DDURING3DSCENE = $00000002;

{ Display hardware contains a video port. }
  DDCAPS2_VIDEOPORT = $00000004;

{ The overlay can be automatically flipped according to the video port VSYNCs, providing automatic doubled buffered
  display of video port data using an overlay. }
  DDCAPS2_AUTOFLIPOVERLAY = $00000008;

{ Overlay can display each field of interlaced data individually while it is interleaved in memory without causing
  jittery artifacts. }
  DDCAPS2_CANBOBINTERLEAVED = $00000010;

{ Overlay can display each field of interlaced data individually while it is not interleaved in memory without
  causing jittery artifacts. }
  DDCAPS2_CANBOBNONINTERLEAVED = $00000020;

{ The overlay surface contains color controls (brightness, sharpness, etc.) }
  DDCAPS2_COLORCONTROLOVERLAY = $00000040;

{ The primary surface contains color controls (gamma, etc.) }
  DDCAPS2_COLORCONTROLPRIMARY = $00000080;

{ RGBZ -> RGB supported for 16:16 RGB:Z }
  DDCAPS2_CANDROPZ16BIT = $00000100;

{ Driver supports non-local video memory. }
  DDCAPS2_NONLOCALVIDMEM = $00000200;

{ Dirver supports non-local video memory but has different capabilities for non-local video memory surfaces.
  If this bit is set then so must DDCAPS2_NONLOCALVIDMEM. }
  DDCAPS2_NONLOCALVIDMEMCAPS = $00000400;

{ Driver neither requires nor prefers surfaces to be pagelocked when performing blts involving system memory surfaces. }
  DDCAPS2_NOPAGELOCKREQUIRED = $00000800;

{ Driver can create surfaces which are wider than the primary surface. }
  DDCAPS2_WIDESURFACES = $00001000;

{ Driver supports bob without using a video port by handling the DDFLIP_ODD and DDFLIP_EVEN flags specified in Flip. }
  DDCAPS2_CANFLIPODDEVEN = $00002000;

{ Driver supports bob using hardware. }
  DDCAPS2_CANBOBHARDWARE = $00004000;

{ Driver supports bltting any FOURCC surface to another surface of the same FOURCC. }
  DDCAPS2_COPYFOURCC = $00008000;

{ Driver supports loadable gamma ramps for the primary surface. }
  DDCAPS2_PRIMARYGAMMA = $00020000;

{ Driver can render in windowed mode. }
  DDCAPS2_CANRENDERWINDOWED = $00080000;

{ A calibrator is available to adjust the gamma ramp according to the physical display properties so that the result
  will be identical on all calibrated systems. }
  DDCAPS2_CANCALIBRATEGAMMA = $00100000;

{ Indicates that the driver will respond to DDFLIP_INTERVALn flags. }
  DDCAPS2_FLIPINTERVAL = $00200000;

{ Indicates that the driver will respond to DDFLIP_NOVSYNC. }
  DDCAPS2_FLIPNOVSYNC = $00400000;

{ Driver supports management of video memory, if this flag is ON, driver manages the texture if requested with
  DDSCAPS2_TEXTUREMANAGE on DirectX manages the texture if this flag is OFF and surface has DDSCAPS2_TEXTUREMANAGE on. }
  DDCAPS2_CANMANAGETEXTURE = $00800000;

{ The Direct3D texture manager uses this cap to decide whether to put managed surfaces in non-local video memory.
  If the cap is set, the texture manager will put managed surfaces in non-local vidmem. Drivers that cannot texture
  from local vidmem SHOULD NOT set this cap. }
  DDCAPS2_TEXMANINNONLOCALVIDMEM = $01000000;

{ Indicates that the driver supports DX7 type of stereo in at least one mode (which may not necessarily be the current
  mode). Applications should use IDirectDraw7 (or higher) ::EnumDisplayModes and check the DDSURFACEDESC.ddsCaps.dwCaps2
  field for the presence of DDSCAPS2_STEREOSURFACELEFT to check if a particular mode supports stereo. The application
  can also use IDirectDraw7(or higher)::GetDisplayMode to check the current mode. }
  DDCAPS2_STEREO = $02000000;

{ This caps bit is intended for internal DirectDraw use.
  - It is only valid if DDCAPS2_NONLOCALVIDMEMCAPS is set.
  - If this bit is set, then DDCAPS_CANBLTSYSMEM MUST be set by the driver (and all the assoicated system memory blt
    caps must be correct).
  - It implies that the system->video blt caps in DDCAPS also apply to system to nonlocal blts. I.e. the dwSVBCaps,
    dwSVBCKeyCaps, dwSVBFXCaps and dwSVBRops members of DDCAPS (DDCORECAPS) are filled in correctly.
  - Any blt from system to nonlocal memory that matches these caps bits will be passed to the driver.

  NOTE: This is intended to enable the driver itself to do efficient reordering of textures. This is NOT meant to imply
  that hardware can write into AGP memory. This operation is not currently supported. }
  DDCAPS2_SYSTONONLOCAL_AS_SYSTOLOCAL = $04000000;

{ was DDCAPS2_PUREHAL }
  DDCAPS2_RESERVED1 = $08000000;

{ Driver supports management of video memory, if this flag is ON, driver manages the resource if requested with
  DDSCAPS2_TEXTUREMANAGE on DirectX manages the resource if this flag is OFF and surface has DDSCAPS2_TEXTUREMANAGE on. }
  DDCAPS2_CANMANAGERESOURCE = $10000000;

{ Driver supports dynamic textures. This will allow the application to set D3DUSAGE_DYNAMIC (DDSCAPS2_HINTDYNAMIC for
  drivers) at texture create time. Video memory dynamic textures WILL be lockable by applications. It is expected that
  these locks will be very efficient (which implies that the driver should always maintain a linear copy, a pointer to
  which can be quickly handed out to the application). }
  DDCAPS2_DYNAMICTEXTURES = $20000000;

{ Driver supports auto-generation of mipmaps. }
  DDCAPS2_CANAUTOGENMIPMAP = $40000000;

{ D3D9Ex only}

{ Driver supports sharing of cross process resouces. }
  DDCAPS2_CANSHARERESOURCE = $80000000;

{ DIRECTDRAW FX ALPHA CAPABILITY FLAGS }

{ Supports alpha blending around the edge of a source color keyed surface.
  For Blt. }
  DDFXALPHACAPS_BLTALPHAEDGEBLEND = $00000001;

{ Supports alpha information in the pixel format. The bit depth of alpha information in the pixel format can be 1,2,4,
  or 8. The alpha value becomes more opaque as the alpha value increases. (0 is transparent.)
  For Blt. }
  DDFXALPHACAPS_BLTALPHAPIXELS = $00000002;

{ Supports alpha information in the pixel format. The bit depth of alpha information in the pixel format can be 1,2,4,
  or 8. The alpha value becomes more transparent as the alpha value increases. (0 is opaque.)
  This flag can only be set if DDCAPS_ALPHA is set.
  For Blt. }
  DDFXALPHACAPS_BLTALPHAPIXELSNEG = $00000004;

{ Supports alpha only surfaces. The bit depth of an alpha only surface can be 1,2,4, or 8. The alpha value becomes more
  opaque as the alpha value increases. (0 is transparent.)
  For Blt. }
  DDFXALPHACAPS_BLTALPHASURFACES = $00000008;

{ The depth of the alpha channel data can range can be 1,2,4, or 8. The NEG suffix indicates that this alpha channel
  becomes more transparent as the alpha value increases. (0 is opaque.)
  This flag can only be set if DDCAPS_ALPHA is set.
  For Blt. }
  DDFXALPHACAPS_BLTALPHASURFACESNEG = $00000010;

{ Supports alpha blending around the edge of a source color keyed surface.
  For Overlays. }
  DDFXALPHACAPS_OVERLAYALPHAEDGEBLEND = $00000020;

{ Supports alpha information in the pixel format. The bit depth of alpha information in the pixel format can be 1,2,4,
  or 8. The alpha value becomes more opaque as the alpha value increases. (0 is transparent.)
  For Overlays. }
  DDFXALPHACAPS_OVERLAYALPHAPIXELS = $00000040;

{ Supports alpha information in the pixel format. The bit depth of alpha information in the pixel format can be 1,2,4,
  or 8. The alpha value becomes more transparent as the alpha value increases. (0 is opaque.)
  This flag can only be set if DDCAPS_ALPHA is set.
  For Overlays. }
  DDFXALPHACAPS_OVERLAYALPHAPIXELSNEG = $00000080;

{ Supports alpha only surfaces. The bit depth of an alpha only surface can be 1,2,4, or 8. The alpha value becomes more
  opaque as the alpha value increases. (0 is transparent.)
  For Overlays. }
  DDFXALPHACAPS_OVERLAYALPHASURFACES = $00000100;

{ The depth of the alpha channel data can range can be 1,2,4, or 8. The NEG suffix indicates that this alpha channel
  becomes more transparent as the alpha value increases. (0 is opaque.)
  This flag can only be set if DDCAPS_ALPHA is set.
  For Overlays. }
  DDFXALPHACAPS_OVERLAYALPHASURFACESNEG = $00000200;

{ DIRECTDRAW FX CAPABILITY FLAGS }

{ Uses arithmetic operations to stretch and shrink surfaces during blt rather than pixel doubling techniques.
  Along the Y axis. }
  DDFXCAPS_BLTARITHSTRETCHY = $00000020;

{ Uses arithmetic operations to stretch during blt rather than pixel doubling techniques.
  Along the Y axis. Only works for x1, x2, etc. }
  DDFXCAPS_BLTARITHSTRETCHYN = $00000010;

{ Supports mirroring left to right in blt. }
  DDFXCAPS_BLTMIRRORLEFTRIGHT = $00000040;

{ Supports mirroring top to bottom in blt. }
  DDFXCAPS_BLTMIRRORUPDOWN = $00000080;

{ Supports arbitrary rotation for blts. }
  DDFXCAPS_BLTROTATION = $00000100;

{ Supports 90 degree rotations for blts. }
  DDFXCAPS_BLTROTATION90 = $00000200;

{ DirectDraw supports arbitrary shrinking of a surface along the x axis (horizontal direction) for blts. }
  DDFXCAPS_BLTSHRINKX = $00000400;

{ DirectDraw supports integer shrinking (1x,2x,) of a surface along the x axis (horizontal direction) for blts. }
  DDFXCAPS_BLTSHRINKXN = $00000800;

{ DirectDraw supports arbitrary shrinking of a surface along the y axis (horizontal direction) for blts. }
  DDFXCAPS_BLTSHRINKY = $00001000;

{ DirectDraw supports integer shrinking (1x,2x,) of a surface along the y axis (vertical direction) for blts. }
  DDFXCAPS_BLTSHRINKYN = $00002000;

{ DirectDraw supports arbitrary stretching of a surface along the x axis (horizontal direction) for blts. }
  DDFXCAPS_BLTSTRETCHX = $00004000;

{ DirectDraw supports integer stretching (1x,2x,) of a surface along the x axis (horizontal direction) for blts. }
  DDFXCAPS_BLTSTRETCHXN = $00008000;

{ DirectDraw supports arbitrary stretching of a surface along the y axis (horizontal direction) for blts. }
  DDFXCAPS_BLTSTRETCHY = $00010000;

{ DirectDraw supports integer stretching (1x,2x,) of a surface along the y axis (vertical direction) for blts. }
  DDFXCAPS_BLTSTRETCHYN = $00020000;

{ Uses arithmetic operations to stretch and shrink surfaces during overlay rather than pixel doubling techniques.
  Along the Y axis for overlays. }
  DDFXCAPS_OVERLAYARITHSTRETCHY = $00040000;

{ Uses arithmetic operations to stretch surfaces during overlay rather than pixel doubling techniques.
  Along the Y axis for overlays. Only works for x1, x2, etc. }
  DDFXCAPS_OVERLAYARITHSTRETCHYN = $00000008;

{ DirectDraw supports arbitrary shrinking of a surface along the x axis (horizontal direction) for overlays. }
  DDFXCAPS_OVERLAYSHRINKX = $00080000;

{ DirectDraw supports integer shrinking (1x,2x,) of a surface along the x axis (horizontal direction) for overlays. }
  DDFXCAPS_OVERLAYSHRINKXN = $00100000;

{ DirectDraw supports arbitrary shrinking of a surface along the y axis (horizontal direction) for overlays. }
  DDFXCAPS_OVERLAYSHRINKY = $00200000;

{ DirectDraw supports integer shrinking (1x,2x,) of a surface along the y axis (vertical direction) for overlays. }
  DDFXCAPS_OVERLAYSHRINKYN = $00400000;

{ DirectDraw supports arbitrary stretching of a surface along the x axis (horizontal direction) for overlays. }
  DDFXCAPS_OVERLAYSTRETCHX = $00800000;

{ DirectDraw supports integer stretching (1x,2x,) of a surface along the x axis (horizontal direction) for overlays. }
  DDFXCAPS_OVERLAYSTRETCHXN = $01000000;

{ DirectDraw supports arbitrary stretching of a surface along the y axis (horizontal direction) for overlays. }
  DDFXCAPS_OVERLAYSTRETCHY = $02000000;

{ DirectDraw supports integer stretching (1x,2x,) of a surface along the y axis (vertical direction) for overlays. }
  DDFXCAPS_OVERLAYSTRETCHYN = $04000000;

{ DirectDraw supports mirroring of overlays across the vertical axis. }
  DDFXCAPS_OVERLAYMIRRORLEFTRIGHT = $08000000;

{ DirectDraw supports mirroring of overlays across the horizontal axis. }
  DDFXCAPS_OVERLAYMIRRORUPDOWN = $10000000;

{ DirectDraw supports deinterlacing of overlay surfaces. }
  DDFXCAPS_OVERLAYDEINTERLACE = $20000000;

{ Driver can do alpha blending for blits. }
  DDFXCAPS_BLTALPHA = $00000001;

{ Driver can do surface-reconstruction filtering for warped blits. }
  DDFXCAPS_BLTFILTER = DDFXCAPS_BLTARITHSTRETCHY;

{ Driver can do alpha blending for overlays. }
  DDFXCAPS_OVERLAYALPHA = $00000004;

{ Driver can do surface-reconstruction filtering for warped overlays. }
  DDFXCAPS_OVERLAYFILTER = DDFXCAPS_OVERLAYARITHSTRETCHY;

{ DIRECTDRAW STEREO VIEW CAPABILITIES }

{ This flag used to be DDSVCAPS_ENIGMA, which is now obsolete. }
  DDSVCAPS_RESERVED1 = $00000001;

{ This flag used to be DDSVCAPS_FLICKER, which is now obsolete. }
  DDSVCAPS_RESERVED2 = $00000002;

{ This flag used to be DDSVCAPS_REDBLUE, which is now obsolete. }
  DDSVCAPS_RESERVED3 = $00000004;

{ This flag used to be DDSVCAPS_SPLIT, which is now obsolete. }
  DDSVCAPS_RESERVED4 = $00000008;

{ The stereo view is accomplished with switching technology. }
  DDSVCAPS_STEREOSEQUENTIAL = $00000010;

{ DIRECTDRAWPALETTE CAPABILITIES }

{ Index is 4 bits.  There are sixteen color entries in the palette table. }
  DDPCAPS_4BIT = $00000001;

{ Index is onto a 8 bit color index.  =This field is only valid with the DDPCAPS_1BIT, DDPCAPS_2BIT or DDPCAPS_4BIT
  capability and the target surface is in 8bpp. Each color entry is one byte long and is an index into destination
  surface's 8bpp palette. }
  DDPCAPS_8BITENTRIES = $00000002;

{ Index is 8 bits.  There are 256 color entries in the palette table. }
  DDPCAPS_8BIT = $00000004;

{ Indicates that this DIRECTDRAWPALETTE should use the palette color array passed into the lpDDColorArray parameter to
  initialize the DIRECTDRAWPALETTE object. This flag is obsolete. DirectDraw always initializes the color array from
  the lpDDColorArray parameter. The definition remains for source-level compatibility. }
  DDPCAPS_INITIALIZE = $00000000;

{ This palette is the one attached to the primary surface. Changing this table has immediate effect on the display
  unless DDPSETPAL_VSYNC is specified and supported. }
  DDPCAPS_PRIMARYSURFACE = $00000010;

{ This palette is the one attached to the primary surface left. Changing this table has immediate effect on the
  display for the left eye unless DDPSETPAL_VSYNC is specified and supported. }
  DDPCAPS_PRIMARYSURFACELEFT = $00000020;

{ This palette can have all 256 entries defined. }
  DDPCAPS_ALLOW256 = $00000040;

{ This palette can have modifications to it synced with the monitors refresh rate. }
  DDPCAPS_VSYNC = $00000080;

{ Index is 1 bit.  There are two color entries in the palette table. }
  DDPCAPS_1BIT = $00000100;

{ Index is 2 bit.  There are four color entries in the palette table. }
  DDPCAPS_2BIT = $00000200;

{ The peFlags member of PALETTEENTRY denotes an 8 bit alpha value. }
  DDPCAPS_ALPHA = $00000400;

{ DIRECTDRAWSURFACE SETPRIVATEDATA CONSTANTS }

{ The passed pointer is an IUnknown ptr. The cbData argument to SetPrivateData must be set to sizeof(IUnknown*).
  DirectDraw will call AddRef through this pointer and Release when the private data is destroyed. This includes when
  the surface or palette is destroyed before such priovate data is destroyed. }
  DDSPD_IUNKNOWNPOINTER = $00000001;

{ Private data is only valid for the current state of the object, as determined by the uniqueness value. }
  DDSPD_VOLATILE = $00000002;

{ DIRECTDRAW BITDEPTH CONSTANTS }

{ NOTE: These are only used to indicate supported bit depths. These are flags only, they are not to be used as an
  actual bit depth. The absolute numbers 1, 2, 4, 8, 16, 24 and 32 are used to indicate actual bit depths in a
  surface or for changing the display mode. }

{ 1 bit per pixel. }
  DDBD_1 = $00004000;

{ 2 bits per pixel. }
  DDBD_2 = $00002000;

{ 4 bits per pixel. }
  DDBD_4 = $00001000;

{ 8 bits per pixel. }
  DDBD_8 = $00000800;

{ 16 bits per pixel. }
  DDBD_16 = $00000400;

{ 24 bits per pixel. }
  DDBD_24 = $00000200;

{ 32 bits per pixel. }
  DDBD_32 = $00000100;

{ DIRECTDRAWSURFACE SET/GET COLOR KEY FLAGS }

{ Set if the structure contains a color space. Not set if the structure contains a single color key. }
  DDCKEY_COLORSPACE = $00000001;

{ Set if the structure specifies a color key or color space which is to be used as a destination color key for blt
  operations. }
  DDCKEY_DESTBLT = $00000002;

{ Set if the structure specifies a color key or color space which is to be used as a destination color key for overlay
  operations. }
  DDCKEY_DESTOVERLAY = $00000004;

{ Set if the structure specifies a color key or color space which is to be used as a source color key for blt
  operations. }
  DDCKEY_SRCBLT = $00000008;

{ Set if the structure specifies a color key or color space which is to be used as a source color key for overlay
  operations. }
  DDCKEY_SRCOVERLAY = $00000010;

{ DIRECTDRAW COLOR KEY CAPABILITY FLAGS }

{ Supports transparent blting using a color key to identify the replaceable bits of the destination surface for RGB
  colors. }
  DDCKEYCAPS_DESTBLT = $00000001;

{ Supports transparent blting using a color space to identify the replaceable bits of the destination surface for RGB
  colors. }
  DDCKEYCAPS_DESTBLTCLRSPACE = $00000002;

{ Supports transparent blting using a color space to identify the replaceable bits of the destination surface for YUV
  colors. }
  DDCKEYCAPS_DESTBLTCLRSPACEYUV = $00000004;

{ Supports transparent blting using a color key to identify the replaceable bits of the destination surface for YUV
  colors. }
  DDCKEYCAPS_DESTBLTYUV = $00000008;

{ Supports overlaying using colorkeying of the replaceable bits of the surface being overlayed for RGB colors. }
  DDCKEYCAPS_DESTOVERLAY = $00000010;

{ Supports a color space as the color key for the destination for RGB colors. }
  DDCKEYCAPS_DESTOVERLAYCLRSPACE = $00000020;

{ Supports a color space as the color key for the destination for YUV colors. }
  DDCKEYCAPS_DESTOVERLAYCLRSPACEYUV = $00000040;

{ Supports only one active destination color key value for visible overlay surfaces.}
  DDCKEYCAPS_DESTOVERLAYONEACTIVE = $00000080;

{ Supports overlaying using colorkeying of the replaceable bits of the surface being overlayed for YUV colors.}
  DDCKEYCAPS_DESTOVERLAYYUV = $00000100;

{ Supports transparent blting using the color key for the source with this surface for RGB colors. }
  DDCKEYCAPS_SRCBLT = $00000200;

{ Supports transparent blting using a color space for the source with this surface for RGB colors. }
  DDCKEYCAPS_SRCBLTCLRSPACE = $00000400;

{ Supports transparent blting using a color space for the source with this surface for YUV colors. }
  DDCKEYCAPS_SRCBLTCLRSPACEYUV = $00000800;

{ Supports transparent blting using the color key for the source with this surface for YUV colors. }
  DDCKEYCAPS_SRCBLTYUV = $00001000;

{ Supports overlays using the color key for the source with this overlay surface for RGB colors. }
  DDCKEYCAPS_SRCOVERLAY = $00002000;

{ Supports overlays using a color space as the source color key for the overlay surface for RGB colors. }
  DDCKEYCAPS_SRCOVERLAYCLRSPACE = $00004000;

{ Supports overlays using a color space as the source color key for the overlay surface for YUV colors. }
  DDCKEYCAPS_SRCOVERLAYCLRSPACEYUV = $00008000;

{ Supports only one active source color key value for visible overlay surfaces. }
  DDCKEYCAPS_SRCOVERLAYONEACTIVE = $00010000;

{ Supports overlays using the color key for the source with this overlay surface for YUV colors. }
  DDCKEYCAPS_SRCOVERLAYYUV = $00020000;

{ There are no bandwidth trade-offs for using colorkey with an overlay. }
  DDCKEYCAPS_NOCOSTOVERLAY = $00040000;

{ DIRECTDRAW PIXELFORMAT FLAGS }

{ The surface has alpha channel information in the pixel format. }
  DDPF_ALPHAPIXELS = $00000001;

{ The pixel format contains alpha only information. }
  DDPF_ALPHA = $00000002;

{ The FourCC code is valid. }
  DDPF_FOURCC = $00000004;

{ The surface is 4-bit color indexed. }
  DDPF_PALETTEINDEXED4 = $00000008;

{ The surface is indexed into a palette which stores indices into the destination surface's 8-bit palette. }
  DDPF_PALETTEINDEXEDTO8 = $00000010;

{ The surface is 8-bit color indexed. }
  DDPF_PALETTEINDEXED8 = $00000020;

{ The RGB data in the pixel format structure is valid. }
  DDPF_RGB = $00000040;

{ The surface will accept pixel data in the format specified and compress it during the write. }
  DDPF_COMPRESSED = $00000080;

{ The surface will accept RGB data and translate it during the write to YUV data. The format of the data to be written
  will be contained in the pixel format structure. The DDPF_RGB flag will be set. }
  DDPF_RGBTOYUV = $00000100;

{ Pixel format is YUV - YUV data in pixel format struct is valid. }
  DDPF_YUV = $00000200;

{ Pixel format is a z buffer only surface. }
  DDPF_ZBUFFER = $00000400;

{ The surface is 1-bit color indexed. }
  DDPF_PALETTEINDEXED1 = $00000800;

{ The surface is 2-bit color indexed. }
  DDPF_PALETTEINDEXED2 = $00001000;

{ The surface contains Z information in the pixels. }
  DDPF_ZPIXELS = $00002000;

{ The surface contains stencil information along with Z. }
  DDPF_STENCILBUFFER = $00004000;

{ Premultiplied alpha format -- the color components have been premultiplied by the alpha component. }
  DDPF_ALPHAPREMULT = $00008000;

{ Luminance data in the pixel format is valid. Use this flag for luminance-only or luminance+alpha surfaces, the bit
  depth is then ddpf.dwLuminanceBitCount. }
  DDPF_LUMINANCE = $00020000;

{ Luminance data in the pixel format is valid. Use this flag when hanging luminance off bumpmap surfaces, the bit mask
  for the luminance portion of the pixel is then ddpf.dwBumpLuminanceBitMask. }
  DDPF_BUMPLUMINANCE = $00040000;

{ Bump map dUdV data in the pixel format is valid. }
  DDPF_BUMPDUDV = $00080000;

{ DIRECTDRAW ENUMSURFACES FLAGS }

{ Enumerate all of the surfaces that meet the search criterion. }
  DDENUMSURFACES_ALL = $00000001;

{ A search hit is a surface that matches the surface description. }
  DDENUMSURFACES_MATCH = $00000002;

{ A search hit is a surface that does not match the surface description. }
  DDENUMSURFACES_NOMATCH = $00000004;

{ Enumerate the first surface that can be created which meets the search criterion. }
  DDENUMSURFACES_CANBECREATED = $00000008;

{ Enumerate the surfaces that already exist that meet the search criterion. }
  DDENUMSURFACES_DOESEXIST = $00000010;

{ DIRECTDRAW SETDISPLAYMODE FLAGS }

{ The desired mode is a standard VGA mode. }
  DDSDM_STANDARDVGAMODE = $00000001;

{ DIRECTDRAW ENUMDISPLAYMODES FLAGS }

{ Enumerate Modes with different refresh rates. EnumDisplayModes guarantees that a particular mode will be enumerated
  only once. This flag specifies whether the refresh rate is taken into account when determining if a mode is unique. }
  DDEDM_REFRESHRATES = $00000001;

{ Enumerate VGA modes. Specify this flag if you wish to enumerate supported VGA modes such as mode 0x13 in addition to
  the usual ModeX modes (which are always enumerated if the application has previously called SetCooperativeLevel with
  the DDSCL_ALLOWMODEX flag set). }
  DDEDM_STANDARDVGAMODES = $00000002;

{ DIRECTDRAW SETCOOPERATIVELEVEL FLAGS }

{ Exclusive mode owner will be responsible for the entire primary surface. GDI can be ignored. Used with DD. }
  DDSCL_FULLSCREEN = $00000001;

{ Allow CTRL_ALT_DEL to work while in fullscreen exclusive mode. }
  DDSCL_ALLOWREBOOT = $00000002;

{ Prevents DDRAW from modifying the application window.
  Prevents DDRAW from minimize/restore the application window on activation. }
  DDSCL_NOWINDOWCHANGES = $00000004;

{ App wants to work as a regular Windows application. }
  DDSCL_NORMAL = $00000008;

{ App wants exclusive access. }
  DDSCL_EXCLUSIVE = $00000010;

{ App can deal with non-windows display modes. }
  DDSCL_ALLOWMODEX = $00000040;

{ This window will receive the focus messages. }
  DDSCL_SETFOCUSWINDOW = $00000080;

{ This window is associated with the DDRAW object and will cover the screen in fullscreen mode. }
  DDSCL_SETDEVICEWINDOW = $00000100;

{ App wants DDRAW to create a window to be associated with the DDRAW object. }
  DDSCL_CREATEDEVICEWINDOW = $00000200;

{ App explicitly asks DDRAW/D3D to be multithread safe. This makes D3D take the global crtisec more frequently. }
  DDSCL_MULTITHREADED = $00000400;

{ App specifies that it would like to keep the FPU set up for optimal Direct3D performance (single precision and
  exceptions disabled) so Direct3D does not need to explicitly set the FPU each time.
  This is assumed by default in DirectX 7. See also DDSCL_FPUPRESERVE. }
  DDSCL_FPUSETUP = $00000800;

{ App specifies that it needs either double precision FPU or FPU exceptions enabled. This makes Direct3D explicitly
  set the FPU state eah time it is called. Setting the flag will reduce Direct3D performance. The flag is assumed by
  default in DirectX 6 and earlier. See also DDSCL_FPUSETUP. }
  DDSCL_FPUPRESERVE = $00001000;

{ DIRECTDRAW BLT FLAGS }

{ Use the alpha information in the pixel format or the alpha channel surface attached to the destination surface as the
  alpha channel for this blt. }
  DDBLT_ALPHADEST = $00000001;

{ Use the dwConstAlphaDest field in the DDBLTFX structure as the alpha channel for the destination surface for this
  blt. }
  DDBLT_ALPHADESTCONSTOVERRIDE = $00000002;

{ The NEG suffix indicates that the destination surface becomes more transparent as the alpha value increases.
  (0 is opaque) }
  DDBLT_ALPHADESTNEG = $00000004;

{ Use the lpDDSAlphaDest field in the DDBLTFX structure as the alpha channel for the destination for this blt. }
  DDBLT_ALPHADESTSURFACEOVERRIDE = $00000008;

{ Use the dwAlphaEdgeBlend field in the DDBLTFX structure as the alpha channel for the edges of the image that border
  the color key colors. }
  DDBLT_ALPHAEDGEBLEND = $00000010;

{ Use the alpha information in the pixel format or the alpha channel surface attached to the source surface as the
  alpha channel for this blt. }
  DDBLT_ALPHASRC = $00000020;

{ Use the dwConstAlphaSrc field in the DDBLTFX structure as the alpha channel for the source for this blt. }
  DDBLT_ALPHASRCCONSTOVERRIDE = $00000040;

{ The NEG suffix indicates that the source surface becomes more transparent as the alpha value increases.
  (0 is opaque) }
  DDBLT_ALPHASRCNEG = $00000080;

{ Use the lpDDSAlphaSrc field in the DDBLTFX structure as the alpha channel for the source for this blt. }
  DDBLT_ALPHASRCSURFACEOVERRIDE = $00000100;

{ Do this blt asynchronously through the FIFO in the order received. If there is no room in the hardware FIFO fail the
  call. }
  DDBLT_ASYNC = $00000200;

{ Uses the dwFillColor field in the DDBLTFX structure as the RGB color to fill the destination rectangle on the
  destination surface with. }
  DDBLT_COLORFILL = $00000400;

{ Uses the dwDDFX field in the DDBLTFX structure to specify the effects to use for the blt. }
  DDBLT_DDFX = $00000800;

{ Uses the dwDDROPS field in the DDBLTFX structure to specify the ROPS that are not part of the Win32 API. }
  DDBLT_DDROPS = $00001000;

{ Use the color key associated with the destination surface. }
  DDBLT_KEYDEST = $00002000;

{ Use the dckDestColorkey field in the DDBLTFX structure as the color key for the destination surface. }
  DDBLT_KEYDESTOVERRIDE = $00004000;

{ Use the color key associated with the source surface. }
  DDBLT_KEYSRC = $00008000;

{ Use the dckSrcColorkey field in the DDBLTFX structure as the color key for the source surface. }
  DDBLT_KEYSRCOVERRIDE = $00010000;

{ Use the dwROP field in the DDBLTFX structure for the raster operation for this blt. These ROPs are the same as the
  ones defined in the Win32 API. }
  DDBLT_ROP = $00020000;

{ Use the dwRotationAngle field in the DDBLTFX structure as the angle (specified in 1/100th of a degree) to rotate the
  surface. }
  DDBLT_ROTATIONANGLE = $00040000;

{ Z-buffered blt using the z-buffers attached to the source and destination surfaces and the dwZBufferOpCode field in
  the DDBLTFX structure as the z-buffer opcode. }
  DDBLT_ZBUFFER = $00080000;

{ Z-buffered blt using the dwConstDest Zfield and the dwZBufferOpCode field in the DDBLTFX structure as the z-buffer
  and z-buffer opcode respectively for the destination. }
  DDBLT_ZBUFFERDESTCONSTOVERRIDE = $00100000;

{ Z-buffered blt using the lpDDSDestZBuffer field and the dwZBufferOpCode field in the DDBLTFX structure as the
  z-buffer and z-buffer opcode respectively for the destination. }
  DDBLT_ZBUFFERDESTOVERRIDE = $00200000;

{ Z-buffered blt using the dwConstSrcZ field and the dwZBufferOpCode field in the DDBLTFX structure as the z-buffer and
  z-buffer opcode respectively for the source. }
  DDBLT_ZBUFFERSRCCONSTOVERRIDE = $00400000;

{ Z-buffered blt using the lpDDSSrcZBuffer field and the dwZBufferOpCode field in the DDBLTFX structure as the z-buffer
  and z-buffer opcode respectively for the source. }
  DDBLT_ZBUFFERSRCOVERRIDE = $00800000;

{ Wait until the device is ready to handle the blt this will cause blt to not return DDERR_WASSTILLDRAWING. }
  DDBLT_WAIT = $01000000;

{ Uses the dwFillDepth field in the DDBLTFX structure as the depth value to fill the destination rectangle on the
  destination Z-buffer surface with. }
  DDBLT_DEPTHFILL = $02000000;

{ Return immediately (with DDERR_WASSTILLDRAWING) if the device is not ready to schedule the blt at the time Blt() is
  called. }
  DDBLT_DONOTWAIT = $08000000;

{ These flags indicate a presentation blt (i.e. a blt that moves surface contents from an offscreen back buffer to the
  primary surface). The driver is not allowed to "queue"  more than three such blts. The "end" of the presentation blt
  is indicated, since the blt may be clipped, in which case the runtime will call the driver with several blts. All
  blts (even if not clipped) are tagged with DDBLT_PRESENTATION and the last (even if not clipped) additionally with
  DDBLT_LAST_PRESENTATION. Thus the true rule is that the driver must not schedule a DDBLT_PRESENTATION blt if there
  are 3 or more DDBLT_PRESENTLAST blts in the hardware pipe. If there are such blts in the pipe, the driver should
  return DDERR_WASSTILLDRAWING until the oldest queued DDBLT_LAST_PRESENTATION blts has been retired (i.e. the pixels
  have been actually written to the primary surface). Once the oldest blt has been retired, the driver is free to
  schedule the current blt. The goal is to provide a mechanism whereby the device's hardware queue never gets more
  than 3 frames ahead of the frames being generated by the application. When excessive queueing occurs, applications
  become unusable because the application visibly lags user input, and such problems make windowed interactive
  applications impossible. Some drivers may not have sufficient knowledge of their hardware's FIFO to know when a
  certain blt has been retired. Such drivers should code cautiously, and simply not allow any frames to be queued at
  all. DDBLT_LAST_PRESENTATION should cause such drivers to return DDERR_WASSTILLDRAWING until the accelerator is
  completely finished- exactly as if the application had called Lock on the source surface before calling Blt.
  In other words, the driver is allowed and encouraged to generate as much latency as it can, but never more than 3
  frames worth. Implementation detail: Drivers should count blts against the SOURCE surface, not against the primary
  surface. This enables multiple parallel windowed application to function more optimally.
  This flag is passed only to DX8 or higher drivers.

  APPLICATIONS DO NOT SET THESE FLAGS. THEY ARE SET BY THE DIRECTDRAW RUNTIME. }
  DDBLT_PRESENTATION = $10000000;
  DDBLT_LAST_PRESENTATION = $20000000;

{ If DDBLT_EXTENDED_FLAGS is set, then the driver should re-interpret other flags according to the definitions that
  follow. For example, bit 0 (0x00000001L) means DDBLT_ALPHADEST, unless DDBLT_EXTENDED_FLAGS is also set, in which
  case bit 0 means DDBLT_EXTENDED_LINEAR_CONTENT. Only DirectX9 and higher drivers will be given extended blt flags.
  Only flags explicitly mentioned here should be re-interpreted. All other flags retain their original meanings.

  List of re-interpreted flags:

  Bit Hex value   New meaning                                  old meaning
  ---------------------------------------------------------------
   2  0x00000004  DDBLT_EXTENDED_LINEAR_CONTENT                DDBLT_ALPHADESTNEG
   4  0x00000010  DDBLT_EXTENDED_PRESENTATION_STRETCHFACTOR    DDBLT_ALPHAEDGEBLEND


  NOTE: APPLICATIONS SHOULD NOT SET THIS FLAG. THIS FLAG IS INTENDED
  FOR USE BY THE DIRECT3D RUNTIME. }
  DDBLT_EXTENDED_FLAGS = $40000000;

{ EXTENDED FLAG. SEE DEFINITION OF DDBLT_EXTENDED_FLAGS.
  This flag indidcates that the source surface contains content in a linear color space. The driver may perform gamma
  correction to the desktop color space (i.e. sRGB, gamma 2.2) as part of this blt. If the device can perform such a
  conversion as part of the copy, the driver should also set D3DCAPS3_LINEAR_TO_SRGB_PRESENTATION

  NOTE: APPLICATIONS SHOULD NOT SET THIS FLAG. THIS FLAG IS INTENDED FOR USE BY THE DIRECT3D RUNTIME.
  Use IDirect3DSwapChain9::Present and specify D3DPRESENT_LINEAR_CONTENT in order to use this functionality. }
  DDBLT_EXTENDED_LINEAR_CONTENT = $00000004;

{ BLTFAST FLAGS }
  DDBLTFAST_NOCOLORKEY = $00000000;
  DDBLTFAST_SRCCOLORKEY = $00000001;
  DDBLTFAST_DESTCOLORKEY = $00000002;
  DDBLTFAST_WAIT = $00000010;
  DDBLTFAST_DONOTWAIT = $00000020;

{ FLIP FLAGS }

  DDFLIP_WAIT = $00000001;

{ Indicates that the target surface contains the even field of video data.
  This flag is only valid with an overlay surface. }
  DDFLIP_EVEN = $00000002;

{ Indicates that the target surface contains the odd field of video data.
  This flag is only valid with an overlay surface. }
  DDFLIP_ODD = $00000004;

{ Causes DirectDraw to perform the physical flip immediately and return to the application. Typically, what was the
  front buffer but is now the back buffer will still be visible (depending on timing) until the next vertical retrace.
  Subsequent operations involving the two flipped surfaces will not check to see if the physical flip has finished
  (i.e. will not return DDERR_WASSTILLDRAWING for that reason (but may for other reasons)). This allows an application
  to perform Flips at a higher frequency than the monitor refresh rate, but may introduce visible artifacts. Only
  effective if DDCAPS2_FLIPNOVSYNC is set. If that bit is not set, DDFLIP_NOVSYNC has no effect. }
  DDFLIP_NOVSYNC = $00000008;

{ Flip Interval Flags. These flags indicate how many vertical retraces to wait between each flip. The default is one.
  DirectDraw will return DDERR_WASSTILLDRAWING for each surface involved in the flip until the specified number of
  vertical retraces has ocurred. Only effective if DDCAPS2_FLIPINTERVAL is set. If that bit is not set,
  DDFLIP_INTERVALn has no effect.}

{ DirectDraw will flip on every other vertical sync. }
  DDFLIP_INTERVAL2 = $02000000;

{ DirectDraw will flip on every third vertical sync. }
  DDFLIP_INTERVAL3 = $03000000;

{ DirectDraw will flip on every fourth vertical sync. }
  DDFLIP_INTERVAL4 = $04000000;

{ DirectDraw will flip and display a main stereo surface. }
  DDFLIP_STEREO = $00000010;

{ On IDirectDrawSurface7 and higher interfaces, the default is DDFLIP_WAIT. If you wish to override the default and use
  time when the accelerator is busy (as denoted by the DDERR_WASSTILLDRAWING return code) then use DDFLIP_DONOTWAIT. }
  DDFLIP_DONOTWAIT = $00000020;

{ DIRECTDRAW SURFACE OVERLAY FLAGS }

{ Use the alpha information in the pixel format or the alpha channel surface attached to the destination surface as the
  alpha channel for the destination overlay. }
  DDOVER_ALPHADEST = $00000001;

{ Use the dwConstAlphaDest field in the DDOVERLAYFX structure as the destination alpha channel for this overlay. }
  DDOVER_ALPHADESTCONSTOVERRIDE = $00000002;

{ The NEG suffix indicates that the destination surface becomes more transparent as the alpha value increases. }
  DDOVER_ALPHADESTNEG = $00000004;

{ Use the lpDDSAlphaDest field in the DDOVERLAYFX structure as the alpha channel destination for this overlay. }
  DDOVER_ALPHADESTSURFACEOVERRIDE = $00000008;

{ Use the dwAlphaEdgeBlend field in the DDOVERLAYFX structure as the alpha channel for the edges of the image that
  border the color key colors. }
  DDOVER_ALPHAEDGEBLEND = $00000010;

{ Use the alpha information in the pixel format or the alpha channel surface attached to the source surface as the
  source alpha channel for this overlay. }
  DDOVER_ALPHASRC = $00000020;

{ Use the dwConstAlphaSrc field in the DDOVERLAYFX structure as the source alpha channel for this overlay. }
  DDOVER_ALPHASRCCONSTOVERRIDE = $00000040;

{ The NEG suffix indicates that the source surface becomes more transparent as the alpha value increases. }
  DDOVER_ALPHASRCNEG = $00000080;

{ Use the lpDDSAlphaSrc field in the DDOVERLAYFX structure as the alpha channel source for this overlay. }
  DDOVER_ALPHASRCSURFACEOVERRIDE = $00000100;

{ Turn this overlay off. }
  DDOVER_HIDE = $00000200;

{ Use the color key associated with the destination surface. }
  DDOVER_KEYDEST = $00000400;

{ Use the dckDestColorkey field in the DDOVERLAYFX structure as the color key for the destination surface. }
  DDOVER_KEYDESTOVERRIDE = $00000800;

{ Use the color key associated with the source surface. }
  DDOVER_KEYSRC = $00001000;

{ Use the dckSrcColorkey field in the DDOVERLAYFX structure as the color key for the source surface. }
  DDOVER_KEYSRCOVERRIDE = $00002000;

{ Turn this overlay on. }
  DDOVER_SHOW = $00004000;

{ Add a dirty rect to an emulated overlayed surface. }
  DDOVER_ADDDIRTYRECT = $00008000;

{ Redraw all dirty rects on an emulated overlayed surface. }
  DDOVER_REFRESHDIRTYRECTS = $00010000;

{ Redraw the entire surface on an emulated overlayed surface. }
  DDOVER_REFRESHALL = $00020000;

{ Use the overlay FX flags to define special overlay FX. }
  DDOVER_DDFX = $00080000;

{ Autoflip the overlay when ever the video port autoflips. }
  DDOVER_AUTOFLIP = $00100000;

{ Display each field of video port data individually without causing any jittery artifacts. }
  DDOVER_BOB = $00200000;

{ Indicates that bob/weave decisions should not be overridden by other interfaces. }
  DDOVER_OVERRIDEBOBWEAVE = $00400000;

{ Indicates that the surface memory is composed of interleaved fields. }
  DDOVER_INTERLEAVED = $00800000;

{ Indicates that bob will be performed using hardware rather than software or emulated. }
  DDOVER_BOBHARDWARE = $01000000;

{ Indicates that overlay FX structure contains valid ARGB scaling factors. }
  DDOVER_ARGBSCALEFACTORS = $02000000;

{ Indicates that ARGB scaling factors can be degraded to fit driver capabilities. }
  DDOVER_DEGRADEARGBSCALING = $04000000;

{ DIRECTDRAWSURFACE SETSURFACEDESC FLAGS }

{ The default. The GDI DC will be tore down. }
  DDSETSURFACEDESC_RECREATEDC = $00000000;

{ The default. The GDI DC will be kept. }
  DDSETSURFACEDESC_PRESERVEDC = $00000001;

{ DIRECTDRAWSURFACE LOCK FLAGS }

{ The default. Set to indicate that Lock should return a valid memory pointer to the top of the specified rectangle.
  If no rectangle is specified then a pointer to the top of the surface is returned. }
  DDLOCK_SURFACEMEMORYPTR = $00000000;

{ Set to indicate that Lock should wait until it can obtain a valid memory pointer before returning. If this bit is
  set, Lock will never return DDERR_WASSTILLDRAWING. }
  DDLOCK_WAIT = $00000001;

{ Set if an event handle is being passed to Lock. Lock will trigger the event when it can return the surface memory
  pointer requested. }
  DDLOCK_EVENT = $00000002;

{ Indicates that the surface being locked will only be read from. }
  DDLOCK_READONLY = $00000010;

{ Indicates that the surface being locked will only be written to. }
  DDLOCK_WRITEONLY = $00000020;

{ Indicates that a system wide lock should not be taken when this surface is locked. This has several advantages
  (cursor responsiveness, ability to call more Windows functions, easier debugging) when locking video memory surfaces.
  However, an application specifying this flag must comply with a number of conditions documented in the help file.
  Furthermore, this flag cannot be specified when locking the primary. }
  DDLOCK_NOSYSLOCK = $00000800;

{ Used only with Direct3D Vertex Buffer Locks. Indicates that no vertices that were referred to in Draw*PrimtiveVB
  calls since the start of the frame (or the last lock without this flag) will be modified during the lock. This can be
  useful when one is only appending data to the vertex buffer. }
  DDLOCK_NOOVERWRITE = $00001000;

{ Indicates that no assumptions will be made about the contents of the surface or vertex buffer during this lock.
  This enables two things:
  - Direct3D or the driver may provide an alternative memory area as the vertex buffer. This is useful when one plans
    to clear the contents of the vertex buffer and fill in new data.
  - Drivers sometimes store surface data in a re-ordered format. When the application locks the surface, the driver is
    forced to un-re-order the surface data before allowing the application to see the surface contents. This flag is a
    hint to the driver that it can skip the un-re-ordering process since the application plans to overwrite every
    single pixel in the surface or locked rectangle (and so erase any un-re-ordered pixels anyway). Applications should
    always set this flag when they intend to overwrite the entire surface or locked rectangle. }
  DDLOCK_DISCARDCONTENTS = $00002000;

{ DDLOCK_OKTOSWAP is an older, less informative name for DDLOCK_DISCARDCONTENTS. }
  DDLOCK_OKTOSWAP = $00002000;

{ On IDirectDrawSurface7 and higher interfaces, the default is DDLOCK_WAIT. If you wish to override the default and use
  time when the accelerator is busy (as denoted by the DDERR_WASSTILLDRAWING return code) then use DDLOCK_DONOTWAIT. }
  DDLOCK_DONOTWAIT = $00004000;

{ This indicates volume texture lock with front and back specified. }
  DDLOCK_HASVOLUMETEXTUREBOXRECT = $00008000;

{ This indicates that the driver should not update dirty rect information for this lock. }
  DDLOCK_NODIRTYUPDATE = $00010000;

{ DIRECTDRAWSURFACE BLT FX FLAGS }

{ If stretching, use arithmetic stretching along the Y axis for this blt. }
  DDBLTFX_ARITHSTRETCHY = $00000001;

{ Do this blt mirroring the surface left to right. Spin the surface around its y-axis. }
  DDBLTFX_MIRRORLEFTRIGHT = $00000002;

{ Do this blt mirroring the surface up and down. Spin the surface around its x-axis. }
  DDBLTFX_MIRRORUPDOWN = $00000004;

{ Schedule this blt to avoid tearing. }
  DDBLTFX_NOTEARING = $00000008;

{ Do this blt rotating the surface one hundred and eighty degrees. }
  DDBLTFX_ROTATE180 = $00000010;

{ Do this blt rotating the surface two hundred and seventy degrees. }
  DDBLTFX_ROTATE270 = $00000020;

{ Do this blt rotating the surface ninety degrees. }
  DDBLTFX_ROTATE90 = $00000040;

{ Do this z blt using dwZBufferLow and dwZBufferHigh as range values specified to limit the bits copied from the source
  surface. }
  DDBLTFX_ZBUFFERRANGE = $00000080;

{ Do this z blt adding the dwZBufferBaseDest to each of the sources z values before comparing it with the desting z
  values. }
  DDBLTFX_ZBUFFERBASEDEST = $00000100;

{ DIRECTDRAWSURFACE OVERLAY FX FLAGS }

{ If stretching, use arithmetic stretching along the Y axis for this overlay. }
  DDOVERFX_ARITHSTRETCHY = $00000001;

{ Mirror the overlay across the vertical axis. }
  DDOVERFX_MIRRORLEFTRIGHT = $00000002;

{ Mirror the overlay across the horizontal axis. }
  DDOVERFX_MIRRORUPDOWN = $00000004;

{ Deinterlace the overlay, if possible. }
  DDOVERFX_DEINTERLACE = $00000008;

{ DIRECTDRAW WAITFORVERTICALBLANK FLAGS }

{ Return when the vertical blank interval begins. }
  DDWAITVB_BLOCKBEGIN = $00000001;

{ Set up an event to trigger when the vertical blank begins. }
  DDWAITVB_BLOCKBEGINEVENT = $00000002;

{ Return when the vertical blank interval ends and display begins. }
  DDWAITVB_BLOCKEND = $00000004;

{ DIRECTDRAW GETFLIPSTATUS FLAGS }

{ is it OK to flip now? }
  DDGFS_CANFLIP = $00000001;

{ is the last flip finished? }
  DDGFS_ISFLIPDONE = $00000002;

{ DIRECTDRAW GETBLTSTATUS FLAGS }

{ is it OK to blt now? }
  DDGBS_CANBLT = $00000001;

{ is the blt to the surface finished? }
  DDGBS_ISBLTDONE = $00000002;

{ DIRECTDRAW ENUMOVERLAYZORDER FLAGS }

{ Enumerate overlays back to front. }
  DDENUMOVERLAYZ_BACKTOFRONT = $00000000;

{ Enumerate overlays front to back. }
  DDENUMOVERLAYZ_FRONTTOBACK = $00000001;

{ DIRECTDRAW UPDATEOVERLAYZORDER FLAGS }

{ Send overlay to front. }
  DDOVERZ_SENDTOFRONT = $00000000;

{ Send overlay to back. }
  DDOVERZ_SENDTOBACK = $00000001;

{ Move Overlay forward. }
  DDOVERZ_MOVEFORWARD = $00000002;

{ Move Overlay backward. }
  DDOVERZ_MOVEBACKWARD = $00000003;

{ Move Overlay in front of relative surface. }
  DDOVERZ_INSERTINFRONTOF = $00000004;

{ Move Overlay in back of relative surface. }
  DDOVERZ_INSERTINBACKOF = $00000005;

{ DIRECTDRAW SETGAMMARAMP FLAGS }

{ Request calibrator to adjust the gamma ramp according to the physical properties of the display so that the result
  should appear identical on all systems. }
  DDSGR_CALIBRATE = $00000001;

{ DIRECTDRAW STARTMODETEST FLAGS }

{ Indicates that the mode being tested has passed. }
  DDSMT_ISTESTREQUIRED = $00000001;

{ DIRECTDRAW EVALUATEMODE FLAGS }

{ Indicates that the mode being tested has passed. }
  DDEM_MODEPASSED = $00000001;

{ Indicates that the mode being tested has failed. }
  DDEM_MODEFAILED = $00000002;

{ DIRECTDRAW RETURN CODES

  The return values from DirectDraw Commands and Surface that return an HRESULT are codes from DirectDraw concerning
  the results of the action requested by DirectDraw. }

{ Status is OK

  Issued by: DirectDraw Commands and all callbacks. }
  DD_OK = S_OK;
  DD_FALSE = S_FALSE;

{ DIRECTDRAW ENUMCALLBACK RETURN VALUES

  EnumCallback returns are used to control the flow of the DIRECTDRAW and DIRECTDRAWSURFACE object enumerations.
  They can only be returned by enumeration callback routines. }

{ Stop the enumeration. }
  DDENUMRET_CANCEL = 0;

{ Continue the enumeration. }
  DDENUMRET_OK = 1;

{ DIRECTDRAW ERRORS

  Errors are represented by negative values and cannot be combined. }
  MAKE_DDHRESULT = HResult(1 shl 31) or HResult($876 shl 16);

{ This object is already initialized. }
  DDERR_ALREADYINITIALIZED = MAKE_DDHRESULT + 5;

{ This surface can not be attached to the requested surface. }
  DDERR_CANNOTATTACHSURFACE = MAKE_DDHRESULT + 10;

{ This surface can not be detached from the requested surface. }
  DDERR_CANNOTDETACHSURFACE = MAKE_DDHRESULT + 20;

{ Support is currently not available. }
  DDERR_CURRENTLYNOTAVAIL = MAKE_DDHRESULT + 40;

{ An exception was encountered while performing the requested operation. }
  DDERR_EXCEPTION = MAKE_DDHRESULT + 55;

{ Generic failure. }
  DDERR_GENERIC = E_FAIL;

{ Height of rectangle provided is not a multiple of reqd alignment. }
  DDERR_HEIGHTALIGN = MAKE_DDHRESULT + 90;

{ Unable to match primary surface creation request with existing primary surface. }
  DDERR_INCOMPATIBLEPRIMARY = MAKE_DDHRESULT + 95;

{ One or more of the caps bits passed to the callback are incorrect. }
  DDERR_INVALIDCAPS = MAKE_DDHRESULT + 100;

{ DirectDraw does not support provided Cliplist. }
  DDERR_INVALIDCLIPLIST = MAKE_DDHRESULT + 110;

{ DirectDraw does not support the requested mode. }
  DDERR_INVALIDMODE = MAKE_DDHRESULT + 120;

{ DirectDraw received a pointer that was an invalid DIRECTDRAW object. }
  DDERR_INVALIDOBJECT = MAKE_DDHRESULT + 130;

{ One or more of the parameters passed to the callback function are incorrect. }
  DDERR_INVALIDPARAMS = E_INVALIDARG;

{ Pixel format was invalid as specified. }
  DDERR_INVALIDPIXELFORMAT = MAKE_DDHRESULT + 145;

{ Rectangle provided was invalid. }
  DDERR_INVALIDRECT = MAKE_DDHRESULT + 150;

{ Operation could not be carried out because one or more surfaces are locked. }
  DDERR_LOCKEDSURFACES = MAKE_DDHRESULT + 160;

{ There is no 3D present. }
  DDERR_NO3D = MAKE_DDHRESULT + 170;

{ Operation could not be carried out because there is no alpha accleration hardware present or available. }
  DDERR_NOALPHAHW = MAKE_DDHRESULT + 180;

{ Operation could not be carried out because there is no stereo hardware present or available. }
  DDERR_NOSTEREOHARDWARE = MAKE_DDHRESULT + 181;

{ Operation could not be carried out because there is no hardware present which supports stereo surfaces. }
  DDERR_NOSURFACELEFT = MAKE_DDHRESULT + 182;

{ No clip list available. }
  DDERR_NOCLIPLIST = MAKE_DDHRESULT + 205;

{ Operation could not be carried out because there is no color conversion hardware present or available. }
  DDERR_NOCOLORCONVHW = MAKE_DDHRESULT + 210;

{ Create function called without DirectDraw object method SetCooperativeLevel being called. }
  DDERR_NOCOOPERATIVELEVELSET = MAKE_DDHRESULT + 212;

{ Surface doesn't currently have a color key. }
  DDERR_NOCOLORKEY = MAKE_DDHRESULT + 215;

{ Operation could not be carried out because there is no hardware support
 * of the dest color key. }
  DDERR_NOCOLORKEYHW = MAKE_DDHRESULT + 220;

{ No DirectDraw support possible with current display driver. }
  DDERR_NODIRECTDRAWSUPPORT = MAKE_DDHRESULT + 222;

{ Operation requires the application to have exclusive mode but the application does not have exclusive mode. }
  DDERR_NOEXCLUSIVEMODE = MAKE_DDHRESULT + 225;

{ Flipping visible surfaces is not supported. }
  DDERR_NOFLIPHW = MAKE_DDHRESULT + 230;

{ There is no GDI present. }
  DDERR_NOGDI = MAKE_DDHRESULT + 240;

{ Operation could not be carried out because there is no hardware present or available. }
  DDERR_NOMIRRORHW = MAKE_DDHRESULT + 250;

{ Requested item was not found. }
  DDERR_NOTFOUND = MAKE_DDHRESULT + 255;

{ Operation could not be carried out because there is no overlay hardware present or available. }
  DDERR_NOOVERLAYHW = MAKE_DDHRESULT + 260;

{ Operation could not be carried out because the source and destination rectangles are on the same surface and overlap
  each other. }
  DDERR_OVERLAPPINGRECTS = MAKE_DDHRESULT + 270;

{ Operation could not be carried out because there is no appropriate raster op hardware present or available. }
  DDERR_NORASTEROPHW = MAKE_DDHRESULT + 280;

{ Operation could not be carried out because there is no rotation hardware present or available. }
  DDERR_NOROTATIONHW = MAKE_DDHRESULT + 290;

{ Operation could not be carried out because there is no hardware support for stretching. }
  DDERR_NOSTRETCHHW = MAKE_DDHRESULT + 310;

{ DirectDrawSurface is not in 4 bit color palette and the requested operation requires 4 bit color palette. }
  DDERR_NOT4BITCOLOR = MAKE_DDHRESULT + 316;

{ DirectDrawSurface is not in 4 bit color index palette and the requested operation requires 4 bit color index
  palette. }
  DDERR_NOT4BITCOLORINDEX = MAKE_DDHRESULT + 317;

{ DirectDraw Surface is not in 8 bit color mode and the requested operation requires 8 bit color. }
  DDERR_NOT8BITCOLOR = MAKE_DDHRESULT + 320;

{ Operation could not be carried out because there is no texture mapping hardware present or available. }
  DDERR_NOTEXTUREHW = MAKE_DDHRESULT + 330;

{ Operation could not be carried out because there is no hardware support for vertical blank synchronized operations.}
  DDERR_NOVSYNCHW = MAKE_DDHRESULT + 335;

{ Operation could not be carried out because there is no hardware support for zbuffer blting. }
  DDERR_NOZBUFFERHW = MAKE_DDHRESULT + 340;

{ Overlay surfaces could not be z layered based on their BltOrder because the hardware does not support z layering of
  overlays. }
  DDERR_NOZOVERLAYHW = MAKE_DDHRESULT + 350;

{ The hardware needed for the requested operation has already been allocated. }
  DDERR_OUTOFCAPS = MAKE_DDHRESULT + 360;

{ DirectDraw does not have enough memory to perform the operation. }
  DDERR_OUTOFMEMORY = E_OUTOFMEMORY;

{ DirectDraw does not have enough memory to perform the operation. }
  DDERR_OUTOFVIDEOMEMORY = MAKE_DDHRESULT + 380;

{ Hardware does not support clipped overlays. }
  DDERR_OVERLAYCANTCLIP = MAKE_DDHRESULT + 382;

{ Can only have ony color key active at one time for overlays. }
  DDERR_OVERLAYCOLORKEYONLYONEACTIVE = MAKE_DDHRESULT + 384;

{ Access to this palette is being refused because the palette is already locked by another thread. }
  DDERR_PALETTEBUSY = MAKE_DDHRESULT + 387;

{ No src color key specified for this operation. }
  DDERR_COLORKEYNOTSET = MAKE_DDHRESULT + 400;

{ This surface is already attached to the surface it is being attached to. }
  DDERR_SURFACEALREADYATTACHED = MAKE_DDHRESULT + 410;

{ This surface is already a dependency of the surface it is being made a dependency of. }
  DDERR_SURFACEALREADYDEPENDENT = MAKE_DDHRESULT + 420;

{ Access to this surface is being refused because the surface is already locked by another thread. }
  DDERR_SURFACEBUSY = MAKE_DDHRESULT + 430;

{ Access to this surface is being refused because no driver exists which can supply a pointer to the surface. This is
  most likely to happen when attempting to lock the primary surface when no DCI provider is present. Will also happen
  on attempts to lock an optimized surface. }
  DDERR_CANTLOCKSURFACE = MAKE_DDHRESULT + 435;

{ Access to Surface refused because Surface is obscured. }
  DDERR_SURFACEISOBSCURED = MAKE_DDHRESULT + 440;

{ Access to this surface is being refused because the surface is gone. The DIRECTDRAWSURFACE object representing this
  surface should have Restore called on it. }
  DDERR_SURFACELOST = MAKE_DDHRESULT + 450;

{ The requested surface is not attached. }
  DDERR_SURFACENOTATTACHED = MAKE_DDHRESULT + 460;

{ Height requested by DirectDraw is too large. }
  DDERR_TOOBIGHEIGHT = MAKE_DDHRESULT + 470;

{ Size requested by DirectDraw is too large --  The individual height and width are OK. }
  DDERR_TOOBIGSIZE = MAKE_DDHRESULT + 480;

{ Width requested by DirectDraw is too large. }
  DDERR_TOOBIGWIDTH = MAKE_DDHRESULT + 490;

{ Action not supported. }
  DDERR_UNSUPPORTED = E_NOTIMPL;

{ Pixel format requested is unsupported by DirectDraw. }
  DDERR_UNSUPPORTEDFORMAT = MAKE_DDHRESULT + 510;

{ Bitmask in the pixel format requested is unsupported by DirectDraw. }
  DDERR_UNSUPPORTEDMASK = MAKE_DDHRESULT + 520;

{ The specified stream contains invalid data. }
  DDERR_INVALIDSTREAM = MAKE_DDHRESULT + 521;

{ Vertical blank is in progress. }
  DDERR_VERTICALBLANKINPROGRESS = MAKE_DDHRESULT + 537;

{ Informs DirectDraw that the previous Blt which is transfering information to or from this Surface is incomplete. }
  DDERR_WASSTILLDRAWING = MAKE_DDHRESULT + 540;

{ The specified surface type requires specification of the COMPLEX flag. }
  DDERR_DDSCAPSCOMPLEXREQUIRED = MAKE_DDHRESULT + 542;

{ Rectangle provided was not horizontally aligned on required boundary. }
  DDERR_XALIGN = MAKE_DDHRESULT + 560;

{ The GUID passed to DirectDrawCreate is not a valid DirectDraw driver identifier. }
  DDERR_INVALIDDIRECTDRAWGUID = MAKE_DDHRESULT + 561;

{ A DirectDraw object representing this driver has already been created for this process. }
  DDERR_DIRECTDRAWALREADYCREATED = MAKE_DDHRESULT + 562;

{ A hardware only DirectDraw object creation was attempted but the driver did not support any hardware. }
  DDERR_NODIRECTDRAWHW = MAKE_DDHRESULT + 563;

{ This process already has created a primary surface. }
  DDERR_PRIMARYSURFACEALREADYEXISTS = MAKE_DDHRESULT + 564;

{ Software emulation not available. }
  DDERR_NOEMULATION = MAKE_DDHRESULT + 565;

{ Region passed to Clipper::GetClipList is too small. }
  DDERR_REGIONTOOSMALL = MAKE_DDHRESULT + 566;

{ An attempt was made to set a clip list for a clipper objec that is already monitoring an hwnd. }
  DDERR_CLIPPERISUSINGHWND = MAKE_DDHRESULT + 567;

{ No clipper object attached to surface object. }
  DDERR_NOCLIPPERATTACHED = MAKE_DDHRESULT + 568;

{ Clipper notification requires an HWND or no HWND has previously been set as the CooperativeLevel HWND. }
  DDERR_NOHWND = MAKE_DDHRESULT + 569;

{ HWND used by DirectDraw CooperativeLevel has been subclassed, this prevents DirectDraw from restoring state. }
  DDERR_HWNDSUBCLASSED = MAKE_DDHRESULT + 570;

{ The CooperativeLevel HWND has already been set.
  It can not be reset while the process has surfaces or palettes created. }
  DDERR_HWNDALREADYSET = MAKE_DDHRESULT + 571;

{ No palette object attached to this surface. }
  DDERR_NOPALETTEATTACHED = MAKE_DDHRESULT + 572;

{ No hardware support for 16 or 256 color palettes. }
  DDERR_NOPALETTEHW = MAKE_DDHRESULT + 573;

{ If a clipper object is attached to the source surface passed into a BltFast call. }
  DDERR_BLTFASTCANTCLIP = MAKE_DDHRESULT + 574;

{ No blter. }
  DDERR_NOBLTHW = MAKE_DDHRESULT + 575;

{ No DirectDraw ROP hardware. }
  DDERR_NODDROPSHW = MAKE_DDHRESULT + 576;

{ Returned when GetOverlayPosition is called on a hidden overlay. }
  DDERR_OVERLAYNOTVISIBLE = MAKE_DDHRESULT + 577;

{ Returned when GetOverlayPosition is called on a overlay that UpdateOverlay has never been called on to establish a
  destionation. }
  DDERR_NOOVERLAYDEST = MAKE_DDHRESULT + 578;

{ Returned when the position of the overlay on the destionation is no longer legal for that destionation. }
  DDERR_INVALIDPOSITION = MAKE_DDHRESULT + 579;

{ Returned when an overlay member is called for a non-overlay surface. }
  DDERR_NOTAOVERLAYSURFACE = MAKE_DDHRESULT + 580;

{ An attempt was made to set the cooperative level when it was already set to exclusive. }
  DDERR_EXCLUSIVEMODEALREADYSET = MAKE_DDHRESULT + 581;

{ An attempt has been made to flip a surface that is not flippable. }
  DDERR_NOTFLIPPABLE = MAKE_DDHRESULT + 582;

{ Can't duplicate primary & 3D surfaces, or surfaces that are implicitly created. }
  DDERR_CANTDUPLICATE = MAKE_DDHRESULT + 583;

{ Surface was not locked.  An attempt to unlock a surface that was not locked at all, or by this process, has been
  attempted. }
  DDERR_NOTLOCKED = MAKE_DDHRESULT + 584;

{ Windows can not create any more DCs, or a DC was requested for a paltte-indexed surface when the surface had no
  palette AND the display mode was not palette-indexed (in this case DirectDraw cannot select a proper palette into
  the DC). }
  DDERR_CANTCREATEDC = MAKE_DDHRESULT + 585;

{ No DC was ever created for this surface. }
  DDERR_NODC = MAKE_DDHRESULT + 586;

{ This surface can not be restored because it was created in a different mode. }
  DDERR_WRONGMODE = MAKE_DDHRESULT + 587;

{ This surface can not be restored because it is an implicitly created surface. }
  DDERR_IMPLICITLYCREATED = MAKE_DDHRESULT + 588;

{ The surface being used is not a palette-based surface. }
  DDERR_NOTPALETTIZED = MAKE_DDHRESULT + 589;

{ The display is currently in an unsupported mode. }
  DDERR_UNSUPPORTEDMODE = MAKE_DDHRESULT + 590;

{ Operation could not be carried out because there is no mip-map texture mapping hardware present or available. }
  DDERR_NOMIPMAPHW = MAKE_DDHRESULT + 591;

{ The requested action could not be performed because the surface was of the wrong type. }
  DDERR_INVALIDSURFACETYPE = MAKE_DDHRESULT + 592;

{ Device does not support optimized surfaces, therefore no video memory optimized surfaces. }
  DDERR_NOOPTIMIZEHW = MAKE_DDHRESULT + 600;

{ Surface is an optimized surface, but has not yet been allocated any memory. }
  DDERR_NOTLOADED = MAKE_DDHRESULT + 601;

{ Attempt was made to create or set a device window without first setting the focus window. }
  DDERR_NOFOCUSWINDOW = MAKE_DDHRESULT + 602;

{ Attempt was made to set a palette on a mipmap sublevel. }
  DDERR_NOTONMIPMAPSUBLEVEL = MAKE_DDHRESULT + 603;

{ A DC has already been returned for this surface. Only one DC can be retrieved per surface. }
  DDERR_DCALREADYCREATED = MAKE_DDHRESULT + 620;

{ An attempt was made to allocate non-local video memory from a device that does not support non-local video memory. }
  DDERR_NONONLOCALVIDMEM = MAKE_DDHRESULT + 630;

{ The attempt to page lock a surface failed. }
  DDERR_CANTPAGELOCK = MAKE_DDHRESULT + 640;

{ The attempt to page unlock a surface failed. }
  DDERR_CANTPAGEUNLOCK = MAKE_DDHRESULT + 660;

{ An attempt was made to page unlock a surface with no outstanding page locks. }
  DDERR_NOTPAGELOCKED = MAKE_DDHRESULT + 680;

{ There is more data available than the specified buffer size could hold. }
  DDERR_MOREDATA = MAKE_DDHRESULT + 690;

{ The data has expired and is therefore no longer valid. }
  DDERR_EXPIRED = MAKE_DDHRESULT + 691;

{ The mode test has finished executing. }
  DDERR_TESTFINISHED = MAKE_DDHRESULT + 692;

{ The mode test has switched to a new mode. }
  DDERR_NEWMODE = MAKE_DDHRESULT + 693;

{ D3D has not yet been initialized. }
  DDERR_D3DNOTINITIALIZED = MAKE_DDHRESULT + 694;

{ The video port is not active. }
  DDERR_VIDEONOTACTIVE = MAKE_DDHRESULT + 695;

{ The monitor does not have EDID data. }
 DDERR_NOMONITORINFORMATION = MAKE_DDHRESULT + 696;

{ The driver does not enumerate display mode refresh rates. }
 DDERR_NODRIVERSUPPORT = MAKE_DDHRESULT + 697;

{ Surfaces created by one direct draw device cannot be used directly by another direct draw device. }
  DDERR_DEVICEDOESNTOWNSURFACE= MAKE_DDHRESULT + 699;

{ An attempt was made to invoke an interface member of a DirectDraw object created by CoCreateInstance() before it was
  initialized.}
  DDERR_NOTINITIALIZED = CO_E_NOTINITIALIZED;

{ Flags for DirectDrawEnumerateEx
  DirectDrawEnumerateEx supercedes DirectDrawEnumerate. You must use GetProcAddress to obtain a function pointer (of
  type LPDIRECTDRAWENUMERATEEX) to DirectDrawEnumerateEx. By default, only the primary display device is enumerated.
  DirectDrawEnumerate is equivalent to DirectDrawEnumerate(,,DDENUM_NONDISPLAYDEVICES) }

{ This flag causes enumeration of any GDI display devices which are part of the Windows Desktop. }
  DDENUM_ATTACHEDSECONDARYDEVICES = $00000001;

{ This flag causes enumeration of any GDI display devices which are not part of the Windows Desktop. }
  DDENUM_DETACHEDSECONDARYDEVICES = $00000002;

{ This flag causes enumeration of non-display devices. }
  DDENUM_NONDISPLAYDEVICES = $00000004;

  DDCREATE_HARDWAREONLY = $00000001;
  DDCREATE_EMULATIONONLY = $00000002;

{ Space required to store ROP array. }
  DD_ROP_SPACE = 256 div 32;

  MAX_DDDEVICEID_STRING = 512;

{ Flags for the IDirectDraw4::GetDeviceIdentifier method }

{ This flag causes GetDeviceIdentifier to return information about the host (typically 2D) adapter in a system equipped
  with a stacked secondary 3D adapter. Such an adapter appears to the application as if it were part of the host
  adapter, but is typically physcially located on a separate card. The stacked secondary's information is returned when
  GetDeviceIdentifier's dwFlags field is zero, since this most accurately reflects the qualities of the DirectDraw
  object involved. }
  DDGDI_GETHOSTIDENTIFIER = $00000001;

{ FOURCC codes for DX compressed-texture pixel formats. }
  FOURCC_DXT1 = 'DXT1';
  FOURCC_DXT2 = 'DXT2';
  FOURCC_DXT3 = 'DXT3';
  FOURCC_DXT4 = 'DXT4';
  FOURCC_DXT5 = 'DXT5';

{ GUIDS used by DirectDraw objects }
  CLSID_DirectDraw: TGuid = '{D7B70EE0-4340-11CF-B063-0020AFC2CD35}';
  CLSID_DirectDraw7: TGuid = '{3C305196-50DB-11D3-9CFE-00C04FD930C5}';
  CLSID_DirectDrawClipper: TGuid = '{593817A0-7DB3-11CF-A2DE-00AA00B93356}';

  SID_IDirectDraw = '{6C14DB80-A733-11CE-A521-0020AF0BE560}';
  SID_IDirectDraw2 = '{B3A6F3E0-2B43-11CF-A2DE-00AA00B93356}';
  SID_IDirectDraw4 = '{9C59509A-39BD-11D1-8C4A-00C04FD930C5}';
  SID_IDirectDraw7 = '{15E65EC0-3B9C-11D2-B92F-00609797EA5B}';
  SID_IDirectDrawSurface = '{6C14DB81-A733-11CE-A521-0020AF0BE560}';
  SID_IDirectDrawSurface2 = '{57805885-6EEC-11CF-9441-A82303C10E27}';
  SID_IDirectDrawSurface3 = '{DA044E00-69B2-11D0-A1D5-00AA00B8DFBB}';
  SID_IDirectDrawSurface4 = '{0B2B8630-AD35-11D0-8EA6-00609797EA5B}';
  SID_IDirectDrawSurface7 = '{06675A80-3B9B-11D2-B92F-00609797EA5B}';
  SID_IDirectDrawPalette = '{6C14DB84-A733-11CE-A521-0020AF0BE560}';
  SID_IDirectDrawClipper = '{6C14DB85-A733-11CE-A521-0020AF0BE560}';
  SID_IDirectDrawColorControl = '{4B9F0EE0-0D7E-11D0-9B06-00A0C903A3B8}';
  SID_IDirectDrawGammaControl = '{69C11C3E-B46B-11D1-AD7A-00C04FC29B4E}';

  IID_IDirectDraw: TGuid = SID_IDirectDraw;
  IID_IDirectDraw2: TGuid = SID_IDirectDraw2;
  IID_IDirectDraw4: TGuid = SID_IDirectDraw4;
  IID_IDirectDraw7: TGuid = SID_IDirectDraw7;
  IID_IDirectDrawSurface: TGuid = SID_IDirectDrawSurface;
  IID_IDirectDrawSurface2: TGuid = SID_IDirectDrawSurface2;
  IID_IDirectDrawSurface3: TGuid = SID_IDirectDrawSurface3;
  IID_IDirectDrawSurface4: TGuid = SID_IDirectDrawSurface4;
  IID_IDirectDrawSurface7: TGuid = SID_IDirectDrawSurface7;
  IID_IDirectDrawPalette: TGuid = SID_IDirectDrawPalette;
  IID_IDirectDrawClipper: TGuid = SID_IDirectDrawClipper;
  IID_IDirectDrawColorControl: TGuid = SID_IDirectDrawColorControl;
  IID_IDirectDrawGammaControl: TGuid = SID_IDirectDrawGammaControl;

type
  IDirectDrawClipper = interface;
  IDirectDrawPalette = interface;
  IDirectDrawSurface = interface;
  IDirectDrawSurface4 = interface;
  IDirectDrawSurface7 = interface;

{ Generic pixel format with 8-bit RGB and alpha components. }
  PDDARGB = ^DDARGB;
  DDARGB = record
    blue: Byte;
    green: Byte;
    red: Byte;
    alpha: Byte;
  end;

{ This version of the structure remains for backwards source compatibility.
  The DDARGB structure is the one that should be used for all DirectDraw APIs. }
  PDDRGBA = ^DDRGBA;
  DDRGBA = record
    red: Byte;
    green: Byte;
    blue: Byte;
    alpha: Byte;
  end;

{ DDCOLORKEY }
  PDDCOLORKEY = ^DDCOLORKEY;
  DDCOLORKEY = record
    dwColorSpaceLowValue: LongWord;
    dwColorSpaceHighValue: LongWord;
  end;

{ DDBLTFX
  Used to pass override information to the DIRECTDRAWSURFACE callback Blt. }
  PDDBLTFX = ^DDBLTFX;
  DDBLTFX = record
    dwSize: LongWord;
    dwDDFX: LongWord;
    dwROP: LongWord;
    dwDDROP: LongWord;
    dwRotationAngle: LongWord;
    dwZBufferOpCode: LongWord;
    dwZBufferLow: LongWord;
    dwZBufferHigh: LongWord;
    dwZBufferBaseDest: LongWord;
    dwZDestConstBitDepth: LongWord;
    case Integer of
      0: (dwZDestConst: LongWord);
      1: (lpDDSZBufferDest: ^IDirectDrawSurface;
          dwZSrcConstBitDepth: LongWord;
          case Integer of
            0: (dwZSrcConst: LongWord);
            1: (lpDDSZBufferSrc: ^IDirectDrawSurface;
                dwAlphaEdgeBlendBitDepth: LongWord;
                dwAlphaEdgeBlend: LongWord;
                dwReserved: LongWord;
                dwAlphaDestConstBitDepth: LongWord;
                case Integer of
                  0: (dwAlphaDestConst: LongWord);
                  1: (lpDDSAlphaDest: ^IDirectDrawSurface;
                      dwAlphaSrcConstBitDepth: LongWord;
                      case Integer of
                        0: (dwAlphaSrcConst: LongWord);
                        1: (case Integer of
                              0: (dwFillColor: LongWord);
                              1: (dwFillDepth: LongWord);
                              2: (dwFillPixel: LongWord);
                              3: (lpDDSPattern: ^IDirectDrawSurface;
                                  ddckDestColorkey: DDCOLORKEY;
                                  ddckSrcColorkey: DDCOLORKEY)))));
  end;

{ DDSCAPS }
  PDDSCAPS = ^DDSCAPS;
  DDSCAPS = record
    dwCaps: LongWord;
  end;

{ DDOSCAPS }
  PDDOSCAPS = ^DDOSCAPS;
  DDOSCAPS = record
    dwCaps: LongWord;
  end;

{ This structure is used internally by DirectDraw. }
  PDDSCAPSEX = ^DDSCAPSEX;
  DDSCAPSEX = record
    dwCaps2: LongWord;
    dwCaps3: LongWord;
    case Integer of
      0: (dwCaps4: LongWord);
      1: (dwVolumeDepth: LongWord);
  end;

{ DDSCAPS2 }
  PDDSCAPS2 = ^DDSCAPS2;
  DDSCAPS2 = record
    dwCaps: LongWord;
    dwCaps2: LongWord;
    dwCaps3: LongWord;
    case Integer of
      0: (dwCaps4: LongWord);
      1: (dwVolumeDepth: LongWord);
  end;

{ This structure represents the DDCAPS structure released in DirectDraw 1.0. It is used internally by DirectDraw to
  interpret caps passed into ddraw by drivers written prior to the release of DirectDraw 2.0. New applications should
  use the DDCAPS structure defined below. }
  PDDCAPS_DX1 = ^DDCAPS_DX1;
  DDCAPS_DX1 = record
    dwSize: LongWord; { size of the DDDRIVERCAPS structure }
    dwCaps: LongWord; { driver specific capabilities }
    dwCaps2: LongWord;  { more driver specific capabilites }
    dwCKeyCaps: LongWord; { color key capabilities of the surface }
    dwFXCaps: LongWord; { driver specific stretching and effects capabilites }
    dwFXAlphaCaps: LongWord; { alpha driver specific capabilities }
    dwPalCaps: LongWord; { palette capabilities }
    dwSVCaps: LongWord; { stereo vision capabilities }
    dwAlphaBltConstBitDepths: LongWord; { DDBD_2,4,8 }
    dwAlphaBltPixelBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwAlphaBltSurfaceBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwAlphaOverlayConstBitDepths: LongWord; { DDBD_2,4,8 }
    dwAlphaOverlayPixelBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwAlphaOverlaySurfaceBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwZBufferBitDepths: LongWord; { DDBD_8,16,24,32 }
    dwVidMemTotal: LongWord; { total amount of video memory }
    dwVidMemFree: LongWord; { amount of free video memory }
    dwMaxVisibleOverlays: LongWord; { maximum number of visible overlays }
    dwCurrVisibleOverlays: LongWord; { current number of visible overlays }
    dwNumFourCCCodes: LongWord; { number of four cc codes }
    dwAlignBoundarySrc: LongWord; { source rectangle alignment }
    dwAlignSizeSrc: LongWord; { source rectangle byte size }
    dwAlignBoundaryDest: LongWord; { dest rectangle alignment }
    dwAlignSizeDest: LongWord; { dest rectangle byte size }
    dwAlignStrideAlign: LongWord; { stride alignment }
    dwRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported }
    ddsCaps: DDSCAPS; { DDSCAPS structure has all the general capabilities }
    dwMinOverlayStretch: LongWord; { minimum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMaxOverlayStretch: LongWord; { maximum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMinLiveVideoStretch: LongWord; { OBSOLETE! This field remains for compatability reasons only }
    dwMaxLiveVideoStretch: LongWord; { OBSOLETE! This field remains for compatability reasons only }
    dwMinHwCodecStretch: LongWord; { OBSOLETE! This field remains for compatability reasons only }
    dwMaxHwCodecStretch: LongWord; { OBSOLETE! This field remains for compatability reasons only }
    dwReserved1: LongWord; { reserved }
    dwReserved2: LongWord; { reserved }
    dwReserved3: LongWord; { reserved }
  end;

{ This structure is the DDCAPS structure as it was in version 2 and 3 of Direct X.
  It is present for back compatability. }
  PDDCAPS_DX3 = ^DDCAPS_DX3;
  DDCAPS_DX3 = record
    dwSize: LongWord; { size of the DDDRIVERCAPS structure }
    dwCaps: LongWord; { driver specific capabilities }
    dwCaps2: LongWord; { more driver specific capabilites }
    dwCKeyCaps: LongWord; { color key capabilities of the surface }
    dwFXCaps: LongWord; { driver specific stretching and effects capabilites }
    dwFXAlphaCaps: LongWord; { alpha driver specific capabilities }
    dwPalCaps: LongWord; { palette capabilities }
    dwSVCaps: LongWord; { stereo vision capabilities }
    dwAlphaBltConstBitDepths: LongWord; { DDBD_2,4,8 }
    dwAlphaBltPixelBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwAlphaBltSurfaceBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwAlphaOverlayConstBitDepths: LongWord; { DDBD_2,4,8 }
    dwAlphaOverlayPixelBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwAlphaOverlaySurfaceBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwZBufferBitDepths: LongWord; { DDBD_8,16,24,32 }
    dwVidMemTotal: LongWord; { total amount of video memory }
    dwVidMemFree: LongWord; { amount of free video memory }
    dwMaxVisibleOverlays: LongWord; { maximum number of visible overlays }
    dwCurrVisibleOverlays: LongWord; { current number of visible overlays }
    dwNumFourCCCodes: LongWord; { number of four cc codes }
    dwAlignBoundarySrc: LongWord; { source rectangle alignment }
    dwAlignSizeSrc: LongWord; { source rectangle byte size }
    dwAlignBoundaryDest: LongWord; { dest rectangle alignment }
    dwAlignSizeDest: LongWord; { dest rectangle byte size }
    dwAlignStrideAlign: LongWord; { stride alignment }
    dwRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported }
    ddsCaps: DDSCAPS; { DDSCAPS structure has all the general capabilities }
    dwMinOverlayStretch: LongWord; { minimum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMaxOverlayStretch: LongWord; { maximum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMinLiveVideoStretch: LongWord; { minimum live video stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMaxLiveVideoStretch: LongWord; { maximum live video stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMinHwCodecStretch: LongWord; { minimum hardware codec stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMaxHwCodecStretch: LongWord; { maximum hardware codec stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwReserved1: LongWord; { reserved }
    dwReserved2: LongWord; { reserved }
    dwReserved3: LongWord; { reserved }
    dwSVBCaps: LongWord; { driver specific capabilities for System->Vmem blts }
    dwSVBCKeyCaps: LongWord; { driver color key capabilities for System->Vmem blts }
    dwSVBFXCaps: LongWord; { driver FX capabilities for System->Vmem blts }
    dwSVBRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported for System->Vmem blts }
    dwVSBCaps: LongWord; { driver specific capabilities for Vmem->System blts }
    dwVSBCKeyCaps: LongWord; { driver color key capabilities for Vmem->System blts }
    dwVSBFXCaps: LongWord; { driver FX capabilities for Vmem->System blts }
    dwVSBRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported for Vmem->System blts }
    dwSSBCaps: LongWord; { driver specific capabilities for System->System blts }
    dwSSBCKeyCaps: LongWord; { driver color key capabilities for System->System blts }
    dwSSBFXCaps: LongWord; { driver FX capabilities for System->System blts }
    dwSSBRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported for System->System blts }
    dwReserved4: LongWord; { reserved }
    dwReserved5: LongWord; { reserved }
    dwReserved6: LongWord; { reserved }
  end;

{ This structure is the DDCAPS structure as it was in version 5 of Direct X.
  It is present for back compatability.}
  PDDCAPS_DX5 = ^DDCAPS_DX5;
  DDCAPS_DX5 = record
    dwSize: LongWord; { size of the DDDRIVERCAPS structure }
    dwCaps: LongWord; { driver specific capabilities }
    dwCaps2: LongWord; { more driver specific capabilites }
    dwCKeyCaps: LongWord; { color key capabilities of the surface }
    dwFXCaps: LongWord; { driver specific stretching and effects capabilites }
    dwFXAlphaCaps: LongWord; { alpha driver specific capabilities }
    dwPalCaps: LongWord; { palette capabilities }
    dwSVCaps: LongWord; { stereo vision capabilities }
    dwAlphaBltConstBitDepths: LongWord; { DDBD_2,4,8 }
    dwAlphaBltPixelBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwAlphaBltSurfaceBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwAlphaOverlayConstBitDepths: LongWord; { DDBD_2,4,8 }
    dwAlphaOverlayPixelBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwAlphaOverlaySurfaceBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwZBufferBitDepths: LongWord; { DDBD_8,16,24,32 }
    dwVidMemTotal: LongWord; { total amount of video memory }
    dwVidMemFree: LongWord; { amount of free video memory }
    dwMaxVisibleOverlays: LongWord; { maximum number of visible overlays }
    dwCurrVisibleOverlays: LongWord; { current number of visible overlays }
    dwNumFourCCCodes: LongWord; { number of four cc codes }
    dwAlignBoundarySrc: LongWord; { source rectangle alignment }
    dwAlignSizeSrc: LongWord; { source rectangle byte size }
    dwAlignBoundaryDest: LongWord; { dest rectangle alignment }
    dwAlignSizeDest: LongWord; { dest rectangle byte size }
    dwAlignStrideAlign: LongWord; { stride alignment }
    dwRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported }
    ddsCaps: DDSCAPS; { DDSCAPS structure has all the general capabilities }
    dwMinOverlayStretch: LongWord; { minimum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMaxOverlayStretch: LongWord; { maximum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMinLiveVideoStretch: LongWord; { minimum live video stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMaxLiveVideoStretch: LongWord; { maximum live video stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMinHwCodecStretch: LongWord; { minimum hardware codec stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMaxHwCodecStretch: LongWord; { maximum hardware codec stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwReserved1: LongWord; { reserved }
    dwReserved2: LongWord; { reserved }
    dwReserved3: LongWord; { reserved }
    dwSVBCaps: LongWord; { driver specific capabilities for System->Vmem blts }
    dwSVBCKeyCaps: LongWord; { driver color key capabilities for System->Vmem blts }
    dwSVBFXCaps: LongWord; { driver FX capabilities for System->Vmem blts }
    dwSVBRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported for System->Vmem blts }
    dwVSBCaps: LongWord; { driver specific capabilities for Vmem->System blts }
    dwVSBCKeyCaps: LongWord; { driver color key capabilities for Vmem->System blts }
    dwVSBFXCaps: LongWord; { driver FX capabilities for Vmem->System blts }
    dwVSBRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported for Vmem->System blts }
    dwSSBCaps: LongWord; { driver specific capabilities for System->System blts }
    dwSSBCKeyCaps: LongWord; { driver color key capabilities for System->System blts }
    dwSSBFXCaps: LongWord; { driver FX capabilities for System->System blts }
    dwSSBRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported for System->System blts }
    { Members added for DX5: }
    dwMaxVideoPorts: LongWord; { maximum number of usable video ports }
    dwCurrVideoPorts: LongWord; { current number of video ports used }
    dwSVBCaps2: LongWord; { more driver specific capabilities for System->Vmem blts }
    dwNLVBCaps: LongWord; { driver specific capabilities for non-local->local vidmem blts }
    dwNLVBCaps2: LongWord; { more driver specific capabilities non-local->local vidmem blts }
    dwNLVBCKeyCaps: LongWord; { driver color key capabilities for non-local->local vidmem blts }
    dwNLVBFXCaps: LongWord; { driver FX capabilities for non-local->local blts }
    dwNLVBRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported for non-local->local blts }
  end;

  PDDCAPS_DX6 = ^DDCAPS_DX6;
  DDCAPS_DX6 = record
    dwSize: LongWord; { size of the DDDRIVERCAPS structure }
    dwCaps: LongWord; { driver specific capabilities }
    dwCaps2: LongWord; { more driver specific capabilites }
    dwCKeyCaps: LongWord; { color key capabilities of the surface }
    dwFXCaps: LongWord; { driver specific stretching and effects capabilites }
    dwFXAlphaCaps: LongWord; { alpha caps }
    dwPalCaps: LongWord; { palette capabilities }
    dwSVCaps: LongWord; { stereo vision capabilities }
    dwAlphaBltConstBitDepths: LongWord; { DDBD_2,4,8 }
    dwAlphaBltPixelBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwAlphaBltSurfaceBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwAlphaOverlayConstBitDepths: LongWord; { DDBD_2,4,8 }
    dwAlphaOverlayPixelBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwAlphaOverlaySurfaceBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwZBufferBitDepths: LongWord; { DDBD_8,16,24,32 }
    dwVidMemTotal: LongWord; { total amount of video memory }
    dwVidMemFree: LongWord; { amount of free video memory }
    dwMaxVisibleOverlays: LongWord; { maximum number of visible overlays }
    dwCurrVisibleOverlays: LongWord; { current number of visible overlays }
    dwNumFourCCCodes: LongWord; { number of four cc codes }
    dwAlignBoundarySrc: LongWord; { source rectangle alignment }
    dwAlignSizeSrc: LongWord; { source rectangle byte size }
    dwAlignBoundaryDest: LongWord; { dest rectangle alignment }
    dwAlignSizeDest: LongWord; { dest rectangle byte size }
    dwAlignStrideAlign: LongWord; { stride alignment }
    dwRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported }
    ddsOldCaps: DDSCAPS; { Was DDSCAPS  ddsCaps. ddsCaps is of type DDSCAPS2 for DX6 }
    dwMinOverlayStretch: LongWord; { minimum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMaxOverlayStretch: LongWord; { maximum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMinLiveVideoStretch: LongWord; { minimum live video stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMaxLiveVideoStretch: LongWord; { maximum live video stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMinHwCodecStretch: LongWord; { minimum hardware codec stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMaxHwCodecStretch: LongWord; { maximum hardware codec stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwReserved1: LongWord; { reserved }
    dwReserved2: LongWord; { reserved }
    dwReserved3: LongWord; { reserved }
    dwSVBCaps: LongWord; { driver specific capabilities for System->Vmem blts }
    dwSVBCKeyCaps: LongWord; { driver color key capabilities for System->Vmem blts }
    dwSVBFXCaps: LongWord; { driver FX capabilities for System->Vmem blts }
    dwSVBRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported for System->Vmem blts }
    dwVSBCaps: LongWord; { driver specific capabilities for Vmem->System blts }
    dwVSBCKeyCaps: LongWord; { driver color key capabilities for Vmem->System blts }
    dwVSBFXCaps: LongWord; { driver FX capabilities for Vmem->System blts }
    dwVSBRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported for Vmem->System blts }
    dwSSBCaps: LongWord; { driver specific capabilities for System->System blts }
    dwSSBCKeyCaps: LongWord; { driver color key capabilities for System->System blts }
    dwSSBFXCaps: LongWord; { driver FX capabilities for System->System blts }
    dwSSBRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported for System->System blts }
    dwMaxVideoPorts: LongWord; { maximum number of usable video ports }
    dwCurrVideoPorts: LongWord; { current number of video ports used }
    dwSVBCaps2: LongWord; { more driver specific capabilities for System->Vmem blts }
    dwNLVBCaps: LongWord; { driver specific capabilities for non-local->local vidmem blts }
    dwNLVBCaps2: LongWord; { more driver specific capabilities non-local->local vidmem blts }
    dwNLVBCKeyCaps: LongWord; { driver color key capabilities for non-local->local vidmem blts }
    dwNLVBFXCaps: LongWord; { driver FX capabilities for non-local->local blts }
    dwNLVBRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported for non-local->local blts }
    { Members added for DX6 release }
    ddsCaps: DDSCAPS2; { Surface Caps }
  end;

  PDDCAPS_DX7 = ^DDCAPS_DX7;
  DDCAPS_DX7 = record
    dwSize: LongWord; { size of the DDDRIVERCAPS structure }
    dwCaps: LongWord; { driver specific capabilities }
    dwCaps2: LongWord; { more driver specific capabilites }
    dwCKeyCaps: LongWord; { color key capabilities of the surface }
    dwFXCaps: LongWord; { driver specific stretching and effects capabilites }
    dwFXAlphaCaps: LongWord; { alpha driver specific capabilities }
    dwPalCaps: LongWord; { palette capabilities }
    dwSVCaps: LongWord; { stereo vision capabilities }
    dwAlphaBltConstBitDepths: LongWord; { DDBD_2,4,8 }
    dwAlphaBltPixelBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwAlphaBltSurfaceBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwAlphaOverlayConstBitDepths: LongWord; { DDBD_2,4,8 }
    dwAlphaOverlayPixelBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwAlphaOverlaySurfaceBitDepths: LongWord; { DDBD_1,2,4,8 }
    dwZBufferBitDepths: LongWord; { DDBD_8,16,24,32 }
    dwVidMemTotal: LongWord; { total amount of video memory }
    dwVidMemFree: LongWord; { amount of free video memory }
    dwMaxVisibleOverlays: LongWord; { maximum number of visible overlays }
    dwCurrVisibleOverlays: LongWord; { current number of visible overlays }
    dwNumFourCCCodes: LongWord; { number of four cc codes }
    dwAlignBoundarySrc: LongWord; { source rectangle alignment }
    dwAlignSizeSrc: LongWord; { source rectangle byte size }
    dwAlignBoundaryDest: LongWord; { dest rectangle alignment }
    dwAlignSizeDest: LongWord; { dest rectangle byte size }
    dwAlignStrideAlign: LongWord; { stride alignment }
    dwRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported }
    ddsOldCaps: DDSCAPS; { Was DDSCAPS  ddsCaps. ddsCaps is of type DDSCAPS2 for DX6 }
    dwMinOverlayStretch: LongWord; { minimum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMaxOverlayStretch: LongWord; { maximum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMinLiveVideoStretch: LongWord; { minimum live video stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMaxLiveVideoStretch: LongWord; { maximum live video stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMinHwCodecStretch: LongWord; { minimum hardware codec stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwMaxHwCodecStretch: LongWord; { maximum hardware codec stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3 }
    dwReserved1: LongWord; { reserved }
    dwReserved2: LongWord; { reserved }
    dwReserved3: LongWord; { reserved }
    dwSVBCaps: LongWord; { driver specific capabilities for System->Vmem blts }
    dwSVBCKeyCaps: LongWord; { driver color key capabilities for System->Vmem blts }
    dwSVBFXCaps: LongWord; { driver FX capabilities for System->Vmem blts }
    dwSVBRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported for System->Vmem blts }
    dwVSBCaps: LongWord; { driver specific capabilities for Vmem->System blts }
    dwVSBCKeyCaps: LongWord; { driver color key capabilities for Vmem->System blts }
    dwVSBFXCaps: LongWord; { driver FX capabilities for Vmem->System blts }
    dwVSBRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported for Vmem->System blts }
    dwSSBCaps: LongWord; { driver specific capabilities for System->System blts }
    dwSSBCKeyCaps: LongWord; { driver color key capabilities for System->System blts }
    dwSSBFXCaps: LongWord; { driver FX capabilities for System->System blts }
    dwSSBRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported for System->System blts }
    dwMaxVideoPorts: LongWord; { maximum number of usable video ports }
    dwCurrVideoPorts: LongWord; { current number of video ports used }
    dwSVBCaps2: LongWord; { more driver specific capabilities for System->Vmem blts }
    dwNLVBCaps: LongWord; { driver specific capabilities for non-local->local vidmem blts }
    dwNLVBCaps2: LongWord; { more driver specific capabilities non-local->local vidmem blts }
    dwNLVBCKeyCaps: LongWord; { driver color key capabilities for non-local->local vidmem blts }
    dwNLVBFXCaps: LongWord; { driver FX capabilities for non-local->local blts }
    dwNLVBRops: array[0..DD_ROP_SPACE - 1] of LongWord; { ROPS supported for non-local->local blts }
    { Members added for DX6 release }
    ddsCaps: DDSCAPS2; { Surface Caps }
  end;

  PDDCAPS = ^DDCAPS;

{$IF DIRECTDRAW_VERSION <= $300}
  DDCAPS = DDCAPS_DX3;
{$ELSEIF DIRECTDRAW_VERSION <= $500}
  DDCAPS = DDCAPS_DX5;
{$ELSEIF DIRECTDRAW_VERSION <= $600}
  DDCAPS = DDCAPS_DX6;
{$ELSE}
  DDCAPS = DDCAPS_DX7;
{$ENDIF}

{ DDPIXELFORMAT }
  PDDPIXELFORMAT = ^DDPIXELFORMAT;
  DDPIXELFORMAT = record
    dwSize: LongWord; { size of structure }
    dwFlags: LongWord; { pixel format flags }
    dwFourCC: LongWord; { (FOURCC code) }
    case Integer of
      0: (dwRGBBitCount: LongWord; { how many bits per pixel }
          dwRBitMask: LongWord; { mask for red bit }
          dwGBitMask: LongWord; { mask for green bits }
          dwBBitMask: LongWord; { mask for blue bits }
          case Integer of
            0: (dwRGBAlphaBitMask: LongWord); { mask for alpha channel }
            1: (dwYUVZBitMask: LongWord)); { mask for Z channel }
      1: (dwYUVBitCount: LongWord; { how many bits per pixel }
          dwYBitMask: LongWord; { mask for Y bits }
          dwUBitMask: LongWord; { mask for U bits }
          dwVBitMask: LongWord; { mask for V bits }
          dwYUVAlphaBitMask: LongWord); { mask for alpha channel }
      2: (dwZBufferBitDepth: LongWord; { how many total bits/pixel in z buffer (including any stencil bits) }
          dwStencilBitDepth: LongWord; { how many stencil bits (note: dwZBufferBitDepth-dwStencilBitDepth is total Z-only bits) }
          dwZBitMask: LongWord; { mask for Z bits }
          dwStencilBitMask: LongWord; { mask for stencil bits }
          dwLuminanceAlphaBitMask: LongWord); { mask for alpha channel }
      3: (dwAlphaBitDepth: LongWord; { how many bits for alpha channels }
          dwLuminanceBitMask: LongWord; { mask for luminance bits }
          dwBumpDvBitMask: LongWord; { mask for bump map V delta bits }
          dwBumpLuminanceBitMask: LongWord; { mask for luminance in bump map }
          dwRGBZBitMask: LongWord); { mask for Z channel }
      4: (dwLuminanceBitCount: LongWord; { how many bits per pixel }
          dwBumpDuBitMask: LongWord; { mask for bump map U delta bits }
          wFlipMSTypes: Word; { Multisample methods supported via flip for this D3DFORMAT }
          wBltMSTypes: Word); { Multisample methods supported via blt for this D3DFORMAT }
      5: (dwBumpBitCount: LongWord; { how many bits per "buxel", total }
          dwOperations: LongWord); { DDPF_D3DFORMAT Operations }
      6: (dwPrivateFormatBitCount: LongWord); { Bits per pixel of private driver formats. Only valid in texture
                                                format list and if DDPF_D3DFORMAT is set. }
  end;

{ DDOVERLAYFX }
  PDDOVERLAYFX = ^DDOVERLAYFX;
  DDOVERLAYFX = record
    dwSize: LongWord; { size of structure }
    dwAlphaEdgeBlendBitDepth: LongWord; { Bit depth used to specify constant for alpha edge blend }
    dwAlphaEdgeBlend: LongWord; { Constant to use as alpha for edge blend }
    dwReserved: LongWord;
    dwAlphaDestConstBitDepth: LongWord; { Bit depth used to specify alpha constant for destination }
    case Integer of
      0: (dwAlphaDestConst: LongWord); { Constant to use as alpha channel for dest }
      1: (lpDDSAlphaDest: ^IDirectDrawSurface; { Surface to use as alpha channel for dest }
          dwAlphaSrcConstBitDepth: LongWord; { Bit depth used to specify alpha constant for source }
          case Integer of
            0: (dwAlphaSrcConst: LongWord); { Constant to use as alpha channel for src }
            1: (lpDDSAlphaSrc: ^IDirectDrawSurface; { Surface to use as alpha channel for src }
                dckDestColorkey: DDCOLORKEY; { DestColorkey override }
                dckSrcColorkey: DDCOLORKEY; { DestColorkey override }
                dwDDFX: LongWord; { Overlay FX }
                dwFlags: LongWord)); { flags }
  end;

{ DDBLTBATCH: BltBatch entry structure }
  PDDBLTBATCH = ^DDBLTBATCH;
  DDBLTBATCH = record
    lprDest: PRect;
    lpDDSSrc: ^IDirectDrawSurface;
    lprSrc: PRect;
    dwFlags: LongWord;
    lpDDBltFx: PDDBLTFX;
  end;

{ DDGAMMARAMP }
  PDDGAMMARAMP = ^DDGAMMARAMP;
  DDGAMMARAMP = record
    red: array[0..255] of Word;
    green: array[0..255] of Word;
    blue: array[0..255] of Word;
  end;

{ This is the structure within which DirectDraw returns data about the current graphics driver and chipset }
  PDDDEVICEIDENTIFIER = ^DDDEVICEIDENTIFIER;
  DDDEVICEIDENTIFIER = record
{ These elements are for presentation to the user only. They should not be used to identify particular drivers, since
  this is unreliable and many different strings may be associated with the same device, and the same driver from
  different vendors. }
    szDriver: array[0..MAX_DDDEVICEID_STRING - 1] of Char;
    szDescription: array[0..MAX_DDDEVICEID_STRING - 1] of Char;

  { This element is the version of the DirectDraw/3D driver. It is legal to do <, > comparisons on the whole 64 bits.
    Caution should be exercised if you use this element to identify problematic drivers. It is recommended that
    guidDeviceIdentifier is used for this purpose.

    This version has the form:
      wProduct = HIWORD(liDriverVersion.HighPart)
      wVersion = LOWORD(liDriverVersion.HighPart)
      wSubVersion = HIWORD(liDriverVersion.LowPart)
      wBuild = LOWORD(liDriverVersion.LowPart) }
    case Integer of
      0: (dwDriverVersionLowPart: LongWord; { Defined for 16 bit driver components }
          dwDriverVersionHighPart: LongWord); { Defined for 16 bit driver components }
      1: (liDriverVersion: UInt64; { Defined for applications and other 32 bit components }

  { These elements can be used to identify particular chipsets. Use with extreme caution.
      dwVendorId    Identifies the manufacturer. May be zero if unknown.
      dwDeviceId    Identifies the type of chipset. May be zero if unknown.
      dwSubSysId    Identifies the subsystem, typically this means the particular board. May be zero if unknown.
      dwRevision    Identifies the revision level of the chipset. May be zero if unknown. }
    dwVendorId: LongWord;
    dwDeviceId: LongWord;
    dwSubSysId: LongWord;
    dwRevision: LongWord;

  { This element can be used to check changes in driver/chipset. This GUID is a unique identifier for the
    driver/chipset pair. Use this element if you wish to track changes to the driver/chipset in order to
    reprofile the graphics subsystem. This element can also be used to identify particular problematic drivers. }
    guidDeviceIdentifier: TGuid);
  end;

  PDDDEVICEIDENTIFIER2 = ^DDDEVICEIDENTIFIER2;
  DDDEVICEIDENTIFIER2 = record
  { These elements are for presentation to the user only. They should not be used to identify particular drivers,
    since this is unreliable and many different strings may be associated with the same device, and the same driver
    from different vendors. }
    szDriver: array[0..MAX_DDDEVICEID_STRING - 1] of Char;
    szDescription: array[0..MAX_DDDEVICEID_STRING - 1] of Char;

  { This element is the version of the DirectDraw/3D driver. It is legal to do <, > comparisons on the whole 64 bits.
    Caution should be exercised if you use this element to identify problematic drivers. It is recommended that
    guidDeviceIdentifier is used for this purpose.

    This version has the form:
      wProduct = HIWORD(liDriverVersion.HighPart)
      wVersion = LOWORD(liDriverVersion.HighPart)
      wSubVersion = HIWORD(liDriverVersion.LowPart)
      wBuild = LOWORD(liDriverVersion.LowPart) }
    case Integer of
      0: (dwDriverVersionLowPart: LongWord; { Defined for 16 bit driver components }
          dwDriverVersionHighPart: LongWord); { Defined for 16 bit driver components }
      1: (liDriverVersion: UInt64; { Defined for applications and other 32 bit components }

  { These elements can be used to identify particular chipsets. Use with extreme caution.
      dwVendorId    Identifies the manufacturer. May be zero if unknown.
      dwDeviceId    Identifies the type of chipset. May be zero if unknown.
      dwSubSysId    Identifies the subsystem, typically this means the particular board. May be zero if unknown.
      dwRevision    Identifies the revision level of the chipset. May be zero if unknown. }
    dwVendorId: LongWord;
    dwDeviceId: LongWord;
    dwSubSysId: LongWord;
    dwRevision: LongWord;

  { This element can be used to check changes in driver/chipset. This GUID is a unique identifier for the
    driver/chipset pair. Use this element if you wish to track changes to the driver/chipset in order to
    reprofile the graphics subsystem. This element can also be used to identify particular problematic drivers. }
    guidDeviceIdentifier: TGuid;

  { This element is used to determine the Windows Hardware Quality Lab (WHQL) certification level for this
    driver/device pair. }
    dwWHQLLevel: LongWord);
  end;

{ DDCOLORCONTROL }
  PDDCOLORCONTROL = ^DDCOLORCONTROL;
  DDCOLORCONTROL = record
    dwSize: LongWord;
    dwFlags: LongWord;
    lBrightness: LongInt;
    lContrast: LongInt;
    lHue: LongInt;
    lSaturation: LongInt;
    lSharpness: LongInt;
    lGamma: LongInt;
    lColorEnable: LongInt;
    dwReserved1: LongWord;
  end;

{ DDSURFACEDESC }
  PDDSURFACEDESC = ^DDSURFACEDESC;
  DDSURFACEDESC = record
    dwSize: LongWord; { size of the DDSURFACEDESC structure}
    dwFlags: LongWord; { determines what fields are valid }
    dwHeight: LongWord; { height of surface to be created }
    dwWidth: LongWord; { width of input surface }
    case Integer of
      0: (lPitch: LongInt; { distance to start of next line (return value only) }
          dwBackBufferCount: LongWord; { number of back buffers requested }
          case Integer of
            0: (dwMipMapCount: LongWord; { number of mip-map levels requested }
                dwAlphaBitDepth: LongWord; { depth of alpha buffer requested }
                dwReserved: LongWord; { reserved }
                lpSurface: Pointer; { pointer to the associated surface memory }
                ddckCKDestOverlay: DDCOLORKEY; { color key for destination overlay use }
                ddckCKDestBlt: DDCOLORKEY; { color key for destination blt use }
                ddckCKSrcOverlay: DDCOLORKEY; { color key for source overlay use }
                ddckCKSrcBlt: DDCOLORKEY; { color key for source blt use }
                ddpfPixelFormat: DDPIXELFORMAT; { pixel format description of the surface }
                ddsCaps: DDSCAPS); { direct draw surface capabilities }
            1: (dwZBufferBitDepth: LongWord); { depth of Z buffer requested }
            2: (dwRefreshRate: LongWord)); { refresh rate (used when display mode is described) }
      1: (dwLinearSize: LongWord); { Formless late-allocated optimized surface size }
  end;

{ DDSURFACEDESC2 }
  PDDSURFACEDESC2 = ^DDSURFACEDESC2;
  DDSURFACEDESC2 = record
    dwSize: LongWord; { size of the DDSURFACEDESC structure }
    dwFlags: LongWord; { determines what fields are valid }
    dwHeight: LongWord; { height of surface to be created }
    dwWidth: LongWord; { width of input surface }
    case Integer of
      0: (lPitch: LongInt; { distance to start of next line (return value only) }
          dwBackBufferCount: LongWord; { number of back buffers requested }
          case Integer of
            0: (dwMipMapCount: LongWord; { number of mip-map levels requested }
                { dwZBufferBitDepth removed, use ddpfPixelFormat one instead }
                dwAlphaBitDepth: LongWord; { depth of alpha buffer requested }
                dwReserved: LongWord; { reserved }
                lpSurface: Pointer; { pointer to the associated surface memory }
                case Integer of
                  0: (ddckCKDestOverlay: DDCOLORKEY; { color key for destination overlay use }
                      ddckCKDestBlt: DDCOLORKEY; { color key for destination blt use }
                      ddckCKSrcOverlay: DDCOLORKEY; { color key for source overlay use }
                      ddckCKSrcBlt: DDCOLORKEY; { color key for source blt use }
                      case Integer of
                        0: (ddpfPixelFormat: DDPIXELFORMAT; { pixel format description of the surface }
                            ddsCaps: DDSCAPS2; { direct draw surface capabilities }
                            dwTextureStage: LongWord); { stage in multitexture cascade }
                        1: (dwFVF: LongWord)); { vertex format description of vertex buffers}
                  1: (dwEmptyFaceColor: LongWord)); { Physical color for empty cubemap faces}
            1: (dwSrcVBHandle: LongWord)); { The source used in VB::Optimize }
      1: (dwLinearSize: LongWord; { Formless late-allocated optimized surface size }
          dwDepth: LongWord; { the depth if this is a volume texture }
          dwRefreshRate: LongWord); { refresh rate (used when display mode is described) }
  end;

{ DDOPTSURFACEDESC}
  PDDOPTSURFACEDESC = ^DDOPTSURFACEDESC;
  DDOPTSURFACEDESC = record
    dwSize: LongWord; { size of the DDOPTSURFACEDESC structure }
    dwFlags: LongWord; { determines what fields are valid }
    ddSCaps: DDSCAPS2; { Common caps like: Memory type }
    ddOSCaps: DDOSCAPS; { Common caps like: Memory type }
    guid: TGuid; { Compression technique GUID }
    dwCompressionRatio: LongWord; { Compression ratio }
  end;

  TDDEnumModesCallback = function(const lpDDSurfaceDesc: DDSURFACEDESC; lpContext: Pointer): HResult; stdcall;
  TDDEnumModesCallback2 = function(const lpDDSurfaceDesc: DDSURFACEDESC2; lpContext: Pointer): HResult; stdcall;
  TDDEnumSurfacesCallback = function(lpDDSurface: IDirectDrawSurface; const lpDDSurfaceDesc: DDSURFACEDESC;
    lpContext: Pointer): HResult; stdcall;
  TDDEnumSurfacesCallback2 = function(lpDDSurface: IDirectDrawSurface4; const lpDDSurfaceDesc: DDSURFACEDESC2;
    lpContext: Pointer): HResult; stdcall;
  TDDEnumSurfacesCallback7 = function(lpDDSurface: IDirectDrawSurface7; const lpDDSurfaceDesc: DDSURFACEDESC2;
    lpContext: Pointer): HResult; stdcall;
  TDDEnumCallbackA = function(lpGUID: PGuid; lpDriverDescription, lpDriverName: PAnsiChar;
    lpContext: Pointer): LongBool; stdcall;
  TDDEnumCallbackW = function(lpGUID: PGuid; lpDriverDescription, lpDriverName: PWideChar;
    lpContext: Pointer): LongBool; stdcall;
  TDDEnumCallbackExA = function(lpGUID: PGuid; lpDriverDescription, lpDriverName: PAnsiChar;
    lpContext: Pointer; Monitor: HMonitor): LongBool; stdcall;
  TDDEnumCallbackExW = function(lpGUID: PGuid; lpDriverDescription, lpDriverName: PWideChar;
    lpContext: Pointer; Monitor: HMonitor): LongBool; stdcall;

  PDirectDraw = ^IDirectDraw;
  IDirectDraw = interface(IUnknown)
    [SID_IDirectDraw]
    function Compact: HResult; stdcall;
    function CreateClipper(dwFlags: LongWord; out lplpDDClipper: IDirectDrawClipper;
      pUnkOuter: IUnknown): HResult; stdcall;
    function CreatePalette(dwFlags: LongWord; lpColorTable: PPaletteEntry; out lplpDDPalette: IDirectDrawPalette;
      pUnkOuter: IUnknown): HResult; stdcall;
    function CreateSurface(var lpDDSurfaceDesc: DDSURFACEDESC; out lplpDDSurface: IDirectDrawSurface;
      pUnkOuter: IUnknown): HResult; stdcall;
    function DuplicateSurface(lpDDSurface: IDirectDrawSurface;
      out lplpDupDDSurface: IDirectDrawSurface): HResult; stdcall;
    function EnumDisplayModes(dwFlags: LongWord; lpDDSurfaceDesc: PDDSURFACEDESC; lpContext: Pointer;
      lpEnumModesCallback: TDDEnumModesCallback): HResult; stdcall;
    function EnumSurfaces(dwFlags: LongWord; const lpDDSD: DDSURFACEDESC; lpContext: Pointer;
      lpEnumCallback: TDDEnumSurfacesCallback): HResult; stdcall;
    function FlipToGDISurface: HResult; stdcall;
    function GetCaps(lpDDDriverCaps, lpDDHELCaps: PDDCAPS): HResult; stdcall;
    function GetDisplayMode(out lpDDSurfaceDesc: DDSURFACEDESC): HResult; stdcall;
    function GetFourCCCodes(var lpNumCodes: LongWord; lpCodes: PLongWord): HResult; stdcall;
    function GetGDISurface(out lplpGDIDDSSurface: IDirectDrawSurface): HResult; stdcall;
    function GetMonitorFrequency(out lpdwFrequency: LongWord): HResult; stdcall;
    function GetScanLine(out lpdwScanLine: LongWord): HResult; stdcall;
    function GetVerticalBlankStatus(out lpbIsInVB: LongBool): HResult; stdcall;
    function Initialize(lpGUID: PGuid): HResult; stdcall;
    function RestoreDisplayMode: HResult; stdcall;
    function SetCooperativeLevel(hWnd: HWND; dwFlags: LongWord): HResult; stdcall;
    function SetDisplayMode(dwWidth, dwHeight, dwBpp: LongWord): HResult; stdcall;
    function WaitForVerticalBlank(dwFlags: LongWord; hEvent: THandle): HResult; stdcall;
  end;

  PDirectDraw2 = ^IDirectDraw2;
  IDirectDraw2 = interface(IUnknown)
    [SID_IDirectDraw2]
    function Compact: HResult; stdcall;
    function CreateClipper(dwFlags: LongWord; out lplpDDClipper: IDirectDrawClipper;
      pUnkOuter: IUnknown): HResult; stdcall;
    function CreatePalette(dwFlags: LongWord; lpColorTable: PPaletteEntry; out lplpDDPalette: IDirectDrawPalette;
      pUnkOuter: IUnknown): HResult; stdcall;
    function CreateSurface(var lpDDSurfaceDesc: DDSURFACEDESC; out lplpDDSurface: IDirectDrawSurface;
      pUnkOuter: IUnknown): HResult; stdcall;
    function DuplicateSurface(lpDDSurface: IDirectDrawSurface;
      out lplpDupDDSurface: IDirectDrawSurface): HResult; stdcall;
    function EnumDisplayModes(dwFlags: LongWord; lpDDSurfaceDesc: PDDSURFACEDESC; lpContext: Pointer;
      lpEnumModesCallback: TDDEnumModesCallback): HResult; stdcall;
    function EnumSurfaces(dwFlags: LongWord; const lpDDSD: DDSURFACEDESC; lpContext: Pointer;
      lpEnumCallback: TDDEnumSurfacesCallback): HResult; stdcall;
    function FlipToGDISurface: HResult; stdcall;
    function GetCaps(lpDDDriverCaps, lpDDHELCaps: PDDCAPS): HResult; stdcall;
    function GetDisplayMode(out lpDDSurfaceDesc: DDSURFACEDESC): HResult; stdcall;
    function GetFourCCCodes(var lpNumCodes: LongWord; lpCodes: PLongWord): HResult; stdcall;
    function GetGDISurface(out lplpGDIDDSSurface: IDirectDrawSurface): HResult; stdcall;
    function GetMonitorFrequency(out lpdwFrequency: LongWord): HResult; stdcall;
    function GetScanLine(out lpdwScanLine: LongWord): HResult; stdcall;
    function GetVerticalBlankStatus(out lpbIsInVB: LongBool): HResult; stdcall;
    function Initialize(lpGUID: PGuid): HResult; stdcall;
    function RestoreDisplayMode: HResult; stdcall;
    function SetCooperativeLevel(hWnd: HWND; dwFlags: LongWord): HResult; stdcall;
    function SetDisplayMode(dwWidth, dwHeight, dwBPP, dwRefreshRate, dwFlags: LongWord): HResult; stdcall;
    function WaitForVerticalBlank(dwFlags: LongWord; hEvent: THandle): HResult; stdcall;
    { Added in the v2 interface }
    function GetAvailableVidMem(var lpDDSCaps: DDSCAPS; out lpdwTotal, lpdwFree: LongWord): HResult; stdcall;
  end;

  PDirectDraw4 = ^IDirectDraw4;
  IDirectDraw4 = interface(IUnknown)
    [SID_IDirectDraw4]
    function Compact: HResult; stdcall;
    function CreateClipper(dwFlags: LongWord; out lplpDDClipper: IDirectDrawClipper;
      pUnkOuter: IUnknown): HResult; stdcall;
    function CreatePalette(dwFlags: LongWord; lpColorTable: PPaletteEntry; out lplpDDPalette: IDirectDrawPalette;
      pUnkOuter: IUnknown): HResult; stdcall;
    function CreateSurface(const lpDDSurfaceDesc: DDSURFACEDESC2; out lplpDDSurface: IDirectDrawSurface4;
      pUnkOuter: IUnknown): HResult; stdcall;
    function DuplicateSurface(lpDDSurface: IDirectDrawSurface4;
      out lplpDupDDSurface: IDirectDrawSurface4): HResult; stdcall;
    function EnumDisplayModes(dwFlags: LongWord; lpDDSurfaceDesc: PDDSURFACEDESC2; lpContext: Pointer;
      Param4: TDDEnumModesCallback2): HResult; stdcall;
    function EnumSurfaces(dwFlags: LongWord; const lpDDSD: DDSURFACEDESC2; lpContext: Pointer;
      lpEnumCallback: TDDEnumSurfacesCallback2): HResult; stdcall;
    function FlipToGDISurface: HResult; stdcall;
    function GetCaps(lpDDDriverCaps, lpDDHELCaps: PDDCAPS): HResult; stdcall;
    function GetDisplayMode(out lpDDSurfaceDesc: DDSURFACEDESC2): HResult; stdcall;
    function GetFourCCCodes(var lpNumCodes: LongWord; lpCodes: PLongWord): HResult; stdcall;
    function GetGDISurface(out lplpGDIDDSSurface: IDirectDrawSurface4): HResult; stdcall;
    function GetMonitorFrequency(out lpdwFrequency: LongWord): HResult; stdcall;
    function GetScanLine(out lpdwScanLine: LongWord): HResult; stdcall;
    function GetVerticalBlankStatus(out lpbIsInVB: LongBool): HResult; stdcall;
    function Initialize(lpGUID: PGuid): HResult; stdcall;
    function RestoreDisplayMode: HResult; stdcall;
    function SetCooperativeLevel(hWnd: HWND; dwFlags: LongWord): HResult; stdcall;
    function SetDisplayMode(dwWidth, dwHeight, dwBPP, dwRefreshRate, dwFlags: LongWord): HResult; stdcall;
    function WaitForVerticalBlank(dwFlags: LongWord; hEvent: THandle): HResult; stdcall;
    { Added in the v2 interface }
    function GetAvailableVidMem(const lpDDSCaps: DDSCAPS2; out lpdwTotal, lpdwFree: LongWord): HResult; stdcall;
    { Added in the v4 Interface }
    function GetSurfaceFromDC(hdc: HDC; out lpDDS4: IDirectDrawSurface4): HResult; stdcall;
    function RestoreAllSurfaces: HResult; stdcall;
    function TestCooperativeLevel: HResult; stdcall;
    function GetDeviceIdentifier(out lpdddi: DDDEVICEIDENTIFIER; dwFlags: LongWord): HResult; stdcall;
  end;

  PDirectDraw7 = ^IDirectDraw7;
  IDirectDraw7 = interface(IUnknown)
    [SID_IDirectDraw7]
    function Compact: HResult; stdcall;
    function CreateClipper(dwFlags: LongWord; out lplpDDClipper: IDirectDrawClipper;
      pUnkOuter: IUnknown): HResult; stdcall;
    function CreatePalette(dwFlags: LongWord; lpColorTable: PPaletteEntry; out lplpDDPalette: IDirectDrawPalette;
      pUnkOuter: IUnknown): HResult; stdcall;
    function CreateSurface(const lpDDSurfaceDesc: DDSURFACEDESC2; out lplpDDSurface: IDirectDrawSurface7;
      pUnkOuter: IUnknown): HResult; stdcall;
    function DuplicateSurface(lpDDSurface: IDirectDrawSurface7;
      out lplpDupDDSurface: IDirectDrawSurface7): HResult; stdcall;
    function EnumDisplayModes(dwFlags: LongWord; lpDDSurfaceDesc: PDDSURFACEDESC2; lpContext: Pointer;
      Param4: TDDEnumModesCallback2): HResult; stdcall;
    function EnumSurfaces(dwFlags: LongWord; const lpDDSD: DDSURFACEDESC2; lpContext: Pointer;
      lpEnumCallback: TDDEnumSurfacesCallback7): HResult; stdcall;
    function FlipToGDISurface: HResult; stdcall;
    function GetCaps(lpDDDriverCaps, lpDDHELCaps: PDDCAPS): HResult; stdcall;
    function GetDisplayMode(out lpDDSurfaceDesc: DDSURFACEDESC2): HResult; stdcall;
    function GetFourCCCodes(var lpNumCodes: LongWord; lpCodes: PLongWord): HResult; stdcall;
    function GetGDISurface(out lplpGDIDDSSurface: IDirectDrawSurface7): HResult; stdcall;
    function GetMonitorFrequency(out lpdwFrequency: LongWord): HResult; stdcall;
    function GetScanLine(out lpdwScanLine: LongWord): HResult; stdcall;
    function GetVerticalBlankStatus(out lpbIsInVB: LongBool): HResult; stdcall;
    function Initialize(lpGUID: PGuid): HResult; stdcall;
    function RestoreDisplayMode: HResult; stdcall;
    function SetCooperativeLevel(hWnd: HWND; dwFlags: LongWord): HResult; stdcall;
    function SetDisplayMode(dwWidth, dwHeight, dwBPP, dwRefreshRate, dwFlags: LongWord): HResult; stdcall;
    function WaitForVerticalBlank(dwFlags: LongWord; hEvent: THandle): HResult; stdcall;
    { Added in the v2 interface }
    function GetAvailableVidMem(const lpDDSCaps: DDSCAPS2; out lpdwTotal, lpdwFree: LongWord): HResult; stdcall;
    { Added in the v4 Interface }
    function GetSurfaceFromDC(hdc: HDC; out lpDDS: IDirectDrawSurface7): HResult; stdcall;
    function RestoreAllSurfaces: HResult; stdcall;
    function TestCooperativeLevel: HResult; stdcall;
    function GetDeviceIdentifier(out lpdddi: PDDDEVICEIDENTIFIER2; dwFlags: LongWord): HResult; stdcall;
    { Added in the v7 interface }
    function StartModeTest(const lpModesToTest: TSize; dwNumEntries, dwFlags: LongWord): HResult; stdcall;
    function EvaluateMode(dwFlags: LongWord; out pSecondsUntilTimeout: LongWord): HResult; stdcall;
  end;

  PDirectDrawPalette = ^IDirectDrawPalette;
  IDirectDrawPalette = interface(IUnknown)
    [SID_IDirectDrawPalette]
    function GetCaps(out lpdwCaps: LongWord): HResult; stdcall;
    function GetEntries(dwFlags, dwBase, dwNumEntries: LongWord; lpEntries: PPaletteEntry): HResult; stdcall;
    function Initialize(lpDD: IDirectDraw; dwFlags: LongWord; lpDDColorTable: PPaletteEntry): HResult; stdcall;
    function SetEntries(dwFlags, dwStartingEntry, dwCount: LongWord; lpEntries: PPaletteEntry): HResult; stdcall;
  end;

  PDirectDrawClipper = ^IDirectDrawClipper;
  IDirectDrawClipper = interface(IUnknown)
    [SID_IDirectDrawClipper]
    function GetClipList(lpRect: PRect; lpClipList: PRgnData; var lpdwSize: LongWord): HResult; stdcall;
    function GetHWnd(out lphWnd: HWND): HResult; stdcall;
    function Initialize(lpDD: IDirectDraw; dwFlags: LongWord): HResult; stdcall;
    function IsClipListChanged(out lpbChanged: LongBool): HResult; stdcall;
    function SetClipList(lpClipList: PRgnData; dwFlags: LongWord): HResult; stdcall;
    function SetHWnd(dwFlags: LongWord; hWnd: HWND): HResult; stdcall;
  end;

  PDirectDrawSurface = ^IDirectDrawSurface;
  IDirectDrawSurface = interface(IUnknown)
    [SID_IDirectDrawSurface]
    function AddAttachedSurface(lpDDSAttachedSurface: IDirectDrawSurface): HResult; stdcall;
    function AddOverlayDirtyRect(const lpRect: TRect): HResult; stdcall;
    function Blt(lpDestRect: PRect; lpDDSrcSurface: IDirectDrawSurface; lpSrcRect: PRect; dwFlags: LongWord;
      lpDDBltFx: PDDBLTFX): HResult; stdcall;
    function BltBatch(const lpDDBltBatch: DDBLTBATCH; dwCount, dwFlags: LongWord): HResult; stdcall;
    function BltFast(dwX, dwY: LongWord; lpDDSrcSurface: IDirectDrawSurface; lpSrcRect: PRect;
      dwTrans: LongWord): HResult; stdcall;
    function DeleteAttachedSurface(dwFlags: LongWord; lpDDSAttachedSurface: IDirectDrawSurface): HResult; stdcall;
    function EnumAttachedSurfaces(lpContext: Pointer;
      lpEnumSurfacesCallback: TDDEnumSurfacesCallback): HResult; stdcall;
    function EnumOverlayZOrders(dwFlags: LongWord; lpContext: Pointer;
      lpfnCallback: TDDEnumSurfacesCallback): HResult; stdcall;
    function Flip(lpDDSurfaceTargetOverride: IDirectDrawSurface; dwFlags: LongWord): HResult; stdcall;
    function GetAttachedSurface(var lpDDSCaps: DDSCAPS;
      out lplpDDAttachedSurface: IDirectDrawSurface): HResult; stdcall;
    function GetBltStatus(dwFlags: LongWord): HResult; stdcall;
    function GetCaps(out lpDDSCaps: DDSCAPS): HResult; stdcall;
    function GetClipper(out lplpDDClipper: IDirectDrawClipper): HResult; stdcall;
    function GetColorKey(dwFlags: LongWord; out lpDDColorKey: DDCOLORKEY): HResult; stdcall;
    function GetDC(out lphDC: HDC): HResult; stdcall;
    function GetFlipStatus(dwFlags: LongWord): HResult; stdcall;
    function GetOverlayPosition(out lplX, lplY: LongInt): HResult; stdcall;
    function GetPalette(out lplpDDPalette: IDirectDrawPalette): HResult; stdcall;
    function GetPixelFormat(out lpDDPixelFormat: DDPIXELFORMAT): HResult; stdcall;
    function GetSurfaceDesc(out lpDDSurfaceDesc: DDSURFACEDESC): HResult; stdcall;
    function Initialize(lpDD: IDirectDraw; out lpDDSurfaceDesc: DDSURFACEDESC): HResult; stdcall;
    function IsLost: HResult; stdcall;
    function Lock(lpDestRect: PRect; out lpDDSurfaceDesc: DDSURFACEDESC; dwFlags: LongWord;
      hEvent: THandle): HResult; stdcall;
    function ReleaseDC(hDC: HDC): HResult; stdcall;
    function Restore: HResult; stdcall;
    function SetClipper(lpDDClipper: IDirectDrawClipper): HResult; stdcall;
    function SetColorKey(dwFlags: LongWord; lpDDColorKey: PDDCOLORKEY): HResult; stdcall;
    function SetOverlayPosition(lX, lY: LongInt): HResult; stdcall;
    function SetPalette(lpDDPalette: IDirectDrawPalette): HResult; stdcall;
    function Unlock(lpSurfaceData: Pointer): HResult; stdcall;
    function UpdateOverlay(lpSrcRect: PRect; lpDDDestSurface: IDirectDrawSurface; lpDestRect: PRect;
      dwFlags: LongWord; lpDDOverlayFx: PDDOVERLAYFX): HResult; stdcall;
    function UpdateOverlayDisplay(dwFlags: LongWord): HResult; stdcall;
    function UpdateOverlayZOrder(dwFlags: LongWord; lpDDSReference: IDirectDrawSurface): HResult; stdcall;
  end;

  PDirectDrawSurface2 = ^IDirectDrawSurface2;
  IDirectDrawSurface2 = interface(IUnknown)
    [SID_IDirectDrawSurface2]
    function AddAttachedSurface(lpDDSAttachedSurface: IDirectDrawSurface2): HResult; stdcall;
    function AddOverlayDirtyRect(const lpRect: TRect): HResult; stdcall;
    function Blt(lpDestRect: PRect; lpDDSrcSurface: IDirectDrawSurface2; lpSrcRect: PRect; dwFlags: LongWord;
      lpDDBltFx: PDDBLTFX): HResult; stdcall;
    function BltBatch(const lpDDBltBatch: DDBLTBATCH; dwCount, dwFlags: LongWord): HResult; stdcall;
    function BltFast(dwX, dwY: LongWord; lpDDSrcSurface: IDirectDrawSurface2; lpSrcRect: PRect;
      dwTrans: LongWord): HResult; stdcall;
    function DeleteAttachedSurface(dwFlags: LongWord; lpDDSAttachedSurface: IDirectDrawSurface2): HResult; stdcall;
    function EnumAttachedSurfaces(lpContext: Pointer;
      lpEnumSurfacesCallback: TDDEnumSurfacesCallback): HResult; stdcall;
    function EnumOverlayZOrders(dwFlags: LongWord; lpContext: Pointer;
      lpfnCallback: TDDEnumSurfacesCallback): HResult; stdcall;
    function Flip(lpDDSurfaceTargetOverride: IDirectDrawSurface2; dwFlags: LongWord): HResult; stdcall;
    function GetAttachedSurface(var lpDDSCaps: DDSCAPS;
      out lplpDDAttachedSurface: IDirectDrawSurface2): HResult; stdcall;
    function GetBltStatus(dwFlags: LongWord): HResult; stdcall;
    function GetCaps(out lpDDSCaps: DDSCAPS): HResult; stdcall;
    function GetClipper(out lplpDDClipper: IDirectDrawClipper): HResult; stdcall;
    function GetColorKey(dwFlags: LongWord; out lpDDColorKey: DDCOLORKEY): HResult; stdcall;
    function GetDC(out lphDC: HDC): HResult; stdcall;
    function GetFlipStatus(dwFlags: LongWord): HResult; stdcall;
    function GetOverlayPosition(out lplX, lplY: LongInt): HResult; stdcall;
    function GetPalette(out lplpDDPalette: IDirectDrawPalette): HResult; stdcall;
    function GetPixelFormat(out lpDDPixelFormat: DDPIXELFORMAT): HResult; stdcall;
    function GetSurfaceDesc(out lpDDSurfaceDesc: DDSURFACEDESC): HResult; stdcall;
    function Initialize(lpDD: IDirectDraw; out lpDDSurfaceDesc: DDSURFACEDESC): HResult; stdcall;
    function IsLost: HResult; stdcall;
    function Lock(lpDestRect: PRect; out lpDDSurfaceDesc: DDSURFACEDESC; dwFlags: LongWord;
      hEvent: THandle): HResult; stdcall;
    function ReleaseDC(hDC: HDC): HResult; stdcall;
    function Restore: HResult; stdcall;
    function SetClipper(lpDDClipper: IDirectDrawClipper): HResult; stdcall;
    function SetColorKey(dwFlags: LongWord; lpDDColorKey: PDDCOLORKEY): HResult; stdcall;
    function SetOverlayPosition(lX, lY: LongInt): HResult; stdcall;
    function SetPalette(lpDDPalette: IDirectDrawPalette): HResult; stdcall;
    function Unlock(lpSurfaceData: Pointer): HResult; stdcall;
    function UpdateOverlay(lpSrcRect: PRect; lpDDDestSurface: IDirectDrawSurface2; lpDestRect: PRect;
      dwFlags: LongWord; lpDDOverlayFx: PDDOVERLAYFX): HResult; stdcall;
    function UpdateOverlayDisplay(dwFlags: LongWord): HResult; stdcall;
    function UpdateOverlayZOrder(dwFlags: LongWord; lpDDSReference: IDirectDrawSurface2): HResult; stdcall;
    { Added in the v2 interface }
    function GetDDInterface(out lplpDD: IUnknown): HResult; stdcall;
    function PageLock(dwFlags: LongWord): HResult; stdcall;
    function PageUnlock(dwFlags: LongWord): HResult; stdcall;
  end;

  PDirectDrawSurface3 = ^IDirectDrawSurface3;
  IDirectDrawSurface3 = interface(IUnknown)
    [SID_IDirectDrawSurface3]
    function AddAttachedSurface(lpDDSAttachedSurface: IDirectDrawSurface3): HResult; stdcall;
    function AddOverlayDirtyRect(const lpRect: TRect): HResult; stdcall;
    function Blt(lpDestRect: PRect; lpDDSrcSurface: IDirectDrawSurface3; lpSrcRect: PRect; dwFlags: LongWord;
      lpDDBltFx: PDDBLTFX): HResult; stdcall;
    function BltBatch(const lpDDBltBatch: DDBLTBATCH; dwCount, dwFlags: LongWord): HResult; stdcall;
    function BltFast(dwX, dwY: LongWord; lpDDSrcSurface: IDirectDrawSurface3; lpSrcRect: PRect;
      dwTrans: LongWord): HResult; stdcall;
    function DeleteAttachedSurface(dwFlags: LongWord; lpDDSAttachedSurface: IDirectDrawSurface3): HResult; stdcall;
    function EnumAttachedSurfaces(lpContext: Pointer;
      lpEnumSurfacesCallback: TDDEnumSurfacesCallback): HResult; stdcall;
    function EnumOverlayZOrders(dwFlags: LongWord; lpContext: Pointer;
      lpfnCallback: TDDEnumSurfacesCallback): HResult; stdcall;
    function Flip(lpDDSurfaceTargetOverride: IDirectDrawSurface3; dwFlags: LongWord): HResult; stdcall;
    function GetAttachedSurface(var lpDDSCaps: DDSCAPS;
      out lplpDDAttachedSurface: IDirectDrawSurface3): HResult; stdcall;
    function GetBltStatus(dwFlags: LongWord): HResult; stdcall;
    function GetCaps(out lpDDSCaps: DDSCAPS): HResult; stdcall;
    function GetClipper(out lplpDDClipper: IDirectDrawClipper): HResult; stdcall;
    function GetColorKey(dwFlags: LongWord; out lpDDColorKey: DDCOLORKEY): HResult; stdcall;
    function GetDC(out lphDC: HDC): HResult; stdcall;
    function GetFlipStatus(dwFlags: LongWord): HResult; stdcall;
    function GetOverlayPosition(out lplX, lplY: LongInt): HResult; stdcall;
    function GetPalette(out lplpDDPalette: IDirectDrawPalette): HResult; stdcall;
    function GetPixelFormat(out lpDDPixelFormat: DDPIXELFORMAT): HResult; stdcall;
    function GetSurfaceDesc(out lpDDSurfaceDesc: DDSURFACEDESC): HResult; stdcall;
    function Initialize(lpDD: IDirectDraw; out lpDDSurfaceDesc: DDSURFACEDESC): HResult; stdcall;
    function IsLost: HResult; stdcall;
    function Lock(lpDestRect: PRect; out lpDDSurfaceDesc: DDSURFACEDESC; dwFlags: LongWord;
      hEvent: THandle): HResult; stdcall;
    function ReleaseDC(hDC: HDC): HResult; stdcall;
    function Restore: HResult; stdcall;
    function SetClipper(lpDDClipper: IDirectDrawClipper): HResult; stdcall;
    function SetColorKey(dwFlags: LongWord; lpDDColorKey: PDDCOLORKEY): HResult; stdcall;
    function SetOverlayPosition(lX, lY: LongInt): HResult; stdcall;
    function SetPalette(lpDDPalette: IDirectDrawPalette): HResult; stdcall;
    function Unlock(lpSurfaceData: Pointer): HResult; stdcall;
    function UpdateOverlay(lpSrcRect: PRect; lpDDDestSurface: IDirectDrawSurface3; lpDestRect: PRect;
      dwFlags: LongWord; lpDDOverlayFx: PDDOVERLAYFX): HResult; stdcall;
    function UpdateOverlayDisplay(dwFlags: LongWord): HResult; stdcall;
    function UpdateOverlayZOrder(dwFlags: LongWord; lpDDSReference: IDirectDrawSurface3): HResult; stdcall;
    { Added in the v2 interface }
    function GetDDInterface(out lplpDD: IUnknown): HResult; stdcall;
    function PageLock(dwFlags: LongWord): HResult; stdcall;
    function PageUnlock(dwFlags: LongWord): HResult; stdcall;
    { Added in the v3 interface }
    function SetSurfaceDesc(const lpddsd: DDSURFACEDESC; dwFlags: LongWord): HResult; stdcall;
  end;

  PDirectDrawSurface4 = ^IDirectDrawSurface4;
  IDirectDrawSurface4 = interface(IUnknown)
    [SID_IDirectDrawSurface4]
    function AddAttachedSurface(lpDDSAttachedSurface: IDirectDrawSurface4): HResult; stdcall;
    function AddOverlayDirtyRect(const lpRect: TRect): HResult; stdcall;
    function Blt(lpDestRect: PRect; lpDDSrcSurface: IDirectDrawSurface4; lpSrcRect: PRect; dwFlags: LongWord;
      lpDDBltFx: PDDBLTFX): HResult; stdcall;
    function BltBatch(const lpDDBltBatch: DDBLTBATCH; dwCount, dwFlags: LongWord): HResult; stdcall;
    function BltFast(dwX, dwY: LongWord; lpDDSrcSurface: IDirectDrawSurface4; lpSrcRect: PRect;
      dwTrans: LongWord): HResult; stdcall;
    function DeleteAttachedSurface(dwFlags: LongWord; lpDDSAttachedSurface: IDirectDrawSurface4): HResult; stdcall;
    function EnumAttachedSurfaces(lpContext: Pointer;
      lpEnumSurfacesCallback: TDDEnumSurfacesCallback2): HResult; stdcall;
    function EnumOverlayZOrders(dwFlags: LongWord; lpContext: Pointer;
      lpfnCallback: TDDEnumSurfacesCallback2): HResult; stdcall;
    function Flip(lpDDSurfaceTargetOverride: IDirectDrawSurface4; dwFlags: LongWord): HResult; stdcall;
    function GetAttachedSurface(const lpDDSCaps: DDSCAPS2;
      out lplpDDAttachedSurface: IDirectDrawSurface4): HResult; stdcall;
    function GetBltStatus(dwFlags: LongWord): HResult; stdcall;
    function GetCaps(out lpDDSCaps: DDSCAPS2): HResult; stdcall;
    function GetClipper(out lplpDDClipper: IDirectDrawClipper): HResult; stdcall;
    function GetColorKey(dwFlags: LongWord; out lpDDColorKey: DDCOLORKEY): HResult; stdcall;
    function GetDC(out lphDC: HDC): HResult; stdcall;
    function GetFlipStatus(dwFlags: LongWord): HResult; stdcall;
    function GetOverlayPosition(out lplX, lplY: LongInt): HResult; stdcall;
    function GetPalette(out lplpDDPalette: IDirectDrawPalette): HResult; stdcall;
    function GetPixelFormat(out lpDDPixelFormat: DDPIXELFORMAT): HResult; stdcall;
    function GetSurfaceDesc(out lpDDSurfaceDesc: DDSURFACEDESC2): HResult; stdcall;
    function Initialize(lpDD: IDirectDraw; out lpDDSurfaceDesc: DDSURFACEDESC2): HResult; stdcall;
    function IsLost: HResult; stdcall;
    function Lock(lpDestRect: PRect; out lpDDSurfaceDesc: DDSURFACEDESC2; dwFlags: LongWord;
      hEvent: THandle): HResult; stdcall;
    function ReleaseDC(hDC: HDC): HResult; stdcall;
    function Restore: HResult; stdcall;
    function SetClipper(lpDDClipper: IDirectDrawClipper): HResult; stdcall;
    function SetColorKey(dwFlags: LongWord; lpDDColorKey: PDDCOLORKEY): HResult; stdcall;
    function SetOverlayPosition(lX, lY: LongInt): HResult; stdcall;
    function SetPalette(lpDDPalette: IDirectDrawPalette): HResult; stdcall;
    function Unlock(lpRect: PRect): HResult; stdcall;
    function UpdateOverlay(lpSrcRect: PRect; lpDDDestSurface: IDirectDrawSurface4; lpDestRect: PRect;
      dwFlags: LongWord; lpDDOverlayFx: PDDOVERLAYFX): HResult; stdcall;
    function UpdateOverlayDisplay(dwFlags: LongWord): HResult; stdcall;
    function UpdateOverlayZOrder(dwFlags: LongWord; lpDDSReference: IDirectDrawSurface4): HResult; stdcall;
    { Added in the v2 interface }
    function GetDDInterface(out lplpDD: IUnknown): HResult; stdcall;
    function PageLock(dwFlags: LongWord): HResult; stdcall;
    function PageUnlock(dwFlags: LongWord): HResult; stdcall;
    { Added in the v3 interface }
    function SetSurfaceDesc(const lpddsd2: DDSURFACEDESC2; dwFlags: LongWord): HResult; stdcall;
    { Added in the v4 interface }
    function SetPrivateData(const guidTag: TGuid; lpData: Pointer; cbSize, dwFlags: LongWord): HResult; stdcall;
    function GetPrivateData(const guidTag: TGuid; lpBuffer: Pointer; var lpcbBufferSize: LongWord): HResult; stdcall;
    function FreePrivateData(const guidTag: TGuid): HResult; stdcall;
    function GetUniquenessValue(out lpValue: LongWord): HResult; stdcall;
    function ChangeUniquenessValue: HResult; stdcall;
  end;

  PDirectDrawSurface7 = ^IDirectDrawSurface7;
  IDirectDrawSurface7 = interface(IUnknown)
    [SID_IDirectDrawSurface7]
    function AddAttachedSurface(lpDDSAttachedSurface: IDirectDrawSurface7): HResult; stdcall;
    function AddOverlayDirtyRect(const lpRect: TRect): HResult; stdcall;
    function Blt(lpDestRect: PRect; lpDDSrcSurface: IDirectDrawSurface7; lpSrcRect: PRect; dwFlags: LongWord;
      lpDDBltFx: PDDBLTFX): HResult; stdcall;
    function BltBatch(const lpDDBltBatch: DDBLTBATCH; dwCount, dwFlags: LongWord): HResult; stdcall;
    function BltFast(dwX, dwY: LongWord; lpDDSrcSurface: IDirectDrawSurface7; lpSrcRect: PRect;
      dwTrans: LongWord): HResult; stdcall;
    function DeleteAttachedSurface(dwFlags: LongWord; lpDDSAttachedSurface: IDirectDrawSurface7): HResult; stdcall;
    function EnumAttachedSurfaces(lpContext: Pointer;
      lpEnumSurfacesCallback: TDDEnumSurfacesCallback7): HResult; stdcall;
    function EnumOverlayZOrders(dwFlags: LongWord; lpContext: Pointer;
      lpfnCallback: TDDEnumSurfacesCallback7): HResult; stdcall;
    function Flip(lpDDSurfaceTargetOverride: IDirectDrawSurface7; dwFlags: LongWord): HResult; stdcall;
    function GetAttachedSurface(const lpDDSCaps: DDSCAPS2;
      out lplpDDAttachedSurface: IDirectDrawSurface7): HResult; stdcall;
    function GetBltStatus(dwFlags: LongWord): HResult; stdcall;
    function GetCaps(out lpDDSCaps: DDSCAPS2): HResult; stdcall;
    function GetClipper(out lplpDDClipper: IDirectDrawClipper): HResult; stdcall;
    function GetColorKey(dwFlags: LongWord; out lpDDColorKey: DDCOLORKEY): HResult; stdcall;
    function GetDC(out lphDC: HDC): HResult; stdcall;
    function GetFlipStatus(dwFlags: LongWord): HResult; stdcall;
    function GetOverlayPosition(out lplX, lplY: LongInt): HResult; stdcall;
    function GetPalette(out lplpDDPalette: IDirectDrawPalette): HResult; stdcall;
    function GetPixelFormat(out lpDDPixelFormat: DDPIXELFORMAT): HResult; stdcall;
    function GetSurfaceDesc(out lpDDSurfaceDesc: DDSURFACEDESC2): HResult; stdcall;
    function Initialize(lpDD: IDirectDraw; out lpDDSurfaceDesc: DDSURFACEDESC2): HResult; stdcall;
    function IsLost: HResult; stdcall;
    function Lock(lpDestRect: PRect; out lpDDSurfaceDesc: DDSURFACEDESC2; dwFlags: LongWord;
      hEvent: THandle): HResult; stdcall;
    function ReleaseDC(hDC: HDC): HResult; stdcall;
    function Restore: HResult; stdcall;
    function SetClipper(lpDDClipper: IDirectDrawClipper): HResult; stdcall;
    function SetColorKey(dwFlags: LongWord; lpDDColorKey: PDDCOLORKEY): HResult; stdcall;
    function SetOverlayPosition(lX, lY: LongInt): HResult; stdcall;
    function SetPalette(lpDDPalette: IDirectDrawPalette): HResult; stdcall;
    function Unlock(lpRect: PRect): HResult; stdcall;
    function UpdateOverlay(lpSrcRect: PRect; lpDDDestSurface: IDirectDrawSurface7; lpDestRect: PRect;
      dwFlags: LongWord; lpDDOverlayFx: PDDOVERLAYFX): HResult; stdcall;
    function UpdateOverlayDisplay(dwFlags: LongWord): HResult; stdcall;
    function UpdateOverlayZOrder(dwFlags: LongWord; lpDDSReference: IDirectDrawSurface7): HResult; stdcall;
    { Added in the v2 interface }
    function GetDDInterface(out lplpDD: IUnknown): HResult; stdcall;
    function PageLock(dwFlags: LongWord): HResult; stdcall;
    function PageUnlock(dwFlags: LongWord): HResult; stdcall;
    { Added in the v3 interface }
    function SetSurfaceDesc(const lpddsd2: DDSURFACEDESC2; dwFlags: LongWord): HResult; stdcall;
    { Added in the v4 interface }
    function SetPrivateData(const guidTag: TGuid; lpData: Pointer; cbSize, dwFlags: LongWord): HResult; stdcall;
    function GetPrivateData(const guidTag: TGuid; lpBuffer: Pointer; var lpcbBufferSize: LongWord): HResult; stdcall;
    function FreePrivateData(const guidTag: TGuid): HResult; stdcall;
    function GetUniquenessValue(out lpValue: LongWord): HResult; stdcall;
    function ChangeUniquenessValue: HResult; stdcall;
    { Moved Texture7 methods here }
    function SetPriority(dwPriority: LongWord): HResult; stdcall;
    function GetPriority(out lpdwPriority: LongWord): HResult; stdcall;
    function SetLOD(dwMaxLOD: LongWord): HResult; stdcall;
    function GetLOD(out lpdwMaxLOD: LongWord): HResult; stdcall;
  end;

  PDirectDrawColorControl = ^IDirectDrawColorControl;
  IDirectDrawColorControl = interface(IUnknown)
    [SID_IDirectDrawColorControl]
    function GetColorControls(out lpColorControl: DDCOLORCONTROL): HResult; stdcall;
    function SetColorControls(const lpColorControl: PDDCOLORCONTROL): HResult; stdcall;
  end;

  PDirectDrawGammaControl = ^IDirectDrawGammaControl;
  IDirectDrawGammaControl = interface(IUnknown)
    [SID_IDirectDrawGammaControl]
    function GetGammaRamp(dwFlags: LongWord; out lpRampData: DDGAMMARAMP): HResult; stdcall;
    function SetGammaRamp(dwFlags: LongWord; const lpRampData: DDGAMMARAMP): HResult; stdcall;
  end;

  TDirectDrawEnumerateA = function(lpCallback: TDDEnumCallbackA; lpContext: Pointer): HResult; stdcall;
  TDirectDrawEnumerateW = function(lpCallback: TDDEnumCallbackW; lpContext: Pointer): HResult; stdcall;
  TDirectDrawEnumerateExA = function(lpCallback: TDDEnumCallbackExA; lpContext: Pointer;
    dwFlags: LongWord): HResult; stdcall;
  TDirectDrawEnumerateExW = function(lpCallback: TDDEnumCallbackExW; lpContext: Pointer;
    dwFlags: LongWord): HResult; stdcall;
  TDirectDrawCreate = function(lpGUID: PGuid; out lplpDD: IDirectDraw; pUnkOuter: IUnknown): HResult; stdcall;
  TDirectDrawCreateEx = function (lpGUID: PGuid; out lplpDD: IDirectDraw7; const iid: TGuid;
    pUnkOuter: IUnknown): HResult; stdcall;
  TDirectDrawCreateClipper = function(dwFlags: LongWord; out lplpDDClipper: IDirectDrawClipper;
    pUnkOuter: IUnknown): HResult; stdcall;

var
  DirectDrawEnumerateA: TDirectDrawEnumerateA = nil;
  DirectDrawEnumerateW: TDirectDrawEnumerateW = nil;
  DirectDrawEnumerateExA: TDirectDrawEnumerateExA = nil;
  DirectDrawEnumerateExW: TDirectDrawEnumerateExW = nil;

{$IFDEF UNICODE}
  DirectDrawEnumerate: TDirectDrawEnumerateW = nil;
  DirectDrawEnumerateEx: TDirectDrawEnumerateExW = nil;
{$ELSE}
  DirectDrawEnumerate: TDirectDrawEnumerateA = nil;
  DirectDrawEnumerateEx: TDirectDrawEnumerateExA = nil;
{$ENDIF}

  DirectDrawCreate: TDirectDrawCreate = nil;
  DirectDrawCreateEx: TDirectDrawCreateEx = nil;
  DirectDrawCreateClipper: TDirectDrawCreateClipper = nil;

function LinkDDraw: Boolean;
procedure UnlinkDDraw;

implementation

const
  LibraryDDraw = 'ddraw.dll';

var
  LibraryHandle: HModule = 0;

procedure ResetReferences;
begin
  DirectDrawEnumerateA := nil;
  DirectDrawEnumerateW := nil;
  DirectDrawEnumerateExA := nil;
  DirectDrawEnumerateExW := nil;
  DirectDrawEnumerate := nil;
  DirectDrawEnumerateEx := nil;
  DirectDrawCreate := nil;
  DirectDrawCreateEx := nil;
  DirectDrawCreateClipper := nil;
end;

function LinkDDraw: Boolean;
begin
  if LibraryHandle = 0 then
  begin
    LibraryHandle := LoadLibrary(LibraryDDraw);
    if LibraryHandle = 0 then
      Exit(False);

    DirectDrawEnumerateA := GetProcAddress(LibraryHandle, 'DirectDrawEnumerateA');
    DirectDrawEnumerateW := GetProcAddress(LibraryHandle, 'DirectDrawEnumerateW');
    DirectDrawEnumerateExA := GetProcAddress(LibraryHandle, 'DirectDrawEnumerateExA');
    DirectDrawEnumerateExW := GetProcAddress(LibraryHandle, 'DirectDrawEnumerateExW');
    DirectDrawCreate := GetProcAddress(LibraryHandle, 'DirectDrawCreate');
    DirectDrawCreateEx := GetProcAddress(LibraryHandle, 'DirectDrawCreateEx');
    DirectDrawCreateClipper := GetProcAddress(LibraryHandle, 'DirectDrawCreateClipper');

  {$IFDEF UNICODE}
    DirectDrawEnumerate := DirectDrawEnumerateW;
    DirectDrawEnumerateEx := DirectDrawEnumerateExW;
  {$ELSE}
    DirectDrawEnumerate := DirectDrawEnumerateA;
    DirectDrawEnumerateEx := DirectDrawEnumerateExA;
  {$ENDIF}

    Result :=
      (Assigned(DirectDrawEnumerateA) or Assigned(DirectDrawEnumerateW)) and
      (Assigned(DirectDrawEnumerateExA) or Assigned(DirectDrawEnumerateExW)) and
      (Assigned(DirectDrawCreate) or Assigned(DirectDrawCreateEx)) and Assigned(DirectDrawCreateClipper);

    if not Result then
      UnlinkDDraw;
  end
  else
    Result := True;
end;

procedure UnlinkDDraw;
begin
  if LibraryHandle <> 0 then
  begin
    ResetReferences;
    FreeLibrary(LibraryHandle);
    LibraryHandle := 0;
  end;
end;

end.
