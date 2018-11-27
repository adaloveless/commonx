unit PXL.Windows.D3D7;
{
  DirectX Headers translation by Yuriy Kotsarenko, August 2015. Revision 1.0.

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General
  Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
  details.

  Translated DirectX C/C++ files:
    d3dtype.h
    d3dcaps.h
    d3d.h

  Original source code was taken from:
    %WINDOWS_KITS%\8.0\Include\um\
    %WINDOWS_KITS%\8.0\Include\shared\

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
  Windows, PXL.Windows.DDraw;

const
  D3DENUMRET_CANCEL = DDENUMRET_CANCEL;
  D3DENUMRET_OK = DDENUMRET_OK;

  D3DCLEAR_TARGET = $00000001;
  D3DCLEAR_ZBUFFER = $00000002;
  D3DCLEAR_STENCIL = $00000004;

  D3DBLEND_ZERO = 1;
  D3DBLEND_ONE = 2;
  D3DBLEND_SRCCOLOR = 3;
  D3DBLEND_INVSRCCOLOR = 4;
  D3DBLEND_SRCALPHA = 5;
  D3DBLEND_INVSRCALPHA = 6;
  D3DBLEND_DESTALPHA = 7;
  D3DBLEND_INVDESTALPHA = 8;
  D3DBLEND_DESTCOLOR = 9;
  D3DBLEND_INVDESTCOLOR = 10;
  D3DBLEND_SRCALPHASAT = 11;
  D3DBLEND_BOTHSRCALPHA = 12;
  D3DBLEND_BOTHINVSRCALPHA = 13;

  D3DTBLEND_DECAL = 1;
  D3DTBLEND_MODULATE = 2;
  D3DTBLEND_DECALALPHA = 3;
  D3DTBLEND_MODULATEALPHA = 4;
  D3DTBLEND_DECALMASK = 5;
  D3DTBLEND_MODULATEMASK = 6;
  D3DTBLEND_COPY = 7;
  D3DTBLEND_ADD = 8;

  D3DTADDRESS_WRAP = 1;
  D3DTADDRESS_MIRROR = 2;
  D3DTADDRESS_CLAMP = 3;
  D3DTADDRESS_BORDER = 4;

  D3DCULL_NONE = 1;
  D3DCULL_CW = 2;
  D3DCULL_CCW = 3;

  D3DCMP_NEVER = 1;
  D3DCMP_LESS = 2;
  D3DCMP_EQUAL = 3;
  D3DCMP_LESSEQUAL = 4;
  D3DCMP_GREATER = 5;
  D3DCMP_NOTEQUAL = 6;
  D3DCMP_GREATEREQUAL = 7;
  D3DCMP_ALWAYS = 8;

  D3DSTENCILOP_KEEP = 1;
  D3DSTENCILOP_ZERO = 2;
  D3DSTENCILOP_REPLACE = 3;
  D3DSTENCILOP_INCRSAT = 4;
  D3DSTENCILOP_DECRSAT = 5;
  D3DSTENCILOP_INVERT = 6;
  D3DSTENCILOP_INCR = 7;
  D3DSTENCILOP_DECR = 8;

  D3DZB_FALSE = 0;
  D3DZB_TRUE = 1;
  D3DZB_USEW = 2;

  D3DPT_POINTLIST = 1;
  D3DPT_LINELIST = 2;
  D3DPT_LINESTRIP = 3;
  D3DPT_TRIANGLELIST = 4;
  D3DPT_TRIANGLESTRIP = 5;
  D3DPT_TRIANGLEFAN = 6;

  D3DRENDERSTATE_ANTIALIAS = 2;
  D3DRENDERSTATE_TEXTUREPERSPECTIVE = 4;
  D3DRENDERSTATE_ZENABLE = 7;
  D3DRENDERSTATE_FILLMODE = 8;
  D3DRENDERSTATE_SHADEMODE = 9;
  D3DRENDERSTATE_LINEPATTERN = 10;
  D3DRENDERSTATE_ZWRITEENABLE = 14;
  D3DRENDERSTATE_ALPHATESTENABLE = 15;
  D3DRENDERSTATE_LASTPIXEL = 16;
  D3DRENDERSTATE_SRCBLEND = 19;
  D3DRENDERSTATE_DESTBLEND = 20;
  D3DRENDERSTATE_CULLMODE = 22;
  D3DRENDERSTATE_ZFUNC = 23;
  D3DRENDERSTATE_ALPHAREF = 24;
  D3DRENDERSTATE_ALPHAFUNC = 25;
  D3DRENDERSTATE_DITHERENABLE = 26;
  D3DRENDERSTATE_ALPHABLENDENABLE = 27;
  D3DRENDERSTATE_BLENDENABLE = D3DRENDERSTATE_ALPHABLENDENABLE;
  D3DRENDERSTATE_FOGENABLE = 28;
  D3DRENDERSTATE_SPECULARENABLE = 29;
  D3DRENDERSTATE_ZVISIBLE = 30;
  D3DRENDERSTATE_STIPPLEDALPHA = 33;
  D3DRENDERSTATE_FOGCOLOR = 34;
  D3DRENDERSTATE_FOGTABLEMODE = 35;
  D3DRENDERSTATE_FOGSTART = 36;
  D3DRENDERSTATE_FOGEND = 37;
  D3DRENDERSTATE_FOGDENSITY = 38;
  D3DRENDERSTATE_EDGEANTIALIAS = 40;
  D3DRENDERSTATE_COLORKEYENABLE = 41;
  D3DRENDERSTATE_ZBIAS = 47;
  D3DRENDERSTATE_RANGEFOGENABLE = 48;
  D3DRENDERSTATE_STENCILENABLE = 52;
  D3DRENDERSTATE_STENCILFAIL = 53;
  D3DRENDERSTATE_STENCILZFAIL = 54;
  D3DRENDERSTATE_STENCILPASS = 55;
  D3DRENDERSTATE_STENCILFUNC = 56;
  D3DRENDERSTATE_STENCILREF = 57;
  D3DRENDERSTATE_STENCILMASK = 58;
  D3DRENDERSTATE_STENCILWRITEMASK = 59;
  D3DRENDERSTATE_TEXTUREFACTOR = 60;
  D3DRENDERSTATE_WRAP0 = 128;
  D3DRENDERSTATE_WRAP1 = 129;
  D3DRENDERSTATE_WRAP2 = 130;
  D3DRENDERSTATE_WRAP3 = 131;
  D3DRENDERSTATE_WRAP4 = 132;
  D3DRENDERSTATE_WRAP5 = 133;
  D3DRENDERSTATE_WRAP6 = 134;
  D3DRENDERSTATE_WRAP7 = 135;
  D3DRENDERSTATE_CLIPPING = 136;
  D3DRENDERSTATE_LIGHTING = 137;
  D3DRENDERSTATE_EXTENTS = 138;
  D3DRENDERSTATE_AMBIENT = 139;
  D3DRENDERSTATE_FOGVERTEXMODE = 140;
  D3DRENDERSTATE_COLORVERTEX = 141;
  D3DRENDERSTATE_LOCALVIEWER = 142;
  D3DRENDERSTATE_NORMALIZENORMALS = 143;
  D3DRENDERSTATE_COLORKEYBLENDENABLE = 144;
  D3DRENDERSTATE_DIFFUSEMATERIALSOURCE = 145;
  D3DRENDERSTATE_SPECULARMATERIALSOURCE = 146;
  D3DRENDERSTATE_AMBIENTMATERIALSOURCE = 147;
  D3DRENDERSTATE_EMISSIVEMATERIALSOURCE = 148;
  D3DRENDERSTATE_VERTEXBLEND = 151;
  D3DRENDERSTATE_CLIPPLANEENABLE = 152;

  D3DWRAP_U = $00000001;
  D3DWRAP_V = $00000002;

  D3DTSS_COLOROP = 1;
  D3DTSS_COLORARG1 = 2;
  D3DTSS_COLORARG2 = 3;
  D3DTSS_ALPHAOP = 4;
  D3DTSS_ALPHAARG1 = 5;
  D3DTSS_ALPHAARG2 = 6;
  D3DTSS_BUMPENVMAT00 = 7;
  D3DTSS_BUMPENVMAT01 = 8;
  D3DTSS_BUMPENVMAT10 = 9;
  D3DTSS_BUMPENVMAT11 = 10;
  D3DTSS_TEXCOORDINDEX = 11;
  D3DTSS_ADDRESS = 12;
  D3DTSS_ADDRESSU = 13;
  D3DTSS_ADDRESSV = 14;
  D3DTSS_BORDERCOLOR = 15;
  D3DTSS_MAGFILTER = 16;
  D3DTSS_MINFILTER = 17;
  D3DTSS_MIPFILTER = 18;
  D3DTSS_MIPMAPLODBIAS = 19;
  D3DTSS_MAXMIPLEVEL = 20;
  D3DTSS_MAXANISOTROPY = 21;
  D3DTSS_BUMPENVLSCALE = 22;
  D3DTSS_BUMPENVLOFFSET = 23;
  D3DTSS_TEXTURETRANSFORMFLAGS = 24;

  D3DTOP_DISABLE = 1;
  D3DTOP_SELECTARG1 = 2;
  D3DTOP_SELECTARG2 = 3;
  D3DTOP_MODULATE = 4;
  D3DTOP_MODULATE2X = 5;
  D3DTOP_MODULATE4X = 6;
  D3DTOP_ADD = 7;
  D3DTOP_ADDSIGNED = 8;
  D3DTOP_ADDSIGNED2X = 9;
  D3DTOP_SUBTRACT = 10;
  D3DTOP_ADDSMOOTH = 11;
  D3DTOP_BLENDDIFFUSEALPHA = 12;
  D3DTOP_BLENDTEXTUREALPHA = 13;
  D3DTOP_BLENDFACTORALPHA = 14;
  D3DTOP_BLENDTEXTUREALPHAPM = 15;
  D3DTOP_BLENDCURRENTALPHA = 16;
  D3DTOP_PREMODULATE = 17;
  D3DTOP_MODULATEALPHA_ADDCOLOR = 18;
  D3DTOP_MODULATECOLOR_ADDALPHA = 19;
  D3DTOP_MODULATEINVALPHA_ADDCOLOR = 20;
  D3DTOP_MODULATEINVCOLOR_ADDALPHA = 21;
  D3DTOP_BUMPENVMAP = 22;
  D3DTOP_BUMPENVMAPLUMINANCE = 23;
  D3DTOP_DOTPRODUCT3 = 24;

  D3DTA_SELECTMASK = $0000000;
  D3DTA_DIFFUSE = $00000000;
  D3DTA_CURRENT = $00000001;
  D3DTA_TEXTURE = $00000002;
  D3DTA_TFACTOR = $00000003;
  D3DTA_SPECULAR = $00000004;
  D3DTA_COMPLEMENT = $00000010;
  D3DTA_ALPHAREPLICATE = $00000020;

  D3DTFG_POINT = 1;
  D3DTFG_LINEAR = 2;
  D3DTFG_FLATCUBIC = 3;
  D3DTFG_GAUSSIANCUBIC = 4;
  D3DTFG_ANISOTROPIC = 5;

  D3DTFN_POINT = 1;
  D3DTFN_LINEAR = 2;
  D3DTFN_ANISOTROPIC = 3;

  D3DTFP_NONE = 1;
  D3DTFP_POINT = 2;
  D3DTFP_LINEAR = 3;

  D3DMAXNUMVERTICES = (1 shl 16) + 1;
  D3DMAXNUMPRIMITIVES = (1 shl 16) + 1;

  D3DFVF_RESERVED0 = $001;
  D3DFVF_POSITION_MASK = $00E;
  D3DFVF_XYZ = $002;
  D3DFVF_XYZRHW = $004;
  D3DFVF_XYZB1 = $006;
  D3DFVF_XYZB2 = $008;
  D3DFVF_XYZB3 = $00A;
  D3DFVF_XYZB4 = $00C;
  D3DFVF_XYZB5 = $00E;
  D3DFVF_NORMAL = $010;
  D3DFVF_RESERVED1 = $020;
  D3DFVF_DIFFUSE = $040;
  D3DFVF_SPECULAR = $080;
  D3DFVF_TEXCOUNT_MASK = $F00;
  D3DFVF_TEXCOUNT_SHIFT = 8;
  D3DFVF_TEX0 = $000;
  D3DFVF_TEX1 = $100;
  D3DFVF_TEX2 = $200;
  D3DFVF_TEX3 = $300;
  D3DFVF_TEX4 = $400;
  D3DFVF_TEX5 = $500;
  D3DFVF_TEX6 = $600;
  D3DFVF_TEX7 = $700;
  D3DFVF_TEX8 = $800;
  D3DFVF_RESERVED2 = $F000;
  D3DFVF_VERTEX = D3DFVF_XYZ or D3DFVF_NORMAL or D3DFVF_TEX1;
  D3DFVF_LVERTEX = D3DFVF_XYZ or D3DFVF_RESERVED1 or D3DFVF_DIFFUSE or D3DFVF_SPECULAR or D3DFVF_TEX1;
  D3DFVF_TLVERTEX = D3DFVF_XYZRHW or D3DFVF_DIFFUSE or D3DFVF_SPECULAR or D3DFVF_TEX1;

  D3DDP_MAXTEXCOORD = 8;

  D3DFVF_TEXTUREFORMAT2 = 0;
  D3DFVF_TEXTUREFORMAT1 = 3;
  D3DFVF_TEXTUREFORMAT3 = 1;
  D3DFVF_TEXTUREFORMAT4 = 2;

  D3D_OK = DD_OK;

  SID_IDirect3D7 = '{F5049E77-4861-11D2-A407-00A0C90629A8}';
  SID_IDirect3DRampDevice = '{F2086B20-259F-11CF-A31A-00AA00B93356}';
  SID_IDirect3DRGBDevice = '{A4665C60-2673-11CF-A31A-00AA00B93356}';
  SID_IDirect3DHALDevice = '{84E63DE0-46AA-11CF-816F-0000C020156E}';
  SID_IDirect3DMMXDevice = '{881949A1-D6F3-11D0-89AB-00A0C9054129}';
  SID_IDirect3DRefDevice = '{50936643-13E9-11D1-89AA-00A0C9054129}';
  SID_IDirect3DNullDevice = '{8767DF22-BACC-11D1-8969-00A0C90629A8}';
  SID_IDirect3DTnLHalDevice = '{F5049E78-4861-11D2-A407-00A0C90629A8}';
  SID_IDirect3DDevice7 = '{F5049E79-4861-11D2-A407-00A0C90629A8}';
  SID_IDirect3DVertexBuffer7 = '{F5049E7D-4861-11D2-A407-00A0C90629A8}';

  IID_IDirect3D7: TGuid = SID_IDirect3D7;
  IID_IDirect3DRampDevice: TGuid = SID_IDirect3DRampDevice;
  IID_IDirect3DRGBDevice: TGuid = SID_IDirect3DRGBDevice;
  IID_IDirect3DHALDevice: TGuid = SID_IDirect3DHALDevice;
  IID_IDirect3DMMXDevice: TGuid = SID_IDirect3DMMXDevice;
  IID_IDirect3DRefDevice: TGuid = SID_IDirect3DRefDevice;
  IID_IDirect3DNullDevice: TGuid = SID_IDirect3DNullDevice;
  IID_IDirect3DTnLHalDevice: TGuid = SID_IDirect3DTnLHalDevice;
  IID_IDirect3DDevice7: TGuid = SID_IDirect3DDevice7;
  IID_IDirect3DVertexBuffer7: TGuid = SID_IDirect3DVertexBuffer7;

type
  PD3DVALUE = ^D3DVALUE;
  D3DVALUE = Single;

  PD3DFIXED = ^D3DFIXED;
  D3DFIXED = LongInt;

  PD3DCOLOR = ^D3DCOLOR;
  D3DCOLOR = LongWord;

  D3DLIGHTTYPE = LongWord;
  D3DPRIMITIVETYPE = LongWord;
  D3DTRANSFORMSTATETYPE = LongWord;
  D3DRENDERSTATETYPE = LongWord;
  D3DTEXTURESTAGESTATETYPE = LongWord;
  D3DSTATEBLOCKTYPE = LongWord;

  PD3DCOLORVALUE = ^D3DCOLORVALUE;
  D3DCOLORVALUE = record
  case Integer of
    0: (r: D3DVALUE;
        g: D3DVALUE;
        b: D3DVALUE;
        a: D3DVALUE);
    1: (dvR: D3DVALUE;
        dvG: D3DVALUE;
        dvB: D3DVALUE;
        dvA: D3DVALUE);
  end;

  PD3DRECT = ^D3DRECT;
  D3DRECT = record
  case Integer of
    0: (x1: LongInt;
        y1: LongInt;
        x2: LongInt;
        y2: LongInt);
    1: (lX1: LongInt;
        lY1: LongInt;
        lX2: LongInt;
        lY2: LongInt);
  end;

  PD3DVECTOR = ^D3DVECTOR;
  D3DVECTOR = record
  case Integer of
    0: (x: D3DVALUE;
        y: D3DVALUE;
        z: D3DVALUE);
    1: (dvX: D3DVALUE;
        dvY: D3DVALUE;
        dvZ: D3DVALUE);
  end;

  PD3DMATRIX = ^D3DMATRIX;
  D3DMATRIX = record
    _11, _12, _13, _14: D3DVALUE;
    _21, _22, _23, _24: D3DVALUE;
    _31, _32, _33, _34: D3DVALUE;
    _41, _42, _43, _44: D3DVALUE;
  end;

  PD3DVIEWPORT7 = ^D3DVIEWPORT7;
  D3DVIEWPORT7 = record
    dwX: LongWord;
    dwY: LongWord;
    dwWidth: LongWord;
    dwHeight: LongWord;
    dvMinZ: D3DVALUE;
    dvMaxZ: D3DVALUE;
  end;

  PD3DMATERIAL7 = ^D3DMATERIAL7;
  D3DMATERIAL7 = record
    case Integer of
      0: (diffuse: D3DCOLORVALUE;
          ambient: D3DCOLORVALUE;
          specular: D3DCOLORVALUE;
          emissive: D3DCOLORVALUE;
          power: D3DVALUE);
      1: (dcvDiffuse: D3DCOLORVALUE;
          dcvAmbient: D3DCOLORVALUE;
          dcvSpecular: D3DCOLORVALUE;
          dcvEmissive: D3DCOLORVALUE;
          dvPower: D3DVALUE);
  end;

  PD3DLIGHT7 = ^D3DLIGHT7;
  D3DLIGHT7 = record
    dltType: D3DLIGHTTYPE;
    dcvDiffuse: D3DCOLORVALUE;
    dcvSpecular: D3DCOLORVALUE;
    dcvAmbient: D3DCOLORVALUE;
    dvPosition: D3DVECTOR;
    dvDirection: D3DVECTOR;
    dvRange: D3DVALUE;
    dvFalloff: D3DVALUE;
    dvAttenuation0: D3DVALUE;
    dvAttenuation1: D3DVALUE;
    dvAttenuation2: D3DVALUE;
    dvTheta: D3DVALUE;
    dvPhi: D3DVALUE;
  end;

  PD3DCLIPSTATUS = ^D3DCLIPSTATUS;
  D3DCLIPSTATUS = record
    dwFlags: LongWord;
    dwStatus: LongWord;
    minx, maxx: Single;
    miny, maxy: Single;
    minz, maxz: Single;
  end;

  PD3DVERTEXBUFFERDESC = ^D3DVERTEXBUFFERDESC;
  D3DVERTEXBUFFERDESC = record
    dwSize: LongWord;
    dwCaps: LongWord;
    dwFVF: LongWord;
    dwNumVertices: LongWord;
  end;

  PD3DDP_PTRSTRIDE = ^D3DDP_PTRSTRIDE;
  D3DDP_PTRSTRIDE = record
    lpvData: Pointer;
    dwStride: LongWord;
  end;

  PD3DDRAWPRIMITIVESTRIDEDDATA = ^D3DDRAWPRIMITIVESTRIDEDDATA;
  D3DDRAWPRIMITIVESTRIDEDDATA = record
    position: D3DDP_PTRSTRIDE;
    normal: D3DDP_PTRSTRIDE;
    diffuse: D3DDP_PTRSTRIDE;
    specular: D3DDP_PTRSTRIDE;
    textureCoords: array[0..D3DDP_MAXTEXCOORD - 1] of D3DDP_PTRSTRIDE;
  end;

  PD3DPRIMCAPS = ^D3DPRIMCAPS;
  D3DPRIMCAPS = record
    dwSize: LongWord;
    dwMiscCaps: LongWord;
    dwRasterCaps: LongWord;
    dwZCmpCaps: LongWord;
    dwSrcBlendCaps: LongWord;
    dwDestBlendCaps: LongWord;
    dwAlphaCmpCaps: LongWord;
    dwShadeCaps: LongWord;
    dwTextureCaps: LongWord;
    dwTextureFilterCaps: LongWord;
    dwTextureBlendCaps: LongWord;
    dwTextureAddressCaps: LongWord;
    dwStippleWidth: LongWord;
    dwStippleHeight: LongWord;
  end;

  PD3DDEVICEDESC7 = ^D3DDEVICEDESC7;
  D3DDEVICEDESC7 = record
    dwDevCaps: LongWord;
    dpcLineCaps: D3DPRIMCAPS;
    dpcTriCaps: D3DPRIMCAPS;
    dwDeviceRenderBitDepth: LongWord;
    dwDeviceZBufferBitDepth: LongWord;
    dwMinTextureWidth: LongWord;
    dwMinTextureHeight: LongWord;
    dwMaxTextureWidth: LongWord;
    dwMaxTextureHeight: LongWord;
    dwMaxTextureRepeat: LongWord;
    dwMaxTextureAspectRatio: LongWord;
    dwMaxAnisotropy: LongWord;
    dvGuardBandLeft: D3DVALUE;
    dvGuardBandTop: D3DVALUE;
    dvGuardBandRight: D3DVALUE;
    dvGuardBandBottom: D3DVALUE;
    dvExtentsAdjust: D3DVALUE;
    dwStencilCaps: LongWord;
    dwFVFCaps: LongWord;
    dwTextureOpCaps: LongWord;
    wMaxTextureBlendStages: Word;
    wMaxSimultaneousTextures: Word;
    dwMaxActiveLights: LongWord;
    dvMaxVertexW: D3DVALUE;
    deviceGUID: TGuid;
    wMaxUserClipPlanes: Word;
    wMaxVertexBlendMatrices: Word;
    dwVertexProcessingCaps: LongWord;
    dwReserved1: LongWord;
    dwReserved2: LongWord;
    dwReserved3: LongWord;
    dwReserved4: LongWord;
  end;

  TD3DEnumDevicesCallback7 = function(lpDeviceDescription: PAnsiChar; lpDeviceName: PAnsiChar;
    const lpD3DDeviceDesc: D3DDEVICEDESC7; lpContext: Pointer): HResult; stdcall;

  TD3DEnumPixelFormatsCallback = function(var lpDDPixFmt: DDPIXELFORMAT; lpContext: Pointer): HResult; stdcall;

  IDirect3DDevice7 = interface;
  IDirect3DVertexBuffer7 = interface;

  IDirect3D7 = interface(IUnknown)
    [SID_IDirect3D7]
    function EnumDevices(lpEnumDevicesCallback: TD3DEnumDevicesCallback7; lpUserArg: Pointer): HResult; stdcall;
    function CreateDevice(const rclsid: TGuid; lpDDS: IDirectDrawSurface7;
      out lplpD3DDevice: IDirect3DDevice7): HResult; stdcall;
    function CreateVertexBuffer(const lpVBDesc: PD3DVERTEXBUFFERDESC; out lplpD3DVertexBuffer: IDirect3DVertexBuffer7;
      dwFlags: LongWord): HResult; stdcall;
    function EnumZBufferFormats(const riidDevice: TGuid; lpEnumCallback: TD3DEnumPixelFormatsCallback;
      lpContext: Pointer): HResult; stdcall;
    function EvictManagedTextures: HResult; stdcall;
  end;

  IDirect3DDevice7 = interface(IUnknown)
    [SID_IDirect3DDevice7]
    function GetCaps(out lpD3DDevDesc: D3DDEVICEDESC7): HResult; stdcall;
    function EnumTextureFormats(lpd3dEnumPixelProc: TD3DEnumPixelFormatsCallback; lpArg: Pointer): HResult; stdcall;
    function BeginScene: HResult; stdcall;
    function EndScene: HResult; stdcall;
    function GetDirect3D(out lpd3d: IDirect3D7): HResult; stdcall;
    function SetRenderTarget(lpNewRenderTarget: IDirectDrawSurface7; dwFlags: LongWord): HResult; stdcall;
    function GetRenderTarget(out lplpRenderTarget: IDirectDrawSurface7): HResult; stdcall;
    function Clear(dwCount: LongWord; lpRects: PD3DRECT; dwFlags: LongWord; dwColor: D3DCOLOR; dvZ: D3DVALUE;
      dwStencil: LongWord): HResult; stdcall;
    function SetTransform(dtstTransformStateType: D3DTRANSFORMSTATETYPE;
      const lpD3DMatrix: D3DMATRIX): HResult; stdcall;
    function GetTransform(dtstTransformStateType: D3DTRANSFORMSTATETYPE; out lpD3DMatrix: D3DMATRIX): HResult; stdcall;
    function SetViewport(const lpViewport: D3DVIEWPORT7): HResult; stdcall;
    function MultiplyTransform(dtstTransformStateType: D3DTRANSFORMSTATETYPE;
      const lpD3DMatrix: D3DMATRIX): HResult; stdcall;
    function GetViewport(out lpViewport: D3DVIEWPORT7): HResult; stdcall;
    function SetMaterial(const lpMaterial: D3DMATERIAL7): HResult; stdcall;
    function GetMaterial(out lpMaterial: D3DMATERIAL7): HResult; stdcall;
    function SetLight(dwLightIndex: LongWord; const lpLight: D3DLIGHT7): HResult; stdcall;
    function GetLight(dwLightIndex: LongWord; out lpLight: D3DLIGHT7): HResult; stdcall;
    function SetRenderState(dwRenderStateType: D3DRENDERSTATETYPE; dwRenderState: LongWord): HResult; stdcall;
    function GetRenderState(dwRenderStateType: D3DRENDERSTATETYPE; out dwRenderState: LongWord): HResult; stdcall;
    function BeginStateBlock: HResult; stdcall;
    function EndStateBlock(out lpdwBlockHandle: LongWord): HResult; stdcall;
    function PreLoad(lpddsTexture: IDirectDrawSurface7): HResult; stdcall;
    function DrawPrimitive(dptPrimitiveType: D3DPRIMITIVETYPE; dwVertexTypeDesc: LongWord; lpvVertices: Pointer;
      dwVertexCount, dwFlags: LongWord): HResult; stdcall;
    function DrawIndexedPrimitive(dptPrimitiveType: D3DPRIMITIVETYPE; dwVertexTypeDesc: LongWord; lpvVertices: Pointer;
      dwVertexCount: LongWord; lpwIndices: PWord; dwIndexCount, dwFlags: LongWord): HResult; stdcall;
    function SetClipStatus(const lpD3DClipStatus: D3DCLIPSTATUS): HResult; stdcall;
    function GetClipStatus(out lpD3DClipStatus: D3DCLIPSTATUS): HResult; stdcall;
    function DrawPrimitiveStrided(dptPrimitiveType: D3DPRIMITIVETYPE; dwVertexTypeDesc: LongWord;
      lpVertexArray: PD3DDRAWPRIMITIVESTRIDEDDATA; dwVertexCount, dwFlags: LongWord): HResult; stdcall;
    function DrawIndexedPrimitiveStrided(dptPrimitiveType: D3DPRIMITIVETYPE; dwVertexTypeDesc: LongWord;
      lpVertexArray: PD3DDRAWPRIMITIVESTRIDEDDATA; dwVertexCount: LongWord; lpwIndices: PWord; dwIndexCount,
      dwFlags: LongWord): HResult; stdcall;
    function DrawPrimitiveVB(dptPrimitiveType: D3DPRIMITIVETYPE; lpD3DVertexBuffer: IDirect3DVertexBuffer7;
      dwStartVertex, dwNumVertices, dwFlags: LongWord): HResult; stdcall;
    function DrawIndexedPrimitiveVB(dptPrimitiveType: D3DPRIMITIVETYPE; lpD3DVertexBuffer: IDirect3DVertexBuffer7;
      dwStartVertex, dwNumVertices: LongWord; lpwIndices: PWord; dwIndexCount, dwFlags: LongWord): HResult; stdcall;
    function ComputeSphereVisibility(lpCenters: PD3DVECTOR; lpRadii: PD3DVALUE; dwNumSpheres, dwFlags: LongWord;
      lpdwReturnValues: PLongWord): HResult; stdcall;
    function GetTexture(dwStage: LongWord; out lplpTexture: IDirectDrawSurface7): HResult; stdcall;
    function SetTexture(dwStage: LongWord; lpTexture: IDirectDrawSurface7): HResult; stdcall;
    function GetTextureStageState(dwStage: LongWord; dwState: D3DTEXTURESTAGESTATETYPE;
      out lpdwValue: LongWord): HResult; stdcall;
    function SetTextureStageState(dwStage: LongWord; dwState: D3DTEXTURESTAGESTATETYPE;
      lpdwValue: LongWord): HResult; stdcall;
    function ValidateDevice(out lpdwExtraPasses: LongWord): HResult; stdcall;
    function ApplyStateBlock(dwBlockHandle: LongWord): HResult; stdcall;
    function CaptureStateBlock(dwBlockHandle: LongWord): HResult; stdcall;
    function DeleteStateBlock(dwBlockHandle: LongWord): HResult; stdcall;
    function CreateStateBlock(d3dsbType: D3DSTATEBLOCKTYPE; out lpdwBlockHandle: LongWord): HResult; stdcall;
    function Load(lpDestTex: IDirectDrawSurface7; lpDestPoint: PPoint; lpSrcTex: IDirectDrawSurface7;
      lprcSrcRect: PRect; dwFlags: LongWord): HResult; stdcall;
    function LightEnable(dwLightIndex: LongWord; bEnable: LongBool): HResult; stdcall;
    function GetLightEnable(dwLightIndex: LongWord; out bEnable: LongBool): HResult; stdcall;
    function SetClipPlane(dwIndex: LongWord; pPlaneEquation: PD3DVALUE): HResult; stdcall;
    function GetClipPlane(dwIndex: LongWord; pPlaneEquation: PD3DVALUE): HResult; stdcall;
    function GetInfo(dwDevInfoID: LongWord; pDevInfoStruct: Pointer; dwSize: LongWord): HResult; stdcall;
  end;

  PIDirect3DVertexBuffer7 = ^IDirect3DVertexBuffer7;
  IDirect3DVertexBuffer7 = interface(IUnknown)
    [SID_IDirect3DVertexBuffer7]
    function Lock(dwFlags: LongWord; out lplpData: Pointer; out lpdwSize: LongWord): HResult; stdcall;
    function Unlock: HResult; stdcall;
    function ProcessVertices(dwVertexOp, dwDestIndex, dwCount: LongWord; lpSrcBuffer: IDirect3DVertexBuffer7;
      dwSrcIndex: LongWord; lpD3DDevice: IDirect3DDevice7; dwFlags: LongWord): HResult; stdcall;
    function GetVertexBufferDesc(out lpVBDesc: D3DVERTEXBUFFERDESC): HResult; stdcall;
    function Optimize(lpD3DDevice: IDirect3DDevice7; dwFlags: LongWord): HResult; stdcall;
    function ProcessVerticesStrided(dwVertexOp, dwDestIndex, dwCount: LongWord;
      lpVertexArray: PD3DDRAWPRIMITIVESTRIDEDDATA; dwVertexTypeDesc: LongWord; lpD3DDevice: IDirect3DDevice7;
      dwFlags: LongWord): HResult; stdcall;
  end;

implementation

end.
