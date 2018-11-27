unit PXL.Windows.DXGI;
{
  DirectX Headers translation by Yuriy Kotsarenko, August 2015. Revision 1.0.

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General
  Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
  details.

  Translated DirectX C/C++ files:
    dxgiformat.h
    dxgitype.h
    dxgi.h
    dxgi1_2.h
    dxgi1_3.h

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
  Windows, MultiMon;

const
  DXGI_STATUS_OCCLUDED = HResult($087A0001);
  DXGI_STATUS_CLIPPED = HResult($087A0002);
  DXGI_STATUS_NO_REDIRECTION = HResult($087A0004);
  DXGI_STATUS_NO_DESKTOP_ACCESS = HResult($087A0005);
  DXGI_STATUS_GRAPHICS_VIDPN_SOURCE_IN_USE = HResult($087A0006);
  DXGI_STATUS_MODE_CHANGED = HResult($087A0007);
  DXGI_STATUS_MODE_CHANGE_IN_PROGRESS = HResult($087A0008);
  DXGI_ERROR_INVALID_CALL = HResult($887A0001);
  DXGI_ERROR_NOT_FOUND = HResult($887A0002);
  DXGI_ERROR_MORE_DATA = HResult($887A0003);
  DXGI_ERROR_UNSUPPORTED = HResult($887A0004);
  DXGI_ERROR_DEVICE_REMOVED = HResult($887A0005);
  DXGI_ERROR_DEVICE_HUNG = HResult($887A0006);
  DXGI_ERROR_DEVICE_RESET = HResult($887A0007);
  DXGI_ERROR_WAS_STILL_DRAWING = HResult($887A000A);
  DXGI_ERROR_FRAME_STATISTICS_DISJOINT = HResult($887A000B);
  DXGI_ERROR_GRAPHICS_VIDPN_SOURCE_IN_USE = HResult($887A000C);
  DXGI_ERROR_DRIVER_INTERNAL_ERROR = HResult($887A0020);
  DXGI_ERROR_NONEXCLUSIVE = HResult($887A0021);
  DXGI_ERROR_NOT_CURRENTLY_AVAILABLE = HResult($887A0022);
  DXGI_ERROR_REMOTE_CLIENT_DISCONNECTED = HResult($887A0023);
  DXGI_ERROR_REMOTE_OUTOFMEMORY = HResult($887A0024);
  DXGI_ERROR_ACCESS_LOST = HResult($887A0026);
  DXGI_ERROR_WAIT_TIMEOUT = HResult($887A0027);
  DXGI_ERROR_SESSION_DISCONNECTED = HResult($887A0028);
  DXGI_ERROR_RESTRICT_TO_OUTPUT_STALE = HResult($887A0029);
  DXGI_ERROR_CANNOT_PROTECT_CONTENT = HResult($887A002A);
  DXGI_ERROR_ACCESS_DENIED = HResult($887A002B);
  DXGI_ERROR_NAME_ALREADY_EXISTS = HResult($887A002C);
  DXGI_ERROR_SDK_COMPONENT_MISSING = HResult($887A002D);
  DXGI_STATUS_UNOCCLUDED = HResult($087A0009);
  DXGI_STATUS_DDA_WAS_STILL_DRAWING = HResult($087A000A);
  DXGI_ERROR_MODE_CHANGE_IN_PROGRESS = HResult($887A0025);
  DXGI_DDI_ERR_WASSTILLDRAWING = HResult($887B0001);
  DXGI_DDI_ERR_UNSUPPORTED = HResult($887B0002);
  DXGI_DDI_ERR_NONEXCLUSIVE = HResult($887B0003);

  DXGI_CPU_ACCESS_NONE = 0;
  DXGI_CPU_ACCESS_DYNAMIC = 1;
  DXGI_CPU_ACCESS_READ_WRITE = 2;
  DXGI_CPU_ACCESS_SCRATCH = 3;
  DXGI_CPU_ACCESS_FIELD = 15;

  DXGI_USAGE_SHADER_INPUT = 1 shl (0 + 4);
  DXGI_USAGE_RENDER_TARGET_OUTPUT = 1 shl (1 + 4);
  DXGI_USAGE_BACK_BUFFER = 1 shl (2 + 4);
  DXGI_USAGE_SHARED = 1 shl (3 + 4);
  DXGI_USAGE_READ_ONLY = 1 shl (4 + 4);
  DXGI_USAGE_DISCARD_ON_PRESENT = 1 shl (5 + 4);
  DXGI_USAGE_UNORDERED_ACCESS = 1 shl (6 + 4);

  DXGI_RESOURCE_PRIORITY_MINIMUM = $28000000;
  DXGI_RESOURCE_PRIORITY_LOW = $50000000;
  DXGI_RESOURCE_PRIORITY_NORMAL = $78000000;
  DXGI_RESOURCE_PRIORITY_HIGH = $A0000000;
  DXGI_RESOURCE_PRIORITY_MAXIMUM = $C8000000;

  DXGI_MAP_READ = 1;
  DXGI_MAP_WRITE = 2;
  DXGI_MAP_DISCARD = 4;

  DXGI_ENUM_MODES_INTERLACED = 1;
  DXGI_ENUM_MODES_SCALING = 2;

  DXGI_MAX_SWAP_CHAIN_BUFFERS = 16;

  DXGI_PRESENT_TEST = $00000001;
  DXGI_PRESENT_DO_NOT_SEQUENCE = $00000002;
  DXGI_PRESENT_RESTART = $00000004;
  DXGI_PRESENT_DO_NOT_WAIT = $00000008;
  DXGI_PRESENT_STEREO_PREFER_RIGHT = $00000010;
  DXGI_PRESENT_STEREO_TEMPORARY_MONO = $00000020;
  DXGI_PRESENT_RESTRICT_TO_OUTPUT = $00000040;

  DXGI_MWA_NO_WINDOW_CHANGES = 1 shl 0;
  DXGI_MWA_NO_ALT_ENTER = 1 shl 1;
  DXGI_MWA_NO_PRINT_SCREEN = 1 shl 2;
  DXGI_MWA_VALID = $7;

  DXGI_ENUM_MODES_STEREO = 4;
  DXGI_ENUM_MODES_DISABLED_STEREO = 8;

  DXGI_SHARED_RESOURCE_READ = $80000000;
  DXGI_SHARED_RESOURCE_WRITE = 1;

  DXGI_CREATE_FACTORY_DEBUG = $1;

  DXGI_FORMAT_UNKNOWN = 0;
  DXGI_FORMAT_R32G32B32A32_TYPELESS = 1;
  DXGI_FORMAT_R32G32B32A32_FLOAT = 2;
  DXGI_FORMAT_R32G32B32A32_UINT = 3;
  DXGI_FORMAT_R32G32B32A32_SINT = 4;
  DXGI_FORMAT_R32G32B32_TYPELESS = 5;
  DXGI_FORMAT_R32G32B32_FLOAT = 6;
  DXGI_FORMAT_R32G32B32_UINT = 7;
  DXGI_FORMAT_R32G32B32_SINT = 8;
  DXGI_FORMAT_R16G16B16A16_TYPELESS = 9;
  DXGI_FORMAT_R16G16B16A16_FLOAT = 10;
  DXGI_FORMAT_R16G16B16A16_UNORM = 11;
  DXGI_FORMAT_R16G16B16A16_UINT = 12;
  DXGI_FORMAT_R16G16B16A16_SNORM = 13;
  DXGI_FORMAT_R16G16B16A16_SINT = 14;
  DXGI_FORMAT_R32G32_TYPELESS = 15;
  DXGI_FORMAT_R32G32_FLOAT = 16;
  DXGI_FORMAT_R32G32_UINT = 17;
  DXGI_FORMAT_R32G32_SINT = 18;
  DXGI_FORMAT_R32G8X24_TYPELESS = 19;
  DXGI_FORMAT_D32_FLOAT_S8X24_UINT = 20;
  DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS = 21;
  DXGI_FORMAT_X32_TYPELESS_G8X24_UINT = 22;
  DXGI_FORMAT_R10G10B10A2_TYPELESS = 23;
  DXGI_FORMAT_R10G10B10A2_UNORM = 24;
  DXGI_FORMAT_R10G10B10A2_UINT = 25;
  DXGI_FORMAT_R11G11B10_FLOAT = 26;
  DXGI_FORMAT_R8G8B8A8_TYPELESS = 27;
  DXGI_FORMAT_R8G8B8A8_UNORM = 28;
  DXGI_FORMAT_R8G8B8A8_UNORM_SRGB = 29;
  DXGI_FORMAT_R8G8B8A8_UINT = 30;
  DXGI_FORMAT_R8G8B8A8_SNORM = 31;
  DXGI_FORMAT_R8G8B8A8_SINT = 32;
  DXGI_FORMAT_R16G16_TYPELESS = 33;
  DXGI_FORMAT_R16G16_FLOAT = 34;
  DXGI_FORMAT_R16G16_UNORM = 35;
  DXGI_FORMAT_R16G16_UINT = 36;
  DXGI_FORMAT_R16G16_SNORM = 37;
  DXGI_FORMAT_R16G16_SINT = 38;
  DXGI_FORMAT_R32_TYPELESS = 39;
  DXGI_FORMAT_D32_FLOAT = 40;
  DXGI_FORMAT_R32_FLOAT = 41;
  DXGI_FORMAT_R32_UINT = 42;
  DXGI_FORMAT_R32_SINT = 43;
  DXGI_FORMAT_R24G8_TYPELESS = 44;
  DXGI_FORMAT_D24_UNORM_S8_UINT = 45;
  DXGI_FORMAT_R24_UNORM_X8_TYPELESS = 46;
  DXGI_FORMAT_X24_TYPELESS_G8_UINT = 47;
  DXGI_FORMAT_R8G8_TYPELESS = 48;
  DXGI_FORMAT_R8G8_UNORM = 49;
  DXGI_FORMAT_R8G8_UINT = 50;
  DXGI_FORMAT_R8G8_SNORM = 51;
  DXGI_FORMAT_R8G8_SINT = 52;
  DXGI_FORMAT_R16_TYPELESS = 53;
  DXGI_FORMAT_R16_FLOAT = 54;
  DXGI_FORMAT_D16_UNORM = 55;
  DXGI_FORMAT_R16_UNORM = 56;
  DXGI_FORMAT_R16_UINT = 57;
  DXGI_FORMAT_R16_SNORM = 58;
  DXGI_FORMAT_R16_SINT = 59;
  DXGI_FORMAT_R8_TYPELESS = 60;
  DXGI_FORMAT_R8_UNORM = 61;
  DXGI_FORMAT_R8_UINT = 62;
  DXGI_FORMAT_R8_SNORM = 63;
  DXGI_FORMAT_R8_SINT = 64;
  DXGI_FORMAT_A8_UNORM = 65;
  DXGI_FORMAT_R1_UNORM = 66;
  DXGI_FORMAT_R9G9B9E5_SHAREDEXP = 67;
  DXGI_FORMAT_R8G8_B8G8_UNORM = 68;
  DXGI_FORMAT_G8R8_G8B8_UNORM = 69;
  DXGI_FORMAT_BC1_TYPELESS = 70;
  DXGI_FORMAT_BC1_UNORM = 71;
  DXGI_FORMAT_BC1_UNORM_SRGB = 72;
  DXGI_FORMAT_BC2_TYPELESS = 73;
  DXGI_FORMAT_BC2_UNORM = 74;
  DXGI_FORMAT_BC2_UNORM_SRGB = 75;
  DXGI_FORMAT_BC3_TYPELESS = 76;
  DXGI_FORMAT_BC3_UNORM = 77;
  DXGI_FORMAT_BC3_UNORM_SRGB = 78;
  DXGI_FORMAT_BC4_TYPELESS = 79;
  DXGI_FORMAT_BC4_UNORM = 80;
  DXGI_FORMAT_BC4_SNORM = 81;
  DXGI_FORMAT_BC5_TYPELESS = 82;
  DXGI_FORMAT_BC5_UNORM = 83;
  DXGI_FORMAT_BC5_SNORM = 84;
  DXGI_FORMAT_B5G6R5_UNORM = 85;
  DXGI_FORMAT_B5G5R5A1_UNORM = 86;
  DXGI_FORMAT_B8G8R8A8_UNORM = 87;
  DXGI_FORMAT_B8G8R8X8_UNORM = 88;
  DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM = 89;
  DXGI_FORMAT_B8G8R8A8_TYPELESS = 90;
  DXGI_FORMAT_B8G8R8A8_UNORM_SRGB = 91;
  DXGI_FORMAT_B8G8R8X8_TYPELESS = 92;
  DXGI_FORMAT_B8G8R8X8_UNORM_SRGB = 93;
  DXGI_FORMAT_BC6H_TYPELESS = 94;
  DXGI_FORMAT_BC6H_UF16 = 95;
  DXGI_FORMAT_BC6H_SF16 = 96;
  DXGI_FORMAT_BC7_TYPELESS = 97;
  DXGI_FORMAT_BC7_UNORM = 98;
  DXGI_FORMAT_BC7_UNORM_SRGB = 99;
  DXGI_FORMAT_AYUV = 100;
  DXGI_FORMAT_Y410 = 101;
  DXGI_FORMAT_Y416 = 102;
  DXGI_FORMAT_NV12 = 103;
  DXGI_FORMAT_P010 = 104;
  DXGI_FORMAT_P016 = 105;
  DXGI_FORMAT_420_OPAQUE = 106;
  DXGI_FORMAT_YUY2 = 107;
  DXGI_FORMAT_Y210 = 108;
  DXGI_FORMAT_Y216 = 109;
  DXGI_FORMAT_NV11 = 110;
  DXGI_FORMAT_AI44 = 111;
  DXGI_FORMAT_IA44 = 112;
  DXGI_FORMAT_P8 = 113;
  DXGI_FORMAT_A8P8 = 114;
  DXGI_FORMAT_B4G4R4A4_UNORM = 115;
  DXGI_FORMAT_FORCE_UINT = $FFFFFFFF;

  DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED = 0;
  DXGI_MODE_SCANLINE_ORDER_PROGRESSIVE = 1;
  DXGI_MODE_SCANLINE_ORDER_UPPER_FIELD_FIRST = 2;
  DXGI_MODE_SCANLINE_ORDER_LOWER_FIELD_FIRST = 3;

  DXGI_MODE_SCALING_UNSPECIFIED = 0;
  DXGI_MODE_SCALING_CENTERED = 1;
  DXGI_MODE_SCALING_STRETCHED = 2;

  DXGI_MODE_ROTATION_UNSPECIFIED = 0;
  DXGI_MODE_ROTATION_IDENTITY = 1;
  DXGI_MODE_ROTATION_ROTATE90 = 2;
  DXGI_MODE_ROTATION_ROTATE180 = 3;
  DXGI_MODE_ROTATION_ROTATE270 = 4;

  DXGI_RESIDENCY_FULLY_RESIDENT = 1;
  DXGI_RESIDENCY_RESIDENT_IN_SHARED_MEMORY = 2;
  DXGI_RESIDENCY_EVICTED_TO_DISK = 3;

  DXGI_SWAP_EFFECT_DISCARD = 0;
  DXGI_SWAP_EFFECT_SEQUENTIAL = 1;
  DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL = 3;

  DXGI_SWAP_CHAIN_FLAG_NONPREROTATED = 1;
  DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH = 2;
  DXGI_SWAP_CHAIN_FLAG_GDI_COMPATIBLE = 4;
  DXGI_SWAP_CHAIN_FLAG_RESTRICTED_CONTENT = 8;
  DXGI_SWAP_CHAIN_FLAG_RESTRICT_SHARED_RESOURCE_DRIVER = 16;
  DXGI_SWAP_CHAIN_FLAG_DISPLAY_ONLY = 32;

  DXGI_ADAPTER_FLAG_NONE = 0;
  DXGI_ADAPTER_FLAG_REMOTE = 1;
  DXGI_ADAPTER_FLAG_SOFTWARE = 2;
  DXGI_ADAPTER_FLAG_FORCE_DWORD = $FFFFFFFF;

  DXGI_OUTDUPL_POINTER_SHAPE_TYPE_MONOCHROME = $1;
  DXGI_OUTDUPL_POINTER_SHAPE_TYPE_COLOR = $2;
  DXGI_OUTDUPL_POINTER_SHAPE_TYPE_MASKED_COLOR = $4;

  DXGI_ALPHA_MODE_UNSPECIFIED = 0;
  DXGI_ALPHA_MODE_PREMULTIPLIED = 1;
  DXGI_ALPHA_MODE_STRAIGHT = 2;
  DXGI_ALPHA_MODE_IGNORE = 3;
  DXGI_ALPHA_MODE_FORCE_DWORD = $FFFFFFFF;

  DXGI_OFFER_RESOURCE_PRIORITY_LOW = 1;
  DXGI_OFFER_RESOURCE_PRIORITY_NORMAL = DXGI_OFFER_RESOURCE_PRIORITY_LOW + 1;
  DXGI_OFFER_RESOURCE_PRIORITY_HIGH = DXGI_OFFER_RESOURCE_PRIORITY_NORMAL + 1;

  DXGI_SCALING_STRETCH = 0;
  DXGI_SCALING_NONE = 1;

  DXGI_GRAPHICS_PREEMPTION_DMA_BUFFER_BOUNDARY = 0;
  DXGI_GRAPHICS_PREEMPTION_PRIMITIVE_BOUNDARY = 1;
  DXGI_GRAPHICS_PREEMPTION_TRIANGLE_BOUNDARY = 2;
  DXGI_GRAPHICS_PREEMPTION_PIXEL_BOUNDARY = 3;
  DXGI_GRAPHICS_PREEMPTION_INSTRUCTION_BOUNDARY = 4;

  DXGI_COMPUTE_PREEMPTION_DMA_BUFFER_BOUNDARY = 0;
  DXGI_COMPUTE_PREEMPTION_DISPATCH_BOUNDARY = 1;
  DXGI_COMPUTE_PREEMPTION_THREAD_GROUP_BOUNDARY = 2;
  DXGI_COMPUTE_PREEMPTION_THREAD_BOUNDARY = 3;
  DXGI_COMPUTE_PREEMPTION_INSTRUCTION_BOUNDARY = 4;

  DXGI_MULTIPLANE_OVERLAY_YCbCr_FLAG_NOMINAL_RANGE = $1;
  DXGI_MULTIPLANE_OVERLAY_YCbCr_FLAG_BT709 = $2;
  DXGI_MULTIPLANE_OVERLAY_YCbCr_FLAG_xvYCC = $4;

  DXGI_FRAME_PRESENTATION_MODE_COMPOSED = 0;
  DXGI_FRAME_PRESENTATION_MODE_OVERLAY = 1;
  DXGI_FRAME_PRESENTATION_MODE_NONE = 2;

  DXGI_OVERLAY_SUPPORT_FLAG_DIRECT = $1;
  DXGI_OVERLAY_SUPPORT_FLAG_SCALING = $2;

  SID_IDXGIObject = '{AEC22FB8-76F3-4639-9BE0-28EB43A67A2E}';
  SID_IDXGIDeviceSubObject = '{3D3E0379-F9DE-4D58-BB6C-18D62992F1A6}';
  SID_IDXGIResource = '{035F3AB4-482E-4E50-B41F-8A7F8BD8960B}';
  SID_IDXGIKeyedMutex = '{9D8E1289-D7B3-465F-8126-250E349AF85D}';
  SID_IDXGISurface = '{CAFCB56C-6AC3-4889-BF47-9E23BBD260EC}';
  SID_IDXGISurface1 = '{4AE63092-6327-4C1B-80AE-BFE12EA32B86}';
  SID_IDXGIAdapter = '{2411E7E1-12AC-4CCF-BD14-9798E8534DC0}';
  SID_IDXGIOutput = '{AE02EEDB-C735-4690-8D52-5A8DC20213AA}';
  SID_IDXGISwapChain = '{310D36A0-D2E7-4C0A-AA04-6A9D23B8886A}';
  SID_IDXGIFactory = '{7B7166EC-21C7-44AE-B21A-C9AE321AE369}';
  SID_IDXGIDevice = '{54EC77FA-1377-44E6-8C32-88FD5F44C84C}';
  SID_IDXGIFactory1 = '{770AAE78-F26F-4DBA-A829-253C83D1B387}';
  SID_IDXGIAdapter1 = '{29038F61-3839-4626-91FD-086879011A05}';
  SID_IDXGIDevice1 = '{77DB970F-6276-48BA-BA28-070143B4392C}';
  SID_IDXGIDisplayControl = '{EA9DBF1A-C88E-4486-854A-98AA0138F30C}';
  SID_IDXGIOutputDuplication = '{191CFAC3-A341-470D-B26E-A864F428319C}';
  SID_IDXGISurface2 = '{ABA496DD-B617-4CB8-A866-BC44D7EB1FA2}';
  SID_IDXGIResource1 = '{30961379-4609-4A41-998E-54FE567EE0C1}';
  SID_IDXGIDevice2 = '{05008617-FBFD-4051-A790-144884B4F6A9}';
  SID_IDXGISwapChain1 = '{790A45F7-0D42-4876-983A-0A55CFE6F4AA}';
  SID_IDXGIFactory2 = '{50C83A1C-E072-4C48-87B0-3630FA36A6D0}';
  SID_IDXGIAdapter2 = '{0AA1AE0A-FA0E-4B84-8644-E05FF8E5ACB5}';
  SID_IDXGIOutput1 = '{00CDDEA8-939B-4B83-A340-A685226666CC}';
  SID_IDXGIDevice3 = '{6007896C-3244-4AFD-BF18-A6D3BEDA5023}';
  SID_IDXGISwapChain2 = '{A8BE2AC4-199F-4946-B331-79599FB98DE7}';
  SID_IDXGIOutput2 = '{595E39D1-2724-4663-99B1-DA969DE28364}';
  SID_IDXGIFactory3 = '{25483823-CD46-4C7D-86CA-47AA95B837BD}';
  SID_IDXGIDecodeSwapChain = '{2633066B-4514-4C7A-8FD8-12EA98059D18}';
  SID_IDXGIFactoryMedia = '{41E7D1F2-A591-4F7B-A2E5-FA9C843E1C12}';
  SID_IDXGISwapChainMedia = '{DD95B90B-F05F-4F6A-BD65-25BFB264BD84}';
  SID_IDXGIOutput3 = '{8A6BB301-7E7E-41F4-A8E0-5B32F7F99B18}';

  IID_IDXGIObject: TGuid = SID_IDXGIObject;
  IID_IDXGIDeviceSubObject: TGuid = SID_IDXGIDeviceSubObject;
  IID_IDXGIResource: TGuid = SID_IDXGIResource;
  IID_IDXGIKeyedMutex: TGuid = SID_IDXGIKeyedMutex;
  IID_IDXGISurface: TGuid = SID_IDXGISurface;
  IID_IDXGISurface1: TGuid = SID_IDXGISurface1;
  IID_IDXGIAdapter: TGuid = SID_IDXGIAdapter;
  IID_IDXGIOutput: TGuid = SID_IDXGIOutput;
  IID_IDXGISwapChain: TGuid = SID_IDXGISwapChain;
  IID_IDXGIFactory: TGuid = SID_IDXGIFactory;
  IID_IDXGIDevice: TGuid = SID_IDXGIDevice;
  IID_IDXGIFactory1: TGuid = SID_IDXGIFactory1;
  IID_IDXGIAdapter1: TGuid = SID_IDXGIAdapter1;
  IID_IDXGIDevice1: TGuid = SID_IDXGIDevice1;
  IID_IDXGIDisplayControl: TGuid = SID_IDXGIDisplayControl;
  IID_IDXGIOutputDuplication: TGuid = SID_IDXGIOutputDuplication;
  IID_IDXGISurface2: TGuid = SID_IDXGISurface2;
  IID_IDXGIResource1: TGuid = SID_IDXGIResource1;
  IID_IDXGIDevice2: TGuid = SID_IDXGIDevice2;
  IID_IDXGISwapChain1: TGuid = SID_IDXGISwapChain1;
  IID_IDXGIFactory2: TGuid = SID_IDXGIFactory2;
  IID_IDXGIAdapter2: TGuid = SID_IDXGIAdapter2;
  IID_IDXGIOutput1: TGuid = SID_IDXGIOutput1;
  IID_IDXGIDevice3: TGuid = SID_IDXGIDevice3;
  IID_IDXGISwapChain2: TGuid = SID_IDXGISwapChain2;
  IID_IDXGIOutput2: TGuid = SID_IDXGIOutput2;
  IID_IDXGIFactory3: TGuid = SID_IDXGIFactory3;
  IID_IDXGIDecodeSwapChain: TGuid = SID_IDXGIDecodeSwapChain;
  IID_IDXGIFactoryMedia: TGuid = SID_IDXGIFactoryMedia;
  IID_IDXGISwapChainMedia: TGuid = SID_IDXGISwapChainMedia;
  IID_IDXGIOutput3: TGuid = SID_IDXGIOutput3;

type
  PDXGI_USAGE = ^DXGI_USAGE;
  DXGI_USAGE = LongWord;

  PDXGI_FORMAT = ^DXGI_FORMAT;
  DXGI_FORMAT = LongWord;

  PDXGI_MODE_SCANLINE_ORDER = ^DXGI_MODE_SCANLINE_ORDER;
  DXGI_MODE_SCANLINE_ORDER = LongWord;

  PDXGI_MODE_SCALING = ^DXGI_MODE_SCALING;
  DXGI_MODE_SCALING = LongWord;

  PDXGI_MODE_ROTATION = ^DXGI_MODE_ROTATION;
  DXGI_MODE_ROTATION = LongWord;

  PDXGI_RESIDENCY = ^DXGI_RESIDENCY;
  DXGI_RESIDENCY = LongWord;

  PDXGI_SWAP_EFFECT = ^DXGI_SWAP_EFFECT;
  DXGI_SWAP_EFFECT = LongWord;

  PDXGI_SWAP_CHAIN_FLAG = ^DXGI_SWAP_CHAIN_FLAG;
  DXGI_SWAP_CHAIN_FLAG = LongWord;

  PDXGI_ADAPTER_FLAG = ^DXGI_ADAPTER_FLAG;
  DXGI_ADAPTER_FLAG = LongWord;

  PDXGI_OUTDUPL_POINTER_SHAPE_TYPE = ^DXGI_OUTDUPL_POINTER_SHAPE_TYPE;
  DXGI_OUTDUPL_POINTER_SHAPE_TYPE = LongWord;

  PDXGI_ALPHA_MODE = ^DXGI_ALPHA_MODE;
  DXGI_ALPHA_MODE = LongWord;

  PDXGI_OFFER_RESOURCE_PRIORITY = ^DXGI_OFFER_RESOURCE_PRIORITY;
  DXGI_OFFER_RESOURCE_PRIORITY = LongWord;

  PDXGI_SCALING = ^DXGI_SCALING;
  DXGI_SCALING = LongWord;

  PDXGI_GRAPHICS_PREEMPTION_GRANULARITY = ^DXGI_GRAPHICS_PREEMPTION_GRANULARITY;
  DXGI_GRAPHICS_PREEMPTION_GRANULARITY = LongWord;

  PDXGI_COMPUTE_PREEMPTION_GRANULARITY = ^DXGI_COMPUTE_PREEMPTION_GRANULARITY;
  DXGI_COMPUTE_PREEMPTION_GRANULARITY = LongWord;

  PDXGI_MULTIPLANE_OVERLAY_YCbCr_FLAGS = ^DXGI_MULTIPLANE_OVERLAY_YCbCr_FLAGS;
  DXGI_MULTIPLANE_OVERLAY_YCbCr_FLAGS = LongWord;

  PDXGI_FRAME_PRESENTATION_MODE = ^DXGI_FRAME_PRESENTATION_MODE;
  DXGI_FRAME_PRESENTATION_MODE = LongWord;

  PDXGI_OVERLAY_SUPPORT_FLAG = ^DXGI_OVERLAY_SUPPORT_FLAG;
  DXGI_OVERLAY_SUPPORT_FLAG = LongWord;

  PDXGI_RGB = ^DXGI_RGB;
  DXGI_RGB = record
    Red: Single;
    Green: Single;
    Blue: Single;
  end;

  PD3DCOLORVALUE = ^D3DCOLORVALUE;
  D3DCOLORVALUE = record
    R: Single;
    G: Single;
    B: Single;
    A: Single;
  end;

  PDXGI_RGBA = ^DXGI_RGBA;
  DXGI_RGBA = D3DCOLORVALUE;

  PDXGI_GAMMA_CONTROL = ^DXGI_GAMMA_CONTROL;
  DXGI_GAMMA_CONTROL = record
    Scale: DXGI_RGB;
    Offset: DXGI_RGB;
    GammaCurve: array[0..1024] of DXGI_RGB;
  end;

  PDXGI_GAMMA_CONTROL_CAPABILITIES = ^DXGI_GAMMA_CONTROL_CAPABILITIES;
  DXGI_GAMMA_CONTROL_CAPABILITIES = record
    ScaleAndOffsetSupported: LongBool;
    MaxConvertedValue: Single;
    MinConvertedValue: Single;
    NumGammaControlPoints: LongWord;
    ControlPointPositions: array[0..1024] of Single;
  end;

  PDXGI_RATIONAL = ^DXGI_RATIONAL;
  DXGI_RATIONAL = record
    Numerator: LongWord;
    Denominator: LongWord;
  end;

  PDXGI_MODE_DESC = ^DXGI_MODE_DESC;
  DXGI_MODE_DESC = record
    Width: LongWord;
    Height: LongWord;
    RefreshRate: DXGI_RATIONAL;
    Format: DXGI_FORMAT;
    ScanlineOrdering: DXGI_MODE_SCANLINE_ORDER;
    Scaling: DXGI_MODE_SCALING;
  end;

  PDXGI_SAMPLE_DESC = ^DXGI_SAMPLE_DESC;
  DXGI_SAMPLE_DESC = record
    Count: LongWord;
    Quality: LongWord;
  end;

  PDXGI_FRAME_STATISTICS = ^DXGI_FRAME_STATISTICS;
  DXGI_FRAME_STATISTICS = record
    PresentCount: LongWord;
    PresentRefreshCount: LongWord;
    SyncRefreshCount: LongWord;
    SyncQPCTime: Int64;
    SyncGPUTime: Int64;
  end;

  PDXGI_MAPPED_RECT = ^DXGI_MAPPED_RECT;
  DXGI_MAPPED_RECT = record
    Pitch: LongInt;
    Bits: Pointer;
  end;

  PDXGI_ADAPTER_DESC = ^DXGI_ADAPTER_DESC;
  DXGI_ADAPTER_DESC = record
    Description: array[0..127] of WideChar;
    VendorId: LongWord;
    DeviceId: LongWord;
    SubSysId: LongWord;
    Revision: LongWord;
    DedicatedVideoMemory: SIZE_T;
    DedicatedSystemMemory: SIZE_T;
    SharedSystemMemory: SIZE_T;
    AdapterLuid: TLuid;
  end;

  PDXGI_OUTPUT_DESC = ^DXGI_OUTPUT_DESC;
  DXGI_OUTPUT_DESC = record
    DeviceName: array[0..31] of WideChar;
    DesktopCoordinates: TRect;
    AttachedToDesktop: LongBool;
    Rotation: DXGI_MODE_ROTATION;
    Monitor: HMonitor;
  end;

  PDXGI_SHARED_RESOURCE = ^DXGI_SHARED_RESOURCE;
  DXGI_SHARED_RESOURCE = record
    Handle: THandle;
  end;

  PDXGI_SURFACE_DESC = ^DXGI_SURFACE_DESC;
  DXGI_SURFACE_DESC = record
    Width: LongWord;
    Height: LongWord;
    Format: DXGI_FORMAT;
    SampleDesc: DXGI_SAMPLE_DESC;
  end;

  PDXGI_SWAP_CHAIN_DESC = ^DXGI_SWAP_CHAIN_DESC;
  DXGI_SWAP_CHAIN_DESC = record
    BufferDesc: DXGI_MODE_DESC;
    SampleDesc: DXGI_SAMPLE_DESC;
    BufferUsage: DXGI_USAGE;
    BufferCount: LongWord;
    OutputWindow: HWND;
    Windowed: LongBool;
    SwapEffect: DXGI_SWAP_EFFECT;
    Flags: LongWord;
  end;

  PDXGI_ADAPTER_DESC1 = ^DXGI_ADAPTER_DESC1;
  DXGI_ADAPTER_DESC1 = record
    Description: array[0..127] of WideChar;
    VendorId: LongWord;
    DeviceId: LongWord;
    SubSysId: LongWord;
    Revision: LongWord;
    DedicatedVideoMemory: SIZE_T;
    DedicatedSystemMemory: SIZE_T;
    SharedSystemMemory: SIZE_T;
    AdapterLuid: TLuid;
    Flags: LongWord;
  end;

  PDXGI_DISPLAY_COLOR_SPACE = ^DXGI_DISPLAY_COLOR_SPACE;
  DXGI_DISPLAY_COLOR_SPACE = record
    PrimaryCoordinates: array[0..7, 0..1] of Single;
    WhitePoints: array[0..15, 0..1] of Single;
  end;

  PSECURITY_ATTRIBUTES = ^SECURITY_ATTRIBUTES;
  SECURITY_ATTRIBUTES = record
    NLength: LongWord;
    SecurityDescriptor: Pointer;
    BInheritHandle: LongBool;
  end;

  PDXGI_OUTDUPL_MOVE_RECT = ^DXGI_OUTDUPL_MOVE_RECT;
  DXGI_OUTDUPL_MOVE_RECT = record
    SourcePoint: TPoint;
    DestinationRect: TRect;
  end;

  PDXGI_OUTDUPL_DESC = ^DXGI_OUTDUPL_DESC;
  DXGI_OUTDUPL_DESC = record
    ModeDesc: DXGI_MODE_DESC;
    Rotation: DXGI_MODE_ROTATION;
    DesktopImageInSystemMemory: LongBool;
  end;

  PDXGI_OUTDUPL_POINTER_POSITION = ^DXGI_OUTDUPL_POINTER_POSITION;
  DXGI_OUTDUPL_POINTER_POSITION = record
    Position: TPoint;
    Visible: LongBool;
  end;

  PDXGI_OUTDUPL_POINTER_SHAPE_INFO = ^DXGI_OUTDUPL_POINTER_SHAPE_INFO;
  DXGI_OUTDUPL_POINTER_SHAPE_INFO = record
    _Type: LongWord;
    Width: LongWord;
    Height: LongWord;
    Pitch: LongWord;
    HotSpot: TPoint;
  end;

  PDXGI_OUTDUPL_FRAME_INFO = ^DXGI_OUTDUPL_FRAME_INFO;
  DXGI_OUTDUPL_FRAME_INFO = record
    LastPresentTime: Int64;
    LastMouseUpdateTime: Int64;
    AccumulatedFrames: LongWord;
    RectsCoalesced: LongBool;
    ProtectedContentMaskedOut: LongBool;
    PointerPosition: DXGI_OUTDUPL_POINTER_POSITION;
    TotalMetadataBufferSize: LongWord;
    PointerShapeBufferSize: LongWord;
  end;

  PDXGI_MODE_DESC1 = ^DXGI_MODE_DESC1;
  DXGI_MODE_DESC1 = record
    Width: LongWord;
    Height: LongWord;
    RefreshRate: DXGI_RATIONAL;
    Format: DXGI_FORMAT;
    ScanlineOrdering: DXGI_MODE_SCANLINE_ORDER;
    Scaling: DXGI_MODE_SCALING;
    Stereo: LongBool;
  end;

  PDXGI_SWAP_CHAIN_DESC1 = ^DXGI_SWAP_CHAIN_DESC1;
  DXGI_SWAP_CHAIN_DESC1 = record
    Width: LongWord;
    Height: LongWord;
    Format: DXGI_FORMAT;
    Stereo: LongBool;
    SampleDesc: DXGI_SAMPLE_DESC;
    BufferUsage: DXGI_USAGE;
    BufferCount: LongWord;
    Scaling: DXGI_SCALING;
    SwapEffect: DXGI_SWAP_EFFECT;
    AlphaMode: DXGI_ALPHA_MODE;
    Flags: LongWord;
  end;

  PDXGI_SWAP_CHAIN_FULLSCREEN_DESC = ^DXGI_SWAP_CHAIN_FULLSCREEN_DESC;
  DXGI_SWAP_CHAIN_FULLSCREEN_DESC = record
    RefreshRate: DXGI_RATIONAL;
    ScanlineOrdering: DXGI_MODE_SCANLINE_ORDER;
    Scaling: DXGI_MODE_SCALING;
    Windowed: LongBool;
  end;

  PDXGI_PRESENT_PARAMETERS = ^DXGI_PRESENT_PARAMETERS;
  DXGI_PRESENT_PARAMETERS = record
    DirtyRectsCount: LongWord;
    DirtyRects: PRect;
    ScrollRect: PRect;
    ScrollOffset: PRect;
  end;

  PDXGI_ADAPTER_DESC2 = ^DXGI_ADAPTER_DESC2;
  DXGI_ADAPTER_DESC2 = record
    Description: array[0..127] of WideChar;
    VendorId: LongWord;
    DeviceId: LongWord;
    SubSysId: LongWord;
    Revision: LongWord;
    DedicatedVideoMemory: SIZE_T;
    DedicatedSystemMemory: SIZE_T;
    SharedSystemMemory: SIZE_T;
    AdapterLuid: TLuid;
    Flags: LongWord;
    GraphicsPreemptionGranularity: DXGI_GRAPHICS_PREEMPTION_GRANULARITY;
    ComputePreemptionGranularity: DXGI_COMPUTE_PREEMPTION_GRANULARITY;
  end;

  PDXGI_MATRIX_3X2_F = ^DXGI_MATRIX_3X2_F;
  DXGI_MATRIX_3X2_F = record
    _11: Single;
    _12: Single;
    _21: Single;
    _22: Single;
    _31: Single;
    _32: Single;
  end;

  PDXGI_DECODE_SWAP_CHAIN_DESC = ^DXGI_DECODE_SWAP_CHAIN_DESC;
  DXGI_DECODE_SWAP_CHAIN_DESC = record
    Flags: LongWord;
  end;

  PDXGI_FRAME_STATISTICS_MEDIA = ^DXGI_FRAME_STATISTICS_MEDIA;
  DXGI_FRAME_STATISTICS_MEDIA = record
    PresentCount: LongWord;
    PresentRefreshCount: LongWord;
    SyncRefreshCount: LongWord;
    SyncQPCTime: Int64;
    SyncGPUTime: Int64;
    CompositionMode: DXGI_FRAME_PRESENTATION_MODE;
    ApprovedPresentDuration: LongWord;
  end;

  PIUnknown = ^IUnknown;

  PIDXGIObject = ^IDXGIObject;
  IDXGIObject = interface(IUnknown)
    [SID_IDXGIObject]
    function SetPrivateData(const Name: TGuid; DataSize: LongWord; Data: Pointer): HResult; stdcall;
    function SetPrivateDataInterface(const Name: TGuid; Unknown: IUnknown): HResult; stdcall;
    function GetPrivateData(const Name: TGuid; var DataSize: LongWord; Data: Pointer): HResult; stdcall;
    function GetParent(const Riid: TGuid; out Parent): HResult; stdcall;
  end;

  PIDXGIDeviceSubObject = ^IDXGIDeviceSubObject;
  IDXGIDeviceSubObject = interface(IDXGIObject)
    [SID_IDXGIDeviceSubObject]
    function GetDevice(const Riid: TGuid; out Device): HResult; stdcall;
  end;

  PIDXGIResource = ^IDXGIResource;
  IDXGIResource = interface(IDXGIDeviceSubObject)
    [SID_IDXGIResource]
    function GetSharedHandle(out SharedHandle: THandle): HResult; stdcall;
    function GetUsage(out Usage: DXGI_USAGE): HResult; stdcall;
    function SetEvictionPriority(EvictionPriority: LongWord): HResult; stdcall;
    function GetEvictionPriority(out EvictionPriority: LongWord): HResult; stdcall;
  end;

  PIDXGIKeyedMutex = ^IDXGIKeyedMutex;
  IDXGIKeyedMutex = interface(IDXGIDeviceSubObject)
    [SID_IDXGIKeyedMutex]
    function AcquireSync(Key: UInt64; Milliseconds: LongWord): HResult; stdcall;
    function ReleaseSync(Key: UInt64): HResult; stdcall;
  end;

  PIDXGISurface = ^IDXGISurface;
  IDXGISurface = interface(IDXGIDeviceSubObject)
    [SID_IDXGISurface]
    function GetDesc(out Desc: DXGI_SURFACE_DESC): HResult; stdcall;
    function Map(out LockedRect: DXGI_MAPPED_RECT; MapFlags: LongWord): HResult; stdcall;
    function Unmap: HResult; stdcall;
  end;

  PIDXGISurface1 = ^IDXGISurface1;
  IDXGISurface1 = interface(IDXGISurface)
    [SID_IDXGISurface1]
    function GetDC(Discard: LongBool; out Handle: HDC): HResult; stdcall;
    function ReleaseDC(DirtyRect: PRect): HResult; stdcall;
  end;

  PIDXGIOutput = ^IDXGIOutput;
  IDXGIOutput = interface(IDXGIObject)
    [SID_IDXGIOutput]
    function GetDesc(out Desc: DXGI_OUTPUT_DESC): HResult; stdcall;
    function GetDisplayModeList(EnumFormat: DXGI_FORMAT; Flags: LongWord; var NumModes: LongWord;
      Desc: PDXGI_MODE_DESC): HResult; stdcall;
    function FindClosestMatchingMode(const ModeToMatch: DXGI_MODE_DESC; out ClosestMatch: DXGI_MODE_DESC;
      ConcernedDevice: IUnknown): HResult; stdcall;
    function WaitForVBlank: HResult; stdcall;
    function TakeOwnership(Device: IUnknown; Exclusive: LongBool): HResult; stdcall;
    procedure ReleaseOwnership; stdcall;
    function GetGammaControlCapabilities(out GammaCaps: DXGI_GAMMA_CONTROL_CAPABILITIES): HResult; stdcall;
    function SetGammaControl(const GammaControl: DXGI_GAMMA_CONTROL): HResult; stdcall;
    function GetGammaControl(out GammaControl: DXGI_GAMMA_CONTROL): HResult; stdcall;
    function SetDisplaySurface(ScanoutSurface: IDXGISurface): HResult; stdcall;
    function GetDisplaySurfaceData(Destination: IDXGISurface): HResult; stdcall;
    function GetFrameStatistics(out Stats: DXGI_FRAME_STATISTICS): HResult; stdcall;
  end;

  PIDXGIAdapter = ^IDXGIAdapter;
  IDXGIAdapter = interface(IDXGIObject)
    [SID_IDXGIAdapter]
    function EnumOutputs(OutputIndex: LongWord; out Output: IDXGIOutput): HResult; stdcall;
    function GetDesc(out Desc: DXGI_ADAPTER_DESC): HResult; stdcall;
    function CheckInterfaceSupport(const InterfaceName: TGuid; out UMDVersion: Int64): HResult; stdcall;
  end;

  PIDXGISwapChain = ^IDXGISwapChain;
  IDXGISwapChain = interface(IDXGIDeviceSubObject)
    [SID_IDXGISwapChain]
    function Present(SyncInterval, Flags: LongWord): HResult; stdcall;
    function GetBuffer(Buffer: LongWord; const Riid: TGuid; out Surface): HResult; stdcall;
    function SetFullscreenState(Fullscreen: LongBool; Target: IDXGIOutput): HResult; stdcall;
    function GetFullscreenState(Fullscreen: PLongBool; Target: PIDXGIOutput): HResult; stdcall;
    function GetDesc(out Desc: DXGI_SWAP_CHAIN_DESC): HResult; stdcall;
    function ResizeBuffers(BufferCount, Width, Height: LongWord; NewFormat: DXGI_FORMAT; 
      SwapChainFlags: LongWord): HResult; stdcall;
    function ResizeTarget(const NewTargetParameters: DXGI_MODE_DESC): HResult; stdcall;
    function GetContainingOutput(out Output: IDXGIOutput): HResult; stdcall;
    function GetFrameStatistics(out Stats: DXGI_FRAME_STATISTICS): HResult; stdcall;
    function GetLastPresentCount(out LastPresentCount: LongWord): HResult; stdcall;
  end;

  PIDXGIFactory = ^IDXGIFactory;
  IDXGIFactory = interface(IDXGIObject)
    [SID_IDXGIFactory]
    function EnumAdapters(AdapterIndex: LongWord; out Adapter: IDXGIAdapter): HResult; stdcall;
    function MakeWindowAssociation(WindowHandle: HWND; Flags: LongWord): HResult; stdcall;
    function GetWindowAssociation(out WindowHandle: HWND): HResult; stdcall;
    function CreateSwapChain(Device: IUnknown; const Desc: DXGI_SWAP_CHAIN_DESC; 
      out SwapChain: IDXGISwapChain): HResult; stdcall;
    function CreateSoftwareAdapter(Module: HMODULE; out Adapter: IDXGIAdapter): HResult; stdcall;
  end;

  PIDXGIDevice = ^IDXGIDevice;
  IDXGIDevice = interface(IDXGIObject)
    [SID_IDXGIDevice]
    function GetAdapter(out Adapter: IDXGIAdapter): HResult; stdcall;
    function CreateSurface(const Desc: DXGI_SURFACE_DESC; NumSurfaces: LongWord; Usage: DXGI_USAGE; 
      SharedResource: PDXGI_SHARED_RESOURCE; out Surface: IDXGISurface): HResult; stdcall;
    function QueryResourceResidency(Resources: PIUnknown; ResidencyStatus: PDXGI_RESIDENCY; 
      NumResources: LongWord): HResult; stdcall;
    function SetGPUThreadPriority(Priority: LongInt): HResult; stdcall;
    function GetGPUThreadPriority(out Priority: LongInt): HResult; stdcall;
  end;

  PIDXGIAdapter1 = ^IDXGIAdapter1;
  IDXGIAdapter1 = interface(IDXGIAdapter)
    [SID_IDXGIAdapter1]
    function GetDesc1(out Desc: DXGI_ADAPTER_DESC1): HResult; stdcall;
  end;

  PIDXGIFactory1 = ^IDXGIFactory1;
  IDXGIFactory1 = interface(IDXGIFactory)
    [SID_IDXGIFactory1]
    function EnumAdapters1(Adapter: LongWord; out _Adapter: IDXGIAdapter1): HResult; stdcall;
    function IsCurrent: LongBool; stdcall;
  end;

  PIDXGIDevice1 = ^IDXGIDevice1;
  IDXGIDevice1 = interface(IDXGIDevice)
    [SID_IDXGIDevice1]
    function SetMaximumFrameLatency(MaxLatency: LongWord): HResult; stdcall;
    function GetMaximumFrameLatency(out MaxLatency: LongWord): HResult; stdcall;
  end;

  PIDXGIDisplayControl = ^IDXGIDisplayControl;
  IDXGIDisplayControl = interface(IUnknown)
    [SID_IDXGIDisplayControl]
    function IsStereoEnabled: LongBool; stdcall;
    procedure SetStereoEnabled(Enabled: LongBool); stdcall;
  end;

  PIDXGIOutputDuplication = ^IDXGIOutputDuplication;
  IDXGIOutputDuplication = interface(IDXGIObject)
    [SID_IDXGIOutputDuplication]
    procedure GetDesc(out Desc: DXGI_OUTDUPL_DESC); stdcall;
    function AcquireNextFrame(TimeoutInMilliseconds: LongWord; out FrameInfo: DXGI_OUTDUPL_FRAME_INFO; 
      out DesktopResource: IDXGIResource): HResult; stdcall;
    function GetFrameDirtyRects(DirtyRectsBufferSize: LongWord; DirtyRectsBuffer: PRect; 
      out DirtyRectsBufferSizeRequired: LongWord): HResult; stdcall;
    function GetFrameMoveRects(MoveRectsBufferSize: LongWord; MoveRectBuffer: PDXGI_OUTDUPL_MOVE_RECT; 
      out MoveRectsBufferSizeRequired: LongWord): HResult; stdcall;
    function GetFramePointerShape(PointerShapeBufferSize: LongWord; PointerShapeBuffer: Pointer; 
      out PointerShapeBufferSizeRequired: LongWord; 
      out PointerShapeInfo: DXGI_OUTDUPL_POINTER_SHAPE_INFO): HResult; stdcall;
    function MapDesktopSurface(out LockedRect: DXGI_MAPPED_RECT): HResult; stdcall;
    function UnMapDesktopSurface: HResult; stdcall;
    function ReleaseFrame: HResult; stdcall;
  end;

  PIDXGISurface2 = ^IDXGISurface2;
  IDXGISurface2 = interface(IDXGISurface1)
    [SID_IDXGISurface2]
    function GetResource(const Riid: TGuid; out ParentResource; out SubresourceIndex: LongWord): HResult; stdcall;
  end;

  PIDXGIResource1 = ^IDXGIResource1;
  IDXGIResource1 = interface(IDXGIResource)
    [SID_IDXGIResource1]
    function CreateSubresourceSurface(Index: LongWord; out Surface: IDXGISurface2): HResult; stdcall;
    function CreateSharedHandle(Attributes: PSECURITY_ATTRIBUTES; Access: LongWord; LpName: PWideChar;
      out Handle: THandle): HResult; stdcall;
  end;

  PIDXGIDevice2 = ^IDXGIDevice2;
  IDXGIDevice2 = interface(IDXGIDevice1)
    [SID_IDXGIDevice2]
    function OfferResources(NumResources: LongWord; Resources: PIDXGIResource; 
      Priority: DXGI_OFFER_RESOURCE_PRIORITY): HResult; stdcall;
    function ReclaimResources(NumResources: LongWord; Resources: PIDXGIResource; 
      Discarded: PLongBool): HResult; stdcall;
    function EnqueueSetEvent(Event: THandle): HResult; stdcall;
  end;

  PIDXGISwapChain1 = ^IDXGISwapChain1;
  IDXGISwapChain1 = interface(IDXGISwapChain)
    [SID_IDXGISwapChain1]
    function GetDesc1(out Desc: DXGI_SWAP_CHAIN_DESC1): HResult; stdcall;
    function GetFullscreenDesc(out Desc: DXGI_SWAP_CHAIN_FULLSCREEN_DESC): HResult; stdcall;
    function GetHwnd(out Handle: HWND): HResult; stdcall;
    function GetCoreWindow(const Refiid: TGuid; out Unk): HResult; stdcall;
    function Present1(SyncInterval, PresentFlags: LongWord; 
      const PresentParameters: DXGI_PRESENT_PARAMETERS): HResult; stdcall;
    function IsTemporaryMonoSupported: LongBool; stdcall;
    function GetRestrictToOutput(out RestrictToOutput: IDXGIOutput): HResult; stdcall;
    function SetBackgroundColor(const Color: DXGI_RGBA): HResult; stdcall;
    function GetBackgroundColor(out Color: DXGI_RGBA): HResult; stdcall;
    function SetRotation(Rotation: DXGI_MODE_ROTATION): HResult; stdcall;
    function GetRotation(out Rotation: DXGI_MODE_ROTATION): HResult; stdcall;
  end;

  PIDXGIFactory2 = ^IDXGIFactory2;
  IDXGIFactory2 = interface(IDXGIFactory1)
    [SID_IDXGIFactory2]
    function IsWindowedStereoEnabled: LongBool; stdcall;
    function CreateSwapChainForHwnd(Device: IUnknown; Handle: HWND; const Desc: DXGI_SWAP_CHAIN_DESC1;
      FullscreenDesc: PDXGI_SWAP_CHAIN_FULLSCREEN_DESC; RestrictToOutput: IDXGIOutput; 
      out SwapChain: IDXGISwapChain1): HResult; stdcall;
    function CreateSwapChainForCoreWindow(Device, Window: IUnknown; const Desc: DXGI_SWAP_CHAIN_DESC1; 
      RestrictToOutput: IDXGIOutput; out SwapChain: IDXGISwapChain1): HResult; stdcall;
    function GetSharedResourceAdapterLuid(Resource: THandle; out Luid: TLuid): HResult; stdcall;
    function RegisterStereoStatusWindow(WindowHandle: HWND; WMsg: LongWord; out Cookie: LongWord): HResult; stdcall;
    function RegisterStereoStatusEvent(Event: THandle; out Cookie: LongWord): HResult; stdcall;
    procedure UnregisterStereoStatus(Cookie: LongWord); stdcall;
    function RegisterOcclusionStatusWindow(WindowHandle: HWND; Msg: LongWord;
      out Cookie: LongWord): HResult; stdcall;
    function RegisterOcclusionStatusEvent(Event: THandle; out Cookie: LongWord): HResult; stdcall;
    procedure UnregisterOcclusionStatus(Cookie: LongWord); stdcall;
    function CreateSwapChainForComposition(Device: IUnknown; const Desc: DXGI_SWAP_CHAIN_DESC1; 
      RestrictToOutput: IDXGIOutput; out SwapChain: IDXGISwapChain1): HResult; stdcall;
  end;

  PIDXGIAdapter2 = ^IDXGIAdapter2;
  IDXGIAdapter2 = interface(IDXGIAdapter1)
    [SID_IDXGIAdapter2]
    function GetDesc2(out Desc: DXGI_ADAPTER_DESC2): HResult; stdcall;
  end;

  PIDXGIOutput1 = ^IDXGIOutput1;
  IDXGIOutput1 = interface(IDXGIOutput)
    [SID_IDXGIOutput1]
    function GetDisplayModeList1(EnumFormat: DXGI_FORMAT; Flags: LongWord; var NumModes: LongWord; 
      Desc: PDXGI_MODE_DESC1): HResult; stdcall;
    function FindClosestMatchingMode1(const ModeToMatch: DXGI_MODE_DESC1; out ClosestMatch: DXGI_MODE_DESC1; 
      ConcernedDevice: IUnknown): HResult; stdcall;
    function GetDisplaySurfaceData1(Destination: IDXGIResource): HResult; stdcall;
    function DuplicateOutput(Device: IUnknown; out OutputDuplication: IDXGIOutputDuplication): HResult; stdcall;
  end;

  PIDXGIDevice3 = ^IDXGIDevice3;
  IDXGIDevice3 = interface(IDXGIDevice2)
    [SID_IDXGIDevice3]
    procedure Trim; stdcall;
  end;

  PIDXGISwapChain2 = ^IDXGISwapChain2;
  IDXGISwapChain2 = interface(IDXGISwapChain1)
    [SID_IDXGISwapChain2]
    function SetSourceSize(Width, Height: LongWord): HResult; stdcall;
    function GetSourceSize(out Width, Height: LongWord): HResult; stdcall;
    function SetMaximumFrameLatency(MaxLatency: LongWord): HResult; stdcall;
    function GetMaximumFrameLatency(out MaxLatency: LongWord): HResult; stdcall;
    function GetFrameLatencyWaitableObject: THandle; stdcall;
    function SetMatrixTransform(const Matrix: DXGI_MATRIX_3X2_F): HResult; stdcall;
    function GetMatrixTransform(out Matrix: DXGI_MATRIX_3X2_F): HResult; stdcall;
  end;

  PIDXGIOutput2 = ^IDXGIOutput2;
  IDXGIOutput2 = interface(IDXGIOutput1)
    [SID_IDXGIOutput2]
    function SupportsOverlays: LongBool; stdcall;
  end;

  PIDXGIFactory3 = ^IDXGIFactory3;
  IDXGIFactory3 = interface(IDXGIFactory2)
    [SID_IDXGIFactory3]
    function GetCreationFlags: LongWord; stdcall;
  end;

  PIDXGIDecodeSwapChain = ^IDXGIDecodeSwapChain;
  IDXGIDecodeSwapChain = interface(IUnknown)
    [SID_IDXGIDecodeSwapChain]
    function PresentBuffer(BufferToPresent, SyncInterval, Flags: LongWord): HResult; stdcall;
    function SetSourceRect(const Rect: TRect): HResult; stdcall;
    function SetTargetRect(const Rect: TRect): HResult; stdcall;
    function SetDestSize(Width, Height: LongWord): HResult; stdcall;
    function GetSourceRect(out Rect: TRect): HResult; stdcall;
    function GetTargetRect(out Rect: TRect): HResult; stdcall;
    function GetDestSize(out Width, Height: LongWord): HResult; stdcall;
    function SetColorSpace(ColorSpace: DXGI_MULTIPLANE_OVERLAY_YCbCr_FLAGS): HResult; stdcall;
    function GetColorSpace: DXGI_MULTIPLANE_OVERLAY_YCbCr_FLAGS; stdcall;
  end;

  PIDXGIFactoryMedia = ^IDXGIFactoryMedia;
  IDXGIFactoryMedia = interface(IUnknown)
    [SID_IDXGIFactoryMedia]
    function CreateSwapChainForCompositionSurfaceHandle(Device: IUnknown; HSurface: THandle; 
      const Desc: DXGI_SWAP_CHAIN_DESC1; RestrictToOutput: IDXGIOutput; 
      out SwapChain: IDXGISwapChain1): HResult; stdcall;
    function CreateDecodeSwapChainForCompositionSurfaceHandle(Device: IUnknown; HSurface: THandle; 
      Desc: PDXGI_DECODE_SWAP_CHAIN_DESC; YuvDecodeBuffers: IDXGIResource; RestrictToOutput: IDXGIOutput; 
      out SwapChain: IDXGIDecodeSwapChain): HResult; stdcall;
  end;

  PIDXGISwapChainMedia = ^IDXGISwapChainMedia;
  IDXGISwapChainMedia = interface(IUnknown)
    [SID_IDXGISwapChainMedia]
    function GetFrameStatisticsMedia(out Stats: DXGI_FRAME_STATISTICS_MEDIA): HResult; stdcall;
    function SetPresentDuration(Duration: LongWord): HResult; stdcall;
    function CheckPresentDurationSupport(DesiredPresentDuration: LongWord; out ClosestSmallerPresentDuration, 
      ClosestLargerPresentDuration: LongWord): HResult; stdcall;
  end;

  PIDXGIOutput3 = ^IDXGIOutput3;
  IDXGIOutput3 = interface(IDXGIOutput2)
    [SID_IDXGIOutput3]
    function CheckOverlaySupport(EnumFormat: DXGI_FORMAT; ConcernedDevice: IUnknown; 
      out Flags: LongWord): HResult; stdcall;
  end;

  TCreateDXGIFactory = function(const Riid: TGuid; out Factory): HResult; stdcall;
  TCreateDXGIFactory2 = function(Flags: LongWord; const Riid: TGuid; out Factory): HResult; stdcall;
  TDXGIGetDebugInterface1 = function(Flags: LongWord; const Riid: TGuid; out Debug): HResult; stdcall;

var
  CreateDXGIFactory: TCreateDXGIFactory = nil;
  CreateDXGIFactory1: TCreateDXGIFactory = nil;
  CreateDXGIFactory2: TCreateDXGIFactory2 = nil;
  DXGIGetDebugInterface1: TDXGIGetDebugInterface1 = nil;

function LinkDXGI: Boolean;
procedure UnlinkDXGI;

implementation

const
  LibraryDXGI = 'dxgi.dll';

var
  LibraryHandle: HModule = 0;

procedure ResetReferences;
begin
  CreateDXGIFactory := nil;
  CreateDXGIFactory1 := nil;
  CreateDXGIFactory2 := nil;
  DXGIGetDebugInterface1 := nil;
end;

function LinkDXGI: Boolean;
begin
  if LibraryHandle = 0 then
  begin
    LibraryHandle := LoadLibrary(LibraryDXGI);
    if LibraryHandle = 0 then
      Exit(False);

    CreateDXGIFactory := GetProcAddress(LibraryHandle, 'CreateDXGIFactory');
    CreateDXGIFactory1 := GetProcAddress(LibraryHandle, 'CreateDXGIFactory1');
    CreateDXGIFactory2 := GetProcAddress(LibraryHandle, 'CreateDXGIFactory2');
    DXGIGetDebugInterface1 := GetProcAddress(LibraryHandle, 'DXGIGetDebugInterface1');

    Result := Assigned(CreateDXGIFactory);
    if not Result then
      UnlinkDXGI;
  end
  else
    Result := True;
end;

procedure UnlinkDXGI;
begin
  if LibraryHandle <> 0 then
  begin
    ResetReferences;
    FreeLibrary(LibraryHandle);
    LibraryHandle := 0;
  end;
end;

end.
