unit D3DX11_JSB;

///////////////////////////////////////////////////////////////////////////////
// Title: Translation of DirectX C++ header files for use with Delphi 2009 and later
//
// File name: D3DX11_JSB.pas
//
// Originator: J S Bladen, Sheffield, UK.
//
// Copyright: J S Bladen, Sheffield, UK.
//
// Translation date and time (UTC): 11/10/2010 19:05:48
//
// Email: DirectXForDelphi@jsbmedical.co.uk
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Original file(s):
//   D3DX11Core.h
//   D3DX11Tex.h
//   D3DX11Async.h
//   D3DX11.h
//   D3DX11Effect.h
//
// Copyright (C) Microsoft Corporation.
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Software licence:
//
// Use of this "software" is subject to the following software licence:
//
// ***** BEGIN LICENCE BLOCK *****
//
// 1) This software is distributed in the hope that it will be useful, but without warranty of any kind.
// 2) The copyright and/or originator notice(s) may not be altered or removed.
// 3) This software may be used for commercial or non-commercial use.
// 4) This software may be redistributed, provided no charge is made.
// 5) There is no obligation to make source code available to end users even if the software is modified.
// 6) Modified versions of this software will be subject to this software licence.
// 7) If the software is modified, the changes must be marked in the source code with the contributors ID (e.g. name)
//    before redistribution.
//
// ***** END LICENCE BLOCK *****
//
// In addition, users of this software are strongly encouraged to contact the originator with feedback, corrections and
// suggestions for improvement.
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Translation notes:
//
// 1) This software is preliminary. For the latest version please see "http://DirectXForDelphi.blogspot.com/".
//
// 2) The header filename suffix "_JSB" is to distinguish the files from the equivalent JEDI/Clootie files
//    and must be left in place". Interface units from different sources may not work correctly together.
//
// 3) By default, optional interface output method and function parameters are translated as "out InterfaceName:IInterfaceName",
//    not "pInterfaceName:PIInterfaceName". This is because with the pointer version, Delphi does not appear to call the
//    COM Release method on the supplied interface before assigning a new value. To pass a nil parameter, use
//    "IInterfaceName(nil^)".
//
//    PLEASE NOTE: This is different to the equivalent JEDI/Clootie files, though only minimal source code changes
//    should be required.
//
//    If you want to use pointers instead, define the conditional define "UsePointersForOptionalOutputInterfaces" but ensure
//    that the interface variable is set to nil before calling the method.
//
// 4) Please contact me if you are interested in versions for FPC or C++ etc.
//
// JSB
///////////////////////////////////////////////////////////////////////////////

interface

{$Z4}

uses
  Windows, SysUtils, {$IFDEF UseJSBErrors} UDirectXErrors, {$ENDIF} DXTypes_JSB, DXGI_JSB, D3DCommon_JSB, D3D10_JSB, D3D11_JSB;

type
  E_Effects11=class(Exception);

const
  DLL_D3DX11={$IFDEF DEBUG}'d3dx11d_43.dll'{$ELSE}'d3dx11_43.dll'{$ENDIF};
  DLL_Effects11='d3dx11Effects_JSB.dll';

///////////////////////////////////////////////////////////////////////////////
// Begin "D3DX11Core.h"
///////////////////////////////////////////////////////////////////////////////

const
  D3DX11_SDK_VERSION=42;
  _FACD3D=$876;
  D3DSTATUS_Base=LongWord(_FACD3D shl 16);
  D3DHRESULT_Base=D3DSTATUS_Base or LongWord(1 shl 31);
  //
  D3D_ERROR_INVALIDCALL=HResult(D3DHRESULT_Base or 2156);
  D3D_ERROR_WASSTILLDRAWING=HResult(D3DHRESULT_Base or 540);

{$IFDEF D3D_DIAG_DLL}

{$IFDEF UseRuntimeLinking}var D3DX11DebugMute:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11DebugMute{$ENDIF}(Mute:LongBool):LongBool; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3DX11CheckVersion:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CheckVersion{$ENDIF}(D3DSdkVersion:LongWord;D3DX11SdkVersion:LongWord):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

type
  ID3DX11DataLoader=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function Load:HResult; virtual; stdcall; abstract;
    function Decompress(var pData:Pointer;pNumBytes:PSIZE_T):HResult; virtual; stdcall; abstract;
    function Destroy:HResult; reintroduce; virtual; stdcall; abstract;
  end;

  ID3DX11DataProcessor=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function Process(pData:Pointer;NumBytes:SIZE_T):HResult; virtual; stdcall; abstract;
    function CreateDeviceObject(var pDataObject:Pointer):HResult; virtual; stdcall; abstract;
    function Destroy:HResult; reintroduce; virtual; stdcall; abstract;
  end;

  ID3DX11ThreadPump=interface(IUnknown)
    ['{C93FECFA-6967-478A-ABBC-402D90621FCB}']
    function AddWorkItem(DataLoader:ID3DX11DataLoader;DataProcessor:ID3DX11DataProcessor;pHResult:PHResult;var pDeviceObject:Pointer):HResult; stdcall;
    function GetWorkItemCount:LongWord; stdcall;
    function WaitForAllItems:HResult; stdcall;
    function ProcessDeviceWorkItems(WorkItemCount:LongWord):HResult; stdcall;
    function PurgeAllItems:HResult; stdcall;
    function GetQueueStatus(pIoQueue:PLongWord;pProcessQueue:PLongWord;pDeviceQueue:PLongWord):HResult; stdcall;
  end;

{$IFDEF UseRuntimeLinking}var D3DX11CreateThreadPump:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateThreadPump{$ENDIF}(NumIOThreads:LongWord;NumProcThreads:LongWord;out ThreadPump:ID3DX11ThreadPump):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
// JSB: Not in DLL_D3DX11. Might be implemented in C++ D3DX11.lib library: {$IFDEF UseRuntimeLinking}var D3DX11UnsetAllDeviceObjects:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11UnsetAllDeviceObjects{$ENDIF}(Context:ID3D11DeviceContext):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

///////////////////////////////////////////////////////////////////////////////
// End "D3DX11Core.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D3DX11Tex.h"
///////////////////////////////////////////////////////////////////////////////

type
  TD3DX11_FilterFlag=
  (
    D3DX11_FILTER_NONE=(1 shl 0),
    D3DX11_FILTER_POINT=(2 shl 0),
    D3DX11_FILTER_LINEAR=(3 shl 0),
    D3DX11_FILTER_TRIANGLE=(4 shl 0),
    D3DX11_FILTER_BOX=(5 shl 0),
    D3DX11_FILTER_MIRROR_U=(1 shl 16),
    D3DX11_FILTER_MIRROR_V=(2 shl 16),
    D3DX11_FILTER_MIRROR_W=(4 shl 16),
    D3DX11_FILTER_MIRROR=(7 shl 16),
    D3DX11_FILTER_DITHER=(1 shl 19),
    D3DX11_FILTER_DITHER_DIFFUSION=(2 shl 19),
    D3DX11_FILTER_SRGB_IN=(1 shl 21),
    D3DX11_FILTER_SRGB_OUT=(2 shl 21),
    D3DX11_FILTER_SRGB=(3 shl 21)
  );
  PTD3DX11_FilterFlag=^TD3DX11_FilterFlag;
  D3DX11_FILTER_FLAG=TD3DX11_FilterFlag;
  PD3DX11_FILTER_FLAG=^TD3DX11_FilterFlag;

  TD3DX11_NormalmapFlag=
  (
    D3DX11_NORMALMAP_MIRROR_U=(1 shl 16),
    D3DX11_NORMALMAP_MIRROR_V=(2 shl 16),
    D3DX11_NORMALMAP_MIRROR=(3 shl 16),
    D3DX11_NORMALMAP_INVERTSIGN=(8 shl 16),
    D3DX11_NORMALMAP_COMPUTE_OCCLUSION=(16 shl 16)
  );
  PTD3DX11_NormalmapFlag=^TD3DX11_NormalmapFlag;
  D3DX11_NORMALMAP_FLAG=TD3DX11_NormalmapFlag;
  PD3DX11_NORMALMAP_FLAG=^TD3DX11_NormalmapFlag;

  TD3DX11_ChannelFlag=
  (
    D3DX11_CHANNEL_RED=(1 shl 0),
    D3DX11_CHANNEL_BLUE=(1 shl 1),
    D3DX11_CHANNEL_GREEN=(1 shl 2),
    D3DX11_CHANNEL_ALPHA=(1 shl 3),
    D3DX11_CHANNEL_LUMINANCE=(1 shl 4)
  );
  PTD3DX11_ChannelFlag=^TD3DX11_ChannelFlag;
  D3DX11_CHANNEL_FLAG=TD3DX11_ChannelFlag;
  PD3DX11_CHANNEL_FLAG=^TD3DX11_ChannelFlag;

  TD3DX11_ImageFileFormat=
  (
    D3DX11_IFF_BMP=0,
    D3DX11_IFF_JPG=1,
    D3DX11_IFF_PNG=3,
    D3DX11_IFF_DDS=4,
    D3DX11_IFF_TIFF=10,
    D3DX11_IFF_GIF=11,
    D3DX11_IFF_WMP=12
  );
  PTD3DX11_ImageFileFormat=^TD3DX11_ImageFileFormat;
  D3DX11_IMAGE_FILE_FORMAT=TD3DX11_ImageFileFormat;
  PD3DX11_IMAGE_FILE_FORMAT=^TD3DX11_ImageFileFormat;

  TD3DX11_SaveTextureFlag=
  (
    D3DX11_STF_USEINPUTBLOB=$0001
  );
  PTD3DX11_SaveTextureFlag=^TD3DX11_SaveTextureFlag;
  D3DX11_SAVE_TEXTURE_FLAG=TD3DX11_SaveTextureFlag;
  PD3DX11_SAVE_TEXTURE_FLAG=^TD3DX11_SaveTextureFlag;

  TD3DX11_ImageInfo=record
    Width:LongWord;
    Height:LongWord;
    Depth:LongWord;
    ArraySize:LongWord;
    MipLevels:LongWord;
    MiscFlags:LongWord;
    Format:TDXGI_FORMAT;
    ResourceDimension:TD3D11_ResourceDimension;
    ImageFileFormat:TD3DX11_ImageFileFormat;
  end;
  PTD3DX11_ImageInfo=^TD3DX11_ImageInfo;
  D3DX11_IMAGE_INFO=TD3DX11_ImageInfo;
  PD3DX11_IMAGE_INFO=^TD3DX11_ImageInfo;

  TD3DX11_ImageLoadInfo=record
    Width:LongWord;
    Height:LongWord;
    Depth:LongWord;
    FirstMipLevel:LongWord;
    MipLevels:LongWord;
    Usage:TD3D11_Usage;
    BindFlags:LongWord;
    CPUAccessFlags:LongWord;
    MiscFlags:LongWord;
    Format:TDXGI_FORMAT;
    Filter:LongWord;
    MipFilter:LongWord;
    pSrcInfo:PTD3DX11_ImageInfo;
  end;
  PTD3DX11_ImageLoadInfo=^TD3DX11_ImageLoadInfo;
  D3DX11_IMAGE_LOAD_INFO=TD3DX11_ImageLoadInfo;
  PD3DX11_IMAGE_LOAD_INFO=^TD3DX11_ImageLoadInfo;

{$IFDEF UseRuntimeLinking}var D3DX11GetImageInfoFromFileA:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11GetImageInfoFromFileA{$ENDIF}(pSrcFile:PAnsiChar;Pump:ID3DX11ThreadPump;pSrcInfo:PTD3DX11_ImageInfo;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11GetImageInfoFromFileW:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11GetImageInfoFromFileW{$ENDIF}(pSrcFile:PWideChar;Pump:ID3DX11ThreadPump;pSrcInfo:PTD3DX11_ImageInfo;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

{$IFDEF UNICODE}

{$IFDEF UseRuntimeLinking}var D3DX11GetImageInfoFromFile:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11GetImageInfoFromFile{$ENDIF}(pSrcFile:PWideChar;Pump:ID3DX11ThreadPump;pSrcInfo:PTD3DX11_ImageInfo;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11GetImageInfoFromFileW';{$ENDIF}

{$ELSE}

{$IFDEF UseRuntimeLinking}var D3DX11GetImageInfoFromFile:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11GetImageInfoFromFile{$ENDIF}(pSrcFile:PAnsiChar;Pump:ID3DX11ThreadPump;pSrcInfo:PTD3DX11_ImageInfo;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11GetImageInfoFromFileA';{$ENDIF}

{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3DX11GetImageInfoFromResourceA:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11GetImageInfoFromResourceA{$ENDIF}(hSrcModule:HMODULE;pSrcResource:PAnsiChar;Pump:ID3DX11ThreadPump;pSrcInfo:PTD3DX11_ImageInfo;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11GetImageInfoFromResourceW:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11GetImageInfoFromResourceW{$ENDIF}(hSrcModule:HMODULE;pSrcResource:PWideChar;Pump:ID3DX11ThreadPump;pSrcInfo:PTD3DX11_ImageInfo;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

{$IFDEF UNICODE}

{$IFDEF UseRuntimeLinking}var D3DX11GetImageInfoFromResource:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11GetImageInfoFromResource{$ENDIF}(hSrcModule:HMODULE;pSrcResource:PWideChar;Pump:ID3DX11ThreadPump;pSrcInfo:PTD3DX11_ImageInfo;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11GetImageInfoFromResourceW';{$ENDIF}

{$ELSE}

{$IFDEF UseRuntimeLinking}var D3DX11GetImageInfoFromResource:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11GetImageInfoFromResource{$ENDIF}(hSrcModule:HMODULE;pSrcResource:PAnsiChar;Pump:ID3DX11ThreadPump;pSrcInfo:PTD3DX11_ImageInfo;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11GetImageInfoFromResourceA';{$ENDIF}

{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3DX11GetImageInfoFromMemory:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11GetImageInfoFromMemory{$ENDIF}(pSrcData:Pointer;SrcDataSize:SIZE_T;Pump:ID3DX11ThreadPump;pSrcInfo:PTD3DX11_ImageInfo;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CreateShaderResourceViewFromFileA:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateShaderResourceViewFromFileA{$ENDIF}(Device:ID3D11Device;pSrcFile:PAnsiChar;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out ShaderResourceView:ID3D11ShaderResourceView;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CreateShaderResourceViewFromFileW:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateShaderResourceViewFromFileW{$ENDIF}(Device:ID3D11Device;pSrcFile:PWideChar;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out ShaderResourceView:ID3D11ShaderResourceView;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

{$IFDEF UNICODE}

{$IFDEF UseRuntimeLinking}var D3DX11CreateShaderResourceViewFromFile:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateShaderResourceViewFromFile{$ENDIF}(Device:ID3D11Device;pSrcFile:PWideChar;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out ShaderResourceView:ID3D11ShaderResourceView;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11CreateShaderResourceViewFromFileW';{$ENDIF}

{$ELSE}

{$IFDEF UseRuntimeLinking}var D3DX11CreateShaderResourceViewFromFile:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateShaderResourceViewFromFile{$ENDIF}(Device:ID3D11Device;pSrcFile:PAnsiChar;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out ShaderResourceView:ID3D11ShaderResourceView;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11CreateShaderResourceViewFromFileA';{$ENDIF}

{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3DX11CreateTextureFromFileA:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateTextureFromFileA{$ENDIF}(Device:ID3D11Device;pSrcFile:PAnsiChar;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out Texture:ID3D11Resource;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CreateTextureFromFileW:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateTextureFromFileW{$ENDIF}(Device:ID3D11Device;pSrcFile:PWideChar;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out Texture:ID3D11Resource;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

{$IFDEF UNICODE}

{$IFDEF UseRuntimeLinking}var D3DX11CreateTextureFromFile:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateTextureFromFile{$ENDIF}(Device:ID3D11Device;pSrcFile:PWideChar;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out Texture:ID3D11Resource;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11CreateTextureFromFileW';{$ENDIF}

{$ELSE}

{$IFDEF UseRuntimeLinking}var D3DX11CreateTextureFromFile:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateTextureFromFile{$ENDIF}(Device:ID3D11Device;pSrcFile:PAnsiChar;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out Texture:ID3D11Resource;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11CreateTextureFromFileA';{$ENDIF}

{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3DX11CreateShaderResourceViewFromResourceA:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateShaderResourceViewFromResourceA{$ENDIF}(Device:ID3D11Device;hSrcModule:HMODULE;pSrcResource:PAnsiChar;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out ShaderResourceView:ID3D11ShaderResourceView;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CreateShaderResourceViewFromResourceW:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateShaderResourceViewFromResourceW{$ENDIF}(Device:ID3D11Device;hSrcModule:HMODULE;pSrcResource:PWideChar;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out ShaderResourceView:ID3D11ShaderResourceView;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

{$IFDEF UNICODE}

{$IFDEF UseRuntimeLinking}var D3DX11CreateShaderResourceViewFromResource:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateShaderResourceViewFromResource{$ENDIF}(Device:ID3D11Device;hSrcModule:HMODULE;pSrcResource:PWideChar;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out ShaderResourceView:ID3D11ShaderResourceView;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11CreateShaderResourceViewFromResourceW';{$ENDIF}

{$ELSE}

{$IFDEF UseRuntimeLinking}var D3DX11CreateShaderResourceViewFromResource:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateShaderResourceViewFromResource{$ENDIF}(Device:ID3D11Device;hSrcModule:HMODULE;pSrcResource:PAnsiChar;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out ShaderResourceView:ID3D11ShaderResourceView;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11CreateShaderResourceViewFromResourceA';{$ENDIF}

{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3DX11CreateTextureFromResourceA:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateTextureFromResourceA{$ENDIF}(Device:ID3D11Device;hSrcModule:HMODULE;pSrcResource:PAnsiChar;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out Texture:ID3D11Resource;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CreateTextureFromResourceW:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateTextureFromResourceW{$ENDIF}(Device:ID3D11Device;hSrcModule:HMODULE;pSrcResource:PWideChar;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out Texture:ID3D11Resource;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

{$IFDEF UNICODE}

{$IFDEF UseRuntimeLinking}var D3DX11CreateTextureFromResource:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateTextureFromResource{$ENDIF}(Device:ID3D11Device;hSrcModule:HMODULE;pSrcResource:PWideChar;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out Texture:ID3D11Resource;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11CreateTextureFromResourceW';{$ENDIF}

{$ELSE}

{$IFDEF UseRuntimeLinking}var D3DX11CreateTextureFromResource:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateTextureFromResource{$ENDIF}(Device:ID3D11Device;hSrcModule:HMODULE;pSrcResource:PAnsiChar;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out Texture:ID3D11Resource;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11CreateTextureFromResourceA';{$ENDIF}

{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3DX11CreateShaderResourceViewFromMemory:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateShaderResourceViewFromMemory{$ENDIF}(Device:ID3D11Device;pSrcData:Pointer;SrcDataSize:SIZE_T;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out ShaderResourceView:ID3D11ShaderResourceView;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CreateTextureFromMemory:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateTextureFromMemory{$ENDIF}(Device:ID3D11Device;pSrcData:Pointer;SrcDataSize:SIZE_T;pLoadInfo:PTD3DX11_ImageLoadInfo;Pump:ID3DX11ThreadPump;out Texture:ID3D11Resource;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

type
  TD3DX11_TextureLoadInfo=record
    pSrcBox:PTD3D11_Box;
    pDstBox:PTD3D11_Box;
    SrcFirstMip:LongWord;
    DstFirstMip:LongWord;
    NumMips:LongWord;
    SrcFirstElement:LongWord;
    DstFirstElement:LongWord;
    NumElements:LongWord;
    Filter:LongWord;
    MipFilter:LongWord;
  end;
  PTD3DX11_TextureLoadInfo=^TD3DX11_TextureLoadInfo;
  D3DX11_TEXTURE_LOAD_INFO=TD3DX11_TextureLoadInfo;
  PD3DX11_TEXTURE_LOAD_INFO=^TD3DX11_TextureLoadInfo;

{$IFDEF UseRuntimeLinking}var D3DX11LoadTextureFromTexture:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11LoadTextureFromTexture{$ENDIF}(Context:ID3D11DeviceContext;SrcTexture:ID3D11Resource;pLoadInfo:PTD3DX11_TextureLoadInfo;DstTexture:ID3D11Resource):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11FilterTexture:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11FilterTexture{$ENDIF}(Context:ID3D11DeviceContext;Texture:ID3D11Resource;SrcLevel:LongWord;MipFilter:LongWord):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11SaveTextureToFileA:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11SaveTextureToFileA{$ENDIF}(Context:ID3D11DeviceContext;SrcTexture:ID3D11Resource;DestFormat:TD3DX11_ImageFileFormat;pDestFile:PAnsiChar):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11SaveTextureToFileW:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11SaveTextureToFileW{$ENDIF}(Context:ID3D11DeviceContext;SrcTexture:ID3D11Resource;DestFormat:TD3DX11_ImageFileFormat;pDestFile:PWideChar):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

{$IFDEF UNICODE}

{$IFDEF UseRuntimeLinking}var D3DX11SaveTextureToFile:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11SaveTextureToFile{$ENDIF}(Context:ID3D11DeviceContext;SrcTexture:ID3D11Resource;DestFormat:TD3DX11_ImageFileFormat;pDestFile:PWideChar):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11SaveTextureToFileW';{$ENDIF}

{$ELSE}

{$IFDEF UseRuntimeLinking}var D3DX11SaveTextureToFile:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11SaveTextureToFile{$ENDIF}(Context:ID3D11DeviceContext;SrcTexture:ID3D11Resource;DestFormat:TD3DX11_ImageFileFormat;pDestFile:PAnsiChar):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11SaveTextureToFileA';{$ENDIF}

{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3DX11SaveTextureToMemory:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11SaveTextureToMemory{$ENDIF}(Context:ID3D11DeviceContext;SrcTexture:ID3D11Resource;DestFormat:TD3DX11_ImageFileFormat;out DestBuf:ID3D10Blob;Flags:LongWord):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11ComputeNormalMap:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11ComputeNormalMap{$ENDIF}(Context:ID3D11DeviceContext;SrcTexture:ID3D11Texture2D;Flags:LongWord;Channel:LongWord;Amplitude:Single;DestTexture:ID3D11Texture2D):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11SHProjectCubeMap:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11SHProjectCubeMap{$ENDIF}(Context:ID3D11DeviceContext;Order:LongWord;CubeMap:ID3D11Texture2D;pROut:PSingle;pGOut:PSingle;pBOut:PSingle):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

///////////////////////////////////////////////////////////////////////////////
// End "D3DX11Tex.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D3DX11Async.h"
///////////////////////////////////////////////////////////////////////////////

{$IFDEF UseRuntimeLinking}var D3DX11CompileFromFileA:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CompileFromFileA{$ENDIF}(pSrcFile:PAnsiChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;pFunctionName:PAnsiChar;pProfile:PAnsiChar;Flags1:LongWord;Flags2:LongWord;Pump:ID3DX11ThreadPump;out Shader:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CompileFromFileW:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CompileFromFileW{$ENDIF}(pSrcFile:PWideChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;pFunctionName:PAnsiChar;pProfile:PAnsiChar;Flags1:LongWord;Flags2:LongWord;Pump:ID3DX11ThreadPump;out Shader:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

{$IFDEF UNICODE}

{$IFDEF UseRuntimeLinking}var D3DX11CompileFromFile:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CompileFromFile{$ENDIF}(pSrcFile:PWideChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;pFunctionName:PAnsiChar;pProfile:PAnsiChar;Flags1:LongWord;Flags2:LongWord;Pump:ID3DX11ThreadPump;out Shader:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11CompileFromFileW';{$ENDIF}

{$ELSE}

{$IFDEF UseRuntimeLinking}var D3DX11CompileFromFile:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CompileFromFile{$ENDIF}(pSrcFile:PAnsiChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;pFunctionName:PAnsiChar;pProfile:PAnsiChar;Flags1:LongWord;Flags2:LongWord;Pump:ID3DX11ThreadPump;out Shader:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11CompileFromFileA';{$ENDIF}

{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3DX11CompileFromResourceA:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CompileFromResourceA{$ENDIF}(hSrcModule:HMODULE;pSrcResource:PAnsiChar;pSrcFileName:PAnsiChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;pFunctionName:PAnsiChar;pProfile:PAnsiChar;Flags1:LongWord;Flags2:LongWord;Pump:ID3DX11ThreadPump;out Shader:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CompileFromResourceW:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CompileFromResourceW{$ENDIF}(hSrcModule:HMODULE;pSrcResource:PWideChar;pSrcFileName:PWideChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;pFunctionName:PAnsiChar;pProfile:PAnsiChar;Flags1:LongWord;Flags2:LongWord;Pump:ID3DX11ThreadPump;out Shader:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

{$IFDEF UNICODE}

{$IFDEF UseRuntimeLinking}var D3DX11CompileFromResource:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CompileFromResource{$ENDIF}(hSrcModule:HMODULE;pSrcResource:PWideChar;pSrcFileName:PWideChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;pFunctionName:PAnsiChar;pProfile:PAnsiChar;Flags1:LongWord;Flags2:LongWord;Pump:ID3DX11ThreadPump;out Shader:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11CompileFromResourceW';{$ENDIF}

{$ELSE}

{$IFDEF UseRuntimeLinking}var D3DX11CompileFromResource:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CompileFromResource{$ENDIF}(hSrcModule:HMODULE;pSrcResource:PAnsiChar;pSrcFileName:PAnsiChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;pFunctionName:PAnsiChar;pProfile:PAnsiChar;Flags1:LongWord;Flags2:LongWord;Pump:ID3DX11ThreadPump;out Shader:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11CompileFromResourceA';{$ENDIF}

{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3DX11CompileFromMemory:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CompileFromMemory{$ENDIF}(pSrcData:PAnsiChar;SrcDataLen:SIZE_T;pFileName:PAnsiChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;pFunctionName:PAnsiChar;pProfile:PAnsiChar;Flags1:LongWord;Flags2:LongWord;Pump:ID3DX11ThreadPump;out Shader:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11PreprocessShaderFromFileA:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11PreprocessShaderFromFileA{$ENDIF}(pFileName:PAnsiChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;Pump:ID3DX11ThreadPump;out ShaderText:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11PreprocessShaderFromFileW:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11PreprocessShaderFromFileW{$ENDIF}(pFileName:PWideChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;Pump:ID3DX11ThreadPump;out ShaderText:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11PreprocessShaderFromMemory:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11PreprocessShaderFromMemory{$ENDIF}(pSrcData:PAnsiChar;SrcDataSize:SIZE_T;pFileName:PAnsiChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;Pump:ID3DX11ThreadPump;out ShaderText:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11PreprocessShaderFromResourceA:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11PreprocessShaderFromResourceA{$ENDIF}(hModule:HMODULE;pResourceName:PAnsiChar;pSrcFileName:PAnsiChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;Pump:ID3DX11ThreadPump;out ShaderText:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11PreprocessShaderFromResourceW:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11PreprocessShaderFromResourceW{$ENDIF}(hModule:HMODULE;pResourceName:PWideChar;pSrcFileName:PWideChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;Pump:ID3DX11ThreadPump;out ShaderText:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

{$IFDEF UNICODE}

{$IFDEF UseRuntimeLinking}var D3DX11PreprocessShaderFromFile:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11PreprocessShaderFromFile{$ENDIF}(pFileName:PWideChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;Pump:ID3DX11ThreadPump;out ShaderText:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11PreprocessShaderFromFileW';{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11PreprocessShaderFromResource:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11PreprocessShaderFromResource{$ENDIF}(hModule:HMODULE;pResourceName:PWideChar;pSrcFileName:PWideChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;Pump:ID3DX11ThreadPump;out ShaderText:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11PreprocessShaderFromResourceW';{$ENDIF}

{$ELSE}

{$IFDEF UseRuntimeLinking}var D3DX11PreprocessShaderFromFile:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11PreprocessShaderFromFile{$ENDIF}(pFileName:PAnsiChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;Pump:ID3DX11ThreadPump;out ShaderText:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11PreprocessShaderFromFileA';{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11PreprocessShaderFromResource:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11PreprocessShaderFromResource{$ENDIF}(hModule:HMODULE;pResourceName:PAnsiChar;pSrcFileName:PAnsiChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;Pump:ID3DX11ThreadPump;out ShaderText:ID3D10Blob;out ErrorMsgs:ID3D10Blob;pHResult:PHResult):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11PreprocessShaderFromResourceA';{$ENDIF}

{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3DX11CreateAsyncCompilerProcessor:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateAsyncCompilerProcessor{$ENDIF}(pFileName:PAnsiChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;pFunctionName:PAnsiChar;pProfile:PAnsiChar;Flags1:LongWord;Flags2:LongWord;out CompiledShader:ID3D10Blob;out ErrorBuffer:ID3D10Blob;out Processor:ID3DX11DataProcessor):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CreateAsyncShaderPreprocessProcessor:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateAsyncShaderPreprocessProcessor{$ENDIF}(pFileName:PAnsiChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;out ShaderText:ID3D10Blob;out ErrorBuffer:ID3D10Blob;out Processor:ID3DX11DataProcessor):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CreateAsyncFileLoaderW:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateAsyncFileLoaderW{$ENDIF}(pFileName:PWideChar;out DataLoader:ID3DX11DataLoader):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CreateAsyncFileLoaderA:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateAsyncFileLoaderA{$ENDIF}(pFileName:PAnsiChar;out DataLoader:ID3DX11DataLoader):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CreateAsyncMemoryLoader:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateAsyncMemoryLoader{$ENDIF}(pData:Pointer;CbData:SIZE_T;out DataLoader:ID3DX11DataLoader):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CreateAsyncResourceLoaderW:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateAsyncResourceLoaderW{$ENDIF}(hSrcModule:HMODULE;pSrcResource:PWideChar;out DataLoader:ID3DX11DataLoader):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CreateAsyncResourceLoaderA:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateAsyncResourceLoaderA{$ENDIF}(hSrcModule:HMODULE;pSrcResource:PAnsiChar;out DataLoader:ID3DX11DataLoader):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

{$IFDEF UNICODE}

{$IFDEF UseRuntimeLinking}var D3DX11CreateAsyncFileLoader:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateAsyncFileLoader{$ENDIF}(pFileName:PWideChar;out DataLoader:ID3DX11DataLoader):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11CreateAsyncFileLoaderW';{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CreateAsyncResourceLoader:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateAsyncResourceLoader{$ENDIF}(hSrcModule:HMODULE;pSrcResource:PWideChar;out DataLoader:ID3DX11DataLoader):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11CreateAsyncResourceLoaderW';{$ENDIF}

{$ELSE}

{$IFDEF UseRuntimeLinking}var D3DX11CreateAsyncFileLoader:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateAsyncFileLoader{$ENDIF}(pFileName:PAnsiChar;out DataLoader:ID3DX11DataLoader):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11CreateAsyncFileLoaderA';{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CreateAsyncResourceLoader:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateAsyncResourceLoader{$ENDIF}(hSrcModule:HMODULE;pSrcResource:PAnsiChar;out DataLoader:ID3DX11DataLoader):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11 name 'D3DX11CreateAsyncResourceLoaderA';{$ENDIF}

{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3DX11CreateAsyncTextureProcessor:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateAsyncTextureProcessor{$ENDIF}(Device:ID3D11Device;pLoadInfo:PTD3DX11_ImageLoadInfo;out DataProcessor:ID3DX11DataProcessor):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CreateAsyncTextureInfoProcessor:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateAsyncTextureInfoProcessor{$ENDIF}(pImageInfo:PTD3DX11_ImageInfo;out DataProcessor:ID3DX11DataProcessor):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3DX11CreateAsyncShaderResourceViewProcessor:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateAsyncShaderResourceViewProcessor{$ENDIF}(Device:ID3D11Device;pLoadInfo:PTD3DX11_ImageLoadInfo;out DataProcessor:ID3DX11DataProcessor):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3DX11;{$ENDIF}

///////////////////////////////////////////////////////////////////////////////
// End "D3DX11Async.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D3DX11.h"
///////////////////////////////////////////////////////////////////////////////

const
  D3DX11_DEFAULT=-1;
  D3DX11_FROM_FILE=-3;
  DXGI_FORMAT_FROM_FILE=TDXGI_Format(-3);
  _FACDD=$876;
  DDSTATUS_Base=UINT(_FACDD shl 16);
  DDHResult_Base=DDSTATUS_Base or UINT(1 shl 31);

const
  D3DX11_ERROR_CANNOT_MODIFY_INDEX_BUFFER=HResult(DDHResult_Base or 2900);
  D3DX11_ERROR_INVALID_MESH=HResult(DDHResult_Base or 2901);
  D3DX11_ERROR_CANNOT_ATTR_SORT=HResult(DDHResult_Base or 2902);
  D3DX11_ERROR_SKINNING_NOT_SUPPORTED=HResult(DDHResult_Base or 2903);
  D3DX11_ERROR_TOO_MANY_INFLUENCES=HResult(DDHResult_Base or 2904);
  D3DX11_ERROR_INVALID_DATA=HResult(DDHResult_Base or 2905);
  D3DX11_ERROR_LOADED_MESH_HAS_NO_DATA=HResult(DDHResult_Base or 2906);
  D3DX11_ERROR_DUPLICATE_NAMED_FRAGMENT=HResult(DDHResult_Base or 2907);
  D3DX11_ERROR_CANNOT_REMOVE_LAST_ITEM=HResult(DDHResult_Base or 2908);

///////////////////////////////////////////////////////////////////////////////
// End "D3DX11.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D3DX11Effect.h"
///////////////////////////////////////////////////////////////////////////////

const
  D3DX11_EFFECT_OPTIMIZED=(1 shl 21);
  D3DX11_EFFECT_CLONE=(1 shl 22);
  D3DX11_EFFECT_RUNTIME_VALID_FLAGS=(0);
  D3DX11_EFFECT_VARIABLE_ANNOTATION=(1 shl 1);
  D3DX11_EFFECT_VARIABLE_EXPLICIT_BIND_POINT=(1 shl 2);
  D3DX11_EFFECT_CLONE_FORCE_NONSINGLE=(1 shl 0);
  D3DX11_EFFECT_PASS_COMMIT_CHANGES=(1 shl 0);
  D3DX11_EFFECT_PASS_OMIT_SHADERS_AND_INTERFACES=(1 shl 1);
  D3DX11_EFFECT_PASS_OMIT_STATE_OBJECTS=(1 shl 2);
  D3DX11_EFFECT_PASS_OMIT_RTVS_AND_DSVS=(1 shl 3);
  D3DX11_EFFECT_PASS_OMIT_SAMPLERS=(1 shl 4);
  D3DX11_EFFECT_PASS_OMIT_CBS=(1 shl 5);
  D3DX11_EFFECT_PASS_OMIT_SRVS=(1 shl 6);
  D3DX11_EFFECT_PASS_OMIT_UAVS=(1 shl 7);
  D3DX11_EFFECT_PASS_ONLY_SET_SHADERS_AND_CBS=
  (
    D3DX11_EFFECT_PASS_OMIT_STATE_OBJECTS
      or D3DX11_EFFECT_PASS_OMIT_RTVS_AND_DSVS
      or D3DX11_EFFECT_PASS_OMIT_SAMPLERS
      or D3DX11_EFFECT_PASS_OMIT_SRVS
      or D3DX11_EFFECT_PASS_OMIT_UAVS
  );

type
  TD3DX11_StateBlockMask=record
    VS:Byte;
    VSSamplers:array[0..1] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT)
    VSShaderResources:array[0..15] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT)
    VSConstantBuffers:array[0..1] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT)
    VSInterfaces:array[0..31] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_SHADER_MAX_INTERFACES)

    HS:Byte;
    HSSamplers:array[0..1] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT)
    HSShaderResources:array[0..15] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT)
    HSConstantBuffers:array[0..1] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT)
    HSInterfaces:array[0..31] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_SHADER_MAX_INTERFACES)

    DS:Byte;
    DSSamplers:array[0..1] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT)
    DSShaderResources:array[0..15] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT)
    DSConstantBuffers:array[0..1] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT)
    DSInterfaces:array[0..31] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_SHADER_MAX_INTERFACES)-1

    GS:Byte;
    GSSamplers:array[0..1] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT)
    GSShaderResources:array[0..15] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT)
    GSConstantBuffers:array[0..1] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT)
    GSInterfaces:array[0..31] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_SHADER_MAX_INTERFACES)

    PS:Byte;
    PSSamplers:array[0..1] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT)
    PSShaderResources:array[0..15] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT)
    PSConstantBuffers:array[0..1] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT)
    PSInterfaces:array[0..31] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_SHADER_MAX_INTERFACES)
    PSUnorderedAccessViews:Byte;

    CS:Byte;
    CSSamplers:array[0..1] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT)
    CSShaderResources:array[0..15] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT)
    CSConstantBuffers:array[0..1] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT)
    CSInterfaces:array[0..31] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_SHADER_MAX_INTERFACES)
    CSUnorderedAccessViews:Byte;

    IAVertexBuffers:array[0..3] of Byte; // D3DX11_BYTES_FROM_BITS(D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT)
    IAIndexBuffer:Byte;
    IAInputLayout:Byte;
    IAPrimitiveTopology:Byte;

    OMRenderTargets:Byte;
    OMDepthStencilState:Byte;
    OMBlendState:Byte;

    RSViewports:Byte;
    RSScissorRects:Byte;
    RSRasterizerState:Byte;

    SOBuffers:Byte;

    Predication:Byte;
  end;
  PTD3DX11_StateBlockMask=^TD3DX11_StateBlockMask;
  D3DX11_STATE_BLOCK_MASK=TD3DX11_StateBlockMask;
  PD3DX11_STATE_BLOCK_MASK=^TD3DX11_StateBlockMask;

  TD3DX11_EffectTypeDesc=record
    TypeName:PAnsiChar;
    _Class:TD3D10_ShaderVariableClass;
    _Type:TD3D10_ShaderVariableType;
    Elements:LongWord;
    Members:LongWord;
    Rows:LongWord;
    Columns:LongWord;
    PackedSize:LongWord;
    UnpackedSize:LongWord;
    Stride:LongWord;
  end;
  PTD3DX11_EffectTypeDesc=^TD3DX11_EffectTypeDesc;
  D3DX11_EFFECT_TYPE_DESC=TD3DX11_EffectTypeDesc;
  PD3DX11_EFFECT_TYPE_DESC=^TD3DX11_EffectTypeDesc;

  ID3DX11EffectType=class;
  PID3DX11EffectType=^ID3DX11EffectType;

  ID3DX11EffectType=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectTypeDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetMemberTypeByIndex
    (
      Index:LongWord
    ):ID3DX11EffectType; virtual; stdcall; abstract;

    function GetMemberTypeByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectType; virtual; stdcall; abstract;

    function GetMemberTypeBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectType; virtual; stdcall; abstract;

    function GetMemberName
    (
      Index:LongWord
    ):PAnsiChar; virtual; stdcall; abstract;

    function GetMemberSemantic
    (
      Index:LongWord
    ):PAnsiChar; virtual; stdcall; abstract;
  end;

  TD3DX11_EffectVariableDesc=record
    Name:PAnsiChar;
    Semantic:PAnsiChar;
    Flags:LongWord;
    Annotations:LongWord;
    BufferOffset:LongWord;
    ExplicitBindPoint:LongWord;
  end;
  PTD3DX11_EffectVariableDesc=^TD3DX11_EffectVariableDesc;
  D3DX11_EFFECT_VARIABLE_DESC=TD3DX11_EffectVariableDesc;
  PD3DX11_EFFECT_VARIABLE_DESC=^TD3DX11_EffectVariableDesc;

  ID3DX11EffectVariable=class;
  PID3DX11EffectVariable=^ID3DX11EffectVariable;

  ID3DX11EffectScalarVariable=class;
  PID3DX11EffectScalarVariable=^ID3DX11EffectScalarVariable;

  ID3DX11EffectVectorVariable=class;
  PID3DX11EffectVectorVariable=^ID3DX11EffectVectorVariable;

  ID3DX11EffectMatrixVariable=class;
  PID3DX11EffectMatrixVariable=^ID3DX11EffectMatrixVariable;

  ID3DX11EffectStringVariable=class;
  PID3DX11EffectStringVariable=^ID3DX11EffectStringVariable;

  ID3DX11EffectClassInstanceVariable=class;
  PID3DX11EffectClassInstanceVariable=^ID3DX11EffectClassInstanceVariable;

  ID3DX11EffectInterfaceVariable=class;
  PID3DX11EffectInterfaceVariable=^ID3DX11EffectInterfaceVariable;

  ID3DX11EffectShaderResourceVariable=class;
  PID3DX11EffectShaderResourceVariable=^ID3DX11EffectShaderResourceVariable;

  ID3DX11EffectUnorderedAccessViewVariable=class;
  PID3DX11EffectUnorderedAccessViewVariable=^ID3DX11EffectUnorderedAccessViewVariable;

  ID3DX11EffectRenderTargetViewVariable=class;
  PID3DX11EffectRenderTargetViewVariable=^ID3DX11EffectRenderTargetViewVariable;

  ID3DX11EffectDepthStencilViewVariable=class;
  PID3DX11EffectDepthStencilViewVariable=^ID3DX11EffectDepthStencilViewVariable;

  ID3DX11EffectConstantBuffer=class;
  PID3DX11EffectConstantBuffer=^ID3DX11EffectConstantBuffer;

  ID3DX11EffectShaderVariable=class;
  PID3DX11EffectShaderVariable=^ID3DX11EffectShaderVariable;

  ID3DX11EffectBlendVariable=class;
  PID3DX11EffectBlendVariable=^ID3DX11EffectBlendVariable;

  ID3DX11EffectDepthStencilVariable=class;
  PID3DX11EffectDepthStencilVariable=^ID3DX11EffectDepthStencilVariable;

  ID3DX11EffectRasterizerVariable=class;
  PID3DX11EffectRasterizerVariable=^ID3DX11EffectRasterizerVariable;

  ID3DX11EffectSamplerVariable=class;
  PID3DX11EffectSamplerVariable=^ID3DX11EffectSamplerVariable;

  ID3DX11EffectVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3DX11EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3DX11EffectScalarVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3DX11EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      ByteOffset:LongWord;
      ByteCount:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      ByteOffset:LongWord;
      ByteCount:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetFloat
    (
      Value:Single
    ):HResult; virtual; stdcall; abstract;

    function GetFloat
    (
      pValue:PSingle
    ):HResult; virtual; stdcall; abstract;

    function SetFloatArray
    (
      pData:PSingle; (* __in_ecount(Count) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetFloatArray
    (
      pData:PSingle;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetInt
    (
      Value:Integer
    ):HResult; virtual; stdcall; abstract;

    function GetInt
    (
      pValue:PInteger
    ):HResult; virtual; stdcall; abstract;

    function SetIntArray
    (
      pData:PInteger; (* __in_ecount(Count) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetIntArray
    (
      pData:PInteger;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetBool
    (
      Value:LongBool
    ):HResult; virtual; stdcall; abstract;

    function GetBool
    (
      pValue:PLongBool
    ):HResult; virtual; stdcall; abstract;

    function SetBoolArray
    (
      pData:PLongBool; (* __in_ecount(Count) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetBoolArray
    (
      pData:PLongBool;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3DX11EffectVectorVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3DX11EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      ByteOffset:LongWord;
      ByteCount:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      ByteOffset:LongWord;
      ByteCount:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetBoolVector
    (
      pData:PLongBool (* __in_ecount(Count) *)
    ):HResult; virtual; stdcall; abstract;

    function SetIntVector
    (
      pData:PInteger (* __in_ecount(Count) *)
    ):HResult; virtual; stdcall; abstract;

    function SetFloatVector
    (
      pData:PSingle (* __in_ecount(Count) *)
    ):HResult; virtual; stdcall; abstract;

    function GetBoolVector
    (
      pData:PLongBool
    ):HResult; virtual; stdcall; abstract;

    function GetIntVector
    (
      pData:PInteger
    ):HResult; virtual; stdcall; abstract;

    function GetFloatVector
    (
      pData:PSingle
    ):HResult; virtual; stdcall; abstract;

    function SetBoolVectorArray
    (
      pData:PLongBool; (* __in_ecount(Count) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetIntVectorArray
    (
      pData:PInteger; (* __in_ecount(Count) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetFloatVectorArray
    (
      pData:PSingle; (* __in_ecount(Count) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetBoolVectorArray
    (
      pData:PLongBool;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetIntVectorArray
    (
      pData:PInteger;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetFloatVectorArray
    (
      pData:PSingle;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3DX11EffectMatrixVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3DX11EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      ByteOffset:LongWord;
      ByteCount:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      ByteOffset:LongWord;
      ByteCount:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetMatrix
    (
      pData:PSingle (* __in_ecount(Count) *)
    ):HResult; virtual; stdcall; abstract;

    function GetMatrix
    (
      pData:PSingle
    ):HResult; virtual; stdcall; abstract;

    function SetMatrixArray
    (
      pData:PSingle; (* __in_ecount(Count) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetMatrixArray
    (
      pData:PSingle;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetMatrixTranspose
    (
      pData:PSingle (* __in_ecount(Count) *)
    ):HResult; virtual; stdcall; abstract;

    function GetMatrixTranspose
    (
      pData:PSingle
    ):HResult; virtual; stdcall; abstract;

    function SetMatrixTransposeArray
    (
      pData:PSingle; (* __in_ecount(Count) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetMatrixTransposeArray
    (
      pData:PSingle;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3DX11EffectStringVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3DX11EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetString
    (
      ppString:PPAnsiChar
    ):HResult; virtual; stdcall; abstract;

    function GetStringArray
    (
      ppStrings:PPAnsiChar;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3DX11EffectClassInstanceVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetClassInstance
    (
      out ClassInstance:ID3D11ClassInstance
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3DX11EffectInterfaceVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetClassInstance
    (
      EffectClassInstance:ID3DX11EffectClassInstanceVariable
    ):HResult; virtual; stdcall; abstract;

    function GetClassInstance
    (
      out EffectClassInstance:ID3DX11EffectClassInstanceVariable
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3DX11EffectShaderResourceVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3DX11EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetResource
    (
      Resource:ID3D11ShaderResourceView
    ):HResult; virtual; stdcall; abstract;

    function GetResource
    (
      out Resource:ID3D11ShaderResourceView
    ):HResult; virtual; stdcall; abstract;

    function SetResourceArray
    (
      pResources:PID3D11ShaderResourceView; (* __in_ecount(0) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetResourceArray
    (
      pResources:PID3D11ShaderResourceView; (* __out_ecount(0) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3DX11EffectUnorderedAccessViewVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3DX11EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetUnorderedAccessView
    (
      Resource:ID3D11UnorderedAccessView
    ):HResult; virtual; stdcall; abstract;

    function GetUnorderedAccessView
    (
      out Resource:ID3D11UnorderedAccessView
    ):HResult; virtual; stdcall; abstract;

    function SetUnorderedAccessViewArray
    (
      out Resources:ID3D11UnorderedAccessView;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetUnorderedAccessViewArray
    (
      out Resources:ID3D11UnorderedAccessView;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3DX11EffectRenderTargetViewVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3DX11EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetRenderTarget
    (
      Resource:ID3D11RenderTargetView
    ):HResult; virtual; stdcall; abstract;

    function GetRenderTarget
    (
      out Resource:ID3D11RenderTargetView
    ):HResult; virtual; stdcall; abstract;

    function SetRenderTargetArray
    (
      out Resources:ID3D11RenderTargetView;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRenderTargetArray
    (
      out Resources:ID3D11RenderTargetView;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3DX11EffectDepthStencilViewVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3DX11EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetDepthStencil
    (
      Resource:ID3D11DepthStencilView
    ):HResult; virtual; stdcall; abstract;

    function GetDepthStencil
    (
      out Resource:ID3D11DepthStencilView
    ):HResult; virtual; stdcall; abstract;

    function SetDepthStencilArray
    (
      out Resources:ID3D11DepthStencilView;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetDepthStencilArray
    (
      out Resources:ID3D11DepthStencilView;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3DX11EffectConstantBuffer=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3DX11EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetConstantBuffer
    (
      ConstantBuffer:ID3D11Buffer
    ):HResult; virtual; stdcall; abstract;

    function UndoSetConstantBuffer:HResult; virtual; stdcall; abstract;

    function GetConstantBuffer
    (
      out ConstantBuffer:ID3D11Buffer
    ):HResult; virtual; stdcall; abstract;

    function SetTextureBuffer
    (
      TextureBuffer:ID3D11ShaderResourceView
    ):HResult; virtual; stdcall; abstract;

    function UndoSetTextureBuffer:HResult; virtual; stdcall; abstract;

    function GetTextureBuffer
    (
      out TextureBuffer:ID3D11ShaderResourceView
    ):HResult; virtual; stdcall; abstract;
  end;

  TD3DX11_EffectShaderDesc=record
    pInputSignature:PByte;
    IsInline:LongBool;
    pBytecode:PByte;
    BytecodeLength:LongWord;
    SODecls:array[0..D3D11_SO_STREAM_COUNT-1] of PAnsiChar;
    RasterizedStream:LongWord;
    NumInputSignatureEntries:LongWord;
    NumOutputSignatureEntries:LongWord;
    NumPatchConstantSignatureEntries:LongWord;
  end;
  PTD3DX11_EffectShaderDesc=^TD3DX11_EffectShaderDesc;
  D3DX11_EFFECT_SHADER_DESC=TD3DX11_EffectShaderDesc;
  PD3DX11_EFFECT_SHADER_DESC=^TD3DX11_EffectShaderDesc;

  ID3DX11EffectShaderVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3DX11EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetShaderDesc
    (
      ShaderIndex:LongWord;
      out Desc:TD3DX11_EffectShaderDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetVertexShader
    (
      ShaderIndex:LongWord;
      out VS:ID3D11VertexShader
    ):HResult; virtual; stdcall; abstract;

    function GetGeometryShader
    (
      ShaderIndex:LongWord;
      out GS:ID3D11GeometryShader
    ):HResult; virtual; stdcall; abstract;

    function GetPixelShader
    (
      ShaderIndex:LongWord;
      out PS:ID3D11PixelShader
    ):HResult; virtual; stdcall; abstract;

    function GetHullShader
    (
      ShaderIndex:LongWord;
      out PS:ID3D11HullShader
    ):HResult; virtual; stdcall; abstract;

    function GetDomainShader
    (
      ShaderIndex:LongWord;
      out PS:ID3D11DomainShader
    ):HResult; virtual; stdcall; abstract;

    function GetComputeShader
    (
      ShaderIndex:LongWord;
      out PS:ID3D11ComputeShader
    ):HResult; virtual; stdcall; abstract;

    function GetInputSignatureElementDesc
    (
      ShaderIndex:LongWord;
      Element:LongWord;
      pDesc:PTD3D11_SignatureParameterDesc
    ):HResult; virtual; stdcall; abstract;

    function GetOutputSignatureElementDesc
    (
      ShaderIndex:LongWord;
      Element:LongWord;
      pDesc:PTD3D11_SignatureParameterDesc
    ):HResult; virtual; stdcall; abstract;

    function GetPatchConstantSignatureElementDesc
    (
      ShaderIndex:LongWord;
      Element:LongWord;
      pDesc:PTD3D11_SignatureParameterDesc
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3DX11EffectBlendVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3DX11EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetBlendState
    (
      Index:LongWord;
      out BlendState:ID3D11BlendState
    ):HResult; virtual; stdcall; abstract;

    function SetBlendState
    (
      Index:LongWord;
      BlendState:ID3D11BlendState
    ):HResult; virtual; stdcall; abstract;

    function UndoSetBlendState
    (
      Index:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetBackingStore
    (
      Index:LongWord;
      pBlendDesc:PTD3D11_BlendDesc
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3DX11EffectDepthStencilVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3DX11EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetDepthStencilState
    (
      Index:LongWord;
      out DepthStencilState:ID3D11DepthStencilState
    ):HResult; virtual; stdcall; abstract;

    function SetDepthStencilState
    (
      Index:LongWord;
      DepthStencilState:ID3D11DepthStencilState
    ):HResult; virtual; stdcall; abstract;

    function UndoSetDepthStencilState
    (
      Index:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetBackingStore
    (
      Index:LongWord;
      pDepthStencilDesc:PTD3D11_DepthStencilDesc
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3DX11EffectRasterizerVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3DX11EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRasterizerState
    (
      Index:LongWord;
      out RasterizerState:ID3D11RasterizerState
    ):HResult; virtual; stdcall; abstract;

    function SetRasterizerState
    (
      Index:LongWord;
      RasterizerState:ID3D11RasterizerState
    ):HResult; virtual; stdcall; abstract;

    function UndoSetRasterizerState
    (
      Index:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetBackingStore
    (
      Index:LongWord;
      pRasterizerDesc:PTD3D11_RasterizerDesc
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3DX11EffectSamplerVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3DX11EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_EffectVariableDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3DX11EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3DX11EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3DX11EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3DX11EffectStringVariable; virtual; stdcall; abstract;

    function AsClassInstance:ID3DX11EffectClassInstanceVariable; virtual; stdcall; abstract;

    function AsInterface:ID3DX11EffectInterfaceVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3DX11EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsUnorderedAccessView:ID3DX11EffectUnorderedAccessViewVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3DX11EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3DX11EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3DX11EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3DX11EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3DX11EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3DX11EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3DX11EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3DX11EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetSampler
    (
      Index:LongWord;
      out Sampler:ID3D11SamplerState
    ):HResult; virtual; stdcall; abstract;

    function SetSampler
    (
      Index:LongWord;
      Sampler:ID3D11SamplerState
    ):HResult; virtual; stdcall; abstract;

    function UndoSetSampler
    (
      Index:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetBackingStore
    (
      Index:LongWord;
      pSamplerDesc:PTD3D11_SamplerDesc
    ):HResult; virtual; stdcall; abstract;
  end;

  TD3DX11_PassDesc=record
    Name:PAnsiChar;
    Annotations:LongWord;
    pIAInputSignature:PByte;
    IAInputSignatureSize:SIZE_T;
    StencilRef:LongWord;
    SampleMask:LongWord;
    BlendFactor:array[0..3] of Single;
  end;
  PTD3DX11_PassDesc=^TD3DX11_PassDesc;
  D3DX11_PASS_DESC=TD3DX11_PassDesc;
  PD3DX11_PASS_DESC=^TD3DX11_PassDesc;

  TD3DX11_PassShaderDesc=record
    pShaderVariable:ID3DX11EffectShaderVariable;
    ShaderIndex:LongWord;
  end;
  PTD3DX11_PassShaderDesc=^TD3DX11_PassShaderDesc;
  D3DX11_PASS_SHADER_DESC=TD3DX11_PassShaderDesc;
  PD3DX11_PASS_SHADER_DESC=^TD3DX11_PassShaderDesc;

  ID3DX11EffectPass=class;
  PID3DX11EffectPass=^ID3DX11EffectPass;

  ID3DX11EffectPass=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_PassDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetVertexShaderDesc
    (
      out Desc:TD3DX11_PassShaderDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetGeometryShaderDesc
    (
      out Desc:TD3DX11_PassShaderDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetPixelShaderDesc
    (
      out Desc:TD3DX11_PassShaderDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetHullShaderDesc
    (
      out Desc:TD3DX11_PassShaderDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetDomainShaderDesc
    (
      out Desc:TD3DX11_PassShaderDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetComputeShaderDesc
    (
      out Desc:TD3DX11_PassShaderDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function Apply
    (
      Flags:LongWord;
      Context:ID3D11DeviceContext
    ):HResult; virtual; stdcall; abstract;

    function ComputeStateBlockMask
    (
      pStateBlockMask:PTD3DX11_StateBlockMask
    ):HResult; virtual; stdcall; abstract;
  end;

  TD3DX11_TechniqueDesc=record
    Name:PAnsiChar;
    Passes:LongWord;
    Annotations:LongWord;
  end;
  PTD3DX11_TechniqueDesc=^TD3DX11_TechniqueDesc;
  D3DX11_TECHNIQUE_DESC=TD3DX11_TechniqueDesc;
  PD3DX11_TECHNIQUE_DESC=^TD3DX11_TechniqueDesc;

  ID3DX11EffectTechnique=class;
  PID3DX11EffectTechnique=^ID3DX11EffectTechnique;

  ID3DX11EffectTechnique=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_TechniqueDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetPassByIndex
    (
      Index:LongWord
    ):ID3DX11EffectPass; virtual; stdcall; abstract;

    function GetPassByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectPass; virtual; stdcall; abstract;

    function ComputeStateBlockMask
    (
      pStateBlockMask:PTD3DX11_StateBlockMask
    ):HResult; virtual; stdcall; abstract;
  end;

  TD3DX11_GroupDesc=record
    Name:PAnsiChar;
    Techniques:LongWord;
    Annotations:LongWord;
  end;
  PTD3DX11_GroupDesc=^TD3DX11_GroupDesc;
  D3DX11_GROUP_DESC=TD3DX11_GroupDesc;
  PD3DX11_GROUP_DESC=^TD3DX11_GroupDesc;

  ID3DX11EffectGroup=class;
  PID3DX11EffectGroup=^ID3DX11EffectGroup;

  ID3DX11EffectGroup=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetDesc
    (
      out Desc:TD3DX11_GroupDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; virtual; stdcall; abstract;

    function GetTechniqueByIndex
    (
      Index:LongWord
    ):ID3DX11EffectTechnique; virtual; stdcall; abstract;

    function GetTechniqueByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectTechnique; virtual; stdcall; abstract;
  end;

  TD3DX11_EffectDesc=record
    ConstantBuffers:LongWord;
    GlobalVariables:LongWord;
    InterfaceVariables:LongWord;
    Techniques:LongWord;
    Groups:LongWord;
  end;
  PTD3DX11_EffectDesc=^TD3DX11_EffectDesc;
  D3DX11_EFFECT_DESC=TD3DX11_EffectDesc;
  PD3DX11_EFFECT_DESC=^TD3DX11_EffectDesc;

  ID3DX11Effect=interface;
  PID3DX11Effect=^ID3DX11Effect;

  ID3DX11Effect=interface
    ['{FA61CA24-E4BA-4262-9DB8-B132E8CAE319}']
    function IsValid:LongBool; stdcall;

    function GetDevice
    (
      out Device:ID3D11Device
    ):HResult; stdcall;

    function GetDesc
    (
      out Desc:TD3DX11_EffectDesc (* __out *)
    ):HResult; stdcall;

    function GetConstantBufferByIndex
    (
      Index:LongWord
    ):ID3DX11EffectConstantBuffer; stdcall;

    function GetConstantBufferByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectConstantBuffer; stdcall;

    function GetVariableByIndex
    (
      Index:LongWord
    ):ID3DX11EffectVariable; stdcall;

    function GetVariableByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectVariable; stdcall;

    function GetVariableBySemantic
    (
      Semantic:PAnsiChar
    ):ID3DX11EffectVariable; stdcall;

    function GetGroupByIndex
    (
      Index:LongWord
    ):ID3DX11EffectGroup; stdcall;

    function GetGroupByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectGroup; stdcall;

    function GetTechniqueByIndex
    (
      Index:LongWord
    ):ID3DX11EffectTechnique; stdcall;

    function GetTechniqueByName
    (
      Name:PAnsiChar
    ):ID3DX11EffectTechnique; stdcall;

    function GetClassLinkage:ID3D11ClassLinkage; stdcall;

    function CloneEffect
    (
      Flags:LongWord;
      out ClonedEffect:ID3DX11Effect
    ):HResult; stdcall;

    function Optimize:HResult; stdcall;

    function IsOptimized:LongBool; stdcall;
  end;

// Requires d3dx11Effects_JSB.dll in System32 folder or other accessible location.
{$IFDEF UseRuntimeLinking}var D3DX11CreateEffectFromMemory:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3DX11CreateEffectFromMemory{$ENDIF}(pData:Pointer;DataLength:SIZE_T;FXFlags:LongWord;Device:ID3D11Device;out Effect:ID3DX11Effect):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_Effects11; {$ENDIF}

///////////////////////////////////////////////////////////////////////////////
// End "D3DX11Effect.h"
///////////////////////////////////////////////////////////////////////////////

{$IFDEF UseRuntimeLinking}
procedure Link;
{$ENDIF}

implementation

{$IFDEF UseJSBErrors}
function HResultToString(Value:HRESULT):String;
begin
  Result:='';
  if SUCCEEDED(Value) then Exit;

  case Value of
    D3D_ERROR_INVALIDCALL: Result:='Invalid call.';
    D3D_ERROR_WASSTILLDRAWING: Result:='Was still drawing.';
    //
    D3DX11_ERROR_CANNOT_MODIFY_INDEX_BUFFER: Result:='D3DX11: Cannot modify index buffer.';
    D3DX11_ERROR_INVALID_MESH: Result:='D3DX11: Invalid mesh.';
    D3DX11_ERROR_CANNOT_ATTR_SORT: Result:='D3DX11: Atribute sort is not supported.';
    D3DX11_ERROR_SKINNING_NOT_SUPPORTED: Result:='D3DX11: Skinning is not supported.';
    D3DX11_ERROR_TOO_MANY_INFLUENCES: Result:='D3DX11: Too many influences specified.';
    D3DX11_ERROR_INVALID_DATA: Result:='D3DX11: Invalid data.';
    D3DX11_ERROR_LOADED_MESH_HAS_NO_DATA: Result:='D3DX11: Loaded mesh has no data.';
    D3DX11_ERROR_DUPLICATE_NAMED_FRAGMENT: Result:='D3DX11: A fragment with that name already exists.';
    D3DX11_ERROR_CANNOT_REMOVE_LAST_ITEM: Result:='D3DX11: Cannot remove last item.';
  end;
end;
{$ENDIF}

{$IFDEF UseRuntimeLinking}
function LoadDLL(DLLName:String):HModule;
begin
  Result:=LoadLibrary(PChar(DLLName));
  if Result=0 then
    raise Exception.Create('Dynamic link library (DLL) '''+DLLName+''' is not available.');
end;

function LinkMethod(hDLL:HModule;MethodName,DLLName:String):Pointer;
begin
  Result:=GetProcAddress(hDLL,PChar(MethodName));
  if Result=nil then
    raise Exception.Create('Failed to link to method '''+MethodName+''' in dynamic link library (DLL) '''+DLLName+'''.');
end;

procedure Link;
var
  hDLL_D3DX11:HModule;
  hDLL_Effects11:HModule;
begin
  hDLL_D3DX11:=LoadDLL(DLL_D3DX11);

  // Convert the exception type to enable a special message if the Effects11 library is missing.
  try
    hDLL_Effects11:=LoadDLL(DLL_Effects11);
  except
    if ExceptObject is Exception then
      raise E_Effects11.Create(Exception(ExceptObject).Message)
    else
      raise;
  end;

{$IFDEF D3D_DIAG_DLL}

  D3DX11DebugMute:=LinkMethod(hDLL_D3DX11,'D3DX11DebugMute',DLL_D3DX11);

{$ENDIF}

  D3DX11CheckVersion:=LinkMethod(hDLL_D3DX11,'D3DX11CheckVersion',DLL_D3DX11);
  D3DX11CreateThreadPump:=LinkMethod(hDLL_D3DX11,'D3DX11CreateThreadPump',DLL_D3DX11);
// JSB: Not in DLL_D3DX11. Might be implemented in C++ D3DX11.lib library: D3DX11UnsetAllDeviceObjects:=LinkMethod(hDLL_D3DX11,'D3DX11UnsetAllDeviceObjects',DLL_D3DX11);
  D3DX11GetImageInfoFromFileA:=LinkMethod(hDLL_D3DX11,'D3DX11GetImageInfoFromFileA',DLL_D3DX11);
  D3DX11GetImageInfoFromFileW:=LinkMethod(hDLL_D3DX11,'D3DX11GetImageInfoFromFileW',DLL_D3DX11);

{$IFDEF UNICODE}

  D3DX11GetImageInfoFromFile:=LinkMethod(hDLL_D3DX11,'D3DX11GetImageInfoFromFileW',DLL_D3DX11);

{$ELSE}

  D3DX11GetImageInfoFromFile:=LinkMethod(hDLL_D3DX11,'D3DX11GetImageInfoFromFileA',DLL_D3DX11);

{$ENDIF}

  D3DX11GetImageInfoFromResourceA:=LinkMethod(hDLL_D3DX11,'D3DX11GetImageInfoFromResourceA',DLL_D3DX11);
  D3DX11GetImageInfoFromResourceW:=LinkMethod(hDLL_D3DX11,'D3DX11GetImageInfoFromResourceW',DLL_D3DX11);

{$IFDEF UNICODE}

  D3DX11GetImageInfoFromResource:=LinkMethod(hDLL_D3DX11,'D3DX11GetImageInfoFromResourceW',DLL_D3DX11);

{$ELSE}

  D3DX11GetImageInfoFromResource:=LinkMethod(hDLL_D3DX11,'D3DX11GetImageInfoFromResourceA',DLL_D3DX11);

{$ENDIF}

  D3DX11GetImageInfoFromMemory:=LinkMethod(hDLL_D3DX11,'D3DX11GetImageInfoFromMemory',DLL_D3DX11);
  D3DX11CreateShaderResourceViewFromFileA:=LinkMethod(hDLL_D3DX11,'D3DX11CreateShaderResourceViewFromFileA',DLL_D3DX11);
  D3DX11CreateShaderResourceViewFromFileW:=LinkMethod(hDLL_D3DX11,'D3DX11CreateShaderResourceViewFromFileW',DLL_D3DX11);

{$IFDEF UNICODE}

  D3DX11CreateShaderResourceViewFromFile:=LinkMethod(hDLL_D3DX11,'D3DX11CreateShaderResourceViewFromFileW',DLL_D3DX11);

{$ELSE}

  D3DX11CreateShaderResourceViewFromFile:=LinkMethod(hDLL_D3DX11,'D3DX11CreateShaderResourceViewFromFileA',DLL_D3DX11);

{$ENDIF}

  D3DX11CreateTextureFromFileA:=LinkMethod(hDLL_D3DX11,'D3DX11CreateTextureFromFileA',DLL_D3DX11);
  D3DX11CreateTextureFromFileW:=LinkMethod(hDLL_D3DX11,'D3DX11CreateTextureFromFileW',DLL_D3DX11);

{$IFDEF UNICODE}

  D3DX11CreateTextureFromFile:=LinkMethod(hDLL_D3DX11,'D3DX11CreateTextureFromFileW',DLL_D3DX11);

{$ELSE}

  D3DX11CreateTextureFromFile:=LinkMethod(hDLL_D3DX11,'D3DX11CreateTextureFromFileA',DLL_D3DX11);

{$ENDIF}

  D3DX11CreateShaderResourceViewFromResourceA:=LinkMethod(hDLL_D3DX11,'D3DX11CreateShaderResourceViewFromResourceA',DLL_D3DX11);
  D3DX11CreateShaderResourceViewFromResourceW:=LinkMethod(hDLL_D3DX11,'D3DX11CreateShaderResourceViewFromResourceW',DLL_D3DX11);

{$IFDEF UNICODE}

  D3DX11CreateShaderResourceViewFromResource:=LinkMethod(hDLL_D3DX11,'D3DX11CreateShaderResourceViewFromResourceW',DLL_D3DX11);

{$ELSE}

  D3DX11CreateShaderResourceViewFromResource:=LinkMethod(hDLL_D3DX11,'D3DX11CreateShaderResourceViewFromResourceA',DLL_D3DX11);

{$ENDIF}

  D3DX11CreateTextureFromResourceA:=LinkMethod(hDLL_D3DX11,'D3DX11CreateTextureFromResourceA',DLL_D3DX11);
  D3DX11CreateTextureFromResourceW:=LinkMethod(hDLL_D3DX11,'D3DX11CreateTextureFromResourceW',DLL_D3DX11);

{$IFDEF UNICODE}

  D3DX11CreateTextureFromResource:=LinkMethod(hDLL_D3DX11,'D3DX11CreateTextureFromResourceW',DLL_D3DX11);

{$ELSE}

  D3DX11CreateTextureFromResource:=LinkMethod(hDLL_D3DX11,'D3DX11CreateTextureFromResourceA',DLL_D3DX11);

{$ENDIF}

  D3DX11CreateShaderResourceViewFromMemory:=LinkMethod(hDLL_D3DX11,'D3DX11CreateShaderResourceViewFromMemory',DLL_D3DX11);
  D3DX11CreateTextureFromMemory:=LinkMethod(hDLL_D3DX11,'D3DX11CreateTextureFromMemory',DLL_D3DX11);
  D3DX11LoadTextureFromTexture:=LinkMethod(hDLL_D3DX11,'D3DX11LoadTextureFromTexture',DLL_D3DX11);
  D3DX11FilterTexture:=LinkMethod(hDLL_D3DX11,'D3DX11FilterTexture',DLL_D3DX11);
  D3DX11SaveTextureToFileA:=LinkMethod(hDLL_D3DX11,'D3DX11SaveTextureToFileA',DLL_D3DX11);
  D3DX11SaveTextureToFileW:=LinkMethod(hDLL_D3DX11,'D3DX11SaveTextureToFileW',DLL_D3DX11);

{$IFDEF UNICODE}

  D3DX11SaveTextureToFile:=LinkMethod(hDLL_D3DX11,'D3DX11SaveTextureToFileW',DLL_D3DX11);

{$ELSE}

  D3DX11SaveTextureToFile:=LinkMethod(hDLL_D3DX11,'D3DX11SaveTextureToFileA',DLL_D3DX11);

{$ENDIF}

  D3DX11SaveTextureToMemory:=LinkMethod(hDLL_D3DX11,'D3DX11SaveTextureToMemory',DLL_D3DX11);
  D3DX11ComputeNormalMap:=LinkMethod(hDLL_D3DX11,'D3DX11ComputeNormalMap',DLL_D3DX11);
  D3DX11SHProjectCubeMap:=LinkMethod(hDLL_D3DX11,'D3DX11SHProjectCubeMap',DLL_D3DX11);
  D3DX11CompileFromFileA:=LinkMethod(hDLL_D3DX11,'D3DX11CompileFromFileA',DLL_D3DX11);
  D3DX11CompileFromFileW:=LinkMethod(hDLL_D3DX11,'D3DX11CompileFromFileW',DLL_D3DX11);

{$IFDEF UNICODE}

  D3DX11CompileFromFile:=LinkMethod(hDLL_D3DX11,'D3DX11CompileFromFileW',DLL_D3DX11);

{$ELSE}

  D3DX11CompileFromFile:=LinkMethod(hDLL_D3DX11,'D3DX11CompileFromFileA',DLL_D3DX11);

{$ENDIF}

  D3DX11CompileFromResourceA:=LinkMethod(hDLL_D3DX11,'D3DX11CompileFromResourceA',DLL_D3DX11);
  D3DX11CompileFromResourceW:=LinkMethod(hDLL_D3DX11,'D3DX11CompileFromResourceW',DLL_D3DX11);

{$IFDEF UNICODE}

  D3DX11CompileFromResource:=LinkMethod(hDLL_D3DX11,'D3DX11CompileFromResourceW',DLL_D3DX11);

{$ELSE}

  D3DX11CompileFromResource:=LinkMethod(hDLL_D3DX11,'D3DX11CompileFromResourceA',DLL_D3DX11);

{$ENDIF}

  D3DX11CompileFromMemory:=LinkMethod(hDLL_D3DX11,'D3DX11CompileFromMemory',DLL_D3DX11);
  D3DX11PreprocessShaderFromFileA:=LinkMethod(hDLL_D3DX11,'D3DX11PreprocessShaderFromFileA',DLL_D3DX11);
  D3DX11PreprocessShaderFromFileW:=LinkMethod(hDLL_D3DX11,'D3DX11PreprocessShaderFromFileW',DLL_D3DX11);
  D3DX11PreprocessShaderFromMemory:=LinkMethod(hDLL_D3DX11,'D3DX11PreprocessShaderFromMemory',DLL_D3DX11);
  D3DX11PreprocessShaderFromResourceA:=LinkMethod(hDLL_D3DX11,'D3DX11PreprocessShaderFromResourceA',DLL_D3DX11);
  D3DX11PreprocessShaderFromResourceW:=LinkMethod(hDLL_D3DX11,'D3DX11PreprocessShaderFromResourceW',DLL_D3DX11);

{$IFDEF UNICODE}

  D3DX11PreprocessShaderFromFile:=LinkMethod(hDLL_D3DX11,'D3DX11PreprocessShaderFromFileW',DLL_D3DX11);
  D3DX11PreprocessShaderFromResource:=LinkMethod(hDLL_D3DX11,'D3DX11PreprocessShaderFromResourceW',DLL_D3DX11);

{$ELSE}

  D3DX11PreprocessShaderFromFile:=LinkMethod(hDLL_D3DX11,'D3DX11PreprocessShaderFromFileA',DLL_D3DX11);
  D3DX11PreprocessShaderFromResource:=LinkMethod(hDLL_D3DX11,'D3DX11PreprocessShaderFromResourceA',DLL_D3DX11);

{$ENDIF}

  D3DX11CreateAsyncCompilerProcessor:=LinkMethod(hDLL_D3DX11,'D3DX11CreateAsyncCompilerProcessor',DLL_D3DX11);
  D3DX11CreateAsyncShaderPreprocessProcessor:=LinkMethod(hDLL_D3DX11,'D3DX11CreateAsyncShaderPreprocessProcessor',DLL_D3DX11);
  D3DX11CreateAsyncFileLoaderW:=LinkMethod(hDLL_D3DX11,'D3DX11CreateAsyncFileLoaderW',DLL_D3DX11);
  D3DX11CreateAsyncFileLoaderA:=LinkMethod(hDLL_D3DX11,'D3DX11CreateAsyncFileLoaderA',DLL_D3DX11);
  D3DX11CreateAsyncMemoryLoader:=LinkMethod(hDLL_D3DX11,'D3DX11CreateAsyncMemoryLoader',DLL_D3DX11);
  D3DX11CreateAsyncResourceLoaderW:=LinkMethod(hDLL_D3DX11,'D3DX11CreateAsyncResourceLoaderW',DLL_D3DX11);
  D3DX11CreateAsyncResourceLoaderA:=LinkMethod(hDLL_D3DX11,'D3DX11CreateAsyncResourceLoaderA',DLL_D3DX11);

{$IFDEF UNICODE}

  D3DX11CreateAsyncFileLoader:=LinkMethod(hDLL_D3DX11,'D3DX11CreateAsyncFileLoaderW',DLL_D3DX11);
  D3DX11CreateAsyncResourceLoader:=LinkMethod(hDLL_D3DX11,'D3DX11CreateAsyncResourceLoaderW',DLL_D3DX11);

{$ELSE}

  D3DX11CreateAsyncFileLoader:=LinkMethod(hDLL_D3DX11,'D3DX11CreateAsyncFileLoaderA',DLL_D3DX11);
  D3DX11CreateAsyncResourceLoader:=LinkMethod(hDLL_D3DX11,'D3DX11CreateAsyncResourceLoaderA',DLL_D3DX11);

{$ENDIF}

  D3DX11CreateAsyncTextureProcessor:=LinkMethod(hDLL_D3DX11,'D3DX11CreateAsyncTextureProcessor',DLL_D3DX11);
  D3DX11CreateAsyncTextureInfoProcessor:=LinkMethod(hDLL_D3DX11,'D3DX11CreateAsyncTextureInfoProcessor',DLL_D3DX11);
  D3DX11CreateAsyncShaderResourceViewProcessor:=LinkMethod(hDLL_D3DX11,'D3DX11CreateAsyncShaderResourceViewProcessor',DLL_D3DX11);

  D3DX11CreateEffectFromMemory:=LinkMethod(hDLL_Effects11,'D3DX11CreateEffectFromMemory',DLL_Effects11);
end;
{$ENDIF}

{$IFDEF UseJSBErrors}
initialization
begin
  AddDirectXHResultToStringHandler(HResultToString);
end;
{$ENDIF}

end.
