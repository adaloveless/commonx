unit D3D10_JSB;

///////////////////////////////////////////////////////////////////////////////
// Title: Translation of DirectX C++ header files for use with Delphi 2009 and later
//
// File name: D3D10_JSB.pas
//
// Originator: J S Bladen, Sheffield, UK.
//
// Copyright: J S Bladen, Sheffield, UK.
//
// Translation date and time (UTC): 07/10/2010 15:12:18
//
// Email: DirectXForDelphi@jsbmedical.co.uk
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Original file(s):
//   D3D10.h
//   D3D10Misc.h
//   D3D10Shader.h
//   D3D10Effect.h
//   D3D10SDKLayers.h
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
  Windows, SysUtils, {$IFDEF UseJSBErrors} UDirectXErrors, {$ENDIF} DXTypes_JSB, DXGI_JSB, D3DCommon_JSB;

const
  DLL_D3D10='d3d10.dll';

///////////////////////////////////////////////////////////////////////////////
// Begin "D3D10.h"
///////////////////////////////////////////////////////////////////////////////

const
  D3D10_16BIT_INDEX_STRIP_CUT_VALUE=$ffff;
  D3D10_32BIT_INDEX_STRIP_CUT_VALUE=$ffffffff;
  D3D10_8BIT_INDEX_STRIP_CUT_VALUE=$ff;
  D3D10_ARRAY_AXIS_ADDRESS_RANGE_BIT_COUNT=9;
  D3D10_CLIP_OR_CULL_DISTANCE_COUNT=8;
  D3D10_CLIP_OR_CULL_DISTANCE_ELEMENT_COUNT=2;
  D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT=14;
  D3D10_COMMONSHADER_CONSTANT_BUFFER_COMPONENTS=4;
  D3D10_COMMONSHADER_CONSTANT_BUFFER_COMPONENT_BIT_COUNT=32;
  D3D10_COMMONSHADER_CONSTANT_BUFFER_HW_SLOT_COUNT=15;
  D3D10_COMMONSHADER_CONSTANT_BUFFER_REGISTER_COMPONENTS=4;
  D3D10_COMMONSHADER_CONSTANT_BUFFER_REGISTER_COUNT=15;
  D3D10_COMMONSHADER_CONSTANT_BUFFER_REGISTER_READS_PER_INST=1;
  D3D10_COMMONSHADER_CONSTANT_BUFFER_REGISTER_READ_PORTS=1;
  D3D10_COMMONSHADER_FLOWCONTROL_NESTING_LIMIT=64;
  D3D10_COMMONSHADER_IMMEDIATE_CONSTANT_BUFFER_REGISTER_COMPONENTS=4;
  D3D10_COMMONSHADER_IMMEDIATE_CONSTANT_BUFFER_REGISTER_COUNT=1;
  D3D10_COMMONSHADER_IMMEDIATE_CONSTANT_BUFFER_REGISTER_READS_PER_INST=1;
  D3D10_COMMONSHADER_IMMEDIATE_CONSTANT_BUFFER_REGISTER_READ_PORTS=1;
  D3D10_COMMONSHADER_IMMEDIATE_VALUE_COMPONENT_BIT_COUNT=32;
  D3D10_COMMONSHADER_INPUT_RESOURCE_REGISTER_COMPONENTS=1;
  D3D10_COMMONSHADER_INPUT_RESOURCE_REGISTER_COUNT=128;
  D3D10_COMMONSHADER_INPUT_RESOURCE_REGISTER_READS_PER_INST=1;
  D3D10_COMMONSHADER_INPUT_RESOURCE_REGISTER_READ_PORTS=1;
  D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT=128;
  D3D10_COMMONSHADER_SAMPLER_REGISTER_COMPONENTS=1;
  D3D10_COMMONSHADER_SAMPLER_REGISTER_COUNT=16;
  D3D10_COMMONSHADER_SAMPLER_REGISTER_READS_PER_INST=1;
  D3D10_COMMONSHADER_SAMPLER_REGISTER_READ_PORTS=1;
  D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT=16;
  D3D10_COMMONSHADER_SUBROUTINE_NESTING_LIMIT=32;
  D3D10_COMMONSHADER_TEMP_REGISTER_COMPONENTS=4;
  D3D10_COMMONSHADER_TEMP_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_COMMONSHADER_TEMP_REGISTER_COUNT=4096;
  D3D10_COMMONSHADER_TEMP_REGISTER_READS_PER_INST=3;
  D3D10_COMMONSHADER_TEMP_REGISTER_READ_PORTS=3;
  D3D10_COMMONSHADER_TEXCOORD_RANGE_REDUCTION_MAX=10;
  D3D10_COMMONSHADER_TEXCOORD_RANGE_REDUCTION_MIN=-10;
  D3D10_COMMONSHADER_TEXEL_OFFSET_MAX_NEGATIVE=-8;
  D3D10_COMMONSHADER_TEXEL_OFFSET_MAX_POSITIVE=7;
  D3D10_DEFAULT_BLEND_FACTOR_ALPHA=1.0;
  D3D10_DEFAULT_BLEND_FACTOR_BLUE=1.0;
  D3D10_DEFAULT_BLEND_FACTOR_GREEN=1.0;
  D3D10_DEFAULT_BLEND_FACTOR_RED=1.0;
  D3D10_DEFAULT_BORDER_COLOR_COMPONENT=0.0;
  D3D10_DEFAULT_DEPTH_BIAS=0;
  D3D10_DEFAULT_DEPTH_BIAS_CLAMP=0.0;
  D3D10_DEFAULT_MAX_ANISOTROPY=16.0;
  D3D10_DEFAULT_MIP_LOD_BIAS=0.0;
  D3D10_DEFAULT_RENDER_TARGET_ARRAY_INDEX=0;
  D3D10_DEFAULT_SAMPLE_MASK=$ffffffff;
  D3D10_DEFAULT_SCISSOR_ENDX=0;
  D3D10_DEFAULT_SCISSOR_ENDY=0;
  D3D10_DEFAULT_SCISSOR_STARTX=0;
  D3D10_DEFAULT_SCISSOR_STARTY=0;
  D3D10_DEFAULT_SLOPE_SCALED_DEPTH_BIAS=0.0;
  D3D10_DEFAULT_STENCIL_READ_MASK=$ff;
  D3D10_DEFAULT_STENCIL_REFERENCE=0;
  D3D10_DEFAULT_STENCIL_WRITE_MASK=$ff;
  D3D10_DEFAULT_VIEWPORT_AND_SCISSORRECT_INDEX=0;
  D3D10_DEFAULT_VIEWPORT_HEIGHT=0;
  D3D10_DEFAULT_VIEWPORT_MAX_DEPTH=0.0;
  D3D10_DEFAULT_VIEWPORT_MIN_DEPTH=0.0;
  D3D10_DEFAULT_VIEWPORT_TOPLEFTX=0;
  D3D10_DEFAULT_VIEWPORT_TOPLEFTY=0;
  D3D10_DEFAULT_VIEWPORT_WIDTH=0;
  D3D10_FLOAT16_FUSED_TOLERANCE_IN_ULP=0.6;
  D3D10_FLOAT32_MAX=3.402823466e+38;
  D3D10_FLOAT32_TO_INTEGER_TOLERANCE_IN_ULP=0.6;
  D3D10_FLOAT_TO_SRGB_EXPONENT_DENOMINATOR=2.4;
  D3D10_FLOAT_TO_SRGB_EXPONENT_NUMERATOR=1.0;
  D3D10_FLOAT_TO_SRGB_OFFSET=0.055;
  D3D10_FLOAT_TO_SRGB_SCALE_1=12.92;
  D3D10_FLOAT_TO_SRGB_SCALE_2=1.055;
  D3D10_FLOAT_TO_SRGB_THRESHOLD=0.0031308;
  D3D10_FTOI_INSTRUCTION_MAX_INPUT=2147483647.999;
  D3D10_FTOI_INSTRUCTION_MIN_INPUT=-2147483648.999;
  D3D10_FTOU_INSTRUCTION_MAX_INPUT=4294967295.999;
  D3D10_FTOU_INSTRUCTION_MIN_INPUT=0.0;
  D3D10_GS_INPUT_PRIM_CONST_REGISTER_COMPONENTS=1;
  D3D10_GS_INPUT_PRIM_CONST_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_GS_INPUT_PRIM_CONST_REGISTER_COUNT=1;
  D3D10_GS_INPUT_PRIM_CONST_REGISTER_READS_PER_INST=2;
  D3D10_GS_INPUT_PRIM_CONST_REGISTER_READ_PORTS=1;
  D3D10_GS_INPUT_REGISTER_COMPONENTS=4;
  D3D10_GS_INPUT_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_GS_INPUT_REGISTER_COUNT=16;
  D3D10_GS_INPUT_REGISTER_READS_PER_INST=2;
  D3D10_GS_INPUT_REGISTER_READ_PORTS=1;
  D3D10_GS_INPUT_REGISTER_VERTICES=6;
  D3D10_GS_OUTPUT_ELEMENTS=32;
  D3D10_GS_OUTPUT_REGISTER_COMPONENTS=4;
  D3D10_GS_OUTPUT_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_GS_OUTPUT_REGISTER_COUNT=32;
  D3D10_IA_DEFAULT_INDEX_BUFFER_OFFSET_IN_BYTES=0;
  D3D10_IA_DEFAULT_PRIMITIVE_TOPOLOGY=0;
  D3D10_IA_DEFAULT_VERTEX_BUFFER_OFFSET_IN_BYTES=0;
  D3D10_IA_INDEX_INPUT_RESOURCE_SLOT_COUNT=1;
  D3D10_IA_INSTANCE_ID_BIT_COUNT=32;
  D3D10_IA_INTEGER_ARITHMETIC_BIT_COUNT=32;
  D3D10_IA_PRIMITIVE_ID_BIT_COUNT=32;
  D3D10_IA_VERTEX_ID_BIT_COUNT=32;
  D3D10_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT=16;
  D3D10_IA_VERTEX_INPUT_STRUCTURE_ELEMENTS_COMPONENTS=64;
  D3D10_IA_VERTEX_INPUT_STRUCTURE_ELEMENT_COUNT=16;
  D3D10_INTEGER_DIVIDE_BY_ZERO_QUOTIENT=$ffffffff;
  D3D10_INTEGER_DIVIDE_BY_ZERO_REMAINDER=$ffffffff;
  D3D10_LINEAR_GAMMA=1.0;
  D3D10_MAX_BORDER_COLOR_COMPONENT=1.0;
  D3D10_MAX_DEPTH=1.0;
  D3D10_MAX_MAXANISOTROPY=16;
  D3D10_MAX_MULTISAMPLE_SAMPLE_COUNT=32;
  D3D10_MAX_POSITION_VALUE=3.402823466e+34;
  D3D10_MAX_TEXTURE_DIMENSION_2_TO_EXP=17;
  D3D10_MIN_BORDER_COLOR_COMPONENT=0.0;
  D3D10_MIN_DEPTH=0.0;
  D3D10_MIN_MAXANISOTROPY=0;
  D3D10_MIP_LOD_BIAS_MAX=15.99;
  D3D10_MIP_LOD_BIAS_MIN=-16.0;
  D3D10_MIP_LOD_FRACTIONAL_BIT_COUNT=6;
  D3D10_MIP_LOD_RANGE_BIT_COUNT=8;
  D3D10_MULTISAMPLE_ANTIALIAS_LINE_WIDTH=1.4;
  D3D10_NONSAMPLE_FETCH_OUT_OF_RANGE_ACCESS_RESULT=0;
  D3D10_PIXEL_ADDRESS_RANGE_BIT_COUNT=13;
  D3D10_PRE_SCISSOR_PIXEL_ADDRESS_RANGE_BIT_COUNT=15;
  D3D10_PS_FRONTFACING_DEFAULT_VALUE=$ffffffff;
  D3D10_PS_FRONTFACING_FALSE_VALUE=0;
  D3D10_PS_FRONTFACING_TRUE_VALUE=$ffffffff;
  D3D10_PS_INPUT_REGISTER_COMPONENTS=4;
  D3D10_PS_INPUT_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_PS_INPUT_REGISTER_COUNT=32;
  D3D10_PS_INPUT_REGISTER_READS_PER_INST=2;
  D3D10_PS_INPUT_REGISTER_READ_PORTS=1;
  D3D10_PS_LEGACY_PIXEL_CENTER_FRACTIONAL_COMPONENT=0.0;
  D3D10_PS_OUTPUT_DEPTH_REGISTER_COMPONENTS=1;
  D3D10_PS_OUTPUT_DEPTH_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_PS_OUTPUT_DEPTH_REGISTER_COUNT=1;
  D3D10_PS_OUTPUT_REGISTER_COMPONENTS=4;
  D3D10_PS_OUTPUT_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_PS_OUTPUT_REGISTER_COUNT=8;
  D3D10_PS_PIXEL_CENTER_FRACTIONAL_COMPONENT=0.5;
  D3D10_REQ_BLEND_OBJECT_COUNT_PER_CONTEXT=4096;
  D3D10_REQ_BUFFER_RESOURCE_TEXEL_COUNT_2_TO_EXP=27;
  D3D10_REQ_CONSTANT_BUFFER_ELEMENT_COUNT=4096;
  D3D10_REQ_DEPTH_STENCIL_OBJECT_COUNT_PER_CONTEXT=4096;
  D3D10_REQ_DRAWINDEXED_INDEX_COUNT_2_TO_EXP=32;
  D3D10_REQ_DRAW_VERTEX_COUNT_2_TO_EXP=32;
  D3D10_REQ_FILTERING_HW_ADDRESSABLE_RESOURCE_DIMENSION=8192;
  D3D10_REQ_GS_INVOCATION_32BIT_OUTPUT_COMPONENT_LIMIT=1024;
  D3D10_REQ_IMMEDIATE_CONSTANT_BUFFER_ELEMENT_COUNT=4096;
  D3D10_REQ_MAXANISOTROPY=16;
  D3D10_REQ_MIP_LEVELS=14;
  D3D10_REQ_MULTI_ELEMENT_STRUCTURE_SIZE_IN_BYTES=2048;
  D3D10_REQ_RASTERIZER_OBJECT_COUNT_PER_CONTEXT=4096;
  D3D10_REQ_RENDER_TO_BUFFER_WINDOW_WIDTH=8192;
  D3D10_REQ_RESOURCE_SIZE_IN_MEGABYTES=128;
  D3D10_REQ_RESOURCE_VIEW_COUNT_PER_CONTEXT_2_TO_EXP=20;
  D3D10_REQ_SAMPLER_OBJECT_COUNT_PER_CONTEXT=4096;
  D3D10_REQ_TEXTURE1D_ARRAY_AXIS_DIMENSION=512;
  D3D10_REQ_TEXTURE1D_U_DIMENSION=8192;
  D3D10_REQ_TEXTURE2D_ARRAY_AXIS_DIMENSION=512;
  D3D10_REQ_TEXTURE2D_U_OR_V_DIMENSION=8192;
  D3D10_REQ_TEXTURE3D_U_V_OR_W_DIMENSION=2048;
  D3D10_REQ_TEXTURECUBE_DIMENSION=8192;
  D3D10_RESINFO_INSTRUCTION_MISSING_COMPONENT_RETVAL=0;
  D3D10_SHADER_MAJOR_VERSION=4;
  D3D10_SHADER_MINOR_VERSION=0;
  D3D10_SHIFT_INSTRUCTION_PAD_VALUE=0;
  D3D10_SHIFT_INSTRUCTION_SHIFT_VALUE_BIT_COUNT=5;
  D3D10_SIMULTANEOUS_RENDER_TARGET_COUNT=8;
  D3D10_SO_BUFFER_MAX_STRIDE_IN_BYTES=2048;
  D3D10_SO_BUFFER_MAX_WRITE_WINDOW_IN_BYTES=256;
  D3D10_SO_BUFFER_SLOT_COUNT=4;
  D3D10_SO_DDI_REGISTER_INDEX_DENOTING_GAP=$ffffffff;
  D3D10_SO_MULTIPLE_BUFFER_ELEMENTS_PER_BUFFER=1;
  D3D10_SO_SINGLE_BUFFER_COMPONENT_LIMIT=64;
  D3D10_SRGB_GAMMA=2.2;
  D3D10_SRGB_TO_FLOAT_DENOMINATOR_1=12.92;
  D3D10_SRGB_TO_FLOAT_DENOMINATOR_2=1.055;
  D3D10_SRGB_TO_FLOAT_EXPONENT=2.4;
  D3D10_SRGB_TO_FLOAT_OFFSET=0.055;
  D3D10_SRGB_TO_FLOAT_THRESHOLD=0.04045;
  D3D10_SRGB_TO_FLOAT_TOLERANCE_IN_ULP=0.5;
  D3D10_STANDARD_COMPONENT_BIT_COUNT=32;
  D3D10_STANDARD_COMPONENT_BIT_COUNT_DOUBLED=64;
  D3D10_STANDARD_MAXIMUM_ELEMENT_ALIGNMENT_BYTE_MULTIPLE=4;
  D3D10_STANDARD_PIXEL_COMPONENT_COUNT=128;
  D3D10_STANDARD_PIXEL_ELEMENT_COUNT=32;
  D3D10_STANDARD_VECTOR_SIZE=4;
  D3D10_STANDARD_VERTEX_ELEMENT_COUNT=16;
  D3D10_STANDARD_VERTEX_TOTAL_COMPONENT_COUNT=64;
  D3D10_SUBPIXEL_FRACTIONAL_BIT_COUNT=8;
  D3D10_SUBTEXEL_FRACTIONAL_BIT_COUNT=6;
  D3D10_TEXEL_ADDRESS_RANGE_BIT_COUNT=18;
  D3D10_UNBOUND_MEMORY_ACCESS_RESULT=0;
  D3D10_VIEWPORT_AND_SCISSORRECT_MAX_INDEX=15;
  D3D10_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE=16;
  D3D10_VIEWPORT_BOUNDS_MAX=16383;
  D3D10_VIEWPORT_BOUNDS_MIN=-16384;
  D3D10_VS_INPUT_REGISTER_COMPONENTS=4;
  D3D10_VS_INPUT_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_VS_INPUT_REGISTER_COUNT=16;
  D3D10_VS_INPUT_REGISTER_READS_PER_INST=2;
  D3D10_VS_INPUT_REGISTER_READ_PORTS=1;
  D3D10_VS_OUTPUT_REGISTER_COMPONENTS=4;
  D3D10_VS_OUTPUT_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_VS_OUTPUT_REGISTER_COUNT=16;
  D3D10_WHQL_CONTEXT_COUNT_FOR_RESOURCE_LIMIT=10;
  D3D10_WHQL_DRAWINDEXED_INDEX_COUNT_2_TO_EXP=25;
  D3D10_WHQL_DRAW_VERTEX_COUNT_2_TO_EXP=25;
  D3D_MAJOR_VERSION=10;
  D3D_MINOR_VERSION=0;
  D3D_SPEC_DATE_DAY=8;
  D3D_SPEC_DATE_MONTH=8;
  D3D_SPEC_DATE_YEAR=2006;
  D3D_SPEC_VERSION=1.050005;

{$IF !defined( __d3d10_1_h__ ) and !(D3D10_HEADER_MINOR_VERSION >= 1)}

const
  D3D10_1_IA_VERTEX_INPUT_STRUCTURE_ELEMENT_COUNT=D3D10_IA_VERTEX_INPUT_STRUCTURE_ELEMENT_COUNT;
  D3D10_1_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT=D3D10_IA_VERTEX_INPUT_STRUCTURE_ELEMENT_COUNT;

{$IFEND}

const
  _FACD3D10=$879;
  _FACD3D10DEBUG=_FACD3D10+1;
  D3D10_STATUS_Base=LongWord(_FACD3D10 shl 16);
  D3D10_HRESULT_Base=D3D10_STATUS_Base or LongWord(1 shl 31);
  //
  D3D10_ERROR_TOO_MANY_UNIQUE_STATE_OBJECTS=HResult(D3D10_HRESULT_Base or 1);
  D3D10_ERROR_FILE_NOT_FOUND=HResult(D3D10_HRESULT_Base or 2);
  //
  D3D10_APPEND_ALIGNED_ELEMENT=$ffffffff;
  D3D10_FILTER_TYPE_MASK=$3;
  D3D10_MIN_FILTER_SHIFT=4;
  D3D10_MAG_FILTER_SHIFT=2;
  D3D10_MIP_FILTER_SHIFT=0;
  D3D10_COMPARISON_FILTERING_BIT=$80;
  D3D10_ANISOTROPIC_FILTERING_BIT=$40;
  D3D10_TEXT_1BIT_BIT=$80000000;
  D3D10_SDK_VERSION=29;

type
  ID3D10DeviceChild=interface;
  PID3D10DeviceChild=^ID3D10DeviceChild;

  ID3D10DepthStencilState=interface;
  PID3D10DepthStencilState=^ID3D10DepthStencilState;

  ID3D10BlendState=interface;
  PID3D10BlendState=^ID3D10BlendState;

  ID3D10RasterizerState=interface;
  PID3D10RasterizerState=^ID3D10RasterizerState;

  ID3D10Resource=interface;
  PID3D10Resource=^ID3D10Resource;

  ID3D10Buffer=interface;
  PID3D10Buffer=^ID3D10Buffer;

  ID3D10Texture1D=interface;
  PID3D10Texture1D=^ID3D10Texture1D;

  ID3D10Texture2D=interface;
  PID3D10Texture2D=^ID3D10Texture2D;

  ID3D10Texture3D=interface;
  PID3D10Texture3D=^ID3D10Texture3D;

  ID3D10View=interface;
  PID3D10View=^ID3D10View;

  ID3D10ShaderResourceView=interface;
  PID3D10ShaderResourceView=^ID3D10ShaderResourceView;

  ID3D10RenderTargetView=interface;
  PID3D10RenderTargetView=^ID3D10RenderTargetView;

  ID3D10DepthStencilView=interface;
  PID3D10DepthStencilView=^ID3D10DepthStencilView;

  ID3D10VertexShader=interface;
  PID3D10VertexShader=^ID3D10VertexShader;

  ID3D10GeometryShader=interface;
  PID3D10GeometryShader=^ID3D10GeometryShader;

  ID3D10PixelShader=interface;
  PID3D10PixelShader=^ID3D10PixelShader;

  ID3D10InputLayout=interface;
  PID3D10InputLayout=^ID3D10InputLayout;

  ID3D10SamplerState=interface;
  PID3D10SamplerState=^ID3D10SamplerState;

  ID3D10Asynchronous=interface;
  PID3D10Asynchronous=^ID3D10Asynchronous;

  ID3D10Query=interface;
  PID3D10Query=^ID3D10Query;

  ID3D10Predicate=interface;
  PID3D10Predicate=^ID3D10Predicate;

  ID3D10Counter=interface;
  PID3D10Counter=^ID3D10Counter;

  ID3D10Device=interface;
  PID3D10Device=^ID3D10Device;

  ID3D10Multithread=interface;
  PID3D10Multithread=^ID3D10Multithread;

  TD3D10_InputClassification=
  (
    D3D10_INPUT_PER_VERTEX_DATA=0,
    D3D10_INPUT_PER_INSTANCE_DATA=1
  );
  PTD3D10_InputClassification=^TD3D10_InputClassification;
  D3D10_INPUT_CLASSIFICATION=TD3D10_InputClassification;
  PD3D10_INPUT_CLASSIFICATION=^TD3D10_InputClassification;

  TD3D10_InputElementDesc=record
    SemanticName:PAnsiChar;
    SemanticIndex:LongWord;
    Format:TDXGI_Format;
    InputSlot:LongWord;
    AlignedByteOffset:LongWord;
    InputSlotClass:TD3D10_InputClassification;
    InstanceDataStepRate:LongWord;
  end;
  PTD3D10_InputElementDesc=^TD3D10_InputElementDesc;
  D3D10_INPUT_ELEMENT_DESC=TD3D10_InputElementDesc;
  PD3D10_INPUT_ELEMENT_DESC=^TD3D10_InputElementDesc;

  TD3D10_FillMode=
  (
    D3D10_FILL_WIREFRAME=2,
    D3D10_FILL_SOLID=3
  );
  PTD3D10_FillMode=^TD3D10_FillMode;
  D3D10_FILL_MODE=TD3D10_FillMode;
  PD3D10_FILL_MODE=^TD3D10_FillMode;

  TD3D10_PrimitiveTopology=D3D_PRIMITIVE_TOPOLOGY;
  PTD3D10_PrimitiveTopology=^TD3D10_PrimitiveTopology;
  D3D10_PRIMITIVE_TOPOLOGY=TD3D10_PrimitiveTopology;
  PD3D10_PRIMITIVE_TOPOLOGY=^TD3D10_PrimitiveTopology;

  TD3D10_Primitive=D3D_PRIMITIVE;
  PTD3D10_Primitive=^TD3D10_Primitive;
  D3D10_PRIMITIVE=TD3D10_Primitive;
  PD3D10_PRIMITIVE=^TD3D10_Primitive;

  TD3D10_CullMode=
  (
    D3D10_CULL_NONE=1,
    D3D10_CULL_FRONT=2,
    D3D10_CULL_BACK=3
  );
  PTD3D10_CullMode=^TD3D10_CullMode;
  D3D10_CULL_MODE=TD3D10_CullMode;
  PD3D10_CULL_MODE=^TD3D10_CullMode;

  TD3D10_SoDeclarationEntry=record
    SemanticName:PAnsiChar;
    SemanticIndex:LongWord;
    StartComponent:Byte;
    ComponentCount:Byte;
    OutputSlot:Byte;
  end;
  PTD3D10_SoDeclarationEntry=^TD3D10_SoDeclarationEntry;
  D3D10_SO_DECLARATION_ENTRY=TD3D10_SoDeclarationEntry;
  PD3D10_SO_DECLARATION_ENTRY=^TD3D10_SoDeclarationEntry;

  TD3D10_Viewport=record
    TopLeftX:Integer;
    TopLeftY:Integer;
    Width:LongWord;
    Height:LongWord;
    MinDepth:Single;
    MaxDepth:Single;
  end;
  PTD3D10_Viewport=^TD3D10_Viewport;
  D3D10_VIEWPORT=TD3D10_Viewport;
  PD3D10_VIEWPORT=^TD3D10_Viewport;

  TD3D10_ResourceDimension=
  (
    D3D10_RESOURCE_DIMENSION_UNKNOWN=0,
    D3D10_RESOURCE_DIMENSION_BUFFER=1,
    D3D10_RESOURCE_DIMENSION_TEXTURE1D=2,
    D3D10_RESOURCE_DIMENSION_TEXTURE2D=3,
    D3D10_RESOURCE_DIMENSION_TEXTURE3D=4
  );
  PTD3D10_ResourceDimension=^TD3D10_ResourceDimension;
  D3D10_RESOURCE_DIMENSION=TD3D10_ResourceDimension;
  PD3D10_RESOURCE_DIMENSION=^TD3D10_ResourceDimension;

  TD3D10_SrvDimension=D3D_SRV_DIMENSION;
  PTD3D10_SrvDimension=^TD3D10_SrvDimension;
  D3D10_SRV_DIMENSION=TD3D10_SrvDimension;
  PD3D10_SRV_DIMENSION=^TD3D10_SrvDimension;

  TD3D10_DsvDimension=
  (
    D3D10_DSV_DIMENSION_UNKNOWN=0,
    D3D10_DSV_DIMENSION_TEXTURE1D=1,
    D3D10_DSV_DIMENSION_TEXTURE1DARRAY=2,
    D3D10_DSV_DIMENSION_TEXTURE2D=3,
    D3D10_DSV_DIMENSION_TEXTURE2DARRAY=4,
    D3D10_DSV_DIMENSION_TEXTURE2DMS=5,
    D3D10_DSV_DIMENSION_TEXTURE2DMSARRAY=6
  );
  PTD3D10_DsvDimension=^TD3D10_DsvDimension;
  D3D10_DSV_DIMENSION=TD3D10_DsvDimension;
  PD3D10_DSV_DIMENSION=^TD3D10_DsvDimension;

  TD3D10_RtvDimension=
  (
    D3D10_RTV_DIMENSION_UNKNOWN=0,
    D3D10_RTV_DIMENSION_BUFFER=1,
    D3D10_RTV_DIMENSION_TEXTURE1D=2,
    D3D10_RTV_DIMENSION_TEXTURE1DARRAY=3,
    D3D10_RTV_DIMENSION_TEXTURE2D=4,
    D3D10_RTV_DIMENSION_TEXTURE2DARRAY=5,
    D3D10_RTV_DIMENSION_TEXTURE2DMS=6,
    D3D10_RTV_DIMENSION_TEXTURE2DMSARRAY=7,
    D3D10_RTV_DIMENSION_TEXTURE3D=8
  );
  PTD3D10_RtvDimension=^TD3D10_RtvDimension;
  D3D10_RTV_DIMENSION=TD3D10_RtvDimension;
  PD3D10_RTV_DIMENSION=^TD3D10_RtvDimension;

  TD3D10_Usage=
  (
    D3D10_USAGE_DEFAULT=0,
    D3D10_USAGE_IMMUTABLE=1,
    D3D10_USAGE_DYNAMIC=2,
    D3D10_USAGE_STAGING=3
  );
  PTD3D10_Usage=^TD3D10_Usage;
  D3D10_USAGE=TD3D10_Usage;
  PD3D10_USAGE=^TD3D10_Usage;

  TD3D10_BindFlag=
  (
    D3D10_BIND_VERTEX_BUFFER=$1,
    D3D10_BIND_INDEX_BUFFER=$2,
    D3D10_BIND_CONSTANT_BUFFER=$4,
    D3D10_BIND_SHADER_RESOURCE=$8,
    D3D10_BIND_STREAM_OUTPUT=$10,
    D3D10_BIND_RENDER_TARGET=$20,
    D3D10_BIND_DEPTH_STENCIL=$40
  );
  PTD3D10_BindFlag=^TD3D10_BindFlag;
  D3D10_BIND_FLAG=TD3D10_BindFlag;
  PD3D10_BIND_FLAG=^TD3D10_BindFlag;

  TD3D10_CpuAccessFlag=
  (
    D3D10_CPU_ACCESS_WRITE=$10000,
    D3D10_CPU_ACCESS_READ=$20000
  );
  PTD3D10_CpuAccessFlag=^TD3D10_CpuAccessFlag;
  D3D10_CPU_ACCESS_FLAG=TD3D10_CpuAccessFlag;
  PD3D10_CPU_ACCESS_FLAG=^TD3D10_CpuAccessFlag;

  TD3D10_ResourceMiscFlag=
  (
    D3D10_RESOURCE_MISC_GENERATE_MIPS=$1,
    D3D10_RESOURCE_MISC_SHARED=$2,
    D3D10_RESOURCE_MISC_TEXTURECUBE=$4,
    D3D10_RESOURCE_MISC_SHARED_KEYEDMUTEX=$10,
    D3D10_RESOURCE_MISC_GDI_COMPATIBLE=$20
  );
  PTD3D10_ResourceMiscFlag=^TD3D10_ResourceMiscFlag;
  D3D10_RESOURCE_MISC_FLAG=TD3D10_ResourceMiscFlag;
  PD3D10_RESOURCE_MISC_FLAG=^TD3D10_ResourceMiscFlag;

  TD3D10_Map=
  (
    D3D10_MAP_READ=1,
    D3D10_MAP_WRITE=2,
    D3D10_MAP_READ_WRITE=3,
    D3D10_MAP_WRITE_DISCARD=4,
    D3D10_MAP_WRITE_NO_OVERWRITE=5
  );
  PTD3D10_Map=^TD3D10_Map;
  D3D10_MAP=TD3D10_Map;
  PD3D10_MAP=^TD3D10_Map;

  TD3D10_MapFlag=
  (
    D3D10_MAP_FLAG_DO_NOT_WAIT=$100000
  );
  PTD3D10_MapFlag=^TD3D10_MapFlag;
  D3D10_MAP_FLAG=TD3D10_MapFlag;
  PD3D10_MAP_FLAG=^TD3D10_MapFlag;

  TD3D10_RaiseFlag=
  (
    D3D10_RAISE_FLAG_DRIVER_INTERNAL_ERROR=$1
  );
  PTD3D10_RaiseFlag=^TD3D10_RaiseFlag;
  D3D10_RAISE_FLAG=TD3D10_RaiseFlag;
  PD3D10_RAISE_FLAG=^TD3D10_RaiseFlag;

  TD3D10_ClearFlag=
  (
    D3D10_CLEAR_DEPTH=$1,
    D3D10_CLEAR_STENCIL=$2
  );
  PTD3D10_ClearFlag=^TD3D10_ClearFlag;
  D3D10_CLEAR_FLAG=TD3D10_ClearFlag;
  PD3D10_CLEAR_FLAG=^TD3D10_ClearFlag;

  TD3D10_Rect=RECT;
  PTD3D10_Rect=^TD3D10_Rect;
  D3D10_RECT=TD3D10_Rect;
  PD3D10_RECT=^TD3D10_Rect;

  TD3D10_Box=record
    Left:LongWord;
    Top:LongWord;
    Front:LongWord;
    Right:LongWord;
    Bottom:LongWord;
    Back:LongWord;
  end;
  PTD3D10_Box=^TD3D10_Box;
  D3D10_BOX=TD3D10_Box;
  PD3D10_BOX=^TD3D10_Box;

  ID3D10DeviceChild=interface(IUnknown)
    ['{9B7E4C00-342C-4106-A19F-4F2704F689F0}']
    procedure GetDevice
    (
      out Device:ID3D10Device (* __out *)
    ); stdcall;

    function GetPrivateData
    (
      const Guid:TGUID; (* __in *)
      var DataSize:LongWord; (* __inout *)
      pData:Pointer (* __out_bcount_opt(*pDataSize) *)
    ):HResult; stdcall;

    function SetPrivateData
    (
      const Guid:TGUID; (* __in *)
      DataSize:LongWord; (* __in *)
      pData:Pointer (* __in_bcount_opt(DataSize) *)
    ):HResult; stdcall;

    function SetPrivateDataInterface
    (
      const Guid:TGUID; (* __in *)
      Data:IUnknown (* __in_opt *)
    ):HResult; stdcall;
  end;

  TD3D10_ComparisonFunc=
  (
    D3D10_COMPARISON_NEVER=1,
    D3D10_COMPARISON_LESS=2,
    D3D10_COMPARISON_EQUAL=3,
    D3D10_COMPARISON_LESS_EQUAL=4,
    D3D10_COMPARISON_GREATER=5,
    D3D10_COMPARISON_NOT_EQUAL=6,
    D3D10_COMPARISON_GREATER_EQUAL=7,
    D3D10_COMPARISON_ALWAYS=8
  );
  PTD3D10_ComparisonFunc=^TD3D10_ComparisonFunc;
  D3D10_COMPARISON_FUNC=TD3D10_ComparisonFunc;
  PD3D10_COMPARISON_FUNC=^TD3D10_ComparisonFunc;

  TD3D10_DepthWriteMask=
  (
    D3D10_DEPTH_WRITE_MASK_ZERO=0,
    D3D10_DEPTH_WRITE_MASK_ALL=1
  );
  PTD3D10_DepthWriteMask=^TD3D10_DepthWriteMask;
  D3D10_DEPTH_WRITE_MASK=TD3D10_DepthWriteMask;
  PD3D10_DEPTH_WRITE_MASK=^TD3D10_DepthWriteMask;

  TD3D10_StencilOp=
  (
    D3D10_STENCIL_OP_KEEP=1,
    D3D10_STENCIL_OP_ZERO=2,
    D3D10_STENCIL_OP_REPLACE=3,
    D3D10_STENCIL_OP_INCR_SAT=4,
    D3D10_STENCIL_OP_DECR_SAT=5,
    D3D10_STENCIL_OP_INVERT=6,
    D3D10_STENCIL_OP_INCR=7,
    D3D10_STENCIL_OP_DECR=8
  );
  PTD3D10_StencilOp=^TD3D10_StencilOp;
  D3D10_STENCIL_OP=TD3D10_StencilOp;
  PD3D10_STENCIL_OP=^TD3D10_StencilOp;

  TD3D10_DepthStencilopDesc=record
    StencilFailOp:TD3D10_StencilOp;
    StencilDepthFailOp:TD3D10_StencilOp;
    StencilPassOp:TD3D10_StencilOp;
    StencilFunc:TD3D10_ComparisonFunc;
  end;
  PTD3D10_DepthStencilopDesc=^TD3D10_DepthStencilopDesc;
  D3D10_DEPTH_STENCILOP_DESC=TD3D10_DepthStencilopDesc;
  PD3D10_DEPTH_STENCILOP_DESC=^TD3D10_DepthStencilopDesc;

  TD3D10_DepthStencilDesc=record
    DepthEnable:LongBool;
    DepthWriteMask:TD3D10_DepthWriteMask;
    DepthFunc:TD3D10_ComparisonFunc;
    StencilEnable:LongBool;
    StencilReadMask:Byte;
    StencilWriteMask:Byte;
    FrontFace:TD3D10_DepthStencilopDesc;
    BackFace:TD3D10_DepthStencilopDesc;
  end;
  PTD3D10_DepthStencilDesc=^TD3D10_DepthStencilDesc;
  D3D10_DEPTH_STENCIL_DESC=TD3D10_DepthStencilDesc;
  PD3D10_DEPTH_STENCIL_DESC=^TD3D10_DepthStencilDesc;

  ID3D10DepthStencilState=interface(ID3D10DeviceChild)
    ['{2B4B1CC8-A4AD-41F8-8322-CA86FC3EC675}']
    procedure GetDesc
    (
      out Desc:TD3D10_DepthStencilDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_Blend=
  (
    D3D10_BLEND_ZERO=1,
    D3D10_BLEND_ONE=2,
    D3D10_BLEND_SRC_COLOR=3,
    D3D10_BLEND_INV_SRC_COLOR=4,
    D3D10_BLEND_SRC_ALPHA=5,
    D3D10_BLEND_INV_SRC_ALPHA=6,
    D3D10_BLEND_DEST_ALPHA=7,
    D3D10_BLEND_INV_DEST_ALPHA=8,
    D3D10_BLEND_DEST_COLOR=9,
    D3D10_BLEND_INV_DEST_COLOR=10,
    D3D10_BLEND_SRC_ALPHA_SAT=11,
    D3D10_BLEND_BLEND_FACTOR=14,
    D3D10_BLEND_INV_BLEND_FACTOR=15,
    D3D10_BLEND_SRC1_COLOR=16,
    D3D10_BLEND_INV_SRC1_COLOR=17,
    D3D10_BLEND_SRC1_ALPHA=18,
    D3D10_BLEND_INV_SRC1_ALPHA=19
  );
  PTD3D10_Blend=^TD3D10_Blend;
  D3D10_BLEND=TD3D10_Blend;
  PD3D10_BLEND=^TD3D10_Blend;

  TD3D10_BlendOp=
  (
    D3D10_BLEND_OP_ADD=1,
    D3D10_BLEND_OP_SUBTRACT=2,
    D3D10_BLEND_OP_REV_SUBTRACT=3,
    D3D10_BLEND_OP_MIN=4,
    D3D10_BLEND_OP_MAX=5
  );
  PTD3D10_BlendOp=^TD3D10_BlendOp;
  D3D10_BLEND_OP=TD3D10_BlendOp;
  PD3D10_BLEND_OP=^TD3D10_BlendOp;

  TD3D10_ColorWriteEnable=
  (
    D3D10_COLOR_WRITE_ENABLE_RED=1,
    D3D10_COLOR_WRITE_ENABLE_GREEN=2,
    D3D10_COLOR_WRITE_ENABLE_BLUE=4,
    D3D10_COLOR_WRITE_ENABLE_ALPHA=8,
    D3D10_COLOR_WRITE_ENABLE_ALL=15
  );
  PTD3D10_ColorWriteEnable=^TD3D10_ColorWriteEnable;
  D3D10_COLOR_WRITE_ENABLE=TD3D10_ColorWriteEnable;
  PD3D10_COLOR_WRITE_ENABLE=^TD3D10_ColorWriteEnable;

  TD3D10_BlendDesc=record
    AlphaToCoverageEnable:LongBool;
    BlendEnable:array[0..7] of LongBool;
    SrcBlend:TD3D10_Blend;
    DestBlend:TD3D10_Blend;
    BlendOp:TD3D10_BlendOp;
    SrcBlendAlpha:TD3D10_Blend;
    DestBlendAlpha:TD3D10_Blend;
    BlendOpAlpha:TD3D10_BlendOp;
    RenderTargetWriteMask:array[0..7] of Byte;
  end;
  PTD3D10_BlendDesc=^TD3D10_BlendDesc;
  D3D10_BLEND_DESC=TD3D10_BlendDesc;
  PD3D10_BLEND_DESC=^TD3D10_BlendDesc;

  ID3D10BlendState=interface(ID3D10DeviceChild)
    ['{EDAD8D19-8A35-4D6D-8566-2EA276CDE161}']
    procedure GetDesc
    (
      out Desc:TD3D10_BlendDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_RasterizerDesc=record
    FillMode:TD3D10_FillMode;
    CullMode:TD3D10_CullMode;
    FrontCounterClockwise:LongBool;
    DepthBias:Integer;
    DepthBiasClamp:Single;
    SlopeScaledDepthBias:Single;
    DepthClipEnable:LongBool;
    ScissorEnable:LongBool;
    MultisampleEnable:LongBool;
    AntialiasedLineEnable:LongBool;
  end;
  PTD3D10_RasterizerDesc=^TD3D10_RasterizerDesc;
  D3D10_RASTERIZER_DESC=TD3D10_RasterizerDesc;
  PD3D10_RASTERIZER_DESC=^TD3D10_RasterizerDesc;

  ID3D10RasterizerState=interface(ID3D10DeviceChild)
    ['{A2A07292-89AF-4345-BE2E-C53D9FBB6E9F}']
    procedure GetDesc
    (
      out Desc:TD3D10_RasterizerDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_SubresourceData=record
    pSysMem:Pointer;
    SysMemPitch:LongWord;
    SysMemSlicePitch:LongWord;
  end;
  PTD3D10_SubresourceData=^TD3D10_SubresourceData;
  D3D10_SUBRESOURCE_DATA=TD3D10_SubresourceData;
  PD3D10_SUBRESOURCE_DATA=^TD3D10_SubresourceData;

  ID3D10Resource=interface(ID3D10DeviceChild)
    ['{9B7E4C01-342C-4106-A19F-4F2704F689F0}']
    procedure GetType
    (
      out RType:TD3D10_ResourceDimension (* __out *)
    ); stdcall;

    procedure SetEvictionPriority
    (
      EvictionPriority:LongWord (* __in *)
    ); stdcall;

    function GetEvictionPriority:LongWord; stdcall;
  end;

  TD3D10_BufferDesc=record
    ByteWidth:LongWord;
    Usage:TD3D10_Usage;
    BindFlags:LongWord;
    CPUAccessFlags:LongWord;
    MiscFlags:LongWord;
  end;
  PTD3D10_BufferDesc=^TD3D10_BufferDesc;
  D3D10_BUFFER_DESC=TD3D10_BufferDesc;
  PD3D10_BUFFER_DESC=^TD3D10_BufferDesc;

  ID3D10Buffer=interface(ID3D10Resource)
    ['{9B7E4C02-342C-4106-A19F-4F2704F689F0}']
    function Map
    (
      MapType:TD3D10_Map; (* __in *)
      MapFlags:LongWord; (* __in *)
      out pData:Pointer (* __out *)
    ):HResult; stdcall;

    procedure Unmap; stdcall;

    procedure GetDesc
    (
      out Desc:TD3D10_BufferDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_Texture1DDesc=record
    Width:LongWord;
    MipLevels:LongWord;
    ArraySize:LongWord;
    Format:TDXGI_Format;
    Usage:TD3D10_Usage;
    BindFlags:LongWord;
    CPUAccessFlags:LongWord;
    MiscFlags:LongWord;
  end;
  PTD3D10_Texture1DDesc=^TD3D10_Texture1DDesc;
  D3D10_TEXTURE1D_DESC=TD3D10_Texture1DDesc;
  PD3D10_TEXTURE1D_DESC=^TD3D10_Texture1DDesc;

  ID3D10Texture1D=interface(ID3D10Resource)
    ['{9B7E4C03-342C-4106-A19F-4F2704F689F0}']
    function Map
    (
      Subresource:LongWord; (* __in *)
      MapType:TD3D10_Map; (* __in *)
      MapFlags:LongWord; (* __in *)
      out pData:Pointer (* __out *)
    ):HResult; stdcall;

    procedure Unmap
    (
      Subresource:LongWord (* __in *)
    ); stdcall;

    procedure GetDesc
    (
      out Desc:TD3D10_Texture1DDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_Texture2DDesc=record
    Width:LongWord;
    Height:LongWord;
    MipLevels:LongWord;
    ArraySize:LongWord;
    Format:TDXGI_Format;
    SampleDesc:TDXGI_SampleDesc;
    Usage:TD3D10_Usage;
    BindFlags:LongWord;
    CPUAccessFlags:LongWord;
    MiscFlags:LongWord;
  end;
  PTD3D10_Texture2DDesc=^TD3D10_Texture2DDesc;
  D3D10_TEXTURE2D_DESC=TD3D10_Texture2DDesc;
  PD3D10_TEXTURE2D_DESC=^TD3D10_Texture2DDesc;

  TD3D10_MappedTexture2D=record
    pData:Pointer;
    RowPitch:LongWord;
  end;
  PTD3D10_MappedTexture2D=^TD3D10_MappedTexture2D;
  D3D10_MAPPED_TEXTURE2D=TD3D10_MappedTexture2D;
  PD3D10_MAPPED_TEXTURE2D=^TD3D10_MappedTexture2D;

  ID3D10Texture2D=interface(ID3D10Resource)
    ['{9B7E4C04-342C-4106-A19F-4F2704F689F0}']
    function Map
    (
      Subresource:LongWord; (* __in *)
      MapType:TD3D10_Map; (* __in *)
      MapFlags:LongWord; (* __in *)
      out MappedTex2D:TD3D10_MappedTexture2D (* __out *)
    ):HResult; stdcall;

    procedure Unmap
    (
      Subresource:LongWord (* __in *)
    ); stdcall;

    procedure GetDesc
    (
      out Desc:TD3D10_Texture2DDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_Texture3DDesc=record
    Width:LongWord;
    Height:LongWord;
    Depth:LongWord;
    MipLevels:LongWord;
    Format:TDXGI_Format;
    Usage:TD3D10_Usage;
    BindFlags:LongWord;
    CPUAccessFlags:LongWord;
    MiscFlags:LongWord;
  end;
  PTD3D10_Texture3DDesc=^TD3D10_Texture3DDesc;
  D3D10_TEXTURE3D_DESC=TD3D10_Texture3DDesc;
  PD3D10_TEXTURE3D_DESC=^TD3D10_Texture3DDesc;

  TD3D10_MappedTexture3D=record
    pData:Pointer;
    RowPitch:LongWord;
    DepthPitch:LongWord;
  end;
  PTD3D10_MappedTexture3D=^TD3D10_MappedTexture3D;
  D3D10_MAPPED_TEXTURE3D=TD3D10_MappedTexture3D;
  PD3D10_MAPPED_TEXTURE3D=^TD3D10_MappedTexture3D;

  ID3D10Texture3D=interface(ID3D10Resource)
    ['{9B7E4C05-342C-4106-A19F-4F2704F689F0}']
    function Map
    (
      Subresource:LongWord; (* __in *)
      MapType:TD3D10_Map; (* __in *)
      MapFlags:LongWord; (* __in *)
      out MappedTex3D:TD3D10_MappedTexture3D (* __out *)
    ):HResult; stdcall;

    procedure Unmap
    (
      Subresource:LongWord (* __in *)
    ); stdcall;

    procedure GetDesc
    (
      out Desc:TD3D10_Texture3DDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_TexturecubeFace=
  (
    D3D10_TEXTURECUBE_FACE_POSITIVE_X=0,
    D3D10_TEXTURECUBE_FACE_NEGATIVE_X=1,
    D3D10_TEXTURECUBE_FACE_POSITIVE_Y=2,
    D3D10_TEXTURECUBE_FACE_NEGATIVE_Y=3,
    D3D10_TEXTURECUBE_FACE_POSITIVE_Z=4,
    D3D10_TEXTURECUBE_FACE_NEGATIVE_Z=5
  );
  PTD3D10_TexturecubeFace=^TD3D10_TexturecubeFace;
  D3D10_TEXTURECUBE_FACE=TD3D10_TexturecubeFace;
  PD3D10_TEXTURECUBE_FACE=^TD3D10_TexturecubeFace;

  ID3D10View=interface(ID3D10DeviceChild)
    ['{C902B03F-60A7-49BA-9936-2A3AB37A7E33}']
    procedure GetResource
    (
      out Resource:ID3D10Resource (* __out *)
    ); stdcall;
  end;

  TD3D10_BufferSrv=record
    case Byte of
      0:
      (
        FirstElement:UINT;
        NumElements:UINT;
      );

      1:
      (
        ElementOffset:UINT;
        ElementWidth:UINT;
      );
  end;
  PTD3D10_BufferSrv=^TD3D10_BufferSrv;
  D3D10_BUFFER_SRV=TD3D10_BufferSrv;
  PD3D10_BUFFER_SRV=^TD3D10_BufferSrv;

  TD3D10_Tex1DSrv=record
    MostDetailedMip:LongWord;
    MipLevels:LongWord;
  end;
  PTD3D10_Tex1DSrv=^TD3D10_Tex1DSrv;
  D3D10_TEX1D_SRV=TD3D10_Tex1DSrv;
  PD3D10_TEX1D_SRV=^TD3D10_Tex1DSrv;

  TD3D10_Tex1DArraySrv=record
    MostDetailedMip:LongWord;
    MipLevels:LongWord;
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex1DArraySrv=^TD3D10_Tex1DArraySrv;
  D3D10_TEX1D_ARRAY_SRV=TD3D10_Tex1DArraySrv;
  PD3D10_TEX1D_ARRAY_SRV=^TD3D10_Tex1DArraySrv;

  TD3D10_Tex2DSrv=record
    MostDetailedMip:LongWord;
    MipLevels:LongWord;
  end;
  PTD3D10_Tex2DSrv=^TD3D10_Tex2DSrv;
  D3D10_TEX2D_SRV=TD3D10_Tex2DSrv;
  PD3D10_TEX2D_SRV=^TD3D10_Tex2DSrv;

  TD3D10_Tex2DArraySrv=record
    MostDetailedMip:LongWord;
    MipLevels:LongWord;
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex2DArraySrv=^TD3D10_Tex2DArraySrv;
  D3D10_TEX2D_ARRAY_SRV=TD3D10_Tex2DArraySrv;
  PD3D10_TEX2D_ARRAY_SRV=^TD3D10_Tex2DArraySrv;

  TD3D10_Tex3DSrv=record
    MostDetailedMip:LongWord;
    MipLevels:LongWord;
  end;
  PTD3D10_Tex3DSrv=^TD3D10_Tex3DSrv;
  D3D10_TEX3D_SRV=TD3D10_Tex3DSrv;
  PD3D10_TEX3D_SRV=^TD3D10_Tex3DSrv;

  TD3D10_TexcubeSrv=record
    MostDetailedMip:LongWord;
    MipLevels:LongWord;
  end;
  PTD3D10_TexcubeSrv=^TD3D10_TexcubeSrv;
  D3D10_TEXCUBE_SRV=TD3D10_TexcubeSrv;
  PD3D10_TEXCUBE_SRV=^TD3D10_TexcubeSrv;

  TD3D10_Tex2DmsSrv=record
    UnusedField_NothingToDefine:LongWord;
  end;
  PTD3D10_Tex2DmsSrv=^TD3D10_Tex2DmsSrv;
  D3D10_TEX2DMS_SRV=TD3D10_Tex2DmsSrv;
  PD3D10_TEX2DMS_SRV=^TD3D10_Tex2DmsSrv;

  TD3D10_Tex2DmsArraySrv=record
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex2DmsArraySrv=^TD3D10_Tex2DmsArraySrv;
  D3D10_TEX2DMS_ARRAY_SRV=TD3D10_Tex2DmsArraySrv;
  PD3D10_TEX2DMS_ARRAY_SRV=^TD3D10_Tex2DmsArraySrv;

  TD3D10_ShaderResourceViewDesc=record
    Format:TDXGI_Format;
    ViewDimension:TD3D10_SrvDimension;
    case Integer of
      0: (Buffer:D3D10_BUFFER_SRV);
      1: (Texture1D:D3D10_TEX1D_SRV);
      2: (Texture1DArray:D3D10_TEX1D_ARRAY_SRV);
      3: (Texture2D:D3D10_TEX2D_SRV);
      4: (Texture2DArray:D3D10_TEX2D_ARRAY_SRV);
      5: (Texture2DMS:D3D10_TEX2DMS_SRV);
      6: (Texture2DMSArray:D3D10_TEX2DMS_ARRAY_SRV);
      7: (Texture3D:D3D10_TEX3D_SRV);
      8: (TextureCube:D3D10_TEXCUBE_SRV);
  end;
  PTD3D10_ShaderResourceViewDesc=^TD3D10_ShaderResourceViewDesc;
  D3D10_SHADER_RESOURCE_VIEW_DESC=TD3D10_ShaderResourceViewDesc;
  PD3D10_SHADER_RESOURCE_VIEW_DESC=^TD3D10_ShaderResourceViewDesc;

  ID3D10ShaderResourceView=interface(ID3D10View)
    ['{9B7E4C07-342C-4106-A19F-4F2704F689F0}']
    procedure GetDesc
    (
      out Desc:TD3D10_ShaderResourceViewDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_BufferRtv=record
    case Byte of
      0:
      (
        FirstElement:UINT;
        NumElements:UINT;
      );

      1:
      (
        ElementOffset:UINT;
        ElementWidth:UINT;
      );
  end;
  PTD3D10_BufferRtv=^TD3D10_BufferRtv;
  D3D10_BUFFER_RTV=TD3D10_BufferRtv;
  PD3D10_BUFFER_RTV=^TD3D10_BufferRtv;

  TD3D10_Tex1DRtv=record
    MipSlice:LongWord;
  end;
  PTD3D10_Tex1DRtv=^TD3D10_Tex1DRtv;
  D3D10_TEX1D_RTV=TD3D10_Tex1DRtv;
  PD3D10_TEX1D_RTV=^TD3D10_Tex1DRtv;

  TD3D10_Tex1DArrayRtv=record
    MipSlice:LongWord;
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex1DArrayRtv=^TD3D10_Tex1DArrayRtv;
  D3D10_TEX1D_ARRAY_RTV=TD3D10_Tex1DArrayRtv;
  PD3D10_TEX1D_ARRAY_RTV=^TD3D10_Tex1DArrayRtv;

  TD3D10_Tex2DRtv=record
    MipSlice:LongWord;
  end;
  PTD3D10_Tex2DRtv=^TD3D10_Tex2DRtv;
  D3D10_TEX2D_RTV=TD3D10_Tex2DRtv;
  PD3D10_TEX2D_RTV=^TD3D10_Tex2DRtv;

  TD3D10_Tex2DmsRtv=record
    UnusedField_NothingToDefine:LongWord;
  end;
  PTD3D10_Tex2DmsRtv=^TD3D10_Tex2DmsRtv;
  D3D10_TEX2DMS_RTV=TD3D10_Tex2DmsRtv;
  PD3D10_TEX2DMS_RTV=^TD3D10_Tex2DmsRtv;

  TD3D10_Tex2DArrayRtv=record
    MipSlice:LongWord;
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex2DArrayRtv=^TD3D10_Tex2DArrayRtv;
  D3D10_TEX2D_ARRAY_RTV=TD3D10_Tex2DArrayRtv;
  PD3D10_TEX2D_ARRAY_RTV=^TD3D10_Tex2DArrayRtv;

  TD3D10_Tex2DmsArrayRtv=record
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex2DmsArrayRtv=^TD3D10_Tex2DmsArrayRtv;
  D3D10_TEX2DMS_ARRAY_RTV=TD3D10_Tex2DmsArrayRtv;
  PD3D10_TEX2DMS_ARRAY_RTV=^TD3D10_Tex2DmsArrayRtv;

  TD3D10_Tex3DRtv=record
    MipSlice:LongWord;
    FirstWSlice:LongWord;
    WSize:LongWord;
  end;
  PTD3D10_Tex3DRtv=^TD3D10_Tex3DRtv;
  D3D10_TEX3D_RTV=TD3D10_Tex3DRtv;
  PD3D10_TEX3D_RTV=^TD3D10_Tex3DRtv;

  TD3D10_RenderTargetViewDesc=record
    Format:TDXGI_Format;
    ViewDimension:TD3D10_RtvDimension;
    case Integer of
      0: (Buffer:D3D10_BUFFER_RTV);
      1: (Texture1D:D3D10_TEX1D_RTV);
      2: (Texture1DArray:D3D10_TEX1D_ARRAY_RTV);
      3: (Texture2D:D3D10_TEX2D_RTV);
      4: (Texture2DArray:D3D10_TEX2D_ARRAY_RTV);
      5: (Texture2DMS:D3D10_TEX2DMS_RTV);
      6: (Texture2DMSArray:D3D10_TEX2DMS_ARRAY_RTV);
      7: (Texture3D:D3D10_TEX3D_RTV);
  end;
  PTD3D10_RenderTargetViewDesc=^TD3D10_RenderTargetViewDesc;
  D3D10_RENDER_TARGET_VIEW_DESC=TD3D10_RenderTargetViewDesc;
  PD3D10_RENDER_TARGET_VIEW_DESC=^TD3D10_RenderTargetViewDesc;

  ID3D10RenderTargetView=interface(ID3D10View)
    ['{9B7E4C08-342C-4106-A19F-4F2704F689F0}']
    procedure GetDesc
    (
      out Desc:TD3D10_RenderTargetViewDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_Tex1DDsv=record
    MipSlice:LongWord;
  end;
  PTD3D10_Tex1DDsv=^TD3D10_Tex1DDsv;
  D3D10_TEX1D_DSV=TD3D10_Tex1DDsv;
  PD3D10_TEX1D_DSV=^TD3D10_Tex1DDsv;

  TD3D10_Tex1DArrayDsv=record
    MipSlice:LongWord;
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex1DArrayDsv=^TD3D10_Tex1DArrayDsv;
  D3D10_TEX1D_ARRAY_DSV=TD3D10_Tex1DArrayDsv;
  PD3D10_TEX1D_ARRAY_DSV=^TD3D10_Tex1DArrayDsv;

  TD3D10_Tex2DDsv=record
    MipSlice:LongWord;
  end;
  PTD3D10_Tex2DDsv=^TD3D10_Tex2DDsv;
  D3D10_TEX2D_DSV=TD3D10_Tex2DDsv;
  PD3D10_TEX2D_DSV=^TD3D10_Tex2DDsv;

  TD3D10_Tex2DArrayDsv=record
    MipSlice:LongWord;
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex2DArrayDsv=^TD3D10_Tex2DArrayDsv;
  D3D10_TEX2D_ARRAY_DSV=TD3D10_Tex2DArrayDsv;
  PD3D10_TEX2D_ARRAY_DSV=^TD3D10_Tex2DArrayDsv;

  TD3D10_Tex2DmsDsv=record
    UnusedField_NothingToDefine:LongWord;
  end;
  PTD3D10_Tex2DmsDsv=^TD3D10_Tex2DmsDsv;
  D3D10_TEX2DMS_DSV=TD3D10_Tex2DmsDsv;
  PD3D10_TEX2DMS_DSV=^TD3D10_Tex2DmsDsv;

  TD3D10_Tex2DmsArrayDsv=record
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex2DmsArrayDsv=^TD3D10_Tex2DmsArrayDsv;
  D3D10_TEX2DMS_ARRAY_DSV=TD3D10_Tex2DmsArrayDsv;
  PD3D10_TEX2DMS_ARRAY_DSV=^TD3D10_Tex2DmsArrayDsv;

  TD3D10_DepthStencilViewDesc=record
    Format:TDXGI_Format;
    ViewDimension:TD3D10_DsvDimension;
    case Integer of
      0: (Texture1D:D3D10_TEX1D_DSV);
      1: (Texture1DArray:D3D10_TEX1D_ARRAY_DSV);
      2: (Texture2D:D3D10_TEX2D_DSV);
      3: (Texture2DArray:D3D10_TEX2D_ARRAY_DSV);
      4: (Texture2DMS:D3D10_TEX2DMS_DSV);
      5: (Texture2DMSArray:D3D10_TEX2DMS_ARRAY_DSV);
  end;
  PTD3D10_DepthStencilViewDesc=^TD3D10_DepthStencilViewDesc;
  D3D10_DEPTH_STENCIL_VIEW_DESC=TD3D10_DepthStencilViewDesc;
  PD3D10_DEPTH_STENCIL_VIEW_DESC=^TD3D10_DepthStencilViewDesc;

  ID3D10DepthStencilView=interface(ID3D10View)
    ['{9B7E4C09-342C-4106-A19F-4F2704F689F0}']
    procedure GetDesc
    (
      out Desc:TD3D10_DepthStencilViewDesc (* __out *)
    ); stdcall;
  end;

  ID3D10VertexShader=interface(ID3D10DeviceChild)
    ['{9B7E4C0A-342C-4106-A19F-4F2704F689F0}']
  end;

  ID3D10GeometryShader=interface(ID3D10DeviceChild)
    ['{6316BE88-54CD-4040-AB44-20461BC81F68}']
  end;

  ID3D10PixelShader=interface(ID3D10DeviceChild)
    ['{4968B601-9D00-4CDE-8346-8E7F675819B6}']
  end;

  ID3D10InputLayout=interface(ID3D10DeviceChild)
    ['{9B7E4C0B-342C-4106-A19F-4F2704F689F0}']
  end;

  TD3D10_Filter=
  (
    D3D10_FILTER_MIN_MAG_MIP_POINT=0,
    D3D10_FILTER_MIN_MAG_POINT_MIP_LINEAR=$1,
    D3D10_FILTER_MIN_POINT_MAG_LINEAR_MIP_POINT=$4,
    D3D10_FILTER_MIN_POINT_MAG_MIP_LINEAR=$5,
    D3D10_FILTER_MIN_LINEAR_MAG_MIP_POINT=$10,
    D3D10_FILTER_MIN_LINEAR_MAG_POINT_MIP_LINEAR=$11,
    D3D10_FILTER_MIN_MAG_LINEAR_MIP_POINT=$14,
    D3D10_FILTER_MIN_MAG_MIP_LINEAR=$15,
    D3D10_FILTER_ANISOTROPIC=$55,
    D3D10_FILTER_COMPARISON_MIN_MAG_MIP_POINT=$80,
    D3D10_FILTER_COMPARISON_MIN_MAG_POINT_MIP_LINEAR=$81,
    D3D10_FILTER_COMPARISON_MIN_POINT_MAG_LINEAR_MIP_POINT=$84,
    D3D10_FILTER_COMPARISON_MIN_POINT_MAG_MIP_LINEAR=$85,
    D3D10_FILTER_COMPARISON_MIN_LINEAR_MAG_MIP_POINT=$90,
    D3D10_FILTER_COMPARISON_MIN_LINEAR_MAG_POINT_MIP_LINEAR=$91,
    D3D10_FILTER_COMPARISON_MIN_MAG_LINEAR_MIP_POINT=$94,
    D3D10_FILTER_COMPARISON_MIN_MAG_MIP_LINEAR=$95,
    D3D10_FILTER_COMPARISON_ANISOTROPIC=$d5,
    D3D10_FILTER_TEXT_1BIT=Integer($80000000)
  );
  PTD3D10_Filter=^TD3D10_Filter;
  D3D10_FILTER=TD3D10_Filter;
  PD3D10_FILTER=^TD3D10_Filter;

  TD3D10_FilterType=
  (
    D3D10_FILTER_TYPE_POINT=0,
    D3D10_FILTER_TYPE_LINEAR=1
  );
  PTD3D10_FilterType=^TD3D10_FilterType;
  D3D10_FILTER_TYPE=TD3D10_FilterType;
  PD3D10_FILTER_TYPE=^TD3D10_FilterType;

  TD3D10_TextureAddressMode=
  (
    D3D10_TEXTURE_ADDRESS_WRAP=1,
    D3D10_TEXTURE_ADDRESS_MIRROR=2,
    D3D10_TEXTURE_ADDRESS_CLAMP=3,
    D3D10_TEXTURE_ADDRESS_BORDER=4,
    D3D10_TEXTURE_ADDRESS_MIRROR_ONCE=5
  );
  PTD3D10_TextureAddressMode=^TD3D10_TextureAddressMode;
  D3D10_TEXTURE_ADDRESS_MODE=TD3D10_TextureAddressMode;
  PD3D10_TEXTURE_ADDRESS_MODE=^TD3D10_TextureAddressMode;

  TD3D10_SamplerDesc=record
    Filter:TD3D10_Filter;
    AddressU:TD3D10_TextureAddressMode;
    AddressV:TD3D10_TextureAddressMode;
    AddressW:TD3D10_TextureAddressMode;
    MipLODBias:Single;
    MaxAnisotropy:LongWord;
    ComparisonFunc:TD3D10_ComparisonFunc;
    BorderColor:array[0..3] of Single;
    MinLOD:Single;
    MaxLOD:Single;
  end;
  PTD3D10_SamplerDesc=^TD3D10_SamplerDesc;
  D3D10_SAMPLER_DESC=TD3D10_SamplerDesc;
  PD3D10_SAMPLER_DESC=^TD3D10_SamplerDesc;

  ID3D10SamplerState=interface(ID3D10DeviceChild)
    ['{9B7E4C0C-342C-4106-A19F-4F2704F689F0}']
    procedure GetDesc
    (
      out Desc:TD3D10_SamplerDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_FormatSupport=
  (
    D3D10_FORMAT_SUPPORT_BUFFER=$1,
    D3D10_FORMAT_SUPPORT_IA_VERTEX_BUFFER=$2,
    D3D10_FORMAT_SUPPORT_IA_INDEX_BUFFER=$4,
    D3D10_FORMAT_SUPPORT_SO_BUFFER=$8,
    D3D10_FORMAT_SUPPORT_TEXTURE1D=$10,
    D3D10_FORMAT_SUPPORT_TEXTURE2D=$20,
    D3D10_FORMAT_SUPPORT_TEXTURE3D=$40,
    D3D10_FORMAT_SUPPORT_TEXTURECUBE=$80,
    D3D10_FORMAT_SUPPORT_SHADER_LOAD=$100,
    D3D10_FORMAT_SUPPORT_SHADER_SAMPLE=$200,
    D3D10_FORMAT_SUPPORT_SHADER_SAMPLE_COMPARISON=$400,
    D3D10_FORMAT_SUPPORT_SHADER_SAMPLE_MONO_TEXT=$800,
    D3D10_FORMAT_SUPPORT_MIP=$1000,
    D3D10_FORMAT_SUPPORT_MIP_AUTOGEN=$2000,
    D3D10_FORMAT_SUPPORT_RENDER_TARGET=$4000,
    D3D10_FORMAT_SUPPORT_BLENDABLE=$8000,
    D3D10_FORMAT_SUPPORT_DEPTH_STENCIL=$10000,
    D3D10_FORMAT_SUPPORT_CPU_LOCKABLE=$20000,
    D3D10_FORMAT_SUPPORT_MULTISAMPLE_RESOLVE=$40000,
    D3D10_FORMAT_SUPPORT_DISPLAY=$80000,
    D3D10_FORMAT_SUPPORT_CAST_WITHIN_BIT_LAYOUT=$100000,
    D3D10_FORMAT_SUPPORT_MULTISAMPLE_RENDERTARGET=$200000,
    D3D10_FORMAT_SUPPORT_MULTISAMPLE_LOAD=$400000,
    D3D10_FORMAT_SUPPORT_SHADER_GATHER=$800000,
    D3D10_FORMAT_SUPPORT_BACK_BUFFER_CAST=$1000000
  );
  PTD3D10_FormatSupport=^TD3D10_FormatSupport;
  D3D10_FORMAT_SUPPORT=TD3D10_FormatSupport;
  PD3D10_FORMAT_SUPPORT=^TD3D10_FormatSupport;

  ID3D10Asynchronous=interface(ID3D10DeviceChild)
    ['{9B7E4C0D-342C-4106-A19F-4F2704F689F0}']
    procedure _Begin; stdcall;

    procedure _End; stdcall;

    function GetData
    (
      pData:Pointer; (* __out_bcount_opt(DataSize) *)
      DataSize:LongWord; (* __in *)
      GetDataFlags:LongWord (* __in *)
    ):HResult; stdcall;

    function GetDataSize:LongWord; stdcall;
  end;

  TD3D10_AsyncGetdataFlag=
  (
    D3D10_ASYNC_GETDATA_DONOTFLUSH=$1
  );
  PTD3D10_AsyncGetdataFlag=^TD3D10_AsyncGetdataFlag;
  D3D10_ASYNC_GETDATA_FLAG=TD3D10_AsyncGetdataFlag;
  PD3D10_ASYNC_GETDATA_FLAG=^TD3D10_AsyncGetdataFlag;

  TD3D10_Query=
  (
    D3D10_QUERY_EVENT=0,
    D3D10_QUERY_OCCLUSION=D3D10_QUERY_EVENT + 1,
    D3D10_QUERY_TIMESTAMP=D3D10_QUERY_OCCLUSION + 1,
    D3D10_QUERY_TIMESTAMP_DISJOINT=D3D10_QUERY_TIMESTAMP + 1,
    D3D10_QUERY_PIPELINE_STATISTICS=D3D10_QUERY_TIMESTAMP_DISJOINT + 1,
    D3D10_QUERY_OCCLUSION_PREDICATE=D3D10_QUERY_PIPELINE_STATISTICS + 1,
    D3D10_QUERY_SO_STATISTICS=D3D10_QUERY_OCCLUSION_PREDICATE + 1,
    D3D10_QUERY_SO_OVERFLOW_PREDICATE=D3D10_QUERY_SO_STATISTICS + 1
  );
  PTD3D10_Query=^TD3D10_Query;
  D3D10_QUERY=TD3D10_Query;
  PD3D10_QUERY=^TD3D10_Query;

  TD3D10_QueryMiscFlag=
  (
    D3D10_QUERY_MISC_PREDICATEHINT=$1
  );
  PTD3D10_QueryMiscFlag=^TD3D10_QueryMiscFlag;
  D3D10_QUERY_MISC_FLAG=TD3D10_QueryMiscFlag;
  PD3D10_QUERY_MISC_FLAG=^TD3D10_QueryMiscFlag;

  TD3D10_QueryDesc=record
    Query:TD3D10_Query;
    MiscFlags:LongWord;
  end;
  PTD3D10_QueryDesc=^TD3D10_QueryDesc;
  D3D10_QUERY_DESC=TD3D10_QueryDesc;
  PD3D10_QUERY_DESC=^TD3D10_QueryDesc;

  ID3D10Query=interface(ID3D10Asynchronous)
    ['{9B7E4C0E-342C-4106-A19F-4F2704F689F0}']
    procedure GetDesc
    (
      out Desc:TD3D10_QueryDesc (* __out *)
    ); stdcall;
  end;

  ID3D10Predicate=interface(ID3D10Query)
    ['{9B7E4C10-342C-4106-A19F-4F2704F689F0}']
  end;

  TD3D10_QueryDataTimestampDisjoint=record
    Frequency:UInt64;
    Disjoint:LongBool;
  end;
  PTD3D10_QueryDataTimestampDisjoint=^TD3D10_QueryDataTimestampDisjoint;
  D3D10_QUERY_DATA_TIMESTAMP_DISJOINT=TD3D10_QueryDataTimestampDisjoint;
  PD3D10_QUERY_DATA_TIMESTAMP_DISJOINT=^TD3D10_QueryDataTimestampDisjoint;

  TD3D10_QueryDataPipelineStatistics=record
    IAVertices:UInt64;
    IAPrimitives:UInt64;
    VSInvocations:UInt64;
    GSInvocations:UInt64;
    GSPrimitives:UInt64;
    CInvocations:UInt64;
    CPrimitives:UInt64;
    PSInvocations:UInt64;
  end;
  PTD3D10_QueryDataPipelineStatistics=^TD3D10_QueryDataPipelineStatistics;
  D3D10_QUERY_DATA_PIPELINE_STATISTICS=TD3D10_QueryDataPipelineStatistics;
  PD3D10_QUERY_DATA_PIPELINE_STATISTICS=^TD3D10_QueryDataPipelineStatistics;

  TD3D10_QueryDataSoStatistics=record
    NumPrimitivesWritten:UInt64;
    PrimitivesStorageNeeded:UInt64;
  end;
  PTD3D10_QueryDataSoStatistics=^TD3D10_QueryDataSoStatistics;
  D3D10_QUERY_DATA_SO_STATISTICS=TD3D10_QueryDataSoStatistics;
  PD3D10_QUERY_DATA_SO_STATISTICS=^TD3D10_QueryDataSoStatistics;

  TD3D10_Counter=
  (
    D3D10_COUNTER_GPU_IDLE=0,
    D3D10_COUNTER_VERTEX_PROCESSING=D3D10_COUNTER_GPU_IDLE + 1,
    D3D10_COUNTER_GEOMETRY_PROCESSING=D3D10_COUNTER_VERTEX_PROCESSING + 1,
    D3D10_COUNTER_PIXEL_PROCESSING=D3D10_COUNTER_GEOMETRY_PROCESSING + 1,
    D3D10_COUNTER_OTHER_GPU_PROCESSING=D3D10_COUNTER_PIXEL_PROCESSING + 1,
    D3D10_COUNTER_HOST_ADAPTER_BANDWIDTH_UTILIZATION=D3D10_COUNTER_OTHER_GPU_PROCESSING + 1,
    D3D10_COUNTER_LOCAL_VIDMEM_BANDWIDTH_UTILIZATION=D3D10_COUNTER_HOST_ADAPTER_BANDWIDTH_UTILIZATION + 1,
    D3D10_COUNTER_VERTEX_THROUGHPUT_UTILIZATION=D3D10_COUNTER_LOCAL_VIDMEM_BANDWIDTH_UTILIZATION + 1,
    D3D10_COUNTER_TRIANGLE_SETUP_THROUGHPUT_UTILIZATION=D3D10_COUNTER_VERTEX_THROUGHPUT_UTILIZATION + 1,
    D3D10_COUNTER_FILLRATE_THROUGHPUT_UTILIZATION=D3D10_COUNTER_TRIANGLE_SETUP_THROUGHPUT_UTILIZATION + 1,
    D3D10_COUNTER_VS_MEMORY_LIMITED=D3D10_COUNTER_FILLRATE_THROUGHPUT_UTILIZATION + 1,
    D3D10_COUNTER_VS_COMPUTATION_LIMITED=D3D10_COUNTER_VS_MEMORY_LIMITED + 1,
    D3D10_COUNTER_GS_MEMORY_LIMITED=D3D10_COUNTER_VS_COMPUTATION_LIMITED + 1,
    D3D10_COUNTER_GS_COMPUTATION_LIMITED=D3D10_COUNTER_GS_MEMORY_LIMITED + 1,
    D3D10_COUNTER_PS_MEMORY_LIMITED=D3D10_COUNTER_GS_COMPUTATION_LIMITED + 1,
    D3D10_COUNTER_PS_COMPUTATION_LIMITED=D3D10_COUNTER_PS_MEMORY_LIMITED + 1,
    D3D10_COUNTER_POST_TRANSFORM_CACHE_HIT_RATE=D3D10_COUNTER_PS_COMPUTATION_LIMITED + 1,
    D3D10_COUNTER_TEXTURE_CACHE_HIT_RATE=D3D10_COUNTER_POST_TRANSFORM_CACHE_HIT_RATE + 1,
    D3D10_COUNTER_DEVICE_DEPENDENT_0=$40000000
  );
  PTD3D10_Counter=^TD3D10_Counter;
  D3D10_COUNTER=TD3D10_Counter;
  PD3D10_COUNTER=^TD3D10_Counter;

  TD3D10_CounterType=
  (
    D3D10_COUNTER_TYPE_FLOAT32=0,
    D3D10_COUNTER_TYPE_UINT16=D3D10_COUNTER_TYPE_FLOAT32 + 1,
    D3D10_COUNTER_TYPE_UINT32=D3D10_COUNTER_TYPE_UINT16 + 1,
    D3D10_COUNTER_TYPE_UINT64=D3D10_COUNTER_TYPE_UINT32 + 1
  );
  PTD3D10_CounterType=^TD3D10_CounterType;
  D3D10_COUNTER_TYPE=TD3D10_CounterType;
  PD3D10_COUNTER_TYPE=^TD3D10_CounterType;

  TD3D10_CounterDesc=record
    Counter:TD3D10_Counter;
    MiscFlags:LongWord;
  end;
  PTD3D10_CounterDesc=^TD3D10_CounterDesc;
  D3D10_COUNTER_DESC=TD3D10_CounterDesc;
  PD3D10_COUNTER_DESC=^TD3D10_CounterDesc;

  TD3D10_CounterInfo=record
    LastDeviceDependentCounter:TD3D10_Counter;
    NumSimultaneousCounters:LongWord;
    NumDetectableParallelUnits:Byte;
  end;
  PTD3D10_CounterInfo=^TD3D10_CounterInfo;
  D3D10_COUNTER_INFO=TD3D10_CounterInfo;
  PD3D10_COUNTER_INFO=^TD3D10_CounterInfo;

  ID3D10Counter=interface(ID3D10Asynchronous)
    ['{9B7E4C11-342C-4106-A19F-4F2704F689F0}']
    procedure GetDesc
    (
      out Desc:TD3D10_CounterDesc (* __out *)
    ); stdcall;
  end;

  ID3D10Device=interface(IUnknown)
    ['{9B7E4C0F-342C-4106-A19F-4F2704F689F0}']
    procedure VSSetConstantBuffers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - 1 ) *)
      NumBuffers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - StartSlot ) *)
      pConstantBuffers:PID3D10Buffer (* __in_ecount(NumBuffers) *)
    ); stdcall;

    procedure PSSetShaderResources
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - 1 ) *)
      NumViews:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - StartSlot ) *)
      pShaderResourceViews:PID3D10ShaderResourceView (* __in_ecount(NumViews) *)
    ); stdcall;

    procedure PSSetShader
    (
      PixelShader:ID3D10PixelShader (* __in_opt *)
    ); stdcall;

    procedure PSSetSamplers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - 1 ) *)
      NumSamplers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - StartSlot ) *)
      pSamplers:PID3D10SamplerState (* __in_ecount(NumSamplers) *)
    ); stdcall;

    procedure VSSetShader
    (
      VertexShader:ID3D10VertexShader (* __in_opt *)
    ); stdcall;

    procedure DrawIndexed
    (
      IndexCount:LongWord; (* __in *)
      StartIndexLocation:LongWord; (* __in *)
      BaseVertexLocation:Integer (* __in *)
    ); stdcall;

    procedure Draw
    (
      VertexCount:LongWord; (* __in *)
      StartVertexLocation:LongWord (* __in *)
    ); stdcall;

    procedure PSSetConstantBuffers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - 1 ) *)
      NumBuffers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - StartSlot ) *)
      pConstantBuffers:PID3D10Buffer (* __in_ecount(NumBuffers) *)
    ); stdcall;

    procedure IASetInputLayout
    (
      InputLayout:ID3D10InputLayout (* __in_opt *)
    ); stdcall;

    procedure IASetVertexBuffers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_1_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT - 1 ) *)
      NumBuffers:LongWord; (* __in_range( 0, D3D10_1_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT - StartSlot ) *)
      pVertexBuffers:PID3D10Buffer; (* __in_ecount(NumBuffers) *)
      pStrides:PLongWord; (* __in_ecount(NumBuffers) *)
      pOffsets:PLongWord (* __in_ecount(NumBuffers) *)
    ); stdcall;

    procedure IASetIndexBuffer
    (
      IndexBuffer:ID3D10Buffer; (* __in_opt *)
      Format:TDXGI_Format; (* __in *)
      Offset:LongWord (* __in *)
    ); stdcall;

    procedure DrawIndexedInstanced
    (
      IndexCountPerInstance:LongWord; (* __in *)
      InstanceCount:LongWord; (* __in *)
      StartIndexLocation:LongWord; (* __in *)
      BaseVertexLocation:Integer; (* __in *)
      StartInstanceLocation:LongWord (* __in *)
    ); stdcall;

    procedure DrawInstanced
    (
      VertexCountPerInstance:LongWord; (* __in *)
      InstanceCount:LongWord; (* __in *)
      StartVertexLocation:LongWord; (* __in *)
      StartInstanceLocation:LongWord (* __in *)
    ); stdcall;

    procedure GSSetConstantBuffers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - 1 ) *)
      NumBuffers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - StartSlot ) *)
      pConstantBuffers:PID3D10Buffer (* __in_ecount(NumBuffers) *)
    ); stdcall;

    procedure GSSetShader
    (
      Shader:ID3D10GeometryShader (* __in_opt *)
    ); stdcall;

    procedure IASetPrimitiveTopology
    (
      Topology:TD3D10_PrimitiveTopology (* __in *)
    ); stdcall;

    procedure VSSetShaderResources
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - 1 ) *)
      NumViews:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - StartSlot ) *)
      pShaderResourceViews:PID3D10ShaderResourceView (* __in_ecount(NumViews) *)
    ); stdcall;

    procedure VSSetSamplers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - 1 ) *)
      NumSamplers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - StartSlot ) *)
      pSamplers:PID3D10SamplerState (* __in_ecount(NumSamplers) *)
    ); stdcall;

    procedure SetPredication
    (
      Predicate:ID3D10Predicate; (* __in_opt *)
      PredicateValue:LongBool (* __in *)
    ); stdcall;

    procedure GSSetShaderResources
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - 1 ) *)
      NumViews:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - StartSlot ) *)
      pShaderResourceViews:PID3D10ShaderResourceView (* __in_ecount(NumViews) *)
    ); stdcall;

    procedure GSSetSamplers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - 1 ) *)
      NumSamplers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - StartSlot ) *)
      pSamplers:PID3D10SamplerState (* __in_ecount(NumSamplers) *)
    ); stdcall;

    procedure OMSetRenderTargets
    (
      NumViews:LongWord; (* __in_range( 0, D3D10_SIMULTANEOUS_RENDER_TARGET_COUNT ) *)
      pRenderTargetViews:PID3D10RenderTargetView; (* __in_ecount_opt(NumViews) *)
      DepthStencilView:ID3D10DepthStencilView (* __in_opt *)
    ); stdcall;

    procedure OMSetBlendState
    (
      BlendState:ID3D10BlendState; (* __in_opt *)
      const BlendFactor:TColorArray; (* __in *)
      SampleMask:LongWord (* __in *)
    ); stdcall;

    procedure OMSetDepthStencilState
    (
      DepthStencilState:ID3D10DepthStencilState; (* __in_opt *)
      StencilRef:LongWord (* __in *)
    ); stdcall;

    procedure SOSetTargets
    (
      NumBuffers:LongWord; (* __in_range( 0, D3D10_SO_BUFFER_SLOT_COUNT) *)
      pSOTargets:PID3D10Buffer; (* __in_ecount_opt(NumBuffers) *)
      pOffsets:PLongWord (* __in_ecount_opt(NumBuffers) *)
    ); stdcall;

    procedure DrawAuto; stdcall;

    procedure RSSetState
    (
      RasterizerState:ID3D10RasterizerState (* __in_opt *)
    ); stdcall;

    procedure RSSetViewports
    (
      NumViewports:LongWord; (* __in_range(0, D3D10_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE) *)
      pViewports:PTD3D10_Viewport (* __in_ecount_opt(NumViewports) *)
    ); stdcall;

    procedure RSSetScissorRects
    (
      NumRects:LongWord; (* __in_range(0, D3D10_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE) *)
      pRects:PTD3D10_Rect (* __in_ecount_opt(NumRects) *)
    ); stdcall;

    procedure CopySubresourceRegion
    (
      DstResource:ID3D10Resource; (* __in *)
      DstSubresource:LongWord; (* __in *)
      DstX:LongWord; (* __in *)
      DstY:LongWord; (* __in *)
      DstZ:LongWord; (* __in *)
      SrcResource:ID3D10Resource; (* __in *)
      SrcSubresource:LongWord; (* __in *)
      pSrcBox:PTD3D10_Box (* __in_opt *)
    ); stdcall;

    procedure CopyResource
    (
      DstResource:ID3D10Resource; (* __in *)
      SrcResource:ID3D10Resource (* __in *)
    ); stdcall;

    procedure UpdateSubresource
    (
      DstResource:ID3D10Resource; (* __in *)
      DstSubresource:LongWord; (* __in *)
      pDstBox:PTD3D10_Box; (* __in_opt *)
      pSrcData:Pointer; (* __in *)
      SrcRowPitch:LongWord; (* __in *)
      SrcDepthPitch:LongWord (* __in *)
    ); stdcall;

    procedure ClearRenderTargetView
    (
      RenderTargetView:ID3D10RenderTargetView; (* __in *)
      const ColorRGBA:TColorArray (* __in *)
    ); stdcall;

    procedure ClearDepthStencilView
    (
      DepthStencilView:ID3D10DepthStencilView; (* __in *)
      ClearFlags:LongWord; (* __in *)
      Depth:Single; (* __in *)
      Stencil:Byte (* __in *)
    ); stdcall;

    procedure GenerateMips
    (
      ShaderResourceView:ID3D10ShaderResourceView (* __in *)
    ); stdcall;

    procedure ResolveSubresource
    (
      DstResource:ID3D10Resource; (* __in *)
      DstSubresource:LongWord; (* __in *)
      SrcResource:ID3D10Resource; (* __in *)
      SrcSubresource:LongWord; (* __in *)
      Format:TDXGI_Format (* __in *)
    ); stdcall;

    procedure VSGetConstantBuffers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - 1 ) *)
      NumBuffers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - StartSlot ) *)
      pConstantBuffers:PID3D10Buffer (* __out_ecount(NumBuffers) *)
    ); stdcall;

    procedure PSGetShaderResources
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - 1 ) *)
      NumViews:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - StartSlot ) *)
      pShaderResourceViews:PID3D10ShaderResourceView (* __out_ecount(NumViews) *)
    ); stdcall;

    procedure PSGetShader
    (
      out PixelShader:ID3D10PixelShader (* __out *)
    ); stdcall;

    procedure PSGetSamplers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - 1 ) *)
      NumSamplers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - StartSlot ) *)
      pSamplers:PID3D10SamplerState (* __out_ecount(NumSamplers) *)
    ); stdcall;

    procedure VSGetShader
    (
      out VertexShader:ID3D10VertexShader (* __out *)
    ); stdcall;

    procedure PSGetConstantBuffers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - 1 ) *)
      NumBuffers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - StartSlot ) *)
      pConstantBuffers:PID3D10Buffer (* __out_ecount(NumBuffers) *)
    ); stdcall;

    procedure IAGetInputLayout
    (
      out InputLayout:ID3D10InputLayout (* __out *)
    ); stdcall;

    procedure IAGetVertexBuffers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_1_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT - 1 ) *)
      NumBuffers:LongWord; (* __in_range( 0, D3D10_1_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT - StartSlot ) *)
      pVertexBuffers:PID3D10Buffer; (* __out_ecount_opt(NumBuffers) *)
      pStrides:PLongWord; (* __out_ecount_opt(NumBuffers) *)
      pOffsets:PLongWord (* __out_ecount_opt(NumBuffers) *)
    ); stdcall;

    procedure IAGetIndexBuffer
    (
      {$IFDEF UsePointersForOptionalOutputInterfaces}pIndexBuffer:PID3D10Buffer{$ELSE}out pIndexBuffer:ID3D10Buffer{$ENDIF}; (* __out_opt *)
      Format:PTDXGI_Format; (* __out_opt *)
      Offset:PLongWord (* __out_opt *)
    ); stdcall;

    procedure GSGetConstantBuffers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - 1 ) *)
      NumBuffers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - StartSlot ) *)
      pConstantBuffers:PID3D10Buffer (* __out_ecount(NumBuffers) *)
    ); stdcall;

    procedure GSGetShader
    (
      out GeometryShader:ID3D10GeometryShader (* __out *)
    ); stdcall;

    procedure IAGetPrimitiveTopology
    (
      out Topology:TD3D10_PrimitiveTopology (* __out *)
    ); stdcall;

    procedure VSGetShaderResources
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - 1 ) *)
      NumViews:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - StartSlot ) *)
      pShaderResourceViews:PID3D10ShaderResourceView (* __out_ecount(NumViews) *)
    ); stdcall;

    procedure VSGetSamplers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - 1 ) *)
      NumSamplers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - StartSlot ) *)
      pSamplers:PID3D10SamplerState (* __out_ecount(NumSamplers) *)
    ); stdcall;

    procedure GetPredication
    (
      {$IFDEF UsePointersForOptionalOutputInterfaces}pPredicate:PID3D10Predicate{$ELSE}out Predicate:ID3D10Predicate{$ENDIF}; (* __out_opt *)
      pPredicateValue:PLongBool (* __out_opt *)
    ); stdcall;

    procedure GSGetShaderResources
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - 1 ) *)
      NumViews:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - StartSlot ) *)
      pShaderResourceViews:PID3D10ShaderResourceView (* __out_ecount(NumViews) *)
    ); stdcall;

    procedure GSGetSamplers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - 1 ) *)
      NumSamplers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - StartSlot ) *)
      pSamplers:PID3D10SamplerState (* __out_ecount(NumSamplers) *)
    ); stdcall;

    procedure OMGetRenderTargets
    (
      NumViews:LongWord; (* __in_range( 0, D3D10_SIMULTANEOUS_RENDER_TARGET_COUNT ) *)
      pRenderTargetViews:PID3D10RenderTargetView; (* __out_ecount_opt(NumViews) *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pDepthStencilView:PID3D10DepthStencilView{$ELSE}out DepthStencilView:ID3D10DepthStencilView{$ENDIF} (* __out_opt *)
    ); stdcall;

    procedure OMGetBlendState
    (
      {$IFDEF UsePointersForOptionalOutputInterfaces}pBlendState:PID3D10BlendState{$ELSE}out BlendState:ID3D10BlendState{$ENDIF}; (* __out_opt *)
      const BlendFactor:TColorArray; (* __out_opt *)
      pSampleMask:PLongWord (* __out_opt *)
    ); stdcall;

    procedure OMGetDepthStencilState
    (
      {$IFDEF UsePointersForOptionalOutputInterfaces}pDepthStencilState:PID3D10DepthStencilState{$ELSE}out DepthStencilState:ID3D10DepthStencilState{$ENDIF}; (* __out_opt *)
      pStencilRef:PLongWord (* __out_opt *)
    ); stdcall;

    procedure SOGetTargets
    (
      NumBuffers:LongWord; (* __in_range( 0, D3D10_SO_BUFFER_SLOT_COUNT ) *)
      pSOTargets:PID3D10Buffer; (* __out_ecount_opt(NumBuffers) *)
      pOffsets:PLongWord (* __out_ecount_opt(NumBuffers) *)
    ); stdcall;

    procedure RSGetState
    (
      out RasterizerState:ID3D10RasterizerState (* __out *)
    ); stdcall;

    procedure RSGetViewports
    (
      var NumViewports:LongWord; (* __inout *)
      pViewports:PTD3D10_Viewport (* __out_ecount_opt(*NumViewports) *)
    ); stdcall;

    procedure RSGetScissorRects
    (
      var NumRects:LongWord; (* __inout *)
      pRects:PTD3D10_Rect (* __out_ecount_opt(*NumRects) *)
    ); stdcall;

    function GetDeviceRemovedReason:HResult; stdcall;

    function SetExceptionMode
    (
      RaiseFlags:LongWord
    ):HResult; stdcall;

    function GetExceptionMode:LongWord; stdcall;

    function GetPrivateData
    (
      const Guid:TGUID; (* __in *)
      var DataSize:LongWord; (* __inout *)
      pData:Pointer (* __out_bcount_opt(*pDataSize) *)
    ):HResult; stdcall;

    function SetPrivateData
    (
      const Guid:TGUID; (* __in *)
      DataSize:LongWord; (* __in *)
      pData:Pointer (* __in_bcount_opt(DataSize) *)
    ):HResult; stdcall;

    function SetPrivateDataInterface
    (
      const Guid:TGUID; (* __in *)
      Data:IUnknown (* __in_opt *)
    ):HResult; stdcall;

    procedure ClearState; stdcall;

    procedure Flush; stdcall;

    function CreateBuffer
    (
      const Desc:TD3D10_BufferDesc; (* __in *)
      pInitialData:PTD3D10_SubresourceData; (* __in_opt *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pBuffer:PID3D10Buffer{$ELSE}out Buffer:ID3D10Buffer{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateTexture1D
    (
      const Desc:TD3D10_Texture1DDesc; (* __in *)
      pInitialData:PTD3D10_SubresourceData; (* __in_xcount_opt(pDesc->MipLevels * pDesc->ArraySize) *)
      out Texture1D:ID3D10Texture1D (* __out *)
    ):HResult; stdcall;

    function CreateTexture2D
    (
      const Desc:TD3D10_Texture2DDesc; (* __in *)
      pInitialData:PTD3D10_SubresourceData; (* __in_xcount_opt(pDesc->MipLevels * pDesc->ArraySize) *)
      out Texture2D:ID3D10Texture2D (* __out *)
    ):HResult; stdcall;

    function CreateTexture3D
    (
      const Desc:TD3D10_Texture3DDesc; (* __in *)
      pInitialData:PTD3D10_SubresourceData; (* __in_xcount_opt(pDesc->MipLevels) *)
      out Texture3D:ID3D10Texture3D (* __out *)
    ):HResult; stdcall;

    function CreateShaderResourceView
    (
      Resource:ID3D10Resource; (* __in *)
      pDesc:PTD3D10_ShaderResourceViewDesc; (* __in_opt *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pSRView:PID3D10ShaderResourceView{$ELSE}out SRView:ID3D10ShaderResourceView{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateRenderTargetView
    (
      Resource:ID3D10Resource; (* __in *)
      pDesc:PTD3D10_RenderTargetViewDesc; (* __in_opt *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pRTView:PID3D10RenderTargetView{$ELSE}out RTView:ID3D10RenderTargetView{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateDepthStencilView
    (
      Resource:ID3D10Resource; (* __in *)
      pDesc:PTD3D10_DepthStencilViewDesc; (* __in_opt *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pDepthStencilView:PID3D10DepthStencilView{$ELSE}out DepthStencilView:ID3D10DepthStencilView{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateInputLayout
    (
      pInputElementDescs:PTD3D10_InputElementDesc; (* __in_ecount(NumElements) *)
      NumElements:LongWord; (* __in_range( 0, D3D10_1_IA_VERTEX_INPUT_STRUCTURE_ELEMENT_COUNT ) *)
      pShaderBytecodeWithInputSignature:Pointer; (* __in *)
      BytecodeLength:SIZE_T; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pInputLayout:PID3D10InputLayout{$ELSE}out InputLayout:ID3D10InputLayout{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateVertexShader
    (
      pShaderBytecode:Pointer; (* __in *)
      BytecodeLength:SIZE_T; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pVertexShader:PID3D10VertexShader{$ELSE}out VertexShader:ID3D10VertexShader{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateGeometryShader
    (
      pShaderBytecode:Pointer; (* __in *)
      BytecodeLength:SIZE_T; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pGeometryShader:PID3D10GeometryShader{$ELSE}out GeometryShader:ID3D10GeometryShader{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateGeometryShaderWithStreamOutput
    (
      pShaderBytecode:Pointer; (* __in *)
      BytecodeLength:SIZE_T; (* __in *)
      pSODeclaration:PTD3D10_SoDeclarationEntry; (* __in_ecount_opt(NumEntries) *)
      NumEntries:LongWord; (* __in_range( 0, D3D10_SO_SINGLE_BUFFER_COMPONENT_LIMIT ) *)
      OutputStreamStride:LongWord; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pGeometryShader:PID3D10GeometryShader{$ELSE}out GeometryShader:ID3D10GeometryShader{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreatePixelShader
    (
      pShaderBytecode:Pointer; (* __in *)
      BytecodeLength:SIZE_T; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pPixelShader:PID3D10PixelShader{$ELSE}out PixelShader:ID3D10PixelShader{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateBlendState
    (
      const BlendStateDesc:TD3D10_BlendDesc; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pBlendState:PID3D10BlendState{$ELSE}out BlendState:ID3D10BlendState{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateDepthStencilState
    (
      const DepthStencilDesc:TD3D10_DepthStencilDesc; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pDepthStencilState:PID3D10DepthStencilState{$ELSE}out DepthStencilState:ID3D10DepthStencilState{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateRasterizerState
    (
      const RasterizerDesc:TD3D10_RasterizerDesc; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pRasterizerState:PID3D10RasterizerState{$ELSE}out RasterizerState:ID3D10RasterizerState{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateSamplerState
    (
      const SamplerDesc:TD3D10_SamplerDesc; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pSamplerState:PID3D10SamplerState{$ELSE}out SamplerState:ID3D10SamplerState{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateQuery
    (
      const QueryDesc:TD3D10_QueryDesc; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pQuery:PID3D10Query{$ELSE}out Query:ID3D10Query{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreatePredicate
    (
      const PredicateDesc:TD3D10_QueryDesc; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pPredicate:PID3D10Predicate{$ELSE}out Predicate:ID3D10Predicate{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateCounter
    (
      const CounterDesc:TD3D10_CounterDesc; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pCounter:PID3D10Counter{$ELSE}out Counter:ID3D10Counter{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CheckFormatSupport
    (
      Format:TDXGI_Format; (* __in *)
      out FormatSupport:LongWord (* __out *)
    ):HResult; stdcall;

    function CheckMultisampleQualityLevels
    (
      Format:TDXGI_Format; (* __in *)
      SampleCount:LongWord; (* __in *)
      out NumQualityLevels:LongWord (* __out *)
    ):HResult; stdcall;

    procedure CheckCounterInfo
    (
      out CounterInfo:TD3D10_CounterInfo (* __out *)
    ); stdcall;

    function CheckCounter
    (
      const Desc:TD3D10_CounterDesc; (* __in *)
      out _Type:TD3D10_CounterType; (* __out *)
      out ActiveCounters:LongWord; (* __out *)
      Name:PAnsiChar; (* __out_ecount_opt(*pNameLength) *)
      pNameLength:PLongWord; (* __inout_opt *)
      Units:PAnsiChar; (* __out_ecount_opt(*pUnitsLength) *)
      pUnitsLength:PLongWord; (* __inout_opt *)
      Description:PAnsiChar; (* __out_ecount_opt(*pDescriptionLength) *)
      pDescriptionLength:PLongWord (* __inout_opt *)
    ):HResult; stdcall;

    function GetCreationFlags:LongWord; stdcall;

    function OpenSharedResource
    (
      hResource:THANDLE; (* __in *)
      const ReturnedInterface:TGUID; (* __in *)
      out Resource {Interface} (* __out_opt *)
    ):HResult; stdcall;

    procedure SetTextFilterSize
    (
      Width:LongWord; (* __in *)
      Height:LongWord (* __in *)
    ); stdcall;

    procedure GetTextFilterSize
    (
      pWidth:PLongWord; (* __out_opt *)
      pHeight:PLongWord (* __out_opt *)
    ); stdcall;
  end;

  ID3D10Multithread=interface(IUnknown)
    ['{9B7E4E00-342C-4106-A19F-4F2704F689F0}']
    procedure Enter; stdcall;

    procedure Leave; stdcall;

    function SetMultithreadProtected
    (
      MTProtect:LongBool (* __in *)
    ):LongBool; stdcall;

    function GetMultithreadProtected:LongBool; stdcall;
  end;

  TD3D10_CreateDeviceFlag=
  (
    D3D10_CREATE_DEVICE_SINGLETHREADED=$1,
    D3D10_CREATE_DEVICE_DEBUG=$2,
    D3D10_CREATE_DEVICE_SWITCH_TO_REF=$4,
    D3D10_CREATE_DEVICE_PREVENT_INTERNAL_THREADING_OPTIMIZATIONS=$8,
    D3D10_CREATE_DEVICE_ALLOW_NULL_FROM_MAP=$10,
    D3D10_CREATE_DEVICE_BGRA_SUPPORT=$20,
    D3D10_CREATE_DEVICE_STRICT_VALIDATION=$200
  );
  PTD3D10_CreateDeviceFlag=^TD3D10_CreateDeviceFlag;
  D3D10_CREATE_DEVICE_FLAG=TD3D10_CreateDeviceFlag;
  PD3D10_CREATE_DEVICE_FLAG=^TD3D10_CreateDeviceFlag;

///////////////////////////////////////////////////////////////////////////////
// End "D3D10.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D3D10Misc.h"
///////////////////////////////////////////////////////////////////////////////

type
  TD3D10_DriverType=
  (
    D3D10_DRIVER_TYPE_HARDWARE=0,
    D3D10_DRIVER_TYPE_REFERENCE=1,
    D3D10_DRIVER_TYPE_NULL=2,
    D3D10_DRIVER_TYPE_SOFTWARE=3,
    D3D10_DRIVER_TYPE_WARP=5
  );
  PTD3D10_DriverType=^TD3D10_DriverType;
  D3D10_DRIVER_TYPE=TD3D10_DriverType;
  PD3D10_DRIVER_TYPE=^TD3D10_DriverType;

{$IFDEF UseRuntimeLinking}var D3D10CreateDevice:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10CreateDevice{$ENDIF}(Adapter:IDXGIAdapter;DriverType:TD3D10_DriverType;Software:HMODULE;Flags:LongWord;SDKVersion:LongWord;out Device:ID3D10Device):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3D10CreateDeviceAndSwapChain:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10CreateDeviceAndSwapChain{$ENDIF}(Adapter:IDXGIAdapter;DriverType:TD3D10_DriverType;Software:HMODULE;Flags:LongWord;SDKVersion:LongWord;pSwapChainDesc:PTDXGI_SwapChainDesc;out SwapChain:IDXGISwapChain;out Device:ID3D10Device):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3D10CreateBlob:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10CreateBlob{$ENDIF}(NumBytes:SIZE_T;out Buffer:ID3D10Blob):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}

///////////////////////////////////////////////////////////////////////////////
// End "D3D10Misc.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D3D10Shader.h"
///////////////////////////////////////////////////////////////////////////////

const
  D3D10_SHADER_DEBUG=(1 shl 0);
  D3D10_SHADER_SKIP_VALIDATION=(1 shl 1);
  D3D10_SHADER_SKIP_OPTIMIZATION=(1 shl 2);
  D3D10_SHADER_PACK_MATRIX_ROW_MAJOR=(1 shl 3);
  D3D10_SHADER_PACK_MATRIX_COLUMN_MAJOR=(1 shl 4);
  D3D10_SHADER_PARTIAL_PRECISION=(1 shl 5);
  D3D10_SHADER_FORCE_VS_SOFTWARE_NO_OPT=(1 shl 6);
  D3D10_SHADER_FORCE_PS_SOFTWARE_NO_OPT=(1 shl 7);
  D3D10_SHADER_NO_PRESHADER=(1 shl 8);
  D3D10_SHADER_AVOID_FLOW_CONTROL=(1 shl 9);
  D3D10_SHADER_PREFER_FLOW_CONTROL=(1 shl 10);
  D3D10_SHADER_ENABLE_STRICTNESS=(1 shl 11);
  D3D10_SHADER_ENABLE_BACKWARDS_COMPATIBILITY=(1 shl 12);
  D3D10_SHADER_IEEE_STRICTNESS=(1 shl 13);
  D3D10_SHADER_WARNINGS_ARE_ERRORS=(1 shl 18);
  D3D10_SHADER_OPTIMIZATION_LEVEL0=(1 shl 14);
  D3D10_SHADER_OPTIMIZATION_LEVEL1=0;
  D3D10_SHADER_OPTIMIZATION_LEVEL2=((1 shl 14) or (1 shl 15));
  D3D10_SHADER_OPTIMIZATION_LEVEL3=(1 shl 15);

type
  TD3D10_ShaderMacro=D3D_SHADER_MACRO;
  PTD3D10_ShaderMacro=^TD3D10_ShaderMacro;
  D3D10_SHADER_MACRO=TD3D10_ShaderMacro;
  PD3D10_SHADER_MACRO=^TD3D10_ShaderMacro;

  TD3D10_ShaderVariableClass=D3D_SHADER_VARIABLE_CLASS;
  PTD3D10_ShaderVariableClass=^TD3D10_ShaderVariableClass;
  D3D10_SHADER_VARIABLE_CLASS=TD3D10_ShaderVariableClass;
  PD3D10_SHADER_VARIABLE_CLASS=^TD3D10_ShaderVariableClass;

  TD3D10_ShaderVariableFlags=D3D_SHADER_VARIABLE_FLAGS;
  PTD3D10_ShaderVariableFlags=^TD3D10_ShaderVariableFlags;
  D3D10_SHADER_VARIABLE_FLAGS=TD3D10_ShaderVariableFlags;
  PD3D10_SHADER_VARIABLE_FLAGS=^TD3D10_ShaderVariableFlags;

  TD3D10_ShaderVariableType=D3D_SHADER_VARIABLE_TYPE;
  PTD3D10_ShaderVariableType=^TD3D10_ShaderVariableType;
  D3D10_SHADER_VARIABLE_TYPE=TD3D10_ShaderVariableType;
  PD3D10_SHADER_VARIABLE_TYPE=^TD3D10_ShaderVariableType;

  TD3D10_ShaderInputFlags=D3D_SHADER_INPUT_FLAGS;
  PTD3D10_ShaderInputFlags=^TD3D10_ShaderInputFlags;
  D3D10_SHADER_INPUT_FLAGS=TD3D10_ShaderInputFlags;
  PD3D10_SHADER_INPUT_FLAGS=^TD3D10_ShaderInputFlags;

  TD3D10_ShaderInputType=D3D_SHADER_INPUT_TYPE;
  PTD3D10_ShaderInputType=^TD3D10_ShaderInputType;
  D3D10_SHADER_INPUT_TYPE=TD3D10_ShaderInputType;
  PD3D10_SHADER_INPUT_TYPE=^TD3D10_ShaderInputType;

  TD3D10_ShaderCbufferFlags=D3D_SHADER_CBUFFER_FLAGS;
  PTD3D10_ShaderCbufferFlags=^TD3D10_ShaderCbufferFlags;
  D3D10_SHADER_CBUFFER_FLAGS=TD3D10_ShaderCbufferFlags;
  PD3D10_SHADER_CBUFFER_FLAGS=^TD3D10_ShaderCbufferFlags;

  TD3D10_CBufferType=D3D_CBUFFER_TYPE;
  PTD3D10_CBufferType=^TD3D10_CBufferType;
  D3D10_CBUFFER_TYPE=TD3D10_CBufferType;
  PD3D10_CBUFFER_TYPE=^TD3D10_CBufferType;

  TD3D10_Name=D3D_NAME;
  PTD3D10_Name=^TD3D10_Name;
  D3D10_NAME=TD3D10_Name;
  PD3D10_NAME=^TD3D10_Name;

  TD3D10_ResourceReturnType=D3D_RESOURCE_RETURN_TYPE;
  PTD3D10_ResourceReturnType=^TD3D10_ResourceReturnType;
  D3D10_RESOURCE_RETURN_TYPE=TD3D10_ResourceReturnType;
  PD3D10_RESOURCE_RETURN_TYPE=^TD3D10_ResourceReturnType;

  TD3D10_RegisterComponentType=D3D_REGISTER_COMPONENT_TYPE;
  PTD3D10_RegisterComponentType=^TD3D10_RegisterComponentType;
  D3D10_REGISTER_COMPONENT_TYPE=TD3D10_RegisterComponentType;
  PD3D10_REGISTER_COMPONENT_TYPE=^TD3D10_RegisterComponentType;

  TD3D10_IncludeType=D3D_INCLUDE_TYPE;
  PTD3D10_IncludeType=^TD3D10_IncludeType;
  D3D10_INCLUDE_TYPE=TD3D10_IncludeType;
  PD3D10_INCLUDE_TYPE=^TD3D10_IncludeType;

  ID3D10Include=ID3DInclude;
  PID3D10Include=^ID3D10Include;

  TD3D10_ShaderDesc=record
    Version:LongWord;
    Creator:PAnsiChar;
    Flags:LongWord;
    ConstantBuffers:LongWord;
    BoundResources:LongWord;
    InputParameters:LongWord;
    OutputParameters:LongWord;
    InstructionCount:LongWord;
    TempRegisterCount:LongWord;
    TempArrayCount:LongWord;
    DefCount:LongWord;
    DclCount:LongWord;
    TextureNormalInstructions:LongWord;
    TextureLoadInstructions:LongWord;
    TextureCompInstructions:LongWord;
    TextureBiasInstructions:LongWord;
    TextureGradientInstructions:LongWord;
    FloatInstructionCount:LongWord;
    IntInstructionCount:LongWord;
    UintInstructionCount:LongWord;
    StaticFlowControlCount:LongWord;
    DynamicFlowControlCount:LongWord;
    MacroInstructionCount:LongWord;
    ArrayInstructionCount:LongWord;
    CutInstructionCount:LongWord;
    EmitInstructionCount:LongWord;
    GSOutputTopology:TD3D10_PrimitiveTopology;
    GSMaxOutputVertexCount:LongWord;
  end;
  PTD3D10_ShaderDesc=^TD3D10_ShaderDesc;
  D3D10_SHADER_DESC=TD3D10_ShaderDesc;
  PD3D10_SHADER_DESC=^TD3D10_ShaderDesc;

  TD3D10_ShaderBufferDesc=record
    Name:PAnsiChar;
    _Type:TD3D10_CBufferType;
    Variables:LongWord;
    Size:LongWord;
    Flags:LongWord;
  end;
  PTD3D10_ShaderBufferDesc=^TD3D10_ShaderBufferDesc;
  D3D10_SHADER_BUFFER_DESC=TD3D10_ShaderBufferDesc;
  PD3D10_SHADER_BUFFER_DESC=^TD3D10_ShaderBufferDesc;

  TD3D10_ShaderVariableDesc=record
    Name:PAnsiChar;
    StartOffset:LongWord;
    Size:LongWord;
    Flags:LongWord;
    DefaultValue:Pointer;
  end;
  PTD3D10_ShaderVariableDesc=^TD3D10_ShaderVariableDesc;
  D3D10_SHADER_VARIABLE_DESC=TD3D10_ShaderVariableDesc;
  PD3D10_SHADER_VARIABLE_DESC=^TD3D10_ShaderVariableDesc;

  TD3D10_ShaderTypeDesc=record
    _Class:TD3D10_ShaderVariableClass;
    _Type:TD3D10_ShaderVariableType;
    Rows:LongWord;
    Columns:LongWord;
    Elements:LongWord;
    Members:LongWord;
    Offset:LongWord;
  end;
  PTD3D10_ShaderTypeDesc=^TD3D10_ShaderTypeDesc;
  D3D10_SHADER_TYPE_DESC=TD3D10_ShaderTypeDesc;
  PD3D10_SHADER_TYPE_DESC=^TD3D10_ShaderTypeDesc;

  TD3D10_ShaderInputBindDesc=record
    Name:PAnsiChar;
    _Type:TD3D10_ShaderInputType;
    BindPoint:LongWord;
    BindCount:LongWord;
    Flags:LongWord;
    ReturnType:TD3D10_ResourceReturnType;
    Dimension:TD3D10_SrvDimension;
    NumSamples:LongWord;
  end;
  PTD3D10_ShaderInputBindDesc=^TD3D10_ShaderInputBindDesc;
  D3D10_SHADER_INPUT_BIND_DESC=TD3D10_ShaderInputBindDesc;
  PD3D10_SHADER_INPUT_BIND_DESC=^TD3D10_ShaderInputBindDesc;

  TD3D10_SignatureParameterDesc=record
    SemanticName:PAnsiChar;
    SemanticIndex:LongWord;
    _Register:LongWord;
    SystemValueType:TD3D10_Name;
    ComponentType:TD3D10_RegisterComponentType;
    Mask:Byte;
    ReadWriteMask:Byte;
  end;
  PTD3D10_SignatureParameterDesc=^TD3D10_SignatureParameterDesc;
  D3D10_SIGNATURE_PARAMETER_DESC=TD3D10_SignatureParameterDesc;
  PD3D10_SIGNATURE_PARAMETER_DESC=^TD3D10_SignatureParameterDesc;

  ID3D10ShaderReflectionType=class;
  PID3D10ShaderReflectionType=^ID3D10ShaderReflectionType;

  ID3D10ShaderReflectionType=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function GetDesc(out Desc:TD3D10_ShaderTypeDesc):HResult; virtual; stdcall; abstract;
    function GetMemberTypeByIndex(Index:LongWord):ID3D10ShaderReflectionType; virtual; stdcall; abstract;
    function GetMemberTypeByName(Name:PAnsiChar):ID3D10ShaderReflectionType; virtual; stdcall; abstract;
    function GetMemberTypeName(Index:LongWord):PAnsiChar; virtual; stdcall; abstract;
  end;

  ID3D10ShaderReflectionVariable=class;
  PID3D10ShaderReflectionVariable=^ID3D10ShaderReflectionVariable;

  ID3D10ShaderReflectionVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function GetDesc(out Desc:TD3D10_ShaderVariableDesc):HResult; virtual; stdcall; abstract;
    function GetType:ID3D10ShaderReflectionType; virtual; stdcall; abstract;
  end;

  ID3D10ShaderReflectionConstantBuffer=class;
  PID3D10ShaderReflectionConstantBuffer=^ID3D10ShaderReflectionConstantBuffer;

  ID3D10ShaderReflectionConstantBuffer=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function GetDesc(out Desc:TD3D10_ShaderBufferDesc):HResult; virtual; stdcall; abstract;
    function GetVariableByIndex(Index:LongWord):ID3D10ShaderReflectionVariable; virtual; stdcall; abstract;
    function GetVariableByName(Name:PAnsiChar):ID3D10ShaderReflectionVariable; virtual; stdcall; abstract;
  end;

  ID3D10ShaderReflection=interface;
  PID3D10ShaderReflection=^ID3D10ShaderReflection;

  ID3D10ShaderReflection=interface(IUnknown)
    ['{D40E20B6-F8F7-42AD-AB20-4BAF8F15DFAA}']
    function GetDesc(out Desc:TD3D10_ShaderDesc):HResult; stdcall;
    function GetConstantBufferByIndex(Index:LongWord):ID3D10ShaderReflectionConstantBuffer; stdcall;
    function GetConstantBufferByName(Name:PAnsiChar):ID3D10ShaderReflectionConstantBuffer; stdcall;
    function GetResourceBindingDesc(ResourceIndex:LongWord;out Desc:TD3D10_ShaderInputBindDesc):HResult; stdcall;
    function GetInputParameterDesc(ParameterIndex:LongWord;out Desc:TD3D10_SignatureParameterDesc):HResult; stdcall;
    function GetOutputParameterDesc(ParameterIndex:LongWord;out Desc:TD3D10_SignatureParameterDesc):HResult; stdcall;
  end;

{$IFDEF UseRuntimeLinking}var D3D10CompileShader:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10CompileShader{$ENDIF}(pSrcData:PAnsiChar;SrcDataLen:SIZE_T;pFileName:PAnsiChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;pFunctionName:PAnsiChar;pProfile:PAnsiChar;Flags:LongWord;out Shader:ID3D10Blob;out ErrorMsgs:ID3D10Blob):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3D10DisassembleShader:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10DisassembleShader{$ENDIF}(pShader:Pointer;BytecodeLength:SIZE_T;EnableColorCode:LongBool;pComments:PAnsiChar;out Disassembly:ID3D10Blob):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3D10GetPixelShaderProfile:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10GetPixelShaderProfile{$ENDIF}(Device:ID3D10Device):PAnsiChar; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3D10GetVertexShaderProfile:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10GetVertexShaderProfile{$ENDIF}(Device:ID3D10Device):PAnsiChar; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3D10GetGeometryShaderProfile:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10GetGeometryShaderProfile{$ENDIF}(Device:ID3D10Device):PAnsiChar; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3D10ReflectShader:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10ReflectShader{$ENDIF}(pShaderBytecode:Pointer;BytecodeLength:SIZE_T;out Reflector:ID3D10ShaderReflection):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3D10PreprocessShader:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10PreprocessShader{$ENDIF}(pSrcData:PAnsiChar;SrcDataSize:SIZE_T;pFileName:PAnsiChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;out ShaderText:ID3D10Blob;out ErrorMsgs:ID3D10Blob):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3D10GetInputSignatureBlob:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10GetInputSignatureBlob{$ENDIF}(pShaderBytecode:Pointer;BytecodeLength:SIZE_T;out SignatureBlob:ID3D10Blob):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3D10GetOutputSignatureBlob:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10GetOutputSignatureBlob{$ENDIF}(pShaderBytecode:Pointer;BytecodeLength:SIZE_T;out SignatureBlob:ID3D10Blob):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3D10GetInputAndOutputSignatureBlob:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10GetInputAndOutputSignatureBlob{$ENDIF}(pShaderBytecode:Pointer;BytecodeLength:SIZE_T;out SignatureBlob:ID3D10Blob):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3D10GetShaderDebugInfo:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10GetShaderDebugInfo{$ENDIF}(pShaderBytecode:Pointer;BytecodeLength:SIZE_T;out DebugInfo:ID3D10Blob):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}

///////////////////////////////////////////////////////////////////////////////
// End "D3D10Shader.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D3D10Effect.h"
///////////////////////////////////////////////////////////////////////////////

const
  D3D10_EFFECT_COMPILE_CHILD_EFFECT=(1 shl 0);
  D3D10_EFFECT_COMPILE_ALLOW_SLOW_OPS=(1 shl 1);
  D3D10_EFFECT_SINGLE_THREADED=(1 shl 3);
  D3D10_EFFECT_VARIABLE_POOLED=(1 shl 0);
  D3D10_EFFECT_VARIABLE_ANNOTATION=(1 shl 1);
  D3D10_EFFECT_VARIABLE_EXPLICIT_BIND_POINT=(1 shl 2);

type
  TD3D10_DeviceStateTypes=
  (
    D3D10_DST_SO_BUFFERS=1,
    D3D10_DST_OM_RENDER_TARGETS,
    D3D10_DST_OM_DEPTH_STENCIL_STATE,
    D3D10_DST_OM_BLEND_STATE,
    D3D10_DST_VS,
    D3D10_DST_VS_SAMPLERS,
    D3D10_DST_VS_SHADER_RESOURCES,
    D3D10_DST_VS_CONSTANT_BUFFERS,
    D3D10_DST_GS,
    D3D10_DST_GS_SAMPLERS,
    D3D10_DST_GS_SHADER_RESOURCES,
    D3D10_DST_GS_CONSTANT_BUFFERS,
    D3D10_DST_PS,
    D3D10_DST_PS_SAMPLERS,
    D3D10_DST_PS_SHADER_RESOURCES,
    D3D10_DST_PS_CONSTANT_BUFFERS,
    D3D10_DST_IA_VERTEX_BUFFERS,
    D3D10_DST_IA_INDEX_BUFFER,
    D3D10_DST_IA_INPUT_LAYOUT,
    D3D10_DST_IA_PRIMITIVE_TOPOLOGY,
    D3D10_DST_RS_VIEWPORTS,
    D3D10_DST_RS_SCISSOR_RECTS,
    D3D10_DST_RS_RASTERIZER_STATE,
    D3D10_DST_PREDICATION
  );
  PTD3D10_DeviceStateTypes=^TD3D10_DeviceStateTypes;
  D3D10_DEVICE_STATE_TYPES=TD3D10_DeviceStateTypes;
  PD3D10_DEVICE_STATE_TYPES=^TD3D10_DeviceStateTypes;

  TD3D10_StateBlockMask=record
    VS:Byte;
    VSSamplers:array [0..1] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT)
    VSShaderResources:array[0..15] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT)
    VSConstantBuffers:array[0..1] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT)

    GS:Byte;
    GSSamplers:array[0..1] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT)
    GSShaderResources:array[0..15] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT)
    GSConstantBuffers:array[0..1] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT)

    PS:Byte;
    PSSamplers:array[0..1] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT)
    PSShaderResources:array[0..15] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT)
    PSConstantBuffers:array[0..1] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT)

    IAVertexBuffers:array[0..1] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT)
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
  PTD3D10_StateBlockMask=^TD3D10_StateBlockMask;
  D3D10_STATE_BLOCK_MASK=TD3D10_StateBlockMask;
  PD3D10_STATE_BLOCK_MASK=^TD3D10_StateBlockMask;

  ID3D10StateBlock=interface;
  PID3D10StateBlock=^ID3D10StateBlock;

  ID3D10StateBlock=interface(IUnknown)
    ['{0803425A-57F5-4DD6-9465-A87570834A08}']
    function Capture:HResult; stdcall;
    function Apply:HResult; stdcall;
    function ReleaseAllDeviceObjects:HResult; stdcall;
    function GetDevice(out Device:ID3D10Device):HResult; stdcall;
  end;

{$IFDEF UseRuntimeLinking}var D3D10StateBlockMaskUnion:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10StateBlockMaskUnion{$ENDIF}
(
  const A:TD3D10_StateBlockMask; (* __in *)
  const B:TD3D10_StateBlockMask; (* __in *)
  out Result:TD3D10_StateBlockMask (* __out *)
):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3D10StateBlockMaskIntersect:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10StateBlockMaskIntersect{$ENDIF}
(
  const A:TD3D10_StateBlockMask; (* __in *)
  const B:TD3D10_StateBlockMask; (* __in *)
  out Result:TD3D10_StateBlockMask (* __out *)
):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3D10StateBlockMaskDifference:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10StateBlockMaskDifference{$ENDIF}
(
  const A:TD3D10_StateBlockMask; (* __in *)
  const B:TD3D10_StateBlockMask; (* __in *)
  out Result:TD3D10_StateBlockMask (* __out *)
):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3D10StateBlockMaskEnableCapture:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10StateBlockMaskEnableCapture{$ENDIF}
(
  const Mask:TD3D10_StateBlockMask; (* __in *)
  StateType:TD3D10_DeviceStateTypes; (* __in *)
  RangeStart:LongWord;
  RangeLength:LongWord
):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3D10StateBlockMaskDisableCapture:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10StateBlockMaskDisableCapture{$ENDIF}
(
  const Mask:TD3D10_StateBlockMask; (* __in *)
  StateType:TD3D10_DeviceStateTypes; (* __in *)
  RangeStart:LongWord;
  RangeLength:LongWord
):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3D10StateBlockMaskEnableAll:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10StateBlockMaskEnableAll{$ENDIF}
(
  const Mask:TD3D10_StateBlockMask (* __in *)
):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3D10StateBlockMaskDisableAll:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10StateBlockMaskDisableAll{$ENDIF}
(
  const Mask:TD3D10_StateBlockMask (* __in *)
):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3D10StateBlockMaskGetSetting:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10StateBlockMaskGetSetting{$ENDIF}
(
  const Mask:TD3D10_StateBlockMask; (* __in *)
  StateType:TD3D10_DeviceStateTypes;
  Entry:LongWord
):LongBool; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}

{$IFDEF UseRuntimeLinking}var D3D10CreateStateBlock:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10CreateStateBlock{$ENDIF}
(
  Device:ID3D10Device;
  const StateBlockMask:TD3D10_StateBlockMask; (* __in *)
  out StateBlock:ID3D10StateBlock
):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}

type
  TD3D10_EffectTypeDesc=record
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
  PTD3D10_EffectTypeDesc=^TD3D10_EffectTypeDesc;
  D3D10_EFFECT_TYPE_DESC=TD3D10_EffectTypeDesc;
  PD3D10_EFFECT_TYPE_DESC=^TD3D10_EffectTypeDesc;

  ID3D10EffectType=class;
  PID3D10EffectType=^ID3D10EffectType;

  ID3D10EffectType=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectTypeDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetMemberTypeByIndex
    (
      Index:LongWord
    ):ID3D10EffectType; virtual; stdcall; abstract;

    function GetMemberTypeByName
    (
      Name:PAnsiChar
    ):ID3D10EffectType; virtual; stdcall; abstract;

    function GetMemberTypeBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectType; virtual; stdcall; abstract;

    function GetMemberName
    (
      Index:LongWord
    ):PAnsiChar; virtual; stdcall; abstract;

    function GetMemberSemantic
    (
      Index:LongWord
    ):PAnsiChar; virtual; stdcall; abstract;
  end;

  TD3D10_EffectVariableDesc=record
    Name:PAnsiChar;
    Semantic:PAnsiChar;
    Flags:LongWord;
    Annotations:LongWord;
    BufferOffset:LongWord;
    ExplicitBindPoint:LongWord;
  end;
  PTD3D10_EffectVariableDesc=^TD3D10_EffectVariableDesc;
  D3D10_EFFECT_VARIABLE_DESC=TD3D10_EffectVariableDesc;
  PD3D10_EFFECT_VARIABLE_DESC=^TD3D10_EffectVariableDesc;

  ID3D10EffectVariable=class;
  PID3D10EffectVariable=^ID3D10EffectVariable;

  ID3D10EffectScalarVariable=class;
  PID3D10EffectScalarVariable=^ID3D10EffectScalarVariable;

  ID3D10EffectVectorVariable=class;
  PID3D10EffectVectorVariable=^ID3D10EffectVectorVariable;

  ID3D10EffectMatrixVariable=class;
  PID3D10EffectMatrixVariable=^ID3D10EffectMatrixVariable;

  ID3D10EffectStringVariable=class;
  PID3D10EffectStringVariable=^ID3D10EffectStringVariable;

  ID3D10EffectShaderResourceVariable=class;
  PID3D10EffectShaderResourceVariable=^ID3D10EffectShaderResourceVariable;

  ID3D10EffectRenderTargetViewVariable=class;
  PID3D10EffectRenderTargetViewVariable=^ID3D10EffectRenderTargetViewVariable;

  ID3D10EffectDepthStencilViewVariable=class;
  PID3D10EffectDepthStencilViewVariable=^ID3D10EffectDepthStencilViewVariable;

  ID3D10EffectConstantBuffer=class;
  PID3D10EffectConstantBuffer=^ID3D10EffectConstantBuffer;

  ID3D10EffectShaderVariable=class;
  PID3D10EffectShaderVariable=^ID3D10EffectShaderVariable;

  ID3D10EffectBlendVariable=class;
  PID3D10EffectBlendVariable=^ID3D10EffectBlendVariable;

  ID3D10EffectDepthStencilVariable=class;
  PID3D10EffectDepthStencilVariable=^ID3D10EffectDepthStencilVariable;

  ID3D10EffectRasterizerVariable=class;
  PID3D10EffectRasterizerVariable=^ID3D10EffectRasterizerVariable;

  ID3D10EffectSamplerVariable=class;
  PID3D10EffectSamplerVariable=^ID3D10EffectSamplerVariable;

  ID3D10EffectVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

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

  ID3D10EffectScalarVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

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
      pData:PSingle;
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
      pData:PInteger;
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
      pData:PLongBool;
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

  ID3D10EffectVectorVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

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
      pData:PLongBool
    ):HResult; virtual; stdcall; abstract;

    function SetIntVector
    (
      pData:PInteger
    ):HResult; virtual; stdcall; abstract;

    function SetFloatVector
    (
      pData:PSingle
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
      pData:PLongBool;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetIntVectorArray
    (
      pData:PInteger;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetFloatVectorArray
    (
      pData:PSingle;
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

  ID3D10EffectMatrixVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

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
      pData:PSingle
    ):HResult; virtual; stdcall; abstract;

    function GetMatrix
    (
      pData:PSingle
    ):HResult; virtual; stdcall; abstract;

    function SetMatrixArray
    (
      pData:PSingle;
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
      pData:PSingle
    ):HResult; virtual; stdcall; abstract;

    function GetMatrixTranspose
    (
      pData:PSingle
    ):HResult; virtual; stdcall; abstract;

    function SetMatrixTransposeArray
    (
      pData:PSingle;
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

  ID3D10EffectStringVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

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

  ID3D10EffectShaderResourceVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

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
      Resource:ID3D10ShaderResourceView
    ):HResult; virtual; stdcall; abstract;

    function GetResource
    (
      out Resource:ID3D10ShaderResourceView
    ):HResult; virtual; stdcall; abstract;

    function SetResourceArray
    (
      pResources:PID3D10ShaderResourceView; (* __in_ecount(0) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetResourceArray
    (
      pResources:PID3D10ShaderResourceView; (* __in_ecount(0) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectRenderTargetViewVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

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
      Resource:ID3D10RenderTargetView
    ):HResult; virtual; stdcall; abstract;

    function GetRenderTarget
    (
      out Resource:ID3D10RenderTargetView
    ):HResult; virtual; stdcall; abstract;

    function SetRenderTargetArray
    (
      pResources:PID3D10RenderTargetView; (* __in_ecount(0) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRenderTargetArray
    (
      pResources:PID3D10RenderTargetView; (* __in_ecount(0) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectDepthStencilViewVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

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
      Resource:ID3D10DepthStencilView
    ):HResult; virtual; stdcall; abstract;

    function GetDepthStencil
    (
      out Resource:ID3D10DepthStencilView
    ):HResult; virtual; stdcall; abstract;

    function SetDepthStencilArray
    (
      out Resources:ID3D10DepthStencilView;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetDepthStencilArray
    (
      out Resources:ID3D10DepthStencilView;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectConstantBuffer=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

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
      ConstantBuffer:ID3D10Buffer
    ):HResult; virtual; stdcall; abstract;

    function GetConstantBuffer
    (
      out ConstantBuffer:ID3D10Buffer
    ):HResult; virtual; stdcall; abstract;

    function SetTextureBuffer
    (
      TextureBuffer:ID3D10ShaderResourceView
    ):HResult; virtual; stdcall; abstract;

    function GetTextureBuffer
    (
      out TextureBuffer:ID3D10ShaderResourceView
    ):HResult; virtual; stdcall; abstract;
  end;

  TD3D10_EffectShaderDesc=record
    pInputSignature:PByte;
    IsInline:LongBool;
    pBytecode:PByte;
    BytecodeLength:LongWord;
    SODecl:PAnsiChar;
    NumInputSignatureEntries:LongWord;
    NumOutputSignatureEntries:LongWord;
  end;
  PTD3D10_EffectShaderDesc=^TD3D10_EffectShaderDesc;
  D3D10_EFFECT_SHADER_DESC=TD3D10_EffectShaderDesc;
  PD3D10_EFFECT_SHADER_DESC=^TD3D10_EffectShaderDesc;

  ID3D10EffectShaderVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

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
      out Desc:TD3D10_EffectShaderDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetVertexShader
    (
      ShaderIndex:LongWord;
      out VS:ID3D10VertexShader
    ):HResult; virtual; stdcall; abstract;

    function GetGeometryShader
    (
      ShaderIndex:LongWord;
      out GS:ID3D10GeometryShader
    ):HResult; virtual; stdcall; abstract;

    function GetPixelShader
    (
      ShaderIndex:LongWord;
      out PS:ID3D10PixelShader
    ):HResult; virtual; stdcall; abstract;

    function GetInputSignatureElementDesc
    (
      ShaderIndex:LongWord;
      Element:LongWord;
      pDesc:PTD3D10_SignatureParameterDesc
    ):HResult; virtual; stdcall; abstract;

    function GetOutputSignatureElementDesc
    (
      ShaderIndex:LongWord;
      Element:LongWord;
      pDesc:PTD3D10_SignatureParameterDesc
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectBlendVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

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
      out BlendState:ID3D10BlendState
    ):HResult; virtual; stdcall; abstract;

    function GetBackingStore
    (
      Index:LongWord;
      pBlendDesc:PTD3D10_BlendDesc
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectDepthStencilVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

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
      out DepthStencilState:ID3D10DepthStencilState
    ):HResult; virtual; stdcall; abstract;

    function GetBackingStore
    (
      Index:LongWord;
      pDepthStencilDesc:PTD3D10_DepthStencilDesc
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectRasterizerVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

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
      out RasterizerState:ID3D10RasterizerState
    ):HResult; virtual; stdcall; abstract;

    function GetBackingStore
    (
      Index:LongWord;
      pRasterizerDesc:PTD3D10_RasterizerDesc
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectSamplerVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

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
      out Sampler:ID3D10SamplerState
    ):HResult; virtual; stdcall; abstract;

    function GetBackingStore
    (
      Index:LongWord;
      pSamplerDesc:PTD3D10_SamplerDesc
    ):HResult; virtual; stdcall; abstract;
  end;

  TD3D10_PassDesc=record
    Name:PAnsiChar;
    Annotations:LongWord;
    pIAInputSignature:PByte;
    IAInputSignatureSize:SIZE_T;
    StencilRef:LongWord;
    SampleMask:LongWord;
    BlendFactor:array[0..3] of Single;
  end;
  PTD3D10_PassDesc=^TD3D10_PassDesc;
  D3D10_PASS_DESC=TD3D10_PassDesc;
  PD3D10_PASS_DESC=^TD3D10_PassDesc;

  TD3D10_PassShaderDesc=record
    pShaderVariable:ID3D10EffectShaderVariable;
    ShaderIndex:LongWord;
  end;
  PTD3D10_PassShaderDesc=^TD3D10_PassShaderDesc;
  D3D10_PASS_SHADER_DESC=TD3D10_PassShaderDesc;
  PD3D10_PASS_SHADER_DESC=^TD3D10_PassShaderDesc;

  ID3D10EffectPass=class;
  PID3D10EffectPass=^ID3D10EffectPass;

  ID3D10EffectPass=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_PassDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetVertexShaderDesc
    (
      out Desc:TD3D10_PassShaderDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetGeometryShaderDesc
    (
      out Desc:TD3D10_PassShaderDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetPixelShaderDesc
    (
      out Desc:TD3D10_PassShaderDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function Apply
    (
      Flags:LongWord
    ):HResult; virtual; stdcall; abstract;

    function ComputeStateBlockMask
    (
      out StateBlockMask:TD3D10_StateBlockMask (* __out *)
    ):HResult; virtual; stdcall; abstract;
  end;

  TD3D10_TechniqueDesc=record
    Name:PAnsiChar;
    Passes:LongWord;
    Annotations:LongWord;
  end;
  PTD3D10_TechniqueDesc=^TD3D10_TechniqueDesc;
  D3D10_TECHNIQUE_DESC=TD3D10_TechniqueDesc;
  PD3D10_TECHNIQUE_DESC=^TD3D10_TechniqueDesc;

  ID3D10EffectTechnique=class;
  PID3D10EffectTechnique=^ID3D10EffectTechnique;

  ID3D10EffectTechnique=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_TechniqueDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetPassByIndex
    (
      Index:LongWord
    ):ID3D10EffectPass; virtual; stdcall; abstract;

    function GetPassByName
    (
      Name:PAnsiChar
    ):ID3D10EffectPass; virtual; stdcall; abstract;

    function ComputeStateBlockMask
    (
      out StateBlockMask:TD3D10_StateBlockMask (* __out *)
    ):HResult; virtual; stdcall; abstract;
  end;

  TD3D10_EffectDesc=record
    IsChildEffect:LongBool;
    ConstantBuffers:LongWord;
    SharedConstantBuffers:LongWord;
    GlobalVariables:LongWord;
    SharedGlobalVariables:LongWord;
    Techniques:LongWord;
  end;
  PTD3D10_EffectDesc=^TD3D10_EffectDesc;
  D3D10_EFFECT_DESC=TD3D10_EffectDesc;
  PD3D10_EFFECT_DESC=^TD3D10_EffectDesc;

  ID3D10Effect=interface;
  PID3D10Effect=^ID3D10Effect;

  ID3D10Effect=interface(IUnknown)
    ['{51B0CA8B-EC0B-4519-870D-8EE1CB5017C7}']
    function IsValid:LongBool; stdcall;

    function IsPool:LongBool; stdcall;

    function GetDevice
    (
      out Device:ID3D10Device
    ):HResult; stdcall;

    function GetDesc
    (
      out Desc:TD3D10_EffectDesc (* __out *)
    ):HResult; stdcall;

    function GetConstantBufferByIndex
    (
      Index:LongWord
    ):ID3D10EffectConstantBuffer; stdcall;

    function GetConstantBufferByName
    (
      Name:PAnsiChar
    ):ID3D10EffectConstantBuffer; stdcall;

    function GetVariableByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; stdcall;

    function GetVariableByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; stdcall;

    function GetVariableBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; stdcall;

    function GetTechniqueByIndex
    (
      Index:LongWord
    ):ID3D10EffectTechnique; stdcall;

    function GetTechniqueByName
    (
      Name:PAnsiChar
    ):ID3D10EffectTechnique; stdcall;

    function Optimize:HResult; stdcall;

    function IsOptimized:LongBool; stdcall;
  end;

  ID3D10EffectPool=interface;
  PID3D10EffectPool=^ID3D10EffectPool;

  ID3D10EffectPool=interface(IUnknown)
    ['{9537AB04-3250-412E-8213-FCD2F8677933}']
    function AsEffect:ID3D10Effect; stdcall;
  end;

{$IFDEF UseRuntimeLinking}var D3D10CompileEffectFromMemory:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10CompileEffectFromMemory{$ENDIF}(pData:Pointer;DataLength:SIZE_T;pSrcFileName:PAnsiChar;pDefines:PTD3D10_ShaderMacro;Include:ID3D10Include;HLSLFlags:LongWord;FXFlags:LongWord;out CompiledEffect:ID3D10Blob;out Errors:ID3D10Blob):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3D10CreateEffectFromMemory:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10CreateEffectFromMemory{$ENDIF}(pData:Pointer;DataLength:SIZE_T;FXFlags:LongWord;Device:ID3D10Device;EffectPool:ID3D10EffectPool;out Effect:ID3D10Effect):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3D10CreateEffectPoolFromMemory:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10CreateEffectPoolFromMemory{$ENDIF}(pData:Pointer;DataLength:SIZE_T;FXFlags:LongWord;Device:ID3D10Device;out EffectPool:ID3D10EffectPool):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}
{$IFDEF UseRuntimeLinking}var D3D10DisassembleEffect:{$ENDIF}function{$IFNDEF UseRuntimeLinking}D3D10DisassembleEffect{$ENDIF}(Effect:ID3D10Effect;EnableColorCode:LongBool;out Disassembly:ID3D10Blob):HResult; stdcall; {$IFNDEF UseRuntimeLinking}external DLL_D3D10;{$ENDIF}

///////////////////////////////////////////////////////////////////////////////
// End "D3D10Effect.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D3D10SDKLayers.h"
///////////////////////////////////////////////////////////////////////////////

const
  D3D10_SDK_LAYERS_VERSION=11;
  D3D10_DEBUG_FEATURE_FLUSH_PER_RENDER_OP=$1;
  D3D10_DEBUG_FEATURE_FINISH_PER_RENDER_OP=$2;
  D3D10_DEBUG_FEATURE_PRESENT_PER_RENDER_OP=$4;
  D3D10_INFO_QUEUE_DEFAULT_MESSAGE_COUNT_LIMIT=1024;
  D3D10_REGKEY_PATH='Software\Microsoft\Direct3D';
  D3D10_MUTE_DEBUG_OUTPUT='MuteDebugOutput';
  D3D10_ENABLE_BREAK_ON_MESSAGE='EnableBreakOnMessage';
  D3D10_INFOQUEUE_STORAGE_FILTER_OVERRIDE='InfoQueueStorageFilterOverride';
  D3D10_MUTE_CATEGORY='Mute_CATEGORY_%s';
  D3D10_MUTE_SEVERITY='Mute_SEVERITY_%s';
  D3D10_MUTE_ID_STRING='Mute_ID_%s';
  D3D10_MUTE_ID_DECIMAL='Mute_ID_%d';
  D3D10_UNMUTE_SEVERITY_INFO='Unmute_SEVERITY_INFO';
  D3D10_BREAKON_CATEGORY='BreakOn_CATEGORY_%s';
  D3D10_BREAKON_SEVERITY='BreakOn_SEVERITY_%s';
  D3D10_BREAKON_ID_STRING='BreakOn_ID_%s';
  D3D10_BREAKON_ID_DECIMAL='BreakOn_ID_%d';
  D3D10_APPSIZE_STRING='Size';
  D3D10_APPNAME_STRING='Name';

type
  ID3D10Debug=interface;
  PID3D10Debug=^ID3D10Debug;

  ID3D10SwitchToRef=interface;
  PID3D10SwitchToRef=^ID3D10SwitchToRef;

  ID3D10InfoQueue=interface;
  PID3D10InfoQueue=^ID3D10InfoQueue;

  ID3D10Debug=interface(IUnknown)
    ['{9B7E4E01-342C-4106-A19F-4F2704F689F0}']
    function SetFeatureMask
    (
      Mask:LongWord
    ):HResult; stdcall;

    function GetFeatureMask:LongWord; stdcall;

    function SetPresentPerRenderOpDelay
    (
      Milliseconds:LongWord
    ):HResult; stdcall;

    function GetPresentPerRenderOpDelay:LongWord; stdcall;

    function SetSwapChain
    (
      SwapChain:IDXGISwapChain (* __in_opt *)
    ):HResult; stdcall;

    function GetSwapChain
    (
      out SwapChain:IDXGISwapChain (* __out *)
    ):HResult; stdcall;

    function Validate:HResult; stdcall;
  end;

  ID3D10SwitchToRef=interface(IUnknown)
    ['{9B7E4E02-342C-4106-A19F-4F2704F689F0}']
    function SetUseRef(UseRef:LongBool):LongBool; stdcall;
    function GetUseRef:LongBool; stdcall;
  end;

  TD3D10_MessageCategory=
  (
    D3D10_MESSAGE_CATEGORY_APPLICATION_DEFINED=0,
    D3D10_MESSAGE_CATEGORY_MISCELLANEOUS=D3D10_MESSAGE_CATEGORY_APPLICATION_DEFINED + 1,
    D3D10_MESSAGE_CATEGORY_INITIALIZATION=D3D10_MESSAGE_CATEGORY_MISCELLANEOUS + 1,
    D3D10_MESSAGE_CATEGORY_CLEANUP=D3D10_MESSAGE_CATEGORY_INITIALIZATION + 1,
    D3D10_MESSAGE_CATEGORY_COMPILATION=D3D10_MESSAGE_CATEGORY_CLEANUP + 1,
    D3D10_MESSAGE_CATEGORY_STATE_CREATION=D3D10_MESSAGE_CATEGORY_COMPILATION + 1,
    D3D10_MESSAGE_CATEGORY_STATE_SETTING=D3D10_MESSAGE_CATEGORY_STATE_CREATION + 1,
    D3D10_MESSAGE_CATEGORY_STATE_GETTING=D3D10_MESSAGE_CATEGORY_STATE_SETTING + 1,
    D3D10_MESSAGE_CATEGORY_RESOURCE_MANIPULATION=D3D10_MESSAGE_CATEGORY_STATE_GETTING + 1,
    D3D10_MESSAGE_CATEGORY_EXECUTION=D3D10_MESSAGE_CATEGORY_RESOURCE_MANIPULATION + 1
  );
  PTD3D10_MessageCategory=^TD3D10_MessageCategory;
  D3D10_MESSAGE_CATEGORY=TD3D10_MessageCategory;
  PD3D10_MESSAGE_CATEGORY=^TD3D10_MessageCategory;

  TD3D10_MessageSeverity=
  (
    D3D10_MESSAGE_SEVERITY_CORRUPTION=0,
    D3D10_MESSAGE_SEVERITY_ERROR=D3D10_MESSAGE_SEVERITY_CORRUPTION + 1,
    D3D10_MESSAGE_SEVERITY_WARNING=D3D10_MESSAGE_SEVERITY_ERROR + 1,
    D3D10_MESSAGE_SEVERITY_INFO=D3D10_MESSAGE_SEVERITY_WARNING + 1
  );
  PTD3D10_MessageSeverity=^TD3D10_MessageSeverity;
  D3D10_MESSAGE_SEVERITY=TD3D10_MessageSeverity;
  PD3D10_MESSAGE_SEVERITY=^TD3D10_MessageSeverity;

  TD3D10_MessageID=
  (
    D3D10_MESSAGE_ID_UNKNOWN=0,
    D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_HAZARD=D3D10_MESSAGE_ID_UNKNOWN + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETINDEXBUFFER_HAZARD=D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_HAZARD + 1,
    D3D10_MESSAGE_ID_DEVICE_VSSETSHADERRESOURCES_HAZARD=D3D10_MESSAGE_ID_DEVICE_IASETINDEXBUFFER_HAZARD + 1,
    D3D10_MESSAGE_ID_DEVICE_VSSETCONSTANTBUFFERS_HAZARD=D3D10_MESSAGE_ID_DEVICE_VSSETSHADERRESOURCES_HAZARD + 1,
    D3D10_MESSAGE_ID_DEVICE_GSSETSHADERRESOURCES_HAZARD=D3D10_MESSAGE_ID_DEVICE_VSSETCONSTANTBUFFERS_HAZARD + 1,
    D3D10_MESSAGE_ID_DEVICE_GSSETCONSTANTBUFFERS_HAZARD=D3D10_MESSAGE_ID_DEVICE_GSSETSHADERRESOURCES_HAZARD + 1,
    D3D10_MESSAGE_ID_DEVICE_PSSETSHADERRESOURCES_HAZARD=D3D10_MESSAGE_ID_DEVICE_GSSETCONSTANTBUFFERS_HAZARD + 1,
    D3D10_MESSAGE_ID_DEVICE_PSSETCONSTANTBUFFERS_HAZARD=D3D10_MESSAGE_ID_DEVICE_PSSETSHADERRESOURCES_HAZARD + 1,
    D3D10_MESSAGE_ID_DEVICE_OMSETRENDERTARGETS_HAZARD=D3D10_MESSAGE_ID_DEVICE_PSSETCONSTANTBUFFERS_HAZARD + 1,
    D3D10_MESSAGE_ID_DEVICE_SOSETTARGETS_HAZARD=D3D10_MESSAGE_ID_DEVICE_OMSETRENDERTARGETS_HAZARD + 1,
    D3D10_MESSAGE_ID_STRING_FROM_APPLICATION=D3D10_MESSAGE_ID_DEVICE_SOSETTARGETS_HAZARD + 1,
    D3D10_MESSAGE_ID_CORRUPTED_THIS=D3D10_MESSAGE_ID_STRING_FROM_APPLICATION + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER1=D3D10_MESSAGE_ID_CORRUPTED_THIS + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER2=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER1 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER3=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER2 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER4=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER3 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER5=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER4 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER6=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER5 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER7=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER6 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER8=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER7 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER9=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER8 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER10=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER9 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER11=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER10 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER12=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER11 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER13=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER12 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER14=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER13 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER15=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER14 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_MULTITHREADING=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER15 + 1,
    D3D10_MESSAGE_ID_MESSAGE_REPORTING_OUTOFMEMORY=D3D10_MESSAGE_ID_CORRUPTED_MULTITHREADING + 1,
    D3D10_MESSAGE_ID_IASETINPUTLAYOUT_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_MESSAGE_REPORTING_OUTOFMEMORY + 1,
    D3D10_MESSAGE_ID_IASETVERTEXBUFFERS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_IASETINPUTLAYOUT_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_IASETINDEXBUFFER_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_IASETVERTEXBUFFERS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_VSSETSHADER_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_IASETINDEXBUFFER_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_VSSETSHADERRESOURCES_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_VSSETSHADER_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_VSSETCONSTANTBUFFERS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_VSSETSHADERRESOURCES_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_VSSETSAMPLERS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_VSSETCONSTANTBUFFERS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_GSSETSHADER_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_VSSETSAMPLERS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_GSSETSHADERRESOURCES_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_GSSETSHADER_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_GSSETCONSTANTBUFFERS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_GSSETSHADERRESOURCES_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_GSSETSAMPLERS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_GSSETCONSTANTBUFFERS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_SOSETTARGETS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_GSSETSAMPLERS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_PSSETSHADER_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_SOSETTARGETS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_PSSETSHADERRESOURCES_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_PSSETSHADER_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_PSSETCONSTANTBUFFERS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_PSSETSHADERRESOURCES_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_PSSETSAMPLERS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_PSSETCONSTANTBUFFERS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_RSSETSTATE_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_PSSETSAMPLERS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_OMSETBLENDSTATE_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_RSSETSTATE_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_OMSETDEPTHSTENCILSTATE_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_OMSETBLENDSTATE_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_OMSETRENDERTARGETS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_OMSETDEPTHSTENCILSTATE_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_SETPREDICATION_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_OMSETRENDERTARGETS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_GETPRIVATEDATA_MOREDATA=D3D10_MESSAGE_ID_SETPREDICATION_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_SETPRIVATEDATA_INVALIDFREEDATA=D3D10_MESSAGE_ID_GETPRIVATEDATA_MOREDATA + 1,
    D3D10_MESSAGE_ID_SETPRIVATEDATA_INVALIDIUNKNOWN=D3D10_MESSAGE_ID_SETPRIVATEDATA_INVALIDFREEDATA + 1,
    D3D10_MESSAGE_ID_SETPRIVATEDATA_INVALIDFLAGS=D3D10_MESSAGE_ID_SETPRIVATEDATA_INVALIDIUNKNOWN + 1,
    D3D10_MESSAGE_ID_SETPRIVATEDATA_CHANGINGPARAMS=D3D10_MESSAGE_ID_SETPRIVATEDATA_INVALIDFLAGS + 1,
    D3D10_MESSAGE_ID_SETPRIVATEDATA_OUTOFMEMORY=D3D10_MESSAGE_ID_SETPRIVATEDATA_CHANGINGPARAMS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDFORMAT=D3D10_MESSAGE_ID_SETPRIVATEDATA_OUTOFMEMORY + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDSAMPLES=D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDUSAGE=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDSAMPLES + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDBINDFLAGS=D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDUSAGE + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDCPUACCESSFLAGS=D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDBINDFLAGS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDMISCFLAGS=D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDCPUACCESSFLAGS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDCPUACCESSFLAGS=D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDBINDFLAGS=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDCPUACCESSFLAGS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDINITIALDATA=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDBINDFLAGS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDDIMENSIONS=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDINITIALDATA + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDMIPLEVELS=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDDIMENSIONS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDMISCFLAGS=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDMIPLEVELS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDARG_RETURN=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_NULLDESC=D3D10_MESSAGE_ID_CREATEBUFFER_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDCONSTANTBUFFERBINDINGS=D3D10_MESSAGE_ID_CREATEBUFFER_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_LARGEALLOCATION=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDCONSTANTBUFFERBINDINGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDFORMAT=D3D10_MESSAGE_ID_CREATEBUFFER_LARGEALLOCATION + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_UNSUPPORTEDFORMAT=D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDSAMPLES=D3D10_MESSAGE_ID_CREATETEXTURE1D_UNSUPPORTEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDUSAGE=D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDSAMPLES + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDBINDFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDUSAGE + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDCPUACCESSFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDBINDFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDMISCFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDCPUACCESSFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDCPUACCESSFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDBINDFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDCPUACCESSFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDINITIALDATA=D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDBINDFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDDIMENSIONS=D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDINITIALDATA + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDMIPLEVELS=D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDDIMENSIONS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDMISCFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDMIPLEVELS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDARG_RETURN=D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_NULLDESC=D3D10_MESSAGE_ID_CREATETEXTURE1D_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_LARGEALLOCATION=D3D10_MESSAGE_ID_CREATETEXTURE1D_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDFORMAT=D3D10_MESSAGE_ID_CREATETEXTURE1D_LARGEALLOCATION + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_UNSUPPORTEDFORMAT=D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDSAMPLES=D3D10_MESSAGE_ID_CREATETEXTURE2D_UNSUPPORTEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDUSAGE=D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDSAMPLES + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDBINDFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDUSAGE + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDCPUACCESSFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDBINDFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDMISCFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDCPUACCESSFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDCPUACCESSFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDBINDFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDCPUACCESSFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDINITIALDATA=D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDBINDFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDDIMENSIONS=D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDINITIALDATA + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDMIPLEVELS=D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDDIMENSIONS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDMISCFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDMIPLEVELS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDARG_RETURN=D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_NULLDESC=D3D10_MESSAGE_ID_CREATETEXTURE2D_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_LARGEALLOCATION=D3D10_MESSAGE_ID_CREATETEXTURE2D_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDFORMAT=D3D10_MESSAGE_ID_CREATETEXTURE2D_LARGEALLOCATION + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_UNSUPPORTEDFORMAT=D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDSAMPLES=D3D10_MESSAGE_ID_CREATETEXTURE3D_UNSUPPORTEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDUSAGE=D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDSAMPLES + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDBINDFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDUSAGE + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDCPUACCESSFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDBINDFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDMISCFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDCPUACCESSFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDCPUACCESSFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDBINDFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDCPUACCESSFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDINITIALDATA=D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDBINDFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDDIMENSIONS=D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDINITIALDATA + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDMIPLEVELS=D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDDIMENSIONS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDMISCFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDMIPLEVELS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDARG_RETURN=D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_NULLDESC=D3D10_MESSAGE_ID_CREATETEXTURE3D_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_LARGEALLOCATION=D3D10_MESSAGE_ID_CREATETEXTURE3D_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_UNRECOGNIZEDFORMAT=D3D10_MESSAGE_ID_CREATETEXTURE3D_LARGEALLOCATION + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDDESC=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_UNRECOGNIZEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDFORMAT=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDDESC + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDDIMENSIONS=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDRESOURCE=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDDIMENSIONS + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_TOOMANYOBJECTS=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDRESOURCE + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDARG_RETURN=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_TOOMANYOBJECTS + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_UNRECOGNIZEDFORMAT=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_UNSUPPORTEDFORMAT=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_UNRECOGNIZEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDDESC=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_UNSUPPORTEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDFORMAT=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDDESC + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDDIMENSIONS=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDRESOURCE=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDDIMENSIONS + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_TOOMANYOBJECTS=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDRESOURCE + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDARG_RETURN=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_TOOMANYOBJECTS + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_UNRECOGNIZEDFORMAT=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDDESC=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_UNRECOGNIZEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDFORMAT=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDDESC + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDDIMENSIONS=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDRESOURCE=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDDIMENSIONS + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_TOOMANYOBJECTS=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDRESOURCE + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDARG_RETURN=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_TOOMANYOBJECTS + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_OUTOFMEMORY=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_TOOMANYELEMENTS=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_OUTOFMEMORY + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDFORMAT=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_TOOMANYELEMENTS + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INCOMPATIBLEFORMAT=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDSLOT=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INCOMPATIBLEFORMAT + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDINPUTSLOTCLASS=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDSLOT + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_STEPRATESLOTCLASSMISMATCH=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDINPUTSLOTCLASS + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDSLOTCLASSCHANGE=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_STEPRATESLOTCLASSMISMATCH + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDSTEPRATECHANGE=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDSLOTCLASSCHANGE + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDALIGNMENT=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDSTEPRATECHANGE + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_DUPLICATESEMANTIC=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDALIGNMENT + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_UNPARSEABLEINPUTSIGNATURE=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_DUPLICATESEMANTIC + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_NULLSEMANTIC=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_UNPARSEABLEINPUTSIGNATURE + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_MISSINGELEMENT=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_NULLSEMANTIC + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_NULLDESC=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_MISSINGELEMENT + 1,
    D3D10_MESSAGE_ID_CREATEVERTEXSHADER_OUTOFMEMORY=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATEVERTEXSHADER_INVALIDSHADERBYTECODE=D3D10_MESSAGE_ID_CREATEVERTEXSHADER_OUTOFMEMORY + 1,
    D3D10_MESSAGE_ID_CREATEVERTEXSHADER_INVALIDSHADERTYPE=D3D10_MESSAGE_ID_CREATEVERTEXSHADER_INVALIDSHADERBYTECODE + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADER_OUTOFMEMORY=D3D10_MESSAGE_ID_CREATEVERTEXSHADER_INVALIDSHADERTYPE + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADER_INVALIDSHADERBYTECODE=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADER_OUTOFMEMORY + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADER_INVALIDSHADERTYPE=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADER_INVALIDSHADERBYTECODE + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_OUTOFMEMORY=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADER_INVALIDSHADERTYPE + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDSHADERBYTECODE=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_OUTOFMEMORY + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDSHADERTYPE=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDSHADERBYTECODE + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDNUMENTRIES=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDSHADERTYPE + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_OUTPUTSTREAMSTRIDEUNUSED=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDNUMENTRIES + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_UNEXPECTEDDECL=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_OUTPUTSTREAMSTRIDEUNUSED + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_EXPECTEDDECL=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_UNEXPECTEDDECL + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_OUTPUTSLOT0EXPECTED=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_EXPECTEDDECL + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDOUTPUTSLOT=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_OUTPUTSLOT0EXPECTED + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_ONLYONEELEMENTPERSLOT=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDOUTPUTSLOT + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDCOMPONENTCOUNT=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_ONLYONEELEMENTPERSLOT + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDSTARTCOMPONENTANDCOMPONENTCOUNT=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDCOMPONENTCOUNT + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDGAPDEFINITION=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDSTARTCOMPONENTANDCOMPONENTCOUNT + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_REPEATEDOUTPUT=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDGAPDEFINITION + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDOUTPUTSTREAMSTRIDE=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_REPEATEDOUTPUT + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_MISSINGSEMANTIC=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDOUTPUTSTREAMSTRIDE + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_MASKMISMATCH=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_MISSINGSEMANTIC + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_CANTHAVEONLYGAPS=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_MASKMISMATCH + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_DECLTOOCOMPLEX=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_CANTHAVEONLYGAPS + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_MISSINGOUTPUTSIGNATURE=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_DECLTOOCOMPLEX + 1,
    D3D10_MESSAGE_ID_CREATEPIXELSHADER_OUTOFMEMORY=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_MISSINGOUTPUTSIGNATURE + 1,
    D3D10_MESSAGE_ID_CREATEPIXELSHADER_INVALIDSHADERBYTECODE=D3D10_MESSAGE_ID_CREATEPIXELSHADER_OUTOFMEMORY + 1,
    D3D10_MESSAGE_ID_CREATEPIXELSHADER_INVALIDSHADERTYPE=D3D10_MESSAGE_ID_CREATEPIXELSHADER_INVALIDSHADERBYTECODE + 1,
    D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_INVALIDFILLMODE=D3D10_MESSAGE_ID_CREATEPIXELSHADER_INVALIDSHADERTYPE + 1,
    D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_INVALIDCULLMODE=D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_INVALIDFILLMODE + 1,
    D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_INVALIDDEPTHBIASCLAMP=D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_INVALIDCULLMODE + 1,
    D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_INVALIDSLOPESCALEDDEPTHBIAS=D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_INVALIDDEPTHBIASCLAMP + 1,
    D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_TOOMANYOBJECTS=D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_INVALIDSLOPESCALEDDEPTHBIAS + 1,
    D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_NULLDESC=D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_TOOMANYOBJECTS + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDDEPTHWRITEMASK=D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDDEPTHFUNC=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDDEPTHWRITEMASK + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDFRONTFACESTENCILFAILOP=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDDEPTHFUNC + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDFRONTFACESTENCILZFAILOP=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDFRONTFACESTENCILFAILOP + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDFRONTFACESTENCILPASSOP=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDFRONTFACESTENCILZFAILOP + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDFRONTFACESTENCILFUNC=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDFRONTFACESTENCILPASSOP + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDBACKFACESTENCILFAILOP=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDFRONTFACESTENCILFUNC + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDBACKFACESTENCILZFAILOP=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDBACKFACESTENCILFAILOP + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDBACKFACESTENCILPASSOP=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDBACKFACESTENCILZFAILOP + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDBACKFACESTENCILFUNC=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDBACKFACESTENCILPASSOP + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_TOOMANYOBJECTS=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDBACKFACESTENCILFUNC + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_NULLDESC=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_TOOMANYOBJECTS + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDSRCBLEND=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDDESTBLEND=D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDSRCBLEND + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDBLENDOP=D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDDESTBLEND + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDSRCBLENDALPHA=D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDBLENDOP + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDDESTBLENDALPHA=D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDSRCBLENDALPHA + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDBLENDOPALPHA=D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDDESTBLENDALPHA + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDRENDERTARGETWRITEMASK=D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDBLENDOPALPHA + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_TOOMANYOBJECTS=D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDRENDERTARGETWRITEMASK + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_NULLDESC=D3D10_MESSAGE_ID_CREATEBLENDSTATE_TOOMANYOBJECTS + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDFILTER=D3D10_MESSAGE_ID_CREATEBLENDSTATE_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDADDRESSU=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDFILTER + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDADDRESSV=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDADDRESSU + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDADDRESSW=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDADDRESSV + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDMIPLODBIAS=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDADDRESSW + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDMAXANISOTROPY=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDMIPLODBIAS + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDCOMPARISONFUNC=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDMAXANISOTROPY + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDMINLOD=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDCOMPARISONFUNC + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDMAXLOD=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDMINLOD + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_TOOMANYOBJECTS=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDMAXLOD + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_NULLDESC=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_TOOMANYOBJECTS + 1,
    D3D10_MESSAGE_ID_CREATEQUERYORPREDICATE_INVALIDQUERY=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATEQUERYORPREDICATE_INVALIDMISCFLAGS=D3D10_MESSAGE_ID_CREATEQUERYORPREDICATE_INVALIDQUERY + 1,
    D3D10_MESSAGE_ID_CREATEQUERYORPREDICATE_UNEXPECTEDMISCFLAG=D3D10_MESSAGE_ID_CREATEQUERYORPREDICATE_INVALIDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATEQUERYORPREDICATE_NULLDESC=D3D10_MESSAGE_ID_CREATEQUERYORPREDICATE_UNEXPECTEDMISCFLAG + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETPRIMITIVETOPOLOGY_TOPOLOGY_UNRECOGNIZED=D3D10_MESSAGE_ID_CREATEQUERYORPREDICATE_NULLDESC + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETPRIMITIVETOPOLOGY_TOPOLOGY_UNDEFINED=D3D10_MESSAGE_ID_DEVICE_IASETPRIMITIVETOPOLOGY_TOPOLOGY_UNRECOGNIZED + 1,
    D3D10_MESSAGE_ID_IASETVERTEXBUFFERS_INVALIDBUFFER=D3D10_MESSAGE_ID_DEVICE_IASETPRIMITIVETOPOLOGY_TOPOLOGY_UNDEFINED + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_OFFSET_TOO_LARGE=D3D10_MESSAGE_ID_IASETVERTEXBUFFERS_INVALIDBUFFER + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_OFFSET_TOO_LARGE + 1,
    D3D10_MESSAGE_ID_IASETINDEXBUFFER_INVALIDBUFFER=D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETINDEXBUFFER_FORMAT_INVALID=D3D10_MESSAGE_ID_IASETINDEXBUFFER_INVALIDBUFFER + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETINDEXBUFFER_OFFSET_TOO_LARGE=D3D10_MESSAGE_ID_DEVICE_IASETINDEXBUFFER_FORMAT_INVALID + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETINDEXBUFFER_OFFSET_UNALIGNED=D3D10_MESSAGE_ID_DEVICE_IASETINDEXBUFFER_OFFSET_TOO_LARGE + 1,
    D3D10_MESSAGE_ID_DEVICE_VSSETSHADERRESOURCES_VIEWS_EMPTY=D3D10_MESSAGE_ID_DEVICE_IASETINDEXBUFFER_OFFSET_UNALIGNED + 1,
    D3D10_MESSAGE_ID_VSSETCONSTANTBUFFERS_INVALIDBUFFER=D3D10_MESSAGE_ID_DEVICE_VSSETSHADERRESOURCES_VIEWS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_VSSETCONSTANTBUFFERS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_VSSETCONSTANTBUFFERS_INVALIDBUFFER + 1,
    D3D10_MESSAGE_ID_DEVICE_VSSETSAMPLERS_SAMPLERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_VSSETCONSTANTBUFFERS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_GSSETSHADERRESOURCES_VIEWS_EMPTY=D3D10_MESSAGE_ID_DEVICE_VSSETSAMPLERS_SAMPLERS_EMPTY + 1,
    D3D10_MESSAGE_ID_GSSETCONSTANTBUFFERS_INVALIDBUFFER=D3D10_MESSAGE_ID_DEVICE_GSSETSHADERRESOURCES_VIEWS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_GSSETCONSTANTBUFFERS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_GSSETCONSTANTBUFFERS_INVALIDBUFFER + 1,
    D3D10_MESSAGE_ID_DEVICE_GSSETSAMPLERS_SAMPLERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_GSSETCONSTANTBUFFERS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_SOSETTARGETS_INVALIDBUFFER=D3D10_MESSAGE_ID_DEVICE_GSSETSAMPLERS_SAMPLERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_SOSETTARGETS_OFFSET_UNALIGNED=D3D10_MESSAGE_ID_SOSETTARGETS_INVALIDBUFFER + 1,
    D3D10_MESSAGE_ID_DEVICE_PSSETSHADERRESOURCES_VIEWS_EMPTY=D3D10_MESSAGE_ID_DEVICE_SOSETTARGETS_OFFSET_UNALIGNED + 1,
    D3D10_MESSAGE_ID_PSSETCONSTANTBUFFERS_INVALIDBUFFER=D3D10_MESSAGE_ID_DEVICE_PSSETSHADERRESOURCES_VIEWS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_PSSETCONSTANTBUFFERS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_PSSETCONSTANTBUFFERS_INVALIDBUFFER + 1,
    D3D10_MESSAGE_ID_DEVICE_PSSETSAMPLERS_SAMPLERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_PSSETCONSTANTBUFFERS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_RSSETVIEWPORTS_INVALIDVIEWPORT=D3D10_MESSAGE_ID_DEVICE_PSSETSAMPLERS_SAMPLERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_RSSETSCISSORRECTS_INVALIDSCISSOR=D3D10_MESSAGE_ID_DEVICE_RSSETVIEWPORTS_INVALIDVIEWPORT + 1,
    D3D10_MESSAGE_ID_CLEARRENDERTARGETVIEW_DENORMFLUSH=D3D10_MESSAGE_ID_DEVICE_RSSETSCISSORRECTS_INVALIDSCISSOR + 1,
    D3D10_MESSAGE_ID_CLEARDEPTHSTENCILVIEW_DENORMFLUSH=D3D10_MESSAGE_ID_CLEARRENDERTARGETVIEW_DENORMFLUSH + 1,
    D3D10_MESSAGE_ID_CLEARDEPTHSTENCILVIEW_INVALID=D3D10_MESSAGE_ID_CLEARDEPTHSTENCILVIEW_DENORMFLUSH + 1,
    D3D10_MESSAGE_ID_DEVICE_IAGETVERTEXBUFFERS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_CLEARDEPTHSTENCILVIEW_INVALID + 1,
    D3D10_MESSAGE_ID_DEVICE_VSGETSHADERRESOURCES_VIEWS_EMPTY=D3D10_MESSAGE_ID_DEVICE_IAGETVERTEXBUFFERS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_VSGETCONSTANTBUFFERS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_VSGETSHADERRESOURCES_VIEWS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_VSGETSAMPLERS_SAMPLERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_VSGETCONSTANTBUFFERS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_GSGETSHADERRESOURCES_VIEWS_EMPTY=D3D10_MESSAGE_ID_DEVICE_VSGETSAMPLERS_SAMPLERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_GSGETCONSTANTBUFFERS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_GSGETSHADERRESOURCES_VIEWS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_GSGETSAMPLERS_SAMPLERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_GSGETCONSTANTBUFFERS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_SOGETTARGETS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_GSGETSAMPLERS_SAMPLERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_PSGETSHADERRESOURCES_VIEWS_EMPTY=D3D10_MESSAGE_ID_DEVICE_SOGETTARGETS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_PSGETCONSTANTBUFFERS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_PSGETSHADERRESOURCES_VIEWS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_PSGETSAMPLERS_SAMPLERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_PSGETCONSTANTBUFFERS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_RSGETVIEWPORTS_VIEWPORTS_EMPTY=D3D10_MESSAGE_ID_DEVICE_PSGETSAMPLERS_SAMPLERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_RSGETSCISSORRECTS_RECTS_EMPTY=D3D10_MESSAGE_ID_DEVICE_RSGETVIEWPORTS_VIEWPORTS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_GENERATEMIPS_RESOURCE_INVALID=D3D10_MESSAGE_ID_DEVICE_RSGETSCISSORRECTS_RECTS_EMPTY + 1,
    D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDDESTINATIONSUBRESOURCE=D3D10_MESSAGE_ID_DEVICE_GENERATEMIPS_RESOURCE_INVALID + 1,
    D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDSOURCESUBRESOURCE=D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDDESTINATIONSUBRESOURCE + 1,
    D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDSOURCEBOX=D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDSOURCESUBRESOURCE + 1,
    D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDSOURCE=D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDSOURCEBOX + 1,
    D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDDESTINATIONSTATE=D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDSOURCE + 1,
    D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDSOURCESTATE=D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDDESTINATIONSTATE + 1,
    D3D10_MESSAGE_ID_COPYRESOURCE_INVALIDSOURCE=D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDSOURCESTATE + 1,
    D3D10_MESSAGE_ID_COPYRESOURCE_INVALIDDESTINATIONSTATE=D3D10_MESSAGE_ID_COPYRESOURCE_INVALIDSOURCE + 1,
    D3D10_MESSAGE_ID_COPYRESOURCE_INVALIDSOURCESTATE=D3D10_MESSAGE_ID_COPYRESOURCE_INVALIDDESTINATIONSTATE + 1,
    D3D10_MESSAGE_ID_UPDATESUBRESOURCE_INVALIDDESTINATIONSUBRESOURCE=D3D10_MESSAGE_ID_COPYRESOURCE_INVALIDSOURCESTATE + 1,
    D3D10_MESSAGE_ID_UPDATESUBRESOURCE_INVALIDDESTINATIONBOX=D3D10_MESSAGE_ID_UPDATESUBRESOURCE_INVALIDDESTINATIONSUBRESOURCE + 1,
    D3D10_MESSAGE_ID_UPDATESUBRESOURCE_INVALIDDESTINATIONSTATE=D3D10_MESSAGE_ID_UPDATESUBRESOURCE_INVALIDDESTINATIONBOX + 1,
    D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_DESTINATION_INVALID=D3D10_MESSAGE_ID_UPDATESUBRESOURCE_INVALIDDESTINATIONSTATE + 1,
    D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_DESTINATION_SUBRESOURCE_INVALID=D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_DESTINATION_INVALID + 1,
    D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_SOURCE_INVALID=D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_DESTINATION_SUBRESOURCE_INVALID + 1,
    D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_SOURCE_SUBRESOURCE_INVALID=D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_SOURCE_INVALID + 1,
    D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_FORMAT_INVALID=D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_SOURCE_SUBRESOURCE_INVALID + 1,
    D3D10_MESSAGE_ID_BUFFER_MAP_INVALIDMAPTYPE=D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_FORMAT_INVALID + 1,
    D3D10_MESSAGE_ID_BUFFER_MAP_INVALIDFLAGS=D3D10_MESSAGE_ID_BUFFER_MAP_INVALIDMAPTYPE + 1,
    D3D10_MESSAGE_ID_BUFFER_MAP_ALREADYMAPPED=D3D10_MESSAGE_ID_BUFFER_MAP_INVALIDFLAGS + 1,
    D3D10_MESSAGE_ID_BUFFER_MAP_DEVICEREMOVED_RETURN=D3D10_MESSAGE_ID_BUFFER_MAP_ALREADYMAPPED + 1,
    D3D10_MESSAGE_ID_BUFFER_UNMAP_NOTMAPPED=D3D10_MESSAGE_ID_BUFFER_MAP_DEVICEREMOVED_RETURN + 1,
    D3D10_MESSAGE_ID_TEXTURE1D_MAP_INVALIDMAPTYPE=D3D10_MESSAGE_ID_BUFFER_UNMAP_NOTMAPPED + 1,
    D3D10_MESSAGE_ID_TEXTURE1D_MAP_INVALIDSUBRESOURCE=D3D10_MESSAGE_ID_TEXTURE1D_MAP_INVALIDMAPTYPE + 1,
    D3D10_MESSAGE_ID_TEXTURE1D_MAP_INVALIDFLAGS=D3D10_MESSAGE_ID_TEXTURE1D_MAP_INVALIDSUBRESOURCE + 1,
    D3D10_MESSAGE_ID_TEXTURE1D_MAP_ALREADYMAPPED=D3D10_MESSAGE_ID_TEXTURE1D_MAP_INVALIDFLAGS + 1,
    D3D10_MESSAGE_ID_TEXTURE1D_MAP_DEVICEREMOVED_RETURN=D3D10_MESSAGE_ID_TEXTURE1D_MAP_ALREADYMAPPED + 1,
    D3D10_MESSAGE_ID_TEXTURE1D_UNMAP_INVALIDSUBRESOURCE=D3D10_MESSAGE_ID_TEXTURE1D_MAP_DEVICEREMOVED_RETURN + 1,
    D3D10_MESSAGE_ID_TEXTURE1D_UNMAP_NOTMAPPED=D3D10_MESSAGE_ID_TEXTURE1D_UNMAP_INVALIDSUBRESOURCE + 1,
    D3D10_MESSAGE_ID_TEXTURE2D_MAP_INVALIDMAPTYPE=D3D10_MESSAGE_ID_TEXTURE1D_UNMAP_NOTMAPPED + 1,
    D3D10_MESSAGE_ID_TEXTURE2D_MAP_INVALIDSUBRESOURCE=D3D10_MESSAGE_ID_TEXTURE2D_MAP_INVALIDMAPTYPE + 1,
    D3D10_MESSAGE_ID_TEXTURE2D_MAP_INVALIDFLAGS=D3D10_MESSAGE_ID_TEXTURE2D_MAP_INVALIDSUBRESOURCE + 1,
    D3D10_MESSAGE_ID_TEXTURE2D_MAP_ALREADYMAPPED=D3D10_MESSAGE_ID_TEXTURE2D_MAP_INVALIDFLAGS + 1,
    D3D10_MESSAGE_ID_TEXTURE2D_MAP_DEVICEREMOVED_RETURN=D3D10_MESSAGE_ID_TEXTURE2D_MAP_ALREADYMAPPED + 1,
    D3D10_MESSAGE_ID_TEXTURE2D_UNMAP_INVALIDSUBRESOURCE=D3D10_MESSAGE_ID_TEXTURE2D_MAP_DEVICEREMOVED_RETURN + 1,
    D3D10_MESSAGE_ID_TEXTURE2D_UNMAP_NOTMAPPED=D3D10_MESSAGE_ID_TEXTURE2D_UNMAP_INVALIDSUBRESOURCE + 1,
    D3D10_MESSAGE_ID_TEXTURE3D_MAP_INVALIDMAPTYPE=D3D10_MESSAGE_ID_TEXTURE2D_UNMAP_NOTMAPPED + 1,
    D3D10_MESSAGE_ID_TEXTURE3D_MAP_INVALIDSUBRESOURCE=D3D10_MESSAGE_ID_TEXTURE3D_MAP_INVALIDMAPTYPE + 1,
    D3D10_MESSAGE_ID_TEXTURE3D_MAP_INVALIDFLAGS=D3D10_MESSAGE_ID_TEXTURE3D_MAP_INVALIDSUBRESOURCE + 1,
    D3D10_MESSAGE_ID_TEXTURE3D_MAP_ALREADYMAPPED=D3D10_MESSAGE_ID_TEXTURE3D_MAP_INVALIDFLAGS + 1,
    D3D10_MESSAGE_ID_TEXTURE3D_MAP_DEVICEREMOVED_RETURN=D3D10_MESSAGE_ID_TEXTURE3D_MAP_ALREADYMAPPED + 1,
    D3D10_MESSAGE_ID_TEXTURE3D_UNMAP_INVALIDSUBRESOURCE=D3D10_MESSAGE_ID_TEXTURE3D_MAP_DEVICEREMOVED_RETURN + 1,
    D3D10_MESSAGE_ID_TEXTURE3D_UNMAP_NOTMAPPED=D3D10_MESSAGE_ID_TEXTURE3D_UNMAP_INVALIDSUBRESOURCE + 1,
    D3D10_MESSAGE_ID_CHECKFORMATSUPPORT_FORMAT_DEPRECATED=D3D10_MESSAGE_ID_TEXTURE3D_UNMAP_NOTMAPPED + 1,
    D3D10_MESSAGE_ID_CHECKMULTISAMPLEQUALITYLEVELS_FORMAT_DEPRECATED=D3D10_MESSAGE_ID_CHECKFORMATSUPPORT_FORMAT_DEPRECATED + 1,
    D3D10_MESSAGE_ID_SETEXCEPTIONMODE_UNRECOGNIZEDFLAGS=D3D10_MESSAGE_ID_CHECKMULTISAMPLEQUALITYLEVELS_FORMAT_DEPRECATED + 1,
    D3D10_MESSAGE_ID_SETEXCEPTIONMODE_INVALIDARG_RETURN=D3D10_MESSAGE_ID_SETEXCEPTIONMODE_UNRECOGNIZEDFLAGS + 1,
    D3D10_MESSAGE_ID_SETEXCEPTIONMODE_DEVICEREMOVED_RETURN=D3D10_MESSAGE_ID_SETEXCEPTIONMODE_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_REF_SIMULATING_INFINITELY_FAST_HARDWARE=D3D10_MESSAGE_ID_SETEXCEPTIONMODE_DEVICEREMOVED_RETURN + 1,
    D3D10_MESSAGE_ID_REF_THREADING_MODE=D3D10_MESSAGE_ID_REF_SIMULATING_INFINITELY_FAST_HARDWARE + 1,
    D3D10_MESSAGE_ID_REF_UMDRIVER_EXCEPTION=D3D10_MESSAGE_ID_REF_THREADING_MODE + 1,
    D3D10_MESSAGE_ID_REF_KMDRIVER_EXCEPTION=D3D10_MESSAGE_ID_REF_UMDRIVER_EXCEPTION + 1,
    D3D10_MESSAGE_ID_REF_HARDWARE_EXCEPTION=D3D10_MESSAGE_ID_REF_KMDRIVER_EXCEPTION + 1,
    D3D10_MESSAGE_ID_REF_ACCESSING_INDEXABLE_TEMP_OUT_OF_RANGE=D3D10_MESSAGE_ID_REF_HARDWARE_EXCEPTION + 1,
    D3D10_MESSAGE_ID_REF_PROBLEM_PARSING_SHADER=D3D10_MESSAGE_ID_REF_ACCESSING_INDEXABLE_TEMP_OUT_OF_RANGE + 1,
    D3D10_MESSAGE_ID_REF_OUT_OF_MEMORY=D3D10_MESSAGE_ID_REF_PROBLEM_PARSING_SHADER + 1,
    D3D10_MESSAGE_ID_REF_INFO=D3D10_MESSAGE_ID_REF_OUT_OF_MEMORY + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEXPOS_OVERFLOW=D3D10_MESSAGE_ID_REF_INFO + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAWINDEXED_INDEXPOS_OVERFLOW=D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEXPOS_OVERFLOW + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAWINSTANCED_VERTEXPOS_OVERFLOW=D3D10_MESSAGE_ID_DEVICE_DRAWINDEXED_INDEXPOS_OVERFLOW + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAWINSTANCED_INSTANCEPOS_OVERFLOW=D3D10_MESSAGE_ID_DEVICE_DRAWINSTANCED_VERTEXPOS_OVERFLOW + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAWINDEXEDINSTANCED_INSTANCEPOS_OVERFLOW=D3D10_MESSAGE_ID_DEVICE_DRAWINSTANCED_INSTANCEPOS_OVERFLOW + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAWINDEXEDINSTANCED_INDEXPOS_OVERFLOW=D3D10_MESSAGE_ID_DEVICE_DRAWINDEXEDINSTANCED_INSTANCEPOS_OVERFLOW + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_SHADER_NOT_SET=D3D10_MESSAGE_ID_DEVICE_DRAWINDEXEDINSTANCED_INDEXPOS_OVERFLOW + 1,
    D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_SEMANTICNAME_NOT_FOUND=D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_SHADER_NOT_SET + 1,
    D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_REGISTERINDEX=D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_SEMANTICNAME_NOT_FOUND + 1,
    D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_COMPONENTTYPE=D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_REGISTERINDEX + 1,
    D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_REGISTERMASK=D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_COMPONENTTYPE + 1,
    D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_SYSTEMVALUE=D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_REGISTERMASK + 1,
    D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_NEVERWRITTEN_ALWAYSREADS=D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_SYSTEMVALUE + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_BUFFER_NOT_SET=D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_NEVERWRITTEN_ALWAYSREADS + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_INPUTLAYOUT_NOT_SET=D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_BUFFER_NOT_SET + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_CONSTANT_BUFFER_NOT_SET=D3D10_MESSAGE_ID_DEVICE_DRAW_INPUTLAYOUT_NOT_SET + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_CONSTANT_BUFFER_TOO_SMALL=D3D10_MESSAGE_ID_DEVICE_DRAW_CONSTANT_BUFFER_NOT_SET + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_SAMPLER_NOT_SET=D3D10_MESSAGE_ID_DEVICE_DRAW_CONSTANT_BUFFER_TOO_SMALL + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_SHADERRESOURCEVIEW_NOT_SET=D3D10_MESSAGE_ID_DEVICE_DRAW_SAMPLER_NOT_SET + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VIEW_DIMENSION_MISMATCH=D3D10_MESSAGE_ID_DEVICE_DRAW_SHADERRESOURCEVIEW_NOT_SET + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_BUFFER_STRIDE_TOO_SMALL=D3D10_MESSAGE_ID_DEVICE_DRAW_VIEW_DIMENSION_MISMATCH + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_BUFFER_TOO_SMALL=D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_BUFFER_STRIDE_TOO_SMALL + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_INDEX_BUFFER_NOT_SET=D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_BUFFER_TOO_SMALL + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_INDEX_BUFFER_FORMAT_INVALID=D3D10_MESSAGE_ID_DEVICE_DRAW_INDEX_BUFFER_NOT_SET + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_INDEX_BUFFER_TOO_SMALL=D3D10_MESSAGE_ID_DEVICE_DRAW_INDEX_BUFFER_FORMAT_INVALID + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_GS_INPUT_PRIMITIVE_MISMATCH=D3D10_MESSAGE_ID_DEVICE_DRAW_INDEX_BUFFER_TOO_SMALL + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_RETURN_TYPE_MISMATCH=D3D10_MESSAGE_ID_DEVICE_DRAW_GS_INPUT_PRIMITIVE_MISMATCH + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_POSITION_NOT_PRESENT=D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_RETURN_TYPE_MISMATCH + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_OUTPUT_STREAM_NOT_SET=D3D10_MESSAGE_ID_DEVICE_DRAW_POSITION_NOT_PRESENT + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_BOUND_RESOURCE_MAPPED=D3D10_MESSAGE_ID_DEVICE_DRAW_OUTPUT_STREAM_NOT_SET + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_INVALID_PRIMITIVETOPOLOGY=D3D10_MESSAGE_ID_DEVICE_DRAW_BOUND_RESOURCE_MAPPED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_OFFSET_UNALIGNED=D3D10_MESSAGE_ID_DEVICE_DRAW_INVALID_PRIMITIVETOPOLOGY + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_STRIDE_UNALIGNED=D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_OFFSET_UNALIGNED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_INDEX_OFFSET_UNALIGNED=D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_STRIDE_UNALIGNED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_OUTPUT_STREAM_OFFSET_UNALIGNED=D3D10_MESSAGE_ID_DEVICE_DRAW_INDEX_OFFSET_UNALIGNED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_FORMAT_LD_UNSUPPORTED=D3D10_MESSAGE_ID_DEVICE_DRAW_OUTPUT_STREAM_OFFSET_UNALIGNED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_FORMAT_SAMPLE_UNSUPPORTED=D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_FORMAT_LD_UNSUPPORTED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_FORMAT_SAMPLE_C_UNSUPPORTED=D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_FORMAT_SAMPLE_UNSUPPORTED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_MULTISAMPLE_UNSUPPORTED=D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_FORMAT_SAMPLE_C_UNSUPPORTED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_SO_TARGETS_BOUND_WITHOUT_SOURCE=D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_MULTISAMPLE_UNSUPPORTED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_SO_STRIDE_LARGER_THAN_BUFFER=D3D10_MESSAGE_ID_DEVICE_DRAW_SO_TARGETS_BOUND_WITHOUT_SOURCE + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_OM_RENDER_TARGET_DOES_NOT_SUPPORT_BLENDING=D3D10_MESSAGE_ID_DEVICE_DRAW_SO_STRIDE_LARGER_THAN_BUFFER + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_OM_DUAL_SOURCE_BLENDING_CAN_ONLY_HAVE_RENDER_TARGET_0=D3D10_MESSAGE_ID_DEVICE_DRAW_OM_RENDER_TARGET_DOES_NOT_SUPPORT_BLENDING + 1,
    D3D10_MESSAGE_ID_DEVICE_REMOVAL_PROCESS_AT_FAULT=D3D10_MESSAGE_ID_DEVICE_DRAW_OM_DUAL_SOURCE_BLENDING_CAN_ONLY_HAVE_RENDER_TARGET_0 + 1,
    D3D10_MESSAGE_ID_DEVICE_REMOVAL_PROCESS_POSSIBLY_AT_FAULT=D3D10_MESSAGE_ID_DEVICE_REMOVAL_PROCESS_AT_FAULT + 1,
    D3D10_MESSAGE_ID_DEVICE_REMOVAL_PROCESS_NOT_AT_FAULT=D3D10_MESSAGE_ID_DEVICE_REMOVAL_PROCESS_POSSIBLY_AT_FAULT + 1,
    D3D10_MESSAGE_ID_DEVICE_OPEN_SHARED_RESOURCE_INVALIDARG_RETURN=D3D10_MESSAGE_ID_DEVICE_REMOVAL_PROCESS_NOT_AT_FAULT + 1,
    D3D10_MESSAGE_ID_DEVICE_OPEN_SHARED_RESOURCE_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_DEVICE_OPEN_SHARED_RESOURCE_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_DEVICE_OPEN_SHARED_RESOURCE_BADINTERFACE_RETURN=D3D10_MESSAGE_ID_DEVICE_OPEN_SHARED_RESOURCE_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VIEWPORT_NOT_SET=D3D10_MESSAGE_ID_DEVICE_OPEN_SHARED_RESOURCE_BADINTERFACE_RETURN + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_TRAILING_DIGIT_IN_SEMANTIC=D3D10_MESSAGE_ID_DEVICE_DRAW_VIEWPORT_NOT_SET + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_TRAILING_DIGIT_IN_SEMANTIC=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_TRAILING_DIGIT_IN_SEMANTIC + 1,
    D3D10_MESSAGE_ID_DEVICE_RSSETVIEWPORTS_DENORMFLUSH=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_TRAILING_DIGIT_IN_SEMANTIC + 1,
    D3D10_MESSAGE_ID_OMSETRENDERTARGETS_INVALIDVIEW=D3D10_MESSAGE_ID_DEVICE_RSSETVIEWPORTS_DENORMFLUSH + 1,
    D3D10_MESSAGE_ID_DEVICE_SETTEXTFILTERSIZE_INVALIDDIMENSIONS=D3D10_MESSAGE_ID_OMSETRENDERTARGETS_INVALIDVIEW + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_SAMPLER_MISMATCH=D3D10_MESSAGE_ID_DEVICE_SETTEXTFILTERSIZE_INVALIDDIMENSIONS + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_TYPE_MISMATCH=D3D10_MESSAGE_ID_DEVICE_DRAW_SAMPLER_MISMATCH + 1,
    D3D10_MESSAGE_ID_BLENDSTATE_GETDESC_LEGACY=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_TYPE_MISMATCH + 1,
    D3D10_MESSAGE_ID_SHADERRESOURCEVIEW_GETDESC_LEGACY=D3D10_MESSAGE_ID_BLENDSTATE_GETDESC_LEGACY + 1,
    D3D10_MESSAGE_ID_CREATEQUERY_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_SHADERRESOURCEVIEW_GETDESC_LEGACY + 1,
    D3D10_MESSAGE_ID_CREATEPREDICATE_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATEQUERY_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATECOUNTER_OUTOFRANGE_COUNTER=D3D10_MESSAGE_ID_CREATEPREDICATE_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATECOUNTER_SIMULTANEOUS_ACTIVE_COUNTERS_EXHAUSTED=D3D10_MESSAGE_ID_CREATECOUNTER_OUTOFRANGE_COUNTER + 1,
    D3D10_MESSAGE_ID_CREATECOUNTER_UNSUPPORTED_WELLKNOWN_COUNTER=D3D10_MESSAGE_ID_CREATECOUNTER_SIMULTANEOUS_ACTIVE_COUNTERS_EXHAUSTED + 1,
    D3D10_MESSAGE_ID_CREATECOUNTER_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATECOUNTER_UNSUPPORTED_WELLKNOWN_COUNTER + 1,
    D3D10_MESSAGE_ID_CREATECOUNTER_NONEXCLUSIVE_RETURN=D3D10_MESSAGE_ID_CREATECOUNTER_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATECOUNTER_NULLDESC=D3D10_MESSAGE_ID_CREATECOUNTER_NONEXCLUSIVE_RETURN + 1,
    D3D10_MESSAGE_ID_CHECKCOUNTER_OUTOFRANGE_COUNTER=D3D10_MESSAGE_ID_CREATECOUNTER_NULLDESC + 1,
    D3D10_MESSAGE_ID_CHECKCOUNTER_UNSUPPORTED_WELLKNOWN_COUNTER=D3D10_MESSAGE_ID_CHECKCOUNTER_OUTOFRANGE_COUNTER + 1,
    D3D10_MESSAGE_ID_SETPREDICATION_INVALID_PREDICATE_STATE=D3D10_MESSAGE_ID_CHECKCOUNTER_UNSUPPORTED_WELLKNOWN_COUNTER + 1,
    D3D10_MESSAGE_ID_QUERY_BEGIN_UNSUPPORTED=D3D10_MESSAGE_ID_SETPREDICATION_INVALID_PREDICATE_STATE + 1,
    D3D10_MESSAGE_ID_PREDICATE_BEGIN_DURING_PREDICATION=D3D10_MESSAGE_ID_QUERY_BEGIN_UNSUPPORTED + 1,
    D3D10_MESSAGE_ID_QUERY_BEGIN_DUPLICATE=D3D10_MESSAGE_ID_PREDICATE_BEGIN_DURING_PREDICATION + 1,
    D3D10_MESSAGE_ID_QUERY_BEGIN_ABANDONING_PREVIOUS_RESULTS=D3D10_MESSAGE_ID_QUERY_BEGIN_DUPLICATE + 1,
    D3D10_MESSAGE_ID_PREDICATE_END_DURING_PREDICATION=D3D10_MESSAGE_ID_QUERY_BEGIN_ABANDONING_PREVIOUS_RESULTS + 1,
    D3D10_MESSAGE_ID_QUERY_END_ABANDONING_PREVIOUS_RESULTS=D3D10_MESSAGE_ID_PREDICATE_END_DURING_PREDICATION + 1,
    D3D10_MESSAGE_ID_QUERY_END_WITHOUT_BEGIN=D3D10_MESSAGE_ID_QUERY_END_ABANDONING_PREVIOUS_RESULTS + 1,
    D3D10_MESSAGE_ID_QUERY_GETDATA_INVALID_DATASIZE=D3D10_MESSAGE_ID_QUERY_END_WITHOUT_BEGIN + 1,
    D3D10_MESSAGE_ID_QUERY_GETDATA_INVALID_FLAGS=D3D10_MESSAGE_ID_QUERY_GETDATA_INVALID_DATASIZE + 1,
    D3D10_MESSAGE_ID_QUERY_GETDATA_INVALID_CALL=D3D10_MESSAGE_ID_QUERY_GETDATA_INVALID_FLAGS + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_PS_OUTPUT_TYPE_MISMATCH=D3D10_MESSAGE_ID_QUERY_GETDATA_INVALID_CALL + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_FORMAT_GATHER_UNSUPPORTED=D3D10_MESSAGE_ID_DEVICE_DRAW_PS_OUTPUT_TYPE_MISMATCH + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_INVALID_USE_OF_CENTER_MULTISAMPLE_PATTERN=D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_FORMAT_GATHER_UNSUPPORTED + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_STRIDE_TOO_LARGE=D3D10_MESSAGE_ID_DEVICE_DRAW_INVALID_USE_OF_CENTER_MULTISAMPLE_PATTERN + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_INVALIDRANGE=D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_STRIDE_TOO_LARGE + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_EMPTY_LAYOUT=D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_INVALIDRANGE + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_SAMPLE_COUNT_MISMATCH=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_EMPTY_LAYOUT + 1,
    D3D10_MESSAGE_ID_D3D10_MESSAGES_END=D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_SAMPLE_COUNT_MISMATCH + 1,
    D3D10_MESSAGE_ID_D3D10L9_MESSAGES_START=$100000,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_STENCIL_NO_TWO_SIDED=D3D10_MESSAGE_ID_D3D10L9_MESSAGES_START + 1,
    D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_DepthBiasClamp_NOT_SUPPORTED=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_STENCIL_NO_TWO_SIDED + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_NO_COMPARISON_SUPPORT=D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_DepthBiasClamp_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_EXCESSIVE_ANISOTROPY=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_NO_COMPARISON_SUPPORT + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_BORDER_OUT_OF_RANGE=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_EXCESSIVE_ANISOTROPY + 1,
    D3D10_MESSAGE_ID_VSSETSAMPLERS_NOT_SUPPORTED=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_BORDER_OUT_OF_RANGE + 1,
    D3D10_MESSAGE_ID_VSSETSAMPLERS_TOO_MANY_SAMPLERS=D3D10_MESSAGE_ID_VSSETSAMPLERS_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_PSSETSAMPLERS_TOO_MANY_SAMPLERS=D3D10_MESSAGE_ID_VSSETSAMPLERS_TOO_MANY_SAMPLERS + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NO_ARRAYS=D3D10_MESSAGE_ID_PSSETSAMPLERS_TOO_MANY_SAMPLERS + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NO_VB_AND_IB_BIND=D3D10_MESSAGE_ID_CREATERESOURCE_NO_ARRAYS + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NO_TEXTURE_1D=D3D10_MESSAGE_ID_CREATERESOURCE_NO_VB_AND_IB_BIND + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_DIMENSION_OUT_OF_RANGE=D3D10_MESSAGE_ID_CREATERESOURCE_NO_TEXTURE_1D + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NOT_BINDABLE_AS_SHADER_RESOURCE=D3D10_MESSAGE_ID_CREATERESOURCE_DIMENSION_OUT_OF_RANGE + 1,
    D3D10_MESSAGE_ID_OMSETRENDERTARGETS_TOO_MANY_RENDER_TARGETS=D3D10_MESSAGE_ID_CREATERESOURCE_NOT_BINDABLE_AS_SHADER_RESOURCE + 1,
    D3D10_MESSAGE_ID_OMSETRENDERTARGETS_NO_DIFFERING_BIT_DEPTHS=D3D10_MESSAGE_ID_OMSETRENDERTARGETS_TOO_MANY_RENDER_TARGETS + 1,
    D3D10_MESSAGE_ID_IASETVERTEXBUFFERS_BAD_BUFFER_INDEX=D3D10_MESSAGE_ID_OMSETRENDERTARGETS_NO_DIFFERING_BIT_DEPTHS + 1,
    D3D10_MESSAGE_ID_DEVICE_RSSETVIEWPORTS_TOO_MANY_VIEWPORTS=D3D10_MESSAGE_ID_IASETVERTEXBUFFERS_BAD_BUFFER_INDEX + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETPRIMITIVETOPOLOGY_ADJACENCY_UNSUPPORTED=D3D10_MESSAGE_ID_DEVICE_RSSETVIEWPORTS_TOO_MANY_VIEWPORTS + 1,
    D3D10_MESSAGE_ID_DEVICE_RSSETSCISSORRECTS_TOO_MANY_SCISSORS=D3D10_MESSAGE_ID_DEVICE_IASETPRIMITIVETOPOLOGY_ADJACENCY_UNSUPPORTED + 1,
    D3D10_MESSAGE_ID_COPYRESOURCE_ONLY_TEXTURE_2D_WITHIN_GPU_MEMORY=D3D10_MESSAGE_ID_DEVICE_RSSETSCISSORRECTS_TOO_MANY_SCISSORS + 1,
    D3D10_MESSAGE_ID_COPYRESOURCE_NO_TEXTURE_3D_READBACK=D3D10_MESSAGE_ID_COPYRESOURCE_ONLY_TEXTURE_2D_WITHIN_GPU_MEMORY + 1,
    D3D10_MESSAGE_ID_COPYRESOURCE_NO_TEXTURE_ONLY_READBACK=D3D10_MESSAGE_ID_COPYRESOURCE_NO_TEXTURE_3D_READBACK + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_UNSUPPORTED_FORMAT=D3D10_MESSAGE_ID_COPYRESOURCE_NO_TEXTURE_ONLY_READBACK + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_ALPHA_TO_COVERAGE=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_UNSUPPORTED_FORMAT + 1,
    D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_DepthClipEnable_MUST_BE_TRUE=D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_ALPHA_TO_COVERAGE + 1,
    D3D10_MESSAGE_ID_DRAWINDEXED_STARTINDEXLOCATION_MUST_BE_POSITIVE=D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_DepthClipEnable_MUST_BE_TRUE + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_MUST_USE_LOWEST_LOD=D3D10_MESSAGE_ID_DRAWINDEXED_STARTINDEXLOCATION_MUST_BE_POSITIVE + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_MINLOD_MUST_NOT_BE_FRACTIONAL=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_MUST_USE_LOWEST_LOD + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_MAXLOD_MUST_BE_FLT_MAX=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_MINLOD_MUST_NOT_BE_FRACTIONAL + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_FIRSTARRAYSLICE_MUST_BE_ZERO=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_MAXLOD_MUST_BE_FLT_MAX + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_CUBES_MUST_HAVE_6_SIDES=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_FIRSTARRAYSLICE_MUST_BE_ZERO + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NOT_BINDABLE_AS_RENDER_TARGET=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_CUBES_MUST_HAVE_6_SIDES + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NO_DWORD_INDEX_BUFFER=D3D10_MESSAGE_ID_CREATERESOURCE_NOT_BINDABLE_AS_RENDER_TARGET + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_MSAA_PRECLUDES_SHADER_RESOURCE=D3D10_MESSAGE_ID_CREATERESOURCE_NO_DWORD_INDEX_BUFFER + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_PRESENTATION_PRECLUDES_SHADER_RESOURCE=D3D10_MESSAGE_ID_CREATERESOURCE_MSAA_PRECLUDES_SHADER_RESOURCE + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_INDEPENDENT_BLEND_ENABLE=D3D10_MESSAGE_ID_CREATERESOURCE_PRESENTATION_PRECLUDES_SHADER_RESOURCE + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_INDEPENDENT_WRITE_MASKS=D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_INDEPENDENT_BLEND_ENABLE + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NO_STREAM_OUT=D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_INDEPENDENT_WRITE_MASKS + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_ONLY_VB_IB_FOR_BUFFERS=D3D10_MESSAGE_ID_CREATERESOURCE_NO_STREAM_OUT + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NO_AUTOGEN_FOR_VOLUMES=D3D10_MESSAGE_ID_CREATERESOURCE_ONLY_VB_IB_FOR_BUFFERS + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_DXGI_FORMAT_R8G8B8A8_CANNOT_BE_SHARED=D3D10_MESSAGE_ID_CREATERESOURCE_NO_AUTOGEN_FOR_VOLUMES + 1,
    D3D10_MESSAGE_ID_VSSHADERRESOURCES_NOT_SUPPORTED=D3D10_MESSAGE_ID_CREATERESOURCE_DXGI_FORMAT_R8G8B8A8_CANNOT_BE_SHARED + 1,
    D3D10_MESSAGE_ID_GEOMETRY_SHADER_NOT_SUPPORTED=D3D10_MESSAGE_ID_VSSHADERRESOURCES_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_STREAM_OUT_NOT_SUPPORTED=D3D10_MESSAGE_ID_GEOMETRY_SHADER_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_TEXT_FILTER_NOT_SUPPORTED=D3D10_MESSAGE_ID_STREAM_OUT_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_SEPARATE_ALPHA_BLEND=D3D10_MESSAGE_ID_TEXT_FILTER_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_MRT_BLEND=D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_SEPARATE_ALPHA_BLEND + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_OPERATION_NOT_SUPPORTED=D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_MRT_BLEND + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_NO_MIRRORONCE=D3D10_MESSAGE_ID_CREATEBLENDSTATE_OPERATION_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_DRAWINSTANCED_NOT_SUPPORTED=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_NO_MIRRORONCE + 1,
    D3D10_MESSAGE_ID_DRAWINDEXEDINSTANCED_NOT_SUPPORTED_BELOW_9_3=D3D10_MESSAGE_ID_DRAWINSTANCED_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_DRAWINDEXED_POINTLIST_UNSUPPORTED=D3D10_MESSAGE_ID_DRAWINDEXEDINSTANCED_NOT_SUPPORTED_BELOW_9_3 + 1,
    D3D10_MESSAGE_ID_SETBLENDSTATE_SAMPLE_MASK_CANNOT_BE_ZERO=D3D10_MESSAGE_ID_DRAWINDEXED_POINTLIST_UNSUPPORTED + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_DIMENSION_EXCEEDS_FEATURE_LEVEL_DEFINITION=D3D10_MESSAGE_ID_SETBLENDSTATE_SAMPLE_MASK_CANNOT_BE_ZERO + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_ONLY_SINGLE_MIP_LEVEL_DEPTH_STENCIL_SUPPORTED=D3D10_MESSAGE_ID_CREATERESOURCE_DIMENSION_EXCEEDS_FEATURE_LEVEL_DEFINITION + 1,
    D3D10_MESSAGE_ID_DEVICE_RSSETSCISSORRECTS_NEGATIVESCISSOR=D3D10_MESSAGE_ID_CREATERESOURCE_ONLY_SINGLE_MIP_LEVEL_DEPTH_STENCIL_SUPPORTED + 1,
    D3D10_MESSAGE_ID_SLOT_ZERO_MUST_BE_D3D10_INPUT_PER_VERTEX_DATA=D3D10_MESSAGE_ID_DEVICE_RSSETSCISSORRECTS_NEGATIVESCISSOR + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NON_POW_2_MIPMAP=D3D10_MESSAGE_ID_SLOT_ZERO_MUST_BE_D3D10_INPUT_PER_VERTEX_DATA + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_BORDER_NOT_SUPPORTED=D3D10_MESSAGE_ID_CREATERESOURCE_NON_POW_2_MIPMAP + 1,
    D3D10_MESSAGE_ID_OMSETRENDERTARGETS_NO_SRGB_MRT=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_BORDER_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_D3D10L9_MESSAGES_END=D3D10_MESSAGE_ID_OMSETRENDERTARGETS_NO_SRGB_MRT + 1
  );
  PTD3D10_MessageID=^TD3D10_MessageID;
  D3D10_MESSAGE_ID=TD3D10_MessageID;
  PD3D10_MESSAGE_ID=^TD3D10_MessageID;

  TD3D10_Message=record
    Category:TD3D10_MessageCategory;
    Severity:TD3D10_MessageSeverity;
    ID:TD3D10_MessageID;
    pDescription:PAnsiChar;
    DescriptionByteLength:SIZE_T;
  end;
  PTD3D10_Message=^TD3D10_Message;
  D3D10_MESSAGE=TD3D10_Message;
  PD3D10_MESSAGE=^TD3D10_Message;

  TD3D10_InfoQueueFilterDesc=record
    NumCategories:LongWord;
    pCategoryList:PTD3D10_MessageCategory;
    NumSeverities:LongWord;
    pSeverityList:PTD3D10_MessageSeverity;
    NumIDs:LongWord;
    pIDList:PTD3D10_MessageID;
  end;
  PTD3D10_InfoQueueFilterDesc=^TD3D10_InfoQueueFilterDesc;
  D3D10_INFO_QUEUE_FILTER_DESC=TD3D10_InfoQueueFilterDesc;
  PD3D10_INFO_QUEUE_FILTER_DESC=^TD3D10_InfoQueueFilterDesc;

  TD3D10_InfoQueueFilter=record
    AllowList:TD3D10_InfoQueueFilterDesc;
    DenyList:TD3D10_InfoQueueFilterDesc;
  end;
  PTD3D10_InfoQueueFilter=^TD3D10_InfoQueueFilter;
  D3D10_INFO_QUEUE_FILTER=TD3D10_InfoQueueFilter;
  PD3D10_INFO_QUEUE_FILTER=^TD3D10_InfoQueueFilter;

  ID3D10InfoQueue=interface(IUnknown)
    ['{1B940B17-2642-4D1F-AB1F-B99BAD0C395F}']
    function SetMessageCountLimit
    (
      MessageCountLimit:UInt64 (* __in *)
    ):HResult; stdcall;

    procedure ClearStoredMessages; stdcall;

    function GetMessage
    (
      MessageIndex:UInt64; (* __in *)
      pMessage:PTD3D10_Message; (* __out_bcount_opt(*pMessageByteLength) *)
      var MessageByteLength:SIZE_T (* __inout *)
    ):HResult; stdcall;

    function GetNumMessagesAllowedByStorageFilter:UInt64; stdcall;

    function GetNumMessagesDeniedByStorageFilter:UInt64; stdcall;

    function GetNumStoredMessages:UInt64; stdcall;

    function GetNumStoredMessagesAllowedByRetrievalFilter:UInt64; stdcall;

    function GetNumMessagesDiscardedByMessageCountLimit:UInt64; stdcall;

    function GetMessageCountLimit:UInt64; stdcall;

    function AddStorageFilterEntries
    (
      const Filter:TD3D10_InfoQueueFilter (* __in *)
    ):HResult; stdcall;

    function GetStorageFilter
    (
      pFilter:PTD3D10_InfoQueueFilter; (* __out_bcount_opt(*pFilterByteLength) *)
      var FilterByteLength:SIZE_T (* __inout *)
    ):HResult; stdcall;

    procedure ClearStorageFilter; stdcall;

    function PushEmptyStorageFilter:HResult; stdcall;

    function PushCopyOfStorageFilter:HResult; stdcall;

    function PushStorageFilter
    (
      const Filter:TD3D10_InfoQueueFilter (* __in *)
    ):HResult; stdcall;

    procedure PopStorageFilter; stdcall;

    function GetStorageFilterStackSize:LongWord; stdcall;

    function AddRetrievalFilterEntries
    (
      const Filter:TD3D10_InfoQueueFilter (* __in *)
    ):HResult; stdcall;

    function GetRetrievalFilter
    (
      pFilter:PTD3D10_InfoQueueFilter; (* __out_bcount_opt(*pFilterByteLength) *)
      var FilterByteLength:SIZE_T (* __inout *)
    ):HResult; stdcall;

    procedure ClearRetrievalFilter; stdcall;

    function PushEmptyRetrievalFilter:HResult; stdcall;

    function PushCopyOfRetrievalFilter:HResult; stdcall;

    function PushRetrievalFilter
    (
      const Filter:TD3D10_InfoQueueFilter (* __in *)
    ):HResult; stdcall;

    procedure PopRetrievalFilter; stdcall;

    function GetRetrievalFilterStackSize:LongWord; stdcall;

    function AddMessage
    (
      Category:TD3D10_MessageCategory; (* __in *)
      Severity:TD3D10_MessageSeverity; (* __in *)
      ID:TD3D10_MessageID; (* __in *)
      pDescription:PAnsiChar (* __in *)
    ):HResult; stdcall;

    function AddApplicationMessage
    (
      Severity:TD3D10_MessageSeverity; (* __in *)
      pDescription:PAnsiChar (* __in *)
    ):HResult; stdcall;

    function SetBreakOnCategory
    (
      Category:TD3D10_MessageCategory; (* __in *)
      Enable:LongBool (* __in *)
    ):HResult; stdcall;

    function SetBreakOnSeverity
    (
      Severity:TD3D10_MessageSeverity; (* __in *)
      Enable:LongBool (* __in *)
    ):HResult; stdcall;

    function SetBreakOnID
    (
      ID:TD3D10_MessageID; (* __in *)
      Enable:LongBool (* __in *)
    ):HResult; stdcall;

    function GetBreakOnCategory
    (
      Category:TD3D10_MessageCategory (* __in *)
    ):LongBool; stdcall;

    function GetBreakOnSeverity
    (
      Severity:TD3D10_MessageSeverity (* __in *)
    ):LongBool; stdcall;

    function GetBreakOnID
    (
      ID:TD3D10_MessageID (* __in *)
    ):LongBool; stdcall;

    procedure SetMuteDebugOutput
    (
      Mute:LongBool (* __in *)
    ); stdcall;

    function GetMuteDebugOutput:LongBool; stdcall;
  end;

///////////////////////////////////////////////////////////////////////////////
// End "D3D10SDKLayers.h"
///////////////////////////////////////////////////////////////////////////////

function MAKE_D3D10_HRESULT(i_Code:LongWord):LongWord; inline;
function MAKE_D3D10_STATUS(i_Code:LongWord):LongWord; inline;
function D3D10_BYTES_FROM_BITS(x:LongWord):LongWord; inline;
function D3D10_TX_VERSION(_Major,_Minor:Byte):LongWord; inline;

{$IFDEF UseRuntimeLinking}
procedure Link;
{$ENDIF}

implementation

function MAKE_D3D10_HRESULT(i_Code:LongWord):LongWord;
begin
  Result:=D3D10_HRESULT_Base or i_Code;
end;

function MAKE_D3D10_STATUS(i_Code:LongWord):LongWord;
begin
  Result:=D3D10_STATUS_Base or i_Code;
end;

function D3D10_BYTES_FROM_BITS(x:LongWord):LongWord;
begin
  Result:=(x+7) mod 8;
end;

function D3D10_TX_VERSION(_Major,_Minor:Byte):LongWord;
begin
  Result:=(Byte('T') shl 24) or (Byte('X') shl 16) or (_Major shl 8) or _Minor;
end;

{$IFDEF UseJSBErrors}
function HResultToString(Value:HRESULT):String;
begin
  Result:='';
  if SUCCEEDED(Value) then Exit;

  case Value of
    D3D10_ERROR_TOO_MANY_UNIQUE_STATE_OBJECTS: Result:='D3D10: Too many unique state objects.';
    D3D10_ERROR_FILE_NOT_FOUND: Result:='D3D10: File not found.';
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
  hDLL_D3D10:HModule;
begin
  hDLL_D3D10:=LoadDLL(DLL_D3D10);

  D3D10CreateDevice:=LinkMethod(hDLL_D3D10,'D3D10CreateDevice',DLL_D3D10);
  D3D10CreateDeviceAndSwapChain:=LinkMethod(hDLL_D3D10,'D3D10CreateDeviceAndSwapChain',DLL_D3D10);
  D3D10CreateBlob:=LinkMethod(hDLL_D3D10,'D3D10CreateBlob',DLL_D3D10);
  D3D10CompileShader:=LinkMethod(hDLL_D3D10,'D3D10CompileShader',DLL_D3D10);
  D3D10DisassembleShader:=LinkMethod(hDLL_D3D10,'D3D10DisassembleShader',DLL_D3D10);
  D3D10GetPixelShaderProfile:=LinkMethod(hDLL_D3D10,'D3D10GetPixelShaderProfile',DLL_D3D10);
  D3D10GetVertexShaderProfile:=LinkMethod(hDLL_D3D10,'D3D10GetVertexShaderProfile',DLL_D3D10);
  D3D10GetGeometryShaderProfile:=LinkMethod(hDLL_D3D10,'D3D10GetGeometryShaderProfile',DLL_D3D10);
  D3D10ReflectShader:=LinkMethod(hDLL_D3D10,'D3D10ReflectShader',DLL_D3D10);
  D3D10PreprocessShader:=LinkMethod(hDLL_D3D10,'D3D10PreprocessShader',DLL_D3D10);
  D3D10GetInputSignatureBlob:=LinkMethod(hDLL_D3D10,'D3D10GetInputSignatureBlob',DLL_D3D10);
  D3D10GetOutputSignatureBlob:=LinkMethod(hDLL_D3D10,'D3D10GetOutputSignatureBlob',DLL_D3D10);
  D3D10GetInputAndOutputSignatureBlob:=LinkMethod(hDLL_D3D10,'D3D10GetInputAndOutputSignatureBlob',DLL_D3D10);
  D3D10GetShaderDebugInfo:=LinkMethod(hDLL_D3D10,'D3D10GetShaderDebugInfo',DLL_D3D10);
  D3D10StateBlockMaskUnion:=LinkMethod(hDLL_D3D10,'D3D10StateBlockMaskUnion',DLL_D3D10);
  D3D10StateBlockMaskIntersect:=LinkMethod(hDLL_D3D10,'D3D10StateBlockMaskIntersect',DLL_D3D10);
  D3D10StateBlockMaskDifference:=LinkMethod(hDLL_D3D10,'D3D10StateBlockMaskDifference',DLL_D3D10);
  D3D10StateBlockMaskEnableCapture:=LinkMethod(hDLL_D3D10,'D3D10StateBlockMaskEnableCapture',DLL_D3D10);
  D3D10StateBlockMaskDisableCapture:=LinkMethod(hDLL_D3D10,'D3D10StateBlockMaskDisableCapture',DLL_D3D10);
  D3D10StateBlockMaskEnableAll:=LinkMethod(hDLL_D3D10,'D3D10StateBlockMaskEnableAll',DLL_D3D10);
  D3D10StateBlockMaskDisableAll:=LinkMethod(hDLL_D3D10,'D3D10StateBlockMaskDisableAll',DLL_D3D10);
  D3D10StateBlockMaskGetSetting:=LinkMethod(hDLL_D3D10,'D3D10StateBlockMaskGetSetting',DLL_D3D10);
  D3D10CreateStateBlock:=LinkMethod(hDLL_D3D10,'D3D10CreateStateBlock',DLL_D3D10);
  D3D10CompileEffectFromMemory:=LinkMethod(hDLL_D3D10,'D3D10CompileEffectFromMemory',DLL_D3D10);
  D3D10CreateEffectFromMemory:=LinkMethod(hDLL_D3D10,'D3D10CreateEffectFromMemory',DLL_D3D10);
  D3D10CreateEffectPoolFromMemory:=LinkMethod(hDLL_D3D10,'D3D10CreateEffectPoolFromMemory',DLL_D3D10);
  D3D10DisassembleEffect:=LinkMethod(hDLL_D3D10,'D3D10DisassembleEffect',DLL_D3D10);
end;
{$ENDIF}

{$IFDEF UseJSBErrors}
initialization
begin
  AddDirectXHResultToStringHandler(HResultToString);
end;
{$ENDIF}

end.
