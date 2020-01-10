(**********************************************************************************
 * Copyright (c) 2008-2012 The Khronos Group Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and/or associated documentation files (the
 * "Materials"), to deal in the Materials without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Materials, and to
 * permit persons to whom the Materials are furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Materials.
 *
 * THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
 **********************************************************************************)

// cl.h          $Revision: 11985 $ on $Date: 2010-07-15 11:16:06 -0700 (Thu, 15 Jul 2010) $
// cl_platform.h $Revision: 11803 $ on $Date: 2010-06-25 10:02:12 -0700 (Fri, 25 Jun 2010) $
// cl_ext.h      $Revision: 11928 $ on $Date: 2010-07-13 09:04:56 -0700 (Tue, 13 Jul 2010) $

// OpenCL 1.2 for Delphi 7 by Michal Pohanka: 2013-07-17.
// http://www.volny.cz/profipohanka
// OpenCL library is loaded manualy and the program will not crash
// when OpenCL is not installed.
// Check OpenCL_loaded variable to see if OpenCL is available.
// Avoid usage of Halt procedure to make sure the FINALIZATION section is executed

// some parts were inspired by "porting of OpenCL 1.0 to FPC" by Dmitry 'skalogryz' Boyarintsev: 28th apr 2009
// some parts were inspired by "OpenCL1.2 and Delphi and Windows" by Maksym Tymkovych
// due to name conflict with type names, some constants have been renamed

// Original C name                 Ported_name
// CL_DEVICE_TYPE                  CL_DEVICE_TYPE_INFO
// CL_DEVICE_LOCAL_MEM_TYPE        CL_DEVICE_LOCAL_MEM_TYPE_INFO
// CL_CONTEXT_PROPERTIES           CL_CONTEXT_PROPERTIES_INFO
// CL_CONTEXT_PLATFORM             CL_CONTEXT_PLATFORM_INFO
// CL_FLOAT                        CL_FLOAT_TYPE
// CL_MEM_FLAGS                    CL_MEM_FLAGS_INFO
// CL_IMAGE_FORMAT                 CL_IMAGE_FORMAT_INFO
// CL_PROGRAM_BINARY_TYPE          CL_PROGRAM_BINARY_TYPE_INFO
// CL_KERNEL_ARG_ADDRESS_QUALIFIER CL_KERNEL_ARG_ADDRESS_QUALIFIER_INFO
// CL_KERNEL_ARG_ACCESS_QUALIFIER  CL_KERNEL_ARG_ACCESS_QUALIFIER_INFO
// CL_KERNEL_ARG_TYPE_QUALIFIER    CL_KERNEL_ARG_TYPE_QUALIFIER_INFO
// CL_DEVICE_DOUBLE_FP_CONFIG      CL_DEVICE_DOUBLE_FP_CONFIG_INFO

unit CL;
{$R-}
interface

{$INCLUDE 'CL.inc'}
uses
  {$IFDEF WINDOWS}
  Windows;
  {$ENDIF}

var
  OpenCL_handle: THandle; // handle for the OpenCL.dll library
  OpenCL_loaded: boolean; // test this value to make sure the OpenCL is loaded
  OpenCL_func_not_loaded_str: AnsiString; // name of functions that were not found in the OpenCL.dll library

{$IFDEF DEFINE_8087CW_NOT_IMPLEMENTED}
var
  Default8087CW: Word = $133F;
{$ENDIF}

///////////////////////////////////////////////////////////////////////////////////////////////////
{cl_platform.h}

{* scalar types  *}

type
  p_pointer  = ^pointer;
  cintptr_t  = pointer;
  intptr_t   = ^integer;

  size_t     = nativeint;
  p_size_t   = ^size_t;

  p_byte     = ^byte;
  pp_byte    = p_byte;

  int8_t     = shortint;//-127..+128;
  uint8_t    = byte;//0..255;
  int16_t    = smallint;//- 32767..+32768;
  uint16_t   = word;//0..+65535;
  int32_t    = longint;//-2147483647..+2147483648;
  uint32_t   = longword;//0..4294967295;
  int64_t    = int64;
  //The error is found by Andrew Terekhov
  uint64_t   = {$IFDEF DEFINE_UINT64_EQU_INT64} int64;{$ELSE} uint64;{$ENDIF}
  float      = single;

  cl_char    = int8_t;
  cl_uchar   = uint8_t;
  cl_short   = int16_t;
  cl_ushort  = uint16_t;
  cl_int     = int32_t;
  cl_uint    = uint32_t;
  cl_long    = int64_t;
  cl_ulong   = uint64_t;

  cl_half    = uint16_t;
  cl_float   = float;
  cl_double  = double;

  p_cl_char   = ^cl_char;
  p_cl_uchar  = ^cl_uchar;
  p_cl_short  = ^cl_short;
  p_cl_ushort = ^cl_ushort;
  p_cl_int    = ^cl_int;
  p_cl_uint   = ^cl_uint;
  p_cl_long   = ^cl_long;
  p_cl_ulong  = ^cl_ulong;

  p_cl_half   = ^cl_half;
  p_cl_float  = ^cl_float;
  p_cl_double = ^cl_double;


const
  CL_CHAR_BIT  = 8;
  CL_SCHAR_MAX = 127;
  CL_SCHAR_MIN = (-127-1);
  CL_CHAR_MAX  = CL_SCHAR_MAX;
  CL_CHAR_MIN  = CL_SCHAR_MIN;
  CL_UCHAR_MAX = 255;
  CL_SHRT_MAX  = 32767;
  CL_SHRT_MIN  = (-32767-1);
  CL_USHRT_MAX = 65535;
  CL_INT_MAX   = 2147483647;
  CL_INT_MIN   = (-2147483647-1);
  CL_UINT_MAX  = $ffffffff;
  CL_LONG_MAX  = cl_long($7FFFFFFFFFFFFFFF);
  CL_LONG_MIN  = cl_long(-$7FFFFFFFFFFFFFFF) - 1;
  CL_ULONG_MAX = cl_ulong($FFFFFFFFFFFFFFFF);

  CL_FLT_DIG        = 6;
  CL_FLT_MANT_DIG   = 24;
  CL_FLT_MAX_10_EXP = +38;
  CL_FLT_MAX_EXP    = +128;
  CL_FLT_MIN_10_EXP = -37;
  CL_FLT_MIN_EXP    = -125;
  CL_FLT_RADIX      = 2;
// TODO: next three constants should be verified
  CL_FLT_MAX        = 340282346638528859811704183484516925440.0; // 3.4028234664e38;
  CL_FLT_MIN        = 1.175494350822287507969e-38; // 1.1754943508e-38;
  CL_FLT_EPSILON    = 1.192092895507812e-07;

  CL_DBL_DIG        = 15;
  CL_DBL_MANT_DIG   = 53;
  CL_DBL_MAX_10_EXP = +308;
  CL_DBL_MAX_EXP    = +1024;
  CL_DBL_MIN_10_EXP = -307;
  CL_DBL_MIN_EXP    = -1021;
  CL_DBL_RADIX      = 2;
// TODO: next three constants should be verified  
  CL_DBL_MAX        = 179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368.0;
  CL_DBL_MIN        = 2.225073858507201383090e-308;
  CL_DBL_EPSILON    = 2.220446049250313080847e-16;

  CL_M_E        = 2.718281828459045090796;
  CL_M_LOG2E    = 1.442695040888963387005;
  CL_M_LOG10E   = 0.434294481903251816668;
  CL_M_LN2      = 0.693147180559945286227;
  CL_M_LN10     = 2.302585092994045901094;
  CL_M_PI       = 3.141592653589793115998;
  CL_M_PI_2     = 1.570796326794896557999;
  CL_M_PI_4     = 0.785398163397448278999;
  CL_M_1_PI     = 0.318309886183790691216;
  CL_M_2_PI     = 0.636619772367581382433;
  CL_M_2_SQRTPI = 1.128379167095512558561;
  CL_M_SQRT2    = 1.414213562373095145475;
  CL_M_SQRT1_2  = 0.707106781186547572737;

  CL_M_E_F        = 2.71828174591064;
  CL_M_LOG2E_F    = 1.44269502162933;
  CL_M_LOG10E_F   = 0.43429449200630;
  CL_M_LN2_F      = 0.69314718246460;
  CL_M_LN10_F     = 2.30258512496948;
  CL_M_PI_F       = 3.14159274101257;
  CL_M_PI_2_F     = 1.57079637050629;
  CL_M_PI_4_F     = 0.78539818525314;
  CL_M_1_PI_F     = 0.31830987334251;
  CL_M_2_PI_F     = 0.63661974668503;
  CL_M_2_SQRTPI_F = 1.12837922573090;
  CL_M_SQRT2_F    = 1.41421353816986;
  CL_M_SQRT1_2_F  = 0.70710676908493;

  CL_HUGE_VALF : cl_float=1e50;
  CL_HUGE_VAL  : cl_double=1e500;
  CL_MAXFLOAT  = CL_FLT_MAX;
  CL_INFINITY  : cl_float=1e50; //CL_HUGE_VALF;
  CL_NAN       = 0/0;//(CL_INFINITY - CL_INFINITY);

type
// Mirror types to GL types. Mirror types allow us to avoid deciding which 87s to load based on whether we are using GL or GLES here.
  cl_GLuint = cl_uint;
  cl_GLint  = cl_int;
  cl_GLenum = cl_uint;

  p_cl_GLuint = ^cl_GLuint;
  p_cl_GLint  = ^cl_GLint;
  p_cl_GLenum = ^cl_GLenum;

{*
 * Vector types
 *
 *  Note:   OpenCL requires that all types be naturally aligned.
 *          This means that vector types must be naturally aligned.
 *          For example, a vector of four floats must be aligned to
 *          a 16 byte boundary (calculated as 4 * the natural 4-byte
 *          alignment of the float).  The alignment qualifiers here
 *          will only function properly if your compiler supports them
 *          and if you don't actively work to defeat them.  For example,
 *          in order for a cl_float4 to be 16 byte aligned in a struct,
 *          the start of the struct must itself be 16-byte aligned.
 *
 *          Maintaining proper alignment is the user's responsibility.
 *}
type
  cl_char2  = array [0..1] of int8_t;
  cl_char4  = array [0..3] of int8_t;
  cl_char3  = cl_char4; // cl_char3 is identical in size, alignment and behavior to cl_char4. See section 6.1.5.
  cl_char8  = array [0..7] of int8_t;
  cl_char16 = array [0..15] of int8_t;

  cl_uchar2  = array [0..1] of uint8_t;
  cl_uchar4  = array [0..3] of uint8_t;
  cl_uchar3  = cl_uchar4; // cl_uchar3 is identical in size, alignment and behavior to cl_uchar4. See section 6.1.5.
  cl_uchar8  = array [0..7] of uint8_t;
  cl_uchar16 = array [0..15] of uint8_t;

  cl_short2  = array [0..1] of int16_t;
  cl_short4  = array [0..3] of int16_t;
  cl_short3  = cl_short4; // cl_short3 is identical in size, alignment and behavior to cl_short4. See section 6.1.5.
  cl_short8  = array [0..7] of int16_t;
  cl_short16 = array [0..15] of int16_t;

  cl_ushort2  = array [0..1] of uint16_t;
  cl_ushort4  = array [0..3] of uint16_t;
  cl_ushort3  = cl_ushort4; // cl_ushort3 is identical in size, alignment and behavior to cl_ushort4. See section 6.1.5.
  cl_ushort8  = array [0..7] of uint16_t;
  cl_ushort16 = array [0..15] of uint16_t;

  cl_int2  = array [0..1] of int32_t;
  cl_int4  = array [0..3] of int32_t;
  cl_int3  = cl_int4; // cl_int3 is identical in size, alignment and behavior to cl_int4. See section 6.1.5.
  cl_int8  = array [0..7] of int32_t;
  cl_int16 = array [0..15] of int32_t;

  cl_uint2  = array [0..1] of uint32_t;
  cl_uint4  = array [0..3] of uint32_t;
  cl_uint3  = cl_uint4; // cl_uint3 is identical in size, alignment and behavior to cl_uint4. See section 6.1.5.
  cl_uint8  = array [0..7] of uint32_t;
  cl_uint16 = array [0..15] of uint32_t;

  cl_long2  = array [0..1] of int64_t;
  cl_long4  = array [0..3] of int64_t;
  cl_long3  = cl_long4; // cl_long3 is identical in size, alignment and behavior to cl_long4. See section 6.1.5.
  cl_long8  = array [0..7] of int64_t;
  cl_long16 = array [0..15] of int64_t;

  cl_ulong2  = array [0..1] of uint64_t;
  cl_ulong4  = array [0..3] of uint64_t;
  cl_ulong3  = cl_ulong4; // cl_ulong3 is identical in size, alignment and behavior to cl_ulong4. See section 6.1.5.
  cl_ulong8  = array [0..7] of uint64_t;
  cl_ulong16 = array [0..15] of uint64_t;

  cl_float2  = array [0..1] of float;
  cl_float4  = array [0..3] of float;
  cl_float3  = cl_float4; // cl_float3 is identical in size, alignment and behavior to cl_float4. See section 6.1.5.
  cl_float8  = array [0..7] of float;
  cl_float16 = array [0..15] of float;

  cl_double2  = array [0..1] of double;
  cl_double4  = array [0..3] of double;
  cl_double3  = cl_double4; // cl_double3 is identical in size, alignment and behavior to cl_double4. See section 6.1.5.
  cl_double8  = array [0..7] of double;
  cl_double16 = array [0..15] of double;

// No vectors for half

(* Macro to facilitate debugging
 * Usage:
 *   Place CL_PROGRAM_STRING_DEBUG_INFO on the line before the first line of your source.
 *   The first line ends with:   CL_PROGRAM_STRING_BEGIN \"
 *   Each line thereafter of OpenCL C source must end with: \n\
 *   The last line ends in ";
 *
 *   Example:
 *
 *   const char *my_program = CL_PROGRAM_STRING_BEGIN "\
 *   kernel void foo( int a, float * b )             \n\
 *   {                                               \n\
 *      // my comment                                \n\
 *      *b[ get_global_id(0)] = a;                   \n\
 *   }                                               \n\
 *   ";
 *
 * This should correctly set up the line, (column) and file information for your source
 * string so you can do source level debugging.

  #define  __CL_STRINGIFY( _x )               # _x
  #define  _CL_STRINGIFY( _x )                __CL_STRINGIFY( _x )
  #define  CL_PROGRAM_STRING_DEBUG_INFO       "#line "  _CL_STRINGIFY(__LINE__) " \"" __FILE__ "\" \n\n"
 *)

///////////////////////////////////////////////////////////////////////////////////////////////////
{cl.h}

type
  _cl_emptyrecord   = record end;

  cl_platform_id   = ^_cl_emptyrecord;
  cl_device_id     = ^_cl_emptyrecord;
  cl_context       = ^_cl_emptyrecord;
  cl_command_queue = ^_cl_emptyrecord;
  cl_mem           = ^_cl_emptyrecord;
  cl_program       = ^_cl_emptyrecord;
  cl_kernel        = ^_cl_emptyrecord;
  cl_event         = ^_cl_emptyrecord;
  cl_sampler       = ^_cl_emptyrecord;

  p_cl_platform_id   = ^cl_platform_id;
  p_cl_device_id     = ^cl_device_id;
  p_cl_context       = ^cl_context;
  p_cl_command_queue = ^cl_command_queue;
  p_cl_mem           = ^cl_mem;
  p_cl_program       = ^cl_program;
  p_cl_kernel        = ^cl_kernel;
  p_cl_event         = ^cl_event;
  p_cl_sampler       = ^cl_sampler;

  cl_bool                     = cl_uint; // WARNING!  Unlike cl_ types in cl_platform.h, cl_bool is not guaranteed to be the same size as the bool in kernels.
  cl_bitfield                 = cl_ulong;
  cl_device_type              = cl_bitfield;
  cl_platform_info            = cl_uint;
  cl_device_info              = cl_uint;
//cl_device_address_info      = cl_bitfield; // only for OpenCL 1.0
  cl_device_fp_config         = cl_bitfield;
  cl_device_mem_cache_type    = cl_uint;
  cl_device_local_mem_type    = cl_uint;
  cl_device_exec_capabilities = cl_bitfield;
  cl_command_queue_properties = cl_bitfield;
  cl_device_partition_property= intptr_t;
  cl_device_affinity_domain   = cl_bitfield;

  cl_context_properties     = cintptr_t;
  cl_context_info           = cl_uint;
  cl_command_queue_info     = cl_uint;
  cl_channel_order          = cl_uint;
  cl_channel_type           = cl_uint;
  cl_mem_flags              = cl_bitfield;
  cl_mem_object_type        = cl_uint;
  cl_mem_info               = cl_uint;
  cl_mem_migration_flags    = cl_bitfield;
  cl_image_info             = cl_uint;
  cl_buffer_create_type     = cl_uint;
  cl_addressing_mode        = cl_uint;
  cl_filter_mode            = cl_uint;
  cl_sampler_info           = cl_uint;
  cl_map_flags              = cl_bitfield;
  cl_program_info           = cl_uint;
  cl_program_build_info     = cl_uint;
  cl_program_binary_type    = cl_uint;
  cl_build_status           = cl_int;
  cl_kernel_info            = cl_uint;
  cl_kernel_arg_info        = cl_uint;
  cl_kernel_arg_address_qualifier = cl_uint;
  cl_kernel_arg_access_qualifier  = cl_uint;
  cl_kernel_arg_type_qualifier    = cl_bitfield;
  cl_kernel_work_group_info = cl_uint;
  cl_event_info             = cl_uint;
  cl_command_type           = cl_uint;
  cl_profiling_info         = cl_uint;

  cl_image_format = packed record
    image_channel_order     : cl_channel_order;
    image_channel_data_type : cl_channel_type;
  end;

  cl_image_desc = packed record
    image_type: cl_mem_object_type;
    image_width: size_t;
    image_height: size_t;
    image_depth: size_t;
    image_array_size: size_t;
    image_row_pitch: size_t;
    image_slice_pitch: size_t;
    num_mip_levels: cl_uint;
    num_samples: cl_uint;
    buffer: cl_mem;
  end;

  cl_buffer_region = packed record
    origin : size_t;
    size   : size_t;
  end;

  p_cl_command_queue_properties = ^cl_command_queue_properties;
  p_cl_context_properties       = ^cl_context_properties;
  p_cl_image_format             = ^cl_image_format;
  p_cl_device_partition_property= ^cl_device_partition_property;
  p_cl_image_desc               = ^cl_image_desc;

const
// Error Codes
  CL_SUCCESS                                   = 0;
  CL_DEVICE_NOT_FOUND                          = -1;
  CL_DEVICE_NOT_AVAILABLE                      = -2;
  CL_DEVICE_COMPILER_NOT_AVAILABLE             = -3;
  CL_MEM_OBJECT_ALLOCATION_FAILURE             = -4;
  CL_OUT_OF_RESOURCES                          = -5;
  CL_OUT_OF_HOST_MEMORY                        = -6;
  CL_PROFILING_INFO_NOT_AVAILABLE              = -7;
  CL_MEM_COPY_OVERLAP                          = -8;
  CL_IMAGE_FORMAT_MISMATCH                     = -9;
  CL_IMAGE_FORMAT_NOT_SUPPORTED                = -10;
  CL_BUILD_PROGRAM_FAILURE                     = -11;
  CL_MAP_FAILURE                               = -12;
  CL_MISALIGNED_SUB_BUFFER_OFFSET              = -13;
  CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST = -14;
  CL_COMPILE_PROGRAM_FAILURE                   = -15;
  CL_LINKER_NOT_AVAILABLE                      = -16;
  CL_LINK_PROGRAM_FAILURE                      = -17;
  CL_DEVICE_PARTITION_FAILED                   = -18;
  CL_KERNEL_ARG_INFO_NOT_AVAILABLE             = -19;

  CL_INVALID_VALUE                   = -30;
  CL_INVALID_DEVICE_TYPE             = -31;
  CL_INVALID_PLATFORM                = -32;
  CL_INVALID_DEVICE                  = -33;
  CL_INVALID_CONTEXT                 = -34;
  CL_INVALID_QUEUE_PROPERTIES        = -35;
  CL_INVALID_COMMAND_QUEUE           = -36;
  CL_INVALID_HOST_PTR                = -37;
  CL_INVALID_MEM_OBJECT              = -38;
  CL_INVALID_IMAGE_FORMAT_DESCRIPTOR = -39;
  CL_INVALID_IMAGE_SIZE              = -40;
  CL_INVALID_SAMPLER                 = -41;
  CL_INVALID_BINARY                  = -42;
  CL_INVALID_BUILD_OPTIONS           = -43;
  CL_INVALID_PROGRAM                 = -44;
  CL_INVALID_PROGRAM_EXECUTABLE      = -45;
  CL_INVALID_KERNEL_NAME             = -46;
  CL_INVALID_KERNEL_DEFINITION       = -47;
  CL_INVALID_KERNEL                  = -48;
  CL_INVALID_ARG_INDEX               = -49;
  CL_INVALID_ARG_VALUE               = -50;
  CL_INVALID_ARG_SIZE                = -51;
  CL_INVALID_KERNEL_ARGS             = -52;
  CL_INVALID_WORK_DIMENSION          = -53;
  CL_INVALID_WORK_GROUP_SIZE         = -54;
  CL_INVALID_WORK_ITEM_SIZE          = -55;
  CL_INVALID_GLOBAL_OFFSET           = -56;
  CL_INVALID_EVENT_WAIT_LIST         = -57;
  CL_INVALID_EVENT                   = -58;
  CL_INVALID_OPERATION               = -59;
  CL_INVALID_GL_OBJECT               = -60;
  CL_INVALID_BUFFER_SIZE             = -61;
  CL_INVALID_MIP_LEVEL               = -62;
  CL_INVALID_GLOBAL_WORK_SIZE        = -63;
  CL_INVALID_PROPERTY                = -64;
  CL_INVALID_IMAGE_DESCRIPTOR        = -65;
  CL_INVALID_COMPILER_OPTIONS        = -66;
  CL_INVALID_LINKER_OPTIONS          = -67;
  CL_INVALID_DEVICE_PARTITION_COUNT  = -68;

// OpenCL Version
  CL_VERSION_1_0 = 1;
  CL_VERSION_1_1 = 1;
  CL_VERSION_1_2 = 1;

// cl_bool
  CL_FALSE        = 0;
  CL_TRUE         = 1;
  CL_BLOCKING     = CL_TRUE;
  CL_NON_BLOCKING = CL_FALSE;

// cl_platform_info
  CL_PLATFORM_PROFILE    = $0900;
  CL_PLATFORM_VERSION    = $0901;
  CL_PLATFORM_NAME       = $0902;
  CL_PLATFORM_VENDOR     = $0903;
  CL_PLATFORM_EXTENSIONS = $0904;

// cl_device_type - bitfield
  CL_DEVICE_TYPE_DEFAULT     = (1 shl 0);
  CL_DEVICE_TYPE_CPU         = (1 shl 1);
  CL_DEVICE_TYPE_GPU         = (1 shl 2);
  CL_DEVICE_TYPE_ACCELERATOR = (1 shl 3);
  CL_DEVICE_TYPE_CUSTOM      = (1 shl 4);
  CL_DEVICE_TYPE_ALL         = $FFFFFFFF;

// cl_device_info
  CL_DEVICE_TYPE_INFO                     = $1000; // CL_DEVICE_TYPE
  CL_DEVICE_VENDOR_ID                     = $1001;
  CL_DEVICE_MAX_COMPUTE_UNITS             = $1002;
  CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS      = $1003;
  CL_DEVICE_MAX_WORK_GROUP_SIZE           = $1004;
  CL_DEVICE_MAX_WORK_ITEM_SIZES           = $1005;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR   = $1006;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT  = $1007;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT    = $1008;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG   = $1009;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT  = $100A;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE = $100B;
  CL_DEVICE_MAX_CLOCK_FREQUENCY           = $100C;
  CL_DEVICE_ADDRESS_BITS                  = $100D;
  CL_DEVICE_MAX_READ_IMAGE_ARGS           = $100E;
  CL_DEVICE_MAX_WRITE_IMAGE_ARGS          = $100F;
  CL_DEVICE_MAX_MEM_ALLOC_SIZE            = $1010;
  CL_DEVICE_IMAGE2D_MAX_WIDTH             = $1011;
  CL_DEVICE_IMAGE2D_MAX_HEIGHT            = $1012;
  CL_DEVICE_IMAGE3D_MAX_WIDTH             = $1013;
  CL_DEVICE_IMAGE3D_MAX_HEIGHT            = $1014;
  CL_DEVICE_IMAGE3D_MAX_DEPTH             = $1015;
  CL_DEVICE_IMAGE_SUPPORT                 = $1016;
  CL_DEVICE_MAX_PARAMETER_SIZE            = $1017;
  CL_DEVICE_MAX_SAMPLERS                  = $1018;
  CL_DEVICE_MEM_BASE_ADDR_ALIGN           = $1019;
  CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE      = $101A;
  CL_DEVICE_SINGLE_FP_CONFIG              = $101B;
  CL_DEVICE_GLOBAL_MEM_CACHE_TYPE         = $101C;
  CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE     = $101D;
  CL_DEVICE_GLOBAL_MEM_CACHE_SIZE         = $101E;
  CL_DEVICE_GLOBAL_MEM_SIZE               = $101F;
  CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE      = $1020;
  CL_DEVICE_MAX_CONSTANT_ARGS             = $1021;
  CL_DEVICE_LOCAL_MEM_TYPE_INFO           = $1022; // CL_DEVICE_LOCAL_MEM_TYPE
  CL_DEVICE_LOCAL_MEM_SIZE                = $1023;
  CL_DEVICE_ERROR_CORRECTION_SUPPORT      = $1024;
  CL_DEVICE_PROFILING_TIMER_RESOLUTION    = $1025;
  CL_DEVICE_ENDIAN_LITTLE                 = $1026;
  CL_DEVICE_AVAILABLE                     = $1027;
  CL_DEVICE_COMPILER_AVAILABLE            = $1028;
  CL_DEVICE_EXECUTION_CAPABILITIES        = $1029;
  CL_DEVICE_QUEUE_PROPERTIES              = $102A;
  CL_DEVICE_NAME                          = $102B;
  CL_DEVICE_VENDOR                        = $102C;
  CL_DRIVER_VERSION                       = $102D;
  CL_DEVICE_PROFILE                       = $102E;
  CL_DEVICE_VERSION                       = $102F;
  CL_DEVICE_EXTENSIONS                    = $1030;
  CL_DEVICE_PLATFORM                      = $1031;
  CL_DEVICE_DOUBLE_FP_CONFIG              = $1032;
  (* 0x1033 reserved for CL_DEVICE_HALF_FP_CONFIG *)
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF   = $1034;
  CL_DEVICE_HOST_UNIFIED_MEMORY           = $1035;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR      = $1036;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT     = $1037;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_INT       = $1038;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG      = $1039;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT     = $103A;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE    = $103B;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF      = $103C;
  CL_DEVICE_OPENCL_C_VERSION              = $103D;
  CL_DEVICE_LINKER_AVAILABLE              = $103E;
  CL_DEVICE_BUILT_IN_KERNELS              = $103F;
  CL_DEVICE_IMAGE_MAX_BUFFER_SIZE         = $1040;
  CL_DEVICE_IMAGE_MAX_ARRAY_SIZE          = $1041;
  CL_DEVICE_PARENT_DEVICE                 = $1042;
  CL_DEVICE_PARTITION_MAX_SUB_DEVICES     = $1043;
  CL_DEVICE_PARTITION_PROPERTIES          = $1044;
  CL_DEVICE_PARTITION_AFFINITY_DOMAIN     = $1045;
  CL_DEVICE_PARTITION_TYPE                = $1046;
  CL_DEVICE_REFERENCE_COUNT               = $1047;
  CL_DEVICE_PREFERRED_INTEROP_USER_SYNC   = $1048;
  CL_DEVICE_PRINTF_BUFFER_SIZE            = $1049;

// cl_device_fp_config - bitfield
  CL_FP_DENORM                        = (1 shl 0);
  CL_FP_INF_NAN                       = (1 shl 1);
  CL_FP_ROUND_TO_NEAREST              = (1 shl 2);
  CL_FP_ROUND_TO_ZERO                 = (1 shl 3);
  CL_FP_ROUND_TO_INF                  = (1 shl 4);
  CL_FP_FMA                           = (1 shl 5);
  CL_FP_SOFT_FLOAT                    = (1 shl 6);
  CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT = (1 shl 7);

// cl_device_mem_cache_type
  CL_NONE             = $0;
  CL_READ_ONLY_CACHE  = $1;
  CL_READ_WRITE_CACHE = $2;

// cl_device_local_mem_type
  CL_LOCAL  = $1;
  CL_GLOBAL = $2;

// cl_device_exec_capabilities - bitfield
  CL_EXEC_KERNEL        = (1 shl 0);
  CL_EXEC_NATIVE_KERNEL = (1 shl 1);

// cl_command_queue_properties - bitfield
  CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE = (1 shl 0);
  CL_QUEUE_PROFILING_ENABLE              = (1 shl 1);

// cl_context_info
  CL_CONTEXT_REFERENCE_COUNT = $1080;
  CL_CONTEXT_DEVICES         = $1081;
  CL_CONTEXT_PROPERTIES_INFO = $1082; // CL_CONTEXT_PROPERTIES
  CL_CONTEXT_NUM_DEVICES     = $1083;

// cl_context_properties
  CL_CONTEXT_PLATFORM_INFO     = $1084; // CL_CONTEXT_PLATFORM
  CL_CONTEXT_INTEROP_USER_SYNC = $1085;

// cl_device_partition_property
  CL_DEVICE_PARTITION_EQUALLY            = $1086;
  CL_DEVICE_PARTITION_BY_COUNTS          = $1087;
  CL_DEVICE_PARTITION_BY_COUNTS_LIST_END = $0;
  CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN = $1088;

// cl_device_affinity_domain
  CL_DEVICE_AFFINITY_DOMAIN_NUMA               = (1 shl 0);
  CL_DEVICE_AFFINITY_DOMAIN_L4_CACHE           = (1 shl 1);
  CL_DEVICE_AFFINITY_DOMAIN_L3_CACHE           = (1 shl 2);
  CL_DEVICE_AFFINITY_DOMAIN_L2_CACHE           = (1 shl 3);
  CL_DEVICE_AFFINITY_DOMAIN_L1_CACHE           = (1 shl 4);
  CL_DEVICE_AFFINITY_DOMAIN_NEXT_PARTITIONABLE = (1 shl 5);

// cl_command_queue_info
  CL_QUEUE_CONTEXT         = $1090;
  CL_QUEUE_DEVICE          = $1091;
  CL_QUEUE_REFERENCE_COUNT = $1092;
  CL_QUEUE_PROPERTIES      = $1093;

// cl_mem_flags - bitfield
  CL_MEM_READ_WRITE     = (1 shl 0);
  CL_MEM_WRITE_ONLY     = (1 shl 1);
  CL_MEM_READ_ONLY      = (1 shl 2);
  CL_MEM_USE_HOST_PTR   = (1 shl 3);
  CL_MEM_ALLOC_HOST_PTR = (1 shl 4);
  CL_MEM_COPY_HOST_PTR  = (1 shl 5);
  // reserved                                  = (1 shl 6);
  CL_MEM_HOST_WRITE_ONLY                       = (1 shl 7);
  CL_MEM_HOST_READ_ONLY                        = (1 shl 8);
  CL_MEM_HOST_NO_ACCESS                        = (1 shl 9);

// cl_mem_migration_flags - bitfield
  CL_MIGRATE_MEM_OBJECT_HOST              = (1 shl 0);
  CL_MIGRATE_MEM_OBJECT_CONTENT_UNDEFINED = (1 shl 1);

// cl_channel_order
  CL_R             = $10B0;
  CL_A             = $10B1;
  CL_RG            = $10B2;
  CL_RA            = $10B3;
  CL_RGB           = $10B4;
  CL_RGBA          = $10B5;
  CL_BGRA          = $10B6;
  CL_ARGB          = $10B7;
  CL_INTENSITY     = $10B8;
  CL_LUMINANCE     = $10B9;
  CL_Rx            = $10BA;
  CL_RGx           = $10BB;
  CL_RGBx          = $10BC;
  CL_DEPTH         = $10BD;
  CL_DEPTH_STENCIL = $10BE;

// cl_channel_type
  CL_SNORM_INT8       = $10D0;
  CL_SNORM_INT16      = $10D1;
  CL_UNORM_INT8       = $10D2;
  CL_UNORM_INT16      = $10D3;
  CL_UNORM_SHORT_565  = $10D4;
  CL_UNORM_SHORT_555  = $10D5;
  CL_UNORM_INT_101010 = $10D6;
  CL_SIGNED_INT8      = $10D7;
  CL_SIGNED_INT16     = $10D8;
  CL_SIGNED_INT32     = $10D9;
  CL_UNSIGNED_INT8    = $10DA;
  CL_UNSIGNED_INT16   = $10DB;
  CL_UNSIGNED_INT32   = $10DC;
  CL_HALF_FLOAT       = $10DD;
  CL_FLOAT_TYPE       = $10DE; // CL_FLOAT
  CL_UNORM_INT24      = $10DF;

// cl_mem_object_type      
  CL_MEM_OBJECT_BUFFER         = $10F0;
  CL_MEM_OBJECT_IMAGE2D        = $10F1;
  CL_MEM_OBJECT_IMAGE3D        = $10F2;
  CL_MEM_OBJECT_IMAGE2D_ARRAY  = $10F3;
  CL_MEM_OBJECT_IMAGE1D        = $10F4;
  CL_MEM_OBJECT_IMAGE1D_ARRAY  = $10F5;
  CL_MEM_OBJECT_IMAGE1D_BUFFER = $10F6;

// cl_mem_info
  CL_MEM_TYPE                 = $1100;
  CL_MEM_FLAGS_INFO           = $1101; // CL_MEM_FLAGS
  CL_MEM_SIZE                 = $1102;
  CL_MEM_HOST_PTR             = $1103;
  CL_MEM_MAP_COUNT            = $1104;
  CL_MEM_REFERENCE_COUNT      = $1105;
  CL_MEM_CONTEXT              = $1106;
  CL_MEM_ASSOCIATED_MEMOBJECT = $1107;
  CL_MEM_OFFSET               = $1108;

// cl_image_info
  CL_IMAGE_FORMAT_INFO    = $1110; // CL_IMAGE_FORMAT
  CL_IMAGE_ELEMENT_SIZE   = $1111;
  CL_IMAGE_ROW_PITCH      = $1112;
  CL_IMAGE_SLICE_PITCH    = $1113;
  CL_IMAGE_WIDTH          = $1114;
  CL_IMAGE_HEIGHT         = $1115;
  CL_IMAGE_DEPTH          = $1116;
  CL_IMAGE_ARRAY_SIZE     = $1117;
  CL_IMAGE_BUFFER         = $1118;
  CL_IMAGE_NUM_MIP_LEVELS = $1119;
  CL_IMAGE_NUM_SAMPLES    = $111A;

// cl_addressing_mode
  CL_ADDRESS_NONE            = $1130;
  CL_ADDRESS_CLAMP_TO_EDGE   = $1131;
  CL_ADDRESS_CLAMP           = $1132;
  CL_ADDRESS_REPEAT          = $1133;
  CL_ADDRESS_MIRRORED_REPEAT = $1134;

// cl_filter_mode
  CL_FILTER_NEAREST = $1140;
  CL_FILTER_LINEAR  = $1141;

// cl_sampler_info
  CL_SAMPLER_REFERENCE_COUNT   = $1150;
  CL_SAMPLER_CONTEXT           = $1151;
  CL_SAMPLER_NORMALIZED_COORDS = $1152;
  CL_SAMPLER_ADDRESSING_MODE   = $1153;
  CL_SAMPLER_FILTER_MODE       = $1154;

// cl_map_flags - bitfield
  CL_MAP_READ                    = (1 shl 0);
  CL_MAP_WRITE                   = (1 shl 1);
  CL_MAP_WRITE_INVALIDATE_REGION = (1 shl 2);

// cl_program_info
  CL_PROGRAM_REFERENCE_COUNT = $1160;
  CL_PROGRAM_CONTEXT         = $1161;
  CL_PROGRAM_NUM_DEVICES     = $1162;
  CL_PROGRAM_DEVICES         = $1163;
  CL_PROGRAM_SOURCE          = $1164;
  CL_PROGRAM_BINARY_SIZES    = $1165;
  CL_PROGRAM_BINARIES        = $1166;
  CL_PROGRAM_NUM_KERNELS     = $1167;
  CL_PROGRAM_KERNEL_NAMES    = $1168;

// cl_program_build_info
  CL_PROGRAM_BUILD_STATUS     = $1181;
  CL_PROGRAM_BUILD_OPTIONS    = $1182;
  CL_PROGRAM_BUILD_LOG        = $1183;
  CL_PROGRAM_BINARY_TYPE_INFO = $1184; // CL_PROGRAM_BINARY_TYPE

// cl_program_binary_type
  CL_PROGRAM_BINARY_TYPE_NONE            = $0;
  CL_PROGRAM_BINARY_TYPE_COMPILED_OBJECT = $1;
  CL_PROGRAM_BINARY_TYPE_LIBRARY         = $2;
  CL_PROGRAM_BINARY_TYPE_EXECUTABLE      = $4;

// cl_build_status
  CL_BUILD_SUCCESS     = 0;
  CL_BUILD_NONE        = -1;
  CL_BUILD_ERROR       = -2;
  CL_BUILD_IN_PROGRESS = -3;

// cl_kernel_info
  CL_KERNEL_FUNCTION_NAME   = $1190;
  CL_KERNEL_NUM_ARGS        = $1191;
  CL_KERNEL_REFERENCE_COUNT = $1192;
  CL_KERNEL_CONTEXT         = $1193;
  CL_KERNEL_PROGRAM         = $1194;
  CL_KERNEL_ATTRIBUTES      = $1195;

// cl_kernel_arg_info
  CL_KERNEL_ARG_ADDRESS_QUALIFIER_INFO = $1196; // CL_KERNEL_ARG_ADDRESS_QUALIFIER
  CL_KERNEL_ARG_ACCESS_QUALIFIER_INFO  = $1197; // CL_KERNEL_ARG_ACCESS_QUALIFIER
  CL_KERNEL_ARG_TYPE_NAME              = $1198;
  CL_KERNEL_ARG_TYPE_QUALIFIER_INFO    = $1199; // CL_KERNEL_ARG_TYPE_QUALIFIER
  CL_KERNEL_ARG_NAME                   = $119A;

// cl_kernel_arg_address_qualifier
  CL_KERNEL_ARG_ADDRESS_GLOBAL   = $119B;
  CL_KERNEL_ARG_ADDRESS_LOCAL    = $119C;
  CL_KERNEL_ARG_ADDRESS_CONSTANT = $119D;
  CL_KERNEL_ARG_ADDRESS_PRIVATE  = $119E;

// cl_kernel_arg_access_qualifier
  CL_KERNEL_ARG_ACCESS_READ_ONLY  = $11A0;
  CL_KERNEL_ARG_ACCESS_WRITE_ONLY = $11A1;
  CL_KERNEL_ARG_ACCESS_READ_WRITE = $11A2;
  CL_KERNEL_ARG_ACCESS_NONE       = $11A3;

// cl_kernel_arg_type_qualifer
  CL_KERNEL_ARG_TYPE_NONE     = 0;
  CL_KERNEL_ARG_TYPE_CONST    = (1 shl 0);
  CL_KERNEL_ARG_TYPE_RESTRICT = (1 shl 1);
  CL_KERNEL_ARG_TYPE_VOLATILE = (1 shl 2);

// cl_kernel_work_group_info
  CL_KERNEL_WORK_GROUP_SIZE                    = $11B0;
  CL_KERNEL_COMPILE_WORK_GROUP_SIZE            = $11B1;
  CL_KERNEL_LOCAL_MEM_SIZE                     = $11B2;
  CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE = $11B3;
  CL_KERNEL_PRIVATE_MEM_SIZE                   = $11B4;
  CL_KERNEL_GLOBAL_WORK_SIZE                   = $11B5;

// cl_event_info
  CL_EVENT_COMMAND_QUEUE            = $11D0;
  CL_EVENT_COMMAND_TYPE             = $11D1;
  CL_EVENT_REFERENCE_COUNT          = $11D2;
  CL_EVENT_COMMAND_EXECUTION_STATUS = $11D3;
  CL_EVENT_CONTEXT                  = $11D4;

// cl_command_type
  CL_COMMAND_NDRANGE_KERNEL       = $11F0;
  CL_COMMAND_TASK                 = $11F1;
  CL_COMMAND_NATIVE_KERNEL        = $11F2;
  CL_COMMAND_READ_BUFFER          = $11F3;
  CL_COMMAND_WRITE_BUFFER         = $11F4;
  CL_COMMAND_COPY_BUFFER          = $11F5;
  CL_COMMAND_READ_IMAGE           = $11F6;
  CL_COMMAND_WRITE_IMAGE          = $11F7;
  CL_COMMAND_COPY_IMAGE           = $11F8;
  CL_COMMAND_COPY_IMAGE_TO_BUFFER = $11F9;
  CL_COMMAND_COPY_BUFFER_TO_IMAGE = $11FA;
  CL_COMMAND_MAP_BUFFER           = $11FB;
  CL_COMMAND_MAP_IMAGE            = $11FC;
  CL_COMMAND_UNMAP_MEM_OBJECT     = $11FD;
  CL_COMMAND_MARKER               = $11FE;
  CL_COMMAND_ACQUIRE_GL_OBJECTS   = $11FF;
  CL_COMMAND_RELEASE_GL_OBJECTS   = $1200;
  CL_COMMAND_READ_BUFFER_RECT     = $1201;
  CL_COMMAND_WRITE_BUFFER_RECT    = $1202;
  CL_COMMAND_COPY_BUFFER_RECT     = $1203;
  CL_COMMAND_USER                 = $1204;
  CL_COMMAND_BARRIER              = $1205;
  CL_COMMAND_MIGRATE_MEM_OBJECTS  = $1206;
  CL_COMMAND_FILL_BUFFER          = $1207;
  CL_COMMAND_FILL_IMAGE           = $1208;

// command execution status
  CL_COMPLETE  = $0;
  CL_RUNNING   = $1;
  CL_SUBMITTED = $2;
  CL_QUEUED    = $3;

// cl_buffer_create_type
  CL_BUFFER_CREATE_TYPE_REGION = $1220;

// cl_profiling_info
  CL_PROFILING_COMMAND_QUEUED = $1280;
  CL_PROFILING_COMMAND_SUBMIT = $1281;
  CL_PROFILING_COMMAND_START  = $1282;
  CL_PROFILING_COMMAND_END    = $1283;

// ****************************************************************************

type
// Platform APIs
  _t_clGetPlatformIDs = function( // CL_API_SUFFIX__VERSION_1_0
    num_entries   : cl_uint;
    platforms     : p_cl_platform_id;
    num_platforms : p_cl_uint
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clGetPlatformInfo = function( // CL_API_SUFFIX__VERSION_1_0
    _platform            : cl_platform_id;
    param_name           : cl_platform_info;
    param_value_size     : size_t;
    param_value          : pointer;
    param_value_size_ret : p_size_t
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

// Device APIs
  _t_clGetDeviceIDs = function( // CL_API_SUFFIX__VERSION_1_0
    _platform   : cl_platform_id;
    device_type : cl_device_type;
    num_entries : cl_uint;
    devices     : p_cl_device_id;
    num_devices : p_cl_uint
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clGetDeviceInfo = function( // CL_API_SUFFIX__VERSION_1_0
    device               : cl_device_id;
    param_name           : cl_device_info;
    param_value_size     : size_t;
    param_value          : pointer;
    param_value_size_ret : p_size_t
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clCreateSubDevices = function( // CL_API_SUFFIX__VERSION_1_2
    in_device       : cl_device_id;
    properties      : p_cl_device_partition_property;
    num_devices     : cl_uint;
    out_devices     : p_cl_device_id;
    num_devices_ret : p_cl_uint
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clRetainDevice = function( // CL_API_SUFFIX__VERSION_1_2
    device : cl_device_id
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clReleaseDevice = function( // CL_API_SUFFIX__VERSION_1_2
    device : cl_device_id
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

// Context APIs
type
  TContextNotify = procedure (name: PAnsiChar; data1: pointer; size: size_t; data2: pointer); {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clCreateContext = function( // CL_API_SUFFIX__VERSION_1_0
    properties  : p_cl_context_properties;
    num_devices : cl_uint;
    devices     : p_cl_device_id;
    pfn_notify  : TContextNotify;
    user_data   : pointer;
    errcode_ret : p_cl_int
    ): cl_context; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clCreateContextFromType = function( // CL_API_SUFFIX__VERSION_1_0
    properties  : p_cl_context_properties;
    device_type : cl_device_type;
    pfn_notify  : TContextNotify;
    user_data   : pointer;
    errcode_ret : p_cl_int
    ): cl_context; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clRetainContext = function( // CL_API_SUFFIX__VERSION_1_0
    context : cl_context
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clReleaseContext = function( // CL_API_SUFFIX__VERSION_1_0
    context : cl_context
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clGetContextInfo = function( // CL_API_SUFFIX__VERSION_1_0
    context              : cl_context;
    param_name           : cl_context_info;
    param_value_size     : size_t;
    param_value          : pointer;
    param_value_size_ret : p_size_t
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
// Command Queue APIs
  _t_clCreateCommandQueue = function( // CL_API_SUFFIX__VERSION_1_0
    context     : cl_context;
    device      : cl_device_id;
    properties  : cl_command_queue_properties;
    errcode_ret : p_cl_int
    ): cl_command_queue; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clRetainCommandQueue = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue : cl_command_queue
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clReleaseCommandQueue = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue : cl_command_queue
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clGetCommandQueueInfo = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue        : cl_command_queue;
    param_name           : cl_command_queue_info;
    param_value_size     : size_t;
    param_value          : pointer;
    param_value_size_ret : p_size_t
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF}; 
  
  (*  WARNING:
   *     This API introduces mutable state into the OpenCL implementation. It has been REMOVED
   *  to better facilitate thread safety.  The 1.0 API is not thread safe. It is not tested by the
   *  OpenCL 1.1 conformance test, and consequently may not work or may not work dependably.
   *  It is likely to be non-performant. Use of this API is not advised. Use at your own risk.
   *
   *  Software developers previously relying on this API are instructed to set the command queue
   *  properties when creating the queue, instead.
  *)
  _t_clSetCommandQueueProperty = function( // CL_EXT_SUFFIX__VERSION_1_0_DEPRECATED
    command_queue  : cl_command_queue;
    properties     : cl_command_queue_properties;
    enable         : cl_bool;
    old_properties : p_cl_command_queue_properties
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF}; 
  
// Memory Object APIs
  _t_clCreateBuffer = function( // CL_API_SUFFIX__VERSION_1_0
    context     : cl_context;
    flags       : cl_mem_flags;
    size        : size_t;
    host_ptr    : pointer;
    errcode_ret : p_cl_int
    ): cl_mem; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clCreateSubBuffer = function( // CL_API_SUFFIX__VERSION_1_1
    buffer             : cl_mem;
    flags              : cl_mem_flags;
    buffer_create_type : cl_buffer_create_type;
    buffer_create_info : pointer;
    errcode_ret        : p_cl_int
    ): cl_mem; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clCreateImage = function( // CL_API_SUFFIX__VERSION_1_2
    context            : cl_context;
    flags              : cl_mem_flags;
    image_format       : p_cl_image_format;
    image_desc         : p_cl_image_desc;
    host_ptr           : pointer;
    errcode_ret        : p_cl_int
    ): cl_mem; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clRetainMemObject = function( // CL_API_SUFFIX__VERSION_1_0
    memobj : cl_mem
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clReleaseMemObject = function( // CL_API_SUFFIX__VERSION_1_0
    memobj : cl_mem
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clGetSupportedImageFormats = function( // CL_API_SUFFIX__VERSION_1_0
    context           : cl_context;
    flags             : cl_mem_flags;
    image_type        : cl_mem_object_type;
    num_entries       : cl_uint;
    image_formats     : p_cl_image_format;
    num_image_formats : p_cl_uint
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clGetMemObjectInfo = function( // CL_API_SUFFIX__VERSION_1_0
    memobj               : cl_mem;
    param_name           : cl_mem_info;
    param_value_size     : size_t;
    param_value          : pointer;
    param_value_size_ret : p_size_t
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clGetImageInfo = function( // CL_API_SUFFIX__VERSION_1_0
    image                : cl_mem;
    param_name           : cl_image_info;
    param_value_size     : size_t;
    param_value          : pointer;
    param_value_size_ret : p_size_t
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

type
  TDestructorNotify = procedure (memobj: cl_mem; user_data: pointer); {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clSetMemObjectDestructorCallback = function( // CL_API_SUFFIX__VERSION_1_1
    memobj     : cl_mem;
    pfn_notify : TDestructorNotify;
    user_data  : pointer
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
// Sampler APIs
  _t_clCreateSampler = function( // CL_API_SUFFIX__VERSION_1_0
    context           : cl_context;
    normalized_coords : cl_bool;
    addressing_mode   : cl_addressing_mode;
    filter_mode       : cl_filter_mode;
    errcode_ret       : p_cl_int
    ): cl_sampler; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clRetainSampler = function( // CL_API_SUFFIX__VERSION_1_0
    sampler: cl_sampler
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clReleaseSampler = function( // CL_API_SUFFIX__VERSION_1_0
    sampler: cl_sampler
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clGetSamplerInfo = function( // CL_API_SUFFIX__VERSION_1_0
    sampler              : cl_sampler;
    param_name           : cl_sampler_info;
    param_value_size     : size_t;
    param_value          : pointer;
    param_value_size_ret : p_size_t
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
// Program Object APIs
  _t_clCreateProgramWithSource = function( // CL_API_SUFFIX__VERSION_1_0
    context     : cl_context;
    count       : cl_uint;
    strings     : pPAnsiChar;
    lengths     : p_size_t;
    errcode_ret : p_cl_int
    ): cl_program; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clCreateProgramWithBinary = function( // CL_API_SUFFIX__VERSION_1_0
    context       : cl_context;
    num_devices   : cl_uint;
    device_list   : p_cl_device_id;
    lengths       : p_size_t;
    binaries      : pp_byte;
    binary_status : p_cl_int;
    errcode_ret   : p_cl_int
    ): cl_program; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clCreateProgramWithBuiltInKernels = function( // CL_API_SUFFIX__VERSION_1_2
    context       : cl_context;
    num_devices   : cl_uint;
    device_list   : p_cl_device_id;
    kernel_names  : PAnsiChar;
    errcode_ret   : p_cl_int
    ): cl_program; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clRetainProgram = function( // CL_API_SUFFIX__VERSION_1_0
    _program: cl_program
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clReleaseProgram = function( // CL_API_SUFFIX__VERSION_1_0
    _program: cl_program
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

type
  TProgramNotify = procedure (_program: cl_program; user_data: pointer); {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clBuildProgram = function( // CL_API_SUFFIX__VERSION_1_0
    _program     : cl_program;
    num_devices  : cl_uint;
    device_list  : p_cl_device_id;
    options      : PAnsiChar;
    pfn_notify   : TProgramNotify;
    user_data    : pointer
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clCompileProgram = function( // CL_API_SUFFIX__VERSION_1_2
    _program             : cl_program;
    num_devices          : cl_uint;
    device_list          : p_cl_device_id;
    options              : PAnsiChar;
    num_input_headers    : cl_uint;
    input_headers        : p_cl_program;
    header_include_names : pPAnsiChar;
    pfn_notify           : TProgramNotify;
    user_data            : pointer
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clLinkProgram = function( // CL_API_SUFFIX__VERSION_1_2
    context            : cl_context;
    num_devices        : cl_uint;
    device_list        : p_cl_device_id;
    options            : PAnsiChar;
    num_input_programs : cl_uint;
    input_programs     : p_cl_program;
    pfn_notify         : TProgramNotify;
    user_data          : pointer;
    errcode_ret        : p_cl_int
    ): cl_program; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clUnloadPlatformCompiler = function( // CL_API_SUFFIX__VERSION_1_2
    platform : cl_platform_id
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clGetProgramInfo = function( // CL_API_SUFFIX__VERSION_1_0
    _program             : cl_program;
    param_name           : cl_program_info;
    param_value_size     : size_t;
    param_value          : pointer;
    param_value_size_ret : p_size_t
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clGetProgramBuildInfo = function( // CL_API_SUFFIX__VERSION_1_0
    _program             : cl_program;
    device               : cl_device_id;
    param_name           : cl_program_build_info;
    param_value_size     : size_t;
    param_value          : pointer;
    param_value_size_ret : p_size_t
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
// Kernel Object APIs
  _t_clCreateKernel = function( // CL_API_SUFFIX__VERSION_1_0
    _program    : cl_program;
    kernel_name : PAnsiChar;
    errcode_ret : p_cl_int
    ): cl_kernel; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clCreateKernelsInProgram = function( // CL_API_SUFFIX__VERSION_1_0
    _program        : cl_program;
    num_kernels     : cl_uint;
    kernels         : p_cl_kernel;
    num_kernels_ret : p_cl_uint
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clRetainKernel = function( // CL_API_SUFFIX__VERSION_1_0
    kernel: cl_kernel
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clReleaseKernel = function( // CL_API_SUFFIX__VERSION_1_0
    kernel: cl_kernel
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
    
  _t_clSetKernelArg = function( // CL_API_SUFFIX__VERSION_1_0
    kernel    : cl_kernel;
    arg_index : cl_uint;
    arg_size  : size_t;
    arg_value : pointer
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clGetKernelInfo = function( // CL_API_SUFFIX__VERSION_1_0
    kernel               : cl_kernel;
    param_name           : cl_kernel_info;
    param_value_size     : size_t;
    param_value          : pointer;
    param_value_size_ret : p_size_t
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clGetKernelArgInfo = function( // CL_API_SUFFIX__VERSION_1_2
    kernel               : cl_kernel;
    arg_indx             : cl_uint;
    param_name           : cl_kernel_arg_info;
    param_value_size     : size_t;
    param_value          : pointer;
    param_value_size_ret : p_size_t
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clGetKernelWorkGroupInfo = function( // CL_API_SUFFIX__VERSION_1_0
    kernel               : cl_kernel;
    device               : cl_device_id;
    param_name           : cl_kernel_work_group_info;
    param_value_size     : size_t;
    param_value          : pointer;
    param_value_size_ret : p_size_t
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

// Event Object APIs
  _t_clWaitForEvents = function( // CL_API_SUFFIX__VERSION_1_0
    num_events  : cl_uint;
    event_list  : cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clGetEventInfo = function( // CL_API_SUFFIX__VERSION_1_0
    event                : cl_event;
    param_name           : cl_event_info;
    param_value_size     : size_t;
    param_value          : pointer;
    param_value_size_ret : p_size_t
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clCreateUserEvent = function( // CL_API_SUFFIX__VERSION_1_1
    context     : cl_context;
    errcode_ret : p_cl_int
    ): cl_event; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clRetainEvent = function( // CL_API_SUFFIX__VERSION_1_0
    event: cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clReleaseEvent = function( // CL_API_SUFFIX__VERSION_1_0
    event: cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clSetUserEventStatus = function( // CL_API_SUFFIX__VERSION_1_1
    event            : cl_event;
    execution_status : cl_int
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

type
  TEventNotify = procedure (event: cl_event; event_command_exec_status: cl_int; user_data: pointer); {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clSetEventCallback = function( // CL_API_SUFFIX__VERSION_1_1
    event                      : cl_event;
    command_exec_callback_type : cl_int;
    pfn_notify                 : TEventNotify;
    user_data                  : pointer
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
// Profiling APIs
  _t_clGetEventProfilingInfo = function( // CL_API_SUFFIX__VERSION_1_0
    event                : cl_event;
    param_name           : cl_profiling_info;
    param_value_size     : size_t;
    param_value          : pointer;
    param_value_size_ret : p_size_t
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

// Flush and Finish APIs
  _t_clFlush = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue: cl_command_queue
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
    
  _t_clFinish = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue: cl_command_queue
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
// Enqueued Commands APIs
  _t_clEnqueueReadBuffer = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue           : cl_command_queue;
    buffer                  : cl_mem;
    blocking_read           : cl_bool;
    offset                  : size_t;
    size                    : size_t;
    ptr                     : pointer;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clEnqueueReadBufferRect = function( // CL_API_SUFFIX__VERSION_1_1
    command_queue           : cl_command_queue;
    buffer                  : cl_mem;
    blocking_read           : cl_bool;
    buffer_offset           : p_size_t;
    host_offset             : p_size_t;
    region                  : p_size_t;
    buffer_row_pitch        : size_t;
    buffer_slice_pitch      : size_t;
    host_row_pitch          : size_t;
    host_slice_pitch        : size_t;
    ptr                     : pointer;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event            
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clEnqueueWriteBuffer = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue           : cl_command_queue;
    buffer                  : cl_mem;
    blocking_write          : cl_bool;
    offset                  : size_t;
    size                    : size_t;
    ptr                     : pointer;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clEnqueueWriteBufferRect = function( // CL_API_SUFFIX__VERSION_1_1
    command_queue           : cl_command_queue;
    buffer                  : cl_mem;
    blocking_write          : cl_bool;
    buffer_offset           : p_size_t;
    host_offset             : p_size_t;
    region                  : p_size_t;
    buffer_row_pitch        : size_t;
    buffer_slice_pitch      : size_t;
    host_row_pitch          : size_t;
    host_slice_pitch        : size_t;
    ptr                     : pointer;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clEnqueueFillBuffer = function( // CL_API_SUFFIX__VERSION_1_2
    command_queue           : cl_command_queue;
    buffer                  : cl_mem;
    pattern                 : pointer;
    pattern_size            : size_t;
    offset                  : size_t;
    size                    : size_t;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clEnqueueCopyBuffer = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue           : cl_command_queue;
    src_buffer              : cl_mem;
    dst_buffer              : cl_mem;
    src_offset              : size_t;
    dst_offset              : size_t;
    size                      : size_t;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clEnqueueCopyBufferRect = function( // CL_API_SUFFIX__VERSION_1_1
    command_queue           : cl_command_queue;
    src_buffer              : cl_mem;
    dst_buffer              : cl_mem;
    src_origin              : p_size_t;
    dst_origin              : p_size_t;
    region                  : p_size_t;
    src_row_pitch           : size_t;
    src_slice_pitch         : size_t;
    dst_row_pitch           : size_t;
    dst_slice_pitch         : size_t;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clEnqueueReadImage = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue           : cl_command_queue;
    image                   : cl_mem;
    blocking_read           : cl_bool;
    origin3D                : p_size_t;
    region3D                : p_size_t;
    row_pitch               : size_t;
    slice_pitch             : size_t;
    ptr                     : pointer;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clEnqueueWriteImage = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue           : cl_command_queue;
    image                   : cl_mem;
    blocking_write          : cl_bool;
    origin3D                : p_size_t;
    region3D                : p_size_t;
    input_row_pitch         : size_t;
    input_slice_pitch       : size_t;
    ptr                     : pointer;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clEnqueueFillImage = function( // CL_API_SUFFIX__VERSION_1_2
    command_queue           : cl_command_queue;
    image                   : cl_mem;
    fill_color              : pointer;
    origin3D                : p_size_t;
    region3D                : p_size_t;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clEnqueueCopyImage = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue           : cl_command_queue;
    src_image               : cl_mem;
    dst_image               : cl_mem;
    src_origin3D            : p_size_t;
    dst_origin3D            : p_size_t;
    region3D                : p_size_t;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clEnqueueCopyImageToBuffer = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue           : cl_command_queue;
    src_image               : cl_mem;
    dst_buffer              : cl_mem;
    src_origin3D            : p_size_t;
    region3D                : p_size_t;
    dst_offset              : size_t;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clEnqueueCopyBufferToImage = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue           : cl_command_queue;
    src_buffer              : cl_mem;
    dst_image               : cl_mem;
    src_offset              : size_t;
    dst_origin3D            : p_size_t;
    region3D                : p_size_t;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clEnqueueMapBuffer = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue           : cl_command_queue;
    buffer                  : cl_mem;
    blocking_map            : cl_bool;
    map_flags               : cl_map_flags;
    offset                  : size_t;
    size                    : size_t;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event;
    errcode_ret             : p_cl_int
    ): pointer; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clEnqueueMapImage = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue           : cl_command_queue;
    image                   : cl_mem;
    blocking_map            : cl_bool;
    map_flags               : cl_map_flags;
    origin3D                : p_size_t;
    region3D                : p_size_t;
    row_pitch               : size_t;
    slice_pitch             : size_t;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event;
    errcode_ret             : p_cl_int
    ): pointer; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clEnqueueUnmapMemObject = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue           : cl_command_queue;
    memobj                  : cl_mem;
    mapped_ptr              : pointer;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clEnqueueMigrateMemObjects = function( // CL_API_SUFFIX__VERSION_1_2
    command_queue           : cl_command_queue;
    num_mem_objects         : cl_uint;
    mem_objects             : p_cl_mem;
    flags                   : cl_mem_migration_flags;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clEnqueueNDRangeKernel = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue           : cl_command_queue;
    kernel                  : cl_kernel;
    work_dim                : cl_uint;
    global_work_offset      : p_size_t;           
    global_work_size        : p_size_t;
    local_work_size         : p_size_t;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clEnqueueTask = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue           : cl_command_queue;
    kernel                  : cl_kernel;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

type
  TEnqueueUserProc = procedure (userdata: pointer); {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clEnqueueNativeKernel = function( // CL_API_SUFFIX__VERSION_1_0
    command_queue           : cl_command_queue;
    user_proc               : TEnqueueUserProc;
    args                    : pointer;
    cb_args                 : size_t;
    num_mem_objects         : cl_uint;
    mem_list                : p_cl_mem;
    args_mem_loc            : p_pointer;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  
  _t_clEnqueueMarkerWithWaitList = function( // CL_API_SUFFIX__VERSION_1_2
    command_queue: cl_command_queue;
    num_events_in_wait_list: cl_uint;
    event_wait_list: p_cl_event;
    event: p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clEnqueueBarrierWithWaitList = function( // CL_API_SUFFIX__VERSION_1_2
    command_queue: cl_command_queue;
    num_events_in_wait_list: cl_uint;
    event_wait_list: p_cl_event;
    event: p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

// Deprecated OpenCL 1.1 APIs
  _t_clCreateImage2D = function( // CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    context         : cl_context;
    flags           : cl_mem_flags;
    image_format    : p_cl_image_format;
    image_width     : size_t;
    image_height    : size_t;
    image_row_pitch : size_t;
    host_ptr        : pointer;
    errcode_ret     : p_cl_int
    ): cl_mem; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clCreateImage3D = function( // CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    context            : cl_context;
    flags              : cl_mem_flags;
    image_format       : p_cl_image_format;
    image_width        : size_t;
    image_height       : size_t;
    image_depth        : size_t;
    image_row_pitch    : size_t;
    image_slice_pitch  : size_t;
    host_ptr           : pointer;
    errcode_ret        : p_cl_int
    ): cl_mem; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clEnqueueMarker = function( // CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    command_queue : cl_command_queue;
    event         : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clEnqueueWaitForEvents = function( // CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    command_queue : cl_command_queue;
    num_events    : cl_uint;
    event_list    : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clEnqueueBarrier = function( // CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    command_queue : cl_command_queue
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clUnloadCompiler = function( // CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

var
  clGetPlatformIDs: _t_clGetPlatformIDs;
  clGetPlatformInfo: _t_clGetPlatformInfo;
// Device APIs
  clGetDeviceIDs: _t_clGetDeviceIDs;
  clGetDeviceInfo: _t_clGetDeviceInfo;
  clCreateSubDevices: _t_clCreateSubDevices;
  clRetainDevice: _t_clRetainDevice;
  clReleaseDevice: _t_clReleaseDevice;
// Context APIs
  clCreateContext: _t_clCreateContext;
  clCreateContextFromType: _t_clCreateContextFromType;
  clRetainContext: _t_clRetainContext;
  clReleaseContext: _t_clReleaseContext; 
  clGetContextInfo: _t_clGetContextInfo; 
// Command Queue APIs
  clCreateCommandQueue: _t_clCreateCommandQueue; 
  clRetainCommandQueue: _t_clRetainCommandQueue; 
  clReleaseCommandQueue: _t_clReleaseCommandQueue; 
  clGetCommandQueueInfo: _t_clGetCommandQueueInfo; 
  (*  WARNING:
   *     This API introduces mutable state into the OpenCL implementation. It has been REMOVED
   *  to better facilitate thread safety.  The 1.0 API is not thread safe. It is not tested by the
   *  OpenCL 1.1 conformance test, and consequently may not work or may not work dependably.
   *  It is likely to be non-performant. Use of this API is not advised. Use at your own risk.
   *
   *  Software developers previously relying on this API are instructed to set the command queue
   *  properties when creating the queue, instead.
  clSetCommandQueueProperty: _t_clSetCommandQueueProperty;
  *)
// Memory Object APIs
  clCreateBuffer: _t_clCreateBuffer;
  clCreateSubBuffer: _t_clCreateSubBuffer;
  clCreateImage: _t_clCreateImage;
  clRetainMemObject: _t_clRetainMemObject;
  clReleaseMemObject: _t_clReleaseMemObject;
  clGetSupportedImageFormats: _t_clGetSupportedImageFormats;
  clGetMemObjectInfo: _t_clGetMemObjectInfo;
  clGetImageInfo: _t_clGetImageInfo;
  clSetMemObjectDestructorCallback: _t_clSetMemObjectDestructorCallback;
// Sampler APIs
  clCreateSampler: _t_clCreateSampler;
  clRetainSampler: _t_clRetainSampler;
  clReleaseSampler: _t_clReleaseSampler;
  clGetSamplerInfo: _t_clGetSamplerInfo;
// Program Object APIs
  clCreateProgramWithSource: _t_clCreateProgramWithSource;
  clCreateProgramWithBinary: _t_clCreateProgramWithBinary;
  clCreateProgramWithBuiltInKernels: _t_clCreateProgramWithBuiltInKernels;
  clRetainProgram: _t_clRetainProgram;
  clReleaseProgram: _t_clReleaseProgram;
  clBuildProgram: _t_clBuildProgram;
  clCompileProgram: _t_clCompileProgram;
  clLinkProgram: _t_clLinkProgram;
  clUnloadPlatformCompiler: _t_clUnloadPlatformCompiler;
  clGetProgramInfo: _t_clGetProgramInfo;
  clGetProgramBuildInfo: _t_clGetProgramBuildInfo;
// Kernel Object APIs
  clCreateKernel: _t_clCreateKernel;
  clCreateKernelsInProgram: _t_clCreateKernelsInProgram;
  clRetainKernel: _t_clRetainKernel;
  clReleaseKernel: _t_clReleaseKernel;
  clSetKernelArg: _t_clSetKernelArg;
  clGetKernelInfo: _t_clGetKernelInfo;
  clGetKernelArgInfo: _t_clGetKernelArgInfo;
  clGetKernelWorkGroupInfo: _t_clGetKernelWorkGroupInfo;
// Event Object APIs
  clWaitForEvents: _t_clWaitForEvents;
  clGetEventInfo: _t_clGetEventInfo;
  clCreateUserEvent: _t_clCreateUserEvent;
  clRetainEvent: _t_clRetainEvent;
  clReleaseEvent: _t_clReleaseEvent;
  clSetUserEventStatus: _t_clSetUserEventStatus;
  clSetEventCallback: _t_clSetEventCallback;
// Profiling APIs
  clGetEventProfilingInfo: _t_clGetEventProfilingInfo;
// Flush and Finish APIs
  clFlush: _t_clFlush;
  clFinish: _t_clFinish;
// Enqueued Commands APIs
  clEnqueueReadBuffer: _t_clEnqueueReadBuffer;
  clEnqueueReadBufferRect: _t_clEnqueueReadBufferRect;
  clEnqueueWriteBuffer: _t_clEnqueueWriteBuffer;
  clEnqueueWriteBufferRect: _t_clEnqueueWriteBufferRect;
  clEnqueueFillBuffer: _t_clEnqueueFillBuffer;
  clEnqueueCopyBuffer: _t_clEnqueueCopyBuffer;
  clEnqueueCopyBufferRect: _t_clEnqueueCopyBufferRect;
  clEnqueueReadImage: _t_clEnqueueReadImage;
  clEnqueueWriteImage: _t_clEnqueueWriteImage;
  clEnqueueFillImage: _t_clEnqueueFillImage;
  clEnqueueCopyImage: _t_clEnqueueCopyImage;
  clEnqueueCopyImageToBuffer: _t_clEnqueueCopyImageToBuffer;
  clEnqueueCopyBufferToImage: _t_clEnqueueCopyBufferToImage;
  clEnqueueMapBuffer: _t_clEnqueueMapBuffer;
  clEnqueueMapImage: _t_clEnqueueMapImage;
  clEnqueueUnmapMemObject: _t_clEnqueueUnmapMemObject;
  clEnqueueMigrateMemObjects: _t_clEnqueueMigrateMemObjects;
  clEnqueueNDRangeKernel: _t_clEnqueueNDRangeKernel;
  clEnqueueTask: _t_clEnqueueTask;
  clEnqueueNativeKernel: _t_clEnqueueNativeKernel;
  clEnqueueMarkerWithWaitList: _t_clEnqueueMarkerWithWaitList;
  clEnqueueBarrierWithWaitList: _t_clEnqueueBarrierWithWaitList;

// Deprecated OpenCL 1.1 APIs
  clCreateImage2D: _t_clCreateImage2D;
  clCreateImage3D: _t_clCreateImage3D;
  clEnqueueMarker: _t_clEnqueueMarker;
  clEnqueueWaitForEvents: _t_clEnqueueWaitForEvents;
  clEnqueueBarrier: _t_clEnqueueBarrier;
  clUnloadCompiler: _t_clUnloadCompiler;

///////////////////////////////////////////////////////////////////////////////////////////////////
{cl_ext.h}
const
// cl_khr_fp64 extension - no extension #define since it has no functions
  CL_DEVICE_DOUBLE_FP_CONFIG_INFO = $1032; // CL_DEVICE_DOUBLE_FP_CONFIG
// cl_khr_fp16 extension - no extension #define since it has no functions
  CL_DEVICE_HALF_FP_CONFIG        = $1033;

(* Memory object destruction
 *
 * Apple extension for use to manage externally allocated buffers used with cl_mem objects with CL_MEM_USE_HOST_PTR
 *
 * Registers a user callback function that will be called when the memory object is deleted and its resources
 * freed. Each call to clSetMemObjectCallbackFn registers the specified user callback function on a callback
 * stack associated with memobj. The registered user callback functions are called in the reverse order in
 * which they were registered. The user callback functions are called and then the memory object is deleted
 * and its resources freed. This provides a mechanism for the application (and libraries) using memobj to be
 * notified when the memory referenced by host_ptr, specified when the memory object is created and used as
 * the storage bits for the memory object, can be reused or freed.
 *
 * The application may not call CL api's with the cl_mem object passed to the pfn_notify.
 *
 * Please check for the "cl_APPLE_SetMemObjectDestructor" extension using clGetDeviceInfo(CL_DEVICE_EXTENSIONS)
 * before using.
 *)
  cl_APPLE_SetMemObjectDestructor = 1;

type
  TDestructorAppleNotify = procedure (memobj: cl_mem; user_data: pointer); // {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clSetMemObjectDestructorAPPLE = function( // CL_EXT_SUFFIX__VERSION_1_0
    memobj     : cl_mem;
    pfn_notify : TDestructorAppleNotify;
    user_data  : pointer
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

(* Context Logging Functions
 *
 * The next three convenience functions are intended to be used as the pfn_notify parameter to clCreateContext().
 * Please check for the "cl_APPLE_ContextLoggingFunctions" extension using clGetDeviceInfo(CL_DEVICE_EXTENSIONS)
 * before using.
 *
 * clLogMessagesToSystemLog fowards on all log messages to the Apple System Logger
 *)
const
  cl_APPLE_ContextLoggingFunctions = 1;
type
  _t_clLogMessagesToSystemLogAPPLE = procedure( // CL_EXT_SUFFIX__VERSION_1_0
    errstr       : PAnsiChar;
    private_info : pointer;
    cb           : size_t;
    user_data    : pointer
  ); {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

// clLogMessagesToStdout sends all log messages to the file descriptor stdout
  _t_clLogMessagesToStdoutAPPLE = procedure( // CL_EXT_SUFFIX__VERSION_1_0
    errstr       : PAnsiChar;
    private_info : pointer;
    cb           : size_t;
    user_data    : pointer
  ); {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

// clLogMessagesToStderr sends all log messages to the file descriptor stderr
  _t_clLogMessagesToStderrAPPLE = procedure( // CL_EXT_SUFFIX__VERSION_1_0
    errstr       : PAnsiChar;
    private_info : pointer;
    cb           : size_t;
    user_data    : pointer
  ); {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

(************************
* cl_khr_icd extension *
************************)
const
  cl_khr_icd = 1;

// cl_platform_info
  CL_PLATFORM_ICD_SUFFIX_KHR = $920;

// Additional Error Codes
  CL_PLATFORM_NOT_FOUND_KHR = -1001;

type
  _t_clIcdGetPlatformIDsKHR = function(
    num_entries   : cl_uint;
    platforms     : p_cl_platform_id;
    num_platforms : p_cl_uint
  ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  clIcdGetPlatformIDsKHR_fn = _t_clIcdGetPlatformIDsKHR;

(* Extension: cl_khr_image2D_buffer
 *
 * This extension allows a 2D image to be created from a cl_mem buffer without a copy.
 * The type associated with a 2D image created from a buffer in an OpenCL program is image2d_t.
 * Both the sampler and sampler-less read_image built-in functions are supported for 2D images
 * and 2D images created from a buffer.  Similarly, the write_image built-ins are also supported
 * for 2D images created from a buffer.
 *
 * When the 2D image from buffer is created, the client must specify the width,
 * height, image format (i.e. channel order and channel data type) and optionally the row pitch
 *
 * The pitch specified must be a multiple of CL_DEVICE_IMAGE_PITCH_ALIGNMENT pixels.
 * The base address of the buffer must be aligned to CL_DEVICE_IMAGE_BASE_ADDRESS_ALIGNMENT pixels.
 *)

(*************************************
 * cl_khr_initalize_memory extension *
 *************************************)
const
  CL_CONTEXT_MEMORY_INITIALIZE_KHR   = $200E;


(**************************************
 * cl_khr_terminate_context extension *
 **************************************)

  CL_DEVICE_TERMINATE_CAPABILITY_KHR = $200F;
  CL_CONTEXT_TERMINATE_KHR           = $2010;

  cl_khr_terminate_context = 1;

type
  _t_clTerminateContextKHR = function( // CL_EXT_SUFFIX__VERSION_1_2
    context : cl_context
  ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  clTerminateContextKHR_fn = _t_clTerminateContextKHR;

(*
 * Extension: cl_khr_spir
 *
 * This extension adds support to create an OpenCL program object from a
 * Standard Portable Intermediate Representation (SPIR) instance
 *)

(******************************************
* cl_nv_device_attribute_query extension *
******************************************)
const
// cl_nv_device_attribute_query extension - no extension #define since it has no functions
  CL_DEVICE_COMPUTE_CAPABILITY_MAJOR_NV = $4000;
  CL_DEVICE_COMPUTE_CAPABILITY_MINOR_NV = $4001;
  CL_DEVICE_REGISTERS_PER_BLOCK_NV      = $4002;
  CL_DEVICE_WARP_SIZE_NV                = $4003;
  CL_DEVICE_GPU_OVERLAP_NV              = $4004;
  CL_DEVICE_KERNEL_EXEC_TIMEOUT_NV      = $4005;
  CL_DEVICE_INTEGRATED_MEMORY_NV        = $4006;


(*********************************
* cl_amd_device_attribute_query *
*********************************)
  CL_DEVICE_PROFILING_TIMER_OFFSET_AMD = $4036;

////////////////
//CL_VERSION_1_1
(***********************************
* cl_ext_device_fission extension *
***********************************)
  cl_ext_device_fission  = 1;

type
  _t_clReleaseDeviceEXT = function( // CL_EXT_SUFFIX__VERSION_1_1
    device   : cl_device_id
  ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  clReleaseDeviceEXT_fn = _t_clReleaseDeviceEXT; // CL_EXT_SUFFIX__VERSION_1_1

  _t_clRetainDeviceEXT = function( // CL_EXT_SUFFIX__VERSION_1_1
    device   : cl_device_id
  ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  clRetainDeviceEXT_fn = _t_clRetainDeviceEXT; // CL_EXT_SUFFIX__VERSION_1_1

  cl_device_partition_property_ext = cl_ulong;
  p_cl_device_partition_property_ext = ^cl_device_partition_property_ext;

  _t_clCreateSubDevicesEXT = function( // CL_EXT_SUFFIX__VERSION_1_1
    in_device   : cl_device_id;
    properties  : p_cl_device_partition_property_ext;
    num_entries : cl_uint;
    out_devices : p_cl_device_id;
    num_devices : p_cl_uint
  ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  clCreateSubDevicesEXT_fn = _t_clCreateSubDevicesEXT; // CL_EXT_SUFFIX__VERSION_1_1

const
// cl_device_partition_property_ext
  CL_DEVICE_PARTITION_EQUALLY_EXT            = $4050;
  CL_DEVICE_PARTITION_BY_COUNTS_EXT          = $4051;
  CL_DEVICE_PARTITION_BY_NAMES_EXT           = $4052;
  CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN_EXT = $4053;

// clDeviceGetInfo selectors
  CL_DEVICE_PARENT_DEVICE_EXT    = $4054;
  CL_DEVICE_PARTITION_TYPES_EXT  = $4055;
  CL_DEVICE_AFFINITY_DOMAINS_EXT = $4056;
  CL_DEVICE_REFERENCE_COUNT_EXT  = $4057;
  CL_DEVICE_PARTITION_STYLE_EXT  = $4058;

// error codes
  CL_DEVICE_PARTITION_FAILED_EXT = -1057;
  CL_INVALID_PARTITION_COUNT_EXT = -1058;
  CL_INVALID_PARTITION_NAME_EXT  = -1059;

// CL_AFFINITY_DOMAINs
  CL_AFFINITY_DOMAIN_L1_CACHE_EXT         = $1;
  CL_AFFINITY_DOMAIN_L2_CACHE_EXT         = $2;
  CL_AFFINITY_DOMAIN_L3_CACHE_EXT         = $3;
  CL_AFFINITY_DOMAIN_L4_CACHE_EXT         = $4;
  CL_AFFINITY_DOMAIN_NUMA_EXT             = $10;
  CL_AFFINITY_DOMAIN_NEXT_FISSIONABLE_EXT = $100;

// cl_device_partition_property_ext list terminators
  CL_PROPERTIES_LIST_END_EXT          : cl_device_partition_property_ext = 0;
  CL_PARTITION_BY_COUNTS_LIST_END_EXT : cl_device_partition_property_ext = 0;
var
  CL_PARTITION_BY_NAMES_LIST_END_EXT  : cl_device_partition_property_ext = 0; // -1 initiated in LoadOpenCL

(*********************************
* cl_qcom_ext_host_ptr extension
*********************************)
const
  CL_MEM_EXT_HOST_PTR_QCOM                  = (1 shl 29);

  CL_DEVICE_EXT_MEM_PADDING_IN_BYTES_QCOM   = $40A0;
  CL_DEVICE_PAGE_SIZE_QCOM                  = $40A1;
  CL_IMAGE_ROW_ALIGNMENT_QCOM               = $40A2;
  CL_IMAGE_SLICE_ALIGNMENT_QCOM             = $40A3;
  CL_MEM_HOST_UNCACHED_QCOM                 = $40A4;
  CL_MEM_HOST_WRITEBACK_QCOM                = $40A5;
  CL_MEM_HOST_WRITETHROUGH_QCOM             = $40A6;
  CL_MEM_HOST_WRITE_COMBINING_QCOM          = $40A7;

type  
  cl_image_pitch_info_qcom = cl_uint;

  _t_clGetDeviceImageInfoQCOM = function( // CL_EXT_SUFFIX__VERSION_1_2
    device: cl_device_id;
    image_width: size_t;
    image_height: size_t;
    image_format: p_cl_image_format;
    param_name: cl_image_pitch_info_qcom;
    param_value_size: size_t;
    param_value: pointer;
    param_value_size_ret: p_size_t
  ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  cl_mem_ext_host_ptr = packed record
    // Type of external memory allocation.
    // Legal values will be defined in layered extensions.
    allocation_type : cl_uint;

    // Host cache policy for this external memory allocation.
    host_cache_policy : cl_uint;
  end;

(*********************************
* cl_qcom_ion_host_ptr extension
*********************************)
const
  CL_MEM_ION_HOST_PTR_QCOM = $40A8;
  
type
  cl_mem_ion_host_ptr = packed record
    // Type of external memory allocation.
    // Must be CL_MEM_ION_HOST_PTR_QCOM for ION allocations.
    ext_host_ptr : cl_mem_ext_host_ptr;

    // ION file descriptor
    ion_filedesc : integer;

    // Host pointer to the ION allocated memory
    ion_hostptr : pointer;
  end;

var
  clSetMemObjectDestructorAPPLE: _t_clSetMemObjectDestructorAPPLE;
  clLogMessagesToSystemLogAPPLE: _t_clLogMessagesToSystemLogAPPLE;
  clLogMessagesToStdoutAPPLE: _t_clLogMessagesToStdoutAPPLE;
  clLogMessagesToStderrAPPLE: _t_clLogMessagesToStderrAPPLE;

  clIcdGetPlatformIDsKHR: _t_clIcdGetPlatformIDsKHR;
  clTerminateContextKHR: _t_clTerminateContextKHR;
  clReleaseDeviceEXT: _t_clReleaseDeviceEXT;
  clRetainDeviceEXT: _t_clRetainDeviceEXT;
  clCreateSubDevicesEXT: _t_clCreateSubDevicesEXT;

  clGetDeviceImageInfoQCOM: _t_clGetDeviceImageInfoQCOM;

///////////////////////////////////////////////////////////////////////////////////////////////////
// Delphi functions

function LoadOpenCL: boolean;
procedure UnloadOpenCL;
function GetOpenCLFuncAddress(lpProcName: LPCSTR): FARPROC;

IMPLEMENTATION

function GetOpenCLFuncAddress(lpProcName: LPCSTR): FARPROC;
begin
  if OpenCL_loaded
  then result:=GetProcAddress(OpenCL_handle, lpProcName)
  else result:=nil;
  if result=nil then OpenCL_func_not_loaded_str:=OpenCL_func_not_loaded_str+AnsiString(lpProcName)+#13#10;
end;

function LoadOpenCL: boolean;
begin
{$IFDEF CPUx86}
  OpenCL_handle:=LoadLibrary('OpenCL.dll');
{$ELSE}
  OpenCL_handle:=LoadLibrary('OpenCL.dll');
//  OpenCL_handle:=LoadLibrary('OpenCL64.dll');
{$ENDIF}
  OpenCL_loaded:=OpenCL_handle<>0;
  result:=OpenCL_loaded;
  OpenCL_func_not_loaded_str:='';

// Load API functions
  @clGetPlatformIDs:=GetOpenCLFuncAddress('clGetPlatformIDs');
  @clGetPlatformInfo:=GetOpenCLFuncAddress('clGetPlatformInfo');
// Device APIs
  @clGetDeviceIDs:=GetOpenCLFuncAddress('clGetDeviceIDs');
  @clGetDeviceInfo:=GetOpenCLFuncAddress('clGetDeviceInfo');
  @clCreateSubDevices:=GetOpenCLFuncAddress('clCreateSubDevices');
  @clRetainDevice:=GetOpenCLFuncAddress('clRetainDevice');
  @clReleaseDevice:=GetOpenCLFuncAddress('clReleaseDevice');
// Context APIs
  @clCreateContext:=GetOpenCLFuncAddress('clCreateContext');
  @clCreateContextFromType:=GetOpenCLFuncAddress('clCreateContextFromType');
  @clRetainContext:=GetOpenCLFuncAddress('clRetainContext');
  @clReleaseContext:=GetOpenCLFuncAddress('clReleaseContext');
  @clGetContextInfo:=GetOpenCLFuncAddress('clGetContextInfo');
// Command Queue APIs
  @clCreateCommandQueue:=GetOpenCLFuncAddress('clCreateCommandQueue');
  @clRetainCommandQueue:=GetOpenCLFuncAddress('clRetainCommandQueue');
  @clReleaseCommandQueue:=GetOpenCLFuncAddress('clReleaseCommandQueue');
  @clGetCommandQueueInfo:=GetOpenCLFuncAddress('clGetCommandQueueInfo');
  (*  WARNING:
   *     This API introduces mutable state into the OpenCL implementation. It has been REMOVED
   *  to better facilitate thread safety.  The 1.0 API is not thread safe. It is not tested by the
   *  OpenCL 1.1 conformance test, and consequently may not work or may not work dependably.
   *  It is likely to be non-performant. Use of this API is not advised. Use at your own risk.
   *
   *  Software developers previously relying on this API are instructed to set the command queue
   *  properties when creating the queue, instead.
  @clSetCommandQueueProperty:=GetOpenCLFuncAddress('clSetCommandQueueProperty');
  *)
// Memory Object APIs
  @clCreateBuffer:=GetOpenCLFuncAddress('clCreateBuffer');
  @clCreateSubBuffer:=GetOpenCLFuncAddress('clCreateSubBuffer');
  @clCreateImage:=GetOpenCLFuncAddress('clCreateImage');
  @clRetainMemObject:=GetOpenCLFuncAddress('clRetainMemObject');
  @clReleaseMemObject:=GetOpenCLFuncAddress('clReleaseMemObject');
  @clGetSupportedImageFormats:=GetOpenCLFuncAddress('clGetSupportedImageFormats');
  @clGetMemObjectInfo:=GetOpenCLFuncAddress('clGetMemObjectInfo');
  @clGetImageInfo:=GetOpenCLFuncAddress('clGetImageInfo');
  @clSetMemObjectDestructorCallback:=GetOpenCLFuncAddress('clSetMemObjectDestructorCallback');
// Sampler APIs
  @clCreateSampler:=GetOpenCLFuncAddress('clCreateSampler');
  @clRetainSampler:=GetOpenCLFuncAddress('clRetainSampler');
  @clReleaseSampler:=GetOpenCLFuncAddress('clReleaseSampler');
  @clGetSamplerInfo:=GetOpenCLFuncAddress('clGetSamplerInfo');
// Program Object APIs
  @clCreateProgramWithSource:=GetOpenCLFuncAddress('clCreateProgramWithSource');
  @clCreateProgramWithBinary:=GetOpenCLFuncAddress('clCreateProgramWithBinary');
  @clCreateProgramWithBuiltInKernels:=GetOpenCLFuncAddress('clCreateProgramWithBuiltInKernels');
  @clRetainProgram:=GetOpenCLFuncAddress('clRetainProgram');
  @clReleaseProgram:=GetOpenCLFuncAddress('clReleaseProgram');
  @clBuildProgram:=GetOpenCLFuncAddress('clBuildProgram');
  @clCompileProgram:=GetOpenCLFuncAddress('clCompileProgram');
  @clLinkProgram:=GetOpenCLFuncAddress('clLinkProgram');
  @clUnloadPlatformCompiler:=GetOpenCLFuncAddress('clUnloadPlatformCompiler');
  @clGetProgramInfo:=GetOpenCLFuncAddress('clGetProgramInfo');
  @clGetProgramBuildInfo:=GetOpenCLFuncAddress('clGetProgramBuildInfo');
// Kernel Object APIs
  @clCreateKernel:=GetOpenCLFuncAddress('clCreateKernel');
  @clCreateKernelsInProgram:=GetOpenCLFuncAddress('clCreateKernelsInProgram');
  @clRetainKernel:=GetOpenCLFuncAddress('clRetainKernel');
  @clReleaseKernel:=GetOpenCLFuncAddress('clReleaseKernel');
  @clSetKernelArg:=GetOpenCLFuncAddress('clSetKernelArg');
  @clGetKernelInfo:=GetOpenCLFuncAddress('clGetKernelInfo');
  @clGetKernelArgInfo:=GetOpenCLFuncAddress('clGetKernelArgInfo');
  @clGetKernelWorkGroupInfo:=GetOpenCLFuncAddress('clGetKernelWorkGroupInfo');
// Event Object APIs
  @clWaitForEvents:=GetOpenCLFuncAddress('clWaitForEvents');
  @clGetEventInfo:=GetOpenCLFuncAddress('clGetEventInfo');
  @clCreateUserEvent:=GetOpenCLFuncAddress('clCreateUserEvent');
  @clRetainEvent:=GetOpenCLFuncAddress('clRetainEvent');
  @clReleaseEvent:=GetOpenCLFuncAddress('clReleaseEvent');
  @clSetUserEventStatus:=GetOpenCLFuncAddress('clSetUserEventStatus');
  @clSetEventCallback:=GetOpenCLFuncAddress('clSetEventCallback');
// Profiling APIs
  @clGetEventProfilingInfo:=GetOpenCLFuncAddress('clGetEventProfilingInfo');
// Flush and Finish APIs
  @clFlush:=GetOpenCLFuncAddress('clFlush');
  @clFinish:=GetOpenCLFuncAddress('clFinish');
// Enqueued Commands APIs
  @clEnqueueReadBuffer:=GetOpenCLFuncAddress('clEnqueueReadBuffer');
  @clEnqueueReadBufferRect:=GetOpenCLFuncAddress('clEnqueueReadBufferRect');
  @clEnqueueWriteBuffer:=GetOpenCLFuncAddress('clEnqueueWriteBuffer');
  @clEnqueueWriteBufferRect:=GetOpenCLFuncAddress('clEnqueueWriteBufferRect');
  @clEnqueueFillBuffer:=GetOpenCLFuncAddress('clEnqueueFillBuffer');
  @clEnqueueCopyBuffer:=GetOpenCLFuncAddress('clEnqueueCopyBuffer');
  @clEnqueueCopyBufferRect:=GetOpenCLFuncAddress('clEnqueueCopyBufferRect');
  @clEnqueueReadImage:=GetOpenCLFuncAddress('clEnqueueReadImage');
  @clEnqueueWriteImage:=GetOpenCLFuncAddress('clEnqueueWriteImage');
  @clEnqueueFillImage:=GetOpenCLFuncAddress('clEnqueueFillImage');
  @clEnqueueCopyImage:=GetOpenCLFuncAddress('clEnqueueCopyImage');
  @clEnqueueCopyImageToBuffer:=GetOpenCLFuncAddress('clEnqueueCopyImageToBuffer');
  @clEnqueueCopyBufferToImage:=GetOpenCLFuncAddress('clEnqueueCopyBufferToImage');
  @clEnqueueMapBuffer:=GetOpenCLFuncAddress('clEnqueueMapBuffer');
  @clEnqueueMapImage:=GetOpenCLFuncAddress('clEnqueueMapImage');
  @clEnqueueUnmapMemObject:=GetOpenCLFuncAddress('clEnqueueUnmapMemObject');
  @clEnqueueMigrateMemObjects:=GetOpenCLFuncAddress('clEnqueueMigrateMemObjects');
  @clEnqueueNDRangeKernel:=GetOpenCLFuncAddress('clEnqueueNDRangeKernel');
  @clEnqueueTask:=GetOpenCLFuncAddress('clEnqueueTask');
  @clEnqueueNativeKernel:=GetOpenCLFuncAddress('clEnqueueNativeKernel');
  @clEnqueueMarkerWithWaitList:=GetOpenCLFuncAddress('clEnqueueMarkerWithWaitList');
  @clEnqueueBarrierWithWaitList:=GetOpenCLFuncAddress('clEnqueueBarrierWithWaitList');
// Deprecated OpenCL 1.1 APIs
  @clCreateImage2D:=GetOpenCLFuncAddress('clCreateImage2D');
  @clCreateImage3D:=GetOpenCLFuncAddress('clCreateImage3D');
  @clEnqueueMarker:=GetOpenCLFuncAddress('clEnqueueMarker');
  @clEnqueueWaitForEvents:=GetOpenCLFuncAddress('clEnqueueWaitForEvents');
  @clEnqueueBarrier:=GetOpenCLFuncAddress('clEnqueueBarrier');
  @clUnloadCompiler:=GetOpenCLFuncAddress('clUnloadCompiler');

//////////
  CL_PARTITION_BY_NAMES_LIST_END_EXT:=integer(CL_PARTITION_BY_NAMES_LIST_END_EXT)-1;
  @clSetMemObjectDestructorAPPLE:=GetOpenCLFuncAddress('clSetMemObjectDestructorAPPLE');
  @clLogMessagesToSystemLogAPPLE:=GetOpenCLFuncAddress('clLogMessagesToSystemLogAPPLE');
  @clLogMessagesToStdoutAPPLE:=GetOpenCLFuncAddress('clLogMessagesToStdoutAPPLE');
  @clLogMessagesToStderrAPPLE:=GetOpenCLFuncAddress('clLogMessagesToStderrAPPLE');

  @clIcdGetPlatformIDsKHR:=GetOpenCLFuncAddress('clIcdGetPlatformIDsKHR');
  @clTerminateContextKHR:=GetOpenCLFuncAddress('clTerminateContextKHR');
  @clReleaseDeviceEXT:=GetOpenCLFuncAddress('clReleaseDeviceEXT');
  @clRetainDeviceEXT:=GetOpenCLFuncAddress('clRetainDeviceEXT');
  @clCreateSubDevicesEXT:=GetOpenCLFuncAddress('clCreateSubDevicesEXT');

  @clGetDeviceImageInfoQCOM:=GetOpenCLFuncAddress('clGetDeviceImageInfoQCOM');
end;

procedure UnloadOpenCL;
begin
// Load API functions
  @clGetPlatformIDs:=nil;
  @clGetPlatformInfo:=nil;
// Device APIs
  @clGetDeviceIDs:=nil;
  @clGetDeviceInfo:=nil;
  @clCreateSubDevices:=nil;
  @clRetainDevice:=nil;
  @clReleaseDevice:=nil;
// Context APIs
  @clCreateContext:=nil;
  @clCreateContextFromType:=nil;
  @clRetainContext:=nil;
  @clReleaseContext:=nil;
  @clGetContextInfo:=nil;
// Command Queue APIs
  @clCreateCommandQueue:=nil;
  @clRetainCommandQueue:=nil;
  @clReleaseCommandQueue:=nil;
  @clGetCommandQueueInfo:=nil;
  (*  WARNING:
   *     This API introduces mutable state into the OpenCL implementation. It has been REMOVED
   *  to better facilitate thread safety.  The 1.0 API is not thread safe. It is not tested by the
   *  OpenCL 1.1 conformance test, and consequently may not work or may not work dependably.
   *  It is likely to be non-performant. Use of this API is not advised. Use at your own risk.
   *
   *  Software developers previously relying on this API are instructed to set the command queue
   *  properties when creating the queue, instead.
  @clSetCommandQueueProperty:=GetOpenCLFuncAddress('clSetCommandQueueProperty');
  *)
// Memory Object APIs
  @clCreateBuffer:=nil;
  @clCreateSubBuffer:=nil;
  @clCreateImage:=nil;
  @clRetainMemObject:=nil;
  @clReleaseMemObject:=nil;
  @clGetSupportedImageFormats:=nil;
  @clGetMemObjectInfo:=nil;
  @clGetImageInfo:=nil;
  @clSetMemObjectDestructorCallback:=nil;
// Sampler APIs
  @clCreateSampler:=nil;
  @clRetainSampler:=nil;
  @clReleaseSampler:=nil;
  @clGetSamplerInfo:=nil;
// Program Object APIs
  @clCreateProgramWithSource:=nil;
  @clCreateProgramWithBinary:=nil;
  @clCreateProgramWithBuiltInKernels:=nil;
  @clRetainProgram:=nil;
  @clReleaseProgram:=nil;
  @clBuildProgram:=nil;
  @clCompileProgram:=nil;
  @clLinkProgram:=nil;
  @clUnloadPlatformCompiler:=nil;
  @clGetProgramInfo:=nil;
  @clGetProgramBuildInfo:=nil;
// Kernel Object APIs
  @clCreateKernel:=nil;
  @clCreateKernelsInProgram:=nil;
  @clRetainKernel:=nil;
  @clReleaseKernel:=nil;
  @clSetKernelArg:=nil;
  @clGetKernelInfo:=nil;
  @clGetKernelArgInfo:=nil;
  @clGetKernelWorkGroupInfo:=nil;
// Event Object APIs
  @clWaitForEvents:=nil;
  @clGetEventInfo:=nil;
  @clCreateUserEvent:=nil;
  @clRetainEvent:=nil;
  @clReleaseEvent:=nil;
  @clSetUserEventStatus:=nil;
  @clSetEventCallback:=nil;
// Profiling APIs
  @clGetEventProfilingInfo:=nil;
// Flush and Finish APIs
  @clFlush:=nil;
  @clFinish:=nil;
// Enqueued Commands APIs
  @clEnqueueReadBuffer:=nil;
  @clEnqueueReadBufferRect:=nil;
  @clEnqueueWriteBuffer:=nil;
  @clEnqueueWriteBufferRect:=nil;
  @clEnqueueFillBuffer:=nil;
  @clEnqueueCopyBuffer:=nil;
  @clEnqueueCopyBufferRect:=nil;
  @clEnqueueReadImage:=nil;
  @clEnqueueWriteImage:=nil;
  @clEnqueueFillImage:=nil;
  @clEnqueueCopyImage:=nil;
  @clEnqueueCopyImageToBuffer:=nil;
  @clEnqueueCopyBufferToImage:=nil;
  @clEnqueueMapBuffer:=nil;
  @clEnqueueMapImage:=nil;
  @clEnqueueUnmapMemObject:=nil;
  @clEnqueueMigrateMemObjects:=nil;
  @clEnqueueNDRangeKernel:=nil;
  @clEnqueueTask:=nil;
  @clEnqueueNativeKernel:=nil;
  @clEnqueueMarkerWithWaitList:=nil;
  @clEnqueueBarrierWithWaitList:=nil;
// Deprecated OpenCL 1.1 APIs
  @clCreateImage2D:=nil;
  @clCreateImage3D:=nil;
  @clEnqueueMarker:=nil;
  @clEnqueueWaitForEvents:=nil;
  @clEnqueueBarrier:=nil;
  @clUnloadCompiler:=nil;

//////////
{cl_ext.h}
  @clSetMemObjectDestructorAPPLE:=nil;
  @clLogMessagesToSystemLogAPPLE:=nil;
  @clLogMessagesToStdoutAPPLE:=nil;
  @clLogMessagesToStderrAPPLE:=nil;

  @clIcdGetPlatformIDsKHR:=nil;
  @clTerminateContextKHR:=nil;
  @clReleaseDeviceEXT:=nil;
  @clRetainDeviceEXT:=nil;
  @clCreateSubDevicesEXT:=nil;

  @clGetDeviceImageInfoQCOM:=nil;

  if OpenCL_loaded then FreeLibrary(OpenCL_handle);
  OpenCL_loaded:=false;
  OpenCL_func_not_loaded_str:='';
end;

INITIALIZATION
  LoadOpenCL;
FINALIZATION
  UnloadOpenCL;
END.
