unit Android.GLES2;

{
  This document is licensed under the SGI Free Software B License Version
  2.0. For details, see http://oss.sgi.com/projects/FreeB/ .

  Android NDK C/C++ files:
    GLES2/gl2platform.h
    GLES2/gl2.h
    GLES2/gl2ext.h

  Original source code was taken from:
    %NDK_DIR%/platforms/android-9/arch-arm/usr/include/

  Pascal translation by Yuriy Kotsarenko, August 2015.
}

interface

{$INCLUDE Android.Config.inc}
{$INCLUDE Android.LibDefs.inc}

{ Data type definitions }
type
  PPGLvoid = ^PGLvoid;
  PGLvoid = Pointer;
//  GLvoid = Pointer;

  PPGLchar = ^PGLchar;
  PGLchar = PAnsiChar;
  GLchar = AnsiChar;

  PGLenum = ^GLenum;
  GLenum = LongWord;

  PGLboolean = ^GLboolean;
  GLboolean = Byte;

  PGLbitfield = ^GLbitfield;
  GLbitfield = LongWord;

  PGLbyte = ^GLbyte;
  GLbyte = ShortInt;

  PGLshort = ^GLshort;
  GLshort = SmallInt;

  PGLint = ^GLint;
  GLint = LongInt;

  PGLsizei = ^GLsizei;
  GLsizei = LongInt;

  PGLubyte = ^GLubyte;
  GLubyte = Byte;

  PGLushort = ^GLushort;
  GLushort = Word;

  PGLuint = ^GLuint;
  GLuint = LongWord;

  PGLfloat = ^GLfloat;
  GLfloat = Single;

  PGLclampf = ^GLclampf;
  GLclampf = Single;

  PGLfixed = ^GLfixed;
  GLfixed = LongInt;

{ GL types for handling large vertex buffer objects }
  GLintptr = PtrInt;
  GLsizeiptr = PtrInt;

{ GL_OES_EGL_image }
  GLeglImageOES = PGLvoid;

const
{ OpenGL ES core versions }
  GL_ES_VERSION_2_0 = 1;

{ ClearBufferMask }
  GL_DEPTH_BUFFER_BIT = $00000100;
  GL_STENCIL_BUFFER_BIT = $00000400;
  GL_COLOR_BUFFER_BIT = $00004000;

{ Boolean }
  GL_FALSE = 0;
  GL_TRUE = 1;

{ BeginMode }
  GL_POINTS = $0000;
  GL_LINES = $0001;
  GL_LINE_LOOP = $0002;
  GL_LINE_STRIP = $0003;
  GL_TRIANGLES = $0004;
  GL_TRIANGLE_STRIP = $0005;
  GL_TRIANGLE_FAN = $0006;

{ AlphaFunction (not supported in ES20) }
//  GL_NEVER
//  GL_LESS
//  GL_EQUAL
//  GL_LEQUAL
//  GL_GREATER
//  GL_NOTEQUAL
//  GL_GEQUAL
//  GL_ALWAYS

{ BlendingFactorDest }
  GL_ZERO = 0;
  GL_ONE = 1;
  GL_SRC_COLOR = $0300;
  GL_ONE_MINUS_SRC_COLOR = $0301;
  GL_SRC_ALPHA = $0302;
  GL_ONE_MINUS_SRC_ALPHA = $0303;
  GL_DST_ALPHA = $0304;
  GL_ONE_MINUS_DST_ALPHA = $0305;

{ BlendingFactorSrc }
//  GL_ZERO
//  GL_ONE
  GL_DST_COLOR = $0306;
  GL_ONE_MINUS_DST_COLOR = $0307;
  GL_SRC_ALPHA_SATURATE = $0308;
//  GL_SRC_ALPHA
//  GL_ONE_MINUS_SRC_ALPHA
//  GL_DST_ALPHA
//  GL_ONE_MINUS_DST_ALPHA

{ BlendEquationSeparate }
  GL_FUNC_ADD = $8006;
  GL_BLEND_EQUATION = $8009;
  GL_BLEND_EQUATION_RGB = $8009; // same as BLEND_EQUATION
  GL_BLEND_EQUATION_ALPHA = $883D;

{ BlendSubtract }
  GL_FUNC_SUBTRACT = $800A;
  GL_FUNC_REVERSE_SUBTRACT = $800B;

{ Separate Blend Functions }
  GL_BLEND_DST_RGB = $80C8;
  GL_BLEND_SRC_RGB = $80C9;
  GL_BLEND_DST_ALPHA = $80CA;
  GL_BLEND_SRC_ALPHA = $80CB;
  GL_CONSTANT_COLOR = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR = $8002;
  GL_CONSTANT_ALPHA = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA = $8004;
  GL_BLEND_COLOR = $8005;

{ Buffer Objects }
  GL_ARRAY_BUFFER = $8892;
  GL_ELEMENT_ARRAY_BUFFER = $8893;
  GL_ARRAY_BUFFER_BINDING = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING = $8895;

  GL_STREAM_DRAW = $88E0;
  GL_STATIC_DRAW = $88E4;
  GL_DYNAMIC_DRAW = $88E8;

  GL_BUFFER_SIZE = $8764;
  GL_BUFFER_USAGE = $8765;

  GL_CURRENT_VERTEX_ATTRIB = $8626;

{ CullFaceMode }
  GL_FRONT = $0404;
  GL_BACK = $0405;
  GL_FRONT_AND_BACK = $0408;

{ DepthFunction }
//  GL_NEVER
//  GL_LESS
//  GL_EQUAL
//  GL_LEQUAL
//  GL_GREATER
//  GL_NOTEQUAL
//  GL_GEQUAL
//  GL_ALWAYS

{ EnableCap }
  GL_TEXTURE_2D = $0DE1;
  GL_CULL_FACE = $0B44;
  GL_BLEND = $0BE2;
  GL_DITHER = $0BD0;
  GL_STENCIL_TEST = $0B90;
  GL_DEPTH_TEST = $0B71;
  GL_SCISSOR_TEST = $0C11;
  GL_POLYGON_OFFSET_FILL = $8037;
  GL_SAMPLE_ALPHA_TO_COVERAGE = $809E;
  GL_SAMPLE_COVERAGE = $80A0;

{ ErrorCode }
  GL_NO_ERROR = 0;
  GL_INVALID_ENUM = $0500;
  GL_INVALID_VALUE = $0501;
  GL_INVALID_OPERATION = $0502;
  GL_OUT_OF_MEMORY = $0505;

{ FrontFaceDirection }
  GL_CW = $0900;
  GL_CCW = $0901;

{ GetPName }
  GL_LINE_WIDTH = $0B21;
  GL_ALIASED_POINT_SIZE_RANGE = $846D;
  GL_ALIASED_LINE_WIDTH_RANGE = $846E;
  GL_CULL_FACE_MODE = $0B45;
  GL_FRONT_FACE = $0B46;
  GL_DEPTH_RANGE = $0B70;
  GL_DEPTH_WRITEMASK = $0B72;
  GL_DEPTH_CLEAR_VALUE = $0B73;
  GL_DEPTH_FUNC = $0B74;
  GL_STENCIL_CLEAR_VALUE = $0B91;
  GL_STENCIL_FUNC = $0B92;
  GL_STENCIL_FAIL = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS = $0B96;
  GL_STENCIL_REF = $0B97;
  GL_STENCIL_VALUE_MASK = $0B93;
  GL_STENCIL_WRITEMASK = $0B98;
  GL_STENCIL_BACK_FUNC = $8800;
  GL_STENCIL_BACK_FAIL = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS = $8803;
  GL_STENCIL_BACK_REF = $8CA3;
  GL_STENCIL_BACK_VALUE_MASK = $8CA4;
  GL_STENCIL_BACK_WRITEMASK = $8CA5;
  GL_VIEWPORT = $0BA2;
  GL_SCISSOR_BOX = $0C10;
//  GL_SCISSOR_TEST
  GL_COLOR_CLEAR_VALUE = $0C22;
  GL_COLOR_WRITEMASK = $0C23;
  GL_UNPACK_ALIGNMENT = $0CF5;
  GL_PACK_ALIGNMENT = $0D05;
  GL_MAX_TEXTURE_SIZE = $0D33;
  GL_MAX_VIEWPORT_DIMS = $0D3A;
  GL_SUBPIXEL_BITS = $0D50;
  GL_RED_BITS = $0D52;
  GL_GREEN_BITS = $0D53;
  GL_BLUE_BITS = $0D54;
  GL_ALPHA_BITS = $0D55;
  GL_DEPTH_BITS = $0D56;
  GL_STENCIL_BITS = $0D57;
  GL_POLYGON_OFFSET_UNITS = $2A00;
//  GL_POLYGON_OFFSET_FILL
  GL_POLYGON_OFFSET_FACTOR = $8038;
  GL_TEXTURE_BINDING_2D = $8069;
  GL_SAMPLE_BUFFERS = $80A8;
  GL_SAMPLES = $80A9;
  GL_SAMPLE_COVERAGE_VALUE = $80AA;
  GL_SAMPLE_COVERAGE_INVERT = $80AB;

{ GetTextureParameter }
//  GL_TEXTURE_MAG_FILTER
//  GL_TEXTURE_MIN_FILTER
//  GL_TEXTURE_WRAP_S
//  GL_TEXTURE_WRAP_T

  GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS = $86A3;

{ HintMode }
  GL_DONT_CARE = $1100;
  GL_FASTEST = $1101;
  GL_NICEST = $1102;

{ HintTarget }
  GL_GENERATE_MIPMAP_HINT = $8192;

{ DataType }
  GL_BYTE = $1400;
  GL_UNSIGNED_BYTE = $1401;
  GL_SHORT = $1402;
  GL_UNSIGNED_SHORT = $1403;
  GL_INT = $1404;
  GL_UNSIGNED_INT = $1405;
  GL_FLOAT = $1406;
  GL_FIXED = $140C;

{ PixelFormat }
  GL_DEPTH_COMPONENT = $1902;
  GL_ALPHA = $1906;
  GL_RGB = $1907;
  GL_RGBA = $1908;
  GL_LUMINANCE = $1909;
  GL_LUMINANCE_ALPHA = $190A;

{ PixelType }
//  GL_UNSIGNED_BYTE
  GL_UNSIGNED_SHORT_4_4_4_4 = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1 = $8034;
  GL_UNSIGNED_SHORT_5_6_5 = $8363;

{ Shaders }
  GL_FRAGMENT_SHADER = $8B30;
  GL_VERTEX_SHADER = $8B31;
  GL_MAX_VERTEX_ATTRIBS = $8869;
  GL_MAX_VERTEX_UNIFORM_VECTORS = $8DFB;
  GL_MAX_VARYING_VECTORS = $8DFC;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS = $8B4C;
  GL_MAX_TEXTURE_IMAGE_UNITS = $8872;
  GL_MAX_FRAGMENT_UNIFORM_VECTORS = $8DFD;
  GL_SHADER_TYPE = $8B4F;
  GL_DELETE_STATUS = $8B80;
  GL_LINK_STATUS = $8B82;
  GL_VALIDATE_STATUS = $8B83;
  GL_ATTACHED_SHADERS = $8B85;
  GL_ACTIVE_UNIFORMS = $8B86;
  GL_ACTIVE_UNIFORM_MAX_LENGTH = $8B87;
  GL_ACTIVE_ATTRIBUTES = $8B89;
  GL_ACTIVE_ATTRIBUTE_MAX_LENGTH = $8B8A;
  GL_SHADING_LANGUAGE_VERSION = $8B8C;
  GL_CURRENT_PROGRAM = $8B8D;

{ StencilFunction }
  GL_NEVER = $0200;
  GL_LESS = $0201;
  GL_EQUAL = $0202;
  GL_LEQUAL = $0203;
  GL_GREATER = $0204;
  GL_NOTEQUAL = $0205;
  GL_GEQUAL = $0206;
  GL_ALWAYS = $0207;

{ StencilOp }
//  GL_ZERO
  GL_KEEP = $1E00;
  GL_REPLACE = $1E01;
  GL_INCR = $1E02;
  GL_DECR = $1E03;
  GL_INVERT = $150A;
  GL_INCR_WRAP = $8507;
  GL_DECR_WRAP = $8508;

{ StringName }
  GL_VENDOR = $1F00;
  GL_RENDERER = $1F01;
  GL_VERSION = $1F02;
  GL_EXTENSIONS = $1F03;

{ TextureMagFilter }
  GL_NEAREST = $2600;
  GL_LINEAR = $2601;

{ TextureMinFilter }
//  GL_NEAREST
//  GL_LINEAR
  GL_NEAREST_MIPMAP_NEAREST = $2700;
  GL_LINEAR_MIPMAP_NEAREST = $2701;
  GL_NEAREST_MIPMAP_LINEAR = $2702;
  GL_LINEAR_MIPMAP_LINEAR = $2703;

{ TextureParameterName }
  GL_TEXTURE_MAG_FILTER = $2800;
  GL_TEXTURE_MIN_FILTER = $2801;
  GL_TEXTURE_WRAP_S = $2802;
  GL_TEXTURE_WRAP_T = $2803;

{ TextureTarget }
//  GL_TEXTURE_2D
  GL_TEXTURE = $1702;

  GL_TEXTURE_CUBE_MAP = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE = $851C;

{ TextureUnit }
  GL_TEXTURE0 = $84C0;
  GL_TEXTURE1 = $84C1;
  GL_TEXTURE2 = $84C2;
  GL_TEXTURE3 = $84C3;
  GL_TEXTURE4 = $84C4;
  GL_TEXTURE5 = $84C5;
  GL_TEXTURE6 = $84C6;
  GL_TEXTURE7 = $84C7;
  GL_TEXTURE8 = $84C8;
  GL_TEXTURE9 = $84C9;
  GL_TEXTURE10 = $84CA;
  GL_TEXTURE11 = $84CB;
  GL_TEXTURE12 = $84CC;
  GL_TEXTURE13 = $84CD;
  GL_TEXTURE14 = $84CE;
  GL_TEXTURE15 = $84CF;
  GL_TEXTURE16 = $84D0;
  GL_TEXTURE17 = $84D1;
  GL_TEXTURE18 = $84D2;
  GL_TEXTURE19 = $84D3;
  GL_TEXTURE20 = $84D4;
  GL_TEXTURE21 = $84D5;
  GL_TEXTURE22 = $84D6;
  GL_TEXTURE23 = $84D7;
  GL_TEXTURE24 = $84D8;
  GL_TEXTURE25 = $84D9;
  GL_TEXTURE26 = $84DA;
  GL_TEXTURE27 = $84DB;
  GL_TEXTURE28 = $84DC;
  GL_TEXTURE29 = $84DD;
  GL_TEXTURE30 = $84DE;
  GL_TEXTURE31 = $84DF;
  GL_ACTIVE_TEXTURE = $84E0;

{ TextureWrapMode }
  GL_REPEAT = $2901;
  GL_CLAMP_TO_EDGE = $812F;
  GL_MIRRORED_REPEAT = $8370;

{ Uniform Types }
  GL_FLOAT_VEC2 = $8B50;
  GL_FLOAT_VEC3 = $8B51;
  GL_FLOAT_VEC4 = $8B52;
  GL_INT_VEC2 = $8B53;
  GL_INT_VEC3 = $8B54;
  GL_INT_VEC4 = $8B55;
  GL_BOOL = $8B56;
  GL_BOOL_VEC2 = $8B57;
  GL_BOOL_VEC3 = $8B58;
  GL_BOOL_VEC4 = $8B59;
  GL_FLOAT_MAT2 = $8B5A;
  GL_FLOAT_MAT3 = $8B5B;
  GL_FLOAT_MAT4 = $8B5C;
  GL_SAMPLER_2D = $8B5E;
  GL_SAMPLER_CUBE = $8B60;

{ Vertex Arrays }
  GL_VERTEX_ATTRIB_ARRAY_ENABLED = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE = $8625;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED = $886A;
  GL_VERTEX_ATTRIB_ARRAY_POINTER = $8645;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;

{ Read Format }
  GL_IMPLEMENTATION_COLOR_READ_TYPE = $8B9A;
  GL_IMPLEMENTATION_COLOR_READ_FORMAT = $8B9B;

{ Shader Source }
  GL_COMPILE_STATUS = $8B81;
  GL_INFO_LOG_LENGTH = $8B84;
  GL_SHADER_SOURCE_LENGTH = $8B88;
  GL_SHADER_COMPILER = $8DFA;

{ Shader Binary }
  GL_SHADER_BINARY_FORMATS = $8DF8;
  GL_NUM_SHADER_BINARY_FORMATS = $8DF9;

{ Shader Precision-Specified Types }
  GL_LOW_FLOAT = $8DF0;
  GL_MEDIUM_FLOAT = $8DF1;
  GL_HIGH_FLOAT = $8DF2;
  GL_LOW_INT = $8DF3;
  GL_MEDIUM_INT = $8DF4;
  GL_HIGH_INT = $8DF5;

{ Framebuffer Object. }
  GL_FRAMEBUFFER = $8D40;
  GL_RENDERBUFFER = $8D41;

  GL_RGBA4 = $8056;
  GL_RGB5_A1 = $8057;
  GL_RGB565 = $8D62;
  GL_DEPTH_COMPONENT16 = $81A5;
  GL_STENCIL_INDEX = $1901;
  GL_STENCIL_INDEX8 = $8D48;

  GL_RENDERBUFFER_WIDTH = $8D42;
  GL_RENDERBUFFER_HEIGHT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT = $8D44;
  GL_RENDERBUFFER_RED_SIZE = $8D50;
  GL_RENDERBUFFER_GREEN_SIZE = $8D51;
  GL_RENDERBUFFER_BLUE_SIZE = $8D52;
  GL_RENDERBUFFER_ALPHA_SIZE = $8D53;
  GL_RENDERBUFFER_DEPTH_SIZE = $8D54;
  GL_RENDERBUFFER_STENCIL_SIZE = $8D55;

  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;

  GL_COLOR_ATTACHMENT0 = $8CE0;
  GL_DEPTH_ATTACHMENT = $8D00;
  GL_STENCIL_ATTACHMENT = $8D20;

  GL_NONE = 0;

  GL_FRAMEBUFFER_COMPLETE = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = $8CD9;
  GL_FRAMEBUFFER_UNSUPPORTED = $8CDD;

  GL_FRAMEBUFFER_BINDING = $8CA6;
  GL_RENDERBUFFER_BINDING = $8CA7;
  GL_MAX_RENDERBUFFER_SIZE = $84E8;

  GL_INVALID_FRAMEBUFFER_OPERATION = $0506;

{
  OES extension tokens
}

{ GL_OES_compressed_ETC1_RGB8_texture }
  GL_ETC1_RGB8_OES = $8D64;

{ GL_OES_compressed_paletted_texture }
  GL_PALETTE4_RGB8_OES = $8B90;
  GL_PALETTE4_RGBA8_OES = $8B91;
  GL_PALETTE4_R5_G6_B5_OES = $8B92;
  GL_PALETTE4_RGBA4_OES = $8B93;
  GL_PALETTE4_RGB5_A1_OES = $8B94;
  GL_PALETTE8_RGB8_OES = $8B95;
  GL_PALETTE8_RGBA8_OES = $8B96;
  GL_PALETTE8_R5_G6_B5_OES = $8B97;
  GL_PALETTE8_RGBA4_OES = $8B98;
  GL_PALETTE8_RGB5_A1_OES = $8B99;

{ GL_OES_depth24 }
  GL_DEPTH_COMPONENT24_OES = $81A6;

{ GL_OES_depth32 }
  GL_DEPTH_COMPONENT32_OES = $81A7;

{ GL_OES_depth_texture }
//  No new tokens introduced by this extension.

{ GL_OES_element_index_uint }
//  GL_UNSIGNED_INT already defined as base data type.

{ GL_OES_get_program_binary }
  GL_PROGRAM_BINARY_LENGTH_OES = $8741;
  GL_NUM_PROGRAM_BINARY_FORMATS_OES = $87FE;
  GL_PROGRAM_BINARY_FORMATS_OES = $87FF;

{ GL_OES_mapbuffer }
  GL_WRITE_ONLY_OES = $88B9;
  GL_BUFFER_ACCESS_OES = $88BB;
  GL_BUFFER_MAPPED_OES = $88BC;
  GL_BUFFER_MAP_POINTER_OES = $88BD;

{ GL_OES_packed_depth_stencil }
  GL_DEPTH_STENCIL_OES = $84F9;
  GL_UNSIGNED_INT_24_8_OES = $84FA;
  GL_DEPTH24_STENCIL8_OES = $88F0;

{ GL_OES_rgb8_rgba8 }
  GL_RGB8_OES = $8051;
  GL_RGBA8_OES = $8058;

{ GL_OES_standard_derivatives }
  GL_FRAGMENT_SHADER_DERIVATIVE_HINT_OES = $8B8B;

{ GL_OES_stencil1 }
  GL_STENCIL_INDEX1_OES = $8D46;

{ GL_OES_stencil4 }
  GL_STENCIL_INDEX4_OES = $8D47;

{ GL_OES_texture_3D }
  GL_TEXTURE_WRAP_R_OES = $8072;
  GL_TEXTURE_3D_OES = $806F;
  GL_TEXTURE_BINDING_3D_OES = $806A;
  GL_MAX_3D_TEXTURE_SIZE_OES = $8073;
  GL_SAMPLER_3D_OES = $8B5F;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_OES = $8CD4;

{ GL_OES_texture_float }
//  No new tokens introduced by this extension.

{ GL_OES_texture_float_linear }
//  No new tokens introduced by this extension.

{ GL_OES_texture_half_float }
  GL_HALF_FLOAT_OES = $8D61;

{ GL_OES_texture_half_float_linear }
//  No new tokens introduced by this extension.

{ GL_OES_texture_npot }
//  No new tokens introduced by this extension.

{ GL_OES_vertex_array_object }
  GL_VERTEX_ARRAY_BINDING_OES = $85B5;

{ GL_OES_vertex_half_float }
//  GL_HALF_FLOAT_OES defined in GL_OES_texture_half_float already.

{ GL_OES_vertex_type_10_10_10_2 }
  GL_UNSIGNED_INT_10_10_10_2_OES = $8DF6;
  GL_INT_10_10_10_2_OES = $8DF7;

{ GL_OES_EGL_image_external }
  GL_TEXTURE_EXTERNAL_OES = $8D65;
  GL_SAMPLER_EXTERNAL_OES = $8D66;
  GL_TEXTURE_BINDING_EXTERNAL_OES = $8D67;
  GL_REQUIRED_TEXTURE_IMAGE_UNITS_OES = $8D68;

{ GL_OES_compressed_ETC1_RGB8_texture }
  GL_OES_compressed_ETC1_RGB8_texture = 1;

{ GL_OES_compressed_paletted_texture }
  GL_OES_compressed_paletted_texture = 1;

{ GL_OES_depth24 }
  GL_OES_depth24 = 1;

{ GL_OES_depth32 }
  GL_OES_depth32 = 1;

{ GL_OES_depth_texture }
  GL_OES_depth_texture = 1;

{ GL_OES_EGL_image }
  GL_OES_EGL_image = 1;

{ GL_OES_element_index_uint }
  GL_OES_element_index_uint = 1;

{ GL_OES_fbo_render_mipmap }
  GL_OES_fbo_render_mipmap = 1;

{ GL_OES_fragment_precision_high }
  GL_OES_fragment_precision_high = 1;

{ GL_OES_get_program_binary }
  GL_OES_get_program_binary = 1;

{ GL_OES_mapbuffer }
  GL_OES_mapbuffer = 1;

{ GL_OES_packed_depth_stencil }
  GL_OES_packed_depth_stencil = 1;

{ GL_OES_rgb8_rgba8 }
  GL_OES_rgb8_rgba8 = 1;

{ GL_OES_standard_derivatives }
  GL_OES_standard_derivatives = 1;

{ GL_OES_stencil1 }
  GL_OES_stencil1 = 1;

{ GL_OES_stencil4 }
  GL_OES_stencil4 = 1;

{ GL_OES_texture_3D }
  GL_OES_texture_3D = 1;

{ GL_OES_texture_float }
  GL_OES_texture_float = 1;

{ GL_OES_texture_float_linear }
  GL_OES_texture_float_linear = 1;

{ GL_OES_texture_half_float }
  GL_OES_texture_half_float = 1;

{ GL_OES_texture_half_float_linear }
  GL_OES_texture_half_float_linear = 1;

{ GL_OES_texture_npot }
  GL_OES_texture_npot = 1;

{ GL_OES_vertex_array_object }
  GL_OES_vertex_array_object = 1;

{ GL_OES_vertex_half_float }
  GL_OES_vertex_half_float = 1;

{ GL_OES_vertex_type_10_10_10_2 }
  GL_OES_vertex_type_10_10_10_2 = 1;

{ GL_OES_EGL_image_external }
  GL_OES_EGL_image_external = 1;

{ AMD extension tokens }

{ GL_AMD_compressed_3DC_texture }
  GL_3DC_X_AMD = $87F9;
  GL_3DC_XY_AMD = $87FA;

{ GL_AMD_compressed_ATC_texture }
  GL_ATC_RGB_AMD = $8C92;
  GL_ATC_RGBA_EXPLICIT_ALPHA_AMD = $8C93;
  GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD = $87EE;

{ GL_AMD_performance_monitor }
  GL_COUNTER_TYPE_AMD = $8BC0;
  GL_COUNTER_RANGE_AMD = $8BC1;
  GL_UNSIGNED_INT64_AMD = $8BC2;
  GL_PERCENTAGE_AMD = $8BC3;
  GL_PERFMON_RESULT_AVAILABLE_AMD = $8BC4;
  GL_PERFMON_RESULT_SIZE_AMD = $8BC5;
  GL_PERFMON_RESULT_AMD = $8BC6;

{ GL_AMD_program_binary_Z400 }
  GL_Z400_BINARY_AMD = $8740;

{ GL_AMD_compressed_3DC_texture }
  GL_AMD_compressed_3DC_texture = 1;

{ GL_AMD_compressed_ATC_texture }
  GL_AMD_compressed_ATC_texture = 1;

{ AMD_performance_monitor }
  GL_AMD_performance_monitor = 1;

{ GL_AMD_program_binary_Z400 }
  GL_AMD_program_binary_Z400 = 1;

{ EXT extension tokens }

{ GL_EXT_blend_minmax }
  GL_MIN_EXT = $8007;
  GL_MAX_EXT = $8008;

{ GL_EXT_discard_framebuffer }
  GL_COLOR_EXT = $1800;
  GL_DEPTH_EXT = $1801;
  GL_STENCIL_EXT = $1802;

{ GL_EXT_multi_draw_arrays }
//  No new tokens introduced by this extension.

{ GL_EXT_read_format_bgra }
  GL_BGRA_EXT = $80E1;
  GL_UNSIGNED_SHORT_4_4_4_4_REV_EXT = $8365;
  GL_UNSIGNED_SHORT_1_5_5_5_REV_EXT = $8366;

{ GL_EXT_texture_filter_anisotropic }
  GL_TEXTURE_MAX_ANISOTROPY_EXT = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = $84FF;

{ GL_EXT_texture_format_BGRA8888 }
//  GL_BGRA_EXT already defined for GL_EXT_read_format_bgra.

{ GL_EXT_texture_type_2_10_10_10_REV }
  GL_UNSIGNED_INT_2_10_10_10_REV_EXT = $8368;

{ GL_EXT_texture_compression_dxt1 }
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT = $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT = $83F1;

{ GL_EXT_blend_minmax }
  GL_EXT_blend_minmax = 1;

{  GL_EXT_discard_framebuffer }
  GL_EXT_discard_framebuffer = 1;

  GL_EXT_multi_draw_arrays = 1;

{  GL_EXT_read_format_bgra }
  GL_EXT_read_format_bgra = 1;

{  GL_EXT_texture_filter_anisotropic }
  GL_EXT_texture_filter_anisotropic = 1;

{  GL_EXT_texture_format_BGRA8888 }
  GL_EXT_texture_format_BGRA8888 = 1;

{  GL_EXT_texture_type_2_10_10_10_REV }
  GL_EXT_texture_type_2_10_10_10_REV = 1;

{  GL_EXT_texture_compression_dxt1 }
  GL_EXT_texture_compression_dxt1 = 1;

{ IMG extension tokens }

{ GL_IMG_program_binary }
  GL_SGX_PROGRAM_BINARY_IMG = $9130;

{ GL_IMG_read_format }
  GL_BGRA_IMG = $80E1;
  GL_UNSIGNED_SHORT_4_4_4_4_REV_IMG = $8365;

{ GL_IMG_shader_binary }
  GL_SGX_BINARY_IMG = $8C0A;

{ GL_IMG_texture_compression_pvrtc }
  GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG = $8C00;
  GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG = $8C01;
  GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG = $8C02;
  GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG = $8C03;

{ GL_IMG_multisampled_render_to_texture }
  GL_RENDERBUFFER_SAMPLES_IMG = $9133;
  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_IMG = $9134;
  GL_MAX_SAMPLES_IMG = $9135;
  GL_TEXTURE_SAMPLES_IMG = $9136;

{ GL_IMG_program_binary }
  GL_IMG_program_binary = 1;

{ GL_IMG_read_format }
  GL_IMG_read_format = 1;

{ GL_IMG_shader_binary }
  GL_IMG_shader_binary = 1;

{ GL_IMG_texture_compression_pvrtc }
  GL_IMG_texture_compression_pvrtc = 1;

{ GL_IMG_multisampled_render_to_texture }
  GL_IMG_multisampled_render_to_texture = 1;

{ NV extension tokens }

{ GL_NV_fence }
  GL_ALL_COMPLETED_NV = $84F2;
  GL_FENCE_STATUS_NV = $84F3;
  GL_FENCE_CONDITION_NV = $84F4;

{ GL_NV_coverage_sample }
  GL_COVERAGE_COMPONENT_NV = $8ED0;
  GL_COVERAGE_COMPONENT4_NV = $8ED1;
  GL_COVERAGE_ATTACHMENT_NV = $8ED2;
  GL_COVERAGE_BUFFERS_NV = $8ED3;
  GL_COVERAGE_SAMPLES_NV = $8ED4;
  GL_COVERAGE_ALL_FRAGMENTS_NV = $8ED5;
  GL_COVERAGE_EDGE_FRAGMENTS_NV = $8ED6;
  GL_COVERAGE_AUTOMATIC_NV = $8ED7;
  GL_COVERAGE_BUFFER_BIT_NV = $8000;

{ GL_NV_depth_nonlinear }
  GL_DEPTH_COMPONENT16_NONLINEAR_NV = $8E2C;

{ GL_NV_fence }
  GL_NV_fence = 1;

{ GL_NV_coverage_sample }
  GL_NV_coverage_sample = 1;

{ GL_NV_depth_nonlinear }
  GL_NV_depth_nonlinear = 1;

{ QCOM extension tokens }

{ GL_QCOM_driver_control }
{ No new tokens introduced by this extension. }

{ GL_QCOM_extended_get }
  GL_TEXTURE_WIDTH_QCOM = $8BD2;
  GL_TEXTURE_HEIGHT_QCOM = $8BD3;
  GL_TEXTURE_DEPTH_QCOM = $8BD4;
  GL_TEXTURE_INTERNAL_FORMAT_QCOM = $8BD5;
  GL_TEXTURE_FORMAT_QCOM = $8BD6;
  GL_TEXTURE_TYPE_QCOM = $8BD7;
  GL_TEXTURE_IMAGE_VALID_QCOM = $8BD8;
  GL_TEXTURE_NUM_LEVELS_QCOM = $8BD9;
  GL_TEXTURE_TARGET_QCOM = $8BDA;
  GL_TEXTURE_OBJECT_VALID_QCOM = $8BDB;
  GL_STATE_RESTORE = $8BDC;

{ GL_QCOM_extended_get2 }
{ No new tokens introduced by this extension. }

{ GL_QCOM_perfmon_global_mode }
  GL_PERFMON_GLOBAL_MODE_QCOM = $8FA0;

{ GL_QCOM_writeonly_rendering }
  GL_WRITEONLY_RENDERING_QCOM = $8823;

{ GL_QCOM_tiled_rendering }
  GL_COLOR_BUFFER_BIT0_QCOM = $00000001;
  GL_COLOR_BUFFER_BIT1_QCOM = $00000002;
  GL_COLOR_BUFFER_BIT2_QCOM = $00000004;
  GL_COLOR_BUFFER_BIT3_QCOM = $00000008;
  GL_COLOR_BUFFER_BIT4_QCOM = $00000010;
  GL_COLOR_BUFFER_BIT5_QCOM = $00000020;
  GL_COLOR_BUFFER_BIT6_QCOM = $00000040;
  GL_COLOR_BUFFER_BIT7_QCOM = $00000080;
  GL_DEPTH_BUFFER_BIT0_QCOM = $00000100;
  GL_DEPTH_BUFFER_BIT1_QCOM = $00000200;
  GL_DEPTH_BUFFER_BIT2_QCOM = $00000400;
  GL_DEPTH_BUFFER_BIT3_QCOM = $00000800;
  GL_DEPTH_BUFFER_BIT4_QCOM = $00001000;
  GL_DEPTH_BUFFER_BIT5_QCOM = $00002000;
  GL_DEPTH_BUFFER_BIT6_QCOM = $00004000;
  GL_DEPTH_BUFFER_BIT7_QCOM = $00008000;
  GL_STENCIL_BUFFER_BIT0_QCOM = $00010000;
  GL_STENCIL_BUFFER_BIT1_QCOM = $00020000;
  GL_STENCIL_BUFFER_BIT2_QCOM = $00040000;
  GL_STENCIL_BUFFER_BIT3_QCOM = $00080000;
  GL_STENCIL_BUFFER_BIT4_QCOM = $00100000;
  GL_STENCIL_BUFFER_BIT5_QCOM = $00200000;
  GL_STENCIL_BUFFER_BIT6_QCOM = $00400000;
  GL_STENCIL_BUFFER_BIT7_QCOM = $00800000;
  GL_MULTISAMPLE_BUFFER_BIT0_QCOM = $01000000;
  GL_MULTISAMPLE_BUFFER_BIT1_QCOM = $02000000;
  GL_MULTISAMPLE_BUFFER_BIT2_QCOM = $04000000;
  GL_MULTISAMPLE_BUFFER_BIT3_QCOM = $08000000;
  GL_MULTISAMPLE_BUFFER_BIT4_QCOM = $10000000;
  GL_MULTISAMPLE_BUFFER_BIT5_QCOM = $20000000;
  GL_MULTISAMPLE_BUFFER_BIT6_QCOM = $40000000;
  GL_MULTISAMPLE_BUFFER_BIT7_QCOM = $80000000;

{ GL_QCOM_driver_control }
  GL_QCOM_driver_control = 1;

{ GL_QCOM_extended_get }
  GL_QCOM_extended_get = 1;

{ GL_QCOM_extended_get2 }
  GL_QCOM_extended_get2 = 1;

{ GL_QCOM_perfmon_global_mode }
  GL_QCOM_perfmon_global_mode = 1;

{ GL_QCOM_writeonly_rendering }
  GL_QCOM_writeonly_rendering = 1;

{ GL_QCOM_tiled_rendering }
  GL_QCOM_tiled_rendering = 1;

{ GL core functions. }
procedure glActiveTexture(texture: GLenum); cdecl;
  external libGLESv2 name 'glActiveTexture';

procedure glAttachShader(_program: GLuint; shader: GLuint); cdecl;
  external libGLESv2 name 'glAttachShader';

procedure glBindAttribLocation(_program: GLuint; index: GLuint; name: PGLchar); cdecl;
  external libGLESv2 name 'glBindAttribLocation';

procedure glBindBuffer(target: GLenum; buffer: GLuint); cdecl;
  external libGLESv2 name 'glBindBuffer';

procedure glBindFramebuffer(target: GLenum; framebuffer: GLuint); cdecl;
  external libGLESv2 name 'glBindFramebuffer';

procedure glBindRenderbuffer(target: GLenum; renderbuffer: GLuint); cdecl;
  external libGLESv2 name 'glBindRenderbuffer';

procedure glBindTexture(target: GLenum; texture: GLuint); cdecl;
  external libGLESv2 name 'glBindTexture';

procedure glBlendColor(red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf); cdecl;
  external libGLESv2 name 'glBlendColor';

procedure glBlendEquation(mode: GLenum); cdecl;
  external libGLESv2 name 'glBlendEquation';

procedure glBlendEquationSeparate(modeRGB: GLenum; modeAlpha: GLenum); cdecl;
  external libGLESv2 name 'glBlendEquationSeparate';

procedure glBlendFunc(sfactor: GLenum; dfactor: GLenum); cdecl;
  external libGLESv2 name 'glBlendFunc';

procedure glBlendFuncSeparate(srcRGB: GLenum; dstRGB: GLenum; srcAlpha: GLenum; dstAlpha: GLenum); cdecl;
  external libGLESv2 name 'glBlendFuncSeparate';

procedure glBufferData(target: GLenum; size: GLsizeiptr; data: PGLvoid; usage: GLenum); cdecl;
  external libGLESv2 name 'glBufferData';

procedure glBufferSubData(target: GLenum; offset: GLintptr; size: GLsizeiptr; data: PGLvoid); cdecl;
  external libGLESv2 name 'glBufferSubData';

function glCheckFramebufferStatus(target: GLenum): GLenum; cdecl;
  external libGLESv2 name 'glCheckFramebufferStatus';

procedure glClear(mask: GLbitfield); cdecl;
  external libGLESv2 name 'glClear';

procedure glClearColor(red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf); cdecl;
  external libGLESv2 name 'glClearColor';

procedure glClearDepthf(depth: GLclampf); cdecl;
  external libGLESv2 name 'glClearDepthf';

procedure glClearStencil(s: GLint); cdecl;
  external libGLESv2 name 'glClearStencil';

procedure glColorMask(red: GLboolean; green: GLboolean; blue: GLboolean; alpha: GLboolean); cdecl;
  external libGLESv2 name 'glColorMask';

procedure glCompileShader(shader: GLuint); cdecl;
  external libGLESv2 name 'glCompileShader';

procedure glCompressedTexImage2D(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei;
  border: GLint; imageSize: GLsizei; data: PGLvoid); cdecl;
  external libGLESv2 name 'glCompressedTexImage2D';

procedure glCompressedTexSubImage2D(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei;
  height: GLsizei; format: GLenum; imageSize: GLsizei; data: PGLvoid); cdecl;
  external libGLESv2 name 'glCompressedTexSubImage2D';

procedure glCopyTexImage2D(target: GLenum; level: GLint; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei;
  height: GLsizei; border: GLint); cdecl;
  external libGLESv2 name 'glCopyTexImage2D';

procedure glCopyTexSubImage2D(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; x: GLint; y: GLint;
  width: GLsizei; height: GLsizei); cdecl;
  external libGLESv2 name 'glCopyTexSubImage2D';

function glCreateProgram: GLuint; cdecl;
  external libGLESv2 name 'glCreateProgram';

function glCreateShader(_type: GLenum): GLuint; cdecl;
  external libGLESv2 name 'glCreateShader';

procedure glCullFace(mode: GLenum); cdecl;
  external libGLESv2 name 'glCullFace';

procedure glDeleteBuffers(n: GLsizei; buffers: PGLuint); cdecl;
  external libGLESv2 name 'glDeleteBuffers';

procedure glDeleteFramebuffers(n: GLsizei; framebuffers: PGLuint); cdecl;
  external libGLESv2 name 'glDeleteFramebuffers';

procedure glDeleteProgram(_program: GLuint); cdecl;
  external libGLESv2 name 'glDeleteProgram';

procedure glDeleteRenderbuffers(n: GLsizei; renderbuffers: PGLuint); cdecl;
  external libGLESv2 name 'glDeleteRenderbuffers';

procedure glDeleteShader(shader: GLuint); cdecl;
  external libGLESv2 name 'glDeleteShader';

procedure glDeleteTextures(n: GLsizei; textures: PGLuint); cdecl;
  external libGLESv2 name 'glDeleteTextures';

procedure glDepthFunc(func: GLenum); cdecl;
  external libGLESv2 name 'glDepthFunc';

procedure glDepthMask(flag: GLboolean); cdecl;
  external libGLESv2 name 'glDepthMask';

procedure glDepthRangef(zNear: GLclampf; zFar: GLclampf); cdecl;
  external libGLESv2 name 'glDepthRangef';

procedure glDetachShader(_program: GLuint; shader: GLuint); cdecl;
  external libGLESv2 name 'glDetachShader';

procedure glDisable(cap: GLenum); cdecl;
  external libGLESv2 name 'glDisable';

procedure glDisableVertexAttribArray(index: GLuint); cdecl;
  external libGLESv2 name 'glDisableVertexAttribArray';

procedure glDrawArrays(mode: GLenum; first: GLint; count: GLsizei); cdecl;
  external libGLESv2 name 'glDrawArrays';

procedure glDrawElements(mode: GLenum; count: GLsizei; _type: GLenum; indices: PGLvoid); cdecl;
  external libGLESv2 name 'glDrawElements';

procedure glEnable(cap: GLenum); cdecl;
  external libGLESv2 name 'glEnable';

procedure glEnableVertexAttribArray(index: GLuint); cdecl;
  external libGLESv2 name 'glEnableVertexAttribArray';

procedure glFinish; cdecl;
  external libGLESv2 name 'glFinish';

procedure glFlush; cdecl;
  external libGLESv2 name 'glFlush';

procedure glFramebufferRenderbuffer(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum;
  renderbuffer: GLuint); cdecl;
  external libGLESv2 name 'glFramebufferRenderbuffer';

procedure glFramebufferTexture2D(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint;
  level: GLint); cdecl;
  external libGLESv2 name 'glFramebufferTexture2D';

procedure glFrontFace(mode: GLenum); cdecl;
  external libGLESv2 name 'glFrontFace';

procedure glGenBuffers(n: GLsizei; buffers: PGLuint); cdecl;
  external libGLESv2 name 'glGenBuffers';

procedure glGenerateMipmap(target: GLenum); cdecl;
  external libGLESv2 name 'glGenerateMipmap';

procedure glGenFramebuffers(n: GLsizei; framebuffers: PGLuint); cdecl;
  external libGLESv2 name 'glGenFramebuffers';

procedure glGenRenderbuffers(n: GLsizei; renderbuffers: PGLuint); cdecl;
  external libGLESv2 name 'glGenRenderbuffers';

procedure glGenTextures(n: GLsizei; textures: PGLuint); cdecl;
  external libGLESv2 name 'glGenTextures';

procedure glGetActiveAttrib(_program: GLuint; index: GLuint; bufsize: GLsizei; length: PGLsizei; size: PGLint;
  _type: PGLenum; name: PGLchar); cdecl;
  external libGLESv2 name 'glGetActiveAttrib';

procedure glGetActiveUniform(_program: GLuint; index: GLuint; bufsize: GLsizei; length: PGLsizei; size: PGLint;
  _type: PGLenum; name: PGLchar); cdecl;
  external libGLESv2 name 'glGetActiveUniform';

procedure glGetAttachedShaders(_program: GLuint; maxcount: GLsizei; count: PGLsizei; shaders: PGLuint); cdecl;
  external libGLESv2 name 'glGetAttachedShaders';

function glGetAttribLocation(_program: GLuint; name: PGLchar): GLint; cdecl;
  external libGLESv2 name 'glGetAttribLocation';

procedure glGetBooleanv(pname: GLenum; params: PGLboolean); cdecl;
  external libGLESv2 name 'glGetBooleanv';

procedure glGetBufferParameteriv(target: GLenum; pname: GLenum; params: PGLint); cdecl;
  external libGLESv2 name 'glGetBufferParameteriv';

function glGetError: GLenum; cdecl;
  external libGLESv2 name 'glGetError';

procedure glGetFloatv(pname: GLenum; params: PGLfloat); cdecl;
  external libGLESv2 name 'glGetFloatv';

procedure glGetFramebufferAttachmentParameteriv(target: GLenum; attachment: GLenum; pname: GLenum;
  params: PGLint); cdecl;
  external libGLESv2 name 'glGetFramebufferAttachmentParameteriv';

procedure glGetIntegerv(pname: GLenum; params: PGLint); cdecl;
  external libGLESv2 name 'glGetIntegerv';

procedure glGetProgramiv(_program: GLuint; pname: GLenum; params: PGLint); cdecl;
  external libGLESv2 name 'glGetProgramiv';

procedure glGetProgramInfoLog(_program: GLuint; bufsize: GLsizei; length: PGLsizei; infolog: PGLchar); cdecl;
  external libGLESv2 name 'glGetProgramInfoLog';

procedure glGetRenderbufferParameteriv(target: GLenum; pname: GLenum; params: PGLint); cdecl;
  external libGLESv2 name 'glGetRenderbufferParameteriv';

procedure glGetShaderiv(shader: GLuint; pname: GLenum; params: PGLint); cdecl;
  external libGLESv2 name 'glGetShaderiv';

procedure glGetShaderInfoLog(shader: GLuint; bufsize: GLsizei; length: PGLsizei; infolog: PGLchar); cdecl;
  external libGLESv2 name 'glGetShaderInfoLog';

procedure glGetShaderPrecisionFormat(shadertype: GLenum; precisiontype: GLenum; range: PGLint;
  precision: PGLint); cdecl;
  external libGLESv2 name 'glGetShaderPrecisionFormat';

procedure glGetShaderSource(shader: GLuint; bufsize: GLsizei; length: PGLsizei; source: PGLchar); cdecl;
  external libGLESv2 name 'glGetShaderSource';

function glGetString(name: GLenum): PGLubyte; cdecl;
  external libGLESv2 name 'glGetString';

procedure glGetTexParameterfv(target: GLenum; pname: GLenum; params: PGLfloat); cdecl;
  external libGLESv2 name 'glGetTexParameterfv';

procedure glGetTexParameteriv(target: GLenum; pname: GLenum; params: PGLint); cdecl;
  external libGLESv2 name 'glGetTexParameteriv';

procedure glGetUniformfv(_program: GLuint; location: GLint; params: PGLfloat); cdecl;
  external libGLESv2 name 'glGetUniformfv';

procedure glGetUniformiv(_program: GLuint; location: GLint; params: PGLint); cdecl;
  external libGLESv2 name 'glGetUniformiv';

function glGetUniformLocation(_program: GLuint; name: PGLchar): GLint; cdecl;
  external libGLESv2 name 'glGetUniformLocation';

procedure glGetVertexAttribfv(index: GLuint; pname: GLenum; params: PGLfloat); cdecl;
  external libGLESv2 name 'glGetVertexAttribfv';

procedure glGetVertexAttribiv(index: GLuint; pname: GLenum; params: PGLint); cdecl;
  external libGLESv2 name 'glGetVertexAttribiv';

procedure glGetVertexAttribPointerv(index: GLuint; pname: GLenum; pointer: PPGLvoid); cdecl;
  external libGLESv2 name 'glGetVertexAttribPointerv';

procedure glHint(target: GLenum; mode: GLenum); cdecl;
  external libGLESv2 name 'glHint';

function glIsBuffer(buffer: GLuint): GLboolean; cdecl;
  external libGLESv2 name 'glIsBuffer';

function glIsEnabled(cap: GLenum): GLboolean; cdecl;
  external libGLESv2 name 'glIsEnabled';

function glIsFramebuffer(framebuffer: GLuint): GLboolean; cdecl;
  external libGLESv2 name 'glIsFramebuffer';

function glIsProgram(_program: GLuint): GLboolean; cdecl;
  external libGLESv2 name 'glIsProgram';

function glIsRenderbuffer(renderbuffer: GLuint): GLboolean; cdecl;
  external libGLESv2 name 'glIsRenderbuffer';

function glIsShader(shader: GLuint): GLboolean; cdecl;
  external libGLESv2 name 'glIsShader';

function glIsTexture(texture: GLuint): GLboolean; cdecl;
  external libGLESv2 name 'glIsTexture';

procedure glLineWidth(width: GLfloat); cdecl;
  external libGLESv2 name 'glLineWidth';

procedure glLinkProgram(_program: GLuint); cdecl;
  external libGLESv2 name 'glLinkProgram';

procedure glPixelStorei(pname: GLenum; param: GLint); cdecl;
  external libGLESv2 name 'glPixelStorei';

procedure glPolygonOffset(factor: GLfloat; units: GLfloat); cdecl;
  external libGLESv2 name 'glPolygonOffset';

procedure glReadPixels(x: GLint; y: GLint; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum;
  pixels: PGLvoid); cdecl;
  external libGLESv2 name 'glReadPixels';

procedure glReleaseShaderCompiler; cdecl;
  external libGLESv2 name 'glReleaseShaderCompiler';

procedure glRenderbufferStorage(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); cdecl;
  external libGLESv2 name 'glRenderbufferStorage';

procedure glSampleCoverage(value: GLclampf; invert: GLboolean); cdecl;
  external libGLESv2 name 'glSampleCoverage';

procedure glScissor(x: GLint; y: GLint; width: GLsizei; height: GLsizei); cdecl;
  external libGLESv2 name 'glScissor';

procedure glShaderBinary(n: GLsizei; shaders: PGLuint; binaryformat: GLenum; binary: PGLvoid; length: GLsizei); cdecl;
  external libGLESv2 name 'glShaderBinary';

procedure glShaderSource(shader: GLuint; count: GLsizei; _string: PPGLchar; length: PGLint); cdecl;
  external libGLESv2 name 'glShaderSource';

procedure glStencilFunc(func: GLenum; ref: GLint; mask: GLuint); cdecl;
  external libGLESv2 name 'glStencilFunc';

procedure glStencilFuncSeparate(face: GLenum; func: GLenum; ref: GLint; mask: GLuint); cdecl;
  external libGLESv2 name 'glStencilFuncSeparate';

procedure glStencilMask(mask: GLuint); cdecl;
  external libGLESv2 name 'glStencilMask';

procedure glStencilMaskSeparate(face: GLenum; mask: GLuint); cdecl;
  external libGLESv2 name 'glStencilMaskSeparate';

procedure glStencilOp(fail: GLenum; zfail: GLenum; zpass: GLenum); cdecl;
  external libGLESv2 name 'glStencilOp';

procedure glStencilOpSeparate(face: GLenum; fail: GLenum; zfail: GLenum; zpass: GLenum); cdecl;
  external libGLESv2 name 'glStencilOpSeparate';

procedure glTexImage2D(target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei;
  border: GLint; format: GLenum; _type: GLenum; pixels: PGLvoid); cdecl;
  external libGLESv2 name 'glTexImage2D';

procedure glTexParameterf(target: GLenum; pname: GLenum; param: GLfloat); cdecl;
  external libGLESv2 name 'glTexParameterf';

procedure glTexParameterfv(target: GLenum; pname: GLenum; params: PGLfloat); cdecl;
  external libGLESv2 name 'glTexParameterfv';

procedure glTexParameteri(target: GLenum; pname: GLenum; param: GLint); cdecl;
  external libGLESv2 name 'glTexParameteri';

procedure glTexParameteriv(target: GLenum; pname: GLenum; params: PGLint); cdecl;
  external libGLESv2 name 'glTexParameteriv';

procedure glTexSubImage2D(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei;
  height: GLsizei; format: GLenum; _type: GLenum; pixels: PGLvoid); cdecl;
  external libGLESv2 name 'glTexSubImage2D';

procedure glUniform1f(location: GLint; x: GLfloat); cdecl;
  external libGLESv2 name 'glUniform1f';

procedure glUniform1fv(location: GLint; count: GLsizei; v: PGLfloat); cdecl;
  external libGLESv2 name 'glUniform1fv';

procedure glUniform1i(location: GLint; x: GLint); cdecl;
  external libGLESv2 name 'glUniform1i';

procedure glUniform1iv(location: GLint; count: GLsizei; v: PGLint); cdecl;
  external libGLESv2 name 'glUniform1iv';

procedure glUniform2f(location: GLint; x: GLfloat; y: GLfloat); cdecl;
  external libGLESv2 name 'glUniform2f';

procedure glUniform2fv(location: GLint; count: GLsizei; v: PGLfloat); cdecl;
  external libGLESv2 name 'glUniform2fv';

procedure glUniform2i(location: GLint; x: GLint; y: GLint); cdecl;
  external libGLESv2 name 'glUniform2i';

procedure glUniform2iv(location: GLint; count: GLsizei; v: PGLint); cdecl;
  external libGLESv2 name 'glUniform2iv';

procedure glUniform3f(location: GLint; x: GLfloat; y: GLfloat; z: GLfloat); cdecl;
  external libGLESv2 name 'glUniform3f';

procedure glUniform3fv(location: GLint; count: GLsizei; v: PGLfloat); cdecl;
  external libGLESv2 name 'glUniform3fv';

procedure glUniform3i(location: GLint; x: GLint; y: GLint; z: GLint); cdecl;
  external libGLESv2 name 'glUniform3i';

procedure glUniform3iv(location: GLint; count: GLsizei; v: PGLint); cdecl;
  external libGLESv2 name 'glUniform3iv';

procedure glUniform4f(location: GLint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); cdecl;
  external libGLESv2 name 'glUniform4f';

procedure glUniform4fv(location: GLint; count: GLsizei; v: PGLfloat); cdecl;
  external libGLESv2 name 'glUniform4fv';

procedure glUniform4i(location: GLint; x: GLint; y: GLint; z: GLint; w: GLint); cdecl;
  external libGLESv2 name 'glUniform4i';

procedure glUniform4iv(location: GLint; count: GLsizei; v: PGLint); cdecl;
  external libGLESv2 name 'glUniform4iv';

procedure glUniformMatrix2fv(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); cdecl;
  external libGLESv2 name 'glUniformMatrix2fv';

procedure glUniformMatrix3fv(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); cdecl;
  external libGLESv2 name 'glUniformMatrix3fv';

procedure glUniformMatrix4fv(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); cdecl;
  external libGLESv2 name 'glUniformMatrix4fv';

procedure glUseProgram(_program: GLuint); cdecl;
  external libGLESv2 name 'glUseProgram';

procedure glValidateProgram(_program: GLuint); cdecl;
  external libGLESv2 name 'glValidateProgram';

procedure glVertexAttrib1f(indx: GLuint; x: GLfloat); cdecl;
  external libGLESv2 name 'glVertexAttrib1f';

procedure glVertexAttrib1fv(indx: GLuint; values: PGLfloat); cdecl;
  external libGLESv2 name 'glVertexAttrib1fv';

procedure glVertexAttrib2f(indx: GLuint; x: GLfloat; y: GLfloat); cdecl;
  external libGLESv2 name 'glVertexAttrib2f';

procedure glVertexAttrib2fv(indx: GLuint; values: PGLfloat); cdecl;
  external libGLESv2 name 'glVertexAttrib2fv';

procedure glVertexAttrib3f(indx: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); cdecl;
  external libGLESv2 name 'glVertexAttrib3f';

procedure glVertexAttrib3fv(indx: GLuint; values: PGLfloat); cdecl;
  external libGLESv2 name 'glVertexAttrib3fv';

procedure glVertexAttrib4f(indx: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); cdecl;
  external libGLESv2 name 'glVertexAttrib4f';

procedure glVertexAttrib4fv(indx: GLuint; values: PGLfloat); cdecl;
  external libGLESv2 name 'glVertexAttrib4fv';

procedure glVertexAttribPointer(indx: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei;
  ptr: PGLvoid); cdecl;
  external libGLESv2 name 'glVertexAttribPointer';

procedure glViewport(x: GLint; y: GLint; width: GLsizei; height: GLsizei); cdecl;
  external libGLESv2 name 'glViewport';

var
{ GL_OES_EGL_image }
  glEGLImageTargetTexture2DOES: procedure(target: GLenum; image: GLeglImageOES); cdecl;
  glEGLImageTargetRenderbufferStorageOES: procedure(target: GLenum; image: GLeglImageOES); cdecl;

{ GL_OES_get_program_binary }
  glGetProgramBinaryOES: procedure(_program: GLuint; bufSize: GLsizei; length: PGLsizei; binaryFormat: PGLenum;
    binary: PGLvoid); cdecl;
  glProgramBinaryOES: procedure(_program: GLuint; binaryFormat: GLenum; const binary: PGLvoid; length: GLint); cdecl;

{ GL_OES_mapbuffer }
  glMapBufferOES: function(target: GLenum; access: GLenum): Pointer; cdecl;
  glUnmapBufferOES: function(target: GLenum): GLboolean; cdecl;
  glGetBufferPointervOES: procedure(target: GLenum; pname: GLenum; params: PPGLvoid); cdecl;

{ GL_OES_texture_3D }
  glTexImage3DOES: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei;
    depth: GLsizei; border: GLint; format: GLenum; _type: GLenum; pixels: PGLvoid); cdecl;
  glTexSubImage3DOES: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint;
    width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; _type: GLenum; pixels: PGLvoid); cdecl;
  glCopyTexSubImage3DOES: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint;
    x: GLint; y: GLint; width: GLsizei; height: GLsizei); cdecl;
  glCompressedTexImage3DOES: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei;
    height: GLsizei; depth: GLsizei; border: GLint; imageSize: GLsizei; data: PGLvoid); cdecl;
  glCompressedTexSubImage3DOES: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint;
    width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; imageSize: GLsizei; data: PGLvoid); cdecl;
  glFramebufferTexture3DOES: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint;
    level: GLint; zoffset: GLint); cdecl;

{ GL_OES_vertex_array_object }
  glBindVertexArrayOES: procedure(_array: GLuint); cdecl;
  glDeleteVertexArraysOES: procedure(n: GLsizei; const arrays: PGLuint); cdecl;
  glGenVertexArraysOES: procedure(n: GLsizei; arrays: PGLuint); cdecl;
  glIsVertexArrayOES: function(_array: GLuint): GLboolean; cdecl;

{ AMD_performance_monitor }
  glGetPerfMonitorGroupsAMD: procedure(numGroups: PGLint; groupsSize: GLsizei; groups: PGLuint); cdecl;
  glGetPerfMonitorCountersAMD: procedure(group: GLuint; numCounters: PGLint; maxActiveCounters: PGLint;
    counterSize: GLsizei; counters: PGLuint); cdecl;
  glGetPerfMonitorGroupStringAMD: procedure(group: GLuint; bufSize: GLsizei; length: PGLsizei;
    groupString: PGLchar); cdecl;
  glGetPerfMonitorCounterStringAMD: procedure(group: GLuint; counter: GLuint; bufSize: GLsizei; length: PGLsizei;
    counterString: PGLchar); cdecl;
  glGetPerfMonitorCounterInfoAMD: procedure(group: GLuint; counter: GLuint; pname: GLenum; data: PGLvoid); cdecl;
  glGenPerfMonitorsAMD: procedure(n: GLsizei; monitors: PGLuint); cdecl;
  glDeletePerfMonitorsAMD: procedure(n: GLsizei; monitors: PGLuint); cdecl;
  glSelectPerfMonitorCountersAMD: procedure(monitor: GLuint; enable: GLboolean; group: GLuint; numCounters: GLint;
    countersList: PGLuint); cdecl;
  glBeginPerfMonitorAMD: procedure(monitor: GLuint); cdecl;
  glEndPerfMonitorAMD: procedure(monitor: GLuint); cdecl;
  glGetPerfMonitorCounterDataAMD: procedure(monitor: GLuint; pname: GLenum; dataSize: GLsizei; data: PGLuint;
    bytesWritten: PGLint); cdecl;

{ GL_EXT_discard_framebuffer }
  glDiscardFramebufferEXT: procedure(target: GLenum; numAttachments: GLsizei; attachments: PGLenum); cdecl;

  glMultiDrawArraysEXT: procedure(mode: GLenum; first: PGLint; count: PGLsizei; primcount: GLsizei); cdecl;
  glMultiDrawElementsEXT: procedure(mode: GLenum; const count: PGLsizei; _type: GLenum; indices: PPGLVoid;
    primcount: GLsizei); cdecl;

{ GL_IMG_multisampled_render_to_texture }
  glRenderbufferStorageMultisampleIMG: procedure(target: GLenum; samples: GLsizei; internalformat: GLenum;
    width: GLsizei; height: GLsizei); cdecl;
  glFramebufferTexture2DMultisampleIMG: procedure(target: GLenum; attachment: GLenum; textarget: GLenum;
    texture: GLuint; level: GLint; samples: GLsizei); cdecl;

{ GL_NV_fence }
  glDeleteFencesNV: procedure(n: GLsizei; const fences: PGLuint); cdecl;
  glGenFencesNV: procedure(n: GLsizei; fences: PGLuint); cdecl;
  glIsFenceNV: function(fence: GLuint): GLboolean; cdecl;
  glTestFenceNV: function(fence: GLuint): GLboolean; cdecl;
  glGetFenceivNV: procedure(fence: GLuint; pname: GLenum; params: PGLint); cdecl;
  glFinishFenceNV: procedure(fence: GLuint); cdecl;
  glSetFenceNV: procedure(fence: GLuint; condition: GLenum); cdecl;

{ GL_NV_coverage_sample }
  glCoverageMaskNV: procedure(mask: GLboolean); cdecl;
  glCoverageOperationNV: procedure(operation: GLenum); cdecl;

{ GL_QCOM_driver_control }
  glGetDriverControlsQCOM: procedure(num: PGLint; size: GLsizei; driverControls: PGLuint); cdecl;
  glGetDriverControlStringQCOM: procedure(driverControl: GLuint; bufSize: GLsizei; length: PGLsizei;
    driverControlString: PGLchar); cdecl;
  glEnableDriverControlQCOM: procedure(driverControl: GLuint); cdecl;
  glDisableDriverControlQCOM: procedure(driverControl: GLuint); cdecl;

{ GL_QCOM_extended_get }
  glExtGetTexturesQCOM: procedure(textures: PGLuint; maxTextures: GLint; numTextures: PGLint); cdecl;
  glExtGetBuffersQCOM: procedure(buffers: PGLuint; maxBuffers: GLint; numBuffers: PGLint); cdecl;
  glExtGetRenderbuffersQCOM: procedure(renderbuffers: PGLuint; maxRenderbuffers: GLint;
    numRenderbuffers: PGLint); cdecl;
  glExtGetFramebuffersQCOM: procedure(framebuffers: PGLuint; maxFramebuffers: GLint; numFramebuffers: PGLint); cdecl;
  glExtGetTexLevelParameterivQCOM: procedure(texture: GLuint; face: GLenum; level: GLint; pname: GLenum;
    params: PGLint); cdecl;
  glExtTexObjectStateOverrideiQCOM: procedure(target: GLenum; pname: GLenum; param: GLint); cdecl;
  glExtGetTexSubImageQCOM: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint;
    width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; _type: GLenum; texels: PGLvoid); cdecl;
  glExtGetBufferPointervQCOM: procedure(target: GLenum; params: PPGLvoid); cdecl;

{ GL_QCOM_extended_get2 }
  glExtGetShadersQCOM: procedure(shaders: PGLuint; maxShaders: GLint; numShaders: PGLint); cdecl;
  glExtGetProgramsQCOM: procedure(programs: PGLuint; maxPrograms: GLint; numPrograms: PGLint); cdecl;
  glExtIsProgramBinaryQCOM: function(_program: GLuint): GLboolean; cdecl;
  glExtGetProgramBinarySourceQCOM: procedure(_program: GLuint; shadertype: GLenum; source: PGLchar;
    length: PGLint); cdecl;

{ GL_QCOM_tiled_rendering }
  glStartTilingQCOM: procedure(x: GLuint; y: GLuint; width: GLuint; height: GLuint; preserveMask: GLbitfield); cdecl;
  glEndTilingQCOM: procedure(preserveMask: GLbitfield); cdecl;

function LoadGLES2Extensions: Boolean;
procedure UnloadGLES2Extensions;

implementation

uses
  Android.DlFcn, Android.EGL;

var
  libGLESv2Handle: Pointer = nil;

procedure ResetExtensions;
begin
  glEGLImageTargetTexture2DOES := nil;
  glEGLImageTargetRenderbufferStorageOES := nil;
  glGetProgramBinaryOES := nil;
  glProgramBinaryOES := nil;
  glMapBufferOES := nil;
  glUnmapBufferOES := nil;
  glGetBufferPointervOES := nil;
  glTexImage3DOES := nil;
  glTexSubImage3DOES := nil;
  glCopyTexSubImage3DOES := nil;
  glCompressedTexImage3DOES := nil;
  glCompressedTexSubImage3DOES := nil;
  glFramebufferTexture3DOES := nil;
  glBindVertexArrayOES := nil;
  glDeleteVertexArraysOES := nil;
  glGenVertexArraysOES := nil;
  glIsVertexArrayOES := nil;
  glGetPerfMonitorGroupsAMD := nil;
  glGetPerfMonitorCountersAMD := nil;
  glGetPerfMonitorGroupStringAMD := nil;
  glGetPerfMonitorCounterStringAMD := nil;
  glGetPerfMonitorCounterInfoAMD := nil;
  glGenPerfMonitorsAMD := nil;
  glDeletePerfMonitorsAMD := nil;
  glSelectPerfMonitorCountersAMD := nil;
  glBeginPerfMonitorAMD := nil;
  glEndPerfMonitorAMD := nil;
  glGetPerfMonitorCounterDataAMD := nil;
  glDiscardFramebufferEXT := nil;
  glMultiDrawArraysEXT := nil;
  glMultiDrawElementsEXT := nil;
  glRenderbufferStorageMultisampleIMG := nil;
  glFramebufferTexture2DMultisampleIMG := nil;
  glDeleteFencesNV := nil;
  glGenFencesNV := nil;
  glIsFenceNV := nil;
  glTestFenceNV := nil;
  glGetFenceivNV := nil;
  glFinishFenceNV := nil;
  glSetFenceNV := nil;
  glCoverageMaskNV := nil;
  glCoverageOperationNV := nil;
  glGetDriverControlsQCOM := nil;
  glGetDriverControlStringQCOM := nil;
  glEnableDriverControlQCOM := nil;
  glDisableDriverControlQCOM := nil;
  glExtGetTexturesQCOM := nil;
  glExtGetBuffersQCOM := nil;
  glExtGetRenderbuffersQCOM := nil;
  glExtGetFramebuffersQCOM := nil;
  glExtGetTexLevelParameterivQCOM := nil;
  glExtTexObjectStateOverrideiQCOM := nil;
  glExtGetTexSubImageQCOM := nil;
  glExtGetBufferPointervQCOM := nil;
  glExtGetShadersQCOM := nil;
  glExtGetProgramsQCOM := nil;
  glExtIsProgramBinaryQCOM := nil;
  glExtGetProgramBinarySourceQCOM := nil;
  glStartTilingQCOM := nil;
  glEndTilingQCOM := nil;
end;

function LoadGLES2Extensions: Boolean;
begin
  if libGLESv2Handle <> nil then
    Exit(True);

  libGLESv2Handle := dlopen(libGLESv2, RTLD_LAZY);
  if libGLESv2Handle = nil then
    Exit(False);

  glEGLImageTargetTexture2DOES := glGetProcAddress(libGLESv2Handle, 'glEGLImageTargetTexture2DOES');
  glEGLImageTargetRenderbufferStorageOES := glGetProcAddress(libGLESv2Handle, 'glEGLImageTargetRenderbufferStorageOES');
  glGetProgramBinaryOES := glGetProcAddress(libGLESv2Handle, 'glGetProgramBinaryOES');
  glProgramBinaryOES := glGetProcAddress(libGLESv2Handle, 'glProgramBinaryOES');
  glMapBufferOES := glGetProcAddress(libGLESv2Handle, 'glMapBufferOES');
  glUnmapBufferOES := glGetProcAddress(libGLESv2Handle, 'glUnmapBufferOES');
  glGetBufferPointervOES := glGetProcAddress(libGLESv2Handle, 'glGetBufferPointervOES');
  glTexImage3DOES := glGetProcAddress(libGLESv2Handle, 'glTexImage3DOES');
  glTexSubImage3DOES := glGetProcAddress(libGLESv2Handle, 'glTexSubImage3DOES');
  glCopyTexSubImage3DOES := glGetProcAddress(libGLESv2Handle, 'glCopyTexSubImage3DOES');
  glCompressedTexImage3DOES := glGetProcAddress(libGLESv2Handle, 'glCompressedTexImage3DOES');
  glCompressedTexSubImage3DOES := glGetProcAddress(libGLESv2Handle, 'glCompressedTexSubImage3DOES');
  glFramebufferTexture3DOES := glGetProcAddress(libGLESv2Handle, 'glFramebufferTexture3DOES');
  glBindVertexArrayOES := glGetProcAddress(libGLESv2Handle, 'glBindVertexArrayOES');
  glDeleteVertexArraysOES := glGetProcAddress(libGLESv2Handle, 'glDeleteVertexArraysOES');
  glGenVertexArraysOES := glGetProcAddress(libGLESv2Handle, 'glGenVertexArraysOES');
  glIsVertexArrayOES := glGetProcAddress(libGLESv2Handle, 'glIsVertexArrayOES');
  glGetPerfMonitorGroupsAMD := glGetProcAddress(libGLESv2Handle, 'glGetPerfMonitorGroupsAMD');
  glGetPerfMonitorCountersAMD := glGetProcAddress(libGLESv2Handle, 'glGetPerfMonitorCountersAMD');
  glGetPerfMonitorGroupStringAMD := glGetProcAddress(libGLESv2Handle, 'glGetPerfMonitorGroupStringAMD');
  glGetPerfMonitorCounterStringAMD := glGetProcAddress(libGLESv2Handle, 'glGetPerfMonitorCounterStringAMD');
  glGetPerfMonitorCounterInfoAMD := glGetProcAddress(libGLESv2Handle, 'glGetPerfMonitorCounterInfoAMD');
  glGenPerfMonitorsAMD := glGetProcAddress(libGLESv2Handle, 'glGenPerfMonitorsAMD');
  glDeletePerfMonitorsAMD := glGetProcAddress(libGLESv2Handle, 'glDeletePerfMonitorsAMD');
  glSelectPerfMonitorCountersAMD := glGetProcAddress(libGLESv2Handle, 'glSelectPerfMonitorCountersAMD');
  glBeginPerfMonitorAMD := glGetProcAddress(libGLESv2Handle, 'glBeginPerfMonitorAMD');
  glEndPerfMonitorAMD := glGetProcAddress(libGLESv2Handle, 'glEndPerfMonitorAMD');
  glGetPerfMonitorCounterDataAMD := glGetProcAddress(libGLESv2Handle, 'glGetPerfMonitorCounterDataAMD');
  glDiscardFramebufferEXT := glGetProcAddress(libGLESv2Handle, 'glDiscardFramebufferEXT');
  glMultiDrawArraysEXT := glGetProcAddress(libGLESv2Handle, 'glMultiDrawArraysEXT');
  glMultiDrawElementsEXT := glGetProcAddress(libGLESv2Handle, 'glMultiDrawElementsEXT');
  glRenderbufferStorageMultisampleIMG := glGetProcAddress(libGLESv2Handle, 'glRenderbufferStorageMultisampleIMG');
  glFramebufferTexture2DMultisampleIMG := glGetProcAddress(libGLESv2Handle, 'glFramebufferTexture2DMultisampleIMG');
  glDeleteFencesNV := glGetProcAddress(libGLESv2Handle, 'glDeleteFencesNV');
  glGenFencesNV := glGetProcAddress(libGLESv2Handle, 'glGenFencesNV');
  glIsFenceNV := glGetProcAddress(libGLESv2Handle, 'glIsFenceNV');
  glTestFenceNV := glGetProcAddress(libGLESv2Handle, 'glTestFenceNV');
  glGetFenceivNV := glGetProcAddress(libGLESv2Handle, 'glGetFenceivNV');
  glFinishFenceNV := glGetProcAddress(libGLESv2Handle, 'glFinishFenceNV');
  glSetFenceNV := glGetProcAddress(libGLESv2Handle, 'glSetFenceNV');
  glCoverageMaskNV := glGetProcAddress(libGLESv2Handle, 'glCoverageMaskNV');
  glCoverageOperationNV := glGetProcAddress(libGLESv2Handle, 'glCoverageOperationNV');
  glGetDriverControlsQCOM := glGetProcAddress(libGLESv2Handle, 'glGetDriverControlsQCOM');
  glGetDriverControlStringQCOM := glGetProcAddress(libGLESv2Handle, 'glGetDriverControlStringQCOM');
  glEnableDriverControlQCOM := glGetProcAddress(libGLESv2Handle, 'glEnableDriverControlQCOM');
  glDisableDriverControlQCOM := glGetProcAddress(libGLESv2Handle, 'glDisableDriverControlQCOM');
  glExtGetTexturesQCOM := glGetProcAddress(libGLESv2Handle, 'glExtGetTexturesQCOM');
  glExtGetBuffersQCOM := glGetProcAddress(libGLESv2Handle, 'glExtGetBuffersQCOM');
  glExtGetRenderbuffersQCOM := glGetProcAddress(libGLESv2Handle, 'glExtGetRenderbuffersQCOM');
  glExtGetFramebuffersQCOM := glGetProcAddress(libGLESv2Handle, 'glExtGetFramebuffersQCOM');
  glExtGetTexLevelParameterivQCOM := glGetProcAddress(libGLESv2Handle, 'glExtGetTexLevelParameterivQCOM');
  glExtTexObjectStateOverrideiQCOM := glGetProcAddress(libGLESv2Handle, 'glExtTexObjectStateOverrideiQCOM');
  glExtGetTexSubImageQCOM := glGetProcAddress(libGLESv2Handle, 'glExtGetTexSubImageQCOM');
  glExtGetBufferPointervQCOM := glGetProcAddress(libGLESv2Handle, 'glExtGetBufferPointervQCOM');
  glExtGetShadersQCOM := glGetProcAddress(libGLESv2Handle, 'glExtGetShadersQCOM');
  glExtGetProgramsQCOM := glGetProcAddress(libGLESv2Handle, 'glExtGetProgramsQCOM');
  glExtIsProgramBinaryQCOM := glGetProcAddress(libGLESv2Handle, 'glExtIsProgramBinaryQCOM');
  glExtGetProgramBinarySourceQCOM := glGetProcAddress(libGLESv2Handle, 'glExtGetProgramBinarySourceQCOM');
  glStartTilingQCOM := glGetProcAddress(libGLESv2Handle, 'glStartTilingQCOM');
  glEndTilingQCOM := glGetProcAddress(libGLESv2Handle, 'glEndTilingQCOM');
  
  Result := True;
end;

procedure UnloadGLES2Extensions;
begin
  if libGLESv2Handle <> nil then
  begin
    dlclose(libGLESv2Handle);
    libGLESv2Handle := nil;

    ResetExtensions;
  end;
end;

initialization
  ResetExtensions;

end.

