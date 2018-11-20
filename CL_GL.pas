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
 **********************************************************************************

 * $Revision: 11708 $ on $Date: 2010-06-13 23:36:24 -0700 (Sun, 13 Jun 2010) $

 *
 * cl_gl.h contains Khronos-approved (KHR) OpenCL extensions which have
 * OpenGL dependencies. The application is responsible for #including
 * OpenGL or OpenGL ES headers before #including cl_gl.h.
 *)
// use dglOpenGL unit for OpenGL from http://www.delphigl.com instead of obsolete Borland OpenGL unit

// OpenCL 1.2 for Delphi 7 by Michal Pohanka: 2013-07-17.
// http://www.volny.cz/profipohanka
// OpenCL library is loaded manualy and the program will not crash
// when OpenCL is not installed.
// Check OpenCL_loaded variable to see if OpenCL is available.
// Avoid usage of Halt procedure to make sure the FINALIZATION section is executed

// some parts were inspired by porting of OpenCL 1.0 to FPC
// by Dmitry 'skalogryz' Boyarintsev: 28th apr 2009

unit CL_GL;

interface

{$INCLUDE 'CL.inc'}

uses
  CL; // dglOpenGL is the latest header translation for OpenGL I found on http://www.delphigl.com

///////////////////////////////////////////////////////////////////////////////////////////////////
{cl_gl.h}
type
  cl_gl_object_type   = cl_uint;
  cl_gl_texture_info  = cl_uint;
  cl_gl_platform_info = cl_uint;
  __GLsync            = _cl_emptyrecord;
  cl_GLsync           = ^__GLsync;


const
// cl_gl_object_type = 0x2000 - 0x200F enum values are currently taken
  CL_GL_OBJECT_BUFFER          = $2000;
  CL_GL_OBJECT_TEXTURE2D       = $2001;
  CL_GL_OBJECT_TEXTURE3D       = $2002;
  CL_GL_OBJECT_RENDERBUFFER    = $2003;
  CL_GL_OBJECT_TEXTURE2D_ARRAY = $200E;
  CL_GL_OBJECT_TEXTURE1D       = $200F;
  CL_GL_OBJECT_TEXTURE1D_ARRAY = $2010;
  CL_GL_OBJECT_TEXTURE_BUFFER  = $2011;

// cl_gl_texture_info
  CL_GL_TEXTURE_TARGET = $2004;
  CL_GL_MIPMAP_LEVEL   = $2005;
  CL_GL_NUM_SAMPLES    = $2012;

type
  _t_clCreateFromGLBuffer = function ( // CL_API_SUFFIX__VERSION_1_0
    context     : cl_context;
    falgs       : cl_mem_flags;
    bufobj      : cl_GLuint;
    errcode_ret : p_cl_int
    ): cl_mem; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clCreateFromGLTexture = function ( // CL_API_SUFFIX__VERSION_1_2
    context: cl_context;
    flags: cl_mem_flags;
    target: cl_GLenum;
    miplevel: cl_GLint;
    texture: cl_GLuint;
    errcode_ret: p_cl_int
    ): cl_mem; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clCreateFromGLRenderbuffer = function ( // CL_API_SUFFIX__VERSION_1_0
    context      : cl_context;
    flags        : cl_mem_flags;
    renderbuffer : cl_GLuint;
    errcode_ret  : p_cl_int
    ): cl_mem; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clGetGLObjectInfo = function ( // CL_API_SUFFIX__VERSION_1_0
    memobj         : cl_mem;
    gl_object_type : cl_gl_object_type;
    gl_object_name : p_cl_GLuint
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clGetGLTextureInfo = function ( // CL_API_SUFFIX__VERSION_1_0
    memobj               : cl_mem;
    param_name           : cl_gl_texture_info;
    param_value_size     : size_t;
    param_value          : Pointer;
    param_value_size_ret : p_size_t
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clEnqueueAcquireGLObjects = function ( // CL_API_SUFFIX__VERSION_1_0
    command_queue           : cl_command_queue;
    num_objects             : cl_uint;
    mem_objects             : p_cl_mem;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clEnqueueReleaseGLObjects = function ( // CL_API_SUFFIX__VERSION_1_0
    command_queue           : cl_command_queue;
    num_objects             : cl_uint;
    mem_objects             : p_cl_mem;
    num_events_in_wait_list : cl_uint;
    event_wait_list         : p_cl_event;
    event                   : p_cl_event
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

// Deprecated OpenCL 1.1 APIs
  _t_clCreateFromGLTexture2D = function ( // CL_API_SUFFIX__VERSION_1_0
    context     : cl_context;
    flags       : cl_mem_flags;
    target      : cl_GLenum;
    miplevel    : cl_GLint;
    texture     : cl_GLuint;
    errcode_ret : p_cl_int
    ): cl_mem; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  _t_clCreateFromGLTexture3D = function ( // CL_API_SUFFIX__VERSION_1_0
    context     : cl_context;
    flags       : cl_mem_flags;
    target      : cl_GLenum;
    miplevel    : cl_GLint;
    texture     : cl_GLuint;
    errcode_ret : p_cl_int
    ): cl_mem; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

// cl_khr_gl_sharing extension
const 
  cl_khr_gl_sharing = 1;

type
  cl_gl_context_info = cl_uint;

const
// Additional Error Codes
  CL_INVALID_GL_SHAREGROUP_REFERENCE_KHR = -1000;

// cl_gl_context_info
  CL_CURRENT_DEVICE_FOR_GL_CONTEXT_KHR   = $2006;
  CL_DEVICES_FOR_GL_CONTEXT_KHR          = $2007;

// Additional cl_context_properties
  CL_GL_CONTEXT_KHR                      = $2008;
  CL_EGL_DISPLAY_KHR                     = $2009;
  CL_GLX_DISPLAY_KHR                     = $200A;
  CL_WGL_HDC_KHR                         = $200B;
  CL_CGL_SHAREGROUP_KHR                  = $200C;

type

  _t_clGetGLContextInfoKHR = function ( // CL_API_SUFFIX__VERSION_1_0
    properties           : p_cl_context_properties;
    param_name           : cl_gl_context_info;
    param_value_size     : size_t;
    param_value          : pointer;
    param_value_size_ret : p_size_t
    ): cl_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  clGetGLContextInfoKHR_fn = _t_clGetGLContextInfoKHR;

var
  clCreateFromGLBuffer: _t_clCreateFromGLBuffer;
  clCreateFromGLTexture: _t_clCreateFromGLTexture;
  clCreateFromGLRenderbuffer: _t_clCreateFromGLRenderbuffer;
  clGetGLObjectInfo: _t_clGetGLObjectInfo;
  clGetGLTextureInfo: _t_clGetGLTextureInfo;
  clEnqueueAcquireGLObjects: _t_clEnqueueAcquireGLObjects;
  clEnqueueReleaseGLObjects: _t_clEnqueueReleaseGLObjects;
  clGetGLContextInfoKHR: _t_clGetGLContextInfoKHR;
  clCreateFromGLTexture2D: _t_clCreateFromGLTexture2D;
  clCreateFromGLTexture3D: _t_clCreateFromGLTexture3D;

///////////////////////////////////////////////////////////////////////////////////////////////////
{cl_gl_ext.h}
(*
 * For each extension, follow this template
 * /* cl_VEN_extname extension  */
 * #define cl_VEN_extname 1
 * ... define new types, if any
 * ... define new tokens, if any
 * ... define new APIs, if any
 *
 *  If you need GLtypes here, mirror them with a cl_GLtype, rather than including a GL header
 *  This allows us to avoid having to decide whether to include GL headers or GLES here.
 *
 *  cl_khr_gl_event  extension
 *  See section 9.9 in the OpenCL 1.1 spec for more information
 *)
const
  CL_COMMAND_GL_FENCE_SYNC_OBJECT_KHR = $200D;

type
  _t_clCreateEventFromGLsyncKHR = function ( // CL_EXT_SUFFIX__VERSION_1_1
    context     : cl_context;
    cl_GLsync   : cl_GLsync;
    errcode_ret : p_cl_int
    ): cl_event; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

var
  clCreateEventFromGLsyncKHR: _t_clCreateEventFromGLsyncKHR;

///////////////////////////////////////////////////////////////////////////////////////////////////
// Delphi functions
procedure LoadOpenCL_GL; // Call after CL.LoadOpenCL
procedure UnloadOpenCL_GL;

IMPLEMENTATION
procedure LoadOpenCL_GL; // Call after CL.LoadOpenCL
begin
// Load API functions
  @clCreateFromGLBuffer:=GetOpenCLFuncAddress('clCreateFromGLBuffer');
  @clCreateFromGLTexture:=GetOpenCLFuncAddress('clCreateFromGLTexture');
  @clCreateFromGLRenderbuffer:=GetOpenCLFuncAddress('clCreateFromGLRenderbuffer');
  @clGetGLObjectInfo:=GetOpenCLFuncAddress('clGetGLObjectInfo');
  @clGetGLTextureInfo:=GetOpenCLFuncAddress('clGetGLTextureInfo');
  @clEnqueueAcquireGLObjects:=GetOpenCLFuncAddress('clEnqueueAcquireGLObjects');
  @clEnqueueReleaseGLObjects:=GetOpenCLFuncAddress('clEnqueueReleaseGLObjects');
  @clGetGLContextInfoKHR:=GetOpenCLFuncAddress('clGetGLContextInfoKHR');
  @clCreateEventFromGLsyncKHR:=GetOpenCLFuncAddress('clCreateEventFromGLsyncKHR');
  @clCreateFromGLTexture2D:=GetOpenCLFuncAddress('clCreateFromGLTexture2D');
  @clCreateFromGLTexture3D:=GetOpenCLFuncAddress('clCreateFromGLTexture3D');
end;

procedure UnloadOpenCL_GL;
begin
// Load API functions
  @clCreateFromGLBuffer:=nil;
  @clCreateFromGLTexture:=nil;
  @clCreateFromGLRenderbuffer:=nil;
  @clGetGLObjectInfo:=nil;
  @clGetGLTextureInfo:=nil;
  @clEnqueueAcquireGLObjects:=nil;
  @clEnqueueReleaseGLObjects:=nil;
  @clGetGLContextInfoKHR:=nil;
  @clCreateEventFromGLsyncKHR:=nil;
  @clCreateFromGLTexture2D:=nil;
  @clCreateFromGLTexture3D:=nil;
end;

INITIALIZATION
  LoadOpenCL_GL;
FINALIZATION
  UnloadOpenCL_GL;
END.
