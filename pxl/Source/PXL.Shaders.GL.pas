unit PXL.Shaders.GL;
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
interface

{$INCLUDE PXL.Config.inc}

uses
{$IFDEF FPC}
  gl, glext,
{$ELSE}
  {$IFDEF MSWINDOWS}
    Winapi.OpenGL, Winapi.OpenGLext,
  {$ENDIF}

  {$IFDEF MACOS}
    Macapi.CocoaTypes, Macapi.OpenGL,
  {$ENDIF}
{$ENDIF}

  SysUtils, PXL.Types, PXL.Canvas;

type
  TShaderString = {$IFDEF FPC}AnsiString{$ELSE}string{$ENDIF};

  TGLCanvasEffect = class(TCustomCanvasEffect)
  private
    FVertexShader: GLuint;
    FFragmentShader: GLuint;
    FProgram: GLuint;
    FOnApply: TStandardNotifyEvent;

    procedure CreateAndLinkProgram;
  public
    constructor Create(const AVertexShader, AFragmentShader: GLuint); overload;
    constructor Create(const VertexShaderText, FragmentShaderText: TShaderString); overload;

    procedure Apply;

    property VertexShader: GLuint read FVertexShader;
    property FragmentShader: GLuint read FFragmentShader;
    property &Program: GLuint read FProgram;

    property OnApply: TStandardNotifyEvent read FOnApply write FOnApply;
  end;

  EGLGeneric = class(Exception);
  EGLCompileShader = class(EGLGeneric);
  EGLLinkShader = class(EGLGeneric);
  EGLInvalidShader = class(EGLGeneric);

function CreateAndCompileShader(const ShaderType: GLenum; const Text: TShaderString): GLuint;
procedure DestroyAndReleaseShader(var Shader: GLuint);

var
  ShaderErrorText: TShaderString = '';

resourcestring
  SGLCompileShader = 'Failed compiling shader [%s].';
  SGLLinkShader = 'Failed linking shader [%s].';
  SGLInvalidShader = 'The specified shader is invalid.';

implementation

const
  MaxShaderErrorLength = 16384;

function CreateAndCompileShader(const ShaderType: GLenum; const Text: TShaderString): GLuint;
var
{$IFDEF FPC}
  ShaderSource: PAnsiChar;
{$ELSE}
  TempBytes: TBytes;
{$ENDIF}
  TextLen: GLint;
  CompileStatus: GLint;
  ErrLength: GLsizei;
begin
  glGetError;

  TextLen := Length(Text);
  if TextLen < 1 then
    Exit(0);

  Result := glCreateShader(ShaderType);
  if Result = 0 then
    Exit;

{$IFDEF FPC}
  ShaderSource := @Text[1];
{$ELSE}
  SetLength(TempBytes, TextLen);
  TMarshal.WriteStringAsAnsi(TPtrWrapper.Create(@TempBytes[0]), Text, TextLen);
{$ENDIF}

{$IFDEF FPC}
  glShaderSource(Result, 1, @ShaderSource, @TextLen);
{$ELSE}
  glShaderSource(Result, 1, @TempBytes, @TextLen);
{$ENDIF}

  glCompileShader(Result);

  glGetShaderiv(Result, GL_COMPILE_STATUS, @CompileStatus);
  if (CompileStatus <> GL_TRUE) or (glGetError <> GL_NO_ERROR) then
  begin
    SetLength(ShaderErrorText, MaxShaderErrorLength);

    glGetShaderInfoLog(Result, MaxShaderErrorLength, @ErrLength, @ShaderErrorText[1]);
    SetLength(ShaderErrorText, ErrLength);

    glDeleteShader(Result);
    Exit(0);
  end;
end;

procedure DestroyAndReleaseShader(var Shader: GLuint);
begin
  if Shader <> 0 then
  begin
    glDeleteShader(Shader);
    Shader := 0;
  end;
end;

constructor TGLCanvasEffect.Create(const AVertexShader, AFragmentShader: GLuint);
begin
  inherited Create;

  if (AVertexShader = 0) or (AFragmentShader = 0) then
    raise EGLInvalidShader.Create(SGLInvalidShader);

  FVertexShader := AVertexShader;
  FFragmentShader := AFragmentShader;

  CreateAndLinkProgram;
end;

constructor TGLCanvasEffect.Create(const VertexShaderText, FragmentShaderText: TShaderString);
var
  VertexShader, FragmentShader: GLuint;
begin
  VertexShader := CreateAndCompileShader(GL_VERTEX_SHADER, VertexShaderText);
  if VertexShader = 0 then
    raise EGLCompileShader.Create(Format(SGLCompileShader, [ShaderErrorText]));

  FragmentShader := CreateAndCompileShader(GL_FRAGMENT_SHADER, FragmentShaderText);
  if FragmentShader = 0 then
    raise EGLCompileShader.Create(Format(SGLCompileShader, [ShaderErrorText]));

  Create(VertexShader, FragmentShader);
end;

procedure TGLCanvasEffect.CreateAndLinkProgram;
var
  LinkStatus, InfoLength: GLint;
  ErrLength: GLsizei;
begin
  FProgram := glCreateProgram;

  glAttachShader(FProgram, FVertexShader);
  glAttachShader(FProgram, FFragmentShader);

  glBindAttribLocation(FProgram, 0, 'InPos');
  glBindAttribLocation(FProgram, 1, 'InpColor');
  glBindAttribLocation(FProgram, 2, 'InpTexCoord');

  glLinkProgram(FProgram);
  glGetProgramiv(FProgram, GL_LINK_STATUS, @LinkStatus);
  glGetProgramiv(FProgram,  GL_INFO_LOG_LENGTH, @InfoLength);

  if (LinkStatus <> GL_TRUE) or (glGetError <> GL_NO_ERROR) then
  begin
    SetLength(ShaderErrorText, MaxShaderErrorLength);

    glGetProgramInfoLog(FProgram, MaxShaderErrorLength, @ErrLength, @ShaderErrorText[1]);
    SetLength(ShaderErrorText, ErrLength);

    glDeleteProgram(FProgram);
    FProgram := 0;

    raise EGLLinkShader.Create(Format(SGLLinkShader, [ShaderErrorText]));
  end;
end;

procedure TGLCanvasEffect.Apply;
begin
  glUseProgram(FProgram);

  if Assigned(FOnApply) then
    FOnApply(Self);
end;

end.
