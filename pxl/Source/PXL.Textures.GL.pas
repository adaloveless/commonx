unit PXL.Textures.GL;
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
  PXL.TypeDef, PXL.Types, PXL.Surfaces, PXL.Devices, PXL.Textures, PXL.Types.GL;

type
  TGLLockableTexture = class(TCustomLockableTexture)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TGLDeviceContext;

    FSurface: TPixelSurface;
    FTexture: Cardinal;
    FTextureFormat: Cardinal;
    FTextureInternalFormat: Cardinal;

    procedure DetermineFormats;

    function CreateTextureSurface: Boolean;
    procedure DestroyTextureSurface;

    function UpdateTextureFromSurface: Boolean;
  protected
    function DoInitialize: Boolean; override;
    procedure DoFinalize; override;

    function DoLock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean; override;
    function DoUnlock: Boolean; override;

    function DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
      const DestPos: TPoint2px): Boolean; override;
  public
    constructor Create(const ADevice: TCustomDevice; const AutoSubscribe: Boolean = True); override;
    destructor Destroy; override;

    function Bind(const Channel: Integer): Boolean; override;

    function DeviceRestore: Boolean; override;
    procedure DeviceRelease; override;

    property Context: TGLDeviceContext read FContext;

    property Surface: TPixelSurface read FSurface;

    property Texture: Cardinal read FTexture;
    property TextureFormat: Cardinal read FTextureFormat;
    property TextureInternalFormat: Cardinal read FTextureInternalFormat;
  end;

  TGLDrawableTexture = class(TCustomDrawableTexture)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TGLDeviceContext;

    FTexture: Cardinal;
    FFrameBuffer: Cardinal;
    FDepthBuffer: Cardinal;
    FStencilBuffer: Cardinal;

    SavedFrameBuffer: Cardinal;
    SavedViewport: array[0..3] of Integer;

    function CreateTextureSurface: Boolean;
    procedure DestroyTextureSurface;
    function CreateFrameObjects: Boolean;
    procedure DestroyFrameObjects;
  protected
    function DoInitialize: Boolean; override;
    procedure DoFinalize; override;

    function DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
      const DestPos: TPoint2px): Boolean; override;
  public
    function Bind(const Channel: Integer): Boolean; override;

    function Clear: Boolean; override;

    function DeviceRestore: Boolean; override;
    procedure DeviceRelease; override;

    function BeginDraw: Boolean; override;
    procedure EndDraw; override;

    property Context: TGLDeviceContext read FContext;

    property Texture: Cardinal read FTexture;
    property FrameBuffer: Cardinal read FFrameBuffer;
    property DepthBuffer: Cardinal read FDepthBuffer;
    property StencilBuffer: Cardinal read FStencilBuffer;
  end;

implementation

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

  PXL.Formats;

{$REGION 'Global Functions'}

function CreateAndInitializeTexture(const MipMapping: Boolean; const Extensions: TExtensions): Cardinal;
begin
  glActiveTexture(GL_TEXTURE0);
  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);

  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

  if Extensions.VersionCheck(1, 4) then
  begin
    if Mipmapping then
    begin
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

      if not Extensions.EXT_framebuffer_object then
        glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);
    end
    else
    begin
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

      if not Extensions.EXT_framebuffer_object then
        glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_FALSE);
    end;
  end
  else
  begin // OpenGL 1.3 and lower.
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  end;

  if glGetError <> GL_NO_ERROR then
  begin
    glBindTexture(GL_TEXTURE_2D, 0);

    if Result <> 0 then
    begin
      glDeleteTextures(1, @Result);
      Result := 0;
    end;
  end;
end;

procedure DestroyAndFinalizeTexture(var Texture: Cardinal);
begin
  glBindTexture(GL_TEXTURE_2D, 0);

  if Texture <> 0 then
  begin
    glDeleteTextures(1, @Texture);
    Texture := 0;
  end;
end;

procedure GenerateMipMaps;
begin
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glGenerateMipmapEXT(GL_TEXTURE_2D);
end;

{$ENDREGION}
{$REGION 'TGLLockableTexture'}

constructor TGLLockableTexture.Create(const ADevice: TCustomDevice; const AutoSubscribe: Boolean);
begin
  inherited;

  FSurface := TPixelSurface.Create;
end;

destructor TGLLockableTexture.Destroy;
begin
  FSurface.Free;

  inherited;
end;

procedure TGLLockableTexture.DetermineFormats;

  function CheckForCommonFormats: Boolean;
  begin
    case FPixelFormat of
      TPixelFormat.L8:
      begin // 8-bit luminance
        FTextureInternalFormat := GL_LUMINANCE;
        FTextureFormat := GL_LUMINANCE;
        Result := True;
      end;

      TPixelFormat.A8L8:
      begin // 16-bit luminance + alpha
        FTextureInternalFormat := GL_LUMINANCE_ALPHA;
        FTextureFormat := GL_LUMINANCE_ALPHA;
        Result := True;
      end;

      TPixelFormat.A8R8G8B8:
      begin // 32-bit RGBA extension
        FTextureInternalFormat := GL_RGBA8;
        FTextureFormat := GL_BGRA;
        Result := True;
      end;

      TPixelFormat.A8B8G8R8:
      begin // 32-bit BGRA
        FTextureInternalFormat := GL_RGBA8;
        FTextureFormat := GL_RGBA;
        Result := True;
      end;

      else
        Result := False;
    end;
  end;

  procedure ApproximatePixelFormat;
  var
    Formats: TPixelFormatList;
  begin
    Formats := TPixelFormatList.Create;
    try
      Formats.Insert(TPixelFormat.A8B8G8R8);
      Formats.Insert(TPixelFormat.A8R8G8B8);
      Formats.Insert(TPixelFormat.L8);
      Formats.Insert(TPixelFormat.A8L8);

      FPixelFormat := FindClosestPixelFormat(FPixelFormat, Formats);
    finally
      Formats.Free;
    end;
  end;

begin
  if not CheckForCommonFormats then
  begin // Exotic pixel format needs to be approximated.
    ApproximatePixelFormat;

    // If still no match was found, force some standard format.
    if FPixelFormat = TPixelFormat.Unknown then
      FPixelFormat := TPixelFormat.A8R8G8B8;

    CheckForCommonFormats;
  end;

  FBytesPerPixel := FPixelFormat.Bytes;
end;

function TGLLockableTexture.CreateTextureSurface: Boolean;
begin
  if FContext = nil then
    Exit(False);

  FTexture := CreateAndInitializeTexture(MipMapping, FContext.Extensions);
  if FTexture = 0 then
    Exit(False);

  if FPixelFormat.Bytes >= 4 then
    glPixelStorei(GL_PACK_ALIGNMENT, 4)
  else
    glPixelStorei(GL_PACK_ALIGNMENT, 1);

  glTexImage2D(GL_TEXTURE_2D, 0, FTextureInternalFormat, Width, Height, 0, FTextureFormat, GL_UNSIGNED_BYTE,
    FSurface.Bits);

  if glGetError <> GL_NO_ERROR then
  begin
    DestroyTextureSurface;
    Exit(False);
  end;

  if MipMapping and FContext.Extensions.EXT_framebuffer_object then
    GenerateMipMaps;

  glBindTexture(GL_TEXTURE_2D, 0);

  Result := glGetError = GL_NO_ERROR;
end;

procedure TGLLockableTexture.DestroyTextureSurface;
begin
  DestroyAndFinalizeTexture(FTexture);
end;

function TGLLockableTexture.DoInitialize: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TGLDeviceContext)) then
    Exit(False);

  FContext := TGLDeviceContext(Device.Context);

  DetermineFormats;

  FSurface.SetSize(Width, Height, FPixelFormat);
  FSurface.Clear(0);

  Result := CreateTextureSurface;
end;

procedure TGLLockableTexture.DoFinalize;
begin
  DestroyTextureSurface;
  FContext := nil;
end;

function TGLLockableTexture.DeviceRestore: Boolean;
begin
  Result := CreateTextureSurface;
end;

procedure TGLLockableTexture.DeviceRelease;
begin
  DestroyTextureSurface;
end;

function TGLLockableTexture.Bind(const Channel: Integer): Boolean;
begin
  if FTexture = 0 then
    Exit(False);

  glBindTexture(GL_TEXTURE_2D, FTexture);

  Result := glGetError = GL_NO_ERROR;
end;

function TGLLockableTexture.UpdateTextureFromSurface: Boolean;
begin
  if (FTexture = 0) or (FSurface = nil) or FSurface.IsEmpty then
    Exit(False);

  glActiveTexture(GL_TEXTURE0);

  glBindTexture(GL_TEXTURE_2D, FTexture);
  try
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, Width, Height, FTextureFormat, GL_UNSIGNED_BYTE, FSurface.Bits);

    if MipMapping and FContext.Extensions.EXT_framebuffer_object then
      GenerateMipMaps;
  finally
    glBindTexture(GL_TEXTURE_2D, 0);
  end;

  Result := glGetError = GL_NO_ERROR;
end;

function TGLLockableTexture.DoLock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean;
begin
  Result := LockSurface(FSurface, Rect, LockedPixels);
end;

function TGLLockableTexture.DoUnlock: Boolean;
begin
  Result := UpdateTextureFromSurface;
end;

function TGLLockableTexture.DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
  const DestPos: TPoint2px): Boolean;
var
  SavedFrameBuffer: GLuint;
  TempBuffer, TempScanline: Pointer;
  ScanlineBytes: Cardinal;
  I, CopyWidth: Integer;
begin
  if Source is TGLDrawableTexture then
  begin
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, @SavedFrameBuffer);
  {$IFDEF POSIX}
    glBindFramebufferEXT(GL_FRAMEBUFFER, TGLDrawableTexture(Source).FFrameBuffer);
  {$ELSE}
    glBindFramebuffer(GL_FRAMEBUFFER, TGLDrawableTexture(Source).FFrameBuffer);
  {$ENDIF}
    try
      if (Size <> Source.Size) or (DestPos <> ZeroPoint2px) or (SourceRect.TopLeft <> ZeroPoint2px) or
        (SourceRect.Size <> Source.Size) or (not (FPixelFormat in [TPixelFormat.A8B8G8R8, TPixelFormat.X8B8G8R8])) then
      begin
        CopyWidth := SourceRect.Width;
        ScanlineBytes := Cardinal(CopyWidth) * SizeOf(TIntColor);

        GetMem(TempBuffer, SourceRect.Height * ScanlineBytes);
        try
          glReadPixels(SourceRect.Left, SourceRect.Top, SourceRect.Width, SourceRect.Height, GL_RGBA, GL_UNSIGNED_BYTE,
            TempBuffer);

          if FPixelFormat in [TPixelFormat.A8B8G8R8, TPixelFormat.X8B8G8R8] then
            // Direct pixel copy.
            for I := 0 to SourceRect.Height - 1 do
              Move(Pointer(PtrUInt(TempBuffer) + Cardinal(I) * ScanlineBytes)^,
                FSurface.PixelPtr[DestPos.X, DestPos.Y + I]^, ScanlineBytes)
          else if FPixelFormat in [TPixelFormat.A8R8G8B8, TPixelFormat.X8R8G8B8] then
            // Swizzle from BGRA to RGBA.
            for I := 0 to SourceRect.Height - 1 do
              PixelXTo32Array(Pointer(PtrUInt(TempBuffer) + Cardinal(I) * ScanlineBytes),
                FSurface.PixelPtr[DestPos.X, DestPos.Y + I], TPixelFormat.A8B8G8R8, CopyWidth)
          else
          begin // Convert from BGRA to custom pixel format.
            GetMem(TempScanline, ScanlineBytes);
            try
              for I := 0 to SourceRect.Height - 1 do
              begin
                PixelXTo32Array(Pointer(PtrUInt(TempBuffer) + Cardinal(I) * ScanlineBytes), TempScanline,
                  TPixelFormat.A8B8G8R8, CopyWidth);

                Pixel32ToXArray(TempScanline, FSurface.PixelPtr[DestPos.X, DestPos.Y + I], FPixelFormat, CopyWidth);
              end;
            finally
              FreeMem(TempScanline);
            end;
          end;
        finally
          FreeMem(TempBuffer);
        end;
      end
      else
        glReadPixels(0, 0, FSurface.Width, FSurface.Height, GL_RGBA, GL_UNSIGNED_BYTE, FSurface.Bits);
    finally
    {$IFDEF POSIX}
      glBindFramebufferEXT(GL_FRAMEBUFFER, SavedFrameBuffer);
    {$ELSE}
      glBindFramebuffer(GL_FRAMEBUFFER, SavedFrameBuffer);
    {$ENDIF}
    end;

    Result := (glGetError = GL_NO_ERROR) and UpdateTextureFromSurface;
  end
  else
    Result := inherited;
end;

{$ENDREGION}
{$REGION 'TGLDrawableTexture'}

function TGLDrawableTexture.CreateTextureSurface: Boolean;
begin
  if FContext = nil then
    Exit(False);

  FTexture := CreateAndInitializeTexture(MipMapping, FContext.Extensions);
  if FTexture = 0 then
    Exit(False);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Width, Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);

  if glGetError <> GL_NO_ERROR then
  begin
    DestroyTextureSurface;
    Exit(False);
  end;

  if MipMapping and FContext.Extensions.EXT_framebuffer_object then
    GenerateMipMaps;

  glBindTexture(GL_TEXTURE_2D, 0);

  Result := glGetError = GL_NO_ERROR;
end;

procedure TGLDrawableTexture.DestroyTextureSurface;
begin
  DestroyAndFinalizeTexture(FTexture);
end;

function TGLDrawableTexture.CreateFrameObjects: Boolean;
var
  PrevFrameBuffer, PrevRenderBuffer: Cardinal;
begin
  if FContext = nil then
    Exit(False);

  glGetIntegerv(GL_FRAMEBUFFER_BINDING_EXT, @PrevFrameBuffer);
  try
    glGenFramebuffersEXT(1, @FFrameBuffer);
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FFrameBuffer);

    if (glGetError <> GL_NO_ERROR) or (FFrameBuffer = 0) then
      Exit(False);

    glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, FTexture, 0);

    if DepthStencil <> TDepthStencil.None then
    begin
      glGetIntegerv(GL_RENDERBUFFER_BINDING_EXT, @PrevRenderBuffer);
      try
        glGenRenderbuffersEXT(1, @FDepthBuffer);
        glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, FDepthBuffer);

        case DepthStencil of
          TDepthStencil.Full:
            if FContext.Extensions.EXT_packed_depth_stencil then
            begin // Interleaved Depth / Stencil Buffers.
              glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH24_STENCIL8_EXT, Width, Height);
              glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT,
                FDepthBuffer);
              glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_STENCIL_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT,
                FDepthBuffer);
            end
            else
            begin // Separate Depth and Stencil Buffers.
              glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT24, Width, Height);
              glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT,
                FDepthBuffer);

              glGenRenderbuffersEXT(1, @FStencilBuffer);
              glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, FStencilBuffer);
              glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_STENCIL_INDEX, Width, Height);
              glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_STENCIL_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT,
                FStencilBuffer);
            end;

          TDepthStencil.DepthOnly:
            begin // Depth Buffer Alone.
              glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT24, Width, Height);
              glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT,
                FDepthBuffer);
            end;
        end;

        if (glGetError <> GL_NO_ERROR) or
          (glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT) <> GL_FRAMEBUFFER_COMPLETE_EXT) then
        begin
          DestroyFrameObjects;
          Exit(False);
        end;
      finally
        glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, PrevRenderBuffer);
      end;
    end;
  finally
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, PrevFrameBuffer);
  end;

  Result := True;
end;

procedure TGLDrawableTexture.DestroyFrameObjects;
begin
  if FStencilBuffer <> 0 then
  begin
    glDeleteRenderbuffersEXT(1, @FStencilBuffer);
    FStencilBuffer := 0;
  end;

  if FDepthBuffer <> 0 then
  begin
    glDeleteRenderbuffersEXT(1, @FDepthBuffer);
    FDepthBuffer := 0;
  end;

  if FFrameBuffer <> 0 then
  begin
    glDeleteFramebuffersEXT(1, @FFrameBuffer);
    FFrameBuffer := 0;
  end;
end;

function TGLDrawableTexture.DoInitialize: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TGLDeviceContext)) then
    Exit(False);

  FContext := TGLDeviceContext(Device.Context);

  if not FContext.Extensions.EXT_framebuffer_object then
    Exit(False);

  FPixelFormat := TPixelFormat.A8B8G8R8;

  if not CreateTextureSurface then
    Exit(False);

  if not CreateFrameObjects then
  begin
    DestroyTextureSurface;
    Exit(False);
  end;

  Result := True;
end;

procedure TGLDrawableTexture.DoFinalize;
begin
  DestroyFrameObjects;
  DestroyTextureSurface;
  FContext := nil;
end;

function TGLDrawableTexture.DeviceRestore: Boolean;
begin
  if not CreateTextureSurface then
    Exit(False);

  if not CreateFrameObjects then
  begin
    DestroyTextureSurface;
    Exit(False);
  end;

  Result := True;
end;

procedure TGLDrawableTexture.DeviceRelease;
begin
  DestroyFrameObjects;
  DestroyTextureSurface;
end;

function TGLDrawableTexture.Bind(const Channel: Integer): Boolean;
begin
  if FTexture = 0 then
    Exit(False);

  glBindTexture(GL_TEXTURE_2D, FTexture);

  Result := glGetError = GL_NO_ERROR;
end;

function TGLDrawableTexture.Clear: Boolean;
begin
  if not BeginDraw then
    Exit(False);
  try
    glClearColor(0.0, 0.0, 0.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT);
    Result := glGetError = GL_NO_ERROR;
  finally
    EndDraw;
  end;
end;

function TGLDrawableTexture.BeginDraw: Boolean;
begin
  if (FFrameBuffer = 0) or (FContext = nil) then
    Exit(False);

  glGetIntegerv(GL_VIEWPORT, @SavedViewport[0]);
  glGetIntegerv(GL_FRAMEBUFFER_BINDING_EXT, @SavedFrameBuffer);

  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FFrameBuffer);
  glViewport(0, 0, Width, Height);

  Result :=  glGetError = GL_NO_ERROR;
  if Result then
    FContext.FrameBufferLevelIncrement;
end;

procedure TGLDrawableTexture.EndDraw;
begin
  FContext.FrameBufferLevelDecrement;

  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, SavedFrameBuffer);
  SavedFrameBuffer := 0;

  glViewport(SavedViewport[0], SavedViewport[1], SavedViewport[2], SavedViewport[3]);
  FillChar(SavedViewport, SizeOf(SavedViewport), 0);

  if MipMapping then
  begin
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, FTexture);

    GenerateMipMaps;

    glBindTexture(GL_TEXTURE_2D, 0);
  end;
end;

function TGLDrawableTexture.DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
  const DestPos: TPoint2px): Boolean;
var
  SavedFrameBuffer: GLuint;
  TempBuffer, TempScanline: Pointer;
  ScanlineBytes: Cardinal;
  I, CopyWidth: Integer;
begin
  if Source is TGLLockableTexture then
  begin
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, FTexture);
    try
      if (Size <> Source.Size) or (DestPos <> ZeroPoint2px) or (SourceRect.TopLeft <> ZeroPoint2px) or
        (SourceRect.Size <> Source.Size) or (TGLLockableTexture(Source).PixelFormat <> TPixelFormat.A8B8G8R8) then
      begin
        CopyWidth := SourceRect.Width;
        ScanlineBytes := Cardinal(CopyWidth) * SizeOf(TIntColor);

        GetMem(TempBuffer, SourceRect.Height * ScanlineBytes);
        try
          if TGLLockableTexture(Source).PixelFormat = TPixelFormat.A8B8G8R8 then
          begin
            // Direct pixel copy.
            for I := 0 to SourceRect.Height - 1 do
              Move(TGLLockableTexture(Source).Surface.PixelPtr[SourceRect.Left, SourceRect.Top + I]^,
                Pointer(PtrUInt(TempBuffer) + Cardinal(I) * ScanlineBytes)^, ScanlineBytes)
          end
          else if TGLLockableTexture(Source).PixelFormat = TPixelFormat.A8R8G8B8 then
            // Swizzle from RGBA to BGRA.
            for I := 0 to SourceRect.Height - 1 do
              Pixel32ToXArray(TGLLockableTexture(Source).Surface.PixelPtr[SourceRect.Left, SourceRect.Top + I],
                Pointer(PtrUInt(TempBuffer) + Cardinal(I) * ScanlineBytes), TPixelFormat.A8B8G8R8, CopyWidth)
          else
          begin // Convert from custom pixel format to BGRA.
            GetMem(TempScanline, ScanlineBytes);
            try
              for I := 0 to SourceRect.Height - 1 do
              begin
                PixelXTo32Array(TGLLockableTexture(Source).Surface.PixelPtr[SourceRect.Left, SourceRect.Top + I],
                  TempScanline, TGLLockableTexture(Source).PixelFormat, CopyWidth);

                Pixel32ToXArray(TempScanline, Pointer(PtrUInt(TempBuffer) + Cardinal(I) * ScanlineBytes),
                  TPixelFormat.A8B8G8R8, CopyWidth);
              end;
            finally
              FreeMem(TempScanline);
            end;
          end;

          glTexSubImage2D(GL_TEXTURE_2D, 0, DestPos.X, DestPos.Y, SourceRect.Width, SourceRect.Height, GL_RGBA,
            GL_UNSIGNED_BYTE, TempBuffer);
        finally
          FreeMem(TempBuffer);
        end;
      end
      else
        glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, TGLLockableTexture(Source).Surface.Width,
          TGLLockableTexture(Source).Surface.Height, GL_RGBA, GL_UNSIGNED_BYTE,
          TGLLockableTexture(Source).Surface.Bits);

      if MipMapping  then
        GenerateMipMaps;
    finally
      glBindTexture(GL_TEXTURE_2D, 0);
    end;

    Result := glGetError = GL_NO_ERROR;
  end
  else if Source is TGLDrawableTexture then
  begin
    glActiveTexture(GL_TEXTURE0);

    glBindTexture(GL_TEXTURE_2D, FTexture);
    try
      glGetIntegerv(GL_FRAMEBUFFER_BINDING, @SavedFrameBuffer);
    {$IFDEF POSIX}
      glBindFramebufferEXT(GL_FRAMEBUFFER, TGLDrawableTexture(Source).FFrameBuffer);
    {$ELSE}
      glBindFramebuffer(GL_FRAMEBUFFER, TGLDrawableTexture(Source).FFrameBuffer);
    {$ENDIF}
      try
        glCopyTexSubImage2D(GL_TEXTURE_2D, 0, DestPos.X, DestPos.Y, SourceRect.Left, SourceRect.Top, SourceRect.Width,
          SourceRect.Height);
      finally
      {$IFDEF POSIX}
        glBindFramebufferEXT(GL_FRAMEBUFFER, SavedFrameBuffer);
      {$ELSE}
        glBindFramebuffer(GL_FRAMEBUFFER, SavedFrameBuffer);
      {$ENDIF}
      end;

      if MipMapping  then
        GenerateMipMaps;
    finally
      glBindTexture(GL_TEXTURE_2D, 0);
    end;

    Result := glGetError = GL_NO_ERROR;
  end
  else
    Result := inherited;
end;

{$ENDREGION}

end.
