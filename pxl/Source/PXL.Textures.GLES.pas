unit PXL.Textures.GLES;
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
  PXL.TypeDef, PXL.Types, PXL.Surfaces, PXL.Devices, PXL.Textures, PXL.Types.GLES;

type
  TGLESLockableTexture = class(TCustomLockableTexture)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TGLESDeviceContext;

    FSurface: TPixelSurface;
    FTexture: Cardinal;
    FTextureFormat: Cardinal;
    FTextureInternalFormat: Cardinal;
    FTextureNPOT: Boolean;

    procedure DetermineFormats;

    function CreateTextureSurface: Boolean;
    procedure DestroyTextureSurface;

    function UpdateTextureFromSurface: Boolean;
  protected
    function DoInitialize: Boolean; override;
    procedure DoFinalize; override;

    function DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
      const DestPos: TPoint2px): Boolean; override;
  public
    constructor Create(const ADevice: TCustomDevice; const AutoSubscribe: Boolean); override;
    destructor Destroy; override;

    function Bind(const Channel: Integer): Boolean; override;

    function DeviceRestore: Boolean; override;
    procedure DeviceRelease; override;

    function DoLock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean; override;
    function DoUnlock: Boolean; override;

    property Context: TGLESDeviceContext read FContext;

    property Surface: TPixelSurface read FSurface;

    property Texture: Cardinal read FTexture;
    property TextureFormat: Cardinal read FTextureFormat;
    property TextureInternalFormat: Cardinal read FTextureInternalFormat;
  end;

  TGLESDrawableTexture = class(TCustomDrawableTexture)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TGLESDeviceContext;

    FTexture: Cardinal;
    FFrameBuffer: Cardinal;
    FDepthBuffer: Cardinal;
    FStencilBuffer: Cardinal;
    FTextureNPOT: Boolean;

    FSavedFrameBuffer: Cardinal;
    FSavedViewport: array[0..3] of Integer;

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

    property Context: TGLESDeviceContext read FContext;

    property Texture: Cardinal read FTexture;
    property FrameBuffer: Cardinal read FFrameBuffer;
    property DepthBuffer: Cardinal read FDepthBuffer;
    property StencilBuffer: Cardinal read FStencilBuffer;
  end;

implementation

uses
{$IFDEF FPC}
  {$IFDEF ANDROID}
    Android.GLES2,
  {$ELSE}
    gles20,
  {$ENDIF}
{$ELSE}
  {$IFDEF ANDROID}
    Androidapi.Gles2, Androidapi.Gles2ext,
  {$ENDIF}
  {$IFDEF IOS}
    iOSapi.OpenGLES,
  {$ENDIF}
{$ENDIF}

  PXL.Formats;

{$REGION 'Global Functions'}

function CreateAndInitializeTexture(const MipMapping, TextureNPOT: Boolean): Cardinal;
begin
  glActiveTexture(GL_TEXTURE0);
  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);

  if TextureNPOT then
  begin
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  end
  else
  begin
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  end;

  if Mipmapping and (not TextureNPOT) then
  begin
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  end
  else
  begin
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
  glGenerateMipmap(GL_TEXTURE_2D);
end;

{$ENDREGION}
{$REGION 'TGLESLockableTexture'}

constructor TGLESLockableTexture.Create(const ADevice: TCustomDevice; const AutoSubscribe: Boolean);
begin
  inherited;

  FSurface := TPixelSurface.Create;
end;

destructor TGLESLockableTexture.Destroy;
begin
  FSurface.Free;

  inherited;
end;

procedure TGLESLockableTexture.DetermineFormats;

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
      begin
        if FContext.Extensions.EXT_texture_format_BGRA8888 then
        begin // 32-bit RGBA extension
          FTextureInternalFormat := GL_BGRA_EXT;
          FTextureFormat := GL_BGRA_EXT;
          Result := True;
        end
        else if FContext.Extensions.APPLE_texture_format_BGRA8888 then
        begin // 32-bit RGBA extension (Apple variant)
          FTextureInternalFormat := GL_RGBA;
          FTextureFormat := GL_BGRA_EXT;
          Result := True;
        end
        else
        begin // Swizzle back to 32-bit BGRA
          FTextureInternalFormat := GL_RGBA;
          FTextureFormat := GL_RGBA;
          Result := True;
        end;
      end;

      TPixelFormat.A8B8G8R8:
      begin // 32-bit BGRA
        FTextureInternalFormat := GL_RGBA;
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
      Formats.Insert(TPixelFormat.L8);
      Formats.Insert(TPixelFormat.A8L8);

      // A8R8G8B8 support is optional.
      if FContext.Extensions.EXT_texture_format_BGRA8888 or FContext.Extensions.APPLE_texture_format_BGRA8888 then
        Formats.Insert(TPixelFormat.A8R8G8B8);

      FPixelFormat := FindClosestPixelFormat(FPixelFormat, Formats);
    finally
      Formats.Free;
    end;
  end;

begin
  if (FContext <> nil) and (not CheckForCommonFormats) then
  begin // Exotic pixel format needs to be approximated.
    ApproximatePixelFormat;

    // If still no match was found, force some standard format.
    if FPixelFormat = TPixelFormat.Unknown then
      FPixelFormat := TPixelFormat.A8B8G8R8;

    CheckForCommonFormats;
  end;

  FBytesPerPixel := FPixelFormat.Bytes;
end;

function TGLESLockableTexture.CreateTextureSurface: Boolean;
begin
  FTextureNPOT := ((not IsPowerOfTwo(Width)) or (not IsPowerOfTwo(Height))) and
    ((FContext = nil) or (not FContext.Extensions.OES_texture_npot));

  FTexture := CreateAndInitializeTexture(MipMapping, FTextureNPOT);
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

  if MipMapping and (not FTextureNPOT) then
    GenerateMipMaps;

  glBindTexture(GL_TEXTURE_2D, 0);

  Result := glGetError = GL_NO_ERROR;
end;

procedure TGLESLockableTexture.DestroyTextureSurface;
begin
  DestroyAndFinalizeTexture(FTexture);
end;

function TGLESLockableTexture.DoInitialize: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TGLESDeviceContext)) then
    Exit(False);

  FContext := TGLESDeviceContext(Device.Context);

  DetermineFormats;

  FSurface.SetSize(Width, Height, FPixelFormat);
  FSurface.Clear(0);

  Result := CreateTextureSurface;
end;

procedure TGLESLockableTexture.DoFinalize;
begin
  DestroyTextureSurface;
  FContext := nil;
end;

function TGLESLockableTexture.DeviceRestore: Boolean;
begin
  Result := CreateTextureSurface;
end;

procedure TGLESLockableTexture.DeviceRelease;
begin
  DestroyTextureSurface;
end;

function TGLESLockableTexture.Bind(const Channel: Integer): Boolean;
begin
  glBindTexture(GL_TEXTURE_2D, FTexture);
  Result := glGetError = GL_NO_ERROR;
end;

function TGLESLockableTexture.UpdateTextureFromSurface: Boolean;
begin
  if (FTexture = 0) or (FSurface = nil) or FSurface.IsEmpty then
    Exit(False);

  glActiveTexture(GL_TEXTURE0);

  glBindTexture(GL_TEXTURE_2D, FTexture);
  try
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, Width, Height, FTextureFormat, GL_UNSIGNED_BYTE, FSurface.Bits);

    if MipMapping and (not FTextureNPOT) then
      GenerateMipMaps;
  finally
    glBindTexture(GL_TEXTURE_2D, 0);
  end;

  Result := glGetError = GL_NO_ERROR;
end;

function TGLESLockableTexture.DoLock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean;
begin
  Result := LockSurface(FSurface, Rect, LockedPixels);
end;

function TGLESLockableTexture.DoUnlock: Boolean;
begin
  Result := UpdateTextureFromSurface;
end;

function TGLESLockableTexture.DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
  const DestPos: TPoint2px): Boolean;
var
  SavedFrameBuffer: GLuint;
  TempBuffer, TempScanline: Pointer;
  ScanlineBytes: Cardinal;
  I, CopyWidth: Integer;
begin
  if Source is TGLESDrawableTexture then
  begin
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, @SavedFrameBuffer);
    glBindFramebuffer(GL_FRAMEBUFFER, TGLESDrawableTexture(Source).FFrameBuffer);
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
      glBindFramebuffer(GL_FRAMEBUFFER, SavedFrameBuffer);
    end;

    Result := (glGetError = GL_NO_ERROR) and UpdateTextureFromSurface;
  end
  else
    Result := inherited;
end;

{$ENDREGION}
{$REGION 'TGLESDrawableTexture'}

function TGLESDrawableTexture.CreateTextureSurface: Boolean;
begin
  FTextureNPOT := ((not IsPowerOfTwo(Width)) or (not IsPowerOfTwo(Height))) and
    ((FContext = nil) or (not FContext.Extensions.OES_texture_npot));

  FTexture := CreateAndInitializeTexture(MipMapping, FTextureNPot);
  if FTexture = 0 then
    Exit(False);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Width, Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);

  if glGetError <> GL_NO_ERROR then
  begin
    DestroyTextureSurface;
    Exit(False);
  end;

  if MipMapping and (not FTextureNPOT) then
    GenerateMipMaps;

  glBindTexture(GL_TEXTURE_2D, 0);

  Result := glGetError = GL_NO_ERROR;
end;

procedure TGLESDrawableTexture.DestroyTextureSurface;
begin
  DestroyAndFinalizeTexture(FTexture);
end;

function TGLESDrawableTexture.CreateFrameObjects: Boolean;
var
  PrevFrameBuffer, PrevRenderBuffer: Cardinal;
begin
  if FContext = nil then
    Exit(False);

  glGetIntegerv(GL_FRAMEBUFFER_BINDING, @PrevFrameBuffer);
  try
    glGenFramebuffers(1, @FFrameBuffer);
    glBindFramebuffer(GL_FRAMEBUFFER, FFrameBuffer);

    if (glGetError <> GL_NO_ERROR) or (FFrameBuffer = 0) then
      Exit(False);

    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, FTexture, 0);

    if DepthStencil <> TDepthStencil.None then
    begin
      glGetIntegerv(GL_RENDERBUFFER_BINDING, @PrevRenderBuffer);
      try
        glGenRenderbuffers(1, @FDepthBuffer);
        glBindRenderbuffer(GL_RENDERBUFFER, FDepthBuffer);

        case DepthStencil of
          TDepthStencil.Full:
            if FContext.Extensions.OES_packed_depth_stencil then
            begin // 24-bit Depth Buffer, 8-bit Stencil Buffer
              glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8_OES, Width, Height);
              glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, FDepthBuffer);
              glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, FDepthBuffer);
            end
            else
            begin // 16-bit Depth Buffer, 8-bit Stencil Buffer
              glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT16, Width, Height);
              glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, FDepthBuffer);

              glGenRenderbuffers(1, @FStencilBuffer);
              glBindRenderbuffer(GL_RENDERBUFFER, FStencilBuffer);
              glRenderbufferStorage(GL_RENDERBUFFER, GL_STENCIL_INDEX8, Width, Height);
              glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, FStencilBuffer);
            end;

          TDepthStencil.DepthOnly:
            if FContext.Extensions.OES_depth24 then
            begin // 24-bit Depth Buffer
              glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT24_OES, Width, Height);
              glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, FDepthBuffer);
            end
            else
            begin // 16-bit Depth Buffer
              glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT16, Width, Height);
              glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, FDepthBuffer);
            end;
        end;

        if (glGetError <> GL_NO_ERROR) or (glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE) then
        begin
          DestroyFrameObjects;
          Exit(False);
        end;
      finally
        glBindRenderbuffer(GL_RENDERBUFFER, PrevRenderBuffer);
      end;
    end;
  finally
    glBindFramebuffer(GL_FRAMEBUFFER, PrevFrameBuffer);
  end;

  Result := True;
end;

procedure TGLESDrawableTexture.DestroyFrameObjects;
begin
  if FStencilBuffer <> 0 then
  begin
    glDeleteRenderbuffers(1, @FStencilBuffer);
    FStencilBuffer := 0;
  end;

  if FDepthBuffer <> 0 then
  begin
    glDeleteRenderbuffers(1, @FDepthBuffer);
    FDepthBuffer := 0;
  end;

  if FFrameBuffer <> 0 then
  begin
    glDeleteFramebuffers(1, @FFrameBuffer);
    FFrameBuffer := 0;
  end;
end;

function TGLESDrawableTexture.DoInitialize: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TGLESDeviceContext)) then
    Exit(False);

  FContext := TGLESDeviceContext(Device.Context);
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

procedure TGLESDrawableTexture.DoFinalize;
begin
  DestroyFrameObjects;
  DestroyTextureSurface;
  FContext := nil;
end;

function TGLESDrawableTexture.DeviceRestore: Boolean;
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

procedure TGLESDrawableTexture.DeviceRelease;
begin
  DestroyFrameObjects;
  DestroyTextureSurface;
end;

function TGLESDrawableTexture.Bind(const Channel: Integer): Boolean;
begin
  glBindTexture(GL_TEXTURE_2D, FTexture);
  Result := glGetError = GL_NO_ERROR;
end;

function TGLESDrawableTexture.Clear: Boolean;
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

function TGLESDrawableTexture.BeginDraw: Boolean;
begin
  if (FFrameBuffer = 0) or (FContext = nil) then
    Exit(False);

  glGetIntegerv(GL_VIEWPORT, @FSavedViewport[0]);
  glGetIntegerv(GL_FRAMEBUFFER_BINDING, @FSavedFrameBuffer);

  glBindFramebuffer(GL_FRAMEBUFFER, FFrameBuffer);
  glViewport(0, 0, Width, Height);

  Result := glGetError = GL_NO_ERROR;
  if Result then
    FContext.FrameBufferLevelIncrement;
end;

procedure TGLESDrawableTexture.EndDraw;
begin
  FContext.FrameBufferLevelDecrement;

  glBindFramebuffer(GL_FRAMEBUFFER, FSavedFrameBuffer);
  FSavedFrameBuffer := 0;

  glViewport(FSavedViewport[0], FSavedViewport[1], FSavedViewport[2], FSavedViewport[3]);
  FillChar(FSavedViewport, SizeOf(FSavedViewport), 0);

  if MipMapping and (not FTextureNPOT) then
  begin
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, FTexture);

    GenerateMipMaps;

    glBindTexture(GL_TEXTURE_2D, 0);
  end;
end;

function TGLESDrawableTexture.DoCopyRect(const Source: TCustomBaseTexture; const SourceRect: TIntRect;
  const DestPos: TPoint2px): Boolean;
var
  SavedFrameBuffer: GLuint;
  TempBuffer, TempScanline: Pointer;
  ScanlineBytes: Cardinal;
  I, CopyWidth: Integer;
begin
  if Source is TGLESLockableTexture then
  begin
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, FTexture);
    try
      if (Size <> Source.Size) or (DestPos <> ZeroPoint2px) or (SourceRect.TopLeft <> ZeroPoint2px) or
        (SourceRect.Size <> Source.Size) or (TGLESLockableTexture(Source).PixelFormat <> TPixelFormat.A8B8G8R8) then
      begin
        CopyWidth := SourceRect.Width;
        ScanlineBytes := Cardinal(CopyWidth) * SizeOf(TIntColor);

        GetMem(TempBuffer, SourceRect.Height * ScanlineBytes);
        try
          if TGLESLockableTexture(Source).PixelFormat = TPixelFormat.A8B8G8R8 then
          begin
            // Direct pixel copy.
            for I := 0 to SourceRect.Height - 1 do
              Move(TGLESLockableTexture(Source).Surface.PixelPtr[SourceRect.Left, SourceRect.Top + I]^,
                Pointer(PtrUInt(TempBuffer) + Cardinal(I) * ScanlineBytes)^, ScanlineBytes)
          end
          else if TGLESLockableTexture(Source).PixelFormat = TPixelFormat.A8R8G8B8 then
            // Swizzle from RGBA to BGRA.
            for I := 0 to SourceRect.Height - 1 do
              Pixel32ToXArray(TGLESLockableTexture(Source).Surface.PixelPtr[SourceRect.Left, SourceRect.Top + I],
                Pointer(PtrUInt(TempBuffer) + Cardinal(I) * ScanlineBytes), TPixelFormat.A8B8G8R8, CopyWidth)
          else
          begin // Convert from custom pixel format to BGRA.
            GetMem(TempScanline, ScanlineBytes);
            try
              for I := 0 to SourceRect.Height - 1 do
              begin
                PixelXTo32Array(TGLESLockableTexture(Source).Surface.PixelPtr[SourceRect.Left, SourceRect.Top + I],
                  TempScanline, TGLESLockableTexture(Source).PixelFormat, CopyWidth);

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
        glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, TGLESLockableTexture(Source).Surface.Width,
          TGLESLockableTexture(Source).Surface.Height, GL_RGBA, GL_UNSIGNED_BYTE,
          TGLESLockableTexture(Source).Surface.Bits);

      if MipMapping and (not FTextureNPOT) then
        GenerateMipMaps;
    finally
      glBindTexture(GL_TEXTURE_2D, 0);
    end;

    Result := glGetError = GL_NO_ERROR;
  end
  else if Source is TGLESDrawableTexture then
  begin
    glActiveTexture(GL_TEXTURE0);

    glBindTexture(GL_TEXTURE_2D, FTexture);
    try
      glGetIntegerv(GL_FRAMEBUFFER_BINDING, @SavedFrameBuffer);
      glBindFramebuffer(GL_FRAMEBUFFER, TGLESDrawableTexture(Source).FFrameBuffer);
      try
        glCopyTexSubImage2D(GL_TEXTURE_2D, 0, DestPos.X, DestPos.Y, SourceRect.Left, SourceRect.Top, SourceRect.Width,
          SourceRect.Height);
      finally
        glBindFramebuffer(GL_FRAMEBUFFER, SavedFrameBuffer);
      end;

      if MipMapping and (not FTextureNPOT) then
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
