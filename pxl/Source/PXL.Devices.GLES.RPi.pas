unit PXL.Devices.GLES.RPi;
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

{ These libraries need to be linked in the following order, which is opposite to how they are loaded in "gles20.pas".
  If they are loaded in different order, it will cause linking failure as libEGL.so requires libGLESv2.so. }
{$LINKLIB libGLESv2}
{$LINKLIB libEGL}

uses
  gles20, PXL.TypeDef, PXL.Types, PXL.Devices, PXL.Types.GLES, PXL.Boards.PI.BCM;

type
  TGLESDevice = class(TCustomStateDevice)
  private
    FDeviceContext: TGLESDeviceContext;

    FNativeWindow: EGL_DISPMANX_WINDOW_T;
    FDispmanElement: DISPMANX_ELEMENT_HANDLE_T;
    FDispmanDisplay: DISPMANX_DISPLAY_HANDLE_T;
    FDispmanUpdate: DISPMANX_UPDATE_HANDLE_T;

    FDisplay: EGLDisplay;
    FSurface: EGLSurface;
    FContext: EGLContext;

    FViewportSize: TPoint2px;

    function InitializeDisplay: Boolean;
    procedure FinalizeDisplay;
  protected
    function GetDeviceContext: TCustomDeviceContext; override;

    function InitDevice: Boolean; override;
    procedure DoneDevice; override;
  public
    constructor Create(const AProvider: TCustomDeviceProvider);
    destructor Destroy; override;

    function Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single = 1.0;
      const StencilValue: Cardinal = 0): Boolean; override;

    function Flip: Boolean;

    property Display: EGLDisplay read FDisplay;
    property Surface: EGLSurface read FSurface;
    property Context: EGLContext read FContext;

    property DisplaySize: TPoint2px read FViewportSize;
  end;

implementation

uses
  SysUtils;

constructor TGLESDevice.Create(const AProvider: TCustomDeviceProvider);
begin
  inherited;

  bcm_host_init;

  FDeviceContext := TGLESDeviceContext.Create(Self);

  FTechnology := TDeviceTechnology.OpenGL_ES;
  FTechVersion := $200;
end;

destructor TGLESDevice.Destroy;
begin
  inherited;
  
  FreeAndNil(FDeviceContext);
  bcm_host_deinit;
end;

function TGLESDevice.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FDeviceContext;
end;

function TGLESDevice.InitializeDisplay: Boolean;
const
  ContextAttribs: array[0..2] of EGLint = (EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);
const
  ConfigAttribs: array[0..10] of EGLint = (
    EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT,
    EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
    EGL_BLUE_SIZE, 8,
    EGL_GREEN_SIZE, 8,
    EGL_RED_SIZE, 8,
    EGL_NONE);
var
  Config: EGLConfig;
  NumConfigs: EGLint;
  DisplayWidth, DisplayHeight: LongWord;
  SrcRect, DestRect: VC_RECT_T;
  Alpha: VC_DISPMANX_ALPHA_T;
begin
  FDisplay := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if FDisplay = nil then
    Exit(False);

  if eglInitialize(FDisplay, nil, nil) <> EGL_TRUE then
    Exit(False);

  if eglChooseConfig(FDisplay, @ConfigAttribs[0], @Config, 1, @NumConfigs) <> EGL_TRUE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  if eglBindAPI(EGL_OPENGL_ES_API) <> EGL_TRUE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  FContext := eglCreateContext(FDisplay, Config, EGL_NO_CONTEXT, @ContextAttribs[0]);
  if FContext = EGL_NO_CONTEXT then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  if graphics_get_display_size(DISPMANX_ID_MAIN_LCD, @DisplayWidth, @DisplayHeight) < 0 then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  FViewportSize.X := DisplayWidth;
  FViewportSize.Y := DisplayHeight;

  FDispmanDisplay := vc_dispmanx_display_open(DISPMANX_ID_MAIN_LCD);
  if FDispmanDisplay = DISPMANX_NO_HANDLE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  FDispmanUpdate := vc_dispmanx_update_start(0);
  if FDispmanUpdate = DISPMANX_NO_HANDLE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  DestRect.x := 0;
  DestRect.y := 0;
  DestRect.width := FViewportSize.X;
  DestRect.height := FViewportSize.Y;

  SrcRect.x := 0;
  SrcRect.y := 0;
  SrcRect.width := FViewportSize.X shl 16;
  SrcRect.height := FViewportSize.Y shl 16;

  FillChar(Alpha, SizeOf(VC_DISPMANX_ALPHA_T), 0);
  Alpha.flags := DISPMANX_FLAGS_ALPHA_FIXED_ALL_PIXELS;
  Alpha.opacity := 255;

  FDispmanElement := vc_dispmanx_element_add(FDispmanUpdate, FDispmanDisplay, 0, @DestRect, 0, @SrcRect,
    DISPMANX_PROTECTION_NONE, @Alpha, nil, 0);
  if FDispmanElement = DISPMANX_NO_HANDLE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  FNativeWindow.element := FDispmanElement;
  FNativeWindow.width := FViewportSize.X;
  FNativeWindow.height := FViewportSize.Y;

  vc_dispmanx_update_submit_sync(FDispmanUpdate);

  FSurface := eglCreateWindowSurface(FDisplay, Config, EGLNativeWindowType(@FNativeWindow), nil);
  if FSurface = EGL_NO_SURFACE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  if eglMakeCurrent(FDisplay, FSurface, FSurface, FContext) <> EGL_TRUE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  Result := True;
end;

procedure TGLESDevice.FinalizeDisplay;
begin
  if FDispmanDisplay <> DISPMANX_NO_HANDLE then
  begin
    vc_dispmanx_display_close(FDispmanDisplay);
    FDispmanDisplay := DISPMANX_NO_HANDLE;
  end;

  if FDisplay <> EGL_NO_DISPLAY then
  begin
    eglMakeCurrent(FDisplay, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);

    if FContext <> EGL_NO_CONTEXT then
    begin
      eglDestroyContext(FDisplay, FContext);
      FContext := EGL_NO_CONTEXT;
    end;

    if FSurface <> EGL_NO_SURFACE then
    begin
      eglDestroySurface(FDisplay, FSurface);
      FSurface := EGL_NO_SURFACE;
    end;

    eglTerminate(FDisplay);
    FDisplay := EGL_NO_DISPLAY;
  end;
end;

function TGLESDevice.InitDevice: Boolean;
begin
  if not InitializeDisplay then
    Exit(False);

  Result := True;
end;

procedure TGLESDevice.DoneDevice;
begin
  FinalizeDisplay;
end;

function TGLESDevice.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor;
  const DepthValue: Single; const StencilValue: Cardinal): Boolean;
var
  Flags: Cardinal;
begin
  if ClearTypes = [] then
    Exit(False);

  Flags := 0;

  if TClearType.Color in ClearTypes then
  begin
    glClearColor(TIntColorRec(ColorValue).Red / 255.0, TIntColorRec(ColorValue).Green / 255.0,
      TIntColorRec(ColorValue).Blue / 255.0, TIntColorRec(ColorValue).Alpha / 255.0);
    Flags := Flags or GL_COLOR_BUFFER_BIT;
  end;

  if TClearType.Depth in ClearTypes then
  begin
    glClearDepthf(DepthValue);
    Flags := Flags or GL_DEPTH_BUFFER_BIT;
  end;

  if TClearType.Stencil in ClearTypes then
  begin
    glClearStencil(StencilValue);
    Flags := Flags or GL_STENCIL_BUFFER_BIT;
  end;

  glClear(Flags);

  Result := glGetError = GL_NO_ERROR;
end;

function TGLESDevice.Flip: Boolean;
begin
  eglSwapBuffers(FDisplay, FSurface);
  Result := glGetError = GL_NO_ERROR;
end;

end.
