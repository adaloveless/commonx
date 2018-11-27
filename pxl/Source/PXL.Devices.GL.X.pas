unit PXL.Devices.GL.X;
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
{$IFDEF LCLGTK2}
  gtk2,
{$ENDIF}

{$IFDEF LCLQT}
  qtwidgets,
{$ENDIF}

  ctypes, xlib, xutil, GLX, PXL.TypeDef, PXL.Types, PXL.Devices, PXL.SwapChains, PXL.Types.GL;

type
  TGLDevice = class(TCustomSwapChainDevice)
  public type
    TGLXVersion = record
      VerMajor: cint;
      VerMinor: cint;
    end;
  private type
    TAttributes = array of cint;
  private
    FDeviceContext: TGLDeviceContext;

    FDisplay: PDisplay;
    FContext: GLXContext;
    FDrawable: GLXDrawable;
    FGLXVersion: TGLXVersion;

    FViewportSize: TPoint2px;

  {$IFDEF LCLGTK2}
    FWidget: PGtkWidget;
  {$ENDIF}

  {$IFDEF LCLQT}
    FWidget: TQtWidget;
  {$ENDIF}

    function CreateDrawable(const Handle: TUntypedHandle): Boolean;
    procedure DestroyDrawable;

    function UpdateGLXVersion: Boolean;
    function ChooseVisualTypical(const DepthStencil: TDepthStencil): PXVisualInfo;
    function ChooseVisualExtended(const DepthStencil: TDepthStencil; const Multisamples: Integer): PXVisualInfo;

    function InitializeDisplay: Boolean;
    procedure FinalizeDisplay;

    class procedure AddAttributes(var CurrentAttributes: TAttributes; const NewAttributes: array of cint);
  protected
    function GetDeviceContext: TCustomDeviceContext; override;

    function InitDevice: Boolean; override;
    procedure DoneDevice; override;

    function ResizeSwapChain(const SwapChainIndex: Integer; const NewSwapChainInfo: PSwapChainInfo): Boolean; override;
  public
    constructor Create(const AProvider: TCustomDeviceProvider);
    destructor Destroy; override;

    function Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single = 1.0;
      const StencilValue: Cardinal = 0): Boolean; override;

    function BeginScene(const SwapChainIndex: Integer = 0): Boolean; override;
    function EndScene: Boolean; override;

    property Display: PDisplay read FDisplay;
    property Context: GLXContext read FContext;
    property Drawable: GLXDrawable read FDrawable;

    property GLXVersion: TGLXVersion read FGLXVersion;
  end;

implementation

uses
{$IFDEF LCLGTK2}
  gdk2x, gtk2proc,
{$ENDIF}

{$IFDEF LCLQT}
  qt4,
{$ENDIF}

  x, gl, SysUtils;

class procedure TGLDevice.AddAttributes(var CurrentAttributes: TAttributes; const NewAttributes: array of cint);
var
  CurrentCount, I: Integer;
begin
  if (Length(CurrentAttributes) > 0) and (CurrentAttributes[Length(CurrentAttributes) - 1] = None) then
    CurrentCount := Length(CurrentAttributes) - 1
  else
    CurrentCount := Length(CurrentAttributes);

  SetLength(CurrentAttributes, CurrentCount + Length(NewAttributes) + 1);

  for I := 0 to Length(NewAttributes) - 1 do
    CurrentAttributes[CurrentCount + I] := NewAttributes[I];

  CurrentAttributes[Length(CurrentAttributes) - 1] := None;
end;

constructor TGLDevice.Create(const AProvider: TCustomDeviceProvider);
begin
  inherited;

  FDeviceContext := TGLDeviceContext.Create(Self);
  FTechnology := TDeviceTechnology.OpenGL;
end;

destructor TGLDevice.Destroy;
begin
  inherited;
  
  FDeviceContext.Free;
end;

function TGLDevice.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FDeviceContext;
end;

function TGLDevice.CreateDrawable(const Handle: TUntypedHandle): Boolean;
begin
{$IFDEF LCLGTK2}
  FDisplay := GDK_DISPLAY;

  FWidget := GetFixedWidget(PGtkWidget(Handle));
  if FWidget = nil then
    Exit(False);

  gtk_widget_realize(FWidget);

  FDrawable := GDK_WINDOW_XID(FWidget.window);
  if FDrawable = 0 then
  begin
    gtk_widget_unrealize(FWidget);
    FWidget := nil;
  end;
{$ENDIF}
{$IFDEF LCLQT}
  FWidget := TQtWidget(Pointer(Handle));
  if FWidget = nil then
    Exit(False);

  FWidget.setAttribute(QtWA_NoSystemBackground);

  FDisplay := QX11Info_display;

  FDrawable := GLXDrawable(QWidget_winID(FWidget.Widget));
  if FDrawable = 0 then
    Exit(False);
{$ENDIF}

  Result := True;
end;

procedure TGLDevice.DestroyDrawable;
begin
{$IFDEF LCLGTK2}
  if FWidget <> nil then
  begin
    gtk_widget_unrealize(FWidget);
    FWidget := nil;
  end;
{$ENDIF}

{$IFDEF LCLQT}
  FDrawable := 0;
  FWidget := nil;
{$ENDIF}

  FDisplay := nil;
end;

function TGLDevice.UpdateGLXVersion: Boolean;
begin
  Result := glXQueryVersion(FDisplay, FGLXVersion.VerMajor, FGLXVersion.VerMinor);
end;

function TGLDevice.ChooseVisualTypical(const DepthStencil: TDepthStencil): PXVisualInfo;
var
  Attributes: TAttributes = nil;
begin
  AddAttributes(Attributes, [GLX_DOUBLEBUFFER, GLX_RGBA]);

  if DepthStencil >= TDepthStencil.DepthOnly then
    AddAttributes(Attributes, [GLX_DEPTH_SIZE, 24]);

  if DepthStencil >= TDepthStencil.Full then
    AddAttributes(Attributes, [GLX_STENCIL_SIZE, 8]);

  Result := glXChooseVisual(FDisplay, DefaultScreen(FDisplay), @Attributes[0]);
end;

function TGLDevice.ChooseVisualExtended(const DepthStencil: TDepthStencil; const Multisamples: Integer): PXVisualInfo;
var
  Attributes: TAttributes = nil;
  Configs: PGLXFBConfig;
  ConfigCount: cint;
begin
  AddAttributes(Attributes, [GLX_X_RENDERABLE, GL_TRUE, GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT, GLX_RENDER_TYPE,
    GLX_RGBA_BIT, GLX_X_VISUAL_TYPE, GLX_TRUE_COLOR, GLX_RED_SIZE, 8, GLX_GREEN_SIZE, 8, GLX_BLUE_SIZE, 8,
    GLX_DOUBLEBUFFER, GL_TRUE]);

  if DepthStencil >= TDepthStencil.DepthOnly then
    AddAttributes(Attributes, [GLX_DEPTH_SIZE, 24]);

  if DepthStencil >= TDepthStencil.Full then
    AddAttributes(Attributes, [GLX_STENCIL_SIZE, 8]);

  if Multisamples > 0 then
    if GLX_version_1_4(FDisplay) then
      AddAttributes(Attributes, [GLX_SAMPLE_BUFFERS, 1, GLX_SAMPLES, Multisamples])
    else if GLX_ARB_multisample(FDisplay, DefaultScreen(FDisplay)) then
      AddAttributes(Attributes, [GLX_SAMPLE_BUFFERS_ARB, 1, GLX_SAMPLES_ARB, Multisamples]);

  Configs := glXChooseFBConfig(FDisplay, DefaultScreen(FDisplay), @Attributes[0], ConfigCount);
  if (ConfigCount <= 0) or (Configs = nil) then
    Exit(nil);
  try
    Result := glXGetVisualFromFBConfig(FDisplay, Configs^);
  finally
    XFree(Configs);
  end;
end;

function TGLDevice.InitializeDisplay: Boolean;
var
  SwapChainInfo: PSwapChainInfo;
  VisualInfo: PXVisualInfo = nil;
  SwapInterval: Integer;
begin
  SwapChainInfo := SwapChains[0];
  if SwapChainInfo = nil then
    Exit(False);

  if not CreateDrawable(SwapChainInfo.WindowHandle) then
    Exit(False);

  if not UpdateGLXVersion then
    Exit(False);

  if GLX_version_1_3(FDisplay) then
    VisualInfo := ChooseVisualExtended(SwapChainInfo.DepthStencil, SwapChainInfo.Multisamples);

  if VisualInfo = nil then
  begin
    VisualInfo := ChooseVisualTypical(SwapChainInfo.DepthStencil);
    if VisualInfo = nil then
      Exit(False);
  end;

  try
    FContext := glXCreateContext(FDisplay, VisualInfo, nil, True);
    if FContext = nil then
      Exit(False);
  finally
    XFree(VisualInfo);
  end;

  if not glXMakeCurrent(FDisplay, FDrawable, FContext) then
  begin
    glXDestroyContext(FDisplay, FContext);
    FContext := nil;
    Exit(False);
  end;

  if not FDeviceContext.LoadLibrary then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  if SwapChainInfo.VSync then
    SwapInterval := 1
  else
    SwapInterval := 0;

  if GLX_SGI_swap_control(FDisplay, DefaultScreen(FDisplay)) then
    glXSwapIntervalSGI(SwapInterval);

  if GLX_EXT_swap_control(FDisplay, DefaultScreen(FDisplay)) then
    glXSwapIntervalEXT(FDisplay, FDrawable, SwapInterval);

  if GLX_MESA_swap_control(FDisplay, DefaultScreen(FDisplay)) then
    glXSwapIntervalMESA(SwapInterval);

  FViewportSize := Point2px(SwapChainInfo.Width, SwapChainInfo.Height);

  FTechVersion := Cardinal(FDeviceContext.Extensions.MajorVersion) shl 8;

  FTechFeatureVersion :=
    (Cardinal(FDeviceContext.Extensions.MajorVersion) shl 8) or
    (Cardinal(FDeviceContext.Extensions.MinorVersion and $0F) shl 4);

  glShadeModel(GL_SMOOTH);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  Result := True;
end;

procedure TGLDevice.FinalizeDisplay;
begin
  glXMakeCurrent(FDisplay, None, nil);

  if FContext <> nil then
  begin
    glXDestroyContext(FDisplay, FContext);
    FContext := nil;
  end;

  DestroyDrawable;
end;

function TGLDevice.InitDevice: Boolean;
begin
  if not InitializeDisplay then
    Exit(False);

  Result := True;
end;

procedure TGLDevice.DoneDevice;
begin
  FinalizeDisplay;
end;

function TGLDevice.ResizeSwapChain(const SwapChainIndex: Integer; const NewSwapChainInfo: PSwapChainInfo): Boolean;
begin
  if SwapChainIndex <> 0 then
    Exit(False);

  FViewportSize := Point2px(NewSwapChainInfo.Width, NewSwapChainInfo.Height);
  Result := True;
end;

function TGLDevice.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor;
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
    glClearDepth(DepthValue);
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

function TGLDevice.BeginScene(const SwapChainIndex: Integer): Boolean;
begin
  if SwapChainIndex <> 0 then
    Exit(False);

  glViewport(0, 0, FViewportSize.X, FViewportSize.Y);
  glDisable(GL_SCISSOR_TEST);

  Result := True;
end;

function TGLDevice.EndScene: Boolean;
begin
  // If double-buffering is not used, "glFlush" must be called here before glXSwapBuffers.
  glXSwapBuffers(FDisplay, FDrawable);
  Result := glGetError = GL_NO_ERROR;
end;

end.
