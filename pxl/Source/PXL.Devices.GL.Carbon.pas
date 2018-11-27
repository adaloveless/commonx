unit PXL.Devices.GL.Carbon;
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
  gl, AGL, PXL.TypeDef, PXL.Types, PXL.Devices, PXL.SwapChains, PXL.Types.GL;

type
  TGLDevice = class(TCustomSwapChainDevice)
  private type
    TAttributes = array of GLint;
  private
    FDeviceContext: TGLDeviceContext;
    FViewportSize: TPoint2px;

    FContext: TAGLContext;

    class procedure AddAttributes(var CurrentAttributes: TAttributes; const NewAttributes: array of GLint);
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

    property Context: TAGLContext read FContext;
  end;

implementation

uses
  MacOSAll, CarbonPrivate, SysUtils;

class procedure TGLDevice.AddAttributes(var CurrentAttributes: TAttributes; const NewAttributes: array of GLint);
var
  CurrentCount, I: Integer;
begin
  if (Length(CurrentAttributes) > 0) and (CurrentAttributes[Length(CurrentAttributes) - 1] = AGL_NONE) then
    CurrentCount := Length(CurrentAttributes) - 1
  else
    CurrentCount := Length(CurrentAttributes);

  SetLength(CurrentAttributes, CurrentCount + Length(NewAttributes) + 1);

  for I := 0 to Length(NewAttributes) - 1 do
    CurrentAttributes[CurrentCount + I] := NewAttributes[I];

  CurrentAttributes[Length(CurrentAttributes) - 1] := AGL_NONE;
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

function TGLDevice.InitDevice: Boolean;
var
  SwapChainInfo: PSwapChainInfo;
  Attributes: TAttributes = nil;
  MultisampleIndex, I: Integer;
  PixelFormat: TAGLPixelFormat;
  Device: GDHandle;
  SwapInterval: GLint;
begin
  SwapChainInfo := SwapChains[0];
  if SwapChainInfo = nil then
    Exit(False);

  AddAttributes(Attributes, [AGL_WINDOW, AGL_DOUBLEBUFFER, AGL_RGBA]);

  if SwapChainInfo.DepthStencil >= TDepthStencil.DepthOnly then
    AddAttributes(Attributes, [AGL_DEPTH_SIZE, 24]);

  if SwapChainInfo.DepthStencil >= TDepthStencil.Full then
    AddAttributes(Attributes, [AGL_STENCIL_SIZE, 8]);

  if SwapChainInfo.Multisamples > 0 then
  begin
    AddAttributes(Attributes, [AGL_SAMPLE_BUFFERS_ARB, 1, AGL_MULTISAMPLE, AGL_SAMPLES_ARB, SwapChainInfo.Multisamples]);
    MultisampleIndex := Length(Attributes) - 2;
  end
  else
    MultisampleIndex := -1;

  Device := GetMainDevice;

  while True do
  begin
    PixelFormat := aglChoosePixelFormat(@Device, 1, @Attributes[0]);
    if (PixelFormat = nil) and (MultisampleIndex <> -1) then
    begin
      if Attributes[MultisampleIndex] <= 0 then
        Exit(False);

      Dec(Attributes[MultisampleIndex]);
      Continue;
    end;

    if PixelFormat = nil then
      Exit(False)
    else
      Break;
  end;

  try
    FContext := aglCreateContext(PixelFormat, nil);
    if FContext = nil then
      Exit(False);
  finally
    aglDestroyPixelFormat(PixelFormat);
  end;

  aglSetDrawable(FContext, GetWindowPort(TCarbonWindow(SwapChainInfo.WindowHandle).Window));

  if (aglSetCurrentContext(FContext) = 0) or (not FDeviceContext.LoadLibrary) then
  begin
    aglDestroyContext(FContext);
    Exit(False);
  end;

  if SwapChainInfo.VSync then
    SwapInterval := 1
  else
    SwapInterval := 0;

  aglSetInteger(FContext, AGL_SWAP_INTERVAL,@SwapInterval);

  FViewportSize := Point2px(SwapChainInfo.Width, SwapChainInfo.Height);

  glShadeModel(GL_SMOOTH);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  Result := True;
end;

procedure TGLDevice.DoneDevice;
begin
  if FContext <> nil then
  begin
    aglDestroyContext(FContext);
    FContext := nil;
  end;
end;

function TGLDevice.ResizeSwapChain(const SwapChainIndex: Integer; const NewSwapChainInfo: PSwapChainInfo): Boolean;
var
  Window: TCarbonWindow;
  WinRef: WindowRef;
  RegRect: MacOSAll.Rect;
  ViewBounds: HIRect;
  Values: array[0..3] of GLint;
begin
  if SwapChainIndex <> 0 then
    Exit(False);

  Window := TCarbonWindow(NewSwapChainInfo.WindowHandle);
  WinRef := HIViewGetWindow(Window.Widget);
  if not Assigned(WinRef) then
    Exit(False);

  GetWindowBounds(WinRef, kWindowStructureRgn, RegRect);

  HIViewGetBounds(Window.Widget, ViewBounds);
  HIViewConvertPoint(ViewBounds.origin, Window.Widget, nil);

  Values[0] := Round(ViewBounds.origin.x);
  Values[1] := Round((RegRect.bottom - RegRect.top) - ViewBounds.origin.y - ViewBounds.size.height);
  Values[2] := Round(ViewBounds.size.width);
  Values[3] := Round(ViewBounds.size.height);

  aglEnable(Context, AGL_BUFFER_RECT);
  aglSetInteger(Context, AGL_BUFFER_RECT, @Values[0]);

  if SwapChainIndex = 0 then
    FViewportSize := Point2px(NewSwapChainInfo.Width, NewSwapChainInfo.Height);

  Result := glGetError = GL_NO_ERROR;
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
  aglSwapBuffers(FContext);
  Result := glGetError = GL_NO_ERROR;
end;

end.
