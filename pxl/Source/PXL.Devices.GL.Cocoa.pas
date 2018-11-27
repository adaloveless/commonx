unit PXL.Devices.GL.Cocoa;
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

{$MODESWITCH ObjectiveC1}
{$LINKFRAMEWORK OpenGL}

uses
  CocoaAll, gl, PXL.TypeDef, PXL.Types, PXL.Devices, PXL.SwapChains, PXL.Types.GL;

type
  TGLDevice = class(TCustomSwapChainDevice)
  private type
    TAttributes = array of GLint;
  private
    FDeviceContext: TGLDeviceContext;
    FViewportSize: TPoint2px;

    FView: NSOpenGLView;

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
  end;

implementation

uses
  SysUtils, CocoaPrivate, CocoaUtils, MacOSAll;

class procedure TGLDevice.AddAttributes(var CurrentAttributes: TAttributes; const NewAttributes: array of GLint);
var
  CurrentCount, I: Integer;
begin
  if (Length(CurrentAttributes) > 0) and (CurrentAttributes[Length(CurrentAttributes) - 1] = 0) then
    CurrentCount := Length(CurrentAttributes) - 1
  else
    CurrentCount := Length(CurrentAttributes);

  SetLength(CurrentAttributes, CurrentCount + Length(NewAttributes) + 1);

  for I := 0 to Length(NewAttributes) - 1 do
    CurrentAttributes[CurrentCount + I] := NewAttributes[I];

  CurrentAttributes[Length(CurrentAttributes) - 1] := 0;
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
  ViewRect: NSRect;
  ParentView: NSView;
  Context: CGLContextObj;
  PixelFormat: NSOpenGLPixelFormat;
  MultisampleIndex, I: Integer;
  SwapInterval: GLint;
begin
  SwapChainInfo := SwapChains[0];
  if SwapChainInfo = nil then
    Exit(False);

  AddAttributes(Attributes, [NSOpenGLPFAAccelerated, NSOpenGLPFADoubleBuffer]);

  if SwapChainInfo.DepthStencil >= TDepthStencil.DepthOnly then
    AddAttributes(Attributes, [NSOpenGLPFADepthSize, 24]);

  if SwapChainInfo.DepthStencil >= TDepthStencil.Full then
    AddAttributes(Attributes, [NSOpenGLPFAStencilSize, 8]);

  if SwapChainInfo.Multisamples > 0 then
    AddAttributes(Attributes, [NSOpenGLPFASampleBuffers, 1, NSOpenGLPFAMultisample, NSOpenGLPFASamples,
      SwapChainInfo.Multisamples]);

  PixelFormat := NSOpenGLPixelFormat(NSOpenGLPixelFormat.alloc).initWithAttributes(@Attributes[0]);

  ViewRect := GetNSRect(0, 0, SwapChainInfo.Width, SwapChainInfo.Height);

  FView := NSOpenGLView(NSOpenGLView.alloc).initWithFrame_pixelFormat(ViewRect, PixelFormat);
  if FView = nil then
    Exit(False);

  ParentView := CocoaUtils.GetNSObjectView(NSObject(SwapChainInfo.WindowHandle));
  ParentView.addSubview(FView);

  SetViewDefaults(FView);

  Context := CGLContextObj(FView.openGLContext.CGLContextObj);
  if CGLSetCurrentContext(Context) <> kCGLNoError then
    Exit(False);

  if not FDeviceContext.LoadLibrary then
    Exit(False);

  if SwapChainInfo.VSync then
    SwapInterval := 1
  else
    SwapInterval := 0;

  CGLSetParameter(Context, kCGLCPSwapInterval, @SwapInterval);

  FTechVersion := Cardinal(FDeviceContext.Extensions.MajorVersion) shl 8;

  FTechFeatureVersion :=
    (Cardinal(FDeviceContext.Extensions.MajorVersion) shl 8) or
    (Cardinal(FDeviceContext.Extensions.MinorVersion and $0F) shl 4);

  FViewportSize := Point2px(SwapChainInfo.Width, SwapChainInfo.Height);

  glShadeModel(GL_SMOOTH);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  Result := True;
end;

procedure TGLDevice.DoneDevice;
begin
  CGLSetCurrentContext(nil);
end;

function TGLDevice.ResizeSwapChain(const SwapChainIndex: Integer; const NewSwapChainInfo: PSwapChainInfo): Boolean;
var
  ViewRect: NSRect;
begin
  if SwapChainIndex <> 0 then
    Exit(False);

  ViewRect := GetNSRect(0, 0, NewSwapChainInfo.Width, NewSwapChainInfo.Height);

  if FView <> nil then
    FView.setFrame(ViewRect);

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
  if FView <> nil then
    FView.openGLContext.flushBuffer;
  Result := glGetError = GL_NO_ERROR;
end;

end.
