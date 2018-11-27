unit PXL.Devices.FM.GLES;
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
  System.Messaging, PXL.Types, PXL.Devices, PXL.Types.GLES;

type
  TFireGLESDevice = class(TCustomDevice)
  private
    FContext: TGLESDeviceContext;

    ContextResetId: Integer;
    ContextLostId: Integer;

    procedure ContextResetHandler(const Sender: TObject; const Msg: TMessage);
    procedure ContextLostHandler(const Sender: TObject; const Msg: TMessage);
  protected
    function GetDeviceContext: TCustomDeviceContext; override;
  public
    constructor Create(const AProvider: TCustomDeviceProvider);
    destructor Destroy; override;

    function Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single = 1.0;
      const StencilValue: Cardinal = 0): Boolean; override;
  end;

implementation

uses
{$IFDEF ANDROID}
  Androidapi.Gles2,
{$ENDIF}

{$IFDEF IOS}
  iOSapi.OpenGLES,
{$ENDIF}

  FMX.Types3D;

constructor TFireGLESDevice.Create;
begin
  inherited;

  FContext := TGLESDeviceContext.Create(Self);

  FTechnology := TDeviceTechnology.OpenGL_ES;
  FTechVersion := $200;

  ContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, ContextResetHandler);
  ContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, ContextLostHandler);
end;

destructor TFireGLESDevice.Destroy;
begin
  inherited;
  
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, ContextLostId);
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, ContextResetId);

  FContext.Free;
end;

function TFireGLESDevice.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FContext;
end;

procedure TFireGLESDevice.ContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  OnRestore.Notify(Self);
end;

procedure TFireGLESDevice.ContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  OnRelease.Notify(Self);
end;

function TFireGLESDevice.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
  const StencilValue: Cardinal): Boolean;
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

end.
