unit PXL.Devices.FM.GL;
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
  PXL.Types, PXL.Devices, PXL.Types.GL;

type
  TFireGLDevice = class(TCustomDevice)
  private
    FContext: TGLDeviceContext;
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
  OpenGL;

constructor TFireGLDevice.Create(const AProvider: TCustomDeviceProvider);
begin
  inherited;

  FContext := TGLDeviceContext.Create(Self);

  FTechnology := TDeviceTechnology.OpenGL;

  FTechVersion := Cardinal(FContext.Extensions.MajorVersion) shl 8;

  FTechFeatureVersion :=
    (Cardinal(FContext.Extensions.MajorVersion) shl 8) or
    (Cardinal(FContext.Extensions.MinorVersion and $0F) shl 4);
end;

destructor TFireGLDevice.Destroy;
begin
  inherited;
  
  FContext.Free;
end;

function TFireGLDevice.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FContext;
end;

function TFireGLDevice.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
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

end.
