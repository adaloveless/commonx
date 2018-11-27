unit PXL.Providers.GL;
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

{ Remove the dot to enable Legacy Canvas that uses OpenGL 1.x for rendering.
  By default, newer canvas uses OpenGL 2.x for rendering. }
{.$DEFINE UseLagacy1xCanvas}

uses
  PXL.Devices, PXL.Textures, PXL.Canvas, PXL.Providers;

type
  TGLProvider = class(TGraphicsDeviceProvider)
  public
    function CreateDevice: TCustomDevice; override;
    function CreateCanvas(const Device: TCustomDevice): TCustomCanvas; override;
    function CreateLockableTexture(const Device: TCustomDevice;
      const AutoSubscribe: Boolean): TCustomLockableTexture; override;
    function CreateDrawableTexture(const Device: TCustomDevice;
      const AutoSubscribe: Boolean): TCustomDrawableTexture; override;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  PXL.Devices.GL.Win,
{$ELSE}
  {$IFDEF DARWIN}
    {$IFDEF LCLCARBON}
      PXL.Devices.GL.Carbon,
    {$ELSE}
      PXL.Devices.GL.Cocoa,
    {$ENDIF}
  {$ELSE}
    PXL.Devices.GL.X,
  {$ENDIF}
{$ENDIF}

{$IFDEF UseLagacy1xCanvas}
  PXL.Canvas.GL.GL1,
{$ELSE}
  PXL.Canvas.GL,
{$ENDIF}

  PXL.Textures.GL;

function TGLProvider.CreateDevice: TCustomDevice;
begin
  Result := TGLDevice.Create(Self);
end;

function TGLProvider.CreateCanvas(const Device: TCustomDevice): TCustomCanvas;
begin
  Result := TGLCanvas.Create(Device);
end;

function TGLProvider.CreateLockableTexture(const Device: TCustomDevice;
  const AutoSubscribe: Boolean): TCustomLockableTexture;
begin
  Result := TGLLockableTexture.Create(Device, AutoSubscribe);
end;

function TGLProvider.CreateDrawableTexture(const Device: TCustomDevice;
  const AutoSubscribe: Boolean): TCustomDrawableTexture;
begin
  Result := TGLDrawableTexture.Create(Device, AutoSubscribe);
end;

end.
