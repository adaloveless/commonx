unit PXL.Providers.GLES;
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
  PXL.Devices, PXL.Textures, PXL.Canvas, PXL.Providers;

type
  TGLESProvider = class(TGraphicsDeviceProvider)
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
{$IFDEF SINGLEBOARD}
  PXL.Devices.GLES.RPi,
{$ELSE}
  {$IFNDEF ANDROID}
    PXL.Devices.GLES.X,
  {$ENDIF}
{$ENDIF}

  PXL.Textures.GLES, PXL.Canvas.GLES;

function TGLESProvider.CreateDevice: TCustomDevice;
begin
{$IFNDEF ANDROID}
  Result := TGLESDevice.Create(Self);
{$ELSE}
  Result := nil;
{$ENDIF}
end;

function TGLESProvider.CreateCanvas(const Device: TCustomDevice): TCustomCanvas;
begin
  Result := TGLESCanvas.Create(Device);
end;

function TGLESProvider.CreateLockableTexture(const Device: TCustomDevice;
  const AutoSubscribe: Boolean): TCustomLockableTexture;
begin
  Result := TGLESLockableTexture.Create(Device, AutoSubscribe);
end;

function TGLESProvider.CreateDrawableTexture(const Device: TCustomDevice;
  const AutoSubscribe: Boolean): TCustomDrawableTexture;
begin
  Result := TGLESDrawableTexture.Create(Device, AutoSubscribe);
end;

end.
