unit PXL.Providers;
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
{< Device-bound factory implementation that creates secondary components such as canvas and textures. }
interface

{$INCLUDE PXL.Config.inc}

uses
  PXL.Devices, PXL.Textures, PXL.Canvas;

type
  { Abstract device provider that is able to create new instances of important rendering classes such as canvas and
    textures. }
  TGraphicsDeviceProvider = class abstract(TCustomDeviceProvider)
  public
    { This function creates new canvas instance that is tied to the given device. }
    function CreateCanvas(const Device: TCustomDevice): TCustomCanvas; virtual;

    { This function creates new lockable texture instance that is tied to the given device. }
    function CreateLockableTexture(const Device: TCustomDevice;
      const AutoSubscribe: Boolean = True): TCustomLockableTexture; virtual;

    { This function creates new drawable texture instance that is tied to the given device.
      If drawable textures are not supported in this provider, @nil is returned. }
    function CreateDrawableTexture(const Device: TCustomDevice;
      const AutoSubscribe: Boolean = True): TCustomDrawableTexture; virtual;
  end;

implementation

function TGraphicsDeviceProvider.CreateCanvas(const Device: TCustomDevice): TCustomCanvas;
begin
  Result := nil;
end;

function TGraphicsDeviceProvider.CreateLockableTexture(const Device: TCustomDevice;
  const AutoSubscribe: Boolean): TCustomLockableTexture;
begin
  Result := nil;
end;

function TGraphicsDeviceProvider.CreateDrawableTexture(const Device: TCustomDevice;
  const AutoSubscribe: Boolean): TCustomDrawableTexture;
begin
  Result := nil;
end;

end.
