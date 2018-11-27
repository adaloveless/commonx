unit PXL.Textures.SRT;
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
  PXL.Types, PXL.Surfaces, PXL.Textures;

type
  TSRTLockableTexture = class(TCustomLockableTexture)
  private
    FSurface: TPixelSurface;
  protected
    function DoInitialize: Boolean; override;
    procedure DoFinalize; override;
  public
    function DoLock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean; override;
    function DoUnlock: Boolean; override;

    property Surface: TPixelSurface read FSurface;
  end;

implementation

uses
  SysUtils;

function TSRTLockableTexture.DoInitialize: Boolean;
begin
  if (Width < 1) or (Height < 1) then
    Exit(False);

  if FSurface = nil then
    FSurface := TPixelSurface.Create;

  FSurface.SetSize(Width, Height, FPixelFormat);

  FPixelFormat := FSurface.PixelFormat;
  FBytesPerPixel := FSurface.BytesPerPixel;

  FSurface.Clear(0);
  Result := True;
end;

procedure TSRTLockableTexture.DoFinalize;
begin
  FreeAndNil(FSurface);
end;

function TSRTLockableTexture.DoLock(const Rect: TIntRect; out LockedPixels: TLockedPixels): Boolean;
begin
  Result := LockSurface(FSurface, Rect, LockedPixels);
end;

function TSRTLockableTexture.DoUnlock: Boolean;
begin
  Result := True;
end;

end.
