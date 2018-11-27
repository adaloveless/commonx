unit Engine.Globals;
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

{ Special note: this code was ported multiple times from earliest framework releases predating Asphyre. }

uses
  PXL.Types, PXL.SwapChains, PXL.Canvas, PXL.Images, PXL.Fonts, PXL.Archives;

var
  DisplaySize: TPoint2px;

  EngineDevice: TCustomSwapChainDevice = nil;
  EngineCanvas: TCustomCanvas = nil;
  EngineImages: TAtlasImages = nil;
  EngineFonts: TBitmapFonts = nil;
  EngineArchive: TArchive = nil;

  ImageBackground: Integer = -1;
  ImageShipArmor: Integer = -1;
  ImageCShineLogo: Integer = -1;
  ImageBandLogo: Integer = -1;
  ImageLogo: Integer = -1;
  ImageShip: Integer = -1;
  ImageRock: Integer = -1;
  ImageTorpedo: Integer = -1;
  ImageExplode: Integer = -1;
  ImageCombust: Integer = -1;

  FontArialBlack: Integer = -1;
  FontTimesRoman: Integer = -1;
  FontImpact: Integer = -1;

implementation

end.
