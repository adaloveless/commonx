unit PXL.Consts;
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
  PXL.TypeDef;

resourcestring
  SAssetManagerNotSpecified = 'Asset Manager has not been specified.';
  SCannotRestoreTexture = 'Cannot restore texture to working status on device recovery.';
  SCannotRestoreTextures = 'Cannot restore textures to working status on device recovery.';
  SCannotRestoreCanvas = 'Cannot restore canvas to working status on device recovery.';
  SCannotRestoreImages = 'Cannot restore images to working status on device recovery.';
  SCanvasGeometryTooBig = 'Geometry passed to canvas is too big to be rendered.';
  SCouldNotActivateShaderEffect = 'Shader effect could not be activated.';

implementation

end.
