unit PXL.ImageFormats.Auto;
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
{< Automatic cross-platform image format handler instance creation. }
interface

{$INCLUDE PXL.Config.inc}

uses
  PXL.ImageFormats;

{ Enable the following option to use Vampyre Imaging Library on non-Windows platforms instead of LCL. This provides
  superior performance but might not work on all platforms. }
{.$DEFINE USE_VAMPYRE_IMAGING}

{$IFDEF DELPHI}
  {$DEFINE USE_VAMPYRE_IMAGING}
{$ENDIF}

{ Creates image format handler by default choosen depending on platform, OS and available support. }
function CreateDefaultImageFormatHandler(const ImageFormatManager: TImageFormatManager): TCustomImageFormatHandler;

implementation

uses
{$IFDEF MSWINDOWS}
  PXL.ImageFormats.WIC
{$ELSE}
  {$IFDEF USE_VAMPYRE_IMAGING}
    PXL.ImageFormats.Vampyre
  {$ELSE}
    PXL.ImageFormats.FCL
  {$ENDIF}
{$ENDIF};

function CreateDefaultImageFormatHandler(const ImageFormatManager: TImageFormatManager): TCustomImageFormatHandler;
begin
{$IFDEF MSWINDOWS}
  Result := TWICImageFormatHandler.Create(ImageFormatManager);
{$ELSE}
  {$IFDEF USE_VAMPYRE_IMAGING}
    Result := TWICImageFormatHandler.Create(ImageFormatManager);
  {$ELSE}
    Result := TFCLImageFormatHandler.Create(ImageFormatManager);
  {$ENDIF}
{$ENDIF}
end;

end.
