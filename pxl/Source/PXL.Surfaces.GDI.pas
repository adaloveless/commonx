unit PXL.Surfaces.GDI;
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
  Windows, PXL.TypeDef, PXL.Types, PXL.Surfaces;

type
{$IFDEF FPC}{$PACKRECORDS C}{$ENDIF}
  TGDIBitmapInfo = record
    bmiHeader: BITMAPINFOHEADER;
    bmiColors: array[0..3] of DWORD;
  end;
{$IFDEF FPC}{$PACKRECORDS DEFAULT}{$ENDIF}

  TGDIPixelSurface = class(TPixelSurface)
  private
    FBitmapInfo: TGDIBitmapInfo;
    FBitmap: HBITMAP;
    FHandle: HDC;

    function CreateHandle: Boolean;
    procedure DestroyHandle;

    function CalculatePitch(const Width, BitCount, Alignment: Integer): Integer;

    function CreateBitmap(const NewWidth, NewHeight: Integer; const NewPixelFormat: TPixelFormat): Boolean;
    procedure DestroyBitmap;
  protected
    procedure ResetAllocation; override;
    function Reallocate(const NewWidth, NewHeight: Integer; const NewPixelFormat: TPixelFormat): Boolean; override;
  public
    function ApproximatePixelFormat(const NewPixelFormat: TPixelFormat): TPixelFormat; override;

    procedure BitBlt(const DestHandle: HDC; const DestAt, Size: TPoint2px; const SrcAt: TPoint2px); overload;
    procedure BitBlt(const DestSurface: TGDIPixelSurface; const DestAt, Size: TPoint2px;
      const SrcAt: TPoint2px); overload; inline;

    property BitmapInfo: TGDIBitmapInfo read FBitmapInfo;
    property Bitmap: HBITMAP read FBitmap;
    property Handle: HDC read FHandle;
  end;

  TGDIPixelMipMapSurface = class(TPixelMipMapSurface)
  protected
    function CreatePixelSurfaces: TPixelSurfaces; override;
  end;

  TGDIPixelSurfaces = class(TPixelSurfaces)
  protected
    function CreatePixelSurface(const SurfaceName: StdString): TPixelSurface; override;
  end;

implementation

uses
  PXL.Formats;

function TGDIPixelSurface.CreateHandle: Boolean;
begin
  if FHandle = 0 then
  begin
    FHandle := CreateCompatibleDC(0);
    if FHandle <> 0 then
      SetMapMode(FHandle, MM_TEXT);
  end;

  Result := FHandle <> 0;
end;

procedure TGDIPixelSurface.DestroyHandle;
begin
  if FHandle <> 0 then
  begin
    DeleteDC(FHandle);
    FHandle := 0;
  end;
end;

function TGDIPixelSurface.CalculatePitch(const Width, BitCount, Alignment: Integer): Integer;
begin
  Result := (((Width * BitCount) + (Alignment - 1)) and (not (Alignment - 1))) div 8;
end;

function TGDIPixelSurface.CreateBitmap(const NewWidth, NewHeight: Integer; const NewPixelFormat: TPixelFormat): Boolean;
begin
  if FBitmap <> 0 then
    DeleteObject(FBitmap);

  FPixelFormat := ApproximatePixelFormat(NewPixelFormat);
  FBytesPerPixel := FPixelFormat.Bytes;

  FillChar(FBitmapInfo, SizeOf(TGDIBitmapInfo), 0);

  FBitmapInfo.bmiHeader.biSize := SizeOf(BITMAPINFOHEADER);
  FBitmapInfo.bmiHeader.biWidth := NewWidth;
  FBitmapInfo.bmiHeader.biHeight := -NewHeight;
  FBitmapInfo.bmiHeader.biPlanes := 1;
  FBitmapInfo.bmiHeader.biBitCount := FPixelFormat.Bits;

  case FPixelFormat of
    TPixelFormat.R5G6B5:
      begin
        FBitmapInfo.bmiHeader.biCompression := BI_BITFIELDS;
        FBitmapInfo.bmiColors[0] := $0000F800;
        FBitmapInfo.bmiColors[1] := $000007E0;
        FBitmapInfo.bmiColors[2] := $0000001F;
      end;

    TPixelFormat.A8B8G8R8:
      begin
        FBitmapInfo.bmiHeader.biCompression := BI_BITFIELDS;
        FBitmapInfo.bmiColors[0] := $000000FF;
        FBitmapInfo.bmiColors[1] := $0000FF00;
        FBitmapInfo.bmiColors[2] := $00FF0000;
      end;

  else
    FBitmapInfo.bmiHeader.biCompression := BI_RGB;
  end;

  FBitmap := CreateDIBSection(FHandle, PBitmapInfo(@FBitmapInfo)^, DIB_RGB_COLORS, FBits, 0, 0);
  if FBitmap = 0 then
  begin
    FPixelFormat := TPixelFormat.Unknown;
    FBytesPerPixel := 0;
    Exit(False);
  end;

  FWidth := NewWidth;
  FHeight := NewHeight;
  FPitch := CalculatePitch(FWidth, FPixelFormat.Bits, 32);
  FBufferSize := FHeight * FPitch;

  SelectObject(FHandle, FBitmap);
  Result := True;
end;

procedure TGDIPixelSurface.DestroyBitmap;
begin
  if FBitmap <> 0 then
  begin
    DeleteObject(FBitmap);
    FBitmap := 0;
  end;
end;

procedure TGDIPixelSurface.ResetAllocation;
begin
  DestroyBitmap;
  DestroyHandle;

  FBits := nil;
  FPitch := 0;
  FWidth := 0;
  FHeight := 0;
  FBufferSize := 0;
  FBytesPerPixel := 0;
  FPixelFormat := TPixelFormat.Unknown;
end;

function TGDIPixelSurface.Reallocate(const NewWidth, NewHeight: Integer; const NewPixelFormat: TPixelFormat): Boolean;
begin
  if not CreateHandle then
    Exit(False);

  Result := CreateBitmap(NewWidth, NewHeight, NewPixelFormat);
end;

function TGDIPixelSurface.ApproximatePixelFormat(const NewPixelFormat: TPixelFormat): TPixelFormat;
const
  DefaultPixelFormat = TPixelFormat.A8R8G8B8;
var
  FormatList: TPixelFormatList;
begin
  if NewPixelFormat = TPixelFormat.Unknown then
    Exit(DefaultPixelFormat);

  FormatList := TPixelFormatList.Create;
  try
    FormatList.Insert(TPixelFormat.A8R8G8B8);
    FormatList.Insert(TPixelFormat.R8G8B8);
    FormatList.Insert(TPixelFormat.X1R5G5B5);
    FormatList.Insert(TPixelFormat.R5G6B5);
    FormatList.Insert(TPixelFormat.A8B8G8R8);

    Result := FindClosestPixelFormat(NewPIxelFormat, FormatList);

    if Result = TPixelFormat.Unknown then
      Result := DefaultPixelFormat;
  finally
    FormatList.Free;
  end;
end;

procedure TGDIPixelSurface.BitBlt(const DestHandle: HDC; const DestAt, Size, SrcAt: TPoint2px);
begin
  if (DestHandle <> 0) and (FHandle <> 0) then
    Windows.BitBlt(DestHandle, DestAt.X, DestAt.Y, Size.X, Size.Y, FHandle, SrcAt.X, SrcAt.Y, SRCCOPY);
end;

procedure TGDIPixelSurface.BitBlt(const DestSurface: TGDIPixelSurface; const DestAt, Size, SrcAt: TPoint2px);
begin
  BitBlt(DestSurface.Handle, DestAt, Size, SrcAt);
end;

function TGDIPixelMipMapSurface.CreatePixelSurfaces: TPixelSurfaces;
begin
  Result := TGDIPixelSurfaces.Create;
end;

function TGDIPixelSurfaces.CreatePixelSurface(const SurfaceName: StdString): TPixelSurface;
begin
  Result := TGDIPixelSurface.CreateNamed(SurfaceName);
end;

end.
